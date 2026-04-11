use crate::types::{Band, Callsign, Continent};
use serde::de::{Deserializer, Error as DeError};
use serde::ser::Serializer;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fs;
use std::path::Path;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ResolvedStation {
    pub dxcc: String,
    pub continent: Continent,
    pub is_wve: bool,
    pub is_na: bool,
}

impl ResolvedStation {
    pub fn new(dxcc: impl AsRef<str>, continent: Continent, is_wve: bool, is_na: bool) -> Self {
        Self {
            dxcc: dxcc.as_ref().trim().to_ascii_uppercase(),
            continent,
            is_wve,
            is_na,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Mode {
    CW,
    SSB,
    RTTY,
    FT8,
    FT4,
    DIGITAL,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Value {
    Bool(bool),
    Int(i64),
    Text(String),
}

impl Value {
    fn as_i64(&self) -> Option<i64> {
        if let Self::Int(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    fn as_text(&self) -> String {
        match self {
            Self::Text(v) => v.clone(),
            Self::Int(v) => v.to_string(),
            Self::Bool(v) => v.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum Scope {
    Config,
    Source,
    Dest,
    Rcvd,
    Sent,
    /// Per-QSO environment: keys `band` and `mode`, derived from the
    /// band/mode the QSO is being applied under. Populated by the engine
    /// at call time; unused by exchange-parse contexts, which substitute
    /// empty values. Available only inside point rules, multiplier
    /// variants, and bonus rules — *not* inside exchange variant
    /// selection (where the mode/band isn't yet bound to the QSO).
    Session,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FieldRef {
    pub scope: Scope,
    pub key: String,
}

impl FieldRef {
    pub fn config(key: impl AsRef<str>) -> Self {
        Self {
            scope: Scope::Config,
            key: key.as_ref().to_string(),
        }
    }

    pub fn source(key: impl AsRef<str>) -> Self {
        Self {
            scope: Scope::Source,
            key: key.as_ref().to_string(),
        }
    }

    pub fn dest(key: impl AsRef<str>) -> Self {
        Self {
            scope: Scope::Dest,
            key: key.as_ref().to_string(),
        }
    }

    pub fn rcvd(key: impl AsRef<str>) -> Self {
        Self {
            scope: Scope::Rcvd,
            key: key.as_ref().to_string(),
        }
    }

    pub fn sent(key: impl AsRef<str>) -> Self {
        Self {
            scope: Scope::Sent,
            key: key.as_ref().to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Operand {
    Field(FieldRef),
    Value(Value),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Predicate {
    Eq(Operand, Operand),
    Ne(Operand, Operand),
    And(Vec<Predicate>),
    Or(Vec<Predicate>),
    Not(Box<Predicate>),
    Between(FieldRef, i64, i64),
    In(FieldRef, Vec<String>),
    /// Tests whether the current QSO's destination callsign appears in an
    /// external domain list (case-insensitive). Used to match bonus stations
    /// like K4TCG / W0MA / VA3CCO without hardcoding callsigns into the spec.
    DestCallIn(DomainRef),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Dimension {
    Band,
    Mode,
    BandMode,
    Period { minutes: u32 },
    Global,
}

impl Serialize for Dimension {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let text = match self {
            Self::Band => "band".to_string(),
            Self::Mode => "mode".to_string(),
            Self::BandMode => "band_mode".to_string(),
            Self::Global => "global".to_string(),
            Self::Period { minutes } => format!("period:{minutes}"),
        };
        serializer.serialize_str(&text)
    }
}

impl<'de> Deserialize<'de> for Dimension {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = String::deserialize(deserializer)?;
        parse_dimension_str(&raw).map_err(D::Error::custom)
    }
}

fn parse_dimension_str(raw: &str) -> Result<Dimension, String> {
    let token = raw.trim().to_ascii_lowercase();
    match token.as_str() {
        "band" => Ok(Dimension::Band),
        "mode" => Ok(Dimension::Mode),
        "bandmode" | "band_mode" => Ok(Dimension::BandMode),
        "global" => Ok(Dimension::Global),
        _ => {
            if let Some(mins) = token.strip_prefix("period:") {
                let minutes = mins
                    .parse::<u32>()
                    .map_err(|_| format!("invalid period dimension '{}'", raw))?;
                if minutes == 0 {
                    return Err("period dimension must be > 0".to_string());
                }
                Ok(Dimension::Period { minutes })
            } else {
                Err(format!("unsupported dimension '{}'", raw))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum DomainRef {
    Any,
    Range { min: i64, max: i64 },
    External { name: String },
    List(Vec<String>),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum FieldType {
    Rst,
    Int,
    Enum,
    Power,
    String,
    Literal,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExchangeField {
    pub id: String,
    pub field_type: FieldType,
    pub required: bool,
    pub domain: Option<DomainRef>,
    pub accept: Vec<String>,
    pub normalize_upper_trim: bool,
    /// Optional separator for multi-value received fields. When set, the
    /// raw value is split on this substring at multiplier-extraction time,
    /// and each split value produces a separate multiplier entry (subject
    /// to per-value domain validation). The parsed field value itself
    /// remains the full original string — this is a **multiplier-side**
    /// split, not an exchange-parsing split. Used by state QSO parties
    /// where county-line stations send multiple county codes in a single
    /// QSO exchange (e.g., Florida "ALC/BAK", CQP multi-county).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub multi_value_sep: Option<String>,
}

impl ExchangeField {
    pub fn required(id: impl AsRef<str>, field_type: FieldType) -> Self {
        Self {
            id: id.as_ref().to_string(),
            field_type,
            required: true,
            domain: None,
            accept: Vec::new(),
            normalize_upper_trim: false,
            multi_value_sep: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExchangeVariant {
    pub when: Option<Predicate>,
    pub fields: Vec<ExchangeField>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExchangeSpec {
    pub received_variants: Vec<ExchangeVariant>,
    #[serde(default)]
    pub sent_variants: Vec<SentVariant>,
    #[serde(default)]
    pub sent_serial: Option<SentSerialSpec>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SentSerialSpec {
    pub field_id: String,
    #[serde(default = "default_serial_start")]
    pub start: u32,
    #[serde(default = "default_serial_width")]
    pub width: usize,
    #[serde(default)]
    pub scope: Option<Dimension>,
}

fn default_serial_start() -> u32 {
    1
}

fn default_serial_width() -> usize {
    3
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum SentValue {
    Const(String),
    Config(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SentField {
    pub id: String,
    pub value: SentValue,
    pub normalize_upper_trim: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SentVariant {
    pub when: Option<Predicate>,
    pub fields: Vec<SentField>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PointRule {
    pub when: Option<Predicate>,
    pub value: i64,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MultiplierSpec {
    pub id: String,
    pub dimension: Dimension,
    pub variants: Vec<MultiplierVariant>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MultiplierVariant {
    pub when: Option<Predicate>,
    pub key: KeyExpr,
    pub domain: DomainRef,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum KeyExpr {
    Field(FieldRef),
    Op(KeyExprOp),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum KeyExprOp {
    Concat(Vec<KeyExprPart>),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum KeyExprPart {
    Field(FieldRef),
    Const(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ConfigField {
    pub id: String,
    pub required: bool,
    pub required_when: Option<Predicate>,
    #[serde(default)]
    pub domain: Option<DomainRef>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ContestSpec {
    pub id: String,
    pub name: String,
    pub cabrillo_contest: String,
    pub bands: Vec<Band>,
    pub modes: Vec<Mode>,
    pub dupe_dimension: Dimension,
    /// Optional received exchange field ids whose values extend the dupe key.
    ///
    /// When empty (the default), dupes are keyed purely on
    /// `(dimension_scope, callsign)` — the historical behavior.
    /// When non-empty, the values of the listed received fields are
    /// concatenated (in list order) with a unit-separator and appended to the
    /// dupe key, so a mobile station sending `INMAD` is not a dupe of the
    /// same call's earlier `INMRN` QSO on the same band/mode.
    ///
    /// Used by state QSO parties where mobile/rover stations are re-workable
    /// when they cross a county or region boundary.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub dupe_extra_rcvd_fields: Vec<String>,
    pub valid_qso: Vec<(Predicate, String)>,
    pub exchange: ExchangeSpec,
    pub multipliers: Vec<MultiplierSpec>,
    pub points: Vec<PointRule>,
    /// Optional additive bonus-point rules. Applied after `points × mults`.
    ///
    /// Empty vector (the default) leaves scoring behavior byte-identical to
    /// historical contests. Non-empty vectors accumulate extra points that
    /// are added to the final score by `claimed_score()`. Used by state QSO
    /// parties to model bonus stations (e.g. "+100 per QSO with K4TCG"),
    /// one-time rewards (e.g. "+250 for any QSO with W1AW/5"), and
    /// activation thresholds (e.g. "+500 per county with at least 10 QSOs").
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub bonus_rules: Vec<BonusRule>,
    /// Optional whole-score multiplier driven by a config field value.
    ///
    /// Applied after `points × mults` and before bonus addition. When
    /// `None` (the default), the factor is 1 and scoring is unchanged.
    /// Used by state QSO parties that award QRP operators a x5 bonus
    /// (NMQP, NEQSOP) or low-power operators a x2 bonus (DEQP, NMQP,
    /// NEQSOP).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub score_multiplier: Option<ScoreMultiplier>,
    pub config_fields: Vec<ConfigField>,
    #[serde(default)]
    pub score: ScoreSpec,
    #[serde(default)]
    pub variants: BTreeMap<String, ContestVariant>,
    #[serde(default)]
    pub default_variant: Option<String>,
    #[serde(default)]
    pub deprecated: Option<DeprecatedSpec>,
}

/// A single bonus rule — a condition and an amount added to the final score.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BonusRule {
    /// Human-readable identifier for error messages and debugging.
    pub id: String,
    /// Optional predicate that must hold for the rule to fire on a QSO.
    /// When `None`, every QSO is a candidate and the `scope` alone controls
    /// how many times the bonus can fire.
    #[serde(default)]
    pub when: Option<Predicate>,
    /// Point amount added for each firing of the rule.
    pub amount: i64,
    /// How the rule accumulates over the log.
    pub scope: BonusScope,
}

/// Whole-score multiplier driven by a config-field value.
///
/// Used to express power-class score multipliers like QRP ×5, Low ×2,
/// High ×1. At scoring time the engine looks up `from_config` in the
/// operator's config, finds that value in `mapping`, and multiplies the
/// base score (`points × mults`) by the resulting factor. If the config
/// value is absent or not in `mapping`, `default` is used.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ScoreMultiplier {
    /// Which config field drives the lookup (usually `my_power_class` or
    /// similar — e.g., config value `"QRP"` / `"LOW"` / `"HIGH"`).
    pub from_config: String,
    /// Lookup table mapping config value → factor (case-insensitive).
    pub mapping: BTreeMap<String, u32>,
    /// Fallback factor when the config value is missing or not in
    /// `mapping`. Defaults to 1 so a partial mapping with a typo still
    /// produces sane scoring.
    #[serde(default = "score_multiplier_default")]
    pub default: u32,
}

fn score_multiplier_default() -> u32 {
    1
}

/// How a `BonusRule` accumulates across multiple QSOs.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BonusScope {
    /// Fires at most once per log — for a matching QSO, accumulate `amount`
    /// and then ignore the rule for the rest of the log. Useful for
    /// "one-time bonus station" rules (e.g. "+250 pts for the first QSO
    /// with W1AW/5").
    Once,
    /// Fires on every matching QSO. Useful for "+10 pts per bonus-station
    /// contact" rules that don't cap the total.
    PerQso,
    /// Fires at most once per `(band, mode)` pair. Useful for "+100 per
    /// band-mode to bonus station K4TCG" — 12 max firings for a CW+SSB
    /// 6-band contest.
    PerBandMode,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Default)]
pub struct DeprecatedSpec {
    #[serde(default)]
    pub replaced_by: Option<String>,
    #[serde(default)]
    pub note: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Default)]
pub struct ContestVariant {
    #[serde(default)]
    pub name: Option<String>,
    #[serde(default)]
    pub cabrillo_contest: Option<String>,
    #[serde(default)]
    pub allowed_modes: Option<Vec<Mode>>,
    #[serde(default)]
    pub exchange: Option<ExchangeOverrides>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Default)]
pub struct ExchangeOverrides {
    #[serde(default)]
    pub sent_rst_value: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct SessionMeta {
    pub variant: Option<String>,
    pub category_mode: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectiveSpec {
    pub allowed_modes: Vec<Mode>,
    pub cabrillo_contest: String,
    pub exchange: EffectiveExchange,
    pub selected_variant: Option<String>,
    pub category_mode: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct EffectiveExchange {
    pub sent_rst_value: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ScoreSpec {
    pub formula: ScoreFormula,
}

impl Default for ScoreSpec {
    fn default() -> Self {
        Self {
            formula: ScoreFormula::PointsTimesTotalMults,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ScoreFormula {
    #[serde(alias = "points_times_mults")]
    PointsTimesTotalMults,
    PointsTimesSumBandMults,
    SumBandPointsTimesBandMults,
}

impl ContestSpec {
    pub fn from_yaml_str(input: &str) -> Result<Self, serde_yaml::Error> {
        serde_yaml::from_str(input)
    }

    pub fn from_json_str(input: &str) -> Result<Self, serde_json::Error> {
        serde_json::from_str(input)
    }

    pub fn from_path(path: impl AsRef<Path>) -> Result<Self, String> {
        let path_ref = path.as_ref();
        let raw = fs::read_to_string(path_ref)
            .map_err(|e| format!("failed to read {}: {}", path_ref.display(), e))?;
        let ext = path_ref
            .extension()
            .and_then(|v| v.to_str())
            .unwrap_or_default()
            .to_ascii_lowercase();
        match ext.as_str() {
            "yaml" | "yml" => {
                Self::from_yaml_str(&raw).map_err(|e| format!("yaml parse failed: {}", e))
            }
            "json" => Self::from_json_str(&raw).map_err(|e| format!("json parse failed: {}", e)),
            _ => Err(format!(
                "unsupported spec extension '{}'; expected .yaml/.yml/.json",
                ext
            )),
        }
    }
}

fn choose_variant(
    spec: &ContestSpec,
    requested: Option<&str>,
) -> Result<Option<(String, ContestVariant)>, EngineError> {
    if let Some(name) = requested {
        let key = name.trim().to_ascii_lowercase();
        let variant = spec
            .variants
            .get(&key)
            .cloned()
            .ok_or_else(|| EngineError::InvalidVariant(format!("unknown variant {}", name)))?;
        return Ok(Some((key, variant)));
    }

    if let Some(default_name) = spec.default_variant.as_ref() {
        let key = default_name.trim().to_ascii_lowercase();
        let variant = spec.variants.get(&key).cloned().ok_or_else(|| {
            EngineError::InvalidVariant(format!("default variant {} not found", default_name))
        })?;
        return Ok(Some((key, variant)));
    }

    if spec.variants.len() == 1 {
        let (name, variant) = spec.variants.iter().next().expect("one variant present");
        return Ok(Some((name.clone(), variant.clone())));
    }

    Ok(None)
}

fn build_effective_spec(spec: &ContestSpec, meta: &SessionMeta) -> Result<EffectiveSpec, EngineError> {
    let selected = choose_variant(spec, meta.variant.as_deref())?;
    let mut allowed_modes = spec.modes.clone();
    let mut cabrillo_contest = spec.cabrillo_contest.clone();
    let mut exchange = EffectiveExchange::default();
    let mut selected_name = None;

    if let Some((name, variant)) = selected {
        selected_name = Some(name);
        if let Some(modes) = variant.allowed_modes {
            allowed_modes = modes;
        }
        if let Some(contest_name) = variant.cabrillo_contest {
            cabrillo_contest = contest_name;
        }
        if let Some(exchange_overrides) = variant.exchange {
            exchange.sent_rst_value = exchange_overrides.sent_rst_value;
        }
    }

    Ok(EffectiveSpec {
        allowed_modes,
        cabrillo_contest,
        exchange,
        selected_variant: selected_name,
        category_mode: meta.category_mode.clone(),
    })
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EngineError {
    InvalidConfig(String),
    InvalidVariant(String),
    InvalidMode { mode: Mode, allowed: Vec<Mode> },
    Exchange(Vec<ExchangeError>),
    Resolve(String),
    InvalidQso(String),
}

impl std::fmt::Display for ExchangeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.field_id {
            Some(field) => write!(f, "{:?} {}: {}", self.kind, field, self.message),
            None => write!(f, "{:?}: {}", self.kind, self.message),
        }
    }
}

impl std::error::Error for ExchangeError {}

impl std::fmt::Display for EngineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EngineError::InvalidConfig(msg) => write!(f, "invalid config: {}", msg),
            EngineError::InvalidVariant(msg) => write!(f, "invalid variant: {}", msg),
            EngineError::InvalidMode { mode, allowed } => {
                write!(f, "invalid mode {:?}; allowed: {:?}", mode, allowed)
            }
            EngineError::Exchange(errors) => {
                if errors.is_empty() {
                    write!(f, "exchange parse failed")
                } else {
                    let rendered = errors
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join("; ");
                    write!(f, "exchange parse failed: {}", rendered)
                }
            }
            EngineError::Resolve(msg) => write!(f, "resolve error: {}", msg),
            EngineError::InvalidQso(msg) => write!(f, "invalid qso: {}", msg),
        }
    }
}

impl std::error::Error for EngineError {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExchangeErrorKind {
    Missing,
    Invalid,
    DomainMismatch,
    ExtraTokens,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExchangeError {
    pub field_id: Option<String>,
    pub kind: ExchangeErrorKind,
    pub message: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CabrilloQso {
    pub freq_khz: u32,
    pub mode: Mode,
    pub date_utc: String,
    pub time_utc: String,
    pub my_call: String,
    pub their_call: String,
    pub sent: Vec<String>,
    pub received: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LogInput {
    pub band: Band,
    pub call: Callsign,
    pub raw_exchange: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReplayError {
    AtIndex(usize, EngineError),
}

#[must_use]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ApplySummary {
    pub is_dupe: bool,
    pub qso_points: i64,
    pub qso_points_rule_index: Option<usize>,
    pub qso_points_is_fallback: bool,
    pub new_mults: Vec<String>,
    pub sent_exchange: HashMap<String, String>,
    pub total_qsos: u32,
    pub total_points: i64,
    pub total_mults: usize,
    pub claimed_score: i64,
}

#[must_use]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CandidateSummary {
    pub is_dupe: bool,
    pub would_be_new_mults: Vec<String>,
}

#[must_use]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CandidateEvalLite {
    pub is_dupe: bool,
    pub new_mults: Vec<String>,
    pub notes: Vec<String>,
}

pub trait DomainProvider {
    fn values(&self, domain_name: &str) -> Option<Arc<[String]>>;
}

pub trait StationResolver {
    fn resolve(&self, call: &Callsign) -> Result<ResolvedStation, String>;
}

#[derive(Debug, Clone, Copy, Default)]
struct SessionEnv {
    band: Option<Band>,
    mode: Option<Mode>,
}

#[derive(Debug, Clone)]
struct EvalContext<'a> {
    config: &'a HashMap<String, Value>,
    source: &'a ResolvedStation,
    dest: &'a ResolvedStation,
    dest_call: &'a str,
    rcvd: &'a HashMap<String, Value>,
    sent: &'a HashMap<String, Value>,
    session: SessionEnv,
}

fn eval_station_field(station: &ResolvedStation, key: &str) -> Option<Value> {
    match key.trim().to_ascii_lowercase().as_str() {
        "dxcc" => Some(Value::Text(station.dxcc.clone())),
        "continent" => Some(Value::Text(format!("{:?}", station.continent))),
        "is_wve" | "iswve" => Some(Value::Bool(station.is_wve)),
        "is_na" | "isna" => Some(Value::Bool(station.is_na)),
        _ => None,
    }
}

fn eval_session_field(env: &SessionEnv, key: &str) -> Option<Value> {
    match key.trim().to_ascii_lowercase().as_str() {
        "band" => env.band.map(|b| Value::Text(format!("{:?}", b))),
        "mode" => env.mode.map(|m| Value::Text(format!("{:?}", m))),
        _ => None,
    }
}

fn eval_field(ctx: &EvalContext<'_>, field: &FieldRef) -> Option<Value> {
    match field.scope {
        Scope::Config => ctx.config.get(&field.key).cloned(),
        Scope::Source => eval_station_field(ctx.source, &field.key),
        Scope::Dest => {
            if field.key.trim().eq_ignore_ascii_case("call") {
                Some(Value::Text(ctx.dest_call.to_string()))
            } else {
                eval_station_field(ctx.dest, &field.key)
            }
        }
        Scope::Rcvd => ctx.rcvd.get(&field.key).cloned(),
        Scope::Sent => ctx.sent.get(&field.key).cloned(),
        Scope::Session => eval_session_field(&ctx.session, &field.key),
    }
}

fn eval_key_part(ctx: &EvalContext<'_>, part: &KeyExprPart) -> Option<String> {
    match part {
        KeyExprPart::Field(field) => eval_field(ctx, field).map(|v| v.as_text()),
        KeyExprPart::Const(text) => Some(text.clone()),
    }
}

fn eval_key_expr(ctx: &EvalContext<'_>, expr: &KeyExpr) -> Option<String> {
    match expr {
        KeyExpr::Field(field) => eval_field(ctx, field).map(|v| v.as_text()),
        KeyExpr::Op(KeyExprOp::Concat(parts)) => {
            let mut out = String::new();
            for part in parts {
                out.push_str(&eval_key_part(ctx, part)?);
            }
            Some(out)
        }
    }
}

fn eval_operand(ctx: &EvalContext<'_>, operand: &Operand) -> Option<Value> {
    match operand {
        Operand::Field(field) => eval_field(ctx, field),
        Operand::Value(value) => Some(value.clone()),
    }
}

fn eval_predicate(ctx: &EvalContext<'_>, pred: &Predicate) -> bool {
    match pred {
        Predicate::Eq(left, right) => eval_operand(ctx, left) == eval_operand(ctx, right),
        Predicate::Ne(left, right) => eval_operand(ctx, left) != eval_operand(ctx, right),
        Predicate::And(items) => items.iter().all(|p| eval_predicate(ctx, p)),
        Predicate::Or(items) => items.iter().any(|p| eval_predicate(ctx, p)),
        Predicate::Not(inner) => !eval_predicate(ctx, inner),
        Predicate::Between(field, min, max) => eval_field(ctx, field)
            .and_then(|v| v.as_i64())
            .map(|v| v >= *min && v <= *max)
            .unwrap_or(false),
        Predicate::In(field, values) => eval_field(ctx, field)
            .map(|v| values.contains(&v.as_text()))
            .unwrap_or(false),
        Predicate::DestCallIn(_) => {
            // Requires a domain provider to look up; this pure-eval path
            // cannot check the list. Use `eval_predicate_with_domains` from
            // callers that need DestCallIn support (bonus rules, valid_qso).
            false
        }
    }
}

/// Variant of `eval_predicate` that supports predicates which need the
/// domain provider — currently only `Predicate::DestCallIn`. All other
/// predicate variants fall through to the pure-eval path.
fn eval_predicate_with_domains(
    ctx: &EvalContext<'_>,
    pred: &Predicate,
    domains: &dyn DomainProvider,
) -> bool {
    match pred {
        Predicate::And(items) => items
            .iter()
            .all(|p| eval_predicate_with_domains(ctx, p, domains)),
        Predicate::Or(items) => items
            .iter()
            .any(|p| eval_predicate_with_domains(ctx, p, domains)),
        Predicate::Not(inner) => !eval_predicate_with_domains(ctx, inner, domains),
        Predicate::DestCallIn(domain) => {
            let call_upper = ctx.dest_call.trim().to_ascii_uppercase();
            match domain {
                DomainRef::Any => !call_upper.is_empty(),
                DomainRef::List(items) => items.iter().any(|v| v.eq_ignore_ascii_case(&call_upper)),
                DomainRef::External { name } => domains
                    .values(name)
                    .map(|vals| vals.iter().any(|v| v.eq_ignore_ascii_case(&call_upper)))
                    .unwrap_or(false),
                DomainRef::Range { .. } => false,
            }
        }
        _ => eval_predicate(ctx, pred),
    }
}

fn operand_uses_scope(operand: &Operand, scope: Scope) -> bool {
    match operand {
        Operand::Field(field) => field.scope == scope,
        Operand::Value(_) => false,
    }
}

fn predicate_uses_scope(pred: &Predicate, scope: Scope) -> bool {
    match pred {
        Predicate::Eq(left, right) | Predicate::Ne(left, right) => {
            operand_uses_scope(left, scope.clone()) || operand_uses_scope(right, scope)
        }
        Predicate::And(items) | Predicate::Or(items) => {
            items.iter().any(|p| predicate_uses_scope(p, scope.clone()))
        }
        Predicate::Not(inner) => predicate_uses_scope(inner, scope),
        Predicate::Between(field, ..) | Predicate::In(field, ..) => field.scope == scope,
        // DestCallIn reads the destination callsign, not any declared scope
        // the caller is interested in tracking via this helper.
        Predicate::DestCallIn(_) => false,
    }
}

fn key_expr_uses_scope(expr: &KeyExpr, scope: Scope) -> bool {
    match expr {
        KeyExpr::Field(field) => field.scope == scope,
        KeyExpr::Op(KeyExprOp::Concat(parts)) => parts.iter().any(|part| match part {
            KeyExprPart::Field(field) => field.scope == scope,
            KeyExprPart::Const(_) => false,
        }),
    }
}

fn variant_requires_exchange_data(variant: &MultiplierVariant) -> bool {
    if key_expr_uses_scope(&variant.key, Scope::Rcvd) || key_expr_uses_scope(&variant.key, Scope::Sent)
    {
        return true;
    }
    variant
        .when
        .as_ref()
        .map(|p| predicate_uses_scope(p, Scope::Rcvd) || predicate_uses_scope(p, Scope::Sent))
        .unwrap_or(false)
}

fn normalize_text(input: &str, upper_trim: bool) -> String {
    if upper_trim {
        input.trim().to_ascii_uppercase()
    } else {
        input.trim().to_string()
    }
}

fn normalize_enum_token(input: &str) -> String {
    input
        .trim()
        .trim_matches(|c: char| !c.is_ascii_alphanumeric())
        .to_ascii_uppercase()
}

fn canonical_location_token(input: &str) -> String {
    match input {
        "NWT" => "NT".to_string(),
        "LAB" => "LB".to_string(),
        "NFLD" => "NL".to_string(),
        _ => input.to_string(),
    }
}

fn validate_name_token(input: &str) -> bool {
    let len = input.len();
    if !(1..=12).contains(&len) {
        return false;
    }
    input
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || matches!(c, '-' | '/' | '\''))
}

fn normalize_power_token(input: &str) -> Result<String, String> {
    let token = input.trim().to_ascii_uppercase().replace(' ', "");
    if token.is_empty() {
        return Err("empty power token".to_string());
    }

    let (numeric, is_kw) = if let Some(value) = token.strip_suffix("KW") {
        (value, true)
    } else if let Some(value) = token.strip_suffix('W') {
        (value, false)
    } else {
        (token.as_str(), false)
    };

    let watts = if is_kw {
        let kw = numeric
            .parse::<f64>()
            .map_err(|_| format!("invalid kw power {}", input))?;
        (kw * 1000.0).round() as i64
    } else {
        numeric
            .parse::<i64>()
            .map_err(|_| format!("invalid power {}", input))?
    };

    if !(1..=2000).contains(&watts) {
        return Err(format!("power out of supported range {}", watts));
    }
    Ok(watts.to_string())
}

fn is_valid_rst(token: &str) -> bool {
    fn is_digit_1_to_9(c: char) -> bool {
        matches!(c, '1'..='9')
    }
    fn is_digit_or_n(c: char) -> bool {
        is_digit_1_to_9(c) || c == 'N'
    }

    let mut chars = token.chars();
    let Some(r) = chars.next() else {
        return false;
    };
    let Some(s) = chars.next() else {
        return false;
    };
    if !matches!(r, '1'..='5') {
        return false;
    }

    match chars.next() {
        None => is_digit_1_to_9(s),
        Some(t) if is_digit_or_n(s) && is_digit_or_n(t) => chars.next().is_none(),
        Some(_) => false,
    }
}

fn parse_field(
    field: &ExchangeField,
    token: Option<&str>,
    domains: &dyn DomainProvider,
) -> Result<Option<Value>, ExchangeError> {
    let token = match token {
        Some(v) if !v.trim().is_empty() => v,
        _ if field.required => {
            return Err(ExchangeError {
                field_id: Some(field.id.clone()),
                kind: ExchangeErrorKind::Missing,
                message: format!("missing {}", field.id),
            });
        }
        _ => return Ok(None),
    };

    let parsed = match field.field_type {
        FieldType::Rst => {
            let upper = token.trim().to_ascii_uppercase();
            if is_valid_rst(&upper) {
                Value::Text(upper)
            } else {
                return Err(ExchangeError {
                    field_id: Some(field.id.clone()),
                    kind: ExchangeErrorKind::Invalid,
                    message: format!("invalid rst {}", token),
                });
            }
        }
        FieldType::Int => {
            let v = token.parse::<i64>().map_err(|_| ExchangeError {
                field_id: Some(field.id.clone()),
                kind: ExchangeErrorKind::Invalid,
                message: format!("invalid int for {}: {}", field.id, token),
            })?;
            Value::Int(v)
        }
        FieldType::Enum => {
            let mut value = normalize_enum_token(token);
            if matches!(field.id.as_str(), "sp" | "loc") {
                value = canonical_location_token(&value);
            }
            Value::Text(value)
        }
        FieldType::Power => {
            let normalized = normalize_power_token(token).map_err(|msg| ExchangeError {
                field_id: Some(field.id.clone()),
                kind: ExchangeErrorKind::Invalid,
                message: msg,
            })?;
            Value::Text(normalized)
        }
        FieldType::String => {
            let value = normalize_text(token, field.normalize_upper_trim);
            if field.id == "name" && !validate_name_token(&value) {
                return Err(ExchangeError {
                    field_id: Some(field.id.clone()),
                    kind: ExchangeErrorKind::Invalid,
                    message: format!("invalid name {}", token),
                });
            }
            Value::Text(value)
        }
        FieldType::Literal => {
            let v = normalize_text(token, true);
            if !field.accept.is_empty() && !field.accept.contains(&v) {
                return Err(ExchangeError {
                    field_id: Some(field.id.clone()),
                    kind: ExchangeErrorKind::Invalid,
                    message: format!("invalid literal {}", token),
                });
            }
            Value::Text(v)
        }
    };

    // Skip the field-level domain check when `multi_value_sep` is set.
    // The field holds a multi-value string like "NH/ON" that would never
    // be in the domain as a whole; the multiplier-extraction path splits
    // it and validates each piece individually.
    if field.multi_value_sep.is_none() {
        if let Some(domain) = &field.domain {
            let text = parsed.as_text();
            let ok = match domain {
                DomainRef::Any => true,
                DomainRef::Range { min, max } => parsed
                    .as_i64()
                    .map(|v| v >= *min && v <= *max)
                    .unwrap_or(false),
                DomainRef::External { name } => domains
                    .values(name)
                    .map(|vals| vals.contains(&text))
                    .unwrap_or(false),
                DomainRef::List(items) => items.contains(&text),
            };
            if !ok {
                return Err(ExchangeError {
                    field_id: Some(field.id.clone()),
                    kind: ExchangeErrorKind::DomainMismatch,
                    message: format!("value {} not in domain for {}", text, field.id),
                });
            }
        }
    }

    Ok(Some(parsed))
}

fn parse_received(
    spec: &ContestSpec,
    config: &HashMap<String, Value>,
    source: &ResolvedStation,
    dest: &ResolvedStation,
    raw_exchange: &str,
    domains: &dyn DomainProvider,
) -> Result<HashMap<String, Value>, Vec<ExchangeError>> {
    let empty = HashMap::new();
    let sent = HashMap::new();
    let base_ctx = EvalContext {
        config,
        source,
        dest,
        rcvd: &empty,
        sent: &sent,
        dest_call: "",
        session: SessionEnv::default(),
    };
    let variant = spec
        .exchange
        .received_variants
        .iter()
        .find(|v| {
            v.when
                .as_ref()
                .map(|w| eval_predicate(&base_ctx, w))
                .unwrap_or(true)
        })
        .ok_or_else(|| {
            vec![ExchangeError {
                field_id: None,
                kind: ExchangeErrorKind::Invalid,
                message: "no matching exchange variant".to_string(),
            }]
        })?;

    let mut tokens: Vec<&str> = raw_exchange.split_whitespace().collect();
    if tokens.len() == 1
        && variant.fields.len() == 2
        && variant.fields[0].field_type == FieldType::Rst
        && variant.fields[1].field_type == FieldType::Int
    {
        let token = tokens[0];
        for idx in 1..token.len() {
            let (left, right) = token.split_at(idx);
            if right.len() > 2 || !right.chars().all(|c| c.is_ascii_digit()) {
                continue;
            }
            let test = ExchangeField::required("rst", FieldType::Rst);
            if parse_field(&test, Some(left), domains).is_ok() {
                tokens = vec![left, right];
                break;
            }
        }
    }

    let mut errors = Vec::new();
    let mut parsed = HashMap::new();
    for (idx, field) in variant.fields.iter().enumerate() {
        match parse_field(field, tokens.get(idx).copied(), domains) {
            Ok(Some(v)) => {
                parsed.insert(field.id.clone(), v);
            }
            Ok(None) => {}
            Err(err) => errors.push(err),
        }
    }
    if tokens.len() > variant.fields.len() {
        errors.push(ExchangeError {
            field_id: None,
            kind: ExchangeErrorKind::ExtraTokens,
            message: format!(
                "extra tokens present: {}",
                tokens[variant.fields.len()..].join(" ")
            ),
        });
    }

    if errors.is_empty() {
        Ok(parsed)
    } else {
        Err(errors)
    }
}

fn validate_value_in_domain(value: &str, domain: &DomainRef, domains: &dyn DomainProvider) -> bool {
    match domain {
        DomainRef::Any => true,
        DomainRef::Range { min, max } => value
            .parse::<i64>()
            .map(|v| v >= *min && v <= *max)
            .unwrap_or(false),
        DomainRef::External { name } => domains
            .values(name)
            .map(|vals| {
                vals.contains(&value.to_ascii_uppercase()) || vals.contains(&value.to_string())
            })
            .unwrap_or(false),
        DomainRef::List(items) => items.contains(&value.to_string()),
    }
}

pub fn render_exchange_errors(errors: &[ExchangeError]) -> Vec<String> {
    errors
        .iter()
        .map(|e| match &e.field_id {
            Some(field) => format!("{:?} {}: {}", e.kind, field, e.message),
            None => format!("{:?}: {}", e.kind, e.message),
        })
        .collect()
}

#[derive(Debug, Clone)]
pub struct SpecEngine {
    spec: ContestSpec,
    effective_spec: EffectiveSpec,
    config: HashMap<String, Value>,
    source: ResolvedStation,
    total_points: i64,
    total_qsos: u32,
    dupes: HashSet<(Option<Band>, Option<Mode>, Option<i64>, Callsign, Option<String>)>,
    mults: HashMap<(String, Option<Band>, Option<Mode>, Option<i64>), HashSet<String>>,
    points_by_band: HashMap<Band, i64>,
    serial_counters: HashMap<(Option<Band>, Option<Mode>, Option<i64>), u32>,
    bonus: BonusAccumulator,
}

/// Per-session accumulator for bonus rules. Stays empty (no allocations,
/// trivial final contribution of 0) when `ContestSpec::bonus_rules` is empty.
#[derive(Debug, Clone, Default)]
struct BonusAccumulator {
    /// Accumulated total across all bonus-rule firings.
    total: i64,
    /// Rule ids that have already fired a `Once` scope.
    fired_once: HashSet<String>,
    /// `(rule_id, band, mode)` keys that have already fired a `PerBandMode`
    /// scope. Each key contributes `amount` at most once.
    fired_per_band_mode: HashSet<(String, Band, Mode)>,
}

pub struct SpecSession<R, D>
where
    R: StationResolver,
    D: DomainProvider,
{
    engine: SpecEngine,
    resolver: R,
    domains: D,
}

impl SpecEngine {
    pub fn multiplier_ids(&self) -> Vec<&str> {
        self.spec.multipliers.iter().map(|m| m.id.as_str()).collect()
    }

    /// Build the rover-identity portion of a dupe key from the received
    /// exchange. Returns `None` when `dupe_extra_rcvd_fields` is empty
    /// (the vast majority of contests), which preserves the historical
    /// 4-tuple dupe-key semantics byte-for-byte.
    fn dupe_extra_key(&self, parsed: &HashMap<String, Value>) -> Option<String> {
        if self.spec.dupe_extra_rcvd_fields.is_empty() {
            return None;
        }
        let parts: Vec<String> = self
            .spec
            .dupe_extra_rcvd_fields
            .iter()
            .map(|id| parsed.get(id).map(Value::as_text).unwrap_or_default())
            .collect();
        // Unit-separator (U+001F) — never appears in normal exchange text.
        Some(parts.join("\u{1f}"))
    }

    /// If `key` is a direct `RCVD` field reference AND that field declares
    /// a `multi_value_sep`, return the separator so the caller can split
    /// the value into multiple multiplier rows. Returns `None` for every
    /// historical spec (no multi_value_sep declared anywhere), preserving
    /// single-value behavior byte-for-byte.
    fn multi_value_sep_for_key(&self, key: &KeyExpr) -> Option<String> {
        let field_id = match key {
            KeyExpr::Field(field) if field.scope == Scope::Rcvd => &field.key,
            _ => return None,
        };
        // Scan every received-variant's field list for a matching id.
        for variant in &self.spec.exchange.received_variants {
            for f in &variant.fields {
                if f.id == *field_id {
                    return f.multi_value_sep.clone();
                }
            }
        }
        None
    }

    /// Evaluate all bonus rules against the current QSO and accumulate any
    /// that fire. Called from `apply_qso_at_with_mode` after multipliers
    /// have been recorded. When `bonus_rules` is empty, this is a
    /// zero-overhead no-op: the for-loop body doesn't execute and
    /// `accumulator.total` stays at 0.
    ///
    /// Takes explicit borrows (`bonus_rules`, `accumulator`) instead of
    /// `&mut self` so the caller can still hold a reference to the
    /// `EvalContext` (which borrows `self.config` immutably).
    fn accumulate_bonuses(
        bonus_rules: &[BonusRule],
        accumulator: &mut BonusAccumulator,
        ctx: &EvalContext<'_>,
        domains: &dyn DomainProvider,
        band: Band,
        mode: Mode,
    ) {
        for rule in bonus_rules {
            let matches = rule
                .when
                .as_ref()
                .map(|p| eval_predicate_with_domains(ctx, p, domains))
                .unwrap_or(true);
            if !matches {
                continue;
            }
            match &rule.scope {
                BonusScope::Once => {
                    if accumulator.fired_once.insert(rule.id.clone()) {
                        accumulator.total += rule.amount;
                    }
                }
                BonusScope::PerQso => {
                    accumulator.total += rule.amount;
                }
                BonusScope::PerBandMode => {
                    let key = (rule.id.clone(), band, mode);
                    if accumulator.fired_per_band_mode.insert(key) {
                        accumulator.total += rule.amount;
                    }
                }
            }
        }
    }

    fn scope_key(
        dimension: Dimension,
        band: Band,
        mode: Mode,
        epoch_seconds: i64,
    ) -> (Option<Band>, Option<Mode>, Option<i64>) {
        match dimension {
            Dimension::Band => (Some(band), None, None),
            Dimension::Mode => (None, Some(mode), None),
            Dimension::BandMode => (Some(band), Some(mode), None),
            Dimension::Period { minutes } => {
                let bucket = epoch_seconds.div_euclid((minutes as i64) * 60);
                (None, None, Some(bucket))
            }
            Dimension::Global => (None, None, None),
        }
    }

    pub fn new(
        spec: ContestSpec,
        source: ResolvedStation,
        config: HashMap<String, Value>,
    ) -> Result<Self, EngineError> {
        Self::new_with_meta(spec, source, config, SessionMeta::default())
    }

    pub fn new_with_meta(
        spec: ContestSpec,
        source: ResolvedStation,
        config: HashMap<String, Value>,
        meta: SessionMeta,
    ) -> Result<Self, EngineError> {
        validate_config(&spec, &source, &config, None)?;
        let effective_spec = build_effective_spec(&spec, &meta)?;
        Ok(Self {
            spec,
            effective_spec,
            config,
            source,
            total_points: 0,
            total_qsos: 0,
            dupes: HashSet::new(),
            mults: HashMap::new(),
            points_by_band: HashMap::new(),
            serial_counters: HashMap::new(),
            bonus: BonusAccumulator::default(),
        })
    }

    pub fn apply_qso(
        &mut self,
        resolver: &dyn StationResolver,
        domains: &dyn DomainProvider,
        band: Band,
        call: Callsign,
        raw_exchange: &str,
    ) -> Result<ApplySummary, EngineError> {
        self.apply_qso_with_mode(resolver, domains, band, Mode::CW, call, raw_exchange)
    }

    pub fn apply_qso_with_mode(
        &mut self,
        resolver: &dyn StationResolver,
        domains: &dyn DomainProvider,
        band: Band,
        mode: Mode,
        call: Callsign,
        raw_exchange: &str,
    ) -> Result<ApplySummary, EngineError> {
        self.apply_qso_at_with_mode(resolver, domains, band, mode, 0, call, raw_exchange)
    }

    pub fn apply_qso_at_with_mode(
        &mut self,
        resolver: &dyn StationResolver,
        domains: &dyn DomainProvider,
        band: Band,
        mode: Mode,
        epoch_seconds: i64,
        call: Callsign,
        raw_exchange: &str,
    ) -> Result<ApplySummary, EngineError> {
        if !self.effective_spec.allowed_modes.contains(&mode) {
            return Err(EngineError::InvalidMode {
                mode,
                allowed: self.effective_spec.allowed_modes.clone(),
            });
        }
        let dest = resolver.resolve(&call).map_err(EngineError::Resolve)?;
        let parsed = parse_received(
            &self.spec,
            &self.config,
            &self.source,
            &dest,
            raw_exchange,
            domains,
        )
        .map_err(EngineError::Exchange)?;
        let call_text = call.as_str().to_string();
        let empty = HashMap::new();
        let ctx = EvalContext {
            config: &self.config,
            source: &self.source,
            dest: &dest,
            dest_call: &call_text,
            rcvd: &parsed,
            sent: &empty,
            session: SessionEnv {
                band: Some(band),
                mode: Some(mode),
            },
        };
        for (check, msg) in &self.spec.valid_qso {
            if !eval_predicate(&ctx, check) {
                return Err(EngineError::InvalidQso(msg.clone()));
            }
        }

        let dupe_scope = Self::scope_key(self.spec.dupe_dimension.clone(), band, mode, epoch_seconds);
        let dupe_extra = self.dupe_extra_key(&parsed);
        if self.dupes.contains(&(
            dupe_scope.0,
            dupe_scope.1,
            dupe_scope.2,
            call.clone(),
            dupe_extra.clone(),
        )) {
            return Ok(self.summary(true, 0, None, false, Vec::new(), HashMap::new()));
        }
        self.dupes.insert((
            dupe_scope.0,
            dupe_scope.1,
            dupe_scope.2,
            call,
            dupe_extra,
        ));
        self.total_qsos += 1;

        let (qso_points, qso_points_rule_index, qso_points_is_fallback) =
            self.compute_points(&ctx, domains);
        self.total_points += qso_points;
        *self.points_by_band.entry(band).or_insert(0) += qso_points;
        let mut new_mults = Vec::new();
        for mult in &self.spec.multipliers {
            let mult_scope = Self::scope_key(mult.dimension.clone(), band, mode, epoch_seconds);
            let variant = match mult.variants.iter().find(|v| {
                v.when
                    .as_ref()
                    .map(|p| eval_predicate(&ctx, p))
                    .unwrap_or(true)
            }) {
                Some(v) => v,
                None => continue,
            };
            let raw_value = match eval_key_expr(&ctx, &variant.key) {
                Some(v) => v,
                None => continue,
            };
            // Split the value into one or more multiplier candidates
            // according to the field's multi_value_sep (if any). The
            // vast majority of contests have no separator declared;
            // multi_sep is None and `values` has exactly one element
            // — the historical behavior.
            let multi_sep = self.multi_value_sep_for_key(&variant.key);
            let values: Vec<String> = match multi_sep {
                Some(sep) if !sep.is_empty() => raw_value
                    .split(sep.as_str())
                    .map(|s| s.trim().to_string())
                    .filter(|s| !s.is_empty())
                    .collect(),
                _ => vec![raw_value],
            };
            let state = self
                .mults
                .entry((mult.id.clone(), mult_scope.0, mult_scope.1, mult_scope.2))
                .or_default();
            for value in values {
                if !validate_value_in_domain(&value, &variant.domain, domains) {
                    continue;
                }
                if state.insert(value.clone()) {
                    new_mults.push(format!("{}:{}", mult.id, value));
                }
            }
        }

        Self::accumulate_bonuses(
            &self.spec.bonus_rules,
            &mut self.bonus,
            &ctx,
            domains,
            band,
            mode,
        );

        let mut sent_exchange = if self.spec.exchange.sent_variants.is_empty() {
            HashMap::new()
        } else {
            self.sent_exchange_for(&dest)?
        };
        if let Some(serial) = self.spec.exchange.sent_serial.as_ref() {
            let scope = serial.scope.clone().unwrap_or(Dimension::Global);
            let serial_scope = Self::scope_key(scope, band, mode, epoch_seconds);
            let next = self
                .serial_counters
                .entry((serial_scope.0, serial_scope.1, serial_scope.2))
                .or_insert(serial.start);
            let value = *next;
            *next += 1;
            sent_exchange.insert(
                serial.field_id.clone(),
                format!("{:0width$}", value, width = serial.width),
            );
        }

        Ok(self.summary(
            false,
            qso_points,
            qso_points_rule_index,
            qso_points_is_fallback,
            new_mults,
            sent_exchange,
        ))
    }

    pub fn sent_exchange_for(
        &self,
        dest: &ResolvedStation,
    ) -> Result<HashMap<String, String>, EngineError> {
        let empty = HashMap::new();
        let ctx = EvalContext {
            config: &self.config,
            source: &self.source,
            dest,
            dest_call: "",
            rcvd: &empty,
            sent: &empty,
            session: SessionEnv::default(),
        };
        let variant = self
            .spec
            .exchange
            .sent_variants
            .iter()
            .find(|v| {
                v.when
                    .as_ref()
                    .map(|p| eval_predicate(&ctx, p))
                    .unwrap_or(true)
            })
            .ok_or_else(|| EngineError::InvalidConfig("no matching sent variant".to_string()))?;

        let mut out = HashMap::new();
        for field in &variant.fields {
            let raw = match &field.value {
                SentValue::Const(v) => v.clone(),
                SentValue::Config(key) => {
                    self.config.get(key).map(Value::as_text).ok_or_else(|| {
                        EngineError::InvalidConfig(format!("missing config {}", key))
                    })?
                }
            };
            let normalized = normalize_text(&raw, field.normalize_upper_trim);
            out.insert(field.id.clone(), normalized);
        }
        if let Some(sent_rst) = self.effective_spec.exchange.sent_rst_value.as_ref() {
            out.insert("rst".to_string(), sent_rst.clone());
        }
        Ok(out)
    }

    pub fn export_cabrillo(
        &self,
        my_call: &str,
        operator_category: &str,
        entries: &[CabrilloQso],
    ) -> String {
        let mut out = String::new();
        out.push_str("START-OF-LOG: 3.0\n");
        out.push_str(&format!(
            "CONTEST: {}\n",
            self.effective_spec.cabrillo_contest
        ));
        out.push_str(&format!("CALLSIGN: {}\n", my_call));
        out.push_str(&format!("CATEGORY-OPERATOR: {}\n", operator_category));
        if let Some(category_mode) = self.effective_spec.category_mode.as_ref() {
            out.push_str(&format!("CATEGORY-MODE: {}\n", category_mode));
        }
        out.push_str(&format!("CLAIMED-SCORE: {}\n", self.claimed_score()));
        for qso in entries {
            let sent = qso.sent.join(" ");
            let rcvd = qso.received.join(" ");
            out.push_str(&format!(
                "QSO: {:>5} {:<4} {} {} {} {} {} {}\n",
                qso.freq_khz,
                mode_to_cabrillo(qso.mode),
                qso.date_utc,
                qso.time_utc,
                qso.my_call,
                sent,
                qso.their_call,
                rcvd
            ));
        }
        out.push_str("END-OF-LOG:\n");
        out
    }

    pub fn replay(
        &mut self,
        resolver: &dyn StationResolver,
        domains: &dyn DomainProvider,
        inputs: &[LogInput],
    ) -> Result<(), ReplayError> {
        for (idx, input) in inputs.iter().enumerate() {
            let _ = self
                .apply_qso(
                resolver,
                domains,
                input.band,
                input.call.clone(),
                &input.raw_exchange,
            )
            .map_err(|err| ReplayError::AtIndex(idx, err))?;
        }
        Ok(())
    }

    pub fn classify_candidate(
        &self,
        resolver: &dyn StationResolver,
        domains: &dyn DomainProvider,
        band: Band,
        call: Callsign,
        raw_exchange: &str,
    ) -> Result<CandidateSummary, EngineError> {
        self.classify_candidate_with_mode(resolver, domains, band, Mode::CW, call, raw_exchange)
    }

    pub fn classify_candidate_with_mode(
        &self,
        resolver: &dyn StationResolver,
        domains: &dyn DomainProvider,
        band: Band,
        mode: Mode,
        call: Callsign,
        raw_exchange: &str,
    ) -> Result<CandidateSummary, EngineError> {
        self.classify_candidate_at_with_mode(resolver, domains, band, mode, 0, call, raw_exchange)
    }

    pub fn classify_candidate_at_with_mode(
        &self,
        resolver: &dyn StationResolver,
        domains: &dyn DomainProvider,
        band: Band,
        mode: Mode,
        epoch_seconds: i64,
        call: Callsign,
        raw_exchange: &str,
    ) -> Result<CandidateSummary, EngineError> {
        if !self.effective_spec.allowed_modes.contains(&mode) {
            return Err(EngineError::InvalidMode {
                mode,
                allowed: self.effective_spec.allowed_modes.clone(),
            });
        }
        let dupe_scope = Self::scope_key(self.spec.dupe_dimension.clone(), band, mode, epoch_seconds);
        let dest = resolver.resolve(&call).map_err(EngineError::Resolve)?;
        let parsed = parse_received(
            &self.spec,
            &self.config,
            &self.source,
            &dest,
            raw_exchange,
            domains,
        )
        .map_err(EngineError::Exchange)?;
        let dupe_extra = self.dupe_extra_key(&parsed);
        let is_dupe = self.dupes.contains(&(
            dupe_scope.0,
            dupe_scope.1,
            dupe_scope.2,
            call.clone(),
            dupe_extra,
        ));
        let call_text = call.as_str().to_string();
        let empty = HashMap::new();
        let ctx = EvalContext {
            config: &self.config,
            source: &self.source,
            dest: &dest,
            dest_call: &call_text,
            rcvd: &parsed,
            sent: &empty,
            session: SessionEnv {
                band: Some(band),
                mode: Some(mode),
            },
        };
        let mut would_be = Vec::new();
        for mult in &self.spec.multipliers {
            let variant = match mult.variants.iter().find(|v| {
                v.when
                    .as_ref()
                    .map(|p| eval_predicate(&ctx, p))
                    .unwrap_or(true)
            }) {
                Some(v) => v,
                None => continue,
            };
            let value = match eval_key_expr(&ctx, &variant.key) {
                Some(v) => v,
                None => continue,
            };
            if !validate_value_in_domain(&value, &variant.domain, domains) {
                continue;
            }
            let seen = self
                .mults
                .get(&{
                    let s = Self::scope_key(mult.dimension.clone(), band, mode, epoch_seconds);
                    (mult.id.clone(), s.0, s.1, s.2)
                })
                .map(|s| s.contains(&value))
                .unwrap_or(false);
            if !seen {
                would_be.push(format!("{}:{}", mult.id, value));
            }
        }
        Ok(CandidateSummary {
            is_dupe,
            would_be_new_mults: would_be,
        })
    }

    pub fn classify_call(
        &self,
        resolver: &dyn StationResolver,
        domains: &dyn DomainProvider,
        band: Band,
        call: Callsign,
    ) -> Result<CandidateSummary, EngineError> {
        self.classify_call_with_mode(resolver, domains, band, Mode::CW, call)
    }

    pub fn classify_call_with_mode(
        &self,
        resolver: &dyn StationResolver,
        domains: &dyn DomainProvider,
        band: Band,
        mode: Mode,
        call: Callsign,
    ) -> Result<CandidateSummary, EngineError> {
        self.classify_call_at_with_mode(resolver, domains, band, mode, 0, call)
    }

    pub fn classify_call_at_with_mode(
        &self,
        resolver: &dyn StationResolver,
        domains: &dyn DomainProvider,
        band: Band,
        mode: Mode,
        epoch_seconds: i64,
        call: Callsign,
    ) -> Result<CandidateSummary, EngineError> {
        if !self.effective_spec.allowed_modes.contains(&mode) {
            return Err(EngineError::InvalidMode {
                mode,
                allowed: self.effective_spec.allowed_modes.clone(),
            });
        }
        let dupe_scope = Self::scope_key(self.spec.dupe_dimension.clone(), band, mode, epoch_seconds);
        // If rover identity is enabled, dupe status depends on received
        // exchange content we don't have here (classify_call takes no
        // exchange). Fall back to "not dupe": the actual duplication check
        // fires for real at apply_qso time with the full exchange in hand.
        let is_dupe = if self.spec.dupe_extra_rcvd_fields.is_empty() {
            self.dupes.contains(&(
                dupe_scope.0,
                dupe_scope.1,
                dupe_scope.2,
                call.clone(),
                None,
            ))
        } else {
            false
        };
        let dest = resolver.resolve(&call).map_err(EngineError::Resolve)?;
        let call_text = call.as_str().to_string();
        let empty = HashMap::new();
        let ctx = EvalContext {
            config: &self.config,
            source: &self.source,
            dest: &dest,
            dest_call: &call_text,
            rcvd: &empty,
            sent: &empty,
            session: SessionEnv {
                band: Some(band),
                mode: Some(mode),
            },
        };

        let mut would_be = Vec::new();
        for mult in &self.spec.multipliers {
            let variant = match mult
                .variants
                .iter()
                .filter(|v| !variant_requires_exchange_data(v))
                .find(|v| {
                    v.when
                        .as_ref()
                        .map(|p| eval_predicate(&ctx, p))
                        .unwrap_or(true)
                }) {
                Some(v) => v,
                None => continue,
            };
            let value = match eval_key_expr(&ctx, &variant.key) {
                Some(v) => v,
                None => continue,
            };
            if !validate_value_in_domain(&value, &variant.domain, domains) {
                continue;
            }
            let seen = self
                .mults
                .get(&{
                    let s = Self::scope_key(mult.dimension.clone(), band, mode, epoch_seconds);
                    (mult.id.clone(), s.0, s.1, s.2)
                })
                .map(|s| s.contains(&value))
                .unwrap_or(false);
            if !seen {
                would_be.push(format!("{}:{}", mult.id, value));
            }
        }
        Ok(CandidateSummary {
            is_dupe,
            would_be_new_mults: would_be,
        })
    }

    pub fn classify_call_lite_with_mode(
        &self,
        resolver: &dyn StationResolver,
        domains: &dyn DomainProvider,
        band: Band,
        mode: Mode,
        call: Callsign,
    ) -> Result<CandidateEvalLite, EngineError> {
        let summary = self.classify_call_with_mode(resolver, domains, band, mode, call)?;
        Ok(CandidateEvalLite {
            is_dupe: summary.is_dupe,
            new_mults: summary.would_be_new_mults,
            notes: Vec::new(),
        })
    }

    pub fn worked_mults(&self, mult_id: &str, band: Option<Band>) -> Vec<String> {
        self.worked_mults_scoped(mult_id, band, None)
    }

    pub fn worked_mults_scoped(
        &self,
        mult_id: &str,
        band: Option<Band>,
        mode: Option<Mode>,
    ) -> Vec<String> {
        let mut out = Vec::new();
        for ((id, b, m, _p), values) in &self.mults {
            if id != mult_id {
                continue;
            }
            if let Some(query_band) = band {
                if *b != Some(query_band) && b.is_some() {
                    continue;
                }
            }
            if let Some(query_mode) = mode {
                if *m != Some(query_mode) && m.is_some() {
                    continue;
                }
            }
            out.extend(values.iter().cloned());
        }
        out.sort_unstable();
        out.dedup();
        out
    }

    pub fn needed_mults(
        &self,
        domains: &dyn DomainProvider,
        mult_id: &str,
        band: Option<Band>,
    ) -> Vec<String> {
        self.needed_mults_scoped(domains, mult_id, band, None)
    }

    pub fn needed_mults_scoped(
        &self,
        domains: &dyn DomainProvider,
        mult_id: &str,
        band: Option<Band>,
        mode: Option<Mode>,
    ) -> Vec<String> {
        let worked: HashSet<String> = self
            .worked_mults_scoped(mult_id, band, mode)
            .into_iter()
            .collect();
        let mult = match self.spec.multipliers.iter().find(|m| m.id == mult_id) {
            Some(v) => v,
            None => return Vec::new(),
        };
        let empty = HashMap::new();
        let ctx = EvalContext {
            config: &self.config,
            source: &self.source,
            dest: &self.source,
            dest_call: "",
            rcvd: &empty,
            sent: &empty,
            session: SessionEnv::default(),
        };
        let variant = match mult.variants.iter().find(|v| {
            v.when
                .as_ref()
                .map(|p| eval_predicate(&ctx, p))
                .unwrap_or(true)
        }) {
            Some(v) => v,
            None => return Vec::new(),
        };
        let mut all = match &variant.domain {
            DomainRef::Any => Vec::new(),
            DomainRef::Range { min, max } => (*min..=*max).map(|v| v.to_string()).collect(),
            DomainRef::External { name } => domains
                .values(name)
                .map(|vals| vals.as_ref().to_vec())
                .unwrap_or_default(),
            DomainRef::List(v) => v.clone(),
        };
        all.retain(|v| !worked.contains(v));
        all.sort_unstable();
        all
    }

    pub fn total_points(&self) -> i64 {
        self.total_points
    }

    pub fn total_qsos(&self) -> u32 {
        self.total_qsos
    }

    pub fn selected_variant(&self) -> Option<&str> {
        self.effective_spec.selected_variant.as_deref()
    }

    pub fn allowed_modes(&self) -> &[Mode] {
        &self.effective_spec.allowed_modes
    }

    pub fn effective_cabrillo_contest(&self) -> &str {
        &self.effective_spec.cabrillo_contest
    }

    pub fn total_mults(&self) -> usize {
        self.mults.values().map(HashSet::len).sum()
    }

    fn sum_band_mults(&self) -> usize {
        self.mults
            .iter()
            .filter(|((_id, band, _mode, _period), _values)| band.is_some())
            .map(|(_, values)| values.len())
            .sum()
    }

    fn mults_for_band(&self, band: Band) -> usize {
        self.mults
            .iter()
            .filter(|((_id, b, _m, _p), _values)| b.is_none() || *b == Some(band))
            .map(|(_, values)| values.len())
            .sum()
    }

    pub fn claimed_score(&self) -> i64 {
        let base = match self.spec.score.formula {
            ScoreFormula::PointsTimesTotalMults => self.total_points * self.total_mults() as i64,
            ScoreFormula::PointsTimesSumBandMults => self.total_points * self.sum_band_mults() as i64,
            ScoreFormula::SumBandPointsTimesBandMults => self
                .points_by_band
                .iter()
                .map(|(band, points)| points * self.mults_for_band(*band) as i64)
                .sum(),
        };
        let scaled = base * self.score_multiplier_factor() as i64;
        scaled + self.bonus.total
    }

    /// Compute the whole-score multiplier factor for the operator. Returns
    /// 1 when no `score_multiplier` is configured, which preserves the
    /// historical scoring formula byte-for-byte.
    fn score_multiplier_factor(&self) -> u32 {
        let Some(sm) = self.spec.score_multiplier.as_ref() else {
            return 1;
        };
        let Some(value) = self.config.get(&sm.from_config) else {
            return sm.default;
        };
        let text = value.as_text().to_ascii_uppercase();
        for (k, v) in &sm.mapping {
            if k.eq_ignore_ascii_case(&text) {
                return *v;
            }
        }
        sm.default
    }

    /// Total additive bonus points awarded by `bonus_rules` so far.
    /// Returns 0 for any spec without bonus rules.
    pub fn bonus_total(&self) -> i64 {
        self.bonus.total
    }

    fn compute_points(
        &self,
        ctx: &EvalContext<'_>,
        domains: &dyn DomainProvider,
    ) -> (i64, Option<usize>, bool) {
        for (idx, rule) in self.spec.points.iter().enumerate() {
            let pass = rule
                .when
                .as_ref()
                .map(|w| eval_predicate_with_domains(ctx, w, domains))
                .unwrap_or(true);
            if pass {
                return (rule.value, Some(idx), rule.when.is_none());
            }
        }
        (0, None, false)
    }

    fn summary(
        &self,
        is_dupe: bool,
        qso_points: i64,
        qso_points_rule_index: Option<usize>,
        qso_points_is_fallback: bool,
        new_mults: Vec<String>,
        sent_exchange: HashMap<String, String>,
    ) -> ApplySummary {
        ApplySummary {
            is_dupe,
            qso_points,
            qso_points_rule_index,
            qso_points_is_fallback,
            new_mults,
            sent_exchange,
            total_qsos: self.total_qsos(),
            total_points: self.total_points(),
            total_mults: self.total_mults(),
            claimed_score: self.claimed_score(),
        }
    }
}

impl<R, D> SpecSession<R, D>
where
    R: StationResolver,
    D: DomainProvider,
{
    pub fn new(
        spec: ContestSpec,
        source: ResolvedStation,
        config: HashMap<String, Value>,
        resolver: R,
        domains: D,
    ) -> Result<Self, EngineError> {
        Self::new_with_meta(
            spec,
            source,
            config,
            resolver,
            domains,
            SessionMeta::default(),
        )
    }

    pub fn new_with_meta(
        spec: ContestSpec,
        source: ResolvedStation,
        config: HashMap<String, Value>,
        resolver: R,
        domains: D,
        meta: SessionMeta,
    ) -> Result<Self, EngineError> {
        validate_config(&spec, &source, &config, Some(&domains))?;
        Ok(Self {
            engine: SpecEngine::new_with_meta(spec, source, config, meta)?,
            resolver,
            domains,
        })
    }

    #[deprecated(note = "use SpecSession::new instead")]
    pub fn new_legacy(
        spec: ContestSpec,
        source: ResolvedStation,
        config: HashMap<String, Value>,
        resolver: R,
        domains: D,
    ) -> Result<Self, EngineError> {
        Self::new(spec, source, config, resolver, domains)
    }

    pub fn from_engine(engine: SpecEngine, resolver: R, domains: D) -> Self {
        Self {
            engine,
            resolver,
            domains,
        }
    }

    pub fn apply_qso(
        &mut self,
        band: Band,
        call: Callsign,
        raw_exchange: &str,
    ) -> Result<ApplySummary, EngineError> {
        self.engine
            .apply_qso(&self.resolver, &self.domains, band, call, raw_exchange)
    }

    pub fn apply_qso_with_mode(
        &mut self,
        band: Band,
        mode: Mode,
        call: Callsign,
        raw_exchange: &str,
    ) -> Result<ApplySummary, EngineError> {
        self.engine
            .apply_qso_with_mode(&self.resolver, &self.domains, band, mode, call, raw_exchange)
    }

    pub fn apply_qso_at_with_mode(
        &mut self,
        band: Band,
        mode: Mode,
        epoch_seconds: i64,
        call: Callsign,
        raw_exchange: &str,
    ) -> Result<ApplySummary, EngineError> {
        self.engine.apply_qso_at_with_mode(
            &self.resolver,
            &self.domains,
            band,
            mode,
            epoch_seconds,
            call,
            raw_exchange,
        )
    }

    pub fn classify_candidate(
        &self,
        band: Band,
        call: Callsign,
        raw_exchange: &str,
    ) -> Result<CandidateSummary, EngineError> {
        self.engine
            .classify_candidate(&self.resolver, &self.domains, band, call, raw_exchange)
    }

    pub fn classify_candidate_with_mode(
        &self,
        band: Band,
        mode: Mode,
        call: Callsign,
        raw_exchange: &str,
    ) -> Result<CandidateSummary, EngineError> {
        self.engine.classify_candidate_with_mode(
            &self.resolver,
            &self.domains,
            band,
            mode,
            call,
            raw_exchange,
        )
    }

    pub fn classify_candidate_at_with_mode(
        &self,
        band: Band,
        mode: Mode,
        epoch_seconds: i64,
        call: Callsign,
        raw_exchange: &str,
    ) -> Result<CandidateSummary, EngineError> {
        self.engine.classify_candidate_at_with_mode(
            &self.resolver,
            &self.domains,
            band,
            mode,
            epoch_seconds,
            call,
            raw_exchange,
        )
    }

    pub fn classify_call(
        &self,
        band: Band,
        call: Callsign,
    ) -> Result<CandidateSummary, EngineError> {
        self.engine
            .classify_call(&self.resolver, &self.domains, band, call)
    }

    pub fn classify_call_with_mode(
        &self,
        band: Band,
        mode: Mode,
        call: Callsign,
    ) -> Result<CandidateSummary, EngineError> {
        self.engine
            .classify_call_with_mode(&self.resolver, &self.domains, band, mode, call)
    }

    pub fn classify_call_lite_with_mode(
        &self,
        band: Band,
        mode: Mode,
        call: Callsign,
    ) -> Result<CandidateEvalLite, EngineError> {
        self.engine
            .classify_call_lite_with_mode(&self.resolver, &self.domains, band, mode, call)
    }

    pub fn classify_call_at_with_mode(
        &self,
        band: Band,
        mode: Mode,
        epoch_seconds: i64,
        call: Callsign,
    ) -> Result<CandidateSummary, EngineError> {
        self.engine.classify_call_at_with_mode(
            &self.resolver,
            &self.domains,
            band,
            mode,
            epoch_seconds,
            call,
        )
    }

    pub fn worked_mults(&self, mult_id: &str, band: Option<Band>) -> Vec<String> {
        self.engine.worked_mults(mult_id, band)
    }

    pub fn worked_mults_scoped(
        &self,
        mult_id: &str,
        band: Option<Band>,
        mode: Option<Mode>,
    ) -> Vec<String> {
        self.engine.worked_mults_scoped(mult_id, band, mode)
    }

    pub fn needed_mults(&self, mult_id: &str, band: Option<Band>) -> Vec<String> {
        self.engine.needed_mults(&self.domains, mult_id, band)
    }

    pub fn needed_mults_scoped(
        &self,
        mult_id: &str,
        band: Option<Band>,
        mode: Option<Mode>,
    ) -> Vec<String> {
        self.engine
            .needed_mults_scoped(&self.domains, mult_id, band, mode)
    }

    pub fn engine(&self) -> &SpecEngine {
        &self.engine
    }

    /// Returns a mutable reference to the resolver, allowing callers to
    /// insert new station entries after session construction.
    pub fn resolver_mut(&mut self) -> &mut R {
        &mut self.resolver
    }

    pub fn replay(&mut self, inputs: &[LogInput]) -> Result<(), ReplayError> {
        self.engine.replay(&self.resolver, &self.domains, inputs)
    }

    pub fn sent_exchange_for_call(
        &self,
        call: Callsign,
    ) -> Result<HashMap<String, String>, EngineError> {
        let dest = self.resolver.resolve(&call).map_err(EngineError::Resolve)?;
        self.engine.sent_exchange_for(&dest)
    }

    pub fn export_cabrillo(
        &self,
        my_call: &str,
        operator_category: &str,
        entries: &[CabrilloQso],
    ) -> String {
        self.engine
            .export_cabrillo(my_call, operator_category, entries)
    }
}

fn mode_to_cabrillo(mode: Mode) -> &'static str {
    match mode {
        Mode::CW => "CW",
        Mode::SSB => "PH",
        Mode::RTTY => "RY",
        Mode::FT8 | Mode::FT4 | Mode::DIGITAL => "DG",
    }
}

fn validate_config(
    spec: &ContestSpec,
    source: &ResolvedStation,
    config: &HashMap<String, Value>,
    domains: Option<&dyn DomainProvider>,
) -> Result<(), EngineError> {
    let empty = HashMap::new();
    let ctx = EvalContext {
        config,
        source,
        dest: source,
        dest_call: "",
        rcvd: &empty,
        sent: &empty,
        session: SessionEnv::default(),
    };
    for field in &spec.config_fields {
        let conditional = field
            .required_when
            .as_ref()
            .map(|p| eval_predicate(&ctx, p))
            .unwrap_or(false);
        let required = field.required || conditional;
        if required && !config.contains_key(&field.id) {
            return Err(EngineError::InvalidConfig(format!(
                "missing config {}",
                field.id
            )));
        }
        if let (Some(value), Some(domain)) = (config.get(&field.id), field.domain.as_ref()) {
            let text = value.as_text();
            let valid = match domain {
                DomainRef::Any => true,
                DomainRef::Range { min, max } => value
                    .as_i64()
                    .map(|v| v >= *min && v <= *max)
                    .unwrap_or(false),
                DomainRef::List(items) => items.contains(&text),
                DomainRef::External { name } => match domains {
                    Some(provider) => provider
                        .values(name)
                        .map(|vals| {
                            vals.contains(&text.to_ascii_uppercase()) || vals.contains(&text)
                        })
                        .unwrap_or(false),
                    None => true,
                },
            };
            if !valid {
                return Err(EngineError::InvalidConfig(format!(
                    "invalid config {} value {}",
                    field.id, text
                )));
            }
        }
    }
    Ok(())
}

#[derive(Debug, Default, Clone)]
pub struct InMemoryDomainProvider {
    values: HashMap<String, Arc<[String]>>,
}

impl InMemoryDomainProvider {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, name: impl AsRef<str>, values: Vec<String>) {
        let normalized: Vec<String> = values
            .into_iter()
            .map(|v| v.trim().to_ascii_uppercase())
            .collect();
        self.values
            .insert(name.as_ref().to_string(), Arc::<[String]>::from(normalized));
    }
}

impl DomainProvider for InMemoryDomainProvider {
    fn values(&self, domain_name: &str) -> Option<Arc<[String]>> {
        self.values.get(domain_name).cloned()
    }
}

pub mod domain_packs {
    use super::InMemoryDomainProvider;
    use std::fs;
    use std::path::Path;

    pub fn load_domain_file(path: impl AsRef<Path>) -> Result<Vec<String>, String> {
        let path_ref = path.as_ref();
        let raw = fs::read_to_string(path_ref)
            .map_err(|e| format!("failed reading {}: {}", path_ref.display(), e))?;
        parse_domain_file_content(&raw)
    }

    /// Parse the raw content of a domain file (one value per line, `#` for comments).
    /// Used by both the filesystem loader and the embedded loader.
    pub(crate) fn parse_domain_file_content(raw: &str) -> Result<Vec<String>, String> {
        let mut out = Vec::new();
        for line in raw.lines() {
            let trimmed = line.trim();
            if trimmed.is_empty() || trimmed.starts_with('#') {
                continue;
            }
            out.push(trimmed.to_ascii_uppercase());
        }
        out.sort_unstable();
        out.dedup();
        Ok(out)
    }

    pub fn load_standard_domain_pack(
        root: impl AsRef<Path>,
    ) -> Result<InMemoryDomainProvider, String> {
        let root = root.as_ref();
        let mut provider = InMemoryDomainProvider::new();
        provider.insert(
            "arrl_dx_wve_multipliers",
            load_domain_file(root.join("arrl_dx_wve_multipliers.txt"))?,
        );
        provider.insert(
            "naqp_multipliers",
            load_domain_file(root.join("naqp_multipliers.txt"))?,
        );
        provider.insert(
            "dxcc_entities_excluding_w_ve",
            load_domain_file(root.join("dxcc_entities_excluding_w_ve.txt"))?,
        );
        provider.insert(
            "dxcc_entities",
            load_domain_file(root.join("dxcc_entities.txt"))?,
        );
        Ok(provider)
    }
}

/// Embedded, compile-time specs and domain data.
///
/// This module bakes the contents of `specs/*.json` and `specs/domains/*.txt`
/// into the binary using `include_str!`, so consumers don't need the contest-engine
/// source tree on disk at runtime. This is the recommended way to consume
/// contest specs in deployed applications.
pub mod embedded {
    use super::domain_packs::parse_domain_file_content;
    use super::{ContestSpec, InMemoryDomainProvider};

    // Contest specs (JSON)
    const CQWW_JSON: &str = include_str!("../specs/cqww.json");
    const CQWW_CW_JSON: &str = include_str!("../specs/cqww_cw.json");
    const CWT_JSON: &str = include_str!("../specs/cwt.json");
    const ARRL_DX_JSON: &str = include_str!("../specs/arrl_dx.json");
    const NAQP_JSON: &str = include_str!("../specs/naqp.json");
    const MST_JSON: &str = include_str!("../specs/mst.json");
    const NS_SPRINT_JSON: &str = include_str!("../specs/ns_sprint.json");
    const SS_JSON: &str = include_str!("../specs/ss.json");
    const NHQP_JSON: &str = include_str!("../specs/nhqp.json");
    const GAQP_JSON: &str = include_str!("../specs/gaqp.json");
    const NDQP_JSON: &str = include_str!("../specs/ndqp.json");
    const MIQP_JSON: &str = include_str!("../specs/miqp.json");
    const INQP_JSON: &str = include_str!("../specs/inqp.json");
    const NEQP_JSON: &str = include_str!("../specs/neqp.json");
    const MOQP_JSON: &str = include_str!("../specs/moqp.json");
    const QCQP_JSON: &str = include_str!("../specs/qcqp.json");
    const ONQP_JSON: &str = include_str!("../specs/onqp.json");
    const DEQP_JSON: &str = include_str!("../specs/deqp.json");
    const NMQP_JSON: &str = include_str!("../specs/nmqp.json");
    const NEQSOP_JSON: &str = include_str!("../specs/neqsop.json");
    const FLQP_JSON: &str = include_str!("../specs/flqp.json");

    // Domain files (plain text, one value per line)
    const ARRL_DX_WVE_MULTS: &str =
        include_str!("../specs/domains/arrl_dx_wve_multipliers.txt");
    const NAQP_MULTS: &str = include_str!("../specs/domains/naqp_multipliers.txt");
    const DXCC_ENTITIES: &str = include_str!("../specs/domains/dxcc_entities.txt");
    const DXCC_ENTITIES_EX_WVE: &str =
        include_str!("../specs/domains/dxcc_entities_excluding_w_ve.txt");
    const NS_SPRINT_MULTS: &str =
        include_str!("../specs/domains/ns_sprint_multipliers.txt");
    const SS_SECTIONS: &str = include_str!("../specs/domains/ss_sections.txt");
    const NH_COUNTIES: &str = include_str!("../specs/domains/nh_counties.txt");
    const NH_IN_STATE_MULTS: &str = include_str!("../specs/domains/nh_in_state_mults.txt");
    const US_STATES: &str = include_str!("../specs/domains/us_states.txt");
    const CA_PROVINCES: &str = include_str!("../specs/domains/ca_provinces.txt");
    const GA_COUNTIES: &str = include_str!("../specs/domains/ga_counties.txt");
    const ND_COUNTIES: &str = include_str!("../specs/domains/nd_counties.txt");
    const MI_COUNTIES: &str = include_str!("../specs/domains/mi_counties.txt");
    const IN_COUNTIES: &str = include_str!("../specs/domains/in_counties.txt");
    const NEQP_COUNTIES: &str = include_str!("../specs/domains/neqp_counties.txt");
    const MO_COUNTIES: &str = include_str!("../specs/domains/mo_counties.txt");
    const MOQP_BONUS_STATIONS: &str =
        include_str!("../specs/domains/moqp_bonus_stations.txt");
    const QC_REGIONS: &str = include_str!("../specs/domains/qc_regions.txt");
    const ON_MULTIPLIER_AREAS: &str =
        include_str!("../specs/domains/on_multiplier_areas.txt");
    const ONQP_BONUS_STATIONS: &str =
        include_str!("../specs/domains/onqp_bonus_stations.txt");
    const DE_COUNTIES: &str = include_str!("../specs/domains/de_counties.txt");
    const NM_COUNTIES: &str = include_str!("../specs/domains/nm_counties.txt");
    const NE_COUNTIES: &str = include_str!("../specs/domains/ne_counties.txt");
    const NEQSOP_BONUS_STATIONS: &str =
        include_str!("../specs/domains/neqsop_bonus_stations.txt");
    const FL_COUNTIES: &str = include_str!("../specs/domains/fl_counties.txt");

    /// All embedded contest spec IDs, in the order they're tried by lookups.
    pub const SPEC_IDS: &[&str] = &[
        "cqww",
        "cqww_cw",
        "cwt",
        "arrl_dx",
        "naqp",
        "mst",
        "ns_sprint",
        "ss",
        "nhqp",
        "gaqp",
        "ndqp",
        "miqp",
        "inqp",
        "neqp",
        "moqp",
        "qcqp",
        "onqp",
        "deqp",
        "nmqp",
        "neqsop",
        "flqp",
    ];

    /// Look up an embedded contest spec by its ID (the filename stem, without `.json`).
    ///
    /// Returns `None` if no embedded spec matches. The returned `ContestSpec` is
    /// parsed fresh from the embedded JSON each call; callers that look up the
    /// same spec repeatedly may want to cache it.
    pub fn spec_by_id(id: &str) -> Option<ContestSpec> {
        let id = id.trim().to_ascii_lowercase();
        let json = match id.as_str() {
            "cqww" => CQWW_JSON,
            "cqww_cw" => CQWW_CW_JSON,
            "cwt" => CWT_JSON,
            "arrl_dx" => ARRL_DX_JSON,
            "naqp" => NAQP_JSON,
            "mst" => MST_JSON,
            "ns_sprint" => NS_SPRINT_JSON,
            "ss" => SS_JSON,
            "nhqp" => NHQP_JSON,
            "gaqp" => GAQP_JSON,
            "ndqp" => NDQP_JSON,
            "miqp" => MIQP_JSON,
            "inqp" => INQP_JSON,
            "neqp" => NEQP_JSON,
            "moqp" => MOQP_JSON,
            "qcqp" => QCQP_JSON,
            "onqp" => ONQP_JSON,
            "deqp" => DEQP_JSON,
            "nmqp" => NMQP_JSON,
            "neqsop" => NEQSOP_JSON,
            "flqp" => FLQP_JSON,
            _ => return None,
        };
        ContestSpec::from_json_str(json).ok()
    }

    /// Build the standard domain provider from embedded domain files.
    ///
    /// Equivalent to `domain_packs::load_standard_domain_pack` but without
    /// touching the filesystem. Provides all the domains referenced by the
    /// embedded contest specs.
    pub fn standard_domain_pack() -> InMemoryDomainProvider {
        let mut provider = InMemoryDomainProvider::new();
        provider.insert(
            "arrl_dx_wve_multipliers",
            parse_domain_file_content(ARRL_DX_WVE_MULTS)
                .expect("embedded arrl_dx_wve_multipliers parse"),
        );
        provider.insert(
            "naqp_multipliers",
            parse_domain_file_content(NAQP_MULTS).expect("embedded naqp_multipliers parse"),
        );
        provider.insert(
            "dxcc_entities_excluding_w_ve",
            parse_domain_file_content(DXCC_ENTITIES_EX_WVE)
                .expect("embedded dxcc_entities_excluding_w_ve parse"),
        );
        provider.insert(
            "dxcc_entities",
            parse_domain_file_content(DXCC_ENTITIES).expect("embedded dxcc_entities parse"),
        );
        provider.insert(
            "ns_sprint_multipliers",
            parse_domain_file_content(NS_SPRINT_MULTS)
                .expect("embedded ns_sprint_multipliers parse"),
        );
        provider.insert(
            "ss_sections",
            parse_domain_file_content(SS_SECTIONS)
                .expect("embedded ss_sections parse"),
        );
        provider.insert(
            "nh_counties",
            parse_domain_file_content(NH_COUNTIES).expect("embedded nh_counties parse"),
        );
        provider.insert(
            "nh_in_state_mults",
            parse_domain_file_content(NH_IN_STATE_MULTS)
                .expect("embedded nh_in_state_mults parse"),
        );
        provider.insert(
            "us_states",
            parse_domain_file_content(US_STATES).expect("embedded us_states parse"),
        );
        provider.insert(
            "ca_provinces",
            parse_domain_file_content(CA_PROVINCES).expect("embedded ca_provinces parse"),
        );
        provider.insert(
            "ga_counties",
            parse_domain_file_content(GA_COUNTIES).expect("embedded ga_counties parse"),
        );
        provider.insert(
            "nd_counties",
            parse_domain_file_content(ND_COUNTIES).expect("embedded nd_counties parse"),
        );
        provider.insert(
            "mi_counties",
            parse_domain_file_content(MI_COUNTIES).expect("embedded mi_counties parse"),
        );
        provider.insert(
            "in_counties",
            parse_domain_file_content(IN_COUNTIES).expect("embedded in_counties parse"),
        );
        provider.insert(
            "neqp_counties",
            parse_domain_file_content(NEQP_COUNTIES).expect("embedded neqp_counties parse"),
        );
        provider.insert(
            "mo_counties",
            parse_domain_file_content(MO_COUNTIES).expect("embedded mo_counties parse"),
        );
        provider.insert(
            "moqp_bonus_stations",
            parse_domain_file_content(MOQP_BONUS_STATIONS)
                .expect("embedded moqp_bonus_stations parse"),
        );
        provider.insert(
            "qc_regions",
            parse_domain_file_content(QC_REGIONS).expect("embedded qc_regions parse"),
        );
        provider.insert(
            "on_multiplier_areas",
            parse_domain_file_content(ON_MULTIPLIER_AREAS)
                .expect("embedded on_multiplier_areas parse"),
        );
        provider.insert(
            "onqp_bonus_stations",
            parse_domain_file_content(ONQP_BONUS_STATIONS)
                .expect("embedded onqp_bonus_stations parse"),
        );
        provider.insert(
            "de_counties",
            parse_domain_file_content(DE_COUNTIES).expect("embedded de_counties parse"),
        );
        provider.insert(
            "nm_counties",
            parse_domain_file_content(NM_COUNTIES).expect("embedded nm_counties parse"),
        );
        provider.insert(
            "ne_counties",
            parse_domain_file_content(NE_COUNTIES).expect("embedded ne_counties parse"),
        );
        provider.insert(
            "neqsop_bonus_stations",
            parse_domain_file_content(NEQSOP_BONUS_STATIONS)
                .expect("embedded neqsop_bonus_stations parse"),
        );
        provider.insert(
            "fl_counties",
            parse_domain_file_content(FL_COUNTIES).expect("embedded fl_counties parse"),
        );
        provider
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn all_declared_spec_ids_parse() {
            for id in SPEC_IDS {
                let spec = spec_by_id(id).unwrap_or_else(|| {
                    panic!("embedded spec '{}' not found via spec_by_id", id)
                });
                // Basic sanity: every contest has an id field
                assert!(
                    !spec.id.is_empty(),
                    "embedded spec '{}' has empty id after parse",
                    id
                );
            }
        }

        #[test]
        fn spec_by_id_unknown_returns_none() {
            assert!(spec_by_id("not_a_real_contest").is_none());
        }

        #[test]
        fn spec_by_id_is_case_insensitive() {
            assert!(spec_by_id("CQWW").is_some());
            assert!(spec_by_id("CqWw").is_some());
        }

        #[test]
        fn embedded_domain_pack_has_all_four_domains() {
            let provider = standard_domain_pack();
            use super::super::DomainProvider;
            assert!(provider.values("arrl_dx_wve_multipliers").is_some());
            assert!(provider.values("naqp_multipliers").is_some());
            assert!(provider.values("dxcc_entities").is_some());
            assert!(provider.values("dxcc_entities_excluding_w_ve").is_some());
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct InMemoryResolver {
    values: HashMap<String, ResolvedStation>,
}

impl InMemoryResolver {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, call: impl AsRef<str>, station: ResolvedStation) {
        self.values
            .insert(call.as_ref().trim().to_ascii_uppercase(), station);
    }
}

impl StationResolver for InMemoryResolver {
    fn resolve(&self, call: &Callsign) -> Result<ResolvedStation, String> {
        self.values
            .get(call.as_str())
            .cloned()
            .ok_or_else(|| format!("unknown callsign {}", call.as_str()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn load_spec(id: &str) -> ContestSpec {
        let path = format!("{}/specs/{id}.json", env!("CARGO_MANIFEST_DIR"));
        ContestSpec::from_path(path).expect("spec should load")
    }

    fn base_domains() -> InMemoryDomainProvider {
        let mut d = InMemoryDomainProvider::new();
        d.insert(
            "dxcc_entities",
            vec![
                "W".to_string(),
                "VE".to_string(),
                "DL".to_string(),
                "G".to_string(),
            ],
        );
        d.insert(
            "dxcc_entities_excluding_w_ve",
            vec!["DL".to_string(), "G".to_string()],
        );
        d.insert(
            "arrl_dx_wve_multipliers",
            vec!["MA".to_string(), "NH".to_string(), "ON".to_string()],
        );
        d.insert(
            "naqp_multipliers",
            vec![
                "MA".to_string(),
                "NH".to_string(),
                "ON".to_string(),
                "NT".to_string(),
                "VE1".to_string(),
            ],
        );
        d
    }

    #[test]
    fn rst_field_accepts_and_preserves_logged_values() {
        let domains = base_domains();
        let rst = ExchangeField::required("rst", FieldType::Rst);
        let cases = [
            ("59", "59"),
            ("57", "57"),
            ("599", "599"),
            ("579", "579"),
            ("449", "449"),
            ("5nn", "5NN"),
            (" 58 ", "58"),
        ];

        for (input, expected) in cases {
            let parsed = parse_field(&rst, Some(input), &domains).unwrap().unwrap();
            assert_eq!(parsed, Value::Text(expected.to_string()));
        }
    }

    #[test]
    fn rst_field_rejects_invalid_values() {
        let domains = base_domains();
        let rst = ExchangeField::required("rst", FieldType::Rst);

        for input in ["5999", "5N", "NN", "9NN", "00", "A59"] {
            let err = parse_field(&rst, Some(input), &domains).unwrap_err();
            assert_eq!(err.kind, ExchangeErrorKind::Invalid);
        }
    }

    #[test]
    fn compact_rst_plus_serial_parses_with_non_599_rst() {
        let spec = load_spec("cqww_cw");
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let dest = ResolvedStation::new("DL", Continent::EU, false, false);
        let config = HashMap::new();

        let parsed = parse_received(&spec, &config, &source, &dest, "44914", &domains).unwrap();
        assert_eq!(parsed.get("rst"), Some(&Value::Text("449".to_string())));
        assert_eq!(parsed.get("zone"), Some(&Value::Int(14)));
    }

    #[test]
    fn cqww_spec_scores_and_tracks_mults() {
        let spec = load_spec("cqww_cw");
        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "DL1ABC",
            ResolvedStation::new("DL", Continent::EU, false, false),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_cq_zone".to_string(), Value::Int(5));
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        let result = engine
            .apply_qso(
                &resolver,
                &domains,
                Band::B20,
                Callsign::new("DL1ABC"),
                "5NN 14",
            )
            .unwrap();

        assert!(!result.is_dupe);
        assert_eq!(result.qso_points, 3);
        assert_eq!(engine.total_mults(), 2);
        assert_eq!(engine.claimed_score(), 6);
    }

    #[test]
    fn arrl_dx_validates_wve_to_dx_only() {
        let spec = load_spec("arrl_dx");
        let mut resolver = InMemoryResolver::new();
        resolver.insert("K1ZZ", ResolvedStation::new("W", Continent::NA, true, true));
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_is_wve".to_string(), Value::Bool(true));
        config.insert(
            "my_state_province".to_string(),
            Value::Text("MA".to_string()),
        );
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        let err = engine
            .apply_qso(
                &resolver,
                &domains,
                Band::B20,
                Callsign::new("K1ZZ"),
                "599 100",
            )
            .unwrap_err();
        assert!(matches!(err, EngineError::InvalidQso(_)));
    }

    #[test]
    fn arrl_dx_dx_station_counts_sp_multiplier_from_exchange() {
        let spec = load_spec("arrl_dx");
        let mut resolver = InMemoryResolver::new();
        resolver.insert("K1AR", ResolvedStation::new("W", Continent::NA, true, true));
        let domains = base_domains();
        let source = ResolvedStation::new("DL", Continent::EU, false, false);
        let mut config = HashMap::new();
        config.insert("my_is_wve".to_string(), Value::Bool(false));
        config.insert("my_power_sent".to_string(), Value::Text("100".to_string()));
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        let result = engine
            .apply_qso(
                &resolver,
                &domains,
                Band::B20,
                Callsign::new("K1AR"),
                "599 MA",
            )
            .unwrap();
        assert_eq!(result.qso_points, 3);
        assert_eq!(engine.worked_mults("dx_mult", Some(Band::B20)), vec!["MA"]);
    }

    #[test]
    fn naqp_non_na_exchange_has_no_multiplier() {
        let spec = load_spec("naqp");
        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "JA1ABC",
            ResolvedStation::new("JA", Continent::AS, false, false),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_name".to_string(), Value::Text("CHRIS".to_string()));
        config.insert("my_loc".to_string(), Value::Text("MA".to_string()));
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        let result = engine
            .apply_qso(
                &resolver,
                &domains,
                Band::B20,
                Callsign::new("JA1ABC"),
                "TARO DX",
            )
            .unwrap();
        assert_eq!(result.qso_points, 1);
        assert!(result.new_mults.is_empty());
    }

    #[test]
    fn needed_mults_returns_domain_minus_worked() {
        let spec = load_spec("cqww_cw");
        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "DL1ABC",
            ResolvedStation::new("DL", Continent::EU, false, false),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_cq_zone".to_string(), Value::Int(5));
        let mut engine = SpecEngine::new(spec, source, config).unwrap();
        let _ = engine.apply_qso(
            &resolver,
            &domains,
            Band::B20,
            Callsign::new("DL1ABC"),
            "599 14",
        );
        let needed_zone = engine.needed_mults(&domains, "zone", Some(Band::B20));
        assert!(!needed_zone.contains(&"14".to_string()));
    }

    #[test]
    fn spec_session_wraps_apply_and_queries() {
        let spec = load_spec("cqww_cw");
        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "DL1ABC",
            ResolvedStation::new("DL", Continent::EU, false, false),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_cq_zone".to_string(), Value::Int(5));
        let mut session = SpecSession::new(spec, source, config, resolver, domains).unwrap();
        let _ = session
            .apply_qso(Band::B20, Callsign::new("DL1ABC"), "599 14")
            .unwrap();

        assert_eq!(session.engine().total_qsos(), 1);
        assert_eq!(session.worked_mults("country", Some(Band::B20)), vec!["DL"]);
        let needed = session.needed_mults("zone", Some(Band::B20));
        assert!(!needed.contains(&"14".to_string()));
    }

    #[test]
    fn classify_call_without_exchange_uses_dest_only_multiplier_data() {
        let spec = load_spec("cqww_cw");
        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "DL1ABC",
            ResolvedStation::new("DL", Continent::EU, false, false),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_cq_zone".to_string(), Value::Int(5));
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        let pre = engine
            .classify_call(&resolver, &domains, Band::B20, Callsign::new("DL1ABC"))
            .unwrap();
        assert!(!pre.is_dupe);
        assert_eq!(pre.would_be_new_mults, vec!["country:DL"]);

        let _ = engine
            .apply_qso(
                &resolver,
                &domains,
                Band::B20,
                Callsign::new("DL1ABC"),
                "599 14",
            )
            .unwrap();

        let post = engine
            .classify_call(&resolver, &domains, Band::B20, Callsign::new("DL1ABC"))
            .unwrap();
        assert!(post.is_dupe);
        assert!(post.would_be_new_mults.is_empty());
    }

    #[test]
    fn replay_reports_indexed_error() {
        let spec = load_spec("arrl_dx");
        let mut resolver = InMemoryResolver::new();
        resolver.insert("K1AR", ResolvedStation::new("W", Continent::NA, true, true));
        let domains = base_domains();
        let source = ResolvedStation::new("DL", Continent::EU, false, false);
        let mut config = HashMap::new();
        config.insert("my_is_wve".to_string(), Value::Bool(false));
        config.insert("my_power_sent".to_string(), Value::Text("100".to_string()));
        let mut engine = SpecEngine::new(spec, source, config).unwrap();
        let inputs = vec![
            LogInput {
                band: Band::B20,
                call: Callsign::new("K1AR"),
                raw_exchange: "599 MA".to_string(),
            },
            LogInput {
                band: Band::B20,
                call: Callsign::new("UNKNOWN"),
                raw_exchange: "599 NH".to_string(),
            },
        ];

        let err = engine.replay(&resolver, &domains, &inputs).unwrap_err();
        assert!(matches!(
            err,
            ReplayError::AtIndex(1, EngineError::Resolve(_))
        ));
    }

    #[test]
    fn contest_spec_files_load() {
        let cqww = load_spec("cqww_cw");
        assert_eq!(cqww.name, "CQ World-Wide DX Contest (CW)");

        let arrl = load_spec("arrl_dx");
        assert_eq!(arrl.name, "ARRL International DX Contest");

        let naqp = load_spec("naqp");
        assert_eq!(naqp.name, "North American QSO Party");

        let cwt = load_spec("cwt");
        assert_eq!(cwt.name, "CWops CWT");
    }

    // ---------- Rover identity (dupe_extra_rcvd_fields) tests ----------

    /// When `dupe_extra_rcvd_fields` is empty, the dupe check is the historical
    /// 4-tuple behavior: same call on same band/mode is a dupe.
    #[test]
    fn rover_identity_disabled_preserves_classic_dupe_semantics() {
        let mut spec = load_spec("naqp");
        assert!(spec.dupe_extra_rcvd_fields.is_empty());
        spec.dupe_dimension = Dimension::BandMode;

        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "K1ABC",
            ResolvedStation::new("W", Continent::NA, true, true),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_name".to_string(), Value::Text("CHRIS".to_string()));
        config.insert("my_loc".to_string(), Value::Text("MA".to_string()));
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        let a = engine
            .apply_qso_with_mode(
                &resolver,
                &domains,
                Band::B20,
                Mode::CW,
                Callsign::new("K1ABC"),
                "MIKE NH",
            )
            .unwrap();
        assert!(!a.is_dupe);

        // Same call, same band/mode, DIFFERENT loc (`NH` vs earlier `NY`).
        // Without rover identity, this still counts as a dupe.
        let b = engine
            .apply_qso_with_mode(
                &resolver,
                &domains,
                Band::B20,
                Mode::CW,
                Callsign::new("K1ABC"),
                "MIKE NH",
            )
            .unwrap();
        assert!(
            b.is_dupe,
            "without dupe_extra_rcvd_fields, second QSO on same band/mode should dupe"
        );
    }

    /// When rover identity is enabled (dupe_extra_rcvd_fields includes "loc"),
    /// the same callsign sending two distinct locations counts as two QSOs.
    #[test]
    fn rover_identity_allows_rework_on_changed_exchange_field() {
        let mut spec = load_spec("naqp");
        spec.dupe_dimension = Dimension::BandMode;
        spec.dupe_extra_rcvd_fields = vec!["loc".to_string()];

        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "K1ABC",
            ResolvedStation::new("W", Continent::NA, true, true),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_name".to_string(), Value::Text("CHRIS".to_string()));
        config.insert("my_loc".to_string(), Value::Text("MA".to_string()));
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        // First QSO: loc=NH, fresh station.
        let first = engine
            .apply_qso_with_mode(
                &resolver,
                &domains,
                Band::B20,
                Mode::CW,
                Callsign::new("K1ABC"),
                "MIKE NH",
            )
            .unwrap();
        assert!(!first.is_dupe);

        // Second QSO: same call, same band/mode, but different loc.
        // Mobile has moved — NOT a dupe under rover identity.
        let second = engine
            .apply_qso_with_mode(
                &resolver,
                &domains,
                Band::B20,
                Mode::CW,
                Callsign::new("K1ABC"),
                "MIKE ON",
            )
            .unwrap();
        assert!(
            !second.is_dupe,
            "different loc for same call should re-work under rover identity"
        );

        // Third QSO: repeating loc=NH on same band/mode → IS a dupe.
        let third = engine
            .apply_qso_with_mode(
                &resolver,
                &domains,
                Band::B20,
                Mode::CW,
                Callsign::new("K1ABC"),
                "MIKE NH",
            )
            .unwrap();
        assert!(
            third.is_dupe,
            "same call and same loc on same band/mode must still dupe"
        );

        // Total: 2 valid QSOs (NH, ON), 1 dupe. total_qsos increments only on
        // non-dupes, so the engine should report 2 QSOs.
        assert_eq!(engine.total_qsos(), 2);
    }

    /// Rover identity is scoped within the dupe dimension — NY on 20m and NY
    /// on 40m are still considered distinct, as are NY on 20m CW and 20m SSB.
    #[test]
    fn rover_identity_respects_dimension_scope() {
        let mut spec = load_spec("naqp");
        spec.dupe_dimension = Dimension::BandMode;
        spec.dupe_extra_rcvd_fields = vec!["loc".to_string()];

        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "K1ABC",
            ResolvedStation::new("W", Continent::NA, true, true),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_name".to_string(), Value::Text("CHRIS".to_string()));
        config.insert("my_loc".to_string(), Value::Text("MA".to_string()));
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        // Three QSOs across 2 bands × same loc.
        let a = engine
            .apply_qso_with_mode(
                &resolver,
                &domains,
                Band::B20,
                Mode::CW,
                Callsign::new("K1ABC"),
                "MIKE NH",
            )
            .unwrap();
        assert!(!a.is_dupe);

        let b = engine
            .apply_qso_with_mode(
                &resolver,
                &domains,
                Band::B40,
                Mode::CW,
                Callsign::new("K1ABC"),
                "MIKE NH",
            )
            .unwrap();
        assert!(!b.is_dupe, "different band scope → not a dupe");

        let c = engine
            .apply_qso_with_mode(
                &resolver,
                &domains,
                Band::B20,
                Mode::CW,
                Callsign::new("K1ABC"),
                "MIKE NH",
            )
            .unwrap();
        assert!(
            c.is_dupe,
            "same band/mode + same loc should dupe under rover identity"
        );
    }

    // ---------- Bonus rules (bonus_rules + DestCallIn) tests ----------

    /// When `bonus_rules` is empty, `claimed_score` is unchanged — this is
    /// the foundation of the additive-only guarantee.
    #[test]
    fn bonus_rules_empty_leaves_score_unchanged() {
        let spec = load_spec("cwt");
        assert!(spec.bonus_rules.is_empty());
        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "K1ABC",
            ResolvedStation::new("W", Continent::NA, true, true),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_name".to_string(), Value::Text("CHRIS".to_string()));
        config.insert("my_xchg".to_string(), Value::Text("MA".to_string()));
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        let _ = engine
            .apply_qso(
                &resolver,
                &domains,
                Band::B20,
                Callsign::new("K1ABC"),
                "MIKE MA",
            )
            .unwrap();
        assert_eq!(engine.bonus_total(), 0);
        // CWT is 1 pt × 1 global mult = 1. A stray bonus would bump this.
        assert_eq!(engine.claimed_score(), 1);
    }

    /// `BonusScope::Once` on a `DestCallIn` predicate fires exactly once
    /// — even if the operator works the bonus station on multiple bands.
    #[test]
    fn bonus_rule_once_with_dest_call_in_fires_one_time() {
        let mut spec = load_spec("cwt");
        spec.bonus_rules.push(BonusRule {
            id: "w1aw_bonus".to_string(),
            when: Some(Predicate::DestCallIn(DomainRef::List(vec![
                "W1AW".to_string(),
            ]))),
            amount: 100,
            scope: BonusScope::Once,
        });

        let mut resolver = InMemoryResolver::new();
        resolver.insert("W1AW", ResolvedStation::new("W", Continent::NA, true, true));
        resolver.insert(
            "K1ABC",
            ResolvedStation::new("W", Continent::NA, true, true),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_name".to_string(), Value::Text("CHRIS".to_string()));
        config.insert("my_xchg".to_string(), Value::Text("MA".to_string()));
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        // First W1AW QSO fires the bonus.
        let _ = engine
            .apply_qso(
                &resolver,
                &domains,
                Band::B20,
                Callsign::new("W1AW"),
                "MIKE CT",
            )
            .unwrap();
        assert_eq!(engine.bonus_total(), 100);

        // Second W1AW QSO on a different band — bonus must NOT fire again.
        let _ = engine
            .apply_qso(
                &resolver,
                &domains,
                Band::B40,
                Callsign::new("W1AW"),
                "MIKE CT",
            )
            .unwrap();
        assert_eq!(engine.bonus_total(), 100, "Once bonus must only fire once");

        // Unrelated QSO — no additional bonus.
        let _ = engine
            .apply_qso(
                &resolver,
                &domains,
                Band::B40,
                Callsign::new("K1ABC"),
                "MIKE MA",
            )
            .unwrap();
        assert_eq!(engine.bonus_total(), 100);
    }

    /// `BonusScope::PerQso` on a `DestCallIn` predicate fires every time the
    /// bonus station is worked (once per band-mode dupe-gate).
    #[test]
    fn bonus_rule_per_qso_with_dest_call_in_fires_each_time() {
        let mut spec = load_spec("cwt");
        spec.bonus_rules.push(BonusRule {
            id: "club_bonus".to_string(),
            when: Some(Predicate::DestCallIn(DomainRef::List(vec![
                "VA3CCO".to_string(),
                "VE3CCO".to_string(),
            ]))),
            amount: 10,
            scope: BonusScope::PerQso,
        });

        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "VA3CCO",
            ResolvedStation::new("VE", Continent::NA, true, true),
        );
        resolver.insert(
            "VE3CCO",
            ResolvedStation::new("VE", Continent::NA, true, true),
        );
        resolver.insert(
            "K1ABC",
            ResolvedStation::new("W", Continent::NA, true, true),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_name".to_string(), Value::Text("CHRIS".to_string()));
        config.insert("my_xchg".to_string(), Value::Text("MA".to_string()));
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        // Work VA3CCO on 20m and 40m, VE3CCO on 20m, and K1ABC (no bonus).
        let _ = engine
            .apply_qso(&resolver, &domains, Band::B20, Callsign::new("VA3CCO"), "JEAN ON")
            .unwrap();
        let _ = engine
            .apply_qso(&resolver, &domains, Band::B40, Callsign::new("VA3CCO"), "JEAN ON")
            .unwrap();
        let _ = engine
            .apply_qso(&resolver, &domains, Band::B20, Callsign::new("VE3CCO"), "PIERRE ON")
            .unwrap();
        let _ = engine
            .apply_qso(&resolver, &domains, Band::B40, Callsign::new("K1ABC"), "MIKE MA")
            .unwrap();

        // 3 bonus-eligible QSOs × 10 = 30.
        assert_eq!(engine.bonus_total(), 30);
    }

    /// `BonusScope::PerBandMode` fires at most once per `(band, mode)` per
    /// rule id — so working the same bonus station on the same band/mode
    /// doesn't double-count (it's a dupe anyway) but working it on a new
    /// band or mode does.
    #[test]
    fn bonus_rule_per_band_mode_fires_once_per_band_mode() {
        let mut spec = load_spec("cwt");
        spec.bonus_rules.push(BonusRule {
            id: "hq_per_bm".to_string(),
            when: Some(Predicate::DestCallIn(DomainRef::List(vec![
                "K4TCG".to_string(),
            ]))),
            amount: 100,
            scope: BonusScope::PerBandMode,
        });

        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "K4TCG",
            ResolvedStation::new("W", Continent::NA, true, true),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_name".to_string(), Value::Text("CHRIS".to_string()));
        config.insert("my_xchg".to_string(), Value::Text("MA".to_string()));
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        let _ = engine
            .apply_qso(&resolver, &domains, Band::B20, Callsign::new("K4TCG"), "BOB TN")
            .unwrap();
        assert_eq!(engine.bonus_total(), 100);

        // Same band, same mode, same call → dupe; bonus unchanged.
        let _ = engine
            .apply_qso(&resolver, &domains, Band::B20, Callsign::new("K4TCG"), "BOB TN")
            .unwrap();
        assert_eq!(engine.bonus_total(), 100);

        // Different band → new bonus firing.
        let _ = engine
            .apply_qso(&resolver, &domains, Band::B40, Callsign::new("K4TCG"), "BOB TN")
            .unwrap();
        assert_eq!(engine.bonus_total(), 200);

        // Different band again → third firing.
        let _ = engine
            .apply_qso(&resolver, &domains, Band::B80, Callsign::new("K4TCG"), "BOB TN")
            .unwrap();
        assert_eq!(engine.bonus_total(), 300);
    }

    // ---------- Score multiplier (power class) tests ----------

    /// When `score_multiplier` is `None`, `claimed_score` is unchanged —
    /// the additive-only guarantee for the Phase 3 feature.
    #[test]
    fn score_multiplier_none_leaves_score_unchanged() {
        let spec = load_spec("cwt");
        assert!(spec.score_multiplier.is_none());
        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "K1ABC",
            ResolvedStation::new("W", Continent::NA, true, true),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_name".to_string(), Value::Text("CHRIS".to_string()));
        config.insert("my_xchg".to_string(), Value::Text("MA".to_string()));
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        let _ = engine
            .apply_qso(
                &resolver,
                &domains,
                Band::B20,
                Callsign::new("K1ABC"),
                "MIKE MA",
            )
            .unwrap();
        assert_eq!(engine.claimed_score(), 1);
    }

    /// QRP ×5, Low ×2, High ×1 — canonical power multiplier matrix used by
    /// NMQP and NEQSOP.
    #[test]
    fn score_multiplier_scales_by_power_class_lookup() {
        let mut spec = load_spec("cwt");
        let mut mapping = BTreeMap::new();
        mapping.insert("QRP".to_string(), 5);
        mapping.insert("LOW".to_string(), 2);
        mapping.insert("HIGH".to_string(), 1);
        spec.score_multiplier = Some(ScoreMultiplier {
            from_config: "my_power_class".to_string(),
            mapping,
            default: 1,
        });

        fn score_with_power(spec: &ContestSpec, power: &str) -> i64 {
            let mut resolver = InMemoryResolver::new();
            resolver.insert(
                "K1ABC",
                ResolvedStation::new("W", Continent::NA, true, true),
            );
            let domains = super::tests::base_domains();
            let source = ResolvedStation::new("W", Continent::NA, true, true);
            let mut config = HashMap::new();
            config.insert("my_name".to_string(), Value::Text("CHRIS".to_string()));
            config.insert("my_xchg".to_string(), Value::Text("MA".to_string()));
            config.insert(
                "my_power_class".to_string(),
                Value::Text(power.to_string()),
            );
            let mut engine = SpecEngine::new(spec.clone(), source, config).unwrap();
            let _ = engine
                .apply_qso(
                    &resolver,
                    &domains,
                    Band::B20,
                    Callsign::new("K1ABC"),
                    "MIKE MA",
                )
                .unwrap();
            engine.claimed_score()
        }

        // CWT base: 1 pt × 1 mult = 1.
        assert_eq!(score_with_power(&spec, "QRP"), 5);
        assert_eq!(score_with_power(&spec, "LOW"), 2);
        assert_eq!(score_with_power(&spec, "HIGH"), 1);
    }

    /// Missing or unknown config value falls through to `default`.
    #[test]
    fn score_multiplier_missing_config_uses_default() {
        let mut spec = load_spec("cwt");
        let mut mapping = BTreeMap::new();
        mapping.insert("QRP".to_string(), 5);
        spec.score_multiplier = Some(ScoreMultiplier {
            from_config: "my_power_class".to_string(),
            mapping,
            default: 3,
        });

        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "K1ABC",
            ResolvedStation::new("W", Continent::NA, true, true),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_name".to_string(), Value::Text("CHRIS".to_string()));
        config.insert("my_xchg".to_string(), Value::Text("MA".to_string()));
        // my_power_class NOT set in config → default applies.
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        let _ = engine
            .apply_qso(
                &resolver,
                &domains,
                Band::B20,
                Callsign::new("K1ABC"),
                "MIKE MA",
            )
            .unwrap();
        assert_eq!(engine.claimed_score(), 3);
    }

    /// Score multiplier applies to the base `(points × mults)`, but bonuses
    /// are added *after* the multiplication. Delaware QSO Party uses this
    /// exact composition: `(points × mults × power) + submission_bonus`.
    #[test]
    fn score_multiplier_composes_with_bonus_additively() {
        let mut spec = load_spec("cwt");
        let mut mapping = BTreeMap::new();
        mapping.insert("QRP".to_string(), 5);
        spec.score_multiplier = Some(ScoreMultiplier {
            from_config: "my_power_class".to_string(),
            mapping,
            default: 1,
        });
        spec.bonus_rules.push(BonusRule {
            id: "flat_submission".to_string(),
            when: None,
            amount: 50,
            scope: BonusScope::Once,
        });

        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "K1ABC",
            ResolvedStation::new("W", Continent::NA, true, true),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_name".to_string(), Value::Text("CHRIS".to_string()));
        config.insert("my_xchg".to_string(), Value::Text("MA".to_string()));
        config.insert(
            "my_power_class".to_string(),
            Value::Text("QRP".to_string()),
        );
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        let _ = engine
            .apply_qso(
                &resolver,
                &domains,
                Band::B20,
                Callsign::new("K1ABC"),
                "MIKE MA",
            )
            .unwrap();
        // Base = 1 × 1 = 1. Scaled by x5 = 5. Plus +50 bonus = 55.
        // (Not 1 × 1 × (5+50) = 55. The bonus is additive, not multiplied.)
        assert_eq!(engine.claimed_score(), 55);
    }

    // ---------- Multi-mult per QSO (multi_value_sep) tests ----------

    /// When `multi_value_sep` is `None` (the default), the multiplier loop
    /// behaves exactly as before — one QSO yields at most one mult value.
    #[test]
    fn multi_value_sep_none_preserves_single_value_behavior() {
        let spec = load_spec("naqp");
        for variant in &spec.exchange.received_variants {
            for f in &variant.fields {
                assert!(
                    f.multi_value_sep.is_none(),
                    "baseline spec {} field {} unexpectedly has multi_value_sep",
                    spec.id,
                    f.id
                );
            }
        }

        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "K1ABC",
            ResolvedStation::new("W", Continent::NA, true, true),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_name".to_string(), Value::Text("CHRIS".to_string()));
        config.insert("my_loc".to_string(), Value::Text("MA".to_string()));
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        let result = engine
            .apply_qso(
                &resolver,
                &domains,
                Band::B20,
                Callsign::new("K1ABC"),
                "MIKE NH",
            )
            .unwrap();
        assert_eq!(result.new_mults, vec!["na_mult:NH"]);
        assert_eq!(engine.total_mults(), 1);
    }

    /// With `multi_value_sep: "/"`, a received value like `NH/ON` splits
    /// into two distinct multiplier rows from a single QSO.
    #[test]
    fn multi_value_sep_slash_splits_one_qso_into_two_mults() {
        let mut spec = load_spec("naqp");
        for variant in &mut spec.exchange.received_variants {
            for f in &mut variant.fields {
                if f.id == "loc" {
                    f.multi_value_sep = Some("/".to_string());
                }
            }
        }

        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "K1CL",
            ResolvedStation::new("W", Continent::NA, true, true),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_name".to_string(), Value::Text("CHRIS".to_string()));
        config.insert("my_loc".to_string(), Value::Text("MA".to_string()));
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        let result = engine
            .apply_qso(
                &resolver,
                &domains,
                Band::B20,
                Callsign::new("K1CL"),
                "MIKE NH/ON",
            )
            .unwrap();
        // Both NH and ON are in base_domains naqp_multipliers. The single
        // QSO produced both mult rows.
        let mut sorted_mults = result.new_mults.clone();
        sorted_mults.sort();
        assert_eq!(
            sorted_mults,
            vec!["na_mult:NH".to_string(), "na_mult:ON".to_string()]
        );
        assert_eq!(engine.total_mults(), 2);
        // Still a single QSO for dupe/count purposes.
        assert_eq!(engine.total_qsos(), 1);
    }

    /// An unknown split value is rejected by domain validation; valid
    /// split values still count. The overall QSO still counts once.
    #[test]
    fn multi_value_sep_skips_unknown_values_and_keeps_valid() {
        let mut spec = load_spec("naqp");
        for variant in &mut spec.exchange.received_variants {
            for f in &mut variant.fields {
                if f.id == "loc" {
                    f.multi_value_sep = Some("/".to_string());
                }
            }
        }

        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "K1CL",
            ResolvedStation::new("W", Continent::NA, true, true),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_name".to_string(), Value::Text("CHRIS".to_string()));
        config.insert("my_loc".to_string(), Value::Text("MA".to_string()));
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        // NH is in base_domains naqp_multipliers, ZZ is not.
        let result = engine
            .apply_qso(
                &resolver,
                &domains,
                Band::B20,
                Callsign::new("K1CL"),
                "MIKE NH/ZZ",
            )
            .unwrap();
        assert_eq!(result.new_mults, vec!["na_mult:NH"]);
        assert_eq!(engine.total_mults(), 1);
        assert_eq!(engine.total_qsos(), 1);
    }

    /// `claimed_score` is `(points × mults) + bonus_total`. Demonstrates the
    /// additive composition end-to-end on a spec with both multipliers and
    /// a Once bonus.
    #[test]
    fn claimed_score_adds_bonus_total_to_base_formula() {
        let mut spec = load_spec("cwt");
        spec.bonus_rules.push(BonusRule {
            id: "flat".to_string(),
            when: None,
            amount: 50,
            scope: BonusScope::Once,
        });

        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "K1ABC",
            ResolvedStation::new("W", Continent::NA, true, true),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_name".to_string(), Value::Text("CHRIS".to_string()));
        config.insert("my_xchg".to_string(), Value::Text("MA".to_string()));
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        let _ = engine
            .apply_qso(
                &resolver,
                &domains,
                Band::B20,
                Callsign::new("K1ABC"),
                "MIKE MA",
            )
            .unwrap();

        // Base: 1 pt × 1 mult = 1. Plus one-time +50 bonus = 51.
        assert_eq!(engine.total_points(), 1);
        assert_eq!(engine.total_mults(), 1);
        assert_eq!(engine.bonus_total(), 50);
        assert_eq!(engine.claimed_score(), 51);
    }

    #[test]
    fn global_multiplier_counts_once_across_bands() {
        let spec = load_spec("cwt");
        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "K1ABC",
            ResolvedStation::new("W", Continent::NA, true, true),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_name".to_string(), Value::Text("CHRIS".to_string()));
        config.insert("my_xchg".to_string(), Value::Text("MA".to_string()));
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        let first = engine
            .apply_qso(
                &resolver,
                &domains,
                Band::B20,
                Callsign::new("K1ABC"),
                "AL MA",
            )
            .unwrap();
        assert_eq!(first.new_mults, vec!["call:K1ABC"]);

        let second = engine
            .apply_qso(
                &resolver,
                &domains,
                Band::B40,
                Callsign::new("K1ABC"),
                "AL MA",
            )
            .unwrap();
        assert!(second.new_mults.is_empty());
    }

    #[test]
    fn bandmode_dupes_and_multipliers_scope_per_mode() {
        let mut spec = load_spec("naqp");
        spec.dupe_dimension = Dimension::BandMode;
        spec.multipliers[0].dimension = Dimension::BandMode;

        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "K1ABC",
            ResolvedStation::new("W", Continent::NA, true, true),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_name".to_string(), Value::Text("CHRIS".to_string()));
        config.insert("my_loc".to_string(), Value::Text("MA".to_string()));
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        let first = engine
            .apply_qso_with_mode(
                &resolver,
                &domains,
                Band::B20,
                Mode::CW,
                Callsign::new("K1ABC"),
                "AL MA",
            )
            .unwrap();
        assert!(!first.is_dupe);
        assert_eq!(first.new_mults, vec!["na_mult:MA"]);

        let dupe = engine
            .apply_qso_with_mode(
                &resolver,
                &domains,
                Band::B20,
                Mode::CW,
                Callsign::new("K1ABC"),
                "AL MA",
            )
            .unwrap();
        assert!(dupe.is_dupe);

        let second_mode = engine
            .apply_qso_with_mode(
                &resolver,
                &domains,
                Band::B20,
                Mode::SSB,
                Callsign::new("K1ABC"),
                "AL MA",
            )
            .unwrap();
        assert!(!second_mode.is_dupe);
        assert_eq!(second_mode.new_mults, vec!["na_mult:MA"]);
        assert_eq!(
            engine.worked_mults_scoped("na_mult", Some(Band::B20), Some(Mode::CW)),
            vec!["MA"]
        );
        assert_eq!(
            engine.worked_mults_scoped("na_mult", Some(Band::B20), Some(Mode::SSB)),
            vec!["MA"]
        );
    }

    #[test]
    fn config_validation_enforces_required_and_conditional_fields() {
        let spec = load_spec("arrl_dx");
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_is_wve".to_string(), Value::Bool(true));
        let err = SpecEngine::new(spec.clone(), source.clone(), config).unwrap_err();
        assert!(matches!(err, EngineError::InvalidConfig(_)));

        let mut good = HashMap::new();
        good.insert("my_is_wve".to_string(), Value::Bool(true));
        good.insert(
            "my_state_province".to_string(),
            Value::Text("MA".to_string()),
        );
        let ok = SpecEngine::new(spec, source, good);
        assert!(ok.is_ok());
    }

    #[test]
    fn session_config_validation_rejects_invalid_domain_values() {
        let spec = load_spec("naqp");
        let resolver = InMemoryResolver::new();
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_name".to_string(), Value::Text("CHRIS".to_string()));
        config.insert("my_loc".to_string(), Value::Text("ZZ".to_string()));

        let err = SpecSession::new(spec, source, config, resolver, domains)
            .err()
            .expect("expected invalid config");
        assert!(matches!(err, EngineError::InvalidConfig(_)));
    }

    #[test]
    fn contestspec_serializes_and_loads_yaml_and_json() {
        let yaml = r#"
id: demo_yaml
name: Demo YAML
cabrillo_contest: DEMO
bands: [B20]
modes: [CW]
dupe_dimension: Band
valid_qso: []
exchange:
  received_variants:
    - when: null
      fields:
        - id: rst
          field_type: Rst
          required: true
          domain: null
          accept: []
          normalize_upper_trim: false
multipliers: []
points:
  - when: null
    value: 1
config_fields: []
"#;
        let loaded_yaml = ContestSpec::from_yaml_str(yaml).unwrap();
        assert_eq!(loaded_yaml.id, "demo_yaml");

        let json = r#"{
  "id":"demo_json",
  "name":"Demo JSON",
  "cabrillo_contest":"DEMO",
  "bands":["B20"],
  "modes":["CW"],
  "dupe_dimension":"Band",
  "valid_qso":[],
  "exchange":{"received_variants":[{"when":null,"fields":[{"id":"rst","field_type":"Rst","required":true,"domain":null,"accept":[],"normalize_upper_trim":false}]}]},
  "multipliers":[],
  "points":[{"when":null,"value":1}],
  "config_fields":[]
}"#;
        let loaded_json = ContestSpec::from_json_str(json).unwrap();
        assert_eq!(loaded_json.id, "demo_json");

        let mut path = std::env::temp_dir();
        path.push("contestspec_test.yaml");
        std::fs::write(&path, yaml).unwrap();
        let loaded_path = ContestSpec::from_path(&path).unwrap();
        assert_eq!(loaded_path.cabrillo_contest, "DEMO");
        let _ = std::fs::remove_file(path);
    }

    #[test]
    fn multiplier_key_concat_expression_evaluates() {
        let spec_json = r#"{
  "id":"concat_demo",
  "name":"Concat Demo",
  "cabrillo_contest":"DEMO",
  "bands":["B20"],
  "modes":["CW"],
  "dupe_dimension":"Band",
  "valid_qso":[],
  "exchange":{
    "received_variants":[{"when":null,"fields":[
      {"id":"rst","field_type":"Rst","required":true,"domain":null,"accept":[],"normalize_upper_trim":false},
      {"id":"zone","field_type":"Int","required":true,"domain":{"Range":{"min":1,"max":40}},"accept":[],"normalize_upper_trim":false}
    ]}]
  },
  "multipliers":[
    {"id":"combo","dimension":"Band","variants":[
      {"when":null,"key":{"Concat":[{"scope":"DEST","key":"dxcc"},"-",{"scope":"RCVD","key":"zone"}]},"domain":{"List":["DL-14"]}}
    ]}
  ],
  "points":[{"when":null,"value":1}],
  "config_fields":[]
}"#;
        let spec = ContestSpec::from_json_str(spec_json).unwrap();
        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "DL1ABC",
            ResolvedStation::new("DL", Continent::EU, false, false),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let config = HashMap::new();
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        let result = engine
            .apply_qso(
                &resolver,
                &domains,
                Band::B20,
                Callsign::new("DL1ABC"),
                "599 14",
            )
            .unwrap();
        assert_eq!(result.new_mults, vec!["combo:DL-14"]);
    }

    #[test]
    fn loads_repo_contestspec_files() {
        let cqww = ContestSpec::from_path("specs/cqww_cw.json").unwrap();
        let cqww_family = ContestSpec::from_path("specs/cqww.json").unwrap();
        let arrl = ContestSpec::from_path("specs/arrl_dx.json").unwrap();
        let naqp = ContestSpec::from_path("specs/naqp.json").unwrap();
        assert_eq!(cqww.id, "cqww_cw");
        assert_eq!(cqww_family.id, "cqww");
        assert_eq!(arrl.id, "arrl_dx");
        assert_eq!(naqp.id, "naqp");
        assert!(cqww.deprecated.is_some());
    }

    #[test]
    fn serial_allocator_generates_deterministic_sent_values() {
        let spec_json = r#"{
  "id":"serial_demo",
  "name":"Serial Demo",
  "cabrillo_contest":"DEMO",
  "bands":["B20","B40"],
  "modes":["CW"],
  "dupe_dimension":"Band",
  "valid_qso":[],
  "exchange":{
    "received_variants":[{"when":null,"fields":[
      {"id":"rst","field_type":"Rst","required":true,"domain":null,"accept":[],"normalize_upper_trim":false}
    ]}],
    "sent_variants":[{"when":null,"fields":[
      {"id":"rst","value":{"Const":"599"},"normalize_upper_trim":false}
    ]}],
    "sent_serial":{"field_id":"serial","start":1,"width":3,"scope":"global"}
  },
  "multipliers":[],
  "points":[{"when":null,"value":1}],
  "score":{"formula":"points_times_mults"},
  "config_fields":[]
}"#;
        let spec = ContestSpec::from_json_str(spec_json).unwrap();
        let mut resolver = InMemoryResolver::new();
        resolver.insert("K1AAA", ResolvedStation::new("W", Continent::NA, true, true));
        resolver.insert("K1BBB", ResolvedStation::new("W", Continent::NA, true, true));
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);

        let mut a = SpecEngine::new(spec.clone(), source.clone(), HashMap::new()).unwrap();
        let mut b = SpecEngine::new(spec, source, HashMap::new()).unwrap();

        let mut seq_a = Vec::new();
        for input in [
            (Band::B20, "K1AAA"),
            (Band::B20, "K1AAA"),
            (Band::B40, "K1AAA"),
            (Band::B20, "K1BBB"),
        ] {
            let r = a
                .apply_qso(&resolver, &domains, input.0, Callsign::new(input.1), "599")
                .unwrap();
            if !r.is_dupe {
                seq_a.push(r.sent_exchange.get("serial").cloned().unwrap_or_default());
            }
        }

        let mut seq_b = Vec::new();
        for input in [
            (Band::B20, "K1AAA"),
            (Band::B20, "K1AAA"),
            (Band::B40, "K1AAA"),
            (Band::B20, "K1BBB"),
        ] {
            let r = b
                .apply_qso(&resolver, &domains, input.0, Callsign::new(input.1), "599")
                .unwrap();
            if !r.is_dupe {
                seq_b.push(r.sent_exchange.get("serial").cloned().unwrap_or_default());
            }
        }

        assert_eq!(seq_a, vec!["001", "002", "003"]);
        assert_eq!(seq_a, seq_b);
    }

    #[test]
    fn variant_selection_and_mode_enforcement_work() {
        let spec = ContestSpec::from_path("specs/cqww.json").unwrap();
        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "DL1ABC",
            ResolvedStation::new("DL", Continent::EU, false, false),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_cq_zone".to_string(), Value::Int(5));
        let mut session = SpecSession::new_with_meta(
            spec,
            source,
            config,
            resolver,
            domains,
            SessionMeta {
                variant: Some("cw".to_string()),
                category_mode: Some("CW".to_string()),
            },
        )
        .unwrap();

        assert_eq!(session.engine().selected_variant(), Some("cw"));
        assert_eq!(session.engine().effective_cabrillo_contest(), "CQ-WW-CW");
        assert_eq!(session.engine().allowed_modes(), &[Mode::CW]);

        let wrong_mode = session
            .apply_qso_with_mode(Band::B20, Mode::SSB, Callsign::new("DL1ABC"), "599 14")
            .unwrap_err();
        assert!(matches!(
            wrong_mode,
            EngineError::InvalidMode {
                mode: Mode::SSB,
                ..
            }
        ));

        let ok = session
            .apply_qso_with_mode(Band::B20, Mode::CW, Callsign::new("DL1ABC"), "599 14")
            .unwrap();
        assert!(!ok.is_dupe);
    }

    #[test]
    fn default_variant_is_selected_when_not_provided() {
        let spec_json = r#"{
  "id":"demo_variant_default",
  "name":"Demo Variant Default",
  "cabrillo_contest":"DEMO",
  "bands":["B20"],
  "modes":["CW","SSB"],
  "dupe_dimension":"Band",
  "valid_qso":[],
  "exchange":{"received_variants":[{"when":null,"fields":[{"id":"rst","field_type":"Rst","required":true,"domain":null,"accept":[],"normalize_upper_trim":false}]}]},
  "multipliers":[],
  "points":[{"when":null,"value":1}],
  "score":{"formula":"points_times_mults"},
  "config_fields":[],
  "default_variant":"cw",
  "variants":{
    "cw":{"cabrillo_contest":"DEMO-CW","allowed_modes":["CW"]},
    "ssb":{"cabrillo_contest":"DEMO-SSB","allowed_modes":["SSB"]}
  }
}"#;
        let spec = ContestSpec::from_json_str(spec_json).unwrap();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let config = HashMap::new();
        let engine = SpecEngine::new_with_meta(spec, source, config, SessionMeta::default()).unwrap();
        assert_eq!(engine.selected_variant(), Some("cw"));
        assert_eq!(engine.effective_cabrillo_contest(), "DEMO-CW");
        assert_eq!(engine.allowed_modes(), &[Mode::CW]);
    }

    #[test]
    fn unknown_variant_is_rejected() {
        let spec = ContestSpec::from_path("specs/cqww.json").unwrap();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_cq_zone".to_string(), Value::Int(5));
        let err = SpecEngine::new_with_meta(
            spec,
            source,
            config,
            SessionMeta {
                variant: Some("nope".to_string()),
                category_mode: None,
            },
        )
        .unwrap_err();
        assert!(matches!(err, EngineError::InvalidVariant(_)));
    }

    #[test]
    fn variant_cabrillo_header_uses_effective_contest() {
        let spec = ContestSpec::from_path("specs/arrl_dx.json").unwrap();
        let mut resolver = InMemoryResolver::new();
        resolver.insert("K1AR", ResolvedStation::new("W", Continent::NA, true, true));
        let domains = base_domains();
        let source = ResolvedStation::new("DL", Continent::EU, false, false);
        let mut config = HashMap::new();
        config.insert("my_is_wve".to_string(), Value::Bool(false));
        config.insert("my_power_sent".to_string(), Value::Text("100".to_string()));
        let mut session = SpecSession::new_with_meta(
            spec,
            source,
            config,
            resolver,
            domains,
            SessionMeta {
                variant: Some("ssb".to_string()),
                category_mode: Some("SSB".to_string()),
            },
        )
        .unwrap();
        let _ = session
            .apply_qso_with_mode(Band::B20, Mode::SSB, Callsign::new("K1AR"), "59 MA")
            .unwrap();
        let cab = session.export_cabrillo(
            "DL1AAA",
            "SINGLE-OP",
            &[CabrilloQso {
                freq_khz: 14250,
                mode: Mode::SSB,
                date_utc: "2026-03-08".to_string(),
                time_utc: "0001".to_string(),
                my_call: "DL1AAA".to_string(),
                their_call: "K1AR".to_string(),
                sent: vec!["59".to_string(), "100".to_string()],
                received: vec!["59".to_string(), "MA".to_string()],
            }],
        );
        assert!(cab.contains("CONTEST: ARRL-DX-SSB"));
        assert!(cab.contains("CATEGORY-MODE: SSB"));
    }

    #[test]
    fn sent_exchange_generation_works_for_arrl_variants() {
        let spec = load_spec("arrl_dx");
        let mut resolver = InMemoryResolver::new();
        resolver.insert("K1AR", ResolvedStation::new("W", Continent::NA, true, true));
        let domains = base_domains();
        let source = ResolvedStation::new("DL", Continent::EU, false, false);
        let mut config = HashMap::new();
        config.insert("my_is_wve".to_string(), Value::Bool(false));
        config.insert("my_power_sent".to_string(), Value::Text("100".to_string()));
        let session = SpecSession::new(spec, source, config, resolver, domains).unwrap();

        let sent = session
            .sent_exchange_for_call(Callsign::new("K1AR"))
            .unwrap();
        assert_eq!(sent.get("rst"), Some(&"599".to_string()));
        assert_eq!(sent.get("power"), Some(&"100".to_string()));
    }

    #[test]
    fn cabrillo_export_includes_qso_lines() {
        let spec = load_spec("cqww_cw");
        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "DL1ABC",
            ResolvedStation::new("DL", Continent::EU, false, false),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_cq_zone".to_string(), Value::Int(5));
        let mut session = SpecSession::new(spec, source, config, resolver, domains).unwrap();
        let _ = session
            .apply_qso(Band::B20, Callsign::new("DL1ABC"), "599 14")
            .unwrap();
        let cab = session.export_cabrillo(
            "W1AW",
            "SINGLE-OP",
            &[CabrilloQso {
                freq_khz: 14025,
                mode: Mode::CW,
                date_utc: "2026-11-28".to_string(),
                time_utc: "0001".to_string(),
                my_call: "W1AW".to_string(),
                their_call: "DL1ABC".to_string(),
                sent: vec!["599".to_string(), "05".to_string()],
                received: vec!["599".to_string(), "14".to_string()],
            }],
        );
        assert!(cab.contains("START-OF-LOG: 3.0"));
        assert!(cab.contains("CONTEST: CQ-WW-CW"));
        assert!(cab.contains("QSO:"));
        assert!(cab.contains("CLAIMED-SCORE: 6"));
    }

    #[test]
    fn power_parser_accepts_common_formats() {
        let spec = load_spec("arrl_dx");
        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "DL1ABC",
            ResolvedStation::new("DL", Continent::EU, false, false),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_is_wve".to_string(), Value::Bool(true));
        config.insert(
            "my_state_province".to_string(),
            Value::Text("MA".to_string()),
        );
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        let _ = engine
            .apply_qso(
                &resolver,
                &domains,
                Band::B20,
                Callsign::new("DL1ABC"),
                "599 1.5KW",
            )
            .unwrap();
        assert_eq!(engine.total_qsos(), 1);
    }

    #[test]
    fn exchange_errors_are_structured_with_field_and_kind() {
        let spec = load_spec("cqww_cw");
        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "DL1ABC",
            ResolvedStation::new("DL", Continent::EU, false, false),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_cq_zone".to_string(), Value::Int(5));
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        let err = engine
            .apply_qso(
                &resolver,
                &domains,
                Band::B20,
                Callsign::new("DL1ABC"),
                "599 99",
            )
            .unwrap_err();
        match err {
            EngineError::Exchange(items) => {
                assert!(!items.is_empty());
                assert_eq!(items[0].field_id.as_deref(), Some("zone"));
                assert_eq!(items[0].kind, ExchangeErrorKind::DomainMismatch);
            }
            other => panic!("expected exchange error, got {:?}", other),
        }
    }

    #[test]
    fn dimension_mode_and_period_affect_dupes() {
        let mut spec = load_spec("naqp");
        let mut resolver = InMemoryResolver::new();
        resolver.insert("K1AAA", ResolvedStation::new("W", Continent::NA, true, true));
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_name".to_string(), Value::Text("CHRIS".to_string()));
        config.insert("my_loc".to_string(), Value::Text("MA".to_string()));

        spec.dupe_dimension = Dimension::Mode;
        let mut mode_engine = SpecEngine::new(spec.clone(), source.clone(), config.clone()).unwrap();
        let first = mode_engine
            .apply_qso_at_with_mode(
                &resolver,
                &domains,
                Band::B20,
                Mode::CW,
                0,
                Callsign::new("K1AAA"),
                "AL MA",
            )
            .unwrap();
        assert!(!first.is_dupe);
        let second = mode_engine
            .apply_qso_at_with_mode(
                &resolver,
                &domains,
                Band::B20,
                Mode::SSB,
                0,
                Callsign::new("K1AAA"),
                "AL MA",
            )
            .unwrap();
        assert!(!second.is_dupe);

        spec.dupe_dimension = Dimension::Period { minutes: 60 };
        let mut period_engine = SpecEngine::new(spec, source, config).unwrap();
        let p1 = period_engine
            .apply_qso_at_with_mode(
                &resolver,
                &domains,
                Band::B20,
                Mode::CW,
                0,
                Callsign::new("K1AAA"),
                "AL MA",
            )
            .unwrap();
        assert!(!p1.is_dupe);
        let p2 = period_engine
            .apply_qso_at_with_mode(
                &resolver,
                &domains,
                Band::B40,
                Mode::CW,
                1800,
                Callsign::new("K1AAA"),
                "AL MA",
            )
            .unwrap();
        assert!(p2.is_dupe);
        let p3 = period_engine
            .apply_qso_at_with_mode(
                &resolver,
                &domains,
                Band::B40,
                Mode::CW,
                3700,
                Callsign::new("K1AAA"),
                "AL MA",
            )
            .unwrap();
        assert!(!p3.is_dupe);
    }

    #[test]
    fn score_formulas_compute_different_results() {
        let mut resolver = InMemoryResolver::new();
        resolver.insert("K1AAA", ResolvedStation::new("W", Continent::NA, true, true));
        resolver.insert("K1BBB", ResolvedStation::new("W", Continent::NA, true, true));
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);

        let mut a = load_spec("naqp");
        a.score.formula = ScoreFormula::PointsTimesTotalMults;
        let mut b = a.clone();
        b.score.formula = ScoreFormula::SumBandPointsTimesBandMults;

        let mut cfg = HashMap::new();
        cfg.insert("my_name".to_string(), Value::Text("CHRIS".to_string()));
        cfg.insert("my_loc".to_string(), Value::Text("MA".to_string()));

        let mut ea = SpecEngine::new(a, source.clone(), cfg.clone()).unwrap();
        let mut eb = SpecEngine::new(b, source, cfg).unwrap();

        for engine in [&mut ea, &mut eb] {
            let _ = engine
                .apply_qso(
                    &resolver,
                    &domains,
                    Band::B20,
                    Callsign::new("K1AAA"),
                    "AL MA",
                )
                .unwrap();
            let _ = engine
                .apply_qso(
                    &resolver,
                    &domains,
                    Band::B40,
                    Callsign::new("K1BBB"),
                    "BOB NH",
                )
                .unwrap();
        }

        assert_ne!(ea.claimed_score(), eb.claimed_score());
    }

    #[test]
    fn points_times_sum_band_mults_excludes_global_mults() {
        let mut resolver = InMemoryResolver::new();
        resolver.insert("K1AAA", ResolvedStation::new("W", Continent::NA, true, true));
        resolver.insert("K1BBB", ResolvedStation::new("W", Continent::NA, true, true));
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);

        let mut base = load_spec("naqp");
        base.multipliers.push(MultiplierSpec {
            id: "call".to_string(),
            dimension: Dimension::Global,
            variants: vec![MultiplierVariant {
                when: None,
                key: KeyExpr::Field(FieldRef::dest("call")),
                domain: DomainRef::Any,
            }],
        });

        let mut a = base.clone();
        a.score.formula = ScoreFormula::PointsTimesTotalMults;
        let mut b = base;
        b.score.formula = ScoreFormula::PointsTimesSumBandMults;

        let mut cfg = HashMap::new();
        cfg.insert("my_name".to_string(), Value::Text("CHRIS".to_string()));
        cfg.insert("my_loc".to_string(), Value::Text("MA".to_string()));

        let mut ea = SpecEngine::new(a, source.clone(), cfg.clone()).unwrap();
        let mut eb = SpecEngine::new(b, source, cfg).unwrap();

        for engine in [&mut ea, &mut eb] {
            let _ = engine
                .apply_qso(
                    &resolver,
                    &domains,
                    Band::B20,
                    Callsign::new("K1AAA"),
                    "AL MA",
                )
                .unwrap();
            let _ = engine
                .apply_qso(
                    &resolver,
                    &domains,
                    Band::B40,
                    Callsign::new("K1BBB"),
                    "BOB NH",
                )
                .unwrap();
        }

        assert!(ea.total_mults() > eb.sum_band_mults());
        assert_ne!(ea.claimed_score(), eb.claimed_score());
    }

    #[test]
    fn points_summary_distinguishes_explicit_zero_from_fallback_zero() {
        let spec_json = r#"{
  "id":"points_reason_demo",
  "name":"Points Reason Demo",
  "cabrillo_contest":"DEMO",
  "bands":["B20"],
  "modes":["CW"],
  "dupe_dimension":"Band",
  "valid_qso":[],
  "exchange":{"received_variants":[{"when":null,"fields":[
    {"id":"rst","field_type":"Rst","required":true,"domain":null,"accept":[],"normalize_upper_trim":false}
  ]}]},
  "multipliers":[],
  "points":[
    {"when":{"Eq":[{"Field":{"scope":"SOURCE","key":"dxcc"}},{"Field":{"scope":"DEST","key":"dxcc"}}]},"value":0},
    {"when":null,"value":0}
  ],
  "score":{"formula":"points_times_mults"},
  "config_fields":[]
}"#;
        let spec = ContestSpec::from_json_str(spec_json).unwrap();

        let mut resolver = InMemoryResolver::new();
        resolver.insert("K1AAA", ResolvedStation::new("W", Continent::NA, true, true));
        resolver.insert("DL1ABC", ResolvedStation::new("DL", Continent::EU, false, false));
        let domains = base_domains();

        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let config = HashMap::new();
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        let explicit_zero = engine
            .apply_qso(
                &resolver,
                &domains,
                Band::B20,
                Callsign::new("K1AAA"),
                "599",
            )
            .unwrap();
        assert_eq!(explicit_zero.qso_points, 0);
        assert_eq!(explicit_zero.qso_points_rule_index, Some(0));
        assert!(!explicit_zero.qso_points_is_fallback);

        let fallback_zero = engine
            .apply_qso(
                &resolver,
                &domains,
                Band::B20,
                Callsign::new("DL1ABC"),
                "599",
            )
            .unwrap();
        assert_eq!(fallback_zero.qso_points, 0);
        assert_eq!(fallback_zero.qso_points_rule_index, Some(1));
        assert!(fallback_zero.qso_points_is_fallback);
    }

    #[test]
    fn engine_error_implements_std_error() {
        let err = EngineError::InvalidConfig("x".to_string());
        let boxed: Box<dyn std::error::Error> = Box::new(err);
        assert!(boxed.to_string().contains("invalid config"));
    }

    #[test]
    fn name_and_location_normalization_handles_aliases() {
        let spec = load_spec("naqp");
        let mut resolver = InMemoryResolver::new();
        resolver.insert(
            "VE1ABC",
            ResolvedStation::new("VE", Continent::NA, true, true),
        );
        let domains = base_domains();
        let source = ResolvedStation::new("W", Continent::NA, true, true);
        let mut config = HashMap::new();
        config.insert("my_name".to_string(), Value::Text("CHRIS".to_string()));
        config.insert("my_loc".to_string(), Value::Text("MA".to_string()));
        let mut engine = SpecEngine::new(spec, source, config).unwrap();

        let result = engine
            .apply_qso(
                &resolver,
                &domains,
                Band::B20,
                Callsign::new("VE1ABC"),
                "BOB nwt",
            )
            .unwrap();
        assert!(result.new_mults.contains(&"na_mult:NT".to_string()));
    }

    #[test]
    fn standard_domain_pack_loads_from_specs_directory() {
        let provider = domain_packs::load_standard_domain_pack("specs/domains").unwrap();
        let arrl = provider.values("arrl_dx_wve_multipliers").unwrap();
        let naqp = provider.values("naqp_multipliers").unwrap();
        assert!(arrl.contains(&"MA".to_string()));
        assert!(arrl.contains(&"VO1".to_string()));
        assert!(naqp.contains(&"DC".to_string()));
    }

    #[test]
    fn domain_pack_integrity_checks() {
        let provider = domain_packs::load_standard_domain_pack("specs/domains").unwrap();
        let all = provider.values("dxcc_entities").unwrap();
        let excl = provider.values("dxcc_entities_excluding_w_ve").unwrap();
        assert!(all.len() > 300, "expected full DXCC list from cty.dat");
        assert!(excl.len() > 290, "expected W/VE-excluded list");
        assert!(!excl.contains(&"K".to_string()));
        assert!(!excl.contains(&"VE".to_string()));
        assert!(!excl.contains(&"W".to_string()));
        for token in excl.iter() {
            assert!(all.contains(token));
        }
    }
}
