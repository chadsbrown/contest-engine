//! Regression fence for existing contest specs.
//!
//! For each shipped spec (SS, ARRL DX, CQ-WW, NAQP, CWT, NS Sprint, MST), this
//! harness replays a hand-designed synthetic log and asserts that the scoring
//! output matches a committed snapshot byte-for-byte. Any change to the
//! scoring path that alters output for any of these specs fails CI.
//!
//! To intentionally regenerate snapshots after a reviewed behavior change, run
//! the tests with `UPDATE_SNAPSHOTS=1`.
//!
//! The log and snapshot fixtures live under `tests/fixtures/logs/` and
//! `tests/fixtures/snapshots/`. Each spec has exactly one `<spec_id>.log.json`
//! and one `<spec_id>.score.json`.

use contest_engine::spec::{
    ContestSpec, InMemoryDomainProvider, InMemoryResolver, ResolvedStation, SpecSession, Value,
};
use contest_engine::types::{Band, Callsign, Continent};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};
use std::fs;
use std::path::PathBuf;

// ---------------------------------------------------------------------------
// Fixture format: synthetic log
// ---------------------------------------------------------------------------

/// Fixture files may include a top-level `description` field for humans;
/// serde will silently ignore it since we don't mirror it here.
#[derive(Debug, Clone, Deserialize)]
struct SyntheticLog {
    /// The spec id (matches the filename stem under `specs/`).
    spec_id: String,
    /// The source (my) station — dxcc, continent, is_wve, is_na.
    source: StationFixture,
    /// Operator config keyed by config field id. Values are typed as
    /// `{"Int": N}`, `{"Text": "..."}`, `{"Bool": true}`.
    #[serde(default)]
    config: HashMap<String, Value>,
    /// Optional variant name (e.g. "cw" or "ssb" for CQ-WW / ARRL-DX).
    #[serde(default)]
    variant: Option<String>,
    /// Stations the resolver should know about, keyed by callsign.
    stations: BTreeMap<String, StationFixture>,
    /// The ordered QSO list.
    qsos: Vec<QsoFixture>,
}

#[derive(Debug, Clone, Deserialize)]
struct StationFixture {
    dxcc: String,
    continent: String,
    is_wve: bool,
    is_na: bool,
}

impl StationFixture {
    fn into_resolved(self) -> ResolvedStation {
        ResolvedStation::new(
            self.dxcc,
            parse_continent(&self.continent),
            self.is_wve,
            self.is_na,
        )
    }
}

#[derive(Debug, Clone, Deserialize)]
struct QsoFixture {
    band: String,
    #[serde(default = "default_mode")]
    mode: String,
    call: String,
    exchange: String,
    #[serde(default)]
    epoch_seconds: i64,
}

fn default_mode() -> String {
    "CW".to_string()
}

fn parse_continent(raw: &str) -> Continent {
    match raw.trim().to_ascii_uppercase().as_str() {
        "NA" => Continent::NA,
        "SA" => Continent::SA,
        "EU" => Continent::EU,
        "AF" => Continent::AF,
        "AS" => Continent::AS,
        "OC" => Continent::OC,
        "AN" => Continent::AN,
        other => panic!("unknown continent '{}'", other),
    }
}

fn parse_band(raw: &str) -> Band {
    match raw.trim().to_ascii_uppercase().as_str() {
        "B160" => Band::B160,
        "B80" => Band::B80,
        "B60" => Band::B60,
        "B40" => Band::B40,
        "B30" => Band::B30,
        "B20" => Band::B20,
        "B17" => Band::B17,
        "B15" => Band::B15,
        "B12" => Band::B12,
        "B10" => Band::B10,
        "B6" => Band::B6,
        "B2" => Band::B2,
        other => panic!("unknown band '{}'", other),
    }
}

fn parse_mode(raw: &str) -> contest_engine::spec::Mode {
    use contest_engine::spec::Mode;
    match raw.trim().to_ascii_uppercase().as_str() {
        "CW" => Mode::CW,
        "SSB" | "PH" | "PHONE" => Mode::SSB,
        "RTTY" => Mode::RTTY,
        "FT8" => Mode::FT8,
        "FT4" => Mode::FT4,
        "DIGITAL" | "DG" => Mode::DIGITAL,
        other => panic!("unknown mode '{}'", other),
    }
}

// ---------------------------------------------------------------------------
// Fixture format: committed snapshot
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct ScoreSnapshot {
    total_qsos: u32,
    dupe_qsos: u32,
    total_points: i64,
    total_mults: usize,
    claimed_score: i64,
    /// Sum of QSO points per band ("B20" → 6). Sorted by band for determinism.
    points_by_band: BTreeMap<String, i64>,
    /// All distinct multiplier values per mult id, sorted for determinism.
    /// Collapses across scope — if a band-dimension mult is worked on both
    /// 20m and 40m, it appears once. This is fine as a regression signature
    /// because `claimed_score` captures the band-multiplicity separately.
    mults: BTreeMap<String, Vec<String>>,
}

fn band_to_str(b: Band) -> &'static str {
    match b {
        Band::B160 => "B160",
        Band::B80 => "B80",
        Band::B60 => "B60",
        Band::B40 => "B40",
        Band::B30 => "B30",
        Band::B20 => "B20",
        Band::B17 => "B17",
        Band::B15 => "B15",
        Band::B12 => "B12",
        Band::B10 => "B10",
        Band::B6 => "B6",
        Band::B2 => "B2",
    }
}

// ---------------------------------------------------------------------------
// Harness
// ---------------------------------------------------------------------------

fn manifest_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

/// Build the fixture filename stem. A `None` scenario uses the spec id alone
/// (`inqp.log.json`); a scenario produces `inqp_<scenario>.log.json` and is
/// used for cases that exercise an alternate config path (in-state vs
/// out-of-state, mode variants, etc.) without colliding with the baseline.
fn fixture_stem(spec_id: &str, scenario: Option<&str>) -> String {
    match scenario {
        Some(s) => format!("{spec_id}_{s}"),
        None => spec_id.to_string(),
    }
}

fn load_log(spec_id: &str, scenario: Option<&str>) -> SyntheticLog {
    let stem = fixture_stem(spec_id, scenario);
    let path = manifest_dir()
        .join("tests/fixtures/logs")
        .join(format!("{stem}.log.json"));
    let raw = fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("failed to read {}: {}", path.display(), e));
    serde_json::from_str(&raw)
        .unwrap_or_else(|e| panic!("failed to parse {}: {}", path.display(), e))
}

fn load_snapshot(spec_id: &str, scenario: Option<&str>) -> Option<ScoreSnapshot> {
    let stem = fixture_stem(spec_id, scenario);
    let path = manifest_dir()
        .join("tests/fixtures/snapshots")
        .join(format!("{stem}.score.json"));
    if !path.exists() {
        return None;
    }
    let raw = fs::read_to_string(&path).expect("read snapshot");
    Some(serde_json::from_str(&raw).expect("parse snapshot"))
}

fn write_snapshot(spec_id: &str, scenario: Option<&str>, snap: &ScoreSnapshot) {
    let stem = fixture_stem(spec_id, scenario);
    let path = manifest_dir()
        .join("tests/fixtures/snapshots")
        .join(format!("{stem}.score.json"));
    let raw = serde_json::to_string_pretty(snap).expect("serialize snapshot");
    fs::write(&path, format!("{raw}\n")).expect("write snapshot");
}

/// Load every .txt file under `specs/domains/` into an `InMemoryDomainProvider`,
/// keyed by the filename stem. This gives tests the real domain data (SS
/// sections, NAQP multipliers, DXCC entities, etc.) and picks up any new
/// domain files added by later phases (county lists, bonus-station lists)
/// without harness changes.
fn load_all_domains() -> InMemoryDomainProvider {
    let dir = manifest_dir().join("specs/domains");
    let mut provider = InMemoryDomainProvider::new();
    let entries = fs::read_dir(&dir)
        .unwrap_or_else(|e| panic!("failed to read domains dir {}: {}", dir.display(), e));
    for entry in entries {
        let entry = entry.expect("read_dir entry");
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) != Some("txt") {
            continue;
        }
        let stem = path
            .file_stem()
            .and_then(|s| s.to_str())
            .expect("txt file stem")
            .to_string();
        let raw = fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("failed to read {}: {}", path.display(), e));
        let values: Vec<String> = raw
            .lines()
            .map(|l| l.trim())
            .filter(|l| !l.is_empty() && !l.starts_with('#'))
            .map(|l| l.to_ascii_uppercase())
            .collect();
        provider.insert(stem, values);
    }
    provider
}

fn load_spec(spec_id: &str) -> ContestSpec {
    let path = manifest_dir().join(format!("specs/{spec_id}.json"));
    ContestSpec::from_path(&path)
        .unwrap_or_else(|e| panic!("failed to load spec {}: {}", spec_id, e))
}

fn run_log_and_score(log: SyntheticLog) -> ScoreSnapshot {
    let spec = load_spec(&log.spec_id);
    let mult_ids: Vec<String> = spec.multipliers.iter().map(|m| m.id.clone()).collect();

    let source = log.source.into_resolved();

    let mut resolver = InMemoryResolver::new();
    for (call, fixture) in log.stations {
        resolver.insert(call, fixture.into_resolved());
    }

    let domains = load_all_domains();

    let config: HashMap<String, Value> = log.config.into_iter().collect();

    let meta = contest_engine::spec::SessionMeta {
        variant: log.variant,
        category_mode: None,
    };

    let mut session = SpecSession::new_with_meta(spec, source, config, resolver, domains, meta)
        .expect("session init");

    let mut dupe_qsos: u32 = 0;
    let mut points_by_band: BTreeMap<String, i64> = BTreeMap::new();

    for q in log.qsos {
        let band = parse_band(&q.band);
        let mode = parse_mode(&q.mode);
        let call = Callsign::new(&q.call);
        let summary = session
            .apply_qso_at_with_mode(band, mode, q.epoch_seconds, call, &q.exchange)
            .unwrap_or_else(|e| {
                panic!(
                    "apply_qso failed for {} {} {}: {:?}",
                    q.call, q.band, q.exchange, e
                )
            });
        if summary.is_dupe {
            dupe_qsos += 1;
        } else {
            *points_by_band
                .entry(band_to_str(band).to_string())
                .or_insert(0) += summary.qso_points;
        }
    }

    let engine = session.engine();
    let total_qsos = engine.total_qsos();
    let total_points = engine.total_points();
    let total_mults = engine.total_mults();
    let claimed_score = engine.claimed_score();

    let mut mults: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for id in mult_ids {
        let mut values = engine.worked_mults(&id, None);
        values.sort();
        values.dedup();
        mults.insert(id, values);
    }

    ScoreSnapshot {
        total_qsos,
        dupe_qsos,
        total_points,
        total_mults,
        claimed_score,
        points_by_band,
        mults,
    }
}

fn run_regression(spec_id: &str) {
    run_regression_scenario(spec_id, None);
}

fn run_regression_scenario(spec_id: &str, scenario: Option<&str>) {
    let log = load_log(spec_id, scenario);
    assert_eq!(
        log.spec_id, spec_id,
        "log file's spec_id must match harness"
    );
    let actual = run_log_and_score(log);
    let label = fixture_stem(spec_id, scenario);

    if std::env::var("UPDATE_SNAPSHOTS").is_ok() {
        write_snapshot(spec_id, scenario, &actual);
        eprintln!("updated snapshot for {}", label);
        return;
    }

    match load_snapshot(spec_id, scenario) {
        None => {
            let stem = fixture_stem(spec_id, scenario);
            let path = manifest_dir()
                .join("tests/fixtures/snapshots")
                .join(format!("{stem}.score.json"));
            panic!(
                "no snapshot at {}; run UPDATE_SNAPSHOTS=1 cargo test --test regression_existing_specs to create one",
                path.display()
            );
        }
        Some(expected) => {
            if actual != expected {
                let actual_str =
                    serde_json::to_string_pretty(&actual).expect("serialize actual");
                let expected_str =
                    serde_json::to_string_pretty(&expected).expect("serialize expected");
                panic!(
                    "regression for {}:\n--- expected ---\n{}\n--- actual ---\n{}",
                    label, expected_str, actual_str
                );
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Per-spec tests
// ---------------------------------------------------------------------------

#[test]
fn regression_ss() {
    run_regression("ss");
}

#[test]
fn regression_arrl_dx() {
    run_regression("arrl_dx");
}

#[test]
fn regression_cqww() {
    run_regression("cqww");
}

#[test]
fn regression_cqww_cw() {
    run_regression("cqww_cw");
}

#[test]
fn regression_naqp() {
    run_regression("naqp");
}

#[test]
fn regression_cwt() {
    run_regression("cwt");
}

#[test]
fn regression_ns_sprint() {
    run_regression("ns_sprint");
}

#[test]
fn regression_mst() {
    run_regression("mst");
}

#[test]
fn regression_nhqp() {
    // NHQP is the Phase 0 smoke test for writing a brand-new spec. It's added
    // here so it joins the regression fence from the moment the spec file is
    // committed; any future change that breaks NHQP scoring will fail CI
    // alongside the established baseline specs.
    run_regression("nhqp");
}

// --- Phase 1 new specs: rover-identity-only state QSO parties ---

#[test]
fn regression_gaqp() {
    run_regression("gaqp");
}

#[test]
fn regression_ndqp() {
    run_regression("ndqp");
}

#[test]
fn regression_miqp() {
    run_regression("miqp");
}

#[test]
fn regression_inqp() {
    run_regression("inqp");
}

#[test]
fn regression_inqp_in_state() {
    // In-state Indiana operator: exercises the my_is_in=true path that
    // baseline `regression_inqp` (out-of-state) doesn't reach. Locks in
    // credit for in-state IN counties + US states + CA provinces + DXCC.
    run_regression_scenario("inqp", Some("in_state"));
}

#[test]
fn regression_neqp() {
    run_regression("neqp");
}

// --- Phase 2 new specs: rover identity + bonus rules ---

#[test]
fn regression_moqp() {
    run_regression("moqp");
}

#[test]
fn regression_qcqp() {
    run_regression("qcqp");
}

#[test]
fn regression_onqp() {
    run_regression("onqp");
}

// --- Phase 3 new specs: rover identity + power multiplier ---

#[test]
fn regression_deqp() {
    run_regression("deqp");
}

#[test]
fn regression_nmqp() {
    run_regression("nmqp");
}

#[test]
fn regression_neqsop() {
    run_regression("neqsop");
}

// --- Phase 4 new specs: rover identity + multi-mult per QSO ---

#[test]
fn regression_flqp() {
    run_regression("flqp");
}
