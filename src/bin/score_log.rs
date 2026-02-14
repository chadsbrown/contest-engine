use adif_parser::{Record, parse_adi};
use contest_engine::spec::{
    ContestSpec, Mode, ResolvedStation, SpecEngine, StationResolver, Value, domain_packs,
};
use contest_engine::types::{Band, Callsign, Continent};
use std::collections::HashMap;
use std::env;
use std::fs;

#[derive(Clone)]
struct CtyEntry {
    dxcc: String,
    continent: Continent,
}

struct CtyResolver {
    exact: HashMap<String, CtyEntry>,
    prefixes: Vec<(String, CtyEntry)>,
}

impl CtyResolver {
    fn from_file(path: &str) -> Result<Self, String> {
        let raw = fs::read_to_string(path).map_err(|e| format!("failed reading cty file: {e}"))?;
        Ok(Self::parse(&raw))
    }

    fn parse(raw: &str) -> Self {
        let mut exact = HashMap::new();
        let mut prefixes = Vec::new();
        let mut current: Option<CtyEntry> = None;
        let mut alias_buf = String::new();

        for line in raw.lines() {
            let line = line.trim();
            if line.is_empty() {
                continue;
            }
            if is_cty_header(line) {
                if let Some(entry) = current.as_ref() {
                    parse_aliases(&alias_buf, entry, &mut exact, &mut prefixes);
                }
                alias_buf.clear();
                current = parse_cty_header(line);
            } else {
                alias_buf.push_str(line);
                alias_buf.push(' ');
            }
        }
        if let Some(entry) = current.as_ref() {
            parse_aliases(&alias_buf, entry, &mut exact, &mut prefixes);
        }

        prefixes.sort_by(|a, b| b.0.len().cmp(&a.0.len()));
        Self { exact, prefixes }
    }

    fn resolve_call(&self, call: &str) -> Option<ResolvedStation> {
        let c = call.trim().to_ascii_uppercase();
        if let Some(entry) = self.exact.get(&c) {
            return Some(to_station(entry));
        }
        for (prefix, entry) in &self.prefixes {
            if c.starts_with(prefix) {
                return Some(to_station(entry));
            }
        }
        None
    }
}

impl StationResolver for CtyResolver {
    fn resolve(&self, call: &Callsign) -> Result<ResolvedStation, String> {
        self.resolve_call(call.as_str())
            .ok_or_else(|| format!("unknown callsign {}", call.as_str()))
    }
}

fn to_station(entry: &CtyEntry) -> ResolvedStation {
    let is_wve = matches!(entry.dxcc.as_str(), "K" | "W" | "VE");
    let is_na = entry.continent == Continent::NA;
    ResolvedStation::new(entry.dxcc.clone(), entry.continent, is_wve, is_na)
}

fn is_cty_header(line: &str) -> bool {
    line.matches(':').count() >= 7 && !line.ends_with(';') && !line.ends_with(',')
}

fn parse_continent(input: &str) -> Option<Continent> {
    match input.trim().to_ascii_uppercase().as_str() {
        "NA" => Some(Continent::NA),
        "SA" => Some(Continent::SA),
        "EU" => Some(Continent::EU),
        "AF" => Some(Continent::AF),
        "AS" => Some(Continent::AS),
        "OC" => Some(Continent::OC),
        "AN" => Some(Continent::AN),
        _ => None,
    }
}

fn parse_cty_header(line: &str) -> Option<CtyEntry> {
    let parts: Vec<&str> = line.split(':').collect();
    if parts.len() < 8 {
        return None;
    }
    let continent = parse_continent(parts[3])?;
    let dxcc = parts[7].trim().trim_start_matches('*').to_ascii_uppercase();
    if dxcc.is_empty() {
        return None;
    }
    Some(CtyEntry { dxcc, continent })
}

fn parse_aliases(
    aliases: &str,
    entry: &CtyEntry,
    exact: &mut HashMap<String, CtyEntry>,
    prefixes: &mut Vec<(String, CtyEntry)>,
) {
    let aliases = aliases.trim().trim_end_matches(';');
    for alias in aliases.split(',') {
        let alias = alias.trim();
        if alias.is_empty() {
            continue;
        }
        let (base, is_exact) = parse_alias(alias);
        if base.is_empty() {
            continue;
        }
        if is_exact {
            exact.insert(base.clone(), entry.clone());
        } else {
            prefixes.push((base, entry.clone()));
        }
    }
}

fn parse_alias(alias: &str) -> (String, bool) {
    let mut s = alias;
    let mut is_exact = false;
    if s.starts_with('=') {
        is_exact = true;
        s = &s[1..];
    }
    let mut out = String::new();
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '(' => skip_until(&mut chars, ')'),
            '[' => skip_until(&mut chars, ']'),
            '{' => skip_until(&mut chars, '}'),
            '<' => skip_until(&mut chars, '>'),
            '~' => skip_until(&mut chars, '~'),
            _ => out.push(c),
        }
    }
    (out.trim().to_ascii_uppercase(), is_exact)
}

fn skip_until(chars: &mut std::iter::Peekable<std::str::Chars<'_>>, end: char) {
    while let Some(&c) = chars.peek() {
        chars.next();
        if c == end {
            break;
        }
    }
}

fn parse_band_from_record(record: &Record) -> Option<Band> {
    if let Some(band) = record.get_value("BAND") {
        let upper = band.trim().to_ascii_uppercase();
        let digits: String = upper.chars().filter(|c| c.is_ascii_digit()).collect();
        return match digits.as_str() {
            "160" => Some(Band::B160),
            "80" => Some(Band::B80),
            "40" => Some(Band::B40),
            "20" => Some(Band::B20),
            "15" => Some(Band::B15),
            "10" => Some(Band::B10),
            _ => None,
        };
    }
    if let Some(freq) = record.get_value("FREQ") {
        if let Ok(mhz) = freq.trim().parse::<f64>() {
            let khz = (mhz * 1000.0).round() as u32;
            return match khz {
                1800..=2000 => Some(Band::B160),
                3500..=4000 => Some(Band::B80),
                7000..=7300 => Some(Band::B40),
                14000..=14350 => Some(Band::B20),
                21000..=21450 => Some(Band::B15),
                28000..=29700 => Some(Band::B10),
                _ => None,
            };
        }
    }
    None
}

fn parse_mode_from_record(record: &Record) -> Option<Mode> {
    let raw = record.get_value("MODE")?.trim().to_ascii_uppercase();
    match raw.as_str() {
        "CW" => Some(Mode::CW),
        "SSB" | "PH" | "PHONE" | "LSB" | "USB" | "FM" | "AM" => Some(Mode::SSB),
        "RTTY" | "RY" | "DIGI" | "DIGITALVOICE" => Some(Mode::RTTY),
        _ => None,
    }
}

fn parse_args() -> Result<HashMap<String, Vec<String>>, String> {
    let mut map: HashMap<String, Vec<String>> = HashMap::new();
    let mut args = env::args().skip(1).peekable();
    while let Some(arg) = args.next() {
        if !arg.starts_with("--") {
            return Err(format!("unexpected argument {arg}"));
        }
        let key = arg.trim_start_matches("--").to_string();
        let val = args
            .next()
            .ok_or_else(|| format!("missing value for --{}", key))?;
        map.entry(key).or_default().push(val);
    }
    Ok(map)
}

fn parse_value(raw: &str) -> Value {
    if raw.eq_ignore_ascii_case("true") {
        return Value::Bool(true);
    }
    if raw.eq_ignore_ascii_case("false") {
        return Value::Bool(false);
    }
    if let Ok(v) = raw.parse::<i64>() {
        return Value::Int(v);
    }
    Value::Text(raw.to_string())
}

fn collapse_ws(s: &str) -> String {
    s.split_whitespace().collect::<Vec<&str>>().join(" ")
}

fn is_rst_like(token: &str) -> bool {
    matches!(
        token.trim().to_ascii_uppercase().as_str(),
        "599" | "59" | "5NN" | "5N" | "NN" | "9NN" | "579" | "589"
    )
}

fn first_value(record: &Record, names: &[&str]) -> Option<String> {
    names
        .iter()
        .find_map(|name| record.get_value(name).map(|v| v.trim().to_string()))
        .filter(|s| !s.is_empty())
}

fn build_exchange(spec_id: &str, record: &Record) -> Option<String> {
    let rst = record.get_value("RST_RCVD").map(str::trim).unwrap_or("");
    let srx = record
        .get_value("SRX_STRING")
        .map(collapse_ws)
        .filter(|s| !s.is_empty());

    match spec_id {
        "naqp" => {
            if let Some(ex) = srx {
                let parts: Vec<&str> = ex.split_whitespace().collect();
                if parts.len() >= 2 {
                    return Some(ex);
                }
                let loc = first_value(record, &["QTH", "STATE", "VE_PROV", "SRX"])?;
                return Some(format!("{ex} {loc}"));
            }
            let name = first_value(record, &["NAME", "OPERATOR"])?;
            let loc = first_value(record, &["QTH", "STATE", "VE_PROV", "SRX"]);
            Some(match loc {
                Some(v) => format!("{name} {v}"),
                None => name,
            })
        }
        "cqww_cw" => {
            if let Some(mut ex) = srx {
                if !rst.is_empty() {
                    let first = ex.split_whitespace().next().unwrap_or_default();
                    if !is_rst_like(first) {
                        ex = format!("{rst} {ex}");
                    }
                }
                return Some(ex);
            }
            let zone = first_value(record, &["CQZ", "ZONE", "CQ_ZONE"])?;
            if rst.is_empty() {
                None
            } else {
                Some(format!("{rst} {zone}"))
            }
        }
        "arrl_dx" => {
            if let Some(mut ex) = srx {
                if !rst.is_empty() {
                    let first = ex.split_whitespace().next().unwrap_or_default();
                    if !is_rst_like(first) {
                        ex = format!("{rst} {ex}");
                    }
                }
                return Some(ex);
            }
            let token = first_value(record, &["STATE", "SRX", "RX_PWR", "ARRL_SECT"])?;
            if rst.is_empty() {
                Some(token)
            } else {
                Some(format!("{rst} {token}"))
            }
        }
        "cwt" => {
            if let Some(ex) = srx {
                let mut parts: Vec<String> =
                    ex.split_whitespace().map(|s| s.to_string()).collect();
                if parts.first().is_some_and(|t| is_rst_like(t)) {
                    parts.remove(0);
                }
                if parts.len() >= 2 {
                    return Some(format!("{} {}", parts[0], parts[1]));
                }
            }
            let name = first_value(record, &["NAME", "OPERATOR"])?;
            let xchg = first_value(
                record,
                &["SRX", "STATE", "VE_PROV", "QTH", "COUNTRY", "SRX_STRING"],
            )?;
            Some(format!("{name} {xchg}"))
        }
        _ => {
            if let Some(ex) = srx {
                return Some(ex);
            }
            if !rst.is_empty() {
                Some(rst.to_string())
            } else {
                first_value(record, &["SRX", "SRX_STRING"])
            }
        }
    }
}

fn main() -> Result<(), String> {
    let args = parse_args()?;
    let spec_path = args
        .get("spec")
        .and_then(|v| v.first())
        .ok_or_else(|| "--spec is required".to_string())?;
    let log_path = args
        .get("log")
        .and_then(|v| v.first())
        .ok_or_else(|| "--log is required".to_string())?;
    let cty_path = args
        .get("cty")
        .and_then(|v| v.first())
        .ok_or_else(|| "--cty is required".to_string())?;
    let domains_dir = args
        .get("domains")
        .and_then(|v| v.first())
        .ok_or_else(|| "--domains is required".to_string())?;
    let my_call = args
        .get("my-call")
        .and_then(|v| v.first())
        .ok_or_else(|| "--my-call is required".to_string())?;

    let spec = ContestSpec::from_path(spec_path)?;
    let spec_id = spec.id.clone();
    let default_mode = if spec.modes.len() == 1 {
        spec.modes.first().copied()
    } else {
        None
    };
    let domains = domain_packs::load_standard_domain_pack(domains_dir)?;
    let resolver = CtyResolver::from_file(cty_path)?;
    let source = resolver
        .resolve_call(my_call)
        .ok_or_else(|| format!("my call not resolvable from cty: {my_call}"))?;

    let mut config = HashMap::new();
    if let Some(items) = args.get("config") {
        for item in items {
            let mut parts = item.splitn(2, '=');
            let key = parts.next().unwrap_or_default().trim();
            let val = parts.next().unwrap_or_default().trim();
            if key.is_empty() {
                return Err(format!("invalid --config entry {item}"));
            }
            config.insert(key.to_string(), parse_value(val));
        }
    }

    let mut engine =
        SpecEngine::new(spec, source, config).map_err(|e| format!("engine init error: {e:?}"))?;
    let log_raw = fs::read_to_string(log_path).map_err(|e| format!("failed reading log: {e}"))?;
    let adif = parse_adi(&log_raw).map_err(|e| format!("adif parse error: {e}"))?;

    let mut applied = 0usize;
    let mut skipped = 0usize;
    let mut failed = 0usize;
    let mut fail_reasons: HashMap<String, usize> = HashMap::new();

    for record in &adif.records {
        let call = match record.call() {
            Some(v) => Callsign::new(v),
            None => {
                skipped += 1;
                continue;
            }
        };
        let band = match parse_band_from_record(record) {
            Some(v) => v,
            None => {
                skipped += 1;
                continue;
            }
        };
        let raw_exchange = match build_exchange(&spec_id, record) {
            Some(v) => v,
            None => {
                skipped += 1;
                continue;
            }
        };
        let mode = match parse_mode_from_record(record).or(default_mode) {
            Some(v) => v,
            None => {
                skipped += 1;
                continue;
            }
        };

        match engine.apply_qso_with_mode(&resolver, &domains, band, mode, call, &raw_exchange) {
            Ok(_) => applied += 1,
            Err(err) => {
                failed += 1;
                let key = format!("{err:?}");
                *fail_reasons.entry(key).or_insert(0) += 1;
            }
        }
    }

    println!("Scoring complete (ADIF)");
    println!("Records: {}", adif.records.len());
    println!("Applied: {applied}");
    println!("Skipped: {skipped}");
    println!("Failed: {failed}");
    println!("Total QSOs: {}", engine.total_qsos());
    println!("QSO Points: {}", engine.total_points());
    println!("Multipliers: {}", engine.total_mults());
    println!("Claimed Score: {}", engine.claimed_score());
    if !fail_reasons.is_empty() {
        println!("Failure reasons:");
        let mut items: Vec<(String, usize)> = fail_reasons.into_iter().collect();
        items.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));
        for (reason, count) in items.into_iter().take(10) {
            println!("  {count}x {reason}");
        }
    }

    Ok(())
}
