use adif_parser::{Record, parse_adi};
use contest_engine::spec::{
    ContestSpec, DomainProvider, Mode, ResolvedStation, SpecEngine, StationResolver, Value,
};
use contest_engine::types::{Band, Callsign, Continent};
use station_data::{CtyDb, DomainPack};
use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::Path;
use std::sync::Arc;

struct StationDataResolver {
    db: CtyDb,
}

impl StationDataResolver {
    fn from_file(path: &str) -> Result<Self, String> {
        let db = CtyDb::from_path(Path::new(path))
            .map_err(|e| format!("failed reading/parsing cty file: {e}"))?;
        Ok(Self { db })
    }

    fn resolve_call(&self, call: &str) -> Option<ResolvedStation> {
        let hit = self.db.lookup(call)?;
        let continent = parse_continent_code(&hit.continent)?;
        Some(ResolvedStation::new(
            hit.dxcc,
            continent,
            hit.is_wve,
            hit.is_na,
        ))
    }
}

impl StationResolver for StationDataResolver {
    fn resolve(&self, call: &Callsign) -> Result<ResolvedStation, String> {
        self.resolve_call(call.as_str())
            .ok_or_else(|| format!("unknown callsign {}", call.as_str()))
    }
}

#[derive(Clone)]
struct StationDataDomains {
    pack: DomainPack,
}

impl StationDataDomains {
    fn from_dir(path: &str) -> Result<Self, String> {
        let pack = DomainPack::from_dir(Path::new(path))
            .map_err(|e| format!("failed reading/parsing domain directory: {e}"))?;
        Ok(Self { pack })
    }
}

impl DomainProvider for StationDataDomains {
    fn values(&self, domain_name: &str) -> Option<Arc<[String]>> {
        self.pack.values(domain_name)
    }
}

fn parse_continent_code(input: &str) -> Option<Continent> {
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

    let domains = StationDataDomains::from_dir(domains_dir)?;
    let resolver = StationDataResolver::from_file(cty_path)?;
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
