use contest_engine::spec::{
    InMemoryDomainProvider, InMemoryResolver, ResolvedStation, SpecSession, Value, ContestSpec,
};
use contest_engine::types::{Band, Callsign, Continent};
use std::collections::HashMap;

fn main() {
    let spec = ContestSpec::from_path("specs/cqww_cw.json").expect("load cqww spec");

    let mut domains = InMemoryDomainProvider::new();
    domains.insert(
        "dxcc_entities",
        vec![
            "W".to_string(),
            "VE".to_string(),
            "DL".to_string(),
            "G".to_string(),
        ],
    );

    let mut resolver = InMemoryResolver::new();
    resolver.insert(
        "DL1ABC",
        ResolvedStation::new("DL", Continent::EU, false, false),
    );

    let source = ResolvedStation::new("W", Continent::NA, true, true);
    let mut config: HashMap<String, Value> = HashMap::new();
    config.insert("my_cq_zone".to_string(), Value::Int(5));
    let mut session = SpecSession::new(spec, source, config, resolver, domains)
        .expect("session should initialize with valid config");

    let result = session
        .apply_qso(Band::B20, Callsign::new("DL1ABC"), "599 14")
        .expect("example QSO should apply");
    println!(
        "Applied QSO: points={}, new_mults={:?}, score={}",
        result.qso_points, result.new_mults, result.claimed_score
    );
}
