use crate::spec::{DomainProvider, ResolvedStation, StationResolver};
use crate::types::Callsign;
use std::collections::HashMap;

#[derive(Debug, Default, Clone)]
pub struct StaticStationResolver {
    map: HashMap<String, ResolvedStation>,
}

impl StaticStationResolver {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, call: impl AsRef<str>, station: ResolvedStation) {
        self.map
            .insert(call.as_ref().trim().to_ascii_uppercase(), station);
    }
}

impl StationResolver for StaticStationResolver {
    fn resolve(&self, call: &Callsign) -> Result<ResolvedStation, String> {
        self.map
            .get(call.as_str())
            .cloned()
            .ok_or_else(|| format!("unknown callsign {}", call.as_str()))
    }
}

pub struct FnStationResolver<F> {
    lookup: F,
}

impl<F> FnStationResolver<F> {
    pub fn new(lookup: F) -> Self {
        Self { lookup }
    }
}

impl<F> StationResolver for FnStationResolver<F>
where
    F: Fn(&Callsign) -> Option<ResolvedStation>,
{
    fn resolve(&self, call: &Callsign) -> Result<ResolvedStation, String> {
        (self.lookup)(call).ok_or_else(|| format!("unknown callsign {}", call.as_str()))
    }
}

#[derive(Debug, Default, Clone)]
pub struct StaticDomainProvider {
    map: HashMap<String, Vec<String>>,
}

impl StaticDomainProvider {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, name: impl AsRef<str>, values: Vec<String>) {
        self.map.insert(
            name.as_ref().to_string(),
            values
                .into_iter()
                .map(|v| v.trim().to_ascii_uppercase())
                .collect(),
        );
    }
}

impl DomainProvider for StaticDomainProvider {
    fn values(&self, domain_name: &str) -> Option<Vec<String>> {
        self.map.get(domain_name).cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::Continent;

    #[test]
    fn static_station_resolver_maps_calls() {
        let mut resolver = StaticStationResolver::new();
        resolver.insert(
            "DL1ABC",
            ResolvedStation::new("DL", Continent::EU, false, false),
        );
        let resolved = resolver.resolve(&Callsign::new("DL1ABC")).unwrap();
        assert_eq!(resolved.dxcc, "DL");
    }

    #[test]
    fn fn_station_resolver_maps_found_and_missing() {
        let resolver = FnStationResolver::new(|call: &Callsign| {
            if call.as_str() == "DL1ABC" {
                Some(ResolvedStation::new("DL", Continent::EU, false, false))
            } else {
                None
            }
        });
        assert_eq!(
            resolver.resolve(&Callsign::new("DL1ABC")).unwrap().dxcc,
            "DL"
        );
        assert!(resolver.resolve(&Callsign::new("UNKNOWN")).is_err());
    }

    #[test]
    fn static_domain_provider_returns_uppercase_domains() {
        let mut domains = StaticDomainProvider::new();
        domains.insert("naqp_multipliers", vec!["ma".to_string(), "nh".to_string()]);
        let values = domains.values("naqp_multipliers").unwrap();
        assert_eq!(values, vec!["MA".to_string(), "NH".to_string()]);
    }
}
