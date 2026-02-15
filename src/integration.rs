use crate::spec::{DomainProvider, ResolvedStation, StationResolver};
use crate::types::{Callsign, Continent};
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternalCallInfo {
    pub dxcc: String,
    pub continent: Continent,
    pub is_wve: bool,
    pub is_na: bool,
}

pub trait ExternalCallInfoSource {
    fn lookup_call(&self, call: &str) -> Option<ExternalCallInfo>;
}

pub trait ExternalDomainSource {
    fn lookup_domain(&self, name: &str) -> Option<Vec<String>>;
}

pub struct StationResolverAdapter<S> {
    source: S,
}

impl<S> StationResolverAdapter<S> {
    pub fn new(source: S) -> Self {
        Self { source }
    }
}

impl<S> StationResolver for StationResolverAdapter<S>
where
    S: ExternalCallInfoSource,
{
    fn resolve(&self, call: &Callsign) -> Result<ResolvedStation, String> {
        let info = self
            .source
            .lookup_call(call.as_str())
            .ok_or_else(|| format!("unknown callsign {}", call.as_str()))?;
        Ok(ResolvedStation::new(
            info.dxcc,
            info.continent,
            info.is_wve,
            info.is_na,
        ))
    }
}

pub struct DomainProviderAdapter<S> {
    source: S,
}

impl<S> DomainProviderAdapter<S> {
    pub fn new(source: S) -> Self {
        Self { source }
    }
}

impl<S> DomainProvider for DomainProviderAdapter<S>
where
    S: ExternalDomainSource,
{
    fn values(&self, domain_name: &str) -> Option<Arc<[String]>> {
        self.source.lookup_domain(domain_name).map(|values| {
            let normalized: Vec<String> = values
                .into_iter()
                .map(|v| v.trim().to_ascii_uppercase())
                .collect();
            Arc::<[String]>::from(normalized)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[derive(Default)]
    struct FakeExternalSource {
        calls: HashMap<String, ExternalCallInfo>,
        domains: HashMap<String, Vec<String>>,
    }

    impl ExternalCallInfoSource for FakeExternalSource {
        fn lookup_call(&self, call: &str) -> Option<ExternalCallInfo> {
            self.calls.get(&call.to_ascii_uppercase()).cloned()
        }
    }

    impl ExternalDomainSource for FakeExternalSource {
        fn lookup_domain(&self, name: &str) -> Option<Vec<String>> {
            self.domains.get(name).cloned()
        }
    }

    #[test]
    fn adapter_resolves_call_info() {
        let mut src = FakeExternalSource::default();
        src.calls.insert(
            "DL1ABC".to_string(),
            ExternalCallInfo {
                dxcc: "DL".to_string(),
                continent: Continent::EU,
                is_wve: false,
                is_na: false,
            },
        );
        let adapter = StationResolverAdapter::new(src);
        let resolved = adapter.resolve(&Callsign::new("DL1ABC")).unwrap();
        assert_eq!(resolved.dxcc, "DL");
        assert_eq!(resolved.continent, Continent::EU);
    }

    #[test]
    fn adapter_normalizes_domain_values() {
        let mut src = FakeExternalSource::default();
        src.domains.insert(
            "naqp_multipliers".to_string(),
            vec!["ma".to_string(), "nh".to_string()],
        );
        let adapter = DomainProviderAdapter::new(src);
        let values = adapter.values("naqp_multipliers").unwrap();
        assert_eq!(values.as_ref(), ["MA".to_string(), "NH".to_string()]);
    }
}
