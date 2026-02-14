use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Band {
    B160,
    B80,
    B40,
    B20,
    B15,
    B10,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Callsign(String);

impl Callsign {
    pub fn new(value: impl AsRef<str>) -> Self {
        Self(value.as_ref().trim().to_ascii_uppercase())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Continent {
    NA,
    SA,
    EU,
    AF,
    AS,
    OC,
    AN,
}
