# contest-engine

Spec-driven amateur radio contest scoring engine.

This project provides a reusable core library for:

- validating contest exchanges
- checking dupes
- calculating QSO points
- tracking multipliers
- computing claimed score

Contest behavior is defined by external spec files (`specs/*.json`), not hardcoded rules.

## Status

Implemented and tested:

- CQWW CW (`specs/cqww_cw.json`)
- ARRL DX (`specs/arrl_dx.json`)
- NAQP (`specs/naqp.json`)
- CWops CWT (`specs/cwt.json`)

Includes an ADIF scoring CLI: `src/bin/score_log.rs`.

## Repository Layout

- `src/spec.rs`: core engine, schema, validation, scoring logic
- `src/types.rs`: common types (`Band`, `Callsign`, `Continent`)
- `src/bin/score_log.rs`: ADIF log scoring tool
- `specs/*.json`: contest definitions
- `specs/domains/*.txt`: external domain packs (multipliers, DXCC lists)

## Build and Test

```bash
cargo test
```

## Engine Notes

- Specs are loaded via `ContestSpec::from_path(...)`.
- Station and domain data are injected via traits:
  - `StationResolver`
  - `DomainProvider`
- Supports multiplier/dupe dimensions:
  - `Band`
  - `BandMode`
  - `Global`
- Multiplier key expressions support:
  - direct field reference
  - `Concat([...])`
- Score schema includes:
  - `score.formula` (currently `points_times_mults`)

## Minimal Library Usage

```rust
use contest_engine::spec::{ContestSpec, InMemoryDomainProvider, InMemoryResolver, ResolvedStation, SpecSession, Value};
use contest_engine::types::{Band, Callsign, Continent};
use std::collections::HashMap;

let spec = ContestSpec::from_path("specs/cqww_cw.json")?;

let mut domains = InMemoryDomainProvider::new();
domains.insert("dxcc_entities", vec!["W".into(), "VE".into(), "DL".into()]);

let mut resolver = InMemoryResolver::new();
resolver.insert("DL1ABC", ResolvedStation::new("DL", Continent::EU, false, false));

let source = ResolvedStation::new("W", Continent::NA, true, true);
let mut config = HashMap::new();
config.insert("my_cq_zone".to_string(), Value::Int(5));

let mut session = SpecSession::new(spec, source, config, resolver, domains)?;
let summary = session.apply_qso(Band::B20, Callsign::new("DL1ABC"), "599 14")?;
println!("Claimed score: {}", summary.claimed_score);
# Ok::<(), Box<dyn std::error::Error>>(())
```

## Development Tool: Score an ADIF Log

`score_log` is intended as a development/debug utility for exercising specs against real logs.

The scorer needs:

- a contest spec (`--spec`)
- an ADIF file (`--log`)
- `cty.dat` for callsign resolution (`--cty`)
- domain pack directory (`--domains`)
- your callsign and contest config values

Example (CWT):

```bash
cargo run --bin score_log -- \
  --spec specs/cwt.json \
  --log N9UNX-CWT-20260212-0300z.adi \
  --cty ../contest_trainer/data/cty.dat \
  --domains specs/domains \
  --my-call N9UNX \
  --config my_name=CHRIS \
  --config my_xchg=MA
```

Example (NAQP):

```bash
cargo run --bin score_log -- \
  --spec specs/naqp.json \
  --log N9UNX-NAQP-20250110.adi \
  --cty ../contest_trainer/data/cty.dat \
  --domains specs/domains \
  --my-call N9UNX \
  --config my_name=CHRIS \
  --config my_loc=MA
```
