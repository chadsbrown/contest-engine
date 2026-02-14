# Contest Specs

This directory contains runtime contest definition files for the spec-driven engine.

Current files:
- `specs/cqww_cw.json`
- `specs/arrl_dx.json`
- `specs/naqp.json`
- `specs/domains/arrl_dx_wve_multipliers.txt`
- `specs/domains/naqp_multipliers.txt`
- `specs/domains/dxcc_entities_excluding_w_ve.txt`
- `specs/domains/dxcc_entities.txt`

These are intended as application/runtime assets (not test fixtures). Use
`contest_engine::spec::ContestSpec::from_path(...)` to load them.

For multiplier domain files, use
`contest_engine::spec::domain_packs::load_standard_domain_pack("specs/domains")`.

`specs/domains/dxcc_entities*.txt` are generated from local `cty.dat` data
(currently from `contest_trainer/data/cty.dat`) and normalized to uppercase.
