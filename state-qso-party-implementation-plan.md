# Implementation Plan: Engine Gaps + 13 State QSO Party Specs

**Status: ✅ Phases 0–4 complete. 12 of 13 target specs shipped. 7QP deferred pending county data.**

## Context

Close four engine gaps (**R**, **B+C+T**, **W**, **M**) and ship spec files for 13 state QSO parties: NM, MO, GA, ND, MI, ON, QC, NE, FL, 7QP, IN, DE, NEQP. Rules and county data are captured in `state-qso-party-spec-data.md` at project root.

**Constraint:** every engine change is additive — zero change to scoring output for the 7 existing specs (SS, ARRL DX, CQ-WW, NAQP, CWT, NS Sprint, MST). Phase 0 established the regression fence *before* any code change touched the scoring path. **The fence held across all 4 phases** — zero snapshot diffs on any baseline spec at any point.

## Execution outcome

| Phase | Deliverable | Specs shipped | Engine LOC (planned → actual) | Status |
|---|---|---|---|---|
| **0** | Regression test harness + NHQP spec | NHQP *(smoke test, not in the 13)* | — → 0 | ✅ complete |
| **1** | **R** (rover identity) | GA, ND, MI, IN, NEQP (5 of 13) | ~50 → ~60 (plus Session scope add-on, ~40) | ✅ complete |
| **2** | **B + C + T** (bonus rules primitive) | MO, QC, ON (3 of 13) | ~150 → ~180 | ✅ complete |
| **3** | **W** (score multiplier) | NM, DE, NE (3 of 13) | ~80 → ~70 | ✅ complete |
| **4** | **M** (multi-mult per QSO) | FL only (7QP pending county data) | ~40 → ~50 | ✅ complete |
| — | 7QP follow-up | 7QP | — | 🔲 deferred (external data missing) |

Each phase ended with `cargo test` green and byte-zero diffs on all existing-spec regression snapshots.

**Final counts:**
- **60 unit tests** (45 baseline + 15 added by Phases 1–4)
- **21 regression tests** (7 existing specs + `cqww_cw` legacy + NHQP smoke + 12 new specs)
- **~400 new production LOC** in `src/spec.rs`
- **0 modifications** to existing `specs/*.json` files
- **0 byte diffs** on existing-contest scoring output

---

## Phase 0 — Regression test harness ✅

**Goal (met):** lock in a golden-output fence around every existing spec so later phases cannot silently break them.

### Files created

- `tests/regression_existing_specs.rs` — integration test with one `#[test] fn regression_<id>()` per spec, a `run_regression()` helper, and a `UPDATE_SNAPSHOTS=1` escape hatch for intentional updates.
- `tests/fixtures/logs/<spec_id>.log.json` — 8 synthetic QSO logs (ss, arrl_dx, cqww, cqww_cw, naqp, cwt, ns_sprint, mst) hand-designed to exercise each spec's rules: all bands used by the spec, every point-rule variant, every multiplier variant, at least one intentional dupe.
- `tests/fixtures/snapshots/<spec_id>.score.json` — committed snapshots for each of the above plus NHQP smoke.
- `specs/nhqp.json` + `specs/domains/nh_counties.txt` + `specs/domains/nh_in_state_mults.txt` + `specs/domains/us_states.txt` + `specs/domains/ca_provinces.txt` — NHQP smoke-test spec and supporting domain files.

### Test harness shape

```rust
#[test] fn regression_ss()        { run_regression("ss"); }
#[test] fn regression_arrl_dx()   { run_regression("arrl_dx"); }
// ... one per spec ...

fn run_regression(spec_id: &str) {
    let log = load_log(spec_id);
    let actual = run_log_and_score(log);
    if std::env::var("UPDATE_SNAPSHOTS").is_ok() {
        write_snapshot(spec_id, &actual);
        return;
    }
    let expected = load_snapshot(spec_id).expect("snapshot missing");
    assert_eq!(actual, expected, "regression for {}", spec_id);
}
```

The snapshot struct is `{total_qsos, dupe_qsos, total_points, total_mults, claimed_score, points_by_band, mults}` — a `BTreeMap`-based format with deterministic ordering so the diff is meaningful.

Domain loading walks `specs/domains/*.txt` so new files added by Phases 1–4 are picked up automatically without harness changes.

### NHQP smoke test (Phase 0 bonus)

Added `specs/nhqp.json` as the first-ever new spec before any engine work. This validated:

1. Serde round-trip of a brand-new spec JSON file.
2. The embedded module integration path (`include_str!` + `SPEC_IDS` + `spec_by_id` + `standard_domain_pack`).
3. The asymmetric-mult-dimension pattern (NH stations global, non-NH per-band) using two multiplier definitions gated by `my_is_nh` predicate.
4. The new-spec workflow end-to-end, surfacing any gotchas in the spec format before Phase 1 also had engine-change variables in play.

**Note:** NHQP was initially shipped as CW-only because per-mode points (Phone 1, CW/Digital 2) couldn't be expressed without the Session scope feature. The Session scope was later added in Phase 1 as a prerequisite for GA/MI/NEQP per-mode scoring. NHQP's current spec remains CW-only; expanding it to all modes is a one-line point-rule change now that Session scope exists.

---

## Phase 1 — Rover identity (R) ✅

**Goal (met):** let `dupe_dimension` optionally include received exchange field values, so a mobile sending `INMAD` is not a dupe of the same call's earlier `INMRN` QSO on the same band/mode.

### Data model change

```rust
pub struct ContestSpec {
    // ...
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub dupe_extra_rcvd_fields: Vec<String>,
    // ...
}
```

### Engine change

Widened `dupes` HashSet from 4-tuple to 5-tuple:

```rust
// before:
dupes: HashSet<(Option<Band>, Option<Mode>, Option<i64>, Callsign)>,
// after:
dupes: HashSet<(Option<Band>, Option<Mode>, Option<i64>, Callsign, Option<String>)>,
```

Added helper `SpecEngine::dupe_extra_key(parsed) -> Option<String>` — returns `None` when `dupe_extra_rcvd_fields` is empty (all existing specs), making the fast path byte-identical to today. When non-empty, concatenates the listed received field values with U+001F unit separators.

Integrated into 3 call sites:
1. `apply_qso_at_with_mode` — computes extra key from parsed exchange.
2. `classify_candidate_at_with_mode` — same, after moving the exchange parse above the dupe check.
3. `classify_call_at_with_mode` — no exchange available, so when `dupe_extra_rcvd_fields` is non-empty falls back to `is_dupe: false` (the authoritative check happens at apply time with the real exchange).

### Side addition: Session scope

Added `Scope::Session` variant with a per-QSO environment exposing `band` and `mode` text values at predicate eval time. Needed so per-mode point rules (GA 1pt-SSB / 2pt-CW, same for MI / NEQP) could be expressed:

```json
"points": [
  {"when": {"Eq": [{"Field": {"scope": "SESSION", "key": "mode"}}, {"Value": {"Text": "SSB"}}]}, "value": 1},
  {"when": null, "value": 2}
]
```

Added `SessionEnv { band: Option<Band>, mode: Option<Mode> }` struct and threaded it through every `EvalContext` construction site. Exchange-parse paths use `SessionEnv::default()` (no band/mode context); QSO-apply paths populate both. Baseline specs don't use `Session` scope, so the regression fence stayed zero-diff.

### Tests added

3 unit tests in `src/spec.rs`:

- `rover_identity_disabled_preserves_classic_dupe_semantics` — empty list, same call+band+mode is still a dupe.
- `rover_identity_allows_rework_on_changed_exchange_field` — non-empty list, different received loc = not dupe; same loc repeated = dupe.
- `rover_identity_respects_dimension_scope` — rover identity composes correctly with Band/Mode/BandMode dupe_dimension.

### Spec files shipped

| Spec | Counties | Points rule | Score (test log) |
|---|---|---|---|
| `gaqp.json` | 159 (dedup'd from 160 raw) | SSB 1 / CW 2 via Session | 11 × 6 = **66** |
| `ndqp.json` | 53 | flat 1 all modes | 5 × 4 = **20** |
| `miqp.json` | 83 | SSB 1 / CW 2 via Session | 9 × 5 = **45** |
| `inqp.json` | 92 (5-letter `INxxx`) | flat 2 all modes (2026 rule) | 10 × 5 = **50** |
| `neqp.json` | 68 (5-letter state+cty across CT/MA/ME/NH/RI/VT) | SSB 1 / CW-Dig 2 via Session | 9 × 5 = **45** |

All 5 have `"dupe_extra_rcvd_fields": ["loc"]` and asymmetric county-vs-state/prov/DXCC multipliers gated by `my_is_<state>` config predicate. Each is in a synthetic log that exercises rover re-work (same mobile in two counties = two distinct QSOs) and a true dupe (same mobile in same county = dupe).

### Side deliverables

- `specs/domains/us_states.txt` (51 entries including DC) — reusable across all new state QP specs.
- `specs/domains/ca_provinces.txt` (13 entries) — same.
- `specs/domains/<spec>_counties.txt` for each new spec.
- All new specs registered in the `embedded` module (`SPEC_IDS`, `spec_by_id`, `standard_domain_pack`).

---

## Phase 2 — Bonus rules primitive (B + C + T) ✅

**Goal (met):** support post-mult additive bonus points and callsign-matched bonus stations. `PerMultiplierActivation { min_qsos }` was dropped from Phase 2 — it requires per-QSO source-location tracking that contest-engine doesn't yet support (see Parking Lot below), and the bonuses that would use it (ONQP mobile +300/area, QCQP mobile +300/region, NMQP mobile +5000/cty) are all mobile-side bonuses awarded to the operator for their own location, not to chasers.

### Data model change

```rust
pub struct ContestSpec {
    // ...
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub bonus_rules: Vec<BonusRule>,
    // ...
}

pub struct BonusRule {
    pub id: String,
    pub when: Option<Predicate>,
    pub amount: i64,
    pub scope: BonusScope,
}

pub enum BonusScope {
    Once,
    PerQso,
    PerBandMode,
}
```

### New predicate variant

```rust
pub enum Predicate {
    // ...
    DestCallIn(DomainRef),
}
```

Tests the destination callsign against a `List` or `External` domain (case-insensitive). The pure `eval_predicate` returns `false` for this variant because it has no domain provider; callers that need it (bonus rule evaluation, point-rule evaluation) route through a new `eval_predicate_with_domains` helper.

### Engine change

Added `BonusAccumulator` on `SpecEngine`:

```rust
struct BonusAccumulator {
    total: i64,
    fired_once: HashSet<String>,
    fired_per_band_mode: HashSet<(String, Band, Mode)>,
}
```

Accumulation is a free function (`SpecEngine::accumulate_bonuses`) taking explicit borrows `(&[BonusRule], &mut BonusAccumulator, &ctx, &dyn DomainProvider, band, mode)` instead of `&mut self` — the split-borrow shape keeps the `EvalContext`'s immutable reference to `self.config` valid during the call.

`claimed_score()` now returns `base + self.bonus.total`. Empty `bonus_rules` → `total` stays at 0 → score unchanged.

`compute_points` was also updated to thread `domains` through, so **conditional point rules can use `DestCallIn`**. This was needed for ONQP: bonus-station QSOs award 10 pts *instead of* the normal 2 pts (replacement, not addition), which is naturally a conditional point rule, not an additive bonus.

### Tests added

4 unit tests:

- `bonus_rules_empty_leaves_score_unchanged` — CWT without bonuses scores the same as before.
- `bonus_rule_once_with_dest_call_in_fires_one_time` — `Once`+`DestCallIn` fires exactly once even across multiple band/mode combinations.
- `bonus_rule_per_qso_with_dest_call_in_fires_each_time` — `PerQso` accumulates per matching QSO.
- `bonus_rule_per_band_mode_fires_once_per_band_mode` — `PerBandMode` respects band+mode boundaries.

### Spec files shipped

| Spec | Counties / Areas | Bonus model | Score (test log) |
|---|---|---|---|
| `moqp.json` | 115 | W0MA +100 once + K0GQ +100 once (two distinct `Once` rules, each with its own `DestCallIn.List`) | 15 × 6 + 200 = **290** |
| `qcqp.json` | 17 regions | none (2026+) — pure Phase 1 rover identity | 9 × 4 = **36** |
| `onqp.json` | 50 mult areas | 5 bonus stations (VA3CCO/VE3CCO/VE3ODX/VE3RHQ/VA3RAC) scored via conditional point rule (10 pts *instead of* 2) | 30 × 6 = **180** |

### Side deliverables

- `specs/domains/moqp_bonus_stations.txt` (2 calls)
- `specs/domains/on_multiplier_areas.txt` (50 codes)
- `specs/domains/onqp_bonus_stations.txt` (5 calls)
- `specs/domains/mo_counties.txt` (115), `specs/domains/qc_regions.txt` (17)
- All registered in `embedded` module.

### Known limitation

**ONQP mobile +300/area activation bonus is NOT implemented.** Rules reward the mobile operator for activating each multiplier area with 3+ QSOs. Expressing this requires the engine to track which area the operator was operating from at each QSO — a per-QSO source-location concept that contest-engine doesn't have. Noted in the spec's log fixture description. Scoring remains correct for non-mobile operators.

---

## Phase 3 — Score multiplier (W) ✅

**Goal (met):** support power-based whole-score multipliers between `(points × mults)` and `+ bonuses`.

### Data model change

```rust
pub struct ContestSpec {
    // ...
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub score_multiplier: Option<ScoreMultiplier>,
    // ...
}

pub struct ScoreMultiplier {
    pub from_config: String,       // e.g. "my_power_class"
    pub mapping: BTreeMap<String, u32>,
    #[serde(default = "score_multiplier_default")]
    pub default: u32,              // fallback when config value not in mapping
}
```

### Scoring change

```rust
pub fn claimed_score(&self) -> i64 {
    let base = /* existing formula branches */;
    let scaled = base * self.score_multiplier_factor() as i64;
    scaled + self.bonus.total
}
```

`score_multiplier_factor()` returns 1 when `score_multiplier` is `None`, preserving byte-identical output for every baseline spec. Lookups against the mapping are case-insensitive.

### Tests added

4 unit tests:

- `score_multiplier_none_leaves_score_unchanged` — empty = no change.
- `score_multiplier_scales_by_power_class_lookup` — QRP ×5, Low ×2, High ×1 with identical base scores.
- `score_multiplier_missing_config_uses_default` — operator with no `my_power_class` falls through to `default`.
- `score_multiplier_composes_with_bonus_additively` — verifies the composition `(base × power) + bonus`, not `base × (power + bonus)` or any other variant.

### Spec files shipped

| Spec | Counties | Points rule | Power mapping | Bonuses | Score (test log) |
|---|---|---|---|---|---|
| `deqp.json` | 3 (NDE/KDE/SDE) | out-of-state Ph 10 / CW 20 / Dig 20 (10× built-in) via conditional point rules | HP 1 / LP 2 / QRP 3 | none | 90 × 4 × 2 = **720** |
| `nmqp.json` | 33 | Ph 1 / CW-Dig 2 via Session | QRP 5 / LP 2 / HP 1 | W1AW/5 +250 once | 13 × 5 × 5 + 250 = **575** |
| `neqsop.json` | 93 | Ph 1 / CW 3 / Dig 1 via Session | QRP 5 / LP 2 / HP 1 | NE0QP +100 once | 16 × 5 × 2 + 100 = **260** |

All three use rover identity + Session-scope points + `score_multiplier`, and NMQP/NEQSOP also use `bonus_rules` — exercising every Phase 1/2/3 feature stacked together.

### Known limitations (documented in each spec's log fixture)

- **NMQP:** Mobile "+5000 per county activated (15 QSO threshold)" is a mobile-side threshold bonus. Deferred pending per-QSO source-location support.
- **NEQSOP:** The "rare grid ×5 points" rule, satellite/mobile point-row categories, and year-variable Section Manager and NE Appointee bonus callsigns are out of scope for this first cut. The domain file seeds only `NE0QP`; year-specific calls can be added when known.
- **DEQP:** The `+50` electronic-submission bonus is metadata (awarded for submitting in Cabrillo format), not a scoring-time feature. Explicitly excluded from the spec.

---

## Phase 4 — Multi-mult per QSO (M) ✅

**Goal (met):** let one received exchange field split into multiple multiplier rows, so a county-line mobile sending `ALC/BAK` in a single QSO yields two multiplier credits while only counting as one QSO for dupe/total purposes.

### Data model change

```rust
pub struct ExchangeField {
    // ...
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub multi_value_sep: Option<String>,
}
```

### Engine changes

1. **Parse-time domain check skipped** when `multi_value_sep` is set — the whole-string value like `ALC/BAK` won't be in the domain anyway. Per-piece validation happens in the mult loop.

2. **`multi_value_sep_for_key` helper** scans the spec's received variants for the field id referenced by a multiplier variant's `KeyExpr::Field(Rcvd)`. Returns the separator (if any) or `None` for every direct-Rcvd reference without a separator declared, and always `None` for `Concat` or non-Rcvd keys.

3. **Multiplier loop iterates split values:**

   ```rust
   let raw_value = eval_key_expr(&ctx, &variant.key);
   let values: Vec<String> = match self.multi_value_sep_for_key(&variant.key) {
       Some(sep) if !sep.is_empty() => raw_value.split(&sep).map(trim).filter(non_empty).collect(),
       _ => vec![raw_value],
   };
   for value in values {
       if !validate_value_in_domain(&value, &variant.domain, domains) { continue; }
       // insert into mult set, push to new_mults
   }
   ```

   Each split piece runs through domain validation independently. The QSO itself still counts as one for `total_qsos` and dupe purposes; only the multiplier-emission count changes.

### Tests added

3 unit tests:

- `multi_value_sep_none_preserves_single_value_behavior` — explicitly asserts that every baseline spec has `multi_value_sep: None` on every received field, and that NAQP with no separator still produces exactly one mult row per QSO.
- `multi_value_sep_slash_splits_one_qso_into_two_mults` — opts NAQP's `loc` field into `/`-split, sends `NH/ON`, asserts two distinct mult rows from one QSO with `total_qsos == 1`.
- `multi_value_sep_skips_unknown_values_and_keeps_valid` — sends `NH/ZZ`; `NH` is in the domain, `ZZ` isn't; result is 1 mult credit (NH only), 1 QSO, no error.

### Spec files shipped

| Spec | Counties | Points rule | Multi-sep | Score (test log) |
|---|---|---|---|---|
| `flqp.json` | 67 | Ph 1 / CW 2 via Session | `/` on the `loc` field | 11 × 7 = **77** |

FLQP's test log includes a county-line mobile sending `ALC/BAK` — one QSO entry producing two county multipliers. All three Phase 4 test assertions pass on this input.

### 7QP — deferred

7QP was in the Phase 4 plan but could not be shipped:

1. **County list unavailable.** `ws7n.net/7QP/new/page.asp?content=counties` returns an IIS configuration error, and no mirror was found. The 259-code state-prefixed list would need to be sourced from N1MM+ section data, reconstructed from historical Cabrillo logs, or built from per-state county lists plus 7QP's own 3-letter convention.

2. **State-prefix-sharing quirk.** 7QP county-line exchanges reuse the state prefix, e.g., `ORDES/JEF` means Oregon-Deschutes plus Oregon-Jefferson — the second county's state prefix is implied from the first. Plain `/`-split produces `ORDES` and `JEF`, where `JEF` alone isn't a valid 7QP code. 7QP needs a small additional preprocessing step (or a more expressive split primitive: "prepend the first N characters of the first split piece to every subsequent piece") before its spec can use multi-value splitting correctly.

Neither blocker is a Phase 4 engine gap — they're 7QP-specific data and parsing concerns. Both are solvable; tracked in the parking lot.

---

## Regression fence (applied at every phase)

Every commit in Phases 1–4 passed:

1. No modifications to any file under `specs/` except adding new files.
2. No modifications to any snapshot under `tests/fixtures/snapshots/` for an existing spec. If a snapshot for SS/ARRL-DX/CQWW/NAQP/CWT/NS-Sprint/MST changed, the PR was broken.
3. `cargo test --test regression_existing_specs` stayed green and zero-diff end-to-end.
4. Each phase added at least one unit test exercising the new feature against a synthetic spec, plus at least one "with the feature absent, behavior is identical" assertion.
5. `UPDATE_SNAPSHOTS=1 cargo test` was only used to capture snapshots for NEW specs (never to update existing snapshots).

## Final file-modification summary

| File | Phase 0 | Phase 1 | Phase 2 | Phase 3 | Phase 4 |
|---|---|---|---|---|---|
| `src/spec.rs` | — | ~100 (R + Session) | ~180 (bonus rules + DestCallIn + compute_points rewire) | ~70 (score mult) | ~50 (multi_value_sep) |
| `tests/regression_existing_specs.rs` | **NEW** ~400 LOC | append 5 | append 3 | append 3 | append 1 |
| `tests/fixtures/logs/*.json` | +8 | +5 | +3 | +3 | +1 |
| `tests/fixtures/snapshots/*.json` | +8 | +5 | +3 | +3 | +1 |
| `specs/*.json` (new only) | +1 (nhqp) | +5 | +3 | +3 | +1 |
| `specs/domains/*.txt` (new only) | +4 (nh_counties, nh_in_state_mults, us_states, ca_provinces) | +5 (ga, nd, mi, in, neqp counties) | +5 (mo counties, moqp bonus, qc regions, on areas, onqp bonus) | +4 (de, nm, ne counties, neqsop bonus) | +1 (fl counties) |
| `specs/*.json` (existing) | **untouched** | **untouched** | **untouched** | **untouched** | **untouched** |

Totals after Phase 4: **13 new specs** (NHQP smoke + 12 of 13 target batch), **~400 new LOC in `src/spec.rs`**, zero byte-changes to existing specs, full regression coverage.

## Per-spec scoring verification (test logs)

| Spec | Total QSOs | Dupes | Points | Mults | Score | Composition |
|---|---|---|---|---|---|---|
| nhqp (CW only) | 5 | 1 | 10 | 5 | **50** | points × mults |
| gaqp | 6 | 1 | 11 | 6 | **66** | points × mults |
| ndqp | 5 | 1 | 5 | 4 | **20** | points × mults |
| miqp | 5 | 1 | 9 | 5 | **45** | points × mults |
| inqp | 5 | 1 | 10 | 5 | **50** | points × mults |
| neqp | 5 | 1 | 9 | 5 | **45** | points × mults |
| moqp | 8 | 1 | 15 | 6 | **290** | (15 × 6) + 100 + 100 |
| qcqp | 5 | 1 | 9 | 4 | **36** | points × mults |
| onqp | 7 | 1 | 30 | 6 | **180** | points × mults (10pt bonus-stn via conditional point rule) |
| deqp | 5 | 1 | 90 | 4 | **720** | (90 × 4) × 2 (LP) |
| nmqp | 7 | 1 | 13 | 5 | **575** | ((13 × 5) × 5 (QRP)) + 250 |
| neqsop | 6 | 1 | 16 | 5 | **260** | ((16 × 5) × 2 (LP)) + 100 |
| flqp | 6 | 1 | 11 | 7 | **77** | points × mults (1 QSO = 2 mults via `/` split) |

All hand-calculated totals match. The scoring path is deterministic across runs and byte-locked in the regression snapshots.

## Critical files to read before modifying anything in this area

- `src/spec.rs` end-to-end — especially `apply_qso_at_with_mode` (~line 1345), `scope_key` / `dupe_extra_key` / `multi_value_sep_for_key` / `accumulate_bonuses` / `score_multiplier_factor` on `SpecEngine` (~line 1280–1320), `claimed_score` (~line 1920), `compute_points` (~line 2000).
- `specs/arrl_dx.json` — asymmetric in/out behavior via `when`-gated multiplier variants.
- `specs/cqww.json` — conditional point rules based on station fields.
- `specs/ss.json` — simple baseline spec with config fields.
- `specs/gaqp.json` / `specs/flqp.json` — the cleanest examples of every Phase 1/4 feature stacked.
- `specs/nmqp.json` / `specs/neqsop.json` — the cleanest examples of every Phase 1/2/3 feature stacked.

## Parking lot (not shipped in this batch)

### Still missing for this 13-party batch

- **7QP county list sourcing** — blocked on external data. The rules are captured; only the 259-code state+county list and the state-prefix-sharing split preprocessing step remain.

### Documented but intentionally out of scope for this batch

- **Mobile-side activation bonuses** (ONQP +300/area, QCQP +300/region, NMQP +5000/county, TNQP +500/county, PAQP +500/county, etc.) — all require the engine to track which location the operator was in at each QSO. No primitive for per-QSO source-location variation exists today. Affects 6–8 parties across the full 47; none are currently blocked on this alone (each has other features shipping correctly).

- **NHQP full multi-mode** — the spec is CW-only today. Expanding to `CW`, `SSB`, `DIGITAL` is a one-line change to `allowed_modes` plus a Session-scope point rule, both of which already exist. Held back only to avoid a mid-phase snapshot regeneration; trivial follow-up.

- **NE year-variable bonus callsigns** — NE0QP is in the domain; Section Manager and NE Appointee calls change yearly and aren't captured. Fixable by editing the `specs/domains/neqsop_bonus_stations.txt` file each year.

- **DE electronic-submission +50 bonus** — metadata-driven, not in-QSO. Intentionally excluded.

- **Grid-as-multiplier** (NEQSOP rare grids ×5, Vermont grid mults ÷3) — can be modeled as conditional point rules matching specific received-field values, but requires the exchange to carry the grid in a dedicated field. Handled ad hoc for each party that needs it; no general engine change.

- **NC weighted rare-county mults** (×2), **NJ power overlay**, **NC 5-of-10 sweep bonus**, **MS per-county mobile score aggregation** — small niche features for individual parties in the broader 47-party set. None in the current 13-party batch. Deferred.

## Sources of truth (cross-references)

- **This file** — historical plan and completed-status summary.
- **`state-qso-party-survey-plan.md`** — the original 47-party survey and blocker matrix.
- **`state-qso-party-spec-data.md`** — rules text, county/bonus-station/region lists, and expressibility notes for the 13-party batch.
- **`specs/<id>.json`** — the authoritative spec files.
- **`tests/fixtures/snapshots/<id>.score.json`** — byte-locked score snapshots. Any change to any of these outside a reviewed update is a regression.
- **`src/spec.rs`** — the engine code. Authoritative for what the scoring actually does.
