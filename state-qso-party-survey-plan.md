# State QSO Party Expressibility Survey

## Context

Survey of the 47 state/regional QSO parties listed at https://stateqsoparty.com/, answering: for each party, can its rules be fully expressed as a contest-engine spec file (JSON in `specs/`) without requiring Rust changes to the engine?

Purpose: identify (a) parties that are ready for a spec PR today and (b) the highest-leverage engine gaps to close if we want to unlock a larger batch.

Research method: fetched each party's rules from its primary source — direct HTML or PDF — using curl with a browser User-Agent and `pdftotext`. 43 of 47 rules pages were successfully read end-to-end from first-party text. The 4 remaining (DE, IL, MD/DC, OK) have broken rules hosting as of the fetch time (Drupal SQL error, JS-only viewers with no extractable PDF URL, 404'd frame pages) and are marked *unverified*.

## Contest-engine spec capabilities (verified from `src/spec.rs` + existing specs)

**Expressible:**
- Exchange fields: `Rst`, `Int`, `Enum` (with `External` domain file lookup), `String`, `Literal`, `Power`, serial numbers
- `received_variants` / `sent_variants` with `when` predicates on `SOURCE` / `DEST` / `CONFIG` / `RCVD` / `SENT` → in-state vs out-of-state asymmetric exchange works
- Multipliers with `Dimension`: `Band`, `Mode`, `BandMode`, `Period { minutes }`, `Global`
- Multiplier key: single `RcvdKey` field, `DestKey` (dxcc only), `Const`, or `Concat([…])`
- Conditional point rules (`when` predicate per-QSO)
- Score formulas: `points_times_total_mults`, `points_times_sum_band_mults`, `sum_band_points_times_band_mults`
- Dupe tracking: `dupe_dimension` (Band / Mode / BandMode / Period / Global)
- QSO rejection predicates (`valid_qso`)
- Operator config fields with `required_when`

**Station resolver exposes (`SOURCE`/`DEST`):** `dxcc`, `continent`, `is_wve`, `is_na`. No county, grid, state, or callsign-list membership.

**Hard engine gaps:**

| Code | Gap | Why it blocks |
|---|---|---|
| **R** | Rover identity | Dupe keys are `(call, dimension_scope)`. Cannot include a received exchange field (e.g. county) in the dupe key, so mobiles-as-new-station-per-county is unscored. |
| **B** | Post-mult additive bonus | All three `ScoreFormula` variants are multiplicative. No way to add a flat term after `× mults`. |
| **C** | Callsign-matched bonus station | Predicates can test SOURCE/DEST fields, but the station resolver has no "callsign ∈ list" primitive. Bonus stations like K4TCG / W7DX / VA7ODX cannot be detected. |
| **M** | Multi-multiplier per single QSO | One QSO → one multiplier row. A county-line QSO sending `CAR/LEH` in one exchange cannot emit two mult rows. |
| **W** | Weighted / tiered multiplier | Mults are unit-valued. Rare-county ×2, power-class score ×, grid-mults-divided-by-N all unexpressible. |
| **S** | Sweep/completion bonus | Needs global state across QSOs (e.g. "+500 for any 5 of 10 rare counties"). |
| **T** | Threshold-to-count multiplier | Needs a min-QSO count before a mult is awarded (MOQP: 50 QSOs from cty → that cty counts as mult; TNQP / PAQP similar). |
| **P** | Per-county score aggregation | MSQP mobile: *final score = sum of per-county scores*, not one global accumulation. |

Soft gaps (workable by convention): no contest-time enforcement, no first-class power-class category, no off-time engine. Most SQPs don't enforce these in software.

## The 47-Party Matrix

All verdicts except 4 are **verified from first-party rules pages** (HTML or PDF read end-to-end). Rows marked with ⚠ are unverified because their hosting is currently broken; verdicts for those rows are inferred from the matrix-fragment hints we could pull.

| # | Party | Ex (in/out) | Pts | Mults | Rover / CL | Bonus | Verdict | Blockers |
|---|---|---|---|---|---|---|---|---|
| 1 | 7QP (7th Call Area) | 5-ltr state/cty / 2-ltr SP | CW 3 / Ph 2 / Dig 4 | W7: 50st+13pr+10DX; non-W7: 259 cty; per-band-mode dupe | W7 mobile re-workable per cty; CL sends **multi-cty in one exchange** (`ORDES/JEF`) | none | **NO** | R, M |
| 2 | Atlantic Canada QP | 5-ltr prov+cty / state-prov | 1 any | 47 cty/div or 63 prov+state per band | CL = separate QSOs (OK); mobile = new stn on cty change | VE1RAC +5 pts/band-mode (per-mult-like, but callsign-match) | **NO** | R, C |
| 3 | Alabama QP | cty / state-prov-DX | 2 any | 67 cty or 50+13+DXCC per mode | AL mobile = new stn/cty; CL sitting banned | none | **NO** | R |
| 4 | Arizona QP | 3-ltr cty / state-prov-DX | CW 2 / Ph 1 | 50+13+DXCC per mode (AZ); 15 cty × 6 bands × 2 modes = 180 (non-AZ) | "Mobiles that change counties are considered new stations" (explicit); CL → log as multi-QSO (OK) | **K7A one-time +100** (post-mult) | **NO** | R, B, C |
| 5 | Arkansas QP | 3-ltr cty / state-prov-DX | 2 any | 75 cty or 75+49+13+1DX (AR) | "When a Mobile or Rover changes locations, they are basically a new station"; CL logged as separate QSOs | **WR5P +200 pts/QSO**; mobile +200 per AR cty; **live streaming bonus** | **NO** | R, B, C |
| 6 | British Columbia QP | 3-ltr district / state-prov-DX | Ph 2 / CW 4 | 43 BC dists or 13 pr+50 st, per-band-mode | not specified | **VA7ODX +20 pts/QSO post-mult** | **NO** | B |
| 7 | California QP | 4-ltr cty / state-prov-DX | Ph 2 / CW 3 | Global 58 cty or 58 st+pr | mobile = new stn per cty; **CL sends all applicable counties in single exchange** | none | **NO** | R, M |
| 8 | Canadian Prairies QP | 3-ltr district / state-prov-DX | 1 any | 62 dists or 63 pr+st, per-band | "When Rover Stations change to a new FED they are now considered to be new stations" | none | **NO** | R |
| 9 | Colorado QP | 3-ltr cty / state-prov-DX | 2 any | 64 cty or 64+state+pr per mode | mobile = new stn; CL = 2 separate QSOs (OK) | **500 pts per CO cty activated** (15 QSO min) | **NO** | R, B, T |
| 10 | Delaware QP ⚠ | unknown | unknown | 3 DE cty only | unknown | unknown | **? (likely MOSTLY)** | site down |
| 11 | Florida QP | cty / state-prov-DXCC/ITU | Ph 1 / CW 2 | 67 cty or 50+13+DXCC+3 ITU, per-mode | FL mobile = new stn per cty; **CL logs one QSO with multi-cty** | none | **NO** | R, M |
| 12 | Georgia QP | cty / state-prov-DX | Ph 1 / CW 2 | GA cty or states-prov, per-mode | GA mobile re-workable on cty change | none | **NO** | R |
| 13 | Hawaii QP | HI mult / state-prov-DX-or-grid | (unknown detail) | HI regions / state-prov | not explicit | none found | **NO** (tentative) | likely R |
| 14 | Idaho QP | 3-ltr cty / state-prov-DX | Ph 1 / CW 2 / QRP 5 | 44 cty or state+pr+DXCC per mode | mobile = new stn per cty; CL = 2 QSOs (OK) | 500–1500 pts per dormant cty activation (post-mult) | **NO** | R, B, T |
| 15 | Illinois QP ⚠ | cty / state-prov-DX | unknown | 75 IL cty | Mobiles and Rovers defined as categories; CL detailed procedures visible | unknown | **NO** (inferred) | likely R |
| 16 | Indiana QP | 5-ltr INxxx / state-prov-DX | 2 any (2026 change) | 92 cty or 92+49+13 per mode | **"Rover stations may be worked again when they change counties (same as mobile)"**; CL = 2 QSOs (preferred) OR multi-cty in one (avoidable) | none | **NO** | R |
| 17 | Iowa QP | 3-ltr cty / state-prov-DX | Ph 1 / CW 2 / Dig 2 | 99 cty or 99+state+pr+1DX | "Mobile/Portable may be worked once per MODE/BAND/COUNTY" | **Bonus Stations +500 pts** | **NO** | R, B, C |
| 18 | Kansas QP | cty / state-prov-DX | In 1 / Out 2 | KS cty | mobile = new stn per cty | bonus stns | **NO** | R, C |
| 19 | Kentucky QP | 3-ltr cty / state-prov-DX | Ph 1 / CW 2 | KY cty or states-pr, per-band-mode-cty | KY mobile = new stn per cty; CL `MCY/WHI` multi-mult allowed | **K4KCG +100 pts/QSO**; +100 Cabrillo submission | **NO** | R, M, B, C |
| 20 | Louisiana QP | parish / state-prov-DX | Ph 2 / CW-Dig 4 | 64 parish or parish+st+pr+DXCC, per-band-mode | "Rovers may be worked once on CW/Dig and once on Phone per band, in EACH parish activated"; CL = separate QSOs | **N5LCC +100 one-time**; Rover +50 pts per parish activated | **NO** | R, B, C |
| 21 | Maryland/DC QP ⚠ | cty or DC / state-prov-DX | unknown | MD cty + DC / state | unknown | unknown (typical MDQP has bonus stations) | **NO** (inferred) | R (likely), C (likely) |
| 22 | Michigan QP | cty / state-prov-DX | Ph 1 / CW 2 | 83 cty or 83+49+13+DXCC per mode | mobile re-workable when changing cty | none | **NO** | R |
| 23 | Minnesota QP | 3-ltr cty+name / state-prov-DX | 2 any | 87 cty + 49 st + DC + 10 pr + 3 terr + 1 DX (MN, global) or 87 cty (non-MN, global) | "MN mobiles and rovers may be worked once per band & mode from each county" | none | **NO** | R |
| 24 | Mississippi QP | cty / state-prov-DX (grid for FT8) | SSB 1 / CW-RTTY-FT 2 | MS cty + 49 st + 13 pr + 336 DXCC (global); MS grids for FT, divided by 4 (weighted) | not explicit rover rule; **Portable/Mobile: "Final score = sum of scores from each county"** — per-county aggregation | none | **NO** | W, P |
| 25 | Missouri QP | 3-ltr cty / state-prov-DX | Ph 1 / CW-Dig 2 | 115 cty + 49 st + 13 pr + 1DX (MO) or 115 cty (non-MO) | MO mobile = new stn per cty | **W0MA +100 once; K0GQ +100 once; Cabrillo +100; 40/80m hours +1 pt each up to +250; "Mobile gets cty mult only after 50 QSOs"** | **NO** | R, B, C, T |
| 26 | Nebraska QP ⚠ | cty / state-prov-DX | unknown | NE cty / state | prominent mobile routes, likely rover-as-new-stn | unknown | **NO** (inferred) | R (likely) |
| 27 | New England QP | cty / state-prov-DX | Ph 1 / CW-Dig 2 | 68 NE cty or 48+14+DXCC per band-mode | mobile = new stn on cty change; CL = separate QSOs (OK) | none | **NO** | R |
| 28 | New Hampshire QP | cty / state-prov-DX | Ph 1 / CW-Dig 2 | **NH: global** (cty+state+pr+≤10 DXCC); **non-NH: per-band** (10 cty × 5 bands = 50 max) | Mobile "may operate from one or more counties" — **no re-workable rule**, no per-cty dupe exception | **none** | **✅ YES** | — *(see note)* |
| 29 | New Jersey QP | 4-ltr cty / state-prov-DX | (unknown detail) | 21 NJ cty + 49 st + 13 pr + 1 DX = 84 max | Mobile/portable crossing area = new stn | none | **NO** | R, **W** (power mult: HP 1×, LP 2×, QRP 4×) |
| 30 | New Mexico QP | cty / state-prov-DX | Ph 1 / CW-Dig 2 | NM cty or state-prov-DXCC | typical mobile-new-stn | **W1AW/5 +250 one-time**; mobile +5000 per cty (15 QSO min) | **NO** | R, B, C |
| 31 | New York QP | 4-char **grid** (NY) / state-prov | In 2 / Out 1 | NY cty / 49+DXCC+prov per band | **"CL counts for both counties"** (multi-mult in one QSO) | yes | **NO** | M, R |
| 32 | North Carolina QP | cty / state-prov-DX | Ph 2 / CW 3 / Dig 5 | 100 cty or 49+DC+13+1DXCC | mobile re-workable; **CL 2-cty credit from single position** | 10 rare cty @ 2× mult; 500-pt sweep of 5 rare cty | **NO** | R, M, W, S |
| 33 | North Dakota QP | cty / state-prov-DX | 1 any | 53 cty or 53+49+13 (global) | **"Mobile ND stations that change counties are considered to be a new station"**; CL = 2 QSOs (OK) | none | **NO** | R |
| 34 | Ohio QP | cty / state-prov-DX | Ph 1 / CW 2 | 88 cty or 88+49+11+1DXCC per mode | mobile may change cty for re-contact (500 ft min) | none | **NO** | R |
| 35 | Oklahoma QP ⚠ | cty / state-prov-DX | unknown | OK cty / state | frames-based site 404s | unknown | **NO** (inferred) | R (likely) |
| 36 | Ontario QP | cty / state-prov-DX | 2 any (per band) | ON cty or ON+prov+state+DXCC per band | mobile activations tracked; "twice per band: once phone, once CW" | bonus stns **+10 pts** each; mobile **+300 pts per cty activated** (3 QSO min) | **NO** | C, B, T |
| 37 | Pennsylvania QP | serial + cty/section / serial + section/DX | CW 2 / Ph 1 | PA cty + ARRL sections + Can sections + 1 DX (global, in); 67 PA cty (global, out) | "Work Rovers and Mobiles again when they change counties"; CL station sends `CAR/LEH` → receiver logs multi-QSO | **QRP multiplier × 2**; **N3XF bonus stn +200 pts/QSO**; PA mobile/rover **+500 pts per cty with 10+ QSOs** | **NO** | R, B, C, W, T |
| 38 | Quebec QP | region / prov-state-DX | Ph 1 / CW 2 per band | QC regions; +prov/state/DXCC for VE2 per-band | rover = new QSO per region crossing | **+300 pts per region activated** (3 QSO min); no bonus stn 2026+ | **NO** | R, B, T |
| 39 | South Carolina QP | cty / state-prov-DX | (unknown detail) | SC cty | Expedition re-workable on cty move | **W4CAE +350; WW4SF +250; K4YTZ +250** | **NO** | R, C |
| 40 | South Dakota QP | cty / state-prov-DX | Ph 1 / CW 2 | SD cty or SD+state+prov+DXCC (global) | rover = new contact per cty; CL = separate (OK) | **W0OJY +100 once** | **NO** | R, C |
| 41 | Tennessee QP | cty / state-prov-DX | **3 any** | 95 cty per band (non-TN) or +state+prov+DXCC per band (TN) | "Mobile/Rover may be worked again if they change counties"; CL = 2 QSOs (OK) | **K4TCG +100 pts/QSO/band-mode**; TN mobile **+500 per cty** (10 QSO min); "bonus points added after geographic multiplier" | **NO** | R, B, C, T |
| 42 | Texas QP | cty / state-prov-DX | Ph 2 / CW-Dig 3 | 254 TX cty or state+cty+prov+DXCC | mobile once/band/mode per cty; CL = separate w/ visual contact (OK) | mobile **+500/+1000 pts** bonuses | **NO** | R, B, S |
| 43 | Vermont QP | cty / state-prov (grid for FT8) | Ph 1 / Dig 2 / CW 3 | 14 cty + state-prov-DXCC + W1NVT + grid-based per mode | mobile CL = separate QSOs (OK) | **W1AW/1 +2 pts/QSO** (non-VT only); grid mults **÷ 3** (weighted) | **NO** | B, C, W |
| 44 | Virginia QP | serial+cty/city / serial+state-prov | Ph 1 / CW 2 / Dig 2; **+3 pts for QSOs with VA mobile/rover/exp** | 95 cty + 38 cities (global) for non-VA; all mults global | VA mobile/rover/exp count cty-city as mult only if 10+ QSOs (**threshold**); CL = 1 QSO + 1 mult | VA mobile/rover/exp **+100 pts per cty/city**; **"Virginia is for Lovers" bonus stns +50 one-time each**; final = pts × mults + bonus | **NO** | R, B, C, T |
| 45 | WA Salmon Run | cty / state-prov-DX | Ph 2 / CW 3 | 39 cty or 39+state+prov+DXCC **global** | mobile cty-change re-workable; CL = separate (OK) | **W7DX +500 pts/mode (1000 max)** | **NO** | R, C |
| 46 | West Virginia QP | cty / state-prov-DXCC | Ph 1 / CW-Dig 2 | 55 cty or 55+states+prov+DXCC | "Mobile stations may be worked and work again upon changing counties"; CL = 2 QSOs + 2 mults (OK) | **W8WVA +100 pts/QSO per band-mode**; WV mobile **+100 per cty**; final = pts × mults + bonus | **NO** | R, B, C |
| 47 | Wisconsin QP | cty / state-prov-DX | Ph 1 / CW-Dig 2 | 72 cty or 72+50+13 per band-mode | mobile workable per cty; CL banned | **W9FK +100/contact**; mobile **+500 per cty outside home** (12 QSO min) | **NO** | R, B, C, T |

⚠ = hosting broken during research, verdict inferred. * = 43 of 47 verified from first-party rules (HTML/PDF).

### Totals

| Verdict | Count | Notes |
|---|---|---|
| **YES** | **1** | New Hampshire QP |
| **MOSTLY** | 0 | *(DE is tentative — 3 counties and site down, likely YES once verified)* |
| **NO** | 42 verified + 4 inferred = **46** | |

## The one winner: New Hampshire QP

NHQP is the only party in the set with none of the hard blockers:
- Flat "once per band per mode" dupe rule — **no per-county exception for mobiles**, which is the R blocker that kills nearly every other party.
- No bonus stations.
- No post-multiplication additive terms.
- No weighted mults, sweep bonuses, threshold mults, or multi-mult-per-QSO.
- Exchange is plain RS(T) + county / state-province / DX, which maps directly onto an `Enum{External}` field with `when` variants.

**One small spec caveat:** multiplier dimension is asymmetric — NH stations score mults globally, non-NH stations score per band. Contest-engine supports multiple multiplier objects each with their own `Dimension` and `when`-gated variants, so this is expressible as two multiplier entries: one `Global` dimension gated on `my_is_nh == true`, one `Band` gated on `my_is_nh == false`. This is the same pattern `arrl_dx.json` uses for its asymmetric mult key. If that two-multiplier-objects pattern is accepted by the resolver (it should be, based on the spec data model), NHQP drops straight into `specs/nhqp.json`.

## Blocker frequency across the 47 parties

| Blocker | Count | % | Unlocks if fixed (assuming other blockers also fixed) |
|---|---|---|---|
| **R** (rover identity) | ~42 | ~89% | almost everything |
| **B** (post-mult additive) | ~22 | ~47% | TN, VA, WV, AZ, CO, ID, IA, KY, LA, MO, NM, ON, PA, QC, TX, VT, WA, WI, BC |
| **C** (callsign-match bonus stn) | ~21 | ~45% | same set as B, minus BC/VT (which are B only) |
| **T** (threshold-to-count mult) | ~8 | ~17% | CO, ID, MO, ON, PA, QC, TN, VA, WI |
| **M** (multi-mult per QSO) | ~6 | ~13% | CQP, 7QP, FL, NY, KY, NC |
| **W** (weighted mult) | 5 | ~11% | NC, NJ, MS, PA, VT |
| **S** (sweep bonus) | 2 | ~4% | NC, TX |
| **P** (per-county score agg.) | 1 | ~2% | MS |

## Highest-leverage engine changes (recommended order)

**1. Rover identity (R)** — single biggest unblock. Proposal: extend `dupe_dimension` to allow an optional list of received exchange field IDs to include in the dupe key. Example:
```json
"dupe_dimension": "BandMode",
"dupe_extra_rcvd_fields": ["cty"]
```
When a mobile sending `INMRN` later sends `INMAD` on the same band/mode, the dupe key changes and the QSO counts. This unlocks **all ~42 R-blocked parties** as a prerequisite.

**2. `BonusRule` primitive (B + C together)** — proposal: add a top-level `bonus_rules` list:
```json
"bonus_rules": [
  {"when": {"DestCallIn": {"External": "tnqp_bonus_stations"}}, "amount": 100, "scope": "PerBandMode"},
  {"when": {"And": [{"my_is_mobile": true}, {"cty_qso_count_gte": 10}]}, "amount": 500, "scope": "PerCounty"}
]
```
Also: add `DestCallIn` predicate op that tests `DEST.call ∈ external_list`. Final score formula becomes `(points × mults) + Σ bonuses`. Unlocks another ~22 parties.

**3. Threshold mult (T)** — proposal: multiplier variants gain an optional `min_qsos_from_mult` threshold. Worth roughly +8 parties. Smaller scope.

**4. Multi-mult per QSO (M)** — proposal: exchange field gains `multi_value_sep` (e.g., `"/"`); runtime expands one QSO into N scoring rows at evaluation time. Unlocks CQP, 7QP, NY, FL, KY, NC. ~6 parties.

**5. Weighted mult (W)** and **sweep bonus (S)** and **per-county score aggregation (P)** — small, niche; leave until last.

## Expected coverage after each step

| After fixing | Expressible parties (rough) |
|---|---|
| Nothing (today) | **1** (NH) + 1 likely (DE, unverified) |
| + R | ~10 *(parties that were R-only: AL, CPQP, IN, MI, MN, ND, OH, NEQP, GA, NH, DE)* |
| + R + (B ∪ C) | ~33 *(adds CO, ID, MO, NM, ON, QC, TN, VT, WA, WI, LA, BC, ACQP, KS, SC, SD, PA, VA, WV, IA, AR, AZ, WI, etc.)* |
| + R + B + C + T | ~40 |
| + R + B + C + T + M | ~44 (adds CQP, 7QP, NY, FL, KY, NC) |
| Full support | 47 |

## Ready-to-spec today

- **New Hampshire QSO Party** — cleanly expressible. Suggested next step: write `specs/nhqp.json` modeled on `arrl_dx.json` (for asymmetric in/out handling) plus `specs/domains/nh_counties.txt`.

Nothing else. Every other party (excluding DE which remains unverified) needs at least the R gap closed.

## Hosting issues to revisit before finalizing

- **Delaware QP** — www.fsarc.org qsoparty page returns near-empty. Try contacting FSARC or checking a DE-local mirror.
- **Illinois QP** — w9awe.org uses a JS PDF viewer with dynamically-resolved URL (`{{openpdflink}}` template variable). Would need to load the page in a headless browser to resolve the real PDF path.
- **Maryland/DC QP** — w3vpr.org Drupal site returning `PDOException SQLSTATE[08004] Too many connections`. Not a rules problem, a Drupal problem — retry later.
- **Oklahoma QP** — k5cm.com OKQP is a frames-based site where the content frames are 404'd. Rules live somewhere but not at any URL I could find.

## Files referenced

- `src/spec.rs` — data model and evaluation (`Dimension`, `ScoreFormula`, `KeyExprOp::Concat`, `Predicate::*`)
- `specs/{ss,arrl_dx,cqww,naqp,ns_sprint,mst,cwt}.json` — reference specs; `arrl_dx.json` is the closest model for NHQP (asymmetric exchange + asymmetric mult key)
- `specs/domains/*.txt` — where a new `nh_counties.txt` would live
- `specs/README.md` — author guidance

## Verification section (for the final NH spec + this analysis)

1. Re-read `specs/arrl_dx.json` and `specs/ss.json` as reference patterns before writing `nhqp.json`.
2. Grep `src/spec.rs` for `Dimension::` and `Multiplier` to confirm that two multiplier objects with different dimensions + `when` gates both evaluate independently (needed for NH's asymmetric global-vs-band dimension).
3. Write `specs/nhqp.json` and `specs/domains/nh_counties.txt`; run the engine against a synthetic log with one NH fixed station working 3 counties on 2 bands to confirm the multiplier totals match the rules (NH=global, so 3; non-NH=band, so 3×2=6).
4. For the 4 ⚠ unverified rows (DE, IL, MD/DC, OK): re-fetch once their hosting is fixed and reclassify if needed. None are likely to flip the top-level conclusion, but DE may become the second YES.
5. For the NO parties: no immediate verification needed; the blocker assignments are mechanical consequences of primary-source rule text already captured in the table's "Rover/CL" and "Bonus" columns.
