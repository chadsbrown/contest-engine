# Spec-Driven Contest Format

This document describes the JSON spec format used by the contest engine to
define a contest declaratively. The format is intentionally data-driven: a
spec describes **what** a contest scores, not **how** the engine counts it.
The spec lives under `specs/*.json`; external domain lists (valid states,
counties, bonus callsigns, etc.) live under `specs/domains/*.txt`.

We use **NCCC NS Sprint** (`specs/ns_sprint.json`) as a running minimal
example, and pull in richer examples from other specs where they illustrate
features not exercised by NS Sprint (state QSO parties, ARRL DX, CQ WW,
Sweepstakes, NAQP, NEQSOP).

---

## 1. Minimal example — NS Sprint

```json
{
  "id": "ns_sprint",
  "name": "NCCC NS Sprint",
  "cabrillo_contest": "NCCC-SPRINT-CW",
  "bands": ["B160", "B80", "B40", "B20", "B15"],
  "modes": ["CW"],
  "dupe_dimension": "Band",
  "valid_qso": [],
  "exchange": {
    "received_variants": [
      {
        "when": null,
        "fields": [
          {"id": "nr",   "field_type": "Int",    "required": true, "domain": {"Range": {"min": 1, "max": 99999}}, "accept": [], "normalize_upper_trim": false},
          {"id": "name", "field_type": "String", "required": true, "domain": null, "accept": [], "normalize_upper_trim": true},
          {"id": "loc",  "field_type": "Enum",   "required": true, "domain": {"External": {"name": "ns_sprint_multipliers"}}, "accept": [], "normalize_upper_trim": false}
        ]
      }
    ],
    "sent_variants": [
      {
        "when": null,
        "fields": [
          {"id": "name", "value": {"Config": "my_name"}, "normalize_upper_trim": true},
          {"id": "loc",  "value": {"Config": "my_loc"},  "normalize_upper_trim": true}
        ]
      }
    ],
    "sent_serial": {"field_id": "serial", "start": 1, "width": 3}
  },
  "multipliers": [
    {"id": "loc_mult", "dimension": "Band",
     "variants": [{"when": null, "key": {"scope": "RCVD", "key": "loc"},
                   "domain": {"External": {"name": "ns_sprint_multipliers"}}}]}
  ],
  "points": [{"when": null, "value": 1}],
  "score": {"formula": "points_times_mults"},
  "config_fields": [
    {"id": "my_name", "required": true, "required_when": null, "domain": null},
    {"id": "my_loc",  "required": true, "required_when": null,
     "domain": {"External": {"name": "ns_sprint_multipliers"}}}
  ]
}
```

Everything that follows is a reference for the keys used above and every
optional extension that more complex contests employ.

---

## 2. Top-level structure

The root JSON object (see `ContestSpec` in `src/spec.rs:357`) has these
fields. Fields marked *(optional)* may be omitted or set to an empty
value.

| Field | Purpose |
| --- | --- |
| `id` | Short internal identifier (e.g. `"ns_sprint"`). Used for spec lookup and logs. |
| `name` | Human-readable name. |
| `cabrillo_contest` | Contest name emitted in the Cabrillo `CONTEST:` header. **Must be a canonical name** from [cabrillo-contest-names.md](cabrillo-contest-names.md) — downstream tools (Cabrillo checkers, RTC uploaders) key off this string. Variants can override per-mode. |
| `bands` | List of bands the contest runs on. See [§3 Bands and modes](#3-bands-and-modes). |
| `modes` | List of modes the contest allows. |
| `dupe_dimension` | How the dupe check is scoped. See [§4 Dupes](#4-dupes). |
| `dupe_extra_rcvd_fields` *(optional)* | Additional received fields appended to the dupe key (for rovers). |
| `valid_qso` | List of `(predicate, message)` pairs asserting which QSOs are eligible. See [§5](#5-valid_qso). |
| `exchange` | Received/sent exchange definitions. See [§6](#6-exchange). |
| `multipliers` | Multiplier rules. See [§7](#7-multipliers). |
| `points` | Per-QSO point rules. See [§8](#8-points). |
| `bonus_rules` *(optional)* | Additive bonuses. See [§9](#9-bonus_rules). |
| `score_multiplier` *(optional)* | Whole-score multiplier driven by a config value. See [§10](#10-score_multiplier). |
| `score` | Score formula. See [§11](#11-score). |
| `config_fields` | Operator config slots the spec depends on. See [§12](#12-config_fields). |
| `variants` *(optional)* | Named sub-contests (e.g. CW/SSB weekends of CQWW). See [§13](#13-variants). |
| `default_variant` *(optional)* | Variant selected when caller doesn't specify one. |
| `deprecated` *(optional)* | Mark a spec as superseded and point at its replacement. |

---

## 3. Bands and modes

`bands` holds one or more of: `B160`, `B80`, `B60`, `B40`, `B30`, `B20`,
`B17`, `B15`, `B12`, `B10`, `B6`, `B2` (`src/types.rs:4`).

`modes` holds one or more of: `CW`, `SSB`, `RTTY`, `FT8`, `FT4`, `DIGITAL`
(`src/spec.rs:29`).

NS Sprint: CW only, 160–15m (no 10m by contest rule).

---

## 4. Dupes

`dupe_dimension` is a `Dimension` — one of:

| Value | Meaning |
| --- | --- |
| `"Band"` | Same call on a new band is not a dupe. |
| `"Mode"` | Same call on a new mode is not a dupe. |
| `"BandMode"` | Same call on a new band *or* new mode is not a dupe. |
| `"Period:N"` | Reset every `N` minutes (e.g. `"Period:60"` for an ICWC MST-style per-hour contest). |
| `"Global"` | Once per log (e.g. ARRL Sweepstakes). |

`dupe_extra_rcvd_fields` *(optional)* is a list of received-field ids
appended to the dupe key. State QSO parties use this to make a mobile
station re-workable when they change counties. Example (`specs/inqp.json`):

```json
"dupe_dimension": "BandMode",
"dupe_extra_rcvd_fields": ["loc"]
```

This means W9ABC in MRN county and the same W9ABC later in MAD county on
the same band+mode are two distinct QSOs.

---

## 5. `valid_qso`

A list of `(predicate, reason)` pairs. **All predicates must pass** or the
QSO is rejected with the given reason. Empty list means every QSO is
accepted.

ARRL DX (`specs/arrl_dx.json`) — only W/VE ↔ DX QSOs count:

```json
"valid_qso": [
  [{"Ne": [{"Field": {"scope": "SOURCE", "key": "is_wve"}},
           {"Field": {"scope": "DEST",   "key": "is_wve"}}]},
   "ARRL DX requires W/VE <-> DX contacts only"]
]
```

Indiana QSO Party — out-of-state stations may only work IN stations:

```json
"valid_qso": [
  [{"Or": [
     {"Eq": [{"Field": {"scope": "CONFIG", "key": "my_is_in"}},
             {"Value": {"Bool": true}}]},
     {"Eq": [{"Field": {"scope": "DEST",   "key": "is_wve"}},
             {"Value": {"Bool": true}}]}
   ]},
   "non-IN stations may only work IN stations"]
]
```

See [§14 Evaluation language](#14-evaluation-language) for predicate
syntax.

---

## 6. `exchange`

Describes both sides of the contest exchange.

### 6.1 `received_variants`

An ordered list of `ExchangeVariant`s. Each variant has:

- `when` — a predicate (or `null` = always). The **first** variant whose
  `when` passes is used. Variant selection happens *before* the exchange
  is parsed, so `when` may only reference `CONFIG`, `SOURCE`, and `DEST`
  scopes (not `RCVD`/`SENT`/`SESSION`).
- `fields` — ordered list of `ExchangeField`s, one per token expected in
  the received exchange string.

ARRL DX uses this for role-dependent exchanges — a W/VE op expects
`<rst> <power>`, a DX op expects `<rst> <section>`:

```json
"received_variants": [
  { "when": {"Eq": [{"Field": {"scope": "CONFIG", "key": "my_is_wve"}}, {"Value": {"Bool": true}}]},
    "fields": [
      {"id": "rst",   "field_type": "Rst",   "required": true, ...},
      {"id": "power", "field_type": "Power", "required": true, ...}
    ]},
  { "when": {"Eq": [{"Field": {"scope": "CONFIG", "key": "my_is_wve"}}, {"Value": {"Bool": false}}]},
    "fields": [
      {"id": "rst", "field_type": "Rst",  "required": true, ...},
      {"id": "sp",  "field_type": "Enum", "required": true,
       "domain": {"External": {"name": "arrl_dx_wve_multipliers"}}, ...}
    ]}
]
```

### 6.2 `ExchangeField`

| Field | Meaning |
| --- | --- |
| `id` | Field name (referenced later as `RCVD.<id>`). |
| `field_type` | One of `Rst`, `Int`, `Enum`, `Power`, `String`, `Literal` — see [§6.3](#63-fieldtype). |
| `required` | If `true` and the token is absent/blank, the parse fails. |
| `domain` *(optional)* | Valid-value check. `Any`, `{"Range": {"min": n, "max": m}}`, `{"External": {"name": "…"}}`, or `{"List": ["A","B"]}`. |
| `accept` | Additional literal tokens that are accepted (used mostly by `Literal`). Example: NAQP DX stations may send the literal `"DX"` for `loc`. |
| `normalize_upper_trim` | If `true`, value is trimmed and upper-cased before comparison. |
| `multi_value_sep` *(optional)* | Separator for county-line exchanges like `"NH/ON"`. The field value keeps the whole string; the multiplier extractor splits on this separator and validates each piece. Used by state QSO parties. |

### 6.3 `FieldType`

| Type | Parser behavior |
| --- | --- |
| `Rst` | Validates `1–5` report digits per RST rules (`59`, `599`, `5NN`, etc.). |
| `Int` | Parses as an integer; `domain` typically a `Range`. |
| `Enum` | Normalizes to uppercase and canonicalizes location abbreviations (e.g. `NWT→NT`). Used for sections, states, counties, zones-as-text. |
| `Power` | Normalizes `100`, `100W`, `1KW`, `1.5 KW` → a whole number of watts. |
| `String` | Free text; trims/uppers if `normalize_upper_trim`. Fields named `name` are constrained to 1–12 alphanumerics plus `-`, `/`, `'`. |
| `Literal` | Accepts only tokens in `accept`. Not required to be present if `required: false`. |

NS Sprint uses `Int` (serial), `String` (name), and `Enum` (location).

A one-token RST+zone exchange (common in CQ WW as `5915` meaning `599
15`) is auto-split when a variant has two fields, the first `Rst` and the
second `Int` (`src/spec.rs:1171`). You don't need to configure anything
for that.

### 6.4 `sent_variants`

Define the tokens this station sends. Similar structure to received
variants, but each field has a `value`:

- `{"Const": "599"}` — a literal string.
- `{"Config": "my_state_province"}` — look up the named config field.

The first variant whose `when` passes is used.

### 6.5 `sent_serial` *(optional)*

Attaches a running serial counter to a sent field:

```json
"sent_serial": {"field_id": "serial", "start": 1, "width": 3}
```

- `field_id` — which sent field the number is written to.
- `start` — first number (default `1`).
- `width` — zero-padded width for rendering (default `3`).
- `scope` *(optional)* — `Dimension` that resets the counter (e.g.
  `"BandMode"` would mean a separate counter per band+mode). Default is a
  single global counter.

NS Sprint uses a 3-digit global serial.

---

## 7. `multipliers`

A list of `MultiplierSpec`s. Each entry defines one multiplier **group**
that can fire once per key-value per dimension bucket.

```json
{
  "id": "loc_mult",
  "dimension": "Band",
  "variants": [
    {"when": null,
     "key": {"scope": "RCVD", "key": "loc"},
     "domain": {"External": {"name": "ns_sprint_multipliers"}}}
  ]
}
```

### 7.1 `dimension`

Same `Dimension` enum as `dupe_dimension`. For multipliers it controls
how the multiplier is counted:

- `Band` → once per unique value per band (NS Sprint: `IN` on 40m and
  `IN` on 20m = 2 mults).
- `BandMode` → once per unique value per `(band, mode)` pair.
- `Mode` → once per unique value per mode (IN QP).
- `Global` → once per unique value in the whole log (Sweepstakes
  section, MOQP county).
- `Period:N` → resets every N minutes.

### 7.2 `variants`

Ordered list. The engine walks them in order; the **first** variant
whose `when` passes is applied. This lets one multiplier group behave
differently for in-state vs. out-of-state operators.

Indiana QP has three multiplier groups, each with a gated variant:

```json
"multipliers": [
  {"id": "county_mult",        "dimension": "Mode",
   "variants": [{"when": {"Eq": [{"Field": {"scope": "CONFIG", "key": "my_is_in"}}, {"Value": {"Bool": false}}]},
                 "key": {"scope": "RCVD", "key": "loc"},
                 "domain": {"External": {"name": "in_counties"}}}]},
  {"id": "in_state_state_mult", "dimension": "Mode",
   "variants": [{"when": {"Eq": [{"Field": {"scope": "CONFIG", "key": "my_is_in"}}, {"Value": {"Bool": true}}]},
                 "key": {"scope": "RCVD", "key": "loc"},
                 "domain": {"External": {"name": "us_states"}}}]},
  {"id": "in_state_prov_mult",  "dimension": "Mode",
   "variants": [{"when": {"Eq": [{"Field": {"scope": "CONFIG", "key": "my_is_in"}}, {"Value": {"Bool": true}}]},
                 "key": {"scope": "RCVD", "key": "loc"},
                 "domain": {"External": {"name": "ca_provinces"}}}]}
]
```

In-state ops get state+province mults; out-of-state ops get county
mults. Using separate *groups* (not just separate variants inside one
group) lets an in-state op collect **both** state and province mults on
the same log.

### 7.3 `key`

`KeyExpr` — either a plain field reference or a concatenation:

- Field: `{"scope": "RCVD", "key": "loc"}` (NS Sprint), or
  `{"scope": "DEST", "key": "dxcc"}` (CQWW country).
- Concat:
  ```json
  {"Op": {"Concat": [
    {"Field": {"scope": "RCVD", "key": "zone"}},
    {"Const": "-"},
    {"Field": {"scope": "DEST", "key": "dxcc"}}
  ]}}
  ```
  (No current spec uses this — it exists for future contests that mult
  on a tuple.)

Callsign-based mults use `{"scope": "DEST", "key": "call"}` (CWT, MST).

### 7.4 `domain`

Constrains which key values count. Values outside the domain produce no
mult. Accepts the same `DomainRef` shapes as exchange-field domains.
NS Sprint reuses the same `ns_sprint_multipliers` external list for the
received `loc` field, the `my_loc` config field, and this mult domain.

---

## 8. `points`

Ordered list of `PointRule`s. The engine evaluates them top-to-bottom and
uses the value of the **first** rule whose `when` passes. A trailing
`{"when": null, "value": 0}` is the default fallback.

NS Sprint — 1 point per QSO:

```json
"points": [{"when": null, "value": 1}]
```

CQ WW — scored by continent relationship:

```json
"points": [
  {"when": {"Eq": [{"Field": {"scope": "SOURCE", "key": "dxcc"}},
                    {"Field": {"scope": "DEST",   "key": "dxcc"}}]}, "value": 0},
  {"when": {"Ne": [{"Field": {"scope": "SOURCE", "key": "continent"}},
                    {"Field": {"scope": "DEST",   "key": "continent"}}]}, "value": 3},
  {"when": {"And": [
     {"Eq": [{"Field": {"scope": "SOURCE", "key": "continent"}}, {"Field": {"scope": "DEST", "key": "continent"}}]},
     {"Ne": [{"Field": {"scope": "SOURCE", "key": "dxcc"}},      {"Field": {"scope": "DEST", "key": "dxcc"}}]},
     {"Eq": [{"Field": {"scope": "SOURCE", "key": "continent"}}, {"Value": {"Text": "NA"}}]}
   ]}, "value": 2},
  {"when": {"And": [
     {"Eq": [{"Field": {"scope": "SOURCE", "key": "continent"}}, {"Field": {"scope": "DEST", "key": "continent"}}]},
     {"Ne": [{"Field": {"scope": "SOURCE", "key": "dxcc"}},      {"Field": {"scope": "DEST", "key": "dxcc"}}]}
   ]}, "value": 1},
  {"when": null, "value": 0}
]
```

Mode-dependent points (NE QP, MO QP, NEQSOP):

```json
"points": [
  {"when": {"Eq": [{"Field": {"scope": "SESSION", "key": "mode"}},
                    {"Value": {"Text": "SSB"}}]}, "value": 1},
  {"when": null, "value": 2}
]
```

Bonus-station point rule (ON QP — `onqp_bonus_stations` get 10 pts):

```json
"points": [
  {"when": {"DestCallIn": {"External": {"name": "onqp_bonus_stations"}}}, "value": 10},
  {"when": null, "value": 2}
]
```

---

## 9. `bonus_rules` *(optional)*

Additive bonus points, applied **after** `points × mults` and before the
score multiplier is consumed (see [§11](#11-score) for the formula).
Empty (default) = no bonuses.

```rust
BonusRule { id, when, amount, scope }
```

`scope` is:

- `"once"` — rule fires at most once per log.
- `"per_qso"` — fires on every matching QSO.
- `"per_band_mode"` — at most once per (band, mode) pair (e.g. 12 max
  for a 6-band CW+SSB contest).

Missouri QP — two one-time 100-point bonuses for working W0MA or K0GQ:

```json
"bonus_rules": [
  {"id": "w0ma_bonus", "when": {"DestCallIn": {"List": ["W0MA"]}}, "amount": 100, "scope": "once"},
  {"id": "k0gq_bonus", "when": {"DestCallIn": {"List": ["K0GQ"]}}, "amount": 100, "scope": "once"}
]
```

Nebraska QSO Party — external list of bonus stations, 100 points once:

```json
"bonus_rules": [
  {"id": "ne0qp_bonus",
   "when": {"DestCallIn": {"External": {"name": "neqsop_bonus_stations"}}},
   "amount": 100, "scope": "once"}
]
```

`DestCallIn` is the one predicate that reads the destination callsign
directly against a domain list — see [§14](#14-evaluation-language).

---

## 10. `score_multiplier` *(optional)*

Whole-score multiplier selected from a config-field value. NEQSOP awards
QRP ops ×5:

```json
"score_multiplier": {
  "from_config": "my_power_class",
  "mapping": {"QRP": 5, "LOW": 2, "HIGH": 1},
  "default": 1
}
```

Applied to `base = points × mults` before bonus addition. Lookup is
case-insensitive; absent/unmapped value uses `default`.

---

## 11. `score`

```json
"score": {"formula": "points_times_mults"}
```

Three formulas exist (`ScoreFormula` in `src/spec.rs:531`):

| Formula | Meaning |
| --- | --- |
| `points_times_mults` (alias: `points_times_total_mults`) | Default. `Σpoints × Σmults`. |
| `points_times_sum_band_mults` | Same as above but mults are counted per-band and summed. (Equivalent when every mult group has `dimension: "Band"`.) |
| `sum_band_points_times_band_mults` | Per-band `points × mults`, then summed across bands. |

Final claimed score (`src/spec.rs:2062`):

```
claimed = (base × score_multiplier_factor) + Σbonus
```

---

## 12. `config_fields`

Declares the operator-side config slots the spec relies on. Each field:

| Field | Meaning |
| --- | --- |
| `id` | Config key. Referenced via `{"scope": "CONFIG", "key": "<id>"}` anywhere predicates or sent values are evaluated. |
| `required` | If `true` and unset at session start, config validation fails. |
| `required_when` *(optional)* | Conditional requirement — the predicate references other config fields. |
| `domain` *(optional)* | Same `DomainRef` shapes as exchange-field domains. |

Indiana QP makes `my_county` required only when `my_is_in == true`:

```json
{"id": "my_county", "required": false,
 "required_when": {"Eq": [{"Field": {"scope": "CONFIG", "key": "my_is_in"}},
                           {"Value": {"Bool": true}}]},
 "domain": {"External": {"name": "in_counties"}}}
```

NS Sprint simply requires `my_name` and `my_loc`.

---

## 13. `variants` *(optional)*

A map of sub-contests sharing the parent spec's structure. Callers
select one by name.

```json
"variants": {
  "cw":  {"name": "CW Weekend",  "cabrillo_contest": "CQ-WW-CW",
          "allowed_modes": ["CW"],  "exchange": {"sent_rst_value": "599"}},
  "ssb": {"name": "SSB Weekend", "cabrillo_contest": "CQ-WW-SSB",
          "allowed_modes": ["SSB"], "exchange": {"sent_rst_value": "59"}}
}
```

Each variant can override:

- `name`, `cabrillo_contest`
- `allowed_modes` (restricts modes for this weekend)
- `exchange.sent_rst_value` (the literal sent report — `599` for CW, `59`
  for SSB)

Selection rules (`src/spec.rs:569`):

1. Caller-requested name wins.
2. Otherwise `default_variant` is used if set.
3. Otherwise if exactly one variant is defined, it is used.
4. Otherwise no variant is selected.

---

## 14. Evaluation language

### 14.1 `Predicate`

| Constructor | Notes |
| --- | --- |
| `{"Eq": [a, b]}` | Equality between two operands. |
| `{"Ne": [a, b]}` | Inequality. |
| `{"And": [p1, p2, …]}` | All must hold. |
| `{"Or": [p1, p2, …]}` | Any must hold. |
| `{"Not": p}` | Negation. |
| `{"Between": [{field}, min, max]}` | Integer range inclusive. |
| `{"In": [{field}, ["A","B"]]}` | Text-valued field against a literal list. |
| `{"DestCallIn": <DomainRef>}` | Destination callsign appears in a domain (list or external file). Only available inside `points`, `bonus_rules`, and `valid_qso`. |

### 14.2 `Operand`

Either `{"Field": <FieldRef>}` or `{"Value": <Value>}`.

Values are `{"Bool": true/false}`, `{"Int": n}`, or `{"Text": "…"}`.

### 14.3 `FieldRef` scopes

| Scope | Keys |
| --- | --- |
| `CONFIG` | Any declared `config_fields` id. |
| `SOURCE` | Operator's station: `dxcc`, `continent`, `is_wve`, `is_na`. |
| `DEST` | Remote station: `dxcc`, `continent`, `is_wve`, `is_na`, `call` (only useful as a key). |
| `RCVD` | Any received exchange field id. |
| `SENT` | Any sent exchange field id. |
| `SESSION` | `band`, `mode` — the band/mode the QSO is being applied under. Available in `points`, multiplier variants, and `bonus_rules`. **Not** available in exchange-variant `when` selection (the QSO's band/mode isn't bound to a parsed exchange yet). |

### 14.4 `DomainRef`

| Shape | Meaning |
| --- | --- |
| `"Any"` | No constraint. |
| `{"Range": {"min": n, "max": m}}` | Integer inclusive range. |
| `{"List": ["A","B","C"]}` | Literal set (case-sensitive; use `normalize_upper_trim` if needed). |
| `{"External": {"name": "<file>"}}` | Look up `specs/domains/<file>.txt`. One token per line. Case-insensitive match. |

---

## 15. External domain files

Files under `specs/domains/*.txt` are newline-delimited, one token per
line, normalized to uppercase. Current packs include:

- `us_states.txt`, `ca_provinces.txt`
- `dxcc_entities.txt`, `dxcc_entities_excluding_w_ve.txt`
- Section lists (`ss_sections.txt`)
- Contest-specific lists: `naqp_multipliers.txt`, `ns_sprint_multipliers.txt`, `arrl_dx_wve_multipliers.txt`
- County lists: `fl_counties.txt`, `ga_counties.txt`, `in_counties.txt`, `mi_counties.txt`, `mo_counties.txt`, `nd_counties.txt`, `ne_counties.txt`, `neqp_counties.txt`, `nh_counties.txt`, `nm_counties.txt`, `de_counties.txt`
- Regional lists: `on_multiplier_areas.txt`, `qc_regions.txt`
- Bonus station lists: `moqp_bonus_stations.txt`, `neqsop_bonus_stations.txt`, `onqp_bonus_stations.txt`

Load at runtime with
`contest_engine::spec::domain_packs::load_standard_domain_pack("specs/domains")`.

---

## 16. Applying a QSO — end-to-end picture

1. The engine picks the `received_variant` whose `when` passes (or the
   first `null`-guarded one).
2. Tokens from the raw exchange are parsed field-by-field per §6. A
   two-token RST+Int exchange may be auto-split from a single token.
3. Station callsigns are resolved to `ResolvedStation` (dxcc, continent,
   W/VE and NA flags).
4. `valid_qso` predicates run; any failure rejects the QSO.
5. Dupe check runs against `dupe_dimension` (+ `dupe_extra_rcvd_fields`).
6. Points are computed from the first matching `points` rule.
7. Each multiplier group's first matching variant produces zero or one
   mult key (subject to `domain`). `multi_value_sep` splits county-line
   exchanges into multiple mult keys.
8. Bonus rules accumulate per their `scope`.
9. `claimed_score = (Σpoints × Σmults) × score_multiplier + Σbonus`.

---

## 17. Where to look in the code

- `src/spec.rs` — the full spec schema (`ContestSpec` at line 357), the
  evaluator, and the `SpecEngine` that applies QSOs.
- `src/types.rs` — `Band`, `Continent`, `Callsign`.
- `specs/*.json` — one spec per contest.
- `specs/domains/*.txt` — external domain lists.
- `tests/fixtures/snapshots/*.score.json` — golden scoring snapshots
  that document the expected behavior of each spec.
