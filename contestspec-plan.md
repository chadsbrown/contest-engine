Awesome — here are **three concrete `ContestSpec` drafts** (CQWW, ARRL DX, NAQP) in the schema style we’ve been discussing, plus a crisp plan for building `contest-engine` around them.

Everything rules-wise below is taken from the official contest rules: CQWW rules page ([CQ World Wide DX Contest][1]), ARRL DX rules PDF ([ARRL Contests][2]), and NAQP rules PDF ([National Contest Journal][3]).

---

# 0) What these specs assume about “resolution” (outside engine)

Each logged QSO handed to the engine includes:

* `SOURCE`: our station derived info (dxcc/entity, continent, maybe “is_wve”, etc.)
* `DEST`: their derived info (dxcc/entity, continent, “is_na” maybe, etc.)

Those are supplied by the logger core (via CTY/history services). The engine never reads CTY.DAT itself.

---

# 1) CQWW (CW) draft spec

```yaml
id: cqww_cw
name: "CQ World-Wide DX Contest (CW)"
cabrillo_contest: "CQ-WW-CW"

bands: [160, 80, 40, 20, 15, 10]
modes: [CW]

dupe:
  dimension: band   # "Stations may be contacted once on each band." :contentReference[oaicite:3]{index=3}

exchange:
  received:
    - id: rst
      type: rst
      required: true
      normalize: rst_599   # allow 5NN/599 variants; normalize to 599
    - id: zone
      type: int
      required: true
      validate:
        - check: "between(DEST.zone, 1, 40)"
          error: "CQ zone must be 1..40"
  sent:
    - id: rst
      type: rst
      value: "599"
    - id: zone
      type: int
      value: "CONFIG.my_cq_zone"

multipliers:
  - id: zone
    key: "DEST.zone"
    dimension: band
    domain: { kind: range, min: 1, max: 40 }  # zone mult per band :contentReference[oaicite:4]{index=4}
  - id: country
    key: "DEST.dxcc"          # “country” = DXCC entity list (+ WAE extras) :contentReference[oaicite:5]{index=5}
    dimension: band
    domain: { kind: external, name: dxcc_entities }

points:
  # CQWW QSO points rules :contentReference[oaicite:6]{index=6}
  - when: "SOURCE.dxcc == DEST.dxcc"
    value: 0
  - when: "SOURCE.continent != DEST.continent"
    value: 3
  - when: "SOURCE.continent == DEST.continent and SOURCE.dxcc != DEST.dxcc and SOURCE.continent == 'NA' and DEST.continent == 'NA'"
    value: 2
  - when: "SOURCE.continent == DEST.continent and SOURCE.dxcc != DEST.dxcc"
    value: 1
  - default: 0

score:
  qso_points: "sum(points)"
  multiplier_total: "count_mult(zone, band) + count_mult(country, band)"
  claimed_score: "qso_points * multiplier_total"

config_fields:
  - { id: my_call, type: callsign, required: true }
  - { id: my_cq_zone, type: int, required: true, validate: [{ check: "between(CONFIG.my_cq_zone,1,40)", error: "My CQ zone must be 1..40"}] }
```

---

# 2) ARRL International DX (CW/SSB) draft spec

This contest has asymmetric exchange & multipliers depending on whether **you** are W/VE or DX. Rules: once per band, W/VE ↔ DX only; W/VE send state/province; DX send power; 3 points/QSO; mults count once per band, but mult *type* depends on W/VE vs DX. ([ARRL Contests][2])

```yaml
id: arrl_dx
name: "ARRL International DX Contest"
cabrillo_contest: "ARRL-DX-CW"   # or ARRL-DX-SSB (select in session config / mode-weekend)
bands: [160, 80, 40, 20, 15, 10]
modes: [CW, SSB]

dupe:
  dimension: band    # once per band :contentReference[oaicite:8]{index=8}

# Valid contact constraints (engine-enforced at apply-time)
valid_qso:
  - check: "SOURCE.is_wve != DEST.is_wve"
    error: "ARRL DX: W/VE stations work DX only; DX stations work W/VE only."  # :contentReference[oaicite:9]{index=9}

exchange:
  received_variants:
    # If WE are W/VE, then the other station is DX, and sends signal report + power :contentReference[oaicite:10]{index=10}
    - when: "CONFIG.my_is_wve == true"
      fields:
        - { id: rst, type: rst, required: true, normalize: rst_599 }
        - { id: power, type: power, required: true }     # accepts "KW", "500", etc.
    # If WE are DX, the other station is W/VE, and sends signal report + state/province :contentReference[oaicite:11]{index=11}
    - when: "CONFIG.my_is_wve == false"
      fields:
        - { id: rst, type: rst, required: true, normalize: rst_599 }
        - { id: sp, type: enum, required: true, domain: { kind: external, name: arrl_dx_wve_multipliers } }

  sent_variants:
    - when: "CONFIG.my_is_wve == true"
      fields:
        - { id: rst, type: rst, value: "599" }
        - { id: sp,  type: enum, value: "CONFIG.my_state_province", domain: { kind: external, name: arrl_dx_wve_multipliers } }
    - when: "CONFIG.my_is_wve == false"
      fields:
        - { id: rst, type: rst, value: "599" }
        - { id: power, type: power, value: "CONFIG.my_power_sent" }

multipliers:
  # Multipliers count once per band :contentReference[oaicite:12]{index=12}
  - id: dx_mult
    dimension: band
    # W/VE stations count DXCC entities (except USA/Canada) :contentReference[oaicite:13]{index=13}
    key_variants:
      - when: "CONFIG.my_is_wve == true"
        key: "DEST.dxcc"
        validate:
          - check: "DEST.dxcc != 'W' and DEST.dxcc != 'VE'"
            error: "ARRL DX: W/VE multiplier excludes USA/Canada."
      # DX stations count US states + DC + Canadian provinces/territories (+LB; VO1/VO2 separate) :contentReference[oaicite:14]{index=14}
      - when: "CONFIG.my_is_wve == false"
        key: "DEST.sp"   # parsed received state/province token
    domain_variants:
      - when: "CONFIG.my_is_wve == true"
        domain: { kind: external, name: dxcc_entities_excluding_w_ve }
      - when: "CONFIG.my_is_wve == false"
        domain: { kind: external, name: arrl_dx_wve_multipliers }

points:
  - default: 3   # each contact = 3 points :contentReference[oaicite:15]{index=15}

score:
  qso_points: "sum(points)"
  multiplier_total: "count_mult(dx_mult, band)"
  claimed_score: "qso_points * multiplier_total"

config_fields:
  - { id: my_call, type: callsign, required: true }
  - { id: my_is_wve, type: bool, required: true }
  - { id: my_state_province, type: enum, required: false, domain: { kind: external, name: arrl_dx_wve_multipliers } }
  - { id: my_power_sent, type: power, required: false }
```

Notes:

* The external domain `arrl_dx_wve_multipliers` is the official ARRL multiplier list (states/DC/provinces/territories plus traditions like VO1/VO2 split, LB, etc.). The rules PDF explicitly references it. ([ARRL Contests][2])
* This keeps the engine clean: it doesn’t embed that list; it consumes it.

---

# 3) NAQP (CW/SSB/RTTY) draft spec

Key points from NAQP rules:

* Exchange: **Name + location** for North American stations; **name only** for non-NA. ([National Contest Journal][3])
* Multipliers: US states + DC + Canadian provinces/territories + other NA DXCC entities; mults count **per band**. ([National Contest Journal][3])
* Stations may be worked **once per band**; duplicates on a band are invalid. ([National Contest Journal][3])
* Scoring is “total valid contacts × sum of multipliers per band,” which implies **1 point per valid QSO**. ([National Contest Journal][3])

```yaml
id: naqp
name: "North American QSO Party"
cabrillo_contest: "NAQP-CW"  # or NAQP-SSB / NAQP-RTTY
bands: [160, 80, 40, 20, 15, 10]
modes: [CW, SSB, RTTY]

dupe:
  dimension: band   # once per band :contentReference[oaicite:21]{index=21}

exchange:
  received_variants:
    # If worked station is North American: name + location :contentReference[oaicite:22]{index=22}
    - when: "DEST.is_na == true"
      fields:
        - { id: name, type: string, required: true, normalize: upper_trim }
        - { id: loc, type: enum, required: true, domain: { kind: external, name: naqp_multipliers } }
    # If worked station is not North American: name only :contentReference[oaicite:23]{index=23}
    - when: "DEST.is_na == false"
      fields:
        - { id: name, type: string, required: true, normalize: upper_trim }
        # allow optional "DX" token and ignore it (common practice)
        - { id: loc, type: literal, required: false, accept: ["DX"] }

  sent:
    # NAQP requires you use a single name throughout; enforce via config validation, not per-QSO. :contentReference[oaicite:24]{index=24}
    - { id: name, type: string, value: "CONFIG.my_name", normalize: upper_trim }
    - { id: loc, type: enum, value: "CONFIG.my_loc", domain: { kind: external, name: naqp_multipliers } }

multipliers:
  - id: na_mult
    key: "DEST.loc"
    dimension: band
    domain: { kind: external, name: naqp_multipliers }  # states, DC, provinces/territories, other NA DXCC :contentReference[oaicite:25]{index=25}

points:
  - default: 1    # implied by scoring definition :contentReference[oaicite:26]{index=26}

score:
  qso_points: "sum(points)"
  multiplier_total: "count_mult(na_mult, band)"
  claimed_score: "qso_points * multiplier_total"

config_fields:
  - { id: my_call, type: callsign, required: true }
  - { id: my_name, type: string, required: true, validate: [{ check: "len(CONFIG.my_name) <= 10", error: "NAQP name too long"}] }
  - { id: my_loc, type: enum, required: true, domain: { kind: external, name: naqp_multipliers } }
```

---

# Overall contest-engine plan (what we implement next)

## Phase 1 — Core infrastructure

* `Value` system and field addressing (`CONFIG.*`, `SOURCE.*`, `DEST.*`, `RCVD.*`, `SENT.*`)
* DSL:

  * predicate AST: `==`, `!=`, `and/or/not`, `between()`, `in()`, `matches()`
  * expression AST for ints/strings/bools + `if()`
* `ContestSpec` loader + compiler → `ContestPlan`

  * precompile regex
  * validate that referenced fields exist

## Phase 2 — Exchange parsing & validation

* tokenization (whitespace)
* typed parsing by `type` (`rst`, `int`, `enum`, `power`, `callsign`)
* conditional variants (`received_variants`, `sent_variants`)
* structured feedback: missing/invalid fields with error strings

## Phase 3 — Runtime state & scoring

* dupe index by dimension
* multiplier tracking by (mult_id, dimension_context)
* points evaluation (ordered rules)
* totals and claimed score formula

## Phase 4 — Query API for UI parity

* `is_dupe(ctx, call)`
* `would_be_new_mults(ctx, resolved_dest, parsed_exchange)`
* `worked_mults(mult_id, band?)`
* `needed_mults(mult_id, band?)` (requires domain provider)

---

# Quick note so you don’t forget logstore

When we get to “edit QSO / bust / NIL / corrections,” the easiest v1 is:

* logstore holds authoritative QSO list
* contest-engine can **recompute state from scratch** from QSOs (20k is fine)
  That keeps both systems simple and correct.

---

If you want to proceed efficiently: next we should pick **one** of these specs (I’d start with **CQWW** because it’s clean) and use it to drive:

1. the minimal DSL implementation (points + mult rules),
2. exchange parsing (RST+zone),
3. dupe/mult indexing.

Then ARRL DX forces conditional variants, and NAQP forces domains/lists + NA/non-NA branching.

[1]: https://cqww.com/rules.htm "CQ WW - Rules"
[2]: https://contests.arrl.org/ContestRules/DX-Rules.pdf "ARRL International DX Contest Rules v 2.0"
[3]: https://www.ncjweb.com/NAQP-Rules.pdf "NAQP rules.indd"

