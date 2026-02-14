Excellent. We’ll start with **CQWW (CW)** as the driver contest and use it to define the **minimum viable contest-engine** that is architecturally correct and extensible.

CQWW forces:

* Band-based dupes
* Two multiplier types (zone + entity)
* Per-band mult dimensions
* Conditional points rules
* Exchange parsing (RST + zone)
* External domain (DXCC list)

It does *not* yet require conditional exchange variants (ARRL DX will force that later), which makes it the ideal first target.

---

# Step 1: Define the Minimal Runtime Model (for CQWW only)

We implement only what CQWW needs first.

## Core Domain Types

These live in `contest-engine-domain`:

```rust
struct QsoInput {
    time: DateTime<Utc>,
    band: Band,
    mode: Mode,
    their_call: Callsign,
    raw_exchange: String,
}

struct ResolvedCall {
    dxcc: DxccEntity,
    continent: Continent,
}
```

For CQWW, we do NOT need:

* WPX prefix
* State/province
* Conditional exchange variants

Keep it minimal.

---

# Step 2: CQWW Exchange Parsing

CQWW exchange:

* RST
* CQ zone (1–40)

## Parsing Strategy (v1)

* Split raw exchange by whitespace
* Expect 2 tokens
* Token 1: RST → normalize to 599
* Token 2: integer zone → validate 1–40

Output:

```rust
struct ParsedExchange {
    rst: String,   // normalized
    zone: u8,
}
```

Validation returns:

```rust
enum EntryError {
    MissingField(FieldId),
    InvalidField(FieldId, String),
}
```

No DSL required yet — hardcode simple checks first. We’ll layer DSL later.

---

# Step 3: Dupe Logic (CQWW)

CQWW rule: once per band.

So dupe key:

```rust
struct DupeKey {
    band: Band,
    call: Callsign,
}
```

Runtime state:

```rust
HashSet<DupeKey>
```

That’s it.

---

# Step 4: Multiplier Logic (CQWW)

CQWW multipliers:

* Zone per band
* Country (DXCC entity) per band

So we need two mult trackers:

```rust
struct MultState {
    zones: HashMap<Band, HashSet<u8>>,
    countries: HashMap<Band, HashSet<DxccEntity>>,
}
```

Later this becomes generic by dimension; for now, keep it concrete to validate architecture.

---

# Step 5: Points Logic (CQWW)

From rules:

* Same country → 0
* Different country, same continent:

  * NA ↔ NA → 2
  * Other continent intra-continent → 1
* Different continent → 3

We implement:

```rust
fn compute_points(source: &ResolvedCall, dest: &ResolvedCall) -> u8
```

Hardcoded for CQWW first.

---

# Step 6: Score State

```rust
struct ContestState {
    total_points: u64,
    total_qsos: u32,
    mult_state: MultState,
    dupes: HashSet<DupeKey>,
}
```

Score computation:

```
claimed_score = total_points * total_mults
```

Where:

```
total_mults = sum_over_bands(
    zones_on_band + countries_on_band
)
```

---

# Step 7: Apply QSO API

```rust
fn apply_qso(
    &mut self,
    qso: QsoInput,
    parsed: ParsedExchange,
    source: ResolvedCall,
    dest: ResolvedCall,
) -> ApplyResult
```

Return:

```rust
struct ApplyResult {
    is_dupe: bool,
    qso_points: u8,
    new_zone_mult: bool,
    new_country_mult: bool,
    new_mult_count: u8,
    claimed_score: u64,
}
```

This is enough to drive:

* log window
* mult window
* score window

No “preview” required.

---

# Step 8: Non-Mutating Classifier (for spots & call window)

Even if you don’t call it preview scoring, you need this:

```rust
fn classify_candidate(
    &self,
    band: Band,
    call: &Callsign,
    dest: &ResolvedCall,
    zone: Option<u8>,
) -> CandidateEval
```

Returns:

```rust
struct CandidateEval {
    is_dupe: bool,
    would_be_new_zone: bool,
    would_be_new_country: bool,
}
```

This powers:

* bandmap highlighting
* callsign window “dupe” indicator
* “needed mult” indicators

Still no mutation.

---

# Step 9: Why This Minimal CQWW Engine Is Architecturally Correct

Because it already establishes:

* Separation of parsing vs scoring
* Separation of resolver vs engine
* Dupe dimension abstraction (band-based)
* Mult dimension abstraction (band-based)
* Query vs mutation APIs
* Deterministic state machine

When we generalize:

* `MultState` becomes generic
* `compute_points()` becomes rule-table driven
* Exchange parser becomes spec-driven
* Dupe dimension becomes enum-driven

But the core flow remains identical.

---

# Step 10: Next Concrete Task

Before writing DSL or general schema support, we should:

1. Implement this CQWW hardcoded engine.
2. Write 10–20 golden tests:

   * same country (0 pts)
   * same continent diff country (1 or 2 pts)
   * diff continent (3 pts)
   * dupe on same band
   * new mult vs old mult
   * mult counts per band

This gives you:

* Confidence in runtime design
* Confidence in performance
* Confidence that logstore integration will be simple

---

# Important Question Before We Code

Do you want:

A) Hardcoded CQWW engine first, then refactor into generic spec-driven engine?
or
B) Start with spec-driven architecture immediately (even if more scaffolding up front)?

My strong recommendation: **A first**, but architect it in a way that extraction into generic structures is obvious.

It dramatically reduces cognitive load and avoids building a DSL prematurely.

Which path do you want?

