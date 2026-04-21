# Cabrillo Contest Names

Every spec's `cabrillo_contest` (and per-mode `variants.<mode>.cabrillo_contest`)
must be a string from the canonical list at
**https://www.contestcalendar.com/cabnames.php** (maintained by WA7BNM).
Log-checking teams and upload services use these strings as keys; any
deviation gets the log misfiled or rejected.

> **Last reviewed:** 2026-04-20.

Downstream, **clogger uses `cabrillo_contest` for both Cabrillo export
and RTC (Real Time Contest) uploads** — one name per (contest, mode)
pair, propagated to every consumer. If a string is wrong here, it's
wrong everywhere.

## Adding a new contest

1. Open the list above and find the exact `CONTEST:` header string for
   the contest you're adding. Copy it verbatim — case and punctuation
   matter.
2. Set `cabrillo_contest` at the top level of the spec to the
   mode-neutral form (or the single form if the contest has one mode).
3. If the contest has mode-specific sponsor names (e.g. `ARRL-DX-CW`
   vs `ARRL-DX-SSB`), add a `variants` block with one entry per mode
   and set `variants.<mode>.cabrillo_contest` to the mode-specific
   string. The top-level value becomes the fallback.
4. If you can't find the contest in cabnames.php, it's probably not
   accepted by standard log-checking services — reach out to the
   contest sponsor for the correct identifier before committing the
   spec.

## Common pitfalls

- **ARRL contests need the `ARRL-` prefix.** E.g. `ARRL-SS-CW`, not
  `SS-CW`. Same for `ARRL-DX-CW`, `ARRL-160`, `ARRL-RTTY`, etc.
- **CWT's Cabrillo name is `CW-OPS`.** Not `CWT` (that's the common
  name, not the header string).
- **Florida QP uses `FCG-FQP`** per the canonical list — some loggers
  emit `FL-QSO-PARTY` historically but the FCG form is what cabnames
  lists.
- **Mode-specific sprint names.** NAQP is `NAQP-CW` / `NAQP-SSB` /
  `NAQP-RTTY` — no bare `NAQP`. NCCC Sprint is `NCCC-SPRINT-CW` /
  `NCCC-SPRINT-FT4` — no bare `NCCC-SPRINT`.

## Verification

After editing a `specs/*.json`:

```
cargo test --test regression_existing_specs
```

If a regression test fails because it hardcoded the old string, update
the fixture to match — the spec is the source of truth.
