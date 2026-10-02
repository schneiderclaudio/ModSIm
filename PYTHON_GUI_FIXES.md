# PySide6 GUI — Fixes Applied

**Scope:** Section A bugs (A1–A11) + B1 wire-up from `PYTHON_GUI_AUDIT.md`, plus
`.gitignore` cleanup for build artifacts.

**Verification:** full suite passes — `python -m unittest discover -s tests`
from `Modsim/Modsimpy/` → **65 tests OK (2 skipped: pre-existing engine-bridge
skips)**.

---

## 1. `.gitignore` cleanup

**File:** `.gitignore`

Added rules for Visual Studio / VC++ build artifacts that were accidentally
tracked, and untracked them from the index (`git rm --cached`):

```
Debug_ifx/
*.sbr
*.bsc
*.opt
*.pdb
*.PDB
*.idb
*.IDB
```

`*.plg` and `Debug/` were already listed but still tracked; their files were
untracked too. ~50 build artifacts (`.sbr`, `.pdb`, `.bsc`, `.opt`, `.plg`,
`.idb`) removed from version control.

---

## 2. I/O + model layer

**Files:** `modsim/io/readers.py`, `modsim/io/writers.py`,
`modsim/models/job.py`, `modsim/tests/test_job_io.py`

### A1 — `.scd` parser (CRITICAL)
`.gcd` and `.scd` were parsed with the same layout, but `.scd` has **no bounds
line**. `.gcd` blocks are `index / nmin nmax / count / values`; `.scd` blocks are
`index / count / values` (no bounds line).
- `_read_dist` gained a `has_bounds` flag: `read_gcd` → True, `read_scd` → False.
- `write_scd` no longer emits a bounds line when regenerating.

### A4 — `.cur` TYPE line misparsed (CRITICAL)
`TYPE 001 HFSU 015 001` was parsed as `in_stream=15, out_stream=1`. The engine
reads it as `TYPE <unit#> <model> <NoPARAM> <unitID>`.
- `CurUnit.in_stream` → `noparam`, `CurUnit.out_stream` → `unit_id`.
- `read_cur` reads exactly `NoPARAM` parameter tokens.

### A5 — `.cur` parameter-count mismatch (CRITICAL)
- `write_cur` writes `TYPE {number} {model} {noparam} {unit_id}` and emits
  exactly `noparam` params (pads with `0.0000E+0`, truncates as needed).

### A8 — case-sensitive lookups (MEDIUM)
- `read_job_directory` now builds a case-insensitive filename map from a single
  `os.listdir`, so `Bougainville.JOB/.TEA/.TRN` resolve on Linux.

**Tests added:** `.scd` with ranges, zero-water `.TRN` round-trip, `NoPARAM`
parsed as param count.

---

## 3. Dialog / schema layer

**Files:** `modsim/gui/dialogs/schema.py`,
`modsim/gui/dialogs/generator.py`, `modsim/tests/test_equipment_dialogs.py`

### A5 — `write_values_to_job` keeps `NoPARAM` authoritative
- Formatted param list is truncated/padded to `unit.noparam`, so the written
  `.cur` never carries a value count different from `NoPARAM`.
- `load_values_from_job` maps only the first `noparam` fields.

### A6 — schema defaults violating their own bounds (HIGH)
71 out-of-bounds defaults corrected to in-bounds values. Percent-encoded legacy
defaults converted to fractions (e.g. `solids_fraction` 20 → 0.20,
`motor_efficiency` 35 → 0.35); dimensional values given defensible in-bounds
defaults (e.g. `trunnion_diameter` 96 → 0.96 m, `shell_thickness` 1.17 →
0.017 m, `KYNC.depth` 0.01 → 3.0 m). Bounds left unchanged. **71 → 0
violations.**

### A7 — GMSU `float(None)` crash (HIGH)
13 GMSU flags (`trunnion_bearing`, `lubrication_type`, `cooling_type`,
`drive_type`, `gear_type`, `shell_material`, `liner_material`, `ball_material`,
`feed_type`, `discharge_type`, `control_mode`, `alarm_level`, `shutdown_level`)
were FLOAT with `default=None`. Converted to INT (`min=0, max=20, default=0`).
No numeric field in any schema has a `None` default.

**Tests added:** no `None` numeric defaults, all defaults within bounds, GMSU
flags are INT, `write_values_to_job` pads/truncates to `noparam` and does not
raise on a real GMSU job.

---

## 4. Canvas / layout layer

**Files:** `modsim/gui/canvas/trn_layout.py`,
`modsim/gui/canvas/flowsheet_window.py`, `modsim/gui/canvas/canvas.py`,
`modsim/tests/test_flowsheet_canvas.py`

### A2 — zero-water `.TRN` corruption (CRITICAL)
The regenerator unconditionally wrote a water-flags line; the parser only
skipped it when `water_count > 0`.
- `TrnLayout` gained `water_flags`; the line is captured on parse and emitted on
  regenerate **only** when `water_count > 0`.
- `Cone.TRN` (water=0) now round-trips 38→38 lines; `Bougainville.TRN`
  (water=2) round-trips 62→62.

### A10 — unit/stream numbering & layout persistence (MEDIUM)
- `auto_arrange(placed=...)` only positions units **without** a saved TRN
  coordinate, so TRN-placed units are never moved/overlapped.
- New palette units get a valid TRN type code in 1–100 via `_next_type_code()`
  (skipping 6/62, which carry mandatory extra lines), and a non-feed `.syd`
  type (`_next_syd_type`), instead of type-0 / feed type 1.
- `_sync_trn` deduplicates stream records by `Str_ID` (legacy TRN IDs must be
  unique); deleted units are dropped on save.

### B1 — equipment dialog unreachable (CRITICAL)
- `canvas.py`: new `unit_edit_requested` signal; double-click opens the
  equipment dialog when the unit has a model code (rename fallback otherwise);
  right-click context menu (Change model parameters / Rename / Delete) with
  Delete wired to the previously-dead `remove_unit`.
- `flowsheet_window.py`: `unit_edit_requested` → `edit_equipment(job,
  unit.number)` (guarded by `.cur` presence), syncing the layout on success.
  Renames persist in-memory.

**Tests added:** zero-water & water round-trips, new-unit type codes, auto-arrange
preserving saved positions, double-click edit signal, rename fallback,
context-menu + Delete wiring.

---

## 5. Results + shell layer

**Files:** `modsim/gui/plotting/parser.py`, `modsim/gui/main_window.py`,
`modsim/tests/test_results_graphing.py`

### A3 — results parser missing per-stream size data (HIGH)
The parser only handled `OPGRAPH.DAT`/`STREAMPROPS.TXT`; the engine also writes
per-stream size data to `OPDISP.DAT` (plus `PHC.OUT`, `PHO1.OUT`, `REPORT.DAT`,
`LIBDISPM.DAT`). VB6 reads `OPGRAPH.DAT` (`OPGRAPH.BAS`); `OPDISP.DAT` is the
engine's calc→SIMOP transfer file carrying per-stream differential class masses.
- Added `_parse_opdisp` (registered first) for the engine's per-stream size-data
  file: parses the header, representative sizes, and per-stream differential
  class masses, converting them to cumulative % passing; populates
  `SizeDistribution` and `StreamData` (flows, % solids, yield).
- Existing `OPGRAPH.DAT`/`STREAMPROPS.TXT`/`LIBDISPM.DAT` parsers untouched.

Result: `parse_results` now yields **11 size distributions and 20 streams**
(previously 0 size distributions, 0 streams).

### A9 — `new_job` → Save wrote nothing (MEDIUM)
- `new_job` now attaches default `JobFlagsFile` + `SydFile`, so
  `write_job_directory` writes a real `Untitled.JOB` + `Untitled.syd`.

### A11 — engine return codes discarded (LOW)
- `run_simulation` captures `inordcalc` and `simop` results; non-`-1` results
  set `SIMULATION_ERROR` and show a mapped message (34 = singular convergence
  matrix, plus 36/37/13/2/3/7/8 and generic fallback). Only proceeds to results
  viewing and `SIMULATION_COMPLETE` on success (`-1`).

**Tests added:** `OPDISP.DAT` parsed into size distributions + stream data.

---

## Not addressed (out of scope for this pass)

Per the agreed scope, the remaining Section B feature gaps were deferred,
including: B2 (HFSU/CONE model mislabeling), B3 (data editors), B4 (delete/undo
— Delete wiring done but no full undo), B5 (full ~51 model forms), B6 (`.PAK`
support), B7–B9 (job flow/lifecycle), B10 (results view gaps).
