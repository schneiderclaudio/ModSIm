# PySide6 GUI Audit — Findings vs VB6 Reference

**Date:** 2026-08-14
**Scope:** `Modsim/Modsimpy/` (PySide6 rewrite, ~6,700 LOC) compared against the legacy VB6 GUI (`Modsim/Modsimvb/`) and the Fortran engine sources.
**Method:** Direct code inspection, headless execution of the GUI and the engine bridge, and four parallel VB6-comparison review lanes.
**Status:** No files were modified during the audit.

All "verified" items were reproduced or confirmed by running the code. Items marked "(review)" come from the VB6 comparison review lanes.

---

## A. Bugs verified by direct execution

### A1. CRITICAL — 10 of 45 shipped jobs fail to open (`.scd` parser bug)
- **Where:** `Modsim/Modsimpy/modsim/io/readers.py` — `_read_dist` (~line 205-245)
- **What:** `.gcd` and `.scd` are parsed with the same layout (`index / bounds / nmin / values`), but the real `.scd` format has **no bounds line** (`index / nmin / values`). The parser reads the value line where it expects the integer `nmin`.
- **Repro:** `read_job_directory` raises `ValueError: invalid literal for int() with base 10: '1.0000E+0 0.0000E+0'` for: `COLUMN`, `Jones`, `MtLyell`, `MtLyell2`, `Flotationbank`, `floatbank`, `Problem 14`, `ELISTA05`, `fodfertil1`, `Distribution jobs/Flotationbank`.
- **Impact:** those jobs cannot be opened at all. The round-trip tests only cover jobs with 0 S-ranges, so the bug is uncaught.

### A2. CRITICAL — Saving any job with zero water streams corrupts its `.TRN`
- **Where:** `Modsim/Modsimpy/modsim/gui/canvas/trn_layout.py` — `regenerate_trn_lines` (~line 277-278) vs `parse_trn_layout` (~line 174-175)
- **What:** the regenerator **unconditionally** writes a water-flags line (`"F"*stream_count`); the parser only skips that line when `water_count > 0`.
- **Repro:** `Cone.TRN` (water=0) round-trips 32 → 33 lines; re-parsing the regenerated file fails with `could not convert string to float: 'T'`.
- **Impact:** any edit (even moving a unit) triggers `sync_layout_to_job` → the saved `.TRN` is unreadable by both the Python GUI and legacy VB6.

### A3. HIGH — Results parser targets files the engine never produces
- **Where:** `Modsim/Modsimpy/modsim/gui/plotting/parser.py` — `_FORMAT_PARSERS` (STREAMPROPS.TXT / OPGRAPH.DAT / LIBDISPM.DAT)
- **What:** the engine writes `OPDISP.DAT`, `PHC.OUT`, `PHO1.OUT`, `REPORT.DAT`, `LIBDISPM.DAT` — not `STREAMPROPS.TXT`/`OPGRAPH.DAT` (those are VB6-side files). `OPDISP.DAT` (the per-stream size-data carrier) has no parser.
- **Repro:** after an end-to-end engine run, `parse_results` returns `0 size_distributions, 2 liberation_spectra, 0 streams` — only the liberation tab has data.
- **Impact:** the ResultsViewer's size-distribution and stream tabs are empty after a real run.

### A4. HIGH — `.cur` TYPE line misparsed (parameter count read as stream numbers)
- **Where:** `Modsim/Modsimpy/modsim/io/readers.py` — `read_cur` (~line 307-311)
- **What:** `TYPE 001 HFSU 015 001` is parsed as `in_stream=15, out_stream=1`. The Fortran engine (`Modsim/Modsimdl/ModsimCurveFitDll/Modelresponse.f90`, `SetModsimParameters`) reads it as `TYPE <unit#> <model> <NoPARAM> <unitID>` — `15` is the **parameter count**.
- **Impact:** byte-round-trips on write, but any consumer of `CurUnit.in_stream/out_stream` gets garbage; also the mechanism behind A5.

### A5. HIGH — Schema edits corrupt `.cur` files (parameter-count mismatch)
- **Where:** `Modsim/Modsimpy/modsim/gui/dialogs/generator.py` — `write_values_to_job` (~line 183-199); `Modsim/Modsimpy/modsim/io/writers.py` — `write_cur`
- **What:** rewriting `unit.params` to the schema field count and clearing `raw_lines` makes the regenerated file carry a different value count than the `NoPARAM` field. The engine reads exactly `NoPARAM` values (`Read(31,*)(Param(I),I=1,NoPARAM)`).
- **Verified mismatches in real jobs:** HFSU 22-schema vs 15-param files (`Bougainville.cur`), GMSU 40 vs 43, SAGM 21 vs 24, FLTN 9 vs 5/8.
- **Impact:** edited `.cur` files become unparseable by the engine.

### A6. HIGH — 71 schema defaults violate the schemas' own bounds
- **Where:** `Modsim/Modsimpy/modsim/gui/dialogs/schema.py`
- **Examples:** `SPLT.split_1` default 2.0 (max 1.0); `FLTN.pulp_level` 300 (max 10); `FLTN.solids_fraction` 2.5 (max 1.0); mill `ball_charge` 1.65 (max 0.6); `critical_speed` 2.0 (max 1.0); `solids_fraction` 20 (max 1.0); `mill_filling` 4.0 (max 1.0); `trunnion_diameter` 96 (max 5.0); `KYNC.depth` 0.01 (min 0.5).
- **Root cause:** defaults are engine-legacy values in engine units (% ball load, % solids) while bounds assume 0–1 fractions.
- **Impact:** `_on_accept` rejects out-of-bounds values, so a dialog **cannot be accepted with legitimate engineering values**.

### A7. HIGH — GMSU dialog crashes on accept (`float(None)`)
- **Where:** `Modsim/Modsimpy/modsim/gui/dialogs/schema.py` (~line 577-589); `generator.py` `_format_param`
- **What:** 13 GMSU fields (`trunnion_bearing`, `lubrication_type`, `cooling_type`, `drive_type`, `gear_type`, `shell_material`, `liner_material`, `ball_material`, `feed_type`, `discharge_type`, `control_mode`, `alarm_level`, `shutdown_level`) are FLOAT with `default=None`.
- **Repro:** `write_values_to_job` raises `TypeError: float() argument must be a string or a real number, not 'NoneType'`; `validate_value` flags them "Must be a number".
- **Note:** these are engine integer choice flags, not floats.

### A8. MEDIUM — Case-sensitive file lookups break on Linux
- **Where:** `Modsim/Modsimpy/modsim/io/readers.py` — `read_job_directory` looks up `f"{name}.{ext}"` with lowercase extensions
- **What:** real files are `Bougainville.JOB`, `.TEA`, `.TRN` (and many `.CUR`/`.GCD`/…). Fine on Windows; on a case-sensitive filesystem every job loses its `.JOB` (name/flags), `.TEA`, and `.TRN` (layout).
- **Impact:** cross-platform breakage (`make run` on Linux).

### A9. MEDIUM — `new_job` → Save writes nothing; New→Save→Run is broken
- **Where:** `Modsim/Modsimpy/modsim/gui/main_window.py` — `new_job`; `Modsim/Modsimpy/modsim/models/job.py` — `Job.files()`
- **What:** `Job(name="Untitled")` has zero file objects; `Job.files()` returns `[]`; `write_job_directory` only `makedirs`.
- **Repro:** verified — saving a new job produces an empty directory; Run then hits an empty job dir.

### A10. MEDIUM — Unit/stream numbering and layout persistence defects
- **Where:** `Modsim/Modsimpy/modsim/gui/canvas/flowsheet_window.py`, `trn_layout.py`, `canvas.py`
- Units in `.syd` but missing from `.TRN` are auto-placed into the grid; since `auto_arrange` only runs when *no* positions exist, they can overlap TRN-placed units (verified for Bougainville units 6–9).
- New palette-dropped units are written into a type-0 `.TRN` block and given `.syd type=1` (feed) — both wrong; the count matrix only covers types 1–100 so type-0 records are unreadable on reload.
- Merged/split streams render as independent curves and are saved as duplicate TRN records with the same Str_ID (TRN IDs are unique in the legacy format).

### A11. LOW — `run_simulation` discards engine return codes
- **Where:** `Modsim/Modsimpy/modsim/gui/main_window.py` — `run_simulation` (~line 369-372)
- **What:** `inordcalc`/`simop` return codes are ignored; the GUI reports `SIMULATION_COMPLETE` regardless.
- **Repro:** end-to-end run produced `inordcalc rc=34` (singular convergence matrix) and `simop rc=3`, yet the GUI would report "Complete".
- **VB6 reference:** maps codes 34/36/37/13/2/3/7/8 to specific error messages.

---

## B. Feature-surface gaps (VB6 comparison review)

### B1. CRITICAL — The equipment dialog is unreachable
- `edit_equipment` / `EquipmentDialog` (`dialogs/generator.py`) are exported but **never imported by any GUI module** (verified by grep).
- Double-clicking a unit only renames (`_edit_unit_label`); the rename isn't even persisted.
- VB6: right-click → "Change model parameters", "Select new model", report files.

### B2. CRITICAL — `HFSU` schema is the wrong model
- `schema.py` defines HFSU as a "High-Frequency Screen" with screen fields; in this codebase HFSU is the **Herbst-Fuerstenau ball mill** (per `MILLMODS.FOR`, VB6 `hfsu.frm`).
- The 22 fields map positionally onto the mill's PARAMs — "feed rate" is really mill diameter, "deck area" is % critical speed. Any edit writes screen-labeled values into mill parameters.
- Also flagged (review): `CONE` is actually a Reichert cone in `PLANT.DAT`, not a cone crusher.

### B3. HIGH — No data editors at all
- VB6 has `SYSDATA.FRM` (ore/system data + CONV/ITER/TOLM convergence settings — the dialog the engine itself recommends in the singular-matrix message), `GENERAL.FRM`, `Ostream.frm`/`Wstream.frm` (streams), `G_data/GC_data/SC_data/S_class/G_class`, `Washdata.frm`, `PSD.FRM`, `OUTFORMAT.FRM`.
- Python parses `.sid/.siz/.gcd/.scd` only as opaque `raw_lines`; none are editable or creatable. A brand-new job has no system data and nothing to populate it.

### B4. HIGH — No delete/undo, no right-click menus, no print/export, no re-fit
- `remove_unit` is dead code (no key-press/context-menu hook); streams can't be deleted, moved, or re-routed; only wheel-zoom + one initial fit.
- VB6: Edit→Delete tool, editable polyline corners, Print (PostScript/metafile export), zoom/locator cursors.

### B5. HIGH — Model coverage: ~51 VB6 parameter forms / ~80 model codes vs 23 schemas
- Units with unschematized models (screens, classifiers, thickeners, magnetic separators, pumps, sumps, RODM, WOCY, …) can't be edited at all — `get_schema` would `KeyError`.

### B6. HIGH — No `.PAK` packed-job support; Open is a directory picker
- **168 `.PAK` files exist in `Modsim/Jobs`** and the GUI cannot open any of them.
- VB6: `File → Existing Job` is a `*.job|*.PAK` file dialog with unpack support.
- The Python flow (directory picker → optional list dialog) also has no "Incorrect file type" validation and no title-bar path+jobname.

### B7. HIGH — Save/Run writes into the source directory with no overwrite prompt
- VB6 keeps `JobSavePath` (originals) separate from the temp `JobPath`, and prompts "already exists. Overwrite?".
- Python writes `TEARS.OUT`/`DATT.DAT` and result files into the opened directory — opening from the shared `Modsim/Jobs` root then Save/Run pollutes the originals.

### B8. HIGH — `.JOB` flags parsed but unused
- VB6 gates which companion files load per flag, and enforces "System data needed"/"Unit parameters needed" before run. Python reads every present file unconditionally.

### B9. MEDIUM — Lifecycle gaps
- No "Close this job" action; no unsaved-changes prompt on exit; no window list; opening a second job orphans the first window; no way to reopen the flowsheet for the current job after closing its window.

### B10. MEDIUM — Results view gaps
- No Output-Format editor (`OUTFORMAT.FRM` equivalent); no report-file viewer; no stream-properties text view; no SIZGRAPH-style input overlay/export; no LIBGRAPH input comparison; no washability curves; duplicate stream tabs when both OPGRAPH.DAT and STREAMPROPS.TXT are present.

---

## C. Relation to the reported bug ("no OK button to load the job")

The canvas **does** load units when the job parses (verified: Bougainville renders 9 units / 9 streams). The reported experience is explained by a combination of the findings above:

- Jobs like **Jones, MtLyell, COLUMN, Flotationbank, Problem 14, ELISTA05, fodfertil1** fail to open outright (A1).
- When a job *does* open there is **no confirmation step** (no VB6-style load flow, no dialog, no title-bar path+name) — the canvas just appears — and **nothing about the loaded job is editable**: double-click renames only (B1), there are no data editors (B3), Run discards engine errors (A11) and needs a save first, and results come back mostly empty (A3). The window therefore feels like "an empty canvas where you can drag elements" rather than "my job, loaded and editable."

---

## D. Suggested fix priorities

1. **Data-integrity first:**
   - A1 — `.scd` format variant (no bounds line)
   - A2 — conditional water-flags line in `regenerate_trn_lines`
   - A4/A5 — `.cur` NoPARAM handling (keep param count authoritative)
   - A6/A7 — regenerate schemas from the Fortran PARAM tables; fix bounds units
2. **Wire what exists:** connect `edit_equipment` to double-click/right-click on units; add delete/undo; make Run check engine return codes.
3. **Match VB6 job flow:** `*.job|*.PAK` file dialog, `.JOB`-flag gating, overwrite prompts, working-dir separation, title-bar path+name, close/unsaved prompts.
4. **Results:** parse `OPDISP.DAT`/`PHC.OUT` (formats the engine actually writes); add the Output-Format editor.
5. **Tests:** add coverage for `.scd` with ranges, zero-water `.TRN` round-trip, `NoPARAM`-mismatch `.cur`, and case-insensitive reads.
