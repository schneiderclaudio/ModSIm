# ModSIM Job Execution & File I/O Contract

Describes the PySide6 GUI (`Modsim/Modsimpy/modsim`) ↔ Fortran engine
(`Modsim/Modsimdl`) job workflow: which files the GUI reads/writes and which
files the engine reads/writes, in what order.

There are **two distinct file layers**:

1. **Job files** — `<name>.<ext>` definition files that the GUI round-trips
   (read on open, written on save). These are the "source of truth".
2. **Run-time files** — derived inputs the GUI writes into the job directory
   immediately before calling the engine, plus everything the engine reads and
   writes during a run. These live in the **job directory** and are (mostly)
   transient.

---

## 1. High-level flow

```
 Open job            Save job               Run simulation (main_window.run_simulation)
  │                    │                        │
  │ parse <name>.*     │ write <name>.*         ├─ write_job_directory()   → all job files
  ▼                    ▼                        ├─ write_tears_out()       → TEARS.OUT
read_job_directory   write_job_directory       ├─ write_datt_dat()        → DATT.DAT
(readers.py)         (writers.py)              │
  │                    │                        ├─ engine.inordcalc(path)  → data input
  │                    │                        ├─ engine.simop(path)      → output phase
  │                    │                        └─ parse_results(path)     → results viewer
  │                    │
  └── GUI edits .cur params etc. → clears raw_lines (regenerated on write)
```

---

## 2. Job files — GUI read / write (round-trip)

Each file has a reader in `modsim/io/readers.py` and a writer in
`modsim/io/writers.py`. Writers emit `raw_lines` **verbatim** when present
(always after a read → byte-identical round trip); otherwise they regenerate
from the structured model.

| Ext      | Reader                    | Writer                    | Contents                                      | Regenerable from structured data? |
|----------|---------------------------|---------------------------|-----------------------------------------------|-----------------------------------|
| `.JOB`   | `read_job`                | `write_job`               | job name + run flags (`"Key",#BOOL#`)         | yes                               |
| `.syd`   | `read_syd`                | `write_syd`               | unit list (`num type kind in out`)            | yes                               |
| `.siz`   | `read_siz`                | `write_siz`               | stream size distributions / feedrates         | yes                               |
| `.gcd`   | `read_gcd`                | `write_gcd`               | grade-class distributions (ranges + bounds)   | yes                               |
| `.scd`   | `read_scd`                | `write_scd`               | S-class distributions (ranges, no bounds)     | yes                               |
| `.mat`   | `read_mat`                | `write_mat`               | cryptic flag string                           | yes                               |
| `.mop`   | `read_mop`                | `write_mop`               | cryptic flag string                           | yes                               |
| `.TEA`   | `read_tea`                | `write_tea`               | stream/mineral counts                         | **no** — raw_lines required       |
| `.sid`   | `read_sid`                | `write_sid`               | system/plant/run data (job def + model codes) | **no** — raw_lines required       |
| `.cur`   | `read_cur`                | `write_cur`               | unit parameters (`TYPE` records + `OUTC`)     | yes                               |
| `.TRN`   | `read_trn`                | `write_trn`               | flowsheet layout                              | **no** — raw_lines required       |

`read_job_directory` (readers.py:356) infers the job base name from the single
`.JOB` file and parses only the file types present; absent files are skipped.
`write_job_directory` (writers.py:247) writes every file present on the `Job`.

A job need not contain every file type. New jobs are seeded with `.JOB` +
`.syd` only (main_window.py:279).

---

## 3. Run time — what the GUI writes before invoking the engine

`run_simulation` (main_window.py:393) requires an open job with a saved
directory; it first persists everything, then runs the engine:

1. `write_job_directory(job, job_path)` (main_window.py:418) — persists all
   job files so the engine reads the latest edits.
2. `write_tears_out(job, job_path)` (main_window.py:421) — writes
   **`TEARS.OUT`** = a copy of the job's `.TEA` **only if** the job has one
   (writers.py:260). Mirrors legacy VB6 `FileCopy ... & ".TEA", "TEARS.OUT"`.
   Only consumed by the engine when `DATT.DAT` carries a `TEAR` keyword.
3. `write_datt_dat(job, job_path)` (main_window.py:425) — writes
   **`DATT.DAT`** = the job's `.sid` raw lines concatenated with the `.cur`
   portion (raw lines if present, else `_render_cur_lines`, writers.py:278).
   Mirrors legacy VB6 `FileConCat(CURRDATA.SYD, CURRDATA.RUN)` (`SYSDAT.BAS`).
4. `engine.inordcalc(job_path)` (main_window.py:437) — data input + ordering +
   calculation + report phases; see §4/§5.
5. `engine.simop(job_path)` (main_window.py:438) — output phase; reads
   `OPDISP.DAT` produced by INORDCALC.
6. `parse_results(job_path)` (main_window.py:484) → results viewer; see §6.

Only files generated here are the **derived inputs**: `DATT.DAT`, `TEARS.OUT`.
Everything else the engine reads is produced by the engine itself between the
two calls.

---

## 4. Engine input files (what must exist for a run)

| File       | Phase        | Required? | Consumed by (engine source)                     | Produced by                            |
|------------|--------------|-----------|-------------------------------------------------|----------------------------------------|
| `DATT.DAT` | data input   | **yes**   | `DMINP`, DATAINPT.FOR:108 (`OPEN(IR...DATT.DAT'`) | GUI `write_datt_dat`                  |
| `TEARS.OUT`| data input   | only if `TEAR` keyword | DATAINPT.FOR:533 (`OPEN(IT...'TEARS.OUT'`)        | GUI `write_tears_out` (== job `.TEA`); engine also appends tears each iteration |
| `OPDISP.DAT`| output phase | **yes**   | `SIMOP`, SIMOP.FOR:88                            | INORDCALC calculation phase, CALC.FOR:128 |
| `SIMOP.DAT`| output phase | optional (missing ⇒ defaults) | SIMOP.FOR:256                            | legacy VB6                              |
| `FORMAT.OUT`| output phase| optional (missing ⇒ auto-created empty ⇒ defaults) | SIMOP.FOR:301                     | legacy VB6 `Outform.frm`                |
| `Repeat.out`| output phase| optional (cumulative only)                       | SIMOP.FOR:379                             | engine / VB6                            |
| `CURRDATA.SYD` | liberation models | only for LJUBAMD/BETAAMD | BETAAMD.FOR:47, LJUBAMD.FOR:40 | legacy flow (copied `.sid`) |
| `LJUBAMD.DAT`/`BETAAMD.DAT` | liberation | produced by engine | MODELGRP.FOR:515-517, MILLMODS.FOR | engine |

> **SIMOP.DAT / FORMAT.OUT are optional.** With default Fortran
> `OPEN ... STATUS='UNKNOWN'`, a missing `FORMAT.OUT` is created empty, its
> read hits EOF (`END=9012`, SIMOP.FOR:313) and SIMOP continues with default
> output settings. A missing `SIMOP.DAT` likewise falls through to defaults
> (SIMOP.FOR:261-266). **`OPDISP.DAT` is the one SIMOP input that is truly
> required** — a missing file returns code `1` (SIMOP.FOR:90-93). Because the
> calculation phase writes `OPDISP.DAT` during `INORDCALC`, the GUI must call
> `inordcalc` before `simop` (it does).

---

## 5. Engine outputs

### 5a. `INORDCALC` (entry `INORDCALC`, SIMULATE.FOR)

`INORDCALC` drives four phases, keeping `DIAGDLL.TXT` open throughout.

| File | Phase / source line | Notes |
|------|---------------------|-------|
| `DIAGDLL.TXT` | SIMULATE.FOR:33 | overall diagnostic log; GUI cites it on failure |
| `PHI1.ECH` | DATAINPT.FOR:113 | data echo |
| `PHI3.DAT` | DATAINPT.FOR:568 | written by data input; **read** by ordering (ORDER.FOR:85) |
| `PHI4.DAT` | DATAINPT.FOR:357 | written by data input |
| `PHO1.OUT` | ORDER.FOR:87 | ordering output |
| `PHO3.DAT` | ORDER.FOR:89 | written by ordering; **read** by calc |
| `OPDISP.DAT` | CALC.FOR:128 | per-stream flows + differential size-class masses; primary results file (`parse_results` reads it) |
| `REPORT.DAT` | CALC.FOR:132 | written by calc; **read** by report writer (REPWRT.FOR:70) |
| `REPORT1.OUT`, `REPORT2.OUT`, `MODELGRP.OUT` | REPWRT.FOR:52-62 | human-readable reports |
| `TEARS.OUT` | CALC.FOR:318/924/1061, CALC1.FOR:433 | **appended** each calc iteration |
| `diagljub.txt`, `LJUBAMD.DAT` | LJUBAMD.FOR | liberation model diagnostics/data |
| `diagbeta.txt`, `BETAAMD.DAT` | BETAAMD.FOR | liberation model diagnostics/data |

`PHI*.DAT`/`PHO*.OUT` are internal hand-off files between phases; nothing else
reads them. `REPORT2.OUT` is also appended to by `UserModels/Utilities.f90:32`.

### 5b. `SIMOP` (entry `SIMOP`, SIMOP.FOR — "program SIMOP1", the output module)

| File | Source line | Notes |
|------|-------------|-------|
| `DiagSIMOP.txt` | SIMOP.FOR:81 | output-phase diagnostic; GUI cites it on failure |
| `SIM.OUT` | SIMOP.FOR:412 | detailed per-stream output |
| `CumData.out` | SIMOP.FOR:453/581 | cumulative output (only when `CumOut`>0; GUI passes 0) |
| `Flydata.out` | SIMOP.FOR:447 | flyash/flydata details |
| `STREAMPROPS.TXT` | SIMOP.FOR:589 | per-stream properties (parsed by GUI) |
| `OPGRAPH.DAT` | SIMOP.FOR:810 | graphical size-distribution data (parsed by GUI) |
| `LIBDISP.DAT` / `LIBDISPM.DAT` | SIMOP.FOR:848/900/979 | liberation display (parsed by GUI) |

---

## 6. Result files the GUI reads after a run

`parse_results(job_dir)` (plotting/parser.py:693) scans **every** `.OUT`/`.DAT`/
`.TXT` file in the job directory, runs format detectors, and skips anything it
cannot parse (binary/unrecognised) without raising. Recognised layouts:

| Detector | File(s) | Data extracted |
|----------|---------|----------------|
| `_parse_opdisp` | `OPDISP.DAT` | per-stream solids/water flow, % solids, yield; size distributions (differential class masses → cumulative % passing) |
| `_parse_streamprops` | `STREAMPROPS.TXT` | solids/water/slurry flow, % solids, yield; size distributions |
| `_parse_opgraph` | `OPGRAPH.DAT` | size distributions (cumulative passing stored 0–1, ×100) |
| `_parse_libdisp` | `LIBDISPM.DAT` | liberation spectra (unconditional / conditional-on-size) |

---

## 7. Return codes

The engine returns **`-1` on success** (`INORDCALC = -1`, SIMULATE.FOR:28).
`INORDCALC` otherwise returns the offending phase's `ExitValue` (`>1` = fatal);
`SIMOP` returns phase/file-I/O codes. The GUI maps codes to messages in
`modsim/gui/main_window.py:48` (`_SIMULATION_ERROR_MESSAGES`):

- `1..3, 7, 8` — SIMOP file open/read/end-of-file (OPDISP.DAT, FORMAT.OUT)
- `10..13` — data-input phase file/read errors (system data, plant/run data,
  tear data)
- `16..18` — flowsheet/stream structural errors
- `20..22` — ordering phase errors
- `34` — singular convergence matrix (usually model parameters / CONVERGENCE
  settings)
- `35` — impossible output code in calculation
- `36` — unit failed solids/water mass balance
- `37` — iteration limit exceeded (RMS did not converge)
- `40..42` — report writer errors

On failure the GUI points the user at `DIAGDLL.TXT` / `DiagSIMOP.txt`.

---

## 8. Critical invariants & gotchas

- **`DATT.DAT` is the engine's authoritative run input, not the `.cur` on disk.**
  After a parameter-edit dialog clears `cur.raw_lines`, `write_datt_dat` must
  regenerate the `.cur` portion from the structured model — before the fix it
  silently skipped, so the engine read a stale `DATT.DAT` (singular matrix,
  exit 34). See writers.py:278.
- **`.sid` is never regenerated** — `write_sid` raises unless `raw_lines` are
  present (writers.py:224). If `sid.raw_lines` is `None`, `write_datt_dat`
  refuses to write (nothing authoritative to concatenate).
- **`.cur` parameter count must be exact.** The engine reads exactly
  `NoPARAM` values (`Read(31,*) ... (Param(I),I=1,NoPARAM)`); the writer pads
  with zeros / truncates to `noparam` and emits 8 values per line
  (writers.py:186-196). `TYPE` records are
  `TYPE {num} {model} {noparam} {unit_id}` (space-separated).
- **`OPDISP.DAT` is written by `INORDCALC`'s calculation phase and consumed by
  `SIMOP`** — the GUI must call `inordcalc` before `simop`, always.
- **Engine paths are `CHARACTER*255`**, passed as a 255-byte buffer padded
  with ASCII spaces and carrying a trailing separator — never NUL-terminated
  (engine_bridge.py `encode_path`). `LEN_TRIM` strips trailing spaces but not
  NUL bytes.
- **Text files are CRLF + ASCII**, floats in Fortran `E`-notation with four
  decimals and a signed exponent (`1.2780E+2`; writers.py `_fmt_e`); `_write_lines`
  writes `\r\n`.
- **Engine global state is per-process.** Running a second job in the same
  process can trigger an access violation / cross-job contamination (observed
  with `Bougainville` after a prior in-process run) even though each job runs
  fine in isolation — diagnose engine failures against a fresh process.
- **`.TEA`/`.sid`/`.TRN` regenerators are not implemented** — they require
  `raw_lines`; programmatic edits to those files are not supported.

---

## 9. File-format contract (writers/readers)

| Token | Meaning |
|-------|---------|
| `TYPE n model np id` | `.cur` unit header: number, model code, noparam (parameter count), unit id |
| `STOP` / `OUTC` | `.cur` terminators (OUTC section precedes STOP) |
| `Stream a b label` | stream block header in `.siz`/`.gcd`/`.scd` |
| `System data`/`Plant data`/`Run data` | `.sid` section markers |
| `"Job name","<name>"`, `"Key",#TRUE#` | `.JOB` CSV-ish records |
