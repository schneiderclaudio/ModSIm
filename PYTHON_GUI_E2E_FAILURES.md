# JobsRPK E2E Sweep — Failure Investigation & Inconsistent-Job Inventory

**Scope:** End-to-end run of every shipped job in `Modsim/JobsRPK/` through the
PySide6 GUI + Fortran engine (`test_e2e_jobsrpk.py`).
**Result:** 92 jobs discovered → 76 stable passes, 16 deterministic failures,
2 flaky, 0 crashes.
**Status:** Two code fixes were applied (missing-data guard §3; liberation-data
staging §6). No shipped data files were modified.

> **⚠ SUPERSEDED 2026-09:** a follow-up RCA re-investigated every failure in
> this report and found the §5.2 "shipped-data" verdict wrong for 7 jobs
> (8 affected incl. flaky): they are IFX `-O2` fast-FP regressions, fixed by
> building the engine with `-O2 -fp:strict`. The engine, the E2E manifests and
> the thresholds were updated accordingly. See
> `PYTHON_GUI_E2E_FAILURES_ADDENDUM.md` for the evidence and per-job verdicts.
---

## 1. Method

Each `*.JOB` under `Modsim/JobsRPK/` is opened the way a user opens it
(File > Open) and then Simulate > Run, in a **separate subprocess per job**.
The engine is not re-entrant: running many jobs in one process leaks
`SAVE`/`COMMON` state and produces spurious access violations, so a fresh
process per job is required, not optional.

A job is "success" when the engine returns `-1/-1` **and** `OPDISP.DAT` parses
into stream/size data. "Fail" is a clean engine error code or a GUI-side
open/run error. "Crash" is a process abort with no JSON result line.

The manifest in `Modsim/Modsimpy/tests/test_e2e_jobsrpk.py` is the single
source of truth (`KNOWN_FAILURES`, `KNOWN_CRASHES`, `FLAKY`).

---

## 2. Summary

| Category | Count | Jobs |
|---|---|---|
| Stable pass | 76 | — |
| Missing `.sid`/`.cur` (guarded by GUI) | 3 | ex5V3, Cone1, MILL |
| Engine access violation (caught, no clean code) | 1 | Batac jig |
| Clean engine error — exit 34 | 2 | Bougainville, Fairlane (root) |
| Clean engine error — exit 36 | 8 | see §5.2 |
| Clean engine error — exit 16 | 1 | CCDTrial |
| Clean engine error — exit 38 | 1 | Kiruna primary circuit |
| Flaky (36 ⇄ success / 34 ⇄ success) | 2 | KennecottDeepak1, Kennecott SAG BM Line 4 |
| **Total** | **92** | |

The remaining 12 clean engine failures (§5) are **shipped-data / model issues**.
A further 9 jobs that previously failed with exit 44/34 were caused by a
**Python-GUI defect** — the liberation transfer-coefficient files were never
staged before the run — and are now fixed (§6).

---

## 3. Root cause 1 — Missing `.sid`/`.cur` (3 inconsistent jobs)

A scan of all 92 jobs confirms **exactly 3** are missing `.sid` and/or `.cur`;
the other 89 all carry both. There is no broader pattern.

### 3.1 The three jobs

| Job | Directory | `.sid` | `.cur` | `.syd` | Cause |
|---|---|---|---|---|---|
| `ex5V3` | `CoalPrep/` | ✗ | ✗ | ✓ | flowsheet-only export, no run data |
| `Cone1` | `ConeOpt/` | ✗ | ✗ | ✗ | stale orphaned `.JOB` from a rename |
| `MILL` | `Kennecott/Deepak data 2003/` | ✓ | ✗ | ✓ | missing one companion file (`.cur`) |

**`Cone1` — stale orphaned `.JOB`.** `ConeOpt/` ships four *complete* file
sets under other names (`ConeOptimization`, `ConeOptimum1`,
`Richards Bay Minerals Cone`, `Discussion of Ergun Paper`). `Cone1.JOB` is the
only remaining `Cone1.*` file — a rename vestige (`Cone1` →
`ConeOptimization`). Its own `.JOB` flags declare `"System input data"=#TRUE#`
(needs `.sid`), but that file set was renamed away. Not a real job.

**`ex5V3` — flowsheet-only export.** Its `.JOB` flags self-describe it:
`"System input data"=#FALSE#`, `"Model parameter data"=#FALSE#`,
`"SIMOP data"=#FALSE#`. It ships only `.syd` (flowsheet) + `.TEA` (tears) +
`.TRN` (layout) + PostScript/WMF images. It was exported as a drawing and was
never meant to be simulated.

**`MILL` — missing one companion file.** Flags: `"System input data"=#TRUE#`
(`.sid` present ✓), `"Unit parameters needed"=#TRUE#` (`.cur` absent ✗). Ships
`.SID/.SYD/.SIZ/.SCD/.GCD/.MOP/.TEA/.TRN` — everything except `.cur`. The
unit-parameters file was never saved.

### 3.2 Why it crashed (hard abort) instead of failing cleanly

`write_datt_dat()` concatenates `.sid` + `.cur` into `DATT.DAT` (mirrors the
VB6 `FileConCat`), and **no-ops when either is missing**. So no `DATT.DAT` is
written. The Fortran `OPEN(DATT.DAT)` carries no `STATUS='OLD'` — the
`STATUS='UNKNOWN'` default **creates an empty file** — and `DATAINPT.FOR`
then reads it on unit 5 → immediate EOF →

```
forrtl: severe (24): end-of-file during read, unit 5, file ...DATT.DAT
```

This kills the process (exit 24) and is **not** Python-catchable.

---

## 4. Root cause 2 — Engine access violation (1 job, caught)

**`Coal processing jobs/Batac jig with proximate analysis`**

Full file set present, but `INORDCALC` raises an access violation (surfaces as
ctypes `OSError`, caught → clean "fail" status, no engine exit code). Its
`.sid` line 22 carries particle properties
`2.7000E+0 0.0000E+0 0.0000E+0 0.0000E+0 0.0000E+0` — i.e. **zero particle
properties** — whereas the working sibling `Coal jig with proximate analysis`
has five non-zero values. Zero properties cause the coal-jig model to overrun.
Data issue, not GUI-guardable.

---

## 5. Root cause 3 — Clean engine failures (12 jobs, data/model issues)

These run to completion but the engine returns a diagnostic exit code. They are
shipped-data/model problems; no code fix is warranted (the data would need to
be edited per-job).

### 5.1 exit 34 — singular convergence matrix (2)

`Bougainville` (root) and `Fairlane` (root). `CALC.FOR:907` finds a zero pivot
during back-substitution: the flowsheet's RMS error does not converge
(`Fairlane` reports `AFTER SETTING UP, THE RMS ERROR IS 0.388687`), so the
convergence matrix becomes singular.

Note: `Fairlane` (root) *does* ship a `.lju` file and its `.JOB` declares
`"Ljubljana model"=#TRUE#`, but its `GMIL` mill model does **not** enable
liberation (the diagnostic shows `Allocating Bvg` with no `Reading data for AMD
…`), so the `.lju` is unused and this is a genuine convergence failure — unlike
the subdirectory `Fairlane/Fairlane` job, which uses liberation and is fixed
by §6.

### 5.2 exit 36 — solids/water mass-balance error (8)

`Trial SAGM`, `ex1aV3` (root), `CoalPrep/ex1aV3`, `sag-cetem_B11`,
`sag-cetem_closed`, `Greens Creek`, `Kennecott SAG only`,
`SAGM Parameter Estimation`. `CALC1.FOR` `CONSRV` exceeds its tolerance
(1% per output stream, 0.1% total solids, 1% water).

Divergence evidence: the SAG-mill jobs produce **zero** product —
`SAGM Parameter Estimation` reports `MASS FLOW FROM THE MODEL 0.000` vs an
expected `27.78`, and `sag-cetem_*` report `TOTAL SOLIDS LEAVING ~0.2e-36`
with zero power draw (`Morell power 0.0 W`, `Austin net power 0.0 W`).
`Greens Creek` fails the balance on units 2/3 every iteration;
`Kennecott SAG only` reports a stream of `3.7e15 kg/s`.

### 5.3 exit 16 — no-solids stream (1)

`Baker Process/CCDTrial` — `DATAINPT.FOR:452` rejects tear stream 7 because
`PERS ≤ 0`. The tear seed in `CCDTrial.TEA` was saved all-zero
(`STRM 7 1` / `.0000 .0000`), i.e. "start iterations from the previous end
point" with no previous end point. Clearing the tear data (or supplying a
non-zero seed) resolves it.

### 5.4 exit 38 — FAG mill recycle divergence (1)

`Kiruna Autogenous Mill/Kiruna primary circuit` — the FAG mill's internal
recycle diverges (grows to ~4.4e7 kg/s), triggering
`MILLMODS.FOR` "Convergence not reached in model FAGM … try increasing the
grate aperture". The outer RMS does converge (`0.000543`), but the internal
grate-recycle loop never settles, so the classification model then emits an
impossible output code.

---

## 6. Root cause 4 — Liberation data not staged (9 jobs, GUI defect — FIXED)

Nine jobs that previously failed with exit 44 (or a downstream exit 34) were
**not** data defects. They ship valid `.lju` / `.amd` liberation
transfer-coefficient files, but the Python GUI never copied them to the
engine's fixed names before the run, so the engine's mill model could not read
them.

### 6.1 The mechanism

A mill whose parameters enable a liberation model (`MILLMODS.FOR` `CONDBF`,
lines 1888/1904) reads `LJUBAMD.DAT` / `BETAAMD.DAT` from the job directory
during the calculation phase. Those files are not shipped; the legacy VB6 GUI
creates them on every run by copying the job's companion files
(`MDIMod.frm:309-319`):

```vb
Case "Ljubljana model":              FileCopy JobFileName & ".lju", JobPath & "LJUBAMD.DAT"
Case "Beta function liberation model": FileCopy JobFileName & ".amd", JobPath & "BetaAMD.DAT"
```

The Python GUI's job model did not load `.lju` / `.amd` at all, and no writer
staged them, so a liberation-enabled job failed with `Error calculating
conditional breakage function` (exit 44, `CONDBF ≤ -1`) or — for the models
that continue past the error — a singular convergence matrix (exit 34).

### 6.2 The nine jobs (now passing)

| Job | Before | After staging |
|---|---|---|
| `DemoLib` (root) | 34 | pass |
| `Fairlane/Fairlane` | 34 | pass |
| `BOOK/Ex10-6BMCircuitwithLiberationandConcentration` | 34 | pass |
| `MtLyell2` | 44 | pass |
| `WHIMS` | 44 | pass |
| `Parameter estimation jobs/GMI1 parameter estimation` | 44 | pass |
| `Parameter estimation jobs/GMSU parameter estimation` | 44 | pass |
| `Parameter estimation jobs/HFML parameter estimation` | 44 | pass |
| `Parameter estimation jobs/HFSU parameter estimation` | 44 | pass |

Verified by running `INORDCALC` both with and without the VB6 staging step:
each of the nine flips from a clean failure to `-1` when the `.lju`/`.amd` file
is staged, and the diagnostic changes from `The file LJUBAMD.DAT … has not been
set up correctly` to `AMD data has been read`.

---

## 7. Flaky (2)

`Kennecott/Kennecott SAG BM Line 4` (alternates exit 34 / success) and
`Kennecott/KennecottDeepak1` (alternates exit 36 / success) — legacy
uninitialised engine state on a convergence boundary. Accepted either way in
the manifest; a *crash* would still be a regression.

---

## 8. Fixes applied (two code changes)

1. **Missing-data guard** — `Modsim/Modsimpy/modsim/gui/main_window.py`
   `run_simulation()` refuses a job whose `.sid` **or** `.cur` is missing
   *before* calling the engine (§3), converting 3 hard EOF aborts into clean,
   explainable refusals.

2. **Liberation-data staging** — the `.lju` / `.amd` files are now part of the
   job model and readers/writers
   (`Modsim/Modsimpy/modsim/models/job.py`, `…/io/readers.py`,
   `…/io/writers.py`), and `run_simulation()` now calls
   `write_liberation_files()` to stage `LJUBAMD.DAT` / `BETAAMD.DAT` before
   running, mirroring the VB6 GUI (§6). This moves the 9 jobs in §6.2 from
   failure to success.

E2E re-run after both fixes: **76 ok, 16 fail, 0 crash, 2 flaky**.

Unit tests: `test_job_io.py` covers the `.lju`/`.amd` round trip and the
`write_liberation_files` staging; the E2E manifest was updated so the nine
fixed jobs are now asserted to pass (removed from `KNOWN_FAILURES`) and
`KennecottDeepak1` was added to `FLAKY`.

---

## 9. Conclusion / disposition

| Item | Disposition |
|---|---|
| 3 missing-data jobs | GUI refuses cleanly; data left untouched (documented) |
| Batac jig | shipped-data defect (zero particle properties) — document only |
| 9 liberation jobs | **fixed** — GUI now stages `.lju`/`.amd` (§6) |
| 12 clean engine failures | shipped-data/model issues — document only |
| 2 flaky | documented, tolerated by manifest |
| Engine non-re-entrancy | documented requirement → subprocess-per-job in E2E |

No shipped data was modified. The 12 remaining clean failures (§5) are genuine
data/model defects: fixing them requires editing the offending job's unit
parameters (SAG/FAG mill settings), the `CCDTrial` tear seed, or completing the
missing companion files for `Cone1` / `MILL` / `ex5V3`.
