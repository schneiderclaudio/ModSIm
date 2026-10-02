# ModSIM E2E — Failing Jobs Inventory

Consolidated list of every shipped job that currently **fails** the end-to-end
smoke suites. Two suites drive every job through the real PySide6 GUI + Fortran
engine (`File > Open` then `Simulate > Run`, one subprocess per job) and assert
a regression contract over the current shipped data:

- `Modsim/Modsimpy/tests/test_e2e_jobs.py` — `Modsim/Jobs/` (45 jobs)
- `Modsim/Modsimpy/tests/test_e2e_jobsrpk.py` — `Modsim/JobsRPK/` (92 jobs)

"Fail" = the engine returns a clean diagnostic exit code, or the GUI refuses to
run the job (missing data). "Crash" = a Fortran process abort with no clean
code — **none occur in either suite today**. Flaky jobs (which pass on some
runs) are listed in their own section at the end.

## Summary

| Suite | Jobs discovered | Failing | Passing | Flaky |
|---|---|---|---|---|
| `Modsim/Jobs/` | 45 | **14** | 31 | 0 |
| `Modsim/JobsRPK/` | 92 | **16** | 74 | 2 |
| **Total** | **137** | **30** | 105 | 2 |

## Engine exit-code reference

| Exit | Meaning (source) |
|---|---|
| 16 | No-solids stream — `PERS ≤ 0` (`DATAINPT.FOR:452`) |
| 32 | Error reading a file during the calculation phase (`CALC.FOR:1102`); diagnostic hints at tear-stream data |
| 34 | Singular convergence matrix — zero pivot (`CALC.FOR:907`) |
| 36 | Solids/water mass-balance error — `CONSRV` (`CALC1.FOR:237-274`) |
| 38 | Calculation-phase error in the classification / mill model (`CLASMODS.FOR` / `MILLMODS.FOR`) |
| — (no code) | GUI refuses to run: missing `.sid` (system) and/or `.cur` (run) data, or engine access violation |

---

## Failures by root cause

### Missing `.sid` / `.cur` — GUI refuses to run (5 jobs)

A job needs both a `.sid` (system/plant) and a `.cur` (run) file to build the
engine's `DATT.DAT`. The GUI now refuses these cleanly instead of letting the
Fortran engine abort on an empty `DATT.DAT`.

| Suite | Job | Missing | Reason |
|---|---|---|---|
| Jobs | `Cone` | `.cur` | Ships `.sid` but no `.cur`; incomplete run data |
| Jobs | `UNITS` | `.sid` | Ships `.cur` but no `.sid`; incomplete system data |
| JobsRPK | `CoalPrep/ex5V3` | `.sid` + `.cur` | Flowsheet-only export (`.syd`/`.TEA`/`.TRN` only); never meant to simulate |
| JobsRPK | `ConeOpt/Cone1` | `.sid` + `.cur` | Stale orphaned `.JOB` left over from a rename (`Cone1` → `ConeOptimization`) |
| JobsRPK | `Kennecott/Deepak data 2003/MILL` | `.cur` | Ships everything except `.cur`; unit-parameters file never saved |

### Exit 34 — singular convergence matrix (4 jobs)

`CALC.FOR:907` finds a zero pivot during back-substitution: the flowsheet's RMS
error does not converge, so the convergence matrix becomes singular.

| Suite | Job | Reason |
|---|---|---|
| Jobs | `Bougainville` | Flowsheet does not converge; matrix becomes singular |
| Jobs | `Fairlane` | Does not converge; ships a `.lju` but the mill model does not enable liberation, so it is a genuine convergence failure |
| JobsRPK | `Bougainville` | Flowsheet does not converge; matrix becomes singular |
| JobsRPK | `Fairlane` (root) | Does not converge (`RMS error 0.388687` after setup); ships `.lju` but `GMIL` mill does not enable liberation |

### Exit 36 — solids/water mass-balance error (15 jobs)

`CALC1.FOR` `CONSRV` exceeds its tolerance (1% per output stream, 0.1% total
solids, 1% water). The SAG-mill jobs typically produce zero (or diverging)
product, e.g. `MASS FLOW FROM THE MODEL 0.000` vs an expected `27.78`, and zero
power draw (`Morell power 0.0 W`).

| Suite | Job | Reason |
|---|---|---|
| Jobs | `SAG-CVRD` | SAG-mill mass balance fails |
| Jobs | `ex1aV3` | Mass balance fails |
| Jobs | `fosfertil793` | Mass balance fails |
| Jobs | `jones test` | Mass balance fails |
| Jobs | `sag-cetem` | SAG-mill mass balance fails |
| Jobs | `sag-cetem_B11` | SAG-mill mass balance fails |
| Jobs | `sag-cetem_closed` | SAG-mill mass balance fails |
| JobsRPK | `Trial SAGM` | Mass balance fails |
| JobsRPK | `ex1aV3` (root) | Mass balance fails |
| JobsRPK | `CoalPrep/ex1aV3` | Mass balance fails |
| JobsRPK | `CETEM/SAG mill/sag-cetem_B11` | SAG-mill mass balance fails |
| JobsRPK | `CETEM/SAG mill/sag-cetem_closed` | SAG-mill mass balance fails |
| JobsRPK | `Greens Creek/Greens Creek` | Balance fails on units 2/3 every iteration |
| JobsRPK | `Kennecott/Kennecott SAG only` | Reports a stream of `3.7e15 kg/s` (diverged) |
| JobsRPK | `Parameter estimation jobs/SAGM Parameter Estimation` | `MASS FLOW FROM THE MODEL 0.000` vs expected `27.78` |

### Exit 16 — no-solids stream (2 jobs)

`DATAINPT.FOR:452` rejects a stream because `PERS ≤ 0` ("no solids").

| Suite | Job | Reason |
|---|---|---|
| Jobs | `ELISTA05` | A stream carries no solids |
| JobsRPK | `Baker Process/CCDTrial` | Tear stream 7 seed saved all-zero (`STRM 7 1 / .0000 .0000`); clearing the tear data (or supplying a non-zero seed) resolves it |

### Exit 38 — calculation-phase error, mill / classification model (2 jobs)

| Suite | Job | Reason |
|---|---|---|
| Jobs | `FAG_SAG` | FAG/SAG mill calculation-phase error (exit 38) |
| JobsRPK | `Kiruna Autogenous Mill/Kiruna primary circuit` | FAG mill internal recycle diverges (~`4.4e7 kg/s`); "try increasing the grate aperture" — internal grate-recycle loop never settles |

### Exit 32 — error reading file during calculation (1 job)

| Suite | Job | Reason |
|---|---|---|
| Jobs | `Cyclone` | `CALC.FOR:1102` — error reading a file during the calculation phase; the diagnostic points at the tear-stream data |

### Engine access violation — no clean exit code (1 job)

| Suite | Job | Reason |
|---|---|---|
| JobsRPK | `Coal processing jobs/Batac jig with proximate analysis` | `INORDCALC` raises an access violation (caught as a clean "fail"). Its `.sid` line 22 carries **zero particle properties** (`2.7000E+0 0.0000E+0 …`), which overruns the coal-jig model |

### Flaky — non-deterministic, may pass or fail (2 jobs)

These flip between success and a clean error code across runs (legacy
uninitialised engine state / a convergence boundary). They are tolerated by the
manifests (either outcome is accepted) but would still be a regression if they
crashed.

| Suite | Job | Reason |
|---|---|---|
| JobsRPK | `Kennecott/Kennecott SAG BM Line 4` | Alternates singular-matrix (exit 34) / success |
| JobsRPK | `Kennecott/KennecottDeepak1` | Alternates mass-balance (exit 36) / success |

---

## Disposition

- **5 missing-data jobs** — GUI refuses cleanly; shipped data left untouched (documented).
- **25 engine failures** (exit 16/32/34/36/38 + 1 access violation) — genuine
  shipped-data / model issues: fixing them requires editing the offending job's
  unit parameters (SAG/FAG mill settings, convergence, tear seeds) or completing
  the missing companion files.
- **Flaky (2 jobs)** — `JobsRPK/Kennecott/Kennecott SAG BM Line 4` (34 ⇄ success)
  and `JobsRPK/Kennecott/KennecottDeepak1` (36 ⇄ success) flip across runs and
  are tolerated by the manifests.

This inventory is generated from the manifests in `test_e2e_jobs.py` and
`test_e2e_jobsrpk.py`, which are the single source of truth. A deeper root-cause
write-up for the JobsRPK failures lives in
[`PYTHON_GUI_E2E_FAILURES.md`](PYTHON_GUI_E2E_FAILURES.md).
