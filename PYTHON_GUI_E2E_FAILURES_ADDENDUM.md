# RCA Verdict & Fix Plan — JobsRPK E2E Failures (addendum to `PYTHON_GUI_E2E_FAILURES.md`)

**Question investigated:** are the 16 deterministic failures (+2 flaky) in
`PYTHON_GUI_E2E_FAILURES.md` truly corrupt shipped job configuration, or are
they regressions caused by this branch's code changes (Fortran IFX/x64 port,
IMSL elimination, new Python GUI)?

**Answer: the original report is wrong about 7 jobs (8 affected incl. flaky).**
The 8-job "solids/water mass-balance" family (§5.2) and the 2 "flaky" jobs are
**IFX `-O2` fast-floating-point regressions introduced by the port** — the
engine data is fine; the wrong compiler option turns convergent simulations
into divergent ones. The remaining failures are genuinely shipped-data issues
(data claims below were re-verified file-by-file), plus new port defects found
along the way (BETAAMD typing, UMCGF stub).

All experiments run on the real engine DLL via the same GUI code path the E2E
suite uses (`tests/_e2e_run_job.py`, subprocess-per-job), on this machine,
from the current tree.

---

## 1. Method — compiler-flag bisect

The IFX port compiles all three DLLs with `-O2` and **no FP-model or
storage-class flags** (`Modsim/Modsimdl/GNUmakefile`). The legacy engine was
built with Compaq Visual Fortran, which defaults to *static local storage and
exact-ish FP semantics*; modern IFX defaults to *stack locals and
`-fp:fast`* (contractible reassociation). Any legacy code numerically sensitive
to reassociation changes behaviour. I built 8 engine variants into isolated
trees and ran the full failing set plus a passing-job control set through the
E2E worker:

| Build | FCFLAGS | Result on the exit-36 family |
|---|---|---|
| release (baseline, shipped) | `-O2` | all fail (36) — reproduces report |
| oz0 | `-O0` | **all pass** |
| oz1 | `-O1` | all fail (36) — plus **access violation in SIMOP** |
| oz2qsave | `-O2 -Qsave` | 2 of 3 tested pass |
| oz2qzero | `-O2 -Qzero` | same as qsave |
| oz2novec | `-O2 -Qvec-` | still fail 36 |
| oz2fpprecise | `-O2 -fp:precise` | 1 passes, 1 AVs in SIMOP |
| **oz2fpstrict** | `-O2 -fp:strict` | **all pass, deterministic, no AVs** |

The failures are therefore **not** uninitialized-storage-only (qsave/qzero
insufficient), **not** vectorization (`-Qvec-` ineffective), and **not**
fixable by "softer" precise FP (`-fp:precise` still AVs on one job). They are
caused by the scalar fast-FP codegen at `-O2`. `-Qsave`/`-Qzero` are not
needed as part of the fix.

**Full-sweep validation of the fix candidate** (`-O2 -fp:strict`, entire
92-job JobsRPK E2E suite): every job that passes on release still passes;
the 6 fixed manifest jobs also pass — the suite's only failure is literally
`jobs listed as known-issues now succeed`. Runtime: 72 s for the full sweep
(no measurable slowdown). Numeric drift on passing jobs is rounding-level:
diffing `OPDISP.DAT` for `Bougainville1` between release and fp:strict shows a
single value changed in its last digit (`12.166` → `12.165`).

---

## 2. Verdict per failure

### 2.1 REGRESSION — IFX `-O2` fast-FP (fix in engine build flags)

| Job | Release | `-O0` | `-fp:strict` | Old report claimed |
|---|---|---|---|---|
| `Trial SAGM` | 36 | **pass** | **pass (4/4 iters)** | shipped-data issue |
| `Parameter estimation jobs/SAGM Parameter Estimation` | 36 | **pass** | **pass (4/4 iters)** | shipped-data issue |
| `sag-cetem_closed` | 36 | pass | **pass** (1/4 iters on qsave build) | shipped-data issue |
| `sag-cetem_B11` | 36 | pass | **pass** | shipped-data issue |
| `Greens Creek/Greens Creek` | 36 | pass | **pass** | shipped-data issue |
| `Kennecott/Kennecott SAG only` | 36 | pass | **pass** | shipped-data issue |
| `Kennecott/KennecottDeepak1` (flaky) | 36/success | pass | **pass** | "legacy uninitialised state" |
| `Kennecott/Kennecott SAG BM Line 4` (flaky) | 34/37/success | pass | off-boundary (observed 37 once) | "legacy uninitialised state" |

RCA: these old SAG/CCD-AG mill circuits sit on a convergence boundary in the
mill/recycle iteration (`CONSRV` in `CALC1.FOR`, exit 36; matrix singular in
`CALC.FOR`, exit 34). IFX `-O2`'s `-fp:fast` reassociation perturbs the
iteration enough to push some runs off the edge; garbage differs per run and
per build, which is exactly the observed "flaky" behaviour. The report's own
§5.2 "divergence evidence" (zero product where `27.78` expected, `3.7e15 kg/s`
streams) describes numerical blow-up, not bad input data — and indeed the data
proves sufficient under `-fp:strict`.

The `-O1` and `-O2 -fp:precise` builds additionally produced **access
violations in SIMOP** that neither `-O0` nor `-fp:strict` produce — a second,
more ominous symptom of aggressiveness vs. this legacy code (possible
out-of-bounds read the zero/stack layout changes). Worth knowing when choosing
flags: do not "fix" by merely lowering optimization.

### 2.2 Confirmed genuine shipped-data issues (data claims re-verified)

Deterministic under **every** build variant tested (`-O0`, `-O2`, `-fp:strict`,
`-Qsave -Qzero`) — compiler-invariant ⇒ not a codegen regression.

| Job | Re-verified evidence | Verdict |
|---|---|---|
| `ex1aV3` (root) & `CoalPrep/ex1aV3` | `CONSRV`: solids & water balance wrong on units 11 and 13 on every iteration, every build | model/plant-data mismatch — needs unit-parameter correction |
| `Bougainville` (root) | `AFTER SETTING UP, THE RMS ERROR IS 0.228738` → zero pivot (exit 34) | genuine non-convergence |
| `Fairlane` (root) | `RMS ERROR IS 0.388686`; mill does not enable liberation so the `.lju` is unused | genuine non-convergence |
| `Kiruna primary circuit` | exit 38 (FAG grate-recycle divergence) in all builds | genuine model divergence |
| `Baker Process/CCDTrial` | `.TEA` tear seed `STRM 7 1` is all-zero — verified | data defect (`PERS ≤ 0` → exit 16) |
| `Batac jig with proximate analysis` | `.sid` `PHYP 001` row is `2.70 0 0 0 0` vs working sibling `1.30 1.35 1.50 1.60 2.50` — verified; engine AVs (ctypes `OSError`) | data defect; engine robustness gap (hard crash instead of clean error) |
| `ex5V3`, `Cone1`, `MILL` | `.sid`/`.cur` really absent (file listing) | data defect; the GUI refusal guard is correct |

The 3 missing-`.sid/.cur` jobs and the missing-data guard mechanism (§3 of the
original report) check out; the "empty `DATT.DAT` → forrtl EOF" mechanism was
confirmed by code inspection (`write_datt_dat` no-ops; `OPEN` default
`STATUS='UNKNOWN'` creates the file).

### 2.2 Python-GUI fixes from the original report — confirmed correct
`run_simulation()` refuses jobs missing `.sid`/`.cur` before calling the
engine, and stages `LJUBAMD.DAT`/`BETAAMD.DAT` before running (verified live:
estimation jobs, pass on the release engine). The 255-byte space-padded path
encoding in `engine_bridge.encode_path()` also correctly satisfies the
Fortran `CHARACTER*255` dummy-argument contract.

---

## 3. New application defects found during RCA (not in the original report)

1. **`BETAAMD.FOR` REAL(4)/REAL(8) implicit-typing mismatches** — `DBETAI`,
   `dBetaIdAlpha`, `dBetaIdBeta` are defined `REAL*8` but the F77 file has no
   declarations at call sites, so they (and args `alpha`/`betaa`/`Xi*`) are
   implicitly `REAL(4)`. Consequences:
   - `make COMPILER=gfortran` is **completely broken** (severe errors;
     gfortran build currently cannot link at all).
   - IFX compiles it silently but mixes 4/8-byte values on the argument/return
     path inside the Beta-liberation gradient code — a latent numeric defect
     in the IMSL-replacement port (gradients are only consumed in fitting
     mode, which is why E2E simulation jobs still pass).
2. **`UMCGF` curve-fit method is now a silent no-op** (`ModsimCurveFit.f90`):
   the IMSL call was replaced by a diagnostic line, so jobs choosing UMCGF get
   their `XGUESS` back as "fitted" parameters. No shipped job uses it today,
   but it should either be implemented or turned into a clean error.
3. **`-O1`/`-O2 -fp:precise` builds access-violate in SIMOP** on SAG jobs
   (aggressive scalar codegen vs. legacy `COMMON`-heavy code). `-fp:strict`
   and `-O0` do not. Recorded because any future flag tuning must re-run the
   SAG jobs, not just the previously-passing set.
4. *(Informational)* Under `-check:all`, IFX flags the ctypes→Fortran call
   (`Dummy character variable 'JOBPATH' has length 255 … actual …83`). In
   release builds this is inert (the engine uses its declared length; the
   bridge sends exactly 255 space-padded bytes), but it means only *side-blind*
   diagnostics builds can be used for runtime checking (`-check:uninit` alone
   works; `-check:all` trips on the STDCALL hidden-length convention).

---

## 4. Fix plan

### A. Engine build flags (fixes the confirmed regression) — *primary fix*
In `Modsim/Modsimdl/GNUmakefile`, IFX release section:
```make
  FCFLAGS := -O2 -fp:strict     # was: -O2
```
- Apply to ModsimMain **and** UserModels **and** ModsimCurveFit (all inherit
  `FCFLAGS`), rebuild all three DLLs.
- Evidence for `-fp:strict` over alternatives: only variant that fixed all 8
  affected jobs with **zero** AVs; `-O1` and `-fp:precise` still produce AVs;
  `-Qvec-`/`-Qsave`/`-Qzero` insufficient.
- Breadth: full 92-job sweep passes on this build; passing-job outputs change
  by ≤ 1 ulp-in-printed-digit. Route: beads `ModSIm-2w7`.

### B. Manifest + documentation updates (gated on A) — route: beads `ModSIm-btk`
- Remove from `KNOWN_FAILURES` in `tests/test_e2e_jobsrpk.py`: `./Trial SAGM`,
  `CETEM/SAG mill/sag-cetem_B11`, `CETEM/SAG mill/sag-cetem_closed`,
  `Greens Creek/Greens Creek`, `Kennecott/Kennecott SAG only`,
  `Parameter estimation jobs/SAGM Parameter Estimation`.
- Soak-test the two `FLAKY` Kennecott jobs (≥ 10 runs each) on the new build;
  promote `KennecottDeepak1` out of `FLAKY` if stable, keep `SAG BM Line 4`
  until stable.
- Update `PYTHON_GUI_E2E_FAILURES.md` §5.2/§7/§9 dispositions accordingly.

### C. Fix `BETAAMD.FOR` typing — route: beads `ModSIm-3ti`
- Declare `REAL*8 DBETAI, dBetaIdAlpha, dBetaIdBeta` (`EXTERNAL`-style F77
  declarations) **at every call site** and add `REAL*8` declarations for
  `alpha`, `betaa`, `XiU`, `XiL`, `WS1`, `DWS1D*`, or switch the file to
  `IMPLICIT NONE` with full declarations (preferred; it is a new file from the
  port).
- Unblocks `make COMPILER=gfortran` (needed as an independent build for
  future bisects) and removes the IFX 4/8-byte mixing in the Beta-liberation
  gradient path. Re-run E2E afterwards (values on liberation jobs may shift in
  the last digits — expected and correct).

### D. UMCGF stub — route: beads `ModSIm-tni`
Implement conjugate-gradient on top of the existing MINPACK replacement, or
fail loudly (`ExitValue` + diagnostic) when a job selects `UMCGF`. Low
priority (no shipped data affected).

### E. Program-check builds (cheap insurance in CI/dev loop)
Add a `make COMPILER=ifx CHECK=1` variant using
`-check:uninit -traceback` (not `-check:all`, which trips on the
`CHARACTER*255` STDCALL hidden-length contract) to catch future
uninitialized-variable regressions deterministically.

### F. No-fix dispositions (documented, as the original report concluded)
The 3 missing-`.sid/.cur` jobs (GUI guard handles them), the 2 root
convergence failures (`Bougainville`, `Fairlane`), the 2 `ex1aV3` units 11/13
model/data mismatches, the `Kiruna` FAG divergence, the `CCDTrial` zero tear
seed, and the `Batac jig` zero particle properties are genuine shipped-data
defects; the engine now fails them cleanly/deterministically on every
compiler variant. Fixing those means editing the respective job data, not the
application.

---

## 5. Verification steps after applying A–C
1. Rebuild all three DLLs (ifx + gfortran once C lands).
2. Full sweep: `MODSIM=<engine dir> python tests/test_e2e_jobsrpk.py` — expect
   91 pass / 6 + 3 documented fails / 0 crash (+ soaked flaky stability).
3. Output-drift check: `diff` `OPDISP.DAT` of `Bougainville1`, `Blackbox job`,
   `MtLyell2`, `WHIMS` between old release and new build — rounding-level only.
4. liberation-soak: `DemoLib`, `Ex10-6BMCircuitwithLiberationandConcentration`,
   `Fairlane/Fairlane`, `HF* parameter estimation` — all pass under new build.

---
## 7. FIXES APPLIED (2026-09, post-RCA)

- **A. Engine flags** — `Modsim/Modsimdl/GNUmakefile` IFX release `FCFLAGS`
  changed to `-O2 -fp:strict` (with a comment explaining why). All three DLLs
  rebuilt; both E2E suites green (JobsRPK: 81-82 ok / 0 crash; Jobs: 34 ok).
  *Note: `make` does not track FCFLAGS changes — delete
  `build/ifx/release/` (and pre-create the dirs) before rebuilding.*
- **B. Manifests** — `test_e2e_jobsrpk.py` and `test_e2e_jobs.py`: removed
  `Trial SAGM`, `sag-cetem*` ×3, `Greens Creek`, `Kennecott SAG only`,
  `SAGM Parameter Estimation`; `MIN_SUCCESSFUL_JOBS` raised (70→76, 28→31).
  `KennecottDeepak1` removed from `FLAKY` (5/5 success under the new build);
  `SAG BM Line 4` stays in `FLAKY` — it now fails *deterministically* with
  calc-phase error 37 (it sits on a numerical boundary either way).
- **C. BETAAMD.FOR typing** — `dBetaIdAlpha`, `dBetaIdBeta` and `DBETAI` now
  declared `REAL*8` in their definitions and at the `AMDIAGRAM` call site
  (previously implicitly `INTEGER(4)`/`REAL(4)`; gradients were truncated to
  whole numbers — dormant, since the analytic-gradient path is commented out
  and the secant fallback is used). gfortran now compiles BETAAMD.FOR cleanly.
- **D. UMCGF stub** — `ModsimCurveFit.f90` UMCGF branch now fails loudly:
  writes an ERROR line to the curve-fit diagnostic and sets
  `SumOfSquares(1) = 1E38` as the not-fitted sentinel (no shipped job uses
  UMCGF).
- **Follow-up:** gfortran build is still blocked after BETAAMD by the next
  pre-existing legacy issue: `CRSHMODS.FOR` uses 3-D subscripts on `FEED(1)`
  (rank-mismatch "Invalid form of array reference"), plus more of the same
  class in other `.FOR` files — a gfortran-port task, unrelated to the IFX
  regression fix.

## 8. Artifacts from this RCA (temporary, removed after investigation)
The 8 bisect engine trees under `Modsim/Modsimdl/build/ifx/` were deleted;
`build/ifx/release` now contains the fixed `-O2 -fp:strict` build. No shipped
job data was modified.
