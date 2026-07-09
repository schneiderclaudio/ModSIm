# ModSIM x64 Build Report

**Goal:** Build `ModsimMain.dll` and `ModsimCurveFit.dll` as 64-bit DLLs using Intel IFX (oneAPI 2025.3.2) and Visual Studio 2022 on this machine, replacing the original 32-bit Compaq/Digital Visual Fortran 6.6 + IMSL toolchain.

---

## Toolchain

| Item | Value |
|---|---|
| Compiler | `ifx.exe` — Intel IFX 2025.3.2 (oneAPI) |
| VS environment | `N:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvarsall.bat x64` |
| IFX environment | `N:\Program Files (x86)\Intel\oneAPI\compiler\2025.3\env\vars.bat intel64` |
| MKL | `N:\Program Files (x86)\Intel\oneAPI\mkl\2025.3\lib\` (`mkl_intel_lp64`, `mkl_sequential`, `mkl_core`) |
| Target | x64 (64-bit Windows DLL) |
| Configuration | Debug (with `/Od /Zi /debug:full /traceback /check:bounds`) |

**Critical:** Both `vcvarsall.bat x64` **and** `compiler\env\vars.bat intel64` must be called before invoking `ifx.exe`. Calling only one causes IFX to crash.

---

## Build Scripts

Run each script from the directory noted. Both `vcvarsall.bat x64` and
`compiler\env\vars.bat intel64` are called inside each script.

| Script | Run from | Purpose |
|---|---|---|
| `docs/build/build.bat` | `Modsim/Modsimdl/ModsimMain/` | Builds `Modsim.dll` via NMAKE on `ModsimMain_ifx.mak` |
| `docs/build/build_usermodels.bat` | `Modsim/Modsimdl/UserModels/` | Builds `UserModels.dll` (IFX + cl) |
| `docs/build/build_curvefit.bat` | `Modsim/Modsimdl/ModsimCurveFitDll/` | Builds `ModsimCurveFit.dll` |

---

## Build Outputs

| DLL | Location | Size |
|---|---|---|
| `Modsim.dll` (ModsimMain) | `Modsim/Modsimdl/Modsim.dll` | 5.0 MB |
| `ModsimCurveFit.dll` | `Modsim/Modsimdl/ModsimCurveFitDll/Debug_ifx/ModsimCurveFit.dll` | 11.4 MB |

Both build with exit code 0 and zero linker errors.

---

## Source Changes

### 1. `Modsim/Modsimdl/BETAAMD.FOR` — IMSL replacement (ModsimMain)

**Problem:** `BETAAMD.FOR` called three IMSL routines (`BETAI`, `DBETAI`, `ANORDF`). IMSL is only available as a 32-bit x86 library (bundled with CVF 6.6 at `C:\Program Files\Microsoft Visual Studio\DF98\IMSL\LIB\`). It cannot be linked into an x64 binary.

**Fix:** Appended pure-Fortran replacements to the end of `BETAAMD.FOR`:

| IMSL routine | Replacement | Notes |
|---|---|---|
| `BETAI(x, a, b)` | Pure Fortran (single precision) | Delegates to `BETAI_DP` |
| `DBETAI(x, a, b)` | Pure Fortran (double precision) | Same engine as `BETAI` |
| `BETAI_DP(x, a, b, result)` | Subroutine workhorse | Lentz continued-fraction (Numerical Recipes §6.4) with symmetry relation |
| `BETACF_DP(x, a, b)` | Continued-fraction kernel | Double precision, 200-iteration limit, EPS=3e-7 |
| `ANORDF(x)` | Pure Fortran | `0.5 * erfc(-x / sqrt(2))` using intrinsic `ERFC` |

The `dBetaIdAlpha` and `dBetaIdBeta` functions in `BETAAMD.FOR` use `DBETAI` for finite-difference derivatives of the incomplete beta — these work unchanged with the new implementation.

---

### 2. `Modsim/Modsimdl/UserModels/` — C++/Fortran linkage fixes

**Problem:** `UserModels.dll` links a C++ module (`ConnectorToC.cpp`) with a Fortran module (`Connector.f90`). The original code lacked `extern "C"` guards and had a name-mangling mismatch.

**Files changed:**

- **`Connector.f90`** — export alias corrected to `'cmodelroutines'` (lowercase, matching C++ expectation)
- **`ConnectorToC.cpp`** — added `extern "C"` around Fortran function declarations
- **`CModelVariables.h`** — added `extern "C"` guards around shared variable declarations

**Result:** `UserModels.lib` (x64 Debug) builds successfully.

---

### 3. `Modsim/Modsimdl/ModsimMain/ModsimMain_ifx.mak` — IMSL removed from link flags

IMSL link libraries (`-limsl -lIMSLMPISTUB -lIMSLS_ERR`) were removed from the makefile. These are 32-bit-only and caused linker errors on x64. The only remaining IMSL calls in ModsimMain sources were in `BETAAMD.FOR`, replaced as above.

---

### 4. `Modsim/Modsimdl/ModsimCurveFitDll/` — Full IMSL replacement (new files)

**Problem:** `ModsimCurveFit.f90` used `Use Numerical_libraries` (an IMSL Fortran module) and called numerous IMSL routines throughout.

#### 4a. `ModsimCurveFit.f90` — modified

- Removed `Use Numerical_libraries`
- `WRRRL`/`WRCRN` call signatures changed: `DiagFile` unit number added as first argument (IMSL routed output internally; our stubs write to a standard Fortran unit)
- `FIN(...)` replaced with `FIN_REPL(...)`
- `UMACH`/`ERSET` calls retained but now invoke no-op stubs
- `UMCGF` branch (conjugate gradient, was already commented out) updated to print a "not implemented" message

#### 4b. `NumericalReplace.f90` — new file

Pure-Fortran / MKL replacements for all IMSL routines:

| IMSL routine | Replacement | Method |
|---|---|---|
| `UMACH(iopt, iunit)` | No-op stub | IMSL I/O unit routing not needed |
| `ERSET(iersvr, ipact, isact)` | No-op stub | IMSL error handling not needed |
| `U4LSF(IPARAM, RPARAM)` | Sets sensible defaults | `RPARAM(1..3) = 1e-6`; `IPARAM = 0` |
| `WRRRL(unit, title, nrow, ncol, A, lda)` | Writes matrix to Fortran unit | Simple formatted write |
| `WRCRN(unit, title, nrow, ncol, A, lda)` | Writes complex matrix to Fortran unit | Simple formatted write |
| `LINRG(n, A, lda, Ainv, ldainv)` | Gauss-Jordan with partial pivoting | Pure Fortran; falls back to identity on singular matrix |
| `EVCRG(n, A, lda, EigVals, EigVects, ldv)` | MKL `SGEEV` | Packs real+imaginary columns into `Complex` arrays per Fortran SGEEV convention |
| `FIN_REPL(p, df1, df2)` | Inverse F-distribution CDF | Bisection on `IBETA_DP_FIN`; uses relation between F-CDF and regularised incomplete beta |
| `UNLSF(FCN, m, n, ...)` | Wraps MINPACK `lmdif1` | Unconstrained Levenberg-Marquardt |
| `BCLSF(FCN, m, n, ..., xlb, xub, ...)` | MINPACK `lmdif1` + sin² transformation | Bounds enforced by `x_i = lb_i + (ub_i - lb_i)·sin²(t_i)`; `BCLSF_FCN_WRAP` back-transforms before calling `TERMS` |

**Module `BCLSF_State`** (top of `NumericalReplace.f90`): stores the bound arrays `BCLSF_LB`/`BCLSF_UB` so that `BCLSF_FCN_WRAP` (passed as callback to `lmdif1`) can access them.

**Note on `BCLSF` callback design:** Fortran 77-style external procedure passing cannot cleanly store an arbitrary callback in a module-level variable without F2003 procedure pointers. Since `TERMS` is the only FCN ever passed to `BCLSF` in this codebase, `BCLSF_FCN_WRAP` calls `TERMS` directly. If a different FCN is ever needed, the procedure pointer approach or a more general wrapper would be required.

#### 4c. `minpack.f90` — new file

Public-domain MINPACK routines (Argonne National Lab, 1980), converted to free-form Fortran 90:

| Routine | Role |
|---|---|
| `lmdif1` | Top-level entry: sets defaults, calls `lmdif` |
| `lmdif` | Core Levenberg-Marquardt loop (finite-difference Jacobian) |
| `fdjac2` | Forward-difference Jacobian approximation |
| `qrfac` | QR factorisation with column pivoting (Householder) |
| `lmpar` | Determines the LM parameter (trust-region subproblem) |
| `qrsolv` | Solves the LM subproblem given QR factorisation |
| `enorm` | Euclidean norm with overflow/underflow protection |

---

### 5. Compilation order

**ModsimMain** (enforced by `ModsimMain_ifx.mak`):
1. `GLOBALS.F90`
2. `ModelVariables.f90`
3. `SIMOPMOD.F90`
4. All other sources (including `BETAAMD.FOR`)

**ModsimCurveFitDll** (enforced by `build_curvefit.bat`):
1. `GLOBALS.F90` (from parent `Modsimdl/`)
2. `WorkingCommon.f90`
3. `NumericalReplace.f90` (defines `BCLSF_State` module — must come before files that `USE` it)
4. `minpack.f90`
5. `Modelresponse.f90`
6. `ModsimCurveFit.f90`

---

## Why IMSL Had to Be Replaced

The IMSL Fortran Numerical Library installed on this machine is distributed with Compaq/Digital Visual Fortran 6.6 (CVF 6.6). Its static libraries reside at:

```
C:\Program Files\Microsoft Visual Studio\DF98\IMSL\LIB\
```

These are **32-bit x86** object files (COFF machine type `14C`). They cannot be linked into an x64 binary (machine type `8664`). The linker produces `LNK1136: invalid or corrupt file` if they are included.

CVF 6.6 itself is also not a viable compiler for this build: it cannot read `.mod` module files produced by IFX, so mixing the two compilers for different source files is not possible.

---

## Known Limitations / Future Work

| Item | Status | Notes |
|---|---|---|
| `ModsimCurveFit.dll` x64 runtime test | Not done | No test harness; requires a VB6 GUI job to exercise |
| `BCLSF` with arbitrary FCN | Not general | Hardcoded to call `TERMS`; see §4b note above |
| `UMCGF` (conjugate gradient) | Not implemented | Was already commented out in original; now prints a warning |
| `ModsimCurveFit.dll` output path | `Debug_ifx/` | Separate from old CVF `Debug/` to avoid clobbering pre-built binaries |
| VB6 GUI | Unchanged | Still requires Visual Basic 6.0 IDE; no CLI build possible |
| IMSL `WRCRN`/`WRRRL` formatting | Simplified | Original IMSL had rich formatting options; stubs write plain `G12.4` format |
