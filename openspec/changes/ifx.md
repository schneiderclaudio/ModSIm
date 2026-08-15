# ModSIM — IFX x64 Port: Summary of Changes

**Branch:** `feat/ifx-support`
**Commit:** `c89b537`
**Date:** 2026-07-09

---

## Objective

Migrate the ModSIM simulation engine from the legacy 32-bit Compaq Visual Fortran 6.6 (CVF)
+ IMSL toolchain to Intel IFX 2025.3.2 (oneAPI) targeting x64 Windows DLLs, driven by a
single cross-platform `GNUmakefile`.

---

## Toolchain

| Aspect | Before | After |
|---|---|---|
| Compiler | CVF 6.6 (32-bit) | IFX 2025.3.2 — Intel oneAPI (64-bit) |
| Numerical library | IMSL (32-bit x86 only) | Pure-Fortran + MINPACK + MKL |
| Build system | NMAKE `.mak` files | GNU make `GNUmakefile` (+ NMAKE kept as reference) |
| Architecture | x86 (32-bit) | x64 (64-bit) |
| C++ compiler | VC98 `cl.exe` | VS2022 `cl.exe` |

---

## New Files

### Build system

| File | Purpose |
|---|---|
| `Modsim/Modsimdl/GNUmakefile` | Master build driver for all three DLLs with correct dependency chain and isolated per-DLL module subdirectories |
| `Modsim/Modsimdl/ModsimMain/ModsimMain_ifx.mak` | NMAKE reference makefile for ModsimMain (IFX) |
| `Modsim/Modsimdl/ModsimCurveFitDll/ModsimCurveFit_ifx.mak` | NMAKE reference makefile for ModsimCurveFit (IFX) |
| `Modsim/Modsimdl/UserModels/UserModels_ifx.mak` | NMAKE reference makefile for UserModels (IFX) |

### Numerical replacements (IMSL removed)

| File | Purpose |
|---|---|
| `Modsim/Modsimdl/ModsimCurveFitDll/NumericalReplace.f90` | Pure-Fortran replacements for all IMSL routines used by ModsimCurveFit: `BCLSF` (bounded Levenberg-Marquardt wrapper), `EVCRG` (eigenvalue via MKL `SGEEV`), `RNSET`/`RNUN` (RNG stubs), plus the `BCLSF_State` module |
| `Modsim/Modsimdl/ModsimCurveFitDll/minpack.f90` | Public-domain MINPACK Levenberg-Marquardt routines (`lmdif`, `lmpar`, `qrfac`, etc.) converted to free-form F90 |

### gfortran compatibility stubs

| File | Purpose |
|---|---|
| `Modsim/Modsimdl/msflib_stub.f90` | Stub implementations of `msflib` intrinsics for gfortran builds |
| `Modsim/Modsimdl/portlib_stub.f90` | Stub implementations of `portlib` intrinsics for gfortran builds |

### Visual Studio 2022 solution

| File | Purpose |
|---|---|
| `ModS/ModS.sln` | VS2022 solution for IFX Fortran project |
| `ModS/ModS.vfproj` | Intel Fortran project file (all source files listed; supports Debug/Release x64) |

### Documentation

| File | Purpose |
|---|---|
| `AGENTS.md` | AI agent instructions for working in this repo (build commands, critical toolchain facts, conventions) |
| `docs/build/BUILD_REPORT.md` | Detailed build report: environment setup, flags, dependency chain |
| `docs/build/SESSION_NOTES.md` | Session-by-session engineering notes (Sessions 1–3) |
| `docs/Relatorio_Modsim.md` | Project overview document |
| `tools/modernize_fortran.py` | Script to convert Fortran 77 fixed-form `.FOR` files to free-form `.f90` (continuation style, operator syntax, DO loops, `IMPLICIT NONE`) |
| `openspec/config.yaml` | OpenSpec workflow configuration |

---

## Modified Files

### `Modsim/Modsimdl/BETAAMD.FOR`

Appended pure-Fortran implementations of three IMSL routines at the end of the file:

- **`BETAI(a, b, x)`** — regularised incomplete beta function (continued-fraction algorithm)
- **`DBETAI(a, b, x)`** — double-precision variant
- **`ANORDF(x)`** — standard normal CDF (rational approximation)

These replaced the IMSL calls that could not be linked into x64.

### `Modsim/Modsimdl/ModsimCurveFitDll/ModsimCurveFit.f90`

- Removed `USE Numerical_libraries` (IMSL module — unavailable in x64)
- Updated all call signatures for `BCLSF` and `EVCRG` to match the new
  `NumericalReplace.f90` interfaces
- Module now compiles cleanly under IFX

### `Modsim/Modsimdl/CALC.FOR`

- Fixed IFX-incompatible syntax (continuation/declaration issue)

### `Modsim/Modsimdl/CALC1.FOR`

- Fixed IFX-incompatible syntax

### `Modsim/Modsimdl/DMINP.FOR`

- Fixed IFX-incompatible syntax

### `Modsim/Modsimdl/GENLSUBS.FOR`

- Fixed IFX-incompatible syntax (9 lines changed)

### `Modsim/Modsimdl/MILLMODS.FOR`

- Fixed IFX-incompatible syntax (3 lines changed)

### `Modsim/Modsimdl/MILLREPS.FOR`

- Fixed IFX-incompatible syntax (2 lines changed)

### `Modsim/Modsimdl/UserModels/CModelVariables.h`

- Added missing struct fields / corrected layout to match the updated
  Fortran `GLOBALS` module for x64 pointer alignment

### `Modsim/Modsimdl/UserModels/Connector.f90`

- Fixed `USE GLOBALS` declaration to resolve under IFX module search path

### `Modsim/Modsimdl/UserModels/ConnectorToC.cpp`

- Updated includes and struct references to compile cleanly under VS2022 `cl.exe`

---

## Build Outputs (verified)

### Debug (`make COMPILER=ifx BUILD=debug all`)

| DLL | Size |
|---|---|
| `build/ifx/debug/UserModels.dll` | 1,221 KB |
| `build/ifx/debug/ModsimMain.dll` | 4,933 KB |
| `build/ifx/debug/ModsimCurveFit.dll` | 9,268 KB |

### Release (`make COMPILER=ifx BUILD=release all`)

| DLL | Size |
|---|---|
| `build/ifx/release/UserModels.dll` | 689 KB |
| `build/ifx/release/ModsimMain.dll` | 2,402 KB |
| `build/ifx/release/ModsimCurveFit.dll` | 8,636 KB |

Both configurations compile with zero errors. Remarks are benign (FORMAT field-width
recommendations). `ModsimCurveFit.dll` is large because MKL static libraries are linked in.

---

## Key Design Decisions

### GNU make over NMAKE
`GNUmakefile` is the primary build driver. NMAKE `.mak` files are kept as reference but are
not the recommended path. GNU make is cross-platform and supports parallel jobs (`-j`).

### Per-DLL module subdirectories
Each DLL compiles into its own module subdirectory (`OUTDIR/UserModels/`, `OUTDIR/CurveFit/`).
`GLOBALS.F90` and `ModelVariables.f90` are re-compiled into each subdir to avoid `.mod` file
collisions at link time.

### `link.exe /dll` directly (not IFX linker driver)
IFX's linker driver (`ifx /dll`) injects `for_main.obj` even for DLLs, causing an unresolved
`MAIN__` symbol. Using `link.exe /dll` directly avoids this.

### `setvars.bat` required (not `compiler/env/vars.bat`)
The component-only Intel vars script sets up IFX but does not add MKL library paths to `LIB`.
The full `setvars.bat intel64` is required.

### IMSL fully replaced
IMSL ships as 32-bit x86 only (`14C` machine type). It cannot be linked into a 64-bit DLL.
All IMSL dependencies were eliminated:
- `BETAI`/`DBETAI`/`ANORDF` → pure-Fortran in `BETAAMD.FOR`
- `BCLSF` → wrapper around MINPACK `lmdif` in `NumericalReplace.f90`
- `EVCRG` → wrapper around MKL `SGEEV` in `NumericalReplace.f90`
- `RNSET`/`RNUN` → no-op stubs (not exercised in current jobs)

---

## Environment Setup (Windows)

```bat
call "N:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvarsall.bat" x64
call "N:\Program Files (x86)\Intel\oneAPI\setvars.bat" intel64
```

**Important:** VS2022 and IFX are installed on drive `N:\`, not `C:\`. Using the wrong path
causes IFX to crash with `STATUS_HEAP_CORRUPTION (0xC0000374)` from ntdll.dll.

---

## Remaining Work

1. **Runtime test** — open a job from `Modsim/Jobs/` or `Modsim/JobsRPK/` in the VB6 GUI
   and run a simulation against the new DLLs. Specifically exercise `LevMarCurveFit`,
   `BETAAMD`, and `EVCRG` code paths.
2. **Release validation** — confirm release DLLs produce identical numerical results to debug.
