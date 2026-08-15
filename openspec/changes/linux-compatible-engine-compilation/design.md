## Context

The reformed ModSIM application is a Python PySide6 GUI driving a 64-bit Fortran engine through a `ctypes` bridge. The GUI is pure Python and already cross-platform, but the engine side is Windows-only in its current state:

- The GNUmakefile's gfortran branch hardcodes Windows system libraries (`-lkernel32 -luser32 ...`) and `.dll` output names; there is no Linux branch for gfortran. An ifx-Linux branch exists but is explicitly marked "future-proofing; untested".
- The ctypes bridge (`modsim/engine/engine_bridge.py`) declares entry points as `lib.INORDCALC` etc. (uppercase, no underscore). gfortran on Linux mangles Fortran symbols to lowercase with a trailing underscore (`inordcalc_`), so the bridge would raise `AttributeError`.
- UserModels' C++ connector (`ConnectorToC.cpp`) is excluded from gfortran builds; ModsimCurveFit links MKL with Windows `.lib` names.

The Fortran sources themselves are portable — no Windows API calls (the `imagehlp.dll` import is transitive, not from source), and the `msflib`/`portlib` stubs already exist for gfortran.

## Goals / Non-Goals

**Goals:**
- Build all three engine libraries as Linux shared libraries (`libmodsim.so`, `libusermodels.so`, `libmodsimcurvefit.so`) with gfortran.
- Make the ctypes bridge resolve and load the Linux `.so` and its entry points despite gfortran symbol mangling.
- Keep the Windows build (gfortran/MinGW and IFX) working unchanged.
- Verify by building on Linux and running the Python harness (engine-bridge tests) against the Linux `.so`.

**Non-Goals:**
- Porting the legacy VB6 GUI or Vetgraph to Linux.
- Supporting IFX on Linux (the existing untested branch stays as-is; gfortran is the Linux path).
- Changing the Fortran numerical behavior or job-file format.
- Setting up Linux CI (out of scope; verification is manual).

## Decisions

### D1. Extend the GNUmakefile with a Linux branch for gfortran
Add a platform check (e.g. `ifeq ($(OS),Linux)` / `uname`) inside the gfortran section that sets Linux-appropriate `SYS_LIBS` (empty or `-lm`/`-ldl` as needed), `-fPIC` in `FCFLAGS`, and `-shared` link flags without the Windows `--out-implib` import-library generation.

- **Why**: keeps a single build driver and reuses the existing dependency graph and source lists.
- **Alternative considered**: a separate `GNUmakefile.linux` — rejected because it would duplicate the source lists and dependency rules and drift out of sync.

### D2. Platform-conditional output names
Make `TARGET`/`UM_TARGET`/`CF_TARGET` platform-conditional: `libmodsim.so` / `libusermodels.so` / `libmodsimcurvefit.so` on Linux, `ModsimMain.dll` / `UserModels.dll` / `ModsimCurveFit.dll` on Windows.

- **Why**: the bridge looks for `libmodsim.so` on Linux; a `.dll`-named ELF file would not be found.
- **Alternative considered**: keep `.dll` names and update the bridge — rejected because it is confusing and non-idiomatic on Linux.

### D3. Bridge symbol resolution: try mangled name, fall back to plain
Add a helper in `engine_bridge.py` that, on POSIX, resolves each entry point by trying the gfortran-mangled name (`inordcalc_`) first, then the plain name (`INORDCALC`). On Windows, keep the current plain-name lookup.

- **Why**: gfortran exports `inordcalc_` by default; trying mangled-first is robust and requires no linker tricks. The plain-name fallback keeps compatibility if a future build exports unmangled names.
- **Alternative considered**: a linker version script / `objcopy --redefine-sym` to export plain names — rejected as more fragile and harder to maintain than a small bridge-side lookup.

### D4. UserModels C++ connector on Linux
Compile `ConnectorToC.cpp` with `g++` in the Linux gfortran build and link it into `libusermodels.so` (currently it is only compiled with IFX/cl).

- **Why**: the connector provides the C interface to user models; excluding it on Linux would silently drop functionality.
- **Alternative considered**: keep excluding it — rejected because it changes behavior between platforms.

### D5. ModsimCurveFit MKL on Linux
Make `MKL_LIBS` platform-conditional: Linux names (`-lmkl_intel_lp64 -lmkl_sequential -lmkl_core`) when MKL is available, else a pure-Fortran configuration. The only MKL-dependent routine is `EVCRG` (eigenvalues via `SGEEV`); if MKL is absent, provide a pure-Fortran eigenvalue fallback in `NumericalReplace.f90`.

- **Why**: MKL is freely available on Linux via Intel oneAPI, so the primary path mirrors Windows; the fallback keeps the build working without it.
- **Alternative considered**: require MKL unconditionally — rejected because it would break builds on Linux hosts without oneAPI.

### D6. Verification on Linux
Build all three libraries on a Linux host, then run `python -m pytest tests/ -q` from `Modsim/Modsimpy/` with `MODSIM` pointing at the Linux build directory. The engine-bridge tests exercise `resolve_library_path()` and symbol resolution against the real `.so`.

- **Why**: the specs are testable; the existing harness already skips gracefully when the library is absent, so it doubles as the Linux smoke test.

## Risks / Trade-offs

- **Case-sensitive filesystem** → The engine opens files by exact name (`DATT.DAT`, `TEARS.OUT`, etc.). The GUI writers already emit uppercase names; verify no lowercase/uppercase mismatch on Linux. Mitigation: run a real job end-to-end on Linux.
- **gfortran vs IFX numerical differences** → Not introduced by this change (gfortran builds already exist on Windows); Linux gfortran output may differ slightly from IFX. Mitigation: document that IFX remains the reference build.
- **MKL unavailable on a Linux host** → `EVCRG` needs a pure-Fortran fallback. Mitigation: implement a small Jacobi eigenvalue routine; keep MKL as the default path.
- **No Linux CI** → regressions could go unnoticed. Mitigation: the Python harness is the smoke test; document the manual verification steps.
- **Symbol export assumptions** → If a future build hides symbols (`-fvisibility=hidden`), the bridge lookup fails. Mitigation: the mangled-first/plain-fallback helper centralises the lookup; add `-Wl,--export-dynamic` to the Linux link flags as a safety net.

## Migration Plan

1. Implement the GNUmakefile Linux branch (D1, D2, D4, D5).
2. Implement the bridge symbol-resolution helper (D3).
3. Build on Linux and fix any compile/link issues.
4. Run the Python harness against the Linux `.so` (D6).
5. Update `README_V2.md` with the Linux build/run instructions.
6. Rollback: the Windows build paths are untouched; reverting the GNUmakefile Linux branch and the bridge helper restores the prior state.

## Open Questions

- Should the Linux build also support IFX (Intel oneAPI for Linux), or is gfortran-only acceptable for now? (Default: gfortran-only; the untested ifx-Linux branch is left as-is.)
- Is a pure-Fortran `EVCRG` fallback required, or is MKL-on-Linux sufficient for the first cut? (Default: implement the fallback to keep the build self-contained.)
