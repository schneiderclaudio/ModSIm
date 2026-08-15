## 1. GNUmakefile Linux support

- [ ] 1.1 Add a Linux platform branch to the gfortran section (detect non-Windows host) that sets Linux `SYS_LIBS` (no `-lkernel32 -luser32 ...`), adds `-fPIC` to `FCFLAGS`, and uses `-shared` link flags without the Windows `--out-implib` import-library generation
- [ ] 1.2 Make `TARGET`, `UM_TARGET`, and `CF_TARGET` platform-conditional: `libmodsim.so` / `libusermodels.so` / `libmodsimcurvefit.so` on Linux, `.dll` names on Windows
- [ ] 1.3 Add `-Wl,--export-dynamic` to the Linux link flags so the four entry-point symbols are exported from `libmodsim.so`
- [ ] 1.4 Compile the UserModels C++ connector (`ConnectorToC.cpp`) with `g++` in the Linux gfortran build and add it to `UM_OBJS`
- [ ] 1.5 Make `MKL_LIBS` platform-conditional for ModsimCurveFit: Linux names (`-lmkl_intel_lp64 -lmkl_sequential -lmkl_core`) when MKL is available, else a pure-Fortran configuration
- [ ] 1.6 Add a pure-Fortran `EVCRG` eigenvalue fallback in `ModsimCurveFitDll/NumericalReplace.f90` for builds without MKL

## 2. Engine bridge Linux resolution

- [ ] 2.1 Add a symbol-resolution helper in `modsim/engine/engine_bridge.py` that, on POSIX, tries the gfortran-mangled name (lowercase + trailing underscore, e.g. `inordcalc_`) first and falls back to the plain name (`INORDCALC`)
- [ ] 2.2 Use the helper when declaring `INORDCALC`, `SIMOP`, `LJUBAMD`, and `BETAAMD` so all four resolve on Linux
- [ ] 2.3 Confirm `resolve_library_path()` uses `libmodsim.so` on Linux (already branches on `sys.platform`; verify and adjust if needed)

## 3. Tests

- [ ] 3.1 Add engine-bridge tests covering Linux symbol resolution (mangled-name lookup and plain-name fallback) without requiring a real `.so`
- [ ] 3.2 Add a test that `resolve_library_path()` returns `libmodsim.so` candidates on a simulated Linux platform
- [ ] 3.3 Run the full Python harness (`python -m pytest tests/ -q` from `Modsim/Modsimpy/`) on Windows and confirm no regressions

## 4. Build and verify on Linux

- [ ] 4.1 On a Linux host, run `make` (gfortran) from `Modsim/Modsimdl/` and confirm `libmodsim.so`, `libusermodels.so`, and `libmodsimcurvefit.so` are produced in `build/gfortran/release/`
- [ ] 4.2 Confirm `nm -D libmodsim.so` shows the four entry-point symbols (mangled form)
- [ ] 4.3 Run the Python harness on Linux with `MODSIM` pointing at the Linux build directory and confirm the engine-bridge tests pass against the real `.so`
- [ ] 4.4 Run a real job end-to-end on Linux (e.g. `Bougainville`) and confirm `INORDCALC`/`SIMOP` succeed and result files are written

## 5. Documentation

- [ ] 5.1 Update `README_V2.md` with the Linux build and run instructions (gfortran `make`, `MODSIM` pointing at the Linux build dir, launch steps)
- [ ] 5.2 Note the gfortran-on-Linux path and that IFX remains the reference build
