## Why

The reformed ModSIM application (Python GUI + 64-bit Fortran engine) is currently Windows-only: the GNUmakefile hardcodes Windows system libraries and `.dll` output names, and the ctypes bridge assumes Windows symbol naming. The GUI itself is pure Python + PySide6 and already cross-platform, so the only thing preventing the full stack from running on Linux is the engine build and the bridge's symbol resolution.

## What Changes

- **GNUmakefile Linux support**: add a gfortran Linux branch that drops the Windows `SYS_LIBS` (`-lkernel32 -luser32 ...`), emits Linux shared-library names (`libmodsim.so`, `libusermodels.so`, `libmodsimcurvefit.so`), and compiles with `-fPIC`.
- **Symbol export on Linux**: ensure the four engine entry points (`INORDCALC`, `SIMOP`, `LJUBAMD`, `BETAAMD`) are exported from the `.so` with names the bridge can resolve (gfortran mangles Fortran symbols to lowercase + trailing underscore, e.g. `inordcalc_`).
- **Engine bridge Linux resolution**: update `modsim/engine/engine_bridge.py` to locate `libmodsim.so` on Linux and resolve the entry points despite gfortran symbol mangling (try the mangled name, fall back to the plain name).
- **UserModels on Linux**: build `libusermodels.so` with gfortran, including the C++ connector compiled with `g++` (currently excluded from gfortran builds).
- **ModsimCurveFit on Linux**: build `libmodsimcurvefit.so`, using Linux MKL library names (`-lmkl_intel_lp64 -lmkl_sequential -lmkl_core`) or a pure-Fortran fallback where MKL is unavailable.
- **Verification**: build all three libraries on Linux and run the Python harness (engine-bridge tests) against the Linux `.so`.

## Capabilities

### New Capabilities
- `linux-engine-build`: The GNUmakefile builds the three engine libraries as Linux shared libraries with gfortran — correct output names, `-fPIC`, no Windows-only link flags, and exported entry-point symbols.
- `linux-engine-bridge`: The ctypes bridge locates and loads the Linux shared libraries and resolves the four Fortran entry points despite gfortran symbol mangling.

### Modified Capabilities
<!-- No existing specs; all capabilities are new. -->

## Impact

- `Modsim/Modsimdl/GNUmakefile` — new gfortran Linux branch; output names and link flags become platform-conditional.
- `Modsim/Modsimdl/UserModels/` — C++ connector (`ConnectorToC.cpp`) build path for Linux (`g++`).
- `Modsim/Modsimdl/ModsimCurveFitDll/` — MKL link flags for Linux.
- `Modsim/Modsimpy/modsim/engine/engine_bridge.py` — Linux library-name and symbol resolution.
- `Modsim/Modsimpy/tests/test_engine_bridge.py` — Linux coverage for symbol resolution.
- `README_V2.md` — document the Linux build/run path.
- No change to the Fortran sources themselves (they are already portable — no Windows API calls).
