## ADDED Requirements

### Requirement: Linux shared-library output
The GNUmakefile SHALL build the three engine libraries as Linux shared libraries when run with gfortran on a Linux host, producing `libmodsim.so`, `libusermodels.so`, and `libmodsimcurvefit.so` in `build/gfortran/release/` (and `build/gfortran/debug/` for debug builds).

#### Scenario: Release build on Linux
- **WHEN** `make` is run with gfortran on a Linux host
- **THEN** `build/gfortran/release/libmodsim.so`, `libusermodels.so`, and `libmodsimcurvefit.so` are produced

#### Scenario: Debug build on Linux
- **WHEN** `make BUILD=debug` is run with gfortran on a Linux host
- **THEN** `build/gfortran/debug/libmodsim.so`, `libusermodels.so`, and `libmodsimcurvefit.so` are produced

### Requirement: No Windows-only link dependencies
The Linux build SHALL NOT link Windows system libraries (`kernel32`, `user32`, `gdi32`, `winspool`, `comdlg32`, `advapi32`, `shell32`, `ole32`, `oleaut32`, `uuid`, `odbc32`, `odbccp32`).

#### Scenario: Link succeeds without Windows libraries
- **WHEN** the Linux build links the engine libraries
- **THEN** the link command contains no Windows system library flags and succeeds on a Linux host

### Requirement: Position-independent code
The Linux build SHALL compile all Fortran and C++ objects with position-independent code (`-fPIC`).

#### Scenario: Objects compiled with -fPIC
- **WHEN** the Linux build compiles any source file
- **THEN** the compile command includes `-fPIC`

### Requirement: Exported entry-point symbols
The Linux build SHALL export the four engine entry points (`INORDCALC`, `SIMOP`, `LJUBAMD`, `BETAAMD`) from `libmodsim.so` so they are resolvable by the ctypes bridge.

#### Scenario: Entry points visible in the shared library
- **WHEN** `nm -D libmodsim.so` is run on the built library
- **THEN** the exported symbol table contains the four entry-point symbols (in gfortran-mangled form, e.g. `inordcalc_`)

### Requirement: UserModels C++ connector on Linux
The Linux build SHALL compile the UserModels C++ connector (`ConnectorToC.cpp`) with `g++` and link it into `libusermodels.so`.

#### Scenario: C++ connector compiled on Linux
- **WHEN** the Linux build compiles UserModels
- **THEN** `ConnectorToC.cpp` is compiled with `g++` and linked into `libusermodels.so`

### Requirement: ModsimCurveFit MKL on Linux
The Linux build SHALL link ModsimCurveFit against the Linux MKL library names (`-lmkl_intel_lp64 -lmkl_sequential -lmkl_core`), or fall back to a pure-Fortran configuration when MKL is unavailable.

#### Scenario: MKL linked on Linux
- **WHEN** the Linux build links ModsimCurveFit and MKL is available
- **THEN** the link command uses the Linux MKL library names

#### Scenario: Build without MKL
- **WHEN** the Linux build links ModsimCurveFit and MKL is not available
- **THEN** the build falls back to a pure-Fortran configuration and still produces `libmodsimcurvefit.so`
