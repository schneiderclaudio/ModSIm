## ADDED Requirements

### Requirement: Linux library resolution
The engine bridge SHALL locate the engine shared library on Linux by checking, in order: an explicit path, the directory named by the `MODSIM` environment variable, and the platform library search path — using the Linux library name `libmodsim.so`.

#### Scenario: Library found via MODSIM on Linux
- **WHEN** `MODSIM` points at a directory containing `libmodsim.so` on a Linux host
- **THEN** `resolve_library_path()` returns the absolute path to that `libmodsim.so`

#### Scenario: Library found via search path on Linux
- **WHEN** `libmodsim.so` is on the platform library search path and `MODSIM` is unset
- **THEN** `resolve_library_path()` returns the resolved path to `libmodsim.so`

### Requirement: Symbol resolution with gfortran mangling
The engine bridge SHALL resolve the four engine entry points (`INORDCALC`, `SIMOP`, `LJUBAMD`, `BETAAMD`) on Linux despite gfortran mangling Fortran symbols to lowercase with a trailing underscore (e.g. `inordcalc_`). It SHALL try the mangled name first and fall back to the plain name.

#### Scenario: Mangled symbols resolved
- **WHEN** the bridge loads `libmodsim.so` built with gfortran
- **THEN** all four entry points are declared and callable without raising `AttributeError`

#### Scenario: Plain-name fallback
- **WHEN** the `.so` exports the plain (unmangled) entry-point names
- **THEN** the bridge still resolves all four entry points

### Requirement: Linux loading mechanism
The engine bridge SHALL load the engine library with `ctypes.CDLL` on Linux (cdecl calling convention).

#### Scenario: Library loaded with CDLL
- **WHEN** the bridge loads the engine library on a Linux host
- **THEN** it uses `ctypes.CDLL` and the loaded library exposes the four entry points
