## ADDED Requirements

### Requirement: Load the Fortran DLL
The system SHALL load the 64-bit Fortran engine shared library (`ModsimMain.dll` on Windows, `libmodsim.so` on Linux, `libmodsim.dylib` on macOS) via `ctypes`, resolving it through the platform library search path or an explicit configured path.

#### Scenario: DLL found on search path
- **WHEN** the shared library is present on the platform library search path
- **THEN** the system loads it successfully

#### Scenario: DLL not found
- **WHEN** the shared library cannot be located
- **THEN** the system reports a clear error to the user

### Requirement: Call INORDCALC
The system SHALL call `INORDCALC(path)` for job initialization and in-order calculation setup, passing a null-terminated path string with a trailing separator.

#### Scenario: Initialize a job
- **WHEN** the user runs a job
- **THEN** the system calls `INORDCALC` with the job directory path and handles the returned result code

### Requirement: Call SIMOP
The system SHALL call `SIMOP(path, &cumOut)` to run the full simulation, passing the path string and a pointer to a long output value.

#### Scenario: Run a simulation
- **WHEN** the user runs a simulation
- **THEN** the system calls `SIMOP` with the job directory path and receives the result code and cumulative output

### Requirement: Call LJUBAMD and BetaAMD
The system SHALL call `LJUBAMD(path, nameOfJob)` and `BetaAMD(path, nameOfJob, &parameters)` for the Ljubljana and Beta liberation models.

#### Scenario: Run liberation models
- **WHEN** a job uses the Ljubljana or Beta liberation model
- **THEN** the system calls the corresponding function with the path and job name

### Requirement: Correct pointer width
The system SHALL use `ctypes` types sized to the target architecture (64-bit pointers on x64) for the `ByRef As Long` parameters.

#### Scenario: x64 call
- **WHEN** the application runs on a 64-bit platform
- **THEN** the FFI uses 64-bit pointer types for the by-reference parameters
