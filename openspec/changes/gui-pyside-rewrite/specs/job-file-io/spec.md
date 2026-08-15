## ADDED Requirements

### Requirement: Read legacy job files
The system SHALL read the legacy text job-file format (`.JOB`, `.syd`, `.siz`, `.gcd`, `.mat`, `.scd`, `.sid`, `.cur`, `.mop`, `.TEA`, `.TRN`) from a job directory.

#### Scenario: Read a job directory
- **WHEN** the user opens a job directory containing legacy job files
- **THEN** the system parses the files into an in-memory job model

#### Scenario: Read existing example job
- **WHEN** the user opens an existing job such as `Modsim/Jobs/Bougainville`
- **THEN** the system parses its `.JOB` and `.syd` files correctly

### Requirement: Write legacy job files
The system SHALL write the legacy text job-file format back to a job directory, preserving compatibility with the engine.

#### Scenario: Write a job directory
- **WHEN** the user saves a job
- **THEN** the system writes the job files in the legacy format to the job directory

#### Scenario: Round-trip compatibility
- **WHEN** a job is read and written without modification
- **THEN** the written files are compatible with the engine and existing jobs

### Requirement: Job directory path handling
The system SHALL resolve the job directory path using the `MODSIM` environment variable, falling back to a configured default, then a temp directory.

#### Scenario: MODSIM env var set
- **WHEN** the `MODSIM` environment variable is set
- **THEN** the system uses it as the default job path

#### Scenario: MODSIM env var unset
- **WHEN** the `MODSIM` environment variable is not set
- **THEN** the system falls back to a configured default or temp directory
