## 1. Project Scaffolding

- [x] 1.1 Create the Python package structure under `Modsim/Modsimpy/` with `modsim/` package and `__main__.py` entry point
- [x] 1.2 Add `pyproject.toml` declaring dependencies (PySide6, Qt Charts, numpy) and Python version
- [x] 1.3 Add a README and run instructions (`python -m modsim`)

## 2. Engine Bridge

- [x] 2.1 Implement `engine_bridge.py` that loads the shared library via `ctypes` (DLL/.so/.dylib) with search-path and configured-path resolution
- [x] 2.2 Declare the 4 functions (`INORDCALC`, `SIMOP`, `LJUBAMD`, `BetaAMD`) with correct x64 pointer types
- [x] 2.3 Implement path-string handling (null-terminated, trailing separator, platform-native encoding)
- [ ] 2.4 Validate the bridge against the existing 64-bit DLL and an existing job (`Modsim/Jobs/Bougainville`)

## 3. Job File I/O

- [x] 3.1 Implement a job model representing the job directory and its data sections
- [x] 3.2 Implement readers for `.JOB`, `.syd`, `.siz`, `.gcd`, `.mat`, `.scd`, `.sid`, `.cur`, `.mop`, `.TEA`, `.TRN`
- [x] 3.3 Implement writers for the same file set in the legacy format
- [x] 3.4 Implement job-path resolution (`MODSIM` env var → configured default → temp dir)
- [x] 3.5 Validate round-trip read/write against existing jobs in `Modsim/Jobs/` and `Modsim/JobsRPK/`

## 4. GUI Shell

- [x] 4.1 Implement the MDI main window (`QMdiArea`) with menu bar, toolbar, and status bar
- [x] 4.2 Implement File menu (Open/Save job) wired to job-file-io
- [x] 4.3 Implement Window menu (cascade, tile, arrange) and MDI child-window management
- [x] 4.4 Implement status-bar feedback for job and simulation state

## 5. Flowsheet Canvas

- [x] 5.1 Implement the `QGraphicsScene`/`QGraphicsView` canvas with unit nodes and stream connectors
- [x] 5.2 Implement drag-and-drop unit palette and unit repositioning with connected streams following
- [x] 5.3 Implement stream creation/routing between unit ports
- [x] 5.4 Implement unit/stream labeling and numbering
- [x] 5.5 Implement layout persistence to/from job files (e.g., `.syd`)
- [x] 5.6 Validate canvas against an existing flowsheet job

## 6. Equipment Dialogs

- [x] 6.1 Audit which of the ~67 equipment models are actually live/used
- [x] 6.2 Define a data-driven schema format (fields, types, units, bounds) for equipment models
- [x] 6.3 Implement a schema-driven dialog generator
- [x] 6.4 Implement parameter validation against schema bounds
- [x] 6.5 Wire dialogs to read/write the corresponding job-file data sections

## 7. Unit Conversion

- [x] 7.1 Port the `SetConversionFactors` table into a data-driven conversion module
- [x] 7.2 Make the conversion module available to all dialogs and data-entry forms

## 8. Results Graphing

- [x] 8.1 Implement parsing of engine result files (e.g., `REPORT2.OUT` and other `.OUT` files)
- [x] 8.2 Implement size-distribution plots via Qt Charts (matplotlib fallback where needed)
- [x] 8.3 Implement liberation-spectra plots
- [x] 8.4 Implement stream plots (grade/recovery)
- [x] 8.5 Validate plotting against a completed simulation's result files

## 9. Integration & Validation

- [x] 9.1 Wire the full flow: open job → edit flowsheet → run simulation → view results
- [~] 9.2 Validate end-to-end against existing jobs and the 64-bit engine
- [x] 9.3 Verify Qt licensing compliance (LGPL for Qt core; GPL for Qt Charts is acceptable) and provide the Qt usage notice
- [x] 9.4 Document run instructions and cross-platform notes
