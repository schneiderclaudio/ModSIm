## Context

The VB6 GUI (`Modsim/Modsimvb/`, ~99,400 lines, 103 forms, 16 modules) is 32-bit-only and cannot call the newly ported 64-bit Fortran engine (`ModsimMain.dll`, Intel IFX 2025.3.2). The GUI↔engine contract is tiny and file-based: the GUI writes ~11 text files into a job directory, calls one of 4 path-string DLL functions, and reads result files back. The engine is decoupled from the GUI and must remain untouched.

The decision (from `.opencode/plans/gui-modernization-comparison.md`) is **Python + PySide6 (Qt6)**. Rationale: Qt gives native MDI (`QMdiArea`) and a gold-standard 2D canvas (`QGraphicsScene`), `ctypes` makes the Fortran FFI trivial (every call is `function(path_string, ...)`), Python is the lingua franca of the scientific/engineering contributor pool, and the scientific ecosystem (numpy, scipy, matplotlib) covers graphing. The heavy computation stays in Fortran, so Python's performance is irrelevant to the core workload. GPL licensing is acceptable for this project, so GPL-licensed Qt modules (including Qt Charts) are permitted.

## Goals / Non-Goals

**Goals:**
- Replace the VB6 GUI with a cross-platform (Windows/Linux/macOS) PySide6 application.
- Preserve the legacy text job-file format so the new GUI works with the existing engine and existing jobs immediately, with zero engine changes.
- Call the 64-bit DLL via `ctypes` using the 4 path-string functions.
- Rebuild the MDI shell, flowsheet canvas, equipment dialogs, job I/O, results graphing, and unit conversion.
- Choose the most suitable graphing library for the job; GPL licensing is acceptable, so Qt Charts is a permitted option.

**Non-Goals:**
- No engine changes (Fortran untouched).
- No job-file format modernization (no JSON/SQLite adapter in this change).
- No packaging/distribution (PyInstaller/conda) in this change — run via `python -m modsim`.
- No porting of the separate `ModsimCurveFitVB` curve-fitting app in this change.
- No full 1:1 port of all 67 dialogs if many are legacy — scope is driven by an audit of live models (see Open Questions).

## Decisions

### D1. Python + PySide6 over C++ Qt, Tauri, Electron, Avalonia
Qt (Python or C++) is the only stack with native MDI and a mature 2D scene graph. Between C++ and Python, Python wins on contributor pool (scientific lingua franca), maintainability, and iteration speed; the Fortran FFI via `ctypes` is trivial because every call is a path string. Tauri/Electron would require redesigning MDI into tabs and add a marshalling layer. **Alternative considered:** C++ Qt (cleanest ABI, zero marshalling) — rejected for C++ complexity tax with no real benefit since the GUI does no heavy computation.

### D2. Preserve the legacy job-file format
The GUI writes the same `.JOB/.syd/.siz/.gcd/.mat/.scd/.sid/.cur/.mop/.TEA/.TRN` files and calls the same 4 functions. This keeps the engine untouched and existing jobs (`Modsim/Jobs/`, `Modsim/JobsRPK/`) usable as test fixtures. **Alternative considered:** JSON/SQLite — rejected because it adds an engine-side adapter and breaks compatibility with existing jobs.

### D3. `ctypes` FFI bridge
Load the DLL by name (`ModsimMain.dll` / `libmodsim.so` / `libmodsim.dylib`) resolved via platform library search path or an explicit configured path. Declare the 4 functions with correct pointer widths for the target architecture (x64 → 64-bit pointers). Pass null-terminated platform-native path strings with a trailing separator. **Alternative considered:** `cffi` — viable but `ctypes` is stdlib and sufficient for this trivial surface.

### D4. MDI shell with `QMdiArea`
Map the VB6 MDI parent/child architecture directly: `QMdiArea` + `QMdiSubWindow` for the main window and flowsheet child windows; `QMenuBar`/`QToolBar`/`QStatusBar` for the shell chrome.

### D5. Flowsheet canvas with `QGraphicsScene`/`QGraphicsView`
Rebuild the 2D diagram editor (drag units, draw/rout streams, label/number units and streams, persist layout) on the Qt scene graph. This is the highest-risk component and is built as its own capability with a dedicated spec.

### D6. Equipment dialogs — data-driven where possible
Port the ~67 equipment-model forms as parameter-entry dialogs that read/write job-file sections. Prefer a data-driven schema (fields, types, units, bounds) that generates forms, so a new model = a new schema, not a new form. Scope is gated on an audit of which models are actually live (see Open Questions).

### D7. Graphing — Qt Charts preferred, matplotlib as fallback
GPL licensing is acceptable for this project, so Qt Charts (GPL v3) is a permitted and preferred option for size distributions, liberation spectra, and stream plots — it is purpose-built for Qt and integrates natively with the PySide6 UI. **Alternative considered:** matplotlib (BSD) embedded via `FigureCanvasQTAgg` — viable and already in the scientific ecosystem; use it if Qt Charts proves insufficient for a specific plot type.

### D8. Unit conversion as a data table
Port the `SetConversionFactors` table (m/cm/ft, kgps/stonphr, etc.) into a data-driven conversion module shared across dialogs.

## Risks / Trade-offs

- [Flowsheet canvas is the highest-risk component] → Build it as its own capability with a dedicated spec and validate against existing `.syd`/`.JOB` fixtures early; iterate on the scene-graph model before wiring the full app.
- [67 dialogs is a large porting surface] → Audit which models are actually live first; use a data-driven form generator to collapse repetitive work.
- [String encoding across the FFI boundary] → Pass null-terminated platform-native (UTF-8 on Linux/macOS, ANSI/UTF-8 on Windows) path strings with trailing separators; verify the engine reads them as `CHARACTER*N`.
- [Pointer width on x64] → Use correct `ctypes` types (`c_long`/`c_void_p` sized to the target) for the `ByRef As Long` parameters.
- [LGPL obligations for Qt] → Dynamic-link Qt and provide LGPL v3 text and a Qt usage notice. GPL-licensed Qt modules (Qt Charts) are permitted since GPL is acceptable for this project.
- [Python packaging is harder than a single binary] → Deferred to a later change; run from source via `python -m modsim` for now.
- [Engine cross-platform build (.so/.dylib) is unproven] → Tracked separately; the GUI is written to load whatever shared library name the platform produces.

## Migration Plan

1. Scaffold the Python package and `engine-bridge` first; validate `ctypes` calls against the existing 64-bit DLL and an existing job (`Modsim/Jobs/Bougainville`).
2. Build `job-file-io` and validate round-trip against existing job files.
3. Build `gui-shell` (MDI) and `flowsheet-canvas`; load and display an existing job.
4. Build `equipment-dialogs`, `unit-conversion`, and `results-graphing` incrementally.
5. Keep the VB6 GUI in place until the rewrite reaches functional parity; the VB6 project is superseded but retained for reference.

## Open Questions

- How many of the ~67 equipment models are actually live/used? (Determines whether data-driven forms are worth building vs. porting individually.)
- Should the separate `ModsimCurveFitVB` curve-fitting app be ported in this change or a follow-up?
- What is the target Python version and packaging story for end users?
