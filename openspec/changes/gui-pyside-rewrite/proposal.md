## Why

The VB6 GUI is 32-bit-only and cannot call the newly ported 64-bit Fortran engine (`ModsimMain.dll`, built with Intel IFX 2025.3.2). This is a hard bitness wall — there is no 64-bit VB6, so the current GUI literally cannot run the new engine. The GUI is also built only in the VB6 IDE (no CLI build), has a shrinking contributor pool, and is Windows-only. We need a modern, cross-platform, open-source-friendly GUI that can call the 64-bit engine and unblock runtime testing and use of the IFX port.

## What Changes

- **Replace the VB6 GUI** (`Modsim/Modsimvb/`) with a new **Python + PySide6 (Qt6)** desktop application.
- **Preserve the existing file-based job contract** — the new GUI writes the same `.JOB/.syd/.siz/.gcd/.mat/.scd/.sid/.cur/.mop/.TEA/.TRN` text files and calls the same 4 path-string DLL functions (`INORDCALC`, `SIMOP`, `LJUBAMD`, `BetaAMD`). The engine is **untouched**.
- **Call the 64-bit DLL via `ctypes`** — trivial FFI since every call is `function(path_string, ...)`.
- **Rebuild the MDI shell** with `QMdiArea`/`QMdiSubWindow` (menu, toolbar, status bar).
- **Rebuild the flowsheet canvas** (the crown jewel) with `QGraphicsScene`/`QGraphicsView`: drag units, draw streams/connectors, label/number units and streams, persist layout.
- **Rebuild the ~67 equipment-model dialogs** as parameter-entry forms that write to job files.
- **Rebuild results/graphing** using the scientific Python ecosystem (matplotlib) for size distributions, liberation spectra, and stream plots.
- **Cross-platform** — runs on Windows, Linux, and macOS.
- **BREAKING:** The VB6 GUI is replaced. Existing `.JOB` job files remain compatible (format preserved), but the VB6 project is no longer the GUI.

## Capabilities

### New Capabilities
- `gui-shell`: The PySide6 MDI application shell — main window, menu bar, toolbar, status bar, job open/save, and window management.
- `flowsheet-canvas`: The 2D flowsheet editor built on QGraphicsScene — drag-and-drop units, draw and route streams, label/number units and streams, persist and load layout.
- `equipment-dialogs`: Parameter-entry dialogs for equipment models that read/write the job-file data sections.
- `job-file-io`: Read/write of the legacy text job-file format (`.JOB`, `.syd`, `.siz`, `.gcd`, `.mat`, `.scd`, `.sid`, `.cur`, `.mop`, `.TEA`, `.TRN`).
- `engine-bridge`: The `ctypes` FFI layer that loads the 64-bit Fortran DLL and calls `INORDCALC`, `SIMOP`, `LJUBAMD`, and `BetaAMD` with path-string arguments.
- `results-graphing`: Parsing of engine result files and rendering of size distributions, liberation spectra, and stream plots via matplotlib.
- `unit-conversion`: The unit-conversion tables (m/cm/ft, kgps/stonphr, etc.) used across the GUI.

### Modified Capabilities
<!-- No existing specs exist yet; all capabilities are new. -->

## Impact

- **Code:** New Python package under `Modsim/Modsimpy/` (or similar). The VB6 GUI (`Modsim/Modsimvb/`) is superseded but retained for reference until the rewrite reaches parity.
- **Engine:** **None** — the Fortran DLL is untouched. The job-file format is preserved, so existing jobs in `Modsim/Jobs/` and `Modsim/JobsRPK/` remain valid test fixtures.
- **Dependencies:** Python 3, PySide6 (LGPL), Qt Charts (GPL — acceptable for this project), numpy. matplotlib may be used as a fallback for specific plot types.
- **Build/run:** No CLI build for the GUI; run via `python -m modsim` (or a launcher script). Packaging via PyInstaller/conda is a later concern.
- **Cross-platform:** Windows, Linux, macOS. The engine must be built as `.dll`/`.so`/`.dylib` per platform (engine portability is tracked separately).
