# ModSIM GUI (PySide6)

A cross-platform desktop GUI for the ModSIM mineral processing plant simulator,
replacing the legacy VB6 interface. It is written in Python with PySide6 (Qt6)
and calls the 64-bit Fortran simulation engine (`ModsimMain.dll` / `libmodsim.so`
/ `libmodsim.dylib`) through a thin `ctypes` bridge.

The legacy text job-file format is preserved, so existing jobs in
`Modsim/Jobs/` and `Modsim/JobsRPK/` remain valid and the Fortran engine is
untouched.

## Requirements

- Python 3.10+
- PySide6 (Qt6) — LGPL
- numpy
- Qt Charts (GPL v3) — ships with PySide6 Addons; acceptable for this project
- matplotlib (optional, fallback for some plot types)

Install dependencies:

```sh
pip install -e .
```

## Running

### Quick start

From the **repo root**:

```sh
make          # build the Fortran engine DLLs (default: ifx release)
make run      # launch the GUI (sets MODSIM, ensures Python deps)
```

`make run` handles steps 1, 3, and 4 below automatically. The detailed steps are
kept for reference and for running without the top-level Makefile.

### 1. Install the Python dependencies

From the `Modsim/Modsimpy/` directory:

```sh
pip install -e .
```

### 2. Build (or locate) the engine DLL

The GUI needs the 64-bit Fortran engine. A pre-built release DLL is present at:

```
Modsim/Modsimdl/build/ifx/release/ModsimMain.dll
```

To rebuild it from source (see `Modsim/Modsimdl/GNUmakefile`):

```sh
cd Modsim/Modsimdl
make COMPILER=ifx        # release, Intel IFX (oneAPI)
```

The engine has no external math-library dependency: the IMSL routines were
replaced with pure-Fortran implementations, so no IMSL/MKL runtime is needed.
The DLL only depends on `UserModels.dll`, which is built alongside it in the
same directory.

### 3. Point `MODSIM` at the engine directory

The GUI resolves the engine library through the `MODSIM` environment variable
(an explicit path, then the platform search path). Set it to the directory
containing `ModsimMain.dll`:

Windows (PowerShell):

```powershell
$env:MODSIM = "C:\Users\User\Repos\ModSIm\Modsim\Modsimdl\build\ifx\release"
```

Linux / macOS:

```sh
export MODSIM=/path/to/engine/dir
```

### 4. Launch the GUI

From the `Modsim/Modsimpy/` directory:

```sh
python -m modsim
```

Or, after installing the package:

```sh
modsim
```

## Running a simulation

1. **Open a job directory** — File > Open, or the toolbar Open button. Pick a
   directory containing a legacy job (`.JOB` and companion files). For example,
   `Modsim/Jobs/Distribution jobs/` contains the complete `Bougainville` job.
2. **Edit the flowsheet** — drag units from the palette onto the canvas and
   draw streams between them. The layout is part of the job and is saved with
   it.
3. **Run** — File > Run, or the toolbar Run button. The GUI persists the job
   to its directory (including `TEARS.OUT` and `DATT.DAT`), then invokes the
   Fortran engine (`INORDCALC` then `SIMOP`).
4. **View results** — on success the result files are parsed and a results
   viewer opens in a new window with size-distribution, liberation, and stream
   plots.

## Troubleshooting

- **"Could not load the ModSIM engine"** — the DLL was not found. Set `MODSIM`
  to the directory containing `ModsimMain.dll` (step 3 above) or build the DLL
  first.
- **Simulation fails with a numerical/convergence error** — the job does not
  converge (e.g. a stale `DATT.DAT` or bad unit parameters). The engine has no
  IMSL dependency, so this is not a missing-library issue.
- **A second simulation in the same session crashes** — the Fortran engine is
  not re-entrant: its global state is not reset between runs. Restart the GUI
  between simulations.

## Engine library

The GUI loads the engine shared library by name, resolved through the platform
library search path or an explicit configured path:

- Windows: `ModsimMain.dll`
- Linux: `libmodsim.so`
- macOS: `libmodsim.dylib`

Set the `MODSIM` environment variable to the directory containing the engine
library and/or the default job directory if it is not on the search path.

## Job directories

A job is a directory of legacy text files (`.JOB`, `.syd`, `.siz`, `.gcd`,
`.mat`, `.scd`, `.sid`, `.cur`, `.mop`, `.TEA`, `.TRN`). The default job path is
resolved from the `MODSIM` environment variable, falling back to a configured
default, then a temporary directory.

## Project layout

```
Modsim/Modsimpy/
  pyproject.toml
  README.md
  modsim/
    __main__.py        # python -m modsim entry point
    engine/            # ctypes bridge to the Fortran DLL
    io/                # legacy job-file readers/writers
    models/            # in-memory job model
    units/             # unit-conversion tables
    gui/
      app.py           # application shell (MDI)
      canvas/          # flowsheet editor (QGraphicsScene)
      dialogs/         # equipment parameter dialogs
      plotting/        # results graphing (Qt Charts / matplotlib)
```

## Cross-platform notes

- The GUI is pure Python + Qt and runs on Windows, Linux, and macOS.
- The Fortran engine must be built for each platform (`.dll` / `.so` /
  `.dylib`); a build for one platform does not run on another.
- The engine has no IMSL dependency — the IMSL routines were replaced with
  pure-Fortran implementations, so no IMSL/MKL runtime is required.
- Path strings passed to the engine are fixed-length `CHARACTER*255` buffers
  (space-padded, never NUL-terminated) with a trailing separator, matching the
  Fortran `LEN_TRIM` contract.

## Licensing

This GUI is built on Qt via PySide6.

- **Qt core modules** (QtCore, QtWidgets, QtGui) are licensed under the GNU
  **LGPL v3**. Qt is **dynamically linked** (no static linking), which
  satisfies the LGPL obligations for this application.
- **Qt Charts** is licensed under the GNU **GPL v3** and is used for the
  size-distribution and liberation plots. GPL is acceptable for this project.
- **matplotlib** (BSD) is used as an optional fallback for stream plots.
- **numpy** (BSD) is used for numerical work.

### Qt usage notice

This application uses Qt under the GNU LGPL v3 and Qt Charts under the GNU GPL
v3. The relevant license texts are available from The Qt Company and are
included with the PySide6 distribution. Downstream users and distributors
should retain these notices and comply with the terms of the LGPL v3 (for the
dynamically linked Qt core modules) and the GPL v3 (for Qt Charts).
