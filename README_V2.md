# ModSIM — Reformed Application (V2)

ModSIM is a mineral processing plant simulator. This document describes the
**reformed (modernised) application**: a cross-platform Python GUI that drives
a 64-bit Fortran simulation engine, replacing the legacy 32-bit VB6 + Compaq
Visual Fortran + IMSL stack.

The legacy text job-file format is preserved byte-for-byte, so existing jobs in
`Modsim/Jobs/` and `Modsim/JobsRPK/` remain valid and run unchanged.

---

## Architecture

```
┌──────────────────────────────────────────────────────────────┐
│  Python GUI (PySide6 / Qt6)          Modsim/Modsimpy/         │
│  flowsheet editor · job management · results viewer           │
└───────────────────────────┬──────────────────────────────────┘
                            │ ctypes bridge (modsim/engine/)
                            ▼
┌──────────────────────────────────────────────────────────────┐
│  Fortran engine (64-bit)              Modsim/Modsimdl/        │
│  ModsimMain.dll · UserModels.dll · ModsimCurveFit.dll         │
└──────────────────────────────────────────────────────────────┘
```

- **GUI** — pure Python + Qt. Flowsheet drawing, job editing, simulation
  control, and results graphing. Runs on Windows, Linux, and macOS.
- **Engine bridge** — a thin `ctypes` layer (`modsim/engine/engine_bridge.py`)
  that loads the engine DLL and exposes four entry points: `INORDCALC`,
  `SIMOP`, `LJUBAMD`, `BETAAMD`.
- **Engine** — the original Fortran simulation engine, ported from 32-bit
  Compaq Visual Fortran 6.6 to 64-bit Intel IFX (oneAPI 2025.3) / gfortran.
  The IMSL numerical library (32-bit x86 only) was removed and replaced with
  pure-Fortran and MKL implementations.

---

## Developer experience

The project builds and runs with one command each, from the repo root:

```sh
make          # build the Fortran engine DLLs (default: ifx release)
make run      # launch the ModSIM GUI
```

| Command | Description |
|---|---|
| `make` | Build `ModsimMain.dll` + `UserModels.dll` + `ModsimCurveFit.dll` |
| `make run` | Set `MODSIM`, ensure Python deps, launch `python -m modsim` |
| `make install` | One-time: `pip install -e` the GUI package |
| `make clean` | Remove build artefacts |
| `make help` | List all commands |

**Zero manual environment setup.** On Windows, `scripts/build-ifx.ps1`
auto-detects Visual Studio 2022 and Intel oneAPI (searching standard install
locations, no hardcoded paths), sources their build environment, then runs
make. On Linux, `scripts/build-ifx.sh` does the same with oneAPI's
`setvars.sh`. If oneAPI isn't installed, fall back to gfortran:
`make COMPILER=gfortran`.

**Cross-platform.** The same `make` / `make run` commands work on Windows and
Linux. On Windows the engine is `ModsimMain.dll`; on Linux it is
`libmodsim.so` (matched by the `ctypes` bridge). `make run` sets
`LD_LIBRARY_PATH` on Linux so dependent `.so` files find each other at runtime.

**Runs without building first.** A pre-built IFX release DLL is checked in at
`Modsim/Modsimdl/build/ifx/release/ModsimMain.dll`, so `make run` works
immediately on a fresh clone.

**VS Code integration.** `Ctrl+Shift+B` builds; the "Run ModSIM GUI" launch
config runs the GUI with the build as a pre-launch task.

---

## What changed vs. the legacy application

| Aspect | Legacy | Reformed (V2) |
|---|---|---|
| GUI | Visual Basic 6.0 (`Modsim/Modsimvb/`) | Python + PySide6 (`Modsim/Modsimpy/`) |
| Engine compiler | Compaq Visual Fortran 6.6 (32-bit) | Intel IFX 2025.3 / gfortran (64-bit) |
| Numerical library | IMSL (32-bit x86 only) | Pure-Fortran + MINPACK + MKL |
| Build system | NMAKE `.mak` | GNU make `GNUmakefile` (top-level) |
| Architecture | x86 (32-bit) | x64 (64-bit) |
| Platforms | Windows only | Windows + Linux (same `make` / `make run`) |
| Graphics | Vetgraph (VB6) | Qt Charts / matplotlib |
| Developer entry point | multiple manual steps | `make` / `make run` (one command each) |

### IMSL removal

IMSL ships as 32-bit x86 only and cannot link into a 64-bit DLL. All IMSL
calls were replaced:

- `BETAI` / `DBETAI` / `ANORDF` → pure-Fortran in `BETAAMD.FOR`
- `BCLSF` → MINPACK `lmdif` wrapper (`NumericalReplace.f90`)
- `EVCRG` → MKL `SGEEV` wrapper (`NumericalReplace.f90`)
- `RNSET` / `RNUN` → no-op stubs

The engine DLL now imports only `KERNEL32.dll`, `UserModels.dll`, and
`imagehlp.dll` — no IMSL/MKL runtime is required.

---

## Repository layout

```
ModSIm/
  GNUmakefile         # top-level entry point: make / make run / make install
  README.md           # legacy (VB6-era) readme
  README_V2.md        # this document
  AGENTS.md           # AI-agent build/toolchain notes
  scripts/
    build-ifx.ps1     # Windows: auto-detects VS2022 + oneAPI, sources env, runs make
    build-ifx.sh      # Linux:   auto-detects oneAPI setvars.sh, runs make
    run.ps1           # Windows: sets MODSIM, ensures Python deps, launches GUI
    run.sh            # Linux:   sets MODSIM + LD_LIBRARY_PATH, launches GUI
  Modsim/
    Modsimdl/          # Fortran engine sources + GNUmakefile
    Modsimpy/          # Python GUI (PySide6) + ctypes bridge
    Modsimvb/          # legacy VB6 GUI (unchanged, VB6 IDE only)
    Jobs/              # example simulation jobs
    JobsRPK/           # more example jobs
  ModS/                # VS2022 solution for the IFX engine
  Vetgraph/            # legacy VB6 graphics package
  docs/                # modernization reports (CHANGES, BUILD_REPORT, ...)
  tools/               # Fortran modernisation script
```

---

## Building the engine

### Quick start (one command)

From the **repo root**:

```sh
make              # release build with Intel IFX (default)
make clean        # remove build artefacts
make help         # list all commands
```

`make` defaults to Intel IFX (oneAPI). `scripts/build-ifx.ps1` auto-detects
and sources Visual Studio 2022 + Intel oneAPI, so no manual environment
setup is needed — just run `make`.

To build with gfortran instead (no oneAPI required):

```sh
make COMPILER=gfortran          # release, gfortran
make COMPILER=gfortran BUILD=debug   # debug, gfortran
make COMPILER=ifx BUILD=debug   # debug, IFX
```

Output: `Modsim/Modsimdl/build/<compiler>/<build>/ModsimMain.dll` plus
`UserModels.dll` and `ModsimCurveFit.dll` (build order: `UserModels.dll` →
`ModsimMain.dll` → `ModsimCurveFit.dll`).

A pre-built IFX release DLL is checked in at
`Modsim/Modsimdl/build/ifx/release/ModsimMain.dll`, so `make run` works
immediately without building first.

### How the build is wired

The top-level `GNUmakefile` is a thin dispatcher that detects the OS and
calls the appropriate helper:

- **IFX on Windows** (default) — delegates to `scripts/build-ifx.ps1`, which
  sources `vcvarsall.bat x64` + oneAPI `setvars.bat intel64` into the process
  environment, then invokes the engine `GNUmakefile` in `Modsim/Modsimdl/`.
  The script searches standard install locations on `C:\`, `N:\`, and `D:\`
  for VS2022 (Community / Professional / Enterprise / BuildTools) and oneAPI,
  so there are no hardcoded paths.
- **IFX on Linux** — delegates to `scripts/build-ifx.sh`, which sources
  oneAPI `setvars.sh` (searching `/opt/intel/oneapi/`, `~/intel/oneapi/`),
  then invokes the engine GNUmakefile.
- **gfortran** (either OS) — invokes `make -C Modsim/Modsimdl` directly;
  gfortran only needs to be on `PATH`.

### Engine GNUmakefile (direct, advanced)

From `Modsim/Modsimdl/`:

```sh
make                          # release, gfortran  (default)
make COMPILER=ifx             # release, Intel IFX (oneAPI)
make BUILD=debug              # debug,   gfortran
make COMPILER=ifx BUILD=debug # debug,   IFX
make clean                    # clean gfortran/release
```

**IFX on Windows** requires both `vcvarsall.bat x64` and the oneAPI
`setvars.bat intel64` to be sourced first — this is what `scripts/build-ifx.ps1`
does automatically. If you invoke the engine GNUmakefile directly with
`COMPILER=ifx`, you must source them yourself.

**Compilation order is enforced** — `GLOBALS.F90` → `ModelVariables.f90` →
`SIMOPMOD.F90` → everything else. `GLOBALS.F90` is auto-generated by
`DIMINP.FOR`; do not hand-edit it.

**UserModels** is a pre-built static library that must exist before linking
(`UserModels/$(BUILD)/UserModels.lib`).

### Visual Studio

Open `ModS/ModS.sln` in VS2022 with the Intel Fortran extension. Configs:
`Debug|Win32`, `Debug|x64`, `Release|Win32`, `Release|x64`.

---

## Installing the GUI

### Quick start

From the **repo root**:

```sh
make install      # pip install -e the Modsimpy GUI package (one-time)
```

`make run` also auto-installs the Python dependencies on first launch, so this
step is optional — `make run` alone is sufficient.

### Manual install

From `Modsim/Modsimpy/`:

```sh
pip install -e .
```

Requires Python 3.10+, PySide6, numpy (matplotlib optional).

---

## Running

### Quick start (one command)

From the **repo root**:

```sh
make run          # launch the ModSIM GUI
```

`make run` does three things automatically:

1. Points the `MODSIM` environment variable at the built engine directory
   (`Modsim/Modsimdl/build/<compiler>/<build>/`).
2. Ensures the Python GUI package is installed (runs `pip install -e .` on
   first launch; subsequent launches skip this).
3. Launches `python -m modsim`.

If the engine DLL is missing, `make run` prints a clear error and points you to
`make`. The pre-built IFX release DLL means `make run` works immediately,
without building first.

To run against a different build tree:

```sh
make run COMPILER=gfortran           # run against a gfortran build
make run COMPILER=ifx BUILD=debug    # run against an IFX debug build
```

### Manual setup (without the top-level Makefile)

#### 1. Point `MODSIM` at the engine directory

The GUI resolves the engine library through the `MODSIM` environment variable
(an explicit path, then the platform search path). On Linux, you also need
`LD_LIBRARY_PATH` so the dynamic linker finds dependent `.so` files
(`libUserModels.so`, `libModsimCurveFit.so`) in the same directory — `make run`
sets this automatically.

Windows (PowerShell):

```powershell
$env:MODSIM = "C:\Users\User\Repos\ModSIm\Modsim\Modsimdl\build\ifx\release"
```

Linux / macOS:

```sh
export MODSIM=/path/to/engine/dir
export LD_LIBRARY_PATH=/path/to/engine/dir:${LD_LIBRARY_PATH:-}
```

#### 2. Launch the GUI

From `Modsim/Modsimpy/`:

```sh
python -m modsim
```

or, after installing the package:

```sh
modsim
```

### Run a simulation

1. **Open a job directory** — File > Open. Pick a directory containing a
   legacy job (`.JOB` and companion files), e.g. `Modsim/Jobs/Distribution jobs/`
   (complete `Bougainville` job).
2. **Edit the flowsheet** — drag units from the palette onto the canvas and
   draw streams between them.
3. **Run** — File > Run. The GUI persists the job (including `TEARS.OUT` and
   `DATT.DAT`), then invokes `INORDCALC` then `SIMOP`.
4. **View results** — result files are parsed and a results viewer opens with
   size-distribution, liberation, and stream plots.

---

## Job format

A job is a directory of legacy text files (`.JOB`, `.syd`, `.siz`, `.gcd`,
`.mat`, `.scd`, `.sid`, `.cur`, `.mop`, `.TEA`, `.TRN`). The format is
preserved from the original application, so existing jobs run unchanged.

The GUI regenerates two derived files before each run so the engine never
reads stale data:

- `TEARS.OUT` — a copy of the job's `.TEA` (tear-stream data).
- `DATT.DAT` — the job's `.sid` system/plant data concatenated with its `.cur`
  run data (mirrors the VB6 `FileConCat` behaviour).

---

## Testing

The Python harness uses pytest:

```sh
cd Modsim/Modsimpy
python -m pytest tests/ -q
```

The engine-bridge tests load the real DLL when `MODSIM` is set; otherwise they
skip gracefully. There are no automated tests for the Fortran engine itself —
validation is done by running jobs manually.

---

## Known limitations

- **The engine is not re-entrant.** Its Fortran global state is not reset
  between runs; invoking `SIMOP` twice in the same process crashes with an
  access violation. Restart the GUI between simulations.
- **The legacy VB6 GUI is unchanged** and still requires the VB6 IDE (no CLI
  build). The Python GUI is the forward path.
- **`ModsimCurveFit.dll`** has no automated runtime test harness yet.
- **`BCLSF`** is hardcoded to call `TERMS` (see `docs/build/BUILD_REPORT.md`).

---

## Licensing

- **GUI**: Qt via PySide6 — Qt core modules under LGPL v3 (dynamically
  linked); Qt Charts under GPL v3; matplotlib (BSD); numpy (BSD).
- **Engine**: original Fortran sources; MINPACK is public domain (Argonne
  National Lab, 1980).
- **MKL** is linked into `ModsimCurveFit.dll` (Intel oneAPI; see Intel's
  licensing terms).
