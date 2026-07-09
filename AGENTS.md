# AGENTS.md

## What This Project Is

ModSIM is a mineral processing plant simulator (v3.6.15). It has two components:
- **Fortran DLL** (`Modsim/Modsimdl/`) — the simulation engine, built as `ModsimMain.dll`
- **VB6 GUI** (`Modsim/Modsimvb/`) — flowsheet drawing and job control; VB6 IDE only, no CLI build

A separate VB6 graphics package (`Vetgraph/`) is also included.

---

## Build

### Fortran DLL — command line (from `Modsim/Modsimdl/`)

```sh
make                          # release, gfortran
make COMPILER=ifx             # release, Intel IFX (oneAPI)
make BUILD=debug              # debug, gfortran
make COMPILER=ifx BUILD=debug # debug, IFX
make clean                    # clean gfortran/release
```

Output: `build/<compiler>/<build>/ModsimMain.dll` + `ModsimMain.lib`

### Fortran DLL — Visual Studio

Open `ModS/ModS.sln` in VS2022 with the Intel Fortran extension. Configs: `Debug|Win32`, `Debug|x64`, `Release|Win32`, `Release|x64`.

### GUI

Open `Modsim/Modsimvb/Modsim.vbp` in **Visual Basic 6.0 IDE**. No CLI build.

---

## Critical Toolchain Facts

**Source compilation order is enforced** — modules must compile before consumers:
1. `GLOBALS.F90`
2. `ModelVariables.f90`
3. `SIMOPMOD.F90`
4. All other sources

**`GLOBALS.F90` is auto-generated** — do not hand-edit. It is produced by `DIMINP.FOR`. If mesh sizes or stream counts change, re-run `DIMINP.FOR`.

**UserModels is a pre-built static library** — `UserModels/$(BUILD)/UserModels.lib` must exist before linking the DLL. Default versions live in `UserModelsDefault/`; customised versions in `UserModels/`.

**gfortran stubs** — `msflib_stub.f90` and `portlib_stub.f90` are only compiled with gfortran. With IFX the real Intel modules are used. Adding new `USE msflib` / `USE portlib` requires updating `MSFLIB_USERS` / `PORTLIB_USERS` in the GNUmakefile.

**IMSL required at link time** — links `-limsl -lIMSLMPISTUB -lIMSLS_ERR`. Without IMSL installed the DLL links but numerical routines fail at runtime.

**Windows-only** — links `kernel32`, `user32`, `gdi32`, etc. gfortran must be from MinGW/MSYS2.

**Mixed file extensions** — legacy files use `.FOR` (uppercase); newer modules use `.f90` or `.F90`. The GNUmakefile has separate pattern rules for each. Match the existing convention when adding files.

**`ModS.vfproj` must stay in sync** — when renaming or adding source files, update `ModS/ModS.vfproj`. The `tools/modernize_fortran.py` script does this automatically; manual renames without updating the vfproj break the VS build silently.

**`PARSET_CLAUDE.F90` is experimental** — `ModS.vfproj` references both `PARSET.f90` (listed twice — duplicate entry bug) and `PARSET_CLAUDE.F90`. Treat `PARSET.f90` as authoritative.

---

## Fortran Modernisation Script

```sh
python tools/modernize_fortran.py
```

Converts listed `.FOR` files (Fortran 77 fixed-form) to `.f90` free-form: continuation style, `.EQ.` → `==`, `CHARACTER*N` → `character(len=N)`, labelled DO loops → `do...end do`, inserts `IMPLICIT NONE`, lowercases keywords. **Deletes the original `.FOR` files** and patches `ModS.vfproj`.

**Hardcoded paths** at lines 37–39 of the script point to `C:\Users\User\Repos\ModSIm\...`. Update them if the repo is cloned to a different location.

---

## No Automated Tests / CI

No test framework and no CI workflows. Validation is done by running simulation jobs in `Modsim/Jobs/` and `Modsim/JobsRPK/` manually.

---

## Conventions

- Legacy Fortran filenames are ALL-CAPS (`CALC.FOR`, `SIMULATE.FOR`); preserve this for existing files.
- New F90 module files use mixed case (`ModelVariables.f90`).
- `~` suffix files are editor backups — do not commit.
- `.iwz` files in `Modsimvb/` are InstallShield installer projects for various release versions.
- `Modsim.dll` checked into `Modsimdl/` is a pre-built binary, not a build artifact from the current source.
