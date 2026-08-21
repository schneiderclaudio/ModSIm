# AGENTS.md

## What This Project Is

ModSIM is a mineral processing plant simulator (v3.6.15). It has two components:
- **Fortran DLL** (`Modsim/Modsimdl/`) — the simulation engine, built as `ModsimMain.dll`
- **VB6 GUI** (`Modsim/Modsimvb/`) — flowsheet drawing and job control; VB6 IDE only, no CLI build

A separate VB6 graphics package (`Vetgraph/`) is also included.

---

## Build

### Quick start (one command)

From the **repo root**:

```sh
make              # build the Fortran engine DLLs (default: ifx release)
make run          # launch the ModSIM GUI (PySide6)
make install      # one-time: install the Python GUI dependencies
make clean        # remove build artefacts
make help         # list all commands
```

`make` defaults to Intel IFX (oneAPI). On Windows, `scripts/build-ifx.ps1`
auto-detects and sources Visual Studio 2022 + Intel oneAPI; on Linux,
`scripts/build-ifx.sh` does the same with oneAPI's `setvars.sh`. No manual env
setup is needed on either OS. To build with gfortran instead (no oneAPI
required): `make COMPILER=gfortran`.

`make run` points `MODSIM` at the built engine directory, ensures the Python
package is installed, then runs `python -m modsim`. On Linux it also sets
`LD_LIBRARY_PATH` so dependent `.so` files find each other. The pre-built IFX
release DLL at `Modsim/Modsimdl/build/ifx/release/ModsimMain.dll` lets
`make run` work immediately on Windows without building first.

VS Code: `Ctrl+Shift+B` builds, and the "Run ModSIM GUI" launch config runs the
GUI with the build as a pre-launch task.

### Fortran DLL — command line (from `Modsim/Modsimdl/`)

```sh
make                          # release, gfortran
make COMPILER=ifx             # release, Intel IFX (oneAPI)
make BUILD=debug              # debug, gfortran
make COMPILER=ifx BUILD=debug # debug, IFX
make clean                    # clean gfortran/release
```

Output: `build/<compiler>/<build>/ModsimMain.dll` + `ModsimMain.lib` (Windows);
`libmodsim.so` + `libUserModels.so` + `libModsimCurveFit.so` (Linux). The
engine GNUmakefile auto-detects the OS and uses the correct shared-library
naming and link style.

### Fortran DLL — Visual Studio

Open `ModS/ModS.sln` in VS2022 with the Intel Fortran extension. Configs: `Debug|Win32`, `Debug|x64`, `Release|Win32`, `Release|x64`.

### GUI

The current GUI is the PySide6 (Qt6) app in `Modsim/Modsimpy/`, run via
`make run` (or `python -m modsim` from `Modsim/Modsimpy/`). It calls the
Fortran engine through a `ctypes` bridge (`modsim/engine/engine_bridge.py`),
resolving `ModsimMain.dll` via the `MODSIM` environment variable.

The legacy VB6 GUI (`Modsim/Modsimvb/Modsim.vbp`) is opened in the Visual Basic
6.0 IDE only — no CLI build.

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

**No IMSL dependency** — the IMSL routines were replaced with pure-Fortran implementations (`IMSL_LIBS` is empty in the GNUmakefile for both gfortran and IFX). The DLL imports only `KERNEL32.dll`, `UserModels.dll`, and `imagehlp.dll`; no IMSL/MKL runtime is required.

**Cross-platform** — on Windows the engine links `kernel32`, `user32`,
`gdi32`, etc. (gfortran must be from MinGW/MSYS2). On Linux the engine links
`pthread`, `m`, `dl` and produces `libmodsim.so` + `libUserModels.so` +
`libModsimCurveFit.so`. The GNUmakefile auto-detects the OS and uses the
correct shared-library naming, link flags (`--out-implib` on Windows,
`-rpath=$ORIGIN` on Linux), and link dependencies (import libs on Windows,
direct `.so` linking on Linux).

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

<!-- BEGIN BEADS INTEGRATION v:1 profile:minimal hash:970c3bf2 -->
## Beads Issue Tracker

This project uses **bd (beads)** for issue tracking. Run `bd prime` to see full workflow context and commands.

### Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --claim  # Claim work
bd close <id>         # Complete work
```

### Rules

- Use `bd` for ALL task tracking — do NOT use TodoWrite, TaskCreate, or markdown TODO lists
- Run `bd prime` for detailed command reference and session close protocol
- Use `bd remember` for persistent knowledge — do NOT use MEMORY.md files

**Architecture in one line:** issues live in a local Dolt DB; sync uses `refs/dolt/data` on your git remote; `.beads/issues.jsonl` is a passive export. See https://github.com/gastownhall/beads/blob/main/docs/SYNC_CONCEPTS.md for details and anti-patterns.

## Agent Context Profiles

The managed Beads block is task-tracking guidance, not permission to override repository, user, or orchestrator instructions.

- **Conservative (default)**: Use `bd` for task tracking. Do not run git commits, git pushes, or Dolt remote sync unless explicitly asked. At handoff, report changed files, validation, and suggested next commands.
- **Minimal**: Keep tool instruction files as pointers to `bd prime`; use the same conservative git policy unless active instructions say otherwise.
- **Team-maintainer**: Only when the repository explicitly opts in, agents may close beads, run quality gates, commit, and push as part of session close. A current "do not commit" or "do not push" instruction still wins.

## Session Completion

This protocol applies when ending a Beads implementation workflow. It is subordinate to explicit user, repository, and orchestrator instructions.

1. **File issues for remaining work** - Create beads for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **Handle git/sync by active profile**:
   ```bash
   # Conservative/minimal/default: report status and proposed commands; wait for approval.
   git status

   # Team-maintainer opt-in only, unless current instructions forbid it:
   git pull --rebase
   bd dolt push
   git push
   git status
   ```
5. **Hand off** - Summarize changes, validation, issue status, and any blocked sync/commit/push step

**Critical rules:**
- Explicit user or orchestrator instructions override this Beads block.
- Do not commit or push without clear authority from the active profile or the current user request.
- If a required sync or push is blocked, stop and report the exact command and error.
<!-- END BEADS INTEGRATION -->

<!-- BEGIN BEADS CODEX SETUP: generated by bd setup codex -->
## Beads Issue Tracker

Use Beads (`bd`) for durable task tracking in repositories that include it. Use the `beads` skill at `.agents/skills/beads/SKILL.md` (project install) or `~/.agents/skills/beads/SKILL.md` (global install) for Beads workflow guidance, then use the `bd` CLI for issue operations.

### Quick Reference

```bash
bd ready                # Find available work
bd show <id>            # View issue details
bd update <id> --claim  # Claim work
bd close <id>           # Complete work
bd prime                # Refresh Beads context
```

### Rules

- Use `bd` for all task tracking; do not create markdown TODO lists.
- Run `bd prime` when Beads context is missing or stale. Codex 0.129.0+ can load Beads context automatically through native hooks; use `/hooks` to inspect or toggle them.
- Keep persistent project memory in Beads via `bd remember`; do not create ad hoc memory files.

**Architecture in one line:** issues live in a local Dolt DB; sync uses `refs/dolt/data` on your git remote; `.beads/issues.jsonl` is a passive export. See https://github.com/gastownhall/beads/blob/main/docs/SYNC_CONCEPTS.md for details and anti-patterns.
<!-- END BEADS CODEX SETUP -->
