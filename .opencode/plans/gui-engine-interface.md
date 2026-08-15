# ModSIM GUI ↔ Fortran Engine Interface

> **Purpose:** Document the contract between the GUI and the Fortran simulation engine, so that any GUI rewrite (in any language) can reproduce the interface correctly.
> **Date:** August 2026
> **Companion doc:** [`gui-modernization-comparison.md`](./gui-modernization-comparison.md)
> **Note:** This file was written to `.opencode/plans/` due to plan-mode write restrictions. Move to `docs/` when plan mode is exited.

---

## 1. The Contract at a Glance

The GUI and the Fortran engine are **decoupled by files on disk**, not by in-memory structs or complex APIs. The contract is:

1. The **GUI writes** a set of text files (the "job") into a directory.
2. The **GUI calls** one of 4 DLL functions, passing only a **directory path string**.
3. The **DLL reads** the job files from that directory, performs the simulation, and **writes result files** back into the same directory.
4. The **GUI reads** the result files and displays them.

```
┌──────────────────────────────────────────────────────────────────────┐
│                      THE FILE-BASED CONTRACT                         │
├──────────────────────────────────────────────────────────────────────┤
│                                                                      │
│   ┌────────────┐                     ┌──────────────────┐            │
│   │            │   1. writes job     │                  │            │
│   │   new GUI  │   files to dir      │   Fortran DLL    │            │
│   │            ├────────────────────▶│   (ModsimMain)   │            │
│   │            │   2. calls fn(path) │                  │            │
│   │            │                     │   3. reads job   │            │
│   │            │                     │      files      │            │
│   │            │                     │   4. simulates  │            │
│   │            │                     │   5. writes     │            │
│   │            │   6. reads results  │      result.out │            │
│   │            │◀────────────────────┤                  │            │
│   └────────────┘                     └──────────────────┘            │
│                                                                      │
│   No shared structs cross the boundary.                              │
│   Every call is: function(directoryPathString) → status code.        │
│                                                                      │
└──────────────────────────────────────────────────────────────────────┘
```

**Implication:** The `MODELVARS` struct in `CModelVariables.h` is **internal to the Fortran+C user-model layer** — the GUI never sees it. Any new GUI just needs to: produce the right job files → call 4 path-string functions → read result files.

---

## 2. The DLL API Surface

### 2.1 ModsimMain.dll — 4 exported functions

Source: `Modsim/Modsimvb/MODSIM.bas:9-12` (the current VB6 `Declare` statements):

```vbnet
Public Declare Function INORDCALC Lib "MODSIM.DLL" (ByVal JobPath As String) As Integer
Public Declare Function SIMOP     Lib "MODSIM.DLL" (ByVal JobPath As String, ByRef CumOut As Long) As Integer
Public Declare Function LJUBAMD   Lib "MODSIM.DLL" (ByVal JobPath As String, ByVal NameOfJob As String) As Long
Public Declare Function BetaAMD   Lib "MODSIM.DLL" Alias "BETAAMD" (ByVal JobPath As String, ByVal NameOfJob As String, ByRef Parameters As Single) As Long
```

| Function | DLL | VB6 Signature | Returns | Purpose |
|---|---|---|---|---|
| `INORDCALC` | `MODSIM.DLL` | `(ByVal JobPath As String) As Integer` | init result code | Job initialization + in-order calculation setup |
| `SIMOP` | `MODSIM.DLL` | `(ByVal JobPath As String, ByRef CumOut As Long) As Integer` | sim result code | **Main simulation execution** — runs the full flowsheet |
| `LJUBAMD` | `MODSIM.DLL` | `(ByVal JobPath As String, ByVal NameOfJob As String) As Long` | long | Ljubljana liberation model |
| `BetaAMD` (alias `BETAAMD`) | `MODSIM.DLL` | `(ByVal JobPath As String, ByVal NameOfJob As String, ByRef Parameters As Single) As Long` | long | Beta function liberation model |

### 2.2 ModsimCurveFit.dll — 1 exported function

Source: `Modsim/Modsimvb/ModsimCurveFitVB/MAINCurveFit.bas:116`:

```vbnet
Public Declare Sub LevMarCurveFit Lib "ModsimCurveFit.dll" ( _
    ByVal JobPath As String, _
    ByVal JobFileName As String, _
    ByRef NOPAR As Long, _
    ByRef Parameters As Single, _
    ByRef LowB As Single, _
    ByRef UpB As Single, _
    ByRef SumOfSquares As Single, _
    ByRef LowerConfLim As Single, _
    ByRef UpperConfLim As Single, _
    ByRef ConfLevel As Single)
```

This is a separate curve-fitting application (`ModsimCurveFitVB/`) that calls Levenberg-Marquardt fitting against model data. It is logically a separate app from the main GUI, but shares the same path-string contract.

### 2.3 Note on the current Declare paths

The active `Declare` statements in `MODSIM.bas` reference a hardcoded developer path:

```
Lib "D:\Documents and Settings\cschneid\My Documents\My Software\modsim\modsimdl\MODSIM.DLL"
```

Commented-out alternatives (lines 13-16) use just `"MODSIM.DLL"` — i.e. relying on DLL search path. A new GUI should load the DLL by name and resolve it via the platform's standard library search path (or an explicit configured path).

---

## 3. The Job-File Protocol

### 3.1 A "job" is a directory

A job is not a single file — it is a **directory containing ~11 text files**, each describing one aspect of the flowsheet and its data. The DLL receives the directory path and reads/writes files within it.

### 3.2 Job file extensions

| Extension | Purpose | Example content |
|---|---|---|
| `.JOB` | Boolean flags indicating which data sections are present | `"Flowsheet",#TRUE#` |
| `.syd` | Stream connections (UnitID, StreamID, Type, from, to) | `1 7 F 0 2` |
| `.siz` | Size distribution data | numeric, whitespace-delimited |
| `.gcd` | Grade-class distribution | numeric, whitespace-delimited |
| `.mat` | Material properties | small text file |
| `.scd` | S-class (size-class) distribution | numeric, whitespace-delimited |
| `.sid` | System / identifier data | larger text file |
| `.cur` | Current data | numeric |
| `.mop` | Model operating parameters | tiny text file |
| `.TEA` | (TEA data) | text |
| `.TRN` | Transform / transition data | text |

### 3.3 Example: Bougainville.JOB

Source: `Modsim/Jobs/Bougainville.JOB`

```text
"Job name","Bougainville"
"Flowsheet",#TRUE#
"System data",#TRUE#
"Model data",#TRUE#
"System input data",#TRUE#
"Size data",#TRUE#
"G-class distribution",#TRUE#
"Liberation data",#TRUE#
"Ljubljana model",#FALSE#
"Beta function liberation model",#FALSE#
"S-class distribution",#TRUE#
"Washability data",#FALSE#
"System data needed",#FALSE#
"Model parameter data",#TRUE#
"Unit parameters needed",#FALSE#
"Output format",#TRUE#
"SIMOP data",#TRUE#
```

The `.JOB` file is a series of `"label",value` pairs where value is either a quoted string or a VB-style boolean (`#TRUE#` / `#FALSE#`). It tells the engine which data sections to expect and which models to run.

### 3.4 Example: Bougainville.syd (stream connections)

Source: `Modsim/Jobs/Bougainville.syd`

```text
Bougainville
 9 
 1  7 F  0  2 
 8  14 W -1  3 
 9  15 W -1  1 
 2  8 O  2  5 
 3  9 O  5  1 
 4  10 O  1  3 
 5  11 O  3  4 
 6  12 O  4  5 
 7  13 O  4  0 
```

Line 1 is the job name. Line 2 is the stream count. Each subsequent line describes a stream: `unitID streamID type fromPort toPort` (where type `F` = feed, `O` = output, `W` = water; ports are connection points on units).

### 3.5 Example jobs for reference

Existing jobs live in:
- `Modsim/Jobs/` — e.g. `Bougainville`, `Bougainville1`, `Camille`, `CoalDemo`
- `Modsim/JobsRPK/` — e.g. `Blackbox job`, `Bougainville`

These are the canonical test fixtures for validating any new GUI's job-file generation.

---

## 4. Data Flow Diagram

```
  ┌─────────────────────────────────────────────────────────────────┐
  │  NEW GUI (any language)                                         │
  │                                                                 │
  │  1. User draws flowsheet on canvas                              │
  │     (drag units, connect streams, set parameters)              │
  │                                                                 │
  │  2. GUI serializes flowsheet → job files:                       │
  │     <jobDir>/MyJob.JOB   (flags)                               │
  │     <jobDir>/MyJob.syd   (streams)                             │
  │     <jobDir>/MyJob.siz   (size distributions)                  │
  │     <jobDir>/MyJob.gcd   (grade-class data)                    │
  │     <jobDir>/MyJob.mat   (materials)                           │
  │     <jobDir>/MyJob.scd   (size-class data)                     │
  │     <jobDir>/MyJob.sid   (system data)                         │
  │     <jobDir>/MyJob.cur   (current data)                        │
  │     <jobDir>/MyJob.mop   (model operating params)              │
  │     <jobDir>/MyJob.TEA   (...)                                  │
  │     <jobDir>/MyJob.TRN   (...)                                  │
  │                                                                 │
  │  3. GUI calls:                                                  │
  │       result = SIMOP(jobDirPath, &cumOut)                      │
  │     (first: INORDCALC(jobDirPath) for initialization)          │
  └────────────────────────────┬────────────────────────────────────┘
                               │
                               │  function(path)  —  just a string
                               │
  ┌────────────────────────────▼────────────────────────────────────┐
  │  Fortran DLL (ModsimMain.dll / .so / .dylib)                    │
  │                                                                 │
  │  4. Reads <jobDir>/*.JOB, *.syd, *.siz, ...                     │
  │  5. Runs simulation (models, convergence, numerics)              │
  │  6. Writes result files back to <jobDir>/:                      │
  │       REPORT2.OUT   (main results)                             │
  │       ...other .OUT files                                       │
  │     Sets CumOut (byref long)                                   │
  └────────────────────────────┬────────────────────────────────────┘
                               │
                               │  result files on disk
                               │
  ┌────────────────────────────▼────────────────────────────────────┐
  │  NEW GUI                                                        │
  │                                                                 │
  │  7. Reads <jobDir>/*.OUT                                        │
  │  8. Parses results                                              │
  │  9. Renders graphs/tables on canvas                             │
  │                                                                 │
  └─────────────────────────────────────────────────────────────────┘
```

---

## 5. What Stays Inside the Engine (NOT the GUI's Concern)

The following are **internal to the Fortran+C user-model layer** and do not cross the GUI↔engine boundary:

### 5.1 The `MODELVARS` struct

Source: `Modsim/Modsimdl/UserModels/CModelVariables.h`

```c
#pragma pack(2)
extern struct modsimvariables {
    int UnitNumber;
    int Reporting;
    int NumSizeClasses, NumGClasses, NumSClasses;
    int UnitType;
    float Feed[10][22][35];
    float Tailing[10][22][35];
    float Concentrate[10][22][35];
    float Middling[10][22][35];
    float TotalSolidsF;
    float TotalSolidsT, TotalSolidsC, TotalSolidsM;
    float FeedWater;
    float TailingsWater, ConcentrateWater, MiddlingsWater;
    float Parameters[150];
    float RepSize[35];
    int NumberOfMessages, NumberOfMinerals;
    float GradeM[7][22], GradeV[7][22];
    float SolidSpGr[22];
    float Texture[50];
    float MagnSusceptG[22];
    float OtherPropG[22];
    float FlotnRateConst[10];
    float MagnSusceptS[10];
    float OtherPropS[10];
    float CalValue[22];
    float TotalSulfur[22];
    float PyriticSulf[22];
    char JobName[80];
} MODELVARS;
```

This struct is shared between the Fortran `GLOBALS` module and C user-model routines (see `ConnectorToC.cpp`). It is the in-memory representation of a single unit's data during simulation — **the GUI never touches it**.

### 5.2 Other engine-internal components

| Component | Location | Role |
|---|---|---|
| `GLOBALS.F90` module | `Modsimdl/` | Auto-generated global declarations (by `DIMINP.FOR`) |
| `ModelVariables.f90` | `Modsimdl/` | Model variable definitions |
| `SIMOPMOD.F90` | `Modsimdl/` | Simulation operation module |
| `ConnectorToC.cpp` | `Modsimdl/UserModels/` | C++ bridge for user-written C models |
| Convergence logic | `CALC.FOR`, `CALC1.FOR` | Iterative solver internals |
| Numerical routines | `NumericalReplace.f90`, `minpack.f90` | MINPACK + MKL helpers |
| Model equations | `*MODS.FOR` files | Crusher, screen, mill, flotation, etc. |

A GUI rewrite does **not** need to understand or interact with any of these. The job files are the entire contract.

---

## 6. Implications for a GUI Rewrite

### 6.1 Language-agnostic

Because the contract is (a) write text files and (b) call `function(char* path)`, **any language** can implement the GUI side:

| Language | FFI mechanism | Difficulty |
|---|---|---|
| Rust (Tauri) | `libloading` crate + `extern "C"` | clean, type-safe |
| Python | `ctypes.CDLL` / `cdll.LoadLibrary` | trivial (3 lines) |
| C++ (Qt) | `extern "C"` / `QLibrary` | **native, zero marshalling** |
| C# (.NET) | `[DllImport]` / `LibraryImport` | clean |
| Node.js | `ffi-napi` | clunky, ABI-sensitive |

### 6.2 Preserve the job-file format (recommended)

Preserving the existing text-based job-file format means:
- The new GUI works with the **existing engine** immediately.
- The new GUI can open **existing jobs** from `Modsim/Jobs/` without conversion.
- The engine requires **zero changes**.
- The rewrite is safe — the engine is untouched and validated.

Modernizing to JSON/SQLite is technically cleaner but adds an engine-side adapter (or a converter layer) and breaks compatibility with existing jobs. **Not recommended for the initial rewrite.**

### 6.3 FFI is trivial because every call is `(char* path, ...)`

Every DLL function takes a null-terminated path string and optionally a few scalars. There are no complex structs, no callbacks, no shared memory. The FFI wrapper in any language is ~20 lines.

### 6.4 The only VB6-specific gotcha

The current VB6 `Declare` statements pass `ByVal String` which marshals VB's BSTR to an ANSI LSTR (null-terminated). A new GUI should pass **UTF-8 or platform-native null-terminated strings** and ensure the Fortran DLL can read them (the engine currently reads paths as Fortran `CHARACTER*N`).

Also: the VB6 `ByRef As Long` parameters are 32-bit pointers. On x64, these become 64-bit — the new GUI's FFI must use the correct pointer width for the target architecture.

---

## 7. Path-String Passing Convention

### 7.1 How the path is used

The GUI passes a **directory path** (e.g. `C:\Users\...\Jobs\Bougainville\`). The DLL:
1. Reads job files from that directory (`<path>Bougainville.JOB`, `<path>Bougainville.syd`, etc.).
2. Writes result files to that directory (`<path>REPORT2.OUT`, etc.).

The path must:
- End with a trailing path separator (the engine concatenates filenames directly).
- Be null-terminated (C-style).
- Use the platform's native path format (`\` on Windows, `/` on Linux/macOS).

### 7.2 The current VB6 startup

Source: `Modsim/Modsimvb/MODSIM.bas:46-63`

```vbnet
JobPath = CurDir & "\"
EnvString = Environ("MODSIM")
If EnvString <> "" Then
  JobPath = EnvString
Else
  EnvString = Environ("TEMP")
  If EnvString <> "" Then
    JobPath = EnvString & "\"
  ...
End If
```

The GUI resolves the job path from the `MODSIM` environment variable, falling back to `TEMP` or `CurDir`. A new GUI should use a similar resolution strategy (env var first, then a configured default, then a temp dir).

---

## 8. References

| What | Where |
|---|---|
| VB6 `Declare` statements (main) | `Modsim/Modsimvb/MODSIM.bas:9-12` |
| VB6 `Declare` statements (curve fit) | `Modsim/Modsimvb/ModsimCurveFitVB/MAINCurveFit.bas:113-118` |
| `MODELVARS` struct (engine-internal) | `Modsim/Modsimdl/UserModels/CModelVariables.h` |
| C↔Fortran user-model bridge | `Modsim/Modsimdl/UserModels/ConnectorToC.cpp` |
| `!DEC$ ATTRIBUTES` export directives | `Modsim/Modsimdl/ModsimCurveFitDll/ModsimCurveFit.f90:6-7` |
| `GENLMODS.FOR` DLL exports (`MESH_SIZE`, `PARTSZ`, `PASSSZ`, `FRPASS`) | `Modsim/Modsimdl/GENLMODS.FOR:294,313,380,439` |
| GNUmakefile (build / OS detection) | `Modsim/Modsimdl/GNUmakefile` |
| IFX port summary | [`docs/CHANGES.md`](../../docs/CHANGES.md) |
| Example jobs | `Modsim/Jobs/`, `Modsim/JobsRPK/` |
