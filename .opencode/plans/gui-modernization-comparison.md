# ModSIM GUI Modernization: Framework Comparison & Considerations

> **Status:** Exploration / decision-support document
> **Date:** August 2026
> **Context:** The Fortran engine has been ported to Intel IFX 2025.3.2 producing a 64-bit DLL (see [`CHANGES.md`](../../docs/CHANGES.md)). The VB6 GUI is 32-bit-only and cannot call the new DLL. This document explores options for replacing the GUI with a modern, cross-platform, open-source-friendly stack.
> **Note:** This file was written to `.opencode/plans/` due to plan-mode write restrictions. Move to `docs/` when plan mode is exited.

---

## 1. Context & Motivation

### 1.1 The bitness wall

```
   VB6 GUI (32-bit)          Fortran DLL (x64)
   ────────────────          ────────────────
   can ONLY load             is NOW 64-bit
   32-bit DLLs               (IFX oneAPI)
        │                          │
        └──── cannot link ─────────┘
              ❌ bitness mismatch
```

VB6's `Declare` statements use `ByRef As Long` (32-bit pointers) and BSTR→ANSI string marshalling. There is no 64-bit VB6. The GUI literally cannot call the new DLL — this is a hard wall, not a soft one.

### 1.2 Goals of the modernization

1. **Solve the immediate 64-bit incompatibility** — unblock runtime testing and use of the IFX-ported engine.
2. **Improve maintainability** — move away from VB6 (no tooling, declining expertise, IDE-only builds).
3. **Encourage open-source contributions** — choose a stack with a large, active contributor pool.
4. **Future-proofness** — a stack that will be alive and supported for the next decade+.
5. **Cross-platform** — enable running on **Windows, Linux, and macOS** (explicit requirement).

---

## 2. The Key Discovery: the GUI↔Engine Interface is Tiny & File-Based

This is the single most important finding, because it reframes the entire rewrite problem.

### 2.1 The contract

The entire GUI↔engine API surface is **4 functions** (5 if you count the separate CurveFit app), and every single one just hands the DLL a **directory path string**. The GUI writes a constellation of small text files into a folder, calls `SIMOP(path, cumOut)`, and reads result files back.

```
┌─────────────────────────────────────────────────────────────────────┐
│                        THE REAL CONTRACT                            │
├─────────────────────────────────────────────────────────────────────┤
│   VB6 GUI                          Fortran DLL (x64)                │
│   ───────                          ───────────────                  │
│                                                                     │
│   ┌──────────┐    4 functions,     ┌──────────────┐                 │
│   │          │    all just pass    │              │                 │
│   │  writes  │    a PATH STRING    │  reads job   │                 │
│   │ job files├───────────────────▶│  files from  │                 │
│   │ to disk  │    INORDCALC(path)  │  that path   │                 │
│   │          │    SIMOP(path,&out) │  writes      │                 │
│   │  reads   │    LJUBAMD(path,job)│  results.out │                 │
│   │ results  │    BetaAMD(path,job)│              │                 │
│   └──────────┘                    └──────────────┘                 │
│        │                                                            │
│        │  .JOB .syd .siz .gcd .mat .scd .sid .cur .mop .TEA .TRN    │
│        └──────── text files in a job directory ──────────┘         │
└─────────────────────────────────────────────────────────────────────┘
```

### 2.2 Implication

The `MODELVARS` struct in `CModelVariables.h` is **internal to the Fortran+C user-model layer** — the GUI never sees it. The GUI and engine are almost fully decoupled already. A GUI rewrite does *not* require touching the engine, adapting to a new API, or marshalling complex structs. Any new GUI just needs to: produce the right job files → call 4 path-string functions → read result files.

See [`gui-engine-interface.md`](./gui-engine-interface.md) for the full interface documentation.

---

## 3. Engine Cross-Platform Readiness

A cross-platform GUI is only useful if the engine is cross-platform too. Investigation of the Fortran source reveals the engine is **already ~90% cross-platform** — the IFX port accidentally achieved this.

### 3.1 Scorecard

```
  Cross-platform readiness:
  ─────────────────────────
  Numerical deps (MINPACK)        ✅ pure Fortran, runs anywhere
  Numerical deps (MKL)            ✅ Intel ships Linux + macOS MKL
                                   (or swap for OpenBLAS/LAPACK)
  MSFLIB (Win32 helpers)          ✅ already stubbed for gfortran
  PORTLIB                         ✅ already stubbed (empty module)
  Direct Win32 API calls          ✅ none exist (verified by grep)
  gfortran build path             ✅ already in GNUmakefile (default!)
  OS detection in makefile        ✅ already there (ifeq Windows_NT)
  IFX-on-Linux section            ✅ already scaffolded (untested)

  Remaining work for .so/.dylib:
  ──────────────────────────────
  !DEC$ ATTRIBUTES directives     ⚠️  need gfortran equivalents or BIND(C)
  SYS_LIBS conditionalization     ⚠️  trivial — already done for IFX-Linux
  STAT function in CALC/CALC1     ⚠️  needs portable replacement
  Actually test on Linux/macOS    ⚠️  the real unknown
```

### 3.2 What the IFX port did

By removing IMSL (Windows-x86-only) and replacing it with:
- **MINPACK** (public-domain, pure Fortran) for Levenberg-Marquardt
- **MKL** (available on Linux/macOS) for eigenvalues

…and by already stubbing `MSFLIB` (only 2 functions: `COMMITQQ` → portable `FLUSH`, `MESSAGEBOXQQ` → stderr) and `PORTLIB` (empty module — no symbols actually called), the port removed every Windows-x86-specific dependency.

### 3.3 No direct Win32 API calls

A grep across all `.FOR`/`.for`/`.F90`/`.f90` source files for direct Win32 API calls (`CreateFile`, `GetTickCount`, `Sleep`, `MessageBox`, `ShellExecute`, etc.) returned **zero matches**. The `SYS_LIBS` (kernel32, user32, gdi32, …) are linked defensively but appear unused by the Fortran code itself.

---

## 4. Current VB6 GUI Architecture (scope of rewrite)

```
┌─ Modsim.vbp ──────────────────────────────────────────────────────┐
│  ~99,400 lines VB6 │ 103 forms │ 16 modules │ 0 class modules    │
│  Startup: Sub Main → MDI parent (MdlMODSMDI)                      │
└───────────────────────────────────────────────────────────────────┘
        │
        ├─── MDI SHELL ──────────── menu, toolbar, status bar
        │
        ├─── FLOWSHEET CANVAS ◆── FLSheet.frm (2,483 lines)
        │        │                 Flwsheet.bas (1,739 lines)
        │        │                 Plant1.bas (1,213 lines)
        │        │                 ── the crown jewel: 2D diagram editor
        │        │                    drag units, draw streams/connectors,
        │        │                    flyout labels, unit/stream numbering
        │        └── Vetgraph/ (separate VB6 GDI plotting package, 11 files)
        │
        ├─── ~67 EQUIPMENT DIALOGS ── CRSHWhiten, HPGR, GYRA, CSCN,
        │        │                     CYCL, DSC1/2, GMIL, FMIL, ELUT,
        │        │                     BLBX, CONE, CONV, FLTN, etc.
        │        └── each = parameter form → writes to job files
        │            (repetitive, mechanical, largely data-entry)
        │
        ├─── JOB FILE I/O ──────── read/write .JOB/.syd/.siz/.gcd/...
        │
        ├─── SIMULATION ORCH. ──── INORDCALC → SIMOP → read results
        │
        ├─── RESULTS / GRAPHING ── OPGRAPH.bas, LIBGRAPH.bas,
        │        │                  modelgrp.bas (Vetgraph/GDI plots)
        │        └── size distributions, liberation spectra, stream plots
        │
        ├─── UNIT CONVERSION ───── MODSIM.bas SetConversionFactors
        │                            (huge table: m/cm/ft, kgps/stonphr, ...)
        │
        └─── SEPARATE APP ──────── ModsimCurveFitVB/ (Levenberg-Marquardt
                                     curve fitting, calls ModsimCurveFit.dll)
```

---

## 5. Six Frameworks Rated

### 5.1 Comparison table

```
┌──────────────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┐
│                  │ Tauri   │Electron │ Python  │ Avalonia│  MAUI   │ C++ Qt  │
│                  │+React   │+React   │+PySide6 │  (C#)   │  (C#)   │  (Qt6)  │
├──────────────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┤
│ CROSS-PLATFORM   │         │         │         │         │         │         │
│  Windows         │  ✅     │  ✅     │  ✅     │  ✅     │  ✅     │  ✅     │
│  Linux           │  ✅     │  ✅     │  ✅     │  ✅     │  ❌     │  ✅     │
│  macOS           │  ✅     │  ✅     │  ✅     │  ✅     │  ✅     │  ✅     │
│  Rating          │ ★★★★★  │ ★★★★★  │ ★★★★★  │ ★★★★   │  ★★     │ ★★★★★  │
├──────────────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┤
│ OSS APPEAL       │         │         │         │         │         │         │
│  Contributor pool│ 20M JS  │ 20M JS  │ 15M Py  │ 7M C#   │ 7M C#   │ 9M C++  │
│  Sci/eng appeal  │  low    │  low    │  HIGH   │  med    │  med    │  HIGH   │
│  Rating          │ ★★★★   │ ★★★    │ ★★★★★  │ ★★★    │ ★★★    │ ★★★    │
├──────────────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┤
│ FORTRAN FFI      │         │         │         │         │         │         │
│  Mechanism       │Rust FFI │node-ffi │ ctypes  │P/Invoke │P/Invoke │extern C │
│  Cleanliness     │  clean  │ clunky  │ trivial │  clean  │  clean  │ NATIVE  │
│  Marshalling     │  yes    │  yes    │  yes    │  yes    │  yes    │  NONE   │
│  Rating          │ ★★★★   │ ★★     │ ★★★★★  │ ★★★★   │ ★★★★   │ ★★★★★  │
├──────────────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┤
│ CANVAS FIT       │         │         │         │         │         │         │
│  Flowsheet editor│ReactFlow│ReactFlow│QGraphics│ custom  │ custom  │QGraphics│
│  Maturity        │  good   │  good   │ GOLD    │  med    │  low    │ GOLD    │
│  MDI support     │  ❌     │  ❌     │  ✅     │  ❌     │  ❌     │  ✅     │
│  Rating          │ ★★★★   │ ★★★★   │ ★★★★★  │ ★★★    │ ★★     │ ★★★★★  │
├──────────────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┤
│ EFFORT           │ ★★★★   │ ★★★★   │ ★★★    │ ★★★    │ ★★★    │ ★★★    │
├──────────────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┤
│ MATURITY         │ ★★★★   │ ★★★★★  │ ★★★★★  │ ★★★★   │ ★★★    │ ★★★★★  │
├──────────────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┤
│ DISTRIBUTION     │         │         │         │         │         │         │
│  Binary size     │ ~10MB   │ ~150MB  │ varies  │ ~30MB   │ ~30MB   │ ~50MB   │
│  Packaging       │ trivial │ trivial │ harder  │  good   │  good   │  good   │
│  Rating          │ ★★★★★  │ ★★★    │ ★★     │ ★★★★   │ ★★★★   │ ★★★★   │
├──────────────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┤
│ LICENSING        │         │         │         │         │         │         │
│  GUI license     │ MIT     │ MIT     │ LGPL    │ MIT     │ MIT     │ LGPL    │
│  Copyleft risk   │  none   │  none   │  low    │  none   │  none   │  low    │
│  Rating          │ ★★★★★  │ ★★★★★  │ ★★★★   │ ★★★★★  │ ★★★★★  │ ★★★★   │
├──────────────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┤
│ ARCH MAP to VB6  │         │         │         │         │         │         │
│  (how closely    │  low    │  low    │  HIGH   │  med    │  med    │  HIGH   │
│   preserves      │ redesign│redesign │ 1:1     │ partial │ partial │  1:1    │
│   current arch)  │ to web  │ to web  │ mapping │         │         │ mapping │
│  Rating          │ ★★     │ ★★     │ ★★★★★  │ ★★★    │ ★★★    │ ★★★★★  │
└──────────────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┘
```

### 5.2 .NET cross-platform — the honest answer

C# *can* be cross-platform, but **not with the UI frameworks you'd naturally reach for**:

```
  .NET runtime (the thing that executes C#)     ✅ cross-platform
       runs on Windows, Linux, macOS

  C# language                                  ✅ cross-platform

  WinForms (what VB6 code maps to naturally)   ❌ Windows-only
  WPF (modern Windows desktop UI)             ❌ Windows-only

  ─── Cross-platform .NET GUI options ───
  Avalonia       ✅ Win/Linux/macOS — WPF-like, mature, popular
  Uno Platform   ✅ Win/Linux/macOS/iOS/Android/Web
  MAUI           ⚠️  Win/macOS — no official Linux desktop
  Blazor         ✅ web-based (runs in browser or via Electron/Photino)
```

There is no "VB6.NET" — VB.NET is a different language that shares some syntax but isn't compatible. There's no automatic VB6→VB.NET conversion; it's a rewrite either way.

### 5.3 MAUI — DISQUALIFIED

```
  .NET MAUI official supported platforms (Microsoft, April 2026):
  ─────────────────────────────────────────────────────────────
  ✅ Android 5.0+
  ✅ iOS 11+
  ✅ macOS 11+ (via Mac Catalyst)
  ✅ Windows 10+
  ❌ Linux desktop — NOT supported, NOT planned

  Community options:
  ┌──────────────────────────────────────────────────────┐
  │ open-maui/maui-linux    13 stars, 2 contributors     │
  │   → "production-ready" (self-claimed), preview.3     │
  │   → partial Wayland, no hardware video accel         │
  │                                                      │
  │ Avalonia MAUI Backend   113 stars, 7 forks           │
  │   → PREVIEW, targets .NET 11 (Nov 2026)              │
  │   → X11/XWayland only, no Wayland                    │
  └──────────────────────────────────────────────────────┘
```

Microsoft has made no official statements indicating plans to add Linux desktop support to .NET MAUI in .NET 8, 9, or 10. Betting a GUI rewrite on a 13-star community fork is the opposite of future-proof.

**MAUI is disqualified** — not because it's bad, but because it fundamentally doesn't target the required platforms.

### 5.4 Electron — strictly worse than Tauri

Electron and Tauri target the same niche (web-tech desktop apps), but Tauri produces ~10MB binaries vs Electron's ~150MB, has a cleaner Rust backend vs Node's `ffi-napi` (clunky, fragile, ABI-sensitive), and is more modern. No reason to choose Electron over Tauri.

### 5.5 C++ Qt 6 — the architecture-mapping dark horse

Qt maps to the existing VB6 architecture almost 1:1:

```
  VB6 concept                    Qt equivalent (C++ or Python)
  ────────────                   ────────────────────────────
  MDI parent form                QMdiArea + QMdiSubWindow     ✅ direct
  MDI child form (FLSheet)       QMdiSubWindow                ✅ direct
  Equipment dialog (.FRM)        QDialog                       ✅ direct
  GDI canvas (Vetgraph)          QGraphicsScene/QGraphicsView  ✅ direct
  StatusBar / Toolbar / Menu     QStatusBar/QToolBar/QMenuBar  ✅ direct
  CommonDialog (file open/save)  QFileDialog                   ✅ direct
  MSFlexGrid (data tables)       QTableWidget/QTableView        ✅ direct
  Declare Function (DLL call)    extern "C" / QLibrary         ✅ direct
```

No other framework gives you MDI natively. And the Fortran interop is the cleanest of any option — **zero marshalling, same ABI**:

```
  Every other stack:                    C++ Qt:
  ─────────────────                     ────────
  GUI language ──FFI──▶ Fortran         C++ ──extern "C"──▶ Fortran
  (marshalling layer needed)            (SAME ABI, no marshalling)

  C++ Qt:  extern "C" {
               void simop_(char*, long*);
           }

           QLibrary lib("libmodsim.so");
           auto fn = lib.resolve("simop_");

           → zero overhead, zero marshalling
```

### 5.6 Python + PySide6 — the scientific-community fit

Same Qt canvas (`QGraphicsScene`), same MDI (`QMdiArea`), same cross-platform reach, same LGPL licensing — but with `ctypes.CDLL` making Fortran FFI trivial (literally 3 lines) and access to the entire scientific Python ecosystem (numpy, scipy, matplotlib, pandas).

---

## 6. The Flowsheet Canvas Question

This is the crown jewel and where the real engineering risk lives.

### 6.1 The current canvas

`FLSheet.frm` (2,483 lines) + `Flwsheet.bas` (1,739 lines) + `Plant1.bas` (1,213 lines) implement a custom 2D diagram editor: drag units onto a canvas, draw streams (connectors) between them, label, number, persist layout. Rendering is raw GDI via the separate Vetgraph package (`Vetgraph/`, 11 files).

### 6.2 Replacement options by stack

```
  Stack              Canvas approach
  ────              ───────────────
  .NET (WPF)        custom Canvas + shapes, or host a diagramming lib
  .NET (WinForms)   GDI+ (closest 1:1 to current GDI code)
  Web (Tauri/Electron)  React Flow / mxGraph / JointJS / GoJS  ← node-edge libs
  Qt (C++ or Python)    QGraphicsScene/QGraphicsView  ← mature 2D scene graph
```

A flowsheet *is* a directed graph of units connected by streams. Node-edge libraries (React Flow et al.) map to that mental model directly. `QGraphicsScene` is more powerful and lower-level — closer to the current GDI approach, with full control over rendering.

### 6.3 MDI preservation

Only Qt (C++ or Python) offers MDI natively. Tauri, Electron, MAUI, and Avalonia would require redesigning the window architecture into tabs or separate windows.

---

## 7. The 67-Dialog Question

The ~67 equipment-model forms are mostly repetitive parameter-entry dialogs. Two philosophies:

```
  1-FORM-PER-MODEL (current)          DATA-DRIVEN (schema → auto-form)
  ────────────────────────           ────────────────────────────────
  port each .FRM by hand            define a schema per model type
  faithful, tedious, 67× work       (fields, types, units, bounds)
                                     → generate forms from schema
                                     less faithful, more maintainable
                                     new model = new schema, not new form
```

**Open question:** How many of the 67 models are actually live/used? If half are legacy/rarely-used, the real porting scope shrinks dramatically. Worth auditing usage before committing to an approach.

---

## 8. Contributor Pool Analysis

```
  Who would contribute?              What stack do they know?
  ────────────────────────           ───────────────────────
  Mineral processing engineers       Python (maybe MATLAB)
  Researchers / academics            Python, maybe R
  Scientific software devs           Python, C++, maybe Rust
  General OSS contributors           JavaScript/TypeScript (by volume)
  Students                           JavaScript, Python

  ┌──────────────────────────────────────────────────────┐
  │  JavaScript/TypeScript:  ~20M developers (largest)   │
  │  Python:                 ~15M developers             │
  │  C++:                    ~9M developers              │
  │  C#:                     ~7M developers              │
  │  Rust:                   ~3M developers (fastest     │
  │                                    growing)          │
  └──────────────────────────────────────────────────────┘
```

The key insight: the heavy computation lives in Fortran. The GUI is "just" orchestration (write job files, call 4 functions, read results, display graphs). For that workload, Python's performance disadvantage is irrelevant — it's not doing the math.

---

## 9. Licensing Considerations

### 9.1 Qt LGPL v3 obligations

When using Qt under LGPL v3, you must:

1. **Provide Qt source code** to users (or a written offer under your control — a link to qt.io is *not* sufficient).
2. **Dynamic linking recommended** — static linking is permitted but requires providing object files so users can relink.
3. **User must be able to relink** — replace Qt with a modified version and still run the app.
4. **Anti-tivoization** — applies only to *consumer products*; B2B/scientific software is exempt.
5. **License notice** — provide LGPL v3 text and prominent Qt usage notice.
6. **Modifications to Qt itself must be shared** back under LGPL v3.

The KDE Free Qt Foundation agreement (surviving since 1998 through Trolltech → Nokia → Digia → Qt Company transitions) guarantees that if Qt Company ever discontinues open-source Qt, the Foundation can release Qt under a BSD-style license within 12 months. Core platforms (X11/Linux desktop, Android) cannot be dropped.

### 9.2 ⚠️ Qt Charts is GPL v3, not LGPL

```
  Qt modules under LGPL v3 (fine for OSS):          Qt modules under GPL v3 (⚠️):
  ────────────────────────────────────              ──────────────────────────────
  Qt Core, GUI, Widgets, Network, SQL              Qt Charts          ← graphing!
  Qt Multimedia, Qt PrintSupport                    Qt Data Visualization
  Qt OpenGL, Qt SVG                                Qt Graphs
  Qt Concurrent, Qt XML                            Qt Quick 3D
                                                    Qt Lottie Animation
```

Using Qt Charts forces the **entire app** to GPL v3 (full copyleft to all your code).

**Workaround:** Use **Qwt** (LGPL, purpose-built for scientific/technical plotting) or QCustomPlot or draw with `QPainter` directly. Qwt is a perfect fit for size distributions, liberation spectra, and stream plots.

### 9.3 Other frameworks

- **Tauri / Electron / .NET / MAUI**: MIT-licensed, no copyleft risk.

---

## 10. Finalists & Recommendation Framework

```
  ┌─────────────────────────────────────────────────────────────┐
  │                                                             │
  │   If target contributors are SCIENTISTS/ENGINEERS:          │
  │   → Python + PySide6 (Qt)                                   │
  │     (they already know Python, Qt canvas is gold-standard,  │
  │      ctypes FFI is 3 lines, scipy/matplotlib ecosystem)     │
  │                                                             │
  │   If target contributors are SOFTWARE ENGINEERS:            │
  │   → Tauri + React/TypeScript                               │
  │     (largest pool, web tech, React Flow for canvas,        │
  │      Rust FFI is clean, smallest binaries)                 │
  │                                                             │
  │   If you want the CLEANEST Fortran interop + best perf:     │
  │   → C++ Qt 6                                                │
  │     (direct native ABI, QGraphicsScene, QMdiArea,           │
  │      but C++ complexity tax for no real benefit here)       │
  │                                                             │
  │   MAUI: ❌ disqualified (no Linux)                          │
  │   Electron: ❌ strictly worse than Tauri                    │
  │   Avalonia: ⚠️ viable but smaller ecosystem than Qt/React  │
  │                                                             │
  └─────────────────────────────────────────────────────────────┘
```

### 10.1 The two finalists, compared

```
                        C++ Qt 6                    Python + PySide6
                        ─────────                   ────────────────
  Same Qt canvas        ✅ QGraphicsScene           ✅ QGraphicsScene
  Same Qt MDI           ✅ QMdiArea                 ✅ QMdiArea
  Same cross-platform   ✅ Win/Linux/macOS          ✅ Win/Linux/macOS
  Same licensing        ✅ LGPL (+Qwt for plots)    ✅ LGPL (+Qwt/matplotlib)
  ────────────────────────────────────────────────────────────────────────
  Fortran interop       ✅✅✅ direct extern "C"    ✅ ctypes (trivial but
                          zero marshalling              has marshalling layer)
                          same ABI                      dynamic, no compile

  Contributor pool      ⚠️ C++ is harder             ✅ Python is the lingua
                          fewer casual devs             franca of scientific
                          but Qt is known in            computing — largest
                          scientific software           pool of domain experts

  Maintainability       ⚠️ C++ is powerful but        ✅ Python is readable,
                          error-prone, slower            rapid to iterate,
                          to iterate                     easy to review

  Performance           ✅ native, fast              ⚠️ Python GIL, but the
                                                         heavy lifting is in
                                                         Fortran anyway

  Distribution          ✅ deployqt tools            ⚠️ PyInstaller/conda,
                          single package                larger, more fragile

  Scientific ecosystem  med (Qwt, VTK)               ✅✅ numpy, scipy,
                                                         matplotlib, pandas
```

---

## 11. Open Questions

1. **Target contributor profile** — Who do you envision contributing? Mineral processing engineers/researchers (→ Python) or general software engineers (→ Tauri/React)? This shapes the stack choice more than any technical factor.

2. **How many of the 67 equipment models are actually live?** If it's 15-20, the rewrite scope is very different from 67. Affects whether data-driven forms (schema → auto-generate) are worth building vs. porting forms individually.

3. **Job-file format — preserve or modernize?** Preserving it means the new GUI works with the existing engine and existing jobs immediately. Modernizing to JSON/SQLite is cleaner but adds an engine-side adapter. Preserving is recommended — it's the decoupling that makes the rewrite safe.

4. **Distribution model** — Tauri gives a single ~10MB binary. Python requires packaging (PyInstaller, conda, or pip install). For an academic/open-source tool, how important is "download and double-click"?

5. **Stopgap appetite** — A 64-bit surrogate (small exe that loads the Fortran DLL, exposes it over localhost HTTP or named pipes) could let the existing VB6 GUI call the new DLL within days — unblocking runtime testing of the IFX port (the #1 "Remaining Work" item in CHANGES.md) while the real GUI rewrite is planned.

6. **Engine portability spike** — The highest-value investigation right now. If `gfortran` can produce a `.so` on Linux (even a partial build), it validates the entire cross-platform premise. The GNUmakefile already has the scaffolding.

---

## Appendix A: Quick Reference — Engine Cross-Platform Status

| Aspect | Status | Notes |
|---|---|---|
| IMSL (Windows-x86-only) | ✅ removed | Replaced by MINPACK + MKL |
| MINPACK | ✅ portable | Pure Fortran, public domain |
| MKL | ✅ cross-platform | Intel ships Linux + macOS MKL |
| `MSFLIB` | ✅ stubbed | Only `COMMITQQ` (→ `FLUSH`) + `MESSAGEBOXQQ` (→ stderr) |
| `PORTLIB` | ✅ stubbed | Empty module — no symbols called |
| Direct Win32 API calls | ✅ none | Verified by grep |
| `SYS_LIBS` in makefile | ⚠️ conditional | Already empty for IFX-Linux path |
| `!DEC$ ATTRIBUTES` directives | ⚠️ needs work | Need gfortran `BIND(C)` equivalents |
| `STAT` function in `CALC`/`CALC1` | ⚠️ needs check | May need portable replacement |
| gfortran build path | ✅ exists | Default in GNUmakefile |
| OS detection | ✅ exists | `ifeq ($(OS),Windows_NT)` |
| IFX-on-Linux scaffolding | ✅ exists | Commented "future-proofing; untested" |
| Actually tested on Linux/macOS | ❌ no | The real unknown |
