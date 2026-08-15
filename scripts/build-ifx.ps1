<#
.SYNOPSIS
    Build the ModSIM Fortran engine DLLs with Intel IFX (oneAPI).

.DESCRIPTION
    Auto-detects Visual Studio 2022 and Intel oneAPI, sources their build
    environment (vcvarsall.bat + setvars.bat) into the current process, then
    invokes the GNUmakefile in Modsim/Modsimdl to build ModsimMain.dll,
    UserModels.dll and ModsimCurveFit.dll.

    This is the helper invoked by the top-level GNUmakefile's `build` target
    when COMPILER=ifx (the default). It exists because IFX needs the MSVC +
    oneAPI environment on PATH/LIB/INCLUDE before make can run, and that
    environment cannot be sourced from inside make itself.

.PARAMETER Build
    Build configuration: "release" (default) or "debug".

.PARAMETER Make
    Name or full path of the GNU make executable. Defaults to "make" (assumed
    on PATH).

.EXAMPLE
    .\scripts\build-ifx.ps1
    .\scripts\build-ifx.ps1 -Build debug
#>
[CmdletBinding()]
param(
    [ValidateSet("release", "debug")]
    [string]$Build = "release",

    [string]$Make = "make"
)

$ErrorActionPreference = "Stop"
$repoRoot = Split-Path -Parent $PSScriptRoot
$dlDir    = Join-Path $repoRoot "Modsim\Modsimdl"

# ── Locate Visual Studio 2022 vcvarsall.bat ──────────────────────────────────
$vsRoots = @(
    "C:\Program Files\Microsoft Visual Studio\2022",
    "C:\Program Files (x86)\Microsoft Visual Studio\2022",
    "N:\Program Files\Microsoft Visual Studio\2022",
    "N:\Program Files (x86)\Microsoft Visual Studio\2022",
    "D:\Program Files\Microsoft Visual Studio\2022"
)
$vsEditions = @("Community", "Professional", "Enterprise", "BuildTools")
$vcvars = $null
foreach ($root in $vsRoots) {
    foreach ($ed in $vsEditions) {
        $candidate = Join-Path $root "$ed\VC\Auxiliary\Build\vcvarsall.bat"
        if (Test-Path $candidate) { $vcvars = $candidate; break }
    }
    if ($vcvars) { break }
}
if (-not $vcvars) {
    throw "Visual Studio 2022 vcvarsall.bat not found. Install VS2022 with the 'Desktop development with C++' workload, or build with gfortran instead: make COMPILER=gfortran"
}

# ── Locate Intel oneAPI setvars.bat ───────────────────────────────────────────
$oneApiRoots = @(
    "C:\Program Files (x86)\Intel\oneAPI",
    "N:\Program Files (x86)\Intel\oneAPI",
    "C:\Program Files\Intel\oneAPI",
    "D:\Program Files (x86)\Intel\oneAPI"
)
$setvars = $null
foreach ($root in $oneApiRoots) {
    $candidate = Join-Path $root "setvars.bat"
    if (Test-Path $candidate) { $setvars = $candidate; break }
}
if (-not $setvars) {
    throw "Intel oneAPI setvars.bat not found. Install Intel oneAPI (compiler + MKL), or build with gfortran instead: make COMPILER=gfortran"
}

Write-Host "Sourcing build environment (Visual Studio 2022 + Intel oneAPI)..." -ForegroundColor DarkGray
Write-Host "  VS:     $vcvars" -ForegroundColor DarkGray
Write-Host "  oneAPI: $setvars" -ForegroundColor DarkGray

# ── Source both env scripts in a cmd subshell and capture the resulting env ──
# vcvarsall.bat and setvars.bat print a lot of output; we suppress theirs and
# capture only the final `set` dump to a temp file, then apply KEY=VALUE pairs
# to this process so the subsequent `make` inherits them.
$envFile = [IO.Path]::GetTempFileName()
try {
    # setvars.bat can be slow (10-30s); redirect its chatter away.
    $cmdLine = "`"$vcvars`" x64 > nul 2>&1 && `"$setvars`" intel64 > nul 2>&1 && set > `"$envFile`""
    cmd /c $cmdLine | Out-Null
    if ($LASTEXITCODE -ne 0) {
        throw "Failed to source build environment (vcvarsall/setvars exited with code $LASTEXITCODE)."
    }

    foreach ($line in Get-Content -LiteralPath $envFile) {
        $idx = $line.IndexOf('=')
        if ($idx -gt 0) {
            $key = $line.Substring(0, $idx)
            $val = $line.Substring($idx + 1)
            [Environment]::SetEnvironmentVariable($key, $val, "Process")
        }
    }
} finally {
    Remove-Item -LiteralPath $envFile -ErrorAction SilentlyContinue
}

# Sanity-check that ifx is now reachable.
if (-not (Get-Command ifx -ErrorAction SilentlyContinue)) {
    throw "ifx is not on PATH after sourcing oneAPI. Check your oneAPI installation."
}

# ── Run make in the Modsimdl directory ────────────────────────────────────────
Write-Host ""
Write-Host "Building ModsimMain.dll + UserModels.dll + ModsimCurveFit.dll (ifx / $Build)..." -ForegroundColor Cyan
Push-Location -LiteralPath $dlDir
try {
    & $Make COMPILER=ifx BUILD=$Build all
    $exit = $LASTEXITCODE
} finally {
    Pop-Location
}

if ($exit -ne 0) {
    Write-Host ""
    Write-Host "BUILD FAILED (exit $exit)" -ForegroundColor Red
    exit $exit
}

$outDir = Join-Path $dlDir "build\ifx\$Build"
Write-Host ""
Write-Host "BUILD SUCCEEDED" -ForegroundColor Green
Write-Host "  Output: $outDir\ModsimMain.dll" -ForegroundColor Green
Write-Host "  Run with: make run" -ForegroundColor Green
