<#
.SYNOPSIS
    Launch the ModSIM GUI (PySide6).

.DESCRIPTION
    Points the MODSIM environment variable at the built Fortran engine
    directory, ensures the Python GUI package is installed, then launches
    `python -m modsim`.

    This is the helper invoked by the top-level GNUmakefile's `run` target.

.PARAMETER Compiler
    Compiler/build tree to point MODSIM at. Defaults to "ifx" (the project
    primary). Use "gfortran" if you built with `make COMPILER=gfortran`.

.PARAMETER Build
    Build configuration: "release" (default) or "debug".

.EXAMPLE
    .\scripts\run.ps1
    .\scripts\run.ps1 -Compiler gfortran -Build debug
#>
[CmdletBinding()]
param(
    [string]$Compiler = "ifx",
    [ValidateSet("release", "debug")]
    [string]$Build = "release"
)

$ErrorActionPreference = "Stop"
$repoRoot  = Split-Path -Parent $PSScriptRoot
$engineDir = Join-Path $repoRoot "Modsim\Modsimdl\build\$Compiler\$Build"
$pyDir     = Join-Path $repoRoot "Modsim\Modsimpy"

# ── Verify the engine DLL exists ─────────────────────────────────────────────
$engineDll = Join-Path $engineDir "ModsimMain.dll"
if (-not (Test-Path $engineDll)) {
    Write-Host "Engine DLL not found:" -ForegroundColor Red
    Write-Host "  $engineDll" -ForegroundColor Red
    Write-Host ""
    Write-Host "Build it first with:  make" -ForegroundColor Yellow
    Write-Host "  (or: make COMPILER=gfortran)" -ForegroundColor Yellow
    exit 1
}

# MODSIM tells the ctypes engine bridge where to find ModsimMain.dll.
$env:MODSIM = $engineDir
Write-Host "Engine: $engineDll" -ForegroundColor DarkGray

# ── Ensure the Python GUI package is installed (one-time, idempotent) ────────
$needInstall = $false
python -c "import modsim" 2>$null | Out-Null
if ($LASTEXITCODE -ne 0) { $needInstall = $true }

if ($needInstall) {
    Write-Host "Installing Python GUI dependencies (first run)..." -ForegroundColor Cyan
    pip install -e $pyDir
    if ($LASTEXITCODE -ne 0) {
        Write-Host "Failed to install Python dependencies." -ForegroundColor Red
        exit $LASTEXITCODE
    }
}

# ── Launch the GUI ────────────────────────────────────────────────────────────
Write-Host "Launching ModSIM GUI..." -ForegroundColor Cyan
Push-Location -LiteralPath $pyDir
try {
    python -m modsim
    $exit = $LASTEXITCODE
} finally {
    Pop-Location
}
exit $exit
