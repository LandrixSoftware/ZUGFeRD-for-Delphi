param([string[]]$Paths = @())

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

# TList<T>.Count is NativeInt from Delphi 13 on. Comparing it against a bare integer
# literal leaves Assert.AreEqual<T> without a common type argument, so the unit fails
# with E2532 - and the compiler then reports unrelated follow-up errors such as E2250
# on WillRaise thousands of lines further down, which point away from the real cause.
# On Delphi 11, where Count is still Integer, the same source compiles, so contributions
# from older environments keep reintroducing this.
#
# The rule is therefore unconditional: every Count in an Assert.AreEqual is written as
# Integer(<expression>.Count). Whether a given Count is NativeInt or Integer cannot be
# decided from the source text - TStringList.Count, for instance, is still Integer - and
# casting one that is already Integer costs nothing. An unconditional rule keeps this
# check free of exception lists and of false positives.
#
# Only .Count on a single line is examined, which is the shape that occurs here.
function Get-TrackedDelphiSource {
    $repositoryRoot = Split-Path -Parent $PSScriptRoot
    Push-Location -LiteralPath $repositoryRoot
    try {
        $tracked = & git ls-files '*.pas' '*.dpr' '*.inc'
        if ($LASTEXITCODE -ne 0) {
            throw 'git ls-files failed; run the check inside the repository or pass -Paths.'
        }
        return @($tracked | ForEach-Object { Join-Path $repositoryRoot $_ })
    } finally {
        Pop-Location
    }
}

# Integer(...) and other casts do not match: a cast puts "(" where the pattern needs the
# member access to continue, so only the uncast forms are reported.
$literalFirst = 'Assert\.AreEqual\(\s*-?\d+\s*,\s*[\w.\[\]]+\.Count\b'
$literalSecond = 'Assert\.AreEqual\(\s*[\w.\[\]]+\.Count\s*,\s*-?\d+'

try {
    if ($Paths.Count -eq 0) {
        $Paths = Get-TrackedDelphiSource
    }
    if ($Paths.Count -eq 0) { throw 'No source files selected.' }

    $findings = @()
    foreach ($path in $Paths) {
        if (-not (Test-Path -LiteralPath $path -PathType Leaf)) {
            throw "Source file not found: $path"
        }
        $lineNumber = 0
        foreach ($line in [IO.File]::ReadAllLines($path)) {
            $lineNumber++
            if ($line -match $literalFirst -or $line -match $literalSecond) {
                $findings += "{0}({1}): {2}" -f $path, $lineNumber, $line.Trim()
            }
        }
    }

    if ($findings.Count -gt 0) {
        foreach ($finding in $findings) {
            [Console]::Error.WriteLine($finding)
        }
        throw "$($findings.Count) count assertion(s) compare a NativeInt Count against an integer literal; wrap the Count in Integer(...)."
    }

    Write-Output "Count assertions checked: $($Paths.Count) files."
    exit 0
} catch {
    [Console]::Error.WriteLine($_.Exception.Message)
    exit 1
}
