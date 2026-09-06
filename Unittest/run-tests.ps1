# run-tests.ps1
# Runs the ZfD DUnitX console test suite and returns a structured summary.
# Called by Claude Code to run tests, read results, and fix failures.
#
# Usage:
#   .\run-tests.ps1                    # run all tests
#   .\run-tests.ps1 -Filter "intf.ZUGFeRD22Tests.UnitTests.TZUGFeRD22Tests.TestComment"
#   .\run-tests.ps1 -ShowXml           # also dump the raw XML

param(
    [string]$Filter = '',
    [switch]$ShowXml,
    [string]$ExePath = "$PSScriptRoot\ZfDUnitTest.exe",
    [string]$XmlPath = "$PSScriptRoot\dunitx-results.xml",
    [ValidateRange(1, 3600)]
    [int]$TimeoutSeconds = 120
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$exitCode = 4
$process = $null
$runXmlPath = Join-Path ([IO.Path]::GetTempPath()) ('zfd-results-' + [Guid]::NewGuid().ToString('N') + '.xml')

try {
    $resolvedExe = Get-Item -LiteralPath $ExePath
    if ($resolvedExe.PSIsContainer) {
        throw "The executable path is a directory: $ExePath"
    }
    $XmlPath = [IO.Path]::GetFullPath($XmlPath)
    if ($Filter.Contains('"') -or $Filter.Contains("`r") -or $Filter.Contains("`n")) {
        throw 'The filter must not contain quotes or line breaks.'
    }

    # DUnitX accepts qualified test/fixture names via --run, not --filter.
    $runnerArguments = @('--exitbehavior:Continue', ('--xmlfile:"' + $runXmlPath + '"'))
    if ($Filter -ne '') {
        $runnerArguments += '--run:"' + $Filter + '"'
    }

    # Close redirected stdin before waiting; no pre-existing placeholder file is required.
    Write-Host "Running: $($resolvedExe.FullName) $($runnerArguments -join ' ')"
    $process = New-Object System.Diagnostics.Process
    $process.StartInfo.FileName = $resolvedExe.FullName
    $process.StartInfo.WorkingDirectory = $resolvedExe.DirectoryName
    $process.StartInfo.Arguments = $runnerArguments -join ' '
    $process.StartInfo.UseShellExecute = $false
    $process.StartInfo.RedirectStandardInput = $true
    if (-not $process.Start()) {
        throw 'The test process did not start.'
    }
    $process.StandardInput.Close()
    if (-not $process.WaitForExit($TimeoutSeconds * 1000)) {
        $process.Kill()
        $process.WaitForExit()
        throw "The test process exceeded the timeout of $TimeoutSeconds seconds."
    }
    $exitCode = $process.ExitCode

    # A unique path prevents a failed launch from reusing a previous successful result.
    if (-not (Test-Path -LiteralPath $runXmlPath -PathType Leaf)) {
        throw "The test process produced no XML results (exit code $exitCode)."
    }
    [xml]$xml = Get-Content -LiteralPath $runXmlPath -Raw -Encoding UTF8
    $suite = $xml.DocumentElement
    if ($suite.LocalName -ne 'test-results') {
        throw 'The result file is not a DUnitX NUnit report.'
    }

    # NUnit 2 does not provide a "passed" attribute on the root element.
    $testCases = @($suite.SelectNodes('.//test-case'))
    $executedCases = @($testCases | Where-Object { $_.GetAttribute('executed') -eq 'True' })
    $passed = @($executedCases | Where-Object { $_.GetAttribute('result') -eq 'Success' }).Count
    $failedCases = @($executedCases | Where-Object { $_.GetAttribute('result') -ne 'Success' })
    $ignored = $testCases.Count - $executedCases.Count
    $reportedTotal = 0
    if (-not [int]::TryParse($suite.GetAttribute('total'), [ref]$reportedTotal) -or
        $reportedTotal -ne $executedCases.Count) {
        throw 'The reported total does not match the executed test cases.'
    }

    if ($exitCode -eq 0) {
        if ($executedCases.Count -eq 0) {
            $exitCode = 3
        } elseif ($failedCases.Count -gt 0 -or $suite.GetAttribute('failures') -ne '0' -or
            $suite.GetAttribute('errors') -ne '0') {
            $exitCode = 1
        }
    }

    Copy-Item -LiteralPath $runXmlPath -Destination $XmlPath
    Write-Host "Executed: $($executedCases.Count); passed: $passed; failed/errors: $($failedCases.Count); ignored: $ignored"
    Write-Host 'Memory checks: explicit heap assertions only; the default DUnitX leak counter is inactive.'

    # Show failing tests
    foreach ($testCase in $failedCases) {
        Write-Host "FAIL: $($testCase.GetAttribute('name'))"
        $message = $testCase.SelectSingleNode('failure/message')
        if ($null -ne $message) {
            Write-Host $message.InnerText.Trim()
        }
    }

    if ($ShowXml) {
        Get-Content -LiteralPath $XmlPath
    }
    Write-Host "XML results: $XmlPath"
} catch {
    [Console]::Error.WriteLine($_.Exception.Message)
    $exitCode = 4
} finally {
    if ($null -ne $process) {
        $process.Dispose()
    }
    if (Test-Path -LiteralPath $runXmlPath -PathType Leaf) {
        Remove-Item -LiteralPath $runXmlPath
    }
}

exit $exitCode
