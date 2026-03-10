# run-tests.ps1
# Runs the ZfD DUnitX console test suite and returns a structured summary.
# Called by Claude Code to run tests, read results, and fix failures.
#
# Usage:
#   .\run-tests.ps1                    # run all tests
#   .\run-tests.ps1 -Filter "TestName" # run specific test(s)
#   .\run-tests.ps1 -ShowXml           # also dump the raw XML

param(
    [string]$Filter = '',
    [switch]$ShowXml,
    [string]$ExePath = "$PSScriptRoot\Win64\Debug\ZfDUnitTest.exe"
)

$xmlPath = "$PSScriptRoot\dunitx-results.xml"
$exitCode = 0

# Build argument list
$args = @()
if ($Filter -ne '') {
    $args += "--filter=$Filter"
}

# Run tests (non-interactive: stdin is not a console)
Write-Host "Running: $ExePath $args" -ForegroundColor Cyan
$proc = Start-Process -FilePath $ExePath -ArgumentList $args `
    -Wait -PassThru -NoNewWindow `
    -RedirectStandardInput "$PSScriptRoot\nul_stdin.tmp"

# Create empty stdin redirect file if it doesn't exist
if (-not (Test-Path "$PSScriptRoot\nul_stdin.tmp")) {
    "" | Out-File "$PSScriptRoot\nul_stdin.tmp" -Encoding ASCII
    $proc = Start-Process -FilePath $ExePath -ArgumentList $args `
        -Wait -PassThru -NoNewWindow `
        -RedirectStandardInput "$PSScriptRoot\nul_stdin.tmp"
}

$exitCode = $proc.ExitCode

# Parse NUnit XML results
if (Test-Path $xmlPath) {
    [xml]$xml = Get-Content $xmlPath -Encoding UTF8

    $suite = $xml.'test-results'
    if ($null -eq $suite) {
        $suite = $xml.SelectSingleNode('//*[@total]')
    }

    $total   = [int]($suite.total   ?? ($xml.SelectSingleNode('//*[@total]').'total'))
    $passed  = [int]($suite.passed  ?? 0)
    $failed  = [int]($suite.failures ?? ($suite.failed ?? 0))
    $errors  = [int]($suite.errors  ?? 0)
    $ignored = [int]($suite.ignored ?? ($suite.skipped ?? 0))

    Write-Host ""
    Write-Host "=== Test Results ===" -ForegroundColor White
    Write-Host "Total:   $total" -ForegroundColor White
    Write-Host "Passed:  $passed" -ForegroundColor Green
    if ($failed -gt 0)  { Write-Host "Failed:  $failed"  -ForegroundColor Red   }
    if ($errors -gt 0)  { Write-Host "Errors:  $errors"  -ForegroundColor Red   }
    if ($ignored -gt 0) { Write-Host "Ignored: $ignored" -ForegroundColor Yellow }

    # Show failing tests
    $failedTests = $xml.SelectNodes('//*[local-name()="test-case" and (@result="Failure" or @result="Error" or @result="Failed")]')
    if ($failedTests.Count -gt 0) {
        Write-Host ""
        Write-Host "=== Failing Tests ===" -ForegroundColor Red
        foreach ($t in $failedTests) {
            $name = $t.name ?? $t.fullname
            $msg  = $t.SelectSingleNode('*[local-name()="failure"]/*[local-name()="message"]')
            Write-Host "  FAIL: $name" -ForegroundColor Red
            if ($null -ne $msg) {
                Write-Host "        $($msg.InnerText.Trim())" -ForegroundColor Yellow
            }
        }
    }

    if ($ShowXml) {
        Write-Host ""
        Write-Host "=== Raw XML ===" -ForegroundColor Gray
        Get-Content $xmlPath
    }
} else {
    Write-Host "ERROR: XML output not found at $xmlPath" -ForegroundColor Red
    $exitCode = 3
}

Write-Host ""
Write-Host "XML results: $xmlPath" -ForegroundColor Gray
exit $exitCode
