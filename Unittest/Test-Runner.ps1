# Black-box regression tests for the compiled console runner and run-tests.ps1.
# Run with Windows PowerShell 5.1 after building ZfDUnitTest.dproj.
# Logs and isolated fixtures remain in ResultsDirectory for inspection.
param(
    [string]$ExePath = "$PSScriptRoot\ZfDUnitTest.exe",
    [string]$ResultsDirectory = (Join-Path ([IO.Path]::GetTempPath()) ('zfd-runner-' + [Guid]::NewGuid().ToString('N')))
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$completedChecks = 0
$helper = Join-Path $PSScriptRoot 'run-tests.ps1'
# Der laufende Host, nicht fest powershell.exe: unter PowerShell 7 heisst die Datei in
# $PSHOME pwsh.exe, und das Zusammensetzen des alten Namens brach den Lauf mit einem
# "Datei nicht gefunden" ab, bevor auch nur eine Pruefung lief.
$powershell = (Get-Process -Id $PID).Path
if ([string]::IsNullOrEmpty($powershell) -or -not (Test-Path -LiteralPath $powershell)) {
    throw 'The running PowerShell host executable could not be determined.'
}
$heapFixture = 'intf.ZUGFeRDTestInfrastructure.UnitTests.TZUGFeRDTestInfrastructureTests'
$retainedTest = "$heapFixture.TestHeapAssertionDetectsRetainedObject"
$releasedTest = "$heapFixture.TestHeapAssertionAcceptsReleasedObject"

# Baut die Stub-Exe fuer die Report-Faelle. PowerShell 7 kennt "Add-Type -OutputAssembly"
# nicht mehr ("assembly types ... are not currently supported"), deshalb wird der Bau dort
# an das immer vorhandene Windows PowerShell 5.1 abgegeben.
function New-StubExecutable {
    param([string]$OutputPath, [string]$Source)
    try {
        Add-Type -OutputAssembly $OutputPath -OutputType ConsoleApplication -TypeDefinition $Source
        return
    } catch [System.PlatformNotSupportedException], [System.Management.Automation.PSNotSupportedException] {
    } catch {
        if ($_.Exception.Message -notmatch 'not currently supported') { throw }
    }

    $windowsPowerShell = Join-Path $env:WINDIR 'System32\WindowsPowerShell\v1.0\powershell.exe'
    if (-not (Test-Path -LiteralPath $windowsPowerShell)) {
        throw 'Building the stub executable requires Windows PowerShell 5.1.'
    }
    $sourcePath = Join-Path ([IO.Path]::GetDirectoryName($OutputPath)) 'ReportProbe.cs'
    [IO.File]::WriteAllText($sourcePath, $Source)
    $command = "Add-Type -OutputAssembly '$OutputPath' -OutputType ConsoleApplication " +
        "-TypeDefinition ([IO.File]::ReadAllText('$sourcePath'))"
    & $windowsPowerShell -NoProfile -ExecutionPolicy Bypass -Command $command
    if ($LASTEXITCODE -ne 0 -or -not (Test-Path -LiteralPath $OutputPath)) {
        throw 'Windows PowerShell could not build the stub executable.'
    }
}

# Each process has separate logs and a timeout so a broken runner cannot hang the verification.
function Invoke-CheckedProcess {
    param([string]$Name, [string]$FileName, [string]$Arguments, [int]$ExpectedExit,
        [string]$XmlFile = '', [int]$ExpectedCount = -1)

    $process = New-Object System.Diagnostics.Process
    try {
        $process.StartInfo.FileName = $FileName
        $process.StartInfo.Arguments = $Arguments
        $process.StartInfo.UseShellExecute = $false
        $process.StartInfo.RedirectStandardInput = $true
        $process.StartInfo.RedirectStandardOutput = $true
        $process.StartInfo.RedirectStandardError = $true
        if (-not $process.Start()) { throw "$Name did not start." }
        $process.StandardInput.Close()
        $stdout = $process.StandardOutput.ReadToEndAsync()
        $stderr = $process.StandardError.ReadToEndAsync()
        if (-not $process.WaitForExit(150000)) {
            $process.Kill()
            $process.WaitForExit()
            throw "$Name exceeded its timeout."
        }
        [IO.File]::WriteAllText((Join-Path $ResultsDirectory "$Name.log"), $stdout.GetAwaiter().GetResult())
        [IO.File]::WriteAllText((Join-Path $ResultsDirectory "$Name.err"), $stderr.GetAwaiter().GetResult())
        if ($process.ExitCode -ne $ExpectedExit) {
            throw "$Name returned $($process.ExitCode), expected $ExpectedExit. See $ResultsDirectory."
        }
        if ($ExpectedCount -ge 0) {
            [xml]$report = Get-Content -LiteralPath $XmlFile -Raw -Encoding UTF8
            $cases = @($report.SelectNodes('/test-results//test-case[@executed="True"]'))
            if ($cases.Count -ne $ExpectedCount -or [int]$report.DocumentElement.GetAttribute('total') -ne $ExpectedCount) {
                throw "$Name executed $($cases.Count) tests, expected $ExpectedCount."
            }
        }
        $script:completedChecks++
        Write-Output "PASS: $Name"
    } finally {
        $process.Dispose()
    }
}

# Test names are checked as well as counts; another passing test must not satisfy a selection probe.
function Assert-TestNames {
    param([string]$XmlFile, [string[]]$ExpectedNames)
    [xml]$report = Get-Content -LiteralPath $XmlFile -Raw -Encoding UTF8
    $names = @($report.SelectNodes('/test-results//test-case[@executed="True"]') |
        ForEach-Object { $_.GetAttribute('name') })
    if (@(Compare-Object ($ExpectedNames | Sort-Object) ($names | Sort-Object)).Count -ne 0) {
        throw "Unexpected selected test names in $XmlFile."
    }
}

try {
    $ExePath = (Get-Item -LiteralPath $ExePath).FullName
    if (Test-Path -LiteralPath $ResultsDirectory) { throw 'ResultsDirectory must not already exist.' }
    $ResultsDirectory = (New-Item -ItemType Directory -Path $ResultsDirectory).FullName

    $singleXml = Join-Path $ResultsDirectory 'single.xml'
    Invoke-CheckedProcess 'direct-single' $ExePath "--run:$retainedTest --xmlfile:`"$singleXml`"" 0 $singleXml 1
    Assert-TestNames $singleXml @('TestHeapAssertionDetectsRetainedObject')

    $fixtureXml = Join-Path $ResultsDirectory 'fixture.xml'
    Invoke-CheckedProcess 'direct-fixture' $ExePath "--run:$heapFixture --xmlfile:`"$fixtureXml`"" 0 $fixtureXml 2
    Assert-TestNames $fixtureXml @('TestHeapAssertionDetectsRetainedObject', 'TestHeapAssertionAcceptsReleasedObject')

    $emptyXml = Join-Path $ResultsDirectory 'empty.xml'
    Invoke-CheckedProcess 'direct-no-match' $ExePath "--run:NoSuchZfDTest --xmlfile:`"$emptyXml`"" 3 $emptyXml 0
    Invoke-CheckedProcess 'direct-invalid-option' $ExePath '--this-option-does-not-exist:42' 2

    # A bare parameterized method is not a case selector; never silently run the whole fixture.
    $parameterMethod = 'intf.ZUGFeRD22Tests.UnitTests.TZUGFeRD22Tests.TestCIIReaderNestedObjectOwnership'
    $parameterCase = "$parameterMethod.ValidDocument"
    foreach ($viaHelper in @($false, $true)) {
        $prefix = if ($viaHelper) { 'helper' } else { 'direct' }
        foreach ($exactCase in @($false, $true)) {
            $name = if ($exactCase) { "$prefix-parameter-case" } else { "$prefix-parameter-method" }
            $filter = if ($exactCase) { $parameterCase } else { $parameterMethod }
            $expectedExit = if ($exactCase) { 0 } else { 3 }
            $expectedCount = if ($exactCase) { 1 } else { 0 }
            $parameterXml = Join-Path $ResultsDirectory "$name.xml"
            if ($viaHelper) {
                Invoke-CheckedProcess $name $powershell "-NoProfile -ExecutionPolicy Bypass -File `"$helper`" -ExePath `"$ExePath`" -XmlPath `"$parameterXml`" -Filter $filter" $expectedExit $parameterXml $expectedCount
            } else {
                Invoke-CheckedProcess $name $ExePath "--run:$filter --xmlfile:`"$parameterXml`"" $expectedExit $parameterXml $expectedCount
            }
            if ($exactCase) { Assert-TestNames $parameterXml @('TestCIIReaderNestedObjectOwnership.ValidDocument') }
        }
    }

    # An option error can precede logger creation. A surviving report does not belong to that run.
    $previousSingleReport = [IO.File]::ReadAllBytes($singleXml)
    Invoke-CheckedProcess 'direct-invalid-option-existing-report' $ExePath "--this-option-does-not-exist:42 --xmlfile:`"$singleXml`"" 2
    if ([Convert]::ToBase64String([IO.File]::ReadAllBytes($singleXml)) -ne [Convert]::ToBase64String($previousSingleReport)) {
        throw 'The early option error unexpectedly replaced the report.'
    }

    # Check the reviewed source list and prove detection of each encoding error in isolated fixtures.
    $encodingCheck = Join-Path $PSScriptRoot 'Test-SourceEncoding.ps1'
    Invoke-CheckedProcess 'source-encoding' $powershell "-NoProfile -ExecutionPolicy Bypass -File `"$encodingCheck`"" 0
    $unicodeText = 'ung' + [char]252 + 'ltig'
    foreach ($encodingCase in @('bom', 'ascii', 'missing-bom', 'invalid-utf8', 'missing-file')) {
        $sourcePath = Join-Path $ResultsDirectory "$encodingCase.pas"
        $expectedExit = 1
        switch ($encodingCase) {
            'bom' { [IO.File]::WriteAllText($sourcePath, $unicodeText, (New-Object Text.UTF8Encoding($true))); $expectedExit = 0 }
            'ascii' { [IO.File]::WriteAllText($sourcePath, 'unit ASCII;', (New-Object Text.UTF8Encoding($false))); $expectedExit = 0 }
            'missing-bom' { [IO.File]::WriteAllText($sourcePath, $unicodeText, (New-Object Text.UTF8Encoding($false))) }
            'invalid-utf8' { [IO.File]::WriteAllBytes($sourcePath, [byte[]]@(239, 187, 191, 195, 40)) }
        }
        Invoke-CheckedProcess "encoding-$encodingCase" $powershell "-NoProfile -ExecutionPolicy Bypass -File `"$encodingCheck`" -Paths `"$sourcePath`"" $expectedExit
    }

    # Check the tracked sources and prove detection of each uncast Count comparison.
    $countCheck = Join-Path $PSScriptRoot 'Test-CountAssertions.ps1'
    Invoke-CheckedProcess 'count-assertions' $powershell "-NoProfile -ExecutionPolicy Bypass -File `"$countCheck`"" 0
    $countCases = [ordered]@{
        'literal-first' = @('  Assert.AreEqual(1, invoice.TradeLineItems.Count);', 1)
        'literal-second' = @('  Assert.AreEqual(invoice.Taxes.Count, 2);', 1)
        'with-message' = @("  Assert.AreEqual(0, invoice.Notes.Count, 'msg');", 1)
        'cast' = @('  Assert.AreEqual(1, Integer(invoice.TradeLineItems.Count));', 0)
        'unrelated' = @('  Assert.AreEqual(1, Length(bytes));', 0)
    }
    foreach ($countCase in $countCases.Keys) {
        $sourcePath = Join-Path $ResultsDirectory "count-$countCase.pas"
        [IO.File]::WriteAllText($sourcePath, "unit Probe;`r`n$($countCases[$countCase][0])`r`n")
        Invoke-CheckedProcess "count-$countCase" $powershell "-NoProfile -ExecutionPolicy Bypass -File `"$countCheck`" -Paths `"$sourcePath`"" $countCases[$countCase][1]
    }
    Invoke-CheckedProcess 'count-missing-file' $powershell "-NoProfile -ExecutionPolicy Bypass -File `"$countCheck`" -Paths `"$(Join-Path $ResultsDirectory 'absent.pas')`"" 1

    $fullXml = Join-Path $ResultsDirectory 'suite.xml'
    Invoke-CheckedProcess 'helper-suite' $powershell "-NoProfile -ExecutionPolicy Bypass -File `"$helper`" -ExePath `"$ExePath`" -XmlPath `"$fullXml`"" 0
    [xml]$fullReport = Get-Content -LiteralPath $fullXml -Raw -Encoding UTF8
    $fullCount = @($fullReport.SelectNodes('/test-results//test-case[@executed="True"]')).Count
    if ($fullCount -le 2) { throw 'The full suite was unexpectedly filtered.' }

    $helperEmptyXml = Join-Path $ResultsDirectory 'helper-empty.xml'
    Invoke-CheckedProcess 'helper-no-match' $powershell "-NoProfile -ExecutionPolicy Bypass -File `"$helper`" -ExePath `"$ExePath`" -XmlPath `"$helperEmptyXml`" -Filter NoSuchZfDTest" 3 $helperEmptyXml 0

    # A clean copy tests default paths, spaces, and the absence of the old stdin placeholder.
    $isolated = (New-Item -ItemType Directory -Path (Join-Path $ResultsDirectory 'clean copy with spaces')).FullName
    $isolatedHelper = Join-Path $isolated 'run-tests.ps1'
    Copy-Item -LiteralPath $helper -Destination $isolatedHelper
    Copy-Item -LiteralPath $ExePath -Destination (Join-Path $isolated 'ZfDUnitTest.exe')
    $isolatedXml = Join-Path $isolated 'dunitx-results.xml'
    Invoke-CheckedProcess 'helper-clean-copy' $powershell "-NoProfile -ExecutionPolicy Bypass -File `"$isolatedHelper`" -Filter $releasedTest" 0 $isolatedXml 1
    Assert-TestNames $isolatedXml @('TestHeapAssertionAcceptsReleasedObject')

    $failingTest = 'intf.ZUGFeRD22Tests.UnitTests.TZUGFeRD22Tests.TestCIIReaderReleasesDescriptorAfterParsingError'
    $readerXml = Join-Path $ResultsDirectory 'reader.xml'
    Invoke-CheckedProcess 'direct-reader-heap' $ExePath "--run:$failingTest --xmlfile:`"$readerXml`"" 0 $readerXml 1
    Assert-TestNames $readerXml @('TestCIIReaderReleasesDescriptorAfterParsingError')
    Invoke-CheckedProcess 'helper-test-failure' $powershell "-NoProfile -ExecutionPolicy Bypass -File `"$isolatedHelper`" -Filter $failingTest" 1 $isolatedXml 1

    # Controlled executables exercise report failures without changing production fixtures or source files.
    $stubExe = Join-Path $isolated 'ReportProbe.exe'
    New-StubExecutable $stubExe @'
using System;
using System.IO;
using System.Threading;
public static class ReportProbe
{
    public static int Main(string[] arguments)
    {
        string mode = File.ReadAllText(Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "mode.txt"));
        string output = null;
        foreach (string argument in arguments)
            if (argument.StartsWith("--xmlfile:", StringComparison.Ordinal)) output = argument.Substring(10);
        if (mode == "timeout") { Thread.Sleep(10000); return 0; }
        if (mode == "missing") return 0;
        if (mode == "malformed") { File.WriteAllText(output, "<invalid"); return 0; }
        if (mode == "wrong-root") { File.WriteAllText(output, "<wrong/>"); return 0; }
        string count = mode == "wrong-count" ? "2" : "1";
        string result = mode == "failure" ? "Failure" : "Success";
        File.WriteAllText(output, "<test-results total=\"" + count + "\" failures=\"0\" errors=\"0\"><test-suite><test-case name=\"Probe\" executed=\"True\" result=\"" + result + "\"/></test-suite></test-results>");
        return mode == "nonzero" ? 7 : 0;
    }
}
'@
    $modePath = Join-Path $isolated 'mode.txt'
    $stubArguments = "-NoProfile -ExecutionPolicy Bypass -File `"$isolatedHelper`" -ExePath `"$stubExe`""
    foreach ($mode in @('valid', 'failure', 'nonzero', 'missing', 'malformed', 'wrong-root', 'wrong-count', 'timeout')) {
        [IO.File]::WriteAllText($modePath, $mode)
        $previousReport = [IO.File]::ReadAllText($isolatedXml)
        $expectedExit = 4
        $arguments = $stubArguments
        switch ($mode) {
            'valid' { $expectedExit = 0 }
            'failure' { $expectedExit = 1 }
            'nonzero' { $expectedExit = 7 }
            'timeout' { $arguments += ' -TimeoutSeconds 1' }
        }
        Invoke-CheckedProcess "report-$mode" $powershell $arguments $expectedExit
        if ($expectedExit -eq 4 -and [IO.File]::ReadAllText($isolatedXml) -ne $previousReport) {
            throw "The invalid $mode result replaced an existing report."
        }
    }
    Invoke-CheckedProcess 'helper-missing-exe' $powershell "-NoProfile -ExecutionPolicy Bypass -File `"$isolatedHelper`" -ExePath `"$isolated\missing.exe`"" 4
    Write-Output "$completedChecks process checks passed; full Delphi suite: $fullCount tests. Logs: $ResultsDirectory"
} catch {
    [Console]::Error.WriteLine($_.Exception.Message)
    exit 1
}
