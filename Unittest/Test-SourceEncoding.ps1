param([string[]]$Paths = @(
    "$PSScriptRoot\intf.ZUGFeRDInvoiceValidatorTests.UnitTests.pas",
    "$PSScriptRoot\intf.ZUGFeRDCrossVersionTests.UnitTests.pas",
    "$PSScriptRoot\intf.XRechnungUBLTests.UnitTests.pas",
    "$PSScriptRoot\intf.ZUGFeRDDocumentationSweep.UnitTests.pas",
    "$PSScriptRoot\..\intf.ZUGFeRDInvoiceDescriptor22UBLWriter.pas",
    "$PSScriptRoot\..\intf.ZUGFeRDInvoiceDescriptor22UblReader.pas",
    "$PSScriptRoot\..\intf.ZUGFeRDProfileAwareXmlTextWriter.pas",
    "$PSScriptRoot\..\intf.ZUGFeRDAdvancePayment.pas"
))

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

# Delphi 11 needs an explicit UTF-8 signature for non-ASCII source on an ANSI system locale.
try {
    if ($Paths.Count -eq 0) { throw 'No source files selected.' }
    $decoder = New-Object System.Text.UTF8Encoding($false, $true)
    foreach ($path in $Paths) {
        $bytes = [IO.File]::ReadAllBytes($path)
        $hasBom = $bytes.Length -ge 3 -and $bytes[0] -eq 239 -and $bytes[1] -eq 187 -and $bytes[2] -eq 191
        $offset = if ($hasBom) { 3 } else { 0 }
        $content = $decoder.GetString($bytes, $offset, $bytes.Length - $offset)
        if (-not $hasBom -and $content -match '[^\x00-\x7F]') {
            throw "Non-ASCII Delphi source requires a UTF-8 BOM: $path"
        }
    }
    Write-Output "Source encoding checked: $($Paths.Count) files."
    exit 0
} catch {
    [Console]::Error.WriteLine($_.Exception.Message)
    exit 1
}
