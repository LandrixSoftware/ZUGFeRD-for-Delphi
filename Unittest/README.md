# Running the test suite

The DUnitX suite lives here as a console runner (`ZfDUnitTest.dpr`) and a GUI runner
(`ZfDUnitTestGUI.dpr`). Build `ZfDUnitTest.dproj` for Win64/Debug; the executable is written
directly to this directory, not to `Win64\Debug\`.

From this directory, Windows PowerShell 5.1 can run the suite or select a fully qualified
DUnitX test or fixture name:

```powershell
.\run-tests.ps1
.\run-tests.ps1 -Filter 'intf.ZUGFeRD22Tests.UnitTests.TZUGFeRD22Tests.TestCIIReaderReleasesDescriptorAfterParsingError'
.\run-tests.ps1 -Filter 'intf.ZUGFeRD22Tests.UnitTests.TZUGFeRD22Tests.TestCIIReaderNestedObjectOwnership.ValidDocument'
.\Test-SourceEncoding.ps1
.\Test-Runner.ps1
```

## Selecting tests

The native DUnitX syntax uses colons: `--run:<qualified-name>`, `--xmlfile:<path>` and
`--exitbehavior:Continue`. `--filter` and `--option=value` are not supported by the DUnitX
version that ships with Delphi. The helper passes `--run` and closes stdin, so non-interactive
runs need neither a placeholder file nor an Enter keypress.

`ZfDUnitTest.dpr` calls `TDUnitX.CheckCommandLine` before creating the runner. `TDUnitX.CreateRunner`
does not do this itself, so without that call the console runner ignored every command-line
option and always ran the full suite.

Parameterized tests require the case suffix: `<unit>.<fixture>.<method>.<case>`. The bare
parameterized method selects no executable cases and returns exit code `3`; selecting its
fixture runs all of that fixture's cases. The helper does not broaden filters.

## Exit codes

| Code | Meaning |
|---|---|
| `0` | tests executed successfully |
| `1` | test failures |
| `2` | runner exception or invalid command-line option |
| `3` | no tests executed |
| `4` | infrastructure failure in the PowerShell helper (missing/invalid XML, timeout) |

The helper parses a unique report for each invocation before publishing `dunitx-results.xml`;
an existing report is not reused as evidence for a failed invocation. `-XmlPath` selects another
report destination and `-TimeoutSeconds` controls the process timeout.

Always check the exit code and the current invocation's report together. An early native option
error returns `2` before logger creation and can leave an earlier XML file untouched. The helper
can likewise leave its published report unchanged when the current run fails before producing a
valid report; that file is not a success result for the failed run.

## Supporting scripts

`Test-Runner.ps1` verifies test names, counts and exit codes, including missing fixtures, invalid
reports and an isolated executable path containing spaces. It requires the compiled console runner
and retains its logs and generated fixtures in a unique temporary directory, or in a new directory
given by `-ResultsDirectory`.

`Test-SourceEncoding.ps1` checks strict UTF-8 and requires a BOM for non-ASCII source, so that
Delphi does not interpret Unicode diagnostics using the system ANSI code page on a machine whose
locale is not UTF-8. Without arguments it sweeps every tracked `.pas`, `.dpr` and `.inc` file in
the repository; `-Paths` limits the check to explicit files. Run it before committing source
changes. `Test-Runner.ps1` includes valid ASCII/UTF-8 cases as well as missing-BOM, invalid-UTF-8
and missing-file counterexamples, and `TestDiagnosticEncoding` captures a real assertion
diagnostic and checks its Unicode code point independently of source literals.

The documentation sweep includes long example paths. On Windows, invoke the executable through a
short `subst` drive alias when the checkout path would exceed `MAX_PATH`.

## What the memory checks do and do not prove

The default DUnitX memory monitor always reports zero: **`Tests Leaked: 0` does not prove that
tests are leak-free.** Ownership regressions use explicit allocated-heap snapshots from
`TZUGFeRDTestBase` around warmed-up, isolated operations. The infrastructure tests prove that the
assertion rejects a retained object and accepts a released object, without leaving the
counterexample allocated. These checks cover only the measured paths, not every test or every
possible reader exception. No replacement memory manager is installed.

The reader ownership tests cover CII 1.0/2.0 and UBL Invoice/CreditNote via direct readers and
automatic dispatch, including successful loads, early/late date errors and payment terms errors.
The CII 2.3 nested-object test has one common valid case and seven distinct date failures. Its
`AfterAttachment` cases verify parent cleanup after a second reference fails, not a decoder failure
in that reference's own attachment. COM, allocation and list insertion failures are not
exhaustively covered by the regular suite.
