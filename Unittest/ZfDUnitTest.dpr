program ZfDUnitTest;

// Debug-Tipp: Einige Tests pruefen erwartete Exceptions (UBLNonAvailability,
// TestInvalidXmlWithException). Der Delphi-Debugger haelt bei diesen an.
// Um das zu unterdruecken:
//   Tools > Options > Debugger Options > Language Exceptions
//   Exception-Typen zur Ignore-Liste hinzufuegen:
//   - TZUGFeRDUnsupportedException
//   - Exception (fuer illegale XML-Zeichen)

{$APPTYPE CONSOLE}

{$STRONGLINKTYPES ON}

uses
  Winapi.ActiveX,
  SysUtils,
  DUnitX.ConsoleWriter.Base,
  DUnitX.DUnitCompatibility,
  DUnitX.Generics,
  DUnitX.InternalInterfaces,
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Text,
  DUnitX.Loggers.XML.NUnit,
  DUnitX.Loggers.XML.xUnit,
//  DUnitX.MacOS.Console,
  DUnitX.Test,
  DUnitX.TestFixture,
  DUnitX.TestFramework,
  DUnitX.TestResult,
  DUnitX.RunResults,
  DUnitX.TestRunner,
  DUnitX.Utils,
  DUnitX.Utils.XML,
  DUnitX.WeakReference,
  DUnitX.Windows.Console,
  DUnitX.StackTrace.EurekaLog7,
  DUnitX.FixtureResult,
  DUnitX.Loggers.Null,
  DUnitX.MemoryLeakMonitor.Default,
  DUnitX.Extensibility,
  DUnitX.CommandLine.OptionDef,
  DUnitX.CommandLine.Options,
  DUnitX.CommandLine.Parser,
  DUnitX.Timeout,
  DUnitX.Attributes,
//  DUnitX.Linux.Console,

  // Test infrastructure
  intf.ZUGFeRDTestBase in 'intf.ZUGFeRDTestBase.pas',
  intf.ZUGFeRDInvoiceProvider in 'intf.ZUGFeRDInvoiceProvider.pas',

  // Test units
  intf.ZUGFeRD10Tests.UnitTests in 'intf.ZUGFeRD10Tests.UnitTests.pas',
  intf.ZUGFeRD20Tests.UnitTests in 'intf.ZUGFeRD20Tests.UnitTests.pas',
  intf.ZUGFeRD22Tests.UnitTests in 'intf.ZUGFeRD22Tests.UnitTests.pas',
  intf.ZUGFeRDCrossVersionTests.UnitTests in 'intf.ZUGFeRDCrossVersionTests.UnitTests.pas',
  intf.XRechnungUBLTests.UnitTests in 'intf.XRechnungUBLTests.UnitTests.pas'
//  intf.ZUGFeRDDataTypeReaderTests.UnitTests in 'intf.ZUGFeRDDataTypeReaderTests.UnitTests.pas'
  ;

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
  xmlOutputFile : string;
begin
  CoInitialize(nil);
  try
    // Warn when running under the Delphi debugger
    {$WARN SYMBOL_PLATFORM OFF}
    if DebugHook <> 0 then
    {$WARN SYMBOL_PLATFORM DEFAULT}
    begin
      System.Writeln('');
      System.Writeln('*** WARNING: Running under the Delphi debugger ***');
      System.Writeln('');
      System.Writeln('Some tests intentionally raise exceptions (e.g. TestInvalidXmlWithException,');
      System.Writeln('TestUBLNonAvailability). The debugger will stop at these expected exceptions.');
      System.Writeln('');
      System.Writeln('To suppress this, go to:');
      System.Writeln('  Tools > Options > Debugger Options > Language Exceptions');
      System.Writeln('and add these exception types to the ignore list:');
      System.Writeln('  - TZUGFeRDUnsupportedException');
      System.Writeln('  - Exception (for illegal XML character tests)');
      System.Writeln('');
      System.Writeln('Press <Enter> to continue anyway...');
      System.Readln;
    end;

    runner := TDUnitX.CreateRunner;
    runner.UseRTTI := True;

    if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
    begin
      logger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
      runner.AddLogger(logger);
    end;

    // Use explicit path so Claude Code always finds the XML at the same location
    xmlOutputFile := TDUnitX.Options.XMLOutputFile;
    if xmlOutputFile = '' then
      xmlOutputFile := ExtractFilePath(ParamStr(0)) + 'dunitx-results.xml';

    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(xmlOutputFile);
    runner.AddLogger(nunitLogger);

    results := runner.Execute;

    ExitCode := 0;
    if not results.AllPassed then
      ExitCode := 1;

    // Only wait for keypress when running interactively (stdin is a console)
    if IsConsole then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
  except
    on E: Exception do
    begin
      System.Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 2;
    end;
  end;
  CoUninitialize;
end.
