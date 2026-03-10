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
  exitCode : Integer;
begin
  CoInitialize(nil);
  try
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

    exitCode := 0;
    if not results.AllPassed then
      exitCode := 1;

    // Only wait for keypress when running interactively (stdin is a console)
    if IsConsole then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;

    ExitCode := exitCode;
  except
    on E: Exception do
    begin
      System.Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 2;
    end;
  end;
  CoUninitialize;
end.
