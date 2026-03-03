program ZfDUnitTest;

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

    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);

    results := runner.Execute;

    System.Write('Done.. press <Enter> key to quit.');
    System.Readln;
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
  CoUninitialize;
end.
