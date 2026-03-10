program ZfDUnitTestGUI;

// DUnitX VCL GUI Test Runner
// Allows interactive test execution with graphical result display.
// The GUI runner also writes NUnit-compatible XML output to dunitx-results.xml.

uses
  Winapi.ActiveX,
  Vcl.Forms,
  SysUtils,
  DUnitX.Loggers.GUI.VCL,
  DUnitX.Loggers.XML.NUnit,
  DUnitX.TestFramework,

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

{$R *.res}

begin
  CoInitialize(nil);
  try
    // The VCL GUI runner registers itself as a logger and runs tests automatically.
    // NUnit XML is also written so results can be read programmatically.
    TDUnitX.Options.XMLOutputFile := ExtractFilePath(ParamStr(0)) + 'dunitx-results.xml';

    Application.Initialize;
    Application.Title := 'ZfD Unit Tests';
    Application.CreateForm(TGUIVCLTestRunner, GUIVCLTestRunner);
    Application.Run;
  finally
    CoUninitialize;
  end;
end.
