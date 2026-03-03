unit intf.ZUGFeRD10Tests.UnitTests;

interface

uses
  DUnitX.TestFramework,
  intf.ZUGFeRDTestBase;

type
  [TestFixture]
  TZUGFeRD10Tests = class(TZUGFeRDTestBase)
  public
    [Test]
    procedure TestReferenceComfortInvoice;
    [Test]
    procedure TestReferenceComfortInvoiceRabattiert;
    [Test]
    procedure TestStoringInvoiceViaFile;
    [Test]
    procedure TestStoringInvoiceViaStreams;
    [Test]
    procedure TestBICIDForDebitorFinancialInstitution;
    [Test]
    procedure TestMissingPropertiesAreNull;
    [Test]
    procedure TestSpecifiedTradePaymentTermsDescription;
    [Test]
    procedure TestSpecifiedTradePaymentTermsCalculationPercent;
  end;

implementation

uses
  System.SysUtils, System.Classes, System.IOUtils,
  intf.ZUGFeRDInvoiceDescriptor,
  intf.ZUGFeRDInvoiceProvider,
  intf.ZUGFeRDProfile,
  intf.ZUGFeRDInvoiceTypes,
  intf.ZUGFeRDVersion,
  intf.ZUGFeRDHelper,
  intf.ZUGFeRDPaymentTerms;

{ TZUGFeRD10Tests }

procedure TZUGFeRD10Tests.TestReferenceComfortInvoice;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd10\ZUGFeRD_1p0_COMFORT_Einfach.xml'));
  try
    Assert.AreEqual(TZUGFeRDProfile.Comfort, desc.Profile);
    Assert.AreEqual(TZUGFeRDInvoiceType.Invoice, desc.Type_);
    Assert.IsTrue(desc.IsTest);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD10Tests.TestReferenceComfortInvoiceRabattiert;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd10\ZUGFeRD_1p0_COMFORT_Rabatte.xml'));
  try
    desc.Save('test.xml', TZUGFeRDVersion.Version1, TZUGFeRDProfile.Comfort);
    Assert.AreEqual(TZUGFeRDProfile.Comfort, desc.Profile);
    Assert.AreEqual(TZUGFeRDInvoiceType.Invoice, desc.Type_);
    Assert.AreEqual('Hausbank M'+#$00FC+'nchen', desc.CreditorBankAccounts[0].BankName);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD10Tests.TestStoringInvoiceViaFile;
var
  desc, desc2: TZUGFeRDInvoiceDescriptor;
  path: string;
begin
  path := 'output.xml';
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.Save(path, TZUGFeRDVersion.Version1, TZUGFeRDProfile.Comfort);
    desc2 := TZUGFeRDInvoiceDescriptor.Load(path);
    try
      Assert.IsNotNull(desc2);
    finally
      desc2.Free;
    end;
  finally
    desc.Free;
    if TFile.Exists(path) then
      TFile.Delete(path);
  end;
end;

procedure TZUGFeRD10Tests.TestStoringInvoiceViaStreams;
var
  desc, desc2: TZUGFeRDInvoiceDescriptor;
  path: string;
  saveStream, loadStream: TFileStream;
  ms: TMemoryStream;
  data: TBytes;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    path := 'output_stream.xml';
    saveStream := TFileStream.Create(path, fmCreate);
    try
      desc.Save(saveStream, TZUGFeRDVersion.Version1, TZUGFeRDProfile.Comfort);
    finally
      saveStream.Free;
    end;

    loadStream := TFileStream.Create(path, fmOpenRead);
    try
      desc2 := TZUGFeRDInvoiceDescriptor.Load(loadStream);
      try
        Assert.AreEqual(TZUGFeRDProfile.Comfort, desc2.Profile);
        Assert.AreEqual(TZUGFeRDInvoiceType.Invoice, desc2.Type_);
      finally
        desc2.Free;
      end;
    finally
      loadStream.Free;
    end;

    // try again with a memory stream
    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version1, TZUGFeRDProfile.Comfort);
      SetLength(data, ms.Size);
      ms.Position := 0;
      ms.ReadBuffer(data[0], Length(data));
      Assert.IsTrue(Length(data) > 0);
    finally
      ms.Free;
    end;
  finally
    desc.Free;
    if TFile.Exists(path) then
      TFile.Delete(path);
  end;
end;

procedure TZUGFeRD10Tests.TestBICIDForDebitorFinancialInstitution;
var
  desc: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  sr: TStreamReader;
  text: string;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.AddDebitorFinancialAccount('DE02120300000000202051', 'MYBIC');

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version1, TZUGFeRDProfile.Comfort);
      ms.Position := 0;

      // Read as text for debugging
      sr := TStreamReader.Create(ms, TEncoding.UTF8);
      try
        text := sr.ReadToEnd;
      finally
        sr.Free;
      end;

      Assert.IsTrue(text.Contains('MYBIC'), 'MYBIC should be present in ZUGFeRD 1.0 output');
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD10Tests.TestMissingPropertiesAreNull;
var
  desc: TZUGFeRDInvoiceDescriptor;
  i: Integer;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd10\ZUGFeRD_1p0_COMFORT_Einfach.xml'));
  try
    for i := 0 to desc.TradeLineItems.Count - 1 do
    begin
      Assert.IsFalse(desc.TradeLineItems[i].BillingPeriodStart.HasValue,
        Format('TradeLineItem[%d].BillingPeriodStart should be null', [i]));
      Assert.IsFalse(desc.TradeLineItems[i].BillingPeriodEnd.HasValue,
        Format('TradeLineItem[%d].BillingPeriodEnd should be null', [i]));
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD10Tests.TestSpecifiedTradePaymentTermsDescription;
var
  desc: TZUGFeRDInvoiceDescriptor;
  terms: TZUGFeRDPaymentTerms;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd10\ZUGFeRD_1p0_EXTENDED_Warenrechnung.xml'));
  try
    Assert.IsTrue(desc.PaymentTermsList.Count > 0, 'PaymentTerms should not be empty');
    terms := desc.PaymentTermsList[0];
    Assert.IsNotNull(terms);
    Assert.AreEqual('Bei Zahlung innerhalb 14 Tagen gew'+#$00E4+'hren wir 2,0% Skonto.', terms.Description);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD10Tests.TestSpecifiedTradePaymentTermsCalculationPercent;
var
  desc: TZUGFeRDInvoiceDescriptor;
  terms: TZUGFeRDPaymentTerms;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd10\ZUGFeRD_1p0_EXTENDED_Warenrechnung.xml'));
  try
    Assert.IsTrue(desc.PaymentTermsList.Count > 0, 'PaymentTerms should not be empty');
    terms := desc.PaymentTermsList[0];
    Assert.IsTrue(terms.Percentage.HasValue, 'Percentage should have a value');
    Assert.AreEqual<Currency>(2, terms.Percentage.Value);
  finally
    desc.Free;
  end;
end;

end.
