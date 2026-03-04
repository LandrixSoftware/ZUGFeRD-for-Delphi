{* Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.}

unit intf.ZUGFeRDCrossVersionTests.UnitTests;

interface

uses
  DUnitX.TestFramework,
  intf.ZUGFeRDTestBase;

type
  [TestFixture]
  TZUGFeRDCrossVersionTests = class(TZUGFeRDTestBase)
  public
    [Test]
    procedure TestAutomaticLineIds;

    [Test]
    [TestCase('V1-Ext',   '100,4')]
    [TestCase('V20-Ext',  '200,4')]
    [TestCase('V23-Ext',  '230,4')]
    procedure TestNoteContentCodes(_version: Integer; _profile: Integer);

    [Test]
    [TestCase('V1-Ext',   '100,4')]
    [TestCase('V20-Ext',  '200,4')]
    [TestCase('V23-Ext',  '230,4')]
    procedure TestNoteSubjectCodes(_version: Integer; _profile: Integer);

    [Test]
    [TestCase('V1-Ext',   '100,4')]
    [TestCase('V20-Ext',  '200,4')]
    [TestCase('V23-Ext',  '230,4')]
    procedure TestKosovoCountryCode(_version: Integer; _profile: Integer);

    [Test]
    [TestCase('V1-Ext',   '100,4')]
    [TestCase('V20-Ext',  '200,4')]
    [TestCase('V23-Ext',  '230,4')]
    procedure TestStandardCountryCode(_version: Integer; _profile: Integer);

    [Test]
    procedure TestManualLineIds;

    [Test]
    procedure TestCommentLine;

    [Test]
    procedure TestGetVersion;

    [Test]
    [TestCase('V1-Ext',       '100,4')]
    [TestCase('V1-XR',        '100,32')]
    [TestCase('V20-Ext',      '200,4')]
    [TestCase('V20-XR',       '200,32')]
    [TestCase('V20-XR1',      '200,64')]
    [TestCase('V23-Ext',      '230,4')]
    [TestCase('V23-XR1',      '230,64')]
    procedure UBLNonAvailability(_version: Integer; _profile: Integer);

    [Test]
    procedure UBLAvailability;

    [Test]
    [TestCase('V1-Ext',   '100,4')]
    [TestCase('V20-Ext',  '200,4')]
    [TestCase('V23-Ext',  '230,4')]
    procedure SavingThenReadingAppliedTradeTaxes(_version: Integer; _profile: Integer);

    [Test]
    [TestCase('V1',  '100')]
    [TestCase('V20', '200')]
    [TestCase('V23', '230')]
    procedure TestDeliveryNoteReferencedDocumentLineIdInExtended(_version: Integer);

    [Test]
    [TestCase('V1',  '100')]
    [TestCase('V20', '200')]
    [TestCase('V23', '230')]
    procedure TestContractReferencedDocumentLineIdInExtended(_version: Integer);

    [Test]
    [TestCase('V1',  '100')]
    [TestCase('V20', '200')]
    [TestCase('V23', '230')]
    procedure TestLongerDecimalPlacesForNetUnitPrice(_version: Integer);

    [Test]
    [TestCase('V1',  '100')]
    [TestCase('V20', '200')]
    [TestCase('V23', '230')]
    procedure TestLongerDecimalPlacesForGrossUnitPrice(_version: Integer);

    [Test]
    [TestCase('V20-Ext-CII', '200,4,0')]
    [TestCase('V23-Ext-CII', '230,4,0')]
    [TestCase('V23-XR-UBL',  '230,32,1')]
    procedure TestSellerTaxRepresentative(_version: Integer; _profile: Integer; _format: Integer);

    [Test]
    [TestCase('V1-CII', '100,0')]
    procedure TestSellerTaxRepresentativeInNonSupportedVersions(_version: Integer; _format: Integer);

    [Test]
    [TestCase('V1',  '100')]
    [TestCase('V20', '200')]
    [TestCase('V23', '230')]
    procedure TestTransportModeWithExtended(_version: Integer);

    [Test]
    [TestCase('V1',  '100')]
    [TestCase('V20', '200')]
    [TestCase('V23', '230')]
    procedure TestTransportModeWithComfort(_version: Integer);

    [Test]
    [TestCase('V20', '200')]
    [TestCase('V23', '230')]
    procedure TestGrossPriceRepresentationForXRechnungAndNotXRechnungNegativeCase(_version: Integer);

    [Test]
    [TestCase('V20', '200')]
    [TestCase('V23', '230')]
    procedure TestGrossPriceRepresentationForXRechnungAndNotXRechnungPositiveCase(_version: Integer);

    [Test]
    [TestCase('V1-CII-Ext',  '100,0,4')]
    [TestCase('V20-CII-Ext', '200,0,4')]
    [TestCase('V23-CII-Ext', '230,0,4')]
    [TestCase('V23-UBL-XR',  '230,1,32')]
    procedure TestHeaderComment(_version: Integer; _format: Integer; _profile: Integer);

    [Test]
    [TestCase('V1-CII-Ext',  '100,0,4')]
    [TestCase('V20-CII-Ext', '200,0,4')]
    [TestCase('V23-CII-Ext', '230,0,4')]
    [TestCase('V23-UBL-XR',  '230,1,32')]
    procedure TestWithoutHeaderComment(_version: Integer; _format: Integer; _profile: Integer);

    [Test]
    [TestCase('V1-CII-Ext',  '100,0,4')]
    [TestCase('V20-CII-Ext', '200,0,4')]
    [TestCase('V23-CII-Ext', '230,0,4')]
    procedure TestZUGFeRDElementComments(_version: Integer; _format: Integer; _profile: Integer);

    [Test]
    [TestCase('V23-UBL-XR', '230,1,32')]
    procedure TestXRechnungElementComments(_version: Integer; _format: Integer; _profile: Integer);

    [Test]
    [TestCase('V1-CII-Ext',  '100,0,4')]
    [TestCase('V20-CII-Ext', '200,0,4')]
    [TestCase('V23-CII-Ext', '230,0,4')]
    [TestCase('V23-UBL-XR',  '230,1,32')]
    procedure TestInvalidXmlWithException(_version: Integer; _format: Integer; _profile: Integer);

    [Test]
    [TestCase('V1-CII-Ext',  '100,0,4')]
    [TestCase('V20-CII-Ext', '200,0,4')]
    [TestCase('V23-CII-Ext', '230,0,4')]
    [TestCase('V23-UBL-XR',  '230,1,32')]
    procedure TestInvalidXmlWithCleaning(_version: Integer; _format: Integer; _profile: Integer);

    [Test]
    [TestCase('V1-CII-Ext',  '100,0,4')]
    [TestCase('V20-CII-Ext', '200,0,4')]
    [TestCase('V23-CII-Ext', '230,0,4')]
    [TestCase('V23-UBL-XR',  '230,1,32')]
    procedure TestValidXml(_version: Integer; _format: Integer; _profile: Integer);

    [Test]
    [TestCase('V1-CII-Ext',  '100,0,4')]
    [TestCase('V20-CII-Ext', '200,0,4')]
    [TestCase('V23-CII-Ext', '230,0,4')]
    procedure TestGrossQuantity(_version: Integer; _format: Integer; _profile: Integer);

    [Test]
    [TestCase('V23-UBL-XR', '230,1,32')]
    procedure TestGrossQuantityForXRechnung(_version: Integer; _format: Integer; _profile: Integer);

    [Test]
    [TestCase('V1-CII-Ext',  '100,0,4')]
    [TestCase('V20-CII-Ext', '200,0,4')]
    [TestCase('V23-CII-Ext', '230,0,4')]
    [TestCase('V23-UBL-XR',  '230,1,32')]
    procedure TestWithoutGrossQuantity(_version: Integer; _format: Integer; _profile: Integer);

    [Test]
    [TestCase('V1-CII-Ext',  '100,0,4')]
    [TestCase('V20-CII-Ext', '200,0,4')]
    [TestCase('V23-CII-Ext', '230,0,4')]
    procedure TestNulledGlobalIDScheme(_version: Integer; _format: Integer; _profile: Integer);

    [Test]
    [TestCase('V20-CII-Ext', '200,0,4')]
    [TestCase('V23-CII-Ext', '230,0,4')]
    procedure TestInvoicerContactWriteAndRead(_version: Integer; _format: Integer; _profile: Integer);

    [Test]
    [TestCase('V20-CII-Ext', '200,0,4')]
    [TestCase('V23-CII-Ext', '230,0,4')]
    [TestCase('V23-CII-XR',  '230,0,32')]
    [TestCase('V23-UBL-XR',  '230,1,32')]
    procedure TestBillingPeriod(_version: Integer; _format: Integer; _profile: Integer);

    [Test]
    [TestCase('V20-CII-Ext', '200,0,4')]
    [TestCase('V23-CII-Ext', '230,0,4')]
    [TestCase('V23-CII-XR',  '230,0,32')]
    [TestCase('V23-UBL-XR',  '230,1,32')]
    procedure TestBillingPeriodOnItemLevel(_version: Integer; _format: Integer; _profile: Integer);

    [Test]
    [TestCase('V1-CII-Ext',  '100,0,4')]
    [TestCase('V20-CII-Ext', '200,0,4')]
    [TestCase('V23-CII-Ext', '230,0,4')]
    [TestCase('V23-UBL-XR',  '230,1,32')]
    procedure TestAvoidEmptyElementsWithDemoInvoice(_version: Integer; _format: Integer; _profile: Integer);

    [Test]
    [TestCase('V1-CII-Comfort',  '100,0,2')]
    [TestCase('V1-CII-Ext',      '100,0,4')]
    [TestCase('V20-CII-Comfort', '200,0,2')]
    [TestCase('V20-CII-Ext',     '200,0,4')]
    [TestCase('V23-CII-Comfort', '230,0,2')]
    [TestCase('V23-CII-Ext',     '230,0,4')]
    [TestCase('V23-UBL-XR',      '230,1,32')]
    procedure TestAvoidEmptyElementsWithMinimalInvoice(_version: Integer; _format: Integer; _profile: Integer);
  end;

implementation

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  Xml.XMLDoc, Xml.XMLIntf,
  intf.ZUGFeRDInvoiceDescriptor,
  intf.ZUGFeRDInvoiceProvider,
  intf.ZUGFeRDProfile,
  intf.ZUGFeRDVersion,
  intf.ZUGFeRDFormats,
  intf.ZUGFeRDHelper,
  intf.ZUGFeRDCurrencyCodes,
  intf.ZUGFeRDCountryCodes,
  intf.ZUGFeRDQuantityCodes,
  intf.ZUGFeRDTaxTypes,
  intf.ZUGFeRDTaxCategoryCodes,
  intf.ZUGFeRDGlobalID,
  intf.ZUGFeRDGlobalIDSchemeIdentifiers,
  intf.ZUGFeRDTradeLineItem,
  intf.ZUGFeRDTradeAllowanceCharge,
  intf.ZUGFeRDAllowanceReasonCodes,
  intf.ZUGFeRDParty,
  intf.ZUGFeRDContact,
  intf.ZUGFeRDNote,
  intf.ZUGFeRDTax,
  intf.ZUGFeRDContentCodes,
  intf.ZUGFeRDSubjectCodes,
  intf.ZUGFeRDTransportModeCodes,
  intf.ZUGFeRDAssociatedDocument,
  intf.ZUGFeRDDeliveryNoteReferencedDocument,
  intf.ZUGFeRDContractReferencedDocument,
  intf.ZUGFeRDExceptions,
  intf.ZUGFeRDInvoiceFormatOptions;

{ TZUGFeRDCrossVersionTests }

procedure TZUGFeRDCrossVersionTests.TestAutomaticLineIds;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.TradeLineItems.Clear;

    desc.AddTradeLineItem('Item1', TZUGFeRDNullableParam<Currency>.Create(0), '',
      TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62));
    desc.AddTradeLineItem('Item2', TZUGFeRDNullableParam<Currency>.Create(0), '',
      TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62));

    Assert.AreEqual('1', desc.TradeLineItems[0].AssociatedDocument.LineID);
    Assert.AreEqual('2', desc.TradeLineItems[1].AssociatedDocument.LineID);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestNoteContentCodes(_version: Integer; _profile: Integer);
var
  version: TZUGFeRDVersion;
  profile: TZUGFeRDProfile;
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  notes: TObjectList<TZUGFeRDNote>;
begin
  version := TZUGFeRDVersion(_version);
  profile := TZUGFeRDProfile(_profile);

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.Notes.Clear;
    desc.AddNote('EEV', nil, TZUGFeRDNullableParam<TZUGFeRDContentCodes>.Create(TZUGFeRDContentCodes.EEV));
    desc.AddNote('WEV', nil, TZUGFeRDNullableParam<TZUGFeRDContentCodes>.Create(TZUGFeRDContentCodes.WEV));
    desc.AddNote('ST1', nil, TZUGFeRDNullableParam<TZUGFeRDContentCodes>.Create(TZUGFeRDContentCodes.ST1));
    desc.AddNote('ST2', nil, TZUGFeRDNullableParam<TZUGFeRDContentCodes>.Create(TZUGFeRDContentCodes.ST2));
    desc.AddNote('ST3', nil, TZUGFeRDNullableParam<TZUGFeRDContentCodes>.Create(TZUGFeRDContentCodes.ST3));
    desc.AddNote('VEV', nil, TZUGFeRDNullableParam<TZUGFeRDContentCodes>.Create(TZUGFeRDContentCodes.VEV));

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, version, profile);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        notes := loadedInvoice.Notes;
        Assert.AreEqual(TZUGFeRDContentCodes.EEV, notes[0].ContentCode.Value);
        Assert.AreEqual(TZUGFeRDContentCodes.WEV, notes[1].ContentCode.Value);
        Assert.AreEqual(TZUGFeRDContentCodes.ST1, notes[2].ContentCode.Value);
        Assert.AreEqual(TZUGFeRDContentCodes.ST2, notes[3].ContentCode.Value);
        Assert.AreEqual(TZUGFeRDContentCodes.ST3, notes[4].ContentCode.Value);
        Assert.AreEqual(TZUGFeRDContentCodes.VEV, notes[5].ContentCode.Value);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestNoteSubjectCodes(_version: Integer; _profile: Integer);
var
  version: TZUGFeRDVersion;
  profile: TZUGFeRDProfile;
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  notes: TObjectList<TZUGFeRDNote>;
begin
  version := TZUGFeRDVersion(_version);
  profile := TZUGFeRDProfile(_profile);

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.Notes.Clear;
    desc.AddNote('ACB', TZUGFeRDNullableParam<TZUGFeRDSubjectCodes>.Create(TZUGFeRDSubjectCodes.ACB));
    desc.AddNote('AAI', TZUGFeRDNullableParam<TZUGFeRDSubjectCodes>.Create(TZUGFeRDSubjectCodes.AAI));
    desc.AddNote('PRF', TZUGFeRDNullableParam<TZUGFeRDSubjectCodes>.Create(TZUGFeRDSubjectCodes.PRF));
    desc.AddNote('REG', TZUGFeRDNullableParam<TZUGFeRDSubjectCodes>.Create(TZUGFeRDSubjectCodes.REG));
    desc.AddNote('SUR', TZUGFeRDNullableParam<TZUGFeRDSubjectCodes>.Create(TZUGFeRDSubjectCodes.SUR));
    desc.AddNote('TXD', TZUGFeRDNullableParam<TZUGFeRDSubjectCodes>.Create(TZUGFeRDSubjectCodes.TXD));

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, version, profile);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        notes := loadedInvoice.Notes;
        Assert.AreEqual(TZUGFeRDSubjectCodes.ACB, notes[0].SubjectCode.Value);
        Assert.AreEqual(TZUGFeRDSubjectCodes.AAI, notes[1].SubjectCode.Value);
        Assert.AreEqual(TZUGFeRDSubjectCodes.PRF, notes[2].SubjectCode.Value);
        Assert.AreEqual(TZUGFeRDSubjectCodes.REG, notes[3].SubjectCode.Value);
        Assert.AreEqual(TZUGFeRDSubjectCodes.SUR, notes[4].SubjectCode.Value);
        Assert.AreEqual(TZUGFeRDSubjectCodes.TXD, notes[5].SubjectCode.Value);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestKosovoCountryCode(_version: Integer; _profile: Integer);
var
  version: TZUGFeRDVersion;
  profile: TZUGFeRDProfile;
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  version := TZUGFeRDVersion(_version);
  profile := TZUGFeRDProfile(_profile);

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.Seller := TZUGFeRDParty.Create;
    desc.Seller.Country := TZUGFeRDCountryCodes._1A;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, version, profile);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual(TZUGFeRDCountryCodes._1A, loadedInvoice.Seller.Country.Value);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestStandardCountryCode(_version: Integer; _profile: Integer);
var
  version: TZUGFeRDVersion;
  profile: TZUGFeRDProfile;
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  version := TZUGFeRDVersion(_version);
  profile := TZUGFeRDProfile(_profile);

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.Seller := TZUGFeRDParty.Create;
    desc.Seller.Country := TZUGFeRDCountryCodes.US;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, version, profile);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual(TZUGFeRDCountryCodes.US, loadedInvoice.Seller.Country.Value);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestManualLineIds;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.TradeLineItems.Clear;
    desc._AddTradeLineItem('item-01', 'Item1', TZUGFeRDNullableParam<Currency>.Create(0), '',
      TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62));
    desc._AddTradeLineItem('item-02', 'Item2', TZUGFeRDNullableParam<Currency>.Create(0), '',
      TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62));

    Assert.AreEqual('item-01', desc.TradeLineItems[0].AssociatedDocument.LineID);
    Assert.AreEqual('item-02', desc.TradeLineItems[1].AssociatedDocument.LineID);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestCommentLine;
var
  expectedComment: string;
  expectedCustomLineId: string;
  desc: TZUGFeRDInvoiceDescriptor;
  numberOfTradeLineItems: Integer;
  lastIdx: Integer;
begin
  expectedComment := TGUID.NewGuid.ToString;
  expectedCustomLineId := TGUID.NewGuid.ToString;

  // test with automatic line id
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    numberOfTradeLineItems := desc.TradeLineItems.Count;
    desc.AddTradeLineCommentItem(expectedComment);

    Assert.AreEqual<NativeInt>(numberOfTradeLineItems + 1, desc.TradeLineItems.Count);
    lastIdx := desc.TradeLineItems.Count - 1;
    Assert.IsNotNull(desc.TradeLineItems[lastIdx].AssociatedDocument);
    Assert.IsNotNull(desc.TradeLineItems[lastIdx].AssociatedDocument.Notes);
    Assert.AreEqual<NativeInt>(1, desc.TradeLineItems[lastIdx].AssociatedDocument.Notes.Count);
    Assert.AreEqual(expectedComment, desc.TradeLineItems[lastIdx].AssociatedDocument.Notes[0].Content);
  finally
    desc.Free;
  end;

  // test with manual line id
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    numberOfTradeLineItems := desc.TradeLineItems.Count;
    desc.AddTradeLineCommentItem(expectedCustomLineId, expectedComment, '', '');

    Assert.AreEqual<NativeInt>(numberOfTradeLineItems + 1, desc.TradeLineItems.Count);
    lastIdx := desc.TradeLineItems.Count - 1;
    Assert.IsNotNull(desc.TradeLineItems[lastIdx].AssociatedDocument);
    Assert.IsNotNull(desc.TradeLineItems[lastIdx].AssociatedDocument.LineID);
    Assert.IsNotNull(desc.TradeLineItems[lastIdx].AssociatedDocument.Notes);
    Assert.AreEqual<NativeInt>(1, desc.TradeLineItems[lastIdx].AssociatedDocument.Notes.Count);
    Assert.AreEqual(expectedComment, desc.TradeLineItems[lastIdx].AssociatedDocument.Notes[0].Content);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestGetVersion;
begin
  Assert.AreEqual(TZUGFeRDVersion.Version1,
    TZUGFeRDInvoiceDescriptor.GetVersion(DemodataPath('zugferd10\ZUGFeRD_1p0_COMFORT_Einfach.xml')));

  Assert.AreEqual(TZUGFeRDVersion.Version20,
    TZUGFeRDInvoiceDescriptor.GetVersion(DemodataPath('zugferd20\zugferd_2p0_BASIC_Einfach.xml')));

  Assert.AreEqual(TZUGFeRDVersion.Version23,
    TZUGFeRDInvoiceDescriptor.GetVersion(DemodataPath('zugferd21\zugferd_2p1_BASIC_Einfach-factur-x.xml')));

  Assert.AreEqual(TZUGFeRDVersion.Version23,
    TZUGFeRDInvoiceDescriptor.GetVersion(DemodataPath('xRechnung\ubl-cn-br-de-17-test-557-code-326.xml')));
end;

procedure TZUGFeRDCrossVersionTests.UBLNonAvailability(_version: Integer; _profile: Integer);
var
  version: TZUGFeRDVersion;
  profile: TZUGFeRDProfile;
  desc: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  version := TZUGFeRDVersion(_version);
  profile := TZUGFeRDProfile(_profile);

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    ms := TMemoryStream.Create;
    try
      Assert.WillRaise(
        procedure begin desc.Save(ms, version, profile, TZUGFeRDFormats.UBL) end,
        TZUGFeRDUnsupportedException);
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.UBLAvailability;
var
  desc: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.SavingThenReadingAppliedTradeTaxes(_version: Integer; _profile: Integer);
var
  version: TZUGFeRDVersion;
  profile: TZUGFeRDProfile;
  expected, actual_: TZUGFeRDInvoiceDescriptor;
  lineItem: TZUGFeRDTradeLineItem;
  ms: TMemoryStream;
  actualTax: TZUGFeRDTax;
begin
  version := TZUGFeRDVersion(_version);
  profile := TZUGFeRDProfile(_profile);

  expected := TZUGFeRDInvoiceDescriptor.CreateInvoice('123', EncodeDate(2024, 12, 5), TZUGFeRDCurrencyCodes.EUR);
  try
    lineItem := expected.AddTradeLineItem(
      {name=}            'Something',
      {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(9.9),
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62),
      {unitQuantity=}    nil,
      {grossUnitPrice=}  TZUGFeRDNullableParam<Currency>.Create(9.9),
      {billedQuantity=}  20,
      {lineTotalAmount=} 0,
      {taxType=}         TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
      {categoryCode=}    TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
      {taxPercent=}      19
    );
    lineItem.LineTotalAmount := 198;

    expected.AddApplicableTradeTax(
      {calculatedAmount=}           29.82,
      {basisAmount=}                198,
      {percent=}                    19,
      {typeCode=}                   TZUGFeRDTaxTypes.VAT,
      {categoryCode=}               TZUGFeRDTaxCategoryCodes.S,
      {allowanceChargeBasisAmount=} TZUGFeRDNullableParam<Currency>.Create(-5),
      {exemptionReasonCode=}        nil,
      {exemptionReason=}            '',
      {lineTotalBasisAmount=}       TZUGFeRDNullableParam<Currency>.Create(198)
    );
    expected.LineTotalAmount := 198;
    expected.TaxBasisAmount := 198;
    expected.TaxTotalAmount := 29.82;
    expected.GrandTotalAmount := 198 + 29.82;
    expected.DuePayableAmount := expected.GrandTotalAmount.Value;

    ms := TMemoryStream.Create;
    try
      expected.Save(ms, version, profile);
      ms.Position := 0;

      actual_ := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual(expected.Taxes.Count, actual_.Taxes.Count);
        Assert.AreEqual<NativeInt>(1, actual_.Taxes.Count);
        actualTax := actual_.Taxes[0];
        Assert.AreEqual<Currency>(198, actualTax.BasisAmount);
        Assert.AreEqual<Currency>(19, actualTax.Percent);
        Assert.AreEqual<Currency>(29.82, actualTax.TaxAmount);
        Assert.AreEqual<TZUGFeRDTaxTypes>(TZUGFeRDTaxTypes.VAT, actualTax.TypeCode.Value);
        Assert.AreEqual<TZUGFeRDTaxCategoryCodes>(TZUGFeRDTaxCategoryCodes.S, actualTax.CategoryCode.Value);
        Assert.AreEqual<Currency>(-5, actualTax.AllowanceChargeBasisAmount.Value);
        Assert.AreEqual<Currency>(198, actualTax.LineTotalBasisAmount.Value);
      finally
        actual_.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    expected.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestDeliveryNoteReferencedDocumentLineIdInExtended(_version: Integer);
var
  version: TZUGFeRDVersion;
  deliveryNoteNumber: string;
  deliveryNoteDate: TDateTime;
  deliveryNoteLineID: string;
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  line: TZUGFeRDTradeLineItem;
  loadedLine: TZUGFeRDTradeLineItem;
  ms: TMemoryStream;
begin
  version := TZUGFeRDVersion(_version);

  deliveryNoteNumber := 'DeliveryNote-0815';
  deliveryNoteDate := Date;
  deliveryNoteLineID := '0815.001';

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    line := desc.AddTradeLineItem('DeliveryNoteReferencedDocument-Text',
      TZUGFeRDNullableParam<Currency>.Create(0), '',
      TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62));
    line.SetDeliveryNoteReferencedDocument(deliveryNoteNumber,
      TZUGFeRDNullableParam<TDateTime>.Create(deliveryNoteDate), deliveryNoteLineID);

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, version, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        loadedLine := loadedInvoice.TradeLineItems[loadedInvoice.TradeLineItems.Count - 1];

        Assert.IsNotNull(loadedLine);
        Assert.IsNotNull(loadedLine.DeliveryNoteReferencedDocument);
        Assert.AreEqual(deliveryNoteNumber, loadedLine.DeliveryNoteReferencedDocument.ID);
        Assert.AreEqual(deliveryNoteDate, loadedLine.DeliveryNoteReferencedDocument.IssueDateTime.Value);
        Assert.AreEqual(deliveryNoteLineID, loadedLine.DeliveryNoteReferencedDocument.LineID);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestContractReferencedDocumentLineIdInExtended(_version: Integer);
var
  version: TZUGFeRDVersion;
  contractNumber: string;
  contractDate: TDateTime;
  contractLineID: string;
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  line: TZUGFeRDTradeLineItem;
  loadedLine: TZUGFeRDTradeLineItem;
  ms: TMemoryStream;
begin
  version := TZUGFeRDVersion(_version);

  contractNumber := 'Contract-0815';
  contractDate := Date;
  contractLineID := '0815.001';

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    line := desc.AddTradeLineItem('ContractReferencedDocument-Text',
      TZUGFeRDNullableParam<Currency>.Create(0), '',
      TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62));
    line.SetContractReferencedDocument(contractNumber,
      TZUGFeRDNullableParam<TDateTime>.Create(contractDate), contractLineID);

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, version, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        loadedLine := loadedInvoice.TradeLineItems[loadedInvoice.TradeLineItems.Count - 1];

        Assert.IsNotNull(loadedLine);
        Assert.IsNotNull(loadedLine.ContractReferencedDocument);
        Assert.AreEqual(contractNumber, loadedLine.ContractReferencedDocument.ID);
        Assert.AreEqual(contractDate, loadedLine.ContractReferencedDocument.IssueDateTime.Value);
        Assert.AreEqual(contractLineID, loadedLine.ContractReferencedDocument.LineID);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestLongerDecimalPlacesForNetUnitPrice(_version: Integer);
var
  version: TZUGFeRDVersion;
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  items: TObjectList<TZUGFeRDTradeLineItem>;
  ms: TMemoryStream;
begin
  version := TZUGFeRDVersion(_version);

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.AddTradeLineItem('Item with 2 decimal places',
      TZUGFeRDNullableParam<Currency>.Create(123.45), '',
      TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62));
    desc.AddTradeLineItem('Item with 3 decimal places',
      TZUGFeRDNullableParam<Currency>.Create(123.456), '',
      TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62));
    desc.AddTradeLineItem('Item with 4 decimal places',
      TZUGFeRDNullableParam<Currency>.Create(123.4567), '',
      TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62));
    desc.AddTradeLineItem('Item with 5 decimal places',
      TZUGFeRDNullableParam<Currency>.Create(123.4568), '',  // Currency rounds 123.45678 to 123.4568
      TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62));

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, version, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        items := loadedInvoice.TradeLineItems;

        Assert.AreEqual<Currency>(123.45,   items[items.Count - 4].NetUnitPrice.Value);
        Assert.AreEqual<Currency>(123.456,  items[items.Count - 3].NetUnitPrice.Value);
        Assert.AreEqual<Currency>(123.4567, items[items.Count - 2].NetUnitPrice.Value);
        Assert.AreEqual<Currency>(123.4568, items[items.Count - 1].NetUnitPrice.Value);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestLongerDecimalPlacesForGrossUnitPrice(_version: Integer);
var
  version: TZUGFeRDVersion;
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  items: TObjectList<TZUGFeRDTradeLineItem>;
  ms: TMemoryStream;
begin
  version := TZUGFeRDVersion(_version);

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.AddTradeLineItem('Item with 2 decimal places',
      TZUGFeRDNullableParam<Currency>.Create(123.45), '',
      TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62),
      nil, TZUGFeRDNullableParam<Currency>.Create(123.45));
    desc.AddTradeLineItem('Item with 2 decimal places',
      TZUGFeRDNullableParam<Currency>.Create(123.456), '',
      TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62),
      nil, TZUGFeRDNullableParam<Currency>.Create(123.456));
    desc.AddTradeLineItem('Item with 2 decimal places',
      TZUGFeRDNullableParam<Currency>.Create(123.4567), '',
      TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62),
      nil, TZUGFeRDNullableParam<Currency>.Create(123.4567));
    desc.AddTradeLineItem('Item with 2 decimal places',
      TZUGFeRDNullableParam<Currency>.Create(123.4568), '',  // Currency rounds 123.45678 to 123.4568
      TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62),
      nil, TZUGFeRDNullableParam<Currency>.Create(123.4568)); // same rounding

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, version, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        items := loadedInvoice.TradeLineItems;

        Assert.AreEqual<Currency>(123.45,   items[items.Count - 4].GrossUnitPrice.Value);
        Assert.AreEqual<Currency>(123.456,  items[items.Count - 3].GrossUnitPrice.Value);
        Assert.AreEqual<Currency>(123.4567, items[items.Count - 2].GrossUnitPrice.Value);
        Assert.AreEqual<Currency>(123.4568, items[items.Count - 1].GrossUnitPrice.Value);

        Assert.AreEqual<Currency>(123.45,   items[items.Count - 4].NetUnitPrice.Value);
        Assert.AreEqual<Currency>(123.456,  items[items.Count - 3].NetUnitPrice.Value);
        Assert.AreEqual<Currency>(123.4567, items[items.Count - 2].NetUnitPrice.Value);
        Assert.AreEqual<Currency>(123.4568, items[items.Count - 1].NetUnitPrice.Value);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestSellerTaxRepresentative(_version: Integer; _profile: Integer; _format: Integer);
var
  version: TZUGFeRDVersion;
  profile: TZUGFeRDProfile;
  format: TZUGFeRDFormats;
  name: string;
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  version := TZUGFeRDVersion(_version);
  profile := TZUGFeRDProfile(_profile);
  format := TZUGFeRDFormats(_format);

  name := TGUID.NewGuid.ToString;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.SellerTaxRepresentative := TZUGFeRDParty.Create;
    desc.SellerTaxRepresentative.Name := name;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, version, profile, format);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice.SellerTaxRepresentative);
        Assert.AreEqual(name, loadedInvoice.SellerTaxRepresentative.Name);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestSellerTaxRepresentativeInNonSupportedVersions(_version: Integer; _format: Integer);
var
  version: TZUGFeRDVersion;
  format: TZUGFeRDFormats;
  name: string;
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  version := TZUGFeRDVersion(_version);
  format := TZUGFeRDFormats(_format);

  name := TGUID.NewGuid.ToString;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.SellerTaxRepresentative := TZUGFeRDParty.Create;
    desc.SellerTaxRepresentative.Name := name;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, version, TZUGFeRDProfile.Extended, format);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNull(loadedInvoice.SellerTaxRepresentative);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestTransportModeWithExtended(_version: Integer);
var
  version: TZUGFeRDVersion;
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  version := TZUGFeRDVersion(_version);

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.TransportMode := TZUGFeRDTransportModeCodes.Road;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, version, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual(TZUGFeRDTransportModeCodes.Road, loadedInvoice.TransportMode.Value);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestTransportModeWithComfort(_version: Integer);
var
  version: TZUGFeRDVersion;
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  version := TZUGFeRDVersion(_version);

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.TransportMode := TZUGFeRDTransportModeCodes.Road;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, version, TZUGFeRDProfile.Comfort);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsFalse(loadedInvoice.TransportMode.HasValue); // not supported in comfort profile
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestGrossPriceRepresentationForXRechnungAndNotXRechnungNegativeCase(_version: Integer);
var
  grossPrice: Currency;
  netPrice: Currency;
  desc: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  loadedExtended, loadedXRechnung: TZUGFeRDInvoiceDescriptor;
begin
  grossPrice := 10.1;
  netPrice := 10.0;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.TradeLineItems.Clear;

    desc.AddTradeLineItem('Test',
      TZUGFeRDNullableParam<Currency>.Create(netPrice),
      'Test',
      TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62),
      nil,
      TZUGFeRDNullableParam<Currency>.Create(grossPrice),
      1);

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);

      loadedExtended := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<NativeInt>(1, loadedExtended.TradeLineItems.Count);
        Assert.AreEqual(grossPrice, loadedExtended.TradeLineItems[0].GrossUnitPrice.Value);
        Assert.AreEqual<NativeInt>(0, loadedExtended.TradeLineItems[0].SpecifiedTradeAllowanceCharges.Count);
      finally
        loadedExtended.Free;
      end;
    finally
      ms.Free;
    end;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung);

      loadedXRechnung := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<NativeInt>(1, loadedXRechnung.TradeLineItems.Count);
        Assert.IsFalse(loadedXRechnung.TradeLineItems[0].GrossUnitPrice.HasValue);
        Assert.AreEqual<NativeInt>(0, loadedXRechnung.TradeLineItems[0].SpecifiedTradeAllowanceCharges.Count);
      finally
        loadedXRechnung.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestGrossPriceRepresentationForXRechnungAndNotXRechnungPositiveCase(_version: Integer);
var
  grossPrice: Currency;
  netPrice: Currency;
  discountAmount: Currency;
  desc: TZUGFeRDInvoiceDescriptor;
  item: TZUGFeRDTradeLineItem;
  ms: TMemoryStream;
  loadedExtended, loadedXRechnung: TZUGFeRDInvoiceDescriptor;
begin
  grossPrice := 10.1;
  netPrice := 10.0;
  discountAmount := 0.1;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.TradeLineItems.Clear;

    item := desc.AddTradeLineItem('Test',
      TZUGFeRDNullableParam<Currency>.Create(netPrice),
      'Test',
      TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62),
      nil,
      TZUGFeRDNullableParam<Currency>.Create(grossPrice),
      1);

    item.AddTradeAllowance(TZUGFeRDCurrencyCodes.EUR, grossPrice, discountAmount,
      'Discount', TZUGFeRDNullableParam<TZUGFeRDAllowanceReasonCodes>.Create(TZUGFeRDAllowanceReasonCodes.Discount));

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);

      loadedExtended := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<NativeInt>(1, loadedExtended.TradeLineItems.Count);
        Assert.AreEqual(grossPrice, loadedExtended.TradeLineItems[0].GrossUnitPrice.Value);
        Assert.AreEqual<NativeInt>(1, loadedExtended.TradeLineItems[0].TradeAllowanceCharges.Count);
        Assert.AreEqual(grossPrice, loadedExtended.TradeLineItems[0].TradeAllowanceCharges[0].BasisAmount.Value);
        Assert.AreEqual(discountAmount, loadedExtended.TradeLineItems[0].TradeAllowanceCharges[0].ActualAmount);
      finally
        loadedExtended.Free;
      end;
    finally
      ms.Free;
    end;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung);

      loadedXRechnung := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<NativeInt>(1, loadedXRechnung.TradeLineItems.Count);
        Assert.AreEqual(grossPrice, loadedXRechnung.TradeLineItems[0].GrossUnitPrice.Value);
        Assert.AreEqual<NativeInt>(1, loadedXRechnung.TradeLineItems[0].TradeAllowanceCharges.Count);
        Assert.IsFalse(loadedXRechnung.TradeLineItems[0].TradeAllowanceCharges[0].BasisAmount.HasValue); // not written in XRechnung
        Assert.AreEqual(discountAmount, loadedXRechnung.TradeLineItems[0].TradeAllowanceCharges[0].ActualAmount);
      finally
        loadedXRechnung.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestHeaderComment(_version: Integer; _format: Integer; _profile: Integer);
var
  version: TZUGFeRDVersion;
  format: TZUGFeRDFormats;
  profile: TZUGFeRDProfile;
  desc: TZUGFeRDInvoiceDescriptor;
  headerComment: string;
  ms: TMemoryStream;
  content: string;
  options: TZUGFeRDInvoiceFormatOptions;
  bytes: TBytes;
begin
  version := TZUGFeRDVersion(_version);
  format := TZUGFeRDFormats(_format);
  profile := TZUGFeRDProfile(_profile);

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    headerComment := TGUID.NewGuid.ToString;
    ms := TMemoryStream.Create;
    try
      options := TZUGFeRDInvoiceFormatOptions.Create;
      try
        options.XmlHeaderComments.Add(headerComment);
        options.IncludeXmlComments := True;
        desc.Save(ms, version, profile, format, options);
      finally
        options.Free;
      end;

      SetLength(bytes, ms.Size);
      ms.Position := 0;
      ms.ReadBuffer(bytes[0], ms.Size);
      content := TEncoding.UTF8.GetString(bytes);

      Assert.Contains(content, headerComment);
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestWithoutHeaderComment(_version: Integer; _format: Integer; _profile: Integer);
var
  version: TZUGFeRDVersion;
  format: TZUGFeRDFormats;
  profile: TZUGFeRDProfile;
  desc: TZUGFeRDInvoiceDescriptor;
  headerComment: string;
  ms: TMemoryStream;
  content: string;
  options: TZUGFeRDInvoiceFormatOptions;
  bytes: TBytes;
begin
  version := TZUGFeRDVersion(_version);
  format := TZUGFeRDFormats(_format);
  profile := TZUGFeRDProfile(_profile);

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    headerComment := TGUID.NewGuid.ToString;
    ms := TMemoryStream.Create;
    try
      options := TZUGFeRDInvoiceFormatOptions.Create;
      try
        options.XmlHeaderComments.Add(headerComment);
        options.IncludeXmlComments := False;
        desc.Save(ms, version, profile, format, options);
      finally
        options.Free;
      end;

      SetLength(bytes, ms.Size);
      ms.Position := 0;
      ms.ReadBuffer(bytes[0], ms.Size);
      content := TEncoding.UTF8.GetString(bytes);

      Assert.DoesNotContain(content, headerComment);
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestZUGFeRDElementComments(_version: Integer; _format: Integer; _profile: Integer);
var
  version: TZUGFeRDVersion;
  format: TZUGFeRDFormats;
  profile: TZUGFeRDProfile;
  desc: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  options: TZUGFeRDInvoiceFormatOptions;
  bytes: TBytes;
  content: string;
  lines: TArray<string>;
  i: Integer;
  onItemLevel: Boolean;
begin
  version := TZUGFeRDVersion(_version);
  format := TZUGFeRDFormats(_format);
  profile := TZUGFeRDProfile(_profile);

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    ms := TMemoryStream.Create;
    try
      options := TZUGFeRDInvoiceFormatOptions.Create;
      try
        options.IncludeXmlComments := True;
        desc.Save(ms, version, profile, format, options);
      finally
        options.Free;
      end;

      SetLength(bytes, ms.Size);
      ms.Position := 0;
      ms.ReadBuffer(bytes[0], ms.Size);
      content := TEncoding.UTF8.GetString(bytes);
      lines := content.Split([sLineBreak], TStringSplitOptions.ExcludeEmpty);

      onItemLevel := False;
      for i := 1 to High(lines) do
      begin
        if lines[i].Contains('<ram:IncludedSupplyChainTradeLineItem>') then
        begin
          onItemLevel := True;
          Assert.Contains(lines[i - 1], '<!--');
        end
        else if lines[i].Contains('</ram:IncludedSupplyChainTradeLineItem>') then
          onItemLevel := False;

        if lines[i].Contains('<ram:NetPriceProductTradePrice>') then
          Assert.Contains(lines[i - 1], '<!--');

        // totals per item
        if version = TZUGFeRDVersion.Version1 then
        begin
          if lines[i].Contains('<ram:SpecifiedTradeSettlementMonetarySummation>') then
            Assert.Contains(lines[i - 1], '<!--');
        end
        else
        begin
          if lines[i].Contains('<ram:SpecifiedTradeSettlementLineMonetarySummation>') then
            Assert.Contains(lines[i - 1], '<!--');
        end;

        // totals on header level
        if version = TZUGFeRDVersion.Version1 then
        begin
          if lines[i].Contains('<ram:SpecifiedTradeSettlementMonetarySummation>') then
            Assert.Contains(lines[i - 1], '<!--');
        end
        else
        begin
          if lines[i].Contains('<ram:SpecifiedTradeSettlementHeaderMonetarySummation>') then
            Assert.Contains(lines[i - 1], '<!--');
        end;

        // header trade agreement
        if lines[i].Contains('<ram:ApplicableHeaderTradeAgreement>') then
          Assert.Contains(lines[i - 1], '<!--');

        // buyer reference
        if lines[i].Contains('<ram:BuyerReference>') then
          Assert.Contains(lines[i - 1], '<!--');

        // seller
        if lines[i].Contains('<ram:SellerTradeParty>') then
          Assert.Contains(lines[i - 1], '<!--');

        // buyer
        if lines[i].Contains('<ram:BuyerTradeParty>') then
          Assert.Contains(lines[i - 1], '<!--');

        // buyer order information
        if lines[i].Contains('<ram:BuyerOrderReferencedDocument>') then
          Assert.Contains(lines[i - 1], '<!--');

        // delivery information
        if lines[i].Contains('<ram:ApplicableHeaderTradeDelivery>') then
          Assert.Contains(lines[i - 1], '<!--');

        // delivery note information
        if lines[i].Contains('<ram:DespatchAdviceReferencedDocument>') then
          Assert.Contains(lines[i - 1], '<!--');

        // document information
        if lines[i].Contains('<ram:ApplicableHeaderTradeSettlement>') then
          Assert.Contains(lines[i - 1], '<!--');

        // payment means
        if lines[i].Contains('<ram:SpecifiedTradeSettlementPaymentMeans>') then
          Assert.Contains(lines[i - 1], '<!--');

        // tax
        if (not onItemLevel) and lines[i].Contains('<ram:ApplicableTradeTax>') then
          Assert.Contains(lines[i - 1], '<!--');
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestXRechnungElementComments(_version: Integer; _format: Integer; _profile: Integer);
var
  version: TZUGFeRDVersion;
  format: TZUGFeRDFormats;
  profile: TZUGFeRDProfile;
  desc: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  options: TZUGFeRDInvoiceFormatOptions;
  bytes: TBytes;
  content: string;
  lines: TArray<string>;
  i: Integer;
begin
  version := TZUGFeRDVersion(_version);
  format := TZUGFeRDFormats(_format);
  profile := TZUGFeRDProfile(_profile);

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    ms := TMemoryStream.Create;
    try
      options := TZUGFeRDInvoiceFormatOptions.Create;
      try
        options.IncludeXmlComments := True;
        desc.Save(ms, version, profile, format, options);
      finally
        options.Free;
      end;

      SetLength(bytes, ms.Size);
      ms.Position := 0;
      ms.ReadBuffer(bytes[0], ms.Size);
      content := TEncoding.UTF8.GetString(bytes);
      lines := content.Split([sLineBreak], TStringSplitOptions.ExcludeEmpty);

      for i := 1 to High(lines) do
      begin
        // invoice line
        if lines[i].Contains('<cac:InvoiceLine>') then
          Assert.Contains(lines[i - 1], '<!--');

        // item price
        if lines[i].Contains('<cbc:PriceAmount>') then
          Assert.Contains(lines[i - 1], '<!--');

        // totals on item level
        if lines[i].Contains('<cbc:LineExtensionAmount>') then
          Assert.Contains(lines[i - 1], '<!--');

        // totals on header level
        if lines[i].Contains('<cac:LegalMonetaryTotal>') then
          Assert.Contains(lines[i - 1], '<!--');

        // buyer
        if lines[i].Contains('<cac:AccountingSupplierParty>') then
          Assert.Contains(lines[i - 1], '<!--');

        // seller
        if lines[i].Contains('<cac:AccountingSupplierParty>') then
          Assert.Contains(lines[i - 1], '<!--');

        // buyer, seller information etc.
        if lines[i].Contains('<cac:OrderReference>') then
          Assert.Contains(lines[i - 1], '<!--');

        // delivery note information
        if lines[i].Contains('<cac:DespatchDocumentReference>') then
          Assert.Contains(lines[i - 1], '<!--');

        // payment means
        if lines[i].Contains('<cac:PaymentMeans>') then
          Assert.Contains(lines[i - 1], '<!--');

        // tax information
        if lines[i].Contains('<cac:TaxSubtotal>') then
          Assert.Contains(lines[i - 1], '<!--');
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestInvalidXmlWithException(_version: Integer; _format: Integer; _profile: Integer);
var
  version: TZUGFeRDVersion;
  format: TZUGFeRDFormats;
  profile: TZUGFeRDProfile;
  desc: TZUGFeRDInvoiceDescriptor;
  invoiceStream: TMemoryStream;
begin
  version := TZUGFeRDVersion(_version);
  format := TZUGFeRDFormats(_format);
  profile := TZUGFeRDProfile(_profile);

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.InvoiceNo := #$001B;
    invoiceStream := TMemoryStream.Create;
    try
      Assert.WillRaiseAny(
        procedure begin desc.Save(invoiceStream, version, profile, format) end);
    finally
      invoiceStream.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestInvalidXmlWithCleaning(_version: Integer; _format: Integer; _profile: Integer);
var
  version: TZUGFeRDVersion;
  format: TZUGFeRDFormats;
  profile: TZUGFeRDProfile;
  desc: TZUGFeRDInvoiceDescriptor;
  options: TZUGFeRDInvoiceFormatOptions;
  invoiceStream: TMemoryStream;
  bytes: TBytes;
  resultStr: string;
begin
  version := TZUGFeRDVersion(_version);
  format := TZUGFeRDFormats(_format);
  profile := TZUGFeRDProfile(_profile);

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.InvoiceNo := 'ABC' + #$001B + 'DEF';

    options := TZUGFeRDInvoiceFormatOptions.Create;
    try
      options.AutomaticallyCleanInvalidCharacters := True;

      invoiceStream := TMemoryStream.Create;
      try
        desc.Save(invoiceStream, version, profile, format, options);

        SetLength(bytes, invoiceStream.Size);
        invoiceStream.Position := 0;
        invoiceStream.ReadBuffer(bytes[0], invoiceStream.Size);
        resultStr := TEncoding.UTF8.GetString(bytes);

        Assert.Contains(resultStr, 'ABCDEF', 'The illegal character should be removed from the invoice number.');
      finally
        invoiceStream.Free;
      end;
    finally
      options.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestValidXml(_version: Integer; _format: Integer; _profile: Integer);
var
  version: TZUGFeRDVersion;
  format: TZUGFeRDFormats;
  profile: TZUGFeRDProfile;
  desc: TZUGFeRDInvoiceDescriptor;
  options: TZUGFeRDInvoiceFormatOptions;
  invoiceStream: TMemoryStream;
  bytes: TBytes;
  resultStr: string;
begin
  version := TZUGFeRDVersion(_version);
  format := TZUGFeRDFormats(_format);
  profile := TZUGFeRDProfile(_profile);

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.InvoiceNo := #$D83D#$DE00; // U+1F600 smiley emoji as surrogate pair

    options := TZUGFeRDInvoiceFormatOptions.Create;
    try
      options.AutomaticallyCleanInvalidCharacters := True;

      invoiceStream := TMemoryStream.Create;
      try
        desc.Save(invoiceStream, version, profile, format, options);

        SetLength(bytes, invoiceStream.Size);
        invoiceStream.Position := 0;
        invoiceStream.ReadBuffer(bytes[0], invoiceStream.Size);
        resultStr := TEncoding.UTF8.GetString(bytes);

        Assert.Contains(resultStr, #$D83D#$DE00);
      finally
        invoiceStream.Free;
      end;
    finally
      options.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestGrossQuantity(_version: Integer; _format: Integer; _profile: Integer);
var
  version: TZUGFeRDVersion;
  format: TZUGFeRDFormats;
  profile: TZUGFeRDProfile;
  desiredNetUnitQuantity: Currency;
  desiredGrossUnitQuantity: Currency;
  desc, loadedDescriptor: TZUGFeRDInvoiceDescriptor;
  item: TZUGFeRDTradeLineItem;
  ms: TMemoryStream;
begin
  version := TZUGFeRDVersion(_version);
  format := TZUGFeRDFormats(_format);
  profile := TZUGFeRDProfile(_profile);

  desiredNetUnitQuantity := 20.0;
  desiredGrossUnitQuantity := 23.0;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    for item in desc.TradeLineItems do
    begin
      item.NetQuantity := desiredNetUnitQuantity;
      item.GrossQuantity := desiredGrossUnitQuantity;
    end;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, version, profile, format);

      loadedDescriptor := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        for item in loadedDescriptor.TradeLineItems do
        begin
          Assert.IsTrue(item.NetQuantity.HasValue);
          Assert.AreEqual<Currency>(desiredNetUnitQuantity, item.NetQuantity.Value);

          Assert.IsTrue(item.GrossQuantity.HasValue);
          Assert.AreEqual<Currency>(desiredGrossUnitQuantity, item.GrossQuantity.Value);
        end;
      finally
        loadedDescriptor.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestGrossQuantityForXRechnung(_version: Integer; _format: Integer; _profile: Integer);
var
  version: TZUGFeRDVersion;
  format: TZUGFeRDFormats;
  profile: TZUGFeRDProfile;
  desiredNetUnitQuantity: Currency;
  desiredGrossUnitQuantity: Currency;
  desc, loadedDescriptor: TZUGFeRDInvoiceDescriptor;
  item: TZUGFeRDTradeLineItem;
  ms: TMemoryStream;
begin
  version := TZUGFeRDVersion(_version);
  format := TZUGFeRDFormats(_format);
  profile := TZUGFeRDProfile(_profile);

  desiredNetUnitQuantity := 20.0;
  desiredGrossUnitQuantity := 23.0;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    for item in desc.TradeLineItems do
    begin
      item.NetQuantity := desiredNetUnitQuantity;
      item.GrossQuantity := desiredGrossUnitQuantity;
    end;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, version, profile, format);

      loadedDescriptor := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        for item in loadedDescriptor.TradeLineItems do
        begin
          Assert.IsTrue(item.NetQuantity.HasValue);
          Assert.AreEqual<Currency>(desiredNetUnitQuantity, item.NetQuantity.Value);

          Assert.IsFalse(item.GrossQuantity.HasValue);
        end;
      finally
        loadedDescriptor.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestWithoutGrossQuantity(_version: Integer; _format: Integer; _profile: Integer);
var
  version: TZUGFeRDVersion;
  format: TZUGFeRDFormats;
  profile: TZUGFeRDProfile;
  desiredNetUnitQuantity: Currency;
  desc, loadedDescriptor: TZUGFeRDInvoiceDescriptor;
  item: TZUGFeRDTradeLineItem;
  ms: TMemoryStream;
begin
  version := TZUGFeRDVersion(_version);
  format := TZUGFeRDFormats(_format);
  profile := TZUGFeRDProfile(_profile);

  desiredNetUnitQuantity := 20.0;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    for item in desc.TradeLineItems do
    begin
      item.NetQuantity := desiredNetUnitQuantity;
      item.GrossQuantity := nil;
    end;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, version, profile, format);

      loadedDescriptor := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        for item in loadedDescriptor.TradeLineItems do
        begin
          Assert.IsTrue(item.NetQuantity.HasValue);
          Assert.AreEqual<Currency>(desiredNetUnitQuantity, item.NetQuantity.Value);

          Assert.IsFalse(item.GrossQuantity.HasValue);
        end;
      finally
        loadedDescriptor.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestNulledGlobalIDScheme(_version: Integer; _format: Integer; _profile: Integer);
var
  version: TZUGFeRDVersion;
  format: TZUGFeRDFormats;
  profile: TZUGFeRDProfile;
  desc, loadedDescriptor: TZUGFeRDInvoiceDescriptor;
  item: TZUGFeRDTradeLineItem;
  ms: TMemoryStream;
begin
  version := TZUGFeRDVersion(_version);
  format := TZUGFeRDFormats(_format);
  profile := TZUGFeRDProfile(_profile);

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.Seller.GlobalID := TZUGFeRDGlobalID.Create;
    desc.Seller.GlobalID.ID := '123';
    desc.Buyer.GlobalID := TZUGFeRDGlobalID.Create;
    desc.Buyer.GlobalID.ID := '213';

    for item in desc.TradeLineItems do
      item.GlobalID.SchemeID := nil;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, version, profile, format);

      loadedDescriptor := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsFalse(loadedDescriptor.Seller.GlobalID.SchemeID.HasValue);
        Assert.IsFalse(loadedDescriptor.Buyer.GlobalID.SchemeID.HasValue);

        for item in loadedDescriptor.TradeLineItems do
          Assert.IsFalse(item.GlobalID.SchemeID.HasValue);
      finally
        loadedDescriptor.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestInvoicerContactWriteAndRead(_version: Integer; _format: Integer; _profile: Integer);
var
  version: TZUGFeRDVersion;
  profile: TZUGFeRDProfile;
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  invoiceStream: TMemoryStream;
  bytes: TBytes;
  invoiceString: string;
begin
  version := TZUGFeRDVersion(_version);
  profile := TZUGFeRDProfile(_profile);

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.Invoicer := TZUGFeRDParty.Create;
    desc.Invoicer.Name := 'Invoicer Name';
    desc.Invoicer.Street := 'Stra'+#$00DF+'e 1';
    desc.Invoicer.Postcode := '01234';
    desc.Invoicer.City := 'Musterstadt';
    desc.Invoicer.Country := TZUGFeRDCountryCodes.DE;

    desc.InvoicerContact := TZUGFeRDContact.Create;
    desc.InvoicerContact.EmailAddress := 'invoiceremail@example.com';
    desc.InvoicerContact.FaxNo := '+49 12345';
    desc.InvoicerContact.PhoneNo := '+49 54321';

    invoiceStream := TMemoryStream.Create;
    try
      desc.Save(invoiceStream, version, profile);

      SetLength(bytes, invoiceStream.Size);
      invoiceStream.Position := 0;
      invoiceStream.ReadBuffer(bytes[0], invoiceStream.Size);
      invoiceString := TEncoding.UTF8.GetString(bytes);

      Assert.Contains(invoiceString, '<ram:CompleteNumber>+49 54321</ram:CompleteNumber>');
      Assert.Contains(invoiceString, '<ram:CompleteNumber>+49 12345</ram:CompleteNumber>');
      Assert.Contains(invoiceString, '<ram:URIID>invoiceremail@example.com</ram:URIID>');

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(invoiceStream);
      try
        Assert.AreEqual('invoiceremail@example.com', loadedInvoice.InvoicerContact.EmailAddress);
        Assert.AreEqual('+49 12345', loadedInvoice.InvoicerContact.FaxNo);
        Assert.AreEqual('+49 54321', loadedInvoice.InvoicerContact.PhoneNo);
      finally
        loadedInvoice.Free;
      end;
    finally
      invoiceStream.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestBillingPeriod(_version: Integer; _format: Integer; _profile: Integer);
var
  version: TZUGFeRDVersion;
  format: TZUGFeRDFormats;
  profile: TZUGFeRDProfile;
  billingPeriodStart, billingPeriodEnd: TDateTime;
  desc, loadedDesc: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  version := TZUGFeRDVersion(_version);
  format := TZUGFeRDFormats(_format);
  profile := TZUGFeRDProfile(_profile);

  billingPeriodStart := Date - 10;
  billingPeriodEnd := Date + 20;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.BillingPeriodStart := billingPeriodStart;
    desc.BillingPeriodEnd := billingPeriodEnd;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, version, profile, format);
      ms.Position := 0;

      loadedDesc := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual(billingPeriodStart, loadedDesc.BillingPeriodStart.Value);
        Assert.AreEqual(billingPeriodEnd, loadedDesc.BillingPeriodEnd.Value);
      finally
        loadedDesc.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestBillingPeriodOnItemLevel(_version: Integer; _format: Integer; _profile: Integer);
var
  version: TZUGFeRDVersion;
  format: TZUGFeRDFormats;
  profile: TZUGFeRDProfile;
  billingPeriodStart, billingPeriodEnd: TDateTime;
  desc, loadedDesc: TZUGFeRDInvoiceDescriptor;
  item: TZUGFeRDTradeLineItem;
  ms: TMemoryStream;
begin
  version := TZUGFeRDVersion(_version);
  format := TZUGFeRDFormats(_format);
  profile := TZUGFeRDProfile(_profile);

  billingPeriodStart := Date - 10;
  billingPeriodEnd := Date + 20;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    for item in desc.TradeLineItems do
    begin
      item.BillingPeriodStart := billingPeriodStart;
      item.BillingPeriodEnd := billingPeriodEnd;
    end;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, version, profile, format);
      ms.Position := 0;

      loadedDesc := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        for item in loadedDesc.TradeLineItems do
        begin
          Assert.AreEqual(billingPeriodStart, item.BillingPeriodStart.Value);
          Assert.AreEqual(billingPeriodEnd, item.BillingPeriodEnd.Value);
        end;
      finally
        loadedDesc.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestAvoidEmptyElementsWithDemoInvoice(_version: Integer; _format: Integer; _profile: Integer);
var
  version: TZUGFeRDVersion;
  format: TZUGFeRDFormats;
  profile: TZUGFeRDProfile;
  desc: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  bytes: TBytes;
  xmlContent: string;
  xmlDoc: IXMLDocument;
  emptyCount: Integer;

  procedure CountEmptyElements(const node: IXMLNode);
  var
    i: Integer;
    child: IXMLNode;
  begin
    for i := 0 to node.ChildNodes.Count - 1 do
    begin
      child := node.ChildNodes[i];
      if child.NodeType = ntElement then
      begin
        if (child.ChildNodes.Count = 0) and (child.AttributeNodes.Count = 0) and
           (child.Text = '') then
          Inc(emptyCount);
        CountEmptyElements(child);
      end;
    end;
  end;

begin
  version := TZUGFeRDVersion(_version);
  format := TZUGFeRDFormats(_format);
  profile := TZUGFeRDProfile(_profile);

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    ms := TMemoryStream.Create;
    try
      desc.Save(ms, version, profile, format);
      ms.Position := 0;

      SetLength(bytes, ms.Size);
      ms.ReadBuffer(bytes[0], ms.Size);
      xmlContent := TEncoding.UTF8.GetString(bytes);

      xmlDoc := TXMLDocument.Create(nil);
      xmlDoc.LoadFromXML(xmlContent);
      xmlDoc.Active := True;

      emptyCount := 0;
      CountEmptyElements(xmlDoc.DocumentElement);
      Assert.AreEqual(0, emptyCount, 'Found empty elements in the XML');
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDCrossVersionTests.TestAvoidEmptyElementsWithMinimalInvoice(_version: Integer; _format: Integer; _profile: Integer);
var
  version: TZUGFeRDVersion;
  format: TZUGFeRDFormats;
  profile: TZUGFeRDProfile;
  desc: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  bytes: TBytes;
  xmlContent: string;
  xmlDoc: IXMLDocument;
  emptyCount: Integer;

  procedure CountEmptyElements(const node: IXMLNode);
  var
    i: Integer;
    child: IXMLNode;
  begin
    for i := 0 to node.ChildNodes.Count - 1 do
    begin
      child := node.ChildNodes[i];
      if child.NodeType = ntElement then
      begin
        if (child.ChildNodes.Count = 0) and (child.AttributeNodes.Count = 0) and
           (child.Text = '') then
          Inc(emptyCount);
        CountEmptyElements(child);
      end;
    end;
  end;

begin
  version := TZUGFeRDVersion(_version);
  format := TZUGFeRDFormats(_format);
  profile := TZUGFeRDProfile(_profile);

  desc := TZUGFeRDInvoiceDescriptor.Create;
  try
    desc.Name := 'Test';
    desc.InvoiceNo := 'R0001';

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, version, profile, format);
      ms.Position := 0;

      SetLength(bytes, ms.Size);
      ms.ReadBuffer(bytes[0], ms.Size);
      xmlContent := TEncoding.UTF8.GetString(bytes);

      xmlDoc := TXMLDocument.Create(nil);
      xmlDoc.LoadFromXML(xmlContent);
      xmlDoc.Active := True;

      emptyCount := 0;
      CountEmptyElements(xmlDoc.DocumentElement);
      Assert.AreEqual(0, emptyCount, 'Found empty elements in the XML');
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TZUGFeRDCrossVersionTests);

end.
