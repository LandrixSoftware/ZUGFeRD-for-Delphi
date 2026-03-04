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

unit intf.ZUGFeRD20Tests.UnitTests;

interface

uses
  DUnitX.TestFramework,
  intf.ZUGFeRDTestBase;

type
  [TestFixture]
  TZUGFeRD20Tests = class(TZUGFeRDTestBase)
  public
    [Test]
    procedure TestLineStatusCode;
    [Test]
    procedure TestReferenceBasicInvoice;
    [Test]
    procedure TestReferenceExtendedInvoice;
    [Test]
    procedure TestTotalRounding;
    [Test]
    procedure TestMissingPropertiesAreNull;
    [Test]
    procedure TestMissingPropertListsEmpty;
    [Test]
    procedure TestLoadingSepaPreNotification;
    [Test]
    procedure TestStoringSepaPreNotification;
    [Test]
    procedure TestPartyExtensions;
    [Test]
    procedure TestMimetypeOfEmbeddedAttachment;
    [Test]
    procedure TestOrderInformation;
    [Test]
    procedure TestSellerOrderReferencedDocument;
    [Test]
    procedure TestWriteAndReadBusinessProcess;
    [Test]
    procedure TestWriteAndReadExtended;
    [Test]
    procedure TestApplicableTradeDeliveryTermsExists;
    [Test]
    procedure TestApplicableTradeDeliveryTermsIsNull;
  end;

implementation

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.IOUtils,
  intf.ZUGFeRDInvoiceDescriptor,
  intf.ZUGFeRDInvoiceProvider,
  intf.ZUGFeRDProfile,
  intf.ZUGFeRDInvoiceTypes,
  intf.ZUGFeRDVersion,
  intf.ZUGFeRDHelper,
  intf.ZUGFeRDCurrencyCodes,
  intf.ZUGFeRDCountryCodes,
  intf.ZUGFeRDQuantityCodes,
  intf.ZUGFeRDTaxTypes,
  intf.ZUGFeRDTaxCategoryCodes,
  intf.ZUGFeRDPaymentMeansTypeCodes,
  intf.ZUGFeRDGlobalID,
  intf.ZUGFeRDGlobalIDSchemeIdentifiers,
  intf.ZUGFeRDLegalOrganization,
  intf.ZUGFeRDTaxRegistrationSchemeID,
  intf.ZUGFeRDPaymentMeans,
  intf.ZUGFeRDPaymentTerms,
  intf.ZUGFeRDTradeLineItem,
  intf.ZUGFeRDAdditionalReferencedDocument,
  intf.ZUGFeRDAdditionalReferencedDocumentTypeCodes,
  intf.ZUGFeRDParty,
  intf.ZUGFeRDBankAccount,
  intf.ZUGFeRDSellerOrderReferencedDocument,
  intf.ZUGFeRDLineStatusCodes,
  intf.ZUGFeRDLineStatusReasonCodes,
  intf.ZUGFeRDTradeAllowanceCharge,
  intf.ZUGFeRDServiceCharge,
  intf.ZUGFeRDTradeDeliveryTermCodes,
  intf.ZUGFeRDElectronicAddressSchemeIdentifiers,
  intf.ZUGFeRDSpecifiedProcuringProject,
  intf.ZUGFeRDFinancialCard,
  intf.ZUGFeRDContractReferencedDocument,
  intf.ZUGFeRDApplicableProductCharacteristic,
  intf.ZUGFeRDAllowanceReasonCodes,
  intf.ZUGFeRDChargeReasonCodes,
  intf.ZUGFeRDReferenceTypeCodes,
  intf.ZUGFeRDTax,
  intf.ZUGFeRDNote,
  intf.ZUGFeRDInvoiceReferencedDocument,
  intf.ZUGFeRDAssociatedDocument,
  intf.ZUGFeRDContact;

{ TZUGFeRD20Tests }

procedure TZUGFeRD20Tests.TestLineStatusCode;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  tradeLineItem1, tradeLineItem3: TZUGFeRDTradeLineItem;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd20\zugferd_2p0_EXTENDED_Warenrechnung.xml'));
  try
    desc.TradeLineItems.Clear;

    tradeLineItem1 := desc.AddTradeLineItem(
      {name=}            'Trennbl'+#$00E4+'tter A4',
      {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(9.9),
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.H87),
      {unitQuantity=}    nil,
      {grossUnitPrice=}  TZUGFeRDNullableParam<Currency>.Create(9.9),
      {billedQuantity=}  20,
      {lineTotalAmount=} 0,
      {taxType=}         TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
      {categoryCode=}    TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
      {taxPercent=}      19.0
    );
    tradeLineItem1.SetLineStatus(TZUGFeRDLineStatusCodes.New, TZUGFeRDLineStatusReasonCodes.DETAIL);

    desc.AddTradeLineItem(
      {name=}            'Joghurt Banane',
      {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(5.5),
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.H87),
      {unitQuantity=}    nil,
      {grossUnitPrice=}  TZUGFeRDNullableParam<Currency>.Create(5.5),
      {billedQuantity=}  50,
      {lineTotalAmount=} 0,
      {taxType=}         TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
      {categoryCode=}    TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
      {taxPercent=}      7.0
    );

    tradeLineItem3 := desc.AddTradeLineItem(
      {name=}            'Abschlagsrechnung vom 01.01.2024',
      {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(500),
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62),
      {unitQuantity=}    nil,
      {grossUnitPrice=}  nil,
      {billedQuantity=}  -1,
      {lineTotalAmount=} 0,
      {taxType=}         TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
      {categoryCode=}    TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
      {taxPercent=}      19.0
    );
    tradeLineItem3.SetLineStatus(TZUGFeRDLineStatusCodes.DocumentationClaim, TZUGFeRDLineStatusReasonCodes.INFORMATION);

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version20, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual(3, loadedInvoice.TradeLineItems.Count);
        Assert.AreEqual(TZUGFeRDLineStatusCodes.New, loadedInvoice.TradeLineItems[0].AssociatedDocument.LineStatusCode.Value);
        Assert.AreEqual(TZUGFeRDLineStatusReasonCodes.DETAIL, loadedInvoice.TradeLineItems[0].AssociatedDocument.LineStatusReasonCode.Value);
        Assert.IsFalse(loadedInvoice.TradeLineItems[1].AssociatedDocument.LineStatusCode.HasValue);
        Assert.IsFalse(loadedInvoice.TradeLineItems[1].AssociatedDocument.LineStatusReasonCode.HasValue);
        Assert.AreEqual(TZUGFeRDLineStatusCodes.DocumentationClaim, loadedInvoice.TradeLineItems[2].AssociatedDocument.LineStatusCode.Value);
        Assert.AreEqual(TZUGFeRDLineStatusReasonCodes.INFORMATION, loadedInvoice.TradeLineItems[2].AssociatedDocument.LineStatusReasonCode.Value);
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

procedure TZUGFeRD20Tests.TestReferenceBasicInvoice;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd20\zugferd_2p0_BASIC_Einfach.xml'));
  try
    Assert.AreEqual(TZUGFeRDProfile.Basic, desc.Profile);
    Assert.AreEqual(TZUGFeRDInvoiceType.Invoice, desc.Type_);
    Assert.AreEqual('471102', desc.InvoiceNo);
    Assert.AreEqual(1, desc.TradeLineItems.Count);
    Assert.AreEqual<Currency>(198.0, desc.LineTotalAmount.Value);
    Assert.IsFalse(desc.IsTest);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD20Tests.TestReferenceExtendedInvoice;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd20\zugferd_2p0_EXTENDED_Warenrechnung.xml'));
  try
    Assert.AreEqual(TZUGFeRDProfile.Extended, desc.Profile);
    Assert.AreEqual(TZUGFeRDInvoiceType.Invoice, desc.Type_);
    Assert.AreEqual('R87654321012345', desc.InvoiceNo);
    Assert.AreEqual(6, desc.TradeLineItems.Count);
    Assert.AreEqual<Currency>(457.20, desc.LineTotalAmount.Value);
    Assert.IsTrue(desc.IsTest);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD20Tests.TestTotalRounding;
var
  uuid: string;
  issueDateTime: TDateTime;
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  msExtended, msBasic: TMemoryStream;
begin
  uuid := TGUID.NewGuid.ToString;
  issueDateTime := Date;

  desc := TZUGFeRDInvoiceDescriptor.Create;
  try
    desc.ContractReferencedDocument := TZUGFeRDContractReferencedDocument.Create;
    desc.ContractReferencedDocument.ID := uuid;
    desc.ContractReferencedDocument.IssueDateTime := issueDateTime;

    desc.SetTotals(1.99, 0, 0, 0, 0, 2, 0, 2, 0.01);

    msExtended := TMemoryStream.Create;
    try
      desc.Save(msExtended, TZUGFeRDVersion.Version20, TZUGFeRDProfile.Extended);
      msExtended.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(msExtended);
      try
        Assert.AreEqual<Currency>(0.01, loadedInvoice.RoundingAmount.Value);
      finally
        loadedInvoice.Free;
      end;
    finally
      msExtended.Free;
    end;

    msBasic := TMemoryStream.Create;
    try
      desc.Save(msBasic, TZUGFeRDVersion.Version20);
      msBasic.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(msBasic);
      try
        Assert.IsFalse(loadedInvoice.RoundingAmount.HasValue);
      finally
        loadedInvoice.Free;
      end;
    finally
      msBasic.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD20Tests.TestMissingPropertiesAreNull;
var
  desc: TZUGFeRDInvoiceDescriptor;
  i: Integer;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd20\zugferd_2p0_BASIC_Einfach.xml'));
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

procedure TZUGFeRD20Tests.TestMissingPropertListsEmpty;
var
  desc: TZUGFeRDInvoiceDescriptor;
  i: Integer;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd20\zugferd_2p0_BASIC_Einfach.xml'));
  try
    for i := 0 to desc.TradeLineItems.Count - 1 do
    begin
      Assert.AreEqual(0, desc.TradeLineItems[i].ApplicableProductCharacteristics.Count,
        Format('TradeLineItem[%d].ApplicableProductCharacteristics should be empty', [i]));
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD20Tests.TestLoadingSepaPreNotification;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd20\zugferd_2p0_EN16931_SEPA_Prenotification.xml'));
  try
    Assert.AreEqual(TZUGFeRDProfile.Comfort, desc.Profile);

    Assert.AreEqual('DE98ZZZ09999999999', desc.PaymentMeans.SEPACreditorIdentifier);
    Assert.AreEqual('REF A-123', desc.PaymentMeans.SEPAMandateReference);
    Assert.AreEqual(1, desc.DebitorBankAccounts.Count);
    Assert.AreEqual('DE21860000000086001055', desc.DebitorBankAccounts[0].IBAN);

    Assert.IsTrue(desc.PaymentTermsList.Count > 0, 'PaymentTermsList should not be empty');
    Assert.AreEqual(
      'Der Betrag in H'+#$00F6+'he von EUR 529,87 wird am 20.03.2018 von Ihrem Konto per SEPA-Lastschrift eingezogen.',
      Trim(desc.PaymentTermsList[0].Description));
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD20Tests.TestStoringSepaPreNotification;
var
  d, d2: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  d := TZUGFeRDInvoiceDescriptor.Create;
  try
    d.Type_ := TZUGFeRDInvoiceType.Invoice;
    d.InvoiceNo := '471102';
    d.Currency := TZUGFeRDCurrencyCodes.EUR;
    d.InvoiceDate := EncodeDate(2018, 3, 5);

    d._AddTradeLineItem(
      {lineID=}          '1',
      {name=}            'Trennbl'+#$00E4+'tter A4',
      {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(9.9),
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.H87),
      {unitQuantity=}    nil,
      {grossUnitPrice=}  TZUGFeRDNullableParam<Currency>.Create(9.9),
      {billedQuantity=}  20,
      {lineTotalAmount=} 0,
      {taxType=}         TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
      {categoryCode=}    TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
      {taxPercent=}      19.0,
      {comment=}         '',
      {id=}              TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.EAN, '4012345001235'),
      {sellerAssignedID=} 'TB100A4',
      {buyerAssignedID=} '',
      {deliveryNoteID=}  '',
      {deliveryNoteDate=} nil,
      {buyerOrderLineID=} '1'
    );

    d._AddTradeLineItem(
      {lineID=}          '2',
      {name=}            'Joghurt Banane',
      {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(5.5),
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.H87),
      {unitQuantity=}    nil,
      {grossUnitPrice=}  TZUGFeRDNullableParam<Currency>.Create(5.5),
      {billedQuantity=}  50,
      {lineTotalAmount=} 0,
      {taxType=}         TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
      {categoryCode=}    TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
      {taxPercent=}      7.0,
      {comment=}         '',
      {id=}              TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.EAN, '4000050986428'),
      {sellerAssignedID=} 'ARNR2'
    );

    d.SetSeller(
      {name=}     'Lieferant GmbH',
      {postcode=} '80333',
      {city=}     'M'+#$00FC+'nchen',
      {street=}   'Lieferantenstra'+#$00DF+'e 20',
      {country=}  TZUGFeRDNullableParam<TZUGFeRDCountryCodes>.Create(TZUGFeRDCountryCodes.DE),
      {id=}       '',
      {globalID=} TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.GLN, '4000001123452'),
      {legalOrg=} TZUGFeRDLegalOrganization.CreateWithParams(TZUGFeRDNullableParam<TZUGFeRDGlobalIDSchemeIdentifiers>.Create(TZUGFeRDGlobalIDSchemeIdentifiers.GLN), '4000001123452', 'Lieferant GmbH')
    );

    d.SetBuyer(
      {name=}     'Kunden AG Mitte',
      {postcode=} '69876',
      {city=}     'Frankfurt',
      {street=}   'Kundenstra'+#$00DF+'e 15',
      {country=}  TZUGFeRDNullableParam<TZUGFeRDCountryCodes>.Create(TZUGFeRDCountryCodes.DE),
      {id=}       'GE2020211',
      {globalID=} TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.GLN, '4000001987658')
    );

    d.SetPaymentMeansSepaDirectDebit('DE98ZZZ09999999999', 'REF A-123');

    d.AddDebitorFinancialAccount('DE21860000000086001055', '');

    d.AddTradePaymentTerms(
      'Der Betrag in H'+#$00F6+'he von EUR 529,87 wird am 20.03.2018 von Ihrem Konto per SEPA-Lastschrift eingezogen.');

    d.SetTotals(
      {lineTotalAmount=}    473.00,
      {chargeTotalAmount=}  0.00,
      {allowanceTotalAmount=} 0.00,
      {taxBasisAmount=}     473.00,
      {taxTotalAmount=}     56.87,
      {grandTotalAmount=}   529.87,
      {totalPrepaidAmount=} 0.00,
      {duePayableAmount=}   529.87
    );

    d.AddSellerTaxRegistration('201/113/40209', TZUGFeRDTaxRegistrationSchemeID.FC);
    d.AddSellerTaxRegistration('DE123456789', TZUGFeRDTaxRegistrationSchemeID.VA);

    d.AddApplicableTradeTax(
      {calculatedAmount=} 275.00 / 100 * 7.00,
      {basisAmount=}      275.00,
      {percent=}          7.00,
      {typeCode=}         TZUGFeRDTaxTypes.VAT,
      {categoryCode=}     TZUGFeRDTaxCategoryCodes.S
    );

    d.AddApplicableTradeTax(
      {calculatedAmount=} 198.00 / 100 * 19.00,
      {basisAmount=}      198.00,
      {percent=}          19.00,
      {typeCode=}         TZUGFeRDTaxTypes.VAT,
      {categoryCode=}     TZUGFeRDTaxCategoryCodes.S
    );

    ms := TMemoryStream.Create;
    try
      d.Save(ms, TZUGFeRDVersion.Version20, TZUGFeRDProfile.Comfort);
      ms.Position := 0;

      d2 := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual('DE98ZZZ09999999999', d2.PaymentMeans.SEPACreditorIdentifier);
        Assert.AreEqual('REF A-123', d2.PaymentMeans.SEPAMandateReference);
        Assert.AreEqual(1, d2.DebitorBankAccounts.Count);
        Assert.AreEqual('DE21860000000086001055', d2.DebitorBankAccounts[0].IBAN);
      finally
        d2.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    d.Free;
  end;
end;

procedure TZUGFeRD20Tests.TestPartyExtensions;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd20\zugferd_2p0_BASIC_Einfach.xml'));
  try
    desc.Invoicee := TZUGFeRDParty.Create;
    desc.Invoicee.Name := 'Test';
    desc.Invoicee.ContactName := 'Max Mustermann';
    desc.Invoicee.Postcode := '83022';
    desc.Invoicee.City := 'Rosenheim';
    desc.Invoicee.Street := 'M'+#$00FC+'nchnerstra'+#$00DF+'e 123';
    desc.Invoicee.AddressLine3 := 'EG links';
    desc.Invoicee.CountrySubdivisionName := 'Bayern';
    desc.Invoicee.Country := TZUGFeRDCountryCodes.DE;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version20, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual('Test', loadedInvoice.Invoicee.Name);
        Assert.AreEqual('Max Mustermann', loadedInvoice.Invoicee.ContactName);
        Assert.AreEqual('83022', loadedInvoice.Invoicee.Postcode);
        Assert.AreEqual('Rosenheim', loadedInvoice.Invoicee.City);
        Assert.AreEqual('M'+#$00FC+'nchnerstra'+#$00DF+'e 123', loadedInvoice.Invoicee.Street);
        Assert.AreEqual('EG links', loadedInvoice.Invoicee.AddressLine3);
        Assert.AreEqual('Bayern', loadedInvoice.Invoicee.CountrySubdivisionName);
        Assert.AreEqual(TZUGFeRDCountryCodes.DE, loadedInvoice.Invoicee.Country.Value);
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

procedure TZUGFeRD20Tests.TestMimetypeOfEmbeddedAttachment;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms, dataStream: TMemoryStream;
  filename1, filename2: string;
  timestamp: TDateTime;
  data: TBytes;
  doc: TZUGFeRDAdditionalReferencedDocument;
  i: Integer;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd20\zugferd_2p0_EXTENDED_Warenrechnung.xml'));
  try
    filename1 := 'myrandomdata.pdf';
    filename2 := 'myrandomdata.bin';
    timestamp := Date;
    SetLength(data, 32768);
    for i := 0 to Length(data) - 1 do
      data[i] := Random(256);

    dataStream := TMemoryStream.Create;
    try
      dataStream.WriteBuffer(data[0], Length(data));
      dataStream.Position := 0;

      desc.AddAdditionalReferencedDocument(
        {id=}       'My-File-PDF',
        {typeCode=} TZUGFeRDAdditionalReferencedDocumentTypeCode.ReferenceDocument,
        {issueDateTime=} TZUGFeRDNullableParam<TDateTime>.Create(timestamp),
        {name=}     'EmbeddedPdf',
        {referenceTypeCode=} nil,
        {attachmentBinaryObject=} dataStream,
        {filename=} filename1
      );

      dataStream.Position := 0;

      desc.AddAdditionalReferencedDocument(
        {id=}       'My-File-BIN',
        {typeCode=} TZUGFeRDAdditionalReferencedDocumentTypeCode.ReferenceDocument,
        {issueDateTime=} TZUGFeRDNullableParam<TDateTime>.Create(timestamp - 2),
        {name=}     'EmbeddedPdf',
        {referenceTypeCode=} nil,
        {attachmentBinaryObject=} dataStream,
        {filename=} filename2
      );
    finally
      dataStream.Free;
    end;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version20, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual(3, loadedInvoice.AdditionalReferencedDocuments.Count);

        for i := 0 to loadedInvoice.AdditionalReferencedDocuments.Count - 1 do
        begin
          doc := loadedInvoice.AdditionalReferencedDocuments[i];
          if doc.ID = 'My-File-PDF' then
          begin
            Assert.AreEqual(filename1, doc.Filename);
            Assert.AreEqual('application/pdf', doc.MimeType);
            Assert.AreEqual(timestamp, doc.IssueDateTime.Value);
          end;
          if doc.ID = 'My-File-BIN' then
          begin
            Assert.AreEqual(filename2, doc.Filename);
            Assert.AreEqual('application/octet-stream', doc.MimeType);
            Assert.AreEqual(timestamp - 2, doc.IssueDateTime.Value);
          end;
        end;
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

procedure TZUGFeRD20Tests.TestOrderInformation;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  timestamp: TDateTime;
begin
  timestamp := Date;

  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd20\zugferd_2p0_EXTENDED_Warenrechnung.xml'));
  try
    desc.OrderDate := timestamp;
    desc.OrderNo := '12345';

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version20, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual(timestamp, loadedInvoice.OrderDate.Value);
        Assert.AreEqual('12345', loadedInvoice.OrderNo);
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

procedure TZUGFeRD20Tests.TestSellerOrderReferencedDocument;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  uuid: string;
  issueDateTime: TDateTime;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd20\zugferd_2p0_EXTENDED_Warenrechnung.xml'));
  try
    uuid := TGUID.NewGuid.ToString;
    issueDateTime := Date;

    desc.SellerOrderReferencedDocument := TZUGFeRDSellerOrderReferencedDocument.Create;
    desc.SellerOrderReferencedDocument.ID := uuid;
    desc.SellerOrderReferencedDocument.IssueDateTime := issueDateTime;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version20, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual(TZUGFeRDProfile.Extended, loadedInvoice.Profile);
        Assert.AreEqual(uuid, loadedInvoice.SellerOrderReferencedDocument.ID);
        Assert.AreEqual(issueDateTime, loadedInvoice.SellerOrderReferencedDocument.IssueDateTime.Value);
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

procedure TZUGFeRD20Tests.TestWriteAndReadBusinessProcess;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.BusinessProcess := 'A1';

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version20, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual('A1', loadedInvoice.BusinessProcess);
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

procedure TZUGFeRD20Tests.TestWriteAndReadExtended;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms, dataStream: TMemoryStream;
  filename2: string;
  timestamp: TDateTime;
  data: TBytes;
  lineItem, loadedLineItem: TZUGFeRDTradeLineItem;
  bankAccount: TZUGFeRDBankAccount;
  debitorBankAccount: TZUGFeRDBankAccount;
  tax: TZUGFeRDTax;
  tradeCharge: TZUGFeRDTradeCharge;
  serviceCharge: TZUGFeRDServiceCharge;
  paymentTerms: TZUGFeRDPaymentTerms;
  lineItemReferencedDoc: TZUGFeRDAdditionalReferencedDocument;
  productChar: TZUGFeRDApplicableProductCharacteristic;
  lineItemAllowance: TZUGFeRDAbstractTradeAllowanceCharge;
  i: Integer;
  found: Boolean;
  tradeCharges: TArray<TZUGFeRDTradeCharge>;
  tradeAllowances: TArray<TZUGFeRDTradeAllowance>;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    filename2 := 'myrandomdata.bin';
    timestamp := Date;
    SetLength(data, 32768);
    for i := 0 to Length(data) - 1 do
      data[i] := Random(256);

    dataStream := TMemoryStream.Create;
    try
      dataStream.WriteBuffer(data[0], Length(data));
      dataStream.Position := 0;

      desc.AddAdditionalReferencedDocument(
        {id=}       'My-File-BIN',
        {typeCode=} TZUGFeRDAdditionalReferencedDocumentTypeCode.ReferenceDocument,
        {issueDateTime=} TZUGFeRDNullableParam<TDateTime>.Create(timestamp - 2),
        {name=}     'EmbeddedPdf',
        {referenceTypeCode=} nil,
        {attachmentBinaryObject=} dataStream,
        {filename=} filename2
      );
    finally
      dataStream.Free;
    end;

    desc.OrderNo := '12345';
    desc.OrderDate := timestamp;

    desc.SetContractReferencedDocument('12345', timestamp);

    desc.SpecifiedProcuringProject := TZUGFeRDSpecifiedProcuringProject.Create;
    desc.SpecifiedProcuringProject.ID := '123';
    desc.SpecifiedProcuringProject.Name := 'Project 123';

    desc.ShipTo := TZUGFeRDParty.Create;
    desc.ShipTo.ID := TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.Unknown, '123');
    desc.ShipTo.GlobalID := TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.DUNS, '789');
    desc.ShipTo.Name := 'Ship To';
    desc.ShipTo.ContactName := 'Max Mustermann';
    desc.ShipTo.Street := 'M'+#$00FC+'nchnerstr. 55';
    desc.ShipTo.Postcode := '83022';
    desc.ShipTo.City := 'Rosenheim';
    desc.ShipTo.Country := TZUGFeRDCountryCodes.DE;

    desc.ShipFrom := TZUGFeRDParty.Create;
    desc.ShipFrom.ID := TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.Unknown, '123');
    desc.ShipFrom.GlobalID := TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.DUNS, '789');
    desc.ShipFrom.Name := 'Ship From';
    desc.ShipFrom.ContactName := 'Eva Musterfrau';
    desc.ShipFrom.Street := 'Alpenweg 5';
    desc.ShipFrom.Postcode := '83022';
    desc.ShipFrom.City := 'Rosenheim';
    desc.ShipFrom.Country := TZUGFeRDCountryCodes.DE;

    desc.PaymentMeans.SEPACreditorIdentifier := 'SepaID';
    desc.PaymentMeans.SEPAMandateReference := 'SepaMandat';
    desc.PaymentMeans.FinancialCard := TZUGFeRDFinancialCard.Create;
    desc.PaymentMeans.FinancialCard.Id := '123';
    desc.PaymentMeans.FinancialCard.CardholderName := 'Mustermann';

    desc.PaymentReference := 'PaymentReference';

    desc.Invoicee := TZUGFeRDParty.Create;
    desc.Invoicee.Name := 'Test';
    desc.Invoicee.ContactName := 'Max Mustermann';
    desc.Invoicee.Postcode := '83022';
    desc.Invoicee.City := 'Rosenheim';
    desc.Invoicee.Street := 'M'+#$00FC+'nchnerstra'+#$00DF+'e 123';
    desc.Invoicee.AddressLine3 := 'EG links';
    desc.Invoicee.CountrySubdivisionName := 'Bayern';
    desc.Invoicee.Country := TZUGFeRDCountryCodes.DE;

    desc.Payee := TZUGFeRDParty.Create;
    desc.Payee.Name := 'Test';
    desc.Payee.ContactName := 'Max Mustermann';
    desc.Payee.Postcode := '83022';
    desc.Payee.City := 'Rosenheim';
    desc.Payee.Street := 'M'+#$00FC+'nchnerstra'+#$00DF+'e 123';
    desc.Payee.AddressLine3 := 'EG links';
    desc.Payee.CountrySubdivisionName := 'Bayern';
    desc.Payee.Country := TZUGFeRDCountryCodes.DE;

    desc.AddDebitorFinancialAccount('DE02120300000000202052', 'BYLADEM1001', '', '', 'Musterbank');

    desc.BillingPeriodStart := timestamp;
    desc.BillingPeriodEnd := timestamp + 14;

    desc.AddTradeCharge(
      {basisAmount=}     ZUGFeRDNullable<Currency>.Create(5),
      {currency=}        TZUGFeRDCurrencyCodes.EUR,
      {actualAmount=}    15,
      {reason=}          'Reason for charge',
      {taxTypeCode=}     ZUGFeRDNullable<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.AAB),
      {taxCategoryCode=} ZUGFeRDNullable<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.AB),
      {taxPercent=}      19,
      {reasonCode=}      TZUGFeRDNullableParam<TZUGFeRDChargeReasonCodes>.Create(TZUGFeRDChargeReasonCodes.HeatTreatment)
    );

    desc.AddLogisticsServiceCharge(10, 'Logistics service charge',
      ZUGFeRDNullable<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.AAC),
      ZUGFeRDNullable<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.AC),
      7);

    desc.PaymentTermsList[0].DueDate := timestamp + 14;
    desc.AddInvoiceReferencedDocument('RE-12345', TZUGFeRDNullableParam<TDateTime>.Create(timestamp));

    // set additional LineItem data
    lineItem := nil;
    for i := 0 to desc.TradeLineItems.Count - 1 do
      if desc.TradeLineItems[i].SellerAssignedID = 'TB100A4' then
      begin
        lineItem := desc.TradeLineItems[i];
        Break;
      end;
    Assert.IsNotNull(lineItem);
    lineItem.Description := 'This is line item TB100A4';
    lineItem.BuyerAssignedID := '0815';
    lineItem.SetOrderReferencedDocument('12345', TZUGFeRDNullableParam<TDateTime>.Create(timestamp), '1');
    lineItem.SetDeliveryNoteReferencedDocument('12345', TZUGFeRDNullableParam<TDateTime>.Create(timestamp), '1');
    lineItem.SetContractReferencedDocument('12345', TZUGFeRDNullableParam<TDateTime>.Create(timestamp), '1');

    lineItem.AddAdditionalReferencedDocument('xyz',
      TZUGFeRDAdditionalReferencedDocumentTypeCode.ReferenceDocument,
      timestamp, '',
      TZUGFeRDNullableParam<TZUGFeRDReferenceTypeCodes>.Create(TZUGFeRDReferenceTypeCodes.AAB));

    lineItem.NetQuantity := 3;
    lineItem.ActualDeliveryDate := timestamp;

    lineItem.ApplicableProductCharacteristics.Add(TZUGFeRDApplicableProductCharacteristic.Create);
    lineItem.ApplicableProductCharacteristics[lineItem.ApplicableProductCharacteristics.Count - 1].Description := 'Product characteristics';
    lineItem.ApplicableProductCharacteristics[lineItem.ApplicableProductCharacteristics.Count - 1].Value := 'Product value';

    lineItem.BillingPeriodStart := timestamp;
    lineItem.BillingPeriodEnd := timestamp + 10;

    lineItem.AddReceivableSpecifiedTradeAccountingAccount('987654');
    lineItem.AddTradeAllowance(TZUGFeRDCurrencyCodes.EUR,
      ZUGFeRDNullable<Currency>.Create(10), 50,
      'Reason: UnitTest',
      TZUGFeRDNullableParam<TZUGFeRDAllowanceReasonCodes>.Create(TZUGFeRDAllowanceReasonCodes.SpecialRebate));

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version20, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual('471102', loadedInvoice.InvoiceNo);
        Assert.AreEqual<TDateTime>(EncodeDate(2018, 03, 05), loadedInvoice.InvoiceDate);
        Assert.AreEqual(TZUGFeRDCurrencyCodes.EUR, loadedInvoice.Currency);

        found := False;
        for i := 0 to loadedInvoice.Notes.Count - 1 do
          if loadedInvoice.Notes[i].Content = 'Rechnung gem'+#$00E4+#$00DF+' Bestellung vom 01.03.2018.' then
          begin
            found := True;
            Break;
          end;
        Assert.IsTrue(found, 'Expected note not found');

        Assert.AreEqual('04011000-12345-34', loadedInvoice.ReferenceOrderNo);
        Assert.AreEqual('Lieferant GmbH', loadedInvoice.Seller.Name);
        Assert.AreEqual('80333', loadedInvoice.Seller.Postcode);
        Assert.AreEqual('M'+#$00FC+'nchen', loadedInvoice.Seller.City);
        Assert.AreEqual('Lieferantenstra'+#$00DF+'e 20', loadedInvoice.Seller.Street);
        Assert.AreEqual(TZUGFeRDCountryCodes.DE, loadedInvoice.Seller.Country.Value);
        Assert.AreEqual(TZUGFeRDGlobalIDSchemeIdentifiers.GLN, loadedInvoice.Seller.GlobalID.SchemeID.Value);
        Assert.AreEqual('4000001123452', loadedInvoice.Seller.GlobalID.ID);
        Assert.AreEqual('Max Mustermann', loadedInvoice.SellerContact.Name);
        Assert.AreEqual('Muster-Einkauf', loadedInvoice.SellerContact.OrgUnit);
        Assert.AreEqual('Max@Mustermann.de', loadedInvoice.SellerContact.EmailAddress);
        Assert.AreEqual('+49891234567', loadedInvoice.SellerContact.PhoneNo);

        Assert.AreEqual('Kunden AG Mitte', loadedInvoice.Buyer.Name);
        Assert.AreEqual('69876', loadedInvoice.Buyer.Postcode);
        Assert.AreEqual('Frankfurt', loadedInvoice.Buyer.City);
        Assert.AreEqual('Kundenstra'+#$00DF+'e 15', loadedInvoice.Buyer.Street);
        Assert.AreEqual(TZUGFeRDCountryCodes.DE, loadedInvoice.Buyer.Country.Value);
        Assert.AreEqual('GE2020211', loadedInvoice.Buyer.ID.ID);

        Assert.AreEqual('12345', loadedInvoice.OrderNo);
        Assert.AreEqual(timestamp, loadedInvoice.OrderDate.Value);

        Assert.AreEqual('123', loadedInvoice.ShipTo.ID.ID);
        Assert.AreEqual(TZUGFeRDGlobalIDSchemeIdentifiers.DUNS, loadedInvoice.ShipTo.GlobalID.SchemeID.Value);
        Assert.AreEqual('789', loadedInvoice.ShipTo.GlobalID.ID);
        Assert.AreEqual('Ship To', loadedInvoice.ShipTo.Name);
        Assert.AreEqual('Max Mustermann', loadedInvoice.ShipTo.ContactName);
        Assert.AreEqual('M'+#$00FC+'nchnerstr. 55', loadedInvoice.ShipTo.Street);
        Assert.AreEqual('83022', loadedInvoice.ShipTo.Postcode);
        Assert.AreEqual('Rosenheim', loadedInvoice.ShipTo.City);
        Assert.AreEqual(TZUGFeRDCountryCodes.DE, loadedInvoice.ShipTo.Country.Value);

        Assert.AreEqual('123', loadedInvoice.ShipFrom.ID.ID);
        Assert.AreEqual(TZUGFeRDGlobalIDSchemeIdentifiers.DUNS, loadedInvoice.ShipFrom.GlobalID.SchemeID.Value);
        Assert.AreEqual('789', loadedInvoice.ShipFrom.GlobalID.ID);
        Assert.AreEqual('Ship From', loadedInvoice.ShipFrom.Name);
        Assert.AreEqual('Eva Musterfrau', loadedInvoice.ShipFrom.ContactName);
        Assert.AreEqual('Alpenweg 5', loadedInvoice.ShipFrom.Street);
        Assert.AreEqual('83022', loadedInvoice.ShipFrom.Postcode);
        Assert.AreEqual('Rosenheim', loadedInvoice.ShipFrom.City);
        Assert.AreEqual(TZUGFeRDCountryCodes.DE, loadedInvoice.ShipFrom.Country.Value);

        Assert.AreEqual(EncodeDate(2018, 03, 05), loadedInvoice.ActualDeliveryDate.Value);
        Assert.AreEqual(TZUGFeRDPaymentMeansTypeCodes.SEPACreditTransfer, loadedInvoice.PaymentMeans.TypeCode.Value);
        Assert.AreEqual('Zahlung per SEPA '+#$00DC+'berweisung.', loadedInvoice.PaymentMeans.Information);

        Assert.AreEqual('PaymentReference', loadedInvoice.PaymentReference);

        Assert.AreEqual('SepaID', loadedInvoice.PaymentMeans.SEPACreditorIdentifier);
        Assert.AreEqual('SepaMandat', loadedInvoice.PaymentMeans.SEPAMandateReference);
        Assert.AreEqual('123', loadedInvoice.PaymentMeans.FinancialCard.Id);
        Assert.AreEqual('Mustermann', loadedInvoice.PaymentMeans.FinancialCard.CardholderName);

        // CreditorBankAccounts
        bankAccount := nil;
        for i := 0 to loadedInvoice.CreditorBankAccounts.Count - 1 do
          if loadedInvoice.CreditorBankAccounts[i].IBAN = 'DE02120300000000202051' then
          begin
            bankAccount := loadedInvoice.CreditorBankAccounts[i];
            Break;
          end;
        Assert.IsNotNull(bankAccount);
        Assert.AreEqual('Kunden AG', bankAccount.Name);
        Assert.AreEqual('DE02120300000000202051', bankAccount.IBAN);
        Assert.AreEqual('BYLADEM1001', bankAccount.BIC);

        // DebitorBankAccounts
        debitorBankAccount := nil;
        for i := 0 to loadedInvoice.DebitorBankAccounts.Count - 1 do
          if loadedInvoice.DebitorBankAccounts[i].IBAN = 'DE02120300000000202052' then
          begin
            debitorBankAccount := loadedInvoice.DebitorBankAccounts[i];
            Break;
          end;
        Assert.IsNotNull(debitorBankAccount);
        Assert.AreEqual('DE02120300000000202052', debitorBankAccount.IBAN);

        // Invoicee
        Assert.AreEqual('Test', loadedInvoice.Invoicee.Name);
        Assert.AreEqual('Max Mustermann', loadedInvoice.Invoicee.ContactName);
        Assert.AreEqual('83022', loadedInvoice.Invoicee.Postcode);
        Assert.AreEqual('Rosenheim', loadedInvoice.Invoicee.City);
        Assert.AreEqual('M'+#$00FC+'nchnerstra'+#$00DF+'e 123', loadedInvoice.Invoicee.Street);
        Assert.AreEqual('EG links', loadedInvoice.Invoicee.AddressLine3);
        Assert.AreEqual('Bayern', loadedInvoice.Invoicee.CountrySubdivisionName);
        Assert.AreEqual(TZUGFeRDCountryCodes.DE, loadedInvoice.Invoicee.Country.Value);

        // Payee
        Assert.AreEqual('Test', loadedInvoice.Payee.Name);
        Assert.AreEqual('Max Mustermann', loadedInvoice.Payee.ContactName);
        Assert.AreEqual('83022', loadedInvoice.Payee.Postcode);
        Assert.AreEqual('Rosenheim', loadedInvoice.Payee.City);
        Assert.AreEqual('M'+#$00FC+'nchnerstra'+#$00DF+'e 123', loadedInvoice.Payee.Street);
        Assert.AreEqual('EG links', loadedInvoice.Payee.AddressLine3);
        Assert.AreEqual('Bayern', loadedInvoice.Payee.CountrySubdivisionName);
        Assert.AreEqual(TZUGFeRDCountryCodes.DE, loadedInvoice.Payee.Country.Value);

        // Taxes
        tax := nil;
        for i := 0 to loadedInvoice.Taxes.Count - 1 do
          if loadedInvoice.Taxes[i].BasisAmount = 275 then
          begin
            tax := loadedInvoice.Taxes[i];
            Break;
          end;
        Assert.IsNotNull(tax);
        Assert.AreEqual<Currency>(275, tax.BasisAmount);
        Assert.AreEqual<Currency>(7, tax.Percent);
        Assert.AreEqual(TZUGFeRDTaxTypes.VAT, tax.TypeCode.Value);
        Assert.AreEqual(TZUGFeRDTaxCategoryCodes.S, tax.CategoryCode.Value);

        Assert.AreEqual(timestamp, loadedInvoice.BillingPeriodStart.Value);
        Assert.AreEqual(timestamp + 14, loadedInvoice.BillingPeriodEnd.Value);

        // TradeAllowanceCharges
        tradeAllowances := loadedInvoice.GetTradeAllowances;
        Assert.AreEqual<Integer>(0, Length(tradeAllowances));

        tradeCharge := nil;
        tradeCharges := loadedInvoice.GetTradeCharges;
        for i := 0 to Length(tradeCharges) - 1 do
          if tradeCharges[i].Reason = 'Reason for charge' then
          begin
            tradeCharge := tradeCharges[i];
            Break;
          end;
        Assert.IsNotNull(tradeCharge);
        Assert.IsTrue(tradeCharge.ChargeIndicator);
        Assert.AreEqual('Reason for charge', tradeCharge.Reason);
        Assert.AreEqual<Currency>(5, tradeCharge.BasisAmount.Value);
        Assert.AreEqual<Currency>(15, tradeCharge.ActualAmount);
        Assert.AreEqual(TZUGFeRDCurrencyCodes.EUR, tradeCharge.Currency);
        Assert.AreEqual<Currency>(19, tradeCharge.Tax.Percent);
        Assert.AreEqual(TZUGFeRDTaxTypes.AAB, tradeCharge.Tax.TypeCode.Value);
        Assert.AreEqual(TZUGFeRDTaxCategoryCodes.AB, tradeCharge.Tax.CategoryCode.Value);

        // ServiceCharges (check on original desc, same as C# test)
        serviceCharge := nil;
        for i := 0 to desc.ServiceCharges.Count - 1 do
          if desc.ServiceCharges[i].Description = 'Logistics service charge' then
          begin
            serviceCharge := desc.ServiceCharges[i];
            Break;
          end;
        Assert.IsNotNull(serviceCharge);
        Assert.AreEqual('Logistics service charge', serviceCharge.Description);
        Assert.AreEqual<Currency>(10, serviceCharge.Amount);
        Assert.AreEqual<Currency>(7, serviceCharge.Tax.Percent);
        Assert.AreEqual(TZUGFeRDTaxTypes.AAC, serviceCharge.Tax.TypeCode.Value);
        Assert.AreEqual(TZUGFeRDTaxCategoryCodes.AC, serviceCharge.Tax.CategoryCode.Value);

        // PaymentTerms
        Assert.IsTrue(loadedInvoice.PaymentTermsList.Count > 0);
        paymentTerms := loadedInvoice.PaymentTermsList[0];
        Assert.IsNotNull(paymentTerms);
        Assert.AreEqual('Zahlbar innerhalb 30 Tagen netto bis 04.04.2018, 3% Skonto innerhalb 10 Tagen bis 15.03.2018', paymentTerms.Description);
        Assert.AreEqual(timestamp + 14, paymentTerms.DueDate.Value);

        // Totals
        Assert.AreEqual<Currency>(473.0, loadedInvoice.LineTotalAmount.Value);
        Assert.IsFalse(loadedInvoice.ChargeTotalAmount.HasValue);
        Assert.IsFalse(loadedInvoice.AllowanceTotalAmount.HasValue);
        Assert.AreEqual<Currency>(473.0, loadedInvoice.TaxBasisAmount.Value);
        Assert.AreEqual<Currency>(56.87, loadedInvoice.TaxTotalAmount.Value);
        Assert.AreEqual<Currency>(529.87, loadedInvoice.GrandTotalAmount.Value);
        Assert.IsFalse(loadedInvoice.TotalPrepaidAmount.HasValue);
        Assert.AreEqual<Currency>(529.87, loadedInvoice.DuePayableAmount.Value);

        // InvoiceReferencedDocument
        Assert.IsTrue(loadedInvoice.InvoiceReferencedDocuments.Count > 0);
        Assert.AreEqual('RE-12345', loadedInvoice.InvoiceReferencedDocuments[0].ID);
        Assert.AreEqual(timestamp, loadedInvoice.InvoiceReferencedDocuments[0].IssueDateTime.Value);

        // Line items
        loadedLineItem := nil;
        for i := 0 to loadedInvoice.TradeLineItems.Count - 1 do
          if loadedInvoice.TradeLineItems[i].SellerAssignedID = 'TB100A4' then
          begin
            loadedLineItem := loadedInvoice.TradeLineItems[i];
            Break;
          end;
        Assert.IsNotNull(loadedLineItem);
        Assert.IsFalse(loadedLineItem.AssociatedDocument.LineID.Trim.IsEmpty);
        Assert.AreEqual('This is line item TB100A4', loadedLineItem.Description);

        Assert.AreEqual('Trennbl'+#$00E4+'tter A4', loadedLineItem.Name);

        Assert.AreEqual('TB100A4', loadedLineItem.SellerAssignedID);
        Assert.AreEqual('0815', loadedLineItem.BuyerAssignedID);
        Assert.AreEqual(TZUGFeRDGlobalIDSchemeIdentifiers.EAN, loadedLineItem.GlobalID.SchemeID.Value);
        Assert.AreEqual('4012345001235', loadedLineItem.GlobalID.ID);

        // GrossPriceProductTradePrice
        Assert.AreEqual<Currency>(9.9, loadedLineItem.GrossUnitPrice.Value);
        Assert.AreEqual(TZUGFeRDQuantityCodes.H87, loadedLineItem.UnitCode.Value);
        Assert.AreEqual<Currency>(3, loadedLineItem.NetQuantity.Value);

        // NetPriceProductTradePrice
        Assert.AreEqual<Currency>(9.9, loadedLineItem.NetUnitPrice.Value);
        Assert.AreEqual<Currency>(20, loadedLineItem.BilledQuantity);

        Assert.AreEqual(TZUGFeRDTaxTypes.VAT, loadedLineItem.TaxType.Value);
        Assert.AreEqual(TZUGFeRDTaxCategoryCodes.S, loadedLineItem.TaxCategoryCode.Value);
        Assert.AreEqual<Currency>(19, loadedLineItem.TaxPercent);

        Assert.AreEqual('1', loadedLineItem.BuyerOrderReferencedDocument.LineID);
        Assert.AreEqual('12345', loadedLineItem.BuyerOrderReferencedDocument.ID);
        Assert.AreEqual(timestamp, loadedLineItem.BuyerOrderReferencedDocument.IssueDateTime.Value);
        Assert.AreEqual('1', loadedLineItem.DeliveryNoteReferencedDocument.LineID);
        Assert.AreEqual('12345', loadedLineItem.DeliveryNoteReferencedDocument.ID);
        Assert.AreEqual(timestamp, loadedLineItem.DeliveryNoteReferencedDocument.IssueDateTime.Value);
        Assert.AreEqual('1', loadedLineItem.ContractReferencedDocument.LineID);
        Assert.AreEqual('12345', loadedLineItem.ContractReferencedDocument.ID);
        Assert.AreEqual(timestamp, loadedLineItem.ContractReferencedDocument.IssueDateTime.Value);

        // Line item referenced documents
        Assert.IsTrue(loadedLineItem.AdditionalReferencedDocuments.Count > 0);
        lineItemReferencedDoc := loadedLineItem.AdditionalReferencedDocuments[0];
        Assert.IsNotNull(lineItemReferencedDoc);
        Assert.AreEqual('xyz', lineItemReferencedDoc.ID);
        Assert.AreEqual(TZUGFeRDAdditionalReferencedDocumentTypeCode.ReferenceDocument, lineItemReferencedDoc.TypeCode.Value);
        Assert.AreEqual(timestamp, lineItemReferencedDoc.IssueDateTime.Value);
        Assert.AreEqual(TZUGFeRDReferenceTypeCodes.AAB, lineItemReferencedDoc.ReferenceTypeCode.Value);

        // Product characteristics
        Assert.IsTrue(loadedLineItem.ApplicableProductCharacteristics.Count > 0);
        productChar := loadedLineItem.ApplicableProductCharacteristics[0];
        Assert.IsNotNull(productChar);
        Assert.AreEqual('Product characteristics', productChar.Description);
        Assert.AreEqual('Product value', productChar.Value);

        Assert.AreEqual(timestamp, loadedLineItem.ActualDeliveryDate.Value);
        Assert.AreEqual(timestamp, loadedLineItem.BillingPeriodStart.Value);
        Assert.AreEqual(timestamp + 10, loadedLineItem.BillingPeriodEnd.Value);

        // Line item trade allowance charges
        lineItemAllowance := nil;
        for i := 0 to loadedLineItem.TradeAllowanceCharges.Count - 1 do
          if loadedLineItem.TradeAllowanceCharges[i].Reason = 'Reason: UnitTest' then
          begin
            lineItemAllowance := loadedLineItem.TradeAllowanceCharges[i];
            Break;
          end;
        Assert.IsNotNull(lineItemAllowance);
        Assert.IsFalse(lineItemAllowance.ChargeIndicator);
        Assert.AreEqual<Currency>(10, lineItemAllowance.BasisAmount.Value);
        Assert.AreEqual<Currency>(50, lineItemAllowance.ActualAmount);
        Assert.AreEqual('Reason: UnitTest', lineItemAllowance.Reason);
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

procedure TZUGFeRD20Tests.TestApplicableTradeDeliveryTermsExists;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.ApplicableTradeDeliveryTermsCode := TZUGFeRDTradeDeliveryTermCodes.CFR;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version20, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual(TZUGFeRDProfile.Extended, loadedInvoice.Profile);
        Assert.AreEqual(TZUGFeRDTradeDeliveryTermCodes.CFR, loadedInvoice.ApplicableTradeDeliveryTermsCode.Value);
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

procedure TZUGFeRD20Tests.TestApplicableTradeDeliveryTermsIsNull;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual(TZUGFeRDProfile.Extended, loadedInvoice.Profile);
        Assert.IsFalse(loadedInvoice.ApplicableTradeDeliveryTermsCode.HasValue);
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

end.
