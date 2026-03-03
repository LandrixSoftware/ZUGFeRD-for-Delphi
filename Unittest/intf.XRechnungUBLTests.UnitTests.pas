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

unit intf.XRechnungUBLTests.UnitTests;

interface

uses
  DUnitX.TestFramework,
  intf.ZUGFeRDTestBase;

type
  [TestFixture]
  TXRechnungUBLTests = class(TZUGFeRDTestBase)
  public
    [Test]
    procedure TestParentLineId;
    [Test]
    procedure TestInvoiceCreation;
    [Test]
    procedure TestTradelineitemProductCharacterstics;
    [Test]
    procedure TestSpecialUnitCodes;
    [Test]
    procedure TestTradelineitemAdditionalDocuments;
    [Test]
    procedure TestSkippingOfAllowanceChargeBasisAmount;
    [Test]
    procedure TestAllowanceChargeOnDocumentLevel;
    [Test]
    procedure TestInvoiceWithAttachment;
    [Test]
    procedure TestActualDeliveryDateWithoutDeliveryAddress;
    [Test]
    procedure TestActualDeliveryDateWithDeliveryAddress;
    [Test]
    procedure TestActualDeliveryAddressWithoutDeliveryDate;
    [Test]
    procedure TestTaxTypes;
    [Test]
    procedure TestSingleSkontoForCorrectIndention;
    [Test]
    procedure TestMultiSkontoForCorrectIndention;
    [Test]
    procedure TestBuyerOrderReferenceLineId;
    [Test]
    procedure TestMultipleCreditorBankAccounts;
    [Test]
    procedure TestBuyerPartyIdwithoutGloablID;
    [Test]
    procedure TestPartyIdentificationForSeller;
    [Test]
    procedure TestPartyIdentificationShouldExistOneTime;
    [Test]
    procedure TestInDebitInvoiceTheFinancialAccountNameAndFinancialInstitutionBranchShouldNotExist;
    [Test]
    procedure TestInDebitInvoiceThePaymentMandateIdShouldExist;
    [Test]
    procedure TestInvoiceWithoutOrderReferenceShouldNotWriteEmptyOrderReferenceElement;
    [Test]
    procedure TestApplicableTradeTaxWithExemption;
    [Test]
    procedure TestNote;
    [Test]
    procedure TestDespatchDocumentReference;
    [Test]
    procedure TestSampleCreditNote326;
    [Test]
    procedure TestSampleCreditNote384;
    [Test]
    procedure TestReferenceXRechnung21UBL;
    [Test]
    procedure TestDecimals;
    [Test]
    procedure TestDesignatedProductClassificationWithFullClassification;
    [Test]
    procedure TestBasicCreditNote;
    [Test]
    procedure TestCreaditNoteTagNS0;
    [Test]
    procedure TestNonStandardDateTimeFormat;
    [Test]
    procedure TestDontMixInvoicePeriodWithTradeLineItem;
    [Test]
    procedure TestSellerPartyDescription;
    [Test]
    procedure TestBuyerSellerSchemeIds;
  end;

implementation

uses
  System.SysUtils, System.Classes, System.Math, System.RegularExpressions,
  System.Generics.Collections,
  intf.ZUGFeRDInvoiceDescriptor,
  intf.ZUGFeRDInvoiceProvider,
  intf.ZUGFeRDProfile,
  intf.ZUGFeRDInvoiceTypes,
  intf.ZUGFeRDVersion,
  intf.ZUGFeRDFormats,
  intf.ZUGFeRDHelper,
  intf.ZUGFeRDCurrencyCodes,
  intf.ZUGFeRDCountryCodes,
  intf.ZUGFeRDQuantityCodes,
  intf.ZUGFeRDTaxTypes,
  intf.ZUGFeRDTaxCategoryCodes,
  intf.ZUGFeRDTaxExemptionReasonCodes,
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
  intf.ZUGFeRDTradeAllowanceCharge,
  intf.ZUGFeRDAllowanceReasonCodes,
  intf.ZUGFeRDReferenceTypeCodes,
  intf.ZUGFeRDTax,
  intf.ZUGFeRDNote,
  intf.ZUGFeRDAssociatedDocument,
  intf.ZUGFeRDContact,
  intf.ZUGFeRDDespatchAdviceReferencedDocument,
  intf.ZUGFeRDDesignatedProductClassification,
  intf.ZUGFeRDDesignatedProductClassificationClassCodes,
  intf.ZUGFeRDElectronicAddressSchemeIdentifiers,
  intf.ZUGFeRDBuyerOrderReferencedDocument,
  intf.ZUGFeRDApplicableProductCharacteristic;

{ TXRechnungUBLTests }

procedure TXRechnungUBLTests.TestParentLineId;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  subTradeLineItem1, subTradeLineItem2, subTradeLineItem3: TZUGFeRDTradeLineItem;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('xRechnung\xRechnung UBL.xml'));
  try
    desc.TradeLineItems.Clear;
    desc.AdditionalReferencedDocuments.Clear;

    desc._AddTradeLineItem('1',
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

    desc._AddTradeLineItem('2',
      {name=}            'Abschlagsrechnungen',
      {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(0),
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62),
      {unitQuantity=}    nil,
      {grossUnitPrice=}  nil,
      {billedQuantity=}  0,
      {lineTotalAmount=} 0,
      {taxType=}         TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
      {categoryCode=}    TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
      {taxPercent=}      0
    );

    subTradeLineItem1 := desc._AddTradeLineItem('2.1',
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
    subTradeLineItem1.SetParentLineId('2');

    subTradeLineItem2 := desc._AddTradeLineItem('2.2',
      {name=}            'Abschlagsrechnung vom 20.01.2024',
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
    subTradeLineItem2.SetParentLineId('2');

    subTradeLineItem3 := desc._AddTradeLineItem('2.2.1',
      {name=}            'Abschlagsrechnung vom 10.01.2024',
      {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(100),
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
    subTradeLineItem3.SetParentLineId('2.2');

    desc._AddTradeLineItem('3',
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

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual(6, loadedInvoice.TradeLineItems.Count);
        Assert.AreEqual('', loadedInvoice.TradeLineItems[0].AssociatedDocument.ParentLineID);
        Assert.AreEqual('', loadedInvoice.TradeLineItems[1].AssociatedDocument.ParentLineID);
        Assert.AreEqual('2', loadedInvoice.TradeLineItems[2].AssociatedDocument.ParentLineID);
        Assert.AreEqual('2', loadedInvoice.TradeLineItems[3].AssociatedDocument.ParentLineID);
        Assert.AreEqual('2.2', loadedInvoice.TradeLineItems[4].AssociatedDocument.ParentLineID);
        Assert.AreEqual('', loadedInvoice.TradeLineItems[5].AssociatedDocument.ParentLineID);
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

procedure TXRechnungUBLTests.TestInvoiceCreation;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNull(loadedInvoice.Invoicee);
        Assert.IsNotNull(loadedInvoice.Seller);
        Assert.AreEqual(2, loadedInvoice.Taxes.Count);
        Assert.AreEqual('Max Mustermann', loadedInvoice.SellerContact.Name);
        Assert.IsNull(loadedInvoice.BuyerContact);
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

procedure TXRechnungUBLTests.TestTradelineitemProductCharacterstics;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.TradeLineItems[0].ApplicableProductCharacteristics.Add(TZUGFeRDApplicableProductCharacteristic.Create);
    desc.TradeLineItems[0].ApplicableProductCharacteristics.Last.Description := 'Test Description';
    desc.TradeLineItems[0].ApplicableProductCharacteristics.Last.Value := '1.5 kg';
    desc.TradeLineItems[0].ApplicableProductCharacteristics.Add(TZUGFeRDApplicableProductCharacteristic.Create);
    desc.TradeLineItems[0].ApplicableProductCharacteristics.Last.Description := 'UBL Characterstics 2';
    desc.TradeLineItems[0].ApplicableProductCharacteristics.Last.Value := '3 kg';

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice.TradeLineItems);
        Assert.AreEqual(2, loadedInvoice.TradeLineItems[0].ApplicableProductCharacteristics.Count);
        Assert.AreEqual('Test Description', loadedInvoice.TradeLineItems[0].ApplicableProductCharacteristics[0].Description);
        Assert.AreEqual('3 kg', loadedInvoice.TradeLineItems[0].ApplicableProductCharacteristics[1].Value);
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

procedure TXRechnungUBLTests.TestSpecialUnitCodes;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  content: string;
  bytes: TBytes;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.TradeLineItems[0].UnitCode := TZUGFeRDQuantityCodes._4G;
    desc.TradeLineItems[1].UnitCode := TZUGFeRDQuantityCodes.H87;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        // test the raw xml file
        SetLength(bytes, ms.Size);
        ms.Position := 0;
        ms.ReadBuffer(bytes[0], ms.Size);
        content := TEncoding.UTF8.GetString(bytes);
        Assert.IsTrue(content.ToLower.Contains('unitcode="h87"'));
        Assert.IsTrue(content.ToLower.Contains('unitcode="4g"'));

        Assert.IsNotNull(loadedInvoice.TradeLineItems);
        Assert.AreEqual(TZUGFeRDQuantityCodes._4G, loadedInvoice.TradeLineItems[0].UnitCode.Value);
        Assert.AreEqual(TZUGFeRDQuantityCodes.H87, loadedInvoice.TradeLineItems[1].UnitCode.Value);
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

procedure TXRechnungUBLTests.TestTradelineitemAdditionalDocuments;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.TradeLineItems[0].AddAdditionalReferencedDocument('testid',
      TZUGFeRDAdditionalReferencedDocumentTypeCode.InvoiceDataSheet, 0, '',
      TZUGFeRDNullableParam<TZUGFeRDReferenceTypeCodes>.Create(TZUGFeRDReferenceTypeCodes.ON));
    desc.TradeLineItems[0].AddAdditionalReferencedDocument('testid2',
      TZUGFeRDAdditionalReferencedDocumentTypeCode.InvoiceDataSheet, 0, '',
      TZUGFeRDNullableParam<TZUGFeRDReferenceTypeCodes>.Create(TZUGFeRDReferenceTypeCodes.ON));

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice.TradeLineItems);
        Assert.AreEqual(2, loadedInvoice.TradeLineItems[0].AdditionalReferencedDocuments.Count);
        Assert.AreEqual('testid', loadedInvoice.TradeLineItems[0].AdditionalReferencedDocuments[0].ID);
        Assert.AreEqual('testid2', loadedInvoice.TradeLineItems[0].AdditionalReferencedDocuments[1].ID);
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

procedure TXRechnungUBLTests.TestSkippingOfAllowanceChargeBasisAmount;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  basisAmount, percent, allowanceChargeBasisAmount: Currency;
  tax: TZUGFeRDTax;
  i: Integer;
begin
  basisAmount := 123.0;
  percent := 11.0;
  allowanceChargeBasisAmount := 121.0;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.AddApplicableTradeTax(basisAmount / 100 * percent, basisAmount, percent,
      TZUGFeRDTaxTypes.LOC, TZUGFeRDTaxCategoryCodes.K,
      TZUGFeRDNullableParam<Currency>.Create(allowanceChargeBasisAmount));

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        tax := nil;
        for i := 0 to loadedInvoice.Taxes.Count - 1 do
        begin
          if loadedInvoice.Taxes[i].TypeCode.Value = TZUGFeRDTaxTypes.LOC then
          begin
            tax := loadedInvoice.Taxes[i];
            Break;
          end;
        end;

        Assert.IsNotNull(tax);
        Assert.AreEqual<Currency>(basisAmount, tax.BasisAmount);
        Assert.AreEqual<Currency>(percent, tax.Percent);
        Assert.IsFalse(tax.AllowanceChargeBasisAmount.HasValue);
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

procedure TXRechnungUBLTests.TestAllowanceChargeOnDocumentLevel;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  basisAmount: Currency;
  actualAmount: Currency;
  taxPercent: Currency;
  reason: string;
  allowances: TArray<TZUGFeRDTradeAllowance>;
  loadedAllowance: TZUGFeRDTradeAllowance;
begin
  basisAmount := 123.45;
  actualAmount := 12.34;
  reason := 'Besondere Vereinbarung';
  taxPercent := 19.0;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.AddTradeAllowance(
      basisAmount, TZUGFeRDCurrencyCodes.EUR, actualAmount, reason,
      TZUGFeRDTaxTypes.VAT, TZUGFeRDTaxCategoryCodes.AA, taxPercent,
      TZUGFeRDNullableParam<TZUGFeRDAllowanceReasonCodes>.Create(TZUGFeRDAllowanceReasonCodes.SpecialAgreement));

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        allowances := loadedInvoice.GetTradeAllowances;
        Assert.IsTrue(Length(allowances) > 0, 'No trade allowances found');
        loadedAllowance := allowances[0];

        Assert.AreEqual(False, loadedAllowance.ChargeIndicator, 'isDiscount');
        Assert.AreEqual<Currency>(basisAmount, loadedAllowance.BasisAmount.Value, 'basisAmount');
        Assert.AreEqual(TZUGFeRDCurrencyCodes.EUR, loadedAllowance.Currency, 'currency');
        Assert.AreEqual<Currency>(actualAmount, loadedAllowance.ActualAmount, 'actualAmount');
        Assert.AreEqual(reason, loadedAllowance.Reason, 'reason');
        Assert.AreEqual(TZUGFeRDAllowanceReasonCodes.SpecialAgreement, loadedAllowance.ReasonCode.Value, 'reasonCode');
        Assert.AreEqual(TZUGFeRDTaxTypes.VAT, loadedAllowance.Tax.TypeCode.Value, 'taxTypeCode');
        Assert.AreEqual(TZUGFeRDTaxCategoryCodes.AA, loadedAllowance.Tax.CategoryCode.Value, 'taxCategoryCode');
        Assert.AreEqual<Currency>(taxPercent, loadedAllowance.Tax.Percent, 'taxPercent');
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

procedure TXRechnungUBLTests.TestInvoiceWithAttachment;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms, attachmentStream: TMemoryStream;
  filename: string;
  data: TBytes;
  loadedData: TBytes;
  i: Integer;
  doc: TZUGFeRDAdditionalReferencedDocument;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    filename := 'myrandomdata.bin';
    SetLength(data, 32768);
    for i := 0 to Length(data) - 1 do
      data[i] := Random(256);

    attachmentStream := TMemoryStream.Create;
    try
      attachmentStream.WriteBuffer(data[0], Length(data));
      attachmentStream.Position := 0;

      desc.AddAdditionalReferencedDocument(
        {id=} 'My-File',
        {typeCode=} TZUGFeRDAdditionalReferencedDocumentTypeCode.ReferenceDocument,
        {issueDateTime=} nil,
        {name=} 'Ausf'+#$00FC+'hrbare Datei',
        {referenceTypeCode=} nil,
        {attachmentBinaryObject=} attachmentStream,
        {filename=} filename);
    finally
      attachmentStream.Free;
    end;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual(1, loadedInvoice.AdditionalReferencedDocuments.Count);

        for doc in loadedInvoice.AdditionalReferencedDocuments do
        begin
          if doc.ID = 'My-File' then
          begin
            SetLength(loadedData, doc.AttachmentBinaryObject.Size);
            doc.AttachmentBinaryObject.Position := 0;
            doc.AttachmentBinaryObject.ReadBuffer(loadedData, doc.AttachmentBinaryObject.Size);
            Assert.AreEqual(Length(data), Length(loadedData));
            for i := 0 to Length(data) - 1 do
              Assert.AreEqual(data[i], loadedData[i]);
            Assert.AreEqual(filename, doc.Filename);
            Break;
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

procedure TXRechnungUBLTests.TestActualDeliveryDateWithoutDeliveryAddress;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  timestamp: TDateTime;
begin
  timestamp := EncodeDate(2024, 08, 11);

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.ActualDeliveryDate := timestamp;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual(timestamp, loadedInvoice.ActualDeliveryDate.Value);
        Assert.IsNull(loadedInvoice.ShipTo);
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

procedure TXRechnungUBLTests.TestActualDeliveryDateWithDeliveryAddress;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  timestamp: TDateTime;
  shipToID, shipToName: string;
begin
  timestamp := EncodeDate(2024, 08, 11);
  shipToID := '1234';
  shipToName := 'Test ShipTo Name';

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.ActualDeliveryDate := timestamp;

    desc.ShipTo := TZUGFeRDParty.Create;
    desc.ShipTo.ID := TZUGFeRDGlobalID.Create;
    desc.ShipTo.ID.ID := shipToID;
    desc.ShipTo.Name := shipToName;
    desc.ShipTo.Country := TZUGFeRDCountryCodes.DE;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual(timestamp, loadedInvoice.ActualDeliveryDate.Value);
        Assert.IsNotNull(loadedInvoice.ShipTo);
        Assert.IsNotNull(loadedInvoice.ShipTo.ID);
        Assert.AreEqual(shipToID, loadedInvoice.ShipTo.ID.ID);
        Assert.AreEqual(shipToName, loadedInvoice.ShipTo.Name);
        Assert.AreEqual(TZUGFeRDCountryCodes.DE, loadedInvoice.ShipTo.Country.Value);
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

procedure TXRechnungUBLTests.TestActualDeliveryAddressWithoutDeliveryDate;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  shipToID, shipToName: string;
begin
  shipToID := '1234';
  shipToName := 'Test ShipTo Name';

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    // ActualDeliveryDate is set by the InvoiceProvider, we are resetting it
    desc.ActualDeliveryDate := Nil;

    desc.ShipTo := TZUGFeRDParty.Create;
    desc.ShipTo.ID := TZUGFeRDGlobalID.Create;
    desc.ShipTo.ID.ID := shipToID;
    desc.ShipTo.Name := shipToName;
    desc.ShipTo.Country := TZUGFeRDCountryCodes.DE;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsFalse(loadedInvoice.ActualDeliveryDate.HasValue);
        Assert.IsNotNull(loadedInvoice.ShipTo);
        Assert.IsNotNull(loadedInvoice.ShipTo.ID);
        Assert.AreEqual(shipToID, loadedInvoice.ShipTo.ID.ID);
        Assert.AreEqual(shipToName, loadedInvoice.ShipTo.Name);
        Assert.AreEqual(TZUGFeRDCountryCodes.DE, loadedInvoice.ShipTo.Country.Value);
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

procedure TXRechnungUBLTests.TestTaxTypes;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  content: string;
  bytes: TBytes;
  i: Integer;
  allVAT: Boolean;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        // test writing and parsing
        Assert.AreEqual(2, loadedInvoice.Taxes.Count);
        allVAT := True;
        for i := 0 to loadedInvoice.Taxes.Count - 1 do
          if loadedInvoice.Taxes[i].TypeCode.Value <> TZUGFeRDTaxTypes.VAT then
            allVAT := False;
        Assert.IsTrue(allVAT);

        // test the raw xml file
        SetLength(bytes, ms.Size);
        ms.Position := 0;
        ms.ReadBuffer(bytes[0], ms.Size);
        content := TEncoding.UTF8.GetString(bytes);
        Assert.IsFalse(content.ToLower.Contains('<cbc:id>va</cbc:id>'));
        Assert.IsTrue(content.ToLower.Contains('<cbc:id>vat</cbc:id>'));
        Assert.IsFalse(content.ToLower.Contains('<cbc:id>fc</cbc:id>'));
        Assert.IsTrue(content.ToLower.Contains('<cbc:id>id</cbc:id>'));
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

procedure TXRechnungUBLTests.TestSingleSkontoForCorrectIndention;
var
  desc: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  reader: TStreamReader;
  lines: TArray<string>;
  insidePaymentTerms, insideCbcNote: Boolean;
  noteIndentation, endNoteIndentation, indention, i, j: Integer;
  line, trimmedLine: string;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.ClearTradePaymentTerms;
    desc.AddTradePaymentTerms('#SKONTO#TAGE#14#PROZENT=0.00#BASISBETRAG=123.45#');

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      reader := TStreamReader.Create(ms, TEncoding.UTF8);
      try
        lines := reader.ReadToEnd.Split([sLineBreak]);
      finally
        reader.Free;
      end;

      insidePaymentTerms := False;
      insideCbcNote := False;
      noteIndentation := -1;

      for i := 0 to Length(lines) - 1 do
      begin
        line := lines[i];
        trimmedLine := line.Trim;

        if trimmedLine.ToLower.StartsWith('<cac:paymentterms>') then
        begin
          insidePaymentTerms := True;
          Continue;
        end
        else if not insidePaymentTerms then
          Continue;

        if (not insideCbcNote) and trimmedLine.ToLower.StartsWith('<cbc:note>') then
        begin
          insideCbcNote := True;
          noteIndentation := 0;
          for j := 1 to Length(line) do
            if line[j] = ' ' then
              Inc(noteIndentation)
            else
              Break;
          Assert.IsTrue(noteIndentation >= 0, 'Indentation for <cbc:Note> should be non-negative.');
          Continue;
        end;

        if insideCbcNote and trimmedLine.ToLower.StartsWith('</cbc:note>') then
        begin
          endNoteIndentation := 0;
          for j := 1 to Length(line) do
            if line[j] = ' ' then
              Inc(endNoteIndentation)
            else
              Break;
          Assert.AreEqual(noteIndentation, endNoteIndentation);
          insideCbcNote := False;
          Break;
        end;

        if insideCbcNote then
        begin
          indention := 0;
          for j := 1 to Length(line) do
            if line[j] = ' ' then
              Inc(indention)
            else
              Break;
          Assert.AreEqual(noteIndentation + 2, indention);
          Continue;
        end;
      end;

      Assert.IsFalse(insideCbcNote, 'We should have exited the <cbc:Note> block.');
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TXRechnungUBLTests.TestMultiSkontoForCorrectIndention;
var
  desc: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  reader: TStreamReader;
  lines: TArray<string>;
  insidePaymentTerms, insideCbcNote: Boolean;
  noteIndentation, endNoteIndentation, indention, i, j: Integer;
  line, trimmedLine: string;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.ClearTradePaymentTerms;
    desc.AddTradePaymentTerms('#SKONTO#TAGE#14#PROZENT=5.00#BASISBETRAG=123.45#');
    desc.AddTradePaymentTerms('#SKONTO#TAGE#21#PROZENT=1.00#BASISBETRAG=123.45#');

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      reader := TStreamReader.Create(ms, TEncoding.UTF8);
      try
        lines := reader.ReadToEnd.Split([sLineBreak]);
      finally
        reader.Free;
      end;

      insidePaymentTerms := False;
      insideCbcNote := False;
      noteIndentation := -1;

      for i := 0 to Length(lines) - 1 do
      begin
        line := lines[i];
        trimmedLine := line.Trim;

        if trimmedLine.ToLower.StartsWith('<cac:paymentterms>') then
        begin
          insidePaymentTerms := True;
          Continue;
        end
        else if not insidePaymentTerms then
          Continue;

        if (not insideCbcNote) and trimmedLine.ToLower.StartsWith('<cbc:note>') then
        begin
          insideCbcNote := True;
          noteIndentation := 0;
          for j := 1 to Length(line) do
            if line[j] = ' ' then
              Inc(noteIndentation)
            else
              Break;
          Assert.IsTrue(noteIndentation >= 0, 'Indentation for <cbc:Note> should be non-negative.');
          Continue;
        end;

        if insideCbcNote and trimmedLine.ToLower.StartsWith('</cbc:note>') then
        begin
          endNoteIndentation := 0;
          for j := 1 to Length(line) do
            if line[j] = ' ' then
              Inc(endNoteIndentation)
            else
              Break;
          Assert.AreEqual(noteIndentation, endNoteIndentation);
          insideCbcNote := False;
          Break;
        end;

        if insideCbcNote then
        begin
          indention := 0;
          for j := 1 to Length(line) do
            if line[j] = ' ' then
              Inc(indention)
            else
              Break;
          Assert.AreEqual(noteIndentation + 2, indention);
          Continue;
        end;
      end;

      Assert.IsFalse(insideCbcNote, 'We should have exited the <cbc:Note> block.');
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TXRechnungUBLTests.TestBuyerOrderReferenceLineId;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('xRechnung\xRechnung with LineId.xml'));
  try
    Assert.AreEqual('6171175.1', desc.TradeLineItems[0].BuyerOrderReferencedDocument.LineID);
  finally
    desc.Free;
  end;
end;

procedure TXRechnungUBLTests.TestMultipleCreditorBankAccounts;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  iban1, iban2, bic1, bic2: string;
  bankAccount: TZUGFeRDBankAccount;
begin
  iban1 := 'DE901213213312311231';
  iban2 := 'DE911213213312311231';
  bic1 := 'BIC-Test';
  bic2 := 'BIC-Test2';

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.CreditorBankAccounts.Clear;

    bankAccount := TZUGFeRDBankAccount.Create;
    bankAccount.IBAN := iban1;
    bankAccount.BIC := bic1;
    desc.CreditorBankAccounts.Add(bankAccount);

    bankAccount := TZUGFeRDBankAccount.Create;
    bankAccount.IBAN := iban2;
    bankAccount.BIC := bic2;
    desc.CreditorBankAccounts.Add(bankAccount);

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual(iban1, loadedInvoice.CreditorBankAccounts[0].IBAN);
        Assert.AreEqual(bic1, loadedInvoice.CreditorBankAccounts[0].BIC);
        Assert.AreEqual(iban2, loadedInvoice.CreditorBankAccounts[1].IBAN);
        Assert.AreEqual(bic2, loadedInvoice.CreditorBankAccounts[1].BIC);
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

procedure TXRechnungUBLTests.TestBuyerPartyIdwithoutGloablID;
var
  d, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  d := TZUGFeRDInvoiceDescriptor.Create;
  try
    d.Type_ := TZUGFeRDInvoiceType.Invoice;
    d.InvoiceNo := '471102';
    d.Currency := TZUGFeRDCurrencyCodes.EUR;
    d.InvoiceDate := EncodeDate(2018, 3, 5);
    d.SetBuyer(
      {name=}     'Kunden AG Mitte',
      {postcode=} '69876',
      {city=}     'Frankfurt',
      {street=}   'Kundenstra'+#$00DF+'e 15',
      {country=}  TZUGFeRDNullableParam<TZUGFeRDCountryCodes>.Create(TZUGFeRDCountryCodes.DE),
      {id=}       'GE2020211'
    );

    ms := TMemoryStream.Create;
    try
      d.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual('GE2020211', loadedInvoice.Buyer.ID.ID);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    d.Free;
  end;
end;

procedure TXRechnungUBLTests.TestPartyIdentificationForSeller;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms, resultStream: TMemoryStream;
  content: string;
  bytes: TBytes;
  countSupplier, countCustomer, idx: Integer;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        loadedInvoice.SetPaymentMeans(TZUGFeRDPaymentMeansTypeCodes.SEPACreditTransfer,
          'Hier sind Informationen', 'DE75512108001245126199', '[Mandate reference identifier]');

        resultStream := TMemoryStream.Create;
        try
          loadedInvoice.Save(resultStream, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);

          SetLength(bytes, resultStream.Size);
          resultStream.Position := 0;
          resultStream.ReadBuffer(bytes, resultStream.Size);
          content := TEncoding.UTF8.GetString(bytes);

          // Count PartyIdentification within AccountingSupplierParty
          countSupplier := 0;
          idx := Pos('<cac:AccountingSupplierParty>', content);
          if idx > 0 then
          begin
            idx := Pos('<cac:PartyIdentification>', content, idx);
            while (idx > 0) and (idx < Pos('</cac:AccountingSupplierParty>', content)) do
            begin
              Inc(countSupplier);
              idx := Pos('<cac:PartyIdentification>', content, idx + 1);
            end;
          end;
          Assert.AreEqual(1, countSupplier);

          // Count PartyIdentification within AccountingCustomerParty
          countCustomer := 0;
          idx := Pos('<cac:AccountingCustomerParty>', content);
          if idx > 0 then
          begin
            idx := Pos('<cac:PartyIdentification>', content, idx);
            while (idx > 0) and (idx < Pos('</cac:AccountingCustomerParty>', content)) do
            begin
              Inc(countCustomer);
              idx := Pos('<cac:PartyIdentification>', content, idx + 1);
            end;
          end;
          Assert.AreEqual(1, countCustomer);
        finally
          resultStream.Free;
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

procedure TXRechnungUBLTests.TestPartyIdentificationShouldExistOneTime;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms, resultStream: TMemoryStream;
  content: string;
  bytes: TBytes;
  count, idx: Integer;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        resultStream := TMemoryStream.Create;
        try
          loadedInvoice.Save(resultStream, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);

          SetLength(bytes, resultStream.Size);
          resultStream.Position := 0;
          resultStream.ReadBuffer(bytes, resultStream.Size);
          content := TEncoding.UTF8.GetString(bytes);

          count := 0;
          idx := Pos('<cac:PartyIdentification>', content);
          while idx > 0 do
          begin
            Inc(count);
            idx := Pos('<cac:PartyIdentification>', content, idx + 1);
          end;
          Assert.AreEqual(1, count);
        finally
          resultStream.Free;
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

procedure TXRechnungUBLTests.TestInDebitInvoiceTheFinancialAccountNameAndFinancialInstitutionBranchShouldNotExist;
var
  d: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  content: string;
  bytes: TBytes;
begin
  d := TZUGFeRDInvoiceDescriptor.Create;
  try
    d.Type_ := TZUGFeRDInvoiceType.Invoice;
    d.InvoiceNo := '471102';
    d.Currency := TZUGFeRDCurrencyCodes.EUR;
    d.InvoiceDate := EncodeDate(2018, 3, 5);

    d._AddTradeLineItem('1',
      {name=}            'Trennbl'+#$00E4+'tter A4',
      {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(9.9),
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.H87),
      {unitQuantity=}    nil,
      {grossUnitPrice=}  TZUGFeRDNullableParam<Currency>.Create(11.781),
      {billedQuantity=}  20,
      {lineTotalAmount=} 0,
      {taxType=}         TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
      {categoryCode=}    TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
      {taxPercent=}      19.0,
      {comment=}         '',
      {id=}              TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.EAN, '4012345001235'),
      {sellerAssignedID=} 'TB100A4'
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
      'Der Betrag in H'+#$00F6+'he von EUR 235,62 wird am 20.03.2018 von Ihrem Konto per SEPA-Lastschrift eingezogen.');

    d.SetTotals(
      {lineTotalAmount=}   198.00,
      {chargeTotalAmount=} 0.00,
      {allowanceTotalAmount=} 0.00,
      {taxBasisAmount=}    198.00,
      {taxTotalAmount=}    37.62,
      {grandTotalAmount=}  235.62,
      {totalPrepaidAmount=} 0.00,
      {duePayableAmount=}  235.62
    );

    d.AddSellerTaxRegistration('201/113/40209', TZUGFeRDTaxRegistrationSchemeID.FC);
    d.AddSellerTaxRegistration('DE123456789', TZUGFeRDTaxRegistrationSchemeID.VA);

    d.AddApplicableTradeTax(
      {calculatedAmount=} 198.00 / 100 * 19.00,
      {basisAmount=}      198.00,
      {percent=}          19.00,
      {typeCode=}         TZUGFeRDTaxTypes.VAT,
      {categoryCode=}     TZUGFeRDTaxCategoryCodes.S
    );

    ms := TMemoryStream.Create;
    try
      d.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      SetLength(bytes, ms.Size);
      ms.Position := 0;
      ms.ReadBuffer(bytes[0], ms.Size);
      content := TEncoding.UTF8.GetString(bytes);

      Assert.IsFalse(TRegEx.IsMatch(content, '<cac:PaymentMandate.*>.*<cbc:Name.*>.*</cac:PaymentMandate>', [roSingleLine]));
      Assert.IsFalse(TRegEx.IsMatch(content, '<cac:PaymentMandate.*>.*<cac:FinancialInstitutionBranch.*></cac:PaymentMandate>', [roSingleLine]));
    finally
      ms.Free;
    end;
  finally
    d.Free;
  end;
end;

procedure TXRechnungUBLTests.TestInDebitInvoiceThePaymentMandateIdShouldExist;
var
  d: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  content: string;
  bytes: TBytes;
begin
  d := TZUGFeRDInvoiceDescriptor.Create;
  try
    d.Type_ := TZUGFeRDInvoiceType.Invoice;
    d.InvoiceNo := '471102';
    d.Currency := TZUGFeRDCurrencyCodes.EUR;
    d.InvoiceDate := EncodeDate(2018, 3, 5);

    d._AddTradeLineItem('1',
      {name=}            'Trennbl'+#$00E4+'tter A4',
      {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(9.9),
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.H87),
      {unitQuantity=}    nil,
      {grossUnitPrice=}  TZUGFeRDNullableParam<Currency>.Create(11.781),
      {billedQuantity=}  20,
      {lineTotalAmount=} 0,
      {taxType=}         TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
      {categoryCode=}    TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
      {taxPercent=}      19.0,
      {comment=}         '',
      {id=}              TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.EAN, '4012345001235'),
      {sellerAssignedID=} 'TB100A4'
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
      'Der Betrag in H'+#$00F6+'he von EUR 235,62 wird am 20.03.2018 von Ihrem Konto per SEPA-Lastschrift eingezogen.');

    d.SetTotals(
      {lineTotalAmount=}   198.00,
      {chargeTotalAmount=} 0.00,
      {allowanceTotalAmount=} 0.00,
      {taxBasisAmount=}    198.00,
      {taxTotalAmount=}    37.62,
      {grandTotalAmount=}  235.62,
      {totalPrepaidAmount=} 0.00,
      {duePayableAmount=}  235.62
    );

    d.AddSellerTaxRegistration('201/113/40209', TZUGFeRDTaxRegistrationSchemeID.FC);
    d.AddSellerTaxRegistration('DE123456789', TZUGFeRDTaxRegistrationSchemeID.VA);

    d.AddApplicableTradeTax(
      {calculatedAmount=} 198.00 / 100 * 19.00,
      {basisAmount=}      198.00,
      {percent=}          19.00,
      {typeCode=}         TZUGFeRDTaxTypes.VAT,
      {categoryCode=}     TZUGFeRDTaxCategoryCodes.S
    );

    ms := TMemoryStream.Create;
    try
      d.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      SetLength(bytes, ms.Size);
      ms.Position := 0;
      ms.ReadBuffer(bytes[0], ms.Size);
      content := TEncoding.UTF8.GetString(bytes);

      Assert.IsTrue(TRegEx.IsMatch(content,
        '<cac:PaymentMeans.*>.*<cac:PaymentMandate.*>.*<cbc:ID.*>REF A-123</cbc:ID.*>.*</cac:PaymentMandate>',
        [roSingleLine]));
    finally
      ms.Free;
    end;
  finally
    d.Free;
  end;
end;

procedure TXRechnungUBLTests.TestInvoiceWithoutOrderReferenceShouldNotWriteEmptyOrderReferenceElement;
var
  d: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  content: string;
  bytes: TBytes;
begin
  d := TZUGFeRDInvoiceDescriptor.Create;
  try
    d.Type_ := TZUGFeRDInvoiceType.Invoice;
    d.InvoiceNo := '471102';
    d.Currency := TZUGFeRDCurrencyCodes.EUR;
    d.InvoiceDate := EncodeDate(2018, 3, 5);

    d._AddTradeLineItem('1',
      {name=}            'Trennbl'+#$00E4+'tter A4',
      {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(9.9),
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.H87),
      {unitQuantity=}    nil,
      {grossUnitPrice=}  TZUGFeRDNullableParam<Currency>.Create(11.781),
      {billedQuantity=}  20,
      {lineTotalAmount=} 0,
      {taxType=}         TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
      {categoryCode=}    TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
      {taxPercent=}      19.0,
      {comment=}         '',
      {id=}              TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.EAN, '4012345001235'),
      {sellerAssignedID=} 'TB100A4'
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
      'Der Betrag in H'+#$00F6+'he von EUR 235,62 wird am 20.03.2018 von Ihrem Konto per SEPA-Lastschrift eingezogen.');

    d.SetTotals(
      {lineTotalAmount=}   198.00,
      {chargeTotalAmount=} 0.00,
      {allowanceTotalAmount=} 0.00,
      {taxBasisAmount=}    198.00,
      {taxTotalAmount=}    37.62,
      {grandTotalAmount=}  235.62,
      {totalPrepaidAmount=} 0.00,
      {duePayableAmount=}  235.62
    );

    d.AddSellerTaxRegistration('201/113/40209', TZUGFeRDTaxRegistrationSchemeID.FC);
    d.AddSellerTaxRegistration('DE123456789', TZUGFeRDTaxRegistrationSchemeID.VA);

    d.AddApplicableTradeTax(
      {calculatedAmount=} 198.00 / 100 * 19.00,
      {basisAmount=}      198.00,
      {percent=}          19.00,
      {typeCode=}         TZUGFeRDTaxTypes.VAT,
      {categoryCode=}     TZUGFeRDTaxCategoryCodes.S
    );

    ms := TMemoryStream.Create;
    try
      d.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      SetLength(bytes, ms.Size);
      ms.Position := 0;
      ms.ReadBuffer(bytes[0], ms.Size);
      content := TEncoding.UTF8.GetString(bytes);

      Assert.IsFalse(content.Contains('OrderReference'));
    finally
      ms.Free;
    end;
  finally
    d.Free;
  end;
end;

procedure TXRechnungUBLTests.TestApplicableTradeTaxWithExemption;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  taxCount: Integer;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    taxCount := desc.Taxes.Count;
    desc.AddApplicableTradeTax(23.00, 123.00, 23, TZUGFeRDTaxTypes.VAT, TZUGFeRDTaxCategoryCodes.S,
      nil,
      TZUGFeRDNullableParam<TZUGFeRDTaxExemptionReasonCodes>.Create(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132),
      'Tax exemption reason');

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice);
        Assert.AreEqual(taxCount + 1, loadedInvoice.Taxes.Count);
        Assert.AreEqual('Tax exemption reason', loadedInvoice.Taxes[loadedInvoice.Taxes.Count - 1].ExemptionReason);
        Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132, loadedInvoice.Taxes[loadedInvoice.Taxes.Count - 1].ExemptionReasonCode.Value);
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

procedure TXRechnungUBLTests.TestNote;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  note: string;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    note := TGUID.NewGuid.ToString;
    desc.AddNote(note);

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice);
        Assert.AreEqual(3, loadedInvoice.Notes.Count); // 2 notes are already added by the InvoiceProvider
        Assert.AreEqual(note, loadedInvoice.Notes[loadedInvoice.Notes.Count - 1].Content);
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

procedure TXRechnungUBLTests.TestDespatchDocumentReference;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  reference: string;
  adviseDate: TDateTime;
begin
  reference := TGUID.NewGuid.ToString;
  adviseDate := Date;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.SetDespatchAdviceReferencedDocument(reference, TZUGFeRDNullableParam<TDateTime>.Create(adviseDate));

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice);
        Assert.IsNotNull(loadedInvoice.DespatchAdviceReferencedDocument);
        Assert.AreEqual(reference, loadedInvoice.DespatchAdviceReferencedDocument.ID);
        Assert.IsFalse(loadedInvoice.DespatchAdviceReferencedDocument.IssueDateTime.HasValue); // not defined in Peppol standard!
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

procedure TXRechnungUBLTests.TestSampleCreditNote326;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('xRechnung\ubl-cn-br-de-17-test-557-code-326.xml'));
  try
    Assert.AreEqual(TZUGFeRDInvoiceType.PartialInvoice, desc.Type_);
    Assert.AreEqual(1, desc.Notes.Count);
    Assert.AreEqual('Invoice Note Description', desc.Notes[0].Content);
    Assert.AreEqual(2, desc.TradeLineItems.Count);
    Assert.AreEqual('Beratung', desc.TradeLineItems[0].Name);
    Assert.AreEqual('Anforderungmanagament', desc.TradeLineItems[0].Description);
    Assert.AreEqual('Beratung', desc.TradeLineItems[desc.TradeLineItems.Count - 1].Name);
    Assert.AreEqual('', desc.TradeLineItems[desc.TradeLineItems.Count - 1].Description);
  finally
    desc.Free;
  end;
end;

procedure TXRechnungUBLTests.TestSampleCreditNote384;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('xRechnung\ubl-cn-br-de-17-test-559-code-384.xml'));
  try
    Assert.AreEqual(TZUGFeRDInvoiceType.Correction, desc.Type_);
    Assert.AreEqual(1, desc.Notes.Count);
    Assert.AreEqual('Invoice Note Description', desc.Notes[0].Content);
    Assert.AreEqual(2, desc.TradeLineItems.Count);
    Assert.AreEqual('Beratung', desc.TradeLineItems[0].Name);
    Assert.AreEqual('Anforderungmanagament', desc.TradeLineItems[0].Description);
    Assert.AreEqual('Beratung', desc.TradeLineItems[desc.TradeLineItems.Count - 1].Name);
    Assert.AreEqual('', desc.TradeLineItems[desc.TradeLineItems.Count - 1].Description);
  finally
    desc.Free;
  end;
end;

procedure TXRechnungUBLTests.TestReferenceXRechnung21UBL;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('xRechnung\xRechnung UBL.xml'));
  try
    Assert.AreEqual(TZUGFeRDProfile.XRechnung, desc.Profile);
    Assert.AreEqual(TZUGFeRDInvoiceType.Invoice, desc.Type_);

    Assert.AreEqual('0815-99-1-a', desc.InvoiceNo);
    Assert.AreEqual<TDateTime>(EncodeDate(2020, 6, 21), desc.InvoiceDate);
    Assert.AreEqual('0815-99-1-a', desc.PaymentReference);
    Assert.AreEqual('0815-99-1', desc.OrderNo);
    Assert.AreEqual(TZUGFeRDCurrencyCodes.EUR, desc.Currency);

    Assert.AreEqual('', desc.Buyer.Name);
    Assert.AreEqual('Klein Schlappstadt a.d. Lusche', desc.Buyer.City);
    Assert.AreEqual('12345', desc.Buyer.Postcode);
    Assert.AreEqual(TZUGFeRDCountryCodes.DE, desc.Buyer.Country.Value);
    Assert.AreEqual('Beispielgasse 17b', desc.Buyer.Street);
    Assert.AreEqual('Rechnungs Roulette GmbH & Co KG', desc.Buyer.SpecifiedLegalOrganization.TradingBusinessName);

    Assert.AreEqual('Manfred Mustermann', desc.BuyerContact.Name);
    Assert.AreEqual('manfred.mustermann@rr.de', desc.BuyerContact.EmailAddress);
    Assert.AreEqual('012345 98 765 - 44', desc.BuyerContact.PhoneNo);

    Assert.AreEqual('', desc.Seller.Name);
    Assert.AreEqual('Klein Schlappstadt a.d. Lusche', desc.Seller.City);
    Assert.AreEqual('12345', desc.Seller.Postcode);
    Assert.AreEqual(TZUGFeRDCountryCodes.DE, desc.Seller.Country.Value);
    Assert.AreEqual('Beispielgasse 17a', desc.Seller.Street);
    Assert.AreEqual('Harry Hirsch Holz- und Trockenbau', desc.Seller.SpecifiedLegalOrganization.TradingBusinessName);

    Assert.AreEqual('Harry Hirsch', desc.SellerContact.Name);
    Assert.AreEqual('harry.hirsch@hhhtb.de', desc.SellerContact.EmailAddress);
    Assert.AreEqual('012345 78 657 - 8', desc.SellerContact.PhoneNo);

    Assert.AreEqual(2, desc.TradeLineItems.Count);

    Assert.AreEqual('0815', desc.TradeLineItems[0].SellerAssignedID);
    Assert.AreEqual('Leimbinder', desc.TradeLineItems[0].Name);
    Assert.AreEqual('Leimbinder 2x18m; Birke', desc.TradeLineItems[0].Description);
    Assert.AreEqual<Currency>(1, desc.TradeLineItems[0].BilledQuantity);
    Assert.AreEqual<Currency>(1245.98, desc.TradeLineItems[0].LineTotalAmount);
    Assert.AreEqual<Currency>(19, desc.TradeLineItems[0].TaxPercent);

    Assert.AreEqual('MON', desc.TradeLineItems[1].SellerAssignedID);
    Assert.AreEqual('Montage', desc.TradeLineItems[1].Name);
    Assert.AreEqual('Montage durch Fachpersonal', desc.TradeLineItems[1].Description);
    Assert.AreEqual<Currency>(1, desc.TradeLineItems[1].BilledQuantity);
    Assert.AreEqual<Currency>(200.00, desc.TradeLineItems[1].LineTotalAmount);
    Assert.AreEqual<Currency>(7, desc.TradeLineItems[1].TaxPercent);

    Assert.AreEqual<Currency>(1445.98, desc.LineTotalAmount.Value);
    Assert.AreEqual<Currency>(250.74, desc.TaxTotalAmount.Value);
    Assert.AreEqual<Currency>(1696.72, desc.GrandTotalAmount.Value);
    Assert.AreEqual<Currency>(1696.72, desc.DuePayableAmount.Value);

    Assert.AreEqual<Currency>(236.74, desc.Taxes[0].TaxAmount);
    Assert.AreEqual<Currency>(1245.98, desc.Taxes[0].BasisAmount);
    Assert.AreEqual<Currency>(19, desc.Taxes[0].Percent);
    Assert.AreEqual(TZUGFeRDTaxTypes.VAT, desc.Taxes[0].TypeCode.Value);
    Assert.AreEqual(TZUGFeRDTaxCategoryCodes.S, desc.Taxes[0].CategoryCode.Value);

    Assert.AreEqual<Currency>(14.0000, desc.Taxes[1].TaxAmount);
    Assert.AreEqual<Currency>(200.00, desc.Taxes[1].BasisAmount);
    Assert.AreEqual<Currency>(7, desc.Taxes[1].Percent);
    Assert.AreEqual(TZUGFeRDTaxTypes.VAT, desc.Taxes[1].TypeCode.Value);
    Assert.AreEqual(TZUGFeRDTaxCategoryCodes.S, desc.Taxes[1].CategoryCode.Value);

    Assert.AreEqual<TDateTime>(EncodeDate(2020, 6, 21), desc.PaymentTermsList[0].DueDate.Value);

    Assert.AreEqual('DE12500105170648489890', desc.CreditorBankAccounts[0].IBAN);
    Assert.AreEqual('INGDDEFFXXX', desc.CreditorBankAccounts[0].BIC);
    Assert.AreEqual('Harry Hirsch', desc.CreditorBankAccounts[0].Name);

    Assert.AreEqual<TZUGFeRDPaymentMeansTypeCodes>(TZUGFeRDPaymentMeansTypeCodes.CreditTransferNonSEPA, desc.PaymentMeans.TypeCode.Value);
  finally
    desc.Free;
  end;
end;

procedure TXRechnungUBLTests.TestDecimals;
var
  desc: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  netUnitPrice, lineTotalTotalAmount, taxBasisAmount, grandTotalAmount, duePayableAmount: Currency;
  invoiceAsString: string;
  bytes: TBytes;
  rounded2, rounded4: Currency;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.TradeLineItems.Clear;

    netUnitPrice := 9.1235; // Currency has 4 decimal places in Delphi
    lineTotalTotalAmount := 123.4568;
    taxBasisAmount := 123.4568;
    grandTotalAmount := 123.4568;
    duePayableAmount := 123.4568;

    desc.LineTotalAmount := lineTotalTotalAmount;
    desc.TaxBasisAmount := taxBasisAmount;
    desc.GrandTotalAmount := grandTotalAmount;
    desc.DuePayableAmount := duePayableAmount;

    desc._AddTradeLineItem('1',
      {name=}            'Trennbl'+#$00E4+'tter A4',
      {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(netUnitPrice),
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.H87),
      {unitQuantity=}    nil,
      {grossUnitPrice=}  nil,
      {billedQuantity=}  20,
      {lineTotalAmount=} 0,
      {taxType=}         TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
      {categoryCode=}    TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
      {taxPercent=}      19,
      {comment=}         '',
      {id=}              TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.EAN, '4012345001235'),
      {sellerAssignedID=} 'TB100A4'
    );

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      desc.Free;
      desc := TZUGFeRDInvoiceDescriptor.Load(ms);

      SetLength(bytes, ms.Size);
      ms.Position := 0;
      ms.ReadBuffer(bytes[0], ms.Size);
      invoiceAsString := TEncoding.UTF8.GetString(bytes);

      // PriceAmount might have 4 decimals
      rounded2 := SimpleRoundTo(netUnitPrice, -2);
      rounded4 := SimpleRoundTo(netUnitPrice, -4);
      Assert.IsFalse(invoiceAsString.Contains('>' + FormatFloat('0.00', rounded2, TFormatSettings.Invariant) + '<'));
      Assert.IsTrue(invoiceAsString.Contains('>' + FormatFloat('0.0000', rounded4, TFormatSettings.Invariant) + '<'));
      Assert.AreEqual<Currency>(rounded4, desc.TradeLineItems[0].NetUnitPrice.Value);

      // Grand total, due payable etc. must have two decimals max
      rounded2 := SimpleRoundTo(lineTotalTotalAmount, -2);
      rounded4 := SimpleRoundTo(lineTotalTotalAmount, -4);
      Assert.IsTrue(invoiceAsString.Contains('>' + FormatFloat('0.00', rounded2, TFormatSettings.Invariant) + '<'));
      Assert.IsFalse(invoiceAsString.Contains('>' + FormatFloat('0.0000', rounded4, TFormatSettings.Invariant) + '<'));
      Assert.AreEqual<Currency>(rounded2, desc.LineTotalAmount.Value);

      rounded2 := SimpleRoundTo(taxBasisAmount, -2);
      rounded4 := SimpleRoundTo(taxBasisAmount, -4);
      Assert.IsTrue(invoiceAsString.Contains('>' + FormatFloat('0.00', rounded2, TFormatSettings.Invariant) + '<'));
      Assert.IsFalse(invoiceAsString.Contains('>' + FormatFloat('0.0000', rounded4, TFormatSettings.Invariant) + '<'));
      Assert.AreEqual<Currency>(rounded2, desc.TaxBasisAmount.Value);

      rounded2 := SimpleRoundTo(grandTotalAmount, -2);
      rounded4 := SimpleRoundTo(grandTotalAmount, -4);
      Assert.IsTrue(invoiceAsString.Contains('>' + FormatFloat('0.00', rounded2, TFormatSettings.Invariant) + '<'));
      Assert.IsFalse(invoiceAsString.Contains('>' + FormatFloat('0.0000', rounded4, TFormatSettings.Invariant) + '<'));
      Assert.AreEqual<Currency>(rounded2, desc.GrandTotalAmount.Value);

      rounded2 := SimpleRoundTo(duePayableAmount, -2);
      rounded4 := SimpleRoundTo(duePayableAmount, -4);
      Assert.IsTrue(invoiceAsString.Contains('>' + FormatFloat('0.00', rounded2, TFormatSettings.Invariant) + '<'));
      Assert.IsFalse(invoiceAsString.Contains('>' + FormatFloat('0.0000', rounded4, TFormatSettings.Invariant) + '<'));
      Assert.AreEqual<Currency>(rounded2, desc.DuePayableAmount.Value);
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TXRechnungUBLTests.TestDesignatedProductClassificationWithFullClassification;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  content: string;
  bytes: TBytes;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.TradeLineItems[0].AddDesignatedProductClassification(
      TZUGFeRDDesignatedProductClassificationClassCodes.HS,
      'List Version ID Value',
      'Class Code',
      'Class Name');

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);

      // string comparison
      ms.Position := 0;
      SetLength(bytes, ms.Size);
      ms.ReadBuffer(bytes[0], ms.Size);
      content := TEncoding.UTF8.GetString(bytes);
      Assert.IsTrue(content.Contains('<cac:CommodityClassification>'));
      Assert.IsTrue(content.Contains('<cbc:ItemClassificationCode listID="HS" listVersionID="List Version ID Value">Class Code</cbc:ItemClassificationCode>'));
      Assert.IsTrue(content.Contains('</cac:CommodityClassification>'));

      // structure comparison
      ms.Position := 0;
      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual(TZUGFeRDDesignatedProductClassificationClassCodes.HS, loadedInvoice.TradeLineItems[0].DesignedProductClassifications[0].ListID.Value);
        Assert.AreEqual('List Version ID Value', loadedInvoice.TradeLineItems[0].DesignedProductClassifications[0].ListVersionID);
        Assert.AreEqual('Class Code', loadedInvoice.TradeLineItems[0].DesignedProductClassifications[0].ClassCode);
        Assert.AreEqual('', loadedInvoice.TradeLineItems[0].DesignedProductClassifications[0].ClassName_);
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

procedure TXRechnungUBLTests.TestBasicCreditNote;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('xRechnung\ubl-cn-br-de-17-test-559-code-384.xml'));
  try
    Assert.AreEqual(TZUGFeRDInvoiceType.Correction, desc.Type_);
    Assert.AreEqual(2, desc.TradeLineItems.Count);
    Assert.AreEqual<Currency>(33, desc.TradeLineItems[0].BilledQuantity);
    Assert.AreEqual(TZUGFeRDQuantityCodes.XPP, desc.TradeLineItems[0].UnitCode.Value);
    Assert.AreEqual<Currency>(42, desc.TradeLineItems[desc.TradeLineItems.Count - 1].BilledQuantity);
    Assert.AreEqual(TZUGFeRDQuantityCodes.XPP, desc.TradeLineItems[desc.TradeLineItems.Count - 1].UnitCode.Value);
  finally
    desc.Free;
  end;
end;

procedure TXRechnungUBLTests.TestCreaditNoteTagNS0;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('xRechnung\maxRechnung_creditnote.xml'));
  try
    Assert.AreEqual(TZUGFeRDProfile.XRechnung, desc.Profile);
    Assert.AreEqual('1234567890', desc.InvoiceNo);
    Assert.AreEqual<TDateTime>(EncodeDate(2018, 10, 15), desc.InvoiceDate);
  finally
    desc.Free;
  end;
end;

procedure TXRechnungUBLTests.TestNonStandardDateTimeFormat;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('xRechnung\01.01a-INVOICE_ubl.xml'));
  try
    Assert.AreEqual<TDateTime>(EncodeDate(2016, 04, 04), desc.InvoiceDate);
  finally
    desc.Free;
  end;
end;

procedure TXRechnungUBLTests.TestDontMixInvoicePeriodWithTradeLineItem;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('xRechnung\01.01a-INVOICE_ubl.xml'));
  try
    Assert.IsFalse(desc.BillingPeriodStart.HasValue);
    Assert.IsFalse(desc.BillingPeriodEnd.HasValue);
  finally
    desc.Free;
  end;
end;

procedure TXRechnungUBLTests.TestSellerPartyDescription;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('xRechnung\maxRechnung_creditnote.xml'));
  try
    Assert.AreEqual('Weitere rechtliche' + #10 + #9#9#9#9#9 + 'Informationen', desc.Seller.Description);
  finally
    desc.Free;
  end;
end;

procedure TXRechnungUBLTests.TestBuyerSellerSchemeIds;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.Buyer.ID := TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.DUNS, '123456789');
    desc.Seller.ID := TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.AbnScheme, '987654321');
    desc.Buyer.SpecifiedLegalOrganization := TZUGFeRDLegalOrganization.CreateWithParams(
      TZUGFeRDNullableParam<TZUGFeRDGlobalIDSchemeIdentifiers>.Create(TZUGFeRDGlobalIDSchemeIdentifiers.DUNS),
      '123456789',
      'Buyer Company Ltd.');
    desc.Seller.SpecifiedLegalOrganization := TZUGFeRDLegalOrganization.CreateWithParams(
      TZUGFeRDNullableParam<TZUGFeRDGlobalIDSchemeIdentifiers>.Create(TZUGFeRDGlobalIDSchemeIdentifiers.AbnScheme),
      '987654321',
      'Seller Company Ltd.');

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<TZUGFeRDGlobalIDSchemeIdentifiers>(TZUGFeRDGlobalIDSchemeIdentifiers.DUNS, loadedInvoice.Buyer.ID.SchemeID.Value);
        Assert.AreEqual('123456789', loadedInvoice.Buyer.ID.ID);
        Assert.AreEqual<TZUGFeRDGlobalIDSchemeIdentifiers>(TZUGFeRDGlobalIDSchemeIdentifiers.AbnScheme, loadedInvoice.Seller.ID.SchemeID.Value);
        Assert.AreEqual('987654321', loadedInvoice.Seller.ID.ID);

        Assert.AreEqual<TZUGFeRDGlobalIDSchemeIdentifiers>(TZUGFeRDGlobalIDSchemeIdentifiers.DUNS, loadedInvoice.Buyer.SpecifiedLegalOrganization.ID.SchemeID.Value);
        Assert.AreEqual('123456789', loadedInvoice.Buyer.SpecifiedLegalOrganization.ID.ID);
        Assert.AreEqual<TZUGFeRDGlobalIDSchemeIdentifiers>(TZUGFeRDGlobalIDSchemeIdentifiers.AbnScheme, loadedInvoice.Seller.SpecifiedLegalOrganization.ID.SchemeID.Value);
        Assert.AreEqual('987654321', loadedInvoice.Seller.SpecifiedLegalOrganization.ID.ID);
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

initialization
  TDUnitX.RegisterTestFixture(TXRechnungUBLTests);

end.
