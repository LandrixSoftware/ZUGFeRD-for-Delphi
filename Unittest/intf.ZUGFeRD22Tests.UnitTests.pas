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

unit intf.ZUGFeRD22Tests.UnitTests;

interface

uses
  DUnitX.TestFramework,
  intf.ZUGFeRDTestBase;

type
  [TestFixture]
  TZUGFeRD22Tests = class(TZUGFeRDTestBase)
  public
    [Test]
    procedure TestComment;
    [Test]
    procedure TestLineStatusCode;
    [Test]
    procedure TestLineStatusReasonCodeWithoutStatusCode;
    [Test]
    procedure TestLineTotalAllowanceChargeAmount;
    [Test]
    procedure TestAdvancePayment;
    [Test]
    procedure TestAdvancePaymentMandatoryDataValidation;
    [Test]
    procedure TestNilAdvancePaymentValidation;
    [Test]
    [TestCase('ValidAmount', 'None')]
    [TestCase('ExplicitZero', 'Zero')]
    [TestCase('MissingPaidAmount', 'PaidAmount')]
    [TestCase('MissingIncludedTradeTax', 'IncludedTradeTax')]
    [TestCase('MissingCalculatedAmount', 'CalculatedAmount')]
    [TestCase('MissingTypeCode', 'TypeCode')]
    [TestCase('MissingCategoryCode', 'CategoryCode')]
    [TestCase('MissingReferenceID', 'IssuerAssignedID')]
    [TestCase('InvalidPaidAmount', 'InvalidPaidAmount')]
    [TestCase('InvalidCalculatedAmount', 'InvalidCalculatedAmount')]
    [TestCase('InvalidTypeCode', 'InvalidTypeCode')]
    [TestCase('InvalidCategoryCode', 'InvalidCategoryCode')]
    procedure TestAdvancePaymentReaderPreservesMissingMandatoryData(const missingField: string);
    [Test]
    procedure TestCIIReaderReleasesDescriptorAfterParsingError;
    /// <summary>Checks one valid document and seven distinct date failures before and after ownership transfer.</summary>
    [Test]
    [TestCase('ValidDocument', 'line-period,False')]
    [TestCase('InvalidLinePeriod', 'line-period,True')]
    [TestCase('InvalidLineOrder', 'line-order,True')]
    [TestCase('InvalidHeaderReference', 'header-reference,True')]
    [TestCase('InvalidLineReference', 'line-reference,True')]
    [TestCase('InvalidHeaderAfterAttachment', 'header-after-attachment,True')]
    [TestCase('InvalidLineAfterAttachment', 'line-after-attachment,True')]
    [TestCase('InvalidLineDelivery', 'line-delivery,True')]
    procedure TestCIIReaderNestedObjectOwnership(const failurePoint: string; invalidData: Boolean);
    [Test]
    procedure TestReferenceAdvancePaymentFromDocumentation;
    [Test]
    procedure TestExtendedInvoiceWithIncludedItems;
    [Test]
    procedure TestTradeLineItemProductFieldsRoundtrip;
    [Test]
    procedure TestIncludedReferencedProductFieldsRoundtrip;
    [Test]
    procedure TestReferenceEReportingFacturXInvoice;
    [Test]
    procedure TestReferenceBasicFacturXInvoice;
    [Test]
    procedure TestStoringReferenceBasicFacturXInvoice;
    [Test]
    procedure TestReferenceBasicWLInvoice;
    [Test]
    procedure TestReferenceExtendedInvoice;
    [Test]
    procedure TestReferenceMinimumInvoice;
    [Test]
    procedure TestReferenceXRechnung1CII;
    [Test]
    procedure TestElectronicAddress;
    [Test]
    procedure TestMinimumInvoice;
    [Test]
    procedure TestInvoiceWithAttachmentXRechnung;
    [Test]
    procedure TestInvoiceWithAttachmentExtended;
    [Test]
    procedure TestInvoiceWithAttachmentComfort;
    [Test]
    procedure TestInvoiceWithAttachmentBasic;
    [Test]
    procedure TestXRechnung1;
    [Test]
    procedure TestXRechnung2;
    [Test]
    procedure TestCreateInvoice_WithProfileEReporting;
    [Test]
    procedure TestBuyerOrderReferencedDocumentWithExtended;
    [Test]
    procedure TestBuyerOrderReferencedDocumentWithXRechnung;
    [Test]
    procedure TestContractReferencedDocumentWithXRechnung;
    [Test]
    procedure TestContractReferencedDocumentWithExtended;
    [Test]
    procedure TestTotalRoundingExtended;
    [Test]
    procedure TestTotalRoundingXRechnung;
    [Test]
    procedure TestMissingPropertiesAreNull;
    [Test]
    procedure TestMissingPropertiesAreEmpty;
    [Test]
    procedure TestReadTradeLineBillingPeriod;
    [Test]
    procedure TestReadTradeLineLineID;
    [Test]
    procedure TestReadTradeLineProductCharacteristics;
    [Test]
    procedure TestWriteTradeLineProductCharacteristics;
    [Test]
    procedure TestWriteTradeLineBillingPeriod;
    [Test]
    procedure TestWriteTradeLineBilledQuantity;
    [Test]
    procedure TestWriteTradeLineChargeFreePackage;
    [Test]
    procedure TestWriteTradeLineNetUnitPrice;
    [Test]
    procedure TestWriteTradeLineLineID;
    [Test]
    procedure TestLoadingSepaPreNotification;
    [Test]
    procedure TestStoringSepaPreNotification;
    [Test]
    procedure TestValidTaxTypes;
    [Test]
    procedure TestInvalidTaxTypes;
    [Test]
    procedure TestAdditionalReferencedDocument;
    [Test]
    procedure TestPartyExtensions;
    [Test]
    procedure TestShipTo;
    [Test]
    procedure TestShipToTradePartyOnItemLevel;
    [Test]
    procedure TestParty_WithTaxRegistration;
    [Test]
    procedure TestBuyerFCTaxRegistrationFiltered;
    [Test]
    procedure TestUltimateShipToTradePartyOnItemLevel;
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
    procedure TestAltteilSteuer;
    [Test]
    procedure TestTradeAllowanceChargeWithoutExplicitPercentage;
    [Test]
    procedure TestTradeAllowanceChargeWithExplicitPercentage;
    [Test]
    [TestCase('Basic','1')]
    [TestCase('BasicWL','16')]
    [TestCase('Comfort','2')]
    [TestCase('Extended','4')]
    [TestCase('XRechnung','32')]
    procedure TestWriteAndReadDespatchAdviceDocumentReferenceAllProfilesButMinimum(profileOrd: Integer);
    [Test]
    procedure TestWriteAndReadDespatchAdviceDocumentReferenceMinimum;
    [Test]
    procedure TestWriteAndReadDespatchAdviceDocumentReferenceExtended;
    [Test]
    [TestCase('Basic','1')]
    [TestCase('Comfort','2')]
    [TestCase('Extended','4')]
    [TestCase('XRechnung','32')]
    procedure TestSpecifiedTradeAllowanceCharge(profileOrd: Integer);
    [Test]
    procedure TestTradeLineItemsNotWrittenInMinimum;
    [Test]
    procedure TestSellerDescription;
    [Test]
    procedure TestSellerContact;
    [Test]
    procedure ShouldLoadCiiWithoutQdtNamespace;
    [Test]
    procedure TestDesignatedProductClassificationWithFullClassification;
    [Test]
    procedure TestDesignatedProductClassificationWithEmptyVersionId;
    [Test]
    procedure TestDesignatedProductClassificationWithEmptyListIdAndVersionId;
    [Test]
    procedure TestDesignatedProductClassificationWithoutAnyOptionalInformation;
    [Test]
    procedure TestPaymentTermsMultiCardinalityWithExtended;
    [Test]
    procedure TestPaymentTermsMultiCardinalityWithBasic;
    [Test]
    procedure TestPaymentTermsMultiCardinalityWithMinimum;
    [Test]
    procedure TestPaymentTermsSingleCardinality;
    [Test]
    procedure TestPaymentTermsXRechnungStructuredEndsWithLineBreak;
    [Test]
    procedure TestPaymentTermsSingleCardinalityXRechnungStructured;
    [Test]
    procedure TestPaymentTermsMultiCardinalityXRechnungStructuredOnlyOneSpecifiedTradePaymentTermsPresent;
    [Test]
    procedure TestPaymentTermsMultiCardinalityXRechnungStructured;
    [Test]
    procedure TestSingleXRechnungStructuredManually;
    [Test]
    procedure TestOfficialXRechnungFileForPaymentTerms;
    [Test]
    procedure TestBuyerOrderReferenceLineId;
    [Test]
    procedure TestRequiredDirectDebitFieldsShouldExist;
    [Test]
    procedure TestInNonDebitInvoiceTheDirectDebitFieldsShouldNotExist;
    [Test]
    procedure TestSpecifiedTradePaymentTermsDueDate;
    [Test]
    procedure TestSpecifiedTradePaymentTermsDescription;
    [Test]
    procedure TestSpecifiedTradePaymentTermsCalculationPercent;
    [Test]
    procedure TestTradeLineItemUnitChargeFreePackageQuantity;
    [Test]
    procedure TestApplicableTradeDeliveryTermsExists;
    [Test]
    procedure TestApplicableTradeDeliveryTermsIsNull;
    [Test]
    procedure TestInvoiceExemptions;
    [Test]
    procedure TestOriginTradeCountry;
    [Test]
    procedure TestLoadingCurrency;
    [Test]
    procedure TestLoadingSellerCountry;
    [Test]
    procedure TestLoadingBuyerCountry;
    [Test]
    procedure TestLoadingInvoiceType;
    [Test]
    procedure TestNonRSMInvoice;
    [Test]
    procedure TestRSMInvoice;
    [Test]
    procedure TestAlternateNamespace;
    [Test]
    procedure TestLocalNamespace;
    [Test]
    procedure TestFinancialInstitutionBICEmpty;
    [Test]
    procedure TestNoBICIDForDebitorFinancialInstitution;
    [Test]
    procedure TestBasisQuantityStandard;
    [Test]
    procedure TestBasisQuantityMultiple;
    [Test]
    procedure TestAccountingCost;
    [Test]
    procedure TestCIIWriterDirectSaveToFileWritesFile;
    [Test]
    procedure TestUBLWriterDirectSaveToFileWritesFile;
    [Test]
    procedure TestUBLWriterDirectRejectsNonXRechnungProfile;
    [Test]
    procedure TestDirectWritersValidateLikeDispatcher;
    [Test]
    procedure TestWriterSaveToFileRaisesWhenValidateReturnsFalse;
    [Test]
    procedure TestFailedSaveKeepsExistingFileUntouched;
    [Test]
    procedure TestXRechnungGuidelineUrnIsXRechnung30;
    [Test]
    procedure TestPrepaymentInvoiceAllowedOutsideXRechnung;
  end;

implementation

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.IOUtils,
  System.RegularExpressions,
  intf.ZUGFeRDInvoiceDescriptor,
  intf.ZUGFeRDInvoiceProvider,
  intf.ZUGFeRDProfile,
  intf.ZUGFeRDInvoiceTypes,
  intf.ZUGFeRDVersion,
  intf.ZUGFeRDHelper,
  intf.ZUGFeRDAdvancePayment,
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
  intf.ZUGFeRDDesignatedProductClassification,
  intf.ZUGFeRDDesignatedProductClassificationClassCodes,
  intf.ZUGFeRDAllowanceReasonCodes,
  intf.ZUGFeRDChargeReasonCodes,
  intf.ZUGFeRDReferenceTypeCodes,
  intf.ZUGFeRDTax,
  intf.ZUGFeRDNote,
  intf.ZUGFeRDSubjectCodes,
  intf.ZUGFeRDContentCodes,
  intf.ZUGFeRDInvoiceReferencedDocument,
  intf.ZUGFeRDAssociatedDocument,
  intf.ZUGFeRDContact,
  intf.ZUGFeRDDespatchAdviceReferencedDocument,
  intf.ZUGFeRDReceivableSpecifiedTradeAccountingAccount,
  intf.ZUGFeRDIncludedReferencedProduct,
  intf.ZUGFeRDDeliveryNoteReferencedDocument,
  intf.ZUGFeRDBuyerOrderReferencedDocument,
  intf.ZUGFeRDFormats,
  intf.ZUGFeRDExceptions,
  intf.ZUGFeRDInvoiceFormatOptions,
  intf.ZUGFeRDIInvoiceDescriptorWriter,
  intf.ZUGFeRDInvoiceDescriptor23Writer,
  intf.ZUGFeRDInvoiceDescriptor23CIIWriter,
  intf.ZUGFeRDInvoiceDescriptor22UBLWriter;

type
  /// <summary>
  /// Writer-Attrappe, deren Validate ohne Exception false liefert; damit wird der
  /// Schutzpfad in TZUGFeRDIInvoiceDescriptorWriter.Save(filename) geprüft.
  /// </summary>
  TValidateFalseWriter = class(TZUGFeRDIInvoiceDescriptorWriter)
  public
    procedure Save(descriptor: TZUGFeRDInvoiceDescriptor; stream: TStream; format: TZUGFeRDFormats = TZUGFeRDFormats.CII; options: TZUGFeRDInvoiceFormatOptions = Nil); override;
    function Validate(descriptor: TZUGFeRDInvoiceDescriptor; throwExceptions: Boolean = True): Boolean; override;
  end;

procedure TValidateFalseWriter.Save(descriptor: TZUGFeRDInvoiceDescriptor; stream: TStream; format: TZUGFeRDFormats; options: TZUGFeRDInvoiceFormatOptions);
begin
  // absichtlich leer
end;

function TValidateFalseWriter.Validate(descriptor: TZUGFeRDInvoiceDescriptor; throwExceptions: Boolean): Boolean;
begin
  Result := False;
end;

function TempInvoiceFileName: string;
begin
  Result := TPath.Combine(TPath.GetTempPath, 'ZfD_' + TPath.GetGUIDFileName + '.xml');
end;

{ TZUGFeRD22Tests }

procedure TZUGFeRD22Tests.TestComment;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.TradeLineItems.Clear;
    desc.AddTradeLineItem(
      {name=}            'Trennbl'+#$00E4+'tter A4',
      {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(9.9),
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.H87),
      {unitQuantity=}    nil,
      {grossUnitPrice=}  TZUGFeRDNullableParam<Currency>.Create(9.9),
      {billedQuantity=}  1,
      {lineTotalAmount=} 9.9,
      {taxType=}         TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
      {categoryCode=}    TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
      {taxPercent=}      19.0
    );

    desc.AddTradeLineCommentItem({comment=} 'Dies ist ein Kommentar');
    desc.TradeLineItems.Last.Name := 'Note';

    desc.Taxes.Clear;
    desc.AddApplicableTradeTax(
      {calculatedAmount=} 9.9 * 0.19,
      {basisAmount=}      9.9,
      {percent=}          19,
      {typeCode=}         TZUGFeRDTaxTypes.VAT,
      {categoryCode=}     TZUGFeRDTaxCategoryCodes.S
    );

    desc.LineTotalAmount := 9.9;
    desc.TaxBasisAmount := 9.9;
    desc.TaxTotalAmount := 9.9 * 0.19;
    desc.GrandTotalAmount := 9.9 * 1.19;
    desc.DuePayableAmount := 9.9 * 1.19;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestLineStatusCode;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  tradeLineItem1, tradeLineItem3: TZUGFeRDTradeLineItem;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd21\zugferd_2p1_EXTENDED_Warenrechnung-factur-x.xml'));
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
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<Integer>(3, loadedInvoice.TradeLineItems.Count);
        Assert.AreEqual<TZUGFeRDLineStatusCodes>(TZUGFeRDLineStatusCodes.New, loadedInvoice.TradeLineItems[0].AssociatedDocument.LineStatusCode.Value);
        Assert.AreEqual<TZUGFeRDLineStatusReasonCodes>(TZUGFeRDLineStatusReasonCodes.DETAIL, loadedInvoice.TradeLineItems[0].AssociatedDocument.LineStatusReasonCode.Value);
        Assert.IsFalse(loadedInvoice.TradeLineItems[1].AssociatedDocument.LineStatusCode.HasValue);
        Assert.IsFalse(loadedInvoice.TradeLineItems[1].AssociatedDocument.LineStatusReasonCode.HasValue);
        Assert.AreEqual<TZUGFeRDLineStatusCodes>(TZUGFeRDLineStatusCodes.DocumentationClaim, loadedInvoice.TradeLineItems[2].AssociatedDocument.LineStatusCode.Value);
        Assert.AreEqual<TZUGFeRDLineStatusReasonCodes>(TZUGFeRDLineStatusReasonCodes.INFORMATION, loadedInvoice.TradeLineItems[2].AssociatedDocument.LineStatusReasonCode.Value);
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

/// <summary>
/// Die offiziellen ZUGFeRD-Beispiele (z.B. die Abschlagsrechnungen mit SubInvoiceLine)
/// nutzen LineStatusReasonCode (BT-X-9) ohne LineStatusCode (BT-X-8). Der Reader muss
/// den Grund daher auch allein übernehmen und nicht nur im Paar.
/// </summary>
procedure TZUGFeRD22Tests.TestLineStatusReasonCodeWithoutStatusCode;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  tradeLineItem: TZUGFeRDTradeLineItem;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd21\zugferd_2p1_EXTENDED_Warenrechnung-factur-x.xml'));
  try
    desc.TradeLineItems.Clear;

    tradeLineItem := desc.AddTradeLineItem(
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
    // bewusst ohne LineStatusCode, deshalb nicht über SetLineStatus
    tradeLineItem.AssociatedDocument.LineStatusReasonCode := TZUGFeRDLineStatusReasonCodes.GROUP;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<Integer>(1, loadedInvoice.TradeLineItems.Count);
        Assert.IsFalse(loadedInvoice.TradeLineItems[0].AssociatedDocument.LineStatusCode.HasValue);
        Assert.IsTrue(loadedInvoice.TradeLineItems[0].AssociatedDocument.LineStatusReasonCode.HasValue,
          'LineStatusReasonCode ohne LineStatusCode ging beim Lesen verloren');
        Assert.AreEqual<TZUGFeRDLineStatusReasonCodes>(TZUGFeRDLineStatusReasonCodes.GROUP,
          loadedInvoice.TradeLineItems[0].AssociatedDocument.LineStatusReasonCode.Value);
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

/// <summary>
/// Gesamtbetrag der Positionszu- und -abschlaege auf Zeilenebene. Nur EXTENDED
/// darf das Element fuehren, EN16931 markiert es als "not used".
/// </summary>
procedure TZUGFeRD22Tests.TestLineTotalAllowanceChargeAmount;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  tradeLineItem: TZUGFeRDTradeLineItem;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd21\zugferd_2p1_EXTENDED_Warenrechnung-factur-x.xml'));
  try
    desc.TradeLineItems.Clear;

    tradeLineItem := desc.AddTradeLineItem(
      {name=}            'Trennbl'+#$00E4+'tter A4',
      {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(9.9),
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.H87),
      {unitQuantity=}    nil,
      {grossUnitPrice=}  TZUGFeRDNullableParam<Currency>.Create(9.9),
      {billedQuantity=}  20,
      {lineTotalAmount=} 198,
      {taxType=}         TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
      {categoryCode=}    TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
      {taxPercent=}      19.0
    );
    tradeLineItem.TotalAllowanceChargeAmount := 12.5;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<Integer>(1, loadedInvoice.TradeLineItems.Count);
        Assert.IsTrue(loadedInvoice.TradeLineItems[0].TotalAllowanceChargeAmount.HasValue,
          'TotalAllowanceChargeAmount ging beim Schreiben oder Lesen verloren');
        Assert.AreEqual<Currency>(12.5, loadedInvoice.TradeLineItems[0].TotalAllowanceChargeAmount.Value);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;

    // In EN16931 darf das Element nicht geschrieben werden
    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Comfort);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsFalse(loadedInvoice.TradeLineItems[0].TotalAllowanceChargeAmount.HasValue,
          'TotalAllowanceChargeAmount darf ausserhalb von EXTENDED nicht geschrieben werden');
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

/// <summary>
/// Vorauszahlungen / Anzahlungen, BG-X-45. Nur EXTENDED führt das Element.
/// </summary>
procedure TZUGFeRD22Tests.TestAdvancePayment;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  advancePayment: TZUGFeRDAdvancePayment;
  includedTax: TZUGFeRDTax;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd21\zugferd_2p1_EXTENDED_Warenrechnung-factur-x.xml'));
  try
    desc.AdvancePayments.Clear;

    advancePayment := TZUGFeRDAdvancePayment.Create;
    advancePayment.PaidAmount := 2975.0;
    advancePayment.FormattedReceivedDateTime := EncodeDate(2025, 6, 7);

    includedTax := TZUGFeRDTax.Create;
    includedTax.TaxAmount := 1900;
    includedTax.TypeCode := TZUGFeRDTaxTypes.VAT;
    includedTax.CategoryCode := TZUGFeRDTaxCategoryCodes.S;
    includedTax.Percent := 19;
    advancePayment.IncludedTradeTaxes.Add(includedTax);

    advancePayment.SetInvoiceReferencedDocument('R202506-01',
      TZUGFeRDNullableParam<TDateTime>.Create(EncodeDate(2025, 6, 1)),
      TZUGFeRDNullableParam<TZUGFeRDInvoiceType>.Create(TZUGFeRDInvoiceType.PartialInvoice));
    desc.AdvancePayments.Add(advancePayment);

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<Integer>(1, loadedInvoice.AdvancePayments.Count);
        Assert.AreEqual<Currency>(2975.0, loadedInvoice.AdvancePayments[0].PaidAmount.Value);
        Assert.AreEqual(EncodeDate(2025, 6, 7), loadedInvoice.AdvancePayments[0].FormattedReceivedDateTime.Value);

        Assert.AreEqual<Integer>(1, loadedInvoice.AdvancePayments[0].IncludedTradeTaxes.Count);
        Assert.AreEqual<Currency>(1900, loadedInvoice.AdvancePayments[0].IncludedTradeTaxes[0].TaxAmount);
        Assert.AreEqual<TZUGFeRDTaxTypes>(TZUGFeRDTaxTypes.VAT, loadedInvoice.AdvancePayments[0].IncludedTradeTaxes[0].TypeCode.Value);
        Assert.AreEqual<TZUGFeRDTaxCategoryCodes>(TZUGFeRDTaxCategoryCodes.S, loadedInvoice.AdvancePayments[0].IncludedTradeTaxes[0].CategoryCode.Value);
        Assert.AreEqual<Currency>(19, loadedInvoice.AdvancePayments[0].IncludedTradeTaxes[0].Percent);

        Assert.IsNotNull(loadedInvoice.AdvancePayments[0].InvoiceSpecifiedReferencedDocument);
        Assert.AreEqual('R202506-01', loadedInvoice.AdvancePayments[0].InvoiceSpecifiedReferencedDocument.ID);
        Assert.IsTrue(loadedInvoice.AdvancePayments[0].InvoiceSpecifiedReferencedDocument.TypeCode.HasValue);
        Assert.AreEqual<TZUGFeRDInvoiceType>(TZUGFeRDInvoiceType.PartialInvoice,
          loadedInvoice.AdvancePayments[0].InvoiceSpecifiedReferencedDocument.TypeCode.Value);
        Assert.AreEqual(EncodeDate(2025, 6, 1), loadedInvoice.AdvancePayments[0].InvoiceSpecifiedReferencedDocument.IssueDateTime.Value);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;

    // Außerhalb von EXTENDED darf das Element nicht geschrieben werden
    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Comfort);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<Integer>(0, loadedInvoice.AdvancePayments.Count);
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

/// <summary>
/// Prüft die Pflichtangaben von Vorauszahlungen vor dem Schreiben.
/// </summary>
procedure TZUGFeRD22Tests.TestAdvancePaymentMandatoryDataValidation;
var
  desc: TZUGFeRDInvoiceDescriptor;
  advancePayment: TZUGFeRDAdvancePayment;
  includedTax: TZUGFeRDTax;

  procedure AssertSaveRaisesMissingData;
  var
    stream: TMemoryStream;
  begin
    stream := TMemoryStream.Create;
    try
      Assert.WillRaise(
        procedure
        begin
          desc.Save(stream, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
        end,
        TZUGFeRDMissingDataException);
    finally
      stream.Free;
    end;
  end;

begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd21\zugferd_2p1_EXTENDED_Warenrechnung-factur-x.xml'));
  try
    desc.AdvancePayments.Clear;
    advancePayment := TZUGFeRDAdvancePayment.Create;
    desc.AdvancePayments.Add(advancePayment);

    AssertSaveRaisesMissingData;

    advancePayment.PaidAmount := 2975.0;
    AssertSaveRaisesMissingData;

    includedTax := TZUGFeRDTax.Create;
    advancePayment.IncludedTradeTaxes.Add(includedTax);
    Assert.IsFalse(includedTax.TaxAmount.HasValue);
    AssertSaveRaisesMissingData;

    includedTax.TaxAmount := 0;
    includedTax.TypeCode := nil;
    AssertSaveRaisesMissingData;

    includedTax.TypeCode := TZUGFeRDTaxTypes.VAT;
    includedTax.CategoryCode := nil;
    AssertSaveRaisesMissingData;

    includedTax.CategoryCode := TZUGFeRDTaxCategoryCodes.S;
    advancePayment.SetInvoiceReferencedDocument('');
    AssertSaveRaisesMissingData;
  finally
    desc.Free;
  end;
end;

/// <summary>
/// Verifies that a nil BG-X-45 list entry is reported separately from a
/// missing paid amount.
/// </summary>
procedure TZUGFeRD22Tests.TestNilAdvancePaymentValidation;
var
  Descriptor: TZUGFeRDInvoiceDescriptor;
  Stream: TMemoryStream;
  Raised: Boolean;
begin
  Descriptor := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd21\zugferd_2p1_EXTENDED_Warenrechnung-factur-x.xml'));
  try
    Descriptor.AdvancePayments.Clear;
    Descriptor.AdvancePayments.Add(nil);

    Stream := TMemoryStream.Create;
    try
      Raised := False;
      try
        Descriptor.Save(Stream, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      except
        on E: TZUGFeRDMissingDataException do
        begin
          Raised := True;
          Assert.IsTrue(E.Message.Contains('entry must not be nil'),
            'The validation message does not identify the nil advance payment entry: ' + E.Message);
        end;
      end;
      Assert.IsTrue(Raised, 'A nil advance payment entry was not rejected');
    finally
      Stream.Free;
    end;
  finally
    Descriptor.Free;
  end;
end;

/// <summary>
/// Bewahrt fehlende BG-X-45-Pflichtwerte ohne erfundene Nullwerte oder Verlust des
/// übrigen Belegs. Nur die gültigen Kontrollen dürfen wieder geschrieben werden.
/// Nichtleere, ungültige Beträge/Codes bleiben dagegen Lesefehler.
/// Aufgewärmte Lade-/Schreibzyklen prüfen zugleich die geänderten Besitzpfade.
/// </summary>
procedure TZUGFeRD22Tests.TestAdvancePaymentReaderPreservesMissingMandatoryData(const missingField: string);
var
  desc: TZUGFeRDInvoiceDescriptor;
  advancePayment: TZUGFeRDAdvancePayment;
  includedTax: TZUGFeRDTax;
  stream: TMemoryStream;
  bytes: TBytes;
  xmlContent, advanceXml, fieldName, removedFieldXml, changedAdvanceXml: string;
  firstBatchMemory, secondBatchMemory: NativeUInt;
  invalidValue: Boolean;

  // Prüft den geladenen Inhalt vor dem erneuten Schreiben, nicht nur Ausnahmefreiheit.
  procedure LoadAndCheck;
  var
    inputStream: TStringStream;
    outputStream: TMemoryStream;
    loadedInvoice: TZUGFeRDInvoiceDescriptor;
    loadedPayment: TZUGFeRDAdvancePayment;
    loadedTax: TZUGFeRDTax;
    expectedAmount: Currency;
  begin
    inputStream := TStringStream.Create(xmlContent, TEncoding.UTF8);
    try
      if invalidValue then
      begin
        Assert.WillRaise(
          procedure
          var
            unexpectedInvoice: TZUGFeRDInvoiceDescriptor;
          begin
            unexpectedInvoice := TZUGFeRDInvoiceDescriptor.Load(inputStream);
            unexpectedInvoice.Free;
          end,
          TZUGFeRDMissingDataException);
        Exit;
      end;
      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(inputStream);
      try
        Assert.AreEqual(desc.InvoiceNo, loadedInvoice.InvoiceNo);
        Assert.AreEqual(Integer(desc.TradeLineItems.Count), Integer(loadedInvoice.TradeLineItems.Count));
        Assert.AreEqual(desc.TradeLineItems[0].Name, loadedInvoice.TradeLineItems[0].Name);
        Assert.AreEqual<Currency>(desc.GrandTotalAmount.Value, loadedInvoice.GrandTotalAmount.Value);
        Assert.AreEqual<Currency>(desc.TotalPrepaidAmount.Value, loadedInvoice.TotalPrepaidAmount.Value);
        Assert.AreEqual<Currency>(desc.Taxes[0].TaxAmount.Value, loadedInvoice.Taxes[0].TaxAmount.Value);
        Assert.AreEqual(1, Integer(loadedInvoice.AdvancePayments.Count));
        loadedPayment := loadedInvoice.AdvancePayments[0];
        Assert.AreEqual(missingField <> 'PaidAmount', loadedPayment.PaidAmount.HasValue);
        if loadedPayment.PaidAmount.HasValue then
          Assert.AreEqual<Currency>(2975, loadedPayment.PaidAmount.Value);
        Assert.AreEqual(EncodeDate(2025, 6, 7), loadedPayment.FormattedReceivedDateTime.Value);

        if missingField = 'IncludedTradeTax' then
          Assert.AreEqual(0, Integer(loadedPayment.IncludedTradeTaxes.Count))
        else
        begin
          Assert.AreEqual(1, Integer(loadedPayment.IncludedTradeTaxes.Count));
          loadedTax := loadedPayment.IncludedTradeTaxes[0];
          Assert.AreEqual(missingField <> 'CalculatedAmount', loadedTax.TaxAmount.HasValue);
          if loadedTax.TaxAmount.HasValue then
          begin
            expectedAmount := 1900;
            if missingField = 'Zero' then
              expectedAmount := 0;
            Assert.AreEqual<Currency>(expectedAmount, loadedTax.TaxAmount.Value);
          end;
          Assert.AreEqual(missingField <> 'TypeCode', loadedTax.TypeCode.HasValue);
          if loadedTax.TypeCode.HasValue then
            Assert.AreEqual<TZUGFeRDTaxTypes>(TZUGFeRDTaxTypes.VAT, loadedTax.TypeCode.Value);
          Assert.AreEqual(missingField <> 'CategoryCode', loadedTax.CategoryCode.HasValue);
          if loadedTax.CategoryCode.HasValue then
            Assert.AreEqual<TZUGFeRDTaxCategoryCodes>(TZUGFeRDTaxCategoryCodes.S, loadedTax.CategoryCode.Value);
        end;

        Assert.IsNotNull(loadedPayment.InvoiceSpecifiedReferencedDocument);
        if missingField = 'IssuerAssignedID' then
          Assert.AreEqual('', loadedPayment.InvoiceSpecifiedReferencedDocument.ID)
        else
          Assert.AreEqual('R202506-01', loadedPayment.InvoiceSpecifiedReferencedDocument.ID);
        Assert.AreEqual(EncodeDate(2025, 6, 1), loadedPayment.InvoiceSpecifiedReferencedDocument.IssueDateTime.Value);
        Assert.AreEqual<TZUGFeRDInvoiceType>(TZUGFeRDInvoiceType.PartialInvoice,
          loadedPayment.InvoiceSpecifiedReferencedDocument.TypeCode.Value);

        outputStream := TMemoryStream.Create;
        try
          if (missingField = 'None') or (missingField = 'Zero') then
          begin
            loadedInvoice.Save(outputStream, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
            Assert.IsTrue(outputStream.Size > 0);
          end
          else
          begin
            Assert.WillRaise(
              procedure
              begin
                loadedInvoice.Save(outputStream, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
              end,
              TZUGFeRDMissingDataException);
            Assert.AreEqual<Int64>(0, outputStream.Size, 'Pflichtdaten vor der Ausgabe ablehnen.');
          end;
        finally
          outputStream.Free;
        end;
      finally
        loadedInvoice.Free;
      end;
      Assert.IsTrue(inputStream.Size > 0, 'Der Reader darf den Eingabestream nicht freigeben.');
    finally
      inputStream.Free;
    end;
  end;

begin
  invalidValue := missingField.StartsWith('Invalid');
  fieldName := missingField;
  if invalidValue then
    fieldName := missingField.Substring(Length('Invalid'));
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd21\zugferd_2p1_EXTENDED_Warenrechnung-factur-x.xml'));
  try
    desc.AdvancePayments.Clear;
    advancePayment := TZUGFeRDAdvancePayment.Create;
    desc.AdvancePayments.Add(advancePayment);
    advancePayment.PaidAmount := 2975.0;
    advancePayment.FormattedReceivedDateTime := EncodeDate(2025, 6, 7);

    includedTax := TZUGFeRDTax.Create;
    advancePayment.IncludedTradeTaxes.Add(includedTax);
    includedTax.TaxAmount := 1900;
    if missingField = 'Zero' then
      includedTax.TaxAmount := 0;
    includedTax.TypeCode := TZUGFeRDTaxTypes.VAT;
    includedTax.CategoryCode := TZUGFeRDTaxCategoryCodes.S;

    advancePayment.SetInvoiceReferencedDocument('R202506-01',
      TZUGFeRDNullableParam<TDateTime>.Create(EncodeDate(2025, 6, 1)),
      TZUGFeRDNullableParam<TZUGFeRDInvoiceType>.Create(TZUGFeRDInvoiceType.PartialInvoice));

    stream := TMemoryStream.Create;
    try
      desc.Save(stream, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      SetLength(bytes, stream.Size);
      stream.Position := 0;
      stream.ReadBuffer(bytes, stream.Size);
      xmlContent := TEncoding.UTF8.GetString(bytes);
    finally
      stream.Free;
    end;

    if (missingField <> 'None') and (missingField <> 'Zero') then
    begin
      // Ausschließlich BG-X-45 mutieren, niemals gleichnamige Kopf-/Positionsfelder.
      advanceXml := TRegEx.Match(xmlContent,
        '<ram:SpecifiedAdvancePayment>.*?</ram:SpecifiedAdvancePayment>', [roSingleLine]).Value;
      Assert.IsTrue(advanceXml <> '', 'Anzahlungsgruppe fehlt in der positiven Kontrolle.');
      removedFieldXml := TRegEx.Match(advanceXml,
        '<ram:' + fieldName + '(?:\s[^>]*)?>.*?</ram:' + fieldName + '>', [roSingleLine]).Value;
      Assert.IsTrue(removedFieldXml <> '', 'Das zu entfernende XML-Pflichtfeld wurde nicht gefunden.');
      // Nur den ersten Treffer entfernen: TypeCode kommt auch in der Rechnungsreferenz vor.
      changedAdvanceXml := StringReplace(advanceXml, removedFieldXml, '', []);
      if invalidValue then
        changedAdvanceXml := StringReplace(advanceXml, removedFieldXml,
          '<ram:' + fieldName + '>INVALID</ram:' + fieldName + '>', []);
      Assert.IsTrue(changedAdvanceXml <> advanceXml, 'Das zu entfernende XML-Pflichtfeld wurde nicht gefunden.');
      xmlContent := StringReplace(xmlContent, advanceXml, changedAdvanceXml, []);
    end;

    for var iteration := 1 to 10 do
      LoadAndCheck;
    firstBatchMemory := GetAllocatedMemory;
    for var iteration := 1 to 10 do
      LoadAndCheck;
    secondBatchMemory := GetAllocatedMemory;
    AssertNoMemoryGrowth(firstBatchMemory, secondBatchMemory);
  finally
    desc.Free;
  end;
end;

/// <summary>
/// Prüft, dass der CII-2.3-Reader einen teilweise aufgebauten Descriptor bei
/// einer Ausnahme vor dem Advance-Payment-Block freigibt.
/// </summary>
procedure TZUGFeRD22Tests.TestCIIReaderReleasesDescriptorAfterParsingError;
const
  validIssueDateXml = '<udt:DateTimeString format="102">20251001</udt:DateTimeString>';
  invalidIssueDateXml = '<udt:DateTimeString format="102">2025100</udt:DateTimeString>';
  measuredLoadCount = 10;
var
  xmlContent: string;
  invalidXml: string;
  invalidStream: TStringStream;
  firstBatchMemory: NativeUInt;
  secondBatchMemory: NativeUInt;
  loadIndex: Integer;
  allLoadsFailed: Boolean;

  function LoadRaisesParsingError: Boolean;
  var
    loadedInvoice: TZUGFeRDInvoiceDescriptor;
  begin
    Result := False;
    loadedInvoice := nil;
    try
      try
        invalidStream.Position := 0;
        loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(invalidStream);
      except
        Result := True;
      end;
    finally
      loadedInvoice.Free;
    end;
  end;

begin
  xmlContent := TFile.ReadAllText(DocumentationPath(
    'zugferd24-facturx1008\de\Beispiele\4. EXTENDED\EXTENDED_Warenrechnung\EXTENDED_Warenrechnung.xml'),
    TEncoding.UTF8);
  invalidXml := StringReplace(xmlContent, validIssueDateXml, invalidIssueDateXml, []);
  Assert.AreNotEqual(xmlContent, invalidXml, 'Das zu ersetzende Rechnungsdatum wurde nicht gefunden.');

  invalidStream := TStringStream.Create(invalidXml, TEncoding.UTF8);
  try
    Assert.IsTrue(LoadRaisesParsingError, 'Das ungültige Rechnungsdatum wurde nicht abgelehnt.');

    for loadIndex := 1 to measuredLoadCount do
      LoadRaisesParsingError;
    firstBatchMemory := GetAllocatedMemory;

    allLoadsFailed := True;
    for loadIndex := 1 to measuredLoadCount do
      allLoadsFailed := LoadRaisesParsingError and allLoadsFailed;
    secondBatchMemory := GetAllocatedMemory;

    Assert.IsTrue(allLoadsFailed, 'Mindestens ein ungültiges Rechnungsdatum wurde nicht abgelehnt.');
    AssertNoMemoryGrowth(firstBatchMemory, secondBatchMemory);
  finally
    invalidStream.Free;
  end;
end;

/// <summary>
/// Exercises late failures with both unowned children and already attached documents and streams.
/// AfterAttachment fails in the second reference, testing cleanup of its parent's first attachment.
/// Date failures precede the failing reference's own stream allocation; they do not exercise decoder/write failures.
/// </summary>
procedure TZUGFeRD22Tests.TestCIIReaderNestedObjectOwnership(const failurePoint: string; invalidData: Boolean);
const
  measuredLoadCount = 10;
  validDate = '20251001';
  invalidDate = '2025100';
var
  xmlContent: string;
  inputStream: TStringStream;
  expectedError: string;
  firstBatchMemory, secondBatchMemory: NativeUInt;
  allLoadsMatched: Boolean;

  /// <summary>Checks that decoded attachments remain usable until their owning invoice is freed.</summary>
  procedure CheckAttachment(attachment: TMemoryStream);
  var
    content: TBytes;
  begin
    Assert.IsNotNull(attachment);
    Assert.AreEqual(Int64(5), attachment.Size);
    SetLength(content, 5);
    attachment.Position := 0;
    attachment.ReadBuffer(content, Length(content));
    Assert.AreEqual('Hello', TEncoding.UTF8.GetString(content));
  end;

  /// <summary>Preserves the expected parser exception; successful results are always released.</summary>
  function LoadAndRelease(checkValues: Boolean): Boolean;
  var
    loadedInvoice: TZUGFeRDInvoiceDescriptor;
  begin
    loadedInvoice := nil;
    try
      try
        inputStream.Position := 0;
        loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(inputStream);
      except
        on error: Exception do
        begin
          if not invalidData then
            raise;
          Exit((error.ClassType = Exception) and (error.Message = expectedError));
        end;
      end;
      Result := not invalidData and (loadedInvoice <> nil);
      if checkValues and Result then
      begin
        Assert.AreEqual(1, Integer(loadedInvoice.TradeLineItems.Count));
        Assert.AreEqual(2, Integer(loadedInvoice.AdditionalReferencedDocuments.Count));
        Assert.IsNotNull(loadedInvoice.ShipTo);
        Assert.IsNotNull(loadedInvoice.ShipTo.SpecifiedLegalOrganization);
        Assert.AreEqual('SHIP-LEGAL', loadedInvoice.ShipTo.SpecifiedLegalOrganization.ID.ID);
        Assert.IsNotNull(loadedInvoice.ShipToContact);
        Assert.AreEqual('CONTACT', loadedInvoice.ShipToContact.Name);
        Assert.AreEqual('HEADER-DOC', loadedInvoice.AdditionalReferencedDocuments[0].ID);
        CheckAttachment(loadedInvoice.AdditionalReferencedDocuments[0].AttachmentBinaryObject);
        Assert.AreEqual('HEADER-LATE', loadedInvoice.AdditionalReferencedDocuments[1].ID);
        Assert.IsNull(loadedInvoice.AdditionalReferencedDocuments[1].AttachmentBinaryObject);
        var lineItem := loadedInvoice.TradeLineItems[0];
        Assert.AreEqual('1', lineItem.AssociatedDocument.LineID);
        Assert.AreEqual(EncodeDate(2025, 10, 1), lineItem.BillingPeriodStart.Value);
        Assert.AreEqual('ORDER', lineItem.BuyerOrderReferencedDocument.ID);
        Assert.AreEqual(1, Integer(lineItem.ApplicableProductCharacteristics.Count));
        Assert.AreEqual('blue', lineItem.ApplicableProductCharacteristics[0].Value);
        Assert.AreEqual(1, Integer(lineItem.IncludedReferencedProducts.Count));
        Assert.AreEqual('PART', lineItem.IncludedReferencedProducts[0].SellerAssignedID);
        Assert.AreEqual(1, Integer(lineItem.ReceivableSpecifiedTradeAccountingAccounts.Count));
        Assert.AreEqual('ACCOUNT', lineItem.ReceivableSpecifiedTradeAccountingAccounts[0].TradeAccountID);
        Assert.AreEqual(2, Integer(lineItem.AdditionalReferencedDocuments.Count));
        Assert.AreEqual('LINE-DOC', lineItem.AdditionalReferencedDocuments[0].ID);
        CheckAttachment(lineItem.AdditionalReferencedDocuments[0].AttachmentBinaryObject);
        Assert.AreEqual('LINE-LATE', lineItem.AdditionalReferencedDocuments[1].ID);
        Assert.IsNull(lineItem.AdditionalReferencedDocuments[1].AttachmentBinaryObject);
        Assert.AreEqual(EncodeDate(2025, 10, 1), lineItem.ActualDeliveryDate.Value);
      end;
    finally
      loadedInvoice.Free;
    end;
  end;

begin
  // Self-contained parser fixture, not a schema-conformance test. Each token has one failure site.
  xmlContent := '<rsm:CrossIndustryInvoice xmlns:rsm="urn:un:unece:uncefact:data:standard:CrossIndustryInvoice:100"' +
    ' xmlns:ram="urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:100"' +
    ' xmlns:udt="urn:un:unece:uncefact:data:standard:UnqualifiedDataType:100"' +
    ' xmlns:qdt="urn:un:unece:uncefact:data:standard:QualifiedDataType:100">' +
    '<rsm:ExchangedDocumentContext><ram:GuidelineSpecifiedDocumentContextParameter>' +
    '<ram:ID>urn:cen.eu:en16931:2017#conformant#urn:factur-x.eu:1p0:extended</ram:ID>' +
    '</ram:GuidelineSpecifiedDocumentContextParameter></rsm:ExchangedDocumentContext>' +
    '<rsm:ExchangedDocument><ram:ID>OWNERSHIP</ram:ID><ram:TypeCode>380</ram:TypeCode></rsm:ExchangedDocument>' +
    '<rsm:SupplyChainTradeTransaction><ram:IncludedSupplyChainTradeLineItem>' +
    '<ram:AssociatedDocumentLineDocument><ram:LineID>1</ram:LineID></ram:AssociatedDocumentLineDocument>' +
    '<ram:SpecifiedTradeProduct><ram:Name>Ownership test</ram:Name>' +
    '<ram:ApplicableProductCharacteristic><ram:Description>Colour</ram:Description><ram:Value>blue</ram:Value></ram:ApplicableProductCharacteristic>' +
    '<ram:IncludedReferencedProduct><ram:SellerAssignedID>PART</ram:SellerAssignedID></ram:IncludedReferencedProduct></ram:SpecifiedTradeProduct>' +
    '<ram:SpecifiedLineTradeAgreement><ram:BuyerOrderReferencedDocument><ram:IssuerAssignedID>ORDER</ram:IssuerAssignedID>' +
    '<ram:FormattedIssueDateTime><qdt:DateTimeString format="102">{line-order}</qdt:DateTimeString></ram:FormattedIssueDateTime>' +
    '</ram:BuyerOrderReferencedDocument></ram:SpecifiedLineTradeAgreement>' +
    '<ram:SpecifiedLineTradeDelivery><ram:ActualDeliverySupplyChainEvent><ram:OccurrenceDateTime>' +
    '<udt:DateTimeString format="102">{line-delivery}</udt:DateTimeString></ram:OccurrenceDateTime>' +
    '</ram:ActualDeliverySupplyChainEvent></ram:SpecifiedLineTradeDelivery>' +
    '<ram:SpecifiedLineTradeSettlement><ram:BillingSpecifiedPeriod><ram:StartDateTime>' +
    '<udt:DateTimeString format="102">{line-period}</udt:DateTimeString></ram:StartDateTime></ram:BillingSpecifiedPeriod>' +
    '<ram:AdditionalReferencedDocument><ram:IssuerAssignedID>LINE-DOC</ram:IssuerAssignedID><ram:TypeCode>916</ram:TypeCode>' +
    '<ram:AttachmentBinaryObject filename="line.txt">SGVsbG8=</ram:AttachmentBinaryObject>' +
    '<ram:FormattedIssueDateTime><qdt:DateTimeString format="102">{line-reference}</qdt:DateTimeString></ram:FormattedIssueDateTime>' +
    '</ram:AdditionalReferencedDocument><ram:AdditionalReferencedDocument><ram:IssuerAssignedID>LINE-LATE</ram:IssuerAssignedID>' +
    '<ram:FormattedIssueDateTime><qdt:DateTimeString format="102">{line-after-attachment}</qdt:DateTimeString></ram:FormattedIssueDateTime>' +
    '</ram:AdditionalReferencedDocument><ram:ReceivableSpecifiedTradeAccountingAccount><ram:ID>ACCOUNT</ram:ID>' +
    '</ram:ReceivableSpecifiedTradeAccountingAccount></ram:SpecifiedLineTradeSettlement></ram:IncludedSupplyChainTradeLineItem>' +
    '<ram:ApplicableHeaderTradeAgreement><ram:AdditionalReferencedDocument>' +
    '<ram:IssuerAssignedID>HEADER-DOC</ram:IssuerAssignedID><ram:TypeCode>916</ram:TypeCode>' +
    '<ram:AttachmentBinaryObject filename="header.txt">SGVsbG8=</ram:AttachmentBinaryObject>' +
    '<ram:FormattedIssueDateTime><qdt:DateTimeString format="102">{header-reference}</qdt:DateTimeString></ram:FormattedIssueDateTime>' +
    '</ram:AdditionalReferencedDocument><ram:AdditionalReferencedDocument><ram:IssuerAssignedID>HEADER-LATE</ram:IssuerAssignedID>' +
    '<ram:FormattedIssueDateTime><qdt:DateTimeString format="102">{header-after-attachment}</qdt:DateTimeString></ram:FormattedIssueDateTime>' +
    '</ram:AdditionalReferencedDocument></ram:ApplicableHeaderTradeAgreement>' +
    '<ram:ApplicableHeaderTradeDelivery><ram:ShipToTradeParty><ram:Name>SHIP</ram:Name>' +
    '<ram:SpecifiedLegalOrganization><ram:ID>SHIP-LEGAL</ram:ID></ram:SpecifiedLegalOrganization>' +
    '<ram:DefinedTradeContact><ram:PersonName>CONTACT</ram:PersonName></ram:DefinedTradeContact>' +
    '</ram:ShipToTradeParty></ram:ApplicableHeaderTradeDelivery>' +
    '</rsm:SupplyChainTradeTransaction></rsm:CrossIndustryInvoice>';

  Assert.IsTrue(xmlContent.Contains('{' + failurePoint + '}'), 'Unknown failure point.');
  expectedError := 'Wrong length of datetime element (format 102)';
  if invalidData then
    xmlContent := xmlContent.Replace('{' + failurePoint + '}', invalidDate);
  for var dateToken in TArray<string>.Create('line-period', 'line-order', 'line-delivery', 'header-reference', 'line-reference',
    'header-after-attachment', 'line-after-attachment') do
    xmlContent := xmlContent.Replace('{' + dateToken + '}', validDate);

  inputStream := TStringStream.Create(xmlContent, TEncoding.UTF8);
  try
    Assert.IsTrue(LoadAndRelease(True), 'The reader did not produce the expected result or parser exception.');
    for var warmupIndex := 1 to measuredLoadCount do
      Assert.IsTrue(LoadAndRelease(False), 'A warmup load did not produce the expected result or parser exception.');
    firstBatchMemory := GetAllocatedMemory;
    allLoadsMatched := True;
    for var loadIndex := 1 to measuredLoadCount do
      allLoadsMatched := LoadAndRelease(False) and allLoadsMatched;
    secondBatchMemory := GetAllocatedMemory;
    Assert.IsTrue(allLoadsMatched, 'A repeated load did not produce the expected result or parser exception.');
    AssertNoMemoryGrowth(firstBatchMemory, secondBatchMemory);
  finally
    inputStream.Free;
  end;
end;

/// <summary>
/// Liest die Anzahlungen aus dem offiziellen Beispiel der Projektabschlussrechnung.
/// Die Fixture gehört zum Testumfang und ihr Fehlen wird als Testfehler gemeldet.
/// </summary>
procedure TZUGFeRD22Tests.TestReferenceAdvancePaymentFromDocumentation;
var
  fileName: string;
  desc: TZUGFeRDInvoiceDescriptor;
begin
  fileName := DocumentationPath(
    'zugferd24-facturx1008\de\Beispiele\4. EXTENDED\EXTENDED_Projektabschlussrechnung\EXTENDED_Projektabschlussrechnung.xml');
  Assert.IsTrue(TFile.Exists(fileName), 'Dokumentations-Fixture nicht gefunden: ' + fileName);

  desc := TZUGFeRDInvoiceDescriptor.Load(fileName);
  try
    Assert.AreEqual<Integer>(4, desc.AdvancePayments.Count);
    Assert.AreEqual<Currency>(2975.0, desc.AdvancePayments[0].PaidAmount.Value);
    Assert.AreEqual(EncodeDate(2025, 6, 7), desc.AdvancePayments[0].FormattedReceivedDateTime.Value);
    Assert.AreEqual<Integer>(1, desc.AdvancePayments[0].IncludedTradeTaxes.Count);
    Assert.AreEqual<Currency>(1900, desc.AdvancePayments[0].IncludedTradeTaxes[0].TaxAmount);
    Assert.AreEqual('R202506-01', desc.AdvancePayments[0].InvoiceSpecifiedReferencedDocument.ID);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestExtendedInvoiceWithIncludedItems;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  lineItem: TZUGFeRDTradeLineItem;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd21\zugferd_2p1_EXTENDED_Warenrechnung-factur-x.xml'));
  try
    desc.TradeLineItems.Clear;

    lineItem := desc._AddTradeLineItem(
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
      {taxPercent=}      19.0
    );
    lineItem.AddIncludedReferencedProduct('Test', ZUGFeRDNullable<Currency>.Create(1), TZUGFeRDQuantityCodes.C62);
    lineItem.AddIncludedReferencedProduct('Test2', ZUGFeRDNullable<Currency>.Create(False), TZUGFeRDQuantityCodes.Unknown);

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<Integer>(1, loadedInvoice.TradeLineItems.Count);
        Assert.AreEqual<Integer>(2, loadedInvoice.TradeLineItems[0].IncludedReferencedProducts.Count);
        Assert.AreEqual<string>('Test', loadedInvoice.TradeLineItems[0].IncludedReferencedProducts[0].Name);
        Assert.AreEqual<Currency>(1, loadedInvoice.TradeLineItems[0].IncludedReferencedProducts[0].UnitQuantity.Value);
        Assert.AreEqual<TZUGFeRDQuantityCodes>(TZUGFeRDQuantityCodes.C62, loadedInvoice.TradeLineItems[0].IncludedReferencedProducts[0].UnitCode.Value);
        Assert.AreEqual('Test2', loadedInvoice.TradeLineItems[0].IncludedReferencedProducts[1].Name);
        Assert.IsFalse(loadedInvoice.TradeLineItems[0].IncludedReferencedProducts[1].UnitQuantity.HasValue);
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

// Roundtrip test for TradeLineItem product identification fields:
// IndustryAssignedID, ModelID, BatchID, BrandName, ModelName
// These fields are Extended profile only (CII 2.3).
procedure TZUGFeRD22Tests.TestTradeLineItemProductFieldsRoundtrip;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  lineItem: TZUGFeRDTradeLineItem;
  loadedLineItem: TZUGFeRDTradeLineItem;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd21\zugferd_2p1_EXTENDED_Warenrechnung-factur-x.xml'));
  try
    desc.TradeLineItems.Clear;

    lineItem := desc._AddTradeLineItem(
      {lineID=}          '1',
      {name=}            'Test Product',
      {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(5.0),
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.H87),
      {unitQuantity=}    nil,
      {grossUnitPrice=}  TZUGFeRDNullableParam<Currency>.Create(5.0),
      {billedQuantity=}  10,
      {lineTotalAmount=} 0,
      {taxType=}         TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
      {categoryCode=}    TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
      {taxPercent=}      19.0,
      {comment=}         '',
      {id=}              nil,
      {sellerAssignedID=} 'SELLER-001'
    );
    lineItem.BuyerAssignedID := 'BUYER-001';
    lineItem.IndustryAssignedID := 'IND-12345';
    lineItem.ModelID := 'MDL-67890';
    lineItem.BatchID := 'BATCH-2025-001';
    lineItem.BrandName := 'TestBrand';
    lineItem.ModelName := 'TestModel Pro';

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<Integer>(1, loadedInvoice.TradeLineItems.Count);
        loadedLineItem := loadedInvoice.TradeLineItems[0];
        Assert.AreEqual('Test Product', loadedLineItem.Name);
        Assert.AreEqual('SELLER-001', loadedLineItem.SellerAssignedID);
        Assert.AreEqual('BUYER-001', loadedLineItem.BuyerAssignedID);
        Assert.AreEqual('IND-12345', loadedLineItem.IndustryAssignedID);
        Assert.AreEqual('MDL-67890', loadedLineItem.ModelID);
        Assert.AreEqual('BATCH-2025-001', loadedLineItem.BatchID);
        Assert.AreEqual('TestBrand', loadedLineItem.BrandName);
        Assert.AreEqual('TestModel Pro', loadedLineItem.ModelName);
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

// Roundtrip test for IncludedReferencedProduct fields:
// GlobalID, SellerAssignedID, BuyerAssignedID, IndustryAssignedID, Description
procedure TZUGFeRD22Tests.TestIncludedReferencedProductFieldsRoundtrip;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  lineItem: TZUGFeRDTradeLineItem;
  loadedLineItem: TZUGFeRDTradeLineItem;
  product1, product2: TZUGFeRDIncludedReferencedProduct;
  inclProd: TZUGFeRDIncludedReferencedProduct;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd21\zugferd_2p1_EXTENDED_Warenrechnung-factur-x.xml'));
  try
    desc.TradeLineItems.Clear;

    lineItem := desc._AddTradeLineItem(
      {lineID=}          '1',
      {name=}            'Main Product',
      {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(20.0),
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.H87),
      {unitQuantity=}    nil,
      {grossUnitPrice=}  TZUGFeRDNullableParam<Currency>.Create(20.0),
      {billedQuantity=}  5,
      {lineTotalAmount=} 0,
      {taxType=}         TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
      {categoryCode=}    TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
      {taxPercent=}      19.0
    );

    // Add included product with all new fields set
    inclProd := TZUGFeRDIncludedReferencedProduct.Create;
    inclProd.GlobalID.Free;
    inclProd.GlobalID := TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.EAN, '4012345999999');
    inclProd.SellerAssignedID := 'SEL-REF-001';
    inclProd.BuyerAssignedID := 'BUY-REF-001';
    inclProd.IndustryAssignedID := 'IND-REF-001';
    inclProd.Name := 'Included Part A';
    inclProd.Description := 'Description of included part A';
    inclProd.UnitQuantity := ZUGFeRDNullable<Currency>.Create(2);
    inclProd.UnitCode := TZUGFeRDQuantityCodes.C62;
    lineItem.IncludedReferencedProducts.Add(inclProd);

    // Add a second included product with minimal fields
    inclProd := TZUGFeRDIncludedReferencedProduct.Create;
    inclProd.Name := 'Included Part B';
    inclProd.Description := 'Description of included part B';
    lineItem.IncludedReferencedProducts.Add(inclProd);

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<Integer>(1, loadedInvoice.TradeLineItems.Count);
        loadedLineItem := loadedInvoice.TradeLineItems[0];
        Assert.AreEqual<Integer>(2, loadedLineItem.IncludedReferencedProducts.Count);

        // Verify first included product (all fields)
        product1 := loadedLineItem.IncludedReferencedProducts[0];
        Assert.AreEqual<TZUGFeRDGlobalIDSchemeIdentifiers>(TZUGFeRDGlobalIDSchemeIdentifiers.EAN, product1.GlobalID.SchemeID);
        Assert.AreEqual('4012345999999', product1.GlobalID.ID);
        Assert.AreEqual('SEL-REF-001', product1.SellerAssignedID);
        Assert.AreEqual('BUY-REF-001', product1.BuyerAssignedID);
        Assert.AreEqual('IND-REF-001', product1.IndustryAssignedID);
        Assert.AreEqual('Included Part A', product1.Name);
        Assert.AreEqual('Description of included part A', product1.Description);
        Assert.AreEqual<Currency>(2, product1.UnitQuantity.Value);
        Assert.AreEqual<TZUGFeRDQuantityCodes>(TZUGFeRDQuantityCodes.C62, product1.UnitCode.Value);

        // Verify second included product (minimal fields)
        product2 := loadedLineItem.IncludedReferencedProducts[1];
        Assert.AreEqual('Included Part B', product2.Name);
        Assert.AreEqual('Description of included part B', product2.Description);
        Assert.AreEqual('', product2.SellerAssignedID);
        Assert.AreEqual('', product2.BuyerAssignedID);
        Assert.AreEqual('', product2.IndustryAssignedID);
        Assert.IsFalse(product2.UnitQuantity.HasValue);
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

procedure TZUGFeRD22Tests.TestReferenceEReportingFacturXInvoice;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd21\zugferd_2p1_EREPORTING-factur-x.xml'));
  try
    Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.EReporting, desc.Profile);
    Assert.AreEqual<TZUGFeRDInvoiceType>(TZUGFeRDInvoiceType.Invoice, desc.Type_);
    Assert.AreEqual('471102', desc.InvoiceNo);
    Assert.AreEqual<Integer>(0, desc.TradeLineItems.Count);
    Assert.IsFalse(desc.LineTotalAmount.HasValue);
    Assert.AreEqual<Currency>(198.0, desc.TaxBasisAmount.Value);
    Assert.IsFalse(desc.IsTest);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestReferenceBasicFacturXInvoice;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd21\zugferd_2p1_BASIC_Einfach-factur-x.xml'));
  try
    Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.Basic, desc.Profile);
    Assert.AreEqual<TZUGFeRDInvoiceType>(TZUGFeRDInvoiceType.Invoice, desc.Type_);
    Assert.AreEqual('471102', desc.InvoiceNo);
    Assert.AreEqual<Integer>(1, desc.TradeLineItems.Count);
    Assert.AreEqual<Currency>(198.0, desc.LineTotalAmount.Value);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestStoringReferenceBasicFacturXInvoice;
var
  originalDesc, desc: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  i: Integer;
begin
  originalDesc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd21\zugferd_2p1_BASIC_Einfach-factur-x.xml'));
  try
    Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.Basic, originalDesc.Profile);
    Assert.AreEqual<TZUGFeRDInvoiceType>(TZUGFeRDInvoiceType.Invoice, originalDesc.Type_);
    Assert.AreEqual('471102', originalDesc.InvoiceNo);
    Assert.AreEqual<Integer>(1, originalDesc.TradeLineItems.Count);
    for i := 0 to originalDesc.TradeLineItems.Count - 1 do
    begin
      Assert.IsFalse(originalDesc.TradeLineItems[i].BillingPeriodStart.HasValue);
      Assert.IsFalse(originalDesc.TradeLineItems[i].BillingPeriodEnd.HasValue);
      Assert.AreEqual<Integer>(0, originalDesc.TradeLineItems[i].ApplicableProductCharacteristics.Count);
    end;
    Assert.AreEqual<Currency>(198.0, originalDesc.LineTotalAmount.Value);
    Assert.AreEqual<Currency>(37.62, originalDesc.Taxes[0].TaxAmount);
    Assert.AreEqual<Currency>(19.0, originalDesc.Taxes[0].Percent);
    originalDesc.IsTest := False;

    ms := TMemoryStream.Create;
    try
      originalDesc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Basic);
      ms.Position := 0;

      desc := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.Basic, desc.Profile);
        Assert.AreEqual<TZUGFeRDInvoiceType>(TZUGFeRDInvoiceType.Invoice, desc.Type_);
        Assert.AreEqual('471102', desc.InvoiceNo);
        Assert.AreEqual<Integer>(1, desc.TradeLineItems.Count);
        for i := 0 to desc.TradeLineItems.Count - 1 do
        begin
          Assert.IsFalse(desc.TradeLineItems[i].BillingPeriodStart.HasValue);
          Assert.IsFalse(desc.TradeLineItems[i].BillingPeriodEnd.HasValue);
          Assert.AreEqual<Integer>(0, desc.TradeLineItems[i].ApplicableProductCharacteristics.Count);
        end;
        Assert.AreEqual<Currency>(198.0, desc.LineTotalAmount.Value);
        Assert.AreEqual<Currency>(37.62, desc.Taxes[0].TaxAmount);
        Assert.AreEqual<Currency>(19.0, desc.Taxes[0].Percent);
      finally
        desc.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    originalDesc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestReferenceBasicWLInvoice;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd21\zugferd_2p1_BASIC-WL_Einfach-factur-x.xml'));
  try
    Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.BasicWL, desc.Profile);
    Assert.AreEqual<TZUGFeRDInvoiceType>(TZUGFeRDInvoiceType.Invoice, desc.Type_);
    Assert.AreEqual('471102', desc.InvoiceNo);
    Assert.AreEqual<Integer>(0, desc.TradeLineItems.Count);
    Assert.AreEqual<Currency>(624.90, desc.LineTotalAmount.Value);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestReferenceExtendedInvoice;
var
  desc: TZUGFeRDInvoiceDescriptor;
  tradeAllowances: TArray<TZUGFeRDTradeAllowance>;
  allowance: TZUGFeRDTradeAllowance;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DocumentationPath(
    'zugferd24-facturx1008\de\Beispiele\4. EXTENDED\EXTENDED_Warenrechnung\EXTENDED_Warenrechnung.xml'));
  try
    Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.Extended, desc.Profile);
    Assert.AreEqual<TZUGFeRDInvoiceType>(TZUGFeRDInvoiceType.Invoice, desc.Type_);
    Assert.AreEqual('R87654321012345', desc.InvoiceNo);
    Assert.AreEqual<Integer>(6, desc.TradeLineItems.Count);
    Assert.AreEqual<Currency>(457.20, desc.LineTotalAmount.Value);

    tradeAllowances := desc.GetTradeAllowances;
    for allowance in tradeAllowances do
    begin
      Assert.AreEqual<TZUGFeRDTaxTypes>(TZUGFeRDTaxTypes.VAT, allowance.Tax.TypeCode);
      Assert.AreEqual<TZUGFeRDTaxCategoryCodes>(TZUGFeRDTaxCategoryCodes.S, allowance.Tax.CategoryCode);
    end;

    Assert.AreEqual<Integer>(4, Length(tradeAllowances));
    Assert.AreEqual<Currency>(19, tradeAllowances[0].Tax.Percent);
    Assert.AreEqual<Currency>(7, tradeAllowances[1].Tax.Percent);
    Assert.AreEqual<Currency>(19, tradeAllowances[2].Tax.Percent);
    Assert.AreEqual<Currency>(7, tradeAllowances[3].Tax.Percent);

    Assert.AreEqual<Integer>(1, desc.ServiceCharges.Count);
    Assert.AreEqual<TZUGFeRDTaxTypes>(TZUGFeRDTaxTypes.VAT, desc.ServiceCharges[0].Tax.TypeCode);
    Assert.AreEqual<TZUGFeRDTaxCategoryCodes>(TZUGFeRDTaxCategoryCodes.S, desc.ServiceCharges[0].Tax.CategoryCode);
    Assert.AreEqual<Currency>(19, desc.ServiceCharges[0].Tax.Percent);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestReferenceMinimumInvoice;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd21\zugferd_2p1_MINIMUM_Rechnung-factur-x.xml'));
  try
    Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.Minimum, desc.Profile);
    Assert.AreEqual<TZUGFeRDInvoiceType>(TZUGFeRDInvoiceType.Invoice, desc.Type_);
    Assert.AreEqual('471102', desc.InvoiceNo);
    Assert.AreEqual<Integer>(0, desc.TradeLineItems.Count);
    Assert.IsFalse(desc.LineTotalAmount.HasValue);
    Assert.AreEqual<Currency>(198.0, desc.TaxBasisAmount.Value);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestReferenceXRechnung1CII;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('xRechnung\xRechnung CII.xml'));
  try
    Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.XRechnung1, desc.Profile);
    Assert.AreEqual<TZUGFeRDInvoiceType>(TZUGFeRDInvoiceType.Invoice, desc.Type_);
    Assert.AreEqual('0815-99-1-a', desc.InvoiceNo);
    Assert.AreEqual<Integer>(2, desc.TradeLineItems.Count);
    Assert.AreEqual<Currency>(1445.98, desc.LineTotalAmount.Value);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestElectronicAddress;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.SetSellerElectronicAddress('DE123456789', TZUGFeRDElectronicAddressSchemeIdentifiers.GermanyVatNumber);
    desc.SetBuyerElectronicAddress('LU987654321', TZUGFeRDElectronicAddressSchemeIdentifiers.LuxemburgVatNumber);

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung);
      ms.Position := 0;
      Assert.AreEqual('DE123456789', desc.SellerElectronicAddress.Address);
      Assert.AreEqual<TZUGFeRDElectronicAddressSchemeIdentifiers>(TZUGFeRDElectronicAddressSchemeIdentifiers.GermanyVatNumber, desc.SellerElectronicAddress.ElectronicAddressSchemeID);
      Assert.AreEqual('LU987654321', desc.BuyerElectronicAddress.Address);
      Assert.AreEqual<TZUGFeRDElectronicAddressSchemeIdentifiers>(TZUGFeRDElectronicAddressSchemeIdentifiers.LuxemburgVatNumber, desc.BuyerElectronicAddress.ElectronicAddressSchemeID);

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual('DE123456789', loadedInvoice.SellerElectronicAddress.Address);
        Assert.AreEqual<TZUGFeRDElectronicAddressSchemeIdentifiers>(TZUGFeRDElectronicAddressSchemeIdentifiers.GermanyVatNumber, loadedInvoice.SellerElectronicAddress.ElectronicAddressSchemeID);
        Assert.AreEqual('LU987654321', loadedInvoice.BuyerElectronicAddress.Address);
        Assert.AreEqual<TZUGFeRDElectronicAddressSchemeIdentifiers>(TZUGFeRDElectronicAddressSchemeIdentifiers.LuxemburgVatNumber, loadedInvoice.BuyerElectronicAddress.ElectronicAddressSchemeID);
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

procedure TZUGFeRD22Tests.TestMinimumInvoice;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.Invoicee := TZUGFeRDParty.Create;
    desc.Invoicee.Name := 'Invoicee';
    desc.Seller := TZUGFeRDParty.Create;
    desc.Seller.Name := 'Seller';
    desc.Seller.SpecifiedLegalOrganization := TZUGFeRDLegalOrganization.Create;
    desc.Seller.SpecifiedLegalOrganization.TradingBusinessName := 'Trading business name for seller party';
    desc.TaxBasisAmount := 73;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Minimum);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNull(loadedInvoice.Invoicee);
        Assert.IsNotNull(loadedInvoice.Seller);
        Assert.IsNull(loadedInvoice.Seller.SpecifiedLegalOrganization);
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

procedure TZUGFeRD22Tests.TestInvoiceWithAttachmentXRechnung;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms, dataStream: TMemoryStream;
  filename: string;
  data: TBytes;
  doc: TZUGFeRDAdditionalReferencedDocument;
  i: Integer;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    filename := 'myrandomdata.bin';
    SetLength(data, 32768);
    for i := 0 to Length(data) - 1 do
      data[i] := Random(256);

    dataStream := TMemoryStream.Create;
    try
      dataStream.WriteBuffer(data[0], Length(data));
      dataStream.Position := 0;

      desc.AddAdditionalReferencedDocument(
        {id=}       'My-File',
        {typeCode=} TZUGFeRDAdditionalReferencedDocumentTypeCode.ReferenceDocument,
        {issueDateTime=} nil,
        {name=}     'Ausf'+#$00FC+'hrbare Datei',
        {referenceTypeCode=} nil,
        {attachmentBinaryObject=} dataStream,
        {filename=} filename
      );
    finally
      dataStream.Free;
    end;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<Integer>(1, loadedInvoice.AdditionalReferencedDocuments.Count);

        for i := 0 to loadedInvoice.AdditionalReferencedDocuments.Count - 1 do
        begin
          doc := loadedInvoice.AdditionalReferencedDocuments[i];
          if doc.ID = 'My-File' then
          begin
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

procedure TZUGFeRD22Tests.TestInvoiceWithAttachmentExtended;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms, dataStream: TMemoryStream;
  filename: string;
  data: TBytes;
  doc: TZUGFeRDAdditionalReferencedDocument;
  i: Integer;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    filename := 'myrandomdata.bin';
    SetLength(data, 32768);
    for i := 0 to Length(data) - 1 do
      data[i] := Random(256);

    dataStream := TMemoryStream.Create;
    try
      dataStream.WriteBuffer(data[0], Length(data));
      dataStream.Position := 0;

      desc.AddAdditionalReferencedDocument(
        {id=}       'My-File',
        {typeCode=} TZUGFeRDAdditionalReferencedDocumentTypeCode.ReferenceDocument,
        {issueDateTime=} nil,
        {name=}     'Ausf'+#$00FC+'hrbare Datei',
        {referenceTypeCode=} nil,
        {attachmentBinaryObject=} dataStream,
        {filename=} filename
      );
    finally
      dataStream.Free;
    end;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<Integer>(1, loadedInvoice.AdditionalReferencedDocuments.Count);

        for i := 0 to loadedInvoice.AdditionalReferencedDocuments.Count - 1 do
        begin
          doc := loadedInvoice.AdditionalReferencedDocuments[i];
          if doc.ID = 'My-File' then
          begin
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

procedure TZUGFeRD22Tests.TestInvoiceWithAttachmentComfort;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms, dataStream: TMemoryStream;
  filename: string;
  data: TBytes;
  doc: TZUGFeRDAdditionalReferencedDocument;
  i: Integer;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    filename := 'myrandomdata.bin';
    SetLength(data, 32768);
    for i := 0 to Length(data) - 1 do
      data[i] := Random(256);

    dataStream := TMemoryStream.Create;
    try
      dataStream.WriteBuffer(data[0], Length(data));
      dataStream.Position := 0;

      desc.AddAdditionalReferencedDocument(
        {id=}       'My-File',
        {typeCode=} TZUGFeRDAdditionalReferencedDocumentTypeCode.ReferenceDocument,
        {issueDateTime=} nil,
        {name=}     'Ausf'+#$00FC+'hrbare Datei',
        {referenceTypeCode=} nil,
        {attachmentBinaryObject=} dataStream,
        {filename=} filename
      );
    finally
      dataStream.Free;
    end;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Comfort);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<Integer>(1, loadedInvoice.AdditionalReferencedDocuments.Count);

        for i := 0 to loadedInvoice.AdditionalReferencedDocuments.Count - 1 do
        begin
          doc := loadedInvoice.AdditionalReferencedDocuments[i];
          if doc.ID = 'My-File' then
          begin
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

procedure TZUGFeRD22Tests.TestInvoiceWithAttachmentBasic;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms, dataStream: TMemoryStream;
  filename: string;
  data: TBytes;
  i: Integer;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    filename := 'myrandomdata.bin';
    SetLength(data, 32768);
    for i := 0 to Length(data) - 1 do
      data[i] := Random(256);

    dataStream := TMemoryStream.Create;
    try
      dataStream.WriteBuffer(data[0], Length(data));
      dataStream.Position := 0;

      desc.AddAdditionalReferencedDocument(
        {id=}       'My-File',
        {typeCode=} TZUGFeRDAdditionalReferencedDocumentTypeCode.ReferenceDocument,
        {issueDateTime=} nil,
        {name=}     'Ausf'+#$00FC+'hrbare Datei',
        {referenceTypeCode=} nil,
        {attachmentBinaryObject=} dataStream,
        {filename=} filename
      );
    finally
      dataStream.Free;
    end;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Basic);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<Integer>(0, loadedInvoice.AdditionalReferencedDocuments.Count);
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

procedure TZUGFeRD22Tests.TestXRechnung1;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung1);
      ms.Position := 0;
      Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.XRechnung1, desc.Profile);

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.XRechnung1, loadedInvoice.Profile);
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

procedure TZUGFeRD22Tests.TestXRechnung2;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung);
      Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.XRechnung, desc.Profile);

      ms.Position := 0;
      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.XRechnung, loadedInvoice.Profile);
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

procedure TZUGFeRD22Tests.TestCreateInvoice_WithProfileEReporting;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.EReporting);
      Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.EReporting, desc.Profile);

      ms.Position := 0;
      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.EReporting, loadedInvoice.Profile);
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

procedure TZUGFeRD22Tests.TestBuyerOrderReferencedDocumentWithExtended;
var
  uuid: string;
  orderDate: TDateTime;
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  uuid := TGUID.NewGuid.ToString;
  orderDate := Date;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.SetBuyerOrderReferenceDocument(uuid, TZUGFeRDNullableParam<TDateTime>.Create(orderDate));

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;
      Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.Extended, desc.Profile);

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual(uuid, loadedInvoice.OrderNo);
        Assert.AreEqual<TDateTime>(orderDate, loadedInvoice.OrderDate.Value);
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

procedure TZUGFeRD22Tests.TestBuyerOrderReferencedDocumentWithXRechnung;
var
  uuid: string;
  orderDate: TDateTime;
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  uuid := TGUID.NewGuid.ToString;
  orderDate := Date;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.SetBuyerOrderReferenceDocument(uuid, TZUGFeRDNullableParam<TDateTime>.Create(orderDate));

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung);
      ms.Position := 0;
      Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.XRechnung, desc.Profile);

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual(uuid, loadedInvoice.OrderNo);
        Assert.IsFalse(loadedInvoice.OrderDate.HasValue);
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

procedure TZUGFeRD22Tests.TestContractReferencedDocumentWithXRechnung;
var
  uuid: string;
  issueDateTime: TDateTime;
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  uuid := TGUID.NewGuid.ToString;
  issueDateTime := Date;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.ContractReferencedDocument := TZUGFeRDContractReferencedDocument.Create;
    desc.ContractReferencedDocument.ID := uuid;
    desc.ContractReferencedDocument.IssueDateTime := issueDateTime;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung);
      ms.Position := 0;
      Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.XRechnung, desc.Profile);

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual(uuid, loadedInvoice.ContractReferencedDocument.ID);
        Assert.IsFalse(loadedInvoice.ContractReferencedDocument.IssueDateTime.HasValue);
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

procedure TZUGFeRD22Tests.TestContractReferencedDocumentWithExtended;
var
  uuid: string;
  issueDateTime: TDateTime;
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  uuid := TGUID.NewGuid.ToString;
  issueDateTime := Date;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.ContractReferencedDocument := TZUGFeRDContractReferencedDocument.Create;
    desc.ContractReferencedDocument.ID := uuid;
    desc.ContractReferencedDocument.IssueDateTime := issueDateTime;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;
      Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.Extended, desc.Profile);

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual(uuid, loadedInvoice.ContractReferencedDocument.ID);
        Assert.AreEqual<TDateTime>(issueDateTime, loadedInvoice.ContractReferencedDocument.IssueDateTime.Value);
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

procedure TZUGFeRD22Tests.TestTotalRoundingExtended;
var
  uuid: string;
  issueDateTime: TDateTime;
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  msExtended, msBasic: TMemoryStream;
begin
  uuid := TGUID.NewGuid.ToString;
  issueDateTime := Date;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.ContractReferencedDocument := TZUGFeRDContractReferencedDocument.Create;
    desc.ContractReferencedDocument.ID := uuid;
    desc.ContractReferencedDocument.IssueDateTime := issueDateTime;
    desc.SetTotals(1.99, 0, 0, 0, 0, 2, 0, 2, 0.01);

    msExtended := TMemoryStream.Create;
    try
      desc.Save(msExtended, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
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
      desc.Save(msBasic, TZUGFeRDVersion.Version23);
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

procedure TZUGFeRD22Tests.TestTotalRoundingXRechnung;
var
  uuid: string;
  issueDateTime: TDateTime;
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  msExtended, msBasic: TMemoryStream;
begin
  uuid := TGUID.NewGuid.ToString;
  issueDateTime := Date;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.ContractReferencedDocument := TZUGFeRDContractReferencedDocument.Create;
    desc.ContractReferencedDocument.ID := uuid;
    desc.ContractReferencedDocument.IssueDateTime := issueDateTime;
    desc.SetTotals(1.99, 0, 0, 0, 0, 2, 0, 2, 0.01);

    msExtended := TMemoryStream.Create;
    try
      desc.Save(msExtended, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung);
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
      desc.Save(msBasic, TZUGFeRDVersion.Version23);
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

procedure TZUGFeRD22Tests.TestMissingPropertiesAreNull;
var
  desc: TZUGFeRDInvoiceDescriptor;
  i: Integer;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd21\zugferd_2p1_BASIC_Einfach-factur-x.xml'));
  try
    for i := 0 to desc.TradeLineItems.Count - 1 do
    begin
      Assert.IsFalse(desc.TradeLineItems[i].BillingPeriodStart.HasValue);
      Assert.IsFalse(desc.TradeLineItems[i].BillingPeriodEnd.HasValue);
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestMissingPropertiesAreEmpty;
var
  desc: TZUGFeRDInvoiceDescriptor;
  i: Integer;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd21\zugferd_2p1_BASIC_Einfach-factur-x.xml'));
  try
    for i := 0 to desc.TradeLineItems.Count - 1 do
      Assert.AreEqual<Integer>(0, desc.TradeLineItems[i].ApplicableProductCharacteristics.Count);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestReadTradeLineBillingPeriod;
var
  desc: TZUGFeRDInvoiceDescriptor;
  tradeLineItem: TZUGFeRDTradeLineItem;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('xRechnung\xrechnung with trade line settlement data.xml'));
  try
    Assert.AreEqual<Integer>(1, desc.TradeLineItems.Count);
    tradeLineItem := desc.TradeLineItems[0];
    Assert.AreEqual<TDateTime>(EncodeDate(2021, 01, 01), tradeLineItem.BillingPeriodStart.Value);
    Assert.AreEqual<TDateTime>(EncodeDate(2021, 01, 31), tradeLineItem.BillingPeriodEnd.Value);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestReadTradeLineLineID;
var
  desc: TZUGFeRDInvoiceDescriptor;
  tradeLineItem: TZUGFeRDTradeLineItem;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('xRechnung\xrechnung with trade line settlement data.xml'));
  try
    Assert.AreEqual<Integer>(1, desc.TradeLineItems.Count);
    tradeLineItem := desc.TradeLineItems[0];
    Assert.AreEqual('2', tradeLineItem.AssociatedDocument.LineID);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestReadTradeLineProductCharacteristics;
var
  desc: TZUGFeRDInvoiceDescriptor;
  tradeLineItem: TZUGFeRDTradeLineItem;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('xRechnung\xrechnung with trade line settlement data.xml'));
  try
    Assert.AreEqual<Integer>(1, desc.TradeLineItems.Count);
    tradeLineItem := desc.TradeLineItems[0];

    Assert.AreEqual('METER_LOCATION', tradeLineItem.ApplicableProductCharacteristics[0].Description);
    Assert.AreEqual('DE213410213', tradeLineItem.ApplicableProductCharacteristics[0].Value);

    Assert.AreEqual('METER_NUMBER', tradeLineItem.ApplicableProductCharacteristics[1].Description);
    Assert.AreEqual('123', tradeLineItem.ApplicableProductCharacteristics[1].Value);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestWriteTradeLineProductCharacteristics;
var
  originalDesc, invoiceDescriptor: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  item0, item1: TZUGFeRDTradeLineItem;
begin
  originalDesc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('xRechnung\xrechnung with trade line settlement empty.xml'));
  try
    item0 := originalDesc.AddTradeLineItem(
      {name=}            '',
      {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(20.0),
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62),
      {unitQuantity=}    nil,
      {grossUnitPrice=}  nil,
      {billedQuantity=}  0,
      {lineTotalAmount=} 0
    );
    item0.ApplicableProductCharacteristics.Add(TZUGFeRDApplicableProductCharacteristic.Create);
    item0.ApplicableProductCharacteristics.Last.Description := 'Description_1_1';
    item0.ApplicableProductCharacteristics.Last.Value := 'Value_1_1';
    item0.ApplicableProductCharacteristics.Add(TZUGFeRDApplicableProductCharacteristic.Create);
    item0.ApplicableProductCharacteristics.Last.Description := 'Description_1_2';
    item0.ApplicableProductCharacteristics.Last.Value := 'Value_1_2';

    item1 := originalDesc.AddTradeLineItem(
      {name=}            '',
      {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(30.0),
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62),
      {unitQuantity=}    nil,
      {grossUnitPrice=}  nil,
      {billedQuantity=}  0,
      {lineTotalAmount=} 0
    );
    item1.ApplicableProductCharacteristics.Add(TZUGFeRDApplicableProductCharacteristic.Create);
    item1.ApplicableProductCharacteristics.Last.Description := 'Description_2_1';
    item1.ApplicableProductCharacteristics.Last.Value := 'Value_2_1';
    item1.ApplicableProductCharacteristics.Add(TZUGFeRDApplicableProductCharacteristic.Create);
    item1.ApplicableProductCharacteristics.Last.Description := 'Description_2_2';
    item1.ApplicableProductCharacteristics.Last.Value := 'Value_2_2';

    originalDesc.IsTest := False;

    ms := TMemoryStream.Create;
    try
      originalDesc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Basic);
      ms.Position := 0;

      invoiceDescriptor := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual('Description_1_1', invoiceDescriptor.TradeLineItems[0].ApplicableProductCharacteristics[0].Description);
        Assert.AreEqual('Value_1_1', invoiceDescriptor.TradeLineItems[0].ApplicableProductCharacteristics[0].Value);
        Assert.AreEqual('Description_1_2', invoiceDescriptor.TradeLineItems[0].ApplicableProductCharacteristics[1].Description);
        Assert.AreEqual('Value_1_2', invoiceDescriptor.TradeLineItems[0].ApplicableProductCharacteristics[1].Value);

        Assert.AreEqual('Description_2_1', invoiceDescriptor.TradeLineItems[1].ApplicableProductCharacteristics[0].Description);
        Assert.AreEqual('Value_2_1', invoiceDescriptor.TradeLineItems[1].ApplicableProductCharacteristics[0].Value);
        Assert.AreEqual('Description_2_2', invoiceDescriptor.TradeLineItems[1].ApplicableProductCharacteristics[1].Description);
        Assert.AreEqual('Value_2_2', invoiceDescriptor.TradeLineItems[1].ApplicableProductCharacteristics[1].Value);
      finally
        invoiceDescriptor.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    originalDesc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestWriteTradeLineBillingPeriod;
var
  originalDesc, invoiceDescriptor: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  originalDesc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('xRechnung\xrechnung with trade line settlement empty.xml'));
  try
    originalDesc.AddTradeLineItem(
      {name=}            '',
      {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(0),
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62),
      {unitQuantity=}    nil,
      {grossUnitPrice=}  nil,
      {billedQuantity=}  0,
      {lineTotalAmount=} 0,
      {taxType=}         nil,
      {categoryCode=}    nil,
      {taxPercent=}      0,
      {comment=}         '',
      {id=}              nil,
      {sellerAssignedID=} '',
      {buyerAssignedID=} '',
      {deliveryNoteID=}  '',
      {deliveryNoteDate=} nil,
      {buyerOrderLineID=} '',
      {buyerOrderID=}    '',
      {buyerOrderDate=}  nil,
      {billingPeriodStart=} TZUGFeRDNullableParam<TDateTime>.Create(EncodeDate(2020, 1, 1)),
      {billingPeriodEnd=}   TZUGFeRDNullableParam<TDateTime>.Create(EncodeDate(2021, 1, 1))
    );

    originalDesc.AddTradeLineItem(
      {name=}            '',
      {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(0),
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62),
      {unitQuantity=}    nil,
      {grossUnitPrice=}  nil,
      {billedQuantity=}  0,
      {lineTotalAmount=} 0,
      {taxType=}         nil,
      {categoryCode=}    nil,
      {taxPercent=}      0,
      {comment=}         '',
      {id=}              nil,
      {sellerAssignedID=} '',
      {buyerAssignedID=} '',
      {deliveryNoteID=}  '',
      {deliveryNoteDate=} nil,
      {buyerOrderLineID=} '',
      {buyerOrderID=}    '',
      {buyerOrderDate=}  nil,
      {billingPeriodStart=} TZUGFeRDNullableParam<TDateTime>.Create(EncodeDate(2021, 1, 1)),
      {billingPeriodEnd=}   TZUGFeRDNullableParam<TDateTime>.Create(EncodeDate(2022, 1, 1))
    );

    originalDesc.IsTest := False;

    ms := TMemoryStream.Create;
    try
      originalDesc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Basic);
      ms.Position := 0;

      invoiceDescriptor := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<TDateTime>(EncodeDate(2020, 1, 1), invoiceDescriptor.TradeLineItems[0].BillingPeriodStart.Value);
        Assert.AreEqual<TDateTime>(EncodeDate(2021, 1, 1), invoiceDescriptor.TradeLineItems[0].BillingPeriodEnd.Value);
        Assert.AreEqual<TDateTime>(EncodeDate(2021, 1, 1), invoiceDescriptor.TradeLineItems[1].BillingPeriodStart.Value);
        Assert.AreEqual<TDateTime>(EncodeDate(2022, 1, 1), invoiceDescriptor.TradeLineItems[1].BillingPeriodEnd.Value);
      finally
        invoiceDescriptor.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    originalDesc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestWriteTradeLineBilledQuantity;
var
  originalDesc, invoiceDescriptor: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  originalDesc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('xRechnung\xrechnung with trade line settlement empty.xml'));
  try
    originalDesc.AddTradeLineItem(
      {name=}            '',
      {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(0),
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62),
      {unitQuantity=}    nil,
      {grossUnitPrice=}  nil,
      {billedQuantity=}  10,
      {lineTotalAmount=} 0
    );
    originalDesc.IsTest := False;

    ms := TMemoryStream.Create;
    try
      originalDesc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Basic);
      ms.Position := 0;

      invoiceDescriptor := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<Currency>(10, invoiceDescriptor.TradeLineItems[0].BilledQuantity);
      finally
        invoiceDescriptor.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    originalDesc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestWriteTradeLineChargeFreePackage;
var
  originalDesc, invoiceDescriptor: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  tli: TZUGFeRDTradeLineItem;
begin
  originalDesc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('xRechnung\xrechnung with trade line settlement empty.xml'));
  try
    tli := originalDesc.AddTradeLineItem(
      {name=}            '',
      {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(0),
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62),
      {unitQuantity=}    nil,
      {grossUnitPrice=}  nil,
      {billedQuantity=}  0,
      {lineTotalAmount=} 0
    );
    tli.ChargeFreeQuantity := 10;
    tli.ChargeFreeUnitCode := TZUGFeRDQuantityCodes.C62;
    tli.PackageQuantity := 20;
    tli.PackageUnitCode := TZUGFeRDQuantityCodes.C62;
    originalDesc.IsTest := False;

    ms := TMemoryStream.Create;
    try
      originalDesc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      invoiceDescriptor := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<Currency>(10, invoiceDescriptor.TradeLineItems[0].ChargeFreeQuantity.Value);
        Assert.AreEqual<TZUGFeRDQuantityCodes>(TZUGFeRDQuantityCodes.C62, invoiceDescriptor.TradeLineItems[0].ChargeFreeUnitCode.Value);
        Assert.AreEqual<Currency>(20, invoiceDescriptor.TradeLineItems[0].PackageQuantity.Value);
        Assert.AreEqual<TZUGFeRDQuantityCodes>(TZUGFeRDQuantityCodes.C62, invoiceDescriptor.TradeLineItems[0].PackageUnitCode.Value);
      finally
        invoiceDescriptor.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    originalDesc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestWriteTradeLineNetUnitPrice;
var
  originalDesc, invoiceDescriptor: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  originalDesc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('xRechnung\xrechnung with trade line settlement empty.xml'));
  try
    originalDesc.AddTradeLineItem(
      {name=}            '',
      {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(25),
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62),
      {unitQuantity=}    nil,
      {grossUnitPrice=}  nil,
      {billedQuantity=}  0,
      {lineTotalAmount=} 0
    );
    originalDesc.IsTest := False;

    ms := TMemoryStream.Create;
    try
      originalDesc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Basic);
      ms.Position := 0;

      invoiceDescriptor := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<Currency>(25, invoiceDescriptor.TradeLineItems[0].NetUnitPrice.Value);
      finally
        invoiceDescriptor.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    originalDesc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestWriteTradeLineLineID;
var
  originalDesc, invoiceDescriptor: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  originalDesc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('xRechnung\xrechnung with trade line settlement empty.xml'));
  try
    originalDesc.TradeLineItems.Clear;

    originalDesc.AddTradeLineCommentItem({lineID=} '2', {comment=} 'Comment_2', {name=} '', {sellerAssignedId=} '');
    originalDesc.AddTradeLineCommentItem({lineID=} '3', {comment=} 'Comment_3', {name=} '', {sellerAssignedId=} '');
    originalDesc.IsTest := False;

    ms := TMemoryStream.Create;
    try
      originalDesc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Basic);
      ms.Position := 0;

      invoiceDescriptor := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual('2', invoiceDescriptor.TradeLineItems[0].AssociatedDocument.LineID);
        Assert.AreEqual('3', invoiceDescriptor.TradeLineItems[1].AssociatedDocument.LineID);
      finally
        invoiceDescriptor.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    originalDesc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestLoadingSepaPreNotification;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd21\zugferd_2p1_EN16931_SEPA_Prenotification.xml'));
  try
    Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.Comfort, desc.Profile);
    Assert.AreEqual<TZUGFeRDInvoiceType>(TZUGFeRDInvoiceType.Invoice, desc.Type_);

    Assert.AreEqual('DE98ZZZ09999999999', desc.PaymentMeans.SEPACreditorIdentifier);
    Assert.AreEqual('REF A-123', desc.PaymentMeans.SEPAMandateReference);
    Assert.AreEqual<TZUGFeRDPaymentMeansTypeCodes>(TZUGFeRDPaymentMeansTypeCodes.SEPADirectDebit, desc.PaymentMeans.TypeCode.Value);
    Assert.AreEqual<Integer>(1, desc.DebitorBankAccounts.Count);
    Assert.AreEqual('DE21860000000086001055', desc.DebitorBankAccounts[0].IBAN);

    Assert.IsTrue(desc.PaymentTermsList.Count > 0, 'PaymentTermsList should not be empty');
    Assert.AreEqual(
      'Der Betrag in H'+#$00F6+'he von EUR 529,87 wird am 20.03.2018 von Ihrem Konto per SEPA-Lastschrift eingezogen.',
      Trim(desc.PaymentTermsList[0].Description));
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestStoringSepaPreNotification;
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
      {sellerAssignedID=} 'TB100A4'
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
    d.SetTotals(473.00, 0.00, 0.00, 473.00, 56.87, 529.87, 0.00, 529.87);

    d.AddSellerTaxRegistration('201/113/40209', TZUGFeRDTaxRegistrationSchemeID.FC);
    d.AddSellerTaxRegistration('DE123456789', TZUGFeRDTaxRegistrationSchemeID.VA);

    d.AddApplicableTradeTax(275.00 / 100 * 7.00, 275.00, 7.00, TZUGFeRDTaxTypes.VAT, TZUGFeRDTaxCategoryCodes.S);
    d.AddApplicableTradeTax(198.00 / 100 * 19.00, 198.00, 19.00, TZUGFeRDTaxTypes.VAT, TZUGFeRDTaxCategoryCodes.S);

    ms := TMemoryStream.Create;
    try
      d.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Comfort);
      ms.Position := 0;

      d2 := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<TZUGFeRDCurrencyCodes>(TZUGFeRDCurrencyCodes.EUR, d2.Currency);
        Assert.AreEqual('DE98ZZZ09999999999', d2.PaymentMeans.SEPACreditorIdentifier);
        Assert.AreEqual('REF A-123', d2.PaymentMeans.SEPAMandateReference);
        Assert.AreEqual<Integer>(1, d2.DebitorBankAccounts.Count);
        Assert.AreEqual('DE21860000000086001055', d2.DebitorBankAccounts[0].IBAN);
        Assert.IsTrue(d.Seller.SpecifiedLegalOrganization.ID.SchemeID.HasValue);
        Assert.AreEqual<TZUGFeRDGlobalIDSchemeIdentifiers>(TZUGFeRDGlobalIDSchemeIdentifiers.GLN, d.Seller.SpecifiedLegalOrganization.ID.SchemeID.Value);
        Assert.AreEqual('4000001123452', d.Seller.SpecifiedLegalOrganization.ID.ID);
        Assert.AreEqual('Lieferant GmbH', d.Seller.SpecifiedLegalOrganization.TradingBusinessName);
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

procedure TZUGFeRD22Tests.TestValidTaxTypes;
var
  invoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  i: Integer;
begin
  invoice := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    for i := 0 to invoice.TradeLineItems.Count - 1 do
      invoice.TradeLineItems[i].TaxType := TZUGFeRDTaxTypes.VAT;

    ms := TMemoryStream.Create;
    try
      invoice.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Basic);
      invoice.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.BasicWL);
      invoice.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Comfort);
      invoice.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      invoice.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung1);

      for i := 0 to invoice.TradeLineItems.Count - 1 do
        invoice.TradeLineItems[i].TaxType := TZUGFeRDTaxTypes.AAA;
      invoice.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung);
      invoice.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
    finally
      ms.Free;
    end;
  finally
    invoice.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestInvalidTaxTypes;
var
  invoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  i: Integer;
begin
  invoice := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    for i := 0 to invoice.TradeLineItems.Count - 1 do
      invoice.TradeLineItems[i].TaxType := TZUGFeRDTaxTypes.AAA;

    ms := TMemoryStream.Create;
    try
      invoice.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Basic);
      invoice.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.BasicWL);
      invoice.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Comfort);
      invoice.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Comfort);
      invoice.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung1);
      invoice.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung);
    finally
      ms.Free;
    end;
  finally
    invoice.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestAdditionalReferencedDocument;
var
  id, uriID: string;
  issueDateTime: TDateTime;
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  id := TGUID.NewGuid.ToString;
  uriID := TGUID.NewGuid.ToString;
  issueDateTime := Date;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.AddAdditionalReferencedDocument(
      {id=}       id,
      {typeCode=} TZUGFeRDAdditionalReferencedDocumentTypeCode.InvoiceDataSheet,
      {issueDateTime=} TZUGFeRDNullableParam<TDateTime>.Create(issueDateTime),
      {name=}     'Invoice Data Sheet',
      {referenceTypeCode=} nil,
      {attachmentBinaryObject=} nil,
      {filename=} ''
    );
    desc.AddAdditionalReferencedDocument(
      {id=}       id + '2',
      {typeCode=} TZUGFeRDAdditionalReferencedDocumentTypeCode.ReferenceDocument,
      {issueDateTime=} TZUGFeRDNullableParam<TDateTime>.Create(issueDateTime),
      {name=}     '',
      {referenceTypeCode=} TZUGFeRDNullableParam<TZUGFeRDReferenceTypeCodes>.Create(TZUGFeRDReferenceTypeCodes.PP),
      {attachmentBinaryObject=} nil,
      {filename=} ''
    );

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<Integer>(2, loadedInvoice.AdditionalReferencedDocuments.Count);
        Assert.AreEqual('Invoice Data Sheet', loadedInvoice.AdditionalReferencedDocuments[0].Name);
        Assert.AreEqual<TDateTime>(issueDateTime, loadedInvoice.AdditionalReferencedDocuments[0].IssueDateTime.Value);
        Assert.AreEqual(id, loadedInvoice.AdditionalReferencedDocuments[0].ID);
        Assert.AreEqual<TZUGFeRDAdditionalReferencedDocumentTypeCode>(TZUGFeRDAdditionalReferencedDocumentTypeCode.InvoiceDataSheet, loadedInvoice.AdditionalReferencedDocuments[0].TypeCode.Value);
        Assert.AreEqual('', loadedInvoice.AdditionalReferencedDocuments[1].Name);
        Assert.AreEqual<TDateTime>(issueDateTime, loadedInvoice.AdditionalReferencedDocuments[1].IssueDateTime.Value);
        Assert.AreEqual(id + '2', loadedInvoice.AdditionalReferencedDocuments[1].ID);
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

procedure TZUGFeRD22Tests.TestPartyExtensions;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.Invoicee := TZUGFeRDParty.Create;
    desc.Invoicee.Name := 'Invoicee';
    desc.Invoicee.ContactName := 'Max Mustermann';
    desc.Invoicee.Postcode := '83022';
    desc.Invoicee.City := 'Rosenheim';
    desc.Invoicee.Street := 'M'+#$00FC+'nchnerstra'+#$00DF+'e 123';
    desc.Invoicee.AddressLine3 := 'EG links';
    desc.Invoicee.CountrySubdivisionName := 'Bayern';
    desc.Invoicee.Country := TZUGFeRDCountryCodes.DE;

    desc.Payee := TZUGFeRDParty.Create;
    desc.Payee.Name := 'Payee';
    desc.Payee.ContactName := 'Max Mustermann';
    desc.Payee.Postcode := '83022';
    desc.Payee.City := 'Rosenheim';
    desc.Payee.Street := 'M'+#$00FC+'nchnerstra'+#$00DF+'e 123';
    desc.Payee.AddressLine3 := 'EG links';
    desc.Payee.CountrySubdivisionName := 'Bayern';
    desc.Payee.Country := TZUGFeRDCountryCodes.DE;

    // test with Comfort
    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Comfort);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNull(loadedInvoice.Invoicee);
        Assert.IsNotNull(loadedInvoice.Seller);
        Assert.IsNotNull(loadedInvoice.Payee);
        Assert.AreEqual('Lieferant GmbH', loadedInvoice.Seller.Name);
        Assert.AreEqual('Payee', loadedInvoice.Payee.Name);
        Assert.AreEqual('', loadedInvoice.Payee.ContactName);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;

    // test with Extended
    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual('Invoicee', loadedInvoice.Invoicee.Name);
        Assert.AreEqual('Max Mustermann', loadedInvoice.Invoicee.ContactName);
        Assert.AreEqual('83022', loadedInvoice.Invoicee.Postcode);
        Assert.AreEqual('Rosenheim', loadedInvoice.Invoicee.City);
        Assert.AreEqual('M'+#$00FC+'nchnerstra'+#$00DF+'e 123', loadedInvoice.Invoicee.Street);
        Assert.AreEqual('EG links', loadedInvoice.Invoicee.AddressLine3);
        Assert.AreEqual('Bayern', loadedInvoice.Invoicee.CountrySubdivisionName);
        Assert.AreEqual<TZUGFeRDCountryCodes>(TZUGFeRDCountryCodes.DE, loadedInvoice.Invoicee.Country.Value);

        Assert.AreEqual('Lieferant GmbH', loadedInvoice.Seller.Name);

        Assert.AreEqual('Payee', loadedInvoice.Payee.Name);
        Assert.AreEqual('Max Mustermann', loadedInvoice.Payee.ContactName);
        Assert.AreEqual('83022', loadedInvoice.Payee.Postcode);
        Assert.AreEqual('Rosenheim', loadedInvoice.Payee.City);
        Assert.AreEqual('M'+#$00FC+'nchnerstra'+#$00DF+'e 123', loadedInvoice.Payee.Street);
        Assert.AreEqual('EG links', loadedInvoice.Payee.AddressLine3);
        Assert.AreEqual('Bayern', loadedInvoice.Payee.CountrySubdivisionName);
        Assert.AreEqual<TZUGFeRDCountryCodes>(TZUGFeRDCountryCodes.DE, loadedInvoice.Payee.Country.Value);
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

procedure TZUGFeRD22Tests.TestShipTo;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.ShipTo := TZUGFeRDParty.Create;
    desc.ShipTo.ID := TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.Unknown, 'SL1001');
    desc.ShipTo.GlobalID := TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.GLN, 'MusterGLN');
    desc.ShipTo.Name := 'AbKunden AG Mitte';
    desc.ShipTo.Postcode := '12345';
    desc.ShipTo.ContactName := 'Einheit: 5.OG rechts';
    desc.ShipTo.Street := 'Verwaltung Stra'+#$00DF+'e 40';
    desc.ShipTo.City := 'Musterstadt';
    desc.ShipTo.Country := TZUGFeRDCountryCodes.DE;
    desc.ShipTo.CountrySubdivisionName := 'Hessen';

    // test minimum
    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Minimum);
      ms.Position := 0;
      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNull(loadedInvoice.ShipTo);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;

    // test basic
    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Basic);
      ms.Position := 0;
      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice.ShipTo);
        Assert.AreEqual('SL1001', loadedInvoice.ShipTo.ID.ID);
        Assert.AreEqual('MusterGLN', loadedInvoice.ShipTo.GlobalID.ID);
        Assert.AreEqual<TZUGFeRDGlobalIDSchemeIdentifiers>(TZUGFeRDGlobalIDSchemeIdentifiers.GLN, loadedInvoice.ShipTo.GlobalID.SchemeID.Value);
        Assert.AreEqual('AbKunden AG Mitte', loadedInvoice.ShipTo.Name);
        Assert.AreEqual('12345', loadedInvoice.ShipTo.Postcode);
        Assert.AreEqual('Einheit: 5.OG rechts', loadedInvoice.ShipTo.ContactName);
        Assert.AreEqual('Verwaltung Stra'+#$00DF+'e 40', loadedInvoice.ShipTo.Street);
        Assert.AreEqual('Musterstadt', loadedInvoice.ShipTo.City);
        Assert.AreEqual<TZUGFeRDCountryCodes>(TZUGFeRDCountryCodes.DE, loadedInvoice.ShipTo.Country.Value);
        Assert.AreEqual('Hessen', loadedInvoice.ShipTo.CountrySubdivisionName);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;

    // test extended
    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;
      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice.ShipTo);
        Assert.AreEqual('SL1001', loadedInvoice.ShipTo.ID.ID);
        Assert.AreEqual('AbKunden AG Mitte', loadedInvoice.ShipTo.Name);
        Assert.AreEqual<TZUGFeRDCountryCodes>(TZUGFeRDCountryCodes.DE, loadedInvoice.ShipTo.Country.Value);
        Assert.AreEqual('Hessen', loadedInvoice.ShipTo.CountrySubdivisionName);
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

procedure TZUGFeRD22Tests.TestShipToTradePartyOnItemLevel;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.TradeLineItems[0].ShipTo := TZUGFeRDParty.Create;
    desc.TradeLineItems[0].ShipTo.Name := 'ShipTo';
    desc.TradeLineItems[0].ShipTo.City := 'ShipToCity';

    // test minimum
    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Minimum);
      ms.Position := 0;
      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice.TradeLineItems);
        Assert.AreEqual<Integer>(0, loadedInvoice.TradeLineItems.Count);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;

    // test basic
    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Basic);
      ms.Position := 0;
      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice.TradeLineItems);
        Assert.IsNull(loadedInvoice.TradeLineItems[0].ShipTo);
        Assert.IsNull(loadedInvoice.TradeLineItems[0].UltimateShipTo);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;

    // test extended
    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;
      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice.TradeLineItems);
        Assert.IsNotNull(loadedInvoice.TradeLineItems[0].ShipTo);
        Assert.IsNull(loadedInvoice.TradeLineItems[0].UltimateShipTo);
        Assert.AreEqual('ShipTo', loadedInvoice.TradeLineItems[0].ShipTo.Name);
        Assert.AreEqual('ShipToCity', loadedInvoice.TradeLineItems[0].ShipTo.City);
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

procedure TZUGFeRD22Tests.TestParty_WithTaxRegistration;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.ShipTo := TZUGFeRDParty.Create;
    desc.ShipTo.ID := TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.Unknown, 'SL1001');
    desc.ShipTo.GlobalID := TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.GLN, 'MusterGLN');
    desc.ShipTo.Name := 'AbKunden AG Mitte';
    desc.ShipTo.Postcode := '12345';
    desc.ShipTo.ContactName := 'Einheit: 5.OG rechts';
    desc.ShipTo.Street := 'Verwaltung Stra'+#$00DF+'e 40';
    desc.ShipTo.City := 'Musterstadt';
    desc.ShipTo.Country := TZUGFeRDCountryCodes.DE;
    desc.ShipTo.CountrySubdivisionName := 'Hessen';

    desc.AddShipToTaxRegistration('DE123456789', TZUGFeRDTaxRegistrationSchemeID.VA);

    desc.Invoicee := TZUGFeRDParty.Create;
    desc.Invoicee.Name := 'Invoicee';
    desc.Invoicee.ContactName := 'Max Mustermann';
    desc.Invoicee.Postcode := '83022';
    desc.Invoicee.City := 'Rosenheim';
    desc.Invoicee.Street := 'M'+#$00FC+'nchnerstra'+#$00DF+'e 123';
    desc.Invoicee.AddressLine3 := 'EG links';
    desc.Invoicee.CountrySubdivisionName := 'Bayern';
    desc.Invoicee.Country := TZUGFeRDCountryCodes.DE;

    desc.AddInvoiceeTaxRegistration('DE987654321', TZUGFeRDTaxRegistrationSchemeID.VA);

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice.ShipTo);
        Assert.AreEqual<Integer>(1, loadedInvoice.ShipToTaxRegistration.Count);
        Assert.AreEqual('DE123456789', loadedInvoice.ShipToTaxRegistration[0].No);

        Assert.IsNotNull(loadedInvoice.Invoicee);
        Assert.AreEqual<Integer>(1, loadedInvoice.InvoiceeTaxRegistration.Count);
        Assert.AreEqual('DE987654321', loadedInvoice.InvoiceeTaxRegistration[0].No);
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

procedure TZUGFeRD22Tests.TestBuyerFCTaxRegistrationFiltered;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    // CreateInvoice already adds Seller with FC + VA
    // Add both FC + VA for Buyer as well
    desc.AddBuyerTaxRegistration('99/999/99999', TZUGFeRDTaxRegistrationSchemeID.FC);
    desc.AddBuyerTaxRegistration('DE987654321', TZUGFeRDTaxRegistrationSchemeID.VA);

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        // Seller: both FC + VA written (Extended allows FC for Seller)
        Assert.AreEqual<Integer>(2, loadedInvoice.SellerTaxRegistration.Count,
          'Seller should have 2 TaxRegistrations (FC + VA)');

        // Buyer: only VA, FC was filtered out
        Assert.AreEqual<Integer>(1, loadedInvoice.BuyerTaxRegistration.Count,
          'Buyer should have only 1 TaxRegistration (VA), FC is filtered');
        Assert.AreEqual<TZUGFeRDTaxRegistrationSchemeID>(
          TZUGFeRDTaxRegistrationSchemeID.VA,
          loadedInvoice.BuyerTaxRegistration[0].SchemeID);
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

procedure TZUGFeRD22Tests.TestUltimateShipToTradePartyOnItemLevel;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.TradeLineItems[0].UltimateShipTo := TZUGFeRDParty.Create;
    desc.TradeLineItems[0].UltimateShipTo.Name := 'ShipTo';
    desc.TradeLineItems[0].UltimateShipTo.City := 'ShipToCity';

    // test minimum
    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Minimum);
      ms.Position := 0;
      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice.TradeLineItems);
        Assert.AreEqual<Integer>(0, loadedInvoice.TradeLineItems.Count);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;

    // test basic
    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Basic);
      ms.Position := 0;
      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice.TradeLineItems);
        Assert.IsNull(loadedInvoice.TradeLineItems[0].ShipTo);
        Assert.IsNull(loadedInvoice.TradeLineItems[0].UltimateShipTo);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;

    // test extended
    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;
      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice.TradeLineItems);
        Assert.IsNull(loadedInvoice.TradeLineItems[0].ShipTo);
        Assert.IsNotNull(loadedInvoice.TradeLineItems[0].UltimateShipTo);
        Assert.AreEqual('ShipTo', loadedInvoice.TradeLineItems[0].UltimateShipTo.Name);
        Assert.AreEqual('ShipToCity', loadedInvoice.TradeLineItems[0].UltimateShipTo.City);
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

procedure TZUGFeRD22Tests.TestMimetypeOfEmbeddedAttachment;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms, dataStream: TMemoryStream;
  filename1, filename2: string;
  timestamp: TDateTime;
  data: TBytes;
  doc: TZUGFeRDAdditionalReferencedDocument;
  i: Integer;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
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
        'My-File-PDF',
        TZUGFeRDAdditionalReferencedDocumentTypeCode.ReferenceDocument,
        TZUGFeRDNullableParam<TDateTime>.Create(timestamp),
        'EmbeddedPdf', nil, dataStream, filename1);

      dataStream.Position := 0;

      desc.AddAdditionalReferencedDocument(
        'My-File-BIN',
        TZUGFeRDAdditionalReferencedDocumentTypeCode.ReferenceDocument,
        TZUGFeRDNullableParam<TDateTime>.Create(timestamp - 2),
        'EmbeddedPdf', nil, dataStream, filename2);
    finally
      dataStream.Free;
    end;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<Integer>(2, loadedInvoice.AdditionalReferencedDocuments.Count);
        for i := 0 to loadedInvoice.AdditionalReferencedDocuments.Count - 1 do
        begin
          doc := loadedInvoice.AdditionalReferencedDocuments[i];
          if doc.ID = 'My-File-PDF' then
          begin
            Assert.AreEqual(filename1, doc.Filename);
            Assert.AreEqual('application/pdf', doc.MimeType);
            Assert.AreEqual<TDateTime>(timestamp, doc.IssueDateTime.Value);
          end;
          if doc.ID = 'My-File-BIN' then
          begin
            Assert.AreEqual(filename2, doc.Filename);
            Assert.AreEqual('application/octet-stream', doc.MimeType);
            Assert.AreEqual<TDateTime>(timestamp - 2, doc.IssueDateTime.Value);
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

procedure TZUGFeRD22Tests.TestOrderInformation;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  timestamp: TDateTime;
begin
  timestamp := Date;

  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd21\zugferd_2p1_EXTENDED_Warenrechnung-factur-x.xml'));
  try
    desc.OrderDate := timestamp;
    desc.OrderNo := '12345';

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<TDateTime>(timestamp, loadedInvoice.OrderDate.Value);
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

procedure TZUGFeRD22Tests.TestSellerOrderReferencedDocument;
var
  uuid: string;
  issueDateTime: TDateTime;
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  uuid := TGUID.NewGuid.ToString;
  issueDateTime := Date;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.SellerOrderReferencedDocument := TZUGFeRDSellerOrderReferencedDocument.Create;
    desc.SellerOrderReferencedDocument.ID := uuid;
    desc.SellerOrderReferencedDocument.IssueDateTime := issueDateTime;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.Extended, loadedInvoice.Profile);
        Assert.AreEqual(uuid, loadedInvoice.SellerOrderReferencedDocument.ID);
        Assert.AreEqual<TDateTime>(issueDateTime, loadedInvoice.SellerOrderReferencedDocument.IssueDateTime.Value);
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

procedure TZUGFeRD22Tests.TestWriteAndReadBusinessProcess;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.BusinessProcess := 'A1';

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
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

procedure TZUGFeRD22Tests.TestWriteAndReadExtended;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms, dataStream: TMemoryStream;
  data: TBytes;
  timestamp: TDateTime;
  lineItem, loadedLineItem: TZUGFeRDTradeLineItem;
  bankAccount: TZUGFeRDBankAccount;
  debitorBankAccount: TZUGFeRDBankAccount;
  tax: TZUGFeRDTax;
  tradeCharge: TZUGFeRDTradeCharge;
  serviceCharge: TZUGFeRDServiceCharge;
  paymentTerms: TZUGFeRDPaymentTerms;
  productChar: TZUGFeRDApplicableProductCharacteristic;
  accountingAccount: TZUGFeRDReceivableSpecifiedTradeAccountingAccount;
  lineItemAllowance: TZUGFeRDAbstractTradeAllowanceCharge;
  lineItemRefDoc: TZUGFeRDAdditionalReferencedDocument;
  i: Integer;
  found: Boolean;
begin
  timestamp := Date;
  SetLength(data, 32768);
  for i := 0 to High(data) do
    data[i] := Random(256);

  dataStream := TMemoryStream.Create;
  try
    dataStream.WriteBuffer(data[0], Length(data));
    dataStream.Position := 0;

    desc := TZUGFeRDInvoiceProvider.CreateInvoice;
    try
      desc.AddAdditionalReferencedDocument(
        'My-File-BIN',
        TZUGFeRDAdditionalReferencedDocumentTypeCode.ReferenceDocument,
        TZUGFeRDNullableParam<TDateTime>.Create(timestamp - 2),
        'EmbeddedPdf',
        TZUGFeRDNullableParam<TZUGFeRDReferenceTypeCodes>.Create(TZUGFeRDReferenceTypeCodes.Unknown),
        dataStream,
        'myrandomdata.bin');

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

    desc.UltimateShipTo := TZUGFeRDParty.Create;
    desc.UltimateShipTo.ID := TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.Unknown, '123');
    desc.UltimateShipTo.GlobalID := TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.DUNS, '789');
    desc.UltimateShipTo.Name := 'Ultimate Ship To';
    desc.UltimateShipTo.ContactName := 'Max Mustermann';
    desc.UltimateShipTo.Street := 'M'+#$00FC+'nchnerstr. 55';
    desc.UltimateShipTo.Postcode := '83022';
    desc.UltimateShipTo.City := 'Rosenheim';
    desc.UltimateShipTo.Country := TZUGFeRDCountryCodes.DE;

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

    desc.AddTradeCharge(5, TZUGFeRDCurrencyCodes.EUR, 15, 'Reason for charge', TZUGFeRDTaxTypes.AAB, TZUGFeRDTaxCategoryCodes.AB, 19, TZUGFeRDNullableParam<TZUGFeRDChargeReasonCodes>.Create(TZUGFeRDChargeReasonCodes.HeatTreatment));
    desc.AddLogisticsServiceCharge(10, 'Logistics service charge', TZUGFeRDTaxTypes.AAC, TZUGFeRDTaxCategoryCodes.AC, 7);

    desc.PaymentTermsList[0].DueDate := timestamp + 14;
    desc.AddInvoiceReferencedDocument('RE-12344', TZUGFeRDNullableParam<TDateTime>.Create(timestamp), TZUGFeRDNullableParam<TZUGFeRDInvoiceType>.Create(TZUGFeRDInvoiceType.PartialInvoice));

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

    lineItem.AddAdditionalReferencedDocument('xyz', TZUGFeRDAdditionalReferencedDocumentTypeCode.ReferenceDocument, timestamp, '', TZUGFeRDNullableParam<TZUGFeRDReferenceTypeCodes>.Create(TZUGFeRDReferenceTypeCodes.AAB));
    lineItem.AddAdditionalReferencedDocument('abc', TZUGFeRDAdditionalReferencedDocumentTypeCode.InvoiceDataSheet, timestamp, '', TZUGFeRDNullableParam<TZUGFeRDReferenceTypeCodes>.Create(TZUGFeRDReferenceTypeCodes.PP));

    lineItem.NetQuantity := 3;
    lineItem.ActualDeliveryDate := timestamp;

    lineItem.ApplicableProductCharacteristics.Add(TZUGFeRDApplicableProductCharacteristic.Create);
    lineItem.ApplicableProductCharacteristics.Last.Description := 'Product characteristics';
    lineItem.ApplicableProductCharacteristics.Last.Value := 'Product value';

    lineItem.BillingPeriodStart := timestamp;
    lineItem.BillingPeriodEnd := timestamp + 10;

    lineItem.AddReceivableSpecifiedTradeAccountingAccount('987654');
    lineItem.AddTradeAllowance(TZUGFeRDCurrencyCodes.EUR, 10, 50, 'Reason: UnitTest',
      TZUGFeRDNullableParam<TZUGFeRDAllowanceReasonCodes>.Create(TZUGFeRDAllowanceReasonCodes.SpecialRebate));

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);

      ms.Position := 0;
      Assert.AreEqual<TZUGFeRDVersion>(TZUGFeRDVersion.Version23, TZUGFeRDInvoiceDescriptor.GetVersion(ms));

      ms.Position := 0;
      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual('471102', loadedInvoice.InvoiceNo);
        Assert.AreEqual<TDateTime>(EncodeDate(2018, 03, 05), loadedInvoice.InvoiceDate);
        Assert.AreEqual<TZUGFeRDCurrencyCodes>(TZUGFeRDCurrencyCodes.EUR, loadedInvoice.Currency);
        Assert.AreEqual('04011000-12345-34', loadedInvoice.ReferenceOrderNo);
        Assert.AreEqual('Lieferant GmbH', loadedInvoice.Seller.Name);
        Assert.AreEqual('80333', loadedInvoice.Seller.Postcode);
        Assert.AreEqual('M'+#$00FC+'nchen', loadedInvoice.Seller.City);
        Assert.AreEqual('Lieferantenstra'+#$00DF+'e 20', loadedInvoice.Seller.Street);
        Assert.AreEqual<TZUGFeRDCountryCodes>(TZUGFeRDCountryCodes.DE, loadedInvoice.Seller.Country.Value);
        Assert.AreEqual<TZUGFeRDGlobalIDSchemeIdentifiers>(TZUGFeRDGlobalIDSchemeIdentifiers.GLN, loadedInvoice.Seller.GlobalID.SchemeID);
        Assert.AreEqual('4000001123452', loadedInvoice.Seller.GlobalID.ID);
        Assert.AreEqual('Max Mustermann', loadedInvoice.SellerContact.Name);
        Assert.AreEqual('Muster-Einkauf', loadedInvoice.SellerContact.OrgUnit);
        Assert.AreEqual('Max@Mustermann.de', loadedInvoice.SellerContact.EmailAddress);
        Assert.AreEqual('+49891234567', loadedInvoice.SellerContact.PhoneNo);

        Assert.AreEqual('Kunden AG Mitte', loadedInvoice.Buyer.Name);
        Assert.AreEqual('69876', loadedInvoice.Buyer.Postcode);
        Assert.AreEqual('Frankfurt', loadedInvoice.Buyer.City);
        Assert.AreEqual('Kundenstra'+#$00DF+'e 15', loadedInvoice.Buyer.Street);
        Assert.AreEqual<TZUGFeRDCountryCodes>(TZUGFeRDCountryCodes.DE, loadedInvoice.Buyer.Country.Value);
        Assert.AreEqual('GE2020211', loadedInvoice.Buyer.ID.ID);

        Assert.AreEqual('12345', loadedInvoice.OrderNo);
        Assert.AreEqual<TDateTime>(timestamp, loadedInvoice.OrderDate.Value);

        Assert.AreEqual('12345', loadedInvoice.ContractReferencedDocument.ID);
        Assert.AreEqual<TDateTime>(timestamp, loadedInvoice.ContractReferencedDocument.IssueDateTime.Value);

        Assert.AreEqual('123', loadedInvoice.SpecifiedProcuringProject.ID);
        Assert.AreEqual('Project 123', loadedInvoice.SpecifiedProcuringProject.Name);

        Assert.AreEqual('Ultimate Ship To', loadedInvoice.UltimateShipTo.Name);

        Assert.AreEqual('123', loadedInvoice.ShipTo.ID.ID);
        Assert.AreEqual<TZUGFeRDGlobalIDSchemeIdentifiers>(TZUGFeRDGlobalIDSchemeIdentifiers.DUNS, loadedInvoice.ShipTo.GlobalID.SchemeID);
        Assert.AreEqual('789', loadedInvoice.ShipTo.GlobalID.ID);
        Assert.AreEqual('Ship To', loadedInvoice.ShipTo.Name);
        Assert.AreEqual('Max Mustermann', loadedInvoice.ShipTo.ContactName);
        Assert.AreEqual('M'+#$00FC+'nchnerstr. 55', loadedInvoice.ShipTo.Street);
        Assert.AreEqual('83022', loadedInvoice.ShipTo.Postcode);
        Assert.AreEqual('Rosenheim', loadedInvoice.ShipTo.City);
        Assert.AreEqual<TZUGFeRDCountryCodes>(TZUGFeRDCountryCodes.DE, loadedInvoice.ShipTo.Country.Value);

        Assert.AreEqual('123', loadedInvoice.ShipFrom.ID.ID);
        Assert.AreEqual<TZUGFeRDGlobalIDSchemeIdentifiers>(TZUGFeRDGlobalIDSchemeIdentifiers.DUNS, loadedInvoice.ShipFrom.GlobalID.SchemeID);
        Assert.AreEqual('789', loadedInvoice.ShipFrom.GlobalID.ID);
        Assert.AreEqual('Ship From', loadedInvoice.ShipFrom.Name);
        Assert.AreEqual('Eva Musterfrau', loadedInvoice.ShipFrom.ContactName);
        Assert.AreEqual('Alpenweg 5', loadedInvoice.ShipFrom.Street);
        Assert.AreEqual('83022', loadedInvoice.ShipFrom.Postcode);
        Assert.AreEqual('Rosenheim', loadedInvoice.ShipFrom.City);
        Assert.AreEqual<TZUGFeRDCountryCodes>(TZUGFeRDCountryCodes.DE, loadedInvoice.ShipFrom.Country.Value);

        Assert.AreEqual<TDateTime>(EncodeDate(2018, 03, 05), loadedInvoice.ActualDeliveryDate.Value);
        Assert.AreEqual<TZUGFeRDPaymentMeansTypeCodes>(TZUGFeRDPaymentMeansTypeCodes.SEPACreditTransfer, loadedInvoice.PaymentMeans.TypeCode);
        Assert.AreEqual('Zahlung per SEPA '+#$00DC+'berweisung.', loadedInvoice.PaymentMeans.Information);

        Assert.AreEqual('PaymentReference', loadedInvoice.PaymentReference);

        Assert.AreEqual('', loadedInvoice.PaymentMeans.SEPACreditorIdentifier);
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
        Assert.AreEqual<TZUGFeRDCountryCodes>(TZUGFeRDCountryCodes.DE, loadedInvoice.Invoicee.Country.Value);

        // Payee
        Assert.AreEqual('Test', loadedInvoice.Payee.Name);
        Assert.AreEqual('Max Mustermann', loadedInvoice.Payee.ContactName);
        Assert.AreEqual('83022', loadedInvoice.Payee.Postcode);
        Assert.AreEqual('Rosenheim', loadedInvoice.Payee.City);
        Assert.AreEqual('M'+#$00FC+'nchnerstra'+#$00DF+'e 123', loadedInvoice.Payee.Street);
        Assert.AreEqual('EG links', loadedInvoice.Payee.AddressLine3);
        Assert.AreEqual('Bayern', loadedInvoice.Payee.CountrySubdivisionName);
        Assert.AreEqual<TZUGFeRDCountryCodes>(TZUGFeRDCountryCodes.DE, loadedInvoice.Payee.Country.Value);

        // Tax
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
        Assert.AreEqual<TZUGFeRDTaxTypes>(TZUGFeRDTaxTypes.VAT, tax.TypeCode);
        Assert.AreEqual<TZUGFeRDTaxCategoryCodes>(TZUGFeRDTaxCategoryCodes.S, tax.CategoryCode);

        Assert.AreEqual<TDateTime>(timestamp, loadedInvoice.BillingPeriodStart.Value);
        Assert.AreEqual<TDateTime>(timestamp + 14, loadedInvoice.BillingPeriodEnd.Value);

        // TradeAllowanceCharges
        Assert.AreEqual<Integer>(0, Length(loadedInvoice.GetTradeAllowances));
        tradeCharge := nil;
        found := False;
        for i := 0 to High(loadedInvoice.GetTradeCharges) do
          if loadedInvoice.GetTradeCharges[i].Reason = 'Reason for charge' then
          begin
            tradeCharge := loadedInvoice.GetTradeCharges[i];
            found := True;
            Break;
          end;
        Assert.IsTrue(found);
        Assert.IsTrue(tradeCharge.ChargeIndicator);
        Assert.AreEqual('Reason for charge', tradeCharge.Reason);
        Assert.AreEqual<Currency>(5, tradeCharge.BasisAmount);
        Assert.AreEqual<Currency>(15, tradeCharge.ActualAmount);
        Assert.AreEqual<TZUGFeRDCurrencyCodes>(TZUGFeRDCurrencyCodes.EUR, tradeCharge.Currency);
        Assert.AreEqual<Currency>(19, tradeCharge.Tax.Percent);
        Assert.AreEqual<TZUGFeRDTaxTypes>(TZUGFeRDTaxTypes.AAB, tradeCharge.Tax.TypeCode);
        Assert.AreEqual<TZUGFeRDTaxCategoryCodes>(TZUGFeRDTaxCategoryCodes.AB, tradeCharge.Tax.CategoryCode);

        // ServiceCharges
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
        Assert.AreEqual<TZUGFeRDTaxTypes>(TZUGFeRDTaxTypes.AAC, serviceCharge.Tax.TypeCode);
        Assert.AreEqual<TZUGFeRDTaxCategoryCodes>(TZUGFeRDTaxCategoryCodes.AC, serviceCharge.Tax.CategoryCode);

        // PaymentTerms
        Assert.IsTrue(loadedInvoice.PaymentTermsList.Count > 0);
        paymentTerms := loadedInvoice.PaymentTermsList[0];
        Assert.IsNotNull(paymentTerms);
        Assert.AreEqual('Zahlbar innerhalb 30 Tagen netto bis 04.04.2018, 3% Skonto innerhalb 10 Tagen bis 15.03.2018', paymentTerms.Description);
        Assert.AreEqual<TDateTime>(timestamp + 14, paymentTerms.DueDate.Value);

        Assert.AreEqual<Currency>(473.0, loadedInvoice.LineTotalAmount.Value);
        Assert.IsFalse(loadedInvoice.ChargeTotalAmount.HasValue);
        Assert.IsFalse(loadedInvoice.AllowanceTotalAmount.HasValue);
        Assert.AreEqual<Currency>(473.0, loadedInvoice.TaxBasisAmount.Value);
        Assert.AreEqual<Currency>(56.87, loadedInvoice.TaxTotalAmount.Value);
        Assert.AreEqual<Currency>(529.87, loadedInvoice.GrandTotalAmount.Value);
        Assert.IsFalse(loadedInvoice.TotalPrepaidAmount.HasValue);
        Assert.AreEqual<Currency>(529.87, loadedInvoice.DuePayableAmount.Value);

        // InvoiceReferencedDocument
        Assert.AreEqual('RE-12344', loadedInvoice.InvoiceReferencedDocuments[0].ID);
        Assert.AreEqual<TDateTime>(timestamp, loadedInvoice.InvoiceReferencedDocuments[0].IssueDateTime.Value);
        Assert.AreEqual<TZUGFeRDInvoiceType>(TZUGFeRDInvoiceType.PartialInvoice, loadedInvoice.InvoiceReferencedDocuments[0].TypeCode);

        // Line items
        loadedLineItem := nil;
        for i := 0 to loadedInvoice.TradeLineItems.Count - 1 do
          if loadedInvoice.TradeLineItems[i].SellerAssignedID = 'TB100A4' then
          begin
            loadedLineItem := loadedInvoice.TradeLineItems[i];
            Break;
          end;
        Assert.IsNotNull(loadedLineItem);
        Assert.IsFalse(loadedLineItem.AssociatedDocument.LineID.IsEmpty);
        Assert.AreEqual('This is line item TB100A4', loadedLineItem.Description);
        Assert.AreEqual('Trennbl'+#$00E4+'tter A4', loadedLineItem.Name);
        Assert.AreEqual('TB100A4', loadedLineItem.SellerAssignedID);
        Assert.AreEqual('0815', loadedLineItem.BuyerAssignedID);
        Assert.AreEqual<TZUGFeRDGlobalIDSchemeIdentifiers>(TZUGFeRDGlobalIDSchemeIdentifiers.EAN, loadedLineItem.GlobalID.SchemeID);
        Assert.AreEqual('4012345001235', loadedLineItem.GlobalID.ID);

        // GrossPriceProductTradePrice
        Assert.AreEqual<Currency>(9.9, loadedLineItem.GrossUnitPrice.Value);
        Assert.AreEqual<TZUGFeRDQuantityCodes>(TZUGFeRDQuantityCodes.H87, loadedLineItem.UnitCode.Value);
        Assert.AreEqual<Currency>(3, loadedLineItem.NetQuantity.Value);

        // NetPriceProductTradePrice
        Assert.AreEqual<Currency>(9.9, loadedLineItem.NetUnitPrice.Value);
        Assert.AreEqual<Currency>(20, loadedLineItem.BilledQuantity);

        Assert.AreEqual<TZUGFeRDTaxTypes>(TZUGFeRDTaxTypes.VAT, loadedLineItem.TaxType);
        Assert.AreEqual<TZUGFeRDTaxCategoryCodes>(TZUGFeRDTaxCategoryCodes.S, loadedLineItem.TaxCategoryCode);
        Assert.AreEqual<Currency>(19, loadedLineItem.TaxPercent);

        Assert.AreEqual('1', loadedLineItem.BuyerOrderReferencedDocument.LineID);
        Assert.AreEqual('12345', loadedLineItem.BuyerOrderReferencedDocument.ID);
        Assert.AreEqual<TDateTime>(timestamp, loadedLineItem.BuyerOrderReferencedDocument.IssueDateTime.Value);
        Assert.AreEqual('1', loadedLineItem.DeliveryNoteReferencedDocument.LineID);
        Assert.AreEqual('12345', loadedLineItem.DeliveryNoteReferencedDocument.ID);
        Assert.AreEqual<TDateTime>(timestamp, loadedLineItem.DeliveryNoteReferencedDocument.IssueDateTime.Value);
        Assert.AreEqual('1', loadedLineItem.ContractReferencedDocument.LineID);
        Assert.AreEqual('12345', loadedLineItem.ContractReferencedDocument.ID);
        Assert.AreEqual<TDateTime>(timestamp, loadedLineItem.ContractReferencedDocument.IssueDateTime.Value);

        Assert.AreEqual<Integer>(1, loadedLineItem.AdditionalReferencedDocuments.Count);
        lineItemRefDoc := loadedLineItem.AdditionalReferencedDocuments[0];
        Assert.IsNotNull(lineItemRefDoc);
        Assert.AreEqual('abc', lineItemRefDoc.ID);
        Assert.AreEqual<TZUGFeRDAdditionalReferencedDocumentTypeCode>(TZUGFeRDAdditionalReferencedDocumentTypeCode.InvoiceDataSheet, lineItemRefDoc.TypeCode);
        Assert.AreEqual<TDateTime>(timestamp, lineItemRefDoc.IssueDateTime.Value);
        Assert.AreEqual<TZUGFeRDReferenceTypeCodes>(TZUGFeRDReferenceTypeCodes.PP, lineItemRefDoc.ReferenceTypeCode.Value);

        productChar := loadedLineItem.ApplicableProductCharacteristics[0];
        Assert.IsNotNull(productChar);
        Assert.AreEqual('Product characteristics', productChar.Description);
        Assert.AreEqual('Product value', productChar.Value);

        Assert.AreEqual<TDateTime>(timestamp, loadedLineItem.ActualDeliveryDate.Value);
        Assert.AreEqual<TDateTime>(timestamp, loadedLineItem.BillingPeriodStart.Value);
        Assert.AreEqual<TDateTime>(timestamp + 10, loadedLineItem.BillingPeriodEnd.Value);

        accountingAccount := loadedLineItem.ReceivableSpecifiedTradeAccountingAccounts[0];
        Assert.IsNotNull(accountingAccount);
        Assert.AreEqual('987654', accountingAccount.TradeAccountID);
        Assert.IsFalse(accountingAccount.TradeAccountTypeCode.HasValue);

        // Line item trade allowance (price-level, via AddTradeAllowance -> TradeAllowanceCharges)
        Assert.IsTrue(loadedLineItem.TradeAllowanceCharges.Count > 0);
        lineItemAllowance := nil;
        for i := 0 to loadedLineItem.TradeAllowanceCharges.Count - 1 do
          if loadedLineItem.TradeAllowanceCharges[i].Reason = 'Reason: UnitTest' then
          begin
            lineItemAllowance := loadedLineItem.TradeAllowanceCharges[i];
            Break;
          end;
        Assert.IsNotNull(lineItemAllowance);
        Assert.IsFalse(lineItemAllowance.ChargeIndicator);
        Assert.AreEqual<Currency>(10, lineItemAllowance.BasisAmount);
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
  finally
    dataStream.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestAltteilSteuer;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceDescriptor.CreateInvoice('112233', EncodeDate(2021, 04, 23), TZUGFeRDCurrencyCodes.EUR);
  try
    desc.Notes.Clear;
    desc.AddNote(
      'Rechnung enth'+#$00E4+'lt 100 EUR (Umsatz)Steuer auf Altteile gem. Abschn. 10.5 Abs. 3 UStAE',
      TZUGFeRDNullableParam<TZUGFeRDSubjectCodes>.Create(TZUGFeRDSubjectCodes.ADU));

    desc.TradeLineItems.Clear;
    desc.AddTradeLineItem(
      {name=}            'Neumotor',
      {netUnitPrice=}    nil,
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.H87),
      {unitQuantity=}    TZUGFeRDNullableParam<Currency>.Create(1),
      {grossUnitPrice=}  nil,
      {billedQuantity=}  1,
      {lineTotalAmount=} 0,
      {taxType=}         TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
      {categoryCode=}    TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
      {taxPercent=}      19
    );
    desc.TradeLineItems.Last.NetUnitPrice := 1000;

    desc.AddTradeLineItem(
      {name=}            'Bemessungsgrundlage und Umsatzsteuer auf Altteil',
      {netUnitPrice=}    nil,
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62),
      {unitQuantity=}    TZUGFeRDNullableParam<Currency>.Create(1),
      {grossUnitPrice=}  nil,
      {billedQuantity=}  1,
      {lineTotalAmount=} 0,
      {taxType=}         TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
      {categoryCode=}    TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
      {taxPercent=}      19
    );
    desc.TradeLineItems.Last.NetUnitPrice := 100;

    desc.AddTradeLineItem(
      {name=}            'Korrektur/Stornierung Bemessungsgrundlage der Umsatzsteuer auf Altteil',
      {netUnitPrice=}    nil,
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.C62),
      {unitQuantity=}    TZUGFeRDNullableParam<Currency>.Create(1),
      {grossUnitPrice=}  nil,
      {billedQuantity=}  -1,
      {lineTotalAmount=} 0,
      {taxType=}         TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
      {categoryCode=}    TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.Z),
      {taxPercent=}      0
    );
    desc.TradeLineItems.Last.NetUnitPrice := 100;

    desc.AddApplicableTradeTax(
      {calculatedAmount=} 1000.0 / 100 * 19,
      {basisAmount=}      1000.0,
      {percent=}          19,
      {typeCode=}         TZUGFeRDTaxTypes.VAT,
      {categoryCode=}     TZUGFeRDTaxCategoryCodes.S
    );

    desc.SetTotals(
      {lineTotalAmount=}   1500,
      {chargeTotalAmount=} 0,
      {allowanceTotalAmount=} 0,
      {taxBasisAmount=}    1500,
      {taxTotalAmount=}    304,
      {grandTotalAmount=}  1804,
      {totalPrepaidAmount=} 0,
      {duePayableAmount=}  1804
    );

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNull(loadedInvoice.Invoicee);
        Assert.AreEqual<TZUGFeRDSubjectCodes>(TZUGFeRDSubjectCodes.ADU, loadedInvoice.Notes[0].SubjectCode.Value);
        Assert.IsFalse(loadedInvoice.Notes[0].ContentCode.HasValue);
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

procedure TZUGFeRD22Tests.TestTradeAllowanceChargeWithoutExplicitPercentage;
var
  invoice, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  allowances: TArray<TZUGFeRDTradeAllowance>;
  charges: TArray<TZUGFeRDTradeCharge>;
begin
  invoice := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    invoice.AddTradeCharge(100, TZUGFeRDCurrencyCodes.EUR, 10, '', TZUGFeRDTaxTypes.VAT, TZUGFeRDTaxCategoryCodes.S, 19, TZUGFeRDNullableParam<TZUGFeRDChargeReasonCodes>.Create(TZUGFeRDChargeReasonCodes.Packing));

    ms := TMemoryStream.Create;
    try
      invoice.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        allowances := loadedInvoice.GetTradeAllowances;
        charges := loadedInvoice.GetTradeCharges;

        Assert.AreEqual<Integer>(0, Length(allowances));
        Assert.AreEqual<Integer>(1, Length(charges));
        Assert.AreEqual<Currency>(100, charges[0].BasisAmount);
        Assert.AreEqual<Currency>(10, charges[0].ActualAmount);
        Assert.IsFalse(charges[0].ChargePercentage.HasValue);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    invoice.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestTradeAllowanceChargeWithExplicitPercentage;
var
  invoice, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  allowances: TArray<TZUGFeRDTradeAllowance>;
  charges: TArray<TZUGFeRDTradeCharge>;
begin
  invoice := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    invoice.AddTradeCharge(100, TZUGFeRDCurrencyCodes.EUR, 10, 12, '', TZUGFeRDTaxTypes.VAT, TZUGFeRDTaxCategoryCodes.S, 19, TZUGFeRDNullableParam<TZUGFeRDChargeReasonCodes>.Create(TZUGFeRDChargeReasonCodes.Packing));

    ms := TMemoryStream.Create;
    try
      invoice.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        allowances := loadedInvoice.GetTradeAllowances;
        charges := loadedInvoice.GetTradeCharges;

        Assert.AreEqual<Integer>(0, Length(allowances));
        Assert.AreEqual<Integer>(1, Length(charges));
        Assert.AreEqual<Currency>(100, charges[0].BasisAmount);
        Assert.AreEqual<Currency>(10, charges[0].ActualAmount);
        Assert.AreEqual<Currency>(12, charges[0].ChargePercentage.Value);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    invoice.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestWriteAndReadDespatchAdviceDocumentReferenceAllProfilesButMinimum(profileOrd: Integer);
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  profile: TZUGFeRDProfile;
begin
  profile := TZUGFeRDProfile(profileOrd);
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.SetDespatchAdviceReferencedDocument('421567982',
      TZUGFeRDNullableParam<TDateTime>.Create(EncodeDate(2024, 5, 14)));

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, profile);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice.DespatchAdviceReferencedDocument);

        if profile = TZUGFeRDProfile.Extended then
          Assert.IsTrue(loadedInvoice.DespatchAdviceReferencedDocument.IssueDateTime.HasValue)
        else
          Assert.IsFalse(loadedInvoice.DespatchAdviceReferencedDocument.IssueDateTime.HasValue);
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

procedure TZUGFeRD22Tests.TestWriteAndReadDespatchAdviceDocumentReferenceMinimum;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.SetDespatchAdviceReferencedDocument('421567982',
      TZUGFeRDNullableParam<TDateTime>.Create(EncodeDate(2024, 5, 14)));

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Minimum);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNull(loadedInvoice.DespatchAdviceReferencedDocument);
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

procedure TZUGFeRD22Tests.TestWriteAndReadDespatchAdviceDocumentReferenceExtended;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  despatchAdviceDate: TDateTime;
begin
  despatchAdviceDate := EncodeDate(2024, 5, 14);
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.SetDespatchAdviceReferencedDocument('421567982',
      TZUGFeRDNullableParam<TDateTime>.Create(despatchAdviceDate));

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual('421567982', loadedInvoice.DespatchAdviceReferencedDocument.ID);
        Assert.AreEqual<TDateTime>(despatchAdviceDate, loadedInvoice.DespatchAdviceReferencedDocument.IssueDateTime.Value);
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

procedure TZUGFeRD22Tests.TestSpecifiedTradeAllowanceCharge(profileOrd: Integer);
var
  invoice, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  profile: TZUGFeRDProfile;
  allowance: TZUGFeRDTradeAllowance;
  allowances: TArray<TZUGFeRDTradeAllowance>;
begin
  profile := TZUGFeRDProfile(profileOrd);
  invoice := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    invoice.TradeLineItems[0].AddSpecifiedTradeAllowance(TZUGFeRDCurrencyCodes.EUR, 198, 19.8, 10, 'Discount 10%');

    ms := TMemoryStream.Create;
    try
      invoice.Save(ms, TZUGFeRDVersion.Version23, profile);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        allowances := loadedInvoice.TradeLineItems[0].GetSpecifiedTradeAllowances;
        Assert.IsTrue(Length(allowances) > 0);
        allowance := allowances[0];

        Assert.IsFalse(allowance.ChargeIndicator); // false = discount
        if profile <> TZUGFeRDProfile.Basic then
        begin
          Assert.AreEqual<Currency>(198, allowance.BasisAmount);
          Assert.AreEqual<Currency>(10, allowance.ChargePercentage.Value);
        end;
        Assert.AreEqual<Currency>(19.8, allowance.ActualAmount);
        Assert.AreEqual('Discount 10%', allowance.Reason);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    invoice.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestTradeLineItemsNotWrittenInMinimum;
var
  invoice, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  invoice := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    invoice.TradeLineItems[0].AddSpecifiedTradeAllowance(TZUGFeRDCurrencyCodes.EUR, 198, 19.8, 10, 'Discount 10%');

    ms := TMemoryStream.Create;
    try
      invoice.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Minimum);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice.TradeLineItems);
        Assert.AreEqual<Integer>(0, loadedInvoice.TradeLineItems.Count);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    invoice.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestSellerDescription;
var
  invoice, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  description: string;
begin
  description := 'Test description';
  invoice := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    invoice.SetSeller(
      'Lieferant GmbH', '80333', 'M'+#$00FC+'nchen',
      'Lieferantenstra'+#$00DF+'e 20',
      TZUGFeRDNullableParam<TZUGFeRDCountryCodes>.Create(TZUGFeRDCountryCodes.DE),
      '',
      TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.GLN, '4000001123452'),
      TZUGFeRDLegalOrganization.CreateWithParams(TZUGFeRDNullableParam<TZUGFeRDGlobalIDSchemeIdentifiers>.Create(TZUGFeRDGlobalIDSchemeIdentifiers.GLN), '4000001123452', 'Lieferant GmbH'),
      description);

    ms := TMemoryStream.Create;
    try
      invoice.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual(description, loadedInvoice.Seller.Description);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    invoice.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestSellerContact;
var
  invoice, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  description: string;
begin
  description := 'Test description';
  invoice := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    invoice.SetSeller(
      'Lieferant GmbH', '80333', 'M'+#$00FC+'nchen',
      'Lieferantenstra'+#$00DF+'e 20',
      TZUGFeRDNullableParam<TZUGFeRDCountryCodes>.Create(TZUGFeRDCountryCodes.DE),
      '',
      TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.GLN, '4000001123452'),
      TZUGFeRDLegalOrganization.CreateWithParams(TZUGFeRDNullableParam<TZUGFeRDGlobalIDSchemeIdentifiers>.Create(TZUGFeRDGlobalIDSchemeIdentifiers.GLN), '4000001123452', 'Lieferant GmbH'),
      description);

    invoice.SetSellerContact('1-123', '2-123', '3-123', '4-123', '5-123');

    ms := TMemoryStream.Create;
    try
      invoice.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual('1-123', loadedInvoice.SellerContact.Name);
        Assert.AreEqual('2-123', loadedInvoice.SellerContact.OrgUnit);
        Assert.AreEqual('3-123', loadedInvoice.SellerContact.EmailAddress);
        Assert.AreEqual('4-123', loadedInvoice.SellerContact.PhoneNo);
        Assert.AreEqual('5-123', loadedInvoice.SellerContact.FaxNo);
        Assert.AreEqual(description, loadedInvoice.Seller.Description);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    invoice.Free;
  end;
end;

procedure TZUGFeRD22Tests.ShouldLoadCiiWithoutQdtNamespace;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('xRechnung\xRechnung CII - without qdt.xml'));
  try
    Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.XRechnung, desc.Profile);
    Assert.AreEqual<TZUGFeRDInvoiceType>(TZUGFeRDInvoiceType.Invoice, desc.Type_);
    Assert.AreEqual('123456XX', desc.InvoiceNo);
    Assert.AreEqual<Integer>(2, desc.TradeLineItems.Count);
    Assert.AreEqual<Currency>(314.86, desc.LineTotalAmount.Value);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestDesignatedProductClassificationWithFullClassification;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  content: string;
  reader: TStreamReader;
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
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung);

      // string comparison
      ms.Position := 0;
      reader := TStreamReader.Create(ms);
      try
        content := reader.ReadToEnd;
      finally
        reader.Free;
      end;
      Assert.IsTrue(content.Contains('<ram:DesignatedProductClassification>'));
      Assert.IsTrue(content.Contains('<ram:ClassCode listID="HS" listVersionID="List Version ID Value">Class Code</ram:ClassCode>'));
      Assert.IsTrue(content.Contains('<ram:ClassName>Class Name</ram:ClassName>'));

      // structure comparison
      ms.Position := 0;
      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<TZUGFeRDDesignatedProductClassificationClassCodes>(TZUGFeRDDesignatedProductClassificationClassCodes.HS,
          loadedInvoice.TradeLineItems[0].DesignedProductClassifications[0].ListID);
        Assert.AreEqual('List Version ID Value',
          loadedInvoice.TradeLineItems[0].DesignedProductClassifications[0].ListVersionID);
        Assert.AreEqual('Class Code',
          loadedInvoice.TradeLineItems[0].DesignedProductClassifications[0].ClassCode);
        Assert.AreEqual('Class Name',
          loadedInvoice.TradeLineItems[0].DesignedProductClassifications[0].ClassName_);
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

procedure TZUGFeRD22Tests.TestDesignatedProductClassificationWithEmptyVersionId;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.TradeLineItems[0].AddDesignatedProductClassification(
      TZUGFeRDDesignatedProductClassificationClassCodes.HS,
      '',
      'Class Code',
      'Class Name');

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<TZUGFeRDDesignatedProductClassificationClassCodes>(TZUGFeRDDesignatedProductClassificationClassCodes.HS,
          desc.TradeLineItems[0].DesignedProductClassifications[0].ListID);
        Assert.IsTrue(desc.TradeLineItems[0].DesignedProductClassifications[0].ListVersionID.IsEmpty
          or (desc.TradeLineItems[0].DesignedProductClassifications[0].ListVersionID = ''));
        Assert.AreEqual('Class Code',
          desc.TradeLineItems[0].DesignedProductClassifications[0].ClassCode);
        Assert.AreEqual('Class Name',
          desc.TradeLineItems[0].DesignedProductClassifications[0].ClassName_);
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

procedure TZUGFeRD22Tests.TestDesignatedProductClassificationWithEmptyListIdAndVersionId;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.TradeLineItems[0].AddDesignatedProductClassification(
      TZUGFeRDDesignatedProductClassificationClassCodes.HS,
      '',
      'Class Code');

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<TZUGFeRDDesignatedProductClassificationClassCodes>(TZUGFeRDDesignatedProductClassificationClassCodes.HS,
          loadedInvoice.TradeLineItems[0].DesignedProductClassifications[0].ListID);
        Assert.AreEqual('',
          loadedInvoice.TradeLineItems[0].DesignedProductClassifications[0].ListVersionID);
        Assert.AreEqual('Class Code',
          loadedInvoice.TradeLineItems[0].DesignedProductClassifications[0].ClassCode);
        Assert.AreEqual('',
          loadedInvoice.TradeLineItems[0].DesignedProductClassifications[0].ClassName_);
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

procedure TZUGFeRD22Tests.TestDesignatedProductClassificationWithoutAnyOptionalInformation;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.TradeLineItems[0].AddDesignatedProductClassification(
      TZUGFeRDDesignatedProductClassificationClassCodes.HS);

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<TZUGFeRDDesignatedProductClassificationClassCodes>(TZUGFeRDDesignatedProductClassificationClassCodes.HS,
          desc.TradeLineItems[0].DesignedProductClassifications[0].ListID);
        Assert.IsTrue(desc.TradeLineItems[0].DesignedProductClassifications[0].ListVersionID.IsEmpty
          or (desc.TradeLineItems[0].DesignedProductClassifications[0].ListVersionID = ''));
        Assert.IsTrue(desc.TradeLineItems[0].DesignedProductClassifications[0].ClassCode.IsEmpty
          or (desc.TradeLineItems[0].DesignedProductClassifications[0].ClassCode = ''));
        Assert.IsTrue(desc.TradeLineItems[0].DesignedProductClassifications[0].ClassName_.IsEmpty
          or (desc.TradeLineItems[0].DesignedProductClassifications[0].ClassName_ = ''));
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

procedure TZUGFeRD22Tests.TestPaymentTermsMultiCardinalityWithExtended;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  timestamp: TDateTime;
  baseAmount, percentage, actualAmount: Currency;
  paymentTerm: TZUGFeRDPaymentTerms;
begin
  timestamp := Date;
  baseAmount := 123;
  percentage := 3;
  actualAmount := 123 * 3 / 100;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.PaymentTermsList.Clear;
    desc.AddTradePaymentTerms(
      'Zahlbar innerhalb 30 Tagen netto bis 04.04.2018',
      TZUGFeRDNullableParam<TDateTime>.Create(timestamp + 14));
    desc.AddTradePaymentTerms(
      '3% Skonto innerhalb 10 Tagen bis 15.03.2018',
      TZUGFeRDNullableParam<TDateTime>.Create(EncodeDate(2018, 3, 15)),
      TZUGFeRDNullableParam<TZUGFeRDPaymentTermsType>.Create(TZUGFeRDPaymentTermsType.Skonto),
      TZUGFeRDNullableParam<Integer>.Create(10),
      TZUGFeRDNullableParam<Currency>.Create(percentage),
      TZUGFeRDNullableParam<Currency>.Create(baseAmount),
      TZUGFeRDNullableParam<Currency>.Create(actualAmount));

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);

      ms.Position := 0;
      Assert.AreEqual<TZUGFeRDVersion>(TZUGFeRDVersion.Version23, TZUGFeRDInvoiceDescriptor.GetVersion(ms));

      ms.Position := 0;
      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice.PaymentTermsList);
        Assert.AreEqual<Integer>(2, loadedInvoice.PaymentTermsList.Count);

        // find the "Zahlbar" term
        paymentTerm := nil;
        for var i := 0 to loadedInvoice.PaymentTermsList.Count - 1 do
          if loadedInvoice.PaymentTermsList[i].Description.StartsWith('Zahlbar') then
          begin
            paymentTerm := loadedInvoice.PaymentTermsList[i];
            Break;
          end;
        Assert.IsNotNull(paymentTerm);
        Assert.AreEqual('Zahlbar innerhalb 30 Tagen netto bis 04.04.2018', paymentTerm.Description);
        Assert.AreEqual<TDateTime>(timestamp + 14, paymentTerm.DueDate.Value);

        // find the Skonto term
        paymentTerm := nil;
        for var i := 0 to loadedInvoice.PaymentTermsList.Count - 1 do
          if loadedInvoice.PaymentTermsList[i].PaymentTermsType.HasValue
            and (loadedInvoice.PaymentTermsList[i].PaymentTermsType.Value = TZUGFeRDPaymentTermsType.Skonto) then
          begin
            paymentTerm := loadedInvoice.PaymentTermsList[i];
            Break;
          end;
        Assert.IsNotNull(paymentTerm);
        Assert.AreEqual('3% Skonto innerhalb 10 Tagen bis 15.03.2018', paymentTerm.Description);
        Assert.AreEqual<Currency>(percentage, paymentTerm.Percentage.Value);
        Assert.AreEqual<Currency>(baseAmount, paymentTerm.BaseAmount.Value);
        Assert.AreEqual<Currency>(actualAmount, paymentTerm.ActualAmount.Value);
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

procedure TZUGFeRD22Tests.TestPaymentTermsMultiCardinalityWithBasic;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  timestamp: TDateTime;
  paymentTerm: TZUGFeRDPaymentTerms;
begin
  timestamp := Date;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.PaymentTermsList.Clear;
    desc.AddTradePaymentTerms(
      'Zahlbar innerhalb 30 Tagen netto bis 04.04.2018',
      TZUGFeRDNullableParam<TDateTime>.Create(EncodeDate(2018, 4, 4)));
    desc.AddTradePaymentTerms(
      '3% Skonto innerhalb 10 Tagen bis 15.03.2018',
      TZUGFeRDNullableParam<TDateTime>.Create(EncodeDate(2018, 3, 15)),
      TZUGFeRDNullableParam<TZUGFeRDPaymentTermsType>.Create(TZUGFeRDPaymentTermsType.Skonto),
      TZUGFeRDNullableParam<Integer>.Create(10),
      TZUGFeRDNullableParam<Currency>.Create(3));
    desc.PaymentTermsList[0].DueDate := timestamp + 14;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Basic);

      ms.Position := 0;
      Assert.AreEqual<TZUGFeRDVersion>(TZUGFeRDVersion.Version23, TZUGFeRDInvoiceDescriptor.GetVersion(ms));

      ms.Position := 0;
      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice.PaymentTermsList);
        Assert.AreEqual<Integer>(2, loadedInvoice.PaymentTermsList.Count);

        paymentTerm := nil;
        for var i := 0 to loadedInvoice.PaymentTermsList.Count - 1 do
          if loadedInvoice.PaymentTermsList[i].Description.StartsWith('Zahlbar') then
          begin
            paymentTerm := loadedInvoice.PaymentTermsList[i];
            Break;
          end;
        Assert.IsNotNull(paymentTerm);
        Assert.IsFalse(paymentTerm.PaymentTermsType.HasValue);
        Assert.AreEqual('Zahlbar innerhalb 30 Tagen netto bis 04.04.2018', paymentTerm.Description);
        Assert.AreEqual<TDateTime>(timestamp + 14, paymentTerm.DueDate.Value);

        paymentTerm := loadedInvoice.PaymentTermsList[loadedInvoice.PaymentTermsList.Count - 1];
        Assert.IsNotNull(paymentTerm);
        Assert.IsFalse(paymentTerm.PaymentTermsType.HasValue);
        Assert.AreEqual('3% Skonto innerhalb 10 Tagen bis 15.03.2018', paymentTerm.Description);
        Assert.IsFalse(paymentTerm.Percentage.HasValue);
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

procedure TZUGFeRD22Tests.TestPaymentTermsMultiCardinalityWithMinimum;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  timestamp: TDateTime;
begin
  timestamp := Date;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.PaymentTermsList.Clear;
    desc.AddTradePaymentTerms(
      'Zahlbar innerhalb 30 Tagen netto bis 04.04.2018',
      TZUGFeRDNullableParam<TDateTime>.Create(EncodeDate(2018, 4, 4)));
    desc.AddTradePaymentTerms(
      '3% Skonto innerhalb 10 Tagen bis 15.03.2018',
      TZUGFeRDNullableParam<TDateTime>.Create(EncodeDate(2018, 3, 15)),
      TZUGFeRDNullableParam<TZUGFeRDPaymentTermsType>.Create(TZUGFeRDPaymentTermsType.Skonto),
      TZUGFeRDNullableParam<Integer>.Create(10),
      TZUGFeRDNullableParam<Currency>.Create(3));
    desc.PaymentTermsList[0].DueDate := timestamp + 14;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Minimum);

      ms.Position := 0;
      Assert.AreEqual<TZUGFeRDVersion>(TZUGFeRDVersion.Version23, TZUGFeRDInvoiceDescriptor.GetVersion(ms));

      ms.Position := 0;
      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice.PaymentTermsList);
        Assert.AreEqual<Integer>(0, loadedInvoice.PaymentTermsList.Count);
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

procedure TZUGFeRD22Tests.TestPaymentTermsSingleCardinality;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  timestamp: TDateTime;
  paymentTerm: TZUGFeRDPaymentTerms;
begin
  timestamp := Date;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.PaymentTermsList.Clear;
    desc.AddTradePaymentTerms(
      'Zahlbar innerhalb 30 Tagen netto bis 04.04.2018',
      TZUGFeRDNullableParam<TDateTime>.Create(EncodeDate(2018, 4, 4)));
    desc.PaymentTermsList[0].DueDate := timestamp + 14;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Comfort);

      ms.Position := 0;
      Assert.AreEqual<TZUGFeRDVersion>(TZUGFeRDVersion.Version23, TZUGFeRDInvoiceDescriptor.GetVersion(ms));

      ms.Position := 0;
      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice.PaymentTermsList);
        Assert.AreEqual<Integer>(1, loadedInvoice.PaymentTermsList.Count);
        paymentTerm := loadedInvoice.PaymentTermsList[0];
        Assert.IsNotNull(paymentTerm);
        Assert.AreEqual('Zahlbar innerhalb 30 Tagen netto bis 04.04.2018', paymentTerm.Description);
        Assert.AreEqual<TDateTime>(timestamp + 14, paymentTerm.DueDate.Value);
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

procedure TZUGFeRD22Tests.TestPaymentTermsXRechnungStructuredEndsWithLineBreak;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  paymentTerm: TZUGFeRDPaymentTerms;
begin
  // Delphi reader decomposes #SKONTO# text into typed fields (unlike C# which preserves raw text).
  // This test verifies the structured parsing result.
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.PaymentTermsList.Clear;
    desc.AddTradePaymentTerms('',
      nil,
      TZUGFeRDNullableParam<TZUGFeRDPaymentTermsType>.Create(TZUGFeRDPaymentTermsType.Skonto),
      TZUGFeRDNullableParam<Integer>.Create(14),
      TZUGFeRDNullableParam<Currency>.Create(2.25));

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung);

      ms.Position := 0;
      Assert.AreEqual<TZUGFeRDVersion>(TZUGFeRDVersion.Version23, TZUGFeRDInvoiceDescriptor.GetVersion(ms));

      ms.Position := 0;
      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice.PaymentTermsList);
        Assert.AreEqual<Integer>(1, loadedInvoice.PaymentTermsList.Count);
        paymentTerm := loadedInvoice.PaymentTermsList[0];
        Assert.IsNotNull(paymentTerm);

        Assert.IsTrue(paymentTerm.PaymentTermsType.HasValue);
        Assert.AreEqual(Ord(TZUGFeRDPaymentTermsType.Skonto), Ord(paymentTerm.PaymentTermsType.Value));
        Assert.IsTrue(paymentTerm.DueDays.HasValue);
        Assert.AreEqual<Integer>(14, paymentTerm.DueDays.Value);
        Assert.IsTrue(paymentTerm.Percentage.HasValue);
        Assert.AreEqual<Currency>(2.25, paymentTerm.Percentage.Value);
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

procedure TZUGFeRD22Tests.TestPaymentTermsSingleCardinalityXRechnungStructured;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  paymentTerm: TZUGFeRDPaymentTerms;
begin
  // Delphi reader decomposes #SKONTO# text into typed fields.
  // Description contains only the plain-text portion, structured data is in typed properties.
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.PaymentTermsList.Clear;
    desc.AddTradePaymentTerms('14 Tage 2,25%',
      nil,
      TZUGFeRDNullableParam<TZUGFeRDPaymentTermsType>.Create(TZUGFeRDPaymentTermsType.Skonto),
      TZUGFeRDNullableParam<Integer>.Create(14),
      TZUGFeRDNullableParam<Currency>.Create(2.25));

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung);

      ms.Position := 0;
      Assert.AreEqual<TZUGFeRDVersion>(TZUGFeRDVersion.Version23, TZUGFeRDInvoiceDescriptor.GetVersion(ms));

      ms.Position := 0;
      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice.PaymentTermsList);
        Assert.AreEqual<Integer>(1, loadedInvoice.PaymentTermsList.Count);
        paymentTerm := loadedInvoice.PaymentTermsList[0];
        Assert.IsNotNull(paymentTerm);

        Assert.AreEqual('14 Tage 2,25%', Trim(paymentTerm.Description));
        Assert.IsTrue(paymentTerm.PaymentTermsType.HasValue);
        Assert.AreEqual(Ord(TZUGFeRDPaymentTermsType.Skonto), Ord(paymentTerm.PaymentTermsType.Value));
        Assert.IsTrue(paymentTerm.DueDays.HasValue);
        Assert.AreEqual<Integer>(14, paymentTerm.DueDays.Value);
        Assert.IsTrue(paymentTerm.Percentage.HasValue);
        Assert.AreEqual<Currency>(2.25, paymentTerm.Percentage.Value);
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

procedure TZUGFeRD22Tests.TestPaymentTermsMultiCardinalityXRechnungStructuredOnlyOneSpecifiedTradePaymentTermsPresent;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  timestamp: TDateTime;
begin
  // Delphi reader decomposes multi-term #SKONTO# into separate PaymentTerms entries.
  // C# keeps them as 1 entry with raw text; Delphi creates 2 entries with typed fields.
  timestamp := Date;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.PaymentTermsList.Clear;
    desc.AddTradePaymentTerms('',
      nil,
      TZUGFeRDNullableParam<TZUGFeRDPaymentTermsType>.Create(TZUGFeRDPaymentTermsType.Skonto),
      TZUGFeRDNullableParam<Integer>.Create(14),
      TZUGFeRDNullableParam<Currency>.Create(2.25));
    desc.PaymentTermsList[0].DueDate := timestamp + 14;
    desc.AddTradePaymentTerms('Description2',
      nil,
      TZUGFeRDNullableParam<TZUGFeRDPaymentTermsType>.Create(TZUGFeRDPaymentTermsType.Skonto),
      TZUGFeRDNullableParam<Integer>.Create(28),
      TZUGFeRDNullableParam<Currency>.Create(1));

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung);

      ms.Position := 0;
      Assert.AreEqual<TZUGFeRDVersion>(TZUGFeRDVersion.Version23, TZUGFeRDInvoiceDescriptor.GetVersion(ms));

      ms.Position := 0;
      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice.PaymentTermsList);
        Assert.AreEqual<Integer>(2, loadedInvoice.PaymentTermsList.Count);

        Assert.IsTrue(loadedInvoice.PaymentTermsList[0].PaymentTermsType.HasValue);
        Assert.AreEqual(Ord(TZUGFeRDPaymentTermsType.Skonto), Ord(loadedInvoice.PaymentTermsList[0].PaymentTermsType.Value));
        Assert.AreEqual<Integer>(14, loadedInvoice.PaymentTermsList[0].DueDays.Value);
        Assert.AreEqual<Currency>(2.25, loadedInvoice.PaymentTermsList[0].Percentage.Value);

        Assert.IsTrue(loadedInvoice.PaymentTermsList[1].PaymentTermsType.HasValue);
        Assert.AreEqual(Ord(TZUGFeRDPaymentTermsType.Skonto), Ord(loadedInvoice.PaymentTermsList[1].PaymentTermsType.Value));
        Assert.AreEqual<Integer>(28, loadedInvoice.PaymentTermsList[1].DueDays.Value);
        Assert.AreEqual<Currency>(1.0, loadedInvoice.PaymentTermsList[1].Percentage.Value);
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

procedure TZUGFeRD22Tests.TestPaymentTermsMultiCardinalityXRechnungStructured;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  timestamp: TDateTime;
begin
  // Delphi reader decomposes multi-term #SKONTO# into separate PaymentTerms entries.
  timestamp := Date;

  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.PaymentTermsList.Clear;
    desc.AddTradePaymentTerms('',
      nil,
      TZUGFeRDNullableParam<TZUGFeRDPaymentTermsType>.Create(TZUGFeRDPaymentTermsType.Skonto),
      TZUGFeRDNullableParam<Integer>.Create(14),
      TZUGFeRDNullableParam<Currency>.Create(2.25));
    desc.PaymentTermsList[0].DueDate := timestamp + 14;
    desc.AddTradePaymentTerms('Description2',
      nil,
      TZUGFeRDNullableParam<TZUGFeRDPaymentTermsType>.Create(TZUGFeRDPaymentTermsType.Skonto),
      TZUGFeRDNullableParam<Integer>.Create(28),
      TZUGFeRDNullableParam<Currency>.Create(1));

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung);

      ms.Position := 0;
      Assert.AreEqual<TZUGFeRDVersion>(TZUGFeRDVersion.Version23, TZUGFeRDInvoiceDescriptor.GetVersion(ms));

      ms.Position := 0;
      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsNotNull(loadedInvoice.PaymentTermsList);
        Assert.AreEqual<Integer>(2, loadedInvoice.PaymentTermsList.Count);

        // First term: Skonto 14 Tage 2.25%
        Assert.IsTrue(loadedInvoice.PaymentTermsList[0].PaymentTermsType.HasValue);
        Assert.AreEqual(Ord(TZUGFeRDPaymentTermsType.Skonto), Ord(loadedInvoice.PaymentTermsList[0].PaymentTermsType.Value));
        Assert.AreEqual<Integer>(14, loadedInvoice.PaymentTermsList[0].DueDays.Value);
        Assert.AreEqual<Currency>(2.25, loadedInvoice.PaymentTermsList[0].Percentage.Value);
        Assert.IsTrue(loadedInvoice.PaymentTermsList[0].DueDate.HasValue);

        // Second term: Skonto 28 Tage 1%
        Assert.AreEqual(Ord(TZUGFeRDPaymentTermsType.Skonto), Ord(loadedInvoice.PaymentTermsList[1].PaymentTermsType.Value));
        Assert.AreEqual<Integer>(28, loadedInvoice.PaymentTermsList[1].DueDays.Value);
        Assert.AreEqual<Currency>(1.0, loadedInvoice.PaymentTermsList[1].Percentage.Value);
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

procedure TZUGFeRD22Tests.TestSingleXRechnungStructuredManually;
var
  desc: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  reader: TStreamReader;
  content: string;
  lines: TStringList;
  insidePaymentTerms, insideDescription: Boolean;
  noteIndentation: Integer;
  trimmedLine: string;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.ClearTradePaymentTerms;
    desc.AddTradePaymentTerms('',
      nil,
      TZUGFeRDNullableParam<TZUGFeRDPaymentTermsType>.Create(TZUGFeRDPaymentTermsType.Skonto),
      TZUGFeRDNullableParam<Integer>.Create(14),
      TZUGFeRDNullableParam<Currency>.Create(2.25));

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung);
      ms.Position := 0;

      reader := TStreamReader.Create(ms);
      try
        content := reader.ReadToEnd;
      finally
        reader.Free;
      end;

      lines := TStringList.Create;
      try
        lines.Text := content;
        insidePaymentTerms := False;
        insideDescription := False;

        for var i := 0 to lines.Count - 1 do
        begin
          trimmedLine := Trim(lines[i]);

          if trimmedLine.StartsWith('<ram:SpecifiedTradePaymentTerms>') then
          begin
            insidePaymentTerms := True;
            Continue;
          end
          else if not insidePaymentTerms then
            Continue;

          if not insideDescription and trimmedLine.StartsWith('<ram:Description>') then
          begin
            insideDescription := True;
            noteIndentation := Length(lines[i]) - Length(TrimLeft(lines[i]));
            Assert.IsTrue(noteIndentation >= 0, 'Indentation for <ram:Description> should be non-negative.');
            Assert.AreEqual('<ram:Description>#SKONTO#TAGE=14#PROZENT=2.25#', trimmedLine);
            Continue;
          end;

          if insideDescription and trimmedLine.Contains('</ram:Description') then
            insideDescription := False;
        end;

        Assert.IsFalse(insideDescription, 'We should have exited the <ram:Description> block.');
      finally
        lines.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestOfficialXRechnungFileForPaymentTerms;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  // #SKONTO# Zeilen werden strukturiert geparst: 3 PaymentTerms, Description nur die Freitext-Zeilen.
  desc := TZUGFeRDInvoiceDescriptor.Load(DocumentationPath(
    'xRechnung\XRechnung 2.3.1\Schematron\generated\cii-br-de-18-freespace-test-599-identity.xml'));
  try
    Assert.AreEqual<Integer>(3, desc.PaymentTermsList.Count);

    // Erster PaymentTerm: bekommt Description (nur Freitext-Zeilen) + SKONTO-Daten
    Assert.IsTrue(desc.PaymentTermsList[0].Description.Contains('testentry'));
    Assert.AreEqual<Integer>(7, desc.PaymentTermsList[0].DueDays.Value);
    Assert.AreEqual<Currency>(2.00, desc.PaymentTermsList[0].Percentage.Value);

    // Zweiter PaymentTerm
    Assert.AreEqual<Integer>(14, desc.PaymentTermsList[1].DueDays.Value);
    Assert.AreEqual<Currency>(1.00, desc.PaymentTermsList[1].Percentage.Value);

    // Dritter PaymentTerm
    Assert.AreEqual<Integer>(30, desc.PaymentTermsList[2].DueDays.Value);
    Assert.AreEqual<Currency>(0.00, desc.PaymentTermsList[2].Percentage.Value);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestBuyerOrderReferenceLineId;
var
  desc: TZUGFeRDInvoiceDescriptor;
  s: TFileStream;
begin
  s := TFileStream.Create(DemodataPath('zugferd22\zugferd_2p2_EXTENDED_Fremdwaehrung-factur-x.xml'), fmOpenRead or fmShareDenyNone);
  try
    desc := TZUGFeRDInvoiceDescriptor.Load(s);
    try
      Assert.AreEqual('1', desc.TradeLineItems[0].BuyerOrderReferencedDocument.LineID);
      Assert.AreEqual('ORDER84359', desc.TradeLineItems[0].BuyerOrderReferencedDocument.ID);
    finally
      desc.Free;
    end;
  finally
    s.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestRequiredDirectDebitFieldsShouldExist;
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
    d._AddTradeLineItem(
      {lineID=}          '1',
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
      {taxPercent=}      19,
      {comment=}         '',
      {id=}              TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.EAN, '4012345001235'),
      {sellerAssignedID=} 'TB100A4'
    );
    d.SetSeller(
      'Lieferant GmbH', '80333', 'M'+#$00FC+'nchen',
      'Lieferantenstra'+#$00DF+'e 20',
      TZUGFeRDNullableParam<TZUGFeRDCountryCodes>.Create(TZUGFeRDCountryCodes.DE),
      '',
      TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.GLN, '4000001123452'),
      TZUGFeRDLegalOrganization.CreateWithParams(TZUGFeRDNullableParam<TZUGFeRDGlobalIDSchemeIdentifiers>.Create(TZUGFeRDGlobalIDSchemeIdentifiers.GLN), '4000001123452', 'Lieferant GmbH'));
    d.SetBuyer(
      'Kunden AG Mitte', '69876', 'Frankfurt',
      'Kundenstra'+#$00DF+'e 15',
      TZUGFeRDNullableParam<TZUGFeRDCountryCodes>.Create(TZUGFeRDCountryCodes.DE),
      'GE2020211',
      TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.GLN, '4000001987658'));
    d.SetPaymentMeansSepaDirectDebit(
      'DE98ZZZ09999999999',
      'REF A-123');
    d.AddDebitorFinancialAccount(
      'DE21860000000086001055', '');
    d.AddTradePaymentTerms(
      'Der Betrag in H'+#$00F6+'he von EUR 235,62 wird am 20.03.2018 von Ihrem Konto per SEPA-Lastschrift eingezogen.');
    d.SetTotals(198.00, 0, 0, 198.00, 37.62, 235.62, 0, 235.62);
    d.AddSellerTaxRegistration('201/113/40209', TZUGFeRDTaxRegistrationSchemeID.FC);
    d.AddSellerTaxRegistration('DE123456789', TZUGFeRDTaxRegistrationSchemeID.VA);
    d.AddApplicableTradeTax(198.00 / 100 * 19, 198.00, 19, TZUGFeRDTaxTypes.VAT, TZUGFeRDTaxCategoryCodes.S);

    ms := TMemoryStream.Create;
    try
      d.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung);
      ms.Position := 0;

      SetLength(bytes, ms.Size);
      ms.ReadBuffer(bytes, ms.Size);
      content := TEncoding.UTF8.GetString(bytes);

      Assert.IsTrue(content.Contains('<ram:CreditorReferenceID>DE98ZZZ09999999999</ram:CreditorReferenceID>'));
      Assert.IsTrue(content.Contains('<ram:DirectDebitMandateID>REF A-123</ram:DirectDebitMandateID>'));
    finally
      ms.Free;
    end;
  finally
    d.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestInNonDebitInvoiceTheDirectDebitFieldsShouldNotExist;
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
    d._AddTradeLineItem(
      {lineID=}          '1',
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
      {taxPercent=}      19,
      {comment=}         '',
      {id=}              TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.EAN, '4012345001235'),
      {sellerAssignedID=} 'TB100A4'
    );
    d.SetSeller(
      'Lieferant GmbH', '80333', 'M'+#$00FC+'nchen',
      'Lieferantenstra'+#$00DF+'e 20',
      TZUGFeRDNullableParam<TZUGFeRDCountryCodes>.Create(TZUGFeRDCountryCodes.DE),
      '',
      TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.GLN, '4000001123452'),
      TZUGFeRDLegalOrganization.CreateWithParams(TZUGFeRDNullableParam<TZUGFeRDGlobalIDSchemeIdentifiers>.Create(TZUGFeRDGlobalIDSchemeIdentifiers.GLN), '4000001123452', 'Lieferant GmbH'));
    d.SetBuyer(
      'Kunden AG Mitte', '69876', 'Frankfurt',
      'Kundenstra'+#$00DF+'e 15',
      TZUGFeRDNullableParam<TZUGFeRDCountryCodes>.Create(TZUGFeRDCountryCodes.DE),
      'GE2020211',
      TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.GLN, '4000001987658'));
    d.SetPaymentMeans(TZUGFeRDPaymentMeansTypeCodes.SEPACreditTransfer,
      'Information of Payment Means',
      'DE98ZZZ09999999999',
      'REF A-123');
    d.AddDebitorFinancialAccount(
      'DE21860000000086001055', '');
    d.AddTradePaymentTerms(
      'Der Betrag in H'+#$00F6+'he von EUR 235,62 wird am 20.03.2018 von Ihrem Konto per SEPA-Lastschrift eingezogen.');
    d.SetTotals(198.00, 0, 0, 198.00, 37.62, 235.62, 0, 235.62);
    d.AddSellerTaxRegistration('201/113/40209', TZUGFeRDTaxRegistrationSchemeID.FC);
    d.AddSellerTaxRegistration('DE123456789', TZUGFeRDTaxRegistrationSchemeID.VA);
    d.AddApplicableTradeTax(198.00 / 100 * 19, 198.00, 19, TZUGFeRDTaxTypes.VAT, TZUGFeRDTaxCategoryCodes.S);

    ms := TMemoryStream.Create;
    try
      d.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung);
      ms.Position := 0;

      SetLength(bytes, ms.Size);
      ms.ReadBuffer(bytes, ms.Size);
      content := TEncoding.UTF8.GetString(bytes);

      Assert.IsFalse(content.Contains('<ram:CreditorReferenceID>DE98ZZZ09999999999</ram:CreditorReferenceID>'));
      Assert.IsFalse(content.Contains('<ram:DirectDebitMandateID>REF A-123</ram:DirectDebitMandateID>'));
    finally
      ms.Free;
    end;
  finally
    d.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestSpecifiedTradePaymentTermsDueDate;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DocumentationPath(
    'zugferd24-facturx1008\de\Beispiele\2. BASIC\BASIC_Einfach\BASIC_Einfach.xml'));
  try
    Assert.IsTrue(desc.PaymentTermsList[0].DueDate.HasValue);
    Assert.AreEqual(EncodeDate(2020, 4, 4), desc.PaymentTermsList[0].DueDate.Value);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestSpecifiedTradePaymentTermsDescription;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DocumentationPath(
    'zugferd24-facturx1008\de\Beispiele\4. EXTENDED\EXTENDED_Warenrechnung\EXTENDED_Warenrechnung.xml'));
  try
    Assert.IsNotEmpty(desc.PaymentTermsList[0].Description);
    Assert.AreEqual(true,SameText('Bei Zahlung innerhalb 14 Tagen gewähren wir 2,0% Skonto.', desc.PaymentTermsList[0].Description));
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestSpecifiedTradePaymentTermsCalculationPercent;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DocumentationPath(
    'zugferd24-facturx1008\de\Beispiele\4. EXTENDED\EXTENDED_Warenrechnung\EXTENDED_Warenrechnung.xml'));
  try
    Assert.IsTrue(desc.PaymentTermsList[0].Percentage.HasValue);
    Assert.AreEqual<Currency>(2, desc.PaymentTermsList[0].Percentage.Value);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestTradeLineItemUnitChargeFreePackageQuantity;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DocumentationPath(
    'zugferd24-facturx1008\de\Beispiele\4. EXTENDED\EXTENDED_Warenrechnung\EXTENDED_Warenrechnung.xml'));
  try
    Assert.IsFalse(desc.TradeLineItems[0].NetQuantity.HasValue);
    Assert.IsFalse(desc.TradeLineItems[0].ChargeFreeQuantity.HasValue);
    Assert.IsTrue(desc.TradeLineItems[0].PackageQuantity.HasValue);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestApplicableTradeDeliveryTermsExists;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.ApplicableTradeDeliveryTermsCode := TZUGFeRDTradeDeliveryTermCodes.CFR;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.Extended, loadedInvoice.Profile);
        Assert.AreEqual<TZUGFeRDTradeDeliveryTermCodes>(TZUGFeRDTradeDeliveryTermCodes.CFR, loadedInvoice.ApplicableTradeDeliveryTermsCode.Value);
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

procedure TZUGFeRD22Tests.TestApplicableTradeDeliveryTermsIsNull;
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
        Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.Extended, loadedInvoice.Profile);
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

procedure TZUGFeRD22Tests.TestInvoiceExemptions;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  tax, taxLoaded: TZUGFeRDTax;
  tradeLineItem: TZUGFeRDTradeLineItem;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DocumentationPath(
    'zugferd24-facturx1008\de\Beispiele\4. EXTENDED\EXTENDED_InnergemeinschLieferungMehrereBestellungen\EXTENDED_InnergemeinschLieferungMehrereBestellungen.xml'));
  try
    tax := desc.Taxes[0];

    Assert.AreEqual('Kein Ausweis der Umsatzsteuer bei innergemeinschaftlichen Lieferungen', Trim(tax.ExemptionReason));
    Assert.AreEqual<TZUGFeRDTaxCategoryCodes>(TZUGFeRDTaxCategoryCodes.K, tax.CategoryCode);
    Assert.AreEqual<TZUGFeRDTaxTypes>(TZUGFeRDTaxTypes.VAT, tax.TypeCode);
    Assert.AreEqual<Currency>(0, tax.Percent);
    Assert.IsFalse(tax.ExemptionReasonCode.HasValue);

    for tradeLineItem in desc.TradeLineItems do
    begin
      Assert.AreEqual('Kein Ausweis der Umsatzsteuer bei innergemeinschaftlichen Lieferungen', Trim(tradeLineItem.TaxExemptionReason));
      Assert.AreEqual<TZUGFeRDTaxCategoryCodes>(TZUGFeRDTaxCategoryCodes.K, tradeLineItem.TaxCategoryCode);
      Assert.AreEqual<TZUGFeRDTaxTypes>(TZUGFeRDTaxTypes.VAT, tradeLineItem.TaxType);
      Assert.AreEqual<Currency>(0, tradeLineItem.TaxPercent);
      Assert.IsFalse(tradeLineItem.TaxExemptionReasonCode.HasValue);
    end;

    tax.ExemptionReason := 'Steuerfreie innergemeinschaftlichen Lieferung';
    tax.ExemptionReasonCode := TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_IC;

    for tradeLineItem in desc.TradeLineItems do
    begin
      tradeLineItem.TaxExemptionReason := 'Steuerfreie innergemeinschaftlichen Lieferung';
      tradeLineItem.TaxExemptionReasonCode := TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_IC;
    end;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;
      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        taxLoaded := loadedInvoice.Taxes[0];

        Assert.AreEqual('Steuerfreie innergemeinschaftlichen Lieferung', taxLoaded.ExemptionReason);
        Assert.AreEqual<TZUGFeRDTaxCategoryCodes>(TZUGFeRDTaxCategoryCodes.K, taxLoaded.CategoryCode);
        Assert.AreEqual<TZUGFeRDTaxTypes>(TZUGFeRDTaxTypes.VAT, taxLoaded.TypeCode);
        Assert.AreEqual<Currency>(0, taxLoaded.Percent);
        Assert.AreEqual<TZUGFeRDTaxExemptionReasonCodes>(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_IC, taxLoaded.ExemptionReasonCode.Value);

        for tradeLineItem in loadedInvoice.TradeLineItems do
        begin
          Assert.AreEqual('Steuerfreie innergemeinschaftlichen Lieferung', tradeLineItem.TaxExemptionReason);
          Assert.AreEqual<TZUGFeRDTaxCategoryCodes>(TZUGFeRDTaxCategoryCodes.K, tradeLineItem.TaxCategoryCode);
          Assert.AreEqual<TZUGFeRDTaxTypes>(TZUGFeRDTaxTypes.VAT, tradeLineItem.TaxType);
          Assert.AreEqual<Currency>(0, tradeLineItem.TaxPercent);
          Assert.AreEqual<TZUGFeRDTaxExemptionReasonCodes>(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_IC, tradeLineItem.TaxExemptionReasonCode.Value);
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

procedure TZUGFeRD22Tests.TestOriginTradeCountry;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.TradeLineItems[0].OriginTradeCountry := TZUGFeRDCountryCodes.DE;

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        Assert.IsTrue(loadedInvoice.TradeLineItems[0].OriginTradeCountry.HasValue);
        Assert.AreEqual<TZUGFeRDCountryCodes>(TZUGFeRDCountryCodes.DE, loadedInvoice.TradeLineItems[0].OriginTradeCountry.Value);
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

procedure TZUGFeRD22Tests.TestLoadingCurrency;
var
  desc: TZUGFeRDInvoiceDescriptor;
  s: TFileStream;
begin
  s := TFileStream.Create(DemodataPath('zugferd21\zugferd_2p1_EXTENDED_Warenrechnung-factur-x.xml'), fmOpenRead or fmShareDenyNone);
  try
    desc := TZUGFeRDInvoiceDescriptor.Load(s);
    try
      Assert.AreEqual<TZUGFeRDCurrencyCodes>(TZUGFeRDCurrencyCodes.EUR, desc.Currency);
      Assert.IsFalse(desc.TaxCurrency.HasValue);
    finally
      desc.Free;
    end;
  finally
    s.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestLoadingSellerCountry;
var
  desc: TZUGFeRDInvoiceDescriptor;
  s: TFileStream;
begin
  s := TFileStream.Create(DemodataPath('zugferd21\zugferd_2p1_EXTENDED_Warenrechnung-factur-x.xml'), fmOpenRead or fmShareDenyNone);
  try
    desc := TZUGFeRDInvoiceDescriptor.Load(s);
    try
      Assert.AreEqual<TZUGFeRDCountryCodes>(TZUGFeRDCountryCodes.DE, desc.Seller.Country.Value);
    finally
      desc.Free;
    end;
  finally
    s.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestLoadingBuyerCountry;
var
  desc: TZUGFeRDInvoiceDescriptor;
  s: TFileStream;
begin
  s := TFileStream.Create(DemodataPath('zugferd21\zugferd_2p1_EXTENDED_Warenrechnung-factur-x.xml'), fmOpenRead or fmShareDenyNone);
  try
    desc := TZUGFeRDInvoiceDescriptor.Load(s);
    try
      Assert.AreEqual<TZUGFeRDCountryCodes>(TZUGFeRDCountryCodes.DE, desc.Buyer.Country.Value);
    finally
      desc.Free;
    end;
  finally
    s.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestLoadingInvoiceType;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  // load standard invoice
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd21\zugferd_2p1_EXTENDED_Warenrechnung-factur-x.xml'));
  try
    Assert.AreEqual<TZUGFeRDInvoiceType>(TZUGFeRDInvoiceType.Invoice, desc.Type_);
  finally
    desc.Free;
  end;

  // Note: Correction invoice test skipped - documentation path not available
end;

procedure TZUGFeRD22Tests.TestNonRSMInvoice;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('xRechnung\non_rsm_zugferd-invoice.xml'));
  try
    Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.Extended, desc.Profile);
    Assert.AreEqual('47110818', desc.InvoiceNo);
    Assert.AreEqual<TDateTime>(EncodeDate(2018, 10, 31), desc.InvoiceDate);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestRSMInvoice;
var
  desc: TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('xRechnung\xRechnung CII.xml'));
  try
    Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.XRechnung1, desc.Profile);
    Assert.AreEqual('0815-99-1-a', desc.InvoiceNo);
    Assert.AreEqual<TDateTime>(EncodeDate(2020, 06, 21), desc.InvoiceDate);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestAlternateNamespace;
var
  invoice: TZUGFeRDInvoiceDescriptor;
begin
  invoice := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd22\zugferd_23_namespace.xml'));
  try
    Assert.AreEqual<TDateTime>(EncodeDate(2025, 10, 24), invoice.InvoiceDate);
    Assert.AreEqual('0000000016', invoice.InvoiceNo);
  finally
    invoice.Free;
  end;
end;

procedure TZUGFeRD22Tests.TestLocalNamespace;
var
  invoice: TZUGFeRDInvoiceDescriptor;
begin
  invoice := TZUGFeRDInvoiceDescriptor.Load(DemodataPath('zugferd22\zugferd_23_local_namespaces.xml'));
  try
    Assert.AreEqual<TZUGFeRDProfile>(TZUGFeRDProfile.Extended, invoice.Profile);
  finally
    invoice.Free;
  end;
end;

/// <summary>
/// This test ensures that BIC won't be written if empty
/// </summary>
procedure TZUGFeRD22Tests.TestFinancialInstitutionBICEmpty;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    // PayeeSpecifiedCreditorFinancialInstitution: clear BIC
    desc.CreditorBankAccounts[0].BIC := '';
    // PayerSpecifiedDebtorFinancialInstitution: add debitor account with empty BIC
    desc.AddDebitorFinancialAccount('DE02120300000000202051', '');

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Comfort);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        // No PayeeSpecifiedCreditorFinancialInstitution shall be present if BIC is empty
        Assert.AreEqual('', loadedInvoice.CreditorBankAccounts[0].BIC);
        // No PayerSpecifiedDebtorFinancialInstitution is ever written for debitor accounts
        Assert.AreEqual('', loadedInvoice.DebitorBankAccounts[0].BIC);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end; // !TestFinancialInstitutionBICEmpty()

/// <summary>
/// This test ensures that no BIC is created for the debitor account even if it is specified
/// </summary>
procedure TZUGFeRD22Tests.TestNoBICIDForDebitorFinancialInstitution;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    // PayerSpecifiedDebtorFinancialInstitution: add debitor account with BIC
    desc.AddDebitorFinancialAccount('DE02120300000000202051', 'MYBIC');

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Comfort);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        // PayerSpecifiedDebtorFinancialInstitution is never written, so BIC is always empty for debitor
        Assert.AreEqual('', loadedInvoice.DebitorBankAccounts[0].BIC);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end; // !TestNoBICIDForDebitorFinancialInstitution()

procedure TZUGFeRD22Tests.TestBasisQuantityStandard;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  lineItem: TZUGFeRDTradeLineItem;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.TradeLineItems.Clear;
    lineItem := desc.AddTradeLineItem(
      {name=}           'Joghurt Banane',
      {netUnitPrice=}   TZUGFeRDNullableParam<Currency>.Create(5.5),
      {description=}    '',
      {unitCode=}       TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.H87),
      {unitQuantity=}   nil,  // no basis quantity: standard = 1
      {grossUnitPrice=} TZUGFeRDNullableParam<Currency>.Create(5.5),
      {billedQuantity=} 50,
      {lineTotalAmount=} 0,  // will be cleared below
      {taxType=}        TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
      {categoryCode=}   TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
      {taxPercent=}     7,
      {comment=}        '',
      {id=}             TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.EAN, '4000050986428'),
      {sellerAssignedID=} 'ARNR2'
    );
    // Override LineTotalAmount with HasValue=false so writer calculates netUnitPrice * billedQuantity
    lineItem.LineTotalAmount := ZUGFeRDNullable<Currency>.Create(False);

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        // 5.5 * 50 / 1 = 275.00
        Assert.AreEqual<Currency>(275.00, loadedInvoice.TradeLineItems[0].LineTotalAmount.Value);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end; // !TestBasisQuantityStandard()

procedure TZUGFeRD22Tests.TestBasisQuantityMultiple;
var
  desc, loadedInvoice: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  lineItem: TZUGFeRDTradeLineItem;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.TradeLineItems.Clear;
    lineItem := desc.AddTradeLineItem(
      {name=}           'Joghurt Banane',
      {netUnitPrice=}   TZUGFeRDNullableParam<Currency>.Create(5.5),
      {description=}    '',
      {unitCode=}       TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.H87),
      {unitQuantity=}   TZUGFeRDNullableParam<Currency>.Create(10),  // basis quantity = 10
      {grossUnitPrice=} TZUGFeRDNullableParam<Currency>.Create(5.5),
      {billedQuantity=} 50,
      {lineTotalAmount=} 0,  // will be cleared below
      {taxType=}        TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
      {categoryCode=}   TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
      {taxPercent=}     7,
      {comment=}        '',
      {id=}             TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.EAN, '4000050986428'),
      {sellerAssignedID=} 'ARNR2'
    );
    // Clear LineTotalAmount so the writer calculates it as netUnitPrice * billedQuantity / unitQuantity
    lineItem.LineTotalAmount := ZUGFeRDNullable<Currency>.Create(False);  // HasValue=false, writer will calculate

    ms := TMemoryStream.Create;
    try
      desc.Save(ms, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung);
      ms.Position := 0;

      loadedInvoice := TZUGFeRDInvoiceDescriptor.Load(ms);
      try
        // 5.5 * 50 / 10 = 27.50
        Assert.AreEqual<Currency>(27.50, loadedInvoice.TradeLineItems[0].LineTotalAmount.Value);
      finally
        loadedInvoice.Free;
      end;
    finally
      ms.Free;
    end;
  finally
    desc.Free;
  end;
end; // !TestBasisQuantityMultiple()

procedure TZUGFeRD22Tests.TestAccountingCost;
var
  d, d2: TZUGFeRDInvoiceDescriptor;
  stream: TMemoryStream;
  accounts: TObjectList<TZUGFeRDReceivableSpecifiedTradeAccountingAccount>;
  found: Boolean;
  i: Integer;
begin
  d := TZUGFeRDInvoiceDescriptor.CreateInvoice('471103', EncodeDate(2025, 11, 11), TZUGFeRDCurrencyCodes.EUR);
  try
    d.Type_ := TZUGFeRDInvoiceType.Invoice;
    d.AddReceivableSpecifiedTradeAccountingAccount('BRE');

    stream := TMemoryStream.Create;
    try
      d.Save(stream, TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung, TZUGFeRDFormats.UBL);
      stream.Position := 0;

      d2 := TZUGFeRDInvoiceDescriptor.Load(stream);
      try
        accounts := d2.ReceivableSpecifiedTradeAccountingAccounts;
        found := False;
        for i := 0 to accounts.Count - 1 do
          if accounts[i].TradeAccountID = 'BRE' then
          begin
            found := True;
            Break;
          end;
        Assert.IsTrue(found);
      finally
        d2.Free;
      end;
    finally
      stream.Free;
    end;
  finally
    d.Free;
  end;
end; // !TestAccountingCost()

/// <summary>
/// Der direkt genutzte CII-Writer muss mit Save(filename) eine lesbare Datei schreiben.
/// Vorher lieferte sein Validate konstant false, und es entstand weder Datei noch Fehler.
/// </summary>
procedure TZUGFeRD22Tests.TestCIIWriterDirectSaveToFileWritesFile;
var
  desc, loaded: TZUGFeRDInvoiceDescriptor;
  writer: TZUGFeRDInvoiceDescriptor23CIIWriter; // bewusst konkret typisiert
  fileName: string;
begin
  fileName := TempInvoiceFileName;
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.Profile := TZUGFeRDProfile.Extended;
    writer := TZUGFeRDInvoiceDescriptor23CIIWriter.Create;
    try
      writer.Save(desc, fileName);
    finally
      writer.Free;
    end;

    Assert.IsTrue(TFile.Exists(fileName), 'CII-Writer hat keine Datei geschrieben.');
    loaded := TZUGFeRDInvoiceDescriptor.Load(fileName);
    try
      Assert.AreEqual(desc.InvoiceNo, loaded.InvoiceNo);
      Assert.AreEqual(TZUGFeRDProfile.Extended, loaded.Profile);
    finally
      loaded.Free;
    end;
  finally
    desc.Free;
    if TFile.Exists(fileName) then
      TFile.Delete(fileName);
  end;
end;

/// <summary>
/// Der direkt genutzte UBL-Writer muss mit Save(filename) eine lesbare Datei schreiben.
/// Vorher warf sein Validate ENotImplemented.
/// </summary>
procedure TZUGFeRD22Tests.TestUBLWriterDirectSaveToFileWritesFile;
var
  desc, loaded: TZUGFeRDInvoiceDescriptor;
  writer: TZUGFeRDInvoiceDescriptor22UBLWriter; // bewusst konkret typisiert
  fileName: string;
begin
  fileName := TempInvoiceFileName;
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.Profile := TZUGFeRDProfile.XRechnung;
    writer := TZUGFeRDInvoiceDescriptor22UBLWriter.Create;
    try
      writer.Save(desc, fileName, TZUGFeRDFormats.UBL);
    finally
      writer.Free;
    end;

    Assert.IsTrue(TFile.Exists(fileName), 'UBL-Writer hat keine Datei geschrieben.');
    loaded := TZUGFeRDInvoiceDescriptor.Load(fileName);
    try
      Assert.AreEqual(desc.InvoiceNo, loaded.InvoiceNo);
      Assert.AreEqual(TZUGFeRDProfile.XRechnung, loaded.Profile);
    finally
      loaded.Free;
    end;
  finally
    desc.Free;
    if TFile.Exists(fileName) then
      TFile.Delete(fileName);
  end;
end;

/// <summary>
/// Der Dispatcher lässt UBL nur mit dem Profil XRechnung zu; der direkt genutzte
/// UBL-Writer muss dieselbe Grenze ziehen.
/// </summary>
procedure TZUGFeRD22Tests.TestUBLWriterDirectRejectsNonXRechnungProfile;
var
  desc: TZUGFeRDInvoiceDescriptor;
  writer: TZUGFeRDInvoiceDescriptor22UBLWriter;
  fileName: string;
begin
  fileName := TempInvoiceFileName;
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.Profile := TZUGFeRDProfile.Extended;
    writer := TZUGFeRDInvoiceDescriptor22UBLWriter.Create;
    try
      Assert.IsFalse(writer.Validate(desc, False), 'UBL-Writer: Validate muss Extended ablehnen.');
      Assert.WillRaise(
        procedure
        begin
          writer.Save(desc, fileName, TZUGFeRDFormats.UBL);
        end,
        TZUGFeRDUnsupportedException);
      Assert.IsFalse(TFile.Exists(fileName), 'UBL-Writer darf für Extended keine Datei anlegen.');

      // Auch der Stream-Überlade darf das Profil nicht durchlassen
      Assert.WillRaise(
        procedure
        var
          ms: TMemoryStream;
        begin
          ms := TMemoryStream.Create;
          try
            writer.Save(desc, ms, TZUGFeRDFormats.UBL);
          finally
            ms.Free;
          end;
        end,
        TZUGFeRDUnsupportedException);
    finally
      writer.Free;
    end;
  finally
    desc.Free;
    if TFile.Exists(fileName) then
      TFile.Delete(fileName);
  end;
end;

/// <summary>
/// CII- und UBL-Writer müssen bei direkter Nutzung dieselben Regeln anwenden wie der
/// Dispatcher TZUGFeRDInvoiceDescriptor23Writer: hier Steuerart ungleich VAT im Profil Comfort.
/// </summary>
procedure TZUGFeRD22Tests.TestDirectWritersValidateLikeDispatcher;
var
  desc: TZUGFeRDInvoiceDescriptor;
  writer: TZUGFeRDIInvoiceDescriptorWriter;
  fileName: string;
begin
  fileName := TempInvoiceFileName;
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.Profile := TZUGFeRDProfile.Comfort;
    desc.TradeLineItems[0].TaxType := TZUGFeRDTaxTypes.AAA;

    writer := TZUGFeRDInvoiceDescriptor23Writer.Create;
    try
      Assert.IsFalse(writer.Validate(desc, False), 'Dispatcher: Validate muss false liefern.');
    finally
      writer.Free;
    end;

    writer := TZUGFeRDInvoiceDescriptor23CIIWriter.Create;
    try
      Assert.IsFalse(writer.Validate(desc, False), 'CII-Writer: Validate muss false liefern.');
      Assert.WillRaise(
        procedure
        begin
          writer.Save(desc, fileName);
        end,
        TZUGFeRDUnsupportedException);
      Assert.IsFalse(TFile.Exists(fileName), 'CII-Writer darf bei ungültigem Descriptor keine Datei anlegen.');
    finally
      writer.Free;
    end;

    // Für den UBL-Writer ein Profil wählen, das seine eigene Profilgrenze passiert
    // (nur XRechnung ist erlaubt). Scheitern darf der Aufruf dann ausschließlich an der
    // gemeinsamen Prüfung ValidateVersion23 - sonst würde der Test auch dann bestehen,
    // wenn der Writer nur sein Profil prüft und die gemeinsame Prüfung auslässt.
    desc.Profile := TZUGFeRDProfile.XRechnung;

    writer := TZUGFeRDInvoiceDescriptor22UBLWriter.Create;
    try
      Assert.IsFalse(writer.Validate(desc, False), 'UBL-Writer: Validate muss false liefern.');
      Assert.WillRaise(
        procedure
        begin
          writer.Save(desc, fileName, TZUGFeRDFormats.UBL);
        end,
        TZUGFeRDUnsupportedException);
      Assert.IsFalse(TFile.Exists(fileName), 'UBL-Writer darf bei ungültigem Descriptor keine Datei anlegen.');
    finally
      writer.Free;
    end;
  finally
    desc.Free;
    if TFile.Exists(fileName) then
      TFile.Delete(fileName);
  end;
end;

/// <summary>
/// Liefert Validate false ohne Exception, darf Save(filename) nicht stillschweigend nichts tun.
/// </summary>
procedure TZUGFeRD22Tests.TestWriterSaveToFileRaisesWhenValidateReturnsFalse;
var
  desc: TZUGFeRDInvoiceDescriptor;
  writer: TZUGFeRDIInvoiceDescriptorWriter;
  fileName: string;
begin
  fileName := TempInvoiceFileName;
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    writer := TValidateFalseWriter.Create;
    try
      Assert.WillRaise(
        procedure
        begin
          writer.Save(desc, fileName);
        end,
        TZUGFeRDUnsupportedException);
    finally
      writer.Free;
    end;
    Assert.IsFalse(TFile.Exists(fileName), 'Bei fehlgeschlagener Validierung darf keine Datei entstehen.');
  finally
    desc.Free;
    if TFile.Exists(fileName) then
      TFile.Delete(fileName);
  end;
end;

/// <summary>
/// Scheitert das Schreiben erst am Zielformat (Validate kennt das Format nicht), darf eine
/// bereits vorhandene Datei nicht geleert werden. Geprüft über den öffentlichen Weg
/// TZUGFeRDInvoiceDescriptor.Save mit der unzulässigen Kombination Extended + UBL.
/// </summary>
procedure TZUGFeRD22Tests.TestFailedSaveKeepsExistingFileUntouched;
const
  ExistingContent = '<vorhandene-datei/>';
var
  desc: TZUGFeRDInvoiceDescriptor;
  fileName: string;
begin
  fileName := TempInvoiceFileName;
  TFile.WriteAllText(fileName, ExistingContent);
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    Assert.WillRaise(
      procedure
      begin
        desc.Save(fileName, TZUGFeRDVersion.Version23, TZUGFeRDProfile.Extended,
                  TZUGFeRDFormats.UBL);
      end,
      TZUGFeRDUnsupportedException);

    Assert.IsTrue(TFile.Exists(fileName), 'Die vorhandene Datei darf nicht verschwinden.');
    Assert.AreEqual(ExistingContent, TFile.ReadAllText(fileName),
      'Die vorhandene Datei darf durch den gescheiterten Schreibversuch nicht verändert werden.');
  finally
    desc.Free;
    if TFile.Exists(fileName) then
      TFile.Delete(fileName);
  end;
end;

/// <summary>
/// Der Guideline-URN für XRechnung darf nicht vom Systemdatum abhängen: geschrieben wird
/// immer XRechnung 3.0, die älteren 2.x-URNs bleiben lesbar.
/// </summary>
procedure TZUGFeRD22Tests.TestXRechnungGuidelineUrnIsXRechnung30;
begin
  Assert.AreEqual('urn:cen.eu:en16931:2017#compliant#urn:xeinkauf.de:kosit:xrechnung_3.0',
    TZUGFeRDProfileExtensions.EnumToString(TZUGFeRDProfile.XRechnung, TZUGFeRDVersion.Version23));

  Assert.AreEqual(TZUGFeRDProfile.XRechnung,
    TZUGFeRDProfileExtensions.StringToEnum('urn:cen.eu:en16931:2017#compliant#urn:xeinkauf.de:kosit:xrechnung_3.0'));
  Assert.AreEqual(TZUGFeRDProfile.XRechnung,
    TZUGFeRDProfileExtensions.StringToEnum('urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_2.3'));
  Assert.AreEqual(TZUGFeRDProfile.XRechnung,
    TZUGFeRDProfileExtensions.StringToEnum('urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_2.2'));
  Assert.AreEqual(TZUGFeRDProfile.XRechnung,
    TZUGFeRDProfileExtensions.StringToEnum('urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_2.1'));
  Assert.AreEqual(TZUGFeRDProfile.XRechnung,
    TZUGFeRDProfileExtensions.StringToEnum('urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_2.0'));
end;

/// <summary>
/// BR-DE-17 ist eine XRechnung-Regel. Eine Vorauszahlungsrechnung (386) ist in Factur-X
/// EXTENDED zulässig und darf nur im Profil XRechnung abgelehnt werden.
/// </summary>
procedure TZUGFeRD22Tests.TestPrepaymentInvoiceAllowedOutsideXRechnung;
var
  desc, loaded: TZUGFeRDInvoiceDescriptor;
  writer: TZUGFeRDIInvoiceDescriptorWriter;
  fileName: string;
begin
  fileName := TempInvoiceFileName;
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.Type_ := TZUGFeRDInvoiceType.PrepaymentInvoice;

    writer := TZUGFeRDInvoiceDescriptor23Writer.Create;
    try
      desc.Profile := TZUGFeRDProfile.Extended;
      Assert.IsTrue(writer.Validate(desc, False), 'Extended: Vorauszahlungsrechnung muss zulässig sein.');
      writer.Save(desc, fileName);
      Assert.IsTrue(TFile.Exists(fileName), 'Extended: Datei muss geschrieben werden.');

      loaded := TZUGFeRDInvoiceDescriptor.Load(fileName);
      try
        Assert.AreEqual(TZUGFeRDInvoiceType.PrepaymentInvoice, loaded.Type_);
      finally
        loaded.Free;
      end;
      TFile.Delete(fileName);

      desc.Profile := TZUGFeRDProfile.XRechnung;
      Assert.IsFalse(writer.Validate(desc, False), 'XRechnung: BR-DE-17 muss weiterhin greifen.');
      Assert.WillRaise(
        procedure
        begin
          writer.Save(desc, fileName);
        end,
        TZUGFeRDUnsupportedException);
      Assert.IsFalse(TFile.Exists(fileName), 'XRechnung: keine Datei bei BR-DE-17-Verstoß.');

      desc.Profile := TZUGFeRDProfile.XRechnung1;
      Assert.IsFalse(writer.Validate(desc, False), 'XRechnung1: BR-DE-17 muss ebenfalls greifen.');
    finally
      writer.Free;
    end;
  finally
    desc.Free;
    if TFile.Exists(fileName) then
      TFile.Delete(fileName);
  end;
end;

end.
