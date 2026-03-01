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

unit intf.ZUGFeRDInvoiceDescriptor22UBLWriter;

interface

uses
  System.SysUtils,System.Classes,System.StrUtils,System.Generics.Collections
  ,intf.ZUGFeRDInvoiceDescriptor
  ,intf.ZUGFeRDInvoiceTypes
  ,intf.ZUGFeRDProfileAwareXmlTextWriter
  ,intf.ZUGFeRDIInvoiceDescriptorwriter
  ,intf.ZUGFeRDProfile
  ,intf.ZUGFeRDExceptions
  ,intf.ZUGFeRDHelper
  ,intf.ZUGFeRDCurrencyCodes
  ,intf.ZUGFeRDVersion
  ,intf.ZUGFeRDNote
  ,intf.ZUGFeRDContentCodes
  ,intf.ZUGFeRDSubjectCodes
  ,intf.ZUGFeRDContact
  ,intf.ZUGFeRDParty
  ,intf.ZUGFeRDTaxRegistration
  ,intf.ZUGFeRDGlobalIDSchemeIdentifiers
  ,intf.ZUGFeRDCountryCodes
  ,intf.ZUGFeRDTaxRegistrationSchemeID
  ,intf.ZUGFeRDTax
  ,intf.ZUGFeRDTaxTypes
  ,intf.ZUGFeRDTaxCategoryCodes
  ,intf.ZUGFeRDTradeLineItem
  ,intf.ZUGFeRDAdditionalReferencedDocument
  ,intf.ZUGFeRDAdditionalReferencedDocumentTypeCodes
  ,intf.ZUGFeRDReferenceTypeCodes
  ,intf.ZUGFeRDPaymentMeansTypeCodes
  ,intf.ZUGFeRDBankAccount
  ,intf.ZUGFeRDTradeAllowanceCharge
  ,intf.ZUGFeRDPaymentTerms
  ,intf.ZUGFeRDServiceCharge
  ,intf.ZUGFeRDQuantityCodes
  ,intf.ZUGFeRDLegalOrganization
  ,intf.ZUGFeRDPartyTypes
  ,intf.ZUGFeRDElectronicAddress
  ,intf.ZUGFeRDElectronicAddressSchemeIdentifiers
  ,intf.ZUGFeRDTaxExemptionReasonCodes
  ,intf.ZUGFeRDApplicableProductCharacteristic
  ,intf.ZUGFeRDReceivableSpecifiedTradeAccountingAccount
  ,intf.ZUGFeRDAccountingAccountTypeCodes
  ,intf.ZUGFeRDMimeTypeMapper
  ,intf.ZUGFeRDSpecialServiceDescriptionCodes
  ,intf.ZUGFeRDFormats
  ,intf.ZUGFeRDDesignatedProductClassification
  ,intf.ZUGFeRDDesignatedProductClassificationClassCodes
  ,intf.ZUGFeRDIncludedReferencedProduct
  ,intf.ZUGFeRDInvoiceReferencedDocument
  ,intf.ZUGFeRDTransportmodeCodes
  ,intf.ZUGFeRDChargeReasonCodes
  ,intf.ZUGFeRDAllowanceReasonCodes
  ,intf.ZUGFeRDInvoiceFormatOptions
  ,intf.ZUGFeRDInvoiceCommentConstants
  ,intf.ZUGFeRDLineStatusCodes
  ,intf.ZUGFeRDLineStatusReasonCodes
  ,intf.ZUGFeRDTradeDeliveryTermCodes
  ;

type
  TZUGFeRDInvoiceDescriptor22UBLWriter = class(TZUGFeRDIInvoiceDescriptorWriter)
  private
    Writer: TZUGFeRDProfileAwareXmlTextWriter;
    Descriptor: TZUGFeRDInvoiceDescriptor;
    procedure _writeOptionalAmount(_writer : TZUGFeRDProfileAwareXmlTextWriter; tagName : string; value : ZUGFeRDNullable<Currency>; numDecimals : Integer = 2; forceCurrency : Boolean = false; profile : TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
    procedure _writeNotes(_writer : TZUGFeRDProfileAwareXmlTextWriter; notes : TObjectList<TZUGFeRDNote>);
    procedure _writeOptionalParty(_writer: TZUGFeRDProfileAwareXmlTextWriter; partyType : TZUGFeRDPartyTypes; party : TZUGFeRDParty; contact : TZUGFeRDContact = nil; electronicAddress : TZUGFeRDElectronicAddress = nil; taxRegistrations : TObjectList<TZUGFeRDTaxRegistration> = nil);
    procedure _writeApplicableProductCharacteristics(_writer: TZUGFeRDProfileAwareXmlTextWriter; productCharacteristics : TObjectList<TZUGFeRDApplicableProductCharacteristic>);
    procedure _writeIncludedReferencedProducts(_writer: TZUGFeRDProfileAwareXmlTextWriter; includedReferencedProducts : TObjectList<TZUGFeRDIncludedReferencedProduct>);
    procedure _WriteDocumentLevelAllowanceCharges(tradeAllowanceCharge: TZUGFeRDAbstractTradeAllowanceCharge);
    procedure _WriteTradeLineItem(tradeLineItem: TZUGFeRDTradeLineItem; isInvoice: Boolean; options: TZUGFeRDInvoiceFormatOptions);
    procedure _WriteItemLevelSpecifiedTradeAllowanceCharge(specifiedTradeAllowanceCharge: TZUGFeRDAbstractTradeAllowanceCharge);
    procedure _WriteCommodityClassification(_writer: TZUGFeRDProfileAwareXmlTextWriter; designatedProductClassifications: TObjectList<TZUGFeRDDesignatedProductClassification>);
    function _encodeInvoiceType(type_ : TZUGFeRDInvoiceType) : Integer;
    function _IsInvoiceAccordingToUBLSpecification(type_: TZUGFeRDInvoiceType): Boolean;
    function _mapTaxRegistrationSchemeID(schemeID: TZUGFeRDTaxRegistrationSchemeID): string;
    function GetNameSpaces(isInvoice: Boolean): TDictionary<string, string>;
  private const
    ALL_PROFILES = [TZUGFeRDProfile.Minimum,
                    TZUGFeRDProfile.BasicWL,
                    TZUGFeRDProfile.Basic,
                    TZUGFeRDProfile.Comfort,
                    TZUGFeRDProfile.Extended,
                    TZUGFeRDProfile.XRechnung1,
                    TZUGFeRDProfile.XRechnung,
                    TZUGFeRDProfile.EReporting];
  public
    function Validate(_descriptor: TZUGFeRDInvoiceDescriptor; _throwExceptions: Boolean = True): Boolean; override;
    procedure Save(_descriptor: TZUGFeRDInvoiceDescriptor; _stream: TStream; _format : TZUGFeRDFormats = TZUGFeRDFormats.CII; options: TZUGFeRDInvoiceFormatOptions = Nil); override;
  end;

implementation

{ TZUGFeRDInvoiceDescriptor22UBLWriter }

function TZUGFeRDInvoiceDescriptor22UBLWriter.GetNameSpaces(isInvoice: Boolean): TDictionary<string, string>;
begin
  Result := TDictionary<string, string>.Create;
  Result.Add('cac', 'urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2');
  Result.Add('cbc', 'urn:oasis:names:specification:ubl:schema:xsd:CommonBasicComponents-2');
  Result.Add('ext', 'urn:oasis:names:specification:ubl:schema:xsd:CommonExtensionComponents-2');
  Result.Add('xs', 'http://www.w3.org/2001/XMLSchema');
  if isInvoice then
    Result.Add('ubl', 'urn:oasis:names:specification:ubl:schema:xsd:Invoice-2')
  else
    Result.Add('ubl', 'urn:oasis:names:specification:ubl:schema:xsd:CreditNote-2');
end;

procedure TZUGFeRDInvoiceDescriptor22UBLWriter.Save(
  _descriptor: TZUGFeRDInvoiceDescriptor; _stream: TStream;
  _format: TZUGFeRDFormats; options: TZUGFeRDInvoiceFormatOptions);
var
  streamPosition : Int64;
  isInvoice : Boolean;
  dueDate : TDateTime;
  dueDateFound : Boolean;
  paymentTerms : TZUGFeRDPaymentTerms;
  invoiceReferencedDocument : TZUGFeRDInvoiceReferencedDocument;
  document : TZUGFeRDAdditionalReferencedDocument;
  traceAccountingAccount : TZUGFeRDReceivableSpecifiedTradeAccountingAccount;
  account : TZUGFeRDBankAccount;
  tax : TZUGFeRDTax;
  tradeAllowanceCharge : TZUGFeRDAbstractTradeAllowanceCharge;
  tradeLineItem : TZUGFeRDTradeLineItem;
begin
  if (_stream = nil) then
    raise TZUGFeRDIllegalStreamException.Create('Cannot write to stream');

  streamPosition := _stream.Position;

  Descriptor := _descriptor;
  isInvoice := _IsInvoiceAccordingToUBLSpecification(Descriptor.Type_);

  Writer := TZUGFeRDProfileAwareXmlTextWriter.Create(_stream, TEncoding.UTF8, Descriptor.Profile);
  Writer.SetNamespaces(GetNameSpaces(isInvoice));
  Writer.Formatting := TZUGFeRDXmlFomatting.xmlFormatting_Indented;
  Writer.WriteStartDocument;

  WriteHeaderComments(Writer, options);

  // #region Kopfbereich
  // UBL has different namespace for different types
  if isInvoice then
  begin
    Writer.WriteStartElement('ubl:Invoice');
    Writer.WriteAttributeString('xmlns', 'urn:oasis:names:specification:ubl:schema:xsd:Invoice-2');
  end
  else
  begin
    Writer.WriteStartElement('ubl:CreditNote');
    Writer.WriteAttributeString('xmlns', 'urn:oasis:names:specification:ubl:schema:xsd:CreditNote-2');
  end;
  Writer.WriteAttributeString('xmlns', 'cac', 'urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2');
  Writer.WriteAttributeString('xmlns', 'cbc', 'urn:oasis:names:specification:ubl:schema:xsd:CommonBasicComponents-2');
  Writer.WriteAttributeString('xmlns', 'ext', 'urn:oasis:names:specification:ubl:schema:xsd:CommonExtensionComponents-2');
  Writer.WriteAttributeString('xmlns', 'xs', 'http://www.w3.org/2001/XMLSchema');
  // #endregion

  Writer.WriteElementString('cbc:CustomizationID', 'urn:cen.eu:en16931:2017#compliant#urn:xeinkauf.de:kosit:xrechnung_3.0');
  Writer.WriteElementString('cbc:ProfileID', 'urn:fdc:peppol.eu:2017:poacc:billing:01:1.0');

  Writer.WriteElementString('cbc:ID', Descriptor.InvoiceNo); // Rechnungsnummer

  if Descriptor.InvoiceDate.HasValue then
  begin
    Writer.WriteElementString('cbc:IssueDate', _formatDate(Descriptor.InvoiceDate.Value, false, true));
  end;

  if isInvoice then
  begin
    // DueDate (BT-9) - cardinality 0..1
    dueDateFound := false;
    for paymentTerms in Descriptor.PaymentTermsList do
    begin
      if paymentTerms.DueDate.HasValue then
      begin
        dueDate := paymentTerms.DueDate.Value;
        dueDateFound := true;
        Break;
      end;
    end;
    if dueDateFound then
      Writer.WriteElementString('cbc:DueDate', _formatDate(dueDate, false, true));
  end;

  if isInvoice then
    Writer.WriteElementString('cbc:InvoiceTypeCode', TEnumExtensions<TZUGFeRDInvoiceType>.EnumToString(Descriptor.Type_))
  else
    Writer.WriteElementString('cbc:CreditNoteTypeCode', TEnumExtensions<TZUGFeRDInvoiceType>.EnumToString(Descriptor.Type_));

  _writeNotes(Writer, Descriptor.Notes);

  Writer.WriteElementString('cbc:DocumentCurrencyCode', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(Descriptor.Currency));

  // BT-6
  if Descriptor.TaxCurrency.HasValue then
    Writer.WriteElementString('cbc:TaxCurrencyCode', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(Descriptor.TaxCurrency.Value));

  // BT-19
  if Descriptor.ReceivableSpecifiedTradeAccountingAccounts.Count > 0 then
  begin
    for traceAccountingAccount in Descriptor.ReceivableSpecifiedTradeAccountingAccounts do
    begin
      if traceAccountingAccount.TradeAccountID.Trim = '' then
        Continue;
      Writer.WriteOptionalElementString('cbc:AccountingCost', traceAccountingAccount.TradeAccountID);
      Break; // Cardinality 0..1
    end;
  end;

  Writer.WriteOptionalElementString('cbc:BuyerReference', Descriptor.ReferenceOrderNo);

  if Descriptor.BillingPeriodStart.HasValue or Descriptor.BillingPeriodEnd.HasValue then
  begin
    Writer.WriteStartElement('cac:InvoicePeriod');
    if Descriptor.BillingPeriodStart.HasValue then
      Writer.WriteElementString('cbc:StartDate', _formatDate(Descriptor.BillingPeriodStart.Value, false, true));
    if Descriptor.BillingPeriodEnd.HasValue then
      Writer.WriteElementString('cbc:EndDate', _formatDate(Descriptor.BillingPeriodEnd.Value, false, true));
    Writer.WriteEndElement; // !InvoicePeriod
  end;

  // OrderReference is optional
  if Descriptor.OrderNo <> '' then
  begin
    WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.BuyerOrderReferencedDocumentComment);
    Writer.WriteStartElement('cac:OrderReference');
    Writer.WriteElementString('cbc:ID', Descriptor.OrderNo);
    if (Descriptor.SellerOrderReferencedDocument <> nil) then
      Writer.WriteOptionalElementString('cbc:SalesOrderID', Descriptor.SellerOrderReferencedDocument.ID);
    Writer.WriteEndElement; // !OrderReference
  end;

  // BillingReference
  if Descriptor.InvoiceReferencedDocuments.Count > 0 then
  begin
    Writer.WriteStartElement('cac:BillingReference');
    for invoiceReferencedDocument in Descriptor.InvoiceReferencedDocuments do
    begin
      Writer.WriteStartElement('cac:InvoiceDocumentReference', [TZUGFeRDProfile.Extended, TZUGFeRDProfile.XRechnung1, TZUGFeRDProfile.XRechnung]);
      Writer.WriteOptionalElementString('cbc:ID', invoiceReferencedDocument.ID);
      if invoiceReferencedDocument.IssueDateTime.HasValue then
        Writer.WriteElementString('cbc:IssueDate', _formatDate(invoiceReferencedDocument.IssueDateTime.Value, false, true));
      Writer.WriteEndElement; // !cac:InvoiceDocumentReference
      Break; // only one reference allowed in UBL
    end;
    Writer.WriteEndElement; // !cac:BillingReference
  end;

  // DespatchDocumentReference
  if Descriptor.DespatchAdviceReferencedDocument <> nil then
  begin
    WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.DespatchAdviceReferencedDocumentComment);
    Writer.WriteStartElement('cac:DespatchDocumentReference');
    Writer.WriteOptionalElementString('cbc:ID', Descriptor.DespatchAdviceReferencedDocument.ID);
    Writer.WriteEndElement; // !DespatchDocumentReference
  end;

  // ContractDocumentReference
  if Descriptor.ContractReferencedDocument <> nil then
  begin
    Writer.WriteStartElement('cac:ContractDocumentReference');
    Writer.WriteOptionalElementString('cbc:ID', Descriptor.ContractReferencedDocument.ID);
    Writer.WriteEndElement; // !ContractDocumentReference
  end;

  // AdditionalDocumentReference
  if Descriptor.AdditionalReferencedDocuments.Count > 0 then
  begin
    for document in Descriptor.AdditionalReferencedDocuments do
    begin
      Writer.WriteStartElement('cac:AdditionalDocumentReference');
      Writer.WriteStartElement('cbc:ID'); // BT-18, BT-22

      if document.ReferenceTypeCode.HasValue then
        Writer.WriteAttributeString('schemeID', TEnumExtensions<TZUGFeRDReferenceTypeCodes>.EnumToString(document.ReferenceTypeCode.Value)); // BT-18-1

      Writer.WriteValue(document.ID);
      Writer.WriteEndElement; // !cbc:ID

      if document.TypeCode.HasValue then
        Writer.WriteElementString('cbc:DocumentTypeCode', TEnumExtensions<TZUGFeRDAdditionalReferencedDocumentTypeCode>.EnumToString(document.TypeCode.Value));

      Writer.WriteOptionalElementString('cbc:DocumentDescription', document.Name); // BT-123

      if document.AttachmentBinaryObject <> nil then
      begin
        Writer.WriteStartElement('cac:Attachment');

        Writer.WriteStartElement('cbc:EmbeddedDocumentBinaryObject'); // BT-125
        Writer.WriteAttributeString('filename', document.Filename);
        Writer.WriteAttributeString('mimeCode', TZUGFeRDMimeTypeMapper.GetMimeType(document.Filename));
        Writer.WriteValue(TZUGFeRDHelper.GetDataAsBase64(document.AttachmentBinaryObject));
        Writer.WriteEndElement; // !cbc:EmbeddedDocumentBinaryObject

        Writer.WriteEndElement; // !cac:Attachment
      end;

      Writer.WriteEndElement; // !AdditionalDocumentReference
    end;
  end;

  // ProjectReference
  if Descriptor.SpecifiedProcuringProject <> nil then
  begin
    Writer.WriteStartElement('cac:ProjectReference');
    Writer.WriteOptionalElementString('cbc:ID', Descriptor.SpecifiedProcuringProject.ID);
    Writer.WriteEndElement; // !ProjectReference
  end;

  // #region SellerTradeParty
  // AccountingSupplierParty = PartyTypes.SellerTradeParty
  WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.SellerTradePartyComment);
  _writeOptionalParty(Writer, TZUGFeRDPartyTypes.SellerTradeParty, Descriptor.Seller, Descriptor.SellerContact, Descriptor.SellerElectronicAddress, Descriptor.SellerTaxRegistration);
  // #endregion

  // #region BuyerTradeParty
  // AccountingCustomerParty = PartyTypes.BuyerTradeParty
  WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.BuyerTradePartyComment);
  _writeOptionalParty(Writer, TZUGFeRDPartyTypes.BuyerTradeParty, Descriptor.Buyer, Descriptor.BuyerContact, Descriptor.BuyerElectronicAddress, Descriptor.BuyerTaxRegistration);
  // #endregion

  if Descriptor.SellerTaxRepresentative <> nil then
    _writeOptionalParty(Writer, TZUGFeRDPartyTypes.SellerTaxRepresentativeTradeParty, Descriptor.SellerTaxRepresentative);

  // Delivery = ShipToTradeParty
  if (Descriptor.ShipTo <> nil) or (Descriptor.ActualDeliveryDate.HasValue) then
  begin
    Writer.WriteStartElement('cac:Delivery');

    if Descriptor.ActualDeliveryDate.HasValue then
    begin
      Writer.WriteStartElement('cbc:ActualDeliveryDate');
      Writer.WriteValue(_formatDate(Descriptor.ActualDeliveryDate.Value, false, true));
      Writer.WriteEndElement; // !ActualDeliveryDate
    end;

    if Descriptor.ShipTo <> nil then
    begin
      Writer.WriteStartElement('cac:DeliveryLocation');

      if Descriptor.ShipTo.ID <> nil then
        Writer.WriteOptionalElementString('cbc:ID', Descriptor.ShipTo.ID.ID);

      Writer.WriteStartElement('cac:Address');
      Writer.WriteOptionalElementString('cbc:StreetName', Descriptor.ShipTo.Street);
      Writer.WriteOptionalElementString('cbc:AdditionalStreetName', Descriptor.ShipTo.AddressLine3);
      Writer.WriteOptionalElementString('cbc:CityName', Descriptor.ShipTo.City);
      Writer.WriteOptionalElementString('cbc:PostalZone', Descriptor.ShipTo.Postcode);
      Writer.WriteOptionalElementString('cbc:CountrySubentity', Descriptor.ShipTo.CountrySubdivisionName);
      Writer.WriteStartElement('cac:Country');
      if Descriptor.ShipTo.Country.HasValue then
        Writer.WriteElementString('cbc:IdentificationCode', TEnumExtensions<TZUGFeRDCountryCodes>.EnumToString(Descriptor.ShipTo.Country.Value));
      Writer.WriteEndElement; // !Country
      Writer.WriteEndElement; // !Address
      Writer.WriteEndElement; // !DeliveryLocation

      if Descriptor.ShipTo.Name <> '' then
      begin
        Writer.WriteStartElement('cac:DeliveryParty');
        Writer.WriteStartElement('cac:PartyName');
        Writer.WriteStartElement('cbc:Name');
        Writer.WriteValue(Descriptor.ShipTo.Name);
        Writer.WriteEndElement; // !Name
        Writer.WriteEndElement; // !PartyName
        Writer.WriteEndElement; // !DeliveryParty
      end;
    end;

    Writer.WriteEndElement; // !Delivery
  end;

  // PaymentMeans
  WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.ApplicableHeaderTradeSettlementComment);
  if (Descriptor.CreditorBankAccounts.Count = 0) and (Descriptor.DebitorBankAccounts.Count = 0) then
  begin
    if Descriptor.PaymentMeans <> nil then
    begin
      if (Descriptor.PaymentMeans <> nil) and Descriptor.PaymentMeans.TypeCode.HasValue then
      begin
        WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.SpecifiedTradeSettlementPaymentMeansComment);
        Writer.WriteStartElement('cac:PaymentMeans', [TZUGFeRDProfile.BasicWL, TZUGFeRDProfile.Basic, TZUGFeRDProfile.Comfort, TZUGFeRDProfile.Extended, TZUGFeRDProfile.XRechnung1, TZUGFeRDProfile.XRechnung]);
        Writer.WriteElementString('cbc:PaymentMeansCode', TEnumExtensions<TZUGFeRDPaymentMeansTypeCodes>.EnumToString(Descriptor.PaymentMeans.TypeCode.Value));
        Writer.WriteOptionalElementString('cbc:PaymentID', Descriptor.PaymentReference);

        if Descriptor.PaymentMeans.FinancialCard <> nil then
        begin
          Writer.WriteStartElement('cac:CardAccount', [TZUGFeRDProfile.BasicWL, TZUGFeRDProfile.Basic, TZUGFeRDProfile.Comfort, TZUGFeRDProfile.Extended, TZUGFeRDProfile.XRechnung1, TZUGFeRDProfile.XRechnung]);
          Writer.WriteElementString('cbc:PrimaryAccountNumberID', Descriptor.PaymentMeans.FinancialCard.Id);
          Writer.WriteOptionalElementString('cbc:HolderName', Descriptor.PaymentMeans.FinancialCard.CardholderName);
          Writer.WriteEndElement; // !CardAccount
        end;
        Writer.WriteEndElement; // !PaymentMeans
      end;
    end;
  end
  else
  begin
    for account in Descriptor.CreditorBankAccounts do
    begin
      WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.SpecifiedTradeSettlementPaymentMeansComment);
      Writer.WriteStartElement('cac:PaymentMeans', [TZUGFeRDProfile.BasicWL, TZUGFeRDProfile.Basic, TZUGFeRDProfile.Comfort, TZUGFeRDProfile.Extended, TZUGFeRDProfile.XRechnung1, TZUGFeRDProfile.XRechnung]);

      if (Descriptor.PaymentMeans <> nil) and Descriptor.PaymentMeans.TypeCode.HasValue then
      begin
        Writer.WriteElementString('cbc:PaymentMeansCode', TEnumExtensions<TZUGFeRDPaymentMeansTypeCodes>.EnumToString(Descriptor.PaymentMeans.TypeCode.Value));
        Writer.WriteOptionalElementString('cbc:PaymentID', Descriptor.PaymentReference);

        if Descriptor.PaymentMeans.FinancialCard <> nil then
        begin
          Writer.WriteStartElement('cac:CardAccount', [TZUGFeRDProfile.BasicWL, TZUGFeRDProfile.Basic, TZUGFeRDProfile.Comfort, TZUGFeRDProfile.Extended, TZUGFeRDProfile.XRechnung1, TZUGFeRDProfile.XRechnung]);
          Writer.WriteElementString('cbc:PrimaryAccountNumberID', Descriptor.PaymentMeans.FinancialCard.Id);
          Writer.WriteOptionalElementString('cbc:HolderName', Descriptor.PaymentMeans.FinancialCard.CardholderName);
          Writer.WriteEndElement; // !CardAccount
        end;
      end;

      // PayeeFinancialAccount
      Writer.WriteStartElement('cac:PayeeFinancialAccount');
      Writer.WriteElementString('cbc:ID', account.IBAN);
      Writer.WriteOptionalElementString('cbc:Name', account.Name);

      if account.BIC <> '' then
      begin
        Writer.WriteStartElement('cac:FinancialInstitutionBranch');
        Writer.WriteElementString('cbc:ID', account.BIC);
        Writer.WriteEndElement; // !FinancialInstitutionBranch
      end;

      Writer.WriteEndElement; // !PayeeFinancialAccount
      Writer.WriteEndElement; // !PaymentMeans
    end;

    // [BR-67] - An Invoice shall contain maximum one Payment Mandate (BG-19).
    for account in Descriptor.DebitorBankAccounts do
    begin
      Writer.WriteStartElement('cac:PaymentMeans', [TZUGFeRDProfile.BasicWL, TZUGFeRDProfile.Basic, TZUGFeRDProfile.Comfort, TZUGFeRDProfile.Extended, TZUGFeRDProfile.XRechnung1, TZUGFeRDProfile.XRechnung]);

      if (Descriptor.PaymentMeans <> nil) and Descriptor.PaymentMeans.TypeCode.HasValue then
      begin
        Writer.WriteElementString('cbc:PaymentMeansCode', TEnumExtensions<TZUGFeRDPaymentMeansTypeCodes>.EnumToString(Descriptor.PaymentMeans.TypeCode.Value));
        Writer.WriteOptionalElementString('cbc:PaymentID', Descriptor.PaymentReference);
      end;

      Writer.WriteStartElement('cac:PaymentMandate');

      // PEPPOL-EN16931-R061: Mandate reference MUST be provided for direct debit.
      Writer.WriteElementString('cbc:ID', Descriptor.PaymentMeans.SEPAMandateReference);

      Writer.WriteStartElement('cac:PayerFinancialAccount');
      Writer.WriteElementString('cbc:ID', account.IBAN);
      Writer.WriteEndElement; // !PayerFinancialAccount
      Writer.WriteEndElement; // !PaymentMandate

      Writer.WriteEndElement; // !PaymentMeans
    end;
  end;

  // PaymentTerms (optional)
  var hasPaymentTermsDescription := false;
  for paymentTerms in Descriptor.PaymentTermsList do
  begin
    if paymentTerms.Description <> '' then
    begin
      hasPaymentTermsDescription := true;
      Break;
    end;
  end;

  if hasPaymentTermsDescription then
  begin
    Writer.WriteStartElement('cac:PaymentTerms');

    var hasAnyDescription := false;
    for paymentTerms in Descriptor.PaymentTermsList do
    begin
      if paymentTerms.Description.Trim <> '' then
      begin
        hasAnyDescription := true;
        Break;
      end;
    end;

    if hasAnyDescription then
    begin
      Writer.WriteStartElement('cbc:Note');

      for paymentTerms in Descriptor.PaymentTermsList do
      begin
        if paymentTerms.Description <> '' then
        begin
          Writer.WriteRawString(sLineBreak);
          Writer.WriteRawIndention;
          Writer.WriteValue(paymentTerms.Description);
        end;
      end;

      Writer.WriteRawString(sLineBreak);
      Writer.WriteEndElement; // !Note
    end;

    Writer.WriteEndElement; // !PaymentTerms
  end;

  // #region AllowanceCharge
  for tradeAllowanceCharge in Descriptor.TradeAllowanceCharges do
    _WriteDocumentLevelAllowanceCharges(tradeAllowanceCharge);
  // #endregion

  // Tax Total
  if (Descriptor.Taxes.Count > 0) and (Descriptor.TaxTotalAmount.HasValue) then
  begin
    Writer.WriteStartElement('cac:TaxTotal');
    _writeOptionalAmount(Writer, 'cbc:TaxAmount', Descriptor.TaxTotalAmount, 2, true);

    for tax in Descriptor.Taxes do
    begin
      WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.ApplicableTradeTaxComment);
      Writer.WriteStartElement('cac:TaxSubtotal');
      _writeOptionalAmount(Writer, 'cbc:TaxableAmount', tax.BasisAmount, 2, true);
      _writeOptionalAmount(Writer, 'cbc:TaxAmount', tax.TaxAmount, 2, true);

      Writer.WriteStartElement('cac:TaxCategory');
      Writer.WriteElementString('cbc:ID', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(tax.CategoryCode));

      if tax.CategoryCode <> TZUGFeRDTaxCategoryCodes.O then // BR-O-05
        Writer.WriteElementString('cbc:Percent', _formatDecimal(tax.Percent));

      if tax.ExemptionReasonCode.HasValue then
        Writer.WriteElementString('cbc:TaxExemptionReasonCode', TEnumExtensions<TZUGFeRDTaxExemptionReasonCodes>.EnumToString(tax.ExemptionReasonCode.Value));

      Writer.WriteOptionalElementString('cbc:TaxExemptionReason', tax.ExemptionReason);
      Writer.WriteStartElement('cac:TaxScheme');
      Writer.WriteElementString('cbc:ID', TEnumExtensions<TZUGFeRDTaxTypes>.EnumToString(tax.TypeCode));
      Writer.WriteEndElement; // !TaxScheme

      Writer.WriteEndElement; // !TaxCategory
      Writer.WriteEndElement; // !TaxSubtotal
    end;

    Writer.WriteEndElement; // !TaxTotal
  end;

  WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.SpecifiedTradeSettlementHeaderMonetarySummationComment);
  Writer.WriteStartElement('cac:LegalMonetaryTotal');
  _writeOptionalAmount(Writer, 'cbc:LineExtensionAmount', Descriptor.LineTotalAmount, 2, true);
  _writeOptionalAmount(Writer, 'cbc:TaxExclusiveAmount', Descriptor.TaxBasisAmount, 2, true);
  _writeOptionalAmount(Writer, 'cbc:TaxInclusiveAmount', Descriptor.GrandTotalAmount, 2, true);
  _writeOptionalAmount(Writer, 'cbc:AllowanceTotalAmount', Descriptor.AllowanceTotalAmount, 2, true);
  _writeOptionalAmount(Writer, 'cbc:ChargeTotalAmount', Descriptor.ChargeTotalAmount, 2, true);
  _writeOptionalAmount(Writer, 'cbc:PrepaidAmount', Descriptor.TotalPrepaidAmount, 2, true);
  _writeOptionalAmount(Writer, 'cbc:PayableRoundingAmount', Descriptor.RoundingAmount, 2, true);
  _writeOptionalAmount(Writer, 'cbc:PayableAmount', Descriptor.DuePayableAmount, 2, true);
  Writer.WriteEndElement; // !LegalMonetaryTotal

  for tradeLineItem in Descriptor.TradeLineItems do
  begin
    // Skip items with parent line id because these are written recursively in _WriteTradeLineItem
    if tradeLineItem.AssociatedDocument.ParentLineID = '' then
    begin
      WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.IncludedSupplyChainTradeLineItemComment);
      _WriteTradeLineItem(tradeLineItem, isInvoice, options);
    end;
  end;

  Writer.WriteEndDocument;
  Writer.Flush;

  _stream.Seek(streamPosition, TSeekOrigin.soBeginning);
end;


procedure TZUGFeRDInvoiceDescriptor22UBLWriter._WriteDocumentLevelAllowanceCharges(
  tradeAllowanceCharge: TZUGFeRDAbstractTradeAllowanceCharge);
begin
  if tradeAllowanceCharge = nil then
    Exit;

  Writer.WriteStartElement('cac:AllowanceCharge');

  Writer.WriteElementString('cbc:ChargeIndicator', IfThen(tradeAllowanceCharge.ChargeIndicator, 'true', 'false'));

  if (tradeAllowanceCharge is TZUGFeRDTradeAllowance) and
     TZUGFeRDTradeAllowance(tradeAllowanceCharge).ReasonCode.HasValue then
  begin
    Writer.WriteStartElement('cbc:AllowanceChargeReasonCode'); // BT-97
    Writer.WriteValue(TEnumExtensions<TZUGFeRDAllowanceReasonCodes>.EnumToString(TZUGFeRDTradeAllowance(tradeAllowanceCharge).ReasonCode.Value));
    Writer.WriteEndElement;
  end
  else if (tradeAllowanceCharge is TZUGFeRDTradeCharge) and
          TZUGFeRDTradeCharge(tradeAllowanceCharge).ReasonCode.HasValue then
  begin
    Writer.WriteStartElement('cbc:AllowanceChargeReasonCode'); // BT-104
    Writer.WriteValue(TEnumExtensions<TZUGFeRDChargeReasonCodes>.EnumToString(TZUGFeRDTradeCharge(tradeAllowanceCharge).ReasonCode.Value));
    Writer.WriteEndElement;
  end;

  if tradeAllowanceCharge.Reason <> '' then
  begin
    Writer.WriteStartElement('cbc:AllowanceChargeReason'); // BT-97 / BT-104
    Writer.WriteValue(tradeAllowanceCharge.Reason);
    Writer.WriteEndElement;
  end;

  if tradeAllowanceCharge.ChargePercentage.HasValue and tradeAllowanceCharge.BasisAmount.HasValue then
  begin
    Writer.WriteStartElement('cbc:MultiplierFactorNumeric');
    Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ChargePercentage.Value, 2));
    Writer.WriteEndElement;
  end;

  Writer.WriteStartElement('cbc:Amount'); // BT-92 / BT-99
  Writer.WriteAttributeString('currencyID', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(Descriptor.Currency));
  Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ActualAmount));
  Writer.WriteEndElement;

  if tradeAllowanceCharge.BasisAmount.HasValue then
  begin
    Writer.WriteStartElement('cbc:BaseAmount'); // BT-93 / BT-100
    Writer.WriteAttributeString('currencyID', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(Descriptor.Currency));
    Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.BasisAmount.Value));
    Writer.WriteEndElement;
  end;

  Writer.WriteStartElement('cac:TaxCategory');
  Writer.WriteElementString('cbc:ID', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(tradeAllowanceCharge.Tax.CategoryCode));
  if tradeAllowanceCharge.Tax.Percent <> 0 then
    Writer.WriteElementString('cbc:Percent', _formatDecimal(tradeAllowanceCharge.Tax.Percent));
  Writer.WriteStartElement('cac:TaxScheme');
  Writer.WriteElementString('cbc:ID', TEnumExtensions<TZUGFeRDTaxTypes>.EnumToString(tradeAllowanceCharge.Tax.TypeCode));
  Writer.WriteEndElement; // !cac:TaxScheme
  Writer.WriteEndElement; // !cac:TaxCategory

  Writer.WriteEndElement; // !AllowanceCharge
end;


procedure TZUGFeRDInvoiceDescriptor22UBLWriter._WriteTradeLineItem(
  tradeLineItem: TZUGFeRDTradeLineItem; isInvoice: Boolean;
  options: TZUGFeRDInvoiceFormatOptions);
var
  document : TZUGFeRDAdditionalReferencedDocument;
  specifiedTradeAllowance : TZUGFeRDTradeAllowance;
  specifiedTradeCharge : TZUGFeRDTradeCharge;
  subTradeLineItem : TZUGFeRDTradeLineItem;
  charges : TObjectList<TZUGFeRDAbstractTradeAllowanceCharge>;
begin
  if tradeLineItem.AssociatedDocument.ParentLineID = '' then
  begin
    if isInvoice then
      Writer.WriteStartElement('cac:InvoiceLine')
    else
      Writer.WriteStartElement('cac:CreditNoteLine');
  end
  else
  begin
    if isInvoice then
      Writer.WriteStartElement('cac:SubInvoiceLine')
    else
      Writer.WriteStartElement('cac:SubCreditNoteLine');
  end;

  Writer.WriteElementString('cbc:ID', tradeLineItem.AssociatedDocument.LineID);

  if (tradeLineItem.AssociatedDocument <> nil) and
     (tradeLineItem.AssociatedDocument.Notes <> nil) and
     (tradeLineItem.AssociatedDocument.Notes.Count > 0) then
  begin
    // BT-127
    var noteContent := '';
    for var note : TZUGFeRDNote in tradeLineItem.AssociatedDocument.Notes do
    begin
      if noteContent <> '' then
        noteContent := noteContent + sLineBreak;
      noteContent := noteContent + note.Content;
    end;
    Writer.WriteStartElement('cbc:Note');
    Writer.WriteValue(noteContent);
    Writer.WriteEndElement; // !cbc:Note
  end;

  if isInvoice then
    Writer.WriteStartElement('cbc:InvoicedQuantity')
  else
    Writer.WriteStartElement('cbc:CreditedQuantity');
  Writer.WriteAttributeString('unitCode', TEnumExtensions<TZUGFeRDQuantityCodes>.EnumToString(tradeLineItem.UnitCode));
  Writer.WriteValue(_formatDecimal(tradeLineItem.BilledQuantity, 4));
  Writer.WriteEndElement; // !InvoicedQuantity || CreditedQuantity

  WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.SpecifiedTradeSettlementLineMonetarySummationComment);
  _writeOptionalAmount(Writer, 'cbc:LineExtensionAmount', tradeLineItem.LineTotalAmount, 2, true);

  if tradeLineItem.BillingPeriodStart.HasValue or tradeLineItem.BillingPeriodEnd.HasValue then
  begin
    Writer.WriteStartElement('cac:InvoicePeriod');
    if tradeLineItem.BillingPeriodStart.HasValue then
      Writer.WriteElementString('cbc:StartDate', _formatDate(tradeLineItem.BillingPeriodStart.Value, false, true));
    if tradeLineItem.BillingPeriodEnd.HasValue then
      Writer.WriteElementString('cbc:EndDate', _formatDate(tradeLineItem.BillingPeriodEnd.Value, false, true));
    Writer.WriteEndElement; // !InvoicePeriod
  end;

  if tradeLineItem.AdditionalReferencedDocuments.Count > 0 then
  begin
    for document in tradeLineItem.AdditionalReferencedDocuments do
    begin
      Writer.WriteStartElement('cac:DocumentReference');
      Writer.WriteStartElement('cbc:ID'); // BT-18, BT-22

      if document.ReferenceTypeCode.HasValue then
        Writer.WriteAttributeString('schemeID', TEnumExtensions<TZUGFeRDReferenceTypeCodes>.EnumToString(document.ReferenceTypeCode.Value)); // BT-18-1

      Writer.WriteValue(document.ID);
      Writer.WriteEndElement; // !cbc:ID

      if document.TypeCode.HasValue then
        Writer.WriteElementString('cbc:DocumentTypeCode', TEnumExtensions<TZUGFeRDAdditionalReferencedDocumentTypeCode>.EnumToString(document.TypeCode.Value));

      Writer.WriteOptionalElementString('cbc:DocumentDescription', document.Name); // BT-123

      Writer.WriteEndElement; // !DocumentReference
    end;
  end;

  for specifiedTradeAllowance in tradeLineItem.GetSpecifiedTradeAllowances do
    _WriteItemLevelSpecifiedTradeAllowanceCharge(specifiedTradeAllowance);

  for specifiedTradeCharge in tradeLineItem.GetSpecifiedTradeCharges do
    _WriteItemLevelSpecifiedTradeAllowanceCharge(specifiedTradeCharge);

  Writer.WriteStartElement('cac:Item');

  Writer.WriteOptionalElementString('cbc:Description', tradeLineItem.Description);
  Writer.WriteElementString('cbc:Name', tradeLineItem.Name);

  if tradeLineItem.BuyerAssignedID <> '' then
  begin
    Writer.WriteStartElement('cac:BuyersItemIdentification');
    Writer.WriteElementString('cbc:ID', tradeLineItem.BuyerAssignedID);
    Writer.WriteEndElement; // !BuyersItemIdentification
  end;

  if tradeLineItem.SellerAssignedID <> '' then
  begin
    Writer.WriteStartElement('cac:SellersItemIdentification');
    Writer.WriteElementString('cbc:ID', tradeLineItem.SellerAssignedID);
    Writer.WriteEndElement; // !SellersItemIdentification
  end;

  _writeIncludedReferencedProducts(Writer, tradeLineItem.IncludedReferencedProducts);
  _WriteCommodityClassification(Writer, tradeLineItem.DesignedProductClassifications);

  // [UBL-SR-48] - Invoice lines shall have one and only one classified tax category.
  Writer.WriteStartElement('cac:ClassifiedTaxCategory');
  Writer.WriteElementString('cbc:ID', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(tradeLineItem.TaxCategoryCode));
  Writer.WriteElementString('cbc:Percent', _formatDecimal(tradeLineItem.TaxPercent));

  Writer.WriteStartElement('cac:TaxScheme');
  Writer.WriteElementString('cbc:ID', TEnumExtensions<TZUGFeRDTaxTypes>.EnumToString(tradeLineItem.TaxType));
  Writer.WriteEndElement; // !TaxScheme

  Writer.WriteEndElement; // !ClassifiedTaxCategory

  _writeApplicableProductCharacteristics(Writer, tradeLineItem.ApplicableProductCharacteristics);

  Writer.WriteEndElement; // !Item

  Writer.WriteStartElement('cac:Price'); // BG-29

  WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.NetPriceProductTradePriceComment);
  Writer.WriteStartElement('cbc:PriceAmount');
  Writer.WriteAttributeString('currencyID', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(Descriptor.Currency));
  // UBL-DT-01 explicitly excempts the price amount from the 2 decimal rule for amount elements,
  // thus allowing for 4 decimal places (needed for e.g. fuel prices)
  Writer.WriteValue(_formatDecimal(tradeLineItem.NetUnitPrice.Value, 4));
  Writer.WriteEndElement;

  if tradeLineItem.NetQuantity.HasValue then
  begin
    Writer.WriteStartElement('cbc:BaseQuantity'); // BT-149
    Writer.WriteAttributeString('unitCode', TEnumExtensions<TZUGFeRDQuantityCodes>.EnumToString(tradeLineItem.UnitCode)); // BT-150
    Writer.WriteValue(_formatDecimal(tradeLineItem.NetQuantity.Value));
    Writer.WriteEndElement;
  end;

  charges := tradeLineItem.TradeAllowanceCharges;
  if charges.Count > 0 then // only one charge possible in UBL
  begin
    var tradeAllowanceCharge := charges[0];
    Writer.WriteStartElement('cac:AllowanceCharge');

    if tradeAllowanceCharge is TZUGFeRDTradeAllowance then
      Writer.WriteElementString('cbc:ChargeIndicator', 'false')
    else
      Writer.WriteElementString('cbc:ChargeIndicator', 'true');

    Writer.WriteStartElement('cbc:Amount'); // BT-147
    Writer.WriteAttributeString('currencyID', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(Descriptor.Currency));
    Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ActualAmount));
    Writer.WriteEndElement;

    if tradeAllowanceCharge.BasisAmount.HasValue then // BT-148 is optional
    begin
      Writer.WriteStartElement('cbc:BaseAmount'); // BT-148
      Writer.WriteAttributeString('currencyID', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(Descriptor.Currency));
      Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.BasisAmount.Value));
      Writer.WriteEndElement;
    end;

    Writer.WriteEndElement; // !AllowanceCharge
  end;

  Writer.WriteEndElement; // !Price

  // Write sub invoice lines recursively
  for subTradeLineItem in Descriptor.TradeLineItems do
  begin
    if subTradeLineItem.AssociatedDocument.ParentLineID = tradeLineItem.AssociatedDocument.LineID then
      _WriteTradeLineItem(subTradeLineItem, isInvoice, options);
  end;

  Writer.WriteEndElement; // !InvoiceLine
end;


procedure TZUGFeRDInvoiceDescriptor22UBLWriter._WriteItemLevelSpecifiedTradeAllowanceCharge(
  specifiedTradeAllowanceCharge: TZUGFeRDAbstractTradeAllowanceCharge);
begin
  Writer.WriteStartElement('cac:AllowanceCharge');
  Writer.WriteElementString('cbc:ChargeIndicator',
    IfThen(specifiedTradeAllowanceCharge.ChargeIndicator, 'true', 'false')); // BG-28-0

  if (specifiedTradeAllowanceCharge is TZUGFeRDTradeAllowance) and
     TZUGFeRDTradeAllowance(specifiedTradeAllowanceCharge).ReasonCode.HasValue then
  begin
    Writer.WriteOptionalElementString('cbc:AllowanceChargeReasonCode',
      TEnumExtensions<TZUGFeRDAllowanceReasonCodes>.EnumToString(
        TZUGFeRDTradeAllowance(specifiedTradeAllowanceCharge).ReasonCode.Value)); // BT-140
  end
  else if (specifiedTradeAllowanceCharge is TZUGFeRDTradeCharge) and
          TZUGFeRDTradeCharge(specifiedTradeAllowanceCharge).ReasonCode.HasValue then
  begin
    Writer.WriteOptionalElementString('cbc:AllowanceChargeReasonCode',
      TEnumExtensions<TZUGFeRDChargeReasonCodes>.EnumToString(
        TZUGFeRDTradeCharge(specifiedTradeAllowanceCharge).ReasonCode.Value)); // BT-145
  end;

  Writer.WriteOptionalElementString('cbc:AllowanceChargeReason',
    specifiedTradeAllowanceCharge.Reason); // BT-139, BT-144

  if specifiedTradeAllowanceCharge.ChargePercentage.HasValue then
  begin
    Writer.WriteOptionalElementString('cbc:MultiplierFactorNumeric',
      _formatDecimal(specifiedTradeAllowanceCharge.ChargePercentage.Value));
  end;

  Writer.WriteStartElement('cbc:Amount');
  Writer.WriteAttributeString('currencyID', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(Descriptor.Currency));
  Writer.WriteValue(_formatDecimal(specifiedTradeAllowanceCharge.ActualAmount));
  Writer.WriteEndElement; // !Amount

  if specifiedTradeAllowanceCharge.BasisAmount.HasValue then
  begin
    Writer.WriteStartElement('cbc:BaseAmount'); // BT-137, BT-142
    Writer.WriteAttributeString('currencyID', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(Descriptor.Currency));
    Writer.WriteValue(_formatDecimal(specifiedTradeAllowanceCharge.BasisAmount.Value));
    Writer.WriteEndElement; // !BaseAmount
  end;

  Writer.WriteEndElement; // !AllowanceCharge
end;


procedure TZUGFeRDInvoiceDescriptor22UBLWriter._WriteCommodityClassification(
  _writer: TZUGFeRDProfileAwareXmlTextWriter;
  designatedProductClassifications: TObjectList<TZUGFeRDDesignatedProductClassification>);
var
  classification : TZUGFeRDDesignatedProductClassification;
begin
  if (designatedProductClassifications = nil) or (designatedProductClassifications.Count = 0) then
    Exit;

  _writer.WriteStartElement('cac:CommodityClassification');

  for classification in designatedProductClassifications do
  begin
    if not classification.ListID.HasValue then
      Continue;

    _writer.WriteStartElement('cbc:ItemClassificationCode'); // BT-158
    Writer.WriteAttributeString('listID', TEnumExtensions<TZUGFeRDDesignatedProductClassificationClassCodes>.EnumToString(classification.ListID.Value)); // BT-158-1

    if classification.ListVersionID <> '' then
      Writer.WriteAttributeString('listVersionID', classification.ListVersionID); // BT-158-2

    // no name attribute in Peppol Billing!
    _writer.WriteValue(classification.ClassCode, ALL_PROFILES);

    _writer.WriteEndElement;
  end;

  _writer.WriteEndElement;
end;


procedure TZUGFeRDInvoiceDescriptor22UBLWriter._writeOptionalParty(
  _writer: TZUGFeRDProfileAwareXmlTextWriter;
  partyType: TZUGFeRDPartyTypes; party: TZUGFeRDParty;
  contact: TZUGFeRDContact; electronicAddress: TZUGFeRDElectronicAddress;
  taxRegistrations: TObjectList<TZUGFeRDTaxRegistration>);
var
  taxReg : TZUGFeRDTaxRegistration;
begin
  // filter according to https://github.com/stephanstapel/ZUGFeRD-csharp/pull/221
  case partyType of
    TZUGFeRDPartyTypes.Unknown: Exit;
    TZUGFeRDPartyTypes.SellerTradeParty: ;
    TZUGFeRDPartyTypes.BuyerTradeParty: ;
    TZUGFeRDPartyTypes.SellerTaxRepresentativeTradeParty: ;
    TZUGFeRDPartyTypes.ShipFromTradeParty: Exit;
    TZUGFeRDPartyTypes.ShipToTradeParty: Exit; // ship to has minimized info, cannot use generic _writeOptionalParty()
  else
    Exit;
  end;

  if party <> nil then
  begin
    case partyType of
      TZUGFeRDPartyTypes.SellerTradeParty:
        _writer.WriteStartElement('cac:AccountingSupplierParty', ALL_PROFILES);
      TZUGFeRDPartyTypes.BuyerTradeParty:
        _writer.WriteStartElement('cac:AccountingCustomerParty', ALL_PROFILES);
      TZUGFeRDPartyTypes.SellerTaxRepresentativeTradeParty:
        _writer.WriteStartElement('cac:TaxRepresentativeParty', ALL_PROFILES);
    end;

    _writer.WriteStartElement('cac:Party', ALL_PROFILES);

    if (electronicAddress <> nil) and (ElectronicAddress.Address<>'') then
    begin
      _writer.WriteStartElement('cbc:EndpointID');
      _writer.WriteAttributeString('schemeID', TEnumExtensions<TZUGFeRDElectronicAddressSchemeIdentifiers>.EnumToString(electronicAddress.ElectronicAddressSchemeID));
      _writer.WriteValue(electronicAddress.Address);
      _writer.WriteEndElement;
    end;

    if partyType = TZUGFeRDPartyTypes.SellerTradeParty then
    begin
      // SEPA Creditor Identifier
      if (Descriptor.PaymentMeans <> nil) and (Descriptor.PaymentMeans.SEPACreditorIdentifier <> '') then
      begin
        _writer.WriteStartElement('cac:PartyIdentification');
        _writer.WriteStartElement('cbc:ID'); // BT-90
        _writer.WriteAttributeString('schemeID', 'SEPA');
        _writer.WriteValue(Descriptor.PaymentMeans.SEPACreditorIdentifier);
        _writer.WriteEndElement; // !ID
        _writer.WriteEndElement; // !PartyIdentification
      end;
      // no 'else' because the cardinality is 0..n
      if (party.ID <> nil) and (party.ID.ID <> '') then
      begin
        _writer.WriteStartElement('cac:PartyIdentification');
        _writer.WriteStartElement('cbc:ID'); // BT-29
        // 'SchemeID' is optional
        if party.ID.SchemeID.HasValue then
          _writer.WriteAttributeString('schemeID', TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.EnumToString(party.ID.SchemeID.Value));
        _writer.WriteValue(party.ID.ID);
        _writer.WriteEndElement; // !ID
        _writer.WriteEndElement; // !PartyIdentification
      end;
    end
    else if partyType = TZUGFeRDPartyTypes.BuyerTradeParty then
    begin
      if (party.GlobalID <> nil) and (party.GlobalID.ID <> '') then
      begin
        _writer.WriteStartElement('cac:PartyIdentification');
        _writer.WriteStartElement('cbc:ID');
        if party.GlobalID.SchemeID.HasValue then
          _writer.WriteAttributeString('schemeID', TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.EnumToString(party.GlobalID.SchemeID.Value));
        _writer.WriteValue(party.GlobalID.ID);
        _writer.WriteEndElement; // !ID
        _writer.WriteEndElement; // !PartyIdentification
      end
      else if (party.ID <> nil) and (party.ID.ID <> '') then
      begin
        _writer.WriteStartElement('cac:PartyIdentification');
        _writer.WriteStartElement('cbc:ID');
        if party.ID.SchemeID.HasValue then
          _writer.WriteAttributeString('schemeID', TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.EnumToString(party.ID.SchemeID.Value));
        _writer.WriteValue(party.ID.ID);
        _writer.WriteEndElement; // !ID
        _writer.WriteEndElement; // !PartyIdentification
      end;
    end;

    if party.Name <> '' then
    begin
      _writer.WriteStartElement('cac:PartyName');
      _writer.WriteStartElement('cbc:Name');
      _writer.WriteValue(party.Name);
      _writer.WriteEndElement; // !Name
      _writer.WriteEndElement; // !PartyName
    end;

    _writer.WriteStartElement('cac:PostalAddress');
    Writer.WriteOptionalElementString('cbc:StreetName', party.Street);
    Writer.WriteOptionalElementString('cbc:AdditionalStreetName', party.AddressLine3);
    Writer.WriteElementString('cbc:CityName', party.City);
    Writer.WriteElementString('cbc:PostalZone', party.Postcode);
    Writer.WriteOptionalElementString('cbc:CountrySubentity', party.CountrySubdivisionName);

    _writer.WriteStartElement('cac:Country');
    if party.Country.HasValue then
      Writer.WriteElementString('cbc:IdentificationCode', TEnumExtensions<TZUGFeRDCountryCodes>.EnumToString(party.Country.Value));
    _writer.WriteEndElement; // !Country

    _writer.WriteEndElement; // !PostalAddress

    if taxRegistrations <> nil then
    begin
      for taxReg in taxRegistrations do
      begin
        Writer.WriteStartElement('cac:PartyTaxScheme');
        Writer.WriteElementString('cbc:CompanyID', taxReg.No);
        Writer.WriteStartElement('cac:TaxScheme');
        Writer.WriteElementString('cbc:ID', _mapTaxRegistrationSchemeID(taxReg.SchemeID));
        Writer.WriteEndElement; // !TaxScheme
        Writer.WriteEndElement; // !PartyTaxScheme
      end;
    end;

    if (party.SpecifiedLegalOrganization <> nil) or (party.Description <> '') then
    begin
      _writer.WriteStartElement('cac:PartyLegalEntity');
      if party.SpecifiedLegalOrganization <> nil then
        _writer.WriteOptionalElementString('cbc:RegistrationName', party.SpecifiedLegalOrganization.TradingBusinessName);

      if (party.SpecifiedLegalOrganization <> nil) and
         (party.SpecifiedLegalOrganization.ID <> nil) and
         (party.SpecifiedLegalOrganization.ID.ID <> '') then
      begin
        // Party legal registration identifier (BT-30)
        Writer.WriteStartElement('cbc:CompanyID');
        if party.SpecifiedLegalOrganization.ID.SchemeID.HasValue then
          Writer.WriteAttributeString('schemeID', TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.EnumToString(party.SpecifiedLegalOrganization.ID.SchemeID.Value));
        Writer.WriteValue(party.SpecifiedLegalOrganization.ID.ID);
        Writer.WriteEndElement; // !CompanyID
      end;

      // Party additional legal information (BT-33)
      Writer.WriteOptionalElementString('cbc:CompanyLegalForm', party.Description);

      _writer.WriteEndElement; // !PartyLegalEntity
    end;

    if contact <> nil then
    begin
      _writer.WriteStartElement('cac:Contact');
      _writer.WriteOptionalElementString('cbc:Name', contact.Name);
      _writer.WriteOptionalElementString('cbc:Telephone', contact.PhoneNo);
      _writer.WriteOptionalElementString('cbc:ElectronicMail', contact.EmailAddress);
      _writer.WriteEndElement; // !Contact
    end;

    _writer.WriteEndElement; // !Party
    _writer.WriteEndElement; // !AccountingSupplierParty / AccountingCustomerParty / TaxRepresentativeParty
  end;
end;


procedure TZUGFeRDInvoiceDescriptor22UBLWriter._writeIncludedReferencedProducts(
  _writer: TZUGFeRDProfileAwareXmlTextWriter;
  includedReferencedProducts: TObjectList<TZUGFeRDIncludedReferencedProduct>);
begin
  if includedReferencedProducts.Count > 0 then
  begin
    // TODO: implement when C# implementation is complete
  end;
end;


procedure TZUGFeRDInvoiceDescriptor22UBLWriter._writeApplicableProductCharacteristics(
  _writer: TZUGFeRDProfileAwareXmlTextWriter;
  productCharacteristics: TObjectList<TZUGFeRDApplicableProductCharacteristic>);
var
  characteristic : TZUGFeRDApplicableProductCharacteristic;
begin
  if productCharacteristics.Count > 0 then
  begin
    for characteristic in productCharacteristics do
    begin
      _writer.WriteStartElement('cac:AdditionalItemProperty');
      _writer.WriteElementString('cbc:Name', characteristic.Description);
      _writer.WriteElementString('cbc:Value', characteristic.Value);
      _writer.WriteEndElement;
    end;
  end;
end;


procedure TZUGFeRDInvoiceDescriptor22UBLWriter._writeNotes(
  _writer: TZUGFeRDProfileAwareXmlTextWriter;
  notes: TObjectList<TZUGFeRDNote>);
var
  note : TZUGFeRDNote;
begin
  if notes.Count > 0 then
  begin
    for note in notes do
      _writer.WriteElementString('cbc:Note', note.Content);
  end;
end;


procedure TZUGFeRDInvoiceDescriptor22UBLWriter._writeOptionalAmount(
  _writer: TZUGFeRDProfileAwareXmlTextWriter; tagName: string;
  value: ZUGFeRDNullable<Currency>; numDecimals: Integer;
  forceCurrency: Boolean; profile: TZUGFeRDProfiles);
begin
  if not value.HasValue then
    Exit;

  _writer.WriteStartElement(tagName, profile);
  if forceCurrency then
    _writer.WriteAttributeString('currencyID', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(Descriptor.Currency));
  _writer.WriteValue(_formatDecimal(value.Value, numDecimals));
  _writer.WriteEndElement; // !tagName
end;


function TZUGFeRDInvoiceDescriptor22UBLWriter.Validate(
  _descriptor: TZUGFeRDInvoiceDescriptor;
  _throwExceptions: Boolean): Boolean;
begin
  raise ENotImplemented.Create('Validate not implemented for UBL writer');
end;


function TZUGFeRDInvoiceDescriptor22UBLWriter._encodeInvoiceType(
  type_: TZUGFeRDInvoiceType): Integer;
begin
  Result := Integer(type_);
end;


function TZUGFeRDInvoiceDescriptor22UBLWriter._IsInvoiceAccordingToUBLSpecification(
  type_: TZUGFeRDInvoiceType): Boolean;
begin
  case type_ of
    TZUGFeRDInvoiceType.RequestForPayment,              // 71
    TZUGFeRDInvoiceType.DebitNoteRelatedToGoodsOrServices, // 80
    TZUGFeRDInvoiceType.MeteredServicesInvoice,          // 82
    TZUGFeRDInvoiceType.DebitnoteRelatedToFinancialAdjustments, // 84
    TZUGFeRDInvoiceType.TaxNotification,                 // 102
    TZUGFeRDInvoiceType.FinalPaymentRequestBasedOnCompletionOfWork, // 218
    TZUGFeRDInvoiceType.PaymentRequestForCompletedUnits, // 219
    TZUGFeRDInvoiceType.PartialInvoice,                  // 326
    TZUGFeRDInvoiceType.CommercialInvoiceWithPackingList, // 331
    TZUGFeRDInvoiceType.Invoice,                         // 380
    TZUGFeRDInvoiceType.CommissionNote,                  // 382
    TZUGFeRDInvoiceType.DebitNote,                       // 383
    TZUGFeRDInvoiceType.Correction,                      // 384
    TZUGFeRDInvoiceType.PrepaymentInvoice,               // 386
    TZUGFeRDInvoiceType.TaxInvoice,                      // 388
    TZUGFeRDInvoiceType.SelfBilledInvoice,               // 389
    TZUGFeRDInvoiceType.FactoredInvoice,                 // 393
    TZUGFeRDInvoiceType.ConsignmentInvoice,              // 395
    TZUGFeRDInvoiceType.ForwardersInvoiceDiscrepancyReport, // 553
    TZUGFeRDInvoiceType.InsurersInvoice,                 // 575
    TZUGFeRDInvoiceType.ForwardersInvoice,               // 623
    TZUGFeRDInvoiceType.FreightInvoice,                  // 780
    TZUGFeRDInvoiceType.ClaimNotification,               // 817
    TZUGFeRDInvoiceType.ConsularInvoice,                 // 870
    TZUGFeRDInvoiceType.PartialConstructionInvoice,      // 875
    TZUGFeRDInvoiceType.PartialFinalConstructionInvoice, // 876
    TZUGFeRDInvoiceType.FinalConstructionInvoice:        // 877
      Result := true;
    TZUGFeRDInvoiceType.CreditNoteRelatedToGoodsOrServices, // 81
    TZUGFeRDInvoiceType.CreditNoteRelatedToFinancialAdjustments, // 83
    TZUGFeRDInvoiceType.CreditNote,                      // 381
    TZUGFeRDInvoiceType.FactoredCreditNote,              // 396
    TZUGFeRDInvoiceType.ForwardersCreditNote:            // 532
      Result := false;
  else
    raise ENotImplemented.CreateFmt('Invoice type %d not implemented in UBL writer.', [Integer(type_)]);
  end;
end;


function TZUGFeRDInvoiceDescriptor22UBLWriter._mapTaxRegistrationSchemeID(
  schemeID: TZUGFeRDTaxRegistrationSchemeID): string;
begin
  case schemeID of
    TZUGFeRDTaxRegistrationSchemeID.VA: Result := 'VAT';
    TZUGFeRDTaxRegistrationSchemeID.FC: Result := 'TAX';
  else
    Result := 'TAX';
  end;
end;

end.
