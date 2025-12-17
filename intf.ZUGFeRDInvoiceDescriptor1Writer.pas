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

unit intf.ZUGFeRDInvoiceDescriptor1Writer;

interface

uses
  System.SysUtils,System.Classes,System.StrUtils,System.Generics.Collections,
  System.Math
  ,intf.ZUGFeRDInvoiceDescriptor
  ,intf.ZUGFeRDIInvoiceDescriptorWriter
  ,intf.ZUGFeRDProfileAwareXmlTextWriter
  ,intf.ZUGFeRDExceptions
  ,intf.ZUGFeRDProfile
  ,intf.ZUGFeRDHelper
  ,intf.ZUGFeRDCurrencyCodes
  ,intf.ZUGFeRDVersion
  ,intf.ZUGFeRDInvoiceTypes
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
  ,intf.ZUGFeRDSpecialServiceDescriptionCodes
  ,intf.ZUGFeRDFormats
  ,intf.ZUGFeRDTransportmodeCodes
  ,intf.ZUGFeRDChargeReasonCodes
  ,intf.ZUGFeRDAllowanceReasonCodes
  ,intf.ZUGFeRDInvoiceFormatOptions
  ,intf.ZUGFeRDInvoiceCommentConstants
  ;
type
  TZUGFeRDInvoiceDescriptor1Writer = class(TZUGFeRDIInvoiceDescriptorWriter)
  private
    Writer: TZUGFeRDProfileAwareXmlTextWriter;
    Descriptor: TZUGFeRDInvoiceDescriptor;
    procedure _writeDocumentLevelSpecifiedTradeAllowanceCharge(_writer: TZUGFeRDProfileAwareXmlTextWriter; tradeAllowanceCharge: TZUGFeRDAbstractTradeAllowanceCharge);
    procedure _writeOptionalAmount(_writer : TZUGFeRDProfileAwareXmlTextWriter; _tagName : string; _value : ZUGFeRDNullable<Currency>; _numDecimals : Integer = 2);
    procedure _writeOptionalAdaptiveAmount(_writer: TZUGFeRDProfileAwareXmlTextWriter; _tagName: string; _value: ZUGFeRDNullable<Currency>; _numDecimals: Integer = 2;_maxnumDecimals: Integer = 4; _forceCurrency: boolean = false);
    procedure _writeNotes(_writer : TZUGFeRDProfileAwareXmlTextWriter; notes : TObjectList<TZUGFeRDNote>);
    procedure _writeOptionalContact(_writer: TZUGFeRDProfileAwareXmlTextWriter; contactTag: String; contact: TZUGFeRDContact);
    procedure _writeOptionalParty(_writer: TZUGFeRDProfileAwareXmlTextWriter; PartyTag: String; Party: TZUGFeRDParty; Contact: TZUGFeRDContact = nil; TaxRegistrations: TObjectList<TZUGFeRDTaxRegistration> = nil);
    procedure _writeOptionalTaxes(_writer: TZUGFeRDProfileAwareXmlTextWriter; options: TZUGFeRDInvoiceFormatOptions);
    procedure _writeElementWithAttribute(_writer: TZUGFeRDProfileAwareXmlTextWriter; tagName, attributeName,attributeValue, nodeValue: String);
    function GetNameSpaces: TDictionary<string, string>;

  private const
    ALL_PROFILES = [TZUGFeRDProfile.Minimum,
                    TZUGFeRDProfile.BasicWL,
                    TZUGFeRDProfile.Basic,
                    TZUGFeRDProfile.Comfort,
                    TZUGFeRDProfile.Extended,
                    TZUGFeRDProfile.XRechnung1,
                    TZUGFeRDProfile.XRechnung,
                    TZUGFeRDProfile.EReporting];
    PROFILE_COMFORT_EXTENDED_XRECHNUNG =
                   [TZUGFeRDProfile.Comfort,
                    TZUGFeRDProfile.Extended,
                    TZUGFeRDProfile.XRechnung1,
                    TZUGFeRDProfile.XRechnung];
  public
    function Validate(_descriptor: TZUGFeRDInvoiceDescriptor; _throwExceptions: Boolean = True): Boolean; override;
    /// <summary>
    /// Saves the given invoice to the given stream.
    /// Make sure that the stream is open and writeable. Otherwise, an IllegalStreamException will be thron.
    /// </summary>
    /// <param name="descriptor">The invoice object that should be saved</param>
    /// <param name="stream">The target stream for saving the invoice</param>
    /// <param name="format">Format of the target file</param>
    procedure Save(_descriptor: TZUGFeRDInvoiceDescriptor; _stream: TStream; _format : TZUGFeRDFormats = TZUGFeRDFormats.CII; options: TZUGFeRDInvoiceFormatOptions = Nil); override;
  end;

implementation

{ TZUGFeRDInvoiceDescriptor1Writer }

function TZUGFeRDInvoiceDescriptor1Writer.GetNameSpaces: TDictionary<string, string>;
begin
  Result := TDictionary<string, string>.Create;
  Result.Add('xsi', 'http://www.w3.org/2001/XMLSchema-instance');
  Result.Add('rsm', 'urn:ferd:CrossIndustryDocument:invoice:1p0');
  Result.Add('ram', 'urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:12');
  Result.Add('udt', 'urn:un:unece:uncefact:data:standard:UnqualifiedDataType:15');
end;

procedure TZUGFeRDInvoiceDescriptor1Writer.Save(_descriptor: TZUGFeRDInvoiceDescriptor; _stream: TStream; _format : TZUGFeRDFormats = TZUGFeRDFormats.CII; options: TZUGFeRDInvoiceFormatOptions = Nil);
var
  streamPosition : Int64;
begin
  if (_stream = nil) then
    raise TZUGFeRDIllegalStreamException.Create('Cannot write to stream');

  if _format = TZUGFeRDFormats.UBL then
    raise TZUGFeRDUnsupportedException.Create('UBL format is not supported for ZUGFeRD 1.');

  // validate data
  if ((_descriptor.Profile = TZUGFeRDProfile.BasicWL) or (_descriptor.Profile = TZUGFeRDProfile.Minimum)) then
    raise TZUGFeRDUnsupportedException.Create('Invalid profile used for ZUGFeRD 1.x invoice.');

  // write data
  streamPosition := _stream.Position;

  Descriptor := _descriptor;
  var automaticallyCleanInvalidXmlCharacters: boolean := false;
  if options<>Nil then
    automaticallyCleanInvalidXmlCharacters:= TZUGFeRDInvoiceFormatOptions(options).AutomaticallyCleanInvalidCharacters;
  Writer := TZUGFeRDProfileAwareXmlTextWriter.Create(_stream,TEncoding.UTF8,Descriptor.Profile, automaticallyCleanInvalidXmlCharacters);
  Writer.Formatting := TZUGFeRDXmlFomatting.xmlFormatting_Indented;
  Writer.SetNamespaces(GetNameSpaces); // Writer takes ownership of NameSpaces

  Writer.WriteStartDocument;
  WriteHeaderComments(Writer, options);

  //#region Kopfbereich
  Writer.WriteStartElement('rsm:CrossIndustryDocument');
  Writer.WriteAttributeString('xmlns', 'xsi', '', 'http://www.w3.org/2001/XMLSchema-instance');
  Writer.WriteAttributeString('xmlns', 'rsm', '', 'urn:ferd:CrossIndustryDocument:invoice:1p0');
  Writer.WriteAttributeString('xmlns', 'ram', '', 'urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:12');
  Writer.WriteAttributeString('xmlns', 'udt', '', 'urn:un:unece:uncefact:data:standard:UnqualifiedDataType:15');
  //#endregion

  //#region SpecifiedExchangedDocumentContext
  Writer.WriteStartElement('rsm:SpecifiedExchangedDocumentContext');
  if (Descriptor.IsTest) then
  begin
    Writer.WriteStartElement('ram:TestIndicator');
    Writer.WriteElementString('udt:Indicator', ifthen(Descriptor.IsTest,'true','false'));
    Writer.WriteEndElement(); // !ram:TestIndicator
  end;

  if (Descriptor.BusinessProcess <> '') then
  begin
    Writer.WriteStartElement('ram:BusinessProcessSpecifiedDocumentContextParameter', [TZUGFeRDProfile.Extended]);
    Writer.WriteElementString('ram:ID', Descriptor.BusinessProcess, [TZUGFeRDProfile.Extended]);
    Writer.WriteEndElement(); // !ram:BusinessProcessSpecifiedDocumentContextParameter
  end;

  Writer.WriteStartElement('ram:GuidelineSpecifiedDocumentContextParameter');
  Writer.WriteElementString('ram:ID', TZUGFeRDProfileExtensions.EnumToString(Descriptor.Profile,TZUGFeRDVersion.Version1));
  Writer.WriteEndElement(); // !ram:GuidelineSpecifiedDocumentContextParameter
  Writer.WriteEndElement(); // !rsm:SpecifiedExchangedDocumentContext

  Writer.WriteStartElement('rsm:HeaderExchangedDocument');
  Writer.WriteElementString('ram:ID', Descriptor.InvoiceNo);
  Writer.WriteElementString('ram:Name', Descriptor.Name);
  Writer.WriteElementString('ram:TypeCode', TEnumExtensions<TZUGFeRDInvoiceType>.EnumToString(Descriptor.Type_));

  if Descriptor.InvoiceDate.HasValue then
  begin
    Writer.WriteStartElement('ram:IssueDateTime');
    Writer.WriteStartElement('udt:DateTimeString');
    Writer.WriteAttributeString('format', '102');
    Writer.WriteValue(_formatDate(Descriptor.InvoiceDate.Value));
    Writer.WriteEndElement(); // !udt:DateTimeString
    Writer.WriteEndElement(); // !IssueDateTime
  end;
  _writeNotes(Writer, Descriptor.Notes);
  Writer.WriteEndElement(); // !rsm:HeaderExchangedDocument

  //#region SpecifiedSupplyChainTradeTransaction
  Writer.WriteStartElement('rsm:SpecifiedSupplyChainTradeTransaction');

  WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.ApplicableHeaderTradeAgreementComment);
  Writer.WriteStartElement('ram:ApplicableSupplyChainTradeAgreement');
  if (Descriptor.ReferenceOrderNo <> '') then
  begin
    WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.ApplicableHeaderTradeAgreementComment);
    Writer.WriteElementString('ram:BuyerReference', Descriptor.ReferenceOrderNo);
  end;

  WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.SellerTradePartyComment);
  _writeOptionalParty(Writer, 'ram:SellerTradeParty', Descriptor.Seller, Descriptor.SellerContact, Descriptor.SellerTaxRegistration);
  WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.BuyerTradePartyComment);
  _writeOptionalParty(Writer, 'ram:BuyerTradeParty', Descriptor.Buyer, Descriptor.BuyerContact, Descriptor.BuyerTaxRegistration);

  if (Descriptor.OrderNo <> '') then
  begin
    WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.BuyerOrderReferencedDocumentComment);
    Writer.WriteStartElement('ram:BuyerOrderReferencedDocument');
    if (Descriptor.OrderDate.HasValue) then
    begin
      Writer.WriteStartElement('ram:IssueDateTime');
      //Writer.WriteStartElement('udt:DateTimeString');
      //Writer.WriteAttributeString('format', '102');
      Writer.WriteValue(_formatDate(Descriptor.OrderDate.Value, false));
      //Writer.WriteEndElement(); // !udt:DateTimeString
      Writer.WriteEndElement(); // !IssueDateTime()
    end;

    Writer.WriteElementString('ram:ID', Descriptor.OrderNo);
    Writer.WriteEndElement(); // !BuyerOrderReferencedDocument
  end;

  for var document : TZUGFeRDAdditionalReferencedDocument in Descriptor.AdditionalReferencedDocuments do
  begin
    Writer.WriteStartElement('ram:AdditionalReferencedDocument');
    if (document.IssueDateTime.HasValue) then
    begin
      Writer.WriteStartElement('ram:IssueDateTime');
      //Writer.WriteStartElement('udt:DateTimeString');
      //Writer.WriteAttributeString('format', '102');
      Writer.WriteValue(_formatDate(document.IssueDateTime.Value, false));
      //Writer.WriteEndElement(); // !udt:DateTimeString
      Writer.WriteEndElement(); // !IssueDateTime()
    end;

    if document.TypeCode.HasValue then
    begin
      Writer.WriteElementString('ram:TypeCode', TEnumExtensions<TZUGFeRDAdditionalReferencedDocumentTypeCode>.EnumToString(document.TypeCode));
    end;

    if document.ReferenceTypeCode.HasValue then
    begin
      Writer.WriteElementString('ram:ReferenceTypeCode', TEnumExtensions<TZUGFeRDReferenceTypeCodes>.EnumToString(document.ReferenceTypeCode));
    end;

    Writer.WriteElementString('ram:ID', document.ID);
    Writer.WriteEndElement(); // !ram:AdditionalReferencedDocument
  end; // !foreach(document)

  Writer.WriteEndElement(); // !ApplicableSupplyChainTradeAgreement

  Writer.WriteStartElement('ram:ApplicableSupplyChainTradeDelivery'); // Pflichteintrag

    //RelatedSupplyChainConsignment --> SpecifiedLogisticsTransportMovement --> ModeCode // Only in extended profile
  if Descriptor.TransportMode <> nil then
  begin
    Writer.WriteStartElement('ram:RelatedSupplyChainConsignment', [TZUGFeRDProfile.Extended]); // BG-X-24
    Writer.WriteStartElement('ram:SpecifiedLogisticsTransportMovement', [TZUGFeRDProfile.Extended]); // BT-X-152-00
    Writer.WriteElementString('ram:ModeCode', TEnumExtensions<TZUGFeRDTransportmodeCodes>.EnumToString(Descriptor.TransportMode)); // BT-X-152
    Writer.WriteEndElement(); // !ram:SpecifiedLogisticsTransportMovement
    Writer.WriteEndElement(); // !ram:RelatedSupplyChainConsignment
  end;

  if Descriptor.Profile = TZUGFeRDProfile.Extended then
  begin
    _writeOptionalParty(Writer, 'ram:ShipToTradeParty', Descriptor.ShipTo);
    _writeOptionalParty(Writer, 'ram:ShipFromTradeParty', Descriptor.ShipFrom);
  end;

  WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.ApplicableHeaderTradeDeliveryComment);
  if (Descriptor.ActualDeliveryDate.HasValue) then
  begin
    Writer.WriteStartElement('ram:ActualDeliverySupplyChainEvent');
    Writer.WriteStartElement('ram:OccurrenceDateTime');
    Writer.WriteStartElement('udt:DateTimeString');
    Writer.WriteAttributeString('format', '102');
    Writer.WriteValue(_formatDate(Descriptor.ActualDeliveryDate.Value));
    Writer.WriteEndElement(); // 'udt:DateTimeString
    Writer.WriteEndElement(); // !OccurrenceDateTime()
    Writer.WriteEndElement(); // !ActualDeliverySupplyChainEvent
  end;

  if (Descriptor.DeliveryNoteReferencedDocument <> nil) then
  begin
    WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.DespatchAdviceReferencedDocumentComment);
    Writer.WriteStartElement('ram:DeliveryNoteReferencedDocument');

    if Descriptor.DeliveryNoteReferencedDocument.IssueDateTime.HasValue then
    begin
      Writer.WriteStartElement('ram:IssueDateTime');
      Writer.WriteValue(_formatDate(Descriptor.DeliveryNoteReferencedDocument.IssueDateTime.Value, false));
      Writer.WriteEndElement(); // !IssueDateTime
    end;

    Writer.WriteElementString('ram:ID', Descriptor.DeliveryNoteReferencedDocument.ID);
    Writer.WriteEndElement(); // !DeliveryNoteReferencedDocument
  end;

  Writer.WriteEndElement(); // !ApplicableSupplyChainTradeDelivery

  Writer.WriteStartElement('ram:ApplicableSupplyChainTradeSettlement');
  Writer.WriteElementString('ram:InvoiceCurrencyCode', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(Descriptor.Currency));

  if Descriptor.Profile <> TZUGFeRDProfile.Basic then
  begin
    _writeOptionalParty(Writer, 'ram:InvoiceeTradeParty', Descriptor.Invoicee);
  end;
  if Descriptor.Profile = TZUGFeRDProfile.Extended then
  begin
   _writeOptionalParty(Writer, 'ram:PayeeTradeParty', Descriptor.Payee);
  end;

  Writer.WriteOptionalElementString('ram:PaymentReference', Descriptor.PaymentReference);

  if (Descriptor.CreditorBankAccounts.Count = 0) and  (Descriptor.DebitorBankAccounts.Count = 0) then
  begin
    if Descriptor.PaymentMeans<> nil then
    begin
      WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.SpecifiedTradeSettlementPaymentMeansComment);
      Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans');

      if (Descriptor.PaymentMeans <> nil) and Descriptor.PaymentMeans.TypeCode.HasValue then
      begin
        Writer.WriteElementString('ram:TypeCode', TEnumExtensions<TZUGFeRDPaymentMeansTypeCodes>.EnumToString(Descriptor.PaymentMeans.TypeCode));
        Writer.WriteOptionalElementString('ram:Information', Descriptor.PaymentMeans.Information);

        if (Descriptor.PaymentMeans.SEPACreditorIdentifier <> '')
        and (Descriptor.PaymentMeans.SEPAMandateReference <> '') then
        begin
          Writer.WriteStartElement('ram:ID');
          Writer.WriteAttributeString('schemeAgencyID', Descriptor.PaymentMeans.SEPACreditorIdentifier);
          Writer.WriteValue(Descriptor.PaymentMeans.SEPAMandateReference);
          Writer.WriteEndElement(); // !ram:ID
        end;
      end;
      Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
    end;
  end
  else
  begin
    for var account : TZUGFeRDBankAccount in Descriptor.CreditorBankAccounts do
    begin
      WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.SpecifiedTradeSettlementPaymentMeansComment);
      Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans');

      if (Descriptor.PaymentMeans<> nil) and Descriptor.PaymentMeans.TypeCode.HasValue then
      begin
        Writer.WriteElementString('ram:TypeCode', TEnumExtensions<TZUGFeRDPaymentMeansTypeCodes>.EnumToString(Descriptor.PaymentMeans.TypeCode));
        Writer.WriteOptionalElementString('ram:Information', Descriptor.PaymentMeans.Information);

        if (Descriptor.PaymentMeans.SEPACreditorIdentifier <> '') and (Descriptor.PaymentMeans.SEPAMandateReference <> '') then
        begin
          Writer.WriteStartElement('ram:ID');
          Writer.WriteAttributeString('schemeAgencyID', Descriptor.PaymentMeans.SEPACreditorIdentifier);
          Writer.WriteValue(Descriptor.PaymentMeans.SEPAMandateReference);
          Writer.WriteEndElement(); // !ram:ID
        end;
      end;

      Writer.WriteStartElement('ram:PayeePartyCreditorFinancialAccount');
      Writer.WriteElementString('ram:IBANID', account.IBAN);
      if (account.Name <> '') then
        Writer.WriteOptionalElementString('ram:AccountName', account.Name);
      Writer.WriteOptionalElementString('ram:ProprietaryID', account.ID);
      Writer.WriteEndElement(); // !PayeePartyCreditorFinancialAccount

      Writer.WriteStartElement('ram:PayeeSpecifiedCreditorFinancialInstitution');
      Writer.WriteElementString('ram:BICID', account.BIC);
      Writer.WriteOptionalElementString('ram:GermanBankleitzahlID', account.Bankleitzahl);
      Writer.WriteOptionalElementString('ram:Name', account.BankName);
      Writer.WriteEndElement(); // !PayeeSpecifiedCreditorFinancialInstitution
      Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
    end;

    for var account : TZUGFeRDBankAccount in Descriptor.DebitorBankAccounts do
    begin
      Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans');

      if (Descriptor.PaymentMeans <> nil) then
      if (Descriptor.PaymentMeans.TypeCode <> TZUGFeRDPaymentMeansTypeCodes.Unknown) then
      begin
        Writer.WriteElementString('ram:TypeCode', TEnumExtensions<TZUGFeRDPaymentMeansTypeCodes>.EnumToString(Descriptor.PaymentMeans.TypeCode));
        Writer.WriteOptionalElementString('ram:Information', Descriptor.PaymentMeans.Information);

        if (Descriptor.PaymentMeans.SEPACreditorIdentifier <> '') and (Descriptor.PaymentMeans.SEPAMandateReference <> '') then
        begin
          Writer.WriteStartElement('ram:ID');
          Writer.WriteAttributeString('schemeAgencyID', Descriptor.PaymentMeans.SEPACreditorIdentifier);
          Writer.WriteValue(Descriptor.PaymentMeans.SEPAMandateReference);
          Writer.WriteEndElement(); // !ram:ID
        end;
      end;

      Writer.WriteStartElement('ram:PayerPartyDebtorFinancialAccount');
      Writer.WriteElementString('ram:IBANID', account.IBAN);
      Writer.WriteOptionalElementString('ram:ProprietaryID', account.ID);
      Writer.WriteEndElement(); // !PayerPartyDebtorFinancialAccount

      Writer.WriteStartElement('ram:PayerSpecifiedDebtorFinancialInstitution');
      Writer.WriteElementString('ram:BICID', account.BIC);
      Writer.WriteOptionalElementString('ram:GermanBankleitzahlID', account.Bankleitzahl);
      Writer.WriteOptionalElementString('ram:Name', account.BankName);
      Writer.WriteEndElement(); // !PayerSpecifiedDebtorFinancialInstitution
      Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
    end;
  end;

  _writeOptionalTaxes(Writer, options);

  for var tradeAllowance : TZUGFeRDTradeAllowance in Descriptor.GetTradeAllowances do
    _WriteDocumentLevelSpecifiedTradeAllowanceCharge(Writer, tradeAllowance);
  for var tradeCharge : TZUGFeRDTradeCharge in Descriptor.GetTradeCharges do
    _WriteDocumentLevelSpecifiedTradeAllowanceCharge(Writer, tradeCharge);

  for var serviceCharge : TZUGFeRDServiceCharge in Descriptor.ServiceCharges do
  begin
    Writer.WriteStartElement('ram:SpecifiedLogisticsServiceCharge');
    Writer.WriteOptionalElementString('ram:Description', serviceCharge.Description, [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);
    Writer.WriteElementString('ram:AppliedAmount', _formatDecimal(serviceCharge.Amount), [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);
    if serviceCharge.Tax <> nil then
    begin
      Writer.WriteStartElement('ram:AppliedTradeTax');
      if serviceCharge.Tax.TypeCode.HasValue then
        Writer.WriteElementString('ram:TypeCode', TEnumExtensions<TZUGFeRDTaxTypes>.EnumToString(serviceCharge.Tax.TypeCode), [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);
      if serviceCharge.Tax.CategoryCode.HasValue then
        Writer.WriteElementString('ram:CategoryCode', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(serviceCharge.Tax.CategoryCode), [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);
      Writer.WriteElementString('ram:ApplicablePercent', _formatDecimal(serviceCharge.Tax.Percent), [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);
      Writer.WriteEndElement();
    end;
    Writer.WriteEndElement();
  end;

  case Descriptor.Profile of
    TZUGFeRDProfile.Unknown,
    TZUGFeRDProfile.Minimum: {do nothing};

    TZUGFeRDProfile.Extended:
    begin
      for var PaymentTerms: TZUGFeRDPaymentTerms in Descriptor.PaymentTermsList do
      begin
        Writer.WriteStartElement('ram:SpecifiedTradePaymentTerms');
        Writer.WriteOptionalElementString('ram:Description', PaymentTerms.Description);
        if (PaymentTerms.DueDate.HasValue) then
        begin
          Writer.WriteStartElement('ram:DueDateDateTime');
          _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(PaymentTerms.DueDate.Value));
          Writer.WriteEndElement(); // !ram:DueDateDateTime
        end;
        Writer.WriteOptionalElementString('ram:DirectDebitMandateID', _descriptor.PaymentMeans.SEPAMandateReference);
        _writeOptionalAmount(Writer, 'ram:PartialPaymentAmount', paymentTerms.PartialPaymentAmount);
        if PaymentTerms.PaymentTermsType.HasValue then
        begin
          if PaymentTerms.PaymentTermsType = TZUGFeRDPaymentTermsType.Skonto then
          begin
            Writer.WriteStartElement('ram:ApplicableTradePaymentDiscountTerms');
            _writeOptionalAmount(Writer, 'ram:BasisAmount', paymentTerms.BaseAmount); // forceCurrency false by default
            Writer.WriteOptionalElementString('ram:CalculationPercent', _formatDecimal(paymentTerms.Percentage));
            _writeOptionalAmount(Writer, 'ram:ActualDiscountAmount', paymentTerms.ActualAmount);
          Writer.WriteEndElement(); // !ram:ApplicableTradePaymentDiscountTerms
          end;
          if PaymentTerms.PaymentTermsType = TZUGFeRDPaymentTermsType.Verzug then
          begin
            Writer.WriteStartElement('ram:ApplicableTradePaymentPenaltyTerms');
            _writeOptionalAmount(Writer, 'ram:BasisAmount', paymentTerms.BaseAmount); // forceCurrency false by default
            Writer.WriteOptionalElementString('ram:CalculationPercent', _formatDecimal(paymentTerms.Percentage));
            _writeOptionalAmount(Writer, 'ram:ActualPenaltyAmount', paymentTerms.ActualAmount);
            Writer.WriteEndElement(); // !ram:ApplicableTradePaymentPenaltyTerms
          end;
        end;
        Writer.WriteEndElement();
      end;
      if (_descriptor.PaymentTermsList.Count=0) and (_descriptor.PaymentMeans.SEPAMandateReference<>'') then
      begin
        Writer.WriteStartElement('ram:SpecifiedTradePaymentTerms');
        Writer.WriteOptionalElementString('ram:DirectDebitMandateID', _descriptor.PaymentMeans.SEPAMandateReference);
        Writer.WriteEndElement();
      end;
    end;
  else
    if Descriptor.PaymentTermsList.Count>0 then
    begin
      Writer.WriteStartElement('ram:SpecifiedTradePaymentTerms');
      var sbPaymentNotes: TStringBuilder := TStringBuilder.Create;
      var dueDate: ZUGFeRDNullable<TDateTime>;
      try
        dueDate.ClearValue;  // ZUGFeRDNullable Record - nicht nil zuweisen!
        for var PaymentTerms: TZUGFeRDPaymentTerms in Descriptor.PaymentTermsList do
        begin
          sbPaymentNotes.AppendLine(paymentTerms.Description);
          dueDate := IfThen(dueDate.HasValue, dueDate, paymentTerms.DueDate);
        end;
        Writer.WriteOptionalElementString('ram:Description', Trim( sbPaymentNotes.ToString));
      finally
        sbPaymentNotes.Free;
      end;
      if dueDate.HasValue then
      begin
        Writer.WriteStartElement('ram:DueDateDateTime');
        _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(DueDate.Value));
        Writer.WriteEndElement(); // !ram:DueDateDateTime
      end;
      Writer.WriteEndElement(); // !ram:SpecifiedTradePaymentTerms
    end;
  end;

  WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.SpecifiedTradeSettlementHeaderMonetarySummationComment);
  Writer.WriteStartElement('ram:SpecifiedTradeSettlementMonetarySummation');
  _writeOptionalAmount(Writer, 'ram:LineTotalAmount', Descriptor.LineTotalAmount);

  _writeOptionalAmount(Writer, 'ram:ChargeTotalAmount', Descriptor.ChargeTotalAmount.Value);  // must occur exactly once!
  _writeOptionalAmount(Writer, 'ram:AllowanceTotalAmount', Descriptor.AllowanceTotalAmount.Value); // must occur exactly once!
  _writeOptionalAmount(Writer, 'ram:TaxBasisTotalAmount', Descriptor.TaxBasisAmount);
  _writeOptionalAmount(Writer, 'ram:TaxTotalAmount', Descriptor.TaxTotalAmount);
  _writeOptionalAmount(Writer, 'ram:GrandTotalAmount', Descriptor.GrandTotalAmount);
  _writeOptionalAmount(Writer, 'ram:TotalPrepaidAmount', Descriptor.TotalPrepaidAmount);
  _writeOptionalAmount(Writer, 'ram:DuePayableAmount', Descriptor.DuePayableAmount);
  Writer.WriteEndElement(); // !ram:SpecifiedTradeSettlementMonetarySummation

  Writer.WriteEndElement(); // !ram:ApplicableSupplyChainTradeSettlement

  for var tradeLineItem :TZUGFeRDTradeLineItem in Descriptor.TradeLineItems do
  begin
    WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.IncludedSupplyChainTradeLineItemComment);
    Writer.WriteStartElement('ram:IncludedSupplyChainTradeLineItem');

    if (tradeLineItem.AssociatedDocument<> nil)
    and (tradeLineItem.AssociatedDocument.LineID <> '') then
    begin
      Writer.WriteStartElement('ram:AssociatedDocumentLineDocument');
      Writer.WriteElementString('ram:LineID', tradeLineItem.AssociatedDocument.LineID);
//      Writer.WriteOptionalElementString('ram:LineStatusCode', tradeLineItem.AssociatedDocument.LineStatusCode);
//      Writer.WriteOptionalElementString('ram:LineStatusReasonCode', tradeLineItem.AssociatedDocument.LineStatusReasonCode);
      _writeNotes(Writer, tradeLineItem.AssociatedDocument.Notes);
      Writer.WriteEndElement(); // ram:AssociatedDocumentLineDocument
    end;

    // handelt es sich um einen Kommentar?
    if (tradeLineItem.AssociatedDocument <> nil)
    and (tradeLineItem.AssociatedDocument.Notes.Count > 0) and
        (tradeLineItem.BilledQuantity = 0) and
        (tradeLineItem.Description = '') then
    begin
      Writer.WriteEndElement(); // !ram:IncludedSupplyChainTradeLineItem
      continue;
    end;

    if (Descriptor.Profile <> TZUGFeRDProfile.Basic) then
    begin
      Writer.WriteStartElement('ram:SpecifiedSupplyChainTradeAgreement');

      if (tradeLineItem.BuyerOrderReferencedDocument<> nil) then
      begin
        Writer.WriteStartElement('ram:BuyerOrderReferencedDocument');
        if (tradeLineItem.BuyerOrderReferencedDocument.IssueDateTime.HasValue) then
        begin
          Writer.WriteStartElement('ram:IssueDateTime');
          Writer.WriteValue(_formatDate(tradeLineItem.BuyerOrderReferencedDocument.IssueDateTime.Value, false));
          Writer.WriteEndElement(); // !ram:IssueDateTime
        end;
        Writer.WriteOptionalElementString('ram:LineID', tradeLineItem.BuyerOrderReferencedDocument.LineID);
        Writer.WriteOptionalElementString('ram:ID', tradeLineItem.BuyerOrderReferencedDocument.ID);
        Writer.WriteEndElement(); // !ram:BuyerOrderReferencedDocument
      end;

      if (tradeLineItem.ContractReferencedDocument<> nil) then
      begin
        Writer.WriteStartElement('ram:ContractReferencedDocument');


        if (tradeLineItem.ContractReferencedDocument.IssueDateTime.HasValue) then
        begin
          Writer.WriteStartElement('ram:IssueDateTime');
          Writer.WriteValue(_formatDate(tradeLineItem.ContractReferencedDocument.IssueDateTime.Value, false));
          Writer.WriteEndElement(); // !ram:IssueDateTime
        end;
        // reference to the contract position
        Writer.WriteOptionalElementString('ram:LineID', tradeLineItem.ContractReferencedDocument.LineID);
        Writer.WriteOptionalElementString('ram:ID', tradeLineItem.ContractReferencedDocument.ID);
        Writer.WriteEndElement(); // !ram:ContractReferencedDocument
      end;

      if (tradeLineItem.AdditionalReferencedDocuments<> nil) then
      begin
        for var document : TZUGFeRDAdditionalReferencedDocument in tradeLineItem.AdditionalReferencedDocuments do
        begin
          Writer.WriteStartElement('ram:AdditionalReferencedDocument');
          if (document.IssueDateTime.HasValue) then
          begin
            Writer.WriteStartElement('ram:IssueDateTime');
            Writer.WriteValue(_formatDate(document.IssueDateTime.Value, false));
            Writer.WriteEndElement(); // !ram:IssueDateTime
          end;

          Writer.WriteElementString('ram:LineID', tradeLineItem.AssociatedDocument.LineID);
          Writer.WriteOptionalElementString('ram:ID', document.ID);
          if document.ReferenceTypeCode.HasValue then
            Writer.WriteElementString('ram:ReferenceTypeCode', TEnumExtensions<TZUGFeRDReferenceTypeCodes>.EnumToString(document.ReferenceTypeCode));

          Writer.WriteEndElement(); // !ram:AdditionalReferencedDocument
        end;
      end;

      Writer.WriteStartElement('ram:GrossPriceProductTradePrice');
      _writeOptionalAdaptiveAmount(Writer, 'ram:ChargeAmount', tradeLineItem.GrossUnitPrice, 4, 4, true);
      if tradeLineItem.GrossQuantity.HasValue then
      begin
        Writer.WriteStartElement('ram:BasisQuantity');
        if tradeLineItem.GrossUnitCode.HasValue then
          Writer.WriteAttributeString('unitCode', TEnumExtensions<TZUGFeRDQuantityCodes>.EnumToString(tradeLineItem.GrossUnitCode));
        Writer.WriteValue(_formatDecimal(tradeLineItem.GrossQuantity.Value, 4));
        Writer.WriteEndElement(); // !ram:BasisQuantity
      end;

      for var tradeAllowanceCharge : TZUGFeRDAbstractTradeAllowanceCharge in tradeLineItem.TradeAllowanceCharges do
      begin
        Writer.WriteStartElement('ram:AppliedTradeAllowanceCharge');

        Writer.WriteStartElement('ram:ChargeIndicator', [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);
        Writer.WriteElementString('udt:Indicator', ifthen(tradeAllowanceCharge.ChargeIndicator,'true','false'));
        Writer.WriteEndElement(); // !ram:ChargeIndicator

        if tradeAllowanceCharge.BasisAmount.HasValue then
        begin
          Writer.WriteStartElement('ram:BasisAmount', [TZUGFeRDProfile.Extended]);
          Writer.WriteAttributeString('currencyID', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(tradeAllowanceCharge.Currency));
          Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.BasisAmount, 4));
          Writer.WriteEndElement();
        end;
        Writer.WriteStartElement('ram:ActualAmount', [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);
        Writer.WriteAttributeString('currencyID', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(tradeAllowanceCharge.Currency));
        Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ActualAmount, 4));
        Writer.WriteEndElement();

        Writer.WriteOptionalElementString('ram:Reason', tradeAllowanceCharge.Reason, [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);

        Writer.WriteEndElement(); // !AppliedTradeAllowanceCharge
      end;

      Writer.WriteEndElement(); // ram:GrossPriceProductTradePrice

      WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.NetPriceProductTradePriceComment);
      Writer.WriteStartElement('ram:NetPriceProductTradePrice');
      _writeOptionalAdaptiveAmount(Writer, 'ram:ChargeAmount', tradeLineItem.NetUnitPrice, 4, 4, true);

      if tradeLineItem.NetQuantity.HasValue then
      begin
        Writer.WriteStartElement('ram:BasisQuantity');
        if tradeLineItem.NetUnitCode.HasValue then
          Writer.WriteAttributeString('unitCode', TEnumExtensions<TZUGFeRDQuantityCodes>.EnumToString(tradeLineItem.NetUnitCode));
       Writer.WriteValue(_formatDecimal(tradeLineItem.NetQuantity.Value, 4));
       Writer.WriteEndElement(); // !ram:BasisQuantity
      end;
      Writer.WriteEndElement(); // ram:NetPriceProductTradePrice

      Writer.WriteEndElement(); // !ram:SpecifiedSupplyChainTradeAgreement
    end;

    if (Descriptor.Profile <> TZUGFeRDProfile.Basic) then
    begin
      Writer.WriteStartElement('ram:SpecifiedSupplyChainTradeDelivery');
      _writeElementWithAttribute(Writer, 'ram:BilledQuantity', 'unitCode', TEnumExtensions<TZUGFeRDQuantityCodes>.EnumToString(tradeLineItem.UnitCode), _formatDecimal(tradeLineItem.BilledQuantity, 4));
      if tradeLineItem.PackageQuantity.HasValue then
        _writeElementWithAttribute(Writer, 'ram:PackageQuantity', 'unitCode', TEnumExtensions<TZUGFeRDQuantityCodes>.EnumToString(tradeLineItem.PackageUnitCode), _formatDecimal(tradeLineItem.PackageQuantity, 4));
      if tradeLineItem.ChargeFreeQuantity.HasValue then
        _writeElementWithAttribute(Writer, 'ram:ChargeFreeQuantity', 'unitCode', TEnumExtensions<TZUGFeRDQuantityCodes>.EnumToString(tradeLineItem.ChargeFreeUnitCode), _formatDecimal(tradeLineItem.ChargeFreeQuantity, 4));

      if (tradeLineItem.DeliveryNoteReferencedDocument<> nil) then
      begin
          Writer.WriteStartElement('ram:DeliveryNoteReferencedDocument');

          if (tradeLineItem.DeliveryNoteReferencedDocument.IssueDateTime.HasValue) then
          begin
            Writer.WriteStartElement('ram:IssueDateTime');
            Writer.WriteValue(_formatDate(tradeLineItem.DeliveryNoteReferencedDocument.IssueDateTime.Value, false));
            Writer.WriteEndElement(); // !ram:IssueDateTime
          end;
           // reference to the delivery note item
          Writer.WriteOptionalElementString('ram:LineID', tradeLineItem.DeliveryNoteReferencedDocument.LineID);
          Writer.WriteOptionalElementString('ram:ID', tradeLineItem.DeliveryNoteReferencedDocument.ID);
          Writer.WriteEndElement(); // !ram:DeliveryNoteReferencedDocument
      end;

      if (tradeLineItem.ActualDeliveryDate.HasValue) then
      begin
        Writer.WriteStartElement('ram:ActualDeliverySupplyChainEvent');
        Writer.WriteStartElement('ram:OccurrenceDateTime');
        Writer.WriteStartElement('udt:DateTimeString');
        Writer.WriteAttributeString('format', '102');
        Writer.WriteValue(_formatDate(tradeLineItem.ActualDeliveryDate.Value));
        Writer.WriteEndElement(); // 'udt:DateTimeString
        Writer.WriteEndElement(); // !OccurrenceDateTime()
        Writer.WriteEndElement(); // !ActualDeliverySupplyChainEvent
      end;

      if (tradeLineItem.DeliveryNoteReferencedDocument<> nil) then
      begin
          Writer.WriteStartElement('ram:DeliveryNoteReferencedDocument');

          // reference to the delivery note item
          Writer.WriteOptionalElementString('ram:LineID', tradeLineItem.DeliveryNoteReferencedDocument.LineID);
          if (tradeLineItem.DeliveryNoteReferencedDocument.IssueDateTime.HasValue) then
          begin
            Writer.WriteStartElement('ram:IssueDateTime');
            Writer.WriteValue(_formatDate(tradeLineItem.DeliveryNoteReferencedDocument.IssueDateTime.Value, false));
            Writer.WriteEndElement(); // !ram:IssueDateTime
          end;
          Writer.WriteOptionalElementString('ram:ID', tradeLineItem.DeliveryNoteReferencedDocument.ID);
          Writer.WriteEndElement(); // !ram:DeliveryNoteReferencedDocument
      end;

      Writer.WriteEndElement(); // !ram:SpecifiedSupplyChainTradeDelivery
    end
    else
    begin
      Writer.WriteStartElement('ram:SpecifiedSupplyChainTradeDelivery');
      _writeElementWithAttribute(Writer, 'ram:BilledQuantity', 'unitCode', TEnumExtensions<TZUGFeRDQuantityCodes>.EnumToString(tradeLineItem.UnitCode), _formatDecimal(tradeLineItem.BilledQuantity, 4));
      Writer.WriteEndElement(); // !ram:SpecifiedSupplyChainTradeDelivery
    end;

    Writer.WriteStartElement('ram:SpecifiedSupplyChainTradeSettlement');

    if (Descriptor.Profile <> TZUGFeRDProfile.Basic) then
    begin
      Writer.WriteStartElement('ram:ApplicableTradeTax');
      if tradeLineItem.TaxType.HasValue  then
        Writer.WriteElementString('ram:TypeCode', TEnumExtensions<TZUGFeRDTaxTypes>.EnumToString(tradeLineItem.TaxType));
      if tradeLineItem.TaxCategoryCode.HasValue then
        Writer.WriteElementString('ram:CategoryCode', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(tradeLineItem.TaxCategoryCode));
      Writer.WriteElementString('ram:ApplicablePercent', _formatDecimal(tradeLineItem.TaxPercent));
      Writer.WriteEndElement(); // !ram:ApplicableTradeTax
    end;

    if (tradeLineItem.BillingPeriodStart.HasValue and tradeLineItem.BillingPeriodEnd.HasValue) then
    begin
      Writer.WriteStartElement('ram:BillingSpecifiedPeriod', [TZUGFeRDProfile.BasicWL,TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung1, TZUGFeRDProfile.XRechnung]);

      Writer.WriteStartElement('ram:StartDateTime');
      _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(tradeLineItem.BillingPeriodStart.Value));
      Writer.WriteEndElement(); // !StartDateTime

      Writer.WriteStartElement('ram:EndDateTime');
      _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(tradeLineItem.BillingPeriodEnd.Value));
      Writer.WriteEndElement(); // !EndDateTime

      Writer.WriteEndElement(); // !BillingSpecifiedPeriod
    end;

    WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.SpecifiedTradeSettlementLineMonetarySummationComment);
    Writer.WriteStartElement('ram:SpecifiedTradeSettlementMonetarySummation');

    var _total : Currency := 0;

    if (tradeLineItem.LineTotalAmount.HasValue) then
    begin
      _total := tradeLineItem.LineTotalAmount.Value;
    end
    else if (tradeLineItem.NetUnitPrice.HasValue) then
    begin
      _total := tradeLineItem.NetUnitPrice.Value * tradeLineItem.BilledQuantity;
      if tradeLineItem.NetQuantity.HasValue then
      if (tradeLineItem.NetQuantity.Value <> 0) then
      begin
        _total := _total / tradeLineItem.NetQuantity.Value;
      end;
    end;

    _writeElementWithAttribute(Writer, 'ram:LineTotalAmount', 'currencyID', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(Descriptor.Currency), _formatDecimal(_total));
    Writer.WriteEndElement(); // ram:SpecifiedTradeSettlementMonetarySummation
    Writer.WriteEndElement(); // !ram:SpecifiedSupplyChainTradeSettlement

    Writer.WriteStartElement('ram:SpecifiedTradeProduct');
    if (tradeLineItem.GlobalID<> nil) and (tradeLineItem.GlobalID.SchemeID.HasValue) and (tradeLineItem.GlobalID.ID <> '') then
    begin
      _writeElementWithAttribute(Writer, 'ram:GlobalID', 'schemeID', TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.EnumToString(tradeLineItem.GlobalID.SchemeID), tradeLineItem.GlobalID.ID);
    end;

    Writer.WriteOptionalElementString('ram:SellerAssignedID', tradeLineItem.SellerAssignedID);
    Writer.WriteOptionalElementString('ram:BuyerAssignedID', tradeLineItem.BuyerAssignedID);
    Writer.WriteOptionalElementString('ram:Name', tradeLineItem.Name);
    Writer.WriteOptionalElementString('ram:Description', tradeLineItem.Description);

    Writer.WriteEndElement(); // !ram:SpecifiedTradeProduct
    Writer.WriteEndElement(); // !ram:IncludedSupplyChainTradeLineItem
  end; // !foreach(tradeLineItem)

  Writer.WriteEndElement(); // !ram:SpecifiedSupplyChainTradeTransaction

  Writer.WriteEndElement(); // !ram:Invoice
  Writer.WriteEndDocument();
  Writer.Flush();
  Writer.Free;

  _stream.Seek(streamPosition, soFromBeginning);
end;// !Save()

procedure TZUGFeRDInvoiceDescriptor1Writer._writeDocumentLevelSpecifiedTradeAllowanceCharge(_writer: TZUGFeRDProfileAwareXmlTextWriter; tradeAllowanceCharge: TZUGFeRDAbstractTradeAllowanceCharge);
begin
  if tradeAllowanceCharge=Nil then
    exit;

  Writer.WriteStartElement('ram:SpecifiedTradeAllowanceCharge');
  Writer.WriteStartElement('ram:ChargeIndicator', [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);
  Writer.WriteElementString('udt:Indicator', ifthen(tradeAllowanceCharge.ChargeIndicator,'true','false'));
  Writer.WriteEndElement(); // !ram:ChargeIndicator

  if tradeAllowanceCharge.BasisAmount.HasValue then
  begin
    Writer.WriteStartElement('ram:BasisAmount', [TZUGFeRDProfile.Extended]);
    Writer.WriteAttributeString('currencyID', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(tradeAllowanceCharge.Currency));
    Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.BasisAmount,4));
    Writer.WriteEndElement();
  end;

  Writer.WriteStartElement('ram:ActualAmount', [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);
  Writer.WriteAttributeString('currencyID', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(tradeAllowanceCharge.Currency));
  Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ActualAmount, 4));
  Writer.WriteEndElement();

  if tradeAllowanceCharge is TZUGFeRDTradeAllowance then
  begin
    var allowance:= tradeAllowanceCharge as TZUGFeRDTradeAllowance;
    if allowance.ReasonCode.HasValue then
      Writer.WriteOptionalElementString('ram:ReasonCode', TEnumExtensions<TZUGFeRDAllowanceReasonCodes>.EnumToString(allowance.ReasonCode));
  end
  else
  if tradeAllowanceCharge is TZUGFeRDTradeCharge then
  begin
    var charge:= tradeAllowanceCharge as TZUGFeRDTradeCharge;
    if charge.ReasonCode.HasValue then
      Writer.WriteOptionalElementString('ram:ReasonCode', TEnumExtensions<TZUGFeRDChargeReasonCodes>.EnumToString(charge.ReasonCode));
  end;

  Writer.WriteOptionalElementString('ram:Reason', tradeAllowanceCharge.Reason, [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);

  if (tradeAllowanceCharge.Tax<> nil) then
  begin
    Writer.WriteStartElement('ram:CategoryTradeTax');
    if tradeAllowanceCharge.Tax.TypeCode.HasValue then
      Writer.WriteElementString('ram:TypeCode', TEnumExtensions<TZUGFeRDTaxTypes>.EnumToString(tradeAllowanceCharge.Tax.TypeCode), [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);
    if tradeAllowanceCharge.Tax.CategoryCode.HasValue then
      Writer.WriteElementString('ram:CategoryCode', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(tradeAllowanceCharge.Tax.CategoryCode), [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);
    Writer.WriteElementString('ram:ApplicablePercent', _formatDecimal(tradeAllowanceCharge.Tax.Percent), [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);
    Writer.WriteEndElement();
  end;
  Writer.WriteEndElement();
end;

procedure TZUGFeRDInvoiceDescriptor1Writer._writeElementWithAttribute(
  _writer : TZUGFeRDProfileAwareXmlTextWriter;
  tagName : String; attributeName : String;
  attributeValue : String; nodeValue: String);
begin
  _writer.WriteStartElement(tagName);
  _writer.WriteAttributeString(attributeName, attributeValue);
  _writer.WriteValue(nodeValue);
  _writer.WriteEndElement(); // !tagName
end;

procedure TZUGFeRDInvoiceDescriptor1Writer._writeOptionalTaxes(_writer : TZUGFeRDProfileAwareXmlTextWriter; options: TZUGFeRDInvoiceFormatOptions);
begin
  for var tax : TZUGFeRDTax in Descriptor.Taxes do
  begin
    WriteComment(_Writer, options, TZUGFeRDInvoiceCommentConstants.ApplicableTradeTaxComment);
    _writer.WriteStartElement('ram:ApplicableTradeTax');

    _writer.WriteStartElement('ram:CalculatedAmount');
    _writer.WriteAttributeString('currencyID', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(Descriptor.Currency));
    _writer.WriteValue(_formatDecimal(tax.TaxAmount));
    _writer.WriteEndElement(); // !CalculatedAmount

    _writer.WriteElementString('ram:TypeCode', TEnumExtensions<TZUGFeRDTaxTypes>.EnumToString(tax.TypeCode));

    _writer.WriteStartElement('ram:BasisAmount');
    _writer.WriteAttributeString('currencyID', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(Descriptor.Currency));
    _writer.WriteValue(_formatDecimal(tax.BasisAmount));
    _writer.WriteEndElement(); // !BasisAmount

    if Descriptor.Profile = TZUGFeRDProfile.Extended then
    begin
      if tax.LineTotalBasisAmount.HasValue and (tax.LineTotalBasisAmount <> 0.0)  then
      begin
        _writer.WriteStartElement('ram:LineTotalBasisAmount', [TZUGFeRDProfile.Extended]);
        _writer.WriteAttributeString('currencyID', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(Descriptor.Currency));
        _writer.WriteValue(_formatDecimal(tax.LineTotalBasisAmount));
        _writer.WriteEndElement(); // !LineTotalBasisAmount
      end;

      if tax.AllowanceChargeBasisAmount.HasValue and (tax.AllowanceChargeBasisAmount <> 0.0) then
      begin
        _writer.WriteStartElement('ram:AllowanceChargeBasisAmount');
        _writer.WriteAttributeString('currencyID', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(Descriptor.Currency));
        _writer.WriteValue(_formatDecimal(tax.AllowanceChargeBasisAmount));
        _writer.WriteEndElement(); // !AllowanceChargeBasisAmount
      end;
    end;
    if tax.CategoryCode.HasValue then
      _writer.WriteElementString('ram:CategoryCode', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(tax.CategoryCode));
    _writer.WriteElementString('ram:ApplicablePercent', _formatDecimal(tax.Percent));
    _writer.WriteEndElement(); // !ApplicableTradeTax
  end;
end;

procedure TZUGFeRDInvoiceDescriptor1Writer._writeNotes(_writer : TZUGFeRDProfileAwareXmlTextWriter; notes : TObjectList<TZUGFeRDNote>);
begin
  if notes.Count = 0 then
    exit;

  for var note : TZUGFeRDNote in notes do
  begin
    _writer.WriteStartElement('ram:IncludedNote');
    if note.ContentCode.HasValue then
      _writer.WriteElementString('ram:ContentCode', TEnumExtensions<TZUGFeRDContentCodes>.EnumToString(note.ContentCode));
    _writer.WriteElementString('ram:Content', note.Content);
    if note.SubjectCode.HasValue then
      _writer.WriteElementString('ram:SubjectCode', TEnumExtensions<TZUGFeRDSubjectCodes>.EnumToString(note.SubjectCode));
    _writer.WriteEndElement();
  end;
end;

procedure TZUGFeRDInvoiceDescriptor1Writer._writeOptionalParty(
  _writer: TZUGFeRDProfileAwareXmlTextWriter;
  PartyTag : String; Party : TZUGFeRDParty; Contact : TZUGFeRDContact = nil;
  TaxRegistrations : TObjectList<TZUGFeRDTaxRegistration> = nil);
begin
  if Party = nil then
    exit;
  _writer.WriteStartElement(PartyTag);

  if (Party.ID <> nil) then
  if (Party.ID.ID <> '') then
  begin
    if Party.ID.SchemeID.HasValue then
    begin
      _writer.WriteStartElement('ram:ID');
      _writer.WriteAttributeString('schemeID', TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.EnumToString(Party.ID.SchemeID));
      _writer.WriteValue(Party.ID.ID);
      _writer.WriteEndElement();
    end else
    begin
      _writer.WriteElementString('ram:ID', Party.ID.ID);
    end;
  end;

  if (Party.GlobalID <> nil) then
  if (Party.GlobalID.ID <> '') and Party.GlobalID.SchemeID.HasValue then
  begin
    _writer.WriteStartElement('ram:GlobalID');
    _writer.WriteAttributeString('schemeID', TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.EnumToString(Party.GlobalID.SchemeID));
    _writer.WriteValue(Party.GlobalID.ID);
    _writer.WriteEndElement();
  end;

  _Writer.WriteOptionalElementString('ram:Name', Party.Name);
  _Writer.WriteOptionalElementString('ram:Description', Party.Description, PROFILE_COMFORT_EXTENDED_XRECHNUNG);
  _writeOptionalContact(_writer, 'ram:DefinedTradeContact', Contact);
  _writer.WriteStartElement('ram:PostalTradeAddress');
  _writer.WriteOptionalElementString('ram:PostcodeCode', Party.Postcode);
  _writer.WriteOptionalElementString('ram:LineOne', ifthen(Party.ContactName = '', Party.Street,Party.ContactName));
  if (Party.ContactName <> '') then
    _writer.WriteOptionalElementString('ram:LineTwo', Party.Street);
  _writer.WriteOptionalElementString('ram:CityName', Party.City);
  if party.Country.HasValue then
    writer.WriteElementString('ram:CountryID', TEnumExtensions<TZUGFeRDCountryCodes>.EnumToString(party.Country)); //buyer: BT-55
  _writer.WriteEndElement(); // !PostalTradeAddress

  if (TaxRegistrations <> nil) then
  begin
    for var _reg : TZUGFeRDTaxRegistration in TaxRegistrations do
    if (_reg.No <> '') then
    begin
      _writer.WriteStartElement('ram:SpecifiedTaxRegistration');
      _writer.WriteStartElement('ram:ID');
      _writer.WriteAttributeString('schemeID', TEnumExtensions<TZUGFeRDTaxRegistrationSchemeID>.EnumToString(_reg.SchemeID));
      _writer.WriteValue(_reg.No);
      _writer.WriteEndElement();
      _writer.WriteEndElement();
    end;
  end;
  _writer.WriteEndElement(); // !*TradeParty
end;

procedure TZUGFeRDInvoiceDescriptor1Writer._writeOptionalContact(
  _writer: TZUGFeRDProfileAwareXmlTextWriter; contactTag : String;
  contact : TZUGFeRDContact);
begin
  if contact = nil then
    exit;

  _writer.WriteStartElement(contactTag);

  _writer.WriteOptionalElementString('ram:PersonName', contact.Name);
  _writer.WriteOptionalElementString('ram:DepartmentName', contact.OrgUnit);

  if (contact.PhoneNo <> '') then
  begin
    _writer.WriteStartElement('ram:TelephoneUniversalCommunication');
    _writer.WriteElementString('ram:CompleteNumber', contact.PhoneNo);
    _writer.WriteEndElement();
  end;

  if (contact.FaxNo <> '') then
  begin
    _writer.WriteStartElement('ram:FaxUniversalCommunication');
    _writer.WriteElementString('ram:CompleteNumber', contact.FaxNo);
    _writer.WriteEndElement();
  end;

  if (contact.EmailAddress <> '') then
  begin
    _writer.WriteStartElement('ram:EmailURIUniversalCommunication');
    _writer.WriteElementString('ram:URIID', contact.EmailAddress);
    _writer.WriteEndElement();
  end;

  _writer.WriteEndElement();
end;

(*
function TZUGFeRDInvoiceDescriptor1Writer._translateInvoiceType(type_ : TZUGFeRDInvoiceType) : String;
begin
  case type_ of
    Invoice: Result := 'RECHNUNG';
    Correction: Result := 'KORREKTURRECHNUNG';
    CreditNote: Result := 'GUTSCHRIFT';
    DebitnoteRelatedToFinancialAdjustments: Result := 'WERTBELASTUNG';
    DebitNote: Result := '';
    SelfBilledInvoice: Result := '';
    else Result := '';
  end;
end;

function TZUGFeRDInvoiceDescriptor1Writer._encodeInvoiceType(type_ : TZUGFeRDInvoiceType) : Integer;
begin
  if (Integer(type_) > 1000) then
    type_ := TZUGFeRDInvoiceType(Integer(type_)-1000);

  // only these types are allowed
  // 84: 'Wertbelastung/Wertrechnung ohne Warenbezug'
  // 380: 'Handelsrechnung (Rechnung für Waren und Dienstleistungen)'
  // 389: 'Selbst ausgestellte Rechnung (Steuerrechtliche Gutschrift/Gutschriftsverfahren)'
  //
  // this is documented in ZUGFeRD-Format_1p0_c1p0_Codelisten.pdf
  // all other types are mapped accordingly
  case type_ of
    TZUGFeRDInvoiceType.SelfBilledInvoice: Result := Integer(TZUGFeRDInvoiceType.SelfBilledInvoice);
    TZUGFeRDInvoiceType.DebitnoteRelatedToFinancialAdjustments: Result := Integer(TZUGFeRDInvoiceType.DebitnoteRelatedToFinancialAdjustments);
    TZUGFeRDInvoiceType.Unknown: Result := Integer(TZUGFeRDInvoiceType.Unknown);
    else Result := Integer(TZUGFeRDInvoiceType.Invoice);
  end;
end;
*)

function TZUGFeRDInvoiceDescriptor1Writer.Validate(
  _descriptor: TZUGFeRDInvoiceDescriptor; _throwExceptions: Boolean): Boolean;
begin
  Result := false;

  if not (_descriptor.Profile in [TZUGFeRDProfile.Basic,
                             TZUGFeRDProfile.Comfort,
                             TZUGFeRDProfile.Extended]) then
  if (_throwExceptions) then
    raise TZUGFeRDUnsupportedException.Create('Invalid TZUGFeRDProfile used for ZUGFeRD 1.0 invoice.')
  else
    exit;

  if (_descriptor.Profile <> TZUGFeRDProfile.Extended) then // check tax types, only extended TZUGFeRDProfile allows tax types other than vat
  begin
    for var l : TZUGFeRDTradeLineItem in _descriptor.TradeLineItems do
    if not ((l.TaxType = TZUGFeRDTaxTypes.Unknown) or
      (l.TaxType = TZUGFeRDTaxTypes.VAT)) then
    begin
      if (_throwExceptions) then
        raise TZUGFeRDUnsupportedException.Create('Tax types other than VAT only possible with extended TZUGFeRDProfile.')
      else
        exit;
    end;
  end;

  Result := true;
end;

procedure TZUGFeRDInvoiceDescriptor1Writer._writeOptionalAmount(
  _writer: TZUGFeRDProfileAwareXmlTextWriter; _tagName: string;
  _value: ZUGFeRDNullable<Currency>;
  _numDecimals: Integer);
begin
  if (_value.HasValue) then // && (value.Value != decimal.MinValue))
  begin
    _writer.WriteStartElement(_tagName);
    _writer.WriteAttributeString('currencyID', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(Descriptor.Currency));
    _writer.WriteValue(_formatDecimal(_value.Value, _numDecimals));
    _writer.WriteEndElement; // !tagName
  end;
end;

procedure TZUGFeRDInvoiceDescriptor1Writer._writeOptionalAdaptiveAmount(
  _writer: TZUGFeRDProfileAwareXmlTextWriter; _tagName: string;
  _value: ZUGFeRDNullable<Currency>;
  _numDecimals: Integer;
  _maxnumDecimals: Integer;
  _forceCurrency: boolean);
begin
  if _value.HasValue then
  begin
    _writer.WriteStartElement(_tagName);
    if _forceCurrency then
      _writer.WriteAttributeString('currencyID', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(Descriptor.Currency));
    var
      rounded: Currency := RoundTo(_value.Value, -_numDecimals);
    if _value = rounded then
      _writer.WriteValue(_formatDecimal(_value.Value, _numDecimals))
    else
      _writer.WriteValue(_formatDecimal(_value.Value, _maxNumDecimals));
    writer.WriteEndElement; // !tagName
  end
end;

end.
