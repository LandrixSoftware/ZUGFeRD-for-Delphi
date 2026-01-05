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

unit intf.ZUGFeRDInvoiceDescriptor20Writer;

interface

uses
  System.SysUtils,System.Classes,System.StrUtils,System.Generics.Collections,
  System.Math
  ,intf.ZUGFeRDInvoiceDescriptor
  ,intf.ZUGFeRDProfileAwareXmlTextWriter
  ,intf.ZUGFeRDIInvoiceDescriptorWriter
  ,intf.ZUGFeRDExceptions
  ,intf.ZUGFeRDProfile
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
  ,intf.ZUGFeRDDateTypeCodes
  ,intf.ZUGFeRDTradeLineItem
  ,intf.ZUGFeRDAdditionalReferencedDocument
  ,intf.ZUGFeRDAdditionalReferencedDocumentTypeCodes
  ,intf.ZUGFeRDReferenceTypeCodes
  ,intf.ZUGFeRDPaymentMeansTypeCodes
  ,intf.ZUGFeRDPaymentTerms
  ,intf.ZUGFeRDBankAccount
  ,intf.ZUGFeRDTradeAllowanceCharge
  ,intf.ZUGFeRDServiceCharge
  ,intf.ZUGFeRDQuantityCodes
  ,intf.ZUGFeRDInvoiceTypes
  ,intf.ZUGFeRDTaxExemptionReasonCodes
  ,intf.ZUGFeRDApplicableProductCharacteristic
  ,intf.ZUGFeRDSpecialServiceDescriptionCodes
  ,intf.ZUGFeRDFormats
  ,intf.ZUGFeRDTransportmodeCodes
  ,intf.ZUGFeRDChargeReasonCodes
  ,intf.ZUGFeRDAllowanceReasonCodes
  ,intf.ZUGFeRDInvoiceFormatOptions
  ,intf.ZUGFeRDInvoiceCommentConstants
  ,intf.ZUGFeRDLineStatusCodes
  ,intf.ZUGFeRDLineStatusReasonCodes
  ,intf.ZUGFeRDTradeDeliveryTermCodes
  ,intf.ZUGFeRDIncludedReferencedProduct
  ,intf.ZUGFeRDInvoiceReferencedDocument
  ,intf.ZUGFeRDMimeTypeMapper
  ;

type
  TZUGFeRDInvoiceDescriptor20Writer = class(TZUGFeRDIInvoiceDescriptorWriter)
  private
    Writer: TZUGFeRDProfileAwareXmlTextWriter;
    Descriptor: TZUGFeRDInvoiceDescriptor;
    procedure _writeDocumentLevelSpecifiedTradeAllowanceCharge(_writer: TZUGFeRDProfileAwareXmlTextWriter; tradeAllowanceCharge: TZUGFeRDAbstractTradeAllowanceCharge);
    procedure _writeOptionalAmount(_writer : TZUGFeRDProfileAwareXmlTextWriter; tagName : string; value : ZUGFeRDNullable<Currency>; numDecimals : Integer = 2; forceCurrency : Boolean = false; profile : TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
    procedure _writeOptionalAdaptiveAmount(_writer: TZUGFeRDProfileAwareXmlTextWriter; _tagName: string; _value: ZUGFeRDNullable<Currency>; _numDecimals: Integer = 2;_maxnumDecimals: Integer = 4; _forceCurrency: boolean = false);
    procedure _writeOptionalContact(_writer: TZUGFeRDProfileAwareXmlTextWriter; contactTag: String; contact: TZUGFeRDContact);
    procedure _writeOptionalParty(_writer: TZUGFeRDProfileAwareXmlTextWriter; PartyTag: String; Party: TZUGFeRDParty; Contact: TZUGFeRDContact = nil; TaxRegistrations: TObjectList<TZUGFeRDTaxRegistration> = nil);
    procedure _writeOptionalTaxes(_writer : TZUGFeRDProfileAwareXmlTextWriter; options: TZUGFeRDInvoiceFormatOptions);
    procedure _writeNotes(_writer : TZUGFeRDProfileAwareXmlTextWriter;notes : TObjectList<TZUGFeRDNote>);
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
    /// <param name="options">Optional `InvoiceFormatOptions` for custom formatting of invoice file</param>
    procedure Save (_descriptor: TZUGFeRDInvoiceDescriptor; _stream: TStream; _format : TZUGFeRDFormats = TZUGFeRDFormats.CII; options: TZUGFeRDInvoiceFormatOptions = Nil); override;
  end;

implementation

{ TZUGFeRDInvoiceDescriptor20Writer }

function TZUGFeRDInvoiceDescriptor20Writer.GetNameSpaces: TDictionary<string, string>;
begin
  Result := TDictionary<string, string>.Create;
  Result.Add('a', 'urn:un:unece:uncefact:data:standard:QualifiedDataType:100');
  Result.Add('rsm', 'urn:un:unece:uncefact:data:standard:CrossIndustryInvoice:100');
  Result.Add('qdt', 'urn:un:unece:uncefact:data:standard:QualifiedDataType:100');
  Result.Add('ram', 'urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:100');
  Result.Add('xs', 'http://www.w3.org/2001/XMLSchema');
  Result.Add('udt', 'urn:un:unece:uncefact:data:standard:UnqualifiedDataType:100');
end;

procedure TZUGFeRDInvoiceDescriptor20Writer.Save  (_descriptor: TZUGFeRDInvoiceDescriptor; _stream: TStream; _format : TZUGFeRDFormats = TZUGFeRDFormats.CII; options: TZUGFeRDInvoiceFormatOptions = Nil);
var
  streamPosition : Int64;
begin
  if (_stream = nil) then
    raise TZUGFeRDIllegalStreamException.Create('Cannot write to stream');

  if _format = TZUGFeRDFormats.UBL then
    raise TZUGFeRDUnsupportedException.Create('UBL format is not supported for ZUGFeRD 2.0');

  // write data
  streamPosition := _stream.Position;

  Descriptor := _descriptor;
  var automaticallyCleanInvalidXmlCharacters: boolean := false;
  if options<>Nil then
    automaticallyCleanInvalidXmlCharacters:= TZUGFeRDInvoiceFormatOptions(options).AutomaticallyCleanInvalidCharacters;
  Writer := TZUGFeRDProfileAwareXmlTextWriter.Create(_stream,TEncoding.UTF8,Descriptor.Profile, automaticallyCleanInvalidXmlCharacters);
  Writer.SetNamespaces(GetNameSpaces); // Writer takes ownership of NameSpaces

  Writer.WriteStartDocument;
  WriteHeaderComments(Writer, options);

  //#region Kopfbereich
  Writer.WriteStartElement('rsm:CrossIndustryInvoice');
  Writer.WriteAttributeString('xmlns', 'a', '', 'urn:un:unece:uncefact:data:standard:QualifiedDataType:100');
  Writer.WriteAttributeString('xmlns', 'rsm', '', 'urn:un:unece:uncefact:data:standard:CrossIndustryInvoice:100');
  Writer.WriteAttributeString('xmlns', 'qdt', '', 'urn:un:unece:uncefact:data:standard:QualifiedDataType:100');
  Writer.WriteAttributeString('xmlns', 'ram', '', 'urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:100');
  Writer.WriteAttributeString('xmlns', 'xs', '', 'http://www.w3.org/2001/XMLSchema');
  Writer.WriteAttributeString('xmlns', 'udt', '', 'urn:un:unece:uncefact:data:standard:UnqualifiedDataType:100');
  //#endregion

  //#region ExchangedDocumentContext
  Writer.WriteStartElement('rsm:ExchangedDocumentContext');
  if Descriptor.IsTest then
  begin
    Writer.WriteStartElement('ram:TestIndicator');
    Writer.WriteElementString('udt:Indicator', ifthen(Descriptor.IsTest,'true','false'));
    Writer.WriteEndElement(); // !ram:TestIndicator
  end;

  if Descriptor.BusinessProcess <> '' then
  begin
    Writer.WriteStartElement('ram:BusinessProcessSpecifiedDocumentContextParameter');
    Writer.WriteElementString('ram:ID', Descriptor.BusinessProcess);
    Writer.WriteEndElement(); // !ram:BusinessProcessSpecifiedDocumentContextParameter
  end;

  Writer.WriteStartElement('ram:GuidelineSpecifiedDocumentContextParameter');
  Writer.WriteElementString('ram:ID', TZUGFeRDProfileExtensions.EnumToString(Descriptor.Profile,TZUGFeRDVersion.Version20));
  Writer.WriteEndElement(); // !ram:GuidelineSpecifiedDocumentContextParameter
  Writer.WriteEndElement(); // !rsm:ExchangedDocumentContext

  Writer.WriteStartElement('rsm:ExchangedDocument');
  Writer.WriteElementString('ram:ID', Descriptor.InvoiceNo);
  Writer.WriteElementString('ram:Name', Descriptor.Name, [TZUGFeRDProfile.Extended]);
  Writer.WriteElementString('ram:TypeCode', TEnumExtensions<TZUGFeRDInvoiceType>.EnumToString(Descriptor.Type_));

  if Descriptor.InvoiceDate.HasValue then
  begin
      Writer.WriteStartElement('ram:IssueDateTime');
      Writer.WriteStartElement('udt:DateTimeString');
      Writer.WriteAttributeString('format', '102');
      Writer.WriteValue(_formatDate(Descriptor.InvoiceDate));
      Writer.WriteEndElement(); // !udt:DateTimeString
      Writer.WriteEndElement(); // !IssueDateTime
  end;
  _writeNotes(Writer, Descriptor.Notes);
  Writer.WriteEndElement(); // !rsm:ExchangedDocument

  //*
  // * @todo continue here to adopt v2 tag names
  // */

  //#region SpecifiedSupplyChainTradeTransaction
  Writer.WriteStartElement('rsm:SupplyChainTradeTransaction');

  for var tradeLineItem : TZUGFeRDTradeLineItem in Descriptor.TradeLineItems do
  begin
    WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.ApplicableHeaderTradeAgreementComment);
    Writer.WriteStartElement('ram:IncludedSupplyChainTradeLineItem');

    if (tradeLineItem.AssociatedDocument <> nil) then
    begin
      Writer.WriteStartElement('ram:AssociatedDocumentLineDocument');
      Writer.WriteElementString('ram:LineID', tradeLineItem.AssociatedDocument.LineID);
      if tradeLineItem.AssociatedDocument.LineStatusCode.HasValue then
        Writer.WriteOptionalElementString('ram:LineStatusCode', TEnumExtensions<TZUGFeRDLineStatusCodes>.EnumToString(tradeLineItem.AssociatedDocument.LineStatusCode));
      if tradeLineItem.AssociatedDocument.LineStatusReasonCode.HasValue then
        Writer.WriteOptionalElementString('ram:LineStatusReasonCode', TEnumExtensions<TZUGFeRDLineStatusReasonCodes>.EnumToString(tradeLineItem.AssociatedDocument.LineStatusReasonCode));
      _writeNotes(Writer, tradeLineItem.AssociatedDocument.Notes);
      Writer.WriteEndElement(); // ram:AssociatedDocumentLineDocument

      // handelt es sich um einen Kommentar?
      if ((tradeLineItem.AssociatedDocument.Notes.Count > 0) and  (tradeLineItem.BilledQuantity = 0) and (tradeLineItem.Description <> '')) then
      begin
        Writer.WriteEndElement(); // !ram:IncludedSupplyChainTradeLineItem
        continue;
      end;
    end;

    Writer.WriteStartElement('ram:SpecifiedTradeProduct');
    if (tradeLineItem.GlobalID <> nil) and (tradeLineItem.GlobalID.SchemeID.HasValue) and (tradeLineItem.GlobalID.ID <> '') then
    begin
      _writeElementWithAttribute(Writer, 'ram:GlobalID', 'schemeID', TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.EnumToString(tradeLineItem.GlobalID.SchemeID), tradeLineItem.GlobalID.ID);
    end;

    Writer.WriteOptionalElementString('ram:SellerAssignedID', tradeLineItem.SellerAssignedID);
    Writer.WriteOptionalElementString('ram:BuyerAssignedID', tradeLineItem.BuyerAssignedID);
    Writer.WriteOptionalElementString('ram:Name', tradeLineItem.Name);
    Writer.WriteOptionalElementString('ram:Description', tradeLineItem.Description);

    if tradeLineItem.ApplicableProductCharacteristics <> nil then
      for var productCharacteristic : TZUGFeRDApplicableProductCharacteristic in tradeLineItem.ApplicableProductCharacteristics do
      begin
        Writer.WriteStartElement('ram:ApplicableProductCharacteristic');
        Writer.WriteOptionalElementString('ram:Description', productCharacteristic.Description);
        Writer.WriteOptionalElementString('ram:Value', productCharacteristic.Value);
        Writer.WriteEndElement(); // !ram:ApplicableProductCharacteristic
      end;

    if tradeLineItem.IncludedReferencedProducts <> nil then
      for var includedItem : TZUGFeRDIncludedReferencedProduct in tradeLineItem.IncludedReferencedProducts do
      begin
        Writer.WriteStartElement('ram:IncludedReferencedProduct');
        if (includedItem.GlobalID <> nil) and includedItem.GlobalID.SchemeID.HasValue and (includedItem.GlobalID.ID <> '') then
          _writeElementWithAttribute(Writer, 'ram:GlobalID', 'schemeID', TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.EnumToString(includedItem.GlobalID.SchemeID), includedItem.GlobalID.ID);
        Writer.WriteOptionalElementString('ram:SellerAssignedID', includedItem.SellerAssignedID);
        Writer.WriteOptionalElementString('ram:BuyerAssignedID', includedItem.BuyerAssignedID);
        Writer.WriteOptionalElementString('ram:IndustryAssignedID', includedItem.IndustryAssignedID);
        Writer.WriteOptionalElementString('ram:Name', includedItem.Name); // BT-X-18
        Writer.WriteOptionalElementString('ram:Description', includedItem.Description);
        if includedItem.UnitQuantity.HasValue then
          _writeElementWithAttribute(Writer, 'ram:UnitQuantity', 'unitCode', TEnumExtensions<TZUGFeRDQuantityCodes>.EnumToString(includedItem.UnitCode), _formatDecimal(includedItem.UnitQuantity, 4));
        Writer.WriteEndElement(); // !ram:IncludedReferencedProduct
      end;

    Writer.WriteEndElement(); // !ram:SpecifiedTradeProduct

    Writer.WriteStartElement('ram:SpecifiedLineTradeAgreement', [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);

    if (tradeLineItem.BuyerOrderReferencedDocument <> nil) then
    begin
      Writer.WriteStartElement('ram:BuyerOrderReferencedDocument', [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);

      // order number
      Writer.WriteOptionalElementString('ram:IssuerAssignedID', tradeLineItem.BuyerOrderReferencedDocument.ID);

      // reference to the order position
      Writer.WriteOptionalElementString('ram:LineID', tradeLineItem.BuyerOrderReferencedDocument.LineID);

      if (tradeLineItem.BuyerOrderReferencedDocument.IssueDateTime.HasValue) then
      begin
        Writer.WriteStartElement('ram:FormattedIssueDateTime');
        Writer.WriteStartElement('qdt:DateTimeString');
        Writer.WriteAttributeString('format', '102');
        Writer.WriteValue(_formatDate(tradeLineItem.BuyerOrderReferencedDocument.IssueDateTime.Value));
        Writer.WriteEndElement(); // !qdt:DateTimeString
        Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
      end;

      Writer.WriteEndElement(); // !ram:BuyerOrderReferencedDocument
    end;

    if (tradeLineItem.ContractReferencedDocument <> nil) then
    begin
      Writer.WriteStartElement('ram:ContractReferencedDocument', [TZUGFeRDProfile.Extended]);

      Writer.WriteOptionalElementString('ram:IssuerAssignedID', tradeLineItem.ContractReferencedDocument.ID);

      // reference to the contract position
      Writer.WriteOptionalElementString('ram:LineID', tradeLineItem.ContractReferencedDocument.LineID);

      if (tradeLineItem.ContractReferencedDocument.IssueDateTime.HasValue) then
      begin
        Writer.WriteStartElement('ram:FormattedIssueDateTime');
        Writer.WriteStartElement('qdt:DateTimeString');
        Writer.WriteAttributeString('format', '102');
        Writer.WriteValue(_formatDate(tradeLineItem.ContractReferencedDocument.IssueDateTime.Value));
        Writer.WriteEndElement(); // !udt:DateTimeString
        Writer.WriteEndElement(); // !ram:IssueDateTime
      end;
      Writer.WriteEndElement(); // !ram:ContractReferencedDocument(Extended)
    end;

    for var document : TZUGFeRDAdditionalReferencedDocument in tradeLineItem.AdditionalReferencedDocuments do
    begin
      Writer.WriteStartElement('ram:AdditionalReferencedDocument', [TZUGFeRDProfile.Extended]);
      if (document.IssueDateTime.HasValue) then
      begin
          Writer.WriteStartElement('ram:FormattedIssueDateTime');
          Writer.WriteStartElement('qdt:DateTimeString');
          Writer.WriteValue(_formatDate(document.IssueDateTime.Value));
          Writer.WriteEndElement(); // !udt:DateTimeString
          Writer.WriteEndElement(); // !ram:IssueDateTime
      end;

      if tradeLineItem.AssociatedDocument<>Nil then
        Writer.WriteElementString('ram:LineID', tradeLineItem.AssociatedDocument.LineID);
      Writer.WriteOptionalElementString('ram:IssuerAssignedID', document.ID);
      if document.TypeCode.HasValue then
        Writer.WriteElementString('ram:TypeCode', TEnumExtensions<TZUGFeRDAdditionalReferencedDocumentTypeCode>.EnumToString(document.TypeCode));

      if document.ReferenceTypeCode.HasValue then
        Writer.WriteElementString('ram:ReferenceTypeCode', TEnumExtensions<TZUGFeRDReferenceTypeCodes>.EnumToString(document.ReferenceTypeCode));

      Writer.WriteEndElement(); // !ram:AdditionalReferencedDocument
    end; // !foreach(document)

    var needToWriteGrossUnitPrice: boolean;
    var hasGrossUnitPrice: boolean := tradeLineItem.GrossUnitPrice.HasValue;
    var hasAllowanceCharges: boolean := tradeLineItem.TradeAllowanceCharges.Count > 0;

    // the PEPPOL business rule for XRechnung is very specific
    // PEPPOL-EN16931-R046
    if (descriptor.Profile in [TZUGFeRDProfile.XRechnung, TZUGFeRDProfile.XRechnung1, TZUGFeRDProfile.Comfort]) then
      needToWriteGrossUnitPrice := hasGrossUnitPrice and hasAllowanceCharges // PEPPOL-EN16931-R046: For XRechnung, both must be present
    else
      needToWriteGrossUnitPrice := hasGrossUnitPrice or hasAllowanceCharges; // For other profiles, either is sufficient

    if needToWriteGrossUnitPrice then
    begin
      Writer.WriteStartElement('ram:GrossPriceProductTradePrice');
      _writeOptionalAdaptiveAmount(Writer, 'ram:ChargeAmount', tradeLineItem.GrossUnitPrice, 2, 4);    // BT-148
      if tradeLineItem.GrossQuantity.HasValue then
        _writeElementWithAttribute(Writer, 'ram:BasisQuantity', 'unitCode', TEnumExtensions<TZUGFeRDQuantityCodes>.EnumToString(tradeLineItem.GrossUnitCode), _formatDecimal(tradeLineItem.GrossQuantity.Value, 4));

      for var tradeAllowanceCharge : TZUGFeRDAbstractTradeAllowanceCharge in tradeLineItem.TradeAllowanceCharges do
      begin
        Writer.WriteStartElement('ram:AppliedTradeAllowanceCharge');

        Writer.WriteStartElement('ram:ChargeIndicator');
        Writer.WriteElementString('udt:Indicator', ifthen(tradeAllowanceCharge.ChargeIndicator,'true','false'));
        Writer.WriteEndElement(); // !ram:ChargeIndicator

        _writeOptionalAdaptiveAmount(Writer, 'BasisAmount', tradeAllowanceCharge.BasisAmount, 2, 4, false); // BT-X-35
        _writeOptionalAdaptiveAmount(Writer, 'ActualAmount', tradeAllowanceCharge.ActualAmount, 2, 4, false); // BT-147

        Writer.WriteOptionalElementString('ram:Reason', tradeAllowanceCharge.Reason);
       // "ReasonCode" nicht im 2.0 Standard!

        Writer.WriteEndElement(); // !AppliedTradeAllowanceCharge
      end;

      Writer.WriteEndElement(); // ram:GrossPriceProductTradePrice
    end;

    WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.NetPriceProductTradePriceComment);
    Writer.WriteStartElement('ram:NetPriceProductTradePrice');
    _writeOptionalAdaptiveAmount(Writer, 'ram:ChargeAmount', tradeLineItem.NetUnitPrice,  2, 4);

    if tradeLineItem.NetQuantity.HasValue then
      _writeElementWithAttribute(Writer, 'ram:BasisQuantity', 'unitCode', TEnumExtensions<TZUGFeRDQuantityCodes>.EnumToString(tradeLineItem.NetUnitCode), _formatDecimal(tradeLineItem.NetQuantity.Value, 4));

    Writer.WriteEndElement(); // ram:NetPriceProductTradePrice

    Writer.WriteEndElement(); // !ram:SpecifiedLineTradeAgreement

    if (Descriptor.Profile <> TZUGFeRDProfile.Basic) then
    begin
      Writer.WriteStartElement('ram:SpecifiedLineTradeDelivery');
      _writeElementWithAttribute(Writer, 'ram:BilledQuantity', 'unitCode', TEnumExtensions<TZUGFeRDQuantityCodes>.EnumToString(tradeLineItem.UnitCode), _formatDecimal(tradeLineItem.BilledQuantity, 4));
      if tradeLineItem.PackageQuantity.HasValue then
        _writeElementWithAttribute(Writer, 'ram:PackageQuantity', 'unitCode', TEnumExtensions<TZUGFeRDQuantityCodes>.EnumToString(tradeLineItem.PackageUnitCode), _formatDecimal(tradeLineItem.PackageQuantity, 4));
      if tradeLineItem.ChargeFreeQuantity.HasValue then
        _writeElementWithAttribute(Writer, 'ram:ChargeFreeQuantity', 'unitCode', TEnumExtensions<TZUGFeRDQuantityCodes>.EnumToString(tradeLineItem.ChargeFreeUnitCode), _formatDecimal(tradeLineItem.ChargeFreeQuantity, 4));

      if (tradeLineItem.DeliveryNoteReferencedDocument <> nil) then
      begin
        Writer.WriteStartElement('ram:DeliveryNoteReferencedDocument');
        if (tradeLineItem.DeliveryNoteReferencedDocument.IssueDateTime.HasValue) then
        begin
          //Old path of Version 1.0
          //Writer.WriteStartElement('ram:IssueDateTime');
          //Writer.WriteValue(_formatDate(tradeLineItem.DeliveryNoteReferencedDocument.IssueDateTime.Value));
          //Writer.WriteEndElement(); // !ram:IssueDateTime

          Writer.WriteStartElement('ram:FormattedIssueDateTime');
          Writer.WriteStartElement('qdt:DateTimeString');
          Writer.WriteAttributeString('format', '102');
          Writer.WriteValue(_formatDate(Descriptor.OrderDate.Value));
          Writer.WriteEndElement(); // 'qdt:DateTimeString
          Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
        end;

        Writer.WriteOptionalElementString('ram:IssuerAssignedID', tradeLineItem.DeliveryNoteReferencedDocument.ID);
        Writer.WriteOptionalElementString('ram:LineID', tradeLineItem.DeliveryNoteReferencedDocument.LineID);
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

      Writer.WriteEndElement(); // !ram:SpecifiedLineTradeDelivery
    end
    else
    begin
      Writer.WriteStartElement('ram:SpecifiedLineTradeDelivery');
      _writeElementWithAttribute(Writer, 'ram:BilledQuantity', 'unitCode', TEnumExtensions<TZUGFeRDQuantityCodes>.EnumToString(tradeLineItem.UnitCode), _formatDecimal(tradeLineItem.BilledQuantity, 4));
      Writer.WriteEndElement(); // !ram:SpecifiedLineTradeDelivery
    end;

    Writer.WriteStartElement('ram:SpecifiedLineTradeSettlement');

    Writer.WriteStartElement('ram:ApplicableTradeTax', [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);
    if tradeLineItem.TaxType.HasValue then
      Writer.WriteElementString('ram:TypeCode', TEnumExtensions<TZUGFeRDTaxTypes>.EnumToString(tradeLineItem.TaxType));
    if tradeLineItem.TaxCategoryCode.HasValue then
      Writer.WriteElementString('ram:CategoryCode', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(tradeLineItem.TaxCategoryCode)); // BT-151
    Writer.WriteElementString('ram:RateApplicablePercent', _formatDecimal(tradeLineItem.TaxPercent));
    Writer.WriteEndElement(); // !ram:ApplicableTradeTax

    if (tradeLineItem.BillingPeriodStart.HasValue or tradeLineItem.BillingPeriodEnd.HasValue) then
    begin
      Writer.WriteStartElement('ram:BillingSpecifiedPeriod', [TZUGFeRDProfile.BasicWL,TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);
      if (tradeLineItem.BillingPeriodStart.HasValue) then
      begin
        Writer.WriteStartElement('ram:StartDateTime');
        _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(tradeLineItem.BillingPeriodStart.Value));
        Writer.WriteEndElement(); // !StartDateTime
      end;

      if (tradeLineItem.BillingPeriodEnd.HasValue) then
      begin
        Writer.WriteStartElement('ram:EndDateTime');
        _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(tradeLineItem.BillingPeriodEnd.Value));
        Writer.WriteEndElement(); // !EndDateTime
      end;
      Writer.WriteEndElement(); // !BillingSpecifiedPeriod
    end;

    WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.SpecifiedTradeSettlementLineMonetarySummationComment);
    Writer.WriteStartElement('ram:SpecifiedTradeSettlementLineMonetarySummation');

    var _total : Currency := 0;

    if tradeLineItem.LineTotalAmount.HasValue then
      _total := tradeLineItem.LineTotalAmount.Value
    else
    if tradeLineItem.NetUnitPrice.HasValue then
    begin
      _total := tradeLineItem.NetUnitPrice.Value * tradeLineItem.BilledQuantity;
      if tradeLineItem.NetQuantity.HasValue
      and (tradeLineItem.NetQuantity.Value <> 0) then
        _total := _total / tradeLineItem.NetQuantity.Value
    end;

    Writer.WriteElementString('ram:LineTotalAmount', _formatDecimal(_total));

    Writer.WriteEndElement(); // ram:SpecifiedTradeSettlementLineMonetarySummation
    Writer.WriteEndElement(); // !ram:SpecifiedLineTradeSettlement

    Writer.WriteEndElement(); // !ram:IncludedSupplyChainTradeLineItem
  end; // !foreach(tradeLineItem)

  Writer.WriteStartElement('ram:ApplicableHeaderTradeAgreement');
  WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.BuyerReferenceComment);
  Writer.WriteOptionalElementString('ram:BuyerReference', Descriptor.ReferenceOrderNo);
  WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.SellerTradePartyComment);
  _writeOptionalParty(Writer, 'ram:SellerTradeParty', Descriptor.Seller, Descriptor.SellerContact, Descriptor.SellerTaxRegistration);
  WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.BuyerTradePartyComment);
  _writeOptionalParty(Writer, 'ram:BuyerTradeParty', Descriptor.Buyer, Descriptor.BuyerContact, Descriptor.BuyerTaxRegistration);

  //#region ApplicableTradeDeliveryTerms
  if Descriptor.ApplicableTradeDeliveryTermsCode.HasValue then
  begin
    // BG-X-22, BT-X-145
    Writer.WriteStartElement('ram:ApplicableTradeDeliveryTerms', [TZUGFeRDProfile.Extended]);
    Writer.WriteElementString('DeliveryTypeCode', TEnumExtensions<TZUGFeRDTradeDeliveryTermCodes>.EnumToString(Descriptor.ApplicableTradeDeliveryTermsCode));
    Writer.WriteEndElement(); // !ApplicableTradeDeliveryTerms
  end;
  //#endregion

  //#region SellerTaxRepresentativeTradeParty
  // BT-63: the tax registration of the SellerTaxRepresentativeTradeParty
  _writeOptionalParty(Writer, 'ram:SellerTaxRepresentativeTradeParty', Descriptor.SellerTaxRepresentative, Nil, Descriptor.SellerTaxRepresentativeTaxRegistration);
  //#endregion

  //#region SellerOrderReferencedDocument (BT-14: Comfort, Extended)
  if (Descriptor.SellerOrderReferencedDocument <> nil) then
  if (Descriptor.SellerOrderReferencedDocument.ID <> '') then
  begin
    Writer.WriteStartElement('ram:SellerOrderReferencedDocument', [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);
    Writer.WriteElementString('ram:IssuerAssignedID', Descriptor.SellerOrderReferencedDocument.ID);
    if (Descriptor.SellerOrderReferencedDocument.IssueDateTime.HasValue) then
    begin
      Writer.WriteStartElement('ram:FormattedIssueDateTime', [TZUGFeRDProfile.Extended]);
      Writer.WriteStartElement('qdt:DateTimeString');
      Writer.WriteAttributeString('format', '102');
      Writer.WriteValue(_formatDate(Descriptor.SellerOrderReferencedDocument.IssueDateTime.Value));
      Writer.WriteEndElement(); // !qdt:DateTimeString
      Writer.WriteEndElement(); // !IssueDateTime()
    end;

    Writer.WriteEndElement(); // !SellerOrderReferencedDocument
  end;
  //#endregion

  if (Descriptor.OrderNo<>'') then
  begin
    WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.BuyerOrderReferencedDocumentComment);
    Writer.WriteStartElement('ram:BuyerOrderReferencedDocument');
    Writer.WriteElementString('ram:IssuerAssignedID', Descriptor.OrderNo);
    if (Descriptor.OrderDate.HasValue) then
    begin
      Writer.WriteStartElement('ram:FormattedIssueDateTime');
      Writer.WriteStartElement('qdt:DateTimeString');
      Writer.WriteAttributeString('format', '102');
      Writer.WriteValue(_formatDate(Descriptor.OrderDate.Value));
      Writer.WriteEndElement(); // !qdt:DateTimeString
      Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
    end;
    Writer.WriteEndElement(); // !BuyerOrderReferencedDocument
  end;

  if (Descriptor.AdditionalReferencedDocuments <> nil) then
  begin
    for var document : TZUGFeRDAdditionalReferencedDocument in Descriptor.AdditionalReferencedDocuments do
    begin
      Writer.WriteStartElement('ram:AdditionalReferencedDocument');
      Writer.WriteElementString('ram:IssuerAssignedID', document.ID);

      if (document.IssueDateTime.HasValue) then
      begin
        Writer.WriteStartElement('ram:FormattedIssueDateTime');
        Writer.WriteStartElement('qdt:DateTimeString');
        Writer.WriteAttributeString('format', '102');
        Writer.WriteValue(_formatDate(document.IssueDateTime.Value));
        Writer.WriteEndElement(); // !udt:DateTimeString
        Writer.WriteEndElement(); // !FormattedIssueDateTime
      end;

      if document.TypeCode.HasValue then
        Writer.WriteElementString('ram:TypeCode', TEnumExtensions<TZUGFeRDAdditionalReferencedDocumentTypeCode>.EnumToString(document.TypeCode));
      if document.ReferenceTypeCode.HasValue then
        Writer.WriteElementString('ram:ReferenceTypeCode', TEnumExtensions<TZUGFeRDReferenceTypeCodes>.EnumToString(document.ReferenceTypeCode));

      if (document.AttachmentBinaryObject <> nil) and (document.AttachmentBinaryObject.Size > 0) then
      begin
        Writer.WriteStartElement('ram:AttachmentBinaryObject', [TZUGFeRDProfile.Comfort, TZUGFeRDProfile.Extended]);  // BT-125, BT-X-31
        Writer.WriteAttributeString('filename', document.Filename);
        Writer.WriteAttributeString('mimeCode', TZUGFeRDMimeTypeMapper.GetMimeType(document.Filename));
        Writer.WriteValue(TZUGFeRDHelper.GetDataAsBase64(document.AttachmentBinaryObject));
        Writer.WriteEndElement(); // !AttachmentBinaryObject()
      end;

      Writer.WriteEndElement(); // !ram:AdditionalReferencedDocument
    end; // !foreach(document)
  end;

  Writer.WriteEndElement(); // !ApplicableHeaderTradeAgreement

  WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.ApplicableHeaderTradeDeliveryComment);
  Writer.WriteStartElement('ram:ApplicableHeaderTradeDelivery'); // Pflichteintrag

  //RelatedSupplyChainConsignment --> SpecifiedLogisticsTransportMovement --> ModeCode // Only in extended profile
  if(Descriptor.TransportMode <> nil) then
  begin
    Writer.WriteStartElement('ram:RelatedSupplyChainConsignment', [TZUGFeRDProfile.Extended]); // BG-X-24
    Writer.WriteStartElement('ram:SpecifiedLogisticsTransportMovement', [TZUGFeRDProfile.Extended]); // BT-X-152-00
    Writer.WriteElementString('ram:ModeCode', TEnumExtensions<TZUGFeRDTransportmodeCodes>.EnumToString(Descriptor.TransportMode)); // BT-X-152
    Writer.WriteEndElement(); // !ram:SpecifiedLogisticsTransportMovement
    Writer.WriteEndElement(); // !ram:RelatedSupplyChainConsignment
  end;

if (Descriptor.Profile = TZUGFeRDProfile.Extended) then
  begin
    _writeOptionalParty(Writer, 'ram:ShipToTradeParty', Descriptor.ShipTo, Descriptor.ShipToContact);
    _writeOptionalParty(Writer, 'ram:ShipFromTradeParty', Descriptor.ShipFrom);
  end;

  if Descriptor.ActualDeliveryDate.HasValue then
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
    Writer.WriteStartElement('ram:DeliveryNoteReferencedDocument');

    if (Descriptor.DeliveryNoteReferencedDocument.IssueDateTime.HasValue) then
    begin
      Writer.WriteStartElement('ram:FormattedIssueDateTime');
      Writer.WriteValue(_formatDate(Descriptor.DeliveryNoteReferencedDocument.IssueDateTime.Value,false));
      Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
    end;

    Writer.WriteElementString('ram:IssuerAssignedID', Descriptor.DeliveryNoteReferencedDocument.ID);
    Writer.WriteEndElement(); // !DeliveryNoteReferencedDocument
  end;

  Writer.WriteEndElement(); // !ApplicableHeaderTradeDelivery

  WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.ApplicableHeaderTradeSettlementComment);
  Writer.WriteStartElement('ram:ApplicableHeaderTradeSettlement');
  // order of sub-elements of ApplicableHeaderTradeSettlement:
  //   1. CreditorReferenceID (optional)
  //   2. PaymentReference (optional)
  //   3. TaxCurrencyCode (optional)
  //   4. InvoiceCurrencyCode (optional)
  //   5. InvoiceIssuerReference (optional)
  //   6. InvoicerTradeParty (optional)
  //   7. InvoiceeTradeParty (optional)
  //   8. PayeeTradeParty (optional)
  //   9. TaxApplicableTradeCurrencyExchange (optional)
  //  10. SpecifiedTradeSettlementPaymentMeans (optional)
  //  11. ApplicableTradeTax (optional)
  //  12. BillingSpecifiedPeriod (optional)
  //  13. SpecifiedTradeAllowanceCharge (optional)
  //  14. SpecifiedLogisticsServiceCharge (optional)
  //  15. SpecifiedTradePaymentTerms (optional)
  //  16. SpecifiedTradeSettlementHeaderMonetarySummation
  //  17. InvoiceReferencedDocument (optional)
  //  18. ReceivableSpecifiedTradeAccountingAccount (optional)
  //  19. SpecifiedAdvancePayment (optional)

  //   1. CreditorReferenceID (optional)
  if (Descriptor.PaymentMeans <> nil) and (Descriptor.PaymentMeans.SEPACreditorIdentifier<>'') then
    Writer.WriteOptionalElementString('ram:CreditorReferenceID', Descriptor.PaymentMeans.SEPACreditorIdentifier, [TZUGFeRDProfile.BasicWL,TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);

  //   2. PaymentReference (optional)
  Writer.WriteOptionalElementString('ram:PaymentReference', Descriptor.PaymentReference);

  //   3. TaxCurrencyCode (optional)
  //   BT-6
	if Descriptor.TaxCurrency.HasValue then
  	Writer.WriteElementString('ram:TaxCurrencyCode', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(Descriptor.TaxCurrency), [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);

  //   4. InvoiceCurrencyCode (optional)
  Writer.WriteElementString('ram:InvoiceCurrencyCode', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(Descriptor.Currency));

  //   5. InvoiceIssuerReference (optional)
	Writer.WriteOptionalElementString('ram:InvoiceIssuerReference', Descriptor.SellerReferenceNo, [TZUGFeRDProfile.Extended]);

	//   6. InvoicerTradeParty (optional)
	_writeOptionalParty(Writer, 'ram:InvoicerTradeParty', Descriptor.Invoicer);

  //   7. InvoiceeTradeParty (optional)
  if (Descriptor.Profile = TZUGFeRDProfile.Extended) then
    _writeOptionalParty(Writer, 'ram:InvoiceeTradeParty', Descriptor.Invoicee);

  //   8. PayeeTradeParty (optional)
  if (Descriptor.Profile <> TZUGFeRDProfile.Minimum) then
  _writeOptionalParty(Writer, 'ram:PayeeTradeParty', Descriptor.Payee);

  //  10. SpecifiedTradeSettlementPaymentMeans (optional)
  if (Descriptor.CreditorBankAccounts.Count = 0) and (Descriptor.DebitorBankAccounts.Count = 0) then
  begin
    if (Descriptor.PaymentMeans <> nil) and Descriptor.PaymentMeans.TypeCode.HasValue then
    begin
      WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.SpecifiedTradeSettlementPaymentMeansComment);
      Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans');

      if (Descriptor.PaymentMeans.TypeCode.HasValue) then
      begin
        Writer.WriteElementString('ram:TypeCode', TEnumExtensions<TZUGFeRDPaymentMeansTypeCodes>.EnumToString(Descriptor.PaymentMeans.TypeCode));
        Writer.WriteOptionalElementString('ram:Information', Descriptor.PaymentMeans.Information);

        if (Descriptor.PaymentMeans.FinancialCard <> nil) then
        begin
          Writer.WriteStartElement('ram:ApplicableTradeSettlementFinancialCard', [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung]);
          Writer.WriteOptionalElementString('ram:ID', Descriptor.PaymentMeans.FinancialCard.Id);
          Writer.WriteOptionalElementString('ram:CardholderName', Descriptor.PaymentMeans.FinancialCard.CardholderName);
          Writer.WriteEndElement(); // !ram:ApplicableTradeSettlementFinancialCard
        end;
      end;
      Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
    end;
  end
  else
  begin
    for var creditorAccount : TZUGFeRDBankAccount in Descriptor.CreditorBankAccounts do
    begin
      WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.SpecifiedTradeSettlementPaymentMeansComment);
      Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans');

      if (Descriptor.PaymentMeans <> nil)
      and Descriptor.PaymentMeans.TypeCode.HasValue then
      begin
        Writer.WriteElementString('ram:TypeCode', TEnumExtensions<TZUGFeRDPaymentMeansTypeCodes>.EnumToString(Descriptor.PaymentMeans.TypeCode));
        Writer.WriteOptionalElementString('ram:Information', Descriptor.PaymentMeans.Information);

        if (Descriptor.PaymentMeans.FinancialCard <> nil) then
        begin
            Writer.WriteStartElement('ram:ApplicableTradeSettlementFinancialCard', [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung]);
            Writer.WriteOptionalElementString('ram:ID', Descriptor.PaymentMeans.FinancialCard.Id);
            Writer.WriteOptionalElementString('ram:CardholderName', Descriptor.PaymentMeans.FinancialCard.CardholderName);
            Writer.WriteEndElement(); // !ram:ApplicableTradeSettlementFinancialCard
        end;
      end;

      Writer.WriteStartElement('ram:PayeePartyCreditorFinancialAccount');
      Writer.WriteElementString('ram:IBANID', creditorAccount.IBAN);
      Writer.WriteOptionalElementString('ram:AccountName', creditorAccount.Name);
      Writer.WriteOptionalElementString('ram:ProprietaryID', creditorAccount.ID);
      Writer.WriteEndElement(); // !PayeePartyCreditorFinancialAccount

      if creditorAccount.BIC<>'' then
      begin
        Writer.WriteStartElement('ram:PayeeSpecifiedCreditorFinancialInstitution');
        Writer.WriteElementString('ram:BICID', creditorAccount.BIC);
        Writer.WriteOptionalElementString('ram:GermanBankleitzahlID', creditorAccount.Bankleitzahl);
        Writer.WriteEndElement(); // !PayeeSpecifiedCreditorFinancialInstitution
      end;

      Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
    end;

    for var debitorAccount : TZUGFeRDBankAccount in Descriptor.DebitorBankAccounts do
    begin
      Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans');

      if (Descriptor.PaymentMeans <> nil)
      and Descriptor.PaymentMeans.TypeCode.HasValue then
      begin
        Writer.WriteElementString('ram:TypeCode', TEnumExtensions<TZUGFeRDPaymentMeansTypeCodes>.EnumToString(Descriptor.PaymentMeans.TypeCode));
        Writer.WriteOptionalElementString('ram:Information', Descriptor.PaymentMeans.Information);
      end;

      Writer.WriteStartElement('ram:PayerPartyDebtorFinancialAccount');
      Writer.WriteElementString('ram:IBANID', debitorAccount.IBAN);
      Writer.WriteOptionalElementString('ram:ProprietaryID', debitorAccount.ID);
      Writer.WriteEndElement(); // !PayerPartyDebtorFinancialAccount

      Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
    end;
  end;


  //            /*
  //             * @todo add writer for this:
  //             * <SpecifiedTradeSettlementPaymentMeans>
  //            * <TypeCode>42</TypeCode>
  //          * 	<Information>Überweisung</Information>
  //           * <PayeePartyCreditorFinancialAccount>
  //          * 		<IBANID>DE08700901001234567890</IBANID>
  //          * 		<ProprietaryID>1234567890</ProprietaryID>
  //          * 	</PayeePartyCreditorFinancialAccount>
  //          * 	<PayeeSpecifiedCreditorFinancialInstitution>
  //          * 		<BICID>GENODEF1M04</BICID>
  //          * 		<GermanBankleitzahlID>70090100</GermanBankleitzahlID>
  //          * 		<Name>Hausbank München</Name>
  //          * 	</PayeeSpecifiedCreditorFinancialInstitution>
  //          * </SpecifiedTradeSettlementPaymentMeans>
  //             */  r

  //  11. ApplicableTradeTax (optional)
  _writeOptionalTaxes(Writer, options);

  //#region BillingSpecifiedPeriod
  //  12. BillingSpecifiedPeriod (optional)
  if Descriptor.BillingPeriodStart.HasValue or Descriptor.BillingPeriodEnd.HasValue then
  begin
    Writer.WriteStartElement('ram:BillingSpecifiedPeriod', [TZUGFeRDProfile.BasicWL,TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);
    if Descriptor.BillingPeriodStart.HasValue then
    begin
        Writer.WriteStartElement('ram:StartDateTime');
        _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(Descriptor.BillingPeriodStart));
        Writer.WriteEndElement(); // !StartDateTime
    end;

    if Descriptor.BillingPeriodEnd.HasValue then
    begin
        Writer.WriteStartElement('ram:EndDateTime');
        _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(Descriptor.BillingPeriodEnd));
        Writer.WriteEndElement(); // !EndDateTime
    end;
    Writer.WriteEndElement(); // !BillingSpecifiedPeriod
  end;
  //#endregion

  //  13. SpecifiedTradeAllowanceCharge (optional)
  for var tradeAllowance : TZUGFeRDTradeAllowance in Descriptor.GetTradeAllowances do
    _WriteDocumentLevelSpecifiedTradeAllowanceCharge(Writer, tradeAllowance);
  for var tradeCharge : TZUGFeRDTradeCharge in Descriptor.GetTradeCharges do
    _WriteDocumentLevelSpecifiedTradeAllowanceCharge(Writer, tradeCharge);

  //  14. SpecifiedLogisticsServiceCharge (optional)
  for var serviceCharge : TZUGFeRDServiceCharge in Descriptor.ServiceCharges do
  begin
    Writer.WriteStartElement('ram:SpecifiedLogisticsServiceCharge');
    if serviceCharge.Description <> '' then
      Writer.WriteElementString('ram:Description', serviceCharge.Description);
    Writer.WriteElementString('ram:AppliedAmount', _formatDecimal(serviceCharge.Amount));
    if serviceCharge.Tax <> nil then
    begin
      Writer.WriteStartElement('ram:AppliedTradeTax');
      Writer.WriteElementString('ram:TypeCode', TEnumExtensions<TZUGFeRDTaxTypes>.EnumToString(serviceCharge.Tax.TypeCode));
      if serviceCharge.Tax.CategoryCode.HasValue then
        Writer.WriteElementString('ram:CategoryCode', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(serviceCharge.Tax.CategoryCode));
      Writer.WriteElementString('ram:RateApplicablePercent', _formatDecimal(serviceCharge.Tax.Percent));
      Writer.WriteEndElement();
    end;
    Writer.WriteEndElement();
  end;

  //  15. SpecifiedTradePaymentTerms (optional)
  case Descriptor.Profile of
    TZUGFeRDProfile.Unknown,
    TZUGFeRDProfile.Minimum: {do nothing};

    TZUGFeRDProfile.XRechnung:
    begin
      // in XRechnung there is only one SpecifiedTradePaymentTerms allowed

      var PaymentNotes: string := '';
      var FirstDueDate: ZUGFeRDNullable<TDateTime>;

      for var PaymentTerms: TZUGFeRDPaymentTerms in Descriptor.PaymentTermsList do
      begin
        var PaymentNote: string;
        PaymentNote:= System.StrUtils.ReplaceText(Trim(PaymentTerms.Description),'#',' '); // make sure no # is present
        if PaymentNote<>'' then
          PaymentNote:= PaymentNote+#13#10;
        if PaymentTerms.PaymentTermsType.HasValue and PaymentTerms.DueDays.HasValue and PaymentTerms.Percentage.HasValue then
        begin
          var formatSettings: TFormatSettings;
          formatSettings.DecimalSeparator := '.';
          PaymentNote:= PaymentNote+
            Format('#%s#TAGE=%d#PROZENT=%.2f#%s'#13#10, [
              IfThen(PaymentTerms.PaymentTermsType = TZUGFeRDPaymentTermsType.Verzug, 'VERZUG', 'SKONTO'),
              Round(PaymentTerms.DueDays.Value),
              PaymentTerms.Percentage.Value,
              IfThen(PaymentTerms.BaseAmount.HasValue, Format('BASISBETRAG=%.2f#', [PaymentTerms.BaseAmount.Value], formatSettings),'')
            ], formatSettings);
        end;
        if PaymentNotes='' then
          PaymentNotes:= PaymentNote
        else
          PaymentNotes:= PaymentNotes + #13#10 + PaymentNote;
        // there can only be one DueDate and we use the first we find
        if PaymentTerms.DueDate.HasValue and Not(FirstDueDate.HasValue) then
          FirstDueDate:= PaymentTerms.DueDate;
      end;
      Writer.WriteStartElement('ram:SpecifiedTradePaymentTerms');
      Writer.WriteOptionalElementString('ram:Description', PaymentNotes);
      if FirstDueDate.HasValue then
      begin
        Writer.WriteStartElement('ram:DueDateDateTime');
        _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(FirstDueDate.Value));
        Writer.WriteEndElement(); // !ram:DueDateDateTime
      end;
      if (_descriptor.PaymentMeans.SEPAMandateReference<>'')
      and (_descriptor.PaymentMeans.TypeCode in [TZUGFeRDPaymentMeansTypeCodes.DirectDebit, TZUGFeRDPaymentMeansTypeCodes.SEPADirectDebit]) then // BT-89 is only required/allowed on DirectDebit (BR-DE-29)
      begin
        Writer.WriteOptionalElementString('ram:DirectDebitMandateID', _descriptor.PaymentMeans.SEPAMandateReference);
      end;

      Writer.WriteEndElement();
    end;

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
        if PaymentTerms.PaymentTermsType.HasValue then
        begin
          if PaymentTerms.PaymentTermsType = TZUGFeRDPaymentTermsType.Skonto then
            Writer.WriteStartElement('ram:ApplicableTradePaymentDiscountTerms')
          else
            Writer.WriteStartElement('ram:ApplicableTradePaymentPenaltyTerms');
          if PaymentTerms.MaturityDate.HasValue then
          begin
            Writer.WriteStartElement('ram:BasisDateTime');
            _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(PaymentTerms.MaturityDate.Value));
            Writer.WriteEndElement(); // !ram:BasisDateTime
          end;
          if paymentTerms.DueDays.HasValue then
            _writeElementWithAttribute(Writer, 'ram:BasisPeriodMeasure', 'unitCode', TEnumExtensions<TZUGFeRDQuantityCodes>.EnumToString(TZUGFeRDQuantityCodes.DAY), IntToStr(paymentTerms.DueDays.Value));
          _writeOptionalAmount(Writer, 'ram:BasisAmount', paymentTerms.BaseAmount); // forceCurrency false by default
          Writer.WriteOptionalElementString('ram:CalculationPercent', _formatDecimal(paymentTerms.Percentage));
          if PaymentTerms.PaymentTermsType = TZUGFeRDPaymentTermsType.Skonto then
            _writeOptionalAmount(Writer, 'ram:ActualDiscountAmount', paymentTerms.ActualAmount)
          else
            _writeOptionalAmount(Writer, 'ram:ActualPenaltyAmount', paymentTerms.ActualAmount);
          Writer.WriteEndElement(); // !ram:ApplicableTradePaymentDiscountTerms or  !ram:ApplicableTradePaymentPenaltyTerms
        end;
        Writer.WriteEndElement();
      end;
      if (_descriptor.PaymentTermsList.Count=0)
      and (_descriptor.PaymentMeans<>Nil) and (_descriptor.PaymentMeans.SEPAMandateReference<>'') then
      begin
        Writer.WriteStartElement('ram:SpecifiedTradePaymentTerms');
        Writer.WriteOptionalElementString('ram:DirectDebitMandateID', _descriptor.PaymentMeans.SEPAMandateReference);
        Writer.WriteEndElement();
      end;
    end;
  else // default
    if (_descriptor.PaymentTermsList.Count=0)
    and (_descriptor.PaymentMeans<>Nil) and (_descriptor.PaymentMeans.SEPAMandateReference<>'') then
    begin
      Writer.WriteStartElement('ram:SpecifiedTradePaymentTerms');
      var sbPaymentNotes: TStringBuilder := TStringBuilder.Create;
      var dueDate: ZUGFeRDNullable<TDateTime> := Nil;

      for var PaymentTerms: TZUGFeRDPaymentTerms in Descriptor.PaymentTermsList do
      begin
        if PaymentTerms.DueDate.HasValue then
        begin
          if paymentTerms.PaymentTermsType = TZUGFeRDPaymentTermsType.Skonto then
          begin
            Writer.WriteStartElement('ApplicableTradePaymentDiscountTerms');
            _writeOptionalAmount(Writer, 'BasisAmount', paymentTerms.BaseAmount);
            Writer.WriteOptionalElementString('CalculationPercent', _formatDecimal(paymentTerms.Percentage));
            Writer.WriteEndElement(); // !ram:ApplicableTradePaymentDiscountTerms
          end;
          if paymentTerms.PaymentTermsType = TZUGFeRDPaymentTermsType.Verzug then
          begin
            Writer.WriteStartElement('ApplicableTradePaymentPenaltyTerms');
            _writeOptionalAmount(Writer, 'BasisAmount', paymentTerms.BaseAmount);
            Writer.WriteOptionalElementString('CalculationPercent', _formatDecimal(paymentTerms.Percentage));
            Writer.WriteEndElement(); // !ram:ApplicableTradePaymentPenaltyTerms
          end;
        end
        else
          dueDate := IfThen(dueDate.HasValue, dueDate, paymentTerms.DueDate);
      end;
      Writer.WriteOptionalElementString('Description', Trim(sbPaymentNotes.ToString));
      if dueDate.HasValue then
      begin
        Writer.WriteStartElement('ram:DueDateDateTime');
        _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(dueDate.Value));
        Writer.WriteEndElement(); // !ram:DueDateDateTime
      end;
      Writer.WriteOptionalElementString('ram:DirectDebitMandateID', _descriptor.PaymentMeans.SEPAMandateReference);
       Writer.WriteEndElement(); // !ram:SpecifiedTradePaymentTerms
    end;
  end;

  //  16. SpecifiedTradeSettlementHeaderMonetarySummation
  WriteComment(Writer, options, TZUGFeRDInvoiceCommentConstants.SpecifiedTradeSettlementHeaderMonetarySummationComment);
  Writer.WriteStartElement('ram:SpecifiedTradeSettlementHeaderMonetarySummation');
  _writeOptionalAmount(Writer, 'ram:LineTotalAmount', Descriptor.LineTotalAmount);
  _writeOptionalAmount(Writer, 'ram:ChargeTotalAmount', Descriptor.ChargeTotalAmount);
  _writeOptionalAmount(Writer, 'ram:AllowanceTotalAmount', Descriptor.AllowanceTotalAmount);
  _writeOptionalAmount(Writer, 'ram:TaxBasisTotalAmount', Descriptor.TaxBasisAmount);
  _writeOptionalAmount(Writer, 'ram:TaxTotalAmount', Descriptor.TaxTotalAmount, 2, true);
  _writeOptionalAmount(Writer, 'ram:RoundingAmount', Descriptor.RoundingAmount, 2, false, [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);  // RoundingAmount  //Rundungsbetrag
  _writeOptionalAmount(Writer, 'ram:GrandTotalAmount', Descriptor.GrandTotalAmount);

  if Descriptor.TotalPrepaidAmount.HasValue then
    _writeOptionalAmount(Writer, 'ram:TotalPrepaidAmount', Descriptor.TotalPrepaidAmount);

  _writeOptionalAmount(Writer, 'ram:DuePayableAmount', Descriptor.DuePayableAmount);
  Writer.WriteEndElement(); // !ram:SpecifiedTradeSettlementHeaderMonetarySummation

  //  17. InvoiceReferencedDocument (optional)
  for var invoiceReferencedDocument: TZUGFeRDInvoiceReferencedDocument in Descriptor.InvoiceReferencedDocuments do
  begin
    Writer.WriteStartElement('ram:InvoiceReferencedDocument',[TZUGFeRDProfile.BasicWL,TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung1, TZUGFeRDProfile.XRechnung]);
    Writer.WriteOptionalElementString('ram:IssuerAssignedID', invoiceReferencedDocument.ID);
    if invoiceReferencedDocument.IssueDateTime.HasValue then
    begin
      Writer.WriteStartElement('ram:FormattedIssueDateTime');
      _writeElementWithAttribute(Writer, 'qdt:DateTimeString', 'format', '102', _formatDate(invoiceReferencedDocument.IssueDateTime.Value));
      Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
    end;
    Writer.WriteEndElement(); // !ram:InvoiceReferencedDocument
    break; // only one occurrence allowed in this version!
  end;

  Writer.WriteEndElement(); // !ram:ApplicableHeaderTradeSettlement

  Writer.WriteEndElement(); // !ram:SupplyChainTradeTransaction
  //#endregion

  Writer.WriteEndElement(); // !ram:Invoice
  Writer.WriteEndDocument();
  Writer.Flush();
  Writer.Free;

  _stream.Seek(streamPosition, soFromBeginning);
end;

procedure TZUGFeRDInvoiceDescriptor20Writer._writeDocumentLevelSpecifiedTradeAllowanceCharge(_writer: TZUGFeRDProfileAwareXmlTextWriter; tradeAllowanceCharge: TZUGFeRDAbstractTradeAllowanceCharge);
begin
  if tradeAllowanceCharge=nil then
    exit;

  Writer.WriteStartElement('ram:SpecifiedTradeAllowanceCharge', ALL_PROFILES-[TZUGFeRDProfile.Minimum]);
  Writer.WriteStartElement('ram:ChargeIndicator');
  Writer.WriteElementString('udt:Indicator', ifthen(tradeAllowanceCharge.ChargeIndicator,'true','false'));
  Writer.WriteEndElement(); // !ram:ChargeIndicator

  if tradeAllowanceCharge.ChargePercentage.HasValue then
  begin
    Writer.WriteStartElement('ram:CalculationPercent'); // allowance: BT-94, charge: BT-101
    Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ChargePercentage));
    Writer.WriteEndElement();
  end;

  // TODO: SequenceNumeric
  if tradeAllowanceCharge.BasisAmount.HasValue then
  begin
    Writer.WriteStartElement('ram:BasisAmount', [TZUGFeRDProfile.Extended]);
    Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.BasisAmount,4));
    Writer.WriteEndElement();
  end;

  Writer.WriteStartElement('ram:ActualAmount', [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);
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
    Writer.WriteElementString('ram:RateApplicablePercent', _formatDecimal(tradeAllowanceCharge.Tax.Percent), [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);
    Writer.WriteEndElement();
  end;
  Writer.WriteEndElement();
end;

procedure TZUGFeRDInvoiceDescriptor20Writer._writeOptionalAmount(_writer: TZUGFeRDProfileAwareXmlTextWriter; tagName: string; value: ZUGFeRDNullable<Currency>;  numDecimals: Integer; forceCurrency: Boolean; profile: TZUGFeRDProfiles);
begin
  if value.HasValue then // && (value.Value != decimal.MinValue))
  begin
    _writer.WriteStartElement(tagName,profile);
    if forceCurrency then
      _writer.WriteAttributeString('currencyID', TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(Descriptor.Currency));
    _writer.WriteValue(_formatDecimal(value.Value, numDecimals));
    _writer.WriteEndElement; // !tagName
  end;
end;

procedure TZUGFeRDInvoiceDescriptor20Writer._writeOptionalAdaptiveAmount(_writer: TZUGFeRDProfileAwareXmlTextWriter; _tagName: string;_value: ZUGFeRDNullable<Currency>; _numDecimals: Integer;  _maxnumDecimals: Integer;  _forceCurrency: boolean);
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

procedure TZUGFeRDInvoiceDescriptor20Writer._writeElementWithAttribute(_writer : TZUGFeRDProfileAwareXmlTextWriter; tagName : String; attributeName : String; attributeValue : String; nodeValue: String);
begin
  _writer.WriteStartElement(tagName);
  _writer.WriteAttributeString(attributeName, attributeValue);
  _writer.WriteValue(nodeValue);
  _writer.WriteEndElement(); // !tagName
end;

procedure TZUGFeRDInvoiceDescriptor20Writer._writeOptionalTaxes(_writer : TZUGFeRDProfileAwareXmlTextWriter; options: TZUGFeRDInvoiceFormatOptions);
begin
  for var tax : TZUGFeRDTax in Descriptor.Taxes do
  begin
    WriteComment(writer, options, TZUGFeRDInvoiceCommentConstants.ApplicableTradeTaxComment);
    _writer.WriteStartElement('ram:ApplicableTradeTax');

    _writer.WriteStartElement('ram:CalculatedAmount');
    _writer.WriteValue(_formatDecimal(tax.TaxAmount));
    _writer.WriteEndElement(); // !CalculatedAmount

    _writer.WriteElementString('ram:TypeCode', TEnumExtensions<TZUGFeRDTaxTypes>.EnumToString(tax.TypeCode));
    _writer.WriteOptionalElementString('ram:ExemptionReason', tax.ExemptionReason);
    _writer.WriteStartElement('ram:BasisAmount');
    _writer.WriteValue(_formatDecimal(tax.BasisAmount));
    _writer.WriteEndElement(); // !BasisAmount

    if tax.LineTotalBasisAmount.HasValue and (tax.LineTotalBasisAmount <> 0.0)  then
    begin
      _writer.WriteStartElement('ram:LineTotalBasisAmount', [TZUGFeRDProfile.Extended]);
      _writer.WriteValue(_formatDecimal(tax.LineTotalBasisAmount));
      _writer.WriteEndElement(); // !LineTotalBasisAmount
    end;

    if tax.AllowanceChargeBasisAmount.HasValue and (tax.AllowanceChargeBasisAmount <> 0.0) then
    begin
      _writer.WriteStartElement('ram:AllowanceChargeBasisAmount', [TZUGFeRDProfile.Extended]);
      _writer.WriteValue(_formatDecimal(tax.AllowanceChargeBasisAmount));
      _writer.WriteEndElement(); // !AllowanceChargeBasisAmount
    end;

    if tax.CategoryCode.HasValue then
      _writer.WriteElementString('ram:CategoryCode', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(tax.CategoryCode));
    if tax.ExemptionReasonCode.hasValue then
      _writer.WriteElementString('ram:ExemptionReasonCode', TEnumExtensions<TZUGFeRDTaxExemptionReasonCodes>.EnumToString(tax.ExemptionReasonCode));
    if tax.TaxPointDate.HasValue then
    begin
      _writer.WriteStartElement('ram:TaxPointDate');
      _writer.WriteStartElement('udt:DateString');
      _writer.WriteAttributeString('format', '102');
      _writer.WriteValue(_formatDate(tax.TaxPointDate.Value));
      _writer.WriteEndElement(); // !udt:DateString
      _writer.WriteEndElement(); // !TaxPointDate
    end
    else if tax.DueDateTypeCode.HasValue then
      _writer.WriteElementString('ram:DueDateTypeCode', TEnumExtensions<TZUGFeRDDateTypeCodes>.EnumToString(tax.DueDateTypeCode));

    _writer.WriteElementString('ram:RateApplicablePercent', _formatDecimal(tax.Percent));
    _writer.WriteEndElement(); // !ApplicableTradeTax
  end;
end;

procedure TZUGFeRDInvoiceDescriptor20Writer._writeNotes(_writer : TZUGFeRDProfileAwareXmlTextWriter; notes : TObjectList<TZUGFeRDNote>);
begin
  if notes.Count > 0 then
    for var note : TZUGFeRDNote in notes do
    begin
      _writer.WriteStartElement('ram:IncludedNote');
      if note.ContentCode.HasValue then
        _writer.WriteElementString('ram:ContentCode', TEnumExtensions<TZUGFeRDContentCodes>.EnumToString(note.ContentCode));
      _writer.WriteElementString('ram:Content', note.Content);
      if note.SubjectCode.HasValue then
        _writer.WriteElementString('ram:SubjectCode', TEnumExtensions<TZUGFeRDSubjectCodes>.EnumToString(note.SubjectCode));
      _writer.WriteEndElement();
    end
end;

procedure TZUGFeRDInvoiceDescriptor20Writer._writeOptionalParty(_writer: TZUGFeRDProfileAwareXmlTextWriter; PartyTag : String; Party : TZUGFeRDParty; Contact : TZUGFeRDContact = nil; TaxRegistrations : TObjectList<TZUGFeRDTaxRegistration> = nil);
begin
  if Party = nil then
    exit;
  _writer.WriteStartElement(PartyTag);

  if (Party.ID <> nil) and (Party.ID.ID <> '')  then
  begin
    if Party.ID.SchemeID.HasValue then
    begin
      _writer.WriteStartElement('ram:ID');
      _writer.WriteAttributeString('schemeID', TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.EnumToString(Party.ID.SchemeID));
      _writer.WriteValue(Party.ID.ID);
      _writer.WriteEndElement();
    end
    else
      _writer.WriteElementString('ram:ID', Party.ID.ID);
  end;

  if (Party.GlobalID <> nil) and (Party.GlobalID.ID <> '') and Party.GlobalID.SchemeID.HasValue then
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
  if Party.ContactName <> '' then
    _writer.WriteOptionalElementString('ram:LineTwo', Party.Street);
  _writer.WriteOptionalElementString('ram:LineThree', Party.AddressLine3); // BT-163
  _writer.WriteOptionalElementString('ram:CityName', Party.City);
  if party.Country.HasValue then
    writer.WriteElementString('ram:CountryID', TEnumExtensions<TZUGFeRDCountryCodes>.EnumToString(party.Country)); //buyer: BT-55
  _writer.WriteOptionalElementString('ram:CountrySubDivisionName', Party.CountrySubdivisionName); // BT-79
  _writer.WriteEndElement(); // !PostalTradeAddress

  if (TaxRegistrations <> nil) then
  begin
    for var registration : TZUGFeRDTaxRegistration in TaxRegistrations do
    if registration.No <> '' then
    begin
      _writer.WriteStartElement('ram:SpecifiedTaxRegistration');
      _writer.WriteStartElement('ram:ID');
      _writer.WriteAttributeString('schemeID', TEnumExtensions<TZUGFeRDTaxRegistrationSchemeID>.EnumToString(registration.SchemeID));
      _writer.WriteValue(registration.No);
      _writer.WriteEndElement();
      _writer.WriteEndElement();
    end;
  end;
  _writer.WriteEndElement(); // !*TradeParty
end;

procedure TZUGFeRDInvoiceDescriptor20Writer._writeOptionalContact(_writer: TZUGFeRDProfileAwareXmlTextWriter; contactTag : String;  contact : TZUGFeRDContact);
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

(*function TZUGFeRDInvoiceDescriptor20Writer._translateInvoiceType(type_ : TZUGFeRDInvoiceType) : String;
begin
  case type_ of
    SelfBilledInvoice,
    Invoice: Result := 'RECHNUNG';
    SelfBilledCreditNote,
    CreditNote: Result := 'GUTSCHRIFT';
    DebitNote: Result := 'BELASTUNGSANZEIGE';
    DebitnoteRelatedToFinancialAdjustments: Result := 'WERTBELASTUNG';
    PartialInvoice: Result := 'TEILRECHNUNG';
    PrepaymentInvoice: Result := 'VORAUSZAHLUNGSRECHNUNG';
    InvoiceInformation: Result := 'KEINERECHNUNG';
    Correction,
    CorrectionOld: Result := 'KORREKTURRECHNUNG';
    else Result := '';
  end;
end;


function TZUGFeRDInvoiceDescriptor20Writer._encodeInvoiceType(type_ : TZUGFeRDInvoiceType) : Integer;
begin
  Result:= StrToIntDef(TEnumExtensions<TZUGFeRDInvoiceTypes>.EnumToString(type_), 0);
  if Result>1000 then
    Result:= Result - 1000;
end;
*)

function TZUGFeRDInvoiceDescriptor20Writer.Validate(
  _descriptor: TZUGFeRDInvoiceDescriptor; _throwExceptions: Boolean): Boolean;
begin
  Result := false;

  //TODO in C# enthalten, aber eigentlich falsch, deswegen auskommentiert
  //if (descriptor.TZUGFeRDProfile = TZUGFeRDProfile.BasicWL) then
  //if (throwExceptions) then
  //  raise TZUGFeRDUnsupportedException.Create('Invalid TZUGFeRDProfile used for ZUGFeRD 2.0 invoice.')
  //else
  //  exit;

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

end.
