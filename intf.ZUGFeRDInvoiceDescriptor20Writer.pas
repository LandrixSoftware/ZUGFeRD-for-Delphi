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
  System.SysUtils,System.Classes,System.StrUtils,Generics.Collections
  ,intf.ZUGFeRDInvoiceDescriptor
  ,intf.ZUGFeRDProfileAwareXmlTextWriter
  ,intf.ZUGFeRDInvoiceDescriptorWriter
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
  ,intf.ZUGFeRDAllowanceOrChargeIdentificationCodes
  ,intf.ZUGFeRDFormats
  ;

type
  TZUGFeRDInvoiceDescriptor20Writer = class(TZUGFeRDInvoiceDescriptorWriter)
  private
    Writer: TZUGFeRDProfileAwareXmlTextWriter;
    Descriptor: TZUGFeRDInvoiceDescriptor;
    procedure _writeOptionalAmount(_writer : TZUGFeRDProfileAwareXmlTextWriter; tagName : string; value : ZUGFeRDNullable<Currency>; numDecimals : Integer = 2; forceCurrency : Boolean = false; profile : TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
    procedure _writeOptionalContact(_writer: TZUGFeRDProfileAwareXmlTextWriter; contactTag: String; contact: TZUGFeRDContact);
    procedure _writeOptionalParty(_writer: TZUGFeRDProfileAwareXmlTextWriter; PartyTag: String; Party: TZUGFeRDParty; Contact: TZUGFeRDContact = nil; TaxRegistrations: TObjectList<TZUGFeRDTaxRegistration> = nil);
    procedure _writeOptionalTaxes(_writer : TZUGFeRDProfileAwareXmlTextWriter);
    procedure _writeNotes(_writer : TZUGFeRDProfileAwareXmlTextWriter;notes : TObjectList<TZUGFeRDNote>);
    procedure _writeElementWithAttribute(_writer: TZUGFeRDProfileAwareXmlTextWriter; tagName, attributeName,attributeValue, nodeValue: String);
    function _translateInvoiceType(type_ : TZUGFeRDInvoiceType) : String;
    function _encodeInvoiceType(type_ : TZUGFeRDInvoiceType) : Integer;
  public
    function Validate(_descriptor: TZUGFeRDInvoiceDescriptor; _throwExceptions: Boolean = True): Boolean; override;
    /// <summary>
    /// Saves the given invoice to the given stream.
    /// Make sure that the stream is open and writeable. Otherwise, an IllegalStreamException will be thron.
    /// </summary>
    /// <param name="descriptor">The invoice object that should be saved</param>
    /// <param name="stream">The target stream for saving the invoice</param>
    /// <param name="format">Format of the target file</param>
    procedure Save(_descriptor: TZUGFeRDInvoiceDescriptor; _stream: TStream; _format : TZUGFeRDFormats = TZUGFeRDFormats.CII); override;
  end;

implementation

{ TZUGFeRDInvoiceDescriptor20Writer }

procedure TZUGFeRDInvoiceDescriptor20Writer.Save(
  _descriptor: TZUGFeRDInvoiceDescriptor; _stream: TStream;
  _format : TZUGFeRDFormats = TZUGFeRDFormats.CII);
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
  Writer := TZUGFeRDProfileAwareXmlTextWriter.Create(_stream,TEncoding.UTF8,Descriptor.Profile);
  Writer.Formatting := TZUGFeRDXmlFomatting.xmlFormatting_Indented;
  Writer.WriteStartDocument;

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
  Writer.WriteElementString('ram:ID', TZUGFeRDProfileExtensions.EnumToString(Descriptor.Profile,TZUGFeRDVersion.Version20));
  Writer.WriteEndElement(); // !ram:GuidelineSpecifiedDocumentContextParameter
  Writer.WriteEndElement(); // !rsm:ExchangedDocumentContext

  Writer.WriteStartElement('rsm:ExchangedDocument');
  Writer.WriteElementString('ram:ID', Descriptor.InvoiceNo);
  Writer.WriteElementString('ram:Name', ifthen(Descriptor.Name<>'',Descriptor.Name,_translateInvoiceType(Descriptor.Type_)), [TZUGFeRDProfile.Extended]);
  Writer.WriteElementString('ram:TypeCode', Format('%d',[_encodeInvoiceType(Descriptor.Type_)]));

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
    Writer.WriteStartElement('ram:IncludedSupplyChainTradeLineItem');

    if (tradeLineItem.AssociatedDocument <> nil) then
    if (tradeLineItem.AssociatedDocument.LineID <> '') then
    begin
      Writer.WriteStartElement('ram:AssociatedDocumentLineDocument');
      Writer.WriteElementString('ram:LineID', tradeLineItem.AssociatedDocument.LineID);
      Writer.WriteOptionalElementString('ram:LineStatusCode', tradeLineItem.AssociatedDocument.LineStatusCode);
      Writer.WriteOptionalElementString('ram:LineStatusReasonCode', tradeLineItem.AssociatedDocument.LineStatusReasonCode);
      _writeNotes(Writer, tradeLineItem.AssociatedDocument.Notes);
      Writer.WriteEndElement(); // ram:AssociatedDocumentLineDocument
    end;

    // handelt es sich um einen Kommentar?
    if tradeLineItem.AssociatedDocument <> nil then
    if ((tradeLineItem.AssociatedDocument.Notes.Count > 0) and
        (tradeLineItem.BilledQuantity = 0) and
        (tradeLineItem.Description <> '')) then
    begin
      Writer.WriteEndElement(); // !ram:IncludedSupplyChainTradeLineItem
      continue;
    end;

    Writer.WriteStartElement('ram:SpecifiedTradeProduct');
    if (tradeLineItem.GlobalID <> nil) then
    if (tradeLineItem.GlobalID.SchemeID <> TZUGFeRDGlobalIDSchemeIdentifiers.Unknown) and (tradeLineItem.GlobalID.ID <> '') then
    begin
      _writeElementWithAttribute(Writer, 'ram:GlobalID', 'schemeID', TZUGFeRDGlobalIDSchemeIdentifiersExtensions.EnumToString(tradeLineItem.GlobalID.SchemeID), tradeLineItem.GlobalID.ID);
    end;

    Writer.WriteOptionalElementString('ram:SellerAssignedID', tradeLineItem.SellerAssignedID);
    Writer.WriteOptionalElementString('ram:BuyerAssignedID', tradeLineItem.BuyerAssignedID);
    Writer.WriteOptionalElementString('ram:Name', tradeLineItem.Name);
    Writer.WriteOptionalElementString('ram:Description', tradeLineItem.Description);

    if (tradeLineItem.ApplicableProductCharacteristics <> nil) then
    if (tradeLineItem.ApplicableProductCharacteristics.Count > 0) then
    begin
      for var productCharacteristic : TZUGFeRDApplicableProductCharacteristic in tradeLineItem.ApplicableProductCharacteristics do
      begin
        Writer.WriteStartElement('ram:ApplicableProductCharacteristic');
        Writer.WriteOptionalElementString('ram:Description', productCharacteristic.Description);
        Writer.WriteOptionalElementString('ram:Value', productCharacteristic.Value);
        Writer.WriteEndElement(); // !ram:ApplicableProductCharacteristic
      end
    end;

    Writer.WriteEndElement(); // !ram:SpecifiedTradeProduct

    Writer.WriteStartElement('ram:SpecifiedLineTradeAgreement', [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);

    if (tradeLineItem.BuyerOrderReferencedDocument <> nil) then
    begin
      Writer.WriteStartElement('ram:BuyerOrderReferencedDocument', [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);

      //#region IssuerAssignedID
      //Bestellnummer
      Writer.WriteOptionalElementString('ram:IssuerAssignedID', tradeLineItem.BuyerOrderReferencedDocument.ID);
      //#endregion

      //#region LineID
      //Referenz zur Bestellposition
      Writer.WriteOptionalElementString('ram:LineID', tradeLineItem.BuyerOrderReferencedDocument.LineID);
      //#endregion

      //#region IssueDateTime
      if (tradeLineItem.BuyerOrderReferencedDocument.IssueDateTime.HasValue) then
      begin
        Writer.WriteStartElement('ram:FormattedIssueDateTime');
        Writer.WriteStartElement('qdt:DateTimeString');
        Writer.WriteAttributeString('format', '102');
        Writer.WriteValue(_formatDate(tradeLineItem.BuyerOrderReferencedDocument.IssueDateTime.Value));
        Writer.WriteEndElement(); // !qdt:DateTimeString
        Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
      end;
      //#endregion

      Writer.WriteEndElement(); // !ram:BuyerOrderReferencedDocument
    end;

    if (tradeLineItem.ContractReferencedDocument <> nil) then
    begin
      Writer.WriteStartElement('ram:ContractReferencedDocument', [TZUGFeRDProfile.Extended]);

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
      Writer.WriteOptionalElementString('ram:IssuerAssignedID', tradeLineItem.ContractReferencedDocument.ID);
      Writer.WriteEndElement(); // !ram:ContractReferencedDocument(Extended)
    end;

    for var document : TZUGFeRDAdditionalReferencedDocument in tradeLineItem.AdditionalReferencedDocuments do
    begin
      Writer.WriteStartElement('ram:AdditionalReferencedDocument', [TZUGFeRDProfile.Extended]);
      if (document.IssueDateTime.HasValue) then
      begin
          Writer.WriteStartElement('ram:FormattedIssueDateTime');
          Writer.WriteStartElement('qdt:DateTimeString');
          Writer.WriteAttributeString('format', '102');
          Writer.WriteValue(_formatDate(document.IssueDateTime.Value));
          Writer.WriteEndElement(); // !udt:DateTimeString
          Writer.WriteEndElement(); // !ram:IssueDateTime
      end;

      Writer.WriteElementString('ram:LineID', Format('%d',[tradeLineItem.AssociatedDocument.LineID]));
      Writer.WriteOptionalElementString('ram:IssuerAssignedID', document.ID);
      Writer.WriteElementString('ram:ReferenceTypeCode', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(document.ReferenceTypeCode));

      Writer.WriteEndElement(); // !ram:AdditionalReferencedDocument
    end; // !foreach(document)

    Writer.WriteStartElement('ram:GrossPriceProductTradePrice');
    _writeOptionalAmount(Writer, 'ram:ChargeAmount', tradeLineItem.GrossUnitPrice, 4, false);
    if (tradeLineItem.UnitQuantity.HasValue) then
    begin
      _writeElementWithAttribute(Writer, 'ram:BasisQuantity', 'unitCode', TZUGFeRDQuantityCodesExtensions.EnumToString(tradeLineItem.UnitCode), _formatDecimal(tradeLineItem.UnitQuantity.Value, 4));
    end;

    for var tradeAllowanceCharge : TZUGFeRDTradeAllowanceCharge in tradeLineItem.TradeAllowanceCharges do
    begin
      Writer.WriteStartElement('ram:AppliedTradeAllowanceCharge');

      //#region ChargeIndicator
      Writer.WriteStartElement('ram:ChargeIndicator');
      Writer.WriteElementString('udt:Indicator', ifthen(tradeAllowanceCharge.ChargeIndicator,'true','false'));
      Writer.WriteEndElement(); // !ram:ChargeIndicator
      //#endregion

      //#region BasisAmount
      Writer.WriteStartElement('ram:BasisAmount');
      Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.BasisAmount, 2));
      Writer.WriteEndElement();
      //#endregion

      //#region ActualAmount
      Writer.WriteStartElement('ram:ActualAmount');
      Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ActualAmount, 2));
      Writer.WriteEndElement();
      //#endregion

      if tradeAllowanceCharge.ChargeIndicator then
      begin
        Writer.WriteOptionalElementString('ram:ReasonCode',
           TZUGFeRDSpecialServiceDescriptionCodesExtensions.EnumToString(
                                     tradeAllowanceCharge.ReasonCodeCharge));
      end else
      begin
        Writer.WriteOptionalElementString('ram:ReasonCode',
           TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(
                                     tradeAllowanceCharge.ReasonCodeAllowance));
      end;

      Writer.WriteOptionalElementString('ram:Reason', tradeAllowanceCharge.Reason);

      Writer.WriteEndElement(); // !AppliedTradeAllowanceCharge
    end;

    Writer.WriteEndElement(); // ram:GrossPriceProductTradePrice

    Writer.WriteStartElement('ram:NetPriceProductTradePrice');
    _writeOptionalAmount(Writer, 'ram:ChargeAmount', tradeLineItem.NetUnitPrice, 4, false);

    if (tradeLineItem.UnitQuantity.HasValue) then
    begin
      _writeElementWithAttribute(Writer, 'ram:BasisQuantity', 'unitCode', TZUGFeRDQuantityCodesExtensions.EnumToString(tradeLineItem.UnitCode), _formatDecimal(tradeLineItem.UnitQuantity.Value, 4));
    end;
    Writer.WriteEndElement(); // ram:NetPriceProductTradePrice

    Writer.WriteEndElement(); // !ram:SpecifiedLineTradeAgreement

    if (Descriptor.Profile <> TZUGFeRDProfile.Basic) then
    begin
      Writer.WriteStartElement('ram:SpecifiedLineTradeDelivery');
      _writeElementWithAttribute(Writer, 'ram:BilledQuantity', 'unitCode', TZUGFeRDQuantityCodesExtensions.EnumToString(tradeLineItem.UnitCode), _formatDecimal(tradeLineItem.BilledQuantity, 4));
      if tradeLineItem.PackageQuantity.HasValue then
        _writeElementWithAttribute(Writer, 'ram:PackageQuantity', 'unitCode', TZUGFeRDQuantityCodesExtensions.EnumToString(tradeLineItem.PackageUnitCode), _formatDecimal(tradeLineItem.PackageQuantity, 4));
      if tradeLineItem.ChargeFreeQuantity.HasValue then
        _writeElementWithAttribute(Writer, 'ram:ChargeFreeQuantity', 'unitCode', TZUGFeRDQuantityCodesExtensions.EnumToString(tradeLineItem.ChargeFreeUnitCode), _formatDecimal(tradeLineItem.ChargeFreeQuantity, 4));

      if (tradeLineItem.DeliveryNoteReferencedDocument <> nil) then
      begin
        Writer.WriteStartElement('ram:DeliveryNoteReferencedDocument');

        // reference to the delivery note item
        Writer.WriteOptionalElementString('ram:LineID', tradeLineItem.DeliveryNoteReferencedDocument.LineID);

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
      _writeElementWithAttribute(Writer, 'ram:BilledQuantity', 'unitCode', TZUGFeRDQuantityCodesExtensions.EnumToString(tradeLineItem.UnitCode), _formatDecimal(tradeLineItem.BilledQuantity, 4));
      Writer.WriteEndElement(); // !ram:SpecifiedLineTradeDelivery
    end;

    Writer.WriteStartElement('ram:SpecifiedLineTradeSettlement');

    Writer.WriteStartElement('ram:ApplicableTradeTax', [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);
    Writer.WriteElementString('ram:TypeCode', TZUGFeRDTaxTypesExtensions.EnumToString(tradeLineItem.TaxType));
    Writer.WriteElementString('ram:CategoryCode', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(tradeLineItem.TaxCategoryCode)); // BT-151
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

    Writer.WriteStartElement('ram:SpecifiedTradeSettlementLineMonetarySummation');

    var _total : double := 0;

    if (tradeLineItem.LineTotalAmount.HasValue) then
    begin
      _total := tradeLineItem.LineTotalAmount.Value;
    end
    else if (tradeLineItem.NetUnitPrice.HasValue) then
    begin
      _total := tradeLineItem.NetUnitPrice.Value * tradeLineItem.BilledQuantity;
      if tradeLineItem.UnitQuantity.HasValue then
      if (tradeLineItem.UnitQuantity.Value <> 0) then
      begin
        _total := _total / tradeLineItem.UnitQuantity.Value;
      end;
    end;

    Writer.WriteElementString('ram:LineTotalAmount', _formatDecimal(_total));

    Writer.WriteEndElement(); // ram:SpecifiedTradeSettlementLineMonetarySummation
    Writer.WriteEndElement(); // !ram:SpecifiedLineTradeSettlement

    Writer.WriteEndElement(); // !ram:IncludedSupplyChainTradeLineItem
  end; // !foreach(tradeLineItem)

  Writer.WriteStartElement('ram:ApplicableHeaderTradeAgreement');
  Writer.WriteOptionalElementString('ram:BuyerReference', Descriptor.ReferenceOrderNo);
  _writeOptionalParty(Writer, 'ram:SellerTradeParty', Descriptor.Seller, Descriptor.SellerContact, Descriptor.SellerTaxRegistration);
  _writeOptionalParty(Writer, 'ram:BuyerTradeParty', Descriptor.Buyer, Descriptor.BuyerContact, Descriptor.BuyerTaxRegistration);

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

      if (document.IssueDateTime.HasValue) then
      begin
        Writer.WriteStartElement('ram:FormattedIssueDateTime');
        Writer.WriteStartElement('qdt:DateTimeString');
        Writer.WriteValue(_formatDate(document.IssueDateTime.Value));
        Writer.WriteEndElement(); // !udt:DateTimeString
        Writer.WriteEndElement(); // !FormattedIssueDateTime
      end;

      if (document.ReferenceTypeCode <> TZUGFeRDReferenceTypeCodes.Unknown) then
      begin
        Writer.WriteElementString('ram:TypeCode', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(document.ReferenceTypeCode));
      end;

      Writer.WriteElementString('ram:ID', document.ID);
      Writer.WriteEndElement(); // !ram:AdditionalReferencedDocument
    end; // !foreach(document)
  end;

  Writer.WriteEndElement(); // !ApplicableHeaderTradeAgreement

  Writer.WriteStartElement('ram:ApplicableHeaderTradeDelivery'); // Pflichteintrag

  if (Descriptor.Profile = TZUGFeRDProfile.Extended) then
  begin
    _writeOptionalParty(Writer, 'ram:ShipToTradeParty', Descriptor.ShipTo);
    _writeOptionalParty(Writer, 'ram:ShipFromTradeParty', Descriptor.ShipFrom);
  end;

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
  if Descriptor.PaymentMeans <> nil then
  if (Descriptor.PaymentMeans.SEPACreditorIdentifier<>'') then
  begin
    Writer.WriteOptionalElementString('ram:CreditorReferenceID', Descriptor.PaymentMeans.SEPACreditorIdentifier, [TZUGFeRDProfile.BasicWL,TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);
  end;

  //   2. PaymentReference (optional)
  Writer.WriteOptionalElementString('ram:PaymentReference', Descriptor.PaymentReference);

  //   3. TaxCurrencyCode (optional)
  //   BT-6
	if (Descriptor.TaxCurrency <> TZUGFeRDCurrencyCodes.Unknown) then
  	Writer.WriteElementString('ram:TaxCurrencyCode', TZUGFeRDCurrencyCodesExtensions.EnumToString(Descriptor.TaxCurrency), [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);

  //   4. InvoiceCurrencyCode (optional)
  Writer.WriteElementString('ram:InvoiceCurrencyCode', TZUGFeRDCurrencyCodesExtensions.EnumToString(Descriptor.Currency));

  //   5. InvoiceIssuerReference (optional)
	Writer.WriteElementString('ram:InvoiceIssuerReference', Descriptor.SellerReferenceNo, [TZUGFeRDProfile.Extended]);

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
    if (Descriptor.PaymentMeans <> nil) then
    if (Descriptor.PaymentMeans.TypeCode <> TZUGFeRDPaymentMeansTypeCodes.Unknown) then
    begin
      Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans');
      Writer.WriteElementString('ram:TypeCode', TZUGFeRDPaymentMeansTypeCodesExtensions.EnumToString(Descriptor.PaymentMeans.TypeCode));
      Writer.WriteOptionalElementString('ram:Information', Descriptor.PaymentMeans.Information);

      if (Descriptor.PaymentMeans.FinancialCard <> nil) then
      begin
        Writer.WriteStartElement('ram:ApplicableTradeSettlementFinancialCard', [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung]);
        Writer.WriteOptionalElementString('ram:ID', Descriptor.PaymentMeans.FinancialCard.Id);
        Writer.WriteOptionalElementString('ram:CardholderName', Descriptor.PaymentMeans.FinancialCard.CardholderName);
        Writer.WriteEndElement(); // !ram:ApplicableTradeSettlementFinancialCard
      end;
      Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
    end;
  end
  else
  begin
    for var creditorAccount : TZUGFeRDBankAccount in Descriptor.CreditorBankAccounts do
    begin
      Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans');

      if (Descriptor.PaymentMeans <> nil) then
      if (Descriptor.PaymentMeans.TypeCode <> TZUGFeRDPaymentMeansTypeCodes.Unknown) then
      begin
        Writer.WriteElementString('ram:TypeCode', TZUGFeRDPaymentMeansTypeCodesExtensions.EnumToString(Descriptor.PaymentMeans.TypeCode));
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

      //if (creditorAccount.BIC<>'') then
      //begin
        Writer.WriteStartElement('ram:PayeeSpecifiedCreditorFinancialInstitution');
        Writer.WriteElementString('ram:BICID', creditorAccount.BIC);
        Writer.WriteOptionalElementString('ram:GermanBankleitzahlID', creditorAccount.Bankleitzahl);
        Writer.WriteEndElement(); // !PayeeSpecifiedCreditorFinancialInstitution
      //end;

      Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
    end;

    for var debitorAccount : TZUGFeRDBankAccount in Descriptor.DebitorBankAccounts do
    begin
      Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans');

      if (Descriptor.PaymentMeans <> nil) then
      if (Descriptor.PaymentMeans.TypeCode <> TZUGFeRDPaymentMeansTypeCodes.Unknown) then
      begin
        Writer.WriteElementString('ram:TypeCode', TZUGFeRDPaymentMeansTypeCodesExtensions.EnumToString(Descriptor.PaymentMeans.TypeCode));
        Writer.WriteOptionalElementString('ram:Information', Descriptor.PaymentMeans.Information);
      end;

      Writer.WriteStartElement('ram:PayerPartyDebtorFinancialAccount');
      Writer.WriteElementString('ram:IBANID', debitorAccount.IBAN);
      Writer.WriteOptionalElementString('ram:ProprietaryID', debitorAccount.ID);
      Writer.WriteEndElement(); // !PayerPartyDebtorFinancialAccount

      if (debitorAccount.BIC<>'') or
         (debitorAccount.Bankleitzahl<>'') or
         (debitorAccount.BankName<>'') then
      begin
        Writer.WriteStartElement('ram:PayerSpecifiedDebtorFinancialInstitution');
        Writer.WriteElementString('ram:BICID', debitorAccount.BIC);
        Writer.WriteOptionalElementString('ram:GermanBankleitzahlID', debitorAccount.Bankleitzahl);
        Writer.WriteOptionalElementString('ram:Name', debitorAccount.BankName);
        Writer.WriteEndElement(); // !PayerSpecifiedDebtorFinancialInstitution
      end;

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
  _writeOptionalTaxes(Writer);

  //#region BillingSpecifiedPeriod
  //  12. BillingSpecifiedPeriod (optional)
  if (Descriptor.BillingPeriodStart.HasValue) or (Descriptor.BillingPeriodEnd.HasValue) then
  begin
    Writer.WriteStartElement('ram:BillingSpecifiedPeriod', [TZUGFeRDProfile.BasicWL,TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);
    if (Descriptor.BillingPeriodStart.HasValue) then
    begin
        Writer.WriteStartElement('ram:StartDateTime');
        _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(Descriptor.BillingPeriodStart));
        Writer.WriteEndElement(); // !StartDateTime
    end;

    if (Descriptor.BillingPeriodEnd.HasValue) then
    begin
        Writer.WriteStartElement('ram:EndDateTime');
        _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(Descriptor.BillingPeriodEnd));
        Writer.WriteEndElement(); // !EndDateTime
    end;
    Writer.WriteEndElement(); // !BillingSpecifiedPeriod
  end;
  //#endregion

  //  13. SpecifiedTradeAllowanceCharge (optional)
  for var tradeAllowanceCharge : TZUGFeRDTradeAllowanceCharge in Descriptor.TradeAllowanceCharges do
  begin
    Writer.WriteStartElement('ram:SpecifiedTradeAllowanceCharge');
    Writer.WriteStartElement('ram:ChargeIndicator');
    Writer.WriteElementString('udt:Indicator', ifthen(tradeAllowanceCharge.ChargeIndicator,'true','false'));
    Writer.WriteEndElement(); // !ram:ChargeIndicator

    if tradeAllowanceCharge.BasisAmount <> 0.0 then
    begin
      Writer.WriteStartElement('ram:BasisAmount');
      Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.BasisAmount));
      Writer.WriteEndElement();
    end;

    Writer.WriteStartElement('ram:ActualAmount');
    Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ActualAmount));
    Writer.WriteEndElement();

    if tradeAllowanceCharge.ChargeIndicator then
    begin
      Writer.WriteOptionalElementString('ram:ReasonCode',
         TZUGFeRDSpecialServiceDescriptionCodesExtensions.EnumToString(
                                   tradeAllowanceCharge.ReasonCodeCharge));
    end else
    begin
      Writer.WriteOptionalElementString('ram:ReasonCode',
         TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(
                                   tradeAllowanceCharge.ReasonCodeAllowance));
    end;

    Writer.WriteOptionalElementString('ram:Reason', tradeAllowanceCharge.Reason);

    if (tradeAllowanceCharge.Tax <> nil) then
    begin
      Writer.WriteStartElement('ram:CategoryTradeTax');
      Writer.WriteElementString('ram:TypeCode', TZUGFeRDTaxTypesExtensions.EnumToString(tradeAllowanceCharge.Tax.TypeCode));
      if (tradeAllowanceCharge.Tax.CategoryCode <> TZUGFeRDTaxCategoryCodes.Unknown) then
        Writer.WriteElementString('ram:CategoryCode', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(tradeAllowanceCharge.Tax.CategoryCode));
      Writer.WriteElementString('ram:RateApplicablePercent', _formatDecimal(tradeAllowanceCharge.Tax.Percent));
      Writer.WriteEndElement();
    end;
    Writer.WriteEndElement();
  end;

  //  14. SpecifiedLogisticsServiceCharge (optional)
  for var serviceCharge : TZUGFeRDServiceCharge in Descriptor.ServiceCharges do
  begin
    Writer.WriteStartElement('ram:SpecifiedLogisticsServiceCharge');
    if serviceCharge.Description <> '' then
      Writer.WriteElementString('ram:Description', serviceCharge.Description);
    Writer.WriteElementString('ram:AppliedAmount', _formatDecimal(serviceCharge.Amount));
    if (serviceCharge.Tax <> nil) then
    begin
      Writer.WriteStartElement('ram:AppliedTradeTax');
      Writer.WriteElementString('ram:TypeCode', TZUGFeRDTaxTypesExtensions.EnumToString(serviceCharge.Tax.TypeCode));
      if (serviceCharge.Tax.CategoryCode <> TZUGFeRDTaxCategoryCodes.Unknown) then
        Writer.WriteElementString('ram:CategoryCode', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(serviceCharge.Tax.CategoryCode));
      Writer.WriteElementString('ram:RateApplicablePercent', _formatDecimal(serviceCharge.Tax.Percent));
      Writer.WriteEndElement();
    end;
    Writer.WriteEndElement();
  end;

  //  15. SpecifiedTradePaymentTerms (optional)
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
    Writer.WriteOptionalElementString('ram:DirectDebitMandateID', PaymentTerms.DirectDebitMandateID);
    //TODO PaymentTerms.PartialPaymentAmount
    //TODO PaymentTerms.ApplicableTradePaymentPenaltyTerms
    if (PaymentTerms.ApplicableTradePaymentDiscountTerms.BasisAmount <> 0.0) or
       (PaymentTerms.ApplicableTradePaymentDiscountTerms.CalculationPercent <> 0.0) or
       PaymentTerms.ApplicableTradePaymentDiscountTerms.BasisPeriodMeasure.HasValue then
    begin
      Writer.WriteStartElement('ram:ApplicableTradePaymentDiscountTerms');
      if PaymentTerms.ApplicableTradePaymentDiscountTerms.BasisPeriodMeasure.HasValue then
        _writeElementWithAttribute(Writer, 'ram:BasisPeriodMeasure', 'unitCode', TZUGFeRDQuantityCodesExtensions.EnumToString(PaymentTerms.ApplicableTradePaymentDiscountTerms.UnitCode), _formatDecimal(paymentTerms.ApplicableTradePaymentDiscountTerms.BasisPeriodMeasure, 4));
      if PaymentTerms.ApplicableTradePaymentDiscountTerms.BasisAmount <> 0.0 then
        _writeOptionalAmount(Writer, 'ram:BasisAmount', PaymentTerms.ApplicableTradePaymentDiscountTerms.BasisAmount);
      if PaymentTerms.ApplicableTradePaymentDiscountTerms.CalculationPercent <> 0.0 then
        _writeOptionalAmount(Writer, 'ram:CalculationPercent', PaymentTerms.ApplicableTradePaymentDiscountTerms.CalculationPercent,4);
      Writer.WriteEndElement();
      //TODO PaymentTerms.ApplicableTradePaymentDiscountTerms.ActualPenaltyAmount
    end;
    Writer.WriteEndElement();
  end;

  //  16. SpecifiedTradeSettlementHeaderMonetarySummation
  Writer.WriteStartElement('ram:SpecifiedTradeSettlementHeaderMonetarySummation');
  _writeOptionalAmount(Writer, 'ram:LineTotalAmount', Descriptor.LineTotalAmount);
  _writeOptionalAmount(Writer, 'ram:ChargeTotalAmount', Descriptor.ChargeTotalAmount);
  _writeOptionalAmount(Writer, 'ram:AllowanceTotalAmount', Descriptor.AllowanceTotalAmount);
  _writeOptionalAmount(Writer, 'ram:TaxBasisTotalAmount', Descriptor.TaxBasisAmount);
  _writeOptionalAmount(Writer, 'ram:TaxTotalAmount', Descriptor.TaxTotalAmount, 2, true);
  _writeOptionalAmount(Writer, 'ram:RoundingAmount', Descriptor.RoundingAmount, 2, false, [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);  // RoundingAmount  //Rundungsbetrag
  _writeOptionalAmount(Writer, 'ram:GrandTotalAmount', Descriptor.GrandTotalAmount);

  if (Descriptor.TotalPrepaidAmount.HasValue) then
  begin
    _writeOptionalAmount(Writer, 'ram:TotalPrepaidAmount', Descriptor.TotalPrepaidAmount);
  end;

  _writeOptionalAmount(Writer, 'ram:DuePayableAmount', Descriptor.DuePayableAmount);
  Writer.WriteEndElement(); // !ram:SpecifiedTradeSettlementHeaderMonetarySummation

  //  17. InvoiceReferencedDocument (optional)
  for var i : Integer := 0 to Descriptor.InvoiceReferencedDocuments.Count-1 do
  begin
    Writer.WriteStartElement('ram:InvoiceReferencedDocument',[TZUGFeRDProfile.BasicWL,TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung1, TZUGFeRDProfile.XRechnung]);
    Writer.WriteOptionalElementString('ram:IssuerAssignedID', Descriptor.InvoiceReferencedDocuments[i].ID);
    if (Descriptor.InvoiceReferencedDocuments[i].IssueDateTime.HasValue) then
    begin
      Writer.WriteStartElement('ram:FormattedIssueDateTime');
      _writeElementWithAttribute(Writer, 'qdt:DateTimeString', 'format', '102', _formatDate(Descriptor.InvoiceReferencedDocuments[i].IssueDateTime.Value));
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

procedure TZUGFeRDInvoiceDescriptor20Writer._writeElementWithAttribute(
  _writer : TZUGFeRDProfileAwareXmlTextWriter;
  tagName : String; attributeName : String;
  attributeValue : String; nodeValue: String);
begin
  _writer.WriteStartElement(tagName);
  _writer.WriteAttributeString(attributeName, attributeValue);
  _writer.WriteValue(nodeValue);
  _writer.WriteEndElement(); // !tagName
end;

procedure TZUGFeRDInvoiceDescriptor20Writer._writeOptionalTaxes(
  _writer : TZUGFeRDProfileAwareXmlTextWriter);
begin
  for var tax : TZUGFeRDTax in Descriptor.Taxes do
  begin
    _writer.WriteStartElement('ram:ApplicableTradeTax');

    _writer.WriteStartElement('ram:CalculatedAmount');
    _writer.WriteValue(_formatDecimal(tax.TaxAmount));
    _writer.WriteEndElement(); // !CalculatedAmount

    _writer.WriteElementString('ram:TypeCode', TZUGFeRDTaxTypesExtensions.EnumToString(tax.TypeCode));
    _writer.WriteOptionalElementString('ram:ExemptionReason', tax.ExemptionReason);
//
    _writer.WriteStartElement('ram:BasisAmount');
    _writer.WriteValue(_formatDecimal(tax.BasisAmount));
    _writer.WriteEndElement(); // !BasisAmount

    if (tax.AllowanceChargeBasisAmount <> 0.0) then
    begin
      _writer.WriteStartElement('ram:AllowanceChargeBasisAmount');
      _writer.WriteValue(_formatDecimal(tax.AllowanceChargeBasisAmount));
      _writer.WriteEndElement(); // !AllowanceChargeBasisAmount
    end;

    if (tax.CategoryCode <> TZUGFeRDTaxCategoryCodes.Unknown) then
    begin
      _writer.WriteElementString('ram:CategoryCode', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(tax.CategoryCode));
    end;
    if (tax.ExemptionReasonCode <> TZUGFeRDTaxExemptionReasonCodes.Unknown) then
    begin
      _writer.WriteElementString('ram:ExemptionReasonCode', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(tax.ExemptionReasonCode));
    end;
    _writer.WriteElementString('ram:RateApplicablePercent', _formatDecimal(tax.Percent));
    _writer.WriteEndElement(); // !ApplicableTradeTax
  end;
end;

procedure TZUGFeRDInvoiceDescriptor20Writer._writeNotes(
  _writer : TZUGFeRDProfileAwareXmlTextWriter;
  notes : TObjectList<TZUGFeRDNote>);
begin
  if notes.Count = 0 then
    exit;

  for var note : TZUGFeRDNote in notes do
  begin
    _writer.WriteStartElement('ram:IncludedNote');
    if (note.ContentCode <> TZUGFeRDContentCodes.Unknown) then
      _writer.WriteElementString('ram:ContentCode', TZUGFeRDContentCodesExtensions.EnumToString(note.ContentCode));
    _writer.WriteElementString('ram:Content', note.Content);
    if (note.SubjectCode <> TZUGFeRDSubjectCodes.Unknown) then
      _writer.WriteElementString('ram:SubjectCode', TZUGFeRDSubjectCodesExtensions.EnumToString(note.SubjectCode));
    _writer.WriteEndElement();
  end;
end;

procedure TZUGFeRDInvoiceDescriptor20Writer._writeOptionalParty(
  _writer: TZUGFeRDProfileAwareXmlTextWriter;
  PartyTag : String; Party : TZUGFeRDParty; Contact : TZUGFeRDContact = nil;
  TaxRegistrations : TObjectList<TZUGFeRDTaxRegistration> = nil);
begin
  if Party = nil then
    exit;
  _writer.WriteStartElement(PartyTag);

  if (Party.ID <> nil) then
  if (Party.ID.ID <> '')  then
  begin
    if (Party.ID.SchemeID <> TZUGFeRDGlobalIDSchemeIdentifiers.Unknown) then
    begin
      _writer.WriteStartElement('ram:ID');
      _writer.WriteAttributeString('schemeID', TZUGFeRDGlobalIDSchemeIdentifiersExtensions.EnumToString(Party.ID.SchemeID));
      _writer.WriteValue(Party.ID.ID);
      _writer.WriteEndElement();
    end
    else
    begin
      _writer.WriteElementString('ram:ID', Party.ID.ID);
    end;
  end;

  if (Party.GlobalID <> nil) then
  if (Party.GlobalID.ID <> '') and (Party.GlobalID.SchemeID <> TZUGFeRDGlobalIDSchemeIdentifiers.Unknown) then
  begin
    _writer.WriteStartElement('ram:GlobalID');
    _writer.WriteAttributeString('schemeID', TZUGFeRDGlobalIDSchemeIdentifiersExtensions.EnumToString(Party.GlobalID.SchemeID));
    _writer.WriteValue(Party.GlobalID.ID);
    _writer.WriteEndElement();
  end;

  _Writer.WriteOptionalElementString('ram:Name', Party.Name);
  _Writer.WriteOptionalElementString('ram:Description', Party.Description, [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung1, TZUGFeRDProfile.XRechnung]);
  _writeOptionalContact(_writer, 'ram:DefinedTradeContact', Contact);
  _writer.WriteStartElement('ram:PostalTradeAddress');
  _writer.WriteOptionalElementString('ram:PostcodeCode', Party.Postcode);
  _writer.WriteOptionalElementString('ram:LineOne', ifthen(Party.ContactName = '', Party.Street,Party.ContactName));
  if (Party.ContactName <> '') then
  begin
    _writer.WriteOptionalElementString('ram:LineTwo', Party.Street);
  end;
  _writer.WriteOptionalElementString('ram:LineThree', Party.AddressLine3); // BT-163
  _writer.WriteOptionalElementString('ram:CityName', Party.City);
  _writer.WriteElementString('ram:CountryID', TZUGFeRDCountryCodesExtensions.EnumToString(Party.Country));
  _writer.WriteOptionalElementString('ram:CountrySubDivisionName', Party.CountrySubdivisionName); // BT-79
  _writer.WriteEndElement(); // !PostalTradeAddress

  if (TaxRegistrations <> nil) then
  begin
    for var _reg : TZUGFeRDTaxRegistration in TaxRegistrations do
    if (_reg.No <> '') then
    begin
      _writer.WriteStartElement('ram:SpecifiedTaxRegistration');
      _writer.WriteStartElement('ram:ID');
      _writer.WriteAttributeString('schemeID', TZUGFeRDTaxRegistrationSchemeIDExtensions.EnumToString(_reg.SchemeID));
      _writer.WriteValue(_reg.No);
      _writer.WriteEndElement();
      _writer.WriteEndElement();
    end;
  end;
  _writer.WriteEndElement(); // !*TradeParty
end;

procedure TZUGFeRDInvoiceDescriptor20Writer._writeOptionalContact(
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

function TZUGFeRDInvoiceDescriptor20Writer._translateInvoiceType(type_ : TZUGFeRDInvoiceType) : String;
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
  if (Integer(type_) > 1000) then
    type_ := TZUGFeRDInvoiceType(Integer(type_)-1000);

  case type_ of
    TZUGFeRDInvoiceType.CorrectionOld: Result := Integer(TZUGFeRDInvoiceType.Correction);
    else Result := Integer(type_);
  end;
end;

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

procedure TZUGFeRDInvoiceDescriptor20Writer._writeOptionalAmount(
  _writer: TZUGFeRDProfileAwareXmlTextWriter; tagName: string;
  value: ZUGFeRDNullable<Currency>;
  numDecimals: Integer; forceCurrency: Boolean; profile: TZUGFeRDProfiles);
begin
  if (value.HasValue) then // && (value.Value != decimal.MinValue))
  begin
    _writer.WriteStartElement(tagName,profile);
    if forceCurrency then
      _writer.WriteAttributeString('currencyID', TZUGFeRDCurrencyCodesExtensions.EnumToString(Descriptor.Currency));
    _writer.WriteValue(_formatDecimal(value.Value, numDecimals));
    _writer.WriteEndElement; // !tagName
  end;
end;

end.
