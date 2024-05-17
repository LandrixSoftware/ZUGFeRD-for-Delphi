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
  ;

type
  TZUGFeRDInvoiceDescriptor20Writer = class(TZUGFeRDInvoiceDescriptorWriter)
  private
    Writer: TZUGFeRDProfileAwareXmlTextWriter;
    FDescriptor: TZUGFeRDInvoiceDescriptor;
    procedure _writeOptionalAmount(_writer : TZUGFeRDProfileAwareXmlTextWriter; tagName : string; value : TZUGFeRDNullable<Currency>; numDecimals : Integer = 2; forceCurrency : Boolean = false; profile : TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
  public
    function Validate(descriptor: TZUGFeRDInvoiceDescriptor; throwExceptions: Boolean = True): Boolean; override;
    /// <summary>
    /// Saves the given invoice to the given stream.
    /// Make sure that the stream is open and writeable. Otherwise, an IllegalStreamException will be thron.
    /// </summary>
    /// <param name="descriptor"></param>
    /// <param name="stream"></param>
    procedure Save(descriptor: TZUGFeRDInvoiceDescriptor; stream: TStream); override;
  end;

implementation

{ TZUGFeRDInvoiceDescriptor20Writer }

procedure TZUGFeRDInvoiceDescriptor20Writer.Save(descriptor: TZUGFeRDInvoiceDescriptor; stream: TStream);
var
  streamPosition : Int64;
begin
  if (stream = nil) then
    raise TZUGFeRDIllegalStreamException.Create('Cannot write to stream');

  // write data
  streamPosition := stream.Position;

  FDescriptor := descriptor;
  Writer := TZUGFeRDProfileAwareXmlTextWriter.Create(stream,TEncoding.UTF8,FDescriptor.Profile);
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
//
//            Writer.WriteStartElement('rsm:ExchangedDocument');
//            Writer.WriteElementString('ram:ID', this.Descriptor.InvoiceNo);
//            Writer.WriteElementString('ram:Name', _translateInvoiceType(this.Descriptor.Type), TZUGFeRDProfile.Extended);
//            Writer.WriteElementString('ram:TypeCode', String.Format('begin0end;', _encodeInvoiceType(this.Descriptor.Type)));
//
//            if (this.Descriptor.InvoiceDate.HasValue)
//            begin
//                Writer.WriteStartElement('ram:IssueDateTime');
//                Writer.WriteStartElement('udt:DateTimeString');
//                Writer.WriteAttributeString('format', '102');
//                Writer.WriteValue(_formatDate(this.Descriptor.InvoiceDate.Value));
//                Writer.WriteEndElement(); // !udt:DateTimeString
//                Writer.WriteEndElement(); // !IssueDateTime
//            end;
//            _writeNotes(Writer, this.Descriptor.Notes);
//            Writer.WriteEndElement(); // !rsm:ExchangedDocument
//            #endregion
//
//            /*
//             * @todo continue here to adopt v2 tag names
//             */
//
//            #region SpecifiedSupplyChainTradeTransaction
//            Writer.WriteStartElement('rsm:SupplyChainTradeTransaction');
//
//            foreach (TradeLineItem tradeLineItem in this.Descriptor.TradeLineItems)
//            begin
//                Writer.WriteStartElement('ram:IncludedSupplyChainTradeLineItem');
//
//                if (tradeLineItem.AssociatedDocument<> nil)
//                begin
//                    Writer.WriteStartElement('ram:AssociatedDocumentLineDocument');
//                    if (!String.IsNullOrEmpty(tradeLineItem.AssociatedDocument.LineID))
//                    begin
//                        Writer.WriteElementString('ram:LineID', tradeLineItem.AssociatedDocument.LineID);
//                    end;
//                    _writeNotes(Writer, tradeLineItem.AssociatedDocument.Notes);
//                    Writer.WriteEndElement(); // ram:AssociatedDocumentLineDocument
//                end;
//
//                // handelt es sich um einen Kommentar?
//                if ((tradeLineItem.AssociatedDocument?.Notes.Count > 0) && (tradeLineItem.BilledQuantity == 0) && (String.IsNullOrEmpty(tradeLineItem.Description)))
//                begin
//                    Writer.WriteEndElement(); // !ram:IncludedSupplyChainTradeLineItem
//                    continue;
//                end;
//
//                Writer.WriteStartElement('ram:SpecifiedTradeProduct');
//                if ((tradeLineItem.GlobalID<> nil) && (tradeLineItem.GlobalID.SchemeID != GlobalIDSchemeIdentifiers.Unknown) && !String.IsNullOrEmpty(tradeLineItem.GlobalID.ID))
//                begin
//                    _writeElementWithAttribute(Writer, 'ram:GlobalID', 'schemeID', tradeLineItem.GlobalID.SchemeID.EnumToString(), tradeLineItem.GlobalID.ID);
//                end;
//
//                _writeOptionalElementString(Writer, 'ram:SellerAssignedID', tradeLineItem.SellerAssignedID);
//                _writeOptionalElementString(Writer, 'ram:BuyerAssignedID', tradeLineItem.BuyerAssignedID);
//                _writeOptionalElementString(Writer, 'ram:Name', tradeLineItem.Name);
//                _writeOptionalElementString(Writer, 'ram:Description', tradeLineItem.Description);
//
//                if (tradeLineItem.ApplicableProductCharacteristics<> nil && tradeLineItem.ApplicableProductCharacteristics.Any())
//                begin
//                    foreach (var productCharacteristic in tradeLineItem.ApplicableProductCharacteristics)
//                    begin
//                        Writer.WriteStartElement('ram:ApplicableProductCharacteristic');
//                        _writeOptionalElementString(Writer, 'ram:Description', productCharacteristic.Description);
//                        _writeOptionalElementString(Writer, 'ram:Value', productCharacteristic.Value);
//                        Writer.WriteEndElement(); // !ram:ApplicableProductCharacteristic
//                    end;
//                end;
//
//                Writer.WriteEndElement(); // !ram:SpecifiedTradeProduct
//
//                Writer.WriteStartElement('ram:SpecifiedLineTradeAgreement', TZUGFeRDProfile.Basic | TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended);
//
//                if (tradeLineItem.BuyerOrderReferencedDocument<> nil)
//                begin
//                    Writer.WriteStartElement('ram:BuyerOrderReferencedDocument', TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung);
//
//                    #region IssuerAssignedID
//                    //Bestellnummer
//                    if (!String.IsNullOrEmpty(tradeLineItem.BuyerOrderReferencedDocument.ID))
//                    begin
//                        Writer.WriteElementString('ram:IssuerAssignedID', tradeLineItem.BuyerOrderReferencedDocument.ID);
//                    end;
//                    #endregion
//
//                    #region LineID
//                    //Referenz zur Bestellposition
//                    //ToDo: fehlt ganz
//                    #endregion
//
//                    #region IssueDateTime
//                    if (tradeLineItem.BuyerOrderReferencedDocument.IssueDateTime.HasValue)
//                    begin
//                        Writer.WriteStartElement('ram:FormattedIssueDateTime');
//                        Writer.WriteStartElement('qdt:DateTimeString');
//                        Writer.WriteAttributeString('format', '102');
//                        Writer.WriteValue(_formatDate(tradeLineItem.BuyerOrderReferencedDocument.IssueDateTime.Value));
//                        Writer.WriteEndElement(); // !qdt:DateTimeString
//                        Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//                    end;
//                    #endregion
//
//                    Writer.WriteEndElement(); // !ram:BuyerOrderReferencedDocument
//                end;
//
//                if (tradeLineItem.ContractReferencedDocument<> nil)
//                begin
//                    Writer.WriteStartElement('ram:ContractReferencedDocument', TZUGFeRDProfile.Extended);
//                    if (tradeLineItem.ContractReferencedDocument.IssueDateTime.HasValue)
//                    begin
//                        Writer.WriteStartElement('ram:FormattedIssueDateTime');
//                        Writer.WriteStartElement('qdt:DateTimeString');
//                        Writer.WriteAttributeString('format', '102');
//                        Writer.WriteValue(_formatDate(tradeLineItem.ContractReferencedDocument.IssueDateTime.Value));
//                        Writer.WriteEndElement(); // !udt:DateTimeString
//                        Writer.WriteEndElement(); // !ram:IssueDateTime
//                    end;
//                    if (!String.IsNullOrEmpty(tradeLineItem.ContractReferencedDocument.ID))
//                    begin
//                        Writer.WriteElementString('ram:IssuerAssignedID', tradeLineItem.ContractReferencedDocument.ID);
//                    end;
//
//                    Writer.WriteEndElement(); // !ram:ContractReferencedDocument(Extended)
//                end;
//
//                if (tradeLineItem.AdditionalReferencedDocuments<> nil)
//                begin
//                    foreach (AdditionalReferencedDocument document in tradeLineItem.AdditionalReferencedDocuments)
//                    begin
//                        Writer.WriteStartElement('ram:AdditionalReferencedDocument');
//                        if (document.IssueDateTime.HasValue)
//                        begin
//                            Writer.WriteStartElement('ram:FormattedIssueDateTime');
//                            Writer.WriteStartElement('qdt:DateTimeString');
//                            Writer.WriteValue(_formatDate(document.IssueDateTime.Value));
//                            Writer.WriteEndElement(); // !qdt:DateTimeString
//                            Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//                        end;
//
//                        Writer.WriteElementString('ram:LineID', String.Format('begin0end;', tradeLineItem.AssociatedDocument?.LineID));
//
//                        if (!String.IsNullOrEmpty(document.ID))
//                        begin
//                            Writer.WriteElementString('ram:IssuerAssignedID', document.ID);
//                        end;
//
//                        Writer.WriteElementString('ram:ReferenceTypeCode', document.ReferenceTypeCode.EnumToString());
//
//                        Writer.WriteEndElement(); // !ram:AdditionalReferencedDocument
//                    end; // !foreach(document)
//                end;
//
//                Writer.WriteStartElement('ram:GrossPriceProductTradePrice');
//                _writeOptionalAmount(Writer, 'ram:ChargeAmount', tradeLineItem.GrossUnitPrice, numDecimals: 4, forceCurrency: false);
//                if (tradeLineItem.UnitQuantity.HasValue)
//                begin
//                    _writeElementWithAttribute(Writer, 'ram:BasisQuantity', 'unitCode', tradeLineItem.UnitCode.EnumToString(), _formatDecimal(tradeLineItem.UnitQuantity.Value, 4));
//                end;
//
//                foreach (TradeAllowanceCharge tradeAllowanceCharge in tradeLineItem.TradeAllowanceCharges)
//                begin
//                    Writer.WriteStartElement('ram:AppliedTradeAllowanceCharge');
//
//                    Writer.WriteStartElement('ram:ChargeIndicator');
//                    Writer.WriteElementString('udt:Indicator', tradeAllowanceCharge.ChargeIndicator ? 'true' : 'false');
//                    Writer.WriteEndElement(); // !ram:ChargeIndicator
//
//                    Writer.WriteStartElement('ram:BasisAmount');
//                    Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.BasisAmount, 4));
//                    Writer.WriteEndElement();
//                    Writer.WriteStartElement('ram:ActualAmount');
//                    Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ActualAmount, 4));
//                    Writer.WriteEndElement();
//
//                    _writeOptionalElementString(Writer, 'ram:Reason', tradeAllowanceCharge.Reason);
//
//                    Writer.WriteEndElement(); // !AppliedTradeAllowanceCharge
//                end;
//
//                Writer.WriteEndElement(); // ram:GrossPriceProductTradePrice
//
//                Writer.WriteStartElement('ram:NetPriceProductTradePrice');
//                _writeOptionalAmount(Writer, 'ram:ChargeAmount', tradeLineItem.NetUnitPrice, numDecimals: 4, forceCurrency: false);
//
//                if (tradeLineItem.UnitQuantity.HasValue)
//                begin
//                    _writeElementWithAttribute(Writer, 'ram:BasisQuantity', 'unitCode', tradeLineItem.UnitCode.EnumToString(), _formatDecimal(tradeLineItem.UnitQuantity.Value, 4));
//                end;
//                Writer.WriteEndElement(); // ram:NetPriceProductTradePrice
//
//                Writer.WriteEndElement(); // !ram:SpecifiedLineTradeAgreement
//
//                if (Descriptor.TZUGFeRDProfile != TZUGFeRDProfile.Basic)
//                begin
//                    Writer.WriteStartElement('ram:SpecifiedLineTradeDelivery');
//                    _writeElementWithAttribute(Writer, 'ram:BilledQuantity', 'unitCode', tradeLineItem.UnitCode.EnumToString(), _formatDecimal(tradeLineItem.BilledQuantity, 4));
//
//                    if (tradeLineItem.DeliveryNoteReferencedDocument<> nil)
//                    begin
//                        Writer.WriteStartElement('ram:DeliveryNoteReferencedDocument');
//                        if (tradeLineItem.DeliveryNoteReferencedDocument.IssueDateTime.HasValue)
//                        begin
//                            //Old path of Version 1.0
//                            //Writer.WriteStartElement('ram:IssueDateTime');
//                            //Writer.WriteValue(_formatDate(tradeLineItem.DeliveryNoteReferencedDocument.IssueDateTime.Value));
//                            //Writer.WriteEndElement(); // !ram:IssueDateTime
//
//                            Writer.WriteStartElement('ram:FormattedIssueDateTime');
//                            Writer.WriteStartElement('qdt:DateTimeString');
//                            Writer.WriteAttributeString('format', '102');
//                            Writer.WriteValue(_formatDate(this.Descriptor.OrderDate.Value));
//                            Writer.WriteEndElement(); // !qdt:DateTimeString
//                            Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//                        end;
//
//                        if (!String.IsNullOrEmpty(tradeLineItem.DeliveryNoteReferencedDocument.ID))
//                        begin
//                            Writer.WriteElementString('ram:IssuerAssignedID', tradeLineItem.DeliveryNoteReferencedDocument.ID);
//                        end;
//
//                        Writer.WriteEndElement(); // !ram:DeliveryNoteReferencedDocument
//                    end;
//
//                    if (tradeLineItem.ActualDeliveryDate.HasValue)
//                    begin
//                        Writer.WriteStartElement('ram:ActualDeliverySupplyChainEvent');
//                        Writer.WriteStartElement('ram:OccurrenceDateTime');
//                        Writer.WriteStartElement('udt:DateTimeString');
//                        Writer.WriteAttributeString('format', '102');
//                        Writer.WriteValue(_formatDate(tradeLineItem.ActualDeliveryDate.Value));
//                        Writer.WriteEndElement(); // 'udt:DateTimeString
//                        Writer.WriteEndElement(); // !OccurrenceDateTime()
//                        Writer.WriteEndElement(); // !ActualDeliverySupplyChainEvent
//                    end;
//
//                    Writer.WriteEndElement(); // !ram:SpecifiedLineTradeDelivery
//                end;
//                else
//                begin
//                    Writer.WriteStartElement('ram:SpecifiedLineTradeDelivery');
//                    _writeElementWithAttribute(Writer, 'ram:BilledQuantity', 'unitCode', tradeLineItem.UnitCode.EnumToString(), _formatDecimal(tradeLineItem.BilledQuantity, 4));
//                    Writer.WriteEndElement(); // !ram:SpecifiedLineTradeDelivery
//                end;
//
//                Writer.WriteStartElement('ram:SpecifiedLineTradeSettlement');
//
//                Writer.WriteStartElement('ram:ApplicableTradeTax', TZUGFeRDProfile.Basic | TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended);
//                Writer.WriteElementString('ram:TypeCode', tradeLineItem.TaxType.EnumToString());
//                Writer.WriteElementString('ram:CategoryCode', tradeLineItem.TaxCategoryCode.EnumToString());
//                Writer.WriteElementString('ram:RateApplicablePercent', _formatDecimal(tradeLineItem.TaxPercent));
//                Writer.WriteEndElement(); // !ram:ApplicableTradeTax
//
//                if (tradeLineItem.BillingPeriodStart.HasValue || tradeLineItem.BillingPeriodEnd.HasValue)
//                begin
//                    Writer.WriteStartElement('ram:BillingSpecifiedPeriod', TZUGFeRDProfile.BasicWL | TZUGFeRDProfile.Basic | TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung);
//                    if (tradeLineItem.BillingPeriodStart.HasValue)
//                    begin
//                        Writer.WriteStartElement('ram:StartDateTime');
//                        _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(tradeLineItem.BillingPeriodStart.Value));
//                        Writer.WriteEndElement(); // !StartDateTime
//                    end;
//
//                    if (tradeLineItem.BillingPeriodEnd.HasValue)
//                    begin
//                        Writer.WriteStartElement('ram:EndDateTime');
//                        _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(tradeLineItem.BillingPeriodEnd.Value));
//                        Writer.WriteEndElement(); // !EndDateTime
//                    end;
//                    Writer.WriteEndElement(); // !BillingSpecifiedPeriod
//                end;
//
//                Writer.WriteStartElement('ram:SpecifiedTradeSettlementLineMonetarySummation');
//
//                decimal _total = 0m;
//
//                if (tradeLineItem.LineTotalAmount.HasValue)
//                begin
//                    _total = tradeLineItem.LineTotalAmount.Value;
//                end;
//                else if (tradeLineItem.NetUnitPrice.HasValue)
//                begin
//                    _total = tradeLineItem.NetUnitPrice.Value * tradeLineItem.BilledQuantity;
//                end;
//
//                Writer.WriteElementString('ram:LineTotalAmount', _formatDecimal(_total));
//
//                Writer.WriteEndElement(); // ram:SpecifiedTradeSettlementLineMonetarySummation
//                Writer.WriteEndElement(); // !ram:SpecifiedLineTradeSettlement
//
//                Writer.WriteEndElement(); // !ram:IncludedSupplyChainTradeLineItem
//            end; // !foreach(tradeLineItem)
//
//            Writer.WriteStartElement('ram:ApplicableHeaderTradeAgreement');
//            if (!String.IsNullOrEmpty(this.Descriptor.ReferenceOrderNo))
//            begin
//                Writer.WriteElementString('ram:BuyerReference', this.Descriptor.ReferenceOrderNo);
//            end;
//
//            _writeOptionalParty(Writer, 'ram:SellerTradeParty', this.Descriptor.Seller, this.Descriptor.SellerContact, TaxRegistrations: this.Descriptor.SellerTaxRegistration);
//            _writeOptionalParty(Writer, 'ram:BuyerTradeParty', this.Descriptor.Buyer, this.Descriptor.BuyerContact, TaxRegistrations: this.Descriptor.BuyerTaxRegistration);
//
//
//            #region SellerOrderReferencedDocument (BT-14: Comfort, Extended)
//            if (null != this.Descriptor.SellerOrderReferencedDocument && !string.IsNullOrEmpty(Descriptor.SellerOrderReferencedDocument.ID))
//            begin
//                Writer.WriteStartElement('ram:SellerOrderReferencedDocument', TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended);
//                Writer.WriteElementString('ram:IssuerAssignedID', this.Descriptor.SellerOrderReferencedDocument.ID);
//                if (this.Descriptor.SellerOrderReferencedDocument.IssueDateTime.HasValue)
//                begin
//                    Writer.WriteStartElement('ram:FormattedIssueDateTime', TZUGFeRDProfile.Extended);
//                    Writer.WriteStartElement('qdt:DateTimeString');
//                    Writer.WriteAttributeString('format', '102');
//                    Writer.WriteValue(_formatDate(this.Descriptor.SellerOrderReferencedDocument.IssueDateTime.Value));
//                    Writer.WriteEndElement(); // !qdt:DateTimeString
//                    Writer.WriteEndElement(); // !IssueDateTime()
//                end;
//
//                Writer.WriteEndElement(); // !SellerOrderReferencedDocument
//            end;
//            #endregion
//
//
//            if (!String.IsNullOrEmpty(this.Descriptor.OrderNo))
//            begin
//                Writer.WriteStartElement('ram:BuyerOrderReferencedDocument');
//                Writer.WriteElementString('ram:IssuerAssignedID', this.Descriptor.OrderNo);
//                if (this.Descriptor.OrderDate.HasValue)
//                begin
//                    Writer.WriteStartElement('ram:FormattedIssueDateTime');
//                    Writer.WriteStartElement('qdt:DateTimeString');
//                    Writer.WriteAttributeString('format', '102');
//                    Writer.WriteValue(_formatDate(this.Descriptor.OrderDate.Value));
//                    Writer.WriteEndElement(); // !qdt:DateTimeString
//                    Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//                end;
//                Writer.WriteEndElement(); // !BuyerOrderReferencedDocument
//            end;
//
//
//            if (this.Descriptor.AdditionalReferencedDocuments<> nil)
//            begin
//                foreach (AdditionalReferencedDocument document in this.Descriptor.AdditionalReferencedDocuments)
//                begin
//                    Writer.WriteStartElement('ram:AdditionalReferencedDocument');
//                    if (document.IssueDateTime.HasValue)
//                    begin
//                        Writer.WriteStartElement('ram:FormattedIssueDateTime');
//                        Writer.WriteStartElement('qdt:DateTimeString');
//                        Writer.WriteValue(_formatDate(document.IssueDateTime.Value));
//                        Writer.WriteEndElement(); // !udt:DateTimeString
//                        Writer.WriteEndElement(); // !FormattedIssueDateTime
//                    end;
//
//                    if (document.ReferenceTypeCode != ReferenceTypeCodes.Unknown)
//                    begin
//                        Writer.WriteElementString('ram:TypeCode', document.ReferenceTypeCode.EnumToString());
//                    end;
//
//                    Writer.WriteElementString('ram:ID', document.ID);
//                    Writer.WriteEndElement(); // !ram:AdditionalReferencedDocument
//                end; // !foreach(document)
//            end;
//
//            Writer.WriteEndElement(); // !ApplicableHeaderTradeAgreement
//
//            Writer.WriteStartElement('ram:ApplicableHeaderTradeDelivery'); // Pflichteintrag
//
//            if (Descriptor.TZUGFeRDProfile == TZUGFeRDProfile.Extended)
//            begin
//                _writeOptionalParty(Writer, 'ram:ShipToTradeParty', this.Descriptor.ShipTo);
//                _writeOptionalParty(Writer, 'ram:ShipFromTradeParty', this.Descriptor.ShipFrom);
//            end;
//
//            if (this.Descriptor.ActualDeliveryDate.HasValue)
//            begin
//                Writer.WriteStartElement('ram:ActualDeliverySupplyChainEvent');
//                Writer.WriteStartElement('ram:OccurrenceDateTime');
//                Writer.WriteStartElement('udt:DateTimeString');
//                Writer.WriteAttributeString('format', '102');
//                Writer.WriteValue(_formatDate(this.Descriptor.ActualDeliveryDate.Value));
//                Writer.WriteEndElement(); // 'udt:DateTimeString
//                Writer.WriteEndElement(); // !OccurrenceDateTime()
//                Writer.WriteEndElement(); // !ActualDeliverySupplyChainEvent
//            end;
//
//            if (this.Descriptor.DeliveryNoteReferencedDocument<> nil)
//            begin
//                Writer.WriteStartElement('ram:DeliveryNoteReferencedDocument');
//
//                if (this.Descriptor.DeliveryNoteReferencedDocument.IssueDateTime.HasValue)
//                begin
//                    Writer.WriteStartElement('ram:FormattedIssueDateTime');
//                    Writer.WriteValue(_formatDate(this.Descriptor.DeliveryNoteReferencedDocument.IssueDateTime.Value, false));
//                    Writer.WriteEndElement(); // !IssueDateTime
//                end;
//
//                Writer.WriteElementString('ram:IssuerAssignedID', this.Descriptor.DeliveryNoteReferencedDocument.ID);
//                Writer.WriteEndElement(); // !DeliveryNoteReferencedDocument
//            end;
//
//            Writer.WriteEndElement(); // !ApplicableHeaderTradeDelivery
//
//            Writer.WriteStartElement('ram:ApplicableHeaderTradeSettlement');
//            // order of sub-elements of ApplicableHeaderTradeSettlement:
//            //   1. CreditorReferenceID (optional)
//            //   2. PaymentReference (optional)
//            //   3. TaxCurrencyCode (optional)
//            //   4. InvoiceCurrencyCode (optional)
//            //   5. InvoiceIssuerReference (optional)
//            //   6. InvoicerTradeParty (optional)
//            //   7. InvoiceeTradeParty (optional)
//            //   8. PayeeTradeParty (optional)
//            //   9. TaxApplicableTradeCurrencyExchange (optional)
//            //  10. SpecifiedTradeSettlementPaymentMeans (optional)
//            //  11. ApplicableTradeTax (optional)
//            //  12. BillingSpecifiedPeriod (optional)
//            //  13. SpecifiedTradeAllowanceCharge (optional)
//            //  14. SpecifiedLogisticsServiceCharge (optional)
//            //  15. SpecifiedTradePaymentTerms (optional)
//            //  16. SpecifiedTradeSettlementHeaderMonetarySummation
//            //  17. InvoiceReferencedDocument (optional)
//            //  18. ReceivableSpecifiedTradeAccountingAccount (optional)
//            //  19. SpecifiedAdvancePayment (optional)
//
//            //   1. CreditorReferenceID (optional)
//            if (!String.IsNullOrEmpty(this.Descriptor.PaymentMeans?.SEPACreditorIdentifier))
//            begin
//                _writeOptionalElementString(Writer, 'ram:CreditorReferenceID', Descriptor.PaymentMeans?.SEPACreditorIdentifier, TZUGFeRDProfile.BasicWL | TZUGFeRDProfile.Basic | TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung | TZUGFeRDProfile.XRechnung1);
//            end;
//
//            //   2. PaymentReference (optional)
//            if (!String.IsNullOrEmpty(this.Descriptor.PaymentReference))
//            begin
//                _writeOptionalElementString(Writer, 'ram:PaymentReference', this.Descriptor.PaymentReference);
//            end;
//
//            //   4. InvoiceCurrencyCode (optional)
//            Writer.WriteElementString('ram:InvoiceCurrencyCode', this.Descriptor.Currency.EnumToString());
//
//            //   7. InvoiceeTradeParty (optional)
//            if (Descriptor.TZUGFeRDProfile == TZUGFeRDProfile.Extended)
//            begin
//                _writeOptionalParty(Writer, 'ram:InvoiceeTradeParty', this.Descriptor.Invoicee);
//            end;
//
//            //   8. PayeeTradeParty (optional)
//            if (Descriptor.TZUGFeRDProfile != TZUGFeRDProfile.Minimum)
//            begin
//                _writeOptionalParty(Writer, 'ram:PayeeTradeParty', this.Descriptor.Payee);
//            end;
//
//            //  10. SpecifiedTradeSettlementPaymentMeans (optional)
//            if (this.Descriptor.CreditorBankAccounts.Count == 0 && this.Descriptor.DebitorBankAccounts.Count == 0)
//            begin
//                if (this.Descriptor.PaymentMeans<> nil)
//                begin
//                    Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans');
//
//                    if ((this.Descriptor.PaymentMeans<> nil) && (this.Descriptor.PaymentMeans.TypeCode != PaymentMeansTypeCodes.Unknown))
//                    begin
//                        Writer.WriteElementString('ram:TypeCode', this.Descriptor.PaymentMeans.TypeCode.EnumToString());
//                        Writer.WriteElementString('ram:Information', this.Descriptor.PaymentMeans.Information);
//
//                        if (this.Descriptor.PaymentMeans.FinancialCard<> nil)
//                        begin
//                            Writer.WriteStartElement('ram:ApplicableTradeSettlementFinancialCard', TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended);
//                            Writer.WriteElementString('ram:ID', Descriptor.PaymentMeans.FinancialCard.Id);
//                            Writer.WriteElementString('ram:CardholderName', Descriptor.PaymentMeans.FinancialCard.CardholderName);
//                            Writer.WriteEndElement(); // !ram:ApplicableTradeSettlementFinancialCard
//                        end;
//                    end;
//                    Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
//                end;
//            end;
//            else
//            begin
//                foreach (BankAccount account in this.Descriptor.CreditorBankAccounts)
//                begin
//                    Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans');
//
//                    if ((this.Descriptor.PaymentMeans<> nil) && (this.Descriptor.PaymentMeans.TypeCode != PaymentMeansTypeCodes.Unknown))
//                    begin
//                        Writer.WriteElementString('ram:TypeCode', this.Descriptor.PaymentMeans.TypeCode.EnumToString());
//                        Writer.WriteElementString('ram:Information', this.Descriptor.PaymentMeans.Information);
//
//                        if (this.Descriptor.PaymentMeans.FinancialCard<> nil)
//                        begin
//                            Writer.WriteStartElement('ram:ApplicableTradeSettlementFinancialCard', TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended);
//                            Writer.WriteElementString('ram:ID', Descriptor.PaymentMeans.FinancialCard.Id);
//                            Writer.WriteElementString('ram:CardholderName', Descriptor.PaymentMeans.FinancialCard.CardholderName);
//                            Writer.WriteEndElement(); // !ram:ApplicableTradeSettlementFinancialCard
//                        end;
//                    end;
//
//                    Writer.WriteStartElement('ram:PayeePartyCreditorFinancialAccount');
//                    Writer.WriteElementString('ram:IBANID', account.IBAN);
//                    if (!String.IsNullOrEmpty(account.Name))
//                    begin
//                        Writer.WriteElementString('ram:AccountName', account.Name);
//                    end;
//                    if (!String.IsNullOrEmpty(account.ID))
//                    begin
//                        Writer.WriteElementString('ram:ProprietaryID', account.ID);
//                    end;
//                    Writer.WriteEndElement(); // !PayeePartyCreditorFinancialAccount
//
//                    Writer.WriteStartElement('ram:PayeeSpecifiedCreditorFinancialInstitution');
//                    Writer.WriteElementString('ram:BICID', account.BIC);
//
//                    if (!String.IsNullOrEmpty(account.Bankleitzahl))
//                    begin
//                        Writer.WriteElementString('ram:GermanBankleitzahlID', account.Bankleitzahl);
//                    end;
//
//                    Writer.WriteEndElement(); // !PayeeSpecifiedCreditorFinancialInstitution
//                    Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
//                end;
//
//                foreach (BankAccount account in this.Descriptor.DebitorBankAccounts)
//                begin
//                    Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans');
//
//                    if ((this.Descriptor.PaymentMeans<> nil) && (this.Descriptor.PaymentMeans.TypeCode != PaymentMeansTypeCodes.Unknown))
//                    begin
//                        Writer.WriteElementString('ram:TypeCode', this.Descriptor.PaymentMeans.TypeCode.EnumToString());
//                        Writer.WriteElementString('ram:Information', this.Descriptor.PaymentMeans.Information);
//                    end;
//
//                    Writer.WriteStartElement('ram:PayerPartyDebtorFinancialAccount');
//                    Writer.WriteElementString('ram:IBANID', account.IBAN);
//                    if (!String.IsNullOrEmpty(account.ID))
//                    begin
//                        Writer.WriteElementString('ram:ProprietaryID', account.ID);
//                    end;
//                    Writer.WriteEndElement(); // !PayerPartyDebtorFinancialAccount
//
//                    if (!string.IsNullOrEmpty(account.BIC) ||
//                        !string.IsNullOrEmpty(account.Bankleitzahl) ||
//                        !string.IsNullOrEmpty(account.BankName))
//                    begin
//                        Writer.WriteStartElement('ram:PayerSpecifiedDebtorFinancialInstitution');
//
//                        if (!String.IsNullOrEmpty(account.BIC))
//                        begin
//                            Writer.WriteElementString('ram:BICID', account.BIC);
//                        end;
//
//                        if (!String.IsNullOrEmpty(account.Bankleitzahl))
//                        begin
//                            Writer.WriteElementString('ram:GermanBankleitzahlID', account.Bankleitzahl);
//                        end;
//
//                        if (!String.IsNullOrEmpty(account.BankName))
//                        begin
//                            Writer.WriteElementString('ram:Name', account.BankName);
//                        end;
//
//                        Writer.WriteEndElement(); // !PayerSpecifiedDebtorFinancialInstitution
//                    end;
//
//                    Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
//                end;
//            end;
//
//
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
//             */
//
//            //  11. ApplicableTradeTax (optional)
//            _writeOptionalTaxes(Writer);
//
//            #region BillingSpecifiedPeriod
//            //  12. BillingSpecifiedPeriod (optional)
//            if (Descriptor.BillingPeriodStart.HasValue || Descriptor.BillingPeriodEnd.HasValue)
//            begin
//                Writer.WriteStartElement('ram:BillingSpecifiedPeriod', TZUGFeRDProfile.BasicWL | TZUGFeRDProfile.Basic | TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung);
//                if (Descriptor.BillingPeriodStart.HasValue)
//                begin
//                    Writer.WriteStartElement('ram:StartDateTime');
//                    _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(this.Descriptor.BillingPeriodStart.Value));
//                    Writer.WriteEndElement(); // !StartDateTime
//                end;
//
//                if (Descriptor.BillingPeriodEnd.HasValue)
//                begin
//                    Writer.WriteStartElement('ram:EndDateTime');
//                    _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(this.Descriptor.BillingPeriodEnd.Value));
//                    Writer.WriteEndElement(); // !EndDateTime
//                end;
//                Writer.WriteEndElement(); // !BillingSpecifiedPeriod
//            end;
//            #endregion
//
//            //  13. SpecifiedTradeAllowanceCharge (optional)
//            if ((this.Descriptor.TradeAllowanceCharges<> nil) && (this.Descriptor.TradeAllowanceCharges.Count > 0))
//            begin
//                foreach (TradeAllowanceCharge tradeAllowanceCharge in this.Descriptor.TradeAllowanceCharges)
//                begin
//                    Writer.WriteStartElement('ram:SpecifiedTradeAllowanceCharge');
//                    Writer.WriteStartElement('ram:ChargeIndicator');
//                    Writer.WriteElementString('udt:Indicator', tradeAllowanceCharge.ChargeIndicator ? 'true' : 'false');
//                    Writer.WriteEndElement(); // !ram:ChargeIndicator
//
//                    Writer.WriteStartElement('ram:BasisAmount');
//                    Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.BasisAmount));
//                    Writer.WriteEndElement();
//
//                    Writer.WriteStartElement('ram:ActualAmount');
//                    Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ActualAmount));
//                    Writer.WriteEndElement();
//
//
//                    _writeOptionalElementString(Writer, 'ram:Reason', tradeAllowanceCharge.Reason);
//
//                    if (tradeAllowanceCharge.Tax<> nil)
//                    begin
//                        Writer.WriteStartElement('ram:CategoryTradeTax');
//                        Writer.WriteElementString('ram:TypeCode', tradeAllowanceCharge.Tax.TypeCode.EnumToString());
//                        if (tradeAllowanceCharge.Tax.CategoryCode.HasValue)
//                            Writer.WriteElementString('ram:CategoryCode', tradeAllowanceCharge.Tax.CategoryCode?.EnumToString());
//                        Writer.WriteElementString('ram:RateApplicablePercent', _formatDecimal(tradeAllowanceCharge.Tax.Percent));
//                        Writer.WriteEndElement();
//                    end;
//                    Writer.WriteEndElement();
//                end;
//            end;
//
//            //  14. SpecifiedLogisticsServiceCharge (optional)
//            if ((this.Descriptor.ServiceCharges<> nil) && (this.Descriptor.ServiceCharges.Count > 0))
//            begin
//                foreach (ServiceCharge serviceCharge in this.Descriptor.ServiceCharges)
//                begin
//                    Writer.WriteStartElement('ram:SpecifiedLogisticsServiceCharge');
//                    if (!String.IsNullOrEmpty(serviceCharge.Description))
//                    begin
//                        Writer.WriteElementString('ram:Description', serviceCharge.Description);
//                    end;
//                    Writer.WriteElementString('ram:AppliedAmount', _formatDecimal(serviceCharge.Amount));
//                    if (serviceCharge.Tax<> nil)
//                    begin
//                        Writer.WriteStartElement('ram:AppliedTradeTax');
//                        Writer.WriteElementString('ram:TypeCode', serviceCharge.Tax.TypeCode.EnumToString());
//                        if (serviceCharge.Tax.CategoryCode.HasValue)
//                            Writer.WriteElementString('ram:CategoryCode', serviceCharge.Tax.CategoryCode?.EnumToString());
//                        Writer.WriteElementString('ram:RateApplicablePercent', _formatDecimal(serviceCharge.Tax.Percent));
//                        Writer.WriteEndElement();
//                    end;
//                    Writer.WriteEndElement();
//                end;
//            end;
//
//            //  15. SpecifiedTradePaymentTerms (optional)
//            if (this.Descriptor.PaymentTerms<> nil || !string.IsNullOrEmpty(Descriptor.PaymentMeans?.SEPAMandateReference))
//            begin
//                Writer.WriteStartElement('ram:SpecifiedTradePaymentTerms');
//                _writeOptionalElementString(Writer, 'ram:Description', this.Descriptor.PaymentTerms?.Description);
//                if (this.Descriptor.PaymentTerms?.DueDate.HasValue ?? false)
//                begin
//                    Writer.WriteStartElement('ram:DueDateDateTime');
//                    _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(this.Descriptor.PaymentTerms.DueDate.Value));
//                    Writer.WriteEndElement(); // !ram:DueDateDateTime
//                end;
//                _writeOptionalElementString(Writer, 'ram:DirectDebitMandateID', Descriptor.PaymentMeans?.SEPAMandateReference);
//                Writer.WriteEndElement();
//            end;
//
//            //  16. SpecifiedTradeSettlementHeaderMonetarySummation
//            Writer.WriteStartElement('ram:SpecifiedTradeSettlementHeaderMonetarySummation');
//            _writeOptionalAmount(Writer, 'ram:LineTotalAmount', this.Descriptor.LineTotalAmount);
//            _writeOptionalAmount(Writer, 'ram:ChargeTotalAmount', this.Descriptor.ChargeTotalAmount);
//            _writeOptionalAmount(Writer, 'ram:AllowanceTotalAmount', this.Descriptor.AllowanceTotalAmount);
//            _writeOptionalAmount(Writer, 'ram:TaxBasisTotalAmount', this.Descriptor.TaxBasisAmount);
//            _writeOptionalAmount(Writer, 'ram:TaxTotalAmount', this.Descriptor.TaxTotalAmount, forceCurrency: true);
//            _writeOptionalAmount(Writer, 'ram:RoundingAmount', this.Descriptor.RoundingAmount, TZUGFeRDProfile: TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended);  // RoundingAmount  //Rundungsbetrag
//            _writeOptionalAmount(Writer, 'ram:GrandTotalAmount', this.Descriptor.GrandTotalAmount);
//
//            if (this.Descriptor.TotalPrepaidAmount.HasValue)
//            begin
//                _writeOptionalAmount(Writer, 'ram:TotalPrepaidAmount', this.Descriptor.TotalPrepaidAmount.Value);
//            end;
//
//            _writeOptionalAmount(Writer, 'ram:DuePayableAmount', this.Descriptor.DuePayableAmount);
//            Writer.WriteEndElement(); // !ram:SpecifiedTradeSettlementHeaderMonetarySummation
//
//            #region InvoiceReferencedDocument
//            //  17. InvoiceReferencedDocument (optional)
//            if (this.Descriptor.InvoiceReferencedDocument<> nil)
//            begin
//                Writer.WriteStartElement('ram:InvoiceReferencedDocument');
//                _writeOptionalElementString(Writer, 'ram:IssuerAssignedID', this.Descriptor.InvoiceReferencedDocument.ID);
//                if (this.Descriptor.InvoiceReferencedDocument.IssueDateTime.HasValue)
//                begin
//                    Writer.WriteStartElement('ram:FormattedIssueDateTime');
//                    _writeElementWithAttribute(Writer, 'qdt:DateTimeString', 'format', '102', _formatDate(this.Descriptor.InvoiceReferencedDocument.IssueDateTime.Value));
//                    Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//                end;
//                Writer.WriteEndElement(); // !ram:InvoiceReferencedDocument
//            end;
//            #endregion
//
//
//            Writer.WriteEndElement(); // !ram:ApplicableHeaderTradeSettlement
//
//            Writer.WriteEndElement(); // !ram:SupplyChainTradeTransaction
//            #endregion
//
  Writer.WriteEndElement(); // !ram:Invoice
  Writer.WriteEndDocument();
  Writer.Flush();

  stream.Seek(streamPosition, soFromBeginning);
end;

function TZUGFeRDInvoiceDescriptor20Writer.Validate(
  descriptor: TZUGFeRDInvoiceDescriptor; throwExceptions: Boolean): Boolean;
begin
  Result := false;

  //TODO in C# enthalten, aber eigentlich falsch, deswegen auskommentiert
  //if (descriptor.TZUGFeRDProfile = TZUGFeRDProfile.BasicWL) then
  //if (throwExceptions) then
  //  raise TZUGFeRDUnsupportedException.Create('Invalid TZUGFeRDProfile used for ZUGFeRD 2.0 invoice.')
  //else
  //  exit;

//
//            if (descriptor.TZUGFeRDProfile != TZUGFeRDProfile.Extended) // check tax types, only extended TZUGFeRDProfile allows tax types other than vat
//            begin
//                if (!descriptor.TradeLineItems.All(l => l.TaxType.Equals(TaxTypes.VAT) || l.TaxType.Equals(TaxTypes.Unknown)))
//                begin
//                    if (throwExceptions) begin throw new UnsupportedException("Tax types other than VAT only possible with extended TZUGFeRDProfile."); end;
//                    return false;
//                end;
//            end;
//
  Result := true;
end;

procedure TZUGFeRDInvoiceDescriptor20Writer._writeOptionalAmount(
  _writer: TZUGFeRDProfileAwareXmlTextWriter; tagName: string;
  value: TZUGFeRDNullable<Currency>;
  numDecimals: Integer; forceCurrency: Boolean; Profile: TZUGFeRDProfiles);
begin
  if (value.HasValue) then // && (value.Value != decimal.MinValue))
  begin
    _writer.WriteStartElement(tagName,Profile);
    if forceCurrency then
      _writer.WriteAttributeString('currencyID', TZUGFeRDCurrencyCodesExtensions.EnumToString(FDescriptor.Currency));
    _writer.WriteValue(_formatDecimal(value.Value, numDecimals));
    _writer.WriteEndElement; // !tagName
  end;
end;

end.
