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

unit intf.ZUGFeRDInvoiceDescriptor21Writer;

interface

uses
  System.SysUtils,System.Classes,System.StrUtils,Generics.Collections
  ,intf.ZUGFeRDInvoiceDescriptor
  ,intf.ZUGFeRDProfileAwareXmlTextWriter
  ,intf.ZUGFeRDInvoiceDescriptorwriter
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
  ;

type
  TZUGFeRDInvoiceDescriptor21Writer = class(TZUGFeRDInvoiceDescriptorWriter)
  private
    Writer: TZUGFeRDProfileAwareXmlTextWriter;
    FDescriptor: TZUGFeRDInvoiceDescriptor;
    procedure _writeOptionalAmount(_writer : TZUGFeRDProfileAwareXmlTextWriter; tagName : string; value : TZUGFeRDNullable<Currency>; numDecimals : Integer = 2; forceCurrency : Boolean = false; profile : TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
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

{ TZUGFeRDInvoiceDescriptor21Writer }

procedure TZUGFeRDInvoiceDescriptor21Writer.Save(descriptor: TZUGFeRDInvoiceDescriptor; stream: TStream);
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
  //Prozesssteuerung
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
  //Gruppierung der Anwendungsempfehlungsinformationen
  Writer.WriteElementString('ram:ID', TZUGFeRDProfileExtensions.EnumToString(Descriptor.Profile,TZUGFeRDVersion.Version21));
  Writer.WriteEndElement(); // !ram:GuidelineSpecifiedDocumentContextParameter
  Writer.WriteEndElement(); // !rsm:ExchangedDocumentContext
  //#endregion
//
//            #region ExchangedDocument
//            //Gruppierung der Eigenschaften, die das gesamte Dokument betreffen.
//            Writer.WriteStartElement('rsm:ExchangedDocument');
//            Writer.WriteElementString('ram:ID', this.Descriptor.InvoiceNo); //Rechnungsnummer
//            Writer.WriteElementString('ram:Name', _translateInvoiceType(this.Descriptor.Type), TZUGFeRDProfile.Extended); //Dokumentenart (Freitext)
//            Writer.WriteElementString('ram:TypeCode', String.Format('begin0end;', _encodeInvoiceType(this.Descriptor.Type))); //Code für den Rechnungstyp
//                                                                                                                       //ToDo: LanguageID      //Sprachkennzeichen
//                                                                                                                       //ToDo: IncludedNote    //Freitext zur Rechnung
//            if (this.Descriptor.InvoiceDate.HasValue)
//            begin
//                Writer.WriteStartElement('ram:IssueDateTime');
//                Writer.WriteStartElement('udt:DateTimeString');  //Rechnungsdatum
//                Writer.WriteAttributeString('format', '102');
//                Writer.WriteValue(_formatDate(this.Descriptor.InvoiceDate.Value));
//                Writer.WriteEndElement(); // !udt:DateTimeString
//                Writer.WriteEndElement(); // !IssueDateTime
//            end;
//            _writeNotes(Writer, this.Descriptor.Notes);
//            Writer.WriteEndElement(); // !rsm:ExchangedDocument
//            #endregion
//
//
//            #region SpecifiedSupplyChainTradeTransaction
//            //Gruppierung der Informationen zum Geschäftsvorfall
//            Writer.WriteStartElement('rsm:SupplyChainTradeTransaction');
//
//            #region  IncludedSupplyChainTradeLineItem
//            foreach (TradeLineItem tradeLineItem in this.Descriptor.TradeLineItems)
//            begin
//                Writer.WriteStartElement('ram:IncludedSupplyChainTradeLineItem');
//
//                #region AssociatedDocumentLineDocument
//                //Gruppierung von allgemeinen Positionsangaben
//                if (tradeLineItem.AssociatedDocument <> nil)
//                begin
//                    Writer.WriteStartElement('ram:AssociatedDocumentLineDocument', TZUGFeRDProfile.Basic | TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung);
//                    if (!String.IsNullOrEmpty(tradeLineItem.AssociatedDocument.LineID))
//                    begin
//                        Writer.WriteElementString('ram:LineID', tradeLineItem.AssociatedDocument.LineID);
//                    end;
//                    _writeNotes(Writer, tradeLineItem.AssociatedDocument.Notes);
//                    Writer.WriteEndElement(); // ram:AssociatedDocumentLineDocument(Basic|Comfort|Extended|XRechnung)
//                end;
//                #endregion
//
//                // handelt es sich um einen Kommentar?
//                bool isCommentItem = false;
//                if ((tradeLineItem.AssociatedDocument?.Notes.Count > 0) && (tradeLineItem.BilledQuantity == 0) && (String.IsNullOrEmpty(tradeLineItem.Description)))
//                begin
//                    isCommentItem = true;
//                end;
//
//                #region SpecifiedTradeProduct
//                //Eine Gruppe von betriebswirtschaftlichen Begriffen, die Informationen über die in Rechnung gestellten Waren und Dienstleistungen enthält
//                Writer.WriteStartElement('ram:SpecifiedTradeProduct', TZUGFeRDProfile.Basic | TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung);
//                if ((tradeLineItem.GlobalID <> nil) && (tradeLineItem.GlobalID.SchemeID != GlobalIDSchemeIdentifiers.Unknown) && !String.IsNullOrEmpty(tradeLineItem.GlobalID.ID))
//                begin
//                    _writeElementWithAttribute(Writer, 'ram:GlobalID', 'schemeID', tradeLineItem.GlobalID.SchemeID.EnumToString(), tradeLineItem.GlobalID.ID, TZUGFeRDProfile.Basic | TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung);
//                end;
//
//                _writeOptionalElementString(Writer, 'ram:SellerAssignedID', tradeLineItem.SellerAssignedID, TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung);
//                _writeOptionalElementString(Writer, 'ram:BuyerAssignedID', tradeLineItem.BuyerAssignedID, TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung);
//
//                // BT-153
//                _writeOptionalElementString(Writer, 'ram:Name', tradeLineItem.Name, TZUGFeRDProfile.Basic | TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended);
//                _writeOptionalElementString(Writer, 'ram:Name', isCommentItem ? 'TEXT' : tradeLineItem.Name, TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung); // XRechnung erfordert einen Item-Namen (BR-25)
//
//                _writeOptionalElementString(Writer, 'ram:Description', tradeLineItem.Description, TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung);
//
//                if (tradeLineItem.ApplicableProductCharacteristics <> nil && tradeLineItem.ApplicableProductCharacteristics.Any())
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
//                Writer.WriteEndElement(); // !ram:SpecifiedTradeProduct(Basic|Comfort|Extended|XRechnung)
//                #endregion
//
//                #region SpecifiedLineTradeAgreement (Basic, Comfort, Extended, XRechnung)
//                //Eine Gruppe von betriebswirtschaftlichen Begriffen, die Informationen über den Preis für die in der betreffenden Rechnungsposition in Rechnung gestellten Waren und Dienstleistungen enthält
//
//                if (new TZUGFeRDProfile[] begin TZUGFeRDProfile.Basic, TZUGFeRDProfile.Comfort, TZUGFeRDProfile.Extended, TZUGFeRDProfile.XRechnung, TZUGFeRDProfile.XRechnung1 end;.Contains(descriptor.TZUGFeRDProfile))
//                begin
//                    Writer.WriteStartElement('ram:SpecifiedLineTradeAgreement', TZUGFeRDProfile.Basic | TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung);
//
//                    #region BuyerOrderReferencedDocument (Comfort, Extended, XRechnung)
//                    //Detailangaben zur zugehörigen Bestellung
//                    if (tradeLineItem.BuyerOrderReferencedDocument <> nil)
//                    begin
//                        Writer.WriteStartElement('ram:BuyerOrderReferencedDocument', TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung);
//
//                        #region IssuerAssignedID
//                        //Bestellnummer
//                        if (!String.IsNullOrEmpty(tradeLineItem.BuyerOrderReferencedDocument.ID))
//                        begin
//                            Writer.WriteElementString('ram:IssuerAssignedID', tradeLineItem.BuyerOrderReferencedDocument.ID);
//                        end;
//                        #endregion
//
//                        #region LineID
//                        //Referenz zur Bestellposition
//                        //ToDo: fehlt ganz
//                        #endregion
//
//                        #region IssueDateTime
//                        if (tradeLineItem.BuyerOrderReferencedDocument.IssueDateTime.HasValue)
//                        begin
//                            Writer.WriteStartElement('ram:FormattedIssueDateTime');
//                            Writer.WriteStartElement('qdt:DateTimeString');
//                            Writer.WriteAttributeString('format', '102');
//                            Writer.WriteValue(_formatDate(tradeLineItem.BuyerOrderReferencedDocument.IssueDateTime.Value));
//                            Writer.WriteEndElement(); // !qdt:DateTimeString
//                            Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//                        end;
//                        #endregion
//
//                        Writer.WriteEndElement(); // !ram:BuyerOrderReferencedDocument
//                    end;
//                    #endregion
//
//                    #region ContractReferencedDocument
//                    //Detailangaben zum zugehörigen Vertrag
//                    if (tradeLineItem.ContractReferencedDocument <> nil)
//                    begin
//                        Writer.WriteStartElement('ram:ContractReferencedDocument', TZUGFeRDProfile.Extended);
//                        if (tradeLineItem.ContractReferencedDocument.IssueDateTime.HasValue)
//                        begin
//                            Writer.WriteStartElement('ram:FormattedIssueDateTime');
//                            Writer.WriteStartElement('qdt:DateTimeString');
//                            Writer.WriteAttributeString('format', '102');
//                            Writer.WriteValue(_formatDate(tradeLineItem.ContractReferencedDocument.IssueDateTime.Value));
//                            Writer.WriteEndElement(); // !udt:DateTimeString
//                            Writer.WriteEndElement(); // !ram:IssueDateTime
//                        end;
//                        if (!String.IsNullOrEmpty(tradeLineItem.ContractReferencedDocument.ID))
//                        begin
//                            Writer.WriteElementString('ram:IssuerAssignedID', tradeLineItem.ContractReferencedDocument.ID);
//                        end;
//
//                        Writer.WriteEndElement(); // !ram:ContractReferencedDocument(Extended)
//                    end;
//                    #endregion
//
//                    #region AdditionalReferencedDocument (Extended)
//
//                    //Detailangaben zu einer zusätzlichen Dokumentenreferenz
//                    if (tradeLineItem.AdditionalReferencedDocuments <> nil)
//                    begin
//                        foreach (AdditionalReferencedDocument document in tradeLineItem.AdditionalReferencedDocuments)
//                        begin
//                            Writer.WriteStartElement('ram:AdditionalReferencedDocument', TZUGFeRDProfile.Extended);
//                            if (document.IssueDateTime.HasValue)
//                            begin
//                                Writer.WriteStartElement('ram:FormattedIssueDateTime');
//                                Writer.WriteStartElement('qdt:DateTimeString');
//                                Writer.WriteAttributeString('format', '102');
//                                Writer.WriteValue(_formatDate(document.IssueDateTime.Value));
//                                Writer.WriteEndElement(); // !udt:DateTimeString
//                                Writer.WriteEndElement(); // !ram:IssueDateTime
//                            end;
//
//                            Writer.WriteElementString('ram:LineID', String.Format('begin0end;', tradeLineItem.AssociatedDocument?.LineID));
//
//                            if (!String.IsNullOrEmpty(document.ID))
//                            begin
//                                Writer.WriteElementString('ram:IssuerAssignedID', document.ID);
//                            end;
//
//                            Writer.WriteElementString('ram:ReferenceTypeCode', document.ReferenceTypeCode.EnumToString());
//
//                            Writer.WriteEndElement(); // !ram:AdditionalReferencedDocument
//                        end; // !foreach(document)
//                    end;
//                    #endregion
//
//                    #region GrossPriceProductTradePrice (Comfort, Extended, XRechnung)
//                    // BT-148
//                    if (tradeLineItem.GrossUnitPrice.HasValue || (tradeLineItem.TradeAllowanceCharges.Count > 0))
//                    begin
//                        Writer.WriteStartElement('ram:GrossPriceProductTradePrice', TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung);
//                        _writeOptionalAmount(Writer, 'ram:ChargeAmount', tradeLineItem.GrossUnitPrice, 4);
//                        if (tradeLineItem.UnitQuantity.HasValue)
//                        begin
//                            _writeElementWithAttribute(Writer, 'ram:BasisQuantity', 'unitCode', tradeLineItem.UnitCode.EnumToString(), _formatDecimal(tradeLineItem.UnitQuantity.Value, 4));
//                        end;
//
//                        foreach (TradeAllowanceCharge tradeAllowanceCharge in tradeLineItem.TradeAllowanceCharges)
//                        begin
//                            Writer.WriteStartElement('ram:AppliedTradeAllowanceCharge');
//
//                            #region ChargeIndicator
//                            Writer.WriteStartElement('ram:ChargeIndicator');
//                            Writer.WriteElementString('udt:Indicator', tradeAllowanceCharge.ChargeIndicator ? 'true' : 'false');
//                            Writer.WriteEndElement(); // !ram:ChargeIndicator
//                            #endregion
//
//                            #region BasisAmount
//                            Writer.WriteStartElement('ram:BasisAmount', TZUGFeRDProfile: TZUGFeRDProfile.Extended); // not in XRechnung, according to CII-SR-123
//                            Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.BasisAmount, 2));
//                            Writer.WriteEndElement();
//                            #endregion
//
//                            #region ActualAmount
//                            Writer.WriteStartElement('ram:ActualAmount');
//                            Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ActualAmount, 2));
//                            Writer.WriteEndElement();
//                            #endregion
//
//                            _writeOptionalElementString(Writer, 'ram:Reason', tradeAllowanceCharge.Reason, TZUGFeRDProfile.Extended); // not in XRechnung according to CII-SR-128
//
//                            Writer.WriteEndElement(); // !AppliedTradeAllowanceCharge
//                        end;
//
//                        Writer.WriteEndElement(); // ram:GrossPriceProductTradePrice(Comfort|Extended|XRechnung)
//                    end;
//                    #endregion // !GrossPriceProductTradePrice(Comfort|Extended|XRechnung)
//
//                    #region NetPriceProductTradePrice
//                    //Im Nettopreis sind alle Zu- und Abschläge enthalten, jedoch nicht die Umsatzsteuer.
//                    Writer.WriteStartElement('ram:NetPriceProductTradePrice', TZUGFeRDProfile.Basic | TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung);
//                    _writeOptionalAmount(Writer, 'ram:ChargeAmount', tradeLineItem.NetUnitPrice, 4);
//
//                    if (tradeLineItem.UnitQuantity.HasValue)
//                    begin
//                        _writeElementWithAttribute(Writer, 'ram:BasisQuantity', 'unitCode', tradeLineItem.UnitCode.EnumToString(), _formatDecimal(tradeLineItem.UnitQuantity.Value, 4));
//                    end;
//                    Writer.WriteEndElement(); // ram:NetPriceProductTradePrice(Basic|Comfort|Extended|XRechnung)
//                    #endregion // !NetPriceProductTradePrice(Basic|Comfort|Extended|XRechnung)
//
//                    #region UltimateCustomerOrderReferencedDocument
//                    //ToDo: UltimateCustomerOrderReferencedDocument
//                    #endregion
//                    Writer.WriteEndElement(); // ram:SpecifiedLineTradeAgreement
//                end;
//                #endregion
//
//                #region SpecifiedLineTradeDelivery (Basic, Comfort, Extended)
//                Writer.WriteStartElement('ram:SpecifiedLineTradeDelivery', TZUGFeRDProfile.Basic | TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung);
//                _writeElementWithAttribute(Writer, 'ram:BilledQuantity', 'unitCode', tradeLineItem.UnitCode.EnumToString(), _formatDecimal(tradeLineItem.BilledQuantity, 4));
//
//                if (tradeLineItem.DeliveryNoteReferencedDocument <> nil)
//                begin
//                    Writer.WriteStartElement('ram:DeliveryNoteReferencedDocument', ALL_PROFILES ^ (TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung));
//                    if (!String.IsNullOrEmpty(tradeLineItem.DeliveryNoteReferencedDocument.ID))
//                    begin
//                        Writer.WriteElementString('ram:IssuerAssignedID', tradeLineItem.DeliveryNoteReferencedDocument.ID);
//                    end;
//
//                    if (tradeLineItem.DeliveryNoteReferencedDocument.IssueDateTime.HasValue)
//                    begin
//                        Writer.WriteStartElement('ram:FormattedIssueDateTime');
//                        Writer.WriteStartElement('qdt:DateTimeString');
//                        Writer.WriteAttributeString('format', '102');
//                        Writer.WriteValue(_formatDate(tradeLineItem.DeliveryNoteReferencedDocument.IssueDateTime.Value));
//                        Writer.WriteEndElement(); // 'qdt:DateTimeString
//                        Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//                    end;
//
//                    Writer.WriteEndElement(); // !ram:DeliveryNoteReferencedDocument
//                end;
//
//                if (tradeLineItem.ActualDeliveryDate.HasValue)
//                begin
//                    Writer.WriteStartElement('ram:ActualDeliverySupplyChainEvent');
//                    Writer.WriteStartElement('ram:OccurrenceDateTime');
//                    Writer.WriteStartElement('udt:DateTimeString');
//                    Writer.WriteAttributeString('format', '102');
//                    Writer.WriteValue(_formatDate(tradeLineItem.ActualDeliveryDate.Value));
//                    Writer.WriteEndElement(); // 'udt:DateTimeString
//                    Writer.WriteEndElement(); // !OccurrenceDateTime()
//                    Writer.WriteEndElement(); // !ActualDeliverySupplyChainEvent
//                end;
//
//                Writer.WriteEndElement(); // !ram:SpecifiedLineTradeDelivery
//                #endregion
//
//                #region SpecifiedLineTradeSettlement
//                Writer.WriteStartElement('ram:SpecifiedLineTradeSettlement', TZUGFeRDProfile.Basic | TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung);
//                #region ApplicableTradeTax
//                Writer.WriteStartElement('ram:ApplicableTradeTax', TZUGFeRDProfile.Basic | TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung);
//                Writer.WriteElementString('ram:TypeCode', tradeLineItem.TaxType.EnumToString());
//                if (!String.IsNullOrEmpty(_translateTaxCategoryCode(tradeLineItem.TaxCategoryCode)))
//                begin
//                    Writer.WriteElementString('ram:ExemptionReason', _translateTaxCategoryCode(tradeLineItem.TaxCategoryCode), TZUGFeRDProfile.Extended);
//                end;
//                Writer.WriteElementString('ram:CategoryCode', tradeLineItem.TaxCategoryCode.EnumToString()); // BT-151
//
//
//
//                if (tradeLineItem.TaxCategoryCode != TaxCategoryCodes.O) // notwendig, damit die Validierung klappt
//                begin
//                    Writer.WriteElementString('ram:RateApplicablePercent', _formatDecimal(tradeLineItem.TaxPercent));
//                end;
//
//                Writer.WriteEndElement(); // !ram:ApplicableTradeTax(Basic|Comfort|Extended|XRechnung)
//                #endregion // !ApplicableTradeTax(Basic|Comfort|Extended|XRechnung)
//
//                #region BillingSpecifiedPeriod
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
//                #endregion
//
//                #region SpecifiedTradeAllowanceCharge
//                //Abschläge auf Ebene der Rechnungsposition (Basic, Comfort, Extended)
//                //ToDo: SpecifiedTradeAllowanceCharge für Basic, Comfort und Extended
//                #endregion
//
//                #region SpecifiedTradeSettlementLineMonetarySummation (Basic, Comfort, Extended)
//                //Detailinformationen zu Positionssummen
//                Writer.WriteStartElement('ram:SpecifiedTradeSettlementLineMonetarySummation');
//                decimal _total = 0m;
//                if (tradeLineItem.LineTotalAmount.HasValue)
//                begin
//                    _total = tradeLineItem.LineTotalAmount.Value;
//                end;
//                else if (tradeLineItem.NetUnitPrice.HasValue)
//                begin
//                    _total = tradeLineItem.NetUnitPrice.Value * tradeLineItem.BilledQuantity;
//                end;
//
//                Writer.WriteStartElement('ram:LineTotalAmount', TZUGFeRDProfile.Basic | TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung);
//                Writer.WriteValue(_formatDecimal(_total));
//                Writer.WriteEndElement(); // !ram:LineTotalAmount
//
//                //ToDo: TotalAllowanceChargeAmount
//                //Gesamtbetrag der Positionszu- und Abschläge
//                Writer.WriteEndElement(); // ram:SpecifiedTradeSettlementMonetarySummation
//                #endregion
//
//                #region AdditionalReferencedDocument
//                //Objektkennung auf Ebene der Rechnungsposition
//                //ToDo: AdditionalReferencedDocument
//                #endregion
//
//                #region ReceivableSpecifiedTradeAccountingAccount
//                //Detailinformationen zur Buchungsreferenz
//                if ((descriptor.TZUGFeRDProfile == TZUGFeRDProfile.XRechnung1 || descriptor.TZUGFeRDProfile == TZUGFeRDProfile.XRechnung) && tradeLineItem.ReceivableSpecifiedTradeAccountingAccounts.Count > 0)
//                begin
//                    //only one ReceivableSpecifiedTradeAccountingAccount (BT-133) is allowed in TZUGFeRDProfile XRechnung
//                    Writer.WriteStartElement('ram:ReceivableSpecifiedTradeAccountingAccount', TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung);
//                    begin
//                        Writer.WriteStartElement('ram:ID');
//                        Writer.WriteValue(tradeLineItem.ReceivableSpecifiedTradeAccountingAccounts[0].TradeAccountID);  //BT-133
//                        Writer.WriteEndElement(); // !ram:ID
//                    end;
//                    Writer.WriteEndElement(); // !ram:ReceivableSpecifiedTradeAccountingAccount
//                end;
//                else
//                begin
//                    //multiple ReceivableSpecifiedTradeAccountingAccounts are allowed in other profiles
//                    foreach (ReceivableSpecifiedTradeAccountingAccount RSTA in tradeLineItem.ReceivableSpecifiedTradeAccountingAccounts)
//                    begin
//                        Writer.WriteStartElement('ram:ReceivableSpecifiedTradeAccountingAccount', TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended);
//
//                        begin
//                            Writer.WriteStartElement('ram:ID');
//                            Writer.WriteValue(RSTA.TradeAccountID);
//                            Writer.WriteEndElement(); // !ram:ID
//                        end;
//
//                        if (RSTA.TradeAccountTypeCode != AccountingAccountTypeCodes.Unknown)
//                        begin
//                            Writer.WriteStartElement('ram:TypeCode', TZUGFeRDProfile.Extended);
//                            Writer.WriteValue(((int)RSTA.TradeAccountTypeCode).ToString());
//                            Writer.WriteEndElement(); // !ram:TypeCode
//                        end;
//
//                        Writer.WriteEndElement(); // !ram:ReceivableSpecifiedTradeAccountingAccount
//                    end;
//                end;
//                #endregion
//
//                Writer.WriteEndElement(); // !ram:SpecifiedLineTradeSettlement
//                #endregion
//
//                Writer.WriteEndElement(); // !ram:IncludedSupplyChainTradeLineItem
//            end; // !foreach(tradeLineItem)
//            #endregion
//
//            #region ApplicableHeaderTradeAgreement
//            Writer.WriteStartElement('ram:ApplicableHeaderTradeAgreement');
//
//            // BT-10
//            if (!String.IsNullOrEmpty(this.Descriptor.ReferenceOrderNo))
//            begin
//                Writer.WriteElementString('ram:BuyerReference', this.Descriptor.ReferenceOrderNo);
//            end;
//
//            #region SellerTradeParty
//            // BT-31: this.Descriptor.SellerTaxRegistration
//            _writeOptionalParty(Writer, PartyTypes.SellerTradeParty, this.Descriptor.Seller, this.Descriptor.SellerContact, this.Descriptor.SellerElectronicAddress, this.Descriptor.SellerTaxRegistration);
//            #endregion
//
//            #region BuyerTradeParty
//            // BT-48: this.Descriptor.BuyerTaxRegistration
//            _writeOptionalParty(Writer, PartyTypes.BuyerTradeParty, this.Descriptor.Buyer, this.Descriptor.BuyerContact, this.Descriptor.BuyerElectronicAddress, this.Descriptor.BuyerTaxRegistration);
//            #endregion
//
//            // TODO: implement SellerTaxRepresentativeTradeParty
//            // BT-63: the tax registration of the SellerTaxRepresentativeTradeParty
//
//            #region BuyerOrderReferencedDocument
//            if (!String.IsNullOrEmpty(this.Descriptor.OrderNo))
//            begin
//                Writer.WriteStartElement('ram:BuyerOrderReferencedDocument');
//                Writer.WriteElementString('ram:IssuerAssignedID', this.Descriptor.OrderNo);
//                if (this.Descriptor.OrderDate.HasValue)
//                begin
//                    Writer.WriteStartElement('ram:FormattedIssueDateTime', ALL_PROFILES ^ (TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung));
//                    Writer.WriteStartElement('qdt:DateTimeString');
//                    Writer.WriteAttributeString('format', '102');
//                    Writer.WriteValue(_formatDate(this.Descriptor.OrderDate.Value));
//                    Writer.WriteEndElement(); // !qdt:DateTimeString
//                    Writer.WriteEndElement(); // !IssueDateTime()
//                end;
//
//                Writer.WriteEndElement(); // !BuyerOrderReferencedDocument
//            end;
//            #endregion
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
//            #region ContractReferencedDocument
//            // BT-12
//            if (this.Descriptor.ContractReferencedDocument <> nil)
//            begin
//                Writer.WriteStartElement('ram:ContractReferencedDocument');
//                Writer.WriteElementString('ram:IssuerAssignedID', this.Descriptor.ContractReferencedDocument.ID);
//                if (this.Descriptor.ContractReferencedDocument.IssueDateTime.HasValue)
//                begin
//                    Writer.WriteStartElement('ram:FormattedIssueDateTime', ALL_PROFILES ^ (TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung));
//                    Writer.WriteStartElement('qdt:DateTimeString');
//                    Writer.WriteAttributeString('format', '102');
//                    Writer.WriteValue(_formatDate(this.Descriptor.ContractReferencedDocument.IssueDateTime.Value));
//                    Writer.WriteEndElement(); // !qdt:DateTimeString
//                    Writer.WriteEndElement(); // !IssueDateTime()
//                end;
//
//                Writer.WriteEndElement(); // !ram:ContractReferencedDocument
//            end;
//            #endregion
//
//            #region AdditionalReferencedDocument
//            if (this.Descriptor.AdditionalReferencedDocuments <> nil)
//            begin
//                foreach (AdditionalReferencedDocument document in this.Descriptor.AdditionalReferencedDocuments)
//                begin
//                    Writer.WriteStartElement('ram:AdditionalReferencedDocument', TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung);
//                    Writer.WriteElementString('ram:IssuerAssignedID', document.ID);
//                    Writer.WriteElementString('ram:TypeCode', document.TypeCode.EnumValueToString());
//
//                    if (document.ReferenceTypeCode != ReferenceTypeCodes.Unknown)
//                    begin
//                        Writer.WriteElementString('ram:ReferenceTypeCode', document.ReferenceTypeCode.EnumToString());
//                    end;
//
//                    if (!String.IsNullOrEmpty(document.Name))
//                    begin
//                        Writer.WriteElementString('ram:Name', document.Name);
//                    end;
//
//                    if (document.AttachmentBinaryObject <> nil)
//                    begin
//                        Writer.WriteStartElement('ram:AttachmentBinaryObject');
//                        Writer.WriteAttributeString('filename', document.Filename);
//                        Writer.WriteAttributeString('mimeCode', MimeTypeMapper.GetMimeType(document.Filename));
//                        Writer.WriteValue(Convert.ToBase64String(document.AttachmentBinaryObject));
//                        Writer.WriteEndElement(); // !AttachmentBinaryObject()
//                    end;
//
//                    if (document.IssueDateTime.HasValue)
//                    begin
//                        Writer.WriteStartElement('ram:FormattedIssueDateTime');
//                        Writer.WriteStartElement('qdt:DateTimeString');
//                        Writer.WriteAttributeString('format', '102');
//                        Writer.WriteValue(_formatDate(document.IssueDateTime.Value));
//                        Writer.WriteEndElement(); // !qdt:DateTimeString
//                        Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//                    end;
//
//                    Writer.WriteEndElement(); // !ram:AdditionalReferencedDocument
//                end;
//            end;
//            #endregion
//
//            #region SpecifiedProcuringProject
//            if (Descriptor.SpecifiedProcuringProject <> nil)
//            begin
//
//                Writer.WriteStartElement('ram:SpecifiedProcuringProject', TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung);
//                Writer.WriteElementString('ram:ID', Descriptor.SpecifiedProcuringProject.ID, TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung);
//                Writer.WriteElementString('ram:Name', Descriptor.SpecifiedProcuringProject.Name, TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung);
//                Writer.WriteEndElement(); // !ram:SpecifiedProcuringProject
//            end;
//            #endregion
//
//            Writer.WriteEndElement(); // !ApplicableHeaderTradeAgreement
//            #endregion
//
//            #region ApplicableHeaderTradeDelivery
//            Writer.WriteStartElement('ram:ApplicableHeaderTradeDelivery'); // Pflichteintrag
//            _writeOptionalParty(Writer, PartyTypes.ShipToTradeParty, this.Descriptor.ShipTo);
//            //ToDo: UltimateShipToTradeParty
//            _writeOptionalParty(Writer, PartyTypes.ShipFromTradeParty, this.Descriptor.ShipFrom); // ShipFrom shall not be written in XRechnung profiles
//
//            #region ActualDeliverySupplyChainEvent
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
//            #endregion
//
//            #region DeliveryNoteReferencedDocument
//            if (this.Descriptor.DeliveryNoteReferencedDocument <> nil)
//            begin
//                Writer.WriteStartElement('ram:DeliveryNoteReferencedDocument');
//                Writer.WriteElementString('ram:IssuerAssignedID', this.Descriptor.DeliveryNoteReferencedDocument.ID);
//
//                if (this.Descriptor.DeliveryNoteReferencedDocument.IssueDateTime.HasValue)
//                begin
//                    Writer.WriteStartElement('ram:FormattedIssueDateTime', ALL_PROFILES ^ (TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung));
//                    Writer.WriteStartElement('qdt:DateTimeString');
//                    Writer.WriteAttributeString('format', '102');
//                    Writer.WriteValue(_formatDate(this.Descriptor.DeliveryNoteReferencedDocument.IssueDateTime.Value));
//                    Writer.WriteEndElement(); // 'qdt:DateTimeString
//                    Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//                end;
//
//                Writer.WriteEndElement(); // !DeliveryNoteReferencedDocument
//            end;
//            #endregion
//
//            Writer.WriteEndElement(); // !ApplicableHeaderTradeDelivery
//            #endregion
//
//            #region ApplicableHeaderTradeSettlement
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
//            _writeOptionalParty(Writer, PartyTypes.InvoiceeTradeParty, this.Descriptor.Invoicee);
//
//            //   8. PayeeTradeParty (optional)
//            _writeOptionalParty(Writer, PartyTypes.PayeeTradeParty, this.Descriptor.Payee);
//
//            #region SpecifiedTradeSettlementPaymentMeans
//            //  10. SpecifiedTradeSettlementPaymentMeans (optional)
//
//            if (this.Descriptor.CreditorBankAccounts.Count == 0 && this.Descriptor.DebitorBankAccounts.Count == 0)
//            begin
//                if (this.Descriptor.PaymentMeans <> nil)
//                begin
//
//                    if ((this.Descriptor.PaymentMeans <> nil) && (this.Descriptor.PaymentMeans.TypeCode != PaymentMeansTypeCodes.Unknown))
//                    begin
//                        Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans', TZUGFeRDProfile.BasicWL | TZUGFeRDProfile.Basic | TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung);
//                        Writer.WriteElementString('ram:TypeCode', this.Descriptor.PaymentMeans.TypeCode.EnumToString());
//                        Writer.WriteElementString('ram:Information', this.Descriptor.PaymentMeans.Information);
//
//                        if (this.Descriptor.PaymentMeans.FinancialCard <> nil)
//                        begin
//                            Writer.WriteStartElement('ram:ApplicableTradeSettlementFinancialCard', TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended);
//                            _writeOptionalElementString(Writer, 'ram:ID', Descriptor.PaymentMeans.FinancialCard.Id);
//                            _writeOptionalElementString(Writer, 'ram:CardholderName', Descriptor.PaymentMeans.FinancialCard.CardholderName);
//                            Writer.WriteEndElement(); // !ram:ApplicableTradeSettlementFinancialCard
//                        end;
//                        Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
//                    end;
//                end;
//            end;
//            else
//            begin
//                foreach (BankAccount account in this.Descriptor.CreditorBankAccounts)
//                begin
//                    Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans');
//
//                    if ((this.Descriptor.PaymentMeans <> nil) && (this.Descriptor.PaymentMeans.TypeCode != PaymentMeansTypeCodes.Unknown))
//                    begin
//                        Writer.WriteElementString('ram:TypeCode', this.Descriptor.PaymentMeans.TypeCode.EnumToString());
//                        Writer.WriteElementString('ram:Information', this.Descriptor.PaymentMeans.Information);
//
//                        if (this.Descriptor.PaymentMeans.FinancialCard <> nil)
//                        begin
//                            Writer.WriteStartElement('ram:ApplicableTradeSettlementFinancialCard', TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended);
//                            _writeOptionalElementString(Writer, 'ram:ID', Descriptor.PaymentMeans.FinancialCard.Id);
//                            _writeOptionalElementString(Writer, 'ram:CardholderName', Descriptor.PaymentMeans.FinancialCard.CardholderName);
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
//                    if (!String.IsNullOrEmpty(account.BIC))
//                    begin
//                        Writer.WriteStartElement('ram:PayeeSpecifiedCreditorFinancialInstitution');
//                        Writer.WriteElementString('ram:BICID', account.BIC);
//                        Writer.WriteEndElement(); // !PayeeSpecifiedCreditorFinancialInstitution
//                    end;
//
//                    Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
//                end;
//
//                foreach (BankAccount account in this.Descriptor.DebitorBankAccounts)
//                begin
//                    Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans');
//
//                    if ((this.Descriptor.PaymentMeans <> nil) && (this.Descriptor.PaymentMeans.TypeCode != PaymentMeansTypeCodes.Unknown))
//                    begin
//                        Writer.WriteElementString('ram:TypeCode', this.Descriptor.PaymentMeans.TypeCode.EnumToString());
//                        Writer.WriteElementString('ram:Information', this.Descriptor.PaymentMeans.Information);
//                    end;
//
//                    Writer.WriteStartElement('ram:PayerPartyDebtorFinancialAccount');
//                    Writer.WriteElementString('ram:IBANID', account.IBAN);
//                    if (!String.IsNullOrEmpty(account.Name))
//                    begin
//                        Writer.WriteElementString('ram:AccountName', account.Name);
//                    end;
//                    if (!String.IsNullOrEmpty(account.ID))
//                    begin
//                        Writer.WriteElementString('ram:ProprietaryID', account.ID);
//                    end;
//                    Writer.WriteEndElement(); // !PayerPartyDebtorFinancialAccount
//
//                    if (!String.IsNullOrEmpty(account.BIC))
//                    begin
//                        Writer.WriteStartElement('ram:PayerSpecifiedDebtorFinancialInstitution');
//                        Writer.WriteElementString('ram:BICID', account.BIC);
//                        Writer.WriteEndElement(); // !PayerSpecifiedDebtorFinancialInstitution
//                    end;
//
//                    Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
//                end;
//            end;
//            #endregion
//
//            #region ApplicableTradeTax
//            //  11. ApplicableTradeTax (optional)
//            _writeOptionalTaxes(Writer);
//            #endregion
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
//            if ((this.Descriptor.TradeAllowanceCharges <> nil) && (this.Descriptor.TradeAllowanceCharges.Count > 0))
//            begin
//                foreach (TradeAllowanceCharge tradeAllowanceCharge in this.Descriptor.TradeAllowanceCharges)
//                begin
//                    Writer.WriteStartElement('ram:SpecifiedTradeAllowanceCharge');
//                    Writer.WriteStartElement('ram:ChargeIndicator');
//                    Writer.WriteElementString('udt:Indicator', tradeAllowanceCharge.ChargeIndicator ? 'true' : 'false');
//                    Writer.WriteEndElement(); // !ram:ChargeIndicator
//
//                    Writer.WriteStartElement('ram:BasisAmount', TZUGFeRDProfile: TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung);
//                    Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.BasisAmount));
//                    Writer.WriteEndElement();
//
//                    Writer.WriteStartElement('ram:ActualAmount');
//                    Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ActualAmount, 2));
//                    Writer.WriteEndElement();
//
//
//                    _writeOptionalElementString(Writer, 'ram:Reason', tradeAllowanceCharge.Reason);
//
//                    if (tradeAllowanceCharge.Tax <> nil)
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
//            if ((this.Descriptor.ServiceCharges <> nil) && (this.Descriptor.ServiceCharges.Count > 0))
//            begin
//                foreach (ServiceCharge serviceCharge in this.Descriptor.ServiceCharges)
//                begin
//                    Writer.WriteStartElement('ram:SpecifiedLogisticsServiceCharge', ALL_PROFILES ^ (TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung));
//                    if (!String.IsNullOrEmpty(serviceCharge.Description))
//                    begin
//                        Writer.WriteElementString('ram:Description', serviceCharge.Description);
//                    end;
//                    Writer.WriteElementString('ram:AppliedAmount', _formatDecimal(serviceCharge.Amount));
//                    if (serviceCharge.Tax <> nil)
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
//            if (this.Descriptor.PaymentTerms <> nil || !string.IsNullOrEmpty(Descriptor.PaymentMeans?.SEPAMandateReference))
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
//            #region SpecifiedTradeSettlementHeaderMonetarySummation
//            //Gesamtsummen auf Dokumentenebene
//            Writer.WriteStartElement('ram:SpecifiedTradeSettlementHeaderMonetarySummation');
//            _writeOptionalAmount(Writer, 'ram:LineTotalAmount', this.Descriptor.LineTotalAmount);                                  // Summe der Nettobeträge aller Rechnungspositionen
//            _writeOptionalAmount(Writer, 'ram:ChargeTotalAmount', this.Descriptor.ChargeTotalAmount);                              // Summe der Zuschläge auf Dokumentenebene
//            _writeOptionalAmount(Writer, 'ram:AllowanceTotalAmount', this.Descriptor.AllowanceTotalAmount);                        // Summe der Abschläge auf Dokumentenebene
//
//            if (this.Descriptor.TZUGFeRDProfile == TZUGFeRDProfile.Extended)
//            begin
//                // there shall be no currency for tax basis total amount, see
//                // https://github.com/stephanstapel/ZUGFeRD-csharp/issues/56#issuecomment-655525467
//                _writeOptionalAmount(Writer, 'ram:TaxBasisTotalAmount', this.Descriptor.TaxBasisAmount, forceCurrency: false);   // Rechnungsgesamtbetrag ohne Umsatzsteuer
//            end;
//            else
//            begin
//                _writeOptionalAmount(Writer, 'ram:TaxBasisTotalAmount', this.Descriptor.TaxBasisAmount);   // Rechnungsgesamtbetrag ohne Umsatzsteuer
//            end;
//            _writeOptionalAmount(Writer, 'ram:TaxTotalAmount', this.Descriptor.TaxTotalAmount, forceCurrency: true);               // Gesamtbetrag der Rechnungsumsatzsteuer, Steuergesamtbetrag in Buchungswährung
//            _writeOptionalAmount(Writer, 'ram:RoundingAmount', this.Descriptor.RoundingAmount, TZUGFeRDProfile: TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended | TZUGFeRDProfile.XRechnung1 | TZUGFeRDProfile.XRechnung);  // RoundingAmount  //Rundungsbetrag
//            _writeOptionalAmount(Writer, 'ram:GrandTotalAmount', this.Descriptor.GrandTotalAmount);                                // Rechnungsgesamtbetrag einschließlich Umsatzsteuer
//            _writeOptionalAmount(Writer, 'ram:TotalPrepaidAmount', this.Descriptor.TotalPrepaidAmount);                            // Vorauszahlungsbetrag
//            _writeOptionalAmount(Writer, 'ram:DuePayableAmount', this.Descriptor.DuePayableAmount);                                // Fälliger Zahlungsbetrag
//            Writer.WriteEndElement(); // !ram:SpecifiedTradeSettlementMonetarySummation
//            #endregion
//
//            #region InvoiceReferencedDocument
//            if (this.Descriptor.InvoiceReferencedDocument <> nil)
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
//            #region ReceivableSpecifiedTradeAccountingAccount
//            if (this.Descriptor.ReceivableSpecifiedTradeAccountingAccounts <> nil && this.Descriptor.ReceivableSpecifiedTradeAccountingAccounts.Count > 0)
//            begin
//                if (descriptor.TZUGFeRDProfile == TZUGFeRDProfile.XRechnung1 || descriptor.TZUGFeRDProfile == TZUGFeRDProfile.XRechnung)
//                begin
//                    if (!string.IsNullOrEmpty(this.Descriptor.ReceivableSpecifiedTradeAccountingAccounts[0].TradeAccountID))
//                    begin
//                        Writer.WriteStartElement('ram:ReceivableSpecifiedTradeAccountingAccount');
//                        begin
//                            //BT-19
//                            Writer.WriteStartElement('ram:ID');
//                            Writer.WriteValue(this.Descriptor.ReceivableSpecifiedTradeAccountingAccounts[0].TradeAccountID);
//                            Writer.WriteEndElement(); // !ram:ID
//                        end;
//                        Writer.WriteEndElement(); // !ram:ReceivableSpecifiedTradeAccountingAccount
//                    end;
//                end;
//                else
//                begin
//                    foreach (ReceivableSpecifiedTradeAccountingAccount RSTAA in this.Descriptor.ReceivableSpecifiedTradeAccountingAccounts)
//                    begin
//                        Writer.WriteStartElement('ram:ReceivableSpecifiedTradeAccountingAccount', TZUGFeRDProfile.BasicWL | TZUGFeRDProfile.Basic | TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended);
//
//                        begin
//                            //BT-19
//                            Writer.WriteStartElement('ram:ID', TZUGFeRDProfile.BasicWL | TZUGFeRDProfile.Basic | TZUGFeRDProfile.Comfort | TZUGFeRDProfile.Extended);
//                            Writer.WriteValue(RSTAA.TradeAccountID);
//                            Writer.WriteEndElement(); // !ram:ID
//                        end;
//
//                        if (RSTAA.TradeAccountTypeCode != AccountingAccountTypeCodes.Unknown)
//                        begin
//                            Writer.WriteStartElement('ram:TypeCode', TZUGFeRDProfile.Extended);
//                            Writer.WriteValue(((int)RSTAA.TradeAccountTypeCode).ToString());
//                            Writer.WriteEndElement(); // !ram:TypeCode
//                        end;
//
//                        Writer.WriteEndElement(); // !ram:ReceivableSpecifiedTradeAccountingAccount
//                    end;
//                end;
//            end;
//            #endregion
//            Writer.WriteEndElement(); // !ram:ApplicableHeaderTradeSettlement
//
//            #endregion
//
//            Writer.WriteEndElement(); // !ram:SpecifiedSupplyChainTradeTransaction
//            #endregion
//
  Writer.WriteEndElement(); // !ram:Invoice
  Writer.WriteEndDocument();
  Writer.Flush();

  stream.Seek(streamPosition, soFromBeginning);
end;

function TZUGFeRDInvoiceDescriptor21Writer.Validate(
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
//            if ((descriptor.TZUGFeRDProfile == TZUGFeRDProfile.XRechnung) || (descriptor.TZUGFeRDProfile == TZUGFeRDProfile.XRechnung1))
//            begin
//                if (descriptor.Seller <> nil)
//                begin
//                    if (descriptor.SellerContact == null)
//                    begin
//                        if (throwExceptions) begin throw new MissingDataException("Seller contact (BG-6) required when seller is set (BR-DE-2)."); end;
//                        return false;
//                    end;
//                    else
//                    begin
//                        if (String.IsNullOrWhiteSpace(descriptor.SellerContact.EmailAddress))
//                        begin
//                            if (throwExceptions) begin throw new MissingDataException("Seller contact email address (BT-43) is required (BR-DE-7)."); end;
//                            return false;
//                        end;
//                        if (String.IsNullOrWhiteSpace(descriptor.SellerContact.PhoneNo))
//                        begin
//                            if (throwExceptions) begin throw new MissingDataException("Seller contact phone no (BT-42) is required (BR-DE-6)."); end;
//                            return false;
//                        end;
//                        if (String.IsNullOrWhiteSpace(descriptor.SellerContact.Name) && String.IsNullOrWhiteSpace(descriptor.SellerContact.OrgUnit))
//                        begin
//                            if (throwExceptions) begin throw new MissingDataException("Seller contact point (name or org unit) no (BT-41) is required (BR-DE-5)."); end;
//                            return false;
//                        end;
//                    end;
//                end;
//
//
//                // BR-DE-17
//                if (!new[] begin InvoiceType.PartialInvoice, InvoiceType.Invoice, InvoiceType.Correction, InvoiceType.SelfBilledInvoice, InvoiceType.CreditNote,
//                             InvoiceType.PartialConstructionInvoice, InvoiceType.PartialFinalConstructionInvoice, InvoiceType.FinalConstructionInvoiceend;.Contains(descriptor.Type))
//                begin
//                    throw new UnsupportedException("Invoice type (BT-3) does not match requirements of BR-DE-17");
//                end;
//            end;
//

  Result := true;
end;

procedure TZUGFeRDInvoiceDescriptor21Writer._writeOptionalAmount(
  _writer: TZUGFeRDProfileAwareXmlTextWriter; tagName: string;
  value: TZUGFeRDNullable<Currency>;
  numDecimals: Integer; forceCurrency: Boolean; Profile : TZUGFeRDProfiles);
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
