﻿{* Licensed to the Apache Software Foundation (ASF) under one
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
  System.Classes,
  intf.ZUGFeRDInvoiceDescriptor
  ,intf.ZUGFeRDProfileAwareXmlTextWriter
  ,intf.ZUGFeRDInvoiceDescriptorWriter
  ,intf.ZUGFeRDExceptions
  ;

type
  TZUGFeRDInvoiceDescriptor20Writer = class(TZUGFeRDInvoiceDescriptorWriter)
  private
    FWriter: TZUGFeRDProfileAwareXmlTextWriter;
    FDescriptor: TZUGFeRDInvoiceDescriptor;
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
begin
  if (stream = nil) then
    raise TZUGFeRDIllegalStreamException.Create('Cannot write to stream');
//
//            // write data
//            long streamPosition = stream.Position;
//
//            this.Descriptor = descriptor;
//            this.Writer = new ProfileAwareXmlTextWriter(stream, Encoding.UTF8, descriptor.Profile);
//            Writer.Formatting = Formatting.Indented;
//            Writer.WriteStartDocument();
//
//            #region Kopfbereich
//            Writer.WriteStartElement("rsm:CrossIndustryInvoice");
//            Writer.WriteAttributeString("xmlns", "a", null, "urn:un:unece:uncefact:data:standard:QualifiedDataType:100");
//            Writer.WriteAttributeString("xmlns", "rsm", null, "urn:un:unece:uncefact:data:standard:CrossIndustryInvoice:100");
//            Writer.WriteAttributeString("xmlns", "qdt", null, "urn:un:unece:uncefact:data:standard:QualifiedDataType:100");
//            Writer.WriteAttributeString("xmlns", "ram", null, "urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:100");
//            Writer.WriteAttributeString("xmlns", "xs", null, "http://www.w3.org/2001/XMLSchema");
//            Writer.WriteAttributeString("xmlns", "udt", null, "urn:un:unece:uncefact:data:standard:UnqualifiedDataType:100");
//            #endregion
//
//            #region SpecifiedExchangedDocumentContext
//            Writer.WriteStartElement("rsm:ExchangedDocumentContext");
//
//            if (Descriptor.IsTest)
//            {
//                Writer.WriteStartElement("ram:TestIndicator");
//                Writer.WriteElementString("udt:Indicator", "true");
//                Writer.WriteEndElement(); // !ram:TestIndicator
//            }
//
//            if (!String.IsNullOrEmpty(this.Descriptor.BusinessProcess))
//            {
//	            Writer.WriteStartElement("ram:BusinessProcessSpecifiedDocumentContextParameter");
//	            Writer.WriteElementString("ram:ID", this.Descriptor.BusinessProcess);
//	            Writer.WriteEndElement(); // !ram:BusinessProcessSpecifiedDocumentContextParameter
//            }
//
//            Writer.WriteStartElement("ram:GuidelineSpecifiedDocumentContextParameter");
//            Writer.WriteElementString("ram:ID", this.Descriptor.Profile.EnumToString(ZUGFeRDVersion.Version20));
//            Writer.WriteEndElement(); // !ram:GuidelineSpecifiedDocumentContextParameter
//            Writer.WriteEndElement(); // !rsm:ExchangedDocumentContext
//
//            Writer.WriteStartElement("rsm:ExchangedDocument");
//            Writer.WriteElementString("ram:ID", this.Descriptor.InvoiceNo);
//            Writer.WriteElementString("ram:Name", _translateInvoiceType(this.Descriptor.Type), Profile.Extended);
//            Writer.WriteElementString("ram:TypeCode", String.Format("{0}", _encodeInvoiceType(this.Descriptor.Type)));
//
//            if (this.Descriptor.InvoiceDate.HasValue)
//            {
//                Writer.WriteStartElement("ram:IssueDateTime");
//                Writer.WriteStartElement("udt:DateTimeString");
//                Writer.WriteAttributeString("format", "102");
//                Writer.WriteValue(_formatDate(this.Descriptor.InvoiceDate.Value));
//                Writer.WriteEndElement(); // !udt:DateTimeString
//                Writer.WriteEndElement(); // !IssueDateTime
//            }
//            _writeNotes(Writer, this.Descriptor.Notes);
//            Writer.WriteEndElement(); // !rsm:ExchangedDocument
//            #endregion
//
//            /*
//             * @todo continue here to adopt v2 tag names
//             */
//
//            #region SpecifiedSupplyChainTradeTransaction
//            Writer.WriteStartElement("rsm:SupplyChainTradeTransaction");
//
//            foreach (TradeLineItem tradeLineItem in this.Descriptor.TradeLineItems)
//            {
//                Writer.WriteStartElement("ram:IncludedSupplyChainTradeLineItem");
//
//                if (tradeLineItem.AssociatedDocument != null)
//                {
//                    Writer.WriteStartElement("ram:AssociatedDocumentLineDocument");
//                    if (!String.IsNullOrEmpty(tradeLineItem.AssociatedDocument.LineID))
//                    {
//                        Writer.WriteElementString("ram:LineID", tradeLineItem.AssociatedDocument.LineID);
//                    }
//                    _writeNotes(Writer, tradeLineItem.AssociatedDocument.Notes);
//                    Writer.WriteEndElement(); // ram:AssociatedDocumentLineDocument
//                }
//
//                // handelt es sich um einen Kommentar?
//                if ((tradeLineItem.AssociatedDocument?.Notes.Count > 0) && (tradeLineItem.BilledQuantity == 0) && (String.IsNullOrEmpty(tradeLineItem.Description)))
//                {
//                    Writer.WriteEndElement(); // !ram:IncludedSupplyChainTradeLineItem
//                    continue;
//                }
//
//                Writer.WriteStartElement("ram:SpecifiedTradeProduct");
//                if ((tradeLineItem.GlobalID != null) && (tradeLineItem.GlobalID.SchemeID != GlobalIDSchemeIdentifiers.Unknown) && !String.IsNullOrEmpty(tradeLineItem.GlobalID.ID))
//                {
//                    _writeElementWithAttribute(Writer, "ram:GlobalID", "schemeID", tradeLineItem.GlobalID.SchemeID.EnumToString(), tradeLineItem.GlobalID.ID);
//                }
//
//                _writeOptionalElementString(Writer, "ram:SellerAssignedID", tradeLineItem.SellerAssignedID);
//                _writeOptionalElementString(Writer, "ram:BuyerAssignedID", tradeLineItem.BuyerAssignedID);
//                _writeOptionalElementString(Writer, "ram:Name", tradeLineItem.Name);
//                _writeOptionalElementString(Writer, "ram:Description", tradeLineItem.Description);
//
//                if (tradeLineItem.ApplicableProductCharacteristics != null && tradeLineItem.ApplicableProductCharacteristics.Any())
//                {
//                    foreach (var productCharacteristic in tradeLineItem.ApplicableProductCharacteristics)
//                    {
//                        Writer.WriteStartElement("ram:ApplicableProductCharacteristic");
//                        _writeOptionalElementString(Writer, "ram:Description", productCharacteristic.Description);
//                        _writeOptionalElementString(Writer, "ram:Value", productCharacteristic.Value);
//                        Writer.WriteEndElement(); // !ram:ApplicableProductCharacteristic
//                    }
//                }
//
//                Writer.WriteEndElement(); // !ram:SpecifiedTradeProduct
//
//                Writer.WriteStartElement("ram:SpecifiedLineTradeAgreement", Profile.Basic | Profile.Comfort | Profile.Extended);
//
//                if (tradeLineItem.BuyerOrderReferencedDocument != null)
//                {
//                    Writer.WriteStartElement("ram:BuyerOrderReferencedDocument", Profile.Comfort | Profile.Extended | Profile.XRechnung1 | Profile.XRechnung);
//
//                    #region IssuerAssignedID
//                    //Bestellnummer
//                    if (!String.IsNullOrEmpty(tradeLineItem.BuyerOrderReferencedDocument.ID))
//                    {
//                        Writer.WriteElementString("ram:IssuerAssignedID", tradeLineItem.BuyerOrderReferencedDocument.ID);
//                    }
//                    #endregion
//
//                    #region LineID
//                    //Referenz zur Bestellposition
//                    //ToDo: fehlt ganz
//                    #endregion
//
//                    #region IssueDateTime
//                    if (tradeLineItem.BuyerOrderReferencedDocument.IssueDateTime.HasValue)
//                    {
//                        Writer.WriteStartElement("ram:FormattedIssueDateTime");
//                        Writer.WriteStartElement("qdt:DateTimeString");
//                        Writer.WriteAttributeString("format", "102");
//                        Writer.WriteValue(_formatDate(tradeLineItem.BuyerOrderReferencedDocument.IssueDateTime.Value));
//                        Writer.WriteEndElement(); // !qdt:DateTimeString
//                        Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//                    }
//                    #endregion
//
//                    Writer.WriteEndElement(); // !ram:BuyerOrderReferencedDocument
//                }
//
//                if (tradeLineItem.ContractReferencedDocument != null)
//                {
//                    Writer.WriteStartElement("ram:ContractReferencedDocument", Profile.Extended);
//                    if (tradeLineItem.ContractReferencedDocument.IssueDateTime.HasValue)
//                    {
//                        Writer.WriteStartElement("ram:FormattedIssueDateTime");
//                        Writer.WriteStartElement("qdt:DateTimeString");
//                        Writer.WriteAttributeString("format", "102");
//                        Writer.WriteValue(_formatDate(tradeLineItem.ContractReferencedDocument.IssueDateTime.Value));
//                        Writer.WriteEndElement(); // !udt:DateTimeString
//                        Writer.WriteEndElement(); // !ram:IssueDateTime
//                    }
//                    if (!String.IsNullOrEmpty(tradeLineItem.ContractReferencedDocument.ID))
//                    {
//                        Writer.WriteElementString("ram:IssuerAssignedID", tradeLineItem.ContractReferencedDocument.ID);
//                    }
//
//                    Writer.WriteEndElement(); // !ram:ContractReferencedDocument(Extended)
//                }
//
//                if (tradeLineItem.AdditionalReferencedDocuments != null)
//                {
//                    foreach (AdditionalReferencedDocument document in tradeLineItem.AdditionalReferencedDocuments)
//                    {
//                        Writer.WriteStartElement("ram:AdditionalReferencedDocument");
//                        if (document.IssueDateTime.HasValue)
//                        {
//                            Writer.WriteStartElement("ram:FormattedIssueDateTime");
//                            Writer.WriteStartElement("qdt:DateTimeString");
//                            Writer.WriteValue(_formatDate(document.IssueDateTime.Value));
//                            Writer.WriteEndElement(); // !qdt:DateTimeString
//                            Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//                        }
//
//                        Writer.WriteElementString("ram:LineID", String.Format("{0}", tradeLineItem.AssociatedDocument?.LineID));
//
//                        if (!String.IsNullOrEmpty(document.ID))
//                        {
//                            Writer.WriteElementString("ram:IssuerAssignedID", document.ID);
//                        }
//
//                        Writer.WriteElementString("ram:ReferenceTypeCode", document.ReferenceTypeCode.EnumToString());
//
//                        Writer.WriteEndElement(); // !ram:AdditionalReferencedDocument
//                    } // !foreach(document)
//                }
//
//                Writer.WriteStartElement("ram:GrossPriceProductTradePrice");
//                _writeOptionalAmount(Writer, "ram:ChargeAmount", tradeLineItem.GrossUnitPrice, numDecimals: 4, forceCurrency: false);
//                if (tradeLineItem.UnitQuantity.HasValue)
//                {
//                    _writeElementWithAttribute(Writer, "ram:BasisQuantity", "unitCode", tradeLineItem.UnitCode.EnumToString(), _formatDecimal(tradeLineItem.UnitQuantity.Value, 4));
//                }
//
//                foreach (TradeAllowanceCharge tradeAllowanceCharge in tradeLineItem.TradeAllowanceCharges)
//                {
//                    Writer.WriteStartElement("ram:AppliedTradeAllowanceCharge");
//
//                    Writer.WriteStartElement("ram:ChargeIndicator");
//                    Writer.WriteElementString("udt:Indicator", tradeAllowanceCharge.ChargeIndicator ? "true" : "false");
//                    Writer.WriteEndElement(); // !ram:ChargeIndicator
//
//                    Writer.WriteStartElement("ram:BasisAmount");
//                    Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.BasisAmount, 4));
//                    Writer.WriteEndElement();
//                    Writer.WriteStartElement("ram:ActualAmount");
//                    Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ActualAmount, 4));
//                    Writer.WriteEndElement();
//
//                    _writeOptionalElementString(Writer, "ram:Reason", tradeAllowanceCharge.Reason);
//
//                    Writer.WriteEndElement(); // !AppliedTradeAllowanceCharge
//                }
//
//                Writer.WriteEndElement(); // ram:GrossPriceProductTradePrice
//
//                Writer.WriteStartElement("ram:NetPriceProductTradePrice");
//                _writeOptionalAmount(Writer, "ram:ChargeAmount", tradeLineItem.NetUnitPrice, numDecimals: 4, forceCurrency: false);
//
//                if (tradeLineItem.UnitQuantity.HasValue)
//                {
//                    _writeElementWithAttribute(Writer, "ram:BasisQuantity", "unitCode", tradeLineItem.UnitCode.EnumToString(), _formatDecimal(tradeLineItem.UnitQuantity.Value, 4));
//                }
//                Writer.WriteEndElement(); // ram:NetPriceProductTradePrice
//
//                Writer.WriteEndElement(); // !ram:SpecifiedLineTradeAgreement
//
//                if (Descriptor.Profile != Profile.Basic)
//                {
//                    Writer.WriteStartElement("ram:SpecifiedLineTradeDelivery");
//                    _writeElementWithAttribute(Writer, "ram:BilledQuantity", "unitCode", tradeLineItem.UnitCode.EnumToString(), _formatDecimal(tradeLineItem.BilledQuantity, 4));
//
//                    if (tradeLineItem.DeliveryNoteReferencedDocument != null)
//                    {
//                        Writer.WriteStartElement("ram:DeliveryNoteReferencedDocument");
//                        if (tradeLineItem.DeliveryNoteReferencedDocument.IssueDateTime.HasValue)
//                        {
//                            //Old path of Version 1.0
//                            //Writer.WriteStartElement("ram:IssueDateTime");
//                            //Writer.WriteValue(_formatDate(tradeLineItem.DeliveryNoteReferencedDocument.IssueDateTime.Value));
//                            //Writer.WriteEndElement(); // !ram:IssueDateTime
//
//                            Writer.WriteStartElement("ram:FormattedIssueDateTime");
//                            Writer.WriteStartElement("qdt:DateTimeString");
//                            Writer.WriteAttributeString("format", "102");
//                            Writer.WriteValue(_formatDate(this.Descriptor.OrderDate.Value));
//                            Writer.WriteEndElement(); // !qdt:DateTimeString
//                            Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//                        }
//
//                        if (!String.IsNullOrEmpty(tradeLineItem.DeliveryNoteReferencedDocument.ID))
//                        {
//                            Writer.WriteElementString("ram:IssuerAssignedID", tradeLineItem.DeliveryNoteReferencedDocument.ID);
//                        }
//
//                        Writer.WriteEndElement(); // !ram:DeliveryNoteReferencedDocument
//                    }
//
//                    if (tradeLineItem.ActualDeliveryDate.HasValue)
//                    {
//                        Writer.WriteStartElement("ram:ActualDeliverySupplyChainEvent");
//                        Writer.WriteStartElement("ram:OccurrenceDateTime");
//                        Writer.WriteStartElement("udt:DateTimeString");
//                        Writer.WriteAttributeString("format", "102");
//                        Writer.WriteValue(_formatDate(tradeLineItem.ActualDeliveryDate.Value));
//                        Writer.WriteEndElement(); // "udt:DateTimeString
//                        Writer.WriteEndElement(); // !OccurrenceDateTime()
//                        Writer.WriteEndElement(); // !ActualDeliverySupplyChainEvent
//                    }
//
//                    Writer.WriteEndElement(); // !ram:SpecifiedLineTradeDelivery
//                }
//                else
//                {
//                    Writer.WriteStartElement("ram:SpecifiedLineTradeDelivery");
//                    _writeElementWithAttribute(Writer, "ram:BilledQuantity", "unitCode", tradeLineItem.UnitCode.EnumToString(), _formatDecimal(tradeLineItem.BilledQuantity, 4));
//                    Writer.WriteEndElement(); // !ram:SpecifiedLineTradeDelivery
//                }
//
//                Writer.WriteStartElement("ram:SpecifiedLineTradeSettlement");
//
//                Writer.WriteStartElement("ram:ApplicableTradeTax", Profile.Basic | Profile.Comfort | Profile.Extended);
//                Writer.WriteElementString("ram:TypeCode", tradeLineItem.TaxType.EnumToString());
//                Writer.WriteElementString("ram:CategoryCode", tradeLineItem.TaxCategoryCode.EnumToString());
//                Writer.WriteElementString("ram:RateApplicablePercent", _formatDecimal(tradeLineItem.TaxPercent));
//                Writer.WriteEndElement(); // !ram:ApplicableTradeTax
//
//                if (tradeLineItem.BillingPeriodStart.HasValue || tradeLineItem.BillingPeriodEnd.HasValue)
//                {
//                    Writer.WriteStartElement("ram:BillingSpecifiedPeriod", Profile.BasicWL | Profile.Basic | Profile.Comfort | Profile.Extended | Profile.XRechnung1 | Profile.XRechnung);
//                    if (tradeLineItem.BillingPeriodStart.HasValue)
//                    {
//                        Writer.WriteStartElement("ram:StartDateTime");
//                        _writeElementWithAttribute(Writer, "udt:DateTimeString", "format", "102", _formatDate(tradeLineItem.BillingPeriodStart.Value));
//                        Writer.WriteEndElement(); // !StartDateTime
//                    }
//
//                    if (tradeLineItem.BillingPeriodEnd.HasValue)
//                    {
//                        Writer.WriteStartElement("ram:EndDateTime");
//                        _writeElementWithAttribute(Writer, "udt:DateTimeString", "format", "102", _formatDate(tradeLineItem.BillingPeriodEnd.Value));
//                        Writer.WriteEndElement(); // !EndDateTime
//                    }
//                    Writer.WriteEndElement(); // !BillingSpecifiedPeriod
//                }
//
//                Writer.WriteStartElement("ram:SpecifiedTradeSettlementLineMonetarySummation");
//
//                decimal _total = 0m;
//
//                if (tradeLineItem.LineTotalAmount.HasValue)
//                {
//                    _total = tradeLineItem.LineTotalAmount.Value;
//                }
//                else if (tradeLineItem.NetUnitPrice.HasValue)
//                {
//                    _total = tradeLineItem.NetUnitPrice.Value * tradeLineItem.BilledQuantity;
//                }
//
//                Writer.WriteElementString("ram:LineTotalAmount", _formatDecimal(_total));
//
//                Writer.WriteEndElement(); // ram:SpecifiedTradeSettlementLineMonetarySummation
//                Writer.WriteEndElement(); // !ram:SpecifiedLineTradeSettlement
//
//                Writer.WriteEndElement(); // !ram:IncludedSupplyChainTradeLineItem
//            } // !foreach(tradeLineItem)
//
//            Writer.WriteStartElement("ram:ApplicableHeaderTradeAgreement");
//            if (!String.IsNullOrEmpty(this.Descriptor.ReferenceOrderNo))
//            {
//                Writer.WriteElementString("ram:BuyerReference", this.Descriptor.ReferenceOrderNo);
//            }
//
//            _writeOptionalParty(Writer, "ram:SellerTradeParty", this.Descriptor.Seller, this.Descriptor.SellerContact, TaxRegistrations: this.Descriptor.SellerTaxRegistration);
//            _writeOptionalParty(Writer, "ram:BuyerTradeParty", this.Descriptor.Buyer, this.Descriptor.BuyerContact, TaxRegistrations: this.Descriptor.BuyerTaxRegistration);
//
//
//            #region SellerOrderReferencedDocument (BT-14: Comfort, Extended)
//            if (null != this.Descriptor.SellerOrderReferencedDocument && !string.IsNullOrEmpty(Descriptor.SellerOrderReferencedDocument.ID))
//            {
//                Writer.WriteStartElement("ram:SellerOrderReferencedDocument", Profile.Comfort | Profile.Extended);
//                Writer.WriteElementString("ram:IssuerAssignedID", this.Descriptor.SellerOrderReferencedDocument.ID);
//                if (this.Descriptor.SellerOrderReferencedDocument.IssueDateTime.HasValue)
//                {
//                    Writer.WriteStartElement("ram:FormattedIssueDateTime", Profile.Extended);
//                    Writer.WriteStartElement("qdt:DateTimeString");
//                    Writer.WriteAttributeString("format", "102");
//                    Writer.WriteValue(_formatDate(this.Descriptor.SellerOrderReferencedDocument.IssueDateTime.Value));
//                    Writer.WriteEndElement(); // !qdt:DateTimeString
//                    Writer.WriteEndElement(); // !IssueDateTime()
//                }
//
//                Writer.WriteEndElement(); // !SellerOrderReferencedDocument
//            }
//            #endregion
//
//
//            if (!String.IsNullOrEmpty(this.Descriptor.OrderNo))
//            {
//                Writer.WriteStartElement("ram:BuyerOrderReferencedDocument");
//                Writer.WriteElementString("ram:IssuerAssignedID", this.Descriptor.OrderNo);
//                if (this.Descriptor.OrderDate.HasValue)
//                {
//                    Writer.WriteStartElement("ram:FormattedIssueDateTime");
//                    Writer.WriteStartElement("qdt:DateTimeString");
//                    Writer.WriteAttributeString("format", "102");
//                    Writer.WriteValue(_formatDate(this.Descriptor.OrderDate.Value));
//                    Writer.WriteEndElement(); // !qdt:DateTimeString
//                    Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//                }
//                Writer.WriteEndElement(); // !BuyerOrderReferencedDocument
//            }
//
//
//            if (this.Descriptor.AdditionalReferencedDocuments != null)
//            {
//                foreach (AdditionalReferencedDocument document in this.Descriptor.AdditionalReferencedDocuments)
//                {
//                    Writer.WriteStartElement("ram:AdditionalReferencedDocument");
//                    if (document.IssueDateTime.HasValue)
//                    {
//                        Writer.WriteStartElement("ram:FormattedIssueDateTime");
//                        Writer.WriteStartElement("qdt:DateTimeString");
//                        Writer.WriteValue(_formatDate(document.IssueDateTime.Value));
//                        Writer.WriteEndElement(); // !udt:DateTimeString
//                        Writer.WriteEndElement(); // !FormattedIssueDateTime
//                    }
//
//                    if (document.ReferenceTypeCode != ReferenceTypeCodes.Unknown)
//                    {
//                        Writer.WriteElementString("ram:TypeCode", document.ReferenceTypeCode.EnumToString());
//                    }
//
//                    Writer.WriteElementString("ram:ID", document.ID);
//                    Writer.WriteEndElement(); // !ram:AdditionalReferencedDocument
//                } // !foreach(document)
//            }
//
//            Writer.WriteEndElement(); // !ApplicableHeaderTradeAgreement
//
//            Writer.WriteStartElement("ram:ApplicableHeaderTradeDelivery"); // Pflichteintrag
//
//            if (Descriptor.Profile == Profile.Extended)
//            {
//                _writeOptionalParty(Writer, "ram:ShipToTradeParty", this.Descriptor.ShipTo);
//                _writeOptionalParty(Writer, "ram:ShipFromTradeParty", this.Descriptor.ShipFrom);
//            }
//
//            if (this.Descriptor.ActualDeliveryDate.HasValue)
//            {
//                Writer.WriteStartElement("ram:ActualDeliverySupplyChainEvent");
//                Writer.WriteStartElement("ram:OccurrenceDateTime");
//                Writer.WriteStartElement("udt:DateTimeString");
//                Writer.WriteAttributeString("format", "102");
//                Writer.WriteValue(_formatDate(this.Descriptor.ActualDeliveryDate.Value));
//                Writer.WriteEndElement(); // "udt:DateTimeString
//                Writer.WriteEndElement(); // !OccurrenceDateTime()
//                Writer.WriteEndElement(); // !ActualDeliverySupplyChainEvent
//            }
//
//            if (this.Descriptor.DeliveryNoteReferencedDocument != null)
//            {
//                Writer.WriteStartElement("ram:DeliveryNoteReferencedDocument");
//
//                if (this.Descriptor.DeliveryNoteReferencedDocument.IssueDateTime.HasValue)
//                {
//                    Writer.WriteStartElement("ram:FormattedIssueDateTime");
//                    Writer.WriteValue(_formatDate(this.Descriptor.DeliveryNoteReferencedDocument.IssueDateTime.Value, false));
//                    Writer.WriteEndElement(); // !IssueDateTime
//                }
//
//                Writer.WriteElementString("ram:IssuerAssignedID", this.Descriptor.DeliveryNoteReferencedDocument.ID);
//                Writer.WriteEndElement(); // !DeliveryNoteReferencedDocument
//            }
//
//            Writer.WriteEndElement(); // !ApplicableHeaderTradeDelivery
//
//            Writer.WriteStartElement("ram:ApplicableHeaderTradeSettlement");
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
//            {
//                _writeOptionalElementString(Writer, "ram:CreditorReferenceID", Descriptor.PaymentMeans?.SEPACreditorIdentifier, Profile.BasicWL | Profile.Basic | Profile.Comfort | Profile.Extended | Profile.XRechnung | Profile.XRechnung1);
//            }
//
//            //   2. PaymentReference (optional)
//            if (!String.IsNullOrEmpty(this.Descriptor.PaymentReference))
//            {
//                _writeOptionalElementString(Writer, "ram:PaymentReference", this.Descriptor.PaymentReference);
//            }
//
//            //   4. InvoiceCurrencyCode (optional)
//            Writer.WriteElementString("ram:InvoiceCurrencyCode", this.Descriptor.Currency.EnumToString());
//
//            //   7. InvoiceeTradeParty (optional)
//            if (Descriptor.Profile == Profile.Extended)
//            {
//                _writeOptionalParty(Writer, "ram:InvoiceeTradeParty", this.Descriptor.Invoicee);
//            }
//
//            //   8. PayeeTradeParty (optional)
//            if (Descriptor.Profile != Profile.Minimum)
//            {
//                _writeOptionalParty(Writer, "ram:PayeeTradeParty", this.Descriptor.Payee);
//            }
//
//            //  10. SpecifiedTradeSettlementPaymentMeans (optional)
//            if (this.Descriptor.CreditorBankAccounts.Count == 0 && this.Descriptor.DebitorBankAccounts.Count == 0)
//            {
//                if (this.Descriptor.PaymentMeans != null)
//                {
//                    Writer.WriteStartElement("ram:SpecifiedTradeSettlementPaymentMeans");
//
//                    if ((this.Descriptor.PaymentMeans != null) && (this.Descriptor.PaymentMeans.TypeCode != PaymentMeansTypeCodes.Unknown))
//                    {
//                        Writer.WriteElementString("ram:TypeCode", this.Descriptor.PaymentMeans.TypeCode.EnumToString());
//                        Writer.WriteElementString("ram:Information", this.Descriptor.PaymentMeans.Information);
//
//                        if (this.Descriptor.PaymentMeans.FinancialCard != null)
//                        {
//                            Writer.WriteStartElement("ram:ApplicableTradeSettlementFinancialCard", Profile.Comfort | Profile.Extended);
//                            Writer.WriteElementString("ram:ID", Descriptor.PaymentMeans.FinancialCard.Id);
//                            Writer.WriteElementString("ram:CardholderName", Descriptor.PaymentMeans.FinancialCard.CardholderName);
//                            Writer.WriteEndElement(); // !ram:ApplicableTradeSettlementFinancialCard
//                        }
//                    }
//                    Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
//                }
//            }
//            else
//            {
//                foreach (BankAccount account in this.Descriptor.CreditorBankAccounts)
//                {
//                    Writer.WriteStartElement("ram:SpecifiedTradeSettlementPaymentMeans");
//
//                    if ((this.Descriptor.PaymentMeans != null) && (this.Descriptor.PaymentMeans.TypeCode != PaymentMeansTypeCodes.Unknown))
//                    {
//                        Writer.WriteElementString("ram:TypeCode", this.Descriptor.PaymentMeans.TypeCode.EnumToString());
//                        Writer.WriteElementString("ram:Information", this.Descriptor.PaymentMeans.Information);
//
//                        if (this.Descriptor.PaymentMeans.FinancialCard != null)
//                        {
//                            Writer.WriteStartElement("ram:ApplicableTradeSettlementFinancialCard", Profile.Comfort | Profile.Extended);
//                            Writer.WriteElementString("ram:ID", Descriptor.PaymentMeans.FinancialCard.Id);
//                            Writer.WriteElementString("ram:CardholderName", Descriptor.PaymentMeans.FinancialCard.CardholderName);
//                            Writer.WriteEndElement(); // !ram:ApplicableTradeSettlementFinancialCard
//                        }
//                    }
//
//                    Writer.WriteStartElement("ram:PayeePartyCreditorFinancialAccount");
//                    Writer.WriteElementString("ram:IBANID", account.IBAN);
//                    if (!String.IsNullOrEmpty(account.Name))
//                    {
//                        Writer.WriteElementString("ram:AccountName", account.Name);
//                    }
//                    if (!String.IsNullOrEmpty(account.ID))
//                    {
//                        Writer.WriteElementString("ram:ProprietaryID", account.ID);
//                    }
//                    Writer.WriteEndElement(); // !PayeePartyCreditorFinancialAccount
//
//                    Writer.WriteStartElement("ram:PayeeSpecifiedCreditorFinancialInstitution");
//                    Writer.WriteElementString("ram:BICID", account.BIC);
//
//                    if (!String.IsNullOrEmpty(account.Bankleitzahl))
//                    {
//                        Writer.WriteElementString("ram:GermanBankleitzahlID", account.Bankleitzahl);
//                    }
//
//                    Writer.WriteEndElement(); // !PayeeSpecifiedCreditorFinancialInstitution
//                    Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
//                }
//
//                foreach (BankAccount account in this.Descriptor.DebitorBankAccounts)
//                {
//                    Writer.WriteStartElement("ram:SpecifiedTradeSettlementPaymentMeans");
//
//                    if ((this.Descriptor.PaymentMeans != null) && (this.Descriptor.PaymentMeans.TypeCode != PaymentMeansTypeCodes.Unknown))
//                    {
//                        Writer.WriteElementString("ram:TypeCode", this.Descriptor.PaymentMeans.TypeCode.EnumToString());
//                        Writer.WriteElementString("ram:Information", this.Descriptor.PaymentMeans.Information);
//                    }
//
//                    Writer.WriteStartElement("ram:PayerPartyDebtorFinancialAccount");
//                    Writer.WriteElementString("ram:IBANID", account.IBAN);
//                    if (!String.IsNullOrEmpty(account.ID))
//                    {
//                        Writer.WriteElementString("ram:ProprietaryID", account.ID);
//                    }
//                    Writer.WriteEndElement(); // !PayerPartyDebtorFinancialAccount
//
//                    if (!string.IsNullOrEmpty(account.BIC) ||
//                        !string.IsNullOrEmpty(account.Bankleitzahl) ||
//                        !string.IsNullOrEmpty(account.BankName))
//                    {
//                        Writer.WriteStartElement("ram:PayerSpecifiedDebtorFinancialInstitution");
//
//                        if (!String.IsNullOrEmpty(account.BIC))
//                        {
//                            Writer.WriteElementString("ram:BICID", account.BIC);
//                        }
//
//                        if (!String.IsNullOrEmpty(account.Bankleitzahl))
//                        {
//                            Writer.WriteElementString("ram:GermanBankleitzahlID", account.Bankleitzahl);
//                        }
//
//                        if (!String.IsNullOrEmpty(account.BankName))
//                        {
//                            Writer.WriteElementString("ram:Name", account.BankName);
//                        }
//
//                        Writer.WriteEndElement(); // !PayerSpecifiedDebtorFinancialInstitution
//                    }
//
//                    Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
//                }
//            }
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
//            {
//                Writer.WriteStartElement("ram:BillingSpecifiedPeriod", Profile.BasicWL | Profile.Basic | Profile.Comfort | Profile.Extended | Profile.XRechnung1 | Profile.XRechnung);
//                if (Descriptor.BillingPeriodStart.HasValue)
//                {
//                    Writer.WriteStartElement("ram:StartDateTime");
//                    _writeElementWithAttribute(Writer, "udt:DateTimeString", "format", "102", _formatDate(this.Descriptor.BillingPeriodStart.Value));
//                    Writer.WriteEndElement(); // !StartDateTime
//                }
//
//                if (Descriptor.BillingPeriodEnd.HasValue)
//                {
//                    Writer.WriteStartElement("ram:EndDateTime");
//                    _writeElementWithAttribute(Writer, "udt:DateTimeString", "format", "102", _formatDate(this.Descriptor.BillingPeriodEnd.Value));
//                    Writer.WriteEndElement(); // !EndDateTime
//                }
//                Writer.WriteEndElement(); // !BillingSpecifiedPeriod
//            }
//            #endregion
//
//            //  13. SpecifiedTradeAllowanceCharge (optional)
//            if ((this.Descriptor.TradeAllowanceCharges != null) && (this.Descriptor.TradeAllowanceCharges.Count > 0))
//            {
//                foreach (TradeAllowanceCharge tradeAllowanceCharge in this.Descriptor.TradeAllowanceCharges)
//                {
//                    Writer.WriteStartElement("ram:SpecifiedTradeAllowanceCharge");
//                    Writer.WriteStartElement("ram:ChargeIndicator");
//                    Writer.WriteElementString("udt:Indicator", tradeAllowanceCharge.ChargeIndicator ? "true" : "false");
//                    Writer.WriteEndElement(); // !ram:ChargeIndicator
//
//                    Writer.WriteStartElement("ram:BasisAmount");
//                    Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.BasisAmount));
//                    Writer.WriteEndElement();
//
//                    Writer.WriteStartElement("ram:ActualAmount");
//                    Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ActualAmount));
//                    Writer.WriteEndElement();
//
//
//                    _writeOptionalElementString(Writer, "ram:Reason", tradeAllowanceCharge.Reason);
//
//                    if (tradeAllowanceCharge.Tax != null)
//                    {
//                        Writer.WriteStartElement("ram:CategoryTradeTax");
//                        Writer.WriteElementString("ram:TypeCode", tradeAllowanceCharge.Tax.TypeCode.EnumToString());
//                        if (tradeAllowanceCharge.Tax.CategoryCode.HasValue)
//                            Writer.WriteElementString("ram:CategoryCode", tradeAllowanceCharge.Tax.CategoryCode?.EnumToString());
//                        Writer.WriteElementString("ram:RateApplicablePercent", _formatDecimal(tradeAllowanceCharge.Tax.Percent));
//                        Writer.WriteEndElement();
//                    }
//                    Writer.WriteEndElement();
//                }
//            }
//
//            //  14. SpecifiedLogisticsServiceCharge (optional)
//            if ((this.Descriptor.ServiceCharges != null) && (this.Descriptor.ServiceCharges.Count > 0))
//            {
//                foreach (ServiceCharge serviceCharge in this.Descriptor.ServiceCharges)
//                {
//                    Writer.WriteStartElement("ram:SpecifiedLogisticsServiceCharge");
//                    if (!String.IsNullOrEmpty(serviceCharge.Description))
//                    {
//                        Writer.WriteElementString("ram:Description", serviceCharge.Description);
//                    }
//                    Writer.WriteElementString("ram:AppliedAmount", _formatDecimal(serviceCharge.Amount));
//                    if (serviceCharge.Tax != null)
//                    {
//                        Writer.WriteStartElement("ram:AppliedTradeTax");
//                        Writer.WriteElementString("ram:TypeCode", serviceCharge.Tax.TypeCode.EnumToString());
//                        if (serviceCharge.Tax.CategoryCode.HasValue)
//                            Writer.WriteElementString("ram:CategoryCode", serviceCharge.Tax.CategoryCode?.EnumToString());
//                        Writer.WriteElementString("ram:RateApplicablePercent", _formatDecimal(serviceCharge.Tax.Percent));
//                        Writer.WriteEndElement();
//                    }
//                    Writer.WriteEndElement();
//                }
//            }
//
//            //  15. SpecifiedTradePaymentTerms (optional)
//            if (this.Descriptor.PaymentTerms != null || !string.IsNullOrEmpty(Descriptor.PaymentMeans?.SEPAMandateReference))
//            {
//                Writer.WriteStartElement("ram:SpecifiedTradePaymentTerms");
//                _writeOptionalElementString(Writer, "ram:Description", this.Descriptor.PaymentTerms?.Description);
//                if (this.Descriptor.PaymentTerms?.DueDate.HasValue ?? false)
//                {
//                    Writer.WriteStartElement("ram:DueDateDateTime");
//                    _writeElementWithAttribute(Writer, "udt:DateTimeString", "format", "102", _formatDate(this.Descriptor.PaymentTerms.DueDate.Value));
//                    Writer.WriteEndElement(); // !ram:DueDateDateTime
//                }
//                _writeOptionalElementString(Writer, "ram:DirectDebitMandateID", Descriptor.PaymentMeans?.SEPAMandateReference);
//                Writer.WriteEndElement();
//            }
//
//            //  16. SpecifiedTradeSettlementHeaderMonetarySummation
//            Writer.WriteStartElement("ram:SpecifiedTradeSettlementHeaderMonetarySummation");
//            _writeOptionalAmount(Writer, "ram:LineTotalAmount", this.Descriptor.LineTotalAmount);
//            _writeOptionalAmount(Writer, "ram:ChargeTotalAmount", this.Descriptor.ChargeTotalAmount);
//            _writeOptionalAmount(Writer, "ram:AllowanceTotalAmount", this.Descriptor.AllowanceTotalAmount);
//            _writeOptionalAmount(Writer, "ram:TaxBasisTotalAmount", this.Descriptor.TaxBasisAmount);
//            _writeOptionalAmount(Writer, "ram:TaxTotalAmount", this.Descriptor.TaxTotalAmount, forceCurrency: true);
//            _writeOptionalAmount(Writer, "ram:RoundingAmount", this.Descriptor.RoundingAmount, profile: Profile.Comfort | Profile.Extended);  // RoundingAmount  //Rundungsbetrag
//            _writeOptionalAmount(Writer, "ram:GrandTotalAmount", this.Descriptor.GrandTotalAmount);
//
//            if (this.Descriptor.TotalPrepaidAmount.HasValue)
//            {
//                _writeOptionalAmount(Writer, "ram:TotalPrepaidAmount", this.Descriptor.TotalPrepaidAmount.Value);
//            }
//
//            _writeOptionalAmount(Writer, "ram:DuePayableAmount", this.Descriptor.DuePayableAmount);
//            Writer.WriteEndElement(); // !ram:SpecifiedTradeSettlementHeaderMonetarySummation
//
//            #region InvoiceReferencedDocument
//            //  17. InvoiceReferencedDocument (optional)
//            if (this.Descriptor.InvoiceReferencedDocument != null)
//            {
//                Writer.WriteStartElement("ram:InvoiceReferencedDocument");
//                _writeOptionalElementString(Writer, "ram:IssuerAssignedID", this.Descriptor.InvoiceReferencedDocument.ID);
//                if (this.Descriptor.InvoiceReferencedDocument.IssueDateTime.HasValue)
//                {
//                    Writer.WriteStartElement("ram:FormattedIssueDateTime");
//                    _writeElementWithAttribute(Writer, "qdt:DateTimeString", "format", "102", _formatDate(this.Descriptor.InvoiceReferencedDocument.IssueDateTime.Value));
//                    Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//                }
//                Writer.WriteEndElement(); // !ram:InvoiceReferencedDocument
//            }
//            #endregion
//
//
//            Writer.WriteEndElement(); // !ram:ApplicableHeaderTradeSettlement
//
//            Writer.WriteEndElement(); // !ram:SupplyChainTradeTransaction
//            #endregion
//
//            Writer.WriteEndElement(); // !ram:Invoice
//            Writer.WriteEndDocument();
//            Writer.Flush();
//
//            stream.Seek(streamPosition, SeekOrigin.Begin);
end;

function TZUGFeRDInvoiceDescriptor20Writer.Validate(
  descriptor: TZUGFeRDInvoiceDescriptor; throwExceptions: Boolean): Boolean;
begin
//            if (descriptor.Profile == Profile.BasicWL)
//            {
//                if (throwExceptions)
//                {
//                    throw new UnsupportedException("Invalid profile used for ZUGFeRD 2.0 invoice.");
//                }
//                return false;
//            }
//
//            if (descriptor.Profile != Profile.Extended) // check tax types, only extended profile allows tax types other than vat
//            {
//                if (!descriptor.TradeLineItems.All(l => l.TaxType.Equals(TaxTypes.VAT) || l.TaxType.Equals(TaxTypes.Unknown)))
//                {
//                    if (throwExceptions) { throw new UnsupportedException("Tax types other than VAT only possible with extended profile."); }
//                    return false;
//                }
//            }
//
//            return true;

end;

end.
