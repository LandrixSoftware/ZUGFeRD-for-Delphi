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
  System.Classes,
  intf.ZUGFeRDInvoiceDescriptor
  ,intf.ZUGFeRDInvoiceDescriptorWriter
  ,intf.ZUGFeRDProfileAwareXmlTextWriter
  ,intf.ZUGFeRDExceptions
  ;

type
  TZUGFeRDInvoiceDescriptor1Writer = class(TZUGFeRDInvoiceDescriptorWriter)
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

{ TZUGFeRDInvoiceDescriptor1Writer }

procedure TZUGFeRDInvoiceDescriptor1Writer.Save(descriptor: TZUGFeRDInvoiceDescriptor; stream: TStream);
begin
  if (stream = nil) then
    raise TZUGFeRDIllegalStreamException.Create('Cannot write to stream');

//
//            // validate data
//            if ((descriptor.Profile == Profile.BasicWL) || (descriptor.Profile == Profile.Minimum))
//            {
//                throw new UnsupportedException("Invalid profile used for ZUGFeRD 1.x invoice.");
//            }
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
//            Writer.WriteStartElement("rsm:CrossIndustryDocument");
//            Writer.WriteAttributeString("xmlns", "xsi", null, "http://www.w3.org/2001/XMLSchema-instance");
//            Writer.WriteAttributeString("xmlns", "rsm", null, "urn:ferd:CrossIndustryDocument:invoice:1p0");
//            Writer.WriteAttributeString("xmlns", "ram", null, "urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:12");
//            Writer.WriteAttributeString("xmlns", "udt", null, "urn:un:unece:uncefact:data:standard:UnqualifiedDataType:15");
//            #endregion
//
//            #region SpecifiedExchangedDocumentContext
//            Writer.WriteStartElement("rsm:SpecifiedExchangedDocumentContext");
//            Writer.WriteStartElement("ram:TestIndicator");
//            Writer.WriteElementString("udt:Indicator", this.Descriptor.IsTest ? "true" : "false");
//            Writer.WriteEndElement(); // !ram:TestIndicator
//
//            if (!String.IsNullOrEmpty(this.Descriptor.BusinessProcess))
//            {
//                Writer.WriteStartElement("ram:BusinessProcessSpecifiedDocumentContextParameter", Profile.Extended);
//                Writer.WriteElementString("ram:ID", this.Descriptor.BusinessProcess, Profile.Extended);
//                Writer.WriteEndElement(); // !ram:BusinessProcessSpecifiedDocumentContextParameter
//            }
//
//            Writer.WriteStartElement("ram:GuidelineSpecifiedDocumentContextParameter");
//            Writer.WriteElementString("ram:ID", this.Descriptor.Profile.EnumToString(ZUGFeRDVersion.Version1));
//            Writer.WriteEndElement(); // !ram:GuidelineSpecifiedDocumentContextParameter
//            Writer.WriteEndElement(); // !rsm:SpecifiedExchangedDocumentContext
//
//            Writer.WriteStartElement("rsm:HeaderExchangedDocument");
//            Writer.WriteElementString("ram:ID", this.Descriptor.InvoiceNo);
//            Writer.WriteElementString("ram:Name", _translateInvoiceType(this.Descriptor.Type));
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
//            Writer.WriteEndElement(); // !rsm:HeaderExchangedDocument
//            #endregion
//
//            #region SpecifiedSupplyChainTradeTransaction
//            Writer.WriteStartElement("rsm:SpecifiedSupplyChainTradeTransaction");
//            Writer.WriteStartElement("ram:ApplicableSupplyChainTradeAgreement");
//            if (!String.IsNullOrEmpty(this.Descriptor.ReferenceOrderNo))
//            {
//                Writer.WriteElementString("ram:BuyerReference", this.Descriptor.ReferenceOrderNo);
//            }
//
//            _writeOptionalParty(Writer, "ram:SellerTradeParty", this.Descriptor.Seller, this.Descriptor.SellerContact, TaxRegistrations: this.Descriptor.SellerTaxRegistration);
//            _writeOptionalParty(Writer, "ram:BuyerTradeParty", this.Descriptor.Buyer, this.Descriptor.BuyerContact, TaxRegistrations: this.Descriptor.BuyerTaxRegistration);
//
//            if (!String.IsNullOrEmpty(this.Descriptor.OrderNo))
//            {
//                Writer.WriteStartElement("ram:BuyerOrderReferencedDocument");
//                if (this.Descriptor.OrderDate.HasValue)
//                {
//                    Writer.WriteStartElement("ram:IssueDateTime");
//                    //Writer.WriteStartElement("udt:DateTimeString");
//                    //Writer.WriteAttributeString("format", "102");
//                    Writer.WriteValue(_formatDate(this.Descriptor.OrderDate.Value, false));
//                    //Writer.WriteEndElement(); // !udt:DateTimeString
//                    Writer.WriteEndElement(); // !IssueDateTime()
//                }
//
//                Writer.WriteElementString("ram:ID", this.Descriptor.OrderNo);
//                Writer.WriteEndElement(); // !BuyerOrderReferencedDocument
//            }
//
//            if (this.Descriptor.AdditionalReferencedDocuments != null)
//            {
//                foreach(AdditionalReferencedDocument document in this.Descriptor.AdditionalReferencedDocuments)
//                {
//                    Writer.WriteStartElement("ram:AdditionalReferencedDocument");
//                    if (document.IssueDateTime.HasValue)
//                    {
//                        Writer.WriteStartElement("ram:IssueDateTime");
//                        //Writer.WriteStartElement("udt:DateTimeString");
//                        //Writer.WriteAttributeString("format", "102");
//                        Writer.WriteValue(_formatDate(document.IssueDateTime.Value, false));
//                        //Writer.WriteEndElement(); // !udt:DateTimeString
//                        Writer.WriteEndElement(); // !IssueDateTime()
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
//            Writer.WriteEndElement(); // !ApplicableSupplyChainTradeAgreement
//
//            Writer.WriteStartElement("ram:ApplicableSupplyChainTradeDelivery"); // Pflichteintrag
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
//                    Writer.WriteStartElement("ram:IssueDateTime");
//                    Writer.WriteValue(_formatDate(this.Descriptor.DeliveryNoteReferencedDocument.IssueDateTime.Value, false));
//                    Writer.WriteEndElement(); // !IssueDateTime
//                }
//
//                Writer.WriteElementString("ram:ID", this.Descriptor.DeliveryNoteReferencedDocument.ID);
//                Writer.WriteEndElement(); // !DeliveryNoteReferencedDocument
//            }
//
//            Writer.WriteEndElement(); // !ApplicableSupplyChainTradeDelivery
//
//            Writer.WriteStartElement("ram:ApplicableSupplyChainTradeSettlement");
//            Writer.WriteElementString("ram:InvoiceCurrencyCode", this.Descriptor.Currency.EnumToString());
//
//            if (Descriptor.Profile != Profile.Basic)
//            {
//                _writeOptionalParty(Writer, "ram:InvoiceeTradeParty", this.Descriptor.Invoicee);
//            }
//            if (Descriptor.Profile == Profile.Extended)
//            {
//                _writeOptionalParty(Writer, "ram:PayeeTradeParty", this.Descriptor.Payee);
//            }
//
//            if (!String.IsNullOrEmpty(this.Descriptor.PaymentReference))
//            {
//                _writeOptionalElementString(Writer, "ram:PaymentReference", this.Descriptor.PaymentReference);
//            }
//
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
//                        if (!String.IsNullOrEmpty(this.Descriptor.PaymentMeans.SEPACreditorIdentifier) && !String.IsNullOrEmpty(this.Descriptor.PaymentMeans.SEPAMandateReference))
//                        {
//                            Writer.WriteStartElement("ram:ID");
//                            Writer.WriteAttributeString("schemeAgencyID", this.Descriptor.PaymentMeans.SEPACreditorIdentifier);
//                            Writer.WriteValue(this.Descriptor.PaymentMeans.SEPAMandateReference);
//                            Writer.WriteEndElement(); // !ram:ID
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
//                        if (!String.IsNullOrEmpty(this.Descriptor.PaymentMeans.SEPACreditorIdentifier) && !String.IsNullOrEmpty(this.Descriptor.PaymentMeans.SEPAMandateReference))
//                        {
//                            Writer.WriteStartElement("ram:ID");
//                            Writer.WriteAttributeString("schemeAgencyID", this.Descriptor.PaymentMeans.SEPACreditorIdentifier);
//                            Writer.WriteValue(this.Descriptor.PaymentMeans.SEPAMandateReference);
//                            Writer.WriteEndElement(); // !ram:ID
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
//                    if (!String.IsNullOrEmpty(account.BankName))
//                    {
//                        Writer.WriteElementString("ram:Name", account.BankName);
//                    }
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
//
//                        if (!String.IsNullOrEmpty(this.Descriptor.PaymentMeans.SEPACreditorIdentifier) && !String.IsNullOrEmpty(this.Descriptor.PaymentMeans.SEPAMandateReference))
//                        {
//                            Writer.WriteStartElement("ram:ID");
//                            Writer.WriteAttributeString("schemeAgencyID", this.Descriptor.PaymentMeans.SEPACreditorIdentifier);
//                            Writer.WriteValue(this.Descriptor.PaymentMeans.SEPAMandateReference);
//                            Writer.WriteEndElement(); // !ram:ID
//                        }
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
//                    Writer.WriteStartElement("ram:PayerSpecifiedDebtorFinancialInstitution");
//                    Writer.WriteElementString("ram:BICID", account.BIC);
//
//                    if (!String.IsNullOrEmpty(account.Bankleitzahl))
//                    {
//                        Writer.WriteElementString("ram:GermanBankleitzahlID", account.Bankleitzahl);
//                    }
//
//                    if (!String.IsNullOrEmpty(account.BankName))
//                    {
//                        Writer.WriteElementString("ram:Name", account.BankName);
//                    }
//                    Writer.WriteEndElement(); // !PayerSpecifiedDebtorFinancialInstitution
//                    Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
//                }
//            }
//
//            _writeOptionalTaxes(Writer);
//
//            if ((this.Descriptor.TradeAllowanceCharges != null) && (this.Descriptor.TradeAllowanceCharges.Count > 0))
//            {
//                foreach (TradeAllowanceCharge tradeAllowanceCharge in this.Descriptor.TradeAllowanceCharges)
//                {
//                    Writer.WriteStartElement("ram:SpecifiedTradeAllowanceCharge");
//                    Writer.WriteStartElement("ram:ChargeIndicator", Profile.Comfort | Profile.Extended);
//                    Writer.WriteElementString("udt:Indicator", tradeAllowanceCharge.ChargeIndicator ? "true" : "false");
//                    Writer.WriteEndElement(); // !ram:ChargeIndicator
//
//                    Writer.WriteStartElement("ram:BasisAmount", Profile.Extended);
//                    Writer.WriteAttributeString("currencyID", tradeAllowanceCharge.Currency.EnumToString());
//                    Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.BasisAmount));
//                    Writer.WriteEndElement();
//
//                    Writer.WriteStartElement("ram:ActualAmount", Profile.Comfort | Profile.Extended);
//                    Writer.WriteAttributeString("currencyID", tradeAllowanceCharge.Currency.EnumToString());
//                    Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ActualAmount, 2));
//                    Writer.WriteEndElement();
//
//
//                    _writeOptionalElementString(Writer, "ram:Reason", tradeAllowanceCharge.Reason, Profile.Comfort | Profile.Extended);
//
//                    if (tradeAllowanceCharge.Tax != null)
//                    {
//                        Writer.WriteStartElement("ram:CategoryTradeTax");
//                        Writer.WriteElementString("ram:TypeCode", tradeAllowanceCharge.Tax.TypeCode.EnumToString(), Profile.Comfort | Profile.Extended);
//                        if (tradeAllowanceCharge.Tax.CategoryCode.HasValue)
//                            Writer.WriteElementString("ram:CategoryCode", tradeAllowanceCharge.Tax.CategoryCode?.EnumToString(), Profile.Comfort | Profile.Extended);
//                        Writer.WriteElementString("ram:ApplicablePercent", _formatDecimal(tradeAllowanceCharge.Tax.Percent), Profile.Comfort | Profile.Extended);
//                        Writer.WriteEndElement();
//                    }
//                    Writer.WriteEndElement();
//                }
//            }
//
//            if ((this.Descriptor.ServiceCharges != null) && (this.Descriptor.ServiceCharges.Count > 0))
//            {
//                foreach (ServiceCharge serviceCharge in this.Descriptor.ServiceCharges)
//                {
//                    Writer.WriteStartElement("ram:SpecifiedLogisticsServiceCharge");
//                    if (!String.IsNullOrEmpty(serviceCharge.Description))
//                    {
//                        Writer.WriteElementString("ram:Description", serviceCharge.Description, Profile.Comfort | Profile.Extended);
//                    }
//                    Writer.WriteElementString("ram:AppliedAmount", _formatDecimal(serviceCharge.Amount), Profile.Comfort | Profile.Extended);
//                    if (serviceCharge.Tax != null)
//                    {
//                        Writer.WriteStartElement("ram:AppliedTradeTax");
//                        Writer.WriteElementString("ram:TypeCode", serviceCharge.Tax.TypeCode.EnumToString(), Profile.Comfort | Profile.Extended);
//                        if (serviceCharge.Tax.CategoryCode.HasValue)
//                            Writer.WriteElementString("ram:CategoryCode", serviceCharge.Tax.CategoryCode?.EnumToString(), Profile.Comfort | Profile.Extended);
//                        Writer.WriteElementString("ram:ApplicablePercent", _formatDecimal(serviceCharge.Tax.Percent), Profile.Comfort | Profile.Extended);
//                        Writer.WriteEndElement();
//                    }
//                    Writer.WriteEndElement();
//                }
//            }
//
//            if (this.Descriptor.PaymentTerms != null)
//            {
//                Writer.WriteStartElement("ram:SpecifiedTradePaymentTerms");
//                _writeOptionalElementString(Writer, "ram:Description", this.Descriptor.PaymentTerms.Description);
//                if (this.Descriptor.PaymentTerms.DueDate.HasValue)
//                {
//                    Writer.WriteStartElement("ram:DueDateDateTime");
//                    _writeElementWithAttribute(Writer, "udt:DateTimeString", "format", "102", _formatDate(this.Descriptor.PaymentTerms.DueDate.Value));
//                    Writer.WriteEndElement(); // !ram:DueDateDateTime
//                }
//                Writer.WriteEndElement();
//            }
//
//            Writer.WriteStartElement("ram:SpecifiedTradeSettlementMonetarySummation");
//            _writeOptionalAmount(Writer, "ram:LineTotalAmount", this.Descriptor.LineTotalAmount);
//
//            _writeOptionalAmount(Writer, "ram:ChargeTotalAmount", this.Descriptor.ChargeTotalAmount);
//            _writeOptionalAmount(Writer, "ram:AllowanceTotalAmount", this.Descriptor.AllowanceTotalAmount);
//            _writeOptionalAmount(Writer, "ram:TaxBasisTotalAmount", this.Descriptor.TaxBasisAmount);
//            _writeOptionalAmount(Writer, "ram:TaxTotalAmount", this.Descriptor.TaxTotalAmount);
//            _writeOptionalAmount(Writer, "ram:GrandTotalAmount", this.Descriptor.GrandTotalAmount);
//            _writeOptionalAmount(Writer, "ram:TotalPrepaidAmount", this.Descriptor.TotalPrepaidAmount);
//            _writeOptionalAmount(Writer, "ram:DuePayableAmount", this.Descriptor.DuePayableAmount);
//            Writer.WriteEndElement(); // !ram:SpecifiedTradeSettlementMonetarySummation
//
//            Writer.WriteEndElement(); // !ram:ApplicableSupplyChainTradeSettlement
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
//                if (Descriptor.Profile != Profile.Basic)
//                {
//                    Writer.WriteStartElement("ram:SpecifiedSupplyChainTradeAgreement");
//
//                    if (tradeLineItem.BuyerOrderReferencedDocument != null)
//                    {
//                        Writer.WriteStartElement("ram:BuyerOrderReferencedDocument");
//                        if (tradeLineItem.BuyerOrderReferencedDocument.IssueDateTime.HasValue)
//                        {
//                            Writer.WriteStartElement("ram:IssueDateTime");
//                            Writer.WriteValue(_formatDate(tradeLineItem.BuyerOrderReferencedDocument.IssueDateTime.Value, false));
//                            Writer.WriteEndElement(); // !ram:IssueDateTime
//                        }
//                        if (!String.IsNullOrEmpty(tradeLineItem.BuyerOrderReferencedDocument.ID))
//                        {
//                            Writer.WriteElementString("ram:ID", tradeLineItem.BuyerOrderReferencedDocument.ID);
//                        }
//
//                        Writer.WriteEndElement(); // !ram:BuyerOrderReferencedDocument
//                    }
//
//                    if (tradeLineItem.ContractReferencedDocument != null)
//                    {
//                        Writer.WriteStartElement("ram:ContractReferencedDocument");
//                        if (tradeLineItem.ContractReferencedDocument.IssueDateTime.HasValue)
//                        {
//                            Writer.WriteStartElement("ram:IssueDateTime");
//                            Writer.WriteValue(_formatDate(tradeLineItem.ContractReferencedDocument.IssueDateTime.Value, false));
//                            Writer.WriteEndElement(); // !ram:IssueDateTime
//                        }
//                        if (!String.IsNullOrEmpty(tradeLineItem.ContractReferencedDocument.ID))
//                        {
//                            Writer.WriteElementString("ram:ID", tradeLineItem.ContractReferencedDocument.ID);
//                        }
//
//                        Writer.WriteEndElement(); // !ram:ContractReferencedDocument
//                    }
//
//                    if (tradeLineItem.AdditionalReferencedDocuments != null)
//                    {
//                        foreach (AdditionalReferencedDocument document in tradeLineItem.AdditionalReferencedDocuments)
//                        {
//                            Writer.WriteStartElement("ram:AdditionalReferencedDocument");
//                            if (document.IssueDateTime.HasValue)
//                            {
//                                Writer.WriteStartElement("ram:IssueDateTime");
//                                Writer.WriteValue(_formatDate(document.IssueDateTime.Value, false));
//                                Writer.WriteEndElement(); // !ram:IssueDateTime
//                            }
//
//                            Writer.WriteElementString("ram:LineID", String.Format("{0}", tradeLineItem.AssociatedDocument?.LineID));
//
//                            if (!String.IsNullOrEmpty(document.ID))
//                            {
//                                Writer.WriteElementString("ram:ID", document.ID);
//                            }
//
//                            Writer.WriteElementString("ram:ReferenceTypeCode", document.ReferenceTypeCode.EnumToString());
//
//                            Writer.WriteEndElement(); // !ram:AdditionalReferencedDocument
//                        }
//                    }
//
//                    Writer.WriteStartElement("ram:GrossPriceProductTradePrice");
//                    _writeOptionalAmount(Writer, "ram:ChargeAmount", tradeLineItem.GrossUnitPrice, 4);
//                    if (tradeLineItem.UnitQuantity.HasValue)
//                    {
//                        _writeElementWithAttribute(Writer, "ram:BasisQuantity", "unitCode", tradeLineItem.UnitCode.EnumToString(), _formatDecimal(tradeLineItem.UnitQuantity.Value, 4));
//                    }
//
//                    foreach (TradeAllowanceCharge tradeAllowanceCharge in tradeLineItem.TradeAllowanceCharges)
//                    {
//                        Writer.WriteStartElement("ram:AppliedTradeAllowanceCharge");
//
//                        Writer.WriteStartElement("ram:ChargeIndicator", Profile.Comfort | Profile.Extended);
//                        Writer.WriteElementString("udt:Indicator", tradeAllowanceCharge.ChargeIndicator ? "true" : "false");
//                        Writer.WriteEndElement(); // !ram:ChargeIndicator
//
//                        Writer.WriteStartElement("ram:BasisAmount", Profile.Extended);
//                        Writer.WriteAttributeString("currencyID", tradeAllowanceCharge.Currency.EnumToString());
//                        Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.BasisAmount, 4));
//                        Writer.WriteEndElement();
//                        Writer.WriteStartElement("ram:ActualAmount", Profile.Comfort | Profile.Extended);
//                        Writer.WriteAttributeString("currencyID", tradeAllowanceCharge.Currency.EnumToString());
//                        Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ActualAmount, 4));
//                        Writer.WriteEndElement();
//
//                        _writeOptionalElementString(Writer, "ram:Reason", tradeAllowanceCharge.Reason, Profile.Comfort | Profile.Extended);
//
//                        Writer.WriteEndElement(); // !AppliedTradeAllowanceCharge
//                    }
//
//                    Writer.WriteEndElement(); // ram:GrossPriceProductTradePrice
//
//                    Writer.WriteStartElement("ram:NetPriceProductTradePrice");
//                    _writeOptionalAmount(Writer, "ram:ChargeAmount", tradeLineItem.NetUnitPrice, 4);
//
//                    if (tradeLineItem.UnitQuantity.HasValue)
//                    {
//                        _writeElementWithAttribute(Writer, "ram:BasisQuantity", "unitCode", tradeLineItem.UnitCode.EnumToString(), _formatDecimal(tradeLineItem.UnitQuantity.Value, 4));
//                    }
//                    Writer.WriteEndElement(); // ram:NetPriceProductTradePrice
//
//                    Writer.WriteEndElement(); // !ram:SpecifiedSupplyChainTradeAgreement
//                }
//
//                if (Descriptor.Profile != Profile.Basic)
//                {
//                    Writer.WriteStartElement("ram:SpecifiedSupplyChainTradeDelivery");
//                    _writeElementWithAttribute(Writer, "ram:BilledQuantity", "unitCode", tradeLineItem.UnitCode.EnumToString(), _formatDecimal(tradeLineItem.BilledQuantity, 4));
//
//                    if (tradeLineItem.DeliveryNoteReferencedDocument != null)
//                    {
//                        Writer.WriteStartElement("ram:DeliveryNoteReferencedDocument");
//                        if (tradeLineItem.DeliveryNoteReferencedDocument.IssueDateTime.HasValue)
//                        {
//                            Writer.WriteStartElement("ram:IssueDateTime");
//                            Writer.WriteValue(_formatDate(tradeLineItem.DeliveryNoteReferencedDocument.IssueDateTime.Value, false));
//                            Writer.WriteEndElement(); // !ram:IssueDateTime
//                        }
//                        if (!String.IsNullOrEmpty(tradeLineItem.DeliveryNoteReferencedDocument.ID))
//                        {
//                            Writer.WriteElementString("ram:ID", tradeLineItem.DeliveryNoteReferencedDocument.ID);
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
//                    Writer.WriteEndElement(); // !ram:SpecifiedSupplyChainTradeDelivery
//                }
//                else
//                {
//                    Writer.WriteStartElement("ram:SpecifiedSupplyChainTradeDelivery");
//                    _writeElementWithAttribute(Writer, "ram:BilledQuantity", "unitCode", tradeLineItem.UnitCode.EnumToString(), _formatDecimal(tradeLineItem.BilledQuantity, 4));
//                    Writer.WriteEndElement(); // !ram:SpecifiedSupplyChainTradeDelivery
//                }
//
//                Writer.WriteStartElement("ram:SpecifiedSupplyChainTradeSettlement");
//
//                if (Descriptor.Profile != Profile.Basic)
//                {
//                    Writer.WriteStartElement("ram:ApplicableTradeTax");
//                    Writer.WriteElementString("ram:TypeCode", tradeLineItem.TaxType.EnumToString());
//                    Writer.WriteElementString("ram:CategoryCode", tradeLineItem.TaxCategoryCode.EnumToString());
//                    Writer.WriteElementString("ram:ApplicablePercent", _formatDecimal(tradeLineItem.TaxPercent));
//                    Writer.WriteEndElement(); // !ram:ApplicableTradeTax
//                }
//
//                if (tradeLineItem.BillingPeriodStart.HasValue && tradeLineItem.BillingPeriodEnd.HasValue)
//                {
//                    Writer.WriteStartElement("ram:BillingSpecifiedPeriod", Profile.BasicWL | Profile.Basic | Profile.Comfort | Profile.Extended | Profile.XRechnung1 | Profile.XRechnung);
//
//                    Writer.WriteStartElement("ram:StartDateTime");
//                    _writeElementWithAttribute(Writer, "udt:DateTimeString", "format", "102", _formatDate(tradeLineItem.BillingPeriodStart.Value));
//                    Writer.WriteEndElement(); // !StartDateTime
//
//
//                    Writer.WriteStartElement("ram:EndDateTime");
//                    _writeElementWithAttribute(Writer, "udt:DateTimeString", "format", "102", _formatDate(tradeLineItem.BillingPeriodEnd.Value));
//                    Writer.WriteEndElement(); // !EndDateTime
//
//                    Writer.WriteEndElement(); // !BillingSpecifiedPeriod
//                }
//
//                Writer.WriteStartElement("ram:SpecifiedTradeSettlementMonetarySummation");
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
//                _writeElementWithAttribute(Writer, "ram:LineTotalAmount", "currencyID", this.Descriptor.Currency.EnumToString(), _formatDecimal(_total));
//                Writer.WriteEndElement(); // ram:SpecifiedTradeSettlementMonetarySummation
//                Writer.WriteEndElement(); // !ram:SpecifiedSupplyChainTradeSettlement
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
//                Writer.WriteEndElement(); // !ram:SpecifiedTradeProduct
//                Writer.WriteEndElement(); // !ram:IncludedSupplyChainTradeLineItem
//            } // !foreach(tradeLineItem)
//
//            Writer.WriteEndElement(); // !ram:SpecifiedSupplyChainTradeTransaction
//            #endregion
//
//            Writer.WriteEndElement(); // !ram:Invoice
//            Writer.WriteEndDocument();
//            Writer.Flush();
//
//            stream.Seek(streamPosition, SeekOrigin.Begin);








//
//  // write data
//  var
//    streamPosition: Int64;
//
//    streamPosition := stream.Position;
//
//    Descriptor := descriptor;
//    Writer := TProfileAwareXMLWriter.Create(stream, TEncoding.UTF8, Descriptor.Profile);
//    try
//      Writer.Formatting := TXMLFormatting.Indented;
//      Writer.WriteStartDocument;
//
//      // Kopfbereich
//      Writer.WriteStartElement('rsm:CrossIndustryInvoice');
//      Writer.WriteAttributeString('xmlns', 'a', '', 'urn:un:unece:uncefact:data:standard:QualifiedDataType:100');
//      Writer.WriteAttributeString('xmlns', 'rsm', '', 'urn:un:unece:uncefact:data:standard:CrossIndustryInvoice:100');
//      Writer.WriteAttributeString('xmlns', 'qdt', '', 'urn:un:unece:uncefact:data:standard:QualifiedDataType:100');
//      Writer.WriteAttributeString('xmlns', 'ram', '', 'urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:100');
//      Writer.WriteAttributeString('xmlns', 'xs', '', 'http://www.w3.org/2001/XMLSchema');
//      Writer.WriteAttributeString('xmlns', 'udt', '', 'urn:un:unece:uncefact:data:standard:UnqualifiedDataType:100');
//
//      // SpecifiedExchangedDocumentContext
//      Writer.WriteStartElement('rsm:ExchangedDocumentContext');
//
//      if Descriptor.IsTest then
//      begin
//        Writer.WriteStartElement('ram:TestIndicator');
//        Writer.WriteElementString('udt:Indicator', 'true');
//        Writer.WriteEndElement(); // !ram:TestIndicator
//      end;
//
//      if not string.IsNullOrEmpty(Descriptor.BusinessProcess) then
//      begin
//        Writer.WriteStartElement('ram:BusinessProcessSpecifiedDocumentContextParameter');
//        Writer.WriteElementString('ram:ID', Descriptor.BusinessProcess);
//        Writer.WriteEndElement(); // !ram:BusinessProcessSpecifiedDocumentContextParameter
//      end;
//
//      Writer.WriteStartElement('ram:GuidelineSpecifiedDocumentContextParameter');
//      Writer.WriteElementString('ram:ID', TZUGFeRDProfile.EnumToString(TZUGFeRDVersion.Version20));
//      Writer.WriteEndElement(); // !ram:GuidelineSpecifiedDocumentContextParameter
//      Writer.WriteEndElement(); // !rsm:ExchangedDocumentContext
//
//      Writer.WriteStartElement('rsm:ExchangedDocument');
//      Writer.WriteElementString('ram:ID', Descriptor.InvoiceNo);
//      Writer.WriteElementString('ram:Name', _translateInvoiceType(Descriptor.&Type), TZUGFeRDProfile.Extended);
//      Writer.WriteElementString('ram:TypeCode', Format('%d', [_encodeInvoiceType(Descriptor.&Type)]));
//
//
//if Descriptor.InvoiceDate.HasValue then
//begin
//  Writer.WriteStartElement('ram:IssueDateTime');
//  Writer.WriteStartElement('udt:DateTimeString');
//  Writer.WriteAttributeString('format', '102');
//  Writer.WriteValue(_formatDate(Descriptor.InvoiceDate.Value));
//  Writer.WriteEndElement(); // !udt:DateTimeString
//  Writer.WriteEndElement(); // !IssueDateTime
//end;
//_writeNotes(Writer, Descriptor.Notes);
//Writer.WriteEndElement(); // !rsm:ExchangedDocument
//
//
//// SpecifiedSupplyChainTradeTransaction
//Writer.WriteStartElement('rsm:SupplyChainTradeTransaction');
//
//for var tradeLineItem: TradeLineItem in Descriptor.TradeLineItems do
//begin
//  Writer.WriteStartElement('ram:IncludedSupplyChainTradeLineItem');
//
//  if Assigned(tradeLineItem.AssociatedDocument) then
//  begin
//    Writer.WriteStartElement('ram:AssociatedDocumentLineDocument');
//    if not string.IsNullOrEmpty(tradeLineItem.AssociatedDocument.LineID) then
//      Writer.WriteElementString('ram:LineID', tradeLineItem.AssociatedDocument.LineID);
//    _writeNotes(Writer, tradeLineItem.AssociatedDocument.Notes);
//    Writer.WriteEndElement(); // ram:AssociatedDocumentLineDocument
//  end;
//
//  // handelt es sich um einen Kommentar?
//  if (Assigned(tradeLineItem.AssociatedDocument?.Notes) and (tradeLineItem.BilledQuantity = 0) and string.IsNullOrEmpty(tradeLineItem.Description)) then
//  begin
//    Writer.WriteEndElement(); // !ram:IncludedSupplyChainTradeLineItem
//    Continue;
//  end;
//
//  Writer.WriteStartElement('ram:SpecifiedTradeProduct');
//  if (Assigned(tradeLineItem.GlobalID) and (tradeLineItem.GlobalID.SchemeID <> GlobalIDSchemeIdentifiers.Unknown) and not string.IsNullOrEmpty(tradeLineItem.GlobalID.ID)) then
//    _writeElementWithAttribute(Writer, 'ram:GlobalID', 'schemeID', tradeLineItem.GlobalID.SchemeID.EnumToString(), tradeLineItem.GlobalID.ID);
//
//  _writeOptionalElementString(Writer, 'ram:SellerAssignedID', tradeLineItem.SellerAssignedID);
//  _writeOptionalElementString(Writer, 'ram:BuyerAssignedID', tradeLineItem.BuyerAssignedID);
//  _writeOptionalElementString(Writer, 'ram:Name', tradeLineItem.Name);
//  _writeOptionalElementString(Writer, 'ram:Description', tradeLineItem.Description);
//
//  if Assigned(tradeLineItem.ApplicableProductCharacteristics) and (tradeLineItem.ApplicableProductCharacteristics.Count > 0) then
//  begin
//    for var productCharacteristic in tradeLineItem.ApplicableProductCharacteristics do
//    begin
//      Writer.WriteStartElement('ram:ApplicableProductCharacteristic');
//      _writeOptionalElementString(Writer, 'ram:Description', productCharacteristic.Description);
//      _writeOptionalElementString(Writer, 'ram:Value', productCharacteristic.Value);
//      Writer.WriteEndElement(); // !ram:ApplicableProductCharacteristic
//    end;
//  end;
//
//  Writer.WriteEndElement(); // !ram:SpecifiedTradeProduct
//
//  Writer.WriteStartElement('ram:SpecifiedLineTradeAgreement', [Profile.Basic, Profile.Comfort, Profile.Extended]);
//
//  if Assigned(tradeLineItem.BuyerOrderReferencedDocument) then
//  begin
//    Writer.WriteStartElement('ram:BuyerOrderReferencedDocument', [Profile.Comfort, Profile.Extended, Profile.XRechnung1, Profile.XRechnung]);
//
//    // IssuerAssignedID
//    // Bestellnummer
//    if not string.IsNullOrEmpty(tradeLineItem.BuyerOrderReferencedDocument.ID) then
//      Writer.WriteElementString('ram:IssuerAssignedID', tradeLineItem.BuyerOrderReferencedDocument.ID);
//
//    // LineID
//    // Referenz zur Bestellposition
//    // ToDo: fehlt ganz
//
//    // IssueDateTime
//    if tradeLineItem.BuyerOrderReferencedDocument.IssueDateTime.HasValue then
//    begin
//      Writer.WriteStartElement('ram:FormattedIssueDateTime');
//      Writer.WriteStartElement('qdt:DateTimeString');
//      Writer.WriteAttributeString('format', '102');
//      Writer.WriteValue(_formatDate(tradeLineItem.BuyerOrderReferencedDocument.IssueDateTime.Value));
//      Writer.WriteEndElement(); // !qdt:DateTimeString
//      Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//    end;
//
//    Writer.WriteEndElement(); // !ram:BuyerOrderReferencedDocument
//  end;
//
//  if Assigned(tradeLineItem.ContractReferencedDocument) then
//  begin
//    Writer.WriteStartElement('ram:ContractReferencedDocument', [Profile.Extended]);
//    if tradeLineItem.ContractReferencedDocument.IssueDateTime.HasValue then
//    begin
//      Writer.WriteStartElement('ram:FormattedIssueDateTime');
//      Writer.WriteStartElement('qdt:DateTimeString');
//      Writer.WriteAttributeString('format', '102');
//      Writer.WriteValue(_formatDate(tradeLineItem.ContractReferencedDocument.IssueDateTime.Value));
//      Writer.WriteEndElement(); // !udt:DateTimeString
//      Writer.WriteEndElement(); // !ram:IssueDateTime
//    end;
//    if not string.IsNullOrEmpty(tradeLineItem.ContractReferencedDocument.ID) then
//      Writer.WriteElementString('ram:IssuerAssignedID', tradeLineItem.ContractReferencedDocument.ID);
//
//    Writer.WriteEndElement(); // !ram:ContractReferencedDocument(Extended)
//  end;
//
//  if Assigned(tradeLineItem.AdditionalReferencedDocuments) then
//  begin
//    for var document in tradeLineItem.AdditionalReferencedDocuments do
//    begin
//      Writer.WriteStartElement('ram:AdditionalReferencedDocument');
//      if document.IssueDateTime.HasValue then
//      begin
//        Writer.WriteStartElement('ram:FormattedIssueDateTime');
//        Writer.WriteStartElement('qdt:DateTimeString');
//        Writer.WriteValue(_formatDate(document.IssueDateTime.Value));
//        Writer.WriteEndElement(); // !qdt:DateTimeString
//        Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//      end;
//
//      Writer.WriteElementString('ram:LineID', Format('%d', [tradeLineItem.AssociatedDocument?.LineID]));
//
//      if not string.IsNullOrEmpty(document.ID) then
//        Writer.WriteElementString('ram:IssuerAssignedID', document.ID);
//
//      Writer.WriteElementString('ram:ReferenceTypeCode', document.ReferenceTypeCode.EnumToString());
//
//      Writer.WriteEndElement(); // !ram:AdditionalReferencedDocument
//    end; // !foreach(document)
//  end;
//end;
//
//
//Writer.WriteStartElement('ram:GrossPriceProductTradePrice');
//_writeOptionalAmount(Writer, 'ram:ChargeAmount', tradeLineItem.GrossUnitPrice, numDecimals: 4, forceCurrency: false);
//
//if tradeLineItem.UnitQuantity.HasValue then
//  _writeElementWithAttribute(Writer, 'ram:BasisQuantity', 'unitCode', tradeLineItem.UnitCode.EnumToString(), _formatDecimal(tradeLineItem.UnitQuantity.Value, 4));
//
//for var tradeAllowanceCharge in tradeLineItem.TradeAllowanceCharges do
//begin
//  Writer.WriteStartElement('ram:AppliedTradeAllowanceCharge');
//
//  Writer.WriteStartElement('ram:ChargeIndicator');
//  Writer.WriteElementString('udt:Indicator', BoolToStr(tradeAllowanceCharge.ChargeIndicator, true));
//  Writer.WriteEndElement(); // !ram:ChargeIndicator
//
//  Writer.WriteStartElement('ram:BasisAmount');
//  Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.BasisAmount, 4));
//  Writer.WriteEndElement();
//
//  Writer.WriteStartElement('ram:ActualAmount');
//  Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ActualAmount, 4));
//  Writer.WriteEndElement();
//
//  _writeOptionalElementString(Writer, 'ram:Reason', tradeAllowanceCharge.Reason);
//
//  Writer.WriteEndElement(); // !AppliedTradeAllowanceCharge
//end;
//
//Writer.WriteEndElement(); // ram:GrossPriceProductTradePrice
//
//Writer.WriteStartElement('ram:NetPriceProductTradePrice');
//_writeOptionalAmount(Writer, 'ram:ChargeAmount', tradeLineItem.NetUnitPrice, numDecimals: 4, forceCurrency: false);
//
//if tradeLineItem.UnitQuantity.HasValue then
//  _writeElementWithAttribute(Writer, 'ram:BasisQuantity', 'unitCode', tradeLineItem.UnitCode.EnumToString(), _formatDecimal(tradeLineItem.UnitQuantity.Value, 4));
//
//Writer.WriteEndElement(); // ram:NetPriceProductTradePrice
//
//Writer.WriteEndElement(); // !ram:SpecifiedLineTradeAgreement
//
//if Descriptor.Profile <> Profile.Basic then
//begin
//  Writer.WriteStartElement('ram:SpecifiedLineTradeDelivery');
//  _writeElementWithAttribute(Writer, 'ram:BilledQuantity', 'unitCode', tradeLineItem.UnitCode.EnumToString(), _formatDecimal(tradeLineItem.BilledQuantity, 4));
//
//  if Assigned(tradeLineItem.DeliveryNoteReferencedDocument) then
//  begin
//    Writer.WriteStartElement('ram:DeliveryNoteReferencedDocument');
//    if tradeLineItem.DeliveryNoteReferencedDocument.IssueDateTime.HasValue then
//    begin
//      Writer.WriteStartElement('ram:FormattedIssueDateTime');
//      Writer.WriteStartElement('qdt:DateTimeString');
//      Writer.WriteAttributeString('format', '102');
//      Writer.WriteValue(_formatDate(tradeLineItem.DeliveryNoteReferencedDocument.IssueDateTime.Value));
//      Writer.WriteEndElement(); // !qdt:DateTimeString
//      Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//    end;
//
//    if not String.IsNullOrEmpty(tradeLineItem.DeliveryNoteReferencedDocument.ID) then
//      Writer.WriteElementString('ram:IssuerAssignedID', tradeLineItem.DeliveryNoteReferencedDocument.ID);
//
//    Writer.WriteEndElement(); // !ram:DeliveryNoteReferencedDocument
//  end;
//
//  if tradeLineItem.ActualDeliveryDate.HasValue then
//  begin
//    Writer.WriteStartElement('ram:ActualDeliverySupplyChainEvent');
//    Writer.WriteStartElement('ram:OccurrenceDateTime');
//    Writer.WriteStartElement('udt:DateTimeString');
//    Writer.WriteAttributeString('format', '102');
//    Writer.WriteValue(_formatDate(tradeLineItem.ActualDeliveryDate.Value));
//    Writer.WriteEndElement(); // "udt:DateTimeString
//    Writer.WriteEndElement(); // !OccurrenceDateTime()
//    Writer.WriteEndElement(); // !ActualDeliverySupplyChainEvent
//  end;
//
//  Writer.WriteEndElement(); // !ram:SpecifiedLineTradeDelivery
//end
//else
//begin
//  Writer.WriteStartElement('ram:SpecifiedLineTradeDelivery');
//  _writeElementWithAttribute(Writer, 'ram:BilledQuantity', 'unitCode', tradeLineItem.UnitCode.EnumToString(), _formatDecimal(tradeLineItem.BilledQuantity, 4));
//  Writer.WriteEndElement(); // !ram:SpecifiedLineTradeDelivery
//end;
//
//Writer.WriteStartElement('ram:SpecifiedLineTradeSettlement');
//
//Writer.WriteStartElement('ram:ApplicableTradeTax', [Profile.Basic, Profile.Comfort, Profile.Extended]);
//Writer.WriteElementString('ram:TypeCode', tradeLineItem.TaxType.EnumToString());
//Writer.WriteElementString('ram:CategoryCode', tradeLineItem.TaxCategoryCode.EnumToString());
//Writer.WriteElementString('ram:RateApplicablePercent', _formatDecimal(tradeLineItem.TaxPercent));
//Writer.WriteEndElement(); // !ram:ApplicableTradeTax
//
//if tradeLineItem.BillingPeriodStart.HasValue or tradeLineItem.BillingPeriodEnd.HasValue then
//begin
//  Writer.WriteStartElement('ram:BillingSpecifiedPeriod', [Profile.BasicWL, Profile.Basic, Profile.Comfort, Profile.Extended, Profile.XRechnung1, Profile.XRechnung]);
//  if tradeLineItem.BillingPeriodStart.HasValue then
//  begin
//    Writer.WriteStartElement('ram:StartDateTime');
//    _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(tradeLineItem.BillingPeriodStart.Value));
//    Writer.WriteEndElement(); // !StartDateTime
//  end;
//
//  if tradeLineItem.BillingPeriodEnd.HasValue then
//  begin
//    Writer.WriteStartElement('ram:EndDateTime');
//    _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(tradeLineItem.BillingPeriodEnd.Value));
//    Writer.WriteEndElement(); // !EndDateTime
//  end;
//  Writer.WriteEndElement(); // !BillingSpecifiedPeriod
//end;
//
//Writer.WriteStartElement('ram:SpecifiedTradeSettlementLineMonetarySummation');
//
//var _total: Decimal := 0;
//if tradeLineItem.LineTotalAmount.HasValue then
//  _total := tradeLineItem.LineTotalAmount.Value
//else if tradeLineItem.NetUnitPrice.HasValue then
//  _total := tradeLineItem.NetUnitPrice.Value * tradeLineItem.BilledQuantity;
//
//Writer.WriteElementString('ram:LineTotalAmount', _formatDecimal(_total));
//
//Writer.WriteEndElement(); // ram:SpecifiedTradeSettlementLineMonetarySummation
//Writer.WriteEndElement(); // !ram:SpecifiedLineTradeSettlement
//
//Writer.WriteEndElement(); // !ram:IncludedSupplyChainTradeLineItem
//end; // !foreach(tradeLineItem)
//
//Writer.WriteStartElement('ram:ApplicableHeaderTradeAgreement');
//if not String.IsNullOrEmpty(this.Descriptor.ReferenceOrderNo) then
//  Writer.WriteElementString('ram:BuyerReference', this.Descriptor.ReferenceOrderNo);
//
//_writeOptionalParty(Writer, 'ram:SellerTradeParty', this.Descriptor.Seller, this.Descriptor.SellerContact, TaxRegistrations: this.Descriptor.SellerTaxRegistration);
//_writeOptionalParty(Writer, 'ram:BuyerTradeParty', this.Descriptor.Buyer, this.Descriptor.BuyerContact, TaxRegistrations: this.Descriptor.BuyerTaxRegistration);
//
//if Assigned(this.Descriptor.SellerOrderReferencedDocument) and not string.IsNullOrEmpty(Descriptor.SellerOrderReferencedDocument.ID) then
//begin
//  Writer.WriteStartElement('ram:SellerOrderReferencedDocument', [Profile.Comfort, Profile.Extended]);
//  Writer.WriteElementString('ram:IssuerAssignedID', this.Descriptor.SellerOrderReferencedDocument.ID);
//  if this.Descriptor.SellerOrderReferencedDocument.IssueDateTime.HasValue then
//  begin
//    Writer.WriteStartElement('ram:FormattedIssueDateTime', Profile.Extended);
//    Writer.WriteStartElement('qdt:DateTimeString');
//    Writer.WriteAttributeString('format', '102');
//    Writer.WriteValue(_formatDate(this.Descriptor.SellerOrderReferencedDocument.IssueDateTime.Value));
//    Writer.WriteEndElement(); // !qdt:DateTimeString
//    Writer.WriteEndElement(); // !IssueDateTime()
//  end;
//
//  Writer.WriteEndElement(); // !SellerOrderReferencedDocument
//end;
//
//if not String.IsNullOrEmpty(this.Descriptor.OrderNo) then
//begin
//  Writer.WriteStartElement('ram:BuyerOrderReferencedDocument');
//  Writer.WriteElementString('ram:IssuerAssignedID', this.Descriptor.OrderNo);
//  if this.Descriptor.OrderDate.HasValue then
//  begin
//    Writer.WriteStartElement('ram:FormattedIssueDateTime');
//    Writer.WriteStartElement('qdt:DateTimeString');
//    Writer.WriteAttributeString('format', '102');
//    Writer.WriteValue(_formatDate(this.Descriptor.OrderDate.Value));
//    Writer.WriteEndElement(); // !qdt:DateTimeString
//    Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//  end;
//  Writer.WriteEndElement(); // !BuyerOrderReferencedDocument
//end;
//
//if Assigned(this.Descriptor.AdditionalReferencedDocuments) then
//begin
//  for var document in this.Descriptor.AdditionalReferencedDocuments do
//  begin
//    Writer.WriteStartElement('ram:AdditionalReferencedDocument');
//    if document.IssueDateTime.HasValue then
//    begin
//      Writer.WriteStartElement('ram:FormattedIssueDateTime');
//      Writer.WriteStartElement('qdt:DateTimeString');
//      Writer.WriteValue(_formatDate(document.IssueDateTime.Value));
//      Writer.WriteEndElement(); // !udt:DateTimeString
//      Writer.WriteEndElement(); // !FormattedIssueDateTime
//    end;
//
//    if document.ReferenceTypeCode <> ReferenceTypeCodes.Unknown then
//      Writer.WriteElementString('ram:TypeCode', document.ReferenceTypeCode.EnumToString());
//
//    Writer.WriteElementString('ram:ID', document.ID);
//    Writer.WriteEndElement(); // !ram:AdditionalReferencedDocument
//  end; // !foreach(document)
//end;
//
//Writer.WriteEndElement(); // !ApplicableHeaderTradeAgreement
//
//Writer.WriteStartElement("ram:ApplicableHeaderTradeDelivery"); // Pflichteintrag
//
//
//Writer.WriteEndElement(); // !ApplicableHeaderTradeAgreement
//
//Writer.WriteStartElement('ram:ApplicableHeaderTradeDelivery'); // Pflichteintrag
//
//if Descriptor.Profile = Profile.Extended then
//begin
//  _writeOptionalParty(Writer, 'ram:ShipToTradeParty', this.Descriptor.ShipTo);
//  _writeOptionalParty(Writer, 'ram:ShipFromTradeParty', this.Descriptor.ShipFrom);
//end;
//
//if this.Descriptor.ActualDeliveryDate.HasValue then
//begin
//  Writer.WriteStartElement('ram:ActualDeliverySupplyChainEvent');
//  Writer.WriteStartElement('ram:OccurrenceDateTime');
//  Writer.WriteStartElement('udt:DateTimeString');
//  Writer.WriteAttributeString('format', '102');
//  Writer.WriteValue(_formatDate(this.Descriptor.ActualDeliveryDate.Value));
//  Writer.WriteEndElement(); // "udt:DateTimeString
//  Writer.WriteEndElement(); // !OccurrenceDateTime()
//  Writer.WriteEndElement(); // !ActualDeliverySupplyChainEvent
//end;
//
//if this.Descriptor.DeliveryNoteReferencedDocument <> nil then
//begin
//  Writer.WriteStartElement('ram:DeliveryNoteReferencedDocument');
//
//  if this.Descriptor.DeliveryNoteReferencedDocument.IssueDateTime.HasValue then
//  begin
//    Writer.WriteStartElement('ram:FormattedIssueDateTime');
//    Writer.WriteValue(_formatDate(this.Descriptor.DeliveryNoteReferencedDocument.IssueDateTime.Value, False));
//    Writer.WriteEndElement(); // !IssueDateTime
//  end;
//
//  Writer.WriteElementString('ram:IssuerAssignedID', this.Descriptor.DeliveryNoteReferencedDocument.ID);
//  Writer.WriteEndElement(); // !DeliveryNoteReferencedDocument
//end;
//
//Writer.WriteEndElement(); // !ApplicableHeaderTradeDelivery
//
//Writer.WriteStartElement('ram:ApplicableHeaderTradeSettlement');
//// order of sub-elements of ApplicableHeaderTradeSettlement:
////   1. CreditorReferenceID (optional)
////   2. PaymentReference (optional)
////   3. TaxCurrencyCode (optional)
////   4. InvoiceCurrencyCode (optional)
////   5. InvoiceIssuerReference (optional)
////   6. InvoicerTradeParty (optional)
////   7. InvoiceeTradeParty (optional)
////   8. PayeeTradeParty (optional)
////   9. TaxApplicableTradeCurrencyExchange (optional)
////  10. SpecifiedTradeSettlementPaymentMeans (optional)
////  11. ApplicableTradeTax (optional)
////  12. BillingSpecifiedPeriod (optional)
////  13. SpecifiedTradeAllowanceCharge (optional)
////  14. SpecifiedLogisticsServiceCharge (optional)
////  15. SpecifiedTradePaymentTerms (optional)
////  16. SpecifiedTradeSettlementHeaderMonetarySummation
////  17. InvoiceReferencedDocument (optional)
////  18. ReceivableSpecifiedTradeAccountingAccount (optional)
////  19. SpecifiedAdvancePayment (optional)
//
////   1. CreditorReferenceID (optional)
//if not String.IsNullOrEmpty(this.Descriptor.PaymentMeans?.SEPACreditorIdentifier) then
//  _writeOptionalElementString(Writer, 'ram:CreditorReferenceID', Descriptor.PaymentMeans?.SEPACreditorIdentifier, [Profile.BasicWL, Profile.Basic, Profile.Comfort, Profile.Extended, Profile.XRechnung, Profile.XRechnung1]);
//
////   2. PaymentReference (optional)
//if not String.IsNullOrEmpty(this.Descriptor.PaymentReference) then
//  _writeOptionalElementString(Writer, 'ram:PaymentReference', this.Descriptor.PaymentReference);
//
////   4. InvoiceCurrencyCode (optional)
//Writer.WriteElementString('ram:InvoiceCurrencyCode', this.Descriptor.Currency.EnumToString());
//
////   7. InvoiceeTradeParty (optional)
//if Descriptor.Profile = Profile.Extended then
//  _writeOptionalParty(Writer, 'ram:InvoiceeTradeParty', this.Descriptor.Invoicee);
//
////   8. PayeeTradeParty (optional)
//if Descriptor.Profile <> Profile.Minimum then
//  _writeOptionalParty(Writer, 'ram:PayeeTradeParty', this.Descriptor.Payee);
//
////  10. SpecifiedTradeSettlementPaymentMeans (optional)
//if (this.Descriptor.CreditorBankAccounts.Count = 0) and (this.Descriptor.DebitorBankAccounts.Count = 0) then
//begin
//  if this.Descriptor.PaymentMeans <> nil then
//  begin
//    Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans');
//
//    if (this.Descriptor.PaymentMeans <> nil) and (this.Descriptor.PaymentMeans.TypeCode <> PaymentMeansTypeCodes.Unknown) then
//    begin
//      Writer.WriteElementString('ram:TypeCode', this.Descriptor.PaymentMeans.TypeCode.EnumToString());
//      Writer.WriteElementString('ram:Information', this.Descriptor.PaymentMeans.Information);
//
//      if this.Descriptor.PaymentMeans.FinancialCard <> nil then
//      begin
//        Writer.WriteStartElement('ram:ApplicableTradeSettlementFinancialCard', [Profile.Comfort, Profile.Extended]);
//        Writer.WriteElementString('ram:ID', Descriptor.PaymentMeans.FinancialCard.Id);
//        Writer.WriteElementString('ram:CardholderName', Descriptor.PaymentMeans.FinancialCard.CardholderName);
//        Writer.WriteEndElement(); // !ram:ApplicableTradeSettlementFinancialCard
//      end;
//    end;
//    Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
//  end;
//end
//else
//begin
//  for var account in this.Descriptor.CreditorBankAccounts do
//  begin
//    Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans');
//
//    if (this.Descriptor.PaymentMeans <> nil) and (this.Descriptor.PaymentMeans.TypeCode <> PaymentMeansTypeCodes.Unknown) then
//    begin
//      Writer.WriteElementString('ram:TypeCode', this.Descriptor.PaymentMeans.TypeCode.EnumToString());
//      Writer.WriteElementString('ram:Information', this.Descriptor.PaymentMeans.Information);
//
//      if this.Descriptor.PaymentMeans.FinancialCard <> nil then
//      begin
//        Writer.WriteStartElement('ram:ApplicableTradeSettlementFinancialCard', [Profile.Comfort, Profile.Extended]);
//        Writer.WriteElementString('ram:ID', Descriptor.PaymentMeans.FinancialCard.Id);
//        Writer.WriteElementString('ram:CardholderName', Descriptor.PaymentMeans.FinancialCard.CardholderName);
//        Writer.WriteEndElement(); // !ram:ApplicableTradeSettlementFinancialCard
//      end;
//    end;
//
//    Writer.WriteStartElement('ram:PayeePartyCreditorFinancialAccount');
//    Writer.WriteElementString('ram:IBANID', account.IBAN);
//    if not String.IsNullOrEmpty(account.Name) then
//      Writer.WriteElementString('ram:AccountName', account.Name);
//    if not String.IsNullOrEmpty(account.ID) then
//      Writer.WriteElementString('ram:ProprietaryID', account.ID);
//    Writer.WriteEndElement(); // !PayeePartyCreditorFinancialAccount
//
//    Writer.WriteStartElement('ram:PayeeSpecifiedCreditorFinancialInstitution');
//    Writer.WriteElementString('ram:BICID', account.BIC);
//
//    if not String.IsNullOrEmpty(account.Bankleitzahl) then
//      Writer.WriteElementString('ram:GermanBankleitzahlID', account.Bankleitzahl);
//
//    Writer.WriteEndElement(); // !PayeeSpecifiedCreditorFinancialInstitution
//    Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
//  end;
//
//  for var account in this.Descriptor.DebitorBankAccounts do
//  begin
//    Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans');
//
//    if (this.Descriptor.PaymentMeans <> nil) and (this.Descriptor.PaymentMeans.TypeCode <> PaymentMeansTypeCodes.Unknown) then
//    begin
//      Writer.WriteElementString('ram:TypeCode', this.Descriptor.PaymentMeans.TypeCode.EnumToString());
//      Writer.WriteElementString('ram:Information', this.Descriptor.PaymentMeans.Information);
//    end;
//
//    Writer.WriteStartElement('ram:PayerPartyDebtorFinancialAccount');
//    Writer.WriteElementString('ram:IBANID', account.IBAN);
//    if not String.IsNullOrEmpty(account.ID) then
//      Writer.WriteElementString('ram:ProprietaryID', account.ID);
//    Writer.WriteEndElement(); // !PayerPartyDebtorFinancialAccount
//
//    if (not String.IsNullOrEmpty(account.BIC)) or
//       (not String.IsNullOrEmpty(account.Bankleitzahl)) or
//       (not String.IsNullOrEmpty(account.BankName)) then
//    begin
//      Writer.WriteStartElement('ram:PayerSpecifiedDebtorFinancialInstitution');
//
//      if not String.IsNullOrEmpty(account.BIC) then
//        Writer.WriteElementString('ram:BICID', account.BIC);
//
//      if not String.IsNullOrEmpty(account.Bankleitzahl) then
//        Writer.WriteElementString('ram:GermanBankleitzahlID', account.Bankleitzahl);
//
//      if not String.IsNullOrEmpty(account.BankName) then
//        Writer.WriteElementString('ram:Name', account.BankName);
//
//      Writer.WriteEndElement(); // !PayerSpecifiedDebtorFinancialInstitution
//    end;
//
//    Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
//  end;
//end;
//
//
//
//
//Writer.WriteEndElement(); // !ApplicableHeaderTradeAgreement
//
//Writer.WriteStartElement('ram:ApplicableHeaderTradeDelivery'); // Pflichteintrag
//
//if Descriptor.Profile = Profile.Extended then
//begin
//  _writeOptionalParty(Writer, 'ram:ShipToTradeParty', this.Descriptor.ShipTo);
//  _writeOptionalParty(Writer, 'ram:ShipFromTradeParty', this.Descriptor.ShipFrom);
//end;
//
//if this.Descriptor.ActualDeliveryDate.HasValue then
//begin
//  Writer.WriteStartElement('ram:ActualDeliverySupplyChainEvent');
//  Writer.WriteStartElement('ram:OccurrenceDateTime');
//  Writer.WriteStartElement('udt:DateTimeString');
//  Writer.WriteAttributeString('format', '102');
//  Writer.WriteValue(_formatDate(this.Descriptor.ActualDeliveryDate.Value));
//  Writer.WriteEndElement(); // "udt:DateTimeString
//  Writer.WriteEndElement(); // !OccurrenceDateTime()
//  Writer.WriteEndElement(); // !ActualDeliverySupplyChainEvent
//end;
//
//if this.Descriptor.DeliveryNoteReferencedDocument <> nil then
//begin
//  Writer.WriteStartElement('ram:DeliveryNoteReferencedDocument');
//
//  if this.Descriptor.DeliveryNoteReferencedDocument.IssueDateTime.HasValue then
//  begin
//    Writer.WriteStartElement('ram:FormattedIssueDateTime');
//    Writer.WriteValue(_formatDate(this.Descriptor.DeliveryNoteReferencedDocument.IssueDateTime.Value, false));
//    Writer.WriteEndElement(); // !IssueDateTime
//  end;
//
//  Writer.WriteElementString('ram:IssuerAssignedID', this.Descriptor.DeliveryNoteReferencedDocument.ID);
//  Writer.WriteEndElement(); // !DeliveryNoteReferencedDocument
//end;
//
//Writer.WriteEndElement(); // !ApplicableHeaderTradeDelivery
//
//Writer.WriteStartElement('ram:ApplicableHeaderTradeSettlement');
//// order of sub-elements of ApplicableHeaderTradeSettlement:
////   1. CreditorReferenceID (optional)
////   2. PaymentReference (optional)
////   3. TaxCurrencyCode (optional)
////   4. InvoiceCurrencyCode (optional)
////   5. InvoiceIssuerReference (optional)
////   6. InvoicerTradeParty (optional)
////   7. InvoiceeTradeParty (optional)
////   8. PayeeTradeParty (optional)
////   9. TaxApplicableTradeCurrencyExchange (optional)
////  10. SpecifiedTradeSettlementPaymentMeans (optional)
////  11. ApplicableTradeTax (optional)
////  12. BillingSpecifiedPeriod (optional)
////  13. SpecifiedTradeAllowanceCharge (optional)
////  14. SpecifiedLogisticsServiceCharge (optional)
////  15. SpecifiedTradePaymentTerms (optional)
////  16. SpecifiedTradeSettlementHeaderMonetarySummation
////  17. InvoiceReferencedDocument (optional)
////  18. ReceivableSpecifiedTradeAccountingAccount (optional)
////  19. SpecifiedAdvancePayment (optional)
//
////   1. CreditorReferenceID (optional)
//if not String.IsNullOrEmpty(this.Descriptor.PaymentMeans?.SEPACreditorIdentifier) then
//  _writeOptionalElementString(Writer, 'ram:CreditorReferenceID', Descriptor.PaymentMeans?.SEPACreditorIdentifier, [Profile.BasicWL, Profile.Basic, Profile.Comfort, Profile.Extended, Profile.XRechnung, Profile.XRechnung1]);
//
////   2. PaymentReference (optional)
//if not String.IsNullOrEmpty(this.Descriptor.PaymentReference) then
//  _writeOptionalElementString(Writer, 'ram:PaymentReference', this.Descriptor.PaymentReference);
//
////   4. InvoiceCurrencyCode (optional)
//Writer.WriteElementString('ram:Invoice
//
//CurrencyCode', this.Descriptor.Currency.EnumToString());
//
////   7. InvoiceeTradeParty (optional)
//if Descriptor.Profile = Profile.Extended then
//  _writeOptionalParty(Writer, 'ram:InvoiceeTradeParty', this.Descriptor.Invoicee);
//
////   8. PayeeTradeParty (optional)
//if Descriptor.Profile <> Profile.Minimum then
//  _writeOptionalParty(Writer, 'ram:PayeeTradeParty', this.Descriptor.Payee);
//
////  10. SpecifiedTradeSettlementPaymentMeans (optional)
//if (this.Descriptor.CreditorBankAccounts.Count = 0) and (this.Descriptor.DebitorBankAccounts.Count = 0) then
//begin
//  if this.Descriptor.PaymentMeans <> nil then
//  begin
//    Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans');
//
//    if (this.Descriptor.PaymentMeans <> nil) and (this.Descriptor.PaymentMeans.TypeCode <> PaymentMeansTypeCodes.Unknown) then
//    begin
//      Writer.WriteElementString('ram:TypeCode', this.Descriptor.PaymentMeans.TypeCode.EnumToString());
//      Writer.WriteElementString('ram:Information', this.Descriptor.PaymentMeans.Information);
//
//      if this.Descriptor.PaymentMeans.FinancialCard <> nil then
//      begin
//        Writer.WriteStartElement('ram:ApplicableTradeSettlementFinancialCard', [Profile.Comfort, Profile.Extended]);
//        Writer.WriteElementString('ram:ID', Descriptor.PaymentMeans.FinancialCard.Id);
//        Writer.WriteElementString('ram:CardholderName', Descriptor.PaymentMeans.FinancialCard.CardholderName);
//        Writer.WriteEndElement(); // !ram:ApplicableTradeSettlementFinancialCard
//      end;
//    end;
//    Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
//  end;
//end
//else
//begin
//  for var account in this.Descriptor.CreditorBankAccounts do
//  begin
//    Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans');
//
//    if (this.Descriptor.PaymentMeans <> nil) and (this.Descriptor.PaymentMeans.TypeCode <> PaymentMeansTypeCodes.Unknown) then
//    begin
//      Writer.WriteElementString('ram:TypeCode', this.Descriptor.PaymentMeans.TypeCode.EnumToString());
//      Writer.WriteElementString('ram:Information', this.Descriptor.PaymentMeans.Information);
//
//      if this.Descriptor.PaymentMeans.FinancialCard <> nil then
//      begin
//        Writer.WriteStartElement('ram:ApplicableTradeSettlementFinancialCard', [Profile.Comfort, Profile.Extended]);
//        Writer.WriteElementString('ram:ID', Descriptor.PaymentMeans.FinancialCard.Id);
//        Writer.WriteElementString('ram:CardholderName', Descriptor.PaymentMeans.FinancialCard.CardholderName);
//        Writer.WriteEndElement(); // !ram:ApplicableTradeSettlementFinancialCard
//      end;
//    end;
//
//    Writer.WriteStartElement('ram:PayeePartyCreditorFinancialAccount');
//    Writer.WriteElementString('ram:IBANID', account.IBAN);
//    if not String.IsNullOrEmpty(account.Name) then
//      Writer.WriteElementString('ram:AccountName', account.Name);
//    if not String.IsNullOrEmpty(account.ID) then
//      Writer.WriteElementString('ram:ProprietaryID', account.ID);
//    Writer.WriteEndElement(); // !PayeePartyCreditorFinancialAccount
//
//    Writer.WriteStartElement('ram:PayeeSpecifiedCreditorFinancialInstitution');
//    Writer.WriteElementString('ram:BICID', account.BIC);
//
//    if not String.IsNullOrEmpty(account.Bankleitzahl) then
//      Writer.WriteElementString('ram:GermanBankleitzahlID', account.Bankleitzahl);
//
//    Writer.WriteEndElement(); // !PayeeSpecifiedCreditorFinancial
//
//Institution
//    Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
//  end;
//
//  for var account in this.Descriptor.DebitorBankAccounts do
//  begin
//    Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans');
//
//    if (this.Descriptor.PaymentMeans <> nil) and (this.Descriptor.PaymentMeans.TypeCode <> PaymentMeansTypeCodes.Unknown) then
//    begin
//      Writer.WriteElementString('ram:TypeCode', this.Descriptor.PaymentMeans.TypeCode.EnumToString());
//      Writer.WriteElementString('ram:Information', this.Descriptor.PaymentMeans.Information);
//    end;
//
//    Writer.WriteStartElement('ram:PayerPartyDebtorFinancialAccount');
//    Writer.WriteElementString('ram:IBANID', account.IBAN);
//    if not String.IsNullOrEmpty(account.ID) then
//      Writer.WriteElementString('ram:ProprietaryID', account.ID);
//    Writer.WriteEndElement(); // !PayerPartyDebtorFinancialAccount
//
//    if (not string.IsNullOrEmpty(account.BIC)) or
//       (not string.IsNullOrEmpty(account.Bankleitzahl)) or
//       (not string.IsNullOrEmpty(account.BankName)) then
//    begin
//      Writer.WriteStartElement('ram:PayerSpecifiedDebtorFinancialInstitution');
//
//      if not String.IsNullOrEmpty(account.BIC) then
//        Writer.WriteElementString('ram:BICID', account.BIC);
//
//      if not String.IsNullOrEmpty(account.Bankleitzahl) then
//        Writer.WriteElementString('ram:GermanBankleitzahlID', account.Bankleitzahl);
//
//      if not String.IsNullOrEmpty(account.BankName) then
//        Writer.WriteElementString('ram:Name', account.BankName);
//
//      Writer.WriteEndElement(); // !PayerSpecifiedDebtorFinancialInstitution
//    end;
//
//    Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
//  end;
//end;
//
//
////  11. ApplicableTradeTax (optional)
//_writeOptionalTaxes(Writer);
//
////  12. BillingSpecifiedPeriod (optional)
//if (Descriptor.BillingPeriodStart.HasValue or Descriptor.BillingPeriodEnd.HasValue) then
//begin
//  Writer.WriteStartElement('ram:BillingSpecifiedPeriod', [Profile.BasicWL, Profile.Basic, Profile.Comfort, Profile.Extended, Profile.XRechnung1, Profile.XRechnung]);
//  if Descriptor.BillingPeriodStart.HasValue then
//  begin
//    Writer.WriteStartElement('ram:StartDateTime');
//    _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(this.Descriptor.BillingPeriodStart.Value));
//    Writer.WriteEndElement(); // !StartDateTime
//  end;
//
//  if Descriptor.BillingPeriodEnd.HasValue then
//  begin
//    Writer.WriteStartElement('ram:EndDateTime');
//    _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(this.Descriptor.BillingPeriodEnd.Value));
//    Writer.WriteEndElement(); // !EndDateTime
//  end;
//  Writer.WriteEndElement(); // !BillingSpecifiedPeriod
//end;
//
////  13. SpecifiedTradeAllowanceCharge (optional)
//if (this.Descriptor.TradeAllowanceCharges <> nil) and (this.Descriptor.TradeAllowanceCharges.Count > 0) then
//begin
//  for var tradeAllowanceCharge in this.Descriptor.TradeAllowanceCharges do
//  begin
//    Writer.WriteStartElement('ram:SpecifiedTradeAllowanceCharge');
//    Writer.WriteStartElement('ram:ChargeIndicator');
//    Writer.WriteElementString('udt:Indicator', BoolToStr(tradeAllowanceCharge.ChargeIndicator, true));
//    Writer.WriteEndElement(); // !ram:ChargeIndicator
//
//    Writer.WriteStartElement('ram:BasisAmount');
//    Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.BasisAmount));
//    Writer.WriteEndElement();
//
//    Writer.WriteStartElement('ram:ActualAmount');
//    Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ActualAmount));
//    Writer.WriteEndElement();
//
//    _writeOptionalElementString(Writer, 'ram:Reason', tradeAllowanceCharge.Reason);
//
//    if tradeAllowanceCharge.Tax <> nil then
//    begin
//      Writer.WriteStartElement('ram:CategoryTradeTax');
//      Writer.WriteElementString('ram:TypeCode', tradeAllowanceCharge.Tax.TypeCode.EnumToString());
//      if tradeAllowanceCharge.Tax.CategoryCode.HasValue then
//        Writer.WriteElementString('ram:CategoryCode', tradeAllowanceCharge.Tax.CategoryCode?.EnumToString());
//      Writer.WriteElementString('ram:RateApplicablePercent', _formatDecimal(tradeAllowanceCharge.Tax.Percent));
//      Writer.WriteEndElement();
//    end;
//    Writer.WriteEndElement();
//  end;
//end;
//
////  14. SpecifiedLogisticsServiceCharge (optional)
//if (this.Descriptor.ServiceCharges <> nil) and (this.Descriptor.ServiceCharges.Count > 0) then
//begin
//  for var serviceCharge in this.Descriptor.ServiceCharges do
//  begin
//    Writer.WriteStartElement('ram:SpecifiedLogisticsServiceCharge');
//    if not String.IsNullOrEmpty(serviceCharge.Description) then
//      Writer.WriteElementString('ram:Description', serviceCharge.Description);
//    Writer.WriteElementString('ram:AppliedAmount', _formatDecimal(serviceCharge.Amount));
//    if serviceCharge.Tax <> nil then
//    begin
//      Writer.WriteStartElement('ram:AppliedTradeTax');
//      Writer.WriteElementString('ram:TypeCode', serviceCharge.Tax.TypeCode.EnumToString());
//      if serviceCharge.Tax.CategoryCode.HasValue then
//        Writer.WriteElementString('ram:CategoryCode
//
//', serviceCharge.Tax.CategoryCode?.EnumToString());
//      Writer.WriteElementString('ram:RateApplicablePercent', _formatDecimal(serviceCharge.Tax.Percent));
//      Writer.WriteEndElement();
//    end;
//    Writer.WriteEndElement();
//  end;
//end;
//
////  15. SpecifiedTradePaymentTerms (optional)
//if (this.Descriptor.PaymentTerms <> nil) or not string.IsNullOrEmpty(Descriptor.PaymentMeans?.SEPAMandateReference) then
//begin
//  Writer.WriteStartElement('ram:SpecifiedTradePaymentTerms');
//  _writeOptionalElementString(Writer, 'ram:Description', this.Descriptor.PaymentTerms?.Description);
//  if (this.Descriptor.PaymentTerms?.DueDate.HasValue) then
//  begin
//    Writer.WriteStartElement('ram:DueDateDateTime');
//    _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(this.Descriptor.PaymentTerms.DueDate.Value));
//    Writer.WriteEndElement(); // !ram:DueDateDateTime
//  end;
//  _writeOptionalElementString(Writer, 'ram:DirectDebitMandateID', Descriptor.PaymentMeans?.SEPAMandateReference);
//  Writer.WriteEndElement();
//end;
//
////  16. SpecifiedTradeSettlementHeaderMonetarySummation
//Writer.WriteStartElement('ram:SpecifiedTradeSettlementHeaderMonetarySummation');
//_writeOptionalAmount(Writer, 'ram:LineTotalAmount', this.Descriptor.LineTotalAmount);
//_writeOptionalAmount(Writer, 'ram:ChargeTotalAmount', this.Descriptor.ChargeTotalAmount);
//_writeOptionalAmount(Writer, 'ram:AllowanceTotalAmount', this.Descriptor.AllowanceTotalAmount);
//_writeOptionalAmount(Writer, 'ram:TaxBasisTotalAmount', this.Descriptor.TaxBasisAmount);
//_writeOptionalAmount(Writer, 'ram:TaxTotalAmount', this.Descriptor.TaxTotalAmount, 2, true);
//_writeOptionalAmount(Writer, 'ram:RoundingAmount', this.Descriptor.RoundingAmount, Profile.Comfort or Profile.Extended);  // RoundingAmount  //Rundungsbetrag
//_writeOptionalAmount(Writer, 'ram:GrandTotalAmount', this.Descriptor.GrandTotalAmount);
//
//if (this.Descriptor.TotalPrepaidAmount.HasValue) then
//  _writeOptionalAmount(Writer, 'ram:TotalPrepaidAmount', this.Descriptor.TotalPrepaidAmount.Value);
//
//_writeOptionalAmount(Writer, 'ram:DuePayableAmount', this.Descriptor.DuePayableAmount);
//Writer.WriteEndElement(); // !ram:SpecifiedTradeSettlementHeaderMonetarySummation
//
////  17. InvoiceReferencedDocument (optional)
//if (this.Descriptor.InvoiceReferencedDocument <> nil) then
//begin
//  Writer.WriteStartElement('ram:InvoiceReferencedDocument');
//  _writeOptionalElementString(Writer, 'ram:IssuerAssignedID', this.Descriptor.InvoiceReferencedDocument.ID);
//  if (this.Descriptor.InvoiceReferencedDocument.IssueDateTime.HasValue) then
//  begin
//    Writer.WriteStartElement('ram:FormattedIssueDateTime');
//    _writeElementWithAttribute(Writer, 'qdt:DateTimeString', 'format', '102', _formatDate(this.Descriptor.InvoiceReferencedDocument.IssueDateTime.Value));
//    Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//  end;
//  Writer.WriteEndElement(); // !ram:InvoiceReferencedDocument
//end;
//
//Writer.WriteEndElement(); // !ram:ApplicableHeaderTradeSettlement
//
//Writer.WriteEndElement(); // !ram:SupplyChainTradeTransaction
//Writer.WriteEndElement(); // !ram:Invoice
//Writer.WriteEndDocument();
//Writer.Flush();
//
//stream.Seek(streamPosition, SeekOrigin.Begin);
//end; // !Save()
//
//
//
//    finally
//      Writer.Free;
//    end;
//end;
//
//end;
//
//
//procedure
//
// _writeOptionalAmount(writer: ProfileAwareXmlTextWriter; tagName: string; value: Nullable<decimal>; numDecimals: Integer = 2; forceCurrency: Boolean = false; profile: Profile = Profile.Unknown);
//begin
//  if (value.HasValue) and (value.Value <> decimal.MinValue) then
//  begin
//    writer.WriteStartElement(tagName, profile);
//    if forceCurrency then
//      writer.WriteAttributeString('currencyID', this.Descriptor.Currency.EnumToString());
//    writer.WriteValue(_formatDecimal(value.Value, numDecimals));
//    writer.WriteEndElement(); // !tagName
//  end;
//end; // !_writeOptionalAmount()
//
//
//procedure _writeElementWithAttribute(writer: ProfileAwareXmlTextWriter; tagName: string; attributeName: string; attributeValue: string; nodeValue: string);
//begin
//  writer.WriteStartElement(tagName);
//  writer.WriteAttributeString(attributeName, attributeValue);
//  writer.WriteValue(nodeValue);
//  writer.WriteEndElement(); // !tagName
//end; // !_writeElementWithAttribute()
//
//
//procedure _writeOptionalTaxes(writer: ProfileAwareXmlTextWriter);
//begin
//  for var tax in this.Descriptor.Taxes do
//  begin
//    writer.WriteStartElement('ram:ApplicableTradeTax');
//
//    writer.WriteStartElement('ram:CalculatedAmount');
//    writer.WriteValue(_formatDecimal(tax.TaxAmount));
//    writer.WriteEndElement(); // !CalculatedAmount
//
//    writer.WriteElementString('ram:TypeCode', tax.TypeCode.EnumToString());
//
//    if not String.IsNullOrEmpty(tax.ExemptionReason) then
//      writer.WriteElementString('ram:ExemptionReason', tax.ExemptionReason);
//
//    writer.WriteStartElement('ram:BasisAmount');
//    writer.WriteValue(_formatDecimal(tax.BasisAmount));
//    writer.WriteEndElement(); // !BasisAmount
//
//    if (tax.AllowanceChargeBasisAmount <> 0) then
//    begin
//      writer.WriteStartElement('ram:AllowanceChargeBasisAmount');
//      writer.WriteValue(_formatDecimal(tax.AllowanceChargeBasisAmount));
//      writer.WriteEndElement(); // !AllowanceChargeBasisAmount
//    end;
//
//    if (tax.CategoryCode.HasValue) then
//      writer.WriteElementString('ram:CategoryCode', tax.CategoryCode?.EnumToString());
//
//    if (tax.ExemptionReasonCode.HasValue) then
//      writer.WriteElementString('ram:ExemptionReasonCode', tax.ExemptionReasonCode?.EnumToString());
//
//    writer.WriteElementString('ram:RateApplicablePercent', _formatDecimal(tax.Percent));
//    writer.WriteEndElement(); // !ApplicableTradeTax
//  end;
//end; // !_writeOptionalTaxes()
//
//procedure _writeNotes(writer: ProfileAwareXmlTextWriter; notes: List<Note>);
//begin
//  if (notes.Count > 0) then
//  begin
//    for var note in notes do
//    begin
//      writer.WriteStartElement('ram:IncludedNote');
//      if (note.ContentCode <> ContentCodes.Unknown) then
//        writer.WriteElementString('ram:ContentCode', note.ContentCode.EnumToString());
//      writer.WriteElementString('ram:Content', note.Content);
//      if (note.SubjectCode <> SubjectCodes.Unknown) then
//        writer.WriteElementString('ram:SubjectCode', note.SubjectCode.EnumToString());
//      writer.WriteEndElement();
//    end;
//  end;
//end; // !_writeNotes()
//
//
//procedure _writeOptionalParty(writer: ProfileAwareXmlTextWriter; PartyTag: string; Party: Party; Contact: Contact = nil; TaxRegistrations: List<TaxRegistration> = nil);
//begin
//  if (Party <> nil) then
//  begin
//    writer.WriteStartElement(PartyTag);
//
//    if (Party.ID <> nil) then
//    begin
//      if (not String.IsNullOrEmpty(Party.ID.ID)) and (Party.ID.SchemeID <> GlobalIDSchemeIdentifiers.Unknown) then
//      begin
//        writer.WriteStartElement('ram:ID');
//        writer.WriteAttributeString('schemeID', Party.ID.SchemeID.EnumToString());
//        writer.WriteValue(Party.ID.ID);
//        writer.WriteEndElement();
//      end
//      else
//      begin
//        writer.WriteElementString('ram:ID', Party.ID.ID);
//      end;
//    end;
//
//    if (Party.GlobalID <> nil) and (not String.IsNullOrEmpty(Party.GlobalID.ID)) and (Party.GlobalID.SchemeID <> GlobalIDSchemeIdentifiers.Unknown) then
//    begin
//      writer.WriteStartElement('ram:GlobalID');
//      writer.WriteAttributeString('schemeID', Party.GlobalID.SchemeID.EnumToString());
//      writer.WriteValue(Party.GlobalID.ID);
//      writer.WriteEndElement();
//    end;
//
//    if (not String.IsNullOrEmpty(Party.Name)) then
//      writer.WriteElementString('ram:Name', Party.Name);
//
//    if (Contact <> nil) then
//      _writeOptionalContact(writer, 'ram:DefinedTradeContact', Contact);
//
//    writer.WriteStartElement('ram:PostalTradeAddress');
//    writer.WriteElementString('ram:PostcodeCode', Party.Postcode);
//    writer.WriteElementString('ram:LineOne', if string.IsNullOrEmpty(Party.ContactName) then Party.Street else Party.ContactName);
//    if (not string.IsNullOrEmpty(Party.ContactName)) then
//      writer.WriteElementString('ram:LineTwo', Party.Street);
//    if (not string.IsNullOrEmpty(Party.AddressLine3)) then
//      writer.WriteElementString('ram:LineThree', Party.AddressLine3); // BT-163
//    writer.WriteElementString('ram:CityName', Party.City);
//    writer.WriteElementString('ram:CountryID', Party.Country.EnumToString());
//    if (not string.IsNullOrEmpty(Party.CountrySubdivisionName)) then
//      writer.WriteElementString('ram:CountrySubDivisionName', Party.CountrySubdivisionName); // BT-79
//    writer.WriteEndElement(); // !PostalTradeAddress
//
//    if (TaxRegistrations <> nil) then
//    begin
//      for var _reg in TaxRegistrations do
//      begin
//        if (not String.IsNullOrEmpty(_reg.No)) then
//        begin
//          writer.WriteStartElement('ram:SpecifiedTaxRegistration');
//          writer.WriteStartElement('ram:ID');
//          writer.WriteAttributeString('schemeID', _reg.SchemeID.EnumToString());
//          writer.WriteValue(_reg.No);
//          writer.WriteEndElement();
//          writer.WriteEndElement();
//        end;
//      end;
//    end;
//    writer.WriteEndElement(); // !*TradeParty
//
//
//  end;
//end; // !_writeOptionalParty()
//
//
//procedure _writeOptionalContact(writer: ProfileAwareXmlTextWriter; contactTag: string; contact: Contact);
//begin
//  if (contact <> nil) then
//  begin
//    writer.WriteStartElement(contactTag);
//
//    if (not String.IsNullOrEmpty(contact.Name)) then
//      writer.WriteElementString('ram:PersonName', contact.Name);
//
//    if (not String.IsNullOrEmpty(contact.OrgUnit)) then
//      writer.WriteElementString('ram:DepartmentName', contact.OrgUnit);
//
//    if (not String.IsNullOrEmpty(contact.PhoneNo)) then
//    begin
//      writer.WriteStartElement('ram:TelephoneUniversalCommunication');
//      writer.WriteElementString('ram:CompleteNumber', contact.PhoneNo);
//      writer.WriteEndElement();
//    end;
//
//    if (not String.IsNullOrEmpty(contact.FaxNo)) then
//    begin
//      writer.WriteStartElement('ram:FaxUniversalCommunication');
//      writer.WriteElementString('ram:CompleteNumber', contact.FaxNo);
//      writer.WriteEndElement();
//    end;
//
//    if (not String.IsNullOrEmpty(contact.EmailAddress)) then
//    begin
//      writer.WriteStartElement('ram:EmailURIUniversalCommunication');
//      writer.WriteElementString('ram:URIID', contact.EmailAddress);
//      writer.WriteEndElement();
//    end;
//
//    writer.WriteEndElement();
//  end;
//end; // !_writeOptionalContact()
//
//
//function _translateInvoiceType(type: InvoiceType): string;
//begin
//  case type of
//    InvoiceType.SelfBilledInvoice,
//    InvoiceType.Invoice: Result := 'RECHNUNG';
//    InvoiceType.SelfBilledCreditNote,
//    InvoiceType.CreditNote: Result := 'GUTSCHRIFT';
//    InvoiceType.DebitNote: Result := 'BELASTUNGSANZEIGE';
//    InvoiceType.DebitnoteRelatedToFinancialAdjustments: Result := 'WERTBELASTUNG';
//    InvoiceType.PartialInvoice: Result := 'TEILRECHNUNG';
//    InvoiceType.PrepaymentInvoice: Result := 'VORAUSZAHLUNGSRECHNUNG';
//    InvoiceType.InvoiceInformation: Result := 'KEINERECHNUNG';
//    InvoiceType.Correction,
//    InvoiceType.CorrectionOld: Result := 'KORREKTURRECHNUNG';
//    InvoiceType.Unknown: Result := '';
//    else Result := '';
//  end;
//end; // !_translateInvoiceType()
//
//
//function _encodeInvoiceType(type: InvoiceType): Integer;
//begin
//  if Integer(type) > 1000 then
//    type -= 1000;
//
//  if (type = InvoiceType.CorrectionOld) then
//    Result := Integer(InvoiceType.Correction)
//  else
//    Result := Integer(type);
//end; // !_translateInvoiceType()
//
//
//function Validate(descriptor: InvoiceDescriptor; throwExceptions: Boolean = true): Boolean;
//begin
//  if (descriptor.Profile = Profile.BasicWL) then
//  begin
//    if (throwExceptions) then
//      raise UnsupportedException.Create('Invalid profile used for ZUGFeRD 2.0 invoice.');
//    Result := False;
//    Exit;
//  end;
//
//  if (descriptor.Profile <> Profile.Extended) then // check tax types, only extended profile allows tax types other than vat
//  begin
//    if (not descriptor.TradeLineItems.All(lambda l: (l.TaxType.Equals(TaxTypes.VAT)) or (l.TaxType.Equals(TaxTypes.Unknown)))) then
//    begin
//      if (throwExceptions) then
//        raise UnsupportedException.Create('Tax types other than VAT only possible with extended profile.');
//      Result := False;
//      Exit;
//    end;
//  end;
//
//  Result := True;
//end; // !Validate()
//
















//
//    internal class InvoiceDescriptor20Writer : IInvoiceDescriptorWriter
//    {
//        private ProfileAwareXmlTextWriter Writer;
//        private InvoiceDescriptor Descriptor;
//
//
//        public override void Save(InvoiceDescriptor descriptor, Stream stream)
//        {
//            if (!stream.CanWrite || !stream.CanSeek)
//            {
//                throw new IllegalStreamException("Cannot write to stream");
//            }
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
//        } // !Save()
//
//
//        private void _writeOptionalAmount(ProfileAwareXmlTextWriter writer, string tagName, decimal? value, int numDecimals = 2, bool forceCurrency = false, Profile profile = Profile.Unknown)
//        {
//            if (value.HasValue && (value.Value != decimal.MinValue))
//            {
//                writer.WriteStartElement(tagName, profile);
//                if (forceCurrency)
//                {
//                    writer.WriteAttributeString("currencyID", this.Descriptor.Currency.EnumToString());
//                }
//                writer.WriteValue(_formatDecimal(value.Value, numDecimals));
//                writer.WriteEndElement(); // !tagName
//            }
//        } // !_writeOptionalAmount()
//
//
//        private void _writeElementWithAttribute(ProfileAwareXmlTextWriter writer, string tagName, string attributeName, string attributeValue, string nodeValue)
//        {
//            writer.WriteStartElement(tagName);
//            writer.WriteAttributeString(attributeName, attributeValue);
//            writer.WriteValue(nodeValue);
//            writer.WriteEndElement(); // !tagName
//        } // !_writeElementWithAttribute()
//
//
//        private void _writeOptionalTaxes(ProfileAwareXmlTextWriter writer)
//        {
//            foreach (Tax tax in this.Descriptor.Taxes)
//            {
//                writer.WriteStartElement("ram:ApplicableTradeTax");
//
//                writer.WriteStartElement("ram:CalculatedAmount");
//                writer.WriteValue(_formatDecimal(tax.TaxAmount));
//                writer.WriteEndElement(); // !CalculatedAmount
//
//                writer.WriteElementString("ram:TypeCode", tax.TypeCode.EnumToString());
//
//                if (!String.IsNullOrEmpty(tax.ExemptionReason))
//                {
//                    writer.WriteElementString("ram:ExemptionReason", tax.ExemptionReason);
//                }
//
//                writer.WriteStartElement("ram:BasisAmount");
//                writer.WriteValue(_formatDecimal(tax.BasisAmount));
//                writer.WriteEndElement(); // !BasisAmount
//
//                if (tax.AllowanceChargeBasisAmount != 0)
//                {
//                    writer.WriteStartElement("ram:AllowanceChargeBasisAmount");
//                    writer.WriteValue(_formatDecimal(tax.AllowanceChargeBasisAmount));
//                    writer.WriteEndElement(); // !AllowanceChargeBasisAmount
//                }
//
//                if (tax.CategoryCode.HasValue)
//                {
//                    writer.WriteElementString("ram:CategoryCode", tax.CategoryCode?.EnumToString());
//                }
//
//                if (tax.ExemptionReasonCode.HasValue)
//                {
//                    writer.WriteElementString("ram:ExemptionReasonCode", tax.ExemptionReasonCode?.EnumToString());
//                }
//
//                writer.WriteElementString("ram:RateApplicablePercent", _formatDecimal(tax.Percent));
//                writer.WriteEndElement(); // !ApplicableTradeTax
//            }
//        } // !_writeOptionalTaxes()
//
//
//        private void _writeNotes(ProfileAwareXmlTextWriter writer, List<Note> notes)
//        {
//            if (notes.Count > 0)
//            {
//                foreach (Note note in notes)
//                {
//                    writer.WriteStartElement("ram:IncludedNote");
//                    if (note.ContentCode != ContentCodes.Unknown)
//                    {
//                        writer.WriteElementString("ram:ContentCode", note.ContentCode.EnumToString());
//                    }
//                    writer.WriteElementString("ram:Content", note.Content);
//                    if (note.SubjectCode != SubjectCodes.Unknown)
//                    {
//                        writer.WriteElementString("ram:SubjectCode", note.SubjectCode.EnumToString());
//                    }
//                    writer.WriteEndElement();
//                }
//            }
//        } // !_writeNotes()
//
//
//        private void _writeOptionalParty(ProfileAwareXmlTextWriter writer, string PartyTag, Party Party, Contact Contact = null, List<TaxRegistration> TaxRegistrations = null)
//        {
//            if (Party != null)
//            {
//                writer.WriteStartElement(PartyTag);
//
//                if (Party.ID != null) {
//                    if (!String.IsNullOrEmpty(Party.ID.ID) && (Party.ID.SchemeID != GlobalIDSchemeIdentifiers.Unknown)) {
//                        writer.WriteStartElement("ram:ID");
//                        writer.WriteAttributeString("schemeID", Party.ID.SchemeID.EnumToString());
//                        writer.WriteValue(Party.ID.ID);
//                        writer.WriteEndElement();
//                    }
//                    else {
//                        writer.WriteElementString("ram:ID", Party.ID.ID);
//                    }
//                }
//
//                if ((Party.GlobalID != null) && !String.IsNullOrEmpty(Party.GlobalID.ID) && (Party.GlobalID.SchemeID != GlobalIDSchemeIdentifiers.Unknown))
//                {
//                    writer.WriteStartElement("ram:GlobalID");
//                    writer.WriteAttributeString("schemeID", Party.GlobalID.SchemeID.EnumToString());
//                    writer.WriteValue(Party.GlobalID.ID);
//                    writer.WriteEndElement();
//                }
//
//                if (!String.IsNullOrEmpty(Party.Name))
//                {
//                    writer.WriteElementString("ram:Name", Party.Name);
//                }
//
//                if (Contact != null)
//                {
//                    _writeOptionalContact(writer, "ram:DefinedTradeContact", Contact);
//                }
//
//                writer.WriteStartElement("ram:PostalTradeAddress");
//                writer.WriteElementString("ram:PostcodeCode", Party.Postcode);
//                writer.WriteElementString("ram:LineOne", string.IsNullOrEmpty(Party.ContactName) ? Party.Street : Party.ContactName);
//                if (!string.IsNullOrEmpty(Party.ContactName))
//                    writer.WriteElementString("ram:LineTwo", Party.Street);
//                if (!string.IsNullOrEmpty(Party.AddressLine3))
//                    writer.WriteElementString("ram:LineThree", Party.AddressLine3); // BT-163
//                writer.WriteElementString("ram:CityName", Party.City);
//                writer.WriteElementString("ram:CountryID", Party.Country.EnumToString());
//                if (!string.IsNullOrEmpty(Party.CountrySubdivisionName))
//                    writer.WriteElementString("ram:CountrySubDivisionName", Party.CountrySubdivisionName); // BT-79
//                writer.WriteEndElement(); // !PostalTradeAddress
//
//                if (TaxRegistrations != null)
//                {
//                    foreach (TaxRegistration _reg in TaxRegistrations)
//                    {
//                        if (!String.IsNullOrEmpty(_reg.No))
//                        {
//                            writer.WriteStartElement("ram:SpecifiedTaxRegistration");
//                            writer.WriteStartElement("ram:ID");
//                            writer.WriteAttributeString("schemeID", _reg.SchemeID.EnumToString());
//                            writer.WriteValue(_reg.No);
//                            writer.WriteEndElement();
//                            writer.WriteEndElement();
//                        }
//                    }
//                }
//                writer.WriteEndElement(); // !*TradeParty
//            }
//        } // !_writeOptionalParty()
//
//
//        private void _writeOptionalContact(ProfileAwareXmlTextWriter writer, string contactTag, Contact contact)
//        {
//            if (contact != null)
//            {
//                writer.WriteStartElement(contactTag);
//
//                if (!String.IsNullOrEmpty(contact.Name))
//                {
//                    writer.WriteElementString("ram:PersonName", contact.Name);
//                }
//
//                if (!String.IsNullOrEmpty(contact.OrgUnit))
//                {
//                    writer.WriteElementString("ram:DepartmentName", contact.OrgUnit);
//                }
//
//                if (!String.IsNullOrEmpty(contact.PhoneNo))
//                {
//                    writer.WriteStartElement("ram:TelephoneUniversalCommunication");
//                    writer.WriteElementString("ram:CompleteNumber", contact.PhoneNo);
//                    writer.WriteEndElement();
//                }
//
//                if (!String.IsNullOrEmpty(contact.FaxNo))
//                {
//                    writer.WriteStartElement("ram:FaxUniversalCommunication");
//                    writer.WriteElementString("ram:CompleteNumber", contact.FaxNo);
//                    writer.WriteEndElement();
//                }
//
//                if (!String.IsNullOrEmpty(contact.EmailAddress))
//                {
//                    writer.WriteStartElement("ram:EmailURIUniversalCommunication");
//                    writer.WriteElementString("ram:URIID", contact.EmailAddress);
//                    writer.WriteEndElement();
//                }
//
//                writer.WriteEndElement();
//            }
//        } // !_writeOptionalContact()
//
//
//        private string _translateInvoiceType(InvoiceType type)
//        {
//            switch (type)
//            {
//                case InvoiceType.SelfBilledInvoice:
//                case InvoiceType.Invoice: return "RECHNUNG";
//                case InvoiceType.SelfBilledCreditNote:
//                case InvoiceType.CreditNote: return "GUTSCHRIFT";
//                case InvoiceType.DebitNote: return "BELASTUNGSANZEIGE";
//                case InvoiceType.DebitnoteRelatedToFinancialAdjustments: return "WERTBELASTUNG";
//                case InvoiceType.PartialInvoice: return "TEILRECHNUNG";
//                case InvoiceType.PrepaymentInvoice: return "VORAUSZAHLUNGSRECHNUNG";
//                case InvoiceType.InvoiceInformation: return "KEINERECHNUNG";
//                case InvoiceType.Correction:
//                case InvoiceType.CorrectionOld: return "KORREKTURRECHNUNG";
//                case InvoiceType.Unknown: return "";
//                default: return "";
//            }
//        } // !_translateInvoiceType()
//
//
//        private int _encodeInvoiceType(InvoiceType type)
//        {
//            if ((int)type > 1000)
//            {
//                type -= 1000;
//            }
//
//            if (type == InvoiceType.CorrectionOld)
//            {
//                return (int)InvoiceType.Correction;
//            }
//
//            return (int)type;
//        } // !_translateInvoiceType()
//
//
//        internal override bool Validate(InvoiceDescriptor descriptor, bool throwExceptions = true)
//        {
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
//        } // !Validate()
//    }
end;



function TZUGFeRDInvoiceDescriptor1Writer.Validate(
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
