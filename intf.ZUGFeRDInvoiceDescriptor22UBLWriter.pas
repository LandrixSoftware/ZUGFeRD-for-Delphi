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

unit intf.ZUGFeRDInvoiceDescriptor22UBLWriter;

interface

uses
  System.SysUtils,System.Classes,System.StrUtils,Generics.Collections
  ,intf.ZUGFeRDInvoiceDescriptor
  ,intf.ZUGFeRDInvoiceTypes
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
  ,intf.ZUGFeRDAllowanceOrChargeIdentificationCodes
  ,intf.ZUGFeRDFormats
  ;

type
  TZUGFeRDInvoiceDescriptor22UBLWriter = class(TZUGFeRDInvoiceDescriptorWriter)
  private
    Writer: TZUGFeRDProfileAwareXmlTextWriter;
    Descriptor: TZUGFeRDInvoiceDescriptor;
{$HINTS OFF} // suppress hint for unused private function for now
    procedure _writeOptionalAmount(_writer : TZUGFeRDProfileAwareXmlTextWriter; tagName : string; value : ZUGFeRDNullable<Currency>; numDecimals : Integer = 2; forceCurrency : Boolean = false; profile : TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
    procedure _writeNotes(_writer : TZUGFeRDProfileAwareXmlTextWriter;notes : TObjectList<TZUGFeRDNote>);
//    procedure _writeOptionalLegalOrganization(_writer : TZUGFeRDProfileAwareXmlTextWriter; legalOrganizationTag : String;legalOrganization : TZUGFeRDLegalOrganization; partyType : TZUGFeRDPartyTypes = TZUGFeRDPartyTypes.Unknown);
    procedure _writeOptionalParty(_writer: TZUGFeRDProfileAwareXmlTextWriter; partyType : TZUGFeRDPartyTypes; party : TZUGFeRDParty; contact : TZUGFeRDContact = nil; electronicAddress : TZUGFeRDElectronicAddress = nil; taxRegistrations : TObjectList<TZUGFeRDTaxRegistration> = nil);
    procedure _writeApplicableProductCharacteristics(_writer: TZUGFeRDProfileAwareXmlTextWriter; productCharacteristics : TObjectList<TZUGFeRDApplicableProductCharacteristic>);
//    procedure _writeOptionalContact(_writer: TZUGFeRDProfileAwareXmlTextWriter;contactTag: String; contact: TZUGFeRDContact;profile: TZUGFeRDProfiles);
//    procedure _writeOptionalTaxes(_writer: TZUGFeRDProfileAwareXmlTextWriter);
//    procedure _writeElementWithAttribute(_writer: TZUGFeRDProfileAwareXmlTextWriter; tagName, attributeName,attributeValue, nodeValue: String; profile: TZUGFeRDProfiles = [TZUGFeRDProfile.Unknown]);
//    function _translateTaxCategoryCode(taxCategoryCode : TZUGFeRDTaxCategoryCodes) : String;
//    function _translateInvoiceType(type_ : TZUGFeRDInvoiceType) : String;
    function _encodeInvoiceType(type_ : TZUGFeRDInvoiceType) : Integer;
{$HINTS ON}
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
    /// <summary>
    /// Saves the given invoice to the given stream.
    /// Make sure that the stream is open and writeable. Otherwise, an IllegalStreamException will be thron.
    /// </summary>
    /// <param name="descriptor"></param>
    /// <param name="stream"></param>
    /// <param name="format">Format of the target file</param>
    procedure Save(_descriptor: TZUGFeRDInvoiceDescriptor; _stream: TStream; _format : TZUGFeRDFormats = TZUGFeRDFormats.UBL); override;
  end;

implementation

{ TZUGFeRDInvoiceDescriptor22UBLWriter }

procedure TZUGFeRDInvoiceDescriptor22UBLWriter.Save(
  _descriptor: TZUGFeRDInvoiceDescriptor; _stream: TStream;
  _format : TZUGFeRDFormats = TZUGFeRDFormats.UBL);
var
  streamPosition : Int64;
begin
  if (_stream = nil) then
    raise TZUGFeRDIllegalStreamException.Create('Cannot write to stream');

  // write data
  streamPosition := _stream.Position;

  Descriptor := _descriptor;
  Writer := TZUGFeRDProfileAwareXmlTextWriter.Create(_stream,TEncoding.UTF8,Descriptor.Profile);
  Writer.Formatting := TZUGFeRDXmlFomatting.xmlFormatting_Indented;
  Writer.WriteStartDocument;

//            if (this.Descriptor.Type != InvoiceType.Invoice && this.Descriptor.Type != InvoiceType.CreditNote)
//                throw new NotImplementedException("Not implemented yet.");
//
//            #region Kopfbereich
//            // UBL has different namespace for different types
//            if (this.Descriptor.Type == InvoiceType.Invoice)
//            {
//                Writer.WriteStartElement("Invoice");
//                Writer.WriteAttributeString("xmlns", null, null, "urn:oasis:names:specification:ubl:schema:xsd:Invoice-2");
//            }
//            else if (this.Descriptor.Type == InvoiceType.CreditNote)
//            {
//                Writer.WriteStartElement("CreditNote");
//                Writer.WriteAttributeString("xmlns", null, null, "urn:oasis:names:specification:ubl:schema:xsd:CreditNote-2");
//            }
//            Writer.WriteAttributeString("xmlns", "cac", null, "urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2");
//            Writer.WriteAttributeString("xmlns", "cbc", null, "urn:oasis:names:specification:ubl:schema:xsd:CommonBasicComponents-2");
//            Writer.WriteAttributeString("xmlns", "ext", null, "urn:oasis:names:specification:ubl:schema:xsd:CommonExtensionComponents-2");
//            Writer.WriteAttributeString("xmlns", "xs", null, "http://www.w3.org/2001/XMLSchema");
//            #endregion
//
//
//            Writer.WriteElementString("cbc:CustomizationID", "urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_3.0");
//            Writer.WriteElementString("cbc:ProfileID", "urn:fdc:peppol.eu:2017:poacc:billing:01:1.0");
//
//            Writer.WriteElementString("cbc:ID", this.Descriptor.InvoiceNo); //Rechnungsnummer
//            Writer.WriteElementString("cbc:IssueDate", _formatDate(this.Descriptor.InvoiceDate.Value, false, true));
//
//            Writer.WriteElementString("cbc:InvoiceTypeCode", String.Format("{0}", _encodeInvoiceType(this.Descriptor.Type))); //Code für den Rechnungstyp
//
//
//            _writeNotes(Writer, this.Descriptor.Notes);
//
//            Writer.WriteElementString("cbc:DocumentCurrencyCode", this.Descriptor.Currency.EnumToString());
//
//            Writer.WriteOptionalElementString("cbc:BuyerReference", this.Descriptor.ReferenceOrderNo);
//
//            // OrderReference
//            Writer.WriteStartElement("cac:OrderReference");
//            Writer.WriteElementString("cbc:ID", this.Descriptor.OrderNo);
//            Writer.WriteEndElement(); // !OrderReference
//
//
//            // BillingReference
//            if (this.Descriptor.GetInvoiceReferencedDocuments().Count > 0)
//            {
//                Writer.WriteStartElement("cac:BillingReference");
//                foreach (InvoiceReferencedDocument invoiceReferencedDocument in this.Descriptor.GetInvoiceReferencedDocuments())
//                {
//                    Writer.WriteStartElement("cac:InvoiceDocumentReference", Profile.Extended | Profile.XRechnung1 | Profile.XRechnung);
//                    Writer.WriteOptionalElementString("cbc:ID", invoiceReferencedDocument.ID);
//                    if (invoiceReferencedDocument.IssueDateTime.HasValue)
//                    {
//                        Writer.WriteElementString("cbc:IssueDate", _formatDate(invoiceReferencedDocument.IssueDateTime.Value, false, true));
//                    }
//                    Writer.WriteEndElement(); // !ram:InvoiceDocumentReference
//                    break; // only one reference allowed in UBL
//                }
//                Writer.WriteEndElement(); // !cac:BillingReference
//            }
//
//            // ContractDocumentReference
//            if (this.Descriptor.ContractReferencedDocument != null)
//            {
//                Writer.WriteStartElement("cac:ContractDocumentReference");
//                Writer.WriteOptionalElementString("cbc:ID", this.Descriptor.ContractReferencedDocument.ID);
//                Writer.WriteEndElement(); // !ContractDocumentReference
//            }
//
//            // ProjectReference
//            if (this.Descriptor.SpecifiedProcuringProject != null)
//            {
//                Writer.WriteStartElement("cac:ProjectReference");
//                Writer.WriteOptionalElementString("cbc:ID", this.Descriptor.SpecifiedProcuringProject.ID);
//                Writer.WriteEndElement(); // !ProjectReference
//            }
//
//
//            #region SellerTradeParty
//            //AccountingSupplierParty
//            _writeOptionalParty(Writer, PartyTypes.SellerTradeParty, this.Descriptor.Seller, this.Descriptor.SellerContact, this.Descriptor.SellerElectronicAddress, this.Descriptor.SellerTaxRegistration);
//            #endregion
//
//            #region BuyerTradeParty
//            //AccountingCustomerParty
//            _writeOptionalParty(Writer, PartyTypes.BuyerTradeParty, this.Descriptor.Buyer, this.Descriptor.BuyerContact, this.Descriptor.BuyerElectronicAddress, this.Descriptor.BuyerTaxRegistration);
//            #endregion
//
//            // PaymentMeans
//
//            if (this.Descriptor.PaymentMeans != null)
//            {
//
//                if ((this.Descriptor.PaymentMeans != null) && (this.Descriptor.PaymentMeans.TypeCode != PaymentMeansTypeCodes.Unknown))
//                {
//                    Writer.WriteStartElement("cac:PaymentMeans", Profile.BasicWL | Profile.Basic | Profile.Comfort | Profile.Extended | Profile.XRechnung1 | Profile.XRechnung);
//                    Writer.WriteElementString("cbc:PaymentMeansCode", this.Descriptor.PaymentMeans.TypeCode.EnumToString());
//                    Writer.WriteOptionalElementString("cbc:PaymentID", this.Descriptor.PaymentReference);
//
//                    if (this.Descriptor.PaymentMeans.FinancialCard != null)
//                    {
//                        Writer.WriteStartElement("cac:CardAccount", Profile.BasicWL | Profile.Basic | Profile.Comfort | Profile.Extended | Profile.XRechnung1 | Profile.XRechnung);
//                        Writer.WriteElementString("cbc:PrimaryAccountNumberID", this.Descriptor.PaymentMeans.FinancialCard.Id);
//                        Writer.WriteElementString("cbc:HolderName", this.Descriptor.PaymentMeans.FinancialCard.CardholderName);
//                        Writer.WriteEndElement(); //!CardAccount
//                    }
//
//
//                    if (this.Descriptor.CreditorBankAccounts.Count > 0)
//                    {
//                        foreach (BankAccount account in this.Descriptor.CreditorBankAccounts)
//                        {
//                            // PayeeFinancialAccount
//                            Writer.WriteStartElement("cac:PayeeFinancialAccount");
//
//                            Writer.WriteElementString("cbc:ID", account.IBAN);
//                            Writer.WriteElementString("cbc:Name", account.Name);
//
//                            Writer.WriteStartElement("cac:FinancialInstitutionBranch");
//                            Writer.WriteElementString("cbc:ID", account.BIC);
//
//                            Writer.WriteStartElement("cac:FinancialInstitution");
//                            Writer.WriteElementString("cbc:Name", account.BankName);
//
//                            Writer.WriteEndElement(); // !FinancialInstitution
//                            Writer.WriteEndElement(); // !FinancialInstitutionBranch
//
//                            Writer.WriteEndElement(); // !PayeeFinancialAccount
//                        }
//                    }
//
//                    if (this.Descriptor.DebitorBankAccounts.Count > 0)
//                    {
//                        // PaymentMandate --> PayerFinancialAccount
//                        foreach (BankAccount account in this.Descriptor.DebitorBankAccounts)
//                        {
//                            Writer.WriteStartElement("cac:PaymentMandate");
//
//                            Writer.WriteStartElement("cac:PayerFinancialAccount");
//
//                            Writer.WriteElementString("cbc:ID", account.IBAN);
//                            Writer.WriteElementString("cbc:Name", account.Name);
//
//                            Writer.WriteStartElement("cac:FinancialInstitutionBranch");
//                            Writer.WriteElementString("cbc:ID", account.BIC);
//
//                            Writer.WriteStartElement("cac:FinancialInstitution");
//                            Writer.WriteElementString("cbc:Name", account.BankName);
//
//                            Writer.WriteEndElement(); // !FinancialInstitution
//                            Writer.WriteEndElement(); // !FinancialInstitutionBranch
//
//                            Writer.WriteEndElement(); // !PayerFinancialAccount
//                            Writer.WriteEndElement(); // !PaymentMandate
//                        }
//                    }
//
//                    Writer.WriteEndElement(); //!PaymentMeans
//                }
//            }
//
//            // PaymentTerms (optional)
//            if (this.Descriptor.PaymentTerms != null)
//            {
//                Writer.WriteStartElement("cac:PaymentTerms");
//                Writer.WriteOptionalElementString("cbc:Note", this.Descriptor.PaymentTerms?.Description);
//                Writer.WriteEndElement();
//            }
//
//
//            // Tax Total
//            Writer.WriteStartElement("cac:TaxTotal");
//            _writeOptionalAmount(Writer, "cbc:TaxAmount", this.Descriptor.TaxTotalAmount, forceCurrency: true);
//
//            foreach (Tax tax in this.Descriptor.Taxes)
//            {
//                Writer.WriteStartElement("cac:TaxSubtotal");
//                _writeOptionalAmount(Writer, "cbc:TaxableAmount", tax.BasisAmount, forceCurrency: true);
//                _writeOptionalAmount(Writer, "cbc:TaxAmount", tax.TaxAmount, forceCurrency: true);
//
//                Writer.WriteStartElement("cac:TaxCategory");
//                Writer.WriteElementString("cbc:ID", tax.CategoryCode.ToString());
//                Writer.WriteElementString("cbc:Percent", _formatDecimal(tax.Percent));
//
//                Writer.WriteStartElement("cac:TaxScheme");
//                Writer.WriteElementString("cbc:ID", tax.TypeCode.EnumToString());
//                Writer.WriteEndElement();// !TaxScheme
//
//                Writer.WriteEndElement();// !TaxCategory
//                Writer.WriteEndElement();// !TaxSubtotal
//            }
//
//            Writer.WriteEndElement();// !TaxTotal
//
//            Writer.WriteStartElement("cac:LegalMonetaryTotal");
//            _writeOptionalAmount(Writer, "cbc:LineExtensionAmount", this.Descriptor.LineTotalAmount, forceCurrency: true);
//            _writeOptionalAmount(Writer, "cbc:TaxExclusiveAmount", this.Descriptor.TaxBasisAmount, forceCurrency: true);
//            _writeOptionalAmount(Writer, "cbc:TaxInclusiveAmount", this.Descriptor.GrandTotalAmount, forceCurrency: true);
//            _writeOptionalAmount(Writer, "cbc:ChargeTotalAmount", this.Descriptor.ChargeTotalAmount, forceCurrency: true);
//            _writeOptionalAmount(Writer, "cbc:AllowanceTotalAmount", this.Descriptor.AllowanceTotalAmount, forceCurrency: true);
//            //_writeOptionalAmount(Writer, "cbc:TaxAmount", this.Descriptor.TaxTotalAmount, forceCurrency: true);
//            _writeOptionalAmount(Writer, "cbc:PrepaidAmount", this.Descriptor.TotalPrepaidAmount, forceCurrency: true);
//            _writeOptionalAmount(Writer, "cbc:PayableAmount", this.Descriptor.DuePayableAmount, forceCurrency: true);
//            //_writeOptionalAmount(Writer, "cbc:PayableAlternativeAmount", this.Descriptor.RoundingAmount, forceCurrency: true);
//            Writer.WriteEndElement(); //!LegalMonetaryTotal
//
//
//
//            foreach (TradeLineItem tradeLineItem in this.Descriptor.TradeLineItems)
//            {
//                Writer.WriteStartElement("cac:InvoiceLine");
//                Writer.WriteElementString("cbc:ID", tradeLineItem.AssociatedDocument.LineID);
//
//                //Writer.WriteElementString("cbc:InvoicedQuantity", tradeLineItem.BilledQuantity.ToString());
//                Writer.WriteStartElement("cbc:InvoicedQuantity");
//                Writer.WriteAttributeString("unitCode", tradeLineItem.UnitCode.ToString());
//                Writer.WriteValue(_formatDecimal(tradeLineItem.BilledQuantity));
//                Writer.WriteEndElement();
//
//
//                //Writer.WriteElementString("cbc:LineExtensionAmount", tradeLineItem.LineTotalAmount.ToString());
//                Writer.WriteStartElement("cbc:LineExtensionAmount");
//                Writer.WriteAttributeString("currencyID", this.Descriptor.Currency.EnumToString());
//                Writer.WriteValue(tradeLineItem.LineTotalAmount.ToString());
//                Writer.WriteEndElement();
//
//
//                Writer.WriteStartElement("cac:Item");
//
//                Writer.WriteElementString("cbc:Description", tradeLineItem.Description);
//                Writer.WriteElementString("cbc:Name", tradeLineItem.Name);
//
//                Writer.WriteStartElement("cac:SellersItemIdentification");
//                Writer.WriteElementString("cbc:ID", tradeLineItem.SellerAssignedID);
//                Writer.WriteEndElement(); //!SellersItemIdentification
//
//                Writer.WriteStartElement("cac:BuyersItemIdentification");
//                Writer.WriteElementString("cbc:ID", tradeLineItem.BuyerAssignedID);
//                Writer.WriteEndElement(); //!BuyersItemIdentification
//
//
//                _writeApplicableProductCharacteristics(Writer, tradeLineItem.ApplicableProductCharacteristics);
//
//                Writer.WriteEndElement(); //!Item
//
//
//                Writer.WriteStartElement("cac:Price");
//
//                //Writer.WriteElementString("cbc:BaseQuantity", tradeLineItem.UnitQuantity.ToString());
//                //Writer.WriteStartElement("cbc:BaseQuantity");
//                //Writer.WriteAttributeString("unitCode", this.Descriptor.Currency.EnumToString());
//                //Writer.WriteValue(tradeLineItem.UnitQuantity.ToString());
//                //Writer.WriteEndElement();
//
//                //Writer.WriteElementString("cbc:PriceAmount", tradeLineItem.NetUnitPrice.ToString());
//                Writer.WriteStartElement("cbc:PriceAmount");
//                Writer.WriteAttributeString("currencyID", this.Descriptor.Currency.EnumToString());
//                Writer.WriteValue(_formatDecimal(tradeLineItem.NetUnitPrice.Value));
//                Writer.WriteEndElement();
//
//                Writer.WriteEndElement(); //!Price
//
//                // TODO Add Tax Information for the tradeline item
//
//                Writer.WriteEndElement(); //!InvoiceLine
//            }



//  //#region Kopfbereich
//  Writer.WriteStartElement('rsm:CrossIndustryInvoice');
//  Writer.WriteAttributeString('xmlns', 'a', '', 'urn:un:unece:uncefact:data:standard:QualifiedDataType:100');
//  Writer.WriteAttributeString('xmlns', 'rsm', '', 'urn:un:unece:uncefact:data:standard:CrossIndustryInvoice:100');
//  Writer.WriteAttributeString('xmlns', 'qdt', '', 'urn:un:unece:uncefact:data:standard:QualifiedDataType:100');
//  Writer.WriteAttributeString('xmlns', 'ram', '', 'urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:100');
//  Writer.WriteAttributeString('xmlns', 'xs', '', 'http://www.w3.org/2001/XMLSchema');
//  Writer.WriteAttributeString('xmlns', 'udt', '', 'urn:un:unece:uncefact:data:standard:UnqualifiedDataType:100');
//  //#endregion
//
//  //#region ExchangedDocumentContext
//  //Prozesssteuerung
//  Writer.WriteStartElement('rsm:ExchangedDocumentContext');
//  if (Descriptor.IsTest) then
//  begin
//    Writer.WriteStartElement('ram:TestIndicator');
//    Writer.WriteElementString('udt:Indicator', ifthen(Descriptor.IsTest,'true','false'));
//    Writer.WriteEndElement(); // !ram:TestIndicator
//  end;
//
//  if (Descriptor.BusinessProcess <> '') then
//  begin
//    Writer.WriteStartElement('ram:BusinessProcessSpecifiedDocumentContextParameter', [TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);
//    Writer.WriteElementString('ram:ID', Descriptor.BusinessProcess, [TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);
//    Writer.WriteEndElement(); // !ram:BusinessProcessSpecifiedDocumentContextParameter
//  end;
//
//  Writer.WriteStartElement('ram:GuidelineSpecifiedDocumentContextParameter');
//  //Gruppierung der Anwendungsempfehlungsinformationen
//  Writer.WriteElementString('ram:ID', TZUGFeRDProfileExtensions.EnumToString(Descriptor.Profile,TZUGFeRDVersion.Version22));
//  Writer.WriteEndElement(); // !ram:GuidelineSpecifiedDocumentContextParameter
//  Writer.WriteEndElement(); // !rsm:ExchangedDocumentContext
//  //#endregion
//
//  //Gruppierung der Eigenschaften, die das gesamte Dokument betreffen.
//  Writer.WriteStartElement('rsm:ExchangedDocument');
//  Writer.WriteElementString('ram:ID', Descriptor.InvoiceNo); //Rechnungsnummer
//  Writer.WriteElementString('ram:Name', ifthen(Descriptor.Name<>'',Descriptor.Name,_translateInvoiceType(Descriptor.Type_)), [TZUGFeRDProfile.Extended]); //Dokumentenart (Freitext)
//  Writer.WriteElementString('ram:TypeCode', Format('%d',[_encodeInvoiceType(Descriptor.Type_)])); //Code für den Rechnungstyp
//                                                                                                             //ToDo: LanguageID      //Sprachkennzeichen
//  if (Descriptor.InvoiceDate > 100) then
//  begin
//      Writer.WriteStartElement('ram:IssueDateTime');
//      Writer.WriteStartElement('udt:DateTimeString');
//      Writer.WriteAttributeString('format', '102');
//      Writer.WriteValue(_formatDate(Descriptor.InvoiceDate));
//      Writer.WriteEndElement(); // !udt:DateTimeString
//      Writer.WriteEndElement(); // !IssueDateTime
//  end;
//  _writeNotes(Writer, Descriptor.Notes);
//  Writer.WriteEndElement(); // !rsm:ExchangedDocument
//
//  //#region SpecifiedSupplyChainTradeTransaction
//  //Gruppierung der Informationen zum Geschäftsvorfall
//  Writer.WriteStartElement('rsm:SupplyChainTradeTransaction');
//
//  //#region  IncludedSupplyChainTradeLineItem
//  for var tradeLineItem : TZUGFeRDTradeLineItem in Descriptor.TradeLineItems do
//  begin
//    Writer.WriteStartElement('ram:IncludedSupplyChainTradeLineItem');
//
//    //#region AssociatedDocumentLineDocument
//    //Gruppierung von allgemeinen Positionsangaben
//    if (tradeLineItem.AssociatedDocument <> nil) then
//    begin
//      Writer.WriteStartElement('ram:AssociatedDocumentLineDocument', [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);
//      if (tradeLineItem.AssociatedDocument.LineID <> '') then
//      begin
//        Writer.WriteElementString('ram:LineID', tradeLineItem.AssociatedDocument.LineID);
//      end;
//linestatuscode ...
//      _writeNotes(Writer, tradeLineItem.AssociatedDocument.Notes);
//      Writer.WriteEndElement(); // ram:AssociatedDocumentLineDocument(Basic|Comfort|Extended|XRechnung)
//    end;
//    //#endregion
//
//    // handelt es sich um einen Kommentar?
//    var isCommentItem : Boolean := false;
//    if tradeLineItem.AssociatedDocument <> nil then
//    if ((tradeLineItem.AssociatedDocument.Notes.Count > 0) and
//        (tradeLineItem.BilledQuantity = 0) and
//        (tradeLineItem.Description<>'')) then
//    begin
//      isCommentItem := true;
//    end;
//
//    //#region SpecifiedTradeProduct
//    //Eine Gruppe von betriebswirtschaftlichen Begriffen, die Informationen über die in Rechnung gestellten Waren und Dienstleistungen enthält
//    Writer.WriteStartElement('ram:SpecifiedTradeProduct', [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);
//    if (tradeLineItem.GlobalID <> nil) then
//    if (tradeLineItem.GlobalID.SchemeID <> TZUGFeRDGlobalIDSchemeIdentifiers.Unknown) and (tradeLineItem.GlobalID.ID<> '') then
//    begin
//      _writeElementWithAttribute(Writer, 'ram:GlobalID', 'schemeID', TZUGFeRDGlobalIDSchemeIdentifiersExtensions.EnumToString(tradeLineItem.GlobalID.SchemeID), tradeLineItem.GlobalID.ID, [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);
//    end;
//
//    Writer.WriteOptionalElementString('ram:SellerAssignedID', tradeLineItem.SellerAssignedID, [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);
//    Writer.WriteOptionalElementString('ram:BuyerAssignedID', tradeLineItem.BuyerAssignedID, [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);
//
//    // BT-153
//    Writer.WriteOptionalElementString('ram:Name', tradeLineItem.Name, [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);
//    Writer.WriteOptionalElementString('ram:Name', ifthen(isCommentItem,'TEXT',tradeLineItem.Name), [TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]); // XRechnung erfordert einen Item-Namen (BR-25)
//
//    Writer.WriteOptionalElementString('ram:Description', tradeLineItem.Description, [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);
//
//    if (tradeLineItem.ApplicableProductCharacteristics <> nil) then
//    if (tradeLineItem.ApplicableProductCharacteristics.Count > 0) then
//    begin
//      for var productCharacteristic : TZUGFeRDApplicableProductCharacteristic in tradeLineItem.ApplicableProductCharacteristics do
//      begin
//        Writer.WriteStartElement('ram:ApplicableProductCharacteristic');
//        Writer.WriteOptionalElementString('ram:Description', productCharacteristic.Description);
//        Writer.WriteOptionalElementString('ram:Value', productCharacteristic.Value);
//        Writer.WriteEndElement(); // !ram:ApplicableProductCharacteristic
//      end
//    end;
//
//    Writer.WriteEndElement(); // !ram:SpecifiedTradeProduct(Basic|Comfort|Extended|XRechnung)
//    //#endregion
//    //#region SpecifiedLineTradeAgreement (Basic, Comfort, Extended, XRechnung)
//    //Eine Gruppe von betriebswirtschaftlichen Begriffen, die Informationen über den Preis für die in der betreffenden Rechnungsposition in Rechnung gestellten Waren und Dienstleistungen enthält
//
//    if (descriptor.Profile in [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]) then
//    begin
//      Writer.WriteStartElement('ram:SpecifiedLineTradeAgreement', [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);
//
//      //#region BuyerOrderReferencedDocument (Comfort, Extended, XRechnung)
//      //Detailangaben zur zugehörigen Bestellung
//      if (tradeLineItem.BuyerOrderReferencedDocument <> nil) then
//      begin
//        Writer.WriteStartElement('ram:BuyerOrderReferencedDocument', [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);
//
//        //#region IssuerAssignedID
//        //Bestellnummer
//        Writer.WriteOptionalElementString('ram:IssuerAssignedID', tradeLineItem.BuyerOrderReferencedDocument.ID);
//        //#endregion
//
//        //#region LineID
//        //Referenz zur Bestellposition
//      Writer.WriteOptionalElementString('ram:LineID', tradeLineItem.BuyerOrderReferencedDocument.LineID);
//        //#endregion
//
//        //#region IssueDateTime
//        if (tradeLineItem.BuyerOrderReferencedDocument.IssueDateTime.HasValue) then
//        begin
//          Writer.WriteStartElement('ram:FormattedIssueDateTime');
//          Writer.WriteStartElement('qdt:DateTimeString');
//          Writer.WriteAttributeString('format', '102');
//          Writer.WriteValue(_formatDate(tradeLineItem.BuyerOrderReferencedDocument.IssueDateTime.Value));
//          Writer.WriteEndElement(); // !qdt:DateTimeString
//          Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//        end;
//        //#endregion
//
//        Writer.WriteEndElement(); // !ram:BuyerOrderReferencedDocument
//      end;
//      //#endregion
//
//      //#region ContractReferencedDocument
//      //Detailangaben zum zugehörigen Vertrag
//      if (tradeLineItem.ContractReferencedDocument <> nil) then
//      begin
//        Writer.WriteStartElement('ram:ContractReferencedDocument', [TZUGFeRDProfile.Extended]);
//        if (tradeLineItem.ContractReferencedDocument.IssueDateTime.HasValue) then
//        begin
//          Writer.WriteStartElement('ram:FormattedIssueDateTime');
//          Writer.WriteStartElement('qdt:DateTimeString');
//          Writer.WriteAttributeString('format', '102');
//          Writer.WriteValue(_formatDate(tradeLineItem.ContractReferencedDocument.IssueDateTime.Value));
//          Writer.WriteEndElement(); // !udt:DateTimeString
//          Writer.WriteEndElement(); // !ram:IssueDateTime
//        end;
//        Writer.WriteOptionalElementString('ram:IssuerAssignedID', tradeLineItem.ContractReferencedDocument.ID);
//        Writer.WriteEndElement(); // !ram:ContractReferencedDocument(Extended)
//      end;
//      //#endregion
//
//      //#region AdditionalReferencedDocument (Extended)
//      //Detailangaben zu einer zusätzlichen Dokumentenreferenz
//      for var document : TZUGFeRDAdditionalReferencedDocument in tradeLineItem.AdditionalReferencedDocuments do
//      begin
//        Writer.WriteStartElement('ram:AdditionalReferencedDocument', [TZUGFeRDProfile.Extended]);
//        if (document.IssueDateTime.HasValue) then
//        begin
//            Writer.WriteStartElement('ram:FormattedIssueDateTime');
//            Writer.WriteStartElement('qdt:DateTimeString');
//            Writer.WriteAttributeString('format', '102');
//            Writer.WriteValue(_formatDate(document.IssueDateTime.Value));
//            Writer.WriteEndElement(); // !udt:DateTimeString
//            Writer.WriteEndElement(); // !ram:IssueDateTime
//        end;
//
//        Writer.WriteElementString('ram:LineID', Format('%d',[tradeLineItem.AssociatedDocument.LineID]));
//        Writer.WriteOptionalElementString('ram:IssuerAssignedID', document.ID);
//        Writer.WriteElementString('ram:ReferenceTypeCode', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(document.ReferenceTypeCode));
//
//        Writer.WriteEndElement(); // !ram:AdditionalReferencedDocument
//      end; // !foreach(document)
//      //#endregion
//
//      //#region GrossPriceProductTradePrice (Comfort, Extended, XRechnung)
//      var needToWriteGrossUnitPrice := false;
//
//      // the PEPPOL business rule for XRechnung is very specific
//      // PEPPOL-EN16931-R046
//      if ((descriptor.Profile = TZUGFeRDProfile.XRechnung) and
//          tradeLineItem.GrossUnitPrice.HasValue and
//          (tradeLineItem.TradeAllowanceCharges.Count > 0)) then
//      begin
//          needToWriteGrossUnitPrice := true;
//      end
//      else
//      if ((descriptor.Profile <> TZUGFeRDProfile.XRechnung) and
//         (tradeLineItem.GrossUnitPrice.HasValue or (tradeLineItem.TradeAllowanceCharges.Count > 0))) then
//      begin
//        needToWriteGrossUnitPrice := true;
//      end;
//
//      if (needToWriteGrossUnitPrice) then
//      begin
//        Writer.WriteStartElement('ram:GrossPriceProductTradePrice', [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);
//        _writeOptionalAmount(Writer, 'ram:ChargeAmount', tradeLineItem.GrossUnitPrice, 2); //BT-148
//        if (tradeLineItem.UnitQuantity.HasValue) then
//        begin
//          _writeElementWithAttribute(Writer, 'ram:BasisQuantity', 'unitCode', TZUGFeRDQuantityCodesExtensions.EnumToString(tradeLineItem.UnitCode), _formatDecimal(tradeLineItem.UnitQuantity.Value, 4));
//        end;
//
//        for var tradeAllowanceCharge : TZUGFeRDTradeAllowanceCharge in tradeLineItem.TradeAllowanceCharges do //BT-147
//        begin
//          Writer.WriteStartElement('ram:AppliedTradeAllowanceCharge');
//
//          //#region ChargeIndicator
//          Writer.WriteStartElement('ram:ChargeIndicator');
//          Writer.WriteElementString('udt:Indicator', ifthen(tradeAllowanceCharge.ChargeIndicator,'true','false'));
//          Writer.WriteEndElement(); // !ram:ChargeIndicator
//          //#endregion
//
//          //#region ChargePercentage
//          if (tradeAllowanceCharge.ChargePercentage <> 0.0) then
//          begin
//            Writer.WriteStartElement('ram:CalculationPercent', [TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung]);
//            Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ChargePercentage, 2));
//            Writer.WriteEndElement();
//          end;
//          //#endregion
//
//          //#region BasisAmount
//          if (tradeAllowanceCharge.BasisAmount <> 0.0) then
//          begin
//            Writer.WriteStartElement('ram:BasisAmount', [TZUGFeRDProfile.Extended]); // not in XRechnung, according to CII-SR-123
//            Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.BasisAmount, 2));
//            Writer.WriteEndElement();
//          end;
//          //#endregion
//
//          //#region ActualAmount
//          Writer.WriteStartElement('ram:ActualAmount');
//          Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ActualAmount, 2));
//          Writer.WriteEndElement();
//          //#endregion
//
//          if tradeAllowanceCharge.ChargeIndicator then
//          begin
//            Writer.WriteOptionalElementString('ram:ReasonCode',
//               TZUGFeRDSpecialServiceDescriptionCodesExtensions.EnumToString(
//                                         tradeAllowanceCharge.ReasonCodeCharge));
//          end else
//          begin
//            Writer.WriteOptionalElementString('ram:ReasonCode',
//               TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(
//                                         tradeAllowanceCharge.ReasonCodeAllowance));
//          end;
//
//          //c# means not in XRechnung according to CII-SR-128
//          //TODO Theoretisch auch Basic, Comfort, XRechnung
//          Writer.WriteOptionalElementString('ram:Reason', tradeAllowanceCharge.Reason, [TZUGFeRDProfile.Extended]);
//
//          Writer.WriteEndElement(); // !AppliedTradeAllowanceCharge
//        end;
//
//        Writer.WriteEndElement(); // ram:GrossPriceProductTradePrice(Comfort|Extended|XRechnung)
//      end;
//      //#endregion // !GrossPriceProductTradePrice(Comfort|Extended|XRechnung)
//
//      //#region NetPriceProductTradePrice
//      //Im Nettopreis sind alle Zu- und Abschläge enthalten, jedoch nicht die Umsatzsteuer.
//      Writer.WriteStartElement('ram:NetPriceProductTradePrice', [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);
//      _writeOptionalAmount(Writer, 'ram:ChargeAmount', tradeLineItem.NetUnitPrice, 2); //BT-146
//
//      if (tradeLineItem.UnitQuantity.HasValue) then
//      begin
//        _writeElementWithAttribute(Writer, 'ram:BasisQuantity', 'unitCode', TZUGFeRDQuantityCodesExtensions.EnumToString(tradeLineItem.UnitCode), _formatDecimal(tradeLineItem.UnitQuantity.Value, 4));
//      end;
//      Writer.WriteEndElement(); // ram:NetPriceProductTradePrice(Basic|Comfort|Extended|XRechnung)
//      //#endregion // !NetPriceProductTradePrice(Basic|Comfort|Extended|XRechnung)
//
//      //#region UltimateCustomerOrderReferencedDocument
//      //ToDo: UltimateCustomerOrderReferencedDocument
//      //#endregion
//      Writer.WriteEndElement(); // ram:SpecifiedLineTradeAgreement
//    end;
//    //#endregion
//
//    //#region SpecifiedLineTradeDelivery (Basic, Comfort, Extended)
//    Writer.WriteStartElement('ram:SpecifiedLineTradeDelivery', [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);
//    _writeElementWithAttribute(Writer, 'ram:BilledQuantity', 'unitCode', TZUGFeRDQuantityCodesExtensions.EnumToString(tradeLineItem.UnitCode), _formatDecimal(tradeLineItem.BilledQuantity, 4));
//    if tradeLineItem.PackageQuantity.HasValue then
//      _writeElementWithAttribute(Writer, 'ram:PackageQuantity', 'unitCode', TZUGFeRDQuantityCodesExtensions.EnumToString(tradeLineItem.PackageUnitCode), _formatDecimal(tradeLineItem.PackageQuantity, 4));
//    if tradeLineItem.ChargeFreeQuantity.HasValue then
//      _writeElementWithAttribute(Writer, 'ram:ChargeFreeQuantity', 'unitCode', TZUGFeRDQuantityCodesExtensions.EnumToString(tradeLineItem.ChargeFreeUnitCode), _formatDecimal(tradeLineItem.ChargeFreeQuantity, 4));
//
//    if (tradeLineItem.DeliveryNoteReferencedDocument <> nil) then
//    begin
//        Writer.WriteStartElement('ram:DeliveryNoteReferencedDocument', ALL_PROFILES - [TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]); // this violates CII-SR-175 for XRechnung 3
//        Writer.WriteOptionalElementString('ram:IssuerAssignedID', tradeLineItem.DeliveryNoteReferencedDocument.ID);
//
//        if (tradeLineItem.DeliveryNoteReferencedDocument.IssueDateTime.HasValue) then
//        begin
//          Writer.WriteStartElement('ram:FormattedIssueDateTime');
//          Writer.WriteStartElement('qdt:DateTimeString');
//          Writer.WriteAttributeString('format', '102');
//          Writer.WriteValue(_formatDate(tradeLineItem.DeliveryNoteReferencedDocument.IssueDateTime.Value));
//          Writer.WriteEndElement(); // !qdt:DateTimeString
//          Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//        end;
//
//        Writer.WriteEndElement(); // !ram:DeliveryNoteReferencedDocument
//    end;
//
//    if (tradeLineItem.ActualDeliveryDate.HasValue) then
//    begin
//      Writer.WriteStartElement('ram:ActualDeliverySupplyChainEvent', ALL_PROFILES - [TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]); // this violates CII-SR-170 for XRechnung 3
//      Writer.WriteStartElement('ram:OccurrenceDateTime');
//      Writer.WriteStartElement('udt:DateTimeString');
//      Writer.WriteAttributeString('format', '102');
//      Writer.WriteValue(_formatDate(tradeLineItem.ActualDeliveryDate.Value));
//      Writer.WriteEndElement(); // !udt:DateTimeString
//      Writer.WriteEndElement(); // !OccurrenceDateTime()
//      Writer.WriteEndElement(); // !ActualDeliverySupplyChainEvent
//    end;
//
//    Writer.WriteEndElement(); // !ram:SpecifiedLineTradeDelivery
//    //#endregion
//
//    //#region SpecifiedLineTradeSettlement
//    Writer.WriteStartElement('ram:SpecifiedLineTradeSettlement', [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);
//    //#region ApplicableTradeTax
//    Writer.WriteStartElement('ram:ApplicableTradeTax', [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);
//    Writer.WriteElementString('ram:TypeCode', TZUGFeRDTaxTypesExtensions.EnumToString(tradeLineItem.TaxType));
//    Writer.WriteOptionalElementString('ram:ExemptionReason', _translateTaxCategoryCode(tradeLineItem.TaxCategoryCode), [TZUGFeRDProfile.Extended]);
//    Writer.WriteElementString('ram:CategoryCode', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(tradeLineItem.TaxCategoryCode)); // BT-151
//
//    if (tradeLineItem.TaxCategoryCode <> TZUGFeRDTaxCategoryCodes.O) then // notwendig, damit die Validierung klappt
//    begin
//      Writer.WriteElementString('ram:RateApplicablePercent', _formatDecimal(tradeLineItem.TaxPercent));
//    end;
//
//    Writer.WriteEndElement(); // !ram:ApplicableTradeTax(Basic|Comfort|Extended|XRechnung)
//    //#endregion // !ApplicableTradeTax(Basic|Comfort|Extended|XRechnung)
//
//    //#region BillingSpecifiedPeriod
//    if (tradeLineItem.BillingPeriodStart.HasValue or tradeLineItem.BillingPeriodEnd.HasValue) then
//    begin
//      Writer.WriteStartElement('ram:BillingSpecifiedPeriod', [TZUGFeRDProfile.BasicWL,TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);
//      if (tradeLineItem.BillingPeriodStart.HasValue) then
//      begin
//        Writer.WriteStartElement('ram:StartDateTime');
//        _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(tradeLineItem.BillingPeriodStart.Value));
//        Writer.WriteEndElement(); // !StartDateTime
//      end;
//
//      if (tradeLineItem.BillingPeriodEnd.HasValue) then
//      begin
//        Writer.WriteStartElement('ram:EndDateTime');
//        _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(tradeLineItem.BillingPeriodEnd.Value));
//        Writer.WriteEndElement(); // !EndDateTime
//      end;
//      Writer.WriteEndElement(); // !BillingSpecifiedPeriod
//    end;
//    //#endregion
//
//    //#region SpecifiedTradeAllowanceCharge (Basic, Comfort, Extended)
//    //Abschläge auf Ebene der Rechnungsposition (Basic, Comfort, Extended)
//    if (descriptor.Profile in [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung]) then
//    if (tradeLineItem.SpecifiedTradeAllowanceCharges.Count > 0) then
//    begin
//      Writer.WriteStartElement('ram:SpecifiedTradeAllowanceCharge', [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung]);
//      for var specifiedTradeAllowanceCharge : TZUGFeRDTradeAllowanceCharge in tradeLineItem.SpecifiedTradeAllowanceCharges do // BG-27 BG-28
//      begin
//        //#region ChargeIndicator
//        Writer.WriteStartElement('ram:ChargeIndicator');
//        Writer.WriteElementString('udt:Indicator', ifthen(specifiedTradeAllowanceCharge.ChargeIndicator,'true','false'));
//        Writer.WriteEndElement(); // !ram:ChargeIndicator
//        //#endregion
//
//        //#region ChargePercentage
//        if (specifiedTradeAllowanceCharge.ChargePercentage <> 0.0) then
//        begin
//          Writer.WriteStartElement('ram:CalculationPercent', [TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung]);
//          Writer.WriteValue(_formatDecimal(specifiedTradeAllowanceCharge.ChargePercentage, 2));
//          Writer.WriteEndElement();
//        end;
//        //#endregion
//
//        //#region BasisAmount
//        if (specifiedTradeAllowanceCharge.BasisAmount <> 0.0) then
//        begin
//          Writer.WriteStartElement('ram:BasisAmount', [TZUGFeRDProfile.Extended]); // not in XRechnung, according to CII-SR-123
//          Writer.WriteValue(_formatDecimal(specifiedTradeAllowanceCharge.BasisAmount, 2));
//          Writer.WriteEndElement();
//        end;
//        //#endregion
//
//        //#region ActualAmount
//        Writer.WriteStartElement('ram:ActualAmount');
//        Writer.WriteValue(_formatDecimal(specifiedTradeAllowanceCharge.ActualAmount, 2));
//        Writer.WriteEndElement();
//        //#endregion
//
//        if specifiedTradeAllowanceCharge.ChargeIndicator then
//        begin
//          Writer.WriteOptionalElementString('ram:ReasonCode',
//             TZUGFeRDSpecialServiceDescriptionCodesExtensions.EnumToString(
//                                       specifiedTradeAllowanceCharge.ReasonCodeCharge));
//        end else
//        begin
//          Writer.WriteOptionalElementString('ram:ReasonCode',
//             TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(
//                                       specifiedTradeAllowanceCharge.ReasonCodeAllowance));
//        end;
//
//        //c# means not in XRechnung according to CII-SR-128
//        //TODO Theoretisch auch Basic, Comfort, XRechnung
//        Writer.WriteOptionalElementString('ram:Reason', specifiedTradeAllowanceCharge.Reason, [TZUGFeRDProfile.Extended]);
//      end;
//      Writer.WriteEndElement(); // !ram:SpecifiedTradeAllowanceCharge
//    end;
//    //#endregion
//
//    //#region SpecifiedTradeSettlementLineMonetarySummation (Basic, Comfort, Extended)
//    //Detailinformationen zu Positionssummen
//    Writer.WriteStartElement('ram:SpecifiedTradeSettlementLineMonetarySummation');
//
//    var _total : double := 0;
//
//    if (tradeLineItem.LineTotalAmount.HasValue) then
//    begin
//      _total := tradeLineItem.LineTotalAmount.Value;
//    end
//    else if (tradeLineItem.NetUnitPrice.HasValue) then
//    begin
//      _total := tradeLineItem.NetUnitPrice.Value * tradeLineItem.BilledQuantity;
//      if tradeLineItem.UnitQuantity.HasValue then
//      if (tradeLineItem.UnitQuantity.Value <> 0) then
//      begin
//        _total := _total / tradeLineItem.UnitQuantity.Value;
//      end;
//    end;
//
//    Writer.WriteStartElement('ram:LineTotalAmount', [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);
//    Writer.WriteValue(_formatDecimal(_total));
//    Writer.WriteEndElement(); // !ram:LineTotalAmount
//
//    //ToDo: TotalAllowanceChargeAmount
//    //Gesamtbetrag der Positionszu- und Abschläge
//    Writer.WriteEndElement(); // ram:SpecifiedTradeSettlementMonetarySummation
//    //#endregion
//
//    //#region AdditionalReferencedDocument
//    //Objektkennung auf Ebene der Rechnungsposition
//    //ToDo: AdditionalReferencedDocument
//    //#endregion
//
//    //#region ReceivableSpecifiedTradeAccountingAccount
//    //Detailinformationen zur Buchungsreferenz
//    if (((descriptor.Profile = TZUGFeRDProfile.XRechnung1) or
//         (descriptor.Profile = TZUGFeRDProfile.XRechnung)) and
//         (tradeLineItem.ReceivableSpecifiedTradeAccountingAccounts.Count > 0)) then
//    begin
//      //only one ReceivableSpecifiedTradeAccountingAccount (BT-133) is allowed in Profile XRechnung
//      Writer.WriteStartElement('ram:ReceivableSpecifiedTradeAccountingAccount', [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);
//      begin
//        Writer.WriteStartElement('ram:ID');
//        Writer.WriteValue(tradeLineItem.ReceivableSpecifiedTradeAccountingAccounts[0].TradeAccountID);  //BT-133
//        Writer.WriteEndElement(); // !ram:ID
//      end;
//      Writer.WriteEndElement(); // !ram:ReceivableSpecifiedTradeAccountingAccount
//    end
//    else
//    begin
//      //multiple ReceivableSpecifiedTradeAccountingAccounts are allowed in other profiles
//      for var RSTA : TZUGFeRDReceivableSpecifiedTradeAccountingAccount in tradeLineItem.ReceivableSpecifiedTradeAccountingAccounts do
//      begin
//        Writer.WriteStartElement('ram:ReceivableSpecifiedTradeAccountingAccount', [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);
//
//        Writer.WriteStartElement('ram:ID');
//        Writer.WriteValue(RSTA.TradeAccountID);
//        Writer.WriteEndElement(); // !ram:ID
//
//        if (RSTA.TradeAccountTypeCode <> TZUGFeRDAccountingAccountTypeCodes.Unknown) then
//        begin
//          Writer.WriteStartElement('ram:TypeCode', [TZUGFeRDProfile.Extended]);
//          Writer.WriteValue((Integer(RSTA.TradeAccountTypeCode)).ToString);
//          Writer.WriteEndElement(); // !ram:TypeCode
//        end;
//
//        Writer.WriteEndElement(); // !ram:ReceivableSpecifiedTradeAccountingAccount
//      end;
//    end;
//    //#endregion
//
//    Writer.WriteEndElement(); // !ram:SpecifiedLineTradeSettlement
//    //#endregion
//
//    Writer.WriteEndElement(); // !ram:IncludedSupplyChainTradeLineItem
//  end; // !foreach(tradeLineItem)
//  // #endregion
//
//  //#region ApplicableHeaderTradeAgreement
//  Writer.WriteStartElement('ram:ApplicableHeaderTradeAgreement');
//
//  // BT-10
//  Writer.WriteOptionalElementString('ram:BuyerReference', Descriptor.ReferenceOrderNo);
//
//  //#region SellerTradeParty
//  // BT-31: Descriptor.SellerTaxRegistration
//  _writeOptionalParty(Writer, TZUGFeRDPartyTypes.SellerTradeParty, Descriptor.Seller, Descriptor.SellerContact, Descriptor.SellerElectronicAddress, Descriptor.SellerTaxRegistration);
//  //#endregion
//
//  //region BuyerTradeParty
//  // BT-48: Descriptor.BuyerTaxRegistration
//  _writeOptionalParty(Writer, TZUGFeRDPartyTypes.BuyerTradeParty, Descriptor.Buyer, Descriptor.BuyerContact, Descriptor.BuyerElectronicAddress, Descriptor.BuyerTaxRegistration);
//  //#endregion
//
//  // TODO: implement SellerTaxRepresentativeTradeParty
//  // BT-63: the tax registration of the SellerTaxRepresentativeTradeParty
//
//  //#region SellerOrderReferencedDocument (BT-14: Comfort, Extended)
//  if (Descriptor.SellerOrderReferencedDocument <> nil) then
//  if (Descriptor.SellerOrderReferencedDocument.ID <> '') then
//  begin
//    Writer.WriteStartElement('ram:SellerOrderReferencedDocument', [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended, TZUGFeRDProfile.XRechnung1, TZUGFeRDProfile.XRechnung]);
//    Writer.WriteElementString('ram:IssuerAssignedID', Descriptor.SellerOrderReferencedDocument.ID);
//    if (Descriptor.SellerOrderReferencedDocument.IssueDateTime.HasValue) then
//    begin
//      Writer.WriteStartElement('ram:FormattedIssueDateTime', [TZUGFeRDProfile.Extended]);
//      Writer.WriteStartElement('qdt:DateTimeString');
//      Writer.WriteAttributeString('format', '102');
//      Writer.WriteValue(_formatDate(Descriptor.SellerOrderReferencedDocument.IssueDateTime.Value));
//      Writer.WriteEndElement(); // !qdt:DateTimeString
//      Writer.WriteEndElement(); // !IssueDateTime()
//    end;
//
//    Writer.WriteEndElement(); // !SellerOrderReferencedDocument
//  end;
//  //#endregion
//
//  //#region BuyerOrderReferencedDocument
//  if (Descriptor.OrderNo <> '') then
//  begin
//    Writer.WriteStartElement('ram:BuyerOrderReferencedDocument');
//    Writer.WriteElementString('ram:IssuerAssignedID', Descriptor.OrderNo);
//    if (Descriptor.OrderDate.HasValue) then
//    begin
//      Writer.WriteStartElement('ram:FormattedIssueDateTime', ALL_PROFILES - [TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);
//      Writer.WriteStartElement('qdt:DateTimeString');
//      Writer.WriteAttributeString('format', '102');
//      Writer.WriteValue(_formatDate(Descriptor.OrderDate.Value));
//      Writer.WriteEndElement(); // !qdt:DateTimeString
//      Writer.WriteEndElement(); // !IssueDateTime()
//    end;
//
//    Writer.WriteEndElement(); // !BuyerOrderReferencedDocument
//  end;
//  //#endregion
//
//  //#region ContractReferencedDocument
//  // BT-12
//  if (Descriptor.ContractReferencedDocument <> nil) then
//  begin
//    Writer.WriteStartElement('ram:ContractReferencedDocument');
//    Writer.WriteElementString('ram:IssuerAssignedID', Descriptor.ContractReferencedDocument.ID);
//    if (Descriptor.ContractReferencedDocument.IssueDateTime.HasValue) then
//    begin
//      Writer.WriteStartElement('ram:FormattedIssueDateTime', ALL_PROFILES  - [TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);
//      Writer.WriteStartElement('qdt:DateTimeString');
//      Writer.WriteAttributeString('format', '102');
//      Writer.WriteValue(_formatDate(Descriptor.ContractReferencedDocument.IssueDateTime.Value));
//      Writer.WriteEndElement(); // !qdt:DateTimeString
//      Writer.WriteEndElement(); // !IssueDateTime()
//    end;
//
//    Writer.WriteEndElement(); // !ram:ContractReferencedDocument
//  end;
//  //#endregion
//
//  //#region AdditionalReferencedDocument
//  if (Descriptor.AdditionalReferencedDocuments <> nil) then
//  begin
//    for var document : TZUGFeRDAdditionalReferencedDocument in Descriptor.AdditionalReferencedDocuments do
//    begin
//      Writer.WriteStartElement('ram:AdditionalReferencedDocument', [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung]);
//      Writer.WriteElementString('ram:IssuerAssignedID', document.ID);
//      Writer.WriteElementString('ram:TypeCode', TZUGFeRDAdditionalReferencedDocumentTypeCodeExtensions.EnumToString(document.TypeCode));
//
//      if (document.ReferenceTypeCode <> TZUGFeRDReferenceTypeCodes.Unknown) then
//      begin
//        Writer.WriteElementString('ram:ReferenceTypeCode', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(document.ReferenceTypeCode));
//      end;
//
//      Writer.WriteOptionalElementString('ram:Name', document.Name);
//
//      if (document.AttachmentBinaryObject <> nil) then
//      begin
//        Writer.WriteStartElement('ram:AttachmentBinaryObject');
//        Writer.WriteAttributeString('filename', document.Filename);
//        Writer.WriteAttributeString('mimeCode', TZUGFeRDMimeTypeMapper.GetMimeType(document.Filename));
//        Writer.WriteValue(TZUGFeRDHelper.GetDataAsBase64(document.AttachmentBinaryObject));
//        Writer.WriteEndElement(); // !AttachmentBinaryObject()
//      end;
//
//      if (document.IssueDateTime.HasValue) then
//      begin
//        Writer.WriteStartElement('ram:FormattedIssueDateTime');
//        Writer.WriteStartElement('qdt:DateTimeString');
//        Writer.WriteAttributeString('format', '102');
//        Writer.WriteValue(_formatDate(document.IssueDateTime.Value));
//        Writer.WriteEndElement(); // !qdt:DateTimeString
//        Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//      end;
//
//      Writer.WriteEndElement(); // !ram:AdditionalReferencedDocument
//    end;
//  end;
//  //#endregion
//
//  //#region SpecifiedProcuringProject
//  if (Descriptor.SpecifiedProcuringProject <> nil) then
//  begin
//    Writer.WriteStartElement('ram:SpecifiedProcuringProject', [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);
//    Writer.WriteElementString('ram:ID', Descriptor.SpecifiedProcuringProject.ID, [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);
//    Writer.WriteElementString('ram:Name', Descriptor.SpecifiedProcuringProject.Name, [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);
//    Writer.WriteEndElement(); // !ram:SpecifiedProcuringProject
//  end;
//  //#endregion
//
//  Writer.WriteEndElement(); // !ApplicableHeaderTradeAgreement
//  //#endregion
//
//  //#region ApplicableHeaderTradeDelivery
//  Writer.WriteStartElement('ram:ApplicableHeaderTradeDelivery'); // Pflichteintrag
//  _writeOptionalParty(Writer, TZUGFeRDPartyTypes.ShipToTradeParty, Descriptor.ShipTo);
//  //ToDo: UltimateShipToTradeParty
//  _writeOptionalParty(Writer, TZUGFeRDPartyTypes.ShipFromTradeParty, Descriptor.ShipFrom); // ShipFrom shall not be written in XRechnung profiles
//
//  //#region ActualDeliverySupplyChainEvent
//  if (Descriptor.ActualDeliveryDate.HasValue) then
//  begin
//    Writer.WriteStartElement('ram:ActualDeliverySupplyChainEvent');
//    Writer.WriteStartElement('ram:OccurrenceDateTime');
//    Writer.WriteStartElement('udt:DateTimeString');
//    Writer.WriteAttributeString('format', '102');
//    Writer.WriteValue(_formatDate(Descriptor.ActualDeliveryDate.Value));
//    Writer.WriteEndElement(); // 'udt:DateTimeString
//    Writer.WriteEndElement(); // !OccurrenceDateTime()
//    Writer.WriteEndElement(); // !ActualDeliverySupplyChainEvent
//  end;
//  //#endregion
//
//  //#region DespatchAdviceReferencedDocument
//  if (Descriptor.DespatchAdviceReferencedDocument <> nil) then
//  begin
//    Writer.WriteStartElement('ram:DespatchAdviceReferencedDocument');
//    Writer.WriteElementString('ram:IssuerAssignedID', Descriptor.DespatchAdviceReferencedDocument.ID);
//
//    if (Descriptor.DespatchAdviceReferencedDocument.IssueDateTime.HasValue) then
//    begin
//      Writer.WriteStartElement('ram:FormattedIssueDateTime');
//      Writer.WriteStartElement('qdt:DateTimeString');
//      Writer.WriteAttributeString('format', '102');
//      Writer.WriteValue(_formatDate(Descriptor.DespatchAdviceReferencedDocument.IssueDateTime.Value));
//      Writer.WriteEndElement(); // "qdt:DateTimeString
//      Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//    end;
//
//    Writer.WriteEndElement(); // !DespatchAdviceReferencedDocument
//  end;
//  //#endregion
//
//  //#region DeliveryNoteReferencedDocument
//  if (Descriptor.DeliveryNoteReferencedDocument <> nil) then
//  begin
//    Writer.WriteStartElement('ram:DeliveryNoteReferencedDocument');
//    Writer.WriteElementString('ram:IssuerAssignedID', Descriptor.DeliveryNoteReferencedDocument.ID);
//
//    if (Descriptor.DeliveryNoteReferencedDocument.IssueDateTime.HasValue) then
//    begin
//      Writer.WriteStartElement('ram:FormattedIssueDateTime', ALL_PROFILES  - [TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);
//      Writer.WriteStartElement('qdt:DateTimeString');
//      Writer.WriteAttributeString('format', '102');
//      Writer.WriteValue(_formatDate(Descriptor.DeliveryNoteReferencedDocument.IssueDateTime.Value));
//      Writer.WriteEndElement(); // 'qdt:DateTimeString
//      Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//    end;
//
//    Writer.WriteEndElement(); // !DeliveryNoteReferencedDocument
//  end;
//  //#endregion
//
//  Writer.WriteEndElement(); // !ApplicableHeaderTradeDelivery
//  //#endregion
//
//  //#region ApplicableHeaderTradeSettlement
//  Writer.WriteStartElement('ram:ApplicableHeaderTradeSettlement');
//  // order of sub-elements of ApplicableHeaderTradeSettlement:
//  //   1. CreditorReferenceID (optional)
//  //   2. PaymentReference (optional)
//  //   3. TaxCurrencyCode (optional)
//  //   4. InvoiceCurrencyCode (optional)
//  //   5. InvoiceIssuerReference (optional)
//  //   6. InvoicerTradeParty (optional)
//  //   7. InvoiceeTradeParty (optional)
//  //   8. PayeeTradeParty (optional)
//  //   9. TaxApplicableTradeCurrencyExchange (optional)
//  //  10. SpecifiedTradeSettlementPaymentMeans (optional)
//  //  11. ApplicableTradeTax (optional)
//  //  12. BillingSpecifiedPeriod (optional)
//  //  13. SpecifiedTradeAllowanceCharge (optional)
//  //  14. SpecifiedLogisticsServiceCharge (optional)
//  //  15. SpecifiedTradePaymentTerms (optional)
//  //  16. SpecifiedTradeSettlementHeaderMonetarySummation
//  //  17. InvoiceReferencedDocument (optional)
//  //  18. ReceivableSpecifiedTradeAccountingAccount (optional)
//  //  19. SpecifiedAdvancePayment (optional)
//
//  //   1. CreditorReferenceID (optional)
//  if Descriptor.PaymentMeans <> nil then
//  if (Descriptor.PaymentMeans.SEPACreditorIdentifier<>'') then
//  begin
//    Writer.WriteOptionalElementString('ram:CreditorReferenceID', Descriptor.PaymentMeans.SEPACreditorIdentifier, [TZUGFeRDProfile.BasicWL,TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);
//  end;
//
//  //   2. PaymentReference (optional)
//  Writer.WriteOptionalElementString('ram:PaymentReference', Descriptor.PaymentReference);
//
//  //   4. InvoiceCurrencyCode (optional)
//  Writer.WriteElementString('ram:InvoiceCurrencyCode', TZUGFeRDCurrencyCodesExtensions.EnumToString(Descriptor.Currency));
//
//  //   7. InvoiceeTradeParty (optional)
//  _writeOptionalParty(Writer, TZUGFeRDPartyTypes.InvoiceeTradeParty, Descriptor.Invoicee);
//
//  //   8. PayeeTradeParty (optional)
//  _writeOptionalParty(Writer, TZUGFeRDPartyTypes.PayeeTradeParty, Descriptor.Payee);
//
//  //#region SpecifiedTradeSettlementPaymentMeans
//  //  10. SpecifiedTradeSettlementPaymentMeans (optional)
//
//  if (Descriptor.CreditorBankAccounts.Count = 0) and (Descriptor.DebitorBankAccounts.Count = 0) then
//  begin
//    if (Descriptor.PaymentMeans <> nil) then
//    if (Descriptor.PaymentMeans.TypeCode <> TZUGFeRDPaymentMeansTypeCodes.Unknown) then
//    begin
//      Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans', [TZUGFeRDProfile.BasicWL,TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);
//      Writer.WriteElementString('ram:TypeCode', TZUGFeRDPaymentMeansTypeCodesExtensions.EnumToString(Descriptor.PaymentMeans.TypeCode));
//      Writer.WriteOptionalElementString('ram:Information', Descriptor.PaymentMeans.Information);
//
//      if (Descriptor.PaymentMeans.FinancialCard <> nil) then
//      begin
//        Writer.WriteStartElement('ram:ApplicableTradeSettlementFinancialCard', [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung]);
//        Writer.WriteOptionalElementString('ram:ID', Descriptor.PaymentMeans.FinancialCard.Id);
//        Writer.WriteOptionalElementString('ram:CardholderName', Descriptor.PaymentMeans.FinancialCard.CardholderName);
//        Writer.WriteEndElement(); // !ram:ApplicableTradeSettlementFinancialCard
//      end;
//      Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
//    end;
//  end
//  else
//  begin
//    for var creditorAccount : TZUGFeRDBankAccount in Descriptor.CreditorBankAccounts do
//    begin
//      Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans');
//
//      if (Descriptor.PaymentMeans <> nil) then
//      if (Descriptor.PaymentMeans.TypeCode <> TZUGFeRDPaymentMeansTypeCodes.Unknown) then
//      begin
//        Writer.WriteElementString('ram:TypeCode', TZUGFeRDPaymentMeansTypeCodesExtensions.EnumToString(Descriptor.PaymentMeans.TypeCode));
//        Writer.WriteOptionalElementString('ram:Information', Descriptor.PaymentMeans.Information);
//
//        if (Descriptor.PaymentMeans.FinancialCard <> nil) then
//        begin
//            Writer.WriteStartElement('ram:ApplicableTradeSettlementFinancialCard', [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung]);
//            Writer.WriteOptionalElementString('ram:ID', Descriptor.PaymentMeans.FinancialCard.Id);
//            Writer.WriteOptionalElementString('ram:CardholderName', Descriptor.PaymentMeans.FinancialCard.CardholderName);
//            Writer.WriteEndElement(); // !ram:ApplicableTradeSettlementFinancialCard
//        end;
//      end;
//
//      Writer.WriteStartElement('ram:PayeePartyCreditorFinancialAccount');
//      Writer.WriteElementString('ram:IBANID', creditorAccount.IBAN);
//      Writer.WriteOptionalElementString('ram:AccountName', creditorAccount.Name);
//      Writer.WriteOptionalElementString('ram:ProprietaryID', creditorAccount.ID);
//      Writer.WriteEndElement(); // !PayeePartyCreditorFinancialAccount
//
//      if (creditorAccount.BIC<>'') then
//      begin
//        Writer.WriteStartElement('ram:PayeeSpecifiedCreditorFinancialInstitution');
//        Writer.WriteElementString('ram:BICID', creditorAccount.BIC);
//        Writer.WriteEndElement(); // !PayeeSpecifiedCreditorFinancialInstitution
//      end;
//
//      Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
//    end;
//
//    for var debitorAccount : TZUGFeRDBankAccount in Descriptor.DebitorBankAccounts do
//    begin
//      Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans'); // BG-16
//
//      if (Descriptor.PaymentMeans <> nil) then
//      if (Descriptor.PaymentMeans.TypeCode <> TZUGFeRDPaymentMeansTypeCodes.Unknown) then
//      begin
//        Writer.WriteElementString('ram:TypeCode', TZUGFeRDPaymentMeansTypeCodesExtensions.EnumToString(Descriptor.PaymentMeans.TypeCode));
//        Writer.WriteOptionalElementString('ram:Information', Descriptor.PaymentMeans.Information);
//      end;
//
//      Writer.WriteStartElement('ram:PayerPartyDebtorFinancialAccount');
//      Writer.WriteElementString('ram:IBANID', debitorAccount.IBAN);
//      Writer.WriteOptionalElementString('ram:AccountName', debitorAccount.Name);
//      Writer.WriteOptionalElementString('ram:ProprietaryID', debitorAccount.ID);
//      Writer.WriteEndElement(); // !PayerPartyDebtorFinancialAccount
//
//      if (debitorAccount.BIC<>'') then
//      begin
//        Writer.WriteStartElement('ram:PayerSpecifiedDebtorFinancialInstitution');
//        Writer.WriteElementString('ram:BICID', debitorAccount.BIC);
//        Writer.WriteEndElement(); // !PayerSpecifiedDebtorFinancialInstitution
//      end;
//
//      Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
//    end;
//  end;
//  //#endregion
//
//  //#region ApplicableTradeTax
//  //  11. ApplicableTradeTax (optional)
//  _writeOptionalTaxes(Writer);
//  //#endregion
//
//  //#region BillingSpecifiedPeriod
//  //  12. BillingSpecifiedPeriod (optional)
//  if (Descriptor.BillingPeriodStart>100) or (Descriptor.BillingPeriodEnd>100) then
//  begin
//    Writer.WriteStartElement('ram:BillingSpecifiedPeriod', [TZUGFeRDProfile.BasicWL,TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);
//    if (Descriptor.BillingPeriodStart>100) then
//    begin
//        Writer.WriteStartElement('ram:StartDateTime');
//        _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(Descriptor.BillingPeriodStart));
//        Writer.WriteEndElement(); // !StartDateTime
//    end;
//
//    if (Descriptor.BillingPeriodEnd>100) then
//    begin
//        Writer.WriteStartElement('ram:EndDateTime');
//        _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(Descriptor.BillingPeriodEnd));
//        Writer.WriteEndElement(); // !EndDateTime
//    end;
//    Writer.WriteEndElement(); // !BillingSpecifiedPeriod
//  end;
//  //#endregion
//
//  //13. SpecifiedTradeAllowanceCharge (optional)
//  for var tradeAllowanceCharge : TZUGFeRDTradeAllowanceCharge in Descriptor.TradeAllowanceCharges do
//  begin
//    Writer.WriteStartElement('ram:SpecifiedTradeAllowanceCharge');
//    Writer.WriteStartElement('ram:ChargeIndicator');
//    Writer.WriteElementString('udt:Indicator', ifthen(tradeAllowanceCharge.ChargeIndicator,'true','false'));
//    Writer.WriteEndElement(); // !ram:ChargeIndicator
//
//    if (tradeAllowanceCharge.ChargePercentage <> 0.0) then
//    begin
//      Writer.WriteStartElement('ram:CalculationPercent', [TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);
//      Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ChargePercentage, 2));
//      Writer.WriteEndElement();
//    end;
//
//    if (tradeAllowanceCharge.BasisAmount <> 0.0) then
//    begin
//      Writer.WriteStartElement('ram:BasisAmount', [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);
//      Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.BasisAmount));
//      Writer.WriteEndElement();
//    end;
//
//    Writer.WriteStartElement('ram:ActualAmount');
//    Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ActualAmount, 2));
//    Writer.WriteEndElement();
//
//    if tradeAllowanceCharge.ChargeIndicator then
//    begin
//      Writer.WriteOptionalElementString('ram:ReasonCode',
//         TZUGFeRDSpecialServiceDescriptionCodesExtensions.EnumToString(
//                                   tradeAllowanceCharge.ReasonCodeCharge));
//    end else
//    begin
//      Writer.WriteOptionalElementString('ram:ReasonCode',
//         TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(
//                                   tradeAllowanceCharge.ReasonCodeAllowance));
//    end;
//
//    Writer.WriteOptionalElementString('ram:Reason', tradeAllowanceCharge.Reason);
//
//    if (tradeAllowanceCharge.Tax <> nil) then
//    begin
//      Writer.WriteStartElement('ram:CategoryTradeTax');
//      Writer.WriteElementString('ram:TypeCode', TZUGFeRDTaxTypesExtensions.EnumToString(tradeAllowanceCharge.Tax.TypeCode));
//      if (tradeAllowanceCharge.Tax.CategoryCode <> TZUGFeRDTaxCategoryCodes.Unknown) then
//        Writer.WriteElementString('ram:CategoryCode', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(tradeAllowanceCharge.Tax.CategoryCode));
//      Writer.WriteElementString('ram:RateApplicablePercent', _formatDecimal(tradeAllowanceCharge.Tax.Percent));
//      Writer.WriteEndElement();
//    end;
//    Writer.WriteEndElement();
//  end;
//
//  //  14. SpecifiedLogisticsServiceCharge (optional)
//  for var serviceCharge : TZUGFeRDServiceCharge in Descriptor.ServiceCharges do
//  begin
//    Writer.WriteStartElement('ram:SpecifiedLogisticsServiceCharge', ALL_PROFILES  - [TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);
//    Writer.WriteOptionalElementString('ram:Description', serviceCharge.Description);
//    Writer.WriteElementString('ram:AppliedAmount', _formatDecimal(serviceCharge.Amount));
//    if (serviceCharge.Tax <> nil) then
//    begin
//      Writer.WriteStartElement('ram:AppliedTradeTax');
//      Writer.WriteElementString('ram:TypeCode', TZUGFeRDTaxTypesExtensions.EnumToString(serviceCharge.Tax.TypeCode));
//      if (serviceCharge.Tax.CategoryCode <> TZUGFeRDTaxCategoryCodes.Unknown) then
//        Writer.WriteElementString('ram:CategoryCode', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(serviceCharge.Tax.CategoryCode));
//      Writer.WriteElementString('ram:RateApplicablePercent', _formatDecimal(serviceCharge.Tax.Percent));
//      Writer.WriteEndElement();
//    end;
//    Writer.WriteEndElement();
//  end;
//
//  //  15. SpecifiedTradePaymentTerms (optional)
//  for var PaymentTerms: TZUGFeRDPaymentTerms in Descriptor.PaymentTermsList do
//  begin
//    Writer.WriteStartElement('ram:SpecifiedTradePaymentTerms');
//    Writer.WriteOptionalElementString('ram:Description', PaymentTerms.Description);
//    if (PaymentTerms.DueDate.HasValue) then
//    begin
//      Writer.WriteStartElement('ram:DueDateDateTime');
//      _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(PaymentTerms.DueDate.Value));
//      Writer.WriteEndElement(); // !ram:DueDateDateTime
//    end;
//    Writer.WriteOptionalElementString('ram:DirectDebitMandateID', PaymentTerms.DirectDebitMandateID);
//    //TODO PaymentTerms.PartialPaymentAmount
//    //TODO PaymentTerms.ApplicableTradePaymentPenaltyTerms
//    if (PaymentTerms.ApplicableTradePaymentDiscountTerms.BasisAmount <> 0.0) or
//       (PaymentTerms.ApplicableTradePaymentDiscountTerms.CalculationPercent <> 0.0) or
//       PaymentTerms.ApplicableTradePaymentDiscountTerms.BasisPeriodMeasure.HasValue then
//    begin
//      Writer.WriteStartElement('ram:ApplicableTradePaymentDiscountTerms');
//      if PaymentTerms.ApplicableTradePaymentDiscountTerms.BasisPeriodMeasure.HasValue then
//        _writeElementWithAttribute(Writer, 'ram:BasisPeriodMeasure', 'unitCode', TZUGFeRDQuantityCodesExtensions.EnumToString(PaymentTerms.ApplicableTradePaymentDiscountTerms.UnitCode), _formatDecimal(paymentTerms.ApplicableTradePaymentDiscountTerms.BasisPeriodMeasure, 4));
//      if PaymentTerms.ApplicableTradePaymentDiscountTerms.BasisAmount <> 0.0 then
//        _writeOptionalAmount(Writer, 'ram:BasisAmount', PaymentTerms.ApplicableTradePaymentDiscountTerms.BasisAmount);
//      if PaymentTerms.ApplicableTradePaymentDiscountTerms.CalculationPercent <> 0.0 then
//        _writeOptionalAmount(Writer, 'ram:CalculationPercent', PaymentTerms.ApplicableTradePaymentDiscountTerms.CalculationPercent,4);
//      Writer.WriteEndElement();
//      //TODO PaymentTerms.ApplicableTradePaymentDiscountTerms.ActualPenaltyAmount
//    end;
//    Writer.WriteEndElement();
//  end;
//
//  //#region SpecifiedTradeSettlementHeaderMonetarySummation
//  //  16. SpecifiedTradeSettlementHeaderMonetarySummation
//  //Gesamtsummen auf Dokumentenebene
//  Writer.WriteStartElement('ram:SpecifiedTradeSettlementHeaderMonetarySummation');
//  _writeOptionalAmount(Writer, 'ram:LineTotalAmount', Descriptor.LineTotalAmount);                                  // Summe der Nettobeträge aller Rechnungspositionen
//  _writeOptionalAmount(Writer, 'ram:ChargeTotalAmount', Descriptor.ChargeTotalAmount);                              // Summe der Zuschläge auf Dokumentenebene
//  _writeOptionalAmount(Writer, 'ram:AllowanceTotalAmount', Descriptor.AllowanceTotalAmount);                        // Summe der Abschläge auf Dokumentenebene
//
//  if (Descriptor.Profile = TZUGFeRDProfile.Extended) then
//  begin
//    // there shall be no currency for tax basis total amount, see
//    // https://github.com/stephanstapel/ZUGFeRD-csharp/issues/56#issuecomment-655525467
//    _writeOptionalAmount(Writer, 'ram:TaxBasisTotalAmount', Descriptor.TaxBasisAmount, 2,false);   // Rechnungsgesamtbetrag ohne Umsatzsteuer
//  end
//  else
//  begin
//    _writeOptionalAmount(Writer, 'ram:TaxBasisTotalAmount', Descriptor.TaxBasisAmount);   // Rechnungsgesamtbetrag ohne Umsatzsteuer
//  end;
//  _writeOptionalAmount(Writer, 'ram:TaxTotalAmount', Descriptor.TaxTotalAmount, 2, true);               // Gesamtbetrag der Rechnungsumsatzsteuer, Steuergesamtbetrag in Buchungswährung
//  _writeOptionalAmount(Writer, 'ram:RoundingAmount', Descriptor.RoundingAmount, 2, false,[TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);  // RoundingAmount  //Rundungsbetrag
//  _writeOptionalAmount(Writer, 'ram:GrandTotalAmount', Descriptor.GrandTotalAmount);                                // Rechnungsgesamtbetrag einschließlich Umsatzsteuer
//  _writeOptionalAmount(Writer, 'ram:TotalPrepaidAmount', Descriptor.TotalPrepaidAmount);                            // Vorauszahlungsbetrag
//  _writeOptionalAmount(Writer, 'ram:DuePayableAmount', Descriptor.DuePayableAmount);                                // Fälliger Zahlungsbetrag
//  Writer.WriteEndElement(); // !ram:SpecifiedTradeSettlementMonetarySummation
//  //#endregion
//
//  //#region InvoiceReferencedDocument
//  if (Descriptor.InvoiceReferencedDocument <> nil) then
//  begin
//    Writer.WriteStartElement('ram:InvoiceReferencedDocument');
//    Writer.WriteOptionalElementString('ram:IssuerAssignedID', Descriptor.InvoiceReferencedDocument.ID);
//    if (Descriptor.InvoiceReferencedDocument.IssueDateTime.HasValue) then
//    begin
//      Writer.WriteStartElement('ram:FormattedIssueDateTime');
//      _writeElementWithAttribute(Writer, 'qdt:DateTimeString', 'format', '102', _formatDate(Descriptor.InvoiceReferencedDocument.IssueDateTime.Value));
//      Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
//    end;
//    Writer.WriteEndElement(); // !ram:InvoiceReferencedDocument
//  end;
//  //#endregion
//
//  //#region ReceivableSpecifiedTradeAccountingAccount
//  if (Descriptor.ReceivableSpecifiedTradeAccountingAccounts <> nil) then
//  if (Descriptor.ReceivableSpecifiedTradeAccountingAccounts.Count > 0) then
//  begin
//    if (descriptor.Profile = TZUGFeRDProfile.XRechnung1) or (descriptor.Profile = TZUGFeRDProfile.XRechnung) then
//    begin
//      if (Descriptor.ReceivableSpecifiedTradeAccountingAccounts[0].TradeAccountID<>'') then
//      begin
//        Writer.WriteStartElement('ram:ReceivableSpecifiedTradeAccountingAccount');
//        begin
//          //BT-19
//          Writer.WriteStartElement('ram:ID');
//          Writer.WriteValue(Descriptor.ReceivableSpecifiedTradeAccountingAccounts[0].TradeAccountID);
//          Writer.WriteEndElement(); // !ram:ID
//        end;
//        Writer.WriteEndElement(); // !ram:ReceivableSpecifiedTradeAccountingAccount
//      end;
//    end
//    else
//    begin
//      for var RSTAA : TZUGFeRDReceivableSpecifiedTradeAccountingAccount in Descriptor.ReceivableSpecifiedTradeAccountingAccounts do
//      begin
//        Writer.WriteStartElement('ram:ReceivableSpecifiedTradeAccountingAccount', [TZUGFeRDProfile.BasicWL,TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);
//
//        //BT-19
//        Writer.WriteStartElement('ram:ID', [TZUGFeRDProfile.BasicWL,TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);
//        Writer.WriteValue(RSTAA.TradeAccountID);
//        Writer.WriteEndElement(); // !ram:ID
//
//        if (RSTAA.TradeAccountTypeCode <> TZUGFeRDAccountingAccountTypeCodes.Unknown) then
//        begin
//          Writer.WriteStartElement('ram:TypeCode', [TZUGFeRDProfile.Extended]);
//          Writer.WriteValue(Integer(RSTAA.TradeAccountTypeCode).ToString);
//          Writer.WriteEndElement(); // !ram:TypeCode
//        end;
//
//        Writer.WriteEndElement(); // !ram:ReceivableSpecifiedTradeAccountingAccount
//      end;
//    end;
//  end;
//  //#endregion
//  Writer.WriteEndElement(); // !ram:ApplicableHeaderTradeSettlement
//
//  //#endregion
//
//  Writer.WriteEndElement(); // !ram:SpecifiedSupplyChainTradeTransaction
//  //#endregion

  Writer.WriteEndElement(); // !ram:Invoice
  Writer.WriteEndDocument();
  Writer.Flush();
  Writer.Free;

  _stream.Seek(streamPosition, soFromBeginning);
end;

function TZUGFeRDInvoiceDescriptor22UBLWriter.Validate(
  _descriptor: TZUGFeRDInvoiceDescriptor; _throwExceptions: Boolean): Boolean;
begin
  raise TZUGFeRDNotImplementedException.Create('TZUGFeRDInvoiceDescriptor22UBLWriter.Validate');
end;

procedure TZUGFeRDInvoiceDescriptor22UBLWriter._writeApplicableProductCharacteristics(
  _writer: TZUGFeRDProfileAwareXmlTextWriter;
  productCharacteristics: TObjectList<TZUGFeRDApplicableProductCharacteristic>);
begin

//            if (productCharacteristics.Count > 0)
//            {
//                foreach (var characteristic in productCharacteristics)
//                {
//                    writer.WriteStartElement("cac:AdditionalItemProperty");
//                    writer.WriteElementString("cbc:Name", characteristic.Description);
//                    writer.WriteElementString("cbc:Value", characteristic.Value);
//                    writer.WriteEndElement();
//                }
//            }
end;

//procedure TZUGFeRDInvoiceDescriptor22UBLWriter._writeElementWithAttribute(
//  _writer : TZUGFeRDProfileAwareXmlTextWriter;
//  tagName : String; attributeName : String;
//  attributeValue : String; nodeValue: String;
//  profile: TZUGFeRDProfiles);
//begin
//  _writer.WriteStartElement(tagName,profile);
//  _writer.WriteAttributeString(attributeName, attributeValue);
//  _writer.WriteValue(nodeValue);
//  _writer.WriteEndElement(); // !tagName
//end;
//
//procedure TZUGFeRDInvoiceDescriptor22UBLWriter._writeOptionalTaxes(
//  _writer : TZUGFeRDProfileAwareXmlTextWriter);
//begin
//  for var tax : TZUGFeRDTax in Descriptor.Taxes do
//  begin
//    _writer.WriteStartElement('ram:ApplicableTradeTax');
//
//    _writer.WriteStartElement('ram:CalculatedAmount');
//    _writer.WriteValue(_formatDecimal(tax.TaxAmount));
//    _writer.WriteEndElement(); // !CalculatedAmount
//
//    _writer.WriteElementString('ram:TypeCode', TZUGFeRDTaxTypesExtensions.EnumToString(tax.TypeCode));
//    _writer.WriteOptionalElementString('ram:ExemptionReason', tax.ExemptionReason);
//
//    _writer.WriteStartElement('ram:BasisAmount');
//    _writer.WriteValue(_formatDecimal(tax.BasisAmount));
//    _writer.WriteEndElement(); // !BasisAmount
//
//    if (tax.AllowanceChargeBasisAmount <> 0.0) then
//    begin
//      _writer.WriteStartElement('ram:AllowanceChargeBasisAmount');
//      _writer.WriteValue(_formatDecimal(tax.AllowanceChargeBasisAmount));
//      _writer.WriteEndElement(); // !AllowanceChargeBasisAmount
//    end;
//
//    if (tax.CategoryCode <> TZUGFeRDTaxCategoryCodes.Unknown) then
//    begin
//      _writer.WriteElementString('ram:CategoryCode', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(tax.CategoryCode));
//    end;
//    if (tax.ExemptionReasonCode <> TZUGFeRDTaxExemptionReasonCodes.Unknown) then
//    begin
//      _writer.WriteElementString('ram:ExemptionReasonCode', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(tax.ExemptionReasonCode));
//    end;
//    _writer.WriteElementString('ram:RateApplicablePercent', _formatDecimal(tax.Percent));
//    _writer.WriteEndElement(); // !ApplicableTradeTax
//  end;
//end;

procedure TZUGFeRDInvoiceDescriptor22UBLWriter._writeNotes(
  _writer : TZUGFeRDProfileAwareXmlTextWriter;
  notes : TObjectList<TZUGFeRDNote>);
begin

//            if (notes.Count > 0)
//            {
//                foreach (Note note in notes)
//                {
//                    writer.WriteElementString("cbc:Note", note.Content);
//                }
//            }

//  if notes.Count = 0 then
//    exit;
//
//  for var note : TZUGFeRDNote in notes do
//  begin
//    _writer.WriteStartElement('ram:IncludedNote');
//    if (note.ContentCode <> TZUGFeRDContentCodes.Unknown) then
//      _writer.WriteElementString('ram:ContentCode', TZUGFeRDContentCodesExtensions.EnumToString(note.ContentCode));
//    _writer.WriteElementString('ram:Content', note.Content);
//    if (note.SubjectCode <> TZUGFeRDSubjectCodes.Unknown) then
//      _writer.WriteElementString('ram:SubjectCode', TZUGFeRDSubjectCodesExtensions.EnumToString(note.SubjectCode));
//    _writer.WriteEndElement();
//  end;
end;

//procedure TZUGFeRDInvoiceDescriptor22UBLWriter._writeOptionalLegalOrganization(
//  _writer : TZUGFeRDProfileAwareXmlTextWriter;
//  legalOrganizationTag : String;
//  legalOrganization : TZUGFeRDLegalOrganization;
//  partyType : TZUGFeRDPartyTypes = TZUGFeRDPartyTypes.Unknown);
//begin
//  if (legalOrganization = nil) then
//    exit;
//
//  case partyType of
//    TZUGFeRDPartyTypes.Unknown: ;
//    TZUGFeRDPartyTypes.SellerTradeParty: ; // all profiles
//    TZUGFeRDPartyTypes.BuyerTradeParty: ; // all profiles
//    TZUGFeRDPartyTypes.ShipToTradeParty:
//      if (Descriptor.Profile <> TZUGFeRDProfile.Extended) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung1) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung) then
//           exit; // extended, XRechnung1, XRechnung profile only
//    TZUGFeRDPartyTypes.UltimateShipToTradeParty:
//      if (Descriptor.Profile <> TZUGFeRDProfile.Extended) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung1) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung) then
//           exit; // extended, XRechnung1, XRechnung profile only
//    TZUGFeRDPartyTypes.ShipFromTradeParty:
//      if (Descriptor.Profile <> TZUGFeRDProfile.Extended) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung1) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung) then
//           exit; // extended, XRechnung1, XRechnung profile only
//    TZUGFeRDPartyTypes.InvoiceeTradeParty:
//      if (Descriptor.Profile <> TZUGFeRDProfile.Extended) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung1) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung) then
//           exit; // extended, XRechnung1, XRechnung profile only
//    TZUGFeRDPartyTypes.PayeeTradeParty: ; // all profiles
//    TZUGFeRDPartyTypes.SalesAgentTradeParty:
//      if (Descriptor.Profile <> TZUGFeRDProfile.Extended) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung1) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung) then
//           exit; // extended, XRechnung1, XRechnung profile only
//    TZUGFeRDPartyTypes.BuyerTaxRepresentativeTradeParty:
//      if (Descriptor.Profile <> TZUGFeRDProfile.Extended) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung1) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung) then
//           exit; // extended, XRechnung1, XRechnung profile only
//    TZUGFeRDPartyTypes.ProductEndUserTradeParty:
//      if (Descriptor.Profile <> TZUGFeRDProfile.Extended) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung1) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung) then
//           exit; // extended, XRechnung1, XRechnung profile only
//    TZUGFeRDPartyTypes.BuyerAgentTradeParty:
//      if (Descriptor.Profile <> TZUGFeRDProfile.Extended) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung1) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung) then
//           exit; // extended, XRechnung1, XRechnung profile only
//    TZUGFeRDPartyTypes.InvoicerTradeParty:
//      if (Descriptor.Profile <> TZUGFeRDProfile.Extended) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung1) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung) then
//           exit; // extended, XRechnung1, XRechnung profile only
//    TZUGFeRDPartyTypes.PayerTradeParty:
//      if (Descriptor.Profile <> TZUGFeRDProfile.Extended) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung1) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung) then
//           exit; // extended, XRechnung1, XRechnung profile only
//    else exit;
//  end;
//
//  writer.WriteStartElement(legalOrganizationTag, [Descriptor.Profile]);
//  if (legalOrganization.ID <> nil) then
//  begin
//    if (legalOrganization.ID.ID <> '') and (TZUGFeRDGlobalIDSchemeIdentifiersExtensions.EnumToString(legalOrganization.ID.SchemeID)<> '') then
//    begin
//      writer.WriteStartElement('ram:ID');
//      writer.WriteAttributeString('schemeID', TZUGFeRDGlobalIDSchemeIdentifiersExtensions.EnumToString(legalOrganization.ID.SchemeID));
//      writer.WriteValue(legalOrganization.ID.ID);
//      writer.WriteEndElement();
//    end
//    else
//    begin
//      writer.WriteElementString('ram:ID', legalOrganization.ID.ID);
//    end;
//    // filter according to https://github.com/stephanstapel/ZUGFeRD-csharp/pull/221
//    if (((partyType = TZUGFeRDPartyTypes.SellerTradeParty) and (Descriptor.Profile <> TZUGFeRDProfile.Minimum)) or
//        ((partyType = TZUGFeRDPartyTypes.PayeeTradeParty) and (Descriptor.Profile <> TZUGFeRDProfile.Minimum)) or
//        ((partyType = TZUGFeRDPartyTypes.BuyerTradeParty) and (Descriptor.Profile <> TZUGFeRDProfile.Minimum)) or
//         (Descriptor.Profile = TZUGFeRDProfile.Extended) // remaining party types
//       ) then
//    begin
//      writer.WriteOptionalElementString('ram:TradingBusinessName', legalOrganization.TradingBusinessName, [Descriptor.Profile]);
//    end;
//  end;
//  writer.WriteEndElement();
//end;
//
procedure TZUGFeRDInvoiceDescriptor22UBLWriter._writeOptionalParty(
  _writer: TZUGFeRDProfileAwareXmlTextWriter;
  partyType : TZUGFeRDPartyTypes;
  party : TZUGFeRDParty;
  contact : TZUGFeRDContact = nil;
  electronicAddress : TZUGFeRDElectronicAddress = nil;
  taxRegistrations : TObjectList<TZUGFeRDTaxRegistration> = nil);
begin
//            // filter according to https://github.com/stephanstapel/ZUGFeRD-csharp/pull/221
//            switch (partyType)
//            {
//                case PartyTypes.Unknown:
//                    return;
//                case PartyTypes.SellerTradeParty:
//                    break;
//                case PartyTypes.BuyerTradeParty:
//                    break;
//                //case PartyTypes.ShipToTradeParty:
//                //    if ((this.Descriptor.Profile != Profile.Extended) && (this.Descriptor.Profile != Profile.XRechnung1) && (this.Descriptor.Profile != Profile.XRechnung)) { return; } // extended, XRechnung1, XRechnung profile only
//                //    break;
//                //case PartyTypes.UltimateShipToTradeParty:
//                //    if ((this.Descriptor.Profile != Profile.Extended) && (this.Descriptor.Profile != Profile.XRechnung1) && (this.Descriptor.Profile != Profile.XRechnung)) { return; } // extended, XRechnung1, XRechnung profile only
//                //    break;
//                //case PartyTypes.ShipFromTradeParty:
//                //    if ((this.Descriptor.Profile != Profile.Extended) && (this.Descriptor.Profile != Profile.XRechnung1) && (this.Descriptor.Profile != Profile.XRechnung)) { return; } // extended, XRechnung1, XRechnung profile only
//                //    break;
//                //case PartyTypes.InvoiceeTradeParty:
//                //    if ((this.Descriptor.Profile != Profile.Extended) && (this.Descriptor.Profile != Profile.XRechnung1) && (this.Descriptor.Profile != Profile.XRechnung)) { return; } // extended, XRechnung1, XRechnung profile only
//                //    break;
//                //case PartyTypes.PayeeTradeParty:
//                //    if (this.Descriptor.Profile == Profile.Minimum) { return; } // party is written for all profiles but minimum
//                //    break;
//                //case PartyTypes.SalesAgentTradeParty:
//                //    if ((this.Descriptor.Profile != Profile.Extended) && (this.Descriptor.Profile != Profile.XRechnung1) && (this.Descriptor.Profile != Profile.XRechnung)) { return; } // extended, XRechnung1, XRechnung profile only
//                //    break;
//                //case PartyTypes.BuyerTaxRepresentativeTradeParty:
//                //    if ((this.Descriptor.Profile != Profile.Extended) && (this.Descriptor.Profile != Profile.XRechnung1) && (this.Descriptor.Profile != Profile.XRechnung)) { return; } // extended, XRechnung1, XRechnung profile only
//                //    break;
//                //case PartyTypes.ProductEndUserTradeParty:
//                //    if ((this.Descriptor.Profile != Profile.Extended) && (this.Descriptor.Profile != Profile.XRechnung1) && (this.Descriptor.Profile != Profile.XRechnung)) { return; } // extended, XRechnung1, XRechnung profile only
//                //    break;
//                //case PartyTypes.BuyerAgentTradeParty:
//                //    if ((this.Descriptor.Profile != Profile.Extended) && (this.Descriptor.Profile != Profile.XRechnung1) && (this.Descriptor.Profile != Profile.XRechnung)) { return; } // extended, XRechnung1, XRechnung profile only
//                //    break;
//                //case PartyTypes.InvoicerTradeParty:
//                //    if ((this.Descriptor.Profile != Profile.Extended) && (this.Descriptor.Profile != Profile.XRechnung1) && (this.Descriptor.Profile != Profile.XRechnung)) { return; } // extended, XRechnung1, XRechnung profile only
//                //    break;
//                //case PartyTypes.PayerTradeParty:
//                //    if ((this.Descriptor.Profile != Profile.Extended) && (this.Descriptor.Profile != Profile.XRechnung1) && (this.Descriptor.Profile != Profile.XRechnung)) { return; } // extended, XRechnung1, XRechnung profile only
//                //    break;
//                default:
//                    return;
//            }
//
//            if (party != null)
//            {
//                switch (partyType)
//                {
//                    case PartyTypes.SellerTradeParty:
//                        writer.WriteStartElement("cac:AccountingSupplierParty", this.Descriptor.Profile);
//                        break;
//                    case PartyTypes.BuyerTradeParty:
//                        writer.WriteStartElement("cac:AccountingCustomerParty", this.Descriptor.Profile);
//                        break;
//                        //case PartyTypes.ShipToTradeParty:
//                        //    writer.WriteStartElement("ram:ShipToTradeParty", this.Descriptor.Profile);
//                        //    break;
//                        //case PartyTypes.UltimateShipToTradeParty:
//                        //    writer.WriteStartElement("ram:UltimateShipToTradeParty", this.Descriptor.Profile);
//                        //    break;
//                        //case PartyTypes.ShipFromTradeParty:
//                        //    writer.WriteStartElement("ram:ShipFromTradeParty", this.Descriptor.Profile);
//                        //    break;
//                        //case PartyTypes.InvoiceeTradeParty:
//                        //    writer.WriteStartElement("ram:InvoiceeTradeParty", this.Descriptor.Profile);
//                        //    break;
//                        //case PartyTypes.PayeeTradeParty:
//                        //    writer.WriteStartElement("ram:PayeeTradeParty", this.Descriptor.Profile);
//                        //    break;
//                        //case PartyTypes.PayerTradeParty:
//                        //    writer.WriteStartElement("ram:PayerTradeParty", this.Descriptor.Profile);
//                        //    break;
//                        //case PartyTypes.SalesAgentTradeParty:
//                        //    writer.WriteStartElement("ram:SalesAgentTradeParty", this.Descriptor.Profile);
//                        //    break;
//                        //case PartyTypes.BuyerTaxRepresentativeTradeParty:
//                        //    writer.WriteStartElement("ram:BuyerTaxRepresentativeTradeParty", this.Descriptor.Profile);
//                        //    break;
//                        //case PartyTypes.ProductEndUserTradeParty:
//                        //    writer.WriteStartElement("ram:ProductEndUserTradeParty", this.Descriptor.Profile);
//                        //    break;
//                        //case PartyTypes.BuyerAgentTradeParty:
//                        //    writer.WriteStartElement("ram:BuyerAgentTradeParty", this.Descriptor.Profile);
//                        //    break;
//                        //case PartyTypes.InvoicerTradeParty:
//                        //    writer.WriteStartElement("ram:InvoicerTradeParty", this.Descriptor.Profile);
//                        //    break;
//                }
//
//                writer.WriteStartElement("cac:Party", this.Descriptor.Profile);
//
//                if (ElectronicAddress != null)
//                {
//                    writer.WriteStartElement("cbc:EndpointID");
//                    writer.WriteAttributeString("schemeID", ElectronicAddress.ElectronicAddressSchemeID.ToString());
//                    writer.WriteValue(ElectronicAddress.Address);
//                    writer.WriteEndElement();
//                }
//
//                writer.WriteStartElement("cac:PartyIdentification");
//                if (this.Descriptor.PaymentMeans.SEPAMandateReference != null)
//                {
//                    writer.WriteStartElement("cbc:ID");
//                    writer.WriteAttributeString("schemeID", "SEPA");
//                    writer.WriteValue(this.Descriptor.PaymentMeans.SEPACreditorIdentifier);
//                    writer.WriteEndElement();//!ID
//                }
//
//                writer.WriteEndElement();//!PartyIdentification
//
//                writer.WriteStartElement("cac:PostalAddress");
//
//                Writer.WriteElementString("cbc:StreetName", party.Street);
//                Writer.WriteOptionalElementString("cbc:AdditionalStreetName", party.AddressLine3);
//                Writer.WriteElementString("cbc:CityName", party.City);
//                Writer.WriteElementString("cbc:PostalZone", party.Postcode);
//
//                writer.WriteStartElement("cac:Country");
//                Writer.WriteElementString("cbc:IdentificationCode", party.Country.ToString());
//                writer.WriteEndElement(); //!Country
//
//                writer.WriteEndElement(); //!PostalTradeAddress
//
//
//                foreach (var tax in taxRegistrations)
//                {
//                    writer.WriteStartElement("cac:PartyTaxScheme");
//
//                    Writer.WriteElementString("cbc:CompanyID", tax.No);
//
//                    writer.WriteStartElement("cac:TaxScheme");
//
//                    Writer.WriteElementString("cbc:ID", tax.SchemeID.ToString());
//
//                    writer.WriteEndElement(); //!TaxScheme
//
//                    writer.WriteEndElement(); //!PartyTaxScheme
//                }
//
//
//                writer.WriteStartElement("cac:PartyLegalEntity");
//
//                writer.WriteElementString("cbc:RegistrationName", party.Name);
//
//                writer.WriteEndElement(); //!PartyLegalEntity
//
//                if (contact != null)
//                {
//                    writer.WriteStartElement("cac:Contact");
//
//                    writer.WriteElementString("cbc:Name", contact.Name);
//                    writer.WriteElementString("cbc:Telephone", contact.PhoneNo);
//                    writer.WriteElementString("cbc:ElectronicMail", contact.EmailAddress);
//
//                    writer.WriteEndElement();
//                }
//
//
//
//                writer.WriteEndElement(); //!Party
//
//
//
//                //if (party.ID != null)
//                //{
//                //    if (!String.IsNullOrWhiteSpace(party.ID.ID) && (party.ID.SchemeID != GlobalIDSchemeIdentifiers.Unknown))
//                //    {
//                //        writer.WriteStartElement("ram:ID");
//                //        writer.WriteAttributeString("schemeID", party.ID.SchemeID.EnumToString());
//                //        writer.WriteValue(party.ID.ID);
//                //        writer.WriteEndElement();
//                //    }
//
//                //    writer.WriteOptionalElementString("ram:ID", party.ID.ID);
//                //}
//
//                //if ((party.GlobalID != null) && !String.IsNullOrWhiteSpace(party.GlobalID.ID) && (party.GlobalID.SchemeID != GlobalIDSchemeIdentifiers.Unknown))
//                //{
//                //    writer.WriteStartElement("ram:GlobalID");
//                //    writer.WriteAttributeString("schemeID", party.GlobalID.SchemeID.EnumToString());
//                //    writer.WriteValue(party.GlobalID.ID);
//                //    writer.WriteEndElement();
//                //}
//                //_writeOptionalLegalOrganization(writer, "ram:SpecifiedLegalOrganization", party.SpecifiedLegalOrganization, partyType);
//                //_writeOptionalContact(writer, "ram:DefinedTradeContact", contact, Profile.Extended | Profile.XRechnung1 | Profile.XRechnung);
//
//                //writer.WriteOptionalElementString("ram:PostcodeCode", party.Postcode); // buyer: BT-53
//                //writer.WriteOptionalElementString("ram:LineOne", string.IsNullOrWhiteSpace(party.ContactName) ? party.Street : party.ContactName); // buyer: BT-50
//                //if (!string.IsNullOrWhiteSpace(party.ContactName)) { writer.WriteOptionalElementString("ram:LineTwo", party.Street); } // buyer: BT-51
//
//                //writer.WriteOptionalElementString("ram:LineThree", party.AddressLine3); // buyer: BT-163
//                //writer.WriteOptionalElementString("ram:CityName", party.City); // buyer: BT-52
//                //writer.WriteElementString("ram:CountryID", party.Country.EnumToString()); // buyer: BT-55
//                //writer.WriteOptionalElementString("ram:CountrySubDivisionName", party.CountrySubdivisionName); // BT-79
//                //writer.WriteEndElement(); // !PostalTradeAddress
//
//                //if (ElectronicAddress != null)
//                //{
//                //    if (!String.IsNullOrWhiteSpace(ElectronicAddress.Address))
//                //    {
//                //        writer.WriteStartElement("ram:URIUniversalCommunication");
//                //        writer.WriteStartElement("ram:URIID");
//                //        writer.WriteAttributeString("schemeID", ElectronicAddress.ElectronicAddressSchemeID.EnumToString());
//                //        writer.WriteValue(ElectronicAddress.Address);
//                //        writer.WriteEndElement();
//                //        writer.WriteEndElement();
//                //    }
//                //}
//
//                //if (taxRegistrations != null)
//                //{
//                //    // for seller: BT-31
//                //    // for buyer : BT-48
//                //    foreach (TaxRegistration _reg in taxRegistrations)
//                //    {
//                //        if (!String.IsNullOrWhiteSpace(_reg.No))
//                //        {
//                //            writer.WriteStartElement("ram:SpecifiedTaxRegistration");
//                //            writer.WriteStartElement("ram:ID");
//                //            writer.WriteAttributeString("schemeID", _reg.SchemeID.EnumToString());
//                //            writer.WriteValue(_reg.No);
//                //            writer.WriteEndElement();
//                //            writer.WriteEndElement();
//                //        }
//                //    }
//                //}
//                //writer.WriteEndElement(); // !*TradeParty
//                Writer.WriteEndElement(); //Invoice


//  // filter according to https://github.com/stephanstapel/ZUGFeRD-csharp/pull/221
//
//  case partyType of
//    TZUGFeRDPartyTypes.Unknown: exit;
//    TZUGFeRDPartyTypes.SellerTradeParty: ;
//    TZUGFeRDPartyTypes.BuyerTradeParty: ;
//    TZUGFeRDPartyTypes.ShipToTradeParty:
//      if (Descriptor.Profile <> TZUGFeRDProfile.Extended) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung1) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung) then
//           exit; // extended, XRechnung1, XRechnung profile only
//    TZUGFeRDPartyTypes.UltimateShipToTradeParty:
//      if (Descriptor.Profile <> TZUGFeRDProfile.Extended) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung1) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung) then
//           exit; // extended, XRechnung1, XRechnung profile only
//    TZUGFeRDPartyTypes.ShipFromTradeParty:
//      if (Descriptor.Profile <> TZUGFeRDProfile.Extended) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung1) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung) then
//           exit; // extended, XRechnung1, XRechnung profile only
//    TZUGFeRDPartyTypes.InvoiceeTradeParty:
//      if (Descriptor.Profile <> TZUGFeRDProfile.Extended) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung1) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung) then
//           exit; // extended, XRechnung1, XRechnung profile only
//    TZUGFeRDPartyTypes.PayeeTradeParty:
//      if (Descriptor.Profile = TZUGFeRDProfile.Minimum) then
//            exit; // party is written for all profiles but minimum
//    TZUGFeRDPartyTypes.SalesAgentTradeParty:
//      if (Descriptor.Profile <> TZUGFeRDProfile.Extended) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung1) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung) then
//           exit; // extended, XRechnung1, XRechnung profile only
//    TZUGFeRDPartyTypes.BuyerTaxRepresentativeTradeParty:
//      if (Descriptor.Profile <> TZUGFeRDProfile.Extended) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung1) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung) then
//           exit; // extended, XRechnung1, XRechnung profile only
//    TZUGFeRDPartyTypes.ProductEndUserTradeParty:
//      if (Descriptor.Profile <> TZUGFeRDProfile.Extended) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung1) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung) then
//           exit; // extended, XRechnung1, XRechnung profile only
//    TZUGFeRDPartyTypes.BuyerAgentTradeParty:
//      if (Descriptor.Profile <> TZUGFeRDProfile.Extended) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung1) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung) then
//           exit; // extended, XRechnung1, XRechnung profile only
//    TZUGFeRDPartyTypes.InvoicerTradeParty:
//      if (Descriptor.Profile <> TZUGFeRDProfile.Extended) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung1) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung) then
//           exit; // extended, XRechnung1, XRechnung profile only
//    TZUGFeRDPartyTypes.PayerTradeParty:
//      if (Descriptor.Profile <> TZUGFeRDProfile.Extended) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung1) and
//         (Descriptor.Profile <> TZUGFeRDProfile.XRechnung) then
//           exit; // extended, XRechnung1, XRechnung profile only
//    else exit;
//  end;
//
//  if (party = nil) then
//    exit;
//
//  case partyType of
//    TZUGFeRDPartyTypes.SellerTradeParty:
//      writer.WriteStartElement('ram:SellerTradeParty', [Descriptor.Profile]);
//    TZUGFeRDPartyTypes.BuyerTradeParty:
//      writer.WriteStartElement('ram:BuyerTradeParty', [Descriptor.Profile]);
//    TZUGFeRDPartyTypes.ShipToTradeParty:
//      writer.WriteStartElement('ram:ShipToTradeParty', [Descriptor.Profile]);
//    TZUGFeRDPartyTypes.UltimateShipToTradeParty:
//      writer.WriteStartElement('ram:UltimateShipToTradeParty', [Descriptor.Profile]);
//    TZUGFeRDPartyTypes.ShipFromTradeParty:
//      writer.WriteStartElement('ram:ShipFromTradeParty', [Descriptor.Profile]);
//    TZUGFeRDPartyTypes.InvoiceeTradeParty:
//      writer.WriteStartElement('ram:InvoiceeTradeParty', [Descriptor.Profile]);
//    TZUGFeRDPartyTypes.PayeeTradeParty:
//      writer.WriteStartElement('ram:PayeeTradeParty', [Descriptor.Profile]);
//    TZUGFeRDPartyTypes.SalesAgentTradeParty:
//      writer.WriteStartElement('ram:SalesAgentTradeParty', [Descriptor.Profile]);
//    TZUGFeRDPartyTypes.BuyerTaxRepresentativeTradeParty:
//      writer.WriteStartElement('ram:BuyerTaxRepresentativeTradeParty', [Descriptor.Profile]);
//    TZUGFeRDPartyTypes.ProductEndUserTradeParty:
//      writer.WriteStartElement('ram:ProductEndUserTradeParty', [Descriptor.Profile]);
//    TZUGFeRDPartyTypes.BuyerAgentTradeParty:
//      writer.WriteStartElement('ram:BuyerAgentTradeParty', [Descriptor.Profile]);
//    TZUGFeRDPartyTypes.InvoicerTradeParty:
//      writer.WriteStartElement('ram:InvoicerTradeParty', [Descriptor.Profile]);
//    TZUGFeRDPartyTypes.PayerTradeParty:
//      writer.WriteStartElement('ram:PayerTradeParty', [Descriptor.Profile]);
//  end;
//
//  if party.ID <> nil then
//  begin
//    if ((party.ID.ID <> '') and (party.ID.SchemeID <> TZUGFeRDGlobalIDSchemeIdentifiers.Unknown)) then
//    begin
//      writer.WriteStartElement('ram:ID');
//      writer.WriteAttributeString('schemeID', TZUGFeRDGlobalIDSchemeIdentifiersExtensions.EnumToString(party.ID.SchemeID));
//      writer.WriteValue(party.ID.ID);
//      writer.WriteEndElement();
//    end;
//
//    writer.WriteOptionalElementString('ram:ID', party.ID.ID);
//  end;
//
//  if (party.GlobalID <> nil) then
//  if ((party.GlobalID.ID <> '') and (party.GlobalID.SchemeID <> TZUGFeRDGlobalIDSchemeIdentifiers.Unknown)) then
//  begin
//    writer.WriteStartElement('ram:GlobalID');
//    writer.WriteAttributeString('schemeID', TZUGFeRDGlobalIDSchemeIdentifiersExtensions.EnumToString(party.GlobalID.SchemeID));
//    writer.WriteValue(party.GlobalID.ID);
//    writer.WriteEndElement();
//  end;
//
//  writer.WriteOptionalElementString('ram:Name', party.Name);
//  writer.WriteOptionalElementString('ram:Description', Party.Description, [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung1, TZUGFeRDProfile.XRechnung]);
//
//  _writeOptionalLegalOrganization(writer, 'ram:SpecifiedLegalOrganization', party.SpecifiedLegalOrganization, partyType);
//  _writeOptionalContact(writer, 'ram:DefinedTradeContact', contact, [TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);
//
//  writer.WriteStartElement('ram:PostalTradeAddress');
//  writer.WriteOptionalElementString('ram:PostcodeCode', party.Postcode); //buyer: BT-53
//  writer.WriteOptionalElementString('ram:LineOne', ifthen(party.ContactName='',party.Street,party.ContactName)); //buyer: BT-50
//  if (party.ContactName<>'')then
//  begin
//      writer.WriteOptionalElementString('ram:LineTwo', party.Street); //buyer: BT-51
//  end;
//
//  writer.WriteOptionalElementString('ram:LineThree', party.AddressLine3); //buyer: BT-163
//  writer.WriteOptionalElementString('ram:CityName', party.City); //buyer: BT-52
//  writer.WriteElementString('ram:CountryID', TZUGFeRDCountryCodesExtensions.EnumToString(party.Country)); //buyer: BT-55
//  writer.WriteOptionalElementString('ram:CountrySubDivisionName', party.CountrySubdivisionName); // BT-79
//  writer.WriteEndElement(); // !PostalTradeAddress
//
//  if (electronicAddress <> nil) then
//  begin
//    if (ElectronicAddress.Address<>'') then
//    begin
//      writer.WriteStartElement('ram:URIUniversalCommunication');
//      writer.WriteStartElement('ram:URIID');
//      writer.WriteAttributeString('schemeID', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(electronicAddress.ElectronicAddressSchemeID));
//      writer.WriteValue(electronicAddress.Address);
//      writer.WriteEndElement();
//      writer.WriteEndElement();
//    end;
//  end;
//
//  if (taxRegistrations <> nil) then
//  begin
//      // for seller: BT-31
//      // for buyer : BT-48
//    for var _reg : TZUGFeRDTaxRegistration in taxRegistrations do
//    begin
//      if (_reg.No <> '') then
//      begin
//        writer.WriteStartElement('ram:SpecifiedTaxRegistration');
//        writer.WriteStartElement('ram:ID');
//        writer.WriteAttributeString('schemeID', TZUGFeRDTaxRegistrationSchemeIDExtensions.EnumToString(_reg.SchemeID));
//        writer.WriteValue(_reg.No);
//        writer.WriteEndElement();
//        writer.WriteEndElement();
//      end;
//    end;
//  end;
//  writer.WriteEndElement(); // !*TradeParty
end;
//
//procedure TZUGFeRDInvoiceDescriptor22UBLWriter._writeOptionalContact(
//  _writer: TZUGFeRDProfileAwareXmlTextWriter; contactTag : String;
//  contact : TZUGFeRDContact; profile : TZUGFeRDProfiles);
//begin
//  if contact = nil then
//    exit;
//
//  _writer.WriteStartElement(contactTag,profile);
//
//  _writer.WriteOptionalElementString('ram:PersonName', contact.Name);
//  _writer.WriteOptionalElementString('ram:DepartmentName', contact.OrgUnit);
//
//  if (contact.PhoneNo <> '') then
//  begin
//    _writer.WriteStartElement('ram:TelephoneUniversalCommunication');
//    _writer.WriteElementString('ram:CompleteNumber', contact.PhoneNo);
//    _writer.WriteEndElement();
//  end;
//
//  if (contact.FaxNo <> '') then
//  begin
//    _writer.WriteStartElement('ram:FaxUniversalCommunication',ALL_PROFILES - [TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);
//    _writer.WriteElementString('ram:CompleteNumber', contact.FaxNo);
//    _writer.WriteEndElement();
//  end;
//
//  if (contact.EmailAddress <> '') then
//  begin
//    _writer.WriteStartElement('ram:EmailURIUniversalCommunication');
//    _writer.WriteElementString('ram:URIID', contact.EmailAddress);
//    _writer.WriteEndElement();
//  end;
//
//  _writer.WriteEndElement();
//end;
//
//function TZUGFeRDInvoiceDescriptor22UBLWriter._translateTaxCategoryCode(
//  taxCategoryCode : TZUGFeRDTaxCategoryCodes) : String;
//begin
//  Result := '';
//  case taxCategoryCode of
//    TZUGFeRDTaxCategoryCodes.A :;
//    TZUGFeRDTaxCategoryCodes.AA :;
//    TZUGFeRDTaxCategoryCodes.AB :;
//    TZUGFeRDTaxCategoryCodes.AC :;
//    TZUGFeRDTaxCategoryCodes.AD :;
//    TZUGFeRDTaxCategoryCodes.AE : Result := 'Umkehrung der Steuerschuldnerschaft';
//    TZUGFeRDTaxCategoryCodes.B: ;
//    TZUGFeRDTaxCategoryCodes.C: ;
//    TZUGFeRDTaxCategoryCodes.E: Result := 'steuerbefreit';
//    TZUGFeRDTaxCategoryCodes.G: Result := 'freier Ausfuhrartikel, Steuer nicht erhoben';
//    TZUGFeRDTaxCategoryCodes.H: ;
//    TZUGFeRDTaxCategoryCodes.O: Result := 'Dienstleistungen außerhalb des Steueranwendungsbereichs';
//    TZUGFeRDTaxCategoryCodes.S: Result := 'Normalsatz';
//    TZUGFeRDTaxCategoryCodes.Z: Result := 'nach dem Nullsatz zu versteuernde Waren';
//    TZUGFeRDTaxCategoryCodes.Unknown: ;
//    TZUGFeRDTaxCategoryCodes.D: ;
//    TZUGFeRDTaxCategoryCodes.F: ;
//    TZUGFeRDTaxCategoryCodes.I: ;
//    TZUGFeRDTaxCategoryCodes.J: ;
//    TZUGFeRDTaxCategoryCodes.K: Result := 'Kein Ausweis der Umsatzsteuer bei innergemeinschaftlichen Lieferungen';
//    TZUGFeRDTaxCategoryCodes.L: Result := 'IGIC (Kanarische Inseln)';
//    TZUGFeRDTaxCategoryCodes.M: Result := 'IPSI (Ceuta/Melilla)';
//  end;
//end;
//
//function TZUGFeRDInvoiceDescriptor22UBLWriter._translateInvoiceType(type_ : TZUGFeRDInvoiceType) : String;
//begin
//  case type_ of
//    SelfBilledInvoice,
//    Invoice: Result := 'RECHNUNG';
//    SelfBilledCreditNote,
//    CreditNote: Result := 'GUTSCHRIFT';
//    DebitNote: Result := 'BELASTUNGSANZEIGE';
//    DebitnoteRelatedToFinancialAdjustments: Result := 'WERTBELASTUNG';
//    PartialInvoice: Result := 'TEILRECHNUNG';
//    PrepaymentInvoice: Result := 'VORAUSZAHLUNGSRECHNUNG';
//    InvoiceInformation: Result := 'KEINERECHNUNG';
//    Correction,
//    CorrectionOld: Result := 'KORREKTURRECHNUNG';
//    else Result := '';
//  end;
//end;

function TZUGFeRDInvoiceDescriptor22UBLWriter._encodeInvoiceType(type_ : TZUGFeRDInvoiceType) : Integer;
begin
Result:= 0; // avoid the warning
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

//  if (Integer(type_) > 1000) then
//    type_ := TZUGFeRDInvoiceType(Integer(type_)-1000);
//
//  case type_ of
//    TZUGFeRDInvoiceType.CorrectionOld: Result := Integer(TZUGFeRDInvoiceType.Correction);
//    else Result := Integer(type_);
//  end;
end;

//function TZUGFeRDInvoiceDescriptor22UBLWriter.Validate(
//  _descriptor: TZUGFeRDInvoiceDescriptor; _throwExceptions: Boolean): Boolean;
//begin
//  Result := false;
//
//  //TODO in C# enthalten, aber eigentlich falsch, deswegen auskommentiert
//  //if (descriptor.TZUGFeRDProfile = TZUGFeRDProfile.BasicWL) then
//  //if (throwExceptions) then
//  //  raise TZUGFeRDUnsupportedException.Create('Invalid TZUGFeRDProfile used for ZUGFeRD 2.0 invoice.')
//  //else
//  //  exit;
//
//  if (_descriptor.Profile <> TZUGFeRDProfile.Extended) then // check tax types, only extended TZUGFeRDProfile allows tax types other than vat
//  begin
//    for var l : TZUGFeRDTradeLineItem in _descriptor.TradeLineItems do
//    if not ((l.TaxType = TZUGFeRDTaxTypes.Unknown) or
//      (l.TaxType = TZUGFeRDTaxTypes.VAT)) then
//    begin
//      if (_throwExceptions) then
//        raise TZUGFeRDUnsupportedException.Create('Tax types other than VAT only possible with extended TZUGFeRDProfile.')
//      else
//        exit;
//    end;
//  end;
//
//  if (_descriptor.Profile in [TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]) then
//  begin
//    if (descriptor.Seller <> nil) then
//    begin
//      if (descriptor.SellerContact = nil) then
//      begin
//          if (_throwExceptions) then
//            raise TZUGFeRDMissingDataException.Create('Seller contact (BG-6) required when seller is set (BR-DE-2).')
//          else
//            exit;
//      end
//      else
//      begin
//          if (descriptor.SellerContact.EmailAddress = '') then
//          begin
//            if (_throwExceptions) then
//              raise TZUGFeRDMissingDataException.Create('Seller contact email address (BT-43) is required (BR-DE-7).')
//            else
//              exit;
//          end;
//          if (descriptor.SellerContact.PhoneNo = '') then
//          begin
//            if (_throwExceptions) then
//                raise TZUGFeRDMissingDataException.Create('Seller contact phone no (BT-42) is required (BR-DE-6).')
//            else
//              exit;
//          end;
//          if (descriptor.SellerContact.Name = '') and
//             (descriptor.SellerContact.OrgUnit = '') then
//          begin
//            if (_throwExceptions) then
//              raise TZUGFeRDMissingDataException.Create('Seller contact point (name or org unit) no (BT-41) is required (BR-DE-5).')
//            else
//              exit;
//          end;
//      end;
//    end;
//  end;
//
//  // BR-DE-17
//  if not ((_descriptor.Type_ = TZUGFeRDInvoiceType.PartialInvoice) or
//          (_descriptor.Type_ = TZUGFeRDInvoiceType.Invoice) or
//          (_descriptor.Type_ = TZUGFeRDInvoiceType.Correction) or
//          (_descriptor.Type_ = TZUGFeRDInvoiceType.SelfBilledInvoice) or
//          (_descriptor.Type_ = TZUGFeRDInvoiceType.CreditNote) or
//          (_descriptor.Type_ = TZUGFeRDInvoiceType.PartialConstructionInvoice) or
//          (_descriptor.Type_ = TZUGFeRDInvoiceType.PartialFinalConstructionInvoice) or
//          (_descriptor.Type_ = TZUGFeRDInvoiceType.FinalConstructionInvoice)) then
//  begin
//    if (_throwExceptions) then
//      raise TZUGFeRDUnsupportedException.Create('Invoice type (BT-3) does not match requirements of BR-DE-17')
//    else
//      exit;
//  end;
//
//  Result := true;
//end;
//
procedure TZUGFeRDInvoiceDescriptor22UBLWriter._writeOptionalAmount(
  _writer: TZUGFeRDProfileAwareXmlTextWriter; tagName: string;
  value: ZUGFeRDNullable<Currency>;
  numDecimals: Integer; forceCurrency: Boolean; profile : TZUGFeRDProfiles);
begin
//            if (value.HasValue)
//            {
//                writer.WriteStartElement(tagName, profile);
//                if (forceCurrency)
//                {
//                    writer.WriteAttributeString("currencyID", this.Descriptor.Currency.EnumToString());
//                }
//                writer.WriteValue(_formatDecimal(value.Value, numDecimals));
//                writer.WriteEndElement(); // !tagName
//            }

//  if (value.HasValue) then // && (value.Value != decimal.MinValue))
//  begin
//    _writer.WriteStartElement(tagName,profile);
//    if forceCurrency then
//      _writer.WriteAttributeString('currencyID', TZUGFeRDCurrencyCodesExtensions.EnumToString(Descriptor.Currency));
//    _writer.WriteValue(_formatDecimal(value.Value, numDecimals));
//    _writer.WriteEndElement; // !tagName
//  end;
end;

end.
