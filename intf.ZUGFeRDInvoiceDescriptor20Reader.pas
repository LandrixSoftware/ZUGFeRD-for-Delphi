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

unit intf.ZUGFeRDInvoiceDescriptor20Reader;

interface

uses
  System.SysUtils, System.Classes, System.DateUtils
  ,System.NetEncoding
  ,Xml.XMLDoc, Xml.xmldom, Xml.XMLIntf,intf.ZUGFeRDMSXML2_TLB
  ,intf.ZUGFeRDXmlHelper
  ,intf.ZUGFeRDInvoiceDescriptorReader
  ,intf.ZUGFeRDTradeLineItem
  ,intf.ZUGFeRDParty
  ,intf.ZUGFeRDAdditionalReferencedDocument
  ,intf.ZUGFeRDInvoiceDescriptor
  ,intf.ZUGFeRDExceptions
  ,intf.ZUGFeRDProfile,intf.ZUGFeRDInvoiceTypes
  ,intf.ZUGFeRDSubjectCodes
  ,intf.ZUGFeRDGlobalID,intf.ZUGFeRDGlobalIDSchemeIdentifiers
  ,intf.ZUGFeRDCountryCodes
  ,intf.ZUGFeRDTaxRegistrationSchemeID
  ,intf.ZUGFeRDContact
  ,intf.ZUGFeRDAdditionalReferencedDocumentTypeCodes
  ,intf.ZUGFeRDReferenceTypeCodes
  ,intf.ZUGFeRDDeliveryNoteReferencedDocument
  ,intf.ZUGFeRDCurrencyCodes
  ,intf.ZUGFeRDPaymentMeans,intf.ZUGFeRDPaymentMeansTypeCodes
  ,intf.ZUGFeRDFinancialCard
  ,intf.ZUGFeRDBankAccount
  ,intf.ZUGFeRDTaxTypes,intf.ZUGFeRDTaxCategoryCodes
  ,intf.ZUGFeRDTaxExemptionReasonCodes
  ,intf.ZUGFeRDPaymentTerms
  ,intf.ZUGFeRDSellerOrderReferencedDocument
  ,intf.ZUGFeRDInvoiceReferencedDocument
  ,intf.ZUGFeRDQuantityCodes
  ,intf.ZUGFeRDApplicableProductCharacteristic
  ,intf.ZUGFeRDAssociatedDocument
  ,intf.ZUGFeRDNote
  ,intf.ZUGFeRDContentCodes
  ,intf.ZUGFeRDBuyerOrderReferencedDocument
  ,intf.ZUGFeRDContractReferencedDocument
  ,intf.ZUGFeRDSpecialServiceDescriptionCodes
  ,intf.ZUGFeRDAllowanceOrChargeIdentificationCodes
  ;

type
  TZUGFeRDInvoiceDescriptor20Reader = class(TZUGFeRDInvoiceDescriptorReader)
  private
    function GetValidURIs : TArray<string>;
    function _parseTradeLineItem(tradeLineItem : IXmlDomNode {nsmgr: XmlNamespaceManager := nil; }) : TZUGFeRDTradeLineItem;
    function _nodeAsParty(basenode: IXmlDomNode; const xpath: string) : TZUGFeRDParty;
    function _getAdditionalReferencedDocument(a_oXmlNode : IXmlDomNode {nsmgr: XmlNamespaceManager := nil; }) : TZUGFeRDAdditionalReferencedDocument;
  public
    function IsReadableByThisReaderVersion(stream: TStream): Boolean; override;
    function IsReadableByThisReaderVersion(xmldocument: IXMLDocument): Boolean; override;
    /// <summary>
    /// Parses the ZUGFeRD invoice from the given stream.
    ///
    /// Make sure that the stream is open, otherwise an IllegalStreamException exception is thrown.
    /// Important: the stream will not be closed by this function.
    /// </summary>
    /// <param name="stream"></param>
    /// <returns>The parsed ZUGFeRD invoice</returns>
    function Load(stream: TStream): TZUGFeRDInvoiceDescriptor; override;

    function Load(xmldocument : IXMLDocument): TZUGFeRDInvoiceDescriptor; override;
  end;

implementation

{ TZUGFeRDInvoiceDescriptor20Reader }

function TZUGFeRDInvoiceDescriptor20Reader.GetValidURIs : TArray<string>;
begin
  Result := TArray<string>.Create(
    'urn:cen.eu:EN16931:2017#conformant#urn:zugferd.de:2p0:extended', // Profil EXTENDED
    'urn:cen.eu:EN16931:2017', // Profil EN 16931 (COMFORT)" +
    'urn:cen.eu:EN16931:2017#compliant#urn:zugferd.de:2p0:basic', // Profil BASIC
    'urn:zugferd.de:2p0:basicwl', // Profil BASIC WL
    'urn:zugferd.de:2p0:minimum' // Profil MINIMUM
  );
end;

function TZUGFeRDInvoiceDescriptor20Reader.IsReadableByThisReaderVersion(
  stream: TStream): Boolean;
begin
  Result := IsReadableByThisReaderVersion(stream, GetValidURIs);
end;

function TZUGFeRDInvoiceDescriptor20Reader.IsReadableByThisReaderVersion(
  xmldocument: IXMLDocument): Boolean;
begin
  Result := IsReadableByThisReaderVersion(xmldocument, GetValidURIs);
end;

function TZUGFeRDInvoiceDescriptor20Reader.Load(stream: TStream): TZUGFeRDInvoiceDescriptor;
var
  xml : IXMLDocument;
begin
  if Stream = nil then
    raise TZUGFeRDIllegalStreamException.Create('Cannot read from stream');

  xml := NewXMLDocument;
  try
    xml.LoadFromStream(stream,TXMLEncodingType.xetUTF_8);
    xml.Active := True;
    Result := Load(xml);
  finally
    xml := nil;
  end;
end;

function TZUGFeRDInvoiceDescriptor20Reader.Load(xmldocument : IXMLDocument): TZUGFeRDInvoiceDescriptor;
var
  doc : IXMLDOMDocument2;
  node : IXMLDOMNode;
  nodes : IXMLDOMNodeList;
  i : Integer;
begin
  doc := TZUGFeRDXmlHelper.PrepareDocumentForXPathQuerys(xmldocument);

  //XmlNamespaceManager nsmgr = _GenerateNamespaceManagerFromNode(doc.DocumentElement);

  Result := TZUGFeRDInvoiceDescriptor.Create;

  Result.IsTest := _nodeAsBool(doc.documentElement,'//*[local-name()="ExchangedDocumentContext"]/ram:TestIndicator');
  Result.BusinessProcess := _nodeAsString(doc.DocumentElement, '//*[local-name()="BusinessProcessSpecifiedDocumentContextParameter"]/ram:ID');//, nsmgr),
  Result.Profile := TZUGFeRDProfileExtensions.FromString(_nodeAsString(doc.DocumentElement, '//ram:GuidelineSpecifiedDocumentContextParameter/ram:ID'));//, nsmgr)),
  Result.Name := _nodeAsString(doc.DocumentElement, '//*[local-name()="ExchangedDocument"]/ram:Name');//, nsmgr)),
  Result.Type_ := TZUGFeRDInvoiceTypeExtensions.FromString(_nodeAsString(doc.DocumentElement, '//*[local-name()="ExchangedDocument"]/ram:TypeCode'));//, nsmgr)),
  Result.InvoiceNo := _nodeAsString(doc.DocumentElement, '//*[local-name()="ExchangedDocument"]/ram:ID');//, nsmgr),
  Result.InvoiceDate := _nodeAsDateTime(doc.DocumentElement, '//*[local-name()="ExchangedDocument"]/ram:IssueDateTime/udt:DateTimeString');//", nsmgr)

  nodes := doc.selectNodes('//*[local-name()="ExchangedDocument"]/ram:IncludedNote');
  for i := 0 to nodes.length-1 do
  begin
    var content : String := _nodeAsString(nodes[i], './/ram:Content');
    var _subjectCode : String := _nodeAsString(nodes[i], './/ram:SubjectCode');
    var subjectCode : TZUGFeRDSubjectCodes := TZUGFeRDSubjectCodesExtensions.FromString(_subjectCode);
    var contentCode : TZUGFeRDContentCodes := TZUGFeRDContentCodesExtensions.FromString(_nodeAsString(nodes[i], './/ram:ContentCode'));
    Result.AddNote(content, subjectCode, contentCode);
  end;

  Result.ReferenceOrderNo := _nodeAsString(doc, '//ram:ApplicableHeaderTradeAgreement/ram:BuyerReference');

  Result.Seller := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:SellerTradeParty');

  nodes := doc.selectNodes('//ram:ApplicableHeaderTradeAgreement/ram:SellerTradeParty/ram:SpecifiedTaxRegistration');
  for i := 0 to nodes.length-1 do
  begin
    var id : String := _nodeAsString(nodes[i], './/ram:ID');
    var schemeID : String := _nodeAsString(nodes[i], './/ram:ID/@schemeID');
    Result.AddSellerTaxRegistration(id, TZUGFeRDTaxRegistrationSchemeIDExtensions.FromString(schemeID));
  end;

  if (doc.selectSingleNode('//ram:SellerTradeParty/ram:DefinedTradeContact') <> nil) then
  begin
    Result.SellerContact := TZUGFeRDContact.Create;
    Result.SellerContact.Name := _nodeAsString(doc.DocumentElement, '//ram:SellerTradeParty/ram:DefinedTradeContact/ram:PersonName');
    Result.SellerContact.OrgUnit := _nodeAsString(doc.DocumentElement, '//ram:SellerTradeParty/ram:DefinedTradeContact/ram:DepartmentName');
    Result.SellerContact.PhoneNo := _nodeAsString(doc.DocumentElement, '//ram:SellerTradeParty/ram:DefinedTradeContact/ram:TelephoneUniversalCommunication/ram:CompleteNumber');
    Result.SellerContact.FaxNo := _nodeAsString(doc.DocumentElement, '//ram:SellerTradeParty/ram:DefinedTradeContact/ram:FaxUniversalCommunication/ram:CompleteNumber');
    Result.SellerContact.EmailAddress := _nodeAsString(doc.DocumentElement, '//ram:SellerTradeParty/ram:DefinedTradeContact/ram:EmailURIUniversalCommunication/ram:URIID');
  end;

  Result.Buyer := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:BuyerTradeParty');

  nodes := doc.selectNodes('//ram:ApplicableHeaderTradeAgreement/ram:BuyerTradeParty/ram:SpecifiedTaxRegistration');
  for i := 0 to nodes.length-1 do
  begin
    var id : String := _nodeAsString(nodes[i], './/ram:ID');
    var schemeID : String := _nodeAsString(nodes[i], './/ram:ID/@schemeID');
    Result.AddBuyerTaxRegistration(id, TZUGFeRDTaxRegistrationSchemeIDExtensions.FromString(schemeID));
  end;

  if (doc.SelectSingleNode('//ram:BuyerTradeParty/ram:DefinedTradeContact') <> nil) then
  begin
    Result.BuyerContact := TZUGFeRDContact.Create;
    Result.SetBuyerContact(
      _nodeAsString(doc.DocumentElement, '//ram:BuyerTradeParty/ram:DefinedTradeContact/ram:PersonName'),
      _nodeAsString(doc.DocumentElement, '//ram:BuyerTradeParty/ram:DefinedTradeContact/ram:DepartmentName'),
      _nodeAsString(doc.DocumentElement, '//ram:BuyerTradeParty/ram:DefinedTradeContact/ram:EmailURIUniversalCommunication/ram:URIID'),
      _nodeAsString(doc.DocumentElement, '//ram:BuyerTradeParty/ram:DefinedTradeContact/ram:TelephoneUniversalCommunication/ram:CompleteNumber'),
      _nodeAsString(doc.DocumentElement, '//ram:BuyerTradeParty/ram:DefinedTradeContact/ram:FaxUniversalCommunication/ram:CompleteNumber')
    );
  end;

  //Get all referenced and embedded documents (BG-24)
  nodes := doc.SelectNodes('.//ram:ApplicableHeaderTradeAgreement/ram:AdditionalReferencedDocument');
  for i := 0 to nodes.length-1 do
  begin
    Result.AdditionalReferencedDocuments.Add(_getAdditionalReferencedDocument(nodes[i]));
  end;

  Result.ShipTo := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:ShipToTradeParty');
  Result.ShipFrom := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:ShipFromTradeParty');
  Result.ActualDeliveryDate:= _nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:ActualDeliverySupplyChainEvent/ram:OccurrenceDateTime/udt:DateTimeString');

  var _deliveryNoteNo : String := _nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssuerAssignedID');
  var _deliveryNoteDate : TDateTime := _nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssueDateTime/udt:DateTimeString');

  if (_deliveryNoteDate < 100) then
  begin
    _deliveryNoteDate := _nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssueDateTime');
  end;

  if ((_deliveryNoteDate > 100) or (_deliveryNoteNo <> '')) then
  begin
    Result.DeliveryNoteReferencedDocument := TZUGFeRDDeliveryNoteReferencedDocument.Create;
    Result.SetDeliveryNoteReferenceDocument(_deliveryNoteNo,_deliveryNoteDate);
  end;

  Result.Invoicee := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:InvoiceeTradeParty');
  Result.Payee := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:PayeeTradeParty');

  Result.PaymentReference := _nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:PaymentReference');
  Result.Currency :=  TZUGFeRDCurrencyCodesExtensions.FromString(_nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:InvoiceCurrencyCode'));
  Result.SellerReferenceNo := _nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:InvoiceIssuerReference');

  // TODO: Multiple SpecifiedTradeSettlementPaymentMeans can exist for each account/institution (with different SEPA?)
  var _tempPaymentMeans : TZUGFeRDPaymentMeans := TZUGFeRDPaymentMeans.Create;
  _tempPaymentMeans.TypeCode := TZUGFeRDPaymentMeansTypeCodesExtensions.FromString(_nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:TypeCode'));
  _tempPaymentMeans.Information := _nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:Information');
  _tempPaymentMeans.SEPACreditorIdentifier := _nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:CreditorReferenceID');

  var financialCardId : String := _nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:ApplicableTradeSettlementFinancialCard/ram:ID');
  var financialCardCardholderName : String := _nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:ApplicableTradeSettlementFinancialCard/ram:CardholderName');

  if ((financialCardId <> '') or (financialCardCardholderName <> '')) then
  begin
    _tempPaymentMeans.FinancialCard := TZUGFeRDFinancialCard.Create;
    _tempPaymentMeans.FinancialCard.Id := financialCardId;
    _tempPaymentMeans.FinancialCard.CardholderName := financialCardCardholderName;
  end;

  Result.PaymentMeans := _tempPaymentMeans;

  //TODO udt:DateTimeString
  Result.BillingPeriodStart := _nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:BillingSpecifiedPeriod/ram:StartDateTime');
  Result.BillingPeriodEnd := _nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:BillingSpecifiedPeriod/ram:EndDateTime');

  var creditorFinancialAccountNodes : IXMLDOMNodeList := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:PayeePartyCreditorFinancialAccount');
  var creditorFinancialInstitutions : IXMLDOMNodeList := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:PayeeSpecifiedCreditorFinancialInstitution');

  if (creditorFinancialAccountNodes.length = creditorFinancialInstitutions.length) then
  for i := 0 to creditorFinancialAccountNodes.length-1 do
  begin
    var _account : TZUGFeRDBankAccount := TZUGFeRDBankAccount.Create;
    _account.ID := _nodeAsString(creditorFinancialAccountNodes[i], './/ram:ProprietaryID');
    _account.IBAN := _nodeAsString(creditorFinancialAccountNodes[i], './/ram:IBANID');
    _account.Name := _nodeAsString(creditorFinancialAccountNodes[i], './/ram:AccountName');
    _account.BIC := _nodeAsString(creditorFinancialInstitutions[i], './/ram:BICID');
    _account.Bankleitzahl := _nodeAsString(creditorFinancialInstitutions[i], './/ram:GermanBankleitzahlID');
    _account.BankName := _nodeAsString(creditorFinancialInstitutions[i], './/ram:Name');
    Result.CreditorBankAccounts.Add(_account);
  end;

  var specifiedTradeSettlementPaymentMeansNodes : IXMLDOMNodeList := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans');
  for i := 0 to specifiedTradeSettlementPaymentMeansNodes.length-1 do
  begin
      var payerPartyDebtorFinancialAccountNode : IXMLDOMNode := specifiedTradeSettlementPaymentMeansNodes[i].selectSingleNode('ram:PayerPartyDebtorFinancialAccount');

      if (payerPartyDebtorFinancialAccountNode = nil) then
        continue;

      var _account : TZUGFeRDBankAccount := TZUGFeRDBankAccount.Create;
      _account.ID := _nodeAsString(payerPartyDebtorFinancialAccountNode, './/ram:ProprietaryID');
      _account.IBAN := _nodeAsString(payerPartyDebtorFinancialAccountNode, './/ram:IBANID');
      _account.Bankleitzahl := _nodeAsString(payerPartyDebtorFinancialAccountNode, './/ram:GermanBankleitzahlID');
      _account.BankName := _nodeAsString(payerPartyDebtorFinancialAccountNode, './/ram:Name');

      var payerSpecifiedDebtorFinancialInstitutionNode : IXMLDOMNode := specifiedTradeSettlementPaymentMeansNodes[i].SelectSingleNode('ram:PayerSpecifiedDebtorFinancialInstitution');
      if (payerSpecifiedDebtorFinancialInstitutionNode <> nil) then
          _account.BIC := _nodeAsString(payerPartyDebtorFinancialAccountNode, './/ram:BICID');

      Result.DebitorBankAccounts.Add(_account);
  end;

  //XmlNodeList debitorFinancialAccountNodes := doc.SelectNodes("//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:PayerPartyDebtorFinancialAccount');
  //XmlNodeList debitorFinancialInstitutions := doc.SelectNodes("//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:PayerSpecifiedDebtorFinancialInstitution');

  //if (debitorFinancialAccountNodes.Count == debitorFinancialInstitutions.Count)
  //{
  //    for (int i := 0; i < debitorFinancialAccountNodes.Count; i++)
  //    {
  //        BankAccount _account := new BankAccount()
  //        {
  //            ID := _nodeAsString(debitorFinancialAccountNodes[0], ".//ram:ProprietaryID'),
  //            IBAN := _nodeAsString(debitorFinancialAccountNodes[0], ".//ram:IBANID'),
  //            BIC := _nodeAsString(debitorFinancialInstitutions[0], ".//ram:BICID'),
  //            Bankleitzahl := _nodeAsString(debitorFinancialInstitutions[0], ".//ram:GermanBankleitzahlID'),
  //            BankName := _nodeAsString(debitorFinancialInstitutions[0], ".//ram:Name'),
  //        };

  //        Result.DebitorBankAccounts.Add(_account);
  //    } // !for(i)
  //

  nodes := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:ApplicableTradeTax');
  for i := 0 to nodes.length-1 do
  begin
    Result.AddApplicableTradeTax(_nodeAsDecimal(nodes[i], './/ram:BasisAmount', 0),
                                 _nodeAsDecimal(nodes[i], './/ram:RateApplicablePercent', 0),
                                 TZUGFeRDTaxTypesExtensions.FromString(_nodeAsString(nodes[i], './/ram:TypeCode')),
                                 TZUGFeRDTaxCategoryCodesExtensions.FromString(_nodeAsString(nodes[i], './/ram:CategoryCode')),
                                 0,
                                 TZUGFeRDTaxExemptionReasonCodesExtensions.FromString(_nodeAsString(nodes[i], './/ram:ExemptionReasonCode')),
                                 _nodeAsString(nodes[i], './/ram:ExemptionReason'));
  end;

  nodes := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeAllowanceCharge');
  for i := 0 to nodes.length-1 do
  begin
    Result.AddTradeAllowanceCharge(not _nodeAsBool(nodes[i], './/ram:ChargeIndicator'), // wichtig: das not (!) beachten
                                   _nodeAsDecimal(nodes[i], './/ram:BasisAmount', 0),
                                   Result.Currency,
                                   _nodeAsDecimal(nodes[i], './/ram:ActualAmount', 0),
                                   _nodeAsString(nodes[i], './/ram:Reason'),
                                   TZUGFeRDSpecialServiceDescriptionCodesExtensions.FromString(_nodeAsString(nodes[i], './ram:ReasonCode')),
                                   TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString(_nodeAsString(nodes[i], './ram:ReasonCode')),
                                   TZUGFeRDTaxTypesExtensions.FromString(_nodeAsString(nodes[i], './/ram:CategoryTradeTax/ram:TypeCode')),
                                   TZUGFeRDTaxCategoryCodesExtensions.FromString(_nodeAsString(nodes[i], './/ram:CategoryTradeTax/ram:CategoryCode')),
                                   _nodeAsDecimal(nodes[i], './/ram:CategoryTradeTax/ram:RateApplicablePercent', 0));
  end;

  nodes := doc.SelectNodes('//ram:SpecifiedLogisticsServiceCharge');
  for i := 0 to nodes.length-1 do
  begin
    Result.AddLogisticsServiceCharge(_nodeAsDecimal(nodes[i], './/ram:AppliedAmount', 0),
                                     _nodeAsString(nodes[i], './/ram:Description'),
                                     TZUGFeRDTaxTypesExtensions.FromString(_nodeAsString(nodes[i], './/ram:AppliedTradeTax/ram:TypeCode')),
                                     TZUGFeRDTaxCategoryCodesExtensions.FromString(_nodeAsString(nodes[i], './/ram:AppliedTradeTax/ram:CategoryCode')),
                                     _nodeAsDecimal(nodes[i], './/ram:AppliedTradeTax/ram:RateApplicablePercent', 0));
  end;

  nodes := doc.SelectNodes('//ram:SpecifiedTradePaymentTerms');
  for i := 0 to nodes.length-1 do
  begin
    var paymentTerm : TZUGFeRDPaymentTerms := TZUGFeRDPaymentTerms.Create;
    paymentTerm.Description := _nodeAsString(nodes[i], './/ram:Description');
    paymentTerm.DueDate:= _nodeAsDateTime(nodes[i], './/ram:DueDateDateTime');
    paymentTerm.DirectDebitMandateID := _nodeAsString(nodes[i], './/ram:DirectDebitMandateID');
    //TODO paymentTerm.PartialPaymentAmount
    //TODO paymentTerm.ApplicableTradePaymentPenaltyTerms
    paymentTerm.ApplicableTradePaymentDiscountTerms.BasisPeriodMeasure := _nodeAsDouble(nodes[i], './/ram:ApplicableTradePaymentDiscountTerms/ram:BasisPeriodMeasure');
    paymentTerm.ApplicableTradePaymentDiscountTerms.UnitCode := TZUGFeRDQuantityCodesExtensions.FromString(_nodeAsString(nodes[i], './/ram:ApplicableTradePaymentDiscountTerms/ram:BasisPeriodMeasure/@unitCode'));
    paymentTerm.ApplicableTradePaymentDiscountTerms.BasisAmount := _nodeAsDecimal(nodes[i], './/ram:ApplicableTradePaymentDiscountTerms/ram:BasisAmount');
    paymentTerm.ApplicableTradePaymentDiscountTerms.CalculationPercent := _nodeAsDouble(nodes[i], './/ram:ApplicableTradePaymentDiscountTerms/ram:CalculationPercent');
    Result.PaymentTermsList.Add(paymentTerm);
  end;

  Result.LineTotalAmount:= _nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:LineTotalAmount', 0);
  Result.ChargeTotalAmount:= _nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:ChargeTotalAmount', 0);
  Result.AllowanceTotalAmount:= _nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:AllowanceTotalAmount', 0);
  Result.TaxBasisAmount:= _nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:TaxBasisTotalAmount',0);
  Result.TaxTotalAmount:= _nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:TaxTotalAmount', 0);
  Result.GrandTotalAmount:= _nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:GrandTotalAmount', 0);
  Result.RoundingAmount:= _nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:RoundingAmount', 0);
  Result.TotalPrepaidAmount:= _nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:TotalPrepaidAmount', 0);
  Result.DuePayableAmount:= _nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:DuePayableAmount', 0);

  if TZUGFeRDXmlHelper.FindNode(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:InvoiceReferencedDocument') then
  begin
    Result.InvoiceReferencedDocument := TZUGFeRDInvoiceReferencedDocument.Create;
    Result.InvoiceReferencedDocument.ID := _nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:InvoiceReferencedDocument/ram:IssuerAssignedID');
    Result.InvoiceReferencedDocument.IssueDateTime:= _nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:InvoiceReferencedDocument/ram:FormattedIssueDateTime');
  end;

  Result.OrderDate:= _nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:BuyerOrderReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString');
  Result.OrderNo := _nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:BuyerOrderReferencedDocument/ram:IssuerAssignedID');

  nodes := doc.SelectNodes('//ram:IncludedSupplyChainTradeLineItem');
  for i := 0 to nodes.length-1 do
    Result.TradeLineItems.Add(_parseTradeLineItem(nodes[i]));

  // Read SellerOrderReferencedDocument
  node := doc.SelectSingleNode('//ram:ApplicableHeaderTradeAgreement/ram:SellerOrderReferencedDocument');
  if node <> nil then
  begin
    Result.SellerOrderReferencedDocument := TZUGFeRDSellerOrderReferencedDocument.Create;
    Result.SellerOrderReferencedDocument.ID := _nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:SellerOrderReferencedDocument/ram:IssuerAssignedID');
    Result.SellerOrderReferencedDocument.IssueDateTime:= _nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:SellerOrderReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString');
  end;
end;

function TZUGFeRDInvoiceDescriptor20Reader._getAdditionalReferencedDocument(
  a_oXmlNode: IXmlDomNode): TZUGFeRDAdditionalReferencedDocument;
begin
  var strBase64BinaryData : String := _nodeAsString(a_oXmlNode, 'ram:AttachmentBinaryObject');
  Result := TZUGFeRDAdditionalReferencedDocument.Create(false);
  Result.ID := _nodeAsString(a_oXmlNode, 'ram:IssuerAssignedID');
  Result.TypeCode := TZUGFeRDAdditionalReferencedDocumentTypeCodeExtensions.FromString(_nodeAsString(a_oXmlNode, 'ram:TypeCode'));
  Result.Name := _nodeAsString(a_oXmlNode, 'ram:Name');
  Result.IssueDateTime:= _nodeAsDateTime(a_oXmlNode, 'ram:FormattedIssueDateTime/qdt:DateTimeString');
  if strBase64BinaryData <> '' then
  begin
    Result.AttachmentBinaryObject := TMemoryStream.Create;
    var strBase64BinaryDataBytes : TBytes := TNetEncoding.Base64String.DecodeStringToBytes(strBase64BinaryData);
    Result.AttachmentBinaryObject.Write(strBase64BinaryDataBytes,Length(strBase64BinaryDataBytes));
  end;
  Result.Filename := _nodeAsString(a_oXmlNode, 'ram:AttachmentBinaryObject/@filename');
  Result.ReferenceTypeCode := TZUGFeRDReferenceTypeCodesExtensions.FromString(_nodeAsString(a_oXmlNode, 'ram:ReferenceTypeCode'));
end;

function TZUGFeRDInvoiceDescriptor20Reader._nodeAsParty(basenode: IXmlDomNode;
  const xpath: string) : TZUGFeRDParty;
var
  node : IXmlDomNode;
  lineOne,lineTwo : String;
begin
  Result := nil;
  if (baseNode = nil) then
    exit;
  node := baseNode.SelectSingleNode(xpath);
  if (node = nil) then
    exit;
  Result := TZUGFeRDParty.Create;
  Result.ID.ID := _nodeAsString(node, 'ram:ID');
  Result.ID.SchemeID := TZUGFeRDGlobalIDSchemeIdentifiers.Unknown;
  Result.GlobalID.ID := _nodeAsString(node, 'ram:GlobalID');
  Result.GlobalID.SchemeID := TZUGFeRDGlobalIDSchemeIdentifiersExtensions.FromString(_nodeAsString(node, 'ram:GlobalID/@schemeID'));
  Result.Name := _nodeAsString(node, 'ram:Name');
  Result.Description := _nodeAsString(node, 'ram:Description'); // Seller only BT-33
  Result.Postcode := _nodeAsString(node, 'ram:PostalTradeAddress/ram:PostcodeCode');
  Result.City := _nodeAsString(node, 'ram:PostalTradeAddress/ram:CityName');
  Result.Country := TZUGFeRDCountryCodesExtensions.FromString(_nodeAsString(node, 'ram:PostalTradeAddress/ram:CountryID'));

  if (not lineTwo.IsEmpty) then
  begin
    Result.ContactName := lineOne;
    Result.Street := lineTwo;
  end else
  begin
    Result.Street := lineOne;
    Result.ContactName := '';
  end;
  Result.AddressLine3 := _nodeAsString(node, 'ram:PostalTradeAddress/ram:LineThree');
  Result.CountrySubdivisionName := _nodeAsString(node, 'ram:PostalTradeAddress/ram:CountrySubDivisionName');
end;

function TZUGFeRDInvoiceDescriptor20Reader._parseTradeLineItem(
  tradeLineItem: IXmlDomNode): TZUGFeRDTradeLineItem;
var
  nodes : IXMLDOMNodeList;
  i : Integer;
begin
  Result := nil;

  if (tradeLineItem = nil) then
    exit;

  Result := TZUGFeRDTradeLineItem.Create;

  Result.GlobalID.ID := _nodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:GlobalID');
  Result.GlobalID.SchemeID := TZUGFeRDGlobalIDSchemeIdentifiersExtensions.FromString(_nodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:GlobalID/@schemeID'));
  Result.SellerAssignedID := _nodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:SellerAssignedID');
  Result.BuyerAssignedID := _nodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:BuyerAssignedID');
  Result.Name := _nodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:Name');
  Result.Description := _nodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:Description');
  Result.UnitQuantity:= _nodeAsDecimal(tradeLineItem, './/ram:BasisQuantity', 0);
  Result.BilledQuantity := _nodeAsDecimal(tradeLineItem, './/ram:BilledQuantity', 0);
  Result.PackageQuantity := _nodeAsDecimal(tradeLineItem, './/ram:PackageQuantity', 0);
  Result.ChargeFreeQuantity := _nodeAsDecimal(tradeLineItem, './/ram:ChargeFreeQuantity', 0);
//  Result.LineTotalAmount.SetValue(_nodeAsDecimal(tradeLineItem, './/ram:LineTotalAmount', 0));
  Result.LineTotalAmount:= _nodeAsDecimal(tradeLineItem, './/ram:LineTotalAmount', 0);
  Result.TaxCategoryCode := TZUGFeRDTaxCategoryCodesExtensions.FromString(_nodeAsString(tradeLineItem, './/ram:ApplicableTradeTax/ram:CategoryCode'));
  Result.TaxType := TZUGFeRDTaxTypesExtensions.FromString(_nodeAsString(tradeLineItem, './/ram:ApplicableTradeTax/ram:TypeCode'));
  Result.TaxPercent := _nodeAsDecimal(tradeLineItem, './/ram:ApplicableTradeTax/ram:RateApplicablePercent', 0);
  Result.NetUnitPrice:= _nodeAsDecimal(tradeLineItem, './/ram:NetPriceProductTradePrice/ram:ChargeAmount', 0);
  Result.GrossUnitPrice:= _nodeAsDecimal(tradeLineItem, './/ram:GrossPriceProductTradePrice/ram:ChargeAmount', 0);
  Result.UnitCode := TZUGFeRDQuantityCodesExtensions.FromString(_nodeAsString(tradeLineItem, './/ram:BasisQuantity/@unitCode'));
  Result.PackageUnitCode := TZUGFeRDQuantityCodesExtensions.FromString(_nodeAsString(tradeLineItem, './/ram:PackageQuantity/@unitCode'));
  Result.ChargeFreeUnitCode := TZUGFeRDQuantityCodesExtensions.FromString(_nodeAsString(tradeLineItem, './/ram:ChargeFreeQuantity/@unitCode'));
  Result.BillingPeriodStart:= _nodeAsDateTime(tradeLineItem, './/ram:BillingSpecifiedPeriod/ram:StartDateTime/udt:DateTimeString');
  Result.BillingPeriodEnd:= _nodeAsDateTime(tradeLineItem, './/ram:BillingSpecifiedPeriod/ram:EndDateTime/udt:DateTimeString');

  nodes := tradeLineItem.SelectNodes('.//ram:SpecifiedTradeProduct/ram:ApplicableProductCharacteristic');
  for i := 0 to nodes.length-1 do
  begin
    var apcItem : TZUGFeRDApplicableProductCharacteristic := TZUGFeRDApplicableProductCharacteristic.Create;
    apcItem.Description := _nodeAsString(nodes[i], './/ram:Description');
    apcItem.Value := _nodeAsString(nodes[i], './/ram:Value');
    Result.ApplicableProductCharacteristics.Add(apcItem);
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:AssociatedDocumentLineDocument') <> nil) then
  begin
    Result.AssociatedDocument := TZUGFeRDAssociatedDocument.Create(_nodeAsString(tradeLineItem, './/ram:AssociatedDocumentLineDocument/ram:LineID'));

    nodes := tradeLineItem.SelectNodes('.//ram:AssociatedDocumentLineDocument/ram:IncludedNote');
    for i := 0 to nodes.length-1 do
    begin
      var noteItem : TZUGFeRDNote := TZUGFeRDNote.Create(
          _nodeAsString(nodes[i], './/ram:Content'),
          TZUGFeRDSubjectCodesExtensions.FromString(_nodeAsString(nodes[i], './/ram:SubjectCode')),
          TZUGFeRDContentCodesExtensions.FromString(_nodeAsString(nodes[i], './/ram:ContentCode'))
        );
      Result.AssociatedDocument.Notes.Add(noteItem);
    end;
  end;

  nodes := tradeLineItem.SelectNodes('.//ram:SpecifiedLineTradeAgreement/ram:GrossPriceProductTradePrice/ram:AppliedTradeAllowanceCharge');
  for i := 0 to nodes.length-1 do
  begin

    var chargeIndicator : Boolean := _nodeAsBool(nodes[i], './ram:ChargeIndicator/udt:Indicator');
    var basisAmount : Currency := _nodeAsDecimal(nodes[i], './ram:BasisAmount',0);
    var basisAmountCurrency : String := _nodeAsString(nodes[i], './ram:BasisAmount/@currencyID');
    var actualAmount : Currency := _nodeAsDecimal(nodes[i], './ram:ActualAmount',0);
    var actualAmountCurrency : String := _nodeAsString(nodes[i], './ram:ActualAmount/@currencyID');
    var reason : String := _nodeAsString(nodes[i], './ram:Reason');
    var reasonCodeCharge : TZUGFeRDSpecialServiceDescriptionCodes := TZUGFeRDSpecialServiceDescriptionCodes.Unknown;
    var reasonCodeAllowance : TZUGFeRDAllowanceOrChargeIdentificationCodes := TZUGFeRDAllowanceOrChargeIdentificationCodes.Unknown;
    if chargeIndicator then
      reasonCodeCharge := TZUGFeRDSpecialServiceDescriptionCodesExtensions.FromString(_nodeAsString(nodes[i], './ram:ReasonCode'))
    else
      reasonCodeAllowance := TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString(_nodeAsString(nodes[i], './ram:ReasonCode'));

    Result.AddTradeAllowanceCharge(not chargeIndicator, // wichtig: das not beachten
                                    TZUGFeRDCurrencyCodesExtensions.FromString(basisAmountCurrency),
                                    basisAmount,
                                    actualAmount,
                                    reason,
                                    reasonCodeCharge,
                                    reasonCodeAllowance);
  end;

  if (Result.UnitCode = TZUGFeRDQuantityCodes.Unknown) then
  begin
    // UnitCode alternativ aus BilledQuantity extrahieren
    Result.UnitCode := TZUGFeRDQuantityCodesExtensions.FromString(_nodeAsString(tradeLineItem, './/ram:BilledQuantity/@unitCode'));
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeAgreement/ram:BuyerOrderReferencedDocument/ram:IssuerAssignedID') <> nil) then
  begin
    Result.BuyerOrderReferencedDocument := TZUGFeRDBuyerOrderReferencedDocument.Create;
    Result.BuyerOrderReferencedDocument.ID := _nodeAsString(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:BuyerOrderReferencedDocument/ram:IssuerAssignedID');
    Result.BuyerOrderReferencedDocument.IssueDateTime:= _nodeAsDateTime(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:BuyerOrderReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString');
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssuerAssignedID') <> nil) then
  begin
    Result.DeliveryNoteReferencedDocument := TZUGFeRDDeliveryNoteReferencedDocument.Create;
    Result.DeliveryNoteReferencedDocument.ID := _nodeAsString(tradeLineItem, './/ram:SpecifiedLineTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssuerAssignedID');
    Result.DeliveryNoteReferencedDocument.IssueDateTime:= _nodeAsDateTime(tradeLineItem, './/ram:SpecifiedLineTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString');
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeDelivery/ram:ActualDeliverySupplyChainEvent/ram:OccurrenceDateTime') <> nil) then
  begin
    Result.ActualDeliveryDate:= _nodeAsDateTime(tradeLineItem, './/ram:SpecifiedLineTradeDelivery/ram:ActualDeliverySupplyChainEvent/ram:OccurrenceDateTime/udt:DateTimeString');
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeAgreement/ram:ContractReferencedDocument/ram:IssuerAssignedID') <> nil) then
  begin
    Result.ContractReferencedDocument := TZUGFeRDContractReferencedDocument.Create;
    Result.ContractReferencedDocument.ID := _nodeAsString(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:ContractReferencedDocument/ram:IssuerAssignedID');
    Result.ContractReferencedDocument.IssueDateTime:= _nodeAsDateTime(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:ContractReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString');
  end;

  //Get all referenced AND embedded documents
  nodes := tradeLineItem.SelectNodes('.//ram:SpecifiedLineTradeAgreement/ram:AdditionalReferencedDocument');
  for i := 0 to nodes.length-1 do
  begin
    Result.AdditionalReferencedDocuments.Add(_getAdditionalReferencedDocument(nodes[i]));
  end;
end;

end.
