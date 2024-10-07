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
  ,Xml.XMLDoc, Xml.xmldom, Xml.XMLIntf
  ,Xml.Win.msxmldom, Winapi.MSXMLIntf, Winapi.msxml
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
  ,intf.ZUGFeRDXmlUtils
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

  Result.IsTest := TZUGFeRDXmlUtils.NodeAsBool(doc.documentElement,'//*[local-name()="ExchangedDocumentContext"]/ram:TestIndicator',false);
  Result.BusinessProcess := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//*[local-name()="BusinessProcessSpecifiedDocumentContextParameter"]/ram:ID');//, nsmgr),
  Result.Profile := TZUGFeRDProfileExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:GuidelineSpecifiedDocumentContextParameter/ram:ID'));//, nsmgr)),
  Result.Name := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//*[local-name()="ExchangedDocument"]/ram:Name');//, nsmgr)),
  Result.Type_ := TZUGFeRDInvoiceTypeExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//*[local-name()="ExchangedDocument"]/ram:TypeCode'));//, nsmgr)),
  Result.InvoiceNo := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//*[local-name()="ExchangedDocument"]/ram:ID');//, nsmgr),
  Result.InvoiceDate := TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//*[local-name()="ExchangedDocument"]/ram:IssueDateTime/udt:DateTimeString');//", nsmgr)

  nodes := doc.selectNodes('//*[local-name()="ExchangedDocument"]/ram:IncludedNote');
  for i := 0 to nodes.length-1 do
  begin
    var content : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Content');
    var _subjectCode : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:SubjectCode');
    var subjectCode : TZUGFeRDSubjectCodes := TZUGFeRDSubjectCodesExtensions.FromString(_subjectCode);
    var contentCode : TZUGFeRDContentCodes := TZUGFeRDContentCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ContentCode'));
    Result.AddNote(content, subjectCode, contentCode);
  end;

  Result.ReferenceOrderNo := TZUGFeRDXmlUtils.NodeAsString(doc, '//ram:ApplicableHeaderTradeAgreement/ram:BuyerReference');

  Result.Seller := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:SellerTradeParty');

  nodes := doc.selectNodes('//ram:ApplicableHeaderTradeAgreement/ram:SellerTradeParty/ram:SpecifiedTaxRegistration');
  for i := 0 to nodes.length-1 do
  begin
    var id : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ID');
    var schemeID : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ID/@schemeID');
    Result.AddSellerTaxRegistration(id, TZUGFeRDTaxRegistrationSchemeIDExtensions.FromString(schemeID));
  end;

  if (doc.selectSingleNode('//ram:SellerTradeParty/ram:DefinedTradeContact') <> nil) then
  begin
    Result.SellerContact := TZUGFeRDContact.Create;
    Result.SellerContact.Name := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:SellerTradeParty/ram:DefinedTradeContact/ram:PersonName');
    Result.SellerContact.OrgUnit := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:SellerTradeParty/ram:DefinedTradeContact/ram:DepartmentName');
    Result.SellerContact.PhoneNo := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:SellerTradeParty/ram:DefinedTradeContact/ram:TelephoneUniversalCommunication/ram:CompleteNumber');
    Result.SellerContact.FaxNo := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:SellerTradeParty/ram:DefinedTradeContact/ram:FaxUniversalCommunication/ram:CompleteNumber');
    Result.SellerContact.EmailAddress := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:SellerTradeParty/ram:DefinedTradeContact/ram:EmailURIUniversalCommunication/ram:URIID');
  end;

  Result.Buyer := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:BuyerTradeParty');

  nodes := doc.selectNodes('//ram:ApplicableHeaderTradeAgreement/ram:BuyerTradeParty/ram:SpecifiedTaxRegistration');
  for i := 0 to nodes.length-1 do
  begin
    var id : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ID');
    var schemeID : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ID/@schemeID');
    Result.AddBuyerTaxRegistration(id, TZUGFeRDTaxRegistrationSchemeIDExtensions.FromString(schemeID));
  end;

  if (doc.SelectSingleNode('//ram:BuyerTradeParty/ram:DefinedTradeContact') <> nil) then
  begin
    Result.BuyerContact := TZUGFeRDContact.Create;
    Result.SetBuyerContact(
      TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:BuyerTradeParty/ram:DefinedTradeContact/ram:PersonName'),
      TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:BuyerTradeParty/ram:DefinedTradeContact/ram:DepartmentName'),
      TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:BuyerTradeParty/ram:DefinedTradeContact/ram:EmailURIUniversalCommunication/ram:URIID'),
      TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:BuyerTradeParty/ram:DefinedTradeContact/ram:TelephoneUniversalCommunication/ram:CompleteNumber'),
      TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:BuyerTradeParty/ram:DefinedTradeContact/ram:FaxUniversalCommunication/ram:CompleteNumber')
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
  Result.ActualDeliveryDate:= TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:ActualDeliverySupplyChainEvent/ram:OccurrenceDateTime/udt:DateTimeString');

  var _deliveryNoteNo : String := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssuerAssignedID');
  var _deliveryNoteDate : TDateTime := TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssueDateTime/udt:DateTimeString');

  if (_deliveryNoteDate < 100) then
  begin
    _deliveryNoteDate := TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssueDateTime');
  end;

  if ((_deliveryNoteDate > 100) or (_deliveryNoteNo <> '')) then
  begin
    Result.DeliveryNoteReferencedDocument := TZUGFeRDDeliveryNoteReferencedDocument.Create;
    Result.SetDeliveryNoteReferenceDocument(_deliveryNoteNo,_deliveryNoteDate);
  end;

  Result.Invoicee := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:InvoiceeTradeParty');
  Result.Payee := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:PayeeTradeParty');

  Result.PaymentReference := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:PaymentReference');
  Result.Currency :=  TZUGFeRDCurrencyCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:InvoiceCurrencyCode'));
  Result.SellerReferenceNo := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:InvoiceIssuerReference');

  // TODO: Multiple SpecifiedTradeSettlementPaymentMeans can exist for each account/institution (with different SEPA?)
  var _tempPaymentMeans : TZUGFeRDPaymentMeans := TZUGFeRDPaymentMeans.Create;
  _tempPaymentMeans.TypeCode := TZUGFeRDPaymentMeansTypeCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:TypeCode'));
  _tempPaymentMeans.Information := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:Information');
  _tempPaymentMeans.SEPACreditorIdentifier := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:CreditorReferenceID');

  var financialCardId : String := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:ApplicableTradeSettlementFinancialCard/ram:ID');
  var financialCardCardholderName : String := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:ApplicableTradeSettlementFinancialCard/ram:CardholderName');

  if ((financialCardId <> '') or (financialCardCardholderName <> '')) then
  begin
    _tempPaymentMeans.FinancialCard := TZUGFeRDFinancialCard.Create;
    _tempPaymentMeans.FinancialCard.Id := financialCardId;
    _tempPaymentMeans.FinancialCard.CardholderName := financialCardCardholderName;
  end;

  Result.PaymentMeans := _tempPaymentMeans;

  //TODO udt:DateTimeString
  Result.BillingPeriodStart := TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:BillingSpecifiedPeriod/ram:StartDateTime');
  Result.BillingPeriodEnd := TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:BillingSpecifiedPeriod/ram:EndDateTime');

  var creditorFinancialAccountNodes : IXMLDOMNodeList := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:PayeePartyCreditorFinancialAccount');
  var creditorFinancialInstitutions : IXMLDOMNodeList := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:PayeeSpecifiedCreditorFinancialInstitution');

  if (creditorFinancialAccountNodes.length = creditorFinancialInstitutions.length) then
  for i := 0 to creditorFinancialAccountNodes.length-1 do
  begin
    var _account : TZUGFeRDBankAccount := TZUGFeRDBankAccount.Create;
    _account.ID := TZUGFeRDXmlUtils.NodeAsString(creditorFinancialAccountNodes[i], './/ram:ProprietaryID');
    _account.IBAN := TZUGFeRDXmlUtils.NodeAsString(creditorFinancialAccountNodes[i], './/ram:IBANID');
    _account.Name := TZUGFeRDXmlUtils.NodeAsString(creditorFinancialAccountNodes[i], './/ram:AccountName');
    _account.BIC := TZUGFeRDXmlUtils.NodeAsString(creditorFinancialInstitutions[i], './/ram:BICID');
    _account.Bankleitzahl := TZUGFeRDXmlUtils.NodeAsString(creditorFinancialInstitutions[i], './/ram:GermanBankleitzahlID');
    _account.BankName := TZUGFeRDXmlUtils.NodeAsString(creditorFinancialInstitutions[i], './/ram:Name');
    Result.CreditorBankAccounts.Add(_account);
  end;

  var specifiedTradeSettlementPaymentMeansNodes : IXMLDOMNodeList := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans');
  for i := 0 to specifiedTradeSettlementPaymentMeansNodes.length-1 do
  begin
      var payerPartyDebtorFinancialAccountNode : IXMLDOMNode := specifiedTradeSettlementPaymentMeansNodes[i].selectSingleNode('ram:PayerPartyDebtorFinancialAccount');

      if (payerPartyDebtorFinancialAccountNode = nil) then
        continue;

      var _account : TZUGFeRDBankAccount := TZUGFeRDBankAccount.Create;
      _account.ID := TZUGFeRDXmlUtils.NodeAsString(payerPartyDebtorFinancialAccountNode, './/ram:ProprietaryID');
      _account.IBAN := TZUGFeRDXmlUtils.NodeAsString(payerPartyDebtorFinancialAccountNode, './/ram:IBANID');
      _account.Bankleitzahl := TZUGFeRDXmlUtils.NodeAsString(payerPartyDebtorFinancialAccountNode, './/ram:GermanBankleitzahlID');
      _account.BankName := TZUGFeRDXmlUtils.NodeAsString(payerPartyDebtorFinancialAccountNode, './/ram:Name');

      var payerSpecifiedDebtorFinancialInstitutionNode : IXMLDOMNode := specifiedTradeSettlementPaymentMeansNodes[i].SelectSingleNode('ram:PayerSpecifiedDebtorFinancialInstitution');
      if (payerSpecifiedDebtorFinancialInstitutionNode <> nil) then
          _account.BIC := TZUGFeRDXmlUtils.NodeAsString(payerPartyDebtorFinancialAccountNode, './/ram:BICID');

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
  //            ID := TZUGFeRDXmlUtils.NodeAsString(debitorFinancialAccountNodes[0], ".//ram:ProprietaryID'),
  //            IBAN := TZUGFeRDXmlUtils.NodeAsString(debitorFinancialAccountNodes[0], ".//ram:IBANID'),
  //            BIC := TZUGFeRDXmlUtils.NodeAsString(debitorFinancialInstitutions[0], ".//ram:BICID'),
  //            Bankleitzahl := TZUGFeRDXmlUtils.NodeAsString(debitorFinancialInstitutions[0], ".//ram:GermanBankleitzahlID'),
  //            BankName := TZUGFeRDXmlUtils.NodeAsString(debitorFinancialInstitutions[0], ".//ram:Name'),
  //        };

  //        Result.DebitorBankAccounts.Add(_account);
  //    } // !for(i)
  //

  nodes := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:ApplicableTradeTax');
  for i := 0 to nodes.length-1 do
  begin
    Result.AddApplicableTradeTax(TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:BasisAmount', 0),
                                 TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:RateApplicablePercent', 0),
                                 TZUGFeRDTaxTypesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:TypeCode')),
                                 TZUGFeRDTaxCategoryCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:CategoryCode')),
                                 0,
                                 TZUGFeRDTaxExemptionReasonCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ExemptionReasonCode')),
                                 TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ExemptionReason'));
  end;

  nodes := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeAllowanceCharge');
  for i := 0 to nodes.length-1 do
  begin
    Result.AddTradeAllowanceCharge(not TZUGFeRDXmlUtils.NodeAsBool(nodes[i], './/ram:ChargeIndicator'), // wichtig: das not (!) beachten
                                   TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:BasisAmount', 0),
                                   Result.Currency,
                                   TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:ActualAmount', 0),
                                   TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Reason'),
                                   TZUGFeRDSpecialServiceDescriptionCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:ReasonCode')),
                                   TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:ReasonCode')),
                                   TZUGFeRDTaxTypesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:CategoryTradeTax/ram:TypeCode')),
                                   TZUGFeRDTaxCategoryCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:CategoryTradeTax/ram:CategoryCode')),
                                   TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:CategoryTradeTax/ram:RateApplicablePercent', 0));
  end;

  nodes := doc.SelectNodes('//ram:SpecifiedLogisticsServiceCharge');
  for i := 0 to nodes.length-1 do
  begin
    Result.AddLogisticsServiceCharge(TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:AppliedAmount', 0),
                                     TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Description'),
                                     TZUGFeRDTaxTypesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:AppliedTradeTax/ram:TypeCode')),
                                     TZUGFeRDTaxCategoryCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:AppliedTradeTax/ram:CategoryCode')),
                                     TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:AppliedTradeTax/ram:RateApplicablePercent', 0));
  end;

  nodes := doc.SelectNodes('//ram:SpecifiedTradePaymentTerms');
  for i := 0 to nodes.length-1 do
  begin
    var paymentTerm : TZUGFeRDPaymentTerms := TZUGFeRDPaymentTerms.Create;
    paymentTerm.Description := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Description');
    paymentTerm.DueDate:= TZUGFeRDXmlUtils.NodeAsDateTime(nodes[i], './/ram:DueDateDateTime');
    paymentTerm.DirectDebitMandateID := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:DirectDebitMandateID');
    //TODO paymentTerm.PartialPaymentAmount
    //TODO paymentTerm.ApplicableTradePaymentPenaltyTerms
    paymentTerm.ApplicableTradePaymentDiscountTerms.BasisPeriodMeasure := TZUGFeRDXmlUtils.NodeAsDouble(nodes[i], './/ram:ApplicableTradePaymentDiscountTerms/ram:BasisPeriodMeasure');
    paymentTerm.ApplicableTradePaymentDiscountTerms.UnitCode := TZUGFeRDQuantityCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ApplicableTradePaymentDiscountTerms/ram:BasisPeriodMeasure/@unitCode'));
    paymentTerm.ApplicableTradePaymentDiscountTerms.BasisAmount := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:ApplicableTradePaymentDiscountTerms/ram:BasisAmount');
    paymentTerm.ApplicableTradePaymentDiscountTerms.CalculationPercent := TZUGFeRDXmlUtils.NodeAsDouble(nodes[i], './/ram:ApplicableTradePaymentDiscountTerms/ram:CalculationPercent');
    Result.PaymentTermsList.Add(paymentTerm);
  end;

  Result.LineTotalAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:LineTotalAmount', 0);
  Result.ChargeTotalAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:ChargeTotalAmount', 0);
  Result.AllowanceTotalAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:AllowanceTotalAmount', 0);
  Result.TaxBasisAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:TaxBasisTotalAmount',0);
  Result.TaxTotalAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:TaxTotalAmount', 0);
  Result.GrandTotalAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:GrandTotalAmount', 0);
  Result.RoundingAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:RoundingAmount', 0);
  Result.TotalPrepaidAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:TotalPrepaidAmount', 0);
  Result.DuePayableAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:DuePayableAmount', 0);

  // in this version we should only have on invoice referenced document but nevertheless...
  nodes := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:InvoiceReferencedDocument');
  for i := 0 to nodes.length-1 do
  begin
    Result.AddInvoiceReferencedDocument(
      TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:IssuerAssignedID'),
      TZUGFeRDXmlUtils.NodeAsDateTime(nodes[i], './ram:FormattedIssueDateTime')
    );
  end;

  node := doc.SelectSingleNode('//ram:ApplicableHeaderTradeAgreement/ram:BuyerOrderReferencedDocument');
  if node <> nil then
  begin
    Result.OrderDate:= TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:BuyerOrderReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString');
    Result.OrderNo := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:BuyerOrderReferencedDocument/ram:IssuerAssignedID');
  end;

  nodes := doc.SelectNodes('//ram:IncludedSupplyChainTradeLineItem');
  for i := 0 to nodes.length-1 do
    Result.TradeLineItems.Add(_parseTradeLineItem(nodes[i]));

  // Read SellerOrderReferencedDocument
  node := doc.SelectSingleNode('//ram:ApplicableHeaderTradeAgreement/ram:SellerOrderReferencedDocument');
  if node <> nil then
  begin
    Result.SellerOrderReferencedDocument := TZUGFeRDSellerOrderReferencedDocument.Create;
    Result.SellerOrderReferencedDocument.ID := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:SellerOrderReferencedDocument/ram:IssuerAssignedID');
    Result.SellerOrderReferencedDocument.IssueDateTime:= TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:SellerOrderReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString');
  end;
end;

function TZUGFeRDInvoiceDescriptor20Reader._getAdditionalReferencedDocument(
  a_oXmlNode: IXmlDomNode): TZUGFeRDAdditionalReferencedDocument;
begin
  var strBase64BinaryData : String := TZUGFeRDXmlUtils.NodeAsString(a_oXmlNode, 'ram:AttachmentBinaryObject');
  Result := TZUGFeRDAdditionalReferencedDocument.Create(false);
  Result.ID := TZUGFeRDXmlUtils.NodeAsString(a_oXmlNode, 'ram:IssuerAssignedID');
  Result.TypeCode := TZUGFeRDAdditionalReferencedDocumentTypeCodeExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(a_oXmlNode, 'ram:TypeCode'));
  Result.Name := TZUGFeRDXmlUtils.NodeAsString(a_oXmlNode, 'ram:Name');
  Result.IssueDateTime:= TZUGFeRDXmlUtils.NodeAsDateTime(a_oXmlNode, 'ram:FormattedIssueDateTime/qdt:DateTimeString');
  if strBase64BinaryData <> '' then
  begin
    Result.AttachmentBinaryObject := TMemoryStream.Create;
    var strBase64BinaryDataBytes : TBytes := TNetEncoding.Base64String.DecodeStringToBytes(strBase64BinaryData);
    Result.AttachmentBinaryObject.Write(strBase64BinaryDataBytes,Length(strBase64BinaryDataBytes));
  end;
  Result.Filename := TZUGFeRDXmlUtils.NodeAsString(a_oXmlNode, 'ram:AttachmentBinaryObject/@filename');
  Result.ReferenceTypeCode := TZUGFeRDReferenceTypeCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(a_oXmlNode, 'ram:ReferenceTypeCode'));
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
  Result.ID.ID := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:ID');
  Result.ID.SchemeID := TZUGFeRDGlobalIDSchemeIdentifiers.Unknown;
  Result.GlobalID.ID := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:GlobalID');
  Result.GlobalID.SchemeID := TZUGFeRDGlobalIDSchemeIdentifiersExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(node, 'ram:GlobalID/@schemeID'));
  Result.Name := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:Name');
  Result.Description := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:Description'); // Seller only BT-33
  Result.Postcode := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:PostalTradeAddress/ram:PostcodeCode');
  Result.City := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:PostalTradeAddress/ram:CityName');
  Result.Country := TZUGFeRDCountryCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(node, 'ram:PostalTradeAddress/ram:CountryID'));

  if (not lineTwo.IsEmpty) then
  begin
    Result.ContactName := lineOne;
    Result.Street := lineTwo;
  end else
  begin
    Result.Street := lineOne;
    Result.ContactName := '';
  end;
  Result.AddressLine3 := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:PostalTradeAddress/ram:LineThree');
  Result.CountrySubdivisionName := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:PostalTradeAddress/ram:CountrySubDivisionName');
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

  Result.GlobalID.ID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:GlobalID');
  Result.GlobalID.SchemeID := TZUGFeRDGlobalIDSchemeIdentifiersExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:GlobalID/@schemeID'));
  Result.SellerAssignedID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:SellerAssignedID');
  Result.BuyerAssignedID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:BuyerAssignedID');
  Result.Name := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:Name');
  Result.Description := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:Description');
  Result.UnitQuantity:= TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:BasisQuantity', 0);
  Result.BilledQuantity := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:BilledQuantity', 0);
  Result.PackageQuantity := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:PackageQuantity', 0);
  Result.ChargeFreeQuantity := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:ChargeFreeQuantity', 0);
//  Result.LineTotalAmount.SetValue(TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:LineTotalAmount', 0));
  Result.LineTotalAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:LineTotalAmount', 0);
  Result.TaxCategoryCode := TZUGFeRDTaxCategoryCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:ApplicableTradeTax/ram:CategoryCode'));
  Result.TaxType := TZUGFeRDTaxTypesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:ApplicableTradeTax/ram:TypeCode'));
  Result.TaxPercent := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:ApplicableTradeTax/ram:RateApplicablePercent', 0);
  Result.NetUnitPrice:= TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:NetPriceProductTradePrice/ram:ChargeAmount', 0);
  Result.GrossUnitPrice:= TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:GrossPriceProductTradePrice/ram:ChargeAmount', 0);
  Result.UnitCode := TZUGFeRDQuantityCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:BasisQuantity/@unitCode'));
  Result.PackageUnitCode := TZUGFeRDQuantityCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:PackageQuantity/@unitCode'));
  Result.ChargeFreeUnitCode := TZUGFeRDQuantityCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:ChargeFreeQuantity/@unitCode'));
  Result.BillingPeriodStart:= TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:BillingSpecifiedPeriod/ram:StartDateTime/udt:DateTimeString');
  Result.BillingPeriodEnd:= TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:BillingSpecifiedPeriod/ram:EndDateTime/udt:DateTimeString');

  nodes := tradeLineItem.SelectNodes('.//ram:SpecifiedTradeProduct/ram:ApplicableProductCharacteristic');
  for i := 0 to nodes.length-1 do
  begin
    var apcItem : TZUGFeRDApplicableProductCharacteristic := TZUGFeRDApplicableProductCharacteristic.Create;
    apcItem.Description := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Description');
    apcItem.Value := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Value');
    Result.ApplicableProductCharacteristics.Add(apcItem);
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:AssociatedDocumentLineDocument') <> nil) then
  begin
    Result.AssociatedDocument := TZUGFeRDAssociatedDocument.Create(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:AssociatedDocumentLineDocument/ram:LineID'));
    Result.AssociatedDocument.LineStatusCode := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:AssociatedDocumentLineDocument/ram:LineStatusCode');
    Result.AssociatedDocument.LineStatusReasonCode := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:AssociatedDocumentLineDocument/ram:LineStatusReasonCode');

    nodes := tradeLineItem.SelectNodes('.//ram:AssociatedDocumentLineDocument/ram:IncludedNote');
    for i := 0 to nodes.length-1 do
    begin
      var noteItem : TZUGFeRDNote := TZUGFeRDNote.Create(
          TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Content'),
          TZUGFeRDSubjectCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:SubjectCode')),
          TZUGFeRDContentCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ContentCode'))
        );
      Result.AssociatedDocument.Notes.Add(noteItem);
    end;
  end;

  nodes := tradeLineItem.SelectNodes('.//ram:SpecifiedLineTradeAgreement/ram:GrossPriceProductTradePrice/ram:AppliedTradeAllowanceCharge');
  for i := 0 to nodes.length-1 do
  begin

    var chargeIndicator : Boolean := TZUGFeRDXmlUtils.NodeAsBool(nodes[i], './ram:ChargeIndicator/udt:Indicator');
    var basisAmount : Currency := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './ram:BasisAmount',0);
    var basisAmountCurrency : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:BasisAmount/@currencyID');
    var actualAmount : Currency := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './ram:ActualAmount',0);
    var actualAmountCurrency : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:ActualAmount/@currencyID');
    var reason : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:Reason');
    var reasonCodeCharge : TZUGFeRDSpecialServiceDescriptionCodes := TZUGFeRDSpecialServiceDescriptionCodes.Unknown;
    var reasonCodeAllowance : TZUGFeRDAllowanceOrChargeIdentificationCodes := TZUGFeRDAllowanceOrChargeIdentificationCodes.Unknown;
    if chargeIndicator then
      reasonCodeCharge := TZUGFeRDSpecialServiceDescriptionCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:ReasonCode'))
    else
      reasonCodeAllowance := TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:ReasonCode'));

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
    Result.UnitCode := TZUGFeRDQuantityCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:BilledQuantity/@unitCode'));
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeAgreement/ram:BuyerOrderReferencedDocument/ram:IssuerAssignedID') <> nil) then
  begin
    Result.BuyerOrderReferencedDocument := TZUGFeRDBuyerOrderReferencedDocument.Create;
    Result.BuyerOrderReferencedDocument.ID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:BuyerOrderReferencedDocument/ram:IssuerAssignedID');
    Result.BuyerOrderReferencedDocument.IssueDateTime:= TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:BuyerOrderReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString');
    Result.BuyerOrderReferencedDocument.LineID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:BuyerOrderReferencedDocument/ram:LineID');
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssuerAssignedID') <> nil) then
  begin
    Result.DeliveryNoteReferencedDocument := TZUGFeRDDeliveryNoteReferencedDocument.Create;
    Result.DeliveryNoteReferencedDocument.ID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedLineTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssuerAssignedID');
    Result.DeliveryNoteReferencedDocument.IssueDateTime:= TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:SpecifiedLineTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString');
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeDelivery/ram:ActualDeliverySupplyChainEvent/ram:OccurrenceDateTime') <> nil) then
  begin
    Result.ActualDeliveryDate:= TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:SpecifiedLineTradeDelivery/ram:ActualDeliverySupplyChainEvent/ram:OccurrenceDateTime/udt:DateTimeString');
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeAgreement/ram:ContractReferencedDocument/ram:IssuerAssignedID') <> nil) then
  begin
    Result.ContractReferencedDocument := TZUGFeRDContractReferencedDocument.Create;
    Result.ContractReferencedDocument.ID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:ContractReferencedDocument/ram:IssuerAssignedID');
    Result.ContractReferencedDocument.IssueDateTime:= TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:ContractReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString');
  end;

  //Get all referenced AND embedded documents
  nodes := tradeLineItem.SelectNodes('.//ram:SpecifiedLineTradeAgreement/ram:AdditionalReferencedDocument');
  for i := 0 to nodes.length-1 do
  begin
    Result.AdditionalReferencedDocuments.Add(_getAdditionalReferencedDocument(nodes[i]));
  end;
end;

end.
