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

unit intf.ZUGFeRDInvoiceDescriptor22UBLReader;

interface

uses
  System.SysUtils, System.Classes, System.DateUtils, System.Math
  ,System.NetEncoding
  ,Xml.XMLDoc, Xml.xmldom, Xml.XMLIntf
  ,Xml.Win.msxmldom, Winapi.MSXMLIntf, Winapi.msxml
  ,intf.ZUGFeRDXmlHelper
  ,intf.ZUGFeRDIInvoiceDescriptorReader
  ,intf.ZUGFeRDTradeLineItem
  ,intf.ZUGFeRDParty
  ,intf.ZUGFeRDAdditionalReferencedDocument
  ,intf.ZUGFeRDInvoiceDescriptor
  ,intf.ZUGFeRDExceptions
  ,intf.ZUGFeRDProfile,intf.ZUGFeRDInvoiceTypes
  ,intf.ZUGFeRDSubjectCodes
  ,intf.ZUGFeRDLegalOrganization
  ,intf.ZUGFeRDGlobalID,intf.ZUGFeRDGlobalIDSchemeIdentifiers
  ,intf.ZUGFeRDCountryCodes
  ,intf.ZUGFeRDTaxRegistrationSchemeID
  ,intf.ZUGFeRDElectronicAddressSchemeIdentifiers
  ,intf.ZUGFeRDContact
  ,intf.ZUGFeRDAdditionalReferencedDocumentTypeCodes
  ,intf.ZUGFeRDReferenceTypeCodes
  ,intf.ZUGFeRDDeliveryNoteReferencedDocument
  ,intf.ZUGFeRDCurrencyCodes
  ,intf.ZUGFeRDPaymentMeans
  ,intf.ZUGFeRDPaymentMeansTypeCodes
  ,intf.ZUGFeRDFinancialCard
  ,intf.ZUGFeRDBankAccount
  ,intf.ZUGFeRDTaxTypes,intf.ZUGFeRDTaxCategoryCodes
  ,intf.ZUGFeRDTaxExemptionReasonCodes
  ,intf.ZUGFeRDInvoiceReferencedDocument
  ,intf.ZUGFeRDPaymentTerms
  ,intf.ZUGFeRDSellerOrderReferencedDocument
  ,intf.ZUGFeRDReceivableSpecifiedTradeAccountingAccount
  ,intf.ZUGFeRDAccountingAccountTypeCodes
  ,intf.ZUGFeRDContractReferencedDocument
  ,intf.ZUGFeRDSpecifiedProcuringProject
  ,intf.ZUGFeRDQuantityCodes
  ,intf.ZUGFeRDApplicableProductCharacteristic
  ,intf.ZUGFeRDBuyerOrderReferencedDocument
  ,intf.ZUGFeRDAssociatedDocument
  ,intf.ZUGFeRDNote
  ,intf.ZUGFeRDContentCodes
  ;

type
  TZUGFeRDInvoiceDescriptor22UblReader = class(TZUGFeRDIInvoiceDescriptorReader)
  private
    function GetValidURIs : TArray<string>;
  {$HINTS OFF} // private symbols not yet used so switch off the hint
    function _parseTradeLineItem(tradeLineItem : IXmlDomNode {nsmgr: XmlNamespaceManager = nil; }) : TZUGFeRDTradeLineItem;
    function _nodeAsParty(basenode: IXmlDomNode; const xpath: string) : TZUGFeRDParty;
    function _getAdditionalReferencedDocument(a_oXmlNode : IXmlDomNode {nsmgr: XmlNamespaceManager = nil; }) : TZUGFeRDAdditionalReferencedDocument;
    function _nodeAsLegalOrganization(basenode: IXmlDomNode; const xpath: string) : TZUGFeRDLegalOrganization;
  {$HINTS ON}
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

{ TZUGFeRDInvoiceDescriptor22UBLReader }

function TZUGFeRDInvoiceDescriptor22UBLReader.GetValidURIs : TArray<string>;
begin
  Result := TArray<string>.Create(
    'urn:oasis:names:specification:ubl:schema:xsd:Invoice-2',
    'urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2',
    'urn:oasis:names:specification:ubl:schema:xsd:CommonBasicComponents-2'
  );
end;

function TZUGFeRDInvoiceDescriptor22UBLReader.IsReadableByThisReaderVersion(
  stream: TStream): Boolean;
begin
  Result := IsReadableByThisReaderVersion(stream, GetValidURIs);
end;

function TZUGFeRDInvoiceDescriptor22UBLReader.IsReadableByThisReaderVersion(
  xmldocument : IXMLDocument): Boolean;
begin
  Result := IsReadableByThisReaderVersion(xmldocument, GetValidURIs);
end;

function TZUGFeRDInvoiceDescriptor22UBLReader.Load(stream: TStream): TZUGFeRDInvoiceDescriptor;
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

function TZUGFeRDInvoiceDescriptor22UBLReader.Load(
  xmldocument : IXMLDocument): TZUGFeRDInvoiceDescriptor;
var
  doc : IXMLDOMDocument2;
//  node : IXMLDOMNode;
//  node,node2,node3,node4,nodeSupplyChainTradeTransaction,
//  nodeApplicableHeaderTradeAgreement : IXMLDOMNode;
//  nodes : IXMLDOMNodeList;
//  i : Integer;
begin
  doc := TZUGFeRDXmlHelper.PrepareDocumentForXPathQuerys(xmldocument);
//
//  //nsmgr.AddNamespace("qdt", "urn:un:unece:uncefact:data:standard:QualifiedDataType:100");
//  //nsmgr.AddNamespace("a", "urn:un:unece:uncefact:data:standard:QualifiedDataType:100");
//  //nsmgr.AddNamespace("rsm", "urn:un:unece:uncefact:data:standard:CrossIndustryInvoice:100");
//  //nsmgr.AddNamespace("ram", "urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:100");
//  //nsmgr.AddNamespace("udt", "urn:un:unece:uncefact:data:standard:UnqualifiedDataType:100");
//
  Result := TZUGFeRDInvoiceDescriptor.Create;
//
//  Result.IsTest := _nodeAsBool(doc.documentElement,'//*[local-name()="ExchangedDocumentContext"]/ram:TestIndicator');
//  Result.BusinessProcess := _nodeAsString(doc.DocumentElement, '//*[local-name()="BusinessProcessSpecifiedDocumentContextParameter"]/ram:ID');//, nsmgr),
//  Result.Profile := TZUGFeRDProfileExtensions.StringToEnum(_nodeAsString(doc.DocumentElement, '//ram:GuidelineSpecifiedDocumentContextParameter/ram:ID'));//, nsmgr)),
//  Result.Type_ := TTEnumExtensions<TZUGFeRDInvoiceTypes>.StringToEnum(_nodeAsString(doc.DocumentElement, '//*[local-name()="ExchangedDocument"]/ram:TypeCode'));//, nsmgr)),
//  Result.InvoiceNo := _nodeAsString(doc.DocumentElement, '//*[local-name()="ExchangedDocument"]/ram:ID');//, nsmgr),
//  Result.InvoiceDate := _nodeAsDateTime(doc.DocumentElement, '//*[local-name()="ExchangedDocument"]/ram:IssueDateTime/udt:DateTimeString');//", nsmgr)
//
//  nodes := doc.selectNodes('//*[local-name()="ExchangedDocument"]/ram:IncludedNote');
//  for i := 0 to nodes.length-1 do
//  begin
//    var content : String := _nodeAsString(nodes[i], './/ram:Content');
//    var _subjectCode : String := _nodeAsString(nodes[i], './/ram:SubjectCode');
//    var subjectCode : TZUGFeRDSubjectCodes := TTEnumExtensions<TZUGFeRDSubjectCodes>.StringToEnum(_subjectCode);
//    Result.AddNote(content, subjectCode);
//  end;
//
//  Result.ReferenceOrderNo := _nodeAsString(doc, '//ram:ApplicableHeaderTradeAgreement/ram:BuyerReference');
//
//  Result.Seller := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:SellerTradeParty');
//
//  if doc.selectSingleNode('//ram:SellerTradeParty/ram:URIUniversalCommunication') <> nil then
//  begin
//    var id : String := _nodeAsString(doc.DocumentElement, '//ram:SellerTradeParty/ram:URIUniversalCommunication/ram:URIID');
//    var schemeID : String := _nodeAsString(doc.DocumentElement, '//ram:SellerTradeParty/ram:URIUniversalCommunication/ram:URIID/@schemeID');
//
//    var eas : TZUGFeRDElectronicAddressSchemeIdentifiers :=
//       TTEnumExtensions<TZUGFeRDElectronicAddressSchemeIdentifiers>.StringToEnum(schemeID);
//
//    if (eas <> TZUGFeRDElectronicAddressSchemeIdentifiers.Unknown) then
//      Result.SetSellerElectronicAddress(id, eas);
//  end;
//
//  nodes := doc.selectNodes('//ram:ApplicableHeaderTradeAgreement/ram:SellerTradeParty/ram:SpecifiedTaxRegistration');
//  for i := 0 to nodes.length-1 do
//  begin
//    var id : String := _nodeAsString(nodes[i], './/ram:ID');
//    var schemeID : String := _nodeAsString(nodes[i], './/ram:ID/@schemeID');
//    Result.AddSellerTaxRegistration(id, TTEnumExtensions<TZUGFeRDTaxRegistrationSchemeID>.StringToEnum(schemeID));
//  end;
//
//  if (doc.selectSingleNode('//ram:SellerTradeParty/ram:DefinedTradeContact') <> nil) then
//  begin
//    Result.SellerContact := TZUGFeRDContact.Create;
//    Result.SellerContact.Name := _nodeAsString(doc.DocumentElement, '//ram:SellerTradeParty/ram:DefinedTradeContact/ram:PersonName');
//    Result.SellerContact.OrgUnit := _nodeAsString(doc.DocumentElement, '//ram:SellerTradeParty/ram:DefinedTradeContact/ram:DepartmentName');
//    Result.SellerContact.PhoneNo := _nodeAsString(doc.DocumentElement, '//ram:SellerTradeParty/ram:DefinedTradeContact/ram:TelephoneUniversalCommunication/ram:CompleteNumber');
//    Result.SellerContact.FaxNo := _nodeAsString(doc.DocumentElement, '//ram:SellerTradeParty/ram:DefinedTradeContact/ram:FaxUniversalCommunication/ram:CompleteNumber');
//    Result.SellerContact.EmailAddress := _nodeAsString(doc.DocumentElement, '//ram:SellerTradeParty/ram:DefinedTradeContact/ram:EmailURIUniversalCommunication/ram:URIID');
//  end;
//
//  Result.Buyer := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:BuyerTradeParty');
//
//  if (doc.SelectSingleNode('//ram:BuyerTradeParty/ram:URIUniversalCommunication') <> nil) then
//  begin
//    var id : String := _nodeAsString(doc.DocumentElement, '//ram:BuyerTradeParty/ram:URIUniversalCommunication/ram:URIID');
//    var schemeID : String := _nodeAsString(doc.DocumentElement, '//ram:BuyerTradeParty/ram:URIUniversalCommunication/ram:URIID/@schemeID');
//
//    var eas : TZUGFeRDElectronicAddressSchemeIdentifiers := TTEnumExtensions<TZUGFeRDElectronicAddressSchemeIdentifiers>.StringToEnum(schemeID);
//
//    if (eas <> TZUGFeRDElectronicAddressSchemeIdentifiers.Unknown) then
//      Result.SetBuyerElectronicAddress(id, eas);
//  end;
//
//  nodes := doc.selectNodes('//ram:ApplicableHeaderTradeAgreement/ram:BuyerTradeParty/ram:SpecifiedTaxRegistration');
//  for i := 0 to nodes.length-1 do
//  begin
//    var id : String := _nodeAsString(nodes[i], './/ram:ID');
//    var schemeID : String := _nodeAsString(nodes[i], './/ram:ID/@schemeID');
//    Result.AddBuyerTaxRegistration(id, TTEnumExtensions<TZUGFeRDTaxRegistrationSchemeID>.StringToEnum(schemeID));
//  end;
//
//  if (doc.SelectSingleNode('//ram:BuyerTradeParty/ram:DefinedTradeContact') <> nil) then
//  begin
//    Result.BuyerContact := TZUGFeRDContact.Create;
//    Result.SetBuyerContact(
//      _nodeAsString(doc.DocumentElement, '//ram:BuyerTradeParty/ram:DefinedTradeContact/ram:PersonName'),
//      _nodeAsString(doc.DocumentElement, '//ram:BuyerTradeParty/ram:DefinedTradeContact/ram:DepartmentName'),
//      _nodeAsString(doc.DocumentElement, '//ram:BuyerTradeParty/ram:DefinedTradeContact/ram:EmailURIUniversalCommunication/ram:URIID'),
//      _nodeAsString(doc.DocumentElement, '//ram:BuyerTradeParty/ram:DefinedTradeContact/ram:TelephoneUniversalCommunication/ram:CompleteNumber'),
//      _nodeAsString(doc.DocumentElement, '//ram:BuyerTradeParty/ram:DefinedTradeContact/ram:FaxUniversalCommunication/ram:CompleteNumber')
//    );
//  end;
//
//  //Get all referenced and embedded documents (BG-24)
//  nodes := doc.SelectNodes('.//ram:ApplicableHeaderTradeAgreement/ram:AdditionalReferencedDocument');
//  for i := 0 to nodes.length-1 do
//  begin
//    Result.AdditionalReferencedDocuments.Add(_getAdditionalReferencedDocument(nodes[i]));
//  end;
//
//  //-------------------------------------------------
//  // hzi: With old implementation only the first document has been read instead of all documents
//  //-------------------------------------------------
//  //if (doc.SelectSingleNode("//ram:AdditionalReferencedDocument') != null)
//  //{
//  //    string _issuerAssignedID := _nodeAsString(doc, "//ram:AdditionalReferencedDocument/ram:IssuerAssignedID');
//  //    string _typeCode := _nodeAsString(doc, "//ram:AdditionalReferencedDocument/ram:TypeCode');
//  //    string _referenceTypeCode := _nodeAsString(doc, "//ram:AdditionalReferencedDocument/ram:ReferenceTypeCode');
//  //    string _name := _nodeAsString(doc, "//ram:AdditionalReferencedDocument/ram:Name');
//  //    DateTime? _date := _nodeAsDateTime(doc.DocumentElement, "//ram:AdditionalReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString');
//
//  //    if (doc.SelectSingleNode("//ram:AdditionalReferencedDocument/ram:AttachmentBinaryObject') != null)
//  //    {
//  //        string _filename := _nodeAsString(doc, "//ram:AdditionalReferencedDocument/ram:AttachmentBinaryObject/@filename');
//  //        byte[] data := Convert.FromBase64String(_nodeAsString(doc, "//ram:AdditionalReferencedDocument/ram:AttachmentBinaryObject'));
//
//  //        Result.AddAdditionalReferencedDocument(id: _issuerAssignedID,
//  //                                               typeCode: default(AdditionalReferencedDocumentTypeCode).FromString(_typeCode),
//  //                                               issueDateTime: _date,
//  //                                               referenceTypeCode: default(ReferenceTypeCodes).FromString(_referenceTypeCode),
//  //                                               name: _name,
//  //                                               attachmentBinaryObject: data,
//  //                                               filename: _filename);
//  //    }
//  //    else
//  //    {
//  //        Result.AddAdditionalReferencedDocument(id: _issuerAssignedID,
//  //                                               typeCode: default(AdditionalReferencedDocumentTypeCode).FromString(_typeCode),
//  //                                               issueDateTime: _date,
//  //                                               referenceTypeCode: default(ReferenceTypeCodes).FromString(_referenceTypeCode),
//  //                                               name: _name);
//  //    }
//  //}
//  //-------------------------------------------------
//
//
//  Result.ShipTo := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:ShipToTradeParty');
//  Result.ShipFrom := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:ShipFromTradeParty');
//  Result.ActualDeliveryDate.SetValue(_nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:ActualDeliverySupplyChainEvent/ram:OccurrenceDateTime/udt:DateTimeString'));
//
//  var _despatchAdviceNo : String := _nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:DespatchAdviceReferencedDocument/ram:IssuerAssignedID');
//  var _despatchAdviceDate : TDateTime := _nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:DespatchAdviceReferencedDocument/ram:FormattedIssueDateTime/udt:DateTimeString');
//
//  if (_despatchAdviceDate < 100) then
//  begin
//    _despatchAdviceDate := _nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:DespatchAdviceReferencedDocument/ram:FormattedIssueDateTime');
//  end;
//
//  if ((_despatchAdviceDate > 100) or (_despatchAdviceNo <> '')) then
//  begin
//    Result.DespatchAdviceReferencedDocument := TZUGFeRDDespatchAdviceReferencedDocument.Create;
//    Result.SetDespatchAdviceReferencedDocument(_despatchAdviceNo,_despatchAdviceDate);
//  end;
//
//  var _deliveryNoteNo : String := _nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssuerAssignedID');
//  var _deliveryNoteDate : TDateTime := _nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssueDateTime/udt:DateTimeString');
//
//  if (_deliveryNoteDate < 100) then
//  begin
//    _deliveryNoteDate := _nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssueDateTime');
//  end;
//
//  if ((_deliveryNoteDate > 100) or (_deliveryNoteNo <> '')) then
//  begin
//    Result.DeliveryNoteReferencedDocument := TZUGFeRDDeliveryNoteReferencedDocument.Create;
//    Result.SetDeliveryNoteReferenceDocument(_deliveryNoteNo,_deliveryNoteDate);
//  end;
//
//  Result.Invoicee := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:InvoiceeTradeParty');
//  Result.Payee := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:PayeeTradeParty');
//
//  Result.PaymentReference := _nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:PaymentReference');
//  Result.Currency :=  TTEnumExtensions<TZUGFeRDCurrencyCodes>.StringToEnum(_nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:InvoiceCurrencyCode'));
//
//  // TODO: Multiple SpecifiedTradeSettlementPaymentMeans can exist for each account/institution (with different SEPA?)
//  var _tempPaymentMeans : TZUGFeRDPaymentMeans := TZUGFeRDPaymentMeans.Create;
//  _tempPaymentMeans.TypeCode := TTEnumExtensions<TZUGFeRDPaymentMeansTypeCodes>.StringToEnum(_nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:TypeCode'));
//  _tempPaymentMeans.Information := _nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:Information');
//  _tempPaymentMeans.SEPACreditorIdentifier := _nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:CreditorReferenceID');
//  _tempPaymentMeans.SEPAMandateReference := _nodeAsString(doc.DocumentElement, '//ram:SpecifiedTradePaymentTerms/ram:DirectDebitMandateID');
//
//  var financialCardId : String := _nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:ApplicableTradeSettlementFinancialCard/ram:ID');
//  var financialCardCardholderName : String := _nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:ApplicableTradeSettlementFinancialCard/ram:CardholderName');
//
//  if ((financialCardId <> '') or (financialCardCardholderName <> '')) then
//  begin
//    _tempPaymentMeans.FinancialCard := TZUGFeRDFinancialCard.Create;
//    _tempPaymentMeans.FinancialCard.Id := financialCardId;
//    _tempPaymentMeans.FinancialCard.CardholderName := financialCardCardholderName;
//  end;
//
//  Result.PaymentMeans := _tempPaymentMeans;
//
//  //TODO udt:DateTimeString
//  Result.BillingPeriodStart := _nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:BillingSpecifiedPeriod/ram:StartDateTime');
//  Result.BillingPeriodEnd := _nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:BillingSpecifiedPeriod/ram:EndDateTime');
//
//  var creditorFinancialAccountNodes : IXMLDOMNodeList := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:PayeePartyCreditorFinancialAccount');
//  var creditorFinancialInstitutions : IXMLDOMNodeList := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:PayeeSpecifiedCreditorFinancialInstitution');
//
//  var numberOfAccounts : Integer := Max(creditorFinancialAccountNodes.Length,creditorFinancialInstitutions.Length);
//  for i := 0 to numberOfAccounts-1 do
//  begin
//    Result.CreditorBankAccounts.Add(TZUGFeRDBankAccount.Create);
//  end;
//
//  for i := 0 to creditorFinancialAccountNodes.Length-1 do
//  begin
//    Result.CreditorBankAccounts[i].ID := _nodeAsString(creditorFinancialAccountNodes[i], './/ram:ProprietaryID');
//    Result.CreditorBankAccounts[i].IBAN := _nodeAsString(creditorFinancialAccountNodes[i], './/ram:IBANID');
//    Result.CreditorBankAccounts[i].Name := _nodeAsString(creditorFinancialAccountNodes[i], './/ram:AccountName');
//  end;
//
//  for i := 0 to creditorFinancialInstitutions.Length-1 do
//  begin
//    Result.CreditorBankAccounts[i].BIC := _nodeAsString(creditorFinancialInstitutions[i], './/ram:BICID');
//    Result.CreditorBankAccounts[i].Bankleitzahl := _nodeAsString(creditorFinancialInstitutions[i], './/ram:GermanBankleitzahlID');
//    Result.CreditorBankAccounts[i].BankName := _nodeAsString(creditorFinancialInstitutions[i], './/ram:Name');
//  end;
//
//  var specifiedTradeSettlementPaymentMeansNodes : IXMLDOMNodeList := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans');
//  for i := 0 to specifiedTradeSettlementPaymentMeansNodes.length-1 do
//  begin
//      var payerPartyDebtorFinancialAccountNode : IXMLDOMNode := specifiedTradeSettlementPaymentMeansNodes[i].selectSingleNode('ram:PayerPartyDebtorFinancialAccount');
//
//      if (payerPartyDebtorFinancialAccountNode = nil) then
//        continue;
//
//      var _account : TZUGFeRDBankAccount := TZUGFeRDBankAccount.Create;
//      _account.ID := _nodeAsString(payerPartyDebtorFinancialAccountNode, './/ram:ProprietaryID');
//      _account.IBAN := _nodeAsString(payerPartyDebtorFinancialAccountNode, './/ram:IBANID');
//      _account.Bankleitzahl := _nodeAsString(payerPartyDebtorFinancialAccountNode, './/ram:GermanBankleitzahlID');
//      _account.BankName := _nodeAsString(payerPartyDebtorFinancialAccountNode, './/ram:Name');
//
//      var payerSpecifiedDebtorFinancialInstitutionNode : IXMLDOMNode := specifiedTradeSettlementPaymentMeansNodes[i].SelectSingleNode('ram:PayerSpecifiedDebtorFinancialInstitution');
//      if (payerSpecifiedDebtorFinancialInstitutionNode <> nil) then
//          _account.BIC := _nodeAsString(payerPartyDebtorFinancialAccountNode, './/ram:BICID');
//
//      Result.DebitorBankAccounts.Add(_account);
//  end;
//
//  nodes := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:ApplicableTradeTax');
//  for i := 0 to nodes.length-1 do
//  begin
//    Result.AddApplicableTradeTax(_nodeAsDecimal(nodes[i], './/ram:BasisAmount', 0),
//                                 _nodeAsDecimal(nodes[i], './/ram:RateApplicablePercent', 0),
//                                 TTEnumExtensions<TZUGFeRDTaxTypes>.StringToEnum(_nodeAsString(nodes[i], './/ram:TypeCode')),
//                                 TTEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum(_nodeAsString(nodes[i], './/ram:CategoryCode')),
//                                 0,
//                                 TTEnumExtensions<TZUGFeRDTaxExemptionReasonCodes>.StringToEnum(_nodeAsString(nodes[i], './/ram:ExemptionReasonCode')),
//                                 _nodeAsString(nodes[i], './/ram:ExemptionReason'));
//  end;
//
//  nodes := doc.SelectNodes('//ram:SpecifiedTradeAllowanceCharge');
//  for i := 0 to nodes.length-1 do
//  begin
//    Result.AddTradeAllowanceCharge(not _nodeAsBool(nodes[i], './/ram:ChargeIndicator'), // wichtig: das not (!) beachten
//                                   _nodeAsDecimal(nodes[i], './/ram:BasisAmount', 0),
//                                   Result.Currency,
//                                   _nodeAsDecimal(nodes[i], './/ram:ActualAmount', 0),
//                                   _nodeAsDecimal(node, './/ram:CalculationPercent', 0),
//                                   _nodeAsString(nodes[i], './/ram:Reason'),
//                                   TTEnumExtensions<TZUGFeRDTaxTypes>.StringToEnum(_nodeAsString(nodes[i], './/ram:CategoryTradeTax/ram:TypeCode')),
//                                   TTEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum(_nodeAsString(nodes[i], './/ram:CategoryTradeTax/ram:CategoryCode')),
//                                   _nodeAsDecimal(nodes[i], './/ram:CategoryTradeTax/ram:RateApplicablePercent', 0));
//  end;
//
//  nodes := doc.SelectNodes('//ram:SpecifiedLogisticsServiceCharge');
//  for i := 0 to nodes.length-1 do
//  begin
//    Result.AddLogisticsServiceCharge(_nodeAsDecimal(nodes[i], './/ram:AppliedAmount', 0),
//                                     _nodeAsString(nodes[i], './/ram:Description'),
//                                     TTEnumExtensions<TZUGFeRDTaxTypes>.StringToEnum(_nodeAsString(nodes[i], './/ram:AppliedTradeTax/ram:TypeCode')),
//                                     TTEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum(_nodeAsString(nodes[i], './/ram:AppliedTradeTax/ram:CategoryCode')),
//                                     _nodeAsDecimal(nodes[i], './/ram:AppliedTradeTax/ram:RateApplicablePercent', 0));
//  end;
//
//  Result.InvoiceReferencedDocument := TZUGFeRDInvoiceReferencedDocument.Create;
//  Result.InvoiceReferencedDocument.ID := _nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:InvoiceReferencedDocument/ram:IssuerAssignedID');
//  Result.InvoiceReferencedDocument.IssueDateTime.SetValue(_nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:InvoiceReferencedDocument/ram:FormattedIssueDateTime'));
//
//  Result.PaymentTerms := TZUGFeRDPaymentTerms.Create;
//  Result.PaymentTerms.Description := _nodeAsString(doc.DocumentElement, '//ram:SpecifiedTradePaymentTerms/ram:Description');
//  Result.PaymentTerms.DueDate.SetValue(_nodeAsDateTime(doc.DocumentElement, '//ram:SpecifiedTradePaymentTerms/ram:DueDateDateTime'));
//
//  Result.LineTotalAmount.SetValue(_nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:LineTotalAmount', 0));
//  Result.ChargeTotalAmount.SetValue(_nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:ChargeTotalAmount', 0));
//  Result.AllowanceTotalAmount.SetValue(_nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:AllowanceTotalAmount', 0));
//  Result.TaxBasisAmount.SetValue(_nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:TaxBasisTotalAmount',0));
//  Result.TaxTotalAmount.SetValue(_nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:TaxTotalAmount', 0));
//  Result.GrandTotalAmount.SetValue(_nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:GrandTotalAmount', 0));
//  Result.RoundingAmount.SetValue(_nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:RoundingAmount', 0));
//  Result.TotalPrepaidAmount.SetValue(_nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:TotalPrepaidAmount', 0));
//  Result.DuePayableAmount.SetValue(_nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:DuePayableAmount', 0));
//
//  nodes := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:ReceivableSpecifiedTradeAccountingAccount');
//  for i := 0 to nodes.length-1 do
//  begin
//    var item : TZUGFeRDReceivableSpecifiedTradeAccountingAccount :=
//      TZUGFeRDReceivableSpecifiedTradeAccountingAccount.Create;
//    item.TradeAccountID := _nodeAsString(nodes[i], './/ram:ID');
//    item.TradeAccountTypeCode := TTEnumExtensions<TZUGFeRDAccountingAccountTypeCodes>.StringToEnum(_nodeAsString(nodes[i], './/ram:TypeCode'));
//    Result.ReceivableSpecifiedTradeAccountingAccounts.Add(item);
//  end;
//
//  Result.OrderDate.SetValue(_nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:BuyerOrderReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString'));
//  Result.OrderNo := _nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:BuyerOrderReferencedDocument/ram:IssuerAssignedID');
//
//  // Read SellerOrderReferencedDocument
//  node := doc.SelectSingleNode('//ram:ApplicableHeaderTradeAgreement/ram:SellerOrderReferencedDocument');
//  if node <> nil then
//  begin
//    Result.SellerOrderReferencedDocument := TZUGFeRDSellerOrderReferencedDocument.Create;
//    Result.SellerOrderReferencedDocument.ID := _nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:SellerOrderReferencedDocument/ram:IssuerAssignedID');
//    Result.SellerOrderReferencedDocument.IssueDateTime.SetValue(_nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:SellerOrderReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString'));
//  end;
//                retval.SellerOrderReferencedDocument = new SellerOrderReferencedDocument()
//                {
//                    ID = XmlUtils.NodeAsString(doc.DocumentElement, "//cac:OrderReference/cbc:SalesOrderID", nsmgr),
//                    // unclear how to map
//                    //    IssueDateTime = XmlUtils.NodeAsDateTime(tradeLineItem, ".//ram:SpecifiedLineTradeAgreement/ram:BuyerOrderReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString", nsmgr),
//                    //    LineID = XmlUtils.NodeAsString(tradeLineItem, ".//ram:SpecifiedSupplyChainTradeAgreement/ram:BuyerOrderReferencedDocument/ram:LineID", nsmgr),
//                };
//
//  // Read ContractReferencedDocument
//  if (doc.SelectSingleNode('//ram:ApplicableHeaderTradeAgreement/ram:ContractReferencedDocument') <> nil) then
//  begin
//    Result.ContractReferencedDocument := TZUGFeRDContractReferencedDocument.Create;
//    Result.ContractReferencedDocument.ID := _nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:ContractReferencedDocument/ram:IssuerAssignedID');
//    Result.ContractReferencedDocument.IssueDateTime.SetValue(_nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:ContractReferencedDocument/ram:FormattedIssueDateTime'));
//  end;
//
//  Result.SpecifiedProcuringProject := TZUGFeRDSpecifiedProcuringProject.Create;
//  Result.SpecifiedProcuringProject.ID := _nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:SpecifiedProcuringProject/ram:ID');
//  Result.SpecifiedProcuringProject.Name := _nodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:SpecifiedProcuringProject/ram:Name');
//
//  nodes := doc.SelectNodes('//ram:IncludedSupplyChainTradeLineItem');
//  for i := 0 to nodes.length-1 do
//    Result.TradeLineItems.Add(_parseTradeLineItem(nodes[i]));
end;

function TZUGFeRDInvoiceDescriptor22UBLReader._getAdditionalReferencedDocument(
  a_oXmlNode: IXmlDomNode): TZUGFeRDAdditionalReferencedDocument;
begin
  Result:= Nil; // avoid warnig until implementation is complete
//  var strBase64BinaryData : String := _nodeAsString(a_oXmlNode, 'ram:AttachmentBinaryObject');
//  Result := TZUGFeRDAdditionalReferencedDocument.Create(false);
//  Result.ID := _nodeAsString(a_oXmlNode, 'ram:IssuerAssignedID');
//  Result.TypeCode := TTEnumExtensions<TZUGFeRDAdditionalReferencedDocumentTypeCodes>.StringToEnum(_nodeAsString(a_oXmlNode, 'ram:TypeCode'));
//  Result.Name := _nodeAsString(a_oXmlNode, 'ram:Name');
//  Result.IssueDateTime.SetValue(_nodeAsDateTime(a_oXmlNode, 'ram:FormattedIssueDateTime/qdt:DateTimeString'));
//  if strBase64BinaryData <> '' then
//  begin
//    Result.AttachmentBinaryObject := TMemoryStream.Create;
//    var strBase64BinaryDataBytes : TBytes := TNetEncoding.Base64String.DecodeStringToBytes(strBase64BinaryData);
//    Result.AttachmentBinaryObject.Write(strBase64BinaryDataBytes,Length(strBase64BinaryDataBytes));
//  end;
//  Result.Filename := _nodeAsString(a_oXmlNode, 'ram:AttachmentBinaryObject/@filename');
//  Result.ReferenceTypeCode := TTEnumExtensions<TZUGFeRDReferenceTypeCodes>.StringToEnum(_nodeAsString(a_oXmlNode, 'ram:ReferenceTypeCode'));
end;

function TZUGFeRDInvoiceDescriptor22UBLReader._nodeAsLegalOrganization(
  basenode: IXmlDomNode; const xpath: string) : TZUGFeRDLegalOrganization;
//var
//  node : IXmlDomNode;
begin
  Result:= Nil; // avoid warnig until implementation is complete
//  Result := nil;
//  if (baseNode = nil) then
//    exit;
//  node := baseNode.SelectSingleNode(xpath);
//  if (node = nil) then
//    exit;
//  Result := TZUGFeRDLegalOrganization.CreateWithParams(
//               TTEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.StringToEnum(_nodeAsString(node, 'ram:ID/@schemeID')),
//               _nodeAsString(node, 'ram:ID'),
//               _nodeAsString(node, 'ram:TradingBusinessName'));
end;

function TZUGFeRDInvoiceDescriptor22UBLReader._nodeAsParty(basenode: IXmlDomNode;
  const xpath: string) : TZUGFeRDParty;
//var
//  node : IXmlDomNode;
//  lineOne,lineTwo : String;
begin
  Result := nil;
//  if (baseNode = nil) then
//    exit;
//  node := baseNode.SelectSingleNode(xpath);
//  if (node = nil) then
//    exit;
//  Result := TZUGFeRDParty.Create;
//  Result.ID.ID := _nodeAsString(node, 'ram:ID');
//  Result.ID.SchemeID := TZUGFeRDGlobalIDSchemeIdentifiers.Unknown;
//  Result.GlobalID.ID := _nodeAsString(node, 'ram:GlobalID');
//  Result.GlobalID.SchemeID := TTEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.StringToEnum(_nodeAsString(node, 'ram:GlobalID/@schemeID'));
//  Result.Name := _nodeAsString(node, 'ram:Name');
//  Result.Postcode := _nodeAsString(node, 'ram:PostalTradeAddress/ram:PostcodeCode');
//  Result.City := _nodeAsString(node, 'ram:PostalTradeAddress/ram:CityName');
//  Result.Country := TTEnumExtensions<TZUGFeRDCountryCodes>.StringToEnum(_nodeAsString(node, 'ram:PostalTradeAddress/ram:CountryID'));
//  Result.SpecifiedLegalOrganization := _nodeAsLegalOrganization(node, 'ram:SpecifiedLegalOrganization');
//
//  lineOne := _nodeAsString(node, 'ram:PostalTradeAddress/ram:LineOne');
//  lineTwo := _nodeAsString(node, 'ram:PostalTradeAddress/ram:LineTwo');
//
//  if (not lineTwo.IsEmpty) then
//  begin
//    Result.ContactName := lineOne;
//    Result.Street := lineTwo;
//  end else
//  begin
//    Result.Street := lineOne;
//    Result.ContactName := '';
//  end;
//  Result.AddressLine3 := _nodeAsString(node, 'ram:PostalTradeAddress/ram:LineThree');
//  Result.CountrySubdivisionName := _nodeAsString(node, 'ram:PostalTradeAddress/ram:CountrySubDivisionName');
end;

function TZUGFeRDInvoiceDescriptor22UBLReader._parseTradeLineItem(
  tradeLineItem: IXmlDomNode): TZUGFeRDTradeLineItem;
//var
//  nodes : IXMLDOMNodeList;
//  i : Integer;
begin
  Result := nil;
//
//  if (tradeLineItem = nil) then
//    exit;
//
//  Result := TZUGFeRDTradeLineItem.Create;
//
//  Result.GlobalID.ID := _nodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:GlobalID');
//  Result.GlobalID.SchemeID := TTEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.StringToEnum(_nodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:GlobalID/@schemeID'));
//  Result.SellerAssignedID := _nodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:SellerAssignedID');
//  Result.BuyerAssignedID := _nodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:BuyerAssignedID');
//  Result.Name := _nodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:Name');
//  Result.Description := _nodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:Description');
//  Result.UnitQuantity.SetValue(_nodeAsDecimal(tradeLineItem, './/ram:BasisQuantity', 1));
//  Result.BilledQuantity := _nodeAsDecimal(tradeLineItem, './/ram:BilledQuantity', 0);
//  Result.LineTotalAmount.SetValue(_nodeAsDecimal(tradeLineItem, './/ram:LineTotalAmount', 0));
//  Result.TaxCategoryCode := TTEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum(_nodeAsString(tradeLineItem, './/ram:ApplicableTradeTax/ram:CategoryCode'));
//  Result.TaxType := TTEnumExtensions<TZUGFeRDTaxTypes>.StringToEnum(_nodeAsString(tradeLineItem, './/ram:ApplicableTradeTax/ram:TypeCode'));
//  Result.TaxPercent := _nodeAsDecimal(tradeLineItem, './/ram:ApplicableTradeTax/ram:RateApplicablePercent', 0);
//  Result.NetUnitPrice.SetValue(_nodeAsDecimal(tradeLineItem, './/ram:NetPriceProductTradePrice/ram:ChargeAmount', 0));
//  Result.GrossUnitPrice.SetValue(_nodeAsDecimal(tradeLineItem, './/ram:GrossPriceProductTradePrice/ram:ChargeAmount', 0));
//  Result.UnitCode := TTEnumExtensions<TZUGFeRDQuantityCodes>.StringToEnum(_nodeAsString(tradeLineItem, './/ram:BasisQuantity/@unitCode'));
//  Result.BillingPeriodStart.SetValue(_nodeAsDateTime(tradeLineItem, './/ram:BillingSpecifiedPeriod/ram:StartDateTime/udt:DateTimeString'));
//  Result.BillingPeriodEnd.SetValue(_nodeAsDateTime(tradeLineItem, './/ram:BillingSpecifiedPeriod/ram:EndDateTime/udt:DateTimeString'));
//
//  nodes := tradeLineItem.SelectNodes('.//ram:SpecifiedTradeProduct/ram:ApplicableProductCharacteristic');
//  for i := 0 to nodes.length-1 do
//  begin
//    var apcItem : TZUGFeRDApplicableProductCharacteristic := TZUGFeRDApplicableProductCharacteristic.Create;
//    apcItem.Description := _nodeAsString(nodes[i], './/ram:Description');
//    apcItem.Value := _nodeAsString(nodes[i], './/ram:Value');
//    Result.ApplicableProductCharacteristics.Add(apcItem);
//  end;
//
//  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeAgreement/ram:BuyerOrderReferencedDocument') <> nil) then
//  begin
//    Result.BuyerOrderReferencedDocument := TZUGFeRDBuyerOrderReferencedDocument.Create;
//    Result.BuyerOrderReferencedDocument.ID := _nodeAsString(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:BuyerOrderReferencedDocument/ram:IssuerAssignedID');
//    Result.BuyerOrderReferencedDocument.IssueDateTime.SetValue(_nodeAsDateTime(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:BuyerOrderReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString'));
//   LineID
//  end;
//
//  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeAgreement/ram:ContractReferencedDocument') <> nil) then
//  begin
//    Result.ContractReferencedDocument := TZUGFeRDContractReferencedDocument.Create;
//    Result.ContractReferencedDocument.ID := _nodeAsString(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:ContractReferencedDocument/ram:IssuerAssignedID');
//    Result.ContractReferencedDocument.IssueDateTime.SetValue(_nodeAsDateTime(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:ContractReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString'));
//  end;
//
//  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeSettlement') <> nil) then
//  begin
//    nodes := tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeSettlement').ChildNodes;
//    for i := 0 to nodes.length-1 do
//    begin
//      if SameText(nodes[i].nodeName,'ram:ApplicableTradeTax') then
//      begin
//        //TODO
//      end else
//      if SameText(nodes[i].nodeName,'ram:BillingSpecifiedPeriod') then
//      begin
//        //TODO
//      end else
//      if SameText(nodes[i].nodeName,'ram:SpecifiedTradeAllowanceCharge') then
//      begin
//        //TODO
//      end else
//      if SameText(nodes[i].nodeName,'ram:SpecifiedTradeSettlementLineMonetarySummation') then
//      begin
//        //TODO
//      end else
//      if SameText(nodes[i].nodeName,'ram:AdditionalReferencedDocument') then
//      begin
//        //TODO
//      end else
//      if SameText(nodes[i].nodeName,'ram:ReceivableSpecifiedTradeAccountingAccount') then
//      begin
//        var rstaaItem : TZUGFeRDReceivableSpecifiedTradeAccountingAccount :=
//          TZUGFeRDReceivableSpecifiedTradeAccountingAccount.Create;
//        rstaaItem.TradeAccountID := _nodeAsString(nodes[i], './/ram:ID');
//        rstaaItem.TradeAccountTypeCode := TTEnumExtensions<TZUGFeRDAccountingAccountTypeCodes>.StringToEnum(_nodeAsString(nodes[i], './/ram:TypeCode'));
//        Result.ReceivableSpecifiedTradeAccountingAccounts.Add(rstaaItem);
//      end;
//    end;
//  end;
//
//  if (tradeLineItem.SelectSingleNode('.//ram:AssociatedDocumentLineDocument') <> nil) then
//  begin
//    Result.AssociatedDocument := TZUGFeRDAssociatedDocument.Create(_nodeAsString(tradeLineItem, './/ram:AssociatedDocumentLineDocument/ram:LineID'));
//linestatuscode ..
//
//    nodes := tradeLineItem.SelectNodes('.//ram:AssociatedDocumentLineDocument/ram:IncludedNote');
//    for i := 0 to nodes.length-1 do
//    begin
//      var noteItem : TZUGFeRDNote := TZUGFeRDNote.Create(
//          _nodeAsString(nodes[i], './/ram:Content'),
//          TTEnumExtensions<TZUGFeRDSubjectCodes>.StringToEnum(_nodeAsString(nodes[i], './/ram:SubjectCode')),
//          TZUGFeRDContentCodes.Unknown
//        );
//      Result.AssociatedDocument.Notes.Add(noteItem);
//    end;
//  end;
//
//  nodes := tradeLineItem.SelectNodes('.//ram:SpecifiedLineTradeAgreement/ram:GrossPriceProductTradePrice/ram:AppliedTradeAllowanceCharge');
//  for i := 0 to nodes.length-1 do
//  begin
//
//    var chargeIndicator : Boolean := _nodeAsBool(nodes[i], './ram:ChargeIndicator/udt:Indicator');
//    var basisAmount : Currency := _nodeAsDecimal(nodes[i], './ram:BasisAmount',0);
//    var basisAmountCurrency : String := _nodeAsString(nodes[i], './ram:BasisAmount/@currencyID');
//    var actualAmount : Currency := _nodeAsDecimal(nodes[i], './ram:ActualAmount',0);
//    var actualAmountCurrency : String := _nodeAsString(nodes[i], './ram:ActualAmount/@currencyID');
//    var reason : String := _nodeAsString(nodes[i], './ram:Reason');
//    var chargePercentage : Currency := _nodeAsDecimal(appliedTradeAllowanceChargeNode, './ram:CalculationPercent',0);
//
//    Result.AddTradeAllowanceCharge(not chargeIndicator, // wichtig: das not beachten
//                                    TTEnumExtensions<TZUGFeRDCurrencyCodes>.StringToEnum(basisAmountCurrency),
//                                    basisAmount,
//                                    actualAmount,
//                                    chargePercentage,
//                                    reason);
//  end;
//
//  if (Result.UnitCode = TZUGFeRDQuantityCodes.Unknown) then
//  begin
//    // UnitCode alternativ aus BilledQuantity extrahieren
//    Result.UnitCode := TTEnumExtensions<TZUGFeRDQuantityCodes>.StringToEnum(_nodeAsString(tradeLineItem, './/ram:BilledQuantity/@unitCode'));
//  end;
//
//  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssuerAssignedID') <> nil) then
//  begin
//    Result.DeliveryNoteReferencedDocument := TZUGFeRDDeliveryNoteReferencedDocument.Create;
//    Result.DeliveryNoteReferencedDocument.ID := _nodeAsString(tradeLineItem, './/ram:SpecifiedLineTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssuerAssignedID');
//    Result.DeliveryNoteReferencedDocument.IssueDateTime.SetValue(_nodeAsDateTime(tradeLineItem, './/ram:SpecifiedLineTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString'));
//  end;
//
//  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeDelivery/ram:ActualDeliverySupplyChainEvent/ram:OccurrenceDateTime') <> nil) then
//  begin
//    Result.ActualDeliveryDate.SetValue(_nodeAsDateTime(tradeLineItem, './/ram:SpecifiedLineTradeDelivery/ram:ActualDeliverySupplyChainEvent/ram:OccurrenceDateTime/udt:DateTimeString'));
//  end;
//
//  //if (tradeLineItem.SelectSingleNode(".//ram:SpecifiedLineTradeAgreement/ram:ContractReferencedDocument/ram:IssuerAssignedID", nsmgr) != null)
//  //{
//  //    item.ContractReferencedDocument = new ContractReferencedDocument()
//  //    {
//  //        ID = _nodeAsString(tradeLineItem, ".//ram:SpecifiedLineTradeAgreement/ram:ContractReferencedDocument/ram:IssuerAssignedID", nsmgr),
//  //        IssueDateTime = _nodeAsDateTime(tradeLineItem, ".//ram:SpecifiedLineTradeAgreement/ram:ContractReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString", nsmgr),
//  //    };
//  //}
//
//  //Get all referenced AND embedded documents
//  nodes := tradeLineItem.SelectNodes('.//ram:SpecifiedLineTradeAgreement/ram:AdditionalReferencedDocument');
//  for i := 0 to nodes.length-1 do
//  begin
//    Result.AdditionalReferencedDocuments.Add(_getAdditionalReferencedDocument(nodes[i]));
//  end;
end;

end.
