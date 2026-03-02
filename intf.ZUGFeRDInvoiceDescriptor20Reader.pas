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
  System.SysUtils, System.Classes, System.DateUtils, System.Generics.Collections,
  System.NetEncoding
  ,Xml.XMLDoc, Xml.xmldom, Xml.XMLIntf
  ,Xml.Win.msxmldom, Winapi.MSXMLIntf, Winapi.msxml
  ,intf.ZUGFeRDHelper
  ,intf.ZUGFeRDXmlHelper
  ,intf.ZUGFeRDIInvoiceDescriptorReader
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
  ,intf.ZUGFeRDDateTypeCodes
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
  ,intf.ZUGFeRDXmlUtils
  ,intf.ZUGFeRDTransportmodeCodes
  ,intf.ZUGFeRDChargeReasonCodes
  ,intf.ZUGFeRDAllowanceReasonCodes
  ,intf.ZUGFeRDTradeDeliveryTermCodes
  ,intf.ZUGFeRDIncludedReferencedProduct
  ,intf.ZUGFeRDLineStatusCodes
  ,intf.ZUGFeRDLineStatusReasonCodes
  ;

type
  TZUGFeRDInvoiceDescriptor20Reader = class(TZUGFeRDIInvoiceDescriptorReader)
  private
    function GetValidURIs : TArray<string>;
    function _parseTradeLineItem(tradeLineItem : IXmlDomNode {nsmgr: XmlNamespaceManager := nil; }) : TZUGFeRDTradeLineItem;
    function _nodeAsParty(basenode: IXmlDomNode; const xpath: string) : TZUGFeRDParty;
    function _getAdditionalReferencedDocument(a_oXmlNode : IXmlDomNode {nsmgr: XmlNamespaceManager := nil; }) : TZUGFeRDAdditionalReferencedDocument;
  public
    constructor Create;
    destructor Destroy; override;
    function IsReadableByThisReaderVersion(stream: TStream): Boolean; override;

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

constructor TZUGFeRDInvoiceDescriptor20Reader.Create;
begin
  inherited;
  FNamespaces := TDictionary<string, string>.Create;
  FNamespaces.Add('a', 'urn:un:unece:uncefact:data:standard:QualifiedDataType:100');
  FNamespaces.Add('rsm', 'urn:un:unece:uncefact:data:standard:CrossIndustryInvoice:100');
  FNamespaces.Add('qdt', 'urn:un:unece:uncefact:data:standard:QualifiedDataType:100');
  FNamespaces.Add('ram', 'urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:100');
  FNamespaces.Add('xs', 'http://www.w3.org/2001/XMLSchema');
  FNamespaces.Add('udt', 'urn:un:unece:uncefact:data:standard:UnqualifiedDataType:100');
  //FNamespaces.Add('udt', 'urn:un:unece:uncefact:data:standard:UnqualifiedDataType:15'); TODO pruefen, erst mal auskommentiert wegen Duplikat im Dictionary
end;

destructor TZUGFeRDInvoiceDescriptor20Reader.Destroy;
begin
  FNamespaces.Free;
  inherited;
end;

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
  Result := _IsReadableByThisReaderVersion(stream, GetValidURIs);
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
  // XmlNamespaceManager nsmgr = _CreateFixedNamespaceManager(doc);
  // if not(nsmgr.HasNamespace('rsm')) then
  //   nsmgr.AddNamespace('rsm', nsmgr.DefaultNamespace);

  Result := TZUGFeRDInvoiceDescriptor.Create;

  Result.IsTest := TZUGFeRDXmlUtils.NodeAsBool(doc.documentElement,'//*[local-name()="ExchangedDocumentContext"]/ram:TestIndicator',false);
  Result.BusinessProcess := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//*[local-name()="BusinessProcessSpecifiedDocumentContextParameter"]/ram:ID');//, nsmgr),
  Result.Guideline := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:GuidelineSpecifiedDocumentContextParameter/ram:ID'); //, nsmgr)),
  Result.Profile := TZUGFeRDProfileExtensions.StringToEnum(Result.Guideline);
  Result.Name := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//*[local-name()="ExchangedDocument"]/ram:Name');//, nsmgr)),
  Result.Type_ := TEnumExtensions<TZUGFeRDInvoiceType>.StringToEnum(TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//*[local-name()="ExchangedDocument"]/ram:TypeCode'));//, nsmgr)),
  Result.InvoiceNo := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//*[local-name()="ExchangedDocument"]/ram:ID');//, nsmgr),
  Result.InvoiceDate := TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//*[local-name()="ExchangedDocument"]/ram:IssueDateTime/udt:DateTimeString');//", nsmgr)

  nodes := doc.selectNodes('//*[local-name()="ExchangedDocument"]/ram:IncludedNote');
  for i := 0 to nodes.length-1 do
    Result.AddNote(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Content'),
                   TEnumExtensions<TZUGFeRDSubjectCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:SubjectCode')),
                   TEnumExtensions<TZUGFeRDContentCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ContentCode')));

  Result.ReferenceOrderNo := TZUGFeRDXmlUtils.NodeAsString(doc, '//ram:ApplicableHeaderTradeAgreement/ram:BuyerReference');

  Result.Seller := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:SellerTradeParty');

  nodes := doc.selectNodes('//ram:ApplicableHeaderTradeAgreement/ram:SellerTradeParty/ram:SpecifiedTaxRegistration');
  for i := 0 to nodes.length-1 do
  begin
    var id : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ID');
    var schemeID : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ID/@schemeID');
    Result.AddSellerTaxRegistration(id, TEnumExtensions<TZUGFeRDTaxRegistrationSchemeID>.StringToEnum(schemeID));
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
    Result.AddBuyerTaxRegistration(id, TEnumExtensions<TZUGFeRDTaxRegistrationSchemeID>.StringToEnum(schemeID));
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

  // SellerTaxRepresentativeTradeParty STEUERBEVOLLMÄCHTIGTER DES VERKÄUFERS, BG-11
  Result.SellerTaxRepresentative := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:SellerTaxRepresentativeTradeParty');
  nodes := doc.selectNodes('//ram:ApplicableHeaderTradeAgreement/ram:SellerTaxRepresentativeTradeParty/ram:SpecifiedTaxRegistration');
  for i := 0 to nodes.length-1 do
  begin
    var id : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ID');
    var schemeID : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ID/@schemeID');
    Result.AddSellerTaxRepresentativeTaxRegistration(id, TEnumExtensions<TZUGFeRDTaxRegistrationSchemeID>.StringToEnum(schemeID));
  end;

  //Get all referenced and embedded documents (BG-24)
  nodes := doc.SelectNodes('.//ram:ApplicableHeaderTradeAgreement/ram:AdditionalReferencedDocument');
  for i := 0 to nodes.length-1 do
    Result.AdditionalReferencedDocuments.Add(_getAdditionalReferencedDocument(nodes[i]));

  //Read TransportModeCodes --> BT-X-152
  if (doc.SelectSingleNode('//ram:ApplicableHeaderTradeDelivery/ram:RelatedSupplyChainConsignment/ram:SpecifiedLogisticsTransportMovement/ram:ModeCode') <> nil) then
    Result.TransportMode := TEnumExtensions<TZUGFeRDTransportmodeCodes>.StringToEnum(TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:RelatedSupplyChainConsignment/ram:SpecifiedLogisticsTransportMovement/ram:ModeCode'));

  Result.ShipTo := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:ShipToTradeParty');
  if (doc.selectSingleNode('//ram:ShipToTradeParty/ram:DefinedTradeContact') <> nil) then
  begin
    Result.ShipToContact := TZUGFeRDContact.Create;
    Result.ShipToContact.Name := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ShipToTradeParty/ram:DefinedTradeContact/ram:PersonName');
    Result.ShipToContact.OrgUnit := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ShipToTradeParty/ram:DefinedTradeContact/ram:DepartmentName');
    Result.ShipToContact.PhoneNo := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ShipToTradeParty/ram:DefinedTradeContact/ram:TelephoneUniversalCommunication/ram:CompleteNumber');
    Result.ShipToContact.FaxNo := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ShipToTradeParty/ram:DefinedTradeContact/ram:FaxUniversalCommunication/ram:CompleteNumber');
    Result.ShipToContact.EmailAddress := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ShipToTradeParty/ram:DefinedTradeContact/ram:EmailURIUniversalCommunication/ram:URIID');
  end;

  Result.ShipFrom := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:ShipFromTradeParty');
  Result.ActualDeliveryDate:= TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:ActualDeliverySupplyChainEvent/ram:OccurrenceDateTime/udt:DateTimeString');

  var _deliveryNoteNo : String := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssuerAssignedID');
  var _deliveryNoteDate : ZUGFeRDNullable<TDateTime> := TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssueDateTime/udt:DateTimeString');

  if Not (_deliveryNoteDate.HasValue) then
  begin
    _deliveryNoteDate := TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssueDateTime');
  end;

  if ((_deliveryNoteDate.HasValue) or (_deliveryNoteNo <> '')) then
  begin
    Result.DeliveryNoteReferencedDocument := TZUGFeRDDeliveryNoteReferencedDocument.Create;
    Result.SetDeliveryNoteReferenceDocument(_deliveryNoteNo,_deliveryNoteDate);
  end;

  Result.Invoicee := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:InvoiceeTradeParty');
  nodes := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:InvoiceeTradeParty/ram:SpecifiedTaxRegistration');
  for i := 0 to nodes.length-1 do
    Result.AddInvoiceeTaxRegistration(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ID'),
                                      TEnumExtensions<TZUGFeRDTaxRegistrationSchemeID>.StringToEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ID/@schemeID')));

  Result.Invoicer := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:InvoicerTradeParty');
  if (doc.selectSingleNode('//ram:InvoicerTradeParty/ram:DefinedTradeContact') <> nil) then
  begin
    Result.InvoicerContact := TZUGFeRDContact.Create;
    Result.InvoicerContact.Name := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:InvoicerTradeParty/ram:DefinedTradeContact/ram:PersonName');
    Result.InvoicerContact.OrgUnit := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:InvoicerTradeParty/ram:DefinedTradeContact/ram:DepartmentName');
    Result.InvoicerContact.PhoneNo := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:InvoicerTradeParty/ram:DefinedTradeContact/ram:TelephoneUniversalCommunication/ram:CompleteNumber');
    Result.InvoicerContact.FaxNo := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:InvoicerTradeParty/ram:DefinedTradeContact/ram:FaxUniversalCommunication/ram:CompleteNumber');
    Result.InvoicerContact.EmailAddress := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:InvoicerTradeParty/ram:DefinedTradeContact/ram:EmailURIUniversalCommunication/ram:URIID');
  end;

  Result.Payee := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:PayeeTradeParty');

  Result.PaymentReference := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:PaymentReference');
  Result.Currency :=  TEnumExtensions<TZUGFeRDCurrencyCodes>.StringToEnum(TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:InvoiceCurrencyCode'));
  Result.SellerReferenceNo := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:InvoiceIssuerReference');

  // TODO: Multiple SpecifiedTradeSettlementPaymentMeans can exist for each account/institution (with different SEPA?)
  Result.PaymentMeans := TZUGFeRDPaymentMeans.Create;
  Result.PaymentMeans.TypeCode := TEnumExtensions<TZUGFeRDPaymentMeansTypeCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:TypeCode'));
  Result.PaymentMeans.Information := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:Information');
  Result.PaymentMeans.SEPACreditorIdentifier := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:CreditorReferenceID');
  Result.PaymentMeans.SEPAMandateReference := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:SpecifiedTradePaymentTerms/ram:DirectDebitMandateID');

  var financialCardId : String := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:ApplicableTradeSettlementFinancialCard/ram:ID');
  var financialCardCardholderName : String := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:ApplicableTradeSettlementFinancialCard/ram:CardholderName');

  if ((financialCardId <> '') or (financialCardCardholderName <> '')) then
  begin
    Result.PaymentMeans.FinancialCard := TZUGFeRDFinancialCard.Create;
    Result.PaymentMeans.FinancialCard.Id := financialCardId;
    Result.PaymentMeans.FinancialCard.CardholderName := financialCardCardholderName;
  end;

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
          _account.BIC := TZUGFeRDXmlUtils.NodeAsString(payerSpecifiedDebtorFinancialInstitutionNode, './/ram:BICID');

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
    Result.AddApplicableTradeTax(TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:CalculatedAmount', 0),
                                 TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:BasisAmount', 0),
                                 TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:RateApplicablePercent', 0),
                                 TEnumExtensions<TZUGFeRDTaxTypes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:TypeCode')),
                                 TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:CategoryCode')),
                                 TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:AllowanceChargeBasisAmount'),
                                 TEnumExtensions<TZUGFeRDTaxExemptionReasonCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ExemptionReasonCode')),
                                 TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ExemptionReason'),
                                 TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:LineTotalBasisAmount')
                                ).SetTaxPointDate(TZUGFeRDXmlUtils.NodeAsDateTime(nodes[i], './/ram:TaxPointDate/udt:DateString'),
                                                  TEnumExtensions<TZUGFeRDDateTypeCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:DueDateTypeCode')));

  nodes := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeAllowanceCharge');
  for i := 0 to nodes.length-1 do
  begin
    var chargePercentage: ZUGFeRDNullable<Currency> :=TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:CalculationPercent');
    var basisAmount: ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:BasisAmount');
    var actualAmount: ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:ActualAmount', 0);
    var reason: string := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Reason');
    var taxTypeCode: ZUGFeRDNullable<TZUGFeRDTaxTypes> := TEnumExtensions<TZUGFeRDTaxTypes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:CategoryTradeTax/ram:TypeCode'));
    var taxCategoryCode: ZUGFeRDNullable<TZUGFeRDTaxCategoryCodes> := TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:CategoryTradeTax/ram:CategoryCode'));
    var taxPercent: ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:CategoryTradeTax/ram:RateApplicablePercent', 0);
    if TZUGFeRDXmlUtils.NodeAsBool(nodes[i], './/ram:ChargeIndicator') then
    begin
      var chargeReasonCode: ZUGFeRDNullable<TZUGFeRDChargeReasonCodes> := TEnumExtensions<TZUGFeRDChargeReasonCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:ReasonCode'));
      Result.AddTradeCharge(basisAmount, Result.Currency, actualAmount, chargePercentage, reason, taxTypeCode, taxCategoryCode, taxPercent, chargeReasonCode);
   end
    else
    begin
      var allowanceReasonCode: ZUGFeRDNullable<TZUGFeRDAllowanceReasonCodes> := TEnumExtensions<TZUGFeRDAllowanceReasonCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:ReasonCode'));
      Result.AddTradeAllowance(basisAmount, Result.Currency, actualAmount, chargePercentage, reason, taxTypeCode, taxCategoryCode, taxPercent, allowanceReasonCode);
    end;
  end;

  nodes := doc.SelectNodes('//ram:SpecifiedLogisticsServiceCharge');
  for i := 0 to nodes.length-1 do
  begin
    Result.AddLogisticsServiceCharge(TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:AppliedAmount', 0),
                                     TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Description'),
                                     TEnumExtensions<TZUGFeRDTaxTypes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:AppliedTradeTax/ram:TypeCode')),
                                     TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:AppliedTradeTax/ram:CategoryCode')),
                                     TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:AppliedTradeTax/ram:RateApplicablePercent', 0));
  end;

  nodes := doc.SelectNodes('//ram:SpecifiedTradePaymentTerms');
  for i := 0 to nodes.length-1 do
  begin
    var paymentTerm : TZUGFeRDPaymentTerms := TZUGFeRDPaymentTerms.Create;
    paymentTerm.Description := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Description');
    paymentTerm.DueDate:= TZUGFeRDXmlUtils.NodeAsDateTime(nodes[i], './/ram:DueDateDateTime/udt:DateTimeString');
    paymentTerm.PartialPaymentAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:PartialPaymentAmount');
    var discountPercent: ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:ApplicableTradePaymentDiscountTerms/ram:CalculationPercent');
    var penaltyPercent: ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:ApplicableTradePaymentPenaltyTerms/ram:CalculationPercent');
    var Days: ZUGFeRDNullable<Integer>;
    if discountPercent.HasValue then
    begin
      paymentTerm.PaymentTermsType:= TZUGFeRDPaymentTermsType.Skonto;
      paymentTerm.Percentage:= discountPercent;
      if TZUGFeRDQuantityCodes.DAY = TEnumExtensions<TZUGFeRDQuantityCodes>.StringToEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ApplicableTradePaymentDiscountTerms/ram:BasisPeriodMeasure/@unitCode')) then
        paymentTerm.DueDays:= TZUGFeRDXmlUtils.NodeAsInt(nodes[i], './/ram:ApplicableTradePaymentDiscountTerms/ram:BasisPeriodMeasure');
      paymentTerm.MaturityDate:= TZUGFeRDXmlUtils.NodeAsDateTime(nodes[i], './/ram:ApplicableTradePaymentDiscountTerms/ram:BasisDateTime/udt:DateTimeString'); //BT-X-282-0
      paymentTerm.BaseAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:ApplicableTradePaymentDiscountTerms/ram:BasisAmount');
      paymentTerm.ActualAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:ApplicableTradePaymentDiscountTerms/ram:ActualDiscountAmount');
    end
    else
    if penaltyPercent.HasValue then
    begin
      paymentTerm.PaymentTermsType:= TZUGFeRDPaymentTermsType.Verzug;
      paymentTerm.Percentage:= penaltyPercent;
      if TZUGFeRDQuantityCodes.DAY = TEnumExtensions<TZUGFeRDQuantityCodes>.StringToEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ApplicableTradePaymentPenaltyTerms/ram:BasisPeriodMeasure/@unitCode')) then
        paymentTerm.DueDays:= TZUGFeRDXmlUtils.NodeAsInt(nodes[i], './/ram:ApplicableTradePaymentPenaltyTerms/ram:BasisPeriodMeasure');
      paymentTerm.MaturityDate:= TZUGFeRDXmlUtils.NodeAsDateTime(nodes[i], './/ram:ApplicableTradePaymentPenaltyTerms/ram:BasisDateTime/udt:DateTimeString'); // BT-X-276-0
      paymentTerm.BaseAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:ApplicableTradePaymentPenaltyTerms/ram:BasisAmount');
      paymentTerm.ActualAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:ApplicableTradePaymentPenaltyTerms/ram:ActualPenaltyAmount');
    end;

    Result.PaymentTermsList.Add(paymentTerm);
  end;

  Result.LineTotalAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:LineTotalAmount');
  Result.ChargeTotalAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:ChargeTotalAmount');
  Result.AllowanceTotalAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:AllowanceTotalAmount');
  Result.TaxBasisAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:TaxBasisTotalAmount');
  Result.TaxTotalAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:TaxTotalAmount');
  Result.GrandTotalAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:GrandTotalAmount');
  Result.RoundingAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:RoundingAmount');
  Result.TotalPrepaidAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:TotalPrepaidAmount');
  Result.DuePayableAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:DuePayableAmount');

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

  var deliveryCodeStr: string := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:ApplicableTradeDeliveryTerms/ram:DeliveryTypeCode');
  if deliveryCodeStr <> '' then
  begin
    var tradeCode:= TEnumExtensions<TZUGFeRDTradeDeliveryTermCodes>.StringToNullableEnum(deliveryCodeStr);
    if tradeCode<>Nil then
      Result.ApplicableTradeDeliveryTermsCode:= tradeCode;
  end;

  // SellerOrderReferencedDocument
  node := doc.SelectSingleNode('//ram:ApplicableHeaderTradeAgreement/ram:SellerOrderReferencedDocument');
  if node <> nil then
  begin
    Result.SellerOrderReferencedDocument := TZUGFeRDSellerOrderReferencedDocument.Create;
    Result.SellerOrderReferencedDocument.ID := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:SellerOrderReferencedDocument/ram:IssuerAssignedID');
    Result.SellerOrderReferencedDocument.IssueDateTime:= TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:SellerOrderReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString');
  end;
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

  var lineId: string := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:AssociatedDocumentLineDocument/ram:LineID');
  var lineStatusCode: ZUGFeRDNullable<TZUGFeRDLineStatusCodes> := TEnumExtensions<TZUGFeRDLineStatusCodes>.StringToNullableEnum(
    TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:AssociatedDocumentLineDocument/ram:LineStatusCode'));
  var lineStatusReasonCode: ZUGFeRDNullable<TZUGFeRDLineStatusReasonCodes> := TEnumExtensions<TZUGFeRDLineStatusReasonCodes>.StringToNullableEnum(
    TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:AssociatedDocumentLineDocument/ram:LineStatusReasonCode'));

  Result := TZUGFeRDTradeLineItem.Create(lineId);

  if lineStatusCode.HasValue and lineStatusReasonCode.HasValue then
    Result.SetLineStatus(lineStatusCode.Value, lineStatusReasonCode.Value);

  Result.GlobalID.SchemeID := TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:GlobalID/@schemeID'));
  Result.GlobalID.ID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:GlobalID');
  Result.SellerAssignedID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:SellerAssignedID');
  Result.BuyerAssignedID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:BuyerAssignedID');
  Result.Name := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:Name');
  Result.Description := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:Description');
  Result.BilledQuantity := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:BilledQuantity', 0);
  Result.UnitCode := TEnumExtensions<TZUGFeRDQuantityCodes>.StringToEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:BilledQuantity/@unitCode'));
  Result.PackageQuantity := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:PackageQuantity');
  Result.PackageUnitCode := TEnumExtensions<TZUGFeRDQuantityCodes>.StringToEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:PackageQuantity/@unitCode'));
  Result.ChargeFreeQuantity := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:ChargeFreeQuantity');
  Result.ChargeFreeUnitCode := TEnumExtensions<TZUGFeRDQuantityCodes>.StringToEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:ChargeFreeQuantity/@unitCode'));
  Result.LineTotalAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:LineTotalAmount');
  Result.TaxCategoryCode := TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:ApplicableTradeTax/ram:CategoryCode'));
  Result.TaxType := TEnumExtensions<TZUGFeRDTaxTypes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:ApplicableTradeTax/ram:TypeCode'));
  Result.TaxPercent := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:ApplicableTradeTax/ram:RateApplicablePercent', 0);
  Result.NetUnitPrice:= TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:NetPriceProductTradePrice/ram:ChargeAmount');
  Result.NetQuantity := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:NetPriceProductTradePrice/ram:BasisQuantity');
  Result.NetUnitCode := TEnumExtensions<TZUGFeRDQuantityCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:NetPriceProductTradePrice/ram:BasisQuantity/@unitCode'));
  Result.GrossUnitPrice:= TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:GrossPriceProductTradePrice/ram:ChargeAmount');
  Result.GrossQuantity := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:GrossPriceProductTradePrice/ram:BasisQuantity');
  Result.GrossUnitCode := TEnumExtensions<TZUGFeRDQuantityCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:GrossPriceProductTradePrice/ram:BasisQuantity/@unitCode'));
  Result.BillingPeriodStart := TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:BillingSpecifiedPeriod/ram:StartDateTime/udt:DateTimeString');
  Result.BillingPeriodEnd := TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:BillingSpecifiedPeriod/ram:EndDateTime/udt:DateTimeString');

  nodes := tradeLineItem.SelectNodes('.//ram:SpecifiedTradeProduct/ram:ApplicableProductCharacteristic');
  for i := 0 to nodes.length-1 do
  begin
    var apcItem : TZUGFeRDApplicableProductCharacteristic := TZUGFeRDApplicableProductCharacteristic.Create;
    apcItem.Description := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Description');
    apcItem.Value := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Value');
    Result.ApplicableProductCharacteristics.Add(apcItem);
  end;

  nodes := tradeLineItem.SelectNodes('.//ram:SpecifiedTradeProduct/ram:IncludedReferencedProduct');
  for i := 0 to nodes.length-1 do
  begin
    var IncludedReferenceProduct: TZUGFeRDIncludedReferencedProduct:= TZUGFeRDIncludedReferencedProduct.Create;
    IncludedReferenceProduct.GlobalID.SchemeID := TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:GlobalID/@schemeID'));
    IncludedReferenceProduct.GlobalID.ID := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:GlobalID');
    IncludedReferenceProduct.SellerAssignedID := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:SellerAssignedID');
    IncludedReferenceProduct.BuyerAssignedID := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:BuyerAssignedID');
    IncludedReferenceProduct.IndustryAssignedID := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:IndustryAssignedID');
    IncludedReferenceProduct.Name:= TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Name');
    IncludedReferenceProduct.Description := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Description');
    IncludedReferenceProduct.UnitQuantity:= TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:UnitQuantity');
    IncludedReferenceProduct.UnitCode:= TEnumExtensions<TZUGFeRDQuantityCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:UnitQuantity/@unitCode'));
    Result.IncludedReferencedProducts.Add(IncludedReferenceProduct);
  end;

  if tradeLineItem.SelectSingleNode('.//ram:AssociatedDocumentLineDocument') <> nil then
  begin
    nodes := tradeLineItem.SelectNodes('.//ram:AssociatedDocumentLineDocument/ram:IncludedNote');
    for i := 0 to nodes.length-1 do
    begin
      var noteItem : TZUGFeRDNote := TZUGFeRDNote.Create(
          TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Content'),
          TEnumExtensions<TZUGFeRDSubjectCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:SubjectCode')),
          TEnumExtensions<TZUGFeRDContentCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ContentCode'))
        );
      Result.AssociatedDocument.Notes.Add(noteItem);
    end;
  end;

  nodes := tradeLineItem.SelectNodes('.//ram:SpecifiedLineTradeAgreement/ram:GrossPriceProductTradePrice/ram:AppliedTradeAllowanceCharge');
  for i := 0 to nodes.length-1 do
  begin

    var chargeIndicator : Boolean := TZUGFeRDXmlUtils.NodeAsBool(nodes[i], './ram:ChargeIndicator/udt:Indicator');
    var basisAmount : ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './ram:BasisAmount');
    var basisAmountCurrency : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:BasisAmount/@currencyID');
    var actualAmount : Currency := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './ram:ActualAmount',0);
    var actualAmountCurrency : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:ActualAmount/@currencyID');
    var reason : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:Reason');

    if chargeIndicator then // charge
      Result.AddTradeCharge(TEnumExtensions<TZUGFeRDCurrencyCodes>.StringToEnum(basisAmountCurrency),
                                    basisAmount,
                                    actualAmount,
                                    reason)
    else // allowance
      Result.AddTradeAllowance(TEnumExtensions<TZUGFeRDCurrencyCodes>.StringToEnum(basisAmountCurrency),
                                    basisAmount,
                                    actualAmount,
                                    reason);
  end;

  if not Result.UnitCode.HasValue then
    // UnitCode alternativ aus BilledQuantity extrahieren
    Result.UnitCode := TEnumExtensions<TZUGFeRDQuantityCodes>.StringToEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:BilledQuantity/@unitCode'));

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
    Result.DeliveryNoteReferencedDocument.LineID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedLineTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:LineID');
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeDelivery/ram:ActualDeliverySupplyChainEvent/ram:OccurrenceDateTime') <> nil) then
    Result.ActualDeliveryDate:= TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:SpecifiedLineTradeDelivery/ram:ActualDeliverySupplyChainEvent/ram:OccurrenceDateTime/udt:DateTimeString');

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeAgreement/ram:ContractReferencedDocument/ram:IssuerAssignedID') <> nil) then
  begin
    Result.ContractReferencedDocument := TZUGFeRDContractReferencedDocument.Create;
    Result.ContractReferencedDocument.ID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:ContractReferencedDocument/ram:IssuerAssignedID');
    Result.ContractReferencedDocument.IssueDateTime:= TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:ContractReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString');
    Result.ContractReferencedDocument.LineID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:ContractReferencedDocument/ram:LineID');
  end;

  //Get all referenced AND embedded documents
  nodes := tradeLineItem.SelectNodes('.//ram:SpecifiedLineTradeAgreement/ram:AdditionalReferencedDocument');
  for i := 0 to nodes.length-1 do
    Result.AdditionalReferencedDocuments.Add(_getAdditionalReferencedDocument(nodes[i]));
end;

function TZUGFeRDInvoiceDescriptor20Reader._nodeAsParty(basenode: IXmlDomNode; const xpath: string) : TZUGFeRDParty;
var
  node : IXmlDomNode;
  lineOne, lineTwo : String;
begin
  Result := nil;
  if (baseNode = nil) then
    exit;
  node := baseNode.SelectSingleNode(xpath);
  if (node = nil) then
    exit;
  Result := TZUGFeRDParty.Create;
  Result.ID.SchemeID := TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(node, 'ram:ID/@schemeID'));
  Result.ID.ID := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:ID');
  Result.GlobalID.SchemeID := TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(node, 'ram:GlobalID/@schemeID'));
  Result.GlobalID.ID := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:GlobalID');
  Result.Name := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:Name');
  Result.Description := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:Description'); // Seller only BT-33
  Result.Postcode := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:PostalTradeAddress/ram:PostcodeCode');
  Result.City := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:PostalTradeAddress/ram:CityName');
  Result.Country := TEnumExtensions<TZUGFeRDCountryCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(node, 'ram:PostalTradeAddress/ram:CountryID'));

  lineOne:= TZUGFeRDXmlUtils.NodeAsString(node, 'ram:PostalTradeAddress/ram:LineOne');
  lineTwo:= TZUGFeRDXmlUtils.NodeAsString(node, 'ram:PostalTradeAddress/ram:LineTwo');
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

function TZUGFeRDInvoiceDescriptor20Reader._getAdditionalReferencedDocument(a_oXmlNode: IXmlDomNode): TZUGFeRDAdditionalReferencedDocument;
begin
  var strBase64BinaryData : String := TZUGFeRDXmlUtils.NodeAsString(a_oXmlNode, 'ram:AttachmentBinaryObject');
  Result := TZUGFeRDAdditionalReferencedDocument.Create(false);
  Result.ID := TZUGFeRDXmlUtils.NodeAsString(a_oXmlNode, 'ram:IssuerAssignedID');
  Result.TypeCode := TEnumExtensions<TZUGFeRDAdditionalReferencedDocumentTypeCode>.StringToEnum(TZUGFeRDXmlUtils.NodeAsString(a_oXmlNode, 'ram:TypeCode'));
  Result.Name := TZUGFeRDXmlUtils.NodeAsString(a_oXmlNode, 'ram:Name');
  Result.IssueDateTime:= TZUGFeRDXmlUtils.NodeAsDateTime(a_oXmlNode, 'ram:FormattedIssueDateTime/qdt:DateTimeString');
  if strBase64BinaryData <> '' then
  begin
    Result.AttachmentBinaryObject := TMemoryStream.Create;
    var strBase64BinaryDataBytes : TBytes := TNetEncoding.Base64String.DecodeStringToBytes(strBase64BinaryData);
    Result.AttachmentBinaryObject.Write(strBase64BinaryDataBytes,Length(strBase64BinaryDataBytes));
  end;
  Result.Filename := TZUGFeRDXmlUtils.NodeAsString(a_oXmlNode, 'ram:AttachmentBinaryObject/@filename');
  Result.ReferenceTypeCode := TEnumExtensions<TZUGFeRDReferenceTypeCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(a_oXmlNode, 'ram:ReferenceTypeCode'));
end;

end.
