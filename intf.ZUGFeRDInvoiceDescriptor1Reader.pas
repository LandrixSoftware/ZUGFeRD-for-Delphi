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

unit intf.ZUGFeRDInvoiceDescriptor1Reader;

interface

uses
  System.SysUtils, System.Classes, System.DateUtils
  ,Xml.XMLDoc, Xml.XMLIntf, intf.MSXML2_TLB
  ,intf.ZUGFeRDXmlHelper
  ,intf.ZUGFeRDInvoiceDescriptorReader
  ,intf.ZUGFeRDInvoiceDescriptor
  ,intf.ZUGFeRDExceptions
  ,intf.ZUGFeRDTradeLineItem,intf.ZUGFeRDParty
  ,intf.ZUGFeRDProfile,intf.ZUGFeRDInvoiceTypes
  ,intf.ZUGFeRDSubjectCodes
  ,intf.ZUGFeRDGlobalID,intf.ZUGFeRDGlobalIDSchemeIdentifiers
  ,intf.ZUGFeRDCountryCodes
  ,intf.ZUGFeRDTaxRegistrationSchemeID
  ,intf.ZUGFeRDContact
  ,intf.ZUGFeRDDeliveryNoteReferencedDocument
  ,intf.ZUGFeRDCurrencyCodes
  ,intf.ZUGFeRDPaymentMeans,intf.ZUGFeRDPaymentMeansTypeCodes
  ,intf.ZUGFeRDBankAccount
  ,intf.ZUGFeRDTaxTypes,intf.ZUGFeRDTaxCategoryCodes
  ,intf.ZUGFeRDTaxExemptionReasonCodes
  ,intf.ZUGFeRDPaymentTerms
  ,intf.ZUGFeRDQuantityCodes
  ,intf.ZUGFeRDAssociatedDocument
  ,intf.ZUGFeRDNote
  ,intf.ZUGFeRDContentCodes
  ,intf.ZUGFeRDBuyerOrderReferencedDocument
  ,intf.ZUGFeRDReferenceTypeCodes
  ,intf.ZUGFeRDContractReferencedDocument
  ;

type
  TZUGFeRDInvoiceDescriptor1Reader = class(TZUGFeRDInvoiceDescriptorReader)
  private
    function _parseTradeLineItem(tradeLineItem : IXmlDomNode) : TZUGFeRDTradeLineItem;
    function _nodeAsParty(basenode: IXmlDomNode; const xpath: string) : TZUGFeRDParty;
  public
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
  end;

implementation

{ TZUGFeRDInvoiceDescriptor1Reader }

function TZUGFeRDInvoiceDescriptor1Reader.IsReadableByThisReaderVersion(
  stream: TStream): Boolean;
var
  validURIs: TArray<string>;
begin
  validURIs := TArray<string>.Create(
    'urn:ferd:invoice:1.0:basic',
    'urn:ferd:invoice:1.0:comfort',
    'urn:ferd:invoice:1.0:extended',
    'urn:ferd:CrossIndustryDocument:invoice:1p0:basic',
    'urn:ferd:CrossIndustryDocument:invoice:1p0:comfort',
    'urn:ferd:CrossIndustryDocument:invoice:1p0:extended'
  );

  Result := IsReadableByThisReaderVersion(stream, validURIs);
end;

function TZUGFeRDInvoiceDescriptor1Reader.Load(Stream: TStream): TZUGFeRDInvoiceDescriptor;
var
  xml : IXMLDocument;
  doc : IXMLDOMDocument2;
  nodes : IXMLDOMNodeList;
  i : Integer;
begin
  if Stream = nil then
    raise TZUGFeRDIllegalStreamException.Create('Cannot read from stream');

  xml := NewXMLDocument;
  xml.LoadFromStream(stream,TXMLEncodingType.xetUTF_8);
  xml.Active := True;

  doc := TZUGFeRDXmlHelper.PrepareDocumentForXPathQuerys(xml);

  //XmlNamespaceManager nsmgr := new XmlNamespaceManager(doc.DocumentElement.OwnerDocument.NameTable);
  //nsmgr.AddNamespace("rsm", "urn:ferd:CrossIndustryDocument:invoice:1p0");
  //nsmgr.AddNamespace("ram", "urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:12");
  //nsmgr.AddNamespace("udt", "urn:un:unece:uncefact:data:standard:UnqualifiedDataType:15");

  Result := TZUGFeRDInvoiceDescriptor.Create;

  Result.IsTest := _nodeAsBool(doc.documentElement,'//*[local-name()="SpecifiedExchangedDocumentContext"]/ram:TestIndicator');
  Result.BusinessProcess := _nodeAsString(doc.DocumentElement, '//*[local-name()="BusinessProcessSpecifiedDocumentContextParameter"]/ram:ID');//, nsmgr),
  Result.Profile := TZUGFeRDProfileExtensions.FromString(_nodeAsString(doc.DocumentElement, '//ram:GuidelineSpecifiedDocumentContextParameter/ram:ID'));//, nsmgr)),
  Result.Type_ := TZUGFeRDInvoiceTypeExtensions.FromString(_nodeAsString(doc.DocumentElement, '//*[local-name()="HeaderExchangedDocument"]/ram:TypeCode'));//, nsmgr)),
  Result.InvoiceNo := _nodeAsString(doc.DocumentElement, '//*[local-name()="HeaderExchangedDocument"]/ram:ID');//, nsmgr),
  Result.InvoiceDate := _nodeAsDateTime(doc.DocumentElement, '//*[local-name()="HeaderExchangedDocument"]/ram:IssueDateTime/udt:DateTimeString');//", nsmgr)

  nodes := doc.selectNodes('//*[local-name()="HeaderExchangedDocument"]/ram:IncludedNote');
  for i := 0 to nodes.length-1 do
  begin
    var content : String := _nodeAsString(nodes[i], './/ram:Content');
    var _subjectCode : String := _nodeAsString(nodes[i], './/ram:SubjectCode');
    var subjectCode : TZUGFeRDSubjectCodes := TZUGFeRDSubjectCodesExtensions.FromString(_subjectCode);
    Result.AddNote(content, subjectCode);
  end;

  Result.ReferenceOrderNo := _nodeAsString(doc, '//ram:ApplicableSupplyChainTradeAgreement/ram:BuyerReference');

  Result.Seller := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeAgreement/ram:SellerTradeParty');

  nodes := doc.selectNodes('//ram:ApplicableSupplyChainTradeAgreement/ram:SellerTradeParty/ram:SpecifiedTaxRegistration');
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

  Result.Buyer := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeAgreement/ram:BuyerTradeParty');

  nodes := doc.selectNodes('//ram:ApplicableSupplyChainTradeAgreement/ram:BuyerTradeParty/ram:SpecifiedTaxRegistration');
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

  Result.ShipTo := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeDelivery/ram:ShipToTradeParty');
  Result.ShipFrom := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeDelivery/ram:ShipFromTradeParty');
  Result.ActualDeliveryDate.SetValue(_nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeDelivery/ram:ActualDeliverySupplyChainEvent/ram:OccurrenceDateTime/udt:DateTimeString'));

  var _deliveryNoteNo : String := _nodeAsString(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:ID');
  var _deliveryNoteDate : TDateTime := _nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssueDateTime/udt:DateTimeString');

  if (_deliveryNoteDate < 100) then
  begin
    _deliveryNoteDate := _nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssueDateTime');
  end;

  if ((_deliveryNoteDate > 100) or (_deliveryNoteNo <> '')) then
  begin
    Result.DeliveryNoteReferencedDocument := TZUGFeRDDeliveryNoteReferencedDocument.Create;
    Result.SetDeliveryNoteReferenceDocument(_deliveryNoteNo,_deliveryNoteDate);
  end;

  Result.Invoicee := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:InvoiceeTradeParty');
  Result.Payee := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:PayeeTradeParty');

  Result.PaymentReference := _nodeAsString(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:PaymentReference');
  Result.Currency :=  TZUGFeRDCurrencyCodesExtensions.FromString(_nodeAsString(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:InvoiceCurrencyCode'));

  // TODO: Multiple SpecifiedTradeSettlementPaymentMeans can exist for each account/institution (with different SEPA?)
  var _tempPaymentMeans : TZUGFeRDPaymentMeans := TZUGFeRDPaymentMeans.Create;
  _tempPaymentMeans.TypeCode := TZUGFeRDPaymentMeansTypeCodesExtensions.FromString(_nodeAsString(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:TypeCode'));
  _tempPaymentMeans.Information := _nodeAsString(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:Information');
  _tempPaymentMeans.SEPACreditorIdentifier := _nodeAsString(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:ID');
  _tempPaymentMeans.SEPAMandateReference := _nodeAsString(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:ID/@schemeAgencyID');

  Result.PaymentMeans := _tempPaymentMeans;

  //TODO udt:DateTimeString
  Result.BillingPeriodStart := _nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:BillingSpecifiedPeriod/ram:StartDateTime');
  Result.BillingPeriodEnd := _nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:BillingSpecifiedPeriod/ram:EndDateTime');
  //Result.BillingPeriodStart := _nodeAsDateTime(doc.DocumentElement, "//ram:ApplicableHeaderTradeSettlement/ram:BillingSpecifiedPeriod/ram:StartDateTime');
  //Result.BillingPeriodEnd := _nodeAsDateTime(doc.DocumentElement, "//ram:ApplicableHeaderTradeSettlement/ram:BillingSpecifiedPeriod/ram:EndDateTime');

  var creditorFinancialAccountNodes : IXMLDOMNodeList := doc.SelectNodes('//ram:ApplicableSupplyChainTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:PayeePartyCreditorFinancialAccount');
  var creditorFinancialInstitutions : IXMLDOMNodeList := doc.SelectNodes('//ram:ApplicableSupplyChainTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:PayeeSpecifiedCreditorFinancialInstitution');

  if (creditorFinancialAccountNodes.length = creditorFinancialInstitutions.length) then
  for i := 0 to creditorFinancialAccountNodes.length-1 do
  begin
    var _account : TZUGFeRDBankAccount := TZUGFeRDBankAccount.Create;
    _account.ID := _nodeAsString(creditorFinancialAccountNodes[i], './/ram:ProprietaryID');
    _account.IBAN := _nodeAsString(creditorFinancialAccountNodes[i], './/ram:IBANID');
    _account.BIC := _nodeAsString(creditorFinancialInstitutions[i], './/ram:BICID');
    _account.Bankleitzahl := _nodeAsString(creditorFinancialInstitutions[i], './/ram:GermanBankleitzahlID');
    _account.BankName := _nodeAsString(creditorFinancialInstitutions[i], './/ram:Name');
    _account.Name := _nodeAsString(creditorFinancialInstitutions[i], './/ram:AccountName');
    Result.CreditorBankAccounts.Add(_account);
  end;

  var debitorFinancialAccountNodes : IXMLDOMNodeList := doc.SelectNodes('//ram:ApplicableSupplyChainTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:PayerPartyDebtorFinancialAccount');
  var debitorFinancialInstitutions : IXMLDOMNodeList := doc.SelectNodes('//ram:ApplicableSupplyChainTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:PayerSpecifiedDebtorFinancialInstitution');

  //TODO Index prüfen in der Schleife, bei csharp stand 0 drin
  if (debitorFinancialAccountNodes.length = debitorFinancialInstitutions.length) then
  begin
    for i := 0 to debitorFinancialAccountNodes.length-1 do
    begin
      var _account : TZUGFeRDBankAccount := TZUGFeRDBankAccount.Create;
      _account.ID := _nodeAsString(debitorFinancialAccountNodes[i], './/ram:ProprietaryID');
      _account.IBAN := _nodeAsString(debitorFinancialAccountNodes[i], './/ram:IBANID');
      _account.BIC := _nodeAsString(debitorFinancialInstitutions[i], './/ram:BICID');
      _account.Bankleitzahl := _nodeAsString(debitorFinancialInstitutions[i], './/ram:GermanBankleitzahlID');
      _account.BankName := _nodeAsString(debitorFinancialInstitutions[i], './/ram:Name');
      Result.DebitorBankAccounts.Add(_account);
    end;
  end;

  nodes := doc.SelectNodes('//ram:ApplicableSupplyChainTradeSettlement/ram:ApplicableTradeTax');
  for i := 0 to nodes.length-1 do
  begin
    Result.AddApplicableTradeTax(_nodeAsDecimal(nodes[i], './/ram:BasisAmount', 0),
                                 _nodeAsDecimal(nodes[i], './/ram:ApplicablePercent', 0),
                                 TZUGFeRDTaxTypesExtensions.FromString(_nodeAsString(nodes[i], './/ram:TypeCode')),
                                 TZUGFeRDTaxCategoryCodesExtensions.FromString(_nodeAsString(nodes[i], './/ram:CategoryCode')));
  end;

  nodes := doc.SelectNodes('//ram:SpecifiedTradeAllowanceCharge');
  for i := 0 to nodes.length-1 do
  begin
    Result.AddTradeAllowanceCharge(not _nodeAsBool(nodes[i], './/ram:ChargeIndicator'), // wichtig: das not (!) beachten
                                   _nodeAsDecimal(nodes[i], './/ram:BasisAmount', 0),
                                   Result.Currency,
                                   _nodeAsDecimal(nodes[i], './/ram:ActualAmount', 0),
                                   _nodeAsString(nodes[i], './/ram:Reason'),
                                   TZUGFeRDTaxTypesExtensions.FromString(_nodeAsString(nodes[i], './/ram:CategoryTradeTax/ram:TypeCode')),
                                   TZUGFeRDTaxCategoryCodesExtensions.FromString(_nodeAsString(nodes[i], './/ram:CategoryTradeTax/ram:CategoryCode')),
                                   _nodeAsDecimal(nodes[i], './/ram:CategoryTradeTax/ram:ApplicablePercent', 0));
  end;

  nodes := doc.SelectNodes('//ram:SpecifiedLogisticsServiceCharge');
  for i := 0 to nodes.length-1 do
  begin
    Result.AddLogisticsServiceCharge(_nodeAsDecimal(nodes[i], './/ram:AppliedAmount', 0),
                                     _nodeAsString(nodes[i], './/ram:Description'),
                                     TZUGFeRDTaxTypesExtensions.FromString(_nodeAsString(nodes[i], './/ram:AppliedTradeTax/ram:TypeCode')),
                                     TZUGFeRDTaxCategoryCodesExtensions.FromString(_nodeAsString(nodes[i], './/ram:AppliedTradeTax/ram:CategoryCode')),
                                     _nodeAsDecimal(nodes[i], './/ram:AppliedTradeTax/ram:ApplicablePercent', 0));
  end;

  Result.PaymentTerms := TZUGFeRDPaymentTerms.Create;
  Result.PaymentTerms.Description := _nodeAsString(doc.DocumentElement, '//ram:SpecifiedTradePaymentTerms/ram:Description');
  Result.PaymentTerms.DueDate.SetValue(_nodeAsDateTime(doc.DocumentElement, '//ram:SpecifiedTradePaymentTerms/ram:DueDateDateTime'));

  Result.LineTotalAmount.SetValue(_nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementMonetarySummation/ram:LineTotalAmount', 0));
  Result.ChargeTotalAmount.SetValue(_nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementMonetarySummation/ram:ChargeTotalAmount', 0));
  Result.AllowanceTotalAmount.SetValue(_nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementMonetarySummation/ram:AllowanceTotalAmount', 0));
  Result.TaxBasisAmount.SetValue(_nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementMonetarySummation/ram:TaxBasisTotalAmount',0));
  Result.TaxTotalAmount.SetValue(_nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementMonetarySummation/ram:TaxTotalAmount', 0));
  Result.GrandTotalAmount.SetValue(_nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementMonetarySummation/ram:GrandTotalAmount', 0));
  Result.RoundingAmount.SetValue(_nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:RoundingAmount', 0));
  Result.TotalPrepaidAmount.SetValue(_nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementMonetarySummation/ram:TotalPrepaidAmount', 0));
  Result.DuePayableAmount.SetValue(_nodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementMonetarySummation/ram:DuePayableAmount', 0));

  if (doc.DocumentElement.SelectSingleNode('//ram:ApplicableSupplyChainTradeAgreement/ram:BuyerOrderReferencedDocument/ram:IssueDateTime/udt:DateTimeString') <> nil) then
    Result.OrderDate.SetValue(_nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeAgreement/ram:BuyerOrderReferencedDocument/ram:IssueDateTime/udt:DateTimeString'))
  else
    Result.OrderDate.SetValue(_nodeAsDateTime(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeAgreement/ram:BuyerOrderReferencedDocument/ram:IssueDateTime'));
  Result.OrderNo := _nodeAsString(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeAgreement/ram:BuyerOrderReferencedDocument/ram:ID');

  nodes := doc.SelectNodes('//ram:IncludedSupplyChainTradeLineItem');
  for i := 0 to nodes.length-1 do
    Result.TradeLineItems.Add(_parseTradeLineItem(nodes[i]));
end;

function TZUGFeRDInvoiceDescriptor1Reader._nodeAsParty(basenode: IXmlDomNode;
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
end;

function TZUGFeRDInvoiceDescriptor1Reader._parseTradeLineItem(
  tradeLineItem: IXmlDomNode): TZUGFeRDTradeLineItem;
var
  nodes : IXMLDOMNodeList;
  i : Integer;
begin
  Result := nil;

  if (tradeLineItem = nil) then
    exit;

  //var s : String := _nodeAsString(tradeLineItem, './/ram:ApplicableTradeTax/ram:TypeCode');
  //var t : TZUGFeRDTaxTypes := TZUGFeRDTaxTypesExtensions.FromString(_nodeAsString(tradeLineItem, './/ram:ApplicableTradeTax/ram:TypeCode'));

  Result := TZUGFeRDTradeLineItem.Create;

  Result.GlobalID.ID := _nodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:GlobalID');
  Result.GlobalID.SchemeID := TZUGFeRDGlobalIDSchemeIdentifiersExtensions.FromString(_nodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:GlobalID/@schemeID'));
  Result.SellerAssignedID := _nodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:SellerAssignedID');
  Result.BuyerAssignedID := _nodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:BuyerAssignedID');
  Result.Name := _nodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:Name');
  Result.Description := _nodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:Description');
  Result.UnitQuantity.SetValue(_nodeAsDecimal(tradeLineItem, './/ram:BasisQuantity', 1));
  Result.BilledQuantity := _nodeAsDecimal(tradeLineItem, './/ram:BilledQuantity', 0);
  Result.LineTotalAmount.SetValue(_nodeAsDecimal(tradeLineItem, './/ram:LineTotalAmount', 0));
  Result.TaxCategoryCode := TZUGFeRDTaxCategoryCodesExtensions.FromString(_nodeAsString(tradeLineItem, './/ram:ApplicableTradeTax/ram:CategoryCode'));
  Result.TaxType := TZUGFeRDTaxTypesExtensions.FromString(_nodeAsString(tradeLineItem, './/ram:ApplicableTradeTax/ram:TypeCode'));
  Result.TaxPercent := _nodeAsDecimal(tradeLineItem, './/ram:ApplicableTradeTax/ram:ApplicablePercent', 0);
  Result.NetUnitPrice.SetValue(_nodeAsDecimal(tradeLineItem, './/ram:NetPriceProductTradePrice/ram:ChargeAmount', 0));
  Result.GrossUnitPrice.SetValue(_nodeAsDecimal(tradeLineItem, './/ram:GrossPriceProductTradePrice/ram:ChargeAmount', 0));
  Result.UnitCode := TZUGFeRDQuantityCodesExtensions.FromString(_nodeAsString(tradeLineItem, './/ram:BasisQuantity/@unitCode'));
  Result.BillingPeriodStart.SetValue(_nodeAsDateTime(tradeLineItem, './/ram:BillingSpecifiedPeriod/ram:StartDateTime/udt:DateTimeString'));
  Result.BillingPeriodEnd.SetValue(_nodeAsDateTime(tradeLineItem, './/ram:BillingSpecifiedPeriod/ram:EndDateTime/udt:DateTimeString'));

  if (tradeLineItem.SelectSingleNode('.//ram:AssociatedDocumentLineDocument') <> nil) then
  begin
    Result.AssociatedDocument := TZUGFeRDAssociatedDocument.Create(_nodeAsString(tradeLineItem, './/ram:AssociatedDocumentLineDocument/ram:LineID'));

    nodes := tradeLineItem.SelectNodes('.//ram:AssociatedDocumentLineDocument/ram:IncludedNote');
    for i := 0 to nodes.length-1 do
    begin
      var noteItem : TZUGFeRDNote := TZUGFeRDNote.Create(
          _nodeAsString(nodes[i], './/ram:Content'),
          TZUGFeRDSubjectCodesExtensions.FromString(_nodeAsString(nodes[i], './/ram:SubjectCode')),
          TZUGFeRDContentCodes.Unknown
        );
      Result.AssociatedDocument.Notes.Add(noteItem);
    end;
  end;

  nodes := tradeLineItem.SelectNodes('.//ram:SpecifiedSupplyChainTradeAgreement/ram:GrossPriceProductTradePrice/ram:AppliedTradeAllowanceCharge');
  for i := 0 to nodes.length-1 do
  begin

    var chargeIndicator : Boolean := _nodeAsBool(nodes[i], './ram:ChargeIndicator/udt:Indicator');
    var basisAmount : Currency := _nodeAsDecimal(nodes[i], './ram:BasisAmount',0);
    var basisAmountCurrency : String := _nodeAsString(nodes[i], './ram:BasisAmount/@currencyID');
    var actualAmount : Currency := _nodeAsDecimal(nodes[i], './ram:ActualAmount',0);
    var actualAmountCurrency : String := _nodeAsString(nodes[i], './ram:ActualAmount/@currencyID');
    var reason : String := _nodeAsString(nodes[i], './ram:Reason');

    Result.AddTradeAllowanceCharge(not chargeIndicator, // wichtig: das not beachten
                                    TZUGFeRDCurrencyCodesExtensions.FromString(basisAmountCurrency),
                                    basisAmount,
                                    actualAmount,
                                    reason);
  end;

  if (Result.UnitCode = TZUGFeRDQuantityCodes.Unknown) then
  begin
    // UnitCode alternativ aus BilledQuantity extrahieren
    Result.UnitCode := TZUGFeRDQuantityCodesExtensions.FromString(_nodeAsString(tradeLineItem, './/ram:BilledQuantity/@unitCode'));
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedSupplyChainTradeAgreement/ram:BuyerOrderReferencedDocument/ram:ID') <> nil) then
  begin
    Result.BuyerOrderReferencedDocument := TZUGFeRDBuyerOrderReferencedDocument.Create;
    Result.BuyerOrderReferencedDocument.ID := _nodeAsString(tradeLineItem, './/ram:SpecifiedSupplyChainTradeAgreement/ram:BuyerOrderReferencedDocument/ram:ID');
    Result.BuyerOrderReferencedDocument.IssueDateTime.SetValue(_nodeAsDateTime(tradeLineItem, './/ram:SpecifiedSupplyChainTradeAgreement/ram:BuyerOrderReferencedDocument/ram:IssueDateTime'));
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedSupplyChainTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:ID') <> nil) then
  begin
    Result.DeliveryNoteReferencedDocument := TZUGFeRDDeliveryNoteReferencedDocument.Create;
    Result.DeliveryNoteReferencedDocument.ID := _nodeAsString(tradeLineItem, './/ram:SpecifiedSupplyChainTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:ID');
    Result.DeliveryNoteReferencedDocument.IssueDateTime.SetValue(_nodeAsDateTime(tradeLineItem, './/ram:SpecifiedSupplyChainTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssueDateTime'));
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedSupplyChainTradeDelivery/ram:ActualDeliverySupplyChainEvent/ram:OccurrenceDateTime') <> nil) then
  begin
    Result.ActualDeliveryDate.SetValue(_nodeAsDateTime(tradeLineItem, './/ram:SpecifiedSupplyChainTradeDelivery/ram:ActualDeliverySupplyChainEvent/ram:OccurrenceDateTime/udt:DateTimeString'));
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedSupplyChainTradeAgreement/ram:ContractReferencedDocument/ram:ID') <> nil) then
  begin
    Result.ContractReferencedDocument := TZUGFeRDContractReferencedDocument.Create;
    Result.ContractReferencedDocument.ID := _nodeAsString(tradeLineItem, './/ram:SpecifiedSupplyChainTradeAgreement/ram:ContractReferencedDocument/ram:ID');
    Result.ContractReferencedDocument.IssueDateTime.SetValue(_nodeAsDateTime(tradeLineItem, './/ram:SpecifiedSupplyChainTradeAgreement/ram:ContractReferencedDocument/ram:IssueDateTime'));
  end;

  //Get all referenced AND embedded documents
  nodes := tradeLineItem.SelectNodes('.//ram:SpecifiedSupplyChainTradeAgreement/ram:AdditionalReferencedDocument');
  for i := 0 to nodes.length-1 do
  begin
    Result.AddAdditionalReferencedDocument(
        _nodeAsString(nodes[i], 'ram:ID'),
        _nodeAsDateTime(nodes[i], 'ram:IssueDateTim'),
        TZUGFeRDReferenceTypeCodesExtensions.FromString(_nodeAsString(nodes[i], 'ram:ReferenceTypeCode'))
    );
  end;

  //TODO warum doppelt
  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedSupplyChainTradeAgreement/ram:ContractReferencedDocument/ram:ID') <> nil) then
  begin
    Result.ContractReferencedDocument := TZUGFeRDContractReferencedDocument.Create;
    Result.ContractReferencedDocument.ID := _nodeAsString(tradeLineItem, './/ram:SpecifiedSupplyChainTradeAgreement/ram:ContractReferencedDocument/ram:ID');
    Result.ContractReferencedDocument.IssueDateTime.SetValue(_nodeAsDateTime(tradeLineItem, './/ram:SpecifiedSupplyChainTradeAgreement/ram:ContractReferencedDocument/ram:IssueDateTime'));
  end;
end;

end.
