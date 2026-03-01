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
  System.SysUtils, System.Classes, System.DateUtils, System.Generics.Collections,
  Xml.XMLDoc, Xml.XMLIntf
  ,Xml.Win.msxmldom, Winapi.MSXMLIntf, Winapi.msxml
  ,intf.ZUGFeRDHelper
  ,intf.ZUGFeRDXmlHelper
  ,intf.ZUGFeRDIInvoiceDescriptorReader
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
  ,intf.ZUGFeRDSpecialServiceDescriptionCodes
  ,intf.ZUGFeRDXmlUtils
  ,intf.ZUGFeRDTransportmodeCodes
  ,intf.ZUGFeRDChargeReasonCodes
  ,intf.ZUGFeRDAllowanceReasonCodes
  ;

type
  TZUGFeRDInvoiceDescriptor1Reader = class(TZUGFeRDIInvoiceDescriptorReader)
  private
    function GetValidURIs : TArray<string>;
    function _parseTradeLineItem(tradeLineItem : IXmlDomNode) : TZUGFeRDTradeLineItem;
    function _nodeAsParty(basenode: IXmlDomNode; const xpath: string) : TZUGFeRDParty;
  public
    constructor Create;
    Destructor Destroy; Override;
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

{ TZUGFeRDInvoiceDescriptor1Reader }

constructor TZUGFeRDInvoiceDescriptor1Reader.Create;
begin
  inherited;
  FNamespaces := TDictionary<string, string>.Create;
  FNamespaces.Add('xsi', 'http://www.w3.org/2001/XMLSchema-instance');
  FNamespaces.Add('rsm', 'urn:ferd:CrossIndustryDocument:invoice:1p0');
  FNamespaces.Add('ram', 'urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:12');
  FNamespaces.Add('udt', 'urn:un:unece:uncefact:data:standard:UnqualifiedDataType:15');
end;

destructor TZUGFeRDInvoiceDescriptor1Reader.Destroy;
begin
  FNamespaces.Free;
  inherited;
end;

function TZUGFeRDInvoiceDescriptor1Reader.GetValidURIs : TArray<string>;
begin
  Result :=  TArray<string>.Create(
    'urn:ferd:invoice:1.0:basic',
    'urn:ferd:invoice:1.0:comfort',
    'urn:ferd:invoice:1.0:extended',
    'urn:ferd:CrossIndustryDocument:invoice:1p0:basic',
    'urn:ferd:CrossIndustryDocument:invoice:1p0:comfort',
    'urn:ferd:CrossIndustryDocument:invoice:1p0:extended'
  );
end;

function TZUGFeRDInvoiceDescriptor1Reader.IsReadableByThisReaderVersion(
  stream: TStream): Boolean;
begin
  Result := _IsReadableByThisReaderVersion(stream, GetValidURIs);
end;

function TZUGFeRDInvoiceDescriptor1Reader.Load(Stream: TStream): TZUGFeRDInvoiceDescriptor;
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

function TZUGFeRDInvoiceDescriptor1Reader.Load(xmldocument : IXMLDocument): TZUGFeRDInvoiceDescriptor;
var
  doc : IXMLDOMDocument2;
  nodes : IXMLDOMNodeList;
  i : Integer;
begin
  doc := TZUGFeRDXmlHelper.PrepareDocumentForXPathQuerys(xmldocument, true);  // defaults for ZUGFeRD1
  // XmlNamespaceManager nsmgr = _CreateFixedNamespaceManager(doc);
  // if not(nsmgr.HasNamespace('rsm')) then
  //   nsmgr.AddNamespace('rsm', nsmgr.DefaultNamespace);

  Result := TZUGFeRDInvoiceDescriptor.Create;

  Result.IsTest := TZUGFeRDXmlUtils.NodeAsBool(doc.documentElement,'//*[local-name()="SpecifiedExchangedDocumentContext"]/ram:TestIndicator',false);
  Result.BusinessProcess := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//*[local-name()="BusinessProcessSpecifiedDocumentContextParameter"]/ram:ID');//, nsmgr),
  Result.Guideline := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:GuidelineSpecifiedDocumentContextParameter/ram:ID'); //, nsmgr)),
  Result.Profile := TZUGFeRDProfileExtensions.StringToEnum(Result.Guideline);
  Result.Name := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//*[local-name()="HeaderExchangedDocument"]/ram:Name');//, nsmgr)),
  Result.Type_ := TEnumExtensions<TZUGFeRDInvoiceType>.StringToEnum(TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//*[local-name()="HeaderExchangedDocument"]/ram:TypeCode'));//, nsmgr)),
  Result.InvoiceNo := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//*[local-name()="HeaderExchangedDocument"]/ram:ID');//, nsmgr),
  Result.InvoiceDate := TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//*[local-name()="HeaderExchangedDocument"]/ram:IssueDateTime/udt:DateTimeString');//", nsmgr)

  nodes := doc.selectNodes('//*[local-name()="HeaderExchangedDocument"]/ram:IncludedNote');
  for i := 0 to nodes.length-1 do
  begin
    var content : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Content');
    var subjectCode : ZUGFeRDNullable<TZUGFeRDSubjectCodes> := TEnumExtensions<TZUGFeRDSubjectCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:SubjectCode'));
    var contentCode : ZUGFeRDNullable<TZUGFeRDContentCodes> := TEnumExtensions<TZUGFeRDContentCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ContentCode'));
    Result.AddNote(content, subjectCode, contentCode);
  end;

  Result.ReferenceOrderNo := TZUGFeRDXmlUtils.NodeAsString(doc, '//ram:ApplicableSupplyChainTradeAgreement/ram:BuyerReference');

  Result.Seller := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeAgreement/ram:SellerTradeParty');

  nodes := doc.selectNodes('//ram:ApplicableSupplyChainTradeAgreement/ram:SellerTradeParty/ram:SpecifiedTaxRegistration');
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

  Result.Buyer := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeAgreement/ram:BuyerTradeParty');

  nodes := doc.selectNodes('//ram:ApplicableSupplyChainTradeAgreement/ram:BuyerTradeParty/ram:SpecifiedTaxRegistration');
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

  //Read TransportModeCodes --> BT-X-152
  if (doc.SelectSingleNode('//ram:ApplicableSupplyChainTradeDelivery/ram:RelatedSupplyChainConsignment/ram:SpecifiedLogisticsTransportMovement/ram:ModeCode') <> nil) then
    Result.TransportMode := TEnumExtensions<TZUGFeRDTransportmodeCodes>.StringToEnum(TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeDelivery/ram:RelatedSupplyChainConsignment/ram:SpecifiedLogisticsTransportMovement/ram:ModeCode'));

  Result.ShipTo := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeDelivery/ram:ShipToTradeParty');
  Result.ShipFrom := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeDelivery/ram:ShipFromTradeParty');
  Result.ActualDeliveryDate:=TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeDelivery/ram:ActualDeliverySupplyChainEvent/ram:OccurrenceDateTime/udt:DateTimeString');

  var _deliveryNoteNo : String := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:ID');
  var _deliveryNoteDate : ZUGFeRDNullable<TDateTime> := TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssueDateTime/udt:DateTimeString');

  if Not (_deliveryNoteDate.HasValue) then
  begin
    _deliveryNoteDate := TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssueDateTime');
  end;

  if ((_deliveryNoteDate.HasValue) or (_deliveryNoteNo <> '')) then
  begin
    Result.DeliveryNoteReferencedDocument := TZUGFeRDDeliveryNoteReferencedDocument.Create;
    Result.SetDeliveryNoteReferenceDocument(_deliveryNoteNo,_deliveryNoteDate);
  end;

  Result.Invoicee := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:InvoiceeTradeParty');
  Result.Payee := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:PayeeTradeParty');

  Result.PaymentReference := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:PaymentReference');
  Result.Currency :=  TEnumExtensions<TZUGFeRDCurrencyCodes>.StringToEnum(TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:InvoiceCurrencyCode'));

  // TODO: Multiple SpecifiedTradeSettlementPaymentMeans can exist for each account/institution (with different SEPA?)
  var _tempPaymentMeans : TZUGFeRDPaymentMeans := TZUGFeRDPaymentMeans.Create;
  _tempPaymentMeans.TypeCode := TEnumExtensions<TZUGFeRDPaymentMeansTypeCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:TypeCode'));
  _tempPaymentMeans.Information := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:Information');
  _tempPaymentMeans.SEPACreditorIdentifier := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:ID');
  _tempPaymentMeans.SEPAMandateReference := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:SpecifiedTradePaymentTerms/ram:DirectDebitMandateID');
  Result.PaymentMeans := _tempPaymentMeans;

  Result.BillingPeriodStart := TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:BillingSpecifiedPeriod/ram:StartDateTime');
  Result.BillingPeriodEnd := TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:BillingSpecifiedPeriod/ram:EndDateTime');

  var creditorFinancialAccountNodes : IXMLDOMNodeList := doc.SelectNodes('//ram:ApplicableSupplyChainTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:PayeePartyCreditorFinancialAccount');
  var creditorFinancialInstitutions : IXMLDOMNodeList := doc.SelectNodes('//ram:ApplicableSupplyChainTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:PayeeSpecifiedCreditorFinancialInstitution');

  if (creditorFinancialAccountNodes.length = creditorFinancialInstitutions.length) then
    for i := 0 to creditorFinancialAccountNodes.length-1 do
      Result.AddCreditorFinancialAccount(TZUGFeRDXmlUtils.NodeAsString(creditorFinancialAccountNodes[i], './/ram:IBANID'),
                                         TZUGFeRDXmlUtils.NodeAsString(creditorFinancialInstitutions[i], './/ram:BICID'),
                                         TZUGFeRDXmlUtils.NodeAsString(creditorFinancialAccountNodes[i], './/ram:ProprietaryID'),
                                         TZUGFeRDXmlUtils.NodeAsString(creditorFinancialInstitutions[i], './/ram:GermanBankleitzahlID'),
                                         TZUGFeRDXmlUtils.NodeAsString(creditorFinancialInstitutions[i], './/ram:Name'),
                                         TZUGFeRDXmlUtils.NodeAsString(creditorFinancialAccountNodes[i], './/ram:AccountName'));

  var debitorFinancialAccountNodes : IXMLDOMNodeList := doc.SelectNodes('//ram:ApplicableSupplyChainTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:PayerPartyDebtorFinancialAccount');
  var debitorFinancialInstitutions : IXMLDOMNodeList := doc.SelectNodes('//ram:ApplicableSupplyChainTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:PayerSpecifiedDebtorFinancialInstitution');

  //TODO Index prüfen in der Schleife, bei csharp stand 0 drin
  if (debitorFinancialAccountNodes.length = debitorFinancialInstitutions.length) then
    for i := 0 to debitorFinancialAccountNodes.length-1 do
       Result.AddDebitorFinancialAccount(TZUGFeRDXmlUtils.NodeAsString(debitorFinancialAccountNodes[i], './/ram:IBANID'),
                                         TZUGFeRDXmlUtils.NodeAsString(debitorFinancialInstitutions[i], './/ram:BICID'),
                                         TZUGFeRDXmlUtils.NodeAsString(debitorFinancialAccountNodes[i], './/ram:ProprietaryID'),
                                         TZUGFeRDXmlUtils.NodeAsString(debitorFinancialInstitutions[i], './/ram:GermanBankleitzahlID'),
                                         TZUGFeRDXmlUtils.NodeAsString(debitorFinancialInstitutions[i], './/ram:Name'));

  nodes := doc.SelectNodes('//ram:ApplicableSupplyChainTradeSettlement/ram:ApplicableTradeTax');
  for i := 0 to nodes.length-1 do
  begin
    Result.AddApplicableTradeTax(TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:CalculatedAmount', 0),
                                 TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:BasisAmount', 0),
                                 TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:ApplicablePercent', 0),
                                 TEnumExtensions<TZUGFeRDTaxTypes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:TypeCode')),
                                 TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:CategoryCode')),
                                 TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:AllowanceChargeBasisAmount', 0),
                                 Nil,
                                 '',
                                 TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:LineTotalBasisAmount'));
  end;

  nodes := doc.SelectNodes('//ram:SpecifiedTradeAllowanceCharge');
  for i := 0 to nodes.length-1 do
  begin
    if TZUGFeRDXmlUtils.NodeAsBool(nodes[i], './/ram:ChargeIndicator') then
      Result.AddTradeCharge(TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:BasisAmount', 0),
                                   Result.Currency,
                                   TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:ActualAmount', 0),
                                   TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Reason'),
                                   TEnumExtensions<TZUGFeRDTaxTypes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:CategoryTradeTax/ram:TypeCode')),
                                   TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:CategoryTradeTax/ram:CategoryCode')),
                                   TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:CategoryTradeTax/ram:ApplicablePercent', 0),
                                   TEnumExtensions<TZUGFeRDChargeReasonCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:ReasonCode')))
  else
      Result.AddTradeAllowance(TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:BasisAmount', 0),
                                   Result.Currency,
                                   TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:ActualAmount', 0),
                                   TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Reason'),
                                   TEnumExtensions<TZUGFeRDTaxTypes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:CategoryTradeTax/ram:TypeCode')),
                                   TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:CategoryTradeTax/ram:CategoryCode')),
                                   TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:CategoryTradeTax/ram:ApplicablePercent', 0),
                                   TEnumExtensions<TZUGFeRDAllowanceReasonCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:ReasonCode')))
  end;

  nodes := doc.SelectNodes('//ram:SpecifiedLogisticsServiceCharge');
  for i := 0 to nodes.length-1 do
  begin
    Result.AddLogisticsServiceCharge(TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:AppliedAmount', 0),
                                     TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Description'),
                                     TEnumExtensions<TZUGFeRDTaxTypes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:AppliedTradeTax/ram:TypeCode')),
                                     TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:AppliedTradeTax/ram:CategoryCode')),
                                     TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:AppliedTradeTax/ram:ApplicablePercent', 0));
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

  Result.LineTotalAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementMonetarySummation/ram:LineTotalAmount', 0);
  Result.ChargeTotalAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementMonetarySummation/ram:ChargeTotalAmount', 0);
  Result.AllowanceTotalAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementMonetarySummation/ram:AllowanceTotalAmount', 0);
  Result.TaxBasisAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementMonetarySummation/ram:TaxBasisTotalAmount',0);
  Result.TaxTotalAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementMonetarySummation/ram:TaxTotalAmount', 0);
  Result.GrandTotalAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementMonetarySummation/ram:GrandTotalAmount', 0);
  Result.RoundingAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementMonetarySummation/ram:RoundingAmount', 0);
  Result.TotalPrepaidAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementMonetarySummation/ram:TotalPrepaidAmount', 0);
  Result.DuePayableAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementMonetarySummation/ram:DuePayableAmount', 0);

  if (doc.DocumentElement.SelectSingleNode('//ram:ApplicableSupplyChainTradeAgreement/ram:BuyerOrderReferencedDocument/ram:IssueDateTime/udt:DateTimeString') <> nil) then
    Result.OrderDate:= TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeAgreement/ram:BuyerOrderReferencedDocument/ram:IssueDateTime/udt:DateTimeString')
  else
    Result.OrderDate:= TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeAgreement/ram:BuyerOrderReferencedDocument/ram:IssueDateTime');
  Result.OrderNo := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeAgreement/ram:BuyerOrderReferencedDocument/ram:ID');

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
  Result.ID.ID := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:ID');
  Result.ID.SchemeID := TZUGFeRDGlobalIDSchemeIdentifiers.Unknown;
  Result.GlobalID.ID := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:GlobalID');
  Result.GlobalID.SchemeID := TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.StringToEnum(TZUGFeRDXmlUtils.NodeAsString(node, 'ram:GlobalID/@schemeID'));
  Result.Name := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:Name');
  Result.Description := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:Description'); // Seller only BT-33
  Result.Postcode := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:PostalTradeAddress/ram:PostcodeCode');
  Result.City := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:PostalTradeAddress/ram:CityName');
  Result.Country := TEnumExtensions<TZUGFeRDCountryCodes>.StringToEnum(TZUGFeRDXmlUtils.NodeAsString(node, 'ram:PostalTradeAddress/ram:CountryID'));

  lineOne := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:PostalTradeAddress/ram:LineOne');
  lineTwo := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:PostalTradeAddress/ram:LineTwo');

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

  var lineId: string := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:AssociatedDocumentLineDocument/ram:LineID');
  Result := TZUGFeRDTradeLineItem.Create(lineId);

  Result.GlobalID.ID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:GlobalID');
  Result.GlobalID.SchemeID := TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.StringToEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:GlobalID/@schemeID'));
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
  Result.LineTotalAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:LineTotalAmount', 0);
  Result.TaxCategoryCode := TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:ApplicableTradeTax/ram:CategoryCode'));
  Result.TaxType := TEnumExtensions<TZUGFeRDTaxTypes>.StringToEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:ApplicableTradeTax/ram:TypeCode'));
  Result.TaxPercent := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:ApplicableTradeTax/ram:ApplicablePercent', 0);
  Result.NetUnitPrice:= TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:NetPriceProductTradePrice/ram:ChargeAmount', 0);
  Result.NetQuantity := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:NetPriceProductTradePrice/ram:BasisQuantity');
  Result.NetUnitCode := TEnumExtensions<TZUGFeRDQuantityCodes>.StringToEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:NetPriceProductTradePrice/ram:BasisQuantity/@unitCode'));
  Result.GrossUnitPrice:= TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:GrossPriceProductTradePrice/ram:ChargeAmount', 0);
  Result.GrossQuantity := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:GrossPriceProductTradePrice/ram:BasisQuantity');
  Result.GrossUnitCode := TEnumExtensions<TZUGFeRDQuantityCodes>.StringToEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:GrossPriceProductTradePrice/ram:BasisQuantity/@unitCode'));
  Result.BillingPeriodStart:= TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:BillingSpecifiedPeriod/ram:StartDateTime/udt:DateTimeString');
  Result.BillingPeriodEnd:= TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:BillingSpecifiedPeriod/ram:EndDateTime/udt:DateTimeString');

  if (tradeLineItem.SelectSingleNode('.//ram:AssociatedDocumentLineDocument') <> nil) then
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

  nodes := tradeLineItem.SelectNodes('.//ram:SpecifiedSupplyChainTradeAgreement/ram:GrossPriceProductTradePrice/ram:AppliedTradeAllowanceCharge');
  for i := 0 to nodes.length-1 do
  begin

    var chargeIndicator : Boolean := TZUGFeRDXmlUtils.NodeAsBool(nodes[i], './ram:ChargeIndicator/udt:Indicator');
    var basisAmount : Currency := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './ram:BasisAmount',0);
    var calculationPercent : ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './ram:CalculationPercent');
    var basisAmountCurrency : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:BasisAmount/@currencyID');
    var actualAmount : Currency := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './ram:ActualAmount',0);
    var actualAmountCurrency : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:ActualAmount/@currencyID');
    var reason : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:Reason');

    if chargeIndicator then // charge
    begin
      if calculationPercent.HasValue then
        Result.AddTradeCharge(TEnumExtensions<TZUGFeRDCurrencyCodes>.StringToEnum(basisAmountCurrency),
                                      basisAmount,
                                      actualAmount,
                                      calculationPercent,
                                      reason)
      else
        Result.AddTradeCharge(TEnumExtensions<TZUGFeRDCurrencyCodes>.StringToEnum(basisAmountCurrency),
                                      basisAmount,
                                      actualAmount,
                                      reason);
    end
    else
    begin
      if calculationPercent.HasValue then
        Result.AddTradeAllowance(TEnumExtensions<TZUGFeRDCurrencyCodes>.StringToEnum(basisAmountCurrency),
                                      basisAmount,
                                      actualAmount,
                                      calculationPercent,
                                      reason)
      else
        Result.AddTradeAllowance(TEnumExtensions<TZUGFeRDCurrencyCodes>.StringToEnum(basisAmountCurrency),
                                      basisAmount,
                                      actualAmount,
                                      reason);
    end;
  end;

  if (Result.UnitCode = TZUGFeRDQuantityCodes.Unknown) then
  begin
    // UnitCode alternativ aus BilledQuantity extrahieren
    Result.UnitCode := TEnumExtensions<TZUGFeRDQuantityCodes>.StringToEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:BilledQuantity/@unitCode'));
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedSupplyChainTradeAgreement/ram:BuyerOrderReferencedDocument/ram:ID') <> nil) then
  begin
    Result.BuyerOrderReferencedDocument := TZUGFeRDBuyerOrderReferencedDocument.Create;
    Result.BuyerOrderReferencedDocument.ID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedSupplyChainTradeAgreement/ram:BuyerOrderReferencedDocument/ram:ID');
    Result.BuyerOrderReferencedDocument.IssueDateTime:= TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:SpecifiedSupplyChainTradeAgreement/ram:BuyerOrderReferencedDocument/ram:IssueDateTime');
    Result.BuyerOrderReferencedDocument.LineID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedSupplyChainTradeAgreement/ram:BuyerOrderReferencedDocument/ram:LineID');
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedSupplyChainTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:ID') <> nil) then
  begin
    Result.DeliveryNoteReferencedDocument := TZUGFeRDDeliveryNoteReferencedDocument.Create;
    Result.DeliveryNoteReferencedDocument.ID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedSupplyChainTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:ID');
    Result.DeliveryNoteReferencedDocument.IssueDateTime:= TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:SpecifiedSupplyChainTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssueDateTime');
    Result.DeliveryNoteReferencedDocument.LineID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedSupplyChainTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:LineID');
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedSupplyChainTradeDelivery/ram:ActualDeliverySupplyChainEvent/ram:OccurrenceDateTime') <> nil) then
  begin
    Result.ActualDeliveryDate:= TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:SpecifiedSupplyChainTradeDelivery/ram:ActualDeliverySupplyChainEvent/ram:OccurrenceDateTime/udt:DateTimeString');
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedSupplyChainTradeAgreement/ram:ContractReferencedDocument') <> nil) then
  begin
    Result.ContractReferencedDocument := TZUGFeRDContractReferencedDocument.Create;
    Result.ContractReferencedDocument.ID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedSupplyChainTradeAgreement/ram:ContractReferencedDocument/ram:ID');
    Result.ContractReferencedDocument.IssueDateTime:= TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:SpecifiedSupplyChainTradeAgreement/ram:ContractReferencedDocument/ram:IssueDateTime');
    Result.ContractReferencedDocument.LineID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedSupplyChainTradeAgreement/ram:ContractReferencedDocument/ram:LineID');
  end;

  //Get all referenced AND embedded documents
  nodes := tradeLineItem.SelectNodes('.//ram:SpecifiedSupplyChainTradeAgreement/ram:AdditionalReferencedDocument');
  for i := 0 to nodes.length-1 do
  begin
    Result.AddAdditionalReferencedDocument(
        TZUGFeRDXmlUtils.NodeAsString(nodes[i], 'ram:ID'),
        TEnumExtensions<TZUGFeRDReferenceTypeCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], 'ram:ReferenceTypeCode')),
        TZUGFeRDXmlUtils.NodeAsDateTime(nodes[i], 'ram:IssueDateTime')
    );
  end;
end;

end.
