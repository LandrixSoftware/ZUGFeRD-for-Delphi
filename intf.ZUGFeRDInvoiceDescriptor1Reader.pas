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
  ,Xml.XMLDoc, Xml.XMLIntf
  ,Xml.Win.msxmldom, Winapi.MSXMLIntf, Winapi.msxml
  ,intf.ZUGFeRDHelper
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
  ,intf.ZUGFeRDSpecialServiceDescriptionCodes
  ,intf.ZUGFeRDAllowanceOrChargeIdentificationCodes
  ,intf.ZUGFeRDXmlUtils
  ;

type
  TZUGFeRDInvoiceDescriptor1Reader = class(TZUGFeRDInvoiceDescriptorReader)
  private
    function GetValidURIs : TArray<string>;
    function _parseTradeLineItem(tradeLineItem : IXmlDomNode) : TZUGFeRDTradeLineItem;
    function _nodeAsParty(basenode: IXmlDomNode; const xpath: string) : TZUGFeRDParty;
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

{ TZUGFeRDInvoiceDescriptor1Reader }

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
  Result := IsReadableByThisReaderVersion(stream, GetValidURIs);
end;

function TZUGFeRDInvoiceDescriptor1Reader.IsReadableByThisReaderVersion(
  xmldocument: IXMLDocument): Boolean;
begin
  Result := IsReadableByThisReaderVersion(xmldocument, GetValidURIs);
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
  //XmlNamespaceManager nsmgr = _GenerateNamespaceManagerFromNode(doc.DocumentElement);

  Result := TZUGFeRDInvoiceDescriptor.Create;

  Result.IsTest := TZUGFeRDXmlUtils.NodeAsBool(doc.documentElement,'//*[local-name()="SpecifiedExchangedDocumentContext"]/ram:TestIndicator',false);
  Result.BusinessProcess := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//*[local-name()="BusinessProcessSpecifiedDocumentContextParameter"]/ram:ID');//, nsmgr),
  Result.Guideline := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:GuidelineSpecifiedDocumentContextParameter/ram:ID'); //, nsmgr)),
  Result.Profile := TZUGFeRDProfileExtensions.FromString(Result.Guideline);
  Result.Name := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//*[local-name()="HeaderExchangedDocument"]/ram:Name');//, nsmgr)),
  Result.Type_ := TZUGFeRDInvoiceTypeExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//*[local-name()="HeaderExchangedDocument"]/ram:TypeCode'));//, nsmgr)),
  Result.InvoiceNo := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//*[local-name()="HeaderExchangedDocument"]/ram:ID');//, nsmgr),
  Result.InvoiceDate := TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//*[local-name()="HeaderExchangedDocument"]/ram:IssueDateTime/udt:DateTimeString');//", nsmgr)

  nodes := doc.selectNodes('//*[local-name()="HeaderExchangedDocument"]/ram:IncludedNote');
  for i := 0 to nodes.length-1 do
  begin
    var content : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Content');
    var _subjectCode : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:SubjectCode');
    var subjectCode : TZUGFeRDSubjectCodes := TZUGFeRDSubjectCodesExtensions.FromString(_subjectCode);
    var contentCode : TZUGFeRDContentCodes := TZUGFeRDContentCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ContentCode'));
    Result.AddNote(content, subjectCode, contentCode);
  end;

  Result.ReferenceOrderNo := TZUGFeRDXmlUtils.NodeAsString(doc, '//ram:ApplicableSupplyChainTradeAgreement/ram:BuyerReference');

  Result.Seller := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeAgreement/ram:SellerTradeParty');

  nodes := doc.selectNodes('//ram:ApplicableSupplyChainTradeAgreement/ram:SellerTradeParty/ram:SpecifiedTaxRegistration');
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

  Result.Buyer := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeAgreement/ram:BuyerTradeParty');

  nodes := doc.selectNodes('//ram:ApplicableSupplyChainTradeAgreement/ram:BuyerTradeParty/ram:SpecifiedTaxRegistration');
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
  Result.Currency :=  TZUGFeRDCurrencyCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:InvoiceCurrencyCode'));

  // TODO: Multiple SpecifiedTradeSettlementPaymentMeans can exist for each account/institution (with different SEPA?)
  var _tempPaymentMeans : TZUGFeRDPaymentMeans := TZUGFeRDPaymentMeans.Create;
  _tempPaymentMeans.TypeCode := TZUGFeRDPaymentMeansTypeCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:TypeCode'));
  _tempPaymentMeans.Information := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:Information');
  _tempPaymentMeans.SEPACreditorIdentifier := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:ID');

  Result.PaymentMeans := _tempPaymentMeans;

  //TODO udt:DateTimeString
  Result.BillingPeriodStart := TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:BillingSpecifiedPeriod/ram:StartDateTime');
  Result.BillingPeriodEnd := TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableSupplyChainTradeSettlement/ram:BillingSpecifiedPeriod/ram:EndDateTime');
  //Result.BillingPeriodStart := TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, "//ram:ApplicableHeaderTradeSettlement/ram:BillingSpecifiedPeriod/ram:StartDateTime');
  //Result.BillingPeriodEnd := TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, "//ram:ApplicableHeaderTradeSettlement/ram:BillingSpecifiedPeriod/ram:EndDateTime');

  var creditorFinancialAccountNodes : IXMLDOMNodeList := doc.SelectNodes('//ram:ApplicableSupplyChainTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:PayeePartyCreditorFinancialAccount');
  var creditorFinancialInstitutions : IXMLDOMNodeList := doc.SelectNodes('//ram:ApplicableSupplyChainTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:PayeeSpecifiedCreditorFinancialInstitution');

  if (creditorFinancialAccountNodes.length = creditorFinancialInstitutions.length) then
  for i := 0 to creditorFinancialAccountNodes.length-1 do
  begin
    var _account : TZUGFeRDBankAccount := TZUGFeRDBankAccount.Create;
    _account.ID := TZUGFeRDXmlUtils.NodeAsString(creditorFinancialAccountNodes[i], './/ram:ProprietaryID');
    _account.IBAN := TZUGFeRDXmlUtils.NodeAsString(creditorFinancialAccountNodes[i], './/ram:IBANID');
    _account.BIC := TZUGFeRDXmlUtils.NodeAsString(creditorFinancialInstitutions[i], './/ram:BICID');
    _account.Bankleitzahl := TZUGFeRDXmlUtils.NodeAsString(creditorFinancialInstitutions[i], './/ram:GermanBankleitzahlID');
    _account.BankName := TZUGFeRDXmlUtils.NodeAsString(creditorFinancialInstitutions[i], './/ram:Name');
    _account.Name := TZUGFeRDXmlUtils.NodeAsString(creditorFinancialInstitutions[i], './/ram:AccountName');
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
      _account.ID := TZUGFeRDXmlUtils.NodeAsString(debitorFinancialAccountNodes[i], './/ram:ProprietaryID');
      _account.IBAN := TZUGFeRDXmlUtils.NodeAsString(debitorFinancialAccountNodes[i], './/ram:IBANID');
      _account.BIC := TZUGFeRDXmlUtils.NodeAsString(debitorFinancialInstitutions[i], './/ram:BICID');
      _account.Bankleitzahl := TZUGFeRDXmlUtils.NodeAsString(debitorFinancialInstitutions[i], './/ram:GermanBankleitzahlID');
      _account.BankName := TZUGFeRDXmlUtils.NodeAsString(debitorFinancialInstitutions[i], './/ram:Name');
      Result.DebitorBankAccounts.Add(_account);
    end;
  end;

  nodes := doc.SelectNodes('//ram:ApplicableSupplyChainTradeSettlement/ram:ApplicableTradeTax');
  for i := 0 to nodes.length-1 do
  begin
    Result.AddApplicableTradeTax(TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:CalculatedAmount', 0),
                                 TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:BasisAmount', 0),
                                 TZUGFeRDXmlUtils.NodeAsDouble(nodes[i], './/ram:ApplicablePercent', 0),
                                 TZUGFeRDTaxTypesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:TypeCode')),
                                 TZUGFeRDTaxCategoryCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:CategoryCode')),
                                 TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:AllowanceChargeBasisAmount', 0),
                                 Nil,
                                 '',
                                 TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:LineTotalBasisAmount'));
  end;

  nodes := doc.SelectNodes('//ram:SpecifiedTradeAllowanceCharge');
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
                                   TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:CategoryTradeTax/ram:ApplicablePercent', 0));
  end;

  nodes := doc.SelectNodes('//ram:SpecifiedLogisticsServiceCharge');
  for i := 0 to nodes.length-1 do
  begin
    Result.AddLogisticsServiceCharge(TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:AppliedAmount', 0),
                                     TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Description'),
                                     TZUGFeRDTaxTypesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:AppliedTradeTax/ram:TypeCode')),
                                     TZUGFeRDTaxCategoryCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:AppliedTradeTax/ram:CategoryCode')),
                                     TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:AppliedTradeTax/ram:ApplicablePercent', 0));
  end;

  nodes := doc.SelectNodes('//ram:SpecifiedTradePaymentTerms');
  for i := 0 to nodes.length-1 do
  begin
    var paymentTerm : TZUGFeRDPaymentTerms := TZUGFeRDPaymentTerms.Create;
    paymentTerm.Description := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Description');
    paymentTerm.DueDate:= TZUGFeRDXmlUtils.NodeAsDateTime(nodes[i], './/ram:DueDateDateTime/udt:DateTimeString');
    paymentTerm.PartialPaymentAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:PartialPaymentAmount');
    var discountPercent: ZUGFeRDNullable<Double> := TZUGFeRDXmlUtils.NodeAsDouble(nodes[i], './/ram:ApplicableTradePaymentDiscountTerms/ram:CalculationPercent');
    var penaltyPercent: ZUGFeRDNullable<Double> := TZUGFeRDXmlUtils.NodeAsDouble(nodes[i], './/ram:ApplicableTradePaymentPenaltyTerms/ram:CalculationPercent');
    var Days: ZUGFeRDNullable<Integer>;
    if discountPercent.HasValue then
    begin
      paymentTerm.PaymentTermsType:= TZUGFeRDPaymentTermsType.Skonto;
      paymentTerm.Percentage:= discountPercent;
      if TZUGFeRDQuantityCodes.DAY = TZUGFeRDQuantityCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ApplicableTradePaymentDiscountTerms/ram:BasisPeriodMeasure/@unitCode')) then
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
      if TZUGFeRDQuantityCodes.DAY = TZUGFeRDQuantityCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ApplicableTradePaymentPenaltyTerms/ram:BasisPeriodMeasure/@unitCode')) then
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
  Result.RoundingAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(doc.DocumentElement, '//ram:SpecifiedTradeSettlementHeaderMonetarySummation/ram:RoundingAmount', 0);
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

  //var s : String := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:ApplicableTradeTax/ram:TypeCode');
  //var t : TZUGFeRDTaxTypes := TZUGFeRDTaxTypesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:ApplicableTradeTax/ram:TypeCode'));

  Result := TZUGFeRDTradeLineItem.Create;

  Result.GlobalID.ID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:GlobalID');
  Result.GlobalID.SchemeID := TZUGFeRDGlobalIDSchemeIdentifiersExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:GlobalID/@schemeID'));
  Result.SellerAssignedID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:SellerAssignedID');
  Result.BuyerAssignedID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:BuyerAssignedID');
  Result.Name := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:Name');
  Result.Description := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:Description');
  Result.BilledQuantity := TZUGFeRDXmlUtils.NodeAsDouble(tradeLineItem, './/ram:BilledQuantity', 0);
  Result.UnitCode := TZUGFeRDQuantityCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:BilledQuantity/@unitCode'));
  Result.PackageQuantity := TZUGFeRDXmlUtils.NodeAsDouble(tradeLineItem, './/ram:PackageQuantity');
  Result.PackageUnitCode := TZUGFeRDQuantityCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:PackageQuantity/@unitCode'));
  Result.ChargeFreeQuantity := TZUGFeRDXmlUtils.NodeAsDouble(tradeLineItem, './/ram:ChargeFreeQuantity');
  Result.ChargeFreeUnitCode := TZUGFeRDQuantityCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:ChargeFreeQuantity/@unitCode'));
  Result.LineTotalAmount:= TZUGFeRDXmlUtils.NodeAsDouble(tradeLineItem, './/ram:LineTotalAmount', 0);
  Result.TaxCategoryCode := TZUGFeRDTaxCategoryCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:ApplicableTradeTax/ram:CategoryCode'));
  Result.TaxType := TZUGFeRDTaxTypesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:ApplicableTradeTax/ram:TypeCode'));
  Result.TaxPercent := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:ApplicableTradeTax/ram:ApplicablePercent', 0);
  Result.NetUnitPrice:= TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:NetPriceProductTradePrice/ram:ChargeAmount', 0);
  Result.NetQuantity := TZUGFeRDXmlUtils.NodeAsDouble(tradeLineItem, './/ram:NetPriceProductTradePrice/ram:BasisQuantity');
  Result.NetUnitCode := TZUGFeRDQuantityCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:NetPriceProductTradePrice/ram:BasisQuantity/@unitCode'));
  Result.GrossUnitPrice:= TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:GrossPriceProductTradePrice/ram:ChargeAmount', 0);
  Result.GrossQuantity := TZUGFeRDXmlUtils.NodeAsDouble(tradeLineItem, './/ram:GrossPriceProductTradePrice/ram:BasisQuantity');
  Result.GrossUnitCode := TZUGFeRDQuantityCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:GrossPriceProductTradePrice/ram:BasisQuantity/@unitCode'));
  Result.BillingPeriodStart:= TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:BillingSpecifiedPeriod/ram:StartDateTime/udt:DateTimeString');
  Result.BillingPeriodEnd:= TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:BillingSpecifiedPeriod/ram:EndDateTime/udt:DateTimeString');

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

  nodes := tradeLineItem.SelectNodes('.//ram:SpecifiedSupplyChainTradeAgreement/ram:GrossPriceProductTradePrice/ram:AppliedTradeAllowanceCharge');
  for i := 0 to nodes.length-1 do
  begin

    var chargeIndicator : Boolean := TZUGFeRDXmlUtils.NodeAsBool(nodes[i], './ram:ChargeIndicator/udt:Indicator');
    var basisAmount : Currency := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './ram:BasisAmount',0);
    var basisAmountCurrency : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:BasisAmount/@currencyID');
    var actualAmount : Currency := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './ram:ActualAmount',0);
    var actualAmountCurrency : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:ActualAmount/@currencyID');
    var reason : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:Reason');

    Result.AddTradeAllowanceCharge(not chargeIndicator, // wichtig: das not beachten
                                    TZUGFeRDCurrencyCodesExtensions.FromString(basisAmountCurrency),
                                    basisAmount,
                                    actualAmount,
                                    reason,
                                    TZUGFeRDSpecialServiceDescriptionCodes.Unknown,
                                    TZUGFeRDAllowanceOrChargeIdentificationCodes.Unknown);
  end;

  if (Result.UnitCode = TZUGFeRDQuantityCodes.Unknown) then
  begin
    // UnitCode alternativ aus BilledQuantity extrahieren
    Result.UnitCode := TZUGFeRDQuantityCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:BilledQuantity/@unitCode'));
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedSupplyChainTradeAgreement/ram:BuyerOrderReferencedDocument/ram:ID') <> nil) then
  begin
    Result.BuyerOrderReferencedDocument := TZUGFeRDBuyerOrderReferencedDocument.Create;
    Result.BuyerOrderReferencedDocument.ID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedSupplyChainTradeAgreement/ram:BuyerOrderReferencedDocument/ram:ID');
    Result.BuyerOrderReferencedDocument.IssueDateTime:= TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:SpecifiedSupplyChainTradeAgreement/ram:BuyerOrderReferencedDocument/ram:IssueDateTime');
    Result.BuyerOrderReferencedDocument.LineID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:BuyerOrderReferencedDocument/ram:LineID');
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedSupplyChainTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:ID') <> nil) then
  begin
    Result.DeliveryNoteReferencedDocument := TZUGFeRDDeliveryNoteReferencedDocument.Create;
    Result.DeliveryNoteReferencedDocument.ID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedSupplyChainTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:ID');
    Result.DeliveryNoteReferencedDocument.IssueDateTime:= TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:SpecifiedSupplyChainTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssueDateTime');
    Result.DeliveryNoteReferencedDocument.LineID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedLineTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:LineID');
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedSupplyChainTradeDelivery/ram:ActualDeliverySupplyChainEvent/ram:OccurrenceDateTime') <> nil) then
  begin
    Result.ActualDeliveryDate:= TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:SpecifiedSupplyChainTradeDelivery/ram:ActualDeliverySupplyChainEvent/ram:OccurrenceDateTime/udt:DateTimeString');
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeAgreement/ram:ContractReferencedDocument') <> nil) then
  begin
    Result.ContractReferencedDocument := TZUGFeRDContractReferencedDocument.Create;
    Result.ContractReferencedDocument.ID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:ContractReferencedDocument/ram:IssuerAssignedID');
    Result.ContractReferencedDocument.IssueDateTime:= TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:ContractReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString');
    Result.ContractReferencedDocument.LineID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:ContractReferencedDocument/ram:LineID');
  end;

  //Get all referenced AND embedded documents
  nodes := tradeLineItem.SelectNodes('.//ram:SpecifiedSupplyChainTradeAgreement/ram:AdditionalReferencedDocument');
  for i := 0 to nodes.length-1 do
  begin
    Result.AddAdditionalReferencedDocument(
        TZUGFeRDXmlUtils.NodeAsString(nodes[i], 'ram:ID'),
        TZUGFeRDReferenceTypeCodesExtensions.FromString(TZUGFeRDXmlUtils.NodeAsString(nodes[i], 'ram:ReferenceTypeCode')),
        TZUGFeRDXmlUtils.NodeAsDateTime(nodes[i], 'ram:IssueDateTime')
    );
  end;
end;

end.
