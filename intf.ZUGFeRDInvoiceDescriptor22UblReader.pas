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
  ,System.Generics.Collections
  ,System.NetEncoding
  ,Xml.XMLDoc, Xml.xmldom, Xml.XMLIntf
  ,Xml.Win.msxmldom, Winapi.MSXMLIntf, Winapi.msxml
  ,intf.ZUGFeRDHelper
  ,intf.ZUGFeRDXmlHelper
  ,intf.ZUGFeRDXmlUtils
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
  ,intf.ZUGFeRDDespatchAdviceReferencedDocument
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
  ,intf.ZUGFeRDDesignatedProductClassificationClassCodes
  ,intf.ZUGFeRDChargeReasonCodes
  ,intf.ZUGFeRDAllowanceReasonCodes
  ;

type
  TZUGFeRDInvoiceDescriptor22UblReader = class(TZUGFeRDIInvoiceDescriptorReader)
  private
    function GetValidURIs : TArray<string>;
    function _parseTradeLineItem(tradeLineItem : IXmlDomNode; isInvoice: Boolean; const parentLineId: string = '') : TZUGFeRDTradeLineItem;
    function _nodeAsParty(basenode: IXmlDomNode; const xpath: string) : TZUGFeRDParty;
    function _nodeAsAddressParty(basenode: IXmlDomNode; const xpath: string) : TZUGFeRDParty;
    function _nodeAsLegalOrganization(basenode: IXmlDomNode; const xpath: string) : TZUGFeRDLegalOrganization;
    function _nodeAsBankAccount(basenode: IXmlDomNode; const xpath: string) : TZUGFeRDBankAccount;
  protected
    function _IsReadableByThisReaderVersion(AStream: TStream; const AValidURIs: TArray<string>): Boolean; override;
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

{ TZUGFeRDInvoiceDescriptor22UBLReader }

constructor TZUGFeRDInvoiceDescriptor22UblReader.Create;
begin
  inherited;
  FNamespaces := TDictionary<string, string>.Create;
  FNamespaces.Add('cac', 'urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2');
  FNamespaces.Add('cbc', 'urn:oasis:names:specification:ubl:schema:xsd:CommonBasicComponents-2');
end;

destructor TZUGFeRDInvoiceDescriptor22UblReader.Destroy;
begin
  FNamespaces.Free;
  inherited;
end;

function TZUGFeRDInvoiceDescriptor22UBLReader.GetValidURIs : TArray<string>;
begin
  Result := TArray<string>.Create(
    'urn:oasis:names:specification:ubl:schema:xsd:Invoice-2',
    'urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2',
    'urn:oasis:names:specification:ubl:schema:xsd:CommonBasicComponents-2'
  );
end;

function TZUGFeRDInvoiceDescriptor22UBLReader.IsReadableByThisReaderVersion(stream: TStream): Boolean;
begin
  Result := _IsReadableByThisReaderVersion(stream, GetValidURIs);
end;

function TZUGFeRDInvoiceDescriptor22UBLReader._IsReadableByThisReaderVersion(AStream: TStream; const AValidURIs: TArray<string>): Boolean;
var
  LOldPosition: Int64;
  LReader: TStreamReader;
  LData: string;
  LValidURI: string;
  LSearchStr: string;
begin
  Result := False;
  LOldPosition := AStream.Position;
  try
    AStream.Position := 0;
    LReader := TStreamReader.Create(AStream, TEncoding.UTF8, True, 1024);
    try
      LData := LReader.ReadToEnd.Replace(' ', '');

      for LValidURI in AValidURIs do
      begin
        LSearchStr := '="' + LValidURI + '"';
        if Pos(LSearchStr.ToLower, LData.ToLower) > 0 then
        begin
          Result := True;
          Break;
        end;
      end;
    finally
      LReader.Free;
    end;
  finally
    AStream.Position := LOldPosition;
  end;
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
  node : IXMLDOMNode;
  nodes : IXMLDOMNodeList;
  i : Integer;
  isInvoice : Boolean;
  typeSelector : string;
  tradeLineItemSelector : string;
  baseNode : IXMLDOMNode;
begin
  doc := TZUGFeRDXmlHelper.PrepareDocumentForXPathQuerys(xmldocument);

  // Detect Invoice vs CreditNote from root element name
  isInvoice := True;
  if doc.documentElement <> nil then
  begin
    var rootName : string := doc.documentElement.nodeName;
    if Pos('CreditNote', rootName) > 0 then
      isInvoice := False;
  end;

  if isInvoice then
  begin
    typeSelector := '//cbc:InvoiceTypeCode';
    tradeLineItemSelector := '//cac:InvoiceLine';
  end
  else
  begin
    typeSelector := '//cbc:CreditNoteTypeCode';
    tradeLineItemSelector := '//cac:CreditNoteLine';
  end;

  baseNode := doc.documentElement;

  Result := TZUGFeRDInvoiceDescriptor.Create;

  Result.IsTest := TZUGFeRDXmlUtils.NodeAsBool(doc.documentElement, '//cbc:TestIndicator', false);
  Result.BusinessProcess := TZUGFeRDXmlUtils.NodeAsString(doc.documentElement, '//cbc:ProfileID');
  Result.Profile := TZUGFeRDProfile.XRechnung; // UBL is always XRechnung profile
  Result.InvoiceNo := TZUGFeRDXmlUtils.NodeAsString(doc.documentElement, '//cbc:ID');
  Result.InvoiceDate := TZUGFeRDXmlUtils.NodeAsDateTime(doc.documentElement, '//cbc:IssueDate');
  Result.Type_ := TEnumExtensions<TZUGFeRDInvoiceType>.StringToEnum(
    TZUGFeRDXmlUtils.NodeAsString(doc.documentElement, typeSelector));

  // Notes - parse #SubjectCode#Content format
  nodes := baseNode.selectNodes('cbc:Note');
  for i := 0 to nodes.length - 1 do
  begin
    var content : string := TZUGFeRDXmlUtils.NodeAsString(nodes[i], '.');
    if content.Trim = '' then
      continue;
    var contentParts : TArray<string> := content.Split(['#'], TStringSplitOptions.ExcludeEmpty);
    var subjectCodeAsString : string := '';
    if (Length(contentParts) > 1) and (Length(contentParts[0]) = 3) then
    begin
      subjectCodeAsString := contentParts[0];
      content := contentParts[1];
    end;
    var subjectCode : ZUGFeRDNullable<TZUGFeRDSubjectCodes> :=
      TEnumExtensions<TZUGFeRDSubjectCodes>.StringToNullableEnum(subjectCodeAsString);
    Result.AddNote(content, subjectCode);
  end;

  Result.ReferenceOrderNo := TZUGFeRDXmlUtils.NodeAsString(doc.documentElement, '//cbc:BuyerReference');

  // Seller (BG-4)
  Result.Seller := _nodeAsParty(doc.documentElement, '//cac:AccountingSupplierParty/cac:Party');

  if doc.selectSingleNode('//cac:AccountingSupplierParty/cac:Party/cbc:EndpointID') <> nil then
  begin
    var id : string := TZUGFeRDXmlUtils.NodeAsString(doc.documentElement, '//cac:AccountingSupplierParty/cac:Party/cbc:EndpointID');
    var schemeID : string := TZUGFeRDXmlUtils.NodeAsString(doc.documentElement, '//cac:AccountingSupplierParty/cac:Party/cbc:EndpointID/@schemeID');
    var eas : ZUGFeRDNullable<TZUGFeRDElectronicAddressSchemeIdentifiers> :=
      TEnumExtensions<TZUGFeRDElectronicAddressSchemeIdentifiers>.StringToNullableEnum(schemeID);
    if eas.HasValue then
      Result.SetSellerElectronicAddress(id, eas);
  end;

  // Seller Tax Registrations
  nodes := doc.selectNodes('//cac:AccountingSupplierParty/cac:Party/cac:PartyTaxScheme');
  for i := 0 to nodes.length - 1 do
  begin
    var id : string := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/cbc:CompanyID');
    var taxSchemeID : string := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/cac:TaxScheme/cbc:ID');
    // Map UBL tax scheme: VAT -> VA, TAX -> FC
    var schemeID : TZUGFeRDTaxRegistrationSchemeID;
    if SameText(taxSchemeID, 'VAT') then
      schemeID := TZUGFeRDTaxRegistrationSchemeID.VA
    else
      schemeID := TZUGFeRDTaxRegistrationSchemeID.FC;
    Result.AddSellerTaxRegistration(id, schemeID);
  end;

  // Seller Contact
  if doc.selectSingleNode('//cac:AccountingSupplierParty/cac:Party/cac:Contact') <> nil then
  begin
    Result.SellerContact := TZUGFeRDContact.Create;
    Result.SellerContact.Name := TZUGFeRDXmlUtils.NodeAsString(doc.documentElement, '//cac:AccountingSupplierParty/cac:Party/cac:Contact/cbc:Name');
    Result.SellerContact.OrgUnit := '';
    Result.SellerContact.PhoneNo := TZUGFeRDXmlUtils.NodeAsString(doc.documentElement, '//cac:AccountingSupplierParty/cac:Party/cac:Contact/cbc:Telephone');
    Result.SellerContact.FaxNo := '';
    Result.SellerContact.EmailAddress := TZUGFeRDXmlUtils.NodeAsString(doc.documentElement, '//cac:AccountingSupplierParty/cac:Party/cac:Contact/cbc:ElectronicMail');
  end;

  // Buyer (BG-7)
  Result.Buyer := _nodeAsParty(doc.documentElement, '//cac:AccountingCustomerParty/cac:Party');

  if doc.selectSingleNode('//cac:AccountingCustomerParty/cac:Party/cbc:EndpointID') <> nil then
  begin
    var id : string := TZUGFeRDXmlUtils.NodeAsString(doc.documentElement, '//cac:AccountingCustomerParty/cac:Party/cbc:EndpointID');
    var schemeID : string := TZUGFeRDXmlUtils.NodeAsString(doc.documentElement, '//cac:AccountingCustomerParty/cac:Party/cbc:EndpointID/@schemeID');
    var eas : ZUGFeRDNullable<TZUGFeRDElectronicAddressSchemeIdentifiers> :=
      TEnumExtensions<TZUGFeRDElectronicAddressSchemeIdentifiers>.StringToNullableEnum(schemeID);
    if eas.HasValue then
      Result.SetBuyerElectronicAddress(id, eas);
  end;

  // Buyer Tax Registrations
  nodes := doc.selectNodes('//cac:AccountingCustomerParty/cac:Party/cac:PartyTaxScheme');
  for i := 0 to nodes.length - 1 do
  begin
    var id : string := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/cbc:CompanyID');
    var taxSchemeID : string := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/cac:TaxScheme/cbc:ID');
    var schemeID : TZUGFeRDTaxRegistrationSchemeID;
    if SameText(taxSchemeID, 'VAT') then
      schemeID := TZUGFeRDTaxRegistrationSchemeID.VA
    else
      schemeID := TZUGFeRDTaxRegistrationSchemeID.FC;
    Result.AddBuyerTaxRegistration(id, schemeID);
  end;

  // Buyer Contact
  if doc.selectSingleNode('//cac:AccountingCustomerParty/cac:Party/cac:Contact') <> nil then
  begin
    Result.BuyerContact := TZUGFeRDContact.Create;
    Result.BuyerContact.Name := TZUGFeRDXmlUtils.NodeAsString(doc.documentElement, '//cac:AccountingCustomerParty/cac:Party/cac:Contact/cbc:Name');
    Result.BuyerContact.OrgUnit := '';
    Result.BuyerContact.PhoneNo := TZUGFeRDXmlUtils.NodeAsString(doc.documentElement, '//cac:AccountingCustomerParty/cac:Party/cac:Contact/cbc:Telephone');
    Result.BuyerContact.FaxNo := '';
    Result.BuyerContact.EmailAddress := TZUGFeRDXmlUtils.NodeAsString(doc.documentElement, '//cac:AccountingCustomerParty/cac:Party/cac:Contact/cbc:ElectronicMail');
  end;

  // Tax Representative (BG-11) - Note: in UBL TaxRepresentativeParty contains party info directly
  Result.SellerTaxRepresentative := _nodeAsParty(doc.documentElement, '//cac:TaxRepresentativeParty');

  // Delivery (BG-13)
  var deliveryNode : IXMLDOMNode := doc.selectSingleNode('//cac:Delivery');
  if deliveryNode <> nil then
  begin
    var deliveryLocationNode : IXMLDOMNode := deliveryNode.selectSingleNode('.//cac:DeliveryLocation');
    if deliveryLocationNode <> nil then
    begin
      Result.ShipTo := _nodeAsAddressParty(deliveryLocationNode, 'cac:Address');
      if Result.ShipTo = nil then
        Result.ShipTo := TZUGFeRDParty.Create;
      Result.ShipTo.GlobalID := TZUGFeRDGlobalID.Create;
      Result.ShipTo.ID := TZUGFeRDGlobalID.CreateWithParams(
        TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.StringToNullableEnum(
          TZUGFeRDXmlUtils.NodeAsString(deliveryLocationNode, './/cbc:ID/@schemeID')),
        TZUGFeRDXmlUtils.NodeAsString(deliveryLocationNode, './/cbc:ID'));
      Result.ShipTo.Name := TZUGFeRDXmlUtils.NodeAsString(deliveryNode, './/cac:DeliveryParty/cac:PartyName/cbc:Name');
    end;
    Result.ActualDeliveryDate := TZUGFeRDXmlUtils.NodeAsDateTime(doc.documentElement, '//cac:Delivery/cbc:ActualDeliveryDate');
  end;

  // Payee (BG-10)
  Result.Payee := _nodeAsParty(doc.documentElement, '//cac:PayeeParty');

  // Payment Reference
  Result.PaymentReference := TZUGFeRDXmlUtils.NodeAsString(doc.documentElement, '//cac:PaymentMeans/cbc:PaymentID');

  // Currency (BT-5)
  Result.Currency := TEnumExtensions<TZUGFeRDCurrencyCodes>.StringToEnum(
    TZUGFeRDXmlUtils.NodeAsString(doc.documentElement, '//cbc:DocumentCurrencyCode'));

  // Tax Currency (BT-6)
  var optionalTaxCurrency : ZUGFeRDNullable<TZUGFeRDCurrencyCodes> :=
    TEnumExtensions<TZUGFeRDCurrencyCodes>.StringToNullableEnum(
      TZUGFeRDXmlUtils.NodeAsString(doc.documentElement, '//cbc:TaxCurrencyCode'));
  if optionalTaxCurrency.HasValue then
    Result.TaxCurrency := optionalTaxCurrency;

  // PaymentMeans (BG-16)
  Result.PaymentMeans := TZUGFeRDPaymentMeans.Create;
  Result.PaymentMeans.TypeCode := TEnumExtensions<TZUGFeRDPaymentMeansTypeCodes>.StringToNullableEnum(
    TZUGFeRDXmlUtils.NodeAsString(doc.documentElement, '//cac:PaymentMeans/cbc:PaymentMeansCode'));
  Result.PaymentMeans.Information := TZUGFeRDXmlUtils.NodeAsString(doc.documentElement, '//cac:PaymentMeans/cbc:PaymentMeansCode/@name');
  Result.PaymentMeans.SEPACreditorIdentifier := TZUGFeRDXmlUtils.NodeAsString(doc.documentElement,
    '//cac:AccountingSupplierParty/cac:Party/cac:PartyIdentification/cbc:ID[@schemeID=''SEPA'']');
  Result.PaymentMeans.SEPAMandateReference := TZUGFeRDXmlUtils.NodeAsString(doc.documentElement,
    '//cac:PaymentMeans/cac:PaymentMandate/cbc:ID');

  var financialCardId : string := TZUGFeRDXmlUtils.NodeAsString(doc.documentElement,
    '//cac:PaymentMeans/cac:CardAccount/cbc:PrimaryAccountNumberID');
  var financialCardCardholderName : string := TZUGFeRDXmlUtils.NodeAsString(doc.documentElement,
    '//cac:PaymentMeans/cac:CardAccount/cbc:HolderName');

  if (financialCardId <> '') or (financialCardCardholderName <> '') then
  begin
    Result.PaymentMeans.FinancialCard := TZUGFeRDFinancialCard.Create;
    Result.PaymentMeans.FinancialCard.Id := financialCardId;
    Result.PaymentMeans.FinancialCard.CardholderName := financialCardCardholderName;
  end;

  // Billing Period
  Result.BillingPeriodStart := TZUGFeRDXmlUtils.NodeAsDateTime(doc.documentElement, '/*[1]/cac:InvoicePeriod/cbc:StartDate');
  Result.BillingPeriodEnd := TZUGFeRDXmlUtils.NodeAsDateTime(doc.documentElement, '/*[1]/cac:InvoicePeriod/cbc:EndDate');

  // Creditor Bank Accounts (BG-17)
  nodes := doc.selectNodes('//cac:PaymentMeans/cac:PayeeFinancialAccount');
  for i := 0 to nodes.length - 1 do
  begin
    var bankAccount : TZUGFeRDBankAccount := _nodeAsBankAccount(nodes[i], '.');
    if bankAccount <> nil then
      Result.CreditorBankAccounts.Add(bankAccount);
  end;

  // Debitor Bank Accounts (BG-19)
  nodes := doc.selectNodes('//cac:PaymentMeans/cac:PaymentMandate/cac:PayerFinancialAccount');
  for i := 0 to nodes.length - 1 do
  begin
    var bankAccount : TZUGFeRDBankAccount := _nodeAsBankAccount(nodes[i], '.');
    if bankAccount <> nil then
      Result.DebitorBankAccounts.Add(bankAccount);
  end;

  // Taxes (BG-23)
  nodes := doc.selectNodes('/*/cac:TaxTotal/cac:TaxSubtotal');
  for i := 0 to nodes.length - 1 do
  begin
    var taxableAmount : Currency := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], 'cbc:TaxableAmount', 0);
    var percent : Currency := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], 'cac:TaxCategory/cbc:Percent', 0);
    var taxAmount : Currency := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], 'cbc:TaxAmount', 0);
    var taxType : TZUGFeRDTaxTypes := TEnumExtensions<TZUGFeRDTaxTypes>.StringToEnum(
      TZUGFeRDXmlUtils.NodeAsString(nodes[i], 'cac:TaxCategory/cac:TaxScheme/cbc:ID'));
    var categoryCode : TZUGFeRDTaxCategoryCodes := TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum(
      TZUGFeRDXmlUtils.NodeAsString(nodes[i], 'cac:TaxCategory/cbc:ID'));
    var exemptionReasonCode : ZUGFeRDNullable<TZUGFeRDTaxExemptionReasonCodes> :=
      TEnumExtensions<TZUGFeRDTaxExemptionReasonCodes>.StringToNullableEnum(
        TZUGFeRDXmlUtils.NodeAsString(nodes[i], 'cac:TaxCategory/cbc:TaxExemptionReasonCode'));
    var exemptionReason : string := TZUGFeRDXmlUtils.NodeAsString(nodes[i], 'cac:TaxCategory/cbc:TaxExemptionReason');

    Result.AddApplicableTradeTax(taxAmount, taxableAmount, percent, taxType, categoryCode,
      nil, exemptionReasonCode, exemptionReason);
  end;

  // Document-level AllowanceCharges (BG-20, BG-21)
  // Use baseNode with direct child selector to avoid getting line-level ones
  nodes := baseNode.selectNodes('cac:AllowanceCharge');
  for i := 0 to nodes.length - 1 do
  begin
    var chargePercentage : ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/cbc:MultiplierFactorNumeric');
    var basisAmount : ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/cbc:BaseAmount');
    var actualAmount : ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/cbc:Amount', 0);
    var reason : string := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/cbc:AllowanceChargeReason');
    var taxTypeCode : ZUGFeRDNullable<TZUGFeRDTaxTypes> := TEnumExtensions<TZUGFeRDTaxTypes>.StringToNullableEnum(
      TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/cac:TaxCategory/cac:TaxScheme/cbc:ID'));
    var taxCategoryCode : ZUGFeRDNullable<TZUGFeRDTaxCategoryCodes> := TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToNullableEnum(
      TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/cac:TaxCategory/cbc:ID'));
    var taxPercent : ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/cac:TaxCategory/cbc:Percent', 0);

    if TZUGFeRDXmlUtils.NodeAsBool(nodes[i], './/cbc:ChargeIndicator') then // charge
    begin
      var chargeReasonCode : ZUGFeRDNullable<TZUGFeRDChargeReasonCodes> := TEnumExtensions<TZUGFeRDChargeReasonCodes>.StringToNullableEnum(
        TZUGFeRDXmlUtils.NodeAsString(nodes[i], './cbc:AllowanceChargeReasonCode'));
      Result.AddTradeCharge(basisAmount, Result.Currency, actualAmount, chargePercentage, reason, taxTypeCode, taxCategoryCode, taxPercent, chargeReasonCode);
    end
    else // allowance
    begin
      var allowanceReasonCode : ZUGFeRDNullable<TZUGFeRDAllowanceReasonCodes> := TEnumExtensions<TZUGFeRDAllowanceReasonCodes>.StringToNullableEnum(
        TZUGFeRDXmlUtils.NodeAsString(nodes[i], './cbc:AllowanceChargeReasonCode'));
      Result.AddTradeAllowance(basisAmount, Result.Currency, actualAmount, chargePercentage, reason, taxTypeCode, taxCategoryCode, taxPercent, allowanceReasonCode);
    end;
  end;

  // Invoice Referenced Documents (BG-3)
  nodes := doc.documentElement.selectNodes('//cac:BillingReference/cac:InvoiceDocumentReference');
  for i := 0 to nodes.length - 1 do
  begin
    Result.AddInvoiceReferencedDocument(
      TZUGFeRDXmlUtils.NodeAsString(nodes[i], './cbc:ID'),
      TZUGFeRDXmlUtils.NodeAsDateTime(nodes[i], './cbc:IssueDate'));
    break; // only one occurrence allowed in UBL
  end;

  // Despatch Document Reference (BT-16)
  node := baseNode.selectSingleNode('cac:DespatchDocumentReference/cbc:ID');
  if node <> nil then
    Result.SetDespatchAdviceReferencedDocument(node.text);

  // Payment Terms
  Result.AddTradePaymentTerms(
    TZUGFeRDXmlUtils.NodeAsString(doc.documentElement, '//cac:PaymentTerms/cbc:Note'),
    TZUGFeRDXmlUtils.NodeAsDateTime(doc.documentElement, '//cbc:DueDate'));

  // Monetary Totals
  Result.LineTotalAmount := TZUGFeRDXmlUtils.NodeAsDecimal(doc.documentElement, '//cac:LegalMonetaryTotal/cbc:LineExtensionAmount');
  Result.ChargeTotalAmount := TZUGFeRDXmlUtils.NodeAsDecimal(doc.documentElement, '//cac:LegalMonetaryTotal/cbc:ChargeTotalAmount');
  Result.AllowanceTotalAmount := TZUGFeRDXmlUtils.NodeAsDecimal(doc.documentElement, '//cac:LegalMonetaryTotal/cbc:AllowanceTotalAmount');
  Result.TaxBasisAmount := TZUGFeRDXmlUtils.NodeAsDecimal(doc.documentElement, '//cac:LegalMonetaryTotal/cbc:TaxExclusiveAmount');
  Result.TaxTotalAmount := TZUGFeRDXmlUtils.NodeAsDecimal(doc.documentElement, '//cac:TaxTotal/cbc:TaxAmount');
  Result.GrandTotalAmount := TZUGFeRDXmlUtils.NodeAsDecimal(doc.documentElement, '//cac:LegalMonetaryTotal/cbc:TaxInclusiveAmount');
  Result.RoundingAmount := TZUGFeRDXmlUtils.NodeAsDecimal(doc.documentElement, '//cac:LegalMonetaryTotal/cbc:PayableRoundingAmount');
  Result.TotalPrepaidAmount := TZUGFeRDXmlUtils.NodeAsDecimal(doc.documentElement, '//cac:LegalMonetaryTotal/cbc:PrepaidAmount');
  Result.DuePayableAmount := TZUGFeRDXmlUtils.NodeAsDecimal(doc.documentElement, '//cac:LegalMonetaryTotal/cbc:PayableAmount');

  // Accounting Cost
  nodes := doc.selectNodes('//cbc:AccountingCost');
  for i := 0 to nodes.length - 1 do
  begin
    var content : string := TZUGFeRDXmlUtils.NodeAsString(nodes[i], '.');
    if content.Trim <> '' then
      Result.AddReceivableSpecifiedTradeAccountingAccount(content);
  end;

  // Order Reference (BT-13)
  Result.OrderNo := TZUGFeRDXmlUtils.NodeAsString(doc.documentElement, '//cac:OrderReference/cbc:ID');

  // Seller Order Referenced Document (BT-14)
  if doc.selectSingleNode('//cac:OrderReference/cbc:SalesOrderID') <> nil then
  begin
    Result.SellerOrderReferencedDocument := TZUGFeRDSellerOrderReferencedDocument.Create;
    Result.SellerOrderReferencedDocument.ID := TZUGFeRDXmlUtils.NodeAsString(doc.documentElement, '//cac:OrderReference/cbc:SalesOrderID');
  end;

  // Contract Referenced Document (BT-12)
  if doc.selectSingleNode('//cac:ContractDocumentReference/cbc:ID') <> nil then
  begin
    Result.ContractReferencedDocument := TZUGFeRDContractReferencedDocument.Create;
    Result.ContractReferencedDocument.ID := TZUGFeRDXmlUtils.NodeAsString(doc.documentElement, '//cac:ContractDocumentReference/cbc:ID');
  end;

  // Additional Document References (BG-24)
  nodes := doc.selectNodes('//cac:AdditionalDocumentReference');
  for i := 0 to nodes.length - 1 do
  begin
    var document : TZUGFeRDAdditionalReferencedDocument := TZUGFeRDAdditionalReferencedDocument.Create(false);
    document.ID := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/cbc:ID');
    document.ReferenceTypeCode := TEnumExtensions<TZUGFeRDReferenceTypeCodes>.StringToNullableEnum(
      TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/cbc:ID/@schemeID'));
    document.TypeCode := TEnumExtensions<TZUGFeRDAdditionalReferencedDocumentTypeCode>.StringToEnum(
      TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/cbc:DocumentTypeCode'));
    document.Name := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/cbc:DocumentDescription');

    var binaryObjectNode : IXMLDOMNode := nodes[i].selectSingleNode('.//cac:Attachment/cbc:EmbeddedDocumentBinaryObject');
    if binaryObjectNode <> nil then
    begin
      var filenameAttr : IXMLDOMNode := binaryObjectNode.attributes.getNamedItem('filename');
      if filenameAttr <> nil then
        document.Filename := filenameAttr.text;
      var strBase64BinaryData : string := binaryObjectNode.text;
      if strBase64BinaryData <> '' then
      begin
        document.AttachmentBinaryObject := TMemoryStream.Create;
        var strBase64BinaryDataBytes : TBytes := TNetEncoding.Base64String.DecodeStringToBytes(strBase64BinaryData);
        document.AttachmentBinaryObject.Write(strBase64BinaryDataBytes, Length(strBase64BinaryDataBytes));
      end;
    end;

    Result.AdditionalReferencedDocuments.Add(document);
  end;

  // Project Reference (BT-11)
  Result.SpecifiedProcuringProject := TZUGFeRDSpecifiedProcuringProject.Create;
  Result.SpecifiedProcuringProject.ID := TZUGFeRDXmlUtils.NodeAsString(doc.documentElement, '//cac:ProjectReference/cbc:ID');
  Result.SpecifiedProcuringProject.Name := '';

  // Trade Line Items
  nodes := doc.selectNodes(tradeLineItemSelector);
  for i := 0 to nodes.length - 1 do
  begin
    var item : TZUGFeRDTradeLineItem := _parseTradeLineItem(nodes[i], isInvoice);
    if item <> nil then
    begin
      Result.TradeLineItems.Add(item);

      // Handle sub-invoice lines recursively
      var subSelector : string;
      if isInvoice then
        subSelector := './/cac:SubInvoiceLine'
      else
        subSelector := './/cac:SubCreditNoteLine';

      var subNodes : IXMLDOMNodeList := nodes[i].selectNodes(subSelector);
      if subNodes <> nil then
        for var j : Integer := 0 to subNodes.length - 1 do
        begin
          var subItem : TZUGFeRDTradeLineItem := _parseTradeLineItem(subNodes[j], isInvoice, item.AssociatedDocument.LineID);
          if subItem <> nil then
          begin
            // Check for duplicates
            var isDuplicate : Boolean := False;
            for var k : Integer := 0 to Result.TradeLineItems.Count - 1 do
              if Result.TradeLineItems[k].AssociatedDocument.LineID = subItem.AssociatedDocument.LineID then
              begin
                isDuplicate := True;
                subItem.Free;
                break;
              end;
            if not isDuplicate then
              Result.TradeLineItems.Add(subItem);
          end;
        end;
    end;
  end;
end;

function TZUGFeRDInvoiceDescriptor22UBLReader._nodeAsLegalOrganization(
  basenode: IXmlDomNode; const xpath: string) : TZUGFeRDLegalOrganization;
var
  node : IXmlDomNode;
begin
  Result := nil;
  if baseNode = nil then
    exit;
  node := baseNode.SelectSingleNode(xpath);
  if node = nil then
    exit;
  Result := TZUGFeRDLegalOrganization.CreateWithParams(
    TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.StringToNullableEnum(
      TZUGFeRDXmlUtils.NodeAsString(node, 'cbc:CompanyID/@schemeID')),
    TZUGFeRDXmlUtils.NodeAsString(node, 'cbc:CompanyID'),
    TZUGFeRDXmlUtils.NodeAsString(node, 'cbc:RegistrationName'));
end;

function TZUGFeRDInvoiceDescriptor22UBLReader._nodeAsParty(basenode: IXmlDomNode;
  const xpath: string) : TZUGFeRDParty;
var
  node : IXmlDomNode;
begin
  Result := nil;
  if baseNode = nil then
    exit;
  node := baseNode.SelectSingleNode(xpath);
  if node = nil then
    exit;

  Result := _nodeAsAddressParty(node, 'cac:PostalAddress');
  if Result = nil then
    Result := TZUGFeRDParty.Create;

  // Party Identification - route GLN to GlobalID, others to ID
  var id : TZUGFeRDGlobalID := TZUGFeRDGlobalID.CreateWithParams(
    TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.StringToNullableEnum(
      TZUGFeRDXmlUtils.NodeAsString(node, 'cac:PartyIdentification/cbc:ID/@schemeID')),
    TZUGFeRDXmlUtils.NodeAsString(node, 'cac:PartyIdentification/cbc:ID'));

  if id.SchemeID.HasValue and (id.SchemeID.Value = TZUGFeRDGlobalIDSchemeIdentifiers.GLN) then
  begin
    Result.ID := TZUGFeRDGlobalID.Create;
    Result.GlobalID := id;
  end
  else
  begin
    Result.ID := id;
    Result.GlobalID := TZUGFeRDGlobalID.Create;
  end;

  Result.Name := TZUGFeRDXmlUtils.NodeAsString(node, 'cac:PartyName/cbc:Name');
  Result.SpecifiedLegalOrganization := _nodeAsLegalOrganization(node, 'cac:PartyLegalEntity');

  if Result.Description.Trim = '' then
    Result.Description := TZUGFeRDXmlUtils.NodeAsString(node, 'cac:PartyLegalEntity/cbc:CompanyLegalForm');
end;

function TZUGFeRDInvoiceDescriptor22UBLReader._nodeAsAddressParty(basenode: IXmlDomNode;
  const xpath: string) : TZUGFeRDParty;
var
  node : IXmlDomNode;
begin
  Result := nil;
  if baseNode = nil then
    exit;
  node := baseNode.SelectSingleNode(xpath);
  if node = nil then
    exit;

  Result := TZUGFeRDParty.Create;
  Result.Street := TZUGFeRDXmlUtils.NodeAsString(node, 'cbc:StreetName');
  Result.AddressLine3 := TZUGFeRDXmlUtils.NodeAsString(node, 'cbc:AdditionalStreetName');
  Result.City := TZUGFeRDXmlUtils.NodeAsString(node, 'cbc:CityName');
  Result.Postcode := TZUGFeRDXmlUtils.NodeAsString(node, 'cbc:PostalZone');
  Result.CountrySubdivisionName := TZUGFeRDXmlUtils.NodeAsString(node, 'cbc:CountrySubentity');
  Result.Country := TEnumExtensions<TZUGFeRDCountryCodes>.StringToNullableEnum(
    TZUGFeRDXmlUtils.NodeAsString(node, 'cac:Country/cbc:IdentificationCode'));

  var addressLine2 : string := TZUGFeRDXmlUtils.NodeAsString(node, 'cac:AddressLine/cbc:Line');
  if addressLine2.Trim <> '' then
  begin
    if Result.AddressLine3.Trim = '' then
      Result.AddressLine3 := addressLine2
    else if Result.ContactName.Trim = '' then
      Result.ContactName := addressLine2;
  end;
end;

function TZUGFeRDInvoiceDescriptor22UBLReader._nodeAsBankAccount(basenode: IXmlDomNode;
  const xpath: string) : TZUGFeRDBankAccount;
var
  node : IXmlDomNode;
begin
  Result := nil;
  if baseNode = nil then
    exit;
  node := baseNode.SelectSingleNode(xpath);
  if node = nil then
    exit;

  Result := TZUGFeRDBankAccount.Create;
  Result.Name := TZUGFeRDXmlUtils.NodeAsString(node, 'cbc:Name');
  Result.IBAN := TZUGFeRDXmlUtils.NodeAsString(node, 'cbc:ID');
  Result.BIC := TZUGFeRDXmlUtils.NodeAsString(node, 'cac:FinancialInstitutionBranch/cbc:ID');
  Result.BankName := TZUGFeRDXmlUtils.NodeAsString(node, 'cac:FinancialInstitutionBranch/cbc:Name');
  Result.ID := '';
end;

function TZUGFeRDInvoiceDescriptor22UBLReader._parseTradeLineItem(
  tradeLineItem: IXmlDomNode; isInvoice: Boolean; const parentLineId: string = ''): TZUGFeRDTradeLineItem;
var
  nodes : IXMLDOMNodeList;
  i : Integer;
begin
  Result := nil;

  if tradeLineItem = nil then
    exit;

  var lineId : string := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/cbc:ID');

  var billedQuantity : ZUGFeRDNullable<Currency>;
  var unitCode : ZUGFeRDNullable<TZUGFeRDQuantityCodes>;
  if isInvoice then
  begin
    billedQuantity := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/cbc:InvoicedQuantity');
    unitCode := TEnumExtensions<TZUGFeRDQuantityCodes>.StringToNullableEnum(
      TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/cbc:InvoicedQuantity/@unitCode'));
  end
  else
  begin
    billedQuantity := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/cbc:CreditedQuantity');
    unitCode := TEnumExtensions<TZUGFeRDQuantityCodes>.StringToNullableEnum(
      TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/cbc:CreditedQuantity/@unitCode'));
  end;

  Result := TZUGFeRDTradeLineItem.Create(lineId);

  Result.GlobalID.SchemeID := TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.StringToNullableEnum(
    TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './cac:Item/cac:StandardItemIdentification/cbc:ID/@schemeID'));
  Result.GlobalID.ID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './cac:Item/cac:StandardItemIdentification/cbc:ID');
  Result.SellerAssignedID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './cac:Item/cac:SellersItemIdentification/cbc:ID');
  Result.BuyerAssignedID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './cac:Item/cac:BuyersItemIdentification/cbc:ID');
  Result.Name := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './cac:Item/cbc:Name');
  Result.Description := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/cac:Item/cbc:Description');
  Result.NetQuantity := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/cac:Price/cbc:BaseQuantity');
  if billedQuantity.HasValue then
    Result.BilledQuantity := billedQuantity.Value
  else
    Result.BilledQuantity := 0;
  Result.LineTotalAmount := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/cbc:LineExtensionAmount', 0);
  Result.TaxCategoryCode := TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToNullableEnum(
    TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/cac:Item/cac:ClassifiedTaxCategory/cbc:ID'));
  Result.TaxType := TEnumExtensions<TZUGFeRDTaxTypes>.StringToNullableEnum(
    TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/cac:Item/cac:ClassifiedTaxCategory/cac:TaxScheme/cbc:ID'));
  Result.TaxPercent := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/cac:Item/cac:ClassifiedTaxCategory/cbc:Percent', 0);
  Result.NetUnitPrice := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/cac:Price/cbc:PriceAmount', 0);
  Result.GrossUnitPrice := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/cac:Price/cbc:PriceAmount', 0);
  Result.UnitCode := unitCode;
  Result.BillingPeriodStart := TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/cac:InvoicePeriod/cbc:StartDate');
  Result.BillingPeriodEnd := TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/cac:InvoicePeriod/cbc:EndDate');

  if parentLineId <> '' then
    Result.SetParentLineId(parentLineId);

  // Commodity Classifications
  nodes := tradeLineItem.selectNodes('.//cac:Item/cac:CommodityClassification/cbc:ItemClassificationCode');
  if nodes <> nil then
    for i := 0 to nodes.length - 1 do
    begin
      var listID : TZUGFeRDDesignatedProductClassificationClassCodes :=
        TEnumExtensions<TZUGFeRDDesignatedProductClassificationClassCodes>.StringToEnum(
          TZUGFeRDXmlUtils.NodeAsString(nodes[i], './@listID'));
      var listVersionID : string := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './@listVersionID');
      Result.AddDesignatedProductClassification(listID, listVersionID, nodes[i].text, '');
    end;

  // Product Characteristics (BG-32)
  nodes := tradeLineItem.selectNodes('.//cac:Item/cac:AdditionalItemProperty');
  if nodes <> nil then
    for i := 0 to nodes.length - 1 do
    begin
      var apcItem : TZUGFeRDApplicableProductCharacteristic := TZUGFeRDApplicableProductCharacteristic.Create;
      apcItem.Description := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/cbc:Name');
      apcItem.Value := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/cbc:Value');
      Result.ApplicableProductCharacteristics.Add(apcItem);
    end;

  // Buyer Order Referenced Document
  if tradeLineItem.selectSingleNode('cac:OrderLineReference') <> nil then
  begin
    Result.BuyerOrderReferencedDocument := TZUGFeRDBuyerOrderReferencedDocument.Create;
    Result.BuyerOrderReferencedDocument.ID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/cac:OrderLineReference/cbc:LineID');
    Result.BuyerOrderReferencedDocument.LineID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/cac:OrderLineReference/cbc:LineID');
  end;

  // Additional Referenced Documents
  nodes := tradeLineItem.selectNodes('.//cac:DocumentReference');
  if nodes <> nil then
    for i := 0 to nodes.length - 1 do
    begin
      var document : TZUGFeRDAdditionalReferencedDocument := TZUGFeRDAdditionalReferencedDocument.Create(false);
      document.ID := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/cbc:ID');
      document.ReferenceTypeCode := TEnumExtensions<TZUGFeRDReferenceTypeCodes>.StringToNullableEnum(
        TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/cbc:ID/@schemeID'));
      document.TypeCode := TEnumExtensions<TZUGFeRDAdditionalReferencedDocumentTypeCode>.StringToEnum(
        TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/cbc:DocumentTypeCode'));
      document.Name := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/cbc:DocumentDescription');

      var binaryObjectNode : IXMLDOMNode := nodes[i].selectSingleNode('.//cac:Attachment/cbc:EmbeddedDocumentBinaryObject');
      if binaryObjectNode <> nil then
      begin
        var filenameAttr : IXMLDOMNode := binaryObjectNode.attributes.getNamedItem('filename');
        if filenameAttr <> nil then
          document.Filename := filenameAttr.text;
        var strBase64BinaryData : string := binaryObjectNode.text;
        if strBase64BinaryData <> '' then
        begin
          document.AttachmentBinaryObject := TMemoryStream.Create;
          var strBase64BinaryDataBytes : TBytes := TNetEncoding.Base64String.DecodeStringToBytes(strBase64BinaryData);
          document.AttachmentBinaryObject.Write(strBase64BinaryDataBytes, Length(strBase64BinaryDataBytes));
        end;
      end;

      Result.AdditionalReferencedDocuments.Add(document);
    end;

  // Notes
  nodes := tradeLineItem.selectNodes('.//cbc:Note');
  if nodes <> nil then
    for i := 0 to nodes.length - 1 do
      Result.AssociatedDocument.Notes.Add(TZUGFeRDNote.Create(nodes[i].text));

  // Line-level AllowanceCharges
  nodes := tradeLineItem.selectNodes('./cac:AllowanceCharge');
  if nodes <> nil then
    for i := 0 to nodes.length - 1 do
    begin
      var chargeIndicator : Boolean := TZUGFeRDXmlUtils.NodeAsBool(nodes[i], './cbc:ChargeIndicator');
      var basisAmount : ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './cbc:BaseAmount');
      var basisAmountCurrency : string := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './cbc:BaseAmount/@currencyID');
      var actualAmount : Currency := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './cbc:Amount', 0);
      var reason : string := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './cbc:AllowanceChargeReason');
      var chargePercentage : ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './cbc:MultiplierFactorNumeric');

      if chargeIndicator then // charge
        Result.AddTradeCharge(
          TEnumExtensions<TZUGFeRDCurrencyCodes>.StringToEnum(basisAmountCurrency),
          basisAmount, actualAmount, chargePercentage, reason)
      else // allowance
        Result.AddTradeAllowance(
          TEnumExtensions<TZUGFeRDCurrencyCodes>.StringToEnum(basisAmountCurrency),
          basisAmount, actualAmount, chargePercentage, reason);
    end;

  // UnitCode fallback
  if not Result.UnitCode.HasValue then
  begin
    if isInvoice then
      Result.UnitCode := TEnumExtensions<TZUGFeRDQuantityCodes>.StringToNullableEnum(
        TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/cbc:InvoicedQuantity/@unitCode'))
    else
      Result.UnitCode := TEnumExtensions<TZUGFeRDQuantityCodes>.StringToNullableEnum(
        TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/cbc:CreditedQuantity/@unitCode'));
  end;
end;

end.
