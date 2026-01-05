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

unit intf.ZUGFeRDInvoiceDescriptor23CIIReader;

interface

uses
  System.SysUtils, System.Classes, System.DateUtils, System.Math, System.Generics.Collections,
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
  ,intf.ZUGFeRDPaymentMeans,intf.ZUGFeRDPaymentMeansTypeCodes
  ,intf.ZUGFeRDFinancialCard
  ,intf.ZUGFeRDBankAccount
  ,intf.ZUGFeRDTaxTypes,intf.ZUGFeRDTaxCategoryCodes
  ,intf.ZUGFeRDDateTypeCodes
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
  ,intf.ZUGFeRDDespatchAdviceReferencedDocument
  ,intf.ZUGFeRDDesignatedProductClassificationClassCodes
  ,intf.ZUGFeRDXmlUtils
  ,intf.ZUGFeRDIncludedReferencedProduct
  ,intf.ZUGFeRDChargeReasonCodes
  ,intf.ZUGFeRDAllowanceReasonCodes
  ,intf.ZUGFeRDTradeDeliveryTermCodes
  ,intf.ZUGFeRDTransportmodeCodes
  ,intf.ZUGFeRDLineStatusCodes
  ,intf.ZUGFeRDLineStatusReasonCodes
  ;

type
  TZUGFeRDInvoiceDescriptor23CIIReader = class(TZUGFeRDIInvoiceDescriptorReader)
  private
    function GetValidURIs : TArray<string>;
    function _parseTradeLineItem(tradeLineItem : IXmlDomNode {nsmgr: XmlNamespaceManager = nil; }) : TZUGFeRDTradeLineItem;
    function _nodeAsContact(basenode: IXmlDomNode; const xpath: string) : TZUGFeRDContact;
    function _nodeAsParty(basenode: IXmlDomNode; const xpath: string) : TZUGFeRDParty;
    function _getAdditionalReferencedDocument(a_oXmlNode : IXmlDomNode {nsmgr: XmlNamespaceManager = nil; }) : TZUGFeRDAdditionalReferencedDocument;
    function _nodeAsLegalOrganization(basenode: IXmlDomNode; const xpath: string) : TZUGFeRDLegalOrganization;
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

{ TZUGFeRDInvoiceDescriptor23CIIReader }

constructor TZUGFeRDInvoiceDescriptor23CIIReader.Create;
begin
  inherited;
  FNamespaces := TDictionary<string, string>.Create;
  FNamespaces.Add('rsm', 'urn:un:unece:uncefact:data:standard:CrossIndustryInvoice:100');
  FNamespaces.Add('ram', 'urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:100');
  FNamespaces.Add('udt', 'urn:un:unece:uncefact:data:standard:UnqualifiedDataType:100');
  FNamespaces.Add('qdt', 'urn:un:unece:uncefact:data:standard:QualifiedDataType:100');
  FNamespaces.Add('a', 'urn:un:unece:uncefact:data:standard:QualifiedDataType:100');
  FNamespaces.Add('xs', 'http://www.w3.org/2001/XMLSchema');
end;

destructor TZUGFeRDInvoiceDescriptor23CIIReader.Destroy;
begin
  FNamespaces.Free;
  inherited;
end;

function TZUGFeRDInvoiceDescriptor23CIIReader.GetValidURIs : TArray<string>;
begin
  Result := TArray<string>.Create(
    'urn:cen.eu:en16931:2017#conformant#urn:factur-x.eu:1p0:extended', // Factur-X 1.03 EXTENDED
    'urn:cen.eu:en16931:2017',  // Profil EN 16931 (COMFORT)
    'urn:cen.eu:en16931:2017#compliant#urn:factur-x.eu:1p0:basic', // BASIC
    'urn:factur-x.eu:1p0:basicwl', // BASIC WL
    'urn:factur-x.eu:1p0:minimum', // MINIMUM
    'urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_1.2', // XRechnung 1.2
    'urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_2.0', // XRechnung 2.0
    'urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_2.1', // XRechnung 2.1
    'urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_2.2', // XRechnung 2.2
    'urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_2.3', // XRechnung 2.3
    'urn:cen.eu:en16931:2017#compliant#urn:xeinkauf.de:kosit:xrechnung_3.0', // XRechnung 3.0
    'urn:cen.eu:en16931:2017#compliant#urn:xeinkauf.de:kosit:xrechnung_3.0#conformant#urn:xeinkauf.de:kosit:extension:xrechnung_3.0', // XRechnung 3.0
    'urn.cpro.gouv.fr:1p0:ereporting' //Factur-X E-reporting
  );
end;

function TZUGFeRDInvoiceDescriptor23CIIReader.IsReadableByThisReaderVersion(
  stream: TStream): Boolean;
begin
  Result := IsReadableByThisReaderVersion(stream, GetValidURIs);
end;

function TZUGFeRDInvoiceDescriptor23CIIReader.Load(stream: TStream): TZUGFeRDInvoiceDescriptor;
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

function TryReadXRechnungPaymentTerms(invoice: TZUGFeRDInvoiceDescriptor; nodes : IXMLDOMNodeList): boolean;
begin
  // try to find if Payment Terms are given in #SKONTO# or #VERZUG# format in the description field
  Result:= false;
  for var i: Integer := 0 to nodes.length-1 do
  begin
    var Description: string := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Description');
    if (Pos('#SKONTO#', Description)>0)
    or (Pos('#VERZUG#', Description)>0) then
      Result:= true;
  end;
  // if not, reading is done as usual
  if Not(Result) then
    exit;

  // we handle lines of the following format
  // #SKONTO#TAGE=2#PROZENT=2.00#BASISBETRAG=252.94#
  // #SKONTO#TAGE=4#PROZENT=4.01#
  // #VERZUG#TAGE=14#PROZENT=1.0#

  for var i: Integer := 0 to nodes.length-1 do
  begin
    var Terms: TStringList := TStringList.Create;
    try
      var Description: string := '';
      var Lines: TStringList := TStringList.Create;
      try
        Lines.Text := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Description');
        for var Line in Lines do
        begin
          if (Length(Line)>0) and (Line[1]='#') then
            Terms.Add(Line)
          else // preserve text descriptions
            if Description='' then
              Description:= Line
            else
              Description:= Description+#13#10+Line
        end;
      finally
        Lines.Free
      end;
      var first: boolean := true;
      for var Term in Terms do
      begin
         var paymentTerm : TZUGFeRDPaymentTerms := TZUGFeRDPaymentTerms.Create;
         if first then
         begin
           paymentTerm.Description:= Description;
           paymentTerm.DueDate:= TZUGFeRDXmlUtils.NodeAsDateTime(nodes[i], './/ram:DueDateDateTime/udt:DateTimeString');
           first:= false;
         end;
         var Parts: TArray<string> := Term.Split(['#']); // 0 Leer, 1 SKONTO/VERZUG, 2 TAGE, 3 PROZENT, 4 Leer or BASISWERT
         if Length(Parts)>=4 then
         begin
           if Parts[1]='SKONTO' then
             paymentTerm.PaymentTermsType:= TZUGFeRDPaymentTermsType.Skonto
           else
           if Parts[1]='VERZUG' then
             paymentTerm.PaymentTermsType:= TZUGFeRDPaymentTermsType.Verzug;
           if copy(Parts[2], 1, 5)='TAGE=' then
             paymentTerm.DueDays:= StrToIntDef(Copy(Parts[2], 6, Length(Parts[2])), 0);
           if copy(Parts[3], 1, 8)='PROZENT=' then
             paymentTerm.Percentage:= StrToFloatDef(Copy(Parts[3], 9, Length(Parts[3])), 0, TFormatSettings.Invariant);
           if copy(Parts[4], 1, 12)='BASISBETRAG=' then
             paymentTerm.BaseAmount:= StrToCurrDef(copy(Parts[4], 13, Length(Parts[4])), 0, TFormatSettings.Invariant);
         end;
        invoice.PaymentTermsList.Add(paymentTerm);
      end;
    finally
      Terms.Free;
    end
  end;

end;

function TZUGFeRDInvoiceDescriptor23CIIReader.Load(xmldocument : IXMLDocument): TZUGFeRDInvoiceDescriptor;
var
  doc : IXMLDOMDocument2;
  // node : IXMLDOMNode;
  nodes : IXMLDOMNodeList;
  i : Integer;
begin
  doc := TZUGFeRDXmlHelper.PrepareDocumentForXPathQuerys(xmldocument);
  // XmlNamespaceManager nsmgr = _CreateFixedNamespaceManager(doc);
  // if not(nsmgr.HasNamespace('rsm')) then
  //   nsmgr.AddNamespace('rsm', nsmgr.DefaultNamespace);

  Result := TZUGFeRDInvoiceDescriptor.Create;

  Result.IsTest := TZUGFeRDXmlUtils.NodeAsBool(doc.documentElement,'//*[local-name()="ExchangedDocumentContext"]/ram:TestIndicator/udt:Indicator',false);
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

  if doc.selectSingleNode('//ram:SellerTradeParty/ram:URIUniversalCommunication') <> nil then
  begin
    var id : String := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:SellerTradeParty/ram:URIUniversalCommunication/ram:URIID');
    var schemeID : String := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:SellerTradeParty/ram:URIUniversalCommunication/ram:URIID/@schemeID');

    var eas : ZUGFeRDNullable<TZUGFeRDElectronicAddressSchemeIdentifiers> := TEnumExtensions<TZUGFeRDElectronicAddressSchemeIdentifiers>.StringToEnum(schemeID);
    if eas.HasValue then
      Result.SetSellerElectronicAddress(id, eas);
  end;

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

  if (doc.SelectSingleNode('//ram:BuyerTradeParty/ram:URIUniversalCommunication') <> nil) then
  begin
    var id : String := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:BuyerTradeParty/ram:URIUniversalCommunication/ram:URIID');
    var schemeID : String := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:BuyerTradeParty/ram:URIUniversalCommunication/ram:URIID/@schemeID');

    var eas : ZUGFeRDNullable<TZUGFeRDElectronicAddressSchemeIdentifiers> := TEnumExtensions<TZUGFeRDElectronicAddressSchemeIdentifiers>.StringToEnum(schemeID);

    if eas.HasValue then
      Result.SetBuyerElectronicAddress(id, eas);
  end;

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
    Result.BuyerContact.Name := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:BuyerTradeParty/ram:DefinedTradeContact/ram:PersonName');
    Result.BuyerContact.OrgUnit := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:BuyerTradeParty/ram:DefinedTradeContact/ram:DepartmentName');
    Result.BuyerContact.PhoneNo := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:BuyerTradeParty/ram:DefinedTradeContact/ram:TelephoneUniversalCommunication/ram:CompleteNumber');
    Result.BuyerContact.FaxNo := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:BuyerTradeParty/ram:DefinedTradeContact/ram:FaxUniversalCommunication/ram:CompleteNumber');
    Result.BuyerContact.EmailAddress := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:BuyerTradeParty/ram:DefinedTradeContact/ram:EmailURIUniversalCommunication/ram:URIID');
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

   // TODO: ProductEndUserTradeParty Detailinformationen zum abweichenden Endverbraucher, BG-X-18

  Result.ApplicableTradeDeliveryTermsCode:= TEnumExtensions<TZUGFeRDTradeDeliveryTermCodes>.StringToNullableEnum(
    TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:ApplicableTradeDeliveryTerms/ram:DeliveryTypeCode'));

  //Get all referenced and embedded documents (BG-24)
  nodes := doc.SelectNodes('.//ram:ApplicableHeaderTradeAgreement/ram:AdditionalReferencedDocument');
  for i := 0 to nodes.length-1 do
    Result.AdditionalReferencedDocuments.Add(_getAdditionalReferencedDocument(nodes[i]));

  //Read TransportModeCodes --> BT-X-152
  if doc.SelectSingleNode('//ram:ApplicableHeaderTradeDelivery/ram:RelatedSupplyChainConsignment/ram:SpecifiedLogisticsTransportMovement/ram:ModeCode') <> nil then
    Result.TransportMode := TEnumExtensions<TZUGFeRDTransportmodeCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:RelatedSupplyChainConsignment/ram:SpecifiedLogisticsTransportMovement/ram:ModeCode'));

  Result.ShipTo := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:ShipToTradeParty');
  Result.ShipToContact := _nodeAsContact(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:ShipToTradeParty/ram:DefinedTradeContact');
  Result.UltimateShipTo := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:UltimateShipToTradeParty');
  Result.UltimateShipToContact := _nodeAsContact(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:UltimateShipToTradeParty/ram:DefinedTradeContact');
  Result.ShipFrom := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:ShipFromTradeParty');
  Result.ActualDeliveryDate:= TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:ActualDeliverySupplyChainEvent/ram:OccurrenceDateTime/udt:DateTimeString');

  // BT-X-66-00
  nodes := doc.selectNodes('//ram:ApplicableHeaderTradeDelivery/ram:ShipToTradeParty/ram:SpecifiedTaxRegistration');
  for i := 0 to nodes.length-1 do
  begin
    var id : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ID');
    var schemeID : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ID/@schemeID');
    Result.AddShipToTaxRegistration(id, TEnumExtensions<TZUGFeRDTaxRegistrationSchemeID>.StringToEnum(schemeID));
  end;

  var _despatchAdviceNo : String := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:DespatchAdviceReferencedDocument/ram:IssuerAssignedID');
  var _despatchAdviceDate : ZUGFeRDNullable<TDateTime> := TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:DespatchAdviceReferencedDocument/ram:FormattedIssueDateTime/udt:DateTimeString');

  if Not(_despatchAdviceDate.HasValue) then
    _despatchAdviceDate := TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:DespatchAdviceReferencedDocument/ram:FormattedIssueDateTime');

  if _despatchAdviceDate.HasValue or (_despatchAdviceNo <> '') then
  begin
    Result.DespatchAdviceReferencedDocument := TZUGFeRDDespatchAdviceReferencedDocument.Create;
    Result.DespatchAdviceReferencedDocument.ID := _despatchAdviceNo;
    Result.DespatchAdviceReferencedDocument.IssueDateTime := _despatchAdviceDate
  end;

  var _deliveryNoteNo : String := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssuerAssignedID');
  var _deliveryNoteDate : ZUGFeRDNullable<TDateTime> := TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssueDateTime/udt:DateTimeString');

  if Not(_deliveryNoteDate.HasValue) then
    _deliveryNoteDate := TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:FormattedIssueDateTime');

  if _deliveryNoteDate.HasValue or (_deliveryNoteNo <> '') then
  begin
    Result.DeliveryNoteReferencedDocument := TZUGFeRDDeliveryNoteReferencedDocument.Create;
    Result.DeliveryNoteReferencedDocument.ID := _deliveryNoteNo;
    Result.DeliveryNoteReferencedDocument.IssueDateTime := _deliveryNoteDate
  end;

  Result.Invoicee := _nodeAsParty(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:InvoiceeTradeParty');

  //BT-X-242-00
  nodes := doc.selectNodes('//ram:ApplicableHeaderTradeSettlement/ram:InvoiceeTradeParty/ram:SpecifiedTaxRegistration');
  for i := 0 to nodes.length-1 do
  begin
    var id : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ID');
    var schemeID : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ID/@schemeID');
    Result.AddInvoiceeTaxRegistration(id, TEnumExtensions<TZUGFeRDTaxRegistrationSchemeID>.StringToEnum(schemeID));
  end;

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

  Result.TaxCurrency := TEnumExtensions<TZUGFeRDCurrencyCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:TaxCurrencyCode')); // BT-6

  // TODO: Multiple SpecifiedTradeSettlementPaymentMeans can exist for each account/institution (with different SEPA?)
  Result.PaymentMeans := TZUGFeRDPaymentMeans.Create;
  Result.PaymentMeans.TypeCode := TEnumExtensions<TZUGFeRDPaymentMeansTypeCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:TypeCode'));
  Result.PaymentMeans.Information := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:Information');
  Result.PaymentMeans.SEPACreditorIdentifier := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:CreditorReferenceID');
  Result.PaymentMeans.SEPAMandateReference := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement,' //ram:SpecifiedTradePaymentTerms/ram:DirectDebitMandateID'); // comes from SpecifiedTradePaymentTerms!!!

  var financialCardId : String := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:ApplicableTradeSettlementFinancialCard/ram:ID');
  var financialCardCardholderName : String := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:ApplicableTradeSettlementFinancialCard/ram:CardholderName');

  if ((financialCardId <> '') or (financialCardCardholderName <> '')) then
  begin
    Result.PaymentMeans.FinancialCard := TZUGFeRDFinancialCard.Create;
    Result.PaymentMeans.FinancialCard.Id := financialCardId;
    Result.PaymentMeans.FinancialCard.CardholderName := financialCardCardholderName;
  end;

  Result.BillingPeriodStart := TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:BillingSpecifiedPeriod/ram:StartDateTime');
  Result.BillingPeriodEnd := TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeSettlement/ram:BillingSpecifiedPeriod/ram:EndDateTime');

  var creditorFinancialAccountNodes : IXMLDOMNodeList := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:PayeePartyCreditorFinancialAccount');
  var creditorFinancialInstitutions : IXMLDOMNodeList := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans/ram:PayeeSpecifiedCreditorFinancialInstitution');

  var numberOfAccounts : Integer := Max(creditorFinancialAccountNodes.Length,creditorFinancialInstitutions.Length);
  for i := 0 to numberOfAccounts-1 do
    Result.CreditorBankAccounts.Add(TZUGFeRDBankAccount.Create);

  for i := 0 to creditorFinancialAccountNodes.Length-1 do
  begin
    Result.CreditorBankAccounts[i].ID := TZUGFeRDXmlUtils.NodeAsString(creditorFinancialAccountNodes[i], './/ram:ProprietaryID');
    Result.CreditorBankAccounts[i].IBAN := TZUGFeRDXmlUtils.NodeAsString(creditorFinancialAccountNodes[i], './/ram:IBANID');
    Result.CreditorBankAccounts[i].Name := TZUGFeRDXmlUtils.NodeAsString(creditorFinancialAccountNodes[i], './/ram:AccountName');
  end;

  for i := 0 to creditorFinancialInstitutions.Length-1 do
  begin
    Result.CreditorBankAccounts[i].BIC := TZUGFeRDXmlUtils.NodeAsString(creditorFinancialInstitutions[i], './/ram:BICID');
    Result.CreditorBankAccounts[i].Bankleitzahl := TZUGFeRDXmlUtils.NodeAsString(creditorFinancialInstitutions[i], './/ram:GermanBankleitzahlID');
    Result.CreditorBankAccounts[i].BankName := TZUGFeRDXmlUtils.NodeAsString(creditorFinancialInstitutions[i], './/ram:Name');
  end;

  var specifiedTradeSettlementPaymentMeansNodes : IXMLDOMNodeList := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeSettlementPaymentMeans');
  for i := 0 to specifiedTradeSettlementPaymentMeansNodes.length-1 do
  begin
    var payerPartyDebtorFinancialAccountNode : IXMLDOMNode := specifiedTradeSettlementPaymentMeansNodes[i].selectSingleNode('ram:PayerPartyDebtorFinancialAccount');

    if payerPartyDebtorFinancialAccountNode = nil then
      continue;

    var _account : TZUGFeRDBankAccount := TZUGFeRDBankAccount.Create;
    _account.ID := TZUGFeRDXmlUtils.NodeAsString(payerPartyDebtorFinancialAccountNode, './/ram:ProprietaryID');
    _account.IBAN := TZUGFeRDXmlUtils.NodeAsString(payerPartyDebtorFinancialAccountNode, './/ram:IBANID');
    _account.Bankleitzahl := TZUGFeRDXmlUtils.NodeAsString(payerPartyDebtorFinancialAccountNode, './/ram:GermanBankleitzahlID');
    _account.BankName := TZUGFeRDXmlUtils.NodeAsString(payerPartyDebtorFinancialAccountNode, './/ram:Name');

    Result.DebitorBankAccounts.Add(_account);
  end;

  nodes := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:ApplicableTradeTax');
  for i := 0 to nodes.length-1 do
  begin
    var calculatedAmount: ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:CalculatedAmount', 0);
    var basisAmount: ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:BasisAmount', 0);
    var ratePercent: ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:RateApplicablePercent', 0);
    var taxType: ZUGFeRDNullable<TZUGFeRDTaxTypes> := TEnumExtensions<TZUGFeRDTaxTypes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:TypeCode'));
    var categoryCode: ZUGFeRDNullable<TZUGFeRDTaxCategoryCodes> := TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:CategoryCode'));
    var allowanceChargeBasis: ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:AllowanceChargeBasisAmount', 0);
    var exemptionReasonCode: ZUGFeRDNullable<TZUGFeRDTaxExemptionReasonCodes> := TEnumExtensions<TZUGFeRDTaxExemptionReasonCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ExemptionReasonCode'));
    var exemptionReason: string := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ExemptionReason');
    var lineTotalBasis: ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:LineTotalBasisAmount');
    var taxPointDate: ZUGFeRDNullable<TDateTime> := TZUGFeRDXmlUtils.NodeAsDateTime(nodes[i], './/ram:TaxPointDate/udt:DateString');
    var dueDateTypeCode: ZUGFeRDNullable<TZUGFeRDDateTypeCodes> := TEnumExtensions<TZUGFeRDDateTypeCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:DueDateTypeCode'));

    Result.AddApplicableTradeTax(calculatedAmount, basisAmount, ratePercent, taxType, categoryCode,
                                 allowanceChargeBasis, exemptionReasonCode, exemptionReason, lineTotalBasis)
          .SetTaxPointDate(taxPointDate, dueDateTypeCode);
  end;
  nodes := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:SpecifiedTradeAllowanceCharge');
  for i := 0 to nodes.length-1 do
  begin
    var chargePercentage: ZUGFeRDNullable<Currency> :=TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:CalculationPercent');
    var basisAmount: ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:BasisAmount', 0);
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
    var appliedAmount: ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:AppliedAmount', 0);
    var description: string := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Description');
    var logTaxType: ZUGFeRDNullable<TZUGFeRDTaxTypes> := TEnumExtensions<TZUGFeRDTaxTypes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:AppliedTradeTax/ram:TypeCode'));
    var logCategoryCode: ZUGFeRDNullable<TZUGFeRDTaxCategoryCodes> := TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:AppliedTradeTax/ram:CategoryCode'));
    var logTaxPercent: ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:AppliedTradeTax/ram:RateApplicablePercent', 0);
    Result.AddLogisticsServiceCharge(appliedAmount, description, logTaxType, logCategoryCode, logTaxPercent);
  end;

  nodes := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:InvoiceReferencedDocument');
  for i := 0 to nodes.length-1 do
  begin
    var issuerAssignedID: string := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:IssuerAssignedID');
    var issueDateTime: ZUGFeRDNullable<TDateTime> := TZUGFeRDXmlUtils.NodeAsDateTime(nodes[i], './ram:FormattedIssueDateTime');
    var invoiceTypeCode: ZUGFeRDNullable<TZUGFeRDInvoiceType> := TEnumExtensions<TZUGFeRDInvoiceType>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:TypeCode'));

    Result.AddInvoiceReferencedDocument(issuerAssignedID, issueDateTime, invoiceTypeCode);
  end;

  nodes := doc.SelectNodes('//ram:SpecifiedTradePaymentTerms');
  if Not (TryReadXRechnungPaymentTerms(Result, nodes)) then
    for i := 0 to nodes.length-1 do
    begin
      var paymentTerm : TZUGFeRDPaymentTerms := TZUGFeRDPaymentTerms.Create;
      paymentTerm.Description := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Description');
      paymentTerm.DueDate:= TZUGFeRDXmlUtils.NodeAsDateTime(nodes[i], './/ram:DueDateDateTime/udt:DateTimeString');
      paymentTerm.PartialPaymentAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:PartialPaymentAmount');
      var discountPercent: ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:ApplicableTradePaymentDiscountTerms/ram:CalculationPercent');
      var penaltyPercent: ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './/ram:ApplicableTradePaymentPenaltyTerms/ram:CalculationPercent');
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

  nodes := doc.SelectNodes('//ram:ApplicableHeaderTradeSettlement/ram:ReceivableSpecifiedTradeAccountingAccount');
  for i := 0 to nodes.length-1 do
    Result.AddReceivableSpecifiedTradeAccountingAccount(
      TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ID'),
      TEnumExtensions<TZUGFeRDAccountingAccountTypeCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:TypeCode')));

  Result.OrderDate:= TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:BuyerOrderReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString');
  Result.OrderNo := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:BuyerOrderReferencedDocument/ram:IssuerAssignedID');

  // Read SellerOrderReferencedDocument
  if doc.SelectSingleNode('//ram:ApplicableHeaderTradeAgreement/ram:SellerOrderReferencedDocument') <> nil then
  begin
    Result.SellerOrderReferencedDocument := TZUGFeRDSellerOrderReferencedDocument.Create;
    Result.SellerOrderReferencedDocument.ID := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:SellerOrderReferencedDocument/ram:IssuerAssignedID');
    Result.SellerOrderReferencedDocument.IssueDateTime:= TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:SellerOrderReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString');
  end;

  // Read ContractReferencedDocument
  if doc.SelectSingleNode('//ram:ApplicableHeaderTradeAgreement/ram:ContractReferencedDocument') <> nil then
  begin
    Result.ContractReferencedDocument := TZUGFeRDContractReferencedDocument.Create;
    Result.ContractReferencedDocument.ID := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:ContractReferencedDocument/ram:IssuerAssignedID');
    Result.ContractReferencedDocument.IssueDateTime:= TZUGFeRDXmlUtils.NodeAsDateTime(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:ContractReferencedDocument/ram:FormattedIssueDateTime');
  end;

  if doc.SelectSingleNode('//ram:ApplicableHeaderTradeAgreement/ram:SpecifiedProcuringProject') <> nil then
  begin
    Result.SpecifiedProcuringProject := TZUGFeRDSpecifiedProcuringProject.Create;
    Result.SpecifiedProcuringProject.ID := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:SpecifiedProcuringProject/ram:ID');
    Result.SpecifiedProcuringProject.Name := TZUGFeRDXmlUtils.NodeAsString(doc.DocumentElement, '//ram:ApplicableHeaderTradeAgreement/ram:SpecifiedProcuringProject/ram:Name');
  end;

  nodes := doc.SelectNodes('//ram:IncludedSupplyChainTradeLineItem');
  for i := 0 to nodes.length-1 do
    Result.TradeLineItems.Add(_parseTradeLineItem(nodes[i]));
end;

function TZUGFeRDInvoiceDescriptor23CIIReader._parseTradeLineItem(tradeLineItem: IXmlDomNode): TZUGFeRDTradeLineItem;
var
  nodes : IXMLDOMNodeList;
  i : Integer;
begin
  Result := nil;

  if tradeLineItem = nil then
    exit;

  var lineId: string := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:AssociatedDocumentLineDocument/ram:LineID');
  var parentLineId: string := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:AssociatedDocumentLineDocument/ram:ParentLineID');
  var lineStatusCode: ZUGFeRDNullable<TZUGFeRDLineStatusCodes> := TEnumExtensions<TZUGFeRDLineStatusCodes>.StringToNullableEnum(
    TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:AssociatedDocumentLineDocument/ram:LineStatusCode'));
  var lineStatusReasonCode: ZUGFeRDNullable<TZUGFeRDLineStatusReasonCodes> := TEnumExtensions<TZUGFeRDLineStatusReasonCodes>.StringToNullableEnum(
    TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:AssociatedDocumentLineDocument/ram:LineStatusReasonCode'));

  Result := TZUGFeRDTradeLineItem.Create(lineId);

  Result.GlobalID.SchemeID := TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:GlobalID/@schemeID'));
  Result.GlobalID.ID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:GlobalID');
  Result.SellerAssignedID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:SellerAssignedID');
  Result.BuyerAssignedID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:BuyerAssignedID');
  Result.Name := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:Name');
  Result.Description := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedTradeProduct/ram:Description');
  Result.BilledQuantity := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:BilledQuantity', 0);
  Result.ShipTo := _nodeAsParty(tradeLineItem, './/ram:SpecifiedLineTradeDelivery/ram:ShipToTradeParty');
  Result.ShipToContact := _nodeAsContact(tradeLineItem, '//ram:SpecifiedLineTradeDelivery/ram:ShipToTradeParty/ram:DefinedTradeContact');
  Result.UltimateShipTo := _nodeAsParty(tradeLineItem, './/ram:SpecifiedLineTradeDelivery/ram:UltimateShipToTradeParty');
  Result.UltimateShipToContact := _nodeAsContact(tradeLineItem, '//ram:SpecifiedLineTradeDelivery/ram:UltimateShipToTradeParty/ram:DefinedTradeContact');
  Result.ChargeFreeQuantity := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:ChargeFreeQuantity');
  Result.PackageQuantity := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:PackageQuantity');
  Result.LineTotalAmount:= TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:LineTotalAmount');
  Result.TaxCategoryCode := TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:ApplicableTradeTax/ram:CategoryCode'));
  Result.TaxType := TEnumExtensions<TZUGFeRDTaxTypes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:ApplicableTradeTax/ram:TypeCode'));
  Result.TaxPercent := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:ApplicableTradeTax/ram:RateApplicablePercent', 0);
  Result.TaxExemptionReasonCode := TEnumExtensions<TZUGFeRDTaxExemptionReasonCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:ApplicableTradeTax/ram:ExemptionReasonCode'));
  Result.TaxExemptionReason := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:ApplicableTradeTax/ram:ExemptionReason');
  Result.NetUnitPrice:= TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:NetPriceProductTradePrice/ram:ChargeAmount');
  Result.NetQuantity := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:NetPriceProductTradePrice/ram:BasisQuantity');
  Result.NetUnitCode := TEnumExtensions<TZUGFeRDQuantityCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:NetPriceProductTradePrice/ram:BasisQuantity/@unitCode'));
  Result.GrossUnitPrice:= TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:GrossPriceProductTradePrice/ram:ChargeAmount');
  Result.GrossQuantity := TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:GrossPriceProductTradePrice/ram:BasisQuantity');
  Result.UnitCode := TEnumExtensions<TZUGFeRDQuantityCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:BilledQuantity/@unitCode'));
  Result.ChargeFreeUnitCode := TEnumExtensions<TZUGFeRDQuantityCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:ChargeFreeQuantity/@unitCode'));
  Result.PackageUnitCode := TEnumExtensions<TZUGFeRDQuantityCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:PackageQuantity/@unitCode'));
  Result.GrossUnitCode := TEnumExtensions<TZUGFeRDQuantityCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:GrossPriceProductTradePrice/ram:BasisQuantity/@unitCode'));
  Result.BillingPeriodStart := TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:BillingSpecifiedPeriod/ram:StartDateTime/udt:DateTimeString');
  Result.BillingPeriodEnd := TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:BillingSpecifiedPeriod/ram:EndDateTime/udt:DateTimeString');

  if parentLineId<>'' then
    Result.SetParentLineId(parentLineId);

  if lineStatusCode.HasValue and LineStatusReasonCode.HasValue then
    Result.SetLineStatus(lineStatusCode.Value, LineStatusReasonCode.Value);

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
    var irpItem: TZUGFeRDIncludedReferencedProduct := TZUGFeRDIncludedReferencedProduct.Create;
    irpItem.Name := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Name');
    irpItem.UnitCode := TEnumExtensions<TZUGFeRDQuantityCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:UnitQuantity/@unitCode'));
    irpItem.UnitQuantity:= TZUGFeRDXmlUtils.NodeAsDecimal(tradeLineItem, './/ram:UnitQuantity', 0);
    Result.IncludedReferencedProducts.Add(irpItem);
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeAgreement/ram:BuyerOrderReferencedDocument') <> nil) then
  begin
    Result.BuyerOrderReferencedDocument:= TZUGFeRDBuyerOrderReferencedDocument.Create;
    Result.BuyerOrderReferencedDocument.ID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:BuyerOrderReferencedDocument/ram:IssuerAssignedID');
    Result.BuyerOrderReferencedDocument.IssueDateTime := TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:BuyerOrderReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString');
    Result.BuyerOrderReferencedDocument.LineID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:BuyerOrderReferencedDocument/ram:LineID');
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeAgreement/ram:ContractReferencedDocument') <> nil) then
  begin
    Result.ContractReferencedDocument := TZUGFeRDContractReferencedDocument.Create;
    Result.ContractReferencedDocument.ID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:ContractReferencedDocument/ram:IssuerAssignedID');
    Result.ContractReferencedDocument.IssueDateTime:= TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:ContractReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString');
    Result.ContractReferencedDocument.LineID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedLineTradeAgreement/ram:ContractReferencedDocument/ram:LineID');
  end;

  // read SpecifiedLineTradeSettlement
  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeSettlement') <> nil) then
  begin
    var applicableTradeTaxNode: IXMLDOMNode:= tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeSettlement/ram:ApplicableTradeTax');
    // TODO: process

    var billingSpecifiedPeriodNode: IXMLDOMNode := tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeSettlement/ram:BillingSpecifiedPeriod');
    // TODO: process

    nodes := tradeLineItem.SelectNodes('.//ram:SpecifiedLineTradeSettlement/ram:SpecifiedTradeAllowanceCharge');
    for i := 0 to nodes.length-1 do
    begin
      var chargeIndicator : Boolean := TZUGFeRDXmlUtils.NodeAsBool(nodes[i], './ram:ChargeIndicator/udt:Indicator');
      var basisAmount : ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './ram:BasisAmount');
      var basisAmountCurrency : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:BasisAmount/@currencyID');
      var actualAmount : Currency := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './ram:ActualAmount',0);
      var actualAmountCurrency : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:ActualAmount/@currencyID');
      var reason : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:Reason');
      var chargePercentage: ZUGFeRDNullable<Currency> :=TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './ram:CalculationPercent');
      var reasonCode: String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:ReasonCode');

        if chargeIndicator then
          Result.AddSpecifiedTradeCharge(TEnumExtensions<TZUGFeRDCurrencyCodes>.StringToEnum(basisAmountCurrency),
                                                             basisAmount,
                                                             actualAmount,
                                                             chargePercentage,
                                                             reason,
                                                             TEnumExtensions<TZUGFeRDChargeReasonCodes>.StringToNullableEnum(reasonCode))
        else // allowance
          Result.AddSpecifiedTradeAllowance(TEnumExtensions<TZUGFeRDCurrencyCodes>.StringToEnum(basisAmountCurrency),
                                                                basisAmount,
                                                                actualAmount,
                                                                chargePercentage,
                                                                reason,
                                                                TEnumExtensions<TZUGFeRDAllowanceReasonCodes>.StringToNullableEnum(reasonCode));
    end;

    var specifiedTradeSettlementLineMonetarySummationNode: IXMLDOMNode:= tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeSettlement/ram:SpecifiedTradeSettlementLineMonetarySummation');
    // TODO: process

    nodes := tradeLineItem.SelectNodes('.//ram:SpecifiedLineTradeSettlement/ram:InvoiceReferencedDocument');
    for i := 0 to nodes.length-1 do
    begin
      // TODO: process
    end;
  end;

  nodes := tradeLineItem.SelectNodes('.//ram:SpecifiedLineTradeSettlement/ram:AdditionalReferencedDocument');
  for i := 0 to nodes.length-1 do
    Result.AdditionalReferencedDocuments.Add(_getAdditionalReferencedDocument(nodes[i]));

  nodes := tradeLineItem.SelectNodes('.//ram:SpecifiedLineTradeSettlement/ram:ReceivableSpecifiedTradeAccountingAccount');
  for i := 0 to nodes.length-1 do
  begin
    var rstaaItem : TZUGFeRDReceivableSpecifiedTradeAccountingAccount := TZUGFeRDReceivableSpecifiedTradeAccountingAccount.Create;
    rstaaItem.TradeAccountID := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ID');
    rstaaItem.TradeAccountTypeCode := TEnumExtensions<TZUGFeRDAccountingAccountTypeCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:TypeCode'));
    Result.ReceivableSpecifiedTradeAccountingAccounts.Add(rstaaItem);
    break; // an if above would have done it also <g>
  end;

  if (tradeLineItem.SelectSingleNode('.//ram:AssociatedDocumentLineDocument') <> nil) then
  begin
    nodes := tradeLineItem.SelectNodes('.//ram:AssociatedDocumentLineDocument/ram:IncludedNote');
    for i := 0 to nodes.length-1 do
      Result.AssociatedDocument.Notes.Add(
        TZUGFeRDNote.Create(
            TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:Content'),
            TEnumExtensions<TZUGFeRDSubjectCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:SubjectCode')),
            TEnumExtensions<TZUGFeRDContentCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ContentCode'))
      ));
  end;

  nodes := tradeLineItem.SelectNodes('.//ram:SpecifiedLineTradeAgreement/ram:GrossPriceProductTradePrice/ram:AppliedTradeAllowanceCharge');
  for i := 0 to nodes.length-1 do
  begin

    var chargeIndicator : Boolean := TZUGFeRDXmlUtils.NodeAsBool(nodes[i], './ram:ChargeIndicator/udt:Indicator');
    var basisAmount : ZUGFeRDNullable<Currency> := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './ram:BasisAmount',0);
    var basisAmountCurrency : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:BasisAmount/@currencyID');
    var actualAmount : Currency := TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './ram:ActualAmount',0);
    var actualAmountCurrency : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:ActualAmount/@currencyID');
    var reason : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './ram:Reason');
    var chargePercentage: ZUGFeRDNullable<Currency> :=TZUGFeRDXmlUtils.NodeAsDecimal(nodes[i], './ram:CalculationPercent');

    if chargeIndicator then // charge
      Result.AddTradeCharge(TEnumExtensions<TZUGFeRDCurrencyCodes>.StringToEnum(basisAmountCurrency),
                                    basisAmount,
                                    actualAmount,
                                    chargePercentage,
                                    reason)
    else // allowance
      Result.AddTradeAllowance(TEnumExtensions<TZUGFeRDCurrencyCodes>.StringToEnum(basisAmountCurrency),
                                    basisAmount,
                                    actualAmount,
                                    chargePercentage,
                                    reason);
  end;

  if Result.UnitCode.HasValue then
    // UnitCode alternativ aus BilledQuantity extrahieren
    Result.UnitCode := TEnumExtensions<TZUGFeRDQuantityCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:BilledQuantity/@unitCode'));

  if (tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssuerAssignedID') <> nil) then
  begin
    Result.DeliveryNoteReferencedDocument := TZUGFeRDDeliveryNoteReferencedDocument.Create;
    Result.DeliveryNoteReferencedDocument.ID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedLineTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:IssuerAssignedID');
    Result.DeliveryNoteReferencedDocument.IssueDateTime:= TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:SpecifiedLineTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:FormattedIssueDateTime/qdt:DateTimeString');
    Result.DeliveryNoteReferencedDocument.LineID := TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:SpecifiedLineTradeDelivery/ram:DeliveryNoteReferencedDocument/ram:LineID');
  end;

  if tradeLineItem.SelectSingleNode('.//ram:SpecifiedLineTradeDelivery/ram:ActualDeliverySupplyChainEvent/ram:OccurrenceDateTime') <> nil then
    Result.ActualDeliveryDate:= TZUGFeRDXmlUtils.NodeAsDateTime(tradeLineItem, './/ram:SpecifiedLineTradeDelivery/ram:ActualDeliverySupplyChainEvent/ram:OccurrenceDateTime/udt:DateTimeString');

  nodes := tradeLineItem.SelectNodes('.//ram:DesignatedProductClassification');
  for i := 0 to nodes.length-1 do
  begin
    var className : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ClassName');
    var classCode : string := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ClassCode');
    var listID : TZUGFeRDDesignatedProductClassificationClassCodes := TEnumExtensions<TZUGFeRDDesignatedProductClassificationClassCodes>.StringToEnum(TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ClassCode/@listID'));
    var listVersionID : String := TZUGFeRDXmlUtils.NodeAsString(nodes[i], './/ram:ClassCode/@listVersionID');
    Result.AddDesignatedProductClassification(listID, listVersionID, className, classCode);
  end;

  if tradeLineItem.SelectSingleNode('.//ram:OriginTradeCountry//ram:ID') <> nil then
    Result.OriginTradeCountry := TEnumExtensions<TZUGFeRDCountryCodes>.StringToEnum(TZUGFeRDXmlUtils.NodeAsString(tradeLineItem, './/ram:OriginTradeCountry//ram:ID'));

end;

function TZUGFeRDInvoiceDescriptor23CIIReader._nodeAsLegalOrganization(
  basenode: IXmlDomNode; const xpath: string) : TZUGFeRDLegalOrganization;
var
  node : IXmlDomNode;
begin
  Result := nil;
  if (baseNode = nil) then
    exit;
  node := baseNode.SelectSingleNode(xpath);
  if (node = nil) then
    exit;
  Result := TZUGFeRDLegalOrganization.CreateWithParams(
               TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(node, 'ram:ID/@schemeID')),
               TZUGFeRDXmlUtils.NodeAsString(node, 'ram:ID'),
               TZUGFeRDXmlUtils.NodeAsString(node, 'ram:TradingBusinessName'));
end;

function TZUGFeRDInvoiceDescriptor23CIIReader._nodeAsContact(basenode: IXmlDomNode;  const xpath: string) : TZUGFeRDContact;
var
  node : IXmlDomNode;
begin
  Result := nil;
  if (baseNode = nil) then
    exit;
  node := baseNode.SelectSingleNode(xpath);
  if (node = nil) then
    exit;
  Result := TZUGFeRDContact.Create;
  Result.Name := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:PersonName');
  Result.OrgUnit := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:DepartmentName');
  Result.PhoneNo := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:TelephoneUniversalCommunication/ram:CompleteNumber');
  Result.FaxNo := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:FaxUniversalCommunication/ram:CompleteNumber');
  Result.EmailAddress := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:EmailURIUniversalCommunication/ram:URIID');
end;

function TZUGFeRDInvoiceDescriptor23CIIReader._nodeAsParty(basenode: IXmlDomNode;  const xpath: string) : TZUGFeRDParty;
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
  Result.ID.SchemeID := TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(node, 'ram:ID/@schemeID'));
  Result.ID.ID := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:ID');
  Result.GlobalID.SchemeID := TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(node, 'ram:GlobalID/@schemeID'));
  Result.GlobalID.ID := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:GlobalID');
  Result.Name := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:Name');
  Result.Description := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:Description'); // Seller only BT-33
  Result.Postcode := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:PostalTradeAddress/ram:PostcodeCode');
  Result.City := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:PostalTradeAddress/ram:CityName');
  Result.Country := TEnumExtensions<TZUGFeRDCountryCodes>.StringToNullableEnum(TZUGFeRDXmlUtils.NodeAsString(node, 'ram:PostalTradeAddress/ram:CountryID'));
  Result.SpecifiedLegalOrganization := _nodeAsLegalOrganization(node, 'ram:SpecifiedLegalOrganization');

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
  Result.AddressLine3 := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:PostalTradeAddress/ram:LineThree');
  Result.CountrySubdivisionName := TZUGFeRDXmlUtils.NodeAsString(node, 'ram:PostalTradeAddress/ram:CountrySubDivisionName');
end;

function TZUGFeRDInvoiceDescriptor23CIIReader._getAdditionalReferencedDocument(a_oXmlNode: IXmlDomNode): TZUGFeRDAdditionalReferencedDocument;
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
  Result.URIID := TZUGFeRDXmlUtils.NodeAsString(a_oXmlNode, 'ram:URIID');
  Result.LineID := TZUGFeRDXmlUtils.NodeAsString(a_oXmlNode, 'ram:LineID');
end;

end.
