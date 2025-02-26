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

unit intf.ZUGFeRDInvoiceDescriptor23CIIWriter;

interface

uses
  System.SysUtils,System.Classes,System.StrUtils,Generics.Collections,
  System.Math
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
  ,intf.ZUGFeRDDateTypeCodes
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
  ,intf.ZUGFeRDDesignatedProductClassification
  ,intf.ZUGFeRDDesignatedProductClassificationClassCodes
  ,intf.ZUGFeRDIncludedReferencedProduct
  ,intf.ZUGFeRDInvoiceReferencedDocument
  ;

type
  TZUGFeRDInvoiceDescriptor23CIIWriter = class(TZUGFeRDInvoiceDescriptorWriter)
  private
    Writer: TZUGFeRDProfileAwareXmlTextWriter;
    Descriptor: TZUGFeRDInvoiceDescriptor;
    procedure _writeOptionalAmount(_writer : TZUGFeRDProfileAwareXmlTextWriter; tagName : string; value : ZUGFeRDNullable<Currency>; numDecimals : Integer = 2; forceCurrency : Boolean = false; profile : TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
    procedure _writeAmount(  _writer: TZUGFeRDProfileAwareXmlTextWriter; tagName: string; value: ZUGFeRDNullable<Currency>; defaultValue: Currency = 0; numDecimals: Integer = 2; forceCurrency: Boolean = false; profile : TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
    procedure _writeOptionalAdaptiveAmount(_writer: TZUGFeRDProfileAwareXmlTextWriter; _tagName: string; _value: ZUGFeRDNullable<Currency>; _numDecimals: Integer = 2;_maxnumDecimals: Integer = 4; _forceCurrency: boolean = false);
    procedure _writeNotes(_writer : TZUGFeRDProfileAwareXmlTextWriter;notes : TObjectList<TZUGFeRDNote>);
    procedure _writeOptionalLegalOrganization(_writer : TZUGFeRDProfileAwareXmlTextWriter; legalOrganizationTag : String;legalOrganization : TZUGFeRDLegalOrganization; partyType : TZUGFeRDPartyTypes = TZUGFeRDPartyTypes.Unknown);
    procedure _writeOptionalParty(_writer: TZUGFeRDProfileAwareXmlTextWriter; partyType : TZUGFeRDPartyTypes; party : TZUGFeRDParty; contact : TZUGFeRDContact = nil; electronicAddress : TZUGFeRDElectronicAddress = nil; taxRegistrations : TObjectList<TZUGFeRDTaxRegistration> = nil);
    procedure _writeOptionalContact(_writer: TZUGFeRDProfileAwareXmlTextWriter;contactTag: String; contact: TZUGFeRDContact;profile: TZUGFeRDProfiles);
    procedure _writeOptionalTaxes(_writer: TZUGFeRDProfileAwareXmlTextWriter);
    procedure _writeElementWithAttribute(_writer: TZUGFeRDProfileAwareXmlTextWriter; tagName, attributeName,attributeValue, nodeValue: String; profile: TZUGFeRDProfiles = [TZUGFeRDProfile.Unknown]);
    procedure _writeAdditionalReferencedDocument(_writer: TZUGFeRDProfileAwareXmlTextWriter; document :TZUGFeRDAdditionalReferencedDocument; profile: TZUGFeRDProfiles = [TZUGFeRDProfile.Unknown]; parentElement: string = '');
    function _translateTaxCategoryCode(taxCategoryCode : TZUGFeRDTaxCategoryCodes) : String;
    function _translateInvoiceType(type_ : TZUGFeRDInvoiceType) : String;
    function _encodeInvoiceType(type_ : TZUGFeRDInvoiceType) : Integer;
  private const
    ALL_PROFILES = [TZUGFeRDProfile.Minimum,
                    TZUGFeRDProfile.BasicWL,
                    TZUGFeRDProfile.Basic,
                    TZUGFeRDProfile.Comfort,
                    TZUGFeRDProfile.Extended,
                    TZUGFeRDProfile.XRechnung1,
                    TZUGFeRDProfile.XRechnung,
                    TZUGFeRDProfile.EReporting];
    PROFILE_COMFORT_EXTENDED_XRECHNUNG =
                   [TZUGFeRDProfile.Comfort,
                    TZUGFeRDProfile.Extended,
                    TZUGFeRDProfile.XRechnung1,
                    TZUGFeRDProfile.XRechnung];
  public
    /// <summary>
    /// This function is implemented in class InvoiceDescriptor22Writer.
    /// </summary>
    function Validate(_descriptor: TZUGFeRDInvoiceDescriptor; _throwExceptions: Boolean = True): Boolean; override;
    /// <summary>
    /// Saves the given invoice to the given stream.
    /// Make sure that the stream is open and writeable. Otherwise, an IllegalStreamException will be thron.
    /// </summary>
    /// <param name="descriptor">The invoice object that should be saved</param>
    /// <param name="stream">The target stream for saving the invoice</param>
    /// <param name="format">Format of the target file</param>
    procedure Save(_descriptor: TZUGFeRDInvoiceDescriptor; _stream: TStream; _format : TZUGFeRDFormats = TZUGFeRDFormats.CII); override;
  end;

implementation

{ TZUGFeRDInvoiceDescriptor23CIIWriter }

procedure TZUGFeRDInvoiceDescriptor23CIIWriter.Save(
  _descriptor: TZUGFeRDInvoiceDescriptor; _stream: TStream;
  _format : TZUGFeRDFormats = TZUGFeRDFormats.CII);
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

  //#region Kopfbereich
  Writer.WriteStartElement('rsm:CrossIndustryInvoice');
  Writer.WriteAttributeString('xmlns', 'a', '', 'urn:un:unece:uncefact:data:standard:QualifiedDataType:100');
  Writer.WriteAttributeString('xmlns', 'rsm', '', 'urn:un:unece:uncefact:data:standard:CrossIndustryInvoice:100');
  Writer.WriteAttributeString('xmlns', 'qdt', '', 'urn:un:unece:uncefact:data:standard:QualifiedDataType:100');
  Writer.WriteAttributeString('xmlns', 'ram', '', 'urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:100');
  Writer.WriteAttributeString('xmlns', 'xs', '', 'http://www.w3.org/2001/XMLSchema');
  Writer.WriteAttributeString('xmlns', 'udt', '', 'urn:un:unece:uncefact:data:standard:UnqualifiedDataType:100');
  //#endregion

  //#region ExchangedDocumentContext
  //Prozesssteuerung
  Writer.WriteStartElement('rsm:ExchangedDocumentContext');
  if (Descriptor.IsTest) then
  begin
    Writer.WriteStartElement('ram:TestIndicator', [TZUGFeRDProfile.Extended]);
    Writer.WriteElementString('udt:Indicator', ifthen(Descriptor.IsTest,'true','false'));
    Writer.WriteEndElement(); // !ram:TestIndicator
  end;

  if (Descriptor.BusinessProcess <> '') then
  begin
    Writer.WriteStartElement('ram:BusinessProcessSpecifiedDocumentContextParameter');
    Writer.WriteElementString('ram:ID', Descriptor.BusinessProcess);
    Writer.WriteEndElement(); // !ram:BusinessProcessSpecifiedDocumentContextParameter
  end;

  Writer.WriteStartElement('ram:GuidelineSpecifiedDocumentContextParameter');
  //Gruppierung der Anwendungsempfehlungsinformationen
  Writer.WriteElementString('ram:ID', TZUGFeRDProfileExtensions.EnumToString(Descriptor.Profile,TZUGFeRDVersion.Version23));
  Writer.WriteEndElement(); // !ram:GuidelineSpecifiedDocumentContextParameter
  Writer.WriteEndElement(); // !rsm:ExchangedDocumentContext
  //#endregion

  //Gruppierung der Eigenschaften, die das gesamte Dokument betreffen.
  Writer.WriteStartElement('rsm:ExchangedDocument');
  Writer.WriteElementString('ram:ID', Descriptor.InvoiceNo); //Rechnungsnummer
  Writer.WriteElementString('ram:Name', ifthen(Descriptor.Name<>'',Descriptor.Name,_translateInvoiceType(Descriptor.Type_)), [TZUGFeRDProfile.Extended]); //Dokumentenart (Freitext)
  Writer.WriteElementString('ram:TypeCode', Format('%d',[_encodeInvoiceType(Descriptor.Type_)])); //Code für den Rechnungstyp
                                                                                                             //ToDo: LanguageID      //Sprachkennzeichen
  if Descriptor.InvoiceDate.HasValue then
  begin
      Writer.WriteStartElement('ram:IssueDateTime');
      Writer.WriteStartElement('udt:DateTimeString');
      Writer.WriteAttributeString('format', '102');
      Writer.WriteValue(_formatDate(Descriptor.InvoiceDate));
      Writer.WriteEndElement(); // !udt:DateTimeString
      Writer.WriteEndElement(); // !IssueDateTime
  end;
  if Descriptor.Profile<>TZUGFeRDProfile.Minimum then
    _writeNotes(Writer, Descriptor.Notes);
  Writer.WriteEndElement(); // !rsm:ExchangedDocument

  //#region SpecifiedSupplyChainTradeTransaction
  //Gruppierung der Informationen zum Geschäftsvorfall
  Writer.WriteStartElement('rsm:SupplyChainTradeTransaction');

  //#region  IncludedSupplyChainTradeLineItem
  for var tradeLineItem : TZUGFeRDTradeLineItem in Descriptor.TradeLineItems do
  begin
    Writer.WriteStartElement('ram:IncludedSupplyChainTradeLineItem');

    //#region AssociatedDocumentLineDocument
    //Gruppierung von allgemeinen Positionsangaben
    if (tradeLineItem.AssociatedDocument <> nil) then
    begin
      Writer.WriteStartElement('ram:AssociatedDocumentLineDocument', [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);
      Writer.WriteOptionalElementString('ram:LineID', tradeLineItem.AssociatedDocument.LineID);
      Writer.WriteOptionalElementString('ram:ParentLineID', tradeLineItem.AssociatedDocument.ParentLineID);
      Writer.WriteOptionalElementString('ram:LineStatusCode', tradeLineItem.AssociatedDocument.LineStatusCode);
      Writer.WriteOptionalElementString('ram:LineStatusReasonCode', tradeLineItem.AssociatedDocument.LineStatusReasonCode);
      if (Descriptor.Profile<>TZUGFeRDProfile.Minimum) and (Descriptor.Profile<>TZUGFeRDProfile.BasicWL) then
       _writeNotes(Writer, tradeLineItem.AssociatedDocument.Notes);
      Writer.WriteEndElement(); // ram:AssociatedDocumentLineDocument(Basic|Comfort|Extended|XRechnung)
    end;
    //#endregion

    // TODO: IncludedNote            // BT-127, Detailinformationen zum Freitext zur Position, Basic+Comfort+Extended+XRechnung

    // handelt es sich um einen Kommentar?
    var isCommentItem : Boolean := false;
    if tradeLineItem.AssociatedDocument <> nil then
    if ((tradeLineItem.AssociatedDocument.Notes.Count > 0) and
        (tradeLineItem.BilledQuantity = 0) and
        (tradeLineItem.Description<>'')) then
    begin
      isCommentItem := true;
    end;

    //#region SpecifiedTradeProduct
    //Eine Gruppe von betriebswirtschaftlichen Begriffen, die Informationen über die in Rechnung gestellten Waren und Dienstleistungen enthält
    Writer.WriteStartElement('ram:SpecifiedTradeProduct', [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);
    if (tradeLineItem.GlobalID <> nil) then
    if (tradeLineItem.GlobalID.SchemeID <> TZUGFeRDGlobalIDSchemeIdentifiers.Unknown) and (tradeLineItem.GlobalID.ID<> '') then
    begin
      _writeElementWithAttribute(Writer, 'ram:GlobalID', 'schemeID', TZUGFeRDGlobalIDSchemeIdentifiersExtensions.EnumToString(tradeLineItem.GlobalID.SchemeID), tradeLineItem.GlobalID.ID, [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);
    end;

    Writer.WriteOptionalElementString('ram:SellerAssignedID', tradeLineItem.SellerAssignedID, PROFILE_COMFORT_EXTENDED_XRECHNUNG);
    Writer.WriteOptionalElementString('ram:BuyerAssignedID', tradeLineItem.BuyerAssignedID, PROFILE_COMFORT_EXTENDED_XRECHNUNG);

    // TODO: IndustryAssignedID     // BT-X-532, Von der Industrie zugewiesene Produktkennung
    // TODO: ModelID                // BT-X-533, Modelkennung des Artikels

    // BT-153
    Writer.WriteOptionalElementString('ram:Name', tradeLineItem.Name, [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended]);
    Writer.WriteOptionalElementString('ram:Name', ifthen(isCommentItem,'TEXT',tradeLineItem.Name), [TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]); // XRechnung erfordert einen Item-Namen (BR-25)

    Writer.WriteOptionalElementString('ram:Description', tradeLineItem.Description, PROFILE_COMFORT_EXTENDED_XRECHNUNG);

    // TODO: BatchID                // BT-X-534, Kennung der Charge (des Loses) des Artikels
    // TODO: BrandName              // BT-X-535, Markenname des Artikels
    // TODO: ModelName              // BT-X-536, Modellbezeichnung des Artikels

    // BG-32, Artikelattribute
    for var productCharacteristic : TZUGFeRDApplicableProductCharacteristic in tradeLineItem.ApplicableProductCharacteristics do
    begin
      Writer.WriteStartElement('ram:ApplicableProductCharacteristic');
      // TODO: TypeCode        // BT-X-11, Art der Produkteigenschaft (Code), Extended
      Writer.WriteOptionalElementString('ram:Description', productCharacteristic.Description);
      // TODO: ValueMeasure    // BT-X-12, Wert der Produkteigenschaft (numerische Messgröße), mit unitCode, Extended
      Writer.WriteOptionalElementString('ram:Value', productCharacteristic.Value);
      Writer.WriteEndElement(); // !ram:ApplicableProductCharacteristic
    end;

    for var designatedProductClassification : TZUGFeRDDesignatedProductClassification in tradeLineItem.DesignedProductClassifications do
    begin
      if (designatedProductClassification.ListId <> TZUGFeRDDesignatedProductClassificationClassCodes.Unknown) then
      begin
        Writer.WriteStartElement('ram:DesignatedProductClassification', PROFILE_COMFORT_EXTENDED_XRECHNUNG);
        Writer.WriteStartElement('ram:ClassCode');
        Writer.WriteAttributeString('listID', TZUGFeRDDesignatedProductClassificationClassCodesExtensions.EnumToString(designatedProductClassification.ListId));
        Writer.WriteAttributeString('listVersionID', designatedProductClassification.ListVersionID);
        Writer.WriteValue(designatedProductClassification.ClassCode);
        Writer.WriteEndElement(); // !ram::ClassCode
        Writer.WriteOptionalElementString('ram:ClassName', designatedProductClassification.ClassName_);
        Writer.WriteEndElement(); // !ram:DesignatedProductClassification
      end;
    end;

    // TODO: IndividualTradeProductInstance, BG-X-84, Artikel (Handelsprodukt) Instanzen

    // BT-159, Detailinformationen zur Produktherkunft
    if tradeLineItem.OriginTradeCountry <> nil then
    begin
      Writer.WriteStartElement('ram:OriginTradeCountry', PROFILE_COMFORT_EXTENDED_XRECHNUNG);
      Writer.WriteElementString('ram:ID', TZUGFeRDCountryCodesExtensions.EnumToString(tradeLineItem.OriginTradeCountry));
      Writer.WriteEndElement(); // !ram:OriginTradeCountry
    end;

    if (Descriptor.Profile=TZUGFeRDProfile.Extended) then
      for var includedItem : TZUGFeRDIncludedReferencedProduct in tradeLineItem.IncludedReferencedProducts do
      begin
        Writer.WriteStartElement('ram:IncludedReferencedProduct');
        // TODO: GlobalID, SellerAssignedID, BuyerAssignedID, IndustryAssignedID, Description
        Writer.WriteOptionalElementString('ram:Name', includedItem.Name);
        if includedItem.UnitQuantity.HasValue then
          _writeElementWithAttribute(Writer, 'ram:UnitQuantity', 'unitCode', TZUGFeRDQuantityCodesExtensions.EnumToString(tradeLineItem.UnitCode), _formatDecimal(includedItem.UnitQuantity.Value, 4));
        Writer.WriteEndElement(); // !ram:IncludedReferencedProduct
      end;

    Writer.WriteEndElement(); // !ram:SpecifiedTradeProduct(Basic|Comfort|Extended|XRechnung)
    //#endregion
    //#region SpecifiedLineTradeAgreement (Basic, Comfort, Extended, XRechnung)
    //Eine Gruppe von betriebswirtschaftlichen Begriffen, die Informationen über den Preis für die in der betreffenden Rechnungsposition in Rechnung gestellten Waren und Dienstleistungen enthält

    if (descriptor.Profile in [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]) then
    begin
      Writer.WriteStartElement('ram:SpecifiedLineTradeAgreement', [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);

      //#region BuyerOrderReferencedDocument (Comfort, Extended, XRechnung)
      //Detailangaben zur zugehörigen Bestellung
      if (tradeLineItem.BuyerOrderReferencedDocument <> nil)
      and (  ((Descriptor.Profile<>TZUGFeRDProfile.Extended) and (tradeLineItem.BuyerOrderReferencedDocument.LineID<>''))
          or ((Descriptor.Profile=TZUGFeRDProfile.Extended) and
               ((tradeLineItem.BuyerOrderReferencedDocument.LineID<>'')
               or (tradeLineItem.BuyerOrderReferencedDocument.ID<>'')
               or (tradeLineItem.BuyerOrderReferencedDocument.IssueDateTime.HasValue))) ) then
      begin
        Writer.WriteStartElement('ram:BuyerOrderReferencedDocument', PROFILE_COMFORT_EXTENDED_XRECHNUNG);

        //#region IssuerAssignedID
        //Bestellnummer
        Writer.WriteOptionalElementString('ram:IssuerAssignedID', tradeLineItem.BuyerOrderReferencedDocument.ID,[TZUGFeRDProfile.Extended]);
        //#endregion

        //#region LineID
        //Referenz zur Bestellposition
        Writer.WriteOptionalElementString('ram:LineID', tradeLineItem.BuyerOrderReferencedDocument.LineID);
        //#endregion

        //#region IssueDateTime
        if (tradeLineItem.BuyerOrderReferencedDocument.IssueDateTime.HasValue) then
        begin
          Writer.WriteStartElement('ram:FormattedIssueDateTime',[TZUGFeRDProfile.Extended]);
          Writer.WriteStartElement('qdt:DateTimeString');
          Writer.WriteAttributeString('format', '102');
          Writer.WriteValue(_formatDate(tradeLineItem.BuyerOrderReferencedDocument.IssueDateTime.Value));
          Writer.WriteEndElement(); // !qdt:DateTimeString
          Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
        end;
        //#endregion

        Writer.WriteEndElement(); // !ram:BuyerOrderReferencedDocument
      end;
      //#endregion

      //#region ContractReferencedDocument
      //Detailangaben zum zugehörigen Vertrag
      if (tradeLineItem.ContractReferencedDocument <> nil) then
      begin
        Writer.WriteStartElement('ram:ContractReferencedDocument', [TZUGFeRDProfile.Extended]);

        // reference to the contract position
        Writer.WriteOptionalElementString('ram:LineID', tradeLineItem.ContractReferencedDocument.LineID);

        if (tradeLineItem.ContractReferencedDocument.IssueDateTime.HasValue) then
        begin
          Writer.WriteStartElement('ram:FormattedIssueDateTime');
          Writer.WriteStartElement('qdt:DateTimeString');
          Writer.WriteAttributeString('format', '102');
          Writer.WriteValue(_formatDate(tradeLineItem.ContractReferencedDocument.IssueDateTime.Value));
          Writer.WriteEndElement(); // !udt:DateTimeString
          Writer.WriteEndElement(); // !ram:IssueDateTime
        end;
        Writer.WriteOptionalElementString('ram:IssuerAssignedID', tradeLineItem.ContractReferencedDocument.ID);
        Writer.WriteEndElement(); // !ram:ContractReferencedDocument(Extended)
      end;
      //#endregion

      //#region AdditionalReferencedDocument (Extended)
      //Detailangaben zu einer zusätzlichen Dokumentenreferenz
      for var document : TZUGFeRDAdditionalReferencedDocument in tradeLineItem.AdditionalReferencedDocuments do
      begin
        _writeAdditionalReferencedDocument(Writer, document, [TZUGFeRDProfile.Extended], 'BG-X-3');
      end; // !foreach(document)
      //#endregion

      //#region GrossPriceProductTradePrice (Comfort, Extended, XRechnung)
      var needToWriteGrossUnitPrice := false;

      // the PEPPOL business rule for XRechnung is very specific
      // PEPPOL-EN16931-R046
      if ((descriptor.Profile = TZUGFeRDProfile.XRechnung) and
          tradeLineItem.GrossUnitPrice.HasValue and
          (tradeLineItem.TradeAllowanceCharges.Count > 0)) then
      begin
          needToWriteGrossUnitPrice := true;
      end
      else
      if ((descriptor.Profile <> TZUGFeRDProfile.XRechnung) and
         (tradeLineItem.GrossUnitPrice.HasValue or (tradeLineItem.TradeAllowanceCharges.Count > 0))) then
      begin
        needToWriteGrossUnitPrice := true;
      end;

      if (needToWriteGrossUnitPrice) then
      begin
        Writer.WriteStartElement('ram:GrossPriceProductTradePrice', PROFILE_COMFORT_EXTENDED_XRECHNUNG);
        _writeOptionalAdaptiveAmount(Writer, 'ram:ChargeAmount', tradeLineItem.GrossUnitPrice, 2, 4); //BT-148
        if tradeLineItem.GrossQuantity.HasValue then
        begin
          Writer.WriteStartElement('ram:BasisQuantity');
          if tradeLineItem.GrossUnitCode.HasValue then
            Writer.WriteAttributeString('unitCode', TZUGFeRDQuantityCodesExtensions.EnumToString(tradeLineItem.GrossUnitCode));
         Writer.WriteValue(_formatDecimal(tradeLineItem.GrossQuantity.Value, 4));
         Writer.WriteEndElement(); // !ram:BasisQuantity
        end;

        for var tradeAllowanceCharge : TZUGFeRDTradeAllowanceCharge in tradeLineItem.TradeAllowanceCharges do //BT-147
        begin
          Writer.WriteStartElement('ram:AppliedTradeAllowanceCharge');

          //#region ChargeIndicator
          Writer.WriteStartElement('ram:ChargeIndicator');
          Writer.WriteElementString('udt:Indicator', ifthen(tradeAllowanceCharge.ChargeIndicator,'true','false'));
          Writer.WriteEndElement(); // !ram:ChargeIndicator
          //#endregion

          //#region ChargePercentage
          if (tradeAllowanceCharge.ChargePercentage <> 0.0) then
          begin
            Writer.WriteStartElement('ram:CalculationPercent', [TZUGFeRDProfile.Extended]);  // not in XRechnung, according to CII-SR-122
            Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ChargePercentage, 2));
            Writer.WriteEndElement();
          end;
          //#endregion

          //#region BasisAmount
          if (tradeAllowanceCharge.BasisAmount <> 0.0) then
          begin
            Writer.WriteStartElement('ram:BasisAmount', [TZUGFeRDProfile.Extended]); // not in XRechnung, according to CII-SR-123
            Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.BasisAmount, 2));
            Writer.WriteEndElement();
          end;
          //#endregion

          //#region ActualAmount
          Writer.WriteStartElement('ram:ActualAmount');
          Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ActualAmount, 2));
          Writer.WriteEndElement();
          //#endregion

          if tradeAllowanceCharge.ChargeIndicator then
          begin
            Writer.WriteOptionalElementString('ram:ReasonCode',
               TZUGFeRDSpecialServiceDescriptionCodesExtensions.EnumToString(
                                         tradeAllowanceCharge.ReasonCodeCharge));
          end else
          begin
            Writer.WriteOptionalElementString('ram:ReasonCode',
               TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(
                                         tradeAllowanceCharge.ReasonCodeAllowance));
          end;

          Writer.WriteOptionalElementString('ram:Reason', tradeAllowanceCharge.Reason, [TZUGFeRDProfile.Extended]);

          Writer.WriteEndElement(); // !AppliedTradeAllowanceCharge
        end;

        Writer.WriteEndElement(); // ram:GrossPriceProductTradePrice(Comfort|Extended|XRechnung)
      end;
      //#endregion // !GrossPriceProductTradePrice(Comfort|Extended|XRechnung)

      //#region NetPriceProductTradePrice
      //Im Nettopreis sind alle Zu- und Abschläge enthalten, jedoch nicht die Umsatzsteuer.
      Writer.WriteStartElement('ram:NetPriceProductTradePrice', [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);
      _writeOptionalAdaptiveAmount(Writer, 'ram:ChargeAmount', tradeLineItem.NetUnitPrice, 2, 4); //BT-146

      if tradeLineItem.NetQuantity.HasValue then
      begin
        Writer.WriteStartElement('ram:BasisQuantity');
        if tradeLineItem.NetUnitCode.HasValue then
          Writer.WriteAttributeString('unitCode', TZUGFeRDQuantityCodesExtensions.EnumToString(tradeLineItem.NetUnitCode));
       Writer.WriteValue(_formatDecimal(tradeLineItem.NetQuantity.Value, 4));
       Writer.WriteEndElement(); // !ram:BasisQuantity
      end;

      Writer.WriteEndElement(); // ram:NetPriceProductTradePrice(Basic|Comfort|Extended|XRechnung)
      //#endregion // !NetPriceProductTradePrice(Basic|Comfort|Extended|XRechnung)

      //#region UltimateCustomerOrderReferencedDocument
      //ToDo: UltimateCustomerOrderReferencedDocument
      //#endregion
      Writer.WriteEndElement(); // ram:SpecifiedLineTradeAgreement
    end;
    //#endregion

    //#region SpecifiedLineTradeDelivery (Basic, Comfort, Extended)
    Writer.WriteStartElement('ram:SpecifiedLineTradeDelivery', [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);
    _writeElementWithAttribute(Writer, 'ram:BilledQuantity', 'unitCode', TZUGFeRDQuantityCodesExtensions.EnumToString(tradeLineItem.UnitCode), _formatDecimal(tradeLineItem.BilledQuantity, 4));
    if tradeLineItem.ChargeFreeQuantity.HasValue then
      _writeElementWithAttribute(Writer, 'ram:ChargeFreeQuantity', 'unitCode', TZUGFeRDQuantityCodesExtensions.EnumToString(tradeLineItem.ChargeFreeUnitCode), _formatDecimal(tradeLineItem.ChargeFreeQuantity, 4));
    if tradeLineItem.PackageQuantity.HasValue then
      _writeElementWithAttribute(Writer, 'ram:PackageQuantity', 'unitCode', TZUGFeRDQuantityCodesExtensions.EnumToString(tradeLineItem.PackageUnitCode), _formatDecimal(tradeLineItem.PackageQuantity, 4));
    if Assigned(tradeLineItem.ShipTo) and (Descriptor.Profile=TZUGFeRDProfile.Extended) then
      _writeOptionalParty(Writer, TZUGFeRDPartyTypes.ShipToTradeParty, tradeLineItem.ShipTo, tradeLineItem.ShipToContact, tradeLineItem.ShipToElectronicAddress, tradeLineItem.ShipToTaxRegistration);

    if Assigned(tradeLineItem.UltimateShipTo) and (Descriptor.Profile=TZUGFeRDProfile.Extended) then
      _writeOptionalParty(Writer, TZUGFeRDPartyTypes.UltimateShipToTradeParty, tradeLineItem.UltimateShipTo, tradeLineItem.UltimateShipToContact, tradeLineItem.UltimateShipToElectronicAddress, tradeLineItem.UltimateShipToTaxRegistration);

    if tradeLineItem.ActualDeliveryDate.HasValue then
    begin
      Writer.WriteStartElement('ram:ActualDeliverySupplyChainEvent', ALL_PROFILES - [TZUGFeRDProfile.XRechnung1, TZUGFeRDProfile.XRechnung]); // this violates CII-SR-170 for XRechnung 3
      Writer.WriteStartElement('ram:OccurrenceDateTime');
      Writer.WriteStartElement('udt:DateTimeString');
      Writer.WriteAttributeString('format', '102');
      Writer.WriteValue(_formatDate(tradeLineItem.ActualDeliveryDate.Value));
      Writer.WriteEndElement(); // !udt:DateTimeString
      Writer.WriteEndElement(); // !OccurrenceDateTime
      Writer.WriteEndElement(); // !ActualDeliverySupplyChainEvent
    end;

    if (tradeLineItem.DeliveryNoteReferencedDocument <> nil) then
    begin
        Writer.WriteStartElement('ram:DeliveryNoteReferencedDocument', ALL_PROFILES - [TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]); // this violates CII-SR-175 for XRechnung 3
        Writer.WriteOptionalElementString('ram:IssuerAssignedID', tradeLineItem.DeliveryNoteReferencedDocument.ID);

        // reference to the delivery note item
        Writer.WriteOptionalElementString('ram:LineID', tradeLineItem.DeliveryNoteReferencedDocument.LineID);

        if (tradeLineItem.DeliveryNoteReferencedDocument.IssueDateTime.HasValue) then
        begin
          Writer.WriteStartElement('ram:FormattedIssueDateTime');
          Writer.WriteStartElement('qdt:DateTimeString');
          Writer.WriteAttributeString('format', '102');
          Writer.WriteValue(_formatDate(tradeLineItem.DeliveryNoteReferencedDocument.IssueDateTime.Value));
          Writer.WriteEndElement(); // !qdt:DateTimeString
          Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
        end;

        Writer.WriteEndElement(); // !ram:DeliveryNoteReferencedDocument
    end;

    Writer.WriteEndElement(); // !ram:SpecifiedLineTradeDelivery
    //#endregion

    //#region SpecifiedLineTradeSettlement
    Writer.WriteStartElement('ram:SpecifiedLineTradeSettlement', [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);
    //#region ApplicableTradeTax
    Writer.WriteStartElement('ram:ApplicableTradeTax', [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);
    Writer.WriteElementString('ram:TypeCode', TZUGFeRDTaxTypesExtensions.EnumToString(tradeLineItem.TaxType));
    Writer.WriteOptionalElementString('ram:ExemptionReason', IfThen(tradeLineItem.TaxExemptionReason='', _translateTaxCategoryCode(tradeLineItem.TaxCategoryCode), tradeLineItem.TaxExemptionReason), [TZUGFeRDProfile.Extended]);  // BT-X-96
    Writer.WriteElementString('ram:CategoryCode', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(tradeLineItem.TaxCategoryCode)); // BT-151
    if tradeLineItem.TaxExemptionReasonCode.HasValue then
      Writer.WriteOptionalElementString('ram:ExemptionReasonCode', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(tradeLineItem.TaxExemptionReasonCode), [TZUGFeRDProfile.Extended]); // BT-X-97

    if (tradeLineItem.TaxCategoryCode <> TZUGFeRDTaxCategoryCodes.O) then // notwendig, damit die Validierung klappt
    begin
      Writer.WriteElementString('ram:RateApplicablePercent', _formatDecimal(tradeLineItem.TaxPercent));
    end;

    Writer.WriteEndElement(); // !ram:ApplicableTradeTax(Basic|Comfort|Extended|XRechnung)
    //#endregion // !ApplicableTradeTax(Basic|Comfort|Extended|XRechnung)

    //#region BillingSpecifiedPeriod
    if (tradeLineItem.BillingPeriodStart.HasValue or tradeLineItem.BillingPeriodEnd.HasValue) then
    begin
      Writer.WriteStartElement('ram:BillingSpecifiedPeriod', ALL_PROFILES-[TZUGFeRDProfile.Minimum]);
      if (tradeLineItem.BillingPeriodStart.HasValue) then
      begin
        Writer.WriteStartElement('ram:StartDateTime');
        _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(tradeLineItem.BillingPeriodStart.Value));
        Writer.WriteEndElement(); // !StartDateTime
      end;

      if (tradeLineItem.BillingPeriodEnd.HasValue) then
      begin
        Writer.WriteStartElement('ram:EndDateTime');
        _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(tradeLineItem.BillingPeriodEnd.Value));
        Writer.WriteEndElement(); // !EndDateTime
      end;
      Writer.WriteEndElement(); // !BillingSpecifiedPeriod
    end;
    //#endregion

    //#region SpecifiedTradeAllowanceCharge (Basic, Comfort, Extended, XRechnung)
    //Abschläge auf Ebene der Rechnungsposition (Basic, Comfort, Extended, XRechnung)
    if (descriptor.Profile in [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung]) then
    if (tradeLineItem.SpecifiedTradeAllowanceCharges.Count > 0) then
    begin
      for var specifiedTradeAllowanceCharge : TZUGFeRDTradeAllowanceCharge in tradeLineItem.SpecifiedTradeAllowanceCharges do // BG-27 BG-28
      begin
        Writer.WriteStartElement('ram:SpecifiedTradeAllowanceCharge');
        //#region ChargeIndicator
        Writer.WriteStartElement('ram:ChargeIndicator');
        Writer.WriteElementString('udt:Indicator', ifthen(specifiedTradeAllowanceCharge.ChargeIndicator,'true','false'));
        Writer.WriteEndElement(); // !ram:ChargeIndicator
        //#endregion

        //#region ChargePercentage
        if (specifiedTradeAllowanceCharge.ChargePercentage <> 0.0) then
        begin
          Writer.WriteStartElement('ram:CalculationPercent');
          Writer.WriteValue(_formatDecimal(specifiedTradeAllowanceCharge.ChargePercentage, 2));
          Writer.WriteEndElement();
        end;
        //#endregion

        //#region BasisAmount
        if (specifiedTradeAllowanceCharge.BasisAmount <> 0.0) then
        begin
          // according to CII-SR-123 not in XRechnung for *Applied*TradeAllowanceCharge  but valid for *Specified*TradeAllowanceCharge!
          Writer.WriteStartElement('ram:BasisAmount', ALL_PROFILES-[TZUGFeRDProfile.Basic]); // BT-137, BT-142
          Writer.WriteValue(_formatDecimal(specifiedTradeAllowanceCharge.BasisAmount, 2));
          Writer.WriteEndElement();
        end;
        //#endregion

        //#region ActualAmount
        Writer.WriteStartElement('ram:ActualAmount');
        Writer.WriteValue(_formatDecimal(specifiedTradeAllowanceCharge.ActualAmount, 2));
        Writer.WriteEndElement();
        //#endregion

        if specifiedTradeAllowanceCharge.ChargeIndicator then
        begin
          Writer.WriteOptionalElementString('ram:ReasonCode',
             TZUGFeRDSpecialServiceDescriptionCodesExtensions.EnumToString(
                                       specifiedTradeAllowanceCharge.ReasonCodeCharge));   // BT-140, BT-145
        end else
        begin
          Writer.WriteOptionalElementString('ram:ReasonCode',
             TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(
                                       specifiedTradeAllowanceCharge.ReasonCodeAllowance));  // BT-140, BT-145
        end;

        // according to CII-SR-123 not in XRechnung for *Applied*TradeAllowanceCharge  but valid for *Specified*TradeAllowanceCharge!
        Writer.WriteOptionalElementString('ram:Reason', specifiedTradeAllowanceCharge.Reason);

        Writer.WriteEndElement(); // !ram:SpecifiedTradeAllowanceCharge
      end;
    end;
    //#endregion

    //#region SpecifiedTradeSettlementLineMonetarySummation (Basic, Comfort, Extended)
    //Detailinformationen zu Positionssummen
    Writer.WriteStartElement('ram:SpecifiedTradeSettlementLineMonetarySummation');

    var _total : double := 0;

    if (tradeLineItem.LineTotalAmount.HasValue) then
    begin
      _total := tradeLineItem.LineTotalAmount.Value;
    end
    else if (tradeLineItem.NetUnitPrice.HasValue) then
    begin
      _total := tradeLineItem.NetUnitPrice.Value * tradeLineItem.BilledQuantity;
      if tradeLineItem.NetQuantity.HasValue then
      if (tradeLineItem.NetQuantity.Value <> 0) then
      begin
        _total := _total / tradeLineItem.NetQuantity.Value;
      end;
    end;

    Writer.WriteStartElement('ram:LineTotalAmount', [TZUGFeRDProfile.Basic,TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);
    Writer.WriteValue(_formatDecimal(_total));
    Writer.WriteEndElement(); // !ram:LineTotalAmount

    //ToDo: TotalAllowanceChargeAmount
    //Gesamtbetrag der Positionszu- und Abschläge
    Writer.WriteEndElement(); // ram:SpecifiedTradeSettlementMonetarySummation
    //#endregion

    //#region AdditionalReferencedDocument
    //Objektkennung auf Ebene der Rechnungsposition, BT-128-00
    for var document: TZUGFeRDAdditionalReferencedDocument in tradeLineItem.AdditionalReferencedDocuments do
    if document.TypeCode=TZUGFeRDAdditionalReferencedDocumentTypeCode.InvoiceDataSheet then // PEPPOL-EN16931-R101: Element Document reference can only be used for Invoice line object
    begin
      if document.ID<>'' then
      begin
        _writeAdditionalReferencedDocument(Writer, document, PROFILE_COMFORT_EXTENDED_XRECHNUNG, 'BT-128-00');
        if Descriptor.Profile=TZUGFeRDProfile.Extended then
          break; // only Extended allows multiple entries
      end;
    end;
    //#endregion

    //#region ReceivableSpecifiedTradeAccountingAccount
    //Detailinformationen zur Buchungsreferenz

    for var tradeAccountingAccount : TZUGFeRDReceivableSpecifiedTradeAccountingAccount in tradeLineItem.ReceivableSpecifiedTradeAccountingAccounts do
    begin
      Writer.WriteStartElement('ram:ReceivableSpecifiedTradeAccountingAccount', PROFILE_COMFORT_EXTENDED_XRECHNUNG);

      Writer.WriteStartElement('ram:ID');
      Writer.WriteValue(tradeAccountingAccount.TradeAccountID);
      Writer.WriteEndElement(); // !ram:ID

      if tradeAccountingAccount.TradeAccountTypeCode.HasValue then
      begin
        Writer.WriteStartElement('ram:TypeCode', [TZUGFeRDProfile.Extended]);
        Writer.WriteValue((Integer(tradeAccountingAccount.TradeAccountTypeCode.Value)).ToString);
        Writer.WriteEndElement(); // !ram:TypeCode
      end;

      Writer.WriteEndElement(); // !ram:ReceivableSpecifiedTradeAccountingAccount
      if Descriptor.Profile<>TZUGFeRDProfile.Extended then
        break;   // Only Extended allows multiple accounts per line item, otherwise break
    end;
    //#endregion

    Writer.WriteEndElement(); // !ram:SpecifiedLineTradeSettlement
    //#endregion

    Writer.WriteEndElement(); // !ram:IncludedSupplyChainTradeLineItem
  end; // !foreach(tradeLineItem)
  // #endregion

  //#region ApplicableHeaderTradeAgreement
  Writer.WriteStartElement('ram:ApplicableHeaderTradeAgreement');

  // BT-10
  Writer.WriteOptionalElementString('ram:BuyerReference', Descriptor.ReferenceOrderNo);

  //#region SellerTradeParty
  // BT-31: Descriptor.SellerTaxRegistration
  _writeOptionalParty(Writer, TZUGFeRDPartyTypes.SellerTradeParty, Descriptor.Seller, Descriptor.SellerContact, Descriptor.SellerElectronicAddress, Descriptor.SellerTaxRegistration);
  //#endregion

  //region BuyerTradeParty
  // BT-48: Descriptor.BuyerTaxRegistration
  _writeOptionalParty(Writer, TZUGFeRDPartyTypes.BuyerTradeParty, Descriptor.Buyer, Descriptor.BuyerContact, Descriptor.BuyerElectronicAddress, Descriptor.BuyerTaxRegistration);
  //#endregion

  // TODO: implement SellerTaxRepresentativeTradeParty
  // BT-63: the tax registration of the SellerTaxRepresentativeTradeParty

  //#region SellerOrderReferencedDocument (BT-14: Comfort, Extended)
  if (Descriptor.SellerOrderReferencedDocument <> nil) then
  if (Descriptor.SellerOrderReferencedDocument.ID <> '') then
  begin
    Writer.WriteStartElement('ram:SellerOrderReferencedDocument', PROFILE_COMFORT_EXTENDED_XRECHNUNG);
    Writer.WriteElementString('ram:IssuerAssignedID', Descriptor.SellerOrderReferencedDocument.ID); // BT-14
    if (Descriptor.SellerOrderReferencedDocument.IssueDateTime.HasValue) then
    begin
      Writer.WriteStartElement('ram:FormattedIssueDateTime', [TZUGFeRDProfile.Extended]);
      Writer.WriteStartElement('qdt:DateTimeString');
      Writer.WriteAttributeString('format', '102');
      Writer.WriteValue(_formatDate(Descriptor.SellerOrderReferencedDocument.IssueDateTime.Value));
      Writer.WriteEndElement(); // !qdt:DateTimeString
      Writer.WriteEndElement(); // !IssueDateTime()
    end;

    Writer.WriteEndElement(); // !SellerOrderReferencedDocument
  end;
  //#endregion

  //#region BuyerOrderReferencedDocument
  if (Descriptor.OrderNo <> '') then
  begin
    Writer.WriteStartElement('ram:BuyerOrderReferencedDocument');
    Writer.WriteElementString('ram:IssuerAssignedID', Descriptor.OrderNo);
    if (Descriptor.OrderDate.HasValue) then
    begin
      Writer.WriteStartElement('ram:FormattedIssueDateTime', [TZUGFeRDProfile.Extended]);
      Writer.WriteStartElement('qdt:DateTimeString');
      Writer.WriteAttributeString('format', '102');
      Writer.WriteValue(_formatDate(Descriptor.OrderDate.Value));
      Writer.WriteEndElement(); // !qdt:DateTimeString
      Writer.WriteEndElement(); // !IssueDateTime()
    end;

    Writer.WriteEndElement(); // !BuyerOrderReferencedDocument
  end;
  //#endregion

  //#region ContractReferencedDocument
  // BT-12
  if (Descriptor.ContractReferencedDocument <> nil) then
  begin
    Writer.WriteStartElement('ram:ContractReferencedDocument');
    Writer.WriteElementString('ram:IssuerAssignedID', Descriptor.ContractReferencedDocument.ID);
    if (Descriptor.ContractReferencedDocument.IssueDateTime.HasValue) then
    begin
      Writer.WriteStartElement('ram:FormattedIssueDateTime', ALL_PROFILES  - [TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);
      Writer.WriteStartElement('qdt:DateTimeString');
      Writer.WriteAttributeString('format', '102');
      Writer.WriteValue(_formatDate(Descriptor.ContractReferencedDocument.IssueDateTime.Value));
      Writer.WriteEndElement(); // !qdt:DateTimeString
      Writer.WriteEndElement(); // !IssueDateTime()
    end;

    Writer.WriteEndElement(); // !ram:ContractReferencedDocument
  end;
  //#endregion

  //#region 4. AdditionalReferencedDocument
  if (Descriptor.AdditionalReferencedDocuments <> nil) then  // BG-24
  begin
    for var document : TZUGFeRDAdditionalReferencedDocument in Descriptor.AdditionalReferencedDocuments do
    begin
      _writeAdditionalReferencedDocument(Writer, document, PROFILE_COMFORT_EXTENDED_XRECHNUNG, 'BG-24');
    end;
  end;
  //#endregion

  //#region SpecifiedProcuringProject
  if (Descriptor.SpecifiedProcuringProject <> nil) then
  begin
    Writer.WriteStartElement('ram:SpecifiedProcuringProject', PROFILE_COMFORT_EXTENDED_XRECHNUNG);
    Writer.WriteElementString('ram:ID', Descriptor.SpecifiedProcuringProject.ID, PROFILE_COMFORT_EXTENDED_XRECHNUNG);
    Writer.WriteElementString('ram:Name', Descriptor.SpecifiedProcuringProject.Name, PROFILE_COMFORT_EXTENDED_XRECHNUNG);
    Writer.WriteEndElement(); // !ram:SpecifiedProcuringProject
  end;
  //#endregion

  Writer.WriteEndElement(); // !ApplicableHeaderTradeAgreement
  //#endregion

  //#region ApplicableHeaderTradeDelivery
  Writer.WriteStartElement('ram:ApplicableHeaderTradeDelivery'); // Pflichteintrag
  if Descriptor.Profile<>TZUGFeRDProfile.Minimum then
    _writeOptionalParty(Writer, TZUGFeRDPartyTypes.ShipToTradeParty, Descriptor.ShipTo, Descriptor.ShipToContact, Nil, Descriptor.ShipToTaxRegistration);
  if Descriptor.Profile in [TZUGFeRDProfile.Extended, TZUGFeRDProfile.XRechnung1, TZUGFeRDProfile.XRechnung] then
    _writeOptionalParty(Writer, TZUGFeRDPartyTypes.UltimateShipToTradeParty, Descriptor.UltimateShipTo, Descriptor.UltimateShipToContact);
  if Descriptor.Profile=TZUGFeRDProfile.Extended then
    _writeOptionalParty(Writer, TZUGFeRDPartyTypes.ShipFromTradeParty, Descriptor.ShipFrom);

  //#region ActualDeliverySupplyChainEvent
  if (Descriptor.ActualDeliveryDate.HasValue) then
  begin
    Writer.WriteStartElement('ram:ActualDeliverySupplyChainEvent');
    Writer.WriteStartElement('ram:OccurrenceDateTime');
    Writer.WriteStartElement('udt:DateTimeString');
    Writer.WriteAttributeString('format', '102');
    Writer.WriteValue(_formatDate(Descriptor.ActualDeliveryDate.Value));
    Writer.WriteEndElement(); // 'udt:DateTimeString
    Writer.WriteEndElement(); // !OccurrenceDateTime()
    Writer.WriteEndElement(); // !ActualDeliverySupplyChainEvent
  end;
  //#endregion

  //#region DespatchAdviceReferencedDocument
  if (Descriptor.DespatchAdviceReferencedDocument <> nil) then
  begin
    Writer.WriteStartElement('ram:DespatchAdviceReferencedDocument');
    Writer.WriteElementString('ram:IssuerAssignedID', Descriptor.DespatchAdviceReferencedDocument.ID);

    if (Descriptor.DespatchAdviceReferencedDocument.IssueDateTime.HasValue) then
    begin
      Writer.WriteStartElement('ram:FormattedIssueDateTime');
      Writer.WriteStartElement('qdt:DateTimeString');
      Writer.WriteAttributeString('format', '102');
      Writer.WriteValue(_formatDate(Descriptor.DespatchAdviceReferencedDocument.IssueDateTime.Value));
      Writer.WriteEndElement(); // "qdt:DateTimeString
      Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
    end;

    Writer.WriteEndElement(); // !DespatchAdviceReferencedDocument
  end;
  //#endregion

  //#region DeliveryNoteReferencedDocument
  if (Descriptor.DeliveryNoteReferencedDocument <> nil) then
  begin
    Writer.WriteStartElement('ram:DeliveryNoteReferencedDocument');
    Writer.WriteElementString('ram:IssuerAssignedID', Descriptor.DeliveryNoteReferencedDocument.ID);

    if (Descriptor.DeliveryNoteReferencedDocument.IssueDateTime.HasValue) then
    begin
      Writer.WriteStartElement('ram:FormattedIssueDateTime', ALL_PROFILES  - [TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);
      Writer.WriteStartElement('qdt:DateTimeString');
      Writer.WriteAttributeString('format', '102');
      Writer.WriteValue(_formatDate(Descriptor.DeliveryNoteReferencedDocument.IssueDateTime.Value));
      Writer.WriteEndElement(); // 'qdt:DateTimeString
      Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
    end;

    Writer.WriteEndElement(); // !DeliveryNoteReferencedDocument
  end;
  //#endregion

  Writer.WriteEndElement(); // !ApplicableHeaderTradeDelivery
  //#endregion

  //#region ApplicableHeaderTradeSettlement
  Writer.WriteStartElement('ram:ApplicableHeaderTradeSettlement');
  // order of sub-elements of ApplicableHeaderTradeSettlement:
  //   1. CreditorReferenceID (optional)
  //   2. PaymentReference (optional)
  //   3. TaxCurrencyCode (optional)
  //   4. InvoiceCurrencyCode (optional)
  //   5. InvoiceIssuerReference (optional)
  //   6. InvoicerTradeParty (optional)
  //   7. InvoiceeTradeParty (optional)
  //   8. PayeeTradeParty (optional)
  //   9. TaxApplicableTradeCurrencyExchange (optional)
  //  10. SpecifiedTradeSettlementPaymentMeans (optional)
  //  11. ApplicableTradeTax (optional)
  //  12. BillingSpecifiedPeriod (optional)
  //  13. SpecifiedTradeAllowanceCharge (optional)
  //  14. SpecifiedLogisticsServiceCharge (optional)
  //  15. SpecifiedTradePaymentTerms (optional)
  //  16. SpecifiedTradeSettlementHeaderMonetarySummation
  //  17. InvoiceReferencedDocument (optional)
  //  18. ReceivableSpecifiedTradeAccountingAccount (optional)
  //  19. SpecifiedAdvancePayment (optional)

  //   1. CreditorReferenceID (optional)
  if Descriptor.PaymentMeans <> nil then
  if (Descriptor.PaymentMeans.SEPACreditorIdentifier<>'') then
  begin
    Writer.WriteOptionalElementString('ram:CreditorReferenceID', Descriptor.PaymentMeans.SEPACreditorIdentifier,  ALL_PROFILES  - [TZUGFeRDProfile.Minimum]);
  end;

  //   2. PaymentReference (optional)
  Writer.WriteOptionalElementString('ram:PaymentReference', Descriptor.PaymentReference, ALL_PROFILES  - [TZUGFeRDProfile.Minimum]);

  //   3. TaxCurrencyCode (optional)
  //   BT-6
	if (Descriptor.TaxCurrency <> TZUGFeRDCurrencyCodes.Unknown) then
  	Writer.WriteElementString('ram:TaxCurrencyCode', TZUGFeRDCurrencyCodesExtensions.EnumToString(Descriptor.TaxCurrency), [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]);

  //   4. InvoiceCurrencyCode (optional)
  Writer.WriteElementString('ram:InvoiceCurrencyCode', TZUGFeRDCurrencyCodesExtensions.EnumToString(Descriptor.Currency));

  //   5. InvoiceIssuerReference (optional)
	Writer.WriteOptionalElementString('ram:InvoiceIssuerReference', Descriptor.SellerReferenceNo, [TZUGFeRDProfile.Extended]);

	//   6. InvoicerTradeParty (optional)
	if Descriptor.Profile=TZUGFeRDProfile.Extended then
    _writeOptionalParty(Writer, TZUGFeRDPartyTypes.InvoicerTradeParty, Descriptor.Invoicer);

  //   7. InvoiceeTradeParty (optional)
  if Descriptor.Profile=TZUGFeRDProfile.Extended then
    _writeOptionalParty(Writer, TZUGFeRDPartyTypes.InvoiceeTradeParty, Descriptor.Invoicee, Nil, Nil, Descriptor.InvoiceeTaxRegistration);

  //   8. PayeeTradeParty (optional)
  if Descriptor.Profile<>TZUGFeRDProfile.Minimum then
    _writeOptionalParty(Writer, TZUGFeRDPartyTypes.PayeeTradeParty, Descriptor.Payee);

  //#region SpecifiedTradeSettlementPaymentMeans
  //  10. SpecifiedTradeSettlementPaymentMeans (optional), BG-16

  if (Descriptor.CreditorBankAccounts.Count = 0) and (Descriptor.DebitorBankAccounts.Count = 0) then
  begin
    if (Descriptor.PaymentMeans <> nil) then
    if (Descriptor.PaymentMeans.TypeCode <> TZUGFeRDPaymentMeansTypeCodes.Unknown) then
    begin
      Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans', ALL_PROFILES  - [TZUGFeRDProfile.Minimum]);
      Writer.WriteElementString('ram:TypeCode', TZUGFeRDPaymentMeansTypeCodesExtensions.EnumToString(Descriptor.PaymentMeans.TypeCode), ALL_PROFILES  - [TZUGFeRDProfile.Minimum]);
      Writer.WriteOptionalElementString('ram:Information', Descriptor.PaymentMeans.Information, PROFILE_COMFORT_EXTENDED_XRECHNUNG);

      if (Descriptor.PaymentMeans.FinancialCard <> nil) then
      begin
        Writer.WriteStartElement('ram:ApplicableTradeSettlementFinancialCard', PROFILE_COMFORT_EXTENDED_XRECHNUNG);
        Writer.WriteOptionalElementString('ram:ID', Descriptor.PaymentMeans.FinancialCard.Id);
        Writer.WriteOptionalElementString('ram:CardholderName', Descriptor.PaymentMeans.FinancialCard.CardholderName);
        Writer.WriteEndElement(); // !ram:ApplicableTradeSettlementFinancialCard
      end;
      Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
    end;
  end
  else
  begin
    for var creditorAccount : TZUGFeRDBankAccount in Descriptor.CreditorBankAccounts do
    begin
      Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans', ALL_PROFILES  - [TZUGFeRDProfile.Minimum]);

      if (Descriptor.PaymentMeans <> nil) then
      if (Descriptor.PaymentMeans.TypeCode <> TZUGFeRDPaymentMeansTypeCodes.Unknown) then
      begin
        Writer.WriteElementString('ram:TypeCode', TZUGFeRDPaymentMeansTypeCodesExtensions.EnumToString(Descriptor.PaymentMeans.TypeCode), ALL_PROFILES  - [TZUGFeRDProfile.Minimum]);
        Writer.WriteOptionalElementString('ram:Information', Descriptor.PaymentMeans.Information, PROFILE_COMFORT_EXTENDED_XRECHNUNG);

        if (Descriptor.PaymentMeans.FinancialCard <> nil) then
        begin
            Writer.WriteStartElement('ram:ApplicableTradeSettlementFinancialCard', PROFILE_COMFORT_EXTENDED_XRECHNUNG);
            Writer.WriteOptionalElementString('ram:ID', Descriptor.PaymentMeans.FinancialCard.Id);
            Writer.WriteOptionalElementString('ram:CardholderName', Descriptor.PaymentMeans.FinancialCard.CardholderName);
            Writer.WriteEndElement(); // !ram:ApplicableTradeSettlementFinancialCard
        end;
      end;

      Writer.WriteStartElement('ram:PayeePartyCreditorFinancialAccount', ALL_PROFILES  - [TZUGFeRDProfile.Minimum]);
      Writer.WriteElementString('ram:IBANID', creditorAccount.IBAN);
      Writer.WriteOptionalElementString('ram:AccountName', creditorAccount.Name, PROFILE_COMFORT_EXTENDED_XRECHNUNG);
      Writer.WriteOptionalElementString('ram:ProprietaryID', creditorAccount.ID);
      Writer.WriteEndElement(); // !PayeePartyCreditorFinancialAccount

      if (creditorAccount.BIC<>'') then
      begin
        Writer.WriteStartElement('ram:PayeeSpecifiedCreditorFinancialInstitution',PROFILE_COMFORT_EXTENDED_XRECHNUNG);
        Writer.WriteElementString('ram:BICID', creditorAccount.BIC);
        Writer.WriteEndElement(); // !PayeeSpecifiedCreditorFinancialInstitution
      end;

      Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
    end;

    for var debitorAccount : TZUGFeRDBankAccount in Descriptor.DebitorBankAccounts do
    begin
      Writer.WriteStartElement('ram:SpecifiedTradeSettlementPaymentMeans', ALL_PROFILES  - [TZUGFeRDProfile.Minimum]); // BG-16

      if (Descriptor.PaymentMeans <> nil) then
      if (Descriptor.PaymentMeans.TypeCode <> TZUGFeRDPaymentMeansTypeCodes.Unknown) then
      begin
        Writer.WriteElementString('ram:TypeCode', TZUGFeRDPaymentMeansTypeCodesExtensions.EnumToString(Descriptor.PaymentMeans.TypeCode), ALL_PROFILES  - [TZUGFeRDProfile.Minimum]);
        Writer.WriteOptionalElementString('ram:Information', Descriptor.PaymentMeans.Information, PROFILE_COMFORT_EXTENDED_XRECHNUNG);
      end;

      Writer.WriteStartElement('ram:PayerPartyDebtorFinancialAccount', ALL_PROFILES  - [TZUGFeRDProfile.Minimum]);
      Writer.WriteElementString('ram:IBANID', debitorAccount.IBAN);
      Writer.WriteOptionalElementString('ram:AccountName', debitorAccount.Name, PROFILE_COMFORT_EXTENDED_XRECHNUNG);
      Writer.WriteOptionalElementString('ram:ProprietaryID', debitorAccount.ID);
      Writer.WriteEndElement(); // !PayerPartyDebtorFinancialAccount

      Writer.WriteEndElement(); // !SpecifiedTradeSettlementPaymentMeans
    end;
  end;
  //#endregion

  //#region ApplicableTradeTax
  //  11. ApplicableTradeTax (optional)
  _writeOptionalTaxes(Writer);
  //#endregion

  //#region BillingSpecifiedPeriod
  //  12. BillingSpecifiedPeriod (optional)
  if (Descriptor.BillingPeriodStart.HasValue) or (Descriptor.BillingPeriodEnd.HasValue) then
  begin
    Writer.WriteStartElement('ram:BillingSpecifiedPeriod', ALL_PROFILES  - [TZUGFeRDProfile.Minimum]);
    if (Descriptor.BillingPeriodStart.HasValue) then
    begin
        Writer.WriteStartElement('ram:StartDateTime');
        _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(Descriptor.BillingPeriodStart));
        Writer.WriteEndElement(); // !StartDateTime
    end;

    if (Descriptor.BillingPeriodEnd.HasValue) then
    begin
        Writer.WriteStartElement('ram:EndDateTime');
        _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(Descriptor.BillingPeriodEnd));
        Writer.WriteEndElement(); // !EndDateTime
    end;
    Writer.WriteEndElement(); // !BillingSpecifiedPeriod
  end;
  //#endregion

  //13. SpecifiedTradeAllowanceCharge (optional)
  for var tradeAllowanceCharge : TZUGFeRDTradeAllowanceCharge in Descriptor.TradeAllowanceCharges do
  begin
    Writer.WriteStartElement('ram:SpecifiedTradeAllowanceCharge', ALL_PROFILES  - [TZUGFeRDProfile.Minimum]);
    Writer.WriteStartElement('ram:ChargeIndicator');
    Writer.WriteElementString('udt:Indicator', ifthen(tradeAllowanceCharge.ChargeIndicator,'true','false'));
    Writer.WriteEndElement(); // !ram:ChargeIndicator

    if (tradeAllowanceCharge.ChargePercentage <> 0.0) then
    begin
      Writer.WriteStartElement('ram:CalculationPercent');
      Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ChargePercentage, 2));
      Writer.WriteEndElement();
    end;

    if (tradeAllowanceCharge.BasisAmount <> 0.0) then
    begin
      Writer.WriteStartElement('ram:BasisAmount'); // BT-100
      Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.BasisAmount));
      Writer.WriteEndElement();
    end;

    // TODO: BasisQuantity (+unitCode), BT-X-269, Basismenge des Rabatts

    Writer.WriteStartElement('ram:ActualAmount');  // BT-99
    Writer.WriteValue(_formatDecimal(tradeAllowanceCharge.ActualAmount, 2));
    Writer.WriteEndElement();

    if tradeAllowanceCharge.ChargeIndicator then
    begin
      Writer.WriteOptionalElementString('ram:ReasonCode',
         TZUGFeRDSpecialServiceDescriptionCodesExtensions.EnumToString(
                                   tradeAllowanceCharge.ReasonCodeCharge));
    end else
    begin
      Writer.WriteOptionalElementString('ram:ReasonCode',
         TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(
                                   tradeAllowanceCharge.ReasonCodeAllowance));
    end;

    Writer.WriteOptionalElementString('ram:Reason', tradeAllowanceCharge.Reason);

    if (tradeAllowanceCharge.Tax <> nil) then
    begin
      Writer.WriteStartElement('ram:CategoryTradeTax');
      Writer.WriteElementString('ram:TypeCode', TZUGFeRDTaxTypesExtensions.EnumToString(tradeAllowanceCharge.Tax.TypeCode));
      if (tradeAllowanceCharge.Tax.CategoryCode <> TZUGFeRDTaxCategoryCodes.Unknown) then
        Writer.WriteElementString('ram:CategoryCode', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(tradeAllowanceCharge.Tax.CategoryCode));
      Writer.WriteElementString('ram:RateApplicablePercent', _formatDecimal(tradeAllowanceCharge.Tax.Percent));
      Writer.WriteEndElement();
    end;
    Writer.WriteEndElement();
  end;

  //  14. SpecifiedLogisticsServiceCharge (optional)
  for var serviceCharge : TZUGFeRDServiceCharge in Descriptor.ServiceCharges do
  begin
    Writer.WriteStartElement('ram:SpecifiedLogisticsServiceCharge', ALL_PROFILES  - [TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);
    Writer.WriteOptionalElementString('ram:Description', serviceCharge.Description);
    Writer.WriteElementString('ram:AppliedAmount', _formatDecimal(serviceCharge.Amount));
    if (serviceCharge.Tax <> nil) then
    begin
      Writer.WriteStartElement('ram:AppliedTradeTax');
      Writer.WriteElementString('ram:TypeCode', TZUGFeRDTaxTypesExtensions.EnumToString(serviceCharge.Tax.TypeCode));
      if (serviceCharge.Tax.CategoryCode <> TZUGFeRDTaxCategoryCodes.Unknown) then
        Writer.WriteElementString('ram:CategoryCode', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(serviceCharge.Tax.CategoryCode));
      Writer.WriteElementString('ram:RateApplicablePercent', _formatDecimal(serviceCharge.Tax.Percent));
      Writer.WriteEndElement();
    end;
    Writer.WriteEndElement();
  end;

  //  15. SpecifiedTradePaymentTerms (optional)
  //  The cardinality depends on the profile.
  case Descriptor.Profile of
    TZUGFeRDProfile.Unknown,
    TZUGFeRDProfile.Minimum: {do nothing};

    TZUGFeRDProfile.XRechnung:
    begin
      // in XRechnung there is only one SpecifiedTradePaymentTerms allowed

      var PaymentNotes: string := '';
      var FirstDueDate: ZUGFeRDNullable<TDateTime>;

      for var PaymentTerms: TZUGFeRDPaymentTerms in Descriptor.PaymentTermsList do
      begin
        var PaymentNote: string;
        PaymentNote:= System.StrUtils.ReplaceText(Trim(PaymentTerms.Description),'#',' '); // make sure no # is present
        if PaymentNote<>'' then
          PaymentNote:= PaymentNote+#13#10;
        if PaymentTerms.PaymentTermsType.HasValue then
        begin
          var formatSettings: TFormatSettings;
          formatSettings.DecimalSeparator := '.';
          PaymentNote:= PaymentNote+
            Format('#%s#TAGE=%d#PROZENT=%.2f#%s'#13#10, [
              IfThen(PaymentTerms.PaymentTermsType = TZUGFeRDPaymentTermsType.Verzug, 'VERZUG', 'SKONTO'),
              Round(PaymentTerms.DueDays.Value),
              PaymentTerms.Percentage.Value,
              IfThen(PaymentTerms.BaseAmount.HasValue, Format('BASISBETRAG=%.2f#', [PaymentTerms.BaseAmount.Value], formatSettings),'')
            ], formatSettings);
        end;
        if PaymentNotes='' then
          PaymentNotes:= PaymentNote
        else
          PaymentNotes:= PaymentNotes + PaymentNote;
        // there can only be one DueDate and we use the first we find
        if PaymentTerms.DueDate.HasValue and Not(FirstDueDate.HasValue) then
          FirstDueDate:= PaymentTerms.DueDate;
      end;
      Writer.WriteStartElement('ram:SpecifiedTradePaymentTerms');
      Writer.WriteOptionalElementString('ram:Description', PaymentNotes);
      if FirstDueDate.HasValue then
      begin
        Writer.WriteStartElement('ram:DueDateDateTime');
        _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(FirstDueDate.Value));
        Writer.WriteEndElement(); // !ram:DueDateDateTime
      end;
      if (_descriptor.PaymentMeans.SEPAMandateReference<>'')
      and (_descriptor.PaymentMeans.TypeCode in [TZUGFeRDPaymentMeansTypeCodes.DirectDebit, TZUGFeRDPaymentMeansTypeCodes.SEPADirectDebit]) then // BT-89 is only required/allowed on DirectDebit (BR-DE-29)
      begin
        Writer.WriteOptionalElementString('ram:DirectDebitMandateID', _descriptor.PaymentMeans.SEPAMandateReference);
      end;

      Writer.WriteEndElement();
    end;

    TZUGFeRDProfile.Extended:
    begin
      for var PaymentTerms: TZUGFeRDPaymentTerms in Descriptor.PaymentTermsList do
      begin
        Writer.WriteStartElement('ram:SpecifiedTradePaymentTerms');
        Writer.WriteOptionalElementString('ram:Description', PaymentTerms.Description);
        if (PaymentTerms.DueDate.HasValue) then
        begin
          Writer.WriteStartElement('ram:DueDateDateTime');
          _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(PaymentTerms.DueDate.Value));
          Writer.WriteEndElement(); // !ram:DueDateDateTime
        end;
        Writer.WriteOptionalElementString('ram:DirectDebitMandateID', _descriptor.PaymentMeans.SEPAMandateReference);
        _writeOptionalAmount(Writer, 'ram:PartialPaymentAmount', paymentTerms.PartialPaymentAmount);
        if PaymentTerms.PaymentTermsType.HasValue then
        begin
          if PaymentTerms.PaymentTermsType = TZUGFeRDPaymentTermsType.Skonto then
            Writer.WriteStartElement('ram:ApplicableTradePaymentDiscountTerms')
          else
            Writer.WriteStartElement('ram:ApplicableTradePaymentPenaltyTerms');
          if PaymentTerms.MaturityDate.HasValue then
          begin
            Writer.WriteStartElement('ram:BasisDateTime');
            _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(PaymentTerms.MaturityDate.Value));
            Writer.WriteEndElement(); // !ram:BasisDateTime
          end;
          if paymentTerms.DueDays.HasValue then
            _writeElementWithAttribute(Writer, 'ram:BasisPeriodMeasure', 'unitCode', TZUGFeRDQuantityCodesExtensions.EnumToString(TZUGFeRDQuantityCodes.DAY), IntToStr(paymentTerms.DueDays.Value));
          _writeOptionalAmount(Writer, 'ram:BasisAmount', paymentTerms.BaseAmount); // forceCurrency false by default
          Writer.WriteOptionalElementString('ram:CalculationPercent', _formatDecimal(paymentTerms.Percentage));
          if PaymentTerms.PaymentTermsType = TZUGFeRDPaymentTermsType.Skonto then
            _writeOptionalAmount(Writer, 'ram:ActualDiscountAmount', paymentTerms.ActualAmount)
          else
            _writeOptionalAmount(Writer, 'ram:ActualPenaltyAmount', paymentTerms.ActualAmount);
          Writer.WriteEndElement(); // !ram:ApplicableTradePaymentDiscountTerms or  !ram:ApplicableTradePaymentPenaltyTerms
        end;
        Writer.WriteEndElement();
      end;
      if (_descriptor.PaymentTermsList.Count=0) and (_descriptor.PaymentMeans.SEPAMandateReference<>'') then
      begin
        Writer.WriteStartElement('ram:SpecifiedTradePaymentTerms');
        Writer.WriteOptionalElementString('ram:DirectDebitMandateID', _descriptor.PaymentMeans.SEPAMandateReference);
        Writer.WriteEndElement();
      end;
    end;
  else
    for var PaymentTerms: TZUGFeRDPaymentTerms in Descriptor.PaymentTermsList do
    begin
      Writer.WriteStartElement('ram:SpecifiedTradePaymentTerms');
      Writer.WriteOptionalElementString('ram:Description', PaymentTerms.Description);
      if (PaymentTerms.DueDate.HasValue) then
      begin
        Writer.WriteStartElement('ram:DueDateDateTime');
        _writeElementWithAttribute(Writer, 'udt:DateTimeString', 'format', '102', _formatDate(PaymentTerms.DueDate.Value));
        Writer.WriteEndElement(); // !ram:DueDateDateTime
      end;
      Writer.WriteOptionalElementString('ram:DirectDebitMandateID', _descriptor.PaymentMeans.SEPAMandateReference);
      Writer.WriteEndElement(); // !ram:SpecifiedTradePaymentTerms
    end;
  end;

  //#region SpecifiedTradeSettlementHeaderMonetarySummation
  //  16. SpecifiedTradeSettlementHeaderMonetarySummation
  //Gesamtsummen auf Dokumentenebene
  Writer.WriteStartElement('ram:SpecifiedTradeSettlementHeaderMonetarySummation');
  _writeAmount(Writer, 'ram:LineTotalAmount', Descriptor.LineTotalAmount, 0.0, 2, false, ALL_PROFILES-[TZUGFeRDProfile.Minimum]);                    // Summe der Nettobeträge aller Rechnungspositionen
  _writeOptionalAmount(Writer, 'ram:ChargeTotalAmount', Descriptor.ChargeTotalAmount, 2, false, ALL_PROFILES-[TZUGFeRDProfile.Minimum]);        // Summe der Zuschläge auf Dokumentenebene
  _writeOptionalAmount(Writer, 'ram:AllowanceTotalAmount', Descriptor.AllowanceTotalAmount, 2, false, ALL_PROFILES-[TZUGFeRDProfile.Minimum]);  // Summe der Abschläge auf Dokumentenebene
 
  if (Descriptor.Profile = TZUGFeRDProfile.Extended) then
  begin
    // there shall be no currency for tax basis total amount, see
    // https://github.com/stephanstapel/ZUGFeRD-csharp/issues/56#issuecomment-655525467
    _writeOptionalAmount(Writer, 'ram:TaxBasisTotalAmount', Descriptor.TaxBasisAmount, 2, false);   // Rechnungsgesamtbetrag ohne Umsatzsteuer
  end
  else
  begin
    _writeOptionalAmount(Writer, 'ram:TaxBasisTotalAmount', Descriptor.TaxBasisAmount);   // Rechnungsgesamtbetrag ohne Umsatzsteuer
  end;
  _writeOptionalAmount(Writer, 'ram:TaxTotalAmount', Descriptor.TaxTotalAmount, 2, true);               // Gesamtbetrag der Rechnungsumsatzsteuer, Steuergesamtbetrag in Buchungswährung
  _writeOptionalAmount(Writer, 'ram:RoundingAmount', Descriptor.RoundingAmount, 2, false,[TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);  // RoundingAmount  //Rundungsbetrag
  _writeOptionalAmount(Writer, 'ram:GrandTotalAmount', Descriptor.GrandTotalAmount);                                // Rechnungsgesamtbetrag einschließlich Umsatzsteuer
  _writeOptionalAmount(Writer, 'ram:TotalPrepaidAmount', Descriptor.TotalPrepaidAmount);                            // Vorauszahlungsbetrag
  _writeOptionalAmount(Writer, 'ram:DuePayableAmount', Descriptor.DuePayableAmount);                                // Fälliger Zahlungsbetrag
  Writer.WriteEndElement(); // !ram:SpecifiedTradeSettlementMonetarySummation
  //#endregion

  for var invoiceReferencedDocument: TZUGFeRDInvoiceReferencedDocument in Descriptor.InvoiceReferencedDocuments do
  begin
    Writer.WriteStartElement('ram:InvoiceReferencedDocument',ALL_PROFILES-[TZUGFeRDProfile.Minimum]);
    Writer.WriteOptionalElementString('ram:IssuerAssignedID', InvoiceReferencedDocument.ID);
    if invoiceReferencedDocument.IssueDateTime.HasValue then
    begin
      Writer.WriteStartElement('ram:FormattedIssueDateTime');
      _writeElementWithAttribute(Writer, 'qdt:DateTimeString', 'format', '102', _formatDate(InvoiceReferencedDocument.IssueDateTime.Value));
      Writer.WriteEndElement(); // !ram:FormattedIssueDateTime
    end;
    Writer.WriteEndElement(); // !ram:InvoiceReferencedDocument
  end;

  //#region ReceivableSpecifiedTradeAccountingAccount
  for var tradeAccountingAccount : TZUGFeRDReceivableSpecifiedTradeAccountingAccount in Descriptor.ReceivableSpecifiedTradeAccountingAccounts do
    if tradeAccountingAccount.TradeAccountID<>'' then
    begin
      Writer.WriteStartElement('ram:ReceivableSpecifiedTradeAccountingAccount', ALL_PROFILES - [TZUGFeRDProfile.Minimum]);
      Writer.WriteStartElement('ram:ID');
      Writer.WriteValue(tradeAccountingAccount.TradeAccountID); // BT-19
      Writer.WriteEndElement(); // !ram:ID

      if tradeAccountingAccount.TradeAccountTypeCode.HasValue then
      begin
        Writer.WriteStartElement('ram:TypeCode', [TZUGFeRDProfile.Extended]);
        Writer.WriteValue(Integer(tradeAccountingAccount.TradeAccountTypeCode.Value).ToString);  // BT-X-290
        Writer.WriteEndElement(); // !ram:TypeCode
      end;

      Writer.WriteEndElement(); // !ram:ReceivableSpecifiedTradeAccountingAccount
      if not(Descriptor.Profile in [TZUGFeRDProfile.BasicWL, TZUGFeRDProfile.Extended]) then
        break;  // Only BasicWL and Extended allow multiple accounts
    end;

    // TODO: SpecifiedAdvancePayment (0..unbounded), BG-X-45
 //#endregion
  Writer.WriteEndElement(); // !ram:ApplicableHeaderTradeSettlement

  //#endregion

  Writer.WriteEndElement(); // !ram:SpecifiedSupplyChainTradeTransaction
  //#endregion

  Writer.WriteEndElement(); // !ram:Invoice
  Writer.WriteEndDocument();
  Writer.Flush();
  Writer.Free;

  _stream.Seek(streamPosition, soFromBeginning);
end;

procedure TZUGFeRDInvoiceDescriptor23CIIWriter._writeAdditionalReferencedDocument(
  _writer: TZUGFeRDProfileAwareXmlTextWriter;
  document: TZUGFeRDAdditionalReferencedDocument; profile: TZUGFeRDProfiles; parentElement: string);
begin
  if document.ID='' then
    exit;

  _writer.WriteStartElement('ram:AdditionalReferencedDocument', profile);
  _writer.WriteOptionalElementString('ram:IssuerAssignedID', document.ID);

  var subProfile:= profile;
  if parentElement='BG-24' then
    subProfile:= [TZUGFeRDProfile.Comfort, TZUGFeRDProfile.Extended, TZUGFeRDProfile.XRechnung]
  else
  if parentElement='BG-X-3' then
    subProfile:= [TZUGFeRDProfile.Extended];
  if (parentElement='BG-24') or (parentElement='BG-X-3') then
    _writer.WriteOptionalElementString('ram:URIID', document.URIID, subProfile);  // BT-124, BT-X-28
  if parentElement='BG-X-3' then
    _writer.WriteOptionalElementString('ram:LineID', document.LineID, subProfile);  // BT-X-29

  if document.TypeCode.HasValue then
    _writer.WriteElementString('ram:TypeCode', TZUGFeRDAdditionalReferencedDocumentTypeCodeExtensions.EnumToString(document.TypeCode));
  if document.ReferenceTypeCode.HasValue then
    if (((parentElement = 'BT-18-00') or (parentElement = 'BT-128-00')) and (document.TypeCode = TZUGFeRDAdditionalReferencedDocumentTypeCode.InvoiceDataSheet)) // CII-DT-024: ReferenceTypeCode is only allowed in BT-18-00 and BT-128-00 for InvoiceDataSheet
    or (parentElement = 'BG-X-3') then
      _writer.WriteElementString('ram:ReferenceTypeCode', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(document.ReferenceTypeCode));
  if (parentElement = 'BG-24') or (parentElement = 'BT-X-3') then
    _writer.WriteOptionalElementString('ram:Name', document.Name, subProfile);
  if (document.AttachmentBinaryObject <> nil) then
  if document.AttachmentBinaryObject.Size > 0 then
  begin
    _writer.WriteStartElement('ram:AttachmentBinaryObject', subProfile);  // BT-125, BT-X-31
    _writer.WriteAttributeString('filename', document.Filename);
    _writer.WriteAttributeString('mimeCode', TZUGFeRDMimeTypeMapper.GetMimeType(document.Filename));
    _writer.WriteValue(TZUGFeRDHelper.GetDataAsBase64(document.AttachmentBinaryObject));
    _writer.WriteEndElement(); // !AttachmentBinaryObject()
  end;
  if (document.IssueDateTime.HasValue) then
  begin
    _writer.WriteStartElement('ram:FormattedIssueDateTime', [TZUGFeRDProfile.Extended]);
    _writer.WriteStartElement('qdt:DateTimeString');
    _writer.WriteAttributeString('format', '102');
    _writer.WriteValue(_formatDate(document.IssueDateTime.Value));
    _writer.WriteEndElement(); // !udt:DateTimeString
    _writer.WriteEndElement(); // !ram:IssueDateTime
  end;
  _writer.WriteEndElement(); // !ram:AdditionalReferencedDocument
end;

procedure TZUGFeRDInvoiceDescriptor23CIIWriter._writeElementWithAttribute(
  _writer : TZUGFeRDProfileAwareXmlTextWriter;
  tagName : String; attributeName : String;
  attributeValue : String; nodeValue: String;
  profile: TZUGFeRDProfiles);
begin
  _writer.WriteStartElement(tagName,profile);
  _writer.WriteAttributeString(attributeName, attributeValue);
  _writer.WriteValue(nodeValue);
  _writer.WriteEndElement(); // !tagName
end;

procedure TZUGFeRDInvoiceDescriptor23CIIWriter._writeOptionalTaxes(
  _writer : TZUGFeRDProfileAwareXmlTextWriter);
begin
  for var tax : TZUGFeRDTax in Descriptor.Taxes do
  begin
    _writer.WriteStartElement('ram:ApplicableTradeTax');

    _writer.WriteStartElement('ram:CalculatedAmount');
    _writer.WriteValue(_formatDecimal(tax.TaxAmount));
    _writer.WriteEndElement(); // !CalculatedAmount

    _writer.WriteElementString('ram:TypeCode', TZUGFeRDTaxTypesExtensions.EnumToString(tax.TypeCode));
    _writer.WriteOptionalElementString('ram:ExemptionReason', tax.ExemptionReason);

    _writer.WriteStartElement('ram:BasisAmount');
    _writer.WriteValue(_formatDecimal(tax.BasisAmount));
    _writer.WriteEndElement(); // !BasisAmount

    if tax.LineTotalBasisAmount <> 0.0  then
    begin
      _writer.WriteStartElement('ram:LineTotalBasisAmount', [TZUGFeRDProfile.Extended]);
      _writer.WriteValue(_formatDecimal(tax.LineTotalBasisAmount));
      _writer.WriteEndElement(); // !LineTotalBasisAmount
    end;

    if tax.AllowanceChargeBasisAmount <> 0.0  then
    begin
      _writer.WriteStartElement('ram:AllowanceChargeBasisAmount', [TZUGFeRDProfile.Extended]);
      _writer.WriteValue(_formatDecimal(tax.AllowanceChargeBasisAmount));
      _writer.WriteEndElement(); // !AllowanceChargeBasisAmount
    end;

    if (tax.CategoryCode <> TZUGFeRDTaxCategoryCodes.Unknown) then
    begin
      _writer.WriteElementString('ram:CategoryCode', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(tax.CategoryCode));
    end;
    if tax.ExemptionReasonCode.HasValue then
      _writer.WriteElementString('ram:ExemptionReasonCode', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(tax.ExemptionReasonCode));
    if tax.TaxPointDate.HasValue then
    begin
      _writer.WriteStartElement('ram:TaxPointDate');
      _writer.WriteStartElement('udt:DateString');
      _writer.WriteAttributeString('format', '102');
      _writer.WriteValue(_formatDate(tax.TaxPointDate.Value));
      _writer.WriteEndElement(); // !udt:DateString
      _writer.WriteEndElement(); // !TaxPointDate
    end;
    if tax.DueDateTypeCode.HasValue then
      _writer.WriteElementString('ram:DueDateTypeCode', TZUGFeRDDateTypeCodesExtensions.EnumToString(tax.DueDateTypeCode));
    _writer.WriteElementString('ram:RateApplicablePercent', _formatDecimal(tax.Percent));
    _writer.WriteEndElement(); // !ApplicableTradeTax
  end;
end;

procedure TZUGFeRDInvoiceDescriptor23CIIWriter._writeNotes(
  _writer : TZUGFeRDProfileAwareXmlTextWriter;
  notes : TObjectList<TZUGFeRDNote>);
begin
  for var note : TZUGFeRDNote in notes do
  begin
    _writer.WriteStartElement('ram:IncludedNote');
    if (note.ContentCode <> TZUGFeRDContentCodes.Unknown) then
      _writer.WriteElementString('ram:ContentCode', TZUGFeRDContentCodesExtensions.EnumToString(note.ContentCode));
    _writer.WriteOptionalElementString('ram:Content', note.Content);
    if (note.SubjectCode <> TZUGFeRDSubjectCodes.Unknown) then
      _writer.WriteElementString('ram:SubjectCode', TZUGFeRDSubjectCodesExtensions.EnumToString(note.SubjectCode));
    _writer.WriteEndElement();
  end;
end;

procedure TZUGFeRDInvoiceDescriptor23CIIWriter._writeOptionalLegalOrganization(
  _writer : TZUGFeRDProfileAwareXmlTextWriter;
  legalOrganizationTag : String;
  legalOrganization : TZUGFeRDLegalOrganization;
  partyType : TZUGFeRDPartyTypes = TZUGFeRDPartyTypes.Unknown);
begin
  if (legalOrganization = nil) then
    exit;

  case partyType of
    TZUGFeRDPartyTypes.SellerTradeParty: ; // all profiles
    TZUGFeRDPartyTypes.BuyerTradeParty: ; // all profiles
    TZUGFeRDPartyTypes.ShipToTradeParty: if (Descriptor.Profile = TZUGFeRDProfile.Minimum) then exit; // // it is also possible to add ShipToTradeParty() to a LineItem. In this case, the correct profile filter is different!
    TZUGFeRDPartyTypes.PayeeTradeParty: if (Descriptor.Profile = TZUGFeRDProfile.Minimum) then exit; // BT-61 / BT-X-508-00
    TZUGFeRDPartyTypes.BuyerAgentTradeParty,
    TZUGFeRDPartyTypes.BuyerTaxRepresentativeTradeParty,
    TZUGFeRDPartyTypes.InvoiceeTradeParty,
    TZUGFeRDPartyTypes.InvoicerTradeParty,
    TZUGFeRDPartyTypes.PayerTradeParty,
    TZUGFeRDPartyTypes.ProductEndUserTradeParty,
    TZUGFeRDPartyTypes.SalesAgentTradeParty,
    TZUGFeRDPartyTypes.ShipFromTradeParty,
    TZUGFeRDPartyTypes.UltimateShipToTradeParty:
      if Descriptor.Profile<>TZUGFeRDProfile.Extended then
        exit;
  else
    exit;
  end;

  writer.WriteStartElement(legalOrganizationTag, [Descriptor.Profile]);
  if (legalOrganization.ID <> nil) then
  begin
    if (legalOrganization.ID.ID <> '') and (TZUGFeRDGlobalIDSchemeIdentifiersExtensions.EnumToString(legalOrganization.ID.SchemeID)<> '') then
    begin
      writer.WriteStartElement('ram:ID');
      writer.WriteAttributeString('schemeID', TZUGFeRDGlobalIDSchemeIdentifiersExtensions.EnumToString(legalOrganization.ID.SchemeID));
      writer.WriteValue(legalOrganization.ID.ID);
      writer.WriteEndElement();
    end
    else
    begin
      writer.WriteElementString('ram:ID', legalOrganization.ID.ID);
    end;
    // filter according to https://github.com/stephanstapel/ZUGFeRD-csharp/pull/221
    if (Descriptor.Profile = TZUGFeRDProfile.Extended)
    or ((partyType = TZUGFeRDPartyTypes.SellerTradeParty) and (Descriptor.Profile <> TZUGFeRDProfile.Minimum)) or
       ((partyType = TZUGFeRDPartyTypes.BuyerTradeParty) and (Descriptor.Profile in [TZUGFeRDProfile.Comfort, TZUGFeRDProfile.XRechnung1, TZUGFeRDProfile.XRechnung, TZUGFeRDProfile.Comfort])) then
      writer.WriteOptionalElementString('ram:TradingBusinessName', legalOrganization.TradingBusinessName, [Descriptor.Profile]);
  end;
  writer.WriteEndElement();
end;

procedure TZUGFeRDInvoiceDescriptor23CIIWriter._writeOptionalParty(
  _writer: TZUGFeRDProfileAwareXmlTextWriter;
  partyType : TZUGFeRDPartyTypes;
  party : TZUGFeRDParty;
  contact : TZUGFeRDContact = nil;
  electronicAddress : TZUGFeRDElectronicAddress = nil;
  taxRegistrations : TObjectList<TZUGFeRDTaxRegistration> = nil);
begin
  if (party = nil) then
    exit;

  case partyType of
    TZUGFeRDPartyTypes.SellerTradeParty:
      writer.WriteStartElement('ram:SellerTradeParty', [Descriptor.Profile]);
    TZUGFeRDPartyTypes.BuyerTradeParty:
      writer.WriteStartElement('ram:BuyerTradeParty', [Descriptor.Profile]);
    TZUGFeRDPartyTypes.ShipToTradeParty:
      writer.WriteStartElement('ram:ShipToTradeParty', [Descriptor.Profile]);
    TZUGFeRDPartyTypes.UltimateShipToTradeParty:
      writer.WriteStartElement('ram:UltimateShipToTradeParty', [Descriptor.Profile]);
    TZUGFeRDPartyTypes.ShipFromTradeParty:
      writer.WriteStartElement('ram:ShipFromTradeParty', [Descriptor.Profile]);
    TZUGFeRDPartyTypes.InvoiceeTradeParty:
      writer.WriteStartElement('ram:InvoiceeTradeParty', [Descriptor.Profile]);
    TZUGFeRDPartyTypes.PayeeTradeParty:
      writer.WriteStartElement('ram:PayeeTradeParty', [Descriptor.Profile]);
    TZUGFeRDPartyTypes.SalesAgentTradeParty:
      writer.WriteStartElement('ram:SalesAgentTradeParty', [Descriptor.Profile]);
    TZUGFeRDPartyTypes.BuyerTaxRepresentativeTradeParty:
      writer.WriteStartElement('ram:BuyerTaxRepresentativeTradeParty', [Descriptor.Profile]);
    TZUGFeRDPartyTypes.ProductEndUserTradeParty:
      writer.WriteStartElement('ram:ProductEndUserTradeParty', [Descriptor.Profile]);
    TZUGFeRDPartyTypes.BuyerAgentTradeParty:
      writer.WriteStartElement('ram:BuyerAgentTradeParty', [Descriptor.Profile]);
    TZUGFeRDPartyTypes.InvoicerTradeParty:
      writer.WriteStartElement('ram:InvoicerTradeParty', [Descriptor.Profile]);
    TZUGFeRDPartyTypes.PayerTradeParty:
      writer.WriteStartElement('ram:PayerTradeParty', [Descriptor.Profile]);
  else
    exit;
  end;

  if (Party.ID <> nil) then
  if (Party.ID.ID <> '')  then
  begin
    if (Party.ID.SchemeID <> TZUGFeRDGlobalIDSchemeIdentifiers.Unknown) then
    begin
      _writer.WriteStartElement('ram:ID');
      _writer.WriteAttributeString('schemeID', TZUGFeRDGlobalIDSchemeIdentifiersExtensions.EnumToString(Party.ID.SchemeID));
      _writer.WriteValue(Party.ID.ID);
      _writer.WriteEndElement();
    end
    else
    begin
      _writer.WriteElementString('ram:ID', Party.ID.ID);
    end;
  end;

  if (party.GlobalID <> nil) then
  if ((party.GlobalID.ID <> '') and (party.GlobalID.SchemeID <> TZUGFeRDGlobalIDSchemeIdentifiers.Unknown)) then
  begin
    writer.WriteStartElement('ram:GlobalID');
    writer.WriteAttributeString('schemeID', TZUGFeRDGlobalIDSchemeIdentifiersExtensions.EnumToString(party.GlobalID.SchemeID));
    writer.WriteValue(party.GlobalID.ID);
    writer.WriteEndElement();
  end;

  writer.WriteOptionalElementString('ram:Name', party.Name);
  writer.WriteOptionalElementString('ram:Description', Party.Description, [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung1, TZUGFeRDProfile.XRechnung]);

  _writeOptionalLegalOrganization(writer, 'ram:SpecifiedLegalOrganization', party.SpecifiedLegalOrganization, partyType);
  _writeOptionalContact(writer, 'ram:DefinedTradeContact', contact, [TZUGFeRDProfile.Comfort,TZUGFeRDProfile.Extended,TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);

  writer.WriteStartElement('ram:PostalTradeAddress');
  writer.WriteOptionalElementString('ram:PostcodeCode', party.Postcode); //buyer: BT-53
  writer.WriteOptionalElementString('ram:LineOne', ifthen(party.ContactName='',party.Street,party.ContactName)); //buyer: BT-50
  if (party.ContactName<>'')then
  begin
      writer.WriteOptionalElementString('ram:LineTwo', party.Street); //buyer: BT-51
  end;

  writer.WriteOptionalElementString('ram:LineThree', party.AddressLine3); //buyer: BT-163
  writer.WriteOptionalElementString('ram:CityName', party.City); //buyer: BT-52
  writer.WriteElementString('ram:CountryID', TZUGFeRDCountryCodesExtensions.EnumToString(party.Country)); //buyer: BT-55
  writer.WriteOptionalElementString('ram:CountrySubDivisionName', party.CountrySubdivisionName); // BT-79
  writer.WriteEndElement(); // !PostalTradeAddress

  if (electronicAddress <> nil) then
  begin
    if (ElectronicAddress.Address<>'') then
    begin
      writer.WriteStartElement('ram:URIUniversalCommunication');
      writer.WriteStartElement('ram:URIID');
      writer.WriteAttributeString('schemeID', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(electronicAddress.ElectronicAddressSchemeID));
      writer.WriteValue(electronicAddress.Address);
      writer.WriteEndElement();
      writer.WriteEndElement();
    end;
  end;

  if (taxRegistrations <> nil) then
  begin
      // for seller: BT-31
      // for buyer : BT-48
    for var _reg : TZUGFeRDTaxRegistration in taxRegistrations do
    begin
      if (_reg.No <> '') then
      begin
        writer.WriteStartElement('ram:SpecifiedTaxRegistration');
        writer.WriteStartElement('ram:ID');
        writer.WriteAttributeString('schemeID', TZUGFeRDTaxRegistrationSchemeIDExtensions.EnumToString(_reg.SchemeID));
        writer.WriteValue(_reg.No);
        writer.WriteEndElement();
        writer.WriteEndElement();
      end;
    end;
  end;
  writer.WriteEndElement(); // !*TradeParty
end;

procedure TZUGFeRDInvoiceDescriptor23CIIWriter._writeOptionalContact(
  _writer: TZUGFeRDProfileAwareXmlTextWriter; contactTag : String;
  contact : TZUGFeRDContact; profile : TZUGFeRDProfiles);
begin
  if contact = nil then
    exit;

  _writer.WriteStartElement(contactTag,profile);

  _writer.WriteOptionalElementString('ram:PersonName', contact.Name);
  _writer.WriteOptionalElementString('ram:DepartmentName', contact.OrgUnit);

  if (contact.PhoneNo <> '') then
  begin
    _writer.WriteStartElement('ram:TelephoneUniversalCommunication');
    _writer.WriteElementString('ram:CompleteNumber', contact.PhoneNo);
    _writer.WriteEndElement();
  end;

  if (contact.FaxNo <> '') then
  begin
    _writer.WriteStartElement('ram:FaxUniversalCommunication',ALL_PROFILES - [TZUGFeRDProfile.XRechnung1,TZUGFeRDProfile.XRechnung]);
    _writer.WriteElementString('ram:CompleteNumber', contact.FaxNo);
    _writer.WriteEndElement();
  end;

  if (contact.EmailAddress <> '') then
  begin
    _writer.WriteStartElement('ram:EmailURIUniversalCommunication');
    _writer.WriteElementString('ram:URIID', contact.EmailAddress);
    _writer.WriteEndElement();
  end;

  _writer.WriteEndElement();
end;

function TZUGFeRDInvoiceDescriptor23CIIWriter._translateTaxCategoryCode(
  taxCategoryCode : TZUGFeRDTaxCategoryCodes) : String;
begin
  Result := '';
  case taxCategoryCode of
    TZUGFeRDTaxCategoryCodes.A :;
    TZUGFeRDTaxCategoryCodes.AA :;
    TZUGFeRDTaxCategoryCodes.AB :;
    TZUGFeRDTaxCategoryCodes.AC :;
    TZUGFeRDTaxCategoryCodes.AD :;
    TZUGFeRDTaxCategoryCodes.AE : Result := 'Umkehrung der Steuerschuldnerschaft';
    TZUGFeRDTaxCategoryCodes.B: ;
    TZUGFeRDTaxCategoryCodes.C: ;
    TZUGFeRDTaxCategoryCodes.E: Result := 'steuerbefreit';
    TZUGFeRDTaxCategoryCodes.G: Result := 'freier Ausfuhrartikel, Steuer nicht erhoben';
    TZUGFeRDTaxCategoryCodes.H: ;
    TZUGFeRDTaxCategoryCodes.O: Result := 'Dienstleistungen außerhalb des Steueranwendungsbereichs';
    TZUGFeRDTaxCategoryCodes.S: Result := ''; // 'Normalsatz' (don't be verbose)
    TZUGFeRDTaxCategoryCodes.Z: Result := 'nach dem Nullsatz zu versteuernde Waren';
    TZUGFeRDTaxCategoryCodes.Unknown: ;
    TZUGFeRDTaxCategoryCodes.D: ;
    TZUGFeRDTaxCategoryCodes.F: ;
    TZUGFeRDTaxCategoryCodes.I: ;
    TZUGFeRDTaxCategoryCodes.J: ;
    TZUGFeRDTaxCategoryCodes.K: Result := 'Kein Ausweis der Umsatzsteuer bei innergemeinschaftlichen Lieferungen';
    TZUGFeRDTaxCategoryCodes.L: Result := 'IGIC (Kanarische Inseln)';
    TZUGFeRDTaxCategoryCodes.M: Result := 'IPSI (Ceuta/Melilla)';
  end;
end;

function TZUGFeRDInvoiceDescriptor23CIIWriter._translateInvoiceType(type_ : TZUGFeRDInvoiceType) : String;
begin
  case type_ of
    SelfBilledInvoice,
    Invoice: Result := 'RECHNUNG';
    SelfBilledCreditNote,
    CreditNote: Result := 'GUTSCHRIFT';
    DebitNote: Result := 'BELASTUNGSANZEIGE';
    DebitnoteRelatedToFinancialAdjustments: Result := 'WERTBELASTUNG';
    PartialInvoice: Result := 'TEILRECHNUNG';
    PrepaymentInvoice: Result := 'VORAUSZAHLUNGSRECHNUNG';
    InvoiceInformation: Result := 'KEINERECHNUNG';
    Correction,
    CorrectionOld: Result := 'KORREKTURRECHNUNG';
    else Result := '';
  end;
end;

function TZUGFeRDInvoiceDescriptor23CIIWriter.Validate(
  _descriptor: TZUGFeRDInvoiceDescriptor; _throwExceptions: Boolean): Boolean;
begin
  Result := false;
end;

function TZUGFeRDInvoiceDescriptor23CIIWriter._encodeInvoiceType(type_ : TZUGFeRDInvoiceType) : Integer;
begin
  if (Integer(type_) > 1000) then
    type_ := TZUGFeRDInvoiceType(Integer(type_)-1000);

  case type_ of
    TZUGFeRDInvoiceType.CorrectionOld: Result := Integer(TZUGFeRDInvoiceType.Correction);
    else Result := Integer(type_);
  end;
end;

procedure TZUGFeRDInvoiceDescriptor23CIIWriter._writeOptionalAmount(
  _writer: TZUGFeRDProfileAwareXmlTextWriter; tagName: string;
  value: ZUGFeRDNullable<Currency>;
  numDecimals: Integer; forceCurrency: Boolean; profile : TZUGFeRDProfiles);
begin
  if (value.HasValue) then // && (value.Value != decimal.MinValue))
  begin
    _writer.WriteStartElement(tagName,profile);
    if forceCurrency then
      _writer.WriteAttributeString('currencyID', TZUGFeRDCurrencyCodesExtensions.EnumToString(Descriptor.Currency));
    _writer.WriteValue(_formatDecimal(value.Value, numDecimals));
    _writer.WriteEndElement; // !tagName
  end;
end;

procedure TZUGFeRDInvoiceDescriptor23CIIWriter._writeAmount(
  _writer: TZUGFeRDProfileAwareXmlTextWriter; tagName: string;
  value: ZUGFeRDNullable<Currency>;
  defaultValue: Currency;
  numDecimals: Integer; forceCurrency: Boolean; profile : TZUGFeRDProfiles);
begin
  _writer.WriteStartElement(tagName,profile);
  if forceCurrency then
    _writer.WriteAttributeString('currencyID', TZUGFeRDCurrencyCodesExtensions.EnumToString(Descriptor.Currency));
  if value.HasValue then
    _writer.WriteValue(_formatDecimal(value.Value, numDecimals))
  else
    _writer.WriteValue(_formatDecimal(defaultValue, numDecimals));
  _writer.WriteEndElement; // !tagName
end;

procedure TZUGFeRDInvoiceDescriptor23CIIWriter._writeOptionalAdaptiveAmount(
  _writer: TZUGFeRDProfileAwareXmlTextWriter; _tagName: string;
  _value: ZUGFeRDNullable<Currency>;
  _numDecimals: Integer;
  _maxnumDecimals: Integer;
  _forceCurrency: boolean);
begin
  if _value.HasValue then
  begin
    _writer.WriteStartElement(_tagName);
    if _forceCurrency then
      _writer.WriteAttributeString('currencyID', TZUGFeRDCurrencyCodesExtensions.EnumToString(Descriptor.Currency));
    var
      rounded: Currency := RoundTo(_value.Value, -_numDecimals);
    if _value = rounded then
      _writer.WriteValue(_formatDecimal(_value.Value, _numDecimals))
    else
      _writer.WriteValue(_formatDecimal(_value.Value, _maxNumDecimals));
    writer.WriteEndElement; // !tagName
  end
end;


end.
