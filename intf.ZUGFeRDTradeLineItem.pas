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

unit intf.ZUGFeRDTradeLineItem;

interface

uses
  System.SysUtils,System.Generics.Collections,
  System.Generics.Defaults,System.Classes,
  intf.ZUGFeRDHelper,
  intf.ZUGFeRDGlobalID,
  intf.ZUGFeRDReferenceTypeCodes,
  intf.ZUGFeRDCurrencyCodes,
  intf.ZUGFeRDContractReferencedDocument,
  intf.ZUGFeRDReceivableSpecifiedTradeAccountingAccount,
  intf.ZUGFeRDAdditionalReferencedDocument,
  intf.ZUGFeRDApplicableProductCharacteristic,
  intf.ZUGFeRDTradeAllowanceCharge,
  intf.ZUGFeRDTaxRegistration,
  intf.ZUGFeRDTaxTypes,
  intf.ZUGFeRDTaxExemptionReasonCodes,
  intf.ZUGFeRDContact,
  intf.ZUGFeRDElectronicAddress,
  intf.ZUGFeRDQuantityCodes,
  intf.ZUGFeRDAssociatedDocument,
  intf.ZUGFeRDTaxCategoryCodes,
  intf.ZUGFeRDDeliveryNoteReferencedDocument,
  intf.ZUGFeRDBuyerOrderReferencedDocument,
  intf.ZUGFeRDAccountingAccountTypeCodes,
  intf.ZUGFeRDSpecialServiceDescriptionCodes,
  intf.ZUGFeRDAllowanceOrChargeIdentificationCodes,
  intf.ZUGFeRDDesignatedProductClassification,
  intf.ZUGFeRDDesignatedProductClassificationClassCodes,
  intf.ZUGFeRDCountryCodes,
  intf.ZUGFeRDAdditionalReferencedDocumentTypeCodes,
  intf.ZUGFeRDIncludedReferencedProduct,
  intf.ZUGFeRDParty
  ;

type
  /// <summary>
  ///  Structure holding item information
  ///
  /// Please note that you might use the object that is returned from InvoiceDescriptor.AddTradeLineItem(...) and use it
  /// to e.g. add an allowance charge using lineItem.AddTradeAllowanceCharge(...)
  /// </summary>
  TZUGFeRDTradeLineItem = class
  private
    FBilledQuantity: Double;
    FChargeFreeQuantity: ZUGFeRDNullable<Double>;
    FPackageQuantity: ZUGFeRDNullable<Double>;
    FNetQuantity: ZUGFeRDNullable<Double>;
    FGrossQuantity: ZUGFeRDNullable<Double>;
    FName: string;
    FContractReferencedDocument: TZUGFeRDContractReferencedDocument;
    FReceivableSpecifiedTradeAccountingAccounts: TObjectList<TZUGFeRDReceivableSpecifiedTradeAccountingAccount>;
    FAdditionalReferencedDocuments: TObjectList<TZUGFeRDAdditionalReferencedDocument>;
    FIncludedReferencedProducts: TObjectList<TZUGFeRDIncludedReferencedProduct>;
    FUnitCode: TZUGFeRDQuantityCodes;
    FChargeFreeUnitCode: TZUGFeRDQuantityCodes;
    FPackageUnitCode: TZUGFeRDQuantityCodes;
    FNetUnitCode: ZUGFeRDNullable<TZUGFeRDQuantityCodes>;
    FGrossUnitCode: ZUGFeRDNullable<TZUGFeRDQuantityCodes>;
    FBillingPeriodStart: ZUGFeRDNullable<TDateTime>;
    FApplicableProductCharacteristics: TObjectList<TZUGFeRDApplicableProductCharacteristic>;
    FDesignedProductClassifications: TObjectList<TZUGFeRDDesignatedProductClassification>;
    FOriginTradeCountry: ZUGFeRDNullable<TZUGFeRDCountryCodes>;
    FShipTo: TZUGFeRDParty;
    FShipToContact: TZUGFeRDContact;
    FShipToTaxRegistration: TObjectList<TZUGFeRDTaxRegistration>;
    FShipToElectronicAddress: TZUGFeRDElectronicAddress;
    FUltimateShipTo: TZUGFeRDParty;
    FUltimateShipToContact: TZUGFeRDContact;
    FUltimateShipToTaxRegistration: TObjectList<TZUGFeRDTaxRegistration>;
    FUltimateShipToElectronicAddress: TZUGFeRDElectronicAddress;
    FSellerAssignedID: string;
    FTradeAllowanceCharges: TObjectList<TZUGFeRDTradeAllowanceCharge>;
    FSpecifiedTradeAllowanceCharges : TObjectList<TZUGFeRDTradeAllowanceCharge>;
    FTaxPercent: Double;
    FTaxType: TZUGFeRDTaxTypes;
    FTaxExemptionReason: string;
    FTaxExemptionReasonCode: ZUGFeRDNullable<TZUGFeRDTaxExemptionReasonCodes>;
    FBuyerAssignedID: string;
    FActualDeliveryDate: ZUGFeRDNullable<TDateTime>;
    FBillingPeriodEnd: ZUGFeRDNullable<TDateTime>;
    FDescription: string;
    FAssociatedDocument: TZUGFeRDAssociatedDocument;
    FTaxCategoryCode: TZUGFeRDTaxCategoryCodes;
    FNetUnitPrice: ZUGFeRDNullable<Currency>;
    FLineTotalAmount: ZUGFeRDNullable<Double>; // TZUGFeRDZUGFeRDNullable<Double>;
    FDeliveryNoteReferencedDocument: TZUGFeRDDeliveryNoteReferencedDocument;
    FGlobalID: TZUGFeRDGlobalID;
    FBuyerOrderReferencedDocument: TZUGFeRDBuyerOrderReferencedDocument;
    FGrossUnitPrice: ZUGFeRDNullable<Currency>;
  public
    /// <summary>
    /// Initialisiert ein neues, leeres Handelspositionsobjekt
    /// </summary>
    constructor Create;
    destructor Destroy; override;

    procedure AddAdditionalReferencedDocument(id: string;
      code : IZUGFeRDNullableParam<TZUGFeRDReferenceTypeCodes> = Nil; issueDateTime: TDateTime = 0); overload;

    /// <summary>
		/// Add an additional reference document
		/// </summary>
		/// <param name="id">Document number such as delivery note no or credit memo no</param>
		/// <param name="typeCode"></param>
		/// <param name="issueDateTime">Document Date</param>
		/// <param name="name"></param>
		/// <param name="referenceTypeCode">Type of the referenced document</param>
		/// <param name="attachmentBinaryObject"></param>
		/// <param name="filename"></param>
    procedure AddAdditionalReferencedDocument(id: string;
      typeCode : TZUGFeRDAdditionalReferencedDocumentTypeCode;
      issueDateTime: TDateTime = 0; name : String = '';
      referenceTypeCode : IZUGFeRDNullableParam<TZUGFeRDReferenceTypeCodes> = Nil;
      attachmentBinaryObject : TStream = nil; filename : String = ''); overload;

    procedure AddIncludedReferencedProduct(Name: String; UnitQuantity: ZUGFeRDNullable<Double>; UnitCode: TZUGFeRDQuantityCodes);

    /// <summary>
		/// Adds an invoice line Buyer accounting reference. BT-133
    /// Please note that XRechnung/ FacturX allows a maximum of one such reference
		/// </summary>
    procedure AddReceivableSpecifiedTradeAccountingAccount(
      AccountID: string; AccountTypeCode: IZUGFeRDNullableParam<TZUGFeRDAccountingAccountTypeCodes> = Nil);

    /// <summary>
    /// As an allowance or charge on item level, attaching it to the corresponding item.
    /// </summary>
    /// <param name="isDiscount">Marks if its an allowance (true) or charge (false). Please note that the xml will present inversed values</param>
    /// <param name="currency">Currency of the allowance or surcharge</param>
    /// <param name="basisAmount">Basis aount for the allowance or surcharge, typicalls the net amount of the item</param>
    /// <param name="actualAmount">The actual allowance or surcharge amount</param>
    /// <param name="reason">Reason for the allowance or surcharge</param>
    /// <param name="reasonCodeCharge"></param>
    /// <param name="reasonCodeAllowance"></param>
    procedure AddTradeAllowanceCharge(isDiscount: Boolean;
      currency: TZUGFeRDCurrencyCodes; basisAmount, actualAmount: double;
      reason: string;
      reasonCodeCharge : TZUGFeRDSpecialServiceDescriptionCodes = TZUGFeRDSpecialServiceDescriptionCodes.Unknown;
      reasonCodeAllowance : TZUGFeRDAllowanceOrChargeIdentificationCodes = TZUGFeRDAllowanceOrChargeIdentificationCodes.Unknown); overload;

    /// <summary>
    /// As an allowance or charge on item level, attaching it to the corresponding item.
    /// </summary>
    /// <param name="isDiscount">Marks if its an allowance (true) or charge (false). Please note that the xml will present inversed values</param>
    /// <param name="currency">Currency of the allowance or surcharge</param>
    /// <param name="basisAmount">Basis aount for the allowance or surcharge, typicalls the net amount of the item</param>
    /// <param name="actualAmount">The actual allowance or surcharge amount</param>
    /// <param name="chargePercentage">Actual allowance or surcharge charge percentage</param>
    /// <param name="reason">Reason for the allowance or surcharge</param>
    /// <param name="reasonCodeCharge"></param>
    /// <param name="reasonCodeAllowance"></param>
    procedure AddTradeAllowanceCharge(isDiscount: Boolean;
      currency: TZUGFeRDCurrencyCodes; basisAmount, actualAmount: double;
      chargePercentage : Currency; reason: string;
      reasonCodeCharge : TZUGFeRDSpecialServiceDescriptionCodes = TZUGFeRDSpecialServiceDescriptionCodes.Unknown;
      reasonCodeAllowance : TZUGFeRDAllowanceOrChargeIdentificationCodes = TZUGFeRDAllowanceOrChargeIdentificationCodes.Unknown); overload;

    /// <summary>
    /// As an allowance or charge on total item price, attaching it to the corresponding item.
    /// </summary>
    /// <param name="isDiscount">Marks if its an allowance (true) or charge (false). Please note that the xml will present inversed values</param>
    /// <param name="currency">Currency of the allowance or surcharge</param>
    /// <param name="basisAmount">Basis aount for the allowance or surcharge, typicalls the net amount of the item</param>
    /// <param name="actualAmount">The actual allowance or surcharge amount</param>
    /// <param name="reason">Reason for the allowance or surcharge</param>
    procedure AddSpecifiedTradeAllowanceCharge(isDiscount: Boolean;
      currency: TZUGFeRDCurrencyCodes; basisAmount, actualAmount: double;
      reason: string;
      reasonCodeCharge : TZUGFeRDSpecialServiceDescriptionCodes = TZUGFeRDSpecialServiceDescriptionCodes.Unknown;
      reasonCodeAllowance : TZUGFeRDAllowanceOrChargeIdentificationCodes = TZUGFeRDAllowanceOrChargeIdentificationCodes.Unknown); overload;

    /// <summary>
    /// As an allowance or charge on total item price, attaching it to the corresponding item.
    /// </summary>
    /// <param name="isDiscount">Marks if its an allowance (true) or charge (false). Please note that the xml will present inversed values</param>
    /// <param name="currency">Currency of the allowance or surcharge</param>
    /// <param name="basisAmount">Basis aount for the allowance or surcharge, typicalls the net amount of the item</param>
    /// <param name="actualAmount">The actual allowance or surcharge amount</param>
    /// <param name="chargePercentage">Actual allowance or surcharge charge percentage</param>
    /// <param name="reason">Reason for the allowance or surcharge</param>
    procedure AddSpecifiedTradeAllowanceCharge(isDiscount: Boolean;
      currency: TZUGFeRDCurrencyCodes; basisAmount, actualAmount: double;
      chargePercentage : Currency; reason: string;
      reasonCodeCharge : TZUGFeRDSpecialServiceDescriptionCodes = TZUGFeRDSpecialServiceDescriptionCodes.Unknown;
      reasonCodeAllowance : TZUGFeRDAllowanceOrChargeIdentificationCodes = TZUGFeRDAllowanceOrChargeIdentificationCodes.Unknown); overload;

    /// <summary>
		/// Adds a product classification
		/// </summary>
		/// <param name="className">Classification name. If you leave className empty, it will be omitted in the output</param>
		/// <param name="classCode">Identifier of the item classification (optional)</param>
		/// <param name="listID">Product classification name (optional)</param>
		/// <param name="listVersionID">Version of product classification (optional)</param>
    procedure AddDesignatedProductClassification(listID  : TZUGFeRDDesignatedProductClassificationClassCodes; listVersionID : String = ''; className : String = ''; classCode: string = '');

    /// <summary>
    /// Recipient of the delivered goods. This party is optional and is written in Extended profile only
    ///
    /// BG-X-7
    /// </summary>
    property ShipTo: TZUGFeRDParty read FShipTo write FShipTo;
    property ShipToContact: TZUGFeRDContact read FShipToContact write FShipToContact;
    property ShipToTaxRegistration: TObjectList<TZUGFeRDTaxRegistration> read FShipToTaxRegistration;
    property ShipToElectronicAddress : TZUGFeRDElectronicAddress read FShipToElectronicAddress;


    /// <summary>
    /// Detailed information on the deviating final recipient. This party is optional and only relevant for Extended profile
    ///
    /// BG-X-10
    /// </summary>
    property UltimateShipTo: TZUGFeRDParty read FUltimateShipTo write FUltimateShipTo;
    property UltimateShipToContact: TZUGFeRDContact read FUltimateShipToContact write FUltimateShipToContact;
    property UltimateShipToTaxRegistration: TObjectList<TZUGFeRDTaxRegistration> read FUltimateShipToTaxRegistration;
    property UltimateShipToElectronicAddress : TZUGFeRDElectronicAddress read FUltimateShipToElectronicAddress;

    procedure SetContractReferencedDocument(contractReferencedId: string;
      contractReferencedDate: IZUGFeRDNullableParam<TDateTime>);

    procedure SetDeliveryNoteReferencedDocument(deliveryNoteId: string;
      deliveryNoteDate: IZUGFeRDNullableParam<TDateTime>);

    /// <summary>
		/// Please note that XRechnung/ FacturX allows a maximum of one such reference
    /// and will only output the referenced order line id
    /// but not issuer assigned id and date
		/// </summary>
    procedure SetOrderReferencedDocument(orderReferencedId: string;
      orderReferencedDate: IZUGFeRDNullableParam<TDateTime>);
  public
    /// <summary>
    /// The identification of articles based on a registered scheme
    ///
    /// The global identifier of the article is a globally unique identifier of the product being assigned to it by its
    /// producer, bases on the rules of a global standardisation body.
    /// </summary>
    property GlobalID: TZUGFeRDGlobalID read FGlobalID write FGlobalID;

    /// <summary>
    /// An identification of the item assigned by the seller.
    /// </summary>
    property SellerAssignedID: string read FSellerAssignedID write FSellerAssignedID;

    /// <summary>
    /// An identification of the item assigned by the buyer.
    /// </summary>
    property BuyerAssignedID: string read FBuyerAssignedID write FBuyerAssignedID;

    /// <summary>
    /// An article’s name
    /// </summary>
    property Name: string read FName write FName;

    /// <summary>
    /// The description of an item
    ///
    /// The item’s description makes it possible to describe a product and its properties more comprehensively
    /// than would be possible with just the article name.
    /// </summary>
    property Description: string read FDescription write FDescription;

    /// <summary>
    /// Invoiced quantity
    /// </summary>
    property BilledQuantity: Double read FBilledQuantity write FBilledQuantity;

    /// <summary>
    /// No charge quantity  BT-X-46
    /// </summary>
    property ChargeFreeQuantity: ZUGFeRDNullable<Double> read FChargeFreeQuantity write FChargeFreeQuantity;

    /// <summary>
    /// Package quantity
    /// </summary>
    property PackageQuantity: ZUGFeRDNullable<Double> read FPackageQuantity write FPackageQuantity;

    /// <summary>
    /// Invoice line net amount including (!) trade allowance charges for the line item
    /// BT-131
    /// </summary>
    property LineTotalAmount: ZUGFeRDNullable<Double> {TZUGFeRDZUGFeRDNullable<Double>} read FLineTotalAmount write FLineTotalAmount;

    /// <summary>
    /// Detailed information about the invoicing period
    ///
    /// Invoicing period start date
    /// </summary>
    property BillingPeriodStart: ZUGFeRDNullable<TDateTime> read FBillingPeriodStart write FBillingPeriodStart;

    /// <summary>
    /// Detailed information about the invoicing period
    ///
    /// Invoicing period end date
    /// </summary>
    property BillingPeriodEnd: ZUGFeRDNullable<TDateTime> read FBillingPeriodEnd write FBillingPeriodEnd;

    /// <summary>
    /// he code valid for the invoiced goods sales tax category
    /// </summary>
    property TaxCategoryCode: TZUGFeRDTaxCategoryCodes read FTaxCategoryCode write FTaxCategoryCode;

    /// <summary>
    /// Tax rate
    /// </summary>
    property TaxPercent: Double read FTaxPercent write FTaxPercent;

    /// <summary>
    /// Tax type
    /// </summary>
    property TaxType: TZUGFeRDTaxTypes read FTaxType write FTaxType default TZUGFeRDTaxTypes.VAT;

    /// <summary>
    /// Exemption Reason Text for no Tax
    ///
    /// BT-X-96
    /// </summary>
    property TaxExemptionReason: string read FTaxExemptionReason write FTaxExemptionReason;

    /// <summary>
    /// ExemptionReasonCode for no Tax
    ///
    /// BT-X-97
    /// </summary>
    property TaxExemptionReasonCode: ZUGFeRDNullable<TZUGFeRDTaxExemptionReasonCodes> read FTaxExemptionReasonCode write FTaxExemptionReasonCode;

    /// <summary>
    /// net unit price of the item
    /// BT-146
    /// </summary>
    property NetUnitPrice: ZUGFeRDNullable<Currency> read FNetUnitPrice write FNetUnitPrice;

    /// <summary>
    /// Net unit price quantity
    /// optional, if filled and if *Gross*UnitPrice is present then it should be equal to GrossQuantity
    ///  BT-149
    /// </summary>
    property NetQuantity: ZUGFeRDNullable<Double> read FNetQuantity write FNetQuantity;

    /// <summary>
    /// Net quantity unit of measure code
    /// optional and if filled should be equal to Unitcode
    /// BT-150
    /// </summary>
    property NetUnitCode: ZUGFeRDNullable<TZUGFeRDQuantityCodes> read FNetUnitCode write FNetUnitCode;

    /// <summary>
    /// gross unit price of the item
    /// </summary>
    property GrossUnitPrice: ZUGFeRDNullable<Currency> read FGrossUnitPrice write FGrossUnitPrice;

    /// <summary>
    /// The number of item units to which the gross unit price applies.
    /// if *Gross*UnitPrice is present and NetQuantity is filled, should be equal to NetQuantity
    /// BT-149-1
    /// </summary>
    property GrossQuantity: ZUGFeRDNullable<Double> read FGrossQuantity write FGrossQuantity;

    /// <summary>
    /// Gross quantity unit of measure code
    /// optional and if filled should be equal to Unitcode
    /// BT-150-1
    /// </summary>
    property GrossUnitCode: ZUGFeRDNullable<TZUGFeRDQuantityCodes> read FGrossUnitCode write FGrossUnitCode;

    /// <summary>
    /// Item Base Quantity Unit Code
    /// BT-130
    /// </summary>
    property UnitCode: TZUGFeRDQuantityCodes read FUnitCode write FUnitCode;

    /// <summary>
    /// Charge Free Quantity Unit Code
    /// </summary>
    property ChargeFreeUnitCode: TZUGFeRDQuantityCodes read FChargeFreeUnitCode write FChargeFreeUnitCode;

    /// <summary>
    /// Package Quantity Unit Code
    /// </summary>
    property PackageUnitCode: TZUGFeRDQuantityCodes read FPackageUnitCode write FPackageUnitCode;

    /// <summary>
    /// Identifier of the invoice line item
    /// </summary>
    property AssociatedDocument: TZUGFeRDAssociatedDocument read FAssociatedDocument write FAssociatedDocument;

    /// <summary>
    /// Detailed information about the actual Delivery
    /// </summary>
    property ActualDeliveryDate: ZUGFeRDNullable<TDateTime> read FActualDeliveryDate write FActualDeliveryDate;

    /// <summary>
    /// Details of the associated order
    /// </summary>
    property BuyerOrderReferencedDocument: TZUGFeRDBuyerOrderReferencedDocument read FBuyerOrderReferencedDocument write FBuyerOrderReferencedDocument;

    /// <summary>
    /// Detailed information about the corresponding delivery note
    /// </summary>
    property DeliveryNoteReferencedDocument: TZUGFeRDDeliveryNoteReferencedDocument read FDeliveryNoteReferencedDocument write FDeliveryNoteReferencedDocument;

    /// <summary>
    /// Details of the associated contract
    /// </summary>
    property ContractReferencedDocument: TZUGFeRDContractReferencedDocument read FContractReferencedDocument write FContractReferencedDocument;

    /// <summary>
    /// Details of an additional document reference
    /// </summary>
    property AdditionalReferencedDocuments: TObjectList<TZUGFeRDAdditionalReferencedDocument> read FAdditionalReferencedDocuments write FAdditionalReferencedDocuments;

    /// <summary>
    /// Included Items referenced from this trade product.
    ///
    /// BG-X-1
    /// </summary>
    property IncludedReferencedProducts: TObjectList<TZUGFeRDIncludedReferencedProduct> read FIncludedReferencedProducts write FIncludedReferencedProducts;

    /// <summary>
    /// A group of business terms providing information about the applicable surcharges or discounts on the total amount of the invoice
    /// </summary>
    property TradeAllowanceCharges: TObjectList<TZUGFeRDTradeAllowanceCharge> read FTradeAllowanceCharges write FTradeAllowanceCharges;

    /// <summary>
    /// A group of business terms providing information about the applicable surcharges or discounts on the total amount of the invoice item
    /// </summary>
    property SpecifiedTradeAllowanceCharges : TObjectList<TZUGFeRDTradeAllowanceCharge> read FSpecifiedTradeAllowanceCharges write FSpecifiedTradeAllowanceCharges;


    /// <summary>
    /// Detailed information on the accounting reference
    /// </summary>
    property ReceivableSpecifiedTradeAccountingAccounts: TObjectList<TZUGFeRDReceivableSpecifiedTradeAccountingAccount> read FReceivableSpecifiedTradeAccountingAccounts write FReceivableSpecifiedTradeAccountingAccounts;

    /// <summary>
    /// Additional product information
    /// </summary>
    property ApplicableProductCharacteristics: TObjectList<TZUGFeRDApplicableProductCharacteristic> read FApplicableProductCharacteristics write FApplicableProductCharacteristics;

    /// <summary>
    /// Detailed information on the item classification
    ///
    /// BG-158
    /// </summary>
    property DesignedProductClassifications: TObjectList<TZUGFeRDDesignatedProductClassification> read FDesignedProductClassifications write FDesignedProductClassifications;

    /// <summary>
    /// Detailed information on the item origin country
    /// BT-159
    /// </summary>
    property OriginTradeCountry: ZUGFeRDNullable<TZUGFeRDCountryCodes> read FOriginTradeCountry write FOriginTradeCountry;

  end;

implementation

constructor TZUGFeRDTradeLineItem.Create;
begin
  inherited;
  FGlobalID := TZUGFeRDGlobalID.Create;
  FNetUnitPrice:= 0.0;
  FGrossUnitPrice:= 0.0;
  FAssociatedDocument:= nil;
  FBillingPeriodStart.ClearValue;
  FShipTo:= nil;//TZUGFeRDParty.Create;
  FShipToContact:= nil;//TZUGFeRDContact.Create;
  FShipToTaxRegistration:= TObjectList<TZUGFeRDTaxRegistration>.Create;
  FShipToElectronicAddress:= TZUGFeRDElectronicAddress.Create;
  FUltimateShipTo:= nil;//TZUGFeRDParty.Create;
  FUltimateShipToContact:= nil;//TZUGFeRDContact.Create;
  FUltimateShipToTaxRegistration:= TObjectList<TZUGFeRDTaxRegistration>.Create;
  FUltimateShipToElectronicAddress:= TZUGFeRDElectronicAddress.Create;
  FBuyerOrderReferencedDocument:= nil;//TZUGFeRDBuyerOrderReferencedDocument.Create;
  FDeliveryNoteReferencedDocument:= nil;//TZUGFeRDDeliveryNoteReferencedDocument.Create;
  FContractReferencedDocument:= nil;//TZUGFeRDContractReferencedDocument.Create;
  FAdditionalReferencedDocuments:= TObjectList<TZUGFeRDAdditionalReferencedDocument>.Create;
  FIncludedReferencedProducts:= TObjectList<TZUGFeRDIncludedReferencedProduct>.Create;
  FTradeAllowanceCharges:= TObjectList<TZUGFeRDTradeAllowanceCharge>.Create;
  FSpecifiedTradeAllowanceCharges := TObjectList<TZUGFeRDTradeAllowanceCharge>.Create;
  FReceivableSpecifiedTradeAccountingAccounts:= TObjectList<TZUGFeRDReceivableSpecifiedTradeAccountingAccount>.Create;
  FApplicableProductCharacteristics := TObjectList<TZUGFeRDApplicableProductCharacteristic>.Create;
  FDesignedProductClassifications:= TObjectList<TZUGFeRDDesignatedProductClassification>.Create;
end;

destructor TZUGFeRDTradeLineItem.Destroy;
begin
  if Assigned(FGlobalID) then begin FGlobalID.Free; FGlobalID := nil; end;
  if Assigned(FAssociatedDocument) then begin FAssociatedDocument.Free; FAssociatedDocument := nil; end;
  if Assigned(FShipTo) then begin FShipTo.Free; FShipTo := nil; end;
  if Assigned(FShipToContact                  ) then begin FShipToContact.Free; FShipToContact := nil; end;
  if Assigned(FShipToTaxRegistration          ) then begin FShipToTaxRegistration.Free; FShipToTaxRegistration := nil; end;
  if Assigned(FShipToElectronicAddress        ) then begin FShipToElectronicAddress.Free; FShipToElectronicAddress := nil; end;
  if Assigned(FUltimateShipTo) then begin FUltimateShipTo.Free; FUltimateShipTo := nil; end;
  if Assigned(FUltimateShipToContact                  ) then begin FUltimateShipToContact.Free; FUltimateShipToContact := nil; end;
  if Assigned(FUltimateShipToTaxRegistration          ) then begin FUltimateShipToTaxRegistration.Free; FUltimateShipToTaxRegistration := nil; end;
  if Assigned(FUltimateShipToElectronicAddress        ) then begin FUltimateShipToElectronicAddress.Free; FUltimateShipToElectronicAddress := nil; end;
  if Assigned(FBuyerOrderReferencedDocument) then begin FBuyerOrderReferencedDocument.Free; FBuyerOrderReferencedDocument := nil; end;
  if Assigned(FDeliveryNoteReferencedDocument) then begin FDeliveryNoteReferencedDocument.Free; FDeliveryNoteReferencedDocument := nil; end;
  if Assigned(FContractReferencedDocument) then begin FContractReferencedDocument.Free; FContractReferencedDocument := nil; end;
  if Assigned(FAdditionalReferencedDocuments) then begin FAdditionalReferencedDocuments.Free; FAdditionalReferencedDocuments := nil; end;
  if Assigned(FIncludedReferencedProducts) then begin FIncludedReferencedProducts.Free; FIncludedReferencedProducts := nil; end;
  if Assigned(FTradeAllowanceCharges) then begin FTradeAllowanceCharges.Free; FTradeAllowanceCharges := nil; end;
  if Assigned(FSpecifiedTradeAllowanceCharges) then begin FSpecifiedTradeAllowanceCharges.Free; FSpecifiedTradeAllowanceCharges := nil; end;
  if Assigned(FReceivableSpecifiedTradeAccountingAccounts) then begin FReceivableSpecifiedTradeAccountingAccounts.Free; FReceivableSpecifiedTradeAccountingAccounts := nil; end;
  if Assigned(FApplicableProductCharacteristics) then begin FApplicableProductCharacteristics.Free; FApplicableProductCharacteristics := nil; end;
  if Assigned(FDesignedProductClassifications) then begin FDesignedProductClassifications.Free; FDesignedProductClassifications := nil; end;
  inherited;
end;

procedure TZUGFeRDTradeLineItem.AddTradeAllowanceCharge(
  isDiscount: Boolean; currency: TZUGFeRDCurrencyCodes;
  basisAmount: double; actualAmount: double;
  reason: string; reasonCodeCharge : TZUGFeRDSpecialServiceDescriptionCodes;
  reasonCodeAllowance : TZUGFeRDAllowanceOrChargeIdentificationCodes);
begin
  FTradeAllowanceCharges.Add(TZUGFeRDTradeAllowanceCharge.Create);
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].ChargeIndicator := not isDiscount;
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].Currency := currency;
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].ActualAmount := actualAmount;
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].BasisAmount := basisAmount;
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].ChargePercentage := 0;
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].ReasonCodeAllowance := reasonCodeAllowance;
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].ReasonCodeCharge := reasonCodeCharge;
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].Reason := reason;
end;

procedure TZUGFeRDTradeLineItem.AddTradeAllowanceCharge(
  isDiscount: Boolean; currency: TZUGFeRDCurrencyCodes;
  basisAmount: double; actualAmount: double;
  chargePercentage : Currency; reason: string;
  reasonCodeCharge : TZUGFeRDSpecialServiceDescriptionCodes;
  reasonCodeAllowance : TZUGFeRDAllowanceOrChargeIdentificationCodes);
begin
  FTradeAllowanceCharges.Add(TZUGFeRDTradeAllowanceCharge.Create);
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].ChargeIndicator := not isDiscount;
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].Currency := currency;
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].ActualAmount := actualAmount;
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].BasisAmount := basisAmount;
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].ChargePercentage := chargePercentage;
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].ReasonCodeAllowance := reasonCodeAllowance;
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].ReasonCodeCharge := reasonCodeCharge;
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].Reason := reason;
end;

procedure TZUGFeRDTradeLineItem.SetDeliveryNoteReferencedDocument(
  deliveryNoteId: string; deliveryNoteDate: IZUGFeRDNullableParam<TDateTime>);
begin
  if FDeliveryNoteReferencedDocument = nil then
    FDeliveryNoteReferencedDocument := TZUGFeRDDeliveryNoteReferencedDocument.Create;
  with FDeliveryNoteReferencedDocument do
  begin
    ID := deliveryNoteId;
    IssueDateTime:= deliveryNoteDate;
  end;
end;

procedure TZUGFeRDTradeLineItem.AddAdditionalReferencedDocument(
  id: string; code: IZUGFeRDNullableParam<TZUGFeRDReferenceTypeCodes> = Nil;
  issueDateTime: TDateTime = 0);
begin
  FAdditionalReferencedDocuments.Add(TZUGFeRDAdditionalReferencedDocument.Create(true));

  FAdditionalReferencedDocuments[FAdditionalReferencedDocuments.Count - 1].ID := id;
  if date <= 0 then
    FAdditionalReferencedDocuments[FAdditionalReferencedDocuments.Count - 1].IssueDateTime:= Nil
  else
    FAdditionalReferencedDocuments[FAdditionalReferencedDocuments.Count - 1].IssueDateTime:= date;
  FAdditionalReferencedDocuments[FAdditionalReferencedDocuments.Count - 1].ReferenceTypeCode := code;
end;

procedure TZUGFeRDTradeLineItem.AddAdditionalReferencedDocument(id: string;
  typeCode: TZUGFeRDAdditionalReferencedDocumentTypeCode;
  issueDateTime: TDateTime = 0; name : String = '';
  referenceTypeCode : IZUGFeRDNullableParam<TZUGFeRDReferenceTypeCodes> = Nil;
  attachmentBinaryObject : TStream = nil; filename : String = '');

begin
  FAdditionalReferencedDocuments.Add(TZUGFeRDAdditionalReferencedDocument.Create(true));

  FAdditionalReferencedDocuments[FAdditionalReferencedDocuments.Count - 1].ReferenceTypeCode := referenceTypeCode;
  FAdditionalReferencedDocuments[FAdditionalReferencedDocuments.Count - 1].ID := id;
  if date <= 0 then
    FAdditionalReferencedDocuments[FAdditionalReferencedDocuments.Count - 1].IssueDateTime:= Nil
  else
    FAdditionalReferencedDocuments[FAdditionalReferencedDocuments.Count - 1].IssueDateTime:= date;
  FAdditionalReferencedDocuments[FAdditionalReferencedDocuments.Count - 1].Name := name;
  FAdditionalReferencedDocuments[FAdditionalReferencedDocuments.Count - 1].AttachmentBinaryObject.Clear;
  FAdditionalReferencedDocuments[FAdditionalReferencedDocuments.Count - 1].AttachmentBinaryObject.LoadFromStream(attachmentBinaryObject);
  FAdditionalReferencedDocuments[FAdditionalReferencedDocuments.Count - 1].Filename := filename;
  FAdditionalReferencedDocuments[FAdditionalReferencedDocuments.Count - 1].TypeCode := typeCode;
end;

procedure TZUGFeRDTradeLineItem.AddIncludedReferencedProduct(Name: String; UnitQuantity: ZUGFeRDNullable<Double>; UnitCode: TZUGFeRDQuantityCodes);
begin
  FIncludedReferencedProducts.Add(TZUGFeRDIncludedReferencedProduct.Create);
  FIncludedReferencedProducts[FIncludedReferencedProducts.Count - 1].Name := Name;
  FIncludedReferencedProducts[FIncludedReferencedProducts.Count - 1].UnitQuantity := UnitQuantity;
  FIncludedReferencedProducts[FIncludedReferencedProducts.Count - 1].UnitCode := UnitCode;
end;

procedure TZUGFeRDTradeLineItem.SetOrderReferencedDocument(
  orderReferencedId: string; orderReferencedDate: IZUGFeRDNullableParam<TDateTime>);
begin
  if FBuyerOrderReferencedDocument = nil then
    FBuyerOrderReferencedDocument := TZUGFeRDBuyerOrderReferencedDocument.Create;
  with FBuyerOrderReferencedDocument do
  begin
    ID := orderReferencedId;
    IssueDateTime:= orderReferencedDate;
  end;
end;

procedure TZUGFeRDTradeLineItem.SetContractReferencedDocument(
  contractReferencedId: string; contractReferencedDate: IZUGFeRDNullableParam<TDateTime>);
begin
  if FContractReferencedDocument = nil then
    FContractReferencedDocument := ContractReferencedDocument.Create;
  with FContractReferencedDocument do
  begin
    ID := contractReferencedId;
    IssueDateTime:= contractReferencedDate;
  end;
end;

procedure TZUGFeRDTradeLineItem.AddDesignatedProductClassification(
  listID:  TZUGFeRDDesignatedProductClassificationClassCodes;
  listVersionID: String;
  className : String;
  classCode: string
);
begin
  DesignedProductClassifications.Add(TZUGFeRDDesignatedProductClassification.Create);
  DesignedProductClassifications.Last.ClassCode := classCode;
  DesignedProductClassifications.Last.ClassName_ := className;
  DesignedProductClassifications.Last.ListID := listID;
  DesignedProductClassifications.Last.ListVersionID := listVersionID;
end;

procedure TZUGFeRDTradeLineItem.AddReceivableSpecifiedTradeAccountingAccount(
  AccountID: string; AccountTypeCode: IZUGFeRDNullableParam<TZUGFeRDAccountingAccountTypeCodes> = Nil);
begin
  FReceivableSpecifiedTradeAccountingAccounts.Add(TZUGFeRDReceivableSpecifiedTradeAccountingAccount.Create);
  with FReceivableSpecifiedTradeAccountingAccounts[FReceivableSpecifiedTradeAccountingAccounts.Count - 1] do
  begin
    TradeAccountID := AccountID;
    TradeAccountTypeCode := AccountTypeCode.Value;
  end;
end;

procedure TZUGFeRDTradeLineItem.AddSpecifiedTradeAllowanceCharge(
  isDiscount: Boolean; currency: TZUGFeRDCurrencyCodes; basisAmount,
  actualAmount: double; chargePercentage: Currency; reason: string;
  reasonCodeCharge : TZUGFeRDSpecialServiceDescriptionCodes = TZUGFeRDSpecialServiceDescriptionCodes.Unknown;
  reasonCodeAllowance : TZUGFeRDAllowanceOrChargeIdentificationCodes = TZUGFeRDAllowanceOrChargeIdentificationCodes.Unknown);
begin
  FSpecifiedTradeAllowanceCharges.Add(TZUGFeRDTradeAllowanceCharge.Create);
  FSpecifiedTradeAllowanceCharges[FSpecifiedTradeAllowanceCharges.Count - 1].ChargeIndicator := not isDiscount;
  FSpecifiedTradeAllowanceCharges[FSpecifiedTradeAllowanceCharges.Count - 1].Currency := currency;
  FSpecifiedTradeAllowanceCharges[FSpecifiedTradeAllowanceCharges.Count - 1].ActualAmount := actualAmount;
  FSpecifiedTradeAllowanceCharges[FSpecifiedTradeAllowanceCharges.Count - 1].BasisAmount := basisAmount;
  FSpecifiedTradeAllowanceCharges[FSpecifiedTradeAllowanceCharges.Count - 1].ChargePercentage := chargePercentage;
  FSpecifiedTradeAllowanceCharges[FSpecifiedTradeAllowanceCharges.Count - 1].ReasonCodeAllowance := reasonCodeAllowance;
  FSpecifiedTradeAllowanceCharges[FSpecifiedTradeAllowanceCharges.Count - 1].ReasonCodeCharge := reasonCodeCharge;
  FSpecifiedTradeAllowanceCharges[FSpecifiedTradeAllowanceCharges.Count - 1].Reason := reason;
end;

procedure TZUGFeRDTradeLineItem.AddSpecifiedTradeAllowanceCharge(
  isDiscount: Boolean; currency: TZUGFeRDCurrencyCodes; basisAmount,
  actualAmount: double; reason: string;
  reasonCodeCharge : TZUGFeRDSpecialServiceDescriptionCodes = TZUGFeRDSpecialServiceDescriptionCodes.Unknown;
  reasonCodeAllowance : TZUGFeRDAllowanceOrChargeIdentificationCodes = TZUGFeRDAllowanceOrChargeIdentificationCodes.Unknown);
begin
  FSpecifiedTradeAllowanceCharges.Add(TZUGFeRDTradeAllowanceCharge.Create);
  FSpecifiedTradeAllowanceCharges[FSpecifiedTradeAllowanceCharges.Count - 1].ChargeIndicator := not isDiscount;
  FSpecifiedTradeAllowanceCharges[FSpecifiedTradeAllowanceCharges.Count - 1].Currency := currency;
  FSpecifiedTradeAllowanceCharges[FSpecifiedTradeAllowanceCharges.Count - 1].ActualAmount := actualAmount;
  FSpecifiedTradeAllowanceCharges[FSpecifiedTradeAllowanceCharges.Count - 1].BasisAmount := basisAmount;
  FSpecifiedTradeAllowanceCharges[FSpecifiedTradeAllowanceCharges.Count - 1].ChargePercentage := 0;
  FSpecifiedTradeAllowanceCharges[FSpecifiedTradeAllowanceCharges.Count - 1].ReasonCodeAllowance := reasonCodeAllowance;
  FSpecifiedTradeAllowanceCharges[FSpecifiedTradeAllowanceCharges.Count - 1].ReasonCodeCharge := reasonCodeCharge;
  FSpecifiedTradeAllowanceCharges[FSpecifiedTradeAllowanceCharges.Count - 1].Reason := reason;
end;

end.
