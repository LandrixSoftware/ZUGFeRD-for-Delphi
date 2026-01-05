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
  intf.ZUGFeRDChargeReasonCodes,
  intf.ZUGFeRDAllowanceReasonCodes,
  intf.ZUGFeRDDesignatedProductClassification,
  intf.ZUGFeRDDesignatedProductClassificationClassCodes,
  intf.ZUGFeRDCountryCodes,
  intf.ZUGFeRDAdditionalReferencedDocumentTypeCodes,
  intf.ZUGFeRDIncludedReferencedProduct,
  intf.ZUGFeRDParty,
  intf.ZUGFeRDLineStatusCodes,
  intf.ZUGFeRDLineStatusReasonCodes
  ;

type
  /// <summary>
  ///  Structure holding item information
  ///
  /// Please note that you might use the object that is returned from InvoiceDescriptor.AddTradeLineItem(...) and use it
  /// to e.g. add an allowance charge using lineItem.AddTradeAllowance(...)
  /// </summary>
  TZUGFeRDTradeLineItem = class
  private
    FBilledQuantity: Currency;
    FChargeFreeQuantity: ZUGFeRDNullable<Currency>;
    FPackageQuantity: ZUGFeRDNullable<Currency>;
    FNetQuantity: ZUGFeRDNullable<Currency>;
    FGrossQuantity: ZUGFeRDNullable<Currency>;
    FName: string;
    FContractReferencedDocument: TZUGFeRDContractReferencedDocument;
    FReceivableSpecifiedTradeAccountingAccounts: TObjectList<TZUGFeRDReceivableSpecifiedTradeAccountingAccount>;
    FAdditionalReferencedDocuments: TObjectList<TZUGFeRDAdditionalReferencedDocument>;
    FIncludedReferencedProducts: TObjectList<TZUGFeRDIncludedReferencedProduct>;
    FUnitCode: ZUGFeRDNullable<TZUGFeRDQuantityCodes>;
    FChargeFreeUnitCode: ZUGFeRDNullable<TZUGFeRDQuantityCodes>;
    FPackageUnitCode: ZUGFeRDNullable<TZUGFeRDQuantityCodes>;
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
    FIndustryAssignedID: string;
    FModelID: string;
    FBatchID: string;
    FBrandName: string;
    FModelName: string;
    FTradeAllowanceCharges: TObjectList<TZUGFeRDAbstractTradeAllowanceCharge>;
    FSpecifiedTradeAllowanceCharges : TObjectList<TZUGFeRDAbstractTradeAllowanceCharge>;
    FTaxPercent: Currency;
    FTaxType: ZUGFeRDNullable<TZUGFeRDTaxTypes>;
    FTaxExemptionReason: string;
    FTaxExemptionReasonCode: ZUGFeRDNullable<TZUGFeRDTaxExemptionReasonCodes>;
    FBuyerAssignedID: string;
    FActualDeliveryDate: ZUGFeRDNullable<TDateTime>;
    FBillingPeriodEnd: ZUGFeRDNullable<TDateTime>;
    FDescription: string;
    FAssociatedDocument: TZUGFeRDAssociatedDocument;
    FTaxCategoryCode: ZUGFeRDNullable<TZUGFeRDTaxCategoryCodes>;
    FNetUnitPrice: ZUGFeRDNullable<Currency>;
    FLineTotalAmount: ZUGFeRDNullable<Currency>;
    FDeliveryNoteReferencedDocument: TZUGFeRDDeliveryNoteReferencedDocument;
    FGlobalID: TZUGFeRDGlobalID;
    FBuyerOrderReferencedDocument: TZUGFeRDBuyerOrderReferencedDocument;
    FGrossUnitPrice: ZUGFeRDNullable<Currency>;
  public
    /// <summary>
    /// Initialisiert ein neues, leeres Handelspositionsobjekt
    /// </summary>
    constructor Create (LineID: string);
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

    procedure AddIncludedReferencedProduct(Name: String; UnitQuantity: ZUGFeRDNullable<Currency>; UnitCode: TZUGFeRDQuantityCodes);

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
    procedure AddTradeAllowanceCharge(isDiscount: Boolean; currency: TZUGFeRDCurrencyCodes;
      basisAmount: ZUGFeRDNullable<Currency>; actualAmount: Currency;
      reason: string;
      reasonCode: IZUGFeRDNullableParam<TZUGFeRDAllowanceReasonCodes> = Nil);

    /// <summary>
    /// As an allowance or charge on item level, attaching it to the corresponding item.
    /// </summary>
    /// <param name="currency">Currency of the allowance or surcharge</param>
    /// <param name="basisAmount">Basis aount for the allowance or surcharge, typicalls the net amount of the item</param>
    /// <param name="actualAmount">The actual allowance or surcharge amount</param>
    /// <param name="chargePercentage">Actual allowance or surcharge charge percentage</param>
    /// <param name="reason">Reason for the allowance or surcharge</param>
    /// <param name="reasonCode">Reason code for the allowance or surcharge</param>
    function AddTradeAllowance (currency: TZUGFeRDCurrencyCodes;
      basisAmount: ZUGFeRDNullable<Currency>; actualAmount: Currency;
      chargePercentage: Currency;
      reason: string; reasonCode : IZUGFeRDNullableParam<TZUGFeRDAllowanceReasonCodes> = Nil): TZUGFeRDTradeLineItem; overload;

    /// <summary>
    /// As an allowance or charge on item level, attaching it to the corresponding item.
    /// </summary>
    /// <param name="currency">Currency of the allowance or surcharge</param>
    /// <param name="basisAmount">Basis aount for the allowance or surcharge, typicalls the net amount of the item</param>
    /// <param name="actualAmount">The actual allowance or surcharge amount</param>
    /// <param name="chargePercentage">Actual allowance or surcharge charge percentage</param>
    /// <param name="reason">Reason for the allowance or surcharge</param>
    /// <param name="reasonCode">Reason code for the allowance or surcharge</param>
    function AddTradeAllowance (currency: TZUGFeRDCurrencyCodes;
      basisAmount: ZUGFeRDNullable<Currency>; actualAmount: Currency;
      reason: string; reasonCode : IZUGFeRDNullableParam<TZUGFeRDAllowanceReasonCodes> = Nil): TZUGFeRDTradeLineItem; overload;

    /// <summary>
    /// As an allowance or charge on item level, attaching it to the corresponding item.
    /// </summary>
    /// <param name="currency">Currency of the allowance or surcharge</param>
    /// <param name="basisAmount">Basis aount for the allowance or surcharge, typicalls the net amount of the item</param>
    /// <param name="actualAmount">The actual allowance or surcharge amount</param>
    /// <param name="chargePercentage">Actual allowance or surcharge charge percentage</param>
    /// <param name="reason">Reason for the allowance or surcharge</param>
    /// <param name="reasonCode">Reason code for the allowance or surcharge</param>
    function AddTradeCharge (currency: TZUGFeRDCurrencyCodes;
      basisAmount: ZUGFeRDNullable<Currency>; actualAmount: Currency;
      chargePercentage: Currency;
      reason: string; reasonCode : IZUGFeRDNullableParam<TZUGFeRDChargeReasonCodes> = Nil): TZUGFeRDTradeLineItem; overload;

  /// <summary>
    /// As an allowance or charge on item level, attaching it to the corresponding item.
    /// </summary>
    /// <param name="currency">Currency of the allowance or surcharge</param>
    /// <param name="basisAmount">Basis aount for the allowance or surcharge, typicalls the net amount of the item</param>
    /// <param name="actualAmount">The actual allowance or surcharge amount</param>
    /// <param name="reason">Reason for the allowance or surcharge</param>
    /// <param name="reasonCode">Reason code for the allowance or surcharge</param>
    function AddTradeCharge (currency: TZUGFeRDCurrencyCodes;
      basisAmount: ZUGFeRDNullable<Currency>; actualAmount: Currency;
      reason: string; reasonCode : IZUGFeRDNullableParam<TZUGFeRDChargeReasonCodes> = Nil): TZUGFeRDTradeLineItem; overload;

    /// <summary>
    /// As an allowance or charge on total item price, attaching it to the corresponding item.
    /// </summary>
    /// <param name="isDiscount">Marks if its an allowance (true) or charge (false). Please note that the xml will present inversed values</param>
    /// <param name="currency">Currency of the allowance or surcharge</param>
    /// <param name="basisAmount">Basis aount for the allowance or surcharge, typicalls the net amount of the item</param>
    /// <param name="actualAmount">The actual allowance or surcharge amount</param>
    /// <param name="reason">Reason for the allowance or surcharge</param>
    procedure AddSpecifiedTradeAllowanceCharge (isDiscount: Boolean; currency: TZUGFeRDCurrencyCodes;
      basisAmount: ZUGFeRDNullable<Currency>; actualAmount: Currency;
      reason: string;
      reasonCode: IZUGFeRDNullableParam<TZUGFeRDAllowanceReasonCodes> = Nil);

    /// <summary>
    /// As an allowance or charge on total item price, attaching it to the corresponding item.
    /// </summary>
    /// <param name="isDiscount">Marks if its an allowance (true) or charge (false). Please note that the xml will present inversed values</param>
    /// <param name="currency">Currency of the allowance or surcharge</param>
    /// <param name="basisAmount">Basis aount for the allowance or surcharge, typicalls the net amount of the item</param>
    /// <param name="actualAmount">The actual allowance or surcharge amount</param>
    /// <param name="reason">Reason for the allowance or surcharge</param>
    procedure AddSpecifiedTradeAllowance (currency: TZUGFeRDCurrencyCodes;
      basisAmount: ZUGFeRDNullable<Currency>; actualAmount: Currency;
      reason: string; reasonCode : IZUGFeRDNullableParam<TZUGFeRDAllowanceReasonCodes> = Nil); overload;

    procedure AddSpecifiedTradeAllowance (currency: TZUGFeRDCurrencyCodes;
      basisAmount: ZUGFeRDNullable<Currency>;
      actualAmount: Currency;
      chargePercentage: ZUGFeRDNullable<Currency>;
      reason: string; reasonCode : IZUGFeRDNullableParam<TZUGFeRDAllowanceReasonCodes> = Nil); overload;


    /// <summary>
    /// As an allowance or charge on total item price, attaching it to the corresponding item.
    /// </summary>
    /// <param name="currency">Currency of the allowance or surcharge</param>
    /// <param name="basisAmount">Basis aount for the allowance or surcharge, typicalls the net amount of the item</param>
    /// <param name="actualAmount">The actual allowance or surcharge amount</param>
    /// <param name="reason">Reason for the allowance or surcharge</param>
    procedure AddSpecifiedTradeCharge (currency: TZUGFeRDCurrencyCodes;
      basisAmount: ZUGFeRDNullable<Currency>; actualAmount: Currency;
      reason: string; reasonCode : IZUGFeRDNullableParam<TZUGFeRDChargeReasonCodes> = Nil); overload;

    procedure AddSpecifiedTradeCharge (currency: TZUGFeRDCurrencyCodes;
      basisAmount: ZUGFeRDNullable<Currency>;
      actualAmount: Currency;
      chargePercentage: ZUGFeRDNullable<Currency>;
      reason: string; reasonCode : IZUGFeRDNullableParam<TZUGFeRDChargeReasonCodes> = Nil); overload;

    /// <summary>
    /// Returns all specified trade allowances for the trade line item
    /// </summary>
    function GetSpecifiedTradeAllowances: TArray<TZUGFeRDTradeAllowance>;

    /// <summary>
    /// Returns all specified trade charges for the trade line item
    /// </summary>
    function GetSpecifiedTradeCharges: TArray<TZUGFeRDTradeCharge>;

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

    function SetContractReferencedDocument(contractReferencedId: string; contractReferencedDate: IZUGFeRDNullableParam<TDateTime>): TZUGFeRDTradeLineItem;

    /// <summary>
    /// The value given here refers to the superior line. In this way, a hierarchy tree of invoice items can be mapped.
    ///
    /// BT-X-304
    /// </summary>
    function SetParentLineId(parentLineId: string): TZUGFeRDTradeLineItem;

    /// <summary>
    /// Sets the status code and reason code for this trade line item
    /// </summary>
    /// <param name="lineStatusCode">The status code for this line</param>
    /// <param name="lineStatusReasonCode">The reason code explaining the status</param>
    function SetLineStatus(lineStatusCode: TZUGFeRDLineStatusCodes; lineStatusReasonCode: TZUGFeRDLineStatusReasonCodes): TZUGFeRDTradeLineItem;

    /// <summary>
    /// Sets the delivery note reference information for this trade line item. BG-X-83
    /// Only available in Extended profile.
    /// </summary>
    /// <param name="deliveryNoteId">The identifier of the delivery note. BT-X-92</param>
    /// <param name="deliveryNoteDate">The date of the delivery note. BT-X-94</param>
    /// <param name="deliveryNoteReferencedLineId">The identifier of the delivery note item. BT-X-93</param>
    function SetDeliveryNoteReferencedDocument(deliveryNoteId: string; deliveryNoteDate: IZUGFeRDNullableParam<TDateTime>): TZUGFeRDTradeLineItem;

    /// <summary>
		/// Please note that XRechnung/ FacturX allows a maximum of one such reference
    /// and will only output the referenced order line id
    /// but not issuer assigned id and date
		/// </summary>
    function SetOrderReferencedDocument(orderReferencedId: string; orderReferencedDate: IZUGFeRDNullableParam<TDateTime>): TZUGFeRDTradeLineItem;
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
    /// An identifier, assigned by the Industry, for the item.
    /// </summary>
    property IndustryAssignedID: string read FIndustryAssignedID write FIndustryAssignedID;

    /// <summary>
    /// A unique model identifier for this item.
    /// </summary>
    property ModelID: string read FModelID write FModelID;

    /// <summary>
    /// A batch identifier for this item.
    /// </summary>
    property BatchID: string read FBatchID write FBatchID;

    /// <summary>
    /// The brand name, expressed as text, for this item.
    /// </summary>
    property BrandName: string read FBrandName write FBrandName;

    /// <summary>
    /// A group of business terms providing information about properties of the goods and services ordered.
    /// </summary>
    property Modelname: string read FModelName write FModelName;

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
    property BilledQuantity: Currency read FBilledQuantity write FBilledQuantity;

    /// <summary>
    /// No charge quantity  BT-X-46
    /// </summary>
    property ChargeFreeQuantity: ZUGFeRDNullable<Currency> read FChargeFreeQuantity write FChargeFreeQuantity;

    /// <summary>
    /// Package quantity
    /// </summary>
    property PackageQuantity: ZUGFeRDNullable<Currency> read FPackageQuantity write FPackageQuantity;

    /// <summary>
    /// Invoice line net amount including (!) trade allowance charges for the line item
    /// BT-131
    /// </summary>
    property LineTotalAmount: ZUGFeRDNullable<Currency> read FLineTotalAmount write FLineTotalAmount;

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
    property TaxCategoryCode: ZUGFeRDNullable<TZUGFeRDTaxCategoryCodes> read FTaxCategoryCode write FTaxCategoryCode;

    /// <summary>
    /// Tax rate
    /// </summary>
    property TaxPercent: Currency read FTaxPercent write FTaxPercent;

    /// <summary>
    /// Tax type
    /// </summary>
    property TaxType: ZUGFeRDNullable<TZUGFeRDTaxTypes> read FTaxType write FTaxType;

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
    property NetQuantity: ZUGFeRDNullable<Currency> read FNetQuantity write FNetQuantity;

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
    property GrossQuantity: ZUGFeRDNullable<Currency> read FGrossQuantity write FGrossQuantity;

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
    property UnitCode: ZUGFeRDNullable<TZUGFeRDQuantityCodes> read FUnitCode write FUnitCode;

    /// <summary>
    /// Charge Free Quantity Unit Code
    /// </summary>
    property ChargeFreeUnitCode: ZUGFeRDNullable<TZUGFeRDQuantityCodes> read FChargeFreeUnitCode write FChargeFreeUnitCode;

    /// <summary>
    /// Package Quantity Unit Code
    /// </summary>
    property PackageUnitCode: ZUGFeRDNullable<TZUGFeRDQuantityCodes> read FPackageUnitCode write FPackageUnitCode;

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
    property TradeAllowanceCharges: TObjectList<TZUGFeRDAbstractTradeAllowanceCharge> read FTradeAllowanceCharges write FTradeAllowanceCharges;

    /// <summary>
    /// A group of business terms providing information about the applicable surcharges or discounts on the total amount of the invoice item
    /// </summary>
    property SpecifiedTradeAllowanceCharges : TObjectList<TZUGFeRDAbstractTradeAllowanceCharge> read FSpecifiedTradeAllowanceCharges write FSpecifiedTradeAllowanceCharges;


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

constructor TZUGFeRDTradeLineItem.Create (LineId: String);
begin
  inherited Create;
  FGlobalID := TZUGFeRDGlobalID.Create;
  FNetUnitPrice:= 0.0;
  FGrossUnitPrice:= 0.0;
  FAssociatedDocument:= TZUGFeRDAssociatedDocument.Create(LineId);
  FTaxType:= TZUGFeRDTaxTypes.VAT; // Default Value
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
  FTradeAllowanceCharges:= TObjectList<TZUGFeRDAbstractTradeAllowanceCharge>.Create;
  FSpecifiedTradeAllowanceCharges := TObjectList<TZUGFeRDAbstractTradeAllowanceCharge>.Create;
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
  basisAmount: ZUGFeRDNullable<Currency>; actualAmount: Currency;
  reason: string;
  reasonCode: IZUGFeRDNullableParam<TZUGFeRDAllowanceReasonCodes> = Nil);
begin
  if isDiscount then
    AddTradeAllowance(currency, basisAmount, actualAmount, reason, reasonCode)
  else
    AddTradeCharge(currency, basisAmount, actualAmount, reason, nil)
end;

function TZUGFeRDTradeLineItem.AddTradeAllowance (
  currency: TZUGFeRDCurrencyCodes;
  basisAmount: ZUGFeRDNullable<Currency>; actualAmount: Currency;
  chargePercentage: Currency;
  reason: string; reasonCode: IZUGFeRDNullableParam<TZUGFeRDAllowanceReasonCodes> = Nil): TZUGFeRDTradeLineItem;
var
  TradeAllowance: TZUGFeRDTradeAllowance;
begin
  TradeAllowance:= TZUGFeRDTradeAllowance.Create;
  TradeAllowance.Currency := currency;
  TradeAllowance.ActualAmount := actualAmount;
  TradeAllowance.BasisAmount := basisAmount;
  TradeAllowance.ChargePercentage := chargePercentage;
  TradeAllowance.ReasonCode := reasonCode;
  TradeAllowance.Reason := reason;
  FTradeAllowanceCharges.Add(TradeAllowance);
  Result:= self;
end;

function TZUGFeRDTradeLineItem.AddTradeAllowance (
  currency: TZUGFeRDCurrencyCodes;
  basisAmount: ZUGFeRDNullable<Currency>; actualAmount: Currency;
  reason: string; reasonCode: IZUGFeRDNullableParam<TZUGFeRDAllowanceReasonCodes> = Nil): TZUGFeRDTradeLineItem;
var
  TradeAllowance: TZUGFeRDTradeAllowance;
begin
  TradeAllowance:= TZUGFeRDTradeAllowance.Create;
  TradeAllowance.Currency := currency;
  TradeAllowance.ActualAmount := actualAmount;
  TradeAllowance.BasisAmount := basisAmount;
  TradeAllowance.ReasonCode := reasonCode;
  TradeAllowance.Reason := reason;
  FTradeAllowanceCharges.Add(TradeAllowance);
  Result:= self;
end;

function TZUGFeRDTradeLineItem.AddTradeCharge (
  currency: TZUGFeRDCurrencyCodes;
  basisAmount: ZUGFeRDNullable<Currency>; actualAmount: Currency;
  chargePercentage: Currency;
  reason: string; reasonCode: IZUGFeRDNullableParam<TZUGFeRDChargeReasonCodes> = Nil): TZUGFeRDTradeLineItem;
var
  TradeCharge: TZUGFeRDTradeCharge;
begin
  TradeCharge:= TZUGFeRDTradeCharge.Create;
  TradeCharge.Currency := currency;
  TradeCharge.ActualAmount := actualAmount;
  TradeCharge.BasisAmount := basisAmount;
  TradeCharge.ChargePercentage := chargePercentage;
  TradeCharge.ReasonCode := reasonCode;
  TradeCharge.Reason := reason;
  FTradeAllowanceCharges.Add(TradeCharge);
  Result:= self;
end;

function TZUGFeRDTradeLineItem.AddTradeCharge (
  currency: TZUGFeRDCurrencyCodes;
  basisAmount: ZUGFeRDNullable<Currency>; actualAmount: Currency;
  reason: string; reasonCode: IZUGFeRDNullableParam<TZUGFeRDChargeReasonCodes> = Nil): TZUGFeRDTradeLineItem;
var
  TradeCharge: TZUGFeRDTradeCharge;
begin
  TradeCharge:= TZUGFeRDTradeCharge.Create;
  TradeCharge.Currency := currency;
  TradeCharge.ActualAmount := actualAmount;
  TradeCharge.BasisAmount := basisAmount;
  TradeCharge.ReasonCode := reasonCode;
  TradeCharge.Reason := reason;
  FTradeAllowanceCharges.Add(TradeCharge);
  Result:= self;
end;

function TZUGFeRDTradeLineItem.SetParentLineId(parentLineId: string): TZUGFeRDTradeLineItem;
begin
  AssociatedDocument.ParentLineID:= parentLineId;
  Result:= self
end;

function TZUGFeRDTradeLineItem.SetLineStatus(lineStatusCode: TZUGFeRDLineStatusCodes; lineStatusReasonCode: TZUGFeRDLineStatusReasonCodes): TZUGFeRDTradeLineItem;
begin
  AssociatedDocument.LineStatusCode:= lineStatusCode;
  AssociatedDocument.LineStatusReasonCode:= lineStatusReasonCode;
  Result:= Self
end;

function TZUGFeRDTradeLineItem.SetDeliveryNoteReferencedDocument(deliveryNoteId: string; deliveryNoteDate: IZUGFeRDNullableParam<TDateTime>): TZUGFeRDTradeLineItem;
begin
  if FDeliveryNoteReferencedDocument = nil then
    FDeliveryNoteReferencedDocument := TZUGFeRDDeliveryNoteReferencedDocument.Create;
  with FDeliveryNoteReferencedDocument do
  begin
    ID := deliveryNoteId;
    IssueDateTime:= deliveryNoteDate;
  end;
  Result:= self
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

procedure TZUGFeRDTradeLineItem.AddIncludedReferencedProduct(Name: String; UnitQuantity: ZUGFeRDNullable<Currency>; UnitCode: TZUGFeRDQuantityCodes);
begin
  FIncludedReferencedProducts.Add(TZUGFeRDIncludedReferencedProduct.Create);
  FIncludedReferencedProducts[FIncludedReferencedProducts.Count - 1].Name := Name;
  FIncludedReferencedProducts[FIncludedReferencedProducts.Count - 1].UnitQuantity := UnitQuantity;
  FIncludedReferencedProducts[FIncludedReferencedProducts.Count - 1].UnitCode := UnitCode;
end;

function TZUGFeRDTradeLineItem.SetOrderReferencedDocument(orderReferencedId: string; orderReferencedDate: IZUGFeRDNullableParam<TDateTime>): TZUGFeRDTradeLineItem;
begin
  if FBuyerOrderReferencedDocument = nil then
    FBuyerOrderReferencedDocument := TZUGFeRDBuyerOrderReferencedDocument.Create;
  with FBuyerOrderReferencedDocument do
  begin
    ID := orderReferencedId;
    IssueDateTime:= orderReferencedDate;
  end;
  Result:= self;
end;

function TZUGFeRDTradeLineItem.SetContractReferencedDocument(contractReferencedId: string; contractReferencedDate: IZUGFeRDNullableParam<TDateTime>): TZUGFeRDTradeLineItem;
begin
  if FContractReferencedDocument = nil then
    FContractReferencedDocument := ContractReferencedDocument.Create;
  with FContractReferencedDocument do
  begin
    ID := contractReferencedId;
    IssueDateTime:= contractReferencedDate;
  end;
  Result:= self
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
  isDiscount: Boolean; currency: TZUGFeRDCurrencyCodes;
  basisAmount: ZUGFeRDNullable<Currency>; actualAmount: Currency;
  reason: string;
  reasonCode: IZUGFeRDNullableParam<TZUGFeRDAllowanceReasonCodes> = Nil);
begin
  if isDiscount then
    AddSpecifiedTradeAllowance(currency, basisAmount, actualAmount, reason, reasonCode)
  else
    AddSpecifiedTradeCharge(currency, basisAmount, actualAmount, reason)
end;

procedure TZUGFeRDTradeLineItem.AddSpecifiedTradeAllowance (
  currency: TZUGFeRDCurrencyCodes;
  basisAmount: ZUGFeRDNullable<Currency>; actualAmount: Currency;
  reason: string; reasonCode: IZUGFeRDNullableParam<TZUGFeRDAllowanceReasonCodes> = Nil);
var
  TradeAllowance: TZUGFeRDTradeAllowance;
begin
  TradeAllowance:= TZUGFeRDTradeAllowance.Create;
  TradeAllowance.ChargeIndicator := false;
  TradeAllowance.Currency := currency;
  TradeAllowance.ActualAmount := actualAmount;
  TradeAllowance.BasisAmount := basisAmount;
  TradeAllowance.ReasonCode := reasonCode;
  TradeAllowance.Reason := reason;
  FSpecifiedTradeAllowanceCharges.Add(TradeAllowance);
end;

procedure TZUGFeRDTradeLineItem.AddSpecifiedTradeAllowance (
  currency: TZUGFeRDCurrencyCodes;
  basisAmount: ZUGFeRDNullable<Currency>;
  actualAmount: Currency;
  chargePercentage: ZUGFeRDNullable<Currency>;
  reason: string; reasonCode: IZUGFeRDNullableParam<TZUGFeRDAllowanceReasonCodes> = Nil);
var
  TradeAllowance: TZUGFeRDTradeAllowance;
begin
  TradeAllowance:= TZUGFeRDTradeAllowance.Create;
  TradeAllowance.ChargeIndicator := false;
  TradeAllowance.Currency := currency;
  TradeAllowance.ActualAmount := actualAmount;
  TradeAllowance.BasisAmount := basisAmount;
  TradeAllowance.ReasonCode := reasonCode;
  TradeAllowance.Reason := reason;
  TradeAllowance.chargePercentage:= chargePercentage;
  FSpecifiedTradeAllowanceCharges.Add(TradeAllowance);
end;

procedure TZUGFeRDTradeLineItem.AddSpecifiedTradeCharge (
  currency: TZUGFeRDCurrencyCodes;
  basisAmount: ZUGFeRDNullable<Currency>; actualAmount: Currency;
  reason: string; reasonCode: IZUGFeRDNullableParam<TZUGFeRDChargeReasonCodes> = Nil);
var
  TradeCharge: TZUGFeRDTradeCharge;
begin
  TradeCharge:= TZUGFeRDTradeCharge.Create;
  TradeCharge.ChargeIndicator := true;
  TradeCharge.Currency := currency;
  TradeCharge.ActualAmount := actualAmount;
  TradeCharge.BasisAmount := basisAmount;
  TradeCharge.ReasonCode := reasonCode;
  TradeCharge.Reason := reason;
  FSpecifiedTradeAllowanceCharges.Add(TradeCharge);
end;

procedure TZUGFeRDTradeLineItem.AddSpecifiedTradeCharge (
  currency: TZUGFeRDCurrencyCodes;
  basisAmount: ZUGFeRDNullable<Currency>;
  actualAmount: Currency;
  chargePercentage: ZUGFeRDNullable<Currency>;
  reason: string; reasonCode: IZUGFeRDNullableParam<TZUGFeRDChargeReasonCodes> = Nil);
var
  TradeCharge: TZUGFeRDTradeCharge;
begin
  TradeCharge:= TZUGFeRDTradeCharge.Create;
  TradeCharge.ChargeIndicator := true;
  TradeCharge.Currency := currency;
  TradeCharge.ActualAmount := actualAmount;
  TradeCharge.BasisAmount := basisAmount;
  TradeCharge.ReasonCode := reasonCode;
  TradeCharge.Reason := reason;
  TradeCharge.chargePercentage:= chargePercentage;
  FSpecifiedTradeAllowanceCharges.Add(TradeCharge);
end;

function TZUGFeRDTradeLineItem.GetSpecifiedTradeAllowances: TArray<TZUGFeRDTradeAllowance>;
// over TArray may be iterated and it is freed automatically
var
  Count: Integer;
begin
  SetLength(Result, FSpecifiedTradeAllowanceCharges.Count);
  Count:= 0;
  for var t in FSpecifiedTradeAllowanceCharges do
  if not(t.ChargeIndicator) then
  begin
    Result[Count]:= t as TZUGFeRDTradeAllowance;
    Inc(Count)
  end;
  SetLength(Result, Count)
end;

function TZUGFeRDTradeLineItem.GetSpecifiedTradeCharges: TArray<TZUGFeRDTradeCharge>;
// over TArray may be iterated and it is freed automatically
var
  Count: Integer;
begin
  SetLength(Result, FSpecifiedTradeAllowanceCharges.Count);
  Count:= 0;
  for var t in FSpecifiedTradeAllowanceCharges do
  if t.ChargeIndicator then
  begin
    Result[Count]:= t as TZUGFeRDTradeCharge;
    Inc(Count)
  end;
  SetLength(Result, Count)
end;

end.
