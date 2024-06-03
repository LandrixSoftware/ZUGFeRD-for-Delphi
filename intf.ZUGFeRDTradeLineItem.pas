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
  System.SysUtils,System.Generics.Collections,System.Generics.Defaults,
  intf.ZUGFeRDHelper,
  intf.ZUGFeRDGlobalID,
  intf.ZUGFeRDReferenceTypeCodes,
  intf.ZUGFeRDCurrencyCodes,
  intf.ZUGFeRDContractReferencedDocument,
  intf.ZUGFeRDReceivableSpecifiedTradeAccountingAccount,
  intf.ZUGFeRDAdditionalReferencedDocument,
  intf.ZUGFeRDApplicableProductCharacteristic,
  intf.ZUGFeRDTradeAllowanceCharge,
  intf.ZUGFeRDTaxTypes,
  intf.ZUGFeRDQuantityCodes,
  intf.ZUGFeRDAssociatedDocument,
  intf.ZUGFeRDTaxCategoryCodes,
  intf.ZUGFeRDDeliveryNoteReferencedDocument,
  intf.ZUGFeRDBuyerOrderReferencedDocument,
  intf.ZUGFeRDAccountingAccountTypeCodes
  ;

type
  /// <summary>
  ///  Structure holding item information
  /// </summary>
  TZUGFeRDTradeLineItem = class
  private
    FBilledQuantity: Double;
    FName: string;
    FContractReferencedDocument: TZUGFeRDContractReferencedDocument;
    FReceivableSpecifiedTradeAccountingAccounts: TObjectList<TZUGFeRDReceivableSpecifiedTradeAccountingAccount>;
    FAdditionalReferencedDocuments: TObjectList<TZUGFeRDAdditionalReferencedDocument>;
    FUnitCode: TZUGFeRDQuantityCodes;
    FBillingPeriodStart: TZUGFeRDNullable<TDateTime>;
    FApplicableProductCharacteristics: TObjectList<TZUGFeRDApplicableProductCharacteristic>;
    FSellerAssignedID: string;
    FTradeAllowanceCharges: TObjectList<TZUGFeRDTradeAllowanceCharge>;
    FTaxPercent: Double;
    FTaxType: TZUGFeRDTaxTypes;
    FBuyerAssignedID: string;
    FActualDeliveryDate: TZUGFeRDNullable<TDateTime>;
    FBillingPeriodEnd: TZUGFeRDNullable<TDateTime>;
    FUnitQuantity: TZUGFeRDNullable<Double>;
    FDescription: string;
    FAssociatedDocument: TZUGFeRDAssociatedDocument;
    FTaxCategoryCode: TZUGFeRDTaxCategoryCodes;
    FNetUnitPrice: TZUGFeRDNullableCurrency;
    FLineTotalAmount: Nullable<Double>; // TZUGFeRDNullable<Double>;
    FDeliveryNoteReferencedDocument: TZUGFeRDDeliveryNoteReferencedDocument;
    FGlobalID: TZUGFeRDGlobalID;
    FBuyerOrderReferencedDocument: TZUGFeRDBuyerOrderReferencedDocument;
    FGrossUnitPrice: TZUGFeRDNullableCurrency;
  public
    /// <summary>
    /// Initialisiert ein neues, leeres Handelspositionsobjekt
    /// </summary>
    constructor Create;
    destructor Destroy; override;

    procedure AddAdditionalReferencedDocument(id: string;
      date: TDateTime = 0; code : TZUGFeRDReferenceTypeCodes = TZUGFeRDReferenceTypeCodes.Unknown);

    procedure AddReceivableSpecifiedTradeAccountingAccount(
      AccountID: string); overload;

    procedure AddReceivableSpecifiedTradeAccountingAccount(
      AccountID: string; AccountTypeCode: TZUGFeRDAccountingAccountTypeCodes); overload;

    /// <summary>
    /// As an allowance or charge on item level, attaching it to the corresponding item.
    /// </summary>
    /// <param name="isDiscount">Marks if its an allowance (true) or charge (false). Please note that the xml will present inversed values</param>
    /// <param name="currency">Currency of the allowance or surcharge</param>
    /// <param name="basisAmount">Basis aount for the allowance or surcharge, typicalls the net amount of the item</param>
    /// <param name="actualAmount">The actual allowance or surcharge amount</param>
    /// <param name="reason">Reason for the allowance or surcharge</param>
    procedure AddTradeAllowanceCharge(isDiscount: Boolean;
      currency: TZUGFeRDCurrencyCodes; basisAmount, actualAmount: double;
      reason: string); overload;

    /// <summary>
    /// As an allowance or charge on item level, attaching it to the corresponding item.
    /// </summary>
    /// <param name="isDiscount">Marks if its an allowance (true) or charge (false). Please note that the xml will present inversed values</param>
    /// <param name="currency">Currency of the allowance or surcharge</param>
    /// <param name="basisAmount">Basis aount for the allowance or surcharge, typicalls the net amount of the item</param>
    /// <param name="actualAmount">The actual allowance or surcharge amount</param>
    /// <param name="chargePercentage">Actual allowance or surcharge charge percentage</param>
    /// <param name="reason">Reason for the allowance or surcharge</param>
    procedure AddTradeAllowanceCharge(isDiscount: Boolean;
      currency: TZUGFeRDCurrencyCodes; basisAmount, actualAmount: double;
      chargePercentage : Currency; reason: string); overload;

    procedure SetContractReferencedDocument(contractReferencedId: string;
      contractReferencedDate: TZUGFeRDNullable<TDateTime>);

    procedure SetDeliveryNoteReferencedDocument(deliveryNoteId: string;
      deliveryNoteDate: TZUGFeRDNullable<TDateTime>);

    procedure SetOrderReferencedDocument(orderReferencedId: string;
      orderReferencedDate: TZUGFeRDNullable<TDateTime>);
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
    /// Included amount
    /// </summary>
    property UnitQuantity: TZUGFeRDNullable<Double> read FUnitQuantity write FUnitQuantity;

    /// <summary>
    /// Invoiced quantity
    /// </summary>
    property BilledQuantity: Double read FBilledQuantity write FBilledQuantity;

    /// <summary>
    /// Invoice line net amount including (!) trade allowance charges for the line item
    /// BT-131
    /// </summary>
    property LineTotalAmount: Nullable<Double> {TZUGFeRDNullable<Double>} read FLineTotalAmount write FLineTotalAmount;

    /// <summary>
    /// Detailed information about the invoicing period
    ///
    /// Invoicing period start date
    /// </summary>
    property BillingPeriodStart: TZUGFeRDNullable<TDateTime> read FBillingPeriodStart write FBillingPeriodStart;

    /// <summary>
    /// Detailed information about the invoicing period
    ///
    /// Invoicing period end date
    /// </summary>
    property BillingPeriodEnd: TZUGFeRDNullable<TDateTime> read FBillingPeriodEnd write FBillingPeriodEnd;

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
    /// net unit price of the item
    /// </summary>
    property NetUnitPrice: TZUGFeRDNullableCurrency read FNetUnitPrice write FNetUnitPrice;

    /// <summary>
    /// gross unit price of the item
    /// </summary>
    property GrossUnitPrice: TZUGFeRDNullableCurrency read FGrossUnitPrice write FGrossUnitPrice;

    /// <summary>
    /// Item Base Quantity Unit Code
    /// </summary>
    property UnitCode: TZUGFeRDQuantityCodes read FUnitCode write FUnitCode;

    /// <summary>
    /// Identifier of the invoice line item
    /// </summary>
    property AssociatedDocument: TZUGFeRDAssociatedDocument read FAssociatedDocument write FAssociatedDocument;

    /// <summary>
    /// Detailed information about the actual Delivery
    /// </summary>
    property ActualDeliveryDate: TZUGFeRDNullable<TDateTime> read FActualDeliveryDate write FActualDeliveryDate;

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
    /// A group of business terms providing information about the applicable surcharges or discounts on the total amount of the invoice
    ///
    /// Now private. Please use GetTradeAllowanceCharges() instead
    /// </summary>
    property TradeAllowanceCharges: TObjectList<TZUGFeRDTradeAllowanceCharge> read FTradeAllowanceCharges write FTradeAllowanceCharges;

    /// <summary>
    /// Detailed information on the accounting reference
    /// </summary>
    property ReceivableSpecifiedTradeAccountingAccounts: TObjectList<TZUGFeRDReceivableSpecifiedTradeAccountingAccount> read FReceivableSpecifiedTradeAccountingAccounts write FReceivableSpecifiedTradeAccountingAccounts;

    /// <summary>
    /// Additional product information
    /// </summary>
    property ApplicableProductCharacteristics: TObjectList<TZUGFeRDApplicableProductCharacteristic> read FApplicableProductCharacteristics write FApplicableProductCharacteristics;
  end;

implementation

constructor TZUGFeRDTradeLineItem.Create;
begin
  inherited;
  FGlobalID := TZUGFeRDGlobalID.Create;
  // UnitQuantity := TZUGFeRDNullable<Double>.Create; // should be unneccesary
  FUnitQuantity:= TZUGFeRDNullable<Double>.Create;
//  FLineTotalAmount:= NTZUGFeRDNullable<Double>.Create;
  FBillingPeriodStart:= TZUGFeRDNullable<TDateTime>.Create;
  FBillingPeriodEnd:= TZUGFeRDNullable<TDateTime>.Create;
  FNetUnitPrice := TZUGFeRDNullableCurrency.CreateWithValue(0.0);
  FGrossUnitPrice:= TZUGFeRDNullableCurrency.CreateWithValue(0.0);
  FAssociatedDocument:= nil;
  FActualDeliveryDate:= TZUGFeRDNullable<TDateTime>.Create;
  FBuyerOrderReferencedDocument:= nil;//TZUGFeRDBuyerOrderReferencedDocument.Create;
  FDeliveryNoteReferencedDocument:= nil;//TZUGFeRDDeliveryNoteReferencedDocument.Create;
  FContractReferencedDocument:= nil;//TZUGFeRDContractReferencedDocument.Create;
  FAdditionalReferencedDocuments:= TObjectList<TZUGFeRDAdditionalReferencedDocument>.Create;
  FTradeAllowanceCharges:= TObjectList<TZUGFeRDTradeAllowanceCharge>.Create;
  FReceivableSpecifiedTradeAccountingAccounts:= TObjectList<TZUGFeRDReceivableSpecifiedTradeAccountingAccount>.Create;
  FApplicableProductCharacteristics := TObjectList<TZUGFeRDApplicableProductCharacteristic>.Create;
end;

destructor TZUGFeRDTradeLineItem.Destroy;
begin
  if Assigned(FGlobalID) then begin FGlobalID.Free; FGlobalID := nil; end;
  if Assigned(FUnitQuantity) then begin FUnitQuantity.Free; FUnitQuantity := nil; end;
//  if Assigned(FLineTotalAmount) then begin FLineTotalAmount.Free; FLineTotalAmount := nil; end;
  if Assigned(FBillingPeriodStart) then begin FBillingPeriodStart.Free; FBillingPeriodStart := nil; end;
  if Assigned(FBillingPeriodEnd) then begin FBillingPeriodEnd.Free; FBillingPeriodEnd := nil; end;
  if Assigned(FNetUnitPrice) then begin FNetUnitPrice.Free; FNetUnitPrice := nil; end;
  if Assigned(FGrossUnitPrice) then begin FGrossUnitPrice.Free; FGrossUnitPrice := nil; end;
  if Assigned(FAssociatedDocument) then begin FAssociatedDocument.Free; FAssociatedDocument := nil; end;
  if Assigned(FActualDeliveryDate) then begin FActualDeliveryDate.Free; FActualDeliveryDate := nil; end;
  if Assigned(FBuyerOrderReferencedDocument) then begin FBuyerOrderReferencedDocument.Free; FBuyerOrderReferencedDocument := nil; end;
  if Assigned(FDeliveryNoteReferencedDocument) then begin FDeliveryNoteReferencedDocument.Free; FDeliveryNoteReferencedDocument := nil; end;
  if Assigned(FContractReferencedDocument) then begin FContractReferencedDocument.Free; FContractReferencedDocument := nil; end;
  if Assigned(FAdditionalReferencedDocuments) then begin FAdditionalReferencedDocuments.Free; FAdditionalReferencedDocuments := nil; end;
  if Assigned(FTradeAllowanceCharges) then begin FTradeAllowanceCharges.Free; FTradeAllowanceCharges := nil; end;
  if Assigned(FReceivableSpecifiedTradeAccountingAccounts) then begin FReceivableSpecifiedTradeAccountingAccounts.Free; FReceivableSpecifiedTradeAccountingAccounts := nil; end;
  if Assigned(FApplicableProductCharacteristics) then begin FApplicableProductCharacteristics.Free; FApplicableProductCharacteristics := nil; end;
  inherited;
end;

procedure TZUGFeRDTradeLineItem.AddTradeAllowanceCharge(
  isDiscount: Boolean; currency: TZUGFeRDCurrencyCodes;
  basisAmount: double; actualAmount: double;
  reason: string);
begin
  FTradeAllowanceCharges.Add(TZUGFeRDTradeAllowanceCharge.Create);
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].ChargeIndicator := not isDiscount;
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].Currency := currency;
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].ActualAmount := actualAmount;
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].BasisAmount := basisAmount;
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].ChargePercentage := 0;
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].Reason := reason;
end;

procedure TZUGFeRDTradeLineItem.AddTradeAllowanceCharge(
  isDiscount: Boolean; currency: TZUGFeRDCurrencyCodes;
  basisAmount: double; actualAmount: double;
  chargePercentage : Currency; reason: string);
begin
  FTradeAllowanceCharges.Add(TZUGFeRDTradeAllowanceCharge.Create);
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].ChargeIndicator := not isDiscount;
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].Currency := currency;
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].ActualAmount := actualAmount;
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].BasisAmount := basisAmount;
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].ChargePercentage := chargePercentage;
  FTradeAllowanceCharges[FTradeAllowanceCharges.Count - 1].Reason := reason;
end;

procedure TZUGFeRDTradeLineItem.SetDeliveryNoteReferencedDocument(
  deliveryNoteId: string; deliveryNoteDate: TZUGFeRDNullable<TDateTime>);
begin
  //FDeliveryNoteReferencedDocument := TZUGFeRDDeliveryNoteReferencedDocument.Create;
  with FDeliveryNoteReferencedDocument do
  begin
    ID := deliveryNoteId;
    if deliveryNoteDate = nil then
      IssueDateTime.ClearValue
    else
    if deliveryNoteDate.HasValue then
      IssueDateTime.SetValue(deliveryNoteDate.GetValue)
    else
      IssueDateTime.ClearValue;
  end;
end;

procedure TZUGFeRDTradeLineItem.AddAdditionalReferencedDocument(
  id: string; date: TDateTime = 0;
  code: TZUGFeRDReferenceTypeCodes = TZUGFeRDReferenceTypeCodes.Unknown);
begin
  FAdditionalReferencedDocuments.Add(TZUGFeRDAdditionalReferencedDocument.Create(true));
  with FAdditionalReferencedDocuments[FAdditionalReferencedDocuments.Count - 1] do
  begin
    ID := id;
    if date <= 0 then
      IssueDateTime.ClearValue
    else
      IssueDateTime.SetValue(date);
    ReferenceTypeCode := code;
  end;
end;

procedure TZUGFeRDTradeLineItem.SetOrderReferencedDocument(
  orderReferencedId: string; orderReferencedDate: TZUGFeRDNullable<TDateTime>);
begin
  //FBuyerOrderReferencedDocument := BuyerOrderReferencedDocument.Create;
  with FBuyerOrderReferencedDocument do
  begin
    ID := orderReferencedId;
    if orderReferencedDate = nil then
      IssueDateTime.ClearValue
    else
    if orderReferencedDate.HasValue then
      IssueDateTime.SetValue(orderReferencedDate.GetValue)
    else
      IssueDateTime.ClearValue;
  end;
end;

procedure TZUGFeRDTradeLineItem.SetContractReferencedDocument(
  contractReferencedId: string; contractReferencedDate: TZUGFeRDNullable<TDateTime>);
begin
  //FContractReferencedDocument := ContractReferencedDocument.Create;
  with FContractReferencedDocument do
  begin
    ID := contractReferencedId;
    if contractReferencedDate = nil then
      IssueDateTime.ClearValue
    else
    if contractReferencedDate.HasValue then
      IssueDateTime.SetValue(contractReferencedDate.GetValue)
    else
      IssueDateTime.ClearValue;
  end;
end;

procedure TZUGFeRDTradeLineItem.AddReceivableSpecifiedTradeAccountingAccount(AccountID: string);
begin
  AddReceivableSpecifiedTradeAccountingAccount(AccountID, TZUGFeRDAccountingAccountTypeCodes.Unknown);
end;

procedure TZUGFeRDTradeLineItem.AddReceivableSpecifiedTradeAccountingAccount(
  AccountID: string; AccountTypeCode: TZUGFeRDAccountingAccountTypeCodes);
begin
  FReceivableSpecifiedTradeAccountingAccounts.Add(TZUGFeRDReceivableSpecifiedTradeAccountingAccount.Create);
  with FReceivableSpecifiedTradeAccountingAccounts[FReceivableSpecifiedTradeAccountingAccounts.Count - 1] do
  begin
    TradeAccountID := AccountID;
    TradeAccountTypeCode := AccountTypeCode;
  end;
end;

end.
