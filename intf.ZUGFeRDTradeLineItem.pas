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
  intf.ZUGFeRDGlobalID
  ;

type
  /// <summary>
  /// Struktur, die Informationen zu einem Artikel enthält
  /// </summary>
  TZUGFeRDTradeLineItem = class
  private
    procedure AddAdditionalReferencedDocument(id: string;
      date: Nullable<TDateTime>; code: ReferenceTypeCodes);
    procedure AddReceivableSpecifiedTradeAccountingAccount(
      AccountID: string);
    procedure AddTradeAllowanceCharge(isDiscount: Boolean;
      currency: CurrencyCodes; basisAmount, actualAmount: Decimal;
      reason: string);
    procedure SetContractReferencedDocument(contractReferencedId: string;
      contractReferencedDate: Nullable<TDateTime>);
    procedure SetDeliveryNoteReferencedDocument(deliveryNoteId: string;
      deliveryNoteDate: Nullable<TDateTime>);
    procedure SetOrderReferencedDocument(orderReferencedId: string;
      orderReferencedDate: Nullable<TDateTime>);
  public
    /// <summary>
    /// Eindeutige Bezeichnung für die betreffende Rechnungsposition
    /// </summary>
    LineID: string;
    /// <summary>
    /// Kennung eines Artikels gemäß einem registrierten Schema
    /// </summary>
    GlobalID: TZUGFeRDGlobalID;
    /// <summary>
    /// Artikelnummer des Verkäufers
    /// </summary>
    SellerAssignedID: string;
    /// <summary>
    /// Artikelnummer des Käufers
    /// </summary>
    BuyerAssignedID: string;
    /// <summary>
    /// Artikelname
    /// </summary>
    Name: string;
    /// <summary>
    /// Artikelbeschreibung
    /// </summary>
    Description: string;
    /// <summary>
    /// Menge, enthalten
    /// </summary>
    UnitQuantity: TZUGFeRDNullable<Double>;
    /// <summary>
    /// Menge, die in Rechnung gestellt wird
    /// </summary>
    BilledQuantity: Double;
    /// <summary>
    /// Nettobetrag der Rechnungsposition
    /// </summary>
    LineTotalAmount: TZUGFeRDNullable<Double>;
    /// <summary>
    /// Beginn des für die Rechnungsposition maßgeblichen Abrechnungszeitraums
    /// </summary>
    BillingPeriodStart: TZUGFeRDNullable<TDateTime>;
    /// <summary>
    /// Ende des für die Rechnungsposition maßgeblichen Abrechnungszeitraums
    /// </summary>
    BillingPeriodEnd: TZUGFeRDNullable<TDateTime>;
    /// <summary>
    /// Steuerkategoriecode
    /// </summary>
    TaxCategoryCode: TaxCategoryCodes;
    /// <summary>
    /// Steuersatz
    /// </summary>
    TaxPercent: Double;
    /// <summary>
    /// Steuertyp
    /// </summary>
    TaxType: TaxTypes;
    /// <summary>
    /// Netto-Einzelpreis
    /// </summary>
    NetUnitPrice: TZUGFeRDNullable<Double>;
    /// <summary>
    /// Brutto-Einzelpreis
    /// </summary>
    GrossUnitPrice: TZUGFeRDNullable<Double>;
    /// <summary>
    /// Einheit der Preisbasismenge
    /// </summary>
    UnitCode: QuantityCodes;
    /// <summary>
    /// Dokument, das mit der Rechnungsposition verknüpft ist
    /// </summary>
    AssociatedDocument: AssociatedDocument;
    /// <summary>
    /// Tatsächliches Lieferdatum
    /// </summary>
    ActualDeliveryDate: TZUGFeRDNullable<TDateTime>;
    /// <summary>
    /// Details zur zugehörigen Bestellung
    /// </summary>
    BuyerOrderReferencedDocument: BuyerOrderReferencedDocument;
    /// <summary>
    /// Detaillierte Informationen zur entsprechenden Lieferscheinreferenz
    /// </summary>
    DeliveryNoteReferencedDocument: DeliveryNoteReferencedDocument;
    /// <summary>
    /// Details zum zugehörigen Vertrag
    /// </summary>
    ContractReferencedDocument: ContractReferencedDocument;
    /// <summary>
    /// Liste der zusätzlichen Referenzdokumente
    /// </summary>
    AdditionalReferencedDocuments: TList<AdditionalReferencedDocument>;
    /// <summary>
    /// Eine Gruppe von Geschäftsbedingungen, die Informationen über anwendbare Zuschläge oder Rabatte auf den
    /// Gesamtbetrag der Rechnung enthält
    /// </summary>
    TradeAllowanceCharges: TList<TradeAllowanceCharge>;
    /// <summary>
    /// Detaillierte Informationen zur buchhalterischen Referenz
    /// </summary>
    ReceivableSpecifiedTradeAccountingAccounts: TList<ReceivableSpecifiedTradeAccountingAccount>;
    /// <summary>
    /// Liste der anwendbaren Produktmerkmale
    /// </summary>
    ApplicableProductCharacteristics: TList<ApplicableProductCharacteristic>;

    /// <summary>
    /// Initialisiert ein neues, leeres Handelspositionsobjekt
    /// </summary>
    constructor Create;
    destructor Destroy; override;
  end;


implementation

/// <summary>
/// Initialisiert ein neues, leeres Handelspositionsobjekt
/// </summary>
constructor TZUGFeRDTradeLineItem.Create;
begin
  inherited;
  GlobalID := TZUGFeRDGlobalID.Create;
  UnitQuantity := TZUGFeRDNullable<Double>.Create;

    LineTotalAmount: Nullable<decimal>;
    BillingPeriodStart: Nullable<TDateTime>;
    BillingPeriodEnd: Nullable<TDateTime>;
  NetUnitPrice := TZUGFeRDNullable<Double>.CreateWithValue(0.0);
  GrossUnitPrice:= TZUGFeRDNullable<Double>.CreateWithValue(0.0);
    ActualDeliveryDate: TZUGFeRDNullable<TDateTime>;

  TradeAllowanceCharges := TList<TradeAllowanceCharge>.Create;
  AdditionalReferencedDocuments := TList<AdditionalReferencedDocument>.Create;
  ReceivableSpecifiedTradeAccountingAccounts := TList<ReceivableSpecifiedTradeAccountingAccount>.Create;
  ApplicableProductCharacteristics := TList<ApplicableProductCharacteristic>.Create;
end;

destructor TZUGFeRDTradeLineItem.Destroy;
begin

  inherited;
end;

/// <summary>
/// Als Nachlass oder Aufschlag auf Positionsebene, der der entsprechenden Position angehängt wird.
/// </summary>
/// <param name="isDiscount">Gibt an, ob es sich um einen Nachlass (true) oder Aufschlag (false) handelt. Bitte beachten Sie, dass die XML-Werte umgekehrt dargestellt werden</param>
/// <param name="currency">Währung des Nachlasses oder Aufschlags</param>
/// <param name="basisAmount">Basisbetrag für den Nachlass oder Aufschlag, in der Regel der Nettobetrag des Artikels</param>
/// <param name="actualAmount">Der tatsächliche Nachlass- oder Aufschlagsbetrag</param>
/// <param name="reason">Grund für den Nachlass oder Aufschlag</param>
procedure TZUGFeRDTradeLineItem.AddTradeAllowanceCharge(isDiscount: Boolean; currency: CurrencyCodes; basisAmount: Decimal; actualAmount: Decimal; reason: string);
begin
  TradeAllowanceCharges.Add(TradeAllowanceCharge.Create);
  with TradeAllowanceCharges[TradeAllowanceCharges.Count - 1] do
  begin
    ChargeIndicator := not isDiscount;
    Currency := currency;
    ActualAmount := actualAmount;
    BasisAmount := basisAmount;
    Reason := reason;
  end;
end;

/// <summary>
/// Setzt das Referenzdokument für den Lieferschein.
/// </summary>
/// <param name="deliveryNoteId">ID des Lieferscheins</param>
/// <param name="deliveryNoteDate">Datum des Lieferscheins</param>
procedure TZUGFeRDTradeLineItem.SetDeliveryNoteReferencedDocument(deliveryNoteId: string; deliveryNoteDate: Nullable<TDateTime>);
begin
  DeliveryNoteReferencedDocument := DeliveryNoteReferencedDocument.Create;
  with DeliveryNoteReferencedDocument do
  begin
    ID := deliveryNoteId;
    IssueDateTime := deliveryNoteDate;
  end;
end;

/// <summary>
/// Fügt ein zusätzliches Referenzdokument hinzu.
/// </summary>
/// <param name="id">ID des Dokuments</param>
/// <param name="date">Datum des Dokuments</param>
/// <param name="code">Referenztyp des Dokuments</param>
procedure TZUGFeRDTradeLineItem.AddAdditionalReferencedDocument(id: string; date: Nullable<TDateTime> = nil; code: ReferenceTypeCodes = ReferenceTypeCodes.Unknown);
begin
  AdditionalReferencedDocuments.Add(AdditionalReferencedDocument.Create);
  with AdditionalReferencedDocuments[AdditionalReferencedDocuments.Count - 1] do
  begin
    ID := id;
    IssueDateTime := date;
    ReferenceTypeCode := code;
  end;
end;

/// <summary>
/// Setzt das Referenzdokument für die Bestellung.
/// </summary>
/// <param name="orderReferencedId">ID der Bestellung</param>
/// <param name="orderReferencedDate">Datum der Bestellung</param>
procedure TZUGFeRDTradeLineItem.SetOrderReferencedDocument(orderReferencedId: string; orderReferencedDate: Nullable<TDateTime>);
begin
  BuyerOrderReferencedDocument := BuyerOrderReferencedDocument.Create;
  with BuyerOrderReferencedDocument do
  begin
    ID := orderReferencedId;
    IssueDateTime := orderReferencedDate;
  end;
end;

/// <summary>
/// Setzt das Referenzdokument für den Vertrag.
/// </summary>
/// <param name="contractReferencedId">ID des Vertrags</param>
/// <param name="contractReferencedDate">Datum des Vertrags</param>
procedure TZUGFeRDTradeLineItem.SetContractReferencedDocument(contractReferencedId: string; contractReferencedDate: Nullable<TDateTime>);
begin
  ContractReferencedDocument := ContractReferencedDocument.Create;
  with ContractReferencedDocument do
  begin
    ID := contractReferencedId;
    IssueDateTime := contractReferencedDate;
  end;
end;

/// <summary>
/// Fügt ein abrechnungsspezifisches Handelskonto hinzu.
/// </summary>
/// <param name="AccountID">ID des Handelskontos</param>
procedure TZUGFeRDTradeLineItem.AddReceivableSpecifiedTradeAccountingAccount(AccountID: string);
begin
  AddReceivableSpecifiedTradeAccountingAccount(AccountID, AccountingAccountTypeCodes.Unknown);
end;

/// <summary>
/// Fügt ein abrechnungsspezifisches Handelskonto hinzu.
/// </summary>
/// <param name="AccountID">ID des Handelskontos</param>
/// <param name="AccountTypeCode">Typ des Handelskontos</param>
procedure TZUGFeRDTradeLineItem.AddReceivableSpecifiedTradeAccountingAccount(AccountID: string; AccountTypeCode: AccountingAccountTypeCodes);
begin
  ReceivableSpecifiedTradeAccountingAccounts.Add(ReceivableSpecifiedTradeAccountingAccount.Create);
  with ReceivableSpecifiedTradeAccountingAccounts[ReceivableSpecifiedTradeAccountingAccounts.Count - 1] do
  begin
    TradeAccountID := AccountID;
    TradeAccountTypeCode := AccountTypeCode;
  end;
end;

end.
