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

unit intf.ZUGFeRDAdvancePayment;

interface

uses
  System.SysUtils, System.Generics.Collections,
  intf.ZUGFeRDTax,
  intf.ZUGFeRDInvoiceReferencedDocument,
  intf.ZUGFeRDInvoiceTypes,
  intf.ZUGFeRDHelper;

type
  /// <summary>
  /// Detailangaben zu einer Vorauszahlung / Anzahlung, BG-X-45.
  ///
  /// Nur im EXTENDED-Profil zulässig (ram:SpecifiedAdvancePayment, 0..n).
  /// </summary>
  TZUGFeRDAdvancePayment = class
  private
    FPaidAmount: ZUGFeRDNullable<Currency>;
    FFormattedReceivedDateTime: ZUGFeRDNullable<TDateTime>;
    FIncludedTradeTaxes: TObjectList<TZUGFeRDTax>;
    FInvoiceSpecifiedReferencedDocument: TZUGFeRDInvoiceReferencedDocument;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Vorauszahlungsbetrag, BT-X-45-00. Pflichtfeld des Elements.
    /// </summary>
    property PaidAmount: ZUGFeRDNullable<Currency> read FPaidAmount write FPaidAmount;

    /// <summary>
    /// Datum des Zahlungseingangs, BT-X-46.
    /// </summary>
    property FormattedReceivedDateTime: ZUGFeRDNullable<TDateTime> read FFormattedReceivedDateTime write FFormattedReceivedDateTime;

    /// <summary>
    /// In der Vorauszahlung enthaltene Steuern, mindestens ein Eintrag.
    /// </summary>
    property IncludedTradeTaxes: TObjectList<TZUGFeRDTax> read FIncludedTradeTaxes;

    /// <summary>
    /// Referenz auf die zugehörige Vorauszahlungsrechnung, BT-X-47.
    /// nil, solange keine Referenz gesetzt wurde.
    /// </summary>
    property InvoiceSpecifiedReferencedDocument: TZUGFeRDInvoiceReferencedDocument read FInvoiceSpecifiedReferencedDocument write FInvoiceSpecifiedReferencedDocument;

    /// <summary>
    /// Legt die Referenz auf die Vorauszahlungsrechnung an bzw. aktualisiert sie.
    /// </summary>
    procedure SetInvoiceReferencedDocument(const id: string;
      const issueDateTime: IZUGFeRDNullableParam<TDateTime> = nil;
      const typeCode: IZUGFeRDNullableParam<TZUGFeRDInvoiceType> = nil);

    /// <summary>
    /// Fügt eine enthaltene Steuer hinzu und liefert sie zurück.
    /// </summary>
    function AddIncludedTradeTax(const tax: TZUGFeRDTax): TZUGFeRDTax;
  end;

implementation

{ TZUGFeRDAdvancePayment }

constructor TZUGFeRDAdvancePayment.Create;
begin
  inherited Create;
  FIncludedTradeTaxes := TObjectList<TZUGFeRDTax>.Create;
  FInvoiceSpecifiedReferencedDocument := nil;
end;

destructor TZUGFeRDAdvancePayment.Destroy;
begin
  if Assigned(FInvoiceSpecifiedReferencedDocument) then
  begin
    FInvoiceSpecifiedReferencedDocument.Free;
    FInvoiceSpecifiedReferencedDocument := nil;
  end;
  if Assigned(FIncludedTradeTaxes) then
  begin
    FIncludedTradeTaxes.Free;
    FIncludedTradeTaxes := nil;
  end;
  inherited;
end;

procedure TZUGFeRDAdvancePayment.SetInvoiceReferencedDocument(const id: string;
  const issueDateTime: IZUGFeRDNullableParam<TDateTime>;
  const typeCode: IZUGFeRDNullableParam<TZUGFeRDInvoiceType>);
begin
  if not Assigned(FInvoiceSpecifiedReferencedDocument) then
    FInvoiceSpecifiedReferencedDocument := TZUGFeRDInvoiceReferencedDocument.Create;
  FInvoiceSpecifiedReferencedDocument.ID := id;
  FInvoiceSpecifiedReferencedDocument.IssueDateTime := issueDateTime;
  FInvoiceSpecifiedReferencedDocument.TypeCode := typeCode;
end;

function TZUGFeRDAdvancePayment.AddIncludedTradeTax(const tax: TZUGFeRDTax): TZUGFeRDTax;
begin
  FIncludedTradeTaxes.Add(tax);
  Result := tax;
end;

end.
