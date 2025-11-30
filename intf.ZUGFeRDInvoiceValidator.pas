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

unit intf.ZUGFeRDInvoiceValidator;

interface

uses
  System.SysUtils,System.TypInfo,System.Classes,
  System.Generics.Collections
  ,intf.ZUGFeRDInvoiceDescriptor
  ,intf.ZUGFeRDTradeLineItem
  ,intf.ZUGFeRDTradeAllowanceCharge
  ,intf.ZUGFeRDTax
  ,intf.ZUGFeRDVersion
  ;

type
  TZUGFeRDValidationResult = class
  private
    FIsValid: Boolean;
    FMessages: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    property IsValid: Boolean read FIsValid write FIsValid;
    property Messages: TStringList read FMessages;
  end;

  /// <summary>
  /// Validator for ZUGFeRD invoice descriptor.
  ///
  /// Currently limited to summarizing line totals
  ///
  /// Output syntax copied from Konik library (https://konik.io/)
  /// </summary>
  TZUGFeRDInvoiceValidator = class
  public
    class procedure ValidateAndPrint(descriptor: TZUGFeRDInvoiceDescriptor; version: TZUGFeRDVersion; filename: string = '');
    class function Validate(descriptor: TZUGFeRDInvoiceDescriptor; version: TZUGFeRDVersion): TZUGFeRDValidationResult;
  end;

implementation

constructor TZUGFeRDValidationResult.Create;
begin
  inherited Create;
  FIsValid := False;
  FMessages := TStringList.Create;
end;

destructor TZUGFeRDValidationResult.Destroy;
begin
  FMessages.Free;
  inherited;
end;


class procedure TZUGFeRDInvoiceValidator.ValidateAndPrint(descriptor: TZUGFeRDInvoiceDescriptor; version: TZUGFeRDVersion; filename: string = '');
var
  validationResult : TZUGFeRDValidationResult;
  line: string;
begin
  validationResult  := TZUGFeRDInvoiceValidator.Validate(descriptor, version);
  try
    if not filename.IsEmpty then
      validationResult.Messages.SaveToFile(filename);

    for line in validationResult.Messages do
      Writeln(line);
  finally
    validationResult.Free;
  end;
end;

class function TZUGFeRDInvoiceValidator.Validate(descriptor: TZUGFeRDInvoiceDescriptor; version: TZUGFeRDVersion): TZUGFeRDValidationResult;
var
  lineCounter: Integer;
  lineTotal, allowanceTotal, chargeTotal, taxTotal, grandTotal: Currency;
  lineTotalPerTax: TDictionary<Currency, Currency>;
  item: TZUGFeRDTradeLineItem;
  kv: TPair<Currency, Currency>;
  tax: TZUGFeRDTax;
begin
  Result := TZUGFeRDValidationResult.Create;
  Result.IsValid := true;

  if descriptor = nil then
  begin
    Result.Messages.Add('Invalid invoice descriptor');
    Result.IsValid := false;
    exit;
  end;


  lineCounter := 0;
  lineTotal := 0;
  allowanceTotal := 0;
  chargeTotal := 0;
  taxTotal := 0;
  //grandTotal := 0;
  lineTotalPerTax := TDictionary<Currency, Currency>.Create;
  try
    // line item summation
    Result.Messages.Add('Validating invoice monetary summation');
    Result.Messages.Add(Format('Starting recalculating line total from %d items...', [descriptor.TradeLineItems.Count]));

    for item in descriptor.TradeLineItems do
    begin
      var _total : Currency := 0;
      if item.NetUnitPrice.HasValue then
      begin
        _total := (item.NetUnitPrice.Value * item.BilledQuantity);
        lineTotal := lineTotal + _total;
      end;

      if not lineTotalPerTax.ContainsKey(item.TaxPercent) then
        lineTotalPerTax.Add(item.TaxPercent, 0);

      lineTotalPerTax[item.TaxPercent] := lineTotalPerTax[item.TaxPercent] + _total;

      //retval.Add(String.Format("==> {0}:", ++lineCounter));
      //retval.Add(String.Format("Recalculating item: [{0}]", item.Name));
      //retval.Add(String.Format("Line total formula: {0:0.0000} EUR (net price) x {1:0.0000} (quantity)", item.NetUnitPrice, item.BilledQuantity));

      //retval.Add(String.Format("Recalculated item line total = {0:0.0000} EUR", _total));
      //retval.Add(String.Format("Recalculated item tax = {0:0.00} %", item.TaxPercent));
      //retval.Add(String.Format("Current monetarySummation.lineTotal = {0:0.0000} EUR(the sum of all line totals)", lineTotal));

      Inc(lineCounter);
      Result.Messages.Add(Format('%d;%s;%f', [lineCounter, item.Name, _total]));
    end;

    Result.Messages.Add('==> DONE!');
    Result.Messages.Add('Finished recalculating monetarySummation.lineTotal...');
    Result.Messages.Add('Adding tax amounts from invoice allowance charge...');

    for var charge in descriptor.GetTradeCharges do
    begin
      Result.Messages.Add(Format('==> added %f to %f%%', [-charge.Amount, charge.Tax.Percent]));

      if not lineTotalPerTax.ContainsKey(charge.Tax.Percent) then
        lineTotalPerTax.Add(charge.Tax.Percent, 0);

      lineTotalPerTax[charge.Tax.Percent] := lineTotalPerTax[charge.Tax.Percent] - charge.Amount;
      chargeTotal:= chargeTotal + charge.Amount
    end;

    for var allowance in descriptor.GetTradeAllowances do
    begin
      Result.Messages.Add(Format('==> added %f to %f%%', [-allowance.Amount, allowance.Tax.Percent]));

      if not lineTotalPerTax.ContainsKey(allowance.Tax.Percent) then
        lineTotalPerTax.Add(allowance.Tax.Percent, 0);

      lineTotalPerTax[allowance.Tax.Percent] := lineTotalPerTax[allowance.Tax.Percent] - allowance.Amount;
      allowanceTotal := allowanceTotal + allowance.Amount;
    end;

    Result.Messages.Add('Adding tax amounts from invoice service charge...');
    // TODO

    // TODO ausgeben: Recalculating tax basis for tax percentages: [Key{percentage=7.00, code=[VAT] Value added tax, category=[S] Standard rate}, Key{percentage=19.00, code=[VAT] Value added tax, category=[S] Standard rate}]

    Result.Messages.Add(Format('Recalculated tax basis = %f', [lineTotal - allowanceTotal]));
    Result.Messages.Add('Calculating tax total...');

    for kv in lineTotalPerTax do
    begin
      var _taxTotal : Currency := (kv.Value * kv.Key / 100);
      taxTotal := taxTotal + _taxTotal;
      Result.Messages.Add(Format('===> %f x %f%% = %f', [kv.Value, kv.Key, _taxTotal]));
    end;

    grandTotal := lineTotal - allowanceTotal + taxTotal;

    Result.Messages.Add(Format('Recalculated tax total = %f', [taxTotal]));
    Result.Messages.Add(Format('Recalculated grand total = %f EUR(tax basis total + tax total)', [grandTotal]));
    Result.Messages.Add('Recalculating invoice monetary summation DONE!');
    Result.Messages.Add(Format('==> result: MonetarySummation[lineTotal = %f, chargeTotal = %f, allowanceTotal = %f, taxBasisTotal = %f, taxTotal = %f, grandTotal = %f, totalPrepaid = %f, duePayable = %f]',
      [lineTotal,
       0.0, // chargeTotal
       allowanceTotal,
       lineTotal - allowanceTotal, // - chargeTotal
       taxTotal,
       grandTotal,
       0.0, // prepaid
       lineTotal - allowanceTotal + taxTotal // - chargetotal + prepaid
       ]));

    var _taxBasisTotal : Currency := 0;
    for tax in descriptor.Taxes do
    begin
      _taxBasisTotal := _taxBasisTotal + tax.BasisAmount;
    end;

    var _allowanceTotal : Currency := 0;
    var _chargeTotal : Currency := 0;
    for var allowance in descriptor.GetTradeAllowances do
      _allowanceTotal := _allowanceTotal + allowance.ActualAmount;
    for var charge in descriptor.GetTradeCharges do
        _chargeTotal := _chargeTotal + charge.ActualAmount;

    if not descriptor.TaxTotalAmount.HasValue then
    begin
      Result.Messages.Add(Format('trade.settlement.monetarySummation.taxTotal Message: Kein TaxTotalAmount vorhanden', []));
      Result.IsValid := false;
    end
    else if Abs(taxTotal - descriptor.TaxTotalAmount.Value) < 0.01 then
    begin
      Result.Messages.Add(Format('trade.settlement.monetarySummation.taxTotal Message: Berechneter Wert ist wie vorhanden:[%4f]', [taxTotal]));
    end
    else
    begin
      Result.Messages.Add(Format('trade.settlement.monetarySummation.taxTotal Message: Berechneter Wert ist[%4f] aber tatsächliche vorhander Wert ist[%4f] | Actual value: %4f)', [taxTotal, descriptor.TaxTotalAmount.GetValueOrDefault]));
      Result.IsValid := false;
    end;

    if Abs(lineTotal - descriptor.LineTotalAmount.Value) < 0.01 then
    begin
      Result.Messages.Add(Format('trade.settlement.monetarySummation.lineTotal Message: Berechneter Wert ist wie vorhanden:[%4f]', [lineTotal]));
    end
    else
    begin
      Result.Messages.Add(Format('trade.settlement.monetarySummation.lineTotal Message: Berechneter Wert ist[%4f] aber tatsächliche vorhander Wert ist[%4f] | Actual value: %4f)', [lineTotal, descriptor.LineTotalAmount.GetValueOrDefault]));
      Result.IsValid := false;
    end;

    if not descriptor.GrandTotalAmount.HasValue then
    begin
      Result.Messages.Add(Format('trade.settlement.monetarySummation.grandTotal Message: Kein GrandTotalAmount vorhanden', []));
      Result.IsValid := false;
    end
    else if Abs(grandTotal - descriptor.GrandTotalAmount.Value) < 0.01 then
    begin
      Result.Messages.Add(Format('trade.settlement.monetarySummation.grandTotal Message: Berechneter Wert ist wie vorhanden:[%4f]', [grandTotal]));
    end
    else
    begin
      Result.Messages.Add(Format('trade.settlement.monetarySummation.grandTotal Message: Berechneter Wert ist[%4f] aber tatsächliche vorhander Wert ist[%4f] | Actual value: %4f)', [grandTotal, descriptor.GrandTotalAmount.GetValueOrDefault]));
      Result.IsValid := false;
    end;

    {
      * @todo Richtige Validierung implementieren
    }
    if Abs(_taxBasisTotal - _taxBasisTotal) < 0.01 then
    begin
      Result.Messages.Add(Format('trade.settlement.monetarySummation.taxBasisTotal Message: Berechneter Wert ist wie vorhanden:[%4f]', [_taxBasisTotal]));
    end
    else
    begin
      Result.Messages.Add(Format('trade.settlement.monetarySummation.taxBasisTotal Message: Berechneter Wert ist[%4f] aber tatsächliche vorhander Wert ist[%4f] | Actual value: %4f)', [_taxBasisTotal, _taxBasisTotal]));
      Result.IsValid := false;
    end;

    if Abs(allowanceTotal - _allowanceTotal) < 0.01 then
    begin
      Result.Messages.Add(Format('trade.settlement.monetarySummation.allowanceTotal  Message: Berechneter Wert ist wie vorhanden:[%4f]', [_allowanceTotal]));
    end
    else
    begin
      Result.Messages.Add(Format('trade.settlement.monetarySummation.allowanceTotal  Message: Berechneter Wert ist[%4f] aber tatsächliche vorhander Wert ist[%4f] | Actual value: %4f)', [allowanceTotal, _allowanceTotal]));
      Result.IsValid := false;
    end;

    if Abs(chargeTotal - _chargeTotal) < 0.01 then
    begin
      Result.Messages.Add(Format('trade.settlement.monetarySummation.chargeTotal  Message: Berechneter Wert ist wie vorhanden:[%4f]', [_chargeTotal]));
    end
    else
    begin
      Result.Messages.Add(Format('trade.settlement.monetarySummation.chargeTotal  Message: Berechneter Wert ist[%4f] aber tatsächliche vorhander Wert ist[%4f] | Actual value: %4f)', [chargeTotal, _chargeTotal]));
      Result.IsValid := false;
    end;

    // version-specific validation
    // ZUGFeRD 1.0 version specific validation skipped

  finally
    lineTotalPerTax.Free;
  end;
end;

end.
