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
  ;

type
  /// <summary>
  /// Validator for ZUGFeRD invoice descriptor.
  ///
  /// Currently limited to summarizing line totals
  ///
  /// Output syntax copied from Konik library (https://konik.io/)
  /// </summary>
  TZUGFeRDInvoiceValidator = class
  public
    class procedure ValidateAndPrint(descriptor: TZUGFeRDInvoiceDescriptor; filename: string = '');
    class function Validate(descriptor: TZUGFeRDInvoiceDescriptor): TStringList;
  end;

implementation

class procedure TZUGFeRDInvoiceValidator.ValidateAndPrint(
  descriptor: TZUGFeRDInvoiceDescriptor; filename: string = '');
var
  output: TStringList;
  line: string;
begin
  output := TZUGFeRDInvoiceValidator.Validate(descriptor);
  try
    if not filename.IsEmpty then
      output.SaveToFile(filename);

    for line in output do
      Writeln(line);
  finally
    output.Free;
  end;
end;

class function TZUGFeRDInvoiceValidator.Validate(
  descriptor: TZUGFeRDInvoiceDescriptor): TStringList;
var
  lineCounter: Integer;
  lineTotal, allowanceTotal, taxTotal, grandTotal: Currency;
  lineTotalPerTax: TDictionary<Currency, Currency>;
  item: TZUGFeRDTradeLineItem;
  charge: TZUGFeRDTradeAllowanceCharge;
  kv: TPair<Currency, Currency>;
  tax: TZUGFeRDTax;
  allowance: TZUGFeRDTradeAllowanceCharge;
begin
  Result := TStringList.Create;

  if descriptor = nil then
  begin
    Result.Add('Invalid invoice descriptor');
    exit;
  end;


  lineCounter := 0;
  lineTotal := 0;
  allowanceTotal := 0;
  taxTotal := 0;
  //grandTotal := 0;
  lineTotalPerTax := TDictionary<Currency, Currency>.Create;
  try
    // line item summation
    Result.Add('Validating invoice monetary summation');
    Result.Add(Format('Starting recalculating line total from %d items...', [descriptor.TradeLineItems.Count]));

//            foreach(TradeLineItem item in descriptor.TradeLineItems)
//            {
//
//
//                retval.Add(String.Format("{0};{1};{2}", ++lineCounter, item.Name, _total));
//            }

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
      Result.Add(Format('%d;%s;%f', [lineCounter, item.Name, _total]));
    end;

    Result.Add('==> DONE!');
    Result.Add('Finished recalculating monetarySummation.lineTotal...');
    Result.Add('Adding tax amounts from invoice allowance charge...');

    for charge in descriptor.TradeAllowanceCharges do
    begin
      Result.Add(Format('==> added %f to %f%%', [-charge.Amount, charge.Tax.Percent]));

      if not lineTotalPerTax.ContainsKey(charge.Tax.Percent) then
        lineTotalPerTax.Add(charge.Tax.Percent, 0);

      lineTotalPerTax[charge.Tax.Percent] := lineTotalPerTax[charge.Tax.Percent] - charge.Amount;
      allowanceTotal := allowanceTotal + charge.Amount;
    end;

    Result.Add('Adding tax amounts from invoice service charge...');
    // TODO

    Result.Add(Format('Recalculated tax basis = %f', [lineTotal - allowanceTotal]));
    Result.Add('Calculating tax total...');

    for kv in lineTotalPerTax do
    begin
      var _taxTotal : Currency := (kv.Value * kv.Key / 100);
      taxTotal := taxTotal + _taxTotal;
      Result.Add(Format('===> %f x %f%% = %f', [kv.Value, kv.Key, _taxTotal]));
    end;

    grandTotal := lineTotal - allowanceTotal + taxTotal;

    Result.Add(Format('Recalculated tax total = %f', [taxTotal]));
    Result.Add(Format('Recalculated grand total = %f EUR(tax basis total + tax total)', [grandTotal]));
    Result.Add('Recalculating invoice monetary summation DONE!');
    Result.Add(Format('==> result: MonetarySummation[lineTotal = %f, chargeTotal = %f, allowanceTotal = %f, taxBasisTotal = %f, taxTotal = %f, grandTotal = %f, totalPrepaid = %f, duePayable = %f]',
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
    for allowance in descriptor.TradeAllowanceCharges do
    begin
      _allowanceTotal := _allowanceTotal + allowance.ActualAmount;
    end;

    if not descriptor.TaxTotalAmount.HasValue then
    begin
      Result.Add(Format('trade.settlement.monetarySummation.taxTotal Message: Kein TaxTotalAmount vorhanden', []));
    end
    else if Abs(taxTotal - descriptor.TaxTotalAmount.Value) < 0.01 then
    begin
      Result.Add(Format('trade.settlement.monetarySummation.taxTotal Message: Berechneter Wert ist wie vorhanden:[%0.0000]', [taxTotal]));
    end
    else
    begin
      Result.Add(Format('trade.settlement.monetarySummation.taxTotal Message: Berechneter Wert ist[%0.0000] aber tatsächliche vorhander Wert ist[%1:0.0000] | Actual value: %1:0.0000)', [taxTotal, descriptor.TaxTotalAmount]));
    end;

    if Abs(lineTotal - descriptor.LineTotalAmount.Value) < 0.01 then
    begin
      Result.Add(Format('trade.settlement.monetarySummation.lineTotal Message: Berechneter Wert ist wie vorhanden:[%0.0000]', [lineTotal]));
    end
    else
    begin
      Result.Add(Format('trade.settlement.monetarySummation.lineTotal Message: Berechneter Wert ist[%0.0000] aber tatsächliche vorhander Wert ist[%1:0.0000] | Actual value: %1:0.0000)', [lineTotal, descriptor.LineTotalAmount]));
    end;

    if not descriptor.GrandTotalAmount.HasValue then
    begin
      Result.Add(Format('trade.settlement.monetarySummation.grandTotal Message: Kein GrandTotalAmount vorhanden', []));
    end
    else if Abs(grandTotal - descriptor.GrandTotalAmount.Value) < 0.01 then
    begin
      Result.Add(Format('trade.settlement.monetarySummation.grandTotal Message: Berechneter Wert ist wie vorhanden:[%0.0000]', [grandTotal]));
    end
    else
    begin
      Result.Add(Format('trade.settlement.monetarySummation.grandTotal Message: Berechneter Wert ist[%0.0000] aber tatsächliche vorhander Wert ist[%1:0.0000] | Actual value: %1:0.0000)', [grandTotal, descriptor.GrandTotalAmount]));
    end;

    {
      * @todo Richtige Validierung implementieren
    }
    if Abs(_taxBasisTotal - _taxBasisTotal) < 0.01 then
    begin
      Result.Add(Format('trade.settlement.monetarySummation.taxBasisTotal Message: Berechneter Wert ist wie vorhanden:[%0.0000]', [_taxBasisTotal]));
    end
    else
    begin
      Result.Add(Format('trade.settlement.monetarySummation.taxBasisTotal Message: Berechneter Wert ist[%0.0000] aber tatsächliche vorhander Wert ist[%1:0.0000] | Actual value: %1:0.0000)', [_taxBasisTotal, _taxBasisTotal]));
    end;

    if Abs(allowanceTotal - _allowanceTotal) < 0.01 then
    begin
      Result.Add(Format('trade.settlement.monetarySummation.allowanceTotal  Message: Berechneter Wert ist wie vorhanden:[%0.0000]', [_allowanceTotal]));
    end
    else
    begin
      Result.Add(Format('trade.settlement.monetarySummation.allowanceTotal  Message: Berechneter Wert ist[%0.0000] aber tatsächliche vorhander Wert ist[%1:0.0000] | Actual value: %1:0.0000)', [allowanceTotal, _allowanceTotal]));
    end;

  finally
    lineTotalPerTax.Free;
  end;
end;

end.
