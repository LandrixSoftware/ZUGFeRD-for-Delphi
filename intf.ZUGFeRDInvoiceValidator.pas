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
  System.SysUtils,System.TypInfo,System.Classes,System.Math
  ,intf.ZUGFeRDInvoiceDescriptor
  ,intf.ZUGFeRDTradeLineItem
  ,intf.ZUGFeRDTradeAllowanceCharge
  ,intf.ZUGFeRDTax
  ,intf.ZUGFeRDTaxTypes
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
  lineTotal, allowanceTotal, chargeTotal, taxTotal, grandTotal, prepaid,
    rounding, duePayable, expectedDuePayable, expectedTaxAmount: Currency;
  declaredAllowanceTotal, declaredChargeTotal, expectedTaxBasis: Currency;
  taxDeviation: Currency;
  item: TZUGFeRDTradeLineItem;
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
  // line item summation
    Result.Messages.Add('Validating invoice monetary summation');
    Result.Messages.Add(Format('Starting recalculating line total from %d items...', [descriptor.TradeLineItems.Count]));

    for item in descriptor.TradeLineItems do
    begin
      var _total : Currency := 0;
      if item.NetUnitPrice.HasValue then
      begin
        _total := (item.NetUnitPrice.Value * item.BilledQuantity);

        // BT-146 gilt je Preisbasismenge BT-149, nicht je Einheit. Fehlt BT-149,
        // ist die Basismenge 1. Der CII-Writer rechnet bei fehlendem BT-131 genauso.
        if item.NetQuantity.HasValue then
        begin
          if item.NetQuantity.Value > 0 then
            _total := _total / item.NetQuantity.Value
          else
          begin
            Result.Messages.Add(Format('BT-149: Die Preisbasismenge der Position [%s] muss größer als 0 sein', [item.Name]));
            Result.IsValid := false;
          end;
        end;

        // BT-131 enthält BG-28-Positionszuschläge und vermindert sich um BG-27-Positionsabschläge.
        for var lineAllowance in item.GetSpecifiedTradeAllowances do
          _total := _total - lineAllowance.ActualAmount;
        for var lineCharge in item.GetSpecifiedTradeCharges do
          _total := _total + lineCharge.ActualAmount;

        lineTotal := lineTotal + _total;
      end;

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
      // ActualAmount, nicht Amount: die geerbte Amount-Property der Basisklasse wird
      // fuer Zu- und Abschlaege nirgends befuellt und ist daher immer 0
      Result.Messages.Add(Format('==> added %f to %f%%', [charge.ActualAmount, charge.Tax.Percent]));

      chargeTotal:= chargeTotal + charge.ActualAmount
    end;

    for var allowance in descriptor.GetTradeAllowances do
    begin
      Result.Messages.Add(Format('==> subtracted %f from %f%%', [allowance.ActualAmount, allowance.Tax.Percent]));

      allowanceTotal := allowanceTotal + allowance.ActualAmount;
    end;

    Result.Messages.Add('Adding tax amounts from invoice service charge...');
    // TODO

    // TODO ausgeben: Recalculating tax basis for tax percentages: [Key{percentage=7.00, code=[VAT] Value added tax, category=[S] Standard rate}, Key{percentage=19.00, code=[VAT] Value added tax, category=[S] Standard rate}]

    Result.Messages.Add(Format('Recalculated tax basis = %f', [lineTotal - allowanceTotal + chargeTotal]));
    Result.Messages.Add('Calculating tax total...');

    for tax in descriptor.Taxes do
    begin
      if not tax.TypeCode.HasValue then
      begin
        Result.Messages.Add('Tax type code is required for every tax breakdown');
        Result.IsValid := false;
        Continue;
      end;
      if tax.TypeCode.Value <> TZUGFeRDTaxTypes.VAT then
        Continue;

      expectedTaxAmount := SimpleRoundTo(tax.BasisAmount * tax.Percent / 100, -2);

      // BR-CO-14 bildet BT-110 aus den angegebenen BT-117, nicht aus den
      // nachgerechneten Sollwerten. Beides fällt nur zusammen, solange BR-CO-17
      // exakt erzwungen wird - die Regel lässt aber eine Abweichung zu.
      taxTotal := taxTotal + tax.TaxAmount;

      Result.Messages.Add(Format('===> %f x %f%% = %f', [tax.BasisAmount, tax.Percent, expectedTaxAmount]));

      // BR-DEC-20: BT-117 darf höchstens zwei Nachkommastellen haben.
      if tax.TaxAmount <> SimpleRoundTo(tax.TaxAmount, -2) then
      begin
        Result.Messages.Add(Format(
          'BR-DEC-20: Der Steuerbetrag[%4f] hat mehr als zwei Nachkommastellen', [tax.TaxAmount]));
        Result.IsValid := false;
      end;

      // BR-CO-17 lässt laut EN-16931-Schematron eine Abweichung von einer
      // Währungseinheit je Steueraufschlüsselung zu. Was darüber liegt, ist ein
      // Verstoß; was darunter liegt, wird protokolliert, damit es nicht untergeht.
      taxDeviation := tax.TaxAmount - expectedTaxAmount;
      if Abs(taxDeviation) > 1 then
      begin
        Result.Messages.Add(Format(
          'BR-CO-17: Berechneter Steuerbetrag ist[%4f] aber vorhandener Steuerbetrag ist[%4f] bei Bemessungsgrundlage[%4f] und Steuersatz[%4f]',
          [expectedTaxAmount, tax.TaxAmount, tax.BasisAmount, tax.Percent]));
        Result.IsValid := false;
      end
      else if taxDeviation <> 0 then
      begin
        Result.Messages.Add(Format(
          'Hinweis: Der Steuerbetrag[%4f] weicht um [%4f] vom berechneten Wert[%4f] ab, bleibt aber innerhalb der BR-CO-17-Toleranz von einer Währungseinheit',
          [tax.TaxAmount, taxDeviation, expectedTaxAmount]));
      end;
    end;

    // BR-CO-14 rundet die Summe der BT-117 auf zwei Nachkommastellen.
    taxTotal := SimpleRoundTo(taxTotal, -2);

    grandTotal := lineTotal - allowanceTotal + taxTotal + chargeTotal;
    prepaid := descriptor.TotalPrepaidAmount.GetValueOrDefault;
    rounding := descriptor.RoundingAmount.GetValueOrDefault;

    // BT-115 ergibt sich aus BT-112 abzüglich BT-113 zuzüglich BT-114.
    duePayable := grandTotal - prepaid + rounding;

    Result.Messages.Add(Format('Recalculated tax total = %f', [taxTotal]));
    Result.Messages.Add(Format('Recalculated grand total = %f EUR(tax basis total + tax total)', [grandTotal]));
    Result.Messages.Add('Recalculating invoice monetary summation DONE!');
    Result.Messages.Add(Format('==> result: MonetarySummation[lineTotal = %f, chargeTotal = %f, allowanceTotal = %f, taxBasisTotal = %f, taxTotal = %f, grandTotal = %f, totalPrepaid = %f, duePayable = %f]',
      [lineTotal,
       chargeTotal,
       allowanceTotal,
       lineTotal - allowanceTotal + chargeTotal, // tax basis total
       taxTotal,
       grandTotal,
       prepaid,
       duePayable
       ]));

    var _taxBasisTotal : Currency := 0;
    for tax in descriptor.Taxes do
    begin
      if tax.TypeCode.HasValue then
        if tax.TypeCode.Value = TZUGFeRDTaxTypes.VAT then
          _taxBasisTotal := _taxBasisTotal + tax.BasisAmount;
    end;

    declaredAllowanceTotal := descriptor.AllowanceTotalAmount.GetValueOrDefault;
    declaredChargeTotal := descriptor.ChargeTotalAmount.GetValueOrDefault;

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
      Result.Messages.Add(Format('trade.settlement.monetarySummation.taxTotal Message: Berechneter Wert ist[%4f] aber tatsächliche vorhander Wert ist[%4f]', [taxTotal, descriptor.TaxTotalAmount.GetValueOrDefault]));
      Result.IsValid := false;
    end;

    if not descriptor.LineTotalAmount.HasValue then
    begin
      Result.Messages.Add('trade.settlement.monetarySummation.lineTotal Message: Kein LineTotalAmount vorhanden');
      Result.IsValid := false;
    end
    else if Abs(lineTotal - descriptor.LineTotalAmount.Value) < 0.01 then
    begin
      Result.Messages.Add(Format('trade.settlement.monetarySummation.lineTotal Message: Berechneter Wert ist wie vorhanden:[%4f]', [lineTotal]));
    end
    else
    begin
      Result.Messages.Add(Format('trade.settlement.monetarySummation.lineTotal Message: Berechneter Wert ist[%4f] aber tatsächliche vorhander Wert ist[%4f]', [lineTotal, descriptor.LineTotalAmount.GetValueOrDefault]));
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
      Result.Messages.Add(Format('trade.settlement.monetarySummation.grandTotal Message: Berechneter Wert ist[%4f] aber tatsächliche vorhander Wert ist[%4f]', [grandTotal, descriptor.GrandTotalAmount.GetValueOrDefault]));
      Result.IsValid := false;
    end;

    if not descriptor.DuePayableAmount.HasValue then
    begin
      Result.Messages.Add('trade.settlement.monetarySummation.duePayable Message: Kein DuePayableAmount vorhanden');
      Result.IsValid := false;
    end
    else if descriptor.GrandTotalAmount.HasValue then
    begin
      expectedDuePayable := descriptor.GrandTotalAmount.Value - prepaid + rounding;
      if Abs(expectedDuePayable - descriptor.DuePayableAmount.Value) < 0.01 then
      begin
        Result.Messages.Add(Format('trade.settlement.monetarySummation.duePayable Message: Berechneter Wert ist wie vorhanden:[%4f]', [expectedDuePayable]));
      end
      else
      begin
        Result.Messages.Add(Format('trade.settlement.monetarySummation.duePayable Message: Berechneter Wert ist[%4f] aber tatsächlicher vorhandener Wert ist[%4f]',
          [expectedDuePayable, descriptor.DuePayableAmount.Value]));
        Result.IsValid := false;
      end;
    end;

    {
      * Die Summe der Steuerbemessungsgrundlagen der Steueraufschlüsselung (BT-116)
      * muss dem Rechnungsbetrag ohne Umsatzsteuer (BT-109) entsprechen.
      *
      * Preisnachlässe aus tradeLineItem.TradeAllowanceCharges sind bereits im
      * Nettoeinzelpreis BT-146 enthalten. Die Nachrechnung rundet nicht je Steuersatz.
      * Deshalb meldet der Validator die Beispielrechnungen der Dokumentation
      * teilweise als ungültig.
    }
    if not descriptor.TaxBasisAmount.HasValue then
    begin
      Result.Messages.Add(Format('trade.settlement.monetarySummation.taxBasisTotal Message: Kein TaxBasisAmount vorhanden', []));
      Result.IsValid := false;
    end
    else if Abs(_taxBasisTotal - descriptor.TaxBasisAmount.Value) < 0.01 then
    begin
      Result.Messages.Add(Format('trade.settlement.monetarySummation.taxBasisTotal Message: Berechneter Wert ist wie vorhanden:[%4f]', [_taxBasisTotal]));
    end
    else
    begin
      Result.Messages.Add(Format('trade.settlement.monetarySummation.taxBasisTotal Message: Berechneter Wert ist[%4f] aber tatsächlicher vorhandener Wert ist[%4f]',
        [_taxBasisTotal, descriptor.TaxBasisAmount.Value]));
      Result.IsValid := false;
    end;

    // BR-CO-11: Die Summe der Abschläge auf Dokumentenebene (BT-107) muss der
    // Summe der einzelnen Kopfabschläge (BT-92) entsprechen. BR-CO-12 verlangt
    // dasselbe für die Zuschläge (BT-108 gegen BT-99). Beide Kopfsummen sind
    // optional; fehlen sie, gelten sie als 0.
    if Abs(allowanceTotal - declaredAllowanceTotal) < 0.01 then
    begin
      Result.Messages.Add(Format('trade.settlement.monetarySummation.allowanceTotal  Message: Berechneter Wert ist wie vorhanden:[%4f]', [declaredAllowanceTotal]));
    end
    else
    begin
      Result.Messages.Add(Format('BR-CO-11: trade.settlement.monetarySummation.allowanceTotal  Message: Berechneter Wert ist[%4f] aber tatsächlich vorhandener Wert ist[%4f]', [allowanceTotal, declaredAllowanceTotal]));
      Result.IsValid := false;
    end;

    if Abs(chargeTotal - declaredChargeTotal) < 0.01 then
    begin
      Result.Messages.Add(Format('trade.settlement.monetarySummation.chargeTotal  Message: Berechneter Wert ist wie vorhanden:[%4f]', [declaredChargeTotal]));
    end
    else
    begin
      Result.Messages.Add(Format('BR-CO-12: trade.settlement.monetarySummation.chargeTotal  Message: Berechneter Wert ist[%4f] aber tatsächlich vorhandener Wert ist[%4f]', [chargeTotal, declaredChargeTotal]));
      Result.IsValid := false;
    end;

    // BR-CO-13: BT-109 ergibt sich aus BT-106 abzüglich BT-107 zuzüglich BT-108.
    // Ohne diese Prüfung können sich BT-109 und die Steueraufschlüsselung
    // gemeinsam vom Positionsnetto entfernen, ohne beanstandet zu werden: der
    // Abgleich weiter oben vergleicht BT-109 nur gegen die Summe der BT-116.
    if descriptor.LineTotalAmount.HasValue and descriptor.TaxBasisAmount.HasValue then
    begin
      expectedTaxBasis := descriptor.LineTotalAmount.Value - declaredAllowanceTotal + declaredChargeTotal;
      if Abs(expectedTaxBasis - descriptor.TaxBasisAmount.Value) < 0.01 then
      begin
        Result.Messages.Add(Format('BR-CO-13: Steuerbemessungsgrundlage aus BT-106 - BT-107 + BT-108 ist wie vorhanden:[%4f]', [expectedTaxBasis]));
      end
      else
      begin
        Result.Messages.Add(Format('BR-CO-13: Steuerbemessungsgrundlage aus BT-106 - BT-107 + BT-108 ist[%4f] aber tatsächlich vorhandener Wert ist[%4f]',
          [expectedTaxBasis, descriptor.TaxBasisAmount.Value]));
        Result.IsValid := false;
      end;
    end;

    // version-specific validation
    // ZUGFeRD 1.0 version specific validation skipped

end;

end.
