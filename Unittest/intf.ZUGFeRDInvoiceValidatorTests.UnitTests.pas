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

unit intf.ZUGFeRDInvoiceValidatorTests.UnitTests;

/// <summary>
/// Tests für TZUGFeRDInvoiceValidator.
///
/// Der Validator rechnet die Summen einer Rechnung nach und vergleicht sie mit den
/// angegebenen Werten. Die Tests bauen dafür bewusst minimale Rechnungen auf, deren
/// Summen exakt aufgehen.
/// </summary>

interface

uses
  DUnitX.TestFramework,
  intf.ZUGFeRDInvoiceDescriptor,
  intf.ZUGFeRDTestBase;

type
  [TestFixture]
  TZUGFeRDInvoiceValidatorTests = class(TZUGFeRDTestBase)
  private
    /// <summary>
    /// Baut eine Rechnung mit einer Position (2 x 100,00 zu 19%) und optionalen
    /// Zu- und Abschlägen. Die Summen im Descriptor sind passend gesetzt, der
    /// Validator muss die Rechnung also als gueltig ansehen.
    /// </summary>
    function CreateBalancedInvoice(const withCharge: Boolean;
      const lineAllowance: Currency = 0; const lineCharge: Currency = 0): TZUGFeRDInvoiceDescriptor;
  public
    [Test]
    procedure TestValidInvoiceWithoutAllowanceOrCharge;
    [Test]
    procedure TestValidInvoiceWithCharge;
    [Test]
    procedure TestValidInvoiceWithLineAllowance;
    [Test]
    procedure TestValidInvoiceWithLineCharge;
    [Test]
    procedure TestPriceAllowanceIsNotSubtractedTwice;
    [Test]
    procedure TestInvalidTaxTotalIsReported;
    [Test]
    procedure TestMissingTaxBasisAmountIsReported;
    [Test]
    procedure TestInvalidTaxBasisAmountIsReported;
    [Test]
    procedure TestValidationDoesNotRaiseOnDeviation;
  end;

implementation

uses
  System.SysUtils, System.Classes,
  intf.ZUGFeRDInvoiceValidator,
  intf.ZUGFeRDVersion,
  intf.ZUGFeRDCurrencyCodes,
  intf.ZUGFeRDQuantityCodes,
  intf.ZUGFeRDTaxTypes,
  intf.ZUGFeRDTaxCategoryCodes,
  intf.ZUGFeRDTradeLineItem,
  intf.ZUGFeRDHelper;

{ TZUGFeRDInvoiceValidatorTests }

function TZUGFeRDInvoiceValidatorTests.CreateBalancedInvoice(
  const withCharge: Boolean; const lineAllowance, lineCharge: Currency): TZUGFeRDInvoiceDescriptor;
var
  lineTotal, chargeTotal, taxBasis, taxTotal: Currency;
  lineItem: TZUGFeRDTradeLineItem;
begin
  Result := TZUGFeRDInvoiceDescriptor.CreateInvoice('RE-4711', EncodeDate(2026, 1, 15),
    TZUGFeRDCurrencyCodes.EUR);

  lineItem := Result.AddTradeLineItem(
    {name=}            'Testartikel',
    {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(100),
    {description=}     '',
    {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.H87),
    {unitQuantity=}    nil,
    {grossUnitPrice=}  nil,
    {billedQuantity=}  2,
    {lineTotalAmount=} 200,
    {taxType=}         TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
    {categoryCode=}    TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
    {taxPercent=}      19.0
  );

  if lineAllowance <> 0 then
    lineItem.AddSpecifiedTradeAllowance(TZUGFeRDCurrencyCodes.EUR, 200,
      lineAllowance, 'Mengenrabatt');
  if lineCharge <> 0 then
    lineItem.AddSpecifiedTradeCharge(TZUGFeRDCurrencyCodes.EUR, 200,
      lineCharge, 'Positionszuschlag');

  // BT-131 enthält BG-28-Positionszuschläge und vermindert sich um BG-27-Positionsabschläge.
  lineTotal := 200 - lineAllowance + lineCharge;
  lineItem.LineTotalAmount := lineTotal;
  chargeTotal := 0;

  if withCharge then
  begin
    chargeTotal := 10;
    Result.AddTradeCharge(
      {basisAmount=}     TZUGFeRDNullableParam<Currency>.Create(lineTotal),
      {currency=}        TZUGFeRDCurrencyCodes.EUR,
      {actualAmount=}    chargeTotal,
      {reason=}          'Eilzuschlag',
      {taxTypeCode=}     TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
      {taxCategoryCode=} TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
      {taxPercent=}      19.0);
  end;

  // Ein Zuschlag erhoeht die Bemessungsgrundlage
  taxBasis := lineTotal + chargeTotal;
  taxTotal := taxBasis * 19 / 100;

  Result.AddApplicableTradeTax({calculatedAmount=} taxTotal, {basisAmount=} taxBasis,
    {percent=} 19.0, TZUGFeRDTaxTypes.VAT, TZUGFeRDTaxCategoryCodes.S);

  Result.SetTotals(
    {aLineTotalAmount=}      lineTotal,
    {aChargeTotalAmount=}    chargeTotal,
    {aAllowanceTotalAmount=} 0,
    {aTaxBasisAmount=}       taxBasis,
    {aTaxTotalAmount=}       taxTotal,
    {aGrandTotalAmount=}     taxBasis + taxTotal,
    {aTotalPrepaidAmount=}   0,
    {aDuePayableAmount=}     taxBasis + taxTotal);
end;

procedure TZUGFeRDInvoiceValidatorTests.TestValidInvoiceWithLineAllowance;
var
  desc: TZUGFeRDInvoiceDescriptor;
  validationResult: TZUGFeRDValidationResult;
begin
  desc := CreateBalancedInvoice(False, {lineAllowance=} 10);
  try
    validationResult := TZUGFeRDInvoiceValidator.Validate(desc, TZUGFeRDVersion.Version23);
    try
      Assert.IsTrue(validationResult.IsValid,
        'Rechnung mit Positionsabschlag wurde als ungueltig gemeldet:'#13#10 + validationResult.Messages.Text);
    finally
      validationResult.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDInvoiceValidatorTests.TestValidInvoiceWithLineCharge;
var
  desc: TZUGFeRDInvoiceDescriptor;
  validationResult: TZUGFeRDValidationResult;
begin
  desc := CreateBalancedInvoice(False, {lineAllowance=} 0, {lineCharge=} 10);
  try
    validationResult := TZUGFeRDInvoiceValidator.Validate(desc, TZUGFeRDVersion.Version23);
    try
      Assert.IsTrue(validationResult.IsValid,
        'Rechnung mit Positionszuschlag wurde als ungueltig gemeldet:'#13#10 + validationResult.Messages.Text);
    finally
      validationResult.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDInvoiceValidatorTests.TestPriceAllowanceIsNotSubtractedTwice;
var
  desc: TZUGFeRDInvoiceDescriptor;
  validationResult: TZUGFeRDValidationResult;
begin
  desc := CreateBalancedInvoice(False);
  try
    // Der Preisnachlass ist bereits im Nettoeinzelpreis BT-146 enthalten und darf BT-131 nicht erneut vermindern.
    desc.TradeLineItems[0].AddTradeAllowance(TZUGFeRDCurrencyCodes.EUR, 110,
      10, 'Preisnachlass');

    validationResult := TZUGFeRDInvoiceValidator.Validate(desc, TZUGFeRDVersion.Version23);
    try
      Assert.IsTrue(validationResult.IsValid,
        'Preisnachlass wurde bei der Positionssumme doppelt abgezogen:'#13#10 + validationResult.Messages.Text);
    finally
      validationResult.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDInvoiceValidatorTests.TestValidInvoiceWithoutAllowanceOrCharge;
var
  desc: TZUGFeRDInvoiceDescriptor;
  validationResult: TZUGFeRDValidationResult;
begin
  desc := CreateBalancedInvoice(False);
  try
    validationResult := TZUGFeRDInvoiceValidator.Validate(desc, TZUGFeRDVersion.Version23);
    try
      Assert.IsTrue(validationResult.IsValid,
        'Rechnung mit stimmigen Summen wurde als ungueltig gemeldet:'#13#10 + validationResult.Messages.Text);
    finally
      validationResult.Free;
    end;
  finally
    desc.Free;
  end;
end;

/// <summary>
/// Sichert zwei Fehler ab, die der Validator hatte: der Zuschlag wurde von der
/// Bemessungsgrundlage abgezogen statt addiert, und er wurde ueber die nie befuellte
/// Property Amount statt ueber ActualAmount gelesen (also immer als 0 gewertet).
/// </summary>
procedure TZUGFeRDInvoiceValidatorTests.TestValidInvoiceWithCharge;
var
  desc: TZUGFeRDInvoiceDescriptor;
  validationResult: TZUGFeRDValidationResult;
begin
  desc := CreateBalancedInvoice(True);
  try
    validationResult := TZUGFeRDInvoiceValidator.Validate(desc, TZUGFeRDVersion.Version23);
    try
      Assert.IsTrue(validationResult.IsValid,
        'Rechnung mit Zuschlag wurde als ungueltig gemeldet:'#13#10 + validationResult.Messages.Text);
    finally
      validationResult.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDInvoiceValidatorTests.TestInvalidTaxTotalIsReported;
var
  desc: TZUGFeRDInvoiceDescriptor;
  validationResult: TZUGFeRDValidationResult;
begin
  desc := CreateBalancedInvoice(False);
  try
    // Steuerbetrag verfaelschen - der Validator muss das melden
    desc.TaxTotalAmount := 1;

    validationResult := TZUGFeRDInvoiceValidator.Validate(desc, TZUGFeRDVersion.Version23);
    try
      Assert.IsFalse(validationResult.IsValid, 'Falscher Steuerbetrag wurde nicht bemaengelt');
      Assert.IsTrue(validationResult.Messages.Text.Contains('taxTotal'),
        'Die Meldungen benennen den beanstandeten Wert nicht');
    finally
      validationResult.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDInvoiceValidatorTests.TestMissingTaxBasisAmountIsReported;
var
  desc: TZUGFeRDInvoiceDescriptor;
  validationResult: TZUGFeRDValidationResult;
begin
  desc := CreateBalancedInvoice(False);
  try
    desc.TaxBasisAmount := ZUGFeRDNullable<Currency>.Create(False);

    validationResult := TZUGFeRDInvoiceValidator.Validate(desc, TZUGFeRDVersion.Version23);
    try
      Assert.IsFalse(validationResult.IsValid, 'Fehlende Steuerbasis wurde nicht beanstandet');
      Assert.IsTrue(validationResult.Messages.Text.Contains('Kein TaxBasisAmount vorhanden'),
        'Die Meldungen benennen die fehlende Steuerbasis nicht');
    finally
      validationResult.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDInvoiceValidatorTests.TestInvalidTaxBasisAmountIsReported;
var
  desc: TZUGFeRDInvoiceDescriptor;
  validationResult: TZUGFeRDValidationResult;
begin
  desc := CreateBalancedInvoice(False);
  try
    desc.TaxBasisAmount := 199;

    validationResult := TZUGFeRDInvoiceValidator.Validate(desc, TZUGFeRDVersion.Version23);
    try
      Assert.IsFalse(validationResult.IsValid, 'Abweichende Steuerbasis wurde nicht beanstandet');
      Assert.IsTrue(validationResult.Messages.Text.Contains('taxBasisTotal'),
        'Die Meldungen benennen die abweichende Steuerbasis nicht');
    finally
      validationResult.Free;
    end;
  finally
    desc.Free;
  end;
end;

/// <summary>
/// Der Meldungspfad hatte Format-Strings mit mehr Platzhaltern als Argumenten, jede
/// Abweichung fuehrte deshalb zu einer EConvertError statt zu IsValid = false.
/// </summary>
procedure TZUGFeRDInvoiceValidatorTests.TestValidationDoesNotRaiseOnDeviation;
var
  desc: TZUGFeRDInvoiceDescriptor;
  validationResult: TZUGFeRDValidationResult;
begin
  desc := CreateBalancedInvoice(False);
  try
    // saemtliche Summen verfaelschen, damit jeder Meldungszweig durchlaufen wird
    desc.TaxTotalAmount := 1;
    desc.LineTotalAmount := 2;
    desc.GrandTotalAmount := 3;
    desc.ChargeTotalAmount := 4;
    desc.AllowanceTotalAmount := 5;

    validationResult := nil;
    Assert.WillNotRaiseAny(
      procedure
      begin
        validationResult := TZUGFeRDInvoiceValidator.Validate(desc, TZUGFeRDVersion.Version23);
      end,
      'Der Validator wirft eine Exception, statt die Abweichung zu melden');
    try
      Assert.IsFalse(validationResult.IsValid);
    finally
      validationResult.Free;
    end;
  finally
    desc.Free;
  end;
end;

end.
