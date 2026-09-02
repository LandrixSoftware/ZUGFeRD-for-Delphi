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
  intf.ZUGFeRDTaxCategoryCodes,
  intf.ZUGFeRDTestBase;

type
  [TestFixture]
  TZUGFeRDInvoiceValidatorTests = class(TZUGFeRDTestBase)
  private
    /// <summary>
    /// Baut eine Rechnung mit einer Position (2 x 100,00 zu 19%) und optionalen
    /// Zu- und Abschlägen, Vorauszahlung und Rundung. Die Summen im Descriptor
    /// sind passend gesetzt, der Validator muss die Rechnung also als gueltig ansehen.
    /// </summary>
    /// <summary>
    /// Baut eine Rechnung, deren Nettoeinzelpreis sich auf eine Preisbasismenge
    /// bezieht (BT-149).
    /// </summary>
    function CreateInvoiceWithPriceBaseQuantity(const unitQuantity: Currency): TZUGFeRDInvoiceDescriptor;
    function CreateBalancedInvoice(const withCharge: Boolean;
      const lineAllowance: Currency = 0; const lineCharge: Currency = 0;
      const prepaidAmount: Currency = 0; const roundingAmount: Currency = 0): TZUGFeRDInvoiceDescriptor;
    /// <summary>
    /// Ergänzt eine Rechnungsposition und die zugehörige Steueraufschlüsselung.
    /// </summary>
    procedure AddTaxGroup(Descriptor: TZUGFeRDInvoiceDescriptor; const Name: string;
      const BasisAmount, TaxPercent, TaxAmount: Currency; const CategoryCode: TZUGFeRDTaxCategoryCodes);
  public
    [Test]
    procedure TestValidInvoiceWithoutAllowanceOrCharge;
    [Test]
    procedure TestValidInvoiceWithCharge;
    [Test]
    procedure TestRecalculatedTaxBasisMessageIncludesCharge;
    [Test]
    procedure TestValidInvoiceWithLineAllowance;
    [Test]
    procedure TestValidInvoiceWithLineCharge;
    [Test]
    procedure TestPriceAllowanceIsNotSubtractedTwice;
    [Test]
    procedure TestValidInvoiceWithPrepaidAmount;
    [Test]
    procedure TestValidInvoiceWithRoundingAmount;
    [Test]
    procedure TestMissingDuePayableAmountIsReported;
    [Test]
    procedure TestInvalidDuePayableAmountIsReported;
    [Test]
    procedure TestDuePayableUsesDeclaredGrandTotalAmount;
    [Test]
    procedure TestInvalidTaxTotalIsReported;
    [Test]
    procedure TestTaxAmountsAreRoundedPerTaxGroup;
    [Test]
    procedure TestUnroundedTaxAmountIsReported;
    [Test]
    procedure TestTaxAmountWithinBRCO17ToleranceIsAccepted;
    [Test]
    procedure TestTaxAmountBeyondBRCO17ToleranceIsReported;
    [Test]
    procedure TestTaxAmountDeviationsWithinSameRateAreReported;
    [Test]
    procedure TestNegativeMidpointTaxAmountIsRoundedAwayFromZero;
    [Test]
    procedure TestNonVatTaxDoesNotAffectVatTotals;
    [Test]
    procedure TestMissingTaxTypeIsReported;
    [Test]
    procedure TestMissingTaxBasisAmountIsReported;
    [Test]
    procedure TestInvalidTaxBasisAmountIsReported;
    [Test]
    procedure TestPriceBaseQuantityIsApplied;
    [Test]
    procedure TestZeroPriceBaseQuantityIsReported;
    [Test]
    procedure TestConsistentlyWrongTaxBasisIsReported;
    [Test]
    procedure TestDeclaredAllowanceTotalDeviationIsReported;
    [Test]
    procedure TestDeclaredChargeTotalDeviationIsReported;
    [Test]
    procedure TestMissingLineTotalAmountIsReported;
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
  intf.ZUGFeRDTradeLineItem,
  intf.ZUGFeRDHelper;

{ TZUGFeRDInvoiceValidatorTests }

procedure TZUGFeRDInvoiceValidatorTests.AddTaxGroup(
  Descriptor: TZUGFeRDInvoiceDescriptor; const Name: string;
  const BasisAmount, TaxPercent, TaxAmount: Currency;
  const CategoryCode: TZUGFeRDTaxCategoryCodes);
begin
  Descriptor.AddTradeLineItem(
    {name=}            Name,
    {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(BasisAmount),
    {description=}     '',
    {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.H87),
    {unitQuantity=}    nil,
    {grossUnitPrice=}  nil,
    {billedQuantity=}  1,
    {lineTotalAmount=} BasisAmount,
    {taxType=}         TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
    {categoryCode=}    TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(CategoryCode),
    {taxPercent=}      TaxPercent);
  Descriptor.AddApplicableTradeTax({calculatedAmount=} TaxAmount, {basisAmount=} BasisAmount,
    {percent=} TaxPercent, TZUGFeRDTaxTypes.VAT, CategoryCode);
end;

function TZUGFeRDInvoiceValidatorTests.CreateBalancedInvoice(
  const withCharge: Boolean; const lineAllowance, lineCharge, prepaidAmount,
  roundingAmount: Currency): TZUGFeRDInvoiceDescriptor;
var
  lineTotal, chargeTotal, taxBasis, taxTotal, grandTotal: Currency;
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
  grandTotal := taxBasis + taxTotal;

  Result.AddApplicableTradeTax({calculatedAmount=} taxTotal, {basisAmount=} taxBasis,
    {percent=} 19.0, TZUGFeRDTaxTypes.VAT, TZUGFeRDTaxCategoryCodes.S);

  Result.SetTotals(
    {aLineTotalAmount=}      lineTotal,
    {aChargeTotalAmount=}    chargeTotal,
    {aAllowanceTotalAmount=} 0,
    {aTaxBasisAmount=}       taxBasis,
    {aTaxTotalAmount=}       taxTotal,
    {aGrandTotalAmount=}     grandTotal,
    {aTotalPrepaidAmount=}   prepaidAmount,
    {aDuePayableAmount=}     grandTotal - prepaidAmount + roundingAmount,
    {aRoundingAmount=}       roundingAmount);
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

procedure TZUGFeRDInvoiceValidatorTests.TestValidInvoiceWithPrepaidAmount;
var
  desc: TZUGFeRDInvoiceDescriptor;
  validationResult: TZUGFeRDValidationResult;
begin
  desc := CreateBalancedInvoice(False, {lineAllowance=} 0, {lineCharge=} 0,
    {prepaidAmount=} 50);
  try
    validationResult := TZUGFeRDInvoiceValidator.Validate(desc, TZUGFeRDVersion.Version23);
    try
      Assert.IsTrue(validationResult.IsValid,
        'Rechnung mit Vorauszahlung wurde als ungueltig gemeldet:'#13#10 + validationResult.Messages.Text);
    finally
      validationResult.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDInvoiceValidatorTests.TestValidInvoiceWithRoundingAmount;
var
  desc: TZUGFeRDInvoiceDescriptor;
  validationResult: TZUGFeRDValidationResult;
begin
  desc := CreateBalancedInvoice(False, {lineAllowance=} 0, {lineCharge=} 0,
    {prepaidAmount=} 0, {roundingAmount=} 0.05);
  try
    validationResult := TZUGFeRDInvoiceValidator.Validate(desc, TZUGFeRDVersion.Version23);
    try
      Assert.IsTrue(validationResult.IsValid,
        'Rechnung mit Rundungsbetrag wurde als ungueltig gemeldet:'#13#10 + validationResult.Messages.Text);
    finally
      validationResult.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDInvoiceValidatorTests.TestMissingDuePayableAmountIsReported;
var
  desc: TZUGFeRDInvoiceDescriptor;
  validationResult: TZUGFeRDValidationResult;
begin
  desc := CreateBalancedInvoice(False);
  try
    desc.DuePayableAmount := ZUGFeRDNullable<Currency>.Create(False);

    validationResult := TZUGFeRDInvoiceValidator.Validate(desc, TZUGFeRDVersion.Version23);
    try
      Assert.IsFalse(validationResult.IsValid, 'Fehlender Zahlbetrag wurde nicht beanstandet');
      Assert.IsTrue(validationResult.Messages.Text.Contains('Kein DuePayableAmount vorhanden'),
        'Die Meldungen benennen den fehlenden Zahlbetrag nicht');
    finally
      validationResult.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDInvoiceValidatorTests.TestInvalidDuePayableAmountIsReported;
var
  desc: TZUGFeRDInvoiceDescriptor;
  validationResult: TZUGFeRDValidationResult;
begin
  desc := CreateBalancedInvoice(False, {lineAllowance=} 0, {lineCharge=} 0,
    {prepaidAmount=} 50, {roundingAmount=} 0.05);
  try
    desc.DuePayableAmount := desc.GrandTotalAmount.Value;

    validationResult := TZUGFeRDInvoiceValidator.Validate(desc, TZUGFeRDVersion.Version23);
    try
      Assert.IsFalse(validationResult.IsValid, 'Abweichender Zahlbetrag wurde nicht beanstandet');
      Assert.IsTrue(validationResult.Messages.Text.Contains('duePayable'),
        'Die Meldungen benennen den abweichenden Zahlbetrag nicht');
    finally
      validationResult.Free;
    end;
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDInvoiceValidatorTests.TestDuePayableUsesDeclaredGrandTotalAmount;
var
  desc: TZUGFeRDInvoiceDescriptor;
  validationResult: TZUGFeRDValidationResult;
begin
  desc := CreateBalancedInvoice(False, {lineAllowance=} 0, {lineCharge=} 0,
    {prepaidAmount=} 50);
  try
    // BR-CO-16 verwendet den deklarierten BT-112, auch wenn dessen eigene Nachrechnung abweicht.
    desc.GrandTotalAmount := desc.GrandTotalAmount.Value + 1;

    validationResult := TZUGFeRDInvoiceValidator.Validate(desc, TZUGFeRDVersion.Version23);
    try
      Assert.IsFalse(validationResult.IsValid, 'Abweichende Summen wurden nicht beanstandet');
      Assert.IsTrue(validationResult.Messages.Text.Contains(
        'monetarySummation.duePayable Message: Berechneter Wert ist['),
        'BT-115 wurde nicht gegen den deklarierten BT-112 geprueft');
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

/// <summary>
/// Die protokollierte Neuberechnung von BT-109 muss Kopfzuschläge ebenso
/// berücksichtigen wie die eigentliche Summenberechnung.
/// </summary>
procedure TZUGFeRDInvoiceValidatorTests.TestRecalculatedTaxBasisMessageIncludesCharge;
var
  desc: TZUGFeRDInvoiceDescriptor;
  validationResult: TZUGFeRDValidationResult;
  expectedMessage: string;
begin
  desc := CreateBalancedInvoice(True);
  try
    validationResult := TZUGFeRDInvoiceValidator.Validate(desc, TZUGFeRDVersion.Version23);
    try
      expectedMessage := Format('Recalculated tax basis = %f', [210.0]);
      Assert.IsTrue(validationResult.Messages.IndexOf(expectedMessage) >= 0,
        'Die protokollierte Steuerbasis enthält den Kopfzuschlag nicht:'#13#10 +
        validationResult.Messages.Text);
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

procedure TZUGFeRDInvoiceValidatorTests.TestTaxAmountsAreRoundedPerTaxGroup;
var
  Descriptor: TZUGFeRDInvoiceDescriptor;
  ValidationResult: TZUGFeRDValidationResult;
begin
  Descriptor := TZUGFeRDInvoiceDescriptor.CreateInvoice('RE-ROUND-GROUPS', EncodeDate(2026, 1, 15), TZUGFeRDCurrencyCodes.EUR);
  try
    // BR-CO-17 rundet jede Steuergruppe vor der Summierung auf zwei Dezimalstellen.
    AddTaxGroup(Descriptor, 'Group 19', 0.03, 19, 0.01, TZUGFeRDTaxCategoryCodes.S);
    AddTaxGroup(Descriptor, 'Group 7', 0.08, 7, 0.01, TZUGFeRDTaxCategoryCodes.S);
    AddTaxGroup(Descriptor, 'Group 5', 0.11, 5, 0.01, TZUGFeRDTaxCategoryCodes.S);
    Descriptor.SetTotals(0.22, 0, 0, 0.22, 0.03, 0.25, 0, 0.25);

    ValidationResult := TZUGFeRDInvoiceValidator.Validate(Descriptor, TZUGFeRDVersion.Version23);
    try
      Assert.IsTrue(ValidationResult.IsValid,
        'Tax amounts rounded per group were rejected:'#13#10 + ValidationResult.Messages.Text);
    finally
      FreeAndNil(ValidationResult);
    end;
  finally
    FreeAndNil(Descriptor);
  end;
end;

procedure TZUGFeRDInvoiceValidatorTests.TestUnroundedTaxAmountIsReported;
var
  Descriptor: TZUGFeRDInvoiceDescriptor;
  ValidationResult: TZUGFeRDValidationResult;
begin
  Descriptor := TZUGFeRDInvoiceDescriptor.CreateInvoice('RE-ROUND-INVALID', EncodeDate(2026, 1, 15), TZUGFeRDCurrencyCodes.EUR);
  try
    AddTaxGroup(Descriptor, 'Unrounded group', 0.03, 19, 0.0057, TZUGFeRDTaxCategoryCodes.S);
    Descriptor.SetTotals(0.03, 0, 0, 0.03, 0.0057, 0.0357, 0, 0.0357);

    ValidationResult := TZUGFeRDInvoiceValidator.Validate(Descriptor, TZUGFeRDVersion.Version23);
    try
      Assert.IsFalse(ValidationResult.IsValid, 'Unrounded tax amount was not reported');
      // 0,0057 liegt innerhalb der BR-CO-17-Toleranz; beanstandet wird die
      // unzulässige Zahl der Nachkommastellen.
      Assert.IsTrue(ValidationResult.Messages.Text.Contains('BR-DEC-20'),
        'Validation messages do not identify BR-DEC-20');
    finally
      FreeAndNil(ValidationResult);
    end;
  finally
    FreeAndNil(Descriptor);
  end;
end;

procedure TZUGFeRDInvoiceValidatorTests.TestTaxAmountDeviationsWithinSameRateAreReported;
var
  Descriptor: TZUGFeRDInvoiceDescriptor;
  ValidationResult: TZUGFeRDValidationResult;
  Message: string;
  BRCO17MessageCount: Integer;
begin
  Descriptor := TZUGFeRDInvoiceDescriptor.CreateInvoice('RE-ROUND-CATEGORY', EncodeDate(2026, 1, 15), TZUGFeRDCurrencyCodes.EUR);
  try
    // Gegensätzliche Abweichungen dürfen sich bei gleichem Steuersatz nicht gegenseitig
    // aufheben. Beide liegen außerhalb der BR-CO-17-Toleranz, ihre Summe ergibt aber
    // exakt den korrekten Gesamtbetrag von 14,00.
    AddTaxGroup(Descriptor, 'Standard rate', 100, 7, 9.00, TZUGFeRDTaxCategoryCodes.S);
    AddTaxGroup(Descriptor, 'Lower rate', 100, 7, 5.00, TZUGFeRDTaxCategoryCodes.AA);
    Descriptor.SetTotals(200, 0, 0, 200, 14, 214, 0, 214);

    ValidationResult := TZUGFeRDInvoiceValidator.Validate(Descriptor, TZUGFeRDVersion.Version23);
    try
      Assert.IsFalse(ValidationResult.IsValid, 'Tax amount deviations within one rate were not reported');
      BRCO17MessageCount := 0;
      for Message in ValidationResult.Messages do
        if Message.Contains('BR-CO-17') then
          Inc(BRCO17MessageCount);
      Assert.AreEqual(2, BRCO17MessageCount, 'Not every tax group was validated separately');
    finally
      FreeAndNil(ValidationResult);
    end;
  finally
    FreeAndNil(Descriptor);
  end;
end;

procedure TZUGFeRDInvoiceValidatorTests.TestNegativeMidpointTaxAmountIsRoundedAwayFromZero;
var
  Descriptor: TZUGFeRDInvoiceDescriptor;
  ValidationResult: TZUGFeRDValidationResult;
begin
  Descriptor := TZUGFeRDInvoiceDescriptor.CreateInvoice('RE-ROUND-NEGATIVE', EncodeDate(2026, 1, 15), TZUGFeRDCurrencyCodes.EUR);
  try
    // Der Rundungsmodus entspricht der Betragsformatierung der Writer und rundet Mittelpunkte von null weg.
    AddTaxGroup(Descriptor, 'Negative midpoint', -0.025, 20, -0.01, TZUGFeRDTaxCategoryCodes.S);
    Descriptor.SetTotals(-0.025, 0, 0, -0.025, -0.01, -0.035, 0, -0.035);

    ValidationResult := TZUGFeRDInvoiceValidator.Validate(Descriptor, TZUGFeRDVersion.Version23);
    try
      Assert.IsTrue(ValidationResult.IsValid,
        'Negative midpoint tax amount was not rounded away from zero:'#13#10 + ValidationResult.Messages.Text);
    finally
      FreeAndNil(ValidationResult);
    end;
  finally
    FreeAndNil(Descriptor);
  end;
end;

procedure TZUGFeRDInvoiceValidatorTests.TestNonVatTaxDoesNotAffectVatTotals;
var
  Descriptor: TZUGFeRDInvoiceDescriptor;
  ValidationResult: TZUGFeRDValidationResult;
begin
  Descriptor := CreateBalancedInvoice(False);
  try
    // Zusätzliche Extended-Steuern gehören weder zu BT-110 noch zur Summe der VAT-Bemessungsgrundlagen.
    Descriptor.AddApplicableTradeTax({calculatedAmount=} 10, {basisAmount=} 200,
      {percent=} 5, TZUGFeRDTaxTypes.AAA, TZUGFeRDTaxCategoryCodes.S);

    ValidationResult := TZUGFeRDInvoiceValidator.Validate(Descriptor, TZUGFeRDVersion.Version23);
    try
      Assert.IsTrue(ValidationResult.IsValid,
        'Non-VAT tax affected VAT totals:'#13#10 + ValidationResult.Messages.Text);
    finally
      FreeAndNil(ValidationResult);
    end;
  finally
    FreeAndNil(Descriptor);
  end;
end;

procedure TZUGFeRDInvoiceValidatorTests.TestMissingTaxTypeIsReported;
var
  Descriptor: TZUGFeRDInvoiceDescriptor;
  ValidationResult: TZUGFeRDValidationResult;
begin
  Descriptor := CreateBalancedInvoice(False);
  try
    Descriptor.Taxes[0].TypeCode := ZUGFeRDNullable<TZUGFeRDTaxTypes>.Create(False);

    ValidationResult := TZUGFeRDInvoiceValidator.Validate(Descriptor, TZUGFeRDVersion.Version23);
    try
      Assert.IsFalse(ValidationResult.IsValid, 'Missing tax type was not reported');
      Assert.IsTrue(ValidationResult.Messages.Text.Contains('Tax type code is required'),
        'Validation messages do not identify the missing tax type');
    finally
      FreeAndNil(ValidationResult);
    end;
  finally
    FreeAndNil(Descriptor);
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

/// <summary>
/// BR-CO-13: BT-109 muss sich aus BT-106 - BT-107 + BT-108 ergeben. Stimmen BT-109
/// und die Steueraufschlüsselung (BT-116) untereinander überein, weichen aber
/// gemeinsam vom Positionsnetto ab, greift keine der übrigen Prüfungen: die
/// Bruttosumme wird aus dem nachgerechneten Positionsnetto gebildet und passt
/// dann ebenfalls.
/// </summary>
procedure TZUGFeRDInvoiceValidatorTests.TestConsistentlyWrongTaxBasisIsReported;
var
  desc: TZUGFeRDInvoiceDescriptor;
  validationResult: TZUGFeRDValidationResult;
begin
  desc := TZUGFeRDInvoiceDescriptor.CreateInvoice('RE-4711', EncodeDate(2026, 1, 15),
    TZUGFeRDCurrencyCodes.EUR);
  try
    desc.AddTradeLineItem(
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
      {taxPercent=}      19.0);

    // BT-116 und BT-109 sind zueinander konsistent, aber um 10,00 zu niedrig.
    desc.AddApplicableTradeTax({calculatedAmount=} 36.10, {basisAmount=} 190,
      {percent=} 19.0, TZUGFeRDTaxTypes.VAT, TZUGFeRDTaxCategoryCodes.S);

    // BT-112 passt zur Nachrechnung aus BT-106 und BT-110, nicht zu BT-109.
    desc.SetTotals(
      {aLineTotalAmount=}      200,
      {aChargeTotalAmount=}    0,
      {aAllowanceTotalAmount=} 0,
      {aTaxBasisAmount=}       190,
      {aTaxTotalAmount=}       36.10,
      {aGrandTotalAmount=}     236.10,
      {aTotalPrepaidAmount=}   0,
      {aDuePayableAmount=}     236.10);

    validationResult := TZUGFeRDInvoiceValidator.Validate(desc, TZUGFeRDVersion.Version23);
    try
      Assert.IsFalse(validationResult.IsValid,
        'Ein von BT-106 abweichendes BT-109 wurde nicht beanstandet:'#13#10 +
        validationResult.Messages.Text);
      Assert.IsTrue(Pos('BR-CO-13', validationResult.Messages.Text) > 0,
        'Es fehlt die Meldung zu BR-CO-13:'#13#10 + validationResult.Messages.Text);
    finally
      validationResult.Free;
    end;
  finally
    desc.Free;
  end;
end;

/// <summary>
/// BR-CO-11: BT-107 muss der Summe der einzelnen Kopfabschläge (BT-92) entsprechen.
/// </summary>
procedure TZUGFeRDInvoiceValidatorTests.TestDeclaredAllowanceTotalDeviationIsReported;
var
  desc: TZUGFeRDInvoiceDescriptor;
  validationResult: TZUGFeRDValidationResult;
begin
  desc := CreateBalancedInvoice(False);
  try
    // Kopfabschläge gibt es keine, BT-107 behauptet aber 15,00.
    desc.AllowanceTotalAmount := 15;

    validationResult := TZUGFeRDInvoiceValidator.Validate(desc, TZUGFeRDVersion.Version23);
    try
      Assert.IsFalse(validationResult.IsValid,
        'Ein BT-107 ohne zugehörige Kopfabschläge wurde nicht beanstandet:'#13#10 +
        validationResult.Messages.Text);
      Assert.IsTrue(Pos('BR-CO-11', validationResult.Messages.Text) > 0,
        'Es fehlt die Meldung zu BR-CO-11:'#13#10 + validationResult.Messages.Text);
    finally
      validationResult.Free;
    end;
  finally
    desc.Free;
  end;
end;

/// <summary>
/// BR-CO-12: BT-108 muss der Summe der einzelnen Kopfzuschläge (BT-99) entsprechen.
/// </summary>
procedure TZUGFeRDInvoiceValidatorTests.TestDeclaredChargeTotalDeviationIsReported;
var
  desc: TZUGFeRDInvoiceDescriptor;
  validationResult: TZUGFeRDValidationResult;
begin
  desc := CreateBalancedInvoice(True);
  try
    // Der einzige Kopfzuschlag beträgt 10,00, BT-108 behauptet 20,00.
    desc.ChargeTotalAmount := 20;

    validationResult := TZUGFeRDInvoiceValidator.Validate(desc, TZUGFeRDVersion.Version23);
    try
      Assert.IsFalse(validationResult.IsValid,
        'Ein von den Kopfzuschlägen abweichendes BT-108 wurde nicht beanstandet:'#13#10 +
        validationResult.Messages.Text);
      Assert.IsTrue(Pos('BR-CO-12', validationResult.Messages.Text) > 0,
        'Es fehlt die Meldung zu BR-CO-12:'#13#10 + validationResult.Messages.Text);
    finally
      validationResult.Free;
    end;
  finally
    desc.Free;
  end;
end;

/// <summary>
/// Ein fehlendes BT-106 darf nicht stillschweigend als 0 gegen die Nachrechnung
/// gestellt werden, sondern muss als fehlende Pflichtangabe gemeldet werden.
/// </summary>
procedure TZUGFeRDInvoiceValidatorTests.TestMissingLineTotalAmountIsReported;
var
  desc: TZUGFeRDInvoiceDescriptor;
  validationResult: TZUGFeRDValidationResult;
begin
  desc := CreateBalancedInvoice(False);
  try
    desc.LineTotalAmount := nil;

    validationResult := TZUGFeRDInvoiceValidator.Validate(desc, TZUGFeRDVersion.Version23);
    try
      Assert.IsFalse(validationResult.IsValid,
        'Ein fehlendes BT-106 wurde nicht beanstandet:'#13#10 +
        validationResult.Messages.Text);
      Assert.IsTrue(Pos('Kein LineTotalAmount vorhanden', validationResult.Messages.Text) > 0,
        'Es fehlt die Meldung zum fehlenden BT-106:'#13#10 + validationResult.Messages.Text);
    finally
      validationResult.Free;
    end;
  finally
    desc.Free;
  end;
end;

/// <summary>
/// Baut eine ausgeglichene Rechnung, deren Nettoeinzelpreis BT-146 sich auf eine
/// Preisbasismenge BT-149 bezieht: 100,00 je 10 Stück bei 2 berechneten Stück
/// ergibt einen Positionsnettobetrag BT-131 von 20,00.
/// </summary>
function TZUGFeRDInvoiceValidatorTests.CreateInvoiceWithPriceBaseQuantity(
  const unitQuantity: Currency): TZUGFeRDInvoiceDescriptor;
begin
  Result := TZUGFeRDInvoiceDescriptor.CreateInvoice('RE-4711', EncodeDate(2026, 1, 15),
    TZUGFeRDCurrencyCodes.EUR);

  Result.AddTradeLineItem(
    {name=}            'Testartikel',
    {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(100),
    {description=}     '',
    {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.H87),
    {unitQuantity=}    TZUGFeRDNullableParam<Currency>.Create(unitQuantity),
    {grossUnitPrice=}  nil,
    {billedQuantity=}  2,
    {lineTotalAmount=} 20,
    {taxType=}         TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
    {categoryCode=}    TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
    {taxPercent=}      19.0);

  Result.AddApplicableTradeTax({calculatedAmount=} 3.80, {basisAmount=} 20,
    {percent=} 19.0, TZUGFeRDTaxTypes.VAT, TZUGFeRDTaxCategoryCodes.S);

  Result.SetTotals(
    {aLineTotalAmount=}      20,
    {aChargeTotalAmount=}    0,
    {aAllowanceTotalAmount=} 0,
    {aTaxBasisAmount=}       20,
    {aTaxTotalAmount=}       3.80,
    {aGrandTotalAmount=}     23.80,
    {aTotalPrepaidAmount=}   0,
    {aDuePayableAmount=}     23.80);
end;

/// <summary>
/// BT-146 gilt je Preisbasismenge BT-149. Wird BT-149 ignoriert, rechnet der
/// Validator das Zehnfache und verwirft eine gültige Rechnung.
/// </summary>
procedure TZUGFeRDInvoiceValidatorTests.TestPriceBaseQuantityIsApplied;
var
  desc: TZUGFeRDInvoiceDescriptor;
  validationResult: TZUGFeRDValidationResult;
begin
  desc := CreateInvoiceWithPriceBaseQuantity(10);
  try
    validationResult := TZUGFeRDInvoiceValidator.Validate(desc, TZUGFeRDVersion.Version23);
    try
      Assert.IsTrue(validationResult.IsValid,
        'Rechnung mit Preisbasismenge wurde als ungültig gemeldet:'#13#10 +
        validationResult.Messages.Text);
    finally
      validationResult.Free;
    end;
  finally
    desc.Free;
  end;
end;

/// <summary>
/// Eine Preisbasismenge von 0 ist fachlich unzulässig und darf nicht still
/// übergangen werden.
/// </summary>
procedure TZUGFeRDInvoiceValidatorTests.TestZeroPriceBaseQuantityIsReported;
var
  desc: TZUGFeRDInvoiceDescriptor;
  validationResult: TZUGFeRDValidationResult;
begin
  desc := CreateInvoiceWithPriceBaseQuantity(0);
  try
    validationResult := TZUGFeRDInvoiceValidator.Validate(desc, TZUGFeRDVersion.Version23);
    try
      Assert.IsFalse(validationResult.IsValid,
        'Eine Preisbasismenge von 0 wurde nicht beanstandet:'#13#10 +
        validationResult.Messages.Text);
      Assert.IsTrue(Pos('BT-149', validationResult.Messages.Text) > 0,
        'Es fehlt die Meldung zu BT-149:'#13#10 + validationResult.Messages.Text);
    finally
      validationResult.Free;
    end;
  finally
    desc.Free;
  end;
end;

/// <summary>
/// BR-CO-17 lässt laut EN-16931-Schematron eine Abweichung von einer
/// Währungseinheit je Steueraufschlüsselung zu. BT-110 ist dabei nach BR-CO-14
/// die Summe der angegebenen BT-117, nicht die der nachgerechneten Sollwerte -
/// sonst wäre eine normkonforme Rechnung allein an der Steuersumme ungültig.
/// </summary>
procedure TZUGFeRDInvoiceValidatorTests.TestTaxAmountWithinBRCO17ToleranceIsAccepted;
var
  Descriptor: TZUGFeRDInvoiceDescriptor;
  ValidationResult: TZUGFeRDValidationResult;
begin
  Descriptor := TZUGFeRDInvoiceDescriptor.CreateInvoice('RE-TOLERANCE-OK', EncodeDate(2026, 1, 15), TZUGFeRDCurrencyCodes.EUR);
  try
    // Sollwert 19,00, angegeben 19,50: Abweichung 0,50 und damit zulässig.
    AddTaxGroup(Descriptor, 'Standard rate', 100, 19, 19.50, TZUGFeRDTaxCategoryCodes.S);
    Descriptor.SetTotals(100, 0, 0, 100, 19.50, 119.50, 0, 119.50);

    ValidationResult := TZUGFeRDInvoiceValidator.Validate(Descriptor, TZUGFeRDVersion.Version23);
    try
      Assert.IsTrue(ValidationResult.IsValid,
        'Eine Abweichung innerhalb der BR-CO-17-Toleranz wurde als Fehler gemeldet:'#13#10 +
        ValidationResult.Messages.Text);
      Assert.IsTrue(ValidationResult.Messages.Text.Contains('BR-CO-17-Toleranz'),
        'Die zulässige Abweichung wurde nicht protokolliert:'#13#10 +
        ValidationResult.Messages.Text);
    finally
      ValidationResult.Free;
    end;
  finally
    Descriptor.Free;
  end;
end;

/// <summary>
/// Jenseits einer Währungseinheit ist BR-CO-17 verletzt.
/// </summary>
procedure TZUGFeRDInvoiceValidatorTests.TestTaxAmountBeyondBRCO17ToleranceIsReported;
var
  Descriptor: TZUGFeRDInvoiceDescriptor;
  ValidationResult: TZUGFeRDValidationResult;
begin
  Descriptor := TZUGFeRDInvoiceDescriptor.CreateInvoice('RE-TOLERANCE-FAIL', EncodeDate(2026, 1, 15), TZUGFeRDCurrencyCodes.EUR);
  try
    // Sollwert 19,00, angegeben 20,50: Abweichung 1,50 und damit unzulässig.
    AddTaxGroup(Descriptor, 'Standard rate', 100, 19, 20.50, TZUGFeRDTaxCategoryCodes.S);
    Descriptor.SetTotals(100, 0, 0, 100, 20.50, 120.50, 0, 120.50);

    ValidationResult := TZUGFeRDInvoiceValidator.Validate(Descriptor, TZUGFeRDVersion.Version23);
    try
      Assert.IsFalse(ValidationResult.IsValid,
        'Eine Abweichung jenseits der BR-CO-17-Toleranz wurde nicht beanstandet:'#13#10 +
        ValidationResult.Messages.Text);
      Assert.IsTrue(ValidationResult.Messages.Text.Contains('BR-CO-17:'),
        'Es fehlt die Meldung zu BR-CO-17:'#13#10 + ValidationResult.Messages.Text);
    finally
      ValidationResult.Free;
    end;
  finally
    Descriptor.Free;
  end;
end;

end.
