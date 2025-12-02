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

// implements AbstractTradeAllowanceCharge from C#
// implements TradeCharge from C#
// implements TradeAllowance from C#

unit intf.ZUGFeRDTradeAllowanceCharge;
interface

uses
  intf.ZUGFeRDHelper,
  intf.ZUGFeRDCharge,
  intf.ZUGFeRDCurrencyCodes,
  intf.ZUGFeRDAllowanceReasonCodes,
  intf.ZUGFeRDChargeReasonCodes;

type
  /// <summary>
  /// Zu- und Abschlag
  ///
  /// Beispiel:
  /// <SpecifiedTradeAllowanceCharge>
  ///   <ChargeIndicator>false</ChargeIndicator>
  ///      <BasisAmount currencyID="EUR">137.30</BasisAmount>
  ///      <ActualAmount>13.73</ActualAmount>
  ///      <Reason>Sondernachlass</Reason>
  ///      <CategoryTradeTax>
  ///        <TypeCode>VAT</TypeCode>
  ///        <CategoryCode>S</CategoryCode>
  ///        <ApplicablePercent>7</ApplicablePercent>
  ///      </CategoryTradeTax>
  ///    </SpecifiedTradeAllowanceCharge>
  /// </summary>
  TZUGFeRDAbstractTradeAllowanceCharge = class(TZUGFeRDCharge)
  private
    FChargeIndicator: Boolean;
    FReason: string;
    FBasisAmount: ZUGFeRDNullable<Currency>;
    FCurrency: TZUGFeRDCurrencyCodes;
    FActualAmount: Currency;
    FChargePercentage: ZUGFeRDNullable<Currency>;
  public
    constructor Create;
  public
    /// <summary>
    /// Switch for discount and surcharge
    ///
    /// false: Discount
    /// true: Surcharge
    ///
    /// In case of a discount (BG-27) the value of the ChargeIndicators has to be "false". In case of a surcharge (BG-28) the value of the ChargeIndicators has to be "true".
    /// </summary>
    property ChargeIndicator: Boolean read FChargeIndicator write FChargeIndicator;

    /// <summary>
    /// The reason for the surcharge or discount in written form
    /// </summary>
    property Reason: string read FReason write FReason;
    /// <summary>
    /// The base amount that may be used in conjunction with the percentage of the invoice line discount to calculate the amount of the invoice line discount
    /// </summary>
    property BasisAmount: ZUGFeRDNullable<Currency> read FBasisAmount write FBasisAmount;
    /// <summary>
    /// Currency that is used for representing BasisAmount and ActualAmount
    /// </summary>
    property Currency: TZUGFeRDCurrencyCodes read FCurrency write FCurrency;
    /// <summary>
    /// The amount of the discount / surcharge or discount without VAT
    /// </summary>
    property ActualAmount: Currency read FActualAmount write FActualAmount;
    /// <summary>
    /// The percentage that may be used in conjunction with the document level discount base amount, to calculate the
    /// document level discount amount.
    /// BT-101
    /// </summary>
    property ChargePercentage : ZUGFeRDNullable<Currency> read FChargePercentage write FChargePercentage;
  end;

  TZUGFeRDTradeAllowance = class(TZUGFeRDAbstractTradeAllowanceCharge)
  private
    FReasonCode: ZUGFeRDNullable<TZUGFeRDAllowanceReasonCodes>;
  public
    constructor Create;

    /// <summary>
    /// The reason code for the surcharge or discount
    /// </summary>
    property ReasonCode: ZUGFeRDNullable<TZUGFeRDAllowanceReasonCodes> read FReasonCode write FReasonCode;
  end;

  TZUGFeRDTradeCharge = class(TZUGFeRDAbstractTradeAllowanceCharge)
  private
    FReasonCode: ZUGFeRDNullable<TZUGFeRDChargeReasonCodes>;
  public
    constructor Create;

    /// <summary>
    /// The reason code for the surcharge or discount
    /// </summary>
    property ReasonCode: ZUGFeRDNullable<TZUGFeRDChargeReasonCodes> read FReasonCode write FReasonCode;
  end;

implementation

{ TZUGFeRDAbstractTradeAllowanceCharge }

constructor TZUGFeRDAbstractTradeAllowanceCharge.Create;
begin
  inherited Create;
  FChargeIndicator:= false;
  FReason:= '';
  FBasisAmount:= 0;
  FCurrency:= TZUGFeRDCurrencyCodes.Unknown;
  FActualAmount:= 0;
  FChargePercentage:= 0;
end;

{ TZUGFeRDTradeAllowance }

constructor TZUGFeRDTradeAllowance.Create;
begin
  inherited Create;
  FChargeIndicator:= false;
end;

{ TZUGFeRDTradeCharge }

constructor TZUGFeRDTradeCharge.Create;
begin
  inherited Create;
  FChargeIndicator:= true;
end;

end.
