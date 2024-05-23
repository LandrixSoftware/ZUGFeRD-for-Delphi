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

unit intf.ZUGFeRDTradeAllowanceCharge;

interface

uses intf.ZUGFeRDCharge,intf.ZUGFeRDCurrencyCodes;

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
  TZUGFeRDTradeAllowanceCharge = class(TZUGFeRDCharge)
  private
    FChargeIndicator: Boolean;
    FReason: string;
    FBasisAmount: Currency;
    FCurrency: TZUGFeRDCurrencyCodes;
    FActualAmount: Currency;
    FChargePercentage: Currency;
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
    property BasisAmount: Currency read FBasisAmount write FBasisAmount;
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
    property ChargePercentage : Currency read FChargePercentage write FChargePercentage;
  end;

implementation

end.
