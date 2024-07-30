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

unit intf.ZUGFeRDTradeCurrencyExchange;

interface

uses
  intf.ZUGFeRDCurrencyCodes,
  intf.ZUGFeRDHelper
  ;

type
  /// <summary>
	/// Specification of the invoice currency, local currency and exchange rate
	/// </summary>
	TZUGFeRDTradeCurrencyExchange = class
  private
    FSourceCurrency: TZUGFeRDCurrencyCodes;
    FConversionRateTimestamp: ZUGFeRDNullable<TDateTime>;
    FTargetCurrency: TZUGFeRDCurrencyCodes;
    FConversionRate: double;
  public
		/// <summary>
		/// Invoice currency
		/// </summary>
		property SourceCurrency : TZUGFeRDCurrencyCodes read FSourceCurrency write FSourceCurrency;

		/// <summary>
		/// Local currency
		/// </summary>
		property TargetCurrency : TZUGFeRDCurrencyCodes read FTargetCurrency write FTargetCurrency;

		/// <summary>
		/// Exchange rate
		/// </summary>
		property ConversionRate : double read FConversionRate write FConversionRate;

		/// <summary>
		/// Exchange rate date
		/// </summary>
		property ConversionRateTimestamp : ZUGFeRDNullable<TDateTime> read FConversionRateTimestamp write FConversionRateTimestamp;

		constructor Create(_sourceCurrency : TZUGFeRDCurrencyCodes; _targetCurrency : TZUGFeRDCurrencyCodes; _conversionRate : double); overload;
		constructor Create(_sourceCurrency : TZUGFeRDCurrencyCodes; _targetCurrency : TZUGFeRDCurrencyCodes; _conversionRate : double; _conversionRateTimestamp : TDateTime); overload;
	end;

implementation

{ TZUGFeRDTradeCurrencyExchange }

constructor TZUGFeRDTradeCurrencyExchange.Create(_sourceCurrency,
  _targetCurrency: TZUGFeRDCurrencyCodes; _conversionRate: double;
  _conversionRateTimestamp: TDateTime);
begin
  SourceCurrency := _sourceCurrency;
  TargetCurrency := _targetCurrency;
  ConversionRate := _conversionRate;
  ConversionRateTimestamp := _conversionRateTimestamp;
end;

constructor TZUGFeRDTradeCurrencyExchange.Create(_sourceCurrency,
  _targetCurrency: TZUGFeRDCurrencyCodes; _conversionRate: double);
begin
  SourceCurrency := _sourceCurrency;
  TargetCurrency := _targetCurrency;
  ConversionRate := _conversionRate;
  ConversionRateTimestamp.ClearValue;
end;

end.
