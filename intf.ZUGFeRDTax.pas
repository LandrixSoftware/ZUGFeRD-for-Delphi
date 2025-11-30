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

unit intf.ZUGFeRDTax;

interface

uses
  System.SysUtils,System.Math,
  intf.ZUGFeRDTaxTypes,
  intf.ZUGFeRDTaxCategoryCodes,
  intf.ZUGFeRDTaxExemptionReasonCodes,
  intf.ZUGFeRDDateTypeCodes,
  intf.ZUGFeRDHelper;

type
  /// <summary>
  /// Structure for holding tax information (generally applicable trade tax)
  /// </summary>
  TZUGFeRDTax = class
  private
    FBasisAmount: Currency;
    FPercent: Currency;
    FTypeCode: ZUGFeRDNullable<TZUGFeRDTaxTypes>;
    FCategoryCode: ZUGFeRDNullable<TZUGFeRDTaxCategoryCodes>;
    FAllowanceChargeBasisAmount: ZUGFeRDNullable<Currency>;
    FLineTotalBasisAmount: ZUGFeRDNullable<Currency>;
    FExemptionReasonCode: ZUGFeRDNullable<TZUGFeRDTaxExemptionReasonCodes>;
    FExemptionReason: string;
    FTaxAmount: Currency;
    FTaxPointDate: ZUGFeRDNullable<TDateTime>;
    FDueDateTypeCode: ZUGFeRDNullable<TZUGFeRDDateTypeCodes>;
  public
    /// <summary>
    /// Returns the amount of the tax (Percent * BasisAmount)
    /// </summary>
    property TaxAmount: Currency read FTaxAmount write FTaxAmount;
    /// <summary>
    /// VAT category taxable amount
    /// </summary>
    property BasisAmount: Currency read FBasisAmount write FBasisAmount;
    /// <summary>
    /// Tax rate
    /// </summary>
    property Percent: Currency read FPercent write FPercent;
    /// <summary>
    /// Type of tax.
    ///
    /// Generally, the fixed value is: "VAT"
    /// </summary>
    property TypeCode: ZUGFeRDNullable<TZUGFeRDTaxTypes> read FTypeCode write FTypeCode;
    /// <summary>
    /// The code valid for the invoiced goods sales tax category.
    /// </summary>
    property CategoryCode: ZUGFeRDNullable<TZUGFeRDTaxCategoryCodes> read FCategoryCode write FCategoryCode;
    /// <summary>
    /// Total amount of charges / allowances on document level
    /// </summary>
    property AllowanceChargeBasisAmount: ZUGFeRDNullable<Currency> read FAllowanceChargeBasisAmount write FAllowanceChargeBasisAmount;
    /// <summary>
    /// A monetary value used as the line total basis on which this trade related tax, levy or duty is calculated
    /// </summary>
    property LineTotalBasisAmount: ZUGFeRDNullable<Currency> read FLineTotalBasisAmount write FLineTotalBasisAmount;
    /// <summary>
    /// ExemptionReasonCode for no Tax
    /// </summary>
    property ExemptionReasonCode: ZUGFeRDNullable<TZUGFeRDTaxExemptionReasonCodes> read FExemptionReasonCode write FExemptionReasonCode;
    /// <summary>
    /// Exemption Reason Text for no Tax
    /// </summary>
    property ExemptionReason: string read FExemptionReason write FExemptionReason;

    /// <summary>
    /// Value added tax point date
    /// The date when the VAT becomes accountable for the Seller and for the Buyer in so far as that date can be determined and differs from the date of issue of the invoice, according to the VAT directive.
    ///
    /// Please note that TaxPointDate and <see cref="DueDateTypeCode"/> are mutually exclusive. In case both are set, only TaxPointDate is serialized.
    /// </summary>
    property TaxPointDate: ZUGFeRDNullable<TDateTime> read  FTaxPointDate write FTaxPointDate;

    /// <summary>
    /// Value added tax point date code
    /// The code of the date when the VAT becomes accountable for the Seller and for the Buyer.
    ///
    /// Please note that DueDateTypeCode and <see cref="TaxPointDate"/> are mutually exclusive. In case both are set, only TaxPointDate is serialized.
    /// </summary>
    property DueDateTypeCode: ZUGFeRDNullable<TZUGFeRDDateTypeCodes> read FDueDateTypeCode write FDueDateTypeCode;

  public
    constructor Create;

    /// <summary>
    /// The tax point is usually the date goods were supplied or services completed (the 'basic tax point'). There are
    /// some variations.Please refer to Article 226 (7) of the Council Directive 2006/112/EC[2] for more information.
    /// This element is required if the Value added tax point date is different from the Invoice issue date.
    /// Both Buyer and Seller should use the Tax Point Date when provided by the Seller.The use of BT-7 and BT-8 is
    /// mutually exclusive.
    ///
    /// BT-7
    ///
    /// Use: The date when the VAT becomes accountable for the Seller and for the Buyer in so far as that date can be
    /// determined and differs from the date of issue of the invoice, according to the VAT directive.
    ///
    /// Please note that taxPointDate and dueDateTypeCode are mutually exclusive. In case both are set, only TaxPointDate is serialized.
    /// </summary>
    /// <param name="taxPointDate">Value added tax point date</param>
    /// <param name="dueDateTypeCode">Value added tax point date code</param>
    procedure SetTaxPointDate (taxPointDate: IZUGFeRDNullableParam<TDateTime> = Nil; dueDateTypeCode: IZUGFeRDNullableParam<TZUGFeRDDateTypeCodes> = Nil);
  end;

implementation

{ TZUGFeRDTax }
constructor TZUGFeRDTax.Create;
begin
  inherited Create;
  FBasisAmount := 0;
  FPercent := 0;
  FExemptionReason := '';
  FTaxAmount := 0;
end;

procedure TZUGFeRDTax.SetTaxPointDate(taxPointDate: IZUGFeRDNullableParam<TDateTime>; dueDateTypeCode: IZUGFeRDNullableParam<TZUGFeRDDateTypeCodes>);
begin
  Self.TaxPointDate:= taxPointDate;
  Self.DueDateTypeCode:= dueDateTypeCode;
end;

end.
