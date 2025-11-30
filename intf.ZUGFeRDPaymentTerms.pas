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

unit intf.ZUGFeRDPaymentTerms;

interface

uses
  intf.ZUGFeRDHelper,
  intf.ZUGFeRDQuantityCodes;

type

  TZUGFeRDPaymentTermsType =  (
    /// <summary>
    /// Skonto indicator
    /// </summary>
    Skonto,
    /// <summary>
    /// Verzug indicator
    /// </summary>
    Verzug
  );

  /// <summary>
  /// Condition that surrounds the payment part of an invoice, describing the specific details and the due date of the invoice.
  /// </summary>
  TZUGFeRDPaymentTerms = class
  private
    FDescription: string;
    FDueDate: ZUGFeRDNullable<TDateTime>;
    FPaymentTermsType: ZUGFeRDNullable<TZUGFeRDPaymentTermsType>;
    FDueDays: ZUGFeRDNullable<Integer>;
    FMaturityDate: ZUGFeRDNullable<TDateTime>;
    FPercentage: ZUGFeRDNullable<Currency>;
    FBaseAmount: ZUGFeRDNullable<Currency>;
    FActualAmount: ZUGFeRDNullable<Currency>;
    FPartialPaymentAmount: ZUGFeRDNullable<Currency>;
  public
    /// <summary>
    /// A textual description of the payment terms that apply to the amount due for payment (including description of possible penalties).
    /// </summary>
    property Description: string read FDescription write FDescription;
    /// <summary>
    /// The date when the payment is due
    /// </summary>
    property DueDate: ZUGFeRDNullable<TDateTime> read FDueDate write FDueDate;

    /// <summary>
    /// Type whether it's a discount or a surcharge / interest
    /// </summary>
    property PaymentTermsType: ZUGFeRDNullable<TZUGFeRDPaymentTermsType> read FPaymentTermsType write FPaymentTermsType;

    /// <summary>
    /// Number of days within terms are valid
    /// </summary>
    property DueDays: ZUGFeRDNullable<Integer> read FDueDays write FDueDays;

    /// <summary>
    /// Fälligkeitsdatum im Kontext der spezifischen Zahlungsbedingung
    /// </summary>
    /// BT-X-276-0/BT-X-282-0
    property MaturityDate: ZUGFeRDNullable<TDateTime> read FMaturityDate write FMaturityDate;

    /// <summary>
    /// Percentage of discount or surcharge
    /// </summary>
    property Percentage: ZUGFeRDNullable<Currency> read FPercentage write FPercentage;

    /// <summary>
    /// Base amount applied to percentage of discount or surcharge
    /// </summary>
    property BaseAmount: ZUGFeRDNullable<Currency> read FBaseAmount write FBaseAmount;

    /// <summary>
    /// The actual amount of discount or surcharge
    /// </summary>
    property ActualAmount: ZUGFeRDNullable<Currency> read FActualAmount write FActualAmount;

    property PartialPaymentAmount: ZUGFeRDNullable<Currency> read FPartialPaymentAmount write FPartialPaymentAmount;

  public
  end;

implementation

end.
