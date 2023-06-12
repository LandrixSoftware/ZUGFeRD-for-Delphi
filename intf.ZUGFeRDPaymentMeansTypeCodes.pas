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

unit intf.ZUGFeRDPaymentMeansTypeCodes;

interface

uses
  System.SysUtils,System.TypInfo
  ;

type
  /// <summary>
  /// Adopted to ZUGFeRD 1.0, German description from ZUGFeRD specification
  /// </summary>
  TZUGFeRDPaymentMeansTypeCodes = (
    /// <summary>
    /// Unknown/ invalid value
    /// </summary>
    Unknown = 0,

    /// <summary>
    /// Keine Zahlungsart definiert
    /// Available in: Extended
    /// </summary>
    NotDefined = 1,

    /// <summary>
    /// Belastung durch automatisierte Clearingstelle, Z.B. bei Abwicklung durch Zahlungsdienstleister wie Online-Bezahlsysteme
    /// </summary>
    AutomatedClearingHouseDebit = 3,

    /// <summary>
    /// Bar
    /// Available in: Basic, Extended
    /// </summary>
    InCash = 10,

    /// <summary>
    /// Scheck
    /// Available in: Basic, Extended
    /// </summary>
    Cheque = 20,

    /// <summary>
    /// Available in: Basic, Extended
    /// </summary>
    CreditTransfer = 30,

    /// <summary>
    /// Lastschriftübermittlung:
    /// Zahlung durch Belastung eines Geldbetrages eines
    /// Kontos zugunsten eines anderen.
    /// Überweisung international und nationale SEPA-Überweisung
    ///
    /// Available in: Extended
    /// </summary>
    DebitTransfer = 31,

    /// <summary>
    /// Zahlung an Bankkonto
    /// Überweisung national, vor SEPA-Umstellung
    /// Available in: Basic, Extended
    /// </summary>
    PaymentToBankAccount = 42,

    /// <summary>
    /// Bankkkarte, Kreditkarte
    /// Available in: Basic, Extended
    /// </summary>
    BankCard = 48,

    /// <summary>
    /// Lastschriftverfahren
    ///
    /// Available in: Basic, Extended
    /// /// </summary>
    DirectDebit = 49,

    /// <summary>
    /// Available in: Basic, Extended
    /// </summary>
    StandingAgreement = 57,


    /// <summary>
    /// Available in: Basic, Extended
    /// </summary>
    SEPACreditTransfer = 58,

    /// <summary>
    /// Available in: Basic, Extended
    /// </summary>
    SEPADirectDebit = 59,

    /// <summary>
    /// Ausgleich zwischen Partnern.
    /// Beträge, die zwei Partner sich gegenseitig schulden werden ausgeglichen um unnütze Zahlungen zu vermeiden.
    /// Available in: Basic, Extended
    /// </summary>
    ClearingBetweenPartners = 97
  );

  TZUGFeRDPaymentMeansTypeCodesExtensions = class
  public
    class function FromString(const s: string): TZUGFeRDPaymentMeansTypeCodes;
    class function EnumToString(t: TZUGFeRDPaymentMeansTypeCodes): string;
  end;

implementation

class function TZUGFeRDPaymentMeansTypeCodesExtensions.FromString(const s: string): TZUGFeRDPaymentMeansTypeCodes;
begin
  if SameText(s,'1') then
    Result := TZUGFeRDPaymentMeansTypeCodes.NotDefined
  else
  if SameText(s,'3') then
    Result := TZUGFeRDPaymentMeansTypeCodes.AutomatedClearingHouseDebit
  else
  if SameText(s,'10') then
    Result := TZUGFeRDPaymentMeansTypeCodes.InCash
  else
  if SameText(s,'20') then
    Result := TZUGFeRDPaymentMeansTypeCodes.Cheque
  else
  if SameText(s,'30') then
    Result := TZUGFeRDPaymentMeansTypeCodes.CreditTransfer
  else
  if SameText(s,'31') then
    Result := TZUGFeRDPaymentMeansTypeCodes.DebitTransfer
  else
  if SameText(s,'42') then
    Result := TZUGFeRDPaymentMeansTypeCodes.PaymentToBankAccount
  else
  if SameText(s,'48') then
    Result := TZUGFeRDPaymentMeansTypeCodes.BankCard
  else
  if SameText(s,'49') then
    Result := TZUGFeRDPaymentMeansTypeCodes.DirectDebit
  else
  if SameText(s,'57') then
    Result := TZUGFeRDPaymentMeansTypeCodes.StandingAgreement
  else
  if SameText(s,'58') then
    Result := TZUGFeRDPaymentMeansTypeCodes.SEPACreditTransfer
  else
  if SameText(s,'59') then
    Result := TZUGFeRDPaymentMeansTypeCodes.SEPADirectDebit
  else
  if SameText(s,'97') then
    Result := TZUGFeRDPaymentMeansTypeCodes.ClearingBetweenPartners
  else
    Result := TZUGFeRDPaymentMeansTypeCodes.Unknown
end;

class function TZUGFeRDPaymentMeansTypeCodesExtensions.EnumToString(t: TZUGFeRDPaymentMeansTypeCodes): string;
begin
  Result := IntToStr(Integer(t));
end;

end.
