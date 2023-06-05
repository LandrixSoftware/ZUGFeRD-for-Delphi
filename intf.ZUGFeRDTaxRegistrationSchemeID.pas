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

unit intf.ZUGFeRDTaxRegistrationSchemeID;

interface

uses
  System.SysUtils,System.TypInfo
  ;

type
  /// <summary>
  /// For a reference see:
  /// http://www.unece.org/trade/untdid/d00a/tred/tred1153.htm
  /// </summary>
  TZUGFeRDTaxRegistrationSchemeID = (
    /// <summary>
    /// Fiscal number
    ///
    /// Tax payer's number. Number assigned to individual
    /// persons as well as to corporates by a public
    /// institution; this number is different from the VAT
    /// registration number.
    /// </summary>
    FC,   // Fiscal number

    /// <summary>
    /// VAT registration number
    ///
    /// Unique number assigned by the relevant tax authority to
    /// identify a party for use in relation to Value Added Tax
    /// (VAT).
    /// </summary>
    VA,   // VAT registration number

    /// <summary>
    /// Unknown/ invalid value
    /// </summary>
    Unknown  // Unknown/ invalid value
  );

  TZUGFeRDTaxRegistrationSchemeIDExtensions = class
  public
    class function FromString(const s: string): TZUGFeRDTaxRegistrationSchemeID;
    class function EnumToString(codes: TZUGFeRDTaxRegistrationSchemeID): string;
  end;

implementation

{ TZUGFeRDTaxRegistrationSchemeIDExtensions }

class function TZUGFeRDTaxRegistrationSchemeIDExtensions.EnumToString(
  codes: TZUGFeRDTaxRegistrationSchemeID): string;
begin
  Result := GetEnumName(TypeInfo(TZUGFeRDTaxRegistrationSchemeID), Integer(codes));
end;

class function TZUGFeRDTaxRegistrationSchemeIDExtensions.FromString(
  const s: string): TZUGFeRDTaxRegistrationSchemeID;
var
  enumValue : Integer;
begin
  enumValue := GetEnumValue(TypeInfo(TZUGFeRDTaxRegistrationSchemeID), s);
  if enumValue >= 0 then
    Result := TZUGFeRDTaxRegistrationSchemeID(enumValue)
  else
    Result := TZUGFeRDTaxRegistrationSchemeID.Unknown;
end;

end.
