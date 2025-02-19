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

unit intf.ZUGFeRDSpecialServiceDescriptionCodes;

interface

uses
  System.SysUtils,System.TypInfo,System.StrUtils
  ;

type
  /// <summary>
  /// https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:untdid.7161_3
  /// </summary>
  TZUGFeRDSpecialServiceDescriptionCodes = (
    AA_Advertising, //The service of providing advertising.
    AAA_Telecommunication, //The service of providing telecommunication activities and/or faclities.
    ABK_Miscellaneous,	//Miscellaneous services.
    ABL_AdditionalPackaging, //The service of providing additional packaging.
    ADR_OtherServices, //A code indicating that other non-specific services are in operation.
    ADT_Pickup, //The service of picking up or collection of goods.
    FC_FreightService, //The service of moving goods, by whatever means, from one place to another.
    FI_Financing, //The service of providing financing.
    LA_Labelling, //Labelling service.
    PC_Packing, //The service of packing.
    TAC_Testing, //The service of testing
    Unknown
  );

  TZUGFeRDSpecialServiceDescriptionCodesExtensions = class
  public
    class function FromString(const s: string): TZUGFeRDSpecialServiceDescriptionCodes;
    class function EnumToString(codes: TZUGFeRDSpecialServiceDescriptionCodes): string;
  end;

implementation

{ TZUGFeRDSpecialServiceDescriptionCodesExtensions }

class function TZUGFeRDSpecialServiceDescriptionCodesExtensions.EnumToString(
  codes: TZUGFeRDSpecialServiceDescriptionCodes): string;
begin
  case codes of
    AA_Advertising:          Result := 'AA';
    AAA_Telecommunication:   Result := 'AAA';
    ABK_Miscellaneous:       Result := 'ABK';
    ABL_AdditionalPackaging: Result := 'ABL';
    ADR_OtherServices:       Result := 'ADR';
    ADT_Pickup:              Result := 'ADT';
    FC_FreightService:       Result := 'FC';
    FI_Financing:            Result := 'FI';
    LA_Labelling:            Result := 'LA';
    PC_Packing:              Result := 'PC';
    TAC_Testing:             Result := 'TAC';
    else Result := '';
  end;
end;

class function TZUGFeRDSpecialServiceDescriptionCodesExtensions.FromString(
  const s: string): TZUGFeRDSpecialServiceDescriptionCodes;
begin
  if SameText(s,'AA') then
    Result := AA_Advertising else
  if SameText(s,'AAA') then
    Result := AAA_Telecommunication else
  if SameText(s,'ABK') then
    Result := ABK_Miscellaneous else
  if SameText(s,'ABL') then
    Result := ABL_AdditionalPackaging else
  if SameText(s,'ADR') then
    Result := ADR_OtherServices else
  if SameText(s,'ADT') then
    Result := ADT_Pickup else
  if SameText(s,'FC') then
    Result := FC_FreightService else
  if SameText(s,'FI') then
    Result := FI_Financing else
  if SameText(s,'LA') then
    Result := LA_Labelling else
  if SameText(s,'PC') then
    Result := PC_Packing else
  if SameText(s,'TAC') then
    Result := TAC_Testing else
  Result := Unknown;
end;

end.
