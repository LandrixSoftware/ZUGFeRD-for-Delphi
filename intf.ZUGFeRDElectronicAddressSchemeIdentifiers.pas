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

unit intf.ZUGFeRDElectronicAddressSchemeIdentifiers;

interface

uses System.Classes, System.SysUtils;

type
  /// <summary>
  /// For a reference see:
  /// https://docs.peppol.eu/poacc/billing/3.0/codelist/eas/
  /// </summary>
  TZUGFeRDElectronicAddressSchemeIdentifiers = (
    /// <summary>
    /// Unknown means, we have a problem ...
    /// </summary>
    Unknown = 0,

    /// <summary>
    /// EAN Location Code
    /// </summary>
    EanLocationCode = 0088,

    /// <summary>
    /// German Leitweg-ID
    /// </summary>
    LeitwegID = 0204,

    /// <summary>
    /// Hungary VAT number
    /// </summary>
    HungaryVatNumber = 9910,

    /// <summary>
    /// Andorra VAT number
    /// </summary>
    AndorraVatNumber = 9922,

    /// <summary>
    /// Albania VAT number
    /// </summary>
    AlbaniaVatNumber = 9923,

    /// <summary>
    /// Bosnia and Herzegovina VAT number
    /// </summary>
    BosniaAndHerzegovinaVatNumber = 9924,

    /// <summary>
    /// Belgium VAT number
    /// </summary>
    BelgiumVatNumber = 9925,

    /// <summary>
    /// Bulgaria VAT number
    /// </summary>
    BulgariaVatNumber = 9926,

    /// <summary>
    /// Switzerland VAT number
    /// </summary>
    SwitzerlandVatNumber = 9927,

    /// <summary>
    /// Cyprus VAT number
    /// </summary>
    CyprusVatNumber = 9928,

    /// <summary>
    /// Czech Republic VAT number
    /// </summary>
    CzechRepublicVatNumber = 9929,

    /// <summary>
    /// Germany VAT number
    /// </summary>
    GermanyVatNumber = 9930,

    /// <summary>
    /// Estonia VAT number
    /// </summary>
    EstoniaVatNumber = 9931,

    /// <summary>
    /// United Kingdom VAT number
    /// </summary>
    UnitedKingdomVatNumber = 9932,

    /// <summary>
    /// Greece VAT number
    /// </summary>
    GreeceVatNumber = 9933,

    /// <summary>
    /// Croatia VAT number
    /// </summary>
    CroatiaVatNumber = 9934,

    /// <summary>
    /// Ireland VAT number
    /// </summary>
    IrelandVatNumber = 9935,

    /// <summary>
    /// Liechtenstein VAT number
    /// </summary>
    LiechtensteinVatNumber = 9936,

    /// <summary>
    /// Lithuania VAT number
    /// </summary>
    LithuaniaVatNumber = 9937,

    /// <summary>
    /// Luxemburg VAT number
    /// </summary>
    LuxemburgVatNumber = 9938,

    /// <summary>
    /// Latvia VAT number
    /// </summary>
    LatviaVatNumber = 9939,

    /// <summary>
    /// Monaco VAT number
    /// </summary>
    MonacoVatNumber = 9940,

    /// <summary>
    /// Montenegro VAT number
    /// </summary>
    MontenegroVatNumber = 9941,

    /// <summary>
    /// Macedonia, of the former Yugoslav Republic VAT number
    /// </summary>
    MacedoniaVatNumber = 9942,

    /// <summary>
    /// Malta VAT number
    /// </summary>
    MaltaVatNumber = 9943,

    /// <summary>
    /// Netherlands VAT number
    /// </summary>
    NetherlandsVatNumber = 9944,

    /// <summary>
    /// Poland VAT number
    /// </summary>
    PolandVatNumber = 9945,

    /// <summary>
    /// Portugal VAT number
    /// </summary>
    PortugalVatNumber = 9946,

    /// <summary>
    /// Romania VAT number
    /// </summary>
    RomaniaVatNumber = 9947,

    /// <summary>
    /// Serbia VAT number
    /// </summary>
    SerbiaVatNumber = 9948,

    /// <summary>
    /// Slovenia VAT number
    /// </summary>
    SloveniaVatNumber = 9949,

    /// <summary>
    /// Slovakia VAT number
    /// </summary>
    SlovakiaVatNumber = 9950,

    /// <summary>
    /// San Marino VAT number
    /// </summary>
    SanMarinoVatNumber = 9951,

    /// <summary>
    /// Turkey VAT number
    /// </summary>
    TurkeyVatNumber = 9952,

    /// <summary>
    /// Holy See (Vatican City State) VAT number
    /// </summary>
    HolySeeVatNumber = 9953,

    /// <summary>
    /// Swedish VAT number
    /// </summary>
    SwedishVatNumber = 9955,

    /// <summary>
    /// French VAT number
    /// </summary>
    FrenchVatNumber = 9957
  );

  TZUGFeRDElectronicAddressSchemeIdentifiersExtensions = class
  public
    class function FromString(s: string): TZUGFeRDElectronicAddressSchemeIdentifiers;
    class function EnumToString(c: TZUGFeRDElectronicAddressSchemeIdentifiers): string;
  end;

implementation

{ TZUGFeRDElectronicAddressSchemeIdentifiersExtensions }

class function TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(
  c: TZUGFeRDElectronicAddressSchemeIdentifiers): string;
begin
  case c of
    TZUGFeRDElectronicAddressSchemeIdentifiers.EanLocationCode: Result := '0088';
    TZUGFeRDElectronicAddressSchemeIdentifiers.LeitwegID: Result := '0204';
    TZUGFeRDElectronicAddressSchemeIdentifiers.HungaryVatNumber: Result := '9910';
    TZUGFeRDElectronicAddressSchemeIdentifiers.AndorraVatNumber: Result := '9922';
    TZUGFeRDElectronicAddressSchemeIdentifiers.AlbaniaVatNumber: Result := '9923';
    TZUGFeRDElectronicAddressSchemeIdentifiers.BosniaAndHerzegovinaVatNumber: Result := '9924';
    TZUGFeRDElectronicAddressSchemeIdentifiers.BelgiumVatNumber: Result := '9925';
    TZUGFeRDElectronicAddressSchemeIdentifiers.BulgariaVatNumber: Result := '9926';
    TZUGFeRDElectronicAddressSchemeIdentifiers.SwitzerlandVatNumber: Result := '9927';
    TZUGFeRDElectronicAddressSchemeIdentifiers.CyprusVatNumber: Result := '9928';
    TZUGFeRDElectronicAddressSchemeIdentifiers.CzechRepublicVatNumber: Result := '9929';
    TZUGFeRDElectronicAddressSchemeIdentifiers.GermanyVatNumber: Result := '9930';
    TZUGFeRDElectronicAddressSchemeIdentifiers.EstoniaVatNumber: Result := '9931';
    TZUGFeRDElectronicAddressSchemeIdentifiers.UnitedKingdomVatNumber: Result := '9932';
    TZUGFeRDElectronicAddressSchemeIdentifiers.GreeceVatNumber: Result := '9933';
    TZUGFeRDElectronicAddressSchemeIdentifiers.CroatiaVatNumber: Result := '9934';
    TZUGFeRDElectronicAddressSchemeIdentifiers.IrelandVatNumber: Result := '9935';
    TZUGFeRDElectronicAddressSchemeIdentifiers.LiechtensteinVatNumber: Result := '9936';
    TZUGFeRDElectronicAddressSchemeIdentifiers.LithuaniaVatNumber: Result := '9937';
    TZUGFeRDElectronicAddressSchemeIdentifiers.LuxemburgVatNumber: Result := '9938';
    TZUGFeRDElectronicAddressSchemeIdentifiers.LatviaVatNumber: Result := '9939';
    TZUGFeRDElectronicAddressSchemeIdentifiers.MonacoVatNumber: Result := '9940';
    TZUGFeRDElectronicAddressSchemeIdentifiers.MontenegroVatNumber: Result := '9941';
    TZUGFeRDElectronicAddressSchemeIdentifiers.MacedoniaVatNumber: Result := '9942';
    TZUGFeRDElectronicAddressSchemeIdentifiers.MaltaVatNumber: Result := '9943';
    TZUGFeRDElectronicAddressSchemeIdentifiers.NetherlandsVatNumber: Result := '9944';
    TZUGFeRDElectronicAddressSchemeIdentifiers.PolandVatNumber: Result := '9945';
    TZUGFeRDElectronicAddressSchemeIdentifiers.PortugalVatNumber: Result := '9946';
    TZUGFeRDElectronicAddressSchemeIdentifiers.RomaniaVatNumber: Result := '9947';
    TZUGFeRDElectronicAddressSchemeIdentifiers.SerbiaVatNumber: Result := '9948';
    TZUGFeRDElectronicAddressSchemeIdentifiers.SloveniaVatNumber: Result := '9949';
    TZUGFeRDElectronicAddressSchemeIdentifiers.SlovakiaVatNumber: Result := '9950';
    TZUGFeRDElectronicAddressSchemeIdentifiers.SanMarinoVatNumber: Result := '9951';
    TZUGFeRDElectronicAddressSchemeIdentifiers.TurkeyVatNumber: Result := '9952';
    TZUGFeRDElectronicAddressSchemeIdentifiers.HolySeeVatNumber: Result := '9953';
    TZUGFeRDElectronicAddressSchemeIdentifiers.SwedishVatNumber: Result := '9955';
    TZUGFeRDElectronicAddressSchemeIdentifiers.FrenchVatNumber: Result := '9957';
  else
    Result := '0000';
  end;
end;

class function TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString(
  s: string): TZUGFeRDElectronicAddressSchemeIdentifiers;
begin
  if SameText(s,'0088') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.EanLocationCode else
  if SameText(s,'0204') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.LeitwegID else
  if SameText(s,'9910') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.HungaryVatNumber else
  if SameText(s,'9922') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.AndorraVatNumber else
  if SameText(s,'9923') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.AlbaniaVatNumber else
  if SameText(s,'9924') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.BosniaAndHerzegovinaVatNumber else
  if SameText(s,'9925') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.BelgiumVatNumber else
  if SameText(s,'9926') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.BulgariaVatNumber else
  if SameText(s,'9927') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.SwitzerlandVatNumber else
  if SameText(s,'9928') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.CyprusVatNumber else
  if SameText(s,'9929') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.CzechRepublicVatNumber else
  if SameText(s,'9930') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.GermanyVatNumber else
  if SameText(s,'9931') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.EstoniaVatNumber else
  if SameText(s,'9932') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.UnitedKingdomVatNumber else
  if SameText(s,'9933') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.GreeceVatNumber else
  if SameText(s,'9934') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.CroatiaVatNumber else
  if SameText(s,'9935') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.IrelandVatNumber else
  if SameText(s,'9936') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.LiechtensteinVatNumber else
  if SameText(s,'9937') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.LithuaniaVatNumber else
  if SameText(s,'9938') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.LuxemburgVatNumber else
  if SameText(s,'9939') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.LatviaVatNumber else
  if SameText(s,'9940') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.MonacoVatNumber else
  if SameText(s,'9941') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.MontenegroVatNumber else
  if SameText(s,'9942') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.MacedoniaVatNumber else
  if SameText(s,'9943') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.MaltaVatNumber else
  if SameText(s,'9944') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.NetherlandsVatNumber else
  if SameText(s,'9945') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.PolandVatNumber else
  if SameText(s,'9946') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.PortugalVatNumber else
  if SameText(s,'9947') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.RomaniaVatNumber else
  if SameText(s,'9948') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.SerbiaVatNumber else
  if SameText(s,'9949') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.SloveniaVatNumber else
  if SameText(s,'9950') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.SlovakiaVatNumber else
  if SameText(s,'9951') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.SanMarinoVatNumber else
  if SameText(s,'9952') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.TurkeyVatNumber else
  if SameText(s,'9953') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.HolySeeVatNumber else
  if SameText(s,'9955') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.SwedishVatNumber else
  if SameText(s,'9957') then
    Result := TZUGFeRDElectronicAddressSchemeIdentifiers.FrenchVatNumber else

  Result := TZUGFeRDElectronicAddressSchemeIdentifiers.Unknown;
end;

end.
