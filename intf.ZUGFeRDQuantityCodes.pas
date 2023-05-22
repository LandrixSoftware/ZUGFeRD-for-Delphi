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

unit intf.ZUGFeRDQuantityCodes;

interface

uses
  System.SysUtils,System.TypInfo
  ;

type
    /// <summary>
    /// ISO Quantity Codes
    ///
    /// for web reference, see e.g.
    /// http://www.robert-kuhlemann.de/iso_masseinheiten.htm
    /// </summary>
  TZUGFeRDQuantityCodes = (
    /// <summary>
    /// Unknown/ invalid quantity code
    /// </summary>
    Unknown = 0,
    /// <summary>
    /// Eins (St�ck)
    /// Abk�rzung: Stk.
    /// </summary>
    C62,
    /// <summary>
    /// Tag
    /// Abk�rzung: Tag(e)
    /// </summary>
    DAY,
    /// <summary>
    /// Piece: A unit of count defining the number of pieces (piece: a single item, article or exemplar).
    /// </summary>
    /// <seealso cref="QuantityCodes.C62"/>
    H87,
    /// <summary>
    /// Hektar
    /// Abk�rzung: ha
    /// </summary>
    HAR,
    /// <summary>
    /// Stunde
    /// Abk�rzung: Std.
    /// </summary>
    HUR,
    /// <summary>
    /// Kilogramm
    /// Abk�rzung: kg
    /// </summary>
    KGM,
    /// <summary>
    /// Zentimeter
    /// Abk�rzung: cm
    /// </summary>
    CMT,
    /// <summary>
    /// Kilometer
    /// Abk�rzung: km (Rec20r13) f�r XRechnung
    /// </summary>
    KMT,
    /// <summary>
    /// Kilometer
    /// Abk�rzung: km (Rec20r16)
    /// </summary>
    KTM,
    /// <summary>
    /// Kilowattstune
    /// Abk�rzung: kWh
    /// </summary>
    KWH,
    /// <summary>
    /// Kilowatt
    /// Abk�rzung: kW
    /// </summary>
    KWT,
    /// <summary>
    /// Pauschale
    /// Abk�rzung: pausch.
    /// </summary>
    LS,
    /// <summary>
    /// Liter
    /// Abk�rzung: l
    /// </summary>
    LTR,
    /// <summary>
    /// Minute
    /// Abk�rzung: min
    /// </summary>
    MIN,
    /// <summary>
    /// Quadratmillimeter
    /// Abk�rzung: mm^2
    /// </summary>
    MMK,
    /// <summary>
    /// Millimeter
    /// Abk�rzung: mm
    /// </summary>
    MMT,
    /// <summary>
    /// Quadratmeter
    /// Abk�rzung: m^2
    /// </summary>
    MTK,
    /// <summary>
    /// Kubikmeter
    /// Abk�rzung: m^3
    /// </summary>
    MTQ,
    /// <summary>
    /// Meter
    /// Abk�rzung: m
    /// </summary>
    MTR,
    /// <summary>
    /// Anzahl Artikel
    /// Abk�rzung: Anz.
    /// </summary>
    NAR,
    /// <summary>
    /// Anzahl Paare
    /// Abk�rzung: Pr.
    /// </summary>
    NPR,
    /// <summary>
    /// Prozent
    /// Abk�rzung: %
    /// </summary>
    P1,
    /// <summary>
    /// St�ck
    /// </summary>
    //[Obsolete("Does not conform to ZUGFeRD standard. Use H87 ('piece') or C62 ('one') instead")]
    PCE,
    /// <summary>
    /// Set
    /// Abk�rzung: Set(s)
    /// </summary>
    SET_,
    /// <summary>
    /// Tonne (metrisch)
    /// Abk�rzung:  t
    /// </summary>
    TNE,
    /// <summary>
    /// Woche
    /// Abk�rzung: Woche(n)
    /// </summary>
    WEE,
    /// <summary>
    /// Monat
    /// Abk�rzung: Monat(e)
    /// </summary>
    MON,
    /// <summary>
    /// Jahr
    /// Abk�rzung: Jahr(e)
    /// </summary>
    ANN,
    /// <summary>
    /// Sekunde
    /// Abk�rzung: Sekunde(n)
    /// </summary>
    SEC,
    /// <summary>
    /// B�ndel
    /// Abk�rzung: Bund
    /// </summary>
    XBE,
    /// <summary>
    /// Flasche
    /// Abk�rzung: Fl
    /// </summary>
    /// <remarks>
    /// Bottle, non-protected, cylindrical
    /// A narrow-necked cylindrical shaped vessel without external protective packing material
    /// </remarks>
    XBO,
    /// <summary>
    /// Karton
    /// Abk�rzung: Kt
    /// </summary>
    XCT,
    /// <summary>
    /// Paar
    /// </summary>
    /// <remarks>
    /// A unit of count defining the number of pairs (pair: item described by two's).
    /// </remarks>
    PR,
    /// <summary>
    /// Palette
    /// Abk�rzung: Pal
    /// </summary>
    /// <remarks>
    /// Platform or open-ended box, usually made of wood, on which goods are retained for ease of mechanical handling during transport and storage.
    /// </remarks>
    XPX,
    /// <summary>
    /// Stange
    /// Abk�rzung: Stg
    /// </summary>
    XRD,
    /// <summary>
    /// Tafel/Board
    /// Abk�rzung: Tf
    /// </summary>
    XBD,
    /// <summary>
    /// tausend St�ck
    /// Abk�rzung: Tsd
    /// </summary>
    /// <remarks>
    /// A unit of count defining the number of pieces in multiples of 1000 (piece: a single item, article or exemplar).
    /// </remarks>
    T3,
    /// <summary>
    /// Verpackung
    /// </summary>
    /// <remarks>
    /// Standard packaging unit
    /// </remarks>
    XPK
  );


  TZUGFeRDQuantityCodesExtensions = class
  public
    class function FromString(const s: string): TZUGFeRDQuantityCodes;
    class function EnumToString(codes: TZUGFeRDQuantityCodes): string;
  end;

implementation

{ TZUGFeRDQuantityCodesExtensions }

class function TZUGFeRDQuantityCodesExtensions.EnumToString(
  codes: TZUGFeRDQuantityCodes): string;
begin
  if codes = TZUGFeRDQuantityCodes.SET_ then
    Result := 'SET'
  else
    Result := GetEnumName(TypeInfo(TZUGFeRDQuantityCodes), Integer(codes));
end;

class function TZUGFeRDQuantityCodesExtensions.FromString(
  const s: string): TZUGFeRDQuantityCodes;
var
  enumValue : Integer;
begin
  if SameText(s,'SET') then
  begin
    Result := TZUGFeRDQuantityCodes.SET_;
    exit;
  end;

  enumValue := GetEnumValue(TypeInfo(TZUGFeRDQuantityCodes), s);
  if enumValue >= 0 then
    Result := TZUGFeRDQuantityCodes(enumValue)
  else
    Result := TZUGFeRDQuantityCodes.Unknown;
end;

end.
