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
    /// Eins (Stück)
    /// Abkürzung: Stk.
    /// </summary>
    C62,
    /// <summary>
    /// Tag
    /// Abkürzung: Tag(e)
    /// </summary>
    DAY,
    /// <summary>
    /// Piece: A unit of count defining the number of pieces (piece: a single item, article or exemplar).
    /// </summary>
    /// <seealso cref="QuantityCodes.C62"/>
    H87,
    /// <summary>
    /// Hektar
    /// Abkürzung: ha
    /// </summary>
    HAR,
    /// <summary>
    /// Stunde
    /// Abkürzung: Std.
    /// </summary>
    HUR,
    /// <summary>
    /// Kilogramm
    /// Abkürzung: kg
    /// </summary>
    KGM,
    /// <summary>
    /// Zentimeter
    /// Abkürzung: cm
    /// </summary>
    CMT,
    /// <summary>
    /// Kilometer
    /// Abkürzung: km (Rec20r13) für XRechnung
    /// </summary>
    KMT,
    /// <summary>
    /// Kilometer
    /// Abkürzung: km (Rec20r16)
    /// </summary>
    KTM,
    /// <summary>
    /// Kilowattstune
    /// Abkürzung: kWh
    /// </summary>
    KWH,
    /// <summary>
    /// Kilowatt
    /// Abkürzung: kW
    /// </summary>
    KWT,
    /// <summary>
    /// Pauschale
    /// Abkürzung: pausch.
    /// </summary>
    LS,
    /// <summary>
    /// Liter
    /// Abkürzung: l
    /// </summary>
    LTR,
    /// <summary>
    /// Minute
    /// Abkürzung: min
    /// </summary>
    MIN,
    /// <summary>
    /// Quadratmillimeter
    /// Abkürzung: mm^2
    /// </summary>
    MMK,
    /// <summary>
    /// Millimeter
    /// Abkürzung: mm
    /// </summary>
    MMT,
    /// <summary>
    /// Quadratmeter
    /// Abkürzung: m^2
    /// </summary>
    MTK,
    /// <summary>
    /// Kubikmeter
    /// Abkürzung: m^3
    /// </summary>
    MTQ,
    /// <summary>
    /// Meter
    /// Abkürzung: m
    /// </summary>
    MTR,
    /// <summary>
    /// Anzahl Artikel
    /// Abkürzung: Anz.
    /// </summary>
    NAR,
    /// <summary>
    /// Anzahl Paare
    /// Abkürzung: Pr.
    /// </summary>
    NPR,
    /// <summary>
    /// Prozent
    /// Abkürzung: %
    /// </summary>
    P1,
    /// <summary>
    /// Stück
    /// </summary>
    //[Obsolete("Does not conform to ZUGFeRD standard. Use H87 ('piece') or C62 ('one') instead")]
    PCE,
    /// <summary>
    /// Set
    /// Abkürzung: Set(s)
    /// </summary>
    SET_,
    /// <summary>
    /// Tonne (metrisch)
    /// Abkürzung:  t
    /// </summary>
    TNE,
    /// <summary>
    /// Woche
    /// Abkürzung: Woche(n)
    /// </summary>
    WEE,
    /// <summary>
    /// Monat
    /// Abkürzung: Monat(e)
    /// </summary>
    MON,
    /// <summary>
    /// Jahr
    /// Abkürzung: Jahr(e)
    /// </summary>
    ANN,
    /// <summary>
    /// Sekunde
    /// Abkürzung: Sekunde(n)
    /// </summary>
    SEC,
    /// <summary>
    /// Bündel
    /// Abkürzung: Bund
    /// </summary>
    XBE,
    /// <summary>
    /// Flasche
    /// Abkürzung: Fl
    /// </summary>
    /// <remarks>
    /// Bottle, non-protected, cylindrical
    /// A narrow-necked cylindrical shaped vessel without external protective packing material
    /// </remarks>
    XBO,
    /// <summary>
    /// Karton
    /// Abkürzung: Kt
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
    /// Abkürzung: Pal
    /// </summary>
    /// <remarks>
    /// Platform or open-ended box, usually made of wood, on which goods are retained for ease of mechanical handling during transport and storage.
    /// </remarks>
    XPX,
    /// <summary>
    /// Stange
    /// Abkürzung: Stg
    /// </summary>
    XRD,
    /// <summary>
    /// Tafel/Board
    /// Abkürzung: Tf
    /// </summary>
    XBD,
    /// <summary>
    /// tausend Stück
    /// Abkürzung: Tsd
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
