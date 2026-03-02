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

unit intf.ZUGFeRDXmlUtils;

interface

uses
  System.Classes, System.SysUtils, System.IOUtils, System.DateUtils
  ,Xml.XMLDoc, Xml.xmldom, Xml.XMLIntf
  ,Xml.Win.msxmldom, Winapi.MSXMLIntf, Winapi.msxml
  ,intf.ZUGFeRDExceptions
  ,intf.ZUGFeRDHelper
  ;


type
  TZUGFeRDXmlUtils = class(TObject)
  public
    class function NodeAsBool(node: IXmlDomNode; const xpath: string; {nsmgr: XmlNamespaceManager = nil; } defaultValue: Boolean = False): Boolean;
    class function NodeAsString(node: IXmlDomNode; const xpath: string; {nsmgr: XmlNamespaceManager = nil; } defaultValue: string = ''): string;
    class function NodeAsInt(node: IXmlDomNode; const xpath: string; {nsmgr: XmlNamespaceManager = nil; } defaultValue: Integer = 0): Integer;
    /// <summary>
    ///  reads the value from given xpath and interprets the value as decimal
    /// </summary>
    class function NodeAsDecimal(node: IXmlDomNode; const xpath: string; {nsmgr: XmlNamespaceManager = nil; } defaultValue: IZUGFeRDNullableParam<Currency> = Nil): ZUGFeRDNullable<Currency>; overload;
    class function NodeAsDecimal(node: IXmlDomNode; const xpath: string; defaultValue: Currency): ZUGFeRDNullable<Currency>; overload;
    /// <summary>
    ///  reads the value from given xpath and interprets the value as date time
    /// </summary>
    class function NodeAsDouble(node: IXmlDomNode; const xpath: string; {nsmgr: XmlNamespaceManager = nil; } defaultValue: IZUGFeRDNullableParam<Double> = Nil): ZUGFeRDNullable<Double>; overload;
    class function NodeAsDouble(node: IXmlDomNode; const xpath: string; defaultValue: Double): ZUGFeRDNullable<Double>; overload;

    class function NodeAsDateTime(node: IXmlDomNode; const xpath: string; {nsmgr: XmlNamespaceManager = nil; } defaultValue: IZUGFeRDNullableParam<TDateTime> = Nil): ZUGFeRDNullable<TDateTime>;
    class function SafeParseDateTime(const year: string = '0'; const month: string = '0'; const day: string = '0'; const hour: string = '0'; const minute: string = '0'; const second: string = '0'): TDateTime;
  end;

implementation

class function TZUGFeRDXmlUtils.NodeAsBool(node: IXmlDomNode; const xpath: string; {nsmgr: XmlNamespaceManager;}
  defaultValue: Boolean): Boolean;
var
  value: string;
begin
  Result := defaultValue;

  if node = nil then
    exit;

  value := TZUGFeRDXmlUtils.NodeAsString(node, xpath{, nsmgr});
  if value.IsEmpty then
    exit
  else
  begin
    value := value.Trim.ToLower;
    if (value = 'true') or (value = '1') then
      Result := true
    else
      Result := false;
  end;
end;

class function TZUGFeRDXmlUtils.NodeAsString(node: IXmlDomNode; const xpath: string; {nsmgr: XmlNamespaceManager;}
  defaultValue: string): string;
var
  _node: IXmlDomNode;
begin
  Result := defaultValue;

  if node = nil then
    exit;

  try
    _node := node.SelectSingleNode(xpath{, nsmgr});
    if _node <> nil then
      Result := _node.Text;
    exit;
  except
    //on XPathException do
    //  Exit(defaultValue);
    on ex: Exception do
      raise ex;
  end;
end;

class function TZUGFeRDXmlUtils.NodeAsInt(node: IXmlDomNode; const xpath: string; {nsmgr: XmlNamespaceManager;}
  defaultValue: Integer): Integer;
var
  temp: string;
begin
  Result := defaultValue;

  if node = nil then
    exit;

  temp := TZUGFeRDXmlUtils.NodeAsString(node, xpath{, nsmgr});

  TryStrToInt(temp, Result);
end;



class function TZUGFeRDXmlUtils.NodeAsDecimal(node: IXmlDomNode; const xpath: string; defaultValue: IZUGFeRDNullableParam<Currency> = Nil): ZUGFeRDNullable<Currency>;
var
  temp: string;
  ResCurrency: Currency;
begin
  Result:= defaultValue;
  if node = nil then
    exit;

  temp := TZUGFeRDXmlUtils.NodeAsString(node, xpath{, nsmgr});
  if TryStrToCurr(temp, ResCurrency, FormatSettings.Invariant) then
    Result:= ResCurrency;
end;

class function TZUGFeRDXmlUtils.NodeAsDecimal(node: IXmlDomNode; const xpath: string; defaultValue: Currency): ZUGFeRDNullable<Currency>;
begin
  Result:= TZUGFeRDXmlUtils.NodeAsDecimal(node, xpath, IZUGFeRDNullableParam<Currency>(ZUGFeRDNullable<Currency>(defaultValue)))
end;
(*
var
  temp: string;
  ResCurrency: Currency;
begin
  Result:= defaultValue;
  if node = nil then
    exit;

  temp := TZUGFeRDXmlUtils.NodeAsString(node, xpath{, nsmgr});
  if TryStrToCurr(temp, ResCurrency, FormatSettings.Invariant) then
    Result:= ResCurrency;
end;
*)

class function TZUGFeRDXmlUtils.NodeAsDouble(node: IXmlDomNode; const xpath: string; defaultValue: IZUGFeRDNullableParam<Double> = Nil): ZUGFeRDNullable<Double>;
var
  temp: string;
  ResDouble: Double;
begin
  Result := defaultValue;

  if node = nil then
    exit;

  temp := TZUGFeRDXmlUtils.NodeAsString(node, xpath{, nsmgr});
  if TryStrToFloat(temp, ResDouble, FormatSettings.Invariant) then
    Result:= ResDouble
end;

class function TZUGFeRDXmlUtils.NodeAsDouble(node: IXmlDomNode; const xpath: string; defaultValue: Double): ZUGFeRDNullable<Double>;
begin
  Result:= TZUGFeRDXmlUtils.NodeAsDouble(node, xpath, IZUGFeRDNullableParam<Double>(ZUGFeRDNullable<Double>(defaultValue)))
end;
(*
var
  temp: string;
  ResDouble: Double;
begin
  Result := defaultValue;

  if node = nil then
    exit;

  temp := TZUGFeRDXmlUtils.NodeAsString(node, xpath{, nsmgr});
  if TryStrToFloat(temp, ResDouble, FormatSettings.Invariant) then
    Result:= ResDouble
end;
*)

class function TZUGFeRDXmlUtils.NodeAsDateTime(node: IXmlDomNode; const xpath: string; {nsmgr: XmlNamespaceManager;}
  defaultValue: IZUGFeRDNullableParam<TDateTime>): ZUGFeRDNullable<TDateTime>;
var
  format, rawValue, year, month, day, hour, minute, second, week: string;
  dateNode: IXmlDomNode;
  aDayOfWeek: Integer;
  jan4,aDay : TDateTime;
begin
  Result := defaultValue;

  if node = nil then
    exit;

  format := '';
  dateNode := node.SelectSingleNode(xpath{, nsmgr});
  if dateNode = nil then
    exit;

  if dateNode.Attributes.getNamedItem('format') <> nil then
  if dateNode.Attributes.getNamedItem('format').text <> '' then
    format := dateNode.Attributes.getNamedItem('format').text;

  // to protect from space and /r /n characters
  rawValue := Trim(dateNode.text);

  if (rawValue = '') then // we have to deal with real-life ZUGFeRD files :(
    exit;

  if (format='102') then
  begin
    if Length(rawValue) <> 8 then
      raise Exception.Create('Wrong length of datetime element (format 102)');

    year := Copy(rawValue, 1, 4);
    month := Copy(rawValue, 5, 2);
    day := Copy(rawValue, 7, 2);

    Result := SafeParseDateTime(year, month, day);
    exit;
  end else
  if (format='610') then
  begin
    if Length(rawValue) <> 6 then
      raise Exception.Create('Wrong length of datetime element (format 610)');

    year := Copy(rawValue, 1, 4);
    month := Copy(rawValue, 5, 2);
    day := '1';

    Result := SafeParseDateTime(year, month, day);
    exit;
  end else
  if (format='616') then
  begin
    if Length(rawValue) <> 6 then
      raise Exception.Create('Wrong length of datetime element (format 616)');

    year := Copy(rawValue, 1, 4);
    week := Copy(rawValue, 5, 2);

    jan4 := EncodeDate(StrToInt(year), 1, 4);
    aDay := IncWeek(jan4, StrToInt(week) - 1);
    aDayOfWeek := DayOfTheWeek(aDay) - 1; // ISO 8601: Mon=1..Sun=7, -1 => Mon=0..Sun=6

    Result := aDay - aDayOfWeek;
    exit;
  end;

  // if none of the codes above is present, use fallback approach
  if Length(rawValue) = 8 then
  begin
    year := Copy(rawValue, 1, 4);
    month := Copy(rawValue, 5, 2);
    day := Copy(rawValue, 7, 2);

    Result := SafeParseDateTime(year, month, day);
    exit;
  end
  else if (Length(rawValue) = 10) and (rawValue[5] = '-') and (rawValue[8] = '-') then // yyyy-mm-dd
  begin
    year := Copy(rawValue, 1, 4);
    month := Copy(rawValue, 6, 2);
    day := Copy(rawValue, 9, 2);

    Result := SafeParseDateTime(year, month, day);
    exit;
  end
  else if (Length(rawValue) = 16) and (rawValue[5] = '-') and (rawValue[8] = '-') and (rawValue[11] = '+') then // yyyy-mm-dd+hh:mm (date with timezone offset)
  begin
    year := Copy(rawValue, 1, 4);
    month := Copy(rawValue, 6, 2);
    day := Copy(rawValue, 9, 2);

    Result := SafeParseDateTime(year, month, day);
    exit;
  end
  else if Length(rawValue) = 19 then
  begin
    year := Copy(rawValue, 1, 4);
    month := Copy(rawValue, 6, 2);
    day := Copy(rawValue, 9, 2);
    hour := Copy(rawValue, 12, 2);
    minute := Copy(rawValue, 15, 2);
    second := Copy(rawValue, 18, 2);

    Result := SafeParseDateTime(year, month, day, hour, minute, second);
    exit;
  end
  else
    raise TZUGFeRDUnsupportedException.Create('Invalid length of datetime value');
end;

class function TZUGFeRDXmlUtils.SafeParseDateTime(const year: string = '0';
  const month: string = '0'; const day: string = '0'; const hour: string = '0';
  const minute: string = '0'; const second: string = '0'): TDateTime;
var
  _year, _month, _day, _hour, _minute, _second: Integer;
begin
  Result := 0;

  if not TryStrToInt(year, _year) then
    exit;

  if not TryStrToInt(month, _month) then
    exit;

  if not TryStrToInt(day, _day) then
    exit;

  if not TryStrToInt(hour, _hour) then
    exit;

  if not TryStrToInt(minute, _minute) then
    exit;

  if not TryStrToInt(second, _second) then
    exit;

  Result := EncodeDateTime(_year, _month, _day, _hour, _minute, _second, 0);
end;

end.
