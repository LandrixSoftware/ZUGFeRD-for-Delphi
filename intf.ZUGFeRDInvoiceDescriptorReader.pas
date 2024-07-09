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

unit intf.ZUGFeRDInvoiceDescriptorReader;

interface

uses
  System.Classes, System.SysUtils, System.IOUtils, System.DateUtils
  ,Xml.XMLDoc, Xml.xmldom, Xml.XMLIntf,intf.ZUGFeRDMSXML2_TLB
  ,intf.ZUGFeRDInvoiceDescriptor
  ,intf.ZUGFeRDExceptions
  ,intf.ZUGFeRDXmlHelper;

type
  TZUGFeRDInvoiceDescriptorReader = class abstract
  public
    function Load(stream: TStream): TZUGFeRDInvoiceDescriptor; overload; virtual; abstract;
    function IsReadableByThisReaderVersion(stream: TStream): Boolean; overload; virtual; abstract;
    function IsReadableByThisReaderVersion(xmldocument: IXMLDocument): Boolean; overload; virtual; abstract;

    function Load(const filename: string): TZUGFeRDInvoiceDescriptor; overload;
    function Load(xmldocument : IXMLDocument): TZUGFeRDInvoiceDescriptor; overload; virtual; abstract;
    function IsReadableByThisReaderVersion(const filename: string): Boolean; overload;
  protected
    //function _GenerateNamespaceManagerFromNode(node: IXmlDomNode) : XmlNamespaceManager;
    function _nodeAsBool(node: IXmlDomNode; const xpath: string; {nsmgr: XmlNamespaceManager = nil; } defaultValue: Boolean = False): Boolean;
    function _nodeAsString(node: IXmlDomNode; const xpath: string; {nsmgr: XmlNamespaceManager = nil; } defaultValue: string = ''): string;
    function _nodeAsInt(node: IXmlDomNode; const xpath: string; {nsmgr: XmlNamespaceManager = nil; } defaultValue: Integer = 0): Integer;
    /// <summary>
    ///  reads the value from given xpath and interprets the value as decimal
    /// </summary>
    function _nodeAsDecimal(node: IXmlDomNode; const xpath: string; {nsmgr: XmlNamespaceManager = nil; } defaultValue: Currency = 0): Currency;
    /// <summary>
    ///  reads the value from given xpath and interprets the value as date time
    /// </summary>
    function _nodeAsDouble(node: IXmlDomNode; const xpath: string; {nsmgr: XmlNamespaceManager = nil; } defaultValue: Double = 0): Double;
    function _nodeAsDateTime(node: IXmlDomNode; const xpath: string; {nsmgr: XmlNamespaceManager = nil; } defaultValue: TDateTime = 0): TDateTime;
    function SafeParseDateTime(const year: string = '0'; const month: string = '0'; const day: string = '0'; const hour: string = '0'; const minute: string = '0'; const second: string = '0'): TDateTime;
    function IsReadableByThisReaderVersion(stream: TStream; const validURIs: TArray<string>): Boolean; overload;
    function IsReadableByThisReaderVersion(xmldocument: IXMLDocument; const validURIs: TArray<string>): Boolean; overload;
  end;

implementation

function TZUGFeRDInvoiceDescriptorReader.Load(const filename: string): TZUGFeRDInvoiceDescriptor;
var
  fs: TFileStream;
begin
  if not FileExists(filename) then
    raise TZUGFeRDFileNotFoundException.Create(filename);

  fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
    Result := Load(fs);
  finally
    fs.Free;
  end;
end;

function TZUGFeRDInvoiceDescriptorReader.IsReadableByThisReaderVersion(const filename: string): Boolean;
var
  fs: TFileStream;
begin
  if not FileExists(filename) then
    raise TZUGFeRDFileNotFoundException.Create(filename);

  fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
    Result := IsReadableByThisReaderVersion(fs);
  finally
    fs.Free;
  end;
end;

function TZUGFeRDInvoiceDescriptorReader._nodeAsBool(node: IXmlDomNode; const xpath: string; {nsmgr: XmlNamespaceManager;}
  defaultValue: Boolean): Boolean;
var
  value: string;
begin
  Result := defaultValue;

  if node = nil then
    exit;

  value := _nodeAsString(node, xpath{, nsmgr});
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

function TZUGFeRDInvoiceDescriptorReader._nodeAsString(node: IXmlDomNode; const xpath: string; {nsmgr: XmlNamespaceManager;}
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

function TZUGFeRDInvoiceDescriptorReader._nodeAsInt(node: IXmlDomNode; const xpath: string; {nsmgr: XmlNamespaceManager;}
  defaultValue: Integer): Integer;
var
  temp: string;
begin
  Result := defaultValue;

  if node = nil then
    exit;

  temp := _nodeAsString(node, xpath{, nsmgr});

  TryStrToInt(temp, Result);
end;

function TZUGFeRDInvoiceDescriptorReader._nodeAsDecimal(node: IXmlDomNode; const xpath: string; {nsmgr: XmlNamespaceManager;}
  defaultValue: Currency): Currency;
var
  temp: string;
begin
  Result := defaultValue;

  if node = nil then
    exit;

  temp := _nodeAsString(node, xpath{, nsmgr});
  TryStrToCurr(temp, Result, FormatSettings.Invariant);
end;

function TZUGFeRDInvoiceDescriptorReader._nodeAsDouble(node: IXmlDomNode;
  const xpath: string; defaultValue: Double): Double;
var
  temp: string;
begin
  Result := defaultValue;

  if node = nil then
    exit;

  temp := _nodeAsString(node, xpath{, nsmgr});
  TryStrToFloat(temp, Result, FormatSettings.Invariant);
end;

function TZUGFeRDInvoiceDescriptorReader._nodeAsDateTime(node: IXmlDomNode; const xpath: string; {nsmgr: XmlNamespaceManager;}
  defaultValue: TDateTime): TDateTime;
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
  begin
    if not (defaultValue <> 0) then
      Result := 0;
    exit;
  end;

  if dateNode.Attributes.getNamedItem('format') <> nil then
  if dateNode.Attributes.getNamedItem('format').text <> '' then
    format := dateNode.Attributes.getNamedItem('format').text;

  rawValue := dateNode.text;

  if (Trim(rawValue) = '') then // we have to deal with real-life ZUGFeRD files :(
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
    aDayOfWeek := DayOfWeek(aDay) - 1;

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

function TZUGFeRDInvoiceDescriptorReader.SafeParseDateTime(const year: string = '0';
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

function TZUGFeRDInvoiceDescriptorReader.IsReadableByThisReaderVersion(stream: TStream;
  const validURIs: TArray<string>): Boolean;
var
  oldStreamPosition: Int64;
  reader: TStreamReader;
  data: string;
  validURI: string;
begin
  Result := false;

  oldStreamPosition := stream.Position;
  stream.Position := 0;
  reader := TStreamReader.Create(stream, TEncoding.UTF8, True, 1024);
  try
    data := reader.ReadToEnd.Replace(' ', '').ToLower;
    for validURI in validURIs do
    begin
      if data.Contains(Format('>%s<', [validURI.ToLower])) then
      begin
        stream.Position := oldStreamPosition;
        Result := true;
        exit;
      end;
    end;
  finally
    reader.Free;
  end;

  stream.Position := oldStreamPosition;
end;

function TZUGFeRDInvoiceDescriptorReader.IsReadableByThisReaderVersion(
  xmldocument: IXMLDocument; const validURIs: TArray<string>): Boolean;
var
  toValidate,validURI: string;
  node,node2 : IXMLNode;
begin
  Result := false;

  if xmldocument = nil then
    exit;

  toValidate := '';

  if (SameText(xmldocument.DocumentElement.NodeName,'Invoice') or
      SameText(xmldocument.DocumentElement.NodeName,'ubl:Invoice')) then
  begin
    if not TZUGFeRDXmlHelper.FindChild(xmldocument.DocumentElement,'cbc:CustomizationID',node) then
      exit;

    toValidate := node.Text;
  end else
  if (SameText(xmldocument.DocumentElement.NodeName,'CrossIndustryInvoice') or
      SameText(xmldocument.DocumentElement.NodeName,'rsm:CrossIndustryInvoice') or
      SameText(xmldocument.DocumentElement.NodeName,'CrossIndustryDocument') or
      SameText(xmldocument.DocumentElement.NodeName,'rsm:CrossIndustryDocument')) then
  begin
    if not (TZUGFeRDXmlHelper.FindChild(xmldocument.DocumentElement,'rsm:ExchangedDocumentContext',node) or
            TZUGFeRDXmlHelper.FindChild(xmldocument.DocumentElement,'ExchangedDocumentContext',node) or
            TZUGFeRDXmlHelper.FindChild(xmldocument.DocumentElement,'rsm:SpecifiedExchangedDocumentContext',node) or
            TZUGFeRDXmlHelper.FindChild(xmldocument.DocumentElement,'SpecifiedExchangedDocumentContext',node)) then
      exit;
    if not TZUGFeRDXmlHelper.FindChild(node,'ram:GuidelineSpecifiedDocumentContextParameter',node2) then
      exit;
    if not TZUGFeRDXmlHelper.FindChild(node2,'ram:ID',node) then
      exit;

    toValidate := node.Text;
  end;

  if toValidate <> '' then
  for validURI in validURIs do
  if SameText(toValidate,validURI) then
  begin
    Result := true;
    break;
  end;
end;

//function TZUGFeRDInvoiceDescriptorReader._GenerateNamespaceManagerFromNode(
//  node: IXmlDomNode) : XmlNamespaceManager;
//begin
//  XmlNamespaceManager nsmgr = new XmlNamespaceManager(node.OwnerDocument.NameTable);
//  foreach (XmlAttribute attr in node.Attributes)
//  {
//      if (attr.Prefix == "xmlns")
//      {
//          nsmgr.AddNamespace(attr.LocalName, attr.Value);
//      }
//      else if (attr.Name == "xmlns")
//      {
//          nsmgr.AddNamespace(string.Empty, attr.Value);
//      }
//  }
//
//  return nsmgr;
//end;

end.

