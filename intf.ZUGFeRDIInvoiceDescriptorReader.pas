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

unit intf.ZUGFeRDIInvoiceDescriptorReader;

interface

uses
  System.Classes, System.SysUtils, System.IOUtils, System.DateUtils,
  System.Generics.Collections, System.Variants,
  Xml.XMLDoc, Xml.xmldom, Xml.XMLIntf,
  Xml.Win.msxmldom, Winapi.MSXMLIntf, Winapi.msxml,
  intf.ZUGFeRDInvoiceDescriptor,
  intf.ZUGFeRDExceptions,
  intf.ZUGFeRDXmlHelper;

type
  TZUGFeRDIInvoiceDescriptorReader = class abstract
  private
    procedure AddNamespaceIfExists(const ADomDoc: IXMLDOMDocument2; ADeclared: TDictionary<string, string>; const APrefix, AExpectedUri: string);
  protected
    FNamespaces: TDictionary<string, string>;
    function CreateFixedNamespaceManager(ADoc: IXMLDocument): IXMLDOMDocument2;
    function IsReadableByThisReaderVersion(AStream: TStream; const AValidURIs: TArray<string>): Boolean; overload;
  public
    function Load(stream: TStream): TZUGFeRDInvoiceDescriptor; overload; virtual; abstract;

    function Load(const filename: string): TZUGFeRDInvoiceDescriptor; overload;
    function Load(xmldocument : IXMLDocument): TZUGFeRDInvoiceDescriptor; overload; virtual; abstract;
    function IsReadableByThisReaderVersion(AStream: TStream): Boolean; overload; virtual; abstract;
    function IsReadableByThisReaderVersion(const AFilename: string): Boolean; overload;
end;

implementation

function TZUGFeRDIInvoiceDescriptorReader.IsReadableByThisReaderVersion(const AFilename: string): Boolean;
var
  LStream: TFileStream;
begin
  if not FileExists(AFilename) then
    raise EFileNotFoundException.Create('File not found: ' + AFilename);

  LStream := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    Result := IsReadableByThisReaderVersion(LStream);
  finally
    LStream.Free;
  end;
end;

function TZUGFeRDIInvoiceDescriptorReader.Load(const filename: string): TZUGFeRDInvoiceDescriptor;
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

function TZUGFeRDIInvoiceDescriptorReader.CreateFixedNamespaceManager(ADoc: IXMLDocument): IXMLDOMDocument2;
var
  LDeclared: TDictionary<string, string>;
  LAttr: IXMLNode;
  I: Integer;
  LPair: TPair<string, string>;
  LDomDoc: IXMLDOMDocument2;
begin
  Result := nil;

  // MSXML Document Interface holen
  if not Supports(ADoc.DOMDocument, IXMLDOMDocument2, LDomDoc) then
    Exit;

  Result := LDomDoc;

  LDeclared := TDictionary<string, string>.Create;
  try
    // Alle deklarierten Namespaces aus dem Dokument einlesen
    if Assigned(ADoc.DocumentElement) then
    begin
      for I := 0 to ADoc.DocumentElement.AttributeNodes.Count - 1 do
      begin
        LAttr := ADoc.DocumentElement.AttributeNodes[I];

        if LAttr.Prefix = 'xmlns' then
          LDeclared.AddOrSetValue(LAttr.LocalName, LAttr.NodeValue)
        else if LAttr.NodeName = 'xmlns' then
          LDeclared.AddOrSetValue('', LAttr.NodeValue);
      end;
    end;

    // Factur-X / ZUGFeRD relevante Namespaces hinzufügen
    for LPair in FNamespaces do
      AddNamespaceIfExists(LDomDoc, LDeclared, LPair.Key, LPair.Value);
  finally
    LDeclared.Free;
  end;
end;

procedure TZUGFeRDIInvoiceDescriptorReader.AddNamespaceIfExists(
  const ADomDoc: IXMLDOMDocument2; ADeclared: TDictionary<string, string>;
  const APrefix, AExpectedUri: string);
var
  LExists: Boolean;
  LValue: string;
  LCurrentNS: string;
begin
  // Prüfen, ob dieser Namespace im Dokument vorkommt
  LExists := False;
  for LValue in ADeclared.Values do
  begin
    if SameText(LValue, AExpectedUri) then
    begin
      LExists := True;
      Break;
    end;
  end;

  if LExists and Assigned(ADomDoc) then
  begin
    // Bei MSXML wird SelectionNamespaces als String-Property gesetzt
    // Format: "xmlns:prefix='uri' xmlns:prefix2='uri2'"
    try
      LCurrentNS := VarToStr(ADomDoc.getProperty('SelectionNamespaces'));
    except
      LCurrentNS := '';
    end;

    if LCurrentNS <> '' then
      LCurrentNS := LCurrentNS + ' ';

    if APrefix = '' then
      LCurrentNS := LCurrentNS + Format('xmlns=''%s''', [AExpectedUri])
    else
      LCurrentNS := LCurrentNS + Format('xmlns:%s=''%s''', [APrefix, AExpectedUri]);

    ADomDoc.setProperty('SelectionNamespaces', LCurrentNS);
  end;
end;

function TZUGFeRDIInvoiceDescriptorReader.IsReadableByThisReaderVersion(AStream: TStream; const AValidURIs: TArray<string>): Boolean;
var
  LOldPosition: Int64;
  LReader: TStreamReader;
  LData: string;
  LValidURI: string;
  LSearchStr: string;
begin
  Result := False;
  LOldPosition := AStream.Position;
  try
    AStream.Position := 0;
    LReader := TStreamReader.Create(AStream, TEncoding.UTF8, True, 1024);
    try
      LData := LReader.ReadToEnd.Replace(' ', '');

      for LValidURI in AValidURIs do
      begin
        LSearchStr := '>' + LValidURI + '<';
        if Pos(LSearchStr.ToLower, LData.ToLower) > 0 then
        begin
          Result := True;
          Break;
        end;
      end;
    finally
      LReader.Free;
    end;
  finally
    AStream.Position := LOldPosition;
  end;
end;

end.

