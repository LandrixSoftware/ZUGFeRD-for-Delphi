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
  ,Xml.XMLDoc, Xml.xmldom, Xml.XMLIntf
  ,Xml.Win.msxmldom, Winapi.MSXMLIntf, Winapi.msxml
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

