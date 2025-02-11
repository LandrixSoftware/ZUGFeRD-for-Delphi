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

unit intf.ZUGFeRDXmlHelper;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes
  ,System.IOUtils,System.Win.COMObj,System.UITypes,System.StrUtils
  ,Xml.xmldom,Xml.XMLDoc,Xml.XMLIntf,Xml.XMLSchema
  ,Xml.Win.msxmldom, Winapi.MSXMLIntf, Winapi.msxml
  ;

type
  TZUGFeRDXmlHelper = class(TObject)
  public type
    TXMLLoadCallback = procedure(Node : IXMLNode) of object;
  public
    class procedure LoadFromChilds(const _NodeName : String; _Node : IXMLNode; _Callback : TXMLLoadCallback);
    class function FindChild(_Node : IXMLNode; const _NodeName : String; out _Result : IXMLNode) : Boolean;
    class function SelectNode(_XnRoot: IXMLDOMNode; const _NodePath: String; out _Result : IXMLDOMNode): Boolean;
    class function SelectNodes(_XnRoot: IXMLDOMNode; const _NodePath: String; out _Result : IXMLDOMNodeList): Boolean;
    class function SelectNodeText(_XnRoot: IXMLDOMNode; const _NodePath: String): String;
    class function FindNode(_XnRoot: IXMLDOMNode; const _NodePath: String): Boolean;
    class function PrepareDocumentForXPathQuerys(_Xml: IXMLDocument; IsZUGFeRD1: boolean = false): IXMLDOMDocument2;
  end;

implementation

{ TXRechnungXMLHelper }

class function TZUGFeRDXmlHelper.SelectNodes(_XnRoot: IXMLDOMNode;
  const _NodePath: String; out _Result : IXMLDOMNodeList): Boolean;
begin
  Result := false;
  _Result := nil;
  if not Assigned(_XnRoot) then
    exit;
  _Result := _XnRoot.selectNodes(_NodePath);
  Result := _Result <> nil;
end;

class function TZUGFeRDXmlHelper.SelectNodeText(_XnRoot: IXMLDOMNode; const _NodePath: String): String;
var
  node : IXMLDOMNode;
begin
  Result := '';
  if TZUGFeRDXmlHelper.SelectNode(_XnRoot,_NodePath,node) then
    Result := node.Text;
end;

class function TZUGFeRDXmlHelper.SelectNode(_XnRoot: IXMLDOMNode;
  const _NodePath: String; out _Result : IXMLDOMNode): Boolean;
begin
  Result := false;
  _Result := nil;
  if not Assigned(_XnRoot) then
    exit;
  _Result := _XnRoot.selectSingleNode(_NodePath);
  Result := _Result <> nil;
end;

class function TZUGFeRDXmlHelper.FindChild(_Node: IXMLNode; const _NodeName: String;
  out _Result: IXMLNode): Boolean;
begin
  Result := false;
  if _Node = nil then
    exit;
  _Result := _Node.ChildNodes.FindNode(_NodeName,'');
  Result := _Result <> nil;
end;

class procedure TZUGFeRDXmlHelper.LoadFromChilds(const _NodeName: String; _Node: IXMLNode;
  _Callback: TXMLLoadCallback);
var
  Node : IXMLNode;
begin
  Node := _Node.ChildNodes.FindNode(_NodeName,'');
  if Node = nil then
    exit;
  _Callback(Node);
end;

class function TZUGFeRDXmlHelper.PrepareDocumentForXPathQuerys(_Xml: IXMLDocument; IsZUGFeRD1: boolean = false): IXMLDOMDocument2;
var
  sNsLine: string;
  ram, rsm, udt, qdt, cac, cbc: string;

  function ReturnNS (Value: string; Default: String; Default1: string = ''): string;
  begin
    Result:= Trim(Value);
    if Result='' then
      if IsZUGFeRD1 and (Default1<>'') then
        Result:= Default1
      else
        Result:= Default
  end;

begin
  Result := nil;
  if not _Xml.Active then
    exit;
  ram:= ReturnNS(_XML.DocumentElement.FindNamespaceURI('ram'), 'urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:100',
                                                               'urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:12');
  rsm:= ReturnNS(_XML.DocumentElement.FindNamespaceURI('rsm'), 'urn:un:unece:uncefact:data:standard:CrossIndustryInvoice:100',
                                                               'urn:ferd:CrossIndustryDocument:invoice:1p0');
  udt:= ReturnNS(_XML.DocumentElement.FindNamespaceURI('udt'), 'urn:un:unece:uncefact:data:standard:UnqualifiedDataType:100',
                                                               'urn:un:unece:uncefact:data:standard:UnqualifiedDataType:15');
  qdt:= ReturnNS(_XML.DocumentElement.FindNamespaceURI('qdt'), 'urn:un:unece:uncefact:data:standard:QualifiedDataType:100');
  cac:= ReturnNS(_XML.DocumentElement.FindNamespaceURI('cac'), 'urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2');
  cbc:= ReturnNS(_XML.DocumentElement.FindNamespaceURI('cbc'), 'urn:oasis:names:specification:ubl:schema:xsd:CommonBasicComponents-2');

  sNsLine := Format('xmlns:qdt="%s" xmlns:ram="%s" xmlns:rsm="%s" xmlns:udt="%s" xmlns:cac="%s" xmlns:cbc="%s"', [qdt, ram, rsm, udt, cac, cbc]);
  Result := CoDOMDocument60.Create;
  Result.preserveWhiteSpace := True; // preserve CRLF on multiline strings also
  Result.loadXML(_Xml.XML.Text);
  Result.setProperty('SelectionLanguage', 'XPath');  // ab 4.0 ist SelectionLanguage eh immer XPath
  Result.setProperty('SelectionNamespaces', sNsLine);
end;


class function TZUGFeRDXmlHelper.FindNode(_XnRoot: IXMLDOMNode; const _NodePath: String): Boolean;
var
  Node : IXMLDOMNode;
begin
  Result := TZUGFeRDXmlHelper.SelectNode(_XnRoot,_NodePath,Node);
end;

end.


