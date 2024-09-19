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
    class function PrepareDocumentForXPathQuerys(_Xml : IXMLDocument) : IXMLDOMDocument2;
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

class function TZUGFeRDXmlHelper.PrepareDocumentForXPathQuerys(_Xml: IXMLDocument): IXMLDOMDocument2;
var
  hList: IDOMNodeList;
  i: Integer;
  s, sNSN, sNSUri: string;
  sNsLine: string;
begin
  Result := nil;
  if not _Xml.Active then
    exit;

  hList := (_Xml.DOMDocument as IDOMNodeSelect).selectNodes('//namespace::*');
  try
    for i := 0 to hList.length - 1 do
    begin
      sNSN := StringReplace(hList.item[i].nodeName, 'xmlns:', '', []);
      if sNSN = 'xml' then
      begin  // wenn es als xmlns:xml hinzugefuegt wird bekommt man die meldung das der Namespacename xml nicht verwendet werden darf...
        sNSN := 'xmlns:MyXml';
        sNSUri := hList.item[i].nodeValue;
      end
      else
      if sNSN = 'xmlns' then
      begin  // den Default Namespace mit einem Namen versehen, damit XPath drauf zugreifen kann.
        sNSN := 'xmlns:dn';
        sNSUri := hList.item[i].nodeValue;
      end
      else
      begin  // alle anderen Namespace auch fuer XPath bekannt machen
        sNSN := hList.item[i].nodeName;
        sNSUri := hList.item[i].nodeValue;
      end;
      s := sNSN + '="'+sNSUri+'"';
      if ContainsText(sNsLine, s) then
        continue;
      if ContainsText(sNsLine,sNSN+'="') then
        continue;
      sNsLine := ' '+s + sNsLine;
    end;

    if not ContainsText(sNsLine, 'xmlns:qdt="urn:un:unece:uncefact:data:standard:QualifiedDataType:100"') then
      sNsLine := sNsLine+ ' xmlns:qdt="urn:un:unece:uncefact:data:standard:QualifiedDataType:100"';

    sNsLine := trim(sNsLine);
  finally
    hList := nil;
  end;

  Result := CoDOMDocument60.Create;
  Result.loadXML(_Xml.XML.Text);
  Result.setProperty('SelectionLanguage', 'XPath');  // ab 4.0 ist SelectionLanguage eh immer XPath
  Result.setProperty('SelectionNamespaces', sNsLine) ;
end;

class function TZUGFeRDXmlHelper.FindNode(_XnRoot: IXMLDOMNode; const _NodePath: String): Boolean;
var
  Node : IXMLDOMNode;
begin
  Result := TZUGFeRDXmlHelper.SelectNode(_XnRoot,_NodePath,Node);
end;

end.


