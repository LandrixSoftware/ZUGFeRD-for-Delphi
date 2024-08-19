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

unit intf.ZUGFeRDDataTypeReader;

interface

uses
  System.Classes, System.SysUtils, System.IOUtils, System.DateUtils
  ,Xml.XMLDoc, Xml.xmldom, Xml.XMLIntf
  ,Xml.Win.msxmldom, Winapi.MSXMLIntf, Winapi.msxml
  ,intf.ZUGFeRDExceptions
  ,intf.ZUGFeRDXmlUtils
  ;

Wird nicht benutzt, sehe momentan keinen Sinn hinter dieser Helperklasse

type
  /// <summary>
  /// Common reader for both qdt and udt data types
  /// </summary>
  TZUGFeRDDataTypeReader = class(TObject)
  public
    class function ReadFormattedIssueDateTime(node: IXmlDomNode; const xpath: string; {nsmgr: XmlNamespaceManager = nil; } defaultValue: TDateTime = 0): TDateTime;
  end;

implementation

{ TZUGFeRDDataTypeReader }

class function TZUGFeRDDataTypeReader.ReadFormattedIssueDateTime(
  node: IXmlDomNode; const xpath: string; defaultValue: TDateTime): TDateTime;
var
  selectedNode: IXmlDomNode;
begin
  Result := defaultValue;

  if node = nil then
    exit;

  try
    selectedNode := node.SelectSingleNode(xpath{, nsmgr});
    if selectedNode = nil then
      exit;
    if Pos('<qdt:',LowerCase(selectedNode.xml))>0 then
      Result := TZUGFeRDXmlUtils.NodeAsDateTime(selectedNode,'./qdt:DateTimeString')
    else
    if Pos('<udt:',LowerCase(selectedNode.xml))>0 then
      Result := TZUGFeRDXmlUtils.NodeAsDateTime(selectedNode,'./udt:DateTimeString');
  except
    on ex: Exception do
      raise ex;
  end;
end;

end.
