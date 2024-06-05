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

unit intf.ZUGFeRDProfileAwareXmlTextWriter;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  System.StrUtils,
  Xml.xmldom,Xml.XMLDoc,Xml.XMLIntf,Xml.XMLSchema,intf.MSXML2_TLB,
  intf.ZUGFeRDProfile
  ;

type
  TZUGFeRDStackInfo = record
    Profile: TZUGFeRDProfiles;
    IsVisible: Boolean;
  end;

  //https://github.com/codejanovic/Delphi-Serialization

  TZUGFeRDXmlFomatting = (xmlFormatting_None, xmlFormatting_Indented);

  TZUGFeRDProfileAwareXmlTextWriter = class
  private
    TextWriter: IXMLDocument;
    XmlStack: TStack<TZUGFeRDStackInfo>;
    CurrentProfile: TZUGFeRDProfile;

    CurrentEncoding : TEncoding;
    CurrentFilename : String;
    CurrentStream : TStream;
    XmlNodeStack: TStack<IXMLNode>;

    function GetFormatting: TZUGFeRDXmlFomatting;
    procedure SetFormatting(value: TZUGFeRDXmlFomatting);

    function _DoesProfileFitToCurrentProfile(profile: TZUGFeRDProfiles): Boolean;
    function _IsNodeVisible: Boolean;
  public
    property Formatting: TZUGFeRDXmlFomatting read GetFormatting write SetFormatting;

    constructor Create(const filename: string; encoding: TEncoding; profile: TZUGFeRDProfile); overload;
    constructor Create(w: TStream; encoding: TEncoding; profile: TZUGFeRDProfile); overload;
    destructor Destroy; override;

    procedure Close;
    procedure Flush;
    procedure WriteEndElement;
    procedure WriteStartDocument; overload;
    procedure WriteStartDocument(standalone: Boolean); overload;
    procedure WriteEndDocument;
    procedure WriteValue(const value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
    procedure WriteOptionalElementString(const tagName, value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
    procedure WriteElementString(const prefix, localName, ns, value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT); overload;
    procedure WriteElementString(const localName, ns, value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT); overload;
    procedure WriteElementString(const localName, value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT); overload;
    procedure WriteAttributeString(const prefix, localName, ns, value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT); overload;
    procedure WriteAttributeString(const localName, value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT); overload;
    procedure WriteAttributeString(const localName, ns, value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT); overload;
    procedure WriteStartElement(const prefix, localName, ns: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT); overload;
    procedure WriteStartElement(const localName: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT); overload;
    procedure WriteStartElement(const localName, ns: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT); overload;
  end;

implementation

constructor TZUGFeRDProfileAwareXmlTextWriter.Create(const filename: string; encoding: TEncoding; profile: TZUGFeRDProfile);
begin
  inherited Create;
  TextWriter := NewXMLDocument;
  CurrentFilename := filename;
  CurrentEncoding := encoding;
  CurrentStream := nil;

  TextWriter.Active := True;
  TextWriter.Version := '1.0';
  TextWriter.StandAlone := 'yes';
  TextWriter.Encoding := 'UTF-8';
  TextWriter.Options := [doNodeAutoCreate, doAttrNull];

  XmlStack := TStack<TZUGFeRDStackInfo>.Create;
  XmlNodeStack := TStack<IXMLNode>.Create;
  CurrentProfile := profile;
end;

constructor TZUGFeRDProfileAwareXmlTextWriter.Create(w: TStream; encoding: TEncoding; profile: TZUGFeRDProfile);
begin
  inherited Create;
  TextWriter := NewXMLDocument;
  CurrentFilename := '';
  CurrentEncoding := encoding;
  CurrentStream := w;

  TextWriter.Active := True;
  TextWriter.Version := '1.0';
  TextWriter.StandAlone := 'yes';
  TextWriter.Encoding := 'UTF-8';
  TextWriter.Options := [doNodeAutoCreate, doAttrNull];

  XmlStack := TStack<TZUGFeRDStackInfo>.Create;
  XmlNodeStack := TStack<IXMLNode>.Create;
  CurrentProfile := profile;
end;

destructor TZUGFeRDProfileAwareXmlTextWriter.Destroy;
begin
  TextWriter := nil;
  if Assigned(XmlStack) then begin XmlStack.Free; XmlStack := nil; end;
  if Assigned(XmlNodeStack) then begin XmlNodeStack.Free; XmlNodeStack := nil; end;
  inherited;
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.Close;
begin
//  TextWriter.Close;
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.Flush;
begin
  if CurrentFilename <> '' then
  begin
    if not System.SysUtils.DirectoryExists(ExtractFilePath(CurrentFilename)) then
      exit;
    TextWriter.SaveToFile(CurrentFilename);
  end else
  if (CurrentStream <> nil) then
  begin
    TextWriter.SaveToStream(CurrentStream);
  end;
end;

function TZUGFeRDProfileAwareXmlTextWriter.GetFormatting: TZUGFeRDXmlFomatting;
begin
  if (doNodeAutoIndent in TXMLDocument(TextWriter).Options) then
    Result := TZUGFeRDXmlFomatting.xmlFormatting_Indented
  else
    Result := TZUGFeRDXmlFomatting.xmlFormatting_None;
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.SetFormatting(value: TZUGFeRDXmlFomatting);
begin
  case value of
    xmlFormatting_None: TXMLDocument(TextWriter).Options := TXMLDocument(TextWriter).Options - [doNodeAutoIndent];
    xmlFormatting_Indented: TXMLDocument(TextWriter).Options := TXMLDocument(TextWriter).Options + [doNodeAutoIndent];
  end;
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteStartElement(const prefix, localName, ns: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
var
  _profile: TZUGFeRDProfiles;
  info: TZUGFeRDStackInfo;
  xmlNode : IXMLNode;
begin
  _profile := profile;
  if profile = TZUGFERDPROFILES_DEFAULT then
    _profile := [CurrentProfile];

  if (not _IsNodeVisible) or (not _DoesProfileFitToCurrentProfile(_profile)) then
  begin
    info.Profile := _profile;
    info.IsVisible := false;
    XmlStack.Push(info);
    exit;
  end;

  info.Profile := _profile;
  info.IsVisible := True;
  XmlStack.Push(info);

  // write value
  //TODO  if (!String.IsNullOrWhiteSpace(prefix))
  //  {
  //      this.TextWriter?.WriteStartElement(prefix, localName, ns);
  //  }
  //  else if (!String.IsNullOrWhiteSpace(ns))
  //  {
  //      this.TextWriter?.WriteStartElement(localName, ns);
  //  }
  //  else
  begin
    if XMLNodeStack.Count = 0 then
      xmlNode := TextWriter.AddChild(localName)
    else
      xmlNode := XMLNodeStack.Peek.AddChild(localName);
    XmlNodeStack.Push(xmlNode);
  end;
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteEndElement;
var
  infoForCurrentXmlLevel: TZUGFeRDStackInfo;
begin
  infoForCurrentXmlLevel := XmlStack.Pop;
  if (_DoesProfileFitToCurrentProfile(infoForCurrentXmlLevel.Profile) and _IsNodeVisible()) then
  begin
    XmlNodeStack.Pop;
  end;
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteOptionalElementString(
  const tagName, value: string; profile: TZUGFeRDProfiles);
begin
  if (value <> '') then
    WriteElementString(tagName, value, profile);
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteElementString(const prefix, localName, ns, value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
var
  _profile: TZUGFeRDProfiles;
begin
  _profile := profile;
  if profile = TZUGFERDPROFILES_DEFAULT then
    _profile := [CurrentProfile];

  if (not _IsNodeVisible) or (not _DoesProfileFitToCurrentProfile(_profile)) then
    exit;

  XMLNodeStack.Peek.AddChild(localName).Text := value;
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteStartDocument;
begin
//  XmlStack.Push(TextWriter.DocumentElement);
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteStartDocument(standalone: Boolean);
begin
//  XmlStack.Push(TextWriter.DocumentElement);
//  TextWriter.WriteStartDocument(standalone);
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteEndDocument;
begin
//  XmlStack.Pop;
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteAttributeString(const prefix, localName, ns, value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
var
  currentNode : IXMLNode;
  infoForCurrentNode: TZUGFeRDStackInfo;
begin
  infoForCurrentNode := XmlStack.Peek;

  if not infoForCurrentNode.IsVisible then
    exit;

  if not _DoesProfileFitToCurrentProfile(profile) then
    exit;

  currentNode := XmlNodeStack.Peek;
  if currentNode = nil then
    exit;
  currentNode.SetAttributeNS(IfThen(prefix<>'',prefix+':','')+ localName,'',value);
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteValue(const value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
var
  infoForCurrentNode: TZUGFeRDStackInfo;
begin
  infoForCurrentNode := XmlStack.Peek;
  if not infoForCurrentNode.IsVisible then
    exit;

  XmlNodeStack.Peek.Text := value;
end;

function TZUGFeRDProfileAwareXmlTextWriter._DoesProfileFitToCurrentProfile(
  profile: TZUGFeRDProfiles): Boolean;
var
  maskedProfile: Integer;
  i : TZUGFeRDProfile;
begin
  if profile = TZUGFERDPROFILES_DEFAULT then
    Exit(true);

  for i := Low(TZUGFeRDProfile) to High(TZUGFeRDProfile) do
  begin
    if i = TZUGFeRDProfile.Unknown then
      continue;
    if not (i in profile) then
      continue;
    maskedProfile := Integer(i) and Integer(CurrentProfile);
    if maskedProfile = Integer(CurrentProfile) then
    begin
      Result := True;
      exit;
    end;
  end;

  Result := false; // profile does not fit
end;

function TZUGFeRDProfileAwareXmlTextWriter._IsNodeVisible: Boolean;
var
  stackInfo: TZUGFeRDStackInfo;
begin
  Result := false;
  for stackInfo in XmlStack do
  begin
    if not stackInfo.IsVisible then
      exit;
  end;
  Result := True;
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteElementString(const localName, ns, value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
begin
  WriteElementString('', localName, ns, value, profile);
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteElementString(const localName, value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
begin
  WriteElementString('', localName, '', value, profile);
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteAttributeString(const localName, value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
begin
  WriteAttributeString('', localName, '', value, profile);
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteAttributeString(const localName, ns, value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
begin
  WriteAttributeString('', localName, ns, value, profile);
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteStartElement(const localName: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
begin
  WriteStartElement('', localName, '', profile);
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteStartElement(const localName, ns: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
begin
  WriteStartElement('', localName, ns, profile);
end;

end.
