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
  Xml.xmldom,Xml.XMLDoc,Xml.XMLIntf,Xml.XMLSchema,
  Xml.Win.msxmldom, Winapi.MSXMLIntf, Winapi.msxml,
  intf.ZUGFeRDProfile
  ;

type
  TZUGFeRDStackInfo = class
    Profile: TZUGFeRDProfiles;
    IsVisible: Boolean;
    Prefix: string;
    LocalName: string;
    NS: string;
    IsWritten: Boolean;
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
    Namespaces: TDictionary<string, string>;
    NeedToIndentEndElement: Boolean;
    AutomaticallyCleanInvalidXmlCharacters: Boolean;

    function GetFormatting: TZUGFeRDXmlFomatting;
    procedure SetFormatting(value: TZUGFeRDXmlFomatting);

    procedure _FlushPendingStartElements;
    function _DoesProfileFitToCurrentProfile(profiles: TZUGFeRDProfiles): Boolean;
    function _IsNodeVisible: Boolean;
    function _CleanInvalidXmlChars(const input: string): string;
    function _IsValidXmlString(const input: string): Boolean;
    function _IsValidXmlChar(c: Char): Boolean;
  public
    property Formatting: TZUGFeRDXmlFomatting read GetFormatting write SetFormatting;

    constructor Create(const filename: string; encoding: TEncoding; profile: TZUGFeRDProfile; _automaticallyCleanInvalidXmlCharacters: Boolean = False); overload;
    constructor Create(w: TStream; encoding: TEncoding; profile: TZUGFeRDProfile; _automaticallyCleanInvalidXmlCharacters: Boolean = False); overload;
    destructor Destroy; override;

    procedure Close;
    procedure Flush;
    procedure WriteEndElement;
    procedure WriteStartDocument; overload;
    procedure WriteStartDocument(standalone: Boolean); overload;
    procedure WriteEndDocument;
    procedure WriteValue(const value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
    procedure WriteOptionalElementString(const tagName, value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT); overload;
    procedure WriteOptionalElementString(const prefix, tagName, value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT); overload;
    procedure WriteElementString(const prefix, localName, ns, value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT); overload;
    procedure WriteElementString(const localName, ns, value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT); overload;
    procedure WriteElementString(const localName, value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT); overload;
    procedure WriteAttributeString(const prefix, localName, ns, value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT); overload;
    procedure WriteAttributeString(const localName, value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT); overload;
    procedure WriteAttributeString(const localName, ns, value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT); overload;
    procedure WriteStartElement(const prefix, localName, ns: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT); overload;
    procedure WriteStartElement(const localName: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT); overload;
    procedure WriteStartElement(const localName, ns: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT); overload;
    procedure WriteRawString(const value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
    procedure WriteRawIndention(profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
    procedure WriteComment(const comment: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
    procedure SetNamespaces(_namespaces: TDictionary<string, string>);
  end;

implementation

constructor TZUGFeRDProfileAwareXmlTextWriter.Create(const filename: string; encoding: TEncoding; profile: TZUGFeRDProfile; _automaticallyCleanInvalidXmlCharacters: Boolean = False);
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
  Namespaces := TDictionary<string, string>.Create;
  NeedToIndentEndElement := False;
  AutomaticallyCleanInvalidXmlCharacters := _automaticallyCleanInvalidXmlCharacters;
  CurrentProfile := profile;
end;

constructor TZUGFeRDProfileAwareXmlTextWriter.Create(w: TStream; encoding: TEncoding; profile: TZUGFeRDProfile; _automaticallyCleanInvalidXmlCharacters: Boolean = False);
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
  Namespaces := TDictionary<string, string>.Create;
  NeedToIndentEndElement := False;
  AutomaticallyCleanInvalidXmlCharacters := _automaticallyCleanInvalidXmlCharacters;
  CurrentProfile := profile;
end;

destructor TZUGFeRDProfileAwareXmlTextWriter.Destroy;
var
  info: TZUGFeRDStackInfo;
begin
  TextWriter := nil;
  if Assigned(XmlStack) then
  begin
    while XmlStack.Count > 0 do
    begin
      info := XmlStack.Pop;
      info.Free;
    end;
    XmlStack.Free;
    XmlStack := nil;
  end;
  if Assigned(XmlNodeStack) then begin XmlNodeStack.Free; XmlNodeStack := nil; end;
  if Assigned(Namespaces) then begin Namespaces.Free; Namespaces := nil; end;
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
begin
  _profile := profile;
  if profile = TZUGFERDPROFILES_DEFAULT then
    _profile := [CurrentProfile];

  info := TZUGFeRDStackInfo.Create;
  info.Profile := _profile;
  info.Prefix := prefix;
  info.LocalName := localName;
  info.NS := ns;

  if (not _IsNodeVisible) or (not _DoesProfileFitToCurrentProfile(_profile)) then
  begin
    info.IsVisible := False;
    XmlStack.Push(info);
    Exit;
  end;

  info.IsVisible := True;
  XmlStack.Push(info);
  // Don't write to XML yet - deferred until content is written
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteEndElement;
var
  infoForCurrentXmlLevel: TZUGFeRDStackInfo;
begin
  infoForCurrentXmlLevel := XmlStack.Pop;
  try
    if infoForCurrentXmlLevel.IsWritten then
    begin
      if NeedToIndentEndElement then
      begin
        WriteRawIndention;
        NeedToIndentEndElement := False;
      end;
      XmlNodeStack.Pop;
    end;
  finally
    infoForCurrentXmlLevel.Free;
  end;
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteOptionalElementString(
  const tagName, value: string; profile: TZUGFeRDProfiles);
var
  cleanedValue: string;
begin
  if value = '' then
    Exit;

  cleanedValue := value;
  if not _IsValidXmlString(cleanedValue) then
  begin
    if AutomaticallyCleanInvalidXmlCharacters then
      cleanedValue := _CleanInvalidXmlChars(cleanedValue)
    else
      raise Exception.CreateFmt('''%s'' contains illegal characters for xml.', [cleanedValue]);
  end;

  WriteElementString(tagName, cleanedValue, profile);
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteElementString(const prefix, localName, ns, value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
var
  _profile: TZUGFeRDProfiles;
  cleanedValue: string;
begin
  cleanedValue := value;
  if not _IsValidXmlString(cleanedValue) then
  begin
    if AutomaticallyCleanInvalidXmlCharacters then
      cleanedValue := _CleanInvalidXmlChars(cleanedValue)
    else
      raise Exception.CreateFmt('''%s'' contains illegal characters for xml.', [cleanedValue]);
  end;

  _profile := profile;
  if profile = TZUGFERDPROFILES_DEFAULT then
    _profile := [CurrentProfile];

  if (not _IsNodeVisible) or (not _DoesProfileFitToCurrentProfile(_profile)) then
    exit;

  _FlushPendingStartElements;

  XMLNodeStack.Peek.AddChild(localName).Text := cleanedValue;
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

  _FlushPendingStartElements;

  currentNode := XmlNodeStack.Peek;
  if currentNode = nil then
    exit;
  currentNode.SetAttributeNS(IfThen(prefix<>'',prefix+':','')+ localName,'',value);
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteValue(const value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
var
  infoForCurrentNode: TZUGFeRDStackInfo;
  cleanedValue: string;
begin
  cleanedValue := value;
  if not _IsValidXmlString(cleanedValue) then
  begin
    if AutomaticallyCleanInvalidXmlCharacters then
      cleanedValue := _CleanInvalidXmlChars(cleanedValue)
    else
      raise Exception.CreateFmt('''%s'' contains illegal characters for xml.', [cleanedValue]);
  end;

  if XmlStack.Count = 0 then
    Exit;

  infoForCurrentNode := XmlStack.Peek;
  if not infoForCurrentNode.IsVisible then
    exit;

  _FlushPendingStartElements;

  XmlNodeStack.Peek.Text := cleanedValue;
end;

procedure TZUGFeRDProfileAwareXmlTextWriter._FlushPendingStartElements;
var
  items: TArray<TZUGFeRDStackInfo>;
  i: Integer;
  info: TZUGFeRDStackInfo;
  namespaceURI: string;
  xmlNode: IXMLNode;
begin
  items := XmlStack.ToArray;
  // items[0] = top (current), items[Length-1] = bottom (root)
  // iterate from root to current so start elements are written in document order
  for i := Length(items) - 1 downto 0 do
  begin
    info := items[i];
    if not info.IsVisible then
      Exit;

    if info.IsWritten then
      Continue;

    // Resolve namespace
    namespaceURI := info.NS;
    if (info.Prefix <> '') and Namespaces.ContainsKey(info.Prefix) then
      namespaceURI := Namespaces[info.Prefix];

    // Create the XML node
    if XmlNodeStack.Count = 0 then
    begin
      if (info.Prefix <> '') or (namespaceURI <> '') then
        xmlNode := TextWriter.AddChild(info.LocalName, namespaceURI)
      else
        xmlNode := TextWriter.AddChild(info.LocalName);
    end
    else
    begin
      if (info.Prefix <> '') or (namespaceURI <> '') then
        xmlNode := XmlNodeStack.Peek.AddChild(info.LocalName, namespaceURI)
      else
        xmlNode := XmlNodeStack.Peek.AddChild(info.LocalName);
    end;
    XmlNodeStack.Push(xmlNode);

    info.IsWritten := True;
  end;
end;

function TZUGFeRDProfileAwareXmlTextWriter._DoesProfileFitToCurrentProfile(profiles: TZUGFeRDProfiles): Boolean;
begin
  if (profiles = TZUGFERDPROFILES_DEFAULT)
  or (CurrentProfile in profiles) then
    Result:= true
  else
    Result:= false;
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

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteOptionalElementString(
  const prefix, tagName, value: string; profile: TZUGFeRDProfiles);
begin
  if value <> '' then
    WriteElementString(prefix, tagName, value, profile);
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteRawString(
  const value: string; profile: TZUGFeRDProfiles);
var
  infoForCurrentNode: TZUGFeRDStackInfo;
  cleanedValue: string;
begin
  cleanedValue := value;
  if not _IsValidXmlString(cleanedValue) then
  begin
    if AutomaticallyCleanInvalidXmlCharacters then
      cleanedValue := _CleanInvalidXmlChars(cleanedValue)
    else
      raise Exception.CreateFmt('''%s'' contains illegal characters for xml.', [cleanedValue]);
  end;

  if XmlStack.Count = 0 then
    Exit;

  infoForCurrentNode := XmlStack.Peek;
  if not infoForCurrentNode.IsVisible then
    Exit;

  _FlushPendingStartElements;
  NeedToIndentEndElement := True;

  // Write value as text to current node
  if XmlNodeStack.Count > 0 then
    XmlNodeStack.Peek.Text := XmlNodeStack.Peek.Text + cleanedValue;
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteRawIndention(
  profile: TZUGFeRDProfiles);
var
  infoForCurrentNode: TZUGFeRDStackInfo;
  i: Integer;
  indentStr: string;
begin
  if XmlStack.Count = 0 then
    Exit;

  infoForCurrentNode := XmlStack.Peek;
  if not infoForCurrentNode.IsVisible then
    Exit;

  _FlushPendingStartElements;
  NeedToIndentEndElement := True;

  // Write indention according to current xml tree position
  // In Delphi, indention is typically handled automatically by the XML framework
  // This is a simplified implementation
  if XmlNodeStack.Count > 0 then
  begin
    indentStr := '';
    for i := 0 to XmlStack.Count - 1 do
      indentStr := indentStr + '  '; // Default indent is 2 spaces

    if XmlNodeStack.Peek.Text <> '' then
      XmlNodeStack.Peek.Text := XmlNodeStack.Peek.Text + indentStr;
  end;
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteComment(
  const comment: string; profile: TZUGFeRDProfiles);
var
  infoForCurrentNode: TZUGFeRDStackInfo;
  cleanedComment: string;
begin
  cleanedComment := comment;
  if not _IsValidXmlString(cleanedComment) then
  begin
    if AutomaticallyCleanInvalidXmlCharacters then
      cleanedComment := _CleanInvalidXmlChars(cleanedComment)
    else
      raise Exception.CreateFmt('''%s'' contains illegal characters for xml.', [cleanedComment]);
  end;

  if XmlStack.Count > 0 then
  begin
    infoForCurrentNode := XmlStack.Peek;
    if not infoForCurrentNode.IsVisible then
      Exit;
  end;

  _FlushPendingStartElements;

  // Write XML comment
  // Note: Delphi's IXMLNode doesn't directly support comments in the same way
  // This would require using the underlying DOM interface
  // Simplified implementation - actual implementation would depend on XML framework
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.SetNamespaces(_namespaces: TDictionary<string, string>);
begin
  if Assigned(Namespaces) then
    Namespaces.Free;
  Namespaces := _namespaces;
end;

function TZUGFeRDProfileAwareXmlTextWriter._CleanInvalidXmlChars(const input: string): string;
var
  output: TStringBuilder;
  c: Char;
begin
  output := TStringBuilder.Create(Length(input));
  try
    for c in input do
    begin
      if _IsValidXmlChar(c) then
        output.Append(c);
    end;
    Result := output.ToString;
  finally
    output.Free;
  end;
end;

function TZUGFeRDProfileAwareXmlTextWriter._IsValidXmlString(const input: string): Boolean;
var
  c: Char;
begin
  if input = '' then
    Exit(True); // empty strings are valid

  for c in input do
  begin
    if not _IsValidXmlChar(c) then
      Exit(False);
  end;
  Result := True;
end;

function TZUGFeRDProfileAwareXmlTextWriter._IsValidXmlChar(c: Char): Boolean;
var
  Code: Word;
begin
  Code := Ord(c);

  Result :=
    // XML 1.0 erlaubte Steuerzeichen
    (Code = $09) or (Code = $0A) or (Code = $0D) or

    // BMP ohne Surrogates
    ((Code >= $20) and (Code <= $D7FF)) or
    ((Code >= $E000) and (Code <= $FFFD)) or

    //Surrogates zulassen (Teil eines gültigen Paares)
    ((Code >= $D800) and (Code <= $DFFF));

  // Note: Characters >= $10000 would require surrogate pair handling in Delphi
end;

end.
