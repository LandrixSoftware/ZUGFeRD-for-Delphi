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
  Xml.XMLDoc, Xml.xmldom, Xml.XMLIntf,
  intf.ZUGFeRDProfile
  ;

type
  TZUGFeRDStackInfo = record
    Profile: TZUGFeRDProfile;
    IsVisible: Boolean;
  end;

  //https://github.com/codejanovic/Delphi-Serialization

  TZUGFeRDXmlFomatting = (xmlFormatting_None, xmlFormatting_Indented);

  TZUGFeRDProfileAwareXmlTextWriter = class
  private
    //TextWriter: TXmlTextWriter;
    XmlStack: TStack<TZUGFeRDStackInfo>;
    CurrentProfile: TZUGFeRDProfile;

    function GetFormatting: TZUGFeRDXmlFomatting;
    procedure SetFormatting(value: TZUGFeRDXmlFomatting);

    function DoesProfileFitToCurrentProfile(profile: TZUGFeRDProfile): Boolean;
    function IsNodeVisible: Boolean;
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
    procedure WriteValue(const value: string; profile: TZUGFeRDProfile = Unknown);
    procedure WriteOptionalElementString(const tagName, value: string; profile: TZUGFeRDProfile = Unknown);
    procedure WriteElementString(const prefix, localName, ns, value: string; profile: TZUGFeRDProfile = Unknown); overload;
    procedure WriteElementString(const localName, ns, value: string; profile: TZUGFeRDProfile = Unknown); overload;
    procedure WriteElementString(const localName, value: string; profile: TZUGFeRDProfile = Unknown); overload;
    procedure WriteAttributeString(const prefix, localName, ns, value: string; profile: TZUGFeRDProfile = Unknown); overload;
    procedure WriteAttributeString(const localName, value: string; profile: TZUGFeRDProfile = Unknown); overload;
    procedure WriteAttributeString(const localName, ns, value: string; profile: TZUGFeRDProfile = Unknown); overload;
    procedure WriteStartElement(const prefix, localName, ns: string; profile: TZUGFeRDProfile = Unknown); overload;
    procedure WriteStartElement(const localName: string; profile: TZUGFeRDProfile = Unknown); overload;
    procedure WriteStartElement(const localName, ns: string; profile: TZUGFeRDProfile = Unknown); overload;
  end;

implementation

constructor TZUGFeRDProfileAwareXmlTextWriter.Create(const filename: string; encoding: TEncoding; profile: TZUGFeRDProfile);
begin
  inherited Create;
  //TextWriter := TXmlTextWriter.Create(filename, encoding);
  XmlStack := TStack<TZUGFeRDStackInfo>.Create;
  CurrentProfile := profile;
end;

constructor TZUGFeRDProfileAwareXmlTextWriter.Create(w: TStream; encoding: TEncoding; profile: TZUGFeRDProfile);
begin
  inherited Create;
  //TextWriter := TXmlTextWriter.Create(w, encoding);
  XmlStack := TStack<TZUGFeRDStackInfo>.Create;
  CurrentProfile := profile;
end;

destructor TZUGFeRDProfileAwareXmlTextWriter.Destroy;
begin
  //if Assigned(TextWriter) then begin TextWriter.Free; TextWriter := nil; end;
  if Assigned(XmlStack) then begin XmlStack.Free; XmlStack := nil; end;
  inherited;
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.Close;
begin
//  TextWriter.Close;
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.Flush;
begin
//  TextWriter.Flush;
end;

function TZUGFeRDProfileAwareXmlTextWriter.GetFormatting: TZUGFeRDXmlFomatting;
begin
  //Result := TextWriter.Formatting;
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.SetFormatting(value: TZUGFeRDXmlFomatting);
begin
  //TextWriter.Formatting := value;
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteStartElement(const prefix, localName, ns: string; profile: TZUGFeRDProfile = Unknown);
//var
//  _profile: TZUGFeRDProfile;
//  info: TStackInfo;
begin
//  _profile := profile;
//  if profile = Unknown then
//    _profile := CurrentProfile;
//
//  if not _IsNodeVisible or not _DoesProfileFitToCurrentProfile(_profile) then
//  begin
//    info.Profile := _profile;
//    info.IsVisible := False;
//    XmlStack.Push(info);
//    Exit;
//  end;
//
//  info.Profile := _profile;
//  info.IsVisible := True;
//  XmlStack.Push(info);
//
//  if not string.IsNullOrEmpty(prefix) then
//    TextWriter.WriteStartElement(prefix, localName, ns)
//  else if not string.IsNullOrEmpty(ns) then
//    TextWriter.WriteStartElement(localName, ns)
//  else
//    TextWriter.WriteStartElement(localName);


//        public void WriteStartElement(string prefix, string localName, string ns, Profile profile = Profile.Unknown)
//        {
//            Profile _profile = profile;
//            if (profile == Profile.Unknown)
//            {
//                _profile = this.CurrentProfile;
//            }
//
//            if (!_IsNodeVisible() || !_DoesProfileFitToCurrentProfile(_profile))
//            {
//                this.XmlStack.Push(new StackInfo() { Profile = _profile, IsVisible = false });
//                return;
//            }
//            else
//            {
//                this.XmlStack.Push(new StackInfo() { Profile = _profile, IsVisible = true });
//            }
//
//            // write value
//            if (!String.IsNullOrEmpty(prefix))
//            {
//                this.TextWriter?.WriteStartElement(prefix, localName, ns);
//            }
//            else if (!String.IsNullOrEmpty(ns))
//            {
//                this.TextWriter?.WriteStartElement(localName, ns);
//            }
//            else
//            {
//                this.TextWriter?.WriteStartElement(localName);
//            }
//        } // !WriteStartElement()

end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteEndElement;
var
  infoForCurrentXmlLevel: TZUGFeRDStackInfo;
begin
  infoForCurrentXmlLevel := XmlStack.Pop;
  if DoesProfileFitToCurrentProfile(infoForCurrentXmlLevel.Profile) and IsNodeVisible then
    //TextWriter.WriteEndElement;
end;
procedure TZUGFeRDProfileAwareXmlTextWriter.WriteOptionalElementString(
  const tagName, value: string; profile: TZUGFeRDProfile);
begin
  if (value <> '') then
    WriteElementString(tagName, value, profile);
end;

//
procedure TZUGFeRDProfileAwareXmlTextWriter.WriteElementString(const prefix, localName, ns, value: string; profile: TZUGFeRDProfile = Unknown);
//var
//  _profile: TZUGFeRDProfile;
begin
//  _profile := profile;
//  if profile = Unknown then
//    _profile := CurrentProfile;
//
//  if not _IsNodeVisible or not _DoesProfileFitToCurrentProfile(_profile) then
//    Exit;
//
//  if not string.IsNullOrEmpty(prefix) then
//    TextWriter.WriteElementString(prefix, localName, ns, value)
//  else if not string.IsNullOrEmpty(ns) then
//    TextWriter.WriteElementString(localName, ns, value)
//  else
//    TextWriter.WriteElementString(localName, value);


//        public void WriteElementString(string prefix, string localName, string ns, string value, Profile profile = Profile.Unknown)
//        {
//            Profile _profile = profile;
//            if (profile == Profile.Unknown)
//            {
//                _profile = this.CurrentProfile;
//            }
//
//            if (!_IsNodeVisible() || !_DoesProfileFitToCurrentProfile(_profile))
//            {
//                return;
//            }
//
//            // write value
//            if (!String.IsNullOrEmpty(prefix))
//            {
//                this.TextWriter?.WriteElementString(prefix, localName, ns, value);
//            }
//            else if (!String.IsNullOrEmpty(ns))
//            {
//                this.TextWriter?.WriteElementString(localName, ns, value);
//            }
//            else
//            {
//                this.TextWriter?.WriteElementString(localName, value);
//            }
//        } // !WriteElementString()

end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteStartDocument;
begin
//  TextWriter.WriteStartDocument;
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteStartDocument(standalone: Boolean);
begin
//  TextWriter.WriteStartDocument(standalone);
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteEndDocument;
begin
//  TextWriter.WriteEndDocument;
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteAttributeString(const prefix, localName, ns, value: string; profile: TZUGFeRDProfile = Unknown);
//var
//  infoForCurrentNode: TStackInfo;
begin
//  infoForCurrentNode := XmlStack.First;
//  if not infoForCurrentNode.IsVisible then
//    Exit;
//
//  if profile <> Unknown then
//  begin
//    if (profile and CurrentProfile) <> CurrentProfile then
//      Exit;
//  end;
//
//  if not string.IsNullOrEmpty(prefix) then
//    TextWriter.WriteAttributeString(prefix, localName, ns, value)
//  else if not string.IsNullOrEmpty(ns) then
//    TextWriter.WriteAttributeString(localName, ns, value)
//  else
//    TextWriter.WriteAttributeString(localName, value);

//        public void WriteAttributeString(string prefix, string localName, string ns, string value, Profile profile = Profile.Unknown)
//        {
//            StackInfo infoForCurrentNode = this.XmlStack.First();
//            if (!infoForCurrentNode.IsVisible)
//            {
//                return;
//            }
//
//            if (profile != Profile.Unknown)
//            {
//                Profile bitmask = profile & this.CurrentProfile;
//                if (bitmask != this.CurrentProfile)
//                {
//                    return;
//                }
//            }
//
//            // write value
//            if (!String.IsNullOrEmpty(prefix))
//            {
//                this.TextWriter?.WriteAttributeString(prefix, localName, ns, value);
//            }
//            else if (!String.IsNullOrEmpty(ns))
//            {
//                this.TextWriter?.WriteAttributeString(localName, ns, value);
//            }
//            else
//            {
//                this.TextWriter?.WriteAttributeString(localName, value);
//            }
//        } // !WriteAttributeString()
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteValue(const value: string; profile: TZUGFeRDProfile = Unknown);
//var
//  infoForCurrentNode: TStackInfo;
begin
//  infoForCurrentNode := XmlStack.First;
//  if not infoForCurrentNode.IsVisible then
//    Exit;
//
//  TextWriter.WriteValue(value);
end;

function TZUGFeRDProfileAwareXmlTextWriter.DoesProfileFitToCurrentProfile(profile: TZUGFeRDProfile): Boolean;
var
  maskedProfile: TZUGFeRDProfile;
begin
//  if profile <> Unknown then
//  begin
//    maskedProfile := profile and CurrentProfile;
//    if maskedProfile <> CurrentProfile then
//      Exit(False);
//  end;
//
//  Result := True;
end;

function TZUGFeRDProfileAwareXmlTextWriter.IsNodeVisible: Boolean;
//var
//  stackInfo: TStackInfo;
begin
//  for stackInfo in XmlStack do
//  begin
//    if not stackInfo.IsVisible then
//      Exit(False);
//  end;
//
//  Result := True;
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteElementString(const localName, ns, value: string; profile: TZUGFeRDProfile = Unknown);
begin
  WriteElementString('', localName, ns, value, profile);
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteElementString(const localName, value: string; profile: TZUGFeRDProfile = Unknown);
begin
  WriteElementString('', localName, '', value, profile);
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteAttributeString(const localName, value: string; profile: TZUGFeRDProfile = Unknown);
begin
  WriteAttributeString('', localName, '', value, profile);
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteAttributeString(const localName, ns, value: string; profile: TZUGFeRDProfile = Unknown);
begin
  WriteAttributeString('', localName, ns, value, profile);
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteStartElement(const localName: string; profile: TZUGFeRDProfile = Unknown);
begin
  WriteStartElement('', localName, '', profile);
end;

procedure TZUGFeRDProfileAwareXmlTextWriter.WriteStartElement(const localName, ns: string; profile: TZUGFeRDProfile = Unknown);
begin
  WriteStartElement('', localName, ns, profile);
end;

end.
