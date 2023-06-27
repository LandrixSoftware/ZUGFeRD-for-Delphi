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

type
  TZUGFeRDStackInfo = record
    Profile: TProfile;
    IsVisible: Boolean;
  end;

  TZUGFeRDProfileAwareXmlTextWriter = class
  private
    TextWriter: TXmlTextWriter;
    XmlStack: TStack<StackInfo>;
    CurrentProfile: TProfile;

    function GetFormatting: TXmlFormatting;
    procedure SetFormatting(value: TXmlFormatting);

    function _DoesProfileFitToCurrentProfile(profile: TProfile): Boolean;
    function _IsNodeVisible: Boolean;
  public
    property Formatting: TXmlFormatting read GetFormatting write SetFormatting;

    constructor Create(const filename: string; encoding: TEncoding; profile: TProfile);
    constructor Create(w: TStream; encoding: TEncoding; profile: TProfile);

    procedure Close;
    procedure Flush;
    procedure WriteStartElement(const prefix, localName, ns: string; profile: TProfile = Unknown);
    procedure WriteEndElement;
    procedure WriteElementString(const prefix, localName, ns, value: string; profile: TProfile = Unknown);
    procedure WriteStartDocument;
    procedure WriteStartDocument(standalone: Boolean);
    procedure WriteEndDocument;
    procedure WriteAttributeString(const prefix, localName, ns, value: string; profile: TProfile = Unknown);
    procedure WriteValue(const value: string; profile: TProfile = Unknown);

    procedure WriteElementString(const localName, ns, value: string; profile: TProfile = Unknown);
    procedure WriteElementString(const localName, value: string; profile: TProfile = Unknown);
    procedure WriteAttributeString(const localName, value: string; profile: TProfile = Unknown);
    procedure WriteAttributeString(const localName, ns, value: string; profile: TProfile = Unknown);
    procedure WriteStartElement(const localName: string; profile: TProfile = Unknown);
    procedure WriteStartElement(const localName, ns: string; profile: TProfile = Unknown);
  end;

implementation

constructor TProfileAwareXmlTextWriter.Create(const filename: string; encoding: TEncoding; profile: TProfile);
begin
  TextWriter := TXmlTextWriter.Create(filename, encoding);
  XmlStack := TStack<TStackInfo>.Create;
  CurrentProfile := profile;
end;

constructor TProfileAwareXmlTextWriter.Create(w: TStream; encoding: TEncoding; profile: TProfile);
begin
  TextWriter := TXmlTextWriter.Create(w, encoding);
  XmlStack := TStack<TStackInfo>.Create;
  CurrentProfile := profile;
end;

procedure TProfileAwareXmlTextWriter.Close;
begin
  TextWriter.Close;
end;

procedure TProfileAwareXmlTextWriter.Flush;
begin
  TextWriter.Flush;
end;

function TProfileAwareXmlTextWriter.GetFormatting: TXmlFormatting;
begin
  Result := TextWriter.Formatting;
end;

procedure TProfileAwareXmlTextWriter.SetFormatting(value: TXmlFormatting);
begin
  TextWriter.Formatting := value;
end;

procedure TProfileAwareXmlTextWriter.WriteStartElement(const prefix, localName, ns: string; profile: TProfile = Unknown);
var
  _profile: TProfile;
  info: TStackInfo;
begin
  _profile := profile;
  if profile = Unknown then
    _profile := CurrentProfile;

  if not _IsNodeVisible or not _DoesProfileFitToCurrentProfile(_profile) then
  begin
    info.Profile := _profile;
    info.IsVisible := False;
    XmlStack.Push(info);
    Exit;
  end;

  info.Profile := _profile;
  info.IsVisible := True;
  XmlStack.Push(info);

  if not string.IsNullOrEmpty(prefix) then
    TextWriter.WriteStartElement(prefix, localName, ns)
  else if not string.IsNullOrEmpty(ns) then
    TextWriter.WriteStartElement(localName, ns)
  else
    TextWriter.WriteStartElement(localName);
end;

procedure TProfileAwareXmlTextWriter.WriteEndElement;
var
  infoForCurrentXmlLevel: TStackInfo;
begin
  infoForCurrentXmlLevel := XmlStack.Pop;
  if _DoesProfileFitToCurrentProfile(infoForCurrentXmlLevel.Profile) and _IsNodeVisible then
    TextWriter.WriteEndElement;
end;

procedure TProfileAwareXmlTextWriter.WriteElementString(const prefix, localName, ns, value: string; profile: TProfile = Unknown);
var
  _profile: TProfile;
begin
  _profile := profile;
  if profile = Unknown then
    _profile := CurrentProfile;

  if not _IsNodeVisible or not _DoesProfileFitToCurrentProfile(_profile) then
    Exit;

  if not string.IsNullOrEmpty(prefix) then
    TextWriter.WriteElementString(prefix, localName, ns, value)
  else if not string.IsNullOrEmpty(ns) then
    TextWriter.WriteElementString(localName, ns, value)
  else
    TextWriter.WriteElementString(localName, value);
end;

procedure TProfileAwareXmlTextWriter.WriteStartDocument;
begin
  TextWriter.WriteStartDocument;
end;

procedure TProfileAwareXmlTextWriter.WriteStartDocument(standalone: Boolean);
begin
  TextWriter.WriteStartDocument(standalone);
end;

procedure TProfileAwareXmlTextWriter.WriteEndDocument;
begin
  TextWriter.WriteEndDocument;
end;

procedure TProfileAwareXmlTextWriter.WriteAttributeString(const prefix, localName, ns, value: string; profile: TProfile = Unknown);
var
  infoForCurrentNode: TStackInfo;
begin
  infoForCurrentNode := XmlStack.First;
  if not infoForCurrentNode.IsVisible then
    Exit;

  if profile <> Unknown then
  begin
    if (profile and CurrentProfile) <> CurrentProfile then
      Exit;
  end;

  if not string.IsNullOrEmpty(prefix) then
    TextWriter.WriteAttributeString(prefix, localName, ns, value)
  else if not string.IsNullOrEmpty(ns) then
    TextWriter.WriteAttributeString(localName, ns, value)
  else
    TextWriter.WriteAttributeString(localName, value);
end;

procedure TProfileAwareXmlTextWriter.WriteValue(const value: string; profile: TProfile = Unknown);
var
  infoForCurrentNode: TStackInfo;
begin
  infoForCurrentNode := XmlStack.First;
  if not infoForCurrentNode.IsVisible then
    Exit;

  TextWriter.WriteValue(value);
end;

function TProfileAwareXmlTextWriter._DoesProfileFitToCurrentProfile(profile: TProfile): Boolean;
var
  maskedProfile: TProfile;
begin
  if profile <> Unknown then
  begin
    maskedProfile := profile and CurrentProfile;
    if maskedProfile <> CurrentProfile then
      Exit(False);
  end;

  Result := True;
end;

function TProfileAwareXmlTextWriter._IsNodeVisible: Boolean;
var
  stackInfo: TStackInfo;
begin
  for stackInfo in XmlStack do
  begin
    if not stackInfo.IsVisible then
      Exit(False);
  end;

  Result := True;
end;

procedure TProfileAwareXmlTextWriter.WriteElementString(const localName, ns, value: string; profile: TProfile = Unknown);
begin
  WriteElementString(nil, localName, ns, value, profile);
end;

procedure TProfileAwareXmlTextWriter.WriteElementString(const localName, value: string; profile: TProfile = Unknown);
begin
  WriteElementString(nil, localName, nil, value, profile);
end;

procedure TProfileAwareXmlTextWriter.WriteAttributeString(const localName, value: string; profile: TProfile = Unknown);
begin
  WriteAttributeString(nil, localName, nil, value, profile);
end;

procedure TProfileAwareXmlTextWriter.WriteAttributeString(const localName, ns, value: string; profile: TProfile = Unknown);
begin
  WriteAttributeString(nil, localName, ns, value, profile);
end;

procedure TProfileAwareXmlTextWriter.WriteStartElement(const localName: string; profile: TProfile = Unknown);
begin
  WriteStartElement(nil, localName, nil, profile);
end;

procedure TProfileAwareXmlTextWriter.WriteStartElement(const localName, ns: string; profile: TProfile = Unknown);
begin
  WriteStartElement(nil, localName, ns, profile);
end;

//    internal class StackInfo
//    {
//        public Profile Profile;
//        public bool IsVisible;
//    }
//
//
//    internal class ProfileAwareXmlTextWriter
//    {
//        private XmlTextWriter TextWriter;
//        private Stack<StackInfo> XmlStack = new Stack<StackInfo>();
//        private Profile CurrentProfile = Profile.Unknown;
//
//
//        public System.Xml.Formatting Formatting
//        {
//            get
//            {
//                return this.TextWriter.Formatting;
//            }
//            set
//            {
//                this.TextWriter.Formatting = value;
//            }
//        }
//
//
//        public ProfileAwareXmlTextWriter(string filename, System.Text.Encoding encoding, Profile profile)
//        {
//            this.TextWriter = new XmlTextWriter(filename, encoding);
//            this.CurrentProfile = profile;
//        }
//
//
//        public ProfileAwareXmlTextWriter(System.IO.Stream w, System.Text.Encoding encoding, Profile profile)
//        {
//            this.TextWriter = new XmlTextWriter(w, encoding);
//            this.CurrentProfile = profile;
//        }
//
//
//        public void Close()
//        {
//            this.TextWriter?.Close();
//        }
//
//
//        public void Flush()
//        {
//            this.TextWriter?.Flush();
//        }
//
//
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
//
//
//        public void WriteEndElement()
//        {
//            StackInfo infoForCurrentXmlLevel = this.XmlStack.Pop();
//            if (_DoesProfileFitToCurrentProfile(infoForCurrentXmlLevel.Profile) && _IsNodeVisible())
//            {
//                this.TextWriter?.WriteEndElement();
//            }
//        }
//
//
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
//
//
//        public void WriteStartDocument()
//        {
//            this.TextWriter?.WriteStartDocument();
//        }
//
//
//        public void WriteStartDocument(bool standalone)
//        {
//            this.TextWriter?.WriteStartDocument(standalone);
//        }
//
//        public void WriteEndDocument()
//        {
//            this.TextWriter?.WriteEndDocument();
//        }
//
//
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
//
//
//        public void WriteValue(string value, Profile profile = Profile.Unknown)
//        {
//            StackInfo infoForCurrentNode = this.XmlStack.First();
//            if (!infoForCurrentNode.IsVisible)
//            {
//                return;
//            }
//
//            // write value
//            this.TextWriter?.WriteValue(value);
//        } // !WriteAttributeString()
//
//
//        #region Stack Management
//        private bool _DoesProfileFitToCurrentProfile(Profile profile)
//        {
//            if (profile != Profile.Unknown)
//            {
//                Profile maskedProfile = (profile & this.CurrentProfile);
//                if (maskedProfile != this.CurrentProfile)
//                {
//                    return false;
//                }
//            }
//
//            return true;
//        } // !_DoesProfileFitToCurrentProfile()
//
//
//        private bool _IsNodeVisible()
//        {
//            foreach (StackInfo stackInfo in this.XmlStack)
//            {
//                if (!stackInfo.IsVisible)
//                {
//                    return false;
//                }
//            }
//
//            return true;
//        } // !_IsNodeVisible()
//        #endregion // !Stack Management
//
//
//        #region Convience functions
//        public void WriteElementString(string localName, string ns, string value, Profile profile = Profile.Unknown)
//        {
//            this.WriteElementString(null, localName, ns, value, profile);
//        } // !WriteElementString()
//
//
//        public void WriteElementString(string localName, string value, Profile profile = Profile.Unknown)
//        {
//            this.WriteElementString(null, localName, null, value, profile);
//        } // !WriteElementString()
//
//
//        public void WriteAttributeString(string localName, string value, Profile profile = Profile.Unknown)
//        {
//            this.WriteAttributeString(null, localName, null, value, profile);
//        } // !WriteAttributeString()
//
//
//        public void WriteAttributeString(string localName, string ns, string value, Profile profile = Profile.Unknown)
//        {
//            this.WriteAttributeString(null, localName, ns, value, profile);
//        } // !WriteAttributeString()
//
//
//        public void WriteStartElement(string localName, Profile profile = Profile.Unknown)
//        {
//            this.WriteStartElement(null, localName, null, profile);
//        } // !WriteStartElement()
//
//
//        public void WriteStartElement(string localName, string ns, Profile profile = Profile.Unknown)
//        {
//            this.WriteStartElement(null, localName, ns, profile);
//        } // !WriteStartElement()
//        #endregion // !Convenience functions
//    }

end.
