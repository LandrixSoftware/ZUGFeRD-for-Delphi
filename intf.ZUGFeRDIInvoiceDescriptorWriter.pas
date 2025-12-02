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

unit intf.ZUGFeRDIInvoiceDescriptorWriter;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils, System.StrUtils
  ,System.Math
  ,intf.ZUGFeRDInvoiceDescriptor
  ,intf.ZUGFeRDProfileAwareXmlTextWriter
  ,intf.ZUGFeRDProfile
  ,intf.ZUGFeRDFormats
  ,intf.ZUGFeRDInvoiceFormatOptions
  ,intf.ZUGFeRDHelper
  ;

type
  TZUGFeRDIInvoiceDescriptorWriter = class abstract
  public
    procedure Save(descriptor: TZUGFeRDInvoiceDescriptor; stream: TStream; format: TZUGFeRDFormats = TZUGFeRDFormats.CII; options: TZUGFeRDInvoiceFormatOptions = Nil); overload; virtual; abstract;
    procedure Save(descriptor: TZUGFeRDInvoiceDescriptor; const filename: string; format: TZUGFeRDFormats = TZUGFeRDFormats.CII; options: TZUGFeRDInvoiceFormatOptions = Nil); overload;
    function Validate(descriptor: TZUGFeRDInvoiceDescriptor; throwExceptions: Boolean = True): Boolean; virtual; abstract;
  public
    procedure WriteOptionalElementString(writer: TZUGFeRDProfileAwareXmlTextWriter; const tagName, value: string; profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
    /// <summary>
    /// Write header comments to XML writer
    /// </summary>
    procedure WriteHeaderComments(Writer: TZUGFeRDProfileAwareXmlTextWriter; Options: TZUGFeRDInvoiceFormatOptions);

    /// <summary>
    /// Write single comment to XML writer
    /// </summary>
    procedure WriteComment(Writer: TZUGFeRDProfileAwareXmlTextWriter; Options: TZUGFeRDInvoiceFormatOptions; const Comment: string);

    /// <summary>
    /// Format decimal value with specified number of decimals
    /// </summary>
    function _formatDecimal(const Value: Currency; NumDecimals: Integer = 2): string;

    /// <summary>
    /// Format date value
    /// </summary>
    function _formatDate(const Date: TDateTime; FormatAs102: Boolean = True; ToUBLDate: Boolean = False): string;
  end;

implementation

procedure TZUGFeRDIInvoiceDescriptorWriter.Save(descriptor: TZUGFeRDInvoiceDescriptor; const filename: string; format:TZUGFeRDFormats; options: TZUGFeRDInvoiceFormatOptions);
var
  fs: TFileStream;
begin
  if Validate(descriptor, True) then
  begin
    fs := TFileStream.Create(filename, fmCreate or fmOpenWrite);
    try
      Save(descriptor, fs, format, options);
      //fs.Flush;
      //fs.Close;
    finally
      fs.Free;
    end;
  end;
end;

procedure TZUGFeRDIInvoiceDescriptorWriter.WriteOptionalElementString(
  writer: TZUGFeRDProfileAwareXmlTextWriter;
  const tagName, value: string;
  profile: TZUGFeRDProfiles = TZUGFERDPROFILES_DEFAULT);
begin
  if not value.IsEmpty then
    writer.WriteElementString(tagName, value, profile);
end;

procedure TZUGFeRDIInvoiceDescriptorWriter.WriteHeaderComments(Writer: TZUGFeRDProfileAwareXmlTextWriter;  Options: TZUGFeRDInvoiceFormatOptions);
var
  Comment: string;
begin
  if (Writer = nil) or (Options = nil) then
    Exit;

  if not Options.IncludeXmlComments or (Options.XmlHeaderComments.Count = 0) then
    Exit;

  for Comment in Options.XmlHeaderComments do
    if Comment <> '' then
      Writer.WriteComment(Comment);
end;

procedure TZUGFeRDIInvoiceDescriptorWriter.WriteComment(Writer: TZUGFeRDProfileAwareXmlTextWriter;  Options: TZUGFeRDInvoiceFormatOptions; const Comment: string);
begin
  if (Writer = nil) or (Options = nil) or (Comment = '') then
    Exit;

  if not Options.IncludeXmlComments then
    Exit;

  Writer.WriteComment(Comment);
end;

function TZUGFeRDIInvoiceDescriptorWriter._formatDecimal(const Value: Currency; NumDecimals: Integer): string;
var
  FormatSettings: TFormatSettings;
  RoundedValue: Currency;
  FormatStr: string;
begin
  FormatSettings := TFormatSettings.Invariant;
  RoundedValue := RoundTo(Value, -NumDecimals);
  FormatStr := '%.' + IntToStr(NumDecimals) + 'f';
  Result := Format(FormatStr, [RoundedValue], FormatSettings);
end;

function TZUGFeRDIInvoiceDescriptorWriter._formatDate(const Date: TDateTime; FormatAs102: Boolean; ToUBLDate: Boolean): string;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings := TFormatSettings.Invariant;

  if FormatAs102 then
  begin
    Result := FormatDateTime('yyyymmdd', Date, FormatSettings);
  end
  else
  begin
    if ToUBLDate then
      Result := FormatDateTime('yyyy-mm-dd', Date, FormatSettings)
    else
      Result := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Date, FormatSettings);
  end;
end;

end.
