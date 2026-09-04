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
  ,System.Math, System.Generics.Collections
  ,intf.ZUGFeRDInvoiceDescriptor
  ,intf.ZUGFeRDProfileAwareXmlTextWriter
  ,intf.ZUGFeRDProfile
  ,intf.ZUGFeRDFormats
  ,intf.ZUGFeRDInvoiceFormatOptions
  ,intf.ZUGFeRDHelper
  ,intf.ZUGFeRDExceptions
  ,intf.ZUGFeRDInvoiceTypes
  ,intf.ZUGFeRDTaxTypes
  ,intf.ZUGFeRDTradeLineItem
  ;

type
  TZUGFeRDIInvoiceDescriptorWriter = class abstract
  protected
    /// <summary>
    /// Gemeinsame Vorprüfung für ZUGFeRD 2.x / Factur-X / XRechnung (Version23).
    /// Liegt hier, damit der Dispatcher (23Writer) und die direkt nutzbaren Writer
    /// (23CIIWriter, 22UBLWriter) dieselben Regeln anwenden, ohne sich gegenseitig
    /// im Interface-Teil zu referenzieren.
    /// </summary>
    class function ValidateVersion23(descriptor: TZUGFeRDInvoiceDescriptor; throwExceptions: Boolean): Boolean; static;
  public
    procedure Save(descriptor: TZUGFeRDInvoiceDescriptor; stream: TStream; format: TZUGFeRDFormats = TZUGFeRDFormats.CII; options: TZUGFeRDInvoiceFormatOptions = Nil); overload; virtual; abstract;
    procedure Save(descriptor: TZUGFeRDInvoiceDescriptor; const filename: string; format: TZUGFeRDFormats = TZUGFeRDFormats.CII; options: TZUGFeRDInvoiceFormatOptions = Nil); overload;
    function Validate(descriptor: TZUGFeRDInvoiceDescriptor; throwExceptions: Boolean = True): Boolean; virtual; abstract;
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
  ms: TMemoryStream;
  fs: TFileStream;
begin
  if not Validate(descriptor, True) then
    // Validate darf hier nur mit Exception scheitern; liefert eine Ableitung trotzdem
    // false, wird das nicht stillschweigend geschluckt (sonst entstünde keine Datei).
    raise TZUGFeRDUnsupportedException.Create(
      'Invoice descriptor failed validation, file "' + filename + '" was not written.');

  // Erst vollständig in den Speicher schreiben, dann die Zieldatei anlegen. Validate kennt
  // das Zielformat nicht; unzulässige Kombinationen aus Profil und Format (z. B. UBL mit
  // einem anderen Profil als XRechnung) fallen erst beim Schreiben auf. Ohne diesen Umweg
  // haette fmCreate eine vorhandene Datei bereits geleert, bevor die Exception fliegt.
  ms := TMemoryStream.Create;
  try
    Save(descriptor, ms, format, options);
    ms.Position := 0;
    fs := TFileStream.Create(filename, fmCreate or fmOpenWrite);
    try
      fs.CopyFrom(ms, ms.Size);
    finally
      fs.Free;
    end;
  finally
    ms.Free;
  end;
end;

class function TZUGFeRDIInvoiceDescriptorWriter.ValidateVersion23(descriptor: TZUGFeRDInvoiceDescriptor; throwExceptions: Boolean): Boolean;
begin
  Result := false;

  //TODO in C# enthalten, aber eigentlich falsch, deswegen auskommentiert
  //if (descriptor.TZUGFeRDProfile = TZUGFeRDProfile.BasicWL) then
  //if (throwExceptions) then
  //  raise TZUGFeRDUnsupportedException.Create('Invalid TZUGFeRDProfile used for ZUGFeRD 2.0 invoice.')
  //else
  //  exit;

  if (descriptor.Profile <> TZUGFeRDProfile.Extended) then // check tax types, only extended TZUGFeRDProfile allows tax types other than vat
  begin
    for var l : TZUGFeRDTradeLineItem in descriptor.TradeLineItems do
    if l.TaxType.HasValue and (l.TaxType <> TZUGFeRDTaxTypes.VAT) then
    begin
      if (throwExceptions) then
        raise TZUGFeRDUnsupportedException.Create('Tax types other than VAT only possible with extended TZUGFeRDProfile.')
      else
        exit;
    end;
  end;

  if (descriptor.Profile in [TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]) then
  begin
    if (descriptor.Seller <> nil) then
    begin
      if (descriptor.SellerContact = nil) then
      begin
          if (throwExceptions) then
            raise TZUGFeRDMissingDataException.Create('Seller contact (BG-6) required when seller is set (BR-DE-2).')
          else
            exit;
      end
      else
      begin
          if (descriptor.SellerContact.EmailAddress = '') then
          begin
            if (throwExceptions) then
              raise TZUGFeRDMissingDataException.Create('Seller contact email address (BT-43) is required (BR-DE-7).')
            else
              exit;
          end;
          if (descriptor.SellerContact.PhoneNo = '') then
          begin
            if (throwExceptions) then
                raise TZUGFeRDMissingDataException.Create('Seller contact phone no (BT-42) is required (BR-DE-6).')
            else
              exit;
          end;
          if (descriptor.SellerContact.Name = '') and
             (descriptor.SellerContact.OrgUnit = '') then
          begin
            if (throwExceptions) then
              raise TZUGFeRDMissingDataException.Create('Seller contact point (name or org unit) no (BT-41) is required (BR-DE-5).')
            else
              exit;
          end;
      end;
    end;
  end;

  // BR-DE-17 ist eine XRechnung-Regel; Factur-X/ZUGFeRD erlauben z. B. die
  // Vorauszahlungsrechnung (386), deshalb nur für die XRechnung-Profile prüfen.
  if (descriptor.Profile in [TZUGFeRDProfile.XRechnung, TZUGFeRDProfile.XRechnung1]) and
     not ((descriptor.Type_ = TZUGFeRDInvoiceType.PartialInvoice) or
          (descriptor.Type_ = TZUGFeRDInvoiceType.Invoice) or
          (descriptor.Type_ = TZUGFeRDInvoiceType.Correction) or
          (descriptor.Type_ = TZUGFeRDInvoiceType.SelfBilledInvoice) or
          (descriptor.Type_ = TZUGFeRDInvoiceType.CreditNote) or
          (descriptor.Type_ = TZUGFeRDInvoiceType.PartialConstructionInvoice) or
          (descriptor.Type_ = TZUGFeRDInvoiceType.PartialFinalConstructionInvoice) or
          (descriptor.Type_ = TZUGFeRDInvoiceType.FinalConstructionInvoice)) then
  begin
    if (throwExceptions) then
      raise TZUGFeRDUnsupportedException.Create('Invoice type (BT-3) does not match requirements of BR-DE-17')
    else
      exit;
  end;

  Result := true;
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
