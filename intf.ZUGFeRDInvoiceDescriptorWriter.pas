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

unit intf.ZUGFeRDInvoiceDescriptorWriter;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils
  ,intf.ZUGFeRDInvoiceDescriptor,intf.ZUGFeRDProfileAwareXmlTextWriter
  ,intf.ZUGFeRDProfile;

type
  TZUGFeRDInvoiceDescriptorWriter = class abstract
  public
    procedure Save(descriptor: TZUGFeRDInvoiceDescriptor; stream: TStream); overload; virtual; abstract;

    procedure Save(descriptor: TZUGFeRDInvoiceDescriptor; const filename: string); overload;
    function Validate(descriptor: TZUGFeRDInvoiceDescriptor; throwExceptions: Boolean = True): Boolean; virtual; abstract;

  protected
    procedure WriteOptionalElementString(writer: TZUGFeRDProfileAwareXmlTextWriter; const tagName, value: string; profile: TZUGFeRDProfile = Unknown);
    function FormatDecimal(value: Currency; numDecimals: Integer = 2): string;
    function FormatDate(date: TDateTime; formatAs102: Boolean = True): string;
  end;

implementation

procedure TZUGFeRDInvoiceDescriptorWriter.Save(descriptor: TZUGFeRDInvoiceDescriptor; const filename: string);
var
  fs: TFileStream;
begin
  if Validate(descriptor, True) then
  begin
    fs := TFileStream.Create(filename, fmCreate or fmOpenWrite);
    try
      Save(descriptor, fs);
      //fs.Flush;
    finally
      fs.Free;
    end;
  end;
end;

procedure TZUGFeRDInvoiceDescriptorWriter.WriteOptionalElementString(writer: TZUGFeRDProfileAwareXmlTextWriter;
  const tagName, value: string; profile: TZUGFeRDProfile);
begin
  if not value.IsEmpty then
    writer.WriteElementString(tagName, value, profile);
end;

function TZUGFeRDInvoiceDescriptorWriter.FormatDecimal(value: Currency; numDecimals: Integer): string;
var
  formatString: string;
  i: Integer;
begin
  formatString := '0.';
  for i := 0 to numDecimals - 1 do
    formatString := formatString + '0';

  Result := FormatFloat(formatString, value);
end;

function TZUGFeRDInvoiceDescriptorWriter.FormatDate(date: TDateTime; formatAs102: Boolean): string;
begin
  if formatAs102 then
    Result := FormatDateTime('yyyymmdd', date)
  else
    Result := FormatDateTime('yyyy-mm-dd"THH:mm:ss', date);
end;

 //   internal abstract class IInvoiceDescriptorWriter
//    {
//        public abstract void Save(InvoiceDescriptor descriptor, Stream stream);
//
//
//        public void Save(InvoiceDescriptor descriptor, string filename)
//        {
//            if (Validate(descriptor, true))
//            {
//                FileStream fs = new FileStream(filename, FileMode.Create, FileAccess.Write);
//                Save(descriptor, fs);
//                fs.Flush();
//                fs.Close();
//            }
//        } // !Save()
//
//
//        internal abstract bool Validate(InvoiceDescriptor descriptor, bool throwExceptions = true);
//
//
//        internal void _writeOptionalElementString(ProfileAwareXmlTextWriter writer, string tagName, string value, Profile profile = Profile.Unknown)
//        {
//            if (!String.IsNullOrEmpty(value))
//            {
//                writer.WriteElementString(tagName, value, profile);
//            }
//        } // !_writeOptionalElementString()
//
//
//        protected string _formatDecimal(decimal value, int numDecimals = 2)
//        {
//            string formatString = "0.";
//            for (int i = 0; i < numDecimals; i++)
//            {
//                formatString += "0";
//            }
//
//            return value.ToString(formatString).Replace(",", ".");
//        } // !_formatDecimal()
//
//
//        protected string _formatDate(DateTime date, bool formatAs102 = true)
//        {
//            if (formatAs102)
//            {
//                return date.ToString("yyyyMMdd");
//            }
//            else
//            {
//                return date.ToString("yyyy-MM-ddTHH:mm:ss");
//            }
//        } // !_formatDate()
//    }
//}

end.
