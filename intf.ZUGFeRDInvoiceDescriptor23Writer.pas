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

unit intf.ZUGFeRDInvoiceDescriptor23Writer;

interface

uses
  System.SysUtils,System.Classes,System.StrUtils,System.Generics.Collections
  ,intf.ZUGFeRDInvoiceDescriptor
  ,intf.ZUGFeRDInvoiceTypes
  ,intf.ZUGFeRDProfileAwareXmlTextWriter
  ,intf.ZUGFeRDIInvoiceDescriptorwriter
  ,intf.ZUGFeRDProfile
  ,intf.ZUGFeRDExceptions
  ,intf.ZUGFeRDHelper
  ,intf.ZUGFeRDCurrencyCodes
  ,intf.ZUGFeRDVersion
  ,intf.ZUGFeRDNote
  ,intf.ZUGFeRDContentCodes
  ,intf.ZUGFeRDSubjectCodes
  ,intf.ZUGFeRDContact
  ,intf.ZUGFeRDParty
  ,intf.ZUGFeRDTaxRegistration
  ,intf.ZUGFeRDGlobalIDSchemeIdentifiers
  ,intf.ZUGFeRDCountryCodes
  ,intf.ZUGFeRDTaxRegistrationSchemeID
  ,intf.ZUGFeRDTax
  ,intf.ZUGFeRDTaxTypes
  ,intf.ZUGFeRDTaxCategoryCodes
  ,intf.ZUGFeRDTradeLineItem
  ,intf.ZUGFeRDAdditionalReferencedDocument
  ,intf.ZUGFeRDAdditionalReferencedDocumentTypeCodes
  ,intf.ZUGFeRDReferenceTypeCodes
  ,intf.ZUGFeRDPaymentMeansTypeCodes
  ,intf.ZUGFeRDBankAccount
  ,intf.ZUGFeRDTradeAllowanceCharge
  ,intf.ZUGFeRDPaymentTerms
  ,intf.ZUGFeRDServiceCharge
  ,intf.ZUGFeRDQuantityCodes
  ,intf.ZUGFeRDLegalOrganization
  ,intf.ZUGFeRDPartyTypes
  ,intf.ZUGFeRDElectronicAddress
  ,intf.ZUGFeRDElectronicAddressSchemeIdentifiers
  ,intf.ZUGFeRDTaxExemptionReasonCodes
  ,intf.ZUGFeRDApplicableProductCharacteristic
  ,intf.ZUGFeRDReceivableSpecifiedTradeAccountingAccount
  ,intf.ZUGFeRDAccountingAccountTypeCodes
  ,intf.ZUGFeRDMimeTypeMapper
  ,intf.ZUGFeRDFormats
  ,intf.ZUGFeRDDesignatedProductClassification
  ,intf.ZUGFeRDDesignatedProductClassificationClassCodes
  ,intf.ZUGFeRDInvoiceFormatOptions;

type
  TZUGFeRDInvoiceDescriptor23Writer = class(TZUGFeRDIInvoiceDescriptorWriter)
  public
    /// <summary>
    /// This function is implemented in class InvoiceDescriptor22Writer.
    /// </summary>
    function Validate(_descriptor: TZUGFeRDInvoiceDescriptor; _throwExceptions: Boolean = True): Boolean; override;
    /// <summary>
    /// Saves the given invoice to the given stream.
    /// Make sure that the stream is open and writeable. Otherwise, an IllegalStreamException will be thron.
    /// </summary>
    /// <param name="descriptor">The invoice object that should be saved</param>
    /// <param name="stream">The target stream for saving the invoice</param>
    /// <param name="format">Format of the target file</param>
    procedure Save (_descriptor: TZUGFeRDInvoiceDescriptor; _stream: TStream; _format : TZUGFeRDFormats = TZUGFeRDFormats.CII; options: TZUGFeRDInvoiceFormatOptions = Nil); override;
  end;

implementation

uses
  intf.ZUGFeRDInvoiceDescriptor23CIIWriter,
  intf.ZUGFeRDInvoiceDescriptor22UBLWriter
  ;

{ TZUGFeRDInvoiceDescriptor23Writer }

procedure TZUGFeRDInvoiceDescriptor23Writer.Save(
  _descriptor: TZUGFeRDInvoiceDescriptor; _stream: TStream;
  _format : TZUGFeRDFormats = TZUGFeRDFormats.CII;
   options: TZUGFeRDInvoiceFormatOptions = Nil);
var
  _writer : TZUGFeRDIInvoiceDescriptorWriter;
begin
  if (_stream = nil) then
    raise TZUGFeRDIllegalStreamException.Create('Cannot write to stream');

  if _format = UBL then
  begin
    if _descriptor.Profile = TZUGFeRDProfile.XRechnung then
      _writer := TZUGFeRDInvoiceDescriptor22UBLWriter.Create
    else
      // Meldung nennt die gueltige Kombination, sonst raet der Aufrufer
      raise TZUGFeRDUnsupportedException.Create(
        'Profile ' + TEnumExtensions<TZUGFeRDProfile>.EnumToString(_descriptor.Profile) +
        ' and format UBL is not supported for ZUGFeRD version 2.3. ' +
        'Format UBL is only allowed with profile XRechnung.');
  end
  else
    _writer := TZUGFeRDInvoiceDescriptor23CIIWriter.Create;
  try
    _writer.Save(_descriptor, _stream, _format, options);
  finally
    _writer.Free;
  end;
end;

function TZUGFeRDInvoiceDescriptor23Writer.Validate(
  _descriptor: TZUGFeRDInvoiceDescriptor; _throwExceptions: Boolean): Boolean;
begin
  // Die eigentlichen Regeln liegen in der Basisklasse, damit 23CIIWriter und
  // 22UBLWriter bei direkter Nutzung dieselbe Prüfung durchlaufen.
  Result := ValidateVersion23(_descriptor, _throwExceptions);
end;

end.
