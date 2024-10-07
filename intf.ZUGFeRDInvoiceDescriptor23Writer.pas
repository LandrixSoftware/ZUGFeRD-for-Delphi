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
  System.SysUtils,System.Classes,System.StrUtils,Generics.Collections
  ,intf.ZUGFeRDInvoiceDescriptor
  ,intf.ZUGFeRDInvoiceTypes
  ,intf.ZUGFeRDProfileAwareXmlTextWriter
  ,intf.ZUGFeRDInvoiceDescriptorwriter
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
  ,intf.ZUGFeRDSpecialServiceDescriptionCodes
  ,intf.ZUGFeRDAllowanceOrChargeIdentificationCodes
  ,intf.ZUGFeRDFormats
  ,intf.ZUGFeRDDesignatedProductClassification
  ,intf.ZUGFeRDDesignatedProductClassificationClassCodes
  ;

type
  TZUGFeRDInvoiceDescriptor23Writer = class(TZUGFeRDInvoiceDescriptorWriter)
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
    procedure Save(_descriptor: TZUGFeRDInvoiceDescriptor; _stream: TStream; _format : TZUGFeRDFormats = TZUGFeRDFormats.CII); override;
  end;

implementation

uses
  intf.ZUGFeRDInvoiceDescriptor23CIIWriter,
  intf.ZUGFeRDInvoiceDescriptor22UBLWriter
  ;

{ TZUGFeRDInvoiceDescriptor23Writer }

procedure TZUGFeRDInvoiceDescriptor23Writer.Save(
  _descriptor: TZUGFeRDInvoiceDescriptor; _stream: TStream;
  _format : TZUGFeRDFormats = TZUGFeRDFormats.CII);
var
  _writer : TZUGFeRDInvoiceDescriptorWriter;
begin
  if (_stream = nil) then
    raise TZUGFeRDIllegalStreamException.Create('Cannot write to stream');

  if _format = UBL then
    _writer := TZUGFeRDInvoiceDescriptor22UBLWriter.Create
  else
    _writer := TZUGFeRDInvoiceDescriptor23CIIWriter.Create;
  try
    _writer.Save(_descriptor, _stream, _format);
  finally
    _writer.Free;
  end;
end;

function TZUGFeRDInvoiceDescriptor23Writer.Validate(
  _descriptor: TZUGFeRDInvoiceDescriptor; _throwExceptions: Boolean): Boolean;
begin
  Result := false;

  //TODO in C# enthalten, aber eigentlich falsch, deswegen auskommentiert
  //if (descriptor.TZUGFeRDProfile = TZUGFeRDProfile.BasicWL) then
  //if (throwExceptions) then
  //  raise TZUGFeRDUnsupportedException.Create('Invalid TZUGFeRDProfile used for ZUGFeRD 2.0 invoice.')
  //else
  //  exit;

  if (_descriptor.Profile <> TZUGFeRDProfile.Extended) then // check tax types, only extended TZUGFeRDProfile allows tax types other than vat
  begin
    for var l : TZUGFeRDTradeLineItem in _descriptor.TradeLineItems do
    if not ((l.TaxType = TZUGFeRDTaxTypes.Unknown) or
      (l.TaxType = TZUGFeRDTaxTypes.VAT)) then
    begin
      if (_throwExceptions) then
        raise TZUGFeRDUnsupportedException.Create('Tax types other than VAT only possible with extended TZUGFeRDProfile.')
      else
        exit;
    end;
  end;

  if (_descriptor.Profile in [TZUGFeRDProfile.XRechnung,TZUGFeRDProfile.XRechnung1]) then
  begin
    if (_descriptor.Seller <> nil) then
    begin
      if (_descriptor.SellerContact = nil) then
      begin
          if (_throwExceptions) then
            raise TZUGFeRDMissingDataException.Create('Seller contact (BG-6) required when seller is set (BR-DE-2).')
          else
            exit;
      end
      else
      begin
          if (_descriptor.SellerContact.EmailAddress = '') then
          begin
            if (_throwExceptions) then
              raise TZUGFeRDMissingDataException.Create('Seller contact email address (BT-43) is required (BR-DE-7).')
            else
              exit;
          end;
          if (_descriptor.SellerContact.PhoneNo = '') then
          begin
            if (_throwExceptions) then
                raise TZUGFeRDMissingDataException.Create('Seller contact phone no (BT-42) is required (BR-DE-6).')
            else
              exit;
          end;
          if (_descriptor.SellerContact.Name = '') and
             (_descriptor.SellerContact.OrgUnit = '') then
          begin
            if (_throwExceptions) then
              raise TZUGFeRDMissingDataException.Create('Seller contact point (name or org unit) no (BT-41) is required (BR-DE-5).')
            else
              exit;
          end;
      end;
    end;
  end;

  // BR-DE-17
  if not ((_descriptor.Type_ = TZUGFeRDInvoiceType.PartialInvoice) or
          (_descriptor.Type_ = TZUGFeRDInvoiceType.Invoice) or
          (_descriptor.Type_ = TZUGFeRDInvoiceType.Correction) or
          (_descriptor.Type_ = TZUGFeRDInvoiceType.SelfBilledInvoice) or
          (_descriptor.Type_ = TZUGFeRDInvoiceType.CreditNote) or
          (_descriptor.Type_ = TZUGFeRDInvoiceType.PartialConstructionInvoice) or
          (_descriptor.Type_ = TZUGFeRDInvoiceType.PartialFinalConstructionInvoice) or
          (_descriptor.Type_ = TZUGFeRDInvoiceType.FinalConstructionInvoice)) then
  begin
    if (_throwExceptions) then
      raise TZUGFeRDUnsupportedException.Create('Invoice type (BT-3) does not match requirements of BR-DE-17')
    else
      exit;
  end;

  Result := true;
end;

end.
