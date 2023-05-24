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

unit intf.ZUGFeRDAdditionalReferencedDocumentTypeCodes;

interface

uses
  System.SysUtils,System.TypInfo
  ;

type
  /// <summary>
  /// http://www.unece.org/trade/untdid/d00a/tred/tred5153.htm
  /// </summary>
  TZUGFeRDAdditionalReferencedDocumentTypeCode = (
        /// <summary>
        /// simple reference document
        /// </summary>
    ReferenceDocument = 916,
        /// <summary>
        /// an invoice which could contain items
        /// </summary>
    InvoiceDataSheet = 130,
        /// <summary>
        /// price and sales catalog
        /// </summary>
    PriceSalesCatalogueResponse = 50,
        /// <summary>
        /// Unknown reference document type
        /// </summary>
    Unknown = 65536
  );

  TZUGFeRDAdditionalReferencedDocumentTypeCodeExtensions = class
  public
    class function FromString(const s: string): TZUGFeRDAdditionalReferencedDocumentTypeCode;
    class function EnumValueToString(t: TZUGFeRDAdditionalReferencedDocumentTypeCode): string;
    class function EnumToString(t: TZUGFeRDAdditionalReferencedDocumentTypeCode): string;
  end;

implementation

class function TZUGFeRDAdditionalReferencedDocumentTypeCodeExtensions.FromString(const s: string): TZUGFeRDAdditionalReferencedDocumentTypeCode;
begin
  if SameText(s,'130') then
    Result := TZUGFeRDAdditionalReferencedDocumentTypeCode.InvoiceDataSheet
  else
  if SameText(s,'50') then
    Result := TZUGFeRDAdditionalReferencedDocumentTypeCode.PriceSalesCatalogueResponse
  else
  if SameText(s,'65536') then
    Result := TZUGFeRDAdditionalReferencedDocumentTypeCode.Unknown
  else
    Result := TZUGFeRDAdditionalReferencedDocumentTypeCode.ReferenceDocument
end;

class function TZUGFeRDAdditionalReferencedDocumentTypeCodeExtensions.EnumValueToString(t: TZUGFeRDAdditionalReferencedDocumentTypeCode): string;
begin
  Result := IntToStr(Integer(t));
end;

class function TZUGFeRDAdditionalReferencedDocumentTypeCodeExtensions.EnumToString(t: TZUGFeRDAdditionalReferencedDocumentTypeCode): string;
begin
  case t of
    TZUGFeRDAdditionalReferencedDocumentTypeCode.ReferenceDocument: Result := '916';
    TZUGFeRDAdditionalReferencedDocumentTypeCode.InvoiceDataSheet: Result := '130';
    TZUGFeRDAdditionalReferencedDocumentTypeCode.PriceSalesCatalogueResponse: Result := '50';
    else Result := '65536';
  end;
end;

end.
