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
  System.SysUtils, System.TypInfo,
  intf.ZUGFeRDHelper
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
    PriceSalesCatalogueResponse = 50
  );

  TZUGFeRDAdditionalReferencedDocumentTypeCodeExtensions = class
  public
    class function FromString(const s: string): ZUGFeRDNullable<TZUGFeRDAdditionalReferencedDocumentTypeCode>;
    class function EnumToString(t: ZUGFeRDNullable<TZUGFeRDAdditionalReferencedDocumentTypeCode>): string;
  end;

implementation

class function TZUGFeRDAdditionalReferencedDocumentTypeCodeExtensions.FromString(const s: string): ZUGFeRDNullable<TZUGFeRDAdditionalReferencedDocumentTypeCode>;
begin
  if Trim(s)='' then
    Exit(Nil);

  if SameText(s,'130') then
    Result := TZUGFeRDAdditionalReferencedDocumentTypeCode.InvoiceDataSheet
  else
  if SameText(s,'50') then
    Result := TZUGFeRDAdditionalReferencedDocumentTypeCode.PriceSalesCatalogueResponse
  else
    Result := TZUGFeRDAdditionalReferencedDocumentTypeCode.ReferenceDocument
end;

class function TZUGFeRDAdditionalReferencedDocumentTypeCodeExtensions.EnumToString(t: ZUGFeRDNullable<TZUGFeRDAdditionalReferencedDocumentTypeCode>): string;
begin
  if t.HasValue then
    Result := IntToStr(Integer(t.Value))
  else
    Result:= ''
end;

end.
