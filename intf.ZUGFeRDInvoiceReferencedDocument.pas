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

unit intf.ZUGFeRDInvoiceReferencedDocument;

interface

uses
  System.Types, System.Classes, System.Contnrs,
  intf.ZUGFeRDBaseReferencedDocument,
  intf.ZUGFeRDInvoiceTypes,
  intf.ZUGFeRDHelper;

type
  {$IF CompilerVersion >= 36.0}
  TInvoiceListItemType = NativeInt;
  {$ELSE}
  TInvoiceListItemType = Integer;
  {$IFEND}
  /// <summary>
  /// Detailed Information about Preceding Invoice
  /// </summary>
  TZUGFeRDInvoiceReferencedDocument = class(TZUGFeRDBaseReferencedDocument)
  private
    FTypeCode: ZUGFeRDNullable<TZUGFeRDInvoiceType>;
  public
    /// <summary>
    /// BT-X-332 - Can be used in the case of a final invoice following a prepaid invoice to refer to the previous prepaid invoices.
    /// Code list UNCL 1001 restricted as BT-3.
    /// </summary>
    property TypeCode: ZUGFeRDNullable<TZUGFeRDInvoiceType> read FTypeCode write FTypeCode;
 end;


implementation

{ TZUGFeRDInvoiceReferencedDocumentObjectList }

end.
