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
  intf.ZUGFeRDBaseReferencedDocument;

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
  public
  end;

  TZUGFeRDInvoiceReferencedDocumentObjectList = class(TObjectList)
  protected
    function GetItem(Index: TInvoiceListItemType): TZUGFeRDInvoiceReferencedDocument;
    procedure SetItem(Index: TInvoiceListItemType; AItem: TZUGFeRDInvoiceReferencedDocument);
  public
	  function  Extract(Item: TObject): TZUGFeRDInvoiceReferencedDocument;
	  function  First: TZUGFeRDInvoiceReferencedDocument;
	  function  Last: TZUGFeRDInvoiceReferencedDocument;
	  property  Items[Index: TInvoiceListItemType]: TZUGFeRDInvoiceReferencedDocument read GetItem write SetItem; default;
  end;

implementation

{ TZUGFeRDInvoiceReferencedDocumentObjectList }

function TZUGFeRDInvoiceReferencedDocumentObjectList.Extract(Item: TObject): TZUGFeRDInvoiceReferencedDocument;
begin Result := TZUGFeRDInvoiceReferencedDocument(inherited Extract(Item)); end;

function TZUGFeRDInvoiceReferencedDocumentObjectList.First: TZUGFeRDInvoiceReferencedDocument;
begin if Count = 0 then Result := nil else Result := TZUGFeRDInvoiceReferencedDocument(inherited First); end;

function TZUGFeRDInvoiceReferencedDocumentObjectList.GetItem(Index: TInvoiceListItemType): TZUGFeRDInvoiceReferencedDocument;
begin Result := TZUGFeRDInvoiceReferencedDocument(inherited Items[Index]); end;

function TZUGFeRDInvoiceReferencedDocumentObjectList.Last: TZUGFeRDInvoiceReferencedDocument;
begin if Count = 0 then Result := nil else Result := TZUGFeRDInvoiceReferencedDocument(inherited Last); end;

procedure TZUGFeRDInvoiceReferencedDocumentObjectList.SetItem(Index: TInvoiceListItemType; AItem: TZUGFeRDInvoiceReferencedDocument);
begin inherited Items[Index] := AItem; end;

end.
