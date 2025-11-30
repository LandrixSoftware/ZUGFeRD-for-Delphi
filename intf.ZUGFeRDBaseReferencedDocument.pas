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

unit intf.ZUGFeRDBaseReferencedDocument;

interface

uses System.SysUtils, intf.ZUGFeRDHelper;

type
  /// <summary>
  /// Base class for reference documents of all kinds.
  /// </summary>
  TZUGFeRDBaseReferencedDocument = class
  private
    FID: string;
    FIssueDateTime: ZUGFeRDNullable<TDateTime>;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// The identifier of the tendering or batch the invoice refers to, or an identifier for an object
    /// given by the seller the invoice refers to, or an identifier of the documents substantiating the invoice.
    /// </summary>
    property ID: string read FID write FID;
    /// <summary>
    /// Bestelldatum / Lieferdatum
    /// </summary>
    property IssueDateTime: ZUGFeRDNullable<TDateTime> read FIssueDateTime write FIssueDateTime;
  end;

implementation

{ TZUGFeRDBaseReferencedDocument }

constructor TZUGFeRDBaseReferencedDocument.Create;
begin
  inherited Create;
//  FIssueDateTime := ZUGFeRDNullable<TDateTime>.Create;
end;

destructor TZUGFeRDBaseReferencedDocument.Destroy;
begin
//  if Assigned(FIssueDateTime) then begin FIssueDateTime.Free; FIssueDateTime := nil; end;
  inherited;
end;

end.
