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

unit intf.ZUGFeRDLegalOrganization;

interface

uses
  System.SysUtils,System.TypInfo
  ,intf.ZUGFeRDGlobalID,intf.ZUGFeRDGlobalIDSchemeIdentifiers
  ;

type
  /// <summary>
  /// Details about a legal organization
  /// </summary>
  TZUGFeRDLegalOrganization = class
  private
    FID: TZUGFeRDGlobalID;
    FTradingBusinessName: string;
  public
    /// <summary>
    /// Create a new LegalOrganization instance
    /// </summary>
    constructor Create;
    /// <summary>
    /// Create a new LegalOrganization instance
    /// </summary>
    /// <param name="id"></param>
    /// <param name="schemeID"></param>
    /// <param name="tradingBusinessName"></param>
    constructor CreateWithParams(schemeID: TZUGFeRDGlobalIDSchemeIdentifiers = Unknown; id: string = ''; tradingBusinessName: string = '');
    destructor Destroy; override;
  public
    /// <summary>
    /// Legal organization ID
    /// </summary>
    property ID: TZUGFeRDGlobalID read FID write FID;
    /// <summary>
    /// Trading business name
    /// </summary>
    property TradingBusinessName: string read FTradingBusinessName write FTradingBusinessName;
  end;

implementation

constructor TZUGFeRDLegalOrganization.Create;
begin
  FID := nil;
  FTradingBusinessName := '';
end;

constructor TZUGFeRDLegalOrganization.CreateWithParams(schemeID: TZUGFeRDGlobalIDSchemeIdentifiers = Unknown; id: string = ''; tradingBusinessName: string = '');
begin
  FID := TZUGFeRDGlobalID.CreateWithParams(schemeID, id);
  FTradingBusinessName := tradingBusinessName;
end;

destructor TZUGFeRDLegalOrganization.Destroy;
begin
  if Assigned(FID) then begin FID.Free; FID := nil; end;
  inherited;
end;

end.
