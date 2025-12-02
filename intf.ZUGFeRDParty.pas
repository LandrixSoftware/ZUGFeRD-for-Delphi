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

unit intf.ZUGFeRDParty;

interface

uses
  intf.ZUGFeRDGlobalID,
  intf.ZUGFeRDCountryCodes,
  intf.ZUGFeRDLegalOrganization,
  intf.ZUGFeRDHelper
  ;

type
  /// <summary>
  /// Detailed information about a party that has a certain role within an invoice
  /// </summary>
  TZUGFeRDParty = class
  private
    FID: TZUGFeRDGlobalID;
    FName: string;
    FDescription: string;
    FContactName: string;
    FCity: string;
    FPostcode: string;
    FCountry: ZUGFeRDNullable<TZUGFeRDCountryCodes>;
    FStreet: string;
    FGlobalID: TZUGFeRDGlobalID;
    FAddressLine3: string;
    FCountrySubdivisionName: string;
    FSpecifiedLegalOrganization: TZUGFeRDLegalOrganization;
  public
    constructor Create;
    destructor Destroy; override;
  public
    /// <summary>
    /// Party identifier
    /// </summary>
    property ID: TZUGFeRDGlobalID read FID write FID;

    /// <summary>
    /// The full formal name by which the party is registered in the national
    /// registry of legal entities or as a Taxable person or otherwise trades
    /// as a person or persons.
    /// </summary>
    property Name: string read FName write FName;

    /// <summary>
    /// other legal information of the seller (BT-33) Seller only
    /// </summary>
    property Description: string read FDescription write FDescription;

    /// <summary>
    /// Name of the contact at the party
    /// </summary>
    property ContactName: string read FContactName write FContactName;

    /// <summary>
    /// City, not including postcode (separate property)
    /// </summary>
    property City: string read FCity write FCity;

    /// <summary>
    /// Party postcode, represented in the respective country format
    /// </summary>
    property Postcode: string read FPostcode write FPostcode;

    /// <summary>
    /// Party country
    /// </summary>
    property Country: ZUGFeRDNullable<TZUGFeRDCountryCodes> read FCountry write FCountry;

    /// <summary>
    /// Street name and number
    /// </summary>
    property Street: string read FStreet write FStreet;

    /// <summary>
    /// Global identifier
    /// </summary>
    property GlobalID: TZUGFeRDGlobalID read FGlobalID write FGlobalID;

    /// <summary>
    /// Address line 3
    /// It's an additional line to give more details to the address.
    /// This field is purely optional.
    /// e.g. used for BT-162, BT-164, BT-165
    /// </summary>
    property AddressLine3: string read FAddressLine3 write FAddressLine3;

    /// <summary>
    /// Country subdivision (e.g. Niedersachsen, Bayern)
    /// This field is purely optional.
    /// e.g. used for BT-39, BT-54, BT-68, BT-79
    /// </summary>
    property CountrySubdivisionName: string read FCountrySubdivisionName write FCountrySubdivisionName;

    /// <summary>
    /// Legal organization
    /// </summary>
    property SpecifiedLegalOrganization: TZUGFeRDLegalOrganization read FSpecifiedLegalOrganization write FSpecifiedLegalOrganization;
  end;

implementation

constructor TZUGFeRDParty.Create;
begin
  inherited Create;
  FID := TZUGFeRDGlobalID.Create;
  FGlobalID := TZUGFeRDGlobalID.Create;
  FSpecifiedLegalOrganization := nil;//TZUGFeRDLegalOrganization.Create;
end;

destructor TZUGFeRDParty.Destroy;
begin
  if Assigned(FID) then begin FID.Free; FID := nil; end;
  if Assigned(FGlobalID) then begin FGlobalID.Free; FGlobalID := nil; end;
  if Assigned(FSpecifiedLegalOrganization) then begin FSpecifiedLegalOrganization.Free; FSpecifiedLegalOrganization := nil; end;
  inherited;
end;

end.
