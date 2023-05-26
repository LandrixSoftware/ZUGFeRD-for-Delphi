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

unit intf.ZUGFeRDGlobalID;

interface

uses
  intf.ZUGFeRDGlobalIDSchemeIdentifiers
  ;

type
  /// <summary>
  /// Global ID definition according to ISO 6523:
  /// http://en.wikipedia.org/wiki/ISO_6523
  ///
  /// For a full list of identifiers, see here:
  /// http://www.oid-info.com/doc/ICD-list.pdf
  ///
  /// Especially important for ZUGFeRD:
  /// Code 0160 - GTIN - Global Trade Item Number for articles
  /// Code 0088 - EAN Location Code
  /// </summary>
  TZUGFeRDGlobalID = class
  private
    FID: string;
    FSchemeID: TZUGFeRDGlobalIDSchemeIdentifiers;
  public
    /// <summary>
    /// The identification of articles based on a registered scheme
    ///
    /// The global identifier of the article is a globally unique identifier of the product being assigned to it by its producer, bases on the rules of a global standardisation body.
    /// </summary>
    property ID: string read FID write FID;
    /// <summary>
    /// The identification of items based on a registered scheme
    ///
    /// The schema of identification must be composed of the entries of the list published by the ISO/IEC 6523 Maintenance Agency.
    /// </summary>
    property SchemeID: TZUGFeRDGlobalIDSchemeIdentifiers read FSchemeID write FSchemeID;

    constructor Create;
    constructor CreateWithParams(schemeID: TZUGFeRDGlobalIDSchemeIdentifiers; ID: string);
  end;

implementation

constructor TZUGFeRDGlobalID.Create;
begin
  FID := '';
  FSchemeID := TZUGFeRDGlobalIDSchemeIdentifiers.Unknown;
end;

constructor TZUGFeRDGlobalID.CreateWithParams(schemeID: TZUGFeRDGlobalIDSchemeIdentifiers; ID: string);
begin
  FID := ID;
  FSchemeID := schemeID;
end;

end.
