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

unit intf.ZUGFeRDIncludedReferencedProduct;

interface

uses
  System.Generics.Collections,
  intf.ZUGFeRDHelper,
  intf.ZUGFeRDQuantityCodes,
  intf.ZUGFeRDGlobalID;

type
  /// <summary>
  /// An included Item referenced from this trade product.
  /// </summary>
  TZUGFeRDIncludedReferencedProduct = class
  private
    FName: string;
    FUnitQuantity: ZUGFeRDNullable<Currency>;
    FUnitCode: TZUGFeRDQuantityCodes;
    FSellerAssignedID: string;
    FBuyerAssignedID: string;
    FIndustryAssignedID: string;
    FGlobalID: TZUGFeRDGlobalID;
    FDescription: string;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Name of Included Item
    ///
    /// BT-X-18
    /// </summary>
    property Name: string read FName write FName;

    /// <summary>
    /// Included quantity
    ///
    /// BT-X-20
    /// </summary>
    property UnitQuantity: ZUGFeRDNullable<Currency> read FUnitQuantity write FUnitQuantity;

    /// <summary>
    /// Item Base Quantity Unit Code
    ///
    /// BT-X-20-1
    /// </summary>
    property UnitCode: TZUGFeRDQuantityCodes read FUnitCode write FUnitCode;

    /// <summary>
    /// The identification of articles based on a registered scheme
    ///
    /// GlobalID of Included Item
    /// </summary>
    property GlobalID: TZUGFeRDGlobalID read FGlobalID write FGlobalID;

    /// <summary>
    /// Included Item Seller's identifier
    /// </summary>
    property SellerAssignedID: string read FSellerAssignedID write FSellerAssignedID;

    /// <summary>
    /// Included Item Buyer's identifier
    /// </summary>
    property BuyerAssignedID: string read FBuyerAssignedID write FBuyerAssignedID;

    /// <summary>
    /// Industry AssignedID of Included Item
    /// </summary>
    property IndustryAssignedID: string read FIndustryAssignedID write FIndustryAssignedID;

    /// <summary>
    /// The description of an item
    ///
    /// The item’s description makes it possible to describe a product and its properties more comprehensively
    /// than would be possible with just the article name.
    /// </summary>
    property Description: string read FDescription write FDescription;

   end;

implementation

constructor TZUGFeRDIncludedReferencedProduct.Create;
begin
  inherited Create;
  FGlobalID := TZUGFeRDGlobalID.Create;
end;

destructor TZUGFeRDIncludedReferencedProduct.Destroy;
begin
  if Assigned(FGlobalID) then begin FGlobalID.Free; FGlobalID := nil; end;
  inherited;
end;

end.
