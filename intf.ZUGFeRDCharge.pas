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

unit intf.ZUGFeRDCharge;

interface

uses
  intf.ZUGFeRDTax;

type
  /// <summary>
  /// Abstract base class for the various types
  /// of charges
  /// </summary>
  TZUGFeRDCharge = class
  private
    FTax: TZUGFeRDTax;
    FAmount: Currency;
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    /// Tax that is applied to the charge
    /// </summary>
    /// <value></value>
    property Tax: TZUGFeRDTax read FTax write FTax;
    /// <summary>
    /// Monetary charge amount, presented in the respective currency
    /// </summary>
    /// <value></value>
    property Amount: Currency read FAmount write FAmount;
  end;

implementation


{ TZUGFeRDCharge }

constructor TZUGFeRDCharge.Create;
begin
  FTax := TZUGFeRDTax.Create;
end;

destructor TZUGFeRDCharge.Destroy;
begin
  if Assigned(FTax) then begin FTax.Free; FTax := nil; end;
  inherited;
end;

end.
