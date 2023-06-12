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

unit intf.ZUGFeRDPaymentMeans;

interface

uses
  intf.ZUGFeRDFinancialCard,intf.ZUGFeRDPaymentMeansTypeCodes;

type
  TZUGFeRDPaymentMeans = class
  private
    FTypeCode: TZUGFeRDPaymentMeansTypeCodes;
    FInformation: string;
    FSEPACreditorIdentifier: string;
    FSEPAMandateReference: string;
    FFinancialCard: TZUGFeRDFinancialCard;
  public
    constructor Create;
    destructor Destroy; override;
  public
    /// <summary>
    /// The means expressed as code, for how a payment is expected to be or has been settled.
    /// </summary>
    property TypeCode: TZUGFeRDPaymentMeansTypeCodes read FTypeCode write FTypeCode;

    /// <summary>
    /// The means expressed as code, for how a payment is expected to be or has been settled.
    /// </summary>
    property Information: string read FInformation write FInformation;

    /// <summary>
    /// Gläubiger-Identifikationsnummer
    ///
    /// https://de.wikipedia.org/wiki/Gl%C3%A4ubiger-Identifikationsnummer
    /// </summary>
    property SEPACreditorIdentifier: string read FSEPACreditorIdentifier write FSEPACreditorIdentifier;

    /// <summary>
    /// Mandatsreferenz
    ///
    /// https://de.wikipedia.org/wiki/Mandatsreferenz
    /// </summary>
    property SEPAMandateReference: string read FSEPAMandateReference write FSEPAMandateReference;

    /// <summary>
    /// Payment card information.
    /// </summary>
    property FinancialCard: TZUGFeRDFinancialCard read FFinancialCard write FFinancialCard;
  end;

implementation

{ TZUGFeRDPaymentMeans }

constructor TZUGFeRDPaymentMeans.Create;
begin
  FFinancialCard := TZUGFeRDFinancialCard.Create;
end;

destructor TZUGFeRDPaymentMeans.Destroy;
begin
  if Assigned(FFinancialCard) then begin FFinancialCard.Free; FFinancialCard := nil; end;
  inherited;
end;

end.
