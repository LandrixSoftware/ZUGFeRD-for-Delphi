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

unit intf.ZUGFeRDAccountingAccountTypeCodes;

interface

uses
  System.SysUtils,System.TypInfo
  ;

type
  //based on https://www.unece.org/fileadmin/DAM/uncefact/codelist/standard/EDIFICASEU_AccountingAccountType_D11A.xsd

  /// <summary>Account Types (EDIFICAS-EU Code List)</summary>
  TZUGFeRDAccountingAccountTypeCodes = (
    /// <summary>
    /// TypeCode not set
    /// </summary>
    Unknown = 0,
    /// <summary>
    /// The code indicates a financial account
    /// </summary>
    Financial = 1,
    /// <summary>
    /// The code indicates a subsidiary account
    /// </summary>
    Subsidiary = 2,
    /// <summary>
    /// The code indicates a budget account
    /// </summary>
    Budget = 3,
    /// <summary>
    /// The code indicates a cost accounting account
    /// </summary>
    Cost_Accounting = 4,
    /// <summary>
    /// The code indicates a receivable account
    /// </summary>
    Receivable = 5,
    /// <summary>
    /// The code indicates a payable account
    /// </summary>
    Payable = 6,
    /// <summary>
    /// The code indicates a job cost accounting
    /// </summary>
    Job_Cost_Accounting = 7
  );

  TZUGFeRDAccountingAccountTypeCodesExtensions = class
  public
    class function FromString(const s: string): TZUGFeRDAccountingAccountTypeCodes;
    class function EnumToString(codes: TZUGFeRDAccountingAccountTypeCodes): string;
  end;

implementation

{ TZUGFeRDAccountingAccountTypeCodesExtensions }

class function TZUGFeRDAccountingAccountTypeCodesExtensions.EnumToString(
  codes: TZUGFeRDAccountingAccountTypeCodes): string;
begin
  if codes = TZUGFeRDAccountingAccountTypeCodes.Unknown then
    Result := ''
  else
    Result := IntToStr(Integer(codes));
end;

class function TZUGFeRDAccountingAccountTypeCodesExtensions.FromString(
  const s: string): TZUGFeRDAccountingAccountTypeCodes;
begin
  case StrToIntDef(s,0) of
    1 : Result := TZUGFeRDAccountingAccountTypeCodes.Financial;
    2 : Result := TZUGFeRDAccountingAccountTypeCodes.Subsidiary;
    3 : Result := TZUGFeRDAccountingAccountTypeCodes.Budget;
    4 : Result := TZUGFeRDAccountingAccountTypeCodes.Cost_Accounting;
    5 : Result := TZUGFeRDAccountingAccountTypeCodes.Receivable;
    6 : Result := TZUGFeRDAccountingAccountTypeCodes.Payable;
    7 : Result := TZUGFeRDAccountingAccountTypeCodes.Job_Cost_Accounting;
    else Result := TZUGFeRDAccountingAccountTypeCodes.Unknown;
  end;
end;

end.
