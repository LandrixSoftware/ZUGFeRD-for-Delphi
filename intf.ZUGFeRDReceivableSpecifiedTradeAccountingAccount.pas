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

unit intf.ZUGFeRDReceivableSpecifiedTradeAccountingAccount;

interface

uses
  intf.ZUGFeRDHelper,
  intf.ZUGFeRDAccountingAccountTypeCodes;

type
  /// <summary>Detailinformationen zur Buchungsreferenz</summary>
  TZUGFeRDReceivableSpecifiedTradeAccountingAccount = class
  private
    FTradeAccountID: string;
    FTradeAccountTypeCode: ZUGFeRDNullable<TZUGFeRDAccountingAccountTypeCodes>;
  public
    /// <summary>
    /// Ein Textwert, der angibt, an welcher Stelle die betreffenden Daten in den Finanzkonten des Käufers zu verbuchen sind
    /// </summary>
    property TradeAccountID: string read FTradeAccountID write FTradeAccountID;
    /// <summary>
    /// EDIFICAS-EU Type Codes: https://www.unece.org/fileadmin/DAM/uncefact/codelist/standard/EDIFICASEU_AccountingAccountType_D11A.xsd
    /// </summary>
    property TradeAccountTypeCode: ZUGFeRDNullable<TZUGFeRDAccountingAccountTypeCodes> read FTradeAccountTypeCode write FTradeAccountTypeCode;
  end;

implementation

end.
