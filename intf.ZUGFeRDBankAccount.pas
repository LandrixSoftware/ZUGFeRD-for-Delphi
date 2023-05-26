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

unit intf.ZUGFeRDBankAccount;

interface

type
  /// <summary>
  /// This class holds information about a bank account. The class is used in different places,
  /// e.g. for holding supplier and customer bank information
  /// </summary>
  TZUGFeRDBankAccount = class
  private
    FID: string;
    FIBAN: string;
    FBIC: string;
    FBankleitzahl: string;
    FBankName: string;
    FName: string;
  public
    /// <summary>
    /// National account number (not SEPA)
    /// </summary>
    property ID: string read FID write FID;
    /// <summary>
    /// IBAN identifier for the bank account. This information is not yet validated.
    /// </summary>
    property IBAN: string read FIBAN write FIBAN;
    /// <summary>
    /// Payment service provider identifier
    /// </summary>
    property BIC: string read FBIC write FBIC;
    /// <summary>
    /// Legacy bank identifier
    /// </summary>
    property Bankleitzahl: string read FBankleitzahl write FBankleitzahl;
    /// <summary>
    /// Clear text name of the bank
    /// </summary>
    property BankName: string read FBankName write FBankName;
    /// <summary>
    /// Payment account name
    /// </summary>
    property Name: string read FName write FName;
  end;

implementation

end.
