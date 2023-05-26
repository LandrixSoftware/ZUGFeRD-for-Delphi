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

unit intf.ZUGFeRDContact;

interface

type
  /// <summary>
  /// Detail information of a contact person of e.g. a party
  /// </summary>
  TZUGFeRDContact = class
  private
    FName: string;
    FOrgUnit: string;
    FEmailAddress: string;
    FPhoneNo: string;
    FFaxNo: string;
  public
    /// <summary>
    /// Contact Name
    /// </summary>
    property Name: string read FName write FName;
    /// <summary>
    /// Contact organizational unit (with a party)
    /// </summary>
    property OrgUnit: string read FOrgUnit write FOrgUnit;
    /// <summary>
    /// Contact email address
    /// </summary>
    property EmailAddress: string read FEmailAddress write FEmailAddress;
    /// <summary>
    /// Contact phone number
    /// </summary>
    property PhoneNo: string read FPhoneNo write FPhoneNo;
    /// <summary>
    /// Contact fax number
    /// </summary>
    property FaxNo: string read FFaxNo write FFaxNo;
  end;

implementation

end.
