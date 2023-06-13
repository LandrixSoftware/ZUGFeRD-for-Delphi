﻿{* Licensed to the Apache Software Foundation (ASF) under one
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

unit intf.ZUGFeRDNote;

interface

uses
  intf.ZUGFeRDContentCodes,
  intf.ZUGFeRDSubjectCodes;

type
  /// <summary>
  /// An aggregation of business terms to disclose free text which is invoice-relevant, as well as their qualification.
  /// </summary>
  TZUGFeRDNote = class
  private
    FContent: string;
    FSubjectCode: TZUGFeRDSubjectCodes;
    FContentCode: TZUGFeRDContentCodes;
  public
    /// <summary>
    /// Initialize a new node
    /// </summary>
    /// <param name="content"></param>
    /// <param name="subjectCode"></param>
    /// <param name="contentCode"></param>
    constructor Create(const content: string; subjectCode: TZUGFeRDSubjectCodes; contentCode: TZUGFeRDContentCodes);
  public
    /// <summary>
    /// A free text containing unstructured information which is relevant for the invoice as a whole.
    /// </summary>
    property Content: string read FContent write FContent;
    /// <summary>
    /// The qualification of the free text of an invoice of BT-22
    /// </summary>
    property SubjectCode: TZUGFeRDSubjectCodes read FSubjectCode write FSubjectCode default TZUGFeRDSubjectCodes.Unknown;
    /// <summary>
    /// Bilaterally agreed text blocks which, here, are transferred as code.
    /// </summary>
    property ContentCode: TZUGFeRDContentCodes read FContentCode write FContentCode default TZUGFeRDContentCodes.Unknown;
  end;

implementation

constructor TZUGFeRDNote.Create(const content: string; subjectCode: TZUGFeRDSubjectCodes; contentCode: TZUGFeRDContentCodes);
begin
  FContent := content;
  FSubjectCode := subjectCode;
  FContentCode := contentCode;
end;

end.
