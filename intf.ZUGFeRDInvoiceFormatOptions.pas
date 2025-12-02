unit intf.ZUGFeRDInvoiceFormatOptions;

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

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections;

type
  TZUGFeRDInvoiceFormatOptions = class sealed
  private
    FXmlHeaderComments: TList<string>;
    FIncludeXmlComments: Boolean;
    FAutomaticallyCleanInvalidCharacters: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Clone: TZUGFeRDInvoiceFormatOptions;

    property XmlHeaderComments: TList<string> read FXmlHeaderComments;
    property IncludeXmlComments: Boolean read FIncludeXmlComments write FIncludeXmlComments;
    property AutomaticallyCleanInvalidCharacters: Boolean read FAutomaticallyCleanInvalidCharacters write FAutomaticallyCleanInvalidCharacters;
  end;

implementation

{ TInvoiceFormatOptions }

constructor TZUGFeRDInvoiceFormatOptions.Create;
begin
  inherited Create;
  FXmlHeaderComments := TList<string>.Create;
  FIncludeXmlComments := False;
  FAutomaticallyCleanInvalidCharacters := False;
end;

destructor TZUGFeRDInvoiceFormatOptions.Destroy;
begin
  FXmlHeaderComments.Free;
  inherited;
end;

function TZUGFeRDInvoiceFormatOptions.Clone: TZUGFeRDInvoiceFormatOptions;
var
  Comment: string;
begin
  Result := TZUGFeRDInvoiceFormatOptions.Create;
  try
    // Copy all comments from the list
    for Comment in Self.FXmlHeaderComments do
      Result.FXmlHeaderComments.Add(Comment);

    Result.FIncludeXmlComments := Self.FIncludeXmlComments;
    Result.FAutomaticallyCleanInvalidCharacters := Self.FAutomaticallyCleanInvalidCharacters;
  except
    Result.Free;
    raise;
  end;
end;

end.
