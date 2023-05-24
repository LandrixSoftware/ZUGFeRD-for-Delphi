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

unit intf.ZUGFeRDAdditionalReferencedDocumentTypeCodes.UnitTests;

interface

uses
  DUnitX.TestFramework, intf.ZUGFeRDAdditionalReferencedDocumentTypeCodes;

type
  [TestFixture]
  TZUGFeRDAdditionalReferencedDocumentTypeCodeTest = class
  public
    [Test]
    procedure TestFromString;
    [Test]
    procedure TestEnumValueToString;
    [Test]
    procedure TestEnumToString;
  end;

implementation

procedure TZUGFeRDAdditionalReferencedDocumentTypeCodeTest.TestFromString;
var
  Code: TZUGFeRDAdditionalReferencedDocumentTypeCode;
begin
  Code := TZUGFeRDAdditionalReferencedDocumentTypeCodeExtensions.FromString('916');
  Assert.AreEqual(TZUGFeRDAdditionalReferencedDocumentTypeCode.ReferenceDocument, Code);

  Code := TZUGFeRDAdditionalReferencedDocumentTypeCodeExtensions.FromString('130');
  Assert.AreEqual(TZUGFeRDAdditionalReferencedDocumentTypeCode.InvoiceDataSheet, Code);

  Code := TZUGFeRDAdditionalReferencedDocumentTypeCodeExtensions.FromString('50');
  Assert.AreEqual(TZUGFeRDAdditionalReferencedDocumentTypeCode.PriceSalesCatalogueResponse, Code);

  Code := TZUGFeRDAdditionalReferencedDocumentTypeCodeExtensions.FromString('65536');
  Assert.AreEqual(TZUGFeRDAdditionalReferencedDocumentTypeCode.Unknown, Code);

  Code := TZUGFeRDAdditionalReferencedDocumentTypeCodeExtensions.FromString('123');
  Assert.AreEqual(TZUGFeRDAdditionalReferencedDocumentTypeCode.ReferenceDocument, Code);
end;

procedure TZUGFeRDAdditionalReferencedDocumentTypeCodeTest.TestEnumToString;
var
  Value: string;
begin
  Value := TZUGFeRDAdditionalReferencedDocumentTypeCodeExtensions.EnumToString(TZUGFeRDAdditionalReferencedDocumentTypeCode.ReferenceDocument);
  Assert.AreEqual('916', Value);

  Value := TZUGFeRDAdditionalReferencedDocumentTypeCodeExtensions.EnumToString(TZUGFeRDAdditionalReferencedDocumentTypeCode.InvoiceDataSheet);
  Assert.AreEqual('130', Value);

  Value := TZUGFeRDAdditionalReferencedDocumentTypeCodeExtensions.EnumToString(TZUGFeRDAdditionalReferencedDocumentTypeCode.PriceSalesCatalogueResponse);
  Assert.AreEqual('50', Value);

  Value := TZUGFeRDAdditionalReferencedDocumentTypeCodeExtensions.EnumToString(TZUGFeRDAdditionalReferencedDocumentTypeCode.Unknown);
  Assert.AreEqual('65536', Value);
end;

procedure TZUGFeRDAdditionalReferencedDocumentTypeCodeTest.TestEnumValueToString;
var
  Value: string;
begin
  Value := TZUGFeRDAdditionalReferencedDocumentTypeCodeExtensions.EnumValueToString(TZUGFeRDAdditionalReferencedDocumentTypeCode.ReferenceDocument);
  Assert.AreEqual('916', Value);

  Value := TZUGFeRDAdditionalReferencedDocumentTypeCodeExtensions.EnumValueToString(TZUGFeRDAdditionalReferencedDocumentTypeCode.InvoiceDataSheet);
  Assert.AreEqual('130', Value);

  Value := TZUGFeRDAdditionalReferencedDocumentTypeCodeExtensions.EnumValueToString(TZUGFeRDAdditionalReferencedDocumentTypeCode.PriceSalesCatalogueResponse);
  Assert.AreEqual('50', Value);

  Value := TZUGFeRDAdditionalReferencedDocumentTypeCodeExtensions.EnumValueToString(TZUGFeRDAdditionalReferencedDocumentTypeCode.Unknown);
  Assert.AreEqual('65536', Value);
end;

initialization

//I was hoping to use RTTI to discover the TestFixture classes, however unlike .NET
//if we don't touch the class somehow then the linker will remove
//the class from the resulting exe.
//We could just do this:
//TMyExampleTests.ClassName;
//TExampleFixture2.ClassName;
//which is enough to make the compiler link the classes into the exe, but that seems a
//bit redundent so I guess we'll just use manual registration. If you use the
//{$STRONGLINKTYPES ON} compiler directive then it will link the TestFixtures in and you
//can use RTTI. The downside to that is the resulting exe will potentially much larger.
//Not sure which version {$STRONGLINKTYPES ON} was introduced so we'll allow RTTI and
//manual registration for now.

//  TDUnitX.RegisterTestFixture(TZUGFeRDAdditionalReferencedDocumentTypeCodeTest);

end.
