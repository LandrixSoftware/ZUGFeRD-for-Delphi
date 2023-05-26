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

unit intf.ZUGFeRDTaxCategoryCodes.UnitTests;

interface

uses
  DUnitX.TestFramework, intf.ZUGFeRDTaxCategoryCodes;

type
  [TestFixture]
  TZUGFeRDTaxCategoryCodesTest = class
  public
    [Test]
    procedure TestFromString;
    [Test]
    procedure TestEnumToString;
  end;

implementation

procedure TZUGFeRDTaxCategoryCodesTest.TestFromString;
begin
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.A, TZUGFeRDTaxCategoryCodesExtensions.FromString('A'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.AA, TZUGFeRDTaxCategoryCodesExtensions.FromString('AA'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.AB, TZUGFeRDTaxCategoryCodesExtensions.FromString('AB'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.AC, TZUGFeRDTaxCategoryCodesExtensions.FromString('AC'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.AD, TZUGFeRDTaxCategoryCodesExtensions.FromString('AD'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.AE, TZUGFeRDTaxCategoryCodesExtensions.FromString('AE'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.B, TZUGFeRDTaxCategoryCodesExtensions.FromString('B'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.C, TZUGFeRDTaxCategoryCodesExtensions.FromString('C'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.D, TZUGFeRDTaxCategoryCodesExtensions.FromString('D'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.E, TZUGFeRDTaxCategoryCodesExtensions.FromString('E'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.F, TZUGFeRDTaxCategoryCodesExtensions.FromString('F'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.G, TZUGFeRDTaxCategoryCodesExtensions.FromString('G'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.H, TZUGFeRDTaxCategoryCodesExtensions.FromString('H'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.I, TZUGFeRDTaxCategoryCodesExtensions.FromString('I'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.J, TZUGFeRDTaxCategoryCodesExtensions.FromString('J'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.K, TZUGFeRDTaxCategoryCodesExtensions.FromString('K'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.L, TZUGFeRDTaxCategoryCodesExtensions.FromString('L'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.M, TZUGFeRDTaxCategoryCodesExtensions.FromString('M'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.O, TZUGFeRDTaxCategoryCodesExtensions.FromString('O'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.S, TZUGFeRDTaxCategoryCodesExtensions.FromString('S'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.Z, TZUGFeRDTaxCategoryCodesExtensions.FromString('Z'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.Unknown, TZUGFeRDTaxCategoryCodesExtensions.FromString('Invalid'));
end;

procedure TZUGFeRDTaxCategoryCodesTest.TestEnumToString;
begin
  Assert.AreEqual('A', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(TZUGFeRDTaxCategoryCodes.A));
  Assert.AreEqual('AA', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(TZUGFeRDTaxCategoryCodes.AA));
  Assert.AreEqual('AB', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(TZUGFeRDTaxCategoryCodes.AB));
  Assert.AreEqual('AC', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(TZUGFeRDTaxCategoryCodes.AC));
  Assert.AreEqual('AD', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(TZUGFeRDTaxCategoryCodes.AD));
  Assert.AreEqual('AE', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(TZUGFeRDTaxCategoryCodes.AE));
  Assert.AreEqual('B', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(TZUGFeRDTaxCategoryCodes.B));
  Assert.AreEqual('C', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(TZUGFeRDTaxCategoryCodes.C));
  Assert.AreEqual('D', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(TZUGFeRDTaxCategoryCodes.D));
  Assert.AreEqual('E', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(TZUGFeRDTaxCategoryCodes.E));
  Assert.AreEqual('F', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(TZUGFeRDTaxCategoryCodes.F));
  Assert.AreEqual('G', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(TZUGFeRDTaxCategoryCodes.G));
  Assert.AreEqual('H', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(TZUGFeRDTaxCategoryCodes.H));
  Assert.AreEqual('I', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(TZUGFeRDTaxCategoryCodes.I));
  Assert.AreEqual('J', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(TZUGFeRDTaxCategoryCodes.J));
  Assert.AreEqual('K', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(TZUGFeRDTaxCategoryCodes.K));
  Assert.AreEqual('L', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(TZUGFeRDTaxCategoryCodes.L));
  Assert.AreEqual('M', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(TZUGFeRDTaxCategoryCodes.M));
  Assert.AreEqual('O', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(TZUGFeRDTaxCategoryCodes.O));
  Assert.AreEqual('S', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(TZUGFeRDTaxCategoryCodes.S));
  Assert.AreEqual('Z', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(TZUGFeRDTaxCategoryCodes.Z));
  Assert.AreEqual('Unknown', TZUGFeRDTaxCategoryCodesExtensions.EnumToString(TZUGFeRDTaxCategoryCodes.Unknown));
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

//  TDUnitX.RegisterTestFixture(TZUGFeRDTaxCategoryCodesTest);

end.
