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
  DUnitX.TestFramework, intf.ZUGFeRDTaxCategoryCodes, intf.ZUGFeRDHelper;

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
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.A, TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum('A'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.AA, TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum('AA'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.AB, TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum('AB'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.AC, TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum('AC'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.AD, TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum('AD'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.AE, TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum('AE'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.B, TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum('B'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.C, TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum('C'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.D, TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum('D'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.E, TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum('E'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.F, TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum('F'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.G, TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum('G'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.H, TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum('H'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.I, TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum('I'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.J, TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum('J'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.K, TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum('K'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.L, TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum('L'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.M, TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum('M'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.O, TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum('O'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.S, TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum('S'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.Z, TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum('Z'));
  Assert.AreEqual(TZUGFeRDTaxCategoryCodes.Unknown, TEnumExtensions<TZUGFeRDTaxCategoryCodes>.StringToEnum('Invalid'));
end;

procedure TZUGFeRDTaxCategoryCodesTest.TestEnumToString;
begin
  Assert.AreEqual('A', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(TZUGFeRDTaxCategoryCodes.A));
  Assert.AreEqual('AA', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(TZUGFeRDTaxCategoryCodes.AA));
  Assert.AreEqual('AB', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(TZUGFeRDTaxCategoryCodes.AB));
  Assert.AreEqual('AC', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(TZUGFeRDTaxCategoryCodes.AC));
  Assert.AreEqual('AD', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(TZUGFeRDTaxCategoryCodes.AD));
  Assert.AreEqual('AE', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(TZUGFeRDTaxCategoryCodes.AE));
  Assert.AreEqual('B', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(TZUGFeRDTaxCategoryCodes.B));
  Assert.AreEqual('C', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(TZUGFeRDTaxCategoryCodes.C));
  Assert.AreEqual('D', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(TZUGFeRDTaxCategoryCodes.D));
  Assert.AreEqual('E', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(TZUGFeRDTaxCategoryCodes.E));
  Assert.AreEqual('F', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(TZUGFeRDTaxCategoryCodes.F));
  Assert.AreEqual('G', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(TZUGFeRDTaxCategoryCodes.G));
  Assert.AreEqual('H', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(TZUGFeRDTaxCategoryCodes.H));
  Assert.AreEqual('I', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(TZUGFeRDTaxCategoryCodes.I));
  Assert.AreEqual('J', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(TZUGFeRDTaxCategoryCodes.J));
  Assert.AreEqual('K', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(TZUGFeRDTaxCategoryCodes.K));
  Assert.AreEqual('L', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(TZUGFeRDTaxCategoryCodes.L));
  Assert.AreEqual('M', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(TZUGFeRDTaxCategoryCodes.M));
  Assert.AreEqual('O', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(TZUGFeRDTaxCategoryCodes.O));
  Assert.AreEqual('S', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(TZUGFeRDTaxCategoryCodes.S));
  Assert.AreEqual('Z', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(TZUGFeRDTaxCategoryCodes.Z));
  Assert.AreEqual('Unknown', TEnumExtensions<TZUGFeRDTaxCategoryCodes>.EnumToString(TZUGFeRDTaxCategoryCodes.Unknown));
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
