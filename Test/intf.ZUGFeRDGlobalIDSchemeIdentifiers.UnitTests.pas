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

unit intf.ZUGFeRDGlobalIDSchemeIdentifiers.UnitTests;

interface

uses
  DUnitX.TestFramework, intf.ZUGFeRDGlobalIDSchemeIdentifiers;

type
  [TestFixture]
  TTZUGFeRDGlobalIDSchemeIdentifiersTest = class
  public
    [Test]
    procedure TestFromString;
    [Test]
    procedure TestEnumToString;
  end;

implementation

procedure TTZUGFeRDGlobalIDSchemeIdentifiersTest.TestFromString;
begin
  Assert.AreEqual(TZUGFeRDGlobalIDSchemeIdentifiers.Sirene, TZUGFeRDGlobalIDSchemeIdentifiersExtensions.FromString('0002'));
  Assert.AreEqual(TZUGFeRDGlobalIDSchemeIdentifiers.SiretCode, TZUGFeRDGlobalIDSchemeIdentifiersExtensions.FromString('0009'));
  Assert.AreEqual(TZUGFeRDGlobalIDSchemeIdentifiers.Swift, TZUGFeRDGlobalIDSchemeIdentifiersExtensions.FromString('0021'));
  Assert.AreEqual(TZUGFeRDGlobalIDSchemeIdentifiers.DUNS, TZUGFeRDGlobalIDSchemeIdentifiersExtensions.FromString('0060'));
  Assert.AreEqual(TZUGFeRDGlobalIDSchemeIdentifiers.GLN, TZUGFeRDGlobalIDSchemeIdentifiersExtensions.FromString('0088'));
  Assert.AreEqual(TZUGFeRDGlobalIDSchemeIdentifiers.EAN, TZUGFeRDGlobalIDSchemeIdentifiersExtensions.FromString('0160'));
  Assert.AreEqual(TZUGFeRDGlobalIDSchemeIdentifiers.ODETTE, TZUGFeRDGlobalIDSchemeIdentifiersExtensions.FromString('0177'));
  Assert.AreEqual(TZUGFeRDGlobalIDSchemeIdentifiers.CompanyNumber, TZUGFeRDGlobalIDSchemeIdentifiersExtensions.FromString('0208'));
  Assert.AreEqual(TZUGFeRDGlobalIDSchemeIdentifiers.Unknown, TZUGFeRDGlobalIDSchemeIdentifiersExtensions.FromString('Invalid'));
end;

procedure TTZUGFeRDGlobalIDSchemeIdentifiersTest.TestEnumToString;
begin
  Assert.AreEqual('0002', TZUGFeRDGlobalIDSchemeIdentifiersExtensions.EnumToString(TZUGFeRDGlobalIDSchemeIdentifiers.Sirene));
  Assert.AreEqual('0009', TZUGFeRDGlobalIDSchemeIdentifiersExtensions.EnumToString(TZUGFeRDGlobalIDSchemeIdentifiers.SiretCode));
  Assert.AreEqual('0021', TZUGFeRDGlobalIDSchemeIdentifiersExtensions.EnumToString(TZUGFeRDGlobalIDSchemeIdentifiers.Swift));
  Assert.AreEqual('0060', TZUGFeRDGlobalIDSchemeIdentifiersExtensions.EnumToString(TZUGFeRDGlobalIDSchemeIdentifiers.DUNS));
  Assert.AreEqual('0088', TZUGFeRDGlobalIDSchemeIdentifiersExtensions.EnumToString(TZUGFeRDGlobalIDSchemeIdentifiers.GLN));
  Assert.AreEqual('0160', TZUGFeRDGlobalIDSchemeIdentifiersExtensions.EnumToString(TZUGFeRDGlobalIDSchemeIdentifiers.EAN));
  Assert.AreEqual('0177', TZUGFeRDGlobalIDSchemeIdentifiersExtensions.EnumToString(TZUGFeRDGlobalIDSchemeIdentifiers.ODETTE));
  Assert.AreEqual('0208', TZUGFeRDGlobalIDSchemeIdentifiersExtensions.EnumToString(TZUGFeRDGlobalIDSchemeIdentifiers.CompanyNumber));
  Assert.AreEqual('0000', TZUGFeRDGlobalIDSchemeIdentifiersExtensions.EnumToString(TZUGFeRDGlobalIDSchemeIdentifiers.Unknown));
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

//  TDUnitX.RegisterTestFixture(TTZUGFeRDGlobalIDSchemeIdentifiersTest);

end.
