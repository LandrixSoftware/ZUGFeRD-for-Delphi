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

unit intf.ZUGFeRDTaxRegistrationSchemeID.UnitTests;

interface

uses
  DUnitX.TestFramework, intf.ZUGFeRDTaxRegistrationSchemeID;

type
  [TestFixture]
  TZUGFeRDTaxRegistrationSchemeIDTest = class
  public
    [Test]
    procedure TestFromString;
    [Test]
    procedure TestEnumToString;
  end;

implementation

procedure TZUGFeRDTaxRegistrationSchemeIDTest.TestFromString;
begin
  Assert.AreEqual(TZUGFeRDTaxRegistrationSchemeID.FC, TZUGFeRDTaxRegistrationSchemeIDExtensions.FromString('FC'));
  Assert.AreEqual(TZUGFeRDTaxRegistrationSchemeID.VA, TZUGFeRDTaxRegistrationSchemeIDExtensions.FromString('VA'));
  Assert.AreEqual(TZUGFeRDTaxRegistrationSchemeID.Unknown, TZUGFeRDTaxRegistrationSchemeIDExtensions.FromString('Unknown'));
  Assert.AreEqual(TZUGFeRDTaxRegistrationSchemeID.Unknown, TZUGFeRDTaxRegistrationSchemeIDExtensions.FromString('Invalid'));
end;

procedure TZUGFeRDTaxRegistrationSchemeIDTest.TestEnumToString;
begin
  Assert.AreEqual('FC', TZUGFeRDTaxRegistrationSchemeIDExtensions.EnumToString(TZUGFeRDTaxRegistrationSchemeID.FC));
  Assert.AreEqual('VA', TZUGFeRDTaxRegistrationSchemeIDExtensions.EnumToString(TZUGFeRDTaxRegistrationSchemeID.VA));
  Assert.AreEqual('Unknown', TZUGFeRDTaxRegistrationSchemeIDExtensions.EnumToString(TZUGFeRDTaxRegistrationSchemeID.Unknown));
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

//  TDUnitX.RegisterTestFixture(TZUGFeRDTaxRegistrationSchemeIDTest);

end.
