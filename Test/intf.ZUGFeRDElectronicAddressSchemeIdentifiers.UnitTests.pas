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

unit intf.ZUGFeRDElectronicAddressSchemeIdentifiers.UnitTests;

interface

uses
  DUnitX.TestFramework, intf.ZUGFeRDElectronicAddressSchemeIdentifiers;

type
  [TestFixture]
  TZUGFeRDElectronicAddressSchemeIdentifiersTest = class
  public
    [Test]
    procedure TestFromString;
    [Test]
    procedure TestEnumToString;
  end;

implementation

procedure TZUGFeRDElectronicAddressSchemeIdentifiersTest.TestFromString;
begin
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.EanLocationCode, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('0088'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.LeitwegID, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('0204'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.HungaryVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9910'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.AndorraVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9922'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.AlbaniaVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9923'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.BosniaAndHerzegovinaVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9924'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.BelgiumVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9925'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.BulgariaVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9926'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.SwitzerlandVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9927'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.CyprusVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9928'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.CzechRepublicVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9929'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.GermanyVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9930'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.EstoniaVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9931'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.UnitedKingdomVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9932'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.GreeceVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9933'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.CroatiaVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9934'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.IrelandVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9935'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.LiechtensteinVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9936'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.LithuaniaVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9937'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.LuxemburgVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9938'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.LatviaVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9939'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.MonacoVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9940'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.MontenegroVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9941'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.MacedoniaVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9942'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.MaltaVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9943'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.NetherlandsVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9944'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.PolandVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9945'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.PortugalVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9946'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.RomaniaVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9947'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.SerbiaVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9948'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.SloveniaVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9949'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.SlovakiaVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9950'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.SanMarinoVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9951'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.TurkeyVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9952'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.HolySeeVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9953'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.SwedishVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9955'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.FrenchVatNumber, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('9957'));
  Assert.AreEqual(TZUGFeRDElectronicAddressSchemeIdentifiers.Unknown, TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.FromString('Invalid'));
end;

procedure TZUGFeRDElectronicAddressSchemeIdentifiersTest.TestEnumToString;
begin
  Assert.AreEqual('0088', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.EanLocationCode));
  Assert.AreEqual('0204', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.LeitwegID));
  Assert.AreEqual('9910', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.HungaryVatNumber));
  Assert.AreEqual('9922', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.AndorraVatNumber));
  Assert.AreEqual('9923', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.AlbaniaVatNumber));
  Assert.AreEqual('9924', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.BosniaAndHerzegovinaVatNumber));
  Assert.AreEqual('9925', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.BelgiumVatNumber));
  Assert.AreEqual('9926', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.BulgariaVatNumber));
  Assert.AreEqual('9927', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.SwitzerlandVatNumber));
  Assert.AreEqual('9928', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.CyprusVatNumber));
  Assert.AreEqual('9929', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.CzechRepublicVatNumber));
  Assert.AreEqual('9930', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.GermanyVatNumber));
  Assert.AreEqual('9931', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.EstoniaVatNumber));
  Assert.AreEqual('9932', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.UnitedKingdomVatNumber));
  Assert.AreEqual('9933', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.GreeceVatNumber));
  Assert.AreEqual('9934', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.CroatiaVatNumber));
  Assert.AreEqual('9935', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.IrelandVatNumber));
  Assert.AreEqual('9936', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.LiechtensteinVatNumber));
  Assert.AreEqual('9937', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.LithuaniaVatNumber));
  Assert.AreEqual('9938', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.LuxemburgVatNumber));
  Assert.AreEqual('9939', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.LatviaVatNumber));
  Assert.AreEqual('9940', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.MonacoVatNumber));
  Assert.AreEqual('9941', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.MontenegroVatNumber));
  Assert.AreEqual('9942', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.MacedoniaVatNumber));
  Assert.AreEqual('9943', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.MaltaVatNumber));
  Assert.AreEqual('9944', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.NetherlandsVatNumber));
  Assert.AreEqual('9945', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.PolandVatNumber));
  Assert.AreEqual('9946', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.PortugalVatNumber));
  Assert.AreEqual('9947', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.RomaniaVatNumber));
  Assert.AreEqual('9948', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.SerbiaVatNumber));
  Assert.AreEqual('9949', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.SloveniaVatNumber));
  Assert.AreEqual('9950', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.SlovakiaVatNumber));
  Assert.AreEqual('9951', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.SanMarinoVatNumber));
  Assert.AreEqual('9952', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.TurkeyVatNumber));
  Assert.AreEqual('9953', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.HolySeeVatNumber));
  Assert.AreEqual('9955', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.SwedishVatNumber));
  Assert.AreEqual('9957', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.FrenchVatNumber));
  Assert.AreEqual('0000', TZUGFeRDElectronicAddressSchemeIdentifiersExtensions.EnumToString(TZUGFeRDElectronicAddressSchemeIdentifiers.Unknown));
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

//  TDUnitX.RegisterTestFixture(TZUGFeRDElectronicAddressSchemeIdentifiersTest);

end.
