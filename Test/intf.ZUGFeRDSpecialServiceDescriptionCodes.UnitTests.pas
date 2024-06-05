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

unit intf.ZUGFeRDSpecialServiceDescriptionCodes.UnitTests;

interface

uses
  DUnitX.TestFramework,
  intf.ZUGFeRDSpecialServiceDescriptionCodes;

type
  [TestFixture]
  TZUGFeRDSpecialServiceDescriptionCodesTest = class
  public
    [Test]
    procedure TestFromString;
    [Test]
    procedure TestEnumToString;
  end;

implementation

procedure TZUGFeRDSpecialServiceDescriptionCodesTest.TestFromString;
begin
  Assert.AreEqual(TZUGFeRDSpecialServiceDescriptionCodes.AA_Advertising,TZUGFeRDSpecialServiceDescriptionCodesExtensions.FromString('AA'));
  Assert.AreEqual(TZUGFeRDSpecialServiceDescriptionCodes.AAA_Telecommunication,TZUGFeRDSpecialServiceDescriptionCodesExtensions.FromString('AAA'));
  Assert.AreEqual(TZUGFeRDSpecialServiceDescriptionCodes.ABK_Miscellaneous,TZUGFeRDSpecialServiceDescriptionCodesExtensions.FromString('ABK'));
  Assert.AreEqual(TZUGFeRDSpecialServiceDescriptionCodes.ABL_AdditionalPackaging,TZUGFeRDSpecialServiceDescriptionCodesExtensions.FromString('ABL'));
  Assert.AreEqual(TZUGFeRDSpecialServiceDescriptionCodes.ADR_OtherServices,TZUGFeRDSpecialServiceDescriptionCodesExtensions.FromString('ADR'));
  Assert.AreEqual(TZUGFeRDSpecialServiceDescriptionCodes.ADT_Pickup,TZUGFeRDSpecialServiceDescriptionCodesExtensions.FromString('ADT'));
  Assert.AreEqual(TZUGFeRDSpecialServiceDescriptionCodes.FC_FreightService,TZUGFeRDSpecialServiceDescriptionCodesExtensions.FromString('FC'));
  Assert.AreEqual(TZUGFeRDSpecialServiceDescriptionCodes.FI_Financing,TZUGFeRDSpecialServiceDescriptionCodesExtensions.FromString('FI'));
  Assert.AreEqual(TZUGFeRDSpecialServiceDescriptionCodes.LA_Labelling,TZUGFeRDSpecialServiceDescriptionCodesExtensions.FromString('LA'));
  Assert.AreEqual(TZUGFeRDSpecialServiceDescriptionCodes.PC_Packing,TZUGFeRDSpecialServiceDescriptionCodesExtensions.FromString('PC'));
  Assert.AreEqual(TZUGFeRDSpecialServiceDescriptionCodes.Unknown, TZUGFeRDSpecialServiceDescriptionCodesExtensions.FromString('Unknown'));
  Assert.AreEqual(TZUGFeRDSpecialServiceDescriptionCodes.Unknown, TZUGFeRDSpecialServiceDescriptionCodesExtensions.FromString('Invalid'));
end;

procedure TZUGFeRDSpecialServiceDescriptionCodesTest.TestEnumToString;
begin
  Assert.AreEqual('AA', TZUGFeRDSpecialServiceDescriptionCodesExtensions.EnumToString(TZUGFeRDSpecialServiceDescriptionCodes.AA_Advertising));
  Assert.AreEqual('AAA', TZUGFeRDSpecialServiceDescriptionCodesExtensions.EnumToString(TZUGFeRDSpecialServiceDescriptionCodes.AAA_Telecommunication));
  Assert.AreEqual('ABK', TZUGFeRDSpecialServiceDescriptionCodesExtensions.EnumToString(TZUGFeRDSpecialServiceDescriptionCodes.ABK_Miscellaneous));
  Assert.AreEqual('ABL', TZUGFeRDSpecialServiceDescriptionCodesExtensions.EnumToString(TZUGFeRDSpecialServiceDescriptionCodes.ABL_AdditionalPackaging));
  Assert.AreEqual('ADR', TZUGFeRDSpecialServiceDescriptionCodesExtensions.EnumToString(TZUGFeRDSpecialServiceDescriptionCodes.ADR_OtherServices));
  Assert.AreEqual('ADT', TZUGFeRDSpecialServiceDescriptionCodesExtensions.EnumToString(TZUGFeRDSpecialServiceDescriptionCodes.ADT_Pickup));
  Assert.AreEqual('FC', TZUGFeRDSpecialServiceDescriptionCodesExtensions.EnumToString(TZUGFeRDSpecialServiceDescriptionCodes.FC_FreightService));
  Assert.AreEqual('FI', TZUGFeRDSpecialServiceDescriptionCodesExtensions.EnumToString(TZUGFeRDSpecialServiceDescriptionCodes.FI_Financing));
  Assert.AreEqual('LA', TZUGFeRDSpecialServiceDescriptionCodesExtensions.EnumToString(TZUGFeRDSpecialServiceDescriptionCodes.LA_Labelling));
  Assert.AreEqual('PC', TZUGFeRDSpecialServiceDescriptionCodesExtensions.EnumToString(TZUGFeRDSpecialServiceDescriptionCodes.PC_Packing));
  Assert.AreEqual('', TZUGFeRDSpecialServiceDescriptionCodesExtensions.EnumToString(TZUGFeRDSpecialServiceDescriptionCodes.Unknown));
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

//  TDUnitX.RegisterTestFixture(TZUGFeRDSpecialServiceDescriptionCodesTest);

end.
