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

unit intf.ZUGFeRDAllowanceOrChargeIdentificationCodes.UnitTests;

interface

uses
  DUnitX.TestFramework,
  intf.ZUGFeRDAllowanceOrChargeIdentificationCodes;

type
  [TestFixture]
  TZUGFeRDAllowanceOrChargeIdentificationCodesTest = class
  public
    [Test]
    procedure TestFromString;
    [Test]
    procedure TestEnumToString;
  end;

implementation

procedure TZUGFeRDAllowanceOrChargeIdentificationCodesTest.TestFromString;
begin
  Assert.AreEqual(TZUGFeRDAllowanceOrChargeIdentificationCodes.BonusForWorksAheadOfSchedule, TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString('41'));
  Assert.AreEqual(TZUGFeRDAllowanceOrChargeIdentificationCodes.OtherBonus, TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString('42'));
  Assert.AreEqual(TZUGFeRDAllowanceOrChargeIdentificationCodes.ManufacturersConsumerDiscount, TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString('60'));
  Assert.AreEqual(TZUGFeRDAllowanceOrChargeIdentificationCodes.DueToMilitaryStatus, TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString('62'));
  Assert.AreEqual(TZUGFeRDAllowanceOrChargeIdentificationCodes.DueToWorkAccident, TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString('63'));
  Assert.AreEqual(TZUGFeRDAllowanceOrChargeIdentificationCodes.SpecialAgreement, TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString('64'));
  Assert.AreEqual(TZUGFeRDAllowanceOrChargeIdentificationCodes.ProductionErrorDiscount, TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString('65'));
  Assert.AreEqual(TZUGFeRDAllowanceOrChargeIdentificationCodes.NewOutletDiscount, TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString('66'));
  Assert.AreEqual(TZUGFeRDAllowanceOrChargeIdentificationCodes.SampleDiscount, TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString('67'));
  Assert.AreEqual(TZUGFeRDAllowanceOrChargeIdentificationCodes.EndOfRangeDiscount, TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString('68'));
  Assert.AreEqual(TZUGFeRDAllowanceOrChargeIdentificationCodes.IncotermDiscount, TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString('70'));
  Assert.AreEqual(TZUGFeRDAllowanceOrChargeIdentificationCodes.PointOfSalesThresholdAllowance, TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString('71'));
  Assert.AreEqual(TZUGFeRDAllowanceOrChargeIdentificationCodes.MaterialSurchargeDeduction, TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString('88'));
  Assert.AreEqual(TZUGFeRDAllowanceOrChargeIdentificationCodes.Discount, TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString('95'));
  Assert.AreEqual(TZUGFeRDAllowanceOrChargeIdentificationCodes.SpecialRebate, TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString('100'));
  Assert.AreEqual(TZUGFeRDAllowanceOrChargeIdentificationCodes.FixedLongTerm, TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString('102'));
  Assert.AreEqual(TZUGFeRDAllowanceOrChargeIdentificationCodes.Temporary, TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString('103'));
  Assert.AreEqual(TZUGFeRDAllowanceOrChargeIdentificationCodes.Standard, TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString('104'));
  Assert.AreEqual(TZUGFeRDAllowanceOrChargeIdentificationCodes.YearlyTurnover, TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString('105'));
  Assert.AreEqual(TZUGFeRDAllowanceOrChargeIdentificationCodes.Unknown, TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString('Unknown'));
  Assert.AreEqual(TZUGFeRDAllowanceOrChargeIdentificationCodes.Unknown, TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString('Invalid'));
end;

procedure TZUGFeRDAllowanceOrChargeIdentificationCodesTest.TestEnumToString;
begin
  Assert.AreEqual('41', TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(TZUGFeRDAllowanceOrChargeIdentificationCodes.BonusForWorksAheadOfSchedule));
  Assert.AreEqual('42', TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(TZUGFeRDAllowanceOrChargeIdentificationCodes.OtherBonus));
  Assert.AreEqual('60', TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(TZUGFeRDAllowanceOrChargeIdentificationCodes.ManufacturersConsumerDiscount));
  Assert.AreEqual('62', TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(TZUGFeRDAllowanceOrChargeIdentificationCodes.DueToMilitaryStatus));
  Assert.AreEqual('63', TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(TZUGFeRDAllowanceOrChargeIdentificationCodes.DueToWorkAccident));
  Assert.AreEqual('64', TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(TZUGFeRDAllowanceOrChargeIdentificationCodes.SpecialAgreement));
  Assert.AreEqual('65', TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(TZUGFeRDAllowanceOrChargeIdentificationCodes.ProductionErrorDiscount));
  Assert.AreEqual('66', TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(TZUGFeRDAllowanceOrChargeIdentificationCodes.NewOutletDiscount));
  Assert.AreEqual('67', TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(TZUGFeRDAllowanceOrChargeIdentificationCodes.SampleDiscount));
  Assert.AreEqual('68', TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(TZUGFeRDAllowanceOrChargeIdentificationCodes.EndOfRangeDiscount));
  Assert.AreEqual('70', TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(TZUGFeRDAllowanceOrChargeIdentificationCodes.IncotermDiscount));
  Assert.AreEqual('71', TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(TZUGFeRDAllowanceOrChargeIdentificationCodes.PointOfSalesThresholdAllowance));
  Assert.AreEqual('88', TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(TZUGFeRDAllowanceOrChargeIdentificationCodes.MaterialSurchargeDeduction));
  Assert.AreEqual('95', TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(TZUGFeRDAllowanceOrChargeIdentificationCodes.Discount));
  Assert.AreEqual('100', TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(TZUGFeRDAllowanceOrChargeIdentificationCodes.SpecialRebate));
  Assert.AreEqual('102', TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(TZUGFeRDAllowanceOrChargeIdentificationCodes.FixedLongTerm));
  Assert.AreEqual('103', TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(TZUGFeRDAllowanceOrChargeIdentificationCodes.Temporary));
  Assert.AreEqual('104', TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(TZUGFeRDAllowanceOrChargeIdentificationCodes.Standard));
  Assert.AreEqual('105', TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(TZUGFeRDAllowanceOrChargeIdentificationCodes.YearlyTurnover));
  Assert.AreEqual('', TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(TZUGFeRDAllowanceOrChargeIdentificationCodes.Unknown));
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

//  TDUnitX.RegisterTestFixture(TZUGFeRDAllowanceOrChargeIdentificationCodesTest);

end.
