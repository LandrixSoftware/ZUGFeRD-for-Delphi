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

unit intf.ZUGFeRDPaymentMeansTypeCodes.UnitTests;

interface

uses
  DUnitX.TestFramework, intf.ZUGFeRDPaymentMeansTypeCodes;

type
  [TestFixture]
  TZUGFeRDPaymentMeansTypeCodesTest = class
  public
    [Test]
    procedure TestFromString;
    //[Test]
    //procedure TestEnumValueToString;
    [Test]
    procedure TestEnumToString;
  end;

implementation

procedure TZUGFeRDPaymentMeansTypeCodesTest.TestFromString;
var
  Code: TZUGFeRDPaymentMeansTypeCodes;
begin
  Code := TZUGFeRDPaymentMeansTypeCodesExtensions.FromString('1');
  Assert.AreEqual(TZUGFeRDPaymentMeansTypeCodes.NotDefined, Code);
  Code := TZUGFeRDPaymentMeansTypeCodesExtensions.FromString('3');
  Assert.AreEqual(TZUGFeRDPaymentMeansTypeCodes.AutomatedClearingHouseDebit, Code);
  Code := TZUGFeRDPaymentMeansTypeCodesExtensions.FromString('10');
  Assert.AreEqual(TZUGFeRDPaymentMeansTypeCodes.InCash, Code);
  Code := TZUGFeRDPaymentMeansTypeCodesExtensions.FromString('20');
  Assert.AreEqual(TZUGFeRDPaymentMeansTypeCodes.Cheque, Code);
  Code := TZUGFeRDPaymentMeansTypeCodesExtensions.FromString('30');
  Assert.AreEqual(TZUGFeRDPaymentMeansTypeCodes.CreditTransfer, Code);
  Code := TZUGFeRDPaymentMeansTypeCodesExtensions.FromString('31');
  Assert.AreEqual(TZUGFeRDPaymentMeansTypeCodes.DebitTransfer, Code);
  Code := TZUGFeRDPaymentMeansTypeCodesExtensions.FromString('42');
  Assert.AreEqual(TZUGFeRDPaymentMeansTypeCodes.PaymentToBankAccount, Code);
  Code := TZUGFeRDPaymentMeansTypeCodesExtensions.FromString('48');
  Assert.AreEqual(TZUGFeRDPaymentMeansTypeCodes.BankCard, Code);
  Code := TZUGFeRDPaymentMeansTypeCodesExtensions.FromString('49');
  Assert.AreEqual(TZUGFeRDPaymentMeansTypeCodes.DirectDebit, Code);
  Code := TZUGFeRDPaymentMeansTypeCodesExtensions.FromString('57');
  Assert.AreEqual(TZUGFeRDPaymentMeansTypeCodes.StandingAgreement, Code);
  Code := TZUGFeRDPaymentMeansTypeCodesExtensions.FromString('58');
  Assert.AreEqual(TZUGFeRDPaymentMeansTypeCodes.SEPACreditTransfer, Code);
  Code := TZUGFeRDPaymentMeansTypeCodesExtensions.FromString('59');
  Assert.AreEqual(TZUGFeRDPaymentMeansTypeCodes.SEPADirectDebit, Code);
  Code := TZUGFeRDPaymentMeansTypeCodesExtensions.FromString('97');
  Assert.AreEqual(TZUGFeRDPaymentMeansTypeCodes.ClearingBetweenPartners, Code);
  Code := TZUGFeRDPaymentMeansTypeCodesExtensions.FromString('0');
  Assert.AreEqual(TZUGFeRDPaymentMeansTypeCodes.Unknown, Code);
  Code := TZUGFeRDPaymentMeansTypeCodesExtensions.FromString('Invalid');
  Assert.AreEqual(TZUGFeRDPaymentMeansTypeCodes.Unknown, Code);
end;

procedure TZUGFeRDPaymentMeansTypeCodesTest.TestEnumToString;
var
  Value: string;
begin
  Value := TZUGFeRDPaymentMeansTypeCodesExtensions.EnumToString(TZUGFeRDPaymentMeansTypeCodes.Unknown);
  Assert.AreEqual('0', Value);
  Value := TZUGFeRDPaymentMeansTypeCodesExtensions.EnumToString(TZUGFeRDPaymentMeansTypeCodes.NotDefined);
  Assert.AreEqual('1', Value);
  Value := TZUGFeRDPaymentMeansTypeCodesExtensions.EnumToString(TZUGFeRDPaymentMeansTypeCodes.AutomatedClearingHouseDebit);
  Assert.AreEqual('3', Value);
  Value := TZUGFeRDPaymentMeansTypeCodesExtensions.EnumToString(TZUGFeRDPaymentMeansTypeCodes.InCash);
  Assert.AreEqual('10', Value);
  Value := TZUGFeRDPaymentMeansTypeCodesExtensions.EnumToString(TZUGFeRDPaymentMeansTypeCodes.Cheque);
  Assert.AreEqual('20', Value);
  Value := TZUGFeRDPaymentMeansTypeCodesExtensions.EnumToString(TZUGFeRDPaymentMeansTypeCodes.CreditTransfer);
  Assert.AreEqual('30', Value);
  Value := TZUGFeRDPaymentMeansTypeCodesExtensions.EnumToString(TZUGFeRDPaymentMeansTypeCodes.DebitTransfer);
  Assert.AreEqual('31', Value);
  Value := TZUGFeRDPaymentMeansTypeCodesExtensions.EnumToString(TZUGFeRDPaymentMeansTypeCodes.PaymentToBankAccount);
  Assert.AreEqual('42', Value);
  Value := TZUGFeRDPaymentMeansTypeCodesExtensions.EnumToString(TZUGFeRDPaymentMeansTypeCodes.BankCard);
  Assert.AreEqual('48', Value);
  Value := TZUGFeRDPaymentMeansTypeCodesExtensions.EnumToString(TZUGFeRDPaymentMeansTypeCodes.DirectDebit);
  Assert.AreEqual('49', Value);
  Value := TZUGFeRDPaymentMeansTypeCodesExtensions.EnumToString(TZUGFeRDPaymentMeansTypeCodes.StandingAgreement);
  Assert.AreEqual('57', Value);
  Value := TZUGFeRDPaymentMeansTypeCodesExtensions.EnumToString(TZUGFeRDPaymentMeansTypeCodes.SEPACreditTransfer);
  Assert.AreEqual('58', Value);
  Value := TZUGFeRDPaymentMeansTypeCodesExtensions.EnumToString(TZUGFeRDPaymentMeansTypeCodes.SEPADirectDebit);
  Assert.AreEqual('59', Value);
  Value := TZUGFeRDPaymentMeansTypeCodesExtensions.EnumToString(TZUGFeRDPaymentMeansTypeCodes.ClearingBetweenPartners);
  Assert.AreEqual('97', Value);
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

//  TDUnitX.RegisterTestFixture(TZUGFeRDPaymentMeansTypeCodesTest);

end.
