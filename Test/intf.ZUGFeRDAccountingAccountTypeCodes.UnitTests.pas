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

unit intf.ZUGFeRDAccountingAccountTypeCodes.UnitTests;

interface

uses
  DUnitX.TestFramework, intf.ZUGFeRDAccountingAccountTypeCodes;

type
  [TestFixture]
  TZUGFeRDAccountingAccountTypeCodesTest = class
  public
    [Test]
    procedure TestFromString;
    [Test]
    procedure TestEnumToString;
  end;

implementation

procedure TZUGFeRDAccountingAccountTypeCodesTest.TestFromString;
begin
  Assert.AreEqual(TZUGFeRDAccountingAccountTypeCodes.Financial, TZUGFeRDAccountingAccountTypeCodesExtensions.FromString('1').GetValueOrDefault);
  Assert.AreEqual(TZUGFeRDAccountingAccountTypeCodes.Subsidiary, TZUGFeRDAccountingAccountTypeCodesExtensions.FromString('2').GetValueOrDefault);
  Assert.AreEqual(TZUGFeRDAccountingAccountTypeCodes.Budget, TZUGFeRDAccountingAccountTypeCodesExtensions.FromString('3').GetValueOrDefault);
  Assert.AreEqual(TZUGFeRDAccountingAccountTypeCodes.Cost_Accounting, TZUGFeRDAccountingAccountTypeCodesExtensions.FromString('4').GetValueOrDefault);
  Assert.AreEqual(TZUGFeRDAccountingAccountTypeCodes.Receivable, TZUGFeRDAccountingAccountTypeCodesExtensions.FromString('5').GetValueOrDefault);
  Assert.AreEqual(TZUGFeRDAccountingAccountTypeCodes.Payable, TZUGFeRDAccountingAccountTypeCodesExtensions.FromString('6').GetValueOrDefault);
  Assert.AreEqual(TZUGFeRDAccountingAccountTypeCodes.Job_Cost_Accounting, TZUGFeRDAccountingAccountTypeCodesExtensions.FromString('7').GetValueOrDefault);
end;

procedure TZUGFeRDAccountingAccountTypeCodesTest.TestEnumToString;
begin
  Assert.AreEqual('1', TZUGFeRDAccountingAccountTypeCodesExtensions.EnumToString(TZUGFeRDAccountingAccountTypeCodes.Financial));
  Assert.AreEqual('2', TZUGFeRDAccountingAccountTypeCodesExtensions.EnumToString(TZUGFeRDAccountingAccountTypeCodes.Subsidiary));
  Assert.AreEqual('3', TZUGFeRDAccountingAccountTypeCodesExtensions.EnumToString(TZUGFeRDAccountingAccountTypeCodes.Budget));
  Assert.AreEqual('4', TZUGFeRDAccountingAccountTypeCodesExtensions.EnumToString(TZUGFeRDAccountingAccountTypeCodes.Cost_Accounting));
  Assert.AreEqual('5', TZUGFeRDAccountingAccountTypeCodesExtensions.EnumToString(TZUGFeRDAccountingAccountTypeCodes.Receivable));
  Assert.AreEqual('6', TZUGFeRDAccountingAccountTypeCodesExtensions.EnumToString(TZUGFeRDAccountingAccountTypeCodes.Payable));
  Assert.AreEqual('7', TZUGFeRDAccountingAccountTypeCodesExtensions.EnumToString(TZUGFeRDAccountingAccountTypeCodes.Job_Cost_Accounting));
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

//  TDUnitX.RegisterTestFixture(TTZUGFeRDAccountingAccountTypeCodesTest);

end.
