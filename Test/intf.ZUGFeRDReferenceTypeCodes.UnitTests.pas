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

unit intf.ZUGFeRDReferenceTypeCodes.UnitTests;

interface

uses
  DUnitX.TestFramework, intf.ZUGFeRDReferenceTypeCodes;

type
  [TestFixture]
  TZUGFeRDReferenceTypeCodesTest = class
  public
    [Test]
    procedure TestFromString;
    [Test]
    procedure TestEnumToString;
  end;

implementation

procedure TZUGFeRDReferenceTypeCodesTest.TestFromString;
begin
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.AAA, TZUGFeRDReferenceTypeCodesExtensions.FromString('AAA').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.AAB, TZUGFeRDReferenceTypeCodesExtensions.FromString('AAB').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.AAG, TZUGFeRDReferenceTypeCodesExtensions.FromString('AAG').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.AAJ, TZUGFeRDReferenceTypeCodesExtensions.FromString('AAJ').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.AAL, TZUGFeRDReferenceTypeCodesExtensions.FromString('AAL').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.AAM, TZUGFeRDReferenceTypeCodesExtensions.FromString('AAM').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.AAS, TZUGFeRDReferenceTypeCodesExtensions.FromString('AAS').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.ABT, TZUGFeRDReferenceTypeCodesExtensions.FromString('ABT').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.AER, TZUGFeRDReferenceTypeCodesExtensions.FromString('AER').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.AGG, TZUGFeRDReferenceTypeCodesExtensions.FromString('AGG').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.AJS, TZUGFeRDReferenceTypeCodesExtensions.FromString('AJS').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.ALQ, TZUGFeRDReferenceTypeCodesExtensions.FromString('ALQ').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.ALO, TZUGFeRDReferenceTypeCodesExtensions.FromString('ALO').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.API, TZUGFeRDReferenceTypeCodesExtensions.FromString('API').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.ASI, TZUGFeRDReferenceTypeCodesExtensions.FromString('ASI').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.AUD, TZUGFeRDReferenceTypeCodesExtensions.FromString('AUD').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.AUT, TZUGFeRDReferenceTypeCodesExtensions.FromString('AUT').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.AWR, TZUGFeRDReferenceTypeCodesExtensions.FromString('AWR').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.BO , TZUGFeRDReferenceTypeCodesExtensions.FromString('BO').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.BC , TZUGFeRDReferenceTypeCodesExtensions.FromString('BC').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.CD , TZUGFeRDReferenceTypeCodesExtensions.FromString('CD').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.DL , TZUGFeRDReferenceTypeCodesExtensions.FromString('DL').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.IV , TZUGFeRDReferenceTypeCodesExtensions.FromString('IV').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.MG , TZUGFeRDReferenceTypeCodesExtensions.FromString('MG').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.OI , TZUGFeRDReferenceTypeCodesExtensions.FromString('OI').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.PL , TZUGFeRDReferenceTypeCodesExtensions.FromString('PL').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.PK , TZUGFeRDReferenceTypeCodesExtensions.FromString('PK').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.POR, TZUGFeRDReferenceTypeCodesExtensions.FromString('POR').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.PP , TZUGFeRDReferenceTypeCodesExtensions.FromString('PP').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.TIN, TZUGFeRDReferenceTypeCodesExtensions.FromString('TIN').Value);
  Assert.AreEqual(TZUGFeRDReferenceTypeCodes.VN , TZUGFeRDReferenceTypeCodesExtensions.FromString('VN').Value);
end;

procedure TZUGFeRDReferenceTypeCodesTest.TestEnumToString;
begin
  Assert.AreEqual('AAA', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.AAA));
  Assert.AreEqual('AAB', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.AAB));
  Assert.AreEqual('AAG', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.AAG));
  Assert.AreEqual('AAJ', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.AAJ));
  Assert.AreEqual('AAL', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.AAL));
  Assert.AreEqual('AAM', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.AAM));
  Assert.AreEqual('AAS', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.AAS));
  Assert.AreEqual('ABT', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.ABT));
  Assert.AreEqual('AER', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.AER));
  Assert.AreEqual('AGG', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.AGG));
  Assert.AreEqual('AJS', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.AJS));
  Assert.AreEqual('ALQ', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.ALQ));
  Assert.AreEqual('ALO', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.ALO));
  Assert.AreEqual('API', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.API));
  Assert.AreEqual('ASI', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.ASI));
  Assert.AreEqual('AUD', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.AUD));
  Assert.AreEqual('AUT', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.AUT));
  Assert.AreEqual('AWR', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.AWR));
  Assert.AreEqual('BO', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.BO ));
  Assert.AreEqual('BC', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.BC ));
  Assert.AreEqual('CD', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.CD ));
  Assert.AreEqual('DL', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.DL ));
  Assert.AreEqual('IV', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.IV ));
  Assert.AreEqual('MG', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.MG ));
  Assert.AreEqual('OI', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.OI ));
  Assert.AreEqual('PL', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.PL ));
  Assert.AreEqual('PK', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.PK ));
  Assert.AreEqual('POR', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.POR));
  Assert.AreEqual('PP', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.PP ));
  Assert.AreEqual('TIN', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.TIN));
  Assert.AreEqual('VN', TZUGFeRDReferenceTypeCodesExtensions.EnumToString(TZUGFeRDReferenceTypeCodes.VN ));
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

//  TDUnitX.RegisterTestFixture(TZUGFeRDReferenceTypeCodesTest);

end.
