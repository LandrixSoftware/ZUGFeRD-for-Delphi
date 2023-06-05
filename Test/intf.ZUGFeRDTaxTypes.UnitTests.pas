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

unit intf.ZUGFeRDTaxTypes.UnitTests;

interface

uses
  DUnitX.TestFramework, intf.ZUGFeRDTaxTypes;

type
  [TestFixture]
  TZUGFeRDTaxTypesTest = class
  public
    [Test]
    procedure TestFromString;
    [Test]
    procedure TestEnumToString;
  end;

implementation

procedure TZUGFeRDTaxTypesTest.TestFromString;
begin
  Assert.AreEqual(TZUGFeRDTaxTypes.AAA, TZUGFeRDTaxTypesExtensions.FromString('AAA'));
  Assert.AreEqual(TZUGFeRDTaxTypes.AAB, TZUGFeRDTaxTypesExtensions.FromString('AAB'));
  Assert.AreEqual(TZUGFeRDTaxTypes.AAC, TZUGFeRDTaxTypesExtensions.FromString('AAC'));
  Assert.AreEqual(TZUGFeRDTaxTypes.AAD, TZUGFeRDTaxTypesExtensions.FromString('AAD'));
  Assert.AreEqual(TZUGFeRDTaxTypes.AAE, TZUGFeRDTaxTypesExtensions.FromString('AAE'));
  Assert.AreEqual(TZUGFeRDTaxTypes.AAF, TZUGFeRDTaxTypesExtensions.FromString('AAF'));
  Assert.AreEqual(TZUGFeRDTaxTypes.AAG, TZUGFeRDTaxTypesExtensions.FromString('AAG'));
  Assert.AreEqual(TZUGFeRDTaxTypes.AAH, TZUGFeRDTaxTypesExtensions.FromString('AAH'));
  Assert.AreEqual(TZUGFeRDTaxTypes.AAI, TZUGFeRDTaxTypesExtensions.FromString('AAI'));
  Assert.AreEqual(TZUGFeRDTaxTypes.AAJ, TZUGFeRDTaxTypesExtensions.FromString('AAJ'));
  Assert.AreEqual(TZUGFeRDTaxTypes.AAK, TZUGFeRDTaxTypesExtensions.FromString('AAK'));
  Assert.AreEqual(TZUGFeRDTaxTypes.AAL, TZUGFeRDTaxTypesExtensions.FromString('AAL'));
  Assert.AreEqual(TZUGFeRDTaxTypes.AAM, TZUGFeRDTaxTypesExtensions.FromString('AAM'));
  Assert.AreEqual(TZUGFeRDTaxTypes.ADD, TZUGFeRDTaxTypesExtensions.FromString('ADD'));
  Assert.AreEqual(TZUGFeRDTaxTypes.BOL, TZUGFeRDTaxTypesExtensions.FromString('BOL'));
  Assert.AreEqual(TZUGFeRDTaxTypes.CAP, TZUGFeRDTaxTypesExtensions.FromString('CAP'));
  Assert.AreEqual(TZUGFeRDTaxTypes.CAR, TZUGFeRDTaxTypesExtensions.FromString('CAR'));
  Assert.AreEqual(TZUGFeRDTaxTypes.COC, TZUGFeRDTaxTypesExtensions.FromString('COC'));
  Assert.AreEqual(TZUGFeRDTaxTypes.CST, TZUGFeRDTaxTypesExtensions.FromString('CST'));
  Assert.AreEqual(TZUGFeRDTaxTypes.CUD, TZUGFeRDTaxTypesExtensions.FromString('CUD'));
  Assert.AreEqual(TZUGFeRDTaxTypes.CVD, TZUGFeRDTaxTypesExtensions.FromString('CVD'));
  Assert.AreEqual(TZUGFeRDTaxTypes.ENV, TZUGFeRDTaxTypesExtensions.FromString('ENV'));
  Assert.AreEqual(TZUGFeRDTaxTypes.EXC, TZUGFeRDTaxTypesExtensions.FromString('EXC'));
  Assert.AreEqual(TZUGFeRDTaxTypes.EXP, TZUGFeRDTaxTypesExtensions.FromString('EXP'));
  Assert.AreEqual(TZUGFeRDTaxTypes.FET, TZUGFeRDTaxTypesExtensions.FromString('FET'));
  Assert.AreEqual(TZUGFeRDTaxTypes.FRE, TZUGFeRDTaxTypesExtensions.FromString('FRE'));
  Assert.AreEqual(TZUGFeRDTaxTypes.GCN, TZUGFeRDTaxTypesExtensions.FromString('GCN'));
  Assert.AreEqual(TZUGFeRDTaxTypes.GST, TZUGFeRDTaxTypesExtensions.FromString('GST'));
  Assert.AreEqual(TZUGFeRDTaxTypes.ILL, TZUGFeRDTaxTypesExtensions.FromString('ILL'));
  Assert.AreEqual(TZUGFeRDTaxTypes.IMP, TZUGFeRDTaxTypesExtensions.FromString('IMP'));
  Assert.AreEqual(TZUGFeRDTaxTypes.IND, TZUGFeRDTaxTypesExtensions.FromString('IND'));
  Assert.AreEqual(TZUGFeRDTaxTypes.LAC, TZUGFeRDTaxTypesExtensions.FromString('LAC'));
  Assert.AreEqual(TZUGFeRDTaxTypes.LCN, TZUGFeRDTaxTypesExtensions.FromString('LCN'));
  Assert.AreEqual(TZUGFeRDTaxTypes.LDP, TZUGFeRDTaxTypesExtensions.FromString('LDP'));
  Assert.AreEqual(TZUGFeRDTaxTypes.LOC, TZUGFeRDTaxTypesExtensions.FromString('LOC'));
  Assert.AreEqual(TZUGFeRDTaxTypes.LST, TZUGFeRDTaxTypesExtensions.FromString('LST'));
  Assert.AreEqual(TZUGFeRDTaxTypes.MCA, TZUGFeRDTaxTypesExtensions.FromString('MCA'));
  Assert.AreEqual(TZUGFeRDTaxTypes.MCD, TZUGFeRDTaxTypesExtensions.FromString('MCD'));
  Assert.AreEqual(TZUGFeRDTaxTypes.OTH, TZUGFeRDTaxTypesExtensions.FromString('OTH'));
  Assert.AreEqual(TZUGFeRDTaxTypes.PDB, TZUGFeRDTaxTypesExtensions.FromString('PDB'));
  Assert.AreEqual(TZUGFeRDTaxTypes.PDC, TZUGFeRDTaxTypesExtensions.FromString('PDC'));
  Assert.AreEqual(TZUGFeRDTaxTypes.PRF, TZUGFeRDTaxTypesExtensions.FromString('PRF'));
  Assert.AreEqual(TZUGFeRDTaxTypes.SCN, TZUGFeRDTaxTypesExtensions.FromString('SCN'));
  Assert.AreEqual(TZUGFeRDTaxTypes.SSS, TZUGFeRDTaxTypesExtensions.FromString('SSS'));
  Assert.AreEqual(TZUGFeRDTaxTypes.SIT, TZUGFeRDTaxTypesExtensions.FromString('SIT'));
  Assert.AreEqual(TZUGFeRDTaxTypes.SUP, TZUGFeRDTaxTypesExtensions.FromString('SUP'));
  Assert.AreEqual(TZUGFeRDTaxTypes.SUR, TZUGFeRDTaxTypesExtensions.FromString('SUR'));
  Assert.AreEqual(TZUGFeRDTaxTypes.SWT, TZUGFeRDTaxTypesExtensions.FromString('SWT'));
  Assert.AreEqual(TZUGFeRDTaxTypes.TAC, TZUGFeRDTaxTypesExtensions.FromString('TAC'));
  Assert.AreEqual(TZUGFeRDTaxTypes.TOT, TZUGFeRDTaxTypesExtensions.FromString('TOT'));
  Assert.AreEqual(TZUGFeRDTaxTypes.TOX, TZUGFeRDTaxTypesExtensions.FromString('TOX'));
  Assert.AreEqual(TZUGFeRDTaxTypes.TTA, TZUGFeRDTaxTypesExtensions.FromString('TTA'));
  Assert.AreEqual(TZUGFeRDTaxTypes.VAD, TZUGFeRDTaxTypesExtensions.FromString('VAD'));
  Assert.AreEqual(TZUGFeRDTaxTypes.VAT, TZUGFeRDTaxTypesExtensions.FromString('VAT'));
  Assert.AreEqual(TZUGFeRDTaxTypes.Unknown, TZUGFeRDTaxTypesExtensions.FromString('Unknown'));
  Assert.AreEqual(TZUGFeRDTaxTypes.Unknown, TZUGFeRDTaxTypesExtensions.FromString('Invalid'));
end;

procedure TZUGFeRDTaxTypesTest.TestEnumToString;
begin
  Assert.AreEqual('AAA', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.AAA));
  Assert.AreEqual('AAB', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.AAB));
  Assert.AreEqual('AAC', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.AAC));
  Assert.AreEqual('AAD', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.AAD));
  Assert.AreEqual('AAE', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.AAE));
  Assert.AreEqual('AAF', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.AAF));
  Assert.AreEqual('AAG', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.AAG));
  Assert.AreEqual('AAH', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.AAH));
  Assert.AreEqual('AAI', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.AAI));
  Assert.AreEqual('AAJ', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.AAJ));
  Assert.AreEqual('AAK', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.AAK));
  Assert.AreEqual('AAL', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.AAL));
  Assert.AreEqual('AAM', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.AAM));
  Assert.AreEqual('ADD', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.ADD));
  Assert.AreEqual('BOL', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.BOL));
  Assert.AreEqual('CAP', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.CAP));
  Assert.AreEqual('CAR', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.CAR));
  Assert.AreEqual('COC', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.COC));
  Assert.AreEqual('CST', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.CST));
  Assert.AreEqual('CUD', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.CUD));
  Assert.AreEqual('CVD', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.CVD));
  Assert.AreEqual('ENV', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.ENV));
  Assert.AreEqual('EXC', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.EXC));
  Assert.AreEqual('EXP', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.EXP));
  Assert.AreEqual('FET', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.FET));
  Assert.AreEqual('FRE', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.FRE));
  Assert.AreEqual('GCN', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.GCN));
  Assert.AreEqual('GST', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.GST));
  Assert.AreEqual('ILL', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.ILL));
  Assert.AreEqual('IMP', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.IMP));
  Assert.AreEqual('IND', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.IND));
  Assert.AreEqual('LAC', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.LAC));
  Assert.AreEqual('LCN', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.LCN));
  Assert.AreEqual('LDP', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.LDP));
  Assert.AreEqual('LOC', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.LOC));
  Assert.AreEqual('LST', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.LST));
  Assert.AreEqual('MCA', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.MCA));
  Assert.AreEqual('MCD', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.MCD));
  Assert.AreEqual('OTH', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.OTH));
  Assert.AreEqual('PDB', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.PDB));
  Assert.AreEqual('PDC', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.PDC));
  Assert.AreEqual('PRF', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.PRF));
  Assert.AreEqual('SCN', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.SCN));
  Assert.AreEqual('SSS', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.SSS));
  Assert.AreEqual('SIT', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.SIT));
  Assert.AreEqual('SUP', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.SUP));
  Assert.AreEqual('SUR', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.SUR));
  Assert.AreEqual('SWT', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.SWT));
  Assert.AreEqual('TAC', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.TAC));
  Assert.AreEqual('TOT', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.TOT));
  Assert.AreEqual('TOX', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.TOX));
  Assert.AreEqual('TTA', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.TTA));
  Assert.AreEqual('VAD', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.VAD));
  Assert.AreEqual('VAT', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.VAT));
  Assert.AreEqual('Unknown', TZUGFeRDTaxTypesExtensions.EnumToString(TZUGFeRDTaxTypes.Unknown));
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

//  TDUnitX.RegisterTestFixture(TZUGFeRDTaxTypesTest);

end.
