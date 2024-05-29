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

unit intf.ZUGFeRDTaxExemptionReasonCodes.UnitTests;

interface

uses
  DUnitX.TestFramework, intf.ZUGFeRDTaxExemptionReasonCodes;

type
  [TestFixture]
  TZUGFeRDTaxExemptionReasonCodesTest = class
  public
    [Test]
    procedure TestFromString;
    [Test]
    procedure TestEnumToString;
  end;

implementation

procedure TZUGFeRDTaxExemptionReasonCodesTest.TestFromString;
begin
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_132_2, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-132-2'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1A, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-132-1A'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1B, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-132-1B'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1C, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-132-1C'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1D, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-132-1D'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1E, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-132-1E'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1F, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-132-1F'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1G, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-132-1G'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1H, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-132-1H'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1I, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-132-1I'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1J, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-132-1J'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1K, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-132-1K'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1L, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-132-1L'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1M, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-132-1M'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1N, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-132-1N'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1O, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-132-1O'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1P, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-132-1P'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1Q, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-132-1Q'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-143'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1A, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-143-1A'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1B, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-143-1B'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1C, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-143-1C'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1D, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-143-1D'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1E, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-143-1E'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1F, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-143-1F'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1FA, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-143-1FA'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1G, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-143-1G'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1H, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-143-1H'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1I, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-143-1I'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1J, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-143-1J'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1K, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-143-1K'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1L, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-143-1L'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_148, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-148'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_148_A, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-148-A'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_148_B, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-148-B'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_148_C, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-148-C'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_148_D, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-148-D'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_148_E, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-148-E'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_148_F, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-148-F'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_148_G, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-148-G'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_151, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-151'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_151_1A, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-151-1A'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_151_1AA, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-151-1AA'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_151_1B, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-151-1B'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_151_1C, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-151-1C'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_151_1D, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-151-1D'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_151_1E, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-151-1E'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_309, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-309'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_AE, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-AE'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_D, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-D'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_F, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-F'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_G, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-G'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_I, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-I'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_IC, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-IC'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_J, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-J'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_O, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('VATEX-EU-O'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.Unknown, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('Unknown'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.Unknown, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('Invalid'));
end;

procedure TZUGFeRDTaxExemptionReasonCodesTest.TestEnumToString;
begin
  Assert.AreEqual('VATEX-132-2', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_132_2));
  Assert.AreEqual('VATEX-EU-132-1A', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1A));
  Assert.AreEqual('VATEX-EU-132-1B', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1B));
  Assert.AreEqual('VATEX-EU-132-1C', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1C));
  Assert.AreEqual('VATEX-EU-132-1D', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1D));
  Assert.AreEqual('VATEX-EU-132-1E', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1E));
  Assert.AreEqual('VATEX-EU-132-1F', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1F));
  Assert.AreEqual('VATEX-EU-132-1G', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1G));
  Assert.AreEqual('VATEX-EU-132-1H', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1H));
  Assert.AreEqual('VATEX-EU-132-1I', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1I));
  Assert.AreEqual('VATEX-EU-132-1J', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1J));
  Assert.AreEqual('VATEX-EU-132-1K', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1K));
  Assert.AreEqual('VATEX-EU-132-1L', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1L));
  Assert.AreEqual('VATEX-EU-132-1M', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1M));
  Assert.AreEqual('VATEX-EU-132-1N', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1N));
  Assert.AreEqual('VATEX-EU-132-1O', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1O));
  Assert.AreEqual('VATEX-EU-132-1P', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1P));
  Assert.AreEqual('VATEX-EU-132-1Q', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_132_1Q));
  Assert.AreEqual('VATEX-EU-143', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143));
  Assert.AreEqual('VATEX-EU-143-1A', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1A));
  Assert.AreEqual('VATEX-EU-143-1B', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1B));
  Assert.AreEqual('VATEX-EU-143-1C', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1C));
  Assert.AreEqual('VATEX-EU-143-1D', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1D));
  Assert.AreEqual('VATEX-EU-143-1E', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1E));
  Assert.AreEqual('VATEX-EU-143-1F', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1F));
  Assert.AreEqual('VATEX-EU-143-1FA', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1FA));
  Assert.AreEqual('VATEX-EU-143-1G', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1G));
  Assert.AreEqual('VATEX-EU-143-1H', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1H));
  Assert.AreEqual('VATEX-EU-143-1I', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1I));
  Assert.AreEqual('VATEX-EU-143-1J', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1J));
  Assert.AreEqual('VATEX-EU-143-1K', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1K));
  Assert.AreEqual('VATEX-EU-143-1L', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_143_1L));
  Assert.AreEqual('VATEX-EU-148', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_148));
  Assert.AreEqual('VATEX-EU-148-A', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_148_A));
  Assert.AreEqual('VATEX-EU-148-B', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_148_B));
  Assert.AreEqual('VATEX-EU-148-C', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_148_C));
  Assert.AreEqual('VATEX-EU-148-D', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_148_D));
  Assert.AreEqual('VATEX-EU-148-E', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_148_E));
  Assert.AreEqual('VATEX-EU-148-F', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_148_F));
  Assert.AreEqual('VATEX-EU-148-G', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_148_G));
  Assert.AreEqual('VATEX-EU-151', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_151));
  Assert.AreEqual('VATEX-EU-151-1A', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_151_1A));
  Assert.AreEqual('VATEX-EU-151-1AA', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_151_1AA));
  Assert.AreEqual('VATEX-EU-151-1B', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_151_1B));
  Assert.AreEqual('VATEX-EU-151-1C', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_151_1C));
  Assert.AreEqual('VATEX-EU-151-1D', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_151_1D));
  Assert.AreEqual('VATEX-EU-151-1E', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_151_1E));
  Assert.AreEqual('VATEX-EU-309', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_309));
  Assert.AreEqual('VATEX-EU-AE', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_AE));
  Assert.AreEqual('VATEX-EU-D', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_D));
  Assert.AreEqual('VATEX-EU-F', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_F));
  Assert.AreEqual('VATEX-EU-G', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_G));
  Assert.AreEqual('VATEX-EU-I', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_I));
  Assert.AreEqual('VATEX-EU-IC', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_IC));
  Assert.AreEqual('VATEX-EU-J', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_J));
  Assert.AreEqual('VATEX-EU-O', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.VATEX_EU_O));
  Assert.AreEqual('Unknown', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.Unknown));
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

//  TDUnitX.RegisterTestFixture(TZUGFeRDTaxExemptionReasonCodesTest);

end.
