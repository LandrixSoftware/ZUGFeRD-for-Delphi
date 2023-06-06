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
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_132_2, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-132-2'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1a, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-132-1a'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1b, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-132-1b'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1c, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-132-1c'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1d, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-132-1d'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1e, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-132-1e'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1f, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-132-1f'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1g, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-132-1g'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1h, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-132-1h'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1i, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-132-1i'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1j, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-132-1j'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1k, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-132-1k'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1l, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-132-1l'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1m, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-132-1m'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1n, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-132-1n'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1o, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-132-1o'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1p, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-132-1p'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1q, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-132-1q'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-143'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1a, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-143-1a'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1b, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-143-1b'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1c, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-143-1c'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1d, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-143-1d'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1e, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-143-1e'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1f, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-143-1f'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1fa, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-143-1fa'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1g, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-143-1g'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1h, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-143-1h'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1i, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-143-1i'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1j, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-143-1j'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1k, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-143-1k'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1l, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-143-1l'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_148, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-148'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_148_a, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-148-a'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_148_b, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-148-b'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_148_c, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-148-c'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_148_d, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-148-d'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_148_e, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-148-e'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_148_f, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-148-f'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_148_g, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-148-g'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_151, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-151'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_151_1a, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-151-1a'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_151_1aa, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-151-1aa'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_151_1b, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-151-1b'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_151_1c, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-151-1c'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_151_1d, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-151-1d'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_151_1e, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-151-1e'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_309, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-309'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_ae, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-ae'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_d, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-d'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_f, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-f'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_g, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-g'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_i, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-i'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_ic, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-ic'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_j, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-j'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_o, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('vatex-eu-o'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.Unknown, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('Unknown'));
  Assert.AreEqual(TZUGFeRDTaxExemptionReasonCodes.Unknown, TZUGFeRDTaxExemptionReasonCodesExtensions.FromString('Invalid'));
end;

procedure TZUGFeRDTaxExemptionReasonCodesTest.TestEnumToString;
begin
  Assert.AreEqual('vatex-132-2', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_132_2));
  Assert.AreEqual('vatex-eu-132-1a', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1a));
  Assert.AreEqual('vatex-eu-132-1b', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1b));
  Assert.AreEqual('vatex-eu-132-1c', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1c));
  Assert.AreEqual('vatex-eu-132-1d', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1d));
  Assert.AreEqual('vatex-eu-132-1e', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1e));
  Assert.AreEqual('vatex-eu-132-1f', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1f));
  Assert.AreEqual('vatex-eu-132-1g', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1g));
  Assert.AreEqual('vatex-eu-132-1h', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1h));
  Assert.AreEqual('vatex-eu-132-1i', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1i));
  Assert.AreEqual('vatex-eu-132-1j', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1j));
  Assert.AreEqual('vatex-eu-132-1k', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1k));
  Assert.AreEqual('vatex-eu-132-1l', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1l));
  Assert.AreEqual('vatex-eu-132-1m', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1m));
  Assert.AreEqual('vatex-eu-132-1n', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1n));
  Assert.AreEqual('vatex-eu-132-1o', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1o));
  Assert.AreEqual('vatex-eu-132-1p', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1p));
  Assert.AreEqual('vatex-eu-132-1q', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_132_1q));
  Assert.AreEqual('vatex-eu-143', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143));
  Assert.AreEqual('vatex-eu-143-1a', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1a));
  Assert.AreEqual('vatex-eu-143-1b', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1b));
  Assert.AreEqual('vatex-eu-143-1c', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1c));
  Assert.AreEqual('vatex-eu-143-1d', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1d));
  Assert.AreEqual('vatex-eu-143-1e', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1e));
  Assert.AreEqual('vatex-eu-143-1f', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1f));
  Assert.AreEqual('vatex-eu-143-1fa', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1fa));
  Assert.AreEqual('vatex-eu-143-1g', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1g));
  Assert.AreEqual('vatex-eu-143-1h', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1h));
  Assert.AreEqual('vatex-eu-143-1i', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1i));
  Assert.AreEqual('vatex-eu-143-1j', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1j));
  Assert.AreEqual('vatex-eu-143-1k', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1k));
  Assert.AreEqual('vatex-eu-143-1l', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_143_1l));
  Assert.AreEqual('vatex-eu-148', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_148));
  Assert.AreEqual('vatex-eu-148-a', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_148_a));
  Assert.AreEqual('vatex-eu-148-b', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_148_b));
  Assert.AreEqual('vatex-eu-148-c', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_148_c));
  Assert.AreEqual('vatex-eu-148-d', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_148_d));
  Assert.AreEqual('vatex-eu-148-e', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_148_e));
  Assert.AreEqual('vatex-eu-148-f', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_148_f));
  Assert.AreEqual('vatex-eu-148-g', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_148_g));
  Assert.AreEqual('vatex-eu-151', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_151));
  Assert.AreEqual('vatex-eu-151-1a', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_151_1a));
  Assert.AreEqual('vatex-eu-151-1aa', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_151_1aa));
  Assert.AreEqual('vatex-eu-151-1b', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_151_1b));
  Assert.AreEqual('vatex-eu-151-1c', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_151_1c));
  Assert.AreEqual('vatex-eu-151-1d', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_151_1d));
  Assert.AreEqual('vatex-eu-151-1e', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_151_1e));
  Assert.AreEqual('vatex-eu-309', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_309));
  Assert.AreEqual('vatex-eu-ae', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_ae));
  Assert.AreEqual('vatex-eu-d', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_d));
  Assert.AreEqual('vatex-eu-f', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_f));
  Assert.AreEqual('vatex-eu-g', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_g));
  Assert.AreEqual('vatex-eu-i', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_i));
  Assert.AreEqual('vatex-eu-ic', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_ic));
  Assert.AreEqual('vatex-eu-j', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_j));
  Assert.AreEqual('vatex-eu-o', TZUGFeRDTaxExemptionReasonCodesExtensions.EnumToString(TZUGFeRDTaxExemptionReasonCodes.vatex_eu_o));
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
