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

unit intf.ZUGFeRDInvoiceTypes.UnitTests;

interface

uses
  DUnitX.TestFramework, intf.ZUGFeRDInvoiceTypes;

type
  [TestFixture]
  TZUGFeRDInvoiceTypeTest = class
  public
    [Test]
    procedure TestFromString;
    [Test]
    procedure TestEnumToString;
  end;

implementation

procedure TZUGFeRDInvoiceTypeTest.TestFromString;
begin
  Assert.AreEqual(TZUGFeRDInvoiceType.DebitnoteRelatedToFinancialAdjustments, TZUGFeRDInvoiceTypeExtensions.FromString('84'));
  Assert.AreEqual(TZUGFeRDInvoiceType.SelfBilledCreditNote, TZUGFeRDInvoiceTypeExtensions.FromString('261'));
  Assert.AreEqual(TZUGFeRDInvoiceType.PartialInvoice, TZUGFeRDInvoiceTypeExtensions.FromString('326'));
  Assert.AreEqual(TZUGFeRDInvoiceType.Invoice, TZUGFeRDInvoiceTypeExtensions.FromString('380'));
  Assert.AreEqual(TZUGFeRDInvoiceType.CreditNote, TZUGFeRDInvoiceTypeExtensions.FromString('381'));
  Assert.AreEqual(TZUGFeRDInvoiceType.DebitNote, TZUGFeRDInvoiceTypeExtensions.FromString('383'));
  Assert.AreEqual(TZUGFeRDInvoiceType.Correction, TZUGFeRDInvoiceTypeExtensions.FromString('384'));
  Assert.AreEqual(TZUGFeRDInvoiceType.PrepaymentInvoice, TZUGFeRDInvoiceTypeExtensions.FromString('386'));
  Assert.AreEqual(TZUGFeRDInvoiceType.SelfBilledInvoice, TZUGFeRDInvoiceTypeExtensions.FromString('389'));
  Assert.AreEqual(TZUGFeRDInvoiceType.InvoiceInformation, TZUGFeRDInvoiceTypeExtensions.FromString('751'));
  Assert.AreEqual(TZUGFeRDInvoiceType.CorrectionOld, TZUGFeRDInvoiceTypeExtensions.FromString('1380'));
  Assert.AreEqual(TZUGFeRDInvoiceType.Cancellation, TZUGFeRDInvoiceTypeExtensions.FromString('457'));
  Assert.AreEqual(TZUGFeRDInvoiceType.Unknown, TZUGFeRDInvoiceTypeExtensions.FromString('Invalid'));
end;

procedure TZUGFeRDInvoiceTypeTest.TestEnumToString;
begin
  Assert.AreEqual('84', TZUGFeRDInvoiceTypeExtensions.EnumToString(TZUGFeRDInvoiceType.DebitnoteRelatedToFinancialAdjustments));
  Assert.AreEqual('261', TZUGFeRDInvoiceTypeExtensions.EnumToString(TZUGFeRDInvoiceType.SelfBilledCreditNote));
  Assert.AreEqual('326', TZUGFeRDInvoiceTypeExtensions.EnumToString(TZUGFeRDInvoiceType.PartialInvoice));
  Assert.AreEqual('380', TZUGFeRDInvoiceTypeExtensions.EnumToString(TZUGFeRDInvoiceType.Invoice));
  Assert.AreEqual('381', TZUGFeRDInvoiceTypeExtensions.EnumToString(TZUGFeRDInvoiceType.CreditNote));
  Assert.AreEqual('383', TZUGFeRDInvoiceTypeExtensions.EnumToString(TZUGFeRDInvoiceType.DebitNote));
  Assert.AreEqual('384', TZUGFeRDInvoiceTypeExtensions.EnumToString(TZUGFeRDInvoiceType.Correction));
  Assert.AreEqual('386', TZUGFeRDInvoiceTypeExtensions.EnumToString(TZUGFeRDInvoiceType.PrepaymentInvoice));
  Assert.AreEqual('389', TZUGFeRDInvoiceTypeExtensions.EnumToString(TZUGFeRDInvoiceType.SelfBilledInvoice));
  Assert.AreEqual('751', TZUGFeRDInvoiceTypeExtensions.EnumToString(TZUGFeRDInvoiceType.InvoiceInformation));
  Assert.AreEqual('1380', TZUGFeRDInvoiceTypeExtensions.EnumToString(TZUGFeRDInvoiceType.CorrectionOld));
  Assert.AreEqual('457', TZUGFeRDInvoiceTypeExtensions.EnumToString(TZUGFeRDInvoiceType.Cancellation));
  Assert.AreEqual('0', TZUGFeRDInvoiceTypeExtensions.EnumToString(TZUGFeRDInvoiceType.Unknown));
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

//  TDUnitX.RegisterTestFixture(TZUGFeRDInvoiceTypeTest);

end.
