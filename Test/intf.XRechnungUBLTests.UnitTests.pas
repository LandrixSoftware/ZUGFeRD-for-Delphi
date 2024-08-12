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

unit intf.XRechnungUBLTests.UnitTests;

interface

uses
  System.SysUtils
  ,DUnitX.TestFramework
  ,intf.ZUGFeRDInvoiceDescriptor
  ,intf.ZUGFeRDProfile,intf.ZUGFeRDInvoiceTypes
  ,intf.ZUGFeRDInvoiceProvider
  ,intf.ZUGFeRDVersion
  ;

type
  [TestFixture]
  TXRechnungUBLTests = class
  public
    [Test]
    procedure TestInvoiceCreation;
    [Test]
    procedure TestTradelineitemProductCharacterstics;
    /// <summary>
    /// https://github.com/stephanstapel/ZUGFeRD-csharp/issues/319
    /// </summary>
    [Test]
    procedure TestSkippingOfAllowanceChargeBasisAmount;
  end;

implementation

{ TXRechnungUBLTests }

procedure TXRechnungUBLTests.TestInvoiceCreation;
begin
//            InvoiceDescriptor desc = this.InvoiceProvider.CreateInvoice();
//            MemoryStream ms = new MemoryStream();
//
//            desc.Save(ms, ZUGFeRDVersion.Version22, Profile.XRechnung, ZUGFeRDFormats.UBL);
//            ms.Seek(0, SeekOrigin.Begin);
//
//            InvoiceDescriptor loadedInvoice = InvoiceDescriptor.Load(ms);
//
//            Assert.AreEqual(loadedInvoice.Invoicee, null);
//            Assert.AreNotEqual(loadedInvoice.Seller, null);
//            Assert.AreEqual(loadedInvoice.Taxes.Count, 2);
//            Assert.AreEqual(loadedInvoice.SellerContact.Name, "Max Mustermann");
//            Assert.IsNull(loadedInvoice.BuyerContact);
end;

procedure TXRechnungUBLTests.TestSkippingOfAllowanceChargeBasisAmount;
begin
//            // actual values do not matter
//            decimal basisAmount = 123.0m;
//            decimal percent = 11.0m;
//            decimal allowanceChargeBasisAmount = 121.0m;
//
//            InvoiceDescriptor desc = this.InvoiceProvider.CreateInvoice();
//            desc.AddApplicableTradeTax(basisAmount, percent, TaxTypes.LOC, TaxCategoryCodes.K, allowanceChargeBasisAmount);
//            MemoryStream ms = new MemoryStream();
//
//            desc.Save(ms, ZUGFeRDVersion.Version22, Profile.XRechnung, ZUGFeRDFormats.UBL);
//            ms.Seek(0, SeekOrigin.Begin);
//
//            InvoiceDescriptor loadedInvoice = InvoiceDescriptor.Load(ms);
//
//            Tax tax = loadedInvoice.Taxes.FirstOrDefault(t => t.TypeCode == TaxTypes.LOC);
//            Assert.IsNotNull(tax);
//            Assert.AreEqual(basisAmount, tax.BasisAmount);
//            Assert.AreEqual(percent, tax.Percent);
//            Assert.AreEqual(null, tax.AllowanceChargeBasisAmount);
end;

procedure TXRechnungUBLTests.TestTradelineitemProductCharacterstics;
begin
//            InvoiceDescriptor desc = this.InvoiceProvider.CreateInvoice();
//
//            desc.TradeLineItems[0].ApplicableProductCharacteristics = new ApplicableProductCharacteristic[]
//                    {
//                        new ApplicableProductCharacteristic()
//                        {
//                            Description = "Test Description",
//                            Value = "1.5 kg"
//                        },
//                        new ApplicableProductCharacteristic()
//                        {
//                            Description = "UBL Characterstics 2",
//                            Value = "3 kg"
//                        },
//                    }.ToList();
//
//            MemoryStream ms = new MemoryStream();
//
//            desc.Save(ms, ZUGFeRDVersion.Version22, Profile.XRechnung, ZUGFeRDFormats.UBL);
//            ms.Seek(0, SeekOrigin.Begin);
//
//            InvoiceDescriptor loadedInvoice = InvoiceDescriptor.Load(ms);
//
//            Assert.IsNotNull(loadedInvoice.TradeLineItems);
//            Assert.AreEqual(loadedInvoice.TradeLineItems[0].ApplicableProductCharacteristics.Count, 2);
//            Assert.AreEqual(loadedInvoice.TradeLineItems[0].ApplicableProductCharacteristics[0].Description, "Test Description");
//            Assert.AreEqual(loadedInvoice.TradeLineItems[0].ApplicableProductCharacteristics[1].Value, "3 kg");
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

end.
