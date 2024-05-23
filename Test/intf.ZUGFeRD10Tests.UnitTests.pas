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

unit intf.ZUGFeRD10Tests.UnitTests;

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
  TZUGFeRD10Tests = class
  public
    [Test]
    procedure TestReferenceComfortInvoice;
    [Test]
    procedure TestReferenceComfortInvoiceRabattiert;
    [Test]
    procedure TestStoringInvoiceViaFile;
    [Test]
    procedure TestStoringInvoiceViaStreams;
    [Test]
    procedure TestMissingPropertiesAreNull;
  end;

implementation

{ TZUGFeRD10Tests }

procedure TZUGFeRD10Tests.TestReferenceComfortInvoice;
begin
  var lFilename : String := '..\..\..\demodata\zugferd10\ZUGFeRD_1p0_COMFORT_Einfach.xml';
  var desc : TZUGFeRDInvoiceDescriptor := TZUGFeRDInvoiceDescriptor.Load(lFilename);
  try
    Assert.AreEqual(desc.Profile, TZUGFeRDProfile.Comfort);
    Assert.AreEqual(desc.Type_, TZUGFeRDInvoiceType.Invoice);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD10Tests.TestReferenceComfortInvoiceRabattiert;
var
  path : String;
  desc : TZUGFeRDInvoiceDescriptor;
begin
  path := '..\..\..\demodata\zugferd10\ZUGFeRD_1p0_COMFORT_Rabatte.xml';

  desc := TZUGFeRDInvoiceDescriptor.Load(path);
  try
    //desc.Save(ExtractFilePath(ParamStr(0))+'test_zugferd1.xml', TZUGFeRDVersion.Version1, TZUGFeRDProfile.Comfort);

    Assert.AreEqual(desc.Profile, TZUGFeRDProfile.Comfort);
    Assert.AreEqual(desc.Type_, TZUGFeRDInvoiceType.Invoice);
    Assert.AreEqual(desc.CreditorBankAccounts[0].BankName, 'Hausbank München');
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD10Tests.TestStoringInvoiceViaFile;
var
  path : String;
  desc : TZUGFeRDInvoiceDescriptor;
begin
//            string path = "output.xml";
//            InvoiceDescriptor desc = this.InvoiceProvider.CreateInvoice();
//            desc.Save(path, ZUGFeRDVersion.Version1, Profile.Comfort);
//
//            InvoiceDescriptor desc2 = InvoiceDescriptor.Load(path);
//            // TODO: Add more asserts
end;

procedure TZUGFeRD10Tests.TestStoringInvoiceViaStreams;
var
  path : String;
  desc : TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
//    path := 'output_stream.xml';
//    FileStream saveStream = new FileStream(path, FileMode.Create);
//    desc.Save(saveStream, ZUGFeRDVersion.Version1, Profile.Comfort);
//    saveStream.Close();
//
//    FileStream loadStream = new FileStream(path, FileMode.Open);
//    InvoiceDescriptor desc2 = InvoiceDescriptor.Load(loadStream);
//    loadStream.Close();
//
//    Assert.AreEqual(desc2.Profile, Profile.Comfort);
//    Assert.AreEqual(desc2.Type, InvoiceType.Invoice);
//
//
//    // try again with a memory stream
//    MemoryStream ms = new MemoryStream();
//    desc.Save(ms, ZUGFeRDVersion.Version1, Profile.Comfort);
//
//    byte[] data = ms.ToArray();
//    string s = System.Text.Encoding.Default.GetString(data);
//    // TODO: Add more asserts
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD10Tests.TestMissingPropertiesAreNull;
var
  path : String;
  invoiceDescriptor : TZUGFeRDInvoiceDescriptor;
begin
  path := '..\..\..\demodata\zugferd10\ZUGFeRD_1p0_COMFORT_Einfach.xml';

  invoiceDescriptor := TZUGFeRDInvoiceDescriptor.Load(path);
  try
//    Assert.IsTrue(invoiceDescriptor.TradeLineItems.TrueForAll(x => x.BillingPeriodStart == null));
//    Assert.IsTrue(invoiceDescriptor.TradeLineItems.TrueForAll(x => x.BillingPeriodEnd == null));
  finally
    invoiceDescriptor.Free;
  end;
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
