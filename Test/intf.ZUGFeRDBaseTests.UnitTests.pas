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

unit intf.ZUGFeRDBaseTests.UnitTests;

interface

uses
  DUnitX.TestFramework
  ,intf.ZUGFeRDVersion
  ,intf.ZUGFeRDInvoiceDescriptor
  ,intf.ZUGFeRDProfile,intf.ZUGFeRDInvoiceTypes
  ,intf.ZUGFeRDInvoiceProvider
  ;

type
  [TestFixture]
  TZUGFeRDBaseTests = class
  public
    [Test]
    procedure TestAutomaticLineIds;
    [Test]
    procedure TestManualLineIds;
    [Test]
    procedure TestCommentLine;
    [Test]
    procedure TestGetVersion;
  end;

implementation

{ TZUGFeRDBaseTests }

procedure TZUGFeRDBaseTests.TestAutomaticLineIds;
var
  desc : TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.TradeLineItems.Clear();

    desc.AddTradeLineItem('Item1','');
    desc.AddTradeLineItem('Item2','');

    Assert.AreEqual(desc.TradeLineItems[0].AssociatedDocument.LineID, '1');
    Assert.AreEqual(desc.TradeLineItems[1].AssociatedDocument.LineID, '2');
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDBaseTests.TestManualLineIds;
var
  desc : TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceProvider.CreateInvoice;
  try
    desc.TradeLineItems.Clear();

    desc.AddTradeLineItem('item-01','Item1','');
    desc.AddTradeLineItem('item-02','Item2','');

    Assert.AreEqual(desc.TradeLineItems[0].AssociatedDocument.LineID, 'item-01');
    Assert.AreEqual(desc.TradeLineItems[1].AssociatedDocument.LineID, 'item-02');
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRDBaseTests.TestCommentLine;
begin
//            string COMMENT = System.Guid.NewGuid().ToString();
//            string CUSTOM_LINE_ID = System.Guid.NewGuid().ToString();
//
//            // test with automatic line id
//            InvoiceDescriptor desc = this.InvoiceProvider.CreateInvoice();
//            int numberOfTradeLineItems = desc.TradeLineItems.Count;
//            desc.AddTradeLineCommentItem(COMMENT);
//
//            Assert.AreEqual(numberOfTradeLineItems + 1, desc.TradeLineItems.Count);
//            Assert.IsNotNull(desc.TradeLineItems[desc.TradeLineItems.Count - 1].AssociatedDocument);
//            Assert.IsNotNull(desc.TradeLineItems[desc.TradeLineItems.Count - 1].AssociatedDocument.Notes);
//            Assert.AreEqual(desc.TradeLineItems[desc.TradeLineItems.Count - 1].AssociatedDocument.Notes.Count, 1);
//            Assert.AreEqual(desc.TradeLineItems[desc.TradeLineItems.Count - 1].AssociatedDocument.Notes[0].Content, COMMENT);
//
//
//            // test with manual line id
//            desc = this.InvoiceProvider.CreateInvoice();
//            numberOfTradeLineItems = desc.TradeLineItems.Count;
//            desc.AddTradeLineCommentItem(CUSTOM_LINE_ID, COMMENT);
//
//            Assert.AreEqual(numberOfTradeLineItems + 1, desc.TradeLineItems.Count);
//            Assert.IsNotNull(desc.TradeLineItems[desc.TradeLineItems.Count - 1].LineID, CUSTOM_LINE_ID);
//            Assert.IsNotNull(desc.TradeLineItems[desc.TradeLineItems.Count - 1].AssociatedDocument);
//            Assert.IsNotNull(desc.TradeLineItems[desc.TradeLineItems.Count - 1].AssociatedDocument.Notes);
//            Assert.AreEqual(desc.TradeLineItems[desc.TradeLineItems.Count - 1].AssociatedDocument.Notes.Count, 1);
//            Assert.AreEqual(desc.TradeLineItems[desc.TradeLineItems.Count - 1].AssociatedDocument.Notes[0].Content, COMMENT);
end;

procedure TZUGFeRDBaseTests.TestGetVersion;
var
  path : String;
begin
  path := '.\..\..\..\demodata\zugferd10\ZUGFeRD_1p0_COMFORT_Einfach.xml';
  Assert.AreEqual(TZUGFeRDInvoiceDescriptor.GetVersion(path), TZUGFeRDVersion.Version1);

  path := '.\..\..\..\demodata\zugferd20\zugferd_2p0_BASIC_Einfach.xml';
  Assert.AreEqual(TZUGFeRDInvoiceDescriptor.GetVersion(path), TZUGFeRDVersion.Version20);

  path := '.\..\..\..\demodata\zugferd21\zugferd_2p1_BASIC_Einfach-factur-x.xml';
  Assert.AreEqual(TZUGFeRDInvoiceDescriptor.GetVersion(path), TZUGFeRDVersion.Version21);
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
