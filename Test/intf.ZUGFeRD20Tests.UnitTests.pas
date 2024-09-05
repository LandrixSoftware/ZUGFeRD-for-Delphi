﻿{* Licensed to the Apache Software Foundation (ASF) under one
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

unit intf.ZUGFeRD20Tests.UnitTests;

interface

uses
  System.SysUtils,System.Classes
  ,DUnitX.TestFramework
  ,intf.ZUGFeRDInvoiceDescriptor
  ,intf.ZUGFeRDProfile,intf.ZUGFeRDInvoiceTypes
  ,intf.ZUGFeRDInvoiceProvider
  ,intf.ZUGFeRDVersion
  ,intf.ZUGFeRDCurrencyCodes
  ,intf.ZUGFeRDPaymentTerms
  ;

type
  [TestFixture]
  TZUGFeRD20Tests = class
  public
    [Test]
    procedure TestReferenceBasicInvoice;
    [Test]
    procedure TestReferenceExtendedInvoice;
    [Test]
    procedure TestTotalRounding;
    [Test]
    procedure TestMissingPropertiesAreNull;
    [Test]
    procedure TestMissingPropertListsEmpty;
    [Test]
    procedure TestLoadingSepaPreNotification;
    [Test]
    procedure TestStoringSepaPreNotification;
    [Test]
    procedure TestPartyExtensions;
    [Test]
    procedure TestMimetypeOfEmbeddedAttachment;
    [Test]
    procedure TestOrderInformation;
    [Test]
    procedure TestSellerOrderReferencedDocument;
    [Test]
    procedure TestWriteAndReadBusinessProcess;
    [Test]
    procedure TestWriteAndReadExtended;
  end;

implementation

{ TZUGFeRD20Tests }

procedure TZUGFeRD20Tests.TestReferenceBasicInvoice;
var
  path : String;
  desc : TZUGFeRDInvoiceDescriptor;
begin
  path := '..\..\..\demodata\zugferd20\zugferd_2p0_BASIC_Einfach.xml';

  desc := TZUGFeRDInvoiceDescriptor.Load(path);
  try
    //desc.Save(ExtractFilePath(ParamStr(0))+'test_zugferd20.xml', TZUGFeRDVersion.Version20, TZUGFeRDProfile.Basic);

    Assert.AreEqual(desc.Profile, TZUGFeRDProfile.Basic);
    Assert.AreEqual(desc.Type_, TZUGFeRDInvoiceType.Invoice);
    Assert.AreEqual(desc.InvoiceNo, '471102');
    Assert.AreEqual(Int64(desc.TradeLineItems.Count), Int64(1));
    Assert.AreEqual(desc.LineTotalAmount.Value, Currency(198.00));
    Assert.IsFalse(desc.IsTest);
  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD20Tests.TestReferenceExtendedInvoice;
var
  path : String;
  desc : TZUGFeRDInvoiceDescriptor;
begin
  path := '..\..\..\demodata\zugferd20\zugferd_2p0_EXTENDED_Warenrechnung.xml';

  desc := TZUGFeRDInvoiceDescriptor.Load(path);
  try
    //desc.Save(ExtractFilePath(ParamStr(0))+'test_zugferd20.xml', TZUGFeRDVersion.Version20, TZUGFeRDProfile.Basic);

    Assert.AreEqual(desc.Profile, TZUGFeRDProfile.Extended);
    Assert.AreEqual(desc.Type_, TZUGFeRDInvoiceType.Invoice);
    Assert.AreEqual(desc.InvoiceNo, 'R87654321012345');
    Assert.AreEqual(Int64(desc.TradeLineItems.Count), Int64(6));
    Assert.AreEqual(desc.LineTotalAmount.Value, Currency(457.20));
    Assert.IsTrue(desc.IsTest);

  finally
    desc.Free;
  end;
end;

procedure TZUGFeRD20Tests.TestStoringSepaPreNotification;
begin
  exit;
  var d := TZUGFeRDInvoiceDescriptor.Create;
  d.Type_ := TZUGFeRDInvoiceType.Invoice;
  d.InvoiceNo := '471102';
  d.Currency := TZUGFeRDCurrencyCodes.EUR;
  d.InvoiceDate := EncodeDate(2018, 3, 5);
//  d.AddTradeLineItem(
//          lineID: "1",
//          id: new GlobalID(GlobalIDSchemeIdentifiers.EAN, "4012345001235"),
//          sellerAssignedID: "TB100A4",
//          name: "Trennblätter A4",
//          billedQuantity: 20m,
//          unitCode: QuantityCodes.C62,
//          netUnitPrice: 9.9m,
//          grossUnitPrice: 9.9m,
//          categoryCode: TaxCategoryCodes.S,
//          taxPercent: 19.0m,
//          taxType: TaxTypes.VAT);
//      d.AddTradeLineItem(
//          lineID: "2",
//          id: new GlobalID(GlobalIDSchemeIdentifiers.EAN, "4000050986428"),
//          sellerAssignedID: "ARNR2",
//          name: "Joghurt Banane",
//          billedQuantity: 50m,
//          unitCode: QuantityCodes.C62,
//          netUnitPrice: 5.5m,
//          grossUnitPrice: 5.5m,
//          categoryCode: TaxCategoryCodes.S,
//          taxPercent: 7.0m,
//          taxType: TaxTypes.VAT);
//      d.SetSeller(
//          id: null,
//          globalID: new GlobalID(GlobalIDSchemeIdentifiers.GLN, "4000001123452"),
//          name: "Lieferant GmbH",
//          postcode: "80333",
//          city: "München",
//          street: "Lieferantenstraße 20",
//          country: CountryCodes.DE,
//          legalOrganization: new LegalOrganization(GlobalIDSchemeIdentifiers.GLN, "4000001123452", "Lieferant GmbH"));
//      d.SetBuyer(
//          id: "GE2020211",
//          globalID: new GlobalID(GlobalIDSchemeIdentifiers.GLN, "4000001987658"),
//          name: "Kunden AG Mitte",
//          postcode: "69876",
//          city: "Frankfurt",
//          street: "Kundenstraße 15",
//          country: CountryCodes.DE);
      d.SetPaymentMeansSepaDirectDebit(
          'DE98ZZZ09999999999',
          'REF A-123');
//      d.AddDebitorFinancialAccount(
//          "DE21860000000086001055",
//          null);
      var paymentTerm : TZUGFeRDPaymentTerms := TZUGFeRDPaymentTerms.Create;
      paymentTerm.Description := 'Der Betrag in Höhe von EUR 529,87 wird am 20.03.2018 von Ihrem Konto per SEPA-Lastschrift eingezogen.';
      d.PaymentTermsList.Add(paymentTerm);
//      d.SetTotals(
//          473.00m,
//          0.00m,
//          0.00m,
//          473.00m,
//          56.87m,
//          529.87m,
//          0.00m,
//          529.87m);
//   d.SellerTaxRegistration.Add(new TaxRegistration
//      {
//        SchemeID = TaxRegistrationSchemeID.FC,
//        No = "201/113/40209"
//      });
//      d.SellerTaxRegistration.Add(new TaxRegistration
//      {
//        SchemeID = TaxRegistrationSchemeID.VA,
//        No = "DE123456789"
//      });
//      d.AddApplicableTradeTax(
//          275.00m,
//          7.00m,
//          TaxTypes.VAT,
//          TaxCategoryCodes.S);
//      d.AddApplicableTradeTax(
//          198.00m,
//          19.00m,
//          TaxTypes.VAT,
//          TaxCategoryCodes.S);
//
  var stream : TMemoryStream := TMemoryStream.Create;
  try
    d.Save(stream,TZUGFeRDVersion.Version20,TZUGFeRDProfile.Comfort);

    stream.Position := 0;

    var d2 : TZUGFeRDInvoiceDescriptor := TZUGFeRDInvoiceDescriptor.Load(stream);
    Assert.AreEqual('DE98ZZZ09999999999', d2.PaymentMeans.SEPACreditorIdentifier);
    Assert.AreEqual('REF A-123', d2.PaymentTermsList[0].DirectDebitMandateID);
    Assert.AreEqual(Int64(1), Int64(d2.DebitorBankAccounts.Count));
    if d2.DebitorBankAccounts.Count = 1 then
    Assert.AreEqual('DE21860000000086001055', d2.DebitorBankAccounts[0].IBAN);
  finally
    stream.Free;
  end;
end;

procedure TZUGFeRD20Tests.TestTotalRounding;
begin
//      var uuid = Guid.NewGuid().ToString();
//      var issueDateTime = DateTime.Today;
//
//      var desc = new InvoiceDescriptor
//      {
//        ContractReferencedDocument = new ContractReferencedDocument
//        {
//          ID = uuid,
//          IssueDateTime = issueDateTime
//        }
//      };
//      desc.SetTotals(1.99m, 0m, 0m, 0m, 0m, 2m, 0m, 2m, 0.01m);
//
//      var msExtended = new MemoryStream();
//      desc.Save(msExtended, ZUGFeRDVersion.Version20, Profile.Extended);
//      msExtended.Seek(0, SeekOrigin.Begin);
//
//      var loadedInvoice = InvoiceDescriptor.Load(msExtended);
//      Assert.AreEqual(loadedInvoice.RoundingAmount, 0.01m);
//
//      var msBasic = new MemoryStream();
//      desc.Save(msBasic, ZUGFeRDVersion.Version20);
//      msBasic.Seek(0, SeekOrigin.Begin);
//
//      loadedInvoice = InvoiceDescriptor.Load(msBasic);
//      Assert.AreEqual(loadedInvoice.RoundingAmount, 0m);
end;

procedure TZUGFeRD20Tests.TestLoadingSepaPreNotification;
begin
//      string path = @"..\..\..\..\demodata\zugferd20\zugferd_2p0_EN16931_SEPA_Prenotification.xml";
//      path = _makeSurePathIsCrossPlatformCompatible(path);
//
//      var invoiceDescriptor = InvoiceDescriptor.Load(path);
//      Assert.AreEqual(Profile.Comfort, invoiceDescriptor.Profile);
//
//      Assert.AreEqual("DE98ZZZ09999999999", invoiceDescriptor.PaymentMeans.SEPACreditorIdentifier);
//      Assert.AreEqual("REF A-123", invoiceDescriptor.PaymentMeans.SEPAMandateReference);
//      Assert.AreEqual(1, invoiceDescriptor.DebitorBankAccounts.Count);
//      Assert.AreEqual("DE21860000000086001055", invoiceDescriptor.DebitorBankAccounts[0].IBAN);
//
//      Assert.AreEqual("Der Betrag in Höhe von EUR 529,87 wird am 20.03.2018 von Ihrem Konto per SEPA-Lastschrift eingezogen.", invoiceDescriptor.PaymentTerms.Description.Trim());

end;

procedure TZUGFeRD20Tests.TestMimetypeOfEmbeddedAttachment;
begin
//      string path = @"..\..\..\..\demodata\zugferd20\zugferd_2p0_EXTENDED_Warenrechnung.xml";
//      path = _makeSurePathIsCrossPlatformCompatible(path);
//
//      Stream s = File.Open(path, FileMode.Open);
//      InvoiceDescriptor desc = InvoiceDescriptor.Load(s);
//      s.Close();
//
//      string filename1 = "myrandomdata.pdf";
//      string filename2 = "myrandomdata.bin";
//      DateTime timestamp = DateTime.Now.Date;
//      byte[] data = new byte[32768];
//      new Random().NextBytes(data);
//
//      desc.AddAdditionalReferencedDocument(
//          id: "My-File-PDF",
//          issueDateTime: timestamp,
//          typeCode: AdditionalReferencedDocumentTypeCode.ReferenceDocument,
//          name: "EmbeddedPdf",
//          attachmentBinaryObject: data,
//          filename: filename1);
//
//      desc.AddAdditionalReferencedDocument(
//          id: "My-File-BIN",
//          issueDateTime: timestamp,
//          typeCode: AdditionalReferencedDocumentTypeCode.ReferenceDocument,
//          name: "EmbeddedPdf",
//          attachmentBinaryObject: data,
//          filename: filename2);
//
//      MemoryStream ms = new MemoryStream();
//
//      desc.Save(ms, ZUGFeRDVersion.Version20, Profile.Extended);
//      ms.Seek(0, SeekOrigin.Begin);
//
//      InvoiceDescriptor loadedInvoice = InvoiceDescriptor.Load(ms);
//
//      //One document is already referenced in "zuferd_2p0_EXTENDED_Warenrechnung.xml"
//      Assert.AreEqual(loadedInvoice.AdditionalReferencedDocuments.Count, 3);
//
//      foreach (AdditionalReferencedDocument document in loadedInvoice.AdditionalReferencedDocuments)
//      {
//        if (document.ID == "My-File-PDF")
//        {
//          Assert.AreEqual(document.Filename, filename1);
//          Assert.AreEqual("application/pdf", document.MimeType);
//          Assert.AreEqual(timestamp, document.IssueDateTime);
//        }
//
//        if (document.ID == "My-File-BIN")
//        {
//          Assert.AreEqual(document.Filename, filename2);
//          Assert.AreEqual("application/octet-stream", document.MimeType);
//          Assert.AreEqual(timestamp.AddDays(-2), document.IssueDateTime);
//        }
//      }

end;

procedure TZUGFeRD20Tests.TestMissingPropertiesAreNull;
begin
//      string path = @"..\..\..\..\demodata\zugferd20\zugferd_2p0_BASIC_Einfach.xml";
//      path = _makeSurePathIsCrossPlatformCompatible(path);
//
//      var invoiceDescriptor = InvoiceDescriptor.Load(path);
//
//      Assert.IsTrue(invoiceDescriptor.TradeLineItems.TrueForAll(x => x.BillingPeriodStart == null));
//      Assert.IsTrue(invoiceDescriptor.TradeLineItems.TrueForAll(x => x.BillingPeriodEnd == null));
end;

procedure TZUGFeRD20Tests.TestMissingPropertListsEmpty;
begin
//      string path = @"..\..\..\..\demodata\zugferd20\zugferd_2p0_BASIC_Einfach.xml";
//      path = _makeSurePathIsCrossPlatformCompatible(path);
//
//      var invoiceDescriptor = InvoiceDescriptor.Load(path);
//
//      Assert.IsTrue(invoiceDescriptor.TradeLineItems.TrueForAll(x => x.ApplicableProductCharacteristics.Count == 0));
end;

procedure TZUGFeRD20Tests.TestOrderInformation;
begin
//      string path = @"..\..\..\..\demodata\zugferd20\zugferd_2p0_EXTENDED_Warenrechnung.xml";
//      path = _makeSurePathIsCrossPlatformCompatible(path);
//
//      DateTime timestamp = DateTime.Now.Date;
//
//      Stream s = File.Open(path, FileMode.Open);
//      InvoiceDescriptor desc = InvoiceDescriptor.Load(s);
//      desc.OrderDate = timestamp;
//      desc.OrderNo = "12345";
//      s.Close();
//
//      MemoryStream ms = new MemoryStream();
//      desc.Save(ms, ZUGFeRDVersion.Version20, Profile.Extended);
//
//      ms.Seek(0, SeekOrigin.Begin);
//      StreamReader reader = new StreamReader(ms);
//      string text = reader.ReadToEnd();
//
//      ms.Seek(0, SeekOrigin.Begin);
//      InvoiceDescriptor loadedInvoice = InvoiceDescriptor.Load(ms);
//      Assert.AreEqual(timestamp, loadedInvoice.OrderDate);
//      Assert.AreEqual("12345", loadedInvoice.OrderNo);

end;

procedure TZUGFeRD20Tests.TestPartyExtensions;
begin
//      string path = @"..\..\..\..\demodata\zugferd20\zugferd_2p0_BASIC_Einfach.xml";
//      path = _makeSurePathIsCrossPlatformCompatible(path);
//
//      Stream s = File.Open(path, FileMode.Open);
//      InvoiceDescriptor desc = InvoiceDescriptor.Load(s);
//      s.Close();
//
//      desc.Invoicee = new Party() // this information will not be stored in the output file since it is available in Extended profile only
//      {
//        Name = "Test",
//        ContactName = "Max Mustermann",
//        Postcode = "83022",
//        City = "Rosenheim",
//        Street = "Münchnerstraße 123",
//        AddressLine3 = "EG links",
//        CountrySubdivisionName = "Bayern",
//        Country = CountryCodes.DE
//      };
//      MemoryStream ms = new MemoryStream();
//
//      desc.Save(ms, ZUGFeRDVersion.Version20, Profile.Extended);
//      ms.Seek(0, SeekOrigin.Begin);
//
//      InvoiceDescriptor loadedInvoice = InvoiceDescriptor.Load(ms);
//      Assert.AreEqual("Test", loadedInvoice.Invoicee.Name);
//      Assert.AreEqual("Max Mustermann", loadedInvoice.Invoicee.ContactName);
//      Assert.AreEqual("83022", loadedInvoice.Invoicee.Postcode);
//      Assert.AreEqual("Rosenheim", loadedInvoice.Invoicee.City);
//      Assert.AreEqual("Münchnerstraße 123", loadedInvoice.Invoicee.Street);
//      Assert.AreEqual("EG links", loadedInvoice.Invoicee.AddressLine3);
//      Assert.AreEqual("Bayern", loadedInvoice.Invoicee.CountrySubdivisionName);
//      Assert.AreEqual(CountryCodes.DE, loadedInvoice.Invoicee.Country);
end;

procedure TZUGFeRD20Tests.TestSellerOrderReferencedDocument;
begin
//      string path = @"..\..\..\..\demodata\zugferd20\zugferd_2p0_EXTENDED_Warenrechnung.xml";
//      path = _makeSurePathIsCrossPlatformCompatible(path);
//
//      Stream s = File.Open(path, FileMode.Open);
//      InvoiceDescriptor desc = InvoiceDescriptor.Load(s);
//      s.Close();
//
//      string uuid = System.Guid.NewGuid().ToString();
//      DateTime issueDateTime = DateTime.Today;
//
//      desc.SellerOrderReferencedDocument = new SellerOrderReferencedDocument()
//      {
//        ID = uuid,
//        IssueDateTime = issueDateTime
//      };
//
//      MemoryStream ms = new MemoryStream();
//      desc.Save(ms, ZUGFeRDVersion.Version20, Profile.Extended);
//
//      ms.Seek(0, SeekOrigin.Begin);
//      StreamReader reader = new StreamReader(ms);
//      string text = reader.ReadToEnd();
//
//      ms.Seek(0, SeekOrigin.Begin);
//      InvoiceDescriptor loadedInvoice = InvoiceDescriptor.Load(ms);
//
//      Assert.AreEqual(Profile.Extended, loadedInvoice.Profile);
//      Assert.AreEqual(uuid, loadedInvoice.SellerOrderReferencedDocument.ID);
//      Assert.AreEqual(issueDateTime, loadedInvoice.SellerOrderReferencedDocument.IssueDateTime);
end;

procedure TZUGFeRD20Tests.TestWriteAndReadBusinessProcess;
begin
//      InvoiceDescriptor desc = this.InvoiceProvider.CreateInvoice();
//      desc.BusinessProcess = "A1";
//
//      MemoryStream ms = new MemoryStream();
//      desc.Save(ms, ZUGFeRDVersion.Version20, Profile.Extended);
//      ms.Seek(0, SeekOrigin.Begin);
//      InvoiceDescriptor loadedInvoice = InvoiceDescriptor.Load(ms);
//
//      Assert.AreEqual("A1", loadedInvoice.BusinessProcess);
end;

    /// <summary>
    /// This test ensure that Writer and Reader uses the same path and namespace for elements
    /// </summary>
procedure TZUGFeRD20Tests.TestWriteAndReadExtended;
begin
//      InvoiceDescriptor desc = this.InvoiceProvider.CreateInvoice();
//      string filename2 = "myrandomdata.bin";
//      DateTime timestamp = DateTime.Now.Date;
//      byte[] data = new byte[32768];
//      new Random().NextBytes(data);
//
//      desc.AddAdditionalReferencedDocument(
//          id: "My-File-BIN",
//          typeCode: AdditionalReferencedDocumentTypeCode.ReferenceDocument,
//          issueDateTime: timestamp.AddDays(-2),
//          name: "EmbeddedPdf",
//          attachmentBinaryObject: data,
//          filename: filename2);
//
//      desc.OrderNo = "12345";
//      desc.OrderDate = timestamp;
//
//      desc.SetContractReferencedDocument("12345", timestamp);
//
//      desc.SpecifiedProcuringProject = new SpecifiedProcuringProject { ID = "123", Name = "Project 123" };
//
//      desc.ShipTo = new Party
//      {
//        ID = new GlobalID(GlobalIDSchemeIdentifiers.Unknown, "123"),
//        GlobalID = new GlobalID(GlobalIDSchemeIdentifiers.DUNS, "789"),
//        Name = "Ship To",
//        ContactName = "Max Mustermann",
//        Street = "Münchnerstr. 55",
//        Postcode = "83022",
//        City = "Rosenheim",
//        Country = CountryCodes.DE
//      };
//
//      desc.ShipFrom = new Party
//      {
//        ID = new GlobalID(GlobalIDSchemeIdentifiers.Unknown, "123"),
//        GlobalID = new GlobalID(GlobalIDSchemeIdentifiers.DUNS, "789"),
//        Name = "Ship From",
//        ContactName = "Eva Musterfrau",
//        Street = "Alpenweg 5",
//        Postcode = "83022",
//        City = "Rosenheim",
//        Country = CountryCodes.DE
//      };
//
//      desc.PaymentMeans.SEPACreditorIdentifier = "SepaID";
//      desc.PaymentMeans.SEPAMandateReference = "SepaMandat";
//      desc.PaymentMeans.FinancialCard = new FinancialCard { Id = "123", CardholderName = "Mustermann" };
//
//      desc.PaymentReference = "PaymentReference";
//
//      desc.Invoicee = new Party()
//      {
//        Name = "Test",
//        ContactName = "Max Mustermann",
//        Postcode = "83022",
//        City = "Rosenheim",
//        Street = "Münchnerstraße 123",
//        AddressLine3 = "EG links",
//        CountrySubdivisionName = "Bayern",
//        Country = CountryCodes.DE
//      };
//
//      desc.Payee = new Party() // this information will not be stored in the output file since it is available in Extended profile only
//      {
//        Name = "Test",
//        ContactName = "Max Mustermann",
//        Postcode = "83022",
//        City = "Rosenheim",
//        Street = "Münchnerstraße 123",
//        AddressLine3 = "EG links",
//        CountrySubdivisionName = "Bayern",
//        Country = CountryCodes.DE
//      };
//
//      desc.AddDebitorFinancialAccount(iban: "DE02120300000000202052", bic: "BYLADEM1001", bankName: "Musterbank");
//      desc.BillingPeriodStart = timestamp;
//      desc.BillingPeriodEnd = timestamp.AddDays(14);
//
//      desc.AddTradeAllowanceCharge(false, 5m, CurrencyCodes.EUR, 15m, "Reason for charge", TaxTypes.AAB, TaxCategoryCodes.AB, 19m);
//      desc.AddLogisticsServiceCharge(10m, "Logistics service charge", TaxTypes.AAC, TaxCategoryCodes.AC, 7m);
//
//      desc.PaymentTerms.DueDate = timestamp.AddDays(14);
//      desc.SetInvoiceReferencedDocument("RE-12345", timestamp);
//
//
//      //set additional LineItem data
//      var lineItem = desc.TradeLineItems.FirstOrDefault(i => i.SellerAssignedID == "TB100A4");
//      Assert.IsNotNull(lineItem);
//      lineItem.Description = "This is line item TB100A4";
//      lineItem.BuyerAssignedID = "0815";
//      lineItem.SetOrderReferencedDocument("12345", timestamp);
//      lineItem.SetDeliveryNoteReferencedDocument("12345", timestamp);
//      lineItem.SetContractReferencedDocument("12345", timestamp);
//
//      lineItem.AddAdditionalReferencedDocument("xyz", ReferenceTypeCodes.AAB, timestamp);
//
//      lineItem.UnitQuantity = 3m;
//      lineItem.ActualDeliveryDate = timestamp;
//
//      lineItem.ApplicableProductCharacteristics.Add(new ApplicableProductCharacteristic
//      {
//        Description = "Product characteristics",
//        Value = "Product value"
//      });
//
//      lineItem.BillingPeriodStart = timestamp;
//      lineItem.BillingPeriodEnd = timestamp.AddDays(10);
//
//      lineItem.AddReceivableSpecifiedTradeAccountingAccount("987654");
//      lineItem.AddTradeAllowanceCharge(false, CurrencyCodes.EUR, 10m, 50m, "Reason: UnitTest");
//
//
//      MemoryStream ms = new MemoryStream();
//      desc.Save(ms, ZUGFeRDVersion.Version20, Profile.Extended);
//
//      ms.Seek(0, SeekOrigin.Begin);
//      StreamReader reader = new StreamReader(ms);
//      string text = reader.ReadToEnd();
//
//      ms.Seek(0, SeekOrigin.Begin);
//      InvoiceDescriptor loadedInvoice = InvoiceDescriptor.Load(ms);
//
//      Assert.AreEqual("471102", loadedInvoice.InvoiceNo);
//      Assert.AreEqual(new DateTime(2018, 03, 05), loadedInvoice.InvoiceDate);
//      Assert.AreEqual(CurrencyCodes.EUR, loadedInvoice.Currency);
//      Assert.IsTrue(loadedInvoice.Notes.Any(n => n.Content == "Rechnung gemäß Bestellung vom 01.03.2018."));
//      Assert.AreEqual("04011000-12345-34", loadedInvoice.ReferenceOrderNo);
//      Assert.AreEqual("Lieferant GmbH", loadedInvoice.Seller.Name);
//      Assert.AreEqual("80333", loadedInvoice.Seller.Postcode);
//      Assert.AreEqual("München", loadedInvoice.Seller.City);
//
//      Assert.AreEqual("Lieferantenstraße 20", loadedInvoice.Seller.Street);
//      Assert.AreEqual(CountryCodes.DE, loadedInvoice.Seller.Country);
//      Assert.AreEqual(GlobalIDSchemeIdentifiers.GLN, loadedInvoice.Seller.GlobalID.SchemeID);
//      Assert.AreEqual("4000001123452", loadedInvoice.Seller.GlobalID.ID);
//      Assert.AreEqual("Max Mustermann", loadedInvoice.SellerContact.Name);
//      Assert.AreEqual("Muster-Einkauf", loadedInvoice.SellerContact.OrgUnit);
//      Assert.AreEqual("Max@Mustermann.de", loadedInvoice.SellerContact.EmailAddress);
//      Assert.AreEqual("+49891234567", loadedInvoice.SellerContact.PhoneNo);
//
//      Assert.AreEqual("Kunden AG Mitte", loadedInvoice.Buyer.Name);
//      Assert.AreEqual("69876", loadedInvoice.Buyer.Postcode);
//      Assert.AreEqual("Frankfurt", loadedInvoice.Buyer.City);
//      Assert.AreEqual("Kundenstraße 15", loadedInvoice.Buyer.Street);
//      Assert.AreEqual(CountryCodes.DE, loadedInvoice.Buyer.Country);
//      Assert.AreEqual<string>("GE2020211", loadedInvoice.Buyer.ID.ID);
//
//      Assert.AreEqual("12345", loadedInvoice.OrderNo);
//      Assert.AreEqual(timestamp, loadedInvoice.OrderDate);
//
//      //Currently not implemented
//      //Assert.AreEqual("12345", loadedInvoice.ContractReferencedDocument.ID);
//      //Assert.AreEqual(timestamp, loadedInvoice.ContractReferencedDocument.IssueDateTime);
//
//      //Currently not implemented
//      //Assert.AreEqual("123", loadedInvoice.SpecifiedProcuringProject.ID);
//      //Assert.AreEqual("Project 123", loadedInvoice.SpecifiedProcuringProject.Name);
//
//      Assert.AreEqual<string>("123", loadedInvoice.ShipTo.ID.ID);
//      Assert.AreEqual(GlobalIDSchemeIdentifiers.DUNS, loadedInvoice.ShipTo.GlobalID.SchemeID);
//      Assert.AreEqual("789", loadedInvoice.ShipTo.GlobalID.ID);
//      Assert.AreEqual("Ship To", loadedInvoice.ShipTo.Name);
//      Assert.AreEqual("Max Mustermann", loadedInvoice.ShipTo.ContactName);
//      Assert.AreEqual("Münchnerstr. 55", loadedInvoice.ShipTo.Street);
//      Assert.AreEqual("83022", loadedInvoice.ShipTo.Postcode);
//      Assert.AreEqual("Rosenheim", loadedInvoice.ShipTo.City);
//      Assert.AreEqual(CountryCodes.DE, loadedInvoice.ShipTo.Country);
//
//      Assert.AreEqual<string>("123", loadedInvoice.ShipFrom.ID.ID);
//      Assert.AreEqual(GlobalIDSchemeIdentifiers.DUNS, loadedInvoice.ShipFrom.GlobalID.SchemeID);
//      Assert.AreEqual("789", loadedInvoice.ShipFrom.GlobalID.ID);
//      Assert.AreEqual("Ship From", loadedInvoice.ShipFrom.Name);
//      Assert.AreEqual("Eva Musterfrau", loadedInvoice.ShipFrom.ContactName);
//      Assert.AreEqual("Alpenweg 5", loadedInvoice.ShipFrom.Street);
//      Assert.AreEqual("83022", loadedInvoice.ShipFrom.Postcode);
//      Assert.AreEqual("Rosenheim", loadedInvoice.ShipFrom.City);
//      Assert.AreEqual(CountryCodes.DE, loadedInvoice.ShipFrom.Country);
//
//
//      Assert.AreEqual(new DateTime(2018, 03, 05), loadedInvoice.ActualDeliveryDate);
//      Assert.AreEqual(PaymentMeansTypeCodes.SEPACreditTransfer, loadedInvoice.PaymentMeans.TypeCode);
//      Assert.AreEqual("Zahlung per SEPA Überweisung.", loadedInvoice.PaymentMeans.Information);
//
//      Assert.AreEqual("PaymentReference", loadedInvoice.PaymentReference);
//
//      Assert.AreEqual("SepaID", loadedInvoice.PaymentMeans.SEPACreditorIdentifier);
//      Assert.AreEqual("SepaMandat", loadedInvoice.PaymentMeans.SEPAMandateReference);
//      Assert.AreEqual("123", loadedInvoice.PaymentMeans.FinancialCard.Id);
//      Assert.AreEqual("Mustermann", loadedInvoice.PaymentMeans.FinancialCard.CardholderName);
//
//      var bankAccount = loadedInvoice.CreditorBankAccounts.FirstOrDefault(a => a.IBAN == "DE02120300000000202051");
//      Assert.IsNotNull(bankAccount);
//      Assert.AreEqual("Kunden AG", bankAccount.Name);
//      Assert.AreEqual("DE02120300000000202051", bankAccount.IBAN);
//      Assert.AreEqual("BYLADEM1001", bankAccount.BIC);
//
//      var debitorBankAccount = loadedInvoice.DebitorBankAccounts.FirstOrDefault(a => a.IBAN == "DE02120300000000202052");
//      Assert.IsNotNull(debitorBankAccount);
//      Assert.AreEqual("DE02120300000000202052", debitorBankAccount.IBAN);
//
//
//      Assert.AreEqual("Test", loadedInvoice.Invoicee.Name);
//      Assert.AreEqual("Max Mustermann", loadedInvoice.Invoicee.ContactName);
//      Assert.AreEqual("83022", loadedInvoice.Invoicee.Postcode);
//      Assert.AreEqual("Rosenheim", loadedInvoice.Invoicee.City);
//      Assert.AreEqual("Münchnerstraße 123", loadedInvoice.Invoicee.Street);
//      Assert.AreEqual("EG links", loadedInvoice.Invoicee.AddressLine3);
//      Assert.AreEqual("Bayern", loadedInvoice.Invoicee.CountrySubdivisionName);
//      Assert.AreEqual(CountryCodes.DE, loadedInvoice.Invoicee.Country);
//
//      Assert.AreEqual("Test", loadedInvoice.Payee.Name);
//      Assert.AreEqual("Max Mustermann", loadedInvoice.Payee.ContactName);
//      Assert.AreEqual("83022", loadedInvoice.Payee.Postcode);
//      Assert.AreEqual("Rosenheim", loadedInvoice.Payee.City);
//      Assert.AreEqual("Münchnerstraße 123", loadedInvoice.Payee.Street);
//      Assert.AreEqual("EG links", loadedInvoice.Payee.AddressLine3);
//      Assert.AreEqual("Bayern", loadedInvoice.Payee.CountrySubdivisionName);
//      Assert.AreEqual(CountryCodes.DE, loadedInvoice.Payee.Country);
//
//
//      var tax = loadedInvoice.Taxes.FirstOrDefault(t => t.BasisAmount == 275m);
//      Assert.IsNotNull(tax);
//      Assert.AreEqual(275m, tax.BasisAmount);
//      Assert.AreEqual(7m, tax.Percent);
//      Assert.AreEqual(TaxTypes.VAT, tax.TypeCode);
//      Assert.AreEqual(TaxCategoryCodes.S, tax.CategoryCode);
//
//      Assert.AreEqual(timestamp, loadedInvoice.BillingPeriodStart);
//      Assert.AreEqual(timestamp.AddDays(14), loadedInvoice.BillingPeriodEnd);
//
//      //TradeAllowanceCharges
//      var tradeAllowanceCharge = loadedInvoice.GetTradeAllowanceCharges.FirstOrDefault(i => i.Reason == "Reason for charge");
//      Assert.IsNotNull(tradeAllowanceCharge);
//      Assert.IsTrue(tradeAllowanceCharge.ChargeIndicator);
//      Assert.AreEqual("Reason for charge", tradeAllowanceCharge.Reason);
//      Assert.AreEqual(5m, tradeAllowanceCharge.BasisAmount);
//      Assert.AreEqual(15m, tradeAllowanceCharge.ActualAmount);
//      Assert.AreEqual(CurrencyCodes.EUR, tradeAllowanceCharge.Currency);
//      Assert.AreEqual(19m, tradeAllowanceCharge.Tax.Percent);
//      Assert.AreEqual(TaxTypes.AAB, tradeAllowanceCharge.Tax.TypeCode);
//      Assert.AreEqual(TaxCategoryCodes.AB, tradeAllowanceCharge.Tax.CategoryCode);
//
//      //ServiceCharges
//      var serviceCharge = desc.ServiceCharges.FirstOrDefault(i => i.Description == "Logistics service charge");
//      Assert.IsNotNull(serviceCharge);
//      Assert.AreEqual("Logistics service charge", serviceCharge.Description);
//      Assert.AreEqual(10m, serviceCharge.Amount);
//      Assert.AreEqual(7m, serviceCharge.Tax.Percent);
//      Assert.AreEqual(TaxTypes.AAC, serviceCharge.Tax.TypeCode);
//      Assert.AreEqual(TaxCategoryCodes.AC, serviceCharge.Tax.CategoryCode);
//
//      Assert.AreEqual("Zahlbar innerhalb 30 Tagen netto bis 04.04.2018, 3% Skonto innerhalb 10 Tagen bis 15.03.2018", loadedInvoice.PaymentTerms.Description);
//      Assert.AreEqual(timestamp.AddDays(14), loadedInvoice.PaymentTerms.DueDate);
//
//
//      Assert.AreEqual(473.0m, loadedInvoice.LineTotalAmount);
//      Assert.AreEqual(null, loadedInvoice.ChargeTotalAmount); // optional
//      Assert.AreEqual(null, loadedInvoice.AllowanceTotalAmount); // optional
//      Assert.AreEqual(473.0m, loadedInvoice.TaxBasisAmount);
//      Assert.AreEqual(56.87m, loadedInvoice.TaxTotalAmount);
//      Assert.AreEqual(529.87m, loadedInvoice.GrandTotalAmount);
//      Assert.AreEqual(null, loadedInvoice.TotalPrepaidAmount); // optional
//      Assert.AreEqual(529.87m, loadedInvoice.DuePayableAmount);
//
//      //InvoiceReferencedDocument
//      Assert.AreEqual("RE-12345", loadedInvoice.InvoiceReferencedDocument.ID);
//      Assert.AreEqual(timestamp, loadedInvoice.InvoiceReferencedDocument.IssueDateTime);
//
//
//      //Line items
//      var loadedLineItem = loadedInvoice.TradeLineItems.FirstOrDefault(i => i.SellerAssignedID == "TB100A4");
//      Assert.IsNotNull(loadedLineItem);
//      Assert.IsTrue(!string.IsNullOrEmpty(loadedLineItem.AssociatedDocument.LineID));
//      Assert.AreEqual("This is line item TB100A4", loadedLineItem.Description);
//
//      Assert.AreEqual("Trennblätter A4", loadedLineItem.Name);
//
//      Assert.AreEqual("TB100A4", loadedLineItem.SellerAssignedID);
//      Assert.AreEqual("0815", loadedLineItem.BuyerAssignedID);
//      Assert.AreEqual(GlobalIDSchemeIdentifiers.EAN, loadedLineItem.GlobalID.SchemeID);
//      Assert.AreEqual("4012345001235", loadedLineItem.GlobalID.ID);
//
//      //GrossPriceProductTradePrice
//      Assert.AreEqual(9.9m, loadedLineItem.GrossUnitPrice);
//      Assert.AreEqual(QuantityCodes.H87, loadedLineItem.UnitCode);
//      Assert.AreEqual(3m, loadedLineItem.UnitQuantity);
//
//      //NetPriceProductTradePrice
//      Assert.AreEqual(9.9m, loadedLineItem.NetUnitPrice);
//      Assert.AreEqual(20m, loadedLineItem.BilledQuantity);
//
//      Assert.AreEqual(TaxTypes.VAT, loadedLineItem.TaxType);
//      Assert.AreEqual(TaxCategoryCodes.S, loadedLineItem.TaxCategoryCode);
//      Assert.AreEqual(19m, loadedLineItem.TaxPercent);
//
//      Assert.AreEqual("12345", loadedLineItem.BuyerOrderReferencedDocument.ID);
//      Assert.AreEqual(timestamp, loadedLineItem.BuyerOrderReferencedDocument.IssueDateTime);
//      Assert.AreEqual("12345", loadedLineItem.DeliveryNoteReferencedDocument.ID);
//      Assert.AreEqual(timestamp, loadedLineItem.DeliveryNoteReferencedDocument.IssueDateTime);
//      Assert.AreEqual("12345", loadedLineItem.ContractReferencedDocument.ID);
//      Assert.AreEqual(timestamp, loadedLineItem.ContractReferencedDocument.IssueDateTime);
//
//      var lineItemReferencedDoc = loadedLineItem.AdditionalReferencedDocuments.FirstOrDefault();
//      Assert.IsNotNull(lineItemReferencedDoc);
//      Assert.AreEqual("xyz", lineItemReferencedDoc.ID);
//      Assert.AreEqual(timestamp, lineItemReferencedDoc.IssueDateTime);
//      Assert.AreEqual(ReferenceTypeCodes.AAB, lineItemReferencedDoc.ReferenceTypeCode);
//
//
//      var productCharacteristics = loadedLineItem.ApplicableProductCharacteristics.FirstOrDefault();
//      Assert.IsNotNull(productCharacteristics);
//      Assert.AreEqual("Product characteristics", productCharacteristics.Description);
//      Assert.AreEqual("Product value", productCharacteristics.Value);
//
//      Assert.AreEqual(timestamp, loadedLineItem.ActualDeliveryDate);
//      Assert.AreEqual(timestamp, loadedLineItem.BillingPeriodStart);
//      Assert.AreEqual(timestamp.AddDays(10), loadedLineItem.BillingPeriodEnd);
//
//      //Currently not implemented
//      //var accountingAccount = loadedLineItem.ReceivableSpecifiedTradeAccountingAccounts.FirstOrDefault();
//      //Assert.IsNotNull(accountingAccount);
//      //Assert.AreEqual("987654", accountingAccount.TradeAccountID);
//
//
//      var lineItemTradeAllowanceCharge = loadedLineItem.GetTradeAllowanceCharges.FirstOrDefault(i => i.Reason == "Reason: UnitTest");
//      Assert.IsNotNull(lineItemTradeAllowanceCharge);
//      Assert.IsTrue(lineItemTradeAllowanceCharge.ChargeIndicator);
//      Assert.AreEqual(10m, lineItemTradeAllowanceCharge.BasisAmount);
//      Assert.AreEqual(50m, lineItemTradeAllowanceCharge.ActualAmount);
//      Assert.AreEqual("Reason: UnitTest", lineItemTradeAllowanceCharge.Reason);
//    }
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
