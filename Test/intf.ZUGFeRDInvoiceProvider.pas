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

unit intf.ZUGFeRDInvoiceProvider;

interface

uses
  System.SysUtils
  ,intf.ZUGFeRDInvoiceDescriptor
  ,intf.ZUGFeRDProfile
  ,intf.ZUGFeRDInvoiceTypes
  ,intf.ZUGFeRDCurrencyCodes
  ,intf.ZUGFeRDSubjectCodes
  ,intf.ZUGFeRDQuantityCodes
  ,intf.ZUGFeRDTaxRegistrationSchemeID
  ;

type
  TZUGFeRDInvoiceProvider = class
  public
    class function CreateInvoice : TZUGFeRDInvoiceDescriptor;
  end;

implementation


{ TZUGFeRDInvoiceProvider }

class function TZUGFeRDInvoiceProvider.CreateInvoice: TZUGFeRDInvoiceDescriptor;
begin
  Result := TZUGFeRDInvoiceDescriptor.CreateInvoice('471102', EncodeDate(2018, 03, 05), TZUGFeRDCurrencyCodes.EUR);
  Result.AddNote('Rechnung gemäß Bestellung vom 01.03.2018.');
  Result.AddNote('Lieferant GmbH'+#13#10+'Lieferantenstraße 20'+#13#10+'80333 München'+#13#10+'Deutschland'+#13#10+'Geschäftsführer: Hans Muster'+#13#10+'Handelsregisternummer: H A 123',
                  TZUGFeRDSubjectCodes.REG);

//  Result.AddTradeLineItem('Trennblätter A4',
//                          TZUGFeRDQuantityCodes.H87,
//                          'TB100A4',
//                          id: new GlobalID(GlobalIDSchemeIdentifiers.EAN, "4012345001235"),
//                          grossUnitPrice: 9.9m,
//                          netUnitPrice: 9.9m,
//                          billedQuantity: 20m,
//                          taxType: TaxTypes.VAT,
//                          categoryCode: TaxCategoryCodes.S,
//                          taxPercent: 19m
//                           );
//
//      desc.AddTradeLineItem(name: "Joghurt Banane",
//          unitCode: QuantityCodes.H87,
//          sellerAssignedID: "ARNR2",
//          id: new GlobalID(GlobalIDSchemeIdentifiers.EAN, "4000050986428"),
//          grossUnitPrice: 5.5m,
//          netUnitPrice: 5.5m,
//          billedQuantity: 50,
//          taxType: TaxTypes.VAT,
//          categoryCode: TaxCategoryCodes.S,
//          taxPercent: 7
//          );
//
  Result.ReferenceOrderNo := '04011000-12345-34';
//      desc.SetSeller(name: "Lieferant GmbH",
//                     postcode: "80333",
//                     city: "München",
//                     street: "Lieferantenstraße 20",
//                     country: CountryCodes.DE,
//                     id: "",
//                     globalID: new GlobalID(GlobalIDSchemeIdentifiers.GLN, "4000001123452"),
//                     legalOrganization: new LegalOrganization(GlobalIDSchemeIdentifiers.GLN, "4000001123452", "Lieferant GmbH")
//                     );
//      desc.SetSellerContact(name: "Max Mustermann",
//                            orgunit: "Muster-Einkauf",
//                            emailAddress: "Max@Mustermann.de",
//                            phoneno: "+49891234567"
//                           );
  Result.AddSellerTaxRegistration('201/113/40209', TZUGFeRDTaxRegistrationSchemeID.FC);
  Result.AddSellerTaxRegistration('DE123456789', TZUGFeRDTaxRegistrationSchemeID.VA);
//
//      desc.SetBuyer(name: "Kunden AG Mitte",
//                    postcode: "69876",
//                    city: "Frankfurt",
//                    street: "Kundenstraße 15",
//                    country: CountryCodes.DE,
//                    id: "GE2020211"
//                    );
//
//      desc.ActualDeliveryDate = new DateTime(2018, 03, 05);
//      desc.SetPaymentMeans(PaymentMeansTypeCodes.SEPACreditTransfer, "Zahlung per SEPA Überweisung.");
//      desc.AddCreditorFinancialAccount(iban: "DE02120300000000202051", bic: "BYLADEM1001", name: "Kunden AG");
//      //desc.AddDebitorFinancialAccount(iban: "DB02120300000000202051", bic: "DBBYLADEM1001", bankName: "KundenDB AG");
//
//      desc.AddApplicableTradeTax(basisAmount: 275.0m,
//                                 percent: 7m,
//                                 typeCode: TaxTypes.VAT,
//                                 categoryCode: TaxCategoryCodes.S
//                                 );
//
//      desc.AddApplicableTradeTax(basisAmount: 198.0m,
//                                 percent: 19m,
//                                 typeCode: TaxTypes.VAT,
//                                 categoryCode: TaxCategoryCodes.S
//                                 );
//
//      desc.SetTradePaymentTerms("Zahlbar innerhalb 30 Tagen netto bis 04.04.2018, 3% Skonto innerhalb 10 Tagen bis 15.03.2018");
//      desc.SetTotals(lineTotalAmount: 473.0m,
//                     taxBasisAmount: 473.0m,
//                     taxTotalAmount: 56.87m,
//                     grandTotalAmount: 529.87m,
//                     duePayableAmount: 529.87m
//                    );
//
//      return desc;
//    } // !CreateInvoice()

end;

end.
