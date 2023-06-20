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

unit intf.ZUGFeRDInvoiceDescriptor;

interface

uses
  System.SysUtils,System.Classes,System.Generics.Collections,System.Generics.Defaults,
  intf.ZUGFeRDAdditionalReferencedDocument,
  intf.ZUGFeRDDeliveryNoteReferencedDocument,
  intf.ZUGFeRDContractReferencedDocument,
  intf.ZUGFeRDSpecifiedProcuringProject,
  intf.ZUGFeRDParty,
  intf.ZUGFeRDTaxRegistration,
  intf.ZUGFeRDContact,
  intf.ZUGFeRDNote,
  intf.ZUGFeRDCurrencyCodes,
  intf.ZUGFeRDProfile,
  intf.ZUGFeRDInvoiceTypes,
  intf.ZUGFeRDTradeLineItem,
  intf.ZUGFeRDTax,
  intf.ZUGFeRDServiceCharge,
  intf.ZUGFeRDTradeAllowanceCharge,
  intf.ZUGFeRDPaymentTerms,
  intf.ZUGFeRDInvoiceReferencedDocument,
  intf.ZUGFeRDBankAccount,
  intf.ZUGFeRDReceivableSpecifiedTradeAccountingAccount,
  intf.ZUGFeRDPaymentMeans,
  intf.ZUGFeRDSellerOrderReferencedDocument,
  intf.ZUGFeRDHelper,
  intf.ZUGFeRDVersion,
  intf.ZUGFeRDExceptions
  ;

type
  /// <summary>
  /// Represents a ZUGFeRD/ Factur-X invoice
  /// </summary>
  TZUGFeRDInvoiceDescriptor = class
  private
    FInvoiceNo: string;
    FInvoiceDate: TZUGFeRDNullable<TDateTime>;
    FPaymentReference: string;
    FOrderNo: string;
    FOrderDate: TZUGFeRDNullable<TDateTime>;
    FAdditionalReferencedDocuments: TObjectList<TZUGFeRDAdditionalReferencedDocument>;
    FDeliveryNoteReferencedDocument: TZUGFeRDDeliveryNoteReferencedDocument;
    FActualDeliveryDate: TZUGFeRDNullable<TDateTime>;
    FContractReferencedDocument: TZUGFeRDContractReferencedDocument;
    FSpecifiedProcuringProject: TZUGFeRDSpecifiedProcuringProject;
    FCurrency: TZUGFeRDCurrencyCodes;
    FBuyer: TZUGFeRDParty;
    FBuyerContact: TZUGFeRDContact;
    FBuyerTaxRegistration: TObjectList<TZUGFeRDTaxRegistration>;
    FSeller: TZUGFeRDParty;
    FSellerContact: TZUGFeRDContact;
    FSellerTaxRegistration: TObjectList<TZUGFeRDTaxRegistration>;
    FInvoicee: TZUGFeRDParty;
    FShipTo: TZUGFeRDParty;
    FPayee: TZUGFeRDParty;
    FShipFrom: TZUGFeRDParty;
    FNotes: TObjectList<TZUGFeRDNote>;
    FBusinessProcess: string;
    FIsTest: Boolean;
    FProfile: TZUGFeRDProfile;
    FType: TZUGFeRDInvoiceType;
    FReferenceOrderNo: string;
    FTradeLineItems: TObjectList<TZUGFeRDTradeLineItem>;
    FLineTotalAmount: Currency;
    FChargeTotalAmount: Currency;
    FAllowanceTotalAmount: Currency;
    FTaxBasisAmount: Currency;
    FTaxTotalAmount: Currency;
    FGrandTotalAmount: Currency;
    FTotalPrepaidAmount: Currency;
    FRoundingAmount: Currency;
    FDuePayableAmount: Currency;
    FTaxes: TObjectList<TZUGFeRDTax>;
    FServiceCharges: TObjectList<TZUGFeRDServiceCharge>;
    FTradeAllowanceCharges: TObjectList<TZUGFeRDTradeAllowanceCharge>;
    FPaymentTerms: TZUGFeRDPaymentTerms;
    FInvoiceReferencedDocument: TZUGFeRDInvoiceReferencedDocument;
    FReceivableSpecifiedTradeAccountingAccounts: TObjectList<TZUGFeRDReceivableSpecifiedTradeAccountingAccount>;
    FCreditorBankAccounts: TObjectList<TZUGFeRDBankAccount>;
    FDebitorBankAccounts: TObjectList<TZUGFeRDBankAccount>;
    FPaymentMeans: TZUGFeRDPaymentMeans;
    FBillingPeriodStart: TDateTime;
    FBillingPeriodEnd: TDateTime;
    FSellerOrderReferencedDocument: TZUGFeRDSellerOrderReferencedDocument;
  public
    /// <summary>
    /// Invoice Number
    /// </summary>
    property InvoiceNo: string read FInvoiceNo write FInvoiceNo;

    /// <summary>
    /// Invoice date
    /// </summary>
    property InvoiceDate: TZUGFeRDNullable<TDateTime> read FInvoiceDate write FInvoiceDate;

    /// <summary>
    /// A textual value used to establish a link between the payment and the invoice, issued by the seller.
    /// </summary>
    property PaymentReference: string read FPaymentReference write FPaymentReference;

    /// <summary>
    /// Order Id
    /// </summary>
    property OrderNo: string read FOrderNo write FOrderNo;

    /// <summary>
    /// Order date
    /// </summary>
    property OrderDate: TZUGFeRDNullable<TDateTime> read FOrderDate write FOrderDate;

    /// <summary>
    /// Details of an additional document reference
    ///
    /// A new reference document is added by AddAdditionalReferenceDocument()
    /// </summary>
    property AdditionalReferencedDocuments: TObjectList<TZUGFeRDAdditionalReferencedDocument> read FAdditionalReferencedDocuments;

    /// <summary>
    /// Detailed information about the corresponding delivery note
    /// </summary>
    property DeliveryNoteReferencedDocument: TZUGFeRDDeliveryNoteReferencedDocument read FDeliveryNoteReferencedDocument write FDeliveryNoteReferencedDocument;

    /// <summary>
    /// Actual delivery date
    /// </summary>
    property ActualDeliveryDate: TZUGFeRDNullable<TDateTime> read FActualDeliveryDate write FActualDeliveryDate;

    /// <summary>
    /// Detailed information on the associated contract
    ///
    /// BT-12
    /// </summary>
    property ContractReferencedDocument: TZUGFeRDContractReferencedDocument read FContractReferencedDocument write FContractReferencedDocument;

    /// <summary>
    /// Details about a project reference
    /// </summary>
    property SpecifiedProcuringProject: TZUGFeRDSpecifiedProcuringProject read FSpecifiedProcuringProject write FSpecifiedProcuringProject;

    /// <summary>
    /// Currency of the invoice
    /// </summary>
    property Currency: TZUGFeRDCurrencyCodes read FCurrency write FCurrency;

    /// <summary>
    /// Information about the buyer
    /// </summary>
    property Buyer: TZUGFeRDParty read FBuyer write FBuyer;

    /// <summary>
    /// Buyer contact information
    ///
    /// A group of business terms providing contact information relevant for the buyer.
    /// </summary>
    property BuyerContact: TZUGFeRDContact read FBuyerContact write FBuyerContact;

    property BuyerTaxRegistration: TObjectList<TZUGFeRDTaxRegistration> read FBuyerTaxRegistration;
    property Seller: TZUGFeRDParty read FSeller write FSeller;
    property SellerContact: TZUGFeRDContact read FSellerContact write FSellerContact;
    property SellerTaxRegistration: TObjectList<TZUGFeRDTaxRegistration> read FSellerTaxRegistration;

    /// <summary>
    /// This party is optional and only relevant for Extended profile
    /// </summary>
    property Invoicee: TZUGFeRDParty read FInvoicee write FInvoicee;

    /// <summary>
    /// This party is optional and only relevant for Extended profile
    /// </summary>
    property ShipTo: TZUGFeRDParty read FShipTo write FShipTo;

    /// <summary>
    /// This party is optional and only relevant for Extended profile
    /// </summary>
    property Payee: TZUGFeRDParty read FPayee write FPayee;

    /// <summary>
    /// This party is optional and only relevant for Extended profile
    /// </summary>
    property ShipFrom: TZUGFeRDParty read FShipFrom write FShipFrom;

    /// <summary>
    /// Free text on header level
    /// </summary>
    property Notes: TObjectList<TZUGFeRDNote> read FNotes;

    /// <summary>
    /// Description: Identifies the context of a business process where the transaction is taking place,
    /// thus allowing the buyer to process the invoice in an appropriate manner.
    ///
    /// Note: These data make it possible to define the purpose of the settlement(invoice of the authorised person,
    /// contractual partner, subcontractor, settlement document for a building contract etc.).
    ///
    /// BT-23
    /// </summary>
    property BusinessProcess: string read FBusinessProcess write FBusinessProcess;

    /// <summary>
    /// The Indicator type may be used when implementing a new system in order to mark the invoice as „trial invoice“.
    /// </summary>
    property IsTest: Boolean read FIsTest write FIsTest;

    /// <summary>
    /// Representation of information that should be used for the document.
    ///
    /// As the library can be used to both write ZUGFeRD files and read ZUGFeRD files, the profile serves two purposes:
    /// It indicates the profile that was used to write the ZUGFeRD file that was loaded or the profile that is to be used when
    /// the document is saved.
    /// </summary>
    property Profile: TZUGFeRDProfile read FProfile default TZUGFeRDProfile.Basic;

    /// <summary>
    /// Indicates the type of the document, if it represents an invoice, a credit note or one of the available 'sub types'
    /// </summary>
    property Type_: TZUGFeRDInvoiceType read FType write FType default TZUGFeRDInvoiceType.Invoice;

    /// <summary>
    /// The identifier is defined by the buyer (e.g. contact ID, department, office ID, project code), but provided by the seller in the invoice.
    /// In France it needs to be filled with 999, if not available.
    ///
    /// BT-10
    /// </summary>
    property ReferenceOrderNo: string read FReferenceOrderNo write FReferenceOrderNo;

    /// <summary>
    /// An aggregation of business terms containing information about individual invoice positions
    /// </summary>
    property TradeLineItems: TObjectList<TZUGFeRDTradeLineItem> read FTradeLineItems;

    /// <summary>
    /// Sum of all invoice line net amounts in the invoice
    /// </summary>
    property LineTotalAmount: Currency read FLineTotalAmount write FLineTotalAmount;

    /// <summary>
    /// Sum of all surcharges on document level in the invoice
    ///
    /// Surcharges on line level are included in the invoice line net amount which is summed up into the sum of invoice line net amount.
    /// </summary>
    property ChargeTotalAmount: Currency read FChargeTotalAmount write FChargeTotalAmount;

    /// <summary>
    /// Sum of discounts on document level in the invoice
    ///
    /// Discounts on line level are included in the invoice line net amount which is summed up into the sum of invoice line net amount.
    /// </summary>
    property AllowanceTotalAmount: Currency read FAllowanceTotalAmount write FAllowanceTotalAmount;

    /// <summary>
    /// The total amount of the invoice without VAT.
    ///
    /// The invoice total amount without VAT is the sum of invoice line net amount minus sum of discounts on document level plus sum of surcharges on document level.
    /// </summary>
    property TaxBasisAmount: Currency read FTaxBasisAmount write FTaxBasisAmount;

    /// <summary>
    /// The total VAT amount for the invoice.
    /// The VAT total amount expressed in the accounting currency accepted or required in the country of the seller
    ///
    /// To be used when the VAT accounting currency (BT-6) differs from the Invoice currency code (BT-5) in accordance
    /// with article 230 of Directive 2006/112 / EC on VAT. The VAT amount in accounting currency is not used
    /// in the calculation of the Invoice totals..
    /// </summary>
    property TaxTotalAmount: Currency read FTaxTotalAmount write FTaxTotalAmount;

    /// <summary>
    /// Invoice total amount with VAT
    ///
    /// The invoice total amount with VAT is the invoice without VAT plus the invoice total VAT amount.
    /// </summary>
    property GrandTotalAmount: Currency read FGrandTotalAmount write FGrandTotalAmount;

    /// <summary>
    /// Sum of amount paid in advance
    ///
    /// This amount is subtracted from the invoice total amount with VAT to calculate the amount due for payment.
    /// </summary>
    property TotalPrepaidAmount: Currency read FTotalPrepaidAmount write FTotalPrepaidAmount;

    /// <summary>
    /// The amount to be added to the invoice total to round the amount to be paid.
    /// </summary>
    property RoundingAmount: Currency read FRoundingAmount write FRoundingAmount;

    /// <summary>
    /// The outstanding amount that is requested to be paid.
    ///
    /// This amount is the invoice total amount with VAT minus the paid amount that has
    /// been paid in advance. The amount is zero in case of a fully paid invoice.
    /// The amount may be negative; in that case the seller owes the amount to the buyer.
    /// </summary>
    property DuePayableAmount: Currency read FDuePayableAmount write FDuePayableAmount;

    /// <summary>
    /// A group of business terms providing information about VAT breakdown by different categories, rates and exemption reasons
    /// </summary>
    property Taxes: TObjectList<TZUGFeRDTax> read FTaxes;

    /// <summary>
    /// Transport and packaging costs
    /// </summary>
    property ServiceCharges: TObjectList<TZUGFeRDServiceCharge> read FServiceCharges;

    /// <summary>
    /// Detailed information on discounts and charges
    /// </summary>
    property TradeAllowanceCharges: TObjectList<TZUGFeRDTradeAllowanceCharge> read FTradeAllowanceCharges;

    /// <summary>
    /// Detailed information about payment terms
    /// </summary>
    property PaymentTerms: TZUGFeRDPaymentTerms read FPaymentTerms write FPaymentTerms;

    /// <summary>
    /// A group of business terms providing information about a preceding invoices.
    ///
    /// To be used in case:
    /// — a preceding invoice is corrected;
    /// — preceding partial invoices are referred to from a final invoice;
    /// — preceding pre-payment invoices are referred to from a final invoice.
    /// </summary>
    property InvoiceReferencedDocument: TZUGFeRDInvoiceReferencedDocument read FInvoiceReferencedDocument write FInvoiceReferencedDocument;

    /// <summary>
    /// Detailed information about the accounting reference
    /// </summary>
    property ReceivableSpecifiedTradeAccountingAccounts: TObjectList<TZUGFeRDReceivableSpecifiedTradeAccountingAccount> read FReceivableSpecifiedTradeAccountingAccounts;

    /// <summary>
    /// Credit Transfer
    ///
    /// A group of business terms to specify credit transfer payments
    /// </summary>
    property CreditorBankAccounts: TObjectList<TZUGFeRDBankAccount> read FCreditorBankAccounts;

    /// <summary>
    /// Buyer bank information
    /// </summary>
    property DebitorBankAccounts: TObjectList<TZUGFeRDBankAccount> read FDebitorBankAccounts;

    /// <summary>
    /// Payment instructions
    ///
    /// /// If various accounts for credit transfers shall be transferred, the element
    /// SpecifiedTradeSettlementPaymentMeans can be repeated for each account. The code
    /// for the type of payment within the element typecode (BT-81) should therefore not
    /// differ within the repetitions.
    /// </summary>
    property PaymentMeans: TZUGFeRDPaymentMeans read FPaymentMeans write FPaymentMeans;

    /// <summary>
    /// Detailed information about the invoicing period, start date
    /// </summary>
    property BillingPeriodStart: TDateTime read FBillingPeriodStart write FBillingPeriodStart;

    /// <summary>
    /// Detailed information about the invoicing period, end date
    /// </summary>
    property BillingPeriodEnd: TDateTime read FBillingPeriodEnd write FBillingPeriodEnd;

    /// <summary>
    /// Details about the associated order confirmation (BT-14).
    /// This is optional and can be used in Profiles Comfort and Extended.
    /// If you add a SellerOrderReferencedDocument you must set the property "ID".
    /// The property "IssueDateTime" is optional an only used in profile "Extended"
    /// </summary>
    property SellerOrderReferencedDocument: TZUGFeRDSellerOrderReferencedDocument read FSellerOrderReferencedDocument write FSellerOrderReferencedDocument;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Gets the ZUGFeRD version of a ZUGFeRD invoice that is passed via filename
    /// </summary>
    /// <param name="filename">Stream where to read the ZUGFeRD invoice</param>
    /// <returns>ZUGFeRD version of the invoice that was passed to the function</returns>
    class function GetVersion(const filename: string): TZUGFeRDVersion; overload;

    /// <summary>
    /// Gets the ZUGFeRD version of a ZUGFeRD invoice that is passed via stream
    ///
    /// </summary>
    /// <param name="stream">Stream where to read the ZUGFeRD invoice</param>
    /// <returns>ZUGFeRD version of the invoice that was passed to the function</returns>
    class function GetVersion(const stream: TStream): TZUGFeRDVersion; overload;
  end;

implementation

{ TZUGFeRDInvoiceDescriptor }

constructor TZUGFeRDInvoiceDescriptor.Create;
begin
  FInvoiceDate := TZUGFeRDNullable<TDateTime>.Create;
  FOrderDate := TZUGFeRDNullable<TDateTime>.Create;
  FActualDeliveryDate := TZUGFeRDNullable<TDateTime>.Create;
  FAdditionalReferencedDocuments := TObjectList<TZUGFeRDAdditionalReferencedDocument>.Create;
  FDeliveryNoteReferencedDocument:= nil;//TZUGFeRDDeliveryNoteReferencedDocument.Create;
  FContractReferencedDocument    := TZUGFeRDContractReferencedDocument.Create;
  FSpecifiedProcuringProject     := TZUGFeRDSpecifiedProcuringProject.Create;
  FBuyer                         := TZUGFeRDParty.Create;
  FBuyerContact                  := TZUGFeRDContact.Create;
  FBuyerTaxRegistration          := TObjectList<TZUGFeRDTaxRegistration>.Create;
  FSeller                        := TZUGFeRDParty.Create;
  FSellerContact                 := TZUGFeRDContact.Create;
  FSellerTaxRegistration         := TObjectList<TZUGFeRDTaxRegistration>.Create;
  FInvoicee                      := TZUGFeRDParty.Create;
  FShipTo                        := TZUGFeRDParty.Create;
  FPayee                         := TZUGFeRDParty.Create;
  FShipFrom                      := TZUGFeRDParty.Create;
  FNotes                         := TObjectList<TZUGFeRDNote>.Create;
  FTradeLineItems                := TObjectList<TZUGFeRDTradeLineItem>.Create;
  FTaxes                         := TObjectList<TZUGFeRDTax>.Create;
  FServiceCharges                := TObjectList<TZUGFeRDServiceCharge>.Create;
  FTradeAllowanceCharges         := TObjectList<TZUGFeRDTradeAllowanceCharge>.Create;
  FPaymentTerms                  := TZUGFeRDPaymentTerms.Create;
  FInvoiceReferencedDocument     := TZUGFeRDInvoiceReferencedDocument.Create;
  FReceivableSpecifiedTradeAccountingAccounts:= TObjectList<TZUGFeRDReceivableSpecifiedTradeAccountingAccount>.Create;
  FCreditorBankAccounts          := TObjectList<TZUGFeRDBankAccount>.Create;
  FDebitorBankAccounts           := TObjectList<TZUGFeRDBankAccount>.Create;
  FPaymentMeans                  := TZUGFeRDPaymentMeans.Create;
  FSellerOrderReferencedDocument := TZUGFeRDSellerOrderReferencedDocument.Create;
end;

destructor TZUGFeRDInvoiceDescriptor.Destroy;
begin
  if Assigned(FInvoiceDate) then begin FInvoiceDate.Free; FInvoiceDate := nil; end;
  if Assigned(FOrderDate) then begin FOrderDate.Free; FOrderDate := nil; end;
  if Assigned(FActualDeliveryDate) then begin FActualDeliveryDate.Free; FActualDeliveryDate := nil; end;
  if Assigned(FAdditionalReferencedDocuments ) then begin FAdditionalReferencedDocuments.Free; FAdditionalReferencedDocuments  := nil; end;
  if Assigned(FDeliveryNoteReferencedDocument) then begin FDeliveryNoteReferencedDocument.Free; FDeliveryNoteReferencedDocument := nil; end;
  if Assigned(FContractReferencedDocument    ) then begin FContractReferencedDocument.Free; FContractReferencedDocument     := nil; end;
  if Assigned(FSpecifiedProcuringProject     ) then begin FSpecifiedProcuringProject.Free; FSpecifiedProcuringProject      := nil; end;
  if Assigned(FBuyer                         ) then begin FBuyer.Free; FBuyer                          := nil; end;
  if Assigned(FBuyerContact                  ) then begin FBuyerContact.Free; FBuyerContact                   := nil; end;
  if Assigned(FBuyerTaxRegistration          ) then begin FBuyerTaxRegistration.Free; FBuyerTaxRegistration           := nil; end;
  if Assigned(FSeller                        ) then begin FSeller.Free; FSeller                         := nil; end;
  if Assigned(FSellerContact                 ) then begin FSellerContact.Free; FSellerContact                  := nil; end;
  if Assigned(FSellerTaxRegistration         ) then begin FSellerTaxRegistration.Free; FSellerTaxRegistration          := nil; end;
  if Assigned(FInvoicee                      ) then begin FInvoicee.Free; FInvoicee                       := nil; end;
  if Assigned(FShipTo                        ) then begin FShipTo.Free; FShipTo                         := nil; end;
  if Assigned(FPayee                         ) then begin FPayee.Free; FPayee                          := nil; end;
  if Assigned(FShipFrom                      ) then begin FShipFrom.Free; FShipFrom                       := nil; end;
  if Assigned(FNotes                         ) then begin FNotes.Free; FNotes                          := nil; end;
  if Assigned(FTradeLineItems                ) then begin FTradeLineItems.Free; FTradeLineItems                 := nil; end;
  if Assigned(FTaxes                         ) then begin FTaxes.Free; FTaxes                          := nil; end;
  if Assigned(FServiceCharges                ) then begin FServiceCharges.Free; FServiceCharges                 := nil; end;
  if Assigned(FTradeAllowanceCharges         ) then begin FTradeAllowanceCharges.Free; FTradeAllowanceCharges          := nil; end;
  if Assigned(FPaymentTerms                  ) then begin FPaymentTerms.Free; FPaymentTerms                   := nil; end;
  if Assigned(FInvoiceReferencedDocument     ) then begin FInvoiceReferencedDocument.Free; FInvoiceReferencedDocument      := nil; end;
  if Assigned(FReceivableSpecifiedTradeAccountingAccounts) then begin FReceivableSpecifiedTradeAccountingAccounts.Free; FReceivableSpecifiedTradeAccountingAccounts := nil; end;
  if Assigned(FCreditorBankAccounts         ) then begin FCreditorBankAccounts.Free; FCreditorBankAccounts          := nil; end;
  if Assigned(FDebitorBankAccounts          ) then begin FDebitorBankAccounts.Free; FDebitorBankAccounts           := nil; end;
  if Assigned(FPaymentMeans                 ) then begin FPaymentMeans.Free; FPaymentMeans                  := nil; end;
  if Assigned(FSellerOrderReferencedDocument) then begin FSellerOrderReferencedDocument.Free; FSellerOrderReferencedDocument := nil; end;
  inherited;
end;

class function TZUGFeRDInvoiceDescriptor.GetVersion(const filename: string): TZUGFeRDVersion;
//var
//  reader: IInvoiceDescriptorReader;
begin
//  reader := TZUGFeRDInvoiceDescriptor1Reader.Create;
//  if reader.IsReadableByThisReaderVersion(filename) then
//  begin
//    Result := TZUGFeRDVersion.Version1;
//    Exit;
//  end;
//
//  reader := TZUGFeRDInvoiceDescriptor21Reader.Create;
//  if reader.IsReadableByThisReaderVersion(filename) then
//  begin
//    Result := TZUGFeRDVersion.Version21;
//    Exit;
//  end;
//
//  reader := TZUGFeRDInvoiceDescriptor20Reader.Create;
//  if reader.IsReadableByThisReaderVersion(filename) then
//  begin
//    Result := TZUGFeRDVersion.Version20;
//    Exit;
//  end;

  raise TZUGFeRDUnsupportedException.Create('No ZUGFeRD invoice reader was able to parse this file "' + filename + '"!');
end;

class function TZUGFeRDInvoiceDescriptor.GetVersion(
  const stream: TStream): TZUGFeRDVersion;
//var
//  reader: IInvoiceDescriptorReader;
begin
//  reader := TZUGFeRDInvoiceDescriptor1Reader.Create;
//  if reader.IsReadableByThisReaderVersion(stream) then
//  begin
//    Result := TZUGFeRDVersion.Version1;
//    Exit;
//  end;
//
//  reader := TZUGFeRDInvoiceDescriptor21Reader.Create;
//  if reader.IsReadableByThisReaderVersion(stream) then
//  begin
//    Result := TZUGFeRDVersion.Version21;
//    Exit;
//  end;
//
//  reader := TZUGFeRDInvoiceDescriptor20Reader.Create;
//  if reader.IsReadableByThisReaderVersion(stream) then
//  begin
//    Result := TZUGFeRDVersion.Version20;
//    Exit;
//  end;

  raise TZUGFeRDUnsupportedException.Create('No ZUGFeRD invoice reader was able to parse this stream!');
end;

end.
