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
  intf.ZUGFeRDAccountingAccountTypeCodes,
  intf.ZUGFeRDAssociatedDocument,
  intf.ZUGFeRDContractReferencedDocument,
  intf.ZUGFeRDSpecifiedProcuringProject,
  intf.ZUGFeRDParty,
  intf.ZUGFeRDQuantityCodes,
  intf.ZUGFeRDPaymentMeansTypeCodes,
  intf.ZUGFeRDAdditionalReferencedDocumentTypeCodes,
  intf.ZUGFeRDReferenceTypeCodes,
  intf.ZUGFeRDGlobalID,intf.ZUGFeRDGlobalIDSchemeIdentifiers,
  intf.ZUGFeRDTaxRegistration,
  intf.ZUGFeRDTaxRegistrationSchemeID,
  intf.ZUGFeRDTaxTypes,
  intf.ZUGFeRDTaxCategoryCodes,
  intf.ZUGFeRDTaxExemptionReasonCodes,
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
  intf.ZUGFeRDExceptions,
  intf.ZUGFeRDSubjectCodes,
  intf.ZUGFeRDContentCodes,
  intf.ZUGFeRDCountryCodes,
  intf.ZUGFeRDLegalOrganization,
  intf.ZUGFeRDElectronicAddress,
  intf.ZUGFeRDElectronicAddressSchemeIdentifiers,
  intf.ZUGFeRDDespatchAdviceReferencedDocument,
  intf.ZUGFeRDSpecialServiceDescriptionCodes,
  intf.ZUGFeRDAllowanceOrChargeIdentificationCodes
  ;

type
  /// <summary>
  /// Represents a ZUGFeRD/ Factur-X invoice
  /// </summary>
  TZUGFeRDInvoiceDescriptor = class
  private
    FInvoiceNo: string;
    FInvoiceDate: TDateTime;
    FPaymentReference: string;
    FOrderNo: string;
    FOrderDate: Nullable<TDateTime>;
    FAdditionalReferencedDocuments: TObjectList<TZUGFeRDAdditionalReferencedDocument>;
    FDeliveryNoteReferencedDocument: TZUGFeRDDeliveryNoteReferencedDocument;
    FActualDeliveryDate: Nullable<TDateTime>;
    FContractReferencedDocument: TZUGFeRDContractReferencedDocument;
    FSpecifiedProcuringProject: TZUGFeRDSpecifiedProcuringProject;
    FCurrency: TZUGFeRDCurrencyCodes;
    FBuyer: TZUGFeRDParty;
    FBuyerContact: TZUGFeRDContact;
    FBuyerTaxRegistration: TObjectList<TZUGFeRDTaxRegistration>;
    FBuyerElectronicAddress: TZUGFeRDElectronicAddress;
    FSeller: TZUGFeRDParty;
    FSellerContact: TZUGFeRDContact;
    FSellerTaxRegistration: TObjectList<TZUGFeRDTaxRegistration>;
    FSellerElectronicAddress: TZUGFeRDElectronicAddress;
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
    FLineTotalAmount: Nullable<Currency>;
    FChargeTotalAmount: Nullable<Currency>;
    FAllowanceTotalAmount: Nullable<Currency>;
    FTaxBasisAmount: Nullable<Currency>;
    FTaxTotalAmount: Nullable<Currency>;
    FGrandTotalAmount: Nullable<Currency>;
    FTotalPrepaidAmount: Nullable<Currency>;
    FRoundingAmount: Nullable<Currency>;
    FDuePayableAmount: Nullable<Currency>;
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
    FDespatchAdviceReferencedDocument: TZUGFeRDDespatchAdviceReferencedDocument;
  public
    /// <summary>
    /// Invoice Number
    /// </summary>
    property InvoiceNo: string read FInvoiceNo write FInvoiceNo;

    /// <summary>
    /// Invoice date
    /// </summary>
    property InvoiceDate: TDateTime read FInvoiceDate write FInvoiceDate;

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
    property OrderDate: Nullable<TDateTime> read FOrderDate write FOrderDate;

    /// <summary>
    /// Details of an additional document reference
    ///
    /// A new reference document is added by AddAdditionalReferenceDocument()
    /// </summary>
    property AdditionalReferencedDocuments: TObjectList<TZUGFeRDAdditionalReferencedDocument> read FAdditionalReferencedDocuments;

    /// <summary>
    /// Detailed information about the corresponding despatch advice
    /// </summary>
    property DespatchAdviceReferencedDocument : TZUGFeRDDespatchAdviceReferencedDocument read FDespatchAdviceReferencedDocument write FDespatchAdviceReferencedDocument;

    /// <summary>
    /// Detailed information about the corresponding delivery note
    /// </summary>
    property DeliveryNoteReferencedDocument: TZUGFeRDDeliveryNoteReferencedDocument read FDeliveryNoteReferencedDocument write FDeliveryNoteReferencedDocument;

    /// <summary>
    /// Actual delivery date
    /// </summary>
    property ActualDeliveryDate: Nullable<TDateTime> read FActualDeliveryDate write FActualDeliveryDate;

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
    property BuyerElectronicAddress : TZUGFeRDElectronicAddress read FBuyerElectronicAddress;
    property Seller: TZUGFeRDParty read FSeller write FSeller;
    property SellerContact: TZUGFeRDContact read FSellerContact write FSellerContact;
    property SellerTaxRegistration: TObjectList<TZUGFeRDTaxRegistration> read FSellerTaxRegistration;
    property SellerElectronicAddress : TZUGFeRDElectronicAddress read FSellerElectronicAddress;

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
    property Profile: TZUGFeRDProfile read FProfile write FProfile default TZUGFeRDProfile.Basic;

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
    property LineTotalAmount: Nullable<Currency> read FLineTotalAmount write FLineTotalAmount;

    /// <summary>
    /// Sum of all surcharges on document level in the invoice
    ///
    /// Surcharges on line level are included in the invoice line net amount which is summed up into the sum of invoice line net amount.
    /// </summary>
    property ChargeTotalAmount: Nullable<Currency> read FChargeTotalAmount write FChargeTotalAmount;

    /// <summary>
    /// Sum of discounts on document level in the invoice
    ///
    /// Discounts on line level are included in the invoice line net amount which is summed up into the sum of invoice line net amount.
    /// </summary>
    property AllowanceTotalAmount: Nullable<Currency> read FAllowanceTotalAmount write FAllowanceTotalAmount;

    /// <summary>
    /// The total amount of the invoice without VAT.
    ///
    /// The invoice total amount without VAT is the sum of invoice line net amount minus sum of discounts on document level plus sum of surcharges on document level.
    /// </summary>
    property TaxBasisAmount: Nullable<Currency> read FTaxBasisAmount write FTaxBasisAmount;

    /// <summary>
    /// The total VAT amount for the invoice.
    /// The VAT total amount expressed in the accounting currency accepted or required in the country of the seller
    ///
    /// To be used when the VAT accounting currency (BT-6) differs from the Invoice currency code (BT-5) in accordance
    /// with article 230 of Directive 2006/112 / EC on VAT. The VAT amount in accounting currency is not used
    /// in the calculation of the Invoice totals..
    /// </summary>
    property TaxTotalAmount: Nullable<Currency> read FTaxTotalAmount write FTaxTotalAmount;

    /// <summary>
    /// Invoice total amount with VAT
    ///
    /// The invoice total amount with VAT is the invoice without VAT plus the invoice total VAT amount.
    /// </summary>
    property GrandTotalAmount: Nullable<Currency> read FGrandTotalAmount write FGrandTotalAmount;

    /// <summary>
    /// Sum of amount paid in advance
    ///
    /// This amount is subtracted from the invoice total amount with VAT to calculate the amount due for payment.
    /// </summary>
    property TotalPrepaidAmount: Nullable<Currency> read FTotalPrepaidAmount write FTotalPrepaidAmount;

    /// <summary>
    /// The amount to be added to the invoice total to round the amount to be paid.
    /// </summary>
    property RoundingAmount: Nullable<Currency> read FRoundingAmount write FRoundingAmount;

    /// <summary>
    /// The outstanding amount that is requested to be paid.
    ///
    /// This amount is the invoice total amount with VAT minus the paid amount that has
    /// been paid in advance. The amount is zero in case of a fully paid invoice.
    /// The amount may be negative; in that case the seller owes the amount to the buyer.
    /// </summary>
    property DuePayableAmount: Nullable<Currency> read FDuePayableAmount write FDuePayableAmount;

    /// <summary>
    /// A group of business terms providing information about VAT breakdown by different categories, rates and exemption reasons
    /// </summary>
    property Taxes: TObjectList<TZUGFeRDTax> read FTaxes;

    /// <summary>
    /// Transport and packaging costs
    /// </summary>
    property ServiceCharges: TObjectList<TZUGFeRDServiceCharge> read FServiceCharges;

    /// <summary>
    /// Detailed information on discounts and charges.
    /// This field is marked as private now, please use GetTradeAllowanceCharges() to retrieve all trade allowance charges
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

    /// <summary>
    /// Loads a ZUGFeRD invoice from a stream.
    ///
    /// Please make sure that the stream is open, otherwise this call will raise an IllegalStreamException.
    ///
    /// Important: the stream will not be closed by this function, make sure to close it by yourself!
    ///
    /// </summary>
    /// <param name="stream">Stream where to read the ZUGFeRD invoice</param>
    /// <returns></returns>
    class function Load(stream: TStream): TZUGFeRDInvoiceDescriptor; overload;

    /// <summary>
    /// Loads a ZUGFeRD invoice from a file.
    ///
    /// Please make sure that the file is exists, otherwise this call will raise a FileNotFoundException.
    /// </summary>
    /// <param name="filename">Name of the ZUGFeRD invoice file</param>
    /// <returns></returns>
    class function Load(filename: String): TZUGFeRDInvoiceDescriptor; overload;

    /// <summary>
    /// Initializes a new invoice object and returns it.
    /// </summary>
    /// <param name="invoiceNo">Invoice number</param>
    /// <param name="invoiceDate">Invoice date</param>
    /// <param name="currency">Currency</param>
    /// <param name="invoiceNoAsReference">Remittance information</param>
    /// <returns></returns>
    class function CreateInvoice(const invoiceNo: string; invoiceDate: TDateTime;
                               currency: TZUGFeRDCurrencyCodes;
                               const invoiceNoAsReference: string = ''): TZUGFeRDInvoiceDescriptor;

    procedure AddNote(const note: string; subjectCode: TZUGFeRDSubjectCodes = TZUGFeRDSubjectCodes.Unknown; contentCode: TZUGFeRDContentCodes = TZUGFeRDContentCodes.Unknown);

    procedure SetBuyer(const name, postcode, city, street: string; country: TZUGFeRDCountryCodes; const id: string = '';
                     globalID: TZUGFeRDGlobalID = nil; const receiver: string = ''; legalOrganization: TZUGFeRDLegalOrganization = nil);

    procedure SetSeller(const name, postcode, city, street: string; country: TZUGFeRDCountryCodes; const id: string = '';
                     globalID: TZUGFeRDGlobalID = nil; legalOrganization: TZUGFeRDLegalOrganization = nil);

    procedure SetSellerContact(const name: string = ''; const orgunit: string = '';
  const emailAddress: string = ''; const phoneno: string = ''; const faxno: string = '');

    procedure SetBuyerContact(const name: string; const orgunit: string = '';
  const emailAddress: string = ''; const phoneno: string = ''; const faxno: string = '');

    /// <summary>
    /// Sets the SpecifiedProcuringProject
    /// </summary>
    /// <param name="id">ProjectId</param>
    /// <param name="name">ProjectName</param>
    procedure SetSpecifiedProcuringProject(const id, name: string);

    procedure AddBuyerTaxRegistration(const no: string; const schemeID: TZUGFeRDTaxRegistrationSchemeID);

    procedure AddSellerTaxRegistration(const no: string; const schemeID: TZUGFeRDTaxRegistrationSchemeID);

    /// <summary>
    /// Sets the Buyer Electronic Address for Peppol
    /// </summary>
    /// <param name="address">Peppol Address</param>
    /// <param name="electronicAddressSchemeID">ElectronicAddressSchemeIdentifier</param>
    procedure SetBuyerElectronicAddress(address : string; electronicAddressSchemeID : TZUGFeRDElectronicAddressSchemeIdentifiers);

    /// <summary>
    /// Sets the Seller Electronic Address for Peppol
    /// </summary>
    /// <param name="address">Peppol Address</param>
    /// <param name="electronicAddressSchemeID">ElectronicAddressSchemeIdentifier</param>
    procedure SetSellerElectronicAddress(address : string; electronicAddressSchemeID : TZUGFeRDElectronicAddressSchemeIdentifiers);

    /// <summary>
    /// Add an additional reference document
    /// </summary>
    /// <param name="id">Document number such as delivery note no or credit memo no</param>
    /// <param name="typeCode"></param>
    /// <param name="issueDateTime">Document Date</param>
    /// <param name="name"></param>
    /// <param name="referenceTypeCode">Type of the referenced document</param>
    /// <param name="attachmentBinaryObject"></param>
    /// <param name="filename"></param>
    procedure AddAdditionalReferencedDocument(const id: string; const typeCode: TZUGFeRDAdditionalReferencedDocumentTypeCode;
  const issueDateTime: TDateTime = 0; const name: string = ''; const referenceTypeCode: TZUGFeRDReferenceTypeCodes = TZUGFeRDReferenceTypeCodes.Unknown;
  const attachmentBinaryObject: TMemoryStream = nil; const filename: string = '');

    /// <summary>
    /// Sets details of the associated order
    /// </summary>
    /// <param name="orderNo"></param>
    /// <param name="orderDate"></param>
    procedure SetBuyerOrderReferenceDocument(const orderNo: string; const orderDate: TDateTime = 0);

    /// <summary>
    /// Sets detailed information about the corresponding despatch advice
    /// </summary>
    /// <param name="deliveryNoteNo"></param>
    /// <param name="deliveryNoteDate"></param>
    procedure SetDespatchAdviceReferencedDocument(despatchAdviceNo : String; despatchAdviceDate: TDateTime = 0);

    /// <summary>
    /// Sets detailed information about the corresponding delivery note
    /// </summary>
    /// <param name="deliveryNoteNo"></param>
    /// <param name="deliveryNoteDate"></param>
    procedure SetDeliveryNoteReferenceDocument(const deliveryNoteNo: string; const deliveryNoteDate: TDateTime = 0);

    /// <summary>
    /// Sets detailed information about the corresponding contract
    /// </summary>
    /// <param name="contractNo">Contract number</param>
    /// <param name="contractDate">Date of the contract</param>
    procedure SetContractReferencedDocument(const contractNo: string; const contractDate: TDateTime);

    procedure AddLogisticsServiceCharge(const amount: Currency; const description: string; const taxTypeCode: TZUGFeRDTaxTypes; const taxCategoryCode: TZUGFeRDTaxCategoryCodes; const taxPercent: Currency);

    /// <summary>
    /// Adds an allowance or charge on document level.
    ///
    /// Allowance represents a discount whereas charge represents a surcharge.
    /// </summary>
    /// <param name="isDiscount">Marks if the allowance charge is a discount. Please note that in contrary to this function, the xml file indicated a surcharge, not a discount (value will be inverted)</param>
    /// <param name="basisAmount">Base amount (basis of allowance)</param>
    /// <param name="currency">Curency of the allowance</param>
    /// <param name="actualAmount">Actual allowance charge amount</param>
    /// <param name="reason">Reason for the allowance</param>
    /// <param name="reasonCodeCharge"></param>
    /// <param name="reasonCodeAllowance"></param>
    /// <param name="taxTypeCode">VAT type code for document level allowance/ charge</param>
    /// <param name="taxCategoryCode">VAT type code for document level allowance/ charge</param>
    /// <param name="taxPercent">VAT rate for the allowance</param>
    procedure AddTradeAllowanceCharge(const isDiscount: Boolean;
             const basisAmount: Currency; const currency: TZUGFeRDCurrencyCodes;
             const actualAmount: Currency; const reason: string;
             const reasonCodeCharge : TZUGFeRDSpecialServiceDescriptionCodes;
             const reasonCodeAllowance : TZUGFeRDAllowanceOrChargeIdentificationCodes;
             const taxTypeCode: TZUGFeRDTaxTypes;
             const taxCategoryCode: TZUGFeRDTaxCategoryCodes;
             const taxPercent: Currency); overload;

    /// <summary>
    /// Adds an allowance or charge on document level.
    ///
    /// Allowance represents a discount whereas charge represents a surcharge.
    /// </summary>
    /// <param name="isDiscount">Marks if the allowance charge is a discount. Please note that in contrary to this function, the xml file indicated a surcharge, not a discount (value will be inverted)</param>
    /// <param name="basisAmount">Base amount (basis of allowance)</param>
    /// <param name="currency">Curency of the allowance</param>
    /// <param name="actualAmount">Actual allowance charge amount</param>
    /// <param name="chargePercentage">Actual allowance charge percentage</param>
    /// <param name="reason">Reason for the allowance</param>
    /// <param name="reasonCodeCharge"></param>
    /// <param name="reasonCodeAllowance"></param>
    /// <param name="taxTypeCode">VAT type code for document level allowance/ charge</param>
    /// <param name="taxCategoryCode">VAT type code for document level allowance/ charge</param>
    /// <param name="taxPercent">VAT rate for the allowance</param>
    procedure AddTradeAllowanceCharge(const isDiscount: Boolean;
             const basisAmount: Currency; const currency: TZUGFeRDCurrencyCodes;
             const actualAmount: Currency; const chargePercentage : Currency;
             const reason: string;
             const reasonCodeCharge : TZUGFeRDSpecialServiceDescriptionCodes;
             const reasonCodeAllowance : TZUGFeRDAllowanceOrChargeIdentificationCodes;
             const taxTypeCode: TZUGFeRDTaxTypes;
             const taxCategoryCode: TZUGFeRDTaxCategoryCodes;
             const taxPercent: Currency); overload;

    procedure SetTradePaymentTerms(const description: string; const dueDate: TDateTime = 0);

    /// <summary>
    /// Set Information about Preceding Invoice
    /// </summary>
    /// <param name="id">Preceding InvoiceNo</param>
    /// <param name="IssueDateTime">Preceding Invoice Date</param>
    procedure SetInvoiceReferencedDocument(const id: string; const IssueDateTime: TDateTime = 0);

    /// <summary>
    /// Detailinformationen zu Belegsummen
    /// </summary>
    /// <param name="lineTotalAmount">Gesamtbetrag der Positionen</param>
    /// <param name="chargeTotalAmount">Gesamtbetrag der Zuschläge</param>
    /// <param name="allowanceTotalAmount">Gesamtbetrag der Abschläge</param>
    /// <param name="taxBasisAmount">Basisbetrag der Steuerberechnung</param>
    /// <param name="taxTotalAmount">Steuergesamtbetrag</param>
    /// <param name="grandTotalAmount">Bruttosumme</param>
    /// <param name="totalPrepaidAmount">Anzahlungsbetrag</param>
    /// <param name="duePayableAmount">Zahlbetrag</param>
    /// <param name="roundingAmount">RoundingAmount / Rundungsbetrag, profile COMFORT and EXTENDED</param>
    procedure SetTotals(const aLineTotalAmount: Currency = 0; const aChargeTotalAmount: Currency = 0;
  const aAllowanceTotalAmount: Currency = 0; const aTaxBasisAmount: Currency = 0; const aTaxTotalAmount: Currency = 0;
  const aGrandTotalAmount: Currency = 0; const aTotalPrepaidAmount: Currency = 0; const aDuePayableAmount: Currency = 0;
  const aRoundingAmount: Currency = 0);

    /// <summary>
    /// Add information about VAT and apply to the invoice line items for goods and services on the invoice.
    ///
    /// This tax is added per VAT/ tax rate.
    /// </summary>
    /// <param name="basisAmount"></param>
    /// <param name="percent">Tax rate where the tax belongs to</param>
    /// <param name="typeCode"></param>
    /// <param name="categoryCode"></param>
    /// <param name="allowanceChargeBasisAmount"></param>
    /// <param name="exemptionReasonCode"></param>
    /// <param name="exemptionReason"></param>
    procedure AddApplicableTradeTax(const basisAmount: Currency; const percent: Currency; const typeCode: TZUGFeRDTaxTypes; const categoryCode: TZUGFeRDTaxCategoryCodes = TZUGFeRDTaxCategoryCodes.Unknown; const allowanceChargeBasisAmount: Currency = 0; const exemptionReasonCode: TZUGFeRDTaxExemptionReasonCodes = TZUGFeRDTaxExemptionReasonCodes.Unknown; const exemptionReason: string = '');

    /// <summary>
    /// Saves the descriptor object into a stream.
    ///
    /// The stream position will be reset to the original position after writing is finished.
    /// This allows easy further processing of the stream.
    /// </summary>
    /// <param name="stream">The stream where the data should be saved to.</param>
    /// <param name="version">The ZUGFeRD version you want to use. Defaults to version 1.</param>
    /// <param name="profile">The ZUGFeRD profile you want to use. Defaults to Basic.</param>
    procedure Save(const stream: TStream; const version: TZUGFeRDVersion = TZUGFeRDVersion.Version1; const profile: TZUGFeRDProfile = TZUGFeRDProfile.Basic); overload;

    /// <summary>
    /// Saves the descriptor object into a file with given name.
    /// </summary>
    /// <param name="filename">The filename where the data should be saved to.</param>
    /// <param name="version">The ZUGFeRD version you want to use. Defaults to version 1.</param>
    /// <param name="profile">The ZUGFeRD profile you want to use. Defaults to Basic.</param>
    procedure Save(const filename: string; const version: TZUGFeRDVersion = TZUGFeRDVersion.Version1; const profile: TZUGFeRDProfile = TZUGFeRDProfile.Basic); overload;

    /// <summary>
    /// Adds a new comment as a dedicated line of the invoice.
    ///
    /// The line id is generated automatically
    /// </summary>
    /// <param name="comment"></param>
    procedure AddTradeLineCommentItem(const comment: string); overload;

    /// <summary>
    /// Adds a new comment as a dedicated line of the invoice.
    ///
    /// The line id is passed as a parameter
    /// </summary>
    /// <param name="lineID"></param>
    /// <param name="comment"></param>
    procedure AddTradeLineCommentItem(const lineID: string; const comment: string); overload;

    /// <summary>
    /// Adds a new line to the invoice. The line id is generated automatically.
    /// </summary>
    /// <param name="name"></param>
    /// <param name="description"></param>
    /// <param name="unitCode"></param>
    /// <param name="unitQuantity"></param>
    /// <param name="grossUnitPrice"></param>
    /// <param name="netUnitPrice"></param>
    /// <param name="billedQuantity"></param>
    /// <param name="lineTotalAmount">net total including discounts and surcharges. This parameter is optional. If it is not filled, the line total amount is automatically calculated based on netUnitPrice and billedQuantity</param>
    /// <param name="taxType"></param>
    /// <param name="categoryCode"></param>
    /// <param name="taxPercent"></param>
    /// <param name="comment"></param>
    /// <param name="id"></param>
    /// <param name="sellerAssignedID"></param>
    /// <param name="buyerAssignedID"></param>
    /// <param name="deliveryNoteID"></param>
    /// <param name="deliveryNoteDate"></param>
    /// <param name="buyerOrderID"></param>
    /// <param name="buyerOrderDate"></param>
    /// <param name="billingPeriodStart"></param>
    /// <param name="billingPeriodEnd"></param>
    /// <returns></returns>
    function AddTradeLineItem(const name: string; const description: string;
                  const unitCode: TZUGFeRDQuantityCodes = TZUGFeRDQuantityCodes.Unknown; const unitQuantity: INullableParam<Double> = nil;
                  const grossUnitPrice: INullableParam<Currency> = nil; const netUnitPrice: INullableParam<Currency> = nil; const billedQuantity: Double = 0; const lineTotalAmount : Currency = 0;
                  const taxType: TZUGFeRDTaxTypes = TZUGFeRDTaxTypes.Unknown; const categoryCode: TZUGFeRDTaxCategoryCodes = TZUGFeRDTaxCategoryCodes.Unknown; const taxPercent: Double = 0;
                  const comment: string = ''; const id: TZUGFeRDGlobalID = nil; const sellerAssignedID: string = '';
                  const buyerAssignedID: string = ''; const deliveryNoteID: string = ''; const deliveryNoteDate: INullableParam<TDateTime> = nil;
                  const buyerOrderID: string = ''; const buyerOrderDate: INullableParam<TDateTime> = nil; const billingPeriodStart: INullableParam<TDateTime> = nil;
                  const billingPeriodEnd: INullableParam<TDateTime> = nil): TZUGFeRDTradeLineItem; overload;


    /// <summary>
    /// Adds a new line to the invoice. The line id is passed as a parameter.
    /// </summary>
    function AddTradeLineItem(const lineID: string; const name: string; const description: string;
                  const unitCode: TZUGFeRDQuantityCodes = TZUGFeRDQuantityCodes.Unknown; const unitQuantity: INullableParam<Double> = nil;
                  const grossUnitPrice: INullableParam<Currency> = nil; const netUnitPrice: INullableParam<Currency> = nil; const billedQuantity: Double = 0; const lineTotalAmount : Currency = 0;
                  const taxType: TZUGFeRDTaxTypes = TZUGFeRDTaxTypes.Unknown; const categoryCode: TZUGFeRDTaxCategoryCodes = TZUGFeRDTaxCategoryCodes.Unknown; const taxPercent: Double = 0;
                  const comment: string = ''; const id: TZUGFeRDGlobalID = nil; const sellerAssignedID: string = ''; const buyerAssignedID: string = '';
                  const deliveryNoteID: string = ''; const deliveryNoteDate: INullableParam<TDateTime> = nil; const buyerOrderID: string = '';
                  const buyerOrderDate: INullableParam<TDateTime> = nil; const billingPeriodStart: INullableParam<TDateTime> = nil;
                  const billingPeriodEnd: INullableParam<TDateTime> = nil): TZUGFeRDTradeLineItem; overload;

    procedure SetPaymentMeans(paymentCode: TZUGFeRDPaymentMeansTypeCodes; const information: string = '';
                  const identifikationsnummer: string = ''; const mandatsnummer: string = '');

    /// <summary>
    ///     Sets up the payment means for SEPA direct debit.
    /// </summary>
    procedure SetPaymentMeansSepaDirectDebit(const sepaCreditorIdentifier: string;
  const sepaMandateReference: string; const information: string = '');

    /// <summary>
    ///     Sets up the payment means for payment via financial card.
    /// </summary>
    procedure SetPaymentMeansFinancialCard(const financialCardId: string;
      const financialCardCardholder: string; const information: string = '');

    /// <summary>
    /// Adds a group of business terms to specify credit transfer payments
    /// </summary>
    /// <param name="iban">IBAN</param>
    /// <param name="bic">BIC</param>
    /// <param name="id">Optional: old German bank account no</param>
    /// <param name="bankleitzahl">Optional: old German Bankleitzahl</param>
    /// <param name="bankName">Optional: old German bank name</param>
    /// <param name="name">Optional: bank account name</param>
    procedure AddCreditorFinancialAccount(const iban: string; const bic: string; const id: string = '';
      const bankleitzahl: string = ''; const bankName: string = ''; const name: string = '');

    procedure AddDebitorFinancialAccount(const iban: string; const bic: string; const id: string = '';
      const bankleitzahl: string = ''; const bankName: string = '');

    procedure AddReceivableSpecifiedTradeAccountingAccount(const AccountID: string); overload;

    procedure AddReceivableSpecifiedTradeAccountingAccount(const AccountID: string;
      const AccountTypeCode: TZUGFeRDAccountingAccountTypeCodes); overload;
  private
    function _getNextLineId: string;
  end;

implementation

uses
  intf.ZUGFeRDInvoiceDescriptorReader,intf.ZUGFeRDInvoiceDescriptorWriter,
  intf.ZUGFeRDInvoiceDescriptor1Reader,intf.ZUGFeRDInvoiceDescriptor1Writer,
  intf.ZUGFeRDInvoiceDescriptor20Reader,intf.ZUGFeRDInvoiceDescriptor20Writer,
  intf.ZUGFeRDInvoiceDescriptor22Reader,intf.ZUGFeRDInvoiceDescriptor22Writer,
  intf.ZUGFeRDInvoiceDescriptor22UblReader
  ;

{ TZUGFeRDInvoiceDescriptor }

constructor TZUGFeRDInvoiceDescriptor.Create;
begin
  FAdditionalReferencedDocuments := TObjectList<TZUGFeRDAdditionalReferencedDocument>.Create;
  FDespatchAdviceReferencedDocument := nil;
  FDeliveryNoteReferencedDocument:= nil;//TZUGFeRDDeliveryNoteReferencedDocument.Create;
  FContractReferencedDocument    := nil;//TZUGFeRDContractReferencedDocument.Create;
  FSpecifiedProcuringProject     := nil;//TZUGFeRDSpecifiedProcuringProject.Create;
  FBuyer                         := nil;//TZUGFeRDParty.Create;
  FBuyerContact                  := nil;//TZUGFeRDContact.Create;
  FBuyerTaxRegistration          := TObjectList<TZUGFeRDTaxRegistration>.Create;
  FBuyerElectronicAddress        := TZUGFeRDElectronicAddress.Create;
  FSeller                        := nil;//TZUGFeRDParty.Create;
  FSellerContact                 := nil;//TZUGFeRDContact.Create;
  FSellerTaxRegistration         := TObjectList<TZUGFeRDTaxRegistration>.Create;
  FSellerElectronicAddress       := TZUGFeRDElectronicAddress.Create;
  FInvoicee                      := nil;//TZUGFeRDParty.Create;
  FShipTo                        := nil;//TZUGFeRDParty.Create;
  FPayee                         := nil;//TZUGFeRDParty.Create;
  FShipFrom                      := nil;//TZUGFeRDParty.Create;
  FNotes                         := TObjectList<TZUGFeRDNote>.Create;
  FTradeLineItems                := TObjectList<TZUGFeRDTradeLineItem>.Create;
  FTaxes                         := TObjectList<TZUGFeRDTax>.Create;
  FServiceCharges                := TObjectList<TZUGFeRDServiceCharge>.Create;
  FTradeAllowanceCharges         := TObjectList<TZUGFeRDTradeAllowanceCharge>.Create;
  FPaymentTerms                  := nil;//TZUGFeRDPaymentTerms.Create;
  FInvoiceReferencedDocument     := nil;//TZUGFeRDInvoiceReferencedDocument.Create;
  FReceivableSpecifiedTradeAccountingAccounts:= TObjectList<TZUGFeRDReceivableSpecifiedTradeAccountingAccount>.Create;
  FCreditorBankAccounts          := TObjectList<TZUGFeRDBankAccount>.Create;
  FDebitorBankAccounts           := TObjectList<TZUGFeRDBankAccount>.Create;
  FPaymentMeans                  := nil;//TZUGFeRDPaymentMeans.Create;
  FSellerOrderReferencedDocument := nil;//TZUGFeRDSellerOrderReferencedDocument.Create;
end;

destructor TZUGFeRDInvoiceDescriptor.Destroy;
begin
  if Assigned(FAdditionalReferencedDocuments ) then begin FAdditionalReferencedDocuments.Free; FAdditionalReferencedDocuments  := nil; end;
  if Assigned(FDespatchAdviceReferencedDocument) then begin FDespatchAdviceReferencedDocument.Free; FDespatchAdviceReferencedDocument := nil; end;
  if Assigned(FDeliveryNoteReferencedDocument) then begin FDeliveryNoteReferencedDocument.Free; FDeliveryNoteReferencedDocument := nil; end;
  if Assigned(FContractReferencedDocument    ) then begin FContractReferencedDocument.Free; FContractReferencedDocument     := nil; end;
  if Assigned(FSpecifiedProcuringProject     ) then begin FSpecifiedProcuringProject.Free; FSpecifiedProcuringProject      := nil; end;
  if Assigned(FBuyer                         ) then begin FBuyer.Free; FBuyer                          := nil; end;
  if Assigned(FBuyerContact                  ) then begin FBuyerContact.Free; FBuyerContact                   := nil; end;
  if Assigned(FBuyerTaxRegistration          ) then begin FBuyerTaxRegistration.Free; FBuyerTaxRegistration           := nil; end;
  if Assigned(FBuyerElectronicAddress        ) then begin FBuyerElectronicAddress        .Free; FBuyerElectronicAddress         := nil; end;
  if Assigned(FSeller                        ) then begin FSeller.Free; FSeller                         := nil; end;
  if Assigned(FSellerContact                 ) then begin FSellerContact.Free; FSellerContact                  := nil; end;
  if Assigned(FSellerTaxRegistration         ) then begin FSellerTaxRegistration.Free; FSellerTaxRegistration          := nil; end;
  if Assigned(FSellerElectronicAddress       ) then begin FSellerElectronicAddress       .Free; FSellerElectronicAddress        := nil; end;
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
var
  reader: TZUGFeRDInvoiceDescriptorReader;
begin
  reader := TZUGFeRDInvoiceDescriptor1Reader.Create;
  try
    if reader.IsReadableByThisReaderVersion(filename) then
    begin
      Result := TZUGFeRDVersion.Version1;
      Exit;
    end;
  finally
    reader.Free;
  end;

  reader := TZUGFeRDInvoiceDescriptor22UblReader.Create;
  try
    if reader.IsReadableByThisReaderVersion(filename) then
    begin
      Result := TZUGFeRDVersion.Version22;
      Exit;
    end;
  finally
    reader.Free;
  end;

  reader := TZUGFeRDInvoiceDescriptor22Reader.Create;
  try
    if reader.IsReadableByThisReaderVersion(filename) then
    begin
      Result := TZUGFeRDVersion.Version22;
      Exit;
    end;
  finally
    reader.Free;
  end;

  reader := TZUGFeRDInvoiceDescriptor20Reader.Create;
  try
    if reader.IsReadableByThisReaderVersion(filename) then
    begin
      Result := TZUGFeRDVersion.Version20;
      Exit;
    end;
  finally
    reader.Free;
  end;

  raise TZUGFeRDUnsupportedException.Create('No ZUGFeRD invoice reader was able to parse this file "' + filename + '"!');
end;

class function TZUGFeRDInvoiceDescriptor.GetVersion(
  const stream: TStream): TZUGFeRDVersion;
var
  reader: TZUGFeRDInvoiceDescriptorReader;
begin
  reader := TZUGFeRDInvoiceDescriptor1Reader.Create;
  try
    if reader.IsReadableByThisReaderVersion(stream) then
    begin
      Result := TZUGFeRDVersion.Version1;
      Exit;
    end;
  finally
    reader.Free;
  end;

  reader := TZUGFeRDInvoiceDescriptor22UblReader.Create;
  try
    if reader.IsReadableByThisReaderVersion(stream) then
    begin
      Result := TZUGFeRDVersion.Version22;
      Exit;
    end;
  finally
    reader.Free;
  end;

  reader := TZUGFeRDInvoiceDescriptor22Reader.Create;
  try
    if reader.IsReadableByThisReaderVersion(stream) then
    begin
      Result := TZUGFeRDVersion.Version22;
      Exit;
    end;
  finally
    reader.Free;
  end;

  reader := TZUGFeRDInvoiceDescriptor20Reader.Create;
  try
    if reader.IsReadableByThisReaderVersion(stream) then
    begin
      Result := TZUGFeRDVersion.Version20;
      Exit;
    end;
  finally
    reader.Free;
  end;

  raise TZUGFeRDUnsupportedException.Create('No ZUGFeRD invoice reader was able to parse this stream!');
end;

class function TZUGFeRDInvoiceDescriptor.Load(stream: TStream): TZUGFeRDInvoiceDescriptor;
var
  reader: TZUGFeRDInvoiceDescriptorReader;
begin
  reader := TZUGFeRDInvoiceDescriptor1Reader.Create;
  try
    if reader.IsReadableByThisReaderVersion(stream) then
    begin
      Result := reader.Load(stream);
      exit;
    end;
  finally
    reader.Free;
  end;

  reader := TZUGFeRDInvoiceDescriptor22UblReader.Create;
  try
    if reader.IsReadableByThisReaderVersion(stream) then
    begin
      Result := reader.Load(stream);
      exit;
    end;
  finally
    reader.Free;
  end;

  reader := TZUGFeRDInvoiceDescriptor22Reader.Create;
  try
    if reader.IsReadableByThisReaderVersion(stream) then
    begin
      Result := reader.Load(stream);
      exit;
    end;
  finally
    reader.Free;
  end;

  reader := TZUGFeRDInvoiceDescriptor20Reader.Create;
  try
    if reader.IsReadableByThisReaderVersion(stream) then
    begin
      Result := reader.Load(stream);
      exit;
    end;
  finally
    reader.Free;
  end;

  raise TZUGFeRDUnsupportedException.Create('No ZUGFeRD invoice reader was able to parse this stream!');
end;

class function TZUGFeRDInvoiceDescriptor.Load(filename: string): TZUGFeRDInvoiceDescriptor;
var
  reader: TZUGFeRDInvoiceDescriptorReader;
begin
  reader := TZUGFeRDInvoiceDescriptor1Reader.Create;
  try
    if reader.IsReadableByThisReaderVersion(filename) then
    begin
      Result := reader.Load(filename);
      exit;
    end;
  finally
    reader.Free;
  end;

  reader := TZUGFeRDInvoiceDescriptor22UblReader.Create;
  try
    if reader.IsReadableByThisReaderVersion(filename) then
    begin
      Result := reader.Load(filename);
      exit;
    end;
  finally
    reader.Free;
  end;

  reader := TZUGFeRDInvoiceDescriptor22Reader.Create;
  try
    if reader.IsReadableByThisReaderVersion(filename) then
    begin
      Result := reader.Load(filename);
      exit;
    end;
  finally
    reader.Free;
  end;

  reader := TZUGFeRDInvoiceDescriptor20Reader.Create;
  try
    if reader.IsReadableByThisReaderVersion(filename) then
    begin
      Result := reader.Load(filename);
      exit;
    end;
  finally
    reader.Free;
  end;

  raise TZUGFeRDUnsupportedException.CreateFmt('No ZUGFeRD invoice reader was able to parse this file ''%s''!', [filename]);
end;

class function TZUGFeRDInvoiceDescriptor.CreateInvoice(const invoiceNo: string; invoiceDate: TDateTime;
  currency: TZUGFeRDCurrencyCodes; const invoiceNoAsReference: string = ''): TZUGFeRDInvoiceDescriptor;
begin
  Result := TZUGFeRDInvoiceDescriptor.Create;
  Result.InvoiceDate := invoiceDate;
  Result.InvoiceNo := invoiceNo;
  Result.Currency := currency;
  Result.PaymentReference := invoiceNoAsReference;
end;

procedure TZUGFeRDInvoiceDescriptor.AddNote(const note: string;
  subjectCode: TZUGFeRDSubjectCodes = TZUGFeRDSubjectCodes.Unknown;
  contentCode: TZUGFeRDContentCodes = TZUGFeRDContentCodes.Unknown);
begin
  //TODO prüfen:
  //ST1, ST2, ST3 nur mit AAK
  //EEV, WEB, VEV nur mit AAJ

  FNotes.Add(TZUGFeRDNote.Create(note, subjectCode, contentCode));
end;

procedure TZUGFeRDInvoiceDescriptor.SetBuyer(const name, postcode, city, street: string;
  country: TZUGFeRDCountryCodes; const id: string = '';
  globalID: TZUGFeRDGlobalID = nil; const receiver: string = '';
  legalOrganization: TZUGFeRDLegalOrganization = nil);
begin
  FBuyer.ID.ID := id;
  FBuyer.ID.SchemeID := TZUGFeRDGlobalIDSchemeIdentifiers.Unknown;
  FBuyer.Name := name;
  FBuyer.Postcode := postcode;
  FBuyer.ContactName := receiver;
  FBuyer.City := city;
  FBuyer.Street := street;
  FBuyer.Country := country;
  if FBuyer.GlobalID <> nil then FBuyer.GlobalID.Free;
  FBuyer.GlobalID := globalID; //TODO Mem Leak
  if FBuyer.SpecifiedLegalOrganization <> nil then FBuyer.SpecifiedLegalOrganization.Free;
  FBuyer.SpecifiedLegalOrganization := legalOrganization; //TODO Mem Leak
end;

procedure TZUGFeRDInvoiceDescriptor.SetSeller(const name, postcode, city, street: string;
  country: TZUGFeRDCountryCodes; const id: string = '';
  globalID: TZUGFeRDGlobalID = nil;
  legalOrganization: TZUGFeRDLegalOrganization = nil);
begin
  FSeller.ID.ID := id;
  FSeller.ID.SchemeID := TZUGFeRDGlobalIDSchemeIdentifiers.Unknown;
  FSeller.Name := name;
  FSeller.Postcode := postcode;
  FSeller.City := city;
  FSeller.Street := street;
  FSeller.Country := country;
  if FSeller.GlobalID <> nil then FSeller.GlobalID.Free;
  FSeller.GlobalID := globalID; //TODO Mem Leak
  if FSeller.SpecifiedLegalOrganization <> nil then FSeller.SpecifiedLegalOrganization.Free;
  FSeller.SpecifiedLegalOrganization := legalOrganization; //TODO Mem Leak
end;

procedure TZUGFeRDInvoiceDescriptor.SetSellerContact(const name: string = ''; const orgunit: string = '';
  const emailAddress: string = ''; const phoneno: string = ''; const faxno: string = '');
begin
  FSellerContact.Name := name;
  FSellerContact.OrgUnit := orgunit;
  FSellerContact.EmailAddress := emailAddress;
  FSellerContact.PhoneNo := phoneno;
  FSellerContact.FaxNo := faxno;
end;

procedure TZUGFeRDInvoiceDescriptor.SetBuyerContact(const name: string; const orgunit: string = '';
  const emailAddress: string = ''; const phoneno: string = ''; const faxno: string = '');
begin
  FBuyerContact.Name := name;
  FBuyerContact.OrgUnit := orgunit;
  FBuyerContact.EmailAddress := emailAddress;
  FBuyerContact.PhoneNo := phoneno;
  FBuyerContact.FaxNo := faxno;
end;

procedure TZUGFeRDInvoiceDescriptor.SetSpecifiedProcuringProject(const id, name: string);
begin
  FSpecifiedProcuringProject.ID := id;
  FSpecifiedProcuringProject.Name := name;
end;

procedure TZUGFeRDInvoiceDescriptor.AddBuyerTaxRegistration(const no: string; const schemeID: TZUGFeRDTaxRegistrationSchemeID);
begin
  FBuyerTaxRegistration.Add(TZUGFeRDTaxRegistration.Create);
  FBuyerTaxRegistration[BuyerTaxRegistration.Count - 1].No := no;
  FBuyerTaxRegistration[BuyerTaxRegistration.Count - 1].SchemeID := schemeID;
end;

procedure TZUGFeRDInvoiceDescriptor.AddSellerTaxRegistration(const no: string; const schemeID: TZUGFeRDTaxRegistrationSchemeID);
begin
  FSellerTaxRegistration.Add(TZUGFeRDTaxRegistration.Create);
  FSellerTaxRegistration[SellerTaxRegistration.Count - 1].No := no;
  FSellerTaxRegistration[SellerTaxRegistration.Count - 1].SchemeID := schemeID;
end;

procedure TZUGFeRDInvoiceDescriptor.SetBuyerElectronicAddress(address : string; electronicAddressSchemeID : TZUGFeRDElectronicAddressSchemeIdentifiers);
begin
  FBuyerElectronicAddress.Address := address;
  FBuyerElectronicAddress.ElectronicAddressSchemeID := electronicAddressSchemeID;
end;

procedure TZUGFeRDInvoiceDescriptor.SetSellerElectronicAddress(address : string; electronicAddressSchemeID : TZUGFeRDElectronicAddressSchemeIdentifiers);
begin
  FSellerElectronicAddress.Address := address;
  FSellerElectronicAddress.ElectronicAddressSchemeID := electronicAddressSchemeID;
end;

procedure TZUGFeRDInvoiceDescriptor.AddAdditionalReferencedDocument(const id: string; const typeCode: TZUGFeRDAdditionalReferencedDocumentTypeCode;
  const issueDateTime: TDateTime = 0; const name: string = ''; const referenceTypeCode: TZUGFeRDReferenceTypeCodes = TZUGFeRDReferenceTypeCodes.Unknown;
  const attachmentBinaryObject: TMemoryStream = nil; const filename: string = '');
begin
  FAdditionalReferencedDocuments.Add(TZUGFeRDAdditionalReferencedDocument.Create(false));
  FAdditionalReferencedDocuments[AdditionalReferencedDocuments.Count - 1].ReferenceTypeCode := referenceTypeCode;
  FAdditionalReferencedDocuments[AdditionalReferencedDocuments.Count - 1].ID := id;
  FAdditionalReferencedDocuments[AdditionalReferencedDocuments.Count - 1].IssueDateTime:= issueDateTime;
  FAdditionalReferencedDocuments[AdditionalReferencedDocuments.Count - 1].Name := name;
  FAdditionalReferencedDocuments[AdditionalReferencedDocuments.Count - 1].AttachmentBinaryObject := attachmentBinaryObject;
  FAdditionalReferencedDocuments[AdditionalReferencedDocuments.Count - 1].Filename := filename;
  FAdditionalReferencedDocuments[AdditionalReferencedDocuments.Count - 1].TypeCode := typeCode;
end;

procedure TZUGFeRDInvoiceDescriptor.SetBuyerOrderReferenceDocument(const orderNo: string; const orderDate: TDateTime = 0);
begin
  FOrderNo := orderNo;
  if orderDate < 100 then
    FOrderDate:= Nil
  else
    FOrderDate:= orderDate;
end;

procedure TZUGFeRDInvoiceDescriptor.SetDeliveryNoteReferenceDocument(const deliveryNoteNo: string; const deliveryNoteDate: TDateTime = 0);
begin
  FDeliveryNoteReferencedDocument.ID := deliveryNoteNo;
  FDeliveryNoteReferencedDocument.IssueDateTime:= deliveryNoteDate;
end;

procedure TZUGFeRDInvoiceDescriptor.SetDespatchAdviceReferencedDocument(
  despatchAdviceNo: String; despatchAdviceDate: TDateTime);
begin
  FDespatchAdviceReferencedDocument.ID := despatchAdviceNo;
  FDespatchAdviceReferencedDocument.IssueDateTime:= despatchAdviceDate;
end;

procedure TZUGFeRDInvoiceDescriptor.SetContractReferencedDocument(const contractNo: string; const contractDate: TDateTime);
begin
  FContractReferencedDocument.ID := contractNo; //TODO memeak
  FContractReferencedDocument.IssueDateTime:= contractDate;
end;

procedure TZUGFeRDInvoiceDescriptor.AddLogisticsServiceCharge(const amount: Currency; const description: string; const taxTypeCode: TZUGFeRDTaxTypes; const taxCategoryCode: TZUGFeRDTaxCategoryCodes; const taxPercent: Currency);
var
  serviceCharge: TZUGFeRDServiceCharge;
begin
  serviceCharge := TZUGFeRDServiceCharge.Create;
  serviceCharge.Description := description;
  serviceCharge.Amount := amount;
  serviceCharge.Tax.CategoryCode := taxCategoryCode;
  serviceCharge.Tax.TypeCode := taxTypeCode;
  serviceCharge.Tax.Percent := taxPercent;
  FServiceCharges.Add(serviceCharge);
end;

procedure TZUGFeRDInvoiceDescriptor.AddTradeAllowanceCharge(
  const isDiscount: Boolean; const basisAmount: Currency;
  const currency: TZUGFeRDCurrencyCodes; const actualAmount: Currency;
  const reason: string;
  const reasonCodeCharge : TZUGFeRDSpecialServiceDescriptionCodes;
  const reasonCodeAllowance : TZUGFeRDAllowanceOrChargeIdentificationCodes;
  const taxTypeCode: TZUGFeRDTaxTypes;
  const taxCategoryCode: TZUGFeRDTaxCategoryCodes; const taxPercent: Currency);
var
  tradeAllowanceCharge: TZUGFeRDTradeAllowanceCharge;
begin
  tradeAllowanceCharge := TZUGFeRDTradeAllowanceCharge.Create;
  tradeAllowanceCharge.ChargeIndicator := not isDiscount;
  tradeAllowanceCharge.Reason := reason;
  tradeAllowanceCharge.ReasonCodeAllowance := reasonCodeAllowance;
  tradeAllowanceCharge.ReasonCodeCharge := reasonCodeCharge;
  tradeAllowanceCharge.BasisAmount := basisAmount;
  tradeAllowanceCharge.ActualAmount := actualAmount;
  tradeAllowanceCharge.Currency := currency;
  tradeAllowanceCharge.Amount := actualAmount;
  tradeAllowanceCharge.ChargePercentage := 0;
  tradeAllowanceCharge.Tax.CategoryCode := taxCategoryCode;
  tradeAllowanceCharge.Tax.TypeCode := taxTypeCode;
  tradeAllowanceCharge.Tax.Percent := taxPercent;
  FTradeAllowanceCharges.Add(tradeAllowanceCharge);
end;

procedure TZUGFeRDInvoiceDescriptor.AddTradeAllowanceCharge(
  const isDiscount: Boolean; const basisAmount: Currency;
  const currency: TZUGFeRDCurrencyCodes; const actualAmount,
  chargePercentage: Currency; const reason: string;
  const reasonCodeCharge : TZUGFeRDSpecialServiceDescriptionCodes;
  const reasonCodeAllowance : TZUGFeRDAllowanceOrChargeIdentificationCodes;
  const taxTypeCode: TZUGFeRDTaxTypes;
  const taxCategoryCode: TZUGFeRDTaxCategoryCodes; const taxPercent: Currency);
var
  tradeAllowanceCharge: TZUGFeRDTradeAllowanceCharge;
begin
  tradeAllowanceCharge := TZUGFeRDTradeAllowanceCharge.Create;
  tradeAllowanceCharge.ChargeIndicator := not isDiscount;
  tradeAllowanceCharge.Reason := reason;
  tradeAllowanceCharge.ReasonCodeAllowance := reasonCodeAllowance;
  tradeAllowanceCharge.ReasonCodeCharge := reasonCodeCharge;
  tradeAllowanceCharge.BasisAmount := basisAmount;
  tradeAllowanceCharge.ActualAmount := actualAmount;
  tradeAllowanceCharge.Currency := currency;
  tradeAllowanceCharge.Amount := actualAmount;
  tradeAllowanceCharge.ChargePercentage := chargePercentage;
  tradeAllowanceCharge.Tax.CategoryCode := taxCategoryCode;
  tradeAllowanceCharge.Tax.TypeCode := taxTypeCode;
  tradeAllowanceCharge.Tax.Percent := taxPercent;
  FTradeAllowanceCharges.Add(tradeAllowanceCharge);
end;

procedure TZUGFeRDInvoiceDescriptor.SetTradePaymentTerms(const description: string; const dueDate: TDateTime = 0);
begin
  if PaymentTerms = nil then PaymentTerms := TZUGFeRDPaymentTerms.Create;
  PaymentTerms.Description := description;
  PaymentTerms.DueDate:= dueDate;
end;

procedure TZUGFeRDInvoiceDescriptor.SetInvoiceReferencedDocument(const id: string; const IssueDateTime: TDateTime = 0);
begin
  FInvoiceReferencedDocument.ID := id;
  FInvoiceReferencedDocument.IssueDateTime:= IssueDateTime;
end;

procedure TZUGFeRDInvoiceDescriptor.SetTotals(const aLineTotalAmount: Currency = 0; const aChargeTotalAmount: Currency = 0;
  const aAllowanceTotalAmount: Currency = 0; const aTaxBasisAmount: Currency = 0; const aTaxTotalAmount: Currency = 0;
  const aGrandTotalAmount: Currency = 0; const aTotalPrepaidAmount: Currency = 0; const aDuePayableAmount: Currency = 0;
  const aRoundingAmount: Currency = 0);
begin
  LineTotalAmount:= aLineTotalAmount;
  ChargeTotalAmount:= aChargeTotalAmount;
  AllowanceTotalAmount:= aAllowanceTotalAmount;
  TaxBasisAmount:= aTaxBasisAmount;
  TaxTotalAmount:= aTaxTotalAmount;
  GrandTotalAmount:= aGrandTotalAmount;
  TotalPrepaidAmount:= aTotalPrepaidAmount;
  DuePayableAmount:= aDuePayableAmount;
  RoundingAmount:= aRoundingAmount;
end;

procedure TZUGFeRDInvoiceDescriptor.AddApplicableTradeTax(const basisAmount: Currency;
  const percent: Currency; const typeCode: TZUGFeRDTaxTypes;
  const categoryCode: TZUGFeRDTaxCategoryCodes = TZUGFeRDTaxCategoryCodes.Unknown;
  const allowanceChargeBasisAmount: Currency = 0;
  const exemptionReasonCode: TZUGFeRDTaxExemptionReasonCodes = TZUGFeRDTaxExemptionReasonCodes.Unknown;
  const exemptionReason: string = '');
var
  tax: TZUGFeRDTax;
begin
  tax := TZUGFeRDTax.Create;
  tax.BasisAmount := basisAmount;
  tax.Percent := percent;
  tax.TypeCode := typeCode;
  tax.AllowanceChargeBasisAmount := allowanceChargeBasisAmount;
  tax.ExemptionReasonCode := exemptionReasonCode;
  tax.ExemptionReason := exemptionReason;

  if (categoryCode <> TZUGFeRDTaxCategoryCodes.Unknown) then
    tax.CategoryCode := categoryCode;

  Taxes.Add(tax);
end;

procedure TZUGFeRDInvoiceDescriptor.Save(const stream: TStream;
  const version: TZUGFeRDVersion = TZUGFeRDVersion.Version1;
  const profile: TZUGFeRDProfile = TZUGFeRDProfile.Basic);
var
  writer: TZUGFeRDInvoiceDescriptorWriter;
begin
  self.Profile := profile;

  case version of
    TZUGFeRDVersion.Version1:
      writer := TZUGFeRDInvoiceDescriptor1Writer.Create;
    TZUGFeRDVersion.Version20:
      writer := TZUGFeRDInvoiceDescriptor20Writer.Create;
    TZUGFeRDVersion.Version22:
      writer := TZUGFeRDInvoiceDescriptor22Writer.Create;
    else
      raise TZUGFeRDUnsupportedException.Create('New ZUGFeRDVersion defined but not implemented!');
  end;
  try
    writer.Save(Self, stream);
  finally
    writer.Free;
  end;
end;

procedure TZUGFeRDInvoiceDescriptor.Save(const filename: string; const version: TZUGFeRDVersion = TZUGFeRDVersion.Version1; const profile: TZUGFeRDProfile = TZUGFeRDProfile.Basic);
var
  writer: TZUGFeRDInvoiceDescriptorWriter;
begin
  self.Profile := profile;

  case version of
    TZUGFeRDVersion.Version1:
      writer := TZUGFeRDInvoiceDescriptor1Writer.Create;
    TZUGFeRDVersion.Version20:
      writer := TZUGFeRDInvoiceDescriptor20Writer.Create;
    TZUGFeRDVersion.Version21,
    TZUGFeRDVersion.Version22:
      writer := TZUGFeRDInvoiceDescriptor22Writer.Create;
    else
      raise TZUGFeRDUnsupportedException.Create('New ZUGFeRDVersion defined but not implemented!');
  end;
  try
    writer.Save(Self, filename);
  finally
    writer.Free;
  end;
end;

procedure TZUGFeRDInvoiceDescriptor.AddTradeLineCommentItem(const comment: string);
begin
  AddTradeLineCommentItem(_getNextLineId(), comment);
end;

procedure TZUGFeRDInvoiceDescriptor.AddTradeLineCommentItem(const lineID: string; const comment: string);
var
  item: TZUGFeRDTradeLineItem;
begin
  if (lineID.IsEmpty) then
    raise TZUGFeRDArgumentException.Create('LineID cannot be Null or Empty')
  else
  begin
    for var i : Integer := 0 to TradeLineItems.Count-1 do
    if (TradeLineItems[i].AssociatedDocument <> nil) then
    if SameText(TradeLineItems[i].AssociatedDocument.LineID,lineID) then
      raise TZUGFeRDArgumentException.Create('LineID must be unique');
  end;

  item := TZUGFeRDTradeLineItem.Create;
  item.AssociatedDocument := TZUGFeRDAssociatedDocument.Create(lineID);
  item.GrossUnitPrice:= 0;
  item.NetUnitPrice:= 0;
  item.BilledQuantity := 0;
  item.UnitCode := TZUGFeRDQuantityCodes.C62;
  item.TaxCategoryCode := TZUGFeRDTaxCategoryCodes.O;

  item.AssociatedDocument.Notes.Add(
    TZUGFeRDNote.Create(comment,
                        TZUGFeRDSubjectCodes.Unknown,
                        TZUGFeRDContentCodes.Unknown
    )
  );

  TradeLineItems.Add(item);
end;

function TZUGFeRDInvoiceDescriptor.AddTradeLineItem(const name: string;
  const description: string;
  const unitCode: TZUGFeRDQuantityCodes = TZUGFeRDQuantityCodes.Unknown;
  const unitQuantity: INullableParam<Double> = nil;
  const grossUnitPrice: INullableParam<Currency> = nil;
  const netUnitPrice: INullableParam<Currency> = nil;
  const billedQuantity: Double = 0;
  const lineTotalAmount: Currency = 0;
  const taxType: TZUGFeRDTaxTypes = TZUGFeRDTaxTypes.Unknown;
  const categoryCode: TZUGFeRDTaxCategoryCodes = TZUGFeRDTaxCategoryCodes.Unknown;
  const taxPercent: Double = 0;
  const comment: string = '';
  const id: TZUGFeRDGlobalID = nil;
  const sellerAssignedID: string = '';
  const buyerAssignedID: string = '';
  const deliveryNoteID: string = '';
  const deliveryNoteDate: INullableParam<TDateTime> = nil;
  const buyerOrderID: string = '';
  const buyerOrderDate: INullableParam<TDateTime> = nil;
  const billingPeriodStart: INullableParam<TDateTime> = nil;
  const billingPeriodEnd: INullableParam<TDateTime> = nil): TZUGFeRDTradeLineItem;
begin
  Result := AddTradeLineItem(_getNextLineId(), name, description, unitCode,
    unitQuantity, grossUnitPrice, netUnitPrice, billedQuantity, lineTotalAmount,
    taxType, categoryCode, taxPercent, comment, id, sellerAssignedID,
    buyerAssignedID, deliveryNoteID, deliveryNoteDate, buyerOrderID,
    buyerOrderDate, billingPeriodStart, billingPeriodEnd);
end;

function TZUGFeRDInvoiceDescriptor.AddTradeLineItem(const lineID: string;
  const name: string; const description: string;
  const unitCode: TZUGFeRDQuantityCodes = TZUGFeRDQuantityCodes.Unknown;
  const unitQuantity: INullableParam<Double> = nil;
  const grossUnitPrice: INullableParam<Currency> = nil;
  const netUnitPrice: INullableParam<Currency> = nil;
  const billedQuantity: Double = 0;
  const lineTotalAmount: Currency = 0;
  const taxType: TZUGFeRDTaxTypes = TZUGFeRDTaxTypes.Unknown;
  const categoryCode: TZUGFeRDTaxCategoryCodes = TZUGFeRDTaxCategoryCodes.Unknown;
  const taxPercent: Double = 0;
  const comment: string = '';
  const id: TZUGFeRDGlobalID = nil;
  const sellerAssignedID: string = '';
  const buyerAssignedID: string = '';
  const deliveryNoteID: string = '';
  const deliveryNoteDate: INullableParam<TDateTime> = nil;
  const buyerOrderID: string = '';
  const buyerOrderDate: INullableParam<TDateTime> = nil;
  const billingPeriodStart: INullableParam<TDateTime> = nil;
  const billingPeriodEnd: INullableParam<TDateTime> = nil): TZUGFeRDTradeLineItem;
var
  newItem: TZUGFeRDTradeLineItem;
begin
  if (lineID.IsEmpty) then
    raise TZUGFeRDArgumentException.Create('LineID cannot be Null or Empty')
  else
  begin
    for var i : Integer := 0 to TradeLineItems.Count-1 do
    if (TradeLineItems[i].AssociatedDocument <> nil) then
    if SameText(TradeLineItems[i].AssociatedDocument.LineID,lineID) then
      raise TZUGFeRDArgumentException.Create('LineID must be unique');
  end;

  newItem := TZUGFeRDTradeLineItem.Create;
  newItem.AssociatedDocument := TZUGFeRDAssociatedDocument.Create(lineID);
  newItem.GlobalID.Free; newItem.GlobalID := id;
  newItem.SellerAssignedID := sellerAssignedID;
  newItem.BuyerAssignedID := buyerAssignedID;
  newItem.Name := name;
  newItem.Description := description;
  newItem.UnitCode := unitCode;
  newItem.UnitQuantity := unitQuantity;
//  newItem.GrossUnitPrice := grossUnitPrice;
//  newItem.NetUnitPrice := netUnitPrice;
  newItem.BilledQuantity := billedQuantity;
  if lineTotalAmount <> 0.0 then
    newItem.LineTotalAmount:= LineTotalAmount;

  newItem.TaxType := taxType;
  newItem.TaxCategoryCode := categoryCode;
  newItem.TaxPercent := taxPercent;
  newItem.BillingPeriodStart:= billingPeriodStart;
  newItem.BillingPeriodEnd:= billingPeriodEnd;
  if (not comment.IsEmpty) then
  newItem.AssociatedDocument.Notes.Add(
    TZUGFeRDNote.Create(comment,
                        TZUGFeRDSubjectCodes.Unknown,
                        TZUGFeRDContentCodes.Unknown
    )
  );
  if (not deliveryNoteID.IsEmpty) or (deliveryNoteDate <> Nil) then
    newItem.SetDeliveryNoteReferencedDocument(deliveryNoteID,deliveryNoteDate);

  if (not buyerOrderID.IsEmpty) or (buyerOrderDate <> nil) then
    newItem.SetOrderReferencedDocument(buyerOrderID, buyerOrderDate);

  TradeLineItems.Add(newItem);

  Result := newItem;
end;

procedure TZUGFeRDInvoiceDescriptor.SetPaymentMeans(paymentCode: TZUGFeRDPaymentMeansTypeCodes; const information: string = '';
  const identifikationsnummer: string = ''; const mandatsnummer: string = '');
begin
  if Self.PaymentMeans = nil then Self.PaymentMeans := TZUGFeRDPaymentMeans.Create;

  Self.PaymentMeans.TypeCode := paymentCode;
  Self.PaymentMeans.Information := information;
  Self.PaymentMeans.SEPACreditorIdentifier := identifikationsnummer;
  Self.PaymentMeans.SEPAMandateReference := mandatsnummer;
end;

procedure TZUGFeRDInvoiceDescriptor.SetPaymentMeansSepaDirectDebit(const sepaCreditorIdentifier: string;
  const sepaMandateReference: string; const information: string = '');
begin
  if Self.PaymentMeans = nil then Self.PaymentMeans := TZUGFeRDPaymentMeans.Create;

  Self.PaymentMeans.TypeCode := TZUGFeRDPaymentMeansTypeCodes.SEPADirectDebit;
  Self.PaymentMeans.Information := information;
  Self.PaymentMeans.SEPACreditorIdentifier := sepaCreditorIdentifier;
  Self.PaymentMeans.SEPAMandateReference := sepaMandateReference;
end;

procedure TZUGFeRDInvoiceDescriptor.SetPaymentMeansFinancialCard(const financialCardId: string;
  const financialCardCardholder: string; const information: string = '');
begin
  if Self.PaymentMeans = nil then Self.PaymentMeans := TZUGFeRDPaymentMeans.Create;

  PaymentMeans.TypeCode := TZUGFeRDPaymentMeansTypeCodes.SEPADirectDebit;
  PaymentMeans.Information := information;
  PaymentMeans.FinancialCard.Id := financialCardId;
  PaymentMeans.FinancialCard.CardholderName := financialCardCardholder;
end;

procedure TZUGFeRDInvoiceDescriptor.AddCreditorFinancialAccount(const iban: string; const bic: string; const id: string = '';
  const bankleitzahl: string = ''; const bankName: string = ''; const name: string = '');
var
  newItem : TZUGFeRDBankAccount;
begin
  newItem := TZUGFeRDBankAccount.Create;
  newItem.ID := id;
  newItem.IBAN := iban;
  newItem.BIC := bic;
  newItem.Bankleitzahl := bankleitzahl;
  newItem.BankName := bankName;
  newItem.Name := name;
  CreditorBankAccounts.Add(newItem);
end;

procedure TZUGFeRDInvoiceDescriptor.AddDebitorFinancialAccount(const iban: string; const bic: string; const id: string = '';
  const bankleitzahl: string = ''; const bankName: string = '');
var
  newItem : TZUGFeRDBankAccount;
begin
  newItem := TZUGFeRDBankAccount.Create;
  newItem.ID := id;
  newItem.IBAN := iban;
  newItem.BIC := bic;
  newItem.Bankleitzahl := bankleitzahl;
  newItem.BankName := bankName;
  DebitorBankAccounts.Add(newItem);
end;

procedure TZUGFeRDInvoiceDescriptor.AddReceivableSpecifiedTradeAccountingAccount(const AccountID: string);
begin
  AddReceivableSpecifiedTradeAccountingAccount(AccountID, TZUGFeRDAccountingAccountTypeCodes.Unknown);
end;

procedure TZUGFeRDInvoiceDescriptor.AddReceivableSpecifiedTradeAccountingAccount(const AccountID: string;
  const AccountTypeCode: TZUGFeRDAccountingAccountTypeCodes);
var
  newItem : TZUGFeRDReceivableSpecifiedTradeAccountingAccount;
begin
  newItem := TZUGFeRDReceivableSpecifiedTradeAccountingAccount.Create;
  newItem.TradeAccountID := AccountID;
  newItem.TradeAccountTypeCode := AccountTypeCode;

  ReceivableSpecifiedTradeAccountingAccounts.Add(newItem);
end;

function TZUGFeRDInvoiceDescriptor._getNextLineId: string;
var
  highestLineId,i: Integer;
begin
  highestLineId := 0;

  for i := 0 to TradeLineItems.Count-1 do
  if TradeLineItems[i].AssociatedDocument <> nil then
  begin
    if StrToIntDef(TradeLineItems[i].AssociatedDocument.LineID,0) > highestLineId then
      highestLineId := StrToIntDef(TradeLineItems[i].AssociatedDocument.LineID,0);
  end;

  Result := (highestLineId + 1).ToString;
end;

end.
