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

unit intf.ZUGFeRDInvoiceTypes;

interface

uses
  System.SysUtils,
  intf.ZUGFeRDHelper;

type
  /// <summary>
  /// Type codes for the various kinds of documents that can be represented using ZUGFeRD.
  /// </summary>
  TZUGFeRDInvoiceType = (
    {.DefinitionStart}
    // automatically converted by PSC#ToDelphiDefinition

    /// <summary>
    /// Request for payment
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('71')]
    RequestForPayment,

    /// <summary>
    /// Debit note related to goods or services
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('80')]
    DebitNoteRelatedToGoodsOrServices,

    /// <summary>
    /// Credit note related to goods or services
    /// EN16931 interpretation: Credit Note
    /// </summary>
    [EnumStringValue('81')]
    CreditNoteRelatedToGoodsOrServices,

    /// <summary>
    /// Metered services invoice
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('82')]
    MeteredServicesInvoice,

    /// <summary>
    /// Credit note related to financial adjustments
    /// EN16931 interpretation: Credit Note
    /// </summary>
    [EnumStringValue('83')]
    CreditNoteRelatedToFinancialAdjustments,

    /// <summary>
    /// Debit note related to financial adjustments
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('84')]
    DebitnoteRelatedToFinancialAdjustments,

    /// <summary>
    /// Tax notification
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('102')]
    TaxNotification,

    /// <summary>
    /// Invoicing data sheet
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('130')]
    InvoicingDataSheet,

    /// <summary>
    /// Direct payment valuation
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('202')]
    DirectPaymentValuation,

    /// <summary>
    /// Provisional payment valuation
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('203')]
    ProvisionalPaymentValuation,

    /// <summary>
    /// Payment valuation
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('204')]
    PaymentValuation,

    /// <summary>
    /// Interim application for payment
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('211')]
    InterimApplicationForPayment,

    /// <summary>
    /// Final payment request based on completion of work
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('218')]
    FinalPaymentRequestBasedOnCompletionOfWork,

    /// <summary>
    /// Payment request for completed units
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('219')]
    PaymentRequestForCompletedUnits,

    /// <summary>
    /// Self billed credit note
    /// EN16931 interpretation: Credit Note
    /// </summary>
    [EnumStringValue('261')]
    SelfBilledCreditNote,

    /// <summary>
    /// Consolidated credit note - goods and services
    /// EN16931 interpretation: Credit Note
    /// </summary>
    [EnumStringValue('262')]
    ConsolidatedCreditNoteGoodsAndServices,

    /// <summary>
    /// Price variation invoice
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('295')]
    PriceVariationInvoice,

    /// <summary>
    /// Credit note for price variation
    /// EN16931 interpretation: Credit Note
    /// </summary>
    [EnumStringValue('296')]
    CreditNoteForPriceVariation,

    /// <summary>
    /// Delcredere credit note
    /// EN16931 interpretation: Credit Note
    /// </summary>
    [EnumStringValue('308')]
    DelcredereCreditNote,

    /// <summary>
    /// Proforma invoice
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('325')]
    ProformaInvoice,

    /// <summary>
    /// Partial invoice
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('326')]
    PartialInvoice,

    /// <summary>
    /// Commercial invoice which includes a packing list
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('331')]
    CommercialInvoiceWithPackingList,

    /// <summary>
    /// Commercial invoice
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('380')]
    Invoice,

    /// <summary>
    /// Credit note
    /// EN16931 interpretation: Credit Note
    /// </summary>
    [EnumStringValue('381')]
    CreditNote,

    /// <summary>
    /// Commission note
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('382')]
    CommissionNote,

    /// <summary>
    /// Debit note
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('383')]
    DebitNote,

    /// <summary>
    /// Corrected invoice
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('384')]
    Correction,

    /// <summary>
    /// Consolidated invoice
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('385')]
    ConsolidatedInvoice,

    /// <summary>
    /// Prepayment invoice
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('386')]
    PrepaymentInvoice,

    /// <summary>
    /// Hire invoice
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('387')]
    HireInvoice,

    /// <summary>
    /// Tax invoice
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('388')]
    TaxInvoice,

    /// <summary>
    /// Self-billed invoice
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('389')]
    SelfBilledInvoice,

    /// <summary>
    /// Delcredere invoice
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('390')]
    DelcredereInvoice,

    /// <summary>
    /// Factored invoice
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('393')]
    FactoredInvoice,

    /// <summary>
    /// Lease invoice
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('394')]
    LeaseInvoice,

    /// <summary>
    /// Consignment invoice
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('395')]
    ConsignmentInvoice,

    /// <summary>
    /// Factored credit note
    /// EN16931 interpretation: Credit Note
    /// </summary>
    [EnumStringValue('396')]
    FactoredCreditNote,

    /// <summary>
    /// Optical Character Reading (OCR) payment credit note
    /// EN16931 interpretation: Credit Note
    /// </summary>
    [EnumStringValue('420')]
    OcrPaymentCreditNote,

    /// <summary>
    /// Debit advice
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('456')]
    DebitAdvice,

    /// <summary>
    /// Reversal of debit
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('457')]
    ReversalOfDebit,

    /// <summary>
    /// Reversal of credit
    /// EN16931 interpretation: Credit Note
    /// </summary>
    [EnumStringValue('458')]
    ReversalOfCredit,

    /// <summary>
    /// Self-billed corrective invoice, invoice type, Corrected
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('471')]
    SelfBilledCorrectiveInvoice,

    /// <summary>
    /// Factored Corrective Invoice, invoice type, Corrected
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('472')]
    FactoredCorrectiveInvoice,

    /// <summary>
    /// Self billed Factored corrective invoice, invoice type, Corrected
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('473')]
    SelfBilledFactoredCorrectiveInvoice,

    /// <summary>
    /// Self Prepayment invoice, invoice type, Original
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('500')]
    SelfPrepaymentInvoice,

    /// <summary>
    /// Self billed factored invoice, invoice type, Original
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('501')]
    SelfBilledFactoredInvoice,

    /// <summary>
    /// Self billed factored Credit Note, Credit note type, Corrected
    /// EN16931 interpretation: Credit Note
    /// </summary>
    [EnumStringValue('502')]
    SelfBilledFactoredCreditNote,

    /// <summary>
    /// Prepayment credit note, credit note type, Corrected
    /// EN16931 interpretation: Credit Note
    /// </summary>
    [EnumStringValue('503')]
    PrepaymentCreditNoteCorrected,

    /// <summary>
    /// Self billed debit note
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('527')]
    SelfBilledDebitNote,

    /// <summary>
    /// Forwarder's credit note
    /// EN16931 interpretation: Credit Note
    /// </summary>
    [EnumStringValue('532')]
    ForwardersCreditNote,

    /// <summary>
    /// Forwarder's invoice discrepancy report
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('553')]
    ForwardersInvoiceDiscrepancyReport,

    /// <summary>
    /// Insurer's invoice
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('575')]
    InsurersInvoice,

    /// <summary>
    /// Forwarder's invoice
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('623')]
    ForwardersInvoice,

    /// <summary>
    /// Port charges documents
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('633')]
    PortChargesDocuments,

    /// <summary>
    /// Invoice information for accounting purposes
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('751')]
    InvoiceInformation,

    /// <summary>
    /// Freight invoice
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('780')]
    FreightInvoice,

    /// <summary>
    /// Claim notification
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('817')]
    ClaimNotification,

    /// <summary>
    /// Consular invoice
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('870')]
    ConsularInvoice,

    /// <summary>
    /// Partial construction invoice
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('875')]
    PartialConstructionInvoice,

    /// <summary>
    /// Partial final construction invoice
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('876')]
    PartialFinalConstructionInvoice,

    /// <summary>
    /// Final construction invoice
    /// EN16931 interpretation: Invoice
    /// </summary>
    [EnumStringValue('877')]
    FinalConstructionInvoice,


    /// <summary>
    /// Unknown value
    /// </summary>
    Unknown
    {.DefinitionEnd}

    // manually defined in ZfD
    ,

    /// <summary>
    /// Corrected invoice (1380)
    /// is an Invoice
    /// Old ZUGFeRD variant, use
    /// Corrected Invoice (384) instead
    /// </summary>
    [EnumStringValue('1380')]
    CorrectionOld
  );

implementation

procedure Map (EnumValue: TZUGFeRDInvoiceType; StringValue: string); inline;
begin
  TEnumExtensions<TZUGFeRDInvoiceType>.RegisterMapping(EnumValue, StringValue)
end;

procedure InitMapping;
begin
  {.MapStart}
  // Mapping generated by PSDelphiDefinitionMapper
  Map(RequestForPayment,                          '71');
  Map(DebitNoteRelatedToGoodsOrServices,          '80');
  Map(CreditNoteRelatedToGoodsOrServices,         '81');
  Map(MeteredServicesInvoice,                     '82');
  Map(CreditNoteRelatedToFinancialAdjustments,    '83');
  Map(DebitnoteRelatedToFinancialAdjustments,     '84');
  Map(TaxNotification,                            '102');
  Map(InvoicingDataSheet,                         '130');
  Map(DirectPaymentValuation,                     '202');
  Map(ProvisionalPaymentValuation,                '203');
  Map(PaymentValuation,                           '204');
  Map(InterimApplicationForPayment,               '211');
  Map(FinalPaymentRequestBasedOnCompletionOfWork, '218');
  Map(PaymentRequestForCompletedUnits,            '219');
  Map(SelfBilledCreditNote,                       '261');
  Map(ConsolidatedCreditNoteGoodsAndServices,     '262');
  Map(PriceVariationInvoice,                      '295');
  Map(CreditNoteForPriceVariation,                '296');
  Map(DelcredereCreditNote,                       '308');
  Map(ProformaInvoice,                            '325');
  Map(PartialInvoice,                             '326');
  Map(CommercialInvoiceWithPackingList,           '331');
  Map(Invoice,                                    '380');
  Map(CreditNote,                                 '381');
  Map(CommissionNote,                             '382');
  Map(DebitNote,                                  '383');
  Map(Correction,                                 '384');
  Map(ConsolidatedInvoice,                        '385');
  Map(PrepaymentInvoice,                          '386');
  Map(HireInvoice,                                '387');
  Map(TaxInvoice,                                 '388');
  Map(SelfBilledInvoice,                          '389');
  Map(DelcredereInvoice,                          '390');
  Map(FactoredInvoice,                            '393');
  Map(LeaseInvoice,                               '394');
  Map(ConsignmentInvoice,                         '395');
  Map(FactoredCreditNote,                         '396');
  Map(OcrPaymentCreditNote,                       '420');
  Map(DebitAdvice,                                '456');
  Map(ReversalOfDebit,                            '457');
  Map(ReversalOfCredit,                           '458');
  Map(SelfBilledCorrectiveInvoice,                '471');
  Map(FactoredCorrectiveInvoice,                  '472');
  Map(SelfBilledFactoredCorrectiveInvoice,        '473');
  Map(SelfPrepaymentInvoice,                      '500');
  Map(SelfBilledFactoredInvoice,                  '501');
  Map(SelfBilledFactoredCreditNote,               '502');
  Map(PrepaymentCreditNoteCorrected,              '503');
  Map(SelfBilledDebitNote,                        '527');
  Map(ForwardersCreditNote,                       '532');
  Map(ForwardersInvoiceDiscrepancyReport,         '553');
  Map(InsurersInvoice,                            '575');
  Map(ForwardersInvoice,                          '623');
  Map(PortChargesDocuments,                       '633');
  Map(InvoiceInformation,                         '751');
  Map(FreightInvoice,                             '780');
  Map(ClaimNotification,                          '817');
  Map(ConsularInvoice,                            '870');
  Map(PartialConstructionInvoice,                 '875');
  Map(PartialFinalConstructionInvoice,            '876');
  Map(FinalConstructionInvoice,                   '877');
  Map(Unknown,                                    'Unknown');
{.MapEnd}
  // manually defined in ZfD
  Map(CorrectionOld,                          '1380');
end;

Initialization
  InitMapping;
end.


