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

unit intf.ZUGFeRDPaymentMeansTypeCodes;

interface

uses
  System.SysUtils,
  intf.ZUGFeRDHelper;

type
  /// <summary>
  /// Adopted to ZUGFeRD 1.0, German description from ZUGFeRD specification
  /// </summary>
  TZUGFeRDPaymentMeansTypeCodes = (
    {.DefinitionStart}
    // automatically converted by PSC#ToDelphiDefinition

    /// <summary>
    /// Instrument not defined
    /// </summary>
    [EnumStringValue('1')]
    InstrumentNotDefined,

    /// <summary>
    /// Automated clearing house (ACH) credit
    /// </summary>
    [EnumStringValue('2')]
    AchCredit,

    /// <summary>
    /// Automated clearing house (ACH) debit
    /// </summary>
    [EnumStringValue('3')]
    AchDebit,

    /// <summary>
    /// Automated clearing house (ACH) demand debit reversal
    /// </summary>
    [EnumStringValue('4')]
    AchDemandDebitReversal,

    /// <summary>
    /// Automated clearing house (ACH) demand credit reversal
    /// </summary>
    [EnumStringValue('5')]
    AchDemandCreditReversal,

    /// <summary>
    /// Automated clearing house (ACH) demand credit
    /// </summary>
    [EnumStringValue('6')]
    AchDemandCredit,

    /// <summary>
    /// Automated clearing house (ACH) demand debit
    /// </summary>
    [EnumStringValue('7')]
    AchDemandDebit,

    /// <summary>
    /// Hold
    /// </summary>
    [EnumStringValue('8')]
    Hold,

    /// <summary>
    /// National or regional clearing
    /// </summary>
    [EnumStringValue('9')]
    NationalOrRegionalClearing,

    /// <summary>
    /// In cash
    /// </summary>
    [EnumStringValue('10')]
    InCash,

    /// <summary>
    /// ACH savings credit reversal
    /// </summary>
    [EnumStringValue('11')]
    AchSavingsCreditReversal,

    /// <summary>
    /// ACH savings debit reversal
    /// </summary>
    [EnumStringValue('12')]
    AchSavingsDebitReversal,

    /// <summary>
    /// ACH savings credit
    /// </summary>
    [EnumStringValue('13')]
    AchSavingsCredit,

    /// <summary>
    /// ACH savings debit
    /// </summary>
    [EnumStringValue('14')]
    AchSavingsDebit,

    /// <summary>
    /// Bookentry credit
    /// </summary>
    [EnumStringValue('15')]
    BookentryCredit,

    /// <summary>
    /// Bookentry debit
    /// </summary>
    [EnumStringValue('16')]
    BookentryDebit,

    /// <summary>
    /// ACH demand cash concentration/disbursement (CCD) credit
    /// </summary>
    [EnumStringValue('17')]
    AchDemandCcdCredit,

    /// <summary>
    /// ACH demand cash concentration/disbursement (CCD) debit
    /// </summary>
    [EnumStringValue('18')]
    AchDemandCcdDebit,

    /// <summary>
    /// ACH demand corporate trade payment (CTP) credit
    /// </summary>
    [EnumStringValue('19')]
    AchDemandCtpCredit,

    /// <summary>
    /// Cheque
    /// </summary>
    [EnumStringValue('20')]
    Cheque,

    /// <summary>
    /// Banker's draft
    /// </summary>
    [EnumStringValue('21')]
    BankersDraft,

    /// <summary>
    /// Certified banker's draft
    /// </summary>
    [EnumStringValue('22')]
    CertifiedBankersDraft,

    /// <summary>
    /// Bank cheque (issued by a banking or similar establishment)
    /// </summary>
    [EnumStringValue('23')]
    BankCheque,

    /// <summary>
    /// Bill of exchange awaiting acceptance
    /// </summary>
    [EnumStringValue('24')]
    BillOfExchangeAwaitingAcceptance,

    /// <summary>
    /// Certified cheque
    /// </summary>
    [EnumStringValue('25')]
    CertifiedCheque,

    /// <summary>
    /// Local cheque
    /// </summary>
    [EnumStringValue('26')]
    LocalCheque,

    /// <summary>
    /// ACH demand corporate trade payment (CTP) debit
    /// </summary>
    [EnumStringValue('27')]
    AchDemandCtpDebit,

    /// <summary>
    /// ACH demand corporate trade exchange (CTX) credit
    /// </summary>
    [EnumStringValue('28')]
    AchDemandCtxCredit,

    /// <summary>
    /// ACH demand corporate trade exchange (CTX) debit
    /// </summary>
    [EnumStringValue('29')]
    AchDemandCtxDebit,

    /// <summary>
    /// Credit transfer (non-SEPA)
    /// </summary>
    [EnumStringValue('30')]
    CreditTransferNonSEPA,

    /// <summary>
    /// Debit transfer (non-SEPA)
    /// </summary>
    [EnumStringValue('31')]
    DebitTransferNonSEPA,

    /// <summary>
    /// ACH demand cash concentration/disbursement plus (CCD+)
    /// </summary>
    [EnumStringValue('32')]
    AchDemandCcdPlus,

    /// <summary>
    /// ACH demand cash concentration/disbursement plus (CCD+)
    /// </summary>
    [EnumStringValue('33')]
    AchDemandCcdPlusDuplicate,

    /// <summary>
    /// ACH prearranged payment and deposit (PPD)
    /// </summary>
    [EnumStringValue('34')]
    AchPrearrangedPaymentDepositPpd,

    /// <summary>
    /// ACH savings cash concentration/disbursement (CCD) credit
    /// </summary>
    [EnumStringValue('35')]
    AchSavingsCcdCredit,

    /// <summary>
    /// ACH savings cash concentration/disbursement (CCD) debit
    /// </summary>
    [EnumStringValue('36')]
    AchSavingsCcdDebit,

    /// <summary>
    /// ACH savings corporate trade payment (CTP) credit
    /// </summary>
    [EnumStringValue('37')]
    AchSavingsCtpCredit,

    /// <summary>
    /// ACH savings corporate trade payment (CTP) debit
    /// </summary>
    [EnumStringValue('38')]
    AchSavingsCtpDebit,

    /// <summary>
    /// ACH savings corporate trade exchange (CTX) credit
    /// </summary>
    [EnumStringValue('39')]
    AchSavingsCtxCredit,

    /// <summary>
    /// ACH savings corporate trade exchange (CTX) debit
    /// </summary>
    [EnumStringValue('40')]
    AchSavingsCtxDebit,

    /// <summary>
    /// ACH savings cash concentration/disbursement plus (CCD+)
    /// </summary>
    [EnumStringValue('41')]
    AchSavingsCcdPlus,

    /// <summary>
    /// Payment to bank account
    /// </summary>
    [EnumStringValue('42')]
    PaymentToBankAccount,

    /// <summary>
    /// ACH savings cash concentration/disbursement plus (CCD+)
    /// </summary>
    [EnumStringValue('43')]
    AchSavingsCcdPlusDuplicate,

    /// <summary>
    /// Accepted bill of exchange
    /// </summary>
    [EnumStringValue('44')]
    AcceptedBillOfExchange,

    /// <summary>
    /// Referenced home-banking credit transfer
    /// </summary>
    [EnumStringValue('45')]
    ReferencedHomeBankingCreditTransfer,

    /// <summary>
    /// Interbank debit transfer
    /// </summary>
    [EnumStringValue('46')]
    InterbankDebitTransfer,

    /// <summary>
    /// Home-banking debit transfer
    /// </summary>
    [EnumStringValue('47')]
    HomeBankingDebitTransfer,

    /// <summary>
    /// Bank card (Use for all payment cards)
    /// </summary>
    [EnumStringValue('48')]
    BankCard,

    /// <summary>
    /// Direct debit
    /// </summary>
    [EnumStringValue('49')]
    DirectDebit,

    /// <summary>
    /// Payment by postgiro
    /// </summary>
    [EnumStringValue('50')]
    PaymentByPostgiro,

    /// <summary>
    /// FR, norme 6 97-Telereglement CFONB (French Organisation for Banking Standards) -
    /// Option A A French standard procedure that allows a debtor to pay an amount
    /// due to a creditor. The creditor will forward it to its bank, which
    /// will collect the money on the bank account of the debtor.
    /// </summary>
    [EnumStringValue('51')]
    FrCfonb,

    /// <summary>
    /// Urgent commercial payment
    /// </summary>
    [EnumStringValue('52')]
    UrgentCommercialPayment,

    /// <summary>
    /// Urgent Treasury Payment
    /// </summary>
    [EnumStringValue('53')]
    UrgentTreasuryPayment,

    /// <summary>
    /// Credit card
    /// </summary>
    [EnumStringValue('54')]
    CreditCard,

    /// <summary>
    /// Debit card
    /// </summary>
    [EnumStringValue('55')]
    DebitCard,

    /// <summary>
    /// Bankgiro
    /// </summary>
    [EnumStringValue('56')]
    Bankgiro,

    /// <summary>
    /// Standing agreement (Contractual payment means)
    /// </summary>
    [EnumStringValue('57')]
    StandingAgreement,

    /// <summary>
    /// SEPA credit transfer (SEPA)
    /// </summary>
    [EnumStringValue('58')]
    SEPACreditTransfer,

    /// <summary>
    /// SEPA direct debit (SEPA)
    /// </summary>
    [EnumStringValue('59')]
    SEPADirectDebit,

    /// <summary>
    /// Promissory note
    /// </summary>
    [EnumStringValue('60')]
    PromissoryNote,

    /// <summary>
    /// Promissory note signed by the debtor
    /// </summary>
    [EnumStringValue('61')]
    PromissoryNoteSignedByDebtor,

    /// <summary>
    /// Promissory note signed by the debtor and endorsed by a bank
    /// </summary>
    [EnumStringValue('62')]
    PromissoryNoteSignedByDebtorEndorsedByBank,

    /// <summary>
    /// Payment by an unconditional promise in writing made by the debtor
    /// to another person, signed by the debtor and endorsed by
    /// a third party, engaging to pay on demand or at a fixed
    /// or determinable future time a sum certain in money, to order
    /// or to bearer.
    /// </summary>
    [EnumStringValue('63')]
    PromissoryNoteSignedByDebtorEndorsedByOther,

    /// <summary>
    /// Promissory note signed by a bank
    /// </summary>
    [EnumStringValue('64')]
    PromissoryNoteSignedByBank,

    /// <summary>
    /// Payment by an unconditional promise in writing made by the bank
    /// to another person, signed by the bank and endorsed by another
    /// bank, engaging to pay on demand or at a fixed or determinable
    /// future time a sum certain in money, to order or to bearer.
    /// </summary>
    [EnumStringValue('65')]
    PromissoryNoteSignedByBankEndorsedByOther,

    /// <summary>
    /// Promissory note signed by a third party
    /// </summary>
    [EnumStringValue('66')]
    PromissoryNoteSignedByThirdParty,

    /// <summary>
    /// Payment by an unconditional promise in writing made by a
    /// third party to another person, signed by the third party
    /// and endorsed by a bank, engaging to pay on demand or at
    /// a fixed or determinable future time a sum certain in money,
    /// to order or to bearer.
    /// </summary>
    [EnumStringValue('67')]
    PromissoryNoteSignedByThirdPartyEndorsedByOther,

    /// <summary>
    /// Online payment service
    /// </summary>
    [EnumStringValue('68')]
    OnlinePaymentService,

    /// <summary>
    /// Transfer Advice
    /// </summary>
    [EnumStringValue('69')]
    TransferAdvice,

    /// <summary>
    /// Bill drawn by the creditor on the debtor
    /// </summary>
    [EnumStringValue('70')]
    BillDrawnByCreditorOnDebtor,

    /// <summary>
    /// Bill drawn by the creditor on a bank
    /// </summary>
    [EnumStringValue('74')]
    BillDrawnByCreditorOnBank,

    /// <summary>
    /// Bill drawn by the creditor, endorsed by another bank
    /// </summary>
    [EnumStringValue('75')]
    BillDrawnByCreditorEndorsedByAnotherBank,

    /// <summary>
    /// Bill drawn by the creditor on a bank and endorsed by a third party.
    /// </summary>
    [EnumStringValue('76')]
    BillDrawnByCreditorOnBankEndorsedByOther,

    /// <summary>
    /// Bill drawn by the creditor on a third party
    /// </summary>
    [EnumStringValue('77')]
    BillDrawnByCreditorOnThirdParty,

    /// <summary>
    /// Bill drawn by creditor on third party, accepted and endorsed by bank.
    /// </summary>
    [EnumStringValue('78')]
    BillDrawnByCreditorOnThirdPartyAccepted,

    /// <summary>
    /// Not transferable banker's draft
    /// </summary>
    [EnumStringValue('91')]
    NotTransferableBankersDraft,

    /// <summary>
    /// Not transferable local cheque
    /// </summary>
    [EnumStringValue('92')]
    NotTransferableLocalCheque,

    /// <summary>
    /// Reference giro
    /// </summary>
    [EnumStringValue('93')]
    ReferenceGiro,

    /// <summary>
    /// Urgent giro
    /// </summary>
    [EnumStringValue('94')]
    UrgentGiro,

    /// <summary>
    /// Free format giro
    /// </summary>
    [EnumStringValue('95')]
    FreeFormatGiro,

    /// <summary>
    /// Requested method for payment was not used
    /// </summary>
    [EnumStringValue('96')]
    RequestedMethodForPaymentNotUsed,

    /// <summary>
    /// Clearing between partners
    /// </summary>
    [EnumStringValue('97')]
    ClearingBetweenPartners,

    /// <summary>
    /// JP, Electronically Recorded Monetary Claims
    /// </summary>
    [EnumStringValue('98')]
    ElectronicallyRecordedMonetaryClaimsJP,

    /// <summary>
    /// Mutually defined
    /// </summary>
    [EnumStringValue('ZZZ')]
    MutuallyDefined,


    /// <summary>
    /// Unknown value
    /// </summary>
    Unknown
    {.DefinitionEnd}
  );

implementation

procedure Map (EnumValue: TZUGFeRDPaymentMeansTypeCodes; StringValue: string); inline;
begin
  TEnumExtensions<TZUGFeRDPaymentMeansTypeCodes>.RegisterMapping(EnumValue, StringValue)
end;

procedure InitMapping;
begin
  {.MapStart}
  // Mapping generated by PSDelphiDefinitionMapper
  Map(InstrumentNotDefined,                            '1');
  Map(AchCredit,                                       '2');
  Map(AchDebit,                                        '3');
  Map(AchDemandDebitReversal,                          '4');
  Map(AchDemandCreditReversal,                         '5');
  Map(AchDemandCredit,                                 '6');
  Map(AchDemandDebit,                                  '7');
  Map(Hold,                                            '8');
  Map(NationalOrRegionalClearing,                      '9');
  Map(InCash,                                          '10');
  Map(AchSavingsCreditReversal,                        '11');
  Map(AchSavingsDebitReversal,                         '12');
  Map(AchSavingsCredit,                                '13');
  Map(AchSavingsDebit,                                 '14');
  Map(BookentryCredit,                                 '15');
  Map(BookentryDebit,                                  '16');
  Map(AchDemandCcdCredit,                              '17');
  Map(AchDemandCcdDebit,                               '18');
  Map(AchDemandCtpCredit,                              '19');
  Map(Cheque,                                          '20');
  Map(BankersDraft,                                    '21');
  Map(CertifiedBankersDraft,                           '22');
  Map(BankCheque,                                      '23');
  Map(BillOfExchangeAwaitingAcceptance,                '24');
  Map(CertifiedCheque,                                 '25');
  Map(LocalCheque,                                     '26');
  Map(AchDemandCtpDebit,                               '27');
  Map(AchDemandCtxCredit,                              '28');
  Map(AchDemandCtxDebit,                               '29');
  Map(CreditTransferNonSEPA,                           '30');
  Map(DebitTransferNonSEPA,                            '31');
  Map(AchDemandCcdPlus,                                '32');
  Map(AchDemandCcdPlusDuplicate,                       '33');
  Map(AchPrearrangedPaymentDepositPpd,                 '34');
  Map(AchSavingsCcdCredit,                             '35');
  Map(AchSavingsCcdDebit,                              '36');
  Map(AchSavingsCtpCredit,                             '37');
  Map(AchSavingsCtpDebit,                              '38');
  Map(AchSavingsCtxCredit,                             '39');
  Map(AchSavingsCtxDebit,                              '40');
  Map(AchSavingsCcdPlus,                               '41');
  Map(PaymentToBankAccount,                            '42');
  Map(AchSavingsCcdPlusDuplicate,                      '43');
  Map(AcceptedBillOfExchange,                          '44');
  Map(ReferencedHomeBankingCreditTransfer,             '45');
  Map(InterbankDebitTransfer,                          '46');
  Map(HomeBankingDebitTransfer,                        '47');
  Map(BankCard,                                        '48');
  Map(DirectDebit,                                     '49');
  Map(PaymentByPostgiro,                               '50');
  Map(FrCfonb,                                         '51');
  Map(UrgentCommercialPayment,                         '52');
  Map(UrgentTreasuryPayment,                           '53');
  Map(CreditCard,                                      '54');
  Map(DebitCard,                                       '55');
  Map(Bankgiro,                                        '56');
  Map(StandingAgreement,                               '57');
  Map(SEPACreditTransfer,                              '58');
  Map(SEPADirectDebit,                                 '59');
  Map(PromissoryNote,                                  '60');
  Map(PromissoryNoteSignedByDebtor,                    '61');
  Map(PromissoryNoteSignedByDebtorEndorsedByBank,      '62');
  Map(PromissoryNoteSignedByDebtorEndorsedByOther,     '63');
  Map(PromissoryNoteSignedByBank,                      '64');
  Map(PromissoryNoteSignedByBankEndorsedByOther,       '65');
  Map(PromissoryNoteSignedByThirdParty,                '66');
  Map(PromissoryNoteSignedByThirdPartyEndorsedByOther, '67');
  Map(OnlinePaymentService,                            '68');
  Map(TransferAdvice,                                  '69');
  Map(BillDrawnByCreditorOnDebtor,                     '70');
  Map(BillDrawnByCreditorOnBank,                       '74');
  Map(BillDrawnByCreditorEndorsedByAnotherBank,        '75');
  Map(BillDrawnByCreditorOnBankEndorsedByOther,        '76');
  Map(BillDrawnByCreditorOnThirdParty,                 '77');
  Map(BillDrawnByCreditorOnThirdPartyAccepted,         '78');
  Map(NotTransferableBankersDraft,                     '91');
  Map(NotTransferableLocalCheque,                      '92');
  Map(ReferenceGiro,                                   '93');
  Map(UrgentGiro,                                      '94');
  Map(FreeFormatGiro,                                  '95');
  Map(RequestedMethodForPaymentNotUsed,                '96');
  Map(ClearingBetweenPartners,                         '97');
  Map(ElectronicallyRecordedMonetaryClaimsJP,          '98');
  Map(MutuallyDefined,                                 'ZZZ');
  Map(Unknown,                                         'Unknown');
{.MapEnd}
end;

Initialization
  InitMapping;
end.


