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

unit intf.ZUGFeRDAllowanceOrChargeIdentificationCodes;

interface

uses
  System.SysUtils,System.TypInfo,System.StrUtils
  ;

type
  /// <summary>
  /// https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:untdid.5189_3
  /// </summary>
  TZUGFeRDAllowanceOrChargeIdentificationCodes = (
    //HandlingCommission, //       Fee for the processing of documentary credit, collection and payment which are charged to the customer.                                                                                    '1';
    //AmendmentCommission, //       Fee for amendments in documentary credit and collection business (not extensions and increases of documentary credits).                                                                   '2';
    //AcceptanceCommission, //       Fee for the acceptance of draft in documentary credit and collection business which are drawn on us (also to be seen as a kind of 'guarantee commission').                               '3';
    //CommissionForObtainingAcceptance, //       Fee for obtaining an acceptance under collections on the basis of 'documents against acceptance'.                                                                            '4';
    //CommissionOnDelivery, //       Fee for delivery of documents without corresponding payment.                                                                                                                             '5';
    //AdvisingCommission, //       Fee for advising documentary credits (charged also in case of confirmed credits).                                                                                                          '6';
    //ConfirmationCommission, //       Fee for confirmation of credit.                                                                                                                                                        '7';
    //DeferredPaymentCommission, //       Fee for the deferred payment period under documentary credits confirmed by bank. This fee are charges for the period from presentation of the document until due date of payment.   '8';
    //CommissionForTakingUpDocuments, //       Fee charged to the foreign bank for the processing of documentary credit.                                                                                                      '9';
    //OpeningCommission, //       Fee for opening revocable documentary credit.                                                                                                                                               '10';
    //FeeForPaymentUnderReserve, //       Fee charged to the customer for discrepancies in credit documents in the case of which the bank have to stipulate payment under reserve.                                            '11';
    //DiscrepancyFee, //       Fee charged to the foreign bank for discrepancies in credit documents.                                                                                                                         '12';
    //DomicilationCommission, //       Fee for the domicilation of bills with the bank.                                                                                                                                       '13';
    //CommissionForReleaseOfGoods, //       Commission for the release of goods sent to the bank.                                                                                                                             '14';
    //CollectionCommission, //       Fee for settling collections on the basis of 'documents against payments'.                                                                                                               '15';
    //NegotiationCommission, //       Fee for the purchase of documents under sight credit for the first ten days.                                                                                                            '16';
    //ReturnCommission, //       Fee for cheques, bills and collections returned unpaid and/or recalled.                                                                                                                      '17';
    //BLSplittingCharges, //       Fee for the splitting of bills of lading.                                                                                                                                                  '18';
    //TrustCommission, //       Fee for the handling on a fiduciary basis of imported goods that have been warehoused.                                                                                                        '19';
    //TransferCommission, //       Fee for the transfer of transferable documentary credits.                                                                                                                                  '20';
    //CommissionForOpeningIrrevocableDocumentaryCredits, //       Fee for opening irrevocable documentary credits.                                                                                                            '21';
    //PreadviceCommission, //       Fee for the pre-advice of a documentary credit.                                                                                                                                           '22';
    //SupervisoryCommission, //       Fee for the supervising unconfirmed documentary credits with a deferred payment period.                                                                                                 '23';
    //ModelCharges, //       Fee for decoding telex messages.                                                                                                                                                                 '24';
    //RiskCommission, //       Commission in addition to the confirmation commission for documentary credits from sensitive countries.                                                                                        '25';
    //GuaranteeCommission, //       Commission for drawing up guaranties.                                                                                                                                                     '26';
    //ReimbursementCommission, //       Fee for reimbursement of, for example, documentary credits.                                                                                                                           '27';
    //StampDuty, //       Tax payable on bills in accordance with national bill of exchange legislation.                                                                                                                      '28';
    //Brokerage, //       Brokers commission arising, in trade with foreign currencies.                                                                                                                                       '29';
    //BankCharges, //       Charges deducted/claimed by other banks involved in the transaction.                                                                                                                              '30';
    //BankChargesInformation, //       Charges not included in the total charge amount i.e. the charges are for information only.                                                                                             '31';
    //CourierFee, //       Fee for use of courier service.                                                                                                                                                                    '32';
    //PhoneFee, //       Fee for use of phone.                                                                                                                                                                                '33';
    //PostageFee, //       Fee for postage.                                                                                                                                                                                   '34';
    //SWIFTFee, //       Fee for use of S.W.I.F.T.                                                                                                                                                                            '35';
    //TelexFee, //       Fee for telex.                                                                                                                                                                                       '36';
    //PenaltyForLateDeliveryOfDocuments, //       Penalty imposed when documents are delivered late.                                                                                                                          '37';
    //PenaltyForLateDeliveryOfValuationOfWorks, //       Penalty imposed when valuation of works is delivered late.                                                                                                           '38';
    //PenaltyForExecutionOfWorksBehindSchedule, //       Penalty imposed when the execution of works is behind schedule.                                                                                                      '39';
    //OtherPenalties, //       Penalty imposed for other reasons.                                                                                                                                                             '40';
    BonusForWorksAheadOfSchedule, //       Bonus for completing work ahead of schedule.                                                                                                                                     '41';
    OtherBonus, //       Bonus earned for other reasons.                                                                                                                                                                    '42';
    //ProjectManagementCost, //       Cost for project management.                                                                                                                                                            '44';
    //ProRataRetention, //       Proportional retention charge.                                                                                                                                                               '45';
    //ContractualRetention, //       Contractual retention charge.                                                                                                                                                            '46';
    //OtherRetentions, //       Retention charge not otherwise specified.                                                                                                                                                     '47';
    //InterestOnArrears, //       Interest for late payment.                                                                                                                                                                  '48';
    //Interest, //       Cost of using money.                                                                                                                                                                                 '49';
    //ChargePerCreditCover, //       Unit charge per credit cover established.                                                                                                                                                '50';
    //ChargePerUnusedCreditCover, //       Unit charge per unused credit cover.                                                                                                                                               '51';
    //MinimumCommission, //       Minimum commission charge.                                                                                                                                                                  '52';
    //FactoringCommission, //       Commission charged for factoring services.                                                                                                                                                '53';
    //ChamberOfCommerceCharge, //       Identifies the charges from the chamber of commerce.                                                                                                                                  '54';
    //TransferCharges, //       Charges for transfer.                                                                                                                                                                         '55';
    //RepatriationCharges, //       Charges for repatriation.                                                                                                                                                                 '56';
    //MiscellaneousCharges, //       Not specifically defined charges.                                                                                                                                                        '57';
    //ForeignExchangeCharges, //       Charges for foreign exchange.                                                                                                                                                          '58';
    //AgreedDebitInterestCharge, //       Charge for agreed debit interest                                                                                                                                                    '59';
    ManufacturersConsumerDiscount, //       A discount given by the manufacturer which should be passed on to the consumer.                                                                                                 '60';
    //FaxAdviceCharge, //       Charge for fax advice.                                                                                                                                                                        '61';
    DueToMilitaryStatus, //       Allowance granted because of the military status.                                                                                                                                         '62';
    DueToWorkAccident, //       Allowance granted to a victim of a work accident.                                                                                                                                           '63';
    SpecialAgreement, //       An allowance or charge as specified in a special agreement.                                                                                                                                  '64';
    ProductionErrorDiscount, //       A discount given for the purchase of a product with a production error.                                                                                                               '65';
    NewOutletDiscount, //       A discount given at the occasion of the opening of a new outlet.                                                                                                                            '66';
    SampleDiscount, //       A discount given for the purchase of a sample of a product.                                                                                                                                    '67';
    EndOfRangeDiscount, //       A discount given for the purchase of an end-of-range product.                                                                                                                              '68';
    //ChargeForACustomerSpecificFinish, //       A charge for the addition of a customer specific finish to a product.                                                                                                        '69';
    IncotermDiscount, //       A discount given for a specified Incoterm.                                                                                                                                                   '70';
    PointOfSalesThresholdAllowance, //       Allowance for reaching or exceeding an agreed sales threshold at the point of sales.                                                                                           '71';
    //TechnicalModificationCosts, //       Costs for technical modifications to a product.                                                                                                                                    '72';
    //JoborderProductionCosts, //       Costs of job-order production.                                                                                                                                                        '73';
    //OffpremisesCosts, //       Expenses for non-local activities.                                                                                                                                                           '74';
    //AdditionalProcessingCosts, //       Costs of additional processing.                                                                                                                                                     '75';
    //AttestingCharge, //       Costs of official attestation.                                                                                                                                                                '76';
    //RushDeliverySurcharge, //       Charge for increased delivery speed.                                                                                                                                                    '77';
    //SpecialConstructionCosts, //       Charge for costs incurred as result of special constructions.                                                                                                                        '78';
    //FreightCharges, //       Amount to be paid for moving goods, by whatever means, from one place to another.                                                                                                              '79';
    //PackingCharge, //       Charge for packing.                                                                                                                                                                             '80';
    //RepairCharge, //       Charge for repair.                                                                                                                                                                               '81';
    //LoadingCharge, //       Charge for loading.                                                                                                                                                                             '82';
    //SetupCharge, //       Charge for setup.                                                                                                                                                                                 '83';
    //TestingCharge, //       Charge for testing.                                                                                                                                                                             '84';
    //WarehousingCharge, //       Charge for storage and handling.                                                                                                                                                            '85';
    //GoldSurcharge, //       Difference between current price and basic value contained in product price in relation to gold content.                                                                                        '86';
    //CopperSurcharge, //       Difference between current price and basic value contained in product price in relation to copper content.                                                                                    '87';
    MaterialSurchargeDeduction, //       Surcharge/deduction, calculated for higher/ lower material's consumption.                                                                                                          '88';
    //LeadSurcharge, //       Difference between current price and basic value contained in product price in relation to lead content.                                                                                        '89';
    //PriceIndexSurcharge, //       Higher/lower price, resulting from change in costs between the times of making offer and delivery.                                                                                        '90';
    //PlatinumSurcharge, //       Difference between current price and basic value contained in product price in relation to platinum content.                                                                                '91';
    //SilverSurcharge, //       Difference between current price and basic value contained in product price in relation to silver content.                                                                                    '92';
    //WolframSurcharge, //       Difference between current price and basic value contained in product price in relation to wolfram content.                                                                                  '93';
    //AluminumSurcharge, //       Difference between current price and basic value contained in product price in relation to aluminum content.                                                                                '94';
    Discount, //       A reduction from a usual or list price.                                                                                                                                                              '95';
    //Insurance, //       Charge for insurance.                                                                                                                                                                               '96';
    //MinimumOrderMinimumBillingCharge, //       Charge for minimum order or minimum billing.                                                                                                                                 '97';
    //MaterialSurchargeSspecialMaterials, //       Surcharge for (special) materials.                                                                                                                                         '98';
    //Surcharge, //       An additional amount added to the usual charge.                                                                                                                                                     '99';
    SpecialRebate, //       A return of part of an amount paid for goods or services, serving as a reduction or discount.                                                                                                   '100';
    //CarbonFootprintCharge, //       A monetary amount charged for carbon footprint related to a regulatory requirement.                                                                                                     '101';
    FixedLongTerm, //       A fixed long term allowance or charge.                                                                                                                                                          '102';
    Temporary, //       A temporary allowance or charge.                                                                                                                                                                    '103';
    Standard, //       The standard available allowance or charge.                                                                                                                                                          '104';
    YearlyTurnover, //       An allowance or charge based on yearly turnover.                                                                                                                                               '105';
    //WithheldTaxesAndSocialSecurityContributions//       The amount of taxes and contributions for social security, that is subtracted from the payable amount as it is to be paid separately.                             '106';

    Unknown
  );

  TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions = class
  public
    class function FromString(const s: string): TZUGFeRDAllowanceOrChargeIdentificationCodes;
    class function EnumToString(codes: TZUGFeRDAllowanceOrChargeIdentificationCodes): string;
  end;

implementation

{ TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions }

class function TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.EnumToString(
  codes: TZUGFeRDAllowanceOrChargeIdentificationCodes): string;
begin
  case codes of
    //HandlingCommission: Result :=                                '1';
    //AmendmentCommission: Result :=                               '2';
    //AcceptanceCommission: Result :=                              '3';
    //CommissionForObtainingAcceptance: Result :=                  '4';
    //CommissionOnDelivery: Result :=                              '5';
    //AdvisingCommission: Result :=                                '6';
    //ConfirmationCommission: Result :=                            '7';
    //DeferredPaymentCommission: Result :=                         '8';
    //CommissionForTakingUpDocuments: Result :=                    '9';
    //OpeningCommission: Result :=                                 '10';
    //FeeForPaymentUnderReserve: Result :=                         '11';
    //DiscrepancyFee: Result :=                                    '12';
    //DomicilationCommission: Result :=                            '13';
    //CommissionForReleaseOfGoods: Result :=                       '14';
    //CollectionCommission: Result :=                              '15';
    //NegotiationCommission: Result :=                             '16';
    //ReturnCommission: Result :=                                  '17';
    //BLSplittingCharges: Result :=                                '18';
    //TrustCommission: Result :=                                   '19';
    //TransferCommission: Result :=                                '20';
    //CommissionForOpeningIrrevocableDocumentaryCredits: Result := '21';
    //PreadviceCommission: Result :=                               '22';
    //SupervisoryCommission: Result :=                             '23';
    //ModelCharges: Result :=                                      '24';
    //RiskCommission: Result :=                                    '25';
    //GuaranteeCommission: Result :=                               '26';
    //ReimbursementCommission: Result :=                           '27';
    //StampDuty: Result :=                                         '28';
    //Brokerage: Result :=                                         '29';
    //BankCharges: Result :=                                       '30';
    //BankChargesInformation: Result :=                            '31';
    //CourierFee: Result :=                                        '32';
    //PhoneFee: Result :=                                          '33';
    //PostageFee: Result :=                                        '34';
    //SWIFTFee: Result :=                                          '35';
    //TelexFee: Result :=                                          '36';
    //PenaltyForLateDeliveryOfDocuments: Result :=                 '37';
    //PenaltyForLateDeliveryOfValuationOfWorks: Result :=          '38';
    //PenaltyForExecutionOfWorksBehindSchedule: Result :=          '39';
    //OtherPenalties: Result :=                                    '40';
    BonusForWorksAheadOfSchedule: Result :=                      '41';
    OtherBonus: Result :=                                        '42';
    //ProjectManagementCost: Result :=                             '44';
    //ProRataRetention: Result :=                                  '45';
    //ContractualRetention: Result :=                              '46';
    //OtherRetentions: Result :=                                   '47';
    //InterestOnArrears: Result :=                                 '48';
    //Interest: Result :=                                          '49';
    //ChargePerCreditCover: Result :=                              '50';
    //ChargePerUnusedCreditCover: Result :=                        '51';
    //MinimumCommission: Result :=                                 '52';
    //FactoringCommission: Result :=                               '53';
    //ChamberOfCommerceCharge: Result :=                           '54';
    //TransferCharges: Result :=                                   '55';
    //RepatriationCharges: Result :=                               '56';
    //MiscellaneousCharges: Result :=                              '57';
    //ForeignExchangeCharges: Result :=                            '58';
    //AgreedDebitInterestCharge: Result :=                         '59';
    ManufacturersConsumerDiscount: Result :=                     '60';
    //FaxAdviceCharge: Result :=                                   '61';
    DueToMilitaryStatus: Result :=                               '62';
    DueToWorkAccident: Result :=                                 '63';
    SpecialAgreement: Result :=                                  '64';
    ProductionErrorDiscount: Result :=                           '65';
    NewOutletDiscount: Result :=                                 '66';
    SampleDiscount: Result :=                                    '67';
    EndOfRangeDiscount: Result :=                                '68';
    //ChargeForACustomerSpecificFinish: Result :=                  '69';
    IncotermDiscount: Result :=                                  '70';
    PointOfSalesThresholdAllowance: Result :=                    '71';
    //TechnicalModificationCosts: Result :=                        '72';
    //JoborderProductionCosts: Result :=                           '73';
    //OffpremisesCosts: Result :=                                  '74';
    //AdditionalProcessingCosts: Result :=                         '75';
    //AttestingCharge: Result :=                                   '76';
    //RushDeliverySurcharge: Result :=                             '77';
    //SpecialConstructionCosts: Result :=                          '78';
    //FreightCharges: Result :=                                    '79';
    //PackingCharge: Result :=                                     '80';
    //RepairCharge: Result :=                                      '81';
    //LoadingCharge: Result :=                                     '82';
    //SetupCharge: Result :=                                       '83';
    //TestingCharge: Result :=                                     '84';
    //WarehousingCharge: Result :=                                 '85';
    //GoldSurcharge: Result :=                                     '86';
    //CopperSurcharge: Result :=                                   '87';
    MaterialSurchargeDeduction: Result :=                        '88';
    //LeadSurcharge: Result :=                                     '89';
    //PriceIndexSurcharge: Result :=                               '90';
    //PlatinumSurcharge: Result :=                                 '91';
    //SilverSurcharge: Result :=                                   '92';
    //WolframSurcharge: Result :=                                  '93';
    //AluminumSurcharge: Result :=                                 '94';
    Discount: Result :=                                          '95';
    //Insurance: Result :=                                         '96';
    //MinimumOrderMinimumBillingCharge: Result :=                  '97';
    //MaterialSurchargeSspecialMaterials: Result :=                '98';
    //Surcharge: Result :=                                         '99';
    SpecialRebate: Result :=                                     '100';
    //CarbonFootprintCharge: Result :=                             '101';
    FixedLongTerm: Result :=                                     '102';
    Temporary: Result :=                                         '103';
    Standard: Result :=                                          '104';
    YearlyTurnover: Result :=                                    '105';
    //WithheldTaxesAndSocialSecurityContributions: Result :=       '106';
    else Result := '';
  end;
end;

class function TZUGFeRDAllowanceOrChargeIdentificationCodesExtensions.FromString(
  const s: string): TZUGFeRDAllowanceOrChargeIdentificationCodes;
begin
  if SameText(s,'41') then
    Result := BonusForWorksAheadOfSchedule else
  if SameText(s,'42') then
    Result := OtherBonus else
  if SameText(s,'60') then
    Result := ManufacturersConsumerDiscount else
  if SameText(s,'62') then
    Result := DueToMilitaryStatus else
  if SameText(s,'63') then
    Result := DueToWorkAccident else
  if SameText(s,'64') then
    Result := SpecialAgreement else
  if SameText(s,'65') then
    Result := ProductionErrorDiscount else
  if SameText(s,'66') then
    Result := NewOutletDiscount else
  if SameText(s,'67') then
    Result := SampleDiscount else
  if SameText(s,'68') then
    Result := EndOfRangeDiscount else
  if SameText(s,'70') then
    Result := IncotermDiscount else
  if SameText(s,'71') then
    Result := PointOfSalesThresholdAllowance else
  if SameText(s,'88') then
    Result := MaterialSurchargeDeduction else
  if SameText(s,'95') then
    Result := Discount else
  if SameText(s,'100') then
    Result := SpecialRebate else
  if SameText(s,'102') then
    Result := FixedLongTerm else
  if SameText(s,'103') then
    Result := Temporary else
  if SameText(s,'104') then
    Result := Standard else
  if SameText(s,'105') then
    Result := YearlyTurnover else
  Result := Unknown;
end;

end.
