{* Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * 'License'); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * 'AS IS' BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.}

unit intf.ZUGFeRDChargeReasonCodes;

interface

uses
  System.SysUtils,System.StrUtils,
  intf.ZUGFeRDHelper;

type
    /// <summary>
    /// Reason codes according to UNCL5189 code list
    /// </summary>
  TZUGFeRDChargeReasonCodes = (
    {.DefinitionStart}
    // automatically converted by PSC#ToDelphiDefinition

    /// Advertising
    [EnumStringValue('AA')]
    Advertising,

    /// Telecommunication
    [EnumStringValue('AAA')]
    Telecommunication,

    /// Technical modification
    [EnumStringValue('AAC')]
    TechnicalModification,

    /// Job-order production
    [EnumStringValue('AAD')]
    JobOrderProduction,

    /// Outlays
    [EnumStringValue('AAE')]
    Outlays,

    /// Off-premises
    [EnumStringValue('AAF')]
    OffPremises,

    /// Additional processing
    [EnumStringValue('AAH')]
    AdditionalProcessing,

    /// Attesting
    [EnumStringValue('AAI')]
    Attesting,

    /// Acceptance
    [EnumStringValue('AAS')]
    Acceptance,

    /// Rush delivery
    [EnumStringValue('AAT')]
    RushDelivery,

    /// Special construction
    [EnumStringValue('AAV')]
    SpecialConstruction,

    /// Airport facilities
    [EnumStringValue('AAY')]
    AirportFacilities,

    /// Concession
    [EnumStringValue('AAZ')]
    Concession,

    /// Compulsory storage
    [EnumStringValue('ABA')]
    CompulsoryStorage,

    /// Fuel removal
    [EnumStringValue('ABB')]
    FuelRemoval,

    /// Into plane
    [EnumStringValue('ABC')]
    IntoPlane,

    /// Overtime
    [EnumStringValue('ABD')]
    Overtime,

    /// Tooling
    [EnumStringValue('ABF')]
    Tooling,

    /// Miscellaneous
    [EnumStringValue('ABK')]
    Miscellaneous,

    /// Additional packaging
    [EnumStringValue('ABL')]
    AdditionalPackaging,

    /// Dunnage
    [EnumStringValue('ABN')]
    Dunnage,

    /// Containerisation
    [EnumStringValue('ABR')]
    Containerisation,

    /// Carton packing
    [EnumStringValue('ABS')]
    CartonPacking,

    /// Hessian wrapped
    [EnumStringValue('ABT')]
    HessianWrapped,

    /// Polyethylene wrap packing
    [EnumStringValue('ABU')]
    PolyethyleneWrapPacking,

    /// Miscellaneous treatment
    [EnumStringValue('ACF')]
    MiscellaneousTreatment,

    /// Enamelling treatment
    [EnumStringValue('ACG')]
    EnamellingTreatment,

    /// Heat treatment
    [EnumStringValue('ACH')]
    HeatTreatment,

    /// Plating treatment
    [EnumStringValue('ACI')]
    PlatingTreatment,

    /// Painting
    [EnumStringValue('ACJ')]
    Painting,

    /// Polishing
    [EnumStringValue('ACK')]
    Polishing,

    /// Priming
    [EnumStringValue('ACL')]
    Priming,

    /// Preservation treatment
    [EnumStringValue('ACM')]
    PreservationTreatment,

    /// Fitting
    [EnumStringValue('ACS')]
    Fitting,

    /// Consolidation
    [EnumStringValue('ADC')]
    Consolidation,

    /// Bill of lading
    [EnumStringValue('ADE')]
    BillOfLading,

    /// Airbag
    [EnumStringValue('ADJ')]
    Airbag,

    /// Transfer
    [EnumStringValue('ADK')]
    Transfer,

    /// Slipsheet
    [EnumStringValue('ADL')]
    Slipsheet,

    /// Binding
    [EnumStringValue('ADM')]
    Binding,

    /// Repair or replacement of broken returnable package
    [EnumStringValue('ADN')]
    RepairOrReplacementOfBrokenReturnablePackage,

    /// Efficient logistics
    [EnumStringValue('ADO')]
    EfficientLogistics,

    /// Merchandising
    [EnumStringValue('ADP')]
    Merchandising,

    /// Product mix
    [EnumStringValue('ADQ')]
    ProductMix,

    /// Other services
    [EnumStringValue('ADR')]
    OtherServices,

    /// Pick-up
    [EnumStringValue('ADT')]
    PickUp,

    /// Chronic illness
    [EnumStringValue('ADW')]
    ChronicIllness,

    /// New product introduction
    [EnumStringValue('ADY')]
    NewProductIntroduction,

    /// Direct delivery
    [EnumStringValue('ADZ')]
    DirectDelivery,

    /// Diversion
    [EnumStringValue('AEA')]
    Diversion,

    /// Disconnect
    [EnumStringValue('AEB')]
    Disconnect,

    /// Distribution
    [EnumStringValue('AEC')]
    Distribution,

    /// Handling of hazardous cargo
    [EnumStringValue('AED')]
    HandlingOfHazardousCargo,

    /// Rents and leases
    [EnumStringValue('AEF')]
    RentsAndLeases,

    /// Location differential
    [EnumStringValue('AEH')]
    LocationDifferential,

    /// Aircraft refueling
    [EnumStringValue('AEI')]
    AircraftRefueling,

    /// Fuel shipped into storage
    [EnumStringValue('AEJ')]
    FuelShippedIntoStorage,

    /// Cash on delivery
    [EnumStringValue('AEK')]
    CashOnDelivery,

    /// Small order processing service
    [EnumStringValue('AEL')]
    SmallOrderProcessingService,

    /// Clerical or administrative services
    [EnumStringValue('AEM')]
    ClericalOrAdministrativeServices,

    /// Guarantee
    [EnumStringValue('AEN')]
    Guarantee,

    /// Collection and recycling
    [EnumStringValue('AEO')]
    CollectionAndRecycling,

    /// Copyright fee collection
    [EnumStringValue('AEP')]
    CopyrightFeeCollection,

    /// Veterinary inspection service
    [EnumStringValue('AES')]
    VeterinaryInspectionService,

    /// Pensioner service
    [EnumStringValue('AET')]
    PensionerService,

    /// Medicine free pass holder
    [EnumStringValue('AEU')]
    MedicineFreePassHolder,

    /// Environmental protection service
    [EnumStringValue('AEV')]
    EnvironmentalProtectionService,

    /// Environmental clean-up service
    [EnumStringValue('AEW')]
    EnvironmentalCleanUpService,

    /// National cheque processing service outside account area
    [EnumStringValue('AEX')]
    NationalChequeProcessingServiceOutsideAccountArea,

    /// National payment service outside account area
    [EnumStringValue('AEY')]
    NationalPaymentServiceOutsideAccountArea,

    /// National payment service within account area
    [EnumStringValue('AEZ')]
    NationalPaymentServiceWithinAccountArea,

    /// Adjustments
    [EnumStringValue('AJ')]
    Adjustments,

    /// Authentication
    [EnumStringValue('AU')]
    Authentication,

    /// Cataloguing
    [EnumStringValue('CA')]
    Cataloguing,

    /// Cartage
    [EnumStringValue('CAB')]
    Cartage,

    /// Certification
    [EnumStringValue('CAD')]
    Certification,

    /// Certificate of conformance
    [EnumStringValue('CAE')]
    CertificateOfConformance,

    /// Certificate of origin
    [EnumStringValue('CAF')]
    CertificateOfOrigin,

    /// Cutting
    [EnumStringValue('CAI')]
    Cutting,

    /// Consular service
    [EnumStringValue('CAJ')]
    ConsularService,

    /// Customer collection
    [EnumStringValue('CAK')]
    CustomerCollection,

    /// Payroll payment service
    [EnumStringValue('CAL')]
    PayrollPaymentService,

    /// Cash transportation
    [EnumStringValue('CAM')]
    CashTransportation,

    /// Home banking service
    [EnumStringValue('CAN')]
    HomeBankingService,

    /// Bilateral agreement service
    [EnumStringValue('CAO')]
    BilateralAgreementService,

    /// Insurance brokerage service
    [EnumStringValue('CAP')]
    InsuranceBrokerageService,

    /// Cheque generation
    [EnumStringValue('CAQ')]
    ChequeGeneration,

    /// Preferential merchandising location
    [EnumStringValue('CAR')]
    PreferentialMerchandisingLocation,

    /// Crane
    [EnumStringValue('CAS')]
    Crane,

    /// Special colour service
    [EnumStringValue('CAT')]
    SpecialColourService,

    /// Sorting
    [EnumStringValue('CAU')]
    Sorting,

    /// Battery collection and recycling
    [EnumStringValue('CAV')]
    BatteryCollectionAndRecycling,

    /// Product take back fee
    [EnumStringValue('CAW')]
    ProductTakeBackFee,

    /// Quality control released
    [EnumStringValue('CAX')]
    QualityControlReleased,

    /// Quality control held
    [EnumStringValue('CAY')]
    QualityControlHeld,

    /// Quality control embargo
    [EnumStringValue('CAZ')]
    QualityControlEmbargo,

    /// Car loading
    [EnumStringValue('CD')]
    CarLoading,

    /// Cleaning
    [EnumStringValue('CG')]
    Cleaning,

    /// Cigarette stamping
    [EnumStringValue('CS')]
    CigaretteStamping,

    /// Count and recount
    [EnumStringValue('CT')]
    CountAndRecount,

    /// Layout/design
    [EnumStringValue('DAB')]
    LayoutDesign,

    /// Assortment allowance
    [EnumStringValue('DAC')]
    AssortmentAllowance,

    /// Driver assigned unloading
    [EnumStringValue('DAD')]
    DriverAssignedUnloading,

    /// Debtor bound
    [EnumStringValue('DAF')]
    DebtorBound,

    /// Dealer allowance
    [EnumStringValue('DAG')]
    DealerAllowance,

    /// Allowance transferable to the consumer
    [EnumStringValue('DAH')]
    AllowanceTransferableToTheConsumer,

    /// Growth of business
    [EnumStringValue('DAI')]
    GrowthOfBusiness,

    /// Introduction allowance
    [EnumStringValue('DAJ')]
    IntroductionAllowance,

    /// Multi-buy promotion
    [EnumStringValue('DAK')]
    MultiBuyPromotion,

    /// Partnership
    [EnumStringValue('DAL')]
    Partnership,

    /// Return handling
    [EnumStringValue('DAM')]
    ReturnHandling,

    /// Minimum order not fulfilled charge
    [EnumStringValue('DAN')]
    MinimumOrderNotFulfilledCharge,

    /// Point of sales threshold allowance
    [EnumStringValue('DAO')]
    PointOfSalesThresholdAllowance,

    /// Wholesaling discount
    [EnumStringValue('DAP')]
    WholesalingDiscount,

    /// Documentary credits transfer commission
    [EnumStringValue('DAQ')]
    DocumentaryCreditsTransferCommission,

    /// Delivery
    [EnumStringValue('DL')]
    Delivery,

    /// Engraving
    [EnumStringValue('EG')]
    Engraving,

    /// Expediting
    [EnumStringValue('EP')]
    Expediting,

    /// Exchange rate guarantee
    [EnumStringValue('ER')]
    ExchangeRateGuarantee,

    /// Fabrication
    [EnumStringValue('FAA')]
    Fabrication,

    /// Freight equalization
    [EnumStringValue('FAB')]
    FreightEqualization,

    /// Freight extraordinary handling
    [EnumStringValue('FAC')]
    FreightExtraordinaryHandling,

    /// Freight service
    [EnumStringValue('FC')]
    FreightService,

    /// Filling/handling
    [EnumStringValue('FH')]
    FillingHandling,

    /// Financing
    [EnumStringValue('FI')]
    Financing,

    /// Grinding
    [EnumStringValue('GAA')]
    Grinding,

    /// Hose
    [EnumStringValue('HAA')]
    Hose,

    /// Handling
    [EnumStringValue('HD')]
    Handling,

    /// Hoisting and hauling
    [EnumStringValue('HH')]
    HoistingAndHauling,

    /// Installation
    [EnumStringValue('IAA')]
    Installation,

    /// Installation and warranty
    [EnumStringValue('IAB')]
    InstallationAndWarranty,

    /// Inside delivery
    [EnumStringValue('ID')]
    InsideDelivery,

    /// Inspection
    [EnumStringValue('IF')]
    Inspection,

    /// Installation and training
    [EnumStringValue('IR')]
    InstallationAndTraining,

    /// Invoicing
    [EnumStringValue('IS')]
    Invoicing,

    /// Koshering
    [EnumStringValue('KO')]
    Koshering,

    /// Carrier count
    [EnumStringValue('L1')]
    CarrierCount,

    /// Labelling
    [EnumStringValue('LA')]
    Labelling,

    /// Labour
    [EnumStringValue('LAA')]
    Labour,

    /// Repair and return
    [EnumStringValue('LAB')]
    RepairAndReturn,

    /// Legalisation
    [EnumStringValue('LF')]
    Legalisation,

    /// Mounting
    [EnumStringValue('MAE')]
    Mounting,

    /// Mail invoice
    [EnumStringValue('MI')]
    MailInvoice,

    /// Mail invoice to each location
    [EnumStringValue('ML')]
    MailInvoiceToEachLocation,

    /// Non-returnable containers
    [EnumStringValue('NAA')]
    NonReturnableContainers,

    /// Outside cable connectors
    [EnumStringValue('OA')]
    OutsideCableConnectors,

    /// Invoice with shipment
    [EnumStringValue('PA')]
    InvoiceWithShipment,

    /// Phosphatizing (steel treatment)
    [EnumStringValue('PAA')]
    PhosphatizingSteelTreatment,

    /// Packing
    [EnumStringValue('PC')]
    Packing,

    /// Palletizing
    [EnumStringValue('PL')]
    Palletizing,

    /// Price variation
    [EnumStringValue('PRV')]
    PriceVariation,

    /// Repacking
    [EnumStringValue('RAB')]
    Repacking,

    /// Repair
    [EnumStringValue('RAC')]
    Repair,

    /// Returnable container
    [EnumStringValue('RAD')]
    ReturnableContainer,

    /// Restocking
    [EnumStringValue('RAF')]
    Restocking,

    /// Re-delivery
    [EnumStringValue('RE')]
    ReDelivery,

    /// Refurbishing
    [EnumStringValue('RF')]
    Refurbishing,

    /// Rail wagon hire
    [EnumStringValue('RH')]
    RailWagonHire,

    /// Loading
    [EnumStringValue('RV')]
    Loading,

    /// Salvaging
    [EnumStringValue('SA')]
    Salvaging,

    /// Shipping and handling
    [EnumStringValue('SAA')]
    ShippingAndHandling,

    /// Special packaging
    [EnumStringValue('SAD')]
    SpecialPackaging,

    /// Stamping
    [EnumStringValue('SAE')]
    Stamping,

    /// Consignee unload
    [EnumStringValue('SAI')]
    ConsigneeUnload,

    /// Shrink-wrap
    [EnumStringValue('SG')]
    ShrinkWrap,

    /// Special handling
    [EnumStringValue('SH')]
    SpecialHandling,

    /// Special finish
    [EnumStringValue('SM')]
    SpecialFinish,

    /// Set-up
    [EnumStringValue('SU')]
    SetUp,

    /// Tank renting
    [EnumStringValue('TAB')]
    TankRenting,

    /// Testing
    [EnumStringValue('TAC')]
    Testing,

    /// Transportation - third party billing
    [EnumStringValue('TT')]
    TransportationThirdPartyBilling,

    /// Transportation by vendor
    [EnumStringValue('TV')]
    TransportationByVendor,

    /// Drop yard
    [EnumStringValue('V1')]
    DropYard,

    /// Drop dock
    [EnumStringValue('V2')]
    DropDock,

    /// Warehousing
    [EnumStringValue('WH')]
    Warehousing,

    /// Combine all same day shipment
    [EnumStringValue('XAA')]
    CombineAllSameDayShipment,

    /// Split pick-up
    [EnumStringValue('YY')]
    SplitPickUp,

    /// Mutually defined
    [EnumStringValue('ZZZ')]
    MutuallyDefined,


    /// <summary>
    /// Unknown value
    /// </summary>
    Unknown
    {.DefinitionEnd}
);

implementation

procedure Map (EnumValue: TZUGFeRDChargeReasonCodes; StringValue: string);
begin
  TEnumExtensions<TZUGFeRDChargeReasonCodes>.RegisterMapping(EnumValue, StringValue)
end;

procedure InitMapping;
begin
  {.MapStart}
  // Mapping generated by PSDelphiDefinitionMapper
  Map(Advertising,                                       'AA');
  Map(Telecommunication,                                 'AAA');
  Map(TechnicalModification,                             'AAC');
  Map(JobOrderProduction,                                'AAD');
  Map(Outlays,                                           'AAE');
  Map(OffPremises,                                       'AAF');
  Map(AdditionalProcessing,                              'AAH');
  Map(Attesting,                                         'AAI');
  Map(Acceptance,                                        'AAS');
  Map(RushDelivery,                                      'AAT');
  Map(SpecialConstruction,                               'AAV');
  Map(AirportFacilities,                                 'AAY');
  Map(Concession,                                        'AAZ');
  Map(CompulsoryStorage,                                 'ABA');
  Map(FuelRemoval,                                       'ABB');
  Map(IntoPlane,                                         'ABC');
  Map(Overtime,                                          'ABD');
  Map(Tooling,                                           'ABF');
  Map(Miscellaneous,                                     'ABK');
  Map(AdditionalPackaging,                               'ABL');
  Map(Dunnage,                                           'ABN');
  Map(Containerisation,                                  'ABR');
  Map(CartonPacking,                                     'ABS');
  Map(HessianWrapped,                                    'ABT');
  Map(PolyethyleneWrapPacking,                           'ABU');
  Map(MiscellaneousTreatment,                            'ACF');
  Map(EnamellingTreatment,                               'ACG');
  Map(HeatTreatment,                                     'ACH');
  Map(PlatingTreatment,                                  'ACI');
  Map(Painting,                                          'ACJ');
  Map(Polishing,                                         'ACK');
  Map(Priming,                                           'ACL');
  Map(PreservationTreatment,                             'ACM');
  Map(Fitting,                                           'ACS');
  Map(Consolidation,                                     'ADC');
  Map(BillOfLading,                                      'ADE');
  Map(Airbag,                                            'ADJ');
  Map(Transfer,                                          'ADK');
  Map(Slipsheet,                                         'ADL');
  Map(Binding,                                           'ADM');
  Map(RepairOrReplacementOfBrokenReturnablePackage,      'ADN');
  Map(EfficientLogistics,                                'ADO');
  Map(Merchandising,                                     'ADP');
  Map(ProductMix,                                        'ADQ');
  Map(OtherServices,                                     'ADR');
  Map(PickUp,                                            'ADT');
  Map(ChronicIllness,                                    'ADW');
  Map(NewProductIntroduction,                            'ADY');
  Map(DirectDelivery,                                    'ADZ');
  Map(Diversion,                                         'AEA');
  Map(Disconnect,                                        'AEB');
  Map(Distribution,                                      'AEC');
  Map(HandlingOfHazardousCargo,                          'AED');
  Map(RentsAndLeases,                                    'AEF');
  Map(LocationDifferential,                              'AEH');
  Map(AircraftRefueling,                                 'AEI');
  Map(FuelShippedIntoStorage,                            'AEJ');
  Map(CashOnDelivery,                                    'AEK');
  Map(SmallOrderProcessingService,                       'AEL');
  Map(ClericalOrAdministrativeServices,                  'AEM');
  Map(Guarantee,                                         'AEN');
  Map(CollectionAndRecycling,                            'AEO');
  Map(CopyrightFeeCollection,                            'AEP');
  Map(VeterinaryInspectionService,                       'AES');
  Map(PensionerService,                                  'AET');
  Map(MedicineFreePassHolder,                            'AEU');
  Map(EnvironmentalProtectionService,                    'AEV');
  Map(EnvironmentalCleanUpService,                       'AEW');
  Map(NationalChequeProcessingServiceOutsideAccountArea, 'AEX');
  Map(NationalPaymentServiceOutsideAccountArea,          'AEY');
  Map(NationalPaymentServiceWithinAccountArea,           'AEZ');
  Map(Adjustments,                                       'AJ');
  Map(Authentication,                                    'AU');
  Map(Cataloguing,                                       'CA');
  Map(Cartage,                                           'CAB');
  Map(Certification,                                     'CAD');
  Map(CertificateOfConformance,                          'CAE');
  Map(CertificateOfOrigin,                               'CAF');
  Map(Cutting,                                           'CAI');
  Map(ConsularService,                                   'CAJ');
  Map(CustomerCollection,                                'CAK');
  Map(PayrollPaymentService,                             'CAL');
  Map(CashTransportation,                                'CAM');
  Map(HomeBankingService,                                'CAN');
  Map(BilateralAgreementService,                         'CAO');
  Map(InsuranceBrokerageService,                         'CAP');
  Map(ChequeGeneration,                                  'CAQ');
  Map(PreferentialMerchandisingLocation,                 'CAR');
  Map(Crane,                                             'CAS');
  Map(SpecialColourService,                              'CAT');
  Map(Sorting,                                           'CAU');
  Map(BatteryCollectionAndRecycling,                     'CAV');
  Map(ProductTakeBackFee,                                'CAW');
  Map(QualityControlReleased,                            'CAX');
  Map(QualityControlHeld,                                'CAY');
  Map(QualityControlEmbargo,                             'CAZ');
  Map(CarLoading,                                        'CD');
  Map(Cleaning,                                          'CG');
  Map(CigaretteStamping,                                 'CS');
  Map(CountAndRecount,                                   'CT');
  Map(LayoutDesign,                                      'DAB');
  Map(AssortmentAllowance,                               'DAC');
  Map(DriverAssignedUnloading,                           'DAD');
  Map(DebtorBound,                                       'DAF');
  Map(DealerAllowance,                                   'DAG');
  Map(AllowanceTransferableToTheConsumer,                'DAH');
  Map(GrowthOfBusiness,                                  'DAI');
  Map(IntroductionAllowance,                             'DAJ');
  Map(MultiBuyPromotion,                                 'DAK');
  Map(Partnership,                                       'DAL');
  Map(ReturnHandling,                                    'DAM');
  Map(MinimumOrderNotFulfilledCharge,                    'DAN');
  Map(PointOfSalesThresholdAllowance,                    'DAO');
  Map(WholesalingDiscount,                               'DAP');
  Map(DocumentaryCreditsTransferCommission,              'DAQ');
  Map(Delivery,                                          'DL');
  Map(Engraving,                                         'EG');
  Map(Expediting,                                        'EP');
  Map(ExchangeRateGuarantee,                             'ER');
  Map(Fabrication,                                       'FAA');
  Map(FreightEqualization,                               'FAB');
  Map(FreightExtraordinaryHandling,                      'FAC');
  Map(FreightService,                                    'FC');
  Map(FillingHandling,                                   'FH');
  Map(Financing,                                         'FI');
  Map(Grinding,                                          'GAA');
  Map(Hose,                                              'HAA');
  Map(Handling,                                          'HD');
  Map(HoistingAndHauling,                                'HH');
  Map(Installation,                                      'IAA');
  Map(InstallationAndWarranty,                           'IAB');
  Map(InsideDelivery,                                    'ID');
  Map(Inspection,                                        'IF');
  Map(InstallationAndTraining,                           'IR');
  Map(Invoicing,                                         'IS');
  Map(Koshering,                                         'KO');
  Map(CarrierCount,                                      'L1');
  Map(Labelling,                                         'LA');
  Map(Labour,                                            'LAA');
  Map(RepairAndReturn,                                   'LAB');
  Map(Legalisation,                                      'LF');
  Map(Mounting,                                          'MAE');
  Map(MailInvoice,                                       'MI');
  Map(MailInvoiceToEachLocation,                         'ML');
  Map(NonReturnableContainers,                           'NAA');
  Map(OutsideCableConnectors,                            'OA');
  Map(InvoiceWithShipment,                               'PA');
  Map(PhosphatizingSteelTreatment,                       'PAA');
  Map(Packing,                                           'PC');
  Map(Palletizing,                                       'PL');
  Map(PriceVariation,                                    'PRV');
  Map(Repacking,                                         'RAB');
  Map(Repair,                                            'RAC');
  Map(ReturnableContainer,                               'RAD');
  Map(Restocking,                                        'RAF');
  Map(ReDelivery,                                        'RE');
  Map(Refurbishing,                                      'RF');
  Map(RailWagonHire,                                     'RH');
  Map(Loading,                                           'RV');
  Map(Salvaging,                                         'SA');
  Map(ShippingAndHandling,                               'SAA');
  Map(SpecialPackaging,                                  'SAD');
  Map(Stamping,                                          'SAE');
  Map(ConsigneeUnload,                                   'SAI');
  Map(ShrinkWrap,                                        'SG');
  Map(SpecialHandling,                                   'SH');
  Map(SpecialFinish,                                     'SM');
  Map(SetUp,                                             'SU');
  Map(TankRenting,                                       'TAB');
  Map(Testing,                                           'TAC');
  Map(TransportationThirdPartyBilling,                   'TT');
  Map(TransportationByVendor,                            'TV');
  Map(DropYard,                                          'V1');
  Map(DropDock,                                          'V2');
  Map(Warehousing,                                       'WH');
  Map(CombineAllSameDayShipment,                         'XAA');
  Map(SplitPickUp,                                       'YY');
  Map(MutuallyDefined,                                   'ZZZ');
  Map(Unknown,                                           'Unknown');
{.MapEnd}
end;

Initialization
  InitMapping;
end.
