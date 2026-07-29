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

unit intf.ZUGFeRDDesignatedProductClassificationClassCodes;

interface

uses
  System.SysUtils,
  intf.ZUGFeRDHelper;

type
	/// <summary>
	/// A code for the classification of an item according to type or kind or nature.
	///
	/// Classification codes are used for the aggregation of similar products, which might be useful for various
	/// purposes,
	/// for instance like public procurement, in accordance with the Common Vocabulary for Public Procurement
	/// [CPV]), e-Commerce(UNSPSC) etc.
	///
	/// Source: UNTDID 7143
	/// Business rule: BR-65
	/// </summary>
  TZUGFeRDDesignatedProductClassificationClassCodes = (
    {.DefinitionStart}
    // Quelle: UNTDID 7143, EN16931 code lists values v17b (Factur-X 1.09)

    /// <summary>
    /// Product version number
    /// </summary>
    AA,

    /// <summary>
    /// Assembly
    /// </summary>
    AB,

    /// <summary>
    /// HIBC (Health Industry Bar Code)
    /// </summary>
    AC,

    /// <summary>
    /// Cold roll number
    /// </summary>
    AD,

    /// <summary>
    /// Hot roll number
    /// </summary>
    AE,

    /// <summary>
    /// Slab number
    /// </summary>
    AF,

    /// <summary>
    /// Software revision number
    /// </summary>
    AG,

    /// <summary>
    /// UPC (Universal Product Code) Consumer package code (1-5-5)
    /// </summary>
    AH,

    /// <summary>
    /// UPC (Universal Product Code) Consumer package code (1-5-5-
    /// </summary>
    AI,

    /// <summary>
    /// Sample number
    /// </summary>
    AJ,

    /// <summary>
    /// Pack number
    /// </summary>
    AK,

    /// <summary>
    /// UPC (Universal Product Code) Shipping container code (1-2-
    /// </summary>
    AL,

    /// <summary>
    /// UPC (Universal Product Code)/EAN (European article number)
    /// </summary>
    AM,

    /// <summary>
    /// UPC (Universal Product Code) suffix
    /// </summary>
    AN,

    /// <summary>
    /// State label code
    /// </summary>
    AO,

    /// <summary>
    /// Heat number
    /// </summary>
    AP,

    /// <summary>
    /// Coupon number
    /// </summary>
    AQ,

    /// <summary>
    /// Resource number
    /// </summary>
    AR,

    /// <summary>
    /// Work task number
    /// </summary>
    AS_,

    /// <summary>
    /// Price look up number
    /// </summary>
    AT,

    /// <summary>
    /// NSN (North Atlantic Treaty Organization Stock Number)
    /// </summary>
    AU,

    /// <summary>
    /// Refined product code
    /// </summary>
    AV,

    /// <summary>
    /// Exhibit
    /// </summary>
    AW,

    /// <summary>
    /// End item
    /// </summary>
    AX,

    /// <summary>
    /// Federal supply classification
    /// </summary>
    AY,

    /// <summary>
    /// Engineering data list
    /// </summary>
    AZ,

    /// <summary>
    /// Milestone event number
    /// </summary>
    BA,

    /// <summary>
    /// Lot number
    /// </summary>
    BB,

    /// <summary>
    /// National drug code 4-4-2 format
    /// </summary>
    BC,

    /// <summary>
    /// National drug code 5-3-2 format
    /// </summary>
    BD,

    /// <summary>
    /// National drug code 5-4-1 format
    /// </summary>
    BE,

    /// <summary>
    /// National drug code 5-4-2 format
    /// </summary>
    BF,

    /// <summary>
    /// National drug code
    /// </summary>
    BG,

    /// <summary>
    /// Part number
    /// </summary>
    BH,

    /// <summary>
    /// Local Stock Number (LSN)
    /// </summary>
    BI,

    /// <summary>
    /// Next higher assembly number
    /// </summary>
    BJ,

    /// <summary>
    /// Data category
    /// </summary>
    BK,

    /// <summary>
    /// Control number
    /// </summary>
    BL,

    /// <summary>
    /// Special material identification code
    /// </summary>
    BM,

    /// <summary>
    /// Locally assigned control number
    /// </summary>
    BN,

    /// <summary>
    /// Buyer's colour
    /// </summary>
    BO,

    /// <summary>
    /// Buyer's part number
    /// </summary>
    BP,

    /// <summary>
    /// Variable measure product code
    /// </summary>
    BQ,

    /// <summary>
    /// Financial phase
    /// </summary>
    BR,

    /// <summary>
    /// Contract breakdown
    /// </summary>
    BS,

    /// <summary>
    /// Technical phase
    /// </summary>
    BT,

    /// <summary>
    /// Dye lot number
    /// </summary>
    BU,

    /// <summary>
    /// Daily statement of activities
    /// </summary>
    BV,

    /// <summary>
    /// Periodical statement of activities within a bilaterally
    /// </summary>
    BW,

    /// <summary>
    /// Calendar week statement of activities
    /// </summary>
    BX,

    /// <summary>
    /// Calendar month statement of activities
    /// </summary>
    BY,

    /// <summary>
    /// Original equipment number
    /// </summary>
    BZ,

    /// <summary>
    /// Industry commodity code
    /// </summary>
    CC,

    /// <summary>
    /// Commodity grouping
    /// </summary>
    CG,

    /// <summary>
    /// Colour number
    /// </summary>
    CL,

    /// <summary>
    /// Contract number
    /// </summary>
    CR,

    /// <summary>
    /// Customs article number
    /// </summary>
    CV,

    /// <summary>
    /// Drawing revision number
    /// </summary>
    DR,

    /// <summary>
    /// Drawing
    /// </summary>
    DW,

    /// <summary>
    /// Engineering change level
    /// </summary>
    EC,

    /// <summary>
    /// Material code
    /// </summary>
    EF,

    /// <summary>
    /// EMDN (European Medical Device Nomenclature)
    /// </summary>
    EMD,

    /// <summary>
    /// International Article Numbering Association (EAN)
    /// </summary>
    EN,

    /// <summary>
    /// Fish species
    /// </summary>
    FS,

    /// <summary>
    /// Buyer's internal product group code
    /// </summary>
    GB,

    /// <summary>
    /// Global model number
    /// </summary>
    GMN,

    /// <summary>
    /// National product group code
    /// </summary>
    GN,

    /// <summary>
    /// General specification number
    /// </summary>
    GS,

    /// <summary>
    /// Harmonised system
    /// </summary>
    HS,

    /// <summary>
    /// ISBN (International Standard Book Number)
    /// </summary>
    IB,

    /// <summary>
    /// Buyer's item number
    /// </summary>
    IN_,

    /// <summary>
    /// ISSN (International Standard Serial Number)
    /// </summary>
    IS_,

    /// <summary>
    /// Buyer's style number
    /// </summary>
    IT,

    /// <summary>
    /// Buyer's size code
    /// </summary>
    IZ,

    /// <summary>
    /// Machine number
    /// </summary>
    MA,

    /// <summary>
    /// Manufacturer's (producer's) article number
    /// </summary>
    MF,

    /// <summary>
    /// Model number
    /// </summary>
    MN,

    /// <summary>
    /// Product/service identification number
    /// </summary>
    MP,

    /// <summary>
    /// Batch number
    /// </summary>
    NB,

    /// <summary>
    /// Customer order number
    /// </summary>
    ON,

    /// <summary>
    /// Part number description
    /// </summary>
    PD,

    /// <summary>
    /// Purchaser's order line number
    /// </summary>
    PL,

    /// <summary>
    /// Purchase order number
    /// </summary>
    PO,

    /// <summary>
    /// Phytosanitary Passport identifier
    /// </summary>
    PPI,

    /// <summary>
    /// Promotional variant number
    /// </summary>
    PV,

    /// <summary>
    /// Buyer's qualifier for size
    /// </summary>
    QS,

    /// <summary>
    /// Returnable container number
    /// </summary>
    RC,

    /// <summary>
    /// Release number
    /// </summary>
    RN,

    /// <summary>
    /// Run number
    /// </summary>
    RU,

    /// <summary>
    /// Record keeping of model year
    /// </summary>
    RY,

    /// <summary>
    /// Supplier's article number
    /// </summary>
    SA,

    /// <summary>
    /// Standard group of products (mixed assortment)
    /// </summary>
    SG,

    /// <summary>
    /// SKU (Stock keeping unit)
    /// </summary>
    SK,

    /// <summary>
    /// Serial number
    /// </summary>
    SN,

    /// <summary>
    /// RSK number
    /// </summary>
    SRS,

    /// <summary>
    /// IFLS (Institut Francais du Libre Service) 5 digit product
    /// </summary>
    SRT,

    /// <summary>
    /// IFLS (Institut Francais du Libre Service) 9 digit product
    /// </summary>
    SRU,

    /// <summary>
    /// GS1 Global Trade Item Number
    /// </summary>
    SRV,

    /// <summary>
    /// EDIS (Energy Data Identification System)
    /// </summary>
    SRW,

    /// <summary>
    /// Slaughter number
    /// </summary>
    SRX,

    /// <summary>
    /// Official animal number
    /// </summary>
    SRY,

    /// <summary>
    /// Harmonized tariff schedule
    /// </summary>
    SRZ,

    /// <summary>
    /// Supplier's supplier article number
    /// </summary>
    SS,

    /// <summary>
    /// 46 Level DOT Code
    /// </summary>
    SSA,

    /// <summary>
    /// Airline Tariff 6D
    /// </summary>
    SSB,

    /// <summary>
    /// Title 49 Code of Federal Regulations
    /// </summary>
    SSC,

    /// <summary>
    /// International Civil Aviation Administration code
    /// </summary>
    SSD,

    /// <summary>
    /// Hazardous Materials ID DOT
    /// </summary>
    SSE,

    /// <summary>
    /// Endorsement
    /// </summary>
    SSF,

    /// <summary>
    /// Air Force Regulation 71-4
    /// </summary>
    SSG,

    /// <summary>
    /// Breed
    /// </summary>
    SSH,

    /// <summary>
    /// Chemical Abstract Service (CAS) registry number
    /// </summary>
    SSI,

    /// <summary>
    /// Engine model designation
    /// </summary>
    SSJ,

    /// <summary>
    /// Institutional Meat Purchase Specifications (IMPS) Number
    /// </summary>
    SSK,

    /// <summary>
    /// Price Look-Up code (PLU)
    /// </summary>
    SSL,

    /// <summary>
    /// International Maritime Organization (IMO) Code
    /// </summary>
    SSM,

    /// <summary>
    /// Bureau of Explosives 600-A (rail)
    /// </summary>
    SSN,

    /// <summary>
    /// United Nations Dangerous Goods List
    /// </summary>
    SSO,

    /// <summary>
    /// International Code of Botanical Nomenclature (ICBN)
    /// </summary>
    SSP,

    /// <summary>
    /// International Code of Zoological Nomenclature (ICZN)
    /// </summary>
    SSQ,

    /// <summary>
    /// International Code of Nomenclature for Cultivated Plants
    /// </summary>
    SSR,

    /// <summary>
    /// Distributor's article identifier
    /// </summary>
    SSS,

    /// <summary>
    /// Norwegian Classification system ENVA
    /// </summary>
    SST,

    /// <summary>
    /// Supplier assigned classification
    /// </summary>
    SSU,

    /// <summary>
    /// Mexican classification system AMECE
    /// </summary>
    SSV,

    /// <summary>
    /// German classification system CCG
    /// </summary>
    SSW,

    /// <summary>
    /// Finnish classification system EANFIN
    /// </summary>
    SSX,

    /// <summary>
    /// Canadian classification system ICC
    /// </summary>
    SSY,

    /// <summary>
    /// French classification system IFLS5
    /// </summary>
    SSZ,

    /// <summary>
    /// Style number
    /// </summary>
    ST,

    /// <summary>
    /// Dutch classification system CBL
    /// </summary>
    STA,

    /// <summary>
    /// Japanese classification system JICFS
    /// </summary>
    STB,

    /// <summary>
    /// European Union dairy subsidy eligibility classification
    /// </summary>
    STC,

    /// <summary>
    /// GS1 Spain classification system
    /// </summary>
    STD,

    /// <summary>
    /// GS1 Poland classification system
    /// </summary>
    STE,

    /// <summary>
    /// Federal Agency on Technical Regulating and Metrology of the
    /// </summary>
    STF,

    /// <summary>
    /// Efficient Consumer Response (ECR) Austria classification
    /// </summary>
    STG,

    /// <summary>
    /// GS1 Italy classification system
    /// </summary>
    STH,

    /// <summary>
    /// CPV (Common Procurement Vocabulary)
    /// </summary>
    STI,

    /// <summary>
    /// IFDA (International Foodservice Distributors Association)
    /// </summary>
    STJ,

    /// <summary>
    /// AHFS (American Hospital Formulary Service) pharmacologic -
    /// </summary>
    STK,

    /// <summary>
    /// ATC (Anatomical Therapeutic Chemical) classification system
    /// </summary>
    STL,

    /// <summary>
    /// CLADIMED (Classification des Dispositifs Médicaux)
    /// </summary>
    STM,

    /// <summary>
    /// CMDR (Canadian Medical Device Regulations) classification
    /// </summary>
    STN,

    /// <summary>
    /// CNDM (Classificazione Nazionale dei Dispositivi Medici)
    /// </summary>
    STO,

    /// <summary>
    /// UK DM&amp;D (Dictionary of Medicines &amp; Devices) standard coding
    /// </summary>
    STP,

    /// <summary>
    /// eCl@ss
    /// </summary>
    STQ,

    /// <summary>
    /// EDMA (European Diagnostic Manufacturers Association)
    /// </summary>
    STR,

    /// <summary>
    /// EGAR (European Generic Article Register)
    /// </summary>
    STS,

    /// <summary>
    /// GMDN (Global Medical Devices Nomenclature)
    /// </summary>
    STT,

    /// <summary>
    /// GPI (Generic Product Identifier)
    /// </summary>
    STU,

    /// <summary>
    /// HCPCS (Healthcare Common Procedure Coding System)
    /// </summary>
    STV,

    /// <summary>
    /// ICPS (International Classification for Patient Safety)
    /// </summary>
    STW,

    /// <summary>
    /// MedDRA (Medical Dictionary for Regulatory Activities)
    /// </summary>
    STX,

    /// <summary>
    /// Medical Columbus
    /// </summary>
    STY,

    /// <summary>
    /// NAPCS (North American Product Classification System)
    /// </summary>
    STZ,

    /// <summary>
    /// NHS (National Health Services) eClass
    /// </summary>
    SUA,

    /// <summary>
    /// US FDA (Food and Drug Administration) Product Code
    /// </summary>
    SUB,

    /// <summary>
    /// SNOMED CT (Systematized Nomenclature of Medicine-Clinical
    /// </summary>
    SUC,

    /// <summary>
    /// UMDNS (Universal Medical Device Nomenclature System)
    /// </summary>
    SUD,

    /// <summary>
    /// GS1 Global Returnable Asset Identifier, non-serialised
    /// </summary>
    SUE,

    /// <summary>
    /// IMEI
    /// </summary>
    SUF,

    /// <summary>
    /// Waste Type (EMSA)
    /// </summary>
    SUG,

    /// <summary>
    /// Ship's store classification type
    /// </summary>
    SUH,

    /// <summary>
    /// Emergency fire code
    /// </summary>
    SUI,

    /// <summary>
    /// Emergency spillage code
    /// </summary>
    SUJ,

    /// <summary>
    /// IMDG packing group
    /// </summary>
    SUK,

    /// <summary>
    /// MARPOL Code IBC
    /// </summary>
    SUL,

    /// <summary>
    /// IMDG subsidiary risk class
    /// </summary>
    SUM,

    /// <summary>
    /// Transport group number
    /// </summary>
    TG,

    /// <summary>
    /// Taxonomic Serial Number
    /// </summary>
    TSN,

    /// <summary>
    /// IMDG main hazard class
    /// </summary>
    TSO,

    /// <summary>
    /// EU Combined Nomenclature
    /// </summary>
    TSP,

    /// <summary>
    /// Therapeutic classification number
    /// </summary>
    TSQ,

    /// <summary>
    /// European Waste Catalogue
    /// </summary>
    TSR,

    /// <summary>
    /// Price grouping code
    /// </summary>
    TSS,

    /// <summary>
    /// UNSPSC
    /// </summary>
    TST,

    /// <summary>
    /// EU RoHS Directive
    /// </summary>
    TSU,

    /// <summary>
    /// Ultimate customer's article number
    /// </summary>
    UA,

    /// <summary>
    /// UPC (Universal product code)
    /// </summary>
    UP,

    /// <summary>
    /// Vendor item number
    /// </summary>
    VN,

    /// <summary>
    /// Vendor's (seller's) part number
    /// </summary>
    VP,

    /// <summary>
    /// Vendor's supplemental item number
    /// </summary>
    VS,

    /// <summary>
    /// Vendor specification number
    /// </summary>
    VX,

    /// <summary>
    /// Mutually defined
    /// </summary>
    ZZZ,


    /// <summary>
    /// Unknown value
    /// </summary>
    Unknown
    {.DefinitionEnd}
  );

implementation

procedure Map (EnumValue: TZUGFeRDDesignatedProductClassificationClassCodes; StringValue: string); inline;
begin
  TEnumExtensions<TZUGFeRDDesignatedProductClassificationClassCodes>.RegisterMapping(EnumValue, StringValue)
end;

procedure InitMapping;
begin
  {.MapStart}
  Map(AA,      'AA');
  Map(AB,      'AB');
  Map(AC,      'AC');
  Map(AD,      'AD');
  Map(AE,      'AE');
  Map(AF,      'AF');
  Map(AG,      'AG');
  Map(AH,      'AH');
  Map(AI,      'AI');
  Map(AJ,      'AJ');
  Map(AK,      'AK');
  Map(AL,      'AL');
  Map(AM,      'AM');
  Map(AN,      'AN');
  Map(AO,      'AO');
  Map(AP,      'AP');
  Map(AQ,      'AQ');
  Map(AR,      'AR');
  Map(AS_,     'AS');
  Map(AT,      'AT');
  Map(AU,      'AU');
  Map(AV,      'AV');
  Map(AW,      'AW');
  Map(AX,      'AX');
  Map(AY,      'AY');
  Map(AZ,      'AZ');
  Map(BA,      'BA');
  Map(BB,      'BB');
  Map(BC,      'BC');
  Map(BD,      'BD');
  Map(BE,      'BE');
  Map(BF,      'BF');
  Map(BG,      'BG');
  Map(BH,      'BH');
  Map(BI,      'BI');
  Map(BJ,      'BJ');
  Map(BK,      'BK');
  Map(BL,      'BL');
  Map(BM,      'BM');
  Map(BN,      'BN');
  Map(BO,      'BO');
  Map(BP,      'BP');
  Map(BQ,      'BQ');
  Map(BR,      'BR');
  Map(BS,      'BS');
  Map(BT,      'BT');
  Map(BU,      'BU');
  Map(BV,      'BV');
  Map(BW,      'BW');
  Map(BX,      'BX');
  Map(BY,      'BY');
  Map(BZ,      'BZ');
  Map(CC,      'CC');
  Map(CG,      'CG');
  Map(CL,      'CL');
  Map(CR,      'CR');
  Map(CV,      'CV');
  Map(DR,      'DR');
  Map(DW,      'DW');
  Map(EC,      'EC');
  Map(EF,      'EF');
  Map(EMD,     'EMD');
  Map(EN,      'EN');
  Map(FS,      'FS');
  Map(GB,      'GB');
  Map(GMN,     'GMN');
  Map(GN,      'GN');
  Map(GS,      'GS');
  Map(HS,      'HS');
  Map(IB,      'IB');
  Map(IN_,     'IN');
  Map(IS_,     'IS');
  Map(IT,      'IT');
  Map(IZ,      'IZ');
  Map(MA,      'MA');
  Map(MF,      'MF');
  Map(MN,      'MN');
  Map(MP,      'MP');
  Map(NB,      'NB');
  Map(ON,      'ON');
  Map(PD,      'PD');
  Map(PL,      'PL');
  Map(PO,      'PO');
  Map(PPI,     'PPI');
  Map(PV,      'PV');
  Map(QS,      'QS');
  Map(RC,      'RC');
  Map(RN,      'RN');
  Map(RU,      'RU');
  Map(RY,      'RY');
  Map(SA,      'SA');
  Map(SG,      'SG');
  Map(SK,      'SK');
  Map(SN,      'SN');
  Map(SRS,     'SRS');
  Map(SRT,     'SRT');
  Map(SRU,     'SRU');
  Map(SRV,     'SRV');
  Map(SRW,     'SRW');
  Map(SRX,     'SRX');
  Map(SRY,     'SRY');
  Map(SRZ,     'SRZ');
  Map(SS,      'SS');
  Map(SSA,     'SSA');
  Map(SSB,     'SSB');
  Map(SSC,     'SSC');
  Map(SSD,     'SSD');
  Map(SSE,     'SSE');
  Map(SSF,     'SSF');
  Map(SSG,     'SSG');
  Map(SSH,     'SSH');
  Map(SSI,     'SSI');
  Map(SSJ,     'SSJ');
  Map(SSK,     'SSK');
  Map(SSL,     'SSL');
  Map(SSM,     'SSM');
  Map(SSN,     'SSN');
  Map(SSO,     'SSO');
  Map(SSP,     'SSP');
  Map(SSQ,     'SSQ');
  Map(SSR,     'SSR');
  Map(SSS,     'SSS');
  Map(SST,     'SST');
  Map(SSU,     'SSU');
  Map(SSV,     'SSV');
  Map(SSW,     'SSW');
  Map(SSX,     'SSX');
  Map(SSY,     'SSY');
  Map(SSZ,     'SSZ');
  Map(ST,      'ST');
  Map(STA,     'STA');
  Map(STB,     'STB');
  Map(STC,     'STC');
  Map(STD,     'STD');
  Map(STE,     'STE');
  Map(STF,     'STF');
  Map(STG,     'STG');
  Map(STH,     'STH');
  Map(STI,     'STI');
  Map(STJ,     'STJ');
  Map(STK,     'STK');
  Map(STL,     'STL');
  Map(STM,     'STM');
  Map(STN,     'STN');
  Map(STO,     'STO');
  Map(STP,     'STP');
  Map(STQ,     'STQ');
  Map(STR,     'STR');
  Map(STS,     'STS');
  Map(STT,     'STT');
  Map(STU,     'STU');
  Map(STV,     'STV');
  Map(STW,     'STW');
  Map(STX,     'STX');
  Map(STY,     'STY');
  Map(STZ,     'STZ');
  Map(SUA,     'SUA');
  Map(SUB,     'SUB');
  Map(SUC,     'SUC');
  Map(SUD,     'SUD');
  Map(SUE,     'SUE');
  Map(SUF,     'SUF');
  Map(SUG,     'SUG');
  Map(SUH,     'SUH');
  Map(SUI,     'SUI');
  Map(SUJ,     'SUJ');
  Map(SUK,     'SUK');
  Map(SUL,     'SUL');
  Map(SUM,     'SUM');
  Map(TG,      'TG');
  Map(TSN,     'TSN');
  Map(TSO,     'TSO');
  Map(TSP,     'TSP');
  Map(TSQ,     'TSQ');
  Map(TSR,     'TSR');
  Map(TSS,     'TSS');
  Map(TST,     'TST');
  Map(TSU,     'TSU');
  Map(UA,      'UA');
  Map(UP,      'UP');
  Map(VN,      'VN');
  Map(VP,      'VP');
  Map(VS,      'VS');
  Map(VX,      'VX');
  Map(ZZZ,     'ZZZ');
  Map(Unknown, 'Unknown');
{.MapEnd}
end;

Initialization
  InitMapping;
end.
