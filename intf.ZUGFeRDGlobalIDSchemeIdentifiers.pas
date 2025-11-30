unit intf.ZUGFeRDGlobalIDSchemeIdentifiers;

interface

uses
  System.SysUtils,
  intf.ZUGFeRDHelper;

// see http://iso6523.info/icd_list.pdf
//

type
  TZUGFeRDGlobalIDSchemeIdentifiers = (
    {.DefinitionStart}
    // automatically converted by PSC#ToDelphiDefinition

    /// <summary>
    /// System Information et Repertoire des Entreprise et des Etablissements: SIRENE
    /// </summary>
    [EnumStringValue('0002')]
    Sirene,

    /// <summary>
    /// Codification Numerique des Etablissments Financiers En Belgique
    /// </summary>
    [EnumStringValue('0003')]
    CodificationNumeriqueDesEtablissementsFinanciersEnBelgique,

    /// <summary>
    /// NBS/OSI NETWORK
    /// </summary>
    [EnumStringValue('0004')]
    NbsOsiNetwork,

    /// <summary>
    /// USA FED GOV OSI NETWORK
    /// </summary>
    [EnumStringValue('0005')]
    UsaFedGovOsiNetwork,

    /// <summary>
    /// USA DOD OSI NETWORK
    /// </summary>
    [EnumStringValue('0006')]
    UsaDodOsiNetwork,

    /// <summary>
    /// Organisationsnummer
    /// </summary>
    [EnumStringValue('0007')]
    Organisationsnummer,

    /// <summary>
    /// LE NUMERO NATIONAL
    /// </summary>
    [EnumStringValue('0008')]
    LeNumeroNational,

    /// <summary>
    /// SIRET-CODE
    /// </summary>
    [EnumStringValue('0009')]
    SiretCode,

    /// <summary>
    /// Organizational Identifiers for Structured Names under ISO 9541 Part 2
    /// </summary>
    [EnumStringValue('0010')]
    Iso9541StructuredNames,

    /// <summary>
    /// International Code Designator for the Identification of OSI-based, Amateur Radio Organizations, Network Objects and Application Services.
    /// </summary>
    [EnumStringValue('0011')]
    InternationalCodeDesignator,

    /// <summary>
    /// European Computer Manufacturers Association: ECMA
    /// </summary>
    [EnumStringValue('0012')]
    Ecma,

    /// <summary>
    /// VSA FTP CODE (FTP = File Transfer Protocol)
    /// </summary>
    [EnumStringValue('0013')]
    VsaFtpCode,

    /// <summary>
    /// NIST/OSI Implememts' Workshop
    /// </summary>
    [EnumStringValue('0014')]
    NistOsiWorkshop,

    /// <summary>
    /// Electronic Data Interchange: EDI
    /// </summary>
    [EnumStringValue('0015')]
    Edi,

    /// <summary>
    /// EWOS Object Identifiers
    /// </summary>
    [EnumStringValue('0016')]
    EwosObjectIdentifiers,

    /// <summary>
    /// COMMON LANGUAGE
    /// </summary>
    [EnumStringValue('0017')]
    CommonLanguage,

    /// <summary>
    /// SNA/OSI Network
    /// </summary>
    [EnumStringValue('0018')]
    SnaOsiNetwork,

    /// <summary>
    /// Air Transport Industry Services Communications Network
    /// </summary>
    [EnumStringValue('0019')]
    AirTransportIndustryNetwork,

    /// <summary>
    /// European Laboratory for Particle Physics: CERN
    /// </summary>
    [EnumStringValue('0020')]
    Cern,

    /// <summary>
    /// SOCIETY FOR WORLDWIDE INTERBANK FINANCIAL, TELECOMMUNICATION S.W.I.F.T.
    /// </summary>
    [EnumStringValue('0021')]
    Swift,

    /// <summary>
    /// OSF Distributed Computing Object Identification
    /// </summary>
    [EnumStringValue('0022')]
    OsfDistributedComputing,

    /// <summary>
    /// Nordic University and Research Network: NORDUnet
    /// </summary>
    [EnumStringValue('0023')]
    Nordunet,

    /// <summary>
    /// Digital Equipment Corporation: DEC
    /// </summary>
    [EnumStringValue('0024')]
    Dec,

    /// <summary>
    /// OSI ASIA-OCEANIA WORKSHOP
    /// </summary>
    [EnumStringValue('0025')]
    OsiAsiaOceaniaWorkshop,

    /// <summary>
    /// NATO ISO 6523 ICDE coding scheme
    /// </summary>
    [EnumStringValue('0026')]
    NatoIso6523Icde,

    /// <summary>
    /// Aeronautical Telecommunications Network (ATN)
    /// </summary>
    [EnumStringValue('0027')]
    AtmNetwork,

    /// <summary>
    /// International Standard ISO 6523
    /// </summary>
    [EnumStringValue('0028')]
    Iso6523,

    /// <summary>
    /// The All-Union Classifier of Enterprises and Organisations
    /// </summary>
    [EnumStringValue('0029')]
    AllUnionClassifier,

    /// <summary>
    /// AT&amp;T/OSI Network
    /// </summary>
    [EnumStringValue('0030')]
    AttOsiNetwork,

    /// <summary>
    /// EDI Partner Identification Code
    /// </summary>
    [EnumStringValue('0031')]
    EdiPartnerCode,

    /// <summary>
    /// Telecom Australia
    /// </summary>
    [EnumStringValue('0032')]
    TelecomAustralia,

    /// <summary>
    /// S G W OSI Internetwork
    /// </summary>
    [EnumStringValue('0033')]
    SgwOsiInternetwork,

    /// <summary>
    /// Reuter Open Address Standard
    /// </summary>
    [EnumStringValue('0034')]
    ReuterOpenAddressStandard,

    /// <summary>
    /// ISO 6523 - ICD
    /// </summary>
    [EnumStringValue('0035')]
    Iso6523Icd,

    /// <summary>
    /// TeleTrust Object Identifiers
    /// </summary>
    [EnumStringValue('0036')]
    TeletrustOids,

    /// <summary>
    /// LY-tunnus
    /// </summary>
    [EnumStringValue('0037')]
    LyTunnus,

    /// <summary>
    /// The Australian GOSIP Network
    /// </summary>
    [EnumStringValue('0038')]
    AustralianGosipNetwork,

    /// <summary>
    /// The OZ DOD OSI Network
    /// </summary>
    [EnumStringValue('0039')]
    OzDodOsiNetwork,

    /// <summary>
    /// Unilever Group Companies
    /// </summary>
    [EnumStringValue('0040')]
    UnileverGroupCompanies,

    /// <summary>
    /// Citicorp Global Information Network
    /// </summary>
    [EnumStringValue('0041')]
    CiticorpGlobalNetwork,

    /// <summary>
    /// DBP Telekom Object Identifiers
    /// </summary>
    [EnumStringValue('0042')]
    DbpTelekomOids,

    /// <summary>
    /// HydroNETT
    /// </summary>
    [EnumStringValue('0043')]
    HydroNett,

    /// <summary>
    /// Thai Industrial Standards Institute (TISI)
    /// </summary>
    [EnumStringValue('0044')]
    Tisi,

    /// <summary>
    /// ICI Company Identification System
    /// </summary>
    [EnumStringValue('0045')]
    IciCompanySystem,

    /// <summary>
    /// FUNLOC
    /// </summary>
    [EnumStringValue('0046')]
    Funloc,

    /// <summary>
    /// BULL ODI/DSA/UNIX Network
    /// </summary>
    [EnumStringValue('0047')]
    BullOdiDsaUnixNetwork,

    /// <summary>
    /// OSINZ
    /// </summary>
    [EnumStringValue('0048')]
    Osinz,

    /// <summary>
    /// Auckland Area Health
    /// </summary>
    [EnumStringValue('0049')]
    AucklandAreaHealth,

    /// <summary>
    /// Firmenich
    /// </summary>
    [EnumStringValue('0050')]
    Firmenich,

    /// <summary>
    /// AGFA-DIS
    /// </summary>
    [EnumStringValue('0051')]
    AgfaDis,

    /// <summary>
    /// Society of Motion Picture and Television Engineers (SMPTE)
    /// </summary>
    [EnumStringValue('0052')]
    Smpte,

    /// <summary>
    /// Migros_Network M_NETOPZ
    /// </summary>
    [EnumStringValue('0053')]
    MigrosNetwork,

    /// <summary>
    /// ISO6523 - ICDPCR
    /// </summary>
    [EnumStringValue('0054')]
    IcdPcr,

    /// <summary>
    /// Energy Net
    /// </summary>
    [EnumStringValue('0055')]
    EnergyNet,

    /// <summary>
    /// Nokia Object Identifiers (NOI)
    /// </summary>
    [EnumStringValue('0056')]
    NokiaOids,

    /// <summary>
    /// Saint Gobain
    /// </summary>
    [EnumStringValue('0057')]
    SaintGobain,

    /// <summary>
    /// Siemens Corporate Network
    /// </summary>
    [EnumStringValue('0058')]
    SiemensCorporateNetwork,

    /// <summary>
    /// DANZNET
    /// </summary>
    [EnumStringValue('0059')]
    Danznet,

    /// <summary>
    /// Data Universal Numbering System (D-U-N-S Number)
    /// </summary>
    [EnumStringValue('0060')]
    DUNS,

    /// <summary>
    /// SOFFEX OSI
    /// </summary>
    [EnumStringValue('0061')]
    SoftexOsi,

    /// <summary>
    /// KPN OVN
    /// </summary>
    [EnumStringValue('0062')]
    KpnOvn,

    /// <summary>
    /// ascomOSINet
    /// </summary>
    [EnumStringValue('0063')]
    AscomOsiNet,

    /// <summary>
    /// UTC: Uniforme Transport Code
    /// </summary>
    [EnumStringValue('0064')]
    UtcUniformTransportCode,

    /// <summary>
    /// SOLVAY OSI CODING
    /// </summary>
    [EnumStringValue('0065')]
    SolvayOsiCoding,

    /// <summary>
    /// Roche Corporate Network
    /// </summary>
    [EnumStringValue('0066')]
    RocheCorporateNetwork,

    /// <summary>
    /// ZellwegerOSINet
    /// </summary>
    [EnumStringValue('0067')]
    ZellwegerOsiNet,

    /// <summary>
    /// Intel Corporation OSI
    /// </summary>
    [EnumStringValue('0068')]
    IntelOsi,

    /// <summary>
    /// SITA Object Identifier Tree
    /// </summary>
    [EnumStringValue('0069')]
    SitaOidTree,

    /// <summary>
    /// DaimlerChrysler Corporate Network
    /// </summary>
    [EnumStringValue('0070')]
    DaimlerChryslerNetwork,

    /// <summary>
    /// LEGO /OSI NETWORK
    /// </summary>
    [EnumStringValue('0071')]
    LegoOsiNetwork,

    /// <summary>
    /// NAVISTAR/OSI Network
    /// </summary>
    [EnumStringValue('0072')]
    NavistarOsiNetwork,

    /// <summary>
    /// ICD Formatted ATM address
    /// </summary>
    [EnumStringValue('0073')]
    IcdFormattedAtmAddress,

    /// <summary>
    /// ARINC
    /// </summary>
    [EnumStringValue('0074')]
    Arinc,

    /// <summary>
    /// Alcanet/Alcatel-Alsthom Corporate Network
    /// </summary>
    [EnumStringValue('0075')]
    AlcanetAlcatelNetwork,

    /// <summary>
    /// Sistema Italiano di Identificazione di ogetti gestito da UNINFO
    /// </summary>
    [EnumStringValue('0076')]
    UninfoItalianIdentification,

    /// <summary>
    /// Sistema Italiano di Indirizzamento di Reti OSI Gestito da UNINFO
    /// </summary>
    [EnumStringValue('0077')]
    UninfoItalianAddressing,

    /// <summary>
    /// Mitel terminal or switching equipment
    /// </summary>
    [EnumStringValue('0078')]
    MitelTerminalSwitching,

    /// <summary>
    /// ATM Forum
    /// </summary>
    [EnumStringValue('0079')]
    AtmForum,

    /// <summary>
    /// UK National Health Service Scheme, (EDIRA compliant)
    /// </summary>
    [EnumStringValue('0080')]
    UkNhsScheme,

    /// <summary>
    /// International NSAP
    /// </summary>
    [EnumStringValue('0081')]
    InternationalNsap,

    /// <summary>
    /// Norwegian Telecommunications Authority's, NTA'S, EDI, identifier scheme (EDIRA compliant)
    /// </summary>
    [EnumStringValue('0082')]
    NorwegianTelecomAuthority,

    /// <summary>
    /// Advanced Telecommunications Modules Limited, Corporate Network
    /// </summary>
    [EnumStringValue('0083')]
    AdvancedTelecomModules,

    /// <summary>
    /// Athens Chamber of Commerce &amp; Industry Scheme (EDIRA compliant)
    /// </summary>
    [EnumStringValue('0084')]
    AthensChamberScheme,

    /// <summary>
    /// Swiss Chambers of Commerce Scheme (EDIRA) compliant
    /// </summary>
    [EnumStringValue('0085')]
    SwissChambersScheme,

    /// <summary>
    /// United States Council for International Business (USCIB) Scheme, (EDIRA compliant)
    /// </summary>
    [EnumStringValue('0086')]
    UscibScheme,

    /// <summary>
    /// National Federation of Chambers of Commerce &amp; Industry of Belgium, Scheme (EDIRA compliant)
    /// </summary>
    [EnumStringValue('0087')]
    BelgianChambersFederation,

    /// <summary>
    /// EAN Location Code
    /// </summary>
    [EnumStringValue('0088')]
    GLN,

    /// <summary>
    /// The Association of British Chambers of Commerce Ltd. Scheme, (EDIRA compliant)
    /// </summary>
    [EnumStringValue('0089')]
    BritishChambersScheme,

    /// <summary>
    /// Internet IP addressing - ISO 6523 ICD encoding
    /// </summary>
    [EnumStringValue('0090')]
    Iso6523IpEncoding,

    /// <summary>
    /// Cisco Sysytems / OSI Network
    /// </summary>
    [EnumStringValue('0091')]
    CiscoOsiNetwork,

    /// <summary>
    /// Revenue Canada Business Number Registration (EDIRA compliant)
    /// </summary>
    [EnumStringValue('0093')]
    RevenueCanadaBnRegistration,

    /// <summary>
    /// DEUTSCHER INDUSTRIE- UND HANDELSTAG (DIHT) Scheme (EDIRA compliant)
    /// </summary>
    [EnumStringValue('0094')]
    DihtScheme,

    /// <summary>
    /// Hewlett - Packard Company Internal AM Network
    /// </summary>
    [EnumStringValue('0095')]
    HewlettPackardNetwork,

    /// <summary>
    /// DANISH CHAMBER OF COMMERCE Scheme (EDIRA compliant)
    /// </summary>
    [EnumStringValue('0096')]
    DanishChamberScheme,

    /// <summary>
    /// FTI - Ediforum Italia, (EDIRA compliant)
    /// </summary>
    [EnumStringValue('0097')]
    EdiforumItalia,

    /// <summary>
    /// CHAMBER OF COMMERCE TEL AVIV-JAFFA Scheme (EDIRA compliant)
    /// </summary>
    [EnumStringValue('0098')]
    TelAvivJaffaChamberScheme,

    /// <summary>
    /// Siemens Supervisory Systems Network
    /// </summary>
    [EnumStringValue('0099')]
    SiemensSupervisoryNetwork,

    /// <summary>
    /// PNG_ICD Scheme
    /// </summary>
    [EnumStringValue('0100')]
    PngIcdScheme,

    /// <summary>
    /// South African Code Allocation
    /// </summary>
    [EnumStringValue('0101')]
    SouthAfricanCodeAllocation,

    /// <summary>
    /// HEAG
    /// </summary>
    [EnumStringValue('0102')]
    Heag,

    /// <summary>
    /// BT - ICD Coding System
    /// </summary>
    [EnumStringValue('0104')]
    BtIcdCodingSystem,

    /// <summary>
    /// Portuguese Chamber of Commerce and Industry Scheme (EDIRA compliant)
    /// </summary>
    [EnumStringValue('0105')]
    PortugueseChamberScheme,

    /// <summary>
    /// Vereniging van Kamers van Koophandel en Fabrieken in Nederland (Association of Chambers of Commerce and Industry in the Netherlands), Scheme (EDIRA compliant)
    /// </summary>
    [EnumStringValue('0106')]
    DutchChambersScheme,

    /// <summary>
    /// Association of Swedish Chambers of Commerce and Industry Scheme (EDIRA compliant)
    /// </summary>
    [EnumStringValue('0107')]
    SwedishChambersScheme,

    /// <summary>
    /// Australian Chambers of Commerce and Industry Scheme (EDIRA compliant)
    /// </summary>
    [EnumStringValue('0108')]
    AustralianChambersScheme,

    /// <summary>
    /// BellSouth ICD AESA (ATM End System Address)
    /// </summary>
    [EnumStringValue('0109')]
    BellSouthAesa,

    /// <summary>
    /// Bell Atlantic
    /// </summary>
    [EnumStringValue('0110')]
    BellAtlantic,

    /// <summary>
    /// Object Identifiers
    /// </summary>
    [EnumStringValue('0111')]
    ObjectIdentifiers,

    /// <summary>
    /// ISO register for Standards producing Organizations
    /// </summary>
    [EnumStringValue('0112')]
    IsoStandardsRegistry,

    /// <summary>
    /// OriginNet
    /// </summary>
    [EnumStringValue('0113')]
    OriginNet,

    /// <summary>
    /// Check Point Software Technologies
    /// </summary>
    [EnumStringValue('0114')]
    CheckPointSoftware,

    /// <summary>
    /// Pacific Bell Data Communications Network
    /// </summary>
    [EnumStringValue('0115')]
    PacificBellDataNetwork,

    /// <summary>
    /// PSS Object Identifiers
    /// </summary>
    [EnumStringValue('0116')]
    PssObjectIdentifiers,

    /// <summary>
    /// STENTOR-ICD CODING SYSTEM
    /// </summary>
    [EnumStringValue('0117')]
    StentorIcdCodingSystem,

    /// <summary>
    /// ATM-Network ZN'96
    /// </summary>
    [EnumStringValue('0118')]
    AtmNetworkZn96,

    /// <summary>
    /// MCI / OSI Network
    /// </summary>
    [EnumStringValue('0119')]
    MciOsiNetwork,

    /// <summary>
    /// Advantis
    /// </summary>
    [EnumStringValue('0120')]
    Advantis,

    /// <summary>
    /// Affable Software Data Interchange Codes
    /// </summary>
    [EnumStringValue('0121')]
    AffableSoftwareDic,

    /// <summary>
    /// BB-DATA GmbH
    /// </summary>
    [EnumStringValue('0122')]
    BbDataGmbh,

    /// <summary>
    /// BASF Company ATM-Network
    /// </summary>
    [EnumStringValue('0123')]
    BasfAtmNetwork,

    /// <summary>
    /// IOTA Identifiers for Organizations for Telecommunications Addressing using the ICD system format defined in ISO/IEC 8348
    /// </summary>
    [EnumStringValue('0124')]
    IotaTelecomIdentifiers,

    /// <summary>
    /// Henkel Corporate Network (H-Net)
    /// </summary>
    [EnumStringValue('0125')]
    HenkelCorporateNetwork,

    /// <summary>
    /// GTE/OSI Network
    /// </summary>
    [EnumStringValue('0126')]
    GteOsiNetwork,

    /// <summary>
    /// Dresdner Bank Corporate Network
    /// </summary>
    [EnumStringValue('0127')]
    DresdnerBankCorporateNetwork,

    /// <summary>
    /// BCNR (Swiss Clearing Bank Number)
    /// </summary>
    [EnumStringValue('0128')]
    SwissClearingBankNumber,

    /// <summary>
    /// BPI (Swiss Business Partner Identification) code
    /// </summary>
    [EnumStringValue('0129')]
    SwissBusinessPartnerId,

    /// <summary>
    /// Directorates of the European Commission
    /// </summary>
    [EnumStringValue('0130')]
    EuropeanCommissionDirectorates,

    /// <summary>
    /// Code for the Identification of National Organizations
    /// </summary>
    [EnumStringValue('0131')]
    NationalOrganizationsCode,

    /// <summary>
    /// Certicom Object Identifiers
    /// </summary>
    [EnumStringValue('0132')]
    CerticomOids,

    /// <summary>
    /// TC68 OID
    /// </summary>
    [EnumStringValue('0133')]
    Tc68Oid,

    /// <summary>
    /// Infonet Services Corporation
    /// </summary>
    [EnumStringValue('0134')]
    InfonetServices,

    /// <summary>
    /// SIA Object Identifiers
    /// </summary>
    [EnumStringValue('0135')]
    SiaObjectIdentifiers,

    /// <summary>
    /// Cable &amp; Wireless Global ATM End-System Address Plan
    /// </summary>
    [EnumStringValue('0136')]
    CableWirelessAesa,

    /// <summary>
    /// Global AESA scheme
    /// </summary>
    [EnumStringValue('0137')]
    GlobalAesaScheme,

    /// <summary>
    /// France Telecom ATM End System Address Plan
    /// </summary>
    [EnumStringValue('0138')]
    FranceTelecomAtmPlan,

    /// <summary>
    /// Savvis Communications AESA:.
    /// </summary>
    [EnumStringValue('0139')]
    SavvisCommunicationsAesa,

    /// <summary>
    /// Toshiba Organizations, Partners, And Suppliers' (TOPAS) Code
    /// </summary>
    [EnumStringValue('0140')]
    Topas,

    /// <summary>
    /// NATO Commercial and Government Entity system
    /// </summary>
    [EnumStringValue('0141')]
    NatoCommercialEntity,

    /// <summary>
    /// SECETI Object Identifiers
    /// </summary>
    [EnumStringValue('0142')]
    SecetiOids,

    /// <summary>
    /// EINESTEINet AG
    /// </summary>
    [EnumStringValue('0143')]
    EinsteinetAg,

    /// <summary>
    /// DoDAAC (Department of Defense Activity Address Code)
    /// </summary>
    [EnumStringValue('0144')]
    DodAAC,

    /// <summary>
    /// DGCP (Direction Générale de la Comptabilité Publique) administrative accounting identification scheme
    /// </summary>
    [EnumStringValue('0145')]
    DgcpAdminAccounting,

    /// <summary>
    /// DGI (Direction Générale des Impots) code
    /// </summary>
    [EnumStringValue('0146')]
    DgiCode,

    /// <summary>
    /// Standard Company Code
    /// </summary>
    [EnumStringValue('0147')]
    StandardCompanyCode,

    /// <summary>
    /// ITU (International Telecommunications Union) Data Network Identification Codes (DNIC)
    /// </summary>
    [EnumStringValue('0148')]
    ItuDnic,

    /// <summary>
    /// Global Business Identifier
    /// </summary>
    [EnumStringValue('0149')]
    GlobalBusinessIdentifier,

    /// <summary>
    /// Madge Networks Ltd- ICD ATM Addressing Scheme
    /// </summary>
    [EnumStringValue('0150')]
    MadgeNetworksAtmScheme,

    /// <summary>
    /// Australian Business Number (ABN) Scheme
    /// </summary>
    [EnumStringValue('0151')]
    AbnScheme,

    /// <summary>
    /// Edira Scheme Identifier Code
    /// </summary>
    [EnumStringValue('0152')]
    EdiraScheme,

    /// <summary>
    /// Concert Global Network Services ICD AESA
    /// </summary>
    [EnumStringValue('0153')]
    ConcertGlobalNetworkServicesAesa,

    /// <summary>
    /// Identification number of economic subjects: (ICO)
    /// </summary>
    [EnumStringValue('0154')]
    IcoEconomicSubjects,

    /// <summary>
    /// Global Crossing AESA (ATM End System Address)
    /// </summary>
    [EnumStringValue('0155')]
    GlobalCrossingAesa,

    /// <summary>
    /// AUNA
    /// </summary>
    [EnumStringValue('0156')]
    Auna,

    /// <summary>
    /// ATM interconnection with the Dutch KPN Telecom
    /// </summary>
    [EnumStringValue('0157')]
    AtmInterconnectDutchKpn,

    /// <summary>
    /// Identification number of economic subject (ICO) Act on State Statistics of 29 November 2'001, § 27
    /// </summary>
    [EnumStringValue('0158')]
    IcoStatisticalAct,

    /// <summary>
    /// ACTALIS Object Identifiers
    /// </summary>
    [EnumStringValue('0159')]
    ActalisOids,

    /// <summary>
    /// GTIN - Global Trade Item Number
    /// </summary>
    [EnumStringValue('0160')]
    EAN,

    /// <summary>
    /// ECCMA Open Technical Directory
    /// </summary>
    [EnumStringValue('0161')]
    EccmaOpenDirectory,

    /// <summary>
    /// CEN/ISSS Object Identifier Scheme
    /// </summary>
    [EnumStringValue('0162')]
    CenisObjectIdentifierScheme,

    /// <summary>
    /// US-EPA Facility Identifier
    /// </summary>
    [EnumStringValue('0163')]
    UsEpaFacilityIdentifier,

    /// <summary>
    /// TELUS Corporation
    /// </summary>
    [EnumStringValue('0164')]
    TelusCorporation,

    /// <summary>
    /// FIEIE Object identifiers
    /// </summary>
    [EnumStringValue('0165')]
    FieieOids,

    /// <summary>
    /// Swissguide Identifier Scheme
    /// </summary>
    [EnumStringValue('0166')]
    SwissguideScheme,

    /// <summary>
    /// Priority Telecom ATM End System Address Plan
    /// </summary>
    [EnumStringValue('0167')]
    PriorityTelecomAtmPlan,

    /// <summary>
    /// Vodafone Ireland OSI Addressing
    /// </summary>
    [EnumStringValue('0168')]
    VodafoneIrelandOsi,

    /// <summary>
    /// Swiss Federal Business Identification Number. Central Business names Index (zefix) Identification Number
    /// </summary>
    [EnumStringValue('0169')]
    SwissFederalBusinessId,

    /// <summary>
    /// Teikoku Company Code
    /// </summary>
    [EnumStringValue('0170')]
    TeikokuCompanyCode,

    /// <summary>
    /// Luxembourg CP &amp; CPS (Certification Policy and Certification Practice Statement) Index
    /// </summary>
    [EnumStringValue('0171')]
    LuxembourgCpsIndex,

    /// <summary>
    /// Project Group “Lists of Properties” (PROLIST®)
    /// </summary>
    [EnumStringValue('0172')]
    Prolist,

    /// <summary>
    /// eCI@ss
    /// </summary>
    [EnumStringValue('0173')]
    EciAss,

    /// <summary>
    /// StepNexus
    /// </summary>
    [EnumStringValue('0174')]
    StepNexus,

    /// <summary>
    /// Siemens AG
    /// </summary>
    [EnumStringValue('0175')]
    SiemensAg,

    /// <summary>
    /// Paradine GmbH
    /// </summary>
    [EnumStringValue('0176')]
    ParadineGmbh,

    /// <summary>
    /// Odette International Limited
    /// </summary>
    [EnumStringValue('0177')]
    Odette,

    /// <summary>
    /// Route1 MobiNET
    /// </summary>
    [EnumStringValue('0178')]
    Route1MobiNet,

    /// <summary>
    /// Penango Object Identifiers
    /// </summary>
    [EnumStringValue('0179')]
    PenangoOids,

    /// <summary>
    /// Lithuanian military PKI
    /// </summary>
    [EnumStringValue('0180')]
    LithuanianMilitaryPki,

    /// <summary>
    /// Numéro d'identification suisse des enterprises (IDE), Swiss Unique Business Identification Number (UIDB)
    /// </summary>
    [EnumStringValue('0183')]
    SwissUidb,

    /// <summary>
    /// DIGSTORG
    /// </summary>
    [EnumStringValue('0184')]
    Digstorg,

    /// <summary>
    /// Perceval Object Code
    /// </summary>
    [EnumStringValue('0185')]
    PercevalObjectCode,

    /// <summary>
    /// TrustPoint Object Identifiers
    /// </summary>
    [EnumStringValue('0186')]
    TrustPointOids,

    /// <summary>
    /// Amazon Unique Identification Scheme
    /// </summary>
    [EnumStringValue('0187')]
    AmazonUniqueIdentificationScheme,

    /// <summary>
    /// Corporate Number of The Social Security and Tax Number System
    /// </summary>
    [EnumStringValue('0188')]
    SocialSecurityTaxNumberSystem,

    /// <summary>
    /// European Business Identifier (EBID)
    /// </summary>
    [EnumStringValue('0189')]
    Ebid,

    /// <summary>
    /// Organisatie Indentificatie Nummer (OIN)
    /// </summary>
    [EnumStringValue('0190')]
    Oin,

    /// <summary>
    /// Company Code (Estonia)
    /// </summary>
    [EnumStringValue('0191')]
    EstoniaCompanyCode,

    /// <summary>
    /// Organisasjonsnummer
    /// </summary>
    [EnumStringValue('0192')]
    Organisasjonsnummer,

    /// <summary>
    /// UBL.BE Party Identifier
    /// </summary>
    [EnumStringValue('0193')]
    UblBePartyIdentifier,

    /// <summary>
    /// KOIOS Open Technical Dictionary
    /// </summary>
    [EnumStringValue('0194')]
    KoiosOpenTechnicalDictionary,

    /// <summary>
    /// Singapore Nationwide E-invoice Framework
    /// </summary>
    [EnumStringValue('0195')]
    SingaporeENInvoiceFramework,

    /// <summary>
    /// Icelandic identifier - Íslensk kennitala
    /// </summary>
    [EnumStringValue('0196')]
    IslenskKennitala,

    /// <summary>
    /// APPLiA Pl Standard
    /// </summary>
    [EnumStringValue('0197')]
    AppliaPlStandard,

    /// <summary>
    /// ERSTORG
    /// </summary>
    [EnumStringValue('0198')]
    Erstorg,

    /// <summary>
    /// Legal Entity Identifier (LEI)
    /// </summary>
    [EnumStringValue('0199')]
    Lei,

    /// <summary>
    /// Legal entity code (Lithuania)
    /// </summary>
    [EnumStringValue('0200')]
    LithuaniaLegalEntityCode,

    /// <summary>
    /// Codice Univoco Unità Organizzativa iPA
    /// </summary>
    [EnumStringValue('0201')]
    IpAUnitCode,

    /// <summary>
    /// Indirizzo di Posta Elettronica Certificata
    /// </summary>
    [EnumStringValue('0202')]
    CertifiedEmailAddress,

    /// <summary>
    /// eDelivery Network Participant identifier
    /// </summary>
    [EnumStringValue('0203')]
    EDeliveryNetworkParticipantIdentifier,

    /// <summary>
    /// Leitweg-ID
    /// </summary>
    [EnumStringValue('0204')]
    LeitwegID,

    /// <summary>
    /// CODDEST
    /// </summary>
    [EnumStringValue('0205')]
    Coddest,

    /// <summary>
    /// Registre du Commerce et de l’Industrie : RCI
    /// </summary>
    [EnumStringValue('0206')]
    Rci,

    /// <summary>
    /// PiLog Ontology Codification Identifier (POCI)
    /// </summary>
    [EnumStringValue('0207')]
    PilogOntologyCodificationIdentifier,

    /// <summary>
    /// Numero d'entreprise / ondernemingsnummer / Unternehmensnummer
    /// </summary>
    [EnumStringValue('0208')]
    CompanyNumber,

    /// <summary>
    /// GS1 identification keys
    /// </summary>
    [EnumStringValue('0209')]
    Gs1IdentificationKeys,

    /// <summary>
    /// CODICE FISCALE
    /// </summary>
    [EnumStringValue('0210')]
    CodiceFiscale,

    /// <summary>
    /// PARTITA IVA
    /// </summary>
    [EnumStringValue('0211')]
    PartitaIva,

    /// <summary>
    /// Finnish Organization Identifier
    /// </summary>
    [EnumStringValue('0212')]
    FinnishOrganizationIdentifier,

    /// <summary>
    /// Finnish Organization Value Add Tax Identifier
    /// </summary>
    [EnumStringValue('0213')]
    FinnishOrganizationVatIdentifier,

    /// <summary>
    /// Tradeplace TradePI Standard
    /// </summary>
    [EnumStringValue('0214')]
    TradeplaceTradePiStandard,

    /// <summary>
    /// Net service ID
    /// </summary>
    [EnumStringValue('0215')]
    NetServiceId,

    /// <summary>
    /// OVTcode
    /// </summary>
    [EnumStringValue('0216')]
    OvtCode,

    /// <summary>
    /// The Netherlands Chamber of Commerce and Industry establishment number
    /// </summary>
    [EnumStringValue('0217')]
    NetherlandsChamberEstablishmentNumber,

    /// <summary>
    /// Unified registration number (Latvia)
    /// </summary>
    [EnumStringValue('0218')]
    LatviaUnifiedRegistrationNumber,

    /// <summary>
    /// Taxpayer registration code (Latvia)
    /// </summary>
    [EnumStringValue('0219')]
    LatviaTaxpayerRegistrationCode,

    /// <summary>
    /// The Register of Natural Persons (Latvia)
    /// </summary>
    [EnumStringValue('0220')]
    LatviaNaturalPersonsRegister,

    /// <summary>
    /// The registered number of the qualified invoice issuer
    /// </summary>
    [EnumStringValue('0221')]
    QualifiedInvoiceIssuerNumber,

    /// <summary>
    /// Metadata Registry Support
    /// </summary>
    [EnumStringValue('0222')]
    MetadataRegistrySupport,

    /// <summary>
    /// EU based company
    /// </summary>
    [EnumStringValue('0223')]
    EuBasedCompany,

    /// <summary>
    /// FTCTC CODE ROUTAGE
    /// </summary>
    [EnumStringValue('0224')]
    FcttcCodeRoutage,

    /// <summary>
    /// FRCTC ELECTRONIC ADDRESS
    /// </summary>
    [EnumStringValue('0225')]
    FrctcElectronicAddress,

    /// <summary>
    /// FRCTC Particulier
    /// </summary>
    [EnumStringValue('0226')]
    FrctcParticulier,

    /// <summary>
    /// NON - EU based company
    /// </summary>
    [EnumStringValue('0227')]
    NonEuBasedCompany,

    /// <summary>
    /// Répertoire des Entreprises et des Etablissements (RIDET)
    /// </summary>
    [EnumStringValue('0228')]
    Ridet,

    /// <summary>
    /// T.A.H.I.T.I (traitement automatique hiérarchisé des institutions de Tahiti et des îles)
    /// </summary>
    [EnumStringValue('0229')]
    TahitiAutomaticHierarchicalInstitutionDirectory,

    /// <summary>
    /// National e-Invoicing Framework
    /// </summary>
    [EnumStringValue('0230')]
    NationalEInvoicingFramework,

    /// <summary>
    /// Single taxable company (France)
    /// </summary>
    [EnumStringValue('0231')]
    SingleTaxableCompanyFrance,

    /// <summary>
    /// NOBB product number
    /// </summary>
    [EnumStringValue('0232')]
    NobbProductNumber,

    /// <summary>
    /// Description not known
    /// </summary>
    [EnumStringValue('0233')]
    DescriptionNotKnown,

    /// <summary>
    /// Toimitusosoite ID
    /// </summary>
    [EnumStringValue('0234')]
    ToimitusosoiteId,

    /// <summary>
    /// UAE Tax Identification Number (TIN)
    /// </summary>
    [EnumStringValue('0235')]
    UaeTaxIdentificationNumber,

    /// <summary>
    /// Description not known
    /// </summary>
    [EnumStringValue('0236')]
    DescriptionNotKnown2,

    /// <summary>
    /// CPR (Danish person civil registration number)
    /// </summary>
    [EnumStringValue('0237')]
    DanishCivilRegistrationNumber,

    /// <summary>
    /// Plateforme.s agréée.s à la facturation électronique (PPF/PDP)
    /// </summary>
    [EnumStringValue('0238')]
    PpfPdp,

    /// <summary>
    /// EAEU
    /// </summary>
    [EnumStringValue('0239')]
    Eaeu,

    /// <summary>
    /// Register of legal persons (in French : Répertoire des personnes morales)
    /// </summary>
    [EnumStringValue('0240')]
    RegisterOfLegalPersons,


    /// <summary>
    /// Unknown value
    /// </summary>
    Unknown
    {.DefinitionEnd}
  );

implementation

procedure Map (EnumValue: TZUGFeRDGlobalIDSchemeIdentifiers; StringValue: string); inline;
begin
  TEnumExtensions<TZUGFeRDGlobalIDSchemeIdentifiers>.RegisterMapping(EnumValue, StringValue)
end;

procedure InitMapping;
begin
  {.MapStart}
  // Mapping generated by PSDelphiDefinitionMapper
  Map(Sirene,                                                     '0002');
  Map(CodificationNumeriqueDesEtablissementsFinanciersEnBelgique, '0003');
  Map(NbsOsiNetwork,                                              '0004');
  Map(UsaFedGovOsiNetwork,                                        '0005');
  Map(UsaDodOsiNetwork,                                           '0006');
  Map(Organisationsnummer,                                        '0007');
  Map(LeNumeroNational,                                           '0008');
  Map(SiretCode,                                                  '0009');
  Map(Iso9541StructuredNames,                                     '0010');
  Map(InternationalCodeDesignator,                                '0011');
  Map(Ecma,                                                       '0012');
  Map(VsaFtpCode,                                                 '0013');
  Map(NistOsiWorkshop,                                            '0014');
  Map(Edi,                                                        '0015');
  Map(EwosObjectIdentifiers,                                      '0016');
  Map(CommonLanguage,                                             '0017');
  Map(SnaOsiNetwork,                                              '0018');
  Map(AirTransportIndustryNetwork,                                '0019');
  Map(Cern,                                                       '0020');
  Map(Swift,                                                      '0021');
  Map(OsfDistributedComputing,                                    '0022');
  Map(Nordunet,                                                   '0023');
  Map(Dec,                                                        '0024');
  Map(OsiAsiaOceaniaWorkshop,                                     '0025');
  Map(NatoIso6523Icde,                                            '0026');
  Map(AtmNetwork,                                                 '0027');
  Map(Iso6523,                                                    '0028');
  Map(AllUnionClassifier,                                         '0029');
  Map(AttOsiNetwork,                                              '0030');
  Map(EdiPartnerCode,                                             '0031');
  Map(TelecomAustralia,                                           '0032');
  Map(SgwOsiInternetwork,                                         '0033');
  Map(ReuterOpenAddressStandard,                                  '0034');
  Map(Iso6523Icd,                                                 '0035');
  Map(TeletrustOids,                                              '0036');
  Map(LyTunnus,                                                   '0037');
  Map(AustralianGosipNetwork,                                     '0038');
  Map(OzDodOsiNetwork,                                            '0039');
  Map(UnileverGroupCompanies,                                     '0040');
  Map(CiticorpGlobalNetwork,                                      '0041');
  Map(DbpTelekomOids,                                             '0042');
  Map(HydroNett,                                                  '0043');
  Map(Tisi,                                                       '0044');
  Map(IciCompanySystem,                                           '0045');
  Map(Funloc,                                                     '0046');
  Map(BullOdiDsaUnixNetwork,                                      '0047');
  Map(Osinz,                                                      '0048');
  Map(AucklandAreaHealth,                                         '0049');
  Map(Firmenich,                                                  '0050');
  Map(AgfaDis,                                                    '0051');
  Map(Smpte,                                                      '0052');
  Map(MigrosNetwork,                                              '0053');
  Map(IcdPcr,                                                     '0054');
  Map(EnergyNet,                                                  '0055');
  Map(NokiaOids,                                                  '0056');
  Map(SaintGobain,                                                '0057');
  Map(SiemensCorporateNetwork,                                    '0058');
  Map(Danznet,                                                    '0059');
  Map(DUNS,                                                       '0060');
  Map(SoftexOsi,                                                  '0061');
  Map(KpnOvn,                                                     '0062');
  Map(AscomOsiNet,                                                '0063');
  Map(UtcUniformTransportCode,                                    '0064');
  Map(SolvayOsiCoding,                                            '0065');
  Map(RocheCorporateNetwork,                                      '0066');
  Map(ZellwegerOsiNet,                                            '0067');
  Map(IntelOsi,                                                   '0068');
  Map(SitaOidTree,                                                '0069');
  Map(DaimlerChryslerNetwork,                                     '0070');
  Map(LegoOsiNetwork,                                             '0071');
  Map(NavistarOsiNetwork,                                         '0072');
  Map(IcdFormattedAtmAddress,                                     '0073');
  Map(Arinc,                                                      '0074');
  Map(AlcanetAlcatelNetwork,                                      '0075');
  Map(UninfoItalianIdentification,                                '0076');
  Map(UninfoItalianAddressing,                                    '0077');
  Map(MitelTerminalSwitching,                                     '0078');
  Map(AtmForum,                                                   '0079');
  Map(UkNhsScheme,                                                '0080');
  Map(InternationalNsap,                                          '0081');
  Map(NorwegianTelecomAuthority,                                  '0082');
  Map(AdvancedTelecomModules,                                     '0083');
  Map(AthensChamberScheme,                                        '0084');
  Map(SwissChambersScheme,                                        '0085');
  Map(UscibScheme,                                                '0086');
  Map(BelgianChambersFederation,                                  '0087');
  Map(GLN,                                                        '0088');
  Map(BritishChambersScheme,                                      '0089');
  Map(Iso6523IpEncoding,                                          '0090');
  Map(CiscoOsiNetwork,                                            '0091');
  Map(RevenueCanadaBnRegistration,                                '0093');
  Map(DihtScheme,                                                 '0094');
  Map(HewlettPackardNetwork,                                      '0095');
  Map(DanishChamberScheme,                                        '0096');
  Map(EdiforumItalia,                                             '0097');
  Map(TelAvivJaffaChamberScheme,                                  '0098');
  Map(SiemensSupervisoryNetwork,                                  '0099');
  Map(PngIcdScheme,                                               '0100');
  Map(SouthAfricanCodeAllocation,                                 '0101');
  Map(Heag,                                                       '0102');
  Map(BtIcdCodingSystem,                                          '0104');
  Map(PortugueseChamberScheme,                                    '0105');
  Map(DutchChambersScheme,                                        '0106');
  Map(SwedishChambersScheme,                                      '0107');
  Map(AustralianChambersScheme,                                   '0108');
  Map(BellSouthAesa,                                              '0109');
  Map(BellAtlantic,                                               '0110');
  Map(ObjectIdentifiers,                                          '0111');
  Map(IsoStandardsRegistry,                                       '0112');
  Map(OriginNet,                                                  '0113');
  Map(CheckPointSoftware,                                         '0114');
  Map(PacificBellDataNetwork,                                     '0115');
  Map(PssObjectIdentifiers,                                       '0116');
  Map(StentorIcdCodingSystem,                                     '0117');
  Map(AtmNetworkZn96,                                             '0118');
  Map(MciOsiNetwork,                                              '0119');
  Map(Advantis,                                                   '0120');
  Map(AffableSoftwareDic,                                         '0121');
  Map(BbDataGmbh,                                                 '0122');
  Map(BasfAtmNetwork,                                             '0123');
  Map(IotaTelecomIdentifiers,                                     '0124');
  Map(HenkelCorporateNetwork,                                     '0125');
  Map(GteOsiNetwork,                                              '0126');
  Map(DresdnerBankCorporateNetwork,                               '0127');
  Map(SwissClearingBankNumber,                                    '0128');
  Map(SwissBusinessPartnerId,                                     '0129');
  Map(EuropeanCommissionDirectorates,                             '0130');
  Map(NationalOrganizationsCode,                                  '0131');
  Map(CerticomOids,                                               '0132');
  Map(Tc68Oid,                                                    '0133');
  Map(InfonetServices,                                            '0134');
  Map(SiaObjectIdentifiers,                                       '0135');
  Map(CableWirelessAesa,                                          '0136');
  Map(GlobalAesaScheme,                                           '0137');
  Map(FranceTelecomAtmPlan,                                       '0138');
  Map(SavvisCommunicationsAesa,                                   '0139');
  Map(Topas,                                                      '0140');
  Map(NatoCommercialEntity,                                       '0141');
  Map(SecetiOids,                                                 '0142');
  Map(EinsteinetAg,                                               '0143');
  Map(DodAAC,                                                     '0144');
  Map(DgcpAdminAccounting,                                        '0145');
  Map(DgiCode,                                                    '0146');
  Map(StandardCompanyCode,                                        '0147');
  Map(ItuDnic,                                                    '0148');
  Map(GlobalBusinessIdentifier,                                   '0149');
  Map(MadgeNetworksAtmScheme,                                     '0150');
  Map(AbnScheme,                                                  '0151');
  Map(EdiraScheme,                                                '0152');
  Map(ConcertGlobalNetworkServicesAesa,                           '0153');
  Map(IcoEconomicSubjects,                                        '0154');
  Map(GlobalCrossingAesa,                                         '0155');
  Map(Auna,                                                       '0156');
  Map(AtmInterconnectDutchKpn,                                    '0157');
  Map(IcoStatisticalAct,                                          '0158');
  Map(ActalisOids,                                                '0159');
  Map(EAN,                                                        '0160');
  Map(EccmaOpenDirectory,                                         '0161');
  Map(CenisObjectIdentifierScheme,                                '0162');
  Map(UsEpaFacilityIdentifier,                                    '0163');
  Map(TelusCorporation,                                           '0164');
  Map(FieieOids,                                                  '0165');
  Map(SwissguideScheme,                                           '0166');
  Map(PriorityTelecomAtmPlan,                                     '0167');
  Map(VodafoneIrelandOsi,                                         '0168');
  Map(SwissFederalBusinessId,                                     '0169');
  Map(TeikokuCompanyCode,                                         '0170');
  Map(LuxembourgCpsIndex,                                         '0171');
  Map(Prolist,                                                    '0172');
  Map(EciAss,                                                     '0173');
  Map(StepNexus,                                                  '0174');
  Map(SiemensAg,                                                  '0175');
  Map(ParadineGmbh,                                               '0176');
  Map(Odette,                                                     '0177');
  Map(Route1MobiNet,                                              '0178');
  Map(PenangoOids,                                                '0179');
  Map(LithuanianMilitaryPki,                                      '0180');
  Map(SwissUidb,                                                  '0183');
  Map(Digstorg,                                                   '0184');
  Map(PercevalObjectCode,                                         '0185');
  Map(TrustPointOids,                                             '0186');
  Map(AmazonUniqueIdentificationScheme,                           '0187');
  Map(SocialSecurityTaxNumberSystem,                              '0188');
  Map(Ebid,                                                       '0189');
  Map(Oin,                                                        '0190');
  Map(EstoniaCompanyCode,                                         '0191');
  Map(Organisasjonsnummer,                                        '0192');
  Map(UblBePartyIdentifier,                                       '0193');
  Map(KoiosOpenTechnicalDictionary,                               '0194');
  Map(SingaporeENInvoiceFramework,                                '0195');
  Map(IslenskKennitala,                                           '0196');
  Map(AppliaPlStandard,                                           '0197');
  Map(Erstorg,                                                    '0198');
  Map(Lei,                                                        '0199');
  Map(LithuaniaLegalEntityCode,                                   '0200');
  Map(IpAUnitCode,                                                '0201');
  Map(CertifiedEmailAddress,                                      '0202');
  Map(EDeliveryNetworkParticipantIdentifier,                      '0203');
  Map(LeitwegID,                                                  '0204');
  Map(Coddest,                                                    '0205');
  Map(Rci,                                                        '0206');
  Map(PilogOntologyCodificationIdentifier,                        '0207');
  Map(CompanyNumber,                                              '0208');
  Map(Gs1IdentificationKeys,                                      '0209');
  Map(CodiceFiscale,                                              '0210');
  Map(PartitaIva,                                                 '0211');
  Map(FinnishOrganizationIdentifier,                              '0212');
  Map(FinnishOrganizationVatIdentifier,                           '0213');
  Map(TradeplaceTradePiStandard,                                  '0214');
  Map(NetServiceId,                                               '0215');
  Map(OvtCode,                                                    '0216');
  Map(NetherlandsChamberEstablishmentNumber,                      '0217');
  Map(LatviaUnifiedRegistrationNumber,                            '0218');
  Map(LatviaTaxpayerRegistrationCode,                             '0219');
  Map(LatviaNaturalPersonsRegister,                               '0220');
  Map(QualifiedInvoiceIssuerNumber,                               '0221');
  Map(MetadataRegistrySupport,                                    '0222');
  Map(EuBasedCompany,                                             '0223');
  Map(FcttcCodeRoutage,                                           '0224');
  Map(FrctcElectronicAddress,                                     '0225');
  Map(FrctcParticulier,                                           '0226');
  Map(NonEuBasedCompany,                                          '0227');
  Map(Ridet,                                                      '0228');
  Map(TahitiAutomaticHierarchicalInstitutionDirectory,            '0229');
  Map(NationalEInvoicingFramework,                                '0230');
  Map(SingleTaxableCompanyFrance,                                 '0231');
  Map(NobbProductNumber,                                          '0232');
  Map(DescriptionNotKnown,                                        '0233');
  Map(ToimitusosoiteId,                                           '0234');
  Map(UaeTaxIdentificationNumber,                                 '0235');
  Map(DescriptionNotKnown2,                                       '0236');
  Map(DanishCivilRegistrationNumber,                              '0237');
  Map(PpfPdp,                                                     '0238');
  Map(Eaeu,                                                       '0239');
  Map(RegisterOfLegalPersons,                                     '0240');
  Map(Unknown,                                                    'Unknown');
{.MapEnd}
end;

Initialization
  InitMapping;
end.

