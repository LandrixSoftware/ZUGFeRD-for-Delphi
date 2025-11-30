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

unit intf.ZUGFeRDCurrencyCodes;

interface

uses
  System.SysUtils,
  intf.ZUGFeRDHelper;

type
  TZUGFeRDCurrencyCodes = (
    {.DefinitionStart}
    // automatically converted by PSC#ToDelphiDefinition

    /// <summary>
    /// UAE Dirham
    /// </summary>
    [EnumStringValue('AED')]
    AED,

    /// <summary>
    /// Afghani
    /// </summary>
    [EnumStringValue('AFN')]
    AFN,

    /// <summary>
    /// Lek
    /// </summary>
    [EnumStringValue('ALL')]
    ALL,

    /// <summary>
    /// Armenian Dram
    /// </summary>
    [EnumStringValue('AMD')]
    AMD,

    /// <summary>
    /// Netherlands Antillean Guilder
    /// </summary>
    [EnumStringValue('ANG')]
    ANG,

    /// <summary>
    /// Kwanza
    /// </summary>
    [EnumStringValue('AOA')]
    AOA,

    /// <summary>
    /// Argentine Peso
    /// </summary>
    [EnumStringValue('ARS')]
    ARS,

    /// <summary>
    /// Australian Dollar
    /// </summary>
    [EnumStringValue('AUD')]
    AUD,

    /// <summary>
    /// Aruban Florin
    /// </summary>
    [EnumStringValue('AWG')]
    AWG,

    /// <summary>
    /// Azerbaijan Manat
    /// </summary>
    [EnumStringValue('AZN')]
    AZN,

    /// <summary>
    /// Convertible Mark
    /// </summary>
    [EnumStringValue('BAM')]
    BAM,

    /// <summary>
    /// Barbados Dollar
    /// </summary>
    [EnumStringValue('BBD')]
    BBD,

    /// <summary>
    /// Taka
    /// </summary>
    [EnumStringValue('BDT')]
    BDT,

    /// <summary>
    /// Bulgarian Lev
    /// </summary>
    [EnumStringValue('BGN')]
    BGN,

    /// <summary>
    /// Bahraini Dinar
    /// </summary>
    [EnumStringValue('BHD')]
    BHD,

    /// <summary>
    /// Burundi Franc
    /// </summary>
    [EnumStringValue('BIF')]
    BIF,

    /// <summary>
    /// Bermudian Dollar
    /// </summary>
    [EnumStringValue('BMD')]
    BMD,

    /// <summary>
    /// Brunei Dollar
    /// </summary>
    [EnumStringValue('BND')]
    BND,

    /// <summary>
    /// Boliviano
    /// </summary>
    [EnumStringValue('BOB')]
    BOB,

    /// <summary>
    /// Mvdol
    /// </summary>
    [EnumStringValue('BOV')]
    BOV,

    /// <summary>
    /// Brazilian Real
    /// </summary>
    [EnumStringValue('BRL')]
    BRL,

    /// <summary>
    /// Bahamian Dollar
    /// </summary>
    [EnumStringValue('BSD')]
    BSD,

    /// <summary>
    /// Ngultrum
    /// </summary>
    [EnumStringValue('BTN')]
    BTN,

    /// <summary>
    /// Pula
    /// </summary>
    [EnumStringValue('BWP')]
    BWP,

    /// <summary>
    /// Belarusian Ruble
    /// </summary>
    [EnumStringValue('BYN')]
    BYN,

    /// <summary>
    /// Belize Dollar
    /// </summary>
    [EnumStringValue('BZD')]
    BZD,

    /// <summary>
    /// Canadian Dollar
    /// </summary>
    [EnumStringValue('CAD')]
    CAD,

    /// <summary>
    /// Congolese Franc
    /// </summary>
    [EnumStringValue('CDF')]
    CDF,

    /// <summary>
    /// WIR Euro
    /// </summary>
    [EnumStringValue('CHE')]
    CHE,

    /// <summary>
    /// Swiss Franc
    /// </summary>
    [EnumStringValue('CHF')]
    CHF,

    /// <summary>
    /// WIR Franc
    /// </summary>
    [EnumStringValue('CHW')]
    CHW,

    /// <summary>
    /// Unidad de Fomento
    /// </summary>
    [EnumStringValue('CLF')]
    CLF,

    /// <summary>
    /// Chilean Peso
    /// </summary>
    [EnumStringValue('CLP')]
    CLP,

    /// <summary>
    /// Yuan Renminbi
    /// </summary>
    [EnumStringValue('CNY')]
    CNY,

    /// <summary>
    /// Colombian Peso
    /// </summary>
    [EnumStringValue('COP')]
    COP,

    /// <summary>
    /// Unidad de Valor Real
    /// </summary>
    [EnumStringValue('COU')]
    COU,

    /// <summary>
    /// Costa Rican Colon
    /// </summary>
    [EnumStringValue('CRC')]
    CRC,

    /// <summary>
    /// Cuban Peso
    /// </summary>
    [EnumStringValue('CUP')]
    CUP,

    /// <summary>
    /// Cabo Verde Escudo
    /// </summary>
    [EnumStringValue('CVE')]
    CVE,

    /// <summary>
    /// Czech Koruna
    /// </summary>
    [EnumStringValue('CZK')]
    CZK,

    /// <summary>
    /// Djibouti Franc
    /// </summary>
    [EnumStringValue('DJF')]
    DJF,

    /// <summary>
    /// Danish Krone
    /// </summary>
    [EnumStringValue('DKK')]
    DKK,

    /// <summary>
    /// Dominican Peso
    /// </summary>
    [EnumStringValue('DOP')]
    DOP,

    /// <summary>
    /// Algerian Dinar
    /// </summary>
    [EnumStringValue('DZD')]
    DZD,

    /// <summary>
    /// Egyptian Pound
    /// </summary>
    [EnumStringValue('EGP')]
    EGP,

    /// <summary>
    /// Nakfa
    /// </summary>
    [EnumStringValue('ERN')]
    ERN,

    /// <summary>
    /// Ethiopian Birr
    /// </summary>
    [EnumStringValue('ETB')]
    ETB,

    /// <summary>
    /// Euro
    /// </summary>
    [EnumStringValue('EUR')]
    EUR,

    /// <summary>
    /// Fiji Dollar
    /// </summary>
    [EnumStringValue('FJD')]
    FJD,

    /// <summary>
    /// Falkland Islands Pound
    /// </summary>
    [EnumStringValue('FKP')]
    FKP,

    /// <summary>
    /// Pound Sterling
    /// </summary>
    [EnumStringValue('GBP')]
    GBP,

    /// <summary>
    /// Lari
    /// </summary>
    [EnumStringValue('GEL')]
    GEL,

    /// <summary>
    /// Ghana Cedi
    /// </summary>
    [EnumStringValue('GHS')]
    GHS,

    /// <summary>
    /// Gibraltar Pound
    /// </summary>
    [EnumStringValue('GIP')]
    GIP,

    /// <summary>
    /// Dalasi
    /// </summary>
    [EnumStringValue('GMD')]
    GMD,

    /// <summary>
    /// Guinean Franc
    /// </summary>
    [EnumStringValue('GNF')]
    GNF,

    /// <summary>
    /// Quetzal
    /// </summary>
    [EnumStringValue('GTQ')]
    GTQ,

    /// <summary>
    /// Guyana Dollar
    /// </summary>
    [EnumStringValue('GYD')]
    GYD,

    /// <summary>
    /// Hong Kong Dollar
    /// </summary>
    [EnumStringValue('HKD')]
    HKD,

    /// <summary>
    /// Lempira
    /// </summary>
    [EnumStringValue('HNL')]
    HNL,

    /// <summary>
    /// Gourde
    /// </summary>
    [EnumStringValue('HTG')]
    HTG,

    /// <summary>
    /// Forint
    /// </summary>
    [EnumStringValue('HUF')]
    HUF,

    /// <summary>
    /// Rupiah
    /// </summary>
    [EnumStringValue('IDR')]
    IDR,

    /// <summary>
    /// New Israeli Sheqel
    /// </summary>
    [EnumStringValue('ILS')]
    ILS,

    /// <summary>
    /// Indian Rupee
    /// </summary>
    [EnumStringValue('INR')]
    INR,

    /// <summary>
    /// Iraqi Dinar
    /// </summary>
    [EnumStringValue('IQD')]
    IQD,

    /// <summary>
    /// Iranian Rial
    /// </summary>
    [EnumStringValue('IRR')]
    IRR,

    /// <summary>
    /// Iceland Krona
    /// </summary>
    [EnumStringValue('ISK')]
    ISK,

    /// <summary>
    /// Jamaican Dollar
    /// </summary>
    [EnumStringValue('JMD')]
    JMD,

    /// <summary>
    /// Jordanian Dinar
    /// </summary>
    [EnumStringValue('JOD')]
    JOD,

    /// <summary>
    /// Yen
    /// </summary>
    [EnumStringValue('JPY')]
    JPY,

    /// <summary>
    /// Kenyan Shilling
    /// </summary>
    [EnumStringValue('KES')]
    KES,

    /// <summary>
    /// Som
    /// </summary>
    [EnumStringValue('KGS')]
    KGS,

    /// <summary>
    /// Riel
    /// </summary>
    [EnumStringValue('KHR')]
    KHR,

    /// <summary>
    /// Comorian Franc 
    /// </summary>
    [EnumStringValue('KMF')]
    KMF,

    /// <summary>
    /// North Korean Won
    /// </summary>
    [EnumStringValue('KPW')]
    KPW,

    /// <summary>
    /// Won
    /// </summary>
    [EnumStringValue('KRW')]
    KRW,

    /// <summary>
    /// Kuwaiti Dinar
    /// </summary>
    [EnumStringValue('KWD')]
    KWD,

    /// <summary>
    /// Cayman Islands Dollar
    /// </summary>
    [EnumStringValue('KYD')]
    KYD,

    /// <summary>
    /// Tenge
    /// </summary>
    [EnumStringValue('KZT')]
    KZT,

    /// <summary>
    /// Lao Kip
    /// </summary>
    [EnumStringValue('LAK')]
    LAK,

    /// <summary>
    /// Lebanese Pound
    /// </summary>
    [EnumStringValue('LBP')]
    LBP,

    /// <summary>
    /// Sri Lanka Rupee
    /// </summary>
    [EnumStringValue('LKR')]
    LKR,

    /// <summary>
    /// Liberian Dollar
    /// </summary>
    [EnumStringValue('LRD')]
    LRD,

    /// <summary>
    /// Loti
    /// </summary>
    [EnumStringValue('LSL')]
    LSL,

    /// <summary>
    /// Libyan Dinar
    /// </summary>
    [EnumStringValue('LYD')]
    LYD,

    /// <summary>
    /// Moroccan Dirham
    /// </summary>
    [EnumStringValue('MAD')]
    MAD,

    /// <summary>
    /// Moldovan Leu
    /// </summary>
    [EnumStringValue('MDL')]
    MDL,

    /// <summary>
    /// Malagasy Ariary
    /// </summary>
    [EnumStringValue('MGA')]
    MGA,

    /// <summary>
    /// Denar
    /// </summary>
    [EnumStringValue('MKD')]
    MKD,

    /// <summary>
    /// Kyat
    /// </summary>
    [EnumStringValue('MMK')]
    MMK,

    /// <summary>
    /// Tugrik
    /// </summary>
    [EnumStringValue('MNT')]
    MNT,

    /// <summary>
    /// Pataca
    /// </summary>
    [EnumStringValue('MOP')]
    MOP,

    /// <summary>
    /// Ouguiya
    /// </summary>
    [EnumStringValue('MRU')]
    MRU,

    /// <summary>
    /// Mauritius Rupee
    /// </summary>
    [EnumStringValue('MUR')]
    MUR,

    /// <summary>
    /// Rufiyaa
    /// </summary>
    [EnumStringValue('MVR')]
    MVR,

    /// <summary>
    /// Malawi Kwacha
    /// </summary>
    [EnumStringValue('MWK')]
    MWK,

    /// <summary>
    /// Mexican Peso
    /// </summary>
    [EnumStringValue('MXN')]
    MXN,

    /// <summary>
    /// Mexican Unidad de Inversion (UDI)
    /// </summary>
    [EnumStringValue('MXV')]
    MXV,

    /// <summary>
    /// Malaysian Ringgit
    /// </summary>
    [EnumStringValue('MYR')]
    MYR,

    /// <summary>
    /// Mozambique Metical
    /// </summary>
    [EnumStringValue('MZN')]
    MZN,

    /// <summary>
    /// Namibia Dollar
    /// </summary>
    [EnumStringValue('NAD')]
    NAD,

    /// <summary>
    /// Naira
    /// </summary>
    [EnumStringValue('NGN')]
    NGN,

    /// <summary>
    /// Cordoba Oro
    /// </summary>
    [EnumStringValue('NIO')]
    NIO,

    /// <summary>
    /// Norwegian Krone
    /// </summary>
    [EnumStringValue('NOK')]
    NOK,

    /// <summary>
    /// Nepalese Rupee
    /// </summary>
    [EnumStringValue('NPR')]
    NPR,

    /// <summary>
    /// New Zealand Dollar
    /// </summary>
    [EnumStringValue('NZD')]
    NZD,

    /// <summary>
    /// Rial Omani
    /// </summary>
    [EnumStringValue('OMR')]
    OMR,

    /// <summary>
    /// Balboa
    /// </summary>
    [EnumStringValue('PAB')]
    PAB,

    /// <summary>
    /// Sol
    /// </summary>
    [EnumStringValue('PEN')]
    PEN,

    /// <summary>
    /// Kina
    /// </summary>
    [EnumStringValue('PGK')]
    PGK,

    /// <summary>
    /// Philippine Peso
    /// </summary>
    [EnumStringValue('PHP')]
    PHP,

    /// <summary>
    /// Pakistan Rupee
    /// </summary>
    [EnumStringValue('PKR')]
    PKR,

    /// <summary>
    /// Zloty
    /// </summary>
    [EnumStringValue('PLN')]
    PLN,

    /// <summary>
    /// Guarani
    /// </summary>
    [EnumStringValue('PYG')]
    PYG,

    /// <summary>
    /// Qatari Rial
    /// </summary>
    [EnumStringValue('QAR')]
    QAR,

    /// <summary>
    /// Romanian Leu
    /// </summary>
    [EnumStringValue('RON')]
    RON,

    /// <summary>
    /// Serbian Dinar
    /// </summary>
    [EnumStringValue('RSD')]
    RSD,

    /// <summary>
    /// Russian Ruble
    /// </summary>
    [EnumStringValue('RUB')]
    RUB,

    /// <summary>
    /// Rwanda Franc
    /// </summary>
    [EnumStringValue('RWF')]
    RWF,

    /// <summary>
    /// Saudi Riyal
    /// </summary>
    [EnumStringValue('SAR')]
    SAR,

    /// <summary>
    /// Solomon Islands Dollar
    /// </summary>
    [EnumStringValue('SBD')]
    SBD,

    /// <summary>
    /// Seychelles Rupee
    /// </summary>
    [EnumStringValue('SCR')]
    SCR,

    /// <summary>
    /// Sudanese Pound
    /// </summary>
    [EnumStringValue('SDG')]
    SDG,

    /// <summary>
    /// Swedish Krona
    /// </summary>
    [EnumStringValue('SEK')]
    SEK,

    /// <summary>
    /// Singapore Dollar
    /// </summary>
    [EnumStringValue('SGD')]
    SGD,

    /// <summary>
    /// Saint Helena Pound
    /// </summary>
    [EnumStringValue('SHP')]
    SHP,

    /// <summary>
    /// Sierra Leone (new valuation 2022)
    /// </summary>
    [EnumStringValue('SLE')]
    SLE,

    /// <summary>
    /// Somali Shilling
    /// </summary>
    [EnumStringValue('SOS')]
    SOS,

    /// <summary>
    /// Surinam Dollar
    /// </summary>
    [EnumStringValue('SRD')]
    SRD,

    /// <summary>
    /// South Sudanese Pound
    /// </summary>
    [EnumStringValue('SSP')]
    SSP,

    /// <summary>
    /// Dobra
    /// </summary>
    [EnumStringValue('STN')]
    STN,

    /// <summary>
    /// El Salvador Colon
    /// </summary>
    [EnumStringValue('SVC')]
    SVC,

    /// <summary>
    /// Syrian Pound
    /// </summary>
    [EnumStringValue('SYP')]
    SYP,

    /// <summary>
    /// Lilangeni
    /// </summary>
    [EnumStringValue('SZL')]
    SZL,

    /// <summary>
    /// Baht
    /// </summary>
    [EnumStringValue('THB')]
    THB,

    /// <summary>
    /// Somoni
    /// </summary>
    [EnumStringValue('TJS')]
    TJS,

    /// <summary>
    /// Turkmenistan New Manat
    /// </summary>
    [EnumStringValue('TMT')]
    TMT,

    /// <summary>
    /// Tunisian Dinar
    /// </summary>
    [EnumStringValue('TND')]
    TND,

    /// <summary>
    /// Pa’anga
    /// </summary>
    [EnumStringValue('TOP')]
    TOP,

    /// <summary>
    /// Turkish Lira
    /// </summary>
    [EnumStringValue('TRY')]
    TRY_,

    /// <summary>
    /// Trinidad and Tobago Dollar
    /// </summary>
    [EnumStringValue('TTD')]
    TTD,

    /// <summary>
    /// New Taiwan Dollar
    /// </summary>
    [EnumStringValue('TWD')]
    TWD,

    /// <summary>
    /// Tanzanian Shilling
    /// </summary>
    [EnumStringValue('TZS')]
    TZS,

    /// <summary>
    /// Hryvnia
    /// </summary>
    [EnumStringValue('UAH')]
    UAH,

    /// <summary>
    /// Uganda Shilling
    /// </summary>
    [EnumStringValue('UGX')]
    UGX,

    /// <summary>
    /// US Dollar
    /// </summary>
    [EnumStringValue('USD')]
    USD,

    /// <summary>
    /// US Dollar (Next day)
    /// </summary>
    [EnumStringValue('USN')]
    USN,

    /// <summary>
    /// Uruguay Peso en Unidades Indexadas (UI)
    /// </summary>
    [EnumStringValue('UYI')]
    UYI,

    /// <summary>
    /// Peso Uruguayo
    /// </summary>
    [EnumStringValue('UYU')]
    UYU,

    /// <summary>
    /// Unidad Previsional
    /// </summary>
    [EnumStringValue('UYW')]
    UYW,

    /// <summary>
    /// Uzbekistan Sum
    /// </summary>
    [EnumStringValue('UZS')]
    UZS,

    /// <summary>
    /// Bolívar Soberano, new valuation
    /// </summary>
    [EnumStringValue('VED')]
    VED,

    /// <summary>
    /// Bolívar Soberano
    /// </summary>
    [EnumStringValue('VES')]
    VES,

    /// <summary>
    /// Dong
    /// </summary>
    [EnumStringValue('VND')]
    VND,

    /// <summary>
    /// Vatu
    /// </summary>
    [EnumStringValue('VUV')]
    VUV,

    /// <summary>
    /// Tala
    /// </summary>
    [EnumStringValue('WST')]
    WST,

    /// <summary>
    /// CFA Franc BEAC
    /// </summary>
    [EnumStringValue('XAF')]
    XAF,

    /// <summary>
    /// Silver
    /// </summary>
    [EnumStringValue('XAG')]
    XAG,

    /// <summary>
    /// Gold
    /// </summary>
    [EnumStringValue('XAU')]
    XAU,

    /// <summary>
    /// Bond Markets Unit European Composite Unit (EURCO)
    /// </summary>
    [EnumStringValue('XBA')]
    XBA,

    /// <summary>
    /// Bond Markets Unit European Monetary Unit (E.M.U.-6)
    /// </summary>
    [EnumStringValue('XBB')]
    XBB,

    /// <summary>
    /// Bond Markets Unit European Unit of Account 9 (E.U.A.-9)
    /// </summary>
    [EnumStringValue('XBC')]
    XBC,

    /// <summary>
    /// Bond Markets Unit European Unit of Account 17 (E.U.A.-17)
    /// </summary>
    [EnumStringValue('XBD')]
    XBD,

    /// <summary>
    /// East Caribbean Dollar
    /// </summary>
    [EnumStringValue('XCD')]
    XCD,

    /// <summary>
    /// SDR (Special Drawing Right)
    /// </summary>
    [EnumStringValue('XDR')]
    XDR,

    /// <summary>
    /// CFA Franc BCEAO
    /// </summary>
    [EnumStringValue('XOF')]
    XOF,

    /// <summary>
    /// Palladium
    /// </summary>
    [EnumStringValue('XPD')]
    XPD,

    /// <summary>
    /// CFP Franc
    /// </summary>
    [EnumStringValue('XPF')]
    XPF,

    /// <summary>
    /// Platinum
    /// </summary>
    [EnumStringValue('XPT')]
    XPT,

    /// <summary>
    /// Sucre
    /// </summary>
    [EnumStringValue('XSU')]
    XSU,

    /// <summary>
    /// Codes specifically reserved for testing purposes
    /// </summary>
    [EnumStringValue('XTS')]
    XTS,

    /// <summary>
    /// ADB Unit of Account
    /// </summary>
    [EnumStringValue('XUA')]
    XUA,

    /// <summary>
    /// The codes assigned for transactions where no currency is involved
    /// </summary>
    [EnumStringValue('XXX')]
    XXX,

    /// <summary>
    /// Yemeni Rial
    /// </summary>
    [EnumStringValue('YER')]
    YER,

    /// <summary>
    /// Rand
    /// </summary>
    [EnumStringValue('ZAR')]
    ZAR,

    /// <summary>
    /// Zambian Kwacha
    /// </summary>
    [EnumStringValue('ZMW')]
    ZMW,

    /// <summary>
    /// Zimbabwe Gold
    /// </summary>
    [EnumStringValue('ZWG')]
    ZWG,


    /// <summary>
    /// Unknown value
    /// </summary>
    Unknown
    {.DefinitionEnd}
  );

implementation

procedure Map (EnumValue: TZUGFeRDCurrencyCodes; StringValue: string); inline;
begin
  TEnumExtensions<TZUGFeRDCurrencyCodes>.RegisterMapping(EnumValue, StringValue)
end;

procedure InitMapping;
begin
  {.MapStart}
  // Mapping generated by PSDelphiDefinitionMapper
  Map(AED,     'AED');
  Map(AFN,     'AFN');
  Map(ALL,     'ALL');
  Map(AMD,     'AMD');
  Map(ANG,     'ANG');
  Map(AOA,     'AOA');
  Map(ARS,     'ARS');
  Map(AUD,     'AUD');
  Map(AWG,     'AWG');
  Map(AZN,     'AZN');
  Map(BAM,     'BAM');
  Map(BBD,     'BBD');
  Map(BDT,     'BDT');
  Map(BGN,     'BGN');
  Map(BHD,     'BHD');
  Map(BIF,     'BIF');
  Map(BMD,     'BMD');
  Map(BND,     'BND');
  Map(BOB,     'BOB');
  Map(BOV,     'BOV');
  Map(BRL,     'BRL');
  Map(BSD,     'BSD');
  Map(BTN,     'BTN');
  Map(BWP,     'BWP');
  Map(BYN,     'BYN');
  Map(BZD,     'BZD');
  Map(CAD,     'CAD');
  Map(CDF,     'CDF');
  Map(CHE,     'CHE');
  Map(CHF,     'CHF');
  Map(CHW,     'CHW');
  Map(CLF,     'CLF');
  Map(CLP,     'CLP');
  Map(CNY,     'CNY');
  Map(COP,     'COP');
  Map(COU,     'COU');
  Map(CRC,     'CRC');
  Map(CUP,     'CUP');
  Map(CVE,     'CVE');
  Map(CZK,     'CZK');
  Map(DJF,     'DJF');
  Map(DKK,     'DKK');
  Map(DOP,     'DOP');
  Map(DZD,     'DZD');
  Map(EGP,     'EGP');
  Map(ERN,     'ERN');
  Map(ETB,     'ETB');
  Map(EUR,     'EUR');
  Map(FJD,     'FJD');
  Map(FKP,     'FKP');
  Map(GBP,     'GBP');
  Map(GEL,     'GEL');
  Map(GHS,     'GHS');
  Map(GIP,     'GIP');
  Map(GMD,     'GMD');
  Map(GNF,     'GNF');
  Map(GTQ,     'GTQ');
  Map(GYD,     'GYD');
  Map(HKD,     'HKD');
  Map(HNL,     'HNL');
  Map(HTG,     'HTG');
  Map(HUF,     'HUF');
  Map(IDR,     'IDR');
  Map(ILS,     'ILS');
  Map(INR,     'INR');
  Map(IQD,     'IQD');
  Map(IRR,     'IRR');
  Map(ISK,     'ISK');
  Map(JMD,     'JMD');
  Map(JOD,     'JOD');
  Map(JPY,     'JPY');
  Map(KES,     'KES');
  Map(KGS,     'KGS');
  Map(KHR,     'KHR');
  Map(KMF,     'KMF');
  Map(KPW,     'KPW');
  Map(KRW,     'KRW');
  Map(KWD,     'KWD');
  Map(KYD,     'KYD');
  Map(KZT,     'KZT');
  Map(LAK,     'LAK');
  Map(LBP,     'LBP');
  Map(LKR,     'LKR');
  Map(LRD,     'LRD');
  Map(LSL,     'LSL');
  Map(LYD,     'LYD');
  Map(MAD,     'MAD');
  Map(MDL,     'MDL');
  Map(MGA,     'MGA');
  Map(MKD,     'MKD');
  Map(MMK,     'MMK');
  Map(MNT,     'MNT');
  Map(MOP,     'MOP');
  Map(MRU,     'MRU');
  Map(MUR,     'MUR');
  Map(MVR,     'MVR');
  Map(MWK,     'MWK');
  Map(MXN,     'MXN');
  Map(MXV,     'MXV');
  Map(MYR,     'MYR');
  Map(MZN,     'MZN');
  Map(NAD,     'NAD');
  Map(NGN,     'NGN');
  Map(NIO,     'NIO');
  Map(NOK,     'NOK');
  Map(NPR,     'NPR');
  Map(NZD,     'NZD');
  Map(OMR,     'OMR');
  Map(PAB,     'PAB');
  Map(PEN,     'PEN');
  Map(PGK,     'PGK');
  Map(PHP,     'PHP');
  Map(PKR,     'PKR');
  Map(PLN,     'PLN');
  Map(PYG,     'PYG');
  Map(QAR,     'QAR');
  Map(RON,     'RON');
  Map(RSD,     'RSD');
  Map(RUB,     'RUB');
  Map(RWF,     'RWF');
  Map(SAR,     'SAR');
  Map(SBD,     'SBD');
  Map(SCR,     'SCR');
  Map(SDG,     'SDG');
  Map(SEK,     'SEK');
  Map(SGD,     'SGD');
  Map(SHP,     'SHP');
  Map(SLE,     'SLE');
  Map(SOS,     'SOS');
  Map(SRD,     'SRD');
  Map(SSP,     'SSP');
  Map(STN,     'STN');
  Map(SVC,     'SVC');
  Map(SYP,     'SYP');
  Map(SZL,     'SZL');
  Map(THB,     'THB');
  Map(TJS,     'TJS');
  Map(TMT,     'TMT');
  Map(TND,     'TND');
  Map(TOP,     'TOP');
  Map(TRY_,    'TRY');
  Map(TTD,     'TTD');
  Map(TWD,     'TWD');
  Map(TZS,     'TZS');
  Map(UAH,     'UAH');
  Map(UGX,     'UGX');
  Map(USD,     'USD');
  Map(USN,     'USN');
  Map(UYI,     'UYI');
  Map(UYU,     'UYU');
  Map(UYW,     'UYW');
  Map(UZS,     'UZS');
  Map(VED,     'VED');
  Map(VES,     'VES');
  Map(VND,     'VND');
  Map(VUV,     'VUV');
  Map(WST,     'WST');
  Map(XAF,     'XAF');
  Map(XAG,     'XAG');
  Map(XAU,     'XAU');
  Map(XBA,     'XBA');
  Map(XBB,     'XBB');
  Map(XBC,     'XBC');
  Map(XBD,     'XBD');
  Map(XCD,     'XCD');
  Map(XDR,     'XDR');
  Map(XOF,     'XOF');
  Map(XPD,     'XPD');
  Map(XPF,     'XPF');
  Map(XPT,     'XPT');
  Map(XSU,     'XSU');
  Map(XTS,     'XTS');
  Map(XUA,     'XUA');
  Map(XXX,     'XXX');
  Map(YER,     'YER');
  Map(ZAR,     'ZAR');
  Map(ZMW,     'ZMW');
  Map(ZWG,     'ZWG');
  Map(Unknown, 'Unknown');
{.MapEnd}
end;

Initialization
  InitMapping;
end.
