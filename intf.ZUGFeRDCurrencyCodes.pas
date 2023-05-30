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
  System.SysUtils,System.TypInfo
  ;

type
  // Source: https://raw.githubusercontent.com/datasets/currency-codes/master/data/codes-all.csv
  //
  // You can regenerate the codes using:
  //
  // import csv
  //
  // currencies = {}
  //
  // with open('currencies.csv', newline= '') as f:
  // lines = csv.reader(f, delimiter = ',', quotechar = '"')
  // for line in lines:
  //     if len(line) < 4:
  //         continue
  //
  //     country = line[0]
  //     currency = line[1]
  //     alphaThree = line[2]
  //     code = line[3]
  //
  //     if len(code) == 0:
  //         continue
  //
  //     # first line
  //     if country.find('Entity') > -1:
  //         continue
  //
  //     if currency.find('Zimbabwe Dollar (old)') > -1:
  //         continue
  //
  //     if code in currencies:
  //         continue
  //
  //     currencies[code] = { 'country' : country, 'currency' : currency, 'alphaThree' : alphaThree, 'code' : code }
  //
  // g = open('currencies.cs', 'w+')
  //
  // for currency in currencies.values():
  //     g.write('/// <summary>\n')
  //     g.write('/// Country: ' + currency['country'] + '\n')
  //     g.write('/// Currency: ' + currency['currency'] + '\n')
  //     g.write('/// </summary>\n')
  //     g.write(currency['alphaThree'] + ' = ' + currency['code'] + ',\n')
  //     g.write('\n')
  //
  // g.close()


  /// <summary>
  /// Full usage of http://csharpmoney.codeplex.com/ not required here,
  /// mapping of ISO codes sufficient.
  ///
  /// ISO 4217 currency codes
  ///
  /// Source: https://raw.githubusercontent.com/datasets/currency-codes/master/data/codes-all.csv
  /// </summary>
  TZUGFeRDCurrencyCodes = (
    /// <summary>
    /// Country: AFGHANISTAN
    /// Currency: Afghani
    /// </summary>
    AFN = 971,

    /// <summary>
    /// Country: ÅLAND ISLANDS
    /// Currency: Euro
    /// </summary>
    EUR = 978,

    /// <summary>
    /// Country: ALBANIA
    /// Currency: Lek
    /// </summary>
    ALL = 008,

    /// <summary>
    /// Country: ALGERIA
    /// Currency: Algerian Dinar
    /// </summary>
    DZD = 012,

    /// <summary>
    /// Country: AMERICAN SAMOA
    /// Currency: US Dollar
    /// </summary>
    USD = 840,

    /// <summary>
    /// Country: ANGOLA
    /// Currency: Kwanza
    /// </summary>
    AOA = 973,

    /// <summary>
    /// Country: ANGUILLA
    /// Currency: East Caribbean Dollar
    /// </summary>
    XCD = 951,

    /// <summary>
    /// Country: ARGENTINA
    /// Currency: Argentine Peso
    /// </summary>
    ARS = 032,

    /// <summary>
    /// Country: ARMENIA
    /// Currency: Armenian Dram
    /// </summary>
    AMD = 051,

    /// <summary>
    /// Country: ARUBA
    /// Currency: Aruban Florin
    /// </summary>
    AWG = 533,

    /// <summary>
    /// Country: AUSTRALIA
    /// Currency: Australian Dollar
    /// </summary>
    AUD = 036,

    /// <summary>
    /// Country: AZERBAIJAN
    /// Currency: Azerbaijan Manat
    /// </summary>
    AZN = 944,

    /// <summary>
    /// Country: BAHAMAS (THE)
    /// Currency: Bahamian Dollar
    /// </summary>
    BSD = 044,

    /// <summary>
    /// Country: BAHRAIN
    /// Currency: Bahraini Dinar
    /// </summary>
    BHD = 048,

    /// <summary>
    /// Country: BANGLADESH
    /// Currency: Taka
    /// </summary>
    BDT = 050,

    /// <summary>
    /// Country: BARBADOS
    /// Currency: Barbados Dollar
    /// </summary>
    BBD = 052,

    /// <summary>
    /// Country: BELARUS
    /// Currency: Belarusian Ruble
    /// </summary>
    BYN = 933,

    /// <summary>
    /// Country: BELIZE
    /// Currency: Belize Dollar
    /// </summary>
    BZD = 084,

    /// <summary>
    /// Country: BENIN
    /// Currency: CFA Franc BCEAO
    /// </summary>
    XOF = 952,

    /// <summary>
    /// Country: BERMUDA
    /// Currency: Bermudian Dollar
    /// </summary>
    BMD = 060,

    /// <summary>
    /// Country: BHUTAN
    /// Currency: Indian Rupee
    /// </summary>
    INR = 356,

    /// <summary>
    /// Country: BHUTAN
    /// Currency: Ngultrum
    /// </summary>
    BTN = 064,

    /// <summary>
    /// Country: BOLIVIA (PLURINATIONAL STATE OF)
    /// Currency: Boliviano
    /// </summary>
    BOB = 068,

    /// <summary>
    /// Country: BOLIVIA (PLURINATIONAL STATE OF)
    /// Currency: Mvdol
    /// </summary>
    BOV = 984,

    /// <summary>
    /// Country: BOSNIA AND HERZEGOVINA
    /// Currency: Convertible Mark
    /// </summary>
    BAM = 977,

    /// <summary>
    /// Country: BOTSWANA
    /// Currency: Pula
    /// </summary>
    BWP = 072,

    /// <summary>
    /// Country: BOUVET ISLAND
    /// Currency: Norwegian Krone
    /// </summary>
    NOK = 578,

    /// <summary>
    /// Country: BRAZIL
    /// Currency: Brazilian Real
    /// </summary>
    BRL = 986,

    /// <summary>
    /// Country: BRUNEI DARUSSALAM
    /// Currency: Brunei Dollar
    /// </summary>
    BND = 096,

    /// <summary>
    /// Country: BULGARIA
    /// Currency: Bulgarian Lev
    /// </summary>
    BGN = 975,

    /// <summary>
    /// Country: BURUNDI
    /// Currency: Burundi Franc
    /// </summary>
    BIF = 108,

    /// <summary>
    /// Country: CABO VERDE
    /// Currency: Cabo Verde Escudo
    /// </summary>
    CVE = 132,

    /// <summary>
    /// Country: CAMBODIA
    /// Currency: Riel
    /// </summary>
    KHR = 116,

    /// <summary>
    /// Country: CAMEROON
    /// Currency: CFA Franc BEAC
    /// </summary>
    XAF = 950,

    /// <summary>
    /// Country: CANADA
    /// Currency: Canadian Dollar
    /// </summary>
    CAD = 124,

    /// <summary>
    /// Country: CAYMAN ISLANDS (THE)
    /// Currency: Cayman Islands Dollar
    /// </summary>
    KYD = 136,

    /// <summary>
    /// Country: CHILE
    /// Currency: Chilean Peso
    /// </summary>
    CLP = 152,

    /// <summary>
    /// Country: CHILE
    /// Currency: Unidad de Fomento
    /// </summary>
    CLF = 990,

    /// <summary>
    /// Country: CHINA
    /// Currency: Yuan Renminbi
    /// </summary>
    CNY = 156,

    /// <summary>
    /// Country: COLOMBIA
    /// Currency: Colombian Peso
    /// </summary>
    COP = 170,

    /// <summary>
    /// Country: COLOMBIA
    /// Currency: Unidad de Valor Real
    /// </summary>
    COU = 970,

    /// <summary>
    /// Country: COMOROS (THE)
    /// Currency: Comorian Franc
    /// </summary>
    KMF = 174,

    /// <summary>
    /// Country: CONGO (THE DEMOCRATIC REPUBLIC OF THE)
    /// Currency: Congolese Franc
    /// </summary>
    CDF = 976,

    /// <summary>
    /// Country: COOK ISLANDS (THE)
    /// Currency: New Zealand Dollar
    /// </summary>
    NZD = 554,

    /// <summary>
    /// Country: COSTA RICA
    /// Currency: Costa Rican Colon
    /// </summary>
    CRC = 188,

    /// <summary>
    /// Country: CROATIA
    /// Currency: Kuna
    /// </summary>
    HRK = 191,

    /// <summary>
    /// Country: CUBA
    /// Currency: Cuban Peso
    /// </summary>
    CUP = 192,

    /// <summary>
    /// Country: CUBA
    /// Currency: Peso Convertible
    /// </summary>
    CUC = 931,

    /// <summary>
    /// Country: CURAÇAO
    /// Currency: Netherlands Antillean Guilder
    /// </summary>
    ANG = 532,

    /// <summary>
    /// Country: CZECHIA
    /// Currency: Czech Koruna
    /// </summary>
    CZK = 203,

    /// <summary>
    /// Country: DENMARK
    /// Currency: Danish Krone
    /// </summary>
    DKK = 208,

    /// <summary>
    /// Country: DJIBOUTI
    /// Currency: Djibouti Franc
    /// </summary>
    DJF = 262,

    /// <summary>
    /// Country: DOMINICAN REPUBLIC (THE)
    /// Currency: Dominican Peso
    /// </summary>
    DOP = 214,

    /// <summary>
    /// Country: EGYPT
    /// Currency: Egyptian Pound
    /// </summary>
    EGP = 818,

    /// <summary>
    /// Country: EL SALVADOR
    /// Currency: El Salvador Colon
    /// </summary>
    SVC = 222,

    /// <summary>
    /// Country: ERITREA
    /// Currency: Nakfa
    /// </summary>
    ERN = 232,

    /// <summary>
    /// Country: ETHIOPIA
    /// Currency: Ethiopian Birr
    /// </summary>
    ETB = 230,

    /// <summary>
    /// Country: FALKLAND ISLANDS (THE) [MALVINAS]
    /// Currency: Falkland Islands Pound
    /// </summary>
    FKP = 238,

    /// <summary>
    /// Country: FIJI
    /// Currency: Fiji Dollar
    /// </summary>
    FJD = 242,

    /// <summary>
    /// Country: FRENCH POLYNESIA
    /// Currency: CFP Franc
    /// </summary>
    XPF = 953,

    /// <summary>
    /// Country: GAMBIA (THE)
    /// Currency: Dalasi
    /// </summary>
    GMD = 270,

    /// <summary>
    /// Country: GEORGIA
    /// Currency: Lari
    /// </summary>
    GEL = 981,

    /// <summary>
    /// Country: GHANA
    /// Currency: Ghana Cedi
    /// </summary>
    GHS = 936,

    /// <summary>
    /// Country: GIBRALTAR
    /// Currency: Gibraltar Pound
    /// </summary>
    GIP = 292,

    /// <summary>
    /// Country: GUATEMALA
    /// Currency: Quetzal
    /// </summary>
    GTQ = 320,

    /// <summary>
    /// Country: GUERNSEY
    /// Currency: Pound Sterling
    /// </summary>
    GBP = 826,

    /// <summary>
    /// Country: GUINEA
    /// Currency: Guinean Franc
    /// </summary>
    GNF = 324,

    /// <summary>
    /// Country: GUYANA
    /// Currency: Guyana Dollar
    /// </summary>
    GYD = 328,

    /// <summary>
    /// Country: HAITI
    /// Currency: Gourde
    /// </summary>
    HTG = 332,

    /// <summary>
    /// Country: HONDURAS
    /// Currency: Lempira
    /// </summary>
    HNL = 340,

    /// <summary>
    /// Country: HONG KONG
    /// Currency: Hong Kong Dollar
    /// </summary>
    HKD = 344,

    /// <summary>
    /// Country: HUNGARY
    /// Currency: Forint
    /// </summary>
    HUF = 348,

    /// <summary>
    /// Country: ICELAND
    /// Currency: Iceland Krona
    /// </summary>
    ISK = 352,

    /// <summary>
    /// Country: INDONESIA
    /// Currency: Rupiah
    /// </summary>
    IDR = 360,

    /// <summary>
    /// Country: INTERNATIONAL MONETARY FUND (IMF)
    /// Currency: SDR (Special Drawing Right)
    /// </summary>
    XDR = 960,

    /// <summary>
    /// Country: IRAN (ISLAMIC REPUBLIC OF)
    /// Currency: Iranian Rial
    /// </summary>
    IRR = 364,

    /// <summary>
    /// Country: IRAQ
    /// Currency: Iraqi Dinar
    /// </summary>
    IQD = 368,

    /// <summary>
    /// Country: ISRAEL
    /// Currency: New Israeli Sheqel
    /// </summary>
    ILS = 376,

    /// <summary>
    /// Country: JAMAICA
    /// Currency: Jamaican Dollar
    /// </summary>
    JMD = 388,

    /// <summary>
    /// Country: JAPAN
    /// Currency: Yen
    /// </summary>
    JPY = 392,

    /// <summary>
    /// Country: JORDAN
    /// Currency: Jordanian Dinar
    /// </summary>
    JOD = 400,

    /// <summary>
    /// Country: KAZAKHSTAN
    /// Currency: Tenge
    /// </summary>
    KZT = 398,

    /// <summary>
    /// Country: KENYA
    /// Currency: Kenyan Shilling
    /// </summary>
    KES = 404,

    /// <summary>
    /// Country: KOREA (THE DEMOCRATIC PEOPLE’S REPUBLIC OF)
    /// Currency: North Korean Won
    /// </summary>
    KPW = 408,

    /// <summary>
    /// Country: KOREA (THE REPUBLIC OF)
    /// Currency: Won
    /// </summary>
    KRW = 410,

    /// <summary>
    /// Country: KUWAIT
    /// Currency: Kuwaiti Dinar
    /// </summary>
    KWD = 414,

    /// <summary>
    /// Country: KYRGYZSTAN
    /// Currency: Som
    /// </summary>
    KGS = 417,

    /// <summary>
    /// Country: LAO PEOPLE’S DEMOCRATIC REPUBLIC (THE)
    /// Currency: Lao Kip
    /// </summary>
    LAK = 418,

    /// <summary>
    /// Country: LEBANON
    /// Currency: Lebanese Pound
    /// </summary>
    LBP = 422,

    /// <summary>
    /// Country: LESOTHO
    /// Currency: Loti
    /// </summary>
    LSL = 426,

    /// <summary>
    /// Country: LESOTHO
    /// Currency: Rand
    /// </summary>
    ZAR = 710,

    /// <summary>
    /// Country: LIBERIA
    /// Currency: Liberian Dollar
    /// </summary>
    LRD = 430,

    /// <summary>
    /// Country: LIBYA
    /// Currency: Libyan Dinar
    /// </summary>
    LYD = 434,

    /// <summary>
    /// Country: LIECHTENSTEIN
    /// Currency: Swiss Franc
    /// </summary>
    CHF = 756,

    /// <summary>
    /// Country: MACAO
    /// Currency: Pataca
    /// </summary>
    MOP = 446,

    /// <summary>
    /// Country: MACEDONIA (THE FORMER YUGOSLAV REPUBLIC OF)
    /// Currency: Denar
    /// </summary>
    MKD = 807,

    /// <summary>
    /// Country: MADAGASCAR
    /// Currency: Malagasy Ariary
    /// </summary>
    MGA = 969,

    /// <summary>
    /// Country: MALAWI
    /// Currency: Malawi Kwacha
    /// </summary>
    MWK = 454,

    /// <summary>
    /// Country: MALAYSIA
    /// Currency: Malaysian Ringgit
    /// </summary>
    MYR = 458,

    /// <summary>
    /// Country: MALDIVES
    /// Currency: Rufiyaa
    /// </summary>
    MVR = 462,

    /// <summary>
    /// Country: MAURITANIA
    /// Currency: Ouguiya
    /// </summary>
    MRU = 929,

    /// <summary>
    /// Country: MAURITIUS
    /// Currency: Mauritius Rupee
    /// </summary>
    MUR = 480,

    /// <summary>
    /// Country: MEMBER COUNTRIES OF THE AFRICAN DEVELOPMENT BANK GROUP
    /// Currency: ADB Unit of Account
    /// </summary>
    XUA = 965,

    /// <summary>
    /// Country: MEXICO
    /// Currency: Mexican Peso
    /// </summary>
    MXN = 484,

    /// <summary>
    /// Country: MEXICO
    /// Currency: Mexican Unidad de Inversion (UDI)
    /// </summary>
    MXV = 979,

    /// <summary>
    /// Country: MOLDOVA (THE REPUBLIC OF)
    /// Currency: Moldovan Leu
    /// </summary>
    MDL = 498,

    /// <summary>
    /// Country: MONGOLIA
    /// Currency: Tugrik
    /// </summary>
    MNT = 496,

    /// <summary>
    /// Country: MOROCCO
    /// Currency: Moroccan Dirham
    /// </summary>
    MAD = 504,

    /// <summary>
    /// Country: MOZAMBIQUE
    /// Currency: Mozambique Metical
    /// </summary>
    MZN = 943,

    /// <summary>
    /// Country: MYANMAR
    /// Currency: Kyat
    /// </summary>
    MMK = 104,

    /// <summary>
    /// Country: NAMIBIA
    /// Currency: Namibia Dollar
    /// </summary>
    NAD = 516,

    /// <summary>
    /// Country: NEPAL
    /// Currency: Nepalese Rupee
    /// </summary>
    NPR = 524,

    /// <summary>
    /// Country: NICARAGUA
    /// Currency: Cordoba Oro
    /// </summary>
    NIO = 558,

    /// <summary>
    /// Country: NIGERIA
    /// Currency: Naira
    /// </summary>
    NGN = 566,

    /// <summary>
    /// Country: OMAN
    /// Currency: Rial Omani
    /// </summary>
    OMR = 512,

    /// <summary>
    /// Country: PAKISTAN
    /// Currency: Pakistan Rupee
    /// </summary>
    PKR = 586,

    /// <summary>
    /// Country: PANAMA
    /// Currency: Balboa
    /// </summary>
    PAB = 590,

    /// <summary>
    /// Country: PAPUA NEW GUINEA
    /// Currency: Kina
    /// </summary>
    PGK = 598,

    /// <summary>
    /// Country: PARAGUAY
    /// Currency: Guarani
    /// </summary>
    PYG = 600,

    /// <summary>
    /// Country: PERU
    /// Currency: Sol
    /// </summary>
    PEN = 604,

    /// <summary>
    /// Country: PHILIPPINES (THE)
    /// Currency: Philippine Peso
    /// </summary>
    PHP = 608,

    /// <summary>
    /// Country: POLAND
    /// Currency: Zloty
    /// </summary>
    PLN = 985,

    /// <summary>
    /// Country: QATAR
    /// Currency: Qatari Rial
    /// </summary>
    QAR = 634,

    /// <summary>
    /// Country: ROMANIA
    /// Currency: Romanian Leu
    /// </summary>
    RON = 946,

    /// <summary>
    /// Country: RUSSIAN FEDERATION (THE)
    /// Currency: Russian Ruble
    /// </summary>
    RUB = 643,

    /// <summary>
    /// Country: RWANDA
    /// Currency: Rwanda Franc
    /// </summary>
    RWF = 646,

    /// <summary>
    /// Country: SAINT HELENA, ASCENSION AND TRISTAN DA CUNHA
    /// Currency: Saint Helena Pound
    /// </summary>
    SHP = 654,

    /// <summary>
    /// Country: SAMOA
    /// Currency: Tala
    /// </summary>
    WST = 882,

    /// <summary>
    /// Country: SAO TOME AND PRINCIPE
    /// Currency: Dobra
    /// </summary>
    STN = 930,

    /// <summary>
    /// Country: SAUDI ARABIA
    /// Currency: Saudi Riyal
    /// </summary>
    SAR = 682,

    /// <summary>
    /// Country: SERBIA
    /// Currency: Serbian Dinar
    /// </summary>
    RSD = 941,

    /// <summary>
    /// Country: SEYCHELLES
    /// Currency: Seychelles Rupee
    /// </summary>
    SCR = 690,

    /// <summary>
    /// Country: SIERRA LEONE
    /// Currency: Leone
    /// </summary>
    SLL = 694,

    /// <summary>
    /// Country: SINGAPORE
    /// Currency: Singapore Dollar
    /// </summary>
    SGD = 702,

    /// <summary>
    /// Country: SISTEMA UNITARIO DE COMPENSACION REGIONAL DE PAGOS "SUCRE"
    /// Currency: Sucre
    /// </summary>
    XSU = 994,

    /// <summary>
    /// Country: SOLOMON ISLANDS
    /// Currency: Solomon Islands Dollar
    /// </summary>
    SBD = 090,

    /// <summary>
    /// Country: SOMALIA
    /// Currency: Somali Shilling
    /// </summary>
    SOS = 706,

    /// <summary>
    /// Country: SOUTH SUDAN
    /// Currency: South Sudanese Pound
    /// </summary>
    SSP = 728,

    /// <summary>
    /// Country: SRI LANKA
    /// Currency: Sri Lanka Rupee
    /// </summary>
    LKR = 144,

    /// <summary>
    /// Country: SUDAN (THE)
    /// Currency: Sudanese Pound
    /// </summary>
    SDG = 938,

    /// <summary>
    /// Country: SURINAME
    /// Currency: Surinam Dollar
    /// </summary>
    SRD = 968,

    /// <summary>
    /// Country: ESWATINI
    /// Currency: Lilangeni
    /// </summary>
    SZL = 748,

    /// <summary>
    /// Country: SWEDEN
    /// Currency: Swedish Krona
    /// </summary>
    SEK = 752,

    /// <summary>
    /// Country: SWITZERLAND
    /// Currency: WIR Euro
    /// </summary>
    CHE = 947,

    /// <summary>
    /// Country: SWITZERLAND
    /// Currency: WIR Franc
    /// </summary>
    CHW = 948,

    /// <summary>
    /// Country: SYRIAN ARAB REPUBLIC
    /// Currency: Syrian Pound
    /// </summary>
    SYP = 760,

    /// <summary>
    /// Country: TAIWAN (PROVINCE OF CHINA)
    /// Currency: New Taiwan Dollar
    /// </summary>
    TWD = 901,

    /// <summary>
    /// Country: TAJIKISTAN
    /// Currency: Somoni
    /// </summary>
    TJS = 972,

    /// <summary>
    /// Country: TANZANIA, UNITED REPUBLIC OF
    /// Currency: Tanzanian Shilling
    /// </summary>
    TZS = 834,

    /// <summary>
    /// Country: THAILAND
    /// Currency: Baht
    /// </summary>
    THB = 764,

    /// <summary>
    /// Country: TONGA
    /// Currency: Pa’anga
    /// </summary>
    TOP = 776,

    /// <summary>
    /// Country: TRINIDAD AND TOBAGO
    /// Currency: Trinidad and Tobago Dollar
    /// </summary>
    TTD = 780,

    /// <summary>
    /// Country: TUNISIA
    /// Currency: Tunisian Dinar
    /// </summary>
    TND = 788,

    /// <summary>
    /// Country: TURKEY
    /// Currency: Turkish Lira
    /// </summary>
    TRY_ = 949,

    /// <summary>
    /// Country: TURKMENISTAN
    /// Currency: Turkmenistan New Manat
    /// </summary>
    TMT = 934,

    /// <summary>
    /// Country: UGANDA
    /// Currency: Uganda Shilling
    /// </summary>
    UGX = 800,

    /// <summary>
    /// Country: UKRAINE
    /// Currency: Hryvnia
    /// </summary>
    UAH = 980,

    /// <summary>
    /// Country: UNITED ARAB EMIRATES (THE)
    /// Currency: UAE Dirham
    /// </summary>
    AED = 784,

    /// <summary>
    /// Country: UNITED STATES OF AMERICA (THE)
    /// Currency: US Dollar (Next day)
    /// </summary>
    USN = 997,

    /// <summary>
    /// Country: URUGUAY
    /// Currency: Peso Uruguayo
    /// </summary>
    UYU = 858,

    /// <summary>
    /// Country: URUGUAY
    /// Currency: Uruguay Peso en Unidades Indexadas (UI)
    /// </summary>
    UYI = 940,

    /// <summary>
    /// Country: URUGUAY
    /// Currency: Unidad Previsional
    /// </summary>
    UYW = 927,

    /// <summary>
    /// Country: UZBEKISTAN
    /// Currency: Uzbekistan Sum
    /// </summary>
    UZS = 860,

    /// <summary>
    /// Country: VANUATU
    /// Currency: Vatu
    /// </summary>
    VUV = 548,

    /// <summary>
    /// Country: VENEZUELA (BOLIVARIAN REPUBLIC OF)
    /// Currency: Bolívar Soberano
    /// </summary>
    VES = 928,

    /// <summary>
    /// Country: VIET NAM
    /// Currency: Dong
    /// </summary>
    VND = 704,

    /// <summary>
    /// Country: YEMEN
    /// Currency: Yemeni Rial
    /// </summary>
    YER = 886,

    /// <summary>
    /// Country: ZAMBIA
    /// Currency: Zambian Kwacha
    /// </summary>
    ZMW = 967,

    /// <summary>
    /// Country: ZIMBABWE
    /// Currency: Zimbabwe Dollar
    /// </summary>
    ZWL = 932,

    /// <summary>
    /// Country: ZZ01_Bond Markets Unit European_EURCO
    /// Currency: Bond Markets Unit European Composite Unit (EURCO)
    /// </summary>
    XBA = 955,

    /// <summary>
    /// Country: ZZ02_Bond Markets Unit European_EMU-6
    /// Currency: Bond Markets Unit European Monetary Unit (E.M.U.-6)
    /// </summary>
    XBB = 956,

    /// <summary>
    /// Country: ZZ03_Bond Markets Unit European_EUA-9
    /// Currency: Bond Markets Unit European Unit of Account 9 (E.U.A.-9)
    /// </summary>
    XBC = 957,

    /// <summary>
    /// Country: ZZ04_Bond Markets Unit European_EUA-17
    /// Currency: Bond Markets Unit European Unit of Account 17 (E.U.A.-17)
    /// </summary>
    XBD = 958,

    /// <summary>
    /// Country: ZZ06_Testing_Code
    /// Currency: Codes specifically reserved for testing purposes
    /// </summary>
    XTS = 963,

    /// <summary>
    /// Country: ZZ07_No_Currency
    /// Currency: The codes assigned for transactions where no currency is involved
    /// </summary>
    XXX = 999,

    /// <summary>
    /// Country: ZZ08_Gold
    /// Currency: Gold
    /// </summary>
    XAU = 959,

    /// <summary>
    /// Country: ZZ09_Palladium
    /// Currency: Palladium
    /// </summary>
    XPD = 964,

    /// <summary>
    /// Country: ZZ10_Platinum
    /// Currency: Platinum
    /// </summary>
    XPT = 962,

    /// <summary>
    /// Country: ZZ11_Silver
    /// Currency: Silver
    /// </summary>
    XAG = 961,

    /// <summary>
    /// Country: AFGHANISTAN
    /// Currency: Afghani
    /// </summary>
    AFA = 004,

    /// <summary>
    /// Country: ÅLAND ISLANDS
    /// Currency: Markka
    /// </summary>
    FIM = 246,

    /// <summary>
    /// Country: ANDORRA
    /// Currency: Andorran Peseta
    /// </summary>
    ADP = 020,

    /// <summary>
    /// Country: ANDORRA
    /// Currency: Spanish Peseta
    /// </summary>
    ESP = 724,

    /// <summary>
    /// Country: ANDORRA
    /// Currency: French Franc
    /// </summary>
    FRF = 250,

    /// <summary>
    /// Country: ANGOLA
    /// Currency: Kwanza
    /// </summary>
    AOK = 024,

    /// <summary>
    /// Country: ANGOLA
    /// Currency: Kwanza Reajustado
    /// </summary>
    AOR = 982,

    /// <summary>
    /// Country: ARMENIA
    /// Currency: Russian Ruble
    /// </summary>
    RUR = 810,

    /// <summary>
    /// Country: AUSTRIA
    /// Currency: Schilling
    /// </summary>
    ATS = 040,

    /// <summary>
    /// Country: AZERBAIJAN
    /// Currency: Azerbaijan Manat
    /// </summary>
    AYM = 945,

    /// <summary>
    /// Country: AZERBAIJAN
    /// Currency: Azerbaijanian Manat
    /// </summary>
    AZM = 031,

    /// <summary>
    /// Country: BELARUS
    /// Currency: Belarusian Ruble
    /// </summary>
    BYB = 112,

    /// <summary>
    /// Country: BELARUS
    /// Currency: Belarusian Ruble
    /// </summary>
    BYR = 974,

    /// <summary>
    /// Country: BELGIUM
    /// Currency: Convertible Franc
    /// </summary>
    BEC = 993,

    /// <summary>
    /// Country: BELGIUM
    /// Currency: Belgian Franc
    /// </summary>
    BEF = 056,

    /// <summary>
    /// Country: BELGIUM
    /// Currency: Financial Franc
    /// </summary>
    BEL = 992,

    /// <summary>
    /// Country: BOSNIA AND HERZEGOVINA
    /// Currency: Dinar
    /// </summary>
    BAD = 070,

    /// <summary>
    /// Country: BRAZIL
    /// Currency: Cruzeiro
    /// </summary>
    BRB = 076,

    /// <summary>
    /// Country: BRAZIL
    /// Currency: Cruzeiro Real
    /// </summary>
    BRR = 987,

    /// <summary>
    /// Country: BULGARIA
    /// Currency: Lev A/52
    /// </summary>
    BGJ = 100,

    /// <summary>
    /// Country: CYPRUS
    /// Currency: Cyprus Pound
    /// </summary>
    CYP = 196,

    /// <summary>
    /// Country: CZECHOSLOVAKIA
    /// Currency: Koruna
    /// </summary>
    CSK = 200,

    /// <summary>
    /// Country: ECUADOR
    /// Currency: Sucre
    /// </summary>
    ECS = 218,

    /// <summary>
    /// Country: ECUADOR
    /// Currency: Unidad de Valor Constante (UVC)
    /// </summary>
    ECV = 983,

    /// <summary>
    /// Country: EQUATORIAL GUINEA
    /// Currency: Ekwele
    /// </summary>
    GQE = 226,

    /// <summary>
    /// Country: ESTONIA
    /// Currency: Kroon
    /// </summary>
    EEK = 233,

    /// <summary>
    /// Country: EUROPEAN MONETARY CO-OPERATION FUND (EMCF)
    /// Currency: European Currency Unit (E.C.U)
    /// </summary>
    XEU = 954,

    /// <summary>
    /// Country: GEORGIA
    /// Currency: Georgian Coupon
    /// </summary>
    GEK = 268,

    /// <summary>
    /// Country: GERMAN DEMOCRATIC REPUBLIC
    /// Currency: Mark der DDR
    /// </summary>
    DDM = 278,

    /// <summary>
    /// Country: GERMANY
    /// Currency: Deutsche Mark
    /// </summary>
    DEM = 276,

    /// <summary>
    /// Country: GHANA
    /// Currency: Cedi
    /// </summary>
    GHC = 288,

    /// <summary>
    /// Country: GHANA
    /// Currency: Ghana Cedi
    /// </summary>
    GHP = 939,

    /// <summary>
    /// Country: GREECE
    /// Currency: Drachma
    /// </summary>
    GRD = 300,

    /// <summary>
    /// Country: GUINEA-BISSAU
    /// Currency: Guinea Escudo
    /// </summary>
    GWE = 624,

    /// <summary>
    /// Country: HOLY SEE (VATICAN CITY STATE)
    /// Currency: Italian Lira
    /// </summary>
    ITL = 380,

    /// <summary>
    /// Country: IRELAND
    /// Currency: Irish Pound
    /// </summary>
    IEP = 372,

    /// <summary>
    /// Country: LATVIA
    /// Currency: Latvian Lats
    /// </summary>
    LVL = 428,

    /// <summary>
    /// Country: LESOTHO
    /// Currency: Financial Rand
    /// </summary>
    ZAL = 991,

    /// <summary>
    /// Country: LITHUANIA
    /// Currency: Lithuanian Litas
    /// </summary>
    LTL = 440,

    /// <summary>
    /// Country: LUXEMBOURG
    /// Currency: Luxembourg Convertible Franc
    /// </summary>
    LUC = 989,

    /// <summary>
    /// Country: LUXEMBOURG
    /// Currency: Luxembourg Franc
    /// </summary>
    LUF = 442,

    /// <summary>
    /// Country: LUXEMBOURG
    /// Currency: Luxembourg Financial Franc
    /// </summary>
    LUL = 988,

    /// <summary>
    /// Country: MADAGASCAR
    /// Currency: Malagasy Franc
    /// </summary>
    MGF = 450,

    /// <summary>
    /// Country: MALI
    /// Currency: Mali Franc
    /// </summary>
    MLF = 466,

    /// <summary>
    /// Country: MALTA
    /// Currency: Maltese Lira
    /// </summary>
    MTL = 470,

    /// <summary>
    /// Country: MAURITANIA
    /// Currency: Ouguiya
    /// </summary>
    MRO = 478,

    /// <summary>
    /// Country: MOZAMBIQUE
    /// Currency: Mozambique Escudo
    /// </summary>
    MZE = 508,

    /// <summary>
    /// Country: NETHERLANDS
    /// Currency: Netherlands Guilder
    /// </summary>
    NLG = 528,

    /// <summary>
    /// Country: POLAND
    /// Currency: Zloty
    /// </summary>
    PLZ = 616,

    /// <summary>
    /// Country: PORTUGAL
    /// Currency: Portuguese Escudo
    /// </summary>
    PTE = 620,

    /// <summary>
    /// Country: ROMANIA
    /// Currency: Leu A/52
    /// </summary>
    ROK = 642,

    /// <summary>
    /// Country: SAO TOME AND PRINCIPE
    /// Currency: Dobra
    /// </summary>
    STD = 678,

    /// <summary>
    /// Country: SERBIA AND MONTENEGRO
    /// Currency: Serbian Dinar
    /// </summary>
    CSD = 891,

    /// <summary>
    /// Country: SLOVAKIA
    /// Currency: Slovak Koruna
    /// </summary>
    SKK = 703,

    /// <summary>
    /// Country: SLOVENIA
    /// Currency: Tolar
    /// </summary>
    SIT = 705,

    /// <summary>
    /// Country: SOUTHERN RHODESIA
    /// Currency: Rhodesian Dollar
    /// </summary>
    RHD = 716,

    /// <summary>
    /// Country: SPAIN
    /// Currency: Spanish Peseta
    /// </summary>
    ESA = 996,

    /// <summary>
    /// Country: SPAIN
    /// Currency: "A" Account (convertible Peseta Account)
    /// </summary>
    ESB = 995,

    /// <summary>
    /// Country: SUDAN
    /// Currency: Sudanese Dinar
    /// </summary>
    SDD = 736,

    /// <summary>
    /// Country: SURINAME
    /// Currency: Surinam Guilder
    /// </summary>
    SRG = 740,

    /// <summary>
    /// Country: TAJIKISTAN
    /// Currency: Tajik Ruble
    /// </summary>
    TJR = 762,

    /// <summary>
    /// Country: TIMOR-LESTE
    /// Currency: Timor Escudo
    /// </summary>
    TPE = 626,

    /// <summary>
    /// Country: TURKEY
    /// Currency: Old Turkish Lira
    /// </summary>
    TRL = 792,

    /// <summary>
    /// Country: TURKMENISTAN
    /// Currency: Turkmenistan Manat
    /// </summary>
    TMM = 795,

    /// <summary>
    /// Country: UKRAINE
    /// Currency: Karbovanet
    /// </summary>
    UAK = 804,

    /// <summary>
    /// Country: UNITED STATES
    /// Currency: US Dollar (Same day)
    /// </summary>
    USS = 998,

    /// <summary>
    /// Country: VENEZUELA
    /// Currency: Bolivar
    /// </summary>
    VEB = 862,

    /// <summary>
    /// Country: VENEZUELA
    /// Currency: Bolivar Fuerte
    /// </summary>
    VEF = 937,

    /// <summary>
    /// Country: YEMEN, DEMOCRATIC
    /// Currency: Yemeni Dinar
    /// </summary>
    YDD = 720,

    /// <summary>
    /// Country: YUGOSLAVIA
    /// Currency: New Yugoslavian Dinar
    /// </summary>
    YUD = 890,

    /// <summary>
    /// Country: ZAIRE
    /// Currency: New Zaire
    /// </summary>
    ZRN = 180,

    /// <summary>
    /// Country: ZAMBIA
    /// Currency: Zambian Kwacha
    /// </summary>
    ZMK = 894,

    /// <summary>
    /// Country: ZIMBABWE
    /// Currency: Zimbabwe Dollar (new)
    /// </summary>
    ZWN = 942,

    /// <summary>
    /// Country: ZIMBABWE
    /// Currency: Zimbabwe Dollar
    /// </summary>
    ZWR = 935,

    /// <summary>
    /// Fallback value
    /// </summary>
    Unknown = 0
  );

  TZUGFeRDCurrencyCodesExtensions = class
  public
    class function FromString(const s: string): TZUGFeRDCurrencyCodes;
    class function EnumToString(codes: TZUGFeRDCurrencyCodes): string;
  end;

implementation

{ TZUGFeRDCurrencyCodesExtensions }

class function TZUGFeRDCurrencyCodesExtensions.EnumToString(
  codes: TZUGFeRDCurrencyCodes): string;
begin
  case codes of
    AFN: Result := 'AFN';
    EUR: Result := 'EUR';
    ALL: Result := 'ALL';
    DZD: Result := 'DZD';
    USD: Result := 'USD';
    AOA: Result := 'AOA';
    XCD: Result := 'XCD';
    ARS: Result := 'ARS';
    AMD: Result := 'AMD';
    AWG: Result := 'AWG';
    AUD: Result := 'AUD';
    AZN: Result := 'AZN';
    BSD: Result := 'BSD';
    BHD: Result := 'BHD';
    BDT: Result := 'BDT';
    BBD: Result := 'BBD';
    BYN: Result := 'BYN';
    BZD: Result := 'BZD';
    XOF: Result := 'XOF';
    BMD: Result := 'BMD';
    INR: Result := 'INR';
    BTN: Result := 'BTN';
    BOB: Result := 'BOB';
    BOV: Result := 'BOV';
    BAM: Result := 'BAM';
    BWP: Result := 'BWP';
    NOK: Result := 'NOK';
    BRL: Result := 'BRL';
    BND: Result := 'BND';
    BGN: Result := 'BGN';
    BIF: Result := 'BIF';
    CVE: Result := 'CVE';
    KHR: Result := 'KHR';
    XAF: Result := 'XAF';
    CAD: Result := 'CAD';
    KYD: Result := 'KYD';
    CLP: Result := 'CLP';
    CLF: Result := 'CLF';
    CNY: Result := 'CNY';
    COP: Result := 'COP';
    COU: Result := 'COU';
    KMF: Result := 'KMF';
    CDF: Result := 'CDF';
    NZD: Result := 'NZD';
    CRC: Result := 'CRC';
    HRK: Result := 'HRK';
    CUP: Result := 'CUP';
    CUC: Result := 'CUC';
    ANG: Result := 'ANG';
    CZK: Result := 'CZK';
    DKK: Result := 'DKK';
    DJF: Result := 'DJF';
    DOP: Result := 'DOP';
    EGP: Result := 'EGP';
    SVC: Result := 'SVC';
    ERN: Result := 'ERN';
    ETB: Result := 'ETB';
    FKP: Result := 'FKP';
    FJD: Result := 'FJD';
    XPF: Result := 'XPF';
    GMD: Result := 'GMD';
    GEL: Result := 'GEL';
    GHS: Result := 'GHS';
    GIP: Result := 'GIP';
    GTQ: Result := 'GTQ';
    GBP: Result := 'GBP';
    GNF: Result := 'GNF';
    GYD: Result := 'GYD';
    HTG: Result := 'HTG';
    HNL: Result := 'HNL';
    HKD: Result := 'HKD';
    HUF: Result := 'HUF';
    ISK: Result := 'ISK';
    IDR: Result := 'IDR';
    XDR: Result := 'XDR';
    IRR: Result := 'IRR';
    IQD: Result := 'IQD';
    ILS: Result := 'ILS';
    JMD: Result := 'JMD';
    JPY: Result := 'JPY';
    JOD: Result := 'JOD';
    KZT: Result := 'KZT';
    KES: Result := 'KES';
    KPW: Result := 'KPW';
    KRW: Result := 'KRW';
    KWD: Result := 'KWD';
    KGS: Result := 'KGS';
    LAK: Result := 'LAK';
    LBP: Result := 'LBP';
    LSL: Result := 'LSL';
    ZAR: Result := 'ZAR';
    LRD: Result := 'LRD';
    LYD: Result := 'LYD';
    CHF: Result := 'CHF';
    MOP: Result := 'MOP';
    MKD: Result := 'MKD';
    MGA: Result := 'MGA';
    MWK: Result := 'MWK';
    MYR: Result := 'MYR';
    MVR: Result := 'MVR';
    MRU: Result := 'MRU';
    MUR: Result := 'MUR';
    XUA: Result := 'XUA';
    MXN: Result := 'MXN';
    MXV: Result := 'MXV';
    MDL: Result := 'MDL';
    MNT: Result := 'MNT';
    MAD: Result := 'MAD';
    MZN: Result := 'MZN';
    MMK: Result := 'MMK';
    NAD: Result := 'NAD';
    NPR: Result := 'NPR';
    NIO: Result := 'NIO';
    NGN: Result := 'NGN';
    OMR: Result := 'OMR';
    PKR: Result := 'PKR';
    PAB: Result := 'PAB';
    PGK: Result := 'PGK';
    PYG: Result := 'PYG';
    PEN: Result := 'PEN';
    PHP: Result := 'PHP';
    PLN: Result := 'PLN';
    QAR: Result := 'QAR';
    RON: Result := 'RON';
    RUB: Result := 'RUB';
    RWF: Result := 'RWF';
    SHP: Result := 'SHP';
    WST: Result := 'WST';
    STN: Result := 'STN';
    SAR: Result := 'SAR';
    RSD: Result := 'RSD';
    SCR: Result := 'SCR';
    SLL: Result := 'SLL';
    SGD: Result := 'SGD';
    XSU: Result := 'XSU';
    SBD: Result := 'SBD';
    SOS: Result := 'SOS';
    SSP: Result := 'SSP';
    LKR: Result := 'LKR';
    SDG: Result := 'SDG';
    SRD: Result := 'SRD';
    SZL: Result := 'SZL';
    SEK: Result := 'SEK';
    CHE: Result := 'CHE';
    CHW: Result := 'CHW';
    SYP: Result := 'SYP';
    TWD: Result := 'TWD';
    TJS: Result := 'TJS';
    TZS: Result := 'TZS';
    THB: Result := 'THB';
    TOP: Result := 'TOP';
    TTD: Result := 'TTD';
    TND: Result := 'TND';
    TRY_:Result := 'TRY';
    TMT: Result := 'TMT';
    UGX: Result := 'UGX';
    UAH: Result := 'UAH';
    AED: Result := 'AED';
    USN: Result := 'USN';
    UYU: Result := 'UYU';
    UYI: Result := 'UYI';
    UYW: Result := 'UYW';
    UZS: Result := 'UZS';
    VUV: Result := 'VUV';
    VES: Result := 'VES';
    VND: Result := 'VND';
    YER: Result := 'YER';
    ZMW: Result := 'ZMW';
    ZWL: Result := 'ZWL';
    XBA: Result := 'XBA';
    XBB: Result := 'XBB';
    XBC: Result := 'XBC';
    XBD: Result := 'XBD';
    XTS: Result := 'XTS';
    XXX: Result := 'XXX';
    XAU: Result := 'XAU';
    XPD: Result := 'XPD';
    XPT: Result := 'XPT';
    XAG: Result := 'XAG';
    AFA: Result := 'AFA';
    FIM: Result := 'FIM';
    ADP: Result := 'ADP';
    ESP: Result := 'ESP';
    FRF: Result := 'FRF';
    AOK: Result := 'AOK';
    AOR: Result := 'AOR';
    RUR: Result := 'RUR';
    ATS: Result := 'ATS';
    AYM: Result := 'AYM';
    AZM: Result := 'AZM';
    BYB: Result := 'BYB';
    BYR: Result := 'BYR';
    BEC: Result := 'BEC';
    BEF: Result := 'BEF';
    BEL: Result := 'BEL';
    BAD: Result := 'BAD';
    BRB: Result := 'BRB';
    BRR: Result := 'BRR';
    BGJ: Result := 'BGJ';
    CYP: Result := 'CYP';
    CSK: Result := 'CSK';
    ECS: Result := 'ECS';
    ECV: Result := 'ECV';
    GQE: Result := 'GQE';
    EEK: Result := 'EEK';
    XEU: Result := 'XEU';
    GEK: Result := 'GEK';
    DDM: Result := 'DDM';
    DEM: Result := 'DEM';
    GHC: Result := 'GHC';
    GHP: Result := 'GHP';
    GRD: Result := 'GRD';
    GWE: Result := 'GWE';
    ITL: Result := 'ITL';
    IEP: Result := 'IEP';
    LVL: Result := 'LVL';
    ZAL: Result := 'ZAL';
    LTL: Result := 'LTL';
    LUC: Result := 'LUC';
    LUF: Result := 'LUF';
    LUL: Result := 'LUL';
    MGF: Result := 'MGF';
    MLF: Result := 'MLF';
    MTL: Result := 'MTL';
    MRO: Result := 'MRO';
    MZE: Result := 'MZE';
    NLG: Result := 'NLG';
    PLZ: Result := 'PLZ';
    PTE: Result := 'PTE';
    ROK: Result := 'ROK';
    STD: Result := 'STD';
    CSD: Result := 'CSD';
    SKK: Result := 'SKK';
    SIT: Result := 'SIT';
    RHD: Result := 'RHD';
    ESA: Result := 'ESA';
    ESB: Result := 'ESB';
    SDD: Result := 'SDD';
    SRG: Result := 'SRG';
    TJR: Result := 'TJR';
    TPE: Result := 'TPE';
    TRL: Result := 'TRL';
    TMM: Result := 'TMM';
    UAK: Result := 'UAK';
    USS: Result := 'USS';
    VEB: Result := 'VEB';
    VEF: Result := 'VEF';
    YDD: Result := 'YDD';
    YUD: Result := 'YUD';
    ZRN: Result := 'ZRN';
    ZMK: Result := 'ZMK';
    ZWN: Result := 'ZWN';
    ZWR: Result := 'ZWR';
    else Result := 'Unknown';
  end;
end;

class function TZUGFeRDCurrencyCodesExtensions.FromString(
  const s: string): TZUGFeRDCurrencyCodes;
begin
  if SameText(s,'AFN') then Result := TZUGFeRDCurrencyCodes.AFN else
  if SameText(s,'EUR') then Result := TZUGFeRDCurrencyCodes.EUR else
  if SameText(s,'ALL') then Result := TZUGFeRDCurrencyCodes.ALL else
  if SameText(s,'DZD') then Result := TZUGFeRDCurrencyCodes.DZD else
  if SameText(s,'USD') then Result := TZUGFeRDCurrencyCodes.USD else
  if SameText(s,'AOA') then Result := TZUGFeRDCurrencyCodes.AOA else
  if SameText(s,'XCD') then Result := TZUGFeRDCurrencyCodes.XCD else
  if SameText(s,'ARS') then Result := TZUGFeRDCurrencyCodes.ARS else
  if SameText(s,'AMD') then Result := TZUGFeRDCurrencyCodes.AMD else
  if SameText(s,'AWG') then Result := TZUGFeRDCurrencyCodes.AWG else
  if SameText(s,'AUD') then Result := TZUGFeRDCurrencyCodes.AUD else
  if SameText(s,'AZN') then Result := TZUGFeRDCurrencyCodes.AZN else
  if SameText(s,'BSD') then Result := TZUGFeRDCurrencyCodes.BSD else
  if SameText(s,'BHD') then Result := TZUGFeRDCurrencyCodes.BHD else
  if SameText(s,'BDT') then Result := TZUGFeRDCurrencyCodes.BDT else
  if SameText(s,'BBD') then Result := TZUGFeRDCurrencyCodes.BBD else
  if SameText(s,'BYN') then Result := TZUGFeRDCurrencyCodes.BYN else
  if SameText(s,'BZD') then Result := TZUGFeRDCurrencyCodes.BZD else
  if SameText(s,'XOF') then Result := TZUGFeRDCurrencyCodes.XOF else
  if SameText(s,'BMD') then Result := TZUGFeRDCurrencyCodes.BMD else
  if SameText(s,'INR') then Result := TZUGFeRDCurrencyCodes.INR else
  if SameText(s,'BTN') then Result := TZUGFeRDCurrencyCodes.BTN else
  if SameText(s,'BOB') then Result := TZUGFeRDCurrencyCodes.BOB else
  if SameText(s,'BOV') then Result := TZUGFeRDCurrencyCodes.BOV else
  if SameText(s,'BAM') then Result := TZUGFeRDCurrencyCodes.BAM else
  if SameText(s,'BWP') then Result := TZUGFeRDCurrencyCodes.BWP else
  if SameText(s,'NOK') then Result := TZUGFeRDCurrencyCodes.NOK else
  if SameText(s,'BRL') then Result := TZUGFeRDCurrencyCodes.BRL else
  if SameText(s,'BND') then Result := TZUGFeRDCurrencyCodes.BND else
  if SameText(s,'BGN') then Result := TZUGFeRDCurrencyCodes.BGN else
  if SameText(s,'BIF') then Result := TZUGFeRDCurrencyCodes.BIF else
  if SameText(s,'CVE') then Result := TZUGFeRDCurrencyCodes.CVE else
  if SameText(s,'KHR') then Result := TZUGFeRDCurrencyCodes.KHR else
  if SameText(s,'XAF') then Result := TZUGFeRDCurrencyCodes.XAF else
  if SameText(s,'CAD') then Result := TZUGFeRDCurrencyCodes.CAD else
  if SameText(s,'KYD') then Result := TZUGFeRDCurrencyCodes.KYD else
  if SameText(s,'CLP') then Result := TZUGFeRDCurrencyCodes.CLP else
  if SameText(s,'CLF') then Result := TZUGFeRDCurrencyCodes.CLF else
  if SameText(s,'CNY') then Result := TZUGFeRDCurrencyCodes.CNY else
  if SameText(s,'COP') then Result := TZUGFeRDCurrencyCodes.COP else
  if SameText(s,'COU') then Result := TZUGFeRDCurrencyCodes.COU else
  if SameText(s,'KMF') then Result := TZUGFeRDCurrencyCodes.KMF else
  if SameText(s,'CDF') then Result := TZUGFeRDCurrencyCodes.CDF else
  if SameText(s,'NZD') then Result := TZUGFeRDCurrencyCodes.NZD else
  if SameText(s,'CRC') then Result := TZUGFeRDCurrencyCodes.CRC else
  if SameText(s,'HRK') then Result := TZUGFeRDCurrencyCodes.HRK else
  if SameText(s,'CUP') then Result := TZUGFeRDCurrencyCodes.CUP else
  if SameText(s,'CUC') then Result := TZUGFeRDCurrencyCodes.CUC else
  if SameText(s,'ANG') then Result := TZUGFeRDCurrencyCodes.ANG else
  if SameText(s,'CZK') then Result := TZUGFeRDCurrencyCodes.CZK else
  if SameText(s,'DKK') then Result := TZUGFeRDCurrencyCodes.DKK else
  if SameText(s,'DJF') then Result := TZUGFeRDCurrencyCodes.DJF else
  if SameText(s,'DOP') then Result := TZUGFeRDCurrencyCodes.DOP else
  if SameText(s,'EGP') then Result := TZUGFeRDCurrencyCodes.EGP else
  if SameText(s,'SVC') then Result := TZUGFeRDCurrencyCodes.SVC else
  if SameText(s,'ERN') then Result := TZUGFeRDCurrencyCodes.ERN else
  if SameText(s,'ETB') then Result := TZUGFeRDCurrencyCodes.ETB else
  if SameText(s,'FKP') then Result := TZUGFeRDCurrencyCodes.FKP else
  if SameText(s,'FJD') then Result := TZUGFeRDCurrencyCodes.FJD else
  if SameText(s,'XPF') then Result := TZUGFeRDCurrencyCodes.XPF else
  if SameText(s,'GMD') then Result := TZUGFeRDCurrencyCodes.GMD else
  if SameText(s,'GEL') then Result := TZUGFeRDCurrencyCodes.GEL else
  if SameText(s,'GHS') then Result := TZUGFeRDCurrencyCodes.GHS else
  if SameText(s,'GIP') then Result := TZUGFeRDCurrencyCodes.GIP else
  if SameText(s,'GTQ') then Result := TZUGFeRDCurrencyCodes.GTQ else
  if SameText(s,'GBP') then Result := TZUGFeRDCurrencyCodes.GBP else
  if SameText(s,'GNF') then Result := TZUGFeRDCurrencyCodes.GNF else
  if SameText(s,'GYD') then Result := TZUGFeRDCurrencyCodes.GYD else
  if SameText(s,'HTG') then Result := TZUGFeRDCurrencyCodes.HTG else
  if SameText(s,'HNL') then Result := TZUGFeRDCurrencyCodes.HNL else
  if SameText(s,'HKD') then Result := TZUGFeRDCurrencyCodes.HKD else
  if SameText(s,'HUF') then Result := TZUGFeRDCurrencyCodes.HUF else
  if SameText(s,'ISK') then Result := TZUGFeRDCurrencyCodes.ISK else
  if SameText(s,'IDR') then Result := TZUGFeRDCurrencyCodes.IDR else
  if SameText(s,'XDR') then Result := TZUGFeRDCurrencyCodes.XDR else
  if SameText(s,'IRR') then Result := TZUGFeRDCurrencyCodes.IRR else
  if SameText(s,'IQD') then Result := TZUGFeRDCurrencyCodes.IQD else
  if SameText(s,'ILS') then Result := TZUGFeRDCurrencyCodes.ILS else
  if SameText(s,'JMD') then Result := TZUGFeRDCurrencyCodes.JMD else
  if SameText(s,'JPY') then Result := TZUGFeRDCurrencyCodes.JPY else
  if SameText(s,'JOD') then Result := TZUGFeRDCurrencyCodes.JOD else
  if SameText(s,'KZT') then Result := TZUGFeRDCurrencyCodes.KZT else
  if SameText(s,'KES') then Result := TZUGFeRDCurrencyCodes.KES else
  if SameText(s,'KPW') then Result := TZUGFeRDCurrencyCodes.KPW else
  if SameText(s,'KRW') then Result := TZUGFeRDCurrencyCodes.KRW else
  if SameText(s,'KWD') then Result := TZUGFeRDCurrencyCodes.KWD else
  if SameText(s,'KGS') then Result := TZUGFeRDCurrencyCodes.KGS else
  if SameText(s,'LAK') then Result := TZUGFeRDCurrencyCodes.LAK else
  if SameText(s,'LBP') then Result := TZUGFeRDCurrencyCodes.LBP else
  if SameText(s,'LSL') then Result := TZUGFeRDCurrencyCodes.LSL else
  if SameText(s,'ZAR') then Result := TZUGFeRDCurrencyCodes.ZAR else
  if SameText(s,'LRD') then Result := TZUGFeRDCurrencyCodes.LRD else
  if SameText(s,'LYD') then Result := TZUGFeRDCurrencyCodes.LYD else
  if SameText(s,'CHF') then Result := TZUGFeRDCurrencyCodes.CHF else
  if SameText(s,'MOP') then Result := TZUGFeRDCurrencyCodes.MOP else
  if SameText(s,'MKD') then Result := TZUGFeRDCurrencyCodes.MKD else
  if SameText(s,'MGA') then Result := TZUGFeRDCurrencyCodes.MGA else
  if SameText(s,'MWK') then Result := TZUGFeRDCurrencyCodes.MWK else
  if SameText(s,'MYR') then Result := TZUGFeRDCurrencyCodes.MYR else
  if SameText(s,'MVR') then Result := TZUGFeRDCurrencyCodes.MVR else
  if SameText(s,'MRU') then Result := TZUGFeRDCurrencyCodes.MRU else
  if SameText(s,'MUR') then Result := TZUGFeRDCurrencyCodes.MUR else
  if SameText(s,'XUA') then Result := TZUGFeRDCurrencyCodes.XUA else
  if SameText(s,'MXN') then Result := TZUGFeRDCurrencyCodes.MXN else
  if SameText(s,'MXV') then Result := TZUGFeRDCurrencyCodes.MXV else
  if SameText(s,'MDL') then Result := TZUGFeRDCurrencyCodes.MDL else
  if SameText(s,'MNT') then Result := TZUGFeRDCurrencyCodes.MNT else
  if SameText(s,'MAD') then Result := TZUGFeRDCurrencyCodes.MAD else
  if SameText(s,'MZN') then Result := TZUGFeRDCurrencyCodes.MZN else
  if SameText(s,'MMK') then Result := TZUGFeRDCurrencyCodes.MMK else
  if SameText(s,'NAD') then Result := TZUGFeRDCurrencyCodes.NAD else
  if SameText(s,'NPR') then Result := TZUGFeRDCurrencyCodes.NPR else
  if SameText(s,'NIO') then Result := TZUGFeRDCurrencyCodes.NIO else
  if SameText(s,'NGN') then Result := TZUGFeRDCurrencyCodes.NGN else
  if SameText(s,'OMR') then Result := TZUGFeRDCurrencyCodes.OMR else
  if SameText(s,'PKR') then Result := TZUGFeRDCurrencyCodes.PKR else
  if SameText(s,'PAB') then Result := TZUGFeRDCurrencyCodes.PAB else
  if SameText(s,'PGK') then Result := TZUGFeRDCurrencyCodes.PGK else
  if SameText(s,'PYG') then Result := TZUGFeRDCurrencyCodes.PYG else
  if SameText(s,'PEN') then Result := TZUGFeRDCurrencyCodes.PEN else
  if SameText(s,'PHP') then Result := TZUGFeRDCurrencyCodes.PHP else
  if SameText(s,'PLN') then Result := TZUGFeRDCurrencyCodes.PLN else
  if SameText(s,'QAR') then Result := TZUGFeRDCurrencyCodes.QAR else
  if SameText(s,'RON') then Result := TZUGFeRDCurrencyCodes.RON else
  if SameText(s,'RUB') then Result := TZUGFeRDCurrencyCodes.RUB else
  if SameText(s,'RWF') then Result := TZUGFeRDCurrencyCodes.RWF else
  if SameText(s,'SHP') then Result := TZUGFeRDCurrencyCodes.SHP else
  if SameText(s,'WST') then Result := TZUGFeRDCurrencyCodes.WST else
  if SameText(s,'STN') then Result := TZUGFeRDCurrencyCodes.STN else
  if SameText(s,'SAR') then Result := TZUGFeRDCurrencyCodes.SAR else
  if SameText(s,'RSD') then Result := TZUGFeRDCurrencyCodes.RSD else
  if SameText(s,'SCR') then Result := TZUGFeRDCurrencyCodes.SCR else
  if SameText(s,'SLL') then Result := TZUGFeRDCurrencyCodes.SLL else
  if SameText(s,'SGD') then Result := TZUGFeRDCurrencyCodes.SGD else
  if SameText(s,'XSU') then Result := TZUGFeRDCurrencyCodes.XSU else
  if SameText(s,'SBD') then Result := TZUGFeRDCurrencyCodes.SBD else
  if SameText(s,'SOS') then Result := TZUGFeRDCurrencyCodes.SOS else
  if SameText(s,'SSP') then Result := TZUGFeRDCurrencyCodes.SSP else
  if SameText(s,'LKR') then Result := TZUGFeRDCurrencyCodes.LKR else
  if SameText(s,'SDG') then Result := TZUGFeRDCurrencyCodes.SDG else
  if SameText(s,'SRD') then Result := TZUGFeRDCurrencyCodes.SRD else
  if SameText(s,'SZL') then Result := TZUGFeRDCurrencyCodes.SZL else
  if SameText(s,'SEK') then Result := TZUGFeRDCurrencyCodes.SEK else
  if SameText(s,'CHE') then Result := TZUGFeRDCurrencyCodes.CHE else
  if SameText(s,'CHW') then Result := TZUGFeRDCurrencyCodes.CHW else
  if SameText(s,'SYP') then Result := TZUGFeRDCurrencyCodes.SYP else
  if SameText(s,'TWD') then Result := TZUGFeRDCurrencyCodes.TWD else
  if SameText(s,'TJS') then Result := TZUGFeRDCurrencyCodes.TJS else
  if SameText(s,'TZS') then Result := TZUGFeRDCurrencyCodes.TZS else
  if SameText(s,'THB') then Result := TZUGFeRDCurrencyCodes.THB else
  if SameText(s,'TOP') then Result := TZUGFeRDCurrencyCodes.TOP else
  if SameText(s,'TTD') then Result := TZUGFeRDCurrencyCodes.TTD else
  if SameText(s,'TND') then Result := TZUGFeRDCurrencyCodes.TND else
  if SameText(s,'TRY') then Result := TZUGFeRDCurrencyCodes.TRY_ else
  if SameText(s,'TMT') then Result := TZUGFeRDCurrencyCodes.TMT else
  if SameText(s,'UGX') then Result := TZUGFeRDCurrencyCodes.UGX else
  if SameText(s,'UAH') then Result := TZUGFeRDCurrencyCodes.UAH else
  if SameText(s,'AED') then Result := TZUGFeRDCurrencyCodes.AED else
  if SameText(s,'USN') then Result := TZUGFeRDCurrencyCodes.USN else
  if SameText(s,'UYU') then Result := TZUGFeRDCurrencyCodes.UYU else
  if SameText(s,'UYI') then Result := TZUGFeRDCurrencyCodes.UYI else
  if SameText(s,'UYW') then Result := TZUGFeRDCurrencyCodes.UYW else
  if SameText(s,'UZS') then Result := TZUGFeRDCurrencyCodes.UZS else
  if SameText(s,'VUV') then Result := TZUGFeRDCurrencyCodes.VUV else
  if SameText(s,'VES') then Result := TZUGFeRDCurrencyCodes.VES else
  if SameText(s,'VND') then Result := TZUGFeRDCurrencyCodes.VND else
  if SameText(s,'YER') then Result := TZUGFeRDCurrencyCodes.YER else
  if SameText(s,'ZMW') then Result := TZUGFeRDCurrencyCodes.ZMW else
  if SameText(s,'ZWL') then Result := TZUGFeRDCurrencyCodes.ZWL else
  if SameText(s,'XBA') then Result := TZUGFeRDCurrencyCodes.XBA else
  if SameText(s,'XBB') then Result := TZUGFeRDCurrencyCodes.XBB else
  if SameText(s,'XBC') then Result := TZUGFeRDCurrencyCodes.XBC else
  if SameText(s,'XBD') then Result := TZUGFeRDCurrencyCodes.XBD else
  if SameText(s,'XTS') then Result := TZUGFeRDCurrencyCodes.XTS else
  if SameText(s,'XXX') then Result := TZUGFeRDCurrencyCodes.XXX else
  if SameText(s,'XAU') then Result := TZUGFeRDCurrencyCodes.XAU else
  if SameText(s,'XPD') then Result := TZUGFeRDCurrencyCodes.XPD else
  if SameText(s,'XPT') then Result := TZUGFeRDCurrencyCodes.XPT else
  if SameText(s,'XAG') then Result := TZUGFeRDCurrencyCodes.XAG else
  if SameText(s,'AFA') then Result := TZUGFeRDCurrencyCodes.AFA else
  if SameText(s,'FIM') then Result := TZUGFeRDCurrencyCodes.FIM else
  if SameText(s,'ADP') then Result := TZUGFeRDCurrencyCodes.ADP else
  if SameText(s,'ESP') then Result := TZUGFeRDCurrencyCodes.ESP else
  if SameText(s,'FRF') then Result := TZUGFeRDCurrencyCodes.FRF else
  if SameText(s,'AOK') then Result := TZUGFeRDCurrencyCodes.AOK else
  if SameText(s,'AOR') then Result := TZUGFeRDCurrencyCodes.AOR else
  if SameText(s,'RUR') then Result := TZUGFeRDCurrencyCodes.RUR else
  if SameText(s,'ATS') then Result := TZUGFeRDCurrencyCodes.ATS else
  if SameText(s,'AYM') then Result := TZUGFeRDCurrencyCodes.AYM else
  if SameText(s,'AZM') then Result := TZUGFeRDCurrencyCodes.AZM else
  if SameText(s,'BYB') then Result := TZUGFeRDCurrencyCodes.BYB else
  if SameText(s,'BYR') then Result := TZUGFeRDCurrencyCodes.BYR else
  if SameText(s,'BEC') then Result := TZUGFeRDCurrencyCodes.BEC else
  if SameText(s,'BEF') then Result := TZUGFeRDCurrencyCodes.BEF else
  if SameText(s,'BEL') then Result := TZUGFeRDCurrencyCodes.BEL else
  if SameText(s,'BAD') then Result := TZUGFeRDCurrencyCodes.BAD else
  if SameText(s,'BRB') then Result := TZUGFeRDCurrencyCodes.BRB else
  if SameText(s,'BRR') then Result := TZUGFeRDCurrencyCodes.BRR else
  if SameText(s,'BGJ') then Result := TZUGFeRDCurrencyCodes.BGJ else
  if SameText(s,'CYP') then Result := TZUGFeRDCurrencyCodes.CYP else
  if SameText(s,'CSK') then Result := TZUGFeRDCurrencyCodes.CSK else
  if SameText(s,'ECS') then Result := TZUGFeRDCurrencyCodes.ECS else
  if SameText(s,'ECV') then Result := TZUGFeRDCurrencyCodes.ECV else
  if SameText(s,'GQE') then Result := TZUGFeRDCurrencyCodes.GQE else
  if SameText(s,'EEK') then Result := TZUGFeRDCurrencyCodes.EEK else
  if SameText(s,'XEU') then Result := TZUGFeRDCurrencyCodes.XEU else
  if SameText(s,'GEK') then Result := TZUGFeRDCurrencyCodes.GEK else
  if SameText(s,'DDM') then Result := TZUGFeRDCurrencyCodes.DDM else
  if SameText(s,'DEM') then Result := TZUGFeRDCurrencyCodes.DEM else
  if SameText(s,'GHC') then Result := TZUGFeRDCurrencyCodes.GHC else
  if SameText(s,'GHP') then Result := TZUGFeRDCurrencyCodes.GHP else
  if SameText(s,'GRD') then Result := TZUGFeRDCurrencyCodes.GRD else
  if SameText(s,'GWE') then Result := TZUGFeRDCurrencyCodes.GWE else
  if SameText(s,'ITL') then Result := TZUGFeRDCurrencyCodes.ITL else
  if SameText(s,'IEP') then Result := TZUGFeRDCurrencyCodes.IEP else
  if SameText(s,'LVL') then Result := TZUGFeRDCurrencyCodes.LVL else
  if SameText(s,'ZAL') then Result := TZUGFeRDCurrencyCodes.ZAL else
  if SameText(s,'LTL') then Result := TZUGFeRDCurrencyCodes.LTL else
  if SameText(s,'LUC') then Result := TZUGFeRDCurrencyCodes.LUC else
  if SameText(s,'LUF') then Result := TZUGFeRDCurrencyCodes.LUF else
  if SameText(s,'LUL') then Result := TZUGFeRDCurrencyCodes.LUL else
  if SameText(s,'MGF') then Result := TZUGFeRDCurrencyCodes.MGF else
  if SameText(s,'MLF') then Result := TZUGFeRDCurrencyCodes.MLF else
  if SameText(s,'MTL') then Result := TZUGFeRDCurrencyCodes.MTL else
  if SameText(s,'MRO') then Result := TZUGFeRDCurrencyCodes.MRO else
  if SameText(s,'MZE') then Result := TZUGFeRDCurrencyCodes.MZE else
  if SameText(s,'NLG') then Result := TZUGFeRDCurrencyCodes.NLG else
  if SameText(s,'PLZ') then Result := TZUGFeRDCurrencyCodes.PLZ else
  if SameText(s,'PTE') then Result := TZUGFeRDCurrencyCodes.PTE else
  if SameText(s,'ROK') then Result := TZUGFeRDCurrencyCodes.ROK else
  if SameText(s,'STD') then Result := TZUGFeRDCurrencyCodes.STD else
  if SameText(s,'CSD') then Result := TZUGFeRDCurrencyCodes.CSD else
  if SameText(s,'SKK') then Result := TZUGFeRDCurrencyCodes.SKK else
  if SameText(s,'SIT') then Result := TZUGFeRDCurrencyCodes.SIT else
  if SameText(s,'RHD') then Result := TZUGFeRDCurrencyCodes.RHD else
  if SameText(s,'ESA') then Result := TZUGFeRDCurrencyCodes.ESA else
  if SameText(s,'ESB') then Result := TZUGFeRDCurrencyCodes.ESB else
  if SameText(s,'SDD') then Result := TZUGFeRDCurrencyCodes.SDD else
  if SameText(s,'SRG') then Result := TZUGFeRDCurrencyCodes.SRG else
  if SameText(s,'TJR') then Result := TZUGFeRDCurrencyCodes.TJR else
  if SameText(s,'TPE') then Result := TZUGFeRDCurrencyCodes.TPE else
  if SameText(s,'TRL') then Result := TZUGFeRDCurrencyCodes.TRL else
  if SameText(s,'TMM') then Result := TZUGFeRDCurrencyCodes.TMM else
  if SameText(s,'UAK') then Result := TZUGFeRDCurrencyCodes.UAK else
  if SameText(s,'USS') then Result := TZUGFeRDCurrencyCodes.USS else
  if SameText(s,'VEB') then Result := TZUGFeRDCurrencyCodes.VEB else
  if SameText(s,'VEF') then Result := TZUGFeRDCurrencyCodes.VEF else
  if SameText(s,'YDD') then Result := TZUGFeRDCurrencyCodes.YDD else
  if SameText(s,'YUD') then Result := TZUGFeRDCurrencyCodes.YUD else
  if SameText(s,'ZRN') then Result := TZUGFeRDCurrencyCodes.ZRN else
  if SameText(s,'ZMK') then Result := TZUGFeRDCurrencyCodes.ZMK else
  if SameText(s,'ZWN') then Result := TZUGFeRDCurrencyCodes.ZWN else
  if SameText(s,'ZWR') then Result := TZUGFeRDCurrencyCodes.ZWR else
  if SameText(s,'Unknown') then Result := TZUGFeRDCurrencyCodes.Unknown else
  Result := TZUGFeRDCurrencyCodes.Unknown;
end;

end.
