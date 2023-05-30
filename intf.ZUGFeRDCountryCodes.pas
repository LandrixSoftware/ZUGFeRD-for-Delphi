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

unit intf.ZUGFeRDCountryCodes;

interface

uses
  System.SysUtils,System.TypInfo
  ;

type
  // You can regenerate the codes using:
  //
  // https://cemil.dev/countrieslist
  //
  // g = open('output.cs', 'w+')
  //
  // with open('countries.json') as json_file:
  // data = json.load(json_file)
  // for p in data:
  //     alphaTwo = p['alpha-2']
  //     countryCode = p['country-code']
  //     name = p['name']
  //     g.write('/// <summary>\n')
  //     g.write('/// ' + name + '\n')
  //     g.write('/// ' + alphaTwo + ' = ' + countryCode + '\n')
  //     g.write('/// </summary>\n')
  //     g.write(alphaTwo + ' = ' + countryCode + ',\n')
  //     g.write('\n')
  //
  // g.close()
  //
  // Kosovo needs manual addition and special treatment

  /// <summary>
  /// Country codes based on ISO 3166 source
  /// with addition of Kosovo according to ZUGFeRD standard
  ///
  /// English short name
  /// Alpha-2 code = numeric code
  /// </summary>
  TZUGFeRDCountryCodes = (
    /// <summary>
    /// Andorra
    /// AD = 20
    /// </summary>
    AD = 20,

    /// <summary>
    /// United Arab Emirates (the)
    /// AE = 784
    /// </summary>
    AE = 784,

    /// <summary>
    /// Afghanistan
    /// AF = 4
    /// </summary>
    AF = 4,

    /// <summary>
    /// Antigua and Barbuda
    /// AG = 28
    /// </summary>
    AG = 28,

    /// <summary>
    /// Anguilla
    /// AI = 660
    /// </summary>
    AI = 660,

    /// <summary>
    /// Albania
    /// AL = 8
    /// </summary>
    AL = 8,

    /// <summary>
    /// Armenia
    /// AM = 51
    /// </summary>
    AM = 51,

    /// <summary>
    /// Angola
    /// AO = 24
    /// </summary>
    AO = 24,

    /// <summary>
    /// Antarctica
    /// AQ = 10
    /// </summary>
    AQ = 10,

    /// <summary>
    /// Argentina
    /// AR = 32
    /// </summary>
    AR = 32,

    /// <summary>
    /// American Samoa
    /// AS = 16
    /// </summary>
    AS_ = 16,

    /// <summary>
    /// Austria
    /// AT = 40
    /// </summary>
    AT = 40,

    /// <summary>
    /// Australia
    /// AU = 36
    /// </summary>
    AU = 36,

    /// <summary>
    /// Aruba
    /// AW = 533
    /// </summary>
    AW = 533,

    /// <summary>
    /// �land Islands
    /// AX = 248
    /// </summary>
    AX = 248,

    /// <summary>
    /// Azerbaijan
    /// AZ = 31
    /// </summary>
    AZ = 31,

    /// <summary>
    /// Bosnia and Herzegovina
    /// BA = 70
    /// </summary>
    BA = 70,

    /// <summary>
    /// Barbados
    /// BB = 52
    /// </summary>
    BB = 52,

    /// <summary>
    /// Bangladesh
    /// BD = 50
    /// </summary>
    BD = 50,

    /// <summary>
    /// Belgium
    /// BE = 56
    /// </summary>
    BE = 56,

    /// <summary>
    /// Burkina Faso
    /// BF = 854
    /// </summary>
    BF = 854,

    /// <summary>
    /// Bulgaria
    /// BG = 100
    /// </summary>
    BG = 100,

    /// <summary>
    /// Bahrain
    /// BH = 48
    /// </summary>
    BH = 48,

    /// <summary>
    /// Burundi
    /// BI = 108
    /// </summary>
    BI = 108,

    /// <summary>
    /// Benin
    /// BJ = 204
    /// </summary>
    BJ = 204,

    /// <summary>
    /// Saint Barthélem
    /// BL = 652
    /// </summary>
    BL = 652,

    /// <summary>
    /// Bermuda
    /// BM = 60
    /// </summary>
    BM = 60,

    /// <summary>
    /// Brunei Darussalam
    /// BN = 96
    /// </summary>
    BN = 96,

    /// <summary>
    /// Bolivia (Plurinational State of)
    /// BO = 68
    /// </summary>
    BO = 68,

    /// <summary>
    /// Bonaire, Sint Eustatius and Saba
    /// BQ = 535
    /// </summary>
    BQ = 535,

    /// <summary>
    /// Brazil
    /// BR = 76
    /// </summary>
    BR = 76,

    /// <summary>
    /// Bahamas (the)
    /// BS = 44
    /// </summary>
    BS = 44,

    /// <summary>
    /// Bhutan
    /// BT = 64
    /// </summary>
    BT = 64,

    /// <summary>
    /// Bouvet Island
    /// BV = 74
    /// </summary>
    BV = 74,

    /// <summary>
    /// Botswana
    /// BW = 72
    /// </summary>
    BW = 72,

    /// <summary>
    /// Belarus
    /// BY = 112
    /// </summary>
    BY = 112,

    /// <summary>
    /// Belize
    /// BZ = 84
    /// </summary>
    BZ = 84,

    /// <summary>
    /// Canada
    /// CA = 124
    /// </summary>
    CA = 124,

    /// <summary>
    /// Cocos (Keeling) Islands (the)
    /// CC = 166
    /// </summary>
    CC = 166,

    /// <summary>
    /// Congo (the Democratic Republic of the)
    /// CD = 180
    /// </summary>
    CD = 180,

    /// <summary>
    /// Central African Republic (the)
    /// CF = 140
    /// </summary>
    CF = 140,

    /// <summary>
    /// Congo (the)
    /// CG = 178
    /// </summary>
    CG = 178,

    /// <summary>
    /// Switzerland
    /// CH = 756
    /// </summary>
    CH = 756,

    /// <summary>
    /// Côte d'Ivoir
    /// CI = 384
    /// </summary>
    CI = 384,

    /// <summary>
    /// Cook Islands (the)
    /// CK = 184
    /// </summary>
    CK = 184,

    /// <summary>
    /// Chile
    /// CL = 152
    /// </summary>
    CL = 152,

    /// <summary>
    /// Cameroon
    /// CM = 120
    /// </summary>
    CM = 120,

    /// <summary>
    /// China
    /// CN = 156
    /// </summary>
    CN = 156,

    /// <summary>
    /// Colombia
    /// CO = 170
    /// </summary>
    CO = 170,

    /// <summary>
    /// Costa Rica
    /// CR = 188
    /// </summary>
    CR = 188,

    /// <summary>
    /// Cuba
    /// CU = 192
    /// </summary>
    CU = 192,

    /// <summary>
    /// Cabo Verde
    /// CV = 132
    /// </summary>
    CV = 132,

    /// <summary>
    /// Curaça
    /// CW = 531
    /// </summary>
    CW = 531,

    /// <summary>
    /// Christmas Island
    /// CX = 162
    /// </summary>
    CX = 162,

    /// <summary>
    /// Cyprus
    /// CY = 196
    /// </summary>
    CY = 196,

    /// <summary>
    /// Czechia
    /// CZ = 203
    /// </summary>
    CZ = 203,

    /// <summary>
    /// Germany
    /// DE = 276
    /// </summary>
    DE = 276,

    /// <summary>
    /// Djibouti
    /// DJ = 262
    /// </summary>
    DJ = 262,

    /// <summary>
    /// Denmark
    /// DK = 208
    /// </summary>
    DK = 208,

    /// <summary>
    /// Dominica
    /// DM = 212
    /// </summary>
    DM = 212,

    /// <summary>
    /// Dominican Republic (the)
    /// DO = 214
    /// </summary>
    DO_ = 214,

    /// <summary>
    /// Algeria
    /// DZ = 12
    /// </summary>
    DZ = 12,

    /// <summary>
    /// Ecuador
    /// EC = 218
    /// </summary>
    EC = 218,

    /// <summary>
    /// Estonia
    /// EE = 233
    /// </summary>
    EE = 233,

    /// <summary>
    /// Egypt
    /// EG = 818
    /// </summary>
    EG = 818,

    /// <summary>
    /// Western Sahara*
    /// EH = 732
    /// </summary>
    EH = 732,

    /// <summary>
    /// Eritrea
    /// ER = 232
    /// </summary>
    ER = 232,

    /// <summary>
    /// Spain
    /// ES = 724
    /// </summary>
    ES = 724,

    /// <summary>
    /// Ethiopia
    /// ET = 231
    /// </summary>
    ET = 231,

    /// <summary>
    /// Finland
    /// FI = 246
    /// </summary>
    FI = 246,

    /// <summary>
    /// Fiji
    /// FJ = 242
    /// </summary>
    FJ = 242,

    /// <summary>
    /// Falkland Islands (the) [Malvinas]
    /// FK = 238
    /// </summary>
    FK = 238,

    /// <summary>
    /// Micronesia (Federated States of)
    /// FM = 583
    /// </summary>
    FM = 583,

    /// <summary>
    /// Faroe Islands (the)
    /// FO = 234
    /// </summary>
    FO = 234,

    /// <summary>
    /// France
    /// FR = 250
    /// </summary>
    FR = 250,

    /// <summary>
    /// Gabon
    /// GA = 266
    /// </summary>
    GA = 266,

    /// <summary>
    /// United Kingdom of Great Britain and Northern Ireland (the)
    /// GB = 826
    /// </summary>
    GB = 826,

    /// <summary>
    /// Grenada
    /// GD = 308
    /// </summary>
    GD = 308,

    /// <summary>
    /// Georgia
    /// GE = 268
    /// </summary>
    GE = 268,

    /// <summary>
    /// Guernsey
    /// GG = 831
    /// </summary>
    GF = 254,

    /// <summary>
    /// Guernsey
    /// GG = 831
    /// </summary>
    GG = 831,

    /// <summary>
    /// Ghana
    /// GH = 288
    /// </summary>
    GH = 288,

    /// <summary>
    /// Gibraltar
    /// GI = 292
    /// </summary>
    GI = 292,

    /// <summary>
    /// Greenland
    /// GL = 304
    /// </summary>
    GL = 304,

    /// <summary>
    /// Gambia (the)
    /// GM = 270
    /// </summary>
    GM = 270,

    /// <summary>
    /// Guinea
    /// GN = 324
    /// </summary>
    GN = 324,

    /// <summary>
    /// Guadeloupe
    /// GP = 312
    /// </summary>
    GP = 312,

    /// <summary>
    /// Equatorial Guinea
    /// GQ = 226
    /// </summary>
    GQ = 226,

    /// <summary>
    /// Greece
    /// GR = 300
    /// </summary>
    GR = 300,

    /// <summary>
    /// South Georgia and the South Sandwich Islands
    /// GS = 239
    /// </summary>
    GS = 239,

    /// <summary>
    /// Guatemala
    /// GT = 320
    /// </summary>
    GT = 320,

    /// <summary>
    /// Guam
    /// GU = 316
    /// </summary>
    GU = 316,

    /// <summary>
    /// Guinea-Bissau
    /// GW = 624
    /// </summary>
    GW = 624,

    /// <summary>
    /// Guyana
    /// GY = 328
    /// </summary>
    GY = 328,

    /// <summary>
    /// Hong Kong
    /// HK = 344
    /// </summary>
    HK = 344,

    /// <summary>
    /// Heard Island and McDonald Islands
    /// HM = 334
    /// </summary>
    HM = 334,

    /// <summary>
    /// Honduras
    /// HN = 340
    /// </summary>
    HN = 340,

    /// <summary>
    /// Croatia
    /// HR = 191
    /// </summary>
    HR = 191,

    /// <summary>
    /// Haiti
    /// HT = 332
    /// </summary>
    HT = 332,

    /// <summary>
    /// Hungary
    /// HU = 348
    /// </summary>
    HU = 348,

    /// <summary>
    /// Indonesia
    /// ID = 360
    /// </summary>
    ID = 360,

    /// <summary>
    /// Ireland
    /// IE = 372
    /// </summary>
    IE = 372,

    /// <summary>
    /// Israel
    /// IL = 376
    /// </summary>
    IL = 376,

    /// <summary>
    /// Isle of Man
    /// IM = 833
    /// </summary>
    IM = 833,

    /// <summary>
    /// India
    /// IN = 356
    /// </summary>
    IN_ = 356,

    /// <summary>
    /// British Indian Ocean Territory (the)
    /// IO = 86
    /// </summary>
    IO = 86,

    /// <summary>
    /// Iraq
    /// IQ = 368
    /// </summary>
    IQ = 368,

    /// <summary>
    /// Iran (Islamic Republic of)
    /// IR = 364
    /// </summary>
    IR = 364,

    /// <summary>
    /// Iceland
    /// IS = 352
    /// </summary>
    IS_ = 352,

    /// <summary>
    /// Italy
    /// IT = 380
    /// </summary>
    IT = 380,

    /// <summary>
    /// Jersey
    /// JE = 832
    /// </summary>
    JE = 832,

    /// <summary>
    /// Jamaica
    /// JM = 388
    /// </summary>
    JM = 388,

    /// <summary>
    /// Jordan
    /// JO = 400
    /// </summary>
    JO = 400,

    /// <summary>
    /// Japan
    /// JP = 392
    /// </summary>
    JP = 392,

    /// <summary>
    /// Kenya
    /// KE = 404
    /// </summary>
    KE = 404,

    /// <summary>
    /// Kyrgyzstan
    /// KG = 417
    /// </summary>
    KG = 417,

    /// <summary>
    /// Cambodia
    /// KH = 116
    /// </summary>
    KH = 116,

    /// <summary>
    /// Kiribati
    /// KI = 296
    /// </summary>
    KI = 296,

    /// <summary>
    /// Comoros (the)
    /// KM = 174
    /// </summary>
    KM = 174,

    /// <summary>
    /// Saint Kitts and Nevis
    /// KN = 659
    /// </summary>
    KN = 659,

    /// <summary>
    /// Korea (the Democratic People's Republic of)
    /// KP = 408
    /// </summary>
    KP = 408,

    /// <summary>
    /// Korea (the Republic of)
    /// KR = 410
    /// </summary>
    KR = 410,

    /// <summary>
    /// Kuwait
    /// KW = 414
    /// </summary>
    KW = 414,

    /// <summary>
    /// Cayman Islands (the)
    /// KY = 136
    /// </summary>
    KY = 136,

    /// <summary>
    /// Kazakhstan
    /// KZ = 398
    /// </summary>
    KZ = 398,

    /// <summary>
    /// Lao People's Democratic Republic (the)
    /// LA = 418
    /// </summary>
    LA = 418,

    /// <summary>
    /// Lebanon
    /// LB = 422
    /// </summary>
    LB = 422,

    /// <summary>
    /// Saint Lucia
    /// LC = 662
    /// </summary>
    LC = 662,

    /// <summary>
    /// Liechtenstein
    /// LI = 438
    /// </summary>
    LI = 438,

    /// <summary>
    /// Sri Lanka
    /// LK = 144
    /// </summary>
    LK = 144,

    /// <summary>
    /// Liberia
    /// LR = 430
    /// </summary>
    LR = 430,

    /// <summary>
    /// Lesotho
    /// LS = 426
    /// </summary>
    LS = 426,

    /// <summary>
    /// Lithuania
    /// LT = 440
    /// </summary>
    LT = 440,

    /// <summary>
    /// Luxembourg
    /// LU = 442
    /// </summary>
    LU = 442,

    /// <summary>
    /// Latvia
    /// LV = 428
    /// </summary>
    LV = 428,

    /// <summary>
    /// Libya
    /// LY = 434
    /// </summary>
    LY = 434,

    /// <summary>
    /// Morocco
    /// MA = 504
    /// </summary>
    MA = 504,

    /// <summary>
    /// Monaco
    /// MC = 492
    /// </summary>
    MC = 492,

    /// <summary>
    /// Moldova (the Republic of)
    /// MD = 498
    /// </summary>
    MD = 498,

    /// <summary>
    /// Montenegro
    /// ME = 499
    /// </summary>
    ME = 499,

    /// <summary>
    /// Saint Martin (French part)
    /// MF = 663
    /// </summary>
    MF = 663,

    /// <summary>
    /// Madagascar
    /// MG = 450
    /// </summary>
    MG = 450,

    /// <summary>
    /// Marshall Islands (the)
    /// MH = 584
    /// </summary>
    MH = 584,

    /// <summary>
    /// North Macedonia
    /// MK = 807
    /// </summary>
    MK = 807,

    /// <summary>
    /// Mali
    /// ML = 466
    /// </summary>
    ML = 466,

    /// <summary>
    /// Myanmar
    /// MM = 104
    /// </summary>
    MM = 104,

    /// <summary>
    /// Mongolia
    /// MN = 496
    /// </summary>
    MN = 496,

    /// <summary>
    /// Macao
    /// MO = 446
    /// </summary>
    MO = 446,

    /// <summary>
    /// Northern Mariana Islands (the)
    /// MP = 580
    /// </summary>
    MP = 580,

    /// <summary>
    /// Martinique
    /// MQ = 474
    /// </summary>
    MQ = 474,

    /// <summary>
    /// Mauritania
    /// MR = 478
    /// </summary>
    MR = 478,

    /// <summary>
    /// Montserrat
    /// MS = 500
    /// </summary>
    MS = 500,

    /// <summary>
    /// Malta
    /// MT = 470
    /// </summary>
    MT = 470,

    /// <summary>
    /// Mauritius
    /// MU = 480
    /// </summary>
    MU = 480,

    /// <summary>
    /// Maldives
    /// MV = 462
    /// </summary>
    MV = 462,

    /// <summary>
    /// Malawi
    /// MW = 454
    /// </summary>
    MW = 454,

    /// <summary>
    /// Malaysia
    /// MY = 458
    /// </summary>
    MX = 484,

    /// <summary>
    /// Malaysia
    /// MY = 458
    /// </summary>
    MY = 458,

    /// <summary>
    /// Mozambique
    /// MZ = 508
    /// </summary>
    MZ = 508,

    /// <summary>
    /// Namibia
    /// NA = 516
    /// </summary>
    NA = 516,

    /// <summary>
    /// New Caledonia
    /// NC = 540
    /// </summary>
    NC = 540,

    /// <summary>
    /// Niger (the)
    /// NE = 562
    /// </summary>
    NE = 562,

    /// <summary>
    /// Norfolk Island
    /// NF = 574
    /// </summary>
    NF = 574,

    /// <summary>
    /// Nigeria
    /// NG = 566
    /// </summary>
    NG = 566,

    /// <summary>
    /// Nicaragua
    /// NI = 558
    /// </summary>
    NI = 558,

    /// <summary>
    /// Netherlands (the)
    /// NL = 528
    /// </summary>
    NL = 528,

    /// <summary>
    /// Norway
    /// NO = 578
    /// </summary>
    NO = 578,

    /// <summary>
    /// Nepal
    /// NP = 524
    /// </summary>
    NP = 524,

    /// <summary>
    /// Nauru
    /// NR = 520
    /// </summary>
    NR = 520,

    /// <summary>
    /// Niue
    /// NU = 570
    /// </summary>
    NU = 570,

    /// <summary>
    /// New Zealand
    /// NZ = 554
    /// </summary>
    NZ = 554,

    /// <summary>
    /// Oman
    /// OM = 512
    /// </summary>
    OM = 512,

    /// <summary>
    /// Panama
    /// PA = 591
    /// </summary>
    PA = 591,

    /// <summary>
    /// Peru
    /// PE = 604
    /// </summary>
    PE = 604,

    /// <summary>
    /// French Polynesia
    /// PF = 258
    /// </summary>
    PF = 258,

    /// <summary>
    /// Papua New Guinea
    /// PG = 598
    /// </summary>
    PG = 598,

    /// <summary>
    /// Philippines (the)
    /// PH = 608
    /// </summary>
    PH = 608,

    /// <summary>
    /// Pakistan
    /// PK = 586
    /// </summary>
    PK = 586,

    /// <summary>
    /// Poland
    /// PL = 616
    /// </summary>
    PL = 616,

    /// <summary>
    /// Saint Pierre and Miquelon
    /// PM = 666
    /// </summary>
    PM = 666,

    /// <summary>
    /// Pitcairn
    /// PN = 612
    /// </summary>
    PN = 612,

    /// <summary>
    /// Puerto Rico
    /// PR = 630
    /// </summary>
    PR = 630,

    /// <summary>
    /// Palestine, State of
    /// PS = 275
    /// </summary>
    PS = 275,

    /// <summary>
    /// Portugal
    /// PT = 620
    /// </summary>
    PT = 620,

    /// <summary>
    /// Palau
    /// PW = 585
    /// </summary>
    PW = 585,

    /// <summary>
    /// Paraguay
    /// PY = 600
    /// </summary>
    PY = 600,

    /// <summary>
    /// Qatar
    /// QA = 634
    /// </summary>
    QA = 634,

    /// <summary>
    /// Réunio
    /// RE = 638
    /// </summary>
    RE = 638,

    /// <summary>
    /// Romania
    /// RO = 642
    /// </summary>
    RO = 642,

    /// <summary>
    /// Serbia
    /// RS = 688
    /// </summary>
    RS = 688,

    /// <summary>
    /// Russian Federation (the)
    /// RU = 643
    /// </summary>
    RU = 643,

    /// <summary>
    /// Rwanda
    /// RW = 646
    /// </summary>
    RW = 646,

    /// <summary>
    /// Saudi Arabia
    /// SA = 682
    /// </summary>
    SA = 682,

    /// <summary>
    /// Solomon Islands
    /// SB = 90
    /// </summary>
    SB = 90,

    /// <summary>
    /// Seychelles
    /// SC = 690
    /// </summary>
    SC = 690,

    /// <summary>
    /// Sudan (the)
    /// SD = 729
    /// </summary>
    SD = 729,

    /// <summary>
    /// Sweden
    /// SE = 752
    /// </summary>
    SE = 752,

    /// <summary>
    /// Singapore
    /// SG = 702
    /// </summary>
    SG = 702,

    /// <summary>
    /// Saint Helena, Ascension and Tristan da Cunha
    /// SH = 654
    /// </summary>
    SH = 654,

    /// <summary>
    /// Slovenia
    /// SI = 705
    /// </summary>
    SI = 705,

    /// <summary>
    /// Svalbard and Jan Mayen
    /// SJ = 744
    /// </summary>
    SJ = 744,

    /// <summary>
    /// Slovakia
    /// SK = 703
    /// </summary>
    SK = 703,

    /// <summary>
    /// Sierra Leone
    /// SL = 694
    /// </summary>
    SL = 694,

    /// <summary>
    /// San Marino
    /// SM = 674
    /// </summary>
    SM = 674,

    /// <summary>
    /// Senegal
    /// SN = 686
    /// </summary>
    SN = 686,

    /// <summary>
    /// Somalia
    /// SO = 706
    /// </summary>
    SO = 706,

    /// <summary>
    /// Suriname
    /// SR = 740
    /// </summary>
    SR = 740,

    /// <summary>
    /// South Sudan
    /// SS = 728
    /// </summary>
    SS = 728,

    /// <summary>
    /// Sao Tome and Principe
    /// ST = 678
    /// </summary>
    ST = 678,

    /// <summary>
    /// El Salvador
    /// SV = 222
    /// </summary>
    SV = 222,

    /// <summary>
    /// Sint Maarten (Dutch part)
    /// SX = 534
    /// </summary>
    SX = 534,

    /// <summary>
    /// Syrian Arab Republic (the)
    /// SY = 760
    /// </summary>
    SY = 760,

    /// <summary>
    /// Eswatini
    /// SZ = 748
    /// </summary>
    SZ = 748,

    /// <summary>
    /// Turks and Caicos Islands (the)
    /// TC = 796
    /// </summary>
    TC = 796,

    /// <summary>
    /// Chad
    /// TD = 148
    /// </summary>
    TD = 148,

    /// <summary>
    /// French Southern Territories (the)
    /// TF = 260
    /// </summary>
    TF = 260,

    /// <summary>
    /// Togo
    /// TG = 768
    /// </summary>
    TG = 768,

    /// <summary>
    /// Thailand
    /// TH = 764
    /// </summary>
    TH = 764,

    /// <summary>
    /// Tajikistan
    /// TJ = 762
    /// </summary>
    TJ = 762,

    /// <summary>
    /// Tokelau
    /// TK = 772
    /// </summary>
    TK = 772,

    /// <summary>
    /// Timor-Leste
    /// TL = 626
    /// </summary>
    TL = 626,

    /// <summary>
    /// Turkmenistan
    /// TM = 795
    /// </summary>
    TM = 795,

    /// <summary>
    /// Tunisia
    /// TN = 788
    /// </summary>
    TN = 788,

    /// <summary>
    /// Tonga
    /// TO = 776
    /// </summary>
    TO_ = 776,

    /// <summary>
    /// Turkey
    /// TR = 792
    /// </summary>
    TR = 792,

    /// <summary>
    /// Trinidad and Tobago
    /// TT = 780
    /// </summary>
    TT = 780,

    /// <summary>
    /// Tuvalu
    /// TV = 798
    /// </summary>
    TV = 798,

    /// <summary>
    /// Taiwan (Province of China)
    /// TW = 158
    /// </summary>
    TW = 158,

    /// <summary>
    /// Tanzania, the United Republic of
    /// TZ = 834
    /// </summary>
    TZ = 834,

    /// <summary>
    /// Ukraine
    /// UA = 804
    /// </summary>
    UA = 804,

    /// <summary>
    /// Uganda
    /// UG = 800
    /// </summary>
    UG = 800,

    /// <summary>
    /// United States Minor Outlying Islands (the)
    /// UM = 581
    /// </summary>
    UM = 581,

    /// <summary>
    /// United States of America (the)
    /// US = 840
    /// </summary>
    US = 840,

    /// <summary>
    /// Uruguay
    /// UY = 858
    /// </summary>
    UY = 858,

    /// <summary>
    /// Uzbekistan
    /// UZ = 860
    /// </summary>
    UZ = 860,

    /// <summary>
    /// Holy See (the)
    /// VA = 336
    /// </summary>
    VA = 336,

    /// <summary>
    /// Saint Vincent and the Grenadines
    /// VC = 670
    /// </summary>
    VC = 670,

    /// <summary>
    /// Venezuela (Bolivarian Republic of)
    /// VE = 862
    /// </summary>
    VE = 862,

    /// <summary>
    /// Virgin Islands (British)
    /// VG = 92
    /// </summary>
    VG = 92,

    /// <summary>
    /// Virgin Islands (U.S.)
    /// VI = 850
    /// </summary>
    VI = 850,

    /// <summary>
    /// Viet Nam
    /// VN = 704
    /// </summary>
    VN = 704,

    /// <summary>
    /// Vanuatu
    /// VU = 548
    /// </summary>
    VU = 548,

    /// <summary>
    /// Wallis and Futuna
    /// WF = 876
    /// </summary>
    WF = 876,

    /// <summary>
    /// Samoa
    /// WS = 882
    /// </summary>
    WS = 882,

    /// <summary>
    /// Yemen
    /// YE = 887
    /// </summary>
    YE = 887,

    /// <summary>
    /// Mayotte
    /// YT = 175
    /// </summary>
    YT = 175,

    /// <summary>
    /// South Africa
    /// ZA = 710
    /// </summary>
    ZA = 710,

    /// <summary>
    /// Zambia
    /// ZM = 894
    /// </summary>
    ZM = 894,

    /// <summary>
    /// Zimbabwe
    /// ZW = 716
    /// </summary>
    ZW = 716,

    /// <summary>
    /// Kosovo
    /// 1A = 999
    /// This is a temporary code,
    /// special treatment required as enums
    /// don't like members starting with a number!
    /// </summary>
    _1A = 999,

    /// <summary>
    /// Fall back for unsupported
    /// Country Codes
    /// Unknown = 0
    /// </summary>
    Unknown = 0
  );

  TZUGFeRDCountryCodesExtensions = class
  public
    class function FromString(const s: string): TZUGFeRDCountryCodes;
    class function EnumToString(codes: TZUGFeRDCountryCodes): string;
  end;

implementation

{ TZUGFeRDCountryCodesExtensions }

class function TZUGFeRDCountryCodesExtensions.EnumToString(
  codes: TZUGFeRDCountryCodes): string;
begin
  case codes of
    TZUGFeRDCountryCodes.AD : Result := 'AD';
    TZUGFeRDCountryCodes.AE : Result := 'AE';
    TZUGFeRDCountryCodes.AF : Result := 'AF';
    TZUGFeRDCountryCodes.AG : Result := 'AG';
    TZUGFeRDCountryCodes.AI : Result := 'AI';
    TZUGFeRDCountryCodes.AL : Result := 'AL';
    TZUGFeRDCountryCodes.AM : Result := 'AM';
    TZUGFeRDCountryCodes.AO : Result := 'AO';
    TZUGFeRDCountryCodes.AQ : Result := 'AQ';
    TZUGFeRDCountryCodes.AR : Result := 'AR';
    TZUGFeRDCountryCodes.AS_: Result := 'AS';
    TZUGFeRDCountryCodes.AT : Result := 'AT';
    TZUGFeRDCountryCodes.AU : Result := 'AU';
    TZUGFeRDCountryCodes.AW : Result := 'AW';
    TZUGFeRDCountryCodes.AX : Result := 'AX';
    TZUGFeRDCountryCodes.AZ : Result := 'AZ';
    TZUGFeRDCountryCodes.BA : Result := 'BA';
    TZUGFeRDCountryCodes.BB : Result := 'BB';
    TZUGFeRDCountryCodes.BD : Result := 'BD';
    TZUGFeRDCountryCodes.BE : Result := 'BE';
    TZUGFeRDCountryCodes.BF : Result := 'BF';
    TZUGFeRDCountryCodes.BG : Result := 'BG';
    TZUGFeRDCountryCodes.BH : Result := 'BH';
    TZUGFeRDCountryCodes.BI : Result := 'BI';
    TZUGFeRDCountryCodes.BJ : Result := 'BJ';
    TZUGFeRDCountryCodes.BL : Result := 'BL';
    TZUGFeRDCountryCodes.BM : Result := 'BM';
    TZUGFeRDCountryCodes.BN : Result := 'BN';
    TZUGFeRDCountryCodes.BO : Result := 'BO';
    TZUGFeRDCountryCodes.BQ : Result := 'BQ';
    TZUGFeRDCountryCodes.BR : Result := 'BR';
    TZUGFeRDCountryCodes.BS : Result := 'BS';
    TZUGFeRDCountryCodes.BT : Result := 'BT';
    TZUGFeRDCountryCodes.BV : Result := 'BV';
    TZUGFeRDCountryCodes.BW : Result := 'BW';
    TZUGFeRDCountryCodes.BY : Result := 'BY';
    TZUGFeRDCountryCodes.BZ : Result := 'BZ';
    TZUGFeRDCountryCodes.CA : Result := 'CA';
    TZUGFeRDCountryCodes.CC : Result := 'CC';
    TZUGFeRDCountryCodes.CD : Result := 'CD';
    TZUGFeRDCountryCodes.CF : Result := 'CF';
    TZUGFeRDCountryCodes.CG : Result := 'CG';
    TZUGFeRDCountryCodes.CH : Result := 'CH';
    TZUGFeRDCountryCodes.CI : Result := 'CI';
    TZUGFeRDCountryCodes.CK : Result := 'CK';
    TZUGFeRDCountryCodes.CL : Result := 'CL';
    TZUGFeRDCountryCodes.CM : Result := 'CM';
    TZUGFeRDCountryCodes.CN : Result := 'CN';
    TZUGFeRDCountryCodes.CO : Result := 'CO';
    TZUGFeRDCountryCodes.CR : Result := 'CR';
    TZUGFeRDCountryCodes.CU : Result := 'CU';
    TZUGFeRDCountryCodes.CV : Result := 'CV';
    TZUGFeRDCountryCodes.CW : Result := 'CW';
    TZUGFeRDCountryCodes.CX : Result := 'CX';
    TZUGFeRDCountryCodes.CY : Result := 'CY';
    TZUGFeRDCountryCodes.CZ : Result := 'CZ';
    TZUGFeRDCountryCodes.DE : Result := 'DE';
    TZUGFeRDCountryCodes.DJ : Result := 'DJ';
    TZUGFeRDCountryCodes.DK : Result := 'DK';
    TZUGFeRDCountryCodes.DM : Result := 'DM';
    TZUGFeRDCountryCodes.DO_: Result := 'DO';
    TZUGFeRDCountryCodes.DZ : Result := 'DZ';
    TZUGFeRDCountryCodes.EC : Result := 'EC';
    TZUGFeRDCountryCodes.EE : Result := 'EE';
    TZUGFeRDCountryCodes.EG : Result := 'EG';
    TZUGFeRDCountryCodes.EH : Result := 'EH';
    TZUGFeRDCountryCodes.ER : Result := 'ER';
    TZUGFeRDCountryCodes.ES : Result := 'ES';
    TZUGFeRDCountryCodes.ET : Result := 'ET';
    TZUGFeRDCountryCodes.FI : Result := 'FI';
    TZUGFeRDCountryCodes.FJ : Result := 'FJ';
    TZUGFeRDCountryCodes.FK : Result := 'FK';
    TZUGFeRDCountryCodes.FM : Result := 'FM';
    TZUGFeRDCountryCodes.FO : Result := 'FO';
    TZUGFeRDCountryCodes.FR : Result := 'FR';
    TZUGFeRDCountryCodes.GA : Result := 'GA';
    TZUGFeRDCountryCodes.GB : Result := 'GB';
    TZUGFeRDCountryCodes.GD : Result := 'GD';
    TZUGFeRDCountryCodes.GE : Result := 'GE';
    TZUGFeRDCountryCodes.GF : Result := 'GF';
    TZUGFeRDCountryCodes.GG : Result := 'GG';
    TZUGFeRDCountryCodes.GH : Result := 'GH';
    TZUGFeRDCountryCodes.GI : Result := 'GI';
    TZUGFeRDCountryCodes.GL : Result := 'GL';
    TZUGFeRDCountryCodes.GM : Result := 'GM';
    TZUGFeRDCountryCodes.GN : Result := 'GN';
    TZUGFeRDCountryCodes.GP : Result := 'GP';
    TZUGFeRDCountryCodes.GQ : Result := 'GQ';
    TZUGFeRDCountryCodes.GR : Result := 'GR';
    TZUGFeRDCountryCodes.GS : Result := 'GS';
    TZUGFeRDCountryCodes.GT : Result := 'GT';
    TZUGFeRDCountryCodes.GU : Result := 'GU';
    TZUGFeRDCountryCodes.GW : Result := 'GW';
    TZUGFeRDCountryCodes.GY : Result := 'GY';
    TZUGFeRDCountryCodes.HK : Result := 'HK';
    TZUGFeRDCountryCodes.HM : Result := 'HM';
    TZUGFeRDCountryCodes.HN : Result := 'HN';
    TZUGFeRDCountryCodes.HR : Result := 'HR';
    TZUGFeRDCountryCodes.HT : Result := 'HT';
    TZUGFeRDCountryCodes.HU : Result := 'HU';
    TZUGFeRDCountryCodes.ID : Result := 'ID';
    TZUGFeRDCountryCodes.IE : Result := 'IE';
    TZUGFeRDCountryCodes.IL : Result := 'IL';
    TZUGFeRDCountryCodes.IM : Result := 'IM';
    TZUGFeRDCountryCodes.IN_: Result := 'IN';
    TZUGFeRDCountryCodes.IO : Result := 'IO';
    TZUGFeRDCountryCodes.IQ : Result := 'IQ';
    TZUGFeRDCountryCodes.IR : Result := 'IR';
    TZUGFeRDCountryCodes.IS_: Result := 'IS';
    TZUGFeRDCountryCodes.IT : Result := 'IT';
    TZUGFeRDCountryCodes.JE : Result := 'JE';
    TZUGFeRDCountryCodes.JM : Result := 'JM';
    TZUGFeRDCountryCodes.JO : Result := 'JO';
    TZUGFeRDCountryCodes.JP : Result := 'JP';
    TZUGFeRDCountryCodes.KE : Result := 'KE';
    TZUGFeRDCountryCodes.KG : Result := 'KG';
    TZUGFeRDCountryCodes.KH : Result := 'KH';
    TZUGFeRDCountryCodes.KI : Result := 'KI';
    TZUGFeRDCountryCodes.KM : Result := 'KM';
    TZUGFeRDCountryCodes.KN : Result := 'KN';
    TZUGFeRDCountryCodes.KP : Result := 'KP';
    TZUGFeRDCountryCodes.KR : Result := 'KR';
    TZUGFeRDCountryCodes.KW : Result := 'KW';
    TZUGFeRDCountryCodes.KY : Result := 'KY';
    TZUGFeRDCountryCodes.KZ : Result := 'KZ';
    TZUGFeRDCountryCodes.LA : Result := 'LA';
    TZUGFeRDCountryCodes.LB : Result := 'LB';
    TZUGFeRDCountryCodes.LC : Result := 'LC';
    TZUGFeRDCountryCodes.LI : Result := 'LI';
    TZUGFeRDCountryCodes.LK : Result := 'LK';
    TZUGFeRDCountryCodes.LR : Result := 'LR';
    TZUGFeRDCountryCodes.LS : Result := 'LS';
    TZUGFeRDCountryCodes.LT : Result := 'LT';
    TZUGFeRDCountryCodes.LU : Result := 'LU';
    TZUGFeRDCountryCodes.LV : Result := 'LV';
    TZUGFeRDCountryCodes.LY : Result := 'LY';
    TZUGFeRDCountryCodes.MA : Result := 'MA';
    TZUGFeRDCountryCodes.MC : Result := 'MC';
    TZUGFeRDCountryCodes.MD : Result := 'MD';
    TZUGFeRDCountryCodes.ME : Result := 'ME';
    TZUGFeRDCountryCodes.MF : Result := 'MF';
    TZUGFeRDCountryCodes.MG : Result := 'MG';
    TZUGFeRDCountryCodes.MH : Result := 'MH';
    TZUGFeRDCountryCodes.MK : Result := 'MK';
    TZUGFeRDCountryCodes.ML : Result := 'ML';
    TZUGFeRDCountryCodes.MM : Result := 'MM';
    TZUGFeRDCountryCodes.MN : Result := 'MN';
    TZUGFeRDCountryCodes.MO : Result := 'MO';
    TZUGFeRDCountryCodes.MP : Result := 'MP';
    TZUGFeRDCountryCodes.MQ : Result := 'MQ';
    TZUGFeRDCountryCodes.MR : Result := 'MR';
    TZUGFeRDCountryCodes.MS : Result := 'MS';
    TZUGFeRDCountryCodes.MT : Result := 'MT';
    TZUGFeRDCountryCodes.MU : Result := 'MU';
    TZUGFeRDCountryCodes.MV : Result := 'MV';
    TZUGFeRDCountryCodes.MW : Result := 'MW';
    TZUGFeRDCountryCodes.MX : Result := 'MX';
    TZUGFeRDCountryCodes.MY : Result := 'MY';
    TZUGFeRDCountryCodes.MZ : Result := 'MZ';
    TZUGFeRDCountryCodes.NA : Result := 'NA';
    TZUGFeRDCountryCodes.NC : Result := 'NC';
    TZUGFeRDCountryCodes.NE : Result := 'NE';
    TZUGFeRDCountryCodes.NF : Result := 'NF';
    TZUGFeRDCountryCodes.NG : Result := 'NG';
    TZUGFeRDCountryCodes.NI : Result := 'NI';
    TZUGFeRDCountryCodes.NL : Result := 'NL';
    TZUGFeRDCountryCodes.NO : Result := 'NO';
    TZUGFeRDCountryCodes.NP : Result := 'NP';
    TZUGFeRDCountryCodes.NR : Result := 'NR';
    TZUGFeRDCountryCodes.NU : Result := 'NU';
    TZUGFeRDCountryCodes.NZ : Result := 'NZ';
    TZUGFeRDCountryCodes.OM : Result := 'OM';
    TZUGFeRDCountryCodes.PA : Result := 'PA';
    TZUGFeRDCountryCodes.PE : Result := 'PE';
    TZUGFeRDCountryCodes.PF : Result := 'PF';
    TZUGFeRDCountryCodes.PG : Result := 'PG';
    TZUGFeRDCountryCodes.PH : Result := 'PH';
    TZUGFeRDCountryCodes.PK : Result := 'PK';
    TZUGFeRDCountryCodes.PL : Result := 'PL';
    TZUGFeRDCountryCodes.PM : Result := 'PM';
    TZUGFeRDCountryCodes.PN : Result := 'PN';
    TZUGFeRDCountryCodes.PR : Result := 'PR';
    TZUGFeRDCountryCodes.PS : Result := 'PS';
    TZUGFeRDCountryCodes.PT : Result := 'PT';
    TZUGFeRDCountryCodes.PW : Result := 'PW';
    TZUGFeRDCountryCodes.PY : Result := 'PY';
    TZUGFeRDCountryCodes.QA : Result := 'QA';
    TZUGFeRDCountryCodes.RE : Result := 'RE';
    TZUGFeRDCountryCodes.RO : Result := 'RO';
    TZUGFeRDCountryCodes.RS : Result := 'RS';
    TZUGFeRDCountryCodes.RU : Result := 'RU';
    TZUGFeRDCountryCodes.RW : Result := 'RW';
    TZUGFeRDCountryCodes.SA : Result := 'SA';
    TZUGFeRDCountryCodes.SB : Result := 'SB';
    TZUGFeRDCountryCodes.SC : Result := 'SC';
    TZUGFeRDCountryCodes.SD : Result := 'SD';
    TZUGFeRDCountryCodes.SE : Result := 'SE';
    TZUGFeRDCountryCodes.SG : Result := 'SG';
    TZUGFeRDCountryCodes.SH : Result := 'SH';
    TZUGFeRDCountryCodes.SI : Result := 'SI';
    TZUGFeRDCountryCodes.SJ : Result := 'SJ';
    TZUGFeRDCountryCodes.SK : Result := 'SK';
    TZUGFeRDCountryCodes.SL : Result := 'SL';
    TZUGFeRDCountryCodes.SM : Result := 'SM';
    TZUGFeRDCountryCodes.SN : Result := 'SN';
    TZUGFeRDCountryCodes.SO : Result := 'SO';
    TZUGFeRDCountryCodes.SR : Result := 'SR';
    TZUGFeRDCountryCodes.SS : Result := 'SS';
    TZUGFeRDCountryCodes.ST : Result := 'ST';
    TZUGFeRDCountryCodes.SV : Result := 'SV';
    TZUGFeRDCountryCodes.SX : Result := 'SX';
    TZUGFeRDCountryCodes.SY : Result := 'SY';
    TZUGFeRDCountryCodes.SZ : Result := 'SZ';
    TZUGFeRDCountryCodes.TC : Result := 'TC';
    TZUGFeRDCountryCodes.TD : Result := 'TD';
    TZUGFeRDCountryCodes.TF : Result := 'TF';
    TZUGFeRDCountryCodes.TG : Result := 'TG';
    TZUGFeRDCountryCodes.TH : Result := 'TH';
    TZUGFeRDCountryCodes.TJ : Result := 'TJ';
    TZUGFeRDCountryCodes.TK : Result := 'TK';
    TZUGFeRDCountryCodes.TL : Result := 'TL';
    TZUGFeRDCountryCodes.TM : Result := 'TM';
    TZUGFeRDCountryCodes.TN : Result := 'TN';
    TZUGFeRDCountryCodes.TO_: Result := 'TO';
    TZUGFeRDCountryCodes.TR : Result := 'TR';
    TZUGFeRDCountryCodes.TT : Result := 'TT';
    TZUGFeRDCountryCodes.TV : Result := 'TV';
    TZUGFeRDCountryCodes.TW : Result := 'TW';
    TZUGFeRDCountryCodes.TZ : Result := 'TZ';
    TZUGFeRDCountryCodes.UA : Result := 'UA';
    TZUGFeRDCountryCodes.UG : Result := 'UG';
    TZUGFeRDCountryCodes.UM : Result := 'UM';
    TZUGFeRDCountryCodes.US : Result := 'US';
    TZUGFeRDCountryCodes.UY : Result := 'UY';
    TZUGFeRDCountryCodes.UZ : Result := 'UZ';
    TZUGFeRDCountryCodes.VA : Result := 'VA';
    TZUGFeRDCountryCodes.VC : Result := 'VC';
    TZUGFeRDCountryCodes.VE : Result := 'VE';
    TZUGFeRDCountryCodes.VG : Result := 'VG';
    TZUGFeRDCountryCodes.VI : Result := 'VI';
    TZUGFeRDCountryCodes.VN : Result := 'VN';
    TZUGFeRDCountryCodes.VU : Result := 'VU';
    TZUGFeRDCountryCodes.WF : Result := 'WF';
    TZUGFeRDCountryCodes.WS : Result := 'WS';
    TZUGFeRDCountryCodes.YE : Result := 'YE';
    TZUGFeRDCountryCodes.YT : Result := 'YT';
    TZUGFeRDCountryCodes.ZA : Result := 'ZA';
    TZUGFeRDCountryCodes.ZM : Result := 'ZM';
    TZUGFeRDCountryCodes.ZW : Result := 'ZW';
    TZUGFeRDCountryCodes._1A: Result := '1A';
    else Result := 'Unknown';
  end;
end;

class function TZUGFeRDCountryCodesExtensions.FromString(
  const s: string): TZUGFeRDCountryCodes;
begin
  if SameText(s,'AD') then Result := TZUGFeRDCountryCodes.AD else
  if SameText(s,'AE') then Result := TZUGFeRDCountryCodes.AE else
  if SameText(s,'AF') then Result := TZUGFeRDCountryCodes.AF else
  if SameText(s,'AG') then Result := TZUGFeRDCountryCodes.AG else
  if SameText(s,'AI') then Result := TZUGFeRDCountryCodes.AI else
  if SameText(s,'AL') then Result := TZUGFeRDCountryCodes.AL else
  if SameText(s,'AM') then Result := TZUGFeRDCountryCodes.AM else
  if SameText(s,'AO') then Result := TZUGFeRDCountryCodes.AO else
  if SameText(s,'AQ') then Result := TZUGFeRDCountryCodes.AQ else
  if SameText(s,'AR') then Result := TZUGFeRDCountryCodes.AR else
  if SameText(s,'AS') then Result := TZUGFeRDCountryCodes.AS_ else
  if SameText(s,'AT') then Result := TZUGFeRDCountryCodes.AT else
  if SameText(s,'AU') then Result := TZUGFeRDCountryCodes.AU else
  if SameText(s,'AW') then Result := TZUGFeRDCountryCodes.AW else
  if SameText(s,'AX') then Result := TZUGFeRDCountryCodes.AX else
  if SameText(s,'AZ') then Result := TZUGFeRDCountryCodes.AZ else
  if SameText(s,'BA') then Result := TZUGFeRDCountryCodes.BA else
  if SameText(s,'BB') then Result := TZUGFeRDCountryCodes.BB else
  if SameText(s,'BD') then Result := TZUGFeRDCountryCodes.BD else
  if SameText(s,'BE') then Result := TZUGFeRDCountryCodes.BE else
  if SameText(s,'BF') then Result := TZUGFeRDCountryCodes.BF else
  if SameText(s,'BG') then Result := TZUGFeRDCountryCodes.BG else
  if SameText(s,'BH') then Result := TZUGFeRDCountryCodes.BH else
  if SameText(s,'BI') then Result := TZUGFeRDCountryCodes.BI else
  if SameText(s,'BJ') then Result := TZUGFeRDCountryCodes.BJ else
  if SameText(s,'BL') then Result := TZUGFeRDCountryCodes.BL else
  if SameText(s,'BM') then Result := TZUGFeRDCountryCodes.BM else
  if SameText(s,'BN') then Result := TZUGFeRDCountryCodes.BN else
  if SameText(s,'BO') then Result := TZUGFeRDCountryCodes.BO else
  if SameText(s,'BQ') then Result := TZUGFeRDCountryCodes.BQ else
  if SameText(s,'BR') then Result := TZUGFeRDCountryCodes.BR else
  if SameText(s,'BS') then Result := TZUGFeRDCountryCodes.BS else
  if SameText(s,'BT') then Result := TZUGFeRDCountryCodes.BT else
  if SameText(s,'BV') then Result := TZUGFeRDCountryCodes.BV else
  if SameText(s,'BW') then Result := TZUGFeRDCountryCodes.BW else
  if SameText(s,'BY') then Result := TZUGFeRDCountryCodes.BY else
  if SameText(s,'BZ') then Result := TZUGFeRDCountryCodes.BZ else
  if SameText(s,'CA') then Result := TZUGFeRDCountryCodes.CA else
  if SameText(s,'CC') then Result := TZUGFeRDCountryCodes.CC else
  if SameText(s,'CD') then Result := TZUGFeRDCountryCodes.CD else
  if SameText(s,'CF') then Result := TZUGFeRDCountryCodes.CF else
  if SameText(s,'CG') then Result := TZUGFeRDCountryCodes.CG else
  if SameText(s,'CH') then Result := TZUGFeRDCountryCodes.CH else
  if SameText(s,'CI') then Result := TZUGFeRDCountryCodes.CI else
  if SameText(s,'CK') then Result := TZUGFeRDCountryCodes.CK else
  if SameText(s,'CL') then Result := TZUGFeRDCountryCodes.CL else
  if SameText(s,'CM') then Result := TZUGFeRDCountryCodes.CM else
  if SameText(s,'CN') then Result := TZUGFeRDCountryCodes.CN else
  if SameText(s,'CO') then Result := TZUGFeRDCountryCodes.CO else
  if SameText(s,'CR') then Result := TZUGFeRDCountryCodes.CR else
  if SameText(s,'CU') then Result := TZUGFeRDCountryCodes.CU else
  if SameText(s,'CV') then Result := TZUGFeRDCountryCodes.CV else
  if SameText(s,'CW') then Result := TZUGFeRDCountryCodes.CW else
  if SameText(s,'CX') then Result := TZUGFeRDCountryCodes.CX else
  if SameText(s,'CY') then Result := TZUGFeRDCountryCodes.CY else
  if SameText(s,'CZ') then Result := TZUGFeRDCountryCodes.CZ else
  if SameText(s,'DE') then Result := TZUGFeRDCountryCodes.DE else
  if SameText(s,'DJ') then Result := TZUGFeRDCountryCodes.DJ else
  if SameText(s,'DK') then Result := TZUGFeRDCountryCodes.DK else
  if SameText(s,'DM') then Result := TZUGFeRDCountryCodes.DM else
  if SameText(s,'DO') then Result := TZUGFeRDCountryCodes.DO_ else
  if SameText(s,'DZ') then Result := TZUGFeRDCountryCodes.DZ else
  if SameText(s,'EC') then Result := TZUGFeRDCountryCodes.EC else
  if SameText(s,'EE') then Result := TZUGFeRDCountryCodes.EE else
  if SameText(s,'EG') then Result := TZUGFeRDCountryCodes.EG else
  if SameText(s,'EH') then Result := TZUGFeRDCountryCodes.EH else
  if SameText(s,'ER') then Result := TZUGFeRDCountryCodes.ER else
  if SameText(s,'ES') then Result := TZUGFeRDCountryCodes.ES else
  if SameText(s,'ET') then Result := TZUGFeRDCountryCodes.ET else
  if SameText(s,'FI') then Result := TZUGFeRDCountryCodes.FI else
  if SameText(s,'FJ') then Result := TZUGFeRDCountryCodes.FJ else
  if SameText(s,'FK') then Result := TZUGFeRDCountryCodes.FK else
  if SameText(s,'FM') then Result := TZUGFeRDCountryCodes.FM else
  if SameText(s,'FO') then Result := TZUGFeRDCountryCodes.FO else
  if SameText(s,'FR') then Result := TZUGFeRDCountryCodes.FR else
  if SameText(s,'GA') then Result := TZUGFeRDCountryCodes.GA else
  if SameText(s,'GB') then Result := TZUGFeRDCountryCodes.GB else
  if SameText(s,'GD') then Result := TZUGFeRDCountryCodes.GD else
  if SameText(s,'GE') then Result := TZUGFeRDCountryCodes.GE else
  if SameText(s,'GF') then Result := TZUGFeRDCountryCodes.GF else
  if SameText(s,'GG') then Result := TZUGFeRDCountryCodes.GG else
  if SameText(s,'GH') then Result := TZUGFeRDCountryCodes.GH else
  if SameText(s,'GI') then Result := TZUGFeRDCountryCodes.GI else
  if SameText(s,'GL') then Result := TZUGFeRDCountryCodes.GL else
  if SameText(s,'GM') then Result := TZUGFeRDCountryCodes.GM else
  if SameText(s,'GN') then Result := TZUGFeRDCountryCodes.GN else
  if SameText(s,'GP') then Result := TZUGFeRDCountryCodes.GP else
  if SameText(s,'GQ') then Result := TZUGFeRDCountryCodes.GQ else
  if SameText(s,'GR') then Result := TZUGFeRDCountryCodes.GR else
  if SameText(s,'GS') then Result := TZUGFeRDCountryCodes.GS else
  if SameText(s,'GT') then Result := TZUGFeRDCountryCodes.GT else
  if SameText(s,'GU') then Result := TZUGFeRDCountryCodes.GU else
  if SameText(s,'GW') then Result := TZUGFeRDCountryCodes.GW else
  if SameText(s,'GY') then Result := TZUGFeRDCountryCodes.GY else
  if SameText(s,'HK') then Result := TZUGFeRDCountryCodes.HK else
  if SameText(s,'HM') then Result := TZUGFeRDCountryCodes.HM else
  if SameText(s,'HN') then Result := TZUGFeRDCountryCodes.HN else
  if SameText(s,'HR') then Result := TZUGFeRDCountryCodes.HR else
  if SameText(s,'HT') then Result := TZUGFeRDCountryCodes.HT else
  if SameText(s,'HU') then Result := TZUGFeRDCountryCodes.HU else
  if SameText(s,'ID') then Result := TZUGFeRDCountryCodes.ID else
  if SameText(s,'IE') then Result := TZUGFeRDCountryCodes.IE else
  if SameText(s,'IL') then Result := TZUGFeRDCountryCodes.IL else
  if SameText(s,'IM') then Result := TZUGFeRDCountryCodes.IM else
  if SameText(s,'IN') then Result := TZUGFeRDCountryCodes.IN_ else
  if SameText(s,'IO') then Result := TZUGFeRDCountryCodes.IO else
  if SameText(s,'IQ') then Result := TZUGFeRDCountryCodes.IQ else
  if SameText(s,'IR') then Result := TZUGFeRDCountryCodes.IR else
  if SameText(s,'IS') then Result := TZUGFeRDCountryCodes.IS_ else
  if SameText(s,'IT') then Result := TZUGFeRDCountryCodes.IT else
  if SameText(s,'JE') then Result := TZUGFeRDCountryCodes.JE else
  if SameText(s,'JM') then Result := TZUGFeRDCountryCodes.JM else
  if SameText(s,'JO') then Result := TZUGFeRDCountryCodes.JO else
  if SameText(s,'JP') then Result := TZUGFeRDCountryCodes.JP else
  if SameText(s,'KE') then Result := TZUGFeRDCountryCodes.KE else
  if SameText(s,'KG') then Result := TZUGFeRDCountryCodes.KG else
  if SameText(s,'KH') then Result := TZUGFeRDCountryCodes.KH else
  if SameText(s,'KI') then Result := TZUGFeRDCountryCodes.KI else
  if SameText(s,'KM') then Result := TZUGFeRDCountryCodes.KM else
  if SameText(s,'KN') then Result := TZUGFeRDCountryCodes.KN else
  if SameText(s,'KP') then Result := TZUGFeRDCountryCodes.KP else
  if SameText(s,'KR') then Result := TZUGFeRDCountryCodes.KR else
  if SameText(s,'KW') then Result := TZUGFeRDCountryCodes.KW else
  if SameText(s,'KY') then Result := TZUGFeRDCountryCodes.KY else
  if SameText(s,'KZ') then Result := TZUGFeRDCountryCodes.KZ else
  if SameText(s,'LA') then Result := TZUGFeRDCountryCodes.LA else
  if SameText(s,'LB') then Result := TZUGFeRDCountryCodes.LB else
  if SameText(s,'LC') then Result := TZUGFeRDCountryCodes.LC else
  if SameText(s,'LI') then Result := TZUGFeRDCountryCodes.LI else
  if SameText(s,'LK') then Result := TZUGFeRDCountryCodes.LK else
  if SameText(s,'LR') then Result := TZUGFeRDCountryCodes.LR else
  if SameText(s,'LS') then Result := TZUGFeRDCountryCodes.LS else
  if SameText(s,'LT') then Result := TZUGFeRDCountryCodes.LT else
  if SameText(s,'LU') then Result := TZUGFeRDCountryCodes.LU else
  if SameText(s,'LV') then Result := TZUGFeRDCountryCodes.LV else
  if SameText(s,'LY') then Result := TZUGFeRDCountryCodes.LY else
  if SameText(s,'MA') then Result := TZUGFeRDCountryCodes.MA else
  if SameText(s,'MC') then Result := TZUGFeRDCountryCodes.MC else
  if SameText(s,'MD') then Result := TZUGFeRDCountryCodes.MD else
  if SameText(s,'ME') then Result := TZUGFeRDCountryCodes.ME else
  if SameText(s,'MF') then Result := TZUGFeRDCountryCodes.MF else
  if SameText(s,'MG') then Result := TZUGFeRDCountryCodes.MG else
  if SameText(s,'MH') then Result := TZUGFeRDCountryCodes.MH else
  if SameText(s,'MK') then Result := TZUGFeRDCountryCodes.MK else
  if SameText(s,'ML') then Result := TZUGFeRDCountryCodes.ML else
  if SameText(s,'MM') then Result := TZUGFeRDCountryCodes.MM else
  if SameText(s,'MN') then Result := TZUGFeRDCountryCodes.MN else
  if SameText(s,'MO') then Result := TZUGFeRDCountryCodes.MO else
  if SameText(s,'MP') then Result := TZUGFeRDCountryCodes.MP else
  if SameText(s,'MQ') then Result := TZUGFeRDCountryCodes.MQ else
  if SameText(s,'MR') then Result := TZUGFeRDCountryCodes.MR else
  if SameText(s,'MS') then Result := TZUGFeRDCountryCodes.MS else
  if SameText(s,'MT') then Result := TZUGFeRDCountryCodes.MT else
  if SameText(s,'MU') then Result := TZUGFeRDCountryCodes.MU else
  if SameText(s,'MV') then Result := TZUGFeRDCountryCodes.MV else
  if SameText(s,'MW') then Result := TZUGFeRDCountryCodes.MW else
  if SameText(s,'MX') then Result := TZUGFeRDCountryCodes.MX else
  if SameText(s,'MY') then Result := TZUGFeRDCountryCodes.MY else
  if SameText(s,'MZ') then Result := TZUGFeRDCountryCodes.MZ else
  if SameText(s,'NA') then Result := TZUGFeRDCountryCodes.NA else
  if SameText(s,'NC') then Result := TZUGFeRDCountryCodes.NC else
  if SameText(s,'NE') then Result := TZUGFeRDCountryCodes.NE else
  if SameText(s,'NF') then Result := TZUGFeRDCountryCodes.NF else
  if SameText(s,'NG') then Result := TZUGFeRDCountryCodes.NG else
  if SameText(s,'NI') then Result := TZUGFeRDCountryCodes.NI else
  if SameText(s,'NL') then Result := TZUGFeRDCountryCodes.NL else
  if SameText(s,'NO') then Result := TZUGFeRDCountryCodes.NO else
  if SameText(s,'NP') then Result := TZUGFeRDCountryCodes.NP else
  if SameText(s,'NR') then Result := TZUGFeRDCountryCodes.NR else
  if SameText(s,'NU') then Result := TZUGFeRDCountryCodes.NU else
  if SameText(s,'NZ') then Result := TZUGFeRDCountryCodes.NZ else
  if SameText(s,'OM') then Result := TZUGFeRDCountryCodes.OM else
  if SameText(s,'PA') then Result := TZUGFeRDCountryCodes.PA else
  if SameText(s,'PE') then Result := TZUGFeRDCountryCodes.PE else
  if SameText(s,'PF') then Result := TZUGFeRDCountryCodes.PF else
  if SameText(s,'PG') then Result := TZUGFeRDCountryCodes.PG else
  if SameText(s,'PH') then Result := TZUGFeRDCountryCodes.PH else
  if SameText(s,'PK') then Result := TZUGFeRDCountryCodes.PK else
  if SameText(s,'PL') then Result := TZUGFeRDCountryCodes.PL else
  if SameText(s,'PM') then Result := TZUGFeRDCountryCodes.PM else
  if SameText(s,'PN') then Result := TZUGFeRDCountryCodes.PN else
  if SameText(s,'PR') then Result := TZUGFeRDCountryCodes.PR else
  if SameText(s,'PS') then Result := TZUGFeRDCountryCodes.PS else
  if SameText(s,'PT') then Result := TZUGFeRDCountryCodes.PT else
  if SameText(s,'PW') then Result := TZUGFeRDCountryCodes.PW else
  if SameText(s,'PY') then Result := TZUGFeRDCountryCodes.PY else
  if SameText(s,'QA') then Result := TZUGFeRDCountryCodes.QA else
  if SameText(s,'RE') then Result := TZUGFeRDCountryCodes.RE else
  if SameText(s,'RO') then Result := TZUGFeRDCountryCodes.RO else
  if SameText(s,'RS') then Result := TZUGFeRDCountryCodes.RS else
  if SameText(s,'RU') then Result := TZUGFeRDCountryCodes.RU else
  if SameText(s,'RW') then Result := TZUGFeRDCountryCodes.RW else
  if SameText(s,'SA') then Result := TZUGFeRDCountryCodes.SA else
  if SameText(s,'SB') then Result := TZUGFeRDCountryCodes.SB else
  if SameText(s,'SC') then Result := TZUGFeRDCountryCodes.SC else
  if SameText(s,'SD') then Result := TZUGFeRDCountryCodes.SD else
  if SameText(s,'SE') then Result := TZUGFeRDCountryCodes.SE else
  if SameText(s,'SG') then Result := TZUGFeRDCountryCodes.SG else
  if SameText(s,'SH') then Result := TZUGFeRDCountryCodes.SH else
  if SameText(s,'SI') then Result := TZUGFeRDCountryCodes.SI else
  if SameText(s,'SJ') then Result := TZUGFeRDCountryCodes.SJ else
  if SameText(s,'SK') then Result := TZUGFeRDCountryCodes.SK else
  if SameText(s,'SL') then Result := TZUGFeRDCountryCodes.SL else
  if SameText(s,'SM') then Result := TZUGFeRDCountryCodes.SM else
  if SameText(s,'SN') then Result := TZUGFeRDCountryCodes.SN else
  if SameText(s,'SO') then Result := TZUGFeRDCountryCodes.SO else
  if SameText(s,'SR') then Result := TZUGFeRDCountryCodes.SR else
  if SameText(s,'SS') then Result := TZUGFeRDCountryCodes.SS else
  if SameText(s,'ST') then Result := TZUGFeRDCountryCodes.ST else
  if SameText(s,'SV') then Result := TZUGFeRDCountryCodes.SV else
  if SameText(s,'SX') then Result := TZUGFeRDCountryCodes.SX else
  if SameText(s,'SY') then Result := TZUGFeRDCountryCodes.SY else
  if SameText(s,'SZ') then Result := TZUGFeRDCountryCodes.SZ else
  if SameText(s,'TC') then Result := TZUGFeRDCountryCodes.TC else
  if SameText(s,'TD') then Result := TZUGFeRDCountryCodes.TD else
  if SameText(s,'TF') then Result := TZUGFeRDCountryCodes.TF else
  if SameText(s,'TG') then Result := TZUGFeRDCountryCodes.TG else
  if SameText(s,'TH') then Result := TZUGFeRDCountryCodes.TH else
  if SameText(s,'TJ') then Result := TZUGFeRDCountryCodes.TJ else
  if SameText(s,'TK') then Result := TZUGFeRDCountryCodes.TK else
  if SameText(s,'TL') then Result := TZUGFeRDCountryCodes.TL else
  if SameText(s,'TM') then Result := TZUGFeRDCountryCodes.TM else
  if SameText(s,'TN') then Result := TZUGFeRDCountryCodes.TN else
  if SameText(s,'TO') then Result := TZUGFeRDCountryCodes.TO_ else
  if SameText(s,'TR') then Result := TZUGFeRDCountryCodes.TR else
  if SameText(s,'TT') then Result := TZUGFeRDCountryCodes.TT else
  if SameText(s,'TV') then Result := TZUGFeRDCountryCodes.TV else
  if SameText(s,'TW') then Result := TZUGFeRDCountryCodes.TW else
  if SameText(s,'TZ') then Result := TZUGFeRDCountryCodes.TZ else
  if SameText(s,'UA') then Result := TZUGFeRDCountryCodes.UA else
  if SameText(s,'UG') then Result := TZUGFeRDCountryCodes.UG else
  if SameText(s,'UM') then Result := TZUGFeRDCountryCodes.UM else
  if SameText(s,'US') then Result := TZUGFeRDCountryCodes.US else
  if SameText(s,'UY') then Result := TZUGFeRDCountryCodes.UY else
  if SameText(s,'UZ') then Result := TZUGFeRDCountryCodes.UZ else
  if SameText(s,'VA') then Result := TZUGFeRDCountryCodes.VA else
  if SameText(s,'VC') then Result := TZUGFeRDCountryCodes.VC else
  if SameText(s,'VE') then Result := TZUGFeRDCountryCodes.VE else
  if SameText(s,'VG') then Result := TZUGFeRDCountryCodes.VG else
  if SameText(s,'VI') then Result := TZUGFeRDCountryCodes.VI else
  if SameText(s,'VN') then Result := TZUGFeRDCountryCodes.VN else
  if SameText(s,'VU') then Result := TZUGFeRDCountryCodes.VU else
  if SameText(s,'WF') then Result := TZUGFeRDCountryCodes.WF else
  if SameText(s,'WS') then Result := TZUGFeRDCountryCodes.WS else
  if SameText(s,'YE') then Result := TZUGFeRDCountryCodes.YE else
  if SameText(s,'YT') then Result := TZUGFeRDCountryCodes.YT else
  if SameText(s,'ZA') then Result := TZUGFeRDCountryCodes.ZA else
  if SameText(s,'ZM') then Result := TZUGFeRDCountryCodes.ZM else
  if SameText(s,'ZW') then Result := TZUGFeRDCountryCodes.ZW else
  if SameText(s,'1A') then Result := TZUGFeRDCountryCodes._1A else
  if SameText(s,'Unknown') then Result := TZUGFeRDCountryCodes.Unknown else
  Result := TZUGFeRDCountryCodes.Unknown;
end;

end.
