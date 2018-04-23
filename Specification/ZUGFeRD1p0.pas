
{************************************************************************************************************}
{                                                                                                            }
{                                              XML-Datenbindung                                              }
{                                                                                                            }
{         Generiert am: 23.04.2018 11:18:06                                                                  }
{       Generiert von: D:\projekte\src_lnxa6_3thParty\_git_ZUGFeRD-for-Delphi\Specification\ZUGFeRD1p0.xsd   }
{                                                                                                            }
{************************************************************************************************************}

unit ZUGFeRD1p0;

interface

uses Xml.xmldom, Xml.XMLDoc, Xml.XMLIntf;

type

{ Forward-Deklarationen }

  IXMLCrossIndustryDocumentType = interface;
  IXMLExchangedDocumentContextType_ram = interface;
  IXMLIndicatorType_udt = interface;
  IXMLDocumentContextParameterType_ram = interface;
  IXMLDocumentContextParameterType_ramList = interface;
  IXMLIDType_udt = interface;
  IXMLIDType_udtList = interface;
  IXMLExchangedDocumentType_ram = interface;
  IXMLTextType_udt = interface;
  IXMLTextType_udtList = interface;
  IXMLDocumentCodeType_qdt = interface;
  IXMLDateTimeType_udt = interface;
  IXMLDateTimeString_udt = interface;
  IXMLNoteType_ram = interface;
  IXMLNoteType_ramList = interface;
  IXMLCodeType_udt = interface;
  IXMLCodeType_udtList = interface;
  IXMLSpecifiedPeriodType_ram = interface;
  IXMLSpecifiedPeriodType_ramList = interface;
  IXMLSupplyChainTradeTransactionType_ram = interface;
  IXMLSupplyChainTradeAgreementType_ram = interface;
  IXMLSupplyChainTradeAgreementType_ramList = interface;
  IXMLTradePartyType_ram = interface;
  IXMLTradePartyType_ramList = interface;
  IXMLTradeContactType_ram = interface;
  IXMLTradeContactType_ramList = interface;
  IXMLUniversalCommunicationType_ram = interface;
  IXMLUniversalCommunicationType_ramList = interface;
  IXMLTradeAddressType_ram = interface;
  IXMLCountryIDType_qdt = interface;
  IXMLTaxRegistrationType_ram = interface;
  IXMLTaxRegistrationType_ramList = interface;
  IXMLTradeDeliveryTermsType_ram = interface;
  IXMLDeliveryTermsCodeType_qdt = interface;
  IXMLReferencedDocumentType_ram = interface;
  IXMLReferencedDocumentType_ramList = interface;
  IXMLReferenceCodeType_qdt = interface;
  IXMLTradePriceType_ram = interface;
  IXMLTradePriceType_ramList = interface;
  IXMLAmountType_udt = interface;
  IXMLAmountType_udtList = interface;
  IXMLQuantityType_udt = interface;
  IXMLQuantityType_udtList = interface;
  IXMLTradeAllowanceChargeType_ram = interface;
  IXMLTradeAllowanceChargeType_ramList = interface;
  IXMLNumericType_udt = interface;
  IXMLPercentType_udt = interface;
  IXMLAllowanceChargeReasonCodeType_qdt = interface;
  IXMLTradeTaxType_ram = interface;
  IXMLTradeTaxType_ramList = interface;
  IXMLTaxTypeCodeType_qdt = interface;
  IXMLTaxCategoryCodeType_qdt = interface;
  IXMLSupplyChainTradeDeliveryType_ram = interface;
  IXMLSupplyChainConsignmentType_ram = interface;
  IXMLSupplyChainConsignmentType_ramList = interface;
  IXMLLogisticsTransportMovementType_ram = interface;
  IXMLSupplyChainEventType_ram = interface;
  IXMLSupplyChainEventType_ramList = interface;
  IXMLSupplyChainTradeSettlementType_ram = interface;
  IXMLTradeSettlementPaymentMeansType_ram = interface;
  IXMLTradeSettlementPaymentMeansType_ramList = interface;
  IXMLPaymentMeansCodeType_qdt = interface;
  IXMLDebtorFinancialAccountType_ram = interface;
  IXMLCreditorFinancialAccountType_ram = interface;
  IXMLDebtorFinancialInstitutionType_ram = interface;
  IXMLCreditorFinancialInstitutionType_ram = interface;
  IXMLLogisticsServiceChargeType_ram = interface;
  IXMLLogisticsServiceChargeType_ramList = interface;
  IXMLTradePaymentTermsType_ram = interface;
  IXMLTradePaymentTermsType_ramList = interface;
  IXMLTradePaymentPenaltyTermsType_ram = interface;
  IXMLTradePaymentPenaltyTermsType_ramList = interface;
  IXMLMeasureType_udt = interface;
  IXMLMeasureType_udtList = interface;
  IXMLTradePaymentDiscountTermsType_ram = interface;
  IXMLTradePaymentDiscountTermsType_ramList = interface;
  IXMLTradeAccountingAccountType_ram = interface;
  IXMLTradeAccountingAccountType_ramList = interface;
  IXMLTradeSettlementMonetarySummationType_ram = interface;
  IXMLSupplyChainTradeLineItemType_ram = interface;
  IXMLSupplyChainTradeLineItemType_ramList = interface;
  IXMLDocumentLineDocumentType_ram = interface;
  IXMLTradeProductType_ram = interface;
  IXMLProductCharacteristicType_ram = interface;
  IXMLProductCharacteristicType_ramList = interface;
  IXMLProductClassificationType_ram = interface;
  IXMLProductClassificationType_ramList = interface;
  IXMLTradeCountryType_ram = interface;
  IXMLTradeCountryType_ramList = interface;
  IXMLReferencedProductType_ram = interface;
  IXMLReferencedProductType_ramList = interface;

{ IXMLCrossIndustryDocumentType }

  IXMLCrossIndustryDocumentType = interface(IXMLNode)
    ['{11833F0E-DDD7-4187-A988-FBF2FE1ECF1C}']
    { Eigenschaftszugriff }
    function Get_SpecifiedExchangedDocumentContext: IXMLExchangedDocumentContextType_ram;
    function Get_HeaderExchangedDocument: IXMLExchangedDocumentType_ram;
    function Get_SpecifiedSupplyChainTradeTransaction: IXMLSupplyChainTradeTransactionType_ram;
    { Methoden & Eigenschaften }
    property SpecifiedExchangedDocumentContext: IXMLExchangedDocumentContextType_ram read Get_SpecifiedExchangedDocumentContext;
    property HeaderExchangedDocument: IXMLExchangedDocumentType_ram read Get_HeaderExchangedDocument;
    property SpecifiedSupplyChainTradeTransaction: IXMLSupplyChainTradeTransactionType_ram read Get_SpecifiedSupplyChainTradeTransaction;
  end;

{ IXMLExchangedDocumentContextType_ram }

  IXMLExchangedDocumentContextType_ram = interface(IXMLNode)
    ['{65D1EF2F-EF29-4ED5-BE04-2F0FF48F902B}']
    { Eigenschaftszugriff }
    function Get_TestIndicator: IXMLIndicatorType_udt;
    function Get_BusinessProcessSpecifiedDocumentContextParameter: IXMLDocumentContextParameterType_ramList;
    function Get_GuidelineSpecifiedDocumentContextParameter: IXMLDocumentContextParameterType_ramList;
    { Methoden & Eigenschaften }
    property TestIndicator: IXMLIndicatorType_udt read Get_TestIndicator;
    property BusinessProcessSpecifiedDocumentContextParameter: IXMLDocumentContextParameterType_ramList read Get_BusinessProcessSpecifiedDocumentContextParameter;
    property GuidelineSpecifiedDocumentContextParameter: IXMLDocumentContextParameterType_ramList read Get_GuidelineSpecifiedDocumentContextParameter;
  end;

{ IXMLIndicatorType_udt }

  IXMLIndicatorType_udt = interface(IXMLNode)
    ['{4F193CE3-F220-4B3C-A03C-7E660A7B6708}']
    { Eigenschaftszugriff }
    function Get_Indicator: Boolean;
    procedure Set_Indicator(Value: Boolean);
    { Methoden & Eigenschaften }
    property Indicator: Boolean read Get_Indicator write Set_Indicator;
  end;

{ IXMLDocumentContextParameterType_ram }

  IXMLDocumentContextParameterType_ram = interface(IXMLNode)
    ['{1241CF35-ECA7-4442-AE08-FFBB459B1104}']
    { Eigenschaftszugriff }
    function Get_ID: IXMLIDType_udt;
    { Methoden & Eigenschaften }
    property ID: IXMLIDType_udt read Get_ID;
  end;

{ IXMLDocumentContextParameterType_ramList }

  IXMLDocumentContextParameterType_ramList = interface(IXMLNodeCollection)
    ['{0AE8C2CC-999D-4C54-8B35-86CA27D1CC9F}']
    { Methoden & Eigenschaften }
    function Add: IXMLDocumentContextParameterType_ram;
    function Insert(const Index: Integer): IXMLDocumentContextParameterType_ram;

    function Get_Item(Index: Integer): IXMLDocumentContextParameterType_ram;
    property Items[Index: Integer]: IXMLDocumentContextParameterType_ram read Get_Item; default;
  end;

{ IXMLIDType_udt }

  IXMLIDType_udt = interface(IXMLNode)
    ['{70DB5F0D-7724-47B3-8A8B-0EFE373BB1FF}']
    { Eigenschaftszugriff }
    function Get_SchemeID: UnicodeString;
    function Get_SchemeAgencyID: UnicodeString;
    procedure Set_SchemeID(Value: UnicodeString);
    procedure Set_SchemeAgencyID(Value: UnicodeString);
    { Methoden & Eigenschaften }
    property SchemeID: UnicodeString read Get_SchemeID write Set_SchemeID;
    property SchemeAgencyID: UnicodeString read Get_SchemeAgencyID write Set_SchemeAgencyID;
  end;

{ IXMLIDType_udtList }

  IXMLIDType_udtList = interface(IXMLNodeCollection)
    ['{2A11265F-71AF-4DB8-A4DC-B5A5651D4773}']
    { Methoden & Eigenschaften }
    function Add: IXMLIDType_udt;
    function Insert(const Index: Integer): IXMLIDType_udt;

    function Get_Item(Index: Integer): IXMLIDType_udt;
    property Items[Index: Integer]: IXMLIDType_udt read Get_Item; default;
  end;

{ IXMLExchangedDocumentType_ram }

  IXMLExchangedDocumentType_ram = interface(IXMLNode)
    ['{26E75DA5-EBC6-46DB-AB06-A14E9EF9283A}']
    { Eigenschaftszugriff }
    function Get_ID: IXMLIDType_udt;
    function Get_Name: IXMLTextType_udtList;
    function Get_TypeCode: IXMLDocumentCodeType_qdt;
    function Get_IssueDateTime: IXMLDateTimeType_udt;
    function Get_CopyIndicator: IXMLIndicatorType_udt;
    function Get_LanguageID: IXMLIDType_udtList;
    function Get_IncludedNote: IXMLNoteType_ramList;
    function Get_EffectiveSpecifiedPeriod: IXMLSpecifiedPeriodType_ram;
    { Methoden & Eigenschaften }
    property ID: IXMLIDType_udt read Get_ID;
    property Name: IXMLTextType_udtList read Get_Name;
    property TypeCode: IXMLDocumentCodeType_qdt read Get_TypeCode;
    property IssueDateTime: IXMLDateTimeType_udt read Get_IssueDateTime;
    property CopyIndicator: IXMLIndicatorType_udt read Get_CopyIndicator;
    property LanguageID: IXMLIDType_udtList read Get_LanguageID;
    property IncludedNote: IXMLNoteType_ramList read Get_IncludedNote;
    property EffectiveSpecifiedPeriod: IXMLSpecifiedPeriodType_ram read Get_EffectiveSpecifiedPeriod;
  end;

{ IXMLTextType_udt }

  IXMLTextType_udt = interface(IXMLNode)
    ['{44839114-81B1-474F-940C-0BB6CEBEEDA8}']
  end;

{ IXMLTextType_udtList }

  IXMLTextType_udtList = interface(IXMLNodeCollection)
    ['{45C2CEA5-2DE0-44B8-8F47-AC3893CBE880}']
    { Methoden & Eigenschaften }
    function Add: IXMLTextType_udt;
    function Insert(const Index: Integer): IXMLTextType_udt;

    function Get_Item(Index: Integer): IXMLTextType_udt;
    property Items[Index: Integer]: IXMLTextType_udt read Get_Item; default;
  end;

{ IXMLDocumentCodeType_qdt }

  IXMLDocumentCodeType_qdt = interface(IXMLNode)
    ['{706A4DE2-A868-4666-9633-CE594546E83A}']
  end;

{ IXMLDateTimeType_udt }

  IXMLDateTimeType_udt = interface(IXMLNode)
    ['{D8290659-CCB1-4320-9304-59A9E9DC4530}']
    { Eigenschaftszugriff }
    function Get_DateTimeString: IXMLDateTimeString_udt;
    { Methoden & Eigenschaften }
    property DateTimeString: IXMLDateTimeString_udt read Get_DateTimeString;
  end;

{ IXMLDateTimeString_udt }

  IXMLDateTimeString_udt = interface(IXMLNode)
    ['{1444B835-40B3-4EED-B413-4FD6B106EE60}']
    { Eigenschaftszugriff }
    function Get_Format: UnicodeString;
    procedure Set_Format(Value: UnicodeString);
    { Methoden & Eigenschaften }
    property Format: UnicodeString read Get_Format write Set_Format;
  end;

{ IXMLNoteType_ram }

  IXMLNoteType_ram = interface(IXMLNode)
    ['{966AA984-22A5-4B71-AA3B-CC2A5412B484}']
    { Eigenschaftszugriff }
    function Get_ContentCode: IXMLCodeType_udtList;
    function Get_Content: IXMLTextType_udtList;
    function Get_SubjectCode: IXMLCodeType_udt;
    { Methoden & Eigenschaften }
    property ContentCode: IXMLCodeType_udtList read Get_ContentCode;
    property Content: IXMLTextType_udtList read Get_Content;
    property SubjectCode: IXMLCodeType_udt read Get_SubjectCode;
  end;

{ IXMLNoteType_ramList }

  IXMLNoteType_ramList = interface(IXMLNodeCollection)
    ['{3E21362E-7FAC-4A0E-816B-F90B4F861941}']
    { Methoden & Eigenschaften }
    function Add: IXMLNoteType_ram;
    function Insert(const Index: Integer): IXMLNoteType_ram;

    function Get_Item(Index: Integer): IXMLNoteType_ram;
    property Items[Index: Integer]: IXMLNoteType_ram read Get_Item; default;
  end;

{ IXMLCodeType_udt }

  IXMLCodeType_udt = interface(IXMLNode)
    ['{669FA19B-BEEC-4A65-9618-470AE1DF9C2A}']
    { Eigenschaftszugriff }
    function Get_ListID: UnicodeString;
    function Get_ListVersionID: UnicodeString;
    procedure Set_ListID(Value: UnicodeString);
    procedure Set_ListVersionID(Value: UnicodeString);
    { Methoden & Eigenschaften }
    property ListID: UnicodeString read Get_ListID write Set_ListID;
    property ListVersionID: UnicodeString read Get_ListVersionID write Set_ListVersionID;
  end;

{ IXMLCodeType_udtList }

  IXMLCodeType_udtList = interface(IXMLNodeCollection)
    ['{856D22FD-1ADF-4255-B779-DADEE41BAB4E}']
    { Methoden & Eigenschaften }
    function Add: IXMLCodeType_udt;
    function Insert(const Index: Integer): IXMLCodeType_udt;

    function Get_Item(Index: Integer): IXMLCodeType_udt;
    property Items[Index: Integer]: IXMLCodeType_udt read Get_Item; default;
  end;

{ IXMLSpecifiedPeriodType_ram }

  IXMLSpecifiedPeriodType_ram = interface(IXMLNode)
    ['{FC63FEC6-4FA6-4212-85A7-7AF49AE38488}']
    { Eigenschaftszugriff }
    function Get_StartDateTime: IXMLDateTimeType_udt;
    function Get_EndDateTime: IXMLDateTimeType_udt;
    function Get_CompleteDateTime: IXMLDateTimeType_udt;
    { Methoden & Eigenschaften }
    property StartDateTime: IXMLDateTimeType_udt read Get_StartDateTime;
    property EndDateTime: IXMLDateTimeType_udt read Get_EndDateTime;
    property CompleteDateTime: IXMLDateTimeType_udt read Get_CompleteDateTime;
  end;

{ IXMLSpecifiedPeriodType_ramList }

  IXMLSpecifiedPeriodType_ramList = interface(IXMLNodeCollection)
    ['{9A5DC9AB-1507-423A-9D90-5D32BAC52AA9}']
    { Methoden & Eigenschaften }
    function Add: IXMLSpecifiedPeriodType_ram;
    function Insert(const Index: Integer): IXMLSpecifiedPeriodType_ram;

    function Get_Item(Index: Integer): IXMLSpecifiedPeriodType_ram;
    property Items[Index: Integer]: IXMLSpecifiedPeriodType_ram read Get_Item; default;
  end;

{ IXMLSupplyChainTradeTransactionType_ram }

  IXMLSupplyChainTradeTransactionType_ram = interface(IXMLNode)
    ['{4BA22EA2-1978-4D37-96B9-3980698B5A84}']
    { Eigenschaftszugriff }
    function Get_ApplicableSupplyChainTradeAgreement: IXMLSupplyChainTradeAgreementType_ramList;
    function Get_ApplicableSupplyChainTradeDelivery: IXMLSupplyChainTradeDeliveryType_ram;
    function Get_ApplicableSupplyChainTradeSettlement: IXMLSupplyChainTradeSettlementType_ram;
    function Get_IncludedSupplyChainTradeLineItem: IXMLSupplyChainTradeLineItemType_ramList;
    { Methoden & Eigenschaften }
    property ApplicableSupplyChainTradeAgreement: IXMLSupplyChainTradeAgreementType_ramList read Get_ApplicableSupplyChainTradeAgreement;
    property ApplicableSupplyChainTradeDelivery: IXMLSupplyChainTradeDeliveryType_ram read Get_ApplicableSupplyChainTradeDelivery;
    property ApplicableSupplyChainTradeSettlement: IXMLSupplyChainTradeSettlementType_ram read Get_ApplicableSupplyChainTradeSettlement;
    property IncludedSupplyChainTradeLineItem: IXMLSupplyChainTradeLineItemType_ramList read Get_IncludedSupplyChainTradeLineItem;
  end;

{ IXMLSupplyChainTradeAgreementType_ram }

  IXMLSupplyChainTradeAgreementType_ram = interface(IXMLNode)
    ['{EEFA49F9-BAAB-4E1C-BCD8-56C97B3C40C8}']
    { Eigenschaftszugriff }
    function Get_BuyerReference: IXMLTextType_udtList;
    function Get_SellerTradeParty: IXMLTradePartyType_ram;
    function Get_BuyerTradeParty: IXMLTradePartyType_ram;
    function Get_ProductEndUserTradeParty: IXMLTradePartyType_ram;
    function Get_ApplicableTradeDeliveryTerms: IXMLTradeDeliveryTermsType_ram;
    function Get_BuyerOrderReferencedDocument: IXMLReferencedDocumentType_ramList;
    function Get_ContractReferencedDocument: IXMLReferencedDocumentType_ramList;
    function Get_AdditionalReferencedDocument: IXMLReferencedDocumentType_ramList;
    function Get_GrossPriceProductTradePrice: IXMLTradePriceType_ramList;
    function Get_NetPriceProductTradePrice: IXMLTradePriceType_ramList;
    function Get_CustomerOrderReferencedDocument: IXMLReferencedDocumentType_ramList;
    { Methoden & Eigenschaften }
    property BuyerReference: IXMLTextType_udtList read Get_BuyerReference;
    property SellerTradeParty: IXMLTradePartyType_ram read Get_SellerTradeParty;
    property BuyerTradeParty: IXMLTradePartyType_ram read Get_BuyerTradeParty;
    property ProductEndUserTradeParty: IXMLTradePartyType_ram read Get_ProductEndUserTradeParty;
    property ApplicableTradeDeliveryTerms: IXMLTradeDeliveryTermsType_ram read Get_ApplicableTradeDeliveryTerms;
    property BuyerOrderReferencedDocument: IXMLReferencedDocumentType_ramList read Get_BuyerOrderReferencedDocument;
    property ContractReferencedDocument: IXMLReferencedDocumentType_ramList read Get_ContractReferencedDocument;
    property AdditionalReferencedDocument: IXMLReferencedDocumentType_ramList read Get_AdditionalReferencedDocument;
    property GrossPriceProductTradePrice: IXMLTradePriceType_ramList read Get_GrossPriceProductTradePrice;
    property NetPriceProductTradePrice: IXMLTradePriceType_ramList read Get_NetPriceProductTradePrice;
    property CustomerOrderReferencedDocument: IXMLReferencedDocumentType_ramList read Get_CustomerOrderReferencedDocument;
  end;

{ IXMLSupplyChainTradeAgreementType_ramList }

  IXMLSupplyChainTradeAgreementType_ramList = interface(IXMLNodeCollection)
    ['{1FF3A4ED-5526-41C6-8B6F-7F3CA74CFBAC}']
    { Methoden & Eigenschaften }
    function Add: IXMLSupplyChainTradeAgreementType_ram;
    function Insert(const Index: Integer): IXMLSupplyChainTradeAgreementType_ram;

    function Get_Item(Index: Integer): IXMLSupplyChainTradeAgreementType_ram;
    property Items[Index: Integer]: IXMLSupplyChainTradeAgreementType_ram read Get_Item; default;
  end;

{ IXMLTradePartyType_ram }

  IXMLTradePartyType_ram = interface(IXMLNode)
    ['{C44462E6-2FA3-4297-A042-B0ED70BACBF0}']
    { Eigenschaftszugriff }
    function Get_ID: IXMLIDType_udtList;
    function Get_GlobalID: IXMLIDType_udtList;
    function Get_Name: IXMLTextType_udt;
    function Get_DefinedTradeContact: IXMLTradeContactType_ramList;
    function Get_PostalTradeAddress: IXMLTradeAddressType_ram;
    function Get_SpecifiedTaxRegistration: IXMLTaxRegistrationType_ramList;
    { Methoden & Eigenschaften }
    property ID: IXMLIDType_udtList read Get_ID;
    property GlobalID: IXMLIDType_udtList read Get_GlobalID;
    property Name: IXMLTextType_udt read Get_Name;
    property DefinedTradeContact: IXMLTradeContactType_ramList read Get_DefinedTradeContact;
    property PostalTradeAddress: IXMLTradeAddressType_ram read Get_PostalTradeAddress;
    property SpecifiedTaxRegistration: IXMLTaxRegistrationType_ramList read Get_SpecifiedTaxRegistration;
  end;

{ IXMLTradePartyType_ramList }

  IXMLTradePartyType_ramList = interface(IXMLNodeCollection)
    ['{DE18F312-6DEB-4EED-B9EA-D212A8B390FE}']
    { Methoden & Eigenschaften }
    function Add: IXMLTradePartyType_ram;
    function Insert(const Index: Integer): IXMLTradePartyType_ram;

    function Get_Item(Index: Integer): IXMLTradePartyType_ram;
    property Items[Index: Integer]: IXMLTradePartyType_ram read Get_Item; default;
  end;

{ IXMLTradeContactType_ram }

  IXMLTradeContactType_ram = interface(IXMLNode)
    ['{5D7CFA45-AF77-4E4B-A73C-B3571E665B8E}']
    { Eigenschaftszugriff }
    function Get_PersonName: IXMLTextType_udt;
    function Get_DepartmentName: IXMLTextType_udt;
    function Get_TelephoneUniversalCommunication: IXMLUniversalCommunicationType_ramList;
    function Get_FaxUniversalCommunication: IXMLUniversalCommunicationType_ramList;
    function Get_EmailURIUniversalCommunication: IXMLUniversalCommunicationType_ram;
    { Methoden & Eigenschaften }
    property PersonName: IXMLTextType_udt read Get_PersonName;
    property DepartmentName: IXMLTextType_udt read Get_DepartmentName;
    property TelephoneUniversalCommunication: IXMLUniversalCommunicationType_ramList read Get_TelephoneUniversalCommunication;
    property FaxUniversalCommunication: IXMLUniversalCommunicationType_ramList read Get_FaxUniversalCommunication;
    property EmailURIUniversalCommunication: IXMLUniversalCommunicationType_ram read Get_EmailURIUniversalCommunication;
  end;

{ IXMLTradeContactType_ramList }

  IXMLTradeContactType_ramList = interface(IXMLNodeCollection)
    ['{646F591C-BC0D-4270-81CC-546A5EC7415B}']
    { Methoden & Eigenschaften }
    function Add: IXMLTradeContactType_ram;
    function Insert(const Index: Integer): IXMLTradeContactType_ram;

    function Get_Item(Index: Integer): IXMLTradeContactType_ram;
    property Items[Index: Integer]: IXMLTradeContactType_ram read Get_Item; default;
  end;

{ IXMLUniversalCommunicationType_ram }

  IXMLUniversalCommunicationType_ram = interface(IXMLNode)
    ['{90B6C282-D4AA-4292-B628-EBC34F0B1C86}']
    { Eigenschaftszugriff }
    function Get_URIID: IXMLIDType_udt;
    function Get_CompleteNumber: IXMLTextType_udt;
    { Methoden & Eigenschaften }
    property URIID: IXMLIDType_udt read Get_URIID;
    property CompleteNumber: IXMLTextType_udt read Get_CompleteNumber;
  end;

{ IXMLUniversalCommunicationType_ramList }

  IXMLUniversalCommunicationType_ramList = interface(IXMLNodeCollection)
    ['{27852972-7AF1-41B7-828F-B1A4CEAAC439}']
    { Methoden & Eigenschaften }
    function Add: IXMLUniversalCommunicationType_ram;
    function Insert(const Index: Integer): IXMLUniversalCommunicationType_ram;

    function Get_Item(Index: Integer): IXMLUniversalCommunicationType_ram;
    property Items[Index: Integer]: IXMLUniversalCommunicationType_ram read Get_Item; default;
  end;

{ IXMLTradeAddressType_ram }

  IXMLTradeAddressType_ram = interface(IXMLNode)
    ['{9AEE0A5D-3044-443B-A607-48B915F1B637}']
    { Eigenschaftszugriff }
    function Get_PostcodeCode: IXMLCodeType_udtList;
    function Get_LineOne: IXMLTextType_udt;
    function Get_LineTwo: IXMLTextType_udt;
    function Get_CityName: IXMLTextType_udt;
    function Get_CountryID: IXMLCountryIDType_qdt;
    { Methoden & Eigenschaften }
    property PostcodeCode: IXMLCodeType_udtList read Get_PostcodeCode;
    property LineOne: IXMLTextType_udt read Get_LineOne;
    property LineTwo: IXMLTextType_udt read Get_LineTwo;
    property CityName: IXMLTextType_udt read Get_CityName;
    property CountryID: IXMLCountryIDType_qdt read Get_CountryID;
  end;

{ IXMLCountryIDType_qdt }

  IXMLCountryIDType_qdt = interface(IXMLNode)
    ['{6641F604-DF84-44B4-AC2B-94650A8834A6}']
  end;

{ IXMLTaxRegistrationType_ram }

  IXMLTaxRegistrationType_ram = interface(IXMLNode)
    ['{7A66529E-E8A0-4A1A-AC67-3843DB4D5D04}']
    { Eigenschaftszugriff }
    function Get_ID: IXMLIDType_udt;
    { Methoden & Eigenschaften }
    property ID: IXMLIDType_udt read Get_ID;
  end;

{ IXMLTaxRegistrationType_ramList }

  IXMLTaxRegistrationType_ramList = interface(IXMLNodeCollection)
    ['{1FD0C1F6-9730-40B6-8571-3E3D7C32472F}']
    { Methoden & Eigenschaften }
    function Add: IXMLTaxRegistrationType_ram;
    function Insert(const Index: Integer): IXMLTaxRegistrationType_ram;

    function Get_Item(Index: Integer): IXMLTaxRegistrationType_ram;
    property Items[Index: Integer]: IXMLTaxRegistrationType_ram read Get_Item; default;
  end;

{ IXMLTradeDeliveryTermsType_ram }

  IXMLTradeDeliveryTermsType_ram = interface(IXMLNode)
    ['{DADF7A87-6345-4DCB-B3B0-2BC6B9527859}']
    { Eigenschaftszugriff }
    function Get_DeliveryTypeCode: IXMLDeliveryTermsCodeType_qdt;
    { Methoden & Eigenschaften }
    property DeliveryTypeCode: IXMLDeliveryTermsCodeType_qdt read Get_DeliveryTypeCode;
  end;

{ IXMLDeliveryTermsCodeType_qdt }

  IXMLDeliveryTermsCodeType_qdt = interface(IXMLNode)
    ['{8D6BF17A-A9DF-4638-8CCF-F5BC5F6F935B}']
  end;

{ IXMLReferencedDocumentType_ram }

  IXMLReferencedDocumentType_ram = interface(IXMLNode)
    ['{2DF98868-1A78-4718-8D44-4C70783270E9}']
    { Eigenschaftszugriff }
    function Get_IssueDateTime: UnicodeString;
    function Get_LineID: IXMLIDType_udt;
    function Get_TypeCode: IXMLDocumentCodeType_qdt;
    function Get_ID: IXMLIDType_udtList;
    function Get_ReferenceTypeCode: IXMLReferenceCodeType_qdt;
    procedure Set_IssueDateTime(Value: UnicodeString);
    { Methoden & Eigenschaften }
    property IssueDateTime: UnicodeString read Get_IssueDateTime write Set_IssueDateTime;
    property LineID: IXMLIDType_udt read Get_LineID;
    property TypeCode: IXMLDocumentCodeType_qdt read Get_TypeCode;
    property ID: IXMLIDType_udtList read Get_ID;
    property ReferenceTypeCode: IXMLReferenceCodeType_qdt read Get_ReferenceTypeCode;
  end;

{ IXMLReferencedDocumentType_ramList }

  IXMLReferencedDocumentType_ramList = interface(IXMLNodeCollection)
    ['{C58C9DFC-83A9-427A-9F8A-3EE4A12D0E47}']
    { Methoden & Eigenschaften }
    function Add: IXMLReferencedDocumentType_ram;
    function Insert(const Index: Integer): IXMLReferencedDocumentType_ram;

    function Get_Item(Index: Integer): IXMLReferencedDocumentType_ram;
    property Items[Index: Integer]: IXMLReferencedDocumentType_ram read Get_Item; default;
  end;

{ IXMLReferenceCodeType_qdt }

  IXMLReferenceCodeType_qdt = interface(IXMLNode)
    ['{B768B0FF-A4B1-46B7-8429-7EDA206C9763}']
  end;

{ IXMLTradePriceType_ram }

  IXMLTradePriceType_ram = interface(IXMLNode)
    ['{8FA37BCB-5DB1-46AF-A2AF-53AD0A267BC4}']
    { Eigenschaftszugriff }
    function Get_ChargeAmount: IXMLAmountType_udtList;
    function Get_BasisQuantity: IXMLQuantityType_udt;
    function Get_AppliedTradeAllowanceCharge: IXMLTradeAllowanceChargeType_ramList;
    { Methoden & Eigenschaften }
    property ChargeAmount: IXMLAmountType_udtList read Get_ChargeAmount;
    property BasisQuantity: IXMLQuantityType_udt read Get_BasisQuantity;
    property AppliedTradeAllowanceCharge: IXMLTradeAllowanceChargeType_ramList read Get_AppliedTradeAllowanceCharge;
  end;

{ IXMLTradePriceType_ramList }

  IXMLTradePriceType_ramList = interface(IXMLNodeCollection)
    ['{A47C6C80-CAEC-40B4-A2FC-01A65A2D1F72}']
    { Methoden & Eigenschaften }
    function Add: IXMLTradePriceType_ram;
    function Insert(const Index: Integer): IXMLTradePriceType_ram;

    function Get_Item(Index: Integer): IXMLTradePriceType_ram;
    property Items[Index: Integer]: IXMLTradePriceType_ram read Get_Item; default;
  end;

{ IXMLAmountType_udt }

  IXMLAmountType_udt = interface(IXMLNode)
    ['{8FEEFBEB-B245-456A-AF8E-9783871A3A2C}']
    { Eigenschaftszugriff }
    function Get_CurrencyID: UnicodeString;
    procedure Set_CurrencyID(Value: UnicodeString);
    { Methoden & Eigenschaften }
    property CurrencyID: UnicodeString read Get_CurrencyID write Set_CurrencyID;
  end;

{ IXMLAmountType_udtList }

  IXMLAmountType_udtList = interface(IXMLNodeCollection)
    ['{4DFEB629-7598-4B21-8D9C-96FB44E9E08D}']
    { Methoden & Eigenschaften }
    function Add: IXMLAmountType_udt;
    function Insert(const Index: Integer): IXMLAmountType_udt;

    function Get_Item(Index: Integer): IXMLAmountType_udt;
    property Items[Index: Integer]: IXMLAmountType_udt read Get_Item; default;
  end;

{ IXMLQuantityType_udt }

  IXMLQuantityType_udt = interface(IXMLNode)
    ['{35F067A7-30A7-4F95-B862-474B10A4923B}']
    { Eigenschaftszugriff }
    function Get_UnitCode: UnicodeString;
    procedure Set_UnitCode(Value: UnicodeString);
    { Methoden & Eigenschaften }
    property UnitCode: UnicodeString read Get_UnitCode write Set_UnitCode;
  end;

{ IXMLQuantityType_udtList }

  IXMLQuantityType_udtList = interface(IXMLNodeCollection)
    ['{C9679178-35C0-40BD-965A-9AA05E07E284}']
    { Methoden & Eigenschaften }
    function Add: IXMLQuantityType_udt;
    function Insert(const Index: Integer): IXMLQuantityType_udt;

    function Get_Item(Index: Integer): IXMLQuantityType_udt;
    property Items[Index: Integer]: IXMLQuantityType_udt read Get_Item; default;
  end;

{ IXMLTradeAllowanceChargeType_ram }

  IXMLTradeAllowanceChargeType_ram = interface(IXMLNode)
    ['{6EED1FA4-4C32-4BC0-AB2E-745DE0ABD5F0}']
    { Eigenschaftszugriff }
    function Get_ChargeIndicator: IXMLIndicatorType_udt;
    function Get_SequenceNumeric: IXMLNumericType_udt;
    function Get_CalculationPercent: IXMLPercentType_udt;
    function Get_BasisAmount: IXMLAmountType_udt;
    function Get_BasisQuantity: IXMLQuantityType_udt;
    function Get_ActualAmount: IXMLAmountType_udtList;
    function Get_ReasonCode: IXMLAllowanceChargeReasonCodeType_qdt;
    function Get_Reason: IXMLTextType_udt;
    function Get_CategoryTradeTax: IXMLTradeTaxType_ramList;
    { Methoden & Eigenschaften }
    property ChargeIndicator: IXMLIndicatorType_udt read Get_ChargeIndicator;
    property SequenceNumeric: IXMLNumericType_udt read Get_SequenceNumeric;
    property CalculationPercent: IXMLPercentType_udt read Get_CalculationPercent;
    property BasisAmount: IXMLAmountType_udt read Get_BasisAmount;
    property BasisQuantity: IXMLQuantityType_udt read Get_BasisQuantity;
    property ActualAmount: IXMLAmountType_udtList read Get_ActualAmount;
    property ReasonCode: IXMLAllowanceChargeReasonCodeType_qdt read Get_ReasonCode;
    property Reason: IXMLTextType_udt read Get_Reason;
    property CategoryTradeTax: IXMLTradeTaxType_ramList read Get_CategoryTradeTax;
  end;

{ IXMLTradeAllowanceChargeType_ramList }

  IXMLTradeAllowanceChargeType_ramList = interface(IXMLNodeCollection)
    ['{C8FBE0D6-65BD-4872-AF78-91D8CA0A3B5B}']
    { Methoden & Eigenschaften }
    function Add: IXMLTradeAllowanceChargeType_ram;
    function Insert(const Index: Integer): IXMLTradeAllowanceChargeType_ram;

    function Get_Item(Index: Integer): IXMLTradeAllowanceChargeType_ram;
    property Items[Index: Integer]: IXMLTradeAllowanceChargeType_ram read Get_Item; default;
  end;

{ IXMLNumericType_udt }

  IXMLNumericType_udt = interface(IXMLNode)
    ['{A21A1A71-9DFE-4D70-9BED-1947FF5A142E}']
  end;

{ IXMLPercentType_udt }

  IXMLPercentType_udt = interface(IXMLNode)
    ['{DC2CD7CF-2859-454B-8762-78A765427E80}']
  end;

{ IXMLAllowanceChargeReasonCodeType_qdt }

  IXMLAllowanceChargeReasonCodeType_qdt = interface(IXMLNode)
    ['{3E258BCE-B815-43DB-ABE8-AFB402B82C33}']
  end;

{ IXMLTradeTaxType_ram }

  IXMLTradeTaxType_ram = interface(IXMLNode)
    ['{AC663710-8F54-452D-9544-0FAF5358EB4D}']
    { Eigenschaftszugriff }
    function Get_CalculatedAmount: IXMLAmountType_udtList;
    function Get_TypeCode: IXMLTaxTypeCodeType_qdt;
    function Get_ExemptionReason: IXMLTextType_udt;
    function Get_BasisAmount: IXMLAmountType_udtList;
    function Get_LineTotalBasisAmount: IXMLAmountType_udtList;
    function Get_AllowanceChargeBasisAmount: IXMLAmountType_udtList;
    function Get_CategoryCode: IXMLTaxCategoryCodeType_qdt;
    function Get_ApplicablePercent: IXMLPercentType_udt;
    { Methoden & Eigenschaften }
    property CalculatedAmount: IXMLAmountType_udtList read Get_CalculatedAmount;
    property TypeCode: IXMLTaxTypeCodeType_qdt read Get_TypeCode;
    property ExemptionReason: IXMLTextType_udt read Get_ExemptionReason;
    property BasisAmount: IXMLAmountType_udtList read Get_BasisAmount;
    property LineTotalBasisAmount: IXMLAmountType_udtList read Get_LineTotalBasisAmount;
    property AllowanceChargeBasisAmount: IXMLAmountType_udtList read Get_AllowanceChargeBasisAmount;
    property CategoryCode: IXMLTaxCategoryCodeType_qdt read Get_CategoryCode;
    property ApplicablePercent: IXMLPercentType_udt read Get_ApplicablePercent;
  end;

{ IXMLTradeTaxType_ramList }

  IXMLTradeTaxType_ramList = interface(IXMLNodeCollection)
    ['{8A05F9CE-8291-455D-9187-1846992EF8CD}']
    { Methoden & Eigenschaften }
    function Add: IXMLTradeTaxType_ram;
    function Insert(const Index: Integer): IXMLTradeTaxType_ram;

    function Get_Item(Index: Integer): IXMLTradeTaxType_ram;
    property Items[Index: Integer]: IXMLTradeTaxType_ram read Get_Item; default;
  end;

{ IXMLTaxTypeCodeType_qdt }

  IXMLTaxTypeCodeType_qdt = interface(IXMLNode)
    ['{B453ED9A-FD0D-4652-AA8C-993A09323490}']
  end;

{ IXMLTaxCategoryCodeType_qdt }

  IXMLTaxCategoryCodeType_qdt = interface(IXMLNode)
    ['{60F1575F-CC14-4B06-88CA-511CCD38D071}']
  end;

{ IXMLSupplyChainTradeDeliveryType_ram }

  IXMLSupplyChainTradeDeliveryType_ram = interface(IXMLNode)
    ['{325BD23C-B473-4438-9C31-8396B74C5E3D}']
    { Eigenschaftszugriff }
    function Get_BilledQuantity: IXMLQuantityType_udt;
    function Get_ChargeFreeQuantity: IXMLQuantityType_udt;
    function Get_PackageQuantity: IXMLQuantityType_udt;
    function Get_RelatedSupplyChainConsignment: IXMLSupplyChainConsignmentType_ramList;
    function Get_ShipToTradeParty: IXMLTradePartyType_ram;
    function Get_UltimateShipToTradeParty: IXMLTradePartyType_ram;
    function Get_ShipFromTradeParty: IXMLTradePartyType_ram;
    function Get_ActualDeliverySupplyChainEvent: IXMLSupplyChainEventType_ramList;
    function Get_DespatchAdviceReferencedDocument: IXMLReferencedDocumentType_ram;
    function Get_ReceivingAdviceReferencedDocument: IXMLReferencedDocumentType_ramList;
    function Get_DeliveryNoteReferencedDocument: IXMLReferencedDocumentType_ram;
    { Methoden & Eigenschaften }
    property BilledQuantity: IXMLQuantityType_udt read Get_BilledQuantity;
    property ChargeFreeQuantity: IXMLQuantityType_udt read Get_ChargeFreeQuantity;
    property PackageQuantity: IXMLQuantityType_udt read Get_PackageQuantity;
    property RelatedSupplyChainConsignment: IXMLSupplyChainConsignmentType_ramList read Get_RelatedSupplyChainConsignment;
    property ShipToTradeParty: IXMLTradePartyType_ram read Get_ShipToTradeParty;
    property UltimateShipToTradeParty: IXMLTradePartyType_ram read Get_UltimateShipToTradeParty;
    property ShipFromTradeParty: IXMLTradePartyType_ram read Get_ShipFromTradeParty;
    property ActualDeliverySupplyChainEvent: IXMLSupplyChainEventType_ramList read Get_ActualDeliverySupplyChainEvent;
    property DespatchAdviceReferencedDocument: IXMLReferencedDocumentType_ram read Get_DespatchAdviceReferencedDocument;
    property ReceivingAdviceReferencedDocument: IXMLReferencedDocumentType_ramList read Get_ReceivingAdviceReferencedDocument;
    property DeliveryNoteReferencedDocument: IXMLReferencedDocumentType_ram read Get_DeliveryNoteReferencedDocument;
  end;

{ IXMLSupplyChainConsignmentType_ram }

  IXMLSupplyChainConsignmentType_ram = interface(IXMLNodeCollection)
    ['{B85E4584-FED7-46E9-A1E2-7D2D02326916}']
    { Eigenschaftszugriff }
    function Get_SpecifiedLogisticsTransportMovement(Index: Integer): IXMLLogisticsTransportMovementType_ram;
    { Methoden & Eigenschaften }
    function Add: IXMLLogisticsTransportMovementType_ram;
    function Insert(const Index: Integer): IXMLLogisticsTransportMovementType_ram;
    property SpecifiedLogisticsTransportMovement[Index: Integer]: IXMLLogisticsTransportMovementType_ram read Get_SpecifiedLogisticsTransportMovement; default;
  end;

{ IXMLSupplyChainConsignmentType_ramList }

  IXMLSupplyChainConsignmentType_ramList = interface(IXMLNodeCollection)
    ['{0DC29F44-200C-4847-A0D3-C7F1F1B1CBF6}']
    { Methoden & Eigenschaften }
    function Add: IXMLSupplyChainConsignmentType_ram;
    function Insert(const Index: Integer): IXMLSupplyChainConsignmentType_ram;

    function Get_Item(Index: Integer): IXMLSupplyChainConsignmentType_ram;
    property Items[Index: Integer]: IXMLSupplyChainConsignmentType_ram read Get_Item; default;
  end;

{ IXMLLogisticsTransportMovementType_ram }

  IXMLLogisticsTransportMovementType_ram = interface(IXMLNode)
    ['{B0E94786-31FC-4F26-9B6C-2900605A4D89}']
    { Eigenschaftszugriff }
    function Get_ModeCode: IXMLCodeType_udt;
    function Get_ID: IXMLIDType_udt;
    { Methoden & Eigenschaften }
    property ModeCode: IXMLCodeType_udt read Get_ModeCode;
    property ID: IXMLIDType_udt read Get_ID;
  end;

{ IXMLSupplyChainEventType_ram }

  IXMLSupplyChainEventType_ram = interface(IXMLNodeCollection)
    ['{C98D5ECA-00FF-4492-861B-853063BD2409}']
    { Eigenschaftszugriff }
    function Get_OccurrenceDateTime(Index: Integer): IXMLDateTimeType_udt;
    { Methoden & Eigenschaften }
    function Add: IXMLDateTimeType_udt;
    function Insert(const Index: Integer): IXMLDateTimeType_udt;
    property OccurrenceDateTime[Index: Integer]: IXMLDateTimeType_udt read Get_OccurrenceDateTime; default;
  end;

{ IXMLSupplyChainEventType_ramList }

  IXMLSupplyChainEventType_ramList = interface(IXMLNodeCollection)
    ['{E154BDFA-8042-4728-A45E-F935ED4E183A}']
    { Methoden & Eigenschaften }
    function Add: IXMLSupplyChainEventType_ram;
    function Insert(const Index: Integer): IXMLSupplyChainEventType_ram;

    function Get_Item(Index: Integer): IXMLSupplyChainEventType_ram;
    property Items[Index: Integer]: IXMLSupplyChainEventType_ram read Get_Item; default;
  end;

{ IXMLSupplyChainTradeSettlementType_ram }

  IXMLSupplyChainTradeSettlementType_ram = interface(IXMLNode)
    ['{77AA6F01-00EF-4D35-B9F5-89941116DEC2}']
    { Eigenschaftszugriff }
    function Get_PaymentReference: IXMLTextType_udtList;
    function Get_InvoiceCurrencyCode: IXMLCodeType_udt;
    function Get_InvoiceeTradeParty: IXMLTradePartyType_ram;
    function Get_PayeeTradeParty: IXMLTradePartyType_ramList;
    function Get_SpecifiedTradeSettlementPaymentMeans: IXMLTradeSettlementPaymentMeansType_ramList;
    function Get_ApplicableTradeTax: IXMLTradeTaxType_ramList;
    function Get_BillingSpecifiedPeriod: IXMLSpecifiedPeriodType_ramList;
    function Get_SpecifiedTradeAllowanceCharge: IXMLTradeAllowanceChargeType_ramList;
    function Get_SpecifiedLogisticsServiceCharge: IXMLLogisticsServiceChargeType_ramList;
    function Get_SpecifiedTradePaymentTerms: IXMLTradePaymentTermsType_ramList;
    function Get_SpecifiedTradeAccountingAccount: IXMLTradeAccountingAccountType_ramList;
    function Get_SpecifiedTradeSettlementMonetarySummation: IXMLTradeSettlementMonetarySummationType_ram;
    function Get_ReceivableSpecifiedTradeAccountingAccount: IXMLTradeAccountingAccountType_ramList;
    { Methoden & Eigenschaften }
    property PaymentReference: IXMLTextType_udtList read Get_PaymentReference;
    property InvoiceCurrencyCode: IXMLCodeType_udt read Get_InvoiceCurrencyCode;
    property InvoiceeTradeParty: IXMLTradePartyType_ram read Get_InvoiceeTradeParty;
    property PayeeTradeParty: IXMLTradePartyType_ramList read Get_PayeeTradeParty;
    property SpecifiedTradeSettlementPaymentMeans: IXMLTradeSettlementPaymentMeansType_ramList read Get_SpecifiedTradeSettlementPaymentMeans;
    property ApplicableTradeTax: IXMLTradeTaxType_ramList read Get_ApplicableTradeTax;
    property BillingSpecifiedPeriod: IXMLSpecifiedPeriodType_ramList read Get_BillingSpecifiedPeriod;
    property SpecifiedTradeAllowanceCharge: IXMLTradeAllowanceChargeType_ramList read Get_SpecifiedTradeAllowanceCharge;
    property SpecifiedLogisticsServiceCharge: IXMLLogisticsServiceChargeType_ramList read Get_SpecifiedLogisticsServiceCharge;
    property SpecifiedTradePaymentTerms: IXMLTradePaymentTermsType_ramList read Get_SpecifiedTradePaymentTerms;
    property SpecifiedTradeAccountingAccount: IXMLTradeAccountingAccountType_ramList read Get_SpecifiedTradeAccountingAccount;
    property SpecifiedTradeSettlementMonetarySummation: IXMLTradeSettlementMonetarySummationType_ram read Get_SpecifiedTradeSettlementMonetarySummation;
    property ReceivableSpecifiedTradeAccountingAccount: IXMLTradeAccountingAccountType_ramList read Get_ReceivableSpecifiedTradeAccountingAccount;
  end;

{ IXMLTradeSettlementPaymentMeansType_ram }

  IXMLTradeSettlementPaymentMeansType_ram = interface(IXMLNode)
    ['{7BDC42F5-FD18-4728-9470-F073FC84CEAD}']
    { Eigenschaftszugriff }
    function Get_TypeCode: IXMLPaymentMeansCodeType_qdt;
    function Get_Information: IXMLTextType_udtList;
    function Get_ID: IXMLIDType_udtList;
    function Get_PayerPartyDebtorFinancialAccount: IXMLDebtorFinancialAccountType_ram;
    function Get_PayeePartyCreditorFinancialAccount: IXMLCreditorFinancialAccountType_ram;
    function Get_PayerSpecifiedDebtorFinancialInstitution: IXMLDebtorFinancialInstitutionType_ram;
    function Get_PayeeSpecifiedCreditorFinancialInstitution: IXMLCreditorFinancialInstitutionType_ram;
    { Methoden & Eigenschaften }
    property TypeCode: IXMLPaymentMeansCodeType_qdt read Get_TypeCode;
    property Information: IXMLTextType_udtList read Get_Information;
    property ID: IXMLIDType_udtList read Get_ID;
    property PayerPartyDebtorFinancialAccount: IXMLDebtorFinancialAccountType_ram read Get_PayerPartyDebtorFinancialAccount;
    property PayeePartyCreditorFinancialAccount: IXMLCreditorFinancialAccountType_ram read Get_PayeePartyCreditorFinancialAccount;
    property PayerSpecifiedDebtorFinancialInstitution: IXMLDebtorFinancialInstitutionType_ram read Get_PayerSpecifiedDebtorFinancialInstitution;
    property PayeeSpecifiedCreditorFinancialInstitution: IXMLCreditorFinancialInstitutionType_ram read Get_PayeeSpecifiedCreditorFinancialInstitution;
  end;

{ IXMLTradeSettlementPaymentMeansType_ramList }

  IXMLTradeSettlementPaymentMeansType_ramList = interface(IXMLNodeCollection)
    ['{232C98E2-8D9B-424F-8E84-AD20873422B5}']
    { Methoden & Eigenschaften }
    function Add: IXMLTradeSettlementPaymentMeansType_ram;
    function Insert(const Index: Integer): IXMLTradeSettlementPaymentMeansType_ram;

    function Get_Item(Index: Integer): IXMLTradeSettlementPaymentMeansType_ram;
    property Items[Index: Integer]: IXMLTradeSettlementPaymentMeansType_ram read Get_Item; default;
  end;

{ IXMLPaymentMeansCodeType_qdt }

  IXMLPaymentMeansCodeType_qdt = interface(IXMLNode)
    ['{A4FD7DF2-5115-44C5-9962-046041B53D3B}']
  end;

{ IXMLDebtorFinancialAccountType_ram }

  IXMLDebtorFinancialAccountType_ram = interface(IXMLNode)
    ['{A79BC3B3-6BB5-4D02-864A-D47158377DE1}']
    { Eigenschaftszugriff }
    function Get_IBANID: IXMLIDType_udt;
    function Get_ProprietaryID: IXMLIDType_udt;
    { Methoden & Eigenschaften }
    property IBANID: IXMLIDType_udt read Get_IBANID;
    property ProprietaryID: IXMLIDType_udt read Get_ProprietaryID;
  end;

{ IXMLCreditorFinancialAccountType_ram }

  IXMLCreditorFinancialAccountType_ram = interface(IXMLNode)
    ['{A666C76E-6B60-4F1B-9DFC-CFD072E2C009}']
    { Eigenschaftszugriff }
    function Get_IBANID: IXMLIDType_udt;
    function Get_AccountName: IXMLTextType_udt;
    function Get_ProprietaryID: IXMLIDType_udt;
    { Methoden & Eigenschaften }
    property IBANID: IXMLIDType_udt read Get_IBANID;
    property AccountName: IXMLTextType_udt read Get_AccountName;
    property ProprietaryID: IXMLIDType_udt read Get_ProprietaryID;
  end;

{ IXMLDebtorFinancialInstitutionType_ram }

  IXMLDebtorFinancialInstitutionType_ram = interface(IXMLNode)
    ['{7F10E427-D55B-42EF-9227-99A26D6D350C}']
    { Eigenschaftszugriff }
    function Get_BICID: IXMLIDType_udt;
    function Get_GermanBankleitzahlID: IXMLIDType_udt;
    function Get_Name: IXMLTextType_udt;
    { Methoden & Eigenschaften }
    property BICID: IXMLIDType_udt read Get_BICID;
    property GermanBankleitzahlID: IXMLIDType_udt read Get_GermanBankleitzahlID;
    property Name: IXMLTextType_udt read Get_Name;
  end;

{ IXMLCreditorFinancialInstitutionType_ram }

  IXMLCreditorFinancialInstitutionType_ram = interface(IXMLNode)
    ['{962BA29F-1312-4571-ACAE-CEA50E5F02F2}']
    { Eigenschaftszugriff }
    function Get_BICID: IXMLIDType_udt;
    function Get_GermanBankleitzahlID: IXMLIDType_udt;
    function Get_Name: IXMLTextType_udt;
    { Methoden & Eigenschaften }
    property BICID: IXMLIDType_udt read Get_BICID;
    property GermanBankleitzahlID: IXMLIDType_udt read Get_GermanBankleitzahlID;
    property Name: IXMLTextType_udt read Get_Name;
  end;

{ IXMLLogisticsServiceChargeType_ram }

  IXMLLogisticsServiceChargeType_ram = interface(IXMLNode)
    ['{195A546F-7BC8-49B0-8C9C-7F6A144E8D7B}']
    { Eigenschaftszugriff }
    function Get_Description: IXMLTextType_udtList;
    function Get_AppliedAmount: IXMLAmountType_udtList;
    function Get_AppliedTradeTax: IXMLTradeTaxType_ramList;
    { Methoden & Eigenschaften }
    property Description: IXMLTextType_udtList read Get_Description;
    property AppliedAmount: IXMLAmountType_udtList read Get_AppliedAmount;
    property AppliedTradeTax: IXMLTradeTaxType_ramList read Get_AppliedTradeTax;
  end;

{ IXMLLogisticsServiceChargeType_ramList }

  IXMLLogisticsServiceChargeType_ramList = interface(IXMLNodeCollection)
    ['{90037BBA-F1D0-4698-A0DE-A6985F7DF517}']
    { Methoden & Eigenschaften }
    function Add: IXMLLogisticsServiceChargeType_ram;
    function Insert(const Index: Integer): IXMLLogisticsServiceChargeType_ram;

    function Get_Item(Index: Integer): IXMLLogisticsServiceChargeType_ram;
    property Items[Index: Integer]: IXMLLogisticsServiceChargeType_ram read Get_Item; default;
  end;

{ IXMLTradePaymentTermsType_ram }

  IXMLTradePaymentTermsType_ram = interface(IXMLNode)
    ['{B4DC4500-8984-4D9B-8106-3B73D70718A5}']
    { Eigenschaftszugriff }
    function Get_Description: IXMLTextType_udtList;
    function Get_DueDateDateTime: IXMLDateTimeType_udt;
    function Get_PartialPaymentAmount: IXMLAmountType_udtList;
    function Get_ApplicableTradePaymentPenaltyTerms: IXMLTradePaymentPenaltyTermsType_ramList;
    function Get_ApplicableTradePaymentDiscountTerms: IXMLTradePaymentDiscountTermsType_ramList;
    { Methoden & Eigenschaften }
    property Description: IXMLTextType_udtList read Get_Description;
    property DueDateDateTime: IXMLDateTimeType_udt read Get_DueDateDateTime;
    property PartialPaymentAmount: IXMLAmountType_udtList read Get_PartialPaymentAmount;
    property ApplicableTradePaymentPenaltyTerms: IXMLTradePaymentPenaltyTermsType_ramList read Get_ApplicableTradePaymentPenaltyTerms;
    property ApplicableTradePaymentDiscountTerms: IXMLTradePaymentDiscountTermsType_ramList read Get_ApplicableTradePaymentDiscountTerms;
  end;

{ IXMLTradePaymentTermsType_ramList }

  IXMLTradePaymentTermsType_ramList = interface(IXMLNodeCollection)
    ['{B28CE7FD-B0E6-4AB4-A1DC-2E107929F204}']
    { Methoden & Eigenschaften }
    function Add: IXMLTradePaymentTermsType_ram;
    function Insert(const Index: Integer): IXMLTradePaymentTermsType_ram;

    function Get_Item(Index: Integer): IXMLTradePaymentTermsType_ram;
    property Items[Index: Integer]: IXMLTradePaymentTermsType_ram read Get_Item; default;
  end;

{ IXMLTradePaymentPenaltyTermsType_ram }

  IXMLTradePaymentPenaltyTermsType_ram = interface(IXMLNode)
    ['{CB43E22B-0021-4939-901F-B6281545D473}']
    { Eigenschaftszugriff }
    function Get_BasisDateTime: IXMLDateTimeType_udt;
    function Get_BasisPeriodMeasure: IXMLMeasureType_udt;
    function Get_BasisAmount: IXMLAmountType_udtList;
    function Get_CalculationPercent: IXMLPercentType_udt;
    function Get_ActualPenaltyAmount: IXMLAmountType_udtList;
    { Methoden & Eigenschaften }
    property BasisDateTime: IXMLDateTimeType_udt read Get_BasisDateTime;
    property BasisPeriodMeasure: IXMLMeasureType_udt read Get_BasisPeriodMeasure;
    property BasisAmount: IXMLAmountType_udtList read Get_BasisAmount;
    property CalculationPercent: IXMLPercentType_udt read Get_CalculationPercent;
    property ActualPenaltyAmount: IXMLAmountType_udtList read Get_ActualPenaltyAmount;
  end;

{ IXMLTradePaymentPenaltyTermsType_ramList }

  IXMLTradePaymentPenaltyTermsType_ramList = interface(IXMLNodeCollection)
    ['{2FC1683C-E7BD-4EC1-A3E8-0CABCF4392FA}']
    { Methoden & Eigenschaften }
    function Add: IXMLTradePaymentPenaltyTermsType_ram;
    function Insert(const Index: Integer): IXMLTradePaymentPenaltyTermsType_ram;

    function Get_Item(Index: Integer): IXMLTradePaymentPenaltyTermsType_ram;
    property Items[Index: Integer]: IXMLTradePaymentPenaltyTermsType_ram read Get_Item; default;
  end;

{ IXMLMeasureType_udt }

  IXMLMeasureType_udt = interface(IXMLNode)
    ['{1B6BC3E3-0AB4-44F9-830D-C4E2CF1CB61D}']
    { Eigenschaftszugriff }
    function Get_UnitCode: UnicodeString;
    procedure Set_UnitCode(Value: UnicodeString);
    { Methoden & Eigenschaften }
    property UnitCode: UnicodeString read Get_UnitCode write Set_UnitCode;
  end;

{ IXMLMeasureType_udtList }

  IXMLMeasureType_udtList = interface(IXMLNodeCollection)
    ['{7CE33802-8382-478F-9D35-4674F0B4CF30}']
    { Methoden & Eigenschaften }
    function Add: IXMLMeasureType_udt;
    function Insert(const Index: Integer): IXMLMeasureType_udt;

    function Get_Item(Index: Integer): IXMLMeasureType_udt;
    property Items[Index: Integer]: IXMLMeasureType_udt read Get_Item; default;
  end;

{ IXMLTradePaymentDiscountTermsType_ram }

  IXMLTradePaymentDiscountTermsType_ram = interface(IXMLNode)
    ['{85A795AE-9C4D-47B1-A295-5D4B24FFD243}']
    { Eigenschaftszugriff }
    function Get_BasisDateTime: IXMLDateTimeType_udt;
    function Get_BasisPeriodMeasure: IXMLMeasureType_udt;
    function Get_BasisAmount: IXMLAmountType_udtList;
    function Get_CalculationPercent: IXMLPercentType_udt;
    function Get_ActualDiscountAmount: IXMLAmountType_udtList;
    { Methoden & Eigenschaften }
    property BasisDateTime: IXMLDateTimeType_udt read Get_BasisDateTime;
    property BasisPeriodMeasure: IXMLMeasureType_udt read Get_BasisPeriodMeasure;
    property BasisAmount: IXMLAmountType_udtList read Get_BasisAmount;
    property CalculationPercent: IXMLPercentType_udt read Get_CalculationPercent;
    property ActualDiscountAmount: IXMLAmountType_udtList read Get_ActualDiscountAmount;
  end;

{ IXMLTradePaymentDiscountTermsType_ramList }

  IXMLTradePaymentDiscountTermsType_ramList = interface(IXMLNodeCollection)
    ['{F75960F5-6B2A-4094-9398-5D9741583030}']
    { Methoden & Eigenschaften }
    function Add: IXMLTradePaymentDiscountTermsType_ram;
    function Insert(const Index: Integer): IXMLTradePaymentDiscountTermsType_ram;

    function Get_Item(Index: Integer): IXMLTradePaymentDiscountTermsType_ram;
    property Items[Index: Integer]: IXMLTradePaymentDiscountTermsType_ram read Get_Item; default;
  end;

{ IXMLTradeAccountingAccountType_ram }

  IXMLTradeAccountingAccountType_ram = interface(IXMLNode)
    ['{356D3D78-0F97-4A3E-A569-27A9E328B7BE}']
    { Eigenschaftszugriff }
    function Get_ID: IXMLIDType_udt;
    { Methoden & Eigenschaften }
    property ID: IXMLIDType_udt read Get_ID;
  end;

{ IXMLTradeAccountingAccountType_ramList }

  IXMLTradeAccountingAccountType_ramList = interface(IXMLNodeCollection)
    ['{98BC2E68-FEFE-482B-BA8B-C40FD9C17231}']
    { Methoden & Eigenschaften }
    function Add: IXMLTradeAccountingAccountType_ram;
    function Insert(const Index: Integer): IXMLTradeAccountingAccountType_ram;

    function Get_Item(Index: Integer): IXMLTradeAccountingAccountType_ram;
    property Items[Index: Integer]: IXMLTradeAccountingAccountType_ram read Get_Item; default;
  end;

{ IXMLTradeSettlementMonetarySummationType_ram }

  IXMLTradeSettlementMonetarySummationType_ram = interface(IXMLNode)
    ['{06A7AAF1-1448-4BAF-9AF0-B3E47B2526A5}']
    { Eigenschaftszugriff }
    function Get_LineTotalAmount: IXMLAmountType_udtList;
    function Get_ChargeTotalAmount: IXMLAmountType_udtList;
    function Get_AllowanceTotalAmount: IXMLAmountType_udtList;
    function Get_TaxBasisTotalAmount: IXMLAmountType_udtList;
    function Get_TaxTotalAmount: IXMLAmountType_udtList;
    function Get_GrandTotalAmount: IXMLAmountType_udtList;
    function Get_TotalPrepaidAmount: IXMLAmountType_udtList;
    function Get_TotalAllowanceChargeAmount: IXMLAmountType_udtList;
    function Get_DuePayableAmount: IXMLAmountType_udtList;
    { Methoden & Eigenschaften }
    property LineTotalAmount: IXMLAmountType_udtList read Get_LineTotalAmount;
    property ChargeTotalAmount: IXMLAmountType_udtList read Get_ChargeTotalAmount;
    property AllowanceTotalAmount: IXMLAmountType_udtList read Get_AllowanceTotalAmount;
    property TaxBasisTotalAmount: IXMLAmountType_udtList read Get_TaxBasisTotalAmount;
    property TaxTotalAmount: IXMLAmountType_udtList read Get_TaxTotalAmount;
    property GrandTotalAmount: IXMLAmountType_udtList read Get_GrandTotalAmount;
    property TotalPrepaidAmount: IXMLAmountType_udtList read Get_TotalPrepaidAmount;
    property TotalAllowanceChargeAmount: IXMLAmountType_udtList read Get_TotalAllowanceChargeAmount;
    property DuePayableAmount: IXMLAmountType_udtList read Get_DuePayableAmount;
  end;

{ IXMLSupplyChainTradeLineItemType_ram }

  IXMLSupplyChainTradeLineItemType_ram = interface(IXMLNode)
    ['{EA0F8C28-19F3-49C5-9C25-CD234A894F7F}']
    { Eigenschaftszugriff }
    function Get_AssociatedDocumentLineDocument: IXMLDocumentLineDocumentType_ram;
    function Get_SpecifiedSupplyChainTradeAgreement: IXMLSupplyChainTradeAgreementType_ram;
    function Get_SpecifiedSupplyChainTradeDelivery: IXMLSupplyChainTradeDeliveryType_ram;
    function Get_SpecifiedSupplyChainTradeSettlement: IXMLSupplyChainTradeSettlementType_ram;
    function Get_SpecifiedTradeProduct: IXMLTradeProductType_ram;
    { Methoden & Eigenschaften }
    property AssociatedDocumentLineDocument: IXMLDocumentLineDocumentType_ram read Get_AssociatedDocumentLineDocument;
    property SpecifiedSupplyChainTradeAgreement: IXMLSupplyChainTradeAgreementType_ram read Get_SpecifiedSupplyChainTradeAgreement;
    property SpecifiedSupplyChainTradeDelivery: IXMLSupplyChainTradeDeliveryType_ram read Get_SpecifiedSupplyChainTradeDelivery;
    property SpecifiedSupplyChainTradeSettlement: IXMLSupplyChainTradeSettlementType_ram read Get_SpecifiedSupplyChainTradeSettlement;
    property SpecifiedTradeProduct: IXMLTradeProductType_ram read Get_SpecifiedTradeProduct;
  end;

{ IXMLSupplyChainTradeLineItemType_ramList }

  IXMLSupplyChainTradeLineItemType_ramList = interface(IXMLNodeCollection)
    ['{87651C9B-8201-440B-A493-066CBE0EA47B}']
    { Methoden & Eigenschaften }
    function Add: IXMLSupplyChainTradeLineItemType_ram;
    function Insert(const Index: Integer): IXMLSupplyChainTradeLineItemType_ram;

    function Get_Item(Index: Integer): IXMLSupplyChainTradeLineItemType_ram;
    property Items[Index: Integer]: IXMLSupplyChainTradeLineItemType_ram read Get_Item; default;
  end;

{ IXMLDocumentLineDocumentType_ram }

  IXMLDocumentLineDocumentType_ram = interface(IXMLNode)
    ['{2F3D56D9-B3F0-4889-9FCE-41039539C664}']
    { Eigenschaftszugriff }
    function Get_LineID: IXMLIDType_udt;
    function Get_IncludedNote: IXMLNoteType_ramList;
    { Methoden & Eigenschaften }
    property LineID: IXMLIDType_udt read Get_LineID;
    property IncludedNote: IXMLNoteType_ramList read Get_IncludedNote;
  end;

{ IXMLTradeProductType_ram }

  IXMLTradeProductType_ram = interface(IXMLNode)
    ['{CEB4D5A6-CA8B-45EA-8C98-F8ACFC396641}']
    { Eigenschaftszugriff }
    function Get_GlobalID: IXMLIDType_udtList;
    function Get_SellerAssignedID: IXMLIDType_udt;
    function Get_BuyerAssignedID: IXMLIDType_udt;
    function Get_Name: IXMLTextType_udtList;
    function Get_Description: IXMLTextType_udtList;
    function Get_ApplicableProductCharacteristic: IXMLProductCharacteristicType_ramList;
    function Get_DesignatedProductClassification: IXMLProductClassificationType_ramList;
    function Get_OriginTradeCountry: IXMLTradeCountryType_ramList;
    function Get_IncludedReferencedProduct: IXMLReferencedProductType_ramList;
    { Methoden & Eigenschaften }
    property GlobalID: IXMLIDType_udtList read Get_GlobalID;
    property SellerAssignedID: IXMLIDType_udt read Get_SellerAssignedID;
    property BuyerAssignedID: IXMLIDType_udt read Get_BuyerAssignedID;
    property Name: IXMLTextType_udtList read Get_Name;
    property Description: IXMLTextType_udtList read Get_Description;
    property ApplicableProductCharacteristic: IXMLProductCharacteristicType_ramList read Get_ApplicableProductCharacteristic;
    property DesignatedProductClassification: IXMLProductClassificationType_ramList read Get_DesignatedProductClassification;
    property OriginTradeCountry: IXMLTradeCountryType_ramList read Get_OriginTradeCountry;
    property IncludedReferencedProduct: IXMLReferencedProductType_ramList read Get_IncludedReferencedProduct;
  end;

{ IXMLProductCharacteristicType_ram }

  IXMLProductCharacteristicType_ram = interface(IXMLNode)
    ['{3195B735-BB34-40DB-A671-9FFF7CF6FF7A}']
    { Eigenschaftszugriff }
    function Get_TypeCode: IXMLCodeType_udtList;
    function Get_Description: IXMLTextType_udtList;
    function Get_ValueMeasure: IXMLMeasureType_udtList;
    function Get_Value: IXMLTextType_udtList;
    { Methoden & Eigenschaften }
    property TypeCode: IXMLCodeType_udtList read Get_TypeCode;
    property Description: IXMLTextType_udtList read Get_Description;
    property ValueMeasure: IXMLMeasureType_udtList read Get_ValueMeasure;
    property Value: IXMLTextType_udtList read Get_Value;
  end;

{ IXMLProductCharacteristicType_ramList }

  IXMLProductCharacteristicType_ramList = interface(IXMLNodeCollection)
    ['{A19B835B-4A7D-495E-B73F-BF962005D843}']
    { Methoden & Eigenschaften }
    function Add: IXMLProductCharacteristicType_ram;
    function Insert(const Index: Integer): IXMLProductCharacteristicType_ram;

    function Get_Item(Index: Integer): IXMLProductCharacteristicType_ram;
    property Items[Index: Integer]: IXMLProductCharacteristicType_ram read Get_Item; default;
  end;

{ IXMLProductClassificationType_ram }

  IXMLProductClassificationType_ram = interface(IXMLNode)
    ['{A0162A6F-5B4D-426B-8657-EECEF793E690}']
    { Eigenschaftszugriff }
    function Get_ClassCode: IXMLCodeType_udt;
    function Get_ClassName: IXMLTextType_udtList;
    { Methoden & Eigenschaften }
    property ClassCode: IXMLCodeType_udt read Get_ClassCode;
    property ClassName: IXMLTextType_udtList read Get_ClassName;
  end;

{ IXMLProductClassificationType_ramList }

  IXMLProductClassificationType_ramList = interface(IXMLNodeCollection)
    ['{56FE5570-00A6-4213-9037-5DD2BD32D271}']
    { Methoden & Eigenschaften }
    function Add: IXMLProductClassificationType_ram;
    function Insert(const Index: Integer): IXMLProductClassificationType_ram;

    function Get_Item(Index: Integer): IXMLProductClassificationType_ram;
    property Items[Index: Integer]: IXMLProductClassificationType_ram read Get_Item; default;
  end;

{ IXMLTradeCountryType_ram }

  IXMLTradeCountryType_ram = interface(IXMLNodeCollection)
    ['{DF025FDB-EA9F-4F4A-B24A-245FD7C2CE08}']
    { Eigenschaftszugriff }
    function Get_ID(Index: Integer): IXMLCountryIDType_qdt;
    { Methoden & Eigenschaften }
    function Add: IXMLCountryIDType_qdt;
    function Insert(const Index: Integer): IXMLCountryIDType_qdt;
    property ID[Index: Integer]: IXMLCountryIDType_qdt read Get_ID; default;
  end;

{ IXMLTradeCountryType_ramList }

  IXMLTradeCountryType_ramList = interface(IXMLNodeCollection)
    ['{9B13DCAB-A9AB-4A56-A35B-1FC056F97BC0}']
    { Methoden & Eigenschaften }
    function Add: IXMLTradeCountryType_ram;
    function Insert(const Index: Integer): IXMLTradeCountryType_ram;

    function Get_Item(Index: Integer): IXMLTradeCountryType_ram;
    property Items[Index: Integer]: IXMLTradeCountryType_ram read Get_Item; default;
  end;

{ IXMLReferencedProductType_ram }

  IXMLReferencedProductType_ram = interface(IXMLNode)
    ['{09952553-EFE4-479B-B915-FE17947EAD29}']
    { Eigenschaftszugriff }
    function Get_GlobalID: IXMLIDType_udtList;
    function Get_SellerAssignedID: IXMLIDType_udt;
    function Get_BuyerAssignedID: IXMLIDType_udt;
    function Get_Name: IXMLTextType_udtList;
    function Get_Description: IXMLTextType_udtList;
    function Get_UnitQuantity: IXMLQuantityType_udtList;
    { Methoden & Eigenschaften }
    property GlobalID: IXMLIDType_udtList read Get_GlobalID;
    property SellerAssignedID: IXMLIDType_udt read Get_SellerAssignedID;
    property BuyerAssignedID: IXMLIDType_udt read Get_BuyerAssignedID;
    property Name: IXMLTextType_udtList read Get_Name;
    property Description: IXMLTextType_udtList read Get_Description;
    property UnitQuantity: IXMLQuantityType_udtList read Get_UnitQuantity;
  end;

{ IXMLReferencedProductType_ramList }

  IXMLReferencedProductType_ramList = interface(IXMLNodeCollection)
    ['{E86E6D4D-0FC9-4266-97D4-2BD2D3504DB8}']
    { Methoden & Eigenschaften }
    function Add: IXMLReferencedProductType_ram;
    function Insert(const Index: Integer): IXMLReferencedProductType_ram;

    function Get_Item(Index: Integer): IXMLReferencedProductType_ram;
    property Items[Index: Integer]: IXMLReferencedProductType_ram read Get_Item; default;
  end;

{ Forward-Deklarationen }

  TXMLCrossIndustryDocumentType = class;
  TXMLExchangedDocumentContextType_ram = class;
  TXMLIndicatorType_udt = class;
  TXMLDocumentContextParameterType_ram = class;
  TXMLDocumentContextParameterType_ramList = class;
  TXMLIDType_udt = class;
  TXMLIDType_udtList = class;
  TXMLExchangedDocumentType_ram = class;
  TXMLTextType_udt = class;
  TXMLTextType_udtList = class;
  TXMLDocumentCodeType_qdt = class;
  TXMLDateTimeType_udt = class;
  TXMLDateTimeString_udt = class;
  TXMLNoteType_ram = class;
  TXMLNoteType_ramList = class;
  TXMLCodeType_udt = class;
  TXMLCodeType_udtList = class;
  TXMLSpecifiedPeriodType_ram = class;
  TXMLSpecifiedPeriodType_ramList = class;
  TXMLSupplyChainTradeTransactionType_ram = class;
  TXMLSupplyChainTradeAgreementType_ram = class;
  TXMLSupplyChainTradeAgreementType_ramList = class;
  TXMLTradePartyType_ram = class;
  TXMLTradePartyType_ramList = class;
  TXMLTradeContactType_ram = class;
  TXMLTradeContactType_ramList = class;
  TXMLUniversalCommunicationType_ram = class;
  TXMLUniversalCommunicationType_ramList = class;
  TXMLTradeAddressType_ram = class;
  TXMLCountryIDType_qdt = class;
  TXMLTaxRegistrationType_ram = class;
  TXMLTaxRegistrationType_ramList = class;
  TXMLTradeDeliveryTermsType_ram = class;
  TXMLDeliveryTermsCodeType_qdt = class;
  TXMLReferencedDocumentType_ram = class;
  TXMLReferencedDocumentType_ramList = class;
  TXMLReferenceCodeType_qdt = class;
  TXMLTradePriceType_ram = class;
  TXMLTradePriceType_ramList = class;
  TXMLAmountType_udt = class;
  TXMLAmountType_udtList = class;
  TXMLQuantityType_udt = class;
  TXMLQuantityType_udtList = class;
  TXMLTradeAllowanceChargeType_ram = class;
  TXMLTradeAllowanceChargeType_ramList = class;
  TXMLNumericType_udt = class;
  TXMLPercentType_udt = class;
  TXMLAllowanceChargeReasonCodeType_qdt = class;
  TXMLTradeTaxType_ram = class;
  TXMLTradeTaxType_ramList = class;
  TXMLTaxTypeCodeType_qdt = class;
  TXMLTaxCategoryCodeType_qdt = class;
  TXMLSupplyChainTradeDeliveryType_ram = class;
  TXMLSupplyChainConsignmentType_ram = class;
  TXMLSupplyChainConsignmentType_ramList = class;
  TXMLLogisticsTransportMovementType_ram = class;
  TXMLSupplyChainEventType_ram = class;
  TXMLSupplyChainEventType_ramList = class;
  TXMLSupplyChainTradeSettlementType_ram = class;
  TXMLTradeSettlementPaymentMeansType_ram = class;
  TXMLTradeSettlementPaymentMeansType_ramList = class;
  TXMLPaymentMeansCodeType_qdt = class;
  TXMLDebtorFinancialAccountType_ram = class;
  TXMLCreditorFinancialAccountType_ram = class;
  TXMLDebtorFinancialInstitutionType_ram = class;
  TXMLCreditorFinancialInstitutionType_ram = class;
  TXMLLogisticsServiceChargeType_ram = class;
  TXMLLogisticsServiceChargeType_ramList = class;
  TXMLTradePaymentTermsType_ram = class;
  TXMLTradePaymentTermsType_ramList = class;
  TXMLTradePaymentPenaltyTermsType_ram = class;
  TXMLTradePaymentPenaltyTermsType_ramList = class;
  TXMLMeasureType_udt = class;
  TXMLMeasureType_udtList = class;
  TXMLTradePaymentDiscountTermsType_ram = class;
  TXMLTradePaymentDiscountTermsType_ramList = class;
  TXMLTradeAccountingAccountType_ram = class;
  TXMLTradeAccountingAccountType_ramList = class;
  TXMLTradeSettlementMonetarySummationType_ram = class;
  TXMLSupplyChainTradeLineItemType_ram = class;
  TXMLSupplyChainTradeLineItemType_ramList = class;
  TXMLDocumentLineDocumentType_ram = class;
  TXMLTradeProductType_ram = class;
  TXMLProductCharacteristicType_ram = class;
  TXMLProductCharacteristicType_ramList = class;
  TXMLProductClassificationType_ram = class;
  TXMLProductClassificationType_ramList = class;
  TXMLTradeCountryType_ram = class;
  TXMLTradeCountryType_ramList = class;
  TXMLReferencedProductType_ram = class;
  TXMLReferencedProductType_ramList = class;

{ TXMLCrossIndustryDocumentType }

  TXMLCrossIndustryDocumentType = class(TXMLNode, IXMLCrossIndustryDocumentType)
  protected
    { IXMLCrossIndustryDocumentType }
    function Get_SpecifiedExchangedDocumentContext: IXMLExchangedDocumentContextType_ram;
    function Get_HeaderExchangedDocument: IXMLExchangedDocumentType_ram;
    function Get_SpecifiedSupplyChainTradeTransaction: IXMLSupplyChainTradeTransactionType_ram;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLExchangedDocumentContextType_ram }

  TXMLExchangedDocumentContextType_ram = class(TXMLNode, IXMLExchangedDocumentContextType_ram)
  private
    FBusinessProcessSpecifiedDocumentContextParameter: IXMLDocumentContextParameterType_ramList;
    FGuidelineSpecifiedDocumentContextParameter: IXMLDocumentContextParameterType_ramList;
  protected
    { IXMLExchangedDocumentContextType_ram }
    function Get_TestIndicator: IXMLIndicatorType_udt;
    function Get_BusinessProcessSpecifiedDocumentContextParameter: IXMLDocumentContextParameterType_ramList;
    function Get_GuidelineSpecifiedDocumentContextParameter: IXMLDocumentContextParameterType_ramList;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLIndicatorType_udt }

  TXMLIndicatorType_udt = class(TXMLNode, IXMLIndicatorType_udt)
  protected
    { IXMLIndicatorType_udt }
    function Get_Indicator: Boolean;
    procedure Set_Indicator(Value: Boolean);
  end;

{ TXMLDocumentContextParameterType_ram }

  TXMLDocumentContextParameterType_ram = class(TXMLNode, IXMLDocumentContextParameterType_ram)
  protected
    { IXMLDocumentContextParameterType_ram }
    function Get_ID: IXMLIDType_udt;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLDocumentContextParameterType_ramList }

  TXMLDocumentContextParameterType_ramList = class(TXMLNodeCollection, IXMLDocumentContextParameterType_ramList)
  protected
    { IXMLDocumentContextParameterType_ramList }
    function Add: IXMLDocumentContextParameterType_ram;
    function Insert(const Index: Integer): IXMLDocumentContextParameterType_ram;

    function Get_Item(Index: Integer): IXMLDocumentContextParameterType_ram;
  end;

{ TXMLIDType_udt }

  TXMLIDType_udt = class(TXMLNode, IXMLIDType_udt)
  protected
    { IXMLIDType_udt }
    function Get_SchemeID: UnicodeString;
    function Get_SchemeAgencyID: UnicodeString;
    procedure Set_SchemeID(Value: UnicodeString);
    procedure Set_SchemeAgencyID(Value: UnicodeString);
  end;

{ TXMLIDType_udtList }

  TXMLIDType_udtList = class(TXMLNodeCollection, IXMLIDType_udtList)
  protected
    { IXMLIDType_udtList }
    function Add: IXMLIDType_udt;
    function Insert(const Index: Integer): IXMLIDType_udt;

    function Get_Item(Index: Integer): IXMLIDType_udt;
  end;

{ TXMLExchangedDocumentType_ram }

  TXMLExchangedDocumentType_ram = class(TXMLNode, IXMLExchangedDocumentType_ram)
  private
    FName: IXMLTextType_udtList;
    FLanguageID: IXMLIDType_udtList;
    FIncludedNote: IXMLNoteType_ramList;
  protected
    { IXMLExchangedDocumentType_ram }
    function Get_ID: IXMLIDType_udt;
    function Get_Name: IXMLTextType_udtList;
    function Get_TypeCode: IXMLDocumentCodeType_qdt;
    function Get_IssueDateTime: IXMLDateTimeType_udt;
    function Get_CopyIndicator: IXMLIndicatorType_udt;
    function Get_LanguageID: IXMLIDType_udtList;
    function Get_IncludedNote: IXMLNoteType_ramList;
    function Get_EffectiveSpecifiedPeriod: IXMLSpecifiedPeriodType_ram;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTextType_udt }

  TXMLTextType_udt = class(TXMLNode, IXMLTextType_udt)
  protected
    { IXMLTextType_udt }
  end;

{ TXMLTextType_udtList }

  TXMLTextType_udtList = class(TXMLNodeCollection, IXMLTextType_udtList)
  protected
    { IXMLTextType_udtList }
    function Add: IXMLTextType_udt;
    function Insert(const Index: Integer): IXMLTextType_udt;

    function Get_Item(Index: Integer): IXMLTextType_udt;
  end;

{ TXMLDocumentCodeType_qdt }

  TXMLDocumentCodeType_qdt = class(TXMLNode, IXMLDocumentCodeType_qdt)
  protected
    { IXMLDocumentCodeType_qdt }
  end;

{ TXMLDateTimeType_udt }

  TXMLDateTimeType_udt = class(TXMLNode, IXMLDateTimeType_udt)
  protected
    { IXMLDateTimeType_udt }
    function Get_DateTimeString: IXMLDateTimeString_udt;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLDateTimeString_udt }

  TXMLDateTimeString_udt = class(TXMLNode, IXMLDateTimeString_udt)
  protected
    { IXMLDateTimeString_udt }
    function Get_Format: UnicodeString;
    procedure Set_Format(Value: UnicodeString);
  end;

{ TXMLNoteType_ram }

  TXMLNoteType_ram = class(TXMLNode, IXMLNoteType_ram)
  private
    FContentCode: IXMLCodeType_udtList;
    FContent: IXMLTextType_udtList;
  protected
    { IXMLNoteType_ram }
    function Get_ContentCode: IXMLCodeType_udtList;
    function Get_Content: IXMLTextType_udtList;
    function Get_SubjectCode: IXMLCodeType_udt;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLNoteType_ramList }

  TXMLNoteType_ramList = class(TXMLNodeCollection, IXMLNoteType_ramList)
  protected
    { IXMLNoteType_ramList }
    function Add: IXMLNoteType_ram;
    function Insert(const Index: Integer): IXMLNoteType_ram;

    function Get_Item(Index: Integer): IXMLNoteType_ram;
  end;

{ TXMLCodeType_udt }

  TXMLCodeType_udt = class(TXMLNode, IXMLCodeType_udt)
  protected
    { IXMLCodeType_udt }
    function Get_ListID: UnicodeString;
    function Get_ListVersionID: UnicodeString;
    procedure Set_ListID(Value: UnicodeString);
    procedure Set_ListVersionID(Value: UnicodeString);
  end;

{ TXMLCodeType_udtList }

  TXMLCodeType_udtList = class(TXMLNodeCollection, IXMLCodeType_udtList)
  protected
    { IXMLCodeType_udtList }
    function Add: IXMLCodeType_udt;
    function Insert(const Index: Integer): IXMLCodeType_udt;

    function Get_Item(Index: Integer): IXMLCodeType_udt;
  end;

{ TXMLSpecifiedPeriodType_ram }

  TXMLSpecifiedPeriodType_ram = class(TXMLNode, IXMLSpecifiedPeriodType_ram)
  protected
    { IXMLSpecifiedPeriodType_ram }
    function Get_StartDateTime: IXMLDateTimeType_udt;
    function Get_EndDateTime: IXMLDateTimeType_udt;
    function Get_CompleteDateTime: IXMLDateTimeType_udt;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSpecifiedPeriodType_ramList }

  TXMLSpecifiedPeriodType_ramList = class(TXMLNodeCollection, IXMLSpecifiedPeriodType_ramList)
  protected
    { IXMLSpecifiedPeriodType_ramList }
    function Add: IXMLSpecifiedPeriodType_ram;
    function Insert(const Index: Integer): IXMLSpecifiedPeriodType_ram;

    function Get_Item(Index: Integer): IXMLSpecifiedPeriodType_ram;
  end;

{ TXMLSupplyChainTradeTransactionType_ram }

  TXMLSupplyChainTradeTransactionType_ram = class(TXMLNode, IXMLSupplyChainTradeTransactionType_ram)
  private
    FApplicableSupplyChainTradeAgreement: IXMLSupplyChainTradeAgreementType_ramList;
    FIncludedSupplyChainTradeLineItem: IXMLSupplyChainTradeLineItemType_ramList;
  protected
    { IXMLSupplyChainTradeTransactionType_ram }
    function Get_ApplicableSupplyChainTradeAgreement: IXMLSupplyChainTradeAgreementType_ramList;
    function Get_ApplicableSupplyChainTradeDelivery: IXMLSupplyChainTradeDeliveryType_ram;
    function Get_ApplicableSupplyChainTradeSettlement: IXMLSupplyChainTradeSettlementType_ram;
    function Get_IncludedSupplyChainTradeLineItem: IXMLSupplyChainTradeLineItemType_ramList;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSupplyChainTradeAgreementType_ram }

  TXMLSupplyChainTradeAgreementType_ram = class(TXMLNode, IXMLSupplyChainTradeAgreementType_ram)
  private
    FBuyerReference: IXMLTextType_udtList;
    FBuyerOrderReferencedDocument: IXMLReferencedDocumentType_ramList;
    FContractReferencedDocument: IXMLReferencedDocumentType_ramList;
    FAdditionalReferencedDocument: IXMLReferencedDocumentType_ramList;
    FGrossPriceProductTradePrice: IXMLTradePriceType_ramList;
    FNetPriceProductTradePrice: IXMLTradePriceType_ramList;
    FCustomerOrderReferencedDocument: IXMLReferencedDocumentType_ramList;
  protected
    { IXMLSupplyChainTradeAgreementType_ram }
    function Get_BuyerReference: IXMLTextType_udtList;
    function Get_SellerTradeParty: IXMLTradePartyType_ram;
    function Get_BuyerTradeParty: IXMLTradePartyType_ram;
    function Get_ProductEndUserTradeParty: IXMLTradePartyType_ram;
    function Get_ApplicableTradeDeliveryTerms: IXMLTradeDeliveryTermsType_ram;
    function Get_BuyerOrderReferencedDocument: IXMLReferencedDocumentType_ramList;
    function Get_ContractReferencedDocument: IXMLReferencedDocumentType_ramList;
    function Get_AdditionalReferencedDocument: IXMLReferencedDocumentType_ramList;
    function Get_GrossPriceProductTradePrice: IXMLTradePriceType_ramList;
    function Get_NetPriceProductTradePrice: IXMLTradePriceType_ramList;
    function Get_CustomerOrderReferencedDocument: IXMLReferencedDocumentType_ramList;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSupplyChainTradeAgreementType_ramList }

  TXMLSupplyChainTradeAgreementType_ramList = class(TXMLNodeCollection, IXMLSupplyChainTradeAgreementType_ramList)
  protected
    { IXMLSupplyChainTradeAgreementType_ramList }
    function Add: IXMLSupplyChainTradeAgreementType_ram;
    function Insert(const Index: Integer): IXMLSupplyChainTradeAgreementType_ram;

    function Get_Item(Index: Integer): IXMLSupplyChainTradeAgreementType_ram;
  end;

{ TXMLTradePartyType_ram }

  TXMLTradePartyType_ram = class(TXMLNode, IXMLTradePartyType_ram)
  private
    FID: IXMLIDType_udtList;
    FGlobalID: IXMLIDType_udtList;
    FDefinedTradeContact: IXMLTradeContactType_ramList;
    FSpecifiedTaxRegistration: IXMLTaxRegistrationType_ramList;
  protected
    { IXMLTradePartyType_ram }
    function Get_ID: IXMLIDType_udtList;
    function Get_GlobalID: IXMLIDType_udtList;
    function Get_Name: IXMLTextType_udt;
    function Get_DefinedTradeContact: IXMLTradeContactType_ramList;
    function Get_PostalTradeAddress: IXMLTradeAddressType_ram;
    function Get_SpecifiedTaxRegistration: IXMLTaxRegistrationType_ramList;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTradePartyType_ramList }

  TXMLTradePartyType_ramList = class(TXMLNodeCollection, IXMLTradePartyType_ramList)
  protected
    { IXMLTradePartyType_ramList }
    function Add: IXMLTradePartyType_ram;
    function Insert(const Index: Integer): IXMLTradePartyType_ram;

    function Get_Item(Index: Integer): IXMLTradePartyType_ram;
  end;

{ TXMLTradeContactType_ram }

  TXMLTradeContactType_ram = class(TXMLNode, IXMLTradeContactType_ram)
  private
    FTelephoneUniversalCommunication: IXMLUniversalCommunicationType_ramList;
    FFaxUniversalCommunication: IXMLUniversalCommunicationType_ramList;
  protected
    { IXMLTradeContactType_ram }
    function Get_PersonName: IXMLTextType_udt;
    function Get_DepartmentName: IXMLTextType_udt;
    function Get_TelephoneUniversalCommunication: IXMLUniversalCommunicationType_ramList;
    function Get_FaxUniversalCommunication: IXMLUniversalCommunicationType_ramList;
    function Get_EmailURIUniversalCommunication: IXMLUniversalCommunicationType_ram;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTradeContactType_ramList }

  TXMLTradeContactType_ramList = class(TXMLNodeCollection, IXMLTradeContactType_ramList)
  protected
    { IXMLTradeContactType_ramList }
    function Add: IXMLTradeContactType_ram;
    function Insert(const Index: Integer): IXMLTradeContactType_ram;

    function Get_Item(Index: Integer): IXMLTradeContactType_ram;
  end;

{ TXMLUniversalCommunicationType_ram }

  TXMLUniversalCommunicationType_ram = class(TXMLNode, IXMLUniversalCommunicationType_ram)
  protected
    { IXMLUniversalCommunicationType_ram }
    function Get_URIID: IXMLIDType_udt;
    function Get_CompleteNumber: IXMLTextType_udt;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLUniversalCommunicationType_ramList }

  TXMLUniversalCommunicationType_ramList = class(TXMLNodeCollection, IXMLUniversalCommunicationType_ramList)
  protected
    { IXMLUniversalCommunicationType_ramList }
    function Add: IXMLUniversalCommunicationType_ram;
    function Insert(const Index: Integer): IXMLUniversalCommunicationType_ram;

    function Get_Item(Index: Integer): IXMLUniversalCommunicationType_ram;
  end;

{ TXMLTradeAddressType_ram }

  TXMLTradeAddressType_ram = class(TXMLNode, IXMLTradeAddressType_ram)
  private
    FPostcodeCode: IXMLCodeType_udtList;
  protected
    { IXMLTradeAddressType_ram }
    function Get_PostcodeCode: IXMLCodeType_udtList;
    function Get_LineOne: IXMLTextType_udt;
    function Get_LineTwo: IXMLTextType_udt;
    function Get_CityName: IXMLTextType_udt;
    function Get_CountryID: IXMLCountryIDType_qdt;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLCountryIDType_qdt }

  TXMLCountryIDType_qdt = class(TXMLNode, IXMLCountryIDType_qdt)
  protected
    { IXMLCountryIDType_qdt }
  end;

{ TXMLTaxRegistrationType_ram }

  TXMLTaxRegistrationType_ram = class(TXMLNode, IXMLTaxRegistrationType_ram)
  protected
    { IXMLTaxRegistrationType_ram }
    function Get_ID: IXMLIDType_udt;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTaxRegistrationType_ramList }

  TXMLTaxRegistrationType_ramList = class(TXMLNodeCollection, IXMLTaxRegistrationType_ramList)
  protected
    { IXMLTaxRegistrationType_ramList }
    function Add: IXMLTaxRegistrationType_ram;
    function Insert(const Index: Integer): IXMLTaxRegistrationType_ram;

    function Get_Item(Index: Integer): IXMLTaxRegistrationType_ram;
  end;

{ TXMLTradeDeliveryTermsType_ram }

  TXMLTradeDeliveryTermsType_ram = class(TXMLNode, IXMLTradeDeliveryTermsType_ram)
  protected
    { IXMLTradeDeliveryTermsType_ram }
    function Get_DeliveryTypeCode: IXMLDeliveryTermsCodeType_qdt;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLDeliveryTermsCodeType_qdt }

  TXMLDeliveryTermsCodeType_qdt = class(TXMLNode, IXMLDeliveryTermsCodeType_qdt)
  protected
    { IXMLDeliveryTermsCodeType_qdt }
  end;

{ TXMLReferencedDocumentType_ram }

  TXMLReferencedDocumentType_ram = class(TXMLNode, IXMLReferencedDocumentType_ram)
  private
    FID: IXMLIDType_udtList;
  protected
    { IXMLReferencedDocumentType_ram }
    function Get_IssueDateTime: UnicodeString;
    function Get_LineID: IXMLIDType_udt;
    function Get_TypeCode: IXMLDocumentCodeType_qdt;
    function Get_ID: IXMLIDType_udtList;
    function Get_ReferenceTypeCode: IXMLReferenceCodeType_qdt;
    procedure Set_IssueDateTime(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLReferencedDocumentType_ramList }

  TXMLReferencedDocumentType_ramList = class(TXMLNodeCollection, IXMLReferencedDocumentType_ramList)
  protected
    { IXMLReferencedDocumentType_ramList }
    function Add: IXMLReferencedDocumentType_ram;
    function Insert(const Index: Integer): IXMLReferencedDocumentType_ram;

    function Get_Item(Index: Integer): IXMLReferencedDocumentType_ram;
  end;

{ TXMLReferenceCodeType_qdt }

  TXMLReferenceCodeType_qdt = class(TXMLNode, IXMLReferenceCodeType_qdt)
  protected
    { IXMLReferenceCodeType_qdt }
  end;

{ TXMLTradePriceType_ram }

  TXMLTradePriceType_ram = class(TXMLNode, IXMLTradePriceType_ram)
  private
    FChargeAmount: IXMLAmountType_udtList;
    FAppliedTradeAllowanceCharge: IXMLTradeAllowanceChargeType_ramList;
  protected
    { IXMLTradePriceType_ram }
    function Get_ChargeAmount: IXMLAmountType_udtList;
    function Get_BasisQuantity: IXMLQuantityType_udt;
    function Get_AppliedTradeAllowanceCharge: IXMLTradeAllowanceChargeType_ramList;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTradePriceType_ramList }

  TXMLTradePriceType_ramList = class(TXMLNodeCollection, IXMLTradePriceType_ramList)
  protected
    { IXMLTradePriceType_ramList }
    function Add: IXMLTradePriceType_ram;
    function Insert(const Index: Integer): IXMLTradePriceType_ram;

    function Get_Item(Index: Integer): IXMLTradePriceType_ram;
  end;

{ TXMLAmountType_udt }

  TXMLAmountType_udt = class(TXMLNode, IXMLAmountType_udt)
  protected
    { IXMLAmountType_udt }
    function Get_CurrencyID: UnicodeString;
    procedure Set_CurrencyID(Value: UnicodeString);
  end;

{ TXMLAmountType_udtList }

  TXMLAmountType_udtList = class(TXMLNodeCollection, IXMLAmountType_udtList)
  protected
    { IXMLAmountType_udtList }
    function Add: IXMLAmountType_udt;
    function Insert(const Index: Integer): IXMLAmountType_udt;

    function Get_Item(Index: Integer): IXMLAmountType_udt;
  end;

{ TXMLQuantityType_udt }

  TXMLQuantityType_udt = class(TXMLNode, IXMLQuantityType_udt)
  protected
    { IXMLQuantityType_udt }
    function Get_UnitCode: UnicodeString;
    procedure Set_UnitCode(Value: UnicodeString);
  end;

{ TXMLQuantityType_udtList }

  TXMLQuantityType_udtList = class(TXMLNodeCollection, IXMLQuantityType_udtList)
  protected
    { IXMLQuantityType_udtList }
    function Add: IXMLQuantityType_udt;
    function Insert(const Index: Integer): IXMLQuantityType_udt;

    function Get_Item(Index: Integer): IXMLQuantityType_udt;
  end;

{ TXMLTradeAllowanceChargeType_ram }

  TXMLTradeAllowanceChargeType_ram = class(TXMLNode, IXMLTradeAllowanceChargeType_ram)
  private
    FActualAmount: IXMLAmountType_udtList;
    FCategoryTradeTax: IXMLTradeTaxType_ramList;
  protected
    { IXMLTradeAllowanceChargeType_ram }
    function Get_ChargeIndicator: IXMLIndicatorType_udt;
    function Get_SequenceNumeric: IXMLNumericType_udt;
    function Get_CalculationPercent: IXMLPercentType_udt;
    function Get_BasisAmount: IXMLAmountType_udt;
    function Get_BasisQuantity: IXMLQuantityType_udt;
    function Get_ActualAmount: IXMLAmountType_udtList;
    function Get_ReasonCode: IXMLAllowanceChargeReasonCodeType_qdt;
    function Get_Reason: IXMLTextType_udt;
    function Get_CategoryTradeTax: IXMLTradeTaxType_ramList;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTradeAllowanceChargeType_ramList }

  TXMLTradeAllowanceChargeType_ramList = class(TXMLNodeCollection, IXMLTradeAllowanceChargeType_ramList)
  protected
    { IXMLTradeAllowanceChargeType_ramList }
    function Add: IXMLTradeAllowanceChargeType_ram;
    function Insert(const Index: Integer): IXMLTradeAllowanceChargeType_ram;

    function Get_Item(Index: Integer): IXMLTradeAllowanceChargeType_ram;
  end;

{ TXMLNumericType_udt }

  TXMLNumericType_udt = class(TXMLNode, IXMLNumericType_udt)
  protected
    { IXMLNumericType_udt }
  end;

{ TXMLPercentType_udt }

  TXMLPercentType_udt = class(TXMLNode, IXMLPercentType_udt)
  protected
    { IXMLPercentType_udt }
  end;

{ TXMLAllowanceChargeReasonCodeType_qdt }

  TXMLAllowanceChargeReasonCodeType_qdt = class(TXMLNode, IXMLAllowanceChargeReasonCodeType_qdt)
  protected
    { IXMLAllowanceChargeReasonCodeType_qdt }
  end;

{ TXMLTradeTaxType_ram }

  TXMLTradeTaxType_ram = class(TXMLNode, IXMLTradeTaxType_ram)
  private
    FCalculatedAmount: IXMLAmountType_udtList;
    FBasisAmount: IXMLAmountType_udtList;
    FLineTotalBasisAmount: IXMLAmountType_udtList;
    FAllowanceChargeBasisAmount: IXMLAmountType_udtList;
  protected
    { IXMLTradeTaxType_ram }
    function Get_CalculatedAmount: IXMLAmountType_udtList;
    function Get_TypeCode: IXMLTaxTypeCodeType_qdt;
    function Get_ExemptionReason: IXMLTextType_udt;
    function Get_BasisAmount: IXMLAmountType_udtList;
    function Get_LineTotalBasisAmount: IXMLAmountType_udtList;
    function Get_AllowanceChargeBasisAmount: IXMLAmountType_udtList;
    function Get_CategoryCode: IXMLTaxCategoryCodeType_qdt;
    function Get_ApplicablePercent: IXMLPercentType_udt;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTradeTaxType_ramList }

  TXMLTradeTaxType_ramList = class(TXMLNodeCollection, IXMLTradeTaxType_ramList)
  protected
    { IXMLTradeTaxType_ramList }
    function Add: IXMLTradeTaxType_ram;
    function Insert(const Index: Integer): IXMLTradeTaxType_ram;

    function Get_Item(Index: Integer): IXMLTradeTaxType_ram;
  end;

{ TXMLTaxTypeCodeType_qdt }

  TXMLTaxTypeCodeType_qdt = class(TXMLNode, IXMLTaxTypeCodeType_qdt)
  protected
    { IXMLTaxTypeCodeType_qdt }
  end;

{ TXMLTaxCategoryCodeType_qdt }

  TXMLTaxCategoryCodeType_qdt = class(TXMLNode, IXMLTaxCategoryCodeType_qdt)
  protected
    { IXMLTaxCategoryCodeType_qdt }
  end;

{ TXMLSupplyChainTradeDeliveryType_ram }

  TXMLSupplyChainTradeDeliveryType_ram = class(TXMLNode, IXMLSupplyChainTradeDeliveryType_ram)
  private
    FRelatedSupplyChainConsignment: IXMLSupplyChainConsignmentType_ramList;
    FActualDeliverySupplyChainEvent: IXMLSupplyChainEventType_ramList;
    FReceivingAdviceReferencedDocument: IXMLReferencedDocumentType_ramList;
  protected
    { IXMLSupplyChainTradeDeliveryType_ram }
    function Get_BilledQuantity: IXMLQuantityType_udt;
    function Get_ChargeFreeQuantity: IXMLQuantityType_udt;
    function Get_PackageQuantity: IXMLQuantityType_udt;
    function Get_RelatedSupplyChainConsignment: IXMLSupplyChainConsignmentType_ramList;
    function Get_ShipToTradeParty: IXMLTradePartyType_ram;
    function Get_UltimateShipToTradeParty: IXMLTradePartyType_ram;
    function Get_ShipFromTradeParty: IXMLTradePartyType_ram;
    function Get_ActualDeliverySupplyChainEvent: IXMLSupplyChainEventType_ramList;
    function Get_DespatchAdviceReferencedDocument: IXMLReferencedDocumentType_ram;
    function Get_ReceivingAdviceReferencedDocument: IXMLReferencedDocumentType_ramList;
    function Get_DeliveryNoteReferencedDocument: IXMLReferencedDocumentType_ram;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSupplyChainConsignmentType_ram }

  TXMLSupplyChainConsignmentType_ram = class(TXMLNodeCollection, IXMLSupplyChainConsignmentType_ram)
  protected
    { IXMLSupplyChainConsignmentType_ram }
    function Get_SpecifiedLogisticsTransportMovement(Index: Integer): IXMLLogisticsTransportMovementType_ram;
    function Add: IXMLLogisticsTransportMovementType_ram;
    function Insert(const Index: Integer): IXMLLogisticsTransportMovementType_ram;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSupplyChainConsignmentType_ramList }

  TXMLSupplyChainConsignmentType_ramList = class(TXMLNodeCollection, IXMLSupplyChainConsignmentType_ramList)
  protected
    { IXMLSupplyChainConsignmentType_ramList }
    function Add: IXMLSupplyChainConsignmentType_ram;
    function Insert(const Index: Integer): IXMLSupplyChainConsignmentType_ram;

    function Get_Item(Index: Integer): IXMLSupplyChainConsignmentType_ram;
  end;

{ TXMLLogisticsTransportMovementType_ram }

  TXMLLogisticsTransportMovementType_ram = class(TXMLNode, IXMLLogisticsTransportMovementType_ram)
  protected
    { IXMLLogisticsTransportMovementType_ram }
    function Get_ModeCode: IXMLCodeType_udt;
    function Get_ID: IXMLIDType_udt;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSupplyChainEventType_ram }

  TXMLSupplyChainEventType_ram = class(TXMLNodeCollection, IXMLSupplyChainEventType_ram)
  protected
    { IXMLSupplyChainEventType_ram }
    function Get_OccurrenceDateTime(Index: Integer): IXMLDateTimeType_udt;
    function Add: IXMLDateTimeType_udt;
    function Insert(const Index: Integer): IXMLDateTimeType_udt;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSupplyChainEventType_ramList }

  TXMLSupplyChainEventType_ramList = class(TXMLNodeCollection, IXMLSupplyChainEventType_ramList)
  protected
    { IXMLSupplyChainEventType_ramList }
    function Add: IXMLSupplyChainEventType_ram;
    function Insert(const Index: Integer): IXMLSupplyChainEventType_ram;

    function Get_Item(Index: Integer): IXMLSupplyChainEventType_ram;
  end;

{ TXMLSupplyChainTradeSettlementType_ram }

  TXMLSupplyChainTradeSettlementType_ram = class(TXMLNode, IXMLSupplyChainTradeSettlementType_ram)
  private
    FPaymentReference: IXMLTextType_udtList;
    FPayeeTradeParty: IXMLTradePartyType_ramList;
    FSpecifiedTradeSettlementPaymentMeans: IXMLTradeSettlementPaymentMeansType_ramList;
    FApplicableTradeTax: IXMLTradeTaxType_ramList;
    FBillingSpecifiedPeriod: IXMLSpecifiedPeriodType_ramList;
    FSpecifiedTradeAllowanceCharge: IXMLTradeAllowanceChargeType_ramList;
    FSpecifiedLogisticsServiceCharge: IXMLLogisticsServiceChargeType_ramList;
    FSpecifiedTradePaymentTerms: IXMLTradePaymentTermsType_ramList;
    FSpecifiedTradeAccountingAccount: IXMLTradeAccountingAccountType_ramList;
    FReceivableSpecifiedTradeAccountingAccount: IXMLTradeAccountingAccountType_ramList;
  protected
    { IXMLSupplyChainTradeSettlementType_ram }
    function Get_PaymentReference: IXMLTextType_udtList;
    function Get_InvoiceCurrencyCode: IXMLCodeType_udt;
    function Get_InvoiceeTradeParty: IXMLTradePartyType_ram;
    function Get_PayeeTradeParty: IXMLTradePartyType_ramList;
    function Get_SpecifiedTradeSettlementPaymentMeans: IXMLTradeSettlementPaymentMeansType_ramList;
    function Get_ApplicableTradeTax: IXMLTradeTaxType_ramList;
    function Get_BillingSpecifiedPeriod: IXMLSpecifiedPeriodType_ramList;
    function Get_SpecifiedTradeAllowanceCharge: IXMLTradeAllowanceChargeType_ramList;
    function Get_SpecifiedLogisticsServiceCharge: IXMLLogisticsServiceChargeType_ramList;
    function Get_SpecifiedTradePaymentTerms: IXMLTradePaymentTermsType_ramList;
    function Get_SpecifiedTradeAccountingAccount: IXMLTradeAccountingAccountType_ramList;
    function Get_SpecifiedTradeSettlementMonetarySummation: IXMLTradeSettlementMonetarySummationType_ram;
    function Get_ReceivableSpecifiedTradeAccountingAccount: IXMLTradeAccountingAccountType_ramList;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTradeSettlementPaymentMeansType_ram }

  TXMLTradeSettlementPaymentMeansType_ram = class(TXMLNode, IXMLTradeSettlementPaymentMeansType_ram)
  private
    FInformation: IXMLTextType_udtList;
    FID: IXMLIDType_udtList;
  protected
    { IXMLTradeSettlementPaymentMeansType_ram }
    function Get_TypeCode: IXMLPaymentMeansCodeType_qdt;
    function Get_Information: IXMLTextType_udtList;
    function Get_ID: IXMLIDType_udtList;
    function Get_PayerPartyDebtorFinancialAccount: IXMLDebtorFinancialAccountType_ram;
    function Get_PayeePartyCreditorFinancialAccount: IXMLCreditorFinancialAccountType_ram;
    function Get_PayerSpecifiedDebtorFinancialInstitution: IXMLDebtorFinancialInstitutionType_ram;
    function Get_PayeeSpecifiedCreditorFinancialInstitution: IXMLCreditorFinancialInstitutionType_ram;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTradeSettlementPaymentMeansType_ramList }

  TXMLTradeSettlementPaymentMeansType_ramList = class(TXMLNodeCollection, IXMLTradeSettlementPaymentMeansType_ramList)
  protected
    { IXMLTradeSettlementPaymentMeansType_ramList }
    function Add: IXMLTradeSettlementPaymentMeansType_ram;
    function Insert(const Index: Integer): IXMLTradeSettlementPaymentMeansType_ram;

    function Get_Item(Index: Integer): IXMLTradeSettlementPaymentMeansType_ram;
  end;

{ TXMLPaymentMeansCodeType_qdt }

  TXMLPaymentMeansCodeType_qdt = class(TXMLNode, IXMLPaymentMeansCodeType_qdt)
  protected
    { IXMLPaymentMeansCodeType_qdt }
  end;

{ TXMLDebtorFinancialAccountType_ram }

  TXMLDebtorFinancialAccountType_ram = class(TXMLNode, IXMLDebtorFinancialAccountType_ram)
  protected
    { IXMLDebtorFinancialAccountType_ram }
    function Get_IBANID: IXMLIDType_udt;
    function Get_ProprietaryID: IXMLIDType_udt;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLCreditorFinancialAccountType_ram }

  TXMLCreditorFinancialAccountType_ram = class(TXMLNode, IXMLCreditorFinancialAccountType_ram)
  protected
    { IXMLCreditorFinancialAccountType_ram }
    function Get_IBANID: IXMLIDType_udt;
    function Get_AccountName: IXMLTextType_udt;
    function Get_ProprietaryID: IXMLIDType_udt;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLDebtorFinancialInstitutionType_ram }

  TXMLDebtorFinancialInstitutionType_ram = class(TXMLNode, IXMLDebtorFinancialInstitutionType_ram)
  protected
    { IXMLDebtorFinancialInstitutionType_ram }
    function Get_BICID: IXMLIDType_udt;
    function Get_GermanBankleitzahlID: IXMLIDType_udt;
    function Get_Name: IXMLTextType_udt;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLCreditorFinancialInstitutionType_ram }

  TXMLCreditorFinancialInstitutionType_ram = class(TXMLNode, IXMLCreditorFinancialInstitutionType_ram)
  protected
    { IXMLCreditorFinancialInstitutionType_ram }
    function Get_BICID: IXMLIDType_udt;
    function Get_GermanBankleitzahlID: IXMLIDType_udt;
    function Get_Name: IXMLTextType_udt;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLLogisticsServiceChargeType_ram }

  TXMLLogisticsServiceChargeType_ram = class(TXMLNode, IXMLLogisticsServiceChargeType_ram)
  private
    FDescription: IXMLTextType_udtList;
    FAppliedAmount: IXMLAmountType_udtList;
    FAppliedTradeTax: IXMLTradeTaxType_ramList;
  protected
    { IXMLLogisticsServiceChargeType_ram }
    function Get_Description: IXMLTextType_udtList;
    function Get_AppliedAmount: IXMLAmountType_udtList;
    function Get_AppliedTradeTax: IXMLTradeTaxType_ramList;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLLogisticsServiceChargeType_ramList }

  TXMLLogisticsServiceChargeType_ramList = class(TXMLNodeCollection, IXMLLogisticsServiceChargeType_ramList)
  protected
    { IXMLLogisticsServiceChargeType_ramList }
    function Add: IXMLLogisticsServiceChargeType_ram;
    function Insert(const Index: Integer): IXMLLogisticsServiceChargeType_ram;

    function Get_Item(Index: Integer): IXMLLogisticsServiceChargeType_ram;
  end;

{ TXMLTradePaymentTermsType_ram }

  TXMLTradePaymentTermsType_ram = class(TXMLNode, IXMLTradePaymentTermsType_ram)
  private
    FDescription: IXMLTextType_udtList;
    FPartialPaymentAmount: IXMLAmountType_udtList;
    FApplicableTradePaymentPenaltyTerms: IXMLTradePaymentPenaltyTermsType_ramList;
    FApplicableTradePaymentDiscountTerms: IXMLTradePaymentDiscountTermsType_ramList;
  protected
    { IXMLTradePaymentTermsType_ram }
    function Get_Description: IXMLTextType_udtList;
    function Get_DueDateDateTime: IXMLDateTimeType_udt;
    function Get_PartialPaymentAmount: IXMLAmountType_udtList;
    function Get_ApplicableTradePaymentPenaltyTerms: IXMLTradePaymentPenaltyTermsType_ramList;
    function Get_ApplicableTradePaymentDiscountTerms: IXMLTradePaymentDiscountTermsType_ramList;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTradePaymentTermsType_ramList }

  TXMLTradePaymentTermsType_ramList = class(TXMLNodeCollection, IXMLTradePaymentTermsType_ramList)
  protected
    { IXMLTradePaymentTermsType_ramList }
    function Add: IXMLTradePaymentTermsType_ram;
    function Insert(const Index: Integer): IXMLTradePaymentTermsType_ram;

    function Get_Item(Index: Integer): IXMLTradePaymentTermsType_ram;
  end;

{ TXMLTradePaymentPenaltyTermsType_ram }

  TXMLTradePaymentPenaltyTermsType_ram = class(TXMLNode, IXMLTradePaymentPenaltyTermsType_ram)
  private
    FBasisAmount: IXMLAmountType_udtList;
    FActualPenaltyAmount: IXMLAmountType_udtList;
  protected
    { IXMLTradePaymentPenaltyTermsType_ram }
    function Get_BasisDateTime: IXMLDateTimeType_udt;
    function Get_BasisPeriodMeasure: IXMLMeasureType_udt;
    function Get_BasisAmount: IXMLAmountType_udtList;
    function Get_CalculationPercent: IXMLPercentType_udt;
    function Get_ActualPenaltyAmount: IXMLAmountType_udtList;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTradePaymentPenaltyTermsType_ramList }

  TXMLTradePaymentPenaltyTermsType_ramList = class(TXMLNodeCollection, IXMLTradePaymentPenaltyTermsType_ramList)
  protected
    { IXMLTradePaymentPenaltyTermsType_ramList }
    function Add: IXMLTradePaymentPenaltyTermsType_ram;
    function Insert(const Index: Integer): IXMLTradePaymentPenaltyTermsType_ram;

    function Get_Item(Index: Integer): IXMLTradePaymentPenaltyTermsType_ram;
  end;

{ TXMLMeasureType_udt }

  TXMLMeasureType_udt = class(TXMLNode, IXMLMeasureType_udt)
  protected
    { IXMLMeasureType_udt }
    function Get_UnitCode: UnicodeString;
    procedure Set_UnitCode(Value: UnicodeString);
  end;

{ TXMLMeasureType_udtList }

  TXMLMeasureType_udtList = class(TXMLNodeCollection, IXMLMeasureType_udtList)
  protected
    { IXMLMeasureType_udtList }
    function Add: IXMLMeasureType_udt;
    function Insert(const Index: Integer): IXMLMeasureType_udt;

    function Get_Item(Index: Integer): IXMLMeasureType_udt;
  end;

{ TXMLTradePaymentDiscountTermsType_ram }

  TXMLTradePaymentDiscountTermsType_ram = class(TXMLNode, IXMLTradePaymentDiscountTermsType_ram)
  private
    FBasisAmount: IXMLAmountType_udtList;
    FActualDiscountAmount: IXMLAmountType_udtList;
  protected
    { IXMLTradePaymentDiscountTermsType_ram }
    function Get_BasisDateTime: IXMLDateTimeType_udt;
    function Get_BasisPeriodMeasure: IXMLMeasureType_udt;
    function Get_BasisAmount: IXMLAmountType_udtList;
    function Get_CalculationPercent: IXMLPercentType_udt;
    function Get_ActualDiscountAmount: IXMLAmountType_udtList;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTradePaymentDiscountTermsType_ramList }

  TXMLTradePaymentDiscountTermsType_ramList = class(TXMLNodeCollection, IXMLTradePaymentDiscountTermsType_ramList)
  protected
    { IXMLTradePaymentDiscountTermsType_ramList }
    function Add: IXMLTradePaymentDiscountTermsType_ram;
    function Insert(const Index: Integer): IXMLTradePaymentDiscountTermsType_ram;

    function Get_Item(Index: Integer): IXMLTradePaymentDiscountTermsType_ram;
  end;

{ TXMLTradeAccountingAccountType_ram }

  TXMLTradeAccountingAccountType_ram = class(TXMLNode, IXMLTradeAccountingAccountType_ram)
  protected
    { IXMLTradeAccountingAccountType_ram }
    function Get_ID: IXMLIDType_udt;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTradeAccountingAccountType_ramList }

  TXMLTradeAccountingAccountType_ramList = class(TXMLNodeCollection, IXMLTradeAccountingAccountType_ramList)
  protected
    { IXMLTradeAccountingAccountType_ramList }
    function Add: IXMLTradeAccountingAccountType_ram;
    function Insert(const Index: Integer): IXMLTradeAccountingAccountType_ram;

    function Get_Item(Index: Integer): IXMLTradeAccountingAccountType_ram;
  end;

{ TXMLTradeSettlementMonetarySummationType_ram }

  TXMLTradeSettlementMonetarySummationType_ram = class(TXMLNode, IXMLTradeSettlementMonetarySummationType_ram)
  private
    FLineTotalAmount: IXMLAmountType_udtList;
    FChargeTotalAmount: IXMLAmountType_udtList;
    FAllowanceTotalAmount: IXMLAmountType_udtList;
    FTaxBasisTotalAmount: IXMLAmountType_udtList;
    FTaxTotalAmount: IXMLAmountType_udtList;
    FGrandTotalAmount: IXMLAmountType_udtList;
    FTotalPrepaidAmount: IXMLAmountType_udtList;
    FTotalAllowanceChargeAmount: IXMLAmountType_udtList;
    FDuePayableAmount: IXMLAmountType_udtList;
  protected
    { IXMLTradeSettlementMonetarySummationType_ram }
    function Get_LineTotalAmount: IXMLAmountType_udtList;
    function Get_ChargeTotalAmount: IXMLAmountType_udtList;
    function Get_AllowanceTotalAmount: IXMLAmountType_udtList;
    function Get_TaxBasisTotalAmount: IXMLAmountType_udtList;
    function Get_TaxTotalAmount: IXMLAmountType_udtList;
    function Get_GrandTotalAmount: IXMLAmountType_udtList;
    function Get_TotalPrepaidAmount: IXMLAmountType_udtList;
    function Get_TotalAllowanceChargeAmount: IXMLAmountType_udtList;
    function Get_DuePayableAmount: IXMLAmountType_udtList;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSupplyChainTradeLineItemType_ram }

  TXMLSupplyChainTradeLineItemType_ram = class(TXMLNode, IXMLSupplyChainTradeLineItemType_ram)
  protected
    { IXMLSupplyChainTradeLineItemType_ram }
    function Get_AssociatedDocumentLineDocument: IXMLDocumentLineDocumentType_ram;
    function Get_SpecifiedSupplyChainTradeAgreement: IXMLSupplyChainTradeAgreementType_ram;
    function Get_SpecifiedSupplyChainTradeDelivery: IXMLSupplyChainTradeDeliveryType_ram;
    function Get_SpecifiedSupplyChainTradeSettlement: IXMLSupplyChainTradeSettlementType_ram;
    function Get_SpecifiedTradeProduct: IXMLTradeProductType_ram;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSupplyChainTradeLineItemType_ramList }

  TXMLSupplyChainTradeLineItemType_ramList = class(TXMLNodeCollection, IXMLSupplyChainTradeLineItemType_ramList)
  protected
    { IXMLSupplyChainTradeLineItemType_ramList }
    function Add: IXMLSupplyChainTradeLineItemType_ram;
    function Insert(const Index: Integer): IXMLSupplyChainTradeLineItemType_ram;

    function Get_Item(Index: Integer): IXMLSupplyChainTradeLineItemType_ram;
  end;

{ TXMLDocumentLineDocumentType_ram }

  TXMLDocumentLineDocumentType_ram = class(TXMLNode, IXMLDocumentLineDocumentType_ram)
  private
    FIncludedNote: IXMLNoteType_ramList;
  protected
    { IXMLDocumentLineDocumentType_ram }
    function Get_LineID: IXMLIDType_udt;
    function Get_IncludedNote: IXMLNoteType_ramList;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTradeProductType_ram }

  TXMLTradeProductType_ram = class(TXMLNode, IXMLTradeProductType_ram)
  private
    FGlobalID: IXMLIDType_udtList;
    FName: IXMLTextType_udtList;
    FDescription: IXMLTextType_udtList;
    FApplicableProductCharacteristic: IXMLProductCharacteristicType_ramList;
    FDesignatedProductClassification: IXMLProductClassificationType_ramList;
    FOriginTradeCountry: IXMLTradeCountryType_ramList;
    FIncludedReferencedProduct: IXMLReferencedProductType_ramList;
  protected
    { IXMLTradeProductType_ram }
    function Get_GlobalID: IXMLIDType_udtList;
    function Get_SellerAssignedID: IXMLIDType_udt;
    function Get_BuyerAssignedID: IXMLIDType_udt;
    function Get_Name: IXMLTextType_udtList;
    function Get_Description: IXMLTextType_udtList;
    function Get_ApplicableProductCharacteristic: IXMLProductCharacteristicType_ramList;
    function Get_DesignatedProductClassification: IXMLProductClassificationType_ramList;
    function Get_OriginTradeCountry: IXMLTradeCountryType_ramList;
    function Get_IncludedReferencedProduct: IXMLReferencedProductType_ramList;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLProductCharacteristicType_ram }

  TXMLProductCharacteristicType_ram = class(TXMLNode, IXMLProductCharacteristicType_ram)
  private
    FTypeCode: IXMLCodeType_udtList;
    FDescription: IXMLTextType_udtList;
    FValueMeasure: IXMLMeasureType_udtList;
    FValue: IXMLTextType_udtList;
  protected
    { IXMLProductCharacteristicType_ram }
    function Get_TypeCode: IXMLCodeType_udtList;
    function Get_Description: IXMLTextType_udtList;
    function Get_ValueMeasure: IXMLMeasureType_udtList;
    function Get_Value: IXMLTextType_udtList;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLProductCharacteristicType_ramList }

  TXMLProductCharacteristicType_ramList = class(TXMLNodeCollection, IXMLProductCharacteristicType_ramList)
  protected
    { IXMLProductCharacteristicType_ramList }
    function Add: IXMLProductCharacteristicType_ram;
    function Insert(const Index: Integer): IXMLProductCharacteristicType_ram;

    function Get_Item(Index: Integer): IXMLProductCharacteristicType_ram;
  end;

{ TXMLProductClassificationType_ram }

  TXMLProductClassificationType_ram = class(TXMLNode, IXMLProductClassificationType_ram)
  private
    FClassName: IXMLTextType_udtList;
  protected
    { IXMLProductClassificationType_ram }
    function Get_ClassCode: IXMLCodeType_udt;
    function Get_ClassName: IXMLTextType_udtList;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLProductClassificationType_ramList }

  TXMLProductClassificationType_ramList = class(TXMLNodeCollection, IXMLProductClassificationType_ramList)
  protected
    { IXMLProductClassificationType_ramList }
    function Add: IXMLProductClassificationType_ram;
    function Insert(const Index: Integer): IXMLProductClassificationType_ram;

    function Get_Item(Index: Integer): IXMLProductClassificationType_ram;
  end;

{ TXMLTradeCountryType_ram }

  TXMLTradeCountryType_ram = class(TXMLNodeCollection, IXMLTradeCountryType_ram)
  protected
    { IXMLTradeCountryType_ram }
    function Get_ID(Index: Integer): IXMLCountryIDType_qdt;
    function Add: IXMLCountryIDType_qdt;
    function Insert(const Index: Integer): IXMLCountryIDType_qdt;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTradeCountryType_ramList }

  TXMLTradeCountryType_ramList = class(TXMLNodeCollection, IXMLTradeCountryType_ramList)
  protected
    { IXMLTradeCountryType_ramList }
    function Add: IXMLTradeCountryType_ram;
    function Insert(const Index: Integer): IXMLTradeCountryType_ram;

    function Get_Item(Index: Integer): IXMLTradeCountryType_ram;
  end;

{ TXMLReferencedProductType_ram }

  TXMLReferencedProductType_ram = class(TXMLNode, IXMLReferencedProductType_ram)
  private
    FGlobalID: IXMLIDType_udtList;
    FName: IXMLTextType_udtList;
    FDescription: IXMLTextType_udtList;
    FUnitQuantity: IXMLQuantityType_udtList;
  protected
    { IXMLReferencedProductType_ram }
    function Get_GlobalID: IXMLIDType_udtList;
    function Get_SellerAssignedID: IXMLIDType_udt;
    function Get_BuyerAssignedID: IXMLIDType_udt;
    function Get_Name: IXMLTextType_udtList;
    function Get_Description: IXMLTextType_udtList;
    function Get_UnitQuantity: IXMLQuantityType_udtList;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLReferencedProductType_ramList }

  TXMLReferencedProductType_ramList = class(TXMLNodeCollection, IXMLReferencedProductType_ramList)
  protected
    { IXMLReferencedProductType_ramList }
    function Add: IXMLReferencedProductType_ram;
    function Insert(const Index: Integer): IXMLReferencedProductType_ram;

    function Get_Item(Index: Integer): IXMLReferencedProductType_ram;
  end;

{ Globale Funktionen }

function GetCrossIndustryDocument(Doc: IXMLDocument): IXMLCrossIndustryDocumentType;
function LoadCrossIndustryDocument(const FileName: string): IXMLCrossIndustryDocumentType;
function NewCrossIndustryDocument: IXMLCrossIndustryDocumentType;

const
  TargetNamespace = 'urn:ferd:CrossIndustryDocument:invoice:1p0';

implementation

uses Xml.xmlutil;

{ Globale Funktionen }

function GetCrossIndustryDocument(Doc: IXMLDocument): IXMLCrossIndustryDocumentType;
begin
  Result := Doc.GetDocBinding('CrossIndustryDocument', TXMLCrossIndustryDocumentType, TargetNamespace) as IXMLCrossIndustryDocumentType;
end;

function LoadCrossIndustryDocument(const FileName: string): IXMLCrossIndustryDocumentType;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('CrossIndustryDocument', TXMLCrossIndustryDocumentType, TargetNamespace) as IXMLCrossIndustryDocumentType;
end;

function NewCrossIndustryDocument: IXMLCrossIndustryDocumentType;
begin
  Result := NewXMLDocument.GetDocBinding('CrossIndustryDocument', TXMLCrossIndustryDocumentType, TargetNamespace) as IXMLCrossIndustryDocumentType;
end;

{ TXMLCrossIndustryDocumentType }

procedure TXMLCrossIndustryDocumentType.AfterConstruction;
begin
  RegisterChildNode('SpecifiedExchangedDocumentContext', TXMLExchangedDocumentContextType_ram);
  RegisterChildNode('HeaderExchangedDocument', TXMLExchangedDocumentType_ram);
  RegisterChildNode('SpecifiedSupplyChainTradeTransaction', TXMLSupplyChainTradeTransactionType_ram);
  inherited;
end;

function TXMLCrossIndustryDocumentType.Get_SpecifiedExchangedDocumentContext: IXMLExchangedDocumentContextType_ram;
begin
  Result := ChildNodes['SpecifiedExchangedDocumentContext'] as IXMLExchangedDocumentContextType_ram;
end;

function TXMLCrossIndustryDocumentType.Get_HeaderExchangedDocument: IXMLExchangedDocumentType_ram;
begin
  Result := ChildNodes['HeaderExchangedDocument'] as IXMLExchangedDocumentType_ram;
end;

function TXMLCrossIndustryDocumentType.Get_SpecifiedSupplyChainTradeTransaction: IXMLSupplyChainTradeTransactionType_ram;
begin
  Result := ChildNodes['SpecifiedSupplyChainTradeTransaction'] as IXMLSupplyChainTradeTransactionType_ram;
end;

{ TXMLExchangedDocumentContextType_ram }

procedure TXMLExchangedDocumentContextType_ram.AfterConstruction;
begin
  RegisterChildNode('TestIndicator', TXMLIndicatorType_udt);
  RegisterChildNode('BusinessProcessSpecifiedDocumentContextParameter', TXMLDocumentContextParameterType_ram);
  RegisterChildNode('GuidelineSpecifiedDocumentContextParameter', TXMLDocumentContextParameterType_ram);
  FBusinessProcessSpecifiedDocumentContextParameter := CreateCollection(TXMLDocumentContextParameterType_ramList, IXMLDocumentContextParameterType_ram, 'BusinessProcessSpecifiedDocumentContextParameter') as IXMLDocumentContextParameterType_ramList;
  FGuidelineSpecifiedDocumentContextParameter := CreateCollection(TXMLDocumentContextParameterType_ramList, IXMLDocumentContextParameterType_ram, 'GuidelineSpecifiedDocumentContextParameter') as IXMLDocumentContextParameterType_ramList;
  inherited;
end;

function TXMLExchangedDocumentContextType_ram.Get_TestIndicator: IXMLIndicatorType_udt;
begin
  Result := ChildNodes['TestIndicator'] as IXMLIndicatorType_udt;
end;

function TXMLExchangedDocumentContextType_ram.Get_BusinessProcessSpecifiedDocumentContextParameter: IXMLDocumentContextParameterType_ramList;
begin
  Result := FBusinessProcessSpecifiedDocumentContextParameter;
end;

function TXMLExchangedDocumentContextType_ram.Get_GuidelineSpecifiedDocumentContextParameter: IXMLDocumentContextParameterType_ramList;
begin
  Result := FGuidelineSpecifiedDocumentContextParameter;
end;

{ TXMLIndicatorType_udt }

function TXMLIndicatorType_udt.Get_Indicator: Boolean;
begin
  Result := ChildNodes['Indicator'].NodeValue;
end;

procedure TXMLIndicatorType_udt.Set_Indicator(Value: Boolean);
begin
  ChildNodes['Indicator'].NodeValue := Value;
end;

{ TXMLDocumentContextParameterType_ram }

procedure TXMLDocumentContextParameterType_ram.AfterConstruction;
begin
  RegisterChildNode('ID', TXMLIDType_udt);
  inherited;
end;

function TXMLDocumentContextParameterType_ram.Get_ID: IXMLIDType_udt;
begin
  Result := ChildNodes['ID'] as IXMLIDType_udt;
end;

{ TXMLDocumentContextParameterType_ramList }

function TXMLDocumentContextParameterType_ramList.Add: IXMLDocumentContextParameterType_ram;
begin
  Result := AddItem(-1) as IXMLDocumentContextParameterType_ram;
end;

function TXMLDocumentContextParameterType_ramList.Insert(const Index: Integer): IXMLDocumentContextParameterType_ram;
begin
  Result := AddItem(Index) as IXMLDocumentContextParameterType_ram;
end;

function TXMLDocumentContextParameterType_ramList.Get_Item(Index: Integer): IXMLDocumentContextParameterType_ram;
begin
  Result := List[Index] as IXMLDocumentContextParameterType_ram;
end;

{ TXMLIDType_udt }

function TXMLIDType_udt.Get_SchemeID: UnicodeString;
begin
  Result := AttributeNodes['schemeID'].Text;
end;

procedure TXMLIDType_udt.Set_SchemeID(Value: UnicodeString);
begin
  SetAttribute('schemeID', Value);
end;

function TXMLIDType_udt.Get_SchemeAgencyID: UnicodeString;
begin
  Result := AttributeNodes['schemeAgencyID'].Text;
end;

procedure TXMLIDType_udt.Set_SchemeAgencyID(Value: UnicodeString);
begin
  SetAttribute('schemeAgencyID', Value);
end;

{ TXMLIDType_udtList }

function TXMLIDType_udtList.Add: IXMLIDType_udt;
begin
  Result := AddItem(-1) as IXMLIDType_udt;
end;

function TXMLIDType_udtList.Insert(const Index: Integer): IXMLIDType_udt;
begin
  Result := AddItem(Index) as IXMLIDType_udt;
end;

function TXMLIDType_udtList.Get_Item(Index: Integer): IXMLIDType_udt;
begin
  Result := List[Index] as IXMLIDType_udt;
end;

{ TXMLExchangedDocumentType_ram }

procedure TXMLExchangedDocumentType_ram.AfterConstruction;
begin
  RegisterChildNode('ID', TXMLIDType_udt);
  RegisterChildNode('Name', TXMLTextType_udt);
  RegisterChildNode('TypeCode', TXMLDocumentCodeType_qdt);
  RegisterChildNode('IssueDateTime', TXMLDateTimeType_udt);
  RegisterChildNode('CopyIndicator', TXMLIndicatorType_udt);
  RegisterChildNode('LanguageID', TXMLIDType_udt);
  RegisterChildNode('IncludedNote', TXMLNoteType_ram);
  RegisterChildNode('EffectiveSpecifiedPeriod', TXMLSpecifiedPeriodType_ram);
  FName := CreateCollection(TXMLTextType_udtList, IXMLTextType_udt, 'Name') as IXMLTextType_udtList;
  FLanguageID := CreateCollection(TXMLIDType_udtList, IXMLIDType_udt, 'LanguageID') as IXMLIDType_udtList;
  FIncludedNote := CreateCollection(TXMLNoteType_ramList, IXMLNoteType_ram, 'IncludedNote') as IXMLNoteType_ramList;
  inherited;
end;

function TXMLExchangedDocumentType_ram.Get_ID: IXMLIDType_udt;
begin
  Result := ChildNodes['ID'] as IXMLIDType_udt;
end;

function TXMLExchangedDocumentType_ram.Get_Name: IXMLTextType_udtList;
begin
  Result := FName;
end;

function TXMLExchangedDocumentType_ram.Get_TypeCode: IXMLDocumentCodeType_qdt;
begin
  Result := ChildNodes['TypeCode'] as IXMLDocumentCodeType_qdt;
end;

function TXMLExchangedDocumentType_ram.Get_IssueDateTime: IXMLDateTimeType_udt;
begin
  Result := ChildNodes['IssueDateTime'] as IXMLDateTimeType_udt;
end;

function TXMLExchangedDocumentType_ram.Get_CopyIndicator: IXMLIndicatorType_udt;
begin
  Result := ChildNodes['CopyIndicator'] as IXMLIndicatorType_udt;
end;

function TXMLExchangedDocumentType_ram.Get_LanguageID: IXMLIDType_udtList;
begin
  Result := FLanguageID;
end;

function TXMLExchangedDocumentType_ram.Get_IncludedNote: IXMLNoteType_ramList;
begin
  Result := FIncludedNote;
end;

function TXMLExchangedDocumentType_ram.Get_EffectiveSpecifiedPeriod: IXMLSpecifiedPeriodType_ram;
begin
  Result := ChildNodes['EffectiveSpecifiedPeriod'] as IXMLSpecifiedPeriodType_ram;
end;

{ TXMLTextType_udt }

{ TXMLTextType_udtList }

function TXMLTextType_udtList.Add: IXMLTextType_udt;
begin
  Result := AddItem(-1) as IXMLTextType_udt;
end;

function TXMLTextType_udtList.Insert(const Index: Integer): IXMLTextType_udt;
begin
  Result := AddItem(Index) as IXMLTextType_udt;
end;

function TXMLTextType_udtList.Get_Item(Index: Integer): IXMLTextType_udt;
begin
  Result := List[Index] as IXMLTextType_udt;
end;

{ TXMLDocumentCodeType_qdt }

{ TXMLDateTimeType_udt }

procedure TXMLDateTimeType_udt.AfterConstruction;
begin
  RegisterChildNode('DateTimeString', TXMLDateTimeString_udt);
  inherited;
end;

function TXMLDateTimeType_udt.Get_DateTimeString: IXMLDateTimeString_udt;
begin
  Result := ChildNodes['DateTimeString'] as IXMLDateTimeString_udt;
end;

{ TXMLDateTimeString_udt }

function TXMLDateTimeString_udt.Get_Format: UnicodeString;
begin
  Result := AttributeNodes['format'].Text;
end;

procedure TXMLDateTimeString_udt.Set_Format(Value: UnicodeString);
begin
  SetAttribute('format', Value);
end;

{ TXMLNoteType_ram }

procedure TXMLNoteType_ram.AfterConstruction;
begin
  RegisterChildNode('ContentCode', TXMLCodeType_udt);
  RegisterChildNode('Content', TXMLTextType_udt);
  RegisterChildNode('SubjectCode', TXMLCodeType_udt);
  FContentCode := CreateCollection(TXMLCodeType_udtList, IXMLCodeType_udt, 'ContentCode') as IXMLCodeType_udtList;
  FContent := CreateCollection(TXMLTextType_udtList, IXMLTextType_udt, 'Content') as IXMLTextType_udtList;
  inherited;
end;

function TXMLNoteType_ram.Get_ContentCode: IXMLCodeType_udtList;
begin
  Result := FContentCode;
end;

function TXMLNoteType_ram.Get_Content: IXMLTextType_udtList;
begin
  Result := FContent;
end;

function TXMLNoteType_ram.Get_SubjectCode: IXMLCodeType_udt;
begin
  Result := ChildNodes['SubjectCode'] as IXMLCodeType_udt;
end;

{ TXMLNoteType_ramList }

function TXMLNoteType_ramList.Add: IXMLNoteType_ram;
begin
  Result := AddItem(-1) as IXMLNoteType_ram;
end;

function TXMLNoteType_ramList.Insert(const Index: Integer): IXMLNoteType_ram;
begin
  Result := AddItem(Index) as IXMLNoteType_ram;
end;

function TXMLNoteType_ramList.Get_Item(Index: Integer): IXMLNoteType_ram;
begin
  Result := List[Index] as IXMLNoteType_ram;
end;

{ TXMLCodeType_udt }

function TXMLCodeType_udt.Get_ListID: UnicodeString;
begin
  Result := AttributeNodes['listID'].Text;
end;

procedure TXMLCodeType_udt.Set_ListID(Value: UnicodeString);
begin
  SetAttribute('listID', Value);
end;

function TXMLCodeType_udt.Get_ListVersionID: UnicodeString;
begin
  Result := AttributeNodes['listVersionID'].Text;
end;

procedure TXMLCodeType_udt.Set_ListVersionID(Value: UnicodeString);
begin
  SetAttribute('listVersionID', Value);
end;

{ TXMLCodeType_udtList }

function TXMLCodeType_udtList.Add: IXMLCodeType_udt;
begin
  Result := AddItem(-1) as IXMLCodeType_udt;
end;

function TXMLCodeType_udtList.Insert(const Index: Integer): IXMLCodeType_udt;
begin
  Result := AddItem(Index) as IXMLCodeType_udt;
end;

function TXMLCodeType_udtList.Get_Item(Index: Integer): IXMLCodeType_udt;
begin
  Result := List[Index] as IXMLCodeType_udt;
end;

{ TXMLSpecifiedPeriodType_ram }

procedure TXMLSpecifiedPeriodType_ram.AfterConstruction;
begin
  RegisterChildNode('StartDateTime', TXMLDateTimeType_udt);
  RegisterChildNode('EndDateTime', TXMLDateTimeType_udt);
  RegisterChildNode('CompleteDateTime', TXMLDateTimeType_udt);
  inherited;
end;

function TXMLSpecifiedPeriodType_ram.Get_StartDateTime: IXMLDateTimeType_udt;
begin
  Result := ChildNodes['StartDateTime'] as IXMLDateTimeType_udt;
end;

function TXMLSpecifiedPeriodType_ram.Get_EndDateTime: IXMLDateTimeType_udt;
begin
  Result := ChildNodes['EndDateTime'] as IXMLDateTimeType_udt;
end;

function TXMLSpecifiedPeriodType_ram.Get_CompleteDateTime: IXMLDateTimeType_udt;
begin
  Result := ChildNodes['CompleteDateTime'] as IXMLDateTimeType_udt;
end;

{ TXMLSpecifiedPeriodType_ramList }

function TXMLSpecifiedPeriodType_ramList.Add: IXMLSpecifiedPeriodType_ram;
begin
  Result := AddItem(-1) as IXMLSpecifiedPeriodType_ram;
end;

function TXMLSpecifiedPeriodType_ramList.Insert(const Index: Integer): IXMLSpecifiedPeriodType_ram;
begin
  Result := AddItem(Index) as IXMLSpecifiedPeriodType_ram;
end;

function TXMLSpecifiedPeriodType_ramList.Get_Item(Index: Integer): IXMLSpecifiedPeriodType_ram;
begin
  Result := List[Index] as IXMLSpecifiedPeriodType_ram;
end;

{ TXMLSupplyChainTradeTransactionType_ram }

procedure TXMLSupplyChainTradeTransactionType_ram.AfterConstruction;
begin
  RegisterChildNode('ApplicableSupplyChainTradeAgreement', TXMLSupplyChainTradeAgreementType_ram);
  RegisterChildNode('ApplicableSupplyChainTradeDelivery', TXMLSupplyChainTradeDeliveryType_ram);
  RegisterChildNode('ApplicableSupplyChainTradeSettlement', TXMLSupplyChainTradeSettlementType_ram);
  RegisterChildNode('IncludedSupplyChainTradeLineItem', TXMLSupplyChainTradeLineItemType_ram);
  FApplicableSupplyChainTradeAgreement := CreateCollection(TXMLSupplyChainTradeAgreementType_ramList, IXMLSupplyChainTradeAgreementType_ram, 'ApplicableSupplyChainTradeAgreement') as IXMLSupplyChainTradeAgreementType_ramList;
  FIncludedSupplyChainTradeLineItem := CreateCollection(TXMLSupplyChainTradeLineItemType_ramList, IXMLSupplyChainTradeLineItemType_ram, 'IncludedSupplyChainTradeLineItem') as IXMLSupplyChainTradeLineItemType_ramList;
  inherited;
end;

function TXMLSupplyChainTradeTransactionType_ram.Get_ApplicableSupplyChainTradeAgreement: IXMLSupplyChainTradeAgreementType_ramList;
begin
  Result := FApplicableSupplyChainTradeAgreement;
end;

function TXMLSupplyChainTradeTransactionType_ram.Get_ApplicableSupplyChainTradeDelivery: IXMLSupplyChainTradeDeliveryType_ram;
begin
  Result := ChildNodes['ApplicableSupplyChainTradeDelivery'] as IXMLSupplyChainTradeDeliveryType_ram;
end;

function TXMLSupplyChainTradeTransactionType_ram.Get_ApplicableSupplyChainTradeSettlement: IXMLSupplyChainTradeSettlementType_ram;
begin
  Result := ChildNodes['ApplicableSupplyChainTradeSettlement'] as IXMLSupplyChainTradeSettlementType_ram;
end;

function TXMLSupplyChainTradeTransactionType_ram.Get_IncludedSupplyChainTradeLineItem: IXMLSupplyChainTradeLineItemType_ramList;
begin
  Result := FIncludedSupplyChainTradeLineItem;
end;

{ TXMLSupplyChainTradeAgreementType_ram }

procedure TXMLSupplyChainTradeAgreementType_ram.AfterConstruction;
begin
  RegisterChildNode('BuyerReference', TXMLTextType_udt);
  RegisterChildNode('SellerTradeParty', TXMLTradePartyType_ram);
  RegisterChildNode('BuyerTradeParty', TXMLTradePartyType_ram);
  RegisterChildNode('ProductEndUserTradeParty', TXMLTradePartyType_ram);
  RegisterChildNode('ApplicableTradeDeliveryTerms', TXMLTradeDeliveryTermsType_ram);
  RegisterChildNode('BuyerOrderReferencedDocument', TXMLReferencedDocumentType_ram);
  RegisterChildNode('ContractReferencedDocument', TXMLReferencedDocumentType_ram);
  RegisterChildNode('AdditionalReferencedDocument', TXMLReferencedDocumentType_ram);
  RegisterChildNode('GrossPriceProductTradePrice', TXMLTradePriceType_ram);
  RegisterChildNode('NetPriceProductTradePrice', TXMLTradePriceType_ram);
  RegisterChildNode('CustomerOrderReferencedDocument', TXMLReferencedDocumentType_ram);
  FBuyerReference := CreateCollection(TXMLTextType_udtList, IXMLTextType_udt, 'BuyerReference') as IXMLTextType_udtList;
  FBuyerOrderReferencedDocument := CreateCollection(TXMLReferencedDocumentType_ramList, IXMLReferencedDocumentType_ram, 'BuyerOrderReferencedDocument') as IXMLReferencedDocumentType_ramList;
  FContractReferencedDocument := CreateCollection(TXMLReferencedDocumentType_ramList, IXMLReferencedDocumentType_ram, 'ContractReferencedDocument') as IXMLReferencedDocumentType_ramList;
  FAdditionalReferencedDocument := CreateCollection(TXMLReferencedDocumentType_ramList, IXMLReferencedDocumentType_ram, 'AdditionalReferencedDocument') as IXMLReferencedDocumentType_ramList;
  FGrossPriceProductTradePrice := CreateCollection(TXMLTradePriceType_ramList, IXMLTradePriceType_ram, 'GrossPriceProductTradePrice') as IXMLTradePriceType_ramList;
  FNetPriceProductTradePrice := CreateCollection(TXMLTradePriceType_ramList, IXMLTradePriceType_ram, 'NetPriceProductTradePrice') as IXMLTradePriceType_ramList;
  FCustomerOrderReferencedDocument := CreateCollection(TXMLReferencedDocumentType_ramList, IXMLReferencedDocumentType_ram, 'CustomerOrderReferencedDocument') as IXMLReferencedDocumentType_ramList;
  inherited;
end;

function TXMLSupplyChainTradeAgreementType_ram.Get_BuyerReference: IXMLTextType_udtList;
begin
  Result := FBuyerReference;
end;

function TXMLSupplyChainTradeAgreementType_ram.Get_SellerTradeParty: IXMLTradePartyType_ram;
begin
  Result := ChildNodes['SellerTradeParty'] as IXMLTradePartyType_ram;
end;

function TXMLSupplyChainTradeAgreementType_ram.Get_BuyerTradeParty: IXMLTradePartyType_ram;
begin
  Result := ChildNodes['BuyerTradeParty'] as IXMLTradePartyType_ram;
end;

function TXMLSupplyChainTradeAgreementType_ram.Get_ProductEndUserTradeParty: IXMLTradePartyType_ram;
begin
  Result := ChildNodes['ProductEndUserTradeParty'] as IXMLTradePartyType_ram;
end;

function TXMLSupplyChainTradeAgreementType_ram.Get_ApplicableTradeDeliveryTerms: IXMLTradeDeliveryTermsType_ram;
begin
  Result := ChildNodes['ApplicableTradeDeliveryTerms'] as IXMLTradeDeliveryTermsType_ram;
end;

function TXMLSupplyChainTradeAgreementType_ram.Get_BuyerOrderReferencedDocument: IXMLReferencedDocumentType_ramList;
begin
  Result := FBuyerOrderReferencedDocument;
end;

function TXMLSupplyChainTradeAgreementType_ram.Get_ContractReferencedDocument: IXMLReferencedDocumentType_ramList;
begin
  Result := FContractReferencedDocument;
end;

function TXMLSupplyChainTradeAgreementType_ram.Get_AdditionalReferencedDocument: IXMLReferencedDocumentType_ramList;
begin
  Result := FAdditionalReferencedDocument;
end;

function TXMLSupplyChainTradeAgreementType_ram.Get_GrossPriceProductTradePrice: IXMLTradePriceType_ramList;
begin
  Result := FGrossPriceProductTradePrice;
end;

function TXMLSupplyChainTradeAgreementType_ram.Get_NetPriceProductTradePrice: IXMLTradePriceType_ramList;
begin
  Result := FNetPriceProductTradePrice;
end;

function TXMLSupplyChainTradeAgreementType_ram.Get_CustomerOrderReferencedDocument: IXMLReferencedDocumentType_ramList;
begin
  Result := FCustomerOrderReferencedDocument;
end;

{ TXMLSupplyChainTradeAgreementType_ramList }

function TXMLSupplyChainTradeAgreementType_ramList.Add: IXMLSupplyChainTradeAgreementType_ram;
begin
  Result := AddItem(-1) as IXMLSupplyChainTradeAgreementType_ram;
end;

function TXMLSupplyChainTradeAgreementType_ramList.Insert(const Index: Integer): IXMLSupplyChainTradeAgreementType_ram;
begin
  Result := AddItem(Index) as IXMLSupplyChainTradeAgreementType_ram;
end;

function TXMLSupplyChainTradeAgreementType_ramList.Get_Item(Index: Integer): IXMLSupplyChainTradeAgreementType_ram;
begin
  Result := List[Index] as IXMLSupplyChainTradeAgreementType_ram;
end;

{ TXMLTradePartyType_ram }

procedure TXMLTradePartyType_ram.AfterConstruction;
begin
  RegisterChildNode('ID', TXMLIDType_udt);
  RegisterChildNode('GlobalID', TXMLIDType_udt);
  RegisterChildNode('Name', TXMLTextType_udt);
  RegisterChildNode('DefinedTradeContact', TXMLTradeContactType_ram);
  RegisterChildNode('PostalTradeAddress', TXMLTradeAddressType_ram);
  RegisterChildNode('SpecifiedTaxRegistration', TXMLTaxRegistrationType_ram);
  FID := CreateCollection(TXMLIDType_udtList, IXMLIDType_udt, 'ID') as IXMLIDType_udtList;
  FGlobalID := CreateCollection(TXMLIDType_udtList, IXMLIDType_udt, 'GlobalID') as IXMLIDType_udtList;
  FDefinedTradeContact := CreateCollection(TXMLTradeContactType_ramList, IXMLTradeContactType_ram, 'DefinedTradeContact') as IXMLTradeContactType_ramList;
  FSpecifiedTaxRegistration := CreateCollection(TXMLTaxRegistrationType_ramList, IXMLTaxRegistrationType_ram, 'SpecifiedTaxRegistration') as IXMLTaxRegistrationType_ramList;
  inherited;
end;

function TXMLTradePartyType_ram.Get_ID: IXMLIDType_udtList;
begin
  Result := FID;
end;

function TXMLTradePartyType_ram.Get_GlobalID: IXMLIDType_udtList;
begin
  Result := FGlobalID;
end;

function TXMLTradePartyType_ram.Get_Name: IXMLTextType_udt;
begin
  Result := ChildNodes['Name'] as IXMLTextType_udt;
end;

function TXMLTradePartyType_ram.Get_DefinedTradeContact: IXMLTradeContactType_ramList;
begin
  Result := FDefinedTradeContact;
end;

function TXMLTradePartyType_ram.Get_PostalTradeAddress: IXMLTradeAddressType_ram;
begin
  Result := ChildNodes['PostalTradeAddress'] as IXMLTradeAddressType_ram;
end;

function TXMLTradePartyType_ram.Get_SpecifiedTaxRegistration: IXMLTaxRegistrationType_ramList;
begin
  Result := FSpecifiedTaxRegistration;
end;

{ TXMLTradePartyType_ramList }

function TXMLTradePartyType_ramList.Add: IXMLTradePartyType_ram;
begin
  Result := AddItem(-1) as IXMLTradePartyType_ram;
end;

function TXMLTradePartyType_ramList.Insert(const Index: Integer): IXMLTradePartyType_ram;
begin
  Result := AddItem(Index) as IXMLTradePartyType_ram;
end;

function TXMLTradePartyType_ramList.Get_Item(Index: Integer): IXMLTradePartyType_ram;
begin
  Result := List[Index] as IXMLTradePartyType_ram;
end;

{ TXMLTradeContactType_ram }

procedure TXMLTradeContactType_ram.AfterConstruction;
begin
  RegisterChildNode('PersonName', TXMLTextType_udt);
  RegisterChildNode('DepartmentName', TXMLTextType_udt);
  RegisterChildNode('TelephoneUniversalCommunication', TXMLUniversalCommunicationType_ram);
  RegisterChildNode('FaxUniversalCommunication', TXMLUniversalCommunicationType_ram);
  RegisterChildNode('EmailURIUniversalCommunication', TXMLUniversalCommunicationType_ram);
  FTelephoneUniversalCommunication := CreateCollection(TXMLUniversalCommunicationType_ramList, IXMLUniversalCommunicationType_ram, 'TelephoneUniversalCommunication') as IXMLUniversalCommunicationType_ramList;
  FFaxUniversalCommunication := CreateCollection(TXMLUniversalCommunicationType_ramList, IXMLUniversalCommunicationType_ram, 'FaxUniversalCommunication') as IXMLUniversalCommunicationType_ramList;
  inherited;
end;

function TXMLTradeContactType_ram.Get_PersonName: IXMLTextType_udt;
begin
  Result := ChildNodes['PersonName'] as IXMLTextType_udt;
end;

function TXMLTradeContactType_ram.Get_DepartmentName: IXMLTextType_udt;
begin
  Result := ChildNodes['DepartmentName'] as IXMLTextType_udt;
end;

function TXMLTradeContactType_ram.Get_TelephoneUniversalCommunication: IXMLUniversalCommunicationType_ramList;
begin
  Result := FTelephoneUniversalCommunication;
end;

function TXMLTradeContactType_ram.Get_FaxUniversalCommunication: IXMLUniversalCommunicationType_ramList;
begin
  Result := FFaxUniversalCommunication;
end;

function TXMLTradeContactType_ram.Get_EmailURIUniversalCommunication: IXMLUniversalCommunicationType_ram;
begin
  Result := ChildNodes['EmailURIUniversalCommunication'] as IXMLUniversalCommunicationType_ram;
end;

{ TXMLTradeContactType_ramList }

function TXMLTradeContactType_ramList.Add: IXMLTradeContactType_ram;
begin
  Result := AddItem(-1) as IXMLTradeContactType_ram;
end;

function TXMLTradeContactType_ramList.Insert(const Index: Integer): IXMLTradeContactType_ram;
begin
  Result := AddItem(Index) as IXMLTradeContactType_ram;
end;

function TXMLTradeContactType_ramList.Get_Item(Index: Integer): IXMLTradeContactType_ram;
begin
  Result := List[Index] as IXMLTradeContactType_ram;
end;

{ TXMLUniversalCommunicationType_ram }

procedure TXMLUniversalCommunicationType_ram.AfterConstruction;
begin
  RegisterChildNode('URIID', TXMLIDType_udt);
  RegisterChildNode('CompleteNumber', TXMLTextType_udt);
  inherited;
end;

function TXMLUniversalCommunicationType_ram.Get_URIID: IXMLIDType_udt;
begin
  Result := ChildNodes['URIID'] as IXMLIDType_udt;
end;

function TXMLUniversalCommunicationType_ram.Get_CompleteNumber: IXMLTextType_udt;
begin
  Result := ChildNodes['CompleteNumber'] as IXMLTextType_udt;
end;

{ TXMLUniversalCommunicationType_ramList }

function TXMLUniversalCommunicationType_ramList.Add: IXMLUniversalCommunicationType_ram;
begin
  Result := AddItem(-1) as IXMLUniversalCommunicationType_ram;
end;

function TXMLUniversalCommunicationType_ramList.Insert(const Index: Integer): IXMLUniversalCommunicationType_ram;
begin
  Result := AddItem(Index) as IXMLUniversalCommunicationType_ram;
end;

function TXMLUniversalCommunicationType_ramList.Get_Item(Index: Integer): IXMLUniversalCommunicationType_ram;
begin
  Result := List[Index] as IXMLUniversalCommunicationType_ram;
end;

{ TXMLTradeAddressType_ram }

procedure TXMLTradeAddressType_ram.AfterConstruction;
begin
  RegisterChildNode('PostcodeCode', TXMLCodeType_udt);
  RegisterChildNode('LineOne', TXMLTextType_udt);
  RegisterChildNode('LineTwo', TXMLTextType_udt);
  RegisterChildNode('CityName', TXMLTextType_udt);
  RegisterChildNode('CountryID', TXMLCountryIDType_qdt);
  FPostcodeCode := CreateCollection(TXMLCodeType_udtList, IXMLCodeType_udt, 'PostcodeCode') as IXMLCodeType_udtList;
  inherited;
end;

function TXMLTradeAddressType_ram.Get_PostcodeCode: IXMLCodeType_udtList;
begin
  Result := FPostcodeCode;
end;

function TXMLTradeAddressType_ram.Get_LineOne: IXMLTextType_udt;
begin
  Result := ChildNodes['LineOne'] as IXMLTextType_udt;
end;

function TXMLTradeAddressType_ram.Get_LineTwo: IXMLTextType_udt;
begin
  Result := ChildNodes['LineTwo'] as IXMLTextType_udt;
end;

function TXMLTradeAddressType_ram.Get_CityName: IXMLTextType_udt;
begin
  Result := ChildNodes['CityName'] as IXMLTextType_udt;
end;

function TXMLTradeAddressType_ram.Get_CountryID: IXMLCountryIDType_qdt;
begin
  Result := ChildNodes['CountryID'] as IXMLCountryIDType_qdt;
end;

{ TXMLCountryIDType_qdt }

{ TXMLTaxRegistrationType_ram }

procedure TXMLTaxRegistrationType_ram.AfterConstruction;
begin
  RegisterChildNode('ID', TXMLIDType_udt);
  inherited;
end;

function TXMLTaxRegistrationType_ram.Get_ID: IXMLIDType_udt;
begin
  Result := ChildNodes['ID'] as IXMLIDType_udt;
end;

{ TXMLTaxRegistrationType_ramList }

function TXMLTaxRegistrationType_ramList.Add: IXMLTaxRegistrationType_ram;
begin
  Result := AddItem(-1) as IXMLTaxRegistrationType_ram;
end;

function TXMLTaxRegistrationType_ramList.Insert(const Index: Integer): IXMLTaxRegistrationType_ram;
begin
  Result := AddItem(Index) as IXMLTaxRegistrationType_ram;
end;

function TXMLTaxRegistrationType_ramList.Get_Item(Index: Integer): IXMLTaxRegistrationType_ram;
begin
  Result := List[Index] as IXMLTaxRegistrationType_ram;
end;

{ TXMLTradeDeliveryTermsType_ram }

procedure TXMLTradeDeliveryTermsType_ram.AfterConstruction;
begin
  RegisterChildNode('DeliveryTypeCode', TXMLDeliveryTermsCodeType_qdt);
  inherited;
end;

function TXMLTradeDeliveryTermsType_ram.Get_DeliveryTypeCode: IXMLDeliveryTermsCodeType_qdt;
begin
  Result := ChildNodes['DeliveryTypeCode'] as IXMLDeliveryTermsCodeType_qdt;
end;

{ TXMLDeliveryTermsCodeType_qdt }

{ TXMLReferencedDocumentType_ram }

procedure TXMLReferencedDocumentType_ram.AfterConstruction;
begin
  RegisterChildNode('LineID', TXMLIDType_udt);
  RegisterChildNode('TypeCode', TXMLDocumentCodeType_qdt);
  RegisterChildNode('ID', TXMLIDType_udt);
  RegisterChildNode('ReferenceTypeCode', TXMLReferenceCodeType_qdt);
  FID := CreateCollection(TXMLIDType_udtList, IXMLIDType_udt, 'ID') as IXMLIDType_udtList;
  inherited;
end;

function TXMLReferencedDocumentType_ram.Get_IssueDateTime: UnicodeString;
begin
  Result := ChildNodes['IssueDateTime'].Text;
end;

procedure TXMLReferencedDocumentType_ram.Set_IssueDateTime(Value: UnicodeString);
begin
  ChildNodes['IssueDateTime'].NodeValue := Value;
end;

function TXMLReferencedDocumentType_ram.Get_LineID: IXMLIDType_udt;
begin
  Result := ChildNodes['LineID'] as IXMLIDType_udt;
end;

function TXMLReferencedDocumentType_ram.Get_TypeCode: IXMLDocumentCodeType_qdt;
begin
  Result := ChildNodes['TypeCode'] as IXMLDocumentCodeType_qdt;
end;

function TXMLReferencedDocumentType_ram.Get_ID: IXMLIDType_udtList;
begin
  Result := FID;
end;

function TXMLReferencedDocumentType_ram.Get_ReferenceTypeCode: IXMLReferenceCodeType_qdt;
begin
  Result := ChildNodes['ReferenceTypeCode'] as IXMLReferenceCodeType_qdt;
end;

{ TXMLReferencedDocumentType_ramList }

function TXMLReferencedDocumentType_ramList.Add: IXMLReferencedDocumentType_ram;
begin
  Result := AddItem(-1) as IXMLReferencedDocumentType_ram;
end;

function TXMLReferencedDocumentType_ramList.Insert(const Index: Integer): IXMLReferencedDocumentType_ram;
begin
  Result := AddItem(Index) as IXMLReferencedDocumentType_ram;
end;

function TXMLReferencedDocumentType_ramList.Get_Item(Index: Integer): IXMLReferencedDocumentType_ram;
begin
  Result := List[Index] as IXMLReferencedDocumentType_ram;
end;

{ TXMLReferenceCodeType_qdt }

{ TXMLTradePriceType_ram }

procedure TXMLTradePriceType_ram.AfterConstruction;
begin
  RegisterChildNode('ChargeAmount', TXMLAmountType_udt);
  RegisterChildNode('BasisQuantity', TXMLQuantityType_udt);
  RegisterChildNode('AppliedTradeAllowanceCharge', TXMLTradeAllowanceChargeType_ram);
  FChargeAmount := CreateCollection(TXMLAmountType_udtList, IXMLAmountType_udt, 'ChargeAmount') as IXMLAmountType_udtList;
  FAppliedTradeAllowanceCharge := CreateCollection(TXMLTradeAllowanceChargeType_ramList, IXMLTradeAllowanceChargeType_ram, 'AppliedTradeAllowanceCharge') as IXMLTradeAllowanceChargeType_ramList;
  inherited;
end;

function TXMLTradePriceType_ram.Get_ChargeAmount: IXMLAmountType_udtList;
begin
  Result := FChargeAmount;
end;

function TXMLTradePriceType_ram.Get_BasisQuantity: IXMLQuantityType_udt;
begin
  Result := ChildNodes['BasisQuantity'] as IXMLQuantityType_udt;
end;

function TXMLTradePriceType_ram.Get_AppliedTradeAllowanceCharge: IXMLTradeAllowanceChargeType_ramList;
begin
  Result := FAppliedTradeAllowanceCharge;
end;

{ TXMLTradePriceType_ramList }

function TXMLTradePriceType_ramList.Add: IXMLTradePriceType_ram;
begin
  Result := AddItem(-1) as IXMLTradePriceType_ram;
end;

function TXMLTradePriceType_ramList.Insert(const Index: Integer): IXMLTradePriceType_ram;
begin
  Result := AddItem(Index) as IXMLTradePriceType_ram;
end;

function TXMLTradePriceType_ramList.Get_Item(Index: Integer): IXMLTradePriceType_ram;
begin
  Result := List[Index] as IXMLTradePriceType_ram;
end;

{ TXMLAmountType_udt }

function TXMLAmountType_udt.Get_CurrencyID: UnicodeString;
begin
  Result := AttributeNodes['currencyID'].Text;
end;

procedure TXMLAmountType_udt.Set_CurrencyID(Value: UnicodeString);
begin
  SetAttribute('currencyID', Value);
end;

{ TXMLAmountType_udtList }

function TXMLAmountType_udtList.Add: IXMLAmountType_udt;
begin
  Result := AddItem(-1) as IXMLAmountType_udt;
end;

function TXMLAmountType_udtList.Insert(const Index: Integer): IXMLAmountType_udt;
begin
  Result := AddItem(Index) as IXMLAmountType_udt;
end;

function TXMLAmountType_udtList.Get_Item(Index: Integer): IXMLAmountType_udt;
begin
  Result := List[Index] as IXMLAmountType_udt;
end;

{ TXMLQuantityType_udt }

function TXMLQuantityType_udt.Get_UnitCode: UnicodeString;
begin
  Result := AttributeNodes['unitCode'].Text;
end;

procedure TXMLQuantityType_udt.Set_UnitCode(Value: UnicodeString);
begin
  SetAttribute('unitCode', Value);
end;

{ TXMLQuantityType_udtList }

function TXMLQuantityType_udtList.Add: IXMLQuantityType_udt;
begin
  Result := AddItem(-1) as IXMLQuantityType_udt;
end;

function TXMLQuantityType_udtList.Insert(const Index: Integer): IXMLQuantityType_udt;
begin
  Result := AddItem(Index) as IXMLQuantityType_udt;
end;

function TXMLQuantityType_udtList.Get_Item(Index: Integer): IXMLQuantityType_udt;
begin
  Result := List[Index] as IXMLQuantityType_udt;
end;

{ TXMLTradeAllowanceChargeType_ram }

procedure TXMLTradeAllowanceChargeType_ram.AfterConstruction;
begin
  RegisterChildNode('ChargeIndicator', TXMLIndicatorType_udt);
  RegisterChildNode('SequenceNumeric', TXMLNumericType_udt);
  RegisterChildNode('CalculationPercent', TXMLPercentType_udt);
  RegisterChildNode('BasisAmount', TXMLAmountType_udt);
  RegisterChildNode('BasisQuantity', TXMLQuantityType_udt);
  RegisterChildNode('ActualAmount', TXMLAmountType_udt);
  RegisterChildNode('ReasonCode', TXMLAllowanceChargeReasonCodeType_qdt);
  RegisterChildNode('Reason', TXMLTextType_udt);
  RegisterChildNode('CategoryTradeTax', TXMLTradeTaxType_ram);
  FActualAmount := CreateCollection(TXMLAmountType_udtList, IXMLAmountType_udt, 'ActualAmount') as IXMLAmountType_udtList;
  FCategoryTradeTax := CreateCollection(TXMLTradeTaxType_ramList, IXMLTradeTaxType_ram, 'CategoryTradeTax') as IXMLTradeTaxType_ramList;
  inherited;
end;

function TXMLTradeAllowanceChargeType_ram.Get_ChargeIndicator: IXMLIndicatorType_udt;
begin
  Result := ChildNodes['ChargeIndicator'] as IXMLIndicatorType_udt;
end;

function TXMLTradeAllowanceChargeType_ram.Get_SequenceNumeric: IXMLNumericType_udt;
begin
  Result := ChildNodes['SequenceNumeric'] as IXMLNumericType_udt;
end;

function TXMLTradeAllowanceChargeType_ram.Get_CalculationPercent: IXMLPercentType_udt;
begin
  Result := ChildNodes['CalculationPercent'] as IXMLPercentType_udt;
end;

function TXMLTradeAllowanceChargeType_ram.Get_BasisAmount: IXMLAmountType_udt;
begin
  Result := ChildNodes['BasisAmount'] as IXMLAmountType_udt;
end;

function TXMLTradeAllowanceChargeType_ram.Get_BasisQuantity: IXMLQuantityType_udt;
begin
  Result := ChildNodes['BasisQuantity'] as IXMLQuantityType_udt;
end;

function TXMLTradeAllowanceChargeType_ram.Get_ActualAmount: IXMLAmountType_udtList;
begin
  Result := FActualAmount;
end;

function TXMLTradeAllowanceChargeType_ram.Get_ReasonCode: IXMLAllowanceChargeReasonCodeType_qdt;
begin
  Result := ChildNodes['ReasonCode'] as IXMLAllowanceChargeReasonCodeType_qdt;
end;

function TXMLTradeAllowanceChargeType_ram.Get_Reason: IXMLTextType_udt;
begin
  Result := ChildNodes['Reason'] as IXMLTextType_udt;
end;

function TXMLTradeAllowanceChargeType_ram.Get_CategoryTradeTax: IXMLTradeTaxType_ramList;
begin
  Result := FCategoryTradeTax;
end;

{ TXMLTradeAllowanceChargeType_ramList }

function TXMLTradeAllowanceChargeType_ramList.Add: IXMLTradeAllowanceChargeType_ram;
begin
  Result := AddItem(-1) as IXMLTradeAllowanceChargeType_ram;
end;

function TXMLTradeAllowanceChargeType_ramList.Insert(const Index: Integer): IXMLTradeAllowanceChargeType_ram;
begin
  Result := AddItem(Index) as IXMLTradeAllowanceChargeType_ram;
end;

function TXMLTradeAllowanceChargeType_ramList.Get_Item(Index: Integer): IXMLTradeAllowanceChargeType_ram;
begin
  Result := List[Index] as IXMLTradeAllowanceChargeType_ram;
end;

{ TXMLNumericType_udt }

{ TXMLPercentType_udt }

{ TXMLAllowanceChargeReasonCodeType_qdt }

{ TXMLTradeTaxType_ram }

procedure TXMLTradeTaxType_ram.AfterConstruction;
begin
  RegisterChildNode('CalculatedAmount', TXMLAmountType_udt);
  RegisterChildNode('TypeCode', TXMLTaxTypeCodeType_qdt);
  RegisterChildNode('ExemptionReason', TXMLTextType_udt);
  RegisterChildNode('BasisAmount', TXMLAmountType_udt);
  RegisterChildNode('LineTotalBasisAmount', TXMLAmountType_udt);
  RegisterChildNode('AllowanceChargeBasisAmount', TXMLAmountType_udt);
  RegisterChildNode('CategoryCode', TXMLTaxCategoryCodeType_qdt);
  RegisterChildNode('ApplicablePercent', TXMLPercentType_udt);
  FCalculatedAmount := CreateCollection(TXMLAmountType_udtList, IXMLAmountType_udt, 'CalculatedAmount') as IXMLAmountType_udtList;
  FBasisAmount := CreateCollection(TXMLAmountType_udtList, IXMLAmountType_udt, 'BasisAmount') as IXMLAmountType_udtList;
  FLineTotalBasisAmount := CreateCollection(TXMLAmountType_udtList, IXMLAmountType_udt, 'LineTotalBasisAmount') as IXMLAmountType_udtList;
  FAllowanceChargeBasisAmount := CreateCollection(TXMLAmountType_udtList, IXMLAmountType_udt, 'AllowanceChargeBasisAmount') as IXMLAmountType_udtList;
  inherited;
end;

function TXMLTradeTaxType_ram.Get_CalculatedAmount: IXMLAmountType_udtList;
begin
  Result := FCalculatedAmount;
end;

function TXMLTradeTaxType_ram.Get_TypeCode: IXMLTaxTypeCodeType_qdt;
begin
  Result := ChildNodes['TypeCode'] as IXMLTaxTypeCodeType_qdt;
end;

function TXMLTradeTaxType_ram.Get_ExemptionReason: IXMLTextType_udt;
begin
  Result := ChildNodes['ExemptionReason'] as IXMLTextType_udt;
end;

function TXMLTradeTaxType_ram.Get_BasisAmount: IXMLAmountType_udtList;
begin
  Result := FBasisAmount;
end;

function TXMLTradeTaxType_ram.Get_LineTotalBasisAmount: IXMLAmountType_udtList;
begin
  Result := FLineTotalBasisAmount;
end;

function TXMLTradeTaxType_ram.Get_AllowanceChargeBasisAmount: IXMLAmountType_udtList;
begin
  Result := FAllowanceChargeBasisAmount;
end;

function TXMLTradeTaxType_ram.Get_CategoryCode: IXMLTaxCategoryCodeType_qdt;
begin
  Result := ChildNodes['CategoryCode'] as IXMLTaxCategoryCodeType_qdt;
end;

function TXMLTradeTaxType_ram.Get_ApplicablePercent: IXMLPercentType_udt;
begin
  Result := ChildNodes['ApplicablePercent'] as IXMLPercentType_udt;
end;

{ TXMLTradeTaxType_ramList }

function TXMLTradeTaxType_ramList.Add: IXMLTradeTaxType_ram;
begin
  Result := AddItem(-1) as IXMLTradeTaxType_ram;
end;

function TXMLTradeTaxType_ramList.Insert(const Index: Integer): IXMLTradeTaxType_ram;
begin
  Result := AddItem(Index) as IXMLTradeTaxType_ram;
end;

function TXMLTradeTaxType_ramList.Get_Item(Index: Integer): IXMLTradeTaxType_ram;
begin
  Result := List[Index] as IXMLTradeTaxType_ram;
end;

{ TXMLTaxTypeCodeType_qdt }

{ TXMLTaxCategoryCodeType_qdt }

{ TXMLSupplyChainTradeDeliveryType_ram }

procedure TXMLSupplyChainTradeDeliveryType_ram.AfterConstruction;
begin
  RegisterChildNode('BilledQuantity', TXMLQuantityType_udt);
  RegisterChildNode('ChargeFreeQuantity', TXMLQuantityType_udt);
  RegisterChildNode('PackageQuantity', TXMLQuantityType_udt);
  RegisterChildNode('RelatedSupplyChainConsignment', TXMLSupplyChainConsignmentType_ram);
  RegisterChildNode('ShipToTradeParty', TXMLTradePartyType_ram);
  RegisterChildNode('UltimateShipToTradeParty', TXMLTradePartyType_ram);
  RegisterChildNode('ShipFromTradeParty', TXMLTradePartyType_ram);
  RegisterChildNode('ActualDeliverySupplyChainEvent', TXMLSupplyChainEventType_ram);
  RegisterChildNode('DespatchAdviceReferencedDocument', TXMLReferencedDocumentType_ram);
  RegisterChildNode('ReceivingAdviceReferencedDocument', TXMLReferencedDocumentType_ram);
  RegisterChildNode('DeliveryNoteReferencedDocument', TXMLReferencedDocumentType_ram);
  FRelatedSupplyChainConsignment := CreateCollection(TXMLSupplyChainConsignmentType_ramList, IXMLSupplyChainConsignmentType_ram, 'RelatedSupplyChainConsignment') as IXMLSupplyChainConsignmentType_ramList;
  FActualDeliverySupplyChainEvent := CreateCollection(TXMLSupplyChainEventType_ramList, IXMLSupplyChainEventType_ram, 'ActualDeliverySupplyChainEvent') as IXMLSupplyChainEventType_ramList;
  FReceivingAdviceReferencedDocument := CreateCollection(TXMLReferencedDocumentType_ramList, IXMLReferencedDocumentType_ram, 'ReceivingAdviceReferencedDocument') as IXMLReferencedDocumentType_ramList;
  inherited;
end;

function TXMLSupplyChainTradeDeliveryType_ram.Get_BilledQuantity: IXMLQuantityType_udt;
begin
  Result := ChildNodes['BilledQuantity'] as IXMLQuantityType_udt;
end;

function TXMLSupplyChainTradeDeliveryType_ram.Get_ChargeFreeQuantity: IXMLQuantityType_udt;
begin
  Result := ChildNodes['ChargeFreeQuantity'] as IXMLQuantityType_udt;
end;

function TXMLSupplyChainTradeDeliveryType_ram.Get_PackageQuantity: IXMLQuantityType_udt;
begin
  Result := ChildNodes['PackageQuantity'] as IXMLQuantityType_udt;
end;

function TXMLSupplyChainTradeDeliveryType_ram.Get_RelatedSupplyChainConsignment: IXMLSupplyChainConsignmentType_ramList;
begin
  Result := FRelatedSupplyChainConsignment;
end;

function TXMLSupplyChainTradeDeliveryType_ram.Get_ShipToTradeParty: IXMLTradePartyType_ram;
begin
  Result := ChildNodes['ShipToTradeParty'] as IXMLTradePartyType_ram;
end;

function TXMLSupplyChainTradeDeliveryType_ram.Get_UltimateShipToTradeParty: IXMLTradePartyType_ram;
begin
  Result := ChildNodes['UltimateShipToTradeParty'] as IXMLTradePartyType_ram;
end;

function TXMLSupplyChainTradeDeliveryType_ram.Get_ShipFromTradeParty: IXMLTradePartyType_ram;
begin
  Result := ChildNodes['ShipFromTradeParty'] as IXMLTradePartyType_ram;
end;

function TXMLSupplyChainTradeDeliveryType_ram.Get_ActualDeliverySupplyChainEvent: IXMLSupplyChainEventType_ramList;
begin
  Result := FActualDeliverySupplyChainEvent;
end;

function TXMLSupplyChainTradeDeliveryType_ram.Get_DespatchAdviceReferencedDocument: IXMLReferencedDocumentType_ram;
begin
  Result := ChildNodes['DespatchAdviceReferencedDocument'] as IXMLReferencedDocumentType_ram;
end;

function TXMLSupplyChainTradeDeliveryType_ram.Get_ReceivingAdviceReferencedDocument: IXMLReferencedDocumentType_ramList;
begin
  Result := FReceivingAdviceReferencedDocument;
end;

function TXMLSupplyChainTradeDeliveryType_ram.Get_DeliveryNoteReferencedDocument: IXMLReferencedDocumentType_ram;
begin
  Result := ChildNodes['DeliveryNoteReferencedDocument'] as IXMLReferencedDocumentType_ram;
end;

{ TXMLSupplyChainConsignmentType_ram }

procedure TXMLSupplyChainConsignmentType_ram.AfterConstruction;
begin
  RegisterChildNode('SpecifiedLogisticsTransportMovement', TXMLLogisticsTransportMovementType_ram);
  ItemTag := 'SpecifiedLogisticsTransportMovement';
  ItemInterface := IXMLLogisticsTransportMovementType_ram;
  inherited;
end;

function TXMLSupplyChainConsignmentType_ram.Get_SpecifiedLogisticsTransportMovement(Index: Integer): IXMLLogisticsTransportMovementType_ram;
begin
  Result := List[Index] as IXMLLogisticsTransportMovementType_ram;
end;

function TXMLSupplyChainConsignmentType_ram.Add: IXMLLogisticsTransportMovementType_ram;
begin
  Result := AddItem(-1) as IXMLLogisticsTransportMovementType_ram;
end;

function TXMLSupplyChainConsignmentType_ram.Insert(const Index: Integer): IXMLLogisticsTransportMovementType_ram;
begin
  Result := AddItem(Index) as IXMLLogisticsTransportMovementType_ram;
end;

{ TXMLSupplyChainConsignmentType_ramList }

function TXMLSupplyChainConsignmentType_ramList.Add: IXMLSupplyChainConsignmentType_ram;
begin
  Result := AddItem(-1) as IXMLSupplyChainConsignmentType_ram;
end;

function TXMLSupplyChainConsignmentType_ramList.Insert(const Index: Integer): IXMLSupplyChainConsignmentType_ram;
begin
  Result := AddItem(Index) as IXMLSupplyChainConsignmentType_ram;
end;

function TXMLSupplyChainConsignmentType_ramList.Get_Item(Index: Integer): IXMLSupplyChainConsignmentType_ram;
begin
  Result := List[Index] as IXMLSupplyChainConsignmentType_ram;
end;

{ TXMLLogisticsTransportMovementType_ram }

procedure TXMLLogisticsTransportMovementType_ram.AfterConstruction;
begin
  RegisterChildNode('ModeCode', TXMLCodeType_udt);
  RegisterChildNode('ID', TXMLIDType_udt);
  inherited;
end;

function TXMLLogisticsTransportMovementType_ram.Get_ModeCode: IXMLCodeType_udt;
begin
  Result := ChildNodes['ModeCode'] as IXMLCodeType_udt;
end;

function TXMLLogisticsTransportMovementType_ram.Get_ID: IXMLIDType_udt;
begin
  Result := ChildNodes['ID'] as IXMLIDType_udt;
end;

{ TXMLSupplyChainEventType_ram }

procedure TXMLSupplyChainEventType_ram.AfterConstruction;
begin
  RegisterChildNode('OccurrenceDateTime', TXMLDateTimeType_udt);
  ItemTag := 'OccurrenceDateTime';
  ItemInterface := IXMLDateTimeType_udt;
  inherited;
end;

function TXMLSupplyChainEventType_ram.Get_OccurrenceDateTime(Index: Integer): IXMLDateTimeType_udt;
begin
  Result := List[Index] as IXMLDateTimeType_udt;
end;

function TXMLSupplyChainEventType_ram.Add: IXMLDateTimeType_udt;
begin
  Result := AddItem(-1) as IXMLDateTimeType_udt;
end;

function TXMLSupplyChainEventType_ram.Insert(const Index: Integer): IXMLDateTimeType_udt;
begin
  Result := AddItem(Index) as IXMLDateTimeType_udt;
end;

{ TXMLSupplyChainEventType_ramList }

function TXMLSupplyChainEventType_ramList.Add: IXMLSupplyChainEventType_ram;
begin
  Result := AddItem(-1) as IXMLSupplyChainEventType_ram;
end;

function TXMLSupplyChainEventType_ramList.Insert(const Index: Integer): IXMLSupplyChainEventType_ram;
begin
  Result := AddItem(Index) as IXMLSupplyChainEventType_ram;
end;

function TXMLSupplyChainEventType_ramList.Get_Item(Index: Integer): IXMLSupplyChainEventType_ram;
begin
  Result := List[Index] as IXMLSupplyChainEventType_ram;
end;

{ TXMLSupplyChainTradeSettlementType_ram }

procedure TXMLSupplyChainTradeSettlementType_ram.AfterConstruction;
begin
  RegisterChildNode('PaymentReference', TXMLTextType_udt);
  RegisterChildNode('InvoiceCurrencyCode', TXMLCodeType_udt);
  RegisterChildNode('InvoiceeTradeParty', TXMLTradePartyType_ram);
  RegisterChildNode('PayeeTradeParty', TXMLTradePartyType_ram);
  RegisterChildNode('SpecifiedTradeSettlementPaymentMeans', TXMLTradeSettlementPaymentMeansType_ram);
  RegisterChildNode('ApplicableTradeTax', TXMLTradeTaxType_ram);
  RegisterChildNode('BillingSpecifiedPeriod', TXMLSpecifiedPeriodType_ram);
  RegisterChildNode('SpecifiedTradeAllowanceCharge', TXMLTradeAllowanceChargeType_ram);
  RegisterChildNode('SpecifiedLogisticsServiceCharge', TXMLLogisticsServiceChargeType_ram);
  RegisterChildNode('SpecifiedTradePaymentTerms', TXMLTradePaymentTermsType_ram);
  RegisterChildNode('SpecifiedTradeAccountingAccount', TXMLTradeAccountingAccountType_ram);
  RegisterChildNode('SpecifiedTradeSettlementMonetarySummation', TXMLTradeSettlementMonetarySummationType_ram);
  RegisterChildNode('ReceivableSpecifiedTradeAccountingAccount', TXMLTradeAccountingAccountType_ram);
  FPaymentReference := CreateCollection(TXMLTextType_udtList, IXMLTextType_udt, 'PaymentReference') as IXMLTextType_udtList;
  FPayeeTradeParty := CreateCollection(TXMLTradePartyType_ramList, IXMLTradePartyType_ram, 'PayeeTradeParty') as IXMLTradePartyType_ramList;
  FSpecifiedTradeSettlementPaymentMeans := CreateCollection(TXMLTradeSettlementPaymentMeansType_ramList, IXMLTradeSettlementPaymentMeansType_ram, 'SpecifiedTradeSettlementPaymentMeans') as IXMLTradeSettlementPaymentMeansType_ramList;
  FApplicableTradeTax := CreateCollection(TXMLTradeTaxType_ramList, IXMLTradeTaxType_ram, 'ApplicableTradeTax') as IXMLTradeTaxType_ramList;
  FBillingSpecifiedPeriod := CreateCollection(TXMLSpecifiedPeriodType_ramList, IXMLSpecifiedPeriodType_ram, 'BillingSpecifiedPeriod') as IXMLSpecifiedPeriodType_ramList;
  FSpecifiedTradeAllowanceCharge := CreateCollection(TXMLTradeAllowanceChargeType_ramList, IXMLTradeAllowanceChargeType_ram, 'SpecifiedTradeAllowanceCharge') as IXMLTradeAllowanceChargeType_ramList;
  FSpecifiedLogisticsServiceCharge := CreateCollection(TXMLLogisticsServiceChargeType_ramList, IXMLLogisticsServiceChargeType_ram, 'SpecifiedLogisticsServiceCharge') as IXMLLogisticsServiceChargeType_ramList;
  FSpecifiedTradePaymentTerms := CreateCollection(TXMLTradePaymentTermsType_ramList, IXMLTradePaymentTermsType_ram, 'SpecifiedTradePaymentTerms') as IXMLTradePaymentTermsType_ramList;
  FSpecifiedTradeAccountingAccount := CreateCollection(TXMLTradeAccountingAccountType_ramList, IXMLTradeAccountingAccountType_ram, 'SpecifiedTradeAccountingAccount') as IXMLTradeAccountingAccountType_ramList;
  FReceivableSpecifiedTradeAccountingAccount := CreateCollection(TXMLTradeAccountingAccountType_ramList, IXMLTradeAccountingAccountType_ram, 'ReceivableSpecifiedTradeAccountingAccount') as IXMLTradeAccountingAccountType_ramList;
  inherited;
end;

function TXMLSupplyChainTradeSettlementType_ram.Get_PaymentReference: IXMLTextType_udtList;
begin
  Result := FPaymentReference;
end;

function TXMLSupplyChainTradeSettlementType_ram.Get_InvoiceCurrencyCode: IXMLCodeType_udt;
begin
  Result := ChildNodes['InvoiceCurrencyCode'] as IXMLCodeType_udt;
end;

function TXMLSupplyChainTradeSettlementType_ram.Get_InvoiceeTradeParty: IXMLTradePartyType_ram;
begin
  Result := ChildNodes['InvoiceeTradeParty'] as IXMLTradePartyType_ram;
end;

function TXMLSupplyChainTradeSettlementType_ram.Get_PayeeTradeParty: IXMLTradePartyType_ramList;
begin
  Result := FPayeeTradeParty;
end;

function TXMLSupplyChainTradeSettlementType_ram.Get_SpecifiedTradeSettlementPaymentMeans: IXMLTradeSettlementPaymentMeansType_ramList;
begin
  Result := FSpecifiedTradeSettlementPaymentMeans;
end;

function TXMLSupplyChainTradeSettlementType_ram.Get_ApplicableTradeTax: IXMLTradeTaxType_ramList;
begin
  Result := FApplicableTradeTax;
end;

function TXMLSupplyChainTradeSettlementType_ram.Get_BillingSpecifiedPeriod: IXMLSpecifiedPeriodType_ramList;
begin
  Result := FBillingSpecifiedPeriod;
end;

function TXMLSupplyChainTradeSettlementType_ram.Get_SpecifiedTradeAllowanceCharge: IXMLTradeAllowanceChargeType_ramList;
begin
  Result := FSpecifiedTradeAllowanceCharge;
end;

function TXMLSupplyChainTradeSettlementType_ram.Get_SpecifiedLogisticsServiceCharge: IXMLLogisticsServiceChargeType_ramList;
begin
  Result := FSpecifiedLogisticsServiceCharge;
end;

function TXMLSupplyChainTradeSettlementType_ram.Get_SpecifiedTradePaymentTerms: IXMLTradePaymentTermsType_ramList;
begin
  Result := FSpecifiedTradePaymentTerms;
end;

function TXMLSupplyChainTradeSettlementType_ram.Get_SpecifiedTradeAccountingAccount: IXMLTradeAccountingAccountType_ramList;
begin
  Result := FSpecifiedTradeAccountingAccount;
end;

function TXMLSupplyChainTradeSettlementType_ram.Get_SpecifiedTradeSettlementMonetarySummation: IXMLTradeSettlementMonetarySummationType_ram;
begin
  Result := ChildNodes['SpecifiedTradeSettlementMonetarySummation'] as IXMLTradeSettlementMonetarySummationType_ram;
end;

function TXMLSupplyChainTradeSettlementType_ram.Get_ReceivableSpecifiedTradeAccountingAccount: IXMLTradeAccountingAccountType_ramList;
begin
  Result := FReceivableSpecifiedTradeAccountingAccount;
end;

{ TXMLTradeSettlementPaymentMeansType_ram }

procedure TXMLTradeSettlementPaymentMeansType_ram.AfterConstruction;
begin
  RegisterChildNode('TypeCode', TXMLPaymentMeansCodeType_qdt);
  RegisterChildNode('Information', TXMLTextType_udt);
  RegisterChildNode('ID', TXMLIDType_udt);
  RegisterChildNode('PayerPartyDebtorFinancialAccount', TXMLDebtorFinancialAccountType_ram);
  RegisterChildNode('PayeePartyCreditorFinancialAccount', TXMLCreditorFinancialAccountType_ram);
  RegisterChildNode('PayerSpecifiedDebtorFinancialInstitution', TXMLDebtorFinancialInstitutionType_ram);
  RegisterChildNode('PayeeSpecifiedCreditorFinancialInstitution', TXMLCreditorFinancialInstitutionType_ram);
  FInformation := CreateCollection(TXMLTextType_udtList, IXMLTextType_udt, 'Information') as IXMLTextType_udtList;
  FID := CreateCollection(TXMLIDType_udtList, IXMLIDType_udt, 'ID') as IXMLIDType_udtList;
  inherited;
end;

function TXMLTradeSettlementPaymentMeansType_ram.Get_TypeCode: IXMLPaymentMeansCodeType_qdt;
begin
  Result := ChildNodes['TypeCode'] as IXMLPaymentMeansCodeType_qdt;
end;

function TXMLTradeSettlementPaymentMeansType_ram.Get_Information: IXMLTextType_udtList;
begin
  Result := FInformation;
end;

function TXMLTradeSettlementPaymentMeansType_ram.Get_ID: IXMLIDType_udtList;
begin
  Result := FID;
end;

function TXMLTradeSettlementPaymentMeansType_ram.Get_PayerPartyDebtorFinancialAccount: IXMLDebtorFinancialAccountType_ram;
begin
  Result := ChildNodes['PayerPartyDebtorFinancialAccount'] as IXMLDebtorFinancialAccountType_ram;
end;

function TXMLTradeSettlementPaymentMeansType_ram.Get_PayeePartyCreditorFinancialAccount: IXMLCreditorFinancialAccountType_ram;
begin
  Result := ChildNodes['PayeePartyCreditorFinancialAccount'] as IXMLCreditorFinancialAccountType_ram;
end;

function TXMLTradeSettlementPaymentMeansType_ram.Get_PayerSpecifiedDebtorFinancialInstitution: IXMLDebtorFinancialInstitutionType_ram;
begin
  Result := ChildNodes['PayerSpecifiedDebtorFinancialInstitution'] as IXMLDebtorFinancialInstitutionType_ram;
end;

function TXMLTradeSettlementPaymentMeansType_ram.Get_PayeeSpecifiedCreditorFinancialInstitution: IXMLCreditorFinancialInstitutionType_ram;
begin
  Result := ChildNodes['PayeeSpecifiedCreditorFinancialInstitution'] as IXMLCreditorFinancialInstitutionType_ram;
end;

{ TXMLTradeSettlementPaymentMeansType_ramList }

function TXMLTradeSettlementPaymentMeansType_ramList.Add: IXMLTradeSettlementPaymentMeansType_ram;
begin
  Result := AddItem(-1) as IXMLTradeSettlementPaymentMeansType_ram;
end;

function TXMLTradeSettlementPaymentMeansType_ramList.Insert(const Index: Integer): IXMLTradeSettlementPaymentMeansType_ram;
begin
  Result := AddItem(Index) as IXMLTradeSettlementPaymentMeansType_ram;
end;

function TXMLTradeSettlementPaymentMeansType_ramList.Get_Item(Index: Integer): IXMLTradeSettlementPaymentMeansType_ram;
begin
  Result := List[Index] as IXMLTradeSettlementPaymentMeansType_ram;
end;

{ TXMLPaymentMeansCodeType_qdt }

{ TXMLDebtorFinancialAccountType_ram }

procedure TXMLDebtorFinancialAccountType_ram.AfterConstruction;
begin
  RegisterChildNode('IBANID', TXMLIDType_udt);
  RegisterChildNode('ProprietaryID', TXMLIDType_udt);
  inherited;
end;

function TXMLDebtorFinancialAccountType_ram.Get_IBANID: IXMLIDType_udt;
begin
  Result := ChildNodes['IBANID'] as IXMLIDType_udt;
end;

function TXMLDebtorFinancialAccountType_ram.Get_ProprietaryID: IXMLIDType_udt;
begin
  Result := ChildNodes['ProprietaryID'] as IXMLIDType_udt;
end;

{ TXMLCreditorFinancialAccountType_ram }

procedure TXMLCreditorFinancialAccountType_ram.AfterConstruction;
begin
  RegisterChildNode('IBANID', TXMLIDType_udt);
  RegisterChildNode('AccountName', TXMLTextType_udt);
  RegisterChildNode('ProprietaryID', TXMLIDType_udt);
  inherited;
end;

function TXMLCreditorFinancialAccountType_ram.Get_IBANID: IXMLIDType_udt;
begin
  Result := ChildNodes['IBANID'] as IXMLIDType_udt;
end;

function TXMLCreditorFinancialAccountType_ram.Get_AccountName: IXMLTextType_udt;
begin
  Result := ChildNodes['AccountName'] as IXMLTextType_udt;
end;

function TXMLCreditorFinancialAccountType_ram.Get_ProprietaryID: IXMLIDType_udt;
begin
  Result := ChildNodes['ProprietaryID'] as IXMLIDType_udt;
end;

{ TXMLDebtorFinancialInstitutionType_ram }

procedure TXMLDebtorFinancialInstitutionType_ram.AfterConstruction;
begin
  RegisterChildNode('BICID', TXMLIDType_udt);
  RegisterChildNode('GermanBankleitzahlID', TXMLIDType_udt);
  RegisterChildNode('Name', TXMLTextType_udt);
  inherited;
end;

function TXMLDebtorFinancialInstitutionType_ram.Get_BICID: IXMLIDType_udt;
begin
  Result := ChildNodes['BICID'] as IXMLIDType_udt;
end;

function TXMLDebtorFinancialInstitutionType_ram.Get_GermanBankleitzahlID: IXMLIDType_udt;
begin
  Result := ChildNodes['GermanBankleitzahlID'] as IXMLIDType_udt;
end;

function TXMLDebtorFinancialInstitutionType_ram.Get_Name: IXMLTextType_udt;
begin
  Result := ChildNodes['Name'] as IXMLTextType_udt;
end;

{ TXMLCreditorFinancialInstitutionType_ram }

procedure TXMLCreditorFinancialInstitutionType_ram.AfterConstruction;
begin
  RegisterChildNode('BICID', TXMLIDType_udt);
  RegisterChildNode('GermanBankleitzahlID', TXMLIDType_udt);
  RegisterChildNode('Name', TXMLTextType_udt);
  inherited;
end;

function TXMLCreditorFinancialInstitutionType_ram.Get_BICID: IXMLIDType_udt;
begin
  Result := ChildNodes['BICID'] as IXMLIDType_udt;
end;

function TXMLCreditorFinancialInstitutionType_ram.Get_GermanBankleitzahlID: IXMLIDType_udt;
begin
  Result := ChildNodes['GermanBankleitzahlID'] as IXMLIDType_udt;
end;

function TXMLCreditorFinancialInstitutionType_ram.Get_Name: IXMLTextType_udt;
begin
  Result := ChildNodes['Name'] as IXMLTextType_udt;
end;

{ TXMLLogisticsServiceChargeType_ram }

procedure TXMLLogisticsServiceChargeType_ram.AfterConstruction;
begin
  RegisterChildNode('Description', TXMLTextType_udt);
  RegisterChildNode('AppliedAmount', TXMLAmountType_udt);
  RegisterChildNode('AppliedTradeTax', TXMLTradeTaxType_ram);
  FDescription := CreateCollection(TXMLTextType_udtList, IXMLTextType_udt, 'Description') as IXMLTextType_udtList;
  FAppliedAmount := CreateCollection(TXMLAmountType_udtList, IXMLAmountType_udt, 'AppliedAmount') as IXMLAmountType_udtList;
  FAppliedTradeTax := CreateCollection(TXMLTradeTaxType_ramList, IXMLTradeTaxType_ram, 'AppliedTradeTax') as IXMLTradeTaxType_ramList;
  inherited;
end;

function TXMLLogisticsServiceChargeType_ram.Get_Description: IXMLTextType_udtList;
begin
  Result := FDescription;
end;

function TXMLLogisticsServiceChargeType_ram.Get_AppliedAmount: IXMLAmountType_udtList;
begin
  Result := FAppliedAmount;
end;

function TXMLLogisticsServiceChargeType_ram.Get_AppliedTradeTax: IXMLTradeTaxType_ramList;
begin
  Result := FAppliedTradeTax;
end;

{ TXMLLogisticsServiceChargeType_ramList }

function TXMLLogisticsServiceChargeType_ramList.Add: IXMLLogisticsServiceChargeType_ram;
begin
  Result := AddItem(-1) as IXMLLogisticsServiceChargeType_ram;
end;

function TXMLLogisticsServiceChargeType_ramList.Insert(const Index: Integer): IXMLLogisticsServiceChargeType_ram;
begin
  Result := AddItem(Index) as IXMLLogisticsServiceChargeType_ram;
end;

function TXMLLogisticsServiceChargeType_ramList.Get_Item(Index: Integer): IXMLLogisticsServiceChargeType_ram;
begin
  Result := List[Index] as IXMLLogisticsServiceChargeType_ram;
end;

{ TXMLTradePaymentTermsType_ram }

procedure TXMLTradePaymentTermsType_ram.AfterConstruction;
begin
  RegisterChildNode('Description', TXMLTextType_udt);
  RegisterChildNode('DueDateDateTime', TXMLDateTimeType_udt);
  RegisterChildNode('PartialPaymentAmount', TXMLAmountType_udt);
  RegisterChildNode('ApplicableTradePaymentPenaltyTerms', TXMLTradePaymentPenaltyTermsType_ram);
  RegisterChildNode('ApplicableTradePaymentDiscountTerms', TXMLTradePaymentDiscountTermsType_ram);
  FDescription := CreateCollection(TXMLTextType_udtList, IXMLTextType_udt, 'Description') as IXMLTextType_udtList;
  FPartialPaymentAmount := CreateCollection(TXMLAmountType_udtList, IXMLAmountType_udt, 'PartialPaymentAmount') as IXMLAmountType_udtList;
  FApplicableTradePaymentPenaltyTerms := CreateCollection(TXMLTradePaymentPenaltyTermsType_ramList, IXMLTradePaymentPenaltyTermsType_ram, 'ApplicableTradePaymentPenaltyTerms') as IXMLTradePaymentPenaltyTermsType_ramList;
  FApplicableTradePaymentDiscountTerms := CreateCollection(TXMLTradePaymentDiscountTermsType_ramList, IXMLTradePaymentDiscountTermsType_ram, 'ApplicableTradePaymentDiscountTerms') as IXMLTradePaymentDiscountTermsType_ramList;
  inherited;
end;

function TXMLTradePaymentTermsType_ram.Get_Description: IXMLTextType_udtList;
begin
  Result := FDescription;
end;

function TXMLTradePaymentTermsType_ram.Get_DueDateDateTime: IXMLDateTimeType_udt;
begin
  Result := ChildNodes['DueDateDateTime'] as IXMLDateTimeType_udt;
end;

function TXMLTradePaymentTermsType_ram.Get_PartialPaymentAmount: IXMLAmountType_udtList;
begin
  Result := FPartialPaymentAmount;
end;

function TXMLTradePaymentTermsType_ram.Get_ApplicableTradePaymentPenaltyTerms: IXMLTradePaymentPenaltyTermsType_ramList;
begin
  Result := FApplicableTradePaymentPenaltyTerms;
end;

function TXMLTradePaymentTermsType_ram.Get_ApplicableTradePaymentDiscountTerms: IXMLTradePaymentDiscountTermsType_ramList;
begin
  Result := FApplicableTradePaymentDiscountTerms;
end;

{ TXMLTradePaymentTermsType_ramList }

function TXMLTradePaymentTermsType_ramList.Add: IXMLTradePaymentTermsType_ram;
begin
  Result := AddItem(-1) as IXMLTradePaymentTermsType_ram;
end;

function TXMLTradePaymentTermsType_ramList.Insert(const Index: Integer): IXMLTradePaymentTermsType_ram;
begin
  Result := AddItem(Index) as IXMLTradePaymentTermsType_ram;
end;

function TXMLTradePaymentTermsType_ramList.Get_Item(Index: Integer): IXMLTradePaymentTermsType_ram;
begin
  Result := List[Index] as IXMLTradePaymentTermsType_ram;
end;

{ TXMLTradePaymentPenaltyTermsType_ram }

procedure TXMLTradePaymentPenaltyTermsType_ram.AfterConstruction;
begin
  RegisterChildNode('BasisDateTime', TXMLDateTimeType_udt);
  RegisterChildNode('BasisPeriodMeasure', TXMLMeasureType_udt);
  RegisterChildNode('BasisAmount', TXMLAmountType_udt);
  RegisterChildNode('CalculationPercent', TXMLPercentType_udt);
  RegisterChildNode('ActualPenaltyAmount', TXMLAmountType_udt);
  FBasisAmount := CreateCollection(TXMLAmountType_udtList, IXMLAmountType_udt, 'BasisAmount') as IXMLAmountType_udtList;
  FActualPenaltyAmount := CreateCollection(TXMLAmountType_udtList, IXMLAmountType_udt, 'ActualPenaltyAmount') as IXMLAmountType_udtList;
  inherited;
end;

function TXMLTradePaymentPenaltyTermsType_ram.Get_BasisDateTime: IXMLDateTimeType_udt;
begin
  Result := ChildNodes['BasisDateTime'] as IXMLDateTimeType_udt;
end;

function TXMLTradePaymentPenaltyTermsType_ram.Get_BasisPeriodMeasure: IXMLMeasureType_udt;
begin
  Result := ChildNodes['BasisPeriodMeasure'] as IXMLMeasureType_udt;
end;

function TXMLTradePaymentPenaltyTermsType_ram.Get_BasisAmount: IXMLAmountType_udtList;
begin
  Result := FBasisAmount;
end;

function TXMLTradePaymentPenaltyTermsType_ram.Get_CalculationPercent: IXMLPercentType_udt;
begin
  Result := ChildNodes['CalculationPercent'] as IXMLPercentType_udt;
end;

function TXMLTradePaymentPenaltyTermsType_ram.Get_ActualPenaltyAmount: IXMLAmountType_udtList;
begin
  Result := FActualPenaltyAmount;
end;

{ TXMLTradePaymentPenaltyTermsType_ramList }

function TXMLTradePaymentPenaltyTermsType_ramList.Add: IXMLTradePaymentPenaltyTermsType_ram;
begin
  Result := AddItem(-1) as IXMLTradePaymentPenaltyTermsType_ram;
end;

function TXMLTradePaymentPenaltyTermsType_ramList.Insert(const Index: Integer): IXMLTradePaymentPenaltyTermsType_ram;
begin
  Result := AddItem(Index) as IXMLTradePaymentPenaltyTermsType_ram;
end;

function TXMLTradePaymentPenaltyTermsType_ramList.Get_Item(Index: Integer): IXMLTradePaymentPenaltyTermsType_ram;
begin
  Result := List[Index] as IXMLTradePaymentPenaltyTermsType_ram;
end;

{ TXMLMeasureType_udt }

function TXMLMeasureType_udt.Get_UnitCode: UnicodeString;
begin
  Result := AttributeNodes['unitCode'].Text;
end;

procedure TXMLMeasureType_udt.Set_UnitCode(Value: UnicodeString);
begin
  SetAttribute('unitCode', Value);
end;

{ TXMLMeasureType_udtList }

function TXMLMeasureType_udtList.Add: IXMLMeasureType_udt;
begin
  Result := AddItem(-1) as IXMLMeasureType_udt;
end;

function TXMLMeasureType_udtList.Insert(const Index: Integer): IXMLMeasureType_udt;
begin
  Result := AddItem(Index) as IXMLMeasureType_udt;
end;

function TXMLMeasureType_udtList.Get_Item(Index: Integer): IXMLMeasureType_udt;
begin
  Result := List[Index] as IXMLMeasureType_udt;
end;

{ TXMLTradePaymentDiscountTermsType_ram }

procedure TXMLTradePaymentDiscountTermsType_ram.AfterConstruction;
begin
  RegisterChildNode('BasisDateTime', TXMLDateTimeType_udt);
  RegisterChildNode('BasisPeriodMeasure', TXMLMeasureType_udt);
  RegisterChildNode('BasisAmount', TXMLAmountType_udt);
  RegisterChildNode('CalculationPercent', TXMLPercentType_udt);
  RegisterChildNode('ActualDiscountAmount', TXMLAmountType_udt);
  FBasisAmount := CreateCollection(TXMLAmountType_udtList, IXMLAmountType_udt, 'BasisAmount') as IXMLAmountType_udtList;
  FActualDiscountAmount := CreateCollection(TXMLAmountType_udtList, IXMLAmountType_udt, 'ActualDiscountAmount') as IXMLAmountType_udtList;
  inherited;
end;

function TXMLTradePaymentDiscountTermsType_ram.Get_BasisDateTime: IXMLDateTimeType_udt;
begin
  Result := ChildNodes['BasisDateTime'] as IXMLDateTimeType_udt;
end;

function TXMLTradePaymentDiscountTermsType_ram.Get_BasisPeriodMeasure: IXMLMeasureType_udt;
begin
  Result := ChildNodes['BasisPeriodMeasure'] as IXMLMeasureType_udt;
end;

function TXMLTradePaymentDiscountTermsType_ram.Get_BasisAmount: IXMLAmountType_udtList;
begin
  Result := FBasisAmount;
end;

function TXMLTradePaymentDiscountTermsType_ram.Get_CalculationPercent: IXMLPercentType_udt;
begin
  Result := ChildNodes['CalculationPercent'] as IXMLPercentType_udt;
end;

function TXMLTradePaymentDiscountTermsType_ram.Get_ActualDiscountAmount: IXMLAmountType_udtList;
begin
  Result := FActualDiscountAmount;
end;

{ TXMLTradePaymentDiscountTermsType_ramList }

function TXMLTradePaymentDiscountTermsType_ramList.Add: IXMLTradePaymentDiscountTermsType_ram;
begin
  Result := AddItem(-1) as IXMLTradePaymentDiscountTermsType_ram;
end;

function TXMLTradePaymentDiscountTermsType_ramList.Insert(const Index: Integer): IXMLTradePaymentDiscountTermsType_ram;
begin
  Result := AddItem(Index) as IXMLTradePaymentDiscountTermsType_ram;
end;

function TXMLTradePaymentDiscountTermsType_ramList.Get_Item(Index: Integer): IXMLTradePaymentDiscountTermsType_ram;
begin
  Result := List[Index] as IXMLTradePaymentDiscountTermsType_ram;
end;

{ TXMLTradeAccountingAccountType_ram }

procedure TXMLTradeAccountingAccountType_ram.AfterConstruction;
begin
  RegisterChildNode('ID', TXMLIDType_udt);
  inherited;
end;

function TXMLTradeAccountingAccountType_ram.Get_ID: IXMLIDType_udt;
begin
  Result := ChildNodes['ID'] as IXMLIDType_udt;
end;

{ TXMLTradeAccountingAccountType_ramList }

function TXMLTradeAccountingAccountType_ramList.Add: IXMLTradeAccountingAccountType_ram;
begin
  Result := AddItem(-1) as IXMLTradeAccountingAccountType_ram;
end;

function TXMLTradeAccountingAccountType_ramList.Insert(const Index: Integer): IXMLTradeAccountingAccountType_ram;
begin
  Result := AddItem(Index) as IXMLTradeAccountingAccountType_ram;
end;

function TXMLTradeAccountingAccountType_ramList.Get_Item(Index: Integer): IXMLTradeAccountingAccountType_ram;
begin
  Result := List[Index] as IXMLTradeAccountingAccountType_ram;
end;

{ TXMLTradeSettlementMonetarySummationType_ram }

procedure TXMLTradeSettlementMonetarySummationType_ram.AfterConstruction;
begin
  RegisterChildNode('LineTotalAmount', TXMLAmountType_udt);
  RegisterChildNode('ChargeTotalAmount', TXMLAmountType_udt);
  RegisterChildNode('AllowanceTotalAmount', TXMLAmountType_udt);
  RegisterChildNode('TaxBasisTotalAmount', TXMLAmountType_udt);
  RegisterChildNode('TaxTotalAmount', TXMLAmountType_udt);
  RegisterChildNode('GrandTotalAmount', TXMLAmountType_udt);
  RegisterChildNode('TotalPrepaidAmount', TXMLAmountType_udt);
  RegisterChildNode('TotalAllowanceChargeAmount', TXMLAmountType_udt);
  RegisterChildNode('DuePayableAmount', TXMLAmountType_udt);
  FLineTotalAmount := CreateCollection(TXMLAmountType_udtList, IXMLAmountType_udt, 'LineTotalAmount') as IXMLAmountType_udtList;
  FChargeTotalAmount := CreateCollection(TXMLAmountType_udtList, IXMLAmountType_udt, 'ChargeTotalAmount') as IXMLAmountType_udtList;
  FAllowanceTotalAmount := CreateCollection(TXMLAmountType_udtList, IXMLAmountType_udt, 'AllowanceTotalAmount') as IXMLAmountType_udtList;
  FTaxBasisTotalAmount := CreateCollection(TXMLAmountType_udtList, IXMLAmountType_udt, 'TaxBasisTotalAmount') as IXMLAmountType_udtList;
  FTaxTotalAmount := CreateCollection(TXMLAmountType_udtList, IXMLAmountType_udt, 'TaxTotalAmount') as IXMLAmountType_udtList;
  FGrandTotalAmount := CreateCollection(TXMLAmountType_udtList, IXMLAmountType_udt, 'GrandTotalAmount') as IXMLAmountType_udtList;
  FTotalPrepaidAmount := CreateCollection(TXMLAmountType_udtList, IXMLAmountType_udt, 'TotalPrepaidAmount') as IXMLAmountType_udtList;
  FTotalAllowanceChargeAmount := CreateCollection(TXMLAmountType_udtList, IXMLAmountType_udt, 'TotalAllowanceChargeAmount') as IXMLAmountType_udtList;
  FDuePayableAmount := CreateCollection(TXMLAmountType_udtList, IXMLAmountType_udt, 'DuePayableAmount') as IXMLAmountType_udtList;
  inherited;
end;

function TXMLTradeSettlementMonetarySummationType_ram.Get_LineTotalAmount: IXMLAmountType_udtList;
begin
  Result := FLineTotalAmount;
end;

function TXMLTradeSettlementMonetarySummationType_ram.Get_ChargeTotalAmount: IXMLAmountType_udtList;
begin
  Result := FChargeTotalAmount;
end;

function TXMLTradeSettlementMonetarySummationType_ram.Get_AllowanceTotalAmount: IXMLAmountType_udtList;
begin
  Result := FAllowanceTotalAmount;
end;

function TXMLTradeSettlementMonetarySummationType_ram.Get_TaxBasisTotalAmount: IXMLAmountType_udtList;
begin
  Result := FTaxBasisTotalAmount;
end;

function TXMLTradeSettlementMonetarySummationType_ram.Get_TaxTotalAmount: IXMLAmountType_udtList;
begin
  Result := FTaxTotalAmount;
end;

function TXMLTradeSettlementMonetarySummationType_ram.Get_GrandTotalAmount: IXMLAmountType_udtList;
begin
  Result := FGrandTotalAmount;
end;

function TXMLTradeSettlementMonetarySummationType_ram.Get_TotalPrepaidAmount: IXMLAmountType_udtList;
begin
  Result := FTotalPrepaidAmount;
end;

function TXMLTradeSettlementMonetarySummationType_ram.Get_TotalAllowanceChargeAmount: IXMLAmountType_udtList;
begin
  Result := FTotalAllowanceChargeAmount;
end;

function TXMLTradeSettlementMonetarySummationType_ram.Get_DuePayableAmount: IXMLAmountType_udtList;
begin
  Result := FDuePayableAmount;
end;

{ TXMLSupplyChainTradeLineItemType_ram }

procedure TXMLSupplyChainTradeLineItemType_ram.AfterConstruction;
begin
  RegisterChildNode('AssociatedDocumentLineDocument', TXMLDocumentLineDocumentType_ram);
  RegisterChildNode('SpecifiedSupplyChainTradeAgreement', TXMLSupplyChainTradeAgreementType_ram);
  RegisterChildNode('SpecifiedSupplyChainTradeDelivery', TXMLSupplyChainTradeDeliveryType_ram);
  RegisterChildNode('SpecifiedSupplyChainTradeSettlement', TXMLSupplyChainTradeSettlementType_ram);
  RegisterChildNode('SpecifiedTradeProduct', TXMLTradeProductType_ram);
  inherited;
end;

function TXMLSupplyChainTradeLineItemType_ram.Get_AssociatedDocumentLineDocument: IXMLDocumentLineDocumentType_ram;
begin
  Result := ChildNodes['AssociatedDocumentLineDocument'] as IXMLDocumentLineDocumentType_ram;
end;

function TXMLSupplyChainTradeLineItemType_ram.Get_SpecifiedSupplyChainTradeAgreement: IXMLSupplyChainTradeAgreementType_ram;
begin
  Result := ChildNodes['SpecifiedSupplyChainTradeAgreement'] as IXMLSupplyChainTradeAgreementType_ram;
end;

function TXMLSupplyChainTradeLineItemType_ram.Get_SpecifiedSupplyChainTradeDelivery: IXMLSupplyChainTradeDeliveryType_ram;
begin
  Result := ChildNodes['SpecifiedSupplyChainTradeDelivery'] as IXMLSupplyChainTradeDeliveryType_ram;
end;

function TXMLSupplyChainTradeLineItemType_ram.Get_SpecifiedSupplyChainTradeSettlement: IXMLSupplyChainTradeSettlementType_ram;
begin
  Result := ChildNodes['SpecifiedSupplyChainTradeSettlement'] as IXMLSupplyChainTradeSettlementType_ram;
end;

function TXMLSupplyChainTradeLineItemType_ram.Get_SpecifiedTradeProduct: IXMLTradeProductType_ram;
begin
  Result := ChildNodes['SpecifiedTradeProduct'] as IXMLTradeProductType_ram;
end;

{ TXMLSupplyChainTradeLineItemType_ramList }

function TXMLSupplyChainTradeLineItemType_ramList.Add: IXMLSupplyChainTradeLineItemType_ram;
begin
  Result := AddItem(-1) as IXMLSupplyChainTradeLineItemType_ram;
end;

function TXMLSupplyChainTradeLineItemType_ramList.Insert(const Index: Integer): IXMLSupplyChainTradeLineItemType_ram;
begin
  Result := AddItem(Index) as IXMLSupplyChainTradeLineItemType_ram;
end;

function TXMLSupplyChainTradeLineItemType_ramList.Get_Item(Index: Integer): IXMLSupplyChainTradeLineItemType_ram;
begin
  Result := List[Index] as IXMLSupplyChainTradeLineItemType_ram;
end;

{ TXMLDocumentLineDocumentType_ram }

procedure TXMLDocumentLineDocumentType_ram.AfterConstruction;
begin
  RegisterChildNode('LineID', TXMLIDType_udt);
  RegisterChildNode('IncludedNote', TXMLNoteType_ram);
  FIncludedNote := CreateCollection(TXMLNoteType_ramList, IXMLNoteType_ram, 'IncludedNote') as IXMLNoteType_ramList;
  inherited;
end;

function TXMLDocumentLineDocumentType_ram.Get_LineID: IXMLIDType_udt;
begin
  Result := ChildNodes['LineID'] as IXMLIDType_udt;
end;

function TXMLDocumentLineDocumentType_ram.Get_IncludedNote: IXMLNoteType_ramList;
begin
  Result := FIncludedNote;
end;

{ TXMLTradeProductType_ram }

procedure TXMLTradeProductType_ram.AfterConstruction;
begin
  RegisterChildNode('GlobalID', TXMLIDType_udt);
  RegisterChildNode('SellerAssignedID', TXMLIDType_udt);
  RegisterChildNode('BuyerAssignedID', TXMLIDType_udt);
  RegisterChildNode('Name', TXMLTextType_udt);
  RegisterChildNode('Description', TXMLTextType_udt);
  RegisterChildNode('ApplicableProductCharacteristic', TXMLProductCharacteristicType_ram);
  RegisterChildNode('DesignatedProductClassification', TXMLProductClassificationType_ram);
  RegisterChildNode('OriginTradeCountry', TXMLTradeCountryType_ram);
  RegisterChildNode('IncludedReferencedProduct', TXMLReferencedProductType_ram);
  FGlobalID := CreateCollection(TXMLIDType_udtList, IXMLIDType_udt, 'GlobalID') as IXMLIDType_udtList;
  FName := CreateCollection(TXMLTextType_udtList, IXMLTextType_udt, 'Name') as IXMLTextType_udtList;
  FDescription := CreateCollection(TXMLTextType_udtList, IXMLTextType_udt, 'Description') as IXMLTextType_udtList;
  FApplicableProductCharacteristic := CreateCollection(TXMLProductCharacteristicType_ramList, IXMLProductCharacteristicType_ram, 'ApplicableProductCharacteristic') as IXMLProductCharacteristicType_ramList;
  FDesignatedProductClassification := CreateCollection(TXMLProductClassificationType_ramList, IXMLProductClassificationType_ram, 'DesignatedProductClassification') as IXMLProductClassificationType_ramList;
  FOriginTradeCountry := CreateCollection(TXMLTradeCountryType_ramList, IXMLTradeCountryType_ram, 'OriginTradeCountry') as IXMLTradeCountryType_ramList;
  FIncludedReferencedProduct := CreateCollection(TXMLReferencedProductType_ramList, IXMLReferencedProductType_ram, 'IncludedReferencedProduct') as IXMLReferencedProductType_ramList;
  inherited;
end;

function TXMLTradeProductType_ram.Get_GlobalID: IXMLIDType_udtList;
begin
  Result := FGlobalID;
end;

function TXMLTradeProductType_ram.Get_SellerAssignedID: IXMLIDType_udt;
begin
  Result := ChildNodes['SellerAssignedID'] as IXMLIDType_udt;
end;

function TXMLTradeProductType_ram.Get_BuyerAssignedID: IXMLIDType_udt;
begin
  Result := ChildNodes['BuyerAssignedID'] as IXMLIDType_udt;
end;

function TXMLTradeProductType_ram.Get_Name: IXMLTextType_udtList;
begin
  Result := FName;
end;

function TXMLTradeProductType_ram.Get_Description: IXMLTextType_udtList;
begin
  Result := FDescription;
end;

function TXMLTradeProductType_ram.Get_ApplicableProductCharacteristic: IXMLProductCharacteristicType_ramList;
begin
  Result := FApplicableProductCharacteristic;
end;

function TXMLTradeProductType_ram.Get_DesignatedProductClassification: IXMLProductClassificationType_ramList;
begin
  Result := FDesignatedProductClassification;
end;

function TXMLTradeProductType_ram.Get_OriginTradeCountry: IXMLTradeCountryType_ramList;
begin
  Result := FOriginTradeCountry;
end;

function TXMLTradeProductType_ram.Get_IncludedReferencedProduct: IXMLReferencedProductType_ramList;
begin
  Result := FIncludedReferencedProduct;
end;

{ TXMLProductCharacteristicType_ram }

procedure TXMLProductCharacteristicType_ram.AfterConstruction;
begin
  RegisterChildNode('TypeCode', TXMLCodeType_udt);
  RegisterChildNode('Description', TXMLTextType_udt);
  RegisterChildNode('ValueMeasure', TXMLMeasureType_udt);
  RegisterChildNode('Value', TXMLTextType_udt);
  FTypeCode := CreateCollection(TXMLCodeType_udtList, IXMLCodeType_udt, 'TypeCode') as IXMLCodeType_udtList;
  FDescription := CreateCollection(TXMLTextType_udtList, IXMLTextType_udt, 'Description') as IXMLTextType_udtList;
  FValueMeasure := CreateCollection(TXMLMeasureType_udtList, IXMLMeasureType_udt, 'ValueMeasure') as IXMLMeasureType_udtList;
  FValue := CreateCollection(TXMLTextType_udtList, IXMLTextType_udt, 'Value') as IXMLTextType_udtList;
  inherited;
end;

function TXMLProductCharacteristicType_ram.Get_TypeCode: IXMLCodeType_udtList;
begin
  Result := FTypeCode;
end;

function TXMLProductCharacteristicType_ram.Get_Description: IXMLTextType_udtList;
begin
  Result := FDescription;
end;

function TXMLProductCharacteristicType_ram.Get_ValueMeasure: IXMLMeasureType_udtList;
begin
  Result := FValueMeasure;
end;

function TXMLProductCharacteristicType_ram.Get_Value: IXMLTextType_udtList;
begin
  Result := FValue;
end;

{ TXMLProductCharacteristicType_ramList }

function TXMLProductCharacteristicType_ramList.Add: IXMLProductCharacteristicType_ram;
begin
  Result := AddItem(-1) as IXMLProductCharacteristicType_ram;
end;

function TXMLProductCharacteristicType_ramList.Insert(const Index: Integer): IXMLProductCharacteristicType_ram;
begin
  Result := AddItem(Index) as IXMLProductCharacteristicType_ram;
end;

function TXMLProductCharacteristicType_ramList.Get_Item(Index: Integer): IXMLProductCharacteristicType_ram;
begin
  Result := List[Index] as IXMLProductCharacteristicType_ram;
end;

{ TXMLProductClassificationType_ram }

procedure TXMLProductClassificationType_ram.AfterConstruction;
begin
  RegisterChildNode('ClassCode', TXMLCodeType_udt);
  RegisterChildNode('ClassName', TXMLTextType_udt);
  FClassName := CreateCollection(TXMLTextType_udtList, IXMLTextType_udt, 'ClassName') as IXMLTextType_udtList;
  inherited;
end;

function TXMLProductClassificationType_ram.Get_ClassCode: IXMLCodeType_udt;
begin
  Result := ChildNodes['ClassCode'] as IXMLCodeType_udt;
end;

function TXMLProductClassificationType_ram.Get_ClassName: IXMLTextType_udtList;
begin
  Result := FClassName;
end;

{ TXMLProductClassificationType_ramList }

function TXMLProductClassificationType_ramList.Add: IXMLProductClassificationType_ram;
begin
  Result := AddItem(-1) as IXMLProductClassificationType_ram;
end;

function TXMLProductClassificationType_ramList.Insert(const Index: Integer): IXMLProductClassificationType_ram;
begin
  Result := AddItem(Index) as IXMLProductClassificationType_ram;
end;

function TXMLProductClassificationType_ramList.Get_Item(Index: Integer): IXMLProductClassificationType_ram;
begin
  Result := List[Index] as IXMLProductClassificationType_ram;
end;

{ TXMLTradeCountryType_ram }

procedure TXMLTradeCountryType_ram.AfterConstruction;
begin
  RegisterChildNode('ID', TXMLCountryIDType_qdt);
  ItemTag := 'ID';
  ItemInterface := IXMLCountryIDType_qdt;
  inherited;
end;

function TXMLTradeCountryType_ram.Get_ID(Index: Integer): IXMLCountryIDType_qdt;
begin
  Result := List[Index] as IXMLCountryIDType_qdt;
end;

function TXMLTradeCountryType_ram.Add: IXMLCountryIDType_qdt;
begin
  Result := AddItem(-1) as IXMLCountryIDType_qdt;
end;

function TXMLTradeCountryType_ram.Insert(const Index: Integer): IXMLCountryIDType_qdt;
begin
  Result := AddItem(Index) as IXMLCountryIDType_qdt;
end;

{ TXMLTradeCountryType_ramList }

function TXMLTradeCountryType_ramList.Add: IXMLTradeCountryType_ram;
begin
  Result := AddItem(-1) as IXMLTradeCountryType_ram;
end;

function TXMLTradeCountryType_ramList.Insert(const Index: Integer): IXMLTradeCountryType_ram;
begin
  Result := AddItem(Index) as IXMLTradeCountryType_ram;
end;

function TXMLTradeCountryType_ramList.Get_Item(Index: Integer): IXMLTradeCountryType_ram;
begin
  Result := List[Index] as IXMLTradeCountryType_ram;
end;

{ TXMLReferencedProductType_ram }

procedure TXMLReferencedProductType_ram.AfterConstruction;
begin
  RegisterChildNode('GlobalID', TXMLIDType_udt);
  RegisterChildNode('SellerAssignedID', TXMLIDType_udt);
  RegisterChildNode('BuyerAssignedID', TXMLIDType_udt);
  RegisterChildNode('Name', TXMLTextType_udt);
  RegisterChildNode('Description', TXMLTextType_udt);
  RegisterChildNode('UnitQuantity', TXMLQuantityType_udt);
  FGlobalID := CreateCollection(TXMLIDType_udtList, IXMLIDType_udt, 'GlobalID') as IXMLIDType_udtList;
  FName := CreateCollection(TXMLTextType_udtList, IXMLTextType_udt, 'Name') as IXMLTextType_udtList;
  FDescription := CreateCollection(TXMLTextType_udtList, IXMLTextType_udt, 'Description') as IXMLTextType_udtList;
  FUnitQuantity := CreateCollection(TXMLQuantityType_udtList, IXMLQuantityType_udt, 'UnitQuantity') as IXMLQuantityType_udtList;
  inherited;
end;

function TXMLReferencedProductType_ram.Get_GlobalID: IXMLIDType_udtList;
begin
  Result := FGlobalID;
end;

function TXMLReferencedProductType_ram.Get_SellerAssignedID: IXMLIDType_udt;
begin
  Result := ChildNodes['SellerAssignedID'] as IXMLIDType_udt;
end;

function TXMLReferencedProductType_ram.Get_BuyerAssignedID: IXMLIDType_udt;
begin
  Result := ChildNodes['BuyerAssignedID'] as IXMLIDType_udt;
end;

function TXMLReferencedProductType_ram.Get_Name: IXMLTextType_udtList;
begin
  Result := FName;
end;

function TXMLReferencedProductType_ram.Get_Description: IXMLTextType_udtList;
begin
  Result := FDescription;
end;

function TXMLReferencedProductType_ram.Get_UnitQuantity: IXMLQuantityType_udtList;
begin
  Result := FUnitQuantity;
end;

{ TXMLReferencedProductType_ramList }

function TXMLReferencedProductType_ramList.Add: IXMLReferencedProductType_ram;
begin
  Result := AddItem(-1) as IXMLReferencedProductType_ram;
end;

function TXMLReferencedProductType_ramList.Insert(const Index: Integer): IXMLReferencedProductType_ram;
begin
  Result := AddItem(Index) as IXMLReferencedProductType_ram;
end;

function TXMLReferencedProductType_ramList.Get_Item(Index: Integer): IXMLReferencedProductType_ram;
begin
  Result := List[Index] as IXMLReferencedProductType_ram;
end;

end.