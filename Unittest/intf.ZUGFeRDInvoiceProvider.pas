unit intf.ZUGFeRDInvoiceProvider;

interface

uses
  System.SysUtils
  ,intf.ZUGFeRDInvoiceDescriptor
  ;

type
  TZUGFeRDInvoiceProvider = class
  public
    class function CreateInvoice: TZUGFeRDInvoiceDescriptor;
  end;

implementation

uses
  intf.ZUGFeRDHelper
  ,intf.ZUGFeRDProfile
  ,intf.ZUGFeRDInvoiceTypes
  ,intf.ZUGFeRDCurrencyCodes
  ,intf.ZUGFeRDSubjectCodes
  ,intf.ZUGFeRDQuantityCodes
  ,intf.ZUGFeRDTaxRegistrationSchemeID
  ,intf.ZUGFeRDCountryCodes
  ,intf.ZUGFeRDTaxTypes
  ,intf.ZUGFeRDTaxCategoryCodes
  ,intf.ZUGFeRDPaymentMeansTypeCodes
  ,intf.ZUGFeRDGlobalID
  ,intf.ZUGFeRDLegalOrganization
  ,intf.ZUGFeRDGlobalIDSchemeIdentifiers
  ,intf.ZUGFeRDElectronicAddressSchemeIdentifiers
  ;

{ TZUGFeRDInvoiceProvider }

class function TZUGFeRDInvoiceProvider.CreateInvoice: TZUGFeRDInvoiceDescriptor;
begin
  Result := TZUGFeRDInvoiceDescriptor.CreateInvoice('471102',
    EncodeDate(2018, 03, 05), TZUGFeRDCurrencyCodes.EUR);
  Result.BusinessProcess := 'urn:fdc:peppol.eu:2017:poacc:billing:01:1.0';
  Result.Name := 'WARENRECHNUNG';

  Result.AddNote('Rechnung gem'+#$00E4+#$00DF+' Bestellung vom 01.03.2018.');
  Result.AddNote('Lieferant GmbH' + #13#10 +
    'Lieferantenstra'+#$00DF+'e 20' + #13#10 +
    '80333 M'+#$00FC+'nchen' + #13#10 +
    'Deutschland' + #13#10 +
    'Gesch'+#$00E4+'ftsf'+#$00FC+'hrer: Hans Muster' + #13#10 +
    'Handelsregisternummer: H A 123' + #13#10,
    TZUGFeRDNullableParam<TZUGFeRDSubjectCodes>.Create(TZUGFeRDSubjectCodes.REG));

  Result.AddTradeLineItem(
    {name=}            'Trennbl'+#$00E4+'tter A4',
    {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(9.9),
    {description=}     '',
    {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.H87),
    {unitQuantity=}    nil,
    {grossUnitPrice=}  TZUGFeRDNullableParam<Currency>.Create(9.9),
    {billedQuantity=}  20,
    {lineTotalAmount=} 0,
    {taxType=}         TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
    {categoryCode=}    TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
    {taxPercent=}      19,
    {comment=}         '',
    {id=}              TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.EAN, '4012345001235'),
    {sellerAssignedID=} 'TB100A4'
  );

  Result.AddTradeLineItem(
    {name=}            'Joghurt Banane',
    {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(5.5),
    {description=}     '',
    {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.H87),
    {unitQuantity=}    nil,
    {grossUnitPrice=}  TZUGFeRDNullableParam<Currency>.Create(5.5),
    {billedQuantity=}  50,
    {lineTotalAmount=} 0,
    {taxType=}         TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
    {categoryCode=}    TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
    {taxPercent=}      7,
    {comment=}         '',
    {id=}              TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.EAN, '4000050986428'),
    {sellerAssignedID=} 'ARNR2'
  );

  Result.ReferenceOrderNo := '04011000-12345-34';

  Result.SetSeller(
    {name=}     'Lieferant GmbH',
    {postcode=} '80333',
    {city=}     'M'+#$00FC+'nchen',
    {street=}   'Lieferantenstra'+#$00DF+'e 20',
    {country=}  TZUGFeRDNullableParam<TZUGFeRDCountryCodes>.Create(TZUGFeRDCountryCodes.DE),
    {id=}       '',
    {globalID=} TZUGFeRDGlobalID.CreateWithParams(TZUGFeRDGlobalIDSchemeIdentifiers.GLN, '4000001123452'),
    {legalOrg=} TZUGFeRDLegalOrganization.CreateWithParams(TZUGFeRDNullableParam<TZUGFeRDGlobalIDSchemeIdentifiers>.Create(TZUGFeRDGlobalIDSchemeIdentifiers.GLN), '4000001123452', 'Lieferant GmbH')
  );

  Result.SetSellerElectronicAddress('DE123456789',
    TZUGFeRDElectronicAddressSchemeIdentifiers.GermanyVatNumber);

  Result.SetSellerContact(
    {name=}         'Max Mustermann',
    {orgunit=}      'Muster-Einkauf',
    {emailAddress=} 'Max@Mustermann.de',
    {phoneno=}      '+49891234567'
  );

  Result.AddSellerTaxRegistration('201/113/40209', TZUGFeRDTaxRegistrationSchemeID.FC);
  Result.AddSellerTaxRegistration('DE123456789', TZUGFeRDTaxRegistrationSchemeID.VA);

  Result.SetBuyer(
    {name=}     'Kunden AG Mitte',
    {postcode=} '69876',
    {city=}     'Frankfurt',
    {street=}   'Kundenstra'+#$00DF+'e 15',
    {country=}  TZUGFeRDNullableParam<TZUGFeRDCountryCodes>.Create(TZUGFeRDCountryCodes.DE),
    {id=}       'GE2020211'
  );

  Result.SetBuyerElectronicAddress('DE2020211',
    TZUGFeRDElectronicAddressSchemeIdentifiers.GermanyVatNumber);

  Result.ActualDeliveryDate := EncodeDate(2018, 03, 05);

  Result.SetPaymentMeans(TZUGFeRDPaymentMeansTypeCodes.SEPACreditTransfer,
    'Zahlung per SEPA '+#$00DC+'berweisung.');

  Result.AddCreditorFinancialAccount(
    {iban=} 'DE02120300000000202051',
    {bic=}  'BYLADEM1001',
    {id=}   '',
    {bankleitzahl=} '',
    {bankName=} '',
    {name=} 'Kunden AG'
  );

  Result.AddApplicableTradeTax(
    {calculatedAmount=} 275.0 / 100 * 7,
    {basisAmount=}      275.0,
    {percent=}          7,
    {typeCode=}         TZUGFeRDTaxTypes.VAT,
    {categoryCode=}     TZUGFeRDTaxCategoryCodes.S
  );

  Result.AddApplicableTradeTax(
    {calculatedAmount=} 198.0 / 100 * 19,
    {basisAmount=}      198.0,
    {percent=}          19,
    {typeCode=}         TZUGFeRDTaxTypes.VAT,
    {categoryCode=}     TZUGFeRDTaxCategoryCodes.S
  );

  Result.AddTradePaymentTerms(
    'Zahlbar innerhalb 30 Tagen netto bis 04.04.2018, 3% Skonto innerhalb 10 Tagen bis 15.03.2018');

  // C# only sets 5 of 9 totals (the rest stay null/unset)
  Result.LineTotalAmount := 473.0;
  Result.TaxBasisAmount := 473.0;
  Result.TaxTotalAmount := 56.87;
  Result.GrandTotalAmount := 529.87;
  Result.DuePayableAmount := 529.87;
end;

end.
