unit intf.ZUGFeRD2;

interface

{.$DEFINE USE_OXMLDomVendor} //http://www.kluug.net/oxml.php

uses
  System.SysUtils,System.Classes,System.Variants,System.StrUtils,
  System.DateUtils,System.Generics.Collections,System.Rtti,
  Xml.xmldom,Xml.XMLDoc,Xml.XMLIntf,
  idCoderMime,
  intf.ZUGFeRDHelper
  {$IFDEF USE_OXMLDomVendor},OXmlDOMVendor{$ENDIF}
  ;

  //https://github.com/konik-io/konik/blob/master/src/main/java/io/konik/zugferd/Invoice.java
type
  TZUGFeRDValidationErrors = class
  public const
    INVALID_DOC_TYPE = -1;
    INVALID_TYPECODE = -2;//Element ram:TypeCode
    EMPTY_ID = -3; //Element ram:ID must occur exactly 1 times
    INVALID_DATE = -4; //Element ram:IssueDateTime must occur exactly 1 times.
    INVALID_COUNTRYID = -5;
    EMPTY_NAME = -6;
  end;

  TZUGFeRDValidationEvent = reference to procedure (_ValidationError : Integer);

  TZUGFeRDInvoiceTypeCode = (zugferdTypeCode_None,
                             zugferdTypeCode_Minimum,
                             zugferdTypeCode_BasicWL,
                             zugferdTypeCode_Basic,
                             zugferdTypeCode_Comfort,
                             zugferdTypeCode_Extended);

  TZUGFeRDID = class(TObject)
  public
		ID : String;
    SchemeID: String;
    SchemeAgencyID: String;
  public
    constructor Create;
    procedure Clear;
    procedure Load(_Node : IXMLNode);
    procedure Save(_ParentNode : IXMLNode);
    function  IsValid(_DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
  end;

  TZUGFeRDIDList = class(TObjectList<TZUGFeRDID>)
  public
    function AddItem : TZUGFeRDID;
  end;

  TZUGFeRDExchangedDocumentContext = class(TObject)
  public const
    P_MINIMUM = 'urn:factur-x.eu:1p0:minimum';
    P_BASICWL = 'urn:factur-x.eu:1p0:basicwl';
    P_BASIC = 'urn:cen.eu:en16931:2017#compliant#urn:factur-x.eu:1p0:basic';
    P_COMFORT = 'urn:cen.eu:en16931:2017';
    P_EXTENDED = 'urn:cen.eu:en16931:2017#conformant#urn:factur-x.eu:1p0:extended';
  public
    TestIndicator : Boolean;  //Testkennzeichen
    //TODO BusinessProcessSpecifiedDocumentContextParameter : TZUGFeRDID; //minOccurs="0" //Geschäftsprzessinformation
    GuidelineSpecifiedDocumentContextParameter : TZUGFeRDInvoiceTypeCode; //Spezifikationskennung
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Load(_Node : IXMLNode);
    procedure Save(_ParentNode : IXMLNode);
    function  IsValid(_DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
  end;

  TZUGFeRDIncludedNote = class(TObject)
  public
    ContentCode : String; //Freitext auf Dokumentenebene
    Content     : String;//Freitext zur Rechnung
    SubjectCode : String; //Code zur Qualifizierung des Freitextes
  public
    constructor Create;
    procedure Clear;
  end;

  TZUGFeRDIncludedNoteList = class(TObjectList<TZUGFeRDIncludedNote>)
  public
    function AddItem : TZUGFeRDIncludedNote;
    function AsText : String;
  end;

  TZUGFeRDDocumentCodeContentType = (dcct_None, //NOt defined
          dcct_80	,//Debit note related to goods or services	Invoice
          dcct_81	,//Credit note related to goods or services	Credit Note
          dcct_82	,//Metered services invoice	Invoice
          dcct_83	,//Credit note related to financial adjustments	Credit Note
          dcct_84	,//Debit note related to financial adjustments	Invoice
          dcct_130	,//Invoicing data sheet	Invoice
          dcct_202	,//Direct payment valuation	Invoice
          dcct_203	,//Provisional payment valuation	Invoice
          dcct_204	,//Payment valuation	Invoice
          dcct_211	,//Interim application for payment	Invoice
          dcct_261	,//Self billed credit note	Credit Note
          dcct_262	,//Consolidated credit note - goods and services	Credit Note
          dcct_295	,//Price variation invoice	Invoice
          dcct_296	,//Credit note for price variation	Credit Note
          dcct_308	,//Delcredere credit note	Credit Note
          dcct_325	,//Proforma invoice	Invoice
          dcct_326	,//Partial invoice	Invoice
          dcct_380	,//Commercial invoice	Invoice
          dcct_381	,//Credit note	Credit Note
          dcct_383	,//Debit note	Invoice
          dcct_384	,//Corrected invoice	Invoice
          dcct_385	,//Consolidated invoice	Invoice
          dcct_386	,//Prepayment invoice	Invoice
          dcct_387	,//Hire invoice	Invoice
          dcct_388	,//Tax invoice	Invoice
          dcct_389	,//Self-billed invoice	Invoice
          dcct_390	,//Delcredere invoice	Invoice
          dcct_393	,//Factored invoice	Invoice
          dcct_394	,//Lease invoice	Invoice
          dcct_395	,//Consignment invoice	Invoice
          dcct_396	,//Factored credit note	Credit Note
          dcct_420	,//Optical Character Reading (OCR) payment credit note	Credit Note
          dcct_456	,//Debit advice	Invoice
          dcct_457	,//Reversal of debit	Invoice
          dcct_458	,//Reversal of credit	Credit Note
          dcct_527	,//Self billed debit note	Invoice
          dcct_532	,//Forwarder's credit note	Credit Note
          dcct_575	,//Insurer's invoice	Invoice
          dcct_623	,//Forwarder's invoice	Invoice
          dcct_633	,//Port charges documents	Invoice
          dcct_751	,//Invoice information for accounting purposes	Invoice
          dcct_780	,//Freight invoice	Invoice
          dcct_935	);//Customs invoice	Invoice

  TZUGFeRDExchangedDocument = class(TObject)
  public
		ID : TZUGFeRDID;                           //Rechnungsnummer
		Name : String;                             //Dokumentenart Freitext
		TypeCode : TZUGFeRDDocumentCodeContentType;//Code für Rechnungstyp
		IssueDateTime : TDateTime;                 //Rechnungsdatum
		IncludedNotes : TZUGFeRDIncludedNoteList;  //Freitext zur Rechnung
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Load(_Node : IXMLNode);
    procedure Save(_ParentNode : IXMLNode);
    function  IsValid(_DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
  end;

  TZUGFeRDApplicableTradeTax = class(TObject)
  public
    CalculatedAmount       : Currency;//Ab Basic   Steuerbetrag Wert
    TypeCode               : String;  //Ab Basic   Steuerart
    BasisAmount            : Currency;//Ab Basic   Basisbetrag der Steuerberechnung Wert
    CategoryCode           : String;  //Ab Comfort Steuerkategorie Wert
    RateApplicablePercent  : double;  //Ab Basic   Steuerprozentsatz Wert
  public
    constructor Create;
    procedure Clear;
    procedure Load(_Node : IXMLNode);
    procedure Save(_ParentNode : IXMLNode);
    function  IsValid(_DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
  end;

  TZUGFeRDApplicableTradeTaxList = class(TObjectList<TZUGFeRDApplicableTradeTax>)
  public
    function AddItem : TZUGFeRDApplicableTradeTax;
    function  IsValid(_DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
  end;

  TZUGFeRDSpecifiedTradePaymentTerm = class(TObject)
  public
		Description : String;
    DueDateDateTime : TDateTime;
    ApplicableTradePaymentDiscountTerms_BasisAmount : Currency;
    ApplicableTradePaymentDiscountTerms_CalculationPercent : double;
    ApplicableTradePaymentDiscountTerms_ActualDiscountAmount : Currency;
  public
    constructor Create;
    procedure Clear;
    procedure Load(_Node : IXMLNode);
    procedure Save(_ParentNode : IXMLNode);
    function  IsValid(_DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
  end;

  TZUGFeRDSpecifiedTradePaymentTerms = class(TObjectList<TZUGFeRDSpecifiedTradePaymentTerm>)
  public
    function ContainsPaymentTermsByDate : Boolean;
    function  IsValid(_DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
  end;

  TZUGFeRDPostalTradeAddress = class(TObject)
  public
    PostcodeCode : String;
    LineOne : String;
    LineTwo : String;
    LineTree : String;
    CityName : String;
    CountryID : String;
    CountrySubDivisionName : String;
  public
    constructor Create;
    procedure Clear;
    procedure Load(_Node : IXMLNode);
    procedure Save(_ParentNode : IXMLNode);
    function  IsValid(_DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
  end;

  TZUGFeRDTradePartyType = class(TObject)
  public
    Name : String;
    PostalTradeAddress : TZUGFeRDPostalTradeAddress;
    SpecifiedTaxRegistration : TZUGFeRDIDList; //Steuernummer und Art der Nummer
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Load(_Node : IXMLNode);
    procedure Save(_TradePartyNode : IXMLNode);
    function  IsValid(_DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
  end;

  TZUGFeRDApplicableHeaderTradeAgreement = class(TObject)
  public
    BuyerReference : String; //Referenz des Käufers, ab COMFORT
    SellerTradeParty: TZUGFeRDTradePartyType;
    BuyerTradeParty: TZUGFeRDTradePartyType;
//    ShipFromTradeParty: TZUGFeRDTradePartyType;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Load(_Node : IXMLNode);
    procedure Save(_ParentNode : IXMLNode);
    function  IsValid(_DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
  end;

  TZUGFeRDApplicableHeaderTradeSettlement = class(TObject)
  public
    PaymentReference : String; //Zahlungsreferenz ab Basic für Überweisungen
    InvoiceCurrencyCode : String; //Ab Basic
    ApplicableTradeTaxList : TZUGFeRDApplicableTradeTaxList; //Ab Basic, Umsatzsteueraufschlüsselung
    SpecifiedTradePaymentTerms : TZUGFeRDSpecifiedTradePaymentTerms; //Zahlungsbedingungen
    SpecifiedTradeSettlementHeaderMonetarySummation_LineTotalAmount : Currency;         //Ab Basic Gesamtbetrag der Positionen Wert
    SpecifiedTradeSettlementHeaderMonetarySummation_ChargeTotalAmount : Currency;       //Ab Basic Gesamtbetrag der Zuschläge
    SpecifiedTradeSettlementHeaderMonetarySummation_AllowanceTotalAmount : Currency;    //Ab Basic Gesamtbetrag der Abschläge
    SpecifiedTradeSettlementHeaderMonetarySummation_TaxBasisTotalAmount : Currency;     //Ab Basic Steuerbasisbetrag
    SpecifiedTradeSettlementHeaderMonetarySummation_TaxBasisTotalAmountCurrID : String;
    SpecifiedTradeSettlementHeaderMonetarySummation_TaxTotalAmount : Currency;          //Ab Basic Steuergesamtbetrag
    SpecifiedTradeSettlementHeaderMonetarySummation_TaxTotalAmountCurrID : String;
    SpecifiedTradeSettlementHeaderMonetarySummation_GrandTotalAmount : Currency;        //Ab Basic Bruttosumme
    SpecifiedTradeSettlementHeaderMonetarySummation_GrandTotalAmountCurrID : String;
    SpecifiedTradeSettlementHeaderMonetarySummation_DuePayableAmount : Currency;        //Fälliger Zahlungsbetrag
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Load(_Node : IXMLNode);
    procedure Save(_ParentNode : IXMLNode);
    function  IsValid(_DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
  end;

  TZUGFeRDIncludedSupplyChainTradeLineItem = class(TObject)
  public
//    constructor Create;
//    destructor Destroy; override;
//    procedure Clear;
    procedure Load(_Node : IXMLNode);
//    procedure Save(_ParentNode : IXMLNode);
//    function  IsValid(_DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
  end;

  TZUGFeRDIncludedSupplyChainTradeLineItems = class(TObjectList<TZUGFeRDIncludedSupplyChainTradeLineItem>)
  public
    procedure Save(_ParentNode : IXMLNode);
    function  IsValid(_DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
  end;

  TZUGFeRDApplicableHeaderTradeDelivery = class(TObject)
  public
//    ShipToTradeParty: TZUGFeRDTradePartyType;
//    UltimateShipToTradeParty: TZUGFeRDTradePartyType;
//    ShipFromTradeParty: TZUGFeRDTradePartyType;
    ActualDeliverySupplyChainEvent_OccurrenceDateTime : TDateTime; //Lieferzeitpunkt, Im Moment nur ein Element, deswegen kein eigenes Objekt
    constructor Create;
//    destructor Destroy; override;
    procedure Clear;
    procedure Load(_Node : IXMLNode);
    procedure Save(_ParentNode : IXMLNode);
    function  IsValid(_DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
  end;

  TZUGFeRDSupplyChainTradeTransaction = class(TObject)
  public
    IncludedSupplyChainTradeLineItems : TZUGFeRDIncludedSupplyChainTradeLineItems;
    ApplicableHeaderTradeAgreement : TZUGFeRDApplicableHeaderTradeAgreement;
    ApplicableHeaderTradeDelivery : TZUGFeRDApplicableHeaderTradeDelivery;
    ApplicableHeaderTradeSettlement : TZUGFeRDApplicableHeaderTradeSettlement;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Load(_Node : IXMLNode);
    procedure Save(_ParentNode : IXMLNode);
    function  IsValid(_DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
  end;

  [ZUGFeRDAttrNS('a','urn:un:unece:uncefact:data:standard:QualifiedDataType:100')]
  [ZUGFeRDAttrNS('rsm','urn:un:unece:uncefact:data:standard:CrossIndustryInvoice:100')]
  [ZUGFeRDAttrNS('qdt','urn:un:unece:uncefact:data:standard:QualifiedDataType:10')]
  [ZUGFeRDAttrNS('ram','urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:100')]
  [ZUGFeRDAttrNS('xs','http://www.w3.org/2001/XMLSchema')]
  [ZUGFeRDAttrNS('udt','urn:un:unece:uncefact:data:standard:UnqualifiedDataType:100')]
  TZUGFeRDDocument = class(TObject)
  private
    procedure LoadDocument(_Xml : IXMLDocument);
    procedure SaveDocument(_Xml : IXMLDocument);
  public
    ExchangedDocumentContext : TZUGFeRDExchangedDocumentContext; //Prozessteuerung
    ExchangedDocument : TZUGFeRDExchangedDocument;
    SupplyChainTradeTransaction : TZUGFeRDSupplyChainTradeTransaction;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(const _Filename : String);
    procedure LoadFromStream(_Stream : TStream);
    procedure LoadFromXMLStr(const _XML : String);
    procedure LoadFromMime(const _XMLMime : String);
    procedure SaveToFile(const _Filename : String);
    procedure SaveToStream(_Stream : TStream);
    procedure SaveToXMLStr(out _XML : String);
    procedure SaveToMime(out _XMLMime : String);
    function  IsValid(_DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
    class function MimeStrToStream(Const _XMLMime : String) : TMemoryStream;
  end;

//  edFremdbelegNr.Text := _Itm.HeaderExchangedDocument.ID.ID;
//  deRGDate.Date := _Itm.HeaderExchangedDocument.IssueDateTime;
//  deValutaDatum.Date := _Itm.HeaderExchangedDocument.IssueDateTime;
//  if _Itm.SpecifiedSupplyChainTradeTransaction.ApplicableSupplyChainTradeSettlement.SpecifiedTradePaymentTerms.ContainsPaymentTermsByDate then
//  begin
//    lbZahlbed.Clear;
//    CurrentRechnung.ZahlungsBedingungen.Clear;
//    for i := 0 to _Itm.SpecifiedSupplyChainTradeTransaction.ApplicableSupplyChainTradeSettlement.SpecifiedTradePaymentTerms.Count-1 do
//    begin
//      pt := TPaymentTerm.Create;
//      pt.Percent := _Itm.SpecifiedSupplyChainTradeTransaction.ApplicableSupplyChainTradeSettlement.SpecifiedTradePaymentTerms[i].ApplicableTradePaymentDiscountTerms_CalculationPercent;
//      pt.DaysHalfMonths := DaysBetween(Trunc(_Itm.HeaderExchangedDocument.IssueDateTime),Trunc(_Itm.SpecifiedSupplyChainTradeTransaction.ApplicableSupplyChainTradeSettlement.SpecifiedTradePaymentTerms[i].DueDateDateTime));
//      CurrentRechnung.Zahlungsbedingungen.AddSorted(pt);
//    end;
//    deFaelligDate.Date := CurrentRechnung.ZahlungsBedingungen.GetFaelligAmDatum(CurrentRechnung.ValutaDatum);
//  end else
//  if CurrentRechnung.ZahlungsBedingungen.Count > 0 then
//  if (MessageDlg('Der Lieferant verfügt über ein Zahlungsziel, die Rechnung nicht.'+#10+'Zahlungsziel des Lieferanten behalten?', mtInformation, [mbYes, mbNo], 0) = mrNo) then
//  begin
//    lbZahlbed.Clear;
//    CurrentRechnung.Zahlungsbedingungen.Clear;
//  end;
//
//  lbMwst.Clear;
//  CurrentRechnung.MwStBetraege.Clear;
//  edBrutto.SetFloat(_Itm.SpecifiedSupplyChainTradeTransaction.ApplicableSupplyChainTradeSettlement.SpecifiedTradeSettlementHeaderMonetarySummation_GrandTotalAmount);
//  for i := 0 to _Itm.SpecifiedSupplyChainTradeTransaction.ApplicableSupplyChainTradeSettlement.ApplicableTradeTaxList.Count-1 do
//  begin
//    CurrentRechnung.MwStBetraege.AddMwstBetrag(
//      _Itm.SpecifiedSupplyChainTradeTransaction.ApplicableSupplyChainTradeSettlement.ApplicableTradeTaxList[i].BasisAmount,
//      _Itm.SpecifiedSupplyChainTradeTransaction.ApplicableSupplyChainTradeSettlement.ApplicableTradeTaxList[i].ApplicablePercent,
//      _Itm.SpecifiedSupplyChainTradeTransaction.ApplicableSupplyChainTradeSettlement.ApplicableTradeTaxList[i].CalculatedAmount);
//  end;
//
//  CurrentRechnung.RgNetto := CurrentRechnung.RgBrutto - CurrentRechnung.MwStBetraege.GetSummeMwStBetraege;
//  CurrentRechnung.RgOffenerBetrag :=  CurrentRechnung.RgBrutto - CurrentRechnung.ZahlungsBuchungen.GetZahlungsBuchungenSumme;
//  edNetto.Text := WideFormat('%n',[CurrentRechnung.RgNetto]);
//
//  lbMwst.Count := CurrentRechnung.MwStBetraege.Count;
//  lbMwst.Repaint;
//  lbZahlbed.Count := CurrentRechnung.Zahlungsbedingungen.Count;
//  lbZahlbed.Repaint;
//
//  if _Itm.SpecifiedSupplyChainTradeTransaction.ApplicableSupplyChainTradeAgreement.BuyerReference <> '' then
//  begin
//    Screen.Cursor := crHourglass;
//    try
//      Frame.RechnungsjournalFrame.FilterProjects(CurrentRechnung.RgNr);
//    finally
//      Screen.Cursor := crDefault;
//    end;
//    sum := Frame.RechnungsjournalFrame.GetKostentraegerSumme;
//    if CurrentRechnung.RgNetto - sum <> 0 then
//    begin
//      sb := TSplitbuchung.Create;
//      sb.RgRef := CurrentRechnung.RgNr;
//      sb.ProjektRef := _Itm.SpecifiedSupplyChainTradeTransaction.ApplicableSupplyChainTradeAgreement.BuyerReference;
//      sb.Kostentraegertyp := lastProkectType;
//      sb.Menge := 1;
//      sb.EP := CurrentRechnung.RgNetto - sum;
//      sbl := TSplitbuchungObjectList.Create;
//      sbl.Add(sb);
//      if TDPR00607Form6.ShowDialog(sbl,sb.EP,false,CurrentRechnung.FremdbelegNr) then
//      begin
//      lock := Frame.RechnungsjournalFrame.LockDB(SYSDATB.VollName);
//      if lock <> nil then
//      try
//        for i := 0 to sbl.Count-1 do
//        begin
//          //lastProjectNr := sbl[i].ProjektRef;
//          lastProkectType := sbl[i].Kostentraegertyp;
//           sbl[i].RgRef := CurrentRechnung.RgNr;
//          Frame.RechnungsjournalFrame.AddSplitBuchung(sbl[i]);
//        end;
//      finally
//        Frame.RechnungsjournalFrame.Unlock(lock);
//      end;
//      end;
//      sbl.Free;
//    end;
//  end;
//
//  if (CurrentRechnung.RgNetto <> _Itm.SpecifiedSupplyChainTradeTransaction.ApplicableSupplyChainTradeSettlement.SpecifiedTradeSettlementHeaderMonetarySummation_TaxBasisTotalAmount) then
//    MessageDlg('Der berechnete Netto-Betrag weicht vom dem im Dokument ab.', mtError, [mbOK], 0);



implementation

type
  TZUGFeRDHelper = class(TObject)
  public
    class function DateFromStr(const _Val : String) : TDateTime;
    class function DateToStr(const _Val : TDateTime) : String;
    class function StrToCurr(_Val : String) : Currency;
    class function StrToFloat(_Val : String) : double;
    class function CurrToStr(_Val : Currency) : String;
    class function FloatToStr(_Val : double) : String;
    class function DocumentCodeContentTypeToStr(_Val : TZUGFeRDDocumentCodeContentType) : String;
    class function DocumentCodeContentTypeFromStr(const _Val : String) : TZUGFeRDDocumentCodeContentType;
  end;

{ TZUGFeRDHelper }

class function TZUGFeRDHelper.CurrToStr(
  _Val: Currency): String;
begin
  Result := ReplaceText(Format('%.2f',[_Val]),',','.');
end;

class function TZUGFeRDHelper.DateFromStr(
  const _Val: String): TDateTime;
begin
  Result := 0;
  if Length(_Val) <> 8 then
    exit;
  Result := EncodeDate(StrToIntDef(Copy(_Val,1,4),1999),StrToIntDef(Copy(_Val,5,2),1),StrToIntDef(Copy(_Val,7,2),1));
end;

class function TZUGFeRDHelper.DateToStr(
  const _Val: TDateTime): String;
begin
  Result := FormatDateTime('yyyymmdd',_Val);
end;

class function TZUGFeRDHelper.DocumentCodeContentTypeFromStr(
  const _Val: String): TZUGFeRDDocumentCodeContentType;
begin
  case StrToIntDef(_Val,0) of
    80  : Result := dcct_80 ;
    81  : Result := dcct_81 ;
    82  : Result := dcct_82 ;
    83  : Result := dcct_83 ;
    84  : Result := dcct_84 ;
    130 : Result := dcct_130;
    202 : Result := dcct_202;
    203 : Result := dcct_203;
    204 : Result := dcct_204;
    211 : Result := dcct_211;
    261 : Result := dcct_261;
    262 : Result := dcct_262;
    295 : Result := dcct_295;
    296 : Result := dcct_296;
    308 : Result := dcct_308;
    325 : Result := dcct_325;
    326 : Result := dcct_326;
    380 : Result := dcct_380;
    381 : Result := dcct_381;
    383 : Result := dcct_383;
    384 : Result := dcct_384;
    385 : Result := dcct_385;
    386 : Result := dcct_386;
    387 : Result := dcct_387;
    388 : Result := dcct_388;
    389 : Result := dcct_389;
    390 : Result := dcct_390;
    393 : Result := dcct_393;
    394 : Result := dcct_394;
    395 : Result := dcct_395;
    396 : Result := dcct_396;
    420 : Result := dcct_420;
    456 : Result := dcct_456;
    457 : Result := dcct_457;
    458 : Result := dcct_458;
    527 : Result := dcct_527;
    532 : Result := dcct_532;
    575 : Result := dcct_575;
    623 : Result := dcct_623;
    633 : Result := dcct_633;
    751 : Result := dcct_751;
    780 : Result := dcct_780;
    935 : Result := dcct_935;
    else Result := dcct_None;
  end;
end;

class function TZUGFeRDHelper.DocumentCodeContentTypeToStr(
  _Val: TZUGFeRDDocumentCodeContentType): String;
begin
  case _Val of
    dcct_80: Result := '80';
    dcct_81: Result := '81';
    dcct_82: Result := '82';
    dcct_83: Result := '83';
    dcct_84: Result := '84';
    dcct_130:Result := '130';
    dcct_202:Result := '202';
    dcct_203:Result := '203';
    dcct_204:Result := '204';
    dcct_211:Result := '211';
    dcct_261:Result := '261';
    dcct_262:Result := '262';
    dcct_295:Result := '295';
    dcct_296:Result := '296';
    dcct_308:Result := '308';
    dcct_325:Result := '325';
    dcct_326:Result := '326';
    dcct_380:Result := '380';
    dcct_381:Result := '381';
    dcct_383:Result := '383';
    dcct_384:Result := '384';
    dcct_385:Result := '385';
    dcct_386:Result := '386';
    dcct_387:Result := '387';
    dcct_388:Result := '388';
    dcct_389:Result := '389';
    dcct_390:Result := '390';
    dcct_393:Result := '393';
    dcct_394:Result := '394';
    dcct_395:Result := '395';
    dcct_396:Result := '396';
    dcct_420:Result := '420';
    dcct_456:Result := '456';
    dcct_457:Result := '457';
    dcct_458:Result := '458';
    dcct_527:Result := '527';
    dcct_532:Result := '532';
    dcct_575:Result := '575';
    dcct_623:Result := '623';
    dcct_633:Result := '633';
    dcct_751:Result := '751';
    dcct_780:Result := '780';
    dcct_935:Result := '935';
    else Result := '';
  end;
end;

class function TZUGFeRDHelper.FloatToStr(
  _Val: double): String;
begin
  Result := ReplaceText(Format('%.2f',[_Val]),',','.');
end;

class function TZUGFeRDHelper.StrToCurr(
  _Val: String): Currency;
begin
  _Val := ReplaceText(_Val,'.',FormatSettings.DecimalSeparator);
  result := StrToCurrDef(_Val,0);
end;

class function TZUGFeRDHelper.StrToFloat(
  _Val: String): double;
begin
  _Val := ReplaceText(_Val,'.',FormatSettings.DecimalSeparator);
  result := StrToFloatDef(_Val,0);
end;


{ TZUGFeRDDocument }

constructor TZUGFeRDDocument.Create;
begin
  ExchangedDocumentContext := TZUGFeRDExchangedDocumentContext.Create;
  ExchangedDocument := TZUGFeRDExchangedDocument.Create;
  SupplyChainTradeTransaction := TZUGFeRDSupplyChainTradeTransaction.Create;
  Clear;
end;

destructor TZUGFeRDDocument.Destroy;
begin
  if Assigned(ExchangedDocumentContext) then begin ExchangedDocumentContext.Free; ExchangedDocumentContext := nil; end;
  if Assigned(ExchangedDocument) then begin ExchangedDocument.Free; ExchangedDocument := nil; end;
  if Assigned(SupplyChainTradeTransaction) then begin SupplyChainTradeTransaction.Free; SupplyChainTradeTransaction := nil; end;
  inherited;
end;

procedure TZUGFeRDDocument.Clear;
begin
  ExchangedDocumentContext.Clear;
  ExchangedDocument.Clear;
  SupplyChainTradeTransaction.Clear;
end;

procedure TZUGFeRDDocument.LoadFromFile(const _Filename : String);
var
  xml : IXMLDocument;
begin
  if not FileExists(_Filename) then
    exit;
  xml := LoadXMLDocument(_FileName);
  try
    Clear;
    LoadDocument(xml);
  finally
    xml := nil;
  end;
end;

procedure TZUGFeRDDocument.LoadFromMime(const _XMLMime: String);
var
  strout : TMemoryStream;
  mime : TIdDecoderMIME;
begin
  strout := TMemoryStream.Create;
  mime := TIdDecoderMIME.Create;
  try
    mime.DecodeStream(_XMLMime,strout);
    strout.Position := 0;
    LoadFromStream(strout);
  finally
    strout.Free;
    mime.Free;
  end;
end;

procedure TZUGFeRDDocument.LoadFromStream(_Stream : TStream);
var
  xml : IXMLDocument;
begin
  if _Stream = nil then
    exit;
  xml := TXMLDocument.Create(nil);
  {$IFDEF USE_OXMLDomVendor}TXMLDocument(xml).DOMVendor := GetDOMVendor(sOXmlDOMVendor);{$ENDIF}
  xml.LoadFromStream(_Stream);
  try
    Clear;
    LoadDocument(xml);
  finally
    xml := nil;
  end;
end;

procedure TZUGFeRDDocument.LoadFromXMLStr(const _XML: String);
var
  xml : IXMLDocument;
begin
  if _XML.IsEmpty then
    exit;
  xml := TXMLDocument.Create(nil);
  {$IFDEF USE_OXMLDomVendor}TXMLDocument(xml).DOMVendor := GetDOMVendor(sOXmlDOMVendor);{$ENDIF}
  xml.LoadFromXML(_XML);
  try
    Clear;
    LoadDocument(xml);
  finally
    xml := nil;
  end;
end;

class function TZUGFeRDDocument.MimeStrToStream(
  const _XMLMime: String): TMemoryStream;
var
  mime : TIdDecoderMIME;
begin
  Result := TMemoryStream.Create;
  if _XMLMime = '' then
    exit;
  mime := TIdDecoderMIME.Create;
  try
    mime.DecodeStream(_XMLMime,Result);
    Result.Position := 0;
  finally
    mime.Free;
  end;
end;

procedure TZUGFeRDDocument.SaveToFile(const _Filename: String);
var
  xml : IXMLDocument;
begin
  xml := NewXMLDocument;
  try
    SaveDocument(xml);
    xml.SaveToFile(_Filename);
  finally
    xml := nil;
  end;
end;

procedure TZUGFeRDDocument.SaveToMime(out _XMLMime: String);
var
  strin : TMemoryStream;
  mime : TIdEncoderMIME;
begin
  strin := TMemoryStream.Create;
  mime := TIdEncoderMIME.Create;
  try
    SaveToStream(strin);
    strin.Position := 0;
    _XMLMime := mime.EncodeStream(strin);
  finally
    strin.Free;
    mime.Free;
  end;
end;

procedure TZUGFeRDDocument.SaveToStream(_Stream: TStream);
var
  xml : IXMLDocument;
begin
  xml := NewXMLDocument;
  try
    SaveDocument(xml);
    xml.SaveToStream(_Stream);
  finally
    xml := nil;
  end;
end;

procedure TZUGFeRDDocument.SaveToXMLStr(out _XML: String);
var
  xml : IXMLDocument;
begin
  xml := NewXMLDocument;
  try
    SaveDocument(xml);
    xml.SaveToXML(_XML);
  finally
    xml := nil;
  end;
end;
//
procedure TZUGFeRDDocument.LoadDocument(
  _Xml: IXMLDocument);
begin
  if not SameText(_Xml.DocumentElement.LocalName,'CrossIndustryInvoice') then
    exit;

  TXMLHelper.LoadFromChilds('rsm:ExchangedDocumentContext',_Xml.DocumentElement,ExchangedDocumentContext.Load);
  TXMLHelper.LoadFromChilds('rsm:ExchangedDocument',_Xml.DocumentElement,ExchangedDocument.Load);
  TXMLHelper.LoadFromChilds('rsm:SupplyChainTradeTransaction',_Xml.DocumentElement,SupplyChainTradeTransaction.Load);
//
////  Result := xmldoc.GetDocBinding('CrossIndustryDocument', TXMLCrossIndustryDocumentType) as IXMLCrossIndustryDocumentType;
////  Result.DeclareNamespace('rsm',TargetNamespace);
//
////  end;
////  ShowMessage(_xml.ChildNodes['HeaderExchangedDocument'].Text);
////  exit;
////
////  Header.ID := _xml.HeaderExchangedDocument.ID.SchemeID;
////  if _xml.HeaderExchangedDocument.Name.Count > 0 then
////    Header.Name := _xml.HeaderExchangedDocument.Name.Items[0].Text;
////  if SameText(_xml.HeaderExchangedDocument.TypeCode.Text,'380') then
////    Header.TypeCode := zugferdTypeCode_Basic
////  else
////  if SameText(_xml.HeaderExchangedDocument.TypeCode.Text,'84') then
////    Header.TypeCode := zugferdTypeCode_Comfort
////  else
////  if SameText(_xml.HeaderExchangedDocument.TypeCode.Text,'389') then
////    Header.TypeCode := zugferdTypeCode_Extended;
////  Header.IssueDateTime := TCrossIndustryDocumentTypeHelper.DateFromStr(_xml.HeaderExchangedDocument.IssueDateTime.DateTimeString.Text);
//////  Header.SubjectCode := _xml.HeaderExchangedDocument.EffectiveSpecifiedPeriod.
//
end;

procedure TZUGFeRDDocument.SaveDocument(
  _Xml: IXMLDocument);
var
  lRTTI: TRTTIContext;
  lRTTIType: TRttiType;
  lRTTIFieldList: TArray<TRttiField>;
  lRTTIAttributes : TArray<TCustomAttribute>;
  i : Integer;
//  attr : LnxAttr;
  val : TValue;
  xRoot : IXMLNode;
begin
  {$IFDEF USE_OXMLDomVendor}TXMLDocument(_Xml).DOMVendor := Xml.xmldom.GetDOMVendor(sOXmlDOMVendor);{$ENDIF}
  //Result := xmldoc.GetDocBinding('rsm:CrossIndustryInvoice', TXMLCrossIndustryDocumentType) as IXMLCrossIndustryDocumentType;
  TXMLDocument(_Xml).Options := TXMLDocument(_Xml).Options + [doNodeAutoIndent];
  _Xml.Active := True;
  _Xml.Version := '1.0';
  _Xml.StandAlone := 'yes';
  _Xml.Encoding := 'UTF-8';

  _Xml.Options := [doNodeAutoCreate, doNodeAutoIndent, doAttrNull];

  xRoot := _Xml.AddChild('rsm:CrossIndustryInvoice');

  lRTTI := TRTTIContext.Create;
  try
    lRTTIType := lRTTI.GetType(self.ClassType);

//  xRoot.DeclareNamespace('rsm',TargetNamespace);
//  xRoot.DeclareNamespace('xsi','http://www.w3.org/2001/XMLSchema-instance');
//  xRoot.DeclareNamespace('ram',TargetNamespaceRam);
//  xRoot.DeclareNamespace('udt',TargetNamespaceUdt);

    lRTTIAttributes := lRTTIType.GetAttributes;
    for var attr : TCustomAttribute in lRTTIAttributes do
    if attr is ZUGFeRDAttrNS then
    with (attr as ZUGFeRDAttrNS) do
    begin
      xRoot.DeclareNamespace(Prefix,URI);
    end;

//    lRTTIFieldList := lRTTIType.GetDeclaredFields;
//    for i := 0 to Length(lRTTIFieldList)-1 do
//    begin
//      if not TLandrixLibraryHelper.HasAttribute<LnxAttr>(lRTTIFieldList[i],attr) then
//        continue;
//      if attr.IsIndexField then
//      if not _WithInternalUID then
//        continue;
//      case lRTTIFieldList[i].FieldType.TypeKind of
//        tkInteger :
//        begin
//          if _OnlyChangedFields then
//          if SameStr(_Itm.InitialFieldValues[i].Value, IntToStr(lRTTIFieldList[i].GetValue(_Itm).AsInteger))  then
//            continue;
//          Result.AddField(attr.Value).ValAsInt := lRTTIFieldList[i].GetValue(_Itm).AsInteger;
//        end;
//        tkUString :
//        begin
//          if _OnlyChangedFields then
//          if SameStr(_Itm.InitialFieldValues[i].Value, THashSHA1.GetHashString( lRTTIFieldList[i].GetValue(_Itm).AsString))  then
//            continue;
//          Result.AddField(attr.Value).Val := lRTTIFieldList[i].GetValue(_Itm).AsString;
//        end;
////        tkChar    : Result.AddField(attr.Value).Val :=lRTTIField.GetValue(_Itm).AsString;
//        tkInt64   :
//        begin
//          if _OnlyChangedFields then
//          if SameStr(_Itm.InitialFieldValues[i].Value, IntToStr(lRTTIFieldList[i].GetValue(_Itm).AsInt64))  then
//            continue;
//          Result.AddField(attr.Value).ValAsInt64 := lRTTIFieldList[i].GetValue(_Itm).AsInt64;
//        end;
//        tkEnumeration:
//        begin
//          val := lRTTIFieldList[i].GetValue(_Itm);
//          if _OnlyChangedFields then
//          if SameStr(_Itm.InitialFieldValues[i].Value, IntToStr(val.AsOrdinal))  then
//            continue;
//          if (val.TypeInfo = System.TypeInfo(Boolean)) then
//            Result.AddField(attr.Value).ValAsBool := val.AsBoolean
//          else
//            Result.AddField(attr.Value).ValAsInt := val.AsOrdinal;
//        end;
//        tkFloat:
//        begin
//          val := lRTTIFieldList[i].GetValue(_Itm);
//          if _OnlyChangedFields then
//          if SameStr(_Itm.InitialFieldValues[i].Value, FloatToStr(val.AsExtended))  then
//            continue;
//          if (val.TypeInfo = System.TypeInfo(TDate)) then
//          begin
//            Result.AddField(attr.Value).ValAsDateTime := val.AsExtended;
//          end
//          else if (val.TypeInfo = System.TypeInfo(TDateTime)) then
//          begin
//            Result.AddField(attr.Value).ValAsDateTime := val.AsExtended;
//          end
//          else if (val.TypeInfo = System.TypeInfo(TTime)) then
//          begin
//            Result.AddField(attr.Value).ValAsDateTime := val.AsExtended;
//          end
//          else
//            Result.AddField(attr.Value).ValAsDouble := val.AsExtended;
//        end;
//        else raise Exception.Create('TLandrixLibraryHelper.ToRODL unknown TypeKind '+lRTTIFieldList[i].Name);
//      end;
//
////      if CanBeRemotelyInvoked(lRTTIMethod) then
////      begin
////        aProc(lRTTIMethod);
////      end;
//    end;

//    lRTTIMethodList := lRTTIType.BaseType.GetMethods;
//    for lRTTIMethod in lRTTIMethodList do
//    begin
//      if TLandrixLibraryHelper.HasAttribute<MVCInheritableAttribute>(lRTTIMethod) and
//        CanBeRemotelyInvoked(lRTTIMethod) then
//      begin
//        aProc(lRTTIMethod);
//      end;
  finally
    lRTTI.Free;
  end;

//  xRoot := _Xml.AddChild('rsm:CrossIndustryDocument');
//  xRoot.DeclareNamespace('rsm',TargetNamespace);
//  xRoot.DeclareNamespace('xsi','http://www.w3.org/2001/XMLSchema-instance');
//  xRoot.DeclareNamespace('ram',TargetNamespaceRam);
//  xRoot.DeclareNamespace('udt',TargetNamespaceUdt);
//
  ExchangedDocumentContext.Save(xRoot);
  ExchangedDocument.Save(xRoot);
  SupplyChainTradeTransaction.Save(xRoot);
//  SpecifiedSupplyChainTradeTransaction.Save(xRoot);
//
////  xRoot.AddChild('rsm:SpecifiedExchangedDocumentContext')
////    .AddChild('ram:TestIndicator');
////  xRoot.ChildNodes.Add(xXmlI.CreateNode('text', ntText));
////  xRoot.ChildNodes.Add(xXmlI.CreateNode('node', ntElement));
//
//
////  xml := TCrossIndustryDocumentTypeHelper.NewCrossIndustryDocument;
////  xml.Encoding := 'UTF-8';
////  xml.WriteBOM := true;
////  xml.WriterSettings.LineBreak := lbDoNotProcess;
////  xml.WriterSettings.IndentType := itFlat;
//
////  xml.SpecifiedExchangedDocumentContext.TestIndicator.Indicator := true;
////  Memo1.Lines.Text := xml.Node.XML;
//
end;

function TZUGFeRDDocument.IsValid(_DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency : TZUGFeRDValidationEvent): Boolean;
begin
  Result := false;
  if _DocType = zugferdTypeCode_None then
    exit;
  Result := ExchangedDocumentContext.IsValid(_DocType,_OnInconsistency);
  if Result then
    Result := ExchangedDocument.IsValid(_DocType,_OnInconsistency);
//  if Result then
//    Result := SpecifiedSupplyChainTradeTransaction.IsValid(_DocType,_OnInconsistency);
end;

{ TZUGFeRDExchangedDocument }

constructor TZUGFeRDExchangedDocument.Create;
begin
  ID := TZUGFeRDID.Create;
  IncludedNotes := TZUGFeRDIncludedNoteList.Create;
  Clear;
end;

destructor TZUGFeRDExchangedDocument.Destroy;
begin
  if Assigned(ID) then begin ID.Free; ID := nil; end;
  if Assigned(IncludedNotes) then begin IncludedNotes.Free; IncludedNotes := nil; end;
  inherited;
end;

procedure TZUGFeRDExchangedDocument.Clear;
begin
  ID.Clear;
  Name := '';
  TypeCode := dcct_None;
  IssueDateTime := 0;
  IncludedNotes.Clear;
end;

function TZUGFeRDExchangedDocument.IsValid(_DocType : TZUGFeRDInvoiceTypeCode;
  _OnInconsistency: TZUGFeRDValidationEvent): Boolean;
begin
  Result := ID.IsValid(_DocType,_OnInconsistency);
  if TypeCode = dcct_None then
  begin
    _OnInconsistency(TZUGFeRDValidationErrors.INVALID_TYPECODE);
    Result := false;
  end;
  if IssueDateTime = 0 then
  begin
    _OnInconsistency(TZUGFeRDValidationErrors.INVALID_DATE);
    Result := false;
  end;
end;

procedure TZUGFeRDExchangedDocument.Load(_Node: IXMLNode);
var
  node, node2 : IXMLNode;
  itmin : TZUGFeRDIncludedNote;
  i,j : Integer;
begin
  TXMLHelper.LoadFromChilds('ram:ID',_Node,ID.Load);
  if TXMLHelper.FindChild(_Node,'ram:Name',node) then
    Name := node.Text;
  if TXMLHelper.FindChild(_Node,'ram:TypeCode',node) then
    TypeCode := TZUGFeRDHelper.DocumentCodeContentTypeFromStr(node.Text);

  if TXMLHelper.FindChild(_Node,'ram:IssueDateTime',node) then
  if TXMLHelper.FindChild(node,'udt:DateTimeString',node2) then
    IssueDateTime := TZUGFeRDHelper.DateFromStr(node2.Text);

  for i := 0 to _Node.ChildNodes.Count-1 do
  if SameText(_Node.ChildNodes[i].NodeName,'ram:IncludedNote') then
  begin
    node := _Node.ChildNodes[i];
    itmin := TZUGFeRDIncludedNote.Create;
    if TXMLHelper.FindChild(node,'ram:ContentCode',node2) then
      itmin.ContentCode := node2.Text;
    if TXMLHelper.FindChild(node,'ram:SubjectCode',node2) then
      itmin.SubjectCode := node2.Text;
    if TXMLHelper.FindChild(node,'ram:Content',node2) then
      itmin.Content := node2.Text;
    IncludedNotes.Add(itmin);
  end;
end;

procedure TZUGFeRDExchangedDocument.Save(_ParentNode: IXMLNode);
var
  i : Integer;
  node : IXMLNode;
begin
  _ParentNode := _ParentNode.AddChild('rsm:ExchangedDocument');
  ID.Save(_ParentNode);
  if not Name.IsEmpty then
    _ParentNode.AddChild('ram:Name').Text := Name;
  _ParentNode.AddChild('ram:TypeCode').Text := TZUGFeRDHelper.DocumentCodeContentTypeToStr(TypeCode);

  with _ParentNode.AddChild('ram:IssueDateTime').AddChild('udt:DateTimeString') do
  begin
    Attributes['format'] := '102';
    Text := TZUGFeRDHelper.DateToStr(IssueDateTime);
  end;
  for i := 0 to IncludedNotes.Count-1 do
  begin
    node := _ParentNode.AddChild('ram:IncludedNote');
    if IncludedNotes[i].ContentCode <> '' then
      node.AddChild('ram:ContentCode').Text := IncludedNotes[i].ContentCode;
    node.AddChild('ram:Content').Text := IncludedNotes[i].Content;
    if IncludedNotes[i].SubjectCode <> '' then
      node.AddChild('ram:SubjectCode').Text := IncludedNotes[i].SubjectCode;
  end;
end;

{ TZUGFeRDExchangedDocumentContext }

constructor TZUGFeRDExchangedDocumentContext.Create;
begin
  Clear;
end;

destructor TZUGFeRDExchangedDocumentContext.Destroy;
begin
  inherited;
end;

function TZUGFeRDExchangedDocumentContext.IsValid(_DocType : TZUGFeRDInvoiceTypeCode;
  _OnInconsistency: TZUGFeRDValidationEvent): Boolean;
begin
  Result := GuidelineSpecifiedDocumentContextParameter = _DocType;
  if not Result then
    _OnInconsistency(TZUGFeRDValidationErrors.INVALID_DOC_TYPE);
end;

procedure TZUGFeRDExchangedDocumentContext.Load(_Node: IXMLNode);
var
  node,node2 : IXMLNode;
begin
  if TXMLHelper.FindChild(_Node,'ram:TestIndicator',node) then
  if TXMLHelper.FindChild(node,'udt:Indicator',node2) then
    TestIndicator := StrToBoolDef(node2.Text,false);

  if TXMLHelper.FindChild(_Node,'ram:GuidelineSpecifiedDocumentContextParameter',node) then
  if TXMLHelper.FindChild(node,'ram:ID',node2) then
  begin
    if SameText(node2.Text,P_MINIMUM) then
      GuidelineSpecifiedDocumentContextParameter := zugferdTypeCode_Minimum
    else
    if SameText(node2.Text,P_BASICWL) then
      GuidelineSpecifiedDocumentContextParameter := zugferdTypeCode_BasicWL
    else
    if SameText(node2.Text,P_BASIC) then
      GuidelineSpecifiedDocumentContextParameter := zugferdTypeCode_Basic
    else
    if SameText(node2.Text,P_COMFORT) then
      GuidelineSpecifiedDocumentContextParameter := zugferdTypeCode_Comfort
    else
    if SameText(node2.Text,P_EXTENDED) then
      GuidelineSpecifiedDocumentContextParameter := zugferdTypeCode_Extended;
  end;
end;

procedure TZUGFeRDExchangedDocumentContext.Save(_ParentNode: IXMLNode);
begin
  _ParentNode := _ParentNode.AddChild('rsm:ExchangedDocumentContext');
  if TestIndicator then
    _ParentNode.AddChild('ram:TestIndicator').AddChild('udt:Indicator').Text := BoolToStr(TestIndicator,true);
  case GuidelineSpecifiedDocumentContextParameter of
    zugferdTypeCode_Minimum: _ParentNode.AddChild('ram:GuidelineSpecifiedDocumentContextParameter').AddChild('ram:ID').Text := P_MINIMUM;
    zugferdTypeCode_BasicWL: _ParentNode.AddChild('ram:GuidelineSpecifiedDocumentContextParameter').AddChild('ram:ID').Text := P_BASICWL;
    zugferdTypeCode_Basic:   _ParentNode.AddChild('ram:GuidelineSpecifiedDocumentContextParameter').AddChild('ram:ID').Text := P_BASIC;
    zugferdTypeCode_Comfort: _ParentNode.AddChild('ram:GuidelineSpecifiedDocumentContextParameter').AddChild('ram:ID').Text := P_COMFORT;
    zugferdTypeCode_Extended: _ParentNode.AddChild('ram:GuidelineSpecifiedDocumentContextParameter').AddChild('ram:ID').Text := P_EXTENDED;
  end;
end;

procedure TZUGFeRDExchangedDocumentContext.Clear;
begin
  TestIndicator := false;
  GuidelineSpecifiedDocumentContextParameter := zugferdTypeCode_None;
end;

{ TZUGFeRDID }

procedure TZUGFeRDID.Clear;
begin
  ID := '';
  SchemeID := '';
  SchemeAgencyID := '';
end;

constructor TZUGFeRDID.Create;
begin
  Clear;
end;

function TZUGFeRDID.IsValid(_DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency: TZUGFeRDValidationEvent): Boolean;
begin
  Result := true;
  if ID.IsEmpty then
  begin
    _OnInconsistency(TZUGFeRDValidationErrors.EMPTY_ID);
    Result := false;
  end;
end;

procedure TZUGFeRDID.Load(_Node: IXMLNode);
begin
  ID := _Node.Text;
  SchemeID := VarToStrDef(_Node.Attributes['schemeID'],'');
  SchemeAgencyID := VarToStrDef(_Node.Attributes['schemeAgencyID'],'');
end;

procedure TZUGFeRDID.Save(_ParentNode: IXMLNode);
begin
  _ParentNode :=_ParentNode.AddChild('ram:ID');
  _ParentNode.Text := ID;
  if not SchemeID.IsEmpty then
    _ParentNode.Attributes['schemeID'] := SchemeID;
  if not SchemeAgencyID.IsEmpty then
    _ParentNode.Attributes['schemeAgencyID'] := SchemeAgencyID;
end;

{ TZUGFeRDSupplyChainTradeTransaction }

constructor TZUGFeRDSupplyChainTradeTransaction.Create;
begin
  IncludedSupplyChainTradeLineItems := TZUGFeRDIncludedSupplyChainTradeLineItems.Create;
  ApplicableHeaderTradeAgreement := TZUGFeRDApplicableHeaderTradeAgreement.Create;
  ApplicableHeaderTradeDelivery := TZUGFeRDApplicableHeaderTradeDelivery.Create;
  ApplicableHeaderTradeSettlement := TZUGFeRDApplicableHeaderTradeSettlement.Create;
end;

destructor TZUGFeRDSupplyChainTradeTransaction.Destroy;
begin
  if Assigned(IncludedSupplyChainTradeLineItems) then begin IncludedSupplyChainTradeLineItems.Free; IncludedSupplyChainTradeLineItems := nil; end;
  if Assigned(ApplicableHeaderTradeAgreement) then begin ApplicableHeaderTradeAgreement.Free; ApplicableHeaderTradeAgreement := nil; end;
  if Assigned(ApplicableHeaderTradeDelivery) then begin ApplicableHeaderTradeDelivery.Free; ApplicableHeaderTradeDelivery := nil; end;
  if Assigned(ApplicableHeaderTradeSettlement) then begin ApplicableHeaderTradeSettlement.Free; ApplicableHeaderTradeSettlement := nil; end;
  inherited;
end;

function TZUGFeRDSupplyChainTradeTransaction.IsValid(
  _DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency: TZUGFeRDValidationEvent): Boolean;
begin
  Result := ApplicableHeaderTradeAgreement.IsValid(_DocType,_OnInconsistency);
  if Result then
    Result := ApplicableHeaderTradeDelivery.IsValid(_DocType,_OnInconsistency);
  if Result then
    Result := ApplicableHeaderTradeSettlement.IsValid(_DocType,_OnInconsistency);
  if Result then
    Result := IncludedSupplyChainTradeLineItems.IsValid(_DocType,_OnInconsistency);
end;

procedure TZUGFeRDSupplyChainTradeTransaction.Clear;
begin
  ApplicableHeaderTradeAgreement.Clear;
  ApplicableHeaderTradeDelivery.Clear;
  ApplicableHeaderTradeSettlement.Clear;
  IncludedSupplyChainTradeLineItems.Clear;
end;

procedure TZUGFeRDSupplyChainTradeTransaction.Load(
  _Node: IXMLNode);
var
  node : IXMLNode;
  i : Integer;
  itm : TZUGFeRDIncludedSupplyChainTradeLineItem;
begin
  TXMLHelper.LoadFromChilds('ram:ApplicableHeaderTradeAgreement',_Node,ApplicableHeaderTradeAgreement.Load);
  TXMLHelper.LoadFromChilds('ram:ApplicableHeaderTradeDelivery',_Node,ApplicableHeaderTradeDelivery.Load);
  TXMLHelper.LoadFromChilds('ram:ApplicableHeaderTradeSettlement',_Node,ApplicableHeaderTradeSettlement.Load);
  for i := 0 to _Node.ChildNodes.Count-1 do
  if SameText(_Node.ChildNodes[i].NodeName,'ram:IncludedSupplyChainTradeLineItem') then
  begin
    itm := TZUGFeRDIncludedSupplyChainTradeLineItem.Create;
    itm.Load(_Node.ChildNodes[i]);
    IncludedSupplyChainTradeLineItems.Add(itm);
  end;
end;

procedure TZUGFeRDSupplyChainTradeTransaction.Save(
  _ParentNode: IXMLNode);
begin
  _ParentNode := _ParentNode.AddChild('rsm:SupplyChainTradeTransaction');
  IncludedSupplyChainTradeLineItems.Save(_ParentNode);
  ApplicableHeaderTradeAgreement.Save(_ParentNode);
  ApplicableHeaderTradeDelivery.Save(_ParentNode);
  ApplicableHeaderTradeSettlement.Save(_ParentNode);
end;

//{ TZUGFeRDIncludedSupplyChainTradeLineItem }
//
//constructor TZUGFeRDIncludedSupplyChainTradeLineItem.Create;
//begin
//
//end;
//
//destructor TZUGFeRDIncludedSupplyChainTradeLineItem.Destroy;
//begin
//
//  inherited;
//end;
//
//procedure TZUGFeRDIncludedSupplyChainTradeLineItem.Clear;
//begin
//
//end;
//
procedure TZUGFeRDIncludedSupplyChainTradeLineItem.Load(
  _Node: IXMLNode);
begin
//  if _ParentNode = nil then
//    exit;
//
////		<ram:IncludedSupplyChainTradeLineItem>
////			<ram:AssociatedDocumentLineDocument>
////				<ram:LineID>10</ram:LineID>
////				<ram:IncludedNote>
////					<ram:Content></ram:Content>
////				</ram:IncludedNote>
////			</ram:AssociatedDocumentLineDocument>
////			<ram:SpecifiedSupplyChainTradeAgreement>
////				<ram:BuyerOrderReferencedDocument>
////					<ram:IssueDateTime>2018-04-03T00:00:00</ram:IssueDateTime>
////					<ram:ID>Lager</ram:ID>
////				</ram:BuyerOrderReferencedDocument>
////				<ram:GrossPriceProductTradePrice>
////					<ram:ChargeAmount currencyID="EUR">39.2000</ram:ChargeAmount>
////					<ram:BasisQuantity unitCode="C62">1.0000</ram:BasisQuantity>
////					<ram:AppliedTradeAllowanceCharge>
////						<ram:ChargeIndicator>
////							<udt:Indicator>false</udt:Indicator>
////						</ram:ChargeIndicator>
////						<ram:CalculationPercent>34.00</ram:CalculationPercent>
////						<ram:BasisAmount currencyID="EUR">39.2000</ram:BasisAmount>
////						<ram:BasisQuantity unitCode="C62">1.0000</ram:BasisQuantity>
////						<ram:ActualAmount currencyID="EUR">13.3300</ram:ActualAmount>
////						<ram:ReasonCode>DI</ram:ReasonCode>
////						<ram:Reason>Rabatt 1</ram:Reason>
////					</ram:AppliedTradeAllowanceCharge>
////				</ram:GrossPriceProductTradePrice>
////				<ram:NetPriceProductTradePrice>
////					<ram:ChargeAmount currencyID="EUR">25.8700</ram:ChargeAmount>
////					<ram:BasisQuantity unitCode="C62">1.0000</ram:BasisQuantity>
////				</ram:NetPriceProductTradePrice>
////			</ram:SpecifiedSupplyChainTradeAgreement>
////			<ram:SpecifiedSupplyChainTradeDelivery>
////				<ram:BilledQuantity unitCode="C62">1.0000</ram:BilledQuantity>
////				<ram:ShipToTradeParty>
////					<ram:ID>2000178</ram:ID>
////					<ram:Name>Kiesewetter GmbH Heizung-Sanitär</ram:Name>
////					<ram:DefinedTradeContact>
////						<ram:PersonName>Kiesewetter GmbH, Herr Kiesewetter</ram:PersonName>
////						<ram:TelephoneUniversalCommunication>
////							<ram:CompleteNumber>034721/24492</ram:CompleteNumber>
////						</ram:TelephoneUniversalCommunication>
////						<ram:FaxUniversalCommunication>
////							<ram:CompleteNumber>034721/24603</ram:CompleteNumber>
////						</ram:FaxUniversalCommunication>
////					</ram:DefinedTradeContact>
////					<ram:PostalTradeAddress>
////						<ram:PostcodeCode>06429</ram:PostcodeCode>
////						<ram:LineOne>Mittelstraße 5</ram:LineOne>
////						<ram:CityName>Nienburg</ram:CityName>
////						<ram:CountryID>DE</ram:CountryID>
////					</ram:PostalTradeAddress>
////				</ram:ShipToTradeParty>
////				<ram:ActualDeliverySupplyChainEvent>
////					<ram:OccurrenceDateTime>
////						<udt:DateTimeString format="102">20180404</udt:DateTimeString>
////					</ram:OccurrenceDateTime>
////				</ram:ActualDeliverySupplyChainEvent>
////				<ram:DeliveryNoteReferencedDocument>
////					<ram:IssueDateTime>2018-04-04T00:00:00</ram:IssueDateTime>
////					<ram:LineID>000010</ram:LineID>
////					<ram:ID>286321191</ram:ID>
////				</ram:DeliveryNoteReferencedDocument>
////			</ram:SpecifiedSupplyChainTradeDelivery>
////			<ram:SpecifiedSupplyChainTradeSettlement>
////				<ram:ApplicableTradeTax>
////					<ram:TypeCode>VAT</ram:TypeCode>
////					<ram:CategoryCode>S</ram:CategoryCode>
////					<ram:ApplicablePercent>19.00</ram:ApplicablePercent>
////				</ram:ApplicableTradeTax>
////				<ram:SpecifiedTradeSettlementHeaderMonetarySummation>
////					<ram:LineTotalAmount currencyID="EUR">25.87</ram:LineTotalAmount>
////				</ram:SpecifiedTradeSettlementHeaderMonetarySummation>
////			</ram:SpecifiedSupplyChainTradeSettlement>
////			<ram:SpecifiedTradeProduct>
////				<ram:SellerAssignedID>2075810</ram:SellerAssignedID>
////				<ram:Name>ATAG Siphon komplett für HR 5000 S4451610</ram:Name>
////			</ram:SpecifiedTradeProduct>
////		</ram:IncludedSupplyChainTradeLineItem>
////
end;
//
//procedure TZUGFeRDIncludedSupplyChainTradeLineItem.Save(_ParentNode: IXMLNode);
//begin
//  if _ParentNode = nil then
//    exit;
//  _ParentNode := _ParentNode.AddChild('ram:IncludedSupplyChainTradeLineItem');
//  _ParentNode.AddChild('ram:AssociatedDocumentLineDocument');
//  _ParentNode.AddChild('ram:SpecifiedSupplyChainTradeSettlement');
//end;
//

{ TZUGFeRDIncludedSupplyChainTradeLineItems }

function TZUGFeRDIncludedSupplyChainTradeLineItems.IsValid(
  _DocType: TZUGFeRDInvoiceTypeCode;
  _OnInconsistency: TZUGFeRDValidationEvent): Boolean;
begin

end;

procedure TZUGFeRDIncludedSupplyChainTradeLineItems.Save(_ParentNode: IXMLNode);
begin

end;

{ TZUGFeRDApplicableHeaderTradeSettlement }

constructor TZUGFeRDApplicableHeaderTradeSettlement.Create;
begin
  ApplicableTradeTaxList := TZUGFeRDApplicableTradeTaxList.Create;
  SpecifiedTradePaymentTerms := TZUGFeRDSpecifiedTradePaymentTerms.Create;
  Clear;
end;

destructor TZUGFeRDApplicableHeaderTradeSettlement.Destroy;
begin
  if Assigned(ApplicableTradeTaxList) then begin  ApplicableTradeTaxList.Free; ApplicableTradeTaxList := nil; end;
  if Assigned(SpecifiedTradePaymentTerms) then begin  SpecifiedTradePaymentTerms.Free; SpecifiedTradePaymentTerms := nil; end;
  inherited;
end;

function TZUGFeRDApplicableHeaderTradeSettlement.IsValid(
  _DocType: TZUGFeRDInvoiceTypeCode;
  _OnInconsistency: TZUGFeRDValidationEvent): Boolean;
begin
  Result := not InvoiceCurrencyCode.IsEmpty; //TODO prüfen auch Codetabelle
end;

procedure TZUGFeRDApplicableHeaderTradeSettlement.Clear;
begin
  PaymentReference := '';
  InvoiceCurrencyCode := '';
  ApplicableTradeTaxList.Clear;
  SpecifiedTradePaymentTerms.Clear;
  SpecifiedTradeSettlementHeaderMonetarySummation_LineTotalAmount := 0;
  SpecifiedTradeSettlementHeaderMonetarySummation_ChargeTotalAmount := 0;
  SpecifiedTradeSettlementHeaderMonetarySummation_AllowanceTotalAmount := 0;
  SpecifiedTradeSettlementHeaderMonetarySummation_TaxBasisTotalAmount := 0;
  SpecifiedTradeSettlementHeaderMonetarySummation_TaxBasisTotalAmountCurrID := '';
  SpecifiedTradeSettlementHeaderMonetarySummation_TaxTotalAmount := 0;
  SpecifiedTradeSettlementHeaderMonetarySummation_TaxTotalAmountCurrID := '';
  SpecifiedTradeSettlementHeaderMonetarySummation_GrandTotalAmount := 0;
  SpecifiedTradeSettlementHeaderMonetarySummation_GrandTotalAmountCurrID := '';
  SpecifiedTradeSettlementHeaderMonetarySummation_DuePayableAmount := 0;
end;

procedure TZUGFeRDApplicableHeaderTradeSettlement.Load(
  _Node: IXMLNode);
var
  node,n2 : IXMLNode;
  i : Integer;
  itm : TZUGFeRDSpecifiedTradePaymentTerm;
  itm2 : TZUGFeRDApplicableTradeTax;
begin
  if TXMLHelper.FindChild(_Node,'ram:PaymentReference',node) then
    PaymentReference := node.Text;
  if TXMLHelper.FindChild(_Node,'ram:InvoiceCurrencyCode',node) then
    InvoiceCurrencyCode := node.Text;
  for i := 0 to _Node.ChildNodes.Count-1 do
  if SameText(_Node.ChildNodes[i].NodeName,'ram:ApplicableTradeTax') then
  begin
    itm2 := TZUGFeRDApplicableTradeTax.Create;
    itm2.Load(_Node.ChildNodes[i]);
    ApplicableTradeTaxList.Add(itm2);
  end else
  if SameText(_Node.ChildNodes[i].NodeName,'ram:SpecifiedTradePaymentTerms') then
  begin
    itm := TZUGFeRDSpecifiedTradePaymentTerm.Create;
    itm.Load(_Node.ChildNodes[i]);
    SpecifiedTradePaymentTerms.Add(itm);
  end;
  if TXMLHelper.FindChild(_Node,'ram:SpecifiedTradeSettlementHeaderMonetarySummation',node) then
  begin
    if TXMLHelper.FindChild(node,'ram:LineTotalAmount',n2) then
    begin
      SpecifiedTradeSettlementHeaderMonetarySummation_LineTotalAmount       := TZUGFeRDHelper.StrToCurr(n2.Text);
    end;
    if TXMLHelper.FindChild(node,'ram:ChargeTotalAmount',n2) then
    begin
      SpecifiedTradeSettlementHeaderMonetarySummation_ChargeTotalAmount       := TZUGFeRDHelper.StrToCurr(n2.Text);
    end;
    if TXMLHelper.FindChild(node,'ram:AllowanceTotalAmount',n2) then
    begin
      SpecifiedTradeSettlementHeaderMonetarySummation_AllowanceTotalAmount       := TZUGFeRDHelper.StrToCurr(n2.Text);
    end;
    if TXMLHelper.FindChild(node,'ram:TaxBasisTotalAmount',n2) then
    begin
      SpecifiedTradeSettlementHeaderMonetarySummation_TaxBasisTotalAmount       := TZUGFeRDHelper.StrToCurr(n2.Text);
      SpecifiedTradeSettlementHeaderMonetarySummation_TaxBasisTotalAmountCurrID := VarToStrDef(n2.GetAttribute('currencyID'),'');
    end;
    if TXMLHelper.FindChild(node,'ram:TaxTotalAmount',n2) then
    begin
      SpecifiedTradeSettlementHeaderMonetarySummation_TaxTotalAmount       := TZUGFeRDHelper.StrToCurr(n2.Text);
      SpecifiedTradeSettlementHeaderMonetarySummation_TaxTotalAmountCurrID := VarToStrDef(n2.GetAttribute('currencyID'),'');
    end;
    if TXMLHelper.FindChild(node,'ram:GrandTotalAmount',n2) then
    begin
      SpecifiedTradeSettlementHeaderMonetarySummation_GrandTotalAmount       := TZUGFeRDHelper.StrToCurr(n2.Text);
      SpecifiedTradeSettlementHeaderMonetarySummation_GrandTotalAmountCurrID := VarToStrDef(n2.GetAttribute('currencyID'),'');
    end;
    if TXMLHelper.FindChild(node,'ram:DuePayableAmount',n2) then
    begin
      SpecifiedTradeSettlementHeaderMonetarySummation_DuePayableAmount       := TZUGFeRDHelper.StrToCurr(n2.Text);
    end;
  end;
end;

procedure TZUGFeRDApplicableHeaderTradeSettlement.Save(
  _ParentNode: IXMLNode);
var
  i : Integer;
  node,n2 : IXMLNode;
begin
  _ParentNode := _ParentNode.AddChild('ram:ApplicableHeaderTradeSettlement');

  if not PaymentReference.IsEmpty then _ParentNode.AddChild('ram:PaymentReference').Text := PaymentReference;
  _ParentNode.AddChild('ram:InvoiceCurrencyCode').Text := InvoiceCurrencyCode;

  for i := 0 to ApplicableTradeTaxList.Count-1 do
    ApplicableTradeTaxList[i].Save(_ParentNode);

  for i := 0 to SpecifiedTradePaymentTerms.Count-1 do
    SpecifiedTradePaymentTerms[i].Save(_ParentNode);

  node := _ParentNode.AddChild('ram:SpecifiedTradeSettlementHeaderMonetarySummation');

  n2 := node.AddChild('ram:LineTotalAmount');
  n2.Text := TZUGFeRDHelper.CurrToStr(SpecifiedTradeSettlementHeaderMonetarySummation_LineTotalAmount);

  n2 := node.AddChild('ram:ChargeTotalAmount');
  n2.Text := TZUGFeRDHelper.CurrToStr(SpecifiedTradeSettlementHeaderMonetarySummation_ChargeTotalAmount);

  n2 := node.AddChild('ram:AllowanceTotalAmount');
  n2.Text := TZUGFeRDHelper.CurrToStr(SpecifiedTradeSettlementHeaderMonetarySummation_AllowanceTotalAmount);

  n2 := node.AddChild('ram:TaxBasisTotalAmount');
  n2.Text := TZUGFeRDHelper.CurrToStr(SpecifiedTradeSettlementHeaderMonetarySummation_TaxBasisTotalAmount);
  if not SpecifiedTradeSettlementHeaderMonetarySummation_TaxBasisTotalAmountCurrID.IsEmpty then
  n2.Attributes['currencyID'] := SpecifiedTradeSettlementHeaderMonetarySummation_TaxBasisTotalAmountCurrID;

  n2 := node.AddChild('ram:TaxTotalAmount');
  n2.Text := TZUGFeRDHelper.CurrToStr(SpecifiedTradeSettlementHeaderMonetarySummation_TaxTotalAmount);
  if not SpecifiedTradeSettlementHeaderMonetarySummation_TaxTotalAmountCurrID.IsEmpty then
  n2.Attributes['currencyID'] := SpecifiedTradeSettlementHeaderMonetarySummation_TaxTotalAmountCurrID;

  n2 := node.AddChild('ram:GrandTotalAmount');
  n2.Text := TZUGFeRDHelper.CurrToStr(SpecifiedTradeSettlementHeaderMonetarySummation_GrandTotalAmount);
  if not SpecifiedTradeSettlementHeaderMonetarySummation_GrandTotalAmountCurrID.IsEmpty then
  n2.Attributes['currencyID'] := SpecifiedTradeSettlementHeaderMonetarySummation_GrandTotalAmountCurrID;

  n2 := node.AddChild('ram:DuePayableAmount');
  n2.Text := TZUGFeRDHelper.CurrToStr(SpecifiedTradeSettlementHeaderMonetarySummation_DuePayableAmount);
end;

{ TZUGFeRDSpecifiedTradePaymentTerm }

constructor TZUGFeRDSpecifiedTradePaymentTerm.Create;
begin
  Clear;
end;

function TZUGFeRDSpecifiedTradePaymentTerm.IsValid(
  _DocType: TZUGFeRDInvoiceTypeCode;
  _OnInconsistency: TZUGFeRDValidationEvent): Boolean;
begin

end;

procedure TZUGFeRDSpecifiedTradePaymentTerm.Clear;
begin
  Description := '';
  DueDateDateTime := 0;
  ApplicableTradePaymentDiscountTerms_BasisAmount := 0;
  ApplicableTradePaymentDiscountTerms_CalculationPercent := 0;
  ApplicableTradePaymentDiscountTerms_ActualDiscountAmount := 0;
end;

procedure TZUGFeRDSpecifiedTradePaymentTerm.Load(_Node: IXMLNode);
var
  node,n2,n3 : IXMLNode;
begin
  DueDateDateTime := 0;
  if TXMLHelper.FindChild(_Node,'ram:Description',node) then
    Description := node.Text;
  if TXMLHelper.FindChild(_Node,'ram:DueDateDateTime',node) then
  if TXMLHelper.FindChild(node,'udt:DateTimeString',n2) then
    DueDateDateTime := TZUGFeRDHelper.DateFromStr(n2.Text);
  if TXMLHelper.FindChild(_Node,'ram:ApplicableTradePaymentDiscountTerms',node) then
  begin
    if DueDateDateTime = 0 then
    begin
      if TXMLHelper.FindChild(node,'ram:BasisDateTime',n3) then
      if TXMLHelper.FindChild(n3,'udt:DateTimeString',n2) then
        DueDateDateTime := TZUGFeRDHelper.DateFromStr(n2.Text);
      if TXMLHelper.FindChild(node,'ram:BasisPeriodMeasure',n3) then
      if VarToStrDef(n3.GetAttribute('unitCode'),'') = 'DAY' then
        DueDateDateTime := IncDay(DueDateDateTime,StrToIntDef(n3.Text,0));
    end;

    if TXMLHelper.FindChild(node,'ram:BasisAmount',n2) then
    begin
      ApplicableTradePaymentDiscountTerms_BasisAmount := TZUGFeRDHelper.StrToCurr(n2.Text);
    end;
    if TXMLHelper.FindChild(node,'ram:CalculationPercent',n2) then
      ApplicableTradePaymentDiscountTerms_CalculationPercent  := TZUGFeRDHelper.StrToFloat(n2.Text);
    if TXMLHelper.FindChild(node,'ram:ActualDiscountAmount',n2) then
    begin
      ApplicableTradePaymentDiscountTerms_ActualDiscountAmount := TZUGFeRDHelper.StrToCurr(n2.Text);
    end;
  end;
end;

procedure TZUGFeRDSpecifiedTradePaymentTerm.Save(_ParentNode: IXMLNode);
begin
  _ParentNode := _ParentNode.AddChild('ram:SpecifiedTradePaymentTerms');
  if not Description.IsEmpty then
    _ParentNode.AddChild('ram:Description').Text := Description;
  if DueDateDateTime > 0 then
  with _ParentNode.AddChild('ram:DueDateDateTime').AddChild('udt:DateTimeString') do
  begin
    Text := TZUGFeRDHelper.DateToStr(DueDateDateTime);
    Attributes['format'] := '102';
  end;
end;

{ TZUGFeRDSpecifiedTradePaymentTerms }

function TZUGFeRDSpecifiedTradePaymentTerms.ContainsPaymentTermsByDate: Boolean;
var
  i : Integer;
begin
  Result := false;
  if Count = 0 then
    exit;
  for i := 0 to Count-1 do
  if Trunc(Items[i].DueDateDateTime) > 0 then
  begin
    Result := true;
    break;
  end;
end;

function TZUGFeRDSpecifiedTradePaymentTerms.IsValid(
  _DocType: TZUGFeRDInvoiceTypeCode;
  _OnInconsistency: TZUGFeRDValidationEvent): Boolean;
begin

end;

{ TZUGFeRDApplicableTradeTax }

constructor TZUGFeRDApplicableTradeTax.Create;
begin
  Clear;
end;

function TZUGFeRDApplicableTradeTax.IsValid(_DocType: TZUGFeRDInvoiceTypeCode;
  _OnInconsistency: TZUGFeRDValidationEvent): Boolean;
begin
  Result := true; //TODO
end;

procedure TZUGFeRDApplicableTradeTax.Clear;
begin
  CalculatedAmount       := 0;
  TypeCode               := '';
  BasisAmount            := 0;
  CategoryCode           := '';
  RateApplicablePercent      := 0;
end;

procedure TZUGFeRDApplicableTradeTax.Load(_Node: IXMLNode);
var
  node : IXMLNode;
begin
  if TXMLHelper.FindChild(_Node,'ram:CalculatedAmount',node) then
    CalculatedAmount       := TZUGFeRDHelper.StrToCurr(node.Text);
  if TXMLHelper.FindChild(_Node,'ram:TypeCode',node) then
    TypeCode               := node.Text;
  if TXMLHelper.FindChild(_Node,'ram:BasisAmount',node) then
    BasisAmount            := TZUGFeRDHelper.StrToCurr(node.Text);
  if TXMLHelper.FindChild(_Node,'ram:CategoryCode',node) then
    CategoryCode           := node.Text;
  if TXMLHelper.FindChild(_Node,'ram:RateApplicablePercent',node) then
    RateApplicablePercent      := TZUGFeRDHelper.StrToFloat(node.Text);
end;

procedure TZUGFeRDApplicableTradeTax.Save(_ParentNode: IXMLNode);
var
  node : IXMLNode;
begin
  _ParentNode :=_ParentNode.AddChild('ram:ApplicableTradeTax');

  node := _ParentNode.AddChild('ram:CalculatedAmount');
  node.Text := TZUGFeRDHelper.CurrToStr(CalculatedAmount);

  node := _ParentNode.AddChild('ram:TypeCode');
  node.Text := TypeCode;

  node := _ParentNode.AddChild('ram:BasisAmount');
  node.Text := TZUGFeRDHelper.CurrToStr(BasisAmount);

  node := _ParentNode.AddChild('ram:CategoryCode');
  node.Text := CategoryCode;

  node := _ParentNode.AddChild('ram:RateApplicablePercent');
  node.Text := TZUGFeRDHelper.FloatToStr(RateApplicablePercent);
end;

{ TZUGFeRDApplicableHeaderTradeAgreement }

procedure TZUGFeRDApplicableHeaderTradeAgreement.Clear;
begin
  BuyerReference := '';
  SellerTradeParty.Clear;
  BuyerTradeParty.Clear;
//  ShipFromTradeParty.Clear;
end;

constructor TZUGFeRDApplicableHeaderTradeAgreement.Create;
begin
  SellerTradeParty:= TZUGFeRDTradePartyType.Create;
  BuyerTradeParty:= TZUGFeRDTradePartyType.Create;
//  ShipFromTradeParty:= TZUGFeRDTradePartyType.Create;
  Clear;
end;

destructor TZUGFeRDApplicableHeaderTradeAgreement.Destroy;
begin
  if Assigned(SellerTradeParty) then begin SellerTradeParty.Free; SellerTradeParty := nil; end;
  if Assigned(BuyerTradeParty) then begin BuyerTradeParty.Free; BuyerTradeParty := nil; end;
//  if Assigned(ShipFromTradeParty) then begin ShipFromTradeParty.Free; ShipFromTradeParty := nil; end;
  inherited;
end;

function TZUGFeRDApplicableHeaderTradeAgreement.IsValid(
  _DocType: TZUGFeRDInvoiceTypeCode;
  _OnInconsistency: TZUGFeRDValidationEvent): Boolean;
begin
  Result := SellerTradeParty.IsValid(_DocType,_OnInconsistency);
  if Result then
    Result := BuyerTradeParty.IsValid(_DocType,_OnInconsistency);
end;

procedure TZUGFeRDApplicableHeaderTradeAgreement.Load(
  _Node: IXMLNode);
var
  node : IXMLNode;
begin
  if TXMLHelper.FindChild(_Node,'ram:BuyerReference',node) then
    BuyerReference := node.Text;
  TXMLHelper.LoadFromChilds('ram:SellerTradeParty',_Node,SellerTradeParty.Load);
  TXMLHelper.LoadFromChilds('ram:BuyerTradeParty',_Node,BuyerTradeParty.Load);
//  TXMLHelper.LoadFromChilds('ram:ShipFromTradeParty',_Node,ShipFromTradeParty.Load);
end;

procedure TZUGFeRDApplicableHeaderTradeAgreement.Save(
  _ParentNode: IXMLNode);
begin
  _ParentNode := _ParentNode.AddChild('ram:ApplicableHeaderTradeAgreement');
  if not BuyerReference.IsEmpty then
    _ParentNode.AddChild('ram:BuyerReference').Text := BuyerReference;
  SellerTradeParty.Save(_ParentNode.AddChild('ram:SellerTradeParty'));
  BuyerTradeParty.Save(_ParentNode.AddChild('ram:BuyerTradeParty'));
//  if not ShipFromTradeParty.Name.IsEmpty then
//    ShipFromTradeParty.Save(_ParentNode.AddChild('ram:ShipFromTradeParty'));
end;

{ TZUGFeRDApplicableHeaderTradeDelivery }

constructor TZUGFeRDApplicableHeaderTradeDelivery.Create;
begin
//  ShipToTradeParty:= TZUGFeRDTradePartyType.Create;
//  UltimateShipToTradeParty:= TZUGFeRDTradePartyType.Create;
//  ShipFromTradeParty:= TZUGFeRDTradePartyType.Create;
  Clear;
end;
//
//destructor TZUGFeRDApplicableHeaderTradeDelivery.Destroy;
//begin
//  if Assigned(ShipToTradeParty) then begin ShipToTradeParty.Free; ShipToTradeParty := nil; end;
//  if Assigned(UltimateShipToTradeParty) then begin UltimateShipToTradeParty.Free; UltimateShipToTradeParty := nil; end;
//  if Assigned(ShipFromTradeParty) then begin ShipFromTradeParty.Free; ShipFromTradeParty := nil; end;
//  inherited;
//end;

function TZUGFeRDApplicableHeaderTradeDelivery.IsValid(
  _DocType: TZUGFeRDInvoiceTypeCode;
  _OnInconsistency: TZUGFeRDValidationEvent): Boolean;
begin
  Result := true;
end;

procedure TZUGFeRDApplicableHeaderTradeDelivery.Clear;
begin
//  ShipToTradeParty.Clear;
//  UltimateShipToTradeParty.Clear;
//  ShipFromTradeParty.Clear;
  ActualDeliverySupplyChainEvent_OccurrenceDateTime := 0;
end;

procedure TZUGFeRDApplicableHeaderTradeDelivery.Load(
  _Node: IXMLNode);
var
  node,node2,node3 : IXMLNode;
begin
//  if TXMLHelper.FindChild(_Node,'ram:ShipToTradeParty',node) then
//    ShipToTradeParty.Load(node);
//  if TXMLHelper.FindChild(_Node,'ram:UltimateShipToTradeParty',node) then
//    UltimateShipToTradeParty.Load(node);
//  if TXMLHelper.FindChild(_Node,'ram:ShipFromTradeParty',node) then
//    ShipFromTradeParty.Load(node);
  if TXMLHelper.FindChild(_Node,'ram:ActualDeliverySupplyChainEvent',node) then
  if TXMLHelper.FindChild(node,'ram:OccurrenceDateTime',node2) then
  if TXMLHelper.FindChild(node2,'udt:DateTimeString',node3) then
    ActualDeliverySupplyChainEvent_OccurrenceDateTime := TZUGFeRDHelper.DateFromStr(node3.Text);
end;

procedure TZUGFeRDApplicableHeaderTradeDelivery.Save(
  _ParentNode: IXMLNode);
var
  node : IXMLNode;
begin
  _ParentNode := _ParentNode.AddChild('ram:ApplicableHeaderTradeDelivery');

  node := _ParentNode.AddChild('ram:ActualDeliverySupplyChainEvent');
  node := node.AddChild('ram:OccurrenceDateTime');
  node := node.AddChild('udt:DateTimeString');
  node.Attributes['format'] := '102';
  node.Text := TZUGFeRDHelper.DateToStr(ActualDeliverySupplyChainEvent_OccurrenceDateTime);
end;

{ TZUGFeRDPostalTradeAddress }

procedure TZUGFeRDPostalTradeAddress.Clear;
begin
  PostcodeCode := '';
  LineOne := '';
  LineTwo := '';
  LineTree := '';
  CityName := '';
  CountryID := '';
  CountrySubDivisionName := '';
end;

constructor TZUGFeRDPostalTradeAddress.Create;
begin
  Clear;
end;

function TZUGFeRDPostalTradeAddress.IsValid(_DocType: TZUGFeRDInvoiceTypeCode;
  _OnInconsistency: TZUGFeRDValidationEvent): Boolean;
begin
  Result := true;
  if CountryID.Length <> 2 then
  begin
    Result := false;
    _OnInconsistency(TZUGFeRDValidationErrors.INVALID_COUNTRYID); //TODO Test auf gültigen Ländercode
  end;
end;

procedure TZUGFeRDPostalTradeAddress.Load(_Node: IXMLNode);
var
  node : IXMLNode;
begin
  if TXMLHelper.FindChild(_Node,'ram:PostcodeCode',node) then
    PostcodeCode := node.Text;
  if TXMLHelper.FindChild(_Node,'ram:LineOne',node) then
    LineOne := node.Text;
  if TXMLHelper.FindChild(_Node,'ram:LineTwo',node) then
    LineTwo := node.Text;
  if TXMLHelper.FindChild(_Node,'ram:LineTree',node) then
    LineTree := node.Text;
  if TXMLHelper.FindChild(_Node,'ram:CityName',node) then
    CityName := node.Text;
  if TXMLHelper.FindChild(_Node,'ram:CountryID',node) then
    CountryID := node.Text;
  if TXMLHelper.FindChild(_Node,'ram:CountrySubDivisionName',node) then
    CountrySubDivisionName := node.Text;
end;

procedure TZUGFeRDPostalTradeAddress.Save(_ParentNode: IXMLNode);
begin
  _ParentNode := _ParentNode.AddChild('ram:PostalTradeAddress');
  _ParentNode.AddChild('ram:PostcodeCode').Text := PostcodeCode;
  _ParentNode.AddChild('ram:LineOne').Text := LineOne;
  if not LineTwo.IsEmpty then _ParentNode.AddChild('ram:LineTwo').Text := LineTwo;
  if not LineTree.IsEmpty then _ParentNode.AddChild('ram:LineTree').Text := LineTree;
  _ParentNode.AddChild('ram:CityName').Text := CityName;
  _ParentNode.AddChild('ram:CountryID').Text := CountryID;
  if not CountrySubDivisionName.IsEmpty then _ParentNode.AddChild('ram:CountrySubDivisionName').Text := CountrySubDivisionName;
end;

{ TZUGFeRDTradePartyType }

procedure TZUGFeRDTradePartyType.Clear;
begin
  Name := '';
  PostalTradeAddress.Clear;
  SpecifiedTaxRegistration.Clear;
end;

constructor TZUGFeRDTradePartyType.Create;
begin
  PostalTradeAddress := TZUGFeRDPostalTradeAddress.Create;
  SpecifiedTaxRegistration := TZUGFeRDIDList.Create;
  Clear;
end;

destructor TZUGFeRDTradePartyType.Destroy;
begin
  if Assigned(PostalTradeAddress) then begin PostalTradeAddress.Free; PostalTradeAddress := nil; end;
  if Assigned(SpecifiedTaxRegistration) then begin SpecifiedTaxRegistration.Free; SpecifiedTaxRegistration := nil; end;
  inherited;
end;

function TZUGFeRDTradePartyType.IsValid(_DocType: TZUGFeRDInvoiceTypeCode;
  _OnInconsistency: TZUGFeRDValidationEvent): Boolean;
begin
  Result := not (Name.IsEmpty);
  if not Result then
    _OnInconsistency(TZUGFeRDValidationErrors.EMPTY_NAME);
  if Result then
    Result := PostalTradeAddress.IsValid(_DocType,_OnInconsistency);
end;

procedure TZUGFeRDTradePartyType.Load(_Node: IXMLNode);
var
  node : IXMLNode;
  itmid : TZUGFeRDID;
  i : Integer;
begin
  if TXMLHelper.FindChild(_Node,'ram:Name',node) then
    Name := node.Text;
  TXMLHelper.LoadFromChilds('ram:PostalTradeAddress',_Node,PostalTradeAddress.Load);

  for i := 0 to _Node.ChildNodes.Count-1 do
  if SameText(_Node.ChildNodes[i].NodeName,'ram:SpecifiedTaxRegistration') then
  begin
    itmid := TZUGFeRDID.Create;
    TXMLHelper.LoadFromChilds('ram:ID',_Node.ChildNodes[i],itmid.Load);
    SpecifiedTaxRegistration.Add(itmid);
  end;
end;

procedure TZUGFeRDTradePartyType.Save(_TradePartyNode: IXMLNode);
var
  i : Integer;
begin
  _TradePartyNode.AddChild('ram:Name').Text := Name;
  PostalTradeAddress.Save(_TradePartyNode);
  for i := 0 to SpecifiedTaxRegistration.Count-1 do
    SpecifiedTaxRegistration[i].Save(_TradePartyNode.AddChild('ram:SpecifiedTaxRegistration'));
end;

{ TZUGFeRDIDList }

function TZUGFeRDIDList.AddItem: TZUGFeRDID;
begin
  Result := TZUGFeRDID.Create;
  Add(Result);
end;

{ TZUGFeRDApplicableTradeTaxList }

function TZUGFeRDApplicableTradeTaxList.AddItem: TZUGFeRDApplicableTradeTax;
begin
  Result := TZUGFeRDApplicableTradeTax.Create;
  Add(Result);
end;

function TZUGFeRDApplicableTradeTaxList.IsValid(
  _DocType: TZUGFeRDInvoiceTypeCode;
  _OnInconsistency: TZUGFeRDValidationEvent): Boolean;
begin
  Result := true;
  for var i : Integer := 0 to Count-1 do
  begin
    Result := Items[i].IsValid(_DocType,_OnInconsistency);
    if not Result then
      break;
  end;
end;

{ TZUGFeRDIncludedNoteList }

function TZUGFeRDIncludedNoteList.AddItem: TZUGFeRDIncludedNote;
begin
  Result := TZUGFeRDIncludedNote.Create;
  Add(Result);
end;

function TZUGFeRDIncludedNoteList.AsText: String;
var
  i : Integer;
begin
  Result := '';
  for i := 0 to Count-1 do
    Result := TrimLeft(Result + #13#10)+Items[i].Content;
end;

{ TZUGFeRDIncludedNote }

procedure TZUGFeRDIncludedNote.Clear;
begin
  ContentCode := '';
  Content := '';
  SubjectCode := '';
end;

constructor TZUGFeRDIncludedNote.Create;
begin
  Clear;
end;

end.
