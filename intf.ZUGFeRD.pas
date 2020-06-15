unit intf.ZUGFeRD;

interface

{$DEFINE USE_OXMLDomVendor} //http://www.kluug.net/oxml.php

uses
  System.SysUtils,System.Classes,System.Variants,System.StrUtils,
  System.DateUtils,System.Generics.Collections,
  Xml.xmldom,Xml.XMLDoc,Xml.XMLIntf,idCoderMime
  {$IFDEF USE_OXMLDomVendor},OXmlDOMVendor{$ENDIF}
  ;

  //https://github.com/konik-io/konik/blob/master/src/main/java/io/konik/zugferd/Invoice.java
type
  TZUGFeRDValidationEvent = reference to procedure (_EventLevel : Integer; const _Msg : String);


  TZUGFeRDInvoiceTypeCode = (zugferdTypeCode_None,
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
    procedure Load(_ParentNode : IXMLNode);
    procedure Save(_ParentNode : IXMLNode);
    function  IsValid(_DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
  end;

  TZUGFeRDIDList = class(TObjectList<TZUGFeRDID>)
  public
    function AddItem : TZUGFeRDID;
  end;

  TZUGFeRDExchangedDocumentContext = class(TObject)
  public const
    P_BASIC = 'urn:ferd:CrossIndustryDocument:invoice:1p0:basic';
    P_COMFORT = 'urn:ferd:CrossIndustryDocument:invoice:1p0:comfort';
    P_EXTENDED = 'urn:ferd:CrossIndustryDocument:invoice:1p0:extended';
  public
    TestIndicator : Boolean;
    GuidelineSpecifiedDocumentContextParameter : TZUGFeRDID;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Load(_ParentNode : IXMLNode);
    procedure Save(_ParentNode : IXMLNode);
    function  IsValid(_DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
  end;

  TZUGFeRDIncludedNote = class(TObject)
  public
    ContentCode : String;
    Content : TStringList;
    SubjectCode : String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
  end;

  TZUGFeRDIncludedNoteList = class(TObjectList<TZUGFeRDIncludedNote>)
  public
    function AddItem : TZUGFeRDIncludedNote;
    function AsText : String;
  end;

  TZUGFeRDExchangedDocument = class(TObject)
  public
		ID : TZUGFeRDID;
		Name : String;
		TypeCode : TZUGFeRDInvoiceTypeCode;
		IssueDateTime : TDateTime;
		IncludedNotes : TZUGFeRDIncludedNoteList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Load(_ParentNode : IXMLNode);
    procedure Save(_ParentNode : IXMLNode);
    function  IsValid(_DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
  end;

  TZUGFeRDApplicableTradeTax = class(TObject)
  public
    CalculatedAmount       : Currency;//Ab Basic   Steuerbetrag Wert
    CalculatedAmountCurrID : String;  //Ab Basic   Währung
    TypeCode               : String;  //Ab Basic   Steuerart
    BasisAmount            : Currency;//Ab Basic   Basisbetrag der Steuerberechnung Wert
    BasisAmountCurrID      : String;  //Ab Basic   Währung
    CategoryCode           : String;  //Ab Comfort Steuerkategorie Wert
    ApplicablePercent      : double;  //Ab Basic   Steuerprozentsatz Wert
  public
    constructor Create;
    procedure Clear;
    procedure Load(_Node : IXMLNode);
    procedure Save(_ParentNode : IXMLNode);
  end;

  TZUGFeRDApplicableTradeTaxList = class(TObjectList<TZUGFeRDApplicableTradeTax>)
  public
    function AddItem : TZUGFeRDApplicableTradeTax;
  end;

  TZUGFeRDSpecifiedTradePaymentTerm = class(TObject)
  public
		Description : String;
    DueDateDateTime : TDateTime;
    ApplicableTradePaymentDiscountTerms_BasisAmount : Currency;
    ApplicableTradePaymentDiscountTerms_BasisAmountCurrID : String;
    ApplicableTradePaymentDiscountTerms_CalculationPercent : double;
    ApplicableTradePaymentDiscountTerms_ActualDiscountAmount : Currency;
    ApplicableTradePaymentDiscountTerms_ActualDiscountAmountCurrID : String;
  public
    constructor Create;
    procedure Clear;
    procedure Load(_Node : IXMLNode);
    procedure Save(_ParentNode : IXMLNode);
  end;

  TZUGFeRDSpecifiedTradePaymentTerms = class(TObjectList<TZUGFeRDSpecifiedTradePaymentTerm>)
  public
    function ContainsPaymentTermsByDate : Boolean;
  end;

  TZUGFeRDPostalTradeAddress = class(TObject)
  public
    PostcodeCode : String;
    LineOne : String;
    LineTwo : String;
    CityName : String;
    CountryID : String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Load(_Node : IXMLNode);
    procedure Save(_ParentNode : IXMLNode);
  end;

  TZUGFeRDTradePartyType = class(TObject)
  public
    Name : String;
    PostalTradeAddress : TZUGFeRDPostalTradeAddress;
    SpecifiedTaxRegistration : TZUGFeRDIDList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Load(_Node : IXMLNode);
    procedure Save(_TradePartyNode : IXMLNode);
  end;

  TZUGFeRDApplicableSupplyChainTradeAgreement = class(TObject)
  public
    BuyerReference : String; //Referenz des Käufers, ab COMFORT
    SellerTradeParty: TZUGFeRDTradePartyType;
    BuyerTradeParty: TZUGFeRDTradePartyType;
    ShipFromTradeParty: TZUGFeRDTradePartyType;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Load(_Node : IXMLNode);
    procedure Save(_ParentNode : IXMLNode);
  end;

  TZUGFeRDApplicableSupplyChainTradeSettlement = class(TObject)
  public
    PaymentReference : String; //Zahlungsreferenz ab Basic für Überweisungen
    InvoiceCurrencyCode : String; //Ab Basic
    ApplicableTradeTaxList : TZUGFeRDApplicableTradeTaxList; //Ab Basic, Detailinformationen zu Steuerangaben
    SpecifiedTradePaymentTerms : TZUGFeRDSpecifiedTradePaymentTerms; //Ab Comfort
    SpecifiedTradeSettlementMonetarySummation_LineTotalAmount : Currency;         //Ab Basic Gesamtbetrag der Positionen Wert
    SpecifiedTradeSettlementMonetarySummation_LineTotalAmountCurrID : String;     //Ab Basic Währung
    SpecifiedTradeSettlementMonetarySummation_ChargeTotalAmount : Currency;       //Ab Basic Gesamtbetrag der Zuschläge
    SpecifiedTradeSettlementMonetarySummation_ChargeTotalAmountCurrID : String;   //Ab Basic Währung
    SpecifiedTradeSettlementMonetarySummation_AllowanceTotalAmount : Currency;    //Ab Basic Gesamtbetrag der Abschläge
    SpecifiedTradeSettlementMonetarySummation_AllowanceTotalAmountCurrID : String;//Ab Basic Währung
    SpecifiedTradeSettlementMonetarySummation_TaxBasisTotalAmount : Currency;     //Ab Basic Steuerbasisbetrag
    SpecifiedTradeSettlementMonetarySummation_TaxBasisTotalAmountCurrID : String;
    SpecifiedTradeSettlementMonetarySummation_TaxTotalAmount : Currency;          //Ab Basic Steuergesamtbetrag
    SpecifiedTradeSettlementMonetarySummation_TaxTotalAmountCurrID : String;
    SpecifiedTradeSettlementMonetarySummation_GrandTotalAmount : Currency;        //Ab Basic Bruttosumme
    SpecifiedTradeSettlementMonetarySummation_GrandTotalAmountCurrID : String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Load(_Node : IXMLNode);
    procedure Save(_ParentNode : IXMLNode);
  end;

  TZUGFeRDIncludedSupplyChainTradeLineItem = class(TObject)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Load(_ParentNode : IXMLNode);
    procedure Save(_ParentNode : IXMLNode);
  end;

  TZUGFeRDIncludedSupplyChainTradeLineItemList = class(TObjectList<TZUGFeRDIncludedSupplyChainTradeLineItem>);

  TZUGFeRDApplicableSupplyChainTradeDelivery = class(TObject)
  public
    ShipToTradeParty: TZUGFeRDTradePartyType;
    UltimateShipToTradeParty: TZUGFeRDTradePartyType;
    ShipFromTradeParty: TZUGFeRDTradePartyType;
    ActualDeliverySupplyChainEvent_OccurrenceDateTime : TDateTime; //Lieferzeitpunkt, Im Moment nur ein Element, deswegen kein eigenes Objekt
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Load(_Node : IXMLNode);
    procedure Save(_ParentNode : IXMLNode);
    function  IsValid(_DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
  end;

  TZUGFeRDSpecifiedSupplyChainTradeTransaction = class(TObject)
  public
    ApplicableSupplyChainTradeAgreement : TZUGFeRDApplicableSupplyChainTradeAgreement;
    ApplicableSupplyChainTradeDelivery : TZUGFeRDApplicableSupplyChainTradeDelivery;
    ApplicableSupplyChainTradeSettlement : TZUGFeRDApplicableSupplyChainTradeSettlement;
    IncludedSupplyChainTradeLineItems : TZUGFeRDIncludedSupplyChainTradeLineItemList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Load(_ParentNode : IXMLNode);
    procedure Save(_ParentNode : IXMLNode);
    function  IsValid(_DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
  end;

  TZUGFeRDCrossIndustryDocument = class(TObject)
  public
    const TargetNamespace = 'urn:ferd:CrossIndustryDocument:invoice:1p0';
    const TargetNamespaceRam = 'urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:12';
    const TargetNamespaceUdt = 'urn:un:unece:uncefact:data:standard:UnqualifiedDataType:15';
  private
    procedure LoadDocument(_Xml : IXMLDocument);
    procedure SaveDocument(_Xml : IXMLDocument);
  public
    SpecifiedExchangedDocumentContext : TZUGFeRDExchangedDocumentContext;
    HeaderExchangedDocument : TZUGFeRDExchangedDocument;
    SpecifiedSupplyChainTradeTransaction : TZUGFeRDSpecifiedSupplyChainTradeTransaction;
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
//  edBrutto.SetFloat(_Itm.SpecifiedSupplyChainTradeTransaction.ApplicableSupplyChainTradeSettlement.SpecifiedTradeSettlementMonetarySummation_GrandTotalAmount);
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
//  if (CurrentRechnung.RgNetto <> _Itm.SpecifiedSupplyChainTradeTransaction.ApplicableSupplyChainTradeSettlement.SpecifiedTradeSettlementMonetarySummation_TaxBasisTotalAmount) then
//    MessageDlg('Der berechnete Netto-Betrag weicht vom dem im Dokument ab.', mtError, [mbOK], 0);



implementation

type
  TZUGFeRDCrossIndustryDocumentHelper = class(TObject)
  public
    class function DateFromStr(const _Val : String) : TDateTime;
    class function DateToStr(const _Val : TDateTime) : String;
    class function StrToCurr(_Val : String) : Currency;
    class function StrToFloat(_Val : String) : double;
    class function CurrToStr(_Val : Currency) : String;
    class function FloatToStr(_Val : double) : String;
  end;

  TXMLHelper = class(TObject)
  public
    class function FindChild(_Node : IXMLNode; const _NodeName : String; out _Result : IXMLNode) : Boolean;
  end;

{ TXMLHelper }

class function TXMLHelper.FindChild(_Node : IXMLNode; const _NodeName: String;
  out _Result: IXMLNode): Boolean;
begin
  Result := false;
  if _Node = nil then
    exit;
  if _Node.ChildNodes.FindNode(_NodeName,'') = nil then
    exit;
  _Result := _Node.ChildNodes.FindNode(_NodeName,'');
  Result := true;
end;

{ TZUGFeRDCrossIndustryDocument }

constructor TZUGFeRDCrossIndustryDocument.Create;
begin
  SpecifiedExchangedDocumentContext := TZUGFeRDExchangedDocumentContext.Create;
  HeaderExchangedDocument := TZUGFeRDExchangedDocument.Create;
  SpecifiedSupplyChainTradeTransaction := TZUGFeRDSpecifiedSupplyChainTradeTransaction.Create;
  Clear;
end;

destructor TZUGFeRDCrossIndustryDocument.Destroy;
begin
  if Assigned(SpecifiedExchangedDocumentContext) then begin  SpecifiedExchangedDocumentContext.Free; SpecifiedExchangedDocumentContext := nil; end;
  if Assigned(HeaderExchangedDocument) then begin HeaderExchangedDocument.Free; HeaderExchangedDocument := nil; end;
  if Assigned(SpecifiedSupplyChainTradeTransaction) then begin SpecifiedSupplyChainTradeTransaction.Free; SpecifiedSupplyChainTradeTransaction := nil; end;
  inherited;
end;

procedure TZUGFeRDCrossIndustryDocument.Clear;
begin
  SpecifiedExchangedDocumentContext.Clear;
  HeaderExchangedDocument.Clear;
  SpecifiedSupplyChainTradeTransaction.Clear;
end;

procedure TZUGFeRDCrossIndustryDocument.LoadFromFile(const _Filename : String);
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

procedure TZUGFeRDCrossIndustryDocument.LoadFromMime(const _XMLMime: String);
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

procedure TZUGFeRDCrossIndustryDocument.LoadFromStream(_Stream : TStream);
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

procedure TZUGFeRDCrossIndustryDocument.LoadFromXMLStr(const _XML: String);
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

class function TZUGFeRDCrossIndustryDocument.MimeStrToStream(
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

procedure TZUGFeRDCrossIndustryDocument.SaveToFile(const _Filename: String);
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

procedure TZUGFeRDCrossIndustryDocument.SaveToMime(out _XMLMime: String);
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
//    mime.Encode(strin,strout);
//    strout.Position := 0;
//    SetLength(_XMLMime,strout.size);
//    strout.Read(_XMLMime[1],strout.size);
  finally
    strin.Free;
    mime.Free;
  end;
end;

procedure TZUGFeRDCrossIndustryDocument.SaveToStream(_Stream: TStream);
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

procedure TZUGFeRDCrossIndustryDocument.SaveToXMLStr(out _XML: String);
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

procedure TZUGFeRDCrossIndustryDocument.LoadDocument(
  _Xml: IXMLDocument);
begin
  if not SameText(_Xml.DocumentElement.LocalName,'CrossIndustryDocument') then
    exit;
  SpecifiedExchangedDocumentContext.Load(_Xml.DocumentElement);
  HeaderExchangedDocument.Load(_Xml.DocumentElement);
  SpecifiedSupplyChainTradeTransaction.Load(_Xml.DocumentElement);

//  Result := xmldoc.GetDocBinding('CrossIndustryDocument', TXMLCrossIndustryDocumentType) as IXMLCrossIndustryDocumentType;
//  Result.DeclareNamespace('rsm',TargetNamespace);

//  end;
//  ShowMessage(_xml.ChildNodes['HeaderExchangedDocument'].Text);
//  exit;
//
//  Header.ID := _xml.HeaderExchangedDocument.ID.SchemeID;
//  if _xml.HeaderExchangedDocument.Name.Count > 0 then
//    Header.Name := _xml.HeaderExchangedDocument.Name.Items[0].Text;
//  if SameText(_xml.HeaderExchangedDocument.TypeCode.Text,'380') then
//    Header.TypeCode := zugferdTypeCode_Basic
//  else
//  if SameText(_xml.HeaderExchangedDocument.TypeCode.Text,'84') then
//    Header.TypeCode := zugferdTypeCode_Comfort
//  else
//  if SameText(_xml.HeaderExchangedDocument.TypeCode.Text,'389') then
//    Header.TypeCode := zugferdTypeCode_Extended;
//  Header.IssueDateTime := TCrossIndustryDocumentTypeHelper.DateFromStr(_xml.HeaderExchangedDocument.IssueDateTime.DateTimeString.Text);
////  Header.SubjectCode := _xml.HeaderExchangedDocument.EffectiveSpecifiedPeriod.

end;

procedure TZUGFeRDCrossIndustryDocument.SaveDocument(
  _Xml: IXMLDocument);
var
  xRoot : IXMLNode;
begin
  {$IFDEF USE_OXMLDomVendor}TXMLDocument(_Xml).DOMVendor := Xml.xmldom.GetDOMVendor(sOXmlDOMVendor);{$ENDIF}
  //Result := xmldoc.GetDocBinding('rsm:CrossIndustryDocument', TXMLCrossIndustryDocumentType) as IXMLCrossIndustryDocumentType;
  TXMLDocument(_Xml).Options := TXMLDocument(_Xml).Options + [doNodeAutoIndent];
  _Xml.Active := True;
  _Xml.Version := '1.0';
  _Xml.StandAlone := 'yes';
  _Xml.Encoding := 'UTF-8';

  _Xml.Options := [doNodeAutoCreate, doNodeAutoIndent, doAttrNull];

  xRoot := _Xml.AddChild('rsm:CrossIndustryDocument');
  xRoot.DeclareNamespace('rsm',TargetNamespace);
  xRoot.DeclareNamespace('xsi','http://www.w3.org/2001/XMLSchema-instance');
  xRoot.DeclareNamespace('ram',TargetNamespaceRam);
  xRoot.DeclareNamespace('udt',TargetNamespaceUdt);

  SpecifiedExchangedDocumentContext.Save(xRoot);
  HeaderExchangedDocument.Save(xRoot);
  SpecifiedSupplyChainTradeTransaction.Save(xRoot);

//  xRoot.AddChild('rsm:SpecifiedExchangedDocumentContext')
//    .AddChild('ram:TestIndicator');
//  xRoot.ChildNodes.Add(xXmlI.CreateNode('text', ntText));
//  xRoot.ChildNodes.Add(xXmlI.CreateNode('node', ntElement));


//  xml := TCrossIndustryDocumentTypeHelper.NewCrossIndustryDocument;
//  xml.Encoding := 'UTF-8';
//  xml.WriteBOM := true;
//  xml.WriterSettings.LineBreak := lbDoNotProcess;
//  xml.WriterSettings.IndentType := itFlat;

//  xml.SpecifiedExchangedDocumentContext.TestIndicator.Indicator := true;
//  Memo1.Lines.Text := xml.Node.XML;

end;

function TZUGFeRDCrossIndustryDocument.IsValid(_DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency : TZUGFeRDValidationEvent): Boolean;
begin
  Result := false;
  if _DocType = zugferdTypeCode_None then
    exit;
  Result := SpecifiedExchangedDocumentContext.IsValid(_DocType,_OnInconsistency);
  if Result then
    Result := HeaderExchangedDocument.IsValid(_DocType,_OnInconsistency);
  if Result then
    Result := SpecifiedSupplyChainTradeTransaction.IsValid(_DocType,_OnInconsistency);
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
  TypeCode := zugferdTypeCode_None;
  IssueDateTime := 0;
  IncludedNotes.Clear;
end;

function TZUGFeRDExchangedDocument.IsValid(_DocType : TZUGFeRDInvoiceTypeCode;
  _OnInconsistency: TZUGFeRDValidationEvent): Boolean;
begin
  Result := ID.IsValid(_DocType,_OnInconsistency);
  if Name.IsEmpty then
  begin
    _OnInconsistency(-1,'Element ram:Name must occur exactly 1 times.');
    Result := false;
  end;
  if TypeCode = zugferdTypeCode_None then
  begin
    _OnInconsistency(-1,'Element ram:TypeCode must occur exactly 1 times.');
    Result := false;
  end;
  if IssueDateTime = 0 then
  begin
    _OnInconsistency(-1,'Element ram:IssueDateTime must occur exactly 1 times.');
    Result := false;
  end;
  if TypeCode in [zugferdTypeCode_Comfort,zugferdTypeCode_Extended] then
  begin
    _OnInconsistency(-1,'Element ram:TypeCode only 380 allowed.');
    Result := false;
  end;
  if _DocType = zugferdTypeCode_Comfort then
  if TypeCode in [zugferdTypeCode_Extended] then
  begin
    _OnInconsistency(-1,'Element ram:TypeCode only 380,84 allowed.');
    Result := false;
  end;
end;

procedure TZUGFeRDExchangedDocument.Load(_ParentNode: IXMLNode);
var
  node, n : IXMLNode;
  itmin : TZUGFeRDIncludedNote;
  i,j : Integer;
begin
  if _ParentNode = nil then
    exit;
  node := _ParentNode.ChildNodes.FindNode('rsm:HeaderExchangedDocument');
  if node = nil then
    node := _ParentNode.ChildNodes.FindNode('HeaderExchangedDocument');
  if node = nil then
    exit;
  ID.Load(node);
  if node.ChildNodes.FindNode('ram:Name','') <> nil then
    Name := node.ChildNodes.FindNode('ram:Name','').Text;
  if node.ChildNodes.FindNode('ram:TypeCode','') <> nil then
  case StrToIntDef(node.ChildNodes.FindNode('ram:TypeCode','').Text,0) of
    380 : TypeCode := zugferdTypeCode_Basic;
     84 : TypeCode := zugferdTypeCode_Comfort;
    389 : TypeCode := zugferdTypeCode_Extended;
    else  TypeCode := zugferdTypeCode_None;
  end;
  if node.ChildNodes.FindNode('ram:IssueDateTime','') <> nil then
  if node.ChildNodes.FindNode('ram:IssueDateTime','').ChildNodes.FindNode('udt:DateTimeString','') <> nil then
    IssueDateTime := TZUGFeRDCrossIndustryDocumentHelper.DateFromStr(node.ChildNodes.FindNode('ram:IssueDateTime','').ChildNodes.FindNode('udt:DateTimeString','').Text);

  for i := 0 to node.ChildNodes.Count-1 do
  if SameText(node.ChildNodes[i].NodeName,'ram:IncludedNote') then
  begin
    itmin := TZUGFeRDIncludedNote.Create;
    if TXMLHelper.FindChild(node.ChildNodes[i],'ram:ContentCode',n) then
      itmin.ContentCode := n.Text;
    if TXMLHelper.FindChild(node.ChildNodes[i],'ram:SubjectCode',n) then
      itmin.SubjectCode := n.Text;

    for j := 0 to node.ChildNodes[i].ChildNodes.Count-1 do
    if SameText(node.ChildNodes[i].ChildNodes[j].NodeName,'ram:Content') then
      itmin.Content.Add(node.ChildNodes[i].ChildNodes[j].Text);

    IncludedNotes.Add(itmin);
  end;
end;

procedure TZUGFeRDExchangedDocument.Save(_ParentNode: IXMLNode);
var
  i,j : Integer;
  node : IXMLNode;
begin
  if _ParentNode = nil then
    exit;
  _ParentNode := _ParentNode.AddChild('rsm:HeaderExchangedDocument');
  ID.Save(_ParentNode);
  _ParentNode.AddChild('ram:Name').Text := Name;
  case TypeCode of
    zugferdTypeCode_Basic   : _ParentNode.AddChild('ram:TypeCode').Text := '380';
    zugferdTypeCode_Comfort : _ParentNode.AddChild('ram:TypeCode').Text :=  '84';
    zugferdTypeCode_Extended: _ParentNode.AddChild('ram:TypeCode').Text := '389';
  end;
  with _ParentNode.AddChild('ram:IssueDateTime').AddChild('udt:DateTimeString') do
  begin
    Attributes['format'] := '102';
    Text := TZUGFeRDCrossIndustryDocumentHelper.DateToStr(IssueDateTime);
  end;
  for i := 0 to IncludedNotes.Count-1 do
  begin
    node := _ParentNode.AddChild('ram:IncludedNote');
    if IncludedNotes[i].ContentCode <> '' then
      node.AddChild('ram:ContentCode').Text := IncludedNotes[i].ContentCode;
    for j := 0 to IncludedNotes[i].Content.Count-1 do
      node.AddChild('ram:Content').Text := IncludedNotes[i].Content[j];
    if IncludedNotes[i].SubjectCode <> '' then
      node.AddChild('ram:SubjectCode').Text := IncludedNotes[i].SubjectCode;
  end;
end;

{ TZUGFeRDExchangedDocumentContext }

constructor TZUGFeRDExchangedDocumentContext.Create;
begin
  GuidelineSpecifiedDocumentContextParameter := TZUGFeRDID.Create;
  Clear;
end;

destructor TZUGFeRDExchangedDocumentContext.Destroy;
begin
  if Assigned(GuidelineSpecifiedDocumentContextParameter) then begin  GuidelineSpecifiedDocumentContextParameter.Free; GuidelineSpecifiedDocumentContextParameter := nil; end;
  inherited;
end;

function TZUGFeRDExchangedDocumentContext.IsValid(_DocType : TZUGFeRDInvoiceTypeCode;
  _OnInconsistency: TZUGFeRDValidationEvent): Boolean;
begin
  Result := GuidelineSpecifiedDocumentContextParameter.IsValid(_DocType,_OnInconsistency);
end;

procedure TZUGFeRDExchangedDocumentContext.Load(_ParentNode: IXMLNode);
var
  node,node2 : IXMLNode;
begin
  if _ParentNode = nil then
    exit;

  node := _ParentNode.ChildNodes.FindNode('rsm:SpecifiedExchangedDocumentContext');
  if node = nil then
    node := _ParentNode.ChildNodes.FindNode('SpecifiedExchangedDocumentContext');
  if node = nil then
    exit;

  node2 := node.ChildNodes.FindNode('ram:TestIndicator','');
  if node2 <> nil then
  begin
    node2 := node2.ChildNodes.FindNode('udt:Indicator','');
    if node2 <> nil then
      TestIndicator := StrToBoolDef(node2.Text,false);
  end;
  node2 := node.ChildNodes.FindNode('ram:GuidelineSpecifiedDocumentContextParameter','');
  if node2 <> nil then
    GuidelineSpecifiedDocumentContextParameter.Load(node2);
end;

procedure TZUGFeRDExchangedDocumentContext.Save(_ParentNode: IXMLNode);
begin
  if _ParentNode = nil then
    exit;
  _ParentNode := _ParentNode.AddChild('rsm:SpecifiedExchangedDocumentContext');
  if TestIndicator then
    _ParentNode.AddChild('ram:TestIndicator').AddChild('udt:Indicator').Text := BoolToStr(TestIndicator,true);
  GuidelineSpecifiedDocumentContextParameter.Save(_ParentNode.AddChild('ram:GuidelineSpecifiedDocumentContextParameter'));
end;

procedure TZUGFeRDExchangedDocumentContext.Clear;
begin
  TestIndicator := false;
  GuidelineSpecifiedDocumentContextParameter.Clear;
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
    _OnInconsistency(-1,'Element ram:ID must occur exactly 1 times.');
    Result := false;
  end;
end;

procedure TZUGFeRDID.Load(_ParentNode: IXMLNode);
var
  node : IXMLNode;
begin
  if _ParentNode = nil then
    exit;
  node := _ParentNode.ChildNodes.FindNode('ram:ID','');
  if node <> nil then
  begin
    ID := node.Text;
    SchemeID := VarToStrDef(node.Attributes['schemeID'],'');
    SchemeAgencyID := VarToStrDef(node.Attributes['schemeAgencyID'],'');
  end;
end;

procedure TZUGFeRDID.Save(_ParentNode: IXMLNode);
var
  node : IXMLNode;
begin
  if _ParentNode = nil then
    exit;
  node :=_ParentNode.AddChild('ram:ID');
  node.Text := ID;
  if not SchemeID.IsEmpty then
    node.Attributes['schemeID'] := SchemeID;
  if not SchemeAgencyID.IsEmpty then
    node.Attributes['schemeAgencyID'] := SchemeAgencyID;
end;

{ TZUGFeRDCrossIndustryDocumentHelper }

class function TZUGFeRDCrossIndustryDocumentHelper.CurrToStr(
  _Val: Currency): String;
begin
  Result := ReplaceText(Format('%.2f',[_Val]),',','.');
end;

class function TZUGFeRDCrossIndustryDocumentHelper.DateFromStr(
  const _Val: String): TDateTime;
begin
  Result := 0;
  if Length(_Val) <> 8 then
    exit;
  Result := EncodeDate(StrToIntDef(Copy(_Val,1,4),1999),StrToIntDef(Copy(_Val,5,2),1),StrToIntDef(Copy(_Val,7,2),1));
end;

class function TZUGFeRDCrossIndustryDocumentHelper.DateToStr(
  const _Val: TDateTime): String;
begin
  Result := FormatDateTime('yyyymmdd',_Val);
end;

class function TZUGFeRDCrossIndustryDocumentHelper.FloatToStr(
  _Val: double): String;
begin
  Result := ReplaceText(Format('%.2f',[_Val]),',','.');
end;

class function TZUGFeRDCrossIndustryDocumentHelper.StrToCurr(
  _Val: String): Currency;
begin
  _Val := ReplaceText(_Val,'.',FormatSettings.DecimalSeparator);
  result := StrToCurrDef(_Val,0);
end;

class function TZUGFeRDCrossIndustryDocumentHelper.StrToFloat(
  _Val: String): double;
begin
  _Val := ReplaceText(_Val,'.',FormatSettings.DecimalSeparator);
  result := StrToFloatDef(_Val,0);
end;

{ TZUGFeRDSpecifiedSupplyChainTradeTransaction }

constructor TZUGFeRDSpecifiedSupplyChainTradeTransaction.Create;
begin
  ApplicableSupplyChainTradeAgreement := TZUGFeRDApplicableSupplyChainTradeAgreement.Create;
  ApplicableSupplyChainTradeDelivery := TZUGFeRDApplicableSupplyChainTradeDelivery.Create;
  IncludedSupplyChainTradeLineItems := TZUGFeRDIncludedSupplyChainTradeLineItemList.Create;
  ApplicableSupplyChainTradeSettlement := TZUGFeRDApplicableSupplyChainTradeSettlement.Create;
end;

destructor TZUGFeRDSpecifiedSupplyChainTradeTransaction.Destroy;
begin
  if Assigned(ApplicableSupplyChainTradeDelivery) then begin ApplicableSupplyChainTradeDelivery.Free; ApplicableSupplyChainTradeDelivery := nil; end;
  if Assigned(IncludedSupplyChainTradeLineItems) then begin  IncludedSupplyChainTradeLineItems.Free; IncludedSupplyChainTradeLineItems := nil; end;
  if Assigned(ApplicableSupplyChainTradeSettlement) then begin  ApplicableSupplyChainTradeSettlement.Free; ApplicableSupplyChainTradeSettlement := nil; end;
  if Assigned(ApplicableSupplyChainTradeAgreement) then begin  ApplicableSupplyChainTradeAgreement.Free; ApplicableSupplyChainTradeAgreement := nil; end;
  inherited;
end;

function TZUGFeRDSpecifiedSupplyChainTradeTransaction.IsValid(
  _DocType : TZUGFeRDInvoiceTypeCode; _OnInconsistency: TZUGFeRDValidationEvent): Boolean;
begin
//TODO  Result := ApplicableSupplyChainTradeAgreement.IsValid(_DocType,_OnInconsistency);
//  if Result then
    Result := ApplicableSupplyChainTradeDelivery.IsValid(_DocType,_OnInconsistency);
//  if Result then
//    Result := IncludedSupplyChainTradeLineItems.IsValid(_DocType,_OnInconsistency);
//  if Result then
//    Result := ApplicableSupplyChainTradeSettlement.IsValid(_DocType,_OnInconsistency);
end;

procedure TZUGFeRDSpecifiedSupplyChainTradeTransaction.Clear;
begin
  IncludedSupplyChainTradeLineItems.Clear;
  ApplicableSupplyChainTradeSettlement.Clear;
  ApplicableSupplyChainTradeAgreement.Clear;
end;

procedure TZUGFeRDSpecifiedSupplyChainTradeTransaction.Load(
  _ParentNode: IXMLNode);
var
  node : IXMLNode;
  i : Integer;
  itm : TZUGFeRDIncludedSupplyChainTradeLineItem;
begin
  if _ParentNode = nil then
    exit;

  node := _ParentNode.ChildNodes.FindNode('rsm:SpecifiedSupplyChainTradeTransaction');
  if node = nil then
    node := _ParentNode.ChildNodes.FindNode('SpecifiedSupplyChainTradeTransaction');
  if node = nil then
    exit;

  for i := 0 to node.ChildNodes.Count-1 do
  if SameText(node.ChildNodes[i].NodeName,'ram:ApplicableSupplyChainTradeDelivery') then
    ApplicableSupplyChainTradeDelivery.Load(node.ChildNodes[i])
  else
  if SameText(node.ChildNodes[i].NodeName,'ram:IncludedSupplyChainTradeLineItem') then
  begin
    itm := TZUGFeRDIncludedSupplyChainTradeLineItem.Create;
    itm.Load(node.ChildNodes[i]);
    IncludedSupplyChainTradeLineItems.Add(itm);
  end else
  if SameText(node.ChildNodes[i].NodeName,'ram:ApplicableSupplyChainTradeSettlement') then
    ApplicableSupplyChainTradeSettlement.Load(node.ChildNodes[i])
  else
  if SameText(node.ChildNodes[i].NodeName,'ram:ApplicableSupplyChainTradeAgreement') then
    ApplicableSupplyChainTradeAgreement.Load(node.ChildNodes[i]);

//    <ram:ApplicableSupplyChainTradeDelivery>
//      <ram:ShipToTradeParty>
//        <ram:Name>Rögo Heizung &amp; Sanitär Inh. Thomas Nitsche</ram:Name>
//        <ram:PostalTradeAddress>
//          <ram:PostcodeCode>02829</ram:PostcodeCode>
//          <ram:LineOne>Hauptstr. 6</ram:LineOne>
//          <ram:CityName>Schöpstal</ram:CityName>
//          <ram:CountryID>DE</ram:CountryID>
//        </ram:PostalTradeAddress>
//      </ram:ShipToTradeParty>
//      <ram:DeliveryNoteReferencedDocument>
//        <ram:IssueDateTime>2018-06-29T00:00:00</ram:IssueDateTime>
//        <ram:ID>18325009-001</ram:ID>
//      </ram:DeliveryNoteReferencedDocument>
//    </ram:ApplicableSupplyChainTradeDelivery>
end;

procedure TZUGFeRDSpecifiedSupplyChainTradeTransaction.Save(
  _ParentNode: IXMLNode);
var
  node : IXMLNode;
  i : Integer;
begin
  if _ParentNode = nil then
    exit;
  node :=_ParentNode.AddChild('rsm:SpecifiedSupplyChainTradeTransaction');
  ApplicableSupplyChainTradeAgreement.Save(node);
  ApplicableSupplyChainTradeDelivery.Save(node);
  ApplicableSupplyChainTradeSettlement.Save(node);
  for i := 0 to IncludedSupplyChainTradeLineItems.Count-1 do
    IncludedSupplyChainTradeLineItems[i].Save(node);
end;

{ TZUGFeRDIncludedSupplyChainTradeLineItem }

constructor TZUGFeRDIncludedSupplyChainTradeLineItem.Create;
begin

end;

destructor TZUGFeRDIncludedSupplyChainTradeLineItem.Destroy;
begin

  inherited;
end;

procedure TZUGFeRDIncludedSupplyChainTradeLineItem.Clear;
begin

end;

procedure TZUGFeRDIncludedSupplyChainTradeLineItem.Load(
  _ParentNode: IXMLNode);
begin
  if _ParentNode = nil then
    exit;

//		<ram:IncludedSupplyChainTradeLineItem>
//			<ram:AssociatedDocumentLineDocument>
//				<ram:LineID>10</ram:LineID>
//				<ram:IncludedNote>
//					<ram:Content></ram:Content>
//				</ram:IncludedNote>
//			</ram:AssociatedDocumentLineDocument>
//			<ram:SpecifiedSupplyChainTradeAgreement>
//				<ram:BuyerOrderReferencedDocument>
//					<ram:IssueDateTime>2018-04-03T00:00:00</ram:IssueDateTime>
//					<ram:ID>Lager</ram:ID>
//				</ram:BuyerOrderReferencedDocument>
//				<ram:GrossPriceProductTradePrice>
//					<ram:ChargeAmount currencyID="EUR">39.2000</ram:ChargeAmount>
//					<ram:BasisQuantity unitCode="C62">1.0000</ram:BasisQuantity>
//					<ram:AppliedTradeAllowanceCharge>
//						<ram:ChargeIndicator>
//							<udt:Indicator>false</udt:Indicator>
//						</ram:ChargeIndicator>
//						<ram:CalculationPercent>34.00</ram:CalculationPercent>
//						<ram:BasisAmount currencyID="EUR">39.2000</ram:BasisAmount>
//						<ram:BasisQuantity unitCode="C62">1.0000</ram:BasisQuantity>
//						<ram:ActualAmount currencyID="EUR">13.3300</ram:ActualAmount>
//						<ram:ReasonCode>DI</ram:ReasonCode>
//						<ram:Reason>Rabatt 1</ram:Reason>
//					</ram:AppliedTradeAllowanceCharge>
//				</ram:GrossPriceProductTradePrice>
//				<ram:NetPriceProductTradePrice>
//					<ram:ChargeAmount currencyID="EUR">25.8700</ram:ChargeAmount>
//					<ram:BasisQuantity unitCode="C62">1.0000</ram:BasisQuantity>
//				</ram:NetPriceProductTradePrice>
//			</ram:SpecifiedSupplyChainTradeAgreement>
//			<ram:SpecifiedSupplyChainTradeDelivery>
//				<ram:BilledQuantity unitCode="C62">1.0000</ram:BilledQuantity>
//				<ram:ShipToTradeParty>
//					<ram:ID>2000178</ram:ID>
//					<ram:Name>Kiesewetter GmbH Heizung-Sanitär</ram:Name>
//					<ram:DefinedTradeContact>
//						<ram:PersonName>Kiesewetter GmbH, Herr Kiesewetter</ram:PersonName>
//						<ram:TelephoneUniversalCommunication>
//							<ram:CompleteNumber>034721/24492</ram:CompleteNumber>
//						</ram:TelephoneUniversalCommunication>
//						<ram:FaxUniversalCommunication>
//							<ram:CompleteNumber>034721/24603</ram:CompleteNumber>
//						</ram:FaxUniversalCommunication>
//					</ram:DefinedTradeContact>
//					<ram:PostalTradeAddress>
//						<ram:PostcodeCode>06429</ram:PostcodeCode>
//						<ram:LineOne>Mittelstraße 5</ram:LineOne>
//						<ram:CityName>Nienburg</ram:CityName>
//						<ram:CountryID>DE</ram:CountryID>
//					</ram:PostalTradeAddress>
//				</ram:ShipToTradeParty>
//				<ram:ActualDeliverySupplyChainEvent>
//					<ram:OccurrenceDateTime>
//						<udt:DateTimeString format="102">20180404</udt:DateTimeString>
//					</ram:OccurrenceDateTime>
//				</ram:ActualDeliverySupplyChainEvent>
//				<ram:DeliveryNoteReferencedDocument>
//					<ram:IssueDateTime>2018-04-04T00:00:00</ram:IssueDateTime>
//					<ram:LineID>000010</ram:LineID>
//					<ram:ID>286321191</ram:ID>
//				</ram:DeliveryNoteReferencedDocument>
//			</ram:SpecifiedSupplyChainTradeDelivery>
//			<ram:SpecifiedSupplyChainTradeSettlement>
//				<ram:ApplicableTradeTax>
//					<ram:TypeCode>VAT</ram:TypeCode>
//					<ram:CategoryCode>S</ram:CategoryCode>
//					<ram:ApplicablePercent>19.00</ram:ApplicablePercent>
//				</ram:ApplicableTradeTax>
//				<ram:SpecifiedTradeSettlementMonetarySummation>
//					<ram:LineTotalAmount currencyID="EUR">25.87</ram:LineTotalAmount>
//				</ram:SpecifiedTradeSettlementMonetarySummation>
//			</ram:SpecifiedSupplyChainTradeSettlement>
//			<ram:SpecifiedTradeProduct>
//				<ram:SellerAssignedID>2075810</ram:SellerAssignedID>
//				<ram:Name>ATAG Siphon komplett für HR 5000 S4451610</ram:Name>
//			</ram:SpecifiedTradeProduct>
//		</ram:IncludedSupplyChainTradeLineItem>
//
end;

procedure TZUGFeRDIncludedSupplyChainTradeLineItem.Save(_ParentNode: IXMLNode);
begin
  if _ParentNode = nil then
    exit;
  _ParentNode := _ParentNode.AddChild('ram:IncludedSupplyChainTradeLineItem');
  _ParentNode.AddChild('ram:AssociatedDocumentLineDocument');
  _ParentNode.AddChild('ram:SpecifiedSupplyChainTradeSettlement');
end;

{ TZUGFeRDApplicableSupplyChainTradeSettlement }

constructor TZUGFeRDApplicableSupplyChainTradeSettlement.Create;
begin
  ApplicableTradeTaxList := TZUGFeRDApplicableTradeTaxList.Create;
  SpecifiedTradePaymentTerms := TZUGFeRDSpecifiedTradePaymentTerms.Create;
end;

destructor TZUGFeRDApplicableSupplyChainTradeSettlement.Destroy;
begin
  if Assigned(ApplicableTradeTaxList) then begin  ApplicableTradeTaxList.Free; ApplicableTradeTaxList := nil; end;
  if Assigned(SpecifiedTradePaymentTerms) then begin  SpecifiedTradePaymentTerms.Free; SpecifiedTradePaymentTerms := nil; end;
  inherited;
end;

procedure TZUGFeRDApplicableSupplyChainTradeSettlement.Clear;
begin
  PaymentReference := '';
  InvoiceCurrencyCode := '';
  ApplicableTradeTaxList.Clear;
  SpecifiedTradePaymentTerms.Clear;
  SpecifiedTradeSettlementMonetarySummation_LineTotalAmount := 0;
  SpecifiedTradeSettlementMonetarySummation_LineTotalAmountCurrID := '';
  SpecifiedTradeSettlementMonetarySummation_ChargeTotalAmount := 0;
  SpecifiedTradeSettlementMonetarySummation_ChargeTotalAmountCurrID := '';
  SpecifiedTradeSettlementMonetarySummation_AllowanceTotalAmount := 0;
  SpecifiedTradeSettlementMonetarySummation_AllowanceTotalAmountCurrID := '';
  SpecifiedTradeSettlementMonetarySummation_TaxBasisTotalAmount := 0;
  SpecifiedTradeSettlementMonetarySummation_TaxBasisTotalAmountCurrID := '';
  SpecifiedTradeSettlementMonetarySummation_TaxTotalAmount := 0;
  SpecifiedTradeSettlementMonetarySummation_TaxTotalAmountCurrID := '';
  SpecifiedTradeSettlementMonetarySummation_GrandTotalAmount := 0;
  SpecifiedTradeSettlementMonetarySummation_GrandTotalAmountCurrID := '';
end;

procedure TZUGFeRDApplicableSupplyChainTradeSettlement.Load(
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
  if TXMLHelper.FindChild(_Node,'ram:SpecifiedTradeSettlementMonetarySummation',node) then
  begin
    if TXMLHelper.FindChild(node,'ram:LineTotalAmount',n2) then
    begin
      SpecifiedTradeSettlementMonetarySummation_LineTotalAmount       := TZUGFeRDCrossIndustryDocumentHelper.StrToCurr(n2.Text);
      SpecifiedTradeSettlementMonetarySummation_LineTotalAmountCurrID := n2.GetAttribute('currencyID');
    end;
    if TXMLHelper.FindChild(node,'ram:ChargeTotalAmount',n2) then
    begin
      SpecifiedTradeSettlementMonetarySummation_ChargeTotalAmount       := TZUGFeRDCrossIndustryDocumentHelper.StrToCurr(n2.Text);
      SpecifiedTradeSettlementMonetarySummation_ChargeTotalAmountCurrID := n2.GetAttribute('currencyID');
    end;
    if TXMLHelper.FindChild(node,'ram:AllowanceTotalAmount',n2) then
    begin
      SpecifiedTradeSettlementMonetarySummation_AllowanceTotalAmount       := TZUGFeRDCrossIndustryDocumentHelper.StrToCurr(n2.Text);
      SpecifiedTradeSettlementMonetarySummation_AllowanceTotalAmountCurrID := n2.GetAttribute('currencyID');
    end;
    if TXMLHelper.FindChild(node,'ram:TaxBasisTotalAmount',n2) then
    begin
      SpecifiedTradeSettlementMonetarySummation_TaxBasisTotalAmount       := TZUGFeRDCrossIndustryDocumentHelper.StrToCurr(n2.Text);
      SpecifiedTradeSettlementMonetarySummation_TaxBasisTotalAmountCurrID := n2.GetAttribute('currencyID');
    end;
    if TXMLHelper.FindChild(node,'ram:TaxTotalAmount',n2) then
    begin
      SpecifiedTradeSettlementMonetarySummation_TaxTotalAmount       := TZUGFeRDCrossIndustryDocumentHelper.StrToCurr(n2.Text);
      SpecifiedTradeSettlementMonetarySummation_TaxTotalAmountCurrID := n2.GetAttribute('currencyID');
    end;
    if TXMLHelper.FindChild(node,'ram:GrandTotalAmount',n2) then
    begin
      SpecifiedTradeSettlementMonetarySummation_GrandTotalAmount       := TZUGFeRDCrossIndustryDocumentHelper.StrToCurr(n2.Text);
      SpecifiedTradeSettlementMonetarySummation_GrandTotalAmountCurrID := n2.GetAttribute('currencyID');
    end;
  end;
end;

procedure TZUGFeRDApplicableSupplyChainTradeSettlement.Save(
  _ParentNode: IXMLNode);
var
  i : Integer;
  node,n2 : IXMLNode;
begin
  if _ParentNode = nil then
    exit;
  _ParentNode := _ParentNode.AddChild('ram:ApplicableSupplyChainTradeSettlement');

  if not PaymentReference.IsEmpty then _ParentNode.AddChild('ram:PaymentReference').Text := PaymentReference;
  if not InvoiceCurrencyCode.IsEmpty then _ParentNode.AddChild('ram:InvoiceCurrencyCode').Text := InvoiceCurrencyCode;
  for i := 0 to ApplicableTradeTaxList.Count-1 do
    ApplicableTradeTaxList[i].Save(_ParentNode);

  node := _ParentNode.AddChild('ram:SpecifiedTradeSettlementMonetarySummation');

  n2 := node.AddChild('ram:LineTotalAmount');
  n2.Text := TZUGFeRDCrossIndustryDocumentHelper.CurrToStr(SpecifiedTradeSettlementMonetarySummation_LineTotalAmount);
  n2.Attributes['currencyID'] := SpecifiedTradeSettlementMonetarySummation_LineTotalAmountCurrID;

  n2 := node.AddChild('ram:ChargeTotalAmount');
  n2.Text := TZUGFeRDCrossIndustryDocumentHelper.CurrToStr(SpecifiedTradeSettlementMonetarySummation_ChargeTotalAmount);
  n2.Attributes['currencyID'] := SpecifiedTradeSettlementMonetarySummation_ChargeTotalAmountCurrID;

  n2 := node.AddChild('ram:AllowanceTotalAmount');
  n2.Text := TZUGFeRDCrossIndustryDocumentHelper.CurrToStr(SpecifiedTradeSettlementMonetarySummation_AllowanceTotalAmount);
  n2.Attributes['currencyID'] := SpecifiedTradeSettlementMonetarySummation_AllowanceTotalAmountCurrID;

  n2 := node.AddChild('ram:TaxBasisTotalAmount');
  n2.Text := TZUGFeRDCrossIndustryDocumentHelper.CurrToStr(SpecifiedTradeSettlementMonetarySummation_TaxBasisTotalAmount);
  n2.Attributes['currencyID'] := SpecifiedTradeSettlementMonetarySummation_TaxBasisTotalAmountCurrID;

  n2 := node.AddChild('ram:TaxTotalAmount');
  n2.Text := TZUGFeRDCrossIndustryDocumentHelper.CurrToStr(SpecifiedTradeSettlementMonetarySummation_TaxTotalAmount);
  n2.Attributes['currencyID'] := SpecifiedTradeSettlementMonetarySummation_TaxTotalAmountCurrID;

  n2 := node.AddChild('ram:GrandTotalAmount');
  n2.Text := TZUGFeRDCrossIndustryDocumentHelper.CurrToStr(SpecifiedTradeSettlementMonetarySummation_GrandTotalAmount);
  n2.Attributes['currencyID'] := SpecifiedTradeSettlementMonetarySummation_GrandTotalAmountCurrID;
end;

{ TZUGFeRDSpecifiedTradePaymentTerm }

constructor TZUGFeRDSpecifiedTradePaymentTerm.Create;
begin
  Clear;
end;

procedure TZUGFeRDSpecifiedTradePaymentTerm.Clear;
begin
  Description := '';
  DueDateDateTime := 0;
  ApplicableTradePaymentDiscountTerms_BasisAmount := 0;
  ApplicableTradePaymentDiscountTerms_BasisAmountCurrID := '';
  ApplicableTradePaymentDiscountTerms_CalculationPercent := 0;
  ApplicableTradePaymentDiscountTerms_ActualDiscountAmount := 0;
  ApplicableTradePaymentDiscountTerms_ActualDiscountAmountCurrID := '';
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
    DueDateDateTime := TZUGFeRDCrossIndustryDocumentHelper.DateFromStr(n2.Text);
  if TXMLHelper.FindChild(_Node,'ram:ApplicableTradePaymentDiscountTerms',node) then
  begin
    if DueDateDateTime = 0 then
    begin
      if TXMLHelper.FindChild(node,'ram:BasisDateTime',n3) then
      if TXMLHelper.FindChild(n3,'udt:DateTimeString',n2) then
        DueDateDateTime := TZUGFeRDCrossIndustryDocumentHelper.DateFromStr(n2.Text);
      if TXMLHelper.FindChild(node,'ram:BasisPeriodMeasure',n3) then
      if n3.GetAttribute('unitCode') = 'DAY' then
        DueDateDateTime := IncDay(DueDateDateTime,StrToIntDef(n3.Text,0));
    end;

    if TXMLHelper.FindChild(node,'ram:BasisAmount',n2) then
    begin
      ApplicableTradePaymentDiscountTerms_BasisAmount := TZUGFeRDCrossIndustryDocumentHelper.StrToCurr(n2.Text);
      ApplicableTradePaymentDiscountTerms_BasisAmountCurrID := n2.GetAttribute('currencyID');
    end;
    if TXMLHelper.FindChild(node,'ram:CalculationPercent',n2) then
      ApplicableTradePaymentDiscountTerms_CalculationPercent  := TZUGFeRDCrossIndustryDocumentHelper.StrToFloat(n2.Text);
    if TXMLHelper.FindChild(node,'ram:ActualDiscountAmount',n2) then
    begin
      ApplicableTradePaymentDiscountTerms_ActualDiscountAmount := TZUGFeRDCrossIndustryDocumentHelper.StrToCurr(n2.Text);
      ApplicableTradePaymentDiscountTerms_ActualDiscountAmountCurrID := n2.GetAttribute('currencyID');
    end;
  end;
end;

procedure TZUGFeRDSpecifiedTradePaymentTerm.Save(_ParentNode: IXMLNode);
begin

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

{ TZUGFeRDApplicableTradeTax }

constructor TZUGFeRDApplicableTradeTax.Create;
begin
  Clear;
end;

procedure TZUGFeRDApplicableTradeTax.Clear;
begin
  CalculatedAmount       := 0;
  CalculatedAmountCurrID := '';
  TypeCode               := '';
  BasisAmount            := 0;
  BasisAmountCurrID      := '';
  CategoryCode           := '';
  ApplicablePercent      := 0;
end;

procedure TZUGFeRDApplicableTradeTax.Load(_Node: IXMLNode);
var
  node : IXMLNode;
begin
  if TXMLHelper.FindChild(_Node,'ram:CalculatedAmount',node) then
  begin
    CalculatedAmount       := TZUGFeRDCrossIndustryDocumentHelper.StrToCurr(node.Text);
    CalculatedAmountCurrID := node.GetAttribute('currencyID');
  end;
  if TXMLHelper.FindChild(_Node,'ram:TypeCode',node) then
    TypeCode               := node.Text;
  if TXMLHelper.FindChild(_Node,'ram:BasisAmount',node) then
  begin
    BasisAmount            := TZUGFeRDCrossIndustryDocumentHelper.StrToCurr(node.Text);
    BasisAmountCurrID      := node.GetAttribute('currencyID');
  end;
  if TXMLHelper.FindChild(_Node,'ram:CategoryCode',node) then
    CategoryCode           := node.Text;
  if TXMLHelper.FindChild(_Node,'ram:ApplicablePercent',node) then
    ApplicablePercent      := TZUGFeRDCrossIndustryDocumentHelper.StrToFloat(node.Text);
end;

procedure TZUGFeRDApplicableTradeTax.Save(_ParentNode: IXMLNode);
var
  node : IXMLNode;
begin
  if _ParentNode = nil then
    exit;
  _ParentNode :=_ParentNode.AddChild('ram:ApplicableTradeTax');

  node := _ParentNode.AddChild('ram:CalculatedAmount');
  node.Text := TZUGFeRDCrossIndustryDocumentHelper.CurrToStr(CalculatedAmount);
  node.Attributes['currencyID'] := CalculatedAmountCurrID;

  node := _ParentNode.AddChild('ram:TypeCode');
  node.Text := TypeCode;

  node := _ParentNode.AddChild('ram:BasisAmount');
  node.Text := TZUGFeRDCrossIndustryDocumentHelper.CurrToStr(BasisAmount);
  node.Attributes['currencyID'] := BasisAmountCurrID;

  node := _ParentNode.AddChild('ram:ApplicablePercent');
  node.Text := TZUGFeRDCrossIndustryDocumentHelper.FloatToStr(ApplicablePercent);
end;

{ TZUGFeRDApplicableSupplyChainTradeAgreement }

procedure TZUGFeRDApplicableSupplyChainTradeAgreement.Clear;
begin
  BuyerReference := '';
  SellerTradeParty.Clear;
  BuyerTradeParty.Clear;
  ShipFromTradeParty.Clear;
end;

constructor TZUGFeRDApplicableSupplyChainTradeAgreement.Create;
begin
  SellerTradeParty:= TZUGFeRDTradePartyType.Create;
  BuyerTradeParty:= TZUGFeRDTradePartyType.Create;
  ShipFromTradeParty:= TZUGFeRDTradePartyType.Create;
  Clear;
end;

destructor TZUGFeRDApplicableSupplyChainTradeAgreement.Destroy;
begin
  if Assigned(SellerTradeParty) then begin SellerTradeParty.Free; SellerTradeParty := nil; end;
  if Assigned(BuyerTradeParty) then begin BuyerTradeParty.Free; BuyerTradeParty := nil; end;
  if Assigned(ShipFromTradeParty) then begin ShipFromTradeParty.Free; ShipFromTradeParty := nil; end;
  inherited;
end;

procedure TZUGFeRDApplicableSupplyChainTradeAgreement.Load(
  _Node: IXMLNode);
var
  node : IXMLNode;
begin
  if _Node = nil then
    exit;
  if TXMLHelper.FindChild(_Node,'ram:BuyerReference',node) then
    BuyerReference := node.Text;
  if TXMLHelper.FindChild(_Node,'ram:SellerTradeParty',node) then
    SellerTradeParty.Load(node);
  if TXMLHelper.FindChild(_Node,'ram:BuyerTradeParty',node) then
    BuyerTradeParty.Load(node);
  if TXMLHelper.FindChild(_Node,'ram:ShipFromTradeParty',node) then
    ShipFromTradeParty.Load(node);
end;

procedure TZUGFeRDApplicableSupplyChainTradeAgreement.Save(
  _ParentNode: IXMLNode);
begin
  if _ParentNode = nil then
    exit;
  _ParentNode := _ParentNode.AddChild('ram:ApplicableSupplyChainTradeAgreement');
  if not BuyerReference.IsEmpty then
    _ParentNode.AddChild('ram:BuyerReference').Text := BuyerReference;
  SellerTradeParty.Save(_ParentNode.AddChild('ram:SellerTradeParty'));
  BuyerTradeParty.Save(_ParentNode.AddChild('ram:BuyerTradeParty'));
  if not ShipFromTradeParty.Name.IsEmpty then
    ShipFromTradeParty.Save(_ParentNode.AddChild('ram:ShipFromTradeParty'));
end;

{ TZUGFeRDApplicableSupplyChainTradeDelivery }

constructor TZUGFeRDApplicableSupplyChainTradeDelivery.Create;
begin
  ShipToTradeParty:= TZUGFeRDTradePartyType.Create;
  UltimateShipToTradeParty:= TZUGFeRDTradePartyType.Create;
  ShipFromTradeParty:= TZUGFeRDTradePartyType.Create;
  Clear;
end;

destructor TZUGFeRDApplicableSupplyChainTradeDelivery.Destroy;
begin
  if Assigned(ShipToTradeParty) then begin ShipToTradeParty.Free; ShipToTradeParty := nil; end;
  if Assigned(UltimateShipToTradeParty) then begin UltimateShipToTradeParty.Free; UltimateShipToTradeParty := nil; end;
  if Assigned(ShipFromTradeParty) then begin ShipFromTradeParty.Free; ShipFromTradeParty := nil; end;
  inherited;
end;

function TZUGFeRDApplicableSupplyChainTradeDelivery.IsValid(
  _DocType: TZUGFeRDInvoiceTypeCode;
  _OnInconsistency: TZUGFeRDValidationEvent): Boolean;
begin
  Result := true;
  if ActualDeliverySupplyChainEvent_OccurrenceDateTime = 0 then
  begin
    _OnInconsistency(-1,'Element ActualDeliverySupplyChainEvent OccurrenceDateTime must occur exactly 1 times.');
    Result := false;
  end;
end;

procedure TZUGFeRDApplicableSupplyChainTradeDelivery.Clear;
begin
  ShipToTradeParty.Clear;
  UltimateShipToTradeParty.Clear;
  ShipFromTradeParty.Clear;
  ActualDeliverySupplyChainEvent_OccurrenceDateTime := 0;
end;

procedure TZUGFeRDApplicableSupplyChainTradeDelivery.Load(
  _Node: IXMLNode);
var
  node : IXMLNode;
begin
  if _Node = nil then
    exit;
  if TXMLHelper.FindChild(_Node,'ram:ShipToTradeParty',node) then
    ShipToTradeParty.Load(node);
  if TXMLHelper.FindChild(_Node,'ram:UltimateShipToTradeParty',node) then
    UltimateShipToTradeParty.Load(node);
  if TXMLHelper.FindChild(_Node,'ram:ShipFromTradeParty',node) then
    ShipFromTradeParty.Load(node);
  if TXMLHelper.FindChild(_Node,'ram:ActualDeliverySupplyChainEvent',node) then
  if TXMLHelper.FindChild(node,'ram:OccurrenceDateTime',node) then
  if TXMLHelper.FindChild(node,'udt:DateTimeString',node) then
    ActualDeliverySupplyChainEvent_OccurrenceDateTime := TZUGFeRDCrossIndustryDocumentHelper.DateFromStr(node.Text);
end;

procedure TZUGFeRDApplicableSupplyChainTradeDelivery.Save(
  _ParentNode: IXMLNode);
var
  node : IXMLNode;
begin
  if _ParentNode = nil then
    exit;
  _ParentNode := _ParentNode.AddChild('ram:ApplicableSupplyChainTradeDelivery');

  node := _ParentNode.AddChild('ram:ActualDeliverySupplyChainEvent');
  node := node.AddChild('ram:OccurrenceDateTime');
  node := node.AddChild('udt:DateTimeString');
  node.Attributes['format'] := '102';
  node.Text := TZUGFeRDCrossIndustryDocumentHelper.DateToStr(ActualDeliverySupplyChainEvent_OccurrenceDateTime);

//TODO  ShipToTradeParty.Load(node);
//  UltimateShipToTradeParty.Load(node);
//  ShipFromTradeParty.Load(node);
end;

{ TZUGFeRDPostalTradeAddress }

procedure TZUGFeRDPostalTradeAddress.Clear;
begin
  PostcodeCode := '';
  LineOne := '';
  LineTwo := '';
  CityName := '';
  CountryID := '';
end;

constructor TZUGFeRDPostalTradeAddress.Create;
begin
  Clear;
end;

destructor TZUGFeRDPostalTradeAddress.Destroy;
begin
  inherited;
end;

procedure TZUGFeRDPostalTradeAddress.Load(_Node: IXMLNode);
var
  node : IXMLNode;
begin
  if _Node = nil then
    exit;
  if TXMLHelper.FindChild(_Node,'ram:PostcodeCode',node) then
    PostcodeCode := node.Text;
  if TXMLHelper.FindChild(_Node,'ram:LineOne',node) then
    LineOne := node.Text;
  if TXMLHelper.FindChild(_Node,'ram:LineTwo',node) then
    LineTwo := node.Text;
  if TXMLHelper.FindChild(_Node,'ram:CityName',node) then
    CityName := node.Text;
  if TXMLHelper.FindChild(_Node,'ram:CountryID',node) then
    CountryID := node.Text;
end;

procedure TZUGFeRDPostalTradeAddress.Save(_ParentNode: IXMLNode);
begin
  if _ParentNode = nil then
    exit;
  _ParentNode := _ParentNode.AddChild('ram:PostalTradeAddress');
  _ParentNode.AddChild('ram:PostcodeCode').Text := PostcodeCode;
  _ParentNode.AddChild('ram:LineOne').Text := LineOne;
  if not LineTwo.IsEmpty then _ParentNode.AddChild('ram:LineTwo').Text := LineTwo;
  _ParentNode.AddChild('ram:CityName').Text := CityName;
  _ParentNode.AddChild('ram:CountryID').Text := CountryID;
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

procedure TZUGFeRDTradePartyType.Load(_Node: IXMLNode);
var
  node : IXMLNode;
  itmid : TZUGFeRDID;
  i : Integer;
begin
  if _Node = nil then
    exit;
  if TXMLHelper.FindChild(_Node,'ram:Name',node) then
    Name := node.Text;
  if TXMLHelper.FindChild(_Node,'ram:PostalTradeAddress',node) then
    PostalTradeAddress.Load(node);

  for i := 0 to _Node.ChildNodes.Count-1 do
  if SameText(_Node.ChildNodes[i].NodeName,'ram:SpecifiedTaxRegistration') then
  begin
    itmid := TZUGFeRDID.Create;
    itmid.Load(_Node.ChildNodes[i]);
    SpecifiedTaxRegistration.Add(itmid);
  end;
end;

procedure TZUGFeRDTradePartyType.Save(_TradePartyNode: IXMLNode);
var
  i : Integer;
begin
  if _TradePartyNode = nil then
    exit;
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
    Result := TrimLeft(Result + #13#10)+Items[i].Content.Text;
end;

{ TZUGFeRDIncludedNote }

procedure TZUGFeRDIncludedNote.Clear;
begin
  ContentCode := '';
  Content.Clear;
  SubjectCode := '';
end;

constructor TZUGFeRDIncludedNote.Create;
begin
  Content := TStringList.Create;
end;

destructor TZUGFeRDIncludedNote.Destroy;
begin
  if Assigned(Content) then begin Content.Free; Content := nil; end;
  inherited;
end;

end.
