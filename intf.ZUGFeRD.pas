unit intf.ZUGFeRD;

interface

{NOT$DEFINE USE_OXMLDomVendor} //http://www.kluug.net/oxml.php

uses
  System.SysUtils,System.Classes,System.Variants,
  Xml.xmldom,Xml.XMLDoc,Xml.XMLIntf
  {$IFDEF USE_OXMLDomVendor},OXmlDOMVendor{$ENDIF}
  ,Dialogs
  ;

  //https://github.com/konik-io/konik/blob/master/src/main/java/io/konik/zugferd/Invoice.java
type
  TZUGFeRDValidationEvent = reference to procedure (_EventLevel : Integer; const _Msg : String);

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
    function  IsValid(_OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
  end;

  TZUGFeRDExchangedDocumentContext = class(TObject)
  public const
    P1 = 'urn:ferd:CrossIndustryDocument:invoice:1p0:basic';
    P2 = 'urn:ferd:CrossIndustryDocument:invoice:1p0:extended';
  public
    TestIndicator : Boolean;
    GuidelineSpecifiedDocumentContextParameter : TZUGFeRDID;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Load(_ParentNode : IXMLNode);
    procedure Save(_ParentNode : IXMLNode);
    function  IsValid(_OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
  end;

  TZUGFeRDInvoiceTypeCode = (zugferdTypeCode_None,
                             zugferdTypeCode_Basic,
                             zugferdTypeCode_Comfort,
                             zugferdTypeCode_Extended);

  TZUGFeRDExchangedDocument = class(TObject)
  public
		ID : TZUGFeRDID;
		Name : String;
		TypeCode : TZUGFeRDInvoiceTypeCode;
		IssueDateTime : TDate;
		IncludedNote : String;
		SubjectCode : String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Load(_ParentNode : IXMLNode);
    procedure Save(_ParentNode : IXMLNode);
    function  IsValid(_OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
  end;

  TZUGFeRDCrossIndustryDocument = class(TObject)
  public
    const TargetNamespace = 'urn:ferd:CrossIndustryDocument:invoice:1p0';
    const TargetNamespaceRam = 'urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:12';
    const TargetNamespaceUdt = 'urn:un:unece:uncefact:data:standard:UnqualifiedDataType:15';
  private
    procedure InternalLoadDocument(_Xml : IXMLDocument);
    procedure InternalSaveDocument(_Xml : IXMLDocument);
  public
    SpecifiedExchangedDocumentContext : TZUGFeRDExchangedDocumentContext;
    HeaderExchangedDocument : TZUGFeRDExchangedDocument;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(const _Filename : String);
    procedure LoadFromStream(_Stream : TStream);
    procedure LoadFromXMLStr(const _XML : String);
    procedure SaveToFile(const _Filename : String);
    procedure SaveToStream(_Stream : TStream);
    procedure SaveToXMLStr(out _XML : String);
    function IsValid(_OnInconsistency : TZUGFeRDValidationEvent) : Boolean;
  end;

implementation

{ TZUGFeRDCrossIndustryDocument }

constructor TZUGFeRDCrossIndustryDocument.Create;
begin
  SpecifiedExchangedDocumentContext := TZUGFeRDExchangedDocumentContext.Create;
  HeaderExchangedDocument := TZUGFeRDExchangedDocument.Create;
  Clear;
end;

destructor TZUGFeRDCrossIndustryDocument.Destroy;
begin
  if Assigned(SpecifiedExchangedDocumentContext) then begin  SpecifiedExchangedDocumentContext.Free; SpecifiedExchangedDocumentContext := nil; end;
  if Assigned(HeaderExchangedDocument) then begin HeaderExchangedDocument.Free; HeaderExchangedDocument := nil; end;
  inherited;
end;

procedure TZUGFeRDCrossIndustryDocument.Clear;
begin
  SpecifiedExchangedDocumentContext.Clear;
  HeaderExchangedDocument.Clear;
end;

procedure TZUGFeRDCrossIndustryDocument.LoadFromFile(const _Filename : String);
var
  xml : IXMLDocument;
begin
  if not FileExists(_Filename) then
    exit;
  xml := LoadXMLDocument(_FileName);
  InternalLoadDocument(xml);
end;

procedure TZUGFeRDCrossIndustryDocument.LoadFromStream(_Stream : TStream);
var
  xml : IXMLDocument;
begin
  if _Stream = nil then
    exit;

  xml := TXMLDocument.Create(nil);
  xml.LoadFromStream(_Stream);
  InternalLoadDocument(xml);
end;

procedure TZUGFeRDCrossIndustryDocument.LoadFromXMLStr(const _XML: String);
begin

end;

procedure TZUGFeRDCrossIndustryDocument.SaveToFile(const _Filename: String);
var
  xml : IXMLDocument;
begin
  xml := NewXMLDocument;
  InternalSaveDocument(xml);
  xml.SaveToFile(_Filename);
end;

procedure TZUGFeRDCrossIndustryDocument.SaveToStream(_Stream: TStream);
var
  xml : IXMLDocument;
begin
  xml := NewXMLDocument;
  InternalSaveDocument(xml);
  xml.SaveToStream(_Stream);
end;

procedure TZUGFeRDCrossIndustryDocument.SaveToXMLStr(out _XML: String);
var
  xml : IXMLDocument;
begin
  xml := NewXMLDocument;
  InternalSaveDocument(xml);
  _XML := xml.DocumentElement.XML;
end;

procedure TZUGFeRDCrossIndustryDocument.InternalLoadDocument(
  _Xml: IXMLDocument);
var
  node : IXMLNode;
begin
  {$IFDEF USE_OXMLDomVendor}TXMLDocument(_Xml).DOMVendor := Xml.xmldom.GetDOMVendor(sOXmlDOMVendor);{$ENDIF}

  if not SameText(_Xml.DocumentElement.LocalName,'CrossIndustryDocument') then
    exit;
  SpecifiedExchangedDocumentContext.Load(_Xml.DocumentElement);
  HeaderExchangedDocument.Load(_Xml.DocumentElement);

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

procedure TZUGFeRDCrossIndustryDocument.InternalSaveDocument(
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

function TZUGFeRDCrossIndustryDocument.IsValid(
  _OnInconsistency: TZUGFeRDValidationEvent): Boolean;
begin

end;

{ TZUGFeRDExchangedDocument }

constructor TZUGFeRDExchangedDocument.Create;
begin
  ID := TZUGFeRDID.Create;
  Clear;
end;

destructor TZUGFeRDExchangedDocument.Destroy;
begin
  if Assigned(ID) then begin ID.Free; ID := nil; end;
  inherited;
end;

procedure TZUGFeRDExchangedDocument.Clear;
begin
  ID.Clear;
  Name := '';
  TypeCode := zugferdTypeCode_None;
  IssueDateTime := 0;
  IncludedNote := '';
  SubjectCode := '';
end;

function TZUGFeRDExchangedDocument.IsValid(
  _OnInconsistency: TZUGFeRDValidationEvent): Boolean;
begin
  Result := ID.IsValid(_OnInconsistency);
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
end;

procedure TZUGFeRDExchangedDocument.Load(_ParentNode: IXMLNode);
var
  node : IXMLNode;
begin
  if _ParentNode = nil then
    exit;
  node := _ParentNode.ChildNodes.FindNode('HeaderExchangedDocument');
  if node = nil then
    exit;
  ID.Load(node);
  Name := node.ChildNodes['ram:Name'].Text;
end;

procedure TZUGFeRDExchangedDocument.Save(_ParentNode: IXMLNode);
begin
  if _ParentNode = nil then
    exit;

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

function TZUGFeRDExchangedDocumentContext.IsValid(
  _OnInconsistency: TZUGFeRDValidationEvent): Boolean;
begin
  Result := GuidelineSpecifiedDocumentContextParameter.IsValid(_OnInconsistency);
end;

procedure TZUGFeRDExchangedDocumentContext.Load(_ParentNode: IXMLNode);
var
  node,node2 : IXMLNode;
begin
  if _ParentNode = nil then
    exit;

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

function TZUGFeRDID.IsValid(_OnInconsistency: TZUGFeRDValidationEvent): Boolean;
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
  node := _ParentNode.ChildNodes.FindNode('ID');
  if node <> nil then
  begin
    ID := node.Text;
    SchemeID := VarToStrDef(node.Attributes['SchemeID'],'');
    SchemeAgencyID := VarToStrDef(node.Attributes['SchemeAgencyID'],'');
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
    node.Attributes['SchemeID'] := SchemeID;
  if not SchemeAgencyID.IsEmpty then
    node.Attributes['SchemeAgencyID'] := SchemeAgencyID;
end;

end.
