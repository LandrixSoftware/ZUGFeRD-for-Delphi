unit intf.ZUGFeRDHelper;

interface

uses
  System.SysUtils,System.Classes,System.Types,System.DateUtils,System.Rtti
  ,System.Variants
  ,Xml.xmldom,Xml.XMLDoc,Xml.XMLIntf,Xml.XMLSchema,intf.ZUGFeRDMSXML2_TLB
  ;

type
  TZUGFeRDXMLHelper = class(TObject)
  public type
    TXMLLoadCallback = procedure(Node : IXMLNode) of object;
  public
    class procedure LoadFromChilds(const _NodeName : String; _Node : IXMLNode; _Callback : TXMLLoadCallback);
    class function FindChild(_Node : IXMLNode; const _NodeName : String; out _Result : IXMLNode) : Boolean;
  end;

  TZUGFeRDValidationHelper = class(TObject)
  public type
    TZUGFeRDVersion = (zugferdVersion_Unkown,zugferdVersion_10,zugferdVersion_21);
    TValidationError = record
    public
      Reason : String;
      SrcText : String;
    end;
  private
    class function LoadXSDFromResource(const _ResourceName : String) : String;
  public
    class function GetZUGFeRDVersion(_Xml : IXMLDocument) : TZUGFeRDVersion;
    class function ValidateZUGFeRD21Basis(const _XML : String; out _Error : TValidationError) : Boolean;
  end;

implementation

{$R intf.ZUGFeRDSchema.res}

{ TZUGFeRDXMLHelper }

class function TZUGFeRDXMLHelper.FindChild(_Node: IXMLNode; const _NodeName: String;
  out _Result: IXMLNode): Boolean;
begin
  Result := false;
  if _Node = nil then
    exit;
  _Result := _Node.ChildNodes.FindNode(_NodeName,'');
  Result := _Result <> nil;
end;

class procedure TZUGFeRDXMLHelper.LoadFromChilds(const _NodeName: String; _Node: IXMLNode;
  _Callback: TXMLLoadCallback);
var
  Node : IXMLNode;
begin
  Node := _Node.ChildNodes.FindNode(_NodeName,'');
  if Node = nil then
    exit;
  _Callback(Node);
end;

{ TZUGFeRDValidationHelper }

class function TZUGFeRDValidationHelper.GetZUGFeRDVersion(
  _Xml: IXMLDocument): TZUGFeRDVersion;
begin
  Result := zugferdVersion_Unkown;
  if _XML = nil then
    exit;
  if SameText(_XML.DocumentElement.NodeName,'rsm:CrossIndustryInvoice') then
    Result := zugferdVersion_21
  else
  if SameText(_XML.DocumentElement.NodeName,'rsm:CrossIndustryDocument') then
    Result := zugferdVersion_10;
end;

class function TZUGFeRDValidationHelper.LoadXSDFromResource(
  const _ResourceName: String): String;
var
  res : TResourceStream;
  str : TStringStream;
begin
  res := TResourceStream.Create(hInstance, _ResourceName, RT_RCDATA);
  str := TStringStream.Create;
  try
    str.LoadFromStream(res);
    result := str.DataString;
  finally
    str.Free;
    res.Free;
  end;
end;

class function TZUGFeRDValidationHelper.ValidateZUGFeRD21Basis(
  const _XML: String; out _Error: TValidationError): Boolean;
var
  xml, xsd: IXMLDOMDocument2;
  cache: IXMLDOMSchemaCollection;
  error: IXMLDOMParseError;
begin
  Result := false;
  cache := CoXMLSchemaCache60.Create;
  try
    xsd := CoDOMDocument60.Create;
    xsd.Async := False;
    xsd.loadXML(TZUGFeRDValidationHelper.LoadXSDFromResource('ZUGFeRD21BASICUnqualifiedDataType'));
    cache.add('urn:un:unece:uncefact:data:standard:UnqualifiedDataType:100', xsd);

    xsd := CoDOMDocument60.Create;
    xsd.Async := False;
    xsd.loadXML(TZUGFeRDValidationHelper.LoadXSDFromResource('ZUGFeRD21BASICQualifiedDataType'));
    cache.add('urn:un:unece:uncefact:data:standard:QualifiedDataType:100', xsd);

    xsd := CoDOMDocument60.Create;
    xsd.Async := False;
    xsd.loadXML(TZUGFeRDValidationHelper.LoadXSDFromResource('ZUGFeRD21BASICReusableAggregateBusinessInformationEntity'));
    cache.add('urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:100', xsd);

    xsd := CoDOMDocument60.Create;
    xsd.Async := False;
    xsd.loadXML(TZUGFeRDValidationHelper.LoadXSDFromResource('ZUGFeRD21BASIC'));
    cache.add('urn:un:unece:uncefact:data:standard:CrossIndustryInvoice:100', xsd);

    xml := CoDOMDocument60.Create;
    xml.async := False;
    xml.schemas := cache;
    xml.validateOnParse := true;

    Result := xml.loadXML(_XML);
    if not Result then
    begin
      _Error.Reason := xml.parseError.reason;
      _Error.SrcText := xml.parseError.srcText;
    end;
  except
    on E:Exception do begin _Error.Reason := E.Message; _Error.SrcText := E.ClassName; end;
  end;
end;

//function ExtractPDFMetadata(const aPDFFileName: TFileName): UTF8String;
//var tmp: RawByteString;
//    i: integer;
//begin
//  with TFileStream.Create(aPDFFileName,fmOpenRead) do
//  try
//    SetLength(tmp,Size);
//    Read(tmp[1],Size);
//  finally
//    Free;
//  end;
//  result := '';
//  i := pos('<x:xmpmeta',tmp);
//  if i=0 then exit;
//  delete(tmp,1,i-1);
//  i := pos('</x:xmpmeta>',tmp);
//  if i=0 then exit;
//  result := copy(tmp,1,i+12);
//end;

end.


