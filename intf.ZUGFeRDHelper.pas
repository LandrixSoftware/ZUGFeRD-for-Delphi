unit intf.ZUGFeRDHelper;

interface

uses
  System.SysUtils,System.Classes,System.Types,System.DateUtils,System.Rtti
  ,System.Variants
  ,Xml.xmldom,Xml.XMLDoc,Xml.XMLIntf,Xml.XMLSchema,MSXML2_TLB
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
    TValidationError = record
    public
      Reason : String;
      SrcText : String;
    end;
  private
    class function LoadXSDFromResource(const _ResourceName : String) : String;
  public
    class function ValidateZUGFeRD20Basis(const _XML : String; out _Error : TValidationError) : Boolean;
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

class function TZUGFeRDValidationHelper.ValidateZUGFeRD20Basis(
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

end.


