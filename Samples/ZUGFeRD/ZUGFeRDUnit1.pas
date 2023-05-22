unit ZUGFeRDUnit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls
//  ,ZUGFeRD1p0
//  ,intf.ZUGFeRD,OXmlDOMVendor
  ,Xml.xmldom,Xml.XMLDoc,Xml.XMLIntf
  ,intf.ZUGFeRDGlobalIDSchemeIdentifiers
  ,intf.ZUGFeRDGlobalID
  ,intf.ZUGFeRDVersion
  //,intf.ZUGFeRDTradeLineItem
  ,intf.ZUGFeRDTaxCategoryCodes
  ;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
    Button1: TButton;
    ListBox1: TListBox;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure generate;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin


//  generate;
//  exit;

//  Button1.Click;
//  ListBox1.ItemIndex := 0;
//  Button2.Click;
end;

procedure TForm1.generate;
var
  xml : IXMLDocument;
  xRoot : IXMLNode;
begin
  xml := NewXMLDocument;


end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i,j : Integer;
begin
//  Screen.Cursor := crHourGlass;
//  try
//    Listbox1.Clear;
//    Memo1.Clear;
//    WPViewPDF.Clear;                                                                    //Rechnung_9202174904
//    WPViewPDF.LoadFromFileAsCopy(ExtractFilePath(Application.ExeName)+'..\..\..\Samples\Beispielrechnung_Handel_Korrektur_basic.pdf' );
//    WPViewPDF.command(COMPDF_GotoFirst);
//    WPViewPDF.command(COMPDF_ZoomFullpage);
//
//    j := WPViewPDF.Command(COMPDF_Attachment_List);
//    for i := 0 to j-1 do
//      ListBox1.Items.Add( WPViewPDF.CommandGetStr( COMPDF_Attachment_GetProp, '', i ) );
//  finally
//    Screen.Cursor := crDefault;
//  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
//var
//  mem : TMemoryStream;
//  l : Integer;
//  fn,hstr : String;
//  inv : TZUGFeRDCrossIndustryDocument;
begin
//  if ListBox1.ItemIndex < 0 then
//    exit;
//  mem := TMemoryStream.Create;
//  try
//    l := WPViewPDF.Command( COMPDF_Attachment_GetData, ListBox1.ItemIndex );
//    if l>=0 then
//    begin
//      mem.SetSize(l);
//      WPViewPDF.CommandEx( COMPDF_MakeGetMEMORY, {$IFDEF WIN64} IntPtr {$ELSE} Cardinal {$ENDIF}( mem.Memory ) );
//      fn := WPViewPDF.CommandGetStr(COMPDF_Attachment_GetProp, 'F',0 );
//      mem.Seek(0,soFromBeginning);
////      mem.SaveToFile(ExtractFilePath(Application.ExeName)+'..\..\..\Samples\Beispielrechnung_Handel_Korrektur_basic_'+fn);
//
////      xml := TCrossIndustryDocumentTypeHelper.LoadCrossIndustryDocument(ExtractFilePath(Application.ExeName)+'..\..\..\Samples\Beispielrechnung_Handel_Korrektur_basic_'+fn);
//
////      Memo1.Lines.Text := xml.XML;
//
////      for l := 0 to xml.ChildNodes['HeaderExchangedDocument'].ChildNodes.Count-1 do
////      if xml.ChildNodes['HeaderExchangedDocument'].ChildNodes[l].IsTextElement then
////      begin
////        ShowMessage(xml.ChildNodes['HeaderExchangedDocument'].ChildNodes[l].Text);
////        ShowMessage(xml.ChildNodes['HeaderExchangedDocument'].ChildNodes[l].NodeName);
////      end;
//
////#        l := xml.ChildNodes['HeaderExchangedDocument'].ChildNodes.IndexOf('ID','urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:12');
////        if xml.ChildNodes['HeaderExchangedDocument'].ChildNodes.FindNode('ID','urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:12') <> nil then
////        ShowMessage(xml.HeaderExchangedDocument.ID.NamespaceURI);
////        ShowMessage(xml.HeaderExchangedDocument.ID.Text);
//
////        xml.OwnerDocument.SaveToFile(ExtractFilePath(Application.ExeName)+'..\..\..\Samples\Beispielrechnung_Handel_Korrektur_basic_2_'+fn);
//      mem.Seek(0,soFromBeginning);
//      inv := TZUGFeRDCrossIndustryDocument.Create;
//      inv.LoadFromStream(mem);
//      inv.SaveToXMLStr(hstr);
//      inv.Free;
//
//      Memo1.Lines.Text := hstr;
//    end;
//  finally
//    mem.Free;
//  end;
end;

end.
