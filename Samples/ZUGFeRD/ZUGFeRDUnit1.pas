unit ZUGFeRDUnit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs
  ,WPViewPDF3 ,WPDF_ViewCommands,WPPDFR1,WPPDFR2, Vcl.StdCtrls, Vcl.ExtCtrls
  ,ZUGFeRD1p0
  ;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
    Button1: TButton;
    ListBox1: TListBox;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  public
    WPViewPDF : TWPViewPDF;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  i,j : Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    Listbox1.Clear;
    Memo1.Clear;
    WPViewPDF.Clear;
    WPViewPDF.LoadFromFileAsCopy(ExtractFilePath(Application.ExeName)+'..\..\..\Samples\Rechnung_9202174904.pdf' );
    WPViewPDF.command(COMPDF_GotoFirst);
    WPViewPDF.command(COMPDF_ZoomFullpage);

    j := WPViewPDF.Command(COMPDF_Attachment_List);
    for i := 0 to j-1 do
      ListBox1.Items.Add( WPViewPDF.CommandGetStr( COMPDF_Attachment_GetProp, '', i ) );
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  mem : TMemoryStream;
  l : Integer;
  fn : String;
  xml : IXMLCrossIndustryDocumentType;
begin
  if ListBox1.ItemIndex < 0 then
    exit;
  mem := TMemoryStream.Create;
  try
    l := WPViewPDF.Command( COMPDF_Attachment_GetData, ListBox1.ItemIndex );
    if l>=0 then
    begin
      mem.SetSize(l);
      WPViewPDF.CommandEx( COMPDF_MakeGetMEMORY, {$IFDEF WIN64} IntPtr {$ELSE} Cardinal {$ENDIF}( mem.Memory ) );
      fn := WPViewPDF.CommandGetStr(COMPDF_Attachment_GetProp, 'F',0 );
      mem.Seek(0,soFromBeginning);
      mem.SaveToFile(ExtractFilePath(Application.ExeName)+'..\..\..\Samples\Rechnung_9202174904_'+fn);

      xml := LoadCrossIndustryDocument(ExtractFilePath(Application.ExeName)+'..\..\..\Samples\Rechnung_9202174904_'+fn);

      Memo1.Lines.Text := xml.XML;
    end;
  finally
    mem.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  WPViewPDF := TWPViewPDF.Create(Panel1);
//  WPViewPDF.DLLName := VIEWDLLNAME_FULL;
  WPViewPDF.ViewerStart('', WPViewPDF_LicName, WPViewPDF_LicKey, WPViewPDF_LicCode);
  WPViewPDF.Parent := Panel1;
  WPViewPDF.Align := alClient;
  WPViewPDF.ViewControls := [wpViewLeftPanel,wpHorzScrollBar, wpVertScrollBar,
                             wpNavigationPanel, wpPropertyPanel, wpViewPanel];
  WPViewPDF.ViewOptions := WPViewPDF.ViewOptions +
                           [wpExpandAllBookmarks, wpDontUseHyperlinks,
                           wpShowPageSelection,wpViewThumbnails];
  WPViewPDF.ViewOptions := WPViewPDF.ViewOptions -
                           [wpDisablePagenrHint];
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(WPViewPDF) then begin WPViewPDF.Parent := nil;  WPViewPDF.Free; WPViewPDF := nil; end;
end;

end.
