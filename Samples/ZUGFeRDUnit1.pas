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

unit ZUGFeRDUnit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs
  ,intf.ZUGFeRDHelper, Vcl.StdCtrls, Vcl.OleCtrls, SHDocVw
  ;

type
  TForm1 = class(TForm)
    Button1: TButton;
    WebBrowser2: TWebBrowser;
    Memo3: TMemo;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    WebBrowserContent : TStringList;
    WebBrowserContentFilename : String;
    WebBrowserContentFilenamePdf : String;
    DistributionBasePath : String;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  DistributionBasePath := ExtractFileDir(Application.ExeName);
  DistributionBasePath := ExtractFileDir(DistributionBasePath);
  DistributionBasePath := ExtractFileDir(DistributionBasePath);
  DistributionBasePath := ExtractFileDir(DistributionBasePath)+PathDelim+'Distribution'+PathDelim;

  Left := 50;
  Top := 50;
  Width := Screen.WorkAreaWidth-100;
  Height := Screen.WorkAreaHeight-100;

  WebBrowserContent := TStringList.Create;
  WebBrowserContentFilename := ExtractFilePath(Application.ExeName)+'content.html';
  WebBrowserContentFilenamePdf := ExtractFilePath(Application.ExeName)+'content.pdf';
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  od : TOpenDialog;
  cmdoutput : String;
  htmlresult : String;
begin
  WebBrowser2.Navigate2('about:blank');

  od := TOpenDialog.Create(nil);
  try
    if not od.Execute then
      exit;

    GetZUGFeRDPdfHelper.SetJavaRuntimeEnvironmentPath(DistributionBasePath+'java\')
        .SetMustangprojectLibPath(DistributionBasePath+'mustangproject\')
        .VisualizeFile(od.FileName, cmdoutput,htmlresult);

    Memo3.Lines.Text := cmdoutput;

    if htmlresult <> '' then
      WebBrowserContent.Text := htmlresult
    else
      WebBrowserContent.Text := '<html><body>Visualisierung nicht erfolgreich. Siehe Verzeichnis ./Distribution/Read.Me</body></html>';
    WebBrowserContent.SaveToFile(WebBrowserContentFilename,TEncoding.UTF8);
    WebBrowser2.Navigate2('file:///'+WebBrowserContentFilename);

  finally
    od.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  od : TOpenDialog;
  cmdoutput : String;
  pdfresult : TMemoryStream;
begin
  WebBrowser2.Navigate2('about:blank');

  ShowMessage('Geht nicht nicht: https://github.com/ZUGFeRD/mustangproject/issues/387');

  od := TOpenDialog.Create(nil);
  try
    if not od.Execute then
      exit;

    GetZUGFeRDPdfHelper.SetJavaRuntimeEnvironmentPath(DistributionBasePath+'java\')
        .SetMustangprojectLibPath(DistributionBasePath+'mustangproject\')
        .VisualizeFileAsPdf(od.FileName, cmdoutput,pdfresult);

    Memo3.Lines.Text := cmdoutput;

    if pdfresult <> nil then
    begin
      pdfresult.SaveToFile(WebBrowserContentFilenamePdf);
      pdfresult.Free;
      WebBrowser2.Navigate2('file:///'+WebBrowserContentFilenamePdf);
    end else
    begin
      WebBrowserContent.Text := '<html><body>Visualisierung nicht erfolgreich. Siehe Verzeichnis ./Distribution/Read.Me</body></html>';
      WebBrowserContent.SaveToFile(WebBrowserContentFilename,TEncoding.UTF8);
      WebBrowser2.Navigate2('file:///'+WebBrowserContentFilename);
    end;

  finally
    od.Free;
  end;
end;

end.
