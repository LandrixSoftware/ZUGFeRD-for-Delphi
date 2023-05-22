program ZUGFeRDProject;

uses
  Vcl.Forms,
  ZUGFeRDUnit1 in 'ZUGFeRDUnit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
