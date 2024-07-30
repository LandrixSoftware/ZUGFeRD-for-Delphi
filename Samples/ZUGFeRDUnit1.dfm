object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'ZUGFeRD'
  ClientHeight = 714
  ClientWidth = 1092
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 129
    Height = 25
    Caption = 'XML-Datei als HTML'
    TabOrder = 0
    OnClick = Button1Click
  end
  object WebBrowser2: TWebBrowser
    Left = 317
    Top = 0
    Width = 775
    Height = 545
    Align = alRight
    TabOrder = 1
    ControlData = {
      4C00000019500000543800000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object Memo3: TMemo
    Left = 0
    Top = 545
    Width = 1092
    Height = 169
    Align = alBottom
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object Button2: TButton
    Left = 8
    Top = 39
    Width = 129
    Height = 25
    Caption = 'XML-Datei als PDF'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 70
    Width = 209
    Height = 25
    Caption = 'XML-Datei aus PDF mit PdfTkServer'
    TabOrder = 7
    OnClick = Button3Click
  end
end
