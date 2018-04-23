object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 986
  ClientWidth = 1626
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 16
    Top = 16
    Width = 777
    Height = 929
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 808
    Top = 119
    Width = 809
    Height = 826
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object Button1: TButton
    Left = 16
    Top = 951
    Width = 75
    Height = 25
    Caption = 'Laden'
    TabOrder = 2
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 808
    Top = 16
    Width = 441
    Height = 97
    ItemHeight = 13
    TabOrder = 3
  end
  object Button2: TButton
    Left = 1255
    Top = 16
    Width = 98
    Height = 25
    Caption = 'Anhang anzeigen'
    TabOrder = 4
    OnClick = Button2Click
  end
end
