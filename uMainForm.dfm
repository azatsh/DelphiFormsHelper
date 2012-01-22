object MainForm: TMainForm
  Left = 205
  Top = 141
  Width = 598
  Height = 427
  Caption = 'MainForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 37
    Height = 13
    Caption = #1055#1088#1086#1077#1082#1090
  end
  object edProject: TEdit
    Left = 24
    Top = 40
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'E:\Work\Projects\gzroot\'
  end
  object btnOpenProject: TButton
    Left = 160
    Top = 40
    Width = 41
    Height = 25
    Caption = '...'
    TabOrder = 1
    OnClick = btnOpenProjectClick
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 374
    Width = 590
    Height = 19
    Panels = <>
  end
  object btnGo: TButton
    Left = 40
    Top = 96
    Width = 75
    Height = 25
    Caption = #1055#1072#1090#1095#1080#1090#1100
    TabOrder = 3
    OnClick = btnGoClick
  end
  object Edit1: TEdit
    Left = 224
    Top = 40
    Width = 121
    Height = 21
    TabOrder = 4
    Text = 'Edit1'
    Visible = False
    OnKeyUp = Edit1KeyUp
  end
  object Edit2: TEdit
    Left = 224
    Top = 64
    Width = 121
    Height = 21
    TabOrder = 5
    Text = 'Edit1'
    Visible = False
    OnKeyUp = Edit1KeyUp
  end
end
