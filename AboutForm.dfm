object AboutFrm: TAboutFrm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 124
  ClientWidth = 423
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object RichEdit1: TRichEdit
    Left = 8
    Top = 8
    Width = 401
    Height = 104
    Color = clBtnFace
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    Lines.Strings = (
      #1055#1088#1086#1075#1088#1072#1084#1084#1072' '#1076#1083#1103' '#1088#1072#1073#1086#1090#1099' '#1089' '#1075#1088#1091#1087#1087#1086#1081' '#1087#1088#1086#1077#1082#1090#1086#1074' '#1076#1077#1083#1092#1080' (BPG). '
      ''
      'author: a.sharipov '
      'e-mail: azatsh@yandex.ru')
    ParentFont = False
    ReadOnly = True
    TabOrder = 0
    WantReturns = False
    WordWrap = False
  end
end
