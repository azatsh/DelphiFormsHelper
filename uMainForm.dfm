object MainForm: TMainForm
  Left = 0
  Top = 0
  ClientHeight = 513
  ClientWidth = 516
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    516
    513)
  PixelsPerInch = 96
  TextHeight = 13
  object btnApply: TSpeedButton
    Left = 327
    Top = 48
    Width = 50
    Height = 22
    Action = actApplyFilter
  end
  object TreeList: TTreeList
    Left = 8
    Top = 90
    Width = 497
    Height = 390
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Courier'
    Font.Style = []
    HideSelection = False
    Indent = 19
    ParentFont = False
    PopupMenu = PopupMenu
    ReadOnly = True
    RightClickSelect = True
    RowSelect = True
    TabOrder = 0
    ToolTips = False
    Visible = True
    Columns = <
      item
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Header = 'Project'
        Width = 150
      end
      item
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        Header = 'File'
        Width = 150
      end
      item
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        Header = 'Form'
        Width = 80
      end
      item
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        Header = 'FormCaption'
        Width = 50
      end>
    ColumnLines = True
    Separator = ';'
    ItemHeight = 18
    HeaderSettings.AllowResize = True
    HeaderSettings.Color = clBtnFace
    HeaderSettings.Font.Charset = DEFAULT_CHARSET
    HeaderSettings.Font.Color = clWindowText
    HeaderSettings.Font.Height = -11
    HeaderSettings.Font.Name = 'Tahoma'
    HeaderSettings.Font.Style = []
    HeaderSettings.Height = 18
    Version = '1.0.1.6'
  end
  object edtFilter: TLabeledEdit
    Left = 8
    Top = 48
    Width = 313
    Height = 21
    EditLabel.Width = 24
    EditLabel.Height = 13
    EditLabel.Caption = 'Filter'
    TabOrder = 2
    OnChange = edtFilterChange
  end
  object chbAutoApply: TCheckBox
    Left = 391
    Top = 50
    Width = 74
    Height = 17
    Action = actAutoApply
    TabOrder = 3
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 516
    Height = 19
    AutoSize = True
    ButtonHeight = 19
    ButtonWidth = 51
    Caption = 'ToolBar'
    List = True
    Menu = MainMenu
    ShowCaptions = True
    TabOrder = 4
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 494
    Width = 516
    Height = 19
    Panels = <>
    SimplePanel = True
    ExplicitLeft = 264
    ExplicitTop = 312
    ExplicitWidth = 0
  end
  object OpenDialog: TOpenDialog
    Filter = 'Borland project group|*.bpg'
    Left = 224
    Top = 144
  end
  object ActionList: TActionList
    Left = 312
    Top = 144
    object actApplyFilter: TAction
      Caption = 'apply'
      OnExecute = actApplyFilterExecute
    end
    object actExecuteFile: TAction
      Caption = 'Open'
      OnExecute = actExecuteFileExecute
    end
    object actAutoApply: TAction
      Caption = 'auto apply'
      OnExecute = actAutoApplyExecute
    end
    object actOpenProject: TAction
      Caption = 'Open'
      OnExecute = actOpenProjectExecute
    end
    object actShowAbout: TAction
      Caption = 'About'
      OnExecute = actShowAboutExecute
    end
  end
  object MainMenu: TMainMenu
    Left = 264
    Top = 144
    object N1: TMenuItem
      Caption = 'File'
      object Open1: TMenuItem
        Action = actOpenProject
      end
    end
    object Settings1: TMenuItem
      Caption = 'Settings'
    end
    object About1: TMenuItem
      AutoCheck = True
      Caption = 'Help'
      object About2: TMenuItem
        Action = actShowAbout
      end
    end
  end
  object PopupMenu: TPopupMenu
    Left = 368
    Top = 144
    object Open2: TMenuItem
      Action = actExecuteFile
    end
    object View1: TMenuItem
      Caption = 'View'
    end
  end
end
