object frmTestAppMain: TfrmTestAppMain
  Left = 0
  Top = 0
  Caption = 'Delphi AST Test App'
  ClientHeight = 804
  ClientWidth = 1018
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 17
  object Splitter1: TSplitter
    Left = 331
    Top = 254
    Width = 4
    Height = 550
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ExplicitTop = 211
    ExplicitHeight = 593
  end
  object Splitter2: TSplitter
    Left = 0
    Top = 250
    Width = 1018
    Height = 4
    Cursor = crVSplit
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    ResizeStyle = rsUpdate
    ExplicitLeft = -60
    ExplicitTop = 180
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1018
    Height = 250
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      1018
      250)
    object Label1: TLabel
      Left = 10
      Top = 10
      Width = 116
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Delphi Source Path'
    end
    object edSrcRoot: TEdit
      Left = 10
      Top = 30
      Width = 715
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'C:\Program Files (x86)\Embarcadero\Studio\22.0\source\'
    end
    object Button1: TButton
      Left = 733
      Top = 25
      Width = 132
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akTop, akRight]
      Caption = 'AST Parse'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 873
      Top = 25
      Width = 135
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akTop, akRight]
      Caption = 'AST Parse RTL'
      TabOrder = 2
      OnClick = Button2Click
    end
    object Memo1: TMemo
      Left = 10
      Top = 64
      Width = 998
      Height = 186
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akLeft, akTop, akRight, akBottom]
      Lines.Strings = (
        'Memo1')
      ScrollBars = ssVertical
      TabOrder = 3
      ExplicitHeight = 147
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 254
    Width = 331
    Height = 550
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alLeft
    BevelOuter = bvNone
    Caption = 'Panel2'
    TabOrder = 1
    ExplicitTop = 211
    ExplicitHeight = 593
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 331
      Height = 41
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Panel3'
      ShowCaption = False
      TabOrder = 0
      object Button3: TButton
        Left = 0
        Top = 8
        Width = 133
        Height = 31
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Load all files'
        TabOrder = 0
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 140
        Top = 8
        Width = 133
        Height = 31
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Parse selected'
        TabOrder = 1
        OnClick = Button4Click
      end
    end
    object lbFiles: TCheckListBox
      Left = 0
      Top = 41
      Width = 331
      Height = 509
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alClient
      ItemHeight = 17
      TabOrder = 1
      ExplicitLeft = 1
      ExplicitTop = 47
    end
  end
  object Panel4: TPanel
    Left = 335
    Top = 254
    Width = 683
    Height = 550
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    Caption = 'Panel4'
    TabOrder = 2
    ExplicitLeft = 0
    ExplicitTop = 0
    ExplicitWidth = 667
    ExplicitHeight = 510
    object PageControl1: TPageControl
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 673
      Height = 540
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ActivePage = tsSource
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 339
      ExplicitTop = 258
      ExplicitWidth = 675
      ExplicitHeight = 542
      object tsSource: TTabSheet
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Source'
        object edUnit: TSynEdit
          Left = 0
          Top = 0
          Width = 665
          Height = 508
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Courier New'
          Font.Style = []
          TabOrder = 0
          CodeFolding.GutterShapeSize = 11
          CodeFolding.CollapsedLineColor = clGrayText
          CodeFolding.FolderBarLinesColor = clGrayText
          CodeFolding.IndentGuidesColor = clGray
          CodeFolding.IndentGuides = True
          CodeFolding.ShowCollapsedLine = False
          CodeFolding.ShowHintMark = True
          UseCodeFolding = False
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -14
          Gutter.Font.Name = 'Courier New'
          Gutter.Font.Style = []
          Gutter.LeftOffset = 20
          Gutter.RightOffset = 3
          Gutter.ShowLineNumbers = True
          Gutter.Width = 38
          Highlighter = SynPasSyn1
          Lines.Strings = (
            'unit TestUnit1;'
            ''
            'interface'
            ''
            'implementation'
            ' '
            ''
            'function Get: pointer;'
            'begin'
            'end;'
            ''
            'initialization'
            '  if Get = nil then; '
            ''
            'end.')
          FontSmoothing = fsmNone
          ExplicitWidth = 667
          ExplicitHeight = 510
        end
      end
      object tsAST: TTabSheet
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'AST'
        ImageIndex = 1
        object tvAST: TTreeView
          Left = 0
          Top = 0
          Width = 665
          Height = 508
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alClient
          Indent = 24
          TabOrder = 0
        end
      end
      object tsNameSpace: TTabSheet
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'NameSpace'
        ImageIndex = 2
        object edAllItems: TSynEdit
          Left = 0
          Top = 0
          Width = 665
          Height = 508
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Courier New'
          Font.Style = []
          TabOrder = 0
          CodeFolding.GutterShapeSize = 11
          CodeFolding.CollapsedLineColor = clGrayText
          CodeFolding.FolderBarLinesColor = clGrayText
          CodeFolding.IndentGuidesColor = clGray
          CodeFolding.IndentGuides = True
          CodeFolding.ShowCollapsedLine = False
          CodeFolding.ShowHintMark = True
          UseCodeFolding = False
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -14
          Gutter.Font.Name = 'Courier New'
          Gutter.Font.Style = []
          Gutter.LeftOffset = 20
          Gutter.RightOffset = 3
          Gutter.ShowLineNumbers = True
          Gutter.Width = 38
          Highlighter = SynPasSyn1
          FontSmoothing = fsmNone
        end
      end
    end
    object chkbShowAnonymous: TCheckBox
      Left = 534
      Top = 8
      Width = 139
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Show Anonymous'
      TabOrder = 1
    end
    object chkbShowConstValues: TCheckBox
      Left = 392
      Top = 8
      Width = 134
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Show const values'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object chkbShowSysDecls: TCheckBox
      Left = 202
      Top = 8
      Width = 182
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Show system declarations'
      TabOrder = 3
    end
  end
  object SynPasSyn1: TSynPasSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 392
    Top = 400
  end
end
