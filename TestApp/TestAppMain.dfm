object frmTestAppMain: TfrmTestAppMain
  Left = 0
  Top = 0
  Caption = 'Delphi AST Test App'
  ClientHeight = 643
  ClientWidth = 814
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 265
    Top = 169
    Height = 474
    ExplicitLeft = 208
    ExplicitTop = 208
    ExplicitHeight = 100
  end
  object PageControl1: TPageControl
    AlignWithMargins = True
    Left = 271
    Top = 172
    Width = 540
    Height = 468
    ActivePage = tsSource
    Align = alClient
    TabOrder = 0
    object tsSource: TTabSheet
      Caption = 'Source'
      object edUnit: TSynEdit
        Left = 0
        Top = 0
        Width = 532
        Height = 440
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
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
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.ShowLineNumbers = True
        Highlighter = SynPasSyn1
        Lines.Strings = (
          'unit TestUnit1;'
          ''
          'interface'
          ''
          'const'
          '  Alpha = ['#39'A'#39'..'#39'Z'#39', '#39'a'#39'..'#39'z'#39', '#39'_'#39'];'
          '  AlphaNumeric = Alpha + ['#39'0'#39'..'#39'9'#39'];'
          '  AlphaNumericDot = AlphaNumeric + ['#39'.'#39'];'
          ''
          'implementation'
          ' '
          'initialization'
          ''
          'end.'
          ''
          ''
          '')
        FontSmoothing = fsmNone
      end
    end
    object tsAST: TTabSheet
      Caption = 'AST'
      ImageIndex = 1
      object tvAST: TTreeView
        Left = 0
        Top = 0
        Width = 532
        Height = 440
        Align = alClient
        Indent = 19
        TabOrder = 0
      end
    end
    object tsNameSpace: TTabSheet
      Caption = 'NameSpace'
      ImageIndex = 2
      object edAllItems: TSynEdit
        Left = 0
        Top = 0
        Width = 532
        Height = 440
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
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
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.ShowLineNumbers = True
        Highlighter = SynPasSyn1
        FontSmoothing = fsmNone
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 814
    Height = 169
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      814
      169)
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 90
      Height = 13
      Caption = 'Delphi Source Path'
    end
    object edSrcRoot: TEdit
      Left = 8
      Top = 24
      Width = 572
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'C:\Program Files (x86)\Embarcadero\Studio\20.0\source\'
    end
    object Button1: TButton
      Left = 586
      Top = 20
      Width = 106
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'AST Parse'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 698
      Top = 20
      Width = 108
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'AST Parse RTL'
      TabOrder = 2
      OnClick = Button2Click
    end
    object Memo1: TMemo
      Left = 8
      Top = 51
      Width = 798
      Height = 118
      Anchors = [akLeft, akTop, akRight]
      Lines.Strings = (
        'Memo1')
      ScrollBars = ssVertical
      TabOrder = 3
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 169
    Width = 265
    Height = 474
    Align = alLeft
    BevelOuter = bvNone
    Caption = 'Panel2'
    TabOrder = 2
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 265
      Height = 33
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Panel3'
      ShowCaption = False
      TabOrder = 0
      object Button3: TButton
        Left = 0
        Top = 6
        Width = 106
        Height = 25
        Caption = 'Load all files'
        TabOrder = 0
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 112
        Top = 6
        Width = 106
        Height = 25
        Caption = 'Parse selected'
        TabOrder = 1
        OnClick = Button4Click
      end
    end
    object lbFiles: TCheckListBox
      Left = 0
      Top = 33
      Width = 265
      Height = 441
      Align = alClient
      ItemHeight = 13
      TabOrder = 1
    end
  end
  object chkbShowSysDecls: TCheckBox
    Left = 432
    Top = 173
    Width = 177
    Height = 17
    Caption = 'Show system declarations'
    TabOrder = 3
  end
  object chkbShowConstValues: TCheckBox
    Left = 576
    Top = 173
    Width = 113
    Height = 17
    Caption = 'Show const values'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object chkbShowAnonymous: TCheckBox
    Left = 695
    Top = 173
    Width = 111
    Height = 17
    Caption = 'Show Anonymous'
    TabOrder = 5
  end
  object SynPasSyn1: TSynPasSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 392
    Top = 400
  end
end
