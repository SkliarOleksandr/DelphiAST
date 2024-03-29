object frmTestAppMain: TfrmTestAppMain
  Left = 0
  Top = 0
  Caption = '_'
  ClientHeight = 774
  ClientWidth = 1266
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 203
    Height = 571
    ExplicitLeft = 265
    ExplicitHeight = 440
  end
  object Splitter2: TSplitter
    Left = 0
    Top = 200
    Width = 1266
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ResizeStyle = rsUpdate
    ExplicitWidth = 814
  end
  object Splitter3: TSplitter
    Left = 481
    Top = 203
    Height = 571
    ResizeStyle = rsUpdate
    ExplicitLeft = 518
    ExplicitTop = 206
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1266
    Height = 200
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 1264
    object ErrMemo: TSynEdit
      AlignWithMargins = True
      Left = 3
      Top = 64
      Width = 1260
      Height = 133
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Consolas'
      Font.Pitch = fpFixed
      Font.Style = []
      Font.Quality = fqClearTypeNatural
      TabOrder = 0
      UseCodeFolding = False
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -13
      Gutter.Font.Name = 'Courier New'
      Gutter.Font.Style = []
      Gutter.Font.Quality = fqClearTypeNatural
      Gutter.Visible = False
      Gutter.Bands = <
        item
          Kind = gbkMarks
          Width = 13
        end
        item
          Kind = gbkLineNumbers
        end
        item
          Kind = gbkFold
        end
        item
          Kind = gbkTrackChanges
        end
        item
          Kind = gbkMargin
          Width = 3
        end>
      ReadOnly = True
      SelectedColor.Alpha = 0.400000005960464500
      WordWrap = True
      ExplicitWidth = 1258
    end
    object Panel6: TPanel
      Left = 0
      Top = 0
      Width = 1266
      Height = 61
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 1264
      DesignSize = (
        1266
        61)
      object Label1: TLabel
        Left = 8
        Top = 18
        Width = 90
        Height = 13
        Caption = 'Delphi Source Path'
      end
      object Label2: TLabel
        Left = 714
        Top = 15
        Width = 40
        Height = 13
        Caption = 'Platform'
      end
      object edSrcRoot: TEdit
        Left = 8
        Top = 37
        Width = 1012
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'C:\Program Files (x86)\Embarcadero\Studio\23.0\source\'
        ExplicitWidth = 1010
      end
      object chkStopIfError: TCheckBox
        Left = 112
        Top = 14
        Width = 122
        Height = 17
        Caption = 'Stop compile if errors'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object chkCompileSsystemForASTParse: TCheckBox
        Left = 240
        Top = 14
        Width = 196
        Height = 17
        Caption = 'Compile "system.pas" for AST Parse'
        TabOrder = 2
      end
      object chkParseAll: TCheckBox
        Left = 442
        Top = 14
        Width = 79
        Height = 17
        Caption = 'Parse Impls'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
      object Button1: TButton
        Left = 1026
        Top = 33
        Width = 106
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'AST Parse'
        TabOrder = 4
        OnClick = Button1Click
        ExplicitLeft = 1024
      end
      object Button2: TButton
        Left = 1138
        Top = 34
        Width = 108
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'AST Parse RTL'
        TabOrder = 5
        OnClick = Button2Click
        ExplicitLeft = 1136
      end
      object chkShowWarnings: TCheckBox
        Left = 527
        Top = 14
        Width = 97
        Height = 17
        Caption = 'Show Warnings'
        TabOrder = 6
      end
      object chkWriteLog: TCheckBox
        Left = 630
        Top = 14
        Width = 83
        Height = 17
        Caption = 'Write Log'
        Checked = True
        State = cbChecked
        TabOrder = 7
      end
      object cbPlatform: TComboBox
        Left = 760
        Top = 10
        Width = 81
        Height = 22
        Style = csOwnerDrawFixed
        ItemIndex = 0
        TabOrder = 8
        Text = 'Win32'
        Items.Strings = (
          'Win32'
          'Win64')
      end
    end
  end
  object Panel4: TPanel
    Left = 484
    Top = 203
    Width = 782
    Height = 571
    Align = alClient
    Caption = 'Panel4'
    TabOrder = 1
    ExplicitWidth = 780
    ExplicitHeight = 563
    object PageControl1: TPageControl
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 774
      Height = 563
      ActivePage = tsSource
      Align = alClient
      TabOrder = 0
      ExplicitWidth = 772
      ExplicitHeight = 555
      object tsSource: TTabSheet
        Caption = 'Source'
        object edUnit: TSynEdit
          Left = 0
          Top = 0
          Width = 766
          Height = 535
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Consolas'
          Font.Pitch = fpFixed
          Font.Style = []
          Font.Quality = fqClearTypeNatural
          TabOrder = 0
          UseCodeFolding = False
          BookMarkOptions.LeftMargin = 0
          BookMarkOptions.Xoffset = 0
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Courier New'
          Gutter.Font.Style = []
          Gutter.Font.Quality = fqClearTypeNatural
          Gutter.ShowLineNumbers = True
          Gutter.Bands = <
            item
              Kind = gbkMarks
              Width = 13
            end
            item
              Kind = gbkLineNumbers
            end
            item
              Kind = gbkFold
            end
            item
              Kind = gbkTrackChanges
            end
            item
              Kind = gbkMargin
              Width = 3
            end>
          Highlighter = SynPasSyn1
          Lines.Strings = (
            'unit TestUnit.XXX;'
            ''
            'interface'
            ''
            'type'
            ''
            '  TA1<T> = class'
            '  type'
            '    TRec = record'
            '      A, B: T;'
            '    end;'
            '  private'
            '    Fld2: T;'
            '  end;'
            ''
            'var'
            '  X: TA1<string>;'
            '  Y: TA1<Integer>.TRec;'
            ''
            'implementation'
            ''
            'end.'
            ''
            '')
          SelectedColor.Alpha = 0.400000005960464500
        end
      end
      object tsAST: TTabSheet
        Caption = 'AST'
        ImageIndex = 1
        object tvAST: TTreeView
          Left = 0
          Top = 0
          Width = 766
          Height = 535
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
          Top = 31
          Width = 766
          Height = 504
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          Font.Quality = fqClearTypeNatural
          TabOrder = 0
          UseCodeFolding = False
          BookMarkOptions.LeftMargin = 0
          BookMarkOptions.Xoffset = 0
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Courier New'
          Gutter.Font.Style = []
          Gutter.Font.Quality = fqClearTypeNatural
          Gutter.ShowLineNumbers = True
          Gutter.Bands = <
            item
              Kind = gbkMarks
              Width = 13
            end
            item
              Kind = gbkLineNumbers
            end
            item
              Kind = gbkFold
            end
            item
              Kind = gbkTrackChanges
            end
            item
              Kind = gbkMargin
              Width = 3
            end>
          Highlighter = SynPasSyn1
          Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoRightMouseMovesCursor, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
          SearchEngine = SynEditSearch1
          SelectedColor.Alpha = 0.400000005960464500
        end
        object Panel5: TPanel
          Left = 0
          Top = 0
          Width = 766
          Height = 31
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          object Button5: TButton
            AlignWithMargins = True
            Left = 688
            Top = 3
            Width = 75
            Height = 25
            Align = alRight
            Caption = 'Search'
            TabOrder = 0
            OnClick = Button5Click
          end
          object NSSearchEdit: TEdit
            AlignWithMargins = True
            Left = 3
            Top = 4
            Width = 679
            Height = 23
            Margins.Top = 4
            Margins.Bottom = 4
            Align = alClient
            TabOrder = 1
            ExplicitHeight = 21
          end
        end
      end
    end
    object chkbShowAnonymous: TCheckBox
      Left = 427
      Top = 6
      Width = 111
      Height = 18
      Caption = 'Show Anonymous'
      TabOrder = 1
    end
    object chkbShowConstValues: TCheckBox
      Left = 314
      Top = 6
      Width = 107
      Height = 18
      Caption = 'Show const values'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object chkbShowSysDecls: TCheckBox
      Left = 162
      Top = 6
      Width = 145
      Height = 18
      Caption = 'Show system declarations'
      TabOrder = 3
    end
  end
  object PageControl2: TPageControl
    Left = 3
    Top = 203
    Width = 478
    Height = 571
    ActivePage = tsLogs
    Align = alLeft
    TabOrder = 2
    ExplicitHeight = 563
    object tsLogs: TTabSheet
      Caption = 'Logs'
      object LogMemo: TSynEdit
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 464
        Height = 537
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Consolas'
        Font.Pitch = fpFixed
        Font.Style = []
        Font.Quality = fqClearTypeNatural
        TabOrder = 0
        UseCodeFolding = False
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -13
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Font.Quality = fqClearTypeNatural
        Gutter.ShowLineNumbers = True
        Gutter.Bands = <
          item
            Kind = gbkMarks
            Width = 13
          end
          item
            Kind = gbkLineNumbers
          end
          item
            Kind = gbkFold
          end
          item
            Kind = gbkTrackChanges
          end
          item
            Kind = gbkMargin
            Width = 3
          end>
        ReadOnly = True
        SelectedColor.Alpha = 0.400000005960464500
        WordWrap = True
        ExplicitHeight = 529
      end
    end
    object tsFiles: TTabSheet
      Caption = 'Files'
      ImageIndex = 1
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 436
        Height = 543
        Align = alLeft
        BevelOuter = bvNone
        Caption = 'Panel2'
        TabOrder = 0
        object Panel3: TPanel
          Left = 0
          Top = 0
          Width = 436
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
          Width = 436
          Height = 510
          Align = alClient
          ItemHeight = 17
          TabOrder = 1
        end
      end
    end
  end
  object SynPasSyn1: TSynPasSyn
    Left = 448
    Top = 336
  end
  object SynEditSearch1: TSynEditSearch
    Left = 355
    Top = 325
  end
end
