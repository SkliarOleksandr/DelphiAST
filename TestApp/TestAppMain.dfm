object frmTestAppMain: TfrmTestAppMain
  Left = 0
  Top = 0
  Caption = 'Delphi AST'
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
  object MainPageControl: TPageControl
    Left = 0
    Top = 0
    Width = 1266
    Height = 774
    ActivePage = SrcTabSheet
    Align = alClient
    TabOrder = 0
    object SrcTabSheet: TTabSheet
      Caption = 'Source'
      object Splitter2: TSplitter
        Left = 0
        Top = 543
        Width = 1258
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        ResizeStyle = rsUpdate
        ExplicitTop = 552
      end
      object Splitter3: TSplitter
        Left = 390
        Top = 33
        Height = 510
        ResizeStyle = rsUpdate
        ExplicitLeft = 518
        ExplicitTop = 187
        ExplicitHeight = 543
      end
      object LeftPageControl: TPageControl
        Left = 0
        Top = 33
        Width = 390
        Height = 510
        ActivePage = tsFiles
        Align = alLeft
        TabOrder = 0
        object tsLogs: TTabSheet
          Caption = 'Logs'
          object LogMemo: TSynEdit
            AlignWithMargins = True
            Left = 3
            Top = 20
            Width = 376
            Height = 459
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
          end
          object Panel1: TPanel
            Left = 0
            Top = 0
            Width = 382
            Height = 17
            Align = alTop
            BevelOuter = bvNone
            ShowCaption = False
            TabOrder = 1
            object WriteLogCheck: TCheckBox
              Left = 3
              Top = 0
              Width = 83
              Height = 17
              Caption = 'Write Log'
              Checked = True
              State = cbChecked
              TabOrder = 0
              OnClick = StopIfErrorCheckClick
            end
          end
        end
        object tsFiles: TTabSheet
          Caption = 'Parse Files'
          ImageIndex = 1
          object Panel2: TPanel
            Left = 0
            Top = 0
            Width = 436
            Height = 482
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
              object LoadFilesButton: TButton
                Left = 0
                Top = 2
                Width = 106
                Height = 25
                Caption = 'Load all files'
                TabOrder = 0
                OnClick = LoadFilesButtonClick
              end
              object ParseFilesButton: TButton
                Left = 112
                Top = 2
                Width = 106
                Height = 25
                Caption = 'Parse selected'
                TabOrder = 1
                OnClick = ParseFilesButtonClick
              end
            end
            object lbFiles: TCheckListBox
              Left = 0
              Top = 33
              Width = 436
              Height = 449
              Align = alClient
              ItemHeight = 17
              TabOrder = 1
            end
          end
        end
      end
      object BottomPanel: TPanel
        Left = 0
        Top = 546
        Width = 1258
        Height = 200
        Align = alBottom
        BevelOuter = bvNone
        ShowCaption = False
        TabOrder = 1
        object ErrMemo: TSynEdit
          AlignWithMargins = True
          Left = 3
          Top = 23
          Width = 1252
          Height = 174
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
        end
        object Panel6: TPanel
          Left = 0
          Top = 0
          Width = 1258
          Height = 20
          Align = alTop
          BevelOuter = bvNone
          ShowCaption = False
          TabOrder = 1
          object ShowWarningsCheck: TCheckBox
            Left = 3
            Top = 3
            Width = 97
            Height = 17
            Caption = 'Show Warnings'
            TabOrder = 0
            OnClick = StopIfErrorCheckClick
          end
        end
      end
      object MainPanel: TPanel
        Left = 393
        Top = 33
        Width = 865
        Height = 510
        Align = alClient
        Caption = 'MainPanel'
        ShowCaption = False
        TabOrder = 2
        object SrcPageControl: TPageControl
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 857
          Height = 502
          ActivePage = tsNameSpace
          Align = alClient
          TabOrder = 0
          object tsSource: TTabSheet
            Caption = 'Source'
            object edUnit: TSynEdit
              Left = 0
              Top = 35
              Width = 849
              Height = 439
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
            object Panel7: TPanel
              Left = 0
              Top = 0
              Width = 849
              Height = 35
              Align = alTop
              BevelOuter = bvNone
              ShowCaption = False
              TabOrder = 1
              object SaveButton: TButton
                Left = 8
                Top = 4
                Width = 75
                Height = 25
                Caption = 'Save'
                TabOrder = 0
                OnClick = SaveButtonClick
              end
            end
          end
          object tsAST: TTabSheet
            Caption = 'AST'
            ImageIndex = 1
            object tvAST: TTreeView
              Left = 0
              Top = 0
              Width = 849
              Height = 474
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
              Width = 849
              Height = 443
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
              Width = 849
              Height = 31
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 1
              object Button5: TButton
                AlignWithMargins = True
                Left = 771
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
                Width = 762
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
          Left = 428
          Top = 5
          Width = 111
          Height = 18
          Caption = 'Show Anonymous'
          TabOrder = 1
          OnClick = StopIfErrorCheckClick
        end
        object chkbShowConstValues: TCheckBox
          Left = 315
          Top = 5
          Width = 107
          Height = 18
          Caption = 'Show const values'
          Checked = True
          State = cbChecked
          TabOrder = 2
          OnClick = StopIfErrorCheckClick
        end
        object chkbShowSysDecls: TCheckBox
          Left = 163
          Top = 5
          Width = 145
          Height = 18
          Caption = 'Show system declarations'
          TabOrder = 3
          OnClick = StopIfErrorCheckClick
        end
      end
      object TopPanel: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 1252
        Height = 27
        Align = alTop
        BevelOuter = bvNone
        ShowCaption = False
        TabOrder = 3
        DesignSize = (
          1252
          27)
        object Label2: TLabel
          Left = 488
          Top = 3
          Width = 40
          Height = 13
          Caption = 'Platform'
        end
        object ASTParseButton: TButton
          Left = 1032
          Top = 1
          Width = 106
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'AST Parse'
          TabOrder = 0
          OnClick = ASTParseButtonClick
        end
        object ASTParseRTLButton: TButton
          Left = 1144
          Top = 1
          Width = 108
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'AST Parse RTL'
          TabOrder = 1
          OnClick = ASTParseRTLButtonClick
        end
        object ParseSystemCheck: TCheckBox
          Left = 129
          Top = 2
          Width = 196
          Height = 17
          Caption = 'Compile "system.pas" for AST Parse'
          TabOrder = 2
          OnClick = StopIfErrorCheckClick
        end
        object cbPlatform: TComboBox
          Left = 534
          Top = 0
          Width = 81
          Height = 21
          ItemIndex = 0
          TabOrder = 3
          Text = 'Win32'
          OnChange = StopIfErrorCheckClick
          Items.Strings = (
            'Win32'
            'Win64')
        end
        object ParseImplsCheck: TCheckBox
          Left = 331
          Top = 2
          Width = 79
          Height = 17
          Caption = 'Parse Impls'
          Checked = True
          State = cbChecked
          TabOrder = 4
          OnClick = StopIfErrorCheckClick
        end
        object StopIfErrorCheck: TCheckBox
          Left = 1
          Top = 2
          Width = 122
          Height = 17
          Caption = 'Stop compile if errors'
          Checked = True
          State = cbChecked
          TabOrder = 5
          OnClick = StopIfErrorCheckClick
        end
      end
    end
    object SettingsTabSheet: TTabSheet
      Caption = 'Settings'
      ImageIndex = 1
      DesignSize = (
        1258
        746)
      object Label1: TLabel
        Left = 14
        Top = 18
        Width = 90
        Height = 13
        Caption = 'Delphi Source Path'
      end
      object Label3: TLabel
        Left = 14
        Top = 64
        Width = 86
        Height = 13
        Caption = 'Unit Scope Names'
      end
      object Label4: TLabel
        Left = 14
        Top = 110
        Width = 80
        Height = 13
        Caption = 'Unit Search Path'
      end
      object Label5: TLabel
        Left = 14
        Top = 156
        Width = 92
        Height = 13
        Caption = 'Conditional Defines'
      end
      object DelphiSrcPathEdit: TEdit
        Left = 14
        Top = 37
        Width = 1241
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'C:\Program Files (x86)\Embarcadero\Studio\23.0\source\'
      end
      object UnitScopeNamesEdit: TEdit
        Left = 14
        Top = 83
        Width = 1241
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 
          'Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win' +
          ';Bde;System;Xml;Data;Datasnap;Web;Soap;Vcl;Vcl.Imaging;Vcl.Touch' +
          ';Vcl.Samples;Vcl.Shell'
      end
      object SaveSettingsButton: TButton
        Left = 1120
        Top = 286
        Width = 119
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Save Settigns'
        TabOrder = 2
        OnClick = SaveSettingsButtonClick
      end
      object UnitSearchPathEdit: TEdit
        Left = 14
        Top = 129
        Width = 1241
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        Text = './'
      end
      object CondDefinesEdit: TEdit
        Left = 14
        Top = 175
        Width = 1241
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        Text = './'
      end
      object DelphiPathIncludeSubDirCheck: TCheckBox
        Left = 1118
        Top = 16
        Width = 137
        Height = 15
        Anchors = [akTop, akRight]
        Caption = 'Include Subdirectories'
        Checked = True
        State = cbChecked
        TabOrder = 5
      end
      object UnitSearchPathIncludeSubDirCheck: TCheckBox
        Left = 1118
        Top = 110
        Width = 137
        Height = 15
        Anchors = [akTop, akRight]
        Caption = 'Include Subdirectories'
        Checked = True
        State = cbChecked
        TabOrder = 6
      end
      object ShowMemLeaksCheck: TCheckBox
        Left = 14
        Top = 216
        Width = 123
        Height = 17
        Caption = 'Show Memory Leaks'
        TabOrder = 7
        OnClick = ShowMemLeaksCheckClick
      end
    end
  end
  object SynPasSyn1: TSynPasSyn
    Left = 128
    Top = 344
  end
  object SynEditSearch1: TSynEditSearch
    Left = 131
    Top = 269
  end
end
