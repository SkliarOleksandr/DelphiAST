object frmTestAppMain: TfrmTestAppMain
  Left = 0
  Top = 0
  Caption = 'Delphi AST Parser'
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
  OnDestroy = FormDestroy
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
        ActivePage = tsTestScripts
        Align = alLeft
        TabOrder = 0
        object tsTestScripts: TTabSheet
          Caption = ' Tests'
          ImageIndex = 2
          object VTTests: TVirtualStringTree
            Left = 0
            Top = 35
            Width = 382
            Height = 421
            Align = alClient
            Colors.BorderColor = 15987699
            Colors.DisabledColor = clGray
            Colors.DropMarkColor = 15385233
            Colors.DropTargetColor = 15385233
            Colors.DropTargetBorderColor = 15385233
            Colors.FocusedSelectionColor = 15385233
            Colors.FocusedSelectionBorderColor = 15385233
            Colors.GridLineColor = 15987699
            Colors.HeaderHotColor = clBlack
            Colors.HotColor = clBlack
            Colors.SelectionRectangleBlendColor = 15385233
            Colors.SelectionRectangleBorderColor = 15385233
            Colors.SelectionTextColor = clBlack
            Colors.TreeLineColor = 9471874
            Colors.UnfocusedColor = clBlack
            Colors.UnfocusedSelectionColor = clWhite
            Colors.UnfocusedSelectionBorderColor = clWhite
            CustomCheckImages = ImageList1
            DefaultNodeHeight = 17
            Header.AutoSizeIndex = -1
            Header.Height = 13
            Header.MainColumn = -1
            Images = ImageList1
            IncrementalSearch = isAll
            IncrementalSearchTimeout = 2000
            ParentShowHint = False
            PopupMenu = TestsPopup
            ShowHint = True
            TabOrder = 0
            TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
            TreeOptions.SelectionOptions = [toRightClickSelect]
            OnChecking = VTTestsChecking
            OnClick = VTTestsClick
            OnDrawText = VTTestsDrawText
            OnGetText = VTTestsGetText
            OnFreeNode = VTTestsFreeNode
            OnGetImageIndex = VTTestsGetImageIndex
            OnNodeClick = VTTestsNodeClick
            OnNodeDblClick = VTTestsNodeDblClick
            Touch.InteractiveGestures = [igPan, igPressAndTap]
            Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
            Columns = <>
          end
          object TestsTopPanel: TPanel
            Left = 0
            Top = 0
            Width = 382
            Height = 35
            Align = alTop
            BevelOuter = bvNone
            ShowCaption = False
            TabOrder = 1
            DesignSize = (
              382
              35)
            object RunAllTestsButton: TButton
              Left = 280
              Top = 4
              Width = 100
              Height = 25
              Action = ParseAllTestsAction
              Anchors = [akTop, akRight]
              TabOrder = 0
            end
          end
          object TestsBottomPanel: TPanel
            Left = 0
            Top = 456
            Width = 382
            Height = 26
            Align = alBottom
            BevelOuter = bvNone
            ShowCaption = False
            TabOrder = 2
            object TotalTestCntLabel: TLabel
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 66
              Height = 13
              Align = alLeft
              Caption = 'Total Tests: 0'
              Layout = tlCenter
            end
            object TestRunProgressLabel: TLabel
              AlignWithMargins = True
              Left = 316
              Top = 3
              Width = 63
              Height = 13
              Align = alClient
              Alignment = taRightJustify
              Caption = 'Test Not Run'
              Layout = tlCenter
            end
          end
        end
        object tsFiles: TTabSheet
          Caption = 'Parse Files'
          ImageIndex = 1
          object Panel2: TPanel
            Left = 0
            Top = 0
            Width = 382
            Height = 482
            Align = alClient
            BevelOuter = bvNone
            Caption = 'Panel2'
            TabOrder = 0
            object Panel3: TPanel
              Left = 0
              Top = 0
              Width = 382
              Height = 33
              Align = alTop
              BevelOuter = bvNone
              Caption = 'Panel3'
              ShowCaption = False
              TabOrder = 0
              DesignSize = (
                382
                33)
              object AddFilesButton: TButton
                Left = 0
                Top = 2
                Width = 75
                Height = 25
                Action = AddFilesAction
                TabOrder = 0
              end
              object ParseFilesButton: TButton
                Left = 305
                Top = 2
                Width = 75
                Height = 25
                Action = ParseFilesAction
                Anchors = [akTop, akRight]
                TabOrder = 1
              end
              object Button1: TButton
                Left = 81
                Top = 2
                Width = 75
                Height = 25
                Action = RemoveFilesAction
                TabOrder = 2
              end
              object SaveASTCheckBox: TCheckBox
                Left = 228
                Top = 10
                Width = 71
                Height = 17
                Hint = 
                  'Saves the AST as a JSON files in the same directories as the original PAS' +
                  ' files'
                Anchors = [akTop, akRight]
                Caption = 'Save AST'
                ParentShowHint = False
                ShowHint = True
                TabOrder = 3
              end
            end
            object lbFiles: TCheckListBox
              Left = 0
              Top = 33
              Width = 382
              Height = 449
              Align = alClient
              ItemHeight = 17
              PopupMenu = FilesPopup
              TabOrder = 1
              OnDblClick = lbFilesDblClick
            end
          end
        end
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
          object UnitsFullPathCheck: TCheckBox
            Left = 106
            Top = 3
            Width = 97
            Height = 17
            Caption = 'Units Full Path'
            TabOrder = 1
            OnClick = StopIfErrorCheckClick
          end
          object ShowProgressCheck: TCheckBox
            Left = 209
            Top = 3
            Width = 97
            Height = 17
            Caption = 'Show Progress'
            TabOrder = 2
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
          ActivePage = tsSource
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
              OnChange = edUnitChange
              OnSpecialLineColors = edUnitSpecialLineColors
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
                Left = 0
                Top = 4
                Width = 75
                Height = 25
                Action = SaveSourceAction
                TabOrder = 0
              end
              object ASTParseButton: TButton
                Left = 79
                Top = 4
                Width = 75
                Height = 25
                Action = ASTParseAction
                TabOrder = 1
              end
            end
          end
          object tsAST: TTabSheet
            Caption = 'AST'
            ImageIndex = 1
            object ASTResultTopPanel: TPanel
              Left = 0
              Top = 0
              Width = 849
              Height = 31
              Align = alTop
              BevelOuter = bvNone
              ShowCaption = False
              TabOrder = 0
              object ASTResultFormatComboBox: TComboBox
                AlignWithMargins = True
                Left = 3
                Top = 3
                Width = 145
                Height = 22
                Align = alLeft
                Style = csOwnerDrawFixed
                ItemIndex = 0
                TabOrder = 0
                Text = 'Show as Code'
                OnChange = ASTResultFormatComboBoxChange
                Items.Strings = (
                  'Show as Code'
                  'Show as JSON')
              end
              object ASTResultViewComboBox: TComboBox
                AlignWithMargins = True
                Left = 154
                Top = 3
                Width = 145
                Height = 22
                Align = alLeft
                Style = csOwnerDrawFixed
                ItemIndex = 0
                TabOrder = 1
                Text = 'Show in Single File'
                OnChange = ASTResultFormatComboBoxChange
                Items.Strings = (
                  'Show in Single File'
                  'Show in Separate Files ')
              end
              object SearchEdit: TEdit
                AlignWithMargins = True
                Left = 305
                Top = 3
                Width = 460
                Height = 24
                Margins.Bottom = 4
                Align = alClient
                TabOrder = 2
                ExplicitHeight = 21
              end
              object SearchButton: TButton
                AlignWithMargins = True
                Left = 771
                Top = 3
                Width = 75
                Height = 25
                Align = alRight
                Caption = 'Search'
                TabOrder = 3
                OnClick = SearchButtonClick
              end
            end
            object ASTPageControl: TPageControl
              Left = 0
              Top = 31
              Width = 849
              Height = 443
              Align = alClient
              TabOrder = 1
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
          Left = 608
          Top = 3
          Width = 40
          Height = 13
          Caption = 'Platform'
        end
        object ASTParseRTLButton: TButton
          Left = 1144
          Top = 1
          Width = 108
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'AST Parse RTL'
          TabOrder = 0
          OnClick = ASTParseRTLButtonClick
        end
        object ParseSystemCheck: TCheckBox
          Left = 129
          Top = 2
          Width = 196
          Height = 17
          Caption = 'Compile "system.pas" for AST Parse'
          TabOrder = 1
          OnClick = StopIfErrorCheckClick
        end
        object cbPlatform: TComboBox
          Left = 654
          Top = 0
          Width = 81
          Height = 21
          ItemIndex = 0
          TabOrder = 2
          Text = 'Win32'
          OnChange = StopIfErrorCheckClick
          Items.Strings = (
            'Win32'
            'Win64')
        end
        object ParseImplsCheck: TCheckBox
          Left = 331
          Top = 2
          Width = 142
          Height = 17
          Caption = 'Parse Used Units Impl.'
          Checked = True
          State = cbChecked
          TabOrder = 3
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
          TabOrder = 4
          OnClick = StopIfErrorCheckClick
        end
        object BreakpointOnErrorCheck: TCheckBox
          Left = 479
          Top = -3
          Width = 114
          Height = 27
          Caption = 'Breakpoint on Error'
          TabOrder = 5
          OnClick = StopIfErrorCheckClick
        end
        object ParseRtlCommonCheck: TCheckBox
          Left = 1054
          Top = 3
          Width = 82
          Height = 17
          Anchors = [akTop, akRight]
          Caption = 'rtl\common'
          TabOrder = 6
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
        Top = 112
        Width = 86
        Height = 13
        Caption = 'Unit Scope Names'
      end
      object Label4: TLabel
        Left = 14
        Top = 158
        Width = 80
        Height = 13
        Caption = 'Unit Search Path'
      end
      object Label5: TLabel
        Left = 14
        Top = 204
        Width = 92
        Height = 13
        Caption = 'Conditional Defines'
      end
      object Label6: TLabel
        Left = 14
        Top = 250
        Width = 81
        Height = 13
        Caption = 'Test Scripts Path'
      end
      object Label7: TLabel
        Left = 14
        Top = 64
        Width = 64
        Height = 13
        Caption = 'Project Name'
      end
      object DelphiSrcPathEdit: TEdit
        Left = 14
        Top = 37
        Width = 1098
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'C:\Program Files (x86)\Embarcadero\Studio\23.0\source\'
      end
      object UnitScopeNamesEdit: TEdit
        Left = 14
        Top = 131
        Width = 1227
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 
          'Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win' +
          ';Bde;System;Xml;Data;Datasnap;Web;Soap;Vcl;Vcl.Imaging;Vcl.Touch' +
          ';Vcl.Samples;Vcl.Shell'
      end
      object SaveSettingsButton: TButton
        Left = 1122
        Top = 326
        Width = 119
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Save Settigns'
        TabOrder = 2
        OnClick = SaveSettingsButtonClick
      end
      object UnitSearchPathEdit: TEdit
        Left = 14
        Top = 177
        Width = 1227
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        Text = './'
      end
      object CondDefinesEdit: TEdit
        Left = 14
        Top = 223
        Width = 1227
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        Text = 'DEBUG'
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
        Top = 302
        Width = 123
        Height = 17
        Caption = 'Show Memory Leaks'
        TabOrder = 7
        OnClick = ShowMemLeaksCheckClick
      end
      object TestScriptsPathEdit: TEdit
        Left = 14
        Top = 269
        Width = 1227
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 8
        Text = '..\..\TestScripts'
      end
      object LoadLSPConfigButton: TButton
        Left = 994
        Top = 326
        Width = 122
        Height = 25
        Action = LoadLSPConfigAction
        Anchors = [akTop, akRight]
        TabOrder = 9
      end
      object ProjectNameEdit: TEdit
        Left = 14
        Top = 83
        Width = 1227
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 10
        Text = 'Project1'
      end
      object DelphiDirComboBox: TComboBox
        Left = 1118
        Top = 37
        Width = 123
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 11
        OnChange = DelphiDirComboBoxChange
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
  object ActionList1: TActionList
    Left = 240
    Top = 185
    object AddFilesAction: TAction
      Caption = 'Add Files'
      OnExecute = AddFilesActionExecute
    end
    object RemoveFilesAction: TAction
      Caption = 'Remove Files'
      OnExecute = RemoveFilesActionExecute
      OnUpdate = RemoveFilesActionUpdate
    end
    object ParseFilesAction: TAction
      Caption = 'Parse Files '
      OnExecute = ParseFilesActionExecute
      OnUpdate = ParseFilesActionUpdate
    end
    object SaveSourceAction: TAction
      Caption = 'Save (Ctrl+S)'
      ShortCut = 16467
      OnExecute = SaveSourceActionExecute
      OnUpdate = SaveSourceActionUpdate
    end
    object ASTParseAction: TAction
      Caption = 'Parse (F9)'
      ShortCut = 120
      OnExecute = ASTParseActionExecute
      OnUpdate = ASTParseActionUpdate
    end
    object CreateNewDirAction: TAction
      Caption = 'Create New Directory'
      OnExecute = CreateNewDirActionExecute
      OnUpdate = CreateNewDirActionUpdate
    end
    object CreateNewTestAction: TAction
      Caption = 'Create New Test'
      OnExecute = CreateNewTestActionExecute
      OnUpdate = CreateNewDirActionUpdate
    end
    object RenameTestAction: TAction
      Caption = 'Rename'
      ShortCut = 113
      OnExecute = RenameTestActionExecute
      OnUpdate = RenameTestActionUpdate
    end
    object ParseAllTestsAction: TAction
      Caption = 'Parse All Tests'
      OnExecute = ParseAllTestsActionExecute
    end
    object ParseSelectedTestAction: TAction
      Caption = 'Parse Selected'
      OnExecute = ParseSelectedTestActionExecute
      OnUpdate = ParseSelectedTestActionUpdate
    end
    object LoadLSPConfigAction: TAction
      Caption = 'Load from LSP Config'
      OnExecute = LoadLSPConfigActionExecute
    end
    object FilesCheckAllAction: TAction
      Caption = 'Check/Uncheck All'
      OnExecute = FilesCheckAllActionExecute
      OnUpdate = FilesCheckAllActionUpdate
    end
    object FilesParseFocusedAction: TAction
      Caption = 'Parse Focused'
      OnExecute = FilesParseFocusedActionExecute
      OnUpdate = FilesParseFocusedActionUpdate
    end
    object FilesRemoveAllAction: TAction
      Caption = 'Remove All'
      OnExecute = FilesRemoveAllActionExecute
    end
    object FilesEditAction: TAction
      Caption = 'Edit'
      OnExecute = FilesEditActionExecute
      OnUpdate = FilesEditActionUpdate
    end
    object GoToLineAction: TAction
      Caption = 'Go To Line Number...'
      ShortCut = 16455
      OnExecute = GoToLineActionExecute
      OnUpdate = GoToLineActionUpdate
    end
  end
  object ImageList1: TImageList
    ColorDepth = cd32Bit
    Left = 264
    Top = 408
    Bitmap = {
      494C010107006001040010001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005050505D616161900000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004747474F000000FF000000FF6161
      6190000000000000000000000000000000000000000000000000000000000000
      000000000001000000FF585858B600000000000000006E8890FF6E7680FF5E6E
      6EFF4E5E5EFF3E4E4EFF2E363EFF1E262EFF0E1E1EFF0E0E1EFF0E0E1EFF0E0E
      1EFF0E0E1EFF0E0E1EFF0E0E1EFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001C08007E1606007E000000000000000000000000000000000000
      00000000000000000000000000000000000025252526000000FF000000FF0000
      00FF5E5E5E7B0000000000000000000000000000000000000000000000000505
      0506121212F5000000FF5353536300000000000000006E8890FFA0E0F0FF6ED0
      F0FF4EB8E0FF2EB0E0FF2EA8E0FF1EA0D0FF1E98C0FF1E90C0FF1E80B0FF1E80
      B0FF0E80B0FF1E6E90FF0E1E1EFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008A2900FF6D2100FF000000004B1600FF00000000000000000000
      0000000000000000000000000000000000000000000018181819191919F10000
      00FF000000FF56565667000000000000000000000000000000000C0C0C0D0C0C
      0CF8000000FF55555566000000000000000000000000808890FFB0E8F0FF90E8
      FFFF80E0FFFF6ED8FFFF6ED0F0FF5EC8F0FF4EC0F0FF3EB8F0FF000080FF2EA8
      E0FF1E98E0FF1E76A0FF1E262EFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009D3F16FF892900FF00000000661E00FF401300FF000000000000
      00000000000000000000000000000000000000000000000000000F0F0F103434
      34DE000000FF000000FF4A4A4A53000000000000000016161617060606FB0000
      00FF5858586C000000000000000000000000000000008090A0FFB0E8F0FFA0E8
      FFFF90E8FFFF80E0FFFF6ED8FFFF6ED0F0FF5EC8F0FF4EC0F0FF000080FF2EA8
      F0FF2EA0E0FF0E80B0FF2E363EFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A44D27FF963307FF000000008C2A00FF621D00FF391100FF0000
      0000000000000000000000000000000000000000000000000000000000000606
      0607484848CC000000FF000000FF3C3C3C4122222223000000FF000000FF5C5C
      5C7600000000000000000000000000000000000000008090A0FFB0F0FFFFB0F0
      FFFFA0E8FFFF90E0FFFF80E0FFFF6ED8FFFF000080FF000080FF000080FF0000
      80FF000080FF0E80B0FF3E3E4EFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A44D27FF973409FF000000009F431BFF902B00FF651E00FF601D
      00FF000000000000000000000000000000000000000000000000000000000000
      000002020203565656B9000000FF000000FF000000FF000000FF606060850000
      000000000000000000000000000000000000000000008098A0FFC0F0FFFFB0F0
      F0FFA0F0FFFFA0E8FFFF90E0FFFF80E0FFFF6ED8FFFF5ED0F0FF000080FF4EB8
      F0FF3EB0F0FF0E88C0FF4E4E5EFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A44D27FF973409FF00000000A85531FFA54F2AFF8D2A00FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000535353BE000000FF000000FF606060A2000000000000
      000000000000000000000000000000000000000000008098A0FFC0F0FFFFB0F0
      FFFFB0F0FFFFA0E8FFFF90E8FFFF90E0FFFF80E0FFFF6ED8FFFF000080FF5EC8
      F0FF4EB8F0FF0E90C0FF4E5E6EFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000AA5936FF9F441CFF00000000AF6240FFAA5936FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000004A4A4A53000000FF000000FF000000FF000000FF161616170000
      0000000000000000000000000000000000000000000090A0A0FFC0F0FFFFB0F0
      FFFFB0F0FFFFB0F0F0FFA0F0FFFF90E8FFFF90E0FFFF80E0FFFF6ED0FFFF5ED0
      F0FF4EC0F0FF1E98D0FF5E6E80FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B46D4EFFAD5E3CFF00000000AF6342FF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000056565667000000FF000000FF565656B960606085000000FF060606FB0C0C
      0C0D000000000000000000000000000000000000000090A0B0FFC0F0FFFFC0F0
      FFFFC0F0FFFFC0F0FFFFB0F0FFFFB0F0FFFFA0E8FFFF90E8FFFF90E0FFFF80D8
      FFFF6ED0FFFF5EC0F0FF5E6E80FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000002C1C157E2B1A127E000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000005E5E
      5E7B000000FF000000FF484848CC02020203000000005C5C5C76000000FF0C0C
      0CF8050505060000000000000000000000000000000090A0B0FF90A0B0FF90A0
      B0FF90A0B0FF90A0B0FF90A0B0FF90A0A0FF9098A0FF8098A0FF8098A0FF8098
      A0FF8098A0FF8098A0FF8098A0FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000616161900000
      00FF000000FF343434DE060606070000000000000000000000005858586C0000
      00FF121212F50000000100000000000000000000000090A8B0FFB0E8F0FFB0F0
      FFFFB0F0F0FF90E0F0FF90A0B0FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000061616190000000FF0000
      00FF191919F10F0F0F1000000000000000000000000000000000000000005555
      5566000000FF000000FF0000000000000000000000000000000090A8B0FF90A8
      B0FF90A8B0FF90A8B0FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005050505D000000FF0000
      00FF181818190000000000000000000000000000000000000000000000000000
      000053535363585858B600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004747474F2525
      2526000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001F00007F800000FF8000
      00FF800000FF800000FF800000FF800000FF800000FF800000FF800000FF8000
      00FF800000FF800000FF1F00007F000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000800000FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000800000FF0000000000000000E3B4AB00C0686000B058
      5000A0505000A0505000A0505000904850009048400090484000804040008038
      4000803840007038400070383000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000006E8890FF6E7680FF5E6E
      6EFF4E5E5EFF3E4E4EFF2E363EFF1E262EFF0E1E1EFF0E0E1EFF0E0E1EFF0E0E
      1EFF0E0E1EFF0E0E1EFF0E0E1EFF0000000000000000800000FF000000008000
      00FF800000FF800000FF800000FF800000FF800000FF800000FF000000000000
      FFFF0000FFFF00000000800000FF0000000000000000D0687000F0909000E080
      8000B048200040302000C0B8B000C0B8B000D0C0C000D0C8C00050505000A040
      3000A0403000A038300070384000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000006E8890FFA0E0F0FF6ED0
      F0FF4EB8E0FF2EB0E0FF2EA8E0FF1EA0D0FF1E98C0FF1E90C0FF1E80B0FF1E80
      B0FF0E80B0FF1E6E90FF0E1E1EFF0000000000000000800000FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000800000FF0000000000000000D0707000FF98A000F088
      8000E0808000705850004040300090787000F0E0E000F0E8E00090807000A040
      3000A0404000A040300080384000000000000000000000000000000000000000
      000000000000000000003B1E12C1000000000000000000000000000000000000
      00000000000000000000000000000000000000000000808890FFB0E8F0FF90E8
      FFFF80E0FFFF6ED8FFFF6ED0F0FF5EC8F0FF4EC0F0FF3EB8F0FF2EA8F0FF2EA8
      E0FF1E98E0FF1E76A0FF1E262EFF0000000000000000800000FF000000008000
      00FF800000FF800000FF800000FF800000FF800000FF800000FF000000000080
      00FF008000FF00000000800000FF0000000000000000D0787000FFA0A000F090
      9000F0888000705850000000000040403000F0D8D000F0E0D00080786000B048
      4000B0484000A040400080404000000000000000000000000000000000000000
      00000000000000000000712200FF32190FC10000000000000000000000000000
      000000000000000000000000000000000000000000008090A0FFB0E8F0FFA0E8
      FFFF90E8FFFF80E0FFFF6ED8FFFF6ED0F0FF5EC8F0FF4EC0F0FF3EB8F0FF2EA8
      F0FF2EA0E0FF0E80B0FF2E363EFF0000000000000000800000FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000800000FF0000000000000000D0788000FFA8B000FFA0
      A000F0909000705850007058500070585000705850007060500080686000C058
      5000B0505000B048400080404000000000000000000000000000000000000000
      000000000000000000007B2500FF631E00FF2D170DC100000000000000000000
      000000000000000000000000000000000000000000008090A0FFB0F0FFFFB0F0
      FFFFA0E8FFFF90E0FFFF80E0FFFF6ED8FFFF6ED0F0FF5EC8F0FF4EC0F0FF3EB0
      F0FF2EA8F0FF0E80B0FF3E3E4EFF0000000000000000800000FF000000008000
      00FF800000FF800000FF800000FF800000FF800000FF800000FF000000000000
      FFFF0000FFFF00000000800000FF0000000000000000E0808000FFB0B000FFB0
      B000FFA0A000F0909000F0888000E0808000E0788000D0707000D0687000C060
      6000C0585000B050500090484000000000000000000000000000000000000000
      000000000000000000008C2A00FF732300FF5F1C00FF2D170DC1000000000000
      000000000000000000000000000000000000000000008098A0FFC0F0FFFFB0F0
      F0FFA0F0FFFFA0E8FFFF90E0FFFF80E0FFFF6ED8FFFF5ED0F0FF5EC8F0FF4EB8
      F0FF3EB0F0FF0E88C0FF4E4E5EFF0000000000000000800000FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000800000FF0000000000000000E0889000FFB8C000FFB8
      B000D0606000C0605000C0585000C0504000B0503000B0483000A0402000A038
      1000C0606000C058500090484000000000000000000000000000000000000000
      000000000000000000009C3E15FF8D2A00FF772400FF661F00FF331A0FC10000
      000000000000000000000000000000000000000000008098A0FFC0F0FFFFB0F0
      FFFFB0F0FFFFA0E8FFFF90E8FFFF90E0FFFF80E0FFFF6ED8FFFF5ED0F0FF5EC8
      F0FF4EB8F0FF0E90C0FF4E5E6EFF0000000000000000800000FF000000008000
      00FF800000FF800000FF800000FF800000FF800000FF800000FF000000000080
      00FF008000FF00000000800000FF0000000000000000E0909000FFC0C000D068
      6000FFFFFF00FFFFFF00FFF8F000F0F0F000F0E8E000F0D8D000E0D0C000E0C8
      C000A0381000C060600090485000000000000000000000000000000000000000
      00000000000000000000AB5B38FFA14720FF922C00FF7E2600FF3C1F12BE0000
      0000000000000000000000000000000000000000000090A0A0FFC0F0FFFFB0F0
      FFFFB0F0FFFFB0F0F0FFA0F0FFFF90E8FFFF90E0FFFF80E0FFFF6ED0FFFF5ED0
      F0FF4EC0F0FF1E98D0FF5E6E80FF0000000000000000800000FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000800000FF0000000000000000E098A000FFC0C000D070
      7000FFFFFF00FFFFFF00FFFFFF00FFF8F000F0F0F000F0E8E000F0D8D000E0D0
      C000A0402000D0686000A0505000000000000000000000000000000000000000
      00000000000000000000B56E4FFFAE603EFFA14821FF4D2818BE000000000000
      0000000000000000000000000000000000000000000090A0B0FFC0F0FFFFC0F0
      FFFFC0F0FFFFC0F0FFFFB0F0FFFFB0F0FFFFA0E8FFFF90E8FFFF90E0FFFF80D8
      FFFF6ED0FFFF5EC0F0FF5E6E80FF0000000000000000800000FF000000008000
      00FF800000FF800000FF800000FF800000FF800000FF800000FF000000000000
      FFFF0000FFFF00000000800000FF0000000000000000F0A0A000FFC0C000E078
      7000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFF8F000F0F0F000F0E8E000F0D8
      D000B0483000D0707000A0505000000000000000000000000000000000000000
      00000000000000000000BA775AFFB36B4BFF593C2EBE00000000000000000000
      0000000000000000000000000000000000000000000090A0B0FF90A0B0FF90A0
      B0FF90A0B0FF90A0B0FF90A0B0FF90A0A0FF9098A0FF8098A0FF8098A0FF8098
      A0FF8098A0FF8098A0FF8098A0FF0000000000000000800000FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000800000FF0000000000000000F0A8A000FFC0C000E080
      8000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFF8F000F0F0F000F0E8
      E000B0503000E0788000A0505000000000000000000000000000000000000000
      00000000000000000000B97659FF61463ABE0000000000000000000000000000
      0000000000000000000000000000000000000000000090A8B0FFB0E8F0FFB0F0
      FFFFB0F0F0FF90E0F0FF90A0B0FF000000000000000000000000000000000000
      00000000000000000000000000000000000000000000800000FF000000008000
      00FF800000FF800000FF800000FF800000FF800000FF800000FF000000000080
      00FF008000FF00000000800000FF0000000000000000F0B0B000FFC0C000F088
      9000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFF8F000F0F0
      F000C050400060303000B0585000000000000000000000000000000000000000
      0000000000000000000061473CBE000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000090A8B0FF90A8
      B0FF90A8B0FF90A8B0FF00000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000800000FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000800000FF0000000000000000F0B0B000FFC0C000FF90
      9000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFF8
      F000C0585000B0586000B0586000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000800000FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000800000FF0000000000000000F0B8B000F0B8B000F0B0
      B000F0B0B000F0A8B000F0A0A000E098A000E0909000E0909000E0889000E080
      8000D0788000D0787000D0707000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001F00007F800000FF8000
      00FF800000FF800000FF800000FF800000FF800000FF800000FF800000FF8000
      00FF800000FF800000FF1F00007F000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF000000FFFFFFFF000000009FFFFFFF0000
      00000FF180010000000007E180010000000083C3800100000000C18780010000
      0000E00F800100000000F01F800100000000FC3F800100000000F81F80010000
      0000F00F800100000000E087800100000000C1C381FF0000000083E3C3FF0000
      000087F3FFFF00000000CFFFFFFF0000FFFF0000FFFF0000FFFF000080010000
      8001000080010000800100008001000080010000800100008001000080010000
      8001000080010000800100008001000080010000800100008001000080010000
      8001000080010000800100008001000081FF000080010000C3FF000080010000
      FFFF000080010000FFFF0000FFFF000000000000000000000000000000000000
      000000000000}
  end
  object TestsPopup: TPopupMenu
    Left = 88
    Top = 209
    object ParseSelected1: TMenuItem
      Action = ParseSelectedTestAction
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object CreateNewTest1: TMenuItem
      Action = CreateNewTestAction
    end
    object CreateNewDirectory1: TMenuItem
      Action = CreateNewDirAction
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Rename1: TMenuItem
      Action = RenameTestAction
    end
  end
  object FilesPopup: TPopupMenu
    Left = 160
    Top = 209
    object Edit1: TMenuItem
      Action = FilesEditAction
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object ParseFocused1: TMenuItem
      Action = FilesParseFocusedAction
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object FilesCheckAllAction1: TMenuItem
      Action = FilesCheckAllAction
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object RemoveAll1: TMenuItem
      Action = FilesRemoveAllAction
    end
  end
  object SynJSONSyn1: TSynJSONSyn
    Left = 128
    Top = 417
  end
  object EditorPopup: TPopupMenu
    Left = 637
    Top = 261
    object GoToLine1: TMenuItem
      Action = GoToLineAction
    end
  end
end
