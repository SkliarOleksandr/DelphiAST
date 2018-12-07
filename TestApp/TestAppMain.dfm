object frmTestAppMain: TfrmTestAppMain
  Left = 0
  Top = 0
  Caption = 'Delphi AST Test App'
  ClientHeight = 616
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    635
    616)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 90
    Height = 13
    Caption = 'Delphi Source Path'
  end
  object Label2: TLabel
    Left = 8
    Top = 207
    Width = 19
    Height = 13
    Caption = 'Unit'
  end
  object Edit1: TEdit
    Left = 8
    Top = 24
    Width = 393
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'C:\Program Files (x86)\Embarcadero\Studio\19.0\source\'
  end
  object Button1: TButton
    Left = 416
    Top = 20
    Width = 115
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Index Sources'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 51
    Width = 619
    Height = 150
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
  object Button2: TButton
    Left = 552
    Top = 20
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Analize'
    TabOrder = 3
    OnClick = Button2Click
  end
  object edUnit: TSynEdit
    Left = 8
    Top = 223
    Width = 617
    Height = 386
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 4
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
      ''
      'type'
      '{ Typed-file and untyped-file record }'
      ''
      
        '{$IF (defined(CPUX86) or defined(CPUX64)) and not defined(ARITH_' +
        'USE_LIBM)}    '
      '{$ENDIF}'
      ''
      'implementation'
      ''
      'end.'
      '')
    FontSmoothing = fsmNone
  end
  object SynPasSyn1: TSynPasSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 392
    Top = 400
  end
end
