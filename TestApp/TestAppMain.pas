unit TestAppMain;

interface

{$I ../Source/AST.Parser.Defines.inc}

uses
  System.SysUtils, System.Variants, System.Classes, System.JSON, System.Generics.Collections, Winapi.Windows,
  Winapi.Messages, Vcl.Graphics, Vcl.ComCtrls, System.Types, Vcl.ExtCtrls, Vcl.CheckLst, System.Actions, Vcl.ActnList,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus, System.ImageList, Vcl.ImgList, System.UITypes,
  SynEdit, SynEditMiscClasses, SynEditSearch, SynEditHighlighter, SynEditCodeFolding, SynHighlighterPas, SynHighlighterJSON,
  VirtualTrees, VirtualTrees.Types, VirtualTrees.Classes,
  AST.Intf,
  AST.Classes,
  AST.Pascal.Project,
  AST.Delphi.Project,
  AST.Delphi.Classes,
  AST.Pascal.Parser,
  AST.Parser.Messages,
  AST.Parser.ProcessStatuses;   // system

type
  TSourceFileInfo = record
    FullPath: string;
    DateModify: TDateTime;
  end;

  TSourcesDict = TDictionary<string, TSourceFileInfo>;

  TTestData = class
  type
    TNodeType = (ntFolder, ntTest);
  private
    FNodeType: TNodeType;
    FCaption: string;
    FFilePath: string;
    FModified: Boolean;
    FTestNode: PVirtualNode;
    procedure SetFilePath(const Value: string);
  public
    constructor Create(const AFileName: string; ANodeType: TNodeType);
    property NodeType: TNodeType read FNodeType;
    property Caption: string read FCaption;
    property FilePath: string read FFilePath write SetFilePath;
    property Modified: Boolean read FModified write FModified;
    property TestNode: PVirtualNode read FTestNode write FTestNode;
  end;

  TfrmTestAppMain = class(TForm)
    SynPasSyn1: TSynPasSyn;
    SrcPageControl: TPageControl;
    tsSource: TTabSheet;
    edUnit: TSynEdit;
    tsAST: TTabSheet;
    BottomPanel: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    AddFilesButton: TButton;
    lbFiles: TCheckListBox;
    ParseFilesButton: TButton;
    chkbShowSysDecls: TCheckBox;
    chkbShowConstValues: TCheckBox;
    chkbShowAnonymous: TCheckBox;
    Splitter2: TSplitter;
    MainPanel: TPanel;
    SynEditSearch1: TSynEditSearch;
    ErrMemo: TSynEdit;
    Panel6: TPanel;
    LeftPageControl: TPageControl;
    tsLogs: TTabSheet;
    tsFiles: TTabSheet;
    tsTestScripts: TTabSheet;
    LogMemo: TSynEdit;
    Splitter3: TSplitter;
    Panel7: TPanel;
    SaveButton: TButton;
    MainPageControl: TPageControl;
    SrcTabSheet: TTabSheet;
    SettingsTabSheet: TTabSheet;
    Label1: TLabel;
    DelphiSrcPathEdit: TEdit;
    TopPanel: TPanel;
    ASTParseRTLButton: TButton;
    ParseSystemCheck: TCheckBox;
    Label2: TLabel;
    cbPlatform: TComboBox;
    ShowWarningsCheck: TCheckBox;
    ParseImplsCheck: TCheckBox;
    StopIfErrorCheck: TCheckBox;
    Label3: TLabel;
    UnitScopeNamesEdit: TEdit;
    SaveSettingsButton: TButton;
    Label4: TLabel;
    UnitSearchPathEdit: TEdit;
    Label5: TLabel;
    CondDefinesEdit: TEdit;
    Panel1: TPanel;
    WriteLogCheck: TCheckBox;
    DelphiPathIncludeSubDirCheck: TCheckBox;
    UnitSearchPathIncludeSubDirCheck: TCheckBox;
    ShowMemLeaksCheck: TCheckBox;
    Button1: TButton;
    ActionList1: TActionList;
    AddFilesAction: TAction;
    RemoveFilesAction: TAction;
    ParseFilesAction: TAction;
    SaveSourceAction: TAction;
    ASTParseButton: TButton;
    ASTParseAction: TAction;
    BreakpointOnErrorCheck: TCheckBox;
    VTTests: TVirtualStringTree;
    Label6: TLabel;
    TestScriptsPathEdit: TEdit;
    ImageList1: TImageList;
    TestsPopup: TPopupMenu;
    CreateNewDirAction: TAction;
    CreateNewDirectory1: TMenuItem;
    CreateNewTestAction: TAction;
    CreateNewTest1: TMenuItem;
    RenameTestAction: TAction;
    N1: TMenuItem;
    Rename1: TMenuItem;
    TestsTopPanel: TPanel;
    RunAllTestsButton: TButton;
    TestsParseAllAction: TAction;
    LoadLSPConfigButton: TButton;
    LoadLSPConfigAction: TAction;
    FilesPopup: TPopupMenu;
    FilesCheckAllAction: TAction;
    FilesCheckAllAction1: TMenuItem;
    N2: TMenuItem;
    FilesParseFocusedAction: TAction;
    ParseFocused1: TMenuItem;
    N3: TMenuItem;
    FilesRemoveAllAction: TAction;
    RemoveAll1: TMenuItem;
    Label7: TLabel;
    ProjectNameEdit: TEdit;
    ASTResultTopPanel: TPanel;
    ASTResultFormatComboBox: TComboBox;
    ASTResultViewComboBox: TComboBox;
    ASTPageControl: TPageControl;
    SynJSONSyn1: TSynJSONSyn;
    SearchEdit: TEdit;
    SearchButton: TButton;
    TestsBottomPanel: TPanel;
    TotalTestCntLabel: TLabel;
    TestRunProgressLabel: TLabel;
    procedure ASTParseRTLButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SearchButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SaveSettingsButtonClick(Sender: TObject);
    procedure StopIfErrorCheckClick(Sender: TObject);
    procedure ShowMemLeaksCheckClick(Sender: TObject);
    procedure AddFilesActionExecute(Sender: TObject);
    procedure RemoveFilesActionExecute(Sender: TObject);
    procedure ParseFilesActionExecute(Sender: TObject);
    procedure ParseFilesActionUpdate(Sender: TObject);
    procedure RemoveFilesActionUpdate(Sender: TObject);
    procedure SaveSourceActionExecute(Sender: TObject);
    procedure SaveSourceActionUpdate(Sender: TObject);
    procedure ASTParseActionExecute(Sender: TObject);
    procedure edUnitSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
    procedure FormDestroy(Sender: TObject);
    procedure VTTestsChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
    procedure VTTestsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure VTTestsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure VTTestsNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure edUnitChange(Sender: TObject);
    procedure VTTestsDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure CreateNewDirActionUpdate(Sender: TObject);
    procedure CreateNewDirActionExecute(Sender: TObject);
    procedure VTTestsClick(Sender: TObject);
    procedure CreateNewTestActionExecute(Sender: TObject);
    procedure RenameTestActionUpdate(Sender: TObject);
    procedure RenameTestActionExecute(Sender: TObject);
    procedure TestsParseAllActionExecute(Sender: TObject);
    procedure LoadLSPConfigActionExecute(Sender: TObject);
    procedure FilesCheckAllActionUpdate(Sender: TObject);
    procedure FilesCheckAllActionExecute(Sender: TObject);
    procedure FilesParseFocusedActionUpdate(Sender: TObject);
    procedure FilesParseFocusedActionExecute(Sender: TObject);
    procedure FilesRemoveAllActionExecute(Sender: TObject);
    procedure ASTResultFormatComboBoxChange(Sender: TObject);
  private
    { Private declarations }
    //fPKG: INPPackage;
    fSettings: IASTProjectSettings;
    FLockSaveSettings: Boolean;
    FAllTests: TDictionary<{Unit FileName} string, {Test Data} TTestData>;
    FSelectedTest: TTestData;
    FLastProject: IASTDelphiProject;
    procedure OnProgress(const Module: IASTModule; Status: TASTProcessStatusClass);
    procedure ShowASTResultAsCode(const Project: IASTDelphiProject; ASynEdit: TSynEdit); overload;
    procedure ShowASTResultAsCode(const AModule: TASTModule; ASynEdit: TSynEdit); overload;
    procedure ShowASTResultAsJSON(const Project: IASTDelphiProject; ASynEdit: TSynEdit); overload;
    procedure ShowASTResultAsJSON(const AModule: TASTModule; ASynEdit: TSynEdit); overload;
    procedure ShowASTResults(const AProject: IASTDelphiProject);
    function ParseProject(const Project: IASTDelphiProject; AClearOutput: Boolean = True): TCompilerResult;
    procedure CompilerMessagesToStrings(const Project: IASTDelphiProject);
    procedure SetDefines(const APrj: IASTDelphiProject);
    procedure SaveSettings;
    procedure LoadSettings;
    procedure LoadTestScripts(const ALastTestName: string);
    procedure LoadLSPConfigDccCmdLine(const ACMDLine: string);
    procedure LoadLSPConfigFiles(const AJSONArray: TJSONArray);
    function CreateProject(AParseSystemUnit: Boolean): IASTDelphiProject;
    function FocusedTestData: TTestData;
    function GetTestSriptsPath: string;
    procedure LoadTests(ARootNode: PVirtualNode; const ARootPath: string);
    procedure SelectTest(ATestData: TTestData);
  public
    { Public declarations }
    procedure IndexSources(const RootPath: string; Dict: TSourcesDict);
  end;

var
  frmTestAppMain: TfrmTestAppMain;

implementation

uses
  System.IOUtils,
  System.TypInfo,
  System.Rtti,
  System.StrUtils,
  System.IniFiles,
  System.NetEncoding,
  AST.Delphi.System,
  AST.Delphi.Parser,
  AST.Delphi.Declarations,
  AST.Writer,
  AST.Targets,
  AST.Delphi.DataTypes,
  AST.Parser.Errors,
  AST.Parser.Utils,
  AST.Parser.Log,
  AST.Utils.CmdLineParser;

{$R *.dfm}

const
  SDefTestScriptName = 'DefaultTest';

type
  TASTTabSheet = class(TTabSheet)
    SynEdit: TSynEdit;
  end;

procedure TfrmTestAppMain.CompilerMessagesToStrings(const Project: IASTDelphiProject);
var
  I: Integer;
  Msg: TCompilerMessage;
begin
  ErrMemo.Lines.Add('===================================================================');
  for i := 0 to Project.Messages.Count - 1 do
  begin
    Msg := Project.Messages[i];
    if (Msg.MessageType >= cmtError) or ShowWarningsCheck.Checked then
    begin

      ErrMemo.Lines.AddStrings(Msg.AsString.Split([sLineBreak]));
      if Msg.MessageType >= cmtError then
      begin
        if Msg.UnitName = FSelectedTest.Caption then
        begin
          edUnit.CaretX := Msg.Col;
          edUnit.CaretY := Msg.Row;
          if edUnit.CanFocus then
            edUnit.SetFocus;
        end;
      end;
    end;
  end;
end;

procedure ASTToTreeView2(ASTUnit: TASTDelphiUnit; TreeView: TTreeView);
var
  WR: TASTWriter<TTreeView, TTreeNode>;
begin
  TreeView.Items.Clear;
  WR := TASTWriter<TTreeView, TTreeNode>.Create(TreeView, ASTUnit,
    function (const Container: TTreeView; const RootNode: TTreeNode; const NodeText: string): TTreeNode
    begin
      Result := Container.Items.AddChild(RootNode, NodeText);
    end,
    procedure (const Node: TTreeNode; const ASTItem: TASTItem)
    begin
      Node.Text := ASTItem.DisplayName;
    end);
  try
    WR.Write(nil);
  finally
    WR.Free;
  end;
  TreeView.FullExpand;
end;

const ExcludePath = 'C:\Program Files (x86)\Embarcadero\Studio\19.0\source\DUnit\examples\';

procedure TfrmTestAppMain.IndexSources(const RootPath: string; Dict: TSourcesDict);
var
  Files: TStringDynArray;
  i: Integer;
  FileName: string;
  FilePath: string;
  FileInfo: TSourceFileInfo;
begin
  Files := TDirectory.GetFiles(RootPath, '*.inc', TSearchOption.soAllDirectories);
  for i := 0 to Length(Files) -1 do
  begin
    FilePath := ExtractFilePath(Files[i]);
    if Pos(ExcludePath, FilePath) >= Low(string) then
      Continue;
    FileInfo.FullPath := Files[i];
    FileName := ExtractFileName(FileInfo.FullPath);
    try
      Dict.Add(FileName, FileInfo);
    except
      ErrMemo.Lines.Add(FileInfo.FullPath);
      ErrMemo.Lines.Add(Dict.Items[FileName].FullPath);
    end;
  end;
end;

function GetDeclName(const Decl: TASTDeclaration): string;
begin
  try
    if Decl.Name <> '' then
      Result := Decl.DisplayName
    else
      Result := '[Anonymous]' + Decl.DisplayName;

    var CastedDecl := (Decl as TIDDeclaration);
    case CastedDecl.ItemType of
      itVar, itConst: Result := Result + ' : '  + CastedDecl.DataType.DisplayName;
      itType: Result := Result + ' ['  + GetDataTypeName(TIDType(CastedDecl).DataTypeID) + ']';
    end;
  except
     on E: Exception do
       Result := E.Message;
  end;
end;

procedure TfrmTestAppMain.AddFilesActionExecute(Sender: TObject);
begin
  var LDlg := TFileOpenDialog.Create(Self);
  try
    LDlg.Options := LDlg.Options + [fdoAllowMultiSelect];
    LDlg.DefaultExtension := '.pas';
    LDlg.DefaultFolder := UnitSearchPathEdit.Text;
    if LDlg.Execute then
    begin
      lbFiles.Items.BeginUpdate;
      try
        for var LFileName in LDlg.Files do
          lbFiles.AddItem(ExtractRelativePath(UnitSearchPathEdit.Text, LFileName), nil);
        lbFiles.CheckAll(cbChecked);
      finally
        lbFiles.Items.EndUpdate;
      end;
      SaveSettings;
    end;
  finally
    LDlg.Free;
  end;
end;

procedure TfrmTestAppMain.ASTParseActionExecute(Sender: TObject);
var
  UN: TASTDelphiUnit;
  Prj: IASTDelphiProject;
begin
  TASTParserLog.Instance.ResetNestedLevel;

  Prj := CreateProject({AParseSystemUnit:} ParseSystemCheck.Checked);

  UN := TASTDelphiUnit.Create(Prj, 'test', edUnit.Text);
  Prj.AddUnit(UN, nil);

  ParseProject(Prj);
end;

procedure TfrmTestAppMain.ASTParseRTLButtonClick(Sender: TObject);

  procedure AddDelphiUnits(var AUsesList: string; const APath: string);
  begin
    var LRtlSources := GetDirectoryFiles(DelphiSrcPathEdit.Text + APath, '*.pas');
    for var LPath in LRtlSources do
    begin
      var LUnitName := StringReplace(ExtractFileName(LPath), '.pas', '', [rfReplaceAll]);
      //todo: implement parsing .dpk files and use BuildWinRTL.dpk here
      if LUnitName = 'System.Internal.MachExceptions' then
        continue;
      AUsesList := AddStringSegment(AUsesList, LUnitName, ',')
    end;
  end;

var
  UN: TASTDelphiUnit;
begin
  TASTParserLog.Instance.ResetNestedLevel;

  FLastProject := CreateProject({AParseSystemUnit:} True);

  var LUsesUntis := '';
  AddDelphiUnits({var} LUsesUntis, 'rtl\sys');

  var RTLUsesSourceText :=
  'unit RTLParseTest; '#10#13 +
  'interface'#10#13 +
  'uses'#10#13 +
   LUsesUntis + ';'#10#13 +
  'implementation'#10#13 +
  'end.';

  UN := TASTDelphiUnit.Create(FLastProject, 'RTLParseTest', RTLUsesSourceText);
  FLastProject.AddUnit(UN, nil);

  ParseProject(FLastProject);
end;

procedure TfrmTestAppMain.ASTResultFormatComboBoxChange(Sender: TObject);
begin
  if Assigned(FLastProject) then
    ShowASTResults(FLastProject);
end;

procedure TfrmTestAppMain.OnProgress(const Module: IASTModule; Status: TASTProcessStatusClass);
begin
  //if Status = TASTStatusParseSuccess then
    ErrMemo.Lines.Add(Module.Name + ' : ' + Status.Name);
end;

const
  SGeneral = 'GENERAL';

procedure TfrmTestAppMain.SaveSettings;
begin
  if not FLockSaveSettings then
  begin
    var LINI := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
    try
      LINI.WriteBool(SGeneral, 'STOP_IF_ERROR', StopIfErrorCheck.Checked);
      LINI.WriteBool(SGeneral, 'PARSE_SYSTEM', ParseSystemCheck.Checked);
      LINI.WriteBool(SGeneral, 'PARSE_IMPLS', ParseImplsCheck.Checked);
      LINI.WriteBool(SGeneral, 'WRITE_LOG', WriteLogCheck.Checked);
      LINI.WriteBool(SGeneral, 'SHOW_WARNINGS', ShowWarningsCheck.Checked);
      LINI.WriteBool(SGeneral, 'SHOW_MEMLEAKS', ShowMemLeaksCheck.Checked);
      LINI.WriteBool(SGeneral, 'BREAKPOINT_ON_ERROR', BreakpointOnErrorCheck.Checked);
      LINI.WriteInteger(SGeneral, 'LEFT_ACTIVE_TAB', LeftPageControl.ActivePageIndex);

      LINI.WriteString(SGeneral, 'PLATFORM', cbPlatform.Text);
      LINI.WriteString(SGeneral, 'DELPHI_SRC_PATH', DelphiSrcPathEdit.Text);
        LINI.WriteBool(SGeneral, 'DELPHI_SRC_PATH_INCLUDE_SUBDIRS', DelphiPathIncludeSubDirCheck.Checked);

      LINI.WriteString(SGeneral, 'PROJECT_NAME', ProjectNameEdit.Text);
      LINI.WriteString(SGeneral, 'UNIT_SCOPE_NAMES', UnitScopeNamesEdit.Text);
      LINI.WriteString(SGeneral, 'UNIT_SEARCH_PATH', UnitSearchPathEdit.Text);
        LINI.WriteBool(SGeneral, 'UNIT_SEARCH_PATH_INCLUDE_SUBDIRS', UnitSearchPathIncludeSubDirCheck.Checked);
      LINI.WriteString(SGeneral, 'COND_DEFINES', CondDefinesEdit.Text);
      LINI.WriteString(SGeneral, 'TEST_SCRIPTS_PATH', TestScriptsPathEdit.Text);
      LINI.WriteString(SGeneral, 'CUSTOM_UNITS', lbFiles.Items.CommaText);
    finally
      LINI.Free;
    end;
  end;
end;

procedure TfrmTestAppMain.SaveSettingsButtonClick(Sender: TObject);
begin
  SaveSettings;
end;

procedure TfrmTestAppMain.SaveSourceActionExecute(Sender: TObject);
begin
  edUnit.Lines.SaveToFile(FSelectedTest.FilePath);
  edUnit.Modified := False;
  FSelectedTest.Modified := False;
  VTTests.Invalidate;
end;

procedure TfrmTestAppMain.SaveSourceActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(FSelectedTest) and edUnit.Modified;
end;

function FileURLDecode(const AFileURL: string): string;
begin
  Result := TURLEncoding.URL.Decode(AFileURL).
              Replace('file:///', '', [rfIgnoreCase]).
              Replace('/', '\', [rfReplaceAll]);
end;

procedure TfrmTestAppMain.LoadLSPConfigDccCmdLine(const ACMDLine: string);
begin
  var LOptions := TDcc32CMDLineParser.Parse(ACMDLine);
  UnitSearchPathEdit.Text := LOptions['-I'].Value;
  UnitSearchPathIncludeSubDirCheck.Checked := False;
  CondDefinesEdit.Text := LOptions['-D'].Value;
  UnitScopeNamesEdit.Text := LOptions['-NS'].Value;
end;

procedure TfrmTestAppMain.LoadLSPConfigFiles(const AJSONArray: TJSONArray);
begin
  lbFiles.Clear;
  for var LObject in AJSONArray do
  begin
    var LFileField := LObject.FindValue('file');
    if Assigned(LFileField) then
    begin
      var LFileName := FileURLDecode(LFileField.Value);
      if FileExists(LFileName) then
        lbFiles.AddItem(LFileName, nil);
    end;
  end;
  lbFiles.CheckAll(cbChecked);
end;

procedure TfrmTestAppMain.LoadLSPConfigActionExecute(Sender: TObject);
begin
  var LDlg := TOpenDialog.Create(Self);
  try
    LDlg.Title := 'Select a Delphi LSP Config file';
    LDlg.Filter := 'Delphi LSP Config|*.delphilsp.json';
    if LDlg.Execute then
    begin
      var LJson := TJSONValue.ParseJSONValue(TFile.ReadAllText(LDlg.FileName));
      try
        var Lproject :=  LJson.FindValue('settings.project');
        if Assigned(Lproject) then
          ProjectNameEdit.Text := FileURLDecode(Lproject.Value);

        var LdccOptions := LJson.FindValue('settings.dccOptions');
        if Assigned(LdccOptions) then
          LoadLSPConfigDccCmdLine(LdccOptions.Value);

        var LprojectFiles := LJson.FindValue('settings.projectFiles');
        if Assigned(LprojectFiles) and (LprojectFiles is TJSONArray) then
          LoadLSPConfigFiles(TJSONArray(LprojectFiles));
      finally
        LJson.Free;
      end;
    end;
  finally
    LDlg.Free;
  end;
end;

procedure TfrmTestAppMain.LoadSettings;
begin
  // TMemIniFile supports longer string values than TIniFile
  var LINI := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    // to avoid SaveSettings auto-triggering
    FLockSaveSettings := True;

    StopIfErrorCheck.Checked := LINI.ReadBool(SGeneral, 'STOP_IF_ERROR', True);
    ParseSystemCheck.Checked := LINI.ReadBool(SGeneral, 'PARSE_SYSTEM', False);
    ParseImplsCheck.Checked := LINI.ReadBool(SGeneral, 'PARSE_IMPLS', True);
    WriteLogCheck.Checked := LINI.ReadBool(SGeneral, 'WRITE_LOG', True);
    ShowWarningsCheck.Checked := LINI.ReadBool(SGeneral, 'SHOW_WARNINGS', False);
    ShowMemLeaksCheck.Checked := LINI.ReadBool(SGeneral, 'SHOW_MEMLEAKS', False);
    BreakpointOnErrorCheck.Checked := LINI.ReadBool(SGeneral, 'BREAKPOINT_ON_ERROR', False);
    LeftPageControl.ActivePageIndex := LINI.ReadInteger(SGeneral, 'LEFT_ACTIVE_TAB', 0);

    cbPlatform.Text := LINI.ReadString(SGeneral, 'PLATFORM', 'WIN32');
    ProjectNameEdit.Text := LINI.ReadString(SGeneral, 'PROJECT_NAME', ProjectNameEdit.Text);
    DelphiSrcPathEdit.Text := LINI.ReadString(SGeneral, 'DELPHI_SRC_PATH', DelphiSrcPathEdit.Text);
    DelphiPathIncludeSubDirCheck.Checked := LINI.ReadBool(SGeneral, 'DELPHI_SRC_PATH_INCLUDE_SUBDIRS', True);
    UnitScopeNamesEdit.Text := LINI.ReadString(SGeneral, 'UNIT_SCOPE_NAMES', UnitScopeNamesEdit.Text);
    UnitSearchPathEdit.Text := LINI.ReadString(SGeneral, 'UNIT_SEARCH_PATH', UnitSearchPathEdit.Text);
    UnitSearchPathIncludeSubDirCheck.Checked := LINI.ReadBool(SGeneral, 'UNIT_SEARCH_PATH_INCLUDE_SUBDIRS', True);
    CondDefinesEdit.Text := LINI.ReadString(SGeneral, 'COND_DEFINES', CondDefinesEdit.Text);
    TestScriptsPathEdit.Text := LINI.ReadString(SGeneral, 'TEST_SCRIPTS_PATH', TestScriptsPathEdit.Text);
    lbFiles.Items.CommaText := LINI.ReadString(SGeneral, 'CUSTOM_UNITS', '');
    lbFiles.CheckAll(TCheckBoxState.cbChecked);

    var LUnitName := LINI.ReadString(SGeneral, 'LAST_TEST_UNIT', SDefTestScriptName);
    LoadTestScripts(LUnitName);
  finally
    FLockSaveSettings := False;
    LINI.Free;
  end;
end;

procedure TfrmTestAppMain.LoadTests(ARootNode: PVirtualNode; const ARootPath: string);
begin
  var LAllFiles := TDirectory.GetFileSystemEntries(ARootPath);
  for var LFileName in LAllFiles do
  begin
    if DirectoryExists(LFileName) then
    begin
      var LNode := VTTests.AddChild(ARootNode, TTestData.Create(LFileName, ntFolder));
      LoadTests(LNode, LFileName);
    end else begin
      if ExtractFileExt(LFileName) = '.pas' then
      begin
        var LTestData := TTestData.Create(LFileName, ntTest);
        LTestData.TestNode := VTTests.AddChild(ARootNode, LTestData);
        FAllTests.Add(LTestData.Caption, LTestData);
      end;
    end;
  end;
end;

procedure TfrmTestAppMain.LoadTestScripts(const ALastTestName: string);
begin
  var LRootPath := GetTestSriptsPath;

  if DirectoryExists(LRootPath) then
    LoadTests(nil, LRootPath);

  if ALastTestName <> '' then
  begin
    var LTestData: TTestData;
    if FAllTests.TryGetValue(ALastTestName, {out} LTestData) then
    begin
      SelectTest(LTestData);

      var LNodeToSelect := LTestData.TestNode;
      if Assigned(LNodeToSelect) then
      begin
        var LParentNode := LNodeToSelect.Parent;
        while Assigned(LParentNode) and (LParentNode <> VTTests.RootNode) do
        begin
          VTTests.Expanded[LParentNode] := True;
          LParentNode := LParentNode.Parent;
        end;
        VTTests.FocusedNode := LNodeToSelect;
        VTTests.Selected[LNodeToSelect] := True;
        VTTests.ScrollIntoView(LNodeToSelect, {Center:} True);
      end;
    end;
  end;
  TotalTestCntLabel.Caption := 'Total Tests: ' + FAllTests.Count.ToString;
end;

procedure TfrmTestAppMain.SelectTest(ATestData: TTestData);
begin
  if Assigned(FSelectedTest) and FSelectedTest.Modified then
    if MessageDlg('The current test script has been modified, cancel changed?',
                   mtConfirmation, [mbYes, mbNo], 0) = mrYes  then
      FSelectedTest.Modified := False
    else
      Exit;

  FSelectedTest := ATestData;
  edUnit.Lines.LoadFromFile(ATestData.FilePath);
  edUnit.Modified := False;
  ATestData.Modified := False;

  var LINI := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    LINI.WriteString(SGeneral, 'LAST_TEST_UNIT', ATestData.Caption);
  finally
    LINI.Free;
  end;
end;

procedure TfrmTestAppMain.SetDefines(const APrj: IASTDelphiProject);
begin
  APrj.Defines.Add('UNICODE');
  case cbPlatform.ItemIndex of
    // Win32
    0: begin
      APrj.Target := TWINX86_Target;
      APrj.Defines.Add('CPUX86');
      APrj.Defines.Add('CPU386');
      APrj.Defines.Add('CPU32BITS');
      APrj.Defines.Add('WIN32');
      APrj.Defines.Add('MSWINDOWS');
      APrj.Defines.Add('ASSEMBLER');
    end;
    // Win64
    1: begin
      APrj.Target := TWINX64_Target;
      APrj.Defines.Add('CPUX64');
      APrj.Defines.Add('CPU64BITS');
      APrj.Defines.Add('WIN64');
      APrj.Defines.Add('MSWINDOWS');
      APrj.Defines.Add('ASSEMBLER');
    end;
  end;
  // add custom defines
  for var LDefine in string(CondDefinesEdit.Text).Split([';']) do
    APrj.Defines.Add(LDefine);

  APrj.Defines.Add('AST_PARSING');
end;

procedure TfrmTestAppMain.CreateNewDirActionExecute(Sender: TObject);
begin
  var LDirName: string;
  if InputQuery('Create New Directory', 'Directory Name', {var} LDirName) then
  begin
    var LParentDir := TestScriptsPathEdit.Text;
    if FocusedTestData <> nil then
      LParentDir := FocusedTestData.FilePath;

    LDirName := IncludeTrailingPathDelimiter(LParentDir) + LDirName;
    if CreateDir(LDirName) then
    begin
      VTTests.FocusedNode := VTTests.AddChild(VTTests.FocusedNode, TTestData.Create(LDirName, ntFolder));
      VTTests.Selected[VTTests.FocusedNode] := True;
    end else
      RaiseLastOSError;
  end;
end;

procedure TfrmTestAppMain.CreateNewDirActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not Assigned(VTTests.FocusedNode) or
    (FocusedTestData.NodeType = ntFolder);
end;

procedure TfrmTestAppMain.CreateNewTestActionExecute(Sender: TObject);
const
  CUnitTemplateFmt = '''
    unit %s;

    interface

    implementation

    end.
    ''';
begin
  var LTestName: string;
  if InputQuery('Create New Test', 'Test Name', {var} LTestName) then
  begin
    var LTestSource := Format(CUnitTemplateFmt, [LTestName]);

    var LParentDir := TestScriptsPathEdit.Text;
    if FocusedTestData <> nil then
      LParentDir := FocusedTestData.FilePath;

    LTestName := IncludeTrailingPathDelimiter(LParentDir) + LTestName + '.pas';
    if not FileExists(LTestName) then
    begin
      TFile.WriteAllText(LTestName, LTestSource);
      var LNewTestData := TTestData.Create(LTestName, ntTest);
      VTTests.FocusedNode := VTTests.AddChild(VTTests.FocusedNode, LNewTestData);
      VTTests.Selected[VTTests.FocusedNode] := True;
      SelectTest(LNewTestData);
    end else
      RaiseLastOSError;
  end;
end;

procedure TfrmTestAppMain.RenameTestActionExecute(Sender: TObject);
begin
  var LTestData := FocusedTestData;
  var LOldName := LTestData.Caption;
  var LNewName := LTestData.Caption;
  var LOldFilePath := LTestData.FilePath;
  if InputQuery('Rename', 'Name', {var} LNewName) then
  begin
    var LNewFilePath := '';
    if LTestData.NodeType = ntFolder then
    begin
      var LPathArray := ExcludeTrailingPathDelimiter(LOldFilePath).Split([PathDelim]);
      for var LIndex := 0 to Length(LPathArray) - 2 do
        LNewFilePath := AddStringSegment(LNewFilePath, LPathArray[LIndex], PathDelim);

      LNewFilePath := AddStringSegment(LNewFilePath, LNewName, PathDelim);
    end else
      LNewFilePath := LOldFilePath.Replace(LOldName + '.pas', LNewName + '.pas', []);

    if RenameFile(LOldFilePath, LNewFilePath) then
    begin
      LTestData.FilePath := LNewFilePath;
      if LTestData.NodeType = ntTest then
        SelectTest(LTestData);
    end else
      RaiseLastOSError;
  end;
end;

function TfrmTestAppMain.CreateProject(AParseSystemUnit: Boolean): IASTDelphiProject;
begin
  if FileExists(ProjectNameEdit.Text) then
    Result := TASTDelphiProject.CreateExisting(ProjectNameEdit.Text)
  else
    Result := TASTDelphiProject.Create(ProjectNameEdit.Text);

  Result.ParseSystemUnit := AParseSystemUnit;
  Result.AddUnitSearchPath(DelphiSrcPathEdit.Text, DelphiPathIncludeSubDirCheck.Checked);
  Result.AddUnitSearchPath(UnitSearchPathEdit.Text, UnitSearchPathIncludeSubDirCheck.Checked);
  Result.AddUnitSearchPath(ExtractFilePath(Application.ExeName), {AIncludeSubDirs:} True);
  Result.UnitScopeNames := UnitScopeNamesEdit.Text;

  SetDefines(Result);

  Result.OnProgress := OnProgress;
  Result.StopCompileIfError := StopIfErrorCheck.Checked;
  Result.CompileAll := ParseImplsCheck.Checked;

  Result.OnConsoleWrite := procedure (const Module: IASTModule; Line: Integer; const Msg: string)
                        begin
                          ErrMemo.Lines.Add(format('#console: [%s: %d]: %s', [Module.Name, Line, Msg]));
                        end;
end;

procedure TfrmTestAppMain.edUnitChange(Sender: TObject);
begin
  if Assigned(FSelectedTest) then
  begin
    FSelectedTest.Modified := edUnit.Modified;
    VTTests.Invalidate;
  end;
end;

procedure TfrmTestAppMain.edUnitSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
begin
  if TSynEdit(Sender).CaretY = Line then
  begin
    Special := True;
    BG := TColorRec.Antiquewhite;
  end;
end;

procedure TfrmTestAppMain.ShowASTResultAsCode(const Project: IASTDelphiProject; ASynEdit: TSynEdit);
begin
  ASynEdit.BeginUpdate;
  try
    var LBuilder := TStringBuilder.Create;
    try
      Project.EnumDeclarations(
        procedure(const Module: TASTModule; const Decl: TASTDeclaration)
        begin
          if not chkbShowAnonymous.Checked and (Decl.ID.Name = '') then
            Exit;

          if not chkbShowSysDecls.Checked and (Module.Name = 'system') then
            Exit;

          try
//            LBuilder.Append('//scope: ' + TIDDeclaration(Decl).Scope.Name);
//            LBuilder.Append(sLineBreak);
//            LBuilder.Append('//class: ' + Decl.ClassName);
//            LBuilder.Append(sLineBreak);
            Decl.Decl2Str(LBuilder, {ANestedLevel:} 0, {AAppendName:} True);
            LBuilder.Append(sLineBreak);
          except
            on E: Exception do
              LBuilder.Append(E.Message);
          end;
        end, {AUnitScope} scopeBoth);
    finally
      ASynEdit.Text := LBuilder.ToString;
      LBuilder.Free;
    end;
  finally
    ASynEdit.EndUpdate;
  end;
end;

procedure TfrmTestAppMain.ShowASTResultAsCode(const AModule: TASTModule; ASynEdit: TSynEdit);
begin
  ASynEdit.BeginUpdate;
  try
    var LBuilder := TStringBuilder.Create;
    try
      AModule.EnumDeclarations(
        procedure(const Module: TASTModule; const Decl: TASTDeclaration)
        begin
          if not chkbShowAnonymous.Checked and (Decl.ID.Name = '') then
            Exit;

          if not chkbShowSysDecls.Checked and (Module.Name = 'system') then
            Exit;

          try
            Decl.Decl2Str(LBuilder, {ANestedLevel:} 0, {AAppendName:} True);
            LBuilder.Append(sLineBreak);
          except
            on E: Exception do
              LBuilder.Append(E.Message);
          end;
        end, {AUnitScope} scopeBoth);
    finally
      ASynEdit.Text := LBuilder.ToString;
      LBuilder.Free;
    end;
  finally
    ASynEdit.EndUpdate;
  end;
end;

procedure TfrmTestAppMain.ShowASTResultAsJSON(const AModule: TASTModule; ASynEdit: TSynEdit);
begin
  //todo:
end;

procedure TfrmTestAppMain.ShowASTResultAsJSON(const Project: IASTDelphiProject; ASynEdit: TSynEdit);
begin
  //todo:
end;

procedure TfrmTestAppMain.ShowASTResults(const AProject: IASTDelphiProject);

  function CreateTab(const ATitle: string): TSynEdit;
  begin
    var LTab := TASTTabSheet.Create(Self);
    LTab.PageControl := ASTPageControl;
    LTab.Caption := ATitle;
    Result := TSynEdit.Create(LTab);
    Result.Parent := LTab;
    Result.Align := alClient;
    Result.Font.Assign(edUnit.Font);
    Result.SearchEngine := SynEditSearch1;
    Result.Options := Result.Options + [eoTabsToSpaces];
    Result.Gutter.ShowLineNumbers := True;
    LTab.SynEdit := Result;
    case ASTResultFormatComboBox.ItemIndex of
      0: Result.Highlighter := SynPasSyn1;
      1: Result.Highlighter := SynJSONSyn1;
    end;
  end;

begin
  // clear previous results
  for var LIndex := ASTPageControl.PageCount - 1 downto 0 do
     ASTPageControl.Pages[LIndex].Free;

  case ASTResultViewComboBox.ItemIndex of
    // single file
    0: begin
         var LTab := CreateTab(AProject.Name);
         case ASTResultFormatComboBox.ItemIndex of
           0: ShowASTResultAsCode(AProject, LTab);
           1: ShowASTResultAsJSON(AProject, LTab);
         end;
       end;
    // separate files
    1: begin
         for var LIndex := 0 to AProject.AllUnitsCount - 1 do
         begin
           var LUnit := AProject.AllUnits[LIndex];
           var LTab := CreateTab(LUnit.Name);
           case ASTResultFormatComboBox.ItemIndex of
             0: ShowASTResultAsCode(LUnit, LTab);
             1: ShowASTResultAsJSON(LUnit, LTab);
           end;
         end;
       end;
  end;

  FLastProject := AProject;
end;

procedure TfrmTestAppMain.ShowMemLeaksCheckClick(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := ShowMemLeaksCheck.Checked;
end;

procedure TfrmTestAppMain.StopIfErrorCheckClick(Sender: TObject);
begin
  SaveSettings;
end;

procedure TfrmTestAppMain.TestsParseAllActionExecute(Sender: TObject);
begin
  ErrMemo.Clear;
  LogMemo.Clear;
  var LRunCount := 0;
  var LFailCount := 0;
  var LProject := CreateProject({AParseSystemUnit:} ParseSystemCheck.Checked);
  for var LTestData in FAllTests.Values do
  begin
    TASTParserLog.Instance.ResetNestedLevel;
    var LUnit := TASTDelphiUnit.Create(LProject, LTestData.FilePath);
    LProject.AddUnit(LUnit, nil);
    if ParseProject(LProject, {AClearOutput:} False) <> CompileSuccess then
      Inc(LFailCount);

    // do not clear implict units (RTL) between tests
    LProject.Clear({AClearImplicitUnits:} False);
    Inc(LRunCount);
    TestRunProgressLabel.Caption := Format('Run Tests: %d from %d (Fail: %d)',
                                           [LRunCount, FAllTests.Count, LFailCount]);
    Application.ProcessMessages;
  end;
  LProject.Clear({AClearImplicitUnits:} True);
end;

procedure SetNodeChecked(Sender: TBaseVirtualTree; Node: PVirtualNode; CheckState: TCheckState);
var
  CNode: PVirtualNode;
begin
  CNode := Sender.GetFirstChild(Node);
  while Assigned(CNode) do
  begin
    CNode.CheckState := CheckState;
    SetNodeChecked(Sender, CNode, CheckState);
    CNode := Sender.GetNextSibling(CNode);
  end;
end;

procedure TfrmTestAppMain.VTTestsChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState;
  var Allowed: Boolean);
begin
  Allowed := True;
  SetNodeChecked(Sender, Node, NewState);
  Sender.Repaint;
end;

procedure TfrmTestAppMain.VTTestsClick(Sender: TObject);
begin
  var LNode := VTTests.GetNodeAt(VTTests.ScreenToClient(Mouse.CursorPos));

  if not Assigned(LNode) then  begin
    VTTests.FocusedNode := nil;
    VTTests.ClearSelection;
  end;
end;

procedure TfrmTestAppMain.VTTestsDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  var LTestData := Sender.GetNodeData<TTestData>(Node);
  if LTestData = FSelectedTest then
    TargetCanvas.Font.Style := [fsBold];
end;

procedure TfrmTestAppMain.VTTestsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  var LTestData := Sender.GetNodeData<TTestData>(Node);
  if (Kind in [ikNormal, ikSelected]) then
  begin
    case LTestData.NodeType of
      ntFolder: ImageIndex := 0;
      ntTest: ImageIndex := 1;
    end
  end;
end;

procedure TfrmTestAppMain.VTTestsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  var LTestData := Sender.GetNodeData<TTestData>(Node);
  CellText := LTestData.Caption;
  if LTestData.Modified then
    CellText := CellText + '*';
end;

procedure TfrmTestAppMain.VTTestsNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
begin
  var LTestData := Sender.GetNodeData<TTestData>(HitInfo.HitNode);
  if LTestData.NodeType = ntTest then
    SelectTest(LTestData);
end;

function TfrmTestAppMain.ParseProject(const Project: IASTDelphiProject; AClearOutput: Boolean): TCompilerResult;
begin
  if AClearOutput then
  begin
    ErrMemo.Clear;
    LogMemo.Clear;
  end;

  BreakpointOnError := BreakpointOnErrorCheck.Checked;

  Screen.Cursor := crHourGlass;
  var Msg := TStringList.Create;
  try
    var LStartedAt := Now;
    Result := Project.Compile;
    if Result = CompileSuccess then
      Msg.Add('compile success')
    else
      Msg.Add('compile fail');

    Msg.Add(format('total units parsed: %d (interface only: %d)',
      [Project.TotalUnitsParsed, Project.TotalUnitsIntfOnlyParsed]));
    Msg.Add(format('total lines parsed: %d in %s', [Project.TotalLinesParsed,
                                                    FormatDateTime('nn:ss.zzz', Now - LStartedAt)]));

    ShowASTResults(Project);
    CompilerMessagesToStrings(Project);

    ErrMemo.Lines.AddStrings(Msg);
    ErrMemo.CaretY := ErrMemo.Lines.Count;
    ErrMemo.CaretX := 1;
  finally
    Screen.Cursor := crDefault;
    Msg.Free;
  end;
end;

procedure TfrmTestAppMain.ParseFilesActionExecute(Sender: TObject);
var
  Prj: IASTDelphiProject;
begin
  SaveSettings;

  Prj := CreateProject({AParseSystemUnit:} True);

  // add selected files to the project
  for var LIndex := 0 to lbFiles.Count - 1 do
    if lbFiles.Checked[LIndex] then
      Prj.AddUnit(lbFiles.Items[LIndex]);

  ParseProject(Prj);

  Prj.Clear({AClearImplicitUnits:} True);
end;

procedure TfrmTestAppMain.ParseFilesActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := lbFiles.Count > 0;
end;

procedure TfrmTestAppMain.RemoveFilesActionExecute(Sender: TObject);
begin
  lbFiles.DeleteSelected;
  SaveSettings;
end;

procedure TfrmTestAppMain.RemoveFilesActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := lbFiles.SelCount > 0;
end;

procedure TfrmTestAppMain.RenameTestActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := FocusedTestData <> nil;
end;

procedure TfrmTestAppMain.SearchButtonClick(Sender: TObject);
begin
  if (ASTPageControl.ActivePage is TASTTabSheet) then
    TASTTabSheet(ASTPageControl.ActivePage).SynEdit.SearchReplace(SearchEdit.Text, '', []);
end;

procedure TfrmTestAppMain.FilesCheckAllActionExecute(Sender: TObject);
begin
  if TAction(Sender).Tag = 0 then
  begin
    lbFiles.CheckAll(cbUnchecked);
    TAction(Sender).Tag := 1;
  end else
  begin
    lbFiles.CheckAll(cbChecked);
    TAction(Sender).Tag := 0;
  end;
end;

procedure TfrmTestAppMain.FilesCheckAllActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := lbFiles.Count > 0;
end;

procedure TfrmTestAppMain.FilesParseFocusedActionExecute(Sender: TObject);
begin
  var LProject := CreateProject({AParseSystemUnit:} True);
  LProject.AddUnit(lbFiles.Items[lbFiles.ItemIndex]);
  ParseProject(LProject);
  LProject.Clear({AClearImplicitUnits:} True);
end;

procedure TfrmTestAppMain.FilesParseFocusedActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := lbFiles.ItemIndex >= 0;
end;

procedure TfrmTestAppMain.FilesRemoveAllActionExecute(Sender: TObject);
begin
  lbFiles.Clear;
end;

function TfrmTestAppMain.FocusedTestData: TTestData;
begin
  if Assigned(VTTests.FocusedNode) then
    Result := VTTests.FocusedNode.GetData<TTestData>
  else
    Result := nil;
end;

procedure TfrmTestAppMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;
  TPooledObject.ClearPool;
end;

procedure TfrmTestAppMain.FormCreate(Sender: TObject);
begin
  FAllTests := TDictionary<string, TTestData>.Create;

  MainPageControl.ActivePageIndex := 0;
  SrcPageControl.ActivePageIndex := 0;

  fSettings := TPascalProjectSettings.Create;
  TASTParserLog.Instance.OnWriteProc := procedure(const AMessage: string; ANestedLevel: Integer)
  begin
    if WriteLogCheck.Checked then
      LogMemo.Lines.Add(DupeString(' ', ANestedLevel) + AMessage);
  end;

  LoadSettings;

  lbFiles.MultiSelect := True;

  ReportMemoryLeaksOnShutdown := ShowMemLeaksCheck.Checked;
end;

procedure TfrmTestAppMain.FormDestroy(Sender: TObject);
begin
  FAllTests.Free;
end;

function TfrmTestAppMain.GetTestSriptsPath: string;
begin
  Result := TestScriptsPathEdit.Text;
  if IsRelativePath(Result) then
    Result := TPath.Combine(ExtractFilePath(Application.ExeName), Result);
end;

//procedure TestSetImplicit;
//type
//  TEnum1 = (en11, en12, en13);
//  TEnum2 = (en21, en22, en23);
//  TXSmallSet = set of 0..2;
//  TSmallSet = set of 0..31;
//  TLargeSet = set of Byte;
//  TEnumSet1 = set of TEnum1;
//  TEnumSet2 = set of TEnum2;
//var
//  SS: TSmallSet;
//  XS: TXSmallSet;
//  LS: TLargeSet;
//  ES1: TEnumSet1;
//  ES2: TEnumSet1;
//  I8: Byte;
//  I32: Integer;
//begin
//  SS := LS;
//  LS := SS;
//
//  ES1 := ES2;
//  ES1 := TEnumSet1(I8);
//
//  //LS := ES;
//end;
//
//procedure TestExplicits;
//type
//  TSProc = procedure(a: Integer);
//  TRProc = reference to procedure(a: Integer);
//  TMProc1 = procedure(a: Integer) of object;
//  TMProc2 = procedure(a, b: Integer) of object;
//  TEnum = (en1, en2, en3);
//var
//  Enm: TEnum;
//  Cmp: Comp;
//  Cur: Currency;
//  Sng: Single;
//  Dbl: Double;
//  Vrn: Variant;
//  Ptr: Pointer;
//  Inf: IInterface;
//  RPrc: TRProc;
//  SPrc: TSProc;
//  MPrc: TMProc1;
//  Obj: TObject;
//  WStr: WideString;
//  AStr: AnsiString;
//  UStr: string;
//  DArr: TStringDynArray;
//  SArr: array [1..SizeOf(Pointer)] of Byte;
//  SArrVar: array [1..SizeOf(Variant)] of Byte;
//  Rec: record i: NativeInt end;
//  RecVar: record arr: array [1..SizeOf(Variant)] of Byte end;
//
//  Rec16: record c, d: pointer end;
//  SArr16: array [1..SizeOf(Pointer)*2] of Byte;
//begin
//  var CC0 := TClass(Ptr);
//  var CC1 := TClass(WStr);
//  var CC2 := TClass(AStr);
//  var CC3 := TClass(UStr);
//  var CC4 := TClass(DArr);
//  var CC5 := TClass(SArr);
//  var CC6 := TClass(Inf);
//  var CC7 := TClass(Rec);
//  var CC8 := TClass(Obj);
//  //var CC9 := TClass(RPrc); !!!
//  //var CC9 := TClass(SPrc); !!!
//
//  var TC0 := TObject(Ptr);
//  var TC1 := TObject(WStr);
//  var TC2 := TObject(AStr);
//  var TC3 := TObject(UStr);
//  var TC4 := TObject(DArr);
//  var TC5 := TObject(SArr);
//  var TC6 := TObject(Inf);
//  var TC7 := TObject(Rec);
//  //var TC8 := TObject(RPrc); !!!
//  //var TC8 := TObject(SPrc); !!!
//
//  var XC0 := IInterface(Ptr);
//  var XC1 := IInterface(WStr);
//  var XC2 := IInterface(AStr);
//  var XC3 := IInterface(UStr);
//  var XC4 := IInterface(DArr);
//  var XC5 := IInterface(SArr);
//  var XC7 := IInterface(Rec);
//  //var XC8 := IInterface(Obj); !!!
//  //var XC9 := IInterface(RPrc); !!!
//  //var XC9 := IInterface(SPrc); !!!
//
//  var DA0 := TStringDynArray(Ptr);
//  var DA1 := TStringDynArray(WStr);
//  var DA2 := TStringDynArray(AStr);
//  var DA3 := TStringDynArray(UStr);
//  var DA4 := TStringDynArray(DArr);
//  var DA5 := TStringDynArray(SArr);
//  var DA6 := TStringDynArray(Inf);
//  var DA7 := TStringDynArray(Rec);
//  var DA8 := TStringDynArray(Obj);
//  //var DA9 := TStringDynArray(RPrc); !!!
//  //var DA9 := TStringDynArray(SPrc); !!!
//
//  var SS0 := string(Ptr);
//  var SS1 := string(WStr);
//  var SS2 := string(AStr);
//  var SS3 := string(UStr);
//  var SS4 := string(DArr);
//  var SS5 := string(SArr);
//  var SS6 := string(Inf);
//  var SS7 := string(Rec);
//  var SS8 := string(Obj);
//  //var SS9 := string(DPrc); !!!
//  //var SS9 := string(SPrc); !!!
//
//  var RP0 := TProc(Ptr);
//  var RP1 := TProc(WStr);
//  var RP2 := TProc(AStr);
//  var RP3 := TProc(UStr);
//  var RP4 := TProc(DArr);
//  var RP5 := TProc(SArr);
//  var RP6 := TProc(Inf);
//  var RP7 := TProc(Rec);
//  var RP8 := TProc(RPrc);
//  var RP9 := TProc(SPrc);
//  //var RP10 := TProc(Obj); !!!
//
//  var SP0 := TSProc(Ptr);
//  var SP1 := TSProc(WStr);
//  var SP2 := TSProc(AStr);
//  var SP3 := TSProc(UStr);
//  var SP4 := TSProc(DArr);
//  var SP5 := TSProc(SArr);
//  var SP6 := TSProc(Inf);
//  var SP7 := TSProc(Rec);
//  var SP8 := TSProc(RPrc);
//  var SP9 := TSProc(Obj);
//
//
//  var PP0 := pointer(Ptr);
//  var PP1 := pointer(WStr);
//  var PP2 := pointer(AStr);
//  var PP3 := pointer(UStr);
//  var PP4 := pointer(DArr);
//  var PP5 := pointer(SArr);
//  var PP6 := pointer(Inf);
//  var PP7 := pointer(Rec);
//  var PP8 := pointer(Obj);
//  //var PP8 := pointer(RPrc); !!!
//  //var PP9 := pointer(SPrc); !!!
//
//  var MP0 := TMProc2(MPrc);
//  var MP1 := TMProc2(SArr16);
//  var MP2 := TMProc2(Rec16);
//
//  //var V0 := Variant(Ptr);
//  var V1 := Variant(WStr);
//  var V2 := Variant(AStr);
//  var V3 := Variant(UStr);
//  var V4 := Variant(DArr);
//  var V5 := Variant(SArrVar);
//  var V6 := Variant(Inf);
//  //var V7 := Variant(Obj);
//  var V8 := Variant(RecVar);
//  var V9 := Variant(Sng);
//  var V10 := Variant(Dbl);
//  var V11 := Variant(Cur);
//  var V12 := Variant(Cmp);
//  var V13 := Variant(Enm);
//end;
//
//procedure TestImplicits_Variant;
//var
//
//  Cmp: Comp;
//  Cur: Currency;
//  Sng: Single;
//  Dbl: Double;
//  Vrn: Variant;
//  Ptr: Pointer;
//  Inf: IInterface;
//  Obj: TObject;
//  WStr: WideString;
//  AStr: AnsiString;
//  UStr: string;
//  UChr: WideChar;
//  AChr: AnsiChar;
//  DArrStr: TStringDynArray;
//  DArrInt: TIntegerDynArray;
//begin
//  Cmp := Vrn;
//  Cur := Vrn;
//  Sng := Vrn;
//  Dbl := Vrn;
//  //Ptr := Vrn;
//  Inf := Vrn;
//  //Obj := Vrn;
//  WStr := Vrn;
//  AStr := Vrn;
//  UStr := Vrn;
//  //UChr := Vrn;
//  //AChr := Vrn;
//  DArrStr := Vrn;
//  DArrInt := Vrn;
//
//  Vrn := Cmp;
//  Vrn:= Cur ;
//  Vrn := Sng;
//  Vrn := Dbl;
//  //Vrn := Ptr;
//  Vrn := Inf;
//  //Vrn := Obj;
//  Vrn := WStr;
//  Vrn := AStr;
//  Vrn := UStr;
////  Vrn := UChr; implicit cast to string?
////  Vrn := AChr; implicit cast to string?
//  Vrn := DArrStr;
//  Vrn := DArrInt;
//end;
//
//
//procedure Test0;
//var
//  L: Int64;
//  H: Int64;
//  R: UInt64;
//  A1, A2: TStringDynArray;
//  AS1, AS2: array [1..10] of byte;
//begin
//  L := 0;
//  H := Int64(MaxUInt64);
//  R := UInt64(H) - UInt64(L);
//  A1 := Copy(A2);
//end;
//
//type
//  TInt = type Integer;
//
//  TTT = class
//    constructor Create;
//    class procedure T1;
//    procedure X1(a: TInt); virtual;
//  end;
//
//  TTT2 = class(TTT)
//    procedure X1(n: TInt); override;
//  end;
//
//  TM = procedure of object;
//  TF = function(C: TClass): TTT of object;
//
//procedure Test12;
//begin
//  var V := @TTT.Destroy;
//end;
//
//{ TTT }
//
//constructor TTT.Create;
//begin
//
//end;
//
//class procedure TTT.T1;
//begin
//
//end;
//
//procedure TTT.X1(a: TInt);
//begin
//
//end;
//
//{ TTT2 }
//
//procedure TTT2.X1(n: TInt);
//begin
//  inherited;
//
//end;
//
//procedure XX(A: Integer); overload;
//begin
//
//end;
//
//procedure XX(B: TInt); overload;
//begin
//
//end;
//
//procedure AliasTest;
//var
//  A: TInt;
//  I: Integer;
//begin
//  I := 0;
//  A := I;
//  //if A <> 0 then;
//end;
//
//type
//  IComparer<T> = interface
//    function Compare(const Left, Right: T): Integer;
//  end;
//
//  TArray = class
//    //class procedure Sort<T>(var Values: array of T); overload; static;
//    class procedure Sort<T>(var Values: array of T; const Comparer: IComparer<T>); static;
//  end;
//
//  TC1 = class abstract
//  private
//    FP1: Integer;
//    function GetItem(Index: Integer): string;
//    procedure SetP1(Index: Integer; Value: Integer);
//  protected
//    property P1: Integer index 0 read FP1 write SetP1 stored False default 0;
//    property Items[Index: Integer]: string read GetItem;
//  end;
//
//  TC2 = class(TC1)
//  public
//    property P1 index 0 stored True default 1;
//    property Items index 1 stored False; default;
//  end;
//
//  TC1Class = class of TC1;
//  TC2Class = class of TC2;
//
//procedure Test2;
//var
//  Arr: array of string;
//begin
//  TArray.Sort<string>(Arr, nil);
//end;
//
//procedure X1(A: TArray<string>); overload;
//begin
//
//end;
//
//procedure X1(A: array of string); overload;
//begin
//
//end;
//
//{ TArray }
//
//class procedure TArray.Sort<T>(var Values: array of T; const Comparer: IComparer<T>);
//begin
//
//end;
//
//{ TC1 }
//
//function TC1.GetItem(Index: Integer): string;
//begin
//
//end;
//
//procedure TC1.SetP1(Index, Value: Integer);
//begin
//
//end;
//
//procedure Test; platform;
//  procedure sub; platform;
//  begin
//
//  end;
//begin
//
//end;
//
//type
//
//  TGeneric = class;
//  TGeneric<T> = class;
//  TGeneric<T, K> = class;
//
//  TGeneric = class
//  end;
//
//  TGeneric<T> = class
//  end;
//
//  TGeneric<T, K> = class
//  end;
//
//  TC = class (TGeneric);
//
//  TX<T> = class(TC)
//
//  end;
//
//procedure TestPointers;
//type
//{$POINTERMATH ON}
//  PInteger = ^Integer;
//  PPointer = ^Pointer;
//{$POINTERMATH OFF}
//var
//  Ptr1: PByte;
//  Ptr2: PInteger;
//  Ptr3: PPointer;
//begin
//  Ptr1[0] := 5;
//  Ptr2[0] := 5;
//  Ptr3[0] := nil;
//end;
//
//var
//  G1: TGeneric;
//  G2: TGeneric<string>;
//  G3: TGeneric<string, string>;
//
//type
//  IMyDisp = dispinterface
//    ['{2933BF80-7B36-11D2-B20E-00C04F983E60}']
//    property nodeName: string readonly dispid 1;
//    property nodeValue: OleVariant writeonly dispid 3;
//    property item[index: Integer]: IMyDisp readonly dispid 0; default;
//  end;
//
//  TTestClass = class
//  var
//    data: string;
//    at,
//    on,
//    out,
////    protected,
//    virtual,
//    operator,
//    align,
//    override: string;
//
////  const
////    protected = 5;
//  protected
//    property automated: string read data;
//    property private: string read data;
//    property protected: string read data;
//    property public: string read data;
//    property published: string read data;
//    property strict: string read data;
//    property sealed: string read data;
//    property abstract: string read data;
//    property readonly: string read data;
//    property writeonly: string read data;
//    property dispid: string read data;
//    property default: string read data;
//    property stored: string read data;
//    property index: string read data;
//    property register: string read data;
//    property safecall: string read data;
//    property stdcall: string read data;
//    property cdecl: string read data;
//    property varargs: string read data;
//    property winapi: string read data;
//  end;
//
//  read = integer;
//
//  TTestClass2 = record
//    function read: read;
//    property _read: read read read;
//  end;
//
//  TTestRecord2 = record
//    on,
//    protected,
//    sealed,
//    abstract,
//    readonly,
//    writeonly,
//    dispid,
//    default,
//    stored,
//    index,
//    register,
//    safecall,
//    stdcall,
//    cdecl,
//    assembler,
//    export,
//    helper,
//    forward,
//    virtual,
//    override,
//    varargs: string;
//  end;
//
//  package = class
//    on,
//    protected,
//    sealed,
//    abstract,
//    readonly,
//    writeonly,
//    dispid,
//    default,
//    stored,
//    index,
//    register,
//    safecall,
//    stdcall,
//    cdecl,
//    assembler,
//    export,
//    helper,
//    forward,
//    virtual,
//    override,
//    varargs: string;
//  end;
//
//  TShortIntHelper = record helper for ShortInt
//  const
//      MaxValue = 127;
//      MinValue = -128;
//    class function Get1: Integer; static;
//  end;
//
//  class function TShortIntHelper.Get1: Integer;
//  begin
//    Result := 1;
//  end;
//
//  procedure public;
//  begin
//  end;
//
//  function TTestClass2.read: read;
//  begin
//    Result := 0;
//  end;
//
//function CoRevokeMallocSpy: integer stdcall;
//begin
//  Result := 0;
//end;
//
//var
//  automated,
//  on: Integer;
//
//const
//  CX = IMyDisp;
//
//type
//  TStatic = record
//    class var SVar: Integer;
//  end;
//  PStatic = ^TStatic;
//
//procedure TestStaticAccess;
//var
//  Ptr: PStatic;
//begin
//  var R1 := Ptr.SVar;
//  var R2 := TStatic.SVar;
//  //var R3 := PStatic.SVar;
//end;
//
//procedure TestAddrOperator;
//type
//  TSProc = procedure;
//var
//  LInt: Integer;
//  LDbl: Double;
//  LProc: TSProc;
//begin
//  var p1 := @Lint;
//  var p2: PDouble := @LDbl;
//  var p3 := @TestStaticAccess;
//
////  LInt := P1^;
//  LDbl := P2^;
//  LProc := p1;
//end;
//
//type
//  TDateTime1 = type Double;
//
//  TTimeSpan1 = record
//    class operator Add(const Left: TDateTime1; Right: TTimeSpan1): TDateTime1;
//    class operator Add(const Left: TTimeSpan1; Right: TDateTime1): TDateTime1;
//  end;
//
//  TXC1 = class
//
//  end;
//
//  TXC2 = class
//
//  end;
//
//  TXC1Class = class of TXC1;
//  TXC2Class = class of TXC2;
//
//  IXC1 = interface
//
//  end;
//
//  IXC2 = interface
//
//  end;
//
//  IXC3 = interface(IXC1)
//
//  end;
//
//  TXC3 = class(TInterfacedObject, IXC3)
//
//  end;
//
//class operator TTimeSpan1.Add(const Left: TDateTime1; Right: TTimeSpan1): TDateTime1;
//begin
//  Result := 0;
//end;
//
//class operator TTimeSpan1.Add(const Left: TTimeSpan1; Right: TDateTime1): TDateTime1;
//begin
//  Result := 0;
//end;
//
//procedure TestTimeSpan1;
//var
//  LDt: TDateTime1;
//  LTs: TTimeSpan1;
//begin
//  LDt := LDt + 0.5;
//  LDt := LTs + LDt;
//  LDt := LDt + LTs;
////  if LTs = LTs then
////    sleep(1);
//end;
//
//procedure TestOperators;
//type
//  {$POINTERMATH ON}
//  PInteger2 = ^PInteger;
//  {$POINTERMATH OFF}
//  PByte2 = ^Byte;
//var
//  Int1: Integer;
//  Flt1, Flt2: Double;
//  C1: TC1;
//  C2: TC2;
//  Cls1: TC1Class;
//  Cls2: TC2Class;
//  CX1: TXC1;
//  CX2: TXC2;
//  CX3: TXC3;
//  XCls1: TXC1Class;
//  XCls2: TXC2Class;
//  Intf1: IXC1;
//  Intf2: IXC2;
//  Intf3: IXC3;
//  Bl: Boolean;
//  t: TStartupInfo;
//  Ptr: Pointer;
//  Arr1: array of Integer;
//  Arr2: array of Integer;
//  ArrC: array of AnsiChar;
//
//  NPtr: PControlListItem;
//  PInt: PInteger;
//  PInt2: PInteger2;
//  PByt: PByte;
//  PByt2: PByte2;
//  PAChr: PAnsiChar;
//  PUChr: PChar;
//  PWChr: PWideChar;
//  AStr: AnsiString;
//  UStr: string;
//
//begin
//  Flt1 := 1;
//  Int1 := 1;
//  Flt2 := Int1 / Flt1;
//
//  if C1 <> C2 then;
//  if Cls1 <> Cls2 then
//  //if CX1 <> CX2 then;
//  //if XCls1 <> XCls2 then
//
//  if C1 <> Ptr then;
//  if Arr1 <> nil then;
//  if Arr1 <> Ptr then;
//  if C1 <> Ptr then;
//
//  if PByt <> Ptr then;
//
//
//
//  // if PInt > PInt then;
//  if PAChr > PInt then;
//  if PUChr > PInt then;
//  if PInt2 > PInt2 then;
//  if PAChr < NPtr then;
//
//  if PAChr = AStr then;
//  if PAChr < UStr then;
//  if PUChr = AStr then;
//  if PUChr < UStr then;
//  if PWChr = AStr then;
//  if PWChr < UStr then;
//
//  //if PAChr < ArrC then;
//
//  if PInt2 > PByt then;
//  //if PInt > PByt then;
//
//  // if Intf1 <> Ptr then;
//  //if Intf1 <> Intf2 then;
//  if Intf1 <> Intf3 then;
//  if Intf1 <> Intf3 then;
//  //if Intf3 <> CX3 then;
//end;
//
//initialization
//  Test0;
//  AliasTest;
//  TestOperators;

{ TTestData }

constructor TTestData.Create(const AFileName: string; ANodeType: TNodeType);
begin
  FNodeType := ANodeType;
  SetFilePath(AFileName);
end;

procedure TTestData.SetFilePath(const Value: string);
begin
  FFilePath := Value;
  if FNodeType = ntFolder then
  begin
    // take latest path segment as a name
    var LPathArray := ExcludeTrailingPathDelimiter(Value).Split([PathDelim]);
    if Length(LPathArray) > 0 then
      FCaption := LPathArray[Length(LPathArray) - 1];
  end else
  begin
    FCaption := ExtractFileName(Value);
    FCaption := FCaption.Replace(ExtractFileExt(FCaption), '');
  end;
end;

end.
