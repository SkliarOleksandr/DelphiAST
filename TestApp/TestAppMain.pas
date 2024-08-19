unit TestAppMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Generics.Collections, AST.Pascal.Project,
  AST.Pascal.Parser, AST.Delphi.Classes, SynEdit, SynEditHighlighter, SynEditCodeFolding, SynHighlighterPas, AST.Delphi.Project,
  Vcl.ComCtrls, System.Types, Vcl.ExtCtrls, AST.Intf, AST.Parser.ProcessStatuses, Vcl.CheckLst, SynEditMiscClasses,
  SynEditSearch, AST.Parser.Messages;   // system

type
  TSourceFileInfo = record
    FullPath: string;
    DateModify: TDateTime;
  end;

  TSourcesDict = TDictionary<string, TSourceFileInfo>;


  TfrmTestAppMain = class(TForm)
    SynPasSyn1: TSynPasSyn;
    SrcPageControl: TPageControl;
    tsSource: TTabSheet;
    edUnit: TSynEdit;
    tsAST: TTabSheet;
    tvAST: TTreeView;
    BottomPanel: TPanel;
    tsNameSpace: TTabSheet;
    edAllItems: TSynEdit;
    Panel2: TPanel;
    Panel3: TPanel;
    LoadFilesButton: TButton;
    lbFiles: TCheckListBox;
    ParseFilesButton: TButton;
    chkbShowSysDecls: TCheckBox;
    chkbShowConstValues: TCheckBox;
    chkbShowAnonymous: TCheckBox;
    Splitter2: TSplitter;
    MainPanel: TPanel;
    SynEditSearch1: TSynEditSearch;
    Panel5: TPanel;
    Button5: TButton;
    NSSearchEdit: TEdit;
    ErrMemo: TSynEdit;
    Panel6: TPanel;
    LeftPageControl: TPageControl;
    tsLogs: TTabSheet;
    tsFiles: TTabSheet;
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
    ASTParseButton: TButton;
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
    procedure ASTParseRTLButtonClick(Sender: TObject);
    procedure ASTParseButtonClick(Sender: TObject);
    procedure LoadFilesButtonClick(Sender: TObject);
    procedure ParseFilesButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SaveButtonClick(Sender: TObject);
    procedure SaveSettingsButtonClick(Sender: TObject);
    procedure StopIfErrorCheckClick(Sender: TObject);
    procedure ShowMemLeaksCheckClick(Sender: TObject);
  private
    { Private declarations }
    //fPKG: INPPackage;
    fFiles: TStringDynArray;
    fSettings: IASTProjectSettings;
    FStartedAt: TDateTime;
    FLockSaveSettings: Boolean;
    procedure OnProgress(const Module: IASTModule; Status: TASTProcessStatusClass);
    procedure ShowAllItems(const Project: IASTDelphiProject);
    procedure ParseProject(const Project: IASTDelphiProject);
    procedure CompilerMessagesToStrings(const Project: IASTDelphiProject);
    procedure SetDefines(const APrj: IASTDelphiProject);
    procedure SaveSettings;
    procedure LoadSettings;
    function CreateProject(AIncludeRTLPath: Boolean): IASTDelphiProject;
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
  AST.Delphi.System,
  AST.Delphi.Parser,
  AST.Delphi.Declarations,
  AST.Classes,
  AST.Writer,
  AST.Targets,
  AST.Delphi.DataTypes,
  AST.Parser.Utils,
  AST.Parser.Log;

{$R *.dfm}

const
  SCurSrcFileName = 'current_source.pas';

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
        if Msg.UnitName = 'TestUnit.XXX' then
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

procedure TfrmTestAppMain.ASTParseButtonClick(Sender: TObject);
var
  UN: TASTDelphiUnit;
  Prj: IASTDelphiProject;
begin
  ErrMemo.Clear;
  LogMemo.Clear;
  TASTParserLog.Instance.ResetNestedLevel;

  FStartedAt := Now;

  Prj := CreateProject({AIncludeRTLPath:} ParseSystemCheck.Checked);

  UN := TASTDelphiUnit.Create(Prj, 'test', edUnit.Text);
  Prj.AddUnit(UN, nil);

  ParseProject(Prj);

  Prj.Clear;
end;

procedure TfrmTestAppMain.ASTParseRTLButtonClick(Sender: TObject);

  procedure AddDelphiUnits(var AUsesList: string; const APath: string);
  begin
    var LRtlSources := GetDirectoryFiles(DelphiSrcPathEdit.Text + APath, '*.pas');
    for var LPath in LRtlSources do
    begin
      var LUnitName := StringReplace(ExtractFileName(LPath), '.pas', '', [rfReplaceAll]);
      AUsesList := AddStringSegment(AUsesList, LUnitName, ',');
    end;

  end;

var
  UN: TASTDelphiUnit;
  Prj: IASTDelphiProject;
begin
  ErrMemo.Clear;
  LogMemo.Clear;
  TASTParserLog.Instance.ResetNestedLevel;

  FStartedAt := Now;

  Prj := CreateProject({AIncludeRTLPath:} True);

  var LUsesUntis := '';
  AddDelphiUnits({var} LUsesUntis, 'rtl\sys');

  var RTLUsesSourceText :=
  'unit RTLParseTest; '#10#13 +
  'interface'#10#13 +
  'uses'#10#13 +
   LUsesUntis + ';'#10#13 +
  'implementation'#10#13 +
  'end.';

  UN := TASTDelphiUnit.Create(Prj, 'RTLParseTest', RTLUsesSourceText);
  Prj.AddUnit(UN, nil);

  ParseProject(Prj);
end;

procedure TfrmTestAppMain.OnProgress(const Module: IASTModule; Status: TASTProcessStatusClass);
begin
  //if Status = TASTStatusParseSuccess then
    ErrMemo.Lines.Add(Module.Name + ' : ' + Status.Name);
end;

procedure TfrmTestAppMain.SaveButtonClick(Sender: TObject);
begin
  edUnit.Lines.SaveToFile(SCurSrcFileName);
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

      LINI.WriteString(SGeneral, 'PLATFORM', cbPlatform.Text);
      LINI.WriteString(SGeneral, 'DELPHI_SRC_PATH', DelphiSrcPathEdit.Text);
        LINI.WriteBool(SGeneral, 'DELPHI_SRC_PATH_INCLUDE_SUBDIRS', DelphiPathIncludeSubDirCheck.Checked);

      LINI.WriteString(SGeneral, 'UNIT_SCOPE_NAMES', UnitScopeNamesEdit.Text);
      LINI.WriteString(SGeneral, 'UNIT_SEARCH_PATH', UnitSearchPathEdit.Text);
        LINI.WriteBool(SGeneral, 'UNIT_SEARCH_PATH_INCLUDE_SUBDIRS', UnitSearchPathIncludeSubDirCheck.Checked);
      LINI.WriteString(SGeneral, 'COND_DEFINES', CondDefinesEdit.Text);
    finally
      LINI.Free;
    end;
  end;
end;

procedure TfrmTestAppMain.SaveSettingsButtonClick(Sender: TObject);
begin
  SaveSettings;
end;

procedure TfrmTestAppMain.LoadSettings;
begin
  var LINI := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    // to avoid SaveSettings auto-triggering
    FLockSaveSettings := True;

    StopIfErrorCheck.Checked := LINI.ReadBool(SGeneral, 'STOP_IF_ERROR', True);
    ParseSystemCheck.Checked := LINI.ReadBool(SGeneral, 'PARSE_SYSTEM', False);
    ParseImplsCheck.Checked := LINI.ReadBool(SGeneral, 'PARSE_IMPLS', True);
    WriteLogCheck.Checked := LINI.ReadBool(SGeneral, 'WRITE_LOG', True);
    ShowWarningsCheck.Checked := LINI.ReadBool(SGeneral, 'SHOW_WARNINGS', False);
    ShowMemLeaksCheck.Checked := LINI.ReadBool(SGeneral, 'SHOW_MEMLEAKS', False);

    cbPlatform.Text := LINI.ReadString(SGeneral, 'PLATFORM', 'WIN32');
    DelphiSrcPathEdit.Text := LINI.ReadString(SGeneral, 'DELPHI_SRC_PATH', DelphiSrcPathEdit.Text);
    DelphiPathIncludeSubDirCheck.Checked := LINI.ReadBool(SGeneral, 'DELPHI_SRC_PATH_INCLUDE_SUBDIRS', True);
    UnitScopeNamesEdit.Text := LINI.ReadString(SGeneral, 'UNIT_SCOPE_NAMES', UnitScopeNamesEdit.Text);
    UnitSearchPathEdit.Text := LINI.ReadString(SGeneral, 'UNIT_SEARCH_PATH', UnitSearchPathEdit.Text);
    UnitSearchPathIncludeSubDirCheck.Checked := LINI.ReadBool(SGeneral, 'UNIT_SEARCH_PATH_INCLUDE_SUBDIRS', True);
    CondDefinesEdit.Text := LINI.ReadString(SGeneral, 'COND_DEFINES', CondDefinesEdit.Text);

  finally
    FLockSaveSettings := False;
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
end;

function TfrmTestAppMain.CreateProject(AIncludeRTLPath: Boolean): IASTDelphiProject;
begin
  Result := TASTDelphiProject.Create('test');

  if AIncludeRTLPath then
    Result.AddUnitSearchPath(DelphiSrcPathEdit.Text, DelphiPathIncludeSubDirCheck.Checked);

  for var LPath in string(UnitSearchPathEdit.Text).Split([';']) do
    Result.AddUnitSearchPath(LPath, UnitSearchPathIncludeSubDirCheck.Checked);

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

procedure TfrmTestAppMain.ShowAllItems(const Project: IASTDelphiProject);
begin
  edAllItems.BeginUpdate;
  try
    edAllItems.Clear;
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
      edAllItems.Text := LBuilder.ToString;
      LBuilder.Free;
    end;
  finally
    edAllItems.EndUpdate;
  end;
end;

procedure TfrmTestAppMain.ShowMemLeaksCheckClick(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := ShowMemLeaksCheck.Checked;
end;

procedure TfrmTestAppMain.StopIfErrorCheckClick(Sender: TObject);
begin
  SaveSettings;
end;

procedure TfrmTestAppMain.ParseProject(const Project: IASTDelphiProject);
begin
  var Msg := TStringList.Create;
  try
    var CResult := Project.Compile;
    if CResult = CompileSuccess then
      Msg.Add('compile success')
    else
      Msg.Add('compile fail');

    Msg.Add(format('total units parsed: %d (interface only: %d)',
      [Project.TotalUnitsParsed, Project.TotalUnitsIntfOnlyParsed]));
    Msg.Add(format('total lines parsed: %d in %s', [Project.TotalLinesParsed,
                                                    FormatDateTime('nn:ss.zzz', Now - FStartedAt)]));

      //ASTToTreeView2(UN, tvAST);

    ShowAllItems(Project);
    CompilerMessagesToStrings(Project);

    ErrMemo.Lines.AddStrings(Msg);
    ErrMemo.CaretY := ErrMemo.Lines.Count;
    ErrMemo.CaretX := 1;
  finally
    Msg.Free;
  end;
end;

procedure TfrmTestAppMain.LoadFilesButtonClick(Sender: TObject);
begin
  fFiles := TDirectory.GetFiles(DelphiSrcPathEdit.Text, '*.pas', TSearchOption.soAllDirectories);
  lbFiles.Clear;
  lbFiles.Items.BeginUpdate;
  try
    for var i := 0 to Length(fFiles) - 1 do
      lbFiles.AddItem(ExtractRelativePath(DelphiSrcPathEdit.Text, fFiles[i]), nil);
    lbFiles.CheckAll(cbChecked);
  finally
    lbFiles.Items.EndUpdate;
  end;
end;

procedure TfrmTestAppMain.ParseFilesButtonClick(Sender: TObject);
var
  Msg: TStrings;
  Prj: IASTDelphiProject;
  CResult: TCompilerResult;
begin
  ErrMemo.Clear;

  Prj := CreateProject({AIncludeRTLPath:} True);

  for var f in fFiles do
    Prj.AddUnit(f);

  Msg := TStringList.Create;
  try
    Msg.Add('===================================================================');
    CResult := Prj.Compile;
    if CResult = CompileSuccess then
      Msg.Add('compile success')
    else
      Msg.Add('compile fail');

    //ASTToTreeView2(UN, tvAST);

    edAllItems.BeginUpdate;
    try
      edAllItems.Clear;
      Prj.EnumDeclarations(
        procedure(const Module: TASTModule; const Decl: TASTDeclaration)
        begin
          edAllItems.Lines.Add(format('%s - %s.%s', [GetItemTypeName(TIDDeclaration(Decl).ItemType), Module.Name, GetDeclName(Decl)]));
          Application.ProcessMessages;
        end, {AUnitScope} scopeInterface);
    finally
      edAllItems.EndUpdate;
    end;

    CompilerMessagesToStrings(Prj);

    ErrMemo.Lines.AddStrings(Msg);
  finally
    Msg.Free;
  end;
end;

procedure TfrmTestAppMain.Button5Click(Sender: TObject);
begin
  edAllItems.SearchReplace(NSSearchEdit.Text, '', []);
end;

procedure TfrmTestAppMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TPooledObject.ClearPool;
end;

procedure TfrmTestAppMain.FormCreate(Sender: TObject);
begin
  MainPageControl.ActivePageIndex := 0;
  SrcPageControl.ActivePageIndex := 0;

  fSettings := TPascalProjectSettings.Create;
  TASTParserLog.Instance.OnWriteProc := procedure(const AMessage: string; ANestedLevel: Integer)
  begin
    if WriteLogCheck.Checked then
      LogMemo.Lines.Add(DupeString(' ', ANestedLevel) + AMessage);
  end;

  if FileExists(SCurSrcFileName) then
    edUnit.Lines.LoadFromFile(SCurSrcFileName);

  LoadSettings;

  ReportMemoryLeaksOnShutdown := ShowMemLeaksCheck.Checked;
end;

procedure TestSetImplicit;
type
  TEnum1 = (en11, en12, en13);
  TEnum2 = (en21, en22, en23);
  TXSmallSet = set of 0..2;
  TSmallSet = set of 0..31;
  TLargeSet = set of Byte;
  TEnumSet1 = set of TEnum1;
  TEnumSet2 = set of TEnum2;
var
  SS: TSmallSet;
  XS: TXSmallSet;
  LS: TLargeSet;
  ES1: TEnumSet1;
  ES2: TEnumSet1;
  I8: Byte;
  I32: Integer;
begin
  SS := LS;
  LS := SS;

  ES1 := ES2;
  ES1 := TEnumSet1(I8);

  //LS := ES;
end;

procedure TestExplicits;
type
  TSProc = procedure(a: Integer);
  TRProc = reference to procedure(a: Integer);
  TMProc1 = procedure(a: Integer) of object;
  TMProc2 = procedure(a, b: Integer) of object;
  TEnum = (en1, en2, en3);
var
  Enm: TEnum;
  Cmp: Comp;
  Cur: Currency;
  Sng: Single;
  Dbl: Double;
  Vrn: Variant;
  Ptr: Pointer;
  Inf: IInterface;
  RPrc: TRProc;
  SPrc: TSProc;
  MPrc: TMProc1;
  Obj: TObject;
  WStr: WideString;
  AStr: AnsiString;
  UStr: string;
  DArr: TStringDynArray;
  SArr: array [1..SizeOf(Pointer)] of Byte;
  SArrVar: array [1..SizeOf(Variant)] of Byte;
  Rec: record i: NativeInt end;
  RecVar: record arr: array [1..SizeOf(Variant)] of Byte end;

  Rec16: record c, d: pointer end;
  SArr16: array [1..SizeOf(Pointer)*2] of Byte;
begin
  var CC0 := TClass(Ptr);
  var CC1 := TClass(WStr);
  var CC2 := TClass(AStr);
  var CC3 := TClass(UStr);
  var CC4 := TClass(DArr);
  var CC5 := TClass(SArr);
  var CC6 := TClass(Inf);
  var CC7 := TClass(Rec);
  var CC8 := TClass(Obj);
  //var CC9 := TClass(RPrc); !!!
  //var CC9 := TClass(SPrc); !!!

  var TC0 := TObject(Ptr);
  var TC1 := TObject(WStr);
  var TC2 := TObject(AStr);
  var TC3 := TObject(UStr);
  var TC4 := TObject(DArr);
  var TC5 := TObject(SArr);
  var TC6 := TObject(Inf);
  var TC7 := TObject(Rec);
  //var TC8 := TObject(RPrc); !!!
  //var TC8 := TObject(SPrc); !!!

  var XC0 := IInterface(Ptr);
  var XC1 := IInterface(WStr);
  var XC2 := IInterface(AStr);
  var XC3 := IInterface(UStr);
  var XC4 := IInterface(DArr);
  var XC5 := IInterface(SArr);
  var XC7 := IInterface(Rec);
  //var XC8 := IInterface(Obj); !!!
  //var XC9 := IInterface(RPrc); !!!
  //var XC9 := IInterface(SPrc); !!!

  var DA0 := TStringDynArray(Ptr);
  var DA1 := TStringDynArray(WStr);
  var DA2 := TStringDynArray(AStr);
  var DA3 := TStringDynArray(UStr);
  var DA4 := TStringDynArray(DArr);
  var DA5 := TStringDynArray(SArr);
  var DA6 := TStringDynArray(Inf);
  var DA7 := TStringDynArray(Rec);
  var DA8 := TStringDynArray(Obj);
  //var DA9 := TStringDynArray(RPrc); !!!
  //var DA9 := TStringDynArray(SPrc); !!!

  var SS0 := string(Ptr);
  var SS1 := string(WStr);
  var SS2 := string(AStr);
  var SS3 := string(UStr);
  var SS4 := string(DArr);
  var SS5 := string(SArr);
  var SS6 := string(Inf);
  var SS7 := string(Rec);
  var SS8 := string(Obj);
  //var SS9 := string(DPrc); !!!
  //var SS9 := string(SPrc); !!!

  var RP0 := TProc(Ptr);
  var RP1 := TProc(WStr);
  var RP2 := TProc(AStr);
  var RP3 := TProc(UStr);
  var RP4 := TProc(DArr);
  var RP5 := TProc(SArr);
  var RP6 := TProc(Inf);
  var RP7 := TProc(Rec);
  var RP8 := TProc(RPrc);
  var RP9 := TProc(SPrc);
  //var RP10 := TProc(Obj); !!!

  var SP0 := TSProc(Ptr);
  var SP1 := TSProc(WStr);
  var SP2 := TSProc(AStr);
  var SP3 := TSProc(UStr);
  var SP4 := TSProc(DArr);
  var SP5 := TSProc(SArr);
  var SP6 := TSProc(Inf);
  var SP7 := TSProc(Rec);
  var SP8 := TSProc(RPrc);
  var SP9 := TSProc(Obj);


  var PP0 := pointer(Ptr);
  var PP1 := pointer(WStr);
  var PP2 := pointer(AStr);
  var PP3 := pointer(UStr);
  var PP4 := pointer(DArr);
  var PP5 := pointer(SArr);
  var PP6 := pointer(Inf);
  var PP7 := pointer(Rec);
  var PP8 := pointer(Obj);
  //var PP8 := pointer(RPrc); !!!
  //var PP9 := pointer(SPrc); !!!

  var MP0 := TMProc2(MPrc);
  var MP1 := TMProc2(SArr16);
  var MP2 := TMProc2(Rec16);

  //var V0 := Variant(Ptr);
  var V1 := Variant(WStr);
  var V2 := Variant(AStr);
  var V3 := Variant(UStr);
  var V4 := Variant(DArr);
  var V5 := Variant(SArrVar);
  var V6 := Variant(Inf);
  //var V7 := Variant(Obj);
  var V8 := Variant(RecVar);
  var V9 := Variant(Sng);
  var V10 := Variant(Dbl);
  var V11 := Variant(Cur);
  var V12 := Variant(Cmp);
  var V13 := Variant(Enm);
end;

procedure TestImplicits_Variant;
var

  Cmp: Comp;
  Cur: Currency;
  Sng: Single;
  Dbl: Double;
  Vrn: Variant;
  Ptr: Pointer;
  Inf: IInterface;
  Obj: TObject;
  WStr: WideString;
  AStr: AnsiString;
  UStr: string;
  UChr: WideChar;
  AChr: AnsiChar;
  DArrStr: TStringDynArray;
  DArrInt: TIntegerDynArray;
begin
  Cmp := Vrn;
  Cur := Vrn;
  Sng := Vrn;
  Dbl := Vrn;
  //Ptr := Vrn;
  Inf := Vrn;
  //Obj := Vrn;
  WStr := Vrn;
  AStr := Vrn;
  UStr := Vrn;
  //UChr := Vrn;
  //AChr := Vrn;
  DArrStr := Vrn;
  DArrInt := Vrn;

  Vrn := Cmp;
  Vrn:= Cur ;
  Vrn := Sng;
  Vrn := Dbl;
  //Vrn := Ptr;
  Vrn := Inf;
  //Vrn := Obj;
  Vrn := WStr;
  Vrn := AStr;
  Vrn := UStr;
//  Vrn := UChr; implicit cast to string?
//  Vrn := AChr; implicit cast to string?
  Vrn := DArrStr;
  Vrn := DArrInt;
end;


procedure Test0;
var
  L: Int64;
  H: Int64;
  R: UInt64;
  A1, A2: TStringDynArray;
  AS1, AS2: array [1..10] of byte;
begin
  L := 0;
  H := Int64(MaxUInt64);
  R := UInt64(H) - UInt64(L);
  A1 := Copy(A2);
end;

type
  TInt = type Integer;

  TTT = class
    constructor Create;
    class procedure T1;
    procedure X1(a: TInt); virtual;
  end;

  TTT2 = class(TTT)
    procedure X1(n: TInt); override;
  end;

  TM = procedure of object;
  TF = function(C: TClass): TTT of object;

procedure Test12;
begin
  var V := @TTT.Destroy;
end;

{ TTT }

constructor TTT.Create;
begin

end;

class procedure TTT.T1;
begin

end;

procedure TTT.X1(a: TInt);
begin

end;

{ TTT2 }

procedure TTT2.X1(n: TInt);
begin
  inherited;

end;

procedure XX(A: Integer); overload;
begin

end;

procedure XX(B: TInt); overload;
begin

end;

procedure AliasTest;
var
  A: TInt;
  I: Integer;
begin
  I := 0;
  A := I;
  //if A <> 0 then;
end;

type
  IComparer<T> = interface
    function Compare(const Left, Right: T): Integer;
  end;

  TArray = class
    //class procedure Sort<T>(var Values: array of T); overload; static;
    class procedure Sort<T>(var Values: array of T; const Comparer: IComparer<T>); static;
  end;

procedure Test2;
var
  Arr: array of string;
begin
  TArray.Sort<string>(Arr, nil);
end;

procedure X1(A: TArray<string>); overload;
begin

end;

procedure X1(A: array of string); overload;
begin

end;

{ TArray }

class procedure TArray.Sort<T>(var Values: array of T; const Comparer: IComparer<T>);
begin

end;

initialization
  Test0;
  AliasTest;

end.
