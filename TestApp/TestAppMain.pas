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
    PageControl1: TPageControl;
    tsSource: TTabSheet;
    edUnit: TSynEdit;
    tsAST: TTabSheet;
    tvAST: TTreeView;
    Panel1: TPanel;
    tsNameSpace: TTabSheet;
    edAllItems: TSynEdit;
    Panel2: TPanel;
    Panel3: TPanel;
    Button3: TButton;
    Splitter1: TSplitter;
    lbFiles: TCheckListBox;
    Button4: TButton;
    chkbShowSysDecls: TCheckBox;
    chkbShowConstValues: TCheckBox;
    chkbShowAnonymous: TCheckBox;
    Splitter2: TSplitter;
    Panel4: TPanel;
    SynEditSearch1: TSynEditSearch;
    Panel5: TPanel;
    Button5: TButton;
    NSSearchEdit: TEdit;
    ErrMemo: TSynEdit;
    Panel6: TPanel;
    Label1: TLabel;
    edSrcRoot: TEdit;
    chkStopIfError: TCheckBox;
    chkCompileSsystemForASTParse: TCheckBox;
    chkParseAll: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    chkShowWarnings: TCheckBox;
    PageControl2: TPageControl;
    tsLogs: TTabSheet;
    tsFiles: TTabSheet;
    LogMemo: TSynEdit;
    chkWriteLog: TCheckBox;
    Splitter3: TSplitter;
    cbPlatform: TComboBox;
    Label2: TLabel;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    //fPKG: INPPackage;
    fFiles: TStringDynArray;
    fSettings: IASTProjectSettings;
    FStartedAt: TDateTime;
    procedure OnProgress(const Module: IASTModule; Status: TASTProcessStatusClass);
    procedure ShowAllItems(const Project: IASTDelphiProject);
    procedure ShowResult(const Project: IASTDelphiProject);
    procedure CompilerMessagesToStrings(const Project: IASTDelphiProject);
    procedure SetDefines(const APrj: IASTDelphiProject);
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
  AST.Delphi.System,
  AST.Delphi.Parser,
  AST.Classes,
  AST.Writer,
  AST.Targets,
  AST.Delphi.DataTypes, AST.Parser.Utils, AST.Parser.Log;

{$R *.dfm}

procedure TfrmTestAppMain.CompilerMessagesToStrings(const Project: IASTDelphiProject);
var
  I: Integer;
  Msg: TCompilerMessage;
begin
  ErrMemo.Lines.Add('===================================================================');
  for i := 0 to Project.Messages.Count - 1 do
  begin
    Msg := Project.Messages[i];
    if (Msg.MessageType >= cmtError) or chkShowWarnings.Checked then
    begin
      ErrMemo.Lines.AddStrings(Msg.AsString.Split([sLineBreak]));
      if Msg.MessageType = cmtError then
      begin
        ErrMemo.CaretY := ErrMemo.Lines.Count;
        ErrMemo.CaretX := Length(Msg.AsString) + 1;
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

procedure TfrmTestAppMain.Button1Click(Sender: TObject);
var
  UN: TASTDelphiUnit;
  Prj: IASTDelphiProject;
begin
  ErrMemo.Clear;
  LogMemo.Clear;
  TASTParserLog.Instance.ResetNestedLevel;

  FStartedAt := Now;

  Prj := TASTDelphiProject.Create('test');
  Prj.AddUnitSearchPath(ExtractFilePath(Application.ExeName));
  if chkCompileSsystemForASTParse.Checked then
    Prj.AddUnitSearchPath(edSrcRoot.Text);

  SetDefines(Prj);
  Prj.OnProgress := OnProgress;
  Prj.StopCompileIfError := chkStopIfError.Checked;
  Prj.OnConsoleWrite := procedure (const Module: IASTModule; Line: Integer; const Msg: string)
                        begin
                          ErrMemo.Lines.Add(format('#console: [%s: %d]: %s', [Module.Name, Line, Msg]));
                        end;

  UN := TASTDelphiUnit.Create(Prj, 'test', edUnit.Text);
  Prj.AddUnit(UN, nil);

  ShowResult(Prj);

  Prj.Clear;
end;

procedure TfrmTestAppMain.OnProgress(const Module: IASTModule; Status: TASTProcessStatusClass);
begin
  //if Status = TASTStatusParseSuccess then
    ErrMemo.Lines.Add(Module.Name + ' : ' + Status.Name);
end;

procedure TfrmTestAppMain.SetDefines(const APrj: IASTDelphiProject);
begin
  APrj.Defines.Add('UNICODE');
  case cbPlatform.ItemIndex of
    // Win32
    0: begin
      APrj.Target := TWINX86_Target.TargetName;
      APrj.Defines.Add('CPUX86');
      APrj.Defines.Add('CPU386');
      APrj.Defines.Add('CPU32BITS');
      APrj.Defines.Add('WIN32');
      APrj.Defines.Add('MSWINDOWS');
      APrj.Defines.Add('ASSEMBLER');
    end;
    // Win64
    1: begin
      APrj.Target := TWINX64_Target.TargetName;
      APrj.Defines.Add('CPUX64');
      APrj.Defines.Add('CPU64BITS');
      APrj.Defines.Add('WIN64');
      APrj.Defines.Add('MSWINDOWS');
      APrj.Defines.Add('ASSEMBLER');
    end;
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

procedure TfrmTestAppMain.ShowResult(const Project: IASTDelphiProject);
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
  finally
    Msg.Free;
  end;
end;

procedure TfrmTestAppMain.Button2Click(Sender: TObject);

  procedure AddDelphiUnits(var AUsesList: string; const APath: string);
  begin
    var LRtlSources := GetDirectoryFiles(edSrcRoot.Text + APath, '*.pas');
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

  Prj := TASTDelphiProject.Create('test');
  Prj.AddUnitSearchPath(edSrcRoot.Text);

  SetDefines(Prj);
  Prj.OnProgress := OnProgress;
  Prj.StopCompileIfError := chkStopIfError.Checked;
  Prj.CompileAll := chkParseAll.Checked;

  var LUsesUntis := '';
  AddDelphiUnits(LUsesUntis, 'rtl\sys');

  var RTLUsesSourceText :=
  'unit RTLParseTest; '#10#13 +
  'interface'#10#13 +
  'uses'#10#13 +
   LUsesUntis + ';'#10#13 +
  'implementation'#10#13 +
  'end.';

  UN := TASTDelphiUnit.Create(Prj, 'RTLParseTest', RTLUsesSourceText);
  Prj.AddUnit(UN, nil);

  ShowResult(Prj);
end;

procedure TfrmTestAppMain.Button3Click(Sender: TObject);
begin
  fFiles := TDirectory.GetFiles(edSrcRoot.Text, '*.pas', TSearchOption.soAllDirectories);
  lbFiles.Clear;
  lbFiles.Items.BeginUpdate;
  try
    for var i := 0 to Length(fFiles) - 1 do
      lbFiles.AddItem(ExtractRelativePath(edSrcRoot.Text, fFiles[i]), nil);
    lbFiles.CheckAll(cbChecked);
  finally
    lbFiles.Items.EndUpdate;
  end;
end;

procedure TfrmTestAppMain.Button4Click(Sender: TObject);
var
  Msg: TStrings;
  Prj: IASTDelphiProject;
  CResult: TCompilerResult;
begin
  ErrMemo.Clear;

  Prj := TASTDelphiProject.Create('test');
  Prj.AddUnitSearchPath(edSrcRoot.Text);
  Prj.Target := TWINX86_Target.TargetName;
  SetDefines(Prj);
  Prj.OnProgress := OnProgress;

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
  fSettings := TPascalProjectSettings.Create;

  TASTParserLog.Instance.OnWriteProc := procedure(const AMessage: string; ANestedLevel: Integer)
  begin
    if chkWriteLog.Checked then
      LogMemo.Lines.Add(DupeString(' ', ANestedLevel) + AMessage);
  end;
end;

procedure Test;
type
  TSProc = procedure(a: Integer);
  TRProc = reference to procedure(a: Integer);
  TMProc1 = procedure(a: Integer) of object;
  TMProc2 = procedure(a, b: Integer) of object;
var
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
  SArr: array [0..3] of Byte;
  Rec: record i: integer end;

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
end;

initialization
  Test;

end.
