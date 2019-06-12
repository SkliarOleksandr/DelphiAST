unit TestAppMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Generics.Collections, AST.Pascal.Project,
  AST.Pascal.Parser, AST.Delphi.Classes, SynEdit, SynEditHighlighter, SynEditCodeFolding, SynHighlighterPas, AST.Delphi.Project,
  Vcl.ComCtrls, Vcl.ExtCtrls;

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
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    //fPKG: INPPackage;
  public
    { Public declarations }
    procedure IndexSources(const RootPath: string; Dict: TSourcesDict);
  end;

var
  frmTestAppMain: TfrmTestAppMain;

implementation

uses
  System.IOUtils, System.Types,
  AST.Delphi.System,
  AST.Delphi.Parser,
  AST.Classes,
  AST.Parser.Messages,
  AST.Writer;

{$R *.dfm}

procedure CompilerMessagesToStrings(const Messages: ICompilerMessages; Strings: TStrings);
var
  I: Integer;
  Msg: TCompilerMessage;
begin
  for i := 0 to Messages.Count - 1 do
  begin
    Msg := Messages[i];
    Strings.Add(Msg.AsString);
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
      Memo1.Lines.Add(FileInfo.FullPath);
      Memo1.Lines.Add(Dict.Items[FileName].FullPath);
    end;
  end;
end;

procedure TfrmTestAppMain.Button1Click(Sender: TObject);
var
  UN: TASTDelphiUnit;
  Msg: TStrings;
  Prj: INPPackage;
  CResult: TCompilerResult;
begin
  Memo1.Clear;

  FreeAndNil(SYSUnit);

  Prj := TASTDelphiProject.Create('test');
  //Prj.AddUnitSearchPath(Edit1.Text);
  Prj.AddUnitSearchPath(ExtractFilePath(Application.ExeName));
  Prj.InitUnits;
  Prj.Target := 'WIN-X86';
  Prj.Defines.Add('CPUX86');
  Prj.Defines.Add('MSWINDOWS');

  //Prj.AddUnit(SystemUnit.SYSUnit, nil);

  UN := TASTDelphiUnit.Create(Prj, 'test', edUnit.Text);
  Prj.AddUnit(UN, nil);

  Msg := TStringList.Create;
  try
    Msg.Add('===================================================================');
    CResult := Prj.Compile;
    if CResult = CompileSuccess then
      Msg.Add('compile success')
    else
      Msg.Add('compile fail');

    ASTToTreeView2(UN, tvAST);

    CompilerMessagesToStrings(Prj.Messages, Msg);

    Memo1.Lines := Msg;
  finally
    Msg.Free;
  end;
end;

const cRTLUsesSource =
'unit RTLParseTest; '#10#13 +
'interface'#10#13 +
'uses System;'#10#13 +
'implementation'#10#13 +
'end.';

procedure TfrmTestAppMain.Button2Click(Sender: TObject);
var
  UN: TASTDelphiUnit;
  Msg: TStrings;
  Prj: INPPackage;
  CResult: TCompilerResult;
begin
  Memo1.Clear;

  FreeAndNil(SYSUnit);

  Prj := TASTDelphiProject.Create('test');
  Prj.AddUnitSearchPath(Edit1.Text);
  Prj.InitUnits;
  Prj.Target := 'WIN-X86';
  Prj.Defines.Add('CPUX86');
  Prj.Defines.Add('CPU386');
  Prj.Defines.Add('MSWINDOWS');

  Prj.AddUnit(AST.Delphi.System.SYSUnit, nil);

  UN := TASTDelphiUnit.Create(Prj, 'RTLParseTest', cRTLUsesSource);
  Prj.AddUnit(UN, nil);

  Msg := TStringList.Create;
  try
    Msg.Add('===================================================================');
    CResult := Prj.Compile;
    if CResult = CompileSuccess then
      Msg.Add('compile success')
    else
      Msg.Add('compile fail');

    ASTToTreeView2(UN, tvAST);

    CompilerMessagesToStrings(Prj.Messages, Msg);

    Memo1.Lines := Msg;
  finally
    Msg.Free;
  end;
end;

end.


