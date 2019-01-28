unit TestAppMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Generics.Collections, NPCompiler.Package,
  NPCompiler.Intf, OPCompiler, NPCompiler.Classes, SynEdit, SynEditHighlighter, SynEditCodeFolding, SynHighlighterPas, AST.Delphi.Project,
  Vcl.ComCtrls;

type
  TSourceFileInfo = record
    FullPath: string;
    DateModify: TDateTime;
  end;

  TSourcesDict = TDictionary<string, TSourceFileInfo>;


  TfrmTestAppMain = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    btnASTParse: TButton;
    Memo1: TMemo;
    Button2: TButton;
    SynPasSyn1: TSynPasSyn;
    PageControl1: TPageControl;
    tsSource: TTabSheet;
    edUnit: TSynEdit;
    tsAST: TTabSheet;
    tvAST: TTreeView;
    procedure btnASTParseClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    fPKG: INPPackage;
  public
    { Public declarations }
    procedure IndexSources(const RootPath: string; Dict: TSourcesDict);
  end;

var
  frmTestAppMain: TfrmTestAppMain;

implementation

uses
  System.IOUtils, IdCTypes, System.Types, SystemUnit, AST.Delphi.Parser, AST.Classes;

{$R *.dfm}


procedure ASTToTreeView(ASTUnit: TASTDelphiUnit; TreeView: TTreeView);
var
  Node: TTreeNode;
begin
  Node := TreeView.Items.AddChild(nil, 'unit ' + ASTUnit.Name);
end;

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

procedure TfrmTestAppMain.btnASTParseClick(Sender: TObject);
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

  Prj.AddUnit(SystemUnit.SYSUnit, nil);

  UN := TASTDelphiUnit.Create(Prj, edUnit.Text);
  Prj.AddUnit(UN, nil);

  Msg := TStringList.Create;
  try
    Msg.Add('===================================================================');
    CResult := Prj.CompileInterfacesOnly;
    if CResult = CompileSuccess then
      Msg.Add('compile success')
    else
      Msg.Add('compile fail');

    ASTToTreeView(UN, tvAST);

    CompilerMessagesToStrings(Prj.Messages, Msg);

    Memo1.Lines := Msg;
  finally
    Msg.Free;
  end;
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

procedure TfrmTestAppMain.Button2Click(Sender: TObject);
var
  UN: TNPUnit;
  Msg: TStrings;
  CResult: TCompilerResult;
begin
  Memo1.Clear;

  FreeAndNil(SYSUnit);

  fPKG := TNPPackage.Create('test');
  //fPKG.AddUnitSearchPath(Edit1.Text);
  fPKG.AddUnitSearchPath(ExtractFilePath(Application.ExeName));
  fPKG.InitUnits;
  fPKG.Target := 'WIN-X86';
  fPKG.Defines.Add('CPUX86');
  fPKG.Defines.Add('MSWINDOWS');

  fPKG.AddUnit(SystemUnit.SYSUnit, nil);

  UN := TNPUnit.Create(fPKG, edUnit.Text);
  fPKG.AddUnit(UN, nil);

  Msg := TStringList.Create;
  try
    Msg.Add('===================================================================');
    CResult := fPKG.Compile;
    if CResult = CompileSuccess then
      Msg.Add('compile success')
    else
      Msg.Add('compile fail');

    CompilerMessagesToStrings(fPKG.Messages, Msg);

    Memo1.Lines := Msg;
  finally
    Msg.Free;
  end;
end;

end.


