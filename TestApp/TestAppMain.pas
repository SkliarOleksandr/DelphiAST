unit TestAppMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Generics.Collections, NPCompiler.Package,
  NPCompiler.Intf, OPCompiler, NPCompiler.Classes, SynEdit, SynEditHighlighter, SynEditCodeFolding, SynHighlighterPas;

type
  TSourceFileInfo = record
    FullPath: string;
    DateModify: TDateTime;
  end;

  TSourcesDict = TDictionary<string, TSourceFileInfo>;


  TfrmTestAppMain = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    Memo1: TMemo;
    Label2: TLabel;
    Button2: TButton;
    edUnit: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    procedure Button1Click(Sender: TObject);
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
  System.IOUtils, IdCTypes, System.Types, SystemUnit;

{$R *.dfm}

procedure TfrmTestAppMain.Button1Click(Sender: TObject);
var
  Dict: TSourcesDict;
begin
  Memo1.Lines.Clear;
  Dict := TSourcesDict.Create();
  IndexSources(Edit1.Text, Dict);
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
    CResult := fPKG.CompileInterfacesOnly;
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


