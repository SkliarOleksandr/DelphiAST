unit NPCompiler.Package;

interface

uses System.SysUtils, System.Classes, System.Types, Generics.Collections, System.IOUtils,
     AVL,
     AST.Delphi.DataTypes,
     AST.Delphi.Classes,
     NPCompiler.Options,
     AST.Targets,
     AST.Parser.Messages,
     AST.Parser.Utils,
     AST.Pascal.Parser,
     AST.Delphi.System,
     AST.Classes;

type
  TUnits = TList<TASTModule>;
  TTypes = TList<TIDType>;

  TNPPackage = class(TASTProject, INPPackage)
  type
    TStrConstKey = record
      StrTypeID: TDataTypeID;
      StrValue: string;
    end;
    TConstInfo = record
      Decl: TIDConstant;
      Index: Integer;
    end;
    TStrLiterals = TAVLTree<TStrConstKey, TConstInfo>;
  private
    FName: string;
    FUnits: TUnits;
    FTargetName: string;
    FTarget: TNPLTarget;
    FDefines: TDefines;
    FStrLiterals: TStrLiterals;
    FIncludeDebugInfo: Boolean;
    FUnitSearchPathes: TStrings;
    FMessages: ICompilerMessages;
    FRTTICharset: TRTTICharset;
    FOptions: TPackageOptions;
    function GetIncludeDebugInfo: Boolean;
    function OpenUnit(const UnitName: string): TASTModule;
    function RefCount: Integer;
    function GetRTTICharset: TRTTICharset;
    function GetUnitsCount: Integer;
    function GetUnit(Index: Integer): TObject; overload;
    function GetUnit(const UnitName: string): TObject; overload;
    function GetSearchPathes: TStrings;
    function GetOptions: TPackageOptions;
    function GetTarget: string;
    function GetDefines: TDefines;
    function GetPointerSize: Integer;
    function GetNativeIntSize: Integer;
    procedure SetIncludeDebugInfo(const Value: Boolean);
    procedure SetRTTICharset(const Value: TRTTICharset);
    procedure SetTarget(const Value: string);
    procedure InitUnits;
    class function StrListCompare(const Left, Right: TStrConstKey): NativeInt; static;
  protected
    function GetUnitClass: TASTUnitClass; override;
  public
    constructor Create(const Name: string; RTTICharset: TRTTICharset = RTTICharsetASCII);
    destructor Destroy; override;
    ////////////////////////////////////////
    procedure SaveToStream(Stream: TStream);
    procedure PrepareStrLiterals;
    procedure SaveStrLiterals(Stream: TStream);
    procedure AddUnit(aUnit, BeforeUnit: TASTModule); overload;
    procedure AddUnit(const Source: string); overload;
    procedure AddUnitSearchPath(const Path: string; IncludeSubDirectories: Boolean);
    procedure Clear;
    property IncludeDebugInfo: Boolean read GetIncludeDebugInfo write SetIncludeDebugInfo;
    property RTTICharset: TRTTICharset read GetRTTICharset write SetRTTICharset;
    property Units: TUnits read FUnits;
    function GetStringConstant(const Value: string): Integer; overload;
    function GetStringConstant(const StrConst: TIDStringConstant): Integer; overload;
    function FindUnitFile(const UnitName: string): string;
    function UsesUnit(const UnitName: string; AfterUnit: TASTModule): TASTModule;
    function GetMessages: ICompilerMessages;
    function Compile: TCompilerResult; virtual;
    function CompileInterfacesOnly: TCompilerResult; virtual;
  end;

implementation

uses AST.Parser.Errors,
     AST.Delphi.Errors;

function TNPPackage.GetOptions: TPackageOptions;
begin
  Result := FOptions;
end;

function TNPPackage.GetDefines: TDefines;
begin
  Result := FDefines;
end;

function TNPPackage.GetIncludeDebugInfo: Boolean;
begin
  Result := FIncludeDebugInfo;
end;

function TNPPackage.GetMessages: ICompilerMessages;
var
  i: Integer;
  UnitMessages: ICompilerMessages;
begin
  FMessages.Clear;
  for i := 0 to FUnits.Count - 1 do begin
    UnitMessages := TNPUnit(FUnits[i]).Messages;
    FMessages.CopyFrom(UnitMessages);
  end;
  Result := FMessages;
end;

function TNPPackage.GetRTTICharset: TRTTICharset;
begin
  Result := FRTTICharset;
end;

function TNPPackage.GetSearchPathes: TStrings;
begin
  Result := FUnitSearchPathes;
end;

function TNPPackage.GetStringConstant(const Value: string): Integer;
var
  Node: TStrLiterals.PAVLNode;
  Data: TConstInfo;
  Key: TStrConstKey;
begin
  case FRTTICharset of
    RTTICharsetASCII: Key.StrTypeID := dtAnsiString;
  else
    Key.StrTypeID := dtString;
  end;

  Key.StrValue := Value;
  Node := FStrLiterals.Find(Key);
  if Assigned(Node) then
    Result := Node.Data.Index
  else begin
    Result := FStrLiterals.Count;
    Data.Decl := nil;
    Data.Index := Result;
    FStrLiterals.InsertNode(Key, Data);
  end;
end;

function TNPPackage.GetStringConstant(const StrConst: TIDStringConstant): Integer;
var
  Node: TStrLiterals.PAVLNode;
  Data: TConstInfo;
  Key: TStrConstKey;
begin
  Key.StrTypeID := StrConst.DataTypeID;
  Key.StrValue := StrConst.Value;
  Node := FStrLiterals.Find(Key);
  if Assigned(Node) then
    Result := Node.Data.Index
  else begin
    Result := FStrLiterals.Count;
    Data.Decl := StrConst;
    Data.Index := Result;
    FStrLiterals.InsertNode(Key, Data);
  end;
end;

function TNPPackage.GetTarget: string;
begin
  Result := FTargetName;
end;

function TNPPackage.GetUnit(Index: Integer): TObject;
begin
  Result := FUnits[Index];
end;

function TNPPackage.GetUnit(const UnitName: string): TObject;
var
  i: Integer;
begin
  for i := 0 to FUnits.Count - 1 do
  begin
    Result := FUnits[i];
    if TNPUnit(Result).Name = UnitName then
      Exit;
  end;
  Result := nil;
end;

function TNPPackage.GetUnitClass: TASTUnitClass;
begin
  Result := TNPUnit;
end;

function TNPPackage.GetUnitsCount: Integer;
begin
  Result := FUnits.Count;
end;

procedure StringStreamLoadFromFile(const FileName: string; Stream: TStringStream);
var
  FStream: TStream;
begin
  FStream := TFileStream.Create(FileName, fmOpenRead);
  try
    FStream.Position := 0;
    Stream.CopyFrom(FStream, FStream.Size);
  finally
    FStream.Free;
  end;
end;

procedure TNPPackage.InitUnits;
var
  Stream: TStringStream;
  SysFileName, SysSource: string;
begin
  try
    if not Assigned(SYSUnit) then
    begin
      SysFileName := FindUnitFile('system');
      if FileExists(SysFileName) then
      begin
        Stream := TStringStream.Create('');
        try
          StringStreamLoadFromFile(SysFileName, Stream);
          SysSource := Stream.DataString;
        finally
          Stream.Free;
        end;
      end else
        SysSource := 'unit system; end.';

      SYSUnit := TSYSTEMUnit.Create(Self, SysFileName, SysSource);
      SYSUnit.InitSystemUnit;
    end;
  except
    on e: exception do
      raise Exception.Create('Internal ERROR: ' + e.Message);
  end;
end;

procedure TNPPackage.AddUnit(aUnit, BeforeUnit: TASTModule);
var
  i: Integer;
begin
  if FUnits.IndexOf(aUnit) > -1 then
    raise ECompilerInternalError.CreateFmt(sUnitAlreadyExistFmt, [AnsiUpperCase(TNPUnit(aUnit).Name)]);

  TNPUnit(aUnit).UnitID := FUnits.Count;

  for i := 0 to FUnits.Count - 1 do
  begin
    if FUnits[i] = BeforeUnit then
    begin
      FUnits.Insert(i, aUnit);
      Exit;
    end;
  end;
  FUnits.Add(aUnit);
end;

class function TNPPackage.StrListCompare(const Left, Right: TStrConstKey): NativeInt;
begin
  Result := Ord(Left.StrTypeID) - Ord(Right.StrTypeID);
  if Result = 0 then
    Result := AnsiCompareStr(Left.StrValue, Right.StrValue);
end;

function TNPPackage.UsesUnit(const UnitName: string; AfterUnit: TASTModule): TASTModule;
var
  i: Integer;
  SUnitName: string;
begin
  // поиск в текущем пакете
  for i := 0 to FUnits.Count - 1 do
  begin
    SUnitName := TNPUnit(FUnits[i]).Name;
    if AnsiCompareText(SUnitName, UnitName) = 0 then
      Exit(FUnits[i]);
  end;
  // ищем на файловой системе
  SUnitName := FindUnitFile(UnitName);
  if SUnitName <> '' then
  begin
    Result := OpenUnit(SUnitName);
    AddUnit(Result, AfterUnit);
  end else
    Result := nil;
end;

constructor TNPPackage.Create(const Name: string; RTTICharset: TRTTICharset);
begin
  FName := Name;
  FUnits := TUnits.Create;
  FDefines := TDefines.Create();
  FOptions := TPackageOptions.Create(nil);
  FUnitSearchPathes := TStringList.Create;
  FMessages := TCompilerMessages.Create;
  FRTTICharset := RTTICharset;
  FStrLiterals := TStrLiterals.Create(StrListCompare);
  GetStringConstant(''); // занимаем нулевой индекс за пустой строкой
  SetTarget('ANY');
end;

procedure TNPPackage.AddUnit(const Source: string);
var
  UN: TNPUnit;
begin
  UN := TNPUnit.Create(Self, Source);
  AddUnit(UN, nil);
end;

procedure TNPPackage.AddUnitSearchPath(const Path: string; IncludeSubDirectories: Boolean);
begin
  FUnitSearchPathes.AddObject(Path, TObject(IncludeSubDirectories));
end;

procedure TNPPackage.Clear;
var
  i: Integer;
begin
  for i := 0 to FUnits.Count - 1 do
    if TNPUnit(FUnits[i]) <> SYSUnit then
      FUnits[i].Free;
  FUnits.Clear;
  FStrLiterals.Free;
  FStrLiterals := TStrLiterals.Create(StrListCompare);
  GetStringConstant(''); // занимаем нулевой индекс за пустой строкой
end;

function TNPPackage.Compile: TCompilerResult;
var
  i: Integer;
begin
  Result := CompileSuccess;
  // компиляция модулей
  for i := 0 to FUnits.Count - 1 do
  begin
    var UN := FUnits[i];
    Result := TNPUnit(UN).Compile;
    if Result = CompileFail then
      Exit;
  end;
end;

destructor TNPPackage.Destroy;
begin
  Clear;
  FUnits.Free;
  FDefines.Free;
  FStrLiterals.Free;
  FUnitSearchPathes.Free;
  FOptions.Free;
  inherited;
end;

function TNPPackage.OpenUnit(const UnitName: string): TASTModule;
var
  Stream: TStringStream;
  UnitClass: TASTUnitClass;
begin
  Stream := TStringStream.Create('');
  try
    StringStreamLoadFromFile(UnitName, Stream);
    UnitClass := GetUnitClass();
    Result := UnitClass.Create(Self, UnitName, Stream.DataString);
  finally
    Stream.Free;
  end;
end;

procedure TNPPackage.PrepareStrLiterals;
var
  Idx: Integer;
  Constant: TIDConstant;
  Node: TStrLiterals.PAVLNode;
begin
  {переупорядочиваем литералы по индексам}
  Node := FStrLiterals.First;
  while Assigned(Node) do begin
    Idx := Node.Data.Index;
    Constant := Node.Data.Decl;
    if Assigned(Constant) then
      Constant.Index := Idx;
    Node := FStrLiterals.Next(Node);
  end;
end;

function TNPPackage.RefCount: Integer;
begin
  Result := FRefCount;
end;

function FindInSubDirs(const RootDir, UnitName: string): string;
var
  i: Integer;
  SearchPath, SearchUnitName: string;
  Dirs: TStringDynArray;
begin
  Dirs := TDirectory.GetDirectories(RootDir);
  for i := 0 to Length(Dirs) - 1 do
  begin
    SearchPath := IncludeTrailingPathDelimiter(Dirs[i]);
    SearchUnitName := SearchPath + UnitName + '.pas';
    if FileExists(SearchUnitName) then
      Exit(SearchUnitName);

    Result := FindInSubDirs(SearchPath, UnitName);
    if Result <> '' then
      Exit;
  end;
  Result := '';
end;

function TNPPackage.FindUnitFile(const UnitName: string): string;
var
  i: Integer;
  SearchPath, SearchUnitName: string;

begin
  // поиск по файловой системе
  for i := 0 to FUnitSearchPathes.Count - 1 do
  begin
    SearchPath := IncludeTrailingPathDelimiter(FUnitSearchPathes[i]);
    SearchUnitName := SearchPath + UnitName + '.pas';
    if FileExists(SearchUnitName) then
      Exit(SearchUnitName);

    // поиск в поддиректориях
    if Boolean(FUnitSearchPathes.Objects[i]) then
    begin
      Result := FindInSubDirs(SearchPath, UnitName);
      if Result <> '' then
        Exit;
    end;
  end;
  Result := '';
end;

procedure TNPPackage.SaveStrLiterals(Stream: TStream);
begin

end;

procedure TNPPackage.SaveToStream(Stream: TStream);
begin

end;

procedure TNPPackage.SetIncludeDebugInfo(const Value: Boolean);
begin
  FIncludeDebugInfo := Value;
end;

procedure TNPPackage.SetRTTICharset(const Value: TRTTICharset);
begin
  FRTTICharset := Value;
end;

procedure TNPPackage.SetTarget(const Value: string);
begin
  FTargetName := Value;
  FTarget := FindTarget(Value);
  if not Assigned(FTarget) then
    AbortWorkInternal('Unknwon target: ' + Value);
end;

function TNPPackage.CompileInterfacesOnly: TCompilerResult;
var
  i: Integer;
begin
  Result := CompileSuccess;
  // компиляция модулей
  for i := 0 to FUnits.Count - 1 do
  begin
    Result := TNPUnit(FUnits[i]).CompileIntfOnly;
    if Result = CompileFail then
      Exit;
  end;
end;

function TNPPackage.GetPointerSize: Integer;
begin
  Result := FTarget.PointerSize;
end;

function TNPPackage.GetNativeIntSize: Integer;
begin
  Result := FTarget.NativeIntSize;
end;

end.

