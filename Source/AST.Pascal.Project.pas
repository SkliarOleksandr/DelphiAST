unit AST.Pascal.Project;

interface

uses System.SysUtils, System.Classes, System.Types, Generics.Collections, System.IOUtils,
     AVL,
     AST.Intf,
     AST.Delphi.DataTypes,
     AST.Parser.Options,
     AST.Targets,
     AST.Delphi.Intf,
     AST.Delphi.Classes,
     AST.Parser.Messages,
     AST.Parser.Utils,
     AST.Pascal.Intf,
     AST.Classes,
     AST.Lexer;

type
  TUnits = TList<IASTPascalUnit>;
  TUnitsDict = TDictionary<{name} string, {module} IASTPascalUnit>;
  TTypes = TList<TIDType>;
  TEnumDeclProc = procedure (Module: TASTModule; Decl: TASTDeclaration);

  TPascalProjectSettings = class(TASTProjectSettings, IPascalProjectSettings)

  end;

  TPascalProject = class(TASTProject, IASTProject, IASTPascalProject)
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
    fUnits: TUnits;
    fAllUnits: TUnits; // explicit & implicit units
    fImplicitUnits: TUnitsDict;
    fTarget: TASTTargetClass;
    fDefines: TDefines;
    fStrLiterals: TStrLiterals;
    fIncludeDebugInfo: Boolean;
    fUnitSearchPathes: TStringList;
    fMessages: ICompilerMessages;
    fRTTICharset: TRTTICharset;
    fOptions: TPackageOptions;
    fTotalLinesParsed: Integer;
    fTotalUnitsParsed: Integer;
    fTotalUnitsIntfOnlyParsed: Integer;
    fCompileAll: Boolean;
    fUnitScopeNames: TStrings;
    fParseSystemUnit: Boolean;
    FProjectFileName: string;
    FInProgress: Boolean;
    function GetIncludeDebugInfo: Boolean;
    function OpenUnit(const AUnitName: string): IASTPascalUnit;
    function RefCount: Integer;
    function GetRTTICharset: TRTTICharset;
    function GetUnitsCount: Integer;
    function GetUnit(Index: Integer): IASTPascalUnit; overload;
    function GetUnit(const AUnitName: string): TObject; overload;
    function GetAllUnitsCount: Integer;
    function GetAllUnit(AIndex: Integer): IASTPascalUnit;
    function GetSearchPathes: TStrings;
    function GetOptions: TPackageOptions;
    function GetTarget: TASTTargetClass;
    function GetDefines: TDefines;
    function GetSysUnit: IASTPascalUnit;
    function GetCompileAll: Boolean;
    function GetUnitScopeNames: string;
    function GetParseSystemUnit: Boolean;
    function GetRootPath: string;
    procedure SetIncludeDebugInfo(const Value: Boolean);
    procedure SetRTTICharset(const Value: TRTTICharset);
    procedure SetTarget(const Value: TASTTargetClass);
    procedure SetCompileAll(const Value: Boolean);
    procedure SetUnitScopeNames(const Value: string);
    procedure SetParseSystemUnit(AValue: Boolean);
    procedure SetProjectFileName(const Value: string);
    class function StrListCompare(const Left, Right: TStrConstKey): NativeInt; static;
    function GetProjectFileName: string;
  protected
    fSysUnit: IASTPascalUnit;
    function GetUnitClass: TASTUnitClass; override;
    function GetSystemUnitClass: TASTUnitClass; virtual; abstract;
    function GetSystemUnitFileName: string; virtual;
    function GetPointerSize: Integer; override;
    function GetNativeIntSize: Integer; override;
    function GetVariantSize: Integer;  override;
    procedure DoBeforeCompileUnit(AUnit: TASTModule); virtual;
    procedure DoFinishCompileUnit(AUnit: TASTModule; AIntfOnly: Boolean); virtual;
  public
    constructor Create(const Name: string); override;
    constructor CreateExisting(const AFileName: string);
    destructor Destroy; override;
    ////////////////////////////////////////
    procedure SaveToStream(Stream: TStream);
    procedure PrepareStrLiterals;
    procedure SaveStrLiterals(Stream: TStream);
    procedure AddUnit(const aUnit, BeforeUnit: IASTPascalUnit); overload;
    procedure AddUnit(const AFileName: string; const AModule: IASTModule); overload;
    procedure AddUnitSource(const Source: string);
    procedure AddUnitSearchPath(const APath: string; AIncludeSubDirs: Boolean);
    procedure RemoveUnitSearchPath(const APath: string);
    procedure Clear(AClearImplicitUnits: Boolean); virtual;
    function GetTotalLinesParsed: Integer; override;
    function GetTotalUnitsParsed: Integer; override;
    function GetTotalUnitsIntfOnlyParsed: Integer; override;
    property IncludeDebugInfo: Boolean read GetIncludeDebugInfo write SetIncludeDebugInfo;
    property RTTICharset: TRTTICharset read GetRTTICharset write SetRTTICharset;
    property Units: TUnits read FUnits;
    property AllUnits: TUnits read fAllUnits;
    property SysUnit: IASTPascalUnit read fSysUnit;
    property Options: TPackageOptions read GetOptions;
    property TotalLinesParsed: Integer read GetTotalLinesParsed;
    property TotalUnitsParsed: Integer read GetTotalUnitsParsed;
    property ProjectFileName: string read GetProjectFileName write SetProjectFileName;
    property RootPath: string read GetRootPath;
    function GetStringConstant(const Value: string): Integer; overload;
    function GetStringConstant(const StrConst: TIDStringConstant): Integer; overload;
    function FindUnitFile(const AUnitName: string; const AModule: IASTModule; const AFileExt: string = ''): string;
    function FindParsedUnit(const AUnitName: string): IASTPascalUnit;
    function UsesUnit(const AUnitName, AUnitPath: string; const AModule: IASTModule): IASTPascalUnit;
    function FindType(const AUnitName, ATypeName: string): TASTDeclaration;
    function GetMessages: ICompilerMessages;
    function InPogress: Boolean; override;
    function Compile: TCompilerResult; virtual;
    function CompileInterfacesOnly: TCompilerResult; virtual;
    procedure EnumDeclarations(const AEnumProc: TEnumASTDeclProc; AUnitScope: TUnitScopeKind);
    procedure PutMessage(const AMessage: IASTParserMessage); override;
    procedure PutMessage(const AModule: IASTModule; AMsgType: TCompilerMessageType; const AMessage: string;
                         const ATextPostition: TTextPosition; ACritical: Boolean); override;
    property UnitScopeNames: string read GetUnitScopeNames write SetUnitScopeNames; // semicolon delimited
  end;

implementation

uses
  AST.Parser.Errors,
  AST.Delphi.Errors,
  AST.Pascal.Parser;

function TPascalProject.GetOptions: TPackageOptions;
begin
  Result := FOptions;
end;

function TPascalProject.GetAllUnitsCount: Integer;
begin
  Result := fAllUnits.Count;
end;

function TPascalProject.GetAllUnit(AIndex: Integer): IASTPascalUnit;
begin
  Result := fAllUnits[AIndex];
end;

function TPascalProject.GetCompileAll: Boolean;
begin
  Result := fCompileAll;
end;

function TPascalProject.GetDefines: TDefines;
begin
  Result := FDefines;
end;

function TPascalProject.GetIncludeDebugInfo: Boolean;
begin
  Result := FIncludeDebugInfo;
end;

function TPascalProject.GetMessages: ICompilerMessages;
begin
  Result := fMessages;
end;

function TPascalProject.GetRootPath: string;
begin
  Result := ExtractFilePath(ProjectFileName);
end;

function TPascalProject.GetRTTICharset: TRTTICharset;
begin
  Result := FRTTICharset;
end;

function TPascalProject.GetSearchPathes: TStrings;
begin
  Result := FUnitSearchPathes;
end;

function TPascalProject.GetStringConstant(const Value: string): Integer;
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

function TPascalProject.GetStringConstant(const StrConst: TIDStringConstant): Integer;
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

function TPascalProject.GetSystemUnitFileName: string;
begin
  Result := 'System.pas';
end;

function TPascalProject.GetSysUnit: IASTPascalUnit;
begin
  if not Assigned(fSysUnit) then
  begin
    var LSystemName := GetSystemUnitFileName;
    var LSystemUnitFileName := FindUnitFile(LSystemName, {AModule:} nil);
    fSysUnit := GetSystemUnitClass.Create(Self, LSystemUnitFileName) as IASTPascalUnit;
    if FileExists(LSystemUnitFileName) and fParseSystemUnit then
      fSysUnit.Compile(not fCompileAll);

    fAllUnits.Add(fSysUnit);
    fImplicitUnits.Add(LowerCase(TPath.GetFileNameWithoutExtension(LSystemName)), fSysUnit);
  end;
  Result := fSysUnit;
end;

function TPascalProject.GetTarget: TASTTargetClass;
begin
  Result := fTarget;
end;

function TPascalProject.GetTotalLinesParsed: Integer;
begin
  Result := fTotalLinesParsed;
end;

function TPascalProject.GetTotalUnitsIntfOnlyParsed: Integer;
begin
  Result := fTotalUnitsIntfOnlyParsed;
end;

function TPascalProject.GetTotalUnitsParsed: Integer;
begin
  Result := fTotalUnitsParsed;
end;

function TPascalProject.GetUnit(Index: Integer): IASTPascalUnit;
begin
  Result := FUnits[Index];
end;

function TPascalProject.GetUnit(const AUnitName: string): TObject;
var
  i: Integer;
begin
  for i := 0 to FUnits.Count - 1 do
  begin
    Result := TObject(FUnits[i]);
    if TPascalUnit(Result).Name = AUnitName then
      Exit;
  end;
  Result := nil;
end;

function TPascalProject.GetUnitClass: TASTUnitClass;
begin
  Result := TPascalUnit;
end;

function TPascalProject.GetUnitScopeNames: string;
begin
  Result := fUnitScopeNames.DelimitedText;
end;

function TPascalProject.GetUnitsCount: Integer;
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

procedure TPascalProject.AddUnit(const aUnit, BeforeUnit: IASTPascalUnit);
var
  i: Integer;
begin
  if FUnits.IndexOf(aUnit) > -1 then
    raise ECompilerInternalError.CreateFmt(sUnitAlreadyExistFmt, [AnsiUpperCase(TPascalUnit(aUnit).Name)]);

  TPascalUnit(aUnit).UnitID := FUnits.Count;

  for i := 0 to FUnits.Count - 1 do
  begin
    if FUnits[i] = BeforeUnit then
    begin
      FUnits.Insert(i, aUnit);
      Exit;
    end;
  end;
  FUnits.Add(aUnit);
  fAllUnits.Add(aUnit);
end;

class function TPascalProject.StrListCompare(const Left, Right: TStrConstKey): NativeInt;
begin
  Result := Ord(Left.StrTypeID) - Ord(Right.StrTypeID);
  if Result = 0 then
    Result := AnsiCompareStr(Left.StrValue, Right.StrValue);
end;

function TPascalProject.FindParsedUnit(const AUnitName: string): IASTPascalUnit;
begin
  for var LIndex := 0 to FUnits.Count - 1 do
  begin
    var LUnitName := TPascalUnit(FUnits[LIndex]).Name;
    if AnsiCompareText(LUnitName, AUnitName) = 0 then
      Exit(FUnits[LIndex]);
  end;

  if not fImplicitUnits.TryGetValue(LowerCase(AUnitName), {out} Result) then
    raise Exception.CreateFmt('Unit ''%s'' not found', [AUnitName]);
end;

function TPascalProject.UsesUnit(const AUnitName, AUnitPath: string; const AModule: IASTModule): IASTPascalUnit;
var
  i: Integer;
  SUnitName: string;
begin
  // use explicit path to find the unit
  if AUnitPath <> '' then
  begin
    var LFullPath := '';
    if IsRelativePath(AUnitPath) then
      LFullPath := TPath.Combine(RootPath, AUnitPath)
    else
      LFullPath := AUnitPath;

    if FileExists(LFullPath) then
    begin
      Result := OpenUnit(LFullPath);
      fAllUnits.Add(Result);
    end else
      Exit(nil);
  end;

  // search in the project itself (explicit units)
  for i := 0 to FUnits.Count - 1 do
  begin
    SUnitName := TPascalUnit(FUnits[i]).Name;
    if AnsiCompareText(SUnitName, AUnitName) = 0 then
      Exit(FUnits[i]);
  end;

  // search in the project itself (implicit units)
  if fImplicitUnits.TryGetValue(LowerCase(AUnitName), {out} Result) then
    Exit;

  // search using search paths
  SUnitName := FindUnitFile(AUnitName, AModule, '.pas');
  if SUnitName <> '' then
  begin
    Result := OpenUnit(SUnitName);
    // add a unit to implicit unit list
    if not fImplicitUnits.TryAdd(LowerCase(AUnitName), Result) then
      raise ECompilerInternalError.CreateFmt(sUnitAlreadyExistFmt, [AUnitName]);

    fAllUnits.Add(Result);
  end else
    Result := nil;
end;

constructor TPascalProject.Create(const Name: string);
begin
  inherited;
  fUnits := TUnits.Create;
  fAllUnits := TUnits.Create;
  fImplicitUnits := TUnitsDict.Create;
  FDefines := TDefines.Create();
  FOptions := TPackageOptions.Create(nil);
  FUnitSearchPathes := TStringList.Create;
  FMessages := TCompilerMessages.Create;
  FRTTICharset := RTTICharset;
  FStrLiterals := TStrLiterals.Create(StrListCompare);
  fUnitScopeNames := TStringList.Create({QuoteChar:} '''', {Delimiter:} ';');
end;

constructor TPascalProject.CreateExisting(const AFileName: string);
begin
  var LProjectName := TPath.GetFileNameWithoutExtension(AFileName);
  Create(LProjectName);
  FProjectFileName := AFileName;
end;

procedure TPascalProject.AddUnitSource(const Source: string);
var
  UN: TPascalUnit;
begin
  UN := TPascalUnit.Create(Self, Source);
  AddUnit(UN, nil);
end;

procedure TPascalProject.AddUnit(const AFileName: string; const AModule: IASTModule);
begin
  var LFullFileName := FindUnitFile(AFileName, AModule);
  if LFullFileName <> '' then
    AddUnit(GetUnitClass().CreateFromFile(Self, LFullFileName) as IASTPascalUnit, nil)
  else
    raise Exception.CreateFmt('%s file cannot be found at the specified paths', [AFileName]);
end;

procedure TPascalProject.AddUnitSearchPath(const APath: string; AIncludeSubDirs: Boolean);
begin
  var LStrings := TStringList.Create({QuoteChar:} '"', {Delimiter:} ';', [soStrictDelimiter]);
  try
    LStrings.DelimitedText := APath;
    for var LPath in LStrings do
      if FUnitSearchPathes.IndexOf(LPath) < 0 then
        FUnitSearchPathes.AddObject(LPath, TObject(AIncludeSubDirs));
  finally
    LStrings.Free;
  end;
end;

procedure TPascalProject.Clear(AClearImplicitUnits: Boolean);
var
  i: Integer;
begin
  for i := 0 to FUnits.Count - 1 do
  begin
    // delete first explicit units from the list
    fAllUnits.Delete(0);
  end;

  FUnits.Clear;

  if AClearImplicitUnits then
  begin
    fImplicitUnits.Clear;
    fAllUnits.Clear;
    fSysUnit := nil;
  end;

  FStrLiterals.Free;
  FStrLiterals := TStrLiterals.Create(StrListCompare);

  fMessages.Clear;
end;

function TPascalProject.Compile: TCompilerResult;
var
  i: Integer;
begin
  FInProgress := True;
  Result := CompileInProgress;
  fTotalLinesParsed := 0;
  try
    // compile explicit project units ("uses units" will be compiled for interface only)
    for i := 0 to FUnits.Count - 1 do
    begin
      var UN := TPascalUnit(FUnits[i]);
      Result := UN.Compile({ACompileIntfOnly:} False);
      if Result = CompileFail then
        Exit;
    end;

    // compile all units (compiling implementations)
    if fCompileAll then
    begin
      // parse impementations for implicit units
      for var LPair in fImplicitUnits do
      begin
        var LUnit := LPair.Value as TPascalUnit;
        if LUnit.UnitState = UnitIntfCompiled then
        begin
          Result := LUnit.Compile({ACompileIntfOnly:} False);
          if (Result = CompileFail) and StopCompileIfError then
            Exit;
        end;
      end;
    end;
  finally
    for i := 0 to FUnits.Count - 1 do
      Inc(fTotalLinesParsed, FUnits[i].TotalLinesParsed);

    for var LPair in fImplicitUnits do
      Inc(fTotalLinesParsed, LPair.Value.TotalLinesParsed);

    FInProgress := False;
  end;
end;

destructor TPascalProject.Destroy;
begin
  Clear({AClearImplicitUnits:} True);
  FUnits.Free;
  fAllUnits.Free;
  FImplicitUnits.Free;
  FDefines.Free;
  FStrLiterals.Free;
  FUnitSearchPathes.Free;
  FOptions.Free;
  inherited;
end;

procedure TPascalProject.DoBeforeCompileUnit(AUnit: TASTModule);
begin
  // do nothing
end;

procedure TPascalProject.DoFinishCompileUnit(AUnit: TASTModule; AIntfOnly: Boolean);
begin
  if AIntfOnly then
    Inc(fTotalUnitsIntfOnlyParsed)
  else begin
    Inc(fTotalUnitsParsed);
    Dec(fTotalUnitsIntfOnlyParsed);
  end;
end;

procedure TPascalProject.EnumDeclarations(const AEnumProc: TEnumASTDeclProc; AUnitScope: TUnitScopeKind);
begin
  for var LIndex := 0 to fAllUnits.Count - 1 do
    fAllUnits[LIndex].EnumDeclarations(AEnumProc, AUnitScope);
end;

function TPascalProject.OpenUnit(const AUnitName: string): IASTPascalUnit;
begin
  var LStrings := TStringList.Create();
  try
    LStrings.LoadFromFile(AUnitName);
    var LUnitClass := GetUnitClass();
    Result := LUnitClass.Create(Self, AUnitName, LStrings.Text) as IASTPascalUnit;
  finally
    LStrings.Free;
  end;
end;

procedure TPascalProject.PrepareStrLiterals;
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

procedure TPascalProject.PutMessage(const AMessage: IASTParserMessage);
begin
  fMessages.Add(AMessage);
end;

procedure TPascalProject.PutMessage(const AModule: IASTModule; AMsgType: TCompilerMessageType; const AMessage: string;
                         const ATextPostition: TTextPosition; ACritical: Boolean);
begin
  fMessages.Add(TCompilerMessage.Create(AModule, AMsgType, AMessage, ATextPostition, ACritical));
end;

function TPascalProject.RefCount: Integer;
begin
  Result := FRefCount;
end;

procedure TPascalProject.RemoveUnitSearchPath(const APath: string);
begin
  var LIndex := FUnitSearchPathes.IndexOf(APath);
  if LIndex >= 0 then
    FUnitSearchPathes.Delete(LIndex);
end;

function TPascalProject.FindType(const AUnitName, ATypeName: string): TASTDeclaration;
begin
  var LUnit := FindParsedUnit(AUnitName) as TPascalUnit;
  Result := LUnit.GetPublicType(ATypeName);
end;

function TPascalProject.FindUnitFile(const AUnitName: string; const AModule: IASTModule; const AFileExt: string): string;

  function DoFindFile(const APath, AFileName: string): string;
  begin
    {TODO: improve search performance using in-memory dictionary of all accessible files}
    Result := TPath.Combine(APath, AFileName);
    if not FileExists(Result) then
    begin
      for var LUnitScope in fUnitScopeNames do
      begin
        Result := APath + LUnitScope + '.' + AFileName;
        if FileExists(Result) then
          Exit;
      end;
      Result := '';
    end;
  end;

  function FindInSubDirs(const ARootDir, AUnitName: string): string;
  begin
    {TODO: improve search performance using in-memory dictionary of all accessible files}
    for var LDir in TDirectory.GetDirectories(ARootDir) do
    begin
      var LPath := IncludeTrailingPathDelimiter(LDir);
      Result := DoFindFile(LPath, AUnitName);
      if Result = '' then
        Result := FindInSubDirs(LPath, AUnitName);
      if Result <> '' then
        Exit;
    end;
    Result := '';
  end;

begin
  if TPath.IsPathRooted(AUnitName) and FileExists(AUnitName) then
    Exit(AUnitName);

  var LFileName := AUnitName;
  if AFileExt <> '' then
    LFileName := LFileName + AFileExt;

  // search in the same dir the unit exists
  if Assigned(AModule) and FileExists(AModule.ModulePath + LFileName) then
    Exit(AModule.ModulePath + LFileName);

  // search file in the project root dir
  if FileExists(RootPath + LFileName) then
    Exit(RootPath + LFileName);

  // search using defined paths (in reverse order)
  for var LIndex := FUnitSearchPathes.Count - 1 downto 0 do
  begin
    var LPath := IncludeTrailingPathDelimiter(FUnitSearchPathes[LIndex]);
    if IsRelativePath(LPath) then
      LPath := TPath.GetFullPath(TPath.Combine(RootPath, LPath));

    Result := DoFindFile(LPath, LFileName);
    // if not found search in the subdirs (if specified)
    if Result = '' then
      if Boolean(FUnitSearchPathes.Objects[LIndex]) then
        Result := FindInSubDirs(LPath, LFileName);

    if Result <> '' then
      Exit;
  end;
  Result := '';
end;

procedure TPascalProject.SaveStrLiterals(Stream: TStream);
begin

end;

procedure TPascalProject.SaveToStream(Stream: TStream);
begin

end;

procedure TPascalProject.SetCompileAll(const Value: Boolean);
begin
  fCompileAll := Value;
end;

procedure TPascalProject.SetIncludeDebugInfo(const Value: Boolean);
begin
  FIncludeDebugInfo := Value;
end;

procedure TPascalProject.SetParseSystemUnit(AValue: Boolean);
begin
  fParseSystemUnit := AValue;
end;

procedure TPascalProject.SetProjectFileName(const Value: string);
begin
  FProjectFileName := Value;
  Name := ExtractFileName(Value);
end;

procedure TPascalProject.SetRTTICharset(const Value: TRTTICharset);
begin
  FRTTICharset := Value;
end;

procedure TPascalProject.SetTarget(const Value: TASTTargetClass);
begin
  FTarget := Value;
end;

procedure TPascalProject.SetUnitScopeNames(const Value: string);
begin
  fUnitScopeNames.DelimitedText := Value;
end;

function TPascalProject.CompileInterfacesOnly: TCompilerResult;
var
  i: Integer;
begin
  Result := CompileSuccess;
  // компиляция модулей
  for i := 0 to FUnits.Count - 1 do
  begin
    Result := TPascalUnit(FUnits[i]).CompileIntfOnly;
    if Result = CompileFail then
      Exit;
  end;
end;

function TPascalProject.GetParseSystemUnit: Boolean;
begin
  Result := fParseSystemUnit;
end;

function TPascalProject.GetPointerSize: Integer;
begin
  Result := FTarget.PointerSize;
end;

function TPascalProject.GetProjectFileName: string;
begin
  Result := FProjectFileName;
end;

function TPascalProject.GetNativeIntSize: Integer;
begin
  Result := FTarget.NativeIntSize;
end;

function TPascalProject.GetVariantSize: Integer;
begin
  Result := FTarget.VariantSize;
end;

function TPascalProject.InPogress: Boolean;
begin
  Result := FInProgress;
end;

end.

