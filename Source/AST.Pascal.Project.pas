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
     AST.Classes, AST.Lexer;

type
  TUnits = TList<TASTModule>;
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
    fName: string;
    fUnits: TUnits;
    fTarget: TASTTargetClass;
    fDefines: TDefines;
    fStrLiterals: TStrLiterals;
    fIncludeDebugInfo: Boolean;
    fUnitSearchPathes: TStrings;
    fMessages: ICompilerMessages;
    fRTTICharset: TRTTICharset;
    fOptions: TPackageOptions;
    fTotalLinesParsed: Integer;
    fTotalUnitsParsed: Integer;
    fTotalUnitsIntfOnlyParsed: Integer;
    fStopCompileIfError: Boolean;
    fCompileAll: Boolean;
    fUnitScopeNames: TStrings;
    fParseSystemUnit: Boolean;
    function GetIncludeDebugInfo: Boolean;
    function OpenUnit(const UnitName: string): TASTModule;
    function RefCount: Integer;
    function GetRTTICharset: TRTTICharset;
    function GetUnitsCount: Integer;
    function GetUnit(Index: Integer): TASTModule; overload;
    function GetUnit(const UnitName: string): TObject; overload;
    function GetSearchPathes: TStrings;
    function GetOptions: TPackageOptions;
    function GetTarget: TASTTargetClass;
    function GetDefines: TDefines;
    function GetSysUnit: TASTModule;
    function GetStopCompileIfError: Boolean;
    function GetCompileAll: Boolean;
    function GetUnitScopeNames: string;
    function GetParseSystemUnit: Boolean;
    procedure SetStopCompileIfError(const Value: Boolean);
    procedure SetIncludeDebugInfo(const Value: Boolean);
    procedure SetRTTICharset(const Value: TRTTICharset);
    procedure SetTarget(const Value: TASTTargetClass);
    procedure SetCompileAll(const Value: Boolean);
    procedure SetUnitScopeNames(const Value: string);
    procedure SetParseSystemUnit(AValue: Boolean);
    class function StrListCompare(const Left, Right: TStrConstKey): NativeInt; static;
  protected
    fSysUnit: TASTModule;
    function GetUnitClass: TASTUnitClass; override;
    function GetSystemUnitClass: TASTUnitClass; virtual; abstract;
    function GetSystemUnitFileName: string; virtual;
    function GetPointerSize: Integer; override;
    function GetNativeIntSize: Integer; override;
    function GetVariantSize: Integer;  override;
    procedure InitSystemUnit; virtual;
    procedure DoBeforeCompileUnit(AUnit: TASTModule); virtual;
    procedure DoFinishCompileUnit(AUnit: TASTModule; AIntfOnly: Boolean); virtual;
  public
    constructor Create(const Name: string); override;
    destructor Destroy; override;
    ////////////////////////////////////////
    procedure SaveToStream(Stream: TStream);
    procedure PrepareStrLiterals;
    procedure SaveStrLiterals(Stream: TStream);
    procedure AddUnit(aUnit, BeforeUnit: TASTModule); overload;
    procedure AddUnit(const AFileName: string); overload;
    procedure AddUnitSource(const Source: string);
    procedure AddUnitSearchPath(const APath: string; AIncludeSubDirs: Boolean);
    procedure Clear;
    function GetTotalLinesParsed: Integer; override;
    function GetTotalUnitsParsed: Integer; override;
    function GetTotalUnitsIntfOnlyParsed: Integer; override;
    property IncludeDebugInfo: Boolean read GetIncludeDebugInfo write SetIncludeDebugInfo;
    property RTTICharset: TRTTICharset read GetRTTICharset write SetRTTICharset;
    property Units: TUnits read FUnits;
    property SysUnit: TASTModule read fSysUnit;
    property Options: TPackageOptions read GetOptions;
    property TotalLinesParsed: Integer read fTotalLinesParsed;
    property TotalUnitsParsed: Integer read fTotalUnitsParsed;
    function GetStringConstant(const Value: string): Integer; overload;
    function GetStringConstant(const StrConst: TIDStringConstant): Integer; overload;
    function FindUnitFile(const AUnitName: string; const AFileExt: string = ''): string;
    function UsesUnit(const UnitName: string; AfterUnit: TASTModule): TASTModule;
    function GetMessages: ICompilerMessages;
    function Compile: TCompilerResult; virtual;
    function CompileInterfacesOnly: TCompilerResult; virtual;
    procedure EnumDeclarations(const AEnumProc: TEnumASTDeclProc; AUnitScope: TUnitScopeKind);
    procedure PutMessage(const Message: TCompilerMessage); overload;
    procedure PutMessage(MessageType: TCompilerMessageType; const MessageText: string); overload;
    procedure PutMessage(MessageType: TCompilerMessageType; const MessageText: string; const SourcePosition: TTextPosition); overload;
    property UnitScopeNames: string read GetUnitScopeNames write SetUnitScopeNames; // semicolon delimited
  end;

implementation

uses AST.Parser.Errors,
     AST.Delphi.Errors,
     AST.Pascal.Parser;

function TPascalProject.GetOptions: TPackageOptions;
begin
  Result := FOptions;
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

function TPascalProject.GetStopCompileIfError: Boolean;
begin
  Result := fStopCompileIfError;
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

function TPascalProject.GetSysUnit: TASTModule;
begin
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

function TPascalProject.GetUnit(Index: Integer): TASTModule;
begin
  Result := FUnits[Index];
end;

function TPascalProject.GetUnit(const UnitName: string): TObject;
var
  i: Integer;
begin
  for i := 0 to FUnits.Count - 1 do
  begin
    Result := FUnits[i];
    if TPascalUnit(Result).Name = UnitName then
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

procedure TPascalProject.InitSystemUnit;
var
  SysFileName, SysSource: string;
begin
  try
    if not Assigned(fSysUnit) then
    begin
      SysFileName := FindUnitFile('system.pas');
      if FileExists(SysFileName) and fParseSystemUnit then
      begin
        var AList := TStringList.Create;
        try
          AList.LoadFromFile(SysFileName);
          SysSource := AList.Text;
        finally
          AList.Free;
        end;
      end else
        SysSource := 'unit system; end.';

      fSysUnit := GetSystemUnitClass.Create(Self, SysFileName, SysSource);
      FUnits.Insert(0, fSysUnit);
    end;
  except
    on e: exception do
      AbortWorkInternal('Internal ERROR: ' + e.Message);
  end;
end;

procedure TPascalProject.AddUnit(aUnit, BeforeUnit: TASTModule);
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
end;

class function TPascalProject.StrListCompare(const Left, Right: TStrConstKey): NativeInt;
begin
  Result := Ord(Left.StrTypeID) - Ord(Right.StrTypeID);
  if Result = 0 then
    Result := AnsiCompareStr(Left.StrValue, Right.StrValue);
end;

function TPascalProject.UsesUnit(const UnitName: string; AfterUnit: TASTModule): TASTModule;
var
  i: Integer;
  SUnitName: string;
begin
  // поиск в текущем пакете
  for i := 0 to FUnits.Count - 1 do
  begin
    SUnitName := TPascalUnit(FUnits[i]).Name;
    if AnsiCompareText(SUnitName, UnitName) = 0 then
      Exit(FUnits[i]);
  end;
  // ищем на файловой системе
  SUnitName := FindUnitFile(UnitName, '.pas');
  if SUnitName <> '' then
  begin
    Result := OpenUnit(SUnitName);
    AddUnit(Result, AfterUnit);
  end else
    Result := nil;
end;

constructor TPascalProject.Create(const Name: string);
begin
  FName := Name;
  FUnits := TUnits.Create;
  FDefines := TDefines.Create();
  FOptions := TPackageOptions.Create(nil);
  FUnitSearchPathes := TStringList.Create;
  FMessages := TCompilerMessages.Create;
  FRTTICharset := RTTICharset;
  FStrLiterals := TStrLiterals.Create(StrListCompare);
  fUnitScopeNames := TStringList.Create({QuoteChar:} '''', {Delimiter:} ';');
end;

procedure TPascalProject.AddUnitSource(const Source: string);
var
  UN: TPascalUnit;
begin
  UN := TPascalUnit.Create(Self, Source);
  AddUnit(UN, nil);
end;

procedure TPascalProject.AddUnit(const AFileName: string);
begin
  var LFullFileName := FindUnitFile(AFileName);
  if LFullFileName <> '' then
    AddUnit(GetUnitClass().CreateFromFile(Self, LFullFileName), nil)
  else
    raise Exception.CreateFmt('%s file cannot be found at the specified paths', [AFileName]);
end;

procedure TPascalProject.AddUnitSearchPath(const APath: string; AIncludeSubDirs: Boolean);
begin
  if FUnitSearchPathes.IndexOf(APath) < 0 then
    FUnitSearchPathes.AddObject(APath, TObject(AIncludeSubDirs));
end;

procedure TPascalProject.Clear;
var
  i: Integer;
begin
  for i := 0 to FUnits.Count - 1 do
    FUnits[i].Free;
  FUnits.Clear;
  FStrLiterals.Free;
  FStrLiterals := TStrLiterals.Create(StrListCompare);
end;

function TPascalProject.Compile: TCompilerResult;
var
  i: Integer;
begin
  Result := CompileInProgress;
  // init system first
  try
    InitSystemUnit;
  except
    on e: ECompilerAbort do begin
      PutMessage(ECompilerAbort(e).CompilerMessage^);
      Exit(CompileFail);
    end;
    on e: Exception do begin
      PutMessage(cmtInteranlError, e.Message);
      Exit(CompileFail);
    end;
  end;

  fTotalLinesParsed := 0;
  try
    // compile explicit project units ("uses units" will be compiled for interface only)
    for i := 0 to FUnits.Count - 1 do
    begin
      var UN := TPascalUnit(FUnits[i]);
      Result := UN.Compile({ACompileIntfOnly:} False);
      Inc(fTotalLinesParsed, UN.TotalLinesParsed);
      if Result = CompileFail then
        Exit;
    end;

    // compile all units (compiling implementations)
    if fCompileAll then
    begin
      for i := 0 to FUnits.Count - 1 do
      begin
        var AUN := TPascalUnit(FUnits[i]);
        if AUN.UnitState = UnitIntfCompiled then
        begin
          Dec(fTotalLinesParsed, AUN.TotalLinesParsed); // - intf lines
          Result := AUN.Compile({ACompileIntfOnly:} False);
          Inc(fTotalLinesParsed, AUN.TotalLinesParsed); // + all lines
          if (Result = CompileFail) and fStopCompileIfError then
            Exit;
        end;
      end;
    end;
  finally
    for i := 0 to FUnits.Count - 1 do
      Inc(fTotalLinesParsed, FUnits[i].TotalLinesParsed);
  end;
end;

destructor TPascalProject.Destroy;
begin
  Clear;
  FUnits.Free;
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
  Inc(fTotalUnitsParsed);
  if AIntfOnly then
    Inc(fTotalUnitsIntfOnlyParsed);
  // do nothing
end;

procedure TPascalProject.EnumDeclarations(const AEnumProc: TEnumASTDeclProc; AUnitScope: TUnitScopeKind);
begin
  for var LIndex := 0 to FUnits.Count - 1 do
    FUnits[LIndex].EnumDeclarations(AEnumProc, AUnitScope);
end;

function TPascalProject.OpenUnit(const UnitName: string): TASTModule;
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

procedure TPascalProject.PutMessage(const Message: TCompilerMessage);
begin
  fMessages.Add(Message);
end;

procedure TPascalProject.PutMessage(MessageType: TCompilerMessageType; const MessageText: string);
begin
  fMessages.Add(TCompilerMessage.Create(Self, MessageType, MessageText, TTextPosition.Empty));
end;

procedure TPascalProject.PutMessage(MessageType: TCompilerMessageType; const MessageText: string;
  const SourcePosition: TTextPosition);
begin
  fMessages.Add(TCompilerMessage.Create(Self, MessageType, MessageText, SourcePosition));
end;

function TPascalProject.RefCount: Integer;
begin
  Result := FRefCount;
end;

function TPascalProject.FindUnitFile(const AUnitName: string; const AFileExt: string): string;

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
  var LFileName := AUnitName;
  if AFileExt <> '' then
    LFileName := LFileName + AFileExt;

  // search using defined paths (in reverse order)
  for var LIndex := FUnitSearchPathes.Count - 1 downto 0 do
  begin
    var LPath := IncludeTrailingPathDelimiter(FUnitSearchPathes[LIndex]);
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

procedure TPascalProject.SetRTTICharset(const Value: TRTTICharset);
begin
  FRTTICharset := Value;
end;

procedure TPascalProject.SetStopCompileIfError(const Value: Boolean);
begin
  fStopCompileIfError := Value;
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

function TPascalProject.GetNativeIntSize: Integer;
begin
  Result := FTarget.NativeIntSize;
end;

function TPascalProject.GetVariantSize: Integer;
begin
  Result := FTarget.VariantSize;
end;

end.

