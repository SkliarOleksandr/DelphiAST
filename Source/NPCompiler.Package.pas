unit NPCompiler.Package;

interface

uses System.SysUtils, System.Classes, System.Types, Generics.Collections, System.IOUtils,
     AVL, NPCompiler.DataTypes, NPCompiler.Classes, NPCompiler.Options, NPLCompiler.Targets,
     NPCompiler.Intf, NPCompiler.Utils, OPCompiler, SystemUnit,
     AST.Classes,
     NPCompiler.Evaluater;

type
  TUnits = TList<TObject>;
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
    FEvaluater: INPCEvaluater;
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
    procedure AddUnit(aUnit, BeforeUnit: TObject); overload;
    procedure AddUnit(const Source: string); overload;
    procedure AddUnitSearchPath(const Path: string; IncludeSubDirectories: Boolean);
    procedure Clear;
    property IncludeDebugInfo: Boolean read GetIncludeDebugInfo write SetIncludeDebugInfo;
    property RTTICharset: TRTTICharset read GetRTTICharset write SetRTTICharset;
    property Units: TUnits read FUnits;
    function GetStringConstant(const Value: string): Integer; overload;
    function GetStringConstant(const StrConst: TIDStringConstant): Integer; overload;
    function FindUnitFile(const UnitName: string): string;
    function UsesUnit(const UnitName: string; AfterUnit: TObject): TObject;
    function GetMessages: ICompilerMessages;
    function Compile: TCompilerResult; virtual;
    function CompileInterfacesOnly: TCompilerResult; virtual;
    property Evaluater: INPCEvaluater read FEvaluater write FEvaluater;
  end;

implementation

uses NPCompiler.Messages;

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
  s, SysSource: string;
begin
  try
    if not Assigned(SYSUnit) then
    begin
      s := FindUnitFile('system');
      if FileExists(s) then
      begin
        Stream := TStringStream.Create('');
        try
          StringStreamLoadFromFile(s, Stream);
          SysSource := Stream.DataString;
        finally
          Stream.Free;
        end;
      end else
        SysSource := 'unit system; end.';

      SYSUnit := TSYSTEMUnit.Create(Self, SysSource);
    end;
  except
    on e: exception do
      raise Exception.Create('Internal ERROR: ' + e.Message);
  end;
end;

procedure TNPPackage.AddUnit(aUnit, BeforeUnit: TObject);
var
  i: Integer;
begin
  if FUnits.IndexOf(aUnit) > -1 then
    raise ECompilerInternalError.CreateFmt(msgUnitAlreadyExistFmt, [AnsiUpperCase(TNPUnit(aUnit).Name)]);

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

function TNPPackage.UsesUnit(const UnitName: string; AfterUnit: TObject): TObject;
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
    Result := TNPUnit(FUnits[i]).Compile;
    if Result = CompileFail then
      Exit;
  end;
  // оптимизация модулей
  for i := 0 to FUnits.Count - 1 do
  begin
    try
      TNPUnit(FUnits[i]).Optimize;
    except
      on e: exception do
      begin
        Result := CompileFail;
      end;
    end;
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
    Result := UnitClass.Create(Self, Stream.DataString);
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
type
  TCRec = record
    DTID: TDataTypeID;
    Str: string;
  end;
var
  Idx, c: Integer;
  Node: TStrLiterals.PAVLNode;
  Consts: array of TCRec;
  CItem: ^TCRec;
begin
  c := FStrLiterals.Count;
  Stream.WriteStretchUInt(c);

  {переупорядочиваем литералы по индексам}
  SetLength(Consts, c);
  Node := FStrLiterals.First;
  while Assigned(Node) do begin
    Idx := Node.Data.Index;
    CItem := addr(Consts[Idx]);
    CItem.DTID := Node.Key.StrTypeID;
    CItem.Str := Node.Key.StrValue;
    Node := FStrLiterals.Next(Node);
  end;

  {сохраняем строки в кодировке UTF8}
  for Idx := 0 to c - 1 do begin
    CItem := addr(Consts[Idx]);
    Stream.WriteUInt8(Ord(CItem.DTID)); // пишем тип литерала
    case CItem.DTID of
      dtString: Stream.WriteUTF8String(CItem.Str);  // сам литерал как UTF8
      dtAnsiString: Stream.WriteAnsiString(AnsiString(CItem.Str)); // пишем как Ansi
    end;
  end;
end;

procedure TNPPackage.SaveToStream(Stream: TStream);
var
  i, UnitCnt: Integer;
  UnitsStream: TMemoryStream;
begin
  {заголовок}
  Stream.WriteBoolean(IncludeDebugInfo);

  {простановка номеров модулей}
  UnitCnt := 0;
  for i := 0 to FUnits.Count - 1 do
  begin
    if TNPUnit(FUnits[i]).CheckUsed then
    begin
      TNPUnit(FUnits[i]).UnitID := UnitCnt;
      Inc(UnitCnt);
    end;
  end;

  {подготавливаем строковые литералы перед сохранением IL}
  PrepareStrLiterals;

  {сохраняем модули в промежуточный файл}
  UnitsStream := TMemoryStream.Create;
  try
    UnitsStream.WriteStretchUInt(UnitCnt);

    {сохраняем все декларации}
    for i := 0 to FUnits.Count - 1 do
      if TNPUnit(FUnits[i]).CheckUsed then
        TNPUnit(FUnits[i]).SaveDeclToStream(UnitsStream);

    {сохраняем все тела процедур}
    for i := 0 to FUnits.Count - 1 do
      if TNPUnit(FUnits[i]).CheckUsed then
        TNPUnit(FUnits[i]).SaveBodyToStream(UnitsStream);

    {табличные строковые константы}
    SaveStrLiterals(Stream);

    // переносим в основной файл модули
    UnitsStream.Position := 0;
    Stream.CopyFrom(UnitsStream, UnitsStream.Size);

  finally
    UnitsStream.Free;
  end;
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

