//====================================================================================================================//
//======================================= THE COMMON PASCAL AST PARSER CLASS =========================================//
//====================================================================================================================//
unit AST.Pascal.Parser;

interface

uses SysUtils, Math, Classes, StrUtils, Types, IOUtils, Generics.Collections,
     AST.Lexer.Delphi,
     AST.Delphi.Classes,
     AST.Delphi.DataTypes,
     AST.Lexer,
     AST.JsonSchema,
     AST.Delphi.Operators,
     AST.Parser.Utils,
     AST.Parser.Messages,
     AST.Parser.Contexts,
     AST.Delphi.Contexts,
     AST.Classes,
     AST.Parser.Options,
     AST.Intf,
     AST.Pascal.Intf;

type

  TPascalUnit = class;
  TUnitSection = (usInterface, usImplementation);

  {parse members context - контекст парсинга выражений вид a.b.c или a[1, 2, 3].b...}
  TPMContext = record
  private
    FCnt: Integer;          // кол-во элементов в выражении
    FItems: TIDExpressions; // элементы цепочки
    function GetLast: TIDExpression; inline;
  public
    ID: TIdentifier;        // текущий идентификатор
    ItemScope: TScope;      // текущий scope
    DataType: TIDType;      // результатирующий тип цепочки выражений
    property Items: TIDExpressions read FItems;
    property Count: Integer read FCnt;
    property Last: TIDExpression read GetLast;
    procedure Init; inline;
    procedure Add(const Expr: TIDExpression);
    procedure Clear;
  end;

  TUnits = TList<TObject>;
  TTypes = TList<TIDType>;

  TIDDeclarationList = TList<TIDDeclaration>;

  TCondIFValue = (condIFFalse, condIfTrue, condIFUnknown);

  TASTArgMatchLevel = (
    MatchNone,                     // doesn't match at all
    MatchImplicitCallAndDataLoss,  // matches using implicit operator call and possible data loss
    MatchImplicitAndDataLoss,      // matches using implicit cast and possible data loss
    MatchImplicitCall,             // matches using implicit operator call
    MatchImplicit,                 // matches using implicit cast (minimally matches)
    MatchGeneric,                  // matches strictly using generic instantiation
    MatchStrict                    // matches strictly
  );

  TASTArgMatchInfo = record
    Level: TASTArgMatchLevel;
    Rate: TASTArgMatchRate;
  end;
  PASTArgMatchInfo = ^TASTArgMatchInfo;

  TASTArgsMachItems = array of TASTArgMatchInfo;

  TASTProcMatchItem = record
    ArgsInfo: TASTArgsMachItems;
    Decl: TIDProcedure;
    TotalRate: Integer;
  end;
  PASTProcMatchItem = ^TASTProcMatchItem;


  TUnitState = (
    UnitNotCompiled,
    UnitIntfCompiled,
    UnitAllCompiled,
    UnitCompileFailed
  );

  TASTProcMachArray = array of TASTProcMatchItem;

  TPascalUnit = class(TASTModule, IASTPascalUnit)
  type
    TVarModifyPlace = (vmpAssignment, vmpPassArgument);
    TIdentifiersPool = TPool<TIdentifier>;
  private
    fID: Integer;                      // ID модуля в пакете
    fLexer: TDelphiLexer;
    fIntfScope: TScope;                // interface scope
    fImplScope: TScope;                // implementation scope
    fIntfImportedUnits: TUnitList;
    fImplImportedUnits: TUnitList;
    fMessages: ICompilerMessages;
    fTypeSpace: TTypeSpace;
    fConsts: TConstSpace;              // список нетривиальных констант (массивы, структуры)
    function GetMessagesText: string;
    function GetProject: IASTPascalProject;
    function GetSysUnit: IASTPascalUnit; inline;
  protected
    fCompiled: TCompilerResult;
    fUnitState: TUnitState;
    fUnitName: TIdentifier;            // the Unit declaration name
    fProcMatches: TASTProcMachArray;
    FMaxHandle: Integer;
    fDefines: TDefines;                // unit conditional defines
    //////////////////////////////////////////////////////////////////////////////////////////////////////////
    function GetModuleName: string; override;
    procedure SetUnitName(const Name: string);
  public
    function FindPublicDecl(const Name: string): TIDDeclaration;
    function GetPublicClass(const Name: string): TIDClass;
    function GetPublicInterface(const AName: string): TIDInterface;
    function GetPublicType(const Name: string): TIDType;
    //======================================================================================================================================
    procedure AddType(const Decl: TIDType); inline;
    procedure AddConstant(const Decl: TIDConstant); inline;
    property Lexer: TDelphiLexer read fLexer;
    property SysUnit: IASTPascalUnit read GetSysUnit;
    ////////////////////////////////////////////////////////////////////////////
    constructor Create(const AProject: IASTProject; const AFileName: string; const ASource: string = ''); override;
    constructor CreateFromFile(const AProject: IASTProject; const AFileName: string); override;
    destructor Destroy; override;
    ////////////////////////////////////////////////////////////////////////////
    procedure SaveConstsToStream(Stream: TStream); // сохраняет сложные константы модуля
    procedure SaveMethodBodies(Stream: TStream);   // сохраняет тела всех методов модуля
    procedure SaveDeclToStream(Stream: TStream);   // сохраняет все декларации модуля
    procedure SaveBodyToStream(Stream: TStream);   // сохраняет тела всех глобальных процедур модуля
    procedure SaveTypesToStream(Stream: TStream);  // сохраняет все типы модуля

    function Compile(ACompileIntfOnly: Boolean; RunPostCompile: Boolean = True): TCompilerResult; override;
    function CompileIntfOnly: TCompilerResult; virtual;
    function UsedUnit(const AUnitName: string): Boolean;
    function GetDefinesAsString: string;
    function Defined(const ADefine: string): Boolean;
    function ToJson: TJsonASTDeclaration; override;
    function Lexer_TokenName(AToken: Integer): string; override;

    property _ID: TIdentifier read FUnitName;
    property UnitID: Integer read FID write FID;
    property Messages: ICompilerMessages read FMessages;
    property MessagesText: string read GetMessagesText;
    property IntfScope: TScope read FIntfScope;    // Interface section scope
    property ImplScope: TScope read FImplScope;    // Implementation section scope
    property IntfImportedUnits: TUnitList read fIntfImportedUnits;
    property ImplImportedUnits: TUnitList read fImplImportedUnits;

    property Compiled: TCompilerResult read FCompiled;
    property UnitState: TUnitState read fUnitState;
    property Project: IASTPascalProject read GetProject;
  end;

implementation

{ TCompiler }

uses AST.Delphi.System,
     AST.Parser.Errors,
     AST.Pascal.Project,
     AST.Pascal.ConstCalculator;


procedure TPascalUnit.SetUnitName(const Name: string);
begin
  FUnitName.Name := Name;
end;

function TPascalUnit.ToJson: TJsonASTDeclaration;
begin
  var LObject := TJsonASTUnit.Create;
  LObject.name := Name;
  LObject.kind := 'unit';
  LObject.fileName := FileName;

  SetLength(LObject.intfDecls, IntfScope.Count);
  for var LIndex := 0 to IntfScope.Count - 1 do
  begin
    var LDecl := IntfScope.Items[LIndex];
    if not (LDecl is TIDUnit) or (TIDUnit(LDecl).UseModule <> Self) then
    begin
      var LJson := LDecl.ToJson;
      LObject.intfDecls[LIndex] := LJson;
    end;
  end;

  SetLength(LObject.implDecls, ImplScope.Count);
  for var LIndex := 0 to ImplScope.Count - 1 do
  begin
    var LDecl := ImplScope.Items[LIndex];
    var LJson := LDecl.ToJson;
    LObject.implDecls[LIndex] := LJson;
  end;

  Result := LObject;
end;

function TPascalUnit.Compile(ACompileIntfOnly: Boolean; RunPostCompile: Boolean = True): TCompilerResult;
begin

  Result := TCompilerResult.CompileInProgress;
  FCompiled := Result;

  // add system unit implicitly
  if Assigned(SysUnit) and (Self <> TObject(SysUnit)) then
  begin
    fIntfImportedUnits.AddObject('system', TObject(SysUnit));
    // add system unit scope as joint
    fIntfScope.AddScope(TPascalUnit(SysUnit).IntfScope);
  end;
end;

function TPascalUnit.CompileIntfOnly: TCompilerResult;
begin
  Result := TCompilerResult.CompileFail;
end;

constructor TPascalUnit.Create(const AProject: IASTProject; const AFileName: string; const ASource: string = '');
begin
  var LSource := ASource;
  if (ASource = '') and FileExists(AFileName) then
    LSource := TFile.ReadAllText(AFileName);

  inherited Create(AProject, AFileName, LSource);

  fLexer := TDelphiLexer.Create(LSource);
  FMessages := TCompilerMessages.Create;
  //FVisibility := vPublic;

  var AUnitName := StringReplace(ExtractFileName(FileName), '.pas', '', []);

  FIntfImportedUnits := TUnitList.Create;
  FImplImportedUnits := TUnitList.Create;

  FIntfScope := TInterfaceScope.Create(Self);
  {$IFDEF DEBUG}FIntfScope.Name := AUnitName + '$intf_scope';{$ENDIF}
  FImplScope := TImplementationScope.Create(FIntfScope);
  {$IFDEF DEBUG}FImplScope.Name := AUnitName + '$impl_scope';{$ENDIF}

  //FBENodesPool := TBENodesPool.Create(16);

  // pre allocate 8 items by 8 args
  SetLength(fProcMatches, 8);
  for var i := 0 to Length(fProcMatches) - 1 do
    SetLength(fProcMatches[i].ArgsInfo, 8);

//  FOptions := TCompilerOptions.Create(Package.Options);
//
//  fCondStack := TSimpleStack<Boolean>.Create(0);
//  fCondStack.OnPopError := procedure begin ERROR_INVALID_COND_DIRECTIVE() end;
end;

constructor TPascalUnit.CreateFromFile(const AProject: IASTProject; const AFileName: string);
begin
  var LStrings := TStringList.Create();
  try
    LStrings.LoadFromFile(AFileName);
    Create(AProject, AFileName, LStrings.Text);
  finally
    LStrings.Free;
  end;
end;

function TPascalUnit.Defined(const ADefine: string): Boolean;
begin
  // 1. search in the module itself
  Result := FDefines.IndexOf(ADefine) > -1;
  // 2. serach in the project
  if not Result then
    Result := Project.Defines.IndexOf(ADefine) > -1;
end;

destructor TPascalUnit.Destroy;
begin
  fDefines.Free;
  FIntfScope.Free;
  FImplScope.Free;
  fLexer.Free;
  FIntfImportedUnits.Free;
  FImplImportedUnits.Free;
  inherited;
end;

function TPascalUnit.FindPublicDecl(const Name: string): TIDDeclaration;
begin
  Result := fIntfScope.FindID(Name);
end;

function TPascalUnit.UsedUnit(const AUnitName: string): Boolean;
var
  i: Integer;
begin
  i := FIntfImportedUnits.IndexOf(AUnitName);
  if i > 0 then
    Exit(True);

  i := FImplImportedUnits.IndexOf(AUnitName);
  if i > 0 then
    Exit(True)
  else
    Exit(False);
end;

procedure TPascalUnit.AddConstant(const Decl: TIDConstant);
var
  Item: TIDConstant;
begin
  Item := FConsts.First;
  while Assigned(Item) do
  begin
    if Item = Decl then
      Exit
    else
      Break;
    Item := TIDConstant(Item.NextItem);
  end;
  FConsts.Add(Decl);
end;

procedure TPascalUnit.AddType(const Decl: TIDType);
begin
  if not (Decl is TIDAliasType) and not Decl.IsPooled then
  begin
    FTypeSpace.Add(Decl);
    Decl.IsPooled := True;
  end;
end;

function TPascalUnit.GetMessagesText: string;
begin
  Result := FMessages.GetAsString;
end;

function TPascalUnit.GetModuleName: string;
begin
  Result := fUnitName.Name;
  if Result = '' then
    Result := ChangeFileExt(inherited GetModuleName(), '');
end;

function TPascalUnit.GetProject: IASTPascalProject;
begin
  Result := FProject as IASTPascalProject;
end;

function TPascalUnit.GetPublicClass(const Name: string): TIDClass;
var
  Res: TIDDeclaration;
begin
  Res := FindPublicDecl(Name);
  Result := Res as TIDClass;
end;

function TPascalUnit.GetPublicInterface(const AName: string): TIDInterface;
begin
  Result := FindPublicDecl(AName) as TIDInterface;
end;

function TPascalUnit.GetPublicType(const Name: string): TIDType;
var
  Res: TIDDeclaration;
begin
  Res := FindPublicDecl(Name);
  Result := Res as TIDType;
end;

function TPascalUnit.GetSysUnit: IASTPascalUnit;
begin
  Result := Project.SysUnit;
end;

function TPascalUnit.Lexer_TokenName(AToken: Integer): string;
begin
  Result := Lexer.TokenLexem(TTokenID(AToken));
end;

{parser methods}

procedure TPascalUnit.SaveConstsToStream(Stream: TStream);
begin
  Assert(False);
end;

procedure TPascalUnit.SaveMethodBodies(Stream: TStream);
begin
  Assert(False);
end;


procedure TPascalUnit.SaveTypesToStream(Stream: TStream);
begin
  Assert(False);
end;

procedure TPascalUnit.SaveDeclToStream(Stream: TStream);
begin
  Assert(False);
end;

procedure TPascalUnit.SaveBodyToStream(Stream: TStream);
begin
  Assert(False);
end;

{ TPMContext }

procedure TPMContext.Add(const Expr: TIDExpression);
begin
  SetLength(FItems, FCnt + 1);
  FItems[FCnt] := Expr;
  Inc(FCnt);
end;

procedure TPMContext.Clear;
begin
  FItems := nil;
  FCnt := 0;
end;

function TPMContext.GetLast: TIDExpression;
begin
  if FCnt > 0 then
    Result := FItems[FCnt - 1]
  else
    Result := nil;
end;

procedure TPMContext.Init;
begin
  FCnt := 0;
  DataType := nil;
end;

function TPascalUnit.GetDefinesAsString: string;
begin
//  Result := FDefines.Text;
end;

initialization
  FormatSettings.DecimalSeparator := '.';

end.

