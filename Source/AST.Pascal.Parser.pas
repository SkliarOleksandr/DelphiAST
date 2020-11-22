//====================================================================================================================//
//======================================= THE COMMON PASCAL AST PARSER CLASS =========================================//
//====================================================================================================================//
unit AST.Pascal.Parser;

interface

{$I compilers.inc}

uses SysUtils, Math, Classes, StrUtils, Types, IOUtils, Generics.Collections,
     AST.Lexer.Delphi,
     AST.Delphi.Classes,
     AST.Delphi.DataTypes,
     AST.Lexer,
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


  TASTProcMachArray = array of TASTProcMatchItem;

  TPascalUnit = class(TASTModule)
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
    fVarSpace: TVarSpace;
    fProcSpace: TProcSpace;
    fTypeSpace: TTypeSpace;
    fConsts: TConstSpace;              // список нетривиальных констант (массивы, структуры)
    function GetMessagesText: string;
  protected
    fCompiled: TCompilerResult;
    fUnitName: TIdentifier;            // the Unit declaration name
    fSysUnit: TASTModule;
    fProcMatches: TASTProcMachArray;
    //////////////////////////////////////////////////////////////////////////////////////////////////////////
    function FindPublicDecl(const Name: string): TIDDeclaration;
    function GetPublicClass(const Name: string): TIDClass;
    function GetModuleName: string; override;
    procedure SetUnitName(const Name: string);
  public
    //======================================================================================================================================
    procedure AddType(const Decl: TIDType); inline;
    procedure AddConstant(const Decl: TIDConstant); inline;
    property Lexer: TDelphiLexer read fLexer;
    property SysUnit: TASTModule read fSysUnit;
    ////////////////////////////////////////////////////////////////////////////
    constructor Create(const Project: IASTProject; const FileName: string; const Source: string = ''); override;
    constructor CreateFromFile(const Project: IASTProject; const FileName: string); override;
    destructor Destroy; override;
    ////////////////////////////////////////////////////////////////////////////
    procedure SaveConstsToStream(Stream: TStream); // сохраняет сложные константы модуля
    procedure SaveMethodBodies(Stream: TStream);   // сохраняет тела всех методов модуля
    procedure SaveDeclToStream(Stream: TStream);   // сохраняет все декларации модуля
    procedure SaveBodyToStream(Stream: TStream);   // сохраняет тела всех глобальных процедур модуля
    procedure SaveTypesToStream(Stream: TStream);  // сохраняет все типы модуля

    function Compile(RunPostCompile: Boolean = True): TCompilerResult; virtual;

    function CompileIntfOnly: TCompilerResult; virtual;

    function UsedUnit(const UnitName: string): Boolean;
    function GetDefinesAsString: string;
    property _ID: TIdentifier read FUnitName;
    property UnitID: Integer read FID write FID;
    property Messages: ICompilerMessages read FMessages;
    property MessagesText: string read GetMessagesText;
    property IntfScope: TScope read FIntfScope;    // Interface section scope
    property ImplScope: TScope read FImplScope;    // Implementation section scope
    property IntfImportedUnits: TUnitList read fIntfImportedUnits;
    property ImplImportedUnits: TUnitList read fImplImportedUnits;

    property Compiled: TCompilerResult read FCompiled;
    property TypeSpace: TTypeSpace read FTypeSpace;
    property VarSpace: TVarSpace read FVarSpace;
    property ProcSpace: TProcSpace read FProcSpace;
    property ConstSpace: TConstSpace read FConsts;
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

function TPascalUnit.Compile(RunPostCompile: Boolean = True): TCompilerResult;
begin
  Result := TCompilerResult.CompileInProgress;
  FCompiled := Result;

  fSysUnit := (Project as IASTPascalProject).SysUnit;

  if Assigned(fSysUnit) then
    fIntfImportedUnits.AddObject('system', fSysUnit);
end;

function TPascalUnit.CompileIntfOnly: TCompilerResult;
begin
  Result := TCompilerResult.CompileFail;
end;

constructor TPascalUnit.Create(const Project: IASTProject; const FileName: string; const Source: string = '');
begin
  inherited Create(Project, FileName, Source);
  fSysUnit := (Project as IASTPascalProject).SysUnit;
  fLexer := TDelphiLexer.Create(Source);
  FMessages := TCompilerMessages.Create;
  //FVisibility := vPublic;
  FIntfScope := TScope.Create(stGlobal, @FVarSpace, @FProcSpace, nil, Self);
  {$IFDEF DEBUG}FIntfScope.Name := 'unit_intf_scope';{$ENDIF}
  FImplScope := TImplementationScope.Create(FIntfScope, nil);
  {$IFDEF DEBUG}FImplScope.Name := 'unit_impl_scope';{$ENDIF}
  FIntfImportedUnits := TUnitList.Create;
  FImplImportedUnits := TUnitList.Create;
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

constructor TPascalUnit.CreateFromFile(const Project: IASTProject; const FileName: string);
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create();
  try
    Stream.LoadFromFile(FileName);
    Create(Project, FileName, Stream.DataString);
  finally
    Stream.Free;
  end;
end;

destructor TPascalUnit.Destroy;
begin
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

function TPascalUnit.UsedUnit(const UnitName: string): Boolean;
var
  i: Integer;
begin
  i := FIntfImportedUnits.IndexOf(UnitName);
  if i > 0 then
    Exit(True);

  i := FImplImportedUnits.IndexOf(UnitName);
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

function TPascalUnit.GetPublicClass(const Name: string): TIDClass;
var
  Res: TIDDeclaration;
begin
  Res := FindPublicDecl(Name);
  Result := Res as TIDClass;
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

