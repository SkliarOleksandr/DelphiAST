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
     AST.Project;

type

  TNPUnit = class;
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

  TNPUnit = class(TASTModule)
  type
    TVarModifyPlace = (vmpAssignment, vmpPassArgument);
    TIdentifiersPool = TPool<TIdentifier>;
  private
    FID: Integer;                      // ID модуля в пакете
    fLexer: TDelphiLexer;
    FIntfScope: TScope;                // interface scope
    FImplScope: TScope;                // implementation scope
    FIntfImportedUnits: TUnitList;
    FImplImportedUnits: TUnitList;
    FMessages: ICompilerMessages;
    FVarSpace: TVarSpace;
    FProcSpace: TProcSpace;
    FTypeSpace: TTypeSpace;
    FConsts: TConstSpace;              // список нетривиальных констант (массивы, структуры)
    FCompiled: Boolean;
    function GetMessagesText: string;
  protected
    fUnitName: TIdentifier;            // the Unit declaration name
    //////////////////////////////////////////////////////////////////////////////////////////////////////////
    function GetModuleName: string; override;
    procedure SetUnitName(const Name: string);
  public
    class function IsConstValueInRange(Value: TIDExpression; RangeExpr: TIDRangeConstant): Boolean; static;
    class function IsConstRangesIntersect(const Left, Right: TIDRangeConstant): Boolean; static;
    class function IsConstEqual(const Left, Right: TIDExpression): Boolean; static;
    //======================================================================================================================================
    procedure AddType(const Decl: TIDType); inline;
    procedure AddConstant(const Decl: TIDConstant); inline;
  public
    property Lexer: TDelphiLexer read fLexer;
  public
    ////////////////////////////////////////////////////////////////////////////
    constructor Create(const Project: IASTProject; const FileName: string; const Source: string = ''); override;
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
    property Name: string read FUnitName.Name;
    property Messages: ICompilerMessages read FMessages;
    property MessagesText: string read GetMessagesText;
    property IntfScope: TScope read FIntfScope;    // Interface section scope
    property ImplScope: TScope read FImplScope;    // Implementation section scope
    property IntfImportedUnits: TUnitList read fIntfImportedUnits;
    property ImplImportedUnits: TUnitList read fImplImportedUnits;

    property Compiled: Boolean read FCompiled;
    property TypeSpace: TTypeSpace read FTypeSpace;
    property VarSpace: TVarSpace read FVarSpace;
    property ProcSpace: TProcSpace read FProcSpace;
    property ConstSpace: TConstSpace read FConsts;
  end;

implementation

{ TCompiler }

uses AST.Delphi.System,
     AST.Parser.Errors,
     AST.Pascal.ConstCalculator;


procedure TNPUnit.SetUnitName(const Name: string);
begin
  FUnitName.Name := Name;
end;

function TNPUnit.Compile(RunPostCompile: Boolean = True): TCompilerResult;
begin
  Result := TCompilerResult.CompileFail;
end;

function TNPUnit.CompileIntfOnly: TCompilerResult;
begin
  Result := TCompilerResult.CompileFail;
end;

constructor TNPUnit.Create(const Project: IASTProject; const FileName: string; const Source: string = '');
begin
  inherited Create(Project, FileName, Source);
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
  if Assigned(SYSUnit) then
  begin
    FTypeSpace.Initialize(SYSUnit.SystemTypesCount);
    // добовляем system в uses
    FIntfImportedUnits.AddObject('system', SYSUnit);
  end;
//  FOptions := TCompilerOptions.Create(Package.Options);
//
//  fCondStack := TSimpleStack<Boolean>.Create(0);
//  fCondStack.OnPopError := procedure begin ERROR_INVALID_COND_DIRECTIVE() end;
end;

destructor TNPUnit.Destroy;
begin
  FIntfScope.Free;
  FImplScope.Free;
  fLexer.Free;
  FIntfImportedUnits.Free;
  FImplImportedUnits.Free;
  inherited;
end;

function TNPUnit.UsedUnit(const UnitName: string): Boolean;
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

class function TNPUnit.IsConstValueInRange(Value: TIDExpression; RangeExpr: TIDRangeConstant): Boolean;
var
  Expr: TIDExpression;
begin
  Expr := ProcessConstOperation(Value, RangeExpr.Value.LBExpression, opLess);
  if TIDBooleanConstant(Expr.Declaration).Value then
    Exit(False);

  Expr := ProcessConstOperation(Value, RangeExpr.Value.HBExpression, opLessOrEqual);
  Result := TIDBooleanConstant(Expr.Declaration).Value;
end;

class function TNPUnit.IsConstRangesIntersect(const Left, Right: TIDRangeConstant): Boolean;
var
  Expr,
  LeftLB, LeftHB,
  RightLB, RightHB: TIDExpression;
begin
  LeftLB := Left.Value.LBExpression;
  LeftHB := Left.Value.HBExpression;

  RightLB := Right.Value.LBExpression;
  RightHB := Right.Value.HBExpression;

  Expr := ProcessConstOperation(LeftLB, RightLB, opLess);
  // если Left.Low < Right.Low
  if TIDBooleanConstant(Expr.Declaration).Value then
  begin
    Expr := ProcessConstOperation(LeftHB, RightLB, opGreaterOrEqual);
    Result := TIDBooleanConstant(Expr.Declaration).Value;
  end else begin
    Expr := ProcessConstOperation(RightHB, LeftLB, opGreaterOrEqual);
    Result := TIDBooleanConstant(Expr.Declaration).Value;
  end;
end;

procedure TNPUnit.AddConstant(const Decl: TIDConstant);
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

procedure TNPUnit.AddType(const Decl: TIDType);
begin
  if not (Decl is TIDAliasType) and not Decl.IsPooled then
  begin
    FTypeSpace.Add(Decl);
    Decl.IsPooled := True;
  end;
end;

function TNPUnit.GetMessagesText: string;
begin
  Result := FMessages.GetAsString;
end;

function TNPUnit.GetModuleName: string;
begin
  Result := fUnitName.Name;
end;

class function TNPUnit.IsConstEqual(const Left, Right: TIDExpression): Boolean;
var
  RExpr: TIDExpression;
begin
  RExpr := ProcessConstOperation(Left, Right, opEqual);
  Result := TIDBooleanConstant(RExpr.Declaration).Value;
end;

{parser methods}


procedure TNPUnit.SaveConstsToStream(Stream: TStream);
begin
  Assert(False);
end;

procedure TNPUnit.SaveMethodBodies(Stream: TStream);
begin
  Assert(False);
end;


procedure TNPUnit.SaveTypesToStream(Stream: TStream);
begin
  Assert(False);
end;

procedure TNPUnit.SaveDeclToStream(Stream: TStream);
begin
  Assert(False);
end;

procedure TNPUnit.SaveBodyToStream(Stream: TStream);
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

function TNPUnit.GetDefinesAsString: string;
begin
//  Result := FDefines.Text;
end;

initialization
  FormatSettings.DecimalSeparator := '.';

end.
