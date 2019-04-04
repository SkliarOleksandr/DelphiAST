unit AST.Delphi.Parser;

interface

uses System.SysUtils,
     System.Types,
     System.StrUtils,
     System.Classes,
     OPCompiler,
     iDStringParser,
     AST.Classes,
     NPCompiler.Intf,
     NPCompiler.Operators,
     OPCompiler.Parser,
     NPCompiler.Messages,
     NPCompiler.Errors,
     NPCompiler.Classes,
     NPCompiler.Utils,
     AST.Parser.Contexts, AST.Project;

type

  TASTDelphiProc = class(TIDProcedure)
  private
    fBody: TASTBlock;
  public
    property Body: TASTBlock read fBody;
  end;

  TASTDelphiLabel = class(TIDDeclaration)
    constructor Create(Scope: TScope; const Identifier: TIdentifier); overload; override;
  end;


  TSContext = TASTSContext<TASTDelphiProc>;
  TEContext = TASTEcontext<TASTDelphiProc>;

  PSContext = ^TSContext;
  PEContext = ^TEContext;

  TASTDelphiUnit = class(TNPUnit)
  private
    procedure CheckLeftOperand(const Status: TRPNStatus);
    function CreateAnonymousConstant(Scope: TScope; var EContext: TEContext;
      const ID: TIdentifier; IdentifierType: TIdentifierType): TIDExpression;
    procedure InitEContext(var EContext: TEContext; const SContext: TSContext; EPosition: TExpessionPosition); inline;

    class function MatchExplicit(const Source: TIDExpression; Destination: TIDType): TIDDeclaration; static;
    class function MatchArrayImplicitToRecord(Source: TIDExpression; Destination: TIDStructure): TIDExpression; static;

  protected
    function GetModuleName: string; override;
    function GetFirstFunc: TASTDeclaration; override;
    function GetFirstVar: TASTDeclaration; override;
    function GetFirstType: TASTDeclaration; override;
    function GetFirstConst: TASTDeclaration; override;
    procedure CheckLabelExpression(const Expr: TIDExpression); overload;
    procedure CheckLabelExpression(const Decl: TIDDeclaration); overload;
    function Process_operators2(var EContext: TEContext; OpID: TOperatorID): TIDExpression;
    function Process_CALL(var EContext: TEContext): TIDExpression;
    function Process_CALL_direct(const SContext: TSContext; PExpr: TIDCallExpression; CallArguments: TIDExpressions): TIDExpression;
    procedure Process_operator_Assign(var EContext: TEContext);
    function Process_operator_neg(var EContext: TEContext): TIDExpression;
    function Process_operator_not(var EContext: TEContext): TIDExpression;
    function Process_operator_Addr(var EContext: TEContext): TIDExpression;
    function Process_operator_Deref(var EContext: TEContext): TIDExpression;
    function Process_operator_In(var EContext: TEContext; const Left, Right: TIDExpression): TIDExpression;
    function Process_operator_Is(var EContext: TEContext): TIDExpression;
    function Process_operator_As(var EContext: TEContext): TIDExpression;
    function Process_operator_Period(var EContext: TEContext): TIDExpression;
    class function CheckImplicit(Source: TIDExpression; Dest: TIDType): TIDDeclaration; override;

    function GetTMPVar(const SContext: TSContext; DataType: TIDType): TIDVariable; overload;
    function GetTMPVar(const EContext: TEContext; DataType: TIDType): TIDVariable; overload;
    function GetTMPRef(const SContext: TSContext; DataType: TIDType): TIDVariable;
    function GetTMPVarExpr(const SContext: TSContext; DataType: TIDType; const TextPos: TTextPosition): TIDExpression; overload; inline;
    function GetTMPVarExpr(const EContext: TEContext; DataType: TIDType; const TextPos: TTextPosition): TIDExpression; overload; inline;
    function GetTMPRefExpr(const SContext: TSContext; DataType: TIDType): TIDExpression; overload; inline;
    function GetTMPRefExpr(const SContext: TSContext; DataType: TIDType; const TextPos: TTextPosition): TIDExpression; overload; inline;

    function MatchImplicit3(const SContext: TSContext; Source: TIDExpression; Dest: TIDType): TIDExpression;
    function MatchImplicitOrNil(const SContext: TSContext; Source: TIDExpression; Dest: TIDType): TIDExpression;
    function MatchArrayImplicit(const SContext: TSContext; Source: TIDExpression; DstArray: TIDArray): TIDExpression;
    function MatchRecordImplicit(const SContext: TSContext; Source: TIDExpression; DstRecord: TIDRecord): TIDExpression;
    function MatchBinarOperator(const SContext: TSContext; Op: TOperatorID; var Left, Right: TIDExpression): TIDDeclaration;
    function MatchBinarOperatorWithImplicit(const SContext: TSContext; Op: TOperatorID; var Left, Right: TIDexpression): TIDDeclaration;
  public
    function ParseStatements(Scope: TScope; var SContext: TSContext; IsBlock: Boolean): TTokenID; overload;
    function ParseExpression(Scope: TScope; var SContext: TSContext; var EContext: TEContext; out ASTE: TASTExpression): TTokenID; overload;
    function ParseConstExpression(Scope: TScope; out Expr: TIDExpression; EPosition: TExpessionPosition): TTokenID; override;
    function ParseMember(Scope: TScope; out Expression: TIDExpression; var EContext: TEContext;
                         var SContext: TSContext; const ASTE: TASTExpression): TTokenID;
    function ParseArrayMember(Scope: TScope; var PMContext: TPMContext; Decl: TIDDeclaration; out DataType: TIDType;
                              var EContext: TEContext; var SContext: TSContext; ASTE: TASTExpression): TTokenID;
    function ParsePropertyMember(var PMContext: TPMContext; Scope: TScope; Prop: TIDProperty; out Expression: TIDExpression;
                                 var EContext: TEContext): TTokenID;
    function ParseIndexedPropertyArgs(Scope: TScope; out ArgumentsCount: Integer; var EContext: TEContext): TTokenID;
    procedure ParseVector(Scope: TScope; var EContext: TEContext);
    function ParseEntryCall(Scope: TScope; CallExpr: TIDCallExpression; var EContext: TEContext;
                            var SContext: TSContext; const ASTE: TASTExpression): TTokenID;
    function ParseBuiltinCall(Scope: TScope; CallExpr: TIDExpression; var EContext: TEContext): TTokenID;
    function ParseExplicitCast(Scope: TScope; var SContext: TSContext; var DstExpression: TIDExpression): TTokenID;
    function ParseExitStatement(Scope: TScope; var SContext: TSContext): TTokenID; overload;
    function ParseProcedure(Scope: TScope; ProcType: TProcType; Struct: TIDStructure = nil): TTokenID; override;
    function ParseProcBody(Proc: TASTDelphiProc): TTokenID;
    function ParseIfThenStatement(Scope: TScope; var SContext: TSContext): TTokenID;
    function ParseWhileStatement(Scope: TScope; var SContext: TSContext): TTokenID;
    function ParseRepeatStatement(Scope: TScope; var SContext: TSContext): TTokenID;
    function ParseWithStatement(Scope: TScope; var SContext: TSContext): TTokenID;
    function ParseForStatement(Scope: TScope; var SContext: TSContext): TTokenID;
    function ParseForInStatement(Scope: TScope; var SContext: TSContext; LoopVar: TIDExpression): TTokenID;
    function ParseCaseStatement(Scope: TScope; var SContext: TSContext): TTokenID;
    function ParseBreakStatement(Scope: TScope; var SContext: TSContext): TTokenID;
    function ParseContinueStatement(Scope: TScope; var SContext: TSContext): TTokenID;
    function ParseImmVarStatement(Scope: TScope; var SContext: TSContext): TTokenID;
    function ParseTrySection(Scope: TScope; var SContext: TSContext): TTokenID;
    function ParseExceptOnSection(Scope: TScope; KW: TASTKWTryBlock; var SContext: TSContext): TTokenID;
    function ParseRaiseStatement(Scope: TScope; var SContext: TSContext): TTokenID;
    function ParseLabelSection(Scope: TScope): TTokenID;
    function ParseGoToStatement(Scope: TScope; var SContext: TSContext): TTokenID;
    function ParseASMStatement(Scope: TScope; var SContext: TSContext): TTokenID;
    function ParseProperty(Struct: TIDStructure): TTokenID; override;
    function ParseVarDefaultValue(Scope: TScope; DataType: TIDType; out DefaultValue: TIDExpression): TTokenID; override;
    function ParseVarStaticArrayDefaultValue(Scope: TScope; ArrType: TIDArray; out DefaultValue: TIDExpression): TTokenID; override;
    function ParseRecordInitValue(Scope: TRecordInitScope; var FirstField: TIDExpression): TTokenID;

    function ParseCondInclude(Scope: TScope): TTokenID; override;

    function ParseGenericsArgs(Scope: TScope; var SContext: TSContext; out Args: TIDExpressions): TTokenID;
    function ParseGenericTypeSpec(Scope: TScope; const ID: TIdentifier; out DataType: TIDType): TTokenID; override;
    function ParseInitSection: TTokenID; override;
    function ParseFinalSection: TTokenID; override;

    procedure CheckIncompletedProcs(ProcSpace: PProcSpace); override;
    function Compile(RunPostCompile: Boolean = True): TCompilerResult; override;
    function CompileIntfOnly: TCompilerResult; override;

    constructor Create(const Project: IASTProject; const FileName: string;  const Source: string = ''); override;
  end;

  TIDBuiltInFunction = class(TIDProcedure)
  protected
    class function GetFunctionID: TBuiltInFunctionID; virtual; abstract;
    class function CreateTMPExpr(const EContext: TEContext; const DataType: TIDType): TIDExpression;
  public
    constructor Create(Scope: TScope; const Name: string; ResultType: TIDType); reintroduce; virtual;
    /////////////////////////////////////////////////////////////////////////////////////////
    property FunctionID: TBuiltInFunctionID read GetFunctionID;
    class function Register(Scope: TScope): TIDBuiltInFunction; virtual; abstract;
  end;
  TIDBuiltInFunctionClass = class of TIDBuiltInFunction;


  TIDSysRuntimeFunction = class(TIDBuiltInFunction)
  protected
    class function GetFunctionID: TBuiltInFunctionID; override;
  public
    function Process(var EContext: TEContext): TIDExpression; virtual;
  end;

  TSysFunctionContext = record
    UN: TNPUnit;
    Scope: TScope;
    ParamsStr: string;
    EContext: ^TEContext;
    SContext: ^TSContext;
  end;

  TIDSysCompileFunction = class(TIDBuiltInFunction)
  protected
    class function GetFunctionID: TBuiltInFunctionID; override;
  public
    function Process(const Ctx: TSysFunctionContext): TIDExpression; virtual; abstract;
  end;

  TIDSysRuntimeFunctionClass = class of TIDSysRuntimeFunction;
  TIDSysCompileFunctionClass = class of TIDSysCompileFunction;

  TIDInternalOperator = class(TIDOperator)
  public
    constructor CreateAsIntOp; reintroduce;
  end;

  TIDInternalOpImplicit = class(TIDInternalOperator)
  public
    constructor CreateInternal(ResultType: TIDType); reintroduce;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; virtual; abstract;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; virtual; abstract;
  end;

  {внутренний implicit оператор String -> AnsiString}
  TIDOpImplicitStringToAnsiString = class(TIDInternalOpImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {внутренний implicit оператор AnsiString -> String}
  TIDOpImplicitAnsiStringToString = class(TIDInternalOpImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {внутренний implicit оператор Char -> String}
  TIDOpImplicitCharToString = class(TIDInternalOpImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {внутренний implicit оператор Char -> AnsiString}
  TIDOpImplicitCharToAnsiString = class(TIDInternalOpImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {внутренний implicit оператор Char -> AnsiChar}
  TIDOpImplicitCharToAnsiChar = class(TIDInternalOpImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {внутренний implicit оператор AnsiChar -> AnsiString}
  TIDOpImplicitAnsiCharToAnsiString = class(TIDInternalOpImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {внутренний implicit оператор AnsiChar -> String}
  TIDOpImplicitAnsiCharToString = class(TIDInternalOpImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {внутренний implicit оператор AnsiChar -> Char}
  TIDOpImplicitAnsiCharToChar = class(TIDInternalOpImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {внутренний implicit оператор String -> AnsiString}
  TIDOpImplicitStringToPChar = class(TIDInternalOpImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {внутренний implicit оператор MetaClass -> TGUID}
  TIDOpImplicitMetaClassToGUID = class(TIDInternalOpImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {внутренний implicit оператор String -> TGUID}
  TIDOpImplicitStringToGUID = class(TIDInternalOpImplicit)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {внутренний implicit оператор Closure -> TMethod}
  TIDOpImplicitClosureToTMethod = class(TIDInternalOpImplicit)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {внутренний implicit оператор ConstDynArray -> Set}
  TIDOpImplicitDynArrayToSet = class(TIDInternalOpImplicit)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {внутренний implicit оператор Any -> Variant}
  TIDOpImplicitAnyToVariant = class(TIDInternalOpImplicit)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {внутренний implicit оператор Variant -> Any}
  TIDOpImplicitVariantToAny = class(TIDInternalOpImplicit)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {внутренний explicit оператор Int -> Enum}
  TIDOpExplicitIntToEnum = class(TIDInternalOpImplicit)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {внутренний explicit оператор Any -> TProc}
  TIDOpExplicitTProcFromAny = class(TIDInternalOpImplicit)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  function GetUnit(const SContext: PSContext): TASTDelphiUnit; overload;
  function GetUnit(const EContext: TEContext): TASTDelphiUnit; overload;

implementation

uses NPCompiler.DataTypes,
     NPCompiler.ConstCalculator,
    SystemUnit;

type
  TSContextHelper = record helper for TSContext
  private
    function GetIsLoopBody: Boolean;
    function GetIsTryBlock: Boolean;
  public
    property IsLoopBody: Boolean read GetIsLoopBody;
    property IsTryBlock: Boolean read GetIsTryBlock;
  end;

function GetUnit(const SContext: PSContext): TASTDelphiUnit;
begin
  Result := SContext.Module as TASTDelphiUnit;
end;

function GetUnit(const EContext: TEContext): TASTDelphiUnit;
begin
  Result := EContext.SContext.Module as TASTDelphiUnit;
end;

{ TASTDelphiUnit }

function TASTDelphiUnit.ParseStatements(Scope: TScope; var SContext: TSContext; IsBlock: Boolean): TTokenID;
var
  LEContext, REContext: TEContext;
  NewScope: TScope;
  AST: TASTItem;
begin
  //     ASTE.AddDeclItem(Expr.Declaration, Expr.TextPosition);
  Result := parser_CurTokenID;
  while True do begin
    case Result of
      {BEGIN}
      token_begin: begin
        parser_NextToken(Scope);
        NewScope := TScope.Create(stLocal, Scope);
        Result := ParseStatements(NewScope, SContext, True);
        parser_MatchToken(Result, token_end);
        Result := parser_NextToken(Scope);
        if not IsBlock then
          Exit;
        continue;
      end;
      {END}
      token_end: begin
        exit;
      end;
      {UNTIL}
      token_until: Break;
      {EXIT}
      token_exit: Result := ParseExitStatement(Scope, SContext);
      {IF}
      token_if: Result := ParseIfThenStatement(Scope, SContext);
      {WHILE}
      token_while: Result := ParseWhileStatement(Scope, SContext);
      {REPEAT}
      token_repeat: Result := ParseRepeatStatement(Scope, SContext);
      {WITH}
      token_with: Result := ParseWithStatement(Scope, SContext);
      {FOR}
      token_for: Result := ParseForStatement(Scope, SContext);
      {CASE}
      token_case: Result := ParseCaseStatement(Scope, SContext);
      {GOTO}
      token_goto: Result := ParseGoTOStatement(Scope, SContext);
      {ASM}
      token_asm: Result := ParseASMStatement(Scope, SContext);
      {INHERITED}
      (*token_inherited: begin
        InitEContext(EContext, SContext, ExprLValue);
        Result := ParseInheritedStatement(Scope, EContext);
      end;*)
      {TRY}
      token_try: Result := ParseTrySection(Scope, SContext);
      {EXCEPT/FINALLY}
      token_except, token_finally: begin
        if not SContext.IsTryBlock then
          ERROR_TRY_KEYWORD_MISSED;
        Exit;
      end;
      {RAISE}
      token_raise: Result := ParseRaiseStatement(Scope, SContext);
      {BREAK}
      token_break: Result := ParseBreakStatement(Scope, SContext);
      {CONTINUE}
      token_continue: Result := ParseContinueStatement(Scope, SContext);
      {;}
      token_semicolon:;
      {VAR}
      token_var: Result := ParseImmVarStatement(Scope, SContext);
      {@}
      token_address: begin
        Result := parser_NextToken(Scope);
        Continue;
      end;
      {IDENTIFIER}
      token_identifier: begin
        InitEContext(LEContext, SContext, ExprLValue);
        begin
          var ASTEDst, ASTESrc: TASTExpression;
          Result := ParseExpression(Scope, SContext, LEContext, ASTEDst);
          if Result = token_assign then begin
            InitEContext(REContext, SContext, ExprRValue);
            parser_NextToken(Scope);
            Result := ParseExpression(Scope, SContext, REContext, ASTESrc);
//            if Assigned(REContext.LastBoolNode) then
//              Bool_CompleteImmediateExpression(REContext, REContext.Result);

            LEContext.RPNPushExpression(REContext.Result);
            LEContext.RPNPushOperator(opAssignment);
            LEContext.RPNFinish();

            var Op := SContext.Add<TASTOpAssign>;
            Op.Dst := ASTEDst;
            Op.Src := ASTESrc;

          end else
          if Result = token_colon then
          begin
            var LExpr := LEContext.Result;
            CheckLabelExpression(LExpr);
            var KW := SContext.Add<TASTKWLabel>;
            KW.&Label := LExpr.Declaration;
            Result := parser_NextToken(Scope);
            Continue;
          end else
            SContext.AddItem(ASTEDst);
          //CheckAndCallFuncImplicit(LEContext);
          //CheckUnusedExprResult(LEContext);
        end;
      end;
      token_initialization,
      token_finalization: Break;
      token_eof: Exit;
    else
      if IsBlock then
        ERROR_EXPECTED_KEYWORD_OR_ID;
    end;

    if IsBlock then
    begin
      parser_MatchSemicolon(Result);
      Result := parser_NextToken(Scope);
    end else
      Exit;
  end;
end;

function TASTDelphiUnit.ParseTrySection(Scope: TScope; var SContext: TSContext): TTokenID;
var
  KW: TASTKWTryBlock;
  NewContext: TSContext;
  ExceptItem: TASTKWTryExceptItem;
begin
  KW := SContext.Add<TASTKWTryBlock>;
  NewContext := SContext.MakeChild(KW.Body);
  // запоминаем предыдущий TryBlock
  parser_NextToken(Scope);
  Result := ParseStatements(Scope, NewContext, True);
  case Result of
    {parse EXCEPT section}
    token_except: begin
      Result := parser_NextToken(Scope);
      if Result <> token_on then
      begin
        ExceptItem := KW.AddExceptBlock(nil);
        NewContext := SContext.MakeChild(ExceptItem.Body);
        Result := ParseStatements(Scope, NewContext, True);
      end else begin
        while Result = token_on do
          Result := ParseExceptOnSection(Scope, KW, SContext);
      end;

      parser_MatchToken(Result, token_end);
      Result := parser_NextToken(Scope);
    end;
    {parse FINALLY section}
    token_finally: begin
      parser_NextToken(Scope);
      KW.FinallyBody := TASTBlock.Create(KW);
      NewContext := SContext.MakeChild(KW.FinallyBody);
      Result := ParseStatements(Scope, NewContext, True);
      parser_MatchToken(Result, token_end);
      Result := parser_NextToken(Scope);
    end;
  else
    AbortWork(sExceptOrFinallySectionWasMissed, parser_Position);
  end;
end;

function TASTDelphiUnit.ParseEntryCall(Scope: TScope; CallExpr: TIDCallExpression; var EContext: TEContext;
                                       var SContext: TSContext; const ASTE: TASTExpression): TTokenID;
var
  ArgumentsCount: Integer;
  Expr: TIDExpression;
  InnerEContext: TEContext;
  ASTExpr: TASTExpression;
  ASTCall: TASTOpCallProc;
begin
  ASTCall := ASTE.AddOperation<TASTOpCallProc>;
  ASTCall.Proc := CallExpr.Declaration;
  ArgumentsCount := 0;
  InitEContext(InnerEContext, SContext, ExprNested);
  {цикл парсинга аргументов}
  while true do begin
    Result := parser_NextToken(Scope);
    if Result = token_closeround then
    begin
      Result := parser_NextToken(Scope);
      Break;
    end;
    Result := ParseExpression(Scope, SContext, InnerEContext, ASTExpr);
    ASTCall.AddArg(ASTExpr);
    Expr := InnerEContext.Result;
    if Assigned(Expr) then begin
      {if Expr.DataType = SYSUnit._Boolean then
      begin
        if (Expr.ItemType = itVar) and Expr.IsAnonymous then
          Bool_CompleteImmediateExpression(InnerEContext, Expr);
      end;}
      EContext.RPNPushExpression(Expr);
    end else begin
      // Добавляем пустой Expression для значения по умолчанию
      EContext.RPNPushExpression(nil);
    end;
    Inc(ArgumentsCount);
    case Result of
      token_coma: begin
        InnerEContext.Reset;
        continue;
      end;
      token_closeround: begin
        Result := parser_NextToken(Scope);
        Break;
      end;
    else
      ERROR_INCOMPLETE_STATEMENT;
    end;
  end;

  if pfDestructor in CallExpr.AsProcedure.Flags then
    ERROR_DESTRUCTOR_CANNOT_BE_CALL_DIRECTLY;

  TIDCallExpression(CallExpr).ArgumentsCount := ArgumentsCount;
  EContext.RPNPushExpression(CallExpr);
  EContext.RPNPushOperator(opCall);
end;

function TASTDelphiUnit.ParseBuiltinCall(Scope: TScope; CallExpr: TIDExpression; var EContext: TEContext): TTokenID;
var
  FuncDecl: TIDBuiltInFunction;
  ArgsCount: Integer;
  Expr: TIDExpression;
  InnerEContext: TEContext;
  MacroID: TBuiltInFunctionID;
  SContext: TSContext;
  MacroParams: TVariableList;
  MParam: TIDVariable;
  i: Integer;
  ParamsBeginPos: Integer;
  ParamsBeginRow: Integer;
  ParamsText: string;
  Ctx: TSysFunctionContext;
  ASTExpr: TASTExpression;
begin
  ArgsCount := 0;
  Result := parser_CurTokenID;
  SContext := EContext.SContext;
  FuncDecl := TIDBuiltInFunction(CallExpr.Declaration);

  // парсинг аргументов
  if Result = token_openround then
  begin
    ParamsBeginPos := Parser.SourcePosition;
    ParamsBeginRow := Parser.LinePosition.Row;
    InitEContext(InnerEContext, EContext.SContext, ExprNested);
    while True do begin
      parser_NextToken(Scope);
      Result := ParseExpression(Scope, SContext, InnerEContext, ASTExpr);
      Expr := InnerEContext.Result;
      if Assigned(Expr) then begin
        {if Expr.DataType = SYSUnit._Boolean then
        begin
          if (Expr.ItemType = itVar) and Expr.IsAnonymous then
            Bool_CompleteImmediateExpression(InnerEContext, Expr);
        end;}
        Inc(ArgsCount);
        EContext.RPNPushExpression(Expr);
      end else begin
        // Добавляем пустой Expression для значения по умолчанию
        if FuncDecl.ParamsCount > 0 then
          EContext.RPNPushExpression(nil);
      end;
      case Result of
        token_coma: begin
          InnerEContext.Reset;
          continue;
        end;
        token_closeround: begin
          ParamsText := Copy(Parser.Source, ParamsBeginPos, Parser.SourcePosition - ParamsBeginPos - 1);
          Result := parser_NextToken(Scope);
          Break;
        end;
      else
        ERROR_INCOMPLETE_STATEMENT;
      end;
    end;
  end else begin
    ParamsBeginRow := 0;
  end;

  MacroParams := FuncDecl.ExplicitParams;
  for i := 0 to FuncDecl.ParamsCount - 1 do
  begin
    MParam := MacroParams[i];
    if Assigned(MParam.DefaultValue) and (ArgsCount < FuncDecl.ParamsCount) then
    begin
      Inc(ArgsCount);
      EContext.RPNPushExpression(MParam.DefaultValue);
    end;
  end;

  // проверка кол-ва аргуметнов
  if ArgsCount > FuncDecl.ParamsCount then
    ERROR_TOO_MANY_ACTUAL_PARAMS(CallExpr)
  else if ArgsCount < FuncDecl.ParamsCount then
    ERROR_NOT_ENOUGH_ACTUAL_PARAMS(CallExpr);

  Ctx.UN := Self;
  Ctx.Scope := Scope;
  Ctx.ParamsStr := ParamsText;
  Ctx.EContext := @EContext;
  Ctx.SContext := @SContext;

  MacroID := FuncDecl.FunctionID;
  case MacroID of
    bf_sysrtfunction: Expr := TIDSysRuntimeFunction(FuncDecl).Process(EContext);
    bf_sysctfunction: Expr := TIDSysCompileFunction(FuncDecl).Process(Ctx);
  else
    ERROR_FEATURE_NOT_SUPPORTED;
  end;
  if Assigned(Expr) then begin
    Expr.TextPosition := CallExpr.TextPosition;
    EContext.RPNPushExpression(Expr);
  end;
end;

function TASTDelphiUnit.ParseExceptOnSection(Scope: TScope; KW: TASTKWTryBlock; var SContext: TSContext): TTokenID;
var
  VarID, TypeID: TIdentifier;
  VarDecl, TypeDecl: TIDDeclaration;
  VarExpr: TASTExpression;
  NewScope: TScope;
  Item: TASTExpBlockItem;
  NewSContext: TSContext;
begin
  parser_ReadNextIdentifier(Scope, VarID);
  Result := parser_NextToken(Scope);
  if Result = token_colon then
  begin
    NewScope := TScope.Create(stLocal, Scope);
    VarDecl := TIDVariable.Create(NewScope, VarID);
    VarExpr := TASTExpression.Create(nil);
    VarExpr.AddDeclItem(VarDecl, parser_Position);
    InsertToScope(NewScope, VarDecl);
    parser_ReadNextIdentifier(Scope, TypeID);
    Result := parser_NextToken(Scope);
  end else begin
    TypeID := VarID;
    NewScope := Scope;
    VarDecl := TIDVariable.CreateAsTemporary(NewScope, nil);
    VarExpr := TASTExpression.Create(nil);
    VarExpr.AddDeclItem(VarDecl, parser_Position);
  end;

  TypeDecl := FindID(Scope, TypeID);
  // check type
  if Assigned(VarDecl) then
    VarDecl.DataType := TypeDecl as TIDType;

  parser_MatchToken(Result, token_do);

  Item := KW.AddExceptBlock(VarExpr);

  NewSContext := SContext.MakeChild(Item.Body);

  parser_NextToken(Scope);
  Result := ParseStatements(NewScope, NewSContext, False);
  parser_MatchSemicolon(Result);
  Result := parser_NextToken(Scope);
end;

function TASTDelphiUnit.ParseWhileStatement(Scope: TScope; var SContext: TSContext): TTokenID;
var
  Expression: TIDExpression;
  EContext: TEContext;
  BodySContext: TSContext;
  ASTExpr: TASTExpression;
  KW: TASTKWWhile;
begin
  KW := SContext.Add<TASTKWWhile>;

  // loop expression
  InitEContext(EContext, SContext, ExprRValue);
  parser_NextToken(Scope);
  Result := ParseExpression(Scope, SContext, EContext, ASTExpr);
  KW.Expression := ASTExpr;
  Expression := EContext.Result;
  CheckEmptyExpression(Expression);
  CheckBooleanExpression(Expression);

  BodySContext := SContext.MakeChild(KW.Body);

  // loop body
  parser_MatchToken(Result, token_do);
  Result := Parser_NextToken(Scope);
  if Result <> token_semicolon then
    Result := ParseStatements(Scope, BodySContext, False);
end;

function TASTDelphiUnit.ParseWithStatement(Scope: TScope; var SContext: TSContext): TTokenID;
var
  Decl: TIDDeclaration;
  EContext: TEContext;
  Expression, Expr: TIDExpression;
  WNextScope: TWithScope;
  WPrevScope: TScope;
  BodySContext: TSContext;
  ASTExpr: TASTExpression;
  KW: TASTKWWith;
begin
  WPrevScope := Scope;
  WNextScope := nil;
  KW := SContext.Add<TASTKWWith>;
  BodySContext := SContext.MakeChild(KW.Body);
  while True do begin
    Result := parser_NextToken(Scope);
    InitEContext(EContext, SContext, ExprRValue);
    parser_MatchToken(Result, token_identifier);
    Result := ParseExpression(Scope, SContext, EContext, ASTExpr);
    KW.AddExpression(ASTExpr);
    Expression := EContext.Result;

    // Проверка что тип сложный
    if not Expression.DataType.InheritsFrom(TIDStructure) then
      AbortWork(sStructTypeRequired, parser_PrevPosition);

    Decl := Expression.Declaration;
    // проверка на повторное выражение/одинаковый тип
    while Assigned(WNextScope) and (WNextScope.ScopeType = stWithScope) do begin
      Expr := WNextScope.Expression;
      if Expr.Declaration = Decl then
        AbortWork('Duplicate expression', parser_PrevPosition);
      if Expr.DataType = Decl.DataType then
        AbortWork('Duplicate expressions types', parser_PrevPosition);
      WNextScope := TWithScope(WNextScope.OuterScope);
    end;
    // создаем специальный "WITH Scope"
    WNextScope := TWithScope.Create(Scope, Expression);
    Scope.AddScope(WNextScope);
    with WNextScope do begin
      OuterScope := WPrevScope;
      InnerScope := TIDStructure(Decl.DataType).Members;
    end;

    case Result of
      token_coma: begin
        WPrevScope := WNextScope;
        Continue;
      end;
      token_do: begin
        parser_NextToken(Scope);
        Result := ParseStatements(WNextScope, BodySContext, False);
        Break;
      end;
      else begin
        parser_MatchToken(Result, token_do);
      end;
    end;
  end;
end;

function TASTDelphiUnit.Process_CALL(var EContext: TEContext): TIDExpression;
var
  PIndex, AIndex, ArgsCount,
  ParamsCount: Integer;
  PExpr: TIDCallExpression;
  UserArguments,
  CallArguments: TIDExpressions;
  ProcParams: TVariableList;
  Decl: TIDDeclaration;
  ResVar: TIDVariable;
  ProcDecl: TIDProcedure;
  ProcResult: TIDType;
  Param: TIDVariable;
  AExpr, NewExpr: TIDExpression;
  SContext: PSContext;
begin
  SContext := @EContext.SContext;
  // читаем декларацию функции
  PExpr := TIDCallExpression(EContext.RPNPopExpression());

  {вычитка явно указанных аргументов функции}
  ArgsCount := PExpr.ArgumentsCount;
  SetLength(UserArguments, ArgsCount);
  for AIndex := ArgsCount - 1 downto 0 do
    UserArguments[AIndex] := EContext.RPNPopExpression();

  ProcDecl := nil;
  Decl := PExpr.Declaration;


  {прямой вызов}
  if Decl.ItemType = itProcedure then
  begin
    ProcDecl := TIDProcedure(Decl);
    {if Assigned(ProcDecl.Struct) and (PExpr.Instance.Declaration is TIDField) then
    begin

    end;}
    if Assigned(ProcDecl.GenericDescriptor) and not Assigned(SContext.Proc.GenericDescriptor) then
    begin
      ProcDecl := SpecializeGenericProc(PExpr, UserArguments);
      ProcParams := ProcDecl.ExplicitParams;
      PExpr.Declaration := ProcDecl;
    end;

    {поиск подходящей декларации}
    if Assigned(ProcDecl.NextOverload) then begin
      ProcDecl := MatchOverloadProc(PExpr, UserArguments, ArgsCount);
      ProcParams := ProcDecl.ExplicitParams;
      PExpr.Declaration := ProcDecl;
    end else begin
      ProcParams := ProcDecl.ExplicitParams;
      MatchProc(PExpr, ProcParams, UserArguments);
    end;

    ProcResult := ProcDecl.ResultType;

  end else
  {вызов через переменную процедурного типа}
  if PExpr.DataTypeID = dtProcType then begin

    Decl := PExpr.DataType;

    ProcParams := TIDProcType(Decl).Params;
    ProcResult := TIDProcType(Decl).ResultType;
    MatchProc(PExpr, ProcParams, UserArguments);
  end else begin
    AbortWorkInternal(msgProcOrProcVarRequired, parser_Position);
    ProcResult := nil;
  end;

  ParamsCount := Length(ProcParams);
  {если все аргументы явно указаны}
  if ParamsCount = ArgsCount then
  begin
    for AIndex := 0 to ParamsCount - 1 do
    begin
      Param := ProcParams[AIndex];
      AExpr := UserArguments[AIndex];

      {если аргумент константый дин. массив то делаем доп. проверку}
      //if AExpr.Declaration is TIDDynArrayConstant then
      //  AExpr := CheckConstDynArray(SContext, Param.DataType, AExpr);

      {проверка диаппазона для константных аргументов}
      if AExpr.IsConstant then
        CheckConstValueOverflow(AExpr, Param.DataType);

      {подбираем implicit оператор}
      AExpr := MatchImplicit3(SContext^, AExpr, Param.DataType);
      CheckNotNullExpression(Param, AExpr);

      {если параметр - constref и аргумент - константа, то создаем временную переменную}
      //if (VarConstRef in Param.Flags) and (AExpr.ItemType = itConst) then
      //  AExpr := GenConstToVar(SContext, AExpr, Param.DataType);

      {если параметр - метод, получаем ссылку на него}
      if (AExpr.ItemType = itProcedure) and (not TIDProcType(AExpr.DataType).IsStatic) then
      begin
        NewExpr := GetTMPVarExpr(SContext^, AExpr.DataType, AExpr.TextPosition);
        AExpr := NewExpr;
      end;

      UserArguments[AIndex] := AExpr;

      if Param.VarReference then
      begin
        CheckVarExpression(AExpr, vmpPassArgument);
        {проверка на строгость соответствия типов}
        if Param.DataType.ActualDataType <> AExpr.DataType.ActualDataType then
        begin
          if not ((Param.DataType.DataTypeID = dtPointer) and
             (AExpr.DataType.DataTypeID = dtPointer) {and
             (TIDPointer(Param.DataType).ReferenceType = TIDPointer(AExpr.DataType).ReferenceType) !!!!!! }) then
          ERROR_REF_PARAM_MUST_BE_IDENTICAL(AExpr);
        end;
      end;
    end;
    CallArguments := UserArguments;
  end else
  {если некоторые аргументы опущены}
  begin
    AIndex := 0;
    PIndex := 0;
    SetLength(CallArguments, ParamsCount);
    while (PIndex < ParamsCount) do
    begin
      Param := ProcParams[PIndex];
      if AIndex < ArgsCount then begin
        AExpr := UserArguments[AIndex];
        Inc(AIndex);
      end else
        AExpr := nil;
      {подстановка аргуметов по умолчанию}
      if Assigned(AExpr) then
        CallArguments[PIndex] := AExpr
      else begin
        AExpr := Param.DefaultValue;
        if not Assigned(AExpr) then
          ERROR_NOT_ENOUGH_ACTUAL_PARAMS(PExpr);
        CallArguments[PIndex] := AExpr;
      end;

      {если аргумент константый дин. массив то делаем доп. проверку}
      //if AExpr.Declaration is TIDDynArrayConstant then
      //  AExpr := CheckConstDynArray(SContext, Param.DataType, AExpr);     // нужно ли????

      {проверка диаппазона для константных аргументов}
      if AExpr.IsConstant then
        CheckConstValueOverflow(AExpr, Param.DataType);

      {подбираем implicit оператор}
      AExpr := MatchImplicit3(SContext^, AExpr, Param.DataType);

      {если параметр - constref и аргумент - константа, то создаем временную переменную}
      //if (VarConstRef in Param.Flags) and (AExpr.ItemType = itConst) then
      //  AExpr := GenConstToVar(SContext, AExpr, Param.DataType);

      {если параметр - метод, получаем ссылку на него}
      if (AExpr.ItemType = itProcedure) and (not TIDProcType(AExpr.DataType).IsStatic) then
      begin
        NewExpr := GetTMPVarExpr(SContext^, AExpr.DataType, AExpr.TextPosition);
        AExpr := NewExpr;
      end;

      CallArguments[PIndex] := AExpr;

      {если параметр открытый массив}
      if Param.DataTypeID = dtOpenArray then
      begin
        {добовляем скрытй аргумент}
        EContext.RPNPushExpression(AExpr);
        Inc(PIndex);
        //CallArguments[PIndex] := ProcessBuiltin_Length(EContext); todo:
      end;

      Inc(PIndex);
      {если параметр передается по ссылке, проверяем что аргумент можно менять}
      if Param.VarReference then
      begin
        CheckVarExpression(AExpr, vmpPassArgument);
        {проверка на строгость соответствия типов}
        if Param.DataType.ActualDataType <> AExpr.DataType.ActualDataType then
          ERROR_REF_PARAM_MUST_BE_IDENTICAL(AExpr);
      end;
    end;
  end;

  {конструирование обьекта}
  if (Assigned(ProcDecl) and (pfConstructor in ProcDecl.Flags)) then
  begin
    //Result := process_CALL_constructor(SContext, PExpr, CallArguments);
    ERROR_FEATURE_NOT_SUPPORTED;
    Exit;
  end;

  {результат функции}
  if Assigned(ProcResult) then begin
    ResVar := GetTMPVar(SContext^, ProcResult);
    ResVar.IncludeFlags([VarTmpResOwner]);
    Result := TIDExpression.Create(ResVar, PExpr.TextPosition);
  end else
    Result := nil;

  {если вызов был коссвенным, инлайн невозможен}
  Decl := PExpr.Declaration;
  if Decl.ItemType = itVar then
  begin
    {если переменная - поле класа, получаем ссылку на поле}
    //
    Exit;
  end;

  {INLINE подстановка (пока только если инлайн процедура готова)}
  if (pfInline in ProcDecl.Flags) and
     (ProcDecl.IsCompleted) and
     (ProcDecl <> SContext.Proc) then
  begin
    // todo:
  end else begin
    // todo:
  end;
end;

function TASTDelphiUnit.Process_CALL_direct(const SContext: TSContext; PExpr: TIDCallExpression; CallArguments: TIDExpressions): TIDExpression;
var
  AIndex, ArgsCount: Integer;
  ProcDecl: TIDProcedure;
  ProcParams: TVariableList;
  ResVar: TIDVariable;
  ProcResult: TIDType;
  Param: TIDVariable;
  AExpr: TIDExpression;
begin
(*  ArgsCount := Length(CallArguments);

  ProcDecl := PExpr.AsProcedure;
  ProcParams := ProcDecl.ExplicitParams;

  if Assigned(ProcDecl.GenericDescriptor) then
  begin
    ProcDecl := SpecializeGenericProc(PExpr, CallArguments);
    ProcParams := ProcDecl.ExplicitParams;
    PExpr.Declaration := ProcDecl;
  end;

  {если все аргументы явно указаны}
  for AIndex := 0 to ArgsCount - 1 do
  begin
    Param := ProcParams[AIndex];
    AExpr := CallArguments[AIndex];

    {если аргумент константый дин. массив то делаем доп. проверку}
    if AExpr.Declaration is TIDDynArrayConstant then
      AExpr := CheckConstDynArray(SContext, Param.DataType, AExpr);

    {проверка диаппазона для константных аргументов}
    if AExpr.IsConstant then
      CheckConstValueOverflow(AExpr, Param.DataType);

    {подбираем и если надо вызываем implicit оператор}
    AExpr := MatchImplicit3(SContext, AExpr, Param.DataType);

    {если параметр - constref и аргумент - константа, то создаем временную переменную}
    if (VarConstRef in Param.Flags) and (AExpr.ItemType = itConst) then
      AExpr := GenConstToVar(AExpr, Param.DataType);

    CallArguments[AIndex] := AExpr;

    if Param.Reference then
      CheckVarExpression(AExpr, vmpPassArgument);
  end;

  {результат функции}
  ProcResult := ProcDecl.ResultType;
  if Assigned(ProcResult) then begin
    ResVar := GetTMPVar(SContext, ProcResult);
    ResVar.IncludeFlags([VarTmpResOwner]);
    Result := TIDExpression.Create(ResVar, PExpr.TextPosition);
  end else
    Result := nil;

  {INLINE подстановка (пока только если инлайн процедура готова)}
  if (pfInline in ProcDecl.Flags) and
     (ProcDecl.IsCompleted) and
     (ProcDecl <> SContext.Proc) then
  begin
    SContext.IL.InlineProc(SContext.Proc, ProcDecl, nil, PExpr.Instance, Result, CallArguments);
  end else begin
    if Assigned(PExpr.Instance) and (ProcDecl.Struct <> PExpr.Instance.DataType) then
      CallInstruction := TIL.IL_InheritedCall(PExpr, Result, PExpr.Instance, CallArguments)
    else
      CallInstruction := TIL.IL_ProcCall(PExpr, Result, PExpr.Instance, CallArguments);
    ILWrite(SContext, CallInstruction);
  end;
  ProcDecl.IncRefCount(1);     *)
end;

function GetOperatorInstance(Op: TOperatorID; Left, Right: TIDExpression): TIDExpression;
var
  LDT, RDT: TIDType;
  Decl: TIDDeclaration;
begin
  LDT := Left.DataType.ActualDataType;
  RDT := Right.DataType.ActualDataType;
  // поиск оператора (у левого операнда)
  Decl := LDT.BinarOperator(Op, RDT);
  if Assigned(Decl) then
    Exit(Left);
  // поиск оператора (у правого операнда)
  Decl := RDT.BinarOperatorFor(Op, LDT);
  if Assigned(Decl) then
    Exit(Right);
  Result := nil
end;

function TASTDelphiUnit.Process_operators2(var EContext: TEContext; OpID: TOperatorID): TIDExpression;
var
  Left, Right: TIDExpression;
  Op: TIDDeclaration;
  TmpVar: TIDVariable;
begin
  Result := nil;
  case OpID of
    opAssignment: Process_operator_Assign(EContext);
    opNegative: Result := Process_operator_neg(EContext);
    opNot: Result := Process_operator_not(EContext);
    opPositive: Result := EContext.RPNPopExpression;
    opDereference: Result := Process_operator_Deref(EContext);
    opCall: Result := Process_CALL(EContext);
    opPeriod: Result := Process_operator_Period(EContext);
    opAddr: Result := Process_operator_Addr(EContext);
    opIs: Result := Process_operator_Is(EContext);
    opAs: Result := Process_operator_As(EContext);
  else begin
      // Читаем первый операнд
      Right := EContext.RPNPopExpression();
      // Читаем второй операнд
      Left := EContext.RPNPopExpression();

      if OpID in [opShiftLeft, opShiftRight] then
        Op := MatchBinarOperator(EContext.SContext, OpID, Left, Left)
      else
        Op := MatchBinarOperator(EContext.SContext, OpID, Left, Right);

      {if not Assigned(OP) and Left.IsDynArrayConst then
        Op := MatchBinarOperatorWithTuple(SContext, OpID, Left, Right);

      if not Assigned(OP) and Right.IsDynArrayConst then
        Op := MatchBinarOperatorWithTuple(SContext, OpID, Right, Left);   }

      if not Assigned(Op) then
        Op := MatchBinarOperatorWithImplicit(EContext.SContext, OpID, Left, Right);

      // если аргументы - константы, производим константные вычисления
      if (Left.IsConstant and Right.IsConstant) and
         (Left.Declaration.ClassType <> TIDSizeofConstant) and
         (Right.Declaration.ClassType <> TIDSizeofConstant) then
      begin
        Result := ProcessConstOperation(Left, Right, OpID);
        Exit;
      end else begin
        TmpVar := GetTMPVar(EContext, TIDType(Op));
        Result := TIDExpression.Create(TmpVar, Left.TextPosition);
      end;

      if Op.ItemType = itType then
      begin
        case OpID of
          opAdd: begin
            // если результат строка или дин. массив - помечаем переменную
            if Result.DataTypeID in [dtDynArray, dtString, dtAnsiString] then
              Result.AsVariable.IncludeFlags([VarTmpResOwner]);
          end;
          opSubtract: ;//ILWrite(SContext, TIL.IL_Sub(Result, Left, Right));
          opMultiply: ;//ILWrite(SContext, TIL.IL_Mul(Result, Left, Right));
          opDivide: ;//ILWrite(SContext, TIL.IL_Div(Result, Left, Right));
          opIntDiv: ;//ILWrite(SContext, TIL.IL_IntDiv(Result, Left, Right));
          opModDiv: ;//ILWrite(SContext, TIL.IL_ModDiv(Result, Left, Right));
          opEqual,
          opNotEqual,
          opLess,
          opLessOrEqual,
          opGreater,
          opGreaterOrEqual: begin
            Result := GetBoolResultExpr(Result);
            //ILWrite(SContext, TIL.IL_Cmp(Left, Right));
            {освобожадем временные переменные}
            //ReleaseExpression(SContext, Left);
            //ReleaseExpression(SContext, Right);
            //ILWrite(SContext, TIL.IL_JmpNext(Left.Line, cNone, nil));
            //Bool_AddExprNode(EContext, SContext.ILLast, TILCondition(Ord(OpID) - Ord(opEqual) + 1));
            Exit;
          end;
          //opIn: Result := Process_operator_In(EContext, Left, Right);
          opAnd, opOr, opXor, opShiftLeft, opShiftRight: begin
            if (OpID in [opAnd, opOr]) and (TmpVar.DataType = SYSUnit._Boolean) then
            begin
              // логические операции
              Result := GetBoolResultExpr(Result);
              //Process_operator_logical_AND_OR(EContext, OpID, Left, Right, Result);
            end;
          end;
        else
          ERROR_UNSUPPORTED_OPERATOR(OpID);
        end;
      end else
      if Op.ItemType = itProcedure then begin
        // вызов перегруженного бинарного оператора
        Result := TIDCallExpression.Create(Op);
        Result.TextPosition := Left.TextPosition;
        TIDCallExpression(Result).ArgumentsCount := 2;
        TIDCallExpression(Result).Instance := GetOperatorInstance(OpID, Left, Right);
        Result := Process_CALL_direct(EContext.SContext, TIDCallExpression(Result), TIDExpressions.Create(Left, Right));
      end;
    end;
  end;
end;

function TASTDelphiUnit.Process_operator_Addr(var EContext: TEContext): TIDExpression;
var
  Expr: TIDExpression;
  TmpDecl: TIDVariable;
  DataType: TIDType;
begin
  Expr := EContext.RPNPopExpression();
  if (Expr.ItemType <> itVar) and
     ((Expr.ItemType = itConst) and not (Expr.DataType.DataTypeID in [dtRecord, dtStaticArray, dtGuid])) and
     (Expr.ItemType <> itProcedure) then
    ERROR_VAR_OR_PROC_EXPRESSION_REQUIRED(Expr);

  DataType := Expr.DataType.DefaultReference;
  if DataType = nil then
  begin
    DataType := Expr.DataType.GetDefaultReference(ImplSection);
    AddType(DataType);
  end;

  if Expr.IsTMPVar and Expr.AsVariable.Reference then
  begin
    Result := GetTMPVarExpr(EContext.SContext, DataType, Expr.TextPosition);
    Result.AsVariable.Absolute := Expr.AsVariable;
  end else begin
    TmpDecl := GetTMPVar(EContext.SContext, DataType);
    TmpDecl.IncludeFlags([VarNotNull]);
    Result := TIDExpression.Create(TmpDecl, Expr.TextPosition);
  end;
end;

function TASTDelphiUnit.Process_operator_As(var EContext: TEContext): TIDExpression;
var
  Src, Dst: TIDExpression;
begin
  Dst := EContext.RPNPopExpression();
  Src := EContext.RPNPopExpression();
  CheckClassOrIntfType(Src.DataType, Src.TextPosition);
  CheckClassOrIntfType(Dst);
  Result := GetTMPVarExpr(EContext, Dst.AsType, Dst.TextPosition);
end;

procedure TASTDelphiUnit.Process_operator_Assign(var EContext: TEContext);
begin
  //
end;

function TASTDelphiUnit.Process_operator_Deref(var EContext: TEContext): TIDExpression;
var
  Src: TIDExpression;
  RefDt: TIDType;
begin
  Src := EContext.RPNPopExpression();
  CheckPointerType(Src);
  RefDt := TIDPointer(Src.DataType).ReferenceType;
  if not Assigned(RefDt) then
    AbortWork('Cannot dereference the untyped pointer', Src.TextPosition);

  Result := TIDDrefExpression.Create(Src);
end;

function TASTDelphiUnit.Process_operator_In(var EContext: TEContext; const Left, Right: TIDExpression): TIDExpression;
begin
  Result := TIDExpression.Create(GetTMPVar(EContext, SYSUnit._Boolean));
end;

function TASTDelphiUnit.Process_operator_Is(var EContext: TEContext): TIDExpression;
var
  Src, Dst: TIDExpression;
begin
  Dst := EContext.RPNPopExpression();
  Src := EContext.RPNPopExpression();
  CheckClassOrIntfType(Src.DataType, Src.TextPosition);
  CheckClassOrIntfType(Dst);
  Result := GetTMPVarExpr(EContext, SYSUnit._Boolean, Dst.TextPosition);
end;

function TASTDelphiUnit.Process_operator_neg(var EContext: TEContext): TIDExpression;
var
  Right: TIDExpression;
  OperatorItem: TIDType;
begin
  // Читаем операнд
  Right := EContext.RPNPopExpression();

  //ReleaseExpression(EContext.SContext, Right);

  OperatorItem := MatchUnarOperator(opNegative, Right.DataType);
  if not Assigned(OperatorItem) then
    ERROR_NO_OVERLOAD_OPERATOR_FOR_TYPES(opNegative, Right);

  if (Right.ItemType = itConst) and (Right.ClassType <> TIDSizeofConstant) then
    Result := ProcessConstOperation(Right, Right, opNegative)
  else begin
    Result := GetTMPVarExpr(EContext, OperatorItem, Right.TextPosition);
  end;
end;

function TASTDelphiUnit.Process_operator_not(var EContext: TEContext): TIDExpression;
var
  Right: TIDExpression;
  OperatorItem: TIDType;
begin
  // Читаем операнд
  Right := EContext.RPNPopExpression();

  OperatorItem := MatchUnarOperator(opNot, Right.DataType);
  if not Assigned(OperatorItem) then
    ERROR_NO_OVERLOAD_OPERATOR_FOR_TYPES(opNot, Right);

  if (Right.ItemType = itConst) and (Right.ClassType <> TIDSizeofConstant) then
    Result := ProcessConstOperation(Right, Right, opNot)
  else begin
    // если это просто выражение (not Bool_Value)
    if (Right.DataTypeID = dtBoolean) and not (Right is TIDBoolResultExpression) then
    begin
       //Bool_AddExprNode(EContext, EContext.SContext.ILLast, cNonZero);
    end;
    {if not Assigned(EContext.LastBoolNode) then
    begin
      // т.к. это не логичиское отрицание
      // генерируем код бинарного отрицания
      Result := GetTMPVarExpr(EContext.SContext, OperatorItem, Right.TextPosition);
      ILWrite(EContext.SContext, TIL.IL_Not(Result, Right));
    end else} begin
      var TmpVar := GetTMPVar(EContext, SYSUnit._Boolean);
      Result := TIDBoolResultExpression.Create(TmpVar, parser_Position);
    end;
  end;
end;

function TASTDelphiUnit.Process_operator_Period(var EContext: TEContext): TIDExpression;
var
  ValueType: TIDDeclaration;
  RangeType: TIDRangeType;
  Decl: TIDDeclaration;
  LB, HB: TIDExpression;
  RExpression: TIDRangeExpression;
begin
  HB := EContext.RPNPopExpression();
  LB := EContext.RPNPopExpression();

  CheckEmptyExpression(LB);
  CheckEmptyExpression(HB);

  CheckOrdinalExpression(LB);
  CheckOrdinalExpression(HB);

  ValueType := MatchImplicit(LB.DataType, HB.DataType);
  if not Assigned(ValueType) then
    AbortWork(sTypesMustBeIdentical, HB.TextPosition);

  RangeType := TIDRangeType.CreateAsSystem(nil, 'Range of ' + ValueType.Name);
  RangeType.ElementType := ValueType as TIDType;

  if LB.IsConstant and HB.IsConstant then
  begin
    if TIDConstant(LB.Declaration).CompareTo(TIDConstant(HB.Declaration)) > 0 then
      AbortWork(sLowerBoundExceedsHigherBound, HB.TextPosition);
  end;

  RExpression.LBExpression := LB;
  RExpression.HBExpression := HB;
  Decl := TIDRangeConstant.CreateAnonymous(nil, RangeType, RExpression);
  Result := TIDExpression.Create(Decl, HB.TextPosition);
end;

procedure TASTDelphiUnit.CheckIncompletedProcs(ProcSpace: PProcSpace);
begin
//  inherited;

end;

procedure TASTDelphiUnit.CheckLabelExpression(const Expr: TIDExpression);
begin
  if Expr.ItemType <> itLabel then
    AbortWork('LABEL required', Expr.TextPosition);
end;

procedure TASTDelphiUnit.CheckLabelExpression(const Decl: TIDDeclaration);
begin
  if Decl.ItemType <> itLabel then
    AbortWork('LABEL required', parser_Position);
end;

function TASTDelphiUnit.Compile(RunPostCompile: Boolean): TCompilerResult;
var
  Token: TTokenID;
  Scope: TScope;
  Platform: TIDPlatform;
begin
  Result := CompileFail;
  Messages.Clear;
  FRCPathCount := 1;
  try
    Parser.First;
    Scope := IntfSection;
    ParseUnitDecl(Scope);
    Token := parser_NextToken(Scope);
    while true do begin
      case Token of
        token_type: begin
          CheckIntfSectionMissing(Scope);
          Token := ParseNamedTypeDecl(Scope);
        end;
        token_asm: begin
          CheckIntfSectionMissing(Scope);
          Token := ParseAsmSpecifier(Platform);
          case Token of
            token_function: Token := ParseProcedure(Scope, ptFunc);
            token_procedure: Token := ParseProcedure(Scope, ptProc);
            else
              ERROR_FEATURE_NOT_SUPPORTED;
          end;
        end;
        token_uses: Token := ParseUsesSection(Scope);
        token_function: begin
          CheckIntfSectionMissing(Scope);
          Token := ParseProcedure(Scope, ptFunc);
        end;
        token_procedure: begin
          CheckIntfSectionMissing(Scope);
          Token := ParseProcedure(Scope, ptProc);
        end;
        token_constructor: begin
          CheckIntfSectionMissing(Scope);
          Token := ParseProcedure(Scope, ptConstructor);
        end;
        token_destructor: begin
          CheckIntfSectionMissing(Scope);
          Token := ParseProcedure(Scope, ptDestructor);
        end;
        token_operator: begin
          CheckIntfSectionMissing(Scope);
          Token := ParseOperator(Scope, nil);
        end;
        token_const: begin
          CheckIntfSectionMissing(Scope);
          Token := ParseConstSection(Scope);
        end;
        token_class: begin
          CheckIntfSectionMissing(Scope);
          Token := parser_NextToken(Scope);
          case Token of
            token_function: Token := ParseProcedure(Scope, ptClassFunc);
            token_procedure: Token := ParseProcedure(Scope, ptClassProc);
            token_constructor: Token := ParseProcedure(Scope, ptClassConstructor);
            token_destructor: Token := ParseProcedure(Scope, ptClassDestructor);
          else
            ERROR_FEATURE_NOT_SUPPORTED;
          end;
        end;
        token_weak: begin
          CheckIntfSectionMissing(Scope);
          parser_NextToken(Scope);
          Token := ParseVarSection(Scope, vLocal, nil, True);
        end;
        token_var: begin
          CheckIntfSectionMissing(Scope);
          parser_NextToken(Scope);
          Token := ParseVarSection(Scope, vLocal, nil, False);
        end;
        token_exports: begin
          // todo:
          Token := parser_SkipTo(Scope, token_semicolon);
          Token := parser_NextToken(Scope);
        end;
        token_interface: begin
          Scope := IntfSection;
          Token := parser_NextToken(Scope);
        end;
        token_implementation: begin
          CheckIntfSectionMissing(Scope);
          Scope := ImplSection;
          Token := parser_NextToken(Scope);
        end;
        token_end: begin
          parser_MatchToken(parser_NextToken(Scope), token_dot);
          Token := parser_NextToken(Scope);
          if Token <> token_eof then
            HINT_TEXT_AFTER_END;
          Break;
        end;
        token_initialization: Token := ParseInitSection;
        token_finalization: Token := ParseFinalSection;
        token_eof: Break;
      else
        if Token >= token_cond_define then
        begin
          Token := ParseCondStatements(Scope, Token);
          continue;
        end;
        ERROR_KEYWORD_EXPECTED;
      end;
    end;
    if RunPostCompile then
      PostCompileProcessUnit;
    Result := CompileSuccess;
    //FCompiled := True;
  except
    on e: ECompilerStop do Exit();
    on e: ECompilerSkip do Exit(CompileSkip);
    on e: ECompilerAbort do PutMessage(ECompilerAbort(e).CompilerMessage^);
    on e: Exception do PutMessage(cmtInteranlError, e.Message, parser_Position);
  end;
end;

function TASTDelphiUnit.CompileIntfOnly: TCompilerResult;
begin
  Result := Compile;
end;

function TASTDelphiUnit.GetFirstConst: TASTDeclaration;
begin
  Result := nil;
end;

function TASTDelphiUnit.GetFirstFunc: TASTDeclaration;
begin
  Result := ProcSpace.First;
end;

function TASTDelphiUnit.GetFirstType: TASTDeclaration;
begin
  Result := TypeSpace.First;
end;

function TASTDelphiUnit.GetFirstVar: TASTDeclaration;
begin
  Result := VarSpace.First;
end;

function TASTDelphiUnit.GetModuleName: string;
begin
  Result := _ID.Name;
end;

function TASTDelphiUnit.GetTMPVar(const EContext: TEContext; DataType: TIDType): TIDVariable;
begin
  Result := TIDProcedure(EContext.Proc).GetTMPVar(DataType);
end;

function TASTDelphiUnit.GetTMPVar(const SContext: TSContext; DataType: TIDType): TIDVariable;
begin
  if Assigned(SContext.Proc) then
    Result := SContext.Proc.GetTMPVar(DataType)
  else
    Result := InitProc.GetTMPVar(DataType);
end;

function TASTDelphiUnit.GetTMPVarExpr(const EContext: TEContext; DataType: TIDType; const TextPos: TTextPosition): TIDExpression;
begin
  Result := TIDExpression.Create(GetTMPVar(EContext, DataType), TextPos);
end;

function TASTDelphiUnit.GetTMPVarExpr(const SContext: TSContext; DataType: TIDType; const TextPos: TTextPosition): TIDExpression;
begin
  Result := TIDExpression.Create(GetTMPVar(SContext, DataType), TextPos);
end;

function TASTDelphiUnit.GetTMPRef(const SContext: TSContext; DataType: TIDType): TIDVariable;
begin
  Result := SContext.Proc.GetTMPRef(DataType);
end;

function TASTDelphiUnit.GetTMPRefExpr(const SContext: TSContext; DataType: TIDType; const TextPos: TTextPosition): TIDExpression;
begin
  Result := TIDExpression.Create(GetTMPRef(SContext, DataType));
  Result.TextPosition := TextPos;
end;

function TASTDelphiUnit.GetTMPRefExpr(const SContext: TSContext; DataType: TIDType): TIDExpression;
begin
  Result := TIDExpression.Create(GetTMPRef(SContext, DataType));
end;

procedure TASTDelphiUnit.InitEContext(var EContext: TEContext; const SContext: TSContext; EPosition: TExpessionPosition);
begin
  EContext.Initialize(SContext, Process_operators2);
  EContext.EPosition := EPosition;
end;

function TASTDelphiUnit.MatchImplicit3(const SContext: TSContext; Source: TIDExpression; Dest: TIDType): TIDExpression;
begin
  Result := MatchImplicitOrNil(SContext, Source, Dest);
    if not Assigned(Result) then
      ERROR_INCOMPATIBLE_TYPES(Source, Dest);
end;

function TASTDelphiUnit.MatchArrayImplicit(const SContext: TSContext; Source: TIDExpression; DstArray: TIDArray): TIDExpression;
  function CreateAnonymousDynArrayType(ElementDataType: TIDType): TIDArray;
  begin
    Result := TIDDynArray.CreateAsAnonymous(ImplSection);
    Result.ElementDataType := ElementDataType;
    AddType(Result);
  end;
var
  i, ACnt: Integer;
  CArray: TIDDynArrayConstant;
  SExpr: TIDExpression;
  DstElementDT: TIDType;
  BoundType: TIDOrdinal;
  NeedCallImplicits: Boolean;
  ImplicitCast: TIDDeclaration;
  TmpArrayType: TIDArray;
  Expr: TIDExpression;
  EContext: TEContext;
  SrcArray: TIDArray;
begin
  Result := nil;
  SrcArray := Source.DataType as TIDArray;
  if Source.Declaration is TIDDynArrayConstant then
  begin
    CArray := Source.AsArrayConst;
    DstElementDT := DstArray.ElementDataType;

    // проверка каждого элемента
    NeedCallImplicits := False;
    ACnt := CArray.ArrayLength;
    for i := 0 to ACnt - 1 do begin
      SExpr := CArray.Value[i];
      ImplicitCast := CheckImplicit(SExpr, DstElementDT);
      if not Assigned(ImplicitCast) then
        ERROR_INCOMPATIBLE_TYPES(SExpr, DstElementDT);

      if ImplicitCast.ItemType = itProcedure then
        NeedCallImplicits := True;
    end;

    {если тип вектора не совподает с типом приемника, подгоняем тип под нужный}
    if TIDArray(CArray.DataType).ElementDataType <> DstElementDT then
      CArray.DataType := CreateAnonymousDynArrayType(DstElementDT);

    // если типы массивов требуют вызовов пользовательских операторов implicit
    // то создаем временный статический массив, и для каждого элемента вызываем пользовательский implicit
    if NeedCallImplicits then
    begin
      if DstArray.DataTypeID = dtOpenArray then
      begin
        // для открытых массивов создаем анонимный тип - статический массив
        TmpArrayType := TIDArray.CreateAnonymousStatic1Dim(ImplSection, DstElementDT, ACnt, BoundType);
        AddType(BoundType);
        AddType(TmpArrayType);
        Result := GetTMPVarExpr(SContext, TmpArrayType, parser_Position);
      end else begin
        // для динамических массивов просто выделяем память
        Result := GetTMPVarExpr(SContext, DstArray, parser_Position);
        //ILWrite(SContext, TIL.IL_DAlloc(Result, IntConstExpression(ACnt)));
      end;

      for i := 0 to ACnt - 1 do begin
        SExpr := CArray.Value[i];
        InitEContext(EContext, SContext, ExprNested);
        Expr := TIDMultiExpression.CreateAnonymous(DstElementDT, [Result, IntConstExpression(i)]);
        EContext.RPNPushExpression(Expr);
        EContext.RPNPushExpression(SExpr);
        Process_operator_Assign(EContext);
      end;
    end else
      Result := Source;
  end else begin
    // приведение динамических массивов
    if (SrcArray.DataTypeID = DstArray.DataTypeID) and (SrcArray.DimensionsCount = DstArray.DimensionsCount) then
    begin
      for i := 0 to SrcArray.DimensionsCount - 1 do
      begin
        // todo: добавить глубокую проверку типов (range/enum/...)
        if SrcArray.Dimensions[i].ActualDataType <> DstArray.Dimensions[i].ActualDataType then
          Exit(nil);
      end;
      if SrcArray.ElementDataType.ActualDataType <> DstArray.ElementDataType.ActualDataType then
        Exit(nil);
      Result := Source;
    end;
  end;
end;

class function TASTDelphiUnit.MatchArrayImplicitToRecord(Source: TIDExpression; Destination: TIDStructure): TIDExpression;
var
  i, TupleItemsCount: Integer;
  ItemExpr: TIDExpression;
  Field: TIDVariable;
  FieldDataType: TIDType;
  Tuple: TIDDynArrayConstant;
  ImplicitCast: TIDDeclaration;
//  NeedCallImplicits: Boolean;
begin
  if not Source.IsDynArrayConst then
    Exit(nil);
  Tuple := Source.AsDynArrayConst;
  TupleItemsCount := Tuple.ArrayLength;

  if TupleItemsCount <> Destination.FieldsCount then
    AbortWork('Invalid fields count', Source.TextPosition);

  Field := Destination.FirstField;

  for i := 0 to TupleItemsCount - 1 do begin
    ItemExpr := Tuple.Value[i];
    FieldDataType := Field.DataType.ActualDataType;
    ImplicitCast := CheckImplicit(ItemExpr, FieldDataType);

    if not Assigned(ImplicitCast) then
      ERROR_INCOMPATIBLE_TYPES(ItemExpr, FieldDataType);

    {if ImplicitCast.ItemType = itProcedure then
      NeedCallImplicits := True;}

    Field := TIDVariable(Field.NextItem);
  end;
  Result := Source;
end;

function TASTDelphiUnit.MatchRecordImplicit(const SContext: TSContext; Source: TIDExpression; DstRecord: TIDRecord): TIDExpression;
var
  SrcRecord: TIDRecord;
  i: Integer;
  SrcFld, DstFld: TIDVariable;
begin
  SrcRecord :=  Source.DataType as TIDRecord;
  if SrcRecord.FieldsCount <> DstRecord.FieldsCount then
    Exit(nil);

  SrcFld := SrcRecord.Fields.First;
  DstFld := DstRecord.Fields.First;

  for i := 0 to SrcRecord.FieldsCount - 1 do
  begin
    if SrcFld.DataType.ActualDataType <> DstFld.DataType.ActualDataType then
      Exit(nil);
    SrcFld :=  TIDVariable(SrcFld.NextItem);
    DstFld :=  TIDVariable(DstFld.NextItem);
  end;
  Result := Source;
end;

function TASTDelphiUnit.MatchImplicitOrNil(const SContext: TSContext; Source: TIDExpression; Dest: TIDType): TIDExpression;
  function CheckAndCallOperator(const SContext: TSContext; OperatorDecl: TIDDeclaration; Src: TIDExpression): TIDExpression;
  var
    CallExpr: TIDCallExpression;
  begin
    CallExpr := TIDCallExpression.Create(OperatorDecl, Src.TextPosition);
    CallExpr.ArgumentsCount := 1;
    Result := Process_CALL_direct(SContext, CallExpr, TIDExpressions.Create(Src));
  end;
var
  SDataType: TIDType;
  Decl: TIDDeclaration;
  SrcDTID, DstDTID: TDataTypeID;
begin
  {$IFDEF DEBUG}
  Result := nil;
  if not Assigned(Source.DataType) then
    AbortWorkInternal('Source data type is not assigned', parser_Position);
  {$ENDIF}

  SDataType := Source.DataType.ActualDataType;
  Dest := Dest.ActualDataType;

  SrcDTID := SDataType.DataTypeID;
  DstDTID := Dest.DataTypeID;

  // если источник элемент битового набора(dtSet[i])
  if (SDataType.DataTypeID = dtBoolean) and (Dest.DataTypeID = dtBoolean) and
     (Source is TIDMultiExpression) and
     (TIDMultiExpression(Source).Items[0].DataTypeID = dtSet) then
  begin
    Decl := GetTMPVar(SContext, SYSUnit._Boolean);
    Result := TIDExpression.Create(Decl);
    Exit;
  end;

  //todo сделать преобразование констант !!! string->ansistring->string

  // ищем явно определенный implicit у источника
  Decl := SDataType.GetImplicitOperatorTo(Dest);
  if Decl is TIDInternalOpImplicit then
  begin
    Decl := TIDInternalOpImplicit(Decl).Check(Source, Dest);
    if Assigned(Decl) then
      Exit(Source);
    Decl := nil;
  end;

  if not Assigned(Decl) then
  begin
    // ищем явно определенный implicit у приемника
    Decl := Dest.GetImplicitOperatorFrom(SDataType);
    if not Assigned(Decl) then
    begin
      // если не нашли точных имплиситов, ищем подходящий (у источника)
      Decl := SDataType.FindImplicitOperatorTo(Dest);
      if not Assigned(Decl) then
      begin
        // если не нашли точных имплиситов, ищем подходящий (у приемника)
        Decl := Dest.FindImplicitOperatorFrom(SDataType);
        if not Assigned(Decl) then
        begin
          { проверка на nullpointer }
          if (Source.Declaration = SYSUnit._NullPtrConstant) and
             (Dest.IsReferenced or (Dest is TIDProcType)) then
            Exit(Source);

          if (SrcDTID = dtPointer) and (DstDTID = dtPointer) then
          begin
            // it needs to check
            if (TIDPointer(SDataType).ReferenceType = nil) or
               (TIDPointer(Dest).ReferenceType = nil) then
              Exit(Source);

            if TIDPointer(SDataType).ReferenceType.ActualDataType = TIDPointer(Dest).ReferenceType.ActualDataType then
              Exit(Source);
          end;

          { если оба - классы, то проверяем InheritsForm }
          if (SrcDTID = dtClass) and (DstDTID = dtClass) then
          begin
            if TIDClass(Source.DataType).IsInheritsForm(TIDClass(Dest)) then
              Exit(Source);
          end;

          { если классы и интерфейс }
          if (SrcDTID = dtClass) and (DstDTID = dtInterface) then
          begin
            if TIDClass(Source.DataType).FindInterface(TIDInterface(Dest)) then
              Exit(Source)
            else
              ERROR_CLASS_NOT_IMPLEMENT_INTF(Source, Dest);
          end;

          { есди приемник - class of }
          if DstDTID = dtClassOf then
            Decl := MatchImplicitClassOf(Source, TIDClassOf(Dest));

          {дин. массив как набор}
          if (DstDTID = dtSet) and (SDataType is TIDDynArray) then
          begin
            Result := MatchSetImplicit(Source, TIDSet(Dest));
            Exit;
          end else
          if (SrcDTID = dtProcType) and (DstDTID = dtProcType) then
            Decl := MatchProcedureTypes(TIDProcType(SDataType), TIDProcType(Dest))
          else
          if SDataType is TIDArray then
          begin
            if Dest is TIDArray then
              Result := MatchArrayImplicit(SContext, Source, TIDArray(Dest))
            else
            if DstDTID = dtRecord then
              Result := MatchArrayImplicitToRecord(Source, TIDStructure(Dest))
            else
              Result := nil;
            Exit;
          end;
          if (SrcDTID = dtRecord) and (DstDTID = dtRecord) then
          begin
            Result := MatchRecordImplicit(SContext, Source, TIDRecord(Dest));
            Exit;
          end;

          {если generic}
          if (SrcDTID = dtGeneric) or (DstDTID = dtGeneric) then
            Exit(Source); // нужна еще проверка на констрейты
        end;
      end;
    end;
  end;

  if Source.ClassType = TIDDrefExpression then
  begin
    Result := GetTMPVarExpr(SContext, Source.DataType, parser_Position);
    Exit;
  end;

  if Assigned(SDataType.SysImplicitToAny) then
  begin
    Decl := TIDInternalOpImplicit(SDataType.SysImplicitToAny).Check(Source, Dest);
    if Assigned(Decl) then
      Exit(Source);
    Decl := nil;
  end;

  if not Assigned(Decl) then
  begin
    if Assigned(Dest.SysExplicitFromAny) then
    begin
      Decl := TIDInternalOpImplicit(Dest.SysExplicitFromAny).Check(Source, Dest);
      if Assigned(Decl) then
        Exit(Source);
    end;
    Exit(nil);
  end;

  if Decl.ItemType = itType then
    Exit(Source);

  Result := CheckAndCallOperator(SContext, Decl, Source);
end;

function TASTDelphiUnit.MatchBinarOperator(const SContext: TSContext; Op: TOperatorID; var Left, Right: TIDExpression): TIDDeclaration;
var
  LeftDT, RightDT: TIDType;
  L2RImplicit, R2LImplicit: TIDDeclaration;
begin
  LeftDT := Left.DataType.ActualDataType;
  RightDT := Right.DataType.ActualDataType;
  // поиск оператора (у левого операнда)
  Result := LeftDT.BinarOperator(Op, RightDT);
  if Assigned(Result) then
    Exit;
  // поиск оператора (у правого операнда)
  Result := RightDT.BinarOperatorFor(Op, LeftDT);
  if Assigned(Result) then
    Exit;

  if (LeftDT.DataTypeID = dtGeneric) then
    Exit(LeftDT);

  if RightDT.DataTypeID = dtGeneric then
    Exit(RightDT);

  if (Op = opIn) and (Right.Declaration.ClassType = TIDRangeConstant) then
  begin
    Result := MatchOperatorIn(Left, Right);
    Exit;
  end;

  if (LeftDT.DataTypeID = dtClass) and
     (RightDT.DataTypeID = dtClass) then
  begin
    if TIDClass(LeftDT).IsInheritsForm(TIDClass(RightDT)) then
      Exit(SYSUnit._Boolean);
    if TIDClass(RightDT).IsInheritsForm(TIDClass(LeftDT)) then
      Exit(SYSUnit._Boolean);
  end;

  // если ненайдено напрямую - ищем через неявное привидение
  L2RImplicit := CheckImplicit(Left, RightDT);
  R2LImplicit := CheckImplicit(Right, LeftDT);
  if L2RImplicit is TIDType then
  begin
    Result := TIDType(L2RImplicit).BinarOperator(Op, RightDT);
    if Assigned(Result) then
    begin
      Left := MatchImplicit3(SContext, Left, RightDT);
      Exit;
    end;
  end;
  if R2LImplicit is TIDType then
  begin
    Result := TIDType(R2LImplicit).BinarOperator(Op, LeftDT);
    if Assigned(Result) then
    begin
      Right := MatchImplicit3(SContext, Right, LeftDT);
      Exit;
    end;
  end;
  if Assigned(L2RImplicit) then
  begin
    Result := RightDT.BinarOperator(Op, L2RImplicit.DataType);
    if Assigned(Result) then
    begin
      Left := MatchImplicit3(SContext, Left, RightDT);
      Exit;
    end;
  end;
  if Assigned(R2LImplicit) then
  begin
    Result := LeftDT.BinarOperator(Op, R2LImplicit.DataType);
    if Assigned(Result) then
    begin
      Right := MatchImplicit3(SContext, Right, LeftDT);
      Exit;
    end;
  end;
  Result := nil;
end;

function TASTDelphiUnit.MatchBinarOperatorWithImplicit(const SContext: TSContext; Op: TOperatorID; var Left,
                                                       Right: TIDexpression): TIDDeclaration;
  {function WriteImplicitCast(Implicit: TIDDeclaration; Src: TIDExpression): TIDExpression;
  var
    PCall: TIDCallExpression;
  begin
    if Implicit.ItemType = itType then
    begin
      Result := GetTMPVarExpr(SContext, TIDType(Implicit));
      ILWrite(SContext, TIL.IL_Move(Result, Src));
    end else
    if Implicit.ItemType = itProcedure then
    begin
      Result := GetTMPVarExpr(SContext, Implicit.DataType);
      PCall := TIDCallExpression.Create(Implicit);
      PCall.TextPosition := Src.TextPosition;
      PCall.ArgumentsCount := 2;
      PCall.Instance := GetOperatorInstance(Op, Result, Src);
      Result := Process_CALL_direct(SContext, PCall, TIDExpressions.Create(Result, Src));
    end else begin
      ERROR_FEATURE_NOT_SUPPORTED;
      Result := nil;
    end;
  end;}
var
  LeftDT, RightDT: TIDType;
  Operators: TIDPairList;
  LeftImplicit, RightImplicit, LeftBinarOp, RightBinarOp: TIDDeclaration;
  LeftImplicitFactor, RightImplicitFactor: Integer;
begin
  LeftImplicitFactor := 0;
  RightImplicitFactor := 0;
  LeftDT := Left.DataType.ActualDataType;
  RightDT := Right.DataType.ActualDataType;

  Operators := LeftDT.BinarOperators[Op];
  if Assigned(Operators) then
    LeftImplicit := FindImplicitFormBinarOperators(Operators, RightDT, LeftImplicitFactor, LeftBinarOp)
  else
    LeftImplicit := nil;

  Operators := RightDT.BinarOperators[Op];
  if Assigned(Operators) then
    RightImplicit := FindImplicitFormBinarOperators(Operators, LeftDT, RightImplicitFactor, RightBinarOp)
  else
    RightImplicit := nil;

  if not Assigned(LeftImplicit) and not Assigned(RightImplicit) then
    ERROR_NO_OVERLOAD_OPERATOR_FOR_TYPES(Op, Left, Right);

  if LeftImplicitFactor >= RightImplicitFactor then
  begin
    Result := LeftBinarOp;
    //Right := WriteImplicitCast(LeftImplicit, Right);
  end else begin
    Result := RightBinarOp;
    //Left := WriteImplicitCast(RightImplicit, Left);
  end;
end;

class function TASTDelphiUnit.MatchExplicit(const Source: TIDExpression; Destination: TIDType): TIDDeclaration;
var
  SrcDataType: TIDType;
  DstDataType: TIDType;
  ExplicitIntOp: TIDOperator;
begin
  SrcDataType := Source.DataType.ActualDataType;
  DstDataType := Destination.ActualDataType;
  if SrcDataType = DstDataType then
    Exit(Destination);

  Result := SrcDataType.GetExplicitOperatorTo(DstDataType);
  if not Assigned(Result) then
    Result := DstDataType.GetExplicitOperatorFrom(SrcDataType);

  if Assigned(Result) then
    Exit;

  ExplicitIntOp := DstDataType.SysExplicitFromAny;
  if ExplicitIntOp is TIDInternalOpImplicit then
  begin
    if TIDInternalOpImplicit(ExplicitIntOp).Check(Source, Destination) <> nil then
      Exit(Destination);
  end;
  Result := nil;
end;

class function TASTDelphiUnit.CheckImplicit(Source: TIDExpression; Dest: TIDType): TIDDeclaration;
var
  SDataType: TIDType;
  SrcDTID, DstDTID: TDataTypeID;
begin
  SDataType := Source.DataType.ActualDataType;
  Dest := Dest.ActualDataType;

  // ищем явно определенный implicit у источника
  Result := SDataType.GetImplicitOperatorTo(Dest);
  if Result is TIDInternalOpImplicit then
    Result := TIDInternalOpImplicit(Result).Check(Source, Dest);

  if Assigned(Result) then
    Exit;

  // ищем явно определенный implicit у приемника
  Result := Dest.GetImplicitOperatorFrom(SDataType);
  if Assigned(Result) then
    Exit;

  // если не нашли точных имплиситов, ищем подходящий (у источника)
  Result := SDataType.FindImplicitOperatorTo(Dest);
  if Assigned(Result) then
    Exit;

  // если не нашли точных имплиситов, ищем подходящий (у приемника)
  Result := Dest.FindImplicitOperatorFrom(SDataType);
  if Assigned(Result) then
    Exit;

  {if (DstDTID = dtSet) and (SDataType is TIDDynArray) then
  begin
    Result := CheckSetImplicit(Source, TIDSet(Dest));
    Exit;
  end;}

  // проверка на nullpointer
  if (Source.Declaration = SYSUnit._NullPtrConstant) and
     (Dest.IsReferenced or (Dest is TIDProcType)) then
    Exit(Dest);

  SrcDTID := Source.DataTypeID;
  DstDTID := Dest.DataTypeID;

  if (Source.IsDynArrayConst) and (Dest.DataTypeID = dtRecord) then
  begin
    if MatchArrayImplicitToRecord(Source, TIDStructure(Dest)) <> nil then
      Exit(Dest);
  end;

  {если оба - классы, то проверяем InheritsForm
  if (SrcDTID = dtClass) and (DstDTID = dtClass) then
  begin
    if TIDClass(Source.DataType).IsInheritsForm(TIDClass(Dest)) then
      Exit(Dest);
  end;}

  // есди приемник - class of
  if DstDTID = dtClassOf then
  begin
    Result := MatchImplicitClassOf(Source, TIDClassOf(Dest));
    if Assigned(Result) then
      Exit;
  end;

  if (SrcDTID = dtPointer) and (DstDTID = dtPointer) then
  begin
    if (TIDPointer(SDataType).ReferenceType = nil) and
       (TIDPointer(Dest).ReferenceType = nil) then
      Exit(Source.DataType);

    // it needs to check !!!
    if not Assigned(TIDPointer(SDataType).ReferenceType) or not Assigned(TIDPointer(Dest).ReferenceType) then
      Exit(Source.DataType);

    if TIDPointer(SDataType).ReferenceType.ActualDataType = TIDPointer(Dest).ReferenceType.ActualDataType then
      Exit(Source.DataType);
  end;

  if (SrcDTID = dtProcType) and (DstDTID = dtProcType) then
    Result := MatchProcedureTypes(TIDProcType(SDataType), TIDProcType(Dest))
  else
  if (Source.Declaration.ItemType = itConst) and (SrcDTID = dtDynArray) then
    Result := MatchConstDynArrayImplicit(Source, Dest)
  else begin
    Result := Dest.GetImplicitOperatorFrom(SDataType);
    if not Assigned(Result) then
      Result := MatchDynArrayImplicit(Source, Dest);
  end;
  if (DstDTID = dtGeneric) or (SrcDTID = dtGeneric) then
    Exit(Source.AsType); // нужна еще проверка на констрейты
end;

function TASTDelphiUnit.ParseArrayMember(Scope: TScope; var PMContext: TPMContext; Decl: TIDDeclaration; out DataType: TIDType;
                                         var EContext: TEContext; var SContext: TSContext; ASTE: TASTExpression): TTokenID;
var
  IdxCount: Integer; // кол-во индексов указанных при обращении к массиву
  Expr: TIDExpression;
  InnerEContext: TEContext;
  DimensionsCount: Integer;
  DeclType: TIDType;
begin
  DeclType := Decl.DataType;
  if DeclType.DataTypeID = dtPointer then
    DeclType := TIDPointer(DeclType).ReferenceType;

  if (Decl.ItemType <> itProperty) and (DeclType is TIDArray) then
  begin
    DimensionsCount := TIDArray(DeclType).DimensionsCount;
    DataType := TIDArray(DeclType).ElementDataType;
  end else
  if (Decl.ItemType = itProperty) and (TIDProperty(Decl).Params.Count > 0) then
  begin
    DimensionsCount := TIDProperty(Decl).ParamsCount;
    DataType := TIDProperty(Decl).DataType;
  end else
  if (DeclType is TIDStructure) and Assigned(TIDStructure(DeclType).DefaultProperty) then
  begin
    Decl := TIDStructure(DeclType).DefaultProperty;
    DimensionsCount := TIDProperty(Decl).ParamsCount;
    DataType := TIDProperty(Decl).DataType;
    Expr := TIDExpression.Create(Decl);
    if EContext.EPosition = ExprRValue then
    begin
      //Result := ParsePropertyMember(PMContext, Scope, TIDProperty(Decl), Expr, EContext);
      DataType := nil;
      Exit;
    end;
    PMContext.Add(Expr);
  end else
  begin
    ERROR_ARRAY_TYPE_REQUIRED(PMContext.ID);
    DimensionsCount := 0;
  end;

  var Op: TASTOpArrayAccess := ASTE.AddOperation<TASTOpArrayAccess>;

  IdxCount := 0;
  InitEContext(InnerEContext, SContext, ExprNested);
  while True do begin
    parser_NextToken(Scope);
    var ASTExpr: TASTExpression := nil;
    Result := ParseExpression(Scope, SContext, InnerEContext, ASTExpr);
    Op.AddIndex(ASTExpr);
    Expr := InnerEContext.Result;
    CheckEmptyExpression(Expr);
    {if Assigned(InnerEContext.LastBoolNode) then
      Bool_CompleteImmediateExpression(InnerEContext, Expr);}

    if Expr.Declaration.ItemType = itConst then
      // статическая проверка на границы массива
      StaticCheckBounds(Expr.AsConst, Decl, IdxCount)
    else begin
       // динамическая проверка на границы массива
       {if UseCheckBound then
         EmitDynCheckBound(EContext.SContext, Decl, Expr);}
    end;

    PMContext.Add(Expr);
    Inc(IdxCount);
    if Result = token_coma then
    begin
      InnerEContext.Reset;
      Continue;
    end;
    parser_MatchToken(Result, token_closeblock);
    Result := parser_NextToken(Scope);
    Break;
  end;
  if IdxCount <> DimensionsCount then
    ERROR_NEED_SPECIFY_NINDEXES(Decl);
end;

function TASTDelphiUnit.ParseASMStatement(Scope: TScope; var SContext: TSContext): TTokenID;
var
  KW: TASTKWAsm;
begin
  KW := SContext.Add<TASTKWAsm>;
  while (Result <> token_end) do
  begin
    // skip all to end
    if KW.Next <> nil then;
    Result := parser_NextToken(Scope);
  end;
  Result := parser_NextToken(Scope);
end;

function TASTDelphiUnit.ParseProperty(Struct: TIDStructure): TTokenID;
var
  ID: TIdentifier;
  Prop: TIDProperty;
  Scope: TScope;
  PropDataType: TIDType;
  Proc: TIDProcedure;
  Expr: TIDExpression;
  EContext: TEContext;
  DataType: TIDType;
  PropParams: TScope;
  VarSpace: TVarSpace;
  SContext: TSContext;
  ASTE: TASTExpression;
begin
  Scope := Struct.Members;
  parser_ReadNextIdentifier(Scope, ID);
  Prop := TIDProperty.Create(Scope, ID);
  Scope.AddProperty(Prop);

  Result := parser_NextToken(Scope);
  if Result = token_openblock then begin
    VarSpace.Initialize;
    PropParams := TProcScope.CreateInDecl(Scope, @VarSpace, nil);
    Prop.Params := PropParams;
    Result := ParseParameters(PropParams);
    parser_MatchToken(Result, token_closeblock);
    Result := parser_NextToken(Scope);
  end else
    PropParams := nil;

  // парсим тип свойства
  parser_MatchToken(Result, token_colon);
  Result := ParseTypeSpec(Scope, PropDataType);
  Prop.DataType := PropDataType;

  SContext := TSContext.Create(Self);

  // геттер
  if Result = token_read then
  begin
    InitEContext(EContext, SContext, ExprRValue);
    parser_NextToken(Scope);
    Result := ParseExpression(Scope, SContext, EContext, ASTE);
    Expr := EContext.Result;
    if Expr.ItemType = itProcedure then
    begin
      Proc := Expr.AsProcedure;
      DataType := Proc.ResultType;
      if not Assigned(DataType) then
        AbortWork(sFieldConstOrFuncRequiredForGetter, Expr.TextPosition);

      if Assigned(PropParams) then
        MatchPropGetter(Prop, Proc, PropParams);

    end else
      DataType := Expr.DataType;

    CheckImplicitTypes(DataType, PropDataType, Expr.TextPosition);
    Prop.Getter := Expr.Declaration;
  end;

  // сеттер
  if Result = token_write then
  begin
    InitEContext(EContext, SContext, ExprRValue);
    parser_NextToken(Scope);
    Result := ParseExpression(Scope, SContext, EContext, ASTE);
    Expr := EContext.Result;
    case Expr.ItemType of
      itConst: AbortWork(sFieldOrProcRequiredForSetter, Expr.TextPosition);
      itProcedure: MatchPropSetter(Prop, Expr, PropParams);
    end;
    Prop.Setter := Expr.Declaration;
  end;

  parser_MatchToken(Result, token_semicolon);
  Result := parser_NextToken(Scope);

  // default - спецификатор
  if Result = token_default then begin
    if Prop.ParamsCount = 0 then
      ERROR_DEFAULT_PROP_MUST_BE_ARRAY_PROP;
    if not Assigned(Struct.DefaultProperty) then
      Struct.DefaultProperty := Prop
    else
      ERROR_DEFAULT_PROP_ALREADY_EXIST(Struct.DefaultProperty);
    parser_ReadSemicolon(Scope);
    Result := parser_NextToken(Scope);
  end;
end;

function TASTDelphiUnit.ParseBreakStatement(Scope: TScope; var SContext: TSContext): TTokenID;
begin
  if not SContext.IsLoopBody then
    if not SContext.IsLoopBody then
    AbortWork(sBreakOrContinueAreAllowedOnlyInALoops, parser_Position);

  SContext.Add<TASTKWBreak>;
  Result := parser_NextToken(Scope);
end;

function TASTDelphiUnit.ParseCondInclude(Scope: TScope): TTokenID;
var
  ID: TIdentifier;
  FileName: string;
  Pos: TParserPosition;
  Stream: TStringStream;
  SC: TSContext;
begin
  while True do begin
    Result := TTokenID(Parser.NextToken);
    if Result = token_closefigure then
      break;

    if Result = token_identifier then
      FileName := FileName + Parser.OriginalToken
    else
      FileName := FileName + '.'; // tmp
  end;
  Stream := TStringStream.Create;
  try
    Stream.LoadFromFile(ExtractFilePath(Self.FileName) + FileName);
    var Messages := CompileSource(usImplementation, Stream.DataString);
    if Messages.HasErrors then
      AbortWork('The included file: ' + FileName + ' has errors', parser_Position);
  finally
    Stream.Free;
  end;
end;

function TASTDelphiUnit.ParseConstExpression(Scope: TScope; out Expr: TIDExpression; EPosition: TExpessionPosition): TTokenID;
var
  EContext: TEContext;
  SContext: TSContext;
  ASTE: TASTExpression;
begin
  SContext := TSContext.Create(Self);
  InitEContext(EContext, SContext, EPosition);
  Result := ParseExpression(Scope, SContext, EContext, ASTE);
  CheckEndOfFile(Result);
  Expr := EContext.Result;
  if not Assigned(Expr) then
    Exit;
  if Expr.IsAnonymous then
    Expr.TextPosition := parser_PrevPosition;
  if (Expr.ItemType <> itType) and (Expr.DataTypeID <> dtGeneric) and not (Expr.Declaration is TIDMacroArgument) then
    CheckConstExpression(Expr);
end;

function TASTDelphiUnit.ParseContinueStatement(Scope: TScope; var SContext: TSContext): TTokenID;
begin
  if not SContext.IsLoopBody then
    AbortWork(sBreakOrContinueAreAllowedOnlyInALoops, parser_Position);

  SContext.Add<TASTKWContinue>;
  Result := parser_NextToken(Scope);
end;


function TASTDelphiUnit.ParseCaseStatement(Scope: TScope; var SContext: TSContext): TTokenID;
type
  TMatchItem = record
    Expression: TIDExpression;
  end;
  PMatchItem = ^TMatchItem;
var
  MatchItems: array of TMatchItem;
  MatchItem: PMatchItem;
  procedure CheckUniqueMIExpression(Cur: TIDExpression);
  var
    i: Integer;
    Prev: TIDExpression;
    IsDuplicate: Boolean;
  begin
    IsDuplicate := False;
    for i := 0 to Length(MatchItems) - 2 do
    begin
      Prev := MatchItems[i].Expression;
      if Prev.IsConstant and Cur.IsConstant then
      begin
        if Prev.DataTypeID = dtRange then
        begin
          if Cur.DataTypeID <> dtRange then
            IsDuplicate := IsConstValueInRange(Cur, TIDRangeConstant(Prev.Declaration))
          else
            IsDuplicate := IsConstRangesIntersect(TIDRangeConstant(Cur.Declaration), TIDRangeConstant(Prev.Declaration))
        end else
        if Cur.DataTypeID = dtRange then
        begin
          IsDuplicate := IsConstValueInRange(Prev, TIDRangeConstant(Cur.Declaration));
        end else
          IsDuplicate := IsConstEqual(Prev, Cur);
      end else
      if (not Prev.IsAnonymous) and (not Cur.IsAnonymous) then
      begin
        IsDuplicate := (Prev.Declaration = Cur.Declaration);
      end;
      if IsDuplicate then
        AbortWork(sDuplicateMatchExpression, Cur.TextPosition);
    end;
  end;
var
  EContext: TEContext;
  SExpression,
  DExpression: TIDExpression;
  TotalMICount, ItemsCount: Integer;
  ElsePresent: Boolean;
  SEConst: Boolean;
  MISContext: TSContext;
  NeedWriteIL,
  NeedCMPCode: Boolean;
  Implicit: TIDDeclaration;
  ASTExpr: TASTExpression;
  KW: TASTKWCase;
  CaseItem: TASTExpBlockItem;
begin
  KW := SContext.Add<TASTKWCase>;

  // NeedCMPCode := False;
  // CASE выражение
  InitEContext(EContext, SContext, ExprRValue);
  parser_NextToken(Scope);
  Result := ParseExpression(Scope, SContext, EContext, ASTExpr);
  KW.Expression := ASTExpr;
  SExpression := EContext.RPNPopExpression();
  CheckEmptyExpression(SExpression);
  {if Assigned(EContext.LastBoolNode) then
    Bool_CompleteImmediateExpression(EContext, SExpression);}
  SEConst := SExpression.IsConstant;
  parser_MatchToken(Result, token_of);
  ItemsCount := 0;
  TotalMICount := 0;
  NeedWriteIL := True;
  ElsePresent := False;
  Result := parser_NextToken(Scope);

  while Result <> token_end do
  begin
    InitEContext(EContext, SContext, ExprRValue);
    if Result <> token_else then
    begin
      while True do begin
        //CFBBegin(SContext, CFB_CASE_ENTRY);
        Result := ParseExpression(Scope, SContext, EContext, ASTExpr);

        CaseItem := KW.AddItem(ASTExpr);
        MISContext := SContext.MakeChild(CaseItem.Body);

        DExpression := EContext.RPNPopExpression();
        CheckEmptyExpression(DExpression);
        // проверка на совпадение типа
        if DExpression.DataTypeID = dtRange then
          Implicit := CheckImplicit(SExpression, TIDRangeType(DExpression.DataType).ElementType)
        else
          Implicit := CheckImplicit(SExpression, DExpression.DataType);
        if not Assigned(Implicit) then
          AbortWork(sMatchExprTypeMustBeIdenticalToCaseExprFmt, [DExpression.DataTypeName, SExpression.DataTypeName], DExpression.TextPosition);

        // проверяем на константу
        if DExpression.IsConstant and SEConst then
        begin
          // если данное выражение истенно, то для остальных генерировать IL код не нужно
          if ((DExpression.DataTypeID = dtRange) and IsConstValueInRange(SExpression, TIDRangeConstant(DExpression.Declaration)))
              or IsConstEqual(SExpression, DExpression) then
          begin
            NeedWriteIL := False;
          end else
            NeedWriteIL := True;
          NeedCMPCode := False;
        end else begin
          //MISContext.WriteIL := NeedWriteIL;
          NeedCMPCode := NeedWriteIL;
          if NeedCMPCode then
          begin
            SetLength(MatchItems, ItemsCount + 1);
            MatchItem := @MatchItems[ItemsCount];
            MatchItem.Expression := DExpression;

            // код проверки условия
            {if DExpression.DataTypeID <> dtRange then
            begin

            end else
              Process_operator_In(EContext, SExpression, DExpression);}
            Inc(ItemsCount);
          end;
        end;
        CheckUniqueMIExpression(DExpression);

        {if Assigned(EContext.LastBoolNode) and Assigned(EContext.LastBoolNode.PrevNode) then
          Bool_AddExprNode(EContext, ntOr);}

        // если была запятая, парсим следующее выражение
        if Result <> token_coma then
          break;

        parser_NextToken(Scope);
      end;
      // двоеточие
      parser_MatchToken(Result, token_colon);
      parser_NextToken(Scope);
      // корректируем переходы
      //Bool_CompleteExpression(EContext.LastBoolNode, JMPToEnd);
      // парсим код секции
      Result := ParseStatements(Scope, MISContext, False);

      parser_MatchToken(Result, token_semicolon);
      Result := parser_NextToken(Scope);
      Inc(TotalMICount);

    end else begin
      // ELSE секция
      MISContext := SContext.MakeChild(KW.ElseBody);
      parser_NextToken(Scope);
      Result := ParseStatements(Scope, MISContext, True);
      parser_MatchToken(Result, token_end);
      Result := parser_NextToken(Scope);
      ElsePresent := True;
      Inc(TotalMICount);
      Break;
    end;
  end;
  // проверяем есть ли хоть одна секция(включая ELSE) в кейсе
  if TotalMICount = 0 then
    AbortWork(sCaseStmtRequireAtLeastOneMatchExpr, parser_PrevPosition);

  // если небыло ELSE секции, парсим END;
  if not ElsePresent then begin
    parser_MatchToken(Result, token_end);
    Result := parser_NextToken(Scope);
  end;
end;

function TASTDelphiUnit.ParseExitStatement(Scope: TScope; var SContext: TSContext): TTokenID;
var
  ExitExpr: TASTExpression;
  EContext: TEContext;
  KW: TASTKWExit;
begin
  KW := SContext.Add<TASTKWExit>;

  Result := parser_NextToken(Scope);
  if Result = token_openround then
  begin
    if not Assigned(SContext.Proc.ResultType) then
      AbortWork(sReturnValueNotAllowedForProc, parser_Position);

    parser_NextToken(Scope);
    InitEContext(EContext, SContext, ExprNested);
    Result := ParseExpression(Scope, SContext, EContext, ExitExpr);
    KW.Expression := ExitExpr;
    parser_MatchToken(Result, token_closeround);
    Result := parser_NextToken(Scope);
  end;

end;

procedure TASTDelphiUnit.CheckLeftOperand(const Status: TRPNStatus);
begin
  if Status <> rpOperand then
    ERROR_EXPRESSION_EXPECTED;
end;

function TASTDelphiUnit.ParseExplicitCast(Scope: TScope; var SContext: TSContext; var DstExpression: TIDExpression): TTokenID;
var
  EContext: TEContext;
  SrcExpr: TIDExpression;
  OperatorDecl: TIDDeclaration;
  CallExpr: TIDCallExpression;
  TargetType: TIDType;
  ASTE: TASTExpression;
begin
  InitEContext(EContext, SContext, ExprNested);
  parser_NextToken(Scope);
  Result := ParseExpression(Scope, SContext, EContext, ASTE);
  SrcExpr := EContext.RPNPopExpression();

//  if Assigned(EContext.LastBoolNode) then
//    Bool_CompleteExpression(EContext.LastBoolNode, SContext.ILLast);
  if Result <> token_closeround then
    ERROR_INCOMPLETE_STATEMENT;

  TargetType := DstExpression.AsType;

  OperatorDecl := MatchExplicit(SrcExpr, TargetType);
  if not Assigned(OperatorDecl) then
  begin
    if (SrcExpr.DataType.DataTypeID = dtClass) and
       (TargetType.DataTypeID = dtClass) and
       (TIDClass(SrcExpr.DataType).IsInheritsForm(TIDClass(TargetType))) then
      OperatorDecl := SYSUnit._Boolean
    else begin
      if (SrcExpr.DataTypeID = dtChar) and (TargetType.DataTypeID = dtAnsiChar) then
      begin
        if SrcExpr.IsConstant then
        begin
          if (Ord(SrcExpr.AsCharConst.Value) <= 255) then
            OperatorDecl := TargetType
          else
            ERROR_CONST_VALUE_OVERFLOW(SrcExpr, TargetType);
        end else
          OperatorDecl := TargetType;
      end else
      if (SrcExpr.DataTypeID = dtPointer) and (DstExpression.AsType.DataTypeID = dtPointer) then
         OperatorDecl := TargetType
      else
      if (DstExpression.AsType.DataTypeID = dtAnsiString) and
         (SrcExpr.DataType is TIDOrdinal) then
      begin
        // todo: make new ANsiType with SrcExpr codepage
        Exit(Parser.NextToken);
      end else
        ERROR_INVALID_TYPECAST(SrcExpr, DstExpression.AsType);
    end;
  end;

  Result := parser_NextToken(Scope);

  if OperatorDecl.ItemType = itType then
  begin
    if (SrcExpr.ItemType = itConst) and (SrcExpr.IsAnonymous) then
    begin
      TIDConstant(SrcExpr.Declaration).ExplicitDataType := OperatorDecl as TIDType;
      DstExpression := TIDExpression.Create(SrcExpr.Declaration);
    end else
      DstExpression := TIDCastExpression.Create(SrcExpr.Declaration, TIDType(DstExpression.Declaration), DstExpression.TextPosition);
    DstExpression.Instruction := SrcExpr.Instruction;
  end else begin
    // вызываем explicit-оператор
    CallExpr := TIDCallExpression.Create(OperatorDecl, DstExpression.TextPosition);
    CallExpr.ArgumentsCount := 1;
    DstExpression := Process_CALL_direct(SContext, CallExpr, TIDExpressions.Create(SrcExpr));
  end;
end;

function TASTDelphiUnit.ParseExpression(Scope: TScope; var SContext: TSContext; var EContext: TEContext;
                                        out ASTE: TASTExpression): TTokenID;
var
  ID: TIdentifier;
  Status: TRPNStatus;
  Expr: TIDExpression;
  RoundCount: Integer;
  RecordInitResultExpr: TIDExpression;
begin
  Status := rprOk;
  RoundCount := 0;
  RecordInitResultExpr := nil;
  Result := parser_CurTokenID;
  ASTE := TASTExpression.Create(nil);
  while True do begin
    case Result of
      token_eof: Break;// ERROR_END_OF_FILE;
      token_openround: begin
        Inc(RoundCount);
        EContext.RPNPushOpenRaund;
        ASTE.AddSubItem(TASTOpOpenRound);
        Status := rprOk;
      end;
      token_closeround: begin
        Dec(RoundCount);
        if RoundCount < 0 then
        begin
          if EContext.EPosition <> ExprLValue then
            Break;

          ERROR_UNNECESSARY_CLOSED_ROUND;
        end;
        ASTE.AddSubItem(TASTOpCloseRound);
        EContext.RPNPushCloseRaund();
        Status := rpOperand;
      end;
      token_openblock: begin
        ParseVector(Scope, EContext);
        Status := rpOperand;
      end;
      token_closeblock: begin
        if EContext.EPosition = ExprNested then
          Break
        else
          ERROR_UNNECESSARY_CLOSED_BLOCK;
      end;
      token_plus: begin
        if Status = rpOperand then
          Status := EContext.RPNPushOperator(opAdd)
        else
          Status := EContext.RPNPushOperator(opPositive);
        ASTE.AddSubItem(TASTOpPlus);
      end;
      token_minus: begin
        if Status = rpOperand then
          Status := EContext.RPNPushOperator(opSubtract)
        else
          Status := EContext.RPNPushOperator(opNegative);
        ASTE.AddSubItem(TASTOpMinus);
      end;
      token_equal: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opEqual);
        ASTE.AddSubItem(TASTOpEqual);
      end;
      token_var: begin
        //Result := ParseInplaceVarDecl(Scope, Expr);
        //SContext.IL.AddVariable(Expr.AsVariable);
        //EContext.RPNPushExpression(Expr);
        Status := rpOperand;
        continue;
      end;
      token_notequal: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opNotEqual);
        ASTE.AddSubItem(TASTOpNotEqual);
      end;
      token_less: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opLess);
        ASTE.AddSubItem(TASTOpLess);
      end;
      token_lessorequal: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opLessOrEqual);
        ASTE.AddSubItem(TASTOpLessEqual);
      end;
      token_above: begin
        if EContext.EPosition = ExprNestedGeneric then
          Break;
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opGreater);
        ASTE.AddSubItem(TASTOpGrater);
      end;
      token_aboveorequal: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opGreaterOrEqual);
        ASTE.AddSubItem(TASTOpGraterEqual);
      end;
      token_asterisk: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opMultiply);
        ASTE.AddSubItem(TASTOpMul);
      end;
      token_in: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opIn);
      end;
      token_slash: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opDivide);
        ASTE.AddSubItem(TASTOpDiv);
      end;
      token_div: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opIntDiv);
        ASTE.AddSubItem(TASTOpIntDiv);
      end;
      token_mod: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opModDiv);
        ASTE.AddSubItem(TASTOpMod);
      end;
      token_period: begin
        CheckLeftOperand(Status);
        //Bool_AddExprNode(EContext, GetILLast(SContext), cGreaterOrEqual); // ???????????????? зачем?
        Status := EContext.RPNPushOperator(opPeriod);
      end;
      token_address: begin
        Status := EContext.RPNPushOperator(opAddr);
      end;
      token_caret: begin
        EContext.RPNPushOperator(opDereference);
        Status := rpOperand;
      end;
      token_and: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opAnd);
      end;
      token_or: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opOr);
      end;
      token_xor: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opXor);
      end;
      token_not: begin
        Status := EContext.RPNPushOperator(opNot);
      end;
      token_shl: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opShiftLeft);
        ASTE.AddSubItem(TASTOpShl);
      end;
      token_shr: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opShiftRight);
        ASTE.AddSubItem(TASTOpShr);
      end;
      token_is: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opIs);
        ASTE.AddSubItem(TASTOpCastCheck);
      end;
      token_as: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opAs);
        ASTE.AddSubItem(TASTOpDynCast);
      end;
      token_procedure, token_function: begin
        if Status = rpOperand then
          break;
        //Result := ParseAnonymousProc(Scope, EContext, SContext, Result);
        Status := rpOperand;
        continue;
      end;
      token_inherited: begin
        //CheckLeftOperand(Status);
        //Result := ParseInheritedStatement(Scope, EContext);
        Status := rpOperand;
        continue;
      end;
      token_identifier: begin
        // есил встретился подряд воторой идентификатор, то выходим
        if Status = rpOperand then
          Break;
        if parser_IdentifireType = itIdentifier then
        begin
          Expr := nil;
          Result := ParseMember(Scope, Expr, EContext, SContext, ASTE);
          // если результат = nil значит это был вызов функции и все
          // необходимые параметры погружены в стек, поэтому идем дальше
          if not Assigned(Expr) then
          begin
            Status := rpOperand;
            continue;
          end;

          // the record default value init
          if (Result = token_colon) and (Scope is TRecordInitScope) then
          begin
            if not Assigned(RecordInitResultExpr) then
            begin
              var Decl := TIDRecordConstant.CreateAnonymous(Scope, TRecordInitScope(Scope).Struct, nil);
              //Decl.DataType := TRecordInitScope(Scope).Struct;
              RecordInitResultExpr := TIDExpression.Create(Decl, Expr.TextPosition);
              EContext.RPNPushExpression(RecordInitResultExpr);
            end;
            Result := ParseRecordInitValue(TRecordInitScope(Scope), Expr);
            Continue;
          end;

          //if Expr.ExpressionType = etDeclaration then
          //  ASTE.AddDeclItem(Expr.Declaration, Expr.TextPosition);

          case Expr.ItemType of
            {именованная константа}
            itConst: status := rpOperand;
            {переменная}
            itVar: Status := rpOperand;
            {тип}
            itType: begin
              case Result of
                {явное преобразование типов}
                token_openround: begin
                  Result := ParseExplicitCast(Scope, SContext, Expr);
                  Status := rpOperand;
                end;
                {доступ к члену типа}
                token_dot: begin
                  ERROR_FEATURE_NOT_SUPPORTED;
                  Expr := nil;
                end;
                else begin
                  Status := rpOperand;
                end;
              end;
            end;
          end;
        end else begin
          {анонимная константа}
          parser_ReadCurrIdentifier(ID);
          Expr := CreateAnonymousConstant(Scope, EContext, ID, parser_IdentifireType);
          Result := parser_NextToken(Scope);
          Status := rpOperand;
          ASTE.AddDeclItem(Expr.Declaration, Expr.TextPosition);
        end;
        EContext.RPNPushExpression(Expr);
        Continue;
      end;
    else
      Break;
    end;
    Result := parser_NextToken(Scope);
  end;

  if (EContext.EPosition <> ExprNested) and (Status <> rpOperand) and NeedRValue(EContext.RPNLastOp) then
    ERROR_EXPRESSION_EXPECTED;

  EContext.RPNFinish();
end;


constructor TASTDelphiUnit.Create(const Project: IASTProject; const FileName: string; const Source: string);
var
  Scope: TScope;
begin
  inherited Create(Project, FileName, Source);

  Scope := TProcScope.CreateInBody(ImplScope);
  FInitProc := TASTDelphiProc.CreateAsSystem(Scope, '$initialization');
  FInitProc.EntryScope := Scope;
  TASTDelphiProc(FInitProc).fBody := TASTBlock.Create(FInitProc);

  Scope := TProcScope.CreateInBody(ImplScope);
  FFinalProc := TASTDelphiProc.CreateAsSystem(Scope, '$finalization');
  FFinalProc.EntryScope := Scope;
  TASTDelphiProc(FFinalProc).fBody := TASTBlock.Create(FFinalProc);
end;

function TASTDelphiUnit.CreateAnonymousConstant(Scope: TScope; var EContext: TEContext; const ID: TIdentifier;
                                         IdentifierType: TIdentifierType): TIDExpression;
var
  i: Integer;
  IntValue: Int64;
  Int32Value: Int32;
  FltValue: Double;
  DataType: TIDType;
  Value: string;
  CItem: TIDConstant;
  Chars: TStringDynArray;
begin
  Value := ID.Name;
  case IdentifierType of
    itChar: CItem := TIDCharConstant.CreateAnonymous(Scope, SYSUnit._Char, Value[1]);
    itString: begin
        // если чарсет метаданных равен ASCII, все строковые константы
        // удовлетворающе набору ASCII, создаются по умолчанию с типом AnsiString
        if (Package.RTTICharset = RTTICharsetASCII) and IsAnsiString(Value) then
          DataType := SYSUnit._AnsiString
        else
          DataType := SYSUnit._String;
      CItem := TIDStringConstant.CreateAnonymous(Scope, DataType, Value);
      CItem.Index := Package.GetStringConstant(TIDStringConstant(CItem));
    end;
    itInteger: begin
      if Value[1] = '#' then begin
        Value := Copy(Value, 2, Length(Value) - 1);
        if TryStrToInt(Value, Int32Value) then
          CItem := TIDCharConstant.CreateAnonymous(Scope, SYSUnit._Char, Char(Int32Value))
        else
          AbortWorkInternal('int convert error', parser_Position);
      end else begin
        if not TryStrToInt64(Value, IntValue) then
        begin
          if (EContext.RPNLastOperator = TOperatorID.opNegative) then
          begin
            // хак для обработки MinInt64 !!!
            if TryStrToInt64('-' + Value, IntValue) then
              EContext.RPNEraiseTopOperator
            else
              AbortWork('Invalid decimal value: %s', [Value], parser_Position);
          end else
            AbortWork('Invalid decimal value: %s', [Value], parser_Position);
        end;
        DataType := SYSUnit.DataTypes[GetValueDataType(IntValue)];
        CItem := TIDIntConstant.CreateAnonymous(Scope, DataType, IntValue);
      end;
    end;
    itFloat: begin
      FltValue := StrToFloat(Value);
      DataType := SYSUnit.DataTypes[GetValueDataType(FltValue)];
      CItem := TIDFloatConstant.CreateAnonymous(Scope, DataType, FltValue);
    end;
    itHextNumber: begin
      try
        IntValue := HexToInt64(Value);
      except
        ERROR_INVALID_HEX_CONSTANT;
        IntValue := 0;
      end;
      DataType := SYSUnit.DataTypes[GetValueDataType(IntValue)];
      CItem := TIDIntConstant.CreateAnonymous(Scope, DataType, IntValue);
    end;
    itBinNumber: begin
      try
        IntValue := BinStringToInt64(Value);
      except
        ERROR_INVALID_BIN_CONSTANT;
        IntValue := 0;
      end;
      DataType := SYSUnit.DataTypes[GetValueDataType(IntValue)];
      CItem := TIDIntConstant.CreateAnonymous(Scope, DataType, IntValue);
    end;
    itCharCodes: begin
      Chars := SplitString(ID.Name, '#');
      if Chars[0] = '' then
        Delete(Chars, 0, 1);
      // this is a string
      if Length(Chars) > 1 then
      begin
        SetLength(Value, Length(Chars));
        for i := 0 to Length(Chars) - 1 do
          Value[Low(string) + i] := Char(StrToInt(Chars[i]));

        // если чарсет метаданных равен ASCII, все строковые константы
        // удовлетворающе набору ASCII, создаются по умолчанию с типом AnsiString
        if (Package.RTTICharset = RTTICharsetASCII) and IsAnsiString(Value) then
          DataType := SYSUnit._AnsiString
        else
          DataType := SYSUnit._String;

        CItem := TIDStringConstant.CreateAnonymous(Scope, DataType, Value);
        CItem.Index := Package.GetStringConstant(TIDStringConstant(CItem));
      end else
      // this is a char
        CItem := TIDCharConstant.CreateAnonymous(Scope, SYSUnit._Char, Char(StrToInt(Chars[0])));
    end;
  else
    ERROR_INTERNAL();
    CItem := nil;
  end;
  Result := TIDExpression.Create(CItem, ID.TextPosition);
end;

function TASTDelphiUnit.ParseForInStatement(Scope: TScope; var SContext: TSContext; LoopVar: TIDExpression): TTokenID;
var
  EContext: TEContext;
  AExpr: TIDExpression;
  LoopArrayDT: TIDType;
  ASTExpr: TASTExpression;
  KW: TASTKWForIn;
  BodySContext: TSContext;
begin
  ASTExpr := TASTExpression.Create(nil);
  ASTExpr.AddDeclItem(LoopVar.Declaration, LoopVar.TextPosition);
  KW := SContext.Add<TASTKWForIn>;
  KW.VarExpr := ASTExpr;
  // парсим выражение-коллекцию
  InitEContext(EContext, SContext, ExprRValue);
  parser_NextToken(Scope);
  Result := ParseExpression(Scope, SContext, EContext, ASTExpr);
  KW.ListExpr := ASTExpr;
  AExpr := EContext.Result;
  CheckArrayExpression(AExpr);
  LoopArrayDT := (AExpr.DataType as TIDArray).ElementDataType;

  if Assigned(LoopVar.DataType) then
  begin
    // если переменная цикла определена зарание
    if MatchImplicit(LoopArrayDT, LoopVar.DataType) = nil then
      ERROR_INCOMPATIBLE_TYPES(LoopVar, LoopArrayDT);
  end else begin
    LoopVar.Declaration.DataType := LoopArrayDT;
  end;

  parser_MatchToken(Result, token_do);
  parser_NextToken(Scope);

  BodySContext := SContext.MakeChild(KW.Body);
  Result := ParseStatements(Scope, BodySContext, False);
end;

type
  TILCondition = (cNone, cEqual, cNotEqual, cGreater, cGreaterOrEqual, cLess, cLessOrEqual, cZero, cNonZero);

function TASTDelphiUnit.ParseForStatement(Scope: TScope; var SContext: TSContext): TTokenID;
var
  EContext: TEContext;
  BodySContext: TSContext;
  ID: TIdentifier;
  LoopVar: TIDDeclaration;
  LExpr, StartExpr, StopExpr: TIDExpression;
  NewScope: TScope;
  KW: TASTKWFor;
  JMPCondition: TILCondition;
  ASTExpr: TASTExpression;
  WriteIL: Boolean;
begin
  // цикловая переменная
  Result := parser_NextToken(Scope);
  if Result = token_var then begin
    parser_ReadNextIdentifier(Scope, ID);
    NewScope := TScope.Create(stLocal, Scope);
    LoopVar := TIDVariable.Create(NewScope, ID);
    NewScope.AddVariable(TIDVariable(LoopVar));
    Scope := NewScope;
  end else begin
    parser_ReadCurrIdentifier(ID);
    LoopVar := FindID(Scope, ID);
  end;

  InitEContext(EContext, SContext, ExprRValue);
  // заталкиваем в стек левое выражение
  LExpr := TIDExpression.Create(LoopVar, ID.TextPosition);
  EContext.RPNPushExpression(LExpr);

  Result := parser_NextToken(Scope);

  {если это цикл for ... in ...}
  if Result = token_in then
  begin
    Result := ParseForInStatement(Scope, SContext, LExpr);
    Exit;
  end;

  KW := SContext.Add<TASTKWFor>;
  BodySContext := SContext.MakeChild(KW.Body);

  parser_MatchToken(Result, token_assign);

  if LoopVar.DataType = nil then
    LoopVar.DataType := SYSUnit._Int32
  else
  if (LoopVar.ItemType <> itVar) or not (LoopVar.DataType.Ordinal) then
    AbortWork(sForLoopIndexVarsMastBeSimpleIntVar, parser_Position);

  // начальное значение
  parser_NextToken(Scope);
  Result := ParseExpression(Scope, SContext, EContext, ASTExpr);
  KW.ExprInit := ASTExpr;
  StartExpr := EContext.Result;
  CheckEmptyExpression(StartExpr);

  // пишем инструкцию присваениея начального значения
  EContext.RPNPushOperator(opAssignment);
  EContext.RPNFinish();

  // устанавливаем флаг цикловой переменной
  with TIDVariable(LoopVar) do Flags := Flags + [VarLoopIndex];

   // to/downto keyword
  case Result of
    token_to: JMPCondition := cGreater;
    token_downto: JMPCondition := cLess;
    else begin
      AbortWork(sKeywordToOrDowntoExpected, parser_PrevPosition);
      JMPCondition := cNone;
    end;
  end;

  // конечное значение
  InitEContext(EContext, SContext, ExprRValue);
  parser_NextToken(Scope);
  Result := ParseExpression(Scope, SContext, EContext, ASTExpr);
  KW.ExprTo := ASTExpr;
  StopExpr := EContext.Result;
  CheckEmptyExpression(StopExpr);

  // если для вычисления конечного выражения использовалась временная переменная
  // помечаем ее как постоянно используемую в блоке FOR цикла
  if StopExpr.IsTMPVar then
    StopExpr.AsVariable.IncludeFlags([VarLoopIndex]);

  // проверка на константы
  if (StartExpr.ItemType = itConst) and
     (StopExpr.ItemType = itConst) then
  begin
    WriteIL := ((JMPCondition = cGreater) and (StartExpr.AsIntConst.Value <= StopExpr.AsIntConst.Value)) or
               ((JMPCondition = cLess) and (StartExpr.AsIntConst.Value >= StopExpr.AsIntConst.Value));
    if not WriteIL then
      Warning(msgForOrWhileLoopExecutesZeroTimes, [], LoopVar.SourcePosition);
  end;

  // тело цикла
  parser_MatchToken(Result, token_do);
  Result := parser_NextToken(Scope);
  if Result <> token_semicolon then
    Result := ParseStatements(Scope, BodySContext, False);

  // сбрасываем флаг цикловой переменной
  with TIDVariable(LoopVar) do Flags := Flags - [VarLoopIndex];
end;

function TASTDelphiUnit.ParseGenericsArgs(Scope: TScope; var SContext: TSContext; out Args: TIDExpressions): TTokenID;
var
  EContext: TEContext;
  Expr: TIDExpression;
  ArgsCount: Integer;
  ASTE: TASTExpression;
begin
  ArgsCount := 0;
  while true do begin
    InitEContext(EContext, SContext, ExprNestedGeneric);
    parser_NextToken(Scope);
    Result := ParseExpression(Scope, SContext, EContext, ASTE);
    Expr := EContext.Result;
    if Assigned(Expr) then begin
      {if Expr.DataType = SYSUnit._Boolean then
      begin
        if (Expr.ItemType = itVar) and Expr.IsAnonymous then
          Bool_CompleteImmediateExpression(EContext, Expr);
      end;}
    end else
      ERROR_EXPRESSION_EXPECTED;

    Inc(ArgsCount);
    SetLength(Args, ArgsCount);
    Args[ArgsCount - 1] := EContext.RPNPopExpression();

    case Result of
      token_coma: begin
        continue;
      end;
      token_above: begin
        Result := parser_NextToken(Scope);
        Break;
      end;
      else
        AbortWork(sIncompleteStatement, parser_PrevPosition);
    end;
  end;

end;

function TASTDelphiUnit.ParseGenericTypeSpec(Scope: TScope; const ID: TIdentifier; out DataType: TIDType): TTokenID;
var
  SContext: TSContext;
  GenericArgs: TIDExpressions;
  SearchName: string;
begin
  SContext := TSContext.Create(Self);
  Result := ParseGenericsArgs(Scope, SContext, GenericArgs);
  SearchName := format('%s<%d>', [ID.Name, Length(GenericArgs)]);
  DataType := TIDType(FindIDNoAbort(Scope, SearchName));
  if Assigned(DataType) then
    DataType := SpecializeGenericType(DataType, ID, GenericArgs)
  else
    AbortWorkInternal('Invalid generic type params');
end;

function TASTDelphiUnit.ParseGoToStatement(Scope: TScope; var SContext: TSContext): TTokenID;
var
  ID: TIdentifier;
  LDecl: TIDDeclaration;
  KW: TASTKWGoTo;
begin
  parser_ReadNextIdentifier(Scope, ID);
  LDecl := FindID(Scope, ID);
  CheckLabelExpression(LDecl);
  KW := SContext.Add<TASTKWGoTo>;
  KW.&Label := LDecl;
  Result := parser_NextToken(Scope);
end;

function TASTDelphiUnit.ParseIfThenStatement(Scope: TScope; var SContext: TSContext): TTokenID;
var
  Expression: TIDExpression;
  EContext: TEContext;
  ThenSContext: TSContext;
  ElseSContext: TSContext;
  NewScope: TScope;
  KW: TASTKWIF;
  CondExpr: TASTExpression;
begin
  KW := SContext.Add(TASTKWIF) as TASTKWIF;
  InitEContext(EContext, SContext, ExprRValue);
  parser_NextToken(Scope);
  Result := ParseExpression(Scope, SContext, EContext, CondExpr);
  KW.Expression := CondExpr;
  Expression := EContext.Result;
  CheckEmptyExpression(Expression);
  CheckBooleanExpression(Expression);

  {then section}
  parser_MatchToken(Result, token_then);
  Result := parser_NextToken(Scope);
  if Result <> token_semicolon then
  begin
    {оптимизация, не создаем лишний scope, если внутри он создастся всеравно}
    if Result <> token_begin then
      NewScope := TScope.Create(stLocal, Scope)
    else
      NewScope := Scope;

    ThenSContext := SContext.MakeChild(KW.ThenBody);

    Result := ParseStatements(NewScope, ThenSContext, False);
  end;
  { else section}
  if Result = token_else then
  begin
    Result := parser_NextToken(Scope);
    {оптимизация, не создаем лишний scope, если внутри он создастся всеравно}
    if Result <> token_begin then
      NewScope := TScope.Create(stLocal, Scope)
    else
      NewScope := Scope;

    KW.ElseBody := TASTKWIF.TASTKWIfElseBlock.Create(KW);
    ElseSContext := SContext.MakeChild(KW.ElseBody);
    Result := ParseStatements(NewScope, ElseSContext, False);
  end;
end;

function TASTDelphiUnit.ParseImmVarStatement(Scope: TScope; var SContext: TSContext): TTokenID;
var
  i, c: Integer;
  DataType: TIDType;
  Expr: TIDExpression;
  Variable: TIDVariable;
  Names: TIdentifiersPool;
  EContext: TEContext;
  Vars: array of TIDVariable;
  KW: TASTKWInlineVarDecl;
begin
  c := 0;
  Names := TIdentifiersPool.Create(1);
  Result := parser_NextToken(Scope);

  KW := SContext.Add<TASTKWInlineVarDecl>;

  while True do begin
    parser_MatchIdentifier(Result);
    Names.Add;
    parser_ReadCurrIdentifier(Names.Items[c]);
    Result := parser_NextToken(Scope);
    if Result = token_Coma then begin
      Inc(c);
      Result := parser_NextToken(Scope);
      Continue;
    end;

    // парсим тип, если определен
    if Result = token_colon then
      Result := ParseTypeSpec(Scope, DataType)
    else
      DataType := nil;

    parser_MatchToken(Result, token_assign);

    InitEContext(EContext, SContext, ExprRValue);

    SetLength(Vars, c + 1);
    for i := 0 to c do
    begin
      Variable := TIDVariable.Create(Scope, Names.Items[i]);
      Variable.DataType := DataType;
      Variable.Visibility := vLocal;
      Variable.DefaultValue := nil;
      Variable.Absolute := nil;
      Scope.AddVariable(Variable);
      Vars[i] := Variable;
      Expr := TIDExpression.Create(Variable, Variable.TextPosition);
      EContext.RPNPushExpression(Expr);
      KW.AddDecl(Variable);
    end;

    parser_NextToken(Scope);
    var ASTExpr: TASTExpression := nil;
    Result := ParseExpression(Scope, SContext, EContext, ASTExpr);
    KW.Expression := ASTExpr;

    if not Assigned(DataType) then
    begin
      DataType := EContext.Result.DataType;
      for i := 0 to c do
        Vars[i].DataType := DataType
    end;

    EContext.RPNPushOperator(opAssignment);
    EContext.RPNFinish;

    parser_MatchSemicolon(Result);
    break;
  end;
end;

function TASTDelphiUnit.ParseLabelSection(Scope: TScope): TTokenID;
var
  ID: TIdentifier;
  Decl: TASTDelphiLabel;
begin
  while True do
  begin
    parser_ReadNextIdentifier(Scope, ID);
    Decl := TASTDelphiLabel.Create(Scope, ID);
    InsertToScope(Scope, Decl);
    Result := parser_NextToken(Scope);
    if Result = token_coma then
      continue;

    break;
  end;
  parser_MatchSemicolon(Result);
  Result := parser_NextToken(Scope);
end;

function TASTDelphiUnit.ParseProcBody(Proc: TASTDelphiProc): TTokenID;
var
  Scope: TScope;
  SContext: TSContext;
begin
  Scope := Proc.EntryScope;
  Result := parser_CurTokenID;
  while true do begin
    case Result of
      token_var: begin
        parser_NextToken(Scope);
        Result := ParseVarSection(Scope, vLocal, nil, False);
      end;
      token_weak: begin
        parser_NextToken(Scope);
        Result := ParseVarSection(Scope, vLocal, nil, True);
      end;
      token_label: Result := ParseLabelSection(Scope);
      token_const: Result := ParseConstSection(Scope);
      token_type: Result := ParseNamedTypeDecl(Scope);
      token_procedure: Result := ParseProcedure(Scope, ptProc);
      token_function: Result := ParseProcedure(Scope, ptFunc);
      token_identifier: ERROR_KEYWORD_EXPECTED;
      token_asm: begin
        // skip the asm...end block
        parser_SkipBlock(token_end);
        Result := parser_NextToken(Scope);
        Exit;
      end;
      token_begin: begin
        Proc.FirstBodyLine := parser_Line;
        Proc.fBody := TASTBlock.Create(Proc);
        //SContext.Initialize;
        //SContext.IL := TIL(Proc.IL);
        SContext := TSContext.Create(Self, Proc, Proc.fBody);
        //CheckInitVariables(@SContext, nil, @Proc.VarSpace);
        parser_NextToken(Scope);
        Result := ParseStatements(Scope, SContext, True);
        parser_MatchToken(Result, token_end);
        Result := parser_NextToken(Scope);
        Proc.LastBodyLine := parser_Line;
        // геренация кода процедуры завершено
        Proc.Flags := Proc.Flags + [pfCompleted];
        BENodesPool.Clear;
        Exit;
      end;
    else
      ERROR_BEGIN_KEYWORD_EXPECTED;
    end;
  end;
end;

function TASTDelphiUnit.ParseProcedure(Scope: TScope; ProcType: TProcType; Struct: TIDStructure): TTokenID;
type
  TFwdDeclState = (dsNew, dsDifferent, dsSame);
var
  ID: TIdentifier;
  Parameters: TProcScope;
  ResultType: TIDType;
  ResultParam: TIDVariable;
  VarSpace: TVarSpace;
  GenericsParams: TIDTypeList;
  Proc, ForwardDecl: TASTDelphiProc;
  FwdDeclState: TFwdDeclState;
  FirstSkipCnt: Integer;
  SRCProcPos: TParserPosition;
  CallConv: TCallConvention;
  ProcFlags: TProcFlags;
  ForwardScope: TScope;
begin
  ForwardScope := Scope;
  Result := ParseProcName(ForwardScope, ID, Struct, Parameters, GenericsParams);

  VarSpace.Initialize;
  Parameters.VarSpace := addr(VarSpace);

  // создаем Result переменную (тип будет определен позже)
  if ProcType < ptProc then
    ResultParam := AddResultParameter(Parameters)
  else
    ResultParam := nil;

  if Assigned(Struct) then
    AddSelfParameter(Parameters, Struct, (ProcType = ptClassProc) or (ProcType = ptClassFunc))
  else
    Parameters.OuterScope := Scope;

  Parser.SaveState(SRCProcPos);

  // парсим параметры
  if Result = token_openround then
  begin
    ParseParameters(Parameters);
    Result := parser_NextToken(Scope);
  end;

  // парсим тип возвращаемого значения
  if ProcType <= ptStaticFunc then begin
    parser_MatchToken(Result, token_colon);
    Result := ParseTypeSpec(Parameters, ResultType);
    ResultParam.DataType := ResultType;
    ResultParam.TextPosition := parser_Position;
  end else
    ResultType := nil;

  parser_MatchToken(Result, token_semicolon);

  // ищем ранее обьявленную декларацию с таким же именем
  if Assigned(Struct) then
  begin
    ForwardDecl := TASTDelphiProc(Struct.Members.FindID(ID.Name));
    if not Assigned(ForwardDecl) and (Scope.ScopeClass = scImplementation) then
      ERROR_METHOD_NOT_DECLARED_IN_CLASS(ID, Struct);
  end else
    ForwardDecl := TASTDelphiProc(ForwardScope.FindID(ID.Name));

  Proc := nil;
  FwdDeclState := dsDifferent;

  {если найдена ранее обьявленная декларация, проверяем соответствие}
  if Assigned(ForwardDecl) then begin
    // ошибка если перекрыли идентификатор другого типа:
    if ForwardDecl.ItemType <> itProcedure then
      ERROR_ID_REDECLARATED(ID);
    // ищем подходящую декларацию в списке перегруженных:
    while True do begin
      if ForwardDecl.SameDeclaration(Parameters) then begin
        // нашли подходящую декларацию
        FwdDeclState := dsSame;
        if Assigned(ForwardDecl.IL) or (Scope.ScopeClass = scInterface) then
          ERROR_ID_REDECLARATED(ID);
        Proc := ForwardDecl;
        Break;
      end;
      // не нашли подходящую декларацию, будем создавать новую,
      // проверку дерективы overload оставим на потом
      if not Assigned(ForwardDecl.NextOverload) then
        Break;
      ForwardDecl := TASTDelphiProc(ForwardDecl.NextOverload);
    end;
  end else
    FwdDeclState := dsNew;

  {создаем новую декларацию}
  if not Assigned(Proc) then
  begin
    Proc := TASTDelphiProc.Create(Scope, ID);
    // если это generic-процедура или это generic-метод
    if Assigned(GenericsParams) then
      Proc.CreateGenericDescriptor(GenericsParams, SRCProcPos)
    else if Assigned(Struct) and Assigned(Struct.GenericDescriptor) then
      Proc.CreateGenericDescriptor(Struct.GenericDescriptor.GenericParams, SRCProcPos);

    // Для Scope будут переопределены VarSpace и ProcSpace
    Proc.ParamsScope := Parameters;
    Proc.VarSpace := VarSpace;
    Proc.ResultType := ResultType;

    FirstSkipCnt := 0;
    if Assigned(Struct) then
      Inc(FirstSkipCnt);
    if Assigned(ResultType) then
      Inc(FirstSkipCnt);
    Proc.ExplicitParams := ScopeToVarList(Parameters, FirstSkipCnt);

    case ProcType of
      ptClassFunc,
      ptClassProc: ProcFlags := [pfClass];
      ptStaticFunc,
      ptStaticProc: ProcFlags := [pfStatic];
      ptConstructor: begin
        if not Assigned(Struct) then
          ERROR_CTOR_DTOR_MUST_BE_DECLARED_IN_STRUCT(parser_PrevPosition);
        ProcFlags := [pfConstructor];
      end;
      ptDestructor: begin
        if not Assigned(Struct) then
          ERROR_CTOR_DTOR_MUST_BE_DECLARED_IN_STRUCT(parser_PrevPosition);
        ProcFlags := [pfDestructor];
      end
    else
      ProcFlags := [];
    end;

    // добовляем новую декларацию в структуру или глобольный список или к списку перегруженных процедур
    if not Assigned(ForwardDecl) then
    begin
      if Assigned(Struct) then begin
        // метод
        Struct.Members.AddProcedure(Proc);
        Proc.Struct := Struct;
        if Struct.DataTypeID = dtRecord then
        case ProcType of
          ptConstructor: begin
            if Proc.ParamsCount = 0 then
            begin
              if Assigned(TIDRecord(Struct).StaticConstructor) then
                ERROR_RECORD_STATIC_CONSTRUCTOR_ALREADY_EXIST(Proc);
              TIDRecord(Struct).StaticConstructor := Proc;
            end;
          end;
          ptDestructor: begin
            CheckStaticRecordConstructorSign(Proc);
            if Assigned(TIDRecord(Struct).StaticDestructor) then
              ERROR_RECORD_STATIC_DESTRUCTOR_ALREADY_EXIST(Proc);
            TIDRecord(Struct).StaticDestructor := Proc;
          end;
        end;
      end else
        // глобальная процедура
        Scope.AddProcedure(Proc);
    end else begin
      // доавляем в список следующую перегруженную процедуру
      ForwardDecl.NextOverload := Proc;
      if Assigned(Struct) then
      begin
        Struct.AddMethod(Proc);
        Proc.Struct := Struct;
      end else
        Scope.ProcSpace.Add(Proc);
    end;
  end else begin
    if Assigned(Proc.GenericDescriptor) then
      Proc.GenericDescriptor.ImplSRCPosition := SRCProcPos;
    ProcFlags := [];
  end;
  CallConv := ConvNative;
  Result := parser_NextToken(Scope);
  while True do begin
    case Result of
      token_forward: Result := ProcSpec_Forward(Scope, Proc, ProcFlags);
      token_export: Result := ProcSpec_Export(Scope, Proc, ProcFlags);
      token_inline: Result := ProcSpec_Inline(Scope, Proc, ProcFlags);
      token_external: Result := ProcSpec_Import(Scope, Proc, ProcFlags);
      token_overload: Result := ProcSpec_Overload(Scope, Proc, ProcFlags);
      token_virtual: Result := ProcSpec_Virtual(Scope, Proc, ProcFlags);
      token_override: Result := ProcSpec_Override(Scope, Proc, ProcFlags);
      token_reintroduce: Result := ProcSpec_Reintroduce(Scope, Proc, ProcFlags);
      token_static: Result := ProcSpec_Static(Scope, Proc, ProcFlags);
      token_stdcall: Result := ProcSpec_StdCall(Scope, CallConv);
      token_fastcall: Result := ProcSpec_FastCall(Scope, CallConv);
      token_cdecl: Result := ProcSpec_CDecl(Scope, CallConv);
      token_varargs: begin
        parser_ReadSemicolon(Scope);
        Result := parser_NextToken(Scope);
      end;
      token_deprecated: begin
        Result := CheckAndParseDeprecated(Scope, token_deprecated);
        Result := parser_NextToken(Scope);
      end;
      token_platform: begin
        Result := ParsePlatform(Scope);
        Result := parser_NextToken(Scope);
      end;
    else
      if (Scope.ScopeClass = scInterface) or (pfImport in ProcFlags) then begin
        Proc.Flags := ProcFlags;
        Proc.CallConvention := CallConv;
        Break;
      end;

      // имена парметров реализации процедуры могут отличатся от ее определения
      // копируем накопленный VarSpace в процедуру

      Proc.VarSpace := VarSpace;

      Parameters.ProcSpace := Proc.ProcSpace;
      Proc.EntryScope := Parameters;

      if (FwdDeclState = dsDifferent) and not (pfOveload in ProcFlags) then
      begin
        if Assigned(Proc.IL) then
          ERROR_OVERLOADED_MUST_BE_MARKED(ID)
        else
          ERROR_DECL_DIFF_WITH_PREV_DECL(ID);
      end;
      Result := ParseProcBody(Proc);
      if Result <> token_semicolon then
        ERROR_SEMICOLON_EXPECTED;

      Result := parser_NextToken(Scope);
      Break;
    end;
  end;

  if (ProcType = ptDestructor) and (Struct.DataTypeID = dtClass) then
    CheckDestructorSignature(Proc);
end;

function TASTDelphiUnit.ParseRaiseStatement(Scope: TScope; var SContext: TSContext): TTokenID;
var
  EExcept: TIDExpression;
  EContext: TEContext;
  ASTExpr: TASTExpression;
  KW: TASTKWRaise;
begin
  parser_NextToken(Scope);
  InitEContext(EContext, SContext, ExprRValue);
  Result := ParseExpression(Scope, SContext, EContext, ASTExpr);
  EExcept := EContext.Result;
  if Assigned(EExcept) then
    CheckClassExpression(EExcept);
  KW := SContext.Add<TASTKWRaise>;
  KW.Expression := ASTExpr;
end;

function TASTDelphiUnit.ParseRecordInitValue(Scope: TRecordInitScope; var FirstField: TIDExpression): TTokenID;
var
  FldValue: TIDExpression;
begin
  parser_NextToken(Scope);
  Result := ParseConstExpression(Scope, FldValue, ExprRValue);
  if Result = token_semicolon then
    Result := parser_NextToken(Scope);
end;

function TASTDelphiUnit.ParseRepeatStatement(Scope: TScope; var SContext: TSContext): TTokenID;
var
  Expression: TIDExpression;
  EContext: TEContext;
  BodySContext: TSContext;
  KW: TASTKWRepeat;
  ASTExpr: TASTExpression;
begin
  KW := SContext.Add<TASTKWRepeat>;

  BodySContext := SContext.MakeChild(KW.Body);

  parser_NextToken(Scope);
  // тело цикла
  Result := ParseStatements(Scope, BodySContext, True);
  parser_MatchToken(Result, token_until);

  // выражение цикла
  InitEContext(EContext, SContext, ExprRValue);
  parser_NextToken(Scope);
  Result := ParseExpression(Scope, SContext, EContext, ASTExpr);
  KW.Expression := ASTExpr;
  Expression := EContext.Result;
  CheckEmptyExpression(Expression);
  CheckBooleanExpression(Expression);
end;

function TASTDelphiUnit.ParseMember(Scope: TScope; out Expression: TIDExpression; var EContext: TEContext;
                                    var SContext: TSContext; const ASTE: TASTExpression): TTokenID;
var
  Decl: TIDDeclaration;
  DataType: TIDType;
  Indexes: TIDExpressions;
  i: Integer;
  Expr, NExpr: TIDExpression;
  StrictSearch: Boolean;
  GenericArgs: TIDExpressions;
  CallExpr: TIDCallExpression;
  PMContext: TPMContext;
  WasProperty: Boolean;
begin
  WasProperty := False;
  PMContext.Init;
  PMContext.ItemScope := Scope;
  StrictSearch := False;
  while True do begin
    parser_ReadCurrIdentifier(PMContext.ID);
    if not StrictSearch then
      Decl := FindIDNoAbort(PMContext.ItemScope, PMContext.ID, Expr)
    else begin
      Decl := PMContext.ItemScope.FindMembers(PMContext.ID.Name);
      Expr := nil;
    end;

    Result := parser_NextToken(Scope);
    if not Assigned(Decl) then begin
      if Result = token_less then
        Result := ParseGenericMember(PMContext, {SContext}nil, StrictSearch, Decl, Expr);

      if PMContext.ItemScope is TConditionalScope then
      begin
        Decl := TIDStringConstant.CreateAnonymous(PMContext.ItemScope, SYSUnit._String, PMContext.ID.Name);
      end;

      if not Assigned(Decl) then
        ERROR_UNDECLARED_ID(PMContext.ID);
    end;

    // если Scope порожден конструкцией WITH
    // то добавляем в выражение первым элементом
    if Assigned(Expr) then begin
      if Expr.ExpressionType = etDeclaration then begin
        PMContext.Add(Expr);
      end else begin
        Indexes := TIDMultiExpression(Expr).Items;
        for i := 0 to Length(Indexes) - 1 do
          PMContext.Add(Indexes[i]);
      end;
    end;

    {проверяем на псевдоним}
    if Decl.ItemType = itAlias then
      Decl := TIDAlias(Decl).Original;

    {проверяем на доступ к члену типа}
    //if Assigned(EContext.SContext) then
    //  CheckAccessMember(SContext, Decl, PMContext.ID);

    case Decl.ItemType of
      {процедура/функция}
      itProcedure: begin
        Expression := TIDCallExpression.Create(Decl, PMContext.ID.TextPosition);
        // если есть открытая угловая скобка, - значит generic-вызов
        if Result = token_less then
        begin
          Result := ParseGenericsArgs(Scope, SContext, GenericArgs);
          SetProcGenericArgs(TIDCallExpression(Expression), GenericArgs);
        end;
        // если есть открытая скобка, - значит вызов
        if Result = token_openround then
        begin
          Result := ParseEntryCall(Scope, TIDCallExpression(Expression), EContext, SContext, ASTE);
          // если это метод, подставляем self из пула
          if PMContext.Count > 0 then
          begin
            if PMContext.Count > 1 then
              Expr := ProcessMemberExpression({SContext}nil, WasProperty, PMContext)
            else
              Expr := PMContext.Last;

            if (Expr.Declaration is TIDField) and (PMContext.Count = 1) then
            begin
              NExpr := GetTMPRefExpr(SContext, Expr.DataType);
              NExpr.TextPosition := Expr.TextPosition;
              Expr := NExpr;
            end;
            TIDCallExpression(Expression).Instance := Expr;
          end else
          if (Decl.ItemType = itProcedure) and Assigned(TIDProcedure(Decl).Struct) and
             (TIDProcedure(Decl).Struct = SContext.Proc.Struct) then
          begin
            // если это собственный метод, добавляем self из списка параметров
            TIDCallExpression(Expression).Instance := TIDExpression.Create(SContext.Proc.SelfParam);
          end;

          // если выражение продолжается дальше, генерируем вызов процедуры
          if Result in [token_dot, token_openblock] then
          begin
            Expression := EContext.RPNPopOperator();
            Decl := Expression.Declaration;
            PMContext.Clear;
          end else begin
            Expression := nil;
            Break;
          end;

        end else begin // иначе создаем процедурный тип, если он отсутствовал
          TIDProcedure(Decl).CreateProcedureTypeIfNeed(Scope);
          PMContext.DataType := TIDProcedure(Decl).DataType;
          AddType(TIDProcedure(Decl).DataType);
        end;
      end;
      {макро функция}
      itMacroFunction: begin
        Expression := TIDExpression.Create(Decl, PMContext.ID.TextPosition);  // создание Expression под ???
        Result := ParseBuiltinCall(Scope, Expression, EContext);
        Expression := nil;
        Break;
      end;
      {переменная}
      itVar: begin
        {if Decl.ClassType = TIDMacroArgument then
          Result := ParseMacroArg(Scope, PMContext.ID, TIDMacroArgument(Decl), Expression, EContext)
        else }begin
          // если есть открытая скобка, - значит вызов
          if Result = token_openround then
          begin
            if Decl.DataTypeID <> dtProcType then
              ERROR_PROC_OR_PROCVAR_REQUIRED(PMContext.ID);

            Expression := TIDCallExpression.Create(Decl, PMContext.ID.TextPosition);
            Result := ParseEntryCall(Scope, TIDCallExpression(Expression), EContext, SContext, ASTE);
            Expression := nil;
            Break;
          end;
          Expression := TIDExpression.Create(Decl, PMContext.ID.TextPosition);
        end;
        PMContext.DataType := Decl.DataType;
      end;
      itConst, itUnit, itLabel: begin
        Expression := TIDExpression.Create(Decl, PMContext.ID.TextPosition);
        PMContext.DataType := Decl.DataType;
      end;
      {свойство}
      itProperty: begin
        WasProperty := True;
        Result := ParsePropertyMember(PMContext, Scope, TIDProperty(Decl), Expression, EContext);
        // заменяем декларацию
        if Assigned(Expression) then
        begin
          Decl := Expression.Declaration;
          // если геттер/сеттер - поле, то снимаем флаг "свойства"
          if Decl.ItemType = itVar then
            WasProperty := False;
        end;
        PMContext.DataType := Decl.DataType;
      end;
      {тип}
      itType: begin
        Expression := TIDExpression.Create(Decl, PMContext.ID.TextPosition);
        {явное преобразование типов}
        if Result = token_openround then
           Result := ParseExplicitCast(Scope, SContext, Expression);

        PMContext.DataType := Decl.DataType;
      end;
    else
      ERROR_FEATURE_NOT_SUPPORTED;
    end;

    PMContext.Add(Expression);
    ASTE.AddDeclItem(Expression.Declaration, Expression.TextPosition);

    {array type}
    if Result = token_openblock then begin
      Result := ParseArrayMember(Scope, PMContext, Decl, PMContext.DataType, EContext, SContext, ASTE);
      if PMContext.DataType = nil then
      begin
        Expression := nil;
        Break;
      end;
    end;

    {call} // todo: make this code as main (remove previous same code)
    if (Result = token_openround) and (Expression.DataTypeID = dtProcType) then
    begin
      if Expression.DataTypeID <> dtProcType then
        ERROR_PROC_OR_PROCVAR_REQUIRED(PMContext.ID);

      if not (Expression is TIDCastExpression) then
        CallExpr := TIDCallExpression.Create(Expression.Declaration, Expression.TextPosition)
      else begin
        CallExpr := TIDCastedCallExpression.Create(Expression.Declaration, Expression.TextPosition);
        TIDCastedCallExpression(CallExpr).DataType := Expression.DataType;
      end;

      Result := ParseEntryCall(Scope, CallExpr, EContext, SContext, ASTE);
      Expression := nil;
      Break;
    end;

    if Result = token_caret then
    begin
      //EContext.RPNPushOperator(opDereference);
      Result := parser_NextToken(Scope);
    end;


    {struct/class/interafce/enum/unit/namespace}
    if (Result = token_dot) then
    begin
      if Decl.ItemType = itUnit then begin
        PMContext.ItemScope := TIDNameSpace(Decl).Members;
        PMContext.Clear;
        parser_NextToken(Scope);
        StrictSearch := True;
        continue;
      end else
      if Decl.ItemType <> itType then
        DataType := Decl.DataType
      else
        DataType := TIDType(Decl);

      if DataType.ClassType = TIDAliasType then
        DataType := TIDAliasType(DataType).Original;

      if DataType.DataTypeID in [dtStaticArray, dtDynArray] then
        DataType := TIDArray(DataType).ElementDataType;

      if DataType.DataTypeID in [dtPointer, dtClassOf] then
        DataType := TIDPointer(DataType).ReferenceType;

      if DataType is TIDStructure then
        PMContext.ItemScope := TIDStructure(DataType).Members
      else
      if Decl.ClassType = TIDEnum then begin
        PMContext.ItemScope := TIDEnum(Decl).Items;
        PMContext.Clear;
      end else
        ERROR_IDENTIFIER_HAS_NO_MEMBERS(Decl);

      ASTE.AddOperation<TASTOpMemberAccess>;

      parser_NextToken(Scope);
      StrictSearch := True;
      continue;
    end;

    if PMContext.Count > 1 then
    begin
      Expression := TIDMultiExpression.Create(PMContext.Items, PMContext.ID.TextPosition);
      TIDMultiExpression(Expression).EffectiveDataType := PMContext.DataType;
    end;

    break;
  end;
end;

function TASTDelphiUnit.ParseIndexedPropertyArgs(Scope: TScope; out ArgumentsCount: Integer; var EContext: TEContext): TTokenID;
var
  Expr: TIDExpression;
  InnerEContext: TEContext;
  SContext: PSContext;
  ASTExpr: TASTExpression;
begin
  ArgumentsCount := 0;
  SContext := @EContext.SContext;
  InitEContext(InnerEContext, SContext^, ExprNested);
  {цикл парсинга аргументов}
  while true do begin
    parser_NextToken(Scope);
    Result := ParseExpression(Scope, SContext^, InnerEContext, ASTExpr);
    Expr := InnerEContext.Result;
    if Assigned(Expr) then begin
      if Expr.DataType = SYSUnit._Boolean then
      begin
        {if (Expr.ItemType = itVar) and Expr.IsAnonymous then
          Bool_CompleteImmediateExpression(InnerEContext, Expr);}
      end;
      EContext.RPNPushExpression(Expr);
    end else begin
      // Добавляем пустой Expression для значения по умолчанию
      EContext.RPNPushExpression(nil);
    end;
    Inc(ArgumentsCount);
    case Result of
      token_coma: begin
        InnerEContext.Reset;
        continue;
      end;
      token_closeblock: begin
        Result := parser_NextToken(Scope);
        Break;
      end;
      else
        AbortWork(sIncompleteStatement, parser_PrevPosition);
    end;
  end;
end;

function TASTDelphiUnit.ParseInitSection: TTokenID;
var
  SContext: TSContext;
begin
  if FInitProcExplicit then
    ERROR_INIT_SECTION_ALREADY_DEFINED;

  FInitProcExplicit := True;

  SContext := TSContext.Create(Self, InitProc as TASTDelphiProc, TASTDelphiProc(InitProc).Body);
  parser_NextToken(InitProc.Scope);
  InitProc.FirstBodyLine := parser_Line;
  Result := ParseStatements(InitProc.Scope, SContext, True);
  InitProc.LastBodyLine := parser_Line;
end;

function TASTDelphiUnit.ParseFinalSection: TTokenID;
var
  SContext: TSContext;
begin
  if fFinalProcExplicit then
    ERROR_FINAL_SECTION_ALREADY_DEFINED;
  FFinalProcExplicit := True;
  SContext := TSContext.Create(Self, FinalProc as TASTDelphiProc, TASTDelphiProc(FinalProc).Body);
  parser_NextToken(FinalProc.Scope);
  FinalProc.FirstBodyLine := parser_Line;
  Result := ParseStatements(FinalProc.Scope, SContext, True);
  FinalProc.LastBodyLine := parser_Line;
end;

function TASTDelphiUnit.ParseVarDefaultValue(Scope: TScope; DataType: TIDType; out DefaultValue: TIDExpression): TTokenID;
var
  SContext: TSContext;
  EContext: TEContext;
  ASTE: TASTExpression;
begin
  case DataType.DataTypeID of
    dtStaticArray: Result := ParseVarStaticArrayDefaultValue(Scope, DataType as TIDArray, DefaultValue);
    dtRecord: begin
      if (DataType.DeclUnit = SYSUnit) and (DataType.Name = 'TGUID') then
      begin
        parser_NextToken(Scope);
        Result := ParseConstExpression(Scope, DefaultValue, ExprRValue)
      end else
        Result := ParseVarRecordDefaultValue(Scope, DataType as TIDStructure, DefaultValue);
    end
  else
    SContext := TSContext.Create(Self);

    Result := parser_NextToken(Scope);

    if Scope.ScopeType = stLocal then
      Result := ParseConstExpression(Scope, DefaultValue, ExprRValue)
    else begin
      InitEContext(EContext, SContext, ExprRValue);
      Result := ParseExpression(Scope, SContext, EContext, ASTE);
      DefaultValue := EContext.Result;
    end;
    CheckEmptyExpression(DefaultValue);

    if CheckImplicit(DefaultValue, DataType) = nil then
      ERROR_INCOMPATIBLE_TYPES(DefaultValue, DataType);

    DefaultValue := MatchImplicit3(SContext, DefaultValue, DataType);

    if DefaultValue.IsAnonymous then
      DefaultValue.Declaration.DataType := DataType; // подгоняем фактичиский тип константы под необходимый
  end;
end;

function TASTDelphiUnit.ParseVarStaticArrayDefaultValue(Scope: TScope; ArrType: TIDArray; out DefaultValue: TIDExpression): TTokenID;
  function DoParse(ArrType: TIDArray; DimIndex: Integer; const CArray: TIDDynArrayConstant): TTokenID;
  var
    i, c: Integer;
    Expr: TIDExpression;
    EContext: TEContext;
    SContext: TSContext;
    NewScope: TScope;
  begin
    Result := parser_NextToken(Scope);
    // first element initializer
    if Result = token_identifier then
    begin
      Result := ParseConstExpression(Scope, Expr, ExprRValue);
      Exit; // todo:
    end;

    if ArrType.ElementDataType.DataTypeID = dtRecord then
    begin
      NewScope := TRecordInitScope.Create(stGlobal, Scope);
      TRecordInitScope(NewScope).Struct := TIDStructure(ArrType.ElementDataType);
      NewScope.AddScope(TIDStructure(ArrType.ElementDataType).Members);
    end else
      NewScope := Scope;

    parser_MatchToken(Result, token_openround);
    c := ArrType.Dimensions[DimIndex].ElementsCount - 1;
    for i := 0 to c do
    begin
      if ArrType.DimensionsCount > (DimIndex + 1) then
        Result := DoParse(ArrType, DimIndex + 1, CArray)
      else begin
        parser_NextToken(Scope);
        Result := ParseConstExpression(NewScope, Expr, ExprNested);
        CheckEmptyExpression(Expr);
        if CheckImplicit(Expr, ArrType.ElementDataType) = nil then
          ERROR_INCOMPATIBLE_TYPES(Expr, ArrType.ElementDataType);
        Expr.Declaration.DataType := ArrType.ElementDataType;
        CArray.AddItem(Expr);
      end;
      if i < c  then
        parser_MatchToken(Result, token_coma);
    end;
    parser_MatchToken(Result, token_closeround);
    Result := parser_NextToken(Scope);
  end;
begin
  DefaultValue := CreateAnonymousConstTuple(Scope, ArrType.ElementDataType);
  Result := DoParse(ArrType, 0, DefaultValue.AsDynArrayConst);
end;

procedure TASTDelphiUnit.ParseVector(Scope: TScope; var EContext: TEContext);
  function MaxType(Type1, Type2: TIDType): TIDType;
  begin
    if Type1 = Type2 then
      Exit(Type1);

    if (Type1 = SYSUnit._Variant) or
       (Type2 = SYSUnit._Variant) then
     Exit(SYSUnit._Variant);

    if Assigned(Type2.GetImplicitOperatorTo(Type1)) then
    begin
      if Type1.DataSize >= Type2.DataSize then
        Result := Type1
      else
        Result := Type2;
    end else
      Result := SYSUnit._Variant;
  end;
var
  i, c, Capacity: Integer;
  InnerEContext: TEContext;
  AConst: TIDDynArrayConstant;
  Expr: TIDExpression;
  Token: TTokenID;
  SItems, DItems: TIDExpressions;
  AType: TIDDynArray;
  ElDt: TIDType;
  IsStatic: Boolean;
  ASTExpr: TASTExpression;
begin
  i := 0;
  c := 0;
  Capacity := 8;
  IsStatic := True;
  SetLength(SItems, Capacity);
  var SContext := EContext.SContext;
  InitEContext(InnerEContext, SContext, ExprNested);
  while True do begin
    parser_NextToken(Scope);
    Token := ParseExpression(Scope, SContext, InnerEContext, ASTExpr);
    Expr := InnerEContext.Result;
    if Assigned(Expr) then begin
      if Expr.Declaration.ItemType <> itConst then
        IsStatic := False;
      Inc(c);
      SItems[i] := Expr;
    end else
    if i > 0 then
      CheckEmptyExpression(Expr);

    case Token of
      token_coma: begin
        if i = 0 then CheckEmptyExpression(Expr);
        Inc(i);
        if i >= Capacity then begin
          Capacity := Capacity * 2;
          SetLength(SItems, Capacity);
        end;
        InnerEContext.Reset;
        Continue;
      end;
      token_closeblock: Break;
      else
        ERROR_UNCLOSED_OPEN_BLOCK;
    end;
  end;

  if c > 0 then begin
    // копирование элементов
    SetLength(DItems, c);
    Move(SItems[0], DItems[0], SizeOf(Pointer)*c);
    // вывод типа элемента
    Expr := SItems[0];
    ElDt := Expr.DataType;
    for i := 1 to c - 1 do begin
      Expr := SItems[i];
      ElDt := MaxType(ElDt, Expr.DataType);
    end;
  end else
    ElDt := SYSUnit._Variant;

  // создаем анонимный тип константного массива
  AType := TIDDynArray.CreateAsAnonymous(Scope);
  AType.ElementDataType := ElDt;
  AType.OverloadImplicitTo(dtSet, TIDOpImplicitDynArrayToSet.CreateInternal(nil));
  // добовляем его в пул
  AddType(AType);
  // создаем анонимный константный динамический массив
  AConst := TIDDynArrayConstant.CreateAnonymous(Scope, AType, DItems);
  AConst.ArrayStatic := IsStatic;
  // если массив константный, добовляем его в пул констант
  if IsStatic then
    AddConstant(AConst);

  Expr := TIDExpression.Create(AConst, Parser.Position);
  // заталкиваем массив в стек
  EContext.RPNPushExpression(Expr);
end;

function TASTDelphiUnit.ParsePropertyMember(var PMContext: TPMContext; Scope: TScope; Prop: TIDProperty; out Expression: TIDExpression; var EContext: TEContext): TTokenID;
var
  CallExpr: TIDCallExpression;
  SelfExpr: TIDExpression;
  Accessor: TIDDeclaration;
  ArgumentsCount: Integer;
begin
  Result := parser_CurTokenID;
  Expression := TIDExpression.Create(Prop, PMContext.ID.TextPosition);
  if EContext.EPosition = ExprLValue then begin
    Accessor := TIDProperty(Prop).Setter;
    if not Assigned(Accessor) then
      ERROR_CANNOT_MODIFY_READONLY_PROPERTY(Expression);
    // если сеттер - процедура, отодвигаем генерацию вызова на момент оброботки опрератора присвоения
    if Accessor.ItemType = itProcedure then
    begin
      Exit;
    end;
  end else begin
    Accessor := TIDProperty(Prop).Getter;
    if not Assigned(Accessor) then
      ERROR_CANNOT_ACCESS_TO_WRITEONLY_PROPERTY(Expression);
  end;
  Expression.Declaration := Accessor;
  case Accessor.ItemType of
    itConst: begin
      PMContext.Clear;
      Exit;
    end;
    itVar: Expression.Declaration := Accessor;
    itProperty: {продолжаем выполнение};
    itProcedure: begin
      case PMContext.Count of
        0: SelfExpr := TIDProcedure(EContext.Proc).SelfParamExpression;
        1: SelfExpr := PMContext.Last;
      else
        SelfExpr := GetTMPRefExpr(EContext.SContext, PMContext.DataType);
        SelfExpr.TextPosition := PMContext.ID.TextPosition;
      end;

      CallExpr := TIDCallExpression.Create(Accessor);
      CallExpr.TextPosition := Expression.TextPosition;
      CallExpr.Instance := SelfExpr;

      if Prop.ParamsCount > 0 then
        Result := ParseIndexedPropertyArgs(Scope, ArgumentsCount, EContext)
      else
        ArgumentsCount := 0;

      CallExpr.ArgumentsCount := ArgumentsCount;
      EContext.RPNPushExpression(CallExpr);

      Expression := Process_CALL(EContext);

      PMContext.Clear;
    end;
  else
    ERROR_FEATURE_NOT_SUPPORTED;
  end;
end;

{ TASTDelphiLabel }

constructor TASTDelphiLabel.Create(Scope: TScope; const Identifier: TIdentifier);
begin
  inherited;
  ItemType := itLabel;
end;

{ TSContextHelper }

function TSContextHelper.GetIsLoopBody: Boolean;
begin
  Result := Block.IsLoopBody;
end;

function TSContextHelper.GetIsTryBlock: Boolean;
begin
   Result := Block.IsTryBlock;
end;

{ TIDOperatorInternal }

(*constructor TIDInternalOperator.CreateAsIntOp;
begin
  CreateFromPool;
end;

constructor TIDInternalOpImplicit.CreateInternal(ResultType: TIDType);
begin
  CreateFromPool;
  ItemType := itProcedure;
  Self.DataType := ResultType;
end;*)

{ TIDSysRuntimeFunction }

class function TIDSysRuntimeFunction.GetFunctionID: TBuiltInFunctionID;
begin
  Result := bf_sysrtfunction;
end;

function TIDSysRuntimeFunction.Process(var EContext: TEContext): TIDExpression;
begin
  AbortWorkInternal('Method "Process" must be override');
  Result := nil;
end;

{ TIDIntOpImplicitStringToAnsiString }

function TIDOpImplicitStringToAnsiString.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Src.IsConstant then
  begin
    if IsAnsiString(Src.AsStrConst.Value) then
      Exit(Dst);
  end;
  Result := nil;
end;

function TIDOpImplicitStringToAnsiString.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
//var
//  UN: TNPUnit;
//  Str: string;
//  TmpVar: TIDVariable;
//  Constant: TIDStringConstant;
begin
//  if Src.IsVariable then
//  begin
//    TmpVar := SContext.Proc.GetTMPVar(Dst);
//    TmpVar.IncludeFlags([VarTmpResOwner]);
//    Result := TIDExpression.Create(TmpVar);
//    SContext.ILWrite(TIL.IL_Convert(Result, Src));
//  end else begin
//    Str := Src.AsStrConst.Value;
//    if IsAnsiString(Str) then
//    begin
//      UN := GetUnit(SContext);
//      Constant := TIDStringConstant.CreateAnonymous(UN.ImplSection, SYSUnit._AnsiString, Str);
//      Constant.Index := UN.Package.GetStringConstant(Constant);
//      Result := TIDExpression.Create(Constant, Src.TextPosition);
//    end else begin
//      TNPUnit.ERROR_STRING_CONST_IS_NOT_ANSI(Src);
//      Result := nil;
//    end;
//  end;
end;

{ TIDOpImplicitAnsiStringToString }

function TIDOpImplicitAnsiStringToString.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  Result := Self;
end;

function TIDOpImplicitAnsiStringToString.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
//var
//  UN: TNPUnit;
//  Str: string;
//  TmpVar: TIDVariable;
//  Constant: TIDStringConstant;
begin
//  if Src.IsVariable then
//  begin
//    TmpVar := SContext.Proc.GetTMPVar(Dst);
//    TmpVar.IncludeFlags([VarTmpResOwner]);
//    Result := TIDExpression.Create(TmpVar);
//    SContext.ILWrite(TIL.IL_Convert(Result, Src));
//  end else begin
//    UN := GetUnit(SContext);
//    Str := Src.AsStrConst.Value;
//    Constant := TIDStringConstant.CreateAnonymous(UN.ImplSection, SYSUnit._String, Str);
//    Constant.Index := UN.Package.GetStringConstant(Constant);
//    Result := TIDExpression.Create(Constant, Src.TextPosition);
//  end;
end;

{ TIDOpImplicitCharToString }

function TIDOpImplicitCharToString.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Src.IsConstant then
    Exit(SYSUnit._String)
  else
    Result := Self;
end;

function TIDOpImplicitCharToString.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
//var
//  UN: TNPUnit;
//  Chr: Char;
//  TmpVar: TIDVariable;
//  Constant: TIDStringConstant;
begin
//  if Src.IsVariable then
//  begin
//    TmpVar := SContext.Proc.GetTMPVar(SYSUnit._String);
//    TmpVar.IncludeFlags([VarTmpResOwner]);
//    Result := TIDExpression.Create(TmpVar, Src.TextPosition);
//    SContext.ILWrite(TIL.IL_Convert(Result, Src));
//  end else begin
//    if not SContext.WriteIL then
//      Exit(Src);
//    UN := GetUnit(SContext);
//    Chr := Src.AsCharConst.Value;
//    Constant := TIDStringConstant.CreateAnonymous(UN.ImplSection, SYSUnit._String, Chr);
//    Constant.Index := UN.Package.GetStringConstant(Constant);
//    Result := TIDExpression.Create(Constant, Src.TextPosition);
//  end;
end;

{ TIDOpImplicitCharToAnsiString }

function TIDOpImplicitCharToAnsiString.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Src.IsConstant then
    Exit(SYSUnit._AnsiString)
  else
    Result := Self;
end;

function TIDOpImplicitCharToAnsiString.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
begin
  Result := nil;
end;

{ TIDOpImplicitCharToAnsiChar }

function TIDOpImplicitCharToAnsiChar.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Src.IsConstant then
    Exit(SYSUnit._AnsiChar)
  else
    Result := Self;
end;

function TIDOpImplicitCharToAnsiChar.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
var
  Chr: Char;
  Constant: TIDCharConstant;
begin
  if Src.IsConstant then
  begin
    Constant := Src.AsCharConst;
    Chr := Constant.Value;
    if IsAnsiString(Chr) then
    begin
      Constant := TIDCharConstant.CreateAnonymous(Constant.Scope, SYSUnit._AnsiChar, Chr);
      Result := TIDExpression.Create(Constant, Src.TextPosition);
      Exit(Result);
    end;
  end;
  Result := nil;
end;

{ TIDOpImplicitAnsiCharToAnsiString }

function TIDOpImplicitAnsiCharToAnsiString.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  Result := nil;
end;

function TIDOpImplicitAnsiCharToAnsiString.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
begin
  Result := nil;
end;

{ TIDOpImplicitAnsiCharToString }

function TIDOpImplicitAnsiCharToString.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  Result := nil;
end;

function TIDOpImplicitAnsiCharToString.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
begin
  Result := nil;
end;

{ TIDOpImplicitAnsiCharToChar }

function TIDOpImplicitAnsiCharToChar.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  Result := nil;
end;

function TIDOpImplicitAnsiCharToChar.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
begin
  Result := nil;
end;

{ TIDInternalCastOperator }

function TIDOpImplicitMetaClassToGUID.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Src.AsType.DataTypeID <> dtInterface then
    TNPUnit.ERROR_INTF_TYPE_REQUIRED(Src);

  Result := Dst;
end;

function TIDOpImplicitMetaClassToGUID.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
//var
//  Intf: TIDInterface;
//  Decl: TIDGuidConstant;
//  UN: TNPUnit;
begin
//  if Src.AsType.DataTypeID <> dtInterface then
//    TNPUnit.ERROR_INTF_TYPE_REQUIRED(Src);
//
//  Intf := Src.AsType as TIDInterface;
//
//  UN := GetUnit(SContext);
//  if Intf.GUID = GUID_NULL then
//    UN.Warning('Interface type "%s" is has empty GUID', [Intf.DisplayName], Src.TextPosition);
//
//  Decl := TIDGuidConstant.CreateAnonymous(UN.ImplSection, SYSUnit._TGuid, Intf.GUID);
//  UN.AddConstant(Decl);
//
//  Result := TIDExpression.Create(Decl, Src.TextPosition);
end;

{ TIDIntOpImplicitStringToGUID }

function TIDOpImplicitStringToGUID.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
var
  GUID: TGUID;
begin
  if Src.IsConstant then
  begin
    if TryStrToGUID(Src.AsStrConst.Value, GUID) then
    begin
      Exit(Src.Declaration); // tmp
    end;
  end;
  Result := nil;
end;

function TIDOpImplicitStringToGUID.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
//var
//  GUID: TGUID;
//  Constant: TIDGuidConstant;
//  UN: TNPUnit;
begin
//  UN := GetUnit(SContext);
//  if Src.IsConstant then
//  begin
//    if TryStrToGUID(Src.AsStrConst.Value, GUID) then
//    begin
//      Constant := TIDGuidConstant.CreateAnonymous(UN.ImplSection, SYSUnit._TGuid, GUID);
//      UN.AddConstant(Constant);
//      Result := TIDExpression.Create(Constant, Src.TextPosition);
//      Exit(Result);
//    end;
//  end;
//  Result := nil;
end;


{ TIDIntOpImplicitClosureToTMethod }

function TIDOpImplicitClosureToTMethod.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  Result := nil;
end;

function TIDOpImplicitClosureToTMethod.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
//var
//  MethodExpr: TIDExpression;
begin
//  Result := TIDExpression.Create(SContext.Proc.GetTMPVar(Dst));
//  MethodExpr := TIDExpression.Create((Src.AsVariable.DataType as TIDClosure).Methods.Last);
//  SContext.ILWrite(TIL.IL_LDMethod(Result, Src, MethodExpr));
end;

{ TIDInternalOpImplicit }

constructor TIDInternalOpImplicit.CreateInternal(ResultType: TIDType);
begin
  CreateFromPool;
  ItemType := itProcedure;
  Self.DataType := ResultType;
end;

{ TIDOpImplicitDynArrayToSet }

function TIDOpImplicitDynArrayToSet.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
var
  Expr: TIDExpression;
begin
  if Src.IsDynArrayConst then
  begin
    Expr := TNPUnit.ConstDynArrayToSet(Src, Dst as TIDSet);
    Result := Expr.DataType;
  end else
    Result := nil;
end;

function TIDOpImplicitDynArrayToSet.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
begin
  if Src.IsDynArrayConst then
  begin
    Result := TNPUnit.ConstDynArrayToSet(Src, Dst as TIDSet);
  end else
    Result := nil;
end;

{ TIDOpImplicitAnyToVariant }

function TIDOpImplicitAnyToVariant.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Src.DataTypeID in [dtInt8, dtInt16, dtInt32, dtInt64, dtUInt8, dtUInt16, dtUInt32, dtUInt64, dtBoolean,
                        dtFloat32, dtFloat64, dtNativeInt, dtNativeUInt, dtChar, dtAnsiChar,
                        dtString, dtAnsiString] then
    Result := Self
  else
    Result := nil;
end;

function TIDOpImplicitAnyToVariant.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
begin
//  if SContext.Proc.Package.Options.VARIANT_EXPLICIT_CONVERT then
//  begin
//    Result := SContext.GetTMPVarExpr(SYSUnit._Variant, Src.TextPosition);
//    SContext.ILWrite(TIL.IL_Convert(Result, Src));
//  end else
//    Result := Src;
end;

{ TIDOpImplicitVariantToAny }

function TIDOpImplicitVariantToAny.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Dst.DataTypeID in [dtInt8, dtInt16, dtInt32, dtInt64, dtUInt8, dtUInt16, dtUInt32, dtUInt64, dtBoolean,
                        dtFloat32, dtFloat64, dtNativeInt, dtNativeUInt, dtChar, dtAnsiChar,
                        dtString, dtAnsiString, dtVariant] then
    Result := Self
  else
    Result := nil;
end;

function TIDOpImplicitVariantToAny.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
var
  TmpVar: TIDVariable;
begin
//  if Src.DataTypeID <> Dst.DataTypeID then
//  begin
//    TmpVar := SContext.GetTMPVar(Dst, [VarTmpResOwner]);
//    Result := TIDExpression.Create(TmpVar, Src.TextPosition);
//    SContext.ILWrite(TIL.IL_Convert(Result, Src));
//  end else
//    Result := Src;
end;

{ TIDOpExplicitIntToEnum }

function TIDOpExplicitIntToEnum.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  // пока так
  if Src.DataTypeID in [dtInt8, dtInt16, dtInt32, dtInt64, dtUInt8, dtUInt16, dtUInt32, dtUInt64] then
    Result := Dst
  else
    Result := nil;
end;

function TIDOpExplicitIntToEnum.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
begin
  // пока так
  if Src.DataTypeID in [dtInt8, dtInt16, dtInt32, dtInt64, dtUInt8, dtUInt16, dtUInt32, dtUInt64] then
    Result := Src
  else
    Result := nil;
end;

{ TIDOpExplicitTProcFromAny }

function TIDOpExplicitTProcFromAny.Match(const SContext: PSContext; const Src: TIDExpression;
                                         const Dst: TIDType): TIDExpression;
begin
  if (Src.DataTypeID = dtPointer) and (Dst as TIDProcType).IsStatic then
    Result := Src
  else
    Result := nil;
end;

function TIDOpExplicitTProcFromAny.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if (Src.DataTypeID = dtPointer) and (Dst as TIDProcType).IsStatic then
    Result := Dst
  else
    Result := nil;
end;

{ TIDInternalOperator }

constructor TIDInternalOperator.CreateAsIntOp;
begin
  CreateFromPool;
end;

{ TIDBuiltInFunction }

constructor TIDBuiltInFunction.Create(Scope: TScope; const Name: string; ResultType: TIDType);
begin
  CreateFromPool;
  FID.Name := Name;
  Scope := Scope;
  ItemType := itMacroFunction;
  Self.DataType := ResultType;
end;

class function TIDBuiltInFunction.CreateTMPExpr(const EContext: TEContext; const DataType: TIDType): TIDExpression;
var
  Decl: TIDVariable;
begin
  Decl := EContext.SContext.Proc.GetTMPVar(DataType);
  Result := TIDExpression.Create(Decl);
end;

{ TIDSysCompileFunction }

class function TIDSysCompileFunction.GetFunctionID: TBuiltInFunctionID;
begin
  Result := bf_sysctfunction;
end;

{ TIDOpImplicitStringToPChar }

function TIDOpImplicitStringToPChar.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  Result := Dst;
end;

function TIDOpImplicitStringToPChar.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
begin
  Result := Src;
end;

end.

