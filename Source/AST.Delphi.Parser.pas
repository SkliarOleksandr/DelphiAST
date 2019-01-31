unit AST.Delphi.Parser;

interface

uses System.SysUtils,
     OPCompiler,
     iDStringParser,
     AST.Classes,
     NPCompiler.Intf,
     OPCompiler.Parser,
     NPCompiler.Messages,
     NPCompiler.Errors,
     NPCompiler.ExpressionContext,
     NPCompiler.Classes;

type
  TASTDelphiProc = class(TIDProcedure)
  private
    fBody: TASTBody;
  public
    property Body: TASTBody read fBody;
  end;


  TASTSContext = record
    Proc: TASTDelphiProc;
    constructor Create(Proc: TASTDelphiProc);
    procedure AddASTItem(Item: TASTItem);
  end;


  TASTDelphiUnit = class(TNPUnit)
  public
    function ParseExitStatement(Scope: TScope; var SContext: TASTSContext): TTokenID; overload;
    function ParseExpression(Scope: TScope; var SContext: TASTSContext; var EContext: TEContext; out ASTE: TASTExpression): TTokenID; overload;
    function ParseProcedure(Scope: TScope; ProcType: TProcType; Struct: TIDStructure = nil; Platform: TIDPlatform = nil): TTokenID; override;
    function ParseProcBody(Proc: TIDProcedure; Platform: TIDPlatform): TTokenID; override;
    function ParseStatements(Scope: TScope; var SContext: TASTSContext; IsBlock: Boolean): TTokenID; overload;
    function ParseIfThenStatement(Scope: TScope; SContext: TASTSContext): TTokenID;
    procedure InitEContext(var EContext: TEContext; EPosition: TExpessionPosition); inline;
    procedure Process_operator_Assign(var EContext: TEContext); override;
    function Compile(RunPostCompile: Boolean = True): TCompilerResult; override;
    function CompileIntfOnly: TCompilerResult; override;
    procedure CheckIncompletedProcs(ProcSpace: PProcSpace); override;
  end;

implementation

uses NPCompiler.Operators, NPCompiler.DataTypes;

{ TDelphiASTUnit }

function TASTDelphiUnit.ParseStatements(Scope: TScope; var SContext: TASTSContext; IsBlock: Boolean): TTokenID;
var
  EContext, REContext: TEContext;
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
      (*token_while: Result := ParseWhileStatement(Scope, SContext);
      {REPEAT}
      token_repeat: Result := ParseRepeatStatement(Scope, SContext);
      {WITH}
      token_with: Result := ParseWithStatement(Scope, SContext);
      {USING}
      token_using: Result := ParseUsingStatement(Scope, SContext);
      {CASE}
      token_case: Result := ParseCaseStatement(Scope, SContext);
      {ASM}
      token_asm: begin
        ParseAsmSpecifier(Platform);
        ParseASMStatement(Scope, Platform, SContext);
        Result := parser_NextToken(Scope);
      end;
      {INHERITED}
      token_inherited: begin
        InitEContext(EContext, SContext, ExprLValue);
        Result := ParseInheritedStatement(Scope, EContext);
      end;
      {FOR}
      token_for: Result := ParseForStatement(Scope, SContext);
      {TRY}
      token_try: begin
        Result := ParseTrySection(Scope, SContext);
        if Result <> token_unknown then;
      end;
      {EXCEPT/FINALLY}
      token_except, token_finally: begin
        if not Assigned(SContext.TryBlock) then
          ERROR_TRY_KEYWORD_MISSED;
        Exit;
      end;
      {RAISE}
      token_raise: Result := ParseRaiseStatement(Scope, SContext);
      {BREAK, CONTINUE}
      token_break, token_continue: Result := ParseBCStatements(Scope, Result, SContext);
      token_semicolon:;
      token_unsafe: Result := ParseUnsafeStatement(Scope, SContext);

      {VAR}
      token_var: Result := ParseImmVarStatement(Scope, SContext);

      token_address: begin
        Result := parser_NextToken(Scope);
        Continue;
      end;     *)

      {IDENTIFIER}
      token_identifier: begin
        InitEContext(EContext, ExprLValue);
        var ASTEDst, ASTESrc: TASTExpression;
        while True do
        begin
          Result := ParseExpression(Scope, SContext, EContext, ASTEDst);
          if Result = token_assign then begin
            InitEContext(REContext, ExprRValue);
            Result := parser_NextToken(Scope);
            Result := ParseExpression(Scope, SContext, REContext, ASTESrc);
            if Assigned(REContext.LastBoolNode) then
              Bool_CompleteImmediateExpression(REContext, REContext.Result);

            EContext.RPNPushExpression(REContext.Result);
            EContext.RPNPushOperator(opAssignment);
            EContext.RPNFinish();

            AST := TASTKWAssign.Create(ASTEDst, ASTESrc);
            SContext.AddASTItem(AST);

          end else
          if Result = token_coma then begin
            parser_NextToken(Scope);
            Continue;
          end;
          CheckAndCallFuncImplicit(EContext);
          CheckUnusedExprResult(EContext);
          Break;
        end;
      end;
      token_initialization,
      token_finalization: Break;
      token_eof: Exit;
    else
      if IsBlock then
        ERROR_EXPECTED_KEYWORD_OR_ID;
    end;

//    if not Assigned(SContext.LContext) then
//      FLoopPool.Clear;

    if IsBlock then
    begin
      parser_MatchToken(Result, token_semicolon);
      Result := parser_NextToken(Scope);
    end else
      Exit;
  end;
end;

procedure TASTDelphiUnit.Process_operator_Assign(var EContext: TEContext);
begin

end;

procedure TASTDelphiUnit.CheckIncompletedProcs(ProcSpace: PProcSpace);
begin
//  inherited;

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
        //token_namespace: Token := ParseNameSpace(Scope);
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

procedure TASTDelphiUnit.InitEContext(var EContext: TEContext; EPosition: TExpessionPosition);
begin
  EContext.Initialize(Process_operators);
  EContext.SContext := nil;
  EContext.EPosition := EPosition;
end;

function TASTDelphiUnit.ParseExitStatement(Scope: TScope; var SContext: TASTSContext): TTokenID;
var
  ExitExpr: TASTExpression;
  EContext: TEContext;
  ASTItem: TASTKWExit;
begin
  ASTItem := TASTKWExit.Create;
  Result := parser_NextToken(Scope);
  if Result = token_openround then
  begin
    if not Assigned(SContext.Proc.ResultType) then
      AbortWork(sReturnValueNotAllowedForProc, parser_Position);

    parser_NextToken(Scope);
    InitEContext(EContext, ExprNested);
    Result := ParseExpression(Scope, SContext, EContext, ExitExpr);
    ASTItem.Expression := ExitExpr;
    parser_MatchToken(Result, token_closeround);
    Result := parser_NextToken(Scope);
  end;
  SContext.AddASTItem(ASTItem);
end;

function TASTDelphiUnit.ParseExpression(Scope: TScope; var SContext: TASTSContext; var EContext: TEContext; out ASTE: TASTExpression): TTokenID;
var
  ID: TIdentifier;
  Status: TEContext.TRPNStatus;
  Expr: TIDExpression;
  RoundCount: Integer;
begin
  Status := rprOk;
  RoundCount := 0;
  Result := parser_CurTokenID;
  ASTE := TASTExpression.Create;
  while True do begin
    case Result of
      token_eof: Break;// ERROR_END_OF_FILE;
      token_openround: begin
        Inc(RoundCount);
        EContext.RPNPushOpenRaund;
        ASTE.AddSubItem(TASTEIOpenRound);
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
        ASTE.AddSubItem(TASTEICloseRound);
        EContext.RPNPushCloseRaund();
        Status := rpOperand;
      end;
      token_openblock: begin
        //ParseVector(Scope, EContext);
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
        ASTE.AddSubItem(TASTEIPlus);
      end;
      token_minus: begin
        if Status = rpOperand then
          Status := EContext.RPNPushOperator(opSubtract)
        else
          Status := EContext.RPNPushOperator(opNegative);
        ASTE.AddSubItem(TASTEIMinus);
      end;
      token_equal: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opEqual);
        ASTE.AddSubItem(TASTEIEqual);
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
        ASTE.AddSubItem(TASTEINotEqual);
      end;
      token_less: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opLess);
        ASTE.AddSubItem(TASTEILess);
      end;
      token_lessorequal: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opLessOrEqual);
        ASTE.AddSubItem(TASTEILessEqual);
      end;
      token_above: begin
        if EContext.EPosition = ExprNestedGeneric then
          Break;
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opGreater);
        ASTE.AddSubItem(TASTEIGrater);
      end;
      token_aboveorequal: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opGreaterOrEqual);
        ASTE.AddSubItem(TASTEIGraterEqual);
      end;
      token_asterisk: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opMultiply);
        ASTE.AddSubItem(TASTEIMul);
      end;
      token_in: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opIn);
      end;
      token_slash: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opDivide);
        ASTE.AddSubItem(TASTEIDiv);
      end;
      token_div: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opIntDiv);
        ASTE.AddSubItem(TASTEIIntDiv);
      end;
      token_mod: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opModDiv);
        ASTE.AddSubItem(TASTEIMod);
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
      token_not: Status := EContext.RPNPushOperator(opNot);
      token_shl: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opShiftLeft);
      end;
      token_shr: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opShiftRight);
      end;
      token_is: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opIs);
      end;
      token_as: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opAs);
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
          Result := ParseMember(Scope, Expr, EContext);
          // если результат = nil значит это был вызов функции и все
          // необходимые параметры погружены в стек, поэтому идем дальше
          if not Assigned(Expr) then
          begin
            Status := rpOperand;
            continue;
          end;

          //CheckPureExpression(SContext, Expr);
          ASTE.AddDeclItem(Expr.Declaration, Expr.TextPosition);

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
                  //Result := ParseExplicitCast(Scope, SContext, Expr);
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

function TASTDelphiUnit.ParseIfThenStatement(Scope: TScope; SContext: TASTSContext): TTokenID;
var
  Expression: TIDExpression;
  ToElseJump,
  EContext: TEContext;
  ThenSContext: TASTSContext;
  ElseSContext: TASTSContext;
  NewScope: TScope;
  IFCondition: (condConstNone, condConstTrue, condConstFalse);
  ASTIF: TASTKWIF;
  CondExpr: TASTExpression;
begin
  ASTIF := TASTKWIF.Create();
  InitEContext(EContext, ExprRValue);
//  CFBBegin(SContext, CFB_IF);
  parser_NextToken(Scope);
  Result := ParseExpression(Scope, SContext, EContext, CondExpr);
  ASTIF.Expression := CondExpr;
  Expression := EContext.Result;
  CheckEmptyExpression(Expression);
  CheckBooleanExpression(Expression);
//  ToElseJump := nil;
//  ThenSContext.Assign(SContext);
//  ElseSContext.Assign(SContext);
  case Expression.ItemType of
    itVar: if not (Expression is TIDBoolResultExpression) then begin // Если if условие соотоит из одной boolean переменной
      {ILWrite(SContext, TIL.IL_Test(Expression, Expression));
      ToElseJump := TIL.IL_JmpNext(Expression.Line, cZero, nil);
      ILWrite(SContext, ToElseJump);}
      IFCondition := condConstNone;
    end else
      IFCondition := condConstNone;
    itConst: begin // Если if условие соотоит из константы
      if TIDBooleanConstant(Expression.Declaration).Value then
      begin
        //ElseSContext.WriteIL := False;
        IFCondition := condConstTrue;
      end else begin
        //ThenSContext.WriteIL := False;
        IFCondition := condConstFalse;
      end;
    end;
  else
    IFCondition := condConstNone;
  end;

  {then section}
  parser_MatchToken(Result, token_then);
  Result := parser_NextToken(Scope);
  if Result <> token_semicolon then
  begin
//    if IFCondition = condConstNone then
//      CFBBegin(SContext, CFB_IF_THEN);

    {оптимизация, не создаем лишний scope, если внутри он создастся всеравно}
    if Result <> token_begin then
      NewScope := TScope.Create(stLocal, Scope)
    else
      NewScope := Scope;

    Result := ParseStatements(NewScope, @ThenSContext, False);

//    if IFCondition = condConstNone then
//      CFBEnd(SContext, CFB_IF_THEN);
  end else begin
    {выходим если if ... then пустой (типа: if x > y then;)}
    //Bool_CompleteExpression(EContext.LastBoolNode, SContext.ILLast);
    Exit;
  end;
  { else section}
  if Result = token_else then begin
//    if IFCondition = condConstNone then
//      CFBBegin(SContext, CFB_IF_ELSE);
//    ToEndJump := TIL.IL_JmpNext(parser_Line - 1, cNone, nil); // безусловный прыжок на END секцию
    // если есть then и else секции
//    ThenSContext.WriteIL := ThenSContext.WriteIL and ElseSContext.WriteIL;
//    ILWrite(@ThenSContext, ToEndJump);
//    if Assigned(ToElseJump) then
//      ToElseJump.Destination := SContext.ILLast
//    else
//      Bool_CompleteExpression(EContext.LastBoolNode, SContext.ILLast);
    parser_NextToken(Scope);

    {оптимизация, не создаем лишний scope, если внутри он создастся всеравно}
    if Result <> token_begin then
      NewScope := TScope.Create(stLocal, Scope)
    else
      NewScope := Scope;

    Result := ParseStatements(NewScope, @ElseSContext, False);
//    ToEndJump.Destination := SContext.ILLast;
//    if IFCondition = condConstNone then
//      CFBEnd(SContext, CFB_IF_ELSE);
  end else
//    if Assigned(ToElseJump) then
//      ToElseJump.Destination := SContext.ILLast
//    else
//      Bool_CompleteExpression(EContext.LastBoolNode, SContext.ILLast);
//  CFBEnd(SContext, CFB_IF);
  SContext.AddASTItem(ASTIF);
end;

function TASTDelphiUnit.ParseProcBody(Proc: TIDProcedure; Platform: TIDPlatform): TTokenID;
var
  Scope: TScope;
  SContext: TASTSContext;
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
      token_const: Result := ParseConstSection(Scope);
      token_type: Result := ParseNamedTypeDecl(Scope);
      token_procedure: Result := ParseProcedure(Scope, ptProc);
      token_function: Result := ParseProcedure(Scope, ptFunc);
      token_identifier: ERROR_KEYWORD_EXPECTED;
      token_asm: begin
        // skip the asm...end block
        Result := parser_SkipBlock(token_end);
        Result := parser_NextToken(Scope);
        Exit;
      end;
      token_begin: begin
        Proc.FirstBodyLine := parser_Line;
        TASTDelphiProc(Proc).fBody := TASTBody.Create;
        //SContext.Initialize;
        //SContext.IL := TIL(Proc.IL);
        SContext.Proc := TASTDelphiProc(Proc);
        //CheckInitVariables(@SContext, nil, @Proc.VarSpace);
        if not Assigned(Platform) then
        begin
          parser_NextToken(Scope);
          Result := ParseStatements(Scope, SContext, True);
          parser_MatchToken(Result, token_end);
          Result := parser_NextToken(Scope);
        end else
          ParseASMStatement(Scope, Platform, @SContext);
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

function TASTDelphiUnit.ParseProcedure(Scope: TScope; ProcType: TProcType; Struct: TIDStructure; Platform: TIDPlatform): TTokenID;
type
  TFwdDeclState = (dsNew, dsDifferent, dsSame);
var
  ID: TIdentifier;
  Parameters: TProcScope;
  ResultType: TIDType;
  ResultParam: TIDVariable;
  VarSpace: TVarSpace;
  GenericsParams: TIDTypeList;
  Proc, ForwardDecl: TIDProcedure;
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
    ForwardDecl := TIDProcedure(Struct.Members.FindID(ID.Name));
    if not Assigned(ForwardDecl) and (Scope.ScopeClass = scImplementation) then
      ERROR_METHOD_NOT_DECLARED_IN_CLASS(ID, Struct);
  end else
    ForwardDecl := TIDProcedure(ForwardScope.FindID(ID.Name));

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
      ForwardDecl := ForwardDecl.NextOverload;
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
      Result := ParseProcBody(Proc, Platform);
      if Result <> token_semicolon then
        ERROR_SEMICOLON_EXPECTED;

      Result := parser_NextToken(Scope);
      Break;
    end;
  end;

  if (ProcType = ptDestructor) and (Struct.DataTypeID = dtClass) then
    CheckDestructorSignature(Proc);
end;

{ TASTContext }

procedure TASTSContext.AddASTItem(Item: TASTItem);
begin
  Proc.fBody.AddChild(Item);
end;

constructor TASTSContext.Create(Proc: TASTDelphiProc);
begin
  Self.Proc := Proc;
end;

end.
