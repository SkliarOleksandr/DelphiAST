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
  TASTContext = record
    Scope: TScope;
    Proc: TIDProcedure;
    function AddExpr: TASTExpression;
  end;

  TDelphiASTUnit = class(TNPUnit)
  private
    function parser_NextToken(var ACtx: TASTContext): TTokenID; overload;

    function ParseExitStatementAST(var ACtx: TASTContext): TTokenID;
    function ParseExpressionAST(var ACtx: TASTContext; var EContext: TEContext): TTokenID;
  public
    function Compile(RunPostCompile: Boolean = True): TCompilerResult; override;
  end;

implementation

uses NPCompiler.Operators;

{ TDelphiASTUnit }

function TDelphiASTUnit.parser_NextToken(var ACtx: TASTContext): TTokenID;
begin
  Result := TTokenID(Parser.NextTokenID);
  if Result >= token_cond_define then
    Result := ParseCondStatements(ACtx.Scope, Result);
end;

function TDelphiASTUnit.Compile(RunPostCompile: Boolean): TCompilerResult;
var
  Token: TTokenID;
  Scope: TScope;
  Platform: TIDPlatform;
  ASTC: TASTContext;
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

function TDelphiASTUnit.ParseExitStatementAST(var ACtx: TASTContext): TTokenID;
var
  ExitExpr: TASTExpression;
  ASTResult: TASTKWExit;
  EContext: TEContext;
begin
  ASTResult := TASTKWExit.Create;
  Result := parser_NextToken(ACtx);
  if Result = token_openround then
  begin
    if not Assigned(ACtx.Proc.ResultType) then
      AbortWork(sReturnValueNotAllowedForProc, parser_Position);

    InitEContext(EContext, nil, ExprNested);
    Result := ParseExpressionAST(ACtx, EContext);
    parser_MatchToken(Result, token_closeround);
    Result := parser_NextToken(ACtx);
  end;
end;

function TDelphiASTUnit.ParseExpressionAST(var ACtx: TASTContext; var EContext: TEContext): TTokenID;
var
  ID: TIdentifier;
  Status: TEContext.TRPNStatus;
  ASTE: TASTExpression;
  Expr: TIDExpression;
  RoundCount: Integer;
begin
  Status := rprOk;
  RoundCount := 0;
  ASTE := ACtx.AddExpr();
  Result := parser_CurTokenID;
  while True do begin
    case Result of
      token_eof: Break;// ERROR_END_OF_FILE;
      token_openround: begin
        Inc(RoundCount);
        EContext.RPNPushOpenRaund;
        ASTE.AddSubItem(TASTExprOpenRound);
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
      end;
      token_minus: begin
        if Status = rpOperand then
          Status := EContext.RPNPushOperator(opSubtract)
        else
          Status := EContext.RPNPushOperator(opNegative);
      end;
      token_equal: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opEqual);
      end;
      token_var: begin
        //Result := ParseInplaceVarDecl(Scope, Expr);
        //SContext.IL.AddVariable(Expr.AsVariable);
        //EContext.RPNPushExpression(Expr);
        Status := rpOperand;
        continue;
      end;
      token_lambda: begin
        //Result := ParseLambdaExpression(Scope, EContext);
        Status := rpOperand;
        continue;
      end;
//      token_bindf: begin
//        Result := ParseBindFunctionExpression(Scope, EContext);
//        Status := rpOperand;
//        continue;
//      end;
      token_notequal: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opNotEqual);
      end;
      token_less: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opLess);
      end;
      token_lessorequal: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opLessOrEqual);
      end;
      token_above: begin
        if EContext.EPosition = ExprNestedGeneric then
          Break;
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opGreater);
      end;
      token_aboveorequal: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opGreaterOrEqual);
      end;
      token_asterisk: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opMultiply);
      end;
      token_in: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opIn);
      end;
      token_slash: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opDivide);
      end;
      token_div: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opIntDiv);
      end;
      token_mod: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opModDiv);
      end;
      token_period: begin
        CheckLeftOperand(Status);
        //Bool_AddExprNode(EContext, GetILLast(SContext), cGreaterOrEqual); // ???????????????? зачем?
        Status := EContext.RPNPushOperator(opPeriod);
      end;
      token_address: begin
        Status := EContext.RPNPushOperator(opAddr);
      end;
      token_plusplus: begin
        EContext.RPNPushOperator(opPostInc);
        Status := rpOperand;
      end;
      token_minusminus: begin
        EContext.RPNPushOperator(opPostDec);
        Status := rpOperand;
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
          //Result := ParseMember(Scope, Expr, EContext);
          // если результат = nil значит это был вызов функции и все
          // необходимые параметры погружены в стек, поэтому идем дальше
          if not Assigned(Expr) then
          begin
            Status := rpOperand;
            continue;
          end;

          //CheckPureExpression(SContext, Expr);

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
          Expr := CreateAnonymousConstant(ACtx.Scope, EContext, ID, parser_IdentifireType);
          Result := parser_NextToken(ACtx);
          Status := rpOperand;
        end;
        EContext.RPNPushExpression(Expr);
        Continue;
      end;
    else
      Break;
    end;
    Result := parser_NextToken(ACtx);
  end;

  if (EContext.EPosition <> ExprNested) and (Status <> rpOperand) and NeedRValue(EContext.RPNLastOp) then
    ERROR_EXPRESSION_EXPECTED;

  EContext.RPNFinish();
end;

{ TASTContext }

function TASTContext.AddExpr: TASTExpression;
begin

end;

end.
