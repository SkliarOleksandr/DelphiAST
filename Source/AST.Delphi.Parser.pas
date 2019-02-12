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

  {PTryContext = ^TTryContext;
  TTryContext = record
  type
    TExitType = (etCallFinally, etJumpToFinally);
    TExitListItem = record
      ExitType: TExitType;
    end;
    PExitListItem = ^TExitListItem;
    TExitList = array of TExitListItem;
    TTrySection = (SectionTry, SectionFinally, SectionExcept);
  var
    Parent: PTryContext;
    ExitList: TExitList;
    Section: TTrySection;
  end;}

  TASTDelphiProc = class(TIDProcedure)
  private
    fBody: TASTBlock;
  public
    property Body: TASTBlock read fBody;
  end;


  TASTSContext = record
  private
   function GetIsLoopBody: Boolean;
    function GetIsTryBlock: Boolean;
  public
    Proc: TASTDelphiProc;
    Body: TASTBlock;
    constructor Create(Proc: TASTDelphiProc); overload;
    constructor Create(Proc: TASTDelphiProc; Block: TASTBlock); overload;
    function AddASTItem(ItemClass: TASTItemClass): TASTItem; overload;
    property IsLoopBody: Boolean read GetIsLoopBody;
    property IsTryBlock: Boolean read GetIsTryBlock;
  end;


  TASTDelphiUnit = class(TNPUnit)
  protected
    function GetModuleName: string; override;
    function GetFirstFunc: TASTDeclaration; override;
    function GetFirstVar: TASTDeclaration; override;
    function GetFirstType: TASTDeclaration; override;
    function GetFirstConst: TASTDeclaration; override;
  public
    function ParseStatements(Scope: TScope; var SContext: TASTSContext; IsBlock: Boolean): TTokenID; overload;
    function ParseExitStatement(Scope: TScope; var SContext: TASTSContext): TTokenID; overload;
    function ParseExpression(Scope: TScope; var SContext: TASTSContext; var EContext: TEContext; out ASTE: TASTExpression): TTokenID; overload;
    function ParseProcedure(Scope: TScope; ProcType: TProcType; Struct: TIDStructure = nil; Platform: TIDPlatform = nil): TTokenID; override;
    function ParseProcBody(Proc: TIDProcedure; Platform: TIDPlatform): TTokenID; override;
    function ParseIfThenStatement(Scope: TScope; var SContext: TASTSContext): TTokenID;
    function ParseWhileStatement(Scope: TScope; var SContext: TASTSContext): TTokenID;
    function ParseRepeatStatement(Scope: TScope; var SContext: TASTSContext): TTokenID;
    function ParseWithStatement(Scope: TScope; var SContext: TASTSContext): TTokenID;
    function ParseMember(Scope: TScope; out Expression: TIDExpression; var EContext: TEContext; var SContext: TASTSContext): TTokenID;
    function ParseForStatement(Scope: TScope; var SContext: TASTSContext): TTokenID;
    function ParseForInStatement(Scope: TScope; var SContext: TASTSContext; LoopVar: TIDExpression): TTokenID;
    function ParseCaseStatement(Scope: TScope; var SContext: TASTSContext): TTokenID;
    function ParseBreakStatement(Scope: TScope; var SContext: TASTSContext): TTokenID;
    function ParseContinueStatement(Scope: TScope; var SContext: TASTSContext): TTokenID;
    function ParseImmVarStatement(Scope: TScope; var SContext: TASTSContext): TTokenID;
    function ParseTrySection(Scope: TScope; var SContext: TASTSContext): TTokenID;
    function ParseRaiseStatement(Scope: TScope; var SContext: TASTSContext): TTokenID;

    procedure InitEContext(var EContext: TEContext; EPosition: TExpessionPosition); inline;
    procedure Process_operator_Assign(var EContext: TEContext); override;
    function Compile(RunPostCompile: Boolean = True): TCompilerResult; override;
    function CompileIntfOnly: TCompilerResult; override;
    procedure CheckIncompletedProcs(ProcSpace: PProcSpace); override;
  end;

implementation

uses NPCompiler.Operators, NPCompiler.DataTypes, SystemUnit;

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
      token_while: Result := ParseWhileStatement(Scope, SContext);
      {REPEAT}
      token_repeat: Result := ParseRepeatStatement(Scope, SContext);
      {WITH}
      token_with: Result := ParseWithStatement(Scope, SContext);
      {FOR}
      token_for: Result := ParseForStatement(Scope, SContext);
      {CASE}
      token_case: Result := ParseCaseStatement(Scope, SContext);
      {ASM}
      (*token_asm: begin
        ParseAsmSpecifier(Platform);
        ParseASMStatement(Scope, Platform, SContext);
        Result := parser_NextToken(Scope);
      end;
      {INHERITED}
      token_inherited: begin
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
      {BREAK, CONTINUE}
      token_break: Result := ParseBreakStatement(Scope, SContext);
      token_continue: Result := ParseContinueStatement(Scope, SContext);
      token_semicolon:;


      {VAR}
      token_var: Result := ParseImmVarStatement(Scope, SContext);

      (*  token_address: begin
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
            parser_NextToken(Scope);
            Result := ParseExpression(Scope, SContext, REContext, ASTESrc);
            if Assigned(REContext.LastBoolNode) then
              Bool_CompleteImmediateExpression(REContext, REContext.Result);

            EContext.RPNPushExpression(REContext.Result);
            EContext.RPNPushOperator(opAssignment);
            EContext.RPNFinish();

            var KW := SContext.AddASTItem(TASTKWAssign) as TASTKWAssign;
            KW.Dst := ASTEDst;
            KW.Src := ASTESrc;

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

function TASTDelphiUnit.ParseTrySection(Scope: TScope; var SContext: TASTSContext): TTokenID;
var
  KW: TASTKWTryBlock;
  NewContext: TASTSContext;
  ExceptItem: TASTExpBlockItem;
begin
  KW := SContext.AddASTItem(TASTKWTryBlock) as TASTKWTryBlock;
  NewContext := TASTSContext.Create(SContext.Proc, KW.Body);
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
        NewContext := TASTSContext.Create(SContext.Proc, ExceptItem.Body);
      end;
      Result := ParseStatements(Scope, NewContext, True);
      parser_MatchToken(Result, token_end);
      Result := parser_NextToken(Scope);
    end;
    {parse FINALLY section}
    token_finally: begin
      parser_NextToken(Scope);
      KW.FinallyBody := TASTBlock.Create(KW);
      NewContext := TASTSContext.Create(SContext.Proc, KW.FinallyBody);
      Result := ParseStatements(Scope, NewContext, True);
      parser_MatchToken(Result, token_end);
      Result := parser_NextToken(Scope);
    end;
  else
    AbortWork(sExceptOrFinallySectionWasMissed, parser_Position);
  end;
end;

function TASTDelphiUnit.ParseWhileStatement(Scope: TScope; var SContext: TASTSContext): TTokenID;
var
  Expression: TIDExpression;
  EContext: TEContext;
  BodySContext: TASTSContext;
  ASTExpr: TASTExpression;
  KW: TASTKWWhile;
begin
  KW := SContext.AddASTItem(TASTKWWhile) as TASTKWWhile;

  // loop expression
  InitEContext(EContext, ExprRValue);
  parser_NextToken(Scope);
  Result := ParseExpression(Scope, SContext, EContext, ASTExpr);
  KW.Expression := ASTExpr;
  Expression := EContext.Result;
  CheckEmptyExpression(Expression);
  CheckBooleanExpression(Expression);

  BodySContext := TASTSContext.Create(SContext.Proc, KW.Body);

  // loop body
  parser_MatchToken(Result, token_do);
  Result := Parser_NextToken(Scope);
  if Result <> token_semicolon then
    Result := ParseStatements(Scope, BodySContext, False);
end;

function TASTDelphiUnit.ParseWithStatement(Scope: TScope; var SContext: TASTSContext): TTokenID;
var
  Decl: TIDDeclaration;
  EContext: TEContext;
  Expression, Expr: TIDExpression;
  WNextScope: TWithScope;
  WPrevScope: TScope;
  BodySContext: TASTSContext;
  ASTExpr: TASTExpression;
  KW: TASTKWWith;
begin
  WPrevScope := Scope;
  WNextScope := nil;
  KW := SContext.AddASTItem(TASTKWWith) as TASTKWWith;
  BodySContext := TASTSContext.Create(SContext.Proc, KW.Body);
  while True do begin
    Result := parser_NextToken(Scope);
    InitEContext(EContext, ExprRValue);
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

procedure TASTDelphiUnit.InitEContext(var EContext: TEContext; EPosition: TExpessionPosition);
begin
  EContext.Initialize(Process_operators);
  EContext.SContext := nil;
  EContext.EPosition := EPosition;
end;

function TASTDelphiUnit.ParseBreakStatement(Scope: TScope; var SContext: TASTSContext): TTokenID;
var
  //TryBlock: PTryContext;
  KW: TASTKWBreak;
begin
  if not SContext.IsLoopBody then
    AbortWork(sBreakOrContinueAreAllowedOnlyInALoops, parser_Position);

  SContext.AddASTItem(TASTKWBreak);

  Result := parser_NextToken(Scope);
  {проверка на выход из try... секции}
  {TryBlock := SContext.TryBlock;
  while Assigned(TryBlock) do begin
    if TryBlock.Section = SectionFinally then
      AbortWork(sBreakContinueExitAreNotAllowedInFinallyClause, parser_PrevPosition);
    // если добрались до внешней (к циклу) try... секци, выходим
    if TryBlock = LContext.TryContext then
      break;
    TryBlock.AddExit(etCallFinally, Instruction);
    TryBlock := TryBlock.Parent;
  end;}
end;

function TASTDelphiUnit.ParseContinueStatement(Scope: TScope; var SContext: TASTSContext): TTokenID;
var
//  TryBlock: PTryContext;
  KW: TASTKWBreak;
begin
  if not SContext.IsLoopBody then
    AbortWork(sBreakOrContinueAreAllowedOnlyInALoops, parser_Position);

  SContext.AddASTItem(TASTKWContinue);

  Result := parser_NextToken(Scope);
  {проверка на выход из try... секции}
  {TryBlock := SContext.TryBlock;
  while Assigned(TryBlock) do begin
    if TryBlock.Section = SectionFinally then
      AbortWork(sBreakContinueExitAreNotAllowedInFinallyClause, parser_PrevPosition);
    // если добрались до внешней (к циклу) try... секци, выходим
    if TryBlock = LContext.TryContext then
      break;
    TryBlock.AddExit(etCallFinally, Instruction);
    TryBlock := TryBlock.Parent;
  end;}
end;


function TASTDelphiUnit.ParseCaseStatement(Scope: TScope; var SContext: TASTSContext): TTokenID;
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
  MISContext: TASTSContext;
  NeedWriteIL,
  NeedCMPCode: Boolean;
  Implicit: TIDDeclaration;
  ASTExpr: TASTExpression;
  KW: TASTKWCase;
  CaseItem: TASTExpBlockItem;
begin
  KW := SContext.AddASTItem(TASTKWCase) as TASTKWCase;

  // NeedCMPCode := False;
  // CASE выражение
  InitEContext(EContext, ExprRValue);
  parser_NextToken(Scope);
  Result := ParseExpression(Scope, SContext, EContext, ASTExpr);
  KW.Expression := ASTExpr;
  SExpression := EContext.RPNPopExpression();
  CheckEmptyExpression(SExpression);
  if Assigned(EContext.LastBoolNode) then
    Bool_CompleteImmediateExpression(EContext, SExpression);
  SEConst := SExpression.IsConstant;
  parser_MatchToken(Result, token_of);
  ItemsCount := 0;
  TotalMICount := 0;
  NeedWriteIL := True;
  ElsePresent := False;
  Result := parser_NextToken(Scope);

  while Result <> token_end do
  begin
    InitEContext(EContext, ExprRValue);
    if Result <> token_else then
    begin
      while True do begin
        //CFBBegin(SContext, CFB_CASE_ENTRY);
        Result := ParseExpression(Scope, SContext, EContext, ASTExpr);

        CaseItem := KW.AddItem(ASTExpr);
        MISContext := TASTSContext.Create(SContext.Proc, CaseItem.Body);

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
            if DExpression.DataTypeID <> dtRange then
            begin
              
            end else
              Process_operator_In(EContext, SExpression, DExpression);
            Inc(ItemsCount);
          end;
        end;
        CheckUniqueMIExpression(DExpression);

        if Assigned(EContext.LastBoolNode) and Assigned(EContext.LastBoolNode.PrevNode) then
          Bool_AddExprNode(EContext, ntOr);

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
      MISContext := TASTSContext.Create(SContext.Proc, KW.ElseBody);
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

function TASTDelphiUnit.ParseExitStatement(Scope: TScope; var SContext: TASTSContext): TTokenID;
var
  ExitExpr: TASTExpression;
  EContext: TEContext;
  KW: TASTKWExit;
begin
  KW := SContext.AddASTItem(TASTKWExit) as TASTKWExit;

  Result := parser_NextToken(Scope);
  if Result = token_openround then
  begin
    if not Assigned(SContext.Proc.ResultType) then
      AbortWork(sReturnValueNotAllowedForProc, parser_Position);

    parser_NextToken(Scope);
    InitEContext(EContext, ExprNested);
    Result := ParseExpression(Scope, SContext, EContext, ExitExpr);
    KW.Expression := ExitExpr;
    parser_MatchToken(Result, token_closeround);
    Result := parser_NextToken(Scope);
  end;

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
  ASTE := TASTExpression.Create(nil);
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
          Expr := nil;
          Result := ParseMember(Scope, Expr, EContext, SContext);
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

function TASTDelphiUnit.ParseForInStatement(Scope: TScope; var SContext: TASTSContext; LoopVar: TIDExpression): TTokenID;
begin

end;


type
  TILCondition = (cNone, cEqual, cNotEqual, cGreater, cGreaterOrEqual, cLess, cLessOrEqual, cZero, cNonZero);

function TASTDelphiUnit.ParseForStatement(Scope: TScope; var SContext: TASTSContext): TTokenID;
var
  EContext: TEContext;
  BodySContext: TASTSContext;
  ID: TIdentifier;
  LoopVar: TIDDeclaration;
  LExpr, StartExpr, StopExpr: TIDExpression;
  NewScope: TScope;
  KW: TASTKWFor;
  JMPCondition: TILCondition;
  ASTExpr: TASTExpression;
  WriteIL: Boolean;
begin
  KW := SContext.AddASTItem(TASTKWFor) as TASTKWFor;
  BodySContext := TASTSContext.Create(SContext.Proc, KW.Body);

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

  InitEContext(EContext, ExprRValue);
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

  parser_MatchToken(Result, token_assign);

  if LoopVar.DataType = nil then
    LoopVar.DataType := SYSUnit._Int32
  else
  if (LoopVar.ItemType <> itVar) or not (LoopVar.DataTypeID in [dtInt32, dtUInt64]) then
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
  InitEContext(EContext, ExprRValue);
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

function TASTDelphiUnit.ParseIfThenStatement(Scope: TScope; var SContext: TASTSContext): TTokenID;
var
  Expression: TIDExpression;
  EContext: TEContext;
  ThenSContext: TASTSContext;
  ElseSContext: TASTSContext;
  NewScope: TScope;
  KW: TASTKWIF;
  CondExpr: TASTExpression;
begin
  KW := SContext.AddASTItem(TASTKWIF) as TASTKWIF;
  InitEContext(EContext, ExprRValue);
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

    ThenSContext := TASTSContext.Create(SContext.Proc, KW.ThenBody);

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

    KW.ElseBody := TASTBlock.Create(KW);
    ElseSContext := TASTSContext.Create(SContext.Proc, KW.ElseBody);
    Result := ParseStatements(NewScope, ElseSContext, False);
  end;
end;

function TASTDelphiUnit.ParseImmVarStatement(Scope: TScope; var SContext: TASTSContext): TTokenID;
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

  KW := SContext.AddASTItem(TASTKWInlineVarDecl) as TASTKWInlineVarDecl;

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

    InitEContext(EContext, ExprRValue);

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

    Result := parser_NextToken(Scope);
    Result := ParseExpression(Scope, EContext, Result);

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
        TASTDelphiProc(Proc).fBody := TASTBlock.Create(Proc);
        //SContext.Initialize;
        //SContext.IL := TIL(Proc.IL);
        SContext := TASTSContext.Create(TASTDelphiProc(Proc));
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

function TASTDelphiUnit.ParseRaiseStatement(Scope: TScope; var SContext: TASTSContext): TTokenID;
var
  EExcept: TIDExpression;
  EContext: TEContext;
  ASTExpr: TASTExpression;
  KW: TASTKWRaise;
begin
  parser_NextToken(Scope);
  InitEContext(EContext, ExprRValue);
  Result := ParseExpression(Scope, SContext, EContext, ASTExpr);
  EExcept := EContext.Result;
  if Assigned(EExcept) then
    CheckClassExpression(EExcept);
  KW := SContext.AddASTItem(TASTKWRaise) as TASTKWRaise;
  KW.Expression := ASTExpr;
end;

function TASTDelphiUnit.ParseRepeatStatement(Scope: TScope; var SContext: TASTSContext): TTokenID;
var
  Expression: TIDExpression;
  EContext: TEContext;
  BodySContext: TASTSContext;
  KW: TASTKWRepeat;
  ASTExpr: TASTExpression;
begin
  KW := SContext.AddASTItem(TASTKWRepeat) as TASTKWRepeat;

  BodySContext := TASTSContext.Create(SContext.Proc, KW.Body);

  parser_NextToken(Scope);
  // тело цикла
  Result := ParseStatements(Scope, BodySContext, True);
  parser_MatchToken(Result, token_until);

  // выражение цикла
  InitEContext(EContext, ExprRValue);
  parser_NextToken(Scope);
  Result := ParseExpression(Scope, SContext, EContext, ASTExpr);
  KW.Expression := ASTExpr;
  Expression := EContext.Result;
  CheckEmptyExpression(Expression);
  CheckBooleanExpression(Expression);
end;

function TASTDelphiUnit.ParseMember(Scope: TScope; out Expression: TIDExpression; var EContext: TEContext; var SContext: TASTSContext): TTokenID;
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
//  SContext: PSContext;
begin
  WasProperty := False;
  PMContext.Init;
  PMContext.ItemScope := Scope;
  StrictSearch := False;
  //SContext := EContext.SContext;
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
          Result := ParseGenericsArgs(Scope, nil, GenericArgs);
          SetProcGenericArgs(TIDCallExpression(Expression), GenericArgs);
        end;
        // если есть открытая скобка, - значит вызов
        if Result = token_openround then
        begin
          Result := ParseEntryCall(Scope, TIDCallExpression(Expression), EContext);
          // если это метод, подставляем self из пула
          if PMContext.Count > 0 then
          begin
            if PMContext.Count > 1 then
              Expr := ProcessMemberExpression({SContext}nil, WasProperty, PMContext)
            else
              Expr := PMContext.Last;

            if (Expr.Declaration is TIDField) and (PMContext.Count = 1) then
            begin
              NExpr := GetTMPRefExpr({SContext}nil, Expr.DataType);
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
        if Decl.ClassType = TIDMacroArgument then
          Result := ParseMacroArg(Scope, PMContext.ID, TIDMacroArgument(Decl), Expression, EContext)
        else begin
          // если есть открытая скобка, - значит вызов
          if Result = token_openround then
          begin
            if Decl.DataTypeID <> dtProcType then
              ERROR_PROC_OR_PROCVAR_REQUIRED(PMContext.ID);

            Expression := TIDCallExpression.Create(Decl, PMContext.ID.TextPosition);
            Result := ParseEntryCall(Scope, TIDCallExpression(Expression), EContext);
            Expression := nil;
            Break;
          end;
          Expression := TIDExpression.Create(Decl, PMContext.ID.TextPosition);
        end;
        PMContext.DataType := Decl.DataType;
      end;
      itConst, itUnit, itNameSpace: begin
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
           Result := ParseExplicitCast(Scope, {SContext}nil, Expression);

        PMContext.DataType := Decl.DataType;
      end;
    else
      ERROR_FEATURE_NOT_SUPPORTED;
    end;

    PMContext.Add(Expression);

    {array type}
    if Result = token_openblock then begin
      Result := ParseArrayMember(PMContext, Scope, Decl, PMContext.DataType, EContext);
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

      Result := ParseEntryCall(Scope, CallExpr, EContext);
      Expression := nil;
      Break;
    end;

    {struct/class/interafce/enum/unit/namespace}
    if (Result = token_dot) then
    begin
      if Decl.ItemType in [itNameSpace, itUnit] then begin
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
      parser_NextToken(Scope);
      StrictSearch := True;
      continue;
    end;

    if PMContext.Count > 1 then
    begin
      Expression := TIDMultiExpression.Create(PMContext.Items, PMContext.ID.TextPosition);
      TIDMultiExpression(Expression).EffectiveDataType := PMContext.DataType;
    end;
    {else
    if (Decl.ClassType = TIDField) and Assigned(EContext.SContext) then
    begin
      Expr := GetTMPRefExpr(SContext, Decl.DataType);
      Expr.TextPosition := PMContext.ID.TextPosition;
      ILWrite(SContext, TIL.IL_GetPtr(Expr, nil, Expression));
      Expression := Expr;
    end;}
    break;
  end;
end;

{ TASTContext }

constructor TASTSContext.Create(Proc: TASTDelphiProc);
begin
  Self.Proc := Proc;
  Self.Body := Proc.Body;
end;

function TASTSContext.AddASTItem(ItemClass: TASTItemClass): TASTItem;
begin
  Result := ItemClass.Create(Body);
  Body.AddChild(Result);
end;

constructor TASTSContext.Create(Proc: TASTDelphiProc; Block: TASTBlock);
begin
  Self.Proc := Proc;
  Self.Body := Block;
end;

function TASTSContext.GetIsLoopBody: Boolean;
begin
  Result := Assigned(Body) and Body.IsLoopBody;
end;

function TASTSContext.GetIsTryBlock: Boolean;
begin
  Result := Assigned(Body) and Body.IsTryBlock;
end;

end.
