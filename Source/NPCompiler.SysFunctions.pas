unit NPCompiler.SysFunctions;

interface

uses OPCompiler, NPCompiler.Classes, NPCompiler.DataTypes, NPCompiler.Errors, SystemUnit, IL.Instructions,
     NPCompiler.Contexts, NPCompiler.ExpressionContext;

type
  {function: typeid}
  TSF_typeid = class(TIDSysRuntimeFunction)
  protected
    function Process(var EContext: TEContext): TIDExpression; override;
  end;

  {function: now}
  TSF_now = class(TIDSysRuntimeFunction)
  protected
    function Process(var EContext: TEContext): TIDExpression; override;
  end;

  {function: RunError}
  TSF_RunError = class(TIDSysRuntimeFunction)
  protected
    function Process(var EContext: TEContext): TIDExpression; override;
  end;

  {function: StaticAssert}
  TSF_StaticAssert = class(TIDSysCompileFunction)
  protected
    function Process(const Ctx: TSysFunctionContext): TIDExpression; override;
  end;

  {function: Defined}
  TSCTF_Defined = class(TIDSysCompileFunction)
  protected
    function Process(const Ctx: TSysFunctionContext): TIDExpression; override;
  end;

  {function: Declared}
  TSCTF_Declared = class(TIDSysCompileFunction)
  protected
    function Process(const Ctx: TSysFunctionContext): TIDExpression; override;
  end;

  {function: AtomicExchange}
  TSF_AtomicExchange = class(TIDSysRuntimeFunction)
  protected
    function Process(var EContext: TEContext): TIDExpression; override;
  end;

implementation

{ TSF_typeid }


function TSF_typeid.Process(var EContext: TEContext): TIDExpression;
var
  UN: TNPUnit;
  Expr: TIDExpression;
  Decl: TIDDeclaration;
  TypeID: TDataTypeID;
  RDecl: TIDIntConstant;
begin
  UN := GetUnit(EContext);
  // ÷èòàåì àðãóìåíò
  Expr := EContext.RPNPopExpression();
  Decl := Expr.Declaration;

  case Decl.ItemType of
    itVar, itConst: TypeID := Decl.DataTypeID;
    itType: TypeID := TIDType(Decl).DataTypeID;
  else
    AbortWork(sVariableOrTypeRequired, Expr.TextPosition);
    Exit(nil);
  end;
  // ðåçóëüòàò
  RDecl := TIDIntConstant.CreateAnonymous(UN.ImplSection, SYSUnit._TypeID, Ord(TypeID));
  Result := TIDExpression.Create(RDecl, Expr.TextPosition);
end;

{ TSF_now }

function TSF_now.Process(var EContext: TEContext): TIDExpression;
var
  ILCode: TILInstruction;
begin
  Result := EContext.SContext.GetTMPVarExpr(SYSUnit._DateTime, GetUnit(EContext).parser_Position);
  ILCode := TIL.IL_Now(Result);
  EContext.SContext.ILWrite(ILCode);
end;

{ TSF_StaticAssert }

function TSF_StaticAssert.Process(const Ctx: TSysFunctionContext): TIDExpression;
var
  TextExpr, Expr: TIDExpression;
begin
  // читаем второй аргумент
  TextExpr := Ctx.EContext.RPNPopExpression();
  Expr := Ctx.EContext.RPNPopExpression();
  Ctx.UN.CheckConstExpression(Expr);
  if not Expr.AsBoolConst.Value then
  begin
    if TextExpr.AsStrConst.Value = '' then
      AbortWork('Static assertion. Expression: "' + Ctx.ParamsStr + '" is false.', Expr.TextPosition)
    else
      AbortWork('Static assertion. Message: "' + TextExpr.AsStrConst.Value + '"', Expr.TextPosition);
  end;
  Result := nil;
end;

{ TSCTF_Defined }

function TSCTF_Defined.Process(const Ctx: TSysFunctionContext): TIDExpression;
var
  Expr: TIDExpression;
begin
  Expr := Ctx.EContext.RPNPopExpression();

  if Expr.DataTypeID <> dtString then
    AbortWork('DEFINE String expected', Expr.TextPosition);

  if Ctx.UN.Defined(Expr.AsStrConst.Value) then
    Result := SYSUnit._TrueExpression
  else
    Result := SYSUnit._FalseExpression;
end;

{ TSF_AtomicExchange }

function TSF_AtomicExchange.Process(var EContext: TEContext): TIDExpression;
var
  Left, Right: TIDExpression;
begin
  Right := EContext.RPNPopExpression();
  Left := EContext.RPNPopExpression();
  // todo:
  Result := Left;
end;

{ TSCTF_Declared }

function TSCTF_Declared.Process(const Ctx: TSysFunctionContext): TIDExpression;
var
  Expr: TIDExpression;
begin
  Expr := Ctx.EContext.RPNPopExpression();

  if Expr.DataTypeID <> dtString then
    AbortWork('DEFINE String expected', Expr.TextPosition);

  if Ctx.Scope.FindIDRecurcive(Expr.AsStrConst.Value) <> nil then
    Result := SYSUnit._TrueExpression
  else
    Result := SYSUnit._FalseExpression;

end;

{ TSF_RunError }

function TSF_RunError.Process(var EContext: TEContext): TIDExpression;
var
  ErrCode: TIDExpression;
begin
  ErrCode := EContext.RPNPopExpression();
  // todo:
  Result := nil;
end;

end.
