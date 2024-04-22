unit AST.Delphi.SysFunctions;

interface

uses AST.Pascal.Parser,
     AST.Delphi.Classes,
     AST.Delphi.Declarations,
     AST.Delphi.DataTypes,
     AST.Delphi.System,
     AST.Parser.Errors,
     AST.Delphi.Parser,
     AST.Delphi.Contexts;

type

  {Assigned}
  TSF_Assigned = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Assert}
  TSF_Assert = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Now}
  TSF_Now = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {New}
  TSF_New = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Dispose}
  TSF_Dispose = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {RunError}
  TSF_RunError = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {ReturnAddress}
  TSF_ReturnAddress = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Defined}
  TSCTF_Defined = class(TIDSysCompileFunction)
  public
    function Process(const Ctx: TSysFunctionContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Declared}
  TSCTF_Declared = class(TIDSysCompileFunction)
  public
    function Process(const Ctx: TSysFunctionContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Default}
  TSCTF_Default = class(TIDSysCompileFunction)
  public
    function Process(const Ctx: TSysFunctionContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {_console}
  TSCTF_Console = class(TIDSysCompileFunction)
  public
    function Process(const Ctx: TSysFunctionContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {_scope}
  TSCTF_Scope = class(TIDSysCompileFunction)
  public
    function Process(const Ctx: TSysFunctionContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {_typename}
  TSCTF_TypeName = class(TIDSysCompileFunction)
  public
    function Process(const Ctx: TSysFunctionContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {IsManagedType}
  TSCTF_IsManagedType = class(TIDSysCompileFunction)
  public
    function Process(const Ctx: TSysFunctionContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {IsConstValue}
  TSCTF_IsConstValue = class(TIDSysCompileFunction)
  public
    function Process(const Ctx: TSysFunctionContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {TypeInfo}
  TSCTF_TypeInfo = class(TIDSysCompileFunction)
  public
    function Process(const Ctx: TSysFunctionContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {GetTypeKind}
  TSCTF_GetTypeKind = class(TIDSysCompileFunction)
  public
    function Process(const Ctx: TSysFunctionContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {HasWeakRef}
  TSCTF_HasWeakRef = class(TIDSysCompileFunction)
  public
    function Process(const Ctx: TSysFunctionContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Delete}
  TSF_Delete = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {AtomicExchange}
  TSF_AtomicExchange = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {AtomicCmpExchange}
  TSF_AtomicCmpExchange = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {AtomicCmpExchange128}
  TSF_AtomicCmpExchange128 = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {AtomicDecrement}
  TSF_AtomicDecrement = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {AtomicIncrement}
  TSF_AtomicIncrement = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {SizeOf}
  TCT_SizeOf = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Break}
  TCT_Break = class(TIDSysCompileFunction)
  public
    function Process(const Ctx: TSysFunctionContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Continue}
  TCT_Continue = class(TIDSysCompileFunction)
  public
    function Process(const Ctx: TSysFunctionContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Low}
  TSF_LoBound = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {High}
  TSF_HiBound = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Hi}
  TSF_HiByte = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Lo}
  TSF_LoByte = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Include}
  TSF_Include = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Inc}
  TSF_Inc = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Dec}
  TCT_Dec = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Length}
  TSF_Length = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {SetLength}
  TSF_SetLength = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {SetString}
  TSF_SetString = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Swap}
  TSF_Swap = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Ord}
  TSF_Ord = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Odd}
  TSF_Odd = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Chr}
  TSF_Chr = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {FillChar}
  TSF_FillChar = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Trunc}
  TSF_Trunc = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Val}
  TSF_Val = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Abs}
  TSF_Abs = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Str}
  TSF_Str = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Sqr}
  TSF_Sqr = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Round}
  TSF_Round = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Pred}
  TSF_Pred = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Succ}
  TSF_Succ = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {GetMem}
  TSF_GetMem = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {GetDir}
  TSF_GetDir = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {FreeMem}
  TSF_FreeMem = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Copy}
  TSF_Copy = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Exit}
  TSF_Exit = class(TIDSysCompileFunction)
  public
    function Process(const Ctx: TSysFunctionContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Exclude}
  TSF_Exclude = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Halt}
  TSF_Halt = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {ReallocMem}
  TSF_ReallocMem = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Close}
  TSF_Close = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Pi}
  TSF_Pi = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Pi}
  TSF_Insert = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {<dyn array type>.create(...)}
  TCT_DynArrayCreate = class(TIDSysCompileFunction)
  protected
    function GetParamsCount: Integer; override;
  public
    function Process(const Ctx: TSysFunctionContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {VarCast}
  TSF_VarCast = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {VarClear}
  TSF_VarClear = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {MulDivModInt64}
  TSF_MulDivInt64 = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {VarArgStart}
  TSF_VarArgStart = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {VarArgGetValue}
  TSF_VarArgGetValue = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {VarArgCopy}
  TSF_VarArgCopy = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

  {VarArgEnd}
  TSF_VarArgEnd = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; override;
  end;

implementation

uses
  AST.Lexer,
  AST.Classes,
  AST.Delphi.Errors;

{ TSF_Now }

function TSF_Now.Process(var EContext: TEContext): TIDExpression;
var
  ResVar: TIDVariable;
  UN: TASTDelphiUnit;
begin
  UN := GetUnit(EContext);
  ResVar := EContext.SContext.Proc.GetTMPVar(SYSUnit._DateTime);
  Result := TIDExpression.Create(ResVar, UN.Lexer_Position);
end;

class function TSF_Now.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Now', SYSUnit._DateTime);
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

class function TSCTF_Defined.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Defined', SYSUnit._Boolean);
  Result.AddParam('Expression', SYSUnit._Boolean, [VarConst]);
end;

{ TSF_AtomicExchange }

function TSF_AtomicExchange.Process(var EContext: TEContext): TIDExpression;
begin
  var AValue := EContext.RPNPopExpression();
  var ATarget := EContext.RPNPopExpression();

  Result := CreateTMPExpr(EContext, ATarget.DataType);
  // todo:
end;

class function TSF_AtomicExchange.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  var AVariativeType := TIDSysVariativeType.CreateAsSystem(Scope,
                        [SysUnit._Int32, SysUnit._NativeInt, SysUnit._Pointer]);
  Result := Self.Create(Scope, 'AtomicExchange', AVariativeType);
  Result.AddParam('Target', SYSUnit._Void, [VarInOut]);
  Result.AddParam('Value', AVariativeType);
end;

{ TSF_AtomicCmpExchange }

function TSF_AtomicCmpExchange.Process(var EContext: TEContext): TIDExpression;
begin
  var Arg1 := EContext.RPNPopExpression();
  var Arg2 := EContext.RPNPopExpression();
  var Arg3 := EContext.RPNPopExpression();

  case Arg1.DataTypeID of
    dtNativeInt: Result := CreateTMPExpr(EContext, SYSUnit._NativeInt);
    dtPointer: Result := CreateTMPExpr(EContext, SYSUnit._Pointer);
  else
    Result := CreateTMPExpr(EContext, SYSUnit._Int32);
  end;
end;

class function TSF_AtomicCmpExchange.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'AtomicCmpExchange', SYSUnit._NativeInt);
  Result.AddParam('Target', SYSUnit._Void, [VarInOut]);
  Result.AddParam('Comparand', SYSUnit._Void, [VarIn]);
  Result.AddParam('Succeeded', SYSUnit._Void, [VarOut]);
end;


{ TSF_AtomicCmpExchange128 }

class function TSF_AtomicCmpExchange128.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'AtomicCmpExchange128', SYSUnit._Boolean);
  Result.AddParam('Target', SYSUnit._Untyped, [VarInOut]);
  Result.AddParam('NewValueHigh', SYSUnit._Int64, [VarIn]);
  Result.AddParam('NewValueLow', SYSUnit._Int64, [VarIn]);
  Result.AddParam('Comparand', SYSUnit._Untyped, [VarInOut]);
end;

function TSF_AtomicCmpExchange128.Process(var EContext: TEContext): TIDExpression;
begin
  EContext.RPNPopExpression();
  EContext.RPNPopExpression();
  EContext.RPNPopExpression();
  EContext.RPNPopExpression();
  Result := SYSUnit._TrueExpression;
end;

{ TSCTF_Declared }

function TSCTF_Declared.Process(const Ctx: TSysFunctionContext): TIDExpression;
var
  Expr: TIDExpression;
begin
  Expr := Ctx.EContext.RPNPopExpression();

  if not Assigned(Expr) then
    AbortWork('DEFINE String expected', TTextPosition.Empty);

  // take the name of the declaration (could be any, const, type, etc) and find it in the scope...
  if Ctx.Scope.FindIDRecurcive(Expr.Declaration.Name) <> nil then
    Result := SYSUnit._TrueExpression
  else
    Result := SYSUnit._FalseExpression;
end;

class function TSCTF_Declared.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Declared', SYSUnit._Boolean);
  Result.AddParam('Expression', SYSUnit._Boolean, [VarConst]);
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

class function TSF_RunError.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'RunError', nil);
  Result.AddParam('ErrorCode', SYSUnit._Int32, []);
end;

{ TCT_SizeOf }

function TCT_SizeOf.Process(var EContext: TEContext): TIDExpression;
var
  Expr: TIDExpression;
  Decl: TIDDeclaration;
  DataType: TIDType;
begin
  // читаем аргумент
  Expr := EContext.RPNPopExpression();
  Decl := Expr.Declaration;
  if Decl.ItemType <> itType then
    DataType := Decl.DataType.ActualDataType
  else
    DataType := TIDType(Decl).ActualDataType;

  Result := IntConstExpression(EContext.SContext, DataType.DataSize);
  Result.TextPosition := Expr.TextPosition;
end;

class function TCT_SizeOf.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := TCT_SizeOf.Create(Scope, 'SizeOf', SYSUnit._Int32);
  Result.AddParam('S', SYSUnit._Void);
end;

{ TSF_LoBound }

function TSF_LoBound.Process(var EContext: TEContext): TIDExpression;
var
  Expr: TIDExpression;
  Decl: TIDDeclaration;
  DataType: TIDType;
  UN: TASTDelphiUnit;
begin
  // читаем аргумент
  Expr := EContext.RPNPopExpression();

  UN := GetUnit(EContext);
  UN.CheckEmptyExpression(Expr);

  Decl := Expr.Declaration;
  case Decl.ItemType of
    itType: DataType := Decl as TIDType;
    itVar, itConst: DataType := Decl.DataType;
  else
    UN.ERRORS.ORDINAL_TYPE_REQUIRED(Expr.TextPosition);
    Exit(nil);
  end;

  DataType := DataType.ActualDataType;

  if DataType.IsOrdinal then
  begin
    Decl := TIDIntConstant.CreateAsAnonymous(UN.IntfScope, DataType, (DataType as TIDOrdinal).LowBound);
  end else
  if DataType.DataTypeID = dtStaticArray then
  begin
    DataType := (DataType as TIDArray).Dimensions[0];
    Decl := TIDIntConstant.CreateAsAnonymous(UN.IntfScope, SYSUnit._Int32, (DataType as TIDOrdinal).LowBound);
  end else
  if DataType.DataTypeID in [dtString, dtShortString, dtAnsiString] then
    Exit(SYSUnit._OneExpression)
  else
  if DataType.DataTypeID in [dtDynArray, dtOpenArray, dtWideString] then
  begin
    Exit(SYSUnit._ZeroIntExpression);
  end else
    UN.ERRORS.ORDINAL_TYPE_REQUIRED(Expr.TextPosition);

  Result := TIDExpression.Create(Decl, UN.Lexer_Position);
end;

class function TSF_LoBound.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Low', SYSUnit._OrdinalType);
  Result.AddParam('Value', SYSUnit._Void, [VarConst]);
end;

{ TSF_HiBound }

function TSF_HiBound.Process(var EContext: TEContext): TIDExpression;
var
  Expr: TIDExpression;
  Decl: TIDDeclaration;
  DataType: TIDType;
  UN: TASTDelphiUnit;
begin
  // читаем аргумент
  Expr := EContext.RPNPopExpression();

  UN := GetUnit(EContext);
  UN.CheckEmptyExpression(Expr);

  Decl := Expr.Declaration;
  case Decl.ItemType of
    itType: DataType := Decl as TIDType;
    itVar, itConst: DataType := Decl.DataType;
  else
    UN.ERRORS.ORDINAL_TYPE_REQUIRED(Expr.TextPosition);
    Exit(nil);
  end;

  DataType := DataType.ActualDataType;

  if DataType.IsOrdinal then
  begin
    Decl := TIDIntConstant.CreateAsAnonymous(UN.IntfScope, DataType, (DataType as TIDOrdinal).HighBound);
  end else
  if DataType.DataTypeID = dtStaticArray then
  begin
    DataType := (DataType as TIDArray).Dimensions[0];
    Decl := TIDIntConstant.CreateAsAnonymous(UN.IntfScope, SYSUnit._Int32, (DataType as TIDOrdinal).HighBound);
  end else
  if DataType.DataTypeID in [dtDynArray, dtOpenArray, dtString, dtShortString, dtAnsiString] then
  begin
    // Lenght - 1
    Decl := EContext.Proc.GetTMPVar(SYSUnit._NativeUInt); // tmp
  end else
    UN.ERRORS.ORDINAL_TYPE_REQUIRED(Expr.TextPosition);

  Result := TIDExpression.Create(Decl, UN.Lexer_Position);
end;

class function TSF_HiBound.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'High', SYSUnit._OrdinalType);
  Result.AddParam('Value', SYSUnit._Void, [VarConst]);
end;

{ TSF_Inc }

function TSF_Inc.Process(var EContext: TEContext): TIDExpression;
var
  Increment, Value: TIDExpression;
begin
  // читаем второй аргумент (значение инкремента/декримента)
  Increment := EContext.RPNPopExpression();
  // читаем первый аргумент (переменная)
  Value := EContext.RPNPopExpression();

  Result := nil;
end;

class function TSF_Inc.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Inc', SYSUnit._OrdinalType);
  Result.AddParam('Value', SYSUnit._OrdinalType, [VarInOut]);
  Result.AddParam('Increment', SYSUnit._OrdinalType, [VarConst, VarHasDefault], SYSUnit._OneExpression);
end;

{ TCT_Dec }

function TCT_Dec.Process(var EContext: TEContext): TIDExpression;
var
  Increment, Value: TIDExpression;
begin
  // читаем второй аргумент (значение инкремента/декримента)
  Increment := EContext.RPNPopExpression();
  // читаем первый аргумент (переменная)
  Value := EContext.RPNPopExpression();

  Result := nil;
end;

class function TCT_Dec.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := TCT_Dec.Create(Scope, 'Dec', SYSUnit._OrdinalType);
  Result.AddParam('Value', SYSUnit._OrdinalType, [VarInOut]);
  Result.AddParam('Decrement', SYSUnit._OrdinalType, [VarConst, VarHasDefault], SYSUnit._OneExpression);
end;

{ TSF_Length }

function TSF_Length.Process(var EContext: TEContext): TIDExpression;
var
  Expr: TIDExpression;
  DataType: TIDType;
  Decl: TIDDeclaration;
begin
  // read argument
  Expr := EContext.RPNPopExpression();

  // resolve possible function call
  if Expr.ItemType = itProcedure then
  begin
    var AProc := Expr.AsProcedure;
    if (AProc.ParamsCount = 0) and Assigned(AProc.ResultType) then
    begin
      var TMPVar := EContext.Proc.GetTMPVar(AProc.ResultType);
      Expr := TIDExpression.Create(TMPVar, Expr.TextPosition);
    end else
     AbortWork(sArrayOrStringTypeRequired, Expr.TextPosition);
  end;

  DataType := Expr.DataType.ActualDataType;

  case DataType.DataTypeID of
    // static array
    dtStaticArray: Result := IntConstExpression(EContext.SContext, TIDArray(DataType).Dimensions[0].ElementsCount);
    // oppen array
    dtOpenArray: begin
      Decl := Expr.Declaration;
      if Decl.ItemType = itConst then
        Result := IntConstExpression(EContext.SContext, TIDDynArrayConstant(Decl).ArrayLength)
      else begin
        var TMPVar := EContext.Proc.GetTMPVar(SYSUnit._NativeInt);
        Result := TIDExpression.Create(TMPVar, Expr.TextPosition);
      end;
    end;
    // dynamic array, string
    dtDynArray, dtShortString, dtString, dtAnsiString, dtWideString: begin
      if Expr.Declaration is TIDDynArrayConstant then
        Result := IntConstExpression(EContext.SContext, Expr.AsDynArrayConst.ArrayLength)
      else
      if Expr.IsConstant then
        Result := IntConstExpression(EContext.SContext, Expr.AsStrConst.StrLength)
      else begin
        var TMPVar := EContext.Proc.GetTMPVar(SYSUnit._NativeInt);
        Result := TIDExpression.Create(TMPVar, Expr.TextPosition);
      end;
    end;
    // pchar, pansichar
    dtAnsiChar, dtChar: begin
      Result := IntConstExpression(EContext.SContext, 1);
    end;
    // pchar, pansichar
    dtPAnsiChar, dtPWideChar: begin
      var TMPVar := EContext.Proc.GetTMPVar(SYSUnit._NativeUInt);
      Result := TIDExpression.Create(TMPVar, Expr.TextPosition);
    end
  else
    AbortWork(sArrayOrStringTypeRequired, Expr.TextPosition);
    Result := nil;
  end;
end;

class function TSF_Length.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Length', SYSUnit._NativeUInt);
  Result.AddParam('S', SYSUnit._AnyArrayType);
end;

{ TSF_SetLength }

class function TSF_SetLength.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'SetLength', SYSUnit._Void);
  Result.AddParam('S', SYSUnit._AnyArrayType);
  Result.AddParam('Len', SYSUnit._NativeUInt);
end;

function TSF_SetLength.Process(var EContext: TEContext): TIDExpression;
var
  SExpr, LExpr: TIDExpression;
begin
  // read arguments
  LExpr := EContext.RPNPopExpression();
  SExpr := EContext.RPNPopExpression();
  Result := nil;
end;

{ TSF_Ord }

function TSF_Ord.Process(var EContext: TEContext): TIDExpression;
var
  Expr: TIDExpression;
  CValue: Int64;
  UN: TASTDelphiUnit;
begin
  UN := GetUnit(EContext);
  // read the argument
  Expr := EContext.RPNPopExpression();
  UN.CheckEmptyExpression(Expr);
  UN.CheckOrdinalExpression(Expr);
  if Expr.IsConstant then begin
    CValue := Expr.AsConst.AsInt64;
    Result := TIDExpression.Create(TIDIntConstant.CreateAsAnonymous(UN.IntfScope, SYSUnit._Int32, CValue), Expr.TextPosition)
  end else
    Result := TIDCastExpression.Create(Expr.Declaration, SYSUnit._Int32, UN.Lexer_PrevPosition);
end;

class function TSF_Ord.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Ord', SYSUnit._Int64);
  Result.AddParam('Value', SYSUnit._Void, [VarConst]);
end;

{ TSF_Chr }

class function TSF_Chr.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Chr', SYSUnit._WideChar);
  Result.AddParam('X', SYSUnit._UInt8, []);
end;

function TSF_Chr.Process(var EContext: TEContext): TIDExpression;
var
  Expr: TIDExpression;
begin
  Expr := EContext.RPNPopExpression();
  Result := TIDCastExpression.Create(Expr.Declaration, SYSUnit._WideChar, Expr.TextPosition);
end;

{ TSF_FillChar }

class function TSF_FillChar.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'FillChar', nil);
  Result.AddParam('X', SYSUnit._UntypedReference, [VarInOut]);
  Result.AddParam('Count', SYSUnit._NativeInt, [VarConst]);
  Result.AddParam('Value', SYSUnit._OrdinalType, [VarConst]);
end;

function TSF_FillChar.Process(var EContext: TEContext): TIDExpression;
var
  EX, ECount, EValue: TIDExpression;
begin
  EX := EContext.RPNPopExpression();
  ECount := EContext.RPNPopExpression();
  EValue := EContext.RPNPopExpression();
  Result := nil;
end;

{ TSF_Assigned }

class function TSF_Assigned.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Assigned', SYSUnit._Boolean);
  Result.AddParam('Value', SYSUnit._Pointer, [VarConst]);
end;

function TSF_Assigned.Process(var EContext: TEContext): TIDExpression;
var
  Expr: TIDExpression;
begin
  Expr := EContext.RPNPopExpression();
  TASTDelphiUnit.CheckReferenceType(Expr);
  Result := GetBoolResultExpr(EContext.SContext);
end;

{ TSF_SetString }

class function TSF_SetString.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'SetString', SYSUnit._Void);
  Result.AddParam('S', SYSUnit._AnyArrayType);
  Result.AddParam('Buffer', SYSUnit._Pointer);
  Result.AddParam('Length', SYSUnit._Int32);
end;

function TSF_SetString.Process(var EContext: TEContext): TIDExpression;
var
  SExpr, BExpr, LExpr: TIDExpression;
begin
  // read arguments
  LExpr := EContext.RPNPopExpression();
  BExpr := EContext.RPNPopExpression();
  SExpr := EContext.RPNPopExpression();
  Result := nil;
end;

{ TSF_Trunc }

class function TSF_Trunc.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Trunc', SYSUnit._Float64);
  Result.AddParam('Val', SYSUnit._Float64);
end;

function TSF_Trunc.Process(var EContext: TEContext): TIDExpression;
var
  Arg: TIDExpression;
begin
  // read argument
  Arg := EContext.RPNPopExpression();
  Result := Arg;

  var ResVar := EContext.SContext.Proc.GetTMPVar(SYSUnit._Int64);
  Result := TIDExpression.Create(ResVar, Arg.TextPosition);
end;

{ TSF_Odd }

class function TSF_Odd.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Odd', SYSUnit._Boolean);
  Result.AddParam('X', SYSUnit._Int32, [VarConst]);
end;

function TSF_Odd.Process(var EContext: TEContext): TIDExpression;
var
  Arg: TIDExpression;
begin
  // read argument
  Arg := EContext.RPNPopExpression();
  Result := SYSUnit._TrueExpression;
end;

{ TSF_Val }

class function TSF_Val.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Val', SYSUnit._Void);
  Result.AddParam('S', SYSUnit._UnicodeString, [VarConst]);
  Result.AddParam('V', SYSUnit._UntypedReference, [VarInOut]);
  Result.AddParam('Code', SYSUnit._Int32, []);
end;

function TSF_Val.Process(var EContext: TEContext): TIDExpression;
var
  ArgS, ArgV, ArgCnt: TIDExpression;
begin
  // read arguments
  ArgCnt := EContext.RPNPopExpression();
  ArgV := EContext.RPNPopExpression();
  ArgS := EContext.RPNPopExpression();
  Result := SYSUnit._ZeroIntExpression;
end;

{ TSF_Abs }

class function TSF_Abs.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Abs', SYSUnit._Void);
  Result.AddParam('X', SYSUnit._Void, [VarConst]);
end;

function TSF_Abs.Process(var EContext: TEContext): TIDExpression;
var
  Arg: TIDExpression;
begin
  // read argument
  Arg := EContext.RPNPopExpression();
  Result := Arg;
end;

{ TSF_Str }

class function TSF_Str.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Str', SYSUnit._Void);
  Result.AddParam('X', SYSUnit._Void, [VarConst]);
  Result.AddParam('S', SYSUnit._UnicodeString, [VarInOut]);
end;

function TSF_Str.Process(var EContext: TEContext): TIDExpression;
var
  ArgX, ArgS: TIDExpression;
begin
  ArgX := EContext.RPNPopExpression();
  ArgS := EContext.RPNPopExpression();
  Result := nil;
end;

{ TSF_Round }

class function TSF_Round.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Round', SYSUnit._Int64);
  Result.AddParam('X', SYSUnit._Float64, [VarConst]);
end;

function TSF_Round.Process(var EContext: TEContext): TIDExpression;
var
  Arg: TIDExpression;
begin
  // read argument
  Arg := EContext.RPNPopExpression();

  var ResVar := EContext.SContext.Proc.GetTMPVar(SYSUnit._Int64);
  Result := TIDExpression.Create(ResVar, Arg.TextPosition);
end;

{ TSF_Pred }

class function TSF_Pred.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Pred', SYSUnit._OrdinalType);
  Result.AddParam('X', SYSUnit._OrdinalType, [VarConst]);
end;

function TSF_Pred.Process(var EContext: TEContext): TIDExpression;
var
  Arg: TIDExpression;
begin
  // read argument
  Arg := EContext.RPNPopExpression();
  Result := Arg;
end;

{ TSF_GetMem }

class function TSF_GetMem.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'GetMem', SYSUnit._Void);
  Result.AddParam('StoragePointer ', SYSUnit._Pointer, [VarInOut]);
  Result.AddParam('StorageSize ', SYSUnit._Int32, [VarConst]);
end;

function TSF_GetMem.Process(var EContext: TEContext): TIDExpression;
var
  ArgP, ArgS: TIDExpression;
begin
  // read arguments
  ArgS := EContext.RPNPopExpression();
  ArgP := EContext.RPNPopExpression();
  Result := nil;
end;

{ TSF_Succ }

class function TSF_Succ.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Succ', SYSUnit._OrdinalType);
  Result.AddParam('X', SYSUnit._OrdinalType, [VarConst]);
end;

function TSF_Succ.Process(var EContext: TEContext): TIDExpression;
var
  Arg: TIDExpression;
begin
  // read argument
  Arg := EContext.RPNPopExpression();
  Result := Arg;
end;

{ TSF_FreeMem }

class function TSF_FreeMem.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'FreeMem', SYSUnit._Void);
  Result.AddParam('P', SYSUnit._Pointer, [VarConst]);
  Result.AddParam('Size', SYSUnit._Int32, [VarConst], SYSUnit._ZeroIntExpression);
end;

function TSF_FreeMem.Process(var EContext: TEContext): TIDExpression;
var
  ArgP, ArgS: TIDExpression;
begin
  // read arguments
  ArgS := EContext.RPNPopExpression();
  ArgP := EContext.RPNPopExpression();
  Result := nil;
end;

{ TSF_Copy }

class function TSF_Copy.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Copy', SYSUnit._Void);
  Result.AddParam('S', SYSUnit._Pointer, [VarConst]);
  Result.AddParam('Index', SYSUnit._Int32, [VarConst], SysUnit._ZeroIntExpression);
  Result.AddParam('Count ', SYSUnit._Int32, [VarConst], SysUnit.SystemDeclarations._MaxIntExpression);
end;

function TSF_Copy.Process(var EContext: TEContext): TIDExpression;
var
  ArgSrc, ArgIdx, ArgCnt: TIDExpression;
begin
  // read arguments
  ArgCnt := EContext.RPNPopExpression();
  ArgIdx := EContext.RPNPopExpression();
  ArgSrc := EContext.RPNPopExpression();
  Result := ArgSrc;
end;

{ TSF_Exit }

class function TSF_Exit.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Exit', SYSUnit._Void);
end;

function TSF_Exit.Process(const Ctx: TSysFunctionContext): TIDExpression;
var
  LProc: TIDProcedure;
  LResultExpr: TIDExpression;
begin
  // read arguments
  LProc := Ctx.SContext.Proc;
  LResultExpr := Ctx.EContext.RPNTryPopExpression();

  if Assigned(LResultExpr) then
  begin
    if not Assigned(LProc.ResultType) then
      Ctx.ERRORS.PROCEDURE_CANNOT_HAVE_RESULT;

    Ctx.UN.MatchImplicit3(Ctx.SContext^, LResultExpr, LProc.ResultType);
  end;
  Result := nil;
end;


{ TSF_Halt }

class function TSF_Halt.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Halt', SYSUnit._Void);
  Result.AddParam('ExitValue', SYSUnit._Int32, [VarConst]);
end;

function TSF_Halt.Process(var EContext: TEContext): TIDExpression;
var
  AResult: TIDExpression;
begin
  // read arguments
  AResult := EContext.RPNPopExpression();
  Result := nil;
end;

{ TSF_New }

class function TSF_New.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'New', SYSUnit._Void);
  Result.AddParam('X', SYSUnit._Pointer, [VarInOut]);
end;

function TSF_New.Process(var EContext: TEContext): TIDExpression;
var
  AResult: TIDExpression;
begin
  // read arguments
  AResult := EContext.RPNPopExpression();
  Result := nil;
end;

{ TSF_Dispose }

class function TSF_Dispose.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Dispose', SYSUnit._Void);
  Result.AddParam('P', SYSUnit._Pointer, [VarConst]);
end;

function TSF_Dispose.Process(var EContext: TEContext): TIDExpression;
var
  AResult: TIDExpression;
begin
  // read arguments
  AResult := EContext.RPNPopExpression();
  Result := nil;
end;

{ TSF_ReallocMem }

class function TSF_ReallocMem.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'ReallocMem', SYSUnit._Void);
  Result.AddParam('P', SYSUnit._Pointer, [VarInOut]);
  Result.AddParam('Size', SYSUnit._Int32, [VarConst]);
end;

function TSF_ReallocMem.Process(var EContext: TEContext): TIDExpression;
var
  ASize, APtr: TIDExpression;
begin
  // read arguments
  ASize := EContext.RPNPopExpression();
  APtr := EContext.RPNPopExpression();
  Result := nil;
end;

{ TSF_AtomicDecreament }

class function TSF_AtomicDecrement.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'AtomicDecrement', SYSUnit._Void);
  Result.AddParam('Target', SYSUnit._UntypedReference, [VarInOut]);
  Result.AddParam('Value', SYSUnit._Int32, [VarConst], SYSUnit._OneExpression);
end;

function TSF_AtomicDecrement.Process(var EContext: TEContext): TIDExpression;
var
  AValue, ATarget: TIDExpression;
begin
  // read arguments
  AValue := EContext.RPNPopExpression();
  ATarget := EContext.RPNPopExpression();
  Result := AValue;
end;

{ TSF_AtomicIncrement }

class function TSF_AtomicIncrement.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'AtomicIncrement', SYSUnit._Void);
  Result.AddParam('Target', SYSUnit._UntypedReference, [VarInOut]);
  Result.AddParam('Value', SYSUnit._Int32, [VarConst], SYSUnit._OneExpression);
end;

function TSF_AtomicIncrement.Process(var EContext: TEContext): TIDExpression;
var
  AValue, ATarget: TIDExpression;
begin
  // read arguments
  AValue := EContext.RPNPopExpression();
  ATarget := EContext.RPNPopExpression();
  Result := AValue;
end;


{ TSF_Assert }

class function TSF_Assert.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Assert', SYSUnit._Void);
  Result.AddParam('Condition', SYSUnit._Boolean, [VarConst]);
  Result.AddParam('Message', SYSUnit._UnicodeString, [VarConst], SYSUnit._NullPtrExpression);
end;

function TSF_Assert.Process(var EContext: TEContext): TIDExpression;
var
  AMessage, ACondition: TIDExpression;
begin
  // read arguments
  AMessage := EContext.RPNPopExpression();
  ACondition := EContext.RPNPopExpression();
  Result := nil;
end;

{ TSF_Swap }

class function TSF_Swap.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Swap', SYSUnit._Int32);
  Result.AddParam('Value', SYSUnit._Int32, [VarConst]);
end;

function TSF_Swap.Process(var EContext: TEContext): TIDExpression;
var
  AValue: TIDExpression;
begin
  // read arguments
  AValue := EContext.RPNPopExpression();
  Result := AValue;
end;

{ TSF_Close }

class function TSF_Close.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Close', SYSUnit._Void);
  Result.AddParam('Handle', SYSUnit._Pointer, [VarConst]);
end;

function TSF_Close.Process(var EContext: TEContext): TIDExpression;
var
  AHandle: TIDExpression;
begin
  // read arguments
  AHandle := EContext.RPNPopExpression();
  Result := nil;
end;

{ TSF_Pi }

class function TSF_Pi.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Pi', SYSUnit._Float64);
end;

function TSF_Pi.Process(var EContext: TEContext): TIDExpression;
begin
  Result := SYSUnit._ZeroFloatExpression;
end;

{ TSF_Sqr }

class function TSF_Sqr.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Sqr', SYSUnit._Variant);
  Result.AddParam('Value', SYSUnit._Variant, [VarConst]);
end;

function TSF_Sqr.Process(var EContext: TEContext): TIDExpression;
var
  AValue: TIDExpression;
begin
  // read arguments
  AValue := EContext.RPNPopExpression();
  Result := AValue;
end;

{ TSF_Include }

class function TSF_Include.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Include', SYSUnit._Void);
  Result.AddParam('Set', SYSUnit._Variant, [VarInOut]);
  Result.AddParam('Flags', SYSUnit._Variant, [VarConst]);
end;

function TSF_Include.Process(var EContext: TEContext): TIDExpression;
var
  A1, A2: TIDExpression;
begin
  // read arguments
  A2 := EContext.RPNPopExpression();
  A1 := EContext.RPNPopExpression();
  Result := nil;
end;

{ TSF_Exclude }

class function TSF_Exclude.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Exclude', SYSUnit._Void);
  Result.AddParam('Set', SYSUnit._Variant, [VarInOut]);
  Result.AddParam('Flags', SYSUnit._Variant, [VarConst]);
end;

function TSF_Exclude.Process(var EContext: TEContext): TIDExpression;
var
  A1, A2: TIDExpression;
begin
  // read arguments
  A2 := EContext.RPNPopExpression();
  A1 := EContext.RPNPopExpression();
  Result := nil;
end;

{ TSF_ReturnAddress }

class function TSF_ReturnAddress.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'ReturnAddress', SYSUnit._Pointer);
end;

function TSF_ReturnAddress.Process(var EContext: TEContext): TIDExpression;
var
  ResVar: TIDVariable;
  UN: TASTDelphiUnit;
begin
  UN := GetUnit(EContext);
  ResVar := EContext.SContext.Proc.GetTMPVar(SYSUnit._Pointer);
  Result := TIDExpression.Create(ResVar, UN.Lexer_Position);
end;

{ TSF_Delete }

class function TSF_Delete.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Delete', nil);
  Result.AddParam('Source', SYSUnit._UnicodeString, [VarInOut]);
  Result.AddParam('StartChar', SYSUnit._Int32, []);
  Result.AddParam('Count', SYSUnit._Int32, []);
end;

function TSF_Delete.Process(var EContext: TEContext): TIDExpression;
var
  A1, A2, A3: TIDExpression;
begin
  // read arguments
  A3 := EContext.RPNPopExpression();
  A2 := EContext.RPNPopExpression();
  A1 := EContext.RPNPopExpression();
  Result := nil;
end;

{ TSF_GetDir }

class function TSF_GetDir.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'GetDir', nil);
  Result.AddParam('Drive', SYSUnit._UInt8, []);
  Result.AddParam('Directory ', SYSUnit._UnicodeString, [VarInOut]);
end;

function TSF_GetDir.Process(var EContext: TEContext): TIDExpression;
var
  A1, A2: TIDExpression;
begin
  // read arguments
  A2 := EContext.RPNPopExpression();
  A1 := EContext.RPNPopExpression();
  Result := nil;
end;

{ TSCTF_Default }

class function TSCTF_Default.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Default', SYSUnit._Void);
  Result.AddParam('Type', SYSUnit._Untyped, [VarConst]);
end;

function TSCTF_Default.Process(const Ctx: TSysFunctionContext): TIDExpression;
begin
  var ATypeExpr := Ctx.EContext.RPNPopExpression();
  Ctx.UN.CheckType(ATypeExpr);
  var ResVar := Ctx.EContext.SContext.Proc.GetTMPVar(ATypeExpr.AsType);
  Result := TIDExpression.Create(ResVar, Ctx.UN.Lexer_Position);
end;

{ TSCTF_Console }

class function TSCTF_Console.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, '_console', SYSUnit._Void);
  Result.AddParam('message', SYSUnit._UnicodeString, [VarConst]);
end;

function TSCTF_Console.Process(const Ctx: TSysFunctionContext): TIDExpression;
begin
  var MsgArg := Ctx.EContext.RPNPopExpression();
  Ctx.UN.CheckConstExpression(MsgArg);
  var StrMessage := MsgArg.AsConst.AsString;
  Ctx.UN.Package.CosoleWrite(Ctx.UN, Ctx.UN.Lexer_Line, StrMessage);
  Result := nil;
end;

{ TSCTF_Scope }

class function TSCTF_Scope.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, '_scope', SYSUnit._Void);
end;

function TSCTF_Scope.Process(const Ctx: TSysFunctionContext): TIDExpression;
begin
  Ctx.UN.Package.CosoleWrite(Ctx.UN, Ctx.UN.Lexer_Line, Ctx.Scope.GetParentNames);
  Result := nil;
end;

{ TSCTF_TypeName }

class function TSCTF_TypeName.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, '_typename', SYSUnit._UnicodeString);
  Result.AddParam('declaration', SYSUnit._Void, [VarConst]);
  Result.AddParam('expandanonymous', SYSUnit._Boolean, [], SysUnit._FalseExpression);
end;

function TSCTF_TypeName.Process(const Ctx: TSysFunctionContext): TIDExpression;
begin
  var ExpandArg := Ctx.EContext.RPNPopExpression();
  var DeclArg := Ctx.EContext.RPNPopExpression();
  var ADataType: TIDType := nil;
  case DeclArg.ItemType of
    itVar, itConst, itProperty: ADataType := DeclArg.DataType;
    itType: ADataType := DeclArg.AsType;
  end;

  var ATypeName := '';
  if Assigned(ADataType) then
  begin
    if ExpandArg.AsBoolConst.Value then
      ATypeName := ADataType.DisplayName
    else
      ATypeName := ADataType.Name;
  end;

  var StrConst := TIDStringConstant.CreateAsAnonymous(Ctx.Scope, SYSUnit._UnicodeString, ATypeName);
  Result := TIDExpression.Create(StrConst, Ctx.UN.Lexer_Position);
end;

{ TSF_Insert }

class function TSF_Insert.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Insert', SYSUnit._Void);
  Result.AddParam('Source', SYSUnit._Untyped, [VarInOut]);
  Result.AddParam('Dest', SYSUnit._Untyped, [VarInOut]);
  Result.AddParam('Index', SYSUnit._Int32, []);
end;

function TSF_Insert.Process(var EContext: TEContext): TIDExpression;
begin
  var A3 := EContext.RPNPopExpression();
  var A2 := EContext.RPNPopExpression();
  var A1 := EContext.RPNPopExpression();
  Result := nil;
end;

{ TCT_DynArrayCreate }

class function TCT_DynArrayCreate.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Create', SYSUnit._Void);
end;

function TCT_DynArrayCreate.GetParamsCount: Integer;
begin
  Result := -1;
end;

function TCT_DynArrayCreate.Process(const Ctx: TSysFunctionContext): TIDExpression;
begin
  for var AIndex := 0 to Ctx.ArgsCount - 1 do
  begin
    var Arg := Ctx.EContext.RPNPopExpression();
    // todo:
  end;

  var AArray := Ctx.SContext.Proc.GetTMPVar(ResultType);
  Result := TIDExpression.Create(AArray, Ctx.UN.Lexer_Position);
end;

{ TSF_HiByte }

class function TSF_HiByte.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Hi', SYSUnit._UInt8);
  Result.AddParam('Value', SYSUnit._Int32, [VarConst]);
end;

function TSF_HiByte.Process(var EContext: TEContext): TIDExpression;
begin
  var AExpr := EContext.RPNPopExpression;
  var AResul := EContext.SContext.Proc.GetTMPVar(SYSUnit._Int32);
  Result := TIDExpression.Create(AResul, AExpr.TextPosition);
end;

{ TSF_LoByte }

class function TSF_LoByte.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Lo', SYSUnit._UInt8);
  Result.AddParam('Value', SYSUnit._Int32, []);
end;

function TSF_LoByte.Process(var EContext: TEContext): TIDExpression;
begin
  var AExpr := EContext.RPNPopExpression;
  var AResul := EContext.SContext.Proc.GetTMPVar(SYSUnit._Int32);
  Result := TIDExpression.Create(AResul, AExpr.TextPosition);
end;

{ TSF_VarCast }

class function TSF_VarCast.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'VarCast', nil);
  Result.AddParam('Dest', SYSUnit._Variant, [VarInOut]);
  Result.AddParam('Source', SYSUnit._Variant, [VarConst]);
  Result.AddParam('VarType', SYSUnit._Int32, []);
end;

function TSF_VarCast.Process(var EContext: TEContext): TIDExpression;
begin
  var AVarType := EContext.RPNPopExpression;
  var ASource := EContext.RPNPopExpression;
  var ADest := EContext.RPNPopExpression;
  Result := nil;
end;

{ TSF_VarClear }

class function TSF_VarClear.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'VarClear', nil);
  Result.AddParam('V', SYSUnit._Variant, [VarInOut]);
end;

function TSF_VarClear.Process(var EContext: TEContext): TIDExpression;
begin
  var AValue := EContext.RPNPopExpression;
  Result := nil;
end;

{ TSCTF_IsManagedType }

class function TSCTF_IsManagedType.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'IsManagedType', SYSUnit._Boolean);
  Result.AddParam('T', SYSUnit._TypeID, []);
end;

function TSCTF_IsManagedType.Process(const Ctx: TSysFunctionContext): TIDExpression;
begin
  var ATypeExpr := Ctx.EContext.RPNPopExpression();
  Ctx.UN.CheckType(ATypeExpr);
  var ResVar := Ctx.EContext.SContext.Proc.GetTMPVar(SYSUnit._Boolean);
  Result := TIDExpression.Create(ResVar, Ctx.UN.Lexer_Position);
end;

{ TSCTF_IsConstValue }

class function TSCTF_IsConstValue.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'IsConstValue', SYSUnit._Boolean);
  Result.AddParam('T', SYSUnit._UntypedReference, [VarConst]);
end;

function TSCTF_IsConstValue.Process(const Ctx: TSysFunctionContext): TIDExpression;
begin
  var AValeExpr := Ctx.EContext.RPNPopExpression();
  var ResVar := Ctx.EContext.SContext.Proc.GetTMPVar(SYSUnit._Boolean);
  Result := TIDExpression.Create(ResVar, Ctx.UN.Lexer_Position);
end;

{ TSCTF_TypeInfo }

class function TSCTF_TypeInfo.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'TypeInfo', SYSUnit._Pointer);
  Result.AddParam('T', SYSUnit._TypeID, []);
end;

function TSCTF_TypeInfo.Process(const Ctx: TSysFunctionContext): TIDExpression;
begin
  var ATypeExpr := Ctx.EContext.RPNPopExpression();
  Ctx.UN.CheckType(ATypeExpr);
  var ResVar := Ctx.EContext.SContext.Proc.GetTMPVar(SYSUnit._Pointer);
  Result := TIDExpression.Create(ResVar, Ctx.UN.Lexer_Position);
end;

{ TSCTF_GetTypeKind }

class function TSCTF_GetTypeKind.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'GetTypeKind', SYSUnit._TTypeKind);
  Result.AddParam('T', SYSUnit._TypeID, []);
end;

function TSCTF_GetTypeKind.Process(const Ctx: TSysFunctionContext): TIDExpression;
begin
  var ATypeExpr := Ctx.EContext.RPNPopExpression();
  Ctx.UN.CheckType(ATypeExpr);
  var ResVar := Ctx.EContext.SContext.Proc.GetTMPVar(SYSUnit._TTypeKind);
  Result := TIDExpression.Create(ResVar, Ctx.UN.Lexer_Position);
end;

{ TSCTF_HasWeakRef }

class function TSCTF_HasWeakRef.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'HasWeakRef', SYSUnit._TTypeKind);
  Result.AddParam('T', SYSUnit._TypeID, []);
end;

function TSCTF_HasWeakRef.Process(const Ctx: TSysFunctionContext): TIDExpression;
begin
  var AValeExpr := Ctx.EContext.RPNPopExpression();
  var ResVar := Ctx.EContext.SContext.Proc.GetTMPVar(SYSUnit._Boolean);
  Result := TIDExpression.Create(ResVar, Ctx.UN.Lexer_Position);
end;

{ TCT_Break }

class function TCT_Break.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Break', SYSUnit._Void);
end;

function TCT_Break.Process(const Ctx: TSysFunctionContext): TIDExpression;
begin
  Result := nil;
  if not Ctx.SContext.IsLoopBody then
    Ctx.ERRORS.BREAK_OR_CONTINUE_ALLOWED_ONLY_IN_LOOPS;

  Ctx.SContext.Add(TASTKWBreak);
end;

{ TCT_Continue }

class function TCT_Continue.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Continue', SYSUnit._Void);
end;

function TCT_Continue.Process(const Ctx: TSysFunctionContext): TIDExpression;
begin
  Result := nil;
  if not Ctx.SContext.IsLoopBody then
    Ctx.ERRORS.BREAK_OR_CONTINUE_ALLOWED_ONLY_IN_LOOPS;

  Ctx.SContext.Add(TASTKWContinue);
end;

{ TSF_MulDivInt64 }

class function TSF_MulDivInt64.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'MulDivInt64', SYSUnit._Int64);
  Result.AddParam('AValue', SysUnit._Int64);
  Result.AddParam('AMul', SysUnit._Int64);
  Result.AddParam('ADiv', SysUnit._Int64);
  Result.AddParam('Remainder', SysUnit._Int64, [VarOut]);
end;

function TSF_MulDivInt64.Process(var EContext: TEContext): TIDExpression;
begin
  EContext.RPNPopExpression();
  EContext.RPNPopExpression();
  EContext.RPNPopExpression();
  EContext.RPNPopExpression();

  var ResVar := EContext.SContext.Proc.GetTMPVar(SYSUnit._Int64);
  Result := TIDExpression.Create(ResVar, SYSUnit.Lexer_Position);
end;

{ TSF_VarArgStart }

class function TSF_VarArgStart.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  // procedure VarArgStart(var ArgList: TVarArgList);
  Result := Self.Create(Scope, 'VarArgStart', SYSUnit._Void);
  Result.AddParam('ArgList', SysUnit._Untyped, [VarInOut]);
end;

function TSF_VarArgStart.Process(var EContext: TEContext): TIDExpression;
begin
  EContext.RPNPopExpression();
  Result := nil;
end;

{ TSF_VarArgGetValue }

class function TSF_VarArgGetValue.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  // function  VarArgGetValue(var ArgList: TVarArgList; ArgType: Type): ArgType;
  Result := Self.Create(Scope, 'VarArgGetValue', SYSUnit._Pointer);
  Result.AddParam('ArgList', SysUnit._Untyped, [VarInOut]);
  Result.AddParam('ArgType', SysUnit._TypeID);
end;

function TSF_VarArgGetValue.Process(var EContext: TEContext): TIDExpression;
begin
  EContext.RPNPopExpression();
  EContext.RPNPopExpression();

  var LResVar := EContext.SContext.Proc.GetTMPVar(SYSUnit._Pointer);
  Result := TIDExpression.Create(LResVar, SYSUnit.Lexer_Position);
end;

{ TSF_VarArgCopy }

class function TSF_VarArgCopy.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  // procedure VarArgCopy(var DestArgList, SrcArgList: TVarArgList);
  Result := Self.Create(Scope, 'VarArgCopy', SYSUnit._Void);
  Result.AddParam('DestArgList', SysUnit._Untyped, [VarInOut]);
  Result.AddParam('SrcArgList', SysUnit._Untyped, [VarInOut]);
end;

function TSF_VarArgCopy.Process(var EContext: TEContext): TIDExpression;
begin
  EContext.RPNPopExpression();
  EContext.RPNPopExpression();
  Result := nil;
end;

{ TSF_VarArgEnd }

class function TSF_VarArgEnd.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  // procedure VarArgEnd(var ArgList: TVarArgList);
  Result := Self.Create(Scope, 'VarArgEnd', SYSUnit._Void);
  Result.AddParam('ArgList', SysUnit._Untyped, [VarInOut]);
end;

function TSF_VarArgEnd.Process(var EContext: TEContext): TIDExpression;
begin
  EContext.RPNPopExpression();
  Result := nil;
end;

end.
