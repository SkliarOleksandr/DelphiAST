unit AST.Delphi.SysFunctions;

interface

uses AST.Pascal.Parser,
     AST.Delphi.Classes,
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
  TSF_Exit = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
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

implementation

uses AST.Delphi.Errors, AST.Lexer;

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
var
  Left, Right: TIDExpression;
begin
  Right := EContext.RPNPopExpression();
  Left := EContext.RPNPopExpression();
  Result := CreateTMPExpr(EContext, SYSUnit._NativeInt);
  // todo:
end;

class function TSF_AtomicExchange.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'AtomicExchange', SYSUnit._NativeInt);
  Result.AddParam('Left', SYSUnit._Void, [VarInOut]);
  Result.AddParam('Right', SYSUnit._Void, [VarInOut]);
end;

{ TSF_AtomicCmpExchange }

function TSF_AtomicCmpExchange.Process(var EContext: TEContext): TIDExpression;
begin
  var Arg1 := EContext.RPNPopExpression();
  var Arg2 := EContext.RPNPopExpression();
  var Arg3 := EContext.RPNPopExpression();
  Result := CreateTMPExpr(EContext, SYSUnit._NativeInt);
end;


class function TSF_AtomicCmpExchange.CreateDecl(SysUnit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'AtomicCmpExchange', SYSUnit._NativeInt);
  Result.AddParam('Target', SYSUnit._Void, [VarInOut]);
  Result.AddParam('Comparand', SYSUnit._Void, [VarIn]);
  Result.AddParam('Succeeded', SYSUnit._Void, [VarOut]);
end;

{ TSCTF_Declared }

function TSCTF_Declared.Process(const Ctx: TSysFunctionContext): TIDExpression;
var
  Expr: TIDExpression;
begin
  Expr := Ctx.EContext.RPNPopExpression();

  if not Assigned(Expr) then
    AbortWork('DEFINE String expected', TTextPosition.Empty);

  if Ctx.Scope.FindIDRecurcive(Expr.AsStrConst.Value) <> nil then
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
        Result := IntConstExpression(EContext.SContext, 0); // todo:
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
        var TMPVar := EContext.Proc.GetTMPVar(SYSUnit._NativeUInt);
        Result := TIDExpression.Create(TMPVar, Expr.TextPosition);
      end;
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
  Result := Self.Create(Scope, 'Chr', SYSUnit._AnsiChar);
  Result.AddParam('X', SYSUnit._UInt8, []);
end;

function TSF_Chr.Process(var EContext: TEContext): TIDExpression;
var
  Expr: TIDExpression;
begin
  Expr := EContext.RPNPopExpression();
  Result := TIDCastExpression.Create(Expr.Declaration, SYSUnit._AnsiChar, Expr.TextPosition);
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
  Result.AddParam('S', SYSUnit._String, [VarConst]);
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
  Result.AddParam('S', SYSUnit._String, [VarInOut]);
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
  Result := Self.Create(Scope, 'Round', SYSUnit._Float64);
  Result.AddParam('X', SYSUnit._Float64, [VarConst]);
end;

function TSF_Round.Process(var EContext: TEContext): TIDExpression;
var
  Arg: TIDExpression;
begin
  // read argument
  Arg := EContext.RPNPopExpression();
  Result := Arg;
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
  Result.AddParam('Source', SYSUnit._Pointer, [VarConst]);
  Result.AddParam('StartChar', SYSUnit._Int32, [VarConst]);
  Result.AddParam('Count ', SYSUnit._Int32, [VarConst]);
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
  Result.AddParam('Result', SYSUnit._Void, [VarConst], SYSUnit._NullPtrExpression);
end;

function TSF_Exit.Process(var EContext: TEContext): TIDExpression;
var
  AResult: TIDExpression;
begin
  // read arguments
  AResult := EContext.RPNPopExpression();
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
  Result.AddParam('Message', SYSUnit._String, [VarConst], SYSUnit._NullPtrExpression);
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

end.
