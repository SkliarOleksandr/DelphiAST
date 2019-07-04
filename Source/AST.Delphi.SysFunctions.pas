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

  {now}
  TSF_Now = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {RunError}
  TSF_RunError = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Defined}
  TSCTF_Defined = class(TIDSysCompileFunction)
  public
    function Process(const Ctx: TSysFunctionContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Declared}
  TSCTF_Declared = class(TIDSysCompileFunction)
  public
    function Process(const Ctx: TSysFunctionContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {AtomicExchange}
  TSF_AtomicExchange = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {AtomicCmpExchange}
  TSF_AtomicCmpExchange = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {SizeOf}
  TCT_SizeOf = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Low}
  TSF_LoBound = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {High}
  TSF_HiBound = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Inc}
  TSF_Inc = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Dec}
  TCT_Dec = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Length}
  TSF_Length = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {SetLength}
  TSF_SetLength = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {SetString}
  TSF_SetString = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Ord}
  TSF_Ord = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Odd}
  TSF_Odd = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Chr}
  TSF_Chr = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {FillChar}
  TSF_FillChar = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Assigned}
  TSF_Assigned = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Get8087CW}
  TSF_Get8087CW = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Set8087CW}
  TSF_Set8087CW = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Trunc}
  TSF_Trunc = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Val}
  TSF_Val = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Abs}
  TSF_Abs = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Str}
  TSF_Str = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Round}
  TSF_Round = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Pred}
  TSF_Pred = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {Succ}
  TSF_Succ = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;



implementation

uses AST.Delphi.Errors;

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

class function TSF_Now.CreateDecl(Scope: TScope): TIDBuiltInFunction;
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

class function TSCTF_Defined.CreateDecl(Scope: TScope): TIDBuiltInFunction;
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

class function TSF_AtomicExchange.CreateDecl(Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'AtomicExchange', SYSUnit._NativeInt);
  Result.AddParam('Left', _Void, [VarInOut]);
  Result.AddParam('Right', _Void, [VarInOut]);
end;

{ TSF_AtomicCmpExchange }

function TSF_AtomicCmpExchange.Process(var EContext: TEContext): TIDExpression;
begin
  var Arg1 := EContext.RPNPopExpression();
  var Arg2 := EContext.RPNPopExpression();
  var Arg3 := EContext.RPNPopExpression();
  Result := CreateTMPExpr(EContext, SYSUnit._NativeInt);
end;


class function TSF_AtomicCmpExchange.CreateDecl(Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'AtomicCmpExchange', SYSUnit._NativeInt);
  Result.AddParam('Target', _Void, [VarInOut]);
  Result.AddParam('Comparand', _Void, [VarIn]);
  Result.AddParam('Succeeded', _Void, [VarOut]);
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

class function TSCTF_Declared.CreateDecl(Scope: TScope): TIDBuiltInFunction;
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

class function TSF_RunError.CreateDecl(Scope: TScope): TIDBuiltInFunction;
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

  Result := IntConstExpression(DataType.DataSize);
  Result.TextPosition := Expr.TextPosition;
end;

class function TCT_SizeOf.CreateDecl(Scope: TScope): TIDBuiltInFunction;
begin
  Result := TCT_SizeOf.Create(Scope, 'SizeOf', SYSUnit._Int32);
  Result.AddParam('S', _Void);
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
    UN.ERROR_ORDINAL_TYPE_REQUIRED(Expr.TextPosition);
    Exit(nil);
  end;

  DataType := DataType.ActualDataType;

  if DataType.Ordinal then
  begin
    Decl := TIDIntConstant.CreateAnonymous(nil, DataType, (DataType as TIDOrdinal).LowBound);
  end else
  if DataType.DataTypeID = dtStaticArray then
  begin
    DataType := (DataType as TIDArray).Dimensions[0];
    Decl := TIDIntConstant.CreateAnonymous(nil, SYSUnit._Int32, (DataType as TIDOrdinal).LowBound);
  end else
  if DataType.DataTypeID in [dtDynArray, dtString, dtAnsiString] then
  begin
    Exit(SYSUnit._ZeroExpression);
  end else
    UN.ERROR_ORDINAL_TYPE_REQUIRED(Expr.TextPosition);

  Result := TIDExpression.Create(Decl, UN.Lexer_Position);
end;

class function TSF_LoBound.CreateDecl(Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Low', SYSUnit._OrdinalType);
  Result.AddParam('Value', _Void, [VarConst]);
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
    UN.ERROR_ORDINAL_TYPE_REQUIRED(Expr.TextPosition);
    Exit(nil);
  end;

  DataType := DataType.ActualDataType;

  if DataType.Ordinal then
  begin
    Decl := TIDIntConstant.CreateAnonymous(nil, DataType, (DataType as TIDOrdinal).HighBound);
  end else
  if DataType.DataTypeID = dtStaticArray then
  begin
    DataType := (DataType as TIDArray).Dimensions[0];
    Decl := TIDIntConstant.CreateAnonymous(nil, SYSUnit._Int32, (DataType as TIDOrdinal).HighBound);
  end else
  if DataType.DataTypeID in [dtDynArray, dtString, dtAnsiString] then
  begin
    // Lenght - 1
    Decl := TIDIntConstant.CreateAnonymous(nil, SYSUnit._Int32, 0); // tmp
  end else
    UN.ERROR_ORDINAL_TYPE_REQUIRED(Expr.TextPosition);

  Result := TIDExpression.Create(Decl, UN.Lexer_Position);
end;

class function TSF_HiBound.CreateDecl(Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'High', SYSUnit._OrdinalType);
  Result.AddParam('Value', _Void, [VarConst]);
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

class function TSF_Inc.CreateDecl(Scope: TScope): TIDBuiltInFunction;
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

class function TCT_Dec.CreateDecl(Scope: TScope): TIDBuiltInFunction;
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
  ParamName: string;
begin
  // read argument
  Expr := EContext.RPNPopExpression();
  DataType := Expr.DataType.ActualDataType;
  case DataType.DataTypeID of
    // static array
    dtStaticArray: Result := IntConstExpression(TIDArray(DataType).Dimensions[0].ElementsCount);
    // oppen array
    dtOpenArray: begin
      Decl := Expr.Declaration;
      if Decl.ItemType = itConst then
        Result := IntConstExpression(TIDDynArrayConstant(Decl).ArrayLength)
      else begin
        ParamName := Decl.Name + '$Length';
        Decl := Expr.Declaration.Scope.FindID(ParamName);
        if not Assigned(Decl) then
          AbortWorkInternal('%s param is not forund', [ParamName], Expr.TextPosition);
        Result := TIDExpression.Create(Decl, Expr.TextPosition);
      end;
    end;
    // dynamic array, string
    dtDynArray, dtString, dtAnsiString: begin
      if Expr.Declaration is TIDDynArrayConstant then
        Result := IntConstExpression(Expr.AsDynArrayConst.ArrayLength)
      else
      if Expr.IsConstant then
        Result := IntConstExpression(Expr.AsStrConst.StrLength)
      else begin
        var TMPVar := EContext.Proc.GetTMPVar(SYSUnit._NativeUInt);
        Result := TIDExpression.Create(TMPVar, Expr.TextPosition);
      end;
    end;
    // pchar, pansichar
    dtPointer: begin
      if (DataType = SYSUnit._PCharType) or (DataType = SYSUnit._PAnsiCharType) then
      begin
        var TMPVar := EContext.Proc.GetTMPVar(SYSUnit._NativeUInt);
        Result := TIDExpression.Create(TMPVar, Expr.TextPosition);
      end else begin
        AbortWork(sArrayOrStringTypeRequired, Expr.TextPosition);
        Result := nil;
      end;
    end
  else
    AbortWork(sArrayOrStringTypeRequired, Expr.TextPosition);
    Result := nil;
  end;
end;

class function TSF_Length.CreateDecl(Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Length', SYSUnit._NativeUInt);
  Result.AddParam('S', SYSUnit._AnyArrayType);
end;

{ TSF_SetLength }

class function TSF_SetLength.CreateDecl(Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'SetLength', _Void);
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
    Result := TIDExpression.Create(TIDIntConstant.CreateAnonymous(nil, SYSUnit._Int32, CValue), Expr.TextPosition)
  end else
    Result := TIDCastExpression.Create(Expr.Declaration, SYSUnit._Int32, UN.Lexer_PrevPosition);
end;

class function TSF_Ord.CreateDecl(Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Ord', SYSUnit._Int64);
  Result.AddParam('Value', _Void, [VarConst]);
end;

{ TSF_Chr }

class function TSF_Chr.CreateDecl(Scope: TScope): TIDBuiltInFunction;
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

class function TSF_FillChar.CreateDecl(Scope: TScope): TIDBuiltInFunction;
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

class function TSF_Assigned.CreateDecl(Scope: TScope): TIDBuiltInFunction;
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

class function TSF_SetString.CreateDecl(Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'SetString', _Void);
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

{ TSF_Get8087CW }

class function TSF_Get8087CW.CreateDecl(Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Get8087CW', SYSUnit._UInt16);
end;

function TSF_Get8087CW.Process(var EContext: TEContext): TIDExpression;
begin
  Result := TASTDelphiUnit.GetTMPVarExpr(EContext, SYSUnit._UInt16, TextPosition.Empty);
end;

{ TSF_Set8087CW }

class function TSF_Set8087CW.CreateDecl(Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Set8087CW', _Void);
  Result.AddParam('Val', SYSUnit._UInt16);
end;

function TSF_Set8087CW.Process(var EContext: TEContext): TIDExpression;
begin
  Result := nil;
end;

{ TSF_Trunc }

class function TSF_Trunc.CreateDecl(Scope: TScope): TIDBuiltInFunction;
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

class function TSF_Odd.CreateDecl(Scope: TScope): TIDBuiltInFunction;
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

class function TSF_Val.CreateDecl(Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Val', _Void);
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
  Result := SYSUnit._ZeroExpression;
end;

{ TSF_Abs }

class function TSF_Abs.CreateDecl(Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Abs', _Void);
  Result.AddParam('X', _Void, [VarConst]);
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

class function TSF_Str.CreateDecl(Scope: TScope): TIDBuiltInFunction;
begin
  Result := Self.Create(Scope, 'Str', _Void);
  Result.AddParam('X', _Void, [VarConst]);
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

class function TSF_Round.CreateDecl(Scope: TScope): TIDBuiltInFunction;
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

class function TSF_Pred.CreateDecl(Scope: TScope): TIDBuiltInFunction;
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

{ TSF_Succ }

class function TSF_Succ.CreateDecl(Scope: TScope): TIDBuiltInFunction;
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

end.
