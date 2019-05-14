unit NPCompiler.SysFunctions;

interface

uses OPCompiler, NPCompiler.Classes, NPCompiler.DataTypes, NPCompiler.Errors, SystemUnit,
     NPCompiler.Contexts, NPCompiler.ExpressionContext, AST.Delphi.Parser;

type

  {function: now}
  TSF_now = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {function: RunError}
  TSF_RunError = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {function: Defined}
  TSCTF_Defined = class(TIDSysCompileFunction)
  public
    function Process(const Ctx: TSysFunctionContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {function: Declared}
  TSCTF_Declared = class(TIDSysCompileFunction)
  public
    function Process(const Ctx: TSysFunctionContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {function: AtomicExchange}
  TSF_AtomicExchange = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {function: AtomicCmpExchange}
  TSF_AtomicCmpExchange = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {function: SizeOf}
  TCT_SizeOf = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {function: Low}
  TCT_LoBound = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {function: High}
  TCT_HiBound = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {function: Inc}
  TCT_Inc = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {function: Dec}
  TCT_Dec = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {function: Length}
  TCT_Length = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {function: Ord}
  TCT_Ord = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;

  {function: FillChar}
  TCT_FillChar = class(TIDSysRuntimeFunction)
  public
    function Process(var EContext: TEContext): TIDExpression; override;
    class function CreateDecl(Scope: TScope): TIDBuiltInFunction; override;
  end;


implementation

{ TSF_now }

function TSF_now.Process(var EContext: TEContext): TIDExpression;
var
  ResVar: TIDVariable;
  UN: TNPUnit;
begin
  UN := GetUnit(EContext);
  ResVar := EContext.SContext.Proc.GetTMPVar(SYSUnit._DateTime);
  Result := TIDExpression.Create(ResVar, UN.parser_Position);
end;

class function TSF_now.CreateDecl(Scope: TScope): TIDBuiltInFunction;
begin
  Result := TSF_now.Create(Scope, 'Now', SYSUnit._DateTime);
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
  Result := TSCTF_Defined.Create(Scope, 'Defined', SYSUnit._Boolean);
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
  Result := TSF_AtomicExchange.Create(Scope, 'AtomicExchange', SYSUnit._NativeInt);
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
  Result := TSF_AtomicCmpExchange.Create(Scope, 'AtomicCmpExchange', SYSUnit._NativeInt);
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
  Result := TSCTF_Declared.Create(Scope, 'Declared', SYSUnit._Boolean);
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
  Result := TSF_RunError.Create(Scope, 'RunError', nil);
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
  // if the type size is defined (the target <> ANY) - generate the int constant
  if DataType.DataSize > 0 then
    Result := IntConstExpression(DataType.DataSize)
  else begin
  // else when data size is not defined yet
    Decl := TIDSizeofConstant.CreateAnonymous(nil, SYSUnit._Int32, DataType);
    Result := TIDExpression.Create(Decl, Expr.TextPosition);
  end;
end;

class function TCT_SizeOf.CreateDecl(Scope: TScope): TIDBuiltInFunction;
begin
  Result := TCT_SizeOf.Create(Scope, 'SizeOf', SYSUnit._Int32);
  Result.AddParam('S', _Void);
end;

{ TCT_LoBound }

function TCT_LoBound.Process(var EContext: TEContext): TIDExpression;
var
  Expr: TIDExpression;
  Decl: TIDDeclaration;
  DataType: TIDType;
  UN: TNPUnit;
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
      {True: begin
        Result := GetTMPVarExpr(EContext, SYSUnit._Int32);
        Result.TextPosition := parser_Position;
        ILWrite(EContext, TIL.IL_Length(Result, Expr));
        ILWrite(EContext, TIL.IL_Sub(Result, Result, SYSUnit._OneExpression));
        Exit;
      end;}
  end else
    UN.ERROR_ORDINAL_TYPE_REQUIRED(Expr.TextPosition);

  Result := TIDExpression.Create(Decl, UN.parser_Position);
end;

class function TCT_LoBound.CreateDecl(Scope: TScope): TIDBuiltInFunction;
begin
  Result := TCT_LoBound.Create(Scope, 'Low', SYSUnit._OrdinalType);
  Result.AddParam('Value', _Void, [VarConst]);
end;

{ TCT_HiBound }

function TCT_HiBound.Process(var EContext: TEContext): TIDExpression;
var
  Expr: TIDExpression;
  Decl: TIDDeclaration;
  DataType: TIDType;
  UN: TNPUnit;
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
    Exit(nil);
      {True: begin
        Result := GetTMPVarExpr(EContext, SYSUnit._Int32);
        Result.TextPosition := parser_Position;
        ILWrite(EContext, TIL.IL_Length(Result, Expr));
        ILWrite(EContext, TIL.IL_Sub(Result, Result, SYSUnit._OneExpression));
        Exit;
      end;}
  end else
    UN.ERROR_ORDINAL_TYPE_REQUIRED(Expr.TextPosition);

  Result := TIDExpression.Create(Decl, UN.parser_Position);
end;

class function TCT_HiBound.CreateDecl(Scope: TScope): TIDBuiltInFunction;
begin
  Result := TCT_HiBound.Create(Scope, 'High', SYSUnit._OrdinalType);
  Result.AddParam('Value', _Void, [VarConst]);
end;

{ TCT_Inc }

function TCT_Inc.Process(var EContext: TEContext): TIDExpression;
var
  Increment, Value: TIDExpression;
begin
  // читаем второй аргумент (значение инкремента/декримента)
  Increment := EContext.RPNPopExpression();
  // читаем первый аргумент (переменная)
  Value := EContext.RPNPopExpression();

  Result := nil;
end;

class function TCT_Inc.CreateDecl(Scope: TScope): TIDBuiltInFunction;
begin
  Result := TCT_Inc.Create(Scope, 'Inc', SYSUnit._OrdinalType);
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

{ TCT_Length }

function TCT_Length.Process(var EContext: TEContext): TIDExpression;
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

class function TCT_Length.CreateDecl(Scope: TScope): TIDBuiltInFunction;
begin
  Result := TCT_Length.Create(Scope, 'Length', SYSUnit._Int32);
  Result.AddParam('S', SYSUnit._AnyArrayType);
end;

{ TCT_Ord }

function TCT_Ord.Process(var EContext: TEContext): TIDExpression;
var
  Expr: TIDExpression;
  CValue: Int64;
  UN: TNPUnit;
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
    Result := TIDCastExpression.Create(Expr.Declaration, SYSUnit._Int32, UN.parser_PrevPosition);
end;

class function TCT_Ord.CreateDecl(Scope: TScope): TIDBuiltInFunction;
begin
  Result := TCT_Ord.Create(Scope, 'Ord', SYSUnit._Int64);
  Result.AddParam('Value', _Void, [VarConst]);
end;

{ TCT_FillChar }

class function TCT_FillChar.CreateDecl(Scope: TScope): TIDBuiltInFunction;
begin
  Result := TCT_FillChar.Create(Scope, 'FillChar', nil);
  Result.AddParam('X', SYSUnit._UntypedReference, [VarInOut]);
  Result.AddParam('Count', SYSUnit._NativeInt, [VarConst]);
  Result.AddParam('Value', SYSUnit._OrdinalType, [VarConst]);
end;

function TCT_FillChar.Process(var EContext: TEContext): TIDExpression;
var
  EX, ECount, EValue: TIDExpression;
begin
  EX := EContext.RPNPopExpression();
  ECount := EContext.RPNPopExpression();
  EValue := EContext.RPNPopExpression();
  Result := nil;
end;

end.
