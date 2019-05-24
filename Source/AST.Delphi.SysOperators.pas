unit AST.Delphi.SysOperators;

interface

uses AST.Delphi.Classes, AST.Delphi.Contexts;

type

  TIDInternalOperator = class(TIDOperator)
  public
    constructor CreateAsIntOp; reintroduce;
  end;

  TSysImplicit = class(TIDInternalOperator)
  public
    constructor CreateInternal(ResultType: TIDType); reintroduce;
    function Check(const Src, Dst: TIDType): Boolean; overload; virtual; abstract;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; overload; virtual;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; virtual;
  end;

  TSysExplisit = class(TSysImplicit)
  end;

  {внутренний implicit оператор String -> AnsiString}
  TIDOpImplicitStringToAnsiString = class(TSysImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {внутренний implicit оператор AnsiString -> String}
  TIDOpImplicitAnsiStringToString = class(TSysImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {внутренний implicit оператор Char -> String}
  TIDOpImplicitCharToString = class(TSysImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {внутренний implicit оператор Char -> AnsiString}
  TIDOpImplicitCharToAnsiString = class(TSysImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {внутренний implicit оператор Char -> AnsiChar}
  TIDOpImplicitCharToAnsiChar = class(TSysImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {внутренний implicit оператор AnsiChar -> AnsiString}
  TIDOpImplicitAnsiCharToAnsiString = class(TSysImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {внутренний implicit оператор AnsiChar -> String}
  TIDOpImplicitAnsiCharToString = class(TSysImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {внутренний implicit оператор AnsiChar -> Char}
  TIDOpImplicitAnsiCharToChar = class(TSysImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {внутренний implicit оператор String -> AnsiString}
  TIDOpImplicitStringToPChar = class(TSysImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {внутренний implicit оператор MetaClass -> TGUID}
  TIDOpImplicitMetaClassToGUID = class(TSysImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {внутренний implicit оператор String -> TGUID}
  TIDOpImplicitStringToGUID = class(TSysImplicit)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {внутренний implicit оператор Closure -> TMethod}
  TIDOpImplicitClosureToTMethod = class(TSysImplicit)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {внутренний implicit оператор ConstDynArray -> Set}
  TIDOpImplicitDynArrayToSet = class(TSysImplicit)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {внутренний implicit оператор Any -> Variant}
  TIDOpImplicitAnyToVariant = class(TSysImplicit)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {внутренний implicit оператор Variant -> Any}
  TIDOpImplicitVariantToAny = class(TSysImplicit)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {внутренний implicit оператор Variant -> Any}
  TIDOpImplicitAnyToUntyped = class(TSysImplicit)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {внутренний explicit оператор Int -> Enum}
  TIDOpExplicitIntToEnum = class(TSysImplicit)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {внутренний explicit оператор Any -> TProc}
  TIDOpExplicitTProcFromAny = class(TSysImplicit)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Check(const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {internal explicit operator: Class of -> Pointer type}
  TIDOpExplicitClassOfToAny = class(TSysImplicit)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {internal explicit operator: Class of <- Any}
  TIDOpExplicitClassOfFromAny = class(TSysExplisit)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Check(const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {internal implicit operator: Pointer -> Any}
  TIDOpImplicitPointerToAny = class(TSysImplicit)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Check(const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {sys explicit operator Enum -> Any}
  TSysExplicitEnumToAny = class(TSysExplisit)
  private
    class var fInstance: TSysImplicit;
  public
    function Check(const Src: TIDType; const Dst: TIDType): Boolean; override;
    class function Instance: TIDInternalOperator;
  end;

  {sys explicit operator AnsiString <- Any}
  TSysExplicitAnsiStringFromAny = class(TSysExplisit)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {internal explicit operator: Pointer <- Any}
  TIDOpExplictPointerFromAny = class(TSysExplisit)
  public
    function Check(const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {internal implicit operator: array -> pointer}
  TIDOpImplicitArrayToAny = class(TSysImplicit)
  public
    function Check(const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;


implementation

uses NPCompiler.DataTypes,
     NPCompiler.Utils,
     OPCompiler,
     SystemUnit;

{ TIDInternalOperator }

constructor TIDInternalOperator.CreateAsIntOp;
begin
  CreateFromPool;
end;

{ TIDInternalOpImplicit }

function TSysImplicit.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Check(Src.DataType, Dst) then
    Result := Dst
  else
    Result := nil;
end;

constructor TSysImplicit.CreateInternal(ResultType: TIDType);
begin
  CreateFromPool;
  ItemType := itProcedure;
  Self.DataType := ResultType;
end;

function TSysImplicit.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
begin
  if Check(Src.DataType, Dst) then
    Result := TIDCastExpression.Create(Src, Dst)
  else
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
  Result := nil;
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
  Result := nil;
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
  Result := nil;
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
  Result := nil;
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
  Assert(False);
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
  Result := nil;
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
  Result := nil;
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
  Result := nil;
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
//var
//  TmpVar: TIDVariable;
begin
//  if Src.DataTypeID <> Dst.DataTypeID then
//  begin
//    TmpVar := SContext.GetTMPVar(Dst, [VarTmpResOwner]);
//    Result := TIDExpression.Create(TmpVar, Src.TextPosition);
//    SContext.ILWrite(TIL.IL_Convert(Result, Src));
//  end else
//    Result := Src;
  Result := nil;
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

function TIDOpExplicitTProcFromAny.Check(const Src, Dst: TIDType): Boolean;
begin
  Result := (Dst.DataTypeID = dtPointer) and (Src as TIDProcType).IsStatic;
end;

function TIDOpExplicitTProcFromAny.Match(const SContext: PSContext; const Src: TIDExpression;
                                         const Dst: TIDType): TIDExpression;
begin
  if Src.ItemType = itProcedure then
  begin
    var SrcProc := Src.AsProcedure;
    var DstProcType := Dst as TIDProcType;
    if TNPUnit.StrictMatchProcSingnatures(SrcProc.ExplicitParams, DstProcType.Params, SrcProc.ResultType, DstProcType.ResultType) then
      Exit(Src)
    else
      Exit(nil);
  end;

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

{ TSysExplicitEnumToAny }

function TSysExplicitEnumToAny.Check(const Src, Dst: TIDType): Boolean;
begin
  Result := Dst.Ordinal;
end;

class function TSysExplicitEnumToAny.Instance: TIDInternalOperator;
begin
  if not Assigned(fInstance) then
    fInstance := TSysExplicitEnumToAny.CreateAsIntOp();
  Result := fInstance;
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

{ TIDOpImplicitAnyToUntyped }

function TIDOpImplicitAnyToUntyped.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  Result := Dst;
end;

function TIDOpImplicitAnyToUntyped.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
begin
  Result := TIDCastExpression.Create(Src.Declaration, Dst, Src.TextPosition);
end;

{ TIDOpExplicitClassOfToPointer }

function TIDOpExplicitClassOfToAny.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Dst.DataTypeID = dtPointer then
    Result := Dst
  else
    Result := nil;
end;

function TIDOpExplicitClassOfToAny.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
begin
  if Dst.DataTypeID = dtPointer then
    Result := Src
  else
    Result := nil;
end;

{ TIDOpImplicitPointerToAny }

function TIDOpImplicitPointerToAny.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Dst.DataTypeID = dtPointer then
    Result := Dst
  else
    Result := nil;
end;

function TIDOpImplicitPointerToAny.Check(const Src, Dst: TIDType): Boolean;
begin
  Result := Dst.DataTypeID = dtPointer;
end;

function TIDOpImplicitPointerToAny.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
begin
  if Dst.DataTypeID = dtPointer then
    Result := Src
  else
    Result := nil;
end;

{ TIDOpExplicitClassOfFromAny }

function TIDOpExplicitClassOfFromAny.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Dst.DataTypeID in [dtPointer, dtNativeInt, dtNativeUInt] then
    Result := Dst
  else
    Result := nil;
end;

function TIDOpExplicitClassOfFromAny.Check(const Src, Dst: TIDType): Boolean;
begin
  Result := Dst.DataTypeID in [dtPointer, dtNativeInt, dtNativeUInt];
end;

function TIDOpExplicitClassOfFromAny.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
begin
  if Dst.DataTypeID in [dtPointer, dtNativeInt, dtNativeUInt] then
    Result := Src
  else
    Result := nil;
end;

{ TIDOpImplicitArrayToAny }

function TIDOpImplicitArrayToAny.Check(const Src, Dst: TIDType): Boolean;
var
  ElType: TIDType;
begin
  ElType := (Src as TIDArray).ElementDataType;
  Result := (Dst.DataTypeID = dtPointer);
end;

{ TIDOpExplictPointerFromAny }

function TIDOpExplictPointerFromAny.Check(const Src, Dst: TIDType): Boolean;
begin
  Result := Dst.Ordinal or (Dst.DataTypeID in [dtPointer]);
end;

{ TSysExplicitAnsiStringFromAny }

function TSysExplicitAnsiStringFromAny.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
begin
  if Src.IsConstant and Src.DataType.Ordinal then
    Result := Src
  else
    Result := nil;
end;

end.
