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
    function Check(const Src, Dst: TIDType): Boolean; overload; virtual;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; overload; virtual;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; virtual;
  end;

  TSysExplisit = class(TSysImplicit)
  end;

  ///////////////////////////////////////////////////////////////////////////////////////////
  ///  IMPLICIT
  ///////////////////////////////////////////////////////////////////////////////////////////

  {implicit String -> AnsiString}
  TIDOpImplicitStringToAnsiString = class(TSysImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit AnsiString -> String}
  TIDOpImplicitAnsiStringToString = class(TSysImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Char -> String}
  TIDOpImplicitCharToString = class(TSysImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Char -> AnsiString}
  TIDOpImplicitCharToAnsiString = class(TSysImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Char -> AnsiChar}
  TIDOpImplicitCharToAnsiChar = class(TSysImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {implicit AnsiChar -> AnsiString}
  TIDOpImplicitAnsiCharToAnsiString = class(TSysImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit AnsiChar -> String}
  TIDOpImplicitAnsiCharToString = class(TSysImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit AnsiChar -> Char}
  TIDOpImplicitAnsiCharToChar = class(TSysImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit String -> AnsiString}
  TIDOpImplicitStringToPChar = class(TSysImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {implicit MetaClass -> TGUID}
  TIDOpImplicitMetaClassToGUID = class(TSysImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit String -> TGUID}
  TIDOpImplicitStringToGUID = class(TSysImplicit)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Closure -> TMethod}
  TIDOpImplicitClosureToTMethod = class(TSysImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit DynArray -> Set}
  TIDOpImplicitDynArrayToSet = class(TSysImplicit)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Any -> Variant}
  TIDOpImplicitAnyToVariant = class(TSysImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Variant -> Any}
  TIDOpImplicitVariantToAny = class(TSysImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Variant -> Any}
  TIDOpImplicitAnyToUntyped = class(TSysImplicit)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Array -> Any}
  TIDOpImplicitArrayToAny = class(TSysImplicit)
  public
    function Check(const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {implicit Pointer -> Any}
  TIDOpImplicitPointerToAny = class(TSysImplicit)
  private
    class var fInstance: TSysImplicit;
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Check(const Src: TIDType; const Dst: TIDType): Boolean; override;
    class function Instance: TIDInternalOperator;
  end;

  ///////////////////////////////////////////////////////////////////////////////////////////
  ///  EXPLICIT
  ///////////////////////////////////////////////////////////////////////////////////////////

  {explicit Int -> Enum}
  TIDOpExplicitIntToEnum = class(TSysImplicit)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {explicit Any -> TProc}
  TIDOpExplicitTProcFromAny = class(TSysImplicit)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Check(const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {explicit Class of -> Pointer type}
  TIDOpExplicitClassOfToAny = class(TSysImplicit)
  public
    function Check(const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {explicit Class of <- Any}
  TIDOpExplicitClassOfFromAny = class(TSysExplisit)
  public
    function Check(const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {explicit Enum -> Any}
  TSysExplicitEnumToAny = class(TSysExplisit)
  private
    class var fInstance: TSysImplicit;
  public
    function Check(const Src: TIDType; const Dst: TIDType): Boolean; override;
    class function Instance: TIDInternalOperator;
  end;

  {explicit AnsiString <- Any}
  TSysExplicitAnsiStringFromAny = class(TSysExplisit)
  private
    class var fInstance: TSysImplicit;
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    class function Instance: TIDInternalOperator;
  end;

  {explicit Pointer -> Any}
  TIDOpExplictPointerToAny = class(TSysExplisit)
  private
    class var fInstance: TSysImplicit;
  public
    function Check(const Src: TIDType; const Dst: TIDType): Boolean; override;
    class function Instance: TIDInternalOperator;
  end;

  {explicit Pointer <- Any}
  TIDOpExplictPointerFromAny = class(TSysExplisit)
  private
    class var fInstance: TSysImplicit;
  public
    function Check(const Src: TIDType; const Dst: TIDType): Boolean; override;
    class function Instance: TIDInternalOperator;
  end;


implementation

uses NPCompiler.DataTypes,
     NPCompiler.Utils,
     AST.Delphi.Parser,
     AST.Delphi.Errors,
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

function TSysImplicit.Check(const Src, Dst: TIDType): Boolean;
begin
  Result := False;
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

{ TIDOpImplicitAnsiStringToString }

function TIDOpImplicitAnsiStringToString.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  Result := Self;
end;

{ TIDOpImplicitCharToString }

function TIDOpImplicitCharToString.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Src.IsConstant then
    Exit(SYSUnit._String)
  else
    Result := Self;
end;

{ TIDOpImplicitCharToAnsiString }

function TIDOpImplicitCharToAnsiString.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Src.IsConstant then
    Exit(SYSUnit._AnsiString)
  else
    Result := Self;
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

{ TIDOpImplicitAnsiCharToString }

function TIDOpImplicitAnsiCharToString.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  Result := nil;
end;

{ TIDOpImplicitAnsiCharToChar }

function TIDOpImplicitAnsiCharToChar.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  Result := nil;
end;

{ TIDInternalCastOperator }

function TIDOpImplicitMetaClassToGUID.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Src.AsType.DataTypeID <> dtInterface then
    TASTDelphiUnit.ERROR_INTF_TYPE_REQUIRED(Src);

  Result := Dst;
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

{ TIDOpImplicitDynArrayToSet }

function TIDOpImplicitDynArrayToSet.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
var
  Expr: TIDExpression;
begin
  if Src.IsDynArrayConst then
  begin
    Expr := TASTDelphiUnit.ConstDynArrayToSet(Src, Dst as TIDSet);
    Result := Expr.DataType;
  end else
    Result := nil;
end;

function TIDOpImplicitDynArrayToSet.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
begin
  if Src.IsDynArrayConst then
  begin
    Result := TASTDelphiUnit.ConstDynArrayToSet(Src, Dst as TIDSet);
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

{ TIDOpExplicitIntToEnum }

function TIDOpExplicitIntToEnum.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Src.DataTypeID in [dtInt8, dtInt16, dtInt32, dtInt64, dtUInt8, dtUInt16, dtUInt32, dtUInt64] then
    Result := Dst
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
    if TASTDelphiUnit.StrictMatchProcSingnatures(SrcProc.ExplicitParams, DstProcType.Params, SrcProc.ResultType, DstProcType.ResultType) then
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
    fInstance := Self.CreateAsIntOp();
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

function TIDOpExplicitClassOfToAny.Check(const Src, Dst: TIDType): Boolean;
begin
  Result := Dst.DataTypeID = dtPointer;
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

class function TIDOpImplicitPointerToAny.Instance: TIDInternalOperator;
begin
  if not Assigned(fInstance) then
    fInstance := Self.CreateAsIntOp();
  Result := fInstance;
end;

{ TIDOpExplicitClassOfFromAny }

function TIDOpExplicitClassOfFromAny.Check(const Src, Dst: TIDType): Boolean;
begin
  Result := Dst.DataTypeID in [dtPointer, dtNativeInt, dtNativeUInt];
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

class function TIDOpExplictPointerFromAny.Instance: TIDInternalOperator;
begin
  if not Assigned(fInstance) then
    fInstance := Self.CreateAsIntOp();
  Result := fInstance;
end;

{ TSysExplicitAnsiStringFromAny }

class function TSysExplicitAnsiStringFromAny.Instance: TIDInternalOperator;
begin
  if not Assigned(fInstance) then
    fInstance := Self.CreateAsIntOp();
  Result := fInstance;
end;

function TSysExplicitAnsiStringFromAny.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
begin
  if Src.IsConstant and Src.DataType.Ordinal then
    Result := Src
  else
    Result := nil;
end;

{ TIDOpExplictPointerToAny }

function TIDOpExplictPointerToAny.Check(const Src, Dst: TIDType): Boolean;
begin
  Result := Dst.Ordinal or (Dst.DataTypeID in [dtPointer, dtProcType, dtRecord]);
end;

class function TIDOpExplictPointerToAny.Instance: TIDInternalOperator;
begin
  if not Assigned(fInstance) then
    fInstance := Self.CreateAsIntOp();
  Result := fInstance;
end;

end.
