unit AST.Delphi.SysOperators;

interface

uses AST.Delphi.Classes, AST.Delphi.Contexts;

type

  TSysOperator = class(TIDOperator)
  protected
    constructor CreateAsIntOp; reintroduce;
  end;

  TSysOpImplicit = class(TSysOperator)
  public
    constructor CreateInternal(ResultType: TIDType); reintroduce;
    function Check(const Src, Dst: TIDType): Boolean; overload; virtual;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; overload; virtual;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; virtual;
  end;

  // Singleton ancestor
  TSysOpImplicit<TInstanceClass: class> = class(TSysOpImplicit)
  private
    class var fInstance: TInstanceClass;
  public
    class function Instance: TInstanceClass;
  end;

  TSysOpExplisit = class(TSysOpImplicit)
  end;

  // Singleton ancestor
  TSysOpExplisit<TInstanceClass: class> = class(TSysOpExplisit)
  private
    class var fInstance: TInstanceClass;
  public
    class function Instance: TInstanceClass;
  end;

  TSysOpBinary = class(TSysOperator)
    function Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression; virtual; abstract;
  end;

  // Singleton ancestor
  TSysOpBinary<TInstanceClass: class> = class(TSysOpBinary)
  private
    class var fInstance: TInstanceClass;
  public
    class function Instance: TInstanceClass;
  end;

  ///////////////////////////////////////////////////////////////////////////////////////////
  /// IMPLICIT
  ///////////////////////////////////////////////////////////////////////////////////////////

  {implicit String -> AnsiString}
  TIDOpImplicitStringToAnsiString = class(TSysOpImplicit<TIDOpImplicitStringToAnsiString>)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit AnsiString -> String}
  TIDOpImplicitAnsiStringToString = class(TSysOpImplicit<TIDOpImplicitAnsiStringToString>)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Char -> String}
  TIDOpImplicitCharToString = class(TSysOpImplicit<TIDOpImplicitCharToString>)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Char -> AnsiString}
  TIDOpImplicitCharToAnsiString = class(TSysOpImplicit<TIDOpImplicitCharToAnsiString>)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Char -> AnsiChar}
  TIDOpImplicitCharToAnsiChar = class(TSysOpImplicit<TIDOpImplicitCharToAnsiChar>)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {implicit AnsiChar -> AnsiString}
  TIDOpImplicitAnsiCharToAnsiString = class(TSysOpImplicit<TIDOpImplicitAnsiCharToAnsiString>)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit AnsiChar -> String}
  TIDOpImplicitAnsiCharToString = class(TSysOpImplicit<TIDOpImplicitAnsiCharToString>)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit AnsiChar -> Char}
  TIDOpImplicitAnsiCharToChar = class(TSysOpImplicit<TIDOpImplicitAnsiCharToChar>)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit String -> AnsiString}
  TIDOpImplicitStringToPChar = class(TSysOpImplicit<TIDOpImplicitStringToPChar>)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {implicit MetaClass -> TGUID}
  TIDOpImplicitMetaClassToGUID = class(TSysOpImplicit<TIDOpImplicitMetaClassToGUID>)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit String -> TGUID}
  TIDOpImplicitStringToGUID = class(TSysOpImplicit<TIDOpImplicitStringToGUID>)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Closure -> TMethod}
  TIDOpImplicitClosureToTMethod = class(TSysOpImplicit<TIDOpImplicitClosureToTMethod>)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit DynArray -> Set}
  TIDOpImplicitDynArrayToSet = class(TSysOpImplicit<TIDOpImplicitDynArrayToSet>)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Any -> Variant}
  TIDOpImplicitAnyToVariant = class(TSysOpImplicit<TIDOpImplicitAnyToVariant>)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Variant -> Any}
  TIDOpImplicitVariantToAny = class(TSysOpImplicit<TIDOpImplicitVariantToAny>)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Variant -> Any}
  TIDOpImplicitAnyToUntyped = class(TSysOpImplicit<TIDOpImplicitAnyToUntyped>)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Array -> Any}
  TSysImplicitArrayToAny = class(TSysOpImplicit<TSysImplicitArrayToAny>)
  public
    function Check(const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {implicit Pointer -> Any}
  TIDOpImplicitPointerToAny = class(TSysOpImplicit<TIDOpImplicitPointerToAny>)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Check(const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  ///////////////////////////////////////////////////////////////////////////////////////////
  /// EXPLICIT
  ///////////////////////////////////////////////////////////////////////////////////////////

  {explicit Int -> Enum}
  TIDOpExplicitIntToEnum = class(TSysOpImplicit<TIDOpExplicitIntToEnum>)
  public
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {explicit Any -> TProc}
  TIDOpExplicitTProcFromAny = class(TSysOpImplicit<TIDOpExplicitTProcFromAny>)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Check(const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {explicit Class of -> Pointer type}
  TIDOpExplicitClassOfToAny = class(TSysOpImplicit<TIDOpExplicitClassOfToAny>)
  public
    function Check(const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {explicit Class of <- Any}
  TSysExplicitClassOfFromAny = class(TSysOpExplisit<TSysExplicitClassOfFromAny>)
  public
    function Check(const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {explicit Enum -> Any}
  TSysExplicitEnumToAny = class(TSysOpExplisit<TSysExplicitEnumToAny>)
  public
    function Check(const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {explicit AnsiString <- Any}
  TSysExplicitAnsiStringFromAny = class(TSysOpExplisit<TSysExplicitAnsiStringFromAny>)
  public
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {explicit Pointer -> Any}
  TSysExplictPointerToAny = class(TSysOpExplisit<TSysExplictPointerToAny>)
  public
    function Check(const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {explicit Pointer <- Any}
  TSysExplictPointerFromAny = class(TSysOpExplisit<TSysExplictPointerFromAny>)
  public
    function Check(const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  ///////////////////////////////////////////////////////////////////////////////////////////
  /// BINARY
  ///////////////////////////////////////////////////////////////////////////////////////////

  {explicit Pointer <- Any}
  TSysAnsiChar_In = class(TSysOpBinary<TSysAnsiChar_In>)
  public
    function Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression; override;
  end;

implementation

uses AST.Delphi.DataTypes,
     AST.Parser.Utils,
     AST.Delphi.Parser,
     AST.Delphi.Errors,
     AST.Pascal.Parser,
     AST.Delphi.System;

{ TIDInternalOperator }

constructor TSysOperator.CreateAsIntOp;
begin
  CreateFromPool;
end;

{ TSysOpImplicit<TInstanceClass> }

class function TSysOpImplicit<TInstanceClass>.Instance: TInstanceClass;
begin
  Result := fInstance;
  if not Assigned(Result) then
  begin
    TObject(Result) := Self.CreateAsIntOp();
    fInstance := Result;
  end;
end;

{ TSysOpExplisit<TInstanceClass> }

class function TSysOpExplisit<TInstanceClass>.Instance: TInstanceClass;
begin
  Result := fInstance;
  if not Assigned(Result) then
  begin
    TObject(Result) := Self.CreateAsIntOp();
    fInstance := Result;
  end;
end;

{ TSysOpBinary<TInstanceClass> }

class function TSysOpBinary<TInstanceClass>.Instance: TInstanceClass;
begin
  Result := fInstance;
  if not Assigned(Result) then
  begin
    TObject(Result) := Self.CreateAsIntOp();
    fInstance := Result;
  end;
end;

{ TIDInternalOpImplicit }

function TSysOpImplicit.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Check(Src.DataType, Dst) then
    Result := Dst
  else
    Result := nil;
end;

function TSysOpImplicit.Check(const Src, Dst: TIDType): Boolean;
begin
  Result := False;
end;

constructor TSysOpImplicit.CreateInternal(ResultType: TIDType);
begin
  CreateFromPool;
  ItemType := itProcedure;
  Self.DataType := ResultType;
end;

function TSysOpImplicit.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
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

{ TSysExplicitClassOfFromAny }

function TSysExplicitClassOfFromAny.Check(const Src, Dst: TIDType): Boolean;
begin
  Result := Dst.DataTypeID in [dtPointer, dtNativeInt, dtNativeUInt];
end;

{ TSysImplicitArrayToAny }

function TSysImplicitArrayToAny.Check(const Src, Dst: TIDType): Boolean;
var
  ElType: TIDType;
begin
  ElType := (Src as TIDArray).ElementDataType;
  Result := (Dst.DataTypeID = dtPointer);
end;

{ TSysExplictPointerFromAny }

function TSysExplictPointerFromAny.Check(const Src, Dst: TIDType): Boolean;
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

{ TSysExplictPointerToAny }

function TSysExplictPointerToAny.Check(const Src, Dst: TIDType): Boolean;
begin
  Result := Dst.Ordinal or (Dst.DataTypeID in [dtPointer, dtProcType, dtRecord]);
end;

{ TSysAnsiChar_In }

function TSysAnsiChar_In.Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression;
begin
  Result := SYSUnit._TrueExpression; // tmp
end;

end.
