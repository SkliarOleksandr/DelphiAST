unit AST.Delphi.SysOperators;

interface

uses AST.Delphi.Classes,
     AST.Delphi.Declarations,
     AST.Delphi.Contexts;

type

  TSysOperator = class(TIDOperator)
  protected
  //  constructor CreateAsIntOp; reintroduce;
  end;

  TSysTypeCast = class(TSysOperator)
  public
    //constructor CreateInternal(ResultType: TIDType); reintroduce;
    function Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean; overload; virtual;
    function Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; overload; virtual;
    function Match(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; virtual;
  end;

  TSysOpImplicit = class(TSysTypeCast)
  end;

  TSysOpExplisit = class(TSysTypeCast)
  end;

  TSysOpBinary = class(TSysOperator)
    function Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression; virtual; abstract;
  end;


  ///////////////////////////////////////////////////////////////////////////////////////////
  /// ANY TYPE CAST
  ///////////////////////////////////////////////////////////////////////////////////////////
  TSysTypeCast_IsOrdinal = class(TSysTypeCast)
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  ///////////////////////////////////////////////////////////////////////////////////////////
  /// IMPLICIT
  ///////////////////////////////////////////////////////////////////////////////////////////

  {implicit String -> AnsiString}
  TSysImplicitStringToAnsiString = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Class -> Class}
  TSysImplicitClassToClass = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {implicit String -> PChar}
  TSysImplicitStringToPChar = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit String <- PChar}
  TSysImplicitStringFromPChar = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit String -> TGUID}
  TSysImplicitStringToGUID = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit ShortString <- Any}
  TSysImplicitShortStringFromAny = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {implicit AnsiString <- Any}
  TSysImplicitAnsiStringFromAny = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {implicit String <- Any}
  TSysImplicitStringFromAny = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {implicit AnsiString -> String}
  TSysImplicitAnsiStringToString = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Char -> String}
  TSysImplicitCharToString = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Char -> AnsiString}
  TSysImplicitCharToAnsiString = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Char -> AnsiChar}
  TSysImplicitCharToAnsiChar = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit AnsiChar -> AnsiString}
  TSysImplicitAnsiCharToAnsiString = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit AnsiChar -> String}
  TSysImplicitAnsiCharToString = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit AnsiChar -> WideChar}
  TSysImplicitAnsiCharToWideChar = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit MetaClass -> TGUID}
  TSysImplicitMetaClassToGUID = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Closure -> TMethod}
  TSysImplicitClosureToTMethod = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Variant <- Any}
  TSysImplicitVariantFromAny = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Variant -> Any}
  TSysImplicitVariantToAny = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Untyped <- Any}
  TSysImplicitUntypedFromAny = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean; override;
  end;

  {implicit Array -> Any}
  TSysImplicitArrayToAny = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {implicit Array <- Any}
  TSysImplicitArrayFromAny = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit Pointer -> Any}
  TSysImplicitPointerToAny = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {implicit Pointer <- Any}
  TSysImplicitPointerFromAny = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const ASrc: TIDType; const ADst: TIDType): Boolean; override;
  end;

  {implicit Range <- Any}
  TSysImplicitRangeFromAny = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {implicit Range -> Any}
  TSysImplicitRangeToAny = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {implicit Set <- Any}
  TSysImplicitSetFromAny = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Match(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {implicit nil -> Any}
  TSysImplicitNullPtrToAny = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {implicit TVarRec -> Any}
  TSysImplicitTVarRecToAny = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {implicit TVarRec <- Any}
  TSysImplicitTVarRecFromAny = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {implicit TVarRec <- Any}
  TSysImplicitSysVarFromAny = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
  end;

  {implicit TProc <- Any}
  TSysImplicitTProcFromAny = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;


  ///////////////////////////////////////////////////////////////////////////////////////////
  /// EXPLICIT
  ///////////////////////////////////////////////////////////////////////////////////////////

  {explicit Enum -> Any}
  TSysExplicitEnumToAny = class(TSysOpExplisit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {explicit Enum <- Any}
  TSysExplicitEnumFromAny = class(TSysOpExplisit)
  public
    function Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean; override;
  end;

  {explicit TProc <- Any}
  TSysExplicitTProcFromAny = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const ASrc: TIDType; const ADst: TIDType): Boolean; override;
  end;

  {explicit Interface <- Any}
  TSysExplicitInterfaceFromAny = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {explicit Class of -> Any}
  TSysExplicitClassOfToAny = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {explicit Class of <- Any}
  TSysExplicitClassOfFromAny = class(TSysOpExplisit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {explicit AnsiString <- Any}
  TSysExplicitAnsiStringFromAny = class(TSysOpExplisit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
    function Match(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; override;
  end;

  {explicit String <- Any}
  TSysExplicitStringFromAny = class(TSysOpExplisit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {explicit RefType -> Any}
  TSysExplicitRefTypeToAny = class(TSysOpExplisit)
  public
    function Check(const SContext: TSContext; const ASrc: TIDType; const ADst: TIDType): Boolean; override;
  end;

  {explicit RefType <- Any}
  TSysExplicitRefTypeFromAny = class(TSysOpExplisit)
  public
    function Check(const SContext: TSContext; const ASrc: TIDType; const ADst: TIDType): Boolean; override;
  end;

  {explicit Untyped Ref -> Any}
  TSysExplicitUntypedRefToAny = class(TSysOpExplisit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {explicit Untyped -> Any}
  TSysExplicitUntypedToAny = class(TSysOpExplisit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {explicit Char -> Any}
  TSysExplicitCharToAny = class(TSysOpExplisit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {explicit Range <- Any}
  TSysExplicitRangeFromAny = class(TSysOpExplisit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {explicit Record <- Any}
  TSysExplicitRecordFromAny = class(TSysOpExplisit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {explicit Record -> Any}
  TSysExplicitRecordToAny = class(TSysOpExplisit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {explicit StaticArray -> Any}
  TSysExplicitStaticArrayToAny = class(TSysOpExplisit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {explicit DynArray -> Any}
  TSysExplicitDynArrayToAny = class(TSysOpExplisit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {explicit Variant -> Any}
  TSysExplicitVariantToAny = class(TSysOpExplisit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {explicit Variant <- Any}
  TSysExplicitVariantFromAny = class(TSysOpExplisit)
  public
    function Check(const SContext: TSContext; const ASrc: TIDType; const ADst: TIDType): Boolean; override;
  end;

  {explicit Class -> Any}
  TSysExplicitClassToAny = class(TSysExplicitRefTypeToAny)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {explicit Class <- Any}
  TSysExplicitClassFromAny = class(TSysExplicitRefTypeFromAny)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;


  ///////////////////////////////////////////////////////////////////////////////////////////
  /// BINARY
  ///////////////////////////////////////////////////////////////////////////////////////////

  {operator Ordinal IN Set}
  TSysOrdinalInSet = class(TSysOpBinary)
  public
    function Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression; override;
  end;

  {operator ptr IntDiv int}
  TSys_Ptr_IntDiv_Int = class(TSysOpBinary)
  public
    function Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression; override;
  end;

  {operator ptr IntDiv int}
  TSys_StaticArray_Add = class(TSysOpBinary)
  public
    function Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression; override;
  end;

  {operator Set + Set}
  TSys_Add_Set = class(TSysOpBinary)
  public
    function Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression; override;
  end;

  {operator Set - Set}
  TSys_Subtract_Set = class(TSysOpBinary)
  public
    function Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression; override;
  end;

  {operator Set * Set}
  TSys_Multiply_Set = class(TSysOpBinary)
  public
    function Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression; override;
  end;

  {operator Set = Set}
  TSys_Equal_Set = class(TSysOpBinary)
  public
    function Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression; override;
  end;

  {operator Set <> Set}
  TSys_NotEqual_Set = class(TSysOpBinary)
  public
    function Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression; override;
  end;

  {operator [...] = [...]}
  TSys_Equal_DynArray = class(TSysOpBinary)
  public
    function Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression; override;
  end;

  {operator any = nullptr}
  TSys_Equal_NullPtr = class(TSysOpBinary)
  public
    function Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression; override;
  end;

  {operator any = nullptr}
  TSys_NotEqual_NullPtr = class(TSysOpBinary)
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

{ TSysOperator }

//constructor TSysOperator.CreateAsIntOp;
//begin
//  CreateFromPool;
//  ItemType := itSysOperator;
//end;

{ TSysTypeCast }

function TSysTypeCast.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Check(SContext, Src.DataType.ActualDataType, Dst) then
    Result := Dst
  else
    Result := nil;
end;

function TSysTypeCast.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := False;
end;

//constructor TSysTypeCast.CreateInternal(ResultType: TIDType);
//begin
//  CreateFromPool;
//  ItemType := itSysOperator;
//  Self.DataType := ResultType;
//end;

function TSysTypeCast.Match(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
begin
  if Check(SContext, Src, Dst) <> nil then
    Result := TIDCastExpression.Create(Src, Dst)
  else
    Result := nil;
end;

{ TSysImplicitStringToAnsiString }

function TSysImplicitStringToAnsiString.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Src.IsConstant then
  begin
    if IsAnsiString(Src.AsStrConst.Value) then
      Exit(Dst);
  end;
  Result := Dst;
end;

{ TSysImplicitAnsiStringToString }

function TSysImplicitAnsiStringToString.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  Result := Self;
end;

{ TSysImplicitCharToString }

function TSysImplicitCharToString.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Src.IsConstant then
    Exit(SYSUnit._UnicodeString)
  else
    Result := Self;
end;

{ TSysImplicitCharToAnsiString }

function TSysImplicitCharToAnsiString.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Src.IsConstant then
    Exit(SYSUnit._AnsiString)
  else
    Result := Self;
end;

{ TSysImplicitCharToAnsiChar }

function TSysImplicitCharToAnsiChar.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Src.IsConstant then
    Exit(SYSUnit._AnsiChar)
  else
    Result := Self;
end;

{ TSysImplicitAnsiCharToAnsiString }

function TSysImplicitAnsiCharToAnsiString.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  Result := Dst;
end;

{ TSysImplicitAnsiCharToString }

function TSysImplicitAnsiCharToString.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  Result := nil;
end;

{ TSysImplicitAnsiCharToWideChar }

function TSysImplicitAnsiCharToWideChar.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  Result := Dst;
end;

{ TSysImplicitMetaClassToGUID }

function TSysImplicitMetaClassToGUID.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Src.AsType.DataTypeID <> dtInterface then
    TASTDelphiErrors.INTF_TYPE_REQUIRED(Src);

  Result := Dst;
end;

{ TSysImplicitStringToGUID }

function TSysImplicitStringToGUID.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
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

{ TSysImplicitClosureToTMethod }

function TSysImplicitClosureToTMethod.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  Result := nil;
end;

{ TSysImplicitVariantFromAny }

function TSysImplicitVariantFromAny.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Src.DataTypeID in [dtInt8, dtInt16, dtInt32, dtInt64, dtUInt8, dtUInt16, dtUInt32, dtUInt64, dtBoolean,
                        dtFloat32, dtFloat64, dtFloat80, dtCurrency, dtComp, dtNativeInt, dtNativeUInt, dtChar, dtAnsiChar,
                        dtString, dtWideString, dtAnsiString, dtInterface] then
    Result := Self
  else
    Result := nil;
end;

{ TSysImplicitVariantToAny }

function TSysImplicitVariantToAny.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if (Dst.DataTypeID in [dtInt8, dtInt16, dtInt32, dtInt64, dtUInt8, dtUInt16, dtUInt32, dtUInt64, dtBoolean,
                         dtFloat32, dtFloat64, dtFloat80, dtCurrency, dtNativeInt, dtNativeUInt, dtChar, dtAnsiChar,
                         dtString, dtAnsiString, dtWideString, dtVariant, dtInterface]) or (Dst = SYSUnit._TVarData) then
    Result := Self
  else
    Result := nil;
end;

{ TSysExplicitEnumFromAny }

function TSysExplicitEnumFromAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := Src.IsOrdinal;
end;

{ TSysImplicitTProcFromAny }

function TSysImplicitTProcFromAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := (Src.DataTypeID = dtProcType) or
            (
              (Src.DataTypeID = dtPointer) and (Dst as TIDProcType).IsStatic
            );
end;

function TSysImplicitTProcFromAny.Check(const SContext: TSContext; const Src: TIDExpression;
  const Dst: TIDType): TIDDeclaration;
begin
  var LDstProcType := Dst as TIDProcType;

  if Src.ItemType = itProcedure then
  begin
    var LSrcProc := Src.AsProcedure;
    repeat
      if TASTDelphiUnit.StrictMatchProcSingnatures(LSrcProc.ExplicitParams,
                                                   LDstProcType.Params,
                                                   LSrcProc.ResultType,
                                                   LDstProcType.ResultType) then
        Exit(Dst);
      LSrcProc := LSrcProc.PrevOverload;
    until not Assigned(LSrcProc);
  end else
  if Src.DataTypeID = dtProcType then
  begin
    var LSrcProcType := Src.DataType as TIDProcType;
    if TASTDelphiUnit.StrictMatchProcSingnatures(LSrcProcType.Params,
                                                 LDstProcType.Params,
                                                 LSrcProcType.ResultType,
                                                 LDstProcType.ResultType) then
      Exit(Dst);
  end else
  begin
    if (Src.DataTypeID = dtPointer) and LDstProcType.IsStatic then
      Exit(Dst);
  end;

  Result := nil;
end;

{ TSysExplicitTProcFromAny }

function TSysExplicitTProcFromAny.Check(const SContext: TSContext; const ASrc, ADst: TIDType): Boolean;
begin
  var IsRecordWithTheSameSize := (ASrc.DataTypeID in [dtRecord, dtStaticArray]) and
                                 (ASrc.DataSize = Package.PointerSize);
  case TIDProcType(ADst).ProcClass of

    procStatic: Result := ASrc.IsOrdinal or
                          ASrc.IsReferenced or
                          IsRecordWithTheSameSize;

    procMethod: Result := (
                            (ASrc.DataTypeID = dtProcType) and
                            (TIDProcType(ASrc).ProcClass = procMethod)
                          );

    procReference: Result := ASrc.IsOrdinal or
                             ASrc.IsReferenced and
                             not (ASrc.DataTypeID = dtClass) or
                             IsRecordWithTheSameSize;
  else
    Result := False;
  end;
end;

{ TSysExplicitEnumToAny }

function TSysExplicitEnumToAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := Dst.IsOrdinal;
end;

{ TSysImplicitStringToPChar }

function TSysImplicitStringToPChar.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  Result := Dst;
end;

{ TSysImplicitUntypedFromAny }

function TSysImplicitUntypedFromAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := True;
end;

{ TIDOpExplicitClassOfToPointer }

function TSysExplicitClassOfToAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := Dst.DataTypeID in [dtClassOf, dtPointer];
end;

{ TSysImplicitPointerToAny }

function TSysImplicitPointerToAny.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Dst.DataTypeID in [dtPointer, dtPAnsiChar, dtPWideChar, dtClassOf, dtClass] then
    Result := Dst
  else
    Result := nil;
end;

function TSysImplicitPointerToAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := (Dst.DataTypeID = dtPointer);
end;

{ TSysExplicitClassOfFromAny }

function TSysExplicitClassOfFromAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := Dst.DataTypeID in [dtPointer, dtNativeInt, dtNativeUInt];
end;

{ TSysImplicitArrayToAny }

function TSysImplicitArrayToAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
var
  SrcElType, DstElType: TIDType;
begin
  SrcElType := (Src as TIDArray).ElementDataType.ActualDataType;
  if Dst is TIDArray then
  begin
    DstElType := TIDArray(Dst).ElementDataType.ActualDataType;
    Result := ({(Dst.DataTypeID = dtOpenArray) and} SameTypes(SrcElType, DstElType)) or
               (SrcElType.IsGeneric and DstElType.IsGeneric);
  end else begin
    Result :=
      ((Dst = SYSUnit._PAnsiChar) and (SrcElType = SYSUnit._AnsiChar)) or
      ((Dst = SYSUnit._PWideChar) and (SrcElType = SYSUnit._WideChar));
  end;
end;

{ TSysImplicitArrayFromAny }

function TSysImplicitArrayFromAny.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Src.IsDynArrayConst then
    Result := TASTDelphiUnit.CheckConstDynArrayImplicit(SContext, Src, Dst)
  else
    Result := nil;
end;

{ TSysExplicitRefTypeFromAny }

function TSysExplicitRefTypeFromAny.Check(const SContext: TSContext; const ASrc, ADst: TIDType): Boolean;
begin
  Result := ASrc.IsOrdinal or
            (ASrc.DataTypeID in [dtPointer, dtClass, dtClassOf, dtInterface,
                                 dtWideString, dtString, dtAnsiString, dtDynArray]) or
            ((ASrc.DataTypeID in [dtRecord, dtStaticArray]) and (ASrc.DataSize = Package.PointerSize));
end;

{ TSysExplicitAnsiStringFromAny }

function TSysExplicitAnsiStringFromAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := (SYSUnit._PAnsiChar = Src) or Src.IsReferenced;
end;

function TSysExplicitAnsiStringFromAny.Match(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
begin
  if Src.IsConstant and Src.DataType.IsOrdinal then
    Result := Src
  else
    Result := inherited;
end;

{ TSysExplicitStringFromAny }

function TSysExplicitStringFromAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := (Src.DataTypeID in [dtAnsiString, dtWideString, dtPointer, dtUntypedRef, dtPAnsiChar, dtPWideChar]) or
            (
              (Src.DataTypeID = dtStaticArray) and
                (
                  (TIDArray(Src).ElementDataType.DataTypeID = dtChar) or
                  (TIDArray(Src).ElementDataType.DataTypeID = dtAnsiChar)
                )
            );
end;

{ TSysExplicitRefTypeToAny }

function TSysExplicitRefTypeToAny.Check(const SContext: TSContext; const ASrc, ADst: TIDType): Boolean;
begin
  Result := ADst.IsOrdinal or (ADst.DataTypeID in [dtPointer,
                                                   dtPAnsiChar,
                                                   dtPWideChar,
                                                   dtProcType,
                                                   dtRecord,
                                                   dtClass,
                                                   dtInterface,
                                                   dtDynArray,
                                                   dtAnsiString,
                                                   dtString,
                                                   dtWideString]);
  if not Result then
    Result := False;
end;

{ TSysOrdinalInSet }

function TSysOrdinalInSet.Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression;
begin
  if (Right.Declaration is TIDDynArrayConstant) or (Right.DataTypeID = dtSet) then
  begin
    Result := SYSUnit._TrueExpression; // tmp
  end else
    Result := nil;
end;


{ TSysImplicitStringFromPChar }

function TSysImplicitStringFromPChar.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  Result := Src.DataType;
end;

{ TSys_Ptr_IntDiv_Int }

function TSys_Ptr_IntDiv_Int.Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression;
begin
  if Left.DataTypeID = dtPointer then
    Result := SYSUnit._ZeroIntExpression // tmp
  else
    Result := nil;
end;

{ TSysExplicitUntypedRefToAny }

function TSysExplicitUntypedRefToAny.Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean;
begin
  // does Delphi support explicit cast to any type?
  Result := True;// Dst.IsOrdinal or (Dst.DataTypeID in [dtPointer, dtStaticArray, dtClass, dtInterface, dtString, dtWideString]);
end;

{ TSysExplicitUntypedToAny }

function TSysExplicitUntypedToAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  // Delphi supports explicit cast to any type from Untyped
  Result :=True;
end;


{ TSysExplicitCharToAny }

function TSysExplicitCharToAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := True;
end;

{ TSys_CharArray_Add_Int }

function TSys_StaticArray_Add.Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression;
begin
  if (Left.DataTypeID = dtStaticArray) and (Right.DataType.IsInteger) then
  begin
    var ADT := TIDArray(Left.DataType);
    if ADT.ElementDataType = SYSUnit._WideChar then
    begin
      Result := TIDExpression.Create(GetTMPVar(SYSUnit._WideChar.DefaultReference));
      Exit;
    end;
    if ADT.ElementDataType = SYSUnit._AnsiChar then
    begin
      Result := TIDExpression.Create(GetTMPVar(SYSUnit._AnsiChar.DefaultReference));
      Exit;
    end;
  end;
  Result := nil;
end;

{ TSysImplicitAnsiStringFromAny }

function TSysImplicitAnsiStringFromAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := (Src.DataTypeID = dtStaticArray) and
            (TIDArray(Src).ElementDataType.DataTypeID in [dtAnsiChar]);
end;

{ TSysImplicitShortStringFromAny }

function TSysImplicitShortStringFromAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := (Src.DataTypeID = dtStaticArray) and
            (TIDArray(Src).ElementDataType.DataTypeID in [dtAnsiChar]);
end;

{ TSysImplicitStringFromAny }

function TSysImplicitStringFromAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := (Src.DataTypeID = dtAnsiString) or
            (Src.DataTypeID in [dtPointer, dtPAnsiChar, dtPWideChar]) or
            (
              (Src.DataTypeID = dtStaticArray) and (TIDArray(Src).ElementDataType.DataTypeID = dtChar)
            ) or
            (
              (Src.DataTypeID = dtStaticArray) and
              (TIDArray(Src).ElementDataType.DataTypeID in [dtAnsiChar, dtChar])
            );
end;

{ TSysImplicitClassToClass }

function TSysImplicitClassToClass.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := (Src.DataTypeID = dtClass) and TIDClass(Src).IsInheritsForm(TIDClass(Dst));
end;

{ TSysExplicitRangeFromAny }

function TSysExplicitRangeFromAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := Src.IsOrdinal;
end;

{ TSysExplicitRecordFromAny }

function TSysExplicitRecordFromAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := Dst.DataSize = Src.DataSize;
end;

{ TSysImplicitRangeFromAny }

function TSysImplicitRangeFromAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := Src.IsOrdinal;
end;

{ TSysImplicitRangeToAny }

function TSysImplicitRangeToAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := Dst.DataSize >= Src.DataSize;
end;

{ TSysTypeCast_IsOrdinal }

function TSysTypeCast_IsOrdinal.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := Src.IsOrdinal;
end;

{ TSysImplicitSetFromAny }

function TSysImplicitSetFromAny.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  Result := nil;
  if Src.IsDynArrayConst then
    Result := TASTDelphiUnit.CheckConstDynArrayImplicit(SContext, Src, Dst)
  else
  if Src.DataTypeId = dtSet then
  begin
// todo: the rule is to comple for now)
//    var DstSetType := TIDSet(Dst);
//    var SrcSetType := TIDSet(Src.DataType);
//    if (Src.Declaration = SYSUnit.SystemDeclarations._EmptyArrayConstant) or
//       (DstSetType.BaseType.ActualDataType = SrcSetType.BaseType.ActualDataType) then
      Exit(Dst);
  end;
end;

function TSysImplicitSetFromAny.Match(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
begin
  Result := nil;
  if Src.IsDynArrayConst then
  begin
    var Implicit := TASTDelphiUnit.CheckConstDynArrayImplicit(SContext, Src, Dst);
    if Assigned(Implicit) then
    begin
      var Decl := TIDSetConstant.CreateAsAnonymous(SContext.Scope, Dst, Src.AsDynArrayConst.Value);
      Result := TIDExpression.Create(Decl, Src.TextPosition);
    end;
  end else
  if Src.DataTypeId = dtSet then
    Result := Src;
end;

{ TSysImplicitNullPtrToAny }

function TSysImplicitNullPtrToAny.Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean;
begin
  Result := Dst.IsReferenced or (Dst.DataTypeID = dtProcType);
end;

{ TSysImplicitPointerFromAny }

function TSysImplicitPointerFromAny.Check(const SContext: TSContext; const ASrc, ADst: TIDType): Boolean;
begin
  Result := (
    (
      (ADst.DataTypeID in [dtPAnsiChar]) and
      (ASrc.DataTypeID = dtStaticArray) and
      (TIDStaticArray(ASrc).ElementDataType = SYSUnit._AnsiChar)
    ) or
    (
      (ADst.DataTypeID in [dtPWideChar]) and
      (ASrc.DataTypeID = dtStaticArray) and
      (TIDStaticArray(ASrc).ElementDataType = SYSUnit._WideChar)
    ) or
      (ASrc.DataTypeID in [dtDynArray, dtClass, dtClassOf, dtInterface])
  );
end;

{ TSysExplicitRecordToAny }

function TSysExplicitRecordToAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := Src.DataSize = Dst.DataSize;
end;

{ TSysExplicitStaticArrayToAny }

function TSysExplicitStaticArrayToAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := Src.DataSize = Dst.DataSize;
end;

{ TSysExplicitDynArrayToAny }

function TSysExplicitDynArrayToAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := (Dst.DataTypeID = dtDynArray) and SameTypes(TIDDynArray(Src).ElementDataType,
                                                        TIDDynArray(Dst).ElementDataType);
end;


{ TSys_Add_Set }

function TSys_Add_Set.Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression;
begin
  // todo:
  Result := Left;
end;

{ TSys_Subtract_Set }

function TSys_Subtract_Set.Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression;
begin
  // todo:
  Result := Left;
end;

{ TSys_Multiply_Set }

function TSys_Multiply_Set.Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression;
begin
  // todo:
  Result := Left;
end;

{ TSys_Equal_Set }

function TSys_Equal_Set.Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression;
begin
  // todo:
  Result := SYSUnit._TrueExpression;
end;

{ TSys_NotEqual_Set }

function TSys_NotEqual_Set.Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression;
begin
  // todo:
  Result := SYSUnit._TrueExpression;
end;

{ TSys_Equal_DynArray }

function TSys_Equal_DynArray.Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression;
begin
  // todo:
  Result := SYSUnit._TrueExpression;
end;

{ TSysImplicitTVarRecToAny }

function TSysImplicitTVarRecToAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  case Dst.DataTypeID of
    dtInt8, dtInt16, dtInt32, dtInt64, dtUInt8, dtUInt16, dtUInt32, dtUInt64,
    dtNativeInt, dtNativeUInt, dtFloat32, dtFloat64, dtFloat80, dtCurrency, dtBoolean,
    dtAnsiChar, dtChar, dtAnsiString, dtString, dtShortString, dtPAnsiChar, dtPWideChar,
    dtClass, dtClassOf, dtInterface: Result := True;
  else
    Result := False;
  end;
end;

{ TSysImplicitTVarRecFromAny }

function TSysImplicitTVarRecFromAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  case Src.DataTypeID of
    dtInt8, dtInt16, dtInt32, dtInt64, dtUInt8, dtUInt16, dtUInt32, dtUInt64,
    dtNativeInt, dtNativeUInt, dtFloat32, dtFloat64, dtFloat80, dtCurrency, dtBoolean,
    dtAnsiChar, dtChar, dtAnsiString, dtString, dtShortString, dtPAnsiChar, dtPWideChar,
    dtClass, dtClassOf, dtInterface: Result := True;
  else
    Result := False;
  end;
end;

{ TSysImplicitSysVarFromAny }

function TSysImplicitSysVarFromAny.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  var ASysVar := Dst as TIDSysVariativeType;
  for var AType in ASysVar.Types do
  begin
    Result := SYSUnit.CheckImplicit(SContext, Src, Dst);
    if Assigned(Result) then
      Exit;
  end;
  Result := nil;
end;

{ TSysExplicitClassToAny }

function TSysExplicitClassToAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  if Dst.DataTypeID = dtGeneric then
  begin
    var LConstraint := TIDGenericParam(Dst).Constraint;
    var LConstrType := TIDGenericParam(Dst).ConstraintType;
    Result := (LConstraint in [gsClass, gsConstructor, gsClassAndConstructor]) or
              (Assigned(LConstrType) and (LConstrType.DataTypeID = dtClass));
  end else
    Result := inherited Check(SContext, Src, Dst);
end;

{ TSysExplicitClassFromAny }

function TSysExplicitClassFromAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := inherited Check(SContext, Src, Dst);
end;

{ TSysExplicitInterfaceFromAny }

function TSysExplicitInterfaceFromAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := (Dst.DataTypeID in [dtInt32, dtPointer, dtClass]) or (Src = SysUnit._Untyped);
end;

{ TSysExplicitVariantToAny }

function TSysExplicitVariantToAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := (Dst.DataTypeID in [dtInt8, dtInt16, dtInt32, dtInt64, dtUInt8, dtUInt16, dtUInt32, dtUInt64, dtBoolean,
                                dtFloat32, dtFloat64, dtNativeInt, dtNativeUInt, dtChar, dtAnsiChar,
                                dtString, dtAnsiString, dtWideString, dtVariant]) or (Dst = SYSUnit._TVarData);
end;

{ TSysExplicitVariantFromAny }

function TSysExplicitVariantFromAny.Check(const SContext: TSContext; const ASrc, ADst: TIDType): Boolean;
begin
  var LRecordWithTheSameSize := (ASrc.DataTypeID in [dtRecord, dtStaticArray]) and
                                (ASrc.DataSize = Package.VariantSize);

  Result := LRecordWithTheSameSize or
            ASrc.IsOrdinal or
            ASrc.IsFloat or
            (ASrc.DataTypeID in [dtString, dtAnsiString, dtWideString, dtDynArray]);
end;

{ TSys_Equal_NullPtr }

function TSys_Equal_NullPtr.Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression;
begin
  // todo:
  Result := SYSUnit._TrueExpression;
end;

{ TSys_NotEqual_NullPtr }

function TSys_NotEqual_NullPtr.Match(const SContext: TSContext; const Left, Right: TIDExpression): TIDExpression;
begin
  // todo:
  Result := SYSUnit._TrueExpression;
end;

end.


