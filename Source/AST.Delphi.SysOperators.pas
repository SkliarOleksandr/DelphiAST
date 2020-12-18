unit AST.Delphi.SysOperators;

interface

uses AST.Delphi.Classes, AST.Delphi.Contexts;

type

  TSysOperator = class(TIDOperator)
  protected
    constructor CreateAsIntOp; reintroduce;
  end;

  TSysTypeCast = class(TSysOperator)
  public
    constructor CreateInternal(ResultType: TIDType); reintroduce;
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

  {implicit AnsiChar -> Char}
  TSysImplicitAnsiCharToChar = class(TSysOpImplicit)
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

  {implicit Pointer -> Any}
  TSysImplicitPointerToAny = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {implicit Pointer <- Any}
  TSysImplicitPointerFromAny = class(TSysOpImplicit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {implicit Range <- Any}
  TSysImplicitRangeFromAny = class(TSysOpImplicit)
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
    function Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; override;
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

  {explicit Pointer -> Any}
  TSysExplictPointerToAny = class(TSysOpExplisit)
  public
    function Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean; override;
  end;

  {explicit Pointer <- Any}
  TSysExplictPointerFromAny = class(TSysOpExplisit)
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


implementation

uses AST.Delphi.DataTypes,
     AST.Parser.Utils,
     AST.Delphi.Parser,
     AST.Delphi.Errors,
     AST.Pascal.Parser,
     AST.Delphi.System;

{ TSysOperator }

constructor TSysOperator.CreateAsIntOp;
begin
  CreateFromPool;
  ItemType := itSysOperator;
end;

{ TSysTypeCast }

function TSysTypeCast.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Check(SContext, Src.DataType, Dst) then
    Result := Dst
  else
    Result := nil;
end;

function TSysTypeCast.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := False;
end;

constructor TSysTypeCast.CreateInternal(ResultType: TIDType);
begin
  CreateFromPool;
  ItemType := itSysOperator;
  Self.DataType := ResultType;
end;

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
  Result := nil;
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
    Exit(SYSUnit._String)
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
  Result := nil;
end;

{ TSysImplicitAnsiCharToString }

function TSysImplicitAnsiCharToString.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  Result := nil;
end;

{ TSysImplicitAnsiCharToChar }

function TSysImplicitAnsiCharToChar.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  Result := nil;
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
                        dtFloat32, dtFloat64, dtNativeInt, dtNativeUInt, dtChar, dtAnsiChar,
                        dtString, dtAnsiString] then
    Result := Self
  else
    Result := nil;
end;

{ TSysImplicitVariantToAny }

function TSysImplicitVariantToAny.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Dst.DataTypeID in [dtInt8, dtInt16, dtInt32, dtInt64, dtUInt8, dtUInt16, dtUInt32, dtUInt64, dtBoolean,
                        dtFloat32, dtFloat64, dtNativeInt, dtNativeUInt, dtChar, dtAnsiChar,
                        dtString, dtAnsiString, dtVariant] then
    Result := Self
  else
    Result := nil;
end;

{ TSysExplicitEnumFromAny }

function TSysExplicitEnumFromAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := Src.IsOrdinal;
end;

{ TSysExplicitTProcFromAny }

function TSysExplicitTProcFromAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := (Src.DataTypeID = dtProcType) or
            (
              (Src.DataTypeID = dtPointer) and (Dst as TIDProcType).IsStatic
            );
end;

function TSysExplicitTProcFromAny.Check(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Src.ItemType = itProcedure then
  begin
    var SrcProc := Src.AsProcedure;
    var DstProcType := Dst as TIDProcType;
    if TASTDelphiUnit.StrictMatchProcSingnatures(SrcProc.ExplicitParams, DstProcType.Params, SrcProc.ResultType, DstProcType.ResultType) then
      Exit(Dst)
    else
      Exit(nil);
  end;

  if (Src.DataTypeID = dtPointer) and (Dst as TIDProcType).IsStatic then
    Result := Dst
  else
    Result := nil;
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
  if Dst.DataTypeID in [dtPointer, dtPAnsiChar, dtPWideChar] then
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
  SrcElType := TIDArray(Src).ElementDataType;
  if Dst is TIDArray then
  begin
    DstElType := TIDArray(Dst).ElementDataType;
    Result := (Dst.DataTypeID = dtOpenArray) and (SrcElType = DstElType);
  end else begin
    Result :=
      ((Dst = SYSUnit._PAnsiChar) and (SrcElType = SYSUnit._AnsiChar)) or
      ((Dst = SYSUnit._PChar) and (SrcElType = SYSUnit._Char));
  end;
end;

{ TSysExplictPointerFromAny }

function TSysExplictPointerFromAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := Src.IsOrdinal or (Src.DataTypeID in [dtPointer, dtClass, dtInterface, dtWideString, dtString, dtAnsiString, dtDynArray]);

  if (Src.DataTypeID = dtRecord) and (Src.DataSize = 4) then   // todo: platform
    Result := True;
end;

{ TSysExplicitAnsiStringFromAny }

function TSysExplicitAnsiStringFromAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := SYSUnit._PAnsiChar = Src;
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
  Result := (Src.DataTypeID = dtAnsiString) or
            (Src.DataTypeID in [dtPointer, dtPAnsiChar, dtPWideChar]) or
            (
              (Src.DataTypeID = dtStaticArray) and (TIDArray(Src).ElementDataType.DataTypeID = dtChar)
            );
end;

{ TSysExplictPointerToAny }

function TSysExplictPointerToAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := Dst.IsOrdinal or (Dst.DataTypeID in [dtPointer,
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

{ TSysExplicitUntypedToAny }

function TSysExplicitUntypedToAny.Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean;
begin
  Result := (Dst.DataTypeID in [dtPointer, dtClass, dtInterface]);
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
    if ADT.ElementDataType = SYSUnit._Char then
    begin
      Result := TIDExpression.Create(GetTMPVar(SYSUnit._Char.DefaultReference));
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

{ TSysImplicitStringFromAny }

function TSysImplicitStringFromAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := (Src.DataTypeID = dtAnsiString) or
            (Src.DataTypeID in [dtPointer, dtPAnsiChar, dtPWideChar]) or
            (
              (Src.DataTypeID = dtStaticArray) and (TIDArray(Src).ElementDataType.DataTypeID = dtChar)
            );
end;

{ TSysImplicitClassToClass }

function TSysImplicitClassToClass.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := TIDClass(Src).IsInheritsForm(TIDClass(Dst));
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
    Result := TASTDelphiUnit.MatchConstDynArrayImplicit(SContext, Src, Dst);
end;

function TSysImplicitSetFromAny.Match(const SContext: TSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
begin
  Result := nil;
  if Src.IsDynArrayConst then
  begin
    var Implicit := TASTDelphiUnit.MatchConstDynArrayImplicit(SContext, Src, Dst);
    if Assigned(Implicit) and (Src.DataTypeID <> dtSet) then
    begin
      var Decl := TIDSetConstant.CreateAsAnonymous(SContext.Scope, Dst, Src.AsDynArrayConst.Value);
      Result := TIDExpression.Create(Decl, Src.TextPosition);
    end;
  end;
end;

{ TSysImplicitNullPtrToAny }

function TSysImplicitNullPtrToAny.Check(const SContext: TSContext; const Src: TIDType; const Dst: TIDType): Boolean;
begin
  Result := Dst.IsReferenced or (Dst.DataTypeID = dtProcType);
end;

{ TSysImplicitPointerFromAny }

function TSysImplicitPointerFromAny.Check(const SContext: TSContext; const Src, Dst: TIDType): Boolean;
begin
  Result := (
    ((Dst.DataTypeID in [dtPAnsiChar]) and (Src.DataTypeID = dtStaticArray) and (TIDStaticArray(Src).ElementDataType = SYSUnit._AnsiChar)) or
    ((Dst.DataTypeID in [dtPWideChar]) and (Src.DataTypeID = dtStaticArray) and (TIDStaticArray(Src).ElementDataType = SYSUnit._Char))
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

end.


