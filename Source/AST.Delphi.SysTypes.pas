unit AST.Delphi.SysTypes;

interface

uses
  AST.Classes,
  AST.Pascal.Intf,
  AST.Delphi.DataTypes,
  AST.Delphi.Operators,
  AST.Delphi.Declarations,
  AST.Delphi.Classes;

type
  TDelphiBuiltinTypes = class(TInterfacedObject, IDelphiBuiltInTypes)
  private
    FProject: IASTPascalProject;
    F_TVariantArg: TIDType;
  public
    // integer types
    _Int8: TIDType;
    _Int16: TIDType;
    _Int32: TIDType;
    _Int64: TIDType;
    _UInt8: TIDType;
    _UInt16: TIDType;
    _UInt32: TIDType;
    _UInt64: TIDType;
    _NativeInt: TIDType;
    _NativeUInt: TIDType;
    // floating point types
    _Float32: TIDType;
    _Float64: TIDType;
    _Float80: TIDType;
    _Real48: TIDType;
    _Currency: TIDType;
    _Comp: TIDType;
    _Real: TIDType;
    // other
    _Boolean: TIDType;
    _AnsiChar: TIDType;
    _WideChar: TIDType;
    _AnsiString: TIDType;
    _OpenString: TIDString;
    _UnicodeString: TIDType;
    _ShortString: TIDType;
    _WideString: TIDType;
    _Variant: TIDType;
    _OleVariant: TIDType;
    _NullPtrType: TIDType;
    _PointerType: TIDPointer;
    _UntypedReference: TIDUntypedRef;
    _Untyped: TIDType;
    _UnknownType: TIDUnknown;
    _UnknownVariable: TIDVariable;
    _UnknownConstant: TIDConstant;
    _UnknownRangeConstant: TIDRangeConstant;
    _UnknownProcedure: TIDProcedure;
    _MetaType: TIDType;
    _Void: TIDType;
    _GuidType: TIDStructure;
    _PAnsiChar: TIDType;
    _PWideChar: TIDType;
    _OrdinalType: TIDType;
    _TObject: TIDClass;
    _IInterface: TIDInterface;
    _Exception: TIDClass;
    _EAssertClass: TIDClass;
    _TTypeKind: TIDEnum;
    _DateTimeType: TIDType;
    _DateType: TIDType;
    _TimeType: TIDType;
    _True: TIDBooleanConstant;
    _False: TIDBooleanConstant;
    _TrueExpression: TIDExpression;
    _FalseExpression: TIDExpression;
    _ZeroConstant: TIDIntConstant;
    _ZeroIntExpression: TIDExpression;
    _ZeroFloatExpression: TIDExpression;
    _OneConstant: TIDIntConstant;
    _OneExpression: TIDExpression;
    _MaxIntConstant: TIDIntConstant;
    _MaxIntExpression: TIDExpression;
    _NullPtrConstant: TIDIntConstant;
    _NullPtrExpression: TIDExpression;
    _EmptyStrConstant: TIDStringConstant;
    _EmptyStrExpression: TIDExpression;
    _DeprecatedDefaultStr: TIDStringConstant;
    _EmptySetType: TIDSet;
    _EmptyArrayConstant: TIDDynArrayConstant;
    _ResStringRecord: TIDType;
    _TVarRec: TIDType;
    _TVarData: TIDType;
    _tagVARIANT: TIDType;
  private
    function Get_Int8: IASTDelphiType;
    function Get_Int16: IASTDelphiType;
    function Get_Int32: IASTDelphiType;
    function Get_Int64: IASTDelphiType;
    function Get_UInt8: IASTDelphiType;
    function Get_UInt16: IASTDelphiType;
    function Get_UInt32: IASTDelphiType;
    function Get_UInt64: IASTDelphiType;
    function Get_NativeInt: IASTDelphiType;
    function Get_NativeUInt: IASTDelphiType;
    // floating point types
    function Get_Float32: IASTDelphiType;
    function Get_Float64: IASTDelphiType;
    function Get_Float80: IASTDelphiType;
    function Get_Currency: IASTDelphiType;
    function Get_Comp: IASTDelphiType;
    function Get_Real: IASTDelphiType;
    // other
    function Get_Boolean: IASTDelphiType;
    function Get_AnsiChar: IASTDelphiType;
    function Get_WideChar: IASTDelphiType;
    function Get_AnsiString: IASTDelphiType;
    function Get_OpenString: IASTDelphiType;
    function Get_UnicodeString: IASTDelphiType;
    function Get_ShortString: IASTDelphiType;
    function Get_WideString: IASTDelphiType;
    function Get_Variant: IASTDelphiType;
    function Get_NullPtrType: IASTDelphiType;
    function Get_PointerType: IASTDelphiType;
    function Get_UntypedReference: IASTDelphiType;
    function Get_Untyped: IASTDelphiType;
    function Get_MetaType: IASTDelphiType;
    function Get_Void: IASTDelphiType;
    function Get_GuidType: IASTDelphiType;
    function Get_PAnsiChar: IASTDelphiType;
    function Get_PWideChar: IASTDelphiType;
    function Get_OrdinalType: IASTDelphiType;
    function Get_TObject: IASTDelphiType;
    function Get_Exception: IASTDelphiType;
    function Get_EAssertClass: IASTDelphiType;
    function Get_TTypeKind: IASTDelphiType;
    function Get_DateTimeType: IASTDelphiType;
    function Get_DateType: IASTDelphiType;
    function Get_TimeType: IASTDelphiType;
    function Get_EmptySetType: IASTDelphiType;
    function Get_ResStringRecord: IASTDelphiType;
    function Get_TVarRec: IASTDelphiType;
    function Get_TVariantArg: TIDType;
  public
    constructor Create(const AProject: IASTPascalProject);

    function GetTypeByID(DataTypeID: TDataTypeID): TIDType;
    property DataTypes[DataTypeID: TDataTypeID]: TIDType read GetTypeByID;

    property _TVariantArg: TIDType read Get_TVariantArg;
  end;

  PDelphiSystemDeclarations = TDelphiBuiltinTypes;

  TBuiltin_IntType = class(TIDOrdinal)
  protected
    function GetIsInteger: Boolean; override;
  public
    procedure SetupOperators(ADecls: PDelphiSystemDeclarations); virtual; abstract;
    function SysBinOperator(ALeft, ARight: TIDType): TIDType;
    function SysBinarOperatorLeft(AOpID: TOperatorID; ARight: TIDType): TIDType; override;
    function SysBinarOperatorRight(AOpID: TOperatorID; ALeft: TIDType): TIDType; override;
    function MatchImplicitTo(ADst: TIDType): Boolean; override;
    function MatchImplicitFrom(ASrc: TIDType): Boolean; override;
  end;

  TBuiltin_FltType = class(TIDType)
  protected
    function GetIsFloat: Boolean; override;
  public
    procedure SetupOperators(ADecls: PDelphiSystemDeclarations); virtual; abstract;
    function SysBinarOperatorLeft(AOpID: TOperatorID; ARight: TIDType): TIDType; override;
    function SysBinarOperatorRight(AOpID: TOperatorID; ALeft: TIDType): TIDType; override;
    function MatchImplicitTo(ADst: TIDType): Boolean; override;
    function MatchImplicitFrom(ASrc: TIDType): Boolean; override;
  end;

  TBuiltin_StrType = class(TIDString)
  public
    procedure SetupOperators(ADecls: PDelphiSystemDeclarations); virtual; abstract;
    function SysBinarOperatorLeft(AOpID: TOperatorID; ARight: TIDType): TIDType; override;
    function SysBinarOperatorRight(AOpID: TOperatorID; ALeft: TIDType): TIDType; override;
  end;

  TBuiltin_TypedPointer = class(TIDPointer)
    function SysBinarOperatorLeft(AOpID: TOperatorID; ARight: TIDType): TIDType; override;
    function SysBinarOperatorRight(AOpID: TOperatorID; ALeft: TIDType): TIDType; override;
  end;

  TBuiltin_Variant = class(TIDVariant)
  public
    function SysUnarOperator(AOpID: TOperatorID): TIDType; override;
    function SysBinarOperatorLeft(AOpID: TOperatorID; ARight: TIDType): TIDType; override;
    function SysBinarOperatorRight(AOpID: TOperatorID; ALeft: TIDType): TIDType; override;
    function MatchExplicitTo(ADst: TIDType): Boolean; override;
    function MatchExplicitFrom(ASrc: TIDType): Boolean; override;
  end;

  TBuiltin_OleVariant = class(TBuiltin_Variant)
  end;


//  TST_Byte = class(TSys_IntType)
//  public
//    constructor CreateAsBuiltin(ASystem: TASTModule); override;
//    procedure CreateStandardOperators; override;
//  end;
//
//  TST_ShortInt = class(TSys_IntType)
//  public
//    constructor CreateAsBuiltin(ASystem: TASTModule); override;
//    procedure CreateStandardOperators; override;
//  end;
   TBuiltin_Boolean = class(TBuiltin_IntType)
   end;

   TBuiltin_UnicodeChar = class(TBuiltin_IntType)
   end;

   TBuiltin_AnsiChar = class(TBuiltin_IntType)
   end;

  TBuiltin_Extended = class(TBuiltin_FltType)
  public
    procedure SetupOperators(ADecls: PDelphiSystemDeclarations); override;
  end;

  TBuiltin_Currency = class(TBuiltin_FltType)
  public
    procedure SetupOperators(ADecls: PDelphiSystemDeclarations); override;
  end;

  TBuiltin_Comp = class(TBuiltin_FltType)
  public
    procedure SetupOperators(ADecls: PDelphiSystemDeclarations); override;
  end;

  TBuiltin_Real48 = class(TBuiltin_FltType)
  protected
    function GetDataSize: Integer; override;
  end;

  TBuiltin_OpenString = class(TBuiltin_StrType)
  public
    procedure SetupOperators(ADecls: PDelphiSystemDeclarations); override;
  end;

  TBuiltin_AnsiString = class(TBuiltin_StrType)
  public
    function MatchImplicitTo(ADst: TIDType): Boolean; override;
    function MatchImplicitFrom(ASrc: TIDType): Boolean; override;
  end;

  TBuiltin_UnicodeString = class(TBuiltin_StrType)
  public
    function MatchImplicitTo(ADst: TIDType): Boolean; override;
    function MatchImplicitFrom(ASrc: TIDType): Boolean; override;
  end;

  TBuiltin_WideString = class(TBuiltin_StrType)
  end;

  TBuiltin_ShortString = class(TBuiltin_AnsiString)

  end;

  TBuiltin_PAnsiChar = class(TBuiltin_TypedPointer)
  public
    function SysBinarOperatorLeft(AOpID: TOperatorID; ARight: TIDType): TIDType; override;
    function SysBinarOperatorRight(AOpID: TOperatorID; ALeft: TIDType): TIDType; override;
  end;

  TBuiltin_PWideChar = class(TBuiltin_TypedPointer)
  public
    function SysBinarOperatorLeft(AOpID: TOperatorID; ARight: TIDType): TIDType; override;
    function SysBinarOperatorRight(AOpID: TOperatorID; ALeft: TIDType): TIDType; override;
  end;



implementation

uses
  AST.Parser.Utils,
  AST.Delphi.Parser,
  AST.Delphi.System;

{ TST_Byte }

//constructor TST_Byte.CreateAsBuiltin(ASystem: TASTModule);
//begin
//  inherited CreateAsSystem(TASTDelphiUnit(ASystem).IntfScope, 'Byte');
//  DataTypeID := dtUInt8;
//  LowBound := 0;
//  HighBound := MaxUInt8;
//end;
//
//procedure TST_Byte.CreateStandardOperators;
//begin
//  inherited;
//
//end;

{ TST_ShortInt }
//
//constructor TST_ShortInt.CreateAsBuiltin(ASystem: TASTModule);
//begin
//  inherited CreateAsSystem(TASTDelphiUnit(ASystem).IntfScope, 'Byte');
//  DataTypeID := dtInt8;
//  LowBound := MinInt8;
//  HighBound := MaxInt8;
//end;
//
//procedure TST_ShortInt.CreateStandardOperators;
//begin
//  inherited;
//
//end;

{ TBuiltin_Comp }

procedure TBuiltin_Comp.SetupOperators(ADecls: PDelphiSystemDeclarations);
begin
  // Comparisons
  OverloadAllCmpOperators;
  // Implicits
//  OverloadImplicitTo(ADecls._Float32);
//  OverloadImplicitTo(ADecls._Float64);
//  OverloadImplicitTo(ADecls._Float80);
//  OverloadImplicitTo(ADecls._Currency);



 // OverloadImplicitTo(ADecls._Variant, Operators.ImplicitVariantFromAny);

end;

{ TBuiltin_Extended }

procedure TBuiltin_Extended.SetupOperators(ADecls: PDelphiSystemDeclarations);
begin
  OverloadBinarOperator2(opDivide, Self, Self);
end;

{ TBuiltin_Currency }

procedure TBuiltin_Currency.SetupOperators(ADecls: PDelphiSystemDeclarations);
begin
  // Comparisons
  OverloadAllCmpOperators;

end;

{ TBuiltin_StrType }

function TBuiltin_StrType.SysBinarOperatorLeft(AOpID: TOperatorID; ARight: TIDType): TIDType;
begin
  case AOpID of
    opEqual, opNotEqual: Result := SYSUnit._Boolean;
    opAdd: begin
      if ARight.DataTypeID in [dtString, dtAnsiString, dtWideString, dtChar, dtAnsiChar] then
        Result := Self
      else
        Result := nil;
    end;
  else
    Result := inherited;
  end;
end;

function TBuiltin_StrType.SysBinarOperatorRight(AOpID: TOperatorID; ALeft: TIDType): TIDType;
begin
  Result := SysBinarOperatorLeft(AOpID, ALeft);
end;

{ TBuiltin_OpenString }

procedure TBuiltin_OpenString.SetupOperators(ADecls: PDelphiSystemDeclarations);
begin
  // Comparisons
  OverloadAllCmpOperators;

end;

{ TBuiltin_AnsiString }

function TBuiltin_AnsiString.MatchImplicitFrom(ASrc: TIDType): Boolean;
begin
  Result := (ASrc.DataTypeID in [dtString, dtWideString, dtAnsiString, dtAnsiChar, dtChar]);
end;

function TBuiltin_AnsiString.MatchImplicitTo(ADst: TIDType): Boolean;
begin
  Result := (ADst.DataTypeID in [dtString, dtWideString, dtAnsiString]) or
            ((ADst.DataTypeID = dtStaticArray) and (TIDStaticArray(ADst).ElementDataType.DataTypeID = dtAnsiChar));
end;

{ TBuiltin_UnicodeString }

function TBuiltin_UnicodeString.MatchImplicitFrom(ASrc: TIDType): Boolean;
begin
  Result := (ASrc.DataTypeID in [dtAnsiString, dtAnsiChar, dtChar]);
end;

function TBuiltin_UnicodeString.MatchImplicitTo(ADst: TIDType): Boolean;
begin
  Result := (ADst.DataTypeID in [dtString, dtAnsiString]);
end;

{ TBuiltin_IntType }

function TBuiltin_IntType.GetIsInteger: Boolean;
begin
  Result := True;
end;

function TBuiltin_IntType.MatchImplicitFrom(ASrc: TIDType): Boolean;
begin
  Result := ASrc.IsInteger;
end;

function TBuiltin_IntType.MatchImplicitTo(ADst: TIDType): Boolean;
begin
  Result := ADst.IsInteger or ADst.IsFloat;
end;

function TBuiltin_IntType.SysBinarOperatorLeft(AOpID: TOperatorID; ARight: TIDType): TIDType;
begin
  if ARight.IsInteger then
  begin
    case AOpID of
      opEqual,
      opNotEqual,
      opGreater,
      opGreaterOrEqual,
      opLess,
      opLessOrEqual: Result := SYSUnit._Boolean;
      opAnd, opOr, opXor: Result := SysBinOperator(Self, ARight);
    else
      Result := Self;
    end;
  end else
    Result := nil;
end;

function TBuiltin_IntType.SysBinarOperatorRight(AOpID: TOperatorID; ALeft: TIDType): TIDType;
begin
  if ALeft.IsInteger then
  begin
    case AOpID of
      opEqual,
      opNotEqual,
      opGreater,
      opGreaterOrEqual,
      opLess,
      opLessOrEqual: Result := SYSUnit._Boolean;
      opAnd, opOr, opXor: Result := SysBinOperator(ALeft, Self);
    else
      Result := Self;
    end;
  end else
    Result := nil;
end;

function TBuiltin_IntType.SysBinOperator(ALeft, ARight: TIDType): TIDType;
begin
  if ALeft.DataSize >= ARight.DataSize then
    Result := ARight
  else
    Result := ALeft;
end;

{ TBuiltin_FltType }

function TBuiltin_FltType.GetIsFloat: Boolean;
begin
  Result := True;
end;

function TBuiltin_FltType.MatchImplicitFrom(ASrc: TIDType): Boolean;
begin
  Result := ASrc.IsFloat or ASrc.IsInteger;
end;

function TBuiltin_FltType.MatchImplicitTo(ADst: TIDType): Boolean;
begin
  Result := ADst.IsFloat;
end;

function TBuiltin_FltType.SysBinarOperatorLeft(AOpID: TOperatorID; ARight: TIDType): TIDType;
begin
  case AOpID of
    opEqual,
    opNotEqual,
    opGreater,
    opGreaterOrEqual,
    opLess,
    opLessOrEqual: begin
      if ARight.IsFloat or ARight.IsInteger then
        Result := SYSUnit._Boolean
      else
        Result := nil;
    end
  else
    if ARight.IsFloat or ARight.IsInteger then
      Result := Self
    else
     Result := nil;
  end;
end;

function TBuiltin_FltType.SysBinarOperatorRight(AOpID: TOperatorID; ALeft: TIDType): TIDType;
begin
  case AOpID of
    opEqual,
    opNotEqual,
    opGreater,
    opGreaterOrEqual,
    opLess,
    opLessOrEqual: begin
      if ALeft.IsFloat or ALeft.IsInteger then
        Result := SYSUnit._Boolean
      else
        Result := nil;
    end;
  else
    if ALeft.IsFloat or ALeft.IsInteger then
      Result := Self
    else
     Result := nil;
  end;
end;

{ TDelphiBuiltinTypes }

function TDelphiBuiltinTypes.Get_AnsiChar: IASTDelphiType;
begin
  Result := _AnsiChar;
end;

function TDelphiBuiltinTypes.Get_AnsiString: IASTDelphiType;
begin
  Result := _AnsiString;
end;

function TDelphiBuiltinTypes.Get_Boolean: IASTDelphiType;
begin
  Result := _Boolean;
end;

function TDelphiBuiltinTypes.Get_Comp: IASTDelphiType;
begin
  Result := _Comp;
end;

function TDelphiBuiltinTypes.Get_Currency: IASTDelphiType;
begin
  Result := _Currency;
end;

function TDelphiBuiltinTypes.Get_DateTimeType: IASTDelphiType;
begin
  Result := _DateTimeType;
end;

function TDelphiBuiltinTypes.Get_DateType: IASTDelphiType;
begin
  Result := _DateType;
end;

function TDelphiBuiltinTypes.Get_EAssertClass: IASTDelphiType;
begin
  Result := _EAssertClass;
end;

function TDelphiBuiltinTypes.Get_EmptySetType: IASTDelphiType;
begin
  Result := _EmptySetType;
end;

function TDelphiBuiltinTypes.Get_Exception: IASTDelphiType;
begin
  Result := _Exception;
end;

function TDelphiBuiltinTypes.Get_Float32: IASTDelphiType;
begin
  Result := _Float32;
end;

function TDelphiBuiltinTypes.Get_Float64: IASTDelphiType;
begin
  Result := _Float64;
end;

function TDelphiBuiltinTypes.Get_Float80: IASTDelphiType;
begin
  Result := _Float80;
end;

function TDelphiBuiltinTypes.Get_GuidType: IASTDelphiType;
begin
  Result := _GuidType;
end;

function TDelphiBuiltinTypes.Get_Int16: IASTDelphiType;
begin
  Result := _Int16;
end;

function TDelphiBuiltinTypes.Get_Int32: IASTDelphiType;
begin
  Result := _Int32;
end;

function TDelphiBuiltinTypes.Get_Int64: IASTDelphiType;
begin
  Result := _Int64;
end;

function TDelphiBuiltinTypes.Get_Int8: IASTDelphiType;
begin
  Result := _Int8;
end;

function TDelphiBuiltinTypes.Get_MetaType: IASTDelphiType;
begin
  Result := _MetaType;
end;

function TDelphiBuiltinTypes.Get_NativeInt: IASTDelphiType;
begin
  Result := _NativeInt;
end;

function TDelphiBuiltinTypes.Get_NativeUInt: IASTDelphiType;
begin
  Result := _NativeUInt;
end;

function TDelphiBuiltinTypes.Get_NullPtrType: IASTDelphiType;
begin
  Result := _NullPtrType;
end;

function TDelphiBuiltinTypes.Get_OpenString: IASTDelphiType;
begin
  Result := _OpenString;
end;

function TDelphiBuiltinTypes.Get_OrdinalType: IASTDelphiType;
begin
  Result := _OrdinalType;
end;

function TDelphiBuiltinTypes.Get_PAnsiChar: IASTDelphiType;
begin
  Result := _PAnsiChar;
end;

function TDelphiBuiltinTypes.Get_PointerType: IASTDelphiType;
begin
  Result := _PointerType;
end;

function TDelphiBuiltinTypes.Get_PWideChar: IASTDelphiType;
begin
  Result := _PWideChar;
end;

function TDelphiBuiltinTypes.Get_Real: IASTDelphiType;
begin
  Result := _Real;
end;

function TDelphiBuiltinTypes.Get_ResStringRecord: IASTDelphiType;
begin
  Result := _ResStringRecord;
end;

function TDelphiBuiltinTypes.Get_ShortString: IASTDelphiType;
begin
  Result := _ShortString;
end;

function TDelphiBuiltinTypes.Get_TimeType: IASTDelphiType;
begin
  Result := _TimeType;
end;

function TDelphiBuiltinTypes.Get_TObject: IASTDelphiType;
begin
  Result := _TObject;
end;

function TDelphiBuiltinTypes.Get_TTypeKind: IASTDelphiType;
begin
  Result := _TTypeKind;
end;

function TDelphiBuiltinTypes.Get_TVariantArg: TIDType;
begin
  if not Assigned(F_TVariantArg) then
    F_TVariantArg := (FProject.FindParsedUnit('Winapi.ActiveX') as TASTDelphiUnit).FindPublicDecl('TVariantArg') as TIDType;

  Result := F_TVariantArg;
end;

function TDelphiBuiltinTypes.Get_TVarRec: IASTDelphiType;
begin
  Result := _TVarRec;
end;

function TDelphiBuiltinTypes.Get_UInt16: IASTDelphiType;
begin
  Result := _UInt16;
end;

function TDelphiBuiltinTypes.Get_UInt32: IASTDelphiType;
begin
  Result := _UInt32;
end;

function TDelphiBuiltinTypes.Get_UInt64: IASTDelphiType;
begin
  Result := _UInt64;
end;

function TDelphiBuiltinTypes.Get_UInt8: IASTDelphiType;
begin
  Result := _UInt8;
end;

function TDelphiBuiltinTypes.Get_UnicodeString: IASTDelphiType;
begin
  Result := _UnicodeString;
end;

function TDelphiBuiltinTypes.Get_Untyped: IASTDelphiType;
begin
  Result := _Untyped;
end;

function TDelphiBuiltinTypes.Get_UntypedReference: IASTDelphiType;
begin
  Result := _UntypedReference;
end;

function TDelphiBuiltinTypes.Get_Variant: IASTDelphiType;
begin
  Result := _Variant;
end;

function TDelphiBuiltinTypes.Get_Void: IASTDelphiType;
begin
  Result := _Void;
end;

function TDelphiBuiltinTypes.Get_WideChar: IASTDelphiType;
begin
  Result := _WideChar;
end;

function TDelphiBuiltinTypes.Get_WideString: IASTDelphiType;
begin
  Result := _WideString;
end;

constructor TDelphiBuiltinTypes.Create(const AProject: IASTPascalProject);
begin
  FProject := AProject;
end;

function TDelphiBuiltinTypes.GetTypeByID(DataTypeID: TDataTypeID): TIDType;
begin
  case DataTypeID of
    dtInt8: Result := _Int8;
    dtInt16: Result := _Int16;
    dtInt32: Result := _Int32;
    dtInt64: Result := _Int64;
    dtUInt8: Result := _UInt8;
    dtUInt16: Result := _UInt16;
    dtUInt32: Result := _UInt32;
    dtUInt64: Result := _UInt64;
    dtNativeInt: Result := _NativeInt;
    dtNativeUInt: Result := _NativeUInt;
    dtFloat32: Result := _Float32;
    dtFloat64: Result := _Float64;
    dtFloat80: Result := _Float80;
    dtCurrency: Result := _Currency;
    dtComp: Result := _Comp;
    dtBoolean: Result := _Boolean;
    dtAnsiChar: Result := _AnsiChar;
    dtChar: Result := _WideChar;
    dtShortString: Result := _ShortString;
    dtAnsiString: Result := _AnsiString;
    dtString: Result := _UnicodeString;
    dtWideString: Result := _WideString;
    dtPAnsiChar: Result := _PAnsiChar;
    dtPWideChar: Result := _PWideChar;
    dtVariant: Result := _Variant;
    dtGuid: Result := _GuidType;
    dtPointer: Result := _PointerType;
  else
    Assert(False, 'Data Type is unknown');
    Result := nil;
  end;
end;

function _IsStrType(AType: TIDType): Boolean;
begin
  Result := AType.DataTypeID in [dtShortString, dtString, dtWideString, dtAnsiString];
end;

function _IsChrType(AType: TIDType): Boolean;
begin
  Result := AType.DataTypeID in [dtAnsiChar, dtChar];
end;

{ TBuiltin_PAnsiChar }

function TBuiltin_PAnsiChar.SysBinarOperatorLeft(AOpID: TOperatorID; ARight: TIDType): TIDType;
begin
  case AOpID of
    opEqual, opNotEqual: begin
      if (ARight = Self) or _IsStrType(ARight) or _IsChrType(ARight) then
        Result := SYSUnit._Boolean
      else
        Result := inherited;
    end;
  else
    Result := inherited;
  end;
end;

function TBuiltin_PAnsiChar.SysBinarOperatorRight(AOpID: TOperatorID; ALeft: TIDType): TIDType;
begin
  case AOpID of
    opEqual, opNotEqual: begin
      if (ALeft = Self) or _IsStrType(ALeft) or _IsChrType(ALeft) then
        Result := SYSUnit._Boolean
      else
        Result := inherited;
    end;
  else
    Result := inherited;
  end;
end;

{ TBuiltin_PWideChar }

function TBuiltin_PWideChar.SysBinarOperatorLeft(AOpID: TOperatorID; ARight: TIDType): TIDType;
begin
  case AOpID of
    opEqual, opNotEqual: begin
      if (ARight = Self) or _IsStrType(ARight) or _IsChrType(ARight) then
        Result := SYSUnit._Boolean
      else
        Result := inherited;
    end;
  else
    Result := inherited;
  end;
end;

function TBuiltin_PWideChar.SysBinarOperatorRight(AOpID: TOperatorID; ALeft: TIDType): TIDType;
begin
  case AOpID of
    opEqual, opNotEqual: begin
      if (ALeft = Self) or _IsStrType(ALeft) or _IsChrType(ALeft) then
        Result := SYSUnit._Boolean
      else
        Result := inherited;
    end;
  else
    Result := inherited;
  end;
end;

{ TBuiltin_Variant }

function TBuiltin_Variant.MatchExplicitFrom(ASrc: TIDType): Boolean;
begin
  Result := (ASrc.DataTypeID in
    [dtInt8, dtInt16, dtInt32, dtInt64, dtUInt8, dtUInt16, dtUInt32, dtUInt64, dtBoolean,
     dtFloat32, dtFloat64, dtNativeInt, dtNativeUInt, dtChar, dtAnsiChar,
     dtString, dtAnsiString, dtWideString, dtVariant]) or
     ((ASrc.DataTypeID = dtRecord) and (TIDRecord(ASrc).DataSize = Package.VariantSize));

  if not Result then
    Result := TIDRecord(ASrc).DataSize = Package.VariantSize;

end;

function TBuiltin_Variant.MatchExplicitTo(ADst: TIDType): Boolean;
begin
  Result := MatchExplicitFrom(ADst);
end;

function TBuiltin_Variant.SysBinarOperatorLeft(AOpID: TOperatorID; ARight: TIDType): TIDType;
begin
  case AOpID of
    opEqual, opNotEqual, opLess, opLessOrEqual, opGreater, opGreaterOrEqual: Result := SYSUnit._Boolean;
  else
    Result := Self;
  end;
end;

function TBuiltin_Variant.SysBinarOperatorRight(AOpID: TOperatorID; ALeft: TIDType): TIDType;
begin
  case AOpID of
    opEqual, opNotEqual, opLess, opLessOrEqual, opGreater, opGreaterOrEqual: Result := SYSUnit._Boolean;
  else
    Result := Self;
  end;
end;

function TBuiltin_Variant.SysUnarOperator(AOpID: TOperatorID): TIDType;
begin
  Result := Self;
end;

{ TBuiltin_Real48 }

function TBuiltin_Real48.GetDataSize: Integer;
begin
  Result := 6;
end;

{ TBuiltin_TypedPointer }

function TBuiltin_TypedPointer.SysBinarOperatorLeft(AOpID: TOperatorID; ARight: TIDType): TIDType;
begin
  case AOpID of
    // pointer subtraction returns the length (integer)
    opSubtract: Result := SYSUnit._NativeInt;
  else
    Result := inherited;
  end;
end;

function TBuiltin_TypedPointer.SysBinarOperatorRight(AOpID: TOperatorID; ALeft: TIDType): TIDType;
begin
  case AOpID of
    // pointer subtraction returns the length (integer)
    opSubtract: Result := SYSUnit._NativeInt;
  else
    Result := inherited;
  end;
end;

end.
