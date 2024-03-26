unit AST.Delphi.Intf;

interface

uses AST.Intf, AST.Delphi.DataTypes, AST.Delphi.Classes, AST.Delphi.Errors;

type

  PDelphiSystemDeclarations = ^TDelphiSystemDeclarations;

  IASTDelphiUnit = interface(IASTModule)
    ['{1A57EA5B-8EA8-4AC7-A885-85E8C959F89E}']
    function GetSystemDeclarations: PDelphiSystemDeclarations;
    function GetErrors: TASTDelphiErrors;
    property SystemDeclarations: PDelphiSystemDeclarations read GetSystemDeclarations;
    property Errors: TASTDelphiErrors read GetErrors;
  end;

  TDelphiSystemDeclarations = record
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
    _NullPtrType: TIDType;
    _PointerType: TIDPointer;
    _UntypedReference: TIDUntypedRef;
    _Untyped: TIDType;
    _MetaType: TIDType;
    _Void: TIDType;
    _GuidType: TIDStructure;
    _PAnsiChar: TIDType;
    _PWideChar: TIDType;
    _OrdinalType: TIDType;
    _TObject: TIDClass;
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
    function GetTypeByID(DataTypeID: TDataTypeID): TIDType;
    property DataTypes[DataTypeID: TDataTypeID]: TIDType read GetTypeByID;
  end;

  IDelphiBuiltInTypes = interface
  ['{5BDBE355-E105-49E7-A9D2-0683390F6ED8}']
    function Get_Int8: TIDType;
    function Get_Int16: TIDType;
    function Get_Int32: TIDType;
    function Get_Int64: TIDType;
    function Get_UInt8: TIDType;
    function Get_UInt16: TIDType;
    function Get_UInt32: TIDType;
    function Get_UInt64: TIDType;
    function Get_NativeInt: TIDType;
    function Get_NativeUInt: TIDType;
    // floating point types
    function Get_Float32: TIDType;
    function Get_Float64: TIDType;
    function Get_Float80: TIDType;
    function Get_Currency: TIDType;
    function Get_Comp: TIDType;
    function Get_Real: TIDType;
    // other
    function Get_Boolean: TIDType;
    function Get_AnsiChar: TIDType;
    function Get_WideChar: TIDType;
    function Get_AnsiString: TIDType;
    function Get_OpenString: TIDString;
    function Get_UnicodeString: TIDType;
    function Get_ShortString: TIDType;
    function Get_WideString: TIDType;
    function Get_Variant: TIDType;
    function Get_NullPtrType: TIDType;
    function Get_PointerType: TIDPointer;
    function Get_UntypedReference: TIDUntypedRef;
    function Get_Untyped: TIDType;
    function Get_MetaType: TIDType;
    function Get_Void: TIDType;
    function Get_GuidType: TIDStructure;
    function Get_PAnsiChar: TIDType;
    function Get_PWideChar: TIDType;
    function Get_OrdinalType: TIDType;
    function Get_TObject: TIDClass;
    function Get_Exception: TIDClass;
    function Get_EAssertClass: TIDClass;
    function Get_TTypeKind: TIDEnum;
    function Get_DateTimeType: TIDType;
    function Get_DateType: TIDType;
    function Get_TimeType: TIDType;
    function Get_EmptySetType: TIDSet;
    function Get_ResStringRecord: TIDType;
    function Get_TVarRec: TIDType;

    property _Int8: TIDType read Get_Int8;
    property _Int16: TIDType read Get_Int16;
    property _Int32: TIDType read Get_Int32;
    property _Int64: TIDType read Get_Int64;
    property _UInt8: TIDType read Get_UInt8;
    property _UInt16: TIDType read Get_UInt16;
    property _UInt32: TIDType read Get_UInt32;
    property _UInt64: TIDType read Get_UInt64;
    property _NativeInt: TIDType read Get_NativeInt;
    property _NativeUInt: TIDType read Get_NativeUInt;
    // floating point types
    property _Float32: TIDType read Get_Float32;
    property _Float64: TIDType read Get_Float64;
    property _Float80: TIDType read Get_Float80;
    property _Currency: TIDType read Get_Currency;
    property _Comp: TIDType read Get_Comp;
    property _Real: TIDType read Get_Real;
    // other
    property _Boolean: TIDType read Get_Boolean;
    property _AnsiChar: TIDType read Get_AnsiChar;
    property _WideChar: TIDType read Get_WideChar;
    property _AnsiString: TIDType read Get_AnsiString;
    property _OpenString: TIDString read Get_OpenString;
    property _UnicodeString: TIDType read Get_UnicodeString;
    property _ShortString: TIDType read Get_ShortString;
    property _WideString: TIDType read Get_WideString;
    property _Variant: TIDType read Get_Variant;
    property _NullPtrType: TIDType read Get_NullPtrType;
    property _PointerType: TIDPointer read Get_PointerType;
    property _UntypedReference: TIDUntypedRef read Get_UntypedReference;
    property _Untyped: TIDType read Get_Untyped;
    property _MetaType: TIDType read Get_MetaType;
    property _Void: TIDType read Get_Void;
    property _GuidType: TIDStructure read Get_GuidType;
    property _PAnsiChar: TIDType read Get_PAnsiChar;
    property _PWideChar: TIDType read Get_PWideChar;
    property _OrdinalType: TIDType read Get_OrdinalType;
    property _TObject: TIDClass read Get_TObject;
    property _Exception: TIDClass read Get_Exception;
    property _EAssertClass: TIDClass read Get_EAssertClass;
    property _TTypeKind: TIDEnum read Get_TTypeKind;
    property _DateTimeType: TIDType read Get_DateTimeType;
    property _DateType: TIDType read Get_DateType;
    property _TimeType: TIDType read Get_TimeType;
    property _EmptySetType: TIDSet read Get_EmptySetType;
    property _ResStringRecord: TIDType read Get_ResStringRecord;
    property _TVarRec: TIDType read Get_TVarRec;
  end;


  IASTDelphiSystemUnit = interface(IASTDelphiUnit)
  end;


implementation

{ TDelphiSystemTypes }

function TDelphiSystemDeclarations.GetTypeByID(DataTypeID: TDataTypeID): TIDType;
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

end.
