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
    _EmptyArrayConstant: TIDDynArrayConstant;
    _ResStringRecord: TIDType;
    _TVarRec: TIDType;
    function GetTypeByID(DataTypeID: TDataTypeID): TIDType;
    property DataTypes[DataTypeID: TDataTypeID]: TIDType read GetTypeByID;
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
