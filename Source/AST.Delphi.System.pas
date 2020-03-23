unit AST.Delphi.System;

interface

{$i compilers.inc}

uses Classes, SysUtils,
     AST.Pascal.Parser,
     AST.Delphi.Classes,
     AST.Delphi.DataTypes,
     AST.Delphi.Operators,
     AST.Parser.Utils,
     AST.Parser.Messages,
     AST.Classes,
     AST.Project,
     AST.Delphi.SysOperators,
     AST.Delphi.Parser;
     // System

type

  TSYSTEMUnit = class(TASTDelphiUnit)
  type
    TDataTypes = array[TDataTypeID] of TIDType;
  const
    SystemTypesCount = Ord(dtPointer) + 1;
  private
  var
    fDataTypes: TDataTypes;
    fTrueConstant: TIDBooleanConstant;
    fFalseConstant: TIDBooleanConstant;
    fFalseExpression: TIDExpression;
    fTrueExpression: TIDExpression;
    fZeroConstant: TIDIntConstant;
    fZeroExpression: TIDExpression;
    fOneConstant: TIDIntConstant;
    fOneExpression: TIDExpression;
    fNullPtrType: TIDType;
    fNullPtrConstatnt: TIDIntConstant;
    fNullPtrExpression: TIDExpression;
    fEmptyStrConstant: TIDStringConstant;
    fEmptyStrExpression: TIDExpression;
    fPointerType: TIDPointer;
    fUntypedReferenceType: TIDPointer;
    fArrayType: TIDArray; // служебный тип для функций Length/SetLength
    fRefType: TIDType; // служебный тип для функций SizeOf
    fGuidType: TIDStructure;
    fPAnsiChar: TIDType;
    fPChar: TIDType;
    fOrdinalType: TIDType;
    fTObject: TIDClass;
    fException: TIDClass;
    fEAssertClass: TIDClass;
    fDateTimeType: TIDType;
    fDateType: TIDType;
    fTimeType: TIDType;
    fTypeIDType: TIDType;
    fImplicitAnyToVariant: TSysOpImplicit;
    fImplicitVariantToAny: TSysOpImplicit;
    fExplicitEnumFromAny: TSysOpImplicit;
    fExplicitTProcFromAny: TSysOpImplicit;
    fCopyArrayOfObjProc: TIDProcedure;
    fCopyArrayOfStrProc: TIDProcedure;
    fFinalArrayOfObjProc: TIDProcedure;
    fFinalArrayOfStrProc: TIDProcedure;
    fFinalArrayOfVarProc: TIDProcedure;
    fAsserProc: TIDProcedure;
    fWideString: TIDType;
    fShortString: TIDType;
    fOpenString: TIDType;
    fDeprecatedDefaultStr: TIDStringConstant;
    procedure AddImplicists;
    procedure AddExplicists;
    procedure AddNegOperators;
    procedure AddAddOperators;
    procedure AddSubOperators;
    procedure AddMulOperators;
    procedure AddDivOperators;
    procedure AddIntDivOperators;
    procedure AddModOperators;
    procedure AddLogicalOperators;
    procedure AddBitwiseOperators;
    procedure AddCompareOperators;
    procedure AddArithmeticOperators;
    procedure RegisterBuiltinFunctions;
    procedure SystemFixup;
    procedure AddSystemOperators;
    procedure InsertToScope(Declaration: TIDDeclaration); overload;
    function RegisterBuiltin(const Name: string; MacroID: TBuiltInFunctionID; ResultDataType: TIDType; Flags: TProcFlags = []): TIDBuiltInFunction; overload;
    function RegisterType(const TypeName: string; TypeClass: TIDTypeClass; DataType: TDataTypeID): TIDType;
    function RegisterTypeCustom(const TypeName: string; TypeClass: TIDTypeClass; DataType: TDataTypeID): TIDType;
    function RegisterOrdinal(const TypeName: string; DataType: TDataTypeID; LowBound: Int64; HighBound: UInt64): TIDType;
    function RegisterTypeAlias(const TypeName: string; OriginalType: TIDType): TIDAliasType;
    function RegisterPointer(const TypeName: string; TargetType: TIDType): TIDPointer;
    function RegisterConstInt(const Name: string; DataType: TIDType; Value: Int64): TIDIntConstant;
    function RegisterConstStr(Scope: TScope; const Name: string; const Value: string ): TIDStringConstant;
    function RegisterVariable(Scope: TScope; const Name: string; DataType: TIDType): TIDVariable;
    function RegisterBuiltin(const BuiltinClass: TIDBuiltInFunctionClass): TIDBuiltInFunction; overload;
  private
    procedure SearchSystemTypes;
    procedure AddCustomExplicits(const Sources: array of TDataTypeID; Dest: TIDType); overload;
  public
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    constructor Create(const Project: IASTProject; const FileName: string; const Source: string); override;
    function Compile(RunPostCompile: Boolean = True): TCompilerResult; override;
    function CompileIntfOnly: TCompilerResult; override;
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    procedure InitSystemUnit;
    //procedure RegisterExternalProc
    property DataTypes: TDataTypes read fDataTypes;
    property _Int8: TIDType read fDataTypes[dtInt8] write fDataTypes[dtInt8];
    property _Int16: TIDType read fDataTypes[dtInt16] write fDataTypes[dtInt16];
    property _Int32: TIDType read fDataTypes[dtInt32] write fDataTypes[dtInt32];
    property _Int64: TIDType read fDataTypes[dtInt64] write fDataTypes[dtInt64];
    property _UInt8: TIDType read fDataTypes[dtUInt8] write fDataTypes[dtUInt8];
    property _UInt16: TIDType read fDataTypes[dtUInt16] write fDataTypes[dtUInt16];
    property _UInt32: TIDType read fDataTypes[dtUInt32] write fDataTypes[dtUInt32];
    property _UInt64: TIDType read fDataTypes[dtUInt64] write fDataTypes[dtUInt64];
    property _NativeInt: TIDType read fDataTypes[dtNativeInt] write fDataTypes[dtNativeInt];
    property _NativeUInt: TIDType read fDataTypes[dtNativeUInt] write fDataTypes[dtNativeUInt];
    property _Float32: TIDType read fDataTypes[dtFloat32] write fDataTypes[dtFloat32];
    property _Float64: TIDType read fDataTypes[dtFloat64] write fDataTypes[dtFloat64];
    property _Boolean: TIDType read fDataTypes[dtBoolean] write fDataTypes[dtBoolean];
    property _AnsiChar: TIDType read fDataTypes[dtAnsiChar] write fDataTypes[dtAnsiChar];
    property _Char: TIDType read fDataTypes[dtChar] write fDataTypes[dtChar];
    property _AnsiString: TIDType read fDataTypes[dtAnsiString] write fDataTypes[dtAnsiString];
    property _String: TIDType read fDataTypes[dtString] write fDataTypes[dtString];
    property _ShortString: TIDType read fShortString write fShortString;
    property _Variant: TIDType read fDataTypes[dtVariant] write fDataTypes[dtVariant];
    property _NilPointer: TIDType read fNullPtrType;
    property _TGuid: TIDStructure read fGuidType;
    property _True: TIDBooleanConstant read fTrueConstant;
    property _False: TIDBooleanConstant read fFalseConstant;
    property _TrueExpression: TIDExpression read fTrueExpression;
    property _FalseExpression: TIDExpression read fFalseExpression;
    property _ZeroConstant: TIDIntConstant read fZeroConstant;
    property _ZeroExpression: TIDExpression read fZeroExpression;
    property _OneConstant: TIDIntConstant read fOneConstant;
    property _OneExpression: TIDExpression read fOneExpression;
    property _NullPtrConstant: TIDIntConstant read fNullPtrConstatnt;
    property _NullPtrExpression: TIDExpression read fNullPtrExpression;
    property _EmptyStrExpression: TIDExpression read fEmptyStrExpression;
    property _Pointer: TIDPointer read fPointerType;
    property _UntypedReference: TIDPointer read fUntypedReferenceType;
    property _TObject: TIDClass read fTObject;
    property _Exception: TIDClass read fException;
    property _EAssert: TIDClass read fEAssertClass;
    property _DateTime: TIDType read fDateTimeType;
    property _Date: TIDType read fDateType;
    property _Time: TIDType read fTimeType;
    property _CopyArrayOfObjProc: TIDProcedure read fCopyArrayOfObjProc;
    property _CopyArrayOfStrProc: TIDProcedure read fCopyArrayOfStrProc;
    property _FinalArrayOfObjProc: TIDProcedure read fFinalArrayOfObjProc;
    property _FinalArrayOfStrProc: TIDProcedure read fFinalArrayOfStrProc;
    property _FinalArrayOfVarProc: TIDProcedure read fFinalArrayOfVarProc;
    property _ExplicitEnumFromAny: TSysOpImplicit read fExplicitEnumFromAny;
    property _ExplicitTProcFromAny: TSysOpImplicit read fExplicitTProcFromAny;
    property _AssertProc: TIDProcedure read fAsserProc;
    property _TypeID: TIDType read fTypeIDType;
    property _DeprecatedDefaultStr: TIDStringConstant read fDeprecatedDefaultStr;
    property _OrdinalType: TIDType read fOrdinalType;
    property _PAnsiCharType: TIDType read fPAnsiChar;
    property _PCharType: TIDType read fPChar;
    property _AnyArrayType: TIDArray read fArrayType;
    property _WideString: TIDType read fWideString;
  end;

var
  SYSUnit: TSYSTEMUnit = nil;  // модуль SYSYTEM
  _MetaType: TIDType = nil;    // служебный мета-тип который является типом всех остальных типов данных.
  _Void: TIDType = nil;


implementation

uses AST.Parser.Errors,
     AST.Delphi.Errors,
     AST.Delphi.SysFunctions, AST.Targets, AST.Lexer;

procedure AddUnarOperator(Op: TOperatorID; Source, Destination: TIDType); inline;
begin
  Source.OverloadUnarOperator(Op, Destination);
end;

procedure AddBinarOperator(Op: TOperatorID; Left, Right, Result: TIDType); overload; inline;
begin
  Left.OverloadBinarOperator2(Op, Right, Result);
end;

procedure AddBinarOperator(Op: TOperatorID; Left: TIDType; const Rights: array of TIDType; Result: TIDType); overload;
var
  i: Integer;
  Right: TIDType;
begin
  for i := 0 to Length(Rights) - 1 do begin
    Right := Rights[i];
    AddBinarOperator(Op, Left, Right, Result);
    if Left <> Right then
      AddBinarOperator(Op, Right, Left, Result);
  end;
end;

{ TSYSTEMUnit }

procedure TSYSTEMUnit.AddImplicists;
  procedure AddBaseImplicits(DataType: TIDType);
  var
    i: TDataTypeID;
  begin
    for i := dtInt8 to dtFloat64 do
      DataType.OverloadImplicitTo(DataTypes[i]);
    DataType.OverloadImplicitTo(_Variant, FImplicitAnyToVariant);
  end;
var
  i: TDataTypeID;
begin
  // signed:
  AddBaseImplicits(_Int8);
  AddBaseImplicits(_Int16);
  AddBaseImplicits(_Int32);
  AddBaseImplicits(_Int64);
  AddBaseImplicits(_UInt8);
  AddBaseImplicits(_UInt16);
  AddBaseImplicits(_UInt32);
  AddBaseImplicits(_UInt64);
  AddBaseImplicits(_NativeInt);
  AddBaseImplicits(_NativeUInt);

  // Variant
  for i := dtInt8 to dtVariant do
    _Variant.OverloadImplicitTo(DataTypes[i], FImplicitVariantToAny);

  // float32
  with _Float32 do begin
    OverloadImplicitTo(_Float32);
    OverloadImplicitTo(_Float64);
    OverloadImplicitTo(_Variant, FImplicitAnyToVariant);
  end;

  // Float64
  with _Float64 do begin
    OverloadImplicitTo(_Float32);
    OverloadImplicitTo(_Float64);
    OverloadImplicitTo(_Variant, FImplicitAnyToVariant);
  end;

  // Char
  with _Char do begin
    OverloadImplicitTo(_Char);
    //OverloadImplicitTo(_AnsiChar);
    //OverloadImplicitTo(_String);
    OverloadImplicitTo(_Variant, FImplicitAnyToVariant);
  end;

  // AnsiChar
  with _AnsiChar do begin
    //OverloadImplicitTo(_Char);
    OverloadImplicitTo(_AnsiChar);
    //OverloadImplicitTo(_String);
    //OverloadImplicitTo(_AnsiString);
    OverloadImplicitTo(_Variant, FImplicitAnyToVariant);
  end;

  // string
  _String.OverloadImplicitTo(_Variant, FImplicitAnyToVariant);
  _String.OverloadImplicitTo(_AnsiString, TIDOpImplicitStringToAnsiString.CreateInternal(_AnsiString));
  _String.OverloadImplicitTo(_TGuid, TIDOpImplicitStringToGUID.CreateInternal(_TGuid));
  _String.OverloadImplicitTo(_PCharType, TIDOpImplicitStringToPChar.CreateInternal(_PCharType));
  _String.OverloadImplicitTo(_PAnsiCharType, TIDOpImplicitStringToPChar.CreateInternal(_PAnsiCharType));
  _String.OverloadImplicitTo(_WideString);
  _String.OverloadImplicitFrom(_PCharType);
  _String.OverloadImplicitFromAny(TSysImplicitStringFromAny.Instance);

  // AnsiString
  _AnsiString.OverloadImplicitTo(_Variant, FImplicitAnyToVariant);
  _AnsiString.OverloadImplicitTo(_PCharType, TIDOpImplicitStringToPChar.CreateInternal(_PCharType));
  _AnsiString.OverloadImplicitTo(_PAnsiCharType, TIDOpImplicitStringToPChar.CreateInternal(_PAnsiCharType));
  _AnsiString.OverloadImplicitTo(_String, TIDOpImplicitAnsiStringToString.CreateInternal(_String));
  _AnsiString.OverloadImplicitTo(_TGuid, TIDOpImplicitStringToGUID.CreateInternal(_TGuid));
  _AnsiString.OverloadImplicitTo(_ShortString);
  _AnsiString.OverloadImplicitFrom(_PAnsiCharType);

  // WideString
  _WideString.OverloadImplicitTo(_String);
 
  // ShortString
  _ShortString.OverloadImplicitTo(_AnsiString);

  // Char
  _Char.OverloadImplicitTo(_String, TIDOpImplicitCharToString.CreateInternal(_String));
  _Char.OverloadImplicitTo(_AnsiString, TIDOpImplicitCharToAnsiString.CreateInternal(_AnsiString));
  _Char.OverloadImplicitTo(_AnsiChar, TIDOpImplicitCharToAnsiChar.CreateInternal(_AnsiChar));

  // AnsiChar
  _AnsiChar.OverloadImplicitTo(_AnsiString, TIDOpImplicitAnsiCharToAnsiString.CreateInternal(_AnsiString));
  _AnsiChar.OverloadImplicitTo(_String, TIDOpImplicitAnsiCharToString.CreateInternal(_String));
  _AnsiChar.OverloadImplicitTo(_Char, TIDOpImplicitAnsiCharToChar.CreateInternal(_Char));

  _MetaType.OverloadImplicitTo(_TGuid, TIDOpImplicitMetaClassToGUID.CreateInternal(_TGuid));

  // Boolean
  _Boolean.OverloadImplicitTo(_Boolean);
  _Boolean.OverloadImplicitTo(_Variant, FImplicitAnyToVariant);

end;

procedure TSYSTEMUnit.AddCustomExplicits(const Sources: array of TDataTypeID; Dest: TIDType);
var
  i: Integer;
begin
  for i := 0 to Length(Sources) - 1 do
     DataTypes[Sources[i]].OverloadExplicitTo(Dest);
end;

procedure TSYSTEMUnit.AddExplicists;
  procedure AddBaseExplicits(DataType: TIDType);
  var
    i: TDataTypeID;
  begin
    for i := dtInt8 to dtFloat64 do
      DataType.OverloadExplicitTo(DataTypes[i]);
  end;
  procedure AddExplicits(DataType: TIDType; LB, HB: TDataTypeID);
  var
    i: TDataTypeID;
  begin
    for i := LB to HB do
      DataType.OverloadExplicitTo(DataTypes[i]);
  end;
begin
  AddBaseExplicits(_Int8);
  AddBaseExplicits(_Int16);
  AddBaseExplicits(_Int32);
  AddBaseExplicits(_Int64);
  AddBaseExplicits(_UInt8);
  AddBaseExplicits(_UInt16);
  AddBaseExplicits(_UInt32);
  AddBaseExplicits(_UInt64);
  AddBaseExplicits(_NativeInt);
  AddBaseExplicits(_NativeUInt);
  AddBaseExplicits(_Variant);
  AddBaseExplicits(_Boolean);
  AddBaseExplicits(_Char);
  AddBaseExplicits(_AnsiChar);

  AddExplicits(_Float32, dtFloat32, dtFloat64);
  AddExplicits(_Float64, dtFloat32, dtFloat64);

  // String
  _String.OverloadExplicitTo(_Pointer);
  _String.OverloadExplicitTo(_NativeInt);
  _String.OverloadExplicitTo(_NativeUInt);
  _String.OverloadExplicitTo(_PCharType);
  _String.OverloadExplicitTo(_WideString);
  _String.OverloadExplicitTo(_ShortString);
  _String.OverloadExplicitFromAny(TSysExplicitStringFromAny.Instance);

  // AnsiString
  _AnsiString.OverloadExplicitTo(_Pointer);
  _AnsiString.OverloadExplicitTo(_NativeInt);
  _AnsiString.OverloadExplicitTo(_NativeUInt);
  _AnsiString.OverloadExplicitTo(_PAnsiCharType);
  _AnsiString.OverloadExplicitTo(_ShortString);
  _AnsiString.OverloadExplicitFromAny(TSysExplicitAnsiStringFromAny.Instance);

  // WideString
  _WideString.OverloadExplicitTo(_String);
  _WideString.OverloadExplicitTo(_Pointer);
  _WideString.OverloadExplicitTo(_NativeInt);
  _WideString.OverloadExplicitTo(_NativeUInt);

  // AnsiChar
  _AnsiChar.OverloadExplicitTo(_Char);

  // ShortString
  _ShortString.OverloadExplicitTo(_AnsiString);

  // Char
  _Char.OverloadExplicitTo(_String);
  _Char.OverloadExplicitToAny(TSysExplicitCharToAny.Instance);

  _PCharType.OverloadExplicitTo(_String);

  AddCustomExplicits([dtInt8, dtInt16, dtInt32, dtInt64, dtUInt8, dtUInt16, dtUInt32, dtUInt64, dtBoolean], _Char);
  AddCustomExplicits([dtInt8, dtInt16, dtInt32, dtInt64, dtUInt8, dtUInt16, dtUInt32, dtUInt64, dtBoolean], _AnsiChar);
end;

procedure TSYSTEMUnit.AddIntDivOperators;
begin
  AddBinarOperator(opIntDiv, _Int8, [_Int8, _UInt8], _Int8);
  AddBinarOperator(opIntDiv, _UInt8, _UInt8, _UInt8);

  AddBinarOperator(opIntDiv, _Int16, [_Int8, _UInt8, _Int16, _UInt16], _Int16);
  AddBinarOperator(opIntDiv, _UInt16, [_UInt8, _UInt16], _UInt16);

  AddBinarOperator(opIntDiv, _Int32, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32], _Int32);
  AddBinarOperator(opIntDiv, _UInt32, [_UInt8, _UInt16, _UInt32], _UInt32);

  AddBinarOperator(opIntDiv, _Int64, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _Int64);
  AddBinarOperator(opIntDiv, _UInt64, [_UInt8, _UInt16, _UInt32, _UInt64], _UInt64);

  AddBinarOperator(opIntDiv, _NativeInt, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _Int64);
  AddBinarOperator(opIntDiv, _NativeUInt, [_UInt8, _UInt16, _UInt32, _UInt64], _UInt64);
end;

procedure TSYSTEMUnit.AddLogicalOperators;
begin
  AddUnarOperator(opNot, _Boolean, _Boolean);
  AddBinarOperator(opAnd, _Boolean, _Boolean, _Boolean);
  AddBinarOperator(opOr, _Boolean, _Boolean, _Boolean);
  AddBinarOperator(opXor, _Boolean, _Boolean, _Boolean);
end;

procedure TSYSTEMUnit.AddModOperators;
begin
  AddBinarOperator(opModDiv, _Int8, [_Int8, _UInt8], _Int8);
  AddBinarOperator(opModDiv, _UInt8, _UInt8, _UInt8);

  AddBinarOperator(opModDiv, _Int16, [_Int8, _UInt8, _Int16, _UInt16], _Int16);
  AddBinarOperator(opModDiv, _UInt16, [_UInt8, _UInt16], _UInt16);

  AddBinarOperator(opModDiv, _Int32, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32], _Int32);
  AddBinarOperator(opModDiv, _UInt32, [_UInt8, _UInt16, _UInt32], _UInt32);

  AddBinarOperator(opModDiv, _Int64, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _Int64);
  AddBinarOperator(opModDiv, _UInt64, [_UInt8, _UInt16, _UInt32, _UInt64], _UInt64);
end;

procedure TSYSTEMUnit.AddMulOperators;
begin
  AddBinarOperator(opMultiply, _Int8, [_Int8, _UInt8], _Int8);
  AddBinarOperator(opMultiply, _UInt8, _UInt8, _UInt8);

  AddBinarOperator(opMultiply, _Int16, [_Int8, _UInt8, _Int16, _UInt16], _Int16);
  AddBinarOperator(opMultiply, _UInt16, [_UInt8, _UInt16], _UInt16);

  AddBinarOperator(opMultiply, _Int32, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32], _Int32);
  AddBinarOperator(opMultiply, _UInt32, [_UInt8, _UInt16, _UInt32], _UInt32);

  AddBinarOperator(opMultiply, _Int64, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _Int64);
  AddBinarOperator(opMultiply, _UInt64, [_UInt8, _UInt16, _UInt32, _UInt64], _UInt64);

  AddBinarOperator(opMultiply, _NativeInt, _NativeInt, _NativeInt);
  AddBinarOperator(opMultiply, _NativeUInt, _NativeUInt, _NativeUInt);

  // int * float
  AddBinarOperator(opMultiply, _Float32, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64, _Float32, _Float64], _Float64);
  AddBinarOperator(opMultiply, _Float64, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _Float64);

  // variant
  AddBinarOperator(opMultiply, _Variant, _Variant, _Variant);
end;

procedure TSYSTEMUnit.AddNegOperators;
begin
  AddUnarOperator(opNegative, _Int8, _Int8);
  AddUnarOperator(opNegative, _UInt8, _Int8);
  AddUnarOperator(opNegative, _Int16, _Int16);
  AddUnarOperator(opNegative, _UInt16, _Int16);
  AddUnarOperator(opNegative, _Int32, _Int32);
  AddUnarOperator(opNegative, _UInt32, _Int32);
  AddUnarOperator(opNegative, _Int64, _Int64);
  AddUnarOperator(opNegative, _UInt64, _Int64);
  AddUnarOperator(opNegative, _NativeInt, _NativeInt);
  AddUnarOperator(opNegative, _NativeUInt, _NativeInt);
  AddUnarOperator(opNegative, _Float32, _Float32);
  AddUnarOperator(opNegative, _Float64, _Float64);
end;

procedure TSYSTEMUnit.AddSubOperators;
begin
  // int - int
  AddBinarOperator(opSubtract, _Int8, [_Int8, _UInt8], _Int8);
  AddBinarOperator(opSubtract, _UInt8, _UInt8, _UInt8);

  AddBinarOperator(opSubtract, _Int16, [_Int8, _UInt8, _Int16, _UInt16], _Int16);
  AddBinarOperator(opSubtract, _UInt16, [_UInt8, _UInt16], _UInt16);

  AddBinarOperator(opSubtract, _Int32, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32], _Int32);
  AddBinarOperator(opSubtract, _UInt32, [_UInt8, _UInt16, _UInt32], _UInt32);

  AddBinarOperator(opSubtract, _NativeInt, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32], _NativeInt);
  AddBinarOperator(opSubtract, _NativeUInt, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32], _NativeInt);

  AddBinarOperator(opSubtract, _Int64, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _Int64);
  AddBinarOperator(opSubtract, _UInt64, [_UInt8, _UInt16, _UInt32, _UInt64], _UInt64);

  // int - float
  AddBinarOperator(opSubtract, _Float32, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64, _Float32], _Float32);
  AddBinarOperator(opSubtract, _Float64, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64, _Float32, _Float64], _Float64);

  AddBinarOperator(opSubtract, _Variant, _Variant, _Variant);
end;

procedure TSYSTEMUnit.AddSystemOperators;
begin
  // IN operator
  _AnsiChar.AddBinarySysOperator(opIn, TSysAnsiChar_In.Instance);
  _Char.AddBinarySysOperator(opIn, TSysAnsiChar_In.Instance);

  // system IN operator for ordinal types
  _Int8.AddBinarySysOperator(opIn, TSysOrdinal_In.Instance);
  _Int16.AddBinarySysOperator(opIn, TSysOrdinal_In.Instance);
  _Int32.AddBinarySysOperator(opIn, TSysOrdinal_In.Instance);
  _UInt8.AddBinarySysOperator(opIn, TSysOrdinal_In.Instance);
  _UInt16.AddBinarySysOperator(opIn, TSysOrdinal_In.Instance);
  _UInt32.AddBinarySysOperator(opIn, TSysOrdinal_In.Instance);
  _Boolean.AddBinarySysOperator(opIn, TSysOrdinal_In.Instance);

  // IntDiv
  _Int8.AddBinarySysOperatorFor(opIntDiv, TSys_Ptr_IntDiv_Int.Instance);
  _Int16.AddBinarySysOperatorFor(opIntDiv, TSys_Ptr_IntDiv_Int.Instance);
  _Int32.AddBinarySysOperatorFor(opIntDiv, TSys_Ptr_IntDiv_Int.Instance);
  _UInt8.AddBinarySysOperatorFor(opIntDiv, TSys_Ptr_IntDiv_Int.Instance);
  _UInt16.AddBinarySysOperatorFor(opIntDiv, TSys_Ptr_IntDiv_Int.Instance);
  _UInt32.AddBinarySysOperatorFor(opIntDiv, TSys_Ptr_IntDiv_Int.Instance);
end;

procedure TSYSTEMUnit.AddAddOperators;
begin
  AddBinarOperator(opAdd, _Int8, [_Int8, _UInt8], _Int8);
  AddBinarOperator(opAdd, _UInt8, _UInt8, _UInt8);

  AddBinarOperator(opAdd, _Int16, [_Int8, _UInt8, _Int16, _UInt16], _Int16);
  AddBinarOperator(opAdd, _UInt16, [_UInt8, _UInt16], _UInt16);

  AddBinarOperator(opAdd, _Int32, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32], _Int32);
  AddBinarOperator(opAdd, _UInt32, [_UInt8, _UInt16, _UInt32], _UInt32);

  AddBinarOperator(opAdd, _NativeInt, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32], _NativeInt);
  AddBinarOperator(opAdd, _NativeUInt, [_UInt8, _UInt16, _UInt32], _NativeUInt);

  AddBinarOperator(opAdd, _Int64, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _Int64);
  AddBinarOperator(opAdd, _UInt64, [_UInt8, _UInt16, _UInt32, _UInt64], _UInt64);

  // int + float
  AddBinarOperator(opAdd, _Float32, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64, _Float32], _Float32);
  AddBinarOperator(opAdd, _Float64, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64, _Float32, _Float64], _Float64);

  // strings
  AddBinarOperator(opAdd, _String, _String, _String);
  AddBinarOperator(opAdd, _Char, _Char, _Char);
  AddBinarOperator(opAdd, _AnsiString, _AnsiString, _AnsiString);
  AddBinarOperator(opAdd, _AnsiChar, _AnsiChar, _AnsiChar);

  AddBinarOperator(opAdd, _Variant, _Variant, _Variant);
end;

procedure TSYSTEMUnit.AddDivOperators;
begin
  AddBinarOperator(opDivide, _Int8, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _Float64);
  AddBinarOperator(opDivide, _Int16, [_Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _Float64);
  AddBinarOperator(opDivide, _Int32, [_Int32, _UInt32, _Int64, _UInt64], _Float64);
  AddBinarOperator(opDivide, _Int64, [_Int64, _UInt64], _Float64);

  AddBinarOperator(opDivide, _Float32, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64, _Float32, _Float64], _Float64);
  AddBinarOperator(opDivide, _Float64, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _Float64);
  AddBinarOperator(opDivide, _Variant, _Variant, _Variant);
end;

procedure TSYSTEMUnit.AddArithmeticOperators;
begin
  AddNegOperators;
  AddAddOperators;
  AddSubOperators;
  AddMulOperators;
  AddDivOperators;
  AddIntDivOperators;
  AddModOperators;
end;


procedure TSYSTEMUnit.AddBitwiseOperators;
  function GetMaxBitwiceOpType(DtLeft, DtRight: TIDType): TIDType;
  begin
    if DtLeft.DataTypeID in [dtInt8, dtUint8, dtInt16, dtUInt16, dtInt32, dtUint32] then
      Result := _UInt32
    else
      Result := _Int64;
  end;

  procedure UnarOp(Op: TOperatorID);
  var
    i: TDataTypeID;
  begin
    for i := dtInt8 to dtUInt64 do
      AddUnarOperator(Op, DataTypes[i], DataTypes[i]);
  end;
  procedure BitwiseOp(Op: TOperatorID);
  var
    i, j: TDataTypeID;
  begin
    for i := dtInt8 to dtNativeUInt do
       for j := dtInt8 to dtUInt64 do
         AddBinarOperator(Op, DataTypes[i], DataTypes[j], GetMaxBitwiceOpType(DataTypes[i], DataTypes[j]));
  end;
begin
  UnarOp(opNot);
  BitwiseOp(opAnd);
  BitwiseOp(opOr);
  BitwiseOp(opXor);
  BitwiseOp(opShiftLeft);
  BitwiseOp(opShiftRight);
end;

procedure TSYSTEMUnit.AddCompareOperators;
  procedure AD(Op: TOperatorID);
  var
    i, j: TDataTypeID;
  begin
    for i := dtInt8 to dtNativeUInt do
      for j := dtInt8 to dtNativeUInt do
        AddBinarOperator(Op, DataTypes[i], DataTypes[j], _Boolean);

    for i := dtInt8 to dtUInt64 do begin
      AddBinarOperator(Op, DataTypes[i], _Float32, _Boolean);
      AddBinarOperator(Op, DataTypes[i], _Float64, _Boolean);
      AddBinarOperator(Op, _Float32, DataTypes[i], _Boolean);
      AddBinarOperator(Op, _Float64, DataTypes[i], _Boolean);
    end;

    AddBinarOperator(Op, _Float32, _Float32, _Boolean);
    AddBinarOperator(Op, _Float32, _Float64, _Boolean);
    AddBinarOperator(Op, _Float64, _Float32, _Boolean);
    AddBinarOperator(Op, _Float64, _Float64, _Boolean);

    AddBinarOperator(Op, _Boolean, _Boolean, _Boolean);
    AddBinarOperator(Op, _Variant, _Variant, _Boolean);
  end;
begin
  AD(opEqual);
  AD(opNotEqual);
  AD(opLess);
  AD(opLessOrEqual);
  AD(opGreater);
  AD(opGreaterOrEqual);

  // char
  AddBinarOperator(opEqual, _Char, _Char, _Boolean);
  AddBinarOperator(opNotEqual, _Char, _Char, _Boolean);
  AddBinarOperator(opLess, _Char, _Char, _Boolean);
  AddBinarOperator(opLessOrEqual, _Char, _Char, _Boolean);
  AddBinarOperator(opGreater, _Char, _Char, _Boolean);
  AddBinarOperator(opGreaterOrEqual, _Char, _Char, _Boolean);

  // ansichar
  AddBinarOperator(opEqual, _AnsiChar, _AnsiChar, _Boolean);
  AddBinarOperator(opNotEqual, _AnsiChar, _AnsiChar, _Boolean);
  AddBinarOperator(opLess, _AnsiChar, _AnsiChar, _Boolean);
  AddBinarOperator(opLessOrEqual, _AnsiChar, _AnsiChar, _Boolean);
  AddBinarOperator(opGreater, _AnsiChar, _AnsiChar, _Boolean);
  AddBinarOperator(opGreaterOrEqual, _AnsiChar, _AnsiChar, _Boolean);

  // ansistring
  AddBinarOperator(opEqual, _AnsiString, _AnsiString, _Boolean);
  AddBinarOperator(opNotEqual, _AnsiString, _AnsiString, _Boolean);
  AddBinarOperator(opLess, _AnsiString, _AnsiString, _Boolean);
  AddBinarOperator(opLessOrEqual, _AnsiString, _AnsiString, _Boolean);
  AddBinarOperator(opGreater, _AnsiString, _AnsiString, _Boolean);
  AddBinarOperator(opGreaterOrEqual, _AnsiString, _AnsiString, _Boolean);

  // string
  AddBinarOperator(opEqual, _String, _String, _Boolean);
  AddBinarOperator(opNotEqual, _String, _String, _Boolean);
  AddBinarOperator(opLess, _String, _String, _Boolean);
  AddBinarOperator(opLessOrEqual, _String, _String, _Boolean);
  AddBinarOperator(opGreater, _String, _String, _Boolean);
  AddBinarOperator(opGreaterOrEqual, _String, _String, _Boolean);
end;

function TSYSTEMUnit.RegisterType(const TypeName: string; TypeClass: TIDTypeClass; DataType: TDataTypeID): TIDType;
begin
  Result := TypeClass.Create(IntfScope, Identifier(TypeName));
  Result.Elementary := True;
  Result.DataTypeID := DataType;
  Result.ItemType := itType;
  InsertToScope(Result);
  FDataTypes[DataType] := Result;
  AddType(Result);
end;

function TSYSTEMUnit.RegisterTypeCustom(const TypeName: string; TypeClass: TIDTypeClass;
  DataType: TDataTypeID): TIDType;
begin
  Result := TypeClass.Create(IntfScope, Identifier(TypeName));
  Result.Elementary := True;
  Result.DataTypeID := DataType;
  Result.ItemType := itType;
  InsertToScope(Result);
  AddType(Result);
end;

function TSYSTEMUnit.RegisterVariable(Scope: TScope; const Name: string; DataType: TIDType): TIDVariable;
begin
  Result := TIDVariable.CreateAsSystem(Scope, Name);
  Result.DataType := DataType;
  InsertToScope(Scope, Result);
end;

procedure TSYSTEMUnit.SearchSystemTypes;
begin
{  FTObject := GetPublicClass('TObject');
  FException := GetPublicClass('Exception');
  FEAssertClass := GetPublicClass('EAssert');
  FTypeIDType := GetPublicType('TDataTypeID');}
end;

procedure TSYSTEMUnit.SystemFixup;
begin
  if Package.Target = TWINX86_Target.TargetName then
  begin
    var coeffTypeType: TIDEnum := RegisterType('coeffType', TIDEnum, dtEnum) as TIDEnum;
    coeffTypeType.Items := TScope.Create(stLocal, ImplScope);
    var Item := TIDIntConstant.Create(coeffTypeType.Items, TIdentifier.Make('cHi'));
    Item.DataType := coeffTypeType;
    Item.Value := 0;
    InsertToScope(coeffTypeType.Items, Item);
    Item := TIDIntConstant.Create(coeffTypeType.Items, TIdentifier.Make('cLo'));
    Item.DataType := coeffTypeType;
    Item.Value := 1;
    InsertToScope(coeffTypeType.Items, Item);
    coeffTypeType.LowBound := 0;
    coeffTypeType.HighBound := 1;
  end;
end;

function TSYSTEMUnit.RegisterBuiltin(const Name: string; MacroID: TBuiltInFunctionID; ResultDataType: TIDType; Flags: TProcFlags = []): TIDBuiltInFunction;
begin
  Result := TIDBuiltInFunction.Create(Self.IntfScope, Name, ResultDataType);
  Result.ResultType := ResultDataType;
  Result.Flags := Flags;
  InsertToScope(Result);
end;

function TSYSTEMUnit.RegisterBuiltin(const BuiltinClass: TIDBuiltInFunctionClass): TIDBuiltInFunction;
begin
  Result := BuiltinClass.CreateDecl(Self.IntfScope);
  InsertToScope(Self.IntfScope, Result);
end;

procedure TSYSTEMUnit.RegisterBuiltinFunctions;
var
  Decl: TIDBuiltInFunction;
begin
  FArrayType := TIDArray.Create(nil, Identifier('<array type or string>'));
  FRefType := TIDType.Create(nil, Identifier(''));

  RegisterBuiltin(TSF_Abs);
  RegisterBuiltin(TSF_Assert);
  RegisterBuiltin(TSF_Assigned);
  RegisterBuiltin(TSF_AtomicCmpExchange);
  RegisterBuiltin(TSF_AtomicDecrement);
  RegisterBuiltin(TSF_AtomicExchange);
  RegisterBuiltin(TSF_AtomicIncrement);
  RegisterBuiltin(TSF_Copy);
  RegisterBuiltin(TSF_Chr);
  RegisterBuiltin(TCT_Dec);
  RegisterBuiltin(TSF_Dispose);
  RegisterBuiltin(TSCTF_Defined);
  RegisterBuiltin(TSCTF_Declared);
  RegisterBuiltin(TSF_Exit);
  RegisterBuiltin(TSF_FreeMem);
  RegisterBuiltin(TSF_FillChar);
  RegisterBuiltin(TSF_GetMem);
  RegisterBuiltin(TSF_Halt);
  RegisterBuiltin(TSF_HiBound);
  RegisterBuiltin(TSF_Inc);
  RegisterBuiltin(TSF_LoBound);
  RegisterBuiltin(TSF_Length);
  RegisterBuiltin(TSF_New);
  RegisterBuiltin(TSF_Now);
  RegisterBuiltin(TSF_Ord);
  RegisterBuiltin(TSF_Odd);
  RegisterBuiltin(TSF_Pred);
  RegisterBuiltin(TSF_ReallocMem);
  RegisterBuiltin(TSF_Round);
  RegisterBuiltin(TSF_RunError);
  RegisterBuiltin(TSF_Str);
  RegisterBuiltin(TCT_SizeOf);
  RegisterBuiltin(TSF_Succ);
  RegisterBuiltin(TSF_SetLength);
  RegisterBuiltin(TSF_SetString);
  RegisterBuiltin(TSF_Trunc);
  RegisterBuiltin(TSF_Val);

  RegisterVariable(ImplScope, 'ReturnAddress', _Pointer);
  RegisterConstStr(ImplScope, 'libmmodulename', '');
end;

function TSYSTEMUnit.RegisterOrdinal(const TypeName: string; DataType: TDataTypeID; LowBound: Int64; HighBound: UInt64): TIDType;
begin
  Result := RegisterType(TypeName, TIDOrdinal, DataType);
  TIDOrdinal(Result).LowBound := LowBound;
  TIDOrdinal(Result).HighBound := HighBound;
  TIDOrdinal(Result).SignedBound  := LowBound < 0;
end;

function TSYSTEMUnit.Compile(RunPostCompile: Boolean = True): TCompilerResult;
begin
  Result := CompileFail;
  try
    RegisterBuiltinFunctions;
    SystemFixup;
    Result := inherited Compile(False);
    if Result = CompileSuccess then
    begin
      SearchSystemTypes;
    end;
  except
    on e: ECompilerStop do Exit;
    on e: ECompilerSkip do Exit(CompileSkip);
    on e: ECompilerAbort do PutMessage(ECompilerAbort(e).CompilerMessage^);
    on e: Exception do PutMessage(cmtInteranlError, e.Message);
  end;
end;

constructor TSYSTEMUnit.Create(const Project: IASTProject; const FileName: string; const Source: string);
begin
  inherited Create(Project, FileName, Source);
  {$IFDEF DEBUG}
  SetUnitName('system');
  {$ENDIF}

  // nil constant
  FNullPtrType := TIDNullPointerType.CreateAsSystem(IntfScope, 'null ptr');
  FNullPtrConstatnt := TIDIntConstant.Create(IntfScope, Identifier('nil'), FNullPtrType, 0);
  FNullPtrExpression := TIDExpression.Create(FNullPtrConstatnt);
  IntfScope.InsertID(FNullPtrConstatnt);

  // Untyped reference
  fUntypedReferenceType := TIDPointer.CreateAsSystem(IntfScope, 'Untyped reference');
  IntfScope.InsertID(fUntypedReferenceType);
  fUntypedReferenceType.OverloadImplicitFromAny(TIDOpImplicitAnyToUntyped.Instance);
  fUntypedReferenceType.OverloadExplicitToAny(TSysExplicitUntypedToAny.Instance);

  fOrdinalType := TIDOrdinal.CreateAsSystem(nil, 'ordinal');
  fExplicitEnumFromAny := TIDOpExplicitIntToEnum.Instance;
  fExplicitTProcFromAny := TIDOpExplicitTProcFromAny.Instance;

  {!!! Ïîðÿäîê ðåãèñòðàöèè òèïîâ ñîîòâåòñòâóåò ïîðÿäêîâîìó íîìåðó êîíñòàíòû DataTypeID !!!}
  //===============================================================
  RegisterOrdinal('_Int8', dtInt8, MinInt8, MaxInt8);
  RegisterOrdinal('_Int16', dtInt16, MinInt16, MaxInt16);
  RegisterOrdinal('_Int32', dtInt32, MinInt32, MaxInt32);
  RegisterOrdinal('Int64', dtInt64, MinInt64, MaxInt64);
  RegisterOrdinal('_UInt8', dtUInt8, 0, MaxUInt8);
  RegisterOrdinal('_UInt16', dtUInt16, 0, MaxUInt16);
  RegisterOrdinal('_UInt32', dtUInt32, 0, MaxUInt32);
  RegisterOrdinal('UInt64', dtUInt64, 0, MaxUInt64);
  RegisterOrdinal('NativeInt', dtNativeInt, MinInt64, MaxInt64);
  RegisterOrdinal('NativeUInt', dtNativeUInt, 0, MaxUInt64);
  RegisterType('Single', TIDType, dtFloat32);
  RegisterType('Double', TIDType, dtFloat64);

  //===============================================================
  RegisterOrdinal('Boolean', dtBoolean, 0, 1);
  RegisterOrdinal('AnsiChar', dtAnsiChar, 0, MaxUInt8);
  RegisterOrdinal('Char', dtChar, 0, MaxUInt16);
  //===============================================================
  RegisterType('AnsiString', TIDString, dtAnsiString);
  TIDString(_AnsiString).ElementDataType := _AnsiChar;
  TIDString(_AnsiString).AddBound(TIDOrdinal(_NativeUInt));
  //===============================================================
  RegisterType('String', TIDString, dtString);
  TIDString(_String).ElementDataType := _Char;
  TIDString(_String).AddBound(TIDOrdinal(_NativeUInt));
  RegisterType('Variant', TIDVariant, dtVariant);
  FImplicitAnyToVariant := TIDOpImplicitAnyToVariant.CreateInternal(_Variant);
  FImplicitVariantToAny := TIDOpImplicitVariantToAny.CreateInternal(nil);
  //===============================================================
  fWideString := RegisterTypeCustom('WideString', TIDString, dtWideString);
  TIDString(fWideString).ElementDataType := _Char;
  TIDString(fWideString).AddBound(TIDOrdinal(_NativeUInt));
  //===============================================================
  fShortString := RegisterTypeCustom('ShortString', TIDString, dtAnsiString);
  TIDString(fShortString).ElementDataType := _Char;
  TIDString(fShortString).AddBound(TIDOrdinal(_NativeUInt));
  //===============================================================
  fOpenString := RegisterTypeCustom('OpenString', TIDString, dtString);
  TIDString(fOpenString).ElementDataType := _Char;
  TIDString(fOpenString).AddBound(TIDOrdinal(_NativeUInt));
  //===============================================================
  // TObject ========================================================
  {FTObject := TIDClass.CreateAsSystem(UnitInterface, 'TObject');
  FTObject.NeedForward := True; // forward declaration
  InsertToScope(FTObject);}
  // TGUID ========================================================
  FGuidType := TIDStructure.CreateAsSystem(IntfScope, '_TGUID');
  FGuidType.DataTypeID := dtGuid;
  FGuidType.AddField('LoDWord', _Int64);
  FGuidType.AddField('HiDWord', _Int64);
  FGuidType.OverloadBinarOperator2(opEqual, FGuidType, _Boolean);
  FGuidType.OverloadBinarOperator2(opNotEqual, FGuidType, _Boolean);
  FGuidType.DataType := _MetaType;
  InsertToScope(FGuidType);
  AddType(FGuidType);
  //===============================================================
  FDateTimeType := TIDAliasType.CreateAliasAsSystem(IntfScope, 'DateTime', _Float64);
  FDateType := TIDAliasType.CreateAliasAsSystem(IntfScope, 'Date', _Float64);
  FTimeType := TIDAliasType.CreateAliasAsSystem(IntfScope, 'Time', _Float64);

  InsertToScope(FDateTimeType);
  InsertToScope(FDateType);
  InsertToScope(FTimeType);
  AddType(FDateTimeType);
  AddType(FDateType);
  AddType(FTimeType);
  //===============================================================
  FPointerType := RegisterPointer('Pointer', nil);
  //===============================================================

  // Delphi system aliases
  RegisterTypeAlias('Extended', _Float64);
  //RegisterTypeAlias('Double', _Float64);
  //RegisterTypeAlias('Single', _Float32);
  RegisterTypeAlias('Currency', _Float64); // Let it be for now

  RegisterTypeAlias('LongInt', _Int32);
  RegisterTypeAlias('LongWord', _UInt32);
  RegisterTypeAlias('ShortInt', _Int8);
  RegisterTypeAlias('SmallInt', _Int16);
  RegisterTypeAlias('Integer', _Int32);
  RegisterTypeAlias('Cardinal', _UInt32);
  RegisterTypeAlias('Comp', _Int64);
  RegisterTypeAlias('Byte', _UInt8);
  RegisterTypeAlias('Word', _UInt16);
  RegisterTypeAlias('_ShortString', fShortString);
  RegisterTypeAlias('UnicodeString', _String);
  RegisterTypeAlias('WideChar', _Char);

  RegisterTypeAlias('WordBool', _Boolean);
  RegisterTypeAlias('LongBool', _Boolean);
  RegisterTypeAlias('OleVariant', _Variant);

  fPAnsiChar := RegisterPointer('PAnsiChar', _AnsiChar);
  fPChar := RegisterPointer('PWideChar', _Char);
  RegisterTypeAlias('PChar', _PCharType);
  RegisterTypeAlias('Text', _Pointer);
  //RegisterTypeAlias('FixedInt', _Int32);
  //RegisterTypeAlias('FixedUInt', _UInt32);

  RegisterConstInt('MaxInt', _Int32, MaxInt32);

  // constant "True"
  FTrueConstant := TIDBooleanConstant.Create(IntfScope, Identifier('TRUE'), _Boolean, True);
  FTrueExpression := TIDExpression.Create(FTrueConstant);
  IntfScope.InsertID(FTrueConstant);
  // constant "False"
  FFalseConstant := TIDBooleanConstant.Create(IntfScope, Identifier('FALSE'), _Boolean, False);
  FFalseExpression := TIDExpression.Create(FFalseConstant);
  IntfScope.InsertID(FFalseConstant);
  // constant "0"
  FZeroConstant := TIDIntConstant.CreateAnonymous(IntfScope, _UInt8, 0);
  FZeroExpression := TIDExpression.Create(FZeroConstant);
  // constant "1"
  FOneConstant := TIDIntConstant.CreateAnonymous(IntfScope, _UInt8, 1);
  FOneExpression := TIDExpression.Create(FOneConstant);
  // constant ""
  FEmptyStrConstant := TIDStringConstant.CreateAnonymous(IntfScope, _String, '');
  FEmptyStrExpression := TIDExpression.Create(FEmptyStrConstant);
  // constant for deprecated
  fDeprecatedDefaultStr := TIDStringConstant.CreateAsSystem(IntfScope, 'The declaration is deprecated');

  AddImplicists;
  AddExplicists;
  AddArithmeticOperators;
  AddLogicalOperators;
  AddBitwiseOperators;
  AddCompareOperators;
  AddSystemOperators;
end;

procedure TSYSTEMUnit.InitSystemUnit;
begin
  fPointerType.CreateStandardOperators;
  fPAnsiChar.CreateStandardOperators;
  fPChar.CreateStandardOperators;
end;

procedure TSYSTEMUnit.InsertToScope(Declaration: TIDDeclaration);
begin
  if Assigned(IntfScope.InsertNode(Declaration.Name, Declaration)) then
    raise Exception.CreateFmt('Unit SYSTEM: ' + sIdentifierRedeclaredFmt, [Declaration.Name]);
end;

function TSYSTEMUnit.CompileIntfOnly: TCompilerResult;
begin
  Result := Compile();
end;

function TSYSTEMUnit.RegisterTypeAlias(const TypeName: string; OriginalType: TIDType): TIDAliasType;
begin
  Result := TIDAliasType.CreateAliasAsSystem(IntfScope, TypeName, OriginalType);
  Result.Elementary := True;
  InsertToScope(Result);
  AddType(Result);
end;

function TSYSTEMUnit.RegisterConstInt(const Name: string; DataType: TIDType; Value: Int64): TIDIntConstant;
begin
  Result := TIDIntConstant.CreateAsSystem(IntfScope, Name);
  Result.DataType := DataType;
  Result.Value := Value;
  InsertToScope(Result);
end;

function TSYSTEMUnit.RegisterConstStr(Scope: TScope; const Name, Value: string): TIDStringConstant;
begin
  Result := TIDStringConstant.CreateAsSystem(Scope, Name);
  Result.DataType := _String;
  Result.Value := Value;
  InsertToScope(Result);
end;

function TSYSTEMUnit.RegisterPointer(const TypeName: string; TargetType: TIDType): TIDPointer;
begin
  Result := TIDPointer.CreateAsSystem(IntfScope, TypeName);
  Result.ReferenceType := TargetType;
  InsertToScope(Result);
  AddType(Result);
end;

initialization
  _Void := TIDType.CreateAsSystem(nil, 'Void');
  _Void.DataTypeID := TDataTypeID(dtUnknown);

  _MetaType := TIDType.CreateAsSystem(nil, 'MetaType');
  _MetaType.DataTypeID := dtClass;

finalization


end.
