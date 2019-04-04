unit SystemUnit;

interface

{$i compilers.inc}

uses Classes, SysUtils, OPCompiler, NPCompiler.Classes, NPCompiler.DataTypes, IL.Types,
     NPCompiler.Operators, NPCompiler.Utils, NPCompiler.Intf,
     AST.Classes,
     AST.Project,
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
    FDataTypes: TDataTypes;
    FTrueConstant: TIDBooleanConstant;
    FFalseConstant: TIDBooleanConstant;
    FFalseExpression: TIDExpression;
    FTrueExpression: TIDExpression;
    FZeroConstant: TIDIntConstant;
    FZeroExpression: TIDExpression;
    FMinusOneConstant: TIDIntConstant;
    FMinusOneExpression: TIDExpression;
    FOneConstant: TIDIntConstant;
    FOneExpression: TIDExpression;
    FNullPtrType: TIDType;
    FNullPtrConstatnt: TIDIntConstant;
    FNullPtrExpression: TIDExpression;
    FEmptyStrConstant: TIDStringConstant;
    FEmptyStrExpression: TIDExpression;
    FPointerType: TIDPointer;
    FUntypedReferenceType: TIDPointer;
    FArrayType: TIDArray; // служебный тип для функций Length/SetLength
    FRefType: TIDType; // служебный тип для функций SizeOf
    FGuidType: TIDStructure;
    fPAnsiChar: TIDType;
    fPChar: TIDType;
    FOrdinalType: TIDType;
    FTObject: TIDClass;
    FException: TIDClass;
    FEAssertClass: TIDClass;
    FDateTimeType: TIDType;
    FDateType: TIDType;
    FTimeType: TIDType;
    FTypeIDType: TIDType;
    FImplicitAnyToVariant: TIDInternalOpImplicit;
    FImplicitVariantToAny: TIDInternalOpImplicit;
    FExplicitEnumFromAny: TIDInternalOpImplicit;
    fExplicitTProcFromAny: TIDInternalOpImplicit;
    FCopyArrayOfObjProc: TIDProcedure;
    FCopyArrayOfStrProc: TIDProcedure;
    FFinalArrayOfObjProc: TIDProcedure;
    FFinalArrayOfStrProc: TIDProcedure;
    FFinalArrayOfVarProc: TIDProcedure;
    FAsserProc: TIDProcedure;
    FSysTStrDynArray: TIDDynArray;
    FSysTObjDynArray: TIDDynArray;
    FSysTVarDynArray: TIDDynArray;
    fExtended: TIDAliasType;
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
    procedure RegisterSystemRTBuiltinFunctions;
    procedure InsertToScope(Declaration: TIDDeclaration);
    function RegisterBuiltin(const Name: string; MacroID: TBuiltInFunctionID; ResultDataType: TIDType; Flags: TProcFlags = [pfPure]): TIDBuiltInFunction; overload;
    function RegisterType(const TypeName: string; TypeClass: TIDTypeClass; DataType: TDataTypeID): TIDType;
    function RegisterTypeCustom(const TypeName: string; TypeClass: TIDTypeClass; DataType: TDataTypeID): TIDType;
    function RegisterRefType(const TypeName: string; TypeClass: TIDTypeClass; DataType: TDataTypeID): TIDType;
    function RegisterOrdinal(const TypeName: string; DataType: TDataTypeID; LowBound: Int64; HighBound: UInt64): TIDType;
    function RegisterTypeAlias(const TypeName: string; OriginalType: TIDType): TIDAliasType;
    function RegisterPointer(const TypeName: string; TargetType: TIDType): TIDPointer;
    function RegisterConstInt(const Name: string; DataType: TIDType; Value: Int64): TIDIntConstant;
    function RegisterBuiltin(const BuiltinClass: TIDBuiltInFunctionClass): TIDBuiltInFunction; overload;
  private
    procedure SearchSystemTypes;
    procedure AddCustomExplicits(const Sources: array of TDataTypeID; Dest: TIDType); overload;
    function AddSysRTFunction(const SysFuncClass: TIDSysRuntimeFunctionClass; const Name: string; ResultType: TIDType): TIDSysRuntimeFunction;
    function AddSysCTFunction(const SysFuncClass: TIDSysCompileFunctionClass; const Name: string; ResultType: TIDType): TIDSysCompileFunction;
  public
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    constructor Create(const Project: IASTProject; const FileName: string; const Source: string); override;
    function Compile(RunPostCompile: Boolean = True): TCompilerResult; override;
    function CompileIntfOnly: TCompilerResult; override;
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    procedure CreateSystemRoutins;
    //procedure RegisterExternalProc
    property DataTypes: TDataTypes read FDataTypes;
    property _Int8: TIDType read FDataTypes[dtInt8] write FDataTypes[dtInt8];
    property _Int16: TIDType read FDataTypes[dtInt16] write FDataTypes[dtInt16];
    property _Int32: TIDType read FDataTypes[dtInt32] write FDataTypes[dtInt32];
    property _Int64: TIDType read FDataTypes[dtInt64] write FDataTypes[dtInt64];
    property _UInt8: TIDType read FDataTypes[dtUInt8] write FDataTypes[dtUInt8];
    property _UInt16: TIDType read FDataTypes[dtUInt16] write FDataTypes[dtUInt16];
    property _UInt32: TIDType read FDataTypes[dtUInt32] write FDataTypes[dtUInt32];
    property _UInt64: TIDType read FDataTypes[dtUInt64] write FDataTypes[dtUInt64];
    property _NativeInt: TIDType read FDataTypes[dtNativeInt] write FDataTypes[dtNativeInt];
    property _NativeUInt: TIDType read FDataTypes[dtNativeUInt] write FDataTypes[dtNativeUInt];
    property _Float32: TIDType read FDataTypes[dtFloat32] write FDataTypes[dtFloat32];
    property _Float64: TIDType read FDataTypes[dtFloat64] write FDataTypes[dtFloat64];
    property _Boolean: TIDType read FDataTypes[dtBoolean] write FDataTypes[dtBoolean];
    property _AnsiChar: TIDType read FDataTypes[dtAnsiChar] write FDataTypes[dtAnsiChar];
    property _Char: TIDType read FDataTypes[dtChar] write FDataTypes[dtChar];
    property _AnsiString: TIDType read FDataTypes[dtAnsiString] write FDataTypes[dtAnsiString];
    property _String: TIDType read FDataTypes[dtString] write FDataTypes[dtString];
    property _Variant: TIDType read FDataTypes[dtVariant] write FDataTypes[dtVariant];
    property _NilPointer: TIDType read FNullPtrType;
    property _TGuid: TIDStructure read FGuidType;
    property _True: TIDBooleanConstant read FTrueConstant;
    property _False: TIDBooleanConstant read FFalseConstant;
    property _TrueExpression: TIDExpression read FTrueExpression;
    property _FalseExpression: TIDExpression read FFalseExpression;
    property _ZeroConstant: TIDIntConstant read FZeroConstant;
    property _ZeroExpression: TIDExpression read FZeroExpression;
    property _MinusOneExpression: TIDExpression read FMinusOneExpression;
    property _OneConstant: TIDIntConstant read FOneConstant;
    property _OneExpression: TIDExpression read FOneExpression;
    property _NullPtrConstant: TIDIntConstant read FNullPtrConstatnt;
    property _NullPtrExpression: TIDExpression read FNullPtrExpression;
    property _EmptyStrExpression: TIDExpression read FEmptyStrExpression;
    property _Pointer: TIDPointer read FPointerType;
    property _UntypedReference: TIDPointer read FUntypedReferenceType;
    property _TObject: TIDClass read FTObject;
    property _Exception: TIDClass read FException;
    property _EAssert: TIDClass read FEAssertClass;
    property _DateTime: TIDType read FDateTimeType;
    property _Date: TIDType read FDateType;
    property _Time: TIDType read FTimeType;
    property _CopyArrayOfObjProc: TIDProcedure read FCopyArrayOfObjProc;
    property _CopyArrayOfStrProc: TIDProcedure read FCopyArrayOfStrProc;
    property _FinalArrayOfObjProc: TIDProcedure read FFinalArrayOfObjProc;
    property _FinalArrayOfStrProc: TIDProcedure read FFinalArrayOfStrProc;
    property _FinalArrayOfVarProc: TIDProcedure read FFinalArrayOfVarProc;
    property _ExplicitEnumFromAny: TIDInternalOpImplicit read FExplicitEnumFromAny;
    property _ExplicitTProcFromAny: TIDInternalOpImplicit read fExplicitTProcFromAny;
    property _AssertProc: TIDProcedure read FAsserProc;
    property _TypeID: TIDType read FTypeIDType;
    property _DeprecatedDefaultStr: TIDStringConstant read fDeprecatedDefaultStr;
    property _OrdinalType: TIDType read FOrdinalType;
    property _PAnsiCharType: TIDType read fPAnsiChar;
    property _PCharType: TIDType read fPChar;
    property _AnyArrayType: TIDArray read FArrayType;
  end;

var
  SYSUnit: TSYSTEMUnit = nil;  // модуль SYSYTEM
  _MetaType: TIDType = nil;    // служебный мета-тип который является типом всех остальных типов данных.
  _Void: TIDType = nil;


implementation


{ TSystemUnit }

uses NPCompiler.Messages, IL.Instructions, NPCompiler.SysFunctions;

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

  // AnsiString
  _AnsiString.OverloadImplicitTo(_Variant, FImplicitAnyToVariant);


  _String.OverloadImplicitTo(_AnsiString, TIDOpImplicitStringToAnsiString.CreateInternal(_AnsiString));
  _String.OverloadImplicitTo(_TGuid, TIDOpImplicitStringToGUID.CreateInternal(_TGuid));

  _String.OverloadImplicitTo(_PCharType, TIDOpImplicitStringToPChar.CreateInternal(_PCharType));
  _String.OverloadImplicitTo(_PAnsiCharType, TIDOpImplicitStringToPChar.CreateInternal(_PAnsiCharType));

  _AnsiString.OverloadImplicitTo(_PCharType, TIDOpImplicitStringToPChar.CreateInternal(_PCharType));
  _AnsiString.OverloadImplicitTo(_PAnsiCharType, TIDOpImplicitStringToPChar.CreateInternal(_PAnsiCharType));


  _AnsiString.OverloadImplicitTo(_String, TIDOpImplicitAnsiStringToString.CreateInternal(_String));
  _AnsiString.OverloadImplicitTo(_TGuid, TIDOpImplicitStringToGUID.CreateInternal(_TGuid));

  _Char.OverloadImplicitTo(_String, TIDOpImplicitCharToString.CreateInternal(_String));
  _Char.OverloadImplicitTo(_AnsiString, TIDOpImplicitCharToAnsiString.CreateInternal(_AnsiString));
  _Char.OverloadImplicitTo(_AnsiChar, TIDOpImplicitCharToAnsiChar.CreateInternal(_AnsiChar));

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

  _String.OverloadExplicitTo(_Pointer);
  _Pointer.OverloadExplicitTo(_NativeInt);
  _Pointer.OverloadExplicitTo(_NativeUInt);
  _AnsiString.OverloadExplicitTo(_Pointer);

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

  AddUnarOperator(opPostDec, _Int8, _Int8);
  AddUnarOperator(opPostDec, _Int16, _Int16);
  AddUnarOperator(opPostDec, _Int32, _Int32);
  AddUnarOperator(opPostDec, _Int64, _Int64);
  AddUnarOperator(opPostDec, _UInt8, _UInt8);
  AddUnarOperator(opPostDec, _UInt16, _UInt16);
  AddUnarOperator(opPostDec, _UInt32, _UInt32);
  AddUnarOperator(opPostDec, _UInt64, _UInt64);
end;

function TSYSTEMUnit.AddSysCTFunction(const SysFuncClass: TIDSysCompileFunctionClass; const Name: string; ResultType: TIDType): TIDSysCompileFunction;
begin
  Result := SysFuncClass.Create(IntfSection, Name, ResultType);
  Result.DataType := ResultType;
  InsertToScope(Result);
end;

function TSYSTEMUnit.AddSysRTFunction(const SysFuncClass: TIDSysRuntimeFunctionClass; const Name: string; ResultType: TIDType): TIDSysRuntimeFunction;
begin
  Result := SysFuncClass.Create(IntfSection, Name, ResultType);
  Result.DataType := ResultType;
  InsertToScope(Result);
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

  AddUnarOperator(opPostInc, _Int8, _Int8);
  AddUnarOperator(opPostInc, _Int16, _Int16);
  AddUnarOperator(opPostInc, _Int32, _Int32);
  AddUnarOperator(opPostInc, _Int64, _Int64);
  AddUnarOperator(opPostInc, _UInt8, _UInt8);
  AddUnarOperator(opPostInc, _UInt16, _UInt16);
  AddUnarOperator(opPostInc, _UInt32, _UInt32);
  AddUnarOperator(opPostInc, _UInt64, _UInt64);
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
    for i := dtInt8 to dtUInt64 do
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
  Result := TypeClass.Create(IntfSection, Identifier(TypeName));
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
  Result := TypeClass.Create(IntfSection, Identifier(TypeName));
  Result.Elementary := True;
  Result.DataTypeID := DataType;
  Result.ItemType := itType;
  InsertToScope(Result);
  AddType(Result);
end;

procedure TSYSTEMUnit.SearchSystemTypes;
begin
{  FTObject := GetPublicClass('TObject');
  FException := GetPublicClass('Exception');
  FEAssertClass := GetPublicClass('EAssert');
  FTypeIDType := GetPublicType('TDataTypeID');}
end;

function TSYSTEMUnit.RegisterRefType(const TypeName: string; TypeClass: TIDTypeClass; DataType: TDataTypeID): TIDType;
begin
  Result := RegisterType(TypeName, TypeClass, DataType);
  // äîáàâëÿåì implicit nullptr
  FNullPtrType.OverloadImplicitTo(Result);
end;

procedure TSYSTEMUnit.RegisterSystemRTBuiltinFunctions;
begin
  RegisterBuiltin(TSF_now);
  RegisterBuiltin(TSF_AtomicExchange);
  RegisterBuiltin(TSF_AtomicCmpExchange);
  RegisterBuiltin(TSF_RunError);
  RegisterBuiltin(TSCTF_Defined);
  RegisterBuiltin(TSCTF_Declared);
  RegisterBuiltin(TCT_SizeOf);
  RegisterBuiltin(TCT_LoBound);
  RegisterBuiltin(TCT_HiBound);
  RegisterBuiltin(TCT_Inc);
  RegisterBuiltin(TCT_Dec);
  RegisterBuiltin(TCT_Length);
end;

function TSYSTEMUnit.RegisterBuiltin(const Name: string; MacroID: TBuiltInFunctionID; ResultDataType: TIDType; Flags: TProcFlags = [pfPure]): TIDBuiltInFunction;
begin
  Result := TIDBuiltInFunction.Create(Self.IntfSection, Name, ResultDataType);
  Result.ResultType := ResultDataType;
  Result.Flags := Flags;
  InsertToScope(Result);
end;

function TSYSTEMUnit.RegisterBuiltin(const BuiltinClass: TIDBuiltInFunctionClass): TIDBuiltInFunction;
begin
  Result := BuiltinClass.Register(Self.IntfSection);
end;

procedure TSYSTEMUnit.RegisterBuiltinFunctions;
var
  Decl: TIDBuiltInFunction;
begin
  FArrayType := TIDArray.Create(nil, Identifier('<array type or string>'));
  FRefType := TIDType.Create(nil, Identifier(''));

  // assigned
  Decl := RegisterBuiltin('Assigned', bf_assigned, _Boolean);
  Decl.AddParam('Value', FRefType, [VarConst]);

  // memset
  Decl := RegisterBuiltin('memset', bf_memset, nil);
  Decl.AddParam('Value', FRefType, [VarConstRef]);
  Decl.AddParam('FillChar', _UInt8, [VarConst, VarHasDefault], _ZeroExpression);

  // SetLength
  Decl := RegisterBuiltin('SetLength', bf_setlength, FArrayType);
  Decl.AddParam('Str', FArrayType, [VarInOut]);
  Decl.AddParam('NewLength', _Int32);

  // Copy
  Decl := RegisterBuiltin('Copy', bf_copy, nil);
  Decl.AddParam('Str', FArrayType, [VarInOut]);
  Decl.AddParam('From', _Int32, [VarIn], _ZeroExpression);
  Decl.AddParam('Count', _Int32, [VarIn], _MinusOneExpression);

  // accert
  Decl := RegisterBuiltin('assert', bf_assert, nil);
  Decl.AddParam('Value', _Boolean);
  Decl.AddParam('ErrorText', _String, [], _EmptyStrExpression);

  // TypeInfo
  Decl := RegisterBuiltin('TypeInfo', bf_typeinfo, _TObject);
  Decl.AddParam('Declaration', _Pointer, [VarConst]);

  // Ord(const Ordinal)
  Decl := RegisterBuiltin('Ord', bf_Ord, _Int64);
  Decl.AddParam('Value', _Void, [VarConst]);


  // include(var Set; const SubSet)
  Decl := RegisterBuiltin('include', bf_include, nil);
  Decl.AddParam('Set', _Void, [VarInOut]);
  Decl.AddParam('SubSet', _Void, [VarInOut]);

  // exclude(var Set; const SubSet)
  Decl := RegisterBuiltin('exclude', bf_exclude, nil);
  Decl.AddParam('Set', _Void, [VarInOut]);
  Decl.AddParam('SubSet', _Void, [VarInOut]);
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
    CreateSystemRoutins;
    Result := inherited Compile(False);
    if Result = CompileSuccess then
    begin
      SearchSystemTypes;
      PostCompileProcessUnit;
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
  FNullPtrType := TIDNullPointerType.CreateAsSystem(IntfSection, 'null ptr');
  FNullPtrConstatnt := TIDIntConstant.Create(IntfSection, Identifier('nil'), FNullPtrType, 0);
  FNullPtrExpression := TIDExpression.Create(FNullPtrConstatnt);
  IntfSection.InsertID(FNullPtrConstatnt);

  FUntypedReferenceType := TIDPointer.CreateAsSystem(IntfSection, 'Untyped reference');
  IntfSection.InsertID(FUntypedReferenceType);

  FOrdinalType := TIDOrdinal.CreateAsSystem(nil, 'ordinal');
  FExplicitEnumFromAny := TIDOpExplicitIntToEnum.CreateAsIntOp;
  fExplicitTProcFromAny := TIDOpExplicitTProcFromAny.CreateAsIntOp;

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
  fWideString := RegisterTypeCustom('WideString', TIDString, dtString);
  TIDString(fWideString).ElementDataType := _Char;
  TIDString(fWideString).AddBound(TIDOrdinal(_NativeUInt));
  //===============================================================
  fShortString := RegisterTypeCustom('ShortString', TIDString, dtString);
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
  FGuidType := TIDStructure.CreateAsSystem(IntfSection, '_TGUID');
  FGuidType.DataTypeID := dtGuid;
  FGuidType.AddField('LoDWord', _Int64);
  FGuidType.AddField('HiDWord', _Int64);
  FGuidType.OverloadBinarOperator2(opEqual, FGuidType, _Boolean);
  FGuidType.OverloadBinarOperator2(opNotEqual, FGuidType, _Boolean);
  FGuidType.DataType := _MetaType;
  InsertToScope(FGuidType);
  AddType(FGuidType);
  //===============================================================
  FDateTimeType := TIDAliasType.CreateAliasAsSystem(IntfSection, 'DateTime', _Float64);
  FDateType := TIDAliasType.CreateAliasAsSystem(IntfSection, 'Date', _Float64);
  FTimeType := TIDAliasType.CreateAliasAsSystem(IntfSection, 'Time', _Float64);

  InsertToScope(FDateTimeType);
  InsertToScope(FDateType);
  InsertToScope(FTimeType);
  AddType(FDateTimeType);
  AddType(FDateType);
  AddType(FTimeType);
  //===============================================================
  FPointerType := RegisterRefType('Pointer', TIDPointer, dtPointer) as TIDPointer;
  FPointerType.OverloadBinarOperator2(opEqual, FPointerType, _Boolean);     // ñòàíäàðòíûå îïåðàòîðû
  FPointerType.OverloadBinarOperator2(opNotEqual, FPointerType, _Boolean);
  FPointerType.OverloadBinarOperator2(opEqual, _NilPointer, _Boolean);
  FPointerType.OverloadBinarOperator2(opNotEqual, _NilPointer, _Boolean);

  FPointerType.OverloadBinarOperator2(opAdd, FPointerType, FPointerType);
  FPointerType.OverloadBinarOperator2(opSubtract, FPointerType, FPointerType);

  FPointerType.OverloadBinarOperator2(opAdd, _Int32, FPointerType);
  FPointerType.OverloadBinarOperator2(opSubtract, _Int32, FPointerType);
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

  // todo: make RegisterPointer method
  fPAnsiChar := RegisterPointer('PAnsiChar', _AnsiChar);
  fPChar := RegisterPointer('PWideChar', _Char);
  RegisterTypeAlias('PChar', _Char);
  RegisterTypeAlias('Text', _Pointer);
  //RegisterTypeAlias('FixedInt', _Int32);
  //RegisterTypeAlias('FixedUInt', _UInt32);

  RegisterConstInt('MaxInt', _Int32, MaxInt32);

  // constant "True"
  FTrueConstant := TIDBooleanConstant.Create(IntfSection, Identifier('TRUE'), _Boolean, True);
  FTrueExpression := TIDExpression.Create(FTrueConstant);
  IntfSection.InsertID(FTrueConstant);
  // constant "False"
  FFalseConstant := TIDBooleanConstant.Create(IntfSection, Identifier('FALSE'), _Boolean, False);
  FFalseExpression := TIDExpression.Create(FFalseConstant);
  IntfSection.InsertID(FFalseConstant);
  // constant "0"
  FZeroConstant := TIDIntConstant.CreateAnonymous(IntfSection, _UInt8, 0);
  FZeroExpression := TIDExpression.Create(FZeroConstant);
  // constant "1"
  FOneConstant := TIDIntConstant.CreateAnonymous(IntfSection, _UInt8, 1);
  FOneExpression := TIDExpression.Create(FOneConstant);
  // constant "-1"
  FMinusOneConstant := TIDIntConstant.CreateAnonymous(IntfSection, _Int32, -1);
  FMinusOneExpression := TIDExpression.Create(FMinusOneConstant);
  // constant ""
  FEmptyStrConstant := TIDStringConstant.CreateAnonymous(IntfSection, _String, '');
  FEmptyStrExpression := TIDExpression.Create(FEmptyStrConstant);
  // constant for deprecated
  fDeprecatedDefaultStr := TIDStringConstant.CreateAsSystem(IntfSection, 'The declaration is deprecated');

  AddImplicists;
  AddExplicists;
  AddArithmeticOperators;
  AddLogicalOperators;
  AddBitwiseOperators;
  AddCompareOperators;
  RegisterBuiltinFunctions;
end;

procedure TSYSTEMUnit.CreateSystemRoutins;
begin
  RegisterSystemRTBuiltinFunctions;
end;

procedure TSYSTEMUnit.InsertToScope(Declaration: TIDDeclaration);
begin
  if Assigned(IntfSection.InsertNode(Declaration.Name, Declaration)) then
    raise Exception.CreateFmt('Unit SYSTEM: ' + msgIdentifierRedeclaredFmt, [Declaration.Name]);
end;

function TSYSTEMUnit.CompileIntfOnly: TCompilerResult;
begin
  Result := Compile();
end;

function TSYSTEMUnit.RegisterTypeAlias(const TypeName: string; OriginalType: TIDType): TIDAliasType;
begin
  Result := TIDAliasType.CreateAliasAsSystem(IntfSection, TypeName, OriginalType);
  Result.Elementary := True;
  InsertToScope(Result);
  AddType(Result);
end;

function TSYSTEMUnit.RegisterConstInt(const Name: string; DataType: TIDType; Value: Int64): TIDIntConstant;
begin
  Result := TIDIntConstant.CreateAsSystem(IntfSection, Name);
  Result.DataType := DataType;
  Result.Value := Value;
  InsertToScope(Result);
end;

function TSYSTEMUnit.RegisterPointer(const TypeName: string; TargetType: TIDType): TIDPointer;
begin
  Result := TIDPointer.CreateAsSystem(IntfSection, TypeName);
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
