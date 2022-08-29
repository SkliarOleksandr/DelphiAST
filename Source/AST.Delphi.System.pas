unit AST.Delphi.System;

interface

{$i compilers.inc}

uses System.Classes,
     System.SysUtils,
     AST.Pascal.Parser,
     AST.Delphi.Classes,
     AST.Delphi.DataTypes,
     AST.Delphi.Operators,
     AST.Parser.Utils,
     AST.Parser.Messages,
     AST.Classes,
     AST.Intf,
     AST.Delphi.Contexts,
     AST.Delphi.Parser, AST.Delphi.Intf;
     // System

type
  TSYSTEMUnit = class;


  TIDBuiltInFunction = class(TIDProcedure)
  protected
    class function GetFunctionID: TBuiltInFunctionID; virtual; abstract;
    class function CreateTMPExpr(const EContext: TEContext; const DataType: TIDType): TIDExpression;
  public
    constructor Create(Scope: TScope; const Name: string; ResultType: TIDType); reintroduce; virtual;
    /////////////////////////////////////////////////////////////////////////////////////////
    property FunctionID: TBuiltInFunctionID read GetFunctionID;
    class function CreateDecl(SyUtit: TSYSTEMUnit; Scope: TScope): TIDBuiltInFunction; virtual; abstract;
  end;
  TIDBuiltInFunctionClass = class of TIDBuiltInFunction;


  TIDSysRuntimeFunction = class(TIDBuiltInFunction)
  protected
    class function GetFunctionID: TBuiltInFunctionID; override;
  public
    function Process(var EContext: TEContext): TIDExpression; virtual;
  end;


  TSysFunctionContext = record
    UN: TASTDelphiUnit;
    Scope: TScope;
    ParamsStr: string;
    EContext: ^TEContext;
    SContext: ^TSContext;
  end;

  TIDSysCompileFunction = class(TIDBuiltInFunction)
  protected
    class function GetFunctionID: TBuiltInFunctionID; override;
  public
    function Process(const Ctx: TSysFunctionContext): TIDExpression; virtual; abstract;
  end;

  TIDSysRuntimeFunctionClass = class of TIDSysRuntimeFunction;
//  TIDSysCompileFunctionClass = class of TIDSysCompileFunction;

  TSystemOperatos = record
  public
    // implicits
    ImplicitVariantToAny,
    ImplicitVariantFromAny,
    ImplicitStringFromAny,
    ImplicitCharToAnsiChar,
    ImplicitCharToString,
    ImplicitRangeFromAny,
    ImplicitStringToAnsiString,
    ImplicitStringToGUID,
    ImplicitStringToPChar,
    ImplicitAnsiStringToString,
    ImplicitPointerToAny,
    ImplicitPointerFromAny,
    ImplicitUntypedFromAny,
    ImplicitClosureToTMethod,
    ImplicitCharToAnsiString,
    ImplicitAnsiCharToAnsiString,
    ImplicitAnsiCharToString,
    ImplicitAnsiCharToWideChar,
    ImplicitMetaClassToGUID,
    ImplicitClassToClass,
    ImplicitArrayToAny,
    ImplicitSetFromAny,
    ImplicitNullPtrToAny,
    ImplicitTVarRecToAny,
    ImplicitTVarRecFromAny,
    ImplicitSysVarFromAny
    : TIDOperator;
    // explicits
    ExplicitStringFromAny,
    ExplicitAnsiStringFromAny,
    ExplicitTProcFromAny,
    ExplicitClassOfToAny,
    ExplicitClassOfFromAny,
    ExplicitPointerToAny,
    ExplicitPointerFromAny,
    ExplicitRecordFromAny,
    ExplicitEnumToAny,
    ExplicitEnumFromAny,
    ExplicitUntypedToAny,
    ExplicitUntypedRefToAny,
    ExplicitCharToAny,
    ExplicitRangeFromAny,
    ExplicitRecordToAny,
    ExplicitStaticArrayToAny: TIDOperator;
    // any cast
    IsOrdinal: TIDOperator;
    // in
    Ordinal_In_Set: TIDOperator;
    // add
    StaticArray_Add: TIDOperator;
    Ptr_IntDiv_Int: TIDOperator;
    // Set Multiplay
    Multiply_Set: TIDOperator;
    // DynArray
    Equal_DynArray: TIDOperator;
    procedure Init(Scope: TScope);
  end;


  TSYSTEMUnit = class(TASTDelphiUnit, IASTDelphiSystemUnit)
  type
    TDataTypes = array[TDataTypeID] of TIDType;
  const
    SystemTypesCount = Ord(dtPointer) + 1;
  private
  var
    fDecls: TDelphiSystemDeclarations;
    fArrayType: TIDArray; // служебный тип для функций Length/SetLength
    fDateTimeType: TIDType;
    fDateType: TIDType;
    fTimeType: TIDType;
    fTypeIDType: TIDType;
    fAsserProc: TIDProcedure;
    fOpenString: TIDType;
    fOperators: TSystemOperatos;
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
    procedure AddTVarRecImplicitOperators;
    procedure RegisterTypes;
    procedure RegisterBuiltinFunctions;
    procedure SystemFixup;
    procedure InsertToScope(Declaration: TIDDeclaration); overload;
    function RegisterType(const TypeName: string; TypeClass: TIDTypeClass; DataType: TDataTypeID): TIDType;
    function RegisterTypeCustom(const TypeName: string; TypeClass: TIDTypeClass; DataType: TDataTypeID): TIDType;
    function RegisterOrdinal(const TypeName: string; DataType: TDataTypeID; LowBound: Int64; HighBound: UInt64): TIDType;
    function RegisterTypeAlias(const TypeName: string; OriginalType: TIDType): TIDAliasType;
    function RegisterPointer(const TypeName: string; TargetType: TIDType): TIDPointer;
    function RegisterConstInt(const Name: string; DataType: TIDType; Value: Int64): TIDIntConstant;
    function RegisterConstStr(Scope: TScope; const Name: string; const Value: string ): TIDStringConstant;
    function RegisterVariable(Scope: TScope; const Name: string; DataType: TIDType): TIDVariable;
    function RegisterBuiltin(const BuiltinClass: TIDBuiltInFunctionClass): TIDBuiltInFunction; overload;
    function GetSystemDeclarations: PDelphiSystemDeclarations; override;
  private
    function GetTypeByID(ID: TDataTypeID): TIDType;
    procedure SearchSystemTypes;
    procedure AddStandardExplicitsTo(const Sources: array of TDataTypeID; Dest: TIDType); overload;
  public
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    constructor Create(const Project: IASTProject; const FileName: string; const Source: string); override;
    function Compile(RunPostCompile: Boolean = True): TCompilerResult; override;
    function CompileIntfOnly: TCompilerResult; override;
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    property DataTypes[ID: TDataTypeID]: TIDType read GetTypeByID;
    property SystemDeclarations: PDelphiSystemDeclarations read GetSystemDeclarations;
    property _Int8: TIDType read fDecls._Int8 write fDecls._Int8;
    property _Int16: TIDType read fDecls._Int16 write fDecls._Int16;
    property _Int32: TIDType read fDecls._Int32 write fDecls._Int32;
    property _Int64: TIDType read fDecls._Int64 write fDecls._Int64;
    property _UInt8: TIDType read fDecls._UInt8 write fDecls._UInt8;
    property _UInt16: TIDType read fDecls._UInt16 write fDecls._UInt16;
    property _UInt32: TIDType read fDecls._UInt32 write fDecls._UInt32;
    property _UInt64: TIDType read fDecls._UInt64 write fDecls._UInt64;
    property _NativeInt: TIDType read fDecls._NativeInt write fDecls._NativeInt;
    property _NativeUInt: TIDType read fDecls._NativeUInt write fDecls._NativeUInt;
    property _Float32: TIDType read fDecls._Float32 write fDecls._Float32;
    property _Float64: TIDType read fDecls._Float64 write fDecls._Float64;
    property _Float80: TIDType read fDecls._Float80 write fDecls._Float80;
    property _Currency: TIDType read fDecls._Currency write fDecls._Currency;
    property _Boolean: TIDType read fDecls._Boolean write fDecls._Boolean;
    property _AnsiChar: TIDType read fDecls._AnsiChar write fDecls._AnsiChar;
    property _WideChar: TIDType read fDecls._WideChar write fDecls._WideChar;
    property _AnsiString: TIDType read fDecls._AnsiString write fDecls._AnsiString;
    property _UnicodeString: TIDType read fDecls._UnicodeString write fDecls._UnicodeString;
    property _ShortString: TIDType read fDecls._ShortString write fDecls._ShortString;
    property _WideString: TIDType read fDecls._WideString write fDecls._WideString;
    property _Variant: TIDType read fDecls._Variant write fDecls._Variant;
    property _NilPointer: TIDType read fDecls._NullPtrType;
    property _TGuid: TIDStructure read fDecls._GuidType;
    property _True: TIDBooleanConstant read fDecls._True;
    property _False: TIDBooleanConstant read fDecls._False;
    property _TrueExpression: TIDExpression read fDecls._TrueExpression;
    property _FalseExpression: TIDExpression read fDecls._FalseExpression;
    property _ZeroConstant: TIDIntConstant read fDecls._ZeroConstant;
    property _ZeroIntExpression: TIDExpression read fDecls._ZeroIntExpression;
    property _ZeroFloatExpression: TIDExpression read fDecls._ZeroFloatExpression;
    property _OneConstant: TIDIntConstant read fDecls._OneConstant;
    property _OneExpression: TIDExpression read fDecls._OneExpression;
    property _NullPtrConstant: TIDIntConstant read fDecls._NullPtrConstant;
    property _NullPtrExpression: TIDExpression read fDecls._NullPtrExpression;
    property _EmptyStrExpression: TIDExpression read fDecls._EmptyStrExpression;
    property _Pointer: TIDPointer read fDecls._PointerType;
    property _UntypedReference: TIDUntypedRef read fDecls._UntypedReference;
    property _Untyped: TIDType read fDecls._Untyped;
    property _TObject: TIDClass read fDecls._TObject;
    property _Exception: TIDClass read fDecls._Exception;
    property _EAssert: TIDClass read fDecls._EAssertClass;
    property _DateTime: TIDType read fDateTimeType;
    property _Date: TIDType read fDateType;
    property _Time: TIDType read fTimeType;
    property _AssertProc: TIDProcedure read fAsserProc;
    property _TypeID: TIDType read fTypeIDType;
    property _DeprecatedDefaultStr: TIDStringConstant read fDecls._DeprecatedDefaultStr;
    property _OrdinalType: TIDType read fDecls._OrdinalType;
    property _PAnsiChar: TIDType read fDecls._PAnsiChar;
    property _PWideChar: TIDType read fDecls._PWideChar;
    property _AnyArrayType: TIDArray read fArrayType;
    property _MetaType: TIDType read fDecls._MetaType;
    property _Void: TIDType read fDecls._Void;
    property _ResStringRecord: TIDType read fDecls._ResStringRecord;
    property Operators: TSystemOperatos read fOperators;
  end;

  TDlphDeclarationHelper = class helper for TIDDeclaration
  private
    function GetSysUnit: TSYSTEMUnit;
  public
    property SYSUnit: TSYSTEMUnit read GetSysUnit;
  end;


implementation

uses AST.Parser.Errors,
     AST.Delphi.Errors,
     AST.Delphi.SysFunctions,
     AST.Delphi.SysOperators,
     AST.Targets,
     AST.Lexer;

{ TDlphDeclarationHelper }

function TDlphDeclarationHelper.GetSysUnit: TSYSTEMUnit;
begin
  if Module is TSYSTEMUnit then
    Result := TSYSTEMUnit(Module)
  else
    Result := TASTDelphiUnit(Module).SysUnit as TSYSTEMUnit;
end;

{ TIDBuiltInFunction }

constructor TIDBuiltInFunction.Create(Scope: TScope; const Name: string; ResultType: TIDType);
begin
  inherited CreateAsSystem(Scope, Name);
  ItemType := itMacroFunction;
  Self.DataType := ResultType;
end;

class function TIDBuiltInFunction.CreateTMPExpr(const EContext: TEContext; const DataType: TIDType): TIDExpression;
var
  Decl: TIDVariable;
begin
  Decl := EContext.SContext.Proc.GetTMPVar(DataType);
  Result := TIDExpression.Create(Decl);
end;

{ TIDSysRuntimeFunction }

class function TIDSysRuntimeFunction.GetFunctionID: TBuiltInFunctionID;
begin
  Result := bf_sysrtfunction;
end;

function TIDSysRuntimeFunction.Process(var EContext: TEContext): TIDExpression;
begin
  AbortWorkInternal('Method "Process" must be override');
  Result := nil;
end;

{ TIDSysCompileFunction }

class function TIDSysCompileFunction.GetFunctionID: TBuiltInFunctionID;
begin
  Result := bf_sysctfunction;
end;

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
    DataType.OverloadImplicitTo(_Variant, Operators.ImplicitVariantFromAny);
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
    _Variant.OverloadImplicitTo(DataTypes[i], Operators.ImplicitVariantToAny);

  // float32
  with _Float32 do begin
    OverloadImplicitTo(_Float32);
    OverloadImplicitTo(_Float64);
    OverloadImplicitTo(_Float80);
    OverloadImplicitTo(_Currency);
    OverloadImplicitTo(_Variant, Operators.ImplicitVariantFromAny);
  end;

  // Float64
  with _Float64 do begin
    OverloadImplicitTo(_Float32);
    OverloadImplicitTo(_Float64);
    OverloadImplicitTo(_Float80);
    OverloadImplicitTo(_Currency);
    OverloadImplicitTo(_Variant, Operators.ImplicitVariantFromAny);
  end;

  // Float80
  with _Float80 do begin
    OverloadImplicitTo(_Float32);
    OverloadImplicitTo(_Float64);
    OverloadImplicitTo(_Currency);
    OverloadImplicitTo(_Variant, Operators.ImplicitVariantFromAny);
  end;

  // Currency
  with _Currency do begin
    OverloadImplicitTo(_Float32);
    OverloadImplicitTo(_Float64);
    OverloadImplicitTo(_Float80);
    OverloadImplicitTo(_Variant, Operators.ImplicitVariantFromAny);
  end;

  // UnicodeString
  _UnicodeString.OverloadImplicitTo(_Variant, Operators.ImplicitVariantFromAny);
  _UnicodeString.OverloadImplicitTo(_AnsiString, Operators.ImplicitStringToAnsiString);
  _UnicodeString.OverloadImplicitTo(_TGuid, Operators.ImplicitStringToGUID);
  _UnicodeString.OverloadImplicitTo(_PWideChar, Operators.ImplicitStringToPChar);
  _UnicodeString.OverloadImplicitTo(_PAnsiChar, Operators.ImplicitStringToPChar);
  _UnicodeString.OverloadImplicitTo(_WideString);
  _UnicodeString.OverloadImplicitFrom(_PWideChar);
  _UnicodeString.OverloadImplicitFromAny(Operators.ImplicitStringFromAny);

  // ShortString
  _ShortString.OverloadImplicitTo(_AnsiString);
  _ShortString.OverloadImplicitTo(_Variant, Operators.ImplicitVariantFromAny);
  _ShortString.OverloadImplicitTo(_PWideChar, Operators.ImplicitStringToPChar);
  _ShortString.OverloadImplicitTo(_PAnsiChar, Operators.ImplicitStringToPChar);
  _ShortString.OverloadImplicitTo(_UnicodeString, Operators.ImplicitAnsiStringToString);
  _ShortString.OverloadImplicitTo(_TGuid, Operators.ImplicitStringToGUID);
  _ShortString.OverloadImplicitFrom(_PAnsiChar);

  // AnsiString
  _AnsiString.OverloadImplicitTo(_Variant, Operators.ImplicitVariantFromAny);
  _AnsiString.OverloadImplicitTo(_PWideChar, Operators.ImplicitStringToPChar);
  _AnsiString.OverloadImplicitTo(_PAnsiChar, Operators.ImplicitStringToPChar);
  _AnsiString.OverloadImplicitTo(_UnicodeString, Operators.ImplicitAnsiStringToString);
  _AnsiString.OverloadImplicitTo(_TGuid, Operators.ImplicitStringToGUID);
  _AnsiString.OverloadImplicitTo(_ShortString);
  _AnsiString.OverloadImplicitTo(_WideString);
  _AnsiString.OverloadImplicitFrom(_PAnsiChar);

  // WideString
  _WideString.OverloadImplicitTo(_UnicodeString);

  // WideChar
  _WideChar.OverloadImplicitTo(_WideChar);
  _WideChar.OverloadImplicitTo(_PWideChar);
  _WideChar.OverloadImplicitTo(_UnicodeString, Operators.ImplicitCharToString);
  _WideChar.OverloadImplicitTo(_AnsiString, Operators.ImplicitCharToAnsiString);
  _WideChar.OverloadImplicitTo(_AnsiChar, Operators.ImplicitCharToAnsiChar);
  _WideChar.OverloadImplicitTo(_Variant, Operators.ImplicitVariantFromAny);

  // AnsiChar
  _AnsiChar.OverloadImplicitTo(_AnsiChar);
  _AnsiChar.OverloadImplicitTo(_PAnsiChar);
  _AnsiChar.OverloadImplicitTo(_AnsiString, Operators.ImplicitAnsiCharToAnsiString);
  _AnsiChar.OverloadImplicitTo(_UnicodeString, Operators.ImplicitAnsiCharToString);
  _AnsiChar.OverloadImplicitTo(_WideChar, Operators.ImplicitAnsiCharToWideChar);
  _AnsiChar.OverloadImplicitTo(_Variant, Operators.ImplicitVariantFromAny);

  _MetaType.OverloadImplicitTo(_TGuid, Operators.ImplicitMetaClassToGUID);

  // Boolean
  _Boolean.OverloadImplicitTo(_Boolean);
  _Boolean.OverloadImplicitTo(_Variant, Operators.ImplicitVariantFromAny);
end;

procedure TSYSTEMUnit.AddStandardExplicitsTo(const Sources: array of TDataTypeID; Dest: TIDType);
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
  AddBaseExplicits(_WideChar);
  AddBaseExplicits(_AnsiChar);

  AddExplicits(_Float32, dtFloat32, dtFloat64);
  AddExplicits(_Float64, dtFloat32, dtFloat64);
  AddExplicits(_Float32, dtInt8, dtNativeUInt);
  AddExplicits(_Float64, dtInt8, dtNativeUInt);

  // String
  _UnicodeString.OverloadExplicitTo(_Pointer);
  _UnicodeString.OverloadExplicitTo(_NativeInt);
  _UnicodeString.OverloadExplicitTo(_NativeUInt);
  _UnicodeString.OverloadExplicitTo(_PWideChar);
  _UnicodeString.OverloadExplicitTo(_WideString);
  _UnicodeString.OverloadExplicitTo(_AnsiString);
  _UnicodeString.OverloadExplicitTo(_ShortString);
  _UnicodeString.OverloadExplicitFromAny(Operators.ExplicitStringFromAny);

  // AnsiString
  _AnsiString.OverloadExplicitTo(_Pointer);
  _AnsiString.OverloadExplicitTo(_NativeInt);
  _AnsiString.OverloadExplicitTo(_NativeUInt);
  _AnsiString.OverloadExplicitTo(_PAnsiChar);
  _AnsiString.OverloadExplicitTo(_ShortString);
  _AnsiString.OverloadExplicitFromAny(Operators.ExplicitAnsiStringFromAny);

  // WideString
  _WideString.OverloadExplicitTo(_UnicodeString);
  _WideString.OverloadExplicitTo(_Pointer);
  _WideString.OverloadExplicitTo(_NativeInt);
  _WideString.OverloadExplicitTo(_NativeUInt);

  // AnsiChar
  _AnsiChar.OverloadExplicitTo(_WideChar);

  // ShortString
  _ShortString.OverloadExplicitTo(_AnsiString);
  _ShortString.OverloadExplicitTo(_UnicodeString);

  // WideChar
  _WideChar.OverloadExplicitTo(_UnicodeString);
  _WideChar.OverloadExplicitToAny(Operators.ExplicitCharToAny);

  _PWideChar.OverloadExplicitTo(_UnicodeString);

  AddStandardExplicitsTo([dtInt8, dtInt16, dtInt32, dtInt64, dtUInt8, dtUInt16, dtUInt32, dtUInt64, dtNativeInt, dtNativeUInt, dtBoolean], _WideChar);
  AddStandardExplicitsTo([dtInt8, dtInt16, dtInt32, dtInt64, dtUInt8, dtUInt16, dtUInt32, dtUInt64, dtNativeInt, dtNativeUInt, dtBoolean], _AnsiChar);
end;

procedure TSYSTEMUnit.AddIntDivOperators;
begin
  AddBinarOperator(opIntDiv, _Int8, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _Int8);
  AddBinarOperator(opIntDiv, _UInt8, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _UInt8);

  AddBinarOperator(opIntDiv, _Int16, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _Int16);
  AddBinarOperator(opIntDiv, _UInt16, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _UInt16);

  AddBinarOperator(opIntDiv, _Int32, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _Int32);
  AddBinarOperator(opIntDiv, _UInt32, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _UInt32);

  AddBinarOperator(opIntDiv, _Int64, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _Int64);
  AddBinarOperator(opIntDiv, _UInt64, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _UInt64);

  AddBinarOperator(opIntDiv, _NativeInt, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _NativeInt);
  AddBinarOperator(opIntDiv, _NativeUInt, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _NativeUInt);
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
  AddBinarOperator(opMultiply, _Int8, [_Int8, _UInt8, _Int16, _UInt16], _Int32);

  AddBinarOperator(opMultiply, _UInt8, [_Int8, _Int16], _Int32);
  AddBinarOperator(opMultiply, _UInt8, [_UInt8, _UInt16], _UInt32);

  AddBinarOperator(opMultiply, _Int16, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32], _Int32);
  AddBinarOperator(opMultiply, _Int16, [_Int64, _UInt64], _Int64);

  AddBinarOperator(opMultiply, _UInt16, [_UInt8, _UInt16, _UInt32], _UInt32);
  AddBinarOperator(opMultiply, _UInt16, [_Int8, _Int16, _Int32], _Int32);

  AddBinarOperator(opMultiply, _Int32, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32], _Int32);
  AddBinarOperator(opMultiply, _UInt32, [_UInt8, _UInt16, _UInt32], _UInt32);

  AddBinarOperator(opMultiply, _Int64, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _Int64);
  AddBinarOperator(opMultiply, _UInt64, [_UInt8, _UInt16, _UInt32, _UInt64], _UInt64);

  AddBinarOperator(opMultiply, _NativeInt, _NativeInt, _NativeInt);
  AddBinarOperator(opMultiply, _NativeUInt, _NativeUInt, _NativeUInt);

  // int * float
  AddBinarOperator(opMultiply, _Float32, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64, _Float32, _Float64], _Float64);
  AddBinarOperator(opMultiply, _Float64, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64, _Float32, _Float64], _Float64);
  AddBinarOperator(opMultiply, _Float80, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64, _Float32, _Float64], _Float80);

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
  AddBinarOperator(opSubtract, _Float80, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64, _Float32, _Float64, _Float80, _Currency], _Float80);

  AddBinarOperator(opSubtract, _Variant, _Variant, _Variant);
end;

procedure TSYSTEMUnit.AddTVarRecImplicitOperators;
begin
  fDecls._TVarRec.OverloadExplicitToAny(Operators.ImplicitTVarRecToAny);
  fDecls._TVarRec.OverloadExplicitFromAny(Operators.ImplicitTVarRecFromAny);
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
  AddBinarOperator(opAdd, _Float80, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64, _Float32, _Float64, _Float80, _Currency], _Float80);

  // strings
  AddBinarOperator(opAdd, _UnicodeString, _UnicodeString, _UnicodeString);
  AddBinarOperator(opAdd, _WideChar, _WideChar, _WideChar);
  AddBinarOperator(opAdd, _AnsiString, _AnsiString, _AnsiString);
  AddBinarOperator(opAdd, _AnsiChar, _AnsiChar, _AnsiChar);

  AddBinarOperator(opAdd, _Variant, _Variant, _Variant);
end;

procedure TSYSTEMUnit.AddDivOperators;
begin
  AddBinarOperator(opDivide, _Int8, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _Float64);
  AddBinarOperator(opDivide, _Int16, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _Float64);
  AddBinarOperator(opDivide, _Int32, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _Float64);
  AddBinarOperator(opDivide, _Int64, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _Float64);

  AddBinarOperator(opDivide, _Float32, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64, _Float32, _Float64], _Float64);
  AddBinarOperator(opDivide, _Float64, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64, _Float32, _Float64], _Float64);
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

    AddBinarOperator(Op, _Float32, _Float80, _Boolean);
    AddBinarOperator(Op, _Float64, _Float80, _Boolean);
    AddBinarOperator(Op, _Float80, _Float80, _Boolean);

    AddBinarOperator(Op, _Float32, _Currency, _Boolean);
    AddBinarOperator(Op, _Float64, _Currency, _Boolean);
    AddBinarOperator(Op, _Float80, _Currency, _Boolean);

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

  // Char
  AddBinarOperator(opEqual, _WideChar, _WideChar, _Boolean);
  AddBinarOperator(opNotEqual, _WideChar, _WideChar, _Boolean);
  AddBinarOperator(opLess, _WideChar, _WideChar, _Boolean);
  AddBinarOperator(opLessOrEqual, _WideChar, _WideChar, _Boolean);
  AddBinarOperator(opGreater, _WideChar, _WideChar, _Boolean);
  AddBinarOperator(opGreaterOrEqual, _WideChar, _WideChar, _Boolean);

  // AnsiChar
  AddBinarOperator(opEqual, _AnsiChar, _AnsiChar, _Boolean);
  AddBinarOperator(opNotEqual, _AnsiChar, _AnsiChar, _Boolean);
  AddBinarOperator(opLess, _AnsiChar, _AnsiChar, _Boolean);
  AddBinarOperator(opLessOrEqual, _AnsiChar, _AnsiChar, _Boolean);
  AddBinarOperator(opGreater, _AnsiChar, _AnsiChar, _Boolean);
  AddBinarOperator(opGreaterOrEqual, _AnsiChar, _AnsiChar, _Boolean);

  // ShortString
  AddBinarOperator(opEqual, _ShortString, _ShortString, _Boolean);
  AddBinarOperator(opNotEqual, _ShortString, _ShortString, _Boolean);
  AddBinarOperator(opLess, _ShortString, _ShortString, _Boolean);
  AddBinarOperator(opLessOrEqual, _ShortString, _ShortString, _Boolean);
  AddBinarOperator(opGreater, _ShortString, _ShortString, _Boolean);
  AddBinarOperator(opGreaterOrEqual, _ShortString, _ShortString, _Boolean);

  // AnsiString
  AddBinarOperator(opEqual, _AnsiString, _AnsiString, _Boolean);
  AddBinarOperator(opNotEqual, _AnsiString, _AnsiString, _Boolean);
  AddBinarOperator(opLess, _AnsiString, _AnsiString, _Boolean);
  AddBinarOperator(opLessOrEqual, _AnsiString, _AnsiString, _Boolean);
  AddBinarOperator(opGreater, _AnsiString, _AnsiString, _Boolean);
  AddBinarOperator(opGreaterOrEqual, _AnsiString, _AnsiString, _Boolean);

  // String
  AddBinarOperator(opEqual, _UnicodeString, _UnicodeString, _Boolean);
  AddBinarOperator(opNotEqual, _UnicodeString, _UnicodeString, _Boolean);
  AddBinarOperator(opLess, _UnicodeString, _UnicodeString, _Boolean);
  AddBinarOperator(opLessOrEqual, _UnicodeString, _UnicodeString, _Boolean);
  AddBinarOperator(opGreater, _UnicodeString, _UnicodeString, _Boolean);
  AddBinarOperator(opGreaterOrEqual, _UnicodeString, _UnicodeString, _Boolean);

  // WideString
  AddBinarOperator(opEqual, _WideString, _WideString, _Boolean);
  AddBinarOperator(opNotEqual, _WideString, _WideString, _Boolean);
  AddBinarOperator(opLess, _WideString, _WideString, _Boolean);
  AddBinarOperator(opLessOrEqual, _WideString, _WideString, _Boolean);
  AddBinarOperator(opGreater, _WideString, _WideString, _Boolean);
  AddBinarOperator(opGreaterOrEqual, _WideString, _WideString, _Boolean);
end;

function TSYSTEMUnit.RegisterType(const TypeName: string; TypeClass: TIDTypeClass; DataType: TDataTypeID): TIDType;
begin
  Result := TypeClass.CreateAsSystem(IntfScope, TypeName);
  Result.Elementary := True;
  Result.DataTypeID := DataType;
  Result.ItemType := itType;
  InsertToScope(Result);
  AddType(Result);
end;

function TSYSTEMUnit.RegisterTypeCustom(const TypeName: string; TypeClass: TIDTypeClass;
  DataType: TDataTypeID): TIDType;
begin
  Result := TypeClass.CreateAsSystem(IntfScope, TypeName);
  Result.Elementary := True;
  Result.DataTypeID := DataType;
  Result.ItemType := itType;
  InsertToScope(Result);
  AddType(Result);
end;

procedure TSYSTEMUnit.RegisterTypes;
begin
  //===============================================================
  _Int8 := RegisterOrdinal('ShortInt', dtInt8, MinInt8, MaxInt8);
  _Int16 := RegisterOrdinal('SmallInt', dtInt16, MinInt16, MaxInt16);
  _Int32 := RegisterOrdinal('Integer', dtInt32, MinInt32, MaxInt32);
  _Int64 := RegisterOrdinal('Int64', dtInt64, MinInt64, MaxInt64);
  _UInt8 := RegisterOrdinal('Byte', dtUInt8, 0, MaxUInt8);
  _UInt16 := RegisterOrdinal('Word', dtUInt16, 0, MaxUInt16);
  _UInt32 := RegisterOrdinal('Cardinal', dtUInt32, 0, MaxUInt32);
  _UInt64 := RegisterOrdinal('UInt64', dtUInt64, 0, MaxUInt64);
  _NativeInt := RegisterOrdinal('NativeInt', dtNativeInt, MinInt64, MaxInt64);
  _NativeUInt := RegisterOrdinal('NativeUInt', dtNativeUInt, 0, MaxUInt64);
  _Float32 := RegisterType('Single', TIDType, dtFloat32);
  _Float64 := RegisterType('Double', TIDType, dtFloat64);
  _Float80 := RegisterType('Extended', TIDType, dtFloat80);
  _Currency := RegisterType('Currency', TIDType, dtCurrency);

  //===============================================================
  _Boolean := RegisterOrdinal('Boolean', dtBoolean, 0, 1);
  _Boolean.OverloadExplicitFromAny(Operators.IsOrdinal);

  _AnsiChar := RegisterOrdinal('AnsiChar', dtAnsiChar, 0, MaxUInt8);
  _WideChar := RegisterOrdinal('Char', dtChar, 0, MaxUInt16);
  //===============================================================
  _ShortString := RegisterType('ShortString', TIDString, dtShortString);
  TIDString(_ShortString).ElementDataType := _AnsiChar;
  //===============================================================
  _AnsiString := RegisterType('AnsiString', TIDString, dtAnsiString);
  TIDString(_AnsiString).ElementDataType := _AnsiChar;
  //===============================================================
  _UnicodeString := RegisterType('String', TIDString, dtString);
  TIDString(_UnicodeString).ElementDataType := _WideChar;
  //===============================================================
  _Variant := RegisterType('Variant', TIDVariant, dtVariant);
  //===============================================================
  _WideString := RegisterType('WideString', TIDString, dtWideString);
  TIDString(_WideString).ElementDataType := _WideChar;
  //===============================================================
  fOpenString := RegisterTypeCustom('OpenString', TIDString, dtString);
  TIDString(fOpenString).ElementDataType := _WideChar;
  //===============================================================
  // TObject ========================================================
  {FTObject := TIDClass.CreateAsSystem(UnitInterface, 'TObject');
  FTObject.NeedForward := True; // forward declaration
  InsertToScope(FTObject);}
  // TGUID ========================================================
  fDecls._GuidType := TIDRecord.CreateAsSystem(IntfScope, 'TGUID');
  fDecls._GuidType.NeedForward := True;
  fDecls._GuidType.DataTypeID := dtGuid;
  fDecls._GuidType.DataType := _MetaType;
  InsertToScope(fDecls._GuidType);
  AddType(fDecls._GuidType);
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
  fDecls._PointerType := RegisterPointer('Pointer', nil);
  //===============================================================

  // nil constant
  fDecls._NullPtrType := TIDNullPointerType.CreateAsSystem(IntfScope, 'null ptr');
  fDecls._NullPtrConstant := TIDIntConstant.Create(IntfScope, Identifier('nil'), fDecls._NullPtrType, 0);
  fDecls._NullPtrExpression := TIDExpression.Create(fDecls._NullPtrConstant);
  IntfScope.InsertID(fDecls._NullPtrConstant);

  fDecls._PointerType.CreateStandardOperators;
  fDecls._NullPtrType.CreateStandardOperators;

  // Untyped reference
  fDecls._UntypedReference := TIDUntypedRef.CreateAsSystem(IntfScope, '$Untyped reference');
  IntfScope.InsertID(fDecls._UntypedReference);
  fDecls._UntypedReference.OverloadImplicitFromAny(Operators.ImplicitUntypedFromAny);
  fDecls._UntypedReference.OverloadExplicitToAny(Operators.ExplicitUntypedRefToAny);

  // Untyped
  fDecls._Untyped := TIDOrdinal.CreateAsSystem(IntfScope, '$Untyped');
  fDecls._Untyped.DataTypeID := dtNativeUInt;
  IntfScope.InsertID(fDecls._Untyped);
  fDecls._Untyped.OverloadExplicitToAny(Operators.ExplicitUntypedToAny);


  // Delphi system aliases
  RegisterTypeAlias('LongInt', _Int32);
  RegisterTypeAlias('LongWord', _UInt32);
  RegisterTypeAlias('Comp', _Int64);
  RegisterTypeAlias('_ShortString', _ShortString);
  RegisterTypeAlias('UnicodeString', _UnicodeString);
  RegisterTypeAlias('WideChar', _WideChar);

  RegisterTypeAlias('ByteBool', _Boolean);
  RegisterTypeAlias('WordBool', _Boolean);
  RegisterTypeAlias('LongBool', _Boolean);
  RegisterTypeAlias('OleVariant', _Variant);

  // PAnsiChar
  fDecls._PAnsiChar := RegisterType('PAnsiChar', TIDPointer, dtPAnsiChar);
  TIDPointer(fDecls._PAnsiChar).ReferenceType := _AnsiChar;
  fDecls._PAnsiChar.OverloadImplicitTo(_Variant);
  fDecls._PAnsiChar.CreateStandardOperators;

  // PWideChar
  fDecls._PWideChar := RegisterType('PWideChar', TIDPointer, dtPWideChar);
  TIDPointer(fDecls._PWideChar).ReferenceType := _WideChar;
  fDecls._PWideChar.OverloadImplicitTo(_Variant);
  fDecls._PWideChar.CreateStandardOperators;

  _AnsiChar.DefaultReference := _PAnsiChar;
  _WideChar.DefaultReference := _PWideChar;

  RegisterTypeAlias('PChar', _PWideChar);
  RegisterTypeAlias('Text', _Pointer);

  fDecls._MaxIntConstant := RegisterConstInt('MaxInt', _Int32, MaxInt32);
  fDecls._MaxIntExpression := TIDExpression.Create(fDecls._MaxIntConstant, 0);

  // constant "True"
  fDecls._True := TIDBooleanConstant.Create(IntfScope, Identifier('TRUE'), _Boolean, True);
  fDecls._TrueExpression := TIDExpression.Create(fDecls._True);
  IntfScope.InsertID(fDecls._True);
  // constant "False"
  fDecls._False := TIDBooleanConstant.Create(IntfScope, Identifier('FALSE'), _Boolean, False);
  fDecls._FalseExpression := TIDExpression.Create(fDecls._False);
  IntfScope.InsertID(fDecls._False);
  // constant "0"
  fDecls._ZeroConstant := TIDIntConstant.CreateAsAnonymous(IntfScope, _UInt8, 0);
  fDecls._ZeroIntExpression := TIDExpression.Create(fDecls._ZeroConstant);

  fDecls._ZeroFloatExpression := TIDExpression.Create(TIDFloatConstant.CreateAsAnonymous(IntfScope, _Float64, 0));

  // constant "1"
  fDecls._OneConstant := TIDIntConstant.CreateAsAnonymous(IntfScope, _UInt8, 1);
  fDecls._OneExpression := TIDExpression.Create(fDecls._OneConstant);
  // constant ""
  fDecls._EmptyStrConstant := TIDStringConstant.CreateAsAnonymous(IntfScope, _UnicodeString, '');
  fDecls._EmptyStrExpression := TIDExpression.Create(fDecls._EmptyStrConstant);
  // constant "[]"
  fDecls._EmptyArrayConstant := TIDDynArrayConstant.CreateAsAnonymous(IntfScope, _Void, []);

  // constant for deprecated
  fDecls._DeprecatedDefaultStr := TIDStringConstant.CreateAsSystem(IntfScope, 'The declaration is deprecated');

  AddImplicists;
  AddExplicists;
  AddArithmeticOperators;
  AddLogicalOperators;
  AddBitwiseOperators;
  AddCompareOperators;
end;

function TSYSTEMUnit.RegisterVariable(Scope: TScope; const Name: string; DataType: TIDType): TIDVariable;
begin
  Result := TIDVariable.CreateAsSystem(Scope, Name);
  Result.DataType := DataType;
  InsertToScope(Scope, Result);
end;

procedure TSYSTEMUnit.SearchSystemTypes;
begin
  // todo: use forward declaration instead
  fDecls._TObject := GetPublicClass('TObject');
  fDecls._ResStringRecord := GetPublicType('PResStringRec');
  fDecls._TVarRec := GetPublicType('TVarRec');
  if Assigned(fDecls._TVarRec) then
    AddTVarRecImplicitOperators;
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

function TSYSTEMUnit.RegisterBuiltin(const BuiltinClass: TIDBuiltInFunctionClass): TIDBuiltInFunction;
begin
  Result := BuiltinClass.CreateDecl(Self, Self.IntfScope);
  InsertToScope(Self.IntfScope, Result);
end;

procedure TSYSTEMUnit.RegisterBuiltinFunctions;
begin
  RegisterBuiltin(TSF_Abs);
  RegisterBuiltin(TSF_Assert);
  RegisterBuiltin(TSF_Assigned);
  RegisterBuiltin(TSF_AtomicCmpExchange);
  RegisterBuiltin(TSF_AtomicDecrement);
  RegisterBuiltin(TSF_AtomicExchange);
  RegisterBuiltin(TSF_AtomicIncrement);
  RegisterBuiltin(TSF_Copy);
  RegisterBuiltin(TSF_Chr);
  RegisterBuiltin(TSF_Close);
  RegisterBuiltin(TCT_Dec);
  RegisterBuiltin(TSF_Dispose);
  RegisterBuiltin(TSCTF_Defined);
  RegisterBuiltin(TSCTF_Default);
  RegisterBuiltin(TSCTF_Console);
  RegisterBuiltin(TSCTF_TypeName);
  RegisterBuiltin(TSCTF_Declared);
  RegisterBuiltin(TSF_Delete);
  RegisterBuiltin(TSF_Exit);
  RegisterBuiltin(TSF_Exclude);
  RegisterBuiltin(TSF_FreeMem);
  RegisterBuiltin(TSF_FillChar);
  RegisterBuiltin(TSF_GetMem);
  RegisterBuiltin(TSF_GetDir);
  RegisterBuiltin(TSF_Halt);
  RegisterBuiltin(TSF_HiBound);
  RegisterBuiltin(TSF_Include);
  RegisterBuiltin(TSF_Inc);
  RegisterBuiltin(TSF_Insert);
  RegisterBuiltin(TSF_LoBound);
  RegisterBuiltin(TSF_Length);
  RegisterBuiltin(TSF_New);
  RegisterBuiltin(TSF_Now);
  RegisterBuiltin(TSF_Ord);
  RegisterBuiltin(TSF_Odd);
  RegisterBuiltin(TSF_Pi);
  RegisterBuiltin(TSF_Pred);
  RegisterBuiltin(TSF_ReallocMem);
  RegisterBuiltin(TSF_Round);
  RegisterBuiltin(TSF_RunError);
  RegisterBuiltin(TSF_Str);
  RegisterBuiltin(TSF_Sqr);
  RegisterBuiltin(TCT_SizeOf);
  RegisterBuiltin(TSF_Succ);
  RegisterBuiltin(TSF_SetLength);
  RegisterBuiltin(TSF_SetString);
  RegisterBuiltin(TSF_Swap);
  RegisterBuiltin(TSF_Trunc);
  RegisterBuiltin(TSF_Val);
  RegisterBuiltin(TSF_ReturnAddress);

  RegisterVariable(ImplScope, 'ReturnAddress', _Pointer);
  RegisterConstStr(ImplScope, 'libmmodulename', '');
  RegisterConstInt('CompilerVersion', _Int32, 35); // Delphi 11.x
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
  Result := CompileInProgress;
  try
    RegisterBuiltinFunctions;
    SystemFixup;
    Result := inherited Compile(False);
    if Result = CompileSuccess then
    begin
      SearchSystemTypes;
      fCompiled := CompileSuccess;
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
  fSysDecls := @fDecls;

  {$IFDEF DEBUG}
  SetUnitName('system');
  {$ENDIF}

  fOperators.Init(IntfScope);

  fDecls._MetaType := TIDType.CreateAsSystem(IntfScope, 'MetaType');
  fDecls._MetaType.DataTypeID := dtClass;

  fDecls._Void := TIDType.CreateAsSystem(IntfScope, 'Void');
  fDecls._Void.DataTypeID := TDataTypeID(dtUnknown);



  fDecls._OrdinalType := TIDOrdinal.CreateAsSystem(IntfScope, 'ordinal');

  RegisterTypes;
//  fDecls._PointerType.CreateStandardOperators;
//  fDecls._PAnsiChar.CreateStandardOperators;
//  fDecls._PChar.CreateStandardOperators;

  fArrayType := TIDArray.CreateAsSystem(IntfScope, 'array');
end;

function TSYSTEMUnit.GetTypeByID(ID: TDataTypeID): TIDType;
begin
  Result := fDecls.DataTypes[ID];
end;

function TSYSTEMUnit.GetSystemDeclarations: PDelphiSystemDeclarations;
begin
  Result := addr(fDecls);
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
  Result.DataType := _UnicodeString;
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

{ TSystemOperatos }

procedure TSystemOperatos.Init(Scope: TScope);
begin
  // implicit
  ImplicitVariantToAny := TSysImplicitVariantToAny.CreateAsSystem(Scope);
  ImplicitVariantFromAny := TSysImplicitVariantFromAny.CreateAsSystem(Scope);
  ImplicitStringFromAny := TSysImplicitStringFromAny.CreateAsSystem(Scope);
  ImplicitCharToAnsiChar := TSysImplicitCharToAnsiChar.CreateAsSystem(Scope);
  ImplicitCharToString := TSysImplicitCharToString.CreateAsSystem(Scope);
  ImplicitRangeFromAny := TSysImplicitRangeFromAny.CreateAsSystem(Scope);
  ImplicitStringToAnsiString := TSysImplicitStringToAnsiString.CreateAsSystem(Scope);
  ImplicitStringToGUID := TSysImplicitStringToGUID.CreateAsSystem(Scope);
  ImplicitStringToPChar := TSysImplicitStringToPChar.CreateAsSystem(Scope);
  ImplicitAnsiStringToString := TSysImplicitAnsiStringToString.CreateAsSystem(Scope);
  ImplicitPointerToAny := TSysImplicitPointerToAny.CreateAsSystem(Scope);
  ImplicitPointerFromAny := TSysImplicitPointerFromAny.CreateAsSystem(Scope);
  ImplicitUntypedFromAny := TSysImplicitUntypedFromAny.CreateAsSystem(Scope);
  ImplicitClosureToTMethod := TSysImplicitClosureToTMethod.CreateAsSystem(Scope);
  ImplicitCharToAnsiString := TSysImplicitCharToAnsiString.CreateAsSystem(Scope);
  ImplicitAnsiCharToAnsiString := TSysImplicitAnsiCharToAnsiString.CreateAsSystem(Scope);
  ImplicitAnsiCharToString := TSysImplicitAnsiCharToString.CreateAsSystem(Scope);
  ImplicitAnsiCharToWideChar := TSysImplicitAnsiCharToWideChar.CreateAsSystem(Scope);
  ImplicitMetaClassToGUID := TSysImplicitMetaClassToGUID.CreateAsSystem(Scope);
  ImplicitClassToClass := TSysImplicitClassToClass.CreateAsSystem(Scope);
  ImplicitArrayToAny := TSysImplicitArrayToAny.CreateAsSystem(Scope);
  ImplicitSetFromAny := TSysImplicitSetFromAny.CreateAsSystem(Scope);
  ImplicitNullPtrToAny := TSysImplicitNullPtrToAny.CreateAsSystem(Scope);
  ImplicitTVarRecToAny := TSysImplicitTVarRecToAny.CreateAsSystem(Scope);
  ImplicitTVarRecFromAny := TSysImplicitTVarRecFromAny.CreateAsSystem(Scope);
  ImplicitSysVarFromAny := TSysImplicitSysVarFromAny.CreateAsSystem(Scope);
  // explicit
  ExplicitStringFromAny := TSysExplicitStringFromAny.CreateAsSystem(Scope);
  ExplicitAnsiStringFromAny := TSysExplicitAnsiStringFromAny.CreateAsSystem(Scope);
  ExplicitTProcFromAny := TSysExplicitTProcFromAny.CreateAsSystem(Scope);
  ExplicitClassOfToAny := TSysExplicitClassOfToAny.CreateAsSystem(Scope);
  ExplicitClassOfFromAny := TSysExplicitClassOfFromAny.CreateAsSystem(Scope);
  ExplicitPointerToAny := TSysExplictPointerToAny.CreateAsSystem(Scope);
  ExplicitPointerFromAny := TSysExplictPointerFromAny.CreateAsSystem(Scope);
  ExplicitRecordFromAny := TSysExplicitRecordFromAny.CreateAsSystem(Scope);
  ExplicitEnumToAny := TSysExplicitEnumToAny.CreateAsSystem(Scope);
  ExplicitEnumFromAny := TSysExplicitEnumFromAny.CreateAsSystem(Scope);
  ExplicitUntypedToAny := TSysExplicitUntypedToAny.CreateAsSystem(Scope);
  ExplicitUntypedRefToAny := TSysExplicitUntypedRefToAny.CreateAsSystem(Scope);
  ExplicitCharToAny := TSysExplicitCharToAny.CreateAsSystem(Scope);
  ExplicitRangeFromAny := TSysExplicitRangeFromAny.CreateAsSystem(Scope);
  ExplicitRecordToAny := TSysExplicitRecordToAny.CreateAsSystem(Scope);
  ExplicitStaticArrayToAny := TSysExplicitStaticArrayToAny.CreateAsSystem(Scope);
  // any cast
  IsOrdinal := TSysTypeCast_IsOrdinal.CreateAsSystem(Scope);

  // in
  Ordinal_In_Set := TSysOrdinalInSet.CreateAsSystem(Scope);
  // add
  StaticArray_Add := TSys_StaticArray_Add.CreateAsSystem(Scope);
  Ptr_IntDiv_Int := TSys_Ptr_IntDiv_Int.CreateAsSystem(Scope);

  Multiply_Set := TSys_Multiply_Set.CreateAsSystem(Scope);

  Equal_DynArray := TSys_Equal_DynArray.CreateAsSystem(Scope);
end;

initialization

finalization

end.
