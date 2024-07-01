unit AST.Delphi.System;

interface

{$i compilers.inc}

uses System.Classes,
     System.SysUtils,
     AST.Pascal.Parser,
     AST.Delphi.Classes,
     AST.Delphi.Declarations,
     AST.Delphi.DataTypes,
     AST.Delphi.Operators,
     AST.Delphi.SysTypes,
     AST.Parser.Utils,
     AST.Parser.Messages,
     AST.Classes,
     AST.Intf,
     AST.Delphi.Errors,
     AST.Delphi.Contexts,
     AST.Delphi.Parser,
     AST.Delphi.Intf;
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
    ArgsCount: Integer;
    EContext: ^TEContext;
    SContext: PSContext;
    ERRORS: TASTDelphiErrors;
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
    ImplicitRangeToAny,
    ImplicitStringToAnsiString,
    ImplicitStringToGUID,
    ImplicitStringToPChar,
    ImplicitAnsiStringToString,
    ImplicitPointerToAny,
    ImplicitPointerFromAny,
    ImplicitUntypedFromAny,
    ImplicitClosureToTMethod,
    ImplicitCharToAnsiString,
    ImplicitAnsiStringFromAny,
    ImplicitAnsiCharToAnsiString,
    ImplicitAnsiCharToString,
    ImplicitAnsiCharToWideChar,
    ImplicitMetaClassToGUID,
    ImplicitClassToClass,
    ImplicitArrayToAny,
    ImplicitArrayFromAny,
    ImplicitSetFromAny,
    ImplicitNullPtrToAny,
    ImplicitTVarRecToAny,
    ImplicitTVarRecFromAny,
    ImplicitSysVarFromAny,
    ImplicitTProcFromAny
    : TIDOperator;
    // explicits
    ExplicitStringFromAny,
    ExplicitAnsiStringFromAny,
    ExplicitTProcFromAny,
    ExplicitClassToAny,
    ExplicitClassFromAny,
    ExplicitClassOfToAny,
    ExplicitClassOfFromAny,
    ExplicitInterfaceFromAny,
    ExplicitRefTypeToAny,
    ExplicitRefTypeFromAny,
    ExplicitRecordFromAny,
    ExplicitEnumToAny,
    ExplicitEnumFromAny,
    ExplicitUntypedToAny,
    ExplicitUntypedRefToAny,
    ExplicitCharToAny,
    ExplicitRangeFromAny,
    ExplicitRecordToAny,
    ExplicitStaticArrayToAny,
    ExplicitDynArrayToAny,
    ExplicitVariantToAny,
    ExplicitVariantFromAny: TIDOperator;
    // any cast
    IsOrdinal: TIDOperator;
    // in
    Ordinal_In_Set: TIDOperator;
    // add
    StaticArray_Add: TIDOperator;
    Ptr_IntDiv_Int: TIDOperator;
    // Set Multiplay
    Multiply_Set: TIDOperator;
    Add_Set: TIDOperator;
    Subtract_Set: TIDOperator;
    Equal_Set: TIDOperator;
    NotEqual_Set: TIDOperator;
    // DynArray
    Equal_DynArray: TIDOperator;
    Equal_NullPtr: TIDOperator;
    NotEqual_NullPtr: TIDOperator;
    procedure Init(Scope: TScope);
  end;


  TSYSTEMUnit = class(TASTDelphiUnit, IASTDelphiSystemUnit)
  type
    TDataTypes = array[TDataTypeID] of TIDType;
  const
    SystemTypesCount = Ord(dtPointer) + 1;
  private
  var
    fDecls: TDelphiBuiltinTypes;
    fArrayType: TIDArray; // служебный тип для функций Length/SetLength
    fTypeIDType: TIDType;
    fAsserProc: TIDProcedure;
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
    function RegisterOrdinal(const TypeName: string; DataType: TDataTypeID; LowBound: Int64; HighBound: UInt64): TIDType;
    function RegisterTypeAlias(const TypeName: string; OriginalType: TIDType): TIDAliasType;
    function RegisterPointer(const TypeName: string; TargetType: TIDType): TIDPointer;
    function RegisterConstInt(const Name: string; DataType: TIDType; Value: Int64): TIDIntConstant;
    function RegisterConstStr(Scope: TScope; const Name: string; const Value: string ): TIDStringConstant;
    function RegisterVariable(Scope: TScope; const Name: string; DataType: TIDType): TIDVariable;
    function RegisterBuiltin(const BuiltinClass: TIDBuiltInFunctionClass): TIDBuiltInFunction; overload;
    function GetTypeByID(ID: TDataTypeID): TIDType;
    procedure SearchSystemTypes;
    procedure AddStandardExplicitsTo(const Sources: array of TDataTypeID; Dest: TIDType); overload;
    function Get_AnsiChar: TIDType;
    function Get_AnsiString: TIDType;
    function Get_Boolean: TIDType;
    function Get_Comp: TIDType;
    function Get_Currency: TIDType;
    function Get_EAssertClass: TIDClass;
    function Get_EmptySetType: TIDSet;
    function Get_Exception: TIDClass;
    function Get_Float32: TIDType;
    function Get_Float64: TIDType;
    function Get_Float80: TIDType;
    function Get_GuidType: TIDStructure;
    function Get_Int16: TIDType;
    function Get_Int32: TIDType;
    function Get_Int64: TIDType;
    function Get_Int8: TIDType;
    function Get_MetaType: TIDType;
    function Get_NativeInt: TIDType;
    function Get_NativeUInt: TIDType;
    function Get_NullPtrType: TIDType;
    function Get_OpenString: TIDString;
    function Get_OrdinalType: TIDType;
    function Get_PAnsiChar: TIDType;
    function Get_PointerType: TIDPointer;
    function Get_PWideChar: TIDType;
    function Get_ResStringRecord: TIDType;
    function Get_ShortString: TIDType;
    function Get_TObject: TIDClass;
    function Get_TTypeKind: TIDEnum;
    function Get_UInt16: TIDType;
    function Get_UInt32: TIDType;
    function Get_UInt64: TIDType;
    function Get_UInt8: TIDType;
    function Get_UnicodeString: TIDType;
    function Get_Untyped: TIDType;
    function Get_UntypedReference: TIDUntypedRef;
    function Get_Variant: TIDType;
    function Get_Void: TIDType;
    function Get_WideChar: TIDType;
    function Get_WideString: TIDType;
    function Get_DeprecatedDefaultStr: TIDStringConstant;
    function Get_EmptyStrExpression: TIDExpression;
    function Get_False: TIDBooleanConstant;
    function Get_FalseExpression: TIDExpression;
    function Get_NullPtrConstant: TIDIntConstant;
    function Get_NullPtrExpression: TIDExpression;
    function Get_OneConstant: TIDIntConstant;
    function Get_OneExpression: TIDExpression;
    function Get_True: TIDBooleanConstant;
    function Get_TrueExpression: TIDExpression;
    function Get_ZeroConstant: TIDIntConstant;
    function Get_ZeroFloatExpression: TIDExpression;
    function Get_ZeroIntExpression: TIDExpression;
    function Get_TVarData: TIDType;
    function Get_DateTimeType: TIDType;
    function Get_DateType: TIDType;
    function Get_TimeType: TIDType;
  protected
    function GetSystemDeclarations: PDelphiSystemDeclarations; override;
  public
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    constructor Create(const Project: IASTProject; const FileName: string; const Source: string); override;
    function Compile(ACompileIntfOnly: Boolean; RunPostCompile: Boolean = True): TCompilerResult; override;
    function CompileIntfOnly: TCompilerResult; override;
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    property DataTypes[ID: TDataTypeID]: TIDType read GetTypeByID;
    property SystemDeclarations: PDelphiSystemDeclarations read GetSystemDeclarations;
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
    property _Float32: TIDType read Get_Float32;
    property _Float64: TIDType read Get_Float64;
    property _Float80: TIDType read Get_Float80;
    property _Currency: TIDType read Get_Currency;
    property _Comp: TIDType read Get_Comp;
    property _Boolean: TIDType read Get_Boolean;
    property _AnsiChar: TIDType read Get_AnsiChar;
    property _WideChar: TIDType read Get_WideChar;
    property _AnsiString: TIDType read Get_AnsiString;
    property _UnicodeString: TIDType read Get_UnicodeString;
    property _ShortString: TIDType read Get_ShortString;
    property _WideString: TIDType read Get_WideString;
    property _OpenString: TIDString read Get_OpenString;
    property _Variant: TIDType read Get_Variant;
    property _NilPointer: TIDType read Get_NullPtrType;
    property _TGuid: TIDStructure read Get_GuidType;
    property _True: TIDBooleanConstant read Get_True;
    property _False: TIDBooleanConstant read Get_False;
    property _TrueExpression: TIDExpression read Get_TrueExpression;
    property _FalseExpression: TIDExpression read Get_FalseExpression;
    property _ZeroConstant: TIDIntConstant read Get_ZeroConstant;
    property _ZeroIntExpression: TIDExpression read Get_ZeroIntExpression;
    property _ZeroFloatExpression: TIDExpression read Get_ZeroFloatExpression;
    property _OneConstant: TIDIntConstant read Get_OneConstant;
    property _OneExpression: TIDExpression read Get_OneExpression;
    property _NullPtrConstant: TIDIntConstant read Get_NullPtrConstant;
    property _NullPtrExpression: TIDExpression read Get_NullPtrExpression;
    property _EmptyStrExpression: TIDExpression read Get_EmptyStrExpression;
    property _Pointer: TIDPointer read Get_PointerType;
    property _UntypedReference: TIDUntypedRef read Get_UntypedReference;
    property _Untyped: TIDType read Get_Untyped;
    property _TObject: TIDClass read Get_TObject;
    property _Exception: TIDClass read Get_Exception;
    property _EAssert: TIDClass read Get_EAssertClass;
    property _TTypeKind: TIDEnum read Get_TTypeKind;
    property _TVarData: TIDType read Get_TVarData;
    property _DateTime: TIDType read Get_DateTimeType;
    property _Date: TIDType read Get_DateType;
    property _Time: TIDType read Get_TimeType;
    property _AssertProc: TIDProcedure read fAsserProc;
    property _TypeID: TIDType read fTypeIDType;
    property _DeprecatedDefaultStr: TIDStringConstant read Get_DeprecatedDefaultStr;
    property _OrdinalType: TIDType read Get_OrdinalType;
    property _PAnsiChar: TIDType read Get_PAnsiChar;
    property _PWideChar: TIDType read Get_PWideChar;
    property _AnyArrayType: TIDArray read fArrayType;
    property _MetaType: TIDType read Get_MetaType;
    property _Void: TIDType read Get_Void;
    property _ResStringRecord: TIDType read Get_ResStringRecord;
    property _EmptySetType: TIDSet read Get_EmptySetType;
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
    for i := dtInt8 to dtComp do
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

  _Variant.OverloadImplicitToAny(Operators.ImplicitVariantToAny);
  _Variant.OverloadImplicitFromAny(Operators.ImplicitVariantFromAny);
  _Variant.OverloadExplicitToAny(Operators.ExplicitVariantToAny);
  _Variant.OverloadExplicitFromAny(Operators.ExplicitVariantFromAny);


  // float32
  with _Float32 do begin
    OverloadImplicitTo(_Float32);
    OverloadImplicitTo(_Float64);
    OverloadImplicitTo(_Float80);
    OverloadImplicitTo(_Currency);
    OverloadImplicitTo(_Comp);
    OverloadImplicitTo(_Variant, Operators.ImplicitVariantFromAny);
  end;

  // Float64
  with _Float64 do begin
    OverloadImplicitTo(_Float32);
    OverloadImplicitTo(_Float64);
    OverloadImplicitTo(_Float80);
    OverloadImplicitTo(_Currency);
    OverloadImplicitTo(_Comp);
    OverloadImplicitTo(_Variant, Operators.ImplicitVariantFromAny);
  end;

  // Float80
  with _Float80 do begin
    OverloadImplicitTo(_Float32);
    OverloadImplicitTo(_Float64);
    OverloadImplicitTo(_Currency);
    OverloadImplicitTo(_Comp);
    OverloadImplicitTo(_Variant, Operators.ImplicitVariantFromAny);
  end;

  // Currency
  with _Currency do begin
    OverloadImplicitTo(_Float32);
    OverloadImplicitTo(_Float64);
    OverloadImplicitTo(_Float80);
    OverloadImplicitTo(_Comp);
    OverloadImplicitTo(_Variant, Operators.ImplicitVariantFromAny);
  end;

  // Comp
  with _Comp do begin
    OverloadImplicitTo(_Float32);
    OverloadImplicitTo(_Float64);
    OverloadImplicitTo(_Float80);
    OverloadImplicitTo(_Currency);
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
  _AnsiString.OverloadImplicitFromAny(Operators.ImplicitAnsiStringFromAny);

  // WideString
  _WideString.OverloadImplicitTo(_Variant, Operators.ImplicitVariantFromAny);
  _WideString.OverloadImplicitTo(_PWideChar, Operators.ImplicitStringToPChar);
  _WideString.OverloadImplicitTo(_PAnsiChar, Operators.ImplicitStringToPChar);
  _WideString.OverloadImplicitTo(_AnsiString);
  _WideString.OverloadImplicitTo(_ShortString);
  _WideString.OverloadImplicitTo(_UnicodeString);
  _WideString.OverloadImplicitFromAny(Operators.ImplicitStringFromAny);
  _WideString.OverloadExplicitFromAny(Operators.ExplicitStringFromAny);

  // WideChar
  _WideChar.OverloadImplicitTo(_WideChar);
  _WideChar.OverloadImplicitTo(_PWideChar);
  _WideChar.OverloadImplicitTo(_UnicodeString, Operators.ImplicitCharToString);
  _WideChar.OverloadImplicitTo(_AnsiString, Operators.ImplicitCharToAnsiString);
  _WideChar.OverloadImplicitTo(_AnsiChar, Operators.ImplicitCharToAnsiChar);
  _WideChar.OverloadImplicitTo(_Variant, Operators.ImplicitVariantFromAny);
  _WideChar.OverloadImplicitTo(_WideString);

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
    for i := dtInt8 to dtComp do
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

  AddExplicits(_Float32, dtFloat32, dtComp);
  AddExplicits(_Float64, dtFloat32, dtComp);
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
  _AnsiString.OverloadExplicitTo(_WideString);
  _AnsiString.OverloadExplicitFromAny(Operators.ExplicitAnsiStringFromAny);

  // WideString
  _WideString.OverloadExplicitTo(_UnicodeString);
  _WideString.OverloadExplicitTo(_Pointer);
  _WideString.OverloadExplicitTo(_NativeInt);
  _WideString.OverloadExplicitTo(_NativeUInt);

  // AnsiChar
  _AnsiChar.OverloadExplicitTo(_WideChar);
  _AnsiChar.OverloadExplicitTo(_WideString);
  _AnsiChar.OverloadExplicitTo(_UnicodeString);

  // ShortString
  _ShortString.OverloadExplicitTo(_AnsiString);
  _ShortString.OverloadExplicitTo(_UnicodeString);

  // WideChar
  _WideChar.OverloadExplicitTo(_UnicodeString);
  _WideChar.OverloadExplicitToAny(Operators.ExplicitCharToAny);

  _PWideChar.OverloadExplicitTo(_UnicodeString);

  _UntypedReference.OverloadExplicitTo(_ShortString);

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

  AddBinarOperator(opModDiv, _NativeInt, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _NativeInt);
  AddBinarOperator(opModDiv, _NativeUInt, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64], _NativeUInt);
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
  AddUnarOperator(opNegative, _Float80, _Float80);
  AddUnarOperator(opNegative, _Currency, _Currency);
  AddUnarOperator(opNegative, _Comp, _Comp);
  AddUnarOperator(opNegative, _Variant, _Variant);
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
  AddBinarOperator(opSubtract, _Currency, [_Int8, _UInt8, _Int16, _UInt16, _Int32, _UInt32, _Int64, _UInt64, _Float32, _Float64, _Float80, _Currency], _Currency);

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
  AddBinarOperator(opAdd, _UnicodeString, _WideChar, _UnicodeString);
  AddBinarOperator(opAdd, _UnicodeString, _PWideChar, _UnicodeString);

  AddBinarOperator(opAdd, _PWideChar, _PWideChar, _UnicodeString);
  AddBinarOperator(opAdd, _PWideChar, _WideChar, _UnicodeString);
  AddBinarOperator(opAdd, _PWideChar, _UnicodeString, _UnicodeString);

  AddBinarOperator(opAdd, _WideChar, _WideChar, _UnicodeString);
  AddBinarOperator(opAdd, _WideChar, _PWideChar, _UnicodeString);
  AddBinarOperator(opAdd, _WideChar, _UnicodeString, _UnicodeString);

  AddBinarOperator(opAdd, _WideString, _WideString, _WideString);

  AddBinarOperator(opAdd, _AnsiString, _AnsiString, _AnsiString);
  AddBinarOperator(opAdd, _AnsiString, _AnsiChar, _AnsiString);
  AddBinarOperator(opAdd, _AnsiString, _PAnsiChar, _AnsiString);

  AddBinarOperator(opAdd, _AnsiChar, _AnsiChar, _AnsiString);
  AddBinarOperator(opAdd, _AnsiChar, _PAnsiChar, _AnsiString);
  AddBinarOperator(opAdd, _AnsiChar, _AnsiString, _AnsiString);

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
    for i := dtInt8 to dtNativeUInt do
      AddUnarOperator(Op, DataTypes[i], DataTypes[i]);
    AddUnarOperator(Op, _Variant, _Variant);
  end;
  procedure BitwiseOp(Op: TOperatorID);
  var
    i, j: TDataTypeID;
  begin
    for i := dtInt8 to dtNativeUInt do
       for j := dtInt8 to dtNativeUInt do
         AddBinarOperator(Op, DataTypes[i], DataTypes[j], GetMaxBitwiceOpType(DataTypes[i], DataTypes[j]));

    AddBinarOperator(Op, _Variant, _Variant, _Variant);
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
      AddBinarOperator(Op, DataTypes[i], _Float64, _Boolean);
      AddBinarOperator(Op, DataTypes[i], _Currency, _Boolean);
      AddBinarOperator(Op, DataTypes[i], _Comp, _Boolean);

      AddBinarOperator(Op, _Float32, DataTypes[i], _Boolean);
      AddBinarOperator(Op, _Float64, DataTypes[i], _Boolean);
      AddBinarOperator(Op, _Float80, DataTypes[i], _Boolean);
      AddBinarOperator(Op, _Currency, DataTypes[i], _Boolean);
      AddBinarOperator(Op, _Comp, DataTypes[i], _Boolean);
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

procedure TSYSTEMUnit.RegisterTypes;
begin
  //===============================================================
  fDecls._Int8 := RegisterOrdinal('ShortInt', dtInt8, MinInt8, MaxInt8);
  fDecls._Int16 := RegisterOrdinal('SmallInt', dtInt16, MinInt16, MaxInt16);
  fDecls._Int32 := RegisterOrdinal('Integer', dtInt32, MinInt32, MaxInt32);
  fDecls._Int64 := RegisterOrdinal('Int64', dtInt64, MinInt64, MaxInt64);
  fDecls._UInt8 := RegisterOrdinal('Byte', dtUInt8, 0, MaxUInt8);
  fDecls._UInt16 := RegisterOrdinal('Word', dtUInt16, 0, MaxUInt16);
  fDecls._UInt32 := RegisterOrdinal('Cardinal', dtUInt32, 0, MaxUInt32);
  fDecls._UInt64 := RegisterOrdinal('UInt64', dtUInt64, 0, MaxUInt64);
  fDecls._NativeInt := RegisterOrdinal('NativeInt', dtNativeInt,
                                       Package.Target.MinNativeInt,
                                       Package.Target.MaxNativeInt);
  fDecls._NativeUInt := RegisterOrdinal('NativeUInt', dtNativeUInt,
                                        Package.Target.MinNativeUInt,
                                        Package.Target.MaxNativeUInt);
  fDecls._Float32 := RegisterType('Single', TIDFloat, dtFloat32);
  fDecls._Float64 := RegisterType('Double', TIDFloat, dtFloat64);
  fDecls._Float80 := RegisterType('Extended', TBuiltin_Extended, dtFloat80);
  fDecls._Currency := RegisterType('Currency', TBuiltin_Currency, dtCurrency);
  fDecls._Comp := RegisterType('Comp', TBuiltin_Comp, dtComp);
  //===============================================================
  fDecls._Boolean := RegisterOrdinal('Boolean', dtBoolean, 0, 1);
  _Boolean.OverloadExplicitFromAny(Operators.IsOrdinal);

  fDecls._AnsiChar := RegisterOrdinal('AnsiChar', dtAnsiChar, 0, MaxUInt8);
  fDecls._WideChar := RegisterOrdinal('Char', dtChar, 0, MaxUInt16);
  //===============================================================
  fDecls._ShortString := RegisterType('ShortString', TBuiltin_ShortString, dtShortString);
  TIDString(_ShortString).ElementDataType := _AnsiChar;
  //===============================================================
  fDecls._AnsiString := RegisterType('AnsiString', TBuiltin_AnsiString, dtAnsiString);
  TIDString(_AnsiString).ElementDataType := _AnsiChar;
  //===============================================================
  fDecls._UnicodeString := RegisterType('String', TIDString, dtString);
  TIDString(_UnicodeString).ElementDataType := _WideChar;
  //===============================================================
  fDecls._Variant := RegisterType('Variant', TIDVariant, dtVariant);
  //===============================================================
  fDecls._WideString := RegisterType('WideString', TIDString, dtWideString);
  TIDString(_WideString).ElementDataType := _WideChar;
  //===============================================================
  fDecls._OpenString := RegisterType('OpenString', TBuiltin_OpenString, dtAnsiString) as TIDString;
  _OpenString.ElementDataType := _AnsiChar;
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
  fDecls._PointerType := RegisterPointer('Pointer', nil);
  //===============================================================

  // null ptr type (special type for "nil" constant)
  fDecls._NullPtrType := TIDNullPointerType.CreateAsSystem(IntfScope, 'null ptr');
//  fDecls._NullPtrType.AddBinarySysOperator(opEqual, fOperators.Equal_NullPtr);
//  fDecls._NullPtrType.AddBinarySysOperator(opNotEqual, fOperators.NotEqual_NullPtr);
  // null ptr type constant
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

  fDecls._Real := RegisterTypeAlias('Real', _Float64);
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
  // empty set type
  fDecls._EmptySetType := TIDSet.CreateAsSystem(IntfScope, '');
  // constant "[]"
  fDecls._EmptyArrayConstant := TIDDynArrayConstant.CreateAsAnonymous(IntfScope, _EmptySetType, []);

  // constant for deprecated
  fDecls._DeprecatedDefaultStr := TIDStringConstant.CreateAsSystem(IntfScope, 'The declaration is deprecated', _AnsiString);


  // setup buit-in type operators
  TBuiltin_FltType(_Currency).SetupOperators(SystemDeclarations);
  TBuiltin_FltType(_Comp).SetupOperators(SystemDeclarations);
  TBuiltin_StrType(_OpenString).SetupOperators(SystemDeclarations);


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
  fDecls._TVarData := GetPublicType('TVarData');
  fDecls._TTypeKind := GetPublicType('TTypeKind') as TIDEnum;
  fDecls._DateTimeType := GetPublicType('TDateTime');
  fDecls._DateType := GetPublicType('TDate');
  fDecls._TimeType := GetPublicType('TTime');

  if Assigned(fDecls._TVarRec) then
    AddTVarRecImplicitOperators;
end;

procedure TSYSTEMUnit.SystemFixup;
begin
  if Package.Target = TWINX86_Target then
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
  RegisterBuiltin(TSCTF_IsManagedType);
  RegisterBuiltin(TSCTF_IsConstValue);
  RegisterBuiltin(TSCTF_TypeInfo);
  RegisterBuiltin(TSCTF_TypeName);
  RegisterBuiltin(TSCTF_GetTypeKind);
  RegisterBuiltin(TSCTF_HasWeakRef);
  RegisterBuiltin(TSCTF_Declared);
  RegisterBuiltin(TCT_Break);
  RegisterBuiltin(TCT_Continue);
  RegisterBuiltin(TSF_Delete);
  RegisterBuiltin(TSF_Exit);
  RegisterBuiltin(TSF_Exclude);
  RegisterBuiltin(TSF_FreeMem);
  RegisterBuiltin(TSF_FillChar);
  RegisterBuiltin(TSF_GetMem);
  RegisterBuiltin(TSF_GetDir);
  RegisterBuiltin(TSF_Halt);
  RegisterBuiltin(TSF_HiBound);
  RegisterBuiltin(TSF_HiByte);
  RegisterBuiltin(TSF_LoByte);
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
  RegisterBuiltin(TSF_VarCast);
  RegisterBuiltin(TSF_VarClear);


  if Project.Target = TWINX64_Target then
  begin
    RegisterBuiltin(TSF_AtomicCmpExchange128);
    RegisterBuiltin(TSF_MulDivInt64);
    RegisterBuiltin(TSF_VarArgStart);
    RegisterBuiltin(TSF_VarArgGetValue);
    RegisterBuiltin(TSF_VarArgCopy);
    RegisterBuiltin(TSF_VarArgEnd);
  end;

  RegisterVariable(ImplScope, 'ReturnAddress', _Pointer);
  RegisterConstStr(ImplScope, 'libmmodulename', '');
  RegisterConstInt('CompilerVersion', _Int32, 35); // Delphi 11.x

  // jsut for debug purpose
  RegisterBuiltin(TSCTF_Console);
  RegisterBuiltin(TSCTF_Scope);
end;

function TSYSTEMUnit.RegisterOrdinal(const TypeName: string; DataType: TDataTypeID; LowBound: Int64; HighBound: UInt64): TIDType;
begin
  Result := RegisterType(TypeName, TIDOrdinal, DataType);
  TIDOrdinal(Result).LowBound := LowBound;
  TIDOrdinal(Result).HighBound := Int64(HighBound);
  TIDOrdinal(Result).SignedBound  := LowBound < 0;
end;

function TSYSTEMUnit.Compile(ACompileIntfOnly: Boolean; RunPostCompile: Boolean = True): TCompilerResult;
begin
  Result := CompileInProgress;
  try
    RegisterBuiltinFunctions;
    SystemFixup;
    Result := inherited Compile(ACompileIntfOnly, {RunPostCompile:} False);
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

  fDecls := TDelphiBuiltinTypes.Create;

  fSysDecls := fDecls;

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
  Result := fDecls.GetTypeByID(ID);
end;

function TSYSTEMUnit.Get_AnsiChar: TIDType;
begin
  Result := fDecls._AnsiChar;
end;

function TSYSTEMUnit.Get_AnsiString: TIDType;
begin
  Result := fDecls._AnsiString;
end;

function TSYSTEMUnit.Get_Boolean: TIDType;
begin
  Result := fDecls._Boolean;
end;

function TSYSTEMUnit.Get_Comp: TIDType;
begin
  Result := fDecls._Comp;
end;

function TSYSTEMUnit.Get_Currency: TIDType;
begin
  Result := fDecls._Currency;
end;

function TSYSTEMUnit.Get_DateTimeType: TIDType;
begin
  Result := fDecls._DateTimeType;
end;

function TSYSTEMUnit.Get_DateType: TIDType;
begin
  Result := fDecls._DateType;
end;

function TSYSTEMUnit.Get_TimeType: TIDType;
begin
  Result := fDecls._TimeType;
end;

function TSYSTEMUnit.Get_DeprecatedDefaultStr: TIDStringConstant;
begin
  Result := fDecls._DeprecatedDefaultStr;
end;

function TSYSTEMUnit.Get_EAssertClass: TIDClass;
begin
  Result := fDecls._EAssertClass;
end;

function TSYSTEMUnit.Get_EmptySetType: TIDSet;
begin
  Result := fDecls._EmptySetType;
end;

function TSYSTEMUnit.Get_EmptyStrExpression: TIDExpression;
begin
  Result := fDecls._EmptyStrExpression;
end;

function TSYSTEMUnit.Get_Exception: TIDClass;
begin
  Result := fDecls._Exception;
end;

function TSYSTEMUnit.Get_False: TIDBooleanConstant;
begin
  Result := fDecls._False;
end;

function TSYSTEMUnit.Get_FalseExpression: TIDExpression;
begin
  Result := fDecls._FalseExpression;
end;

function TSYSTEMUnit.Get_Float32: TIDType;
begin
  Result := fDecls._Float32;
end;

function TSYSTEMUnit.Get_Float64: TIDType;
begin
  Result := fDecls._Float64;
end;

function TSYSTEMUnit.Get_Float80: TIDType;
begin
  Result := fDecls._Float80;
end;

function TSYSTEMUnit.Get_GuidType: TIDStructure;
begin
  Result := fDecls._GuidType;
end;

function TSYSTEMUnit.Get_Int16: TIDType;
begin
  Result := fDecls._Int16;
end;

function TSYSTEMUnit.Get_Int32: TIDType;
begin
  Result := fDecls._Int32;
end;

function TSYSTEMUnit.Get_Int64: TIDType;
begin
  Result := fDecls._Int64;
end;

function TSYSTEMUnit.Get_Int8: TIDType;
begin
  Result := fDecls._Int8;
end;

function TSYSTEMUnit.Get_MetaType: TIDType;
begin
  Result := fDecls._MetaType;
end;

function TSYSTEMUnit.Get_NativeInt: TIDType;
begin
  Result := fDecls._NativeInt;
end;

function TSYSTEMUnit.Get_NativeUInt: TIDType;
begin
  Result := fDecls._NativeUInt;
end;

function TSYSTEMUnit.Get_NullPtrConstant: TIDIntConstant;
begin
  Result := fDecls._NullPtrConstant;
end;

function TSYSTEMUnit.Get_NullPtrExpression: TIDExpression;
begin
  Result := fDecls._NullPtrExpression;
end;

function TSYSTEMUnit.Get_NullPtrType: TIDType;
begin
  Result := fDecls._NullPtrType;
end;

function TSYSTEMUnit.Get_OneConstant: TIDIntConstant;
begin
  Result := fDecls._OneConstant;
end;

function TSYSTEMUnit.Get_OneExpression: TIDExpression;
begin
  Result := fDecls._OneExpression;
end;

function TSYSTEMUnit.Get_OpenString: TIDString;
begin
  Result := fDecls._OpenString;
end;

function TSYSTEMUnit.Get_OrdinalType: TIDType;
begin
  Result := fDecls._OrdinalType;
end;

function TSYSTEMUnit.Get_PAnsiChar: TIDType;
begin
  Result := fDecls._PAnsiChar;
end;

function TSYSTEMUnit.Get_PointerType: TIDPointer;
begin
  Result := fDecls._PointerType;
end;

function TSYSTEMUnit.Get_PWideChar: TIDType;
begin
  Result := fDecls._PWideChar;
end;

function TSYSTEMUnit.Get_ResStringRecord: TIDType;
begin
  Result := fDecls._ResStringRecord;
end;

function TSYSTEMUnit.Get_ShortString: TIDType;
begin
  Result := fDecls._ShortString;
end;

function TSYSTEMUnit.Get_TObject: TIDClass;
begin
  Result := fDecls._TObject;
end;

function TSYSTEMUnit.Get_True: TIDBooleanConstant;
begin
  Result := fDecls._True;
end;

function TSYSTEMUnit.Get_TrueExpression: TIDExpression;
begin
  Result := fDecls._TrueExpression;
end;

function TSYSTEMUnit.Get_TTypeKind: TIDEnum;
begin
  Result := fDecls._TTypeKind;
end;

function TSYSTEMUnit.Get_TVarData: TIDType;
begin
  Result := fDecls._TVarData;
end;

function TSYSTEMUnit.Get_UInt16: TIDType;
begin
  Result := fDecls._UInt16;
end;

function TSYSTEMUnit.Get_UInt32: TIDType;
begin
  Result := fDecls._UInt32;
end;

function TSYSTEMUnit.Get_UInt64: TIDType;
begin
  Result := fDecls._UInt64;
end;

function TSYSTEMUnit.Get_UInt8: TIDType;
begin
  Result := fDecls._UInt8;
end;

function TSYSTEMUnit.Get_UnicodeString: TIDType;
begin
  Result := fDecls._UnicodeString;
end;

function TSYSTEMUnit.Get_Untyped: TIDType;
begin
  Result := fDecls._Untyped;
end;

function TSYSTEMUnit.Get_UntypedReference: TIDUntypedRef;
begin
  Result := fDecls._UntypedReference;
end;

function TSYSTEMUnit.Get_Variant: TIDType;
begin
  Result := fDecls._Variant;
end;

function TSYSTEMUnit.Get_Void: TIDType;
begin
  Result := fDecls._Void;
end;

function TSYSTEMUnit.Get_WideChar: TIDType;
begin
  Result := fDecls._WideChar;
end;

function TSYSTEMUnit.Get_WideString: TIDType;
begin
  Result := fDecls._WideString;
end;

function TSYSTEMUnit.Get_ZeroConstant: TIDIntConstant;
begin
  Result := fDecls._ZeroConstant;
end;

function TSYSTEMUnit.Get_ZeroFloatExpression: TIDExpression;
begin
  Result := fDecls._ZeroFloatExpression;
end;

function TSYSTEMUnit.Get_ZeroIntExpression: TIDExpression;
begin
  Result := fDecls._ZeroIntExpression;
end;

function TSYSTEMUnit.GetSystemDeclarations: PDelphiSystemDeclarations;
begin
  Result := fDecls;
end;

procedure TSYSTEMUnit.InsertToScope(Declaration: TIDDeclaration);
begin
  if Assigned(IntfScope.InsertNode(Declaration.Name, Declaration)) then
    raise Exception.CreateFmt('Unit SYSTEM: ' + sIdentifierRedeclaredFmt, [Declaration.Name]);
end;

function TSYSTEMUnit.CompileIntfOnly: TCompilerResult;
begin
  Result := Compile({ACompileIntfOnly:} True);
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
  Result := TIDIntConstant.CreateAsSystem(IntfScope, Name, DataType);
  Result.Value := Value;
  InsertToScope(Result);
end;

function TSYSTEMUnit.RegisterConstStr(Scope: TScope; const Name, Value: string): TIDStringConstant;
begin
  Result := TIDStringConstant.CreateAsSystem(Scope, Name, _UnicodeString);
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
  ImplicitRangeToAny := TSysImplicitRangeToAny.CreateAsSystem(Scope);
  ImplicitStringToAnsiString := TSysImplicitStringToAnsiString.CreateAsSystem(Scope);
  ImplicitStringToGUID := TSysImplicitStringToGUID.CreateAsSystem(Scope);
  ImplicitStringToPChar := TSysImplicitStringToPChar.CreateAsSystem(Scope);
  ImplicitAnsiStringToString := TSysImplicitAnsiStringToString.CreateAsSystem(Scope);
  ImplicitPointerToAny := TSysImplicitPointerToAny.CreateAsSystem(Scope);
  ImplicitPointerFromAny := TSysImplicitPointerFromAny.CreateAsSystem(Scope);
  ImplicitUntypedFromAny := TSysImplicitUntypedFromAny.CreateAsSystem(Scope);
  ImplicitClosureToTMethod := TSysImplicitClosureToTMethod.CreateAsSystem(Scope);
  ImplicitCharToAnsiString := TSysImplicitCharToAnsiString.CreateAsSystem(Scope);
  ImplicitAnsiStringFromAny := TSysImplicitAnsiStringFromAny.CreateAsSystem(Scope);
  ImplicitAnsiCharToAnsiString := TSysImplicitAnsiCharToAnsiString.CreateAsSystem(Scope);
  ImplicitAnsiCharToString := TSysImplicitAnsiCharToString.CreateAsSystem(Scope);
  ImplicitAnsiCharToWideChar := TSysImplicitAnsiCharToWideChar.CreateAsSystem(Scope);
  ImplicitMetaClassToGUID := TSysImplicitMetaClassToGUID.CreateAsSystem(Scope);
  ImplicitClassToClass := TSysImplicitClassToClass.CreateAsSystem(Scope);
  ImplicitArrayToAny := TSysImplicitArrayToAny.CreateAsSystem(Scope);
  ImplicitArrayFromAny := TSysImplicitArrayFromAny.CreateAsSystem(Scope);
  ImplicitSetFromAny := TSysImplicitSetFromAny.CreateAsSystem(Scope);
  ImplicitNullPtrToAny := TSysImplicitNullPtrToAny.CreateAsSystem(Scope);
  ImplicitTVarRecToAny := TSysImplicitTVarRecToAny.CreateAsSystem(Scope);
  ImplicitTVarRecFromAny := TSysImplicitTVarRecFromAny.CreateAsSystem(Scope);
  ImplicitSysVarFromAny := TSysImplicitSysVarFromAny.CreateAsSystem(Scope);
  ImplicitTProcFromAny := TSysImplicitTProcFromAny.CreateAsSystem(Scope);
  // explicit
  ExplicitStringFromAny := TSysExplicitStringFromAny.CreateAsSystem(Scope);
  ExplicitAnsiStringFromAny := TSysExplicitAnsiStringFromAny.CreateAsSystem(Scope);
  ExplicitTProcFromAny := TSysExplicitTProcFromAny.CreateAsSystem(Scope);
  ExplicitClassToAny := TSysExplicitClassToAny.CreateAsSystem(Scope);
  ExplicitClassFromAny := TSysExplicitClassFromAny.CreateAsSystem(Scope);
  ExplicitClassOfToAny := TSysExplicitClassOfToAny.CreateAsSystem(Scope);
  ExplicitClassOfFromAny := TSysExplicitClassOfFromAny.CreateAsSystem(Scope);
  ExplicitInterfaceFromAny := TSysExplicitInterfaceFromAny.CreateAsSystem(Scope);
  ExplicitRefTypeToAny := TSysExplicitRefTypeToAny.CreateAsSystem(Scope);
  ExplicitRefTypeFromAny := TSysExplicitRefTypeFromAny.CreateAsSystem(Scope);
  ExplicitRecordFromAny := TSysExplicitRecordFromAny.CreateAsSystem(Scope);
  ExplicitEnumToAny := TSysExplicitEnumToAny.CreateAsSystem(Scope);
  ExplicitEnumFromAny := TSysExplicitEnumFromAny.CreateAsSystem(Scope);
  ExplicitUntypedToAny := TSysExplicitUntypedToAny.CreateAsSystem(Scope);
  ExplicitUntypedRefToAny := TSysExplicitUntypedRefToAny.CreateAsSystem(Scope);
  ExplicitCharToAny := TSysExplicitCharToAny.CreateAsSystem(Scope);
  ExplicitRangeFromAny := TSysExplicitRangeFromAny.CreateAsSystem(Scope);
  ExplicitRecordToAny := TSysExplicitRecordToAny.CreateAsSystem(Scope);
  ExplicitStaticArrayToAny := TSysExplicitStaticArrayToAny.CreateAsSystem(Scope);
  ExplicitDynArrayToAny := TSysExplicitDynArrayToAny.CreateAsSystem(Scope);
  ExplicitVariantToAny := TSysExplicitVariantToAny.CreateAsSystem(Scope);
  ExplicitVariantFromAny := TSysExplicitVariantFromAny.CreateAsSystem(Scope);
  // any cast
  IsOrdinal := TSysTypeCast_IsOrdinal.CreateAsSystem(Scope);

  // in
  Ordinal_In_Set := TSysOrdinalInSet.CreateAsSystem(Scope);
  // add
  StaticArray_Add := TSys_StaticArray_Add.CreateAsSystem(Scope);
  Ptr_IntDiv_Int := TSys_Ptr_IntDiv_Int.CreateAsSystem(Scope);

  Multiply_Set := TSys_Multiply_Set.CreateAsSystem(Scope);
  Add_Set := TSys_Add_Set.CreateAsSystem(Scope);
  Subtract_Set := TSys_Subtract_Set.CreateAsSystem(Scope);
  Equal_Set := TSys_Equal_Set.CreateAsSystem(Scope);
  NotEqual_Set := TSys_NotEqual_Set.CreateAsSystem(Scope);

  Equal_DynArray := TSys_Equal_DynArray.CreateAsSystem(Scope);
  Equal_NullPtr := TSys_Equal_NullPtr.CreateAsSystem(Scope);
  NotEqual_NullPtr := TSys_NotEqual_NullPtr.CreateAsSystem(Scope);
end;

initialization

finalization

end.
