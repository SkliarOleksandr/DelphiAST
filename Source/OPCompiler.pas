//====================================================================================================================//
//=========================================== THE OBJECT PASCAL COMPILER =============================================//
//====================================================================================================================//
unit OPCompiler;

interface

{$I compilers.inc}

uses SysUtils, Classes, StrUtils, Types, OPCompiler.Parser, NPCompiler.Classes, NPCompiler.DataTypes,
     iDStringParser, IOUtils, IL.Types, AVL, NPCompiler.Operators, Math, NPCompiler.Errors,
     NPCompiler.Utils, Generics.Collections,
     NPCompiler.Intf,
     NPCompiler.Contexts,
     NPCompiler.ExpressionContext,
     AST.Classes,
     IL.Instructions,
     NPCompiler.Options, AST.Project;  // system, dateutils, sysinit

type

  TNPUnit = class;
  TUnitSection = (usInterface, usImplementation);

  TIDPlatform = class

  end;
  
  TExprPool = TPool<TIDExpression>;

  {parse members context - контекст парсинга выражений вид a.b.c или a[1, 2, 3].b...}
  TPMContext = record
  private
    FCnt: Integer;          // кол-во элементов в выражении
    FItems: TIDExpressions; // элементы цепочки
    function GetLast: TIDExpression; inline;
  public
    ID: TIdentifier;        // текущий идентификатор
    ItemScope: TScope;      // текущий scope
    DataType: TIDType;      // результатирующий тип цепочки выражений
    property Items: TIDExpressions read FItems;
    property Count: Integer read FCnt;
    property Last: TIDExpression read GetLast;
    procedure Init; inline;
    procedure Add(const Expr: TIDExpression);
    procedure Clear;
  end;

  TLambaParseContext = record
    VarSpace: TVarSpace;
    Parameters: TScope;
    ResultType: TIDType;
    CapturedVars: TIDClosure.TCapturedVars;
  end;


  TUnits = TList<TObject>;
  TTypes = TList<TIDType>;

  TProcSpecification = (
    PS_FORWARD,
    PS_INLINE,
    PS_PURE,
    PS_NORETURN,
    PS_NOEXCEPT,
    PS_OVELOAD,
    PS_EXPORT,
    PS_IMPORT,
    PS_VIRTUAL,
    PS_OVERRIDE,
    PS_STATIC,
    PS_STDCALL,
    PS_FASTCALL,
    PS_CDECL,
    PS_REINTRODUCE
  );

  TBreakPoint = record
    Line: Integer;
  end;

  TBreakPoints = TList<TBreakPoint>;

  TIDDeclarationList = TList<TIDDeclaration>;

  //TIDTypeCastProc = reference to function (UN: TNPUnit; SContext: PSContext; Src: TIDExpression; Dst: TIDType): TIDExpression;

  {предок всех внутренних процедур перегрузки операторов компилятора}
//  TIDInternalOperator = class(TIDOperator)
//  public
//    constructor CreateAsIntOp; reintroduce;
//  end;
//
//  TIDInternalOpImplicit = class(TIDInternalOperator)
//  public
//    constructor CreateInternal(ResultType: TIDType); reintroduce;
//    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; virtual; abstract;
//    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; virtual; abstract;
//  end;

  TLoopCodeContext = record
  {  Proc: TIDProcedure;
    IL: TIL;
    ItemExpr: TIDExpression;
    LBStart: TILInstruction;
    LVarExpr: TIDExpression;
    Arr: TIDArray;
    ALen: TIDExpression;
    RET: TILInstruction;
    ArrParam: TIDVariable;
    constructor Create(Proc: TIDProcedure; ArrParam: TIDVariable);}
  end;


  TNPUnit = class(TASTModule)
  type
    TVarModifyPlace = (vmpAssignment, vmpPassArgument);
    TBENodesPool = TPool<TBoolExprNode>;
    TLoopPool = TPool<TLContext>;
    TIdentifiersPool = TPool<TIdentifier>;
    TCondIFValue = (condIFFalse, condIfTrue, condIFUnknown);
  strict private
    FBENodesPool: TBENodesPool;       // Пул нодов для формирования булевых выражений
    FLoopPool: TLoopPool;             // Пул контекстов циклов
  ////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////////////////
    procedure RPNPushExpression(var EContext: TEContext; Operand: TIDExpression); inline;
    function RPNPushOperator(var EContext: TEContext; OpID: TOperatorID): TEContext.TRPNStatus; inline;
    function RPNReadExpression(var EContext: TEContext; Index: Integer): TIDExpression; inline;
    function RPNPopExpression(var EContext: TEContext): TIDExpression; inline;
  private
    FID: Integer;                      // ID модуля в пакете
    FPackage: INPPackage;
    FParser: TDelphiLexer;
    FUnitName: TIdentifier;
    FIntfScope: TScope;                // interface scope
    FImplScope: TScope;                // implementation scope
    FIntfImportedUnits: TUnitList;
    FImplImportedUnits: TUnitList;
    FMessages: ICompilerMessages;
    FVarSpace: TVarSpace;
    FProcSpace: TProcSpace;
    FTypeSpace: TTypeSpace;
    FTMPVars: TItemsStack;
    FDefines: TDefines;
    fCondStack: TSimpleStack<Boolean>;

    FConsts: TConstSpace;              // список нетривиальных констант (массивы, структуры)
    FCompiled: Boolean;

    FSystemExplicitUse: Boolean;
    FUseARC: Boolean;
    FOptions: TCompilerOptions;
    FBreakPoints: TBreakPoints;
    FUseCheckBound: Boolean;
    fInitProcSConect: TSContext;
    fFinalProcSConect: TSContext;
    function GetMessagesText: string;
    //========================================================================================================
    // процедуры генерации ошибок компиляции
  public
    class procedure ERROR_ARG_VAR_REQUIRED(Expr: TIDExpression); static;
    class procedure ERROR_CONST_EXPRESSION_REQUIRED(Expr: TIDExpression); static;
    class procedure ERROR_INCOMPATIBLE_TYPES(const Src, Dst: TIDExpression); overload; static;
    class procedure ERROR_INCOMPATIBLE_TYPES(const Src: TIDExpression; Dst: TIDType); overload; static;
    class procedure ERROR_INVALID_TYPECAST(const Src: TIDExpression; Dst: TIDType); static;
    class procedure ERROR_VAR_EXPRESSION_REQUIRED(Expr: TIDExpression); static;
    class procedure ERROR_VAR_OR_PROC_EXPRESSION_REQUIRED(Expr: TIDExpression); static;
    class procedure ERROR_CANNOT_MODIFY_FOR_LOOP_VARIABLE(Expr: TIDExpression); static;
    class procedure ERROR_CANNOT_MODIFY_CONSTANT(Expr: TIDExpression); static;
    class procedure ERROR_BOOLEAN_EXPRESSION_REQUIRED(Expr: TIDExpression); static;
    class procedure ERROR_ARRAY_EXPRESSION_REQUIRED(Expr: TIDExpression); static;
    class procedure ERROR_ID_REDECLARATED(Decl: TIDDeclaration); overload; static;
    class procedure ERROR_ID_REDECLARATED(const ID: TIdentifier); overload; static;
    class procedure ERROR_UNDECLARED_ID(const ID: TIdentifier); overload; static;
    class procedure ERROR_UNDECLARED_ID(const ID: TIdentifier; const GenericParams: TIDTypeList); overload; static;
    class procedure ERROR_UNDECLARED_ID(const Name: string; const TextPosition: TTextPosition); overload; static;
    class procedure ERROR_NOT_ENOUGH_ACTUAL_PARAMS(CallExpr: TIDExpression); static;
    class procedure ERROR_TOO_MANY_ACTUAL_PARAMS(CallExpr: TIDExpression); static;
    class procedure ERROR_OVERLOADED_MUST_BE_MARKED(const ID: TIdentifier); static;
    class procedure ERROR_DECL_DIFF_WITH_PREV_DECL(const ID: TIdentifier); static;
    class procedure ERROR_STRUCT_TYPE_REQUIRED(const TextPosition: TTextPosition); static;
    class procedure ERROR_ARRAY_TYPE_REQUIRED(const ID: TIdentifier); static;
    class procedure ERROR_INTEGER_TYPE_REQUIRED(const Pos: TTextPosition); static;
    class procedure ERROR_ORDINAL_TYPE_REQUIRED(const Pos: TTextPosition); static;
    class procedure ERROR_OBJECT_CANNOT_BE_USED_ON_PURE_PROC(const Expr: TIDExpression); static;
    class procedure ERROR_PROC_OR_PROCVAR_REQUIRED(const ID: TIdentifier); static;
    class procedure ERROR_PROC_REQUIRED(const Position: TTextPosition); static;
    class procedure ERROR_PROC_OR_TYPE_REQUIRED(const ID: TIdentifier); static;
    class procedure ERROR_CANNOT_ACCESS_TO_WRITEONLY_PROPERTY(const Expr: TIDExpression); static;
    class procedure ERROR_CANNOT_MODIFY_READONLY_PROPERTY(const Expr: TIDExpression); static;
    class procedure ERROR_OVERLOAD(CallExpr: TIDExpression); static;
    class procedure ERROR_AMBIGUOUS_OVERLOAD_CALL(CallExpr: TIDExpression); static;
    class procedure ERROR_INCOMPLETE_PROC(Decl: TIDDeclaration); static;
    class procedure ERROR_NO_OVERLOAD_OPERATOR_FOR_TYPES(Op: TOperatorID; Left, Right: TIDExpression); overload; static;
    class procedure ERROR_NO_OVERLOAD_OPERATOR_FOR_TYPES(Op: TOperatorID; Right: TIDExpression); overload;
    class procedure ERROR_UNIT_NOT_FOUND(const ID: TIdentifier); static;
    class procedure ERROR_UNIT_RECURSIVELY_USES_ITSELF(const ID: TIdentifier); static;
    class procedure ERROR_SETTER_MUST_BE_SUCH(const Setter: TIDProcedure; const DeclString: string); static;
    class procedure ERROR_GETTER_MUST_BE_SUCH(const Getter: TIDProcedure; const DeclString: string); static;
    class procedure ERROR_DIVISION_BY_ZERO(Expr: TIDExpression); static;
    class procedure ERROR_CONST_VALUE_OVERFLOW(Expr: TIDExpression; DstDataType: TIDType); static;
    class procedure ERROR_CLASS_TYPE_REQUIRED(const TextPosition: TTextPosition); static;
    class procedure ERROR_TYPE_REQUIRED(const TextPosition: TTextPosition); static;
    class procedure ERROR_CLASS_OR_INTF_TYPE_REQUIRED(const TextPosition: TTextPosition); static;
    class procedure ERROR_CLASS_NOT_IMPLEMENT_INTF(const Src: TIDExpression; Dest: TIDType); static;
    class procedure ERROR_METHOD_NOT_DECLARED_IN_CLASS(const ID: TIdentifier; Struct: TIDStructure); static;
    class procedure ERROR_INPLACEVAR_ALLOWED_ONLY_FOR_OUT_PARAMS(Variable: TIDVariable);
    class procedure ERROR_CANNOT_ACCESS_PRIVATE_MEMBER(const ID: TIdentifier); static;
    class procedure ERROR_REF_PARAM_MUST_BE_IDENTICAL(Expr: TIDExpression);
    class procedure ERROR_UNKNOWN_OPTION(const ID: TIdentifier);
    class procedure ERROR_CANNOT_ASSIGN_TEMPORARRY_OBJECT(Expr: TIDExpression);
    class procedure ERROR_INTF_TYPE_REQUIRED(Expr: TIDExpression);
    class procedure ERROR_INTF_ALREADY_IMPLEMENTED(Expr: TIDExpression);
    class procedure ERROR_INTF_METHOD_NOT_IMPLEMENTED(ClassType: TIDClass; Proc: TIDProcedure);
    class procedure ERROR_RECORD_STATIC_CONSTRUCTOR_ALREADY_EXIST(const Proc: TIDProcedure);
    class procedure ERROR_RECORD_STATIC_DESTRUCTOR_ALREADY_EXIST(const Proc: TIDProcedure);
    class procedure ERROR_CTOR_DTOR_MUST_BE_DECLARED_IN_STRUCT(const Position: TTextPosition);
    class procedure ERROR_OPERATOR_MUST_BE_DECLARED_IN_STRUCT(const Position: TTextPosition);
    class procedure ERROR_CANNOT_ASSIGN_NULL_TO_NOTNULL(const Src: TIDExpression);
    class procedure ERROR_ORDINAL_OR_SET_REQUIRED(const Src: TIDExpression);
    class procedure ERROR_REFERENCE_TYPE_EXPECTED(const Expr: TIDExpression);
    class procedure ERROR_STRING_CONST_IS_NOT_ANSI(const Src: TIDExpression);
    procedure ERROR_NO_METHOD_IN_BASE_CLASS(Proc: TIDProcedure);
    procedure ERROR_DEFAULT_PROP_MUST_BE_ARRAY_PROP;
    procedure ERROR_DEFAULT_PROP_ALREADY_EXIST(Prop: TIDProperty);
    procedure ERROR_IMPORT_FUNCTION_CANNOT_BE_INLINE;
    procedure ERROR_INVALID_TYPE_DECLARATION;
    procedure ERROR_EXPECTED_TOKEN(Token: TTokenID; ActulToken: TTokenID = token_unknown);
    procedure ERROR_EXPECTED_KEYWORD_OR_ID;
    procedure ERROR_IDENTIFIER_EXPECTED(ActualToken: TTokenID); overload;
    procedure ERROR_IDENTIFIER_EXPECTED; overload;
    procedure ERROR_PARAM_NAME_ID_EXPECTED(ActualToken: TTokenID);
    procedure ERROR_SEMICOLON_EXPECTED;
    procedure ERROR_EMPTY_EXPRESSION;
    procedure ERROR_END_OF_FILE;
    procedure ERROR_EXPRESSION_EXPECTED;
    procedure ERROR_FEATURE_NOT_SUPPORTED;
    procedure ERROR_INTF_SECTION_MISSING;
    procedure ERROR_KEYWORD_EXPECTED;
    procedure ERROR_BEGIN_KEYWORD_EXPECTED;
    procedure ERROR_DUPLICATE_SPECIFICATION(Spec: TProcSpecification);
    procedure ERROR_UNSUPPORTED_OPERATOR(Op: TOperatorID);
    procedure ERROR_TRY_KEYWORD_MISSED;
    procedure ERROR_EXPORT_ALLOWS_ONLY_IN_INTF_SECTION;
    procedure ERROR_NEED_SPECIFY_NINDEXES(const Decl: TIDDeclaration);
    procedure ERROR_IDENTIFIER_HAS_NO_MEMBERS(const Decl: TIDDeclaration);
    procedure ERROR_INVALID_COND_DIRECTIVE;
    procedure ERROR_INCOMPLETE_STATEMENT;
    procedure ERROR_PROC_OR_PROP_OR_VAR_REQUIRED;
    procedure HINT_TEXT_AFTER_END;
    procedure ERROR_PARAM_TYPE_REQUIRED;
    procedure ERROR_INHERITED_ALLOWED_ONLY_IN_OVERRIDE_METHODS;
    procedure ERROR_UNCLOSED_OPEN_BLOCK;
    procedure ERROR_UNNECESSARY_CLOSED_BLOCK;
    procedure ERROR_UNNECESSARY_CLOSED_ROUND;
    procedure ERROR_INIT_SECTION_ALREADY_DEFINED;
    procedure ERROR_FINAL_SECTION_ALREADY_DEFINED;
    procedure ERROR_ORDINAL_CONST_OR_TYPE_REQURED;
    procedure ERROR_DESTRUCTOR_CANNOT_BE_CALL_DIRECTLY;
    procedure ERROR_VIRTUAL_ALLOWED_ONLY_IN_CLASSES;
    procedure ERROR_WEAKREF_CANNOT_BE_DECLARED_FOR_TYPE(TypeDecl: TIDType);
    procedure ERROR_TYPE_IS_NOT_AN_ANCESTOR_FOR_THIS_TYPE(TypeDecl, ChildType: TIDType);
    procedure ERROR_INVALID_HEX_CONSTANT;
    procedure ERROR_INVALID_BIN_CONSTANT;
    procedure ERROR_INTERNAL(const Message: string = 'GENERAL ERROR');
    procedure HINT_RESULT_EXPR_IS_NOT_USED(const Expr: TIDExpression);
    procedure HINT_TYPE_DELETE_UNUSED(Decl: TIDDeclaration);
    procedure HINT_PROC_DELETE_UNUSED(Decl: TIDDeclaration);
  protected
    FRCPathCount: UInt32;              // кол-во проходов increfcount/decrefcount для деклараций
    FInitProcExplicit: Boolean;        // определена ли явно секция init
    FFinalProcExplicit: Boolean;       // определена ли явно секция final
    FInitProc: TIDProcedure;
    FFinalProc: TIDProcedure;

    property BENodesPool: TBENodesPool read FBENodesPool;
    property InitProc: TIDProcedure read FInitProc;
    property FinalProc: TIDProcedure read FFinalProc;
    //========================================================================================================
    function ProcSpec_Inline(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
    function ProcSpec_Export(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
    function ProcSpec_Forward(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
    function ProcSpec_Import(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
    function ProcSpec_Overload(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
    //function ProcSpec_Pure(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
    //function ProcSpec_NoReturn(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
    function ProcSpec_Virtual(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
    function ProcSpec_Override(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
    function ProcSpec_Reintroduce(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
    function ProcSpec_Static(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
    function ProcSpec_FastCall(Scope: TScope; var CallConvention: TCallConvention): TTokenID;
    function ProcSpec_StdCall(Scope: TScope; var CallConvention: TCallConvention): TTokenID;
    function ProcSpec_CDecl(Scope: TScope; var CallConvention: TCallConvention): TTokenID;
    function ParseGenericMember(const PMContext: TPMContext; SContext: PSContext; StrictSearch: Boolean; out Decl: TIDDeclaration; out WithExpression: TIDExpression): TTokenID;
    //////////////////////////////////////////////////////////////////////////////////////////////////////////
    function GetSource: string;
//    procedure CheckAndCallRecordInit(const Proc: TIDProcedure; const Variable: TIDVariable);
//    procedure CheckAndCallRecordCopy(const Proc: TIDProcedure; const Variable: TIDVariable; InsetBefore: TILInstruction);
//    procedure CheckAndCallRecordFinal(const Proc: TIDProcedure; const Variable: TIDVariable; InsetBefore: TILInstruction);
//    procedure CheckAndCallArrayInit(const Proc: TIDProcedure; const Variable: TIDVariable);
//    procedure CheckAndCallArrayCopy(const Proc: TIDProcedure; const Variable: TIDVariable; InsetBefore: TILInstruction);
//    procedure CheckAndCallArrayFinal(const Proc: TIDProcedure; const Variable: TIDVariable; InsetBefore: TILInstruction);
    procedure CondCompPush(Condition: Boolean);
    procedure CondCompPop;
    procedure SetUnitName(const Name: string);
  public
    function CreateSysProc(const SysName: string): TIDProcedure;
    function CreateArraySysProc(const Arr: TIDArray; const Name: string; out ProcParam: TIDVariable): TIDProcedure;
    procedure MakeLoopBodyBegin(var Ctx: TLoopCodeContext);
    procedure MakeLoopBodyEnd(var Ctx: TLoopCodeContext);

    class procedure ERROR_VAR_IS_NOT_INITIALIZED(const Variable: TIDExpression);
    class function IsConstValueInRange(Value: TIDExpression; RangeExpr: TIDRangeConstant): Boolean; static;
    class function IsConstRangesIntersect(const Left, Right: TIDRangeConstant): Boolean; static;
    class function IsConstEqual(const Left, Right: TIDExpression): Boolean; static;
    function Process_operators(var EContext: TEContext; OpID: TOperatorID): TIDExpression;
    function Process_operator_Period(var EContext: TEContext): TIDExpression;
    function Process_operator_Addr(var EContext: TEContext): TIDExpression;
    function Process_operator_Deref(var EContext: TEContext): TIDExpression;
    function Process_operator_In(var EContext: TEContext; const Left, Right: TIDExpression): TIDExpression;
    function Process_operator_Is(var EContext: TEContext): TIDExpression;
    function Process_operator_As(var EContext: TEContext): TIDExpression;
    { общая функция генерации вызова процедуры }
    function Process_CALL(var EContext: TEContext): TIDExpression;
    function process_CALL_constructor(SContext: PSContext; CallExpression: TIDCallExpression; const CallArguments: TIDExpressions): TIDExpression;
    { быстрая функция генерации вызова явно указанной процедуры (без перебора перегруженных процедур) }
    function Process_CALL_direct(SContext: PSContext; PExpr: TIDCallExpression; CallArguments: TIDExpressions): TIDExpression;
    //======================================================================================================================================
    function Process_operator_neg(var EContext: TEContext): TIDExpression;
    function Process_operator_not(var EContext: TEContext): TIDExpression;
    { оброботка присвоения для цепочек (a.x := b.y )}
    function Process_operator_assign_complex(Src: TIDExpression; Dst: TIDMultiExpression; var EContext: TEContext): Boolean;
    { оброботка одиночного присвоения (a := b )}
    procedure Process_operator_Assign(var EContext: TEContext); virtual;
    { оброботка множественного присвоения (a, b, c := d )}
    procedure Process_operator_AssignMulti(var EContext: TEContext);
    procedure Process_operator_logical_AND_OR(var EContext: TEContext; Op: TOperatorID; Left, Right, Result: TIDExpression);
    procedure ProcessMoveSet(SContext: PSContext; Dest, Source: TIDExpression);
    //======================================================================================================================================
    function ProcessBuiltin_Assigned(SContext: PSContext; var EContext: TEContext): TIDExpression;
    function ProcessBuiltin_IncDec(var EContext: TEContext; MacroID: TBuiltInFunctionID): TIDExpression;
    function ProcessBuiltin_Length(var EContext: TEContext): TIDExpression;
    function ProcessBuiltin_SetLength(var EContext: TEContext): TIDExpression;
    function ProcessBuiltin_Copy(var EContext: TEContext): TIDExpression;
    function ProcessBuiltin_Move(var EContext: TEContext): TIDExpression;
    function ProcessBuiltin_MemSet(var EContext: TEContext): TIDExpression;
    function ProcessBuiltin_SizeOf(var EContext: TEContext): TIDExpression;
    function ProcessBuiltin_Assert(var EContext: TEContext; const ParamsTest: string; SourceRow: Integer): TIDExpression;
    function ProcessBuiltin_TypeName(var EContext: TEContext): TIDExpression;
    function ProcessBuiltin_New(var EContext: TEContext): TIDExpression;
    function ProcessBuiltin_Free(var EContext: TEContext): TIDExpression;
    function ProcessBuiltin_GetRef(var EContext: TEContext): TIDExpression;
    function ProcessBuiltin_TypeInfo(var EContext: TEContext): TIDExpression;
    function ProcessBuiltin_LoHiBound(var EContext: TEContext; HiBound: Boolean): TIDExpression;
    function ProcessBuiltin_Ord(var EContext: TEContext): TIDExpression;
    function ProcessBuiltin_Include(var EContext: TEContext): TIDExpression;
    function ProcessBuiltin_Exclude(var EContext: TEContext): TIDExpression;
    function ProcessBuiltin_RefCount(var EContext: TEContext): TIDExpression;
    function ProcessBuiltin_GetBit(var EContext: TEContext): TIDExpression;
    function ProcessBuiltin_SetBit(var EContext: TEContext): TIDExpression;
    //======================================================================================================================================
    class procedure ILWrite(SContext: PSContext; Instruction: TILInstruction); overload; static; inline;
    class procedure ILWrite(const EContext: TEContext; Instruction: TILInstruction); overload; static; inline;
    class procedure ILWrite(Proc: TIDProcedure; Instruction: TILInstruction); overload; static; inline;
    class procedure ILWriteFirst(SContext: PSContext; Instruction: TILInstruction); static; inline;
    class procedure ILDelete(SContext: PSContext; FromInstruction, ToInstruction: TILInstruction); static; inline;
    { процедура CheckUnusedExprResult проверяет есть ли неиспользуемый результат последнего выражения }
    procedure CheckUnusedExprResult(var EContext: TEContext);
    { Поиск и удаление неиспользуемых переменных/типов/процедур }
    procedure CheckUnusedVariables(VarSpace: PVarSpace);
    function CheckUnusedRecordType(Struct: TIDStructure): Boolean;
    function CheckAndDelUnusedTypes(var TypeSpace: TTypeSpace): Integer;
    function CheckAndDelUnusedProcs(var ProcSpace: TProcSpace): Integer;
    function CheckAndDelUnusedConsts(var ConstSpace: TConstSpace): Integer;
    procedure CheckAndDelGenericTypes(var TypeSpace: TTypeSpace);
    procedure CheckAndDelGenericProcs(var ProcSpace: TProcSpace);
    procedure CheckInitVariables(SContext: PSContext; ParentDecl: TIDMultiExpression; VarSpace: PVarSpace);

    // процедура оптимизирует (повторное использование) использование временных переменных
    procedure Opt_ReuseTMPVars(const Proc: TIDProcedure);
    procedure Opt_ReduceTMPVars(const Proc: TIDProcedure);
    procedure Opt_ReduceIncRefDecRef(const Proc: TIDProcedure);

    procedure WriteRecordFromTuple(SContext: PSContext; Source, Destination: TIDExpression);

    procedure CheckIntfSectionMissing(Scope: TScope); inline;
    procedure CheckImplicitTypes(Src, Dst: TIDType; Position: TTextPosition); inline;
    procedure CheckLeftOperand(const Status: TEContext.TRPNStatus); inline;
    procedure CheckEmptyExpression(Expression: TIDExpression); inline;
    procedure CheckArrayExpression(Expression: TIDExpression); inline;
    procedure CheckIncompletedProcs(ProcSpace: PProcSpace); virtual;
    procedure CheckIncompletedIntfProcs(ClassType: TIDClass);
    procedure StaticCheckBounds(ConstValue: TIDConstant; Decl: TIDDeclaration; DimNumber: Integer);
    procedure CheckIncompleteFwdTypes;
    procedure CheckEndOfFile(Token: TTokenID);
    procedure CheckProcedureType(DeclType: TIDType); inline;
    class procedure CheckDestructorSignature(const DProc: TIDProcedure); static;
    class procedure CheckStaticRecordConstructorSign(const CProc: TIDProcedure); static;
    class procedure CheckConstValueOverflow(Src: TIDExpression; DstDataType: TIDType); static;
    class procedure CheckStringExpression(Expression: TIDExpression); static; inline;
    class procedure CheckConstExpression(Expression: TIDExpression); static; inline;
    class procedure CheckIntExpression(Expression: TIDExpression); static; inline;
    class procedure CheckOrdinalExpression(Expression: TIDExpression); static; inline;
    class procedure CheckNumericExpression(Expression: TIDExpression); static; inline;
    class procedure CheckBooleanExpression(Expression: TIDExpression); static; inline;
    class procedure CheckVarExpression(Expression: TIDExpression; VarModifyPlace: TVarModifyPlace); static;
    class procedure CheckPointerType(Expression: TIDExpression); static; inline;
    class procedure CheckReferenceType(Expression: TIDExpression); static; inline;
    class procedure CheckRecordType(Expression: TIDExpression); static; inline;
    class procedure CheckStructType(Expression: TIDExpression); static; inline;
    class procedure CheckClassType(Expression: TIDExpression); static; inline;
    class procedure CheckClassExpression(Expression: TIDExpression); static; inline;
    class procedure CheckSetType(Expression: TIDExpression); static; inline;
    class procedure CheckClassOrIntfType(Expression: TIDExpression); overload; static;
    class procedure CheckClassOrIntfType(DataType: TIDType; const TextPosition: TTextPosition); overload; static;
    class procedure CheckInterfaceType(Expression: TIDExpression); static; inline;
    class procedure CheckPureExpression(SContext: PSContext; Expr: TIDExpression); static;
    class procedure CheckIncompleteType(Fields: TScope); static;
    class procedure CheckAccessMember(SContext: PSContext; Decl: TIDDeclaration; const ID: TIdentifier);
    class procedure CheckNotNullExpression(const Dest: TIDVariable; const Source: TIDExpression); static;
    class procedure CheckIntConstInRange(const Expr: TIDExpression; HiBount, LowBound: Int64); static;
    procedure InitEContext(var EContext: TEContext; SContext: PSContext; EPosition: TExpessionPosition); inline;
    procedure AddType(const Decl: TIDType); inline;
    procedure AddConstant(const Decl: TIDConstant); inline;
  public
    procedure PutMessage(Message: TCompilerMessage); overload;
    procedure PutMessage(MessageType: TCompilerMessageType; const MessageText: string); overload;
    procedure PutMessage(MessageType: TCompilerMessageType; const MessageText: string; const SourcePosition: TTextPosition); overload;
    procedure Error(const Message: string; const Params: array of const; const TextPosition: TTextPosition);
    procedure Warning(const Message: string; const Params: array of const; const TextPosition: TTextPosition);
    procedure Hint(const Message: string; const Params: array of const; const TextPosition: TTextPosition); overload;
    procedure Hint(const Message: string; const Params: array of const); overload;
    //////////////////////////////////////////////////////////////////////////////////////////////////////
    ///  функции работы с парсером
    procedure parser_ReadToken(Scope: TScope; const Token: TTokenID); inline;
    procedure parser_ReadSemicolon(Scope: TScope); inline;
    procedure parser_MatchIdentifier(const ActualToken: TTokenID); inline;
    procedure parser_MatchToken(const ActualToken, ExpectedToken: TTokenID); inline;
    procedure parser_MatchNextToken(Scope: TScope; ExpectedToken: TTokenID); inline;
    procedure parser_MatchSemicolon(const ActualToken: TTokenID); inline;
    procedure parser_ReadCurrIdentifier(var Identifier: TIdentifier); inline;
    procedure parser_ReadTokenAsID(var Identifier: TIdentifier);
    procedure parser_ReadNextIdentifier(Scope: TScope; var Identifier: TIdentifier); inline;
    procedure parser_MatchParamNameIdentifier(ActualToken: TTokenID); inline;
    function parser_CurTokenID: TTokenID; inline;
    function parser_ReadSemicolonAndToken(Scope: TScope): TTokenID; inline;
    function parser_NextToken(Scope: TScope): TTokenID; inline;
    function parser_Position: TTextPosition; inline;
    function parser_PrevPosition: TTextPosition; inline;
    function parser_IdentifireType: TIdentifierType; inline;
    function parser_TokenLexem(const TokenID: TTokenID): string; inline;
    function parser_Line: Integer; inline;
    function parser_SkipBlock(StopToken: TTokenID): TTokenID;
    function parser_SkipTo(Scope: TScope; StopToken: TTokenID): TTokenID;
    //=======================================================================================================================
    /// функции для работы с булевыми выражениями
    procedure Bool_AddExprNode(var Context: TEContext; Instruction: TILInstruction; Condition: TILCondition); overload;
    procedure Bool_AddExprNode(var EContext: TEContext; NodeType: TBoolExprNode.TNodeType); overload;
    { завершает обработку булева выражения, расставляя переходы между подвыражениями}
    procedure Bool_CompleteExpression(Node: PBoolExprNode; ElsePosition: TILInstruction);
    { тоже самое только для выражений вида BoolVar := BoolExpr }
    function Bool_CompleteImmediateExpression(const EContext: TEContext; Destination: TIDExpression): Boolean;
    //=======================================================================================================================
    class procedure CFBBegin(SContext: PSContext; CFBlockType: TCFBlockType); static;
    class procedure CFBEnd(SContext: PSContext; CFBlockType: TCFBlockType); static;

    class procedure InsertToScope(Scope: TScope; Item: TIDDeclaration); overload; static; inline;
    class procedure InsertToScope(Scope: TScope; const ID: string; Declaration: TIDDeclaration); overload; static; inline;

    procedure ParseUnitDecl(Scope: TScope);
    {функция парсит название модуля}
    function ParseUnitName(Scope: TScope; out ID: TIdentifier): TTokenID;
    function ParseUsesSection(Scope: TScope): TTokenID;
    function ParseInitSection: TTokenID; virtual;
    function ParseFinalSection: TTokenID; virtual;
    // функция парсинга секции переменных/полей
    function ParseVarDefaultValue(Scope: TScope; DataType: TIDType; out DefaultValue: TIDExpression): TTokenID; virtual;
    function ParseVarStaticArrayDefaultValue(Scope: TScope; ArrType: TIDArray; out DefaultValue: TIDExpression): TTokenID; virtual;
    function ParseVarRecordDefaultValue(Scope: TScope; Struct: TIDStructure; out DefaultValue: TIDExpression): TTokenID;
    function ParseVarSection(Scope: TScope; Visibility: TVisibility; Struct: TIDStructure; IsWeak: Boolean = False; isRef: Boolean = False): TTokenID;
    function ParseVarInCaseRecord(Scope: TScope; Visibility: TVisibility; Struct: TIDStructure): TTokenID;
    function ParseInplaceVarDecl(Scope: TScope; out Expression: TIDExpression): TTokenID;
    //=======================================================================================================================
    ///  Парсинг типов
    function ParseUnionSection(Scope: TScope; Struct: TIDStructure): TTokenID;
    procedure ParseEnumType(Scope: TScope; Decl: TIDEnum);
    procedure ParseRangeType(Scope: TScope; Expr: TIDExpression; Decl: TIDRangeType);
    function ParseImportStatement(Scope: TScope; Decl: TIDDeclaration): TTokenID;
    function ParseStaticArrayType(Scope: TScope; Decl: TIDArray): TTokenID;
    function ParseSetType(Scope: TScope; Decl: TIDSet): TTokenID;
    function ParseProcType(Scope: TScope; const ID: TIdentifier; ProcType: TProcType; out Decl: TIDProcType): TTokenID;
    function ParseRecordType(Scope: TScope; Decl: TIDStructure): TTokenID;
    function ParseCaseRecord(Scope: TScope; Decl: TIDStructure): TTokenID;
    function ParseClassAncestorType(Scope, GenericScope: TScope; GDescriptor: PGenericDescriptor; ClassDecl: TIDClass): TTokenID;
    function ParseClassType(Scope, GenericScope: TScope; GDescriptor: PGenericDescriptor; const ID: TIdentifier; out Decl: TIDClass): TTokenID;
    {функция парсинга обьявления членов типа (не экземплярных)}
    function ParseTypeMember(Scope: TScope; Struct: TIDStructure): TTokenID;
    {тип - метакласс}
    function ParseClassOfType(Scope: TScope; const ID: TIdentifier; out Decl: TIDClassOf): TTokenID;
    function ParseInterfaceType(Scope, GenericScope: TScope; GDescriptor: PGenericDescriptor; const ID: TIdentifier; out Decl: TIDInterface): TTokenID;
    function ParseIntfGUID(Scope: TScope; Decl: TIDInterface): TTokenID;
    //=======================================================================================================================
    function ParseTypeOf(Scope: TScope; out Decl: TIDType): TTokenID;
    function ParseTypeRecord(Scope, GenericScope: TScope; GDescriptor: PGenericDescriptor; const ID: TIdentifier; out Decl: TIDType): TTokenID;
    function ParseTypeArray(Scope, GenericScope: TScope; GDescriptor: PGenericDescriptor; const ID: TIdentifier; out Decl: TIDType): TTokenID;
    function ParseTypeHelper(Scope, GenericScope: TScope; GDescriptor: PGenericDescriptor; const ID: TIdentifier; out Decl: TIDType): TTokenID;
    // функция парсинга анонимного типа
    function ParseTypeDecl(Scope, GenericScope: TScope; GDescriptor: PGenericDescriptor; const ID: TIdentifier; out Decl: TIDType): TTokenID;
    // функция парсинга именованного типа
    function ParseNamedTypeDecl(Scope: TScope): TTokenID;
    // функция парсинга указания типа (имени существующего или анонимного типа)
    function ParseTypeSpec(Scope: TScope; out DataType: TIDType): TTokenID;
    function ParseGenericTypeSpec(Scope: TScope; const ID: TIdentifier; out DataType: TIDType): TTokenID; virtual;
    // функция парсинга название процедуры/метода
    function ParseProcName(var Scope: TScope; out Name: TIdentifier; var Struct: TIDStructure; out ProcScope: TProcScope; out GenericParams: TIDTypeList): TTokenID;
    function ParseProcBody(Proc: TIDProcedure; Platform: TIDPlatform): TTokenID; virtual;
    function ParseProperty(Struct: TIDStructure): TTokenID; virtual;
    function ParseConstSection(Scope: TScope): TTokenID;
    function ParseParameters(Scope: TScope; InMacro: Boolean = False): TTokenID;
    function ParseAsmSpecifier(out Platform: TIDPlatform): TTokenID;
    function ParseAsmProc(Scope: TScope): TTokenID;
    function ParseTrySection(Scope: TScope; SContext: PSContext): TTokenID;
    //function ParseNameSpace(Scope: TScope): TTokenID;
    function ParseRaiseStatement(Scope: TScope; SContext: PSContext): TTokenID;
    //=======================================================================================================================
    /// условная компиляция
    function ParseCondStatements(Scope: TScope; Token: TTokenID): TTokenID;
    function ParseCondIfDef(Scope: TScope): Boolean;
    function ParseCondSkip(Scope: TScope): TTokenID;
    function ParseCondHint(Scope: TScope): TTokenID;
    function ParseCondWarn(Scope: TScope): TTokenID;
    function ParseCondError(Scope: TScope): TTokenID;
    function ParseCondMessage(Scope: TScope): TTokenID;
    function ParseCondIf(Scope: TScope; out ExpressionResult: TCondIFValue): TTokenID;
    function ParseDelphiCondIfDef(Scope: TScope): Boolean;
    function Defined(const Name: string): Boolean;
    function ParseCondOptSet(Scope: TScope): TTokenID;
    function ParseCondOptPush(Scope: TScope): TTokenID;
    function ParseCondOptPop(Scope: TScope): TTokenID;
    //function ParseCondMacro(Scope: TScope): TTokenID;
    //function ParseCondEmit(Scope: TScope; SContext: PSContext): TTokenID;
    function ParseMacroArg(Scope: TScope; const ExprID: TIdentifier; Arg: TIDMacroArgument; out Expression: TIDExpression; var EContext: TEContext): TTokenID;
    //function ParseCondTarget(Scope: TScope): TTokenID;
    procedure ParseCondDefine(Scope: TScope; add_define: Boolean);
    function ParseCondInclude(Scope: TScope): TTokenID; virtual;
    //=======================================================================================================================
    class procedure AddSelfParameter(Params: TScope; Struct: TIDStructure; ClassMethod: Boolean); static; inline;
    class function AddResultParameter(Params: TScope): TIDVariable; static; inline;
    // statemets
    function ParseStatements(Scope: TScope; SContext: PSContext; IsBlock: Boolean): TTokenID; virtual;
    function ParseExitStatement(Scope: TScope; SContext: PSContext): TTokenID;
    function ParseEntryCall(Scope: TScope; CallExpr: TIDCallExpression; var EContext: TEContext): TTokenID; overload;
    function ParseEntryCall(Scope: TScope; SContext: PSContext; out Args: TIDExpressions): TTokenID; overload;
    function ParseIndexedPropertyArgs(Scope: TScope; out ArgumentsCount: Integer; var EContext: TEContext): TTokenID;
    function ParseBuiltinCall(Scope: TScope; CallExpr: TIDExpression; var EContext: TEContext): TTokenID;
    function ParseUserDefinedMacroCall(Scope: TScope; CallExpr: TIDExpression; var EContext: TEContext): TTokenID;
    function ParseIfThenStatement(Scope: TScope; SContext: PSContext): TTokenID;
    function ParseUsingStatement(Scope: TScope; SContext: PSContext): TTokenID;
    function ParseWhileStatement(Scope: TScope; SContext: PSContext): TTokenID;
    function ParseRepeatStatement(Scope: TScope; SContext: PSContext): TTokenID;
    function ParseWithStatement(Scope: TScope; SContext: PSContext): TTokenID;
    function ParseCaseStatement(Scope: TScope; SContext: PSContext): TTokenID;
    function ParseForStatement(Scope: TScope; SContext: PSContext): TTokenID;
    function ParseForInStatement(Scope: TScope; SContext: PSContext; LoopVar: TIDExpression): TTokenID;
    function ParseBCStatements(Scope: TScope; Token: TTokenID; SContext: PSContext): TTokenID;
    function ParseExplicitCast(Scope: TScope; Scontext: PSContext; var DstExpression: TIDExpression): TTokenID;
    function ParseInheritedStatement(Scope: TScope; var EContext: TEContext): TTokenID;
    function ParseMultyComment(Scope: TScope): TTokenID;
    function ParseUnsafeStatement(Scope: TScope; Scontext: PSContext): TTokenID;
    function ParseImmVarStatement(Scope: TScope; Scontext: PSContext): TTokenID;
    function ParsePlatform(Scope: TScope): TTokenID;
    function ParseAttribute(Scope: TScope): TTokenID;
    function ParseDeprecated(Scope: TScope; out &Deprecated: TIDExpression): TTokenID;
    function CheckAndParseDeprecated(Scope: TScope; CurrToken: TTokenID): TTokenID;
    function CheckAndParseAttribute(Scope: TScope): TTokenID;
    function CheckAndParseProcTypeCallConv(Scope: TScope; TypeDecl: TIDType): TTokenID;
    function CheckAndCallFuncImplicit(SContext: PSContext; Source: TIDExpression): TIDExpression; overload;
    procedure CheckAndCallFuncImplicit(const EContext: TEContext); overload;
    //=======================================================================================================================
    /// парсинг выражений
    function ParseArrayMember(var PMContext: TPMContext; Scope: TScope; Decl: TIDDeclaration; out DataType: TIDType; var EContext: TEContext): TTokenID;
    function ParsePropertyMember(var PMContext: TPMContext; Scope: TScope; Prop: TIDProperty; out Expression: TIDExpression; var EContext: TEContext): TTokenID;
    function ParseMember(Scope: TScope; out Expression: TIDExpression; var EContext: TEContext): TTokenID;
    function ParseExpression(Scope: TScope; var EContext: TEContext; SrartToken: TTokenID): TTokenID;
    function ParseLambdaExpression(Scope: TScope; var EContext: TEContext): TTokenID;
    //function ParseBindFunctionExpression(Scope: TScope; var EContext: TEContext): TTokenID;
    function ParseLambdaExpressionBody(Scope: TScope; var EContext: TEContext; const LPContext: TLambaParseContext): TTokenID;
    function ParseClosureCapturedVarsDecl(Scope: TScope; out CapturedVars: TIDClosure.TCapturedVars): TTokenID;
    function ParseConstExpression(Scope: TScope; out Expr: TIDExpression; EPosition: TExpessionPosition): TTokenID; virtual;
    function ParseEmitExpression(Scope: TScope; out Expr: TIDExpression): TTokenID;
    function ProcessMemberExpression(SContext: PSContext; WasProperty: Boolean; var PMContext: TPMContext): TIDExpression;
    //=======================================================================================================================
    function ParseGenericsHeader(Params: TScope; out Args: TIDTypeList): TTokenID;
    function ParseGenericsArgs(Scope: TScope; SContext: PSContext; out Args: TIDExpressions): TTokenID;
    function ParseGenericProcRepeatedly(Scope: TScope; GenericProc, Proc: TIDProcedure; Struct: TIDStructure): TTokenID;
    function ParseProcedure(Scope: TScope; ProcType: TProcType; Struct: TIDStructure = nil): TTokenID; virtual;
    function ParseOperator(Scope: TScope; Struct: TIDStructure): TTokenID;
    function ParseAnonymousProc(Scope: TScope; var EContext: TEContext; SContext: PSContext; ProcType: TTokenID): TTokenID;
    function GetWeakRefType(Scope: TScope; SourceDataType: TIDType): TIDWeekRef;
    function CheckAndMakeClosure(SContext: PSContext; const ProcDecl: TIDProcedure): TIDClosure;
    procedure SetProcGenericArgs(CallExpr: TIDCallExpression; Args: TIDExpressions);
    procedure ParseASMStatement(Scope: TScope; Platform: TIDPlatform; SContext: PSContext);
    procedure ParseIIFStatement(Scope: TScope; var EContext: TEContext);
    { функция парсинга вектор(кортеж) вид [a, b, ... z]}
    procedure ParseVector(Scope: TScope; var EContext: TEContext);
    { функция пост-оброботки откомпилированного модуля}
    procedure PostCompileProcessUnit;
    procedure CheckManagedInitFinal(const Proc: TIDProcedure);
    procedure CheckManagedInitFinalUnit;
    procedure CheckManagedDstSrc(Proc: TIDProcedure; Instruction: TILDestInstruction);
    { функция пост-оброботки инициализации/финализации типа}
    procedure CheckAndMakeInitFinalStruct(const Struct: TIDStructure);
    procedure CheckAndMakeInitFinalArray(const Arr: TIDArray);
    procedure CheckProc(Proc: TIDProcedure);
    procedure ReleaseExpression(Expr: TIDExpression); overload;
    procedure ReleaseExpression(SContext: PSContext; Expr: TIDExpression); overload;
    procedure ReleaseExpression(const EContext: TEContext; Expr: TIDExpression); overload;
    procedure ReleaseExpressions(SContext: PSContext; Exprs: TIDExpressions);
    procedure EmitDynCheckBound(SContext: PSContext; ArrDecl: TIDDeclaration; Idx: TIDExpression);
    function EmitCreateClosure(SContext: PSContext; Closure: TIDClosure): TIDExpression;
    {функция ищет заданный класс в секции interface, бросает ошибку если не нашла}
    function GetPublicClass(const Name: string): TIDClass;
    function GetPublicType(const Name: string): TIDType;
    // специализация (инстанцирование) обобщенной процедуры
    function SpecializeGenericProc(CallExpr: TIDCallExpression; const CallArgs: TIDExpressions): TIDProcedure;
    // специализация (инстанцирование) обобщенного типа
    function SpecializeGenericType(GenericType: TIDType; const ID: TIdentifier; const SpecializeArgs: TIDExpressions): TIDType;

    class function CreateRangeType(Scope: TScope; LoBound, HiBound: Integer): TIDRangeType; static; inline;

    procedure MatchProc(CallExpr: TIDExpression; const ProcParams: TVariableList; var CallArgs: TIDExpressions);
    class function MatchOverloadProc(Item: TIDExpression; const CallArgs: TIDExpressions; CallArgsCount: Integer): TIDProcedure; static;
    function CheckConstDynArray(SContext: PSContext; Destination: TIDType; ConstArray: TIDExpression): TIDExpression;
    class function MatchImplicit(Source, Destination: TIDType): TIDDeclaration; static; inline;
    class function MatchImplicitClassOf(Source: TIDExpression; Destination: TIDClassOf): TIDDeclaration; static;
    class function StrictMatchProcSingnatures(const SrcParams, DstParams: TVariableList; const SrcResultType, DstResultType: TIDType): Boolean;
    class function StrictMatchProc(const Src, Dst: TIDProcedure): Boolean;
    class function CreateStructInitProc(const Struct: TIDStructure): TIDProcedure;
    class function CreateStructCopyProc(const Struct: TIDStructure): TIDProcedure;
    function CreateStructFinalProc(const Struct: TIDStructure): TIDProcedure;
    
    {функция возвращает специальную анонимную переменную - результат логических булевых выражений}
    {такая переменная не используется для генерации IL кода, а используется лишь для определения типа выражения}
    class function GetBoolResultExpr(ExistExpr: TIDExpression): TIDBoolResultExpression; overload; static; inline;
    function GetBoolResultExpr(SContext: PSContext): TIDBoolResultExpression; overload; inline;

    {функция CheckImplicit проверяет, возможно ли неявное преобразование}
    class function CheckImplicit(Source: TIDExpression; Dest: TIDType): TIDDeclaration; virtual;

    {функция MatchImplicit проверяет, возможно ли неявное преобразование и генерирует код преобразования(если нужен)}
    function MatchImplicitOrNil(SContext: PSContext; Source: TIDExpression; Dest: TIDType): TIDExpression;
    function MatchImplicit3(SContext: PSContext; Source: TIDExpression; Dest: TIDType): TIDExpression; overload;

    class procedure CheckAndMatchDerefExpression(SContext: PSContext; var Source: TIDExpression);

    class procedure MatchConstantExplicit(Source: TIDExpression; Dest: TIDType);

    { функция проверяет возможно ли нативное приведение или необходим вызов implicit оператора для каждого элемента }
    function MatchArrayImplicit(SContext: PSContext; Source: TIDExpression; DstArray: TIDArray): TIDExpression;

    { функция проверяет возможно ли приведение структуры к структуры (по полям)}
    function MatchRecordImplicit(SContext: PSContext; Source: TIDExpression; DstRecord: TIDRecord): TIDExpression;

    { функция проверяет возможно ли приведение кортежа к структуре }
    class function MatchArrayImplicitToRecord(Source: TIDExpression; Destination: TIDStructure): TIDExpression;

    { функция проверяет возможно ли приведение кортежа к битовому набору }
    class function MatchSetImplicit(Source: TIDExpression; Destination: TIDSet): TIDExpression; static;
    class function ConstDynArrayToSet(const CDynArray: TIDExpression; TargetSetType: TIDSet): TIDExpression; static;
    { функция проверяет возможно ли приведение кортежа к битовому набору }
    class function CheckSetImplicit(Source: TIDExpression; Destination: TIDSet): TIDDeclaration; static;

    class function MatchExplicit(const Source: TIDExpression; Destination: TIDType): TIDDeclaration; static; inline;
    class function MatchConstDynArrayImplicit(Source: TIDExpression; Destination: TIDType): TIDType; static;
    class function MatchDynArrayImplicit(Source: TIDExpression; Destination: TIDType): TIDType; static;
    class function MatchProcedureTypes(Src: TIDProcType; Dst: TIDProcType): TIDType; static;
    class procedure MatchPropSetter(Prop: TIDProperty; Setter: TIDExpression; PropParams: TScope);
    class procedure MatchPropGetter(Prop: TIDProperty; Getter: TIDProcedure; PropParams: TScope);
    class function MatchOperatorIn(const Left, Right: TIDExpression): TIDDeclaration; static;
    class function MatchUnarOperator(Op: TOperatorID; Right: TIDType): TIDType; static; inline;


    function FindImplicitFormBinarOperators(const Operators: TIDPairList; const Right: TIDType; out BetterFactor: Integer; out BetterOp: TIDDeclaration): TIDDeclaration;
    function MatchBinarOperator(SContext: PSContext; Op: TOperatorID; var Left, Right: TIDExpression): TIDDeclaration;
    function MatchBinarOperatorWithImplicit(SContext: PSContext; Op: TOperatorID; var Left, Right: TIDexpression): TIDDeclaration;
    function MatchBinarOperatorWithTuple(SContext: PSContext; Op: TOperatorID; var CArray: TIDExpression; const SecondArg: TIDExpression): TIDDeclaration;

    function CreateAnonymousConstant(Scope: TScope; var EContext: TEContext; const ID: TIdentifier;
                                     IdentifierType: TIdentifierType): TIDExpression;
    function CreateAnonymousConstTuple(Scope: TScope; ElementDataType: TIDType): TIDExpression;
    function FindID(Scope: TScope; const ID: TIdentifier): TIDDeclaration; overload; inline;
    function FindID(Scope: TScope; const ID: TIdentifier; out Expression: TIDExpression): TIDDeclaration; overload;
    function FindIDNoAbort(Scope: TScope; const ID: TIdentifier): TIDDeclaration; overload; inline;
    function FindIDNoAbort(Scope: TScope; const ID: string): TIDDeclaration; overload; inline;
    function FindIDNoAbort(Scope: TScope; const ID: TIdentifier; out Expression: TIDExpression): TIDDeclaration; overload; inline;
    property Parser: TDelphiLexer read FParser;
  strict private
    function GetTMPVar(SContext: PSContext; DataType: TIDType): TIDVariable; overload;
    function GetTMPVar(var EContext: TEContext; DataType: TIDType): TIDVariable; overload;
    function GetTMPVar(SContext: PSContext; DataType: TIDType; VarFlags: TVariableFlags): TIDVariable; overload;
    function GetTMPRef(SContext: PSContext; DataType: TIDType): TIDVariable;
    function GetTMPRefExpr(SContext: PSContext; DataType: TIDType): TIDExpression; overload; inline;
    function GetTMPVarExpr(SContext: PSContext; DataType: TIDType): TIDExpression; overload; inline;
    function GetTMPVarExpr(const EContext: TEContext; DataType: TIDType): TIDExpression; overload; inline;
    function GetTMPVarExpr(SContext: PSContext; DataType: TIDType; VarFlags: TVariableFlags): TIDExpression; overload; inline;
    function GetTMPVarExpr(SContext: PSContext; DataType: TIDType; const TextPos: TTextPosition): TIDExpression; overload; inline;
    function GetTMPVarExpr(var EContext: TEContext; DataType: TIDType; const TextPos: TTextPosition): TIDExpression; overload; inline;
  public
    ////////////////////////////////////////////////////////////////////////////
    constructor Create(const Project: IASTProject; const FileName: string; const Source: string = ''); override;
    destructor Destroy; override;
    ////////////////////////////////////////////////////////////////////////////
    procedure SaveConstsToStream(Stream: TStream); // сохраняет сложные константы модуля
    procedure SaveMethodBodies(Stream: TStream);   // сохраняет тела всех методов модуля
    procedure SaveDeclToStream(Stream: TStream);   // сохраняет все декларации модуля
    procedure SaveBodyToStream(Stream: TStream);   // сохраняет тела всех глобальных процедур модуля
    procedure SaveTypesToStream(Stream: TStream);  // сохраняет все типы модуля
    {возвращает список доступных в данном участке исходного кода обьявлений}
    procedure GetSRCDeclarations(const SrcPos: TTextPosition; Items: TIDDeclarationList);
    procedure GetINFDeclarations(Items: TIDDeclarationList);

    function Compile(RunPostCompile: Boolean = True): TCompilerResult; virtual;

    function CompileIntfOnly: TCompilerResult; virtual;

    function GetILText: string;
    function CheckUsed: Boolean;
    function UsedUnit(const UnitName: string): Boolean;
    function GetDefinesAsString: string;
    property _ID: TIdentifier read FUnitName;
    property UnitID: Integer read FID write FID;
    property Name: string read FUnitName.Name;
    property Messages: ICompilerMessages read FMessages;
    property MessagesText: string read GetMessagesText;
    property IntfSection: TScope read FIntfScope;
    property ImplSection: TScope read FImplScope;
    property IntfScope: TScope read FIntfScope;
    property ImplScope: TScope read FImplScope;
    property IntfImportedUnits: TUnitList read FIntfImportedUnits;
    property Package: INPPackage read FPackage;
    property Compiled: Boolean read FCompiled;
    property UseARC: Boolean read FUseARC write FUseARC; // temporary !!!
    property UseCheckBound: Boolean read FUseCheckBound write FUseCheckBound; // temporary !!!
    property Options: TCompilerOptions read FOptions;
    function CompileSource(Section: TUnitSection; const Source: string): ICompilerMessages;
    function CompileTypeDecl(Section: TUnitSection; const Source: string; out Decl: TIDType): ICompilerMessages;
    function CompileProcDecl(Section: TUnitSection; const Source: string; out Proc: TIDProcedure): ICompilerMessages;
    function CompileMethodDecl(Struct: TIDStructure; const Source: string; out Proc: TIDProcedure): ICompilerMessages;
    property Source: string read GetSource;
    property TypeSpace: TTypeSpace read FTypeSpace;
    property VarSpace: TVarSpace read FVarSpace;
    property ProcSpace: TProcSpace read FProcSpace;
  end;

  function GetUnit(const SContext: PSContext): TNPUnit; overload;
  function GetUnit(const EContext: TEContext): TNPUnit; overload;
  function ScopeToVarList(Scope: TScope; SkipFirstCount: Integer): TVariableList;

implementation

{ TCompiler }

uses SystemUnit, NPCompiler.Messages, TypInfo, NPCompiler.ConstCalculator;

function GetUnit(const SContext: PSContext): TNPUnit; overload;
begin
  Result := TNPUnit(SContext.Proc.DeclUnit);
end;

function GetUnit(const EContext: TEContext): TNPUnit; overload;
begin
  Result := TNPUnit(EContext.SContext.Proc.DeclUnit);
end;

procedure StopCompile(CompileSuccess: Boolean);
begin
  raise ECompilerStop.Create(CompileSuccess);
end;

function GetILLast(const Context: PSContext): TILInstruction;
begin
  if Assigned(Context) and Assigned(Context.IL) then
    Result := Context.IL.Last
  else
    Result := nil;
end;

procedure TNPUnit.ERROR_INCOMPLETE_STATEMENT;
begin
  AbortWork(sIncompleteStatement, FParser.Position);
end;

procedure TNPUnit.ERROR_INHERITED_ALLOWED_ONLY_IN_OVERRIDE_METHODS;
begin
  AbortWork('INHERITED calls allowed only in OVERRIDE methods', FParser.PrevPosition);
end;

procedure TNPUnit.ERROR_INIT_SECTION_ALREADY_DEFINED;
begin
  AbortWork('INITIALIZATION section is already defined', FParser.Position);
end;

class procedure TNPUnit.ERROR_INPLACEVAR_ALLOWED_ONLY_FOR_OUT_PARAMS(Variable: TIDVariable);
begin
  AbortWork('Inpalce VAR declaration allowed only for OUT parameters', Variable.TextPosition);
end;

class procedure TNPUnit.ERROR_INCOMPATIBLE_TYPES(const Src, Dst: TIDExpression);
var
  SrcName, DstName: string;
begin
  if Src.ItemType <> itType then SrcName := Src.DataTypeName else SrcName := Src.DisplayName;
  if Dst.ItemType <> itType then DstName := Dst.DataTypeName else DstName := Dst.DisplayName;
  AbortWork(sIncompatibleTypesFmt, [SrcName, DstName], Src.TextPosition);
end;

class procedure TNPUnit.ERROR_INCOMPATIBLE_TYPES(const Src: TIDExpression; Dst: TIDType);
begin
  AbortWork(sIncompatibleTypesFmt, [Src.DataTypeName, Dst.DisplayName], Src.TextPosition);
end;

class procedure TNPUnit.ERROR_CONST_EXPRESSION_REQUIRED(Expr: TIDExpression);
begin
  AbortWork(sExpressionMustBeConstant, Expr.TextPosition);
end;

class procedure TNPUnit.ERROR_CONST_VALUE_OVERFLOW(Expr: TIDExpression; DstDataType: TIDType);
begin
  AbortWork(errConstValueOverflow, [Expr.DisplayName, DstDataType.DisplayName], Expr.TextPosition);
end;

class procedure TNPUnit.ERROR_CTOR_DTOR_MUST_BE_DECLARED_IN_STRUCT(const Position: TTextPosition);
begin
  AbortWork('CONSTRUCTOR or DESTRUCTOR must be declared only in a CLASS or RECORD type', Position);
end;

class procedure TNPUnit.ERROR_OPERATOR_MUST_BE_DECLARED_IN_STRUCT(const Position: TTextPosition);
begin
  AbortWork('OPERATOR must be declared only in a CLASS or RECORD type', Position);
end;

class procedure TNPUnit.ERROR_VAR_EXPRESSION_REQUIRED(Expr: TIDExpression);
begin
  AbortWork(sVariableRequired, Expr.TextPosition);
end;

class procedure TNPUnit.ERROR_VAR_IS_NOT_INITIALIZED(const Variable: TIDExpression);
begin
  // пока еще не доделано
  // AbortWork('Variable "%s" is not initialized', [Variable.DisplayName], Variable.TextPosition);
end;

class procedure TNPUnit.ERROR_VAR_OR_PROC_EXPRESSION_REQUIRED(Expr: TIDExpression);
begin
  AbortWork(sVarOrProcRequired, Expr.TextPosition);
end;

procedure TNPUnit.ERROR_VIRTUAL_ALLOWED_ONLY_IN_CLASSES;
begin
  AbortWork('VIRTUAL allowed only for class methods', parser_PrevPosition);
end;

procedure TNPUnit.ERROR_WEAKREF_CANNOT_BE_DECLARED_FOR_TYPE(TypeDecl: TIDType);
begin
  AbortWork('The WEAK reference can not be declared for type: %s', [TypeDecl.DisplayName], parser_PrevPosition);
end;

class procedure TNPUnit.ERROR_ARG_VAR_REQUIRED(Expr: TIDExpression);
begin
  AbortWork(sConstCannotBePassedAsVarParam, Expr.TextPosition);
end;

class procedure TNPUnit.ERROR_CANNOT_MODIFY_CONSTANT(Expr: TIDExpression);
begin
  AbortWork(sCannotModifyObjectPassedAsConstParam, Expr.TextPosition);
end;

class procedure TNPUnit.ERROR_CANNOT_MODIFY_FOR_LOOP_VARIABLE(Expr: TIDExpression);
begin
  AbortWork(sCannotModifyForLoopIndexVarFmt, [Expr.DisplayName], Expr.TextPosition);
end;

procedure TNPUnit.ERROR_BEGIN_KEYWORD_EXPECTED;
begin
  AbortWork('BEGIN expected', parser_PrevPosition);
end;

class procedure TNPUnit.ERROR_BOOLEAN_EXPRESSION_REQUIRED(Expr: TIDExpression);
begin
  AbortWork(sExpressionMustBeBoolean, Expr.TextPosition);
end;

class procedure TNPUnit.ERROR_NOT_ENOUGH_ACTUAL_PARAMS(CallExpr: TIDExpression);
begin
  AbortWork(sNotEnoughActualParametersFmt, [CallExpr.DisplayName], CallExpr.TextPosition);
end;

class procedure TNPUnit.ERROR_OVERLOADED_MUST_BE_MARKED(const ID: TIdentifier);
begin
  AbortWork(sOverloadedMustBeMarked, [ID.Name], ID.TextPosition);
end;

class procedure TNPUnit.ERROR_DECL_DIFF_WITH_PREV_DECL(const ID: TIdentifier);
begin
  AbortWork(sDeclDifWithPrevDecl, [ID.Name], ID.TextPosition);
end;

procedure TNPUnit.ERROR_DEFAULT_PROP_ALREADY_EXIST(Prop: TIDProperty);
begin
  AbortWork(errorDefaultPropertyAlreadyExistsFmt, [Prop.Name], FParser.Position);
end;

procedure TNPUnit.ERROR_DEFAULT_PROP_MUST_BE_ARRAY_PROP;
begin
  AbortWork(errorDefaultPropertyMustBeAnArrayProperty, FParser.Position);
end;

procedure TNPUnit.ERROR_DESTRUCTOR_CANNOT_BE_CALL_DIRECTLY;
begin
  AbortWork('DESTRUCTOR cannot be call directly', FParser.PrevPosition);
end;

class procedure TNPUnit.ERROR_DIVISION_BY_ZERO(Expr: TIDExpression);
begin
  AbortWork(sDevisionByZero, Expr.TextPosition);
end;

class procedure TNPUnit.ERROR_TOO_MANY_ACTUAL_PARAMS(CallExpr: TIDExpression);
begin
  AbortWork(sTooManyActualParameters, CallExpr.TextPosition);
end;

procedure TNPUnit.ERROR_SEMICOLON_EXPECTED;
begin
  AbortWork(sSemicolonExpected, FParser.PrevPosition);
end;

procedure TNPUnit.ERROR_EXPECTED_KEYWORD_OR_ID;
begin
  AbortWork(sExpectedButFoundFmt, ['Identifier or keyword', FParser.TokenName], parser_Position);
end;

procedure TNPUnit.ERROR_EXPECTED_TOKEN(Token: TTokenID; ActulToken: TTokenID = token_unknown);
begin
  if ActulToken = token_unknown then
    AbortWork(sExpected, ['Token "' + UpperCase(FParser.TokenLexem(Token)) + '"'], FParser.PrevPosition)
  else
    AbortWork(sExpectedButFoundFmt, ['Token "' + UpperCase(FParser.TokenLexem(Token)) + '"',
                                                 UpperCase(FParser.TokenLexem(ActulToken))], FParser.PrevPosition);
end;

procedure TNPUnit.ERROR_IDENTIFIER_EXPECTED(ActualToken: TTokenID);
begin
  AbortWork(sIdExpectedButFoundFmt, [FParser.TokenLexem(ActualToken)], FParser.PrevPosition);
end;

procedure TNPUnit.ERROR_PARAM_NAME_ID_EXPECTED(ActualToken: TTokenID);
begin
  AbortWork(sIdExpectedButFoundFmt, [FParser.TokenLexem(ActualToken)], FParser.PrevPosition);
end;

procedure TNPUnit.ERROR_PARAM_TYPE_REQUIRED;
begin
  AbortWork(errParameterTypeRequred, FParser.PrevPosition);
end;

procedure TNPUnit.ERROR_IDENTIFIER_EXPECTED;
begin
  AbortWork(sIdentifierExpected, FParser.PrevPosition);
end;

procedure TNPUnit.ERROR_EMPTY_EXPRESSION;
begin
  AbortWork(sEmptyExpression, FParser.PrevPosition);
end;

procedure TNPUnit.ERROR_EXPRESSION_EXPECTED;
begin
  AbortWork(sExpressionExpected, FParser.PrevPosition);
end;

procedure TNPUnit.ERROR_END_OF_FILE;
begin
  AbortWork(sUnexpectedEndOfFile, FParser.PrevPosition);
end;

procedure TNPUnit.ERROR_FEATURE_NOT_SUPPORTED;
begin
  AbortWork(sFeatureNotSupported, FParser.Position);
end;

procedure TNPUnit.ERROR_FINAL_SECTION_ALREADY_DEFINED;
begin
  AbortWork('FINALIZATION section is already defined', FParser.Position);
end;

procedure TNPUnit.ERROR_INTERNAL(const Message: string);
begin
  raise ECompilerInternalError.Create('Internal error: ' + Message, parser_Position);
end;

class procedure TNPUnit.ERROR_INTF_ALREADY_IMPLEMENTED(Expr: TIDExpression);
begin
  AbortWork('INTERFACE "%s" is already implemented', [Expr.DisplayName], Expr.TextPosition);
end;

class procedure TNPUnit.ERROR_INTF_METHOD_NOT_IMPLEMENTED(ClassType: TIDClass; Proc: TIDProcedure);
begin
  AbortWork('Interface method "%s" is not implemented in class "%s"', [Proc.DisplayName, ClassType.DisplayName], ClassType.TextPosition);
end;

procedure TNPUnit.ERROR_INTF_SECTION_MISSING;
begin
  AbortWork(sIntfSectionMissing, FParser.Position);
end;

class procedure TNPUnit.ERROR_INTF_TYPE_REQUIRED(Expr: TIDExpression);
begin
  AbortWork('INTERFACE type required', Expr.TextPosition);
end;

procedure TNPUnit.ERROR_INVALID_BIN_CONSTANT;
begin
  AbortWork('Invalid BIN constant', parser_Position);
end;

procedure TNPUnit.ERROR_INVALID_COND_DIRECTIVE;
begin
  AbortWork(sInvalidConditionalStatement, parser_Position);
end;

procedure TNPUnit.ERROR_INVALID_HEX_CONSTANT;
begin
  AbortWork('Invalid HEX constant', parser_Position);
end;

class procedure TNPUnit.ERROR_INVALID_TYPECAST(const Src: TIDExpression; Dst: TIDType);
begin
  AbortWork(sInvalidTypecast, [Src.DataTypeName, Dst.DisplayName], Src.TextPosition);
end;

procedure TNPUnit.ERROR_INVALID_TYPE_DECLARATION;
begin
  AbortWork(errInvalidTypeDeclaration, FParser.PrevPosition);
end;

class procedure TNPUnit.ERROR_ID_REDECLARATED(Decl: TIDDeclaration);
begin
  AbortWork(sIdentifierRedeclared, [Decl.DisplayName], Decl.SourcePosition);
end;

class procedure TNPUnit.ERROR_ID_REDECLARATED(const ID: TIdentifier);
begin
  AbortWork(sIdentifierRedeclared, [ID.Name], ID.TextPosition);
end;

procedure TNPUnit.ERROR_IMPORT_FUNCTION_CANNOT_BE_INLINE;
begin
  AbortWork(sImportFuncCannotBeInline, parser_Position);
end;

procedure TNPUnit.ERROR_UNCLOSED_OPEN_BLOCK;
begin
  AbortWork(sUnclosedOpenBlock, FParser.PrevPosition);
end;

class procedure TNPUnit.ERROR_UNDECLARED_ID(const ID: TIdentifier);
begin
  AbortWork(sUndeclaredIdentifier, [ID.Name], ID.TextPosition);
end;

class procedure TNPUnit.ERROR_UNDECLARED_ID(const ID: TIdentifier; const GenericParams: TIDTypeList);
var
  i: integer;
  s: string;
begin
  if Assigned(GenericParams) then
  begin
    for i := 0 to Length(GenericParams) - 1 do
      s := AddStringSegment(s, GenericParams[i].Name, ', ');
    s := '<' + s + '>';
  end;
  AbortWork(sUndeclaredIdentifier, [ID.Name + s], ID.TextPosition);
end;

class procedure TNPUnit.ERROR_UNDECLARED_ID(const Name: string; const TextPosition: TTextPosition);
begin
  AbortWork(sUndeclaredIdentifier, [Name], TextPosition);
end;

procedure TNPUnit.ERROR_UNNECESSARY_CLOSED_BLOCK;
begin
  AbortWork(sUnnecessaryClosedBlock, parser_Position);
end;

procedure TNPUnit.ERROR_UNNECESSARY_CLOSED_ROUND;
begin
  AbortWork(sUnnecessaryClosedBracket, parser_Position);
end;

class procedure TNPUnit.ERROR_UNIT_NOT_FOUND(const ID: TIdentifier);
begin
  AbortWork(errUnitNotFoundFmt, [ID.Name], ID.TextPosition);
end;

class procedure TNPUnit.ERROR_UNIT_RECURSIVELY_USES_ITSELF(const ID: TIdentifier);
begin
  AbortWork(errUnitRecursivelyUsesItselfFmt, [ID.Name], ID.TextPosition);
end;

class procedure TNPUnit.ERROR_UNKNOWN_OPTION(const ID: TIdentifier);
begin
  AbortWork('Unknown option: %s', [ID.Name], ID.TextPosition);
end;

procedure TNPUnit.ERROR_KEYWORD_EXPECTED;
begin
  AbortWork(sKeywordExpected, [FParser.OriginalToken], FParser.Position);
end;

class procedure TNPUnit.ERROR_METHOD_NOT_DECLARED_IN_CLASS(const ID: TIdentifier; Struct: TIDStructure);
begin
  AbortWork('Method "%s" is not declared in "%s"', [ID.Name, Struct.Name], ID.TextPosition);
end;

procedure TNPUnit.ERROR_DUPLICATE_SPECIFICATION(Spec: TProcSpecification);
var
  SpecName: string;
begin
  case Spec of
    PS_FORWARD: SpecName := 'FORWARD';
    PS_INLINE: SpecName := 'INLINE';
    PS_PURE: SpecName := 'PURE';
    PS_NORETURN: SpecName := 'NORETURN';
    PS_NOEXCEPT: SpecName := 'NOEXCEPT';
    PS_OVELOAD: SpecName := 'OVERLOAD';
    PS_EXPORT: SpecName := 'EXPORT';
    PS_IMPORT: SpecName := 'EXTERNAL';
    PS_VIRTUAL: SpecName := 'VIRTUAL';
    PS_OVERRIDE: SpecName := 'OVERRIDE';
    PS_STATIC: SpecName := 'STATIC';
    PS_STDCALL: SpecName := 'STDCALL';
    PS_CDECL: SpecName := 'CDECL';
    PS_FASTCALL: SpecName := 'REGISTER';
    PS_REINTRODUCE: SpecName := 'REINTRODUCE';
  else
    SpecName := '<UNKNOWN>';
  end;
  AbortWork(sDuplicateSpecificationFmt, [SpecName], FParser.Position);
end;

procedure TNPUnit.ERROR_UNSUPPORTED_OPERATOR(Op: TOperatorID);
begin
  AbortWork('Unsupported operator "%s"', [OperatorFullName(Op)], FParser.Position);
end;

procedure TNPUnit.ERROR_TRY_KEYWORD_MISSED;
begin
  AbortWork(sTryKeywordMissed, FParser.Position);
end;

procedure TNPUnit.ERROR_TYPE_IS_NOT_AN_ANCESTOR_FOR_THIS_TYPE(TypeDecl, ChildType: TIDType);
begin
  AbortWork('The type "%s" is not an ancestor of type "%s"', [TypeDecl.DisplayName, ChildType.DisplayName], FParser.Position);
end;

class procedure TNPUnit.ERROR_TYPE_REQUIRED(const TextPosition: TTextPosition);
begin
  AbortWork('TYPE specification required', TextPosition);
end;

class procedure TNPUnit.ERROR_SETTER_MUST_BE_SUCH(const Setter: TIDProcedure; const DeclString: string);
begin
  AbortWork('Setter must have declaration: %s', [DeclString], Setter.ID.TextPosition);
end;

class procedure TNPUnit.ERROR_GETTER_MUST_BE_SUCH(const Getter: TIDProcedure; const DeclString: string);
begin
  AbortWork('Getter must have declaration: %s', [DeclString], Getter.ID.TextPosition);
end;

procedure TNPUnit.ERROR_EXPORT_ALLOWS_ONLY_IN_INTF_SECTION;
begin
  AbortWork(sExportAllowsOnlyInIntfSection, FParser.Position);
end;

class procedure TNPUnit.ERROR_STRING_CONST_IS_NOT_ANSI(const Src: TIDExpression);
begin
  AbortWork('string constant is not an ANSI string', Src.TextPosition);
end;

class procedure TNPUnit.ERROR_STRUCT_TYPE_REQUIRED(const TextPosition: TTextPosition);
begin
  AbortWork(sStructTypeRequired, TextPosition);
end;

class procedure TNPUnit.ERROR_ARRAY_EXPRESSION_REQUIRED(Expr: TIDExpression);
begin
  AbortWork('ARRAY expression required', Expr.TextPosition);
end;

class procedure TNPUnit.ERROR_ARRAY_TYPE_REQUIRED(const ID: TIdentifier);
begin
  AbortWork(sArrayTypeRequired, ID.TextPosition);
end;

procedure TNPUnit.ERROR_ORDINAL_CONST_OR_TYPE_REQURED;
begin
  AbortWork(sOrdinalConstOrTypeRequred, parser_PrevPosition);
end;

class procedure TNPUnit.ERROR_ORDINAL_OR_SET_REQUIRED(const Src: TIDExpression);
begin
  AbortWork('ORDINAL or SET required', Src.TextPosition);
end;

class procedure TNPUnit.ERROR_INTEGER_TYPE_REQUIRED(const Pos: TTextPosition);
begin
  AbortWork('INTEGER type required', Pos);
end;

class procedure TNPUnit.ERROR_ORDINAL_TYPE_REQUIRED(const Pos: TTextPosition);
begin
  AbortWork(sOrdinalTypeRequired, Pos);
end;

class procedure TNPUnit.ERROR_OBJECT_CANNOT_BE_USED_ON_PURE_PROC(const Expr: TIDExpression);
begin
  AbortWork(sObjectCannotBeUsedOnPureProc, [Expr.DisplayName], Expr.TextPosition);
end;

class procedure TNPUnit.ERROR_PROC_OR_PROCVAR_REQUIRED(const ID: TIdentifier);
begin
  AbortWork(sProcOrProcVarRequired, ID.TextPosition);
end;

procedure TNPUnit.ERROR_PROC_OR_PROP_OR_VAR_REQUIRED;
begin
  AbortWork('PROCEDURE or FUNCTION or PROPERTY or VAR required', [], FParser.PrevPosition);
end;

class procedure TNPUnit.ERROR_PROC_OR_TYPE_REQUIRED(const ID: TIdentifier);
begin
  AbortWork('PROCEDURE or TYPE required', ID.TextPosition);
end;

class procedure TNPUnit.ERROR_PROC_REQUIRED(const Position: TTextPosition);
begin
  AbortWork('PROCEDURE or FUNCTION required', Position);
end;

class procedure TNPUnit.ERROR_RECORD_STATIC_CONSTRUCTOR_ALREADY_EXIST(const Proc: TIDProcedure);
begin
  AbortWork('Record CONSTRUCTOR already exist', Proc.TextPosition);
end;

class procedure TNPUnit.ERROR_RECORD_STATIC_DESTRUCTOR_ALREADY_EXIST(const Proc: TIDProcedure);
begin
  AbortWork('Record DESTRUCTOR already exist', Proc.TextPosition);
end;

class procedure TNPUnit.ERROR_REFERENCE_TYPE_EXPECTED(const Expr: TIDExpression);
begin
  AbortWork('REFERENCE type required', Expr.TextPosition);
end;

class procedure TNPUnit.ERROR_REF_PARAM_MUST_BE_IDENTICAL(Expr: TIDExpression);
begin
  AbortWork('Types of actual and formal VAR parameters must be identical', Expr.TextPosition);
end;

class procedure TNPUnit.ERROR_CANNOT_MODIFY_READONLY_PROPERTY(const Expr: TIDExpression);
begin
  AbortWork(sCannotModifyReadOnlyProperty, Expr.TextPosition);
end;

class procedure TNPUnit.ERROR_CLASS_NOT_IMPLEMENT_INTF(const Src: TIDExpression; Dest: TIDType);
begin
  AbortWork('CLASS "%s" not implement the "%s" interface', [Src.DataTypeName, Dest.DisplayName], Src.TextPosition);
end;

class procedure TNPUnit.ERROR_CLASS_OR_INTF_TYPE_REQUIRED(const TextPosition: TTextPosition);
begin
  AbortWork('CLASS or INTERFACE type required', TextPosition);
end;

class procedure TNPUnit.ERROR_CLASS_TYPE_REQUIRED(const TextPosition: TTextPosition);
begin
  AbortWork(errCLASSTypeRequired, TextPosition);
end;

class procedure TNPUnit.ERROR_CANNOT_ACCESS_PRIVATE_MEMBER(const ID: TIdentifier);
begin
  AbortWork('Cannot access private member: %s', [ID.Name], ID.TextPosition);
end;

class procedure TNPUnit.ERROR_CANNOT_ACCESS_TO_WRITEONLY_PROPERTY(const Expr: TIDExpression);
begin
  AbortWork(sCannotAccessToWriteOnlyProperty, Expr.TextPosition);
end;

class procedure TNPUnit.ERROR_CANNOT_ASSIGN_NULL_TO_NOTNULL(const Src: TIDExpression);
begin
  AbortWork('Cannot assign NULLPTR to NOT NULL variable', [], Src.TextPosition);
end;

class procedure TNPUnit.ERROR_CANNOT_ASSIGN_TEMPORARRY_OBJECT(Expr: TIDExpression);
begin
  AbortWork('Cannot modify a temporary object', Expr.TextPosition);
end;

procedure TNPUnit.ERROR_NEED_SPECIFY_NINDEXES(const Decl: TIDDeclaration);
var
  ADataType: TIDArray;
begin
  ADataType := TIDArray(Decl.DataType);
  AbortWork(sNeedSpecifyNIndexesFmt, [Decl.DisplayName, ADataType.DisplayName, ADataType.DimensionsCount], parser_PrevPosition);
end;

procedure TNPUnit.ERROR_IDENTIFIER_HAS_NO_MEMBERS(const Decl: TIDDeclaration);
begin
  AbortWork(sIdentifierHasNoMembersFmt, [Decl.DisplayName], FParser.PrevPosition);
end;

class procedure TNPUnit.ERROR_OVERLOAD(CallExpr: TIDExpression);
begin
  AbortWork(sErrorOverload, CallExpr.TextPosition);
end;

function TNPUnit.EmitCreateClosure(SContext: PSContext; Closure: TIDClosure): TIDExpression;
begin
  Assert(False);
end;

procedure TNPUnit.EmitDynCheckBound(SContext: PSContext; ArrDecl: TIDDeclaration; Idx: TIDExpression);
begin
end;

procedure TNPUnit.Error(const Message: string; const Params: array of const; const TextPosition: TTextPosition);
begin
  PutMessage(cmtError, Format(Message, Params), TextPosition);
end;

class procedure TNPUnit.ERROR_AMBIGUOUS_OVERLOAD_CALL(CallExpr: TIDExpression);
begin
  AbortWork(sAmbiguousOverloadedCallFmt, [CallExpr.DisplayName], CallExpr.TextPosition);
end;

class procedure TNPUnit.ERROR_INCOMPLETE_PROC(Decl: TIDDeclaration);
var
  ProcName: string;
begin
  ProcName := Decl.DisplayName;
  if Assigned(TIDProcedure(Decl).Struct) then
    ProcName := TIDProcedure(Decl).Struct.Name + '.' + ProcName;
  AbortWork(sIncompleteProcFmt, [ProcName], Decl.SourcePosition);
end;

class procedure TNPUnit.ERROR_NO_OVERLOAD_OPERATOR_FOR_TYPES(Op: TOperatorID; Left, Right: TIDExpression);
begin
  AbortWork(sNoOverloadOperatorForTypesFmt, [OperatorFullName(Op), Left.DataTypeName, Right.DataTypeName], Left.TextPosition);
end;

class procedure TNPUnit.ERROR_NO_OVERLOAD_OPERATOR_FOR_TYPES(Op: TOperatorID; Right: TIDExpression);
begin
  AbortWork(sNoOverloadOperatorForTypeFmt, [OperatorFullName(Op), Right.DataTypeName], Right.TextPosition);
end;

procedure TNPUnit.ERROR_NO_METHOD_IN_BASE_CLASS(Proc: TIDProcedure);
begin
  AbortWork('Method %s is not found in base classes', [Proc.DisplayName], FParser.Position);
end;

function FindSimilarFreeVar(const Proc: TIDProcedure; CurInstruction: TILInstruction; Src: TIDVariable): TIDVariable;
var
  VItem: TIDVariable;
  LCode: TILInstruction;
begin
  VItem := Proc.VarSpace.First;
  while Assigned(VItem) do
  begin
    LCode := TILInstruction(VItem.LastRWInstruction);
    if Assigned(LCode) and (LCode.Position <= CurInstruction.Position) then
    begin
      if (VItem.DataType.ActualDataType = Src.DataType.ActualDataType) and
         (VItem.Reference = Src.Reference) and
         (VItem.IsTemporary) and
         (VItem <> Src) then
      begin
        if not (VarLoopIndex in VItem.Flags) or (LCode.CFBlock <> CurInstruction.CFBlock) then
          Exit(VItem);
      end;
    end;
    VItem := TIDVariable(VItem.NextItem);
  end;
  Result := nil;
end;

procedure ReplaceVar(const Proc: TIDProcedure; const DelVar, ReplVar: TIDVariable);
var
  Code: TILInstruction;
  BreakEnum: Boolean;
  VItem: TIDVariable;
  RCPathCount: UInt32;
begin
  RCPathCount := 1;
  BreakEnum := False;
  Proc.VarSpace.Delete(DelVar);
  Code := TIL(Proc.IL).First;
  while Assigned(Code) do begin
    Code.EnumerateArgs(
      procedure(const Arg: TIDExpression; var BreakEnum: Boolean)
      begin
        if Arg.Declaration = DelVar then
          Arg.Declaration := ReplVar;
      end,
      BreakEnum);
    Code.IncReferences(RCPathCount);
    Code := Code.Next;
  end;
  VItem := Proc.VarSpace.First;
  while Assigned(VItem) do begin
    if VItem.Absolute = DelVar then
      VItem.Absolute := ReplVar;
    VItem := TIDVariable(VItem.NextItem);
  end;
end;

procedure TNPUnit.Opt_ReuseTMPVars(const Proc: TIDProcedure);
var
  Code, FirstWInstruction: TILInstruction;
  Dest: TIDExpression;
  DestVar: TIDVariable;
  ReplVar: TIDVariable;
begin
  if Proc.TempVars.Count = 0 then
    Exit;

  Code := TIL(Proc.IL).First;
  while Assigned(Code) do begin
    if Code is TILDestInstruction then
    begin
      Dest := TILDestInstruction(Code).Destination;
      if Assigned(Dest) and Dest.IsTMPVar then
      begin
        DestVar := Dest.AsVariable;
        FirstWInstruction := TILInstruction(DestVar.FirstWInstruction);
        // замене подвеграются только временные переменные (кроме цикловых переменных)
        // для которых это первая инструкция
        if (FirstWInstruction = Code) and not (VarLoopIndex in DestVar.Flags) then
        begin
          ReplVar := FindSimilarFreeVar(Proc, Code, DestVar);
          if Assigned(ReplVar) then
            ReplaceVar(Proc, DestVar, ReplVar);
        end;
      end;
    end;
    Code := Code.Next;
  end;
  Proc.VarSpace.Reindex;
end;

procedure TNPUnit.Opt_ReduceIncRefDecRef(const Proc: TIDProcedure);
begin

end;

procedure TNPUnit.Opt_ReduceTMPVars(const Proc: TIDProcedure);
begin

end;

procedure TNPUnit.ReleaseExpression(SContext: PSContext; Expr: TIDExpression);
begin

end;

procedure TNPUnit.ReleaseExpression(const EContext: TEContext; Expr: TIDExpression);
begin
  ReleaseExpression(EContext.SContext, Expr);
end;

procedure TNPUnit.ReleaseExpression(Expr: TIDExpression);
begin

end;

procedure TNPUnit.ReleaseExpressions(SContext: PSContext; Exprs: TIDExpressions);
begin
  // в цепочке A.B.C... только первый элемент может быть временной переменной
  if Length(Exprs) > 0 then
    ReleaseExpression(SContext, Exprs[0]);
end;

{parser inline methods}

function TNPUnit.parser_CurTokenID: TTokenID;
begin
  Result := TTokenID(FParser.CurrentTokenID);
end;

function TNPUnit.parser_Position: TTextPosition;
begin
  Result := FParser.Position;
end;

function TNPUnit.parser_PrevPosition: TTextPosition;
begin
  Result := FParser.PrevPosition;
end;

function TNPUnit.parser_IdentifireType: TIdentifierType;
begin
  Result := FParser.IdentifireType;
end;

function TNPUnit.parser_Line: Integer;
begin
  Result := FParser.Position.Row;
end;

function TNPUnit.parser_TokenLexem(const TokenID: TTokenID): string;
begin
  Result := FParser.TokenLexem(TokenID);
end;

class procedure TNPUnit.ILWrite(SContext: PSContext; Instruction: TILInstruction);
begin
  if Assigned(SContext) and SContext.WriteIL then
    SContext.IL.Write(Instruction);
end;

class procedure TNPUnit.ILWrite(const EContext: TEContext; Instruction: TILInstruction);
begin
  if Assigned(EContext.SContext) and EContext.SContext.WriteIL then
    EContext.SContext.IL.Write(Instruction);
end;

class procedure TNPUnit.ILWrite(Proc: TIDProcedure; Instruction: TILInstruction);
begin
  if Assigned(Proc.IL) then
    TIL(Proc.IL).Write(Instruction);
end;

class procedure TNPUnit.ILWriteFirst(SContext: PSContext; Instruction: TILInstruction);
begin
  if Assigned(SContext) and SContext.WriteIL then
    SContext.IL.InsertFirst(Instruction);
end;

class procedure TNPUnit.ILDelete(SContext: PSContext; FromInstruction, ToInstruction: TILInstruction);
begin
  SContext.IL.Delete(FromInstruction, ToInstruction);
end;

procedure TNPUnit.CheckIntfSectionMissing(Scope: TScope);
begin
  if not Assigned(Scope) then
    ERROR_INTF_SECTION_MISSING;
end;

class procedure TNPUnit.InsertToScope(Scope: TScope; Item: TIDDeclaration);
begin
  if not Scope.InsertID(Item) then
    ERROR_ID_REDECLARATED(Item);
end;

class procedure TNPUnit.InsertToScope(Scope: TScope; const ID: string; Declaration: TIDDeclaration);
begin
  if Assigned(Scope.InsertNode(ID, Declaration)) then
    ERROR_ID_REDECLARATED(Declaration);
end;

class function TNPUnit.MatchUnarOperator(Op: TOperatorID; Right: TIDType): TIDType;
begin
  Right := Right.ActualDataType;
  Result := Right.UnarOperator(Op, Right);
  if Assigned(Result) then
    Exit;
  if (Right.DataTypeID = dtGeneric) then
    Exit(Right);
end;

function TNPUnit.MatchArrayImplicit(SContext: PSContext; Source: TIDExpression; DstArray: TIDArray): TIDExpression;
begin
  Assert(False);
end;

class function TNPUnit.MatchArrayImplicitToRecord(Source: TIDExpression; Destination: TIDStructure): TIDExpression;
begin
  Assert(False);
end;

procedure TNPUnit.WriteRecordFromTuple(SContext: PSContext; Source, Destination: TIDExpression);
//var
//  i: Integer;
//  ItemExpr: TIDExpression;
//  Struct: TIDStructure;
//  Field: TIDVariable;
//  Tuple: TIDDynArrayConstant;
//  DstExpr: TIDMultiExpression;
begin
  Assert(False);
//  Tuple := Source.AsDynArrayConst;
//
//  Struct := Destination.DataType as TIDStructure;
//
//  Field := Struct.FirstField;
//
//  for i := 0 to Tuple.ArrayLength - 1 do begin
//    ItemExpr := Tuple.Value[i];
//    DstExpr := TIDMultiExpression.Create(TIDExpressions.Create(Destination, TIDExpression.Create(Field, Field.TextPosition)), Destination.TextPosition);
//
//    ILWrite(SContext, TIL.IL_Move(DstExpr, ItemExpr));
//
//    Field := TIDVariable(Field.NextItem);
//  end;
end;

class function TNPUnit.MatchSetImplicit(Source: TIDExpression; Destination: TIDSet): TIDExpression;
var
  i: Integer;
  CArray: TIDDynArrayConstant;
  SExpr: TIDExpression;
  ImplicitCast: TIDDeclaration;
  //NeedCallImplicits: Boolean;
  EnumDecl: TIDType;
  ItemValue, SetValue: Int64;
begin
  SetValue := 0;
  EnumDecl := Destination.BaseType;
  CArray := Source.AsDynArrayConst;
  for i := 0 to CArray.ArrayLength - 1 do begin
    SExpr := CArray.Value[i];
    ImplicitCast := CheckImplicit(SExpr,  EnumDecl);
    if not Assigned(ImplicitCast) then
      ERROR_INCOMPATIBLE_TYPES(SExpr, EnumDecl);

    {if ImplicitCast.ItemType = itProcedure then
      NeedCallImplicits := True;}

    ItemValue := SExpr.AsIntConst.Value;

    SetValue := SetValue or (1 shl ItemValue);
  end;
  Result := TIDExpression.Create(TIDIntConstant.CreateAnonymous(nil, SYSUnit._Int32, SetValue), Source.TextPosition);
end;

class function TNPUnit.ConstDynArrayToSet(const CDynArray: TIDExpression; TargetSetType: TIDSet): TIDExpression;
var
  i: Integer;
  CArray: TIDDynArrayConstant;
  SExpr: TIDExpression;
  ImplicitCast: TIDDeclaration;
  ItemType: TIDType;
  ItemValue, SetValue: Int64;
begin
  SetValue := 0;
  CArray := CDynArray.AsDynArrayConst;
  ItemType := TargetSetType.BaseType;
  Assert(CArray.ArrayLength <= 64);
  for i := 0 to CArray.ArrayLength - 1 do begin
    SExpr := CArray.Value[i];
    ImplicitCast := CheckImplicit(SExpr, ItemType);
    if not Assigned(ImplicitCast) then
      ERROR_INCOMPATIBLE_TYPES(SExpr, ItemType);

    ItemValue := SExpr.AsIntConst.Value;
    SetValue := SetValue or (1 shl ItemValue);
  end;
  Result := IntConstExpression(SetValue);
  Result.TextPosition := CDynArray.TextPosition;
end;

function TNPUnit.FindImplicitFormBinarOperators(const Operators: TIDPairList; const Right: TIDType; out BetterFactor: Integer; out BetterOp: TIDDeclaration): TIDDeclaration;
var
  ParamFactor: Integer;
  ImplicitCast: TIDDeclaration;
  ParamDataType: TIDType;
  Node: TIDPairList.PAVLNode;
begin
  Result := nil;
  BetterFactor := 0;
  Node := Operators.First;
  while Assigned(Node) do begin
    ParamDataType := TIDType(Node.Key);
    ImplicitCast := MatchImplicit(Right, ParamDataType);
    if Assigned(ImplicitCast) then begin
      ParamFactor := ImplicitFactor(Right.DataTypeID, ParamDataType.DataTypeID);
      if ParamFactor > BetterFactor then begin
        BetterFactor := ParamFactor;
        BetterOp := Node.Data as TIDDeclaration;
        Result := ImplicitCast;
      end;
    end;
    Node := Operators.Next(Node);
  end;
end;

function TNPUnit.GetPublicClass(const Name: string): TIDClass;
var
  Decl: TIDDeclaration;
begin
  Decl := IntfSection.FindIDRecurcive(Name);
  if not Assigned(Decl) or not (Decl is TIDClass) then
    ERROR_UNDECLARED_ID(Self.Name + '.' + Name, parser_PrevPosition);
  Result := TIDClass(Decl);
end;

function TNPUnit.GetPublicType(const Name: string): TIDType;
var
  Decl: TIDDeclaration;
begin
  Decl := IntfSection.FindIDRecurcive(Name);
  if not Assigned(Decl) or not (Decl is TIDType) then
    ERROR_UNDECLARED_ID(Self.Name + '.' + Name, parser_PrevPosition);
  Result := TIDType(Decl);
end;

function GetOperatorInstance(Op: TOperatorID; Left, Right: TIDExpression): TIDExpression;
var
  LDT, RDT: TIDType;
  Decl: TIDDeclaration;
begin
  LDT := Left.DataType.ActualDataType;
  RDT := Right.DataType.ActualDataType;
  // поиск оператора (у левого операнда)
  Decl := LDT.BinarOperator(Op, RDT);
  if Assigned(Decl) then
    Exit(Left);
  // поиск оператора (у правого операнда)
  Decl := RDT.BinarOperatorFor(Op, LDT);
  if Assigned(Decl) then
    Exit(Right);
  Result := nil
end;

function TNPUnit.MatchBinarOperatorWithImplicit(SContext: PSContext; Op: TOperatorID; var Left, Right: TIDexpression): TIDDeclaration;
  function WriteImplicitCast(Implicit: TIDDeclaration; Src: TIDExpression): TIDExpression;
  var
    PCall: TIDCallExpression;
  begin
    if Implicit.ItemType = itType then
    begin
      Result := GetTMPVarExpr(SContext, TIDType(Implicit));
      ILWrite(SContext, TIL.IL_Move(Result, Src));
    end else
    if Implicit.ItemType = itProcedure then
    begin
      Result := GetTMPVarExpr(SContext, Implicit.DataType);
      PCall := TIDCallExpression.Create(Implicit);
      PCall.TextPosition := Src.TextPosition;
      PCall.ArgumentsCount := 2;
      PCall.Instance := GetOperatorInstance(Op, Result, Src);
      Result := Process_CALL_direct(SContext, PCall, TIDExpressions.Create(Result, Src));
    end else begin
      ERROR_FEATURE_NOT_SUPPORTED;
      Result := nil;
    end;
  end;
var
  LeftDT, RightDT: TIDType;
  Operators: TIDPairList;
  LeftImplicit, RightImplicit, LeftBinarOp, RightBinarOp: TIDDeclaration;
  LeftImplicitFactor, RightImplicitFactor: Integer;
begin
  LeftDT := Left.DataType.ActualDataType;
  RightDT := Right.DataType.ActualDataType;

  Operators := LeftDT.BinarOperators[Op];
  if Assigned(Operators) then
    LeftImplicit := FindImplicitFormBinarOperators(Operators, RightDT, LeftImplicitFactor, LeftBinarOp)
  else
    LeftImplicit := nil;

  Operators := RightDT.BinarOperators[Op];
  if Assigned(Operators) then
    RightImplicit := FindImplicitFormBinarOperators(Operators, LeftDT, RightImplicitFactor, RightBinarOp)
  else
    RightImplicit := nil;

  if not Assigned(LeftImplicit) and not Assigned(RightImplicit) then
    ERROR_NO_OVERLOAD_OPERATOR_FOR_TYPES(Op, Left, Right);

  if LeftImplicitFactor >= RightImplicitFactor then
  begin
    Result := LeftBinarOp;
    Right := WriteImplicitCast(LeftImplicit, Right);
  end else begin
    Result := RightBinarOp;
    Left := WriteImplicitCast(RightImplicit, Left);
  end;
end;

class function TNPUnit.MatchOperatorIn(const Left, Right: TIDExpression): TIDDeclaration;
var
  RangeConst: TIDRangeConstant;
  DataType: TIDType;
begin
  RangeConst := Right.Declaration as TIDRangeConstant;

  DataType := RangeConst.Value.LBExpression.DataType;
  Result := CheckImplicit(Left, DataType);
  if Assigned(Result) then
  begin
    DataType := RangeConst.Value.HBExpression.DataType;
    Result := CheckImplicit(Left, DataType);
  end;
  // пока без проверки на пользовательские операторы
end;

function TNPUnit.MatchBinarOperatorWithTuple(SContext: PSContext; Op: TOperatorID; var CArray: TIDExpression; const SecondArg: TIDExpression): TIDDeclaration;
var
  DataType: TIDType;
begin
  Result := nil;
  DataType := SecondArg.DataType;
  if DataType.DataTypeID = dtSet then begin
    CArray := MatchSetImplicit(CArray, DataType as TIDSet);
    Result := SYSUnit._Boolean; // tmp  надо проверять оператор
  end;
end;

function TNPUnit.MatchBinarOperator(SContext: PSContext; Op: TOperatorID; var Left, Right: TIDExpression): TIDDeclaration;
var
  LeftDT, RightDT: TIDType;
  L2RImplicit, R2LImplicit: TIDDeclaration;
begin
  LeftDT := Left.DataType.ActualDataType;
  RightDT := Right.DataType.ActualDataType;
  // поиск оператора (у левого операнда)
  Result := LeftDT.BinarOperator(Op, RightDT);
  if Assigned(Result) then
    Exit;
  // поиск оператора (у правого операнда)
  Result := RightDT.BinarOperatorFor(Op, LeftDT);
  if Assigned(Result) then
    Exit;

  if (LeftDT.DataTypeID = dtGeneric) then
    Exit(LeftDT);

  if RightDT.DataTypeID = dtGeneric then
    Exit(RightDT);

  if (Op = opIn) and (Right.Declaration.ClassType = TIDRangeConstant) then
  begin
    Result := MatchOperatorIn(Left, Right);
    Exit;
  end;

  if (LeftDT.DataTypeID = dtClass) and
     (RightDT.DataTypeID = dtClass) then
  begin
    if TIDClass(LeftDT).IsInheritsForm(TIDClass(RightDT)) then
      Exit(SYSUnit._Boolean);
    if TIDClass(RightDT).IsInheritsForm(TIDClass(LeftDT)) then
      Exit(SYSUnit._Boolean);
  end;

  // если ненайдено напрямую - ищем через неявное привидение
  L2RImplicit := CheckImplicit(Left, RightDT);
  R2LImplicit := CheckImplicit(Right, LeftDT);
  if L2RImplicit is TIDType then
  begin
    Result := TIDType(L2RImplicit).BinarOperator(Op, RightDT);
    if Assigned(Result) then
    begin
      Left := MatchImplicit3(SContext, Left, RightDT);
      Exit;
    end;
  end;
  if R2LImplicit is TIDType then
  begin
    Result := TIDType(R2LImplicit).BinarOperator(Op, LeftDT);
    if Assigned(Result) then
    begin
      Right := MatchImplicit3(SContext, Right, LeftDT);
      Exit;
    end;
  end;
  if Assigned(L2RImplicit) then
  begin
    Result := RightDT.BinarOperator(Op, L2RImplicit.DataType);
    if Assigned(Result) then
    begin
      Left := MatchImplicit3(SContext, Left, RightDT);
      Exit;
    end;
  end;
  if Assigned(R2LImplicit) then
  begin
    Result := LeftDT.BinarOperator(Op, R2LImplicit.DataType);
    if Assigned(Result) then
    begin
      Right := MatchImplicit3(SContext, Right, LeftDT);
      Exit;
    end;
  end;
  Result := nil;
end;

procedure TNPUnit.MatchProc(CallExpr: TIDExpression; const ProcParams: TVariableList; var CallArgs: TIDExpressions);
var
  i, ArgIdx, pc: Integer;
  Param: TIDVariable;
  Arg: TIDExpression;
  Implicit: TIDDeclaration;
  CallArgsCount: Integer;
begin
  pc := Length(ProcParams);
  CallArgsCount := Length(CallArgs);
  if CallArgsCount > pc then
    ERROR_TOO_MANY_ACTUAL_PARAMS(CallArgs[pc]);

  ArgIdx := 0;
  for i := 0 to pc - 1 do begin
    Param := ProcParams[i];
    if VarHiddenParam in Param.Flags then
      Continue; // пропускаем скрытые параметры

    if (ArgIdx < CallArgsCount) then
    begin
      Arg := CallArgs[ArgIdx];
      Inc(ArgIdx);
      if Assigned(Arg) then
      begin
        if (Arg.ItemType = itVar) and (Arg.DataType = _Void) then
        begin
          if VarOut in Param.Flags then
            Arg.AsVariable.DataType := Param.DataType
          else
            ERROR_INPLACEVAR_ALLOWED_ONLY_FOR_OUT_PARAMS(Arg.AsVariable);
        end;

        Implicit := CheckImplicit(Arg, Param.DataType);
        if Assigned(Implicit) then
          continue;
        ERROR_INCOMPATIBLE_TYPES(Arg, Param.DataType);
      end else
      if not Assigned(Param.DefaultValue) then
        ERROR_NOT_ENOUGH_ACTUAL_PARAMS(CallExpr);
    end;
  end;
end;

class function TNPUnit.MatchProcedureTypes(Src: TIDProcType; Dst: TIDProcType): TIDType;
begin
  if StrictMatchProcSingnatures(Src.Params, Dst.Params, Src.ResultType, Dst.ResultType) then
    Result := Dst
  else
    Result := nil;
end;

class function TNPUnit.StrictMatchProc(const Src, Dst: TIDProcedure): Boolean;
begin
  Result := StrictMatchProcSingnatures(Src.ExplicitParams, Dst.ExplicitParams, Src.ResultType, Dst.ResultType);
end;

class function TNPUnit.StrictMatchProcSingnatures(const SrcParams, DstParams: TVariableList; const SrcResultType, DstResultType: TIDType): Boolean;
var
  i: Integer;
  SParamsCount,
  DParamsCount: Integer;
  SParamType, DParamType: TIDType;
begin
  // проверяем на соответсвтие возвращаемый результат (если есть)
  if SrcResultType <> DstResultType then
    Exit(False);

  if Assigned(DstResultType) and Assigned(DstResultType) then
  begin
    if SrcResultType.ActualDataType <> DstResultType.ActualDataType then
      Exit(False);
  end;

  SParamsCount := Length(SrcParams);
  DParamsCount := Length(DstParams);

  // проверяем на соответсвтие кол-во параметров
  if SParamsCount <> DParamsCount then
    Exit(False);

  // проверяем на соответсвтие типов параметров
  for i := 0 to SParamsCount - 1 do begin
    SParamType := SrcParams[i].DataType.ActualDataType;
    DParamType := DstParams[i].DataType.ActualDataType;
    if SParamType <> DParamType then
      Exit(False);
  end;
  Result := True;
end;

class procedure TNPUnit.MatchConstantExplicit(Source: TIDExpression; Dest: TIDType);
begin
  if (Source.DataTypeID = dtString) and
     (Dest.DataTypeID = dtAnsiString) then
  begin
    if not IsAnsiString(Source.AsStrConst.Value) then
      AbortWork('The constant: "%s" is not ANSI', [Source.AsStrConst.Value], Source.TextPosition);
    Source.Declaration.DataType := SYSUnit._AnsiString;
  end;
end;

class function TNPUnit.MatchConstDynArrayImplicit(Source: TIDExpression; Destination: TIDType): TIDType;
var
  i, c: Integer;
  SConst: TIDDynArrayConstant;
  SExpr: TIDExpression;
  DstElementDataType: TIDType;
  ImplicitCast: TIDDeclaration;
  SrcArrayElement: TIDType;
begin
  SConst := TIDDynArrayConstant(Source.Declaration);
  c := Length(SConst.Value);
  if c = 0 then Exit(Destination);

  case Destination.DataTypeID of
    dtSet: begin
      DstElementDataType := TIDSet(Destination){.BaseType}; // tmp
    end;
    dtDynArray, dtOpenArray: begin
      DstElementDataType := TIDDynArray(Destination).ElementDataType;
    end;
    else begin
      ERROR_INCOMPATIBLE_TYPES(Source, Destination);
      DstElementDataType := nil;
    end;
  end;

  // проверка каждого элемента
  for i := 0 to c - 1 do begin
    SExpr := SConst.Value[i];
    ImplicitCast := CheckImplicit(SExpr, DstElementDataType);
    if not Assigned(ImplicitCast) then
      ERROR_INCOMPATIBLE_TYPES(SExpr, DstElementDataType);
  end;
  Result := Destination;

  // подгонка типа константного массива под тип приемника
  if Destination.DataTypeID <> dtOpenArray then
  begin
    SrcArrayElement := TIDDynArray(SConst.DataType).ElementDataType;
    if DstElementDataType.DataSize <> SrcArrayElement.DataSize then
      SConst.DataType := Destination
  end;
end;

class procedure TNPUnit.CheckAndMatchDerefExpression(SContext: PSContext; var Source: TIDExpression);
begin

end;

class function TNPUnit.MatchDynArrayImplicit(Source: TIDExpression; Destination: TIDType): TIDType;
var
  SrcDataType: TIDArray;
begin
  if (Destination.DataTypeID in [dtDynArray, dtOpenArray]) and
     (Source.DataTypeID = Destination.DataTypeID) then
  begin
    SrcDataType := TIDArray(Source.DataType);
    if SrcDataType.ElementDataType.ActualDataType = TIDArray(Destination).ElementDataType.ActualDataType then
      Exit(Destination);
  end;
  Result := nil;
end;

class function TNPUnit.MatchImplicit(Source, Destination: TIDType): TIDDeclaration;
begin
  // ищем явно определенный implicit у источника
  Result := Source.GetImplicitOperatorTo(Destination);
  if Assigned(Result) then
    Exit;

  // ищем явно определенный implicit у приемника
  Result := Destination.GetImplicitOperatorFrom(Source);
  if Assigned(Result) then
    Exit;

  // если не нашли точных имплиситов, ищем подходящий (у источника)
  Result := Source.FindImplicitOperatorTo(Destination);
  if Assigned(Result) then
    Exit;

  // если не нашли точных имплиситов, ищем подходящий (у приемника)
  Result := Destination.FindImplicitOperatorFrom(Source);
  if Assigned(Result) then
    Exit;

  if (Destination.DataTypeID = dtGeneric) or (Source.DataTypeID = dtGeneric) then
    Exit(Source); // нужна еще проверка на констрейты
end;

class function TNPUnit.CheckImplicit(Source: TIDExpression; Dest: TIDType): TIDDeclaration;
//var
//  SDataType: TIDType;
//  SrcDTID, DstDTID: TDataTypeID;
begin
  Assert(False);
//  SDataType := Source.DataType.ActualDataType;
//  Dest := Dest.ActualDataType;
//
//  // ищем явно определенный implicit у источника
//  Result := SDataType.GetImplicitOperatorTo(Dest);
//  if Result is TIDInternalOpImplicit then
//    Result := TIDInternalOpImplicit(Result).Check(Source, Dest);
//
//  if Assigned(Result) then
//    Exit;
//
//  // ищем явно определенный implicit у приемника
//  Result := Dest.GetImplicitOperatorFrom(SDataType);
//  if Assigned(Result) then
//    Exit;
//
//  // если не нашли точных имплиситов, ищем подходящий (у источника)
//  Result := SDataType.FindImplicitOperatorTo(Dest);
//  if Assigned(Result) then
//    Exit;
//
//  // если не нашли точных имплиситов, ищем подходящий (у приемника)
//  Result := Dest.FindImplicitOperatorFrom(SDataType);
//  if Assigned(Result) then
//    Exit;
//
//  {if (DstDTID = dtSet) and (SDataType is TIDDynArray) then
//  begin
//    Result := CheckSetImplicit(Source, TIDSet(Dest));
//    Exit;
//  end;}
//
//  // проверка на nullpointer
//  if (Source.Declaration = SYSUnit._NullPtrConstant) and
//     (Dest.IsReferenced or (Dest is TIDProcType)) then
//    Exit(Dest);
//
//  SrcDTID := Source.DataTypeID;
//  DstDTID := Dest.DataTypeID;
//
//  if (Source.IsDynArrayConst) and (Dest.DataTypeID = dtRecord) then
//  begin
//    if MatchArrayImplicitToRecord(Source, TIDStructure(Dest)) <> nil then
//      Exit(Dest);
//  end;
//
//  {если оба - классы, то проверяем InheritsForm
//  if (SrcDTID = dtClass) and (DstDTID = dtClass) then
//  begin
//    if TIDClass(Source.DataType).IsInheritsForm(TIDClass(Dest)) then
//      Exit(Dest);
//  end;}
//
//  // есди приемник - class of
//  if DstDTID = dtClassOf then
//  begin
//    Result := MatchImplicitClassOf(Source, TIDClassOf(Dest));
//    if Assigned(Result) then
//      Exit;
//  end;
//
//  if (SrcDTID = dtPointer) and (DstDTID = dtPointer) then
//  begin
//    if (TIDPointer(SDataType).ReferenceType = nil) and
//       (TIDPointer(Dest).ReferenceType = nil) then
//      Exit(Source.DataType);
//
//    // it needs to check !!!
//    if not Assigned(TIDPointer(SDataType).ReferenceType) or not Assigned(TIDPointer(Dest).ReferenceType) then
//      Exit(Source.DataType);
//
//    if TIDPointer(SDataType).ReferenceType.ActualDataType = TIDPointer(Dest).ReferenceType.ActualDataType then
//      Exit(Source.DataType);
//  end;
//
//  if (SrcDTID = dtProcType) and (DstDTID = dtProcType) then
//    Result := MatchProcedureTypes(TIDProcType(SDataType), TIDProcType(Dest))
//  else
//  if (Source.Declaration.ItemType = itConst) and (SrcDTID = dtDynArray) then
//    Result := MatchConstDynArrayImplicit(Source, Dest)
//  else begin
//    Result := Dest.GetImplicitOperatorFrom(SDataType);
//    if not Assigned(Result) then
//      Result := MatchDynArrayImplicit(Source, Dest);
//  end;
//  if (DstDTID = dtGeneric) or (SrcDTID = dtGeneric) then
//    Exit(Source.AsType); // нужна еще проверка на констрейты
end;

function TNPUnit.MatchImplicitOrNil(SContext: PSContext; Source: TIDExpression; Dest: TIDType): TIDExpression;
begin
  Assert(False);
end;

function TNPUnit.MatchImplicit3(SContext: PSContext; Source: TIDExpression; Dest: TIDType): TIDExpression;
begin
  Result := MatchImplicitOrNil(SContext, Source, Dest);
    if not Assigned(Result) then
      ERROR_INCOMPATIBLE_TYPES(Source, Dest);
end;

class function TNPUnit.MatchImplicitClassOf(Source: TIDExpression; Destination: TIDClassOf): TIDDeclaration;
var
  SrcDecl: TIDDeclaration;
  DstDecl: TIDClass;
begin
  SrcDecl := Source.Declaration;
  DstDecl := TIDClass(Destination.ReferenceType);
  // если источник - тип
  if SrcDecl.ItemType = itType then begin
    if SrcDecl is TIDStructure then
    begin
      if (SrcDecl = DstDecl) or  TIDStructure(SrcDecl).IsInheritsForm(DstDecl) then
        Exit(Destination);
    end else
      AbortWork(errCLASSTypeRequired, Source.TextPosition);
  end else
  // если источник - переменная
  if SrcDecl.ItemType = itVar then begin
   if SrcDecl.DataType.DataTypeID = dtClassOf then
   begin
     SrcDecl := TIDClassOf(SrcDecl.DataType).ReferenceType;
     if (SrcDecl = DstDecl) or TIDClass(SrcDecl).IsInheritsForm(DstDecl) then
       Exit(Destination);
   end;
  end;
  Result := nil;
end;

class function TNPUnit.MatchExplicit(const Source: TIDExpression; Destination: TIDType): TIDDeclaration;
var
  SrcDataType: TIDType;
  DstDataType: TIDType;
  ExplicitIntOp: TIDOperator;
begin
  Assert(False);
//  SrcDataType := Source.DataType.ActualDataType;
//  DstDataType := Destination.ActualDataType;
//  if SrcDataType = DstDataType then
//    Exit(Destination);
//
//  Result := SrcDataType.GetExplicitOperatorTo(DstDataType);
//  if not Assigned(Result) then
//    Result := DstDataType.GetExplicitOperatorFrom(SrcDataType);
//
//  if Assigned(Result) then
//    Exit;
//
//  ExplicitIntOp := DstDataType.SysExplicitFromAny;
//  if ExplicitIntOp is TIDInternalOpImplicit then
//  begin
//    if TIDInternalOpImplicit(ExplicitIntOp).Check(Source, Destination) <> nil then
//      Exit(Destination);
//  end;
//  Result := nil;
end;


class function TNPUnit.MatchOverloadProc(Item: TIDExpression; const CallArgs: TIDExpressions; CallArgsCount: Integer): TIDProcedure;
const
  cWFactor = 1000000;            // Масштабирующий коэффициент
var
  i,
  ParamFactor,                   // Коэффициент совпадения параметра
  DataLossCount,                 // Кол-во возможных потерь на декларацию
  MinDataLossFactor,             // Минимальное кол-во возможных потерь
  DeclarationFactor,             // Коэффициент совпадения всей декларации
  MaxMatchedFactor,              // Максимальный найденный коэффициент совпадения всей декларации
  MatchedCount: Integer;         // Кол-во деклараций имеющих одинаковый максимальный коэффициент
  ParamDataType,                 // Тип формального параметра процедуры
  ArgDataType                    // Тип передаваемого аргумента
  : TIDType;
  Param: TIDVariable;
  ImplicitCast: TIDDeclaration;
  Declaration: TIDProcedure;
  SrcDataTypeID,
  DstDataTypeID: TDataTypeID;
begin
  Result := nil;
  MaxMatchedFactor := 0;
  MatchedCount := 0;
  MinDataLossFactor := MaxInt;
  Declaration := TIDProcedure(Item.Declaration);
  repeat
    DeclarationFactor := 0;
    DataLossCount := 0;
    if (Declaration.ParamsCount = 0) and (CallArgsCount = 0) then
      DeclarationFactor := 100*cWFactor
    else
    if CallArgsCount <= Declaration.ParamsCount then
    begin
      for i := 0 to Declaration.ParamsCount - 1 do begin
        Param := Declaration.ExplicitParams[i];
        // Если аргумент не пропущен
        if (i < CallArgsCount) and Assigned(CallArgs[i]) then
        begin
          ParamDataType := Param.DataType.ActualDataType;
          ArgDataType := CallArgs[i].DataType.ActualDataType;
          // сравнение типов формального параметра и аргумента (пока не учитываются модификаторы const, var... etc)
          if ParamDataType.DataTypeID = dtGeneric then
            ParamFactor := 90   // tmp
          else
          if ParamDataType = ArgDataType then
            ParamFactor := 100
          else begin
            // Подбираем implicit type cast
            ParamFactor := 0;
            ImplicitCast := MatchImplicit(ArgDataType, ParamDataType);
            if Assigned(ImplicitCast) then
            begin
              SrcDataTypeID := ArgDataType.DataTypeID;
              DstDataTypeID := ParamDataType.DataTypeID;
              ParamFactor := ImplicitFactor(SrcDataTypeID, DstDataTypeID);
              Inc(DataLossCount, DataLossFactor(SrcDataTypeID, DstDataTypeID));
            end;
            if ParamFactor = 0 then begin
              DeclarationFactor := 0; // Данная декларация не подходит
              Break;                  // Ищим дальше
            end;
          end;
        end else begin
          // Если аргумент пропущен и если параметр имеет значение по умолчанию
          if VarHasDefault in param.Flags then
            ParamFactor := 100      // Аргумент подходит на 100%
          else begin
            DeclarationFactor := 0; // Данная декларация не подходит
            Break;                  // Ищим дальше
          end;
        end;
        DeclarationFactor := DeclarationFactor + ParamFactor;
      end;
      // Масштабируем получившийся коэффициент и усредняем его по кол-ву параметров
      DeclarationFactor := DeclarationFactor*cWFactor div Declaration.ParamsCount;
    end else begin
      Declaration := Declaration.NextOverload;
      Continue;
    end;

    if DataLossCount < MinDataLossFactor then
    begin
      MinDataLossFactor := DataLossCount;
      MaxMatchedFactor := DeclarationFactor;
      Result := Declaration;
      MatchedCount := 1;
    end else
    // Нашли новый максимальный коэффициент
    if DeclarationFactor > MaxMatchedFactor then
    begin
      MaxMatchedFactor := DeclarationFactor;
      Result := Declaration;
      MatchedCount := 1;
    end else
    if (DataLossCount = MinDataLossFactor) and
       (DeclarationFactor > 0) and
       (DeclarationFactor = MaxMatchedFactor) then
      Inc(MatchedCount);
    // Берем следующую overload декларацию
    Declaration := Declaration.NextOverload;
  until Declaration = nil;

  if MatchedCount = 0 then
    ERROR_OVERLOAD(Item)
  else
  if MatchedCount > 1 then
    ERROR_AMBIGUOUS_OVERLOAD_CALL(Item);
end;

procedure TNPUnit.parser_MatchParamNameIdentifier(ActualToken: TTokenID);
begin
  if ActualToken <> token_Identifier then
    ERROR_PARAM_NAME_ID_EXPECTED(ActualToken);
end;

procedure TNPUnit.parser_MatchSemicolon(const ActualToken: TTokenID);
begin
  if ActualToken <> token_semicolon then
    ERROR_SEMICOLON_EXPECTED;
end;

class procedure TNPUnit.CheckConstExpression(Expression: TIDExpression);
begin
  if not (Expression.Declaration.ItemType in [itConst, itProcedure]) then
    ERROR_CONST_EXPRESSION_REQUIRED(Expression);
end;

class procedure TNPUnit.CheckConstValueOverflow(Src: TIDExpression; DstDataType: TIDType);
var
  Matched: Boolean;
  ValueI64: Int64;
  ValueU64: Int64;
  DataType: TIDType;
begin
  if Src.Declaration is TIDIntConstant then
  begin
    DataType := DstDataType.ActualDataType;
    ValueI64 := TIDIntConstant(Src.Declaration).AsInt64;
    ValueU64 := TIDIntConstant(Src.Declaration).AsUInt64;
    case DataType.DataTypeID of
      dtInt8: Matched := (ValueI64 >= MinInt8) and (ValueI64 <= MaxInt8);
      dtInt16: Matched := (ValueI64 >= MinInt16) and (ValueI64 <= MaxInt16);
      dtInt32: Matched := (ValueI64 >= MinInt32) and (ValueI64 <= MaxInt32);
      //dtInt64: Matched := (IntValue >= MinInt64) and (IntValue <= MaxInt64); // always true
      dtUInt8: Matched := (ValueU64 <= MaxUInt8);
      dtUInt16: Matched := (ValueU64 <= MaxUInt16);
      dtUInt32: Matched := (ValueU64 <= MaxUInt32);
      dtUInt64: Matched := (ValueU64 <= MaxUInt64);
      dtBoolean: Matched := (ValueU64 <= 1);
      dtAnsiChar: Matched := (ValueU64 <= MaxUInt8);
      dtChar: Matched := (ValueU64 <= MaxUInt16);
    else
      Matched := True;
    end;
    if not Matched then
      ERROR_CONST_VALUE_OVERFLOW(Src, DstDataType);
  end;
  // необходимо реализовать проверку остальных типов
end;

function TNPUnit.CheckAndDelUnusedConsts(var ConstSpace: TConstSpace): Integer;
var
  Decl: TIDConstant;
begin
  Result := 0;
  Decl := ConstSpace.First;
  while Assigned(Decl) do
  begin
    // удаляем приватные процедуры которые никогда не использовали
    if (Decl.Scope <> FIntfScope) and (Decl.RefCount = 0) then
    begin
      inc(Result);
      ConstSpace.Delete(Decl);
      if not Decl.IsAnonymous then
        Hint('CONSTANT ' + Decl.Name + ' was eliminated as not used', []);
    end;
    Decl := TIDConstant(Decl.NextItem);
  end;
  ConstSpace.Reindex;
end;

procedure TNPUnit.CheckUnusedExprResult(var EContext: TEContext);
var
  Expr: TIDExpression;
  ExprCnt: Integer;
begin
  Assert(EContext.RPNExprCount >= 0);
  ExprCnt := (EContext.RPNExprCount);
  if ExprCnt > 0 then
  begin
    Expr := RPNReadExpression(EContext, ExprCnt - 1);
    ReleaseExpression(EContext.SContext, Expr);
    HINT_RESULT_EXPR_IS_NOT_USED(Expr);
  end;
end;

procedure TNPUnit.CheckEmptyExpression(Expression: TIDExpression);
begin
  if not Assigned(Expression) then
    ERROR_EMPTY_EXPRESSION;
end;

procedure TNPUnit.CheckEndOfFile(Token: TTokenID);
begin
  if Token = token_eof then
    ERROR_END_OF_FILE;
end;

procedure TNPUnit.SetProcGenericArgs(CallExpr: TIDCallExpression; Args: TIDExpressions);
var
  Proc: TIDProcedure;
  ArgsCount, ParamsCount: Integer;
  GDescriptor: PGenericDescriptor;
begin
  Proc := CallExpr.AsProcedure;
  GDescriptor := Proc.GenericDescriptor;
  if not Assigned(GDescriptor) then
    AbortWork(sProcHasNoGenericParams, [Proc.ProcTypeName, Proc.DisplayName], CallExpr.TextPosition);

  ParamsCount := Length(GDescriptor.GenericParams);
  ArgsCount := Length(Args);

  if ParamsCount > ArgsCount then
    AbortWork(sProcRequiresExplicitTypeArgumentFmt, [Proc.ProcTypeName, Proc.DisplayName], CallExpr.TextPosition);

  if ParamsCount < ArgsCount then
    AbortWork(sTooManyActualTypeParameters, CallExpr.TextPosition);

  CallExpr.GenericArgs := Args;
end;

procedure TNPUnit.SetUnitName(const Name: string);
begin
  FUnitName.Name := Name;
end;

class procedure TNPUnit.CheckIntExpression(Expression: TIDExpression);
begin
  if Expression.DataTypeID >= dtNativeUInt then
    ERROR_INTEGER_TYPE_REQUIRED(Expression.TextPosition);
end;

class procedure TNPUnit.CheckOrdinalExpression(Expression: TIDExpression);
begin
  if not Expression.Declaration.DataType.Ordinal then
    ERROR_ORDINAL_TYPE_REQUIRED(Expression.TextPosition);
end;

class procedure TNPUnit.CheckNotNullExpression(const Dest: TIDVariable; const Source: TIDExpression);
begin
  if Source.ItemType = itVar then
  begin
    if (VarNotNull in Dest.Flags) and not (VarNotNull in Source.AsVariable.Flags) then
      AbortWork('The NOT NULL reference: "%s" can not be assigned from a NULLABLE reference', [Dest.DisplayName], Source.TextPosition);
  end else
  { проверка на nullpointer }
  if (Source.Declaration = SYSUnit._NullPtrConstant) {and Dest.IsReferenced} then
  begin
    if VarNotNull in Dest.Flags then
      ERROR_CANNOT_ASSIGN_NULL_TO_NOTNULL(Source);
  end;
end;

class procedure TNPUnit.CheckNumericExpression(Expression: TIDExpression);
begin
  if not (Expression.DataTypeID in [dtInt8..dtChar]) then
    AbortWork(sNumericTypeRequired, Expression.TextPosition);
end;

class procedure TNPUnit.CheckPointerType(Expression: TIDExpression);
begin
  if (Expression.DataTypeID <> dtPointer) then
    AbortWork(sPointerTypeRequired, Expression.TextPosition);
end;

procedure TNPUnit.CheckProcedureType(DeclType: TIDType);
begin
  if (DeclType.DataTypeID <> dtProcType) then
    AbortWork('Procedure type required', fParser.Position);
end;

function IDVarCompare(const Key1, Key2: TIDVariable): NativeInt;
begin
  Result := NativeInt(Key1) - NativeInt(Key2);
end;

function TNPUnit.CheckAndMakeClosure(SContext: PSContext; const ProcDecl: TIDProcedure): TIDClosure;
var
  Closure: TIDClosure;
begin
  Closure := nil;
  TIL(ProcDecl.IL).EnumerateArgs(
    procedure (const Arg: TIDExpression; var BreakEnum: Boolean)
    var
      LVar: TIDVariable;
    begin
      {если это локальная переменная}
      if Arg.IsLocalVar then
      begin
        LVar := Arg.AsVariable;
        if LVar.Scope <> ProcDecl.EntryScope then
        begin
          if not Assigned(Closure) then
          begin
            Closure := TIDClosure.CreateClosure(SContext.Proc, ProcDecl);
            Closure.Ancestor := SYSUnit._TObject;
          end;
          {замена декларации локальной переменной на поле замыкания}
          Arg.Declaration := Closure.GetCapturedVar(LVar);
        end;
      end;
    end);

  if Assigned(Closure) then
    AddType(Closure);

  Result := Closure;
end;

procedure TNPUnit.MakeLoopBodyBegin(var Ctx: TLoopCodeContext);
{var
  IL: TIL;
  Item: TIDVariable;
  ArrExpr: TIDExpression;
  LoopVar: TIDVariable;}
begin
(*  IL := TIL(Ctx.Proc.IL);
  // генерируем код цикла прохода по массиву
  IL.AddVariable(Ctx.ArrParam);
  IL.CFBBegin(CFB_FOR_BODY);
  LoopVar := Ctx.Proc.GetTMPVar(SYSUnit._Int32);
  Ctx.LVarExpr := TIDExpression.Create(LoopVar);
  Ctx.LBStart := TIL.IL_Move(Ctx.LVarExpr, SYSUnit._ZeroExpression);
  IL.Write(Ctx.LBStart);          // инициализация нулем
  ArrExpr := TIDExpression.Create(Ctx.ArrParam);
  if Ctx.Arr.DataTypeID = dtStaticArray then
    Ctx.ALen := IntConstExpression(Ctx.Arr.Dimensions[0].ElementsCount)
  else begin
    Ctx.ALen := TIDExpression.Create(Ctx.Proc.GetTMPVar(SYSUnit._Int32));    // для динмического массива:
    IL.Write(TIL.IL_Length(Ctx.ALen, ArrExpr));                          // вычисляем длинну массива
    IL.Write(TIL.IL_Cmp(Ctx.LVarExpr, Ctx.ALen));                            // начальное сравнение
    Ctx.LBStart := TIL.IL_Jmp(0, cGreaterOrEqual, Ctx.RET);                  // выход за пределы цикла
    IL.Write(Ctx.LBStart);
  end;
  Item := Ctx.Proc.GetTMPRef(Ctx.Arr.ElementDataType);
  Ctx.ItemExpr := TIDExpression.Create(Item);
  IL.Write(TIL.IL_GetPtr(Ctx.ItemExpr, [ArrExpr, Ctx.LVarExpr])); *)
end;

procedure TNPUnit.MakeLoopBodyEnd(var Ctx: TLoopCodeContext);
{var
  IL: TIL;}
begin
{  IL := TIL(Ctx.Proc.IL);
  IL.Write(TIL.IL_Add(Ctx.LVarExpr, Ctx.LVarExpr, SYSUnit._OneExpression));  // инкремент
  IL.Write(TIL.IL_Cmp(Ctx.LVarExpr, Ctx.ALen));                              // сравнение
  IL.Write(TIL.IL_JmpNext(0, cLess, Ctx.LBStart));
  IL.CFBEnd(CFB_FOR_BODY);
  IL.Write(Ctx.RET);}
end;

procedure TNPUnit.CheckAndMakeInitFinalArray(const Arr: TIDArray);
{var
  ElementDT: TIDType;
  ArrParam: TIDVariable;
  CallExpr: TIDCallExpression;
  Ctx: TLoopCodeContext;}
begin
(*  ElementDT := Arr.ElementDataType;
  // ===============================================================
  // если элементы массива требуют инициализаии
  if mtNeedClear in ElementDT.ManagedFlags then
  begin
    if Assigned(ElementDT.InitProc) then
    begin
      Arr.InitProc := CreateArraySysProc(Arr, '$' + Arr.DisplayName + '$init', ArrParam);
      Ctx := TLoopCodeContext.Create(Arr.InitProc, ArrParam);
      // генерируем код цикла прохода по массиву
      MakeLoopBodyBegin(Ctx);
      // тело цикла
      {if Assigned(ElementDT.InitProc) then
      begin}
        CallExpr := TIDCallExpression.Create(ElementDT.InitProc);
        Ctx.IL.Write(TIL.IL_ProcCall(CallExpr, nil, nil, [Ctx.ItemExpr]));
      {end else
        Ctx.IL.Write(TIL.IL_Init(Ctx.ItemExpr));}
      // конец цикла
      MakeLoopBodyEnd(Ctx);
    end;
  end;
  // ===============================================================
  // если элементы массива требуют копирования
  if mtNeedIncRef in ElementDT.ManagedFlags then
  begin
    if Arr.DataTypeID = dtDynArray then
    begin
      case ElementDT.DataTypeID of
        dtClass: Arr.CopyProc := SYSUnit._CopyArrayOfObjProc;
        dtString, dtAnsiString: Arr.CopyProc := SYSUnit._CopyArrayOfStrProc;
      end;
    end else begin
      Arr.CopyProc := CreateArraySysProc(Arr, '$' + Arr.DisplayName + '$copy', ArrParam);
      Ctx := TLoopCodeContext.Create(Arr.CopyProc, ArrParam);
      // генерируем код цикла прохода по массиву
      MakeLoopBodyBegin(Ctx);
      // тело цикла
      if Assigned(ElementDT.CopyProc) then
      begin
        CallExpr := TIDCallExpression.Create(ElementDT.CopyProc);
        Ctx.IL.Write(TIL.IL_ProcCall(CallExpr, nil, nil, [Ctx.ItemExpr]));
      end else
        Ctx.IL.Write(TIL.IL_IncRef(Ctx.ItemExpr));
      // конец цикла
      MakeLoopBodyEnd(Ctx);
    end;
  end;
  // ===============================================================
  // если элементы массива требуют финализации
  if dtNeedFinal in ElementDT.ManagedFlags then
  begin
    if Arr.DataTypeID = dtDynArray then
    begin
      case ElementDT.DataTypeID of
        dtClass: Arr.FinalProc := SYSUnit._FinalArrayOfObjProc;
        dtString, dtAnsiString: Arr.FinalProc := SYSUnit._FinalArrayOfStrProc;
        dtVariant: Arr.FinalProc := SYSUnit._FinalArrayOfVarProc;
      end;
    end else begin
      Arr.FinalProc := CreateArraySysProc(Arr, '$' + Arr.DisplayName + '$final', ArrParam);
      Ctx := TLoopCodeContext.Create(Arr.FinalProc, ArrParam);
      // генерируем код цикла прохода по массиву
      MakeLoopBodyBegin(Ctx);
      // тело цикла
      if Assigned(ElementDT.FinalProc) then
      begin
        CallExpr := TIDCallExpression.Create(ElementDT.FinalProc);
        Ctx.IL.Write(TIL.IL_ProcCall(CallExpr, nil, nil, [Ctx.ItemExpr]));
      end else
        Ctx.IL.Write(TIL.IL_DecRef(Ctx.ItemExpr));
      // конец цикла
      MakeLoopBodyEnd(Ctx);
    end;
  end;   *)
end;

procedure TNPUnit.CheckAndMakeInitFinalStruct(const Struct: TIDStructure);
{var
  Fld: TIDVariable;
  Expr: TIDExpression;
  Proc: TIDProcedure;
  InitIL, CopyIL, FinalIL: TIL;}
begin
(*  InitIL := nil;
  CopyIL := nil;
  FinalIL := nil;
  Fld := Struct.Members.VarSpace.First;
  while Assigned(Fld) do begin
    // если поле структуры требует инициализаии
    if (Struct.DataTypeID in [dtRecord, dtStaticArray, dtDynArray]) and
       (mtNeedClear in Fld.DataType.ManagedFlags) then
    begin
      if not Assigned(InitIL) then
      begin
        Proc := CreateStructInitProc(Struct);
        InitIL := TIL(Proc.IL);
      end;
      Expr := TIDExpression.Create(Fld);
      InitIL.Write(TIL.IL_Init(Expr));
    end;
    // если поле структуры требует incref
    if (Struct.DataTypeID in [dtRecord, dtStaticArray, dtDynArray]) and
       (mtNeedIncRef in Fld.DataType.ManagedFlags) then
    begin
      if not Assigned(CopyIL) then
      begin
        Proc := CreateStructCopyProc(Struct);
        CopyIL := TIL(Proc.IL);
      end;
      Expr := TIDExpression.Create(Fld);
      CopyIL.Write(TIL.IL_IncRef(Expr));
    end;
    // если поле структуры требует финализация
    if dtNeedFinal in Fld.DataType.ManagedFlags then
    begin
      if not Assigned(FinalIL) then
      begin
        Proc := CreateStructFinalProc(Struct);
        FinalIL := TIL(Proc.IL);
      end;
      Expr := TIDExpression.Create(Fld);
      FinalIL.Write(TIL.IL_DecRef(Expr));
    end;
    Fld := TIDVariable(Fld.NextItem);
  end;*)
end;

procedure TNPUnit.CheckManagedInitFinal(const Proc: TIDProcedure);
{var
  Variable: TIDVariable;
  DataType: TIDType;
  FI, Code: TILInstruction;
  ManagedFlags: TManagedDataTypeFlags;
  Expr: TIDExpression;
  VarFlags: TVariableFlags;}
begin
(*  FI := TIL(Proc.IL).First;
  Variable := Proc.VarSpace.First;
  while Assigned(Variable) do begin
    DataType := Variable.DataType;
    if DataType.ItemType <> itType then
    begin
      Variable := TIDVariable(Variable.NextItem);
      continue;
    end;
    VarFlags := Variable.Flags;
    ManagedFlags := DataType.ManagedFlags;
    {результат обнуляем если нужно}
    if VarResult in VarFlags then
    begin
      if mtNeedClear in ManagedFlags then
      begin
        Expr := TIDExpression.Create(Variable, Proc.FirstBodyLine);
        Code := TIL.IL_Init(Expr);
        TIL(Proc.IL).InsertBefore(FI, Code);
      end;
    end else
    {исключаются константные и переданные по ссылке параметры}
    if not (VarConst in VarFlags) and
       not (VarOut in VarFlags) and
       not (VarInOut in VarFlags) then
    begin
      // проверка вызова контрукторов/деструкторов для структурых типов
      case DataType.DataTypeID of
        dtRecord: begin
          CheckAndCallRecordInit(Proc, Variable);
          CheckAndCallRecordFinal(Proc, Variable, nil);
        end;
        dtStaticArray: begin
          CheckAndCallArrayInit(Proc, Variable);
          CheckAndCallArrayFinal(Proc, Variable, nil);
        end;
        dtVariant: begin
          Expr := TIDExpression.Create(Variable, Proc.FirstBodyLine);
          Code := TIL.IL_Init(Expr);
          TIL(Proc.IL).InsertBefore(FI, Code);

          Expr := TIDExpression.Create(Variable, Proc.LastBodyLine);
          Code := TIL.IL_DecRef(Expr);
          TIL(Proc.IL).Write(Code);
        end;
      else
        // проверка вызова инициализации/финализации для простых типов
        if (mtNeedIncRef in ManagedFlags) and not Variable.Reference then
        begin
          // если параметр:
          if (VarParameter in VarFlags) and FUseARC then
          begin
            Expr := TIDExpression.Create(Variable, Proc.FirstBodyLine);
            Code := TIL.IL_IncRef(Expr);
            TIL(Proc.IL).InsertBefore(FI, Code);

            Expr := TIDExpression.Create(Variable, Proc.LastBodyLine);
            Code := TIL.IL_DecRef(Expr);
            TIL(Proc.IL).Write(Code);
          end else begin
          // если локальная переменная:
            if (dtNeedAlwaysFinal in ManagedFlags) and not Variable.IsTemporaryOwner then
            begin
              Expr := TIDExpression.Create(Variable, Proc.LastBodyLine);
              Code := TIL.IL_DecRef(Expr);
              TIL(Proc.IL).Write(Code);
            end;
            // временные переменные не инициализируются
            if not Variable.IsAnonymous then
            begin
              Expr := TIDExpression.Create(Variable, Proc.FirstBodyLine);
              Code := TIL.IL_Init(Expr);
              TIL(Proc.IL).InsertBefore(FI, Code);
              // todo: данное условие нужно обьединить с верхним и оптимизировать!
              if (dtNeedFinal in ManagedFlags) and
                 not (dtNeedAlwaysFinal in ManagedFlags) and
                 FUseARC and not Variable.IsTemporaryOwner then
              begin
                Expr := TIDExpression.Create(Variable, Proc.LastBodyLine);
                Code := TIL.IL_DecRef(Expr);
                TIL(Proc.IL).Write(Code);
              end;
            end;
          end;
        end;
      end;
    end;
    Variable := TIDVariable(Variable.NextItem);
  end;  *)
end;

procedure TNPUnit.CheckAndDelGenericTypes(var TypeSpace: TTypeSpace);
var
  TDecl: TIDType;
begin
  TDecl := TypeSpace.First;
  while Assigned(TDecl) do
  begin
    if Assigned(TDecl.GenericDescriptor) then
      TypeSpace.Delete(TDecl);
    TDecl := TIDType(TDecl.NextItem);
  end;
  TypeSpace.Reindex;
end;

procedure TNPUnit.CheckManagedInitFinalUnit;
{var
  Variable: TIDVariable;
  DataType: TIDType;
  FI, Code: TILInstruction;
  InitProcIL: TIL;
  FinalProcIL: TIL;}
begin
{  Variable := FVarSpace.First;

  InitProcIL := TIL(FInitProc.IL);
  FinalProcIL := TIL(FFinalProc.IL);

  FI := InitProcIL.First;
  while Assigned(Variable) do begin
    DataType := Variable.DataType;
    // проверка вызова контрукторов/деструкторов для структурых типов
    case DataType.DataTypeID of
      dtRecord: begin
        CheckAndCallRecordInit(FInitProc, Variable);
        CheckAndCallRecordFinal(FFinalProc, Variable, nil);
      end;
      dtStaticArray: begin
        CheckAndCallArrayInit(FInitProc, Variable);
        CheckAndCallArrayFinal(FFinalProc, Variable, nil);
      end;
      //dtDynArray: CheckAndCallArrayFinal(FFinalProc, Variable);
    end;
    // проверка вызова инициализации/финализации для простых типов
      if not (VarConst in Variable.Flags) then
      begin
        if not Assigned(Variable.DefaultValue) then
        begin
           if mtNeedClear in DataType.ManagedFlags then
           begin
             Code := TIL.IL_Init(TIDExpression.Create(Variable, FInitProc.FirstBodyLine));
             InitProcIL.InsertBefore(FI, Code);
           end;
        end;
        if dtNeedFinal in DataType.ManagedFlags then
        begin
          Code := TIL.IL_DecRef(TIDExpression.Create(Variable, FFinalProc.LastBodyLine));
          FinalProcIL.Write(Code);
        end;
      end;
    Variable := TIDVariable(Variable.NextItem);
  end; }
end;

{функция проверяет равна ли NIL-у managed переменная}
function CheckManagedNeedDecRef(const Instruction: TILDestInstruction): Boolean;
{var
  Code: TILInstruction;}
begin
  Exit(True);
  (*Code := Instruction.Prev;
  while Assigned(Code) do
  begin
    // если это инструкция icInit то к переменной нет надобности применять icDecRef
    if (Code.ILCode in [icInit, icMoveZero]) and (TILInit(Code).Destination.Declaration = Instruction.Destination.Declaration) then
      Exit(False);
    if (Code is TILDestInstruction) and Assigned(TILDestInstruction(Code).Destination) and
       (TILDestInstruction(Code).Destination.Declaration = Instruction.Destination.Declaration) then
      Exit(True);
    {if Assigned(Instruction.Destination.Instruction) and
       (TILInstruction(Instruction.Destination.Instruction).ILCode in [icGetPtr, icGetSelfPtr]) then
      Exit(True);}

    Code := Code.Prev;
  end;
  Result := True;
  *) // врет эта функция безбожно!
end;

procedure TNPUnit.CheckManagedDstSrc(Proc: TIDProcedure; Instruction: TILDestInstruction);
{var
  ILCode: TILCode;
  Dst: TIDExpression;
  Src, Src2, Tmp: TIDExpression;
  IL: TIL;}
begin
(*  IL := TIL(Proc.IL);
  ILCode := Instruction.ILCode;
  Dst := Instruction.Destination;

  if not Assigned(Dst) then
    Exit;

  if (Dst.AsVariable.Reference) and not Dst.IsAnonymous then
    Exit;

  case ILCode of
    //==============================================================
    icMove: begin
      Src := TILMove(Instruction).Source;
      if Src.ItemType = itVar then
      begin
        {проверка на вызов incref у источника}
        // делаем вызов incref если:
        // - Src - глобальная переменная
        // - Src - локальная переменная
        // - Src - ссылка
        if (mtNeedIncRef in Src.DataType.ManagedFlags) and not Src.IsAnonymousVar then
          IL.InsertBefore(Instruction, TIL.IL_IncRef(Src))
        else
        if (not Dst.IsAnonymousVar) and
           (not Src.AsVariable.IsTemporaryOwner) and
           (mtNeedIncRef in Src.DataType.ManagedFlags) then
        begin
          IL.InsertBefore(Instruction, TIL.IL_IncRef(Src));
        end;

        // вызываем функцию $copy у источника
        if Assigned(Src.DataType.CopyProc) and not Src.IsAnonymousVar then
        begin
          case Src.DataType.DataTypeID of
            dtRecord: CheckAndCallRecordCopy(Proc, Src.AsVariable, Instruction);
            dtStaticArray: CheckAndCallArrayCopy(Proc, Src.AsVariable, Instruction);
          end;
        end;
      end;

      {проверка на вызов финализатора у приемника}
      if (Dst.DataTypeID in [dtRecord]) and Assigned(Dst.DataType.FinalProc) and not Dst.IsAnonymousVar then
      begin
        CheckAndCallRecordFinal(Proc, Dst.AsVariable, Instruction);
      end else
      if (Dst.DataTypeID in [dtStaticArray]) and Assigned(Dst.DataType.FinalProc) and not Dst.IsAnonymousVar then
      begin
        CheckAndCallArrayFinal(Proc, Dst.AsVariable, Instruction);
      end;

      {проверка на вызов decref у приемника}
      if (mtNeedIncRef in Dst.DataType.ManagedFlags) and not Dst.IsAnonymousVar and
         CheckManagedNeedDecRef(Instruction) then
        IL.InsertBefore(Instruction, TIL.IL_DecRef(Dst));
    end;
    //==============================================================
    icMoveZero: if (mtNeedIncRef in Dst.DataType.ManagedFlags) {and not Dst.IsAnonymous} and
                   CheckManagedNeedDecRef(Instruction) then
                  IL.InsertBefore(Instruction, TIL.IL_DecRef(Dst));
    //==============================================================
    icProcCall: begin
      if Assigned(Dst) and
         (mtNeedIncRef in Dst.DataType.ManagedFlags) and not Dst.IsAnonymous and  // бля надо разобратся с анонимусами!!!
         CheckManagedNeedDecRef(Instruction) then
      begin
        // для строк не делается DecRef!!!
        //if not (Dst.DataType.DataTypeID in [dtString, dtAnsiString]) then почему???
          IL.InsertBefore(Instruction, TIL.IL_DecRef(Dst));
      end;
    end;
    //==============================================================
    icWeakRef: begin
      IL.InsertBefore(Instruction, TIL.IL_DecRef(Dst));
    end;
    //==============================================================
    icStrongRef: begin
      IL.InsertBefore(Instruction, TIL.IL_DecRef(Dst));
    end;
    //==============================================================
    icQueryType: begin
      if (mtNeedIncRef in Dst.DataType.ManagedFlags) and not Dst.IsAnonymousVar and
         CheckManagedNeedDecRef(Instruction) then
      begin
        IL.InsertBefore(Instruction, TIL.IL_DecRef(Dst));
        Src := TILMove(Instruction).Source;
        if (Src.ItemType = itVar) and (mtNeedIncRef in Src.DataType.ManagedFlags) and not Src.IsAnonymousVar then
          IL.InsertBefore(Instruction, TIL.IL_IncRef(Src));
      end;
    end;
    //==============================================================
    icAdd, icAdd2: begin
      Src := TILDstSrcSrcInstruction(Instruction).Left;
      Src2 := TILDstSrcSrcInstruction(Instruction).Right;
      if (mtNeedIncRef in Dst.DataType.ManagedFlags) and not Dst.IsAnonymous and CheckManagedNeedDecRef(Instruction) then
      begin
        if (Dst.Declaration <> Src.Declaration) and  (Dst.Declaration <> Src2.Declaration) then
          IL.InsertBefore(Instruction, TIL.IL_DecRef(Dst))
        else begin
          // в случае если слева от знака присвоить стоит тот же самый аргумент Str1 = Str1 + Str2
          // то инструкцию DECREF мы генерируем после инструкции сложения
          Tmp := TIDExpression.Create(Proc.GetTMPVar(Dst.DataType), Dst.TextPosition);
          IL.InsertBefore(Instruction, TIL.IL_Move(Tmp, Dst));
          IL.InsertAfter(Instruction, TIL.IL_DecRef(Tmp));
        end;
      end;
    end;
    //==============================================================
    icArrayDAlloc: begin
      if (mtNeedIncRef in Dst.DataType.ManagedFlags) and not Dst.IsAnonymous and CheckManagedNeedDecRef(Instruction) then
      begin
        IL.InsertBefore(Instruction, TIL.IL_DecRef(Dst));
      end;
    end;
  end;  *)
end;

procedure TNPUnit.CheckProc(Proc: TIDProcedure);
{var
  IL: TIL;
  PrevCode,
  Instruction: TILInstruction;
  CallInstruction: TILProcCall;
  CallExpr: TIDExpression;
  CallProc: TIDProcedure;}
begin
(*  IL := TIL(Proc.IL);

  {завершаем вложенные процедуры}
  CheckIncompletedProcs(Proc.ProcSpace);

  {если процедура не скомпилирована,- компилируем}
  if not Assigned(IL) then
  begin
    ParseGenericProcRepeatedly(Proc.Scope, Proc.GenericPrototype, Proc, Proc.Struct);
    Exit;
  end;

  {инициализация/финализация управляемых локальных переменных/параметров}
  CheckManagedInitFinal(Proc);

  Instruction := IL.First;
  while Assigned(Instruction) do
  begin
    if Instruction.ILCode = icProcCall then
    begin
      CallInstruction := TILProcCall(Instruction);
      CallExpr := CallInstruction.Proc;
      CallProc := CallExpr.AsProcedure;
      {если процедура не встроенна,- встраиваем}
      if (pfInline in CallProc.Flags) and (CallProc <> Proc) then
      begin
        Instruction := CallInstruction.Next;
        IL.InlineProc(Proc, CallProc, CallInstruction, CallInstruction.Instance, CallInstruction.Destination, CallInstruction.Arguments);
        IL.Delete(CallInstruction);
        continue;
      end;
    end;

    PrevCode := Instruction;
    Instruction := Instruction.Next;
    {проверка на необходимость генерации IncRef/DecRef для управляемых переменных}
    if FUseARC and (PrevCode is TILDestInstruction) then
      CheckManagedDstSrc(Proc, TILDestInstruction(PrevCode));
  end;

  {индексация инструкций}
  IL.Complete(FRCPathCount);

  {проверка переменных на инициализацию}
  IL.CheckVarsInitialized;

  {проверка на неиспользуемые переменные}
  CheckUnusedVariables(addr(Proc.VarSpace));

  {оптимизация: повторного использования временных переменных}
  if Options.OPT_REUSE_TMP_VARS then
    Opt_ReuseTMPVars(Proc);

  {оптимизация: удаления временных переменных}
  if Options.OPT_REDUCE_TMP_VARS then
    Opt_ReduceTMPVars(Proc);

  {оптимизация: удаления ненужных IncRef/DecRef}
  if Options.OPT_REDUCE_INCREF_DECREF then
    Opt_ReduceIncRefDecRef(Proc);

  if Assigned(Proc.FinalSection) then
    TIL(Proc.IL).CopyFrom(TIL(Proc.FinalSection));

  {завершающая инструкция RET (только в дебаг-режиме)}
  if Package.IncludeDebugInfo and not (TIL(Proc.IL).Last is TILRet) then
    TIL(Proc.IL).Write(TIL.IL_Ret(Proc.LastBodyLine, cNone));  *)
end;

class procedure TNPUnit.CheckPureExpression(SContext: PSContext; Expr: TIDExpression);
var
  IsVar: Boolean;
  Decl: TIDDeclaration;
begin
  if Assigned(SContext) and Assigned(SContext.Proc) and (pfPure in SContext.Proc.Flags) then
  begin
    Decl := Expr.Declaration;
    IsVar := False;
    case Decl.ItemType of
      itVar: IsVar := (Decl.Scope.ScopeType <> stLocal);
      itProcedure,
      itMacroFunction: IsVar := not (pfPure in TIDProcedure(Decl).Flags);
    end;
    if IsVar then
      ERROR_OBJECT_CANNOT_BE_USED_ON_PURE_PROC(Expr);
  end;
end;

class procedure TNPUnit.CheckIntConstInRange(const Expr: TIDExpression; HiBount, LowBound: Int64);
var
  V64: Int64;
begin
  V64 := Expr.AsIntConst.Value;
  if (V64 < LowBound) or (V64 > HiBount) then
    AbortWork('Eexpression must be in range [%d..%d]', [LowBound, HiBount], Expr.TextPosition);
end;

class procedure TNPUnit.CheckRecordType(Expression: TIDExpression);
var
  Decl: TIDDeclaration;
begin
  Decl := Expression.Declaration;
  if (Decl.ItemType <> itType) or
     (TIDType(Decl).DataTypeID <> dtRecord) then
    AbortWork(sRecordTypeRequired, Expression.TextPosition);
end;

class procedure TNPUnit.CheckReferenceType(Expression: TIDExpression);
begin
  if not Expression.DataType.IsReferenced then
    AbortWork(sReferenceTypeRequired, Expression.TextPosition);
end;

class procedure TNPUnit.CheckStringExpression(Expression: TIDExpression);
begin
  if MatchImplicit(Expression.DataType, SYSUnit._String) = nil then
    AbortWork(errStringExpressionRequired, Expression.TextPosition);
end;

class procedure TNPUnit.CheckStructType(Expression: TIDExpression);
var
  Decl: TIDDeclaration;
begin
  Decl := Expression.Declaration;
  if not (Decl is TIDStructure) then
    AbortWork(sStructTypeRequired, Expression.TextPosition);
end;

class procedure TNPUnit.CheckInterfaceType(Expression: TIDExpression);
var
  Decl: TIDDeclaration;
begin
  Decl := Expression.Declaration;
  if (Decl.ItemType <> itType) or
     (TIDType(Decl).DataTypeID <> dtInterface) then
    AbortWork(sInterfaceTypeRequired, Expression.TextPosition);
end;

class procedure TNPUnit.CheckClassOrIntfType(Expression: TIDExpression);
var
  Decl: TIDDeclaration;
begin
  Decl := Expression.Declaration;
  if (Decl.ItemType = itType) and
     ((TIDType(Decl).DataTypeID = dtClass) or
      (TIDType(Decl).DataTypeID = dtInterface)) then Exit;
  ERROR_CLASS_OR_INTF_TYPE_REQUIRED(Expression.TextPosition);
end;

class procedure TNPUnit.CheckClassOrIntfType(DataType: TIDType; const TextPosition: TTextPosition);
begin
  if (DataType.DataTypeID = dtClass) or
     (DataType.DataTypeID = dtInterface) then Exit;
  ERROR_CLASS_OR_INTF_TYPE_REQUIRED(TextPosition);
end;

class procedure TNPUnit.CheckClassType(Expression: TIDExpression);
var
  Decl: TIDDeclaration;
begin
  Decl := Expression.Declaration;
  if (Decl.ItemType <> itType) or
     (TIDType(Decl).DataTypeID <> dtClass) then
    ERROR_CLASS_TYPE_REQUIRED(Expression.TextPosition);
end;

class procedure TNPUnit.CheckClassExpression(Expression: TIDExpression);
begin
  if Expression.DataTypeID <> dtClass then
    ERROR_CLASS_TYPE_REQUIRED(Expression.TextPosition);
end;

procedure TNPUnit.CheckImplicitTypes(Src, Dst: TIDType; Position: TTextPosition);
begin
  if not Assigned(Src) then
    ERROR_EMPTY_EXPRESSION;
  if not Assigned(MatchImplicit(Src, Dst)) then
    AbortWork(sIncompatibleTypesFmt, [Src.DisplayName, Dst.DisplayName], Position);
end;

procedure TNPUnit.CheckIncompletedIntfProcs(ClassType: TIDClass);
var
  i: Integer;
  Intf: TIDInterface;
  IM, CM: TIDProcedure;
  Matched: Boolean;
begin
  for i := 0 to ClassType.InterfacesCount - 1 do
  begin
    Intf := ClassType.Interfaces[i];
    IM := Intf.Methods.First;
    while Assigned(IM) do
    begin
      CM := ClassType.FindMethod(IM.Name);
      if not Assigned(CM) then
        ERROR_INTF_METHOD_NOT_IMPLEMENTED(ClassType, IM);

      Matched := StrictMatchProc(IM, CM);
      if not Matched then
        ERROR_INTF_METHOD_NOT_IMPLEMENTED(ClassType, IM); // tmp !!!

      ClassType.MapInterfaceMethod(Intf, IM, CM);
      IM := TIDProcedure(IM.NextItem);
    end;
  end;
end;

procedure TNPUnit.CheckIncompletedProcs(ProcSpace: PProcSpace);
var
  Proc: TIDProcedure;
begin
  Proc := ProcSpace.First;
  while Assigned(Proc) do begin
    if (Proc.ImportName = 0) and (not Assigned(Proc.IL)) and
       not Assigned(Proc.GenericPrototype) then ERROR_INCOMPLETE_PROC(Proc);
    if Proc.ImportName = 0 then
      CheckProc(Proc);
    Proc := TIDProcedure(Proc.NextItem);
  end;
end;

procedure TNPUnit.CheckIncompleteFwdTypes;
var
  T: TIDType;
  DT: TIDDeclaration;
begin
  T := FTypeSpace.First;
  while Assigned(T) do
  begin
    if T.DataTypeID = dtPointer then
    begin
      DT := TIDPointer(T).ReferenceType; // запрос сделает инициализацию ReferenceType
      if Assigned(DT) then;
      // Assert(Assigned(DT));
    end;
    T := TIDType(T.NextItem);
  end;
end;

class procedure TNPUnit.CheckIncompleteType(Fields: TScope);
var
  Field: TIDDeclaration;
  FieldType: TIDType;
begin
  Field := Fields.VarSpace.First;
  while Assigned(Field) do begin
    FieldType := Field.DataType;
    if (Field.DataTypeID = dtRecord) and
       (not(StructCompleted in TIDStructure(FieldType).StructFlags)) then
      AbortWork(sRecurciveTypeLinkIsNotAllowed, Field.ID.TextPosition);
    Field := Field.NextItem;
  end;
end;

procedure TNPUnit.CheckInitVariables(SContext: PSContext; ParentDecl: TIDMultiExpression; VarSpace: PVarSpace);
begin
  Assert(False);
end;

procedure TNPUnit.CheckUnusedVariables(VarSpace: PVarSpace);
//var
//  Prev, Curr: TIDVariable;
begin
  Assert(False);
//  if not Options.OPT_ELEMINATE_UNUSED_LOCAL_VARS then
//    Exit;
//  Curr := VarSpace.First;
//  VarSpace.Initialize;
//  while Assigned(Curr) do begin
//    {если переменная/параметр не используется}
//    if (Curr.RefCount = 0) and (Curr.Scope.ScopeClass = scProc) then
//    begin
//      {если переменная не глобальная, не временная или не скрытый параметр}
//      if (not Curr.IsAnonymous) and not (VarHiddenParam in Curr.Flags) then
//        Hint(msgVariableIsDeclaredButNeverUsedInFmt, [Curr.ID.Name], Curr.ID.TextPosition);
//      {если это не параметр, удалям его}
//      if not (VarParameter in Curr.Flags) then
//      begin
//        Curr := TIDVariable(Curr.NextItem);
//        Continue;
//      end;
//    end;
//    VarSpace.Add(Curr);
//    // увеличиваем ссылку на тип для глобальных переменных (пока так!)
//    if (Curr.Scope = FIntfScope) or (Curr.Scope = FImplScope) then
//      Curr.DataType.IncRefCount(1);
//    Prev := Curr;
//    Curr := TIDVariable(Curr.NextItem);
//    Prev.NextItem := nil;
//  end;
//  VarSpace.Reindex;
end;

function TNPUnit.CheckUsed: Boolean;
begin
  if FVarSpace.Count > 0 then
    Exit(True);

  if FProcSpace.Count > 0 then
    Exit(True);

  Result := True;
end;

function TNPUnit.CheckAndDelUnusedProcs(var ProcSpace: TProcSpace): Integer;
var
  Proc: TIDProcedure;
begin
  Result := 0;
  Proc := ProcSpace.First;
  while Assigned(Proc) do
  begin
    if (Proc.ImportLib > 0) then begin
      Proc.IncTypesReadCountInSignature(FRCPathCount);
      Inc(FRCPathCount);
    end else
    // удаляем приватные не импортируемые процедуры которые никогда не использовали
    if (Proc.Scope = FImplScope) and (Proc.RefCount = 0) then
    begin
      Inc(Result);
      // удаляем зависимости в IL
      Proc.RemoveILReferences(FRCPathCount);
      // удаляем из списка процедур
      ProcSpace.Delete(Proc);
      HINT_PROC_DELETE_UNUSED(Proc);
    end;
    Proc := TIDProcedure(Proc.NextItem);
  end;
  ProcSpace.Reindex;
end;

function TNPUnit.CheckUnusedRecordType(Struct: TIDStructure): Boolean;
var
  Method: TIDProcedure;
begin
  Method := Struct.Methods.First;
  while Assigned(Method) do
  begin
    if Method.Export > 0 then
      Exit(False);
    Method := TIDProcedure(Method.NextItem);
  end;

  if Struct.RefCount = 0 then
    Exit(True);
  Result := False;
end;

procedure TNPUnit.HINT_TYPE_DELETE_UNUSED(Decl: TIDDeclaration);
begin
  //if not Decl.IsAnonymous then
    Hint('TYPE ' + Decl.DisplayName + ' was eliminated as not used', [], Decl.TextPosition);
end;

procedure TNPUnit.HINT_PROC_DELETE_UNUSED(Decl: TIDDeclaration);
begin
  //if not Decl.IsAnonymous then
  Hint('PROCEDURE ' + Decl.DisplayName + ' was eliminated as not used', [], Decl.TextPosition);
end;

function TNPUnit.CheckAndDelUnusedTypes(var TypeSpace: TTypeSpace): Integer;
var
  TDecl: TIDType;
begin
  Result := 0;
  TDecl := TypeSpace.First;
  while Assigned(TDecl) do
  begin
    if TDecl.ImportLib > 0 then
      TDecl.IncRefCount(1)
    else
    if Assigned(TDecl.GenericDescriptor) then
       TypeSpace.Delete(TDecl)
    else
    // удаляем простые пользовательские типы по условию (ReadCount = 0)
    case TDecl.DataTypeID of
      {структурные типы}
      dtRecord, dtClass: begin
        if CheckUnusedRecordType(TIDStructure(TDecl)) then
        begin
          inc(Result);
          TypeSpace.Delete(TDecl);
          HINT_TYPE_DELETE_UNUSED(TDecl);
        end;
      end;
      {указатели}
      dtPointer: begin
        if (TDecl.RefCount = 0) and (TDecl <> SYSUnit._Pointer) then
        begin
          inc(Result);
          TypeSpace.Delete(TDecl);
          HINT_TYPE_DELETE_UNUSED(TDecl);
        end;
      end;
      {остальные типы}
      dtStaticArray, dtDynArray, dtRange, dtEnum, dtSet, dtOpenArray, dtProcType, dtClassOf:
      begin
        if {(TDecl.Scope <> FIntfScope) and} (TDecl.RefCount = 0) then
        begin
          inc(Result);
          TypeSpace.Delete(TDecl);
          HINT_TYPE_DELETE_UNUSED(TDecl);
        end;
      end;
    end;
    TDecl := TIDType(TDecl.NextItem);
  end;
  TypeSpace.Reindex;
end;

class procedure TNPUnit.CheckVarExpression(Expression: TIDExpression; VarModifyPlace: TVarModifyPlace);
var
  Flags: TVariableFlags;
  Decl: TIDDeclaration;
begin
  if Expression.ExpressionType = etDeclaration then
    Decl := Expression.Declaration
  else
    Decl := TIDMultiExpression(Expression).Items[0].Declaration;  // пока так
  // сдесь необходимо отдельно обрабатывать случаи:
  // 1. доступа к членам класса/структуры
  // 2. массива
  // 3. индексного свойства

  if Decl.ItemType <> itVar then
  case VarModifyPlace of
    vmpAssignment: ERROR_VAR_EXPRESSION_REQUIRED(Expression);
    vmpPassArgument: ERROR_ARG_VAR_REQUIRED(Expression);
  end;
  Flags := TIDVariable(Decl).Flags;
  if VarConst in Flags then begin
    ERROR_CANNOT_MODIFY_CONSTANT(Expression);
  end;
  if VarLoopIndex in Flags then
    ERROR_CANNOT_MODIFY_FOR_LOOP_VARIABLE(Expression);
end;

class procedure TNPUnit.CFBBegin(SContext: PSContext; CFBlockType: TCFBlockType);
begin
  if Assigned(SContext) and SContext.WriteIL then
    SContext.IL.CFBBegin(CFBlockType);
end;

class procedure TNPUnit.CFBEnd(SContext: PSContext; CFBlockType: TCFBlockType);
begin
  if Assigned(SContext) and SContext.WriteIL then
    SContext.IL.CFBEnd(CFBlockType);
end;

class procedure TNPUnit.CheckAccessMember(SContext: PSContext; Decl: TIDDeclaration; const ID: TIdentifier);
begin
  case Decl.Visibility of
    vLocal, vPublic: Exit;
    vProtected: begin
      if Decl.DeclUnit = SContext.Proc.DeclUnit then
        Exit;
      if not SContext.Proc.Struct.IsInheritsForm(TStructScope(Decl.Scope).Struct) then
        ERROR_CANNOT_ACCESS_PRIVATE_MEMBER(ID);
    end;
    vStrictProtected: begin
      if not SContext.Proc.Struct.IsInheritsForm(TStructScope(Decl.Scope).Struct) then
        ERROR_CANNOT_ACCESS_PRIVATE_MEMBER(ID);
    end;
    vPrivate: begin
      if Decl.DeclUnit = SContext.Proc.DeclUnit then
        Exit;
      if TStructScope(Decl.Scope).Struct <> SContext.Proc.Struct then
        ERROR_CANNOT_ACCESS_PRIVATE_MEMBER(ID);
    end;
    vStrictPrivate: begin
      if TStructScope(Decl.Scope).Struct <> SContext.Proc.Struct then
        ERROR_CANNOT_ACCESS_PRIVATE_MEMBER(ID);
    end;
  end;
end;

procedure TNPUnit.CheckArrayExpression(Expression: TIDExpression);
begin
  if not (Expression.DataType is TIDArray) then
    ERROR_ARRAY_EXPRESSION_REQUIRED(Expression);
end;

class procedure TNPUnit.CheckBooleanExpression(Expression: TIDExpression);
begin
  if Expression.DataType <> SYSUnit._Boolean then
    ERROR_BOOLEAN_EXPRESSION_REQUIRED(Expression);
end;

function TNPUnit.Compile(RunPostCompile: Boolean = True): TCompilerResult;
begin

end;

function TNPUnit.CompileMethodDecl(Struct: TIDStructure; const Source: string; out Proc: TIDProcedure): ICompilerMessages;
begin

end;

function TNPUnit.CompileIntfOnly: TCompilerResult;
var
  Token: TTokenID;
  Scope: TScope;
  Platform: TIDPlatform;
begin
  Result := CompileFail;
  FMessages.Clear;
  FRCPathCount := 1;
  try
    FParser.First;
    Scope := FIntfScope;
    ParseUnitDecl(Scope);
    Token := parser_NextToken(Scope);
    while true do begin
      case Token of
        token_type: begin
          CheckIntfSectionMissing(Scope);
          Token := ParseNamedTypeDecl(Scope);
        end;
        token_asm: begin
          CheckIntfSectionMissing(Scope);
          Token := ParseAsmSpecifier(Platform);
          case Token of
            token_function: Token := ParseProcedure(Scope, ptFunc);
            token_procedure: Token := ParseProcedure(Scope, ptProc);
            else
              ERROR_FEATURE_NOT_SUPPORTED;
          end;
        end;
        token_uses: Token := ParseUsesSection(Scope);
        token_function: begin
          CheckIntfSectionMissing(Scope);
          Token := ParseProcedure(Scope, ptFunc);
        end;
        token_procedure: begin
          CheckIntfSectionMissing(Scope);
          Token := ParseProcedure(Scope, ptProc);
        end;
        //token_namespace: Token := ParseNameSpace(Scope);
        token_constructor: begin
          CheckIntfSectionMissing(Scope);
          Token := ParseProcedure(Scope, ptConstructor);
        end;
        token_destructor: begin
          CheckIntfSectionMissing(Scope);
          Token := ParseProcedure(Scope, ptDestructor);
        end;
        token_operator: begin
          CheckIntfSectionMissing(Scope);
          Token := ParseOperator(Scope, nil);
        end;
        token_const: begin
          CheckIntfSectionMissing(Scope);
          Token := ParseConstSection(Scope);
        end;
        token_class: begin
          CheckIntfSectionMissing(Scope);
          Token := parser_NextToken(Scope);
          case Token of
            token_function: Token := ParseProcedure(Scope, ptClassFunc);
            token_procedure: Token := ParseProcedure(Scope, ptClassProc);
            token_constructor: Token := ParseProcedure(Scope, ptClassConstructor);
            token_destructor: Token := ParseProcedure(Scope, ptClassDestructor);
          else
            ERROR_FEATURE_NOT_SUPPORTED;
          end;
        end;
        token_weak: begin
          CheckIntfSectionMissing(Scope);
          parser_NextToken(Scope);
          Token := ParseVarSection(Scope, vLocal, nil, True);
        end;
        token_var: begin
          CheckIntfSectionMissing(Scope);
          parser_NextToken(Scope);
          Token := ParseVarSection(Scope, vLocal, nil, False);
        end;
//        token_ref: begin
//          CheckIntfSectionMissing(Scope);
//          parser_NextToken(Scope);
//          Token := ParseVarSection(Scope, vLocal, nil, False, True);
//        end;
        token_interface: begin
          Scope := FIntfScope;
          Token := parser_NextToken(Scope);
        end;
        token_implementation: begin
          // stop parsing
          Break;
        end;
        token_end: begin
          parser_MatchToken(parser_NextToken(Scope), token_dot);
          Token := parser_NextToken(Scope);
          if Token <> token_eof then
            HINT_TEXT_AFTER_END;
          Break;
        end;
        //token_cond_macro: Token := ParseCondMacro(Scope);
        token_initialization: Token := ParseInitSection;
        token_finalization: Token := ParseFinalSection;
        token_eof: Break;
      else
        if Token >= token_cond_define then
        begin
          Token := ParseCondStatements(Scope, Token);
          continue;
        end;
        ERROR_KEYWORD_EXPECTED;
      end;
    end;
    Result := CompileSuccess;
    FCompiled := True;
  except
    on e: ECompilerStop do Exit();
    on e: ECompilerSkip do Exit(CompileSkip);
    on e: ECompilerAbort do PutMessage(ECompilerAbort(e).CompilerMessage^);
    on e: Exception do PutMessage(cmtInteranlError, e.Message);
  end;
end;

constructor TNPUnit.Create(const Project: IASTProject; const FileName: string; const Source: string = '');
var
  Scope: TScope;
begin
  inherited Create(Project, FileName, Source);
  FDefines := TDefines.Create();
  FPackage := Project as INPPackage;
  FParser := TDelphiLexer.Create(Source);
  FMessages := TCompilerMessages.Create;
  //FVisibility := vPublic;
  FIntfScope := TScope.Create(stGlobal, @FVarSpace, @FProcSpace, nil, Self);
  {$IFDEF DEBUG}FIntfScope.Name := 'unit_intf_scope';{$ENDIF}
  FImplScope := TImplementationScope.Create(FIntfScope, nil);
  {$IFDEF DEBUG}FImplScope.Name := 'unit_impl_scope';{$ENDIF}
  FIntfImportedUnits := TUnitList.Create;
  FImplImportedUnits := TUnitList.Create;
  FBENodesPool := TBENodesPool.Create(16);
  FLoopPool := TLoopPool.Create(4);
  FTMPVars := TItemsStack.Create(8);
  FUseARC := True;
  if Assigned(SYSUnit) then
  begin
    FTypeSpace.Initialize(SYSUnit.SystemTypesCount);
    // добовляем system в uses
    FIntfImportedUnits.AddObject('system', SYSUnit);
  end;
  FOptions := TCompilerOptions.Create(Package.Options);

//  Scope := TProcScope.CreateInBody(FImplScope);
//  FInitProc := TIDProcedure.CreateAsSystem(Scope, '$initialization');
//  FInitProc.EntryScope := Scope;
//  FInitProc.IL := TIL.Create(FInitProc);
//  fInitProcSConect.Proc := FInitProc;
//  fInitProcSConect.IL := TIL(FInitProc.IL);
//  fInitProcSConect.WriteIL := True;
//
//  Scope := TProcScope.CreateInBody(FImplScope);
//  FFinalProc := TIDProcedure.CreateAsSystem(Scope, '$finalization');
//  FFinalProc.EntryScope := Scope;
//  FFinalProc.IL := TIL.Create(FFinalProc);
//  fFinalProcSConect.Proc := FInitProc;
//  fFinalProcSConect.IL := TIL(FInitProc.IL);
//  fFinalProcSConect.WriteIL := True;

  FBreakPoints := TBreakPoints.Create;
  fCondStack := TSimpleStack<Boolean>.Create(0);
  fCondStack.OnPopError := procedure begin ERROR_INVALID_COND_DIRECTIVE() end;
end;

function TNPUnit.CreateAnonymousConstant(Scope: TScope; var EContext: TEContext; const ID: TIdentifier;
                                         IdentifierType: TIdentifierType): TIDExpression;
var
  i: Integer;
  IntValue: Int64;
  Int32Value: Int32;
  FltValue: Double;
  DataType: TIDType;
  Value: string;
  CItem: TIDConstant;
  Chars: TStringDynArray;
begin
  Value := ID.Name;
  case IdentifierType of
    itChar: CItem := TIDCharConstant.CreateAnonymous(Scope, SYSUnit._Char, Value[1]);
    itString: begin
        // если чарсет метаданных равен ASCII, все строковые константы
        // удовлетворающе набору ASCII, создаются по умолчанию с типом AnsiString
        if (FPackage.RTTICharset = RTTICharsetASCII) and IsAnsiString(Value) then
          DataType := SYSUnit._AnsiString
        else
          DataType := SYSUnit._String;
      CItem := TIDStringConstant.CreateAnonymous(Scope, DataType, Value);
      CItem.Index := FPackage.GetStringConstant(TIDStringConstant(CItem));
    end;
    itInteger: begin
      if Value[1] = '#' then begin
        Value := Copy(Value, 2, Length(Value) - 1);
        if TryStrToInt(Value, Int32Value) then
          CItem := TIDCharConstant.CreateAnonymous(Scope, SYSUnit._Char, Char(Int32Value))
        else
          AbortWorkInternal('int convert error', parser_Position);
      end else begin
        if not TryStrToInt64(Value, IntValue) then
        begin
          if (EContext.RPNLastOperator = TOperatorID.opNegative) then
          begin
            // хак для обработки MinInt64 !!!
            if TryStrToInt64('-' + Value, IntValue) then
              EContext.RPNEraiseTopOperator
            else
              AbortWork('Invalid decimal value: %s', [Value], parser_Position);
          end else
            AbortWork('Invalid decimal value: %s', [Value], parser_Position);
        end;
        DataType := SYSUnit.DataTypes[GetValueDataType(IntValue)];
        CItem := TIDIntConstant.CreateAnonymous(Scope, DataType, IntValue);
      end;
    end;
    itFloat: begin
      FltValue := StrToFloat(Value);
      DataType := SYSUnit.DataTypes[GetValueDataType(FltValue)];
      CItem := TIDFloatConstant.CreateAnonymous(Scope, DataType, FltValue);
    end;
    itHextNumber: begin
      try
        IntValue := HexToInt64(Value);
      except
        ERROR_INVALID_HEX_CONSTANT;
        IntValue := 0;
      end;
      DataType := SYSUnit.DataTypes[GetValueDataType(IntValue)];
      CItem := TIDIntConstant.CreateAnonymous(Scope, DataType, IntValue);
    end;
    itBinNumber: begin
      try
        IntValue := BinStringToInt64(Value);
      except
        ERROR_INVALID_BIN_CONSTANT;
        IntValue := 0;
      end;
      DataType := SYSUnit.DataTypes[GetValueDataType(IntValue)];
      CItem := TIDIntConstant.CreateAnonymous(Scope, DataType, IntValue);
    end;
    itCharCodes: begin
      Chars := SplitString(ID.Name, '#');
      if Chars[0] = '' then
        Delete(Chars, 0, 1);
      // this is a string
      if Length(Chars) > 1 then
      begin
        SetLength(Value, Length(Chars));
        for i := 0 to Length(Chars) - 1 do
          Value[Low(string) + i] := Char(StrToInt(Chars[i]));

        // если чарсет метаданных равен ASCII, все строковые константы
        // удовлетворающе набору ASCII, создаются по умолчанию с типом AnsiString
        if (FPackage.RTTICharset = RTTICharsetASCII) and IsAnsiString(Value) then
          DataType := SYSUnit._AnsiString
        else
          DataType := SYSUnit._String;

        CItem := TIDStringConstant.CreateAnonymous(Scope, DataType, Value);
        CItem.Index := FPackage.GetStringConstant(TIDStringConstant(CItem));
      end else
      // this is a char
        CItem := TIDCharConstant.CreateAnonymous(Scope, SYSUnit._Char, Char(StrToInt(Chars[0])));
    end;
  else
    ERROR_INTERNAL();
    CItem := nil;
  end;
  Result := TIDExpression.Create(CItem, ID.TextPosition);
end;

function TNPUnit.CreateAnonymousConstTuple(Scope: TScope; ElementDataType: TIDType): TIDExpression;
var
  Decl: TIDDynArrayConstant;
  AType: TIDDynArray;
begin
  AType := TIDDynArray.CreateAsAnonymous(Scope);
  AType.ElementDataType := ElementDataType;
  Decl := TIDDynArrayConstant.CreateAnonymous(Scope, AType, nil);
  Result := TIDExpression.Create(Decl);
end;

class function TNPUnit.CreateRangeType(Scope: TScope;  LoBound, HiBound: Integer): TIDRangeType;
begin
  Result := TIDRangeType.CreateAsAnonymous(Scope);
  Result.LowBound := LoBound;
  Result.HighBound := HiBound;
end;

class function TNPUnit.CreateStructInitProc(const Struct: TIDStructure): TIDProcedure;
begin
  Result := TIDProcedure.CreateAsSystemMethod(Struct, '$init');
  Result.Struct := Struct;
  Result.IL := TIL.Create(Result);
  Struct.InitProc := Result;
  Struct.AddMethod(Result);
end;

class function TNPUnit.CreateStructCopyProc(const Struct: TIDStructure): TIDProcedure;
begin
  Result := TIDProcedure.CreateAsSystemMethod(Struct, '$copy');
  Result.Struct := Struct;
  Result.IL := TIL.Create(Result);
  Struct.CopyProc := Result;
  Struct.AddMethod(Result);
end;

function TNPUnit.CreateStructFinalProc(const Struct: TIDStructure): TIDProcedure;
begin
  Result := TIDProcedure.CreateAsSystemMethod(Struct, '$final');
  Result.Struct := Struct;
  Result.IL := TIL.Create(Result);
  Struct.FinalProc := Result;
  Struct.AddMethod(Result);
end;

function TNPUnit.CreateSysProc(const SysName: string): TIDProcedure;
begin
  Result := TIDProcedure.CreateAsSystem(FImplScope, SysName);
  Result.IL := TIL.Create(Result);
  FProcSpace.Add(Result);
end;

function TNPUnit.CreateArraySysProc(const Arr: TIDArray; const Name: string; out ProcParam: TIDVariable): TIDProcedure;
begin
  Result := TIDProcedure.CreateAsSystem(FImplScope, Name);
  Result.IL := TIL.Create(Result);
  ProcParam := TIDVariable.CreateAsSystem(Result.ParamsScope, 'Arr');
  ProcParam.DataType := Arr;
  {статические массивы передает по ссылке}
  if Arr.DataTypeID = dtStaticArray then
    ProcParam.Flags := [VarInOut, VarParameter]
  else
    ProcParam.Flags := [VarIn, VarConst, VarParameter];
  Result.AddParam(ProcParam);
  FProcSpace.Add(Result);
end;

function TNPUnit.CheckConstDynArray(SContext: PSContext; Destination: TIDType; ConstArray: TIDExpression): TIDExpression;
//var
//  i, Cnt: Integer;
//  CArr: TIDDynArrayConstant;
//  ItemExpr: TIDExpression;
//  TMPVar: TIDVariable;
//  DItems: TIDExpressions;
//  AType: TIDArray;
//  ARange: TIDRangeType;
begin
  Exit(ConstArray); // todo: неизвестно зачем написана эта функция! Вспомнить!!!!!
//
//  CArr := TIDDynArrayConstant(ConstArray.Declaration);
//  if CArr.ArrayStatic then
//    Exit(ConstArray);
//
//  Cnt := CArr.ArrayLength;
//  case Destination.DataTypeID of
//    dtDynArray: begin
//      {создаем динамический массив}
//      TMPVar := GetTMPVar(SContext, ConstArray.DataType);
//      TMPVar.Flags := [VarConst];
//      Result := TIDExpression.Create(TMPVar, ConstArray.TextPosition);
//      ILWrite(SContext, TIL.IL_DAlloc(Result, IntConstExpression(Cnt)));
//    end;
//    dtOpenArray: begin
//      {создаем статический массив}
//      AType := TIDArray.CreateAsAnonymous(FImplScope);
//      AType.ElementDataType := TIDArray(ConstArray.DataType).ElementDataType;
//      ARange := CreateRangeType(FImplScope, 0, Cnt - 1);
//      AddType(ARange);
//      AType.AddBound(ARange);
//      AddType(AType);
//      TMPVar := GetTMPVar(SContext, AType);
//      TMPVar.Flags := [VarConst];
//      Result := TIDExpression.Create(TMPVar, ConstArray.TextPosition);
//    end;
//  else
//    ERROR_FEATURE_NOT_SUPPORTED;
//    Result := nil;
//  end;
//  {генерируем код заполнения массива}
//  for i := 0 to Cnt - 1 do begin
//    SetLength(DItems, 2); // каждую итерацю необходима новая копия массива
//    DItems[0] := Result;
//    DItems[1] := IntConstExpression(i);
//    ItemExpr := CArr.Value[i];
//    ItemExpr := MatchImplicit3(SContext, ItemExpr, TIDArray(Result.DataType).ElementDataType);
//    ILWrite(SContext, TIL.IL_Move(TIDMultiExpression.Create(DItems, FParser.Position), ItemExpr));
//  end;
end;

function TNPUnit.Defined(const Name: string): Boolean;
begin
  // поиск в локальном списке модуля
  Result := FDefines.IndexOf(Name) > -1;
  // поиск в пакете
  if not Result then
    Result := FPackage.Defines.IndexOf(Name) > -1;
end;

destructor TNPUnit.Destroy;
begin
  FDefines.Free;
  FIntfScope.Free;
  FImplScope.Free;
  FParser.Free;
  FOptions.Free;
  FIntfImportedUnits.Free;
  FImplImportedUnits.Free;
  FBreakPoints.Free;
  inherited;
end;

function TNPUnit.ParseParameters(Scope: TScope; InMacro: Boolean = False): TTokenID;
type
  PIDItem = ^TIDItem;
  TIDItem = record
    ID: TIdentifier;
    Param: TIDVariable;
    NextItem: PIDItem;
  end;
var
  i: Integer;
  DataType: TIDType;
  Param: TIDVariable;
  VarFlags: TVariableFlags;
  DefaultExpr: TIDExpression;
  IDItem: TIDItem;
  CNItem: PIDItem;
  NextItem: PIDItem;
begin
  CNItem := addr(IDItem);
  CNItem.Param := nil;
  CNItem.NextItem := nil;
  while True do begin
    Result := parser_NextToken(Scope);
    if (Result = token_closeround) or
       (Result = token_closeblock) then
     Exit;
    VarFlags := [VarParameter];
    case Result of
      token_const: begin
        Include(VarFlags, VarConst);
        Result := parser_NextToken(Scope);
      end;
      token_constref: begin
        Include(VarFlags, VarConstRef);
        Result := parser_NextToken(Scope);
      end;
      token_var: begin
        Include(VarFlags, VarInOut);
        Result := parser_NextToken(Scope);
      end;
      token_out: begin
        Include(VarFlags, VarOut);
        Result := parser_NextToken(Scope);
      end;
    end;
    parser_MatchIdentifier(Result);
    while True do begin
      parser_ReadCurrIdentifier(CNItem.ID); // read name
      Result := parser_NextToken(Scope);
      if Result = token_coma then begin
        Result := parser_NextToken(Scope);
        parser_MatchParamNameIdentifier(Result);
        New(NextItem);
        CNItem.NextItem := NextItem;
        CNItem := NextItem;
        CNItem.Param := nil;
        CNItem.NextItem := nil;
        Continue;
      end;
      if Result = token_colon then
      begin
        Result := ParseTypeSpec(Scope, DataType);
        // парсим значение по умолчанию
        if Result = token_equal then
        begin
          parser_NextToken(Scope);
          Result := ParseConstExpression(Scope, DefaultExpr, ExprNested)
        end else
          DefaultExpr := nil;
      end else begin

        // если тип не указан, то это нетепизированная ссылка
        if (VarConst in VarFlags) or
           (VarInOut in VarFlags) or
           (VarOut in VarFlags) or
           (VarConstRef in VarFlags) then
        begin
          DataType := SYSUnit._UntypedReference;
        end else
        if InMacro then
          DataType := TIDGenericType.Create(Scope, Identifier('T'))
        else
          ERROR_PARAM_TYPE_REQUIRED;
      end;

      NextItem := addr(IDItem);
      while Assigned(NextItem) do begin
        if not Assigned(NextItem.Param) then
        begin
          Param := TIDVariable.Create(Scope, NextItem.ID, DataType, VarFlags);
          Param.DefaultValue := DefaultExpr;
          NextItem.Param := Param;
        end;
        NextItem := NextItem.NextItem;
      end;
      Break;
    end;
    if Result = token_semicolon then
    begin
      New(NextItem);
      CNItem.NextItem := NextItem;
      CNItem := NextItem;
      CNItem.Param := nil;
      CNItem.NextItem := nil;
      continue;
    end;
    
    // insert params to proc scope
    NextItem := addr(IDItem);
    while Assigned(NextItem) do
    begin
      Param := NextItem.Param;
      Scope.AddVariable(Param);
      {если параметр - открытй массив, то добавляем скрытый параметр "Длинна массива"}
      if Param.DataType.DataTypeID = dtOpenArray then
      begin
        Param := TIDVariable.CreateAsSystem(Scope, NextItem.ID.Name + '$Length');
        Param.DataType := SYSUnit._UInt32;
        Param.Flags := [VarParameter, VarHiddenParam, VarConst];
        Scope.AddVariable(Param);
      end;
      NextItem := NextItem.NextItem;
    end;
    // disposing items
    CNItem := addr(IDItem);
    CNItem := CNItem.NextItem;
    while Assigned(CNItem) do
    begin
      NextItem := CNItem;
      CNItem := CNItem.NextItem;
      Dispose(NextItem);
    end;

    Exit;
  end;
end;

procedure TNPUnit.StaticCheckBounds(ConstValue: TIDConstant; Decl: TIDDeclaration; DimNumber: Integer);
var
  DeclType: TIDType;
  Dim: TIDOrdinal;
begin
  if Decl.ItemType <> itProperty then
  begin
    DeclType := Decl.DataType;
    if DeclType.DataTypeID = dtPointer then
      DeclType := TIDPointer(DeclType).ReferenceType;

    with TIDArray(DeclType) do
    begin
      if DimNumber >= DimensionsCount then
        AbortWork(sNeedSpecifyNIndexesFmt, [Decl.DisplayName, DisplayName, DimensionsCount], FParser.PrevPosition);

      Dim := Dimensions[DimNumber];
      if (ConstValue.AsInt64 < Dim.LowBound) or (ConstValue.AsInt64 > Dim.HighBound) then
        AbortWork(sConstExprOutOfRangeFmt, [ConstValue.AsInt64, Dim.LowBound, Dim.HighBound], FParser.PrevPosition);
    end;
  end;
end;

function TNPUnit.UsedUnit(const UnitName: string): Boolean;
var
  i: Integer;
begin
  i := FIntfImportedUnits.IndexOf(UnitName);
  if i > 0 then
    Exit(True);

  i := FImplImportedUnits.IndexOf(UnitName);
  if i > 0 then
    Exit(True)
  else
    Exit(False);
end;

function TNPUnit.ParseArrayMember(var PMContext: TPMContext; Scope: TScope; Decl: TIDDeclaration; out DataType: TIDType; var EContext: TEContext): TTokenID;
var
  IdxCount: Integer; // кол-во индексов указанных при обращении к массиву
  Expr: TIDExpression;
  InnerEContext: TEContext;
  DimensionsCount: Integer;
  DeclType: TIDType;
begin
  DeclType := Decl.DataType;
  if DeclType.DataTypeID = dtPointer then
    DeclType := TIDPointer(DeclType).ReferenceType;

  if (Decl.ItemType <> itProperty) and (DeclType is TIDArray) then
  begin
    DimensionsCount := TIDArray(DeclType).DimensionsCount;
    DataType := TIDArray(DeclType).ElementDataType;
  end else
  if (Decl.ItemType = itProperty) and (TIDProperty(Decl).Params.Count > 0) then
  begin
    DimensionsCount := TIDProperty(Decl).ParamsCount;
    DataType := TIDProperty(Decl).DataType;
  end else
  if (DeclType is TIDStructure) and Assigned(TIDStructure(DeclType).DefaultProperty) then
  begin
    Decl := TIDStructure(DeclType).DefaultProperty;
    DimensionsCount := TIDProperty(Decl).ParamsCount;
    DataType := TIDProperty(Decl).DataType;
    Expr := TIDExpression.Create(Decl);
    if EContext.EPosition = ExprRValue then
    begin
      Result := ParsePropertyMember(PMContext, Scope, TIDProperty(Decl), Expr, EContext);
      DataType := nil;
      Exit;
    end;
    PMContext.Add(Expr);
  end else
  begin
    ERROR_ARRAY_TYPE_REQUIRED(PMContext.ID);
    DimensionsCount := 0;
  end;

  IdxCount := 0;
  InitEContext(InnerEContext, EContext.SContext, ExprNested);
  while True do begin
    Result := ParseExpression(Scope, InnerEContext, parser_NextToken(Scope));
    Expr := InnerEContext.Result;
    CheckEmptyExpression(Expr);
    if Assigned(InnerEContext.LastBoolNode) then
      Bool_CompleteImmediateExpression(InnerEContext, Expr);

    if Expr.Declaration.ItemType = itConst then
      // статическая проверка на границы массива
      StaticCheckBounds(Expr.AsConst, Decl, IdxCount)
    else begin
       // динамическая проверка на границы массива
       if FUseCheckBound then
         EmitDynCheckBound(EContext.SContext, Decl, Expr);
    end;

    PMContext.Add(Expr);
    Inc(IdxCount);
    if Result = token_coma then
    begin
      InnerEContext.Reset;
      Continue;
    end;
    parser_MatchToken(Result, token_closeblock);
    Result := parser_NextToken(Scope);
    Break;
  end;
  if IdxCount <> DimensionsCount then
    ERROR_NEED_SPECIFY_NINDEXES(Decl);
end;

function TNPUnit.ParsePropertyMember(var PMContext: TPMContext; Scope: TScope; Prop: TIDProperty; out Expression: TIDExpression; var EContext: TEContext): TTokenID;
//var
//  CallExpr: TIDCallExpression;
//  SelfExpr: TIDExpression;
//  Accessor: TIDDeclaration;
//  ArgumentsCount: Integer;
begin
  Assert(False);
//  Result := parser_CurTokenID;
//  Expression := TIDExpression.Create(Prop, PMContext.ID.TextPosition);
//  if EContext.EPosition = ExprLValue then begin
//    Accessor := TIDProperty(Prop).Setter;
//    if not Assigned(Accessor) then
//      ERROR_CANNOT_MODIFY_READONLY_PROPERTY(Expression);
//    // если сеттер - процедура, отодвигаем генерацию вызова на момент оброботки опрератора присвоения
//    if Accessor.ItemType = itProcedure then
//    begin
//      Exit;
//    end;
//  end else begin
//    Accessor := TIDProperty(Prop).Getter;
//    if not Assigned(Accessor) then
//      ERROR_CANNOT_ACCESS_TO_WRITEONLY_PROPERTY(Expression);
//  end;
//  Expression.Declaration := Accessor;
//  case Accessor.ItemType of
//    itConst: begin
//      PMContext.Clear;
//      Exit;
//    end;
//    itVar: Expression.Declaration := Accessor;
//    itProperty: {продолжаем выполнение};
//    itProcedure: begin
//      case PMContext.Count of
//        0: SelfExpr := EContext.SContext.Proc.SelfParamExpression;
//        1: SelfExpr := PMContext.Last;
//      else
//        SelfExpr := GetTMPRefExpr(EContext.SContext, PMContext.DataType);
//        SelfExpr.TextPosition := PMContext.ID.TextPosition;
//        ILWrite(EContext.SContext, TIL.IL_GetPtr(SelfExpr, PMContext.Items));
//      end;
//
//      CallExpr := TIDCallExpression.Create(Accessor);
//      CallExpr.TextPosition := Expression.TextPosition;
//      CallExpr.Instance := SelfExpr;
//
//      if Prop.ParamsCount > 0 then
//        Result := ParseIndexedPropertyArgs(Scope, ArgumentsCount, EContext)
//      else
//        ArgumentsCount := 0;
//
//      CallExpr.ArgumentsCount := ArgumentsCount;
//      RPNPushExpression(EContext, CallExpr);
//
//      Expression := Process_CALL(EContext);
//      //ReleaseExpressions(EContext.SContext, PMContext.Items);  // не работает пока!!!
//      PMContext.Clear;
//    end;
//  else
//    ERROR_FEATURE_NOT_SUPPORTED;
//  end;
end;

function TNPUnit.ParseGenericMember(const PMContext: TPMContext; SContext: PSContext; StrictSearch: Boolean; out Decl: TIDDeclaration; out WithExpression: TIDExpression): TTokenID;
var
  Scope: TScope;
  GenericArgs: TIDExpressions;
  ExprName: string;
begin
  Scope := PMContext.ItemScope;

  Result := ParseGenericsArgs(Scope, SContext, GenericArgs);
  ExprName := format('%s<%d>', [PMContext.ID.Name, Length(GenericArgs)]);


  if not StrictSearch then
    Decl := Scope.FindIDRecurcive(ExprName, WithExpression)
  else
    Decl := Scope.FindMembers(ExprName);
  if not Assigned(Decl) then
    ERROR_UNDECLARED_ID(ExprName, parser_PrevPosition);

  if Decl.ItemType = itType then
    Decl := SpecializeGenericType(TIDType(Decl), PMContext.ID, GenericArgs)
  else
    ERROR_FEATURE_NOT_SUPPORTED;
end;

function TNPUnit.ParseMacroArg(Scope: TScope; const ExprID: TIdentifier; Arg: TIDMacroArgument; out Expression: TIDExpression; var EContext: TEContext): TTokenID;
var
  PrevPos: TParserPosition;
  PrevSrc: string;
  Decl: TIDStringConstant;
begin
  if Assigned(Arg.ArgExpession) and Arg.ArgExpession.IsConstant then
  begin
    Expression := Arg.ArgExpession
  end else
  if not EContext.SContext.ExpandMacro then
  begin
    Decl := TIDStringConstant.CreateAnonymous(nil, SYSUnit._String, Arg.Text);
    Expression := TIDExpression.Create(Decl, Arg.TextPosition);
  end else
  begin
    FParser.SaveState(PrevPos);
    PrevSrc := FParser.Source;

    FParser.Source := Arg.Text;
    FParser.First;
    FParser.NextToken;

    try
      ParseMember(Scope, Expression, EContext);
      if Assigned(Expression) then
        Expression.TextPosition := ExprID.TextPosition;
    except
      on e: ECompilerAbort do begin
        E.CompilerMessage.Row := ExprID.TextPosition.Row;
        E.CompilerMessage.Col := ExprID.TextPosition.Col;
        raise;
      end;
    end;

    FParser.Source := PrevSrc;
    FParser.LoadState(PrevPos);
  end;
  Result := TTokenID(FParser.CurrentTokenID);
end;

function TNPUnit.ParseMember(Scope: TScope; out Expression: TIDExpression; var EContext: TEContext): TTokenID;
//var
//  Decl: TIDDeclaration;
//  DataType: TIDType;
//  Indexes: TIDExpressions;
//  i: Integer;
//  Expr, NExpr: TIDExpression;
//  StrictSearch: Boolean;
//  GenericArgs: TIDExpressions;
//  CallExpr: TIDCallExpression;
//  PMContext: TPMContext;
//  WasProperty: Boolean;
//  SContext: PSContext;
begin
  Assert(False);
//  WasProperty := False;
//  PMContext.Init;
//  PMContext.ItemScope := Scope;
//  StrictSearch := False;
//  SContext := EContext.SContext;
//  while True do begin
//    parser_ReadCurrIdentifier(PMContext.ID);
//    if not StrictSearch then
//      Decl := FindIDNoAbort(PMContext.ItemScope, PMContext.ID, Expr)
//    else begin
//      Decl := PMContext.ItemScope.FindMembers(PMContext.ID.Name);
//      Expr := nil;
//    end;
//
//    Result := parser_NextToken(Scope);
//    if not Assigned(Decl) then begin
//      if Result = token_less then
//        Result := ParseGenericMember(PMContext, SContext, StrictSearch, Decl, Expr);
//
//      if PMContext.ItemScope is TConditionalScope then
//      begin
//        Decl := TIDStringConstant.CreateAnonymous(PMContext.ItemScope, SYSUnit._String, PMContext.ID.Name);
//      end;
//
//
//      if not Assigned(Decl) then
//        ERROR_UNDECLARED_ID(PMContext.ID);
//    end;
//
//    // если Scope порожден конструкцией WITH
//    // то добавляем в выражение первым элементом
//    if Assigned(Expr) then begin
//      if Expr.ExpressionType = etDeclaration then begin
//        PMContext.Add(Expr);
//      end else begin
//        Indexes := TIDMultiExpression(Expr).Items;
//        for i := 0 to Length(Indexes) - 1 do
//          PMContext.Add(Indexes[i]);
//      end;
//    end;
//
//    {проверяем на псевдоним}
//    if Decl.ItemType = itAlias then
//      Decl := TIDAlias(Decl).Original;
//
//    {проверяем на доступ к члену типа}
//    if Assigned(EContext.SContext) then
//      CheckAccessMember(SContext, Decl, PMContext.ID);
//
//    case Decl.ItemType of
//      {процедура/функция}
//      itProcedure: begin
//        Expression := TIDCallExpression.Create(Decl, PMContext.ID.TextPosition);
//        // если есть открытая угловая скобка, - значит generic-вызов
//        if Result = token_less then
//        begin
//          Result := ParseGenericsArgs(Scope, SContext, GenericArgs);
//          SetProcGenericArgs(TIDCallExpression(Expression), GenericArgs);
//        end;
//        // если есть открытая скобка, - значит вызов
//        if Result = token_openround then
//        begin
//          Result := ParseEntryCall(Scope, TIDCallExpression(Expression), EContext);
//          // если это метод, подставляем self из пула
//          if PMContext.Count > 0 then
//          begin
//            if PMContext.Count > 1 then
//              Expr := ProcessMemberExpression(SContext, WasProperty, PMContext)
//            else
//              Expr := PMContext.Last;
//
//            if (Expr.Declaration is TIDField) and (PMContext.Count = 1) then
//            begin
//              NExpr := GetTMPRefExpr(SContext, Expr.DataType);
//              NExpr.TextPosition := Expr.TextPosition;
//              ILWrite(SContext, TIL.IL_GetPtr(NExpr, nil, Expr));
//              Expr := NExpr;
//            end;
//            TIDCallExpression(Expression).Instance := Expr;
//          end else
//          if (Decl.ItemType = itProcedure) and Assigned(TIDProcedure(Decl).Struct) and
//             (TIDProcedure(Decl).Struct = SContext.Proc.Struct) then
//          begin
//            // если это собственный метод, добавляем self из списка параметров
//            TIDCallExpression(Expression).Instance := TIDExpression.Create(SContext.Proc.SelfParam);
//          end;
//
//          // если выражение продолжается дальше, генерируем вызов процедуры
//          if Result in [token_dot, token_openblock] then
//          begin
//            Expression := EContext.RPNPopOperator();
//            Decl := Expression.Declaration;
//            PMContext.Clear;
//          end else begin
//            Expression := nil;
//            Break;
//          end;
//
//        end else begin // иначе создаем процедурный тип, если он отсутствовал
//          TIDProcedure(Decl).CreateProcedureTypeIfNeed(Scope);
//          PMContext.DataType := TIDProcedure(Decl).DataType;
//          AddType(TIDProcedure(Decl).DataType);
//        end;
//      end;
//      {макро функция}
//      itMacroFunction: begin
//        Expression := TIDExpression.Create(Decl, PMContext.ID.TextPosition);  // создание Expression под ???
//        Result := ParseBuiltinCall(Scope, Expression, EContext);
//        Expression := nil;
//        Break;
//      end;
//      {переменная}
//      itVar: begin
//        if Decl.ClassType = TIDMacroArgument then
//          Result := ParseMacroArg(Scope, PMContext.ID, TIDMacroArgument(Decl), Expression, EContext)
//        else begin
//          // если есть открытая скобка, - значит вызов
//          if Result = token_openround then
//          begin
//            if Decl.DataTypeID <> dtProcType then
//              ERROR_PROC_OR_PROCVAR_REQUIRED(PMContext.ID);
//
//            Expression := TIDCallExpression.Create(Decl, PMContext.ID.TextPosition);
//            Result := ParseEntryCall(Scope, TIDCallExpression(Expression), EContext);
//            Expression := nil;
//            Break;
//          end;
//          Expression := TIDExpression.Create(Decl, PMContext.ID.TextPosition);
//        end;
//        PMContext.DataType := Decl.DataType;
//      end;
//      itConst, itUnit, itNameSpace: begin
//        Expression := TIDExpression.Create(Decl, PMContext.ID.TextPosition);
//        PMContext.DataType := Decl.DataType;
//      end;
//      {свойство}
//      itProperty: begin
//        WasProperty := True;
//        Result := ParsePropertyMember(PMContext, Scope, TIDProperty(Decl), Expression, EContext);
//        // заменяем декларацию
//        if Assigned(Expression) then
//        begin
//          Decl := Expression.Declaration;
//          // если геттер/сеттер - поле, то снимаем флаг "свойства"
//          if Decl.ItemType = itVar then
//            WasProperty := False;
//        end;
//        PMContext.DataType := Decl.DataType;
//      end;
//      {тип}
//      itType: begin
//        Expression := TIDExpression.Create(Decl, PMContext.ID.TextPosition);
//        {явное преобразование типов}
//        if Result = token_openround then
//           Result := ParseExplicitCast(Scope, SContext, Expression);
//
//        PMContext.DataType := Decl.DataType;
//      end;
//    else
//      ERROR_FEATURE_NOT_SUPPORTED;
//    end;
//
//    PMContext.Add(Expression);
//
//    {array type}
//    if Result = token_openblock then begin
//      Result := ParseArrayMember(PMContext, Scope, Decl, PMContext.DataType, EContext);
//      if PMContext.DataType = nil then
//      begin
//        Expression := nil;
//        Break;
//      end;
//    end;
//
//    {call} // todo: make this code as main (remove previous same code)
//    if (Result = token_openround) and (Expression.DataTypeID = dtProcType) then
//    begin
//      if Expression.DataTypeID <> dtProcType then
//        ERROR_PROC_OR_PROCVAR_REQUIRED(PMContext.ID);
//
//      if not (Expression is TIDCastExpression) then
//        CallExpr := TIDCallExpression.Create(Expression.Declaration, Expression.TextPosition)
//      else begin
//        CallExpr := TIDCastedCallExpression.Create(Expression.Declaration, Expression.TextPosition);
//        TIDCastedCallExpression(CallExpr).DataType := Expression.DataType;
//      end;
//
//      Result := ParseEntryCall(Scope, CallExpr, EContext);
//      Expression := nil;
//      Break;
//    end;
//
//    {struct/class/interafce/enum/unit/namespace}
//    if (Result = token_dot) then
//    begin
//      if Decl.ItemType in [itNameSpace, itUnit] then begin
//        PMContext.ItemScope := TIDNameSpace(Decl).Members;
//        PMContext.Clear;
//        parser_NextToken(Scope);
//        StrictSearch := True;
//        continue;
//      end else
//      if Decl.ItemType <> itType then
//        DataType := Decl.DataType
//      else
//        DataType := TIDType(Decl);
//
//      if DataType.ClassType = TIDAliasType then
//        DataType := TIDAliasType(DataType).Original;
//
//      if DataType.DataTypeID in [dtStaticArray, dtDynArray] then
//        DataType := TIDArray(DataType).ElementDataType;
//
//      if DataType.DataTypeID in [dtPointer, dtClassOf] then
//        DataType := TIDPointer(DataType).ReferenceType;
//
//      if DataType is TIDStructure then
//        PMContext.ItemScope := TIDStructure(DataType).Members
//      else
//      if Decl.ClassType = TIDEnum then begin
//        PMContext.ItemScope := TIDEnum(Decl).Items;
//        PMContext.Clear;
//      end else
//        ERROR_IDENTIFIER_HAS_NO_MEMBERS(Decl);
//      parser_NextToken(Scope);
//      StrictSearch := True;
//      continue;
//    end;
//
//    if PMContext.Count > 1 then
//      Expression := ProcessMemberExpression(SContext, WasProperty, PMContext)
//    else
//    if (Decl.ClassType = TIDField) and Assigned(EContext.SContext) then
//    begin
//      Expr := GetTMPRefExpr(SContext, Decl.DataType);
//      Expr.TextPosition := PMContext.ID.TextPosition;
//      ILWrite(SContext, TIL.IL_GetPtr(Expr, nil, Expression));
//      Expression := Expr;
//    end;
//    break;
//  end;
end;

function TNPUnit.ProcessMemberExpression(SContext: PSContext; WasProperty: Boolean; var PMContext: TPMContext): TIDExpression;
var
  Decl: TIDDeclaration;
begin
  if not WasProperty and (PMContext.Items[PMContext.Count - 2].DataTypeID <> dtSet) then
  begin
    Decl := PMContext.Last.Declaration;
    if Decl.ItemType <> itProcedure then
      Result := GetTMPRefExpr(SContext, PMContext.DataType)
    else
      Result := GetTMPVarExpr(SContext, PMContext.DataType);
    Result.TextPosition := PMContext.ID.TextPosition;
    ILWrite(SContext, TIL.IL_GetPtr(Result, PMContext.Items));
    ReleaseExpressions(SContext, PMContext.Items);
  end else begin
    // если было свойтво(оно не может быть развернуто в GETPTR немедлено),
    // то обработка откладывается на более поздний срок
    Result := TIDMultiExpression.Create(PMContext.Items, PMContext.ID.TextPosition);
    TIDMultiExpression(Result).EffectiveDataType := PMContext.DataType;
  end;
end;

function TNPUnit.ParseMultyComment(Scope: TScope): TTokenID;
begin
  Result := token_unknown;
end;

class function TNPUnit.IsConstValueInRange(Value: TIDExpression; RangeExpr: TIDRangeConstant): Boolean;
var
  Expr: TIDExpression;
begin
  Expr := ProcessConstOperation(Value, RangeExpr.Value.LBExpression, opLess);
  if TIDBooleanConstant(Expr.Declaration).Value then
    Exit(False);

  Expr := ProcessConstOperation(Value, RangeExpr.Value.HBExpression, opLessOrEqual);
  Result := TIDBooleanConstant(Expr.Declaration).Value;
end;

class function TNPUnit.IsConstRangesIntersect(const Left, Right: TIDRangeConstant): Boolean;
var
  Expr,
  LeftLB, LeftHB,
  RightLB, RightHB: TIDExpression;
begin
  LeftLB := Left.Value.LBExpression;
  LeftHB := Left.Value.HBExpression;

  RightLB := Right.Value.LBExpression;
  RightHB := Right.Value.HBExpression;

  Expr := ProcessConstOperation(LeftLB, RightLB, opLess);
  // если Left.Low < Right.Low
  if TIDBooleanConstant(Expr.Declaration).Value then
  begin
    Expr := ProcessConstOperation(LeftHB, RightLB, opGreaterOrEqual);
    Result := TIDBooleanConstant(Expr.Declaration).Value;
  end else begin
    Expr := ProcessConstOperation(RightHB, LeftLB, opGreaterOrEqual);
    Result := TIDBooleanConstant(Expr.Declaration).Value;
  end;
end;

function TNPUnit.ParseCaseStatement(Scope: TScope; SContext: PSContext): TTokenID;
type
  TMatchItem = record
    Expression: TIDExpression;
    FirstInstruction: TILInstruction;
    JMPToEnd: TILJampedInstruction;
  end;
  PMatchItem = ^TMatchItem;
var
  MatchItems: array of TMatchItem;
  MatchItem: PMatchItem;
  procedure CheckUniqueMIExpression(Cur: TIDExpression);
  var
    i: Integer;
    Prev: TIDExpression;
    IsDuplicate: Boolean;
  begin
    IsDuplicate := False;
    for i := 0 to Length(MatchItems) - 2 do
    begin
      Prev := MatchItems[i].Expression;
      if Prev.IsConstant and Cur.IsConstant then
      begin
        if Prev.DataTypeID = dtRange then
        begin
          if Cur.DataTypeID <> dtRange then
            IsDuplicate := IsConstValueInRange(Cur, TIDRangeConstant(Prev.Declaration))
          else
            IsDuplicate := IsConstRangesIntersect(TIDRangeConstant(Cur.Declaration), TIDRangeConstant(Prev.Declaration))
        end else
        if Cur.DataTypeID = dtRange then
        begin
          IsDuplicate := IsConstValueInRange(Prev, TIDRangeConstant(Cur.Declaration));
        end else
          IsDuplicate := IsConstEqual(Prev, Cur);
      end else
      if (not Prev.IsAnonymous) and (not Cur.IsAnonymous) then
      begin
        IsDuplicate := (Prev.Declaration = Cur.Declaration);
      end;
      if IsDuplicate then
        AbortWork(sDuplicateMatchExpression, Cur.TextPosition);
    end;
  end;
var
  EContext: TEContext;
  SExpression,
  DExpression: TIDExpression;
  i, TotalMICount, ItemsCount: Integer;
  ElsePresent: Boolean;
  SEConst: Boolean;
  MISContext: TSContext;
  NeedWriteIL,
  NeedCMPCode: Boolean;
  JMPToEnd: TILJampedInstruction;
  FirstMatchingInstruction,
  StartMIInstruction: TILInstruction;
  Implicit: TIDDeclaration;
begin
  // NeedCMPCode := False;
  // CASE выражение
  CFBBegin(SContext, CFB_CASE);
  InitEContext(EContext, SContext, ExprRValue);
  Result := ParseExpression(Scope, EContext, parser_NextToken(Scope));
  SExpression := RPNPopExpression(EContext);
  CheckEmptyExpression(SExpression);
  if Assigned(EContext.LastBoolNode) then
    Bool_CompleteImmediateExpression(EContext, SExpression);
  SEConst := SExpression.IsConstant;
  parser_MatchToken(Result, token_of);
  ItemsCount := 0;
  TotalMICount := 0;
  NeedWriteIL:= SContext.WriteIL;
  ElsePresent := False;
  Result := parser_NextToken(Scope);
  MISContext.Assign(SContext);
  FirstMatchingInstruction := SContext.ILLast;
  while Result <> token_end do
  begin
    InitEContext(EContext, @MISContext, ExprRValue);
    if Result <> token_else then
    begin
      JMPToEnd := nil;
      while True do begin
        CFBBegin(SContext, CFB_CASE_ENTRY);
        // запоминаем начало секции
        StartMIInstruction := SContext.ILLast;
        Result := ParseExpression(Scope, EContext, Result);
        DExpression := RPNPopExpression(EContext);
        CheckEmptyExpression(DExpression);
        // проверка на совпадение типа
        if DExpression.DataTypeID = dtRange then
          Implicit := CheckImplicit(SExpression, TIDRangeType(DExpression.DataType).ElementType)
        else
          Implicit := CheckImplicit(SExpression, DExpression.DataType);
        if not Assigned(Implicit) then
          AbortWork(sMatchExprTypeMustBeIdenticalToCaseExprFmt, [DExpression.DataTypeName, SExpression.DataTypeName], DExpression.TextPosition);

        // проверяем на константу
        if DExpression.IsConstant and SEConst then
        begin
          // если данное выражение истенно, то для остальных генерировать IL код не нужно
          if ((DExpression.DataTypeID = dtRange) and IsConstValueInRange(SExpression, TIDRangeConstant(DExpression.Declaration)))
              or IsConstEqual(SExpression, DExpression) then
          begin
            NeedWriteIL := False;
            MISContext.WriteIL := SContext.WriteIL;
            // удаляем весь ранее сгенерированный IL код
            if Assigned(FirstMatchingInstruction) and Assigned(FirstMatchingInstruction.Next) then
              ILDelete(SContext, FirstMatchingInstruction.Next, SContext.ILLast);
          end else
            MISContext.WriteIL := False;
          // служебные инструкции сравнения и переходов не нужны
          NeedCMPCode := False;
        end else begin
          MISContext.WriteIL := NeedWriteIL;
          NeedCMPCode := NeedWriteIL;
          if NeedCMPCode then
          begin
            SetLength(MatchItems, ItemsCount + 1);
            MatchItem := @MatchItems[ItemsCount];
            MatchItem.Expression := DExpression;
            MatchItem.FirstInstruction := StartMIInstruction;
            if not Assigned(JMPToEnd) then
              JMPToEnd := TIL.IL_JmpNext(parser_Line, cNone, nil);
            MatchItem.JMPToEnd := JMPToEnd;

            // код проверки условия
            if DExpression.DataTypeID <> dtRange then
            begin
              ILWrite(SContext, TIL.IL_Cmp(SExpression, DExpression));
              ILWrite(SContext, TIL.IL_JmpNext(SExpression.Line, cNotEqual, JMPToEnd));
              Bool_AddExprNode(EContext, MISContext.ILLast, cEqual);
            end else
              Process_operator_In(EContext, SExpression, DExpression);
            Inc(ItemsCount);
          end;
        end;
        CheckUniqueMIExpression(DExpression);

        if Assigned(EContext.LastBoolNode) and Assigned(EContext.LastBoolNode.PrevNode) then
          Bool_AddExprNode(EContext, ntOr);

        // если была запятая, парсим следующее выражение
        if Result <> token_coma then
          break;

        Result := parser_NextToken(Scope);
      end;
      // двоеточие
      parser_MatchToken(Result, token_colon);
      parser_NextToken(Scope);
      // корректируем переходы
      Bool_CompleteExpression(EContext.LastBoolNode, JMPToEnd);
      // парсим код секции
      Result := ParseStatements(Scope, @MISContext, False);
      if NeedCMPCode then
        ILWrite(@MISContext, JMPToEnd);
      MISContext.WriteIL := NeedWriteIL;
      parser_MatchToken(Result, token_semicolon);
      Result := parser_NextToken(Scope);
      Inc(TotalMICount);
      CFBEnd(SContext, CFB_CASE_ENTRY);
    end else begin
      // ELSE секция
      CFBBegin(SContext, CFB_CASE_ELSE);
      if SEConst and NeedWriteIL then
      begin
        // удаляем весь ранее сгенерированный IL код
        if Assigned(FirstMatchingInstruction) and Assigned(FirstMatchingInstruction.Next) then
          ILDelete(SContext, FirstMatchingInstruction.Next, SContext.ILLast);
      end;
      parser_NextToken(Scope);
      MISContext.WriteIL := NeedWriteIL;
      Result := ParseStatements(Scope, @MISContext, True);
      parser_MatchToken(Result, token_end);
      Result := parser_NextToken(Scope);
      ElsePresent := True;
      Inc(TotalMICount);
      CFBEnd(SContext, CFB_CASE_ELSE);
      Break;
    end;
  end;
  // проверяем есть ли хоть одна секция(включая ELSE) в кейсе
  if TotalMICount = 0 then
    AbortWork(sCaseStmtRequireAtLeastOneMatchExpr, FParser.PrevPosition);
  // пост-обработка элементов
  for i := 0 to ItemsCount - 1 do
  begin
    MatchItems[i].JMPToEnd.Destination := SContext.ILLast;
  end;
  // если небыло ELSE секции, парсим END;
  if not ElsePresent then begin
    parser_MatchToken(Result, token_end);
    Result := parser_NextToken(Scope);
  end;
  CFBEnd(SContext, CFB_CASE);
end;

procedure TNPUnit.ParseCondDefine(Scope: TScope; add_define: Boolean);
var
  ID: TIdentifier;
  idx: Integer;
begin
  parser_ReadNextIdentifier(Scope, ID);
  idx := FDefines.IndexOf(ID.Name);
  if add_define then begin
    if idx = -1 then
      FDefines.Add(ID.Name);
  end else begin
    if idx > -1 then
      FDefines.Delete(idx);
  end;
end;

function TNPUnit.ParseCondIf(Scope: TScope; out ExpressionResult: TCondIFValue): TTokenID;
var
  Expr: TIDExpression;
  CondScope: TConditionalScope;
begin
  CondScope := TConditionalScope.Create(TScopeType.stLocal, Scope);
  FParser.NextToken;
  Result := ParseConstExpression(CondScope, Expr, ExprRValue);
  if Expr.DataTypeID <> dtGeneric then
  begin
    CheckBooleanExpression(Expr);
    ExpressionResult := TCondIFValue(Expr.AsBoolConst.Value);
  end else
    ExpressionResult := condIFUnknown;
end;

function TNPUnit.ParseCondIfDef(Scope: TScope): Boolean;
var
  ID: TIdentifier;
begin
  parser_ReadNextIdentifier(Scope, ID);
  Result := Defined(ID.Name);
end;

function TNPUnit.ParseCondInclude(Scope: TScope): TTokenID;
begin
end;

function TNPUnit.ParseCondOptPop(Scope: TScope): TTokenID;
var
  ID: TIdentifier;
begin
  parser_ReadNextIdentifier(Scope, ID);
  if not FOptions.OptPop(ID.Name) then
    ERROR_UNKNOWN_OPTION(ID);
  parser_ReadSemicolon(Scope);
  Result := FParser.NextToken;
end;

function TNPUnit.ParseCondOptPush(Scope: TScope): TTokenID;
var
  ID: TIdentifier;
begin
  parser_ReadNextIdentifier(Scope, ID);
  if not FOptions.OptPush(ID.Name) then
    ERROR_UNKNOWN_OPTION(ID);
  parser_ReadSemicolon(Scope);
  Result := FParser.NextToken;
end;

function TNPUnit.ParseCondOptSet(Scope: TScope): TTokenID;
var
  ID: TIdentifier;
  Expr: TIDExpression;
begin
  parser_ReadNextIdentifier(Scope, ID);
  if not FOptions.Exist(ID.Name) then
    ERROR_UNKNOWN_OPTION(ID);

  parser_ReadToken(Scope, token_equal);

  FParser.NextToken;
  Result := ParseConstExpression(Scope, Expr, ExprRValue);

  parser_MatchSemicolon(Result);

  CheckEmptyExpression(Expr);

  FOptions.OptSet(ID.Name, Expr.AsConst.AsVariant);
  Result := FParser.NextToken;
end;

function TNPUnit.ParseImmVarStatement(Scope: TScope; Scontext: PSContext): TTokenID;
var
  i, c: Integer;
  DataType: TIDType;
  Expr: TIDExpression;
  Variable: TIDVariable;
  Names: TIdentifiersPool;
  EContext: TEContext;
  Vars: array of TIDVariable;
begin
  c := 0;
  Names := TIdentifiersPool.Create(1);
  Result := parser_NextToken(Scope);

  while True do begin
    parser_MatchIdentifier(Result);
    Names.Add;
    parser_ReadCurrIdentifier(Names.Items[c]);
    Result := parser_NextToken(Scope);
    if Result = token_Coma then begin
      Inc(c);
      Result := parser_NextToken(Scope);
      Continue;
    end;

    // парсим тип, если определен
    if Result = token_colon then
      Result := ParseTypeSpec(Scope, DataType)
    else
      DataType := nil;

    parser_MatchToken(Result, token_assign);

    InitEContext(EContext, SContext, ExprRValue);

    SetLength(Vars, c + 1);
    for i := 0 to c do
    begin
      Variable := TIDVariable.Create(Scope, Names.Items[i]);
      Variable.DataType := DataType;
      Variable.Visibility := vLocal;
      Variable.DefaultValue := nil;
      Variable.Absolute := nil;
      Scope.AddVariable(Variable);
      Vars[i] := Variable;
      Expr := TIDExpression.Create(Variable, Variable.TextPosition);
      RPNPushExpression(EContext, Expr);
    end;

    Result := parser_NextToken(Scope);
    Result := ParseExpression(Scope, EContext, Result);

    if not Assigned(DataType) then
    begin
      DataType := EContext.Result.DataType;
      for i := 0 to c do
        Vars[i].DataType := DataType
    end;

    RPNPushOperator(EContext, opAssignment);
    EContext.RPNFinish;

    parser_MatchSemicolon(Result);
    break;
  end;
end;

function TNPUnit.ParseDelphiCondIfDef(Scope: TScope): Boolean;
var
  ID: TIdentifier;
begin
  parser_ReadNextIdentifier(Scope, ID);
  Result := Defined(ID.Name);
  parser_MatchToken(FParser.NextToken, token_closefigure);
end;

function TNPUnit.ParseCondSkip(Scope: TScope): TTokenID;
var
  ID: TIdentifier;
begin
  Result := FParser.NextToken;
  if Result = token_identifier then
  begin
    parser_ReadCurrIdentifier(ID);
    PutMessage(cmtHint, ID.Name, ID.TextPosition);
  end;
  raise ECompilerSkip.Create;
end;

function TNPUnit.ParseCondError(Scope: TScope): TTokenID;
var
  ID: TIdentifier;
begin
  parser_ReadNextIdentifier(Scope, ID);
  AbortWork(ID.Name, ID.TextPosition);
  Result := parser_NextToken(Scope);
end;

function TNPUnit.ParseCondMessage(Scope: TScope): TTokenID;
var
  ID: TIdentifier;
  MsgType: string;
begin
  parser_ReadNextIdentifier(Scope, ID);
  MsgType := UpperCase(ID.Name);
  parser_ReadNextIdentifier(Scope, ID);
  if MsgType = 'ERROR' then
    AbortWork(ID.Name, ID.TextPosition);
  // todo:
  Result := FParser.NextToken;
end;

function TNPUnit.ParseCondHint(Scope: TScope): TTokenID;
var
  ID: TIdentifier;
begin
  parser_ReadNextIdentifier(Scope, ID);
  PutMessage(cmtHint, ID.Name, ID.TextPosition);
  Result := parser_NextToken(Scope);
end;

function TNPUnit.ParseCondWarn(Scope: TScope): TTokenID;
var
  ID: TIdentifier;
begin
  parser_ReadNextIdentifier(Scope, ID);
  PutMessage(cmtWarning, ID.Name, ID.TextPosition);
  Result := parser_NextToken(Scope);
end;

function TNPUnit.parser_SkipBlock(StopToken: TTokenID): TTokenID;
var
  ECnt: Integer;
begin
  ECnt := 0;
  while True do begin
    Result := FParser.NextToken;
    case Result of
      token_begin, token_try, token_case: Inc(ECnt);
      token_end: begin
        if (StopToken = token_end) and (ECnt = 0) then
          Exit;
        Dec(ECnt);
        if (StopToken = token_end) and (ECnt = 0) then
          Exit;
      end;
      token_eof: Exit(token_eof);
    end;
  end;
end;

function TNPUnit.ParseCondStatements(Scope: TScope; Token: TTokenID): TTokenID;
  function SkipToElseOrEnd(SkipToEnd: Boolean): TTokenID;
  var
    ifcnt: Integer;
  begin
    ifcnt := 0;
    while True do begin
      Result := FParser.NextToken;
      case Result of
        token_cond_if,
        token_cond_ifdef,
        token_cond_ifopt,
        token_cond_ifndef: Inc(ifcnt); // считаем вложенные конструкции

        token_cond_else: begin
          if SkipToEnd then
            continue;
          if ifcnt = 0 then
            Exit;
        end;
        token_cond_else_if: begin
          if ifcnt = 0 then
            Exit;
        end;
        token_cond_end: begin
          if ifcnt = 0 then
            Exit;
          Dec(ifcnt);
        end;
        token_eof: ERROR_END_OF_FILE;
      end;
    end;
  end;
var
  ExprResult: TCondIFValue;
  BP: TBreakPoint;
  CondResult: Boolean;
begin
  Result := Token;
  while true do
  begin
    case Result of
      //////////////////////////////////////////
      // {$DEFINE ...}
      token_cond_define: begin
        ParseCondDefine(Scope, True);
        Result := FParser.NextToken;
        parser_MatchToken(Result, token_closefigure);
        Result := parser_NextToken(Scope);
      end;
      //////////////////////////////////////////
      // {$else ... }
      token_cond_else: begin
        // skip all comment tokens
        repeat
          Result := FParser.NextToken;
        until (Result = token_eof) or (Result = token_closefigure);
        if fCondStack.Top then
          Result := SkipToElseOrEnd(False);
      end;
      //////////////////////////////////////////
      // {$elseif (condition)}
      token_cond_else_if: begin
        if fCondStack.Top then
          Result := SkipToElseOrEnd(True)
        else begin
          Result := ParseCondIf(Scope, ExprResult);
          fCondStack.Top := (ExprResult = condIfTrue);
          case ExprResult of
            condIFFalse: Result := SkipToElseOrEnd(False);
            condIfTrue:;
            condIFUnknown: Result := SkipToElseOrEnd(True);
          end;
        end;
      end;
      //////////////////////////////////////////
      // {$endif ...}
      token_cond_end: begin
        fCondStack.Pop;
        repeat
          Result := FParser.NextToken;
        until (Result = token_eof) or (Result = token_closefigure);
        Result := FParser.NextToken;
      end;
      //////////////////////////////////////////
      // #include
      token_cond_include: Result := ParseCondInclude(Scope);
      //////////////////////////////////////////
      // {$UNDEFINE ...}
      token_cond_undefine: begin
        ParseCondDefine(Scope, False);
        Result := FParser.NextToken;
        parser_MatchToken(Result, token_closefigure);
        Result := parser_NextToken(Scope);
      end;
      //////////////////////////////////////////
      // {$IFDEF ...}
      token_cond_ifdef: begin
        CondResult := ParseDelphiCondIfDef(Scope);
        fCondStack.Push(CondResult);
        if not CondResult then
          Result := SkipToElseOrEnd(False)
        else
          Result := FParser.NextToken;
      end;
      //////////////////////////////////////////
      // {$IFNDEF ...}
      token_cond_ifndef: begin
        CondResult := ParseDelphiCondIfDef(Scope);
        fCondStack.Push(not CondResult);
        if CondResult then
          Result := SkipToElseOrEnd(False)
        else
          Result := FParser.NextToken;
      end;
      //////////////////////////////////////////
      // {$IF...}
      token_cond_if: begin
        Result := ParseCondIf(Scope, ExprResult);
        fCondStack.Push(ExprResult = condIfTrue);
        case ExprResult of
          condIFFalse: Result := SkipToElseOrEnd(False);
          condIFUnknown: Result := SkipToElseOrEnd(True);
        end;
      end;
      //////////////////////////////////////////
      // {$IFOPT...}
      token_cond_ifopt: begin
        // todo: complete the options check
        repeat
          Result := FParser.NextToken;
        until (Result = token_eof) or (Result = token_closefigure);
        fCondStack.Push(False);
        Result := SkipToElseOrEnd(False);
      end;
      //////////////////////////////////////////
      // {$MESSAGE...}
      token_cond_message: Result := ParseCondMessage(Scope);
    else
      ERROR_FEATURE_NOT_SUPPORTED;
      Result := token_unknown;
    end;
    if Result = token_closefigure then
      Result := FParser.NextToken;
    if Result < token_cond_define then
      Break;
  end;

{  if Result = token_cond_end then
    Result := FParser.NextToken;}
end;

(*function TNPUnit.ParseCondTarget(Scope: TScope): TTokenID;
var
  ID: TIdentifier;
begin
  parser_ReadNextIdentifier(Scope, ID);

  if (FPackage.Target = '') or (FPackage.Target = 'ANY') then
    FPackage.Target := ID.Name
  else
    AbortWork('Target is already defined as "%s"', [FPackage.Target], ID.TextPosition);

  // добавляем глобальный define
  FPackage.Defines.Add('target_' + ID.Name);

  parser_ReadSemicolon(Scope);
  Result := parser_NextToken(Scope);
end;*)

function TNPUnit.ParseConstSection(Scope: TScope): TTokenID;
var
  i, c: Integer;
  DataType: TIDType;
  Item: TIDDeclaration;
  Expr: TIDExpression;
  Names: TIdentifiersPool;
begin
  c := 0;
  Names := TIdentifiersPool.Create(2);
  parser_MatchIdentifier(parser_NextToken(Scope));
  repeat
    Names.Add;
    parser_ReadCurrIdentifier(Names.Items[c]); // read name
    Result := parser_NextToken(Scope);
    if Result = token_Coma then begin
      Inc(c);
      Result := parser_NextToken(Scope);
      parser_MatchIdentifier(Result);
      Continue;
    end;
    if Result = token_colon then
      Result := ParseTypeSpec(Scope, DataType)
    else
      DataType := nil;

    // =
    parser_MatchToken(Result, token_equal);

    if Assigned(DataType) then
    begin
      Result := ParseVarDefaultValue(Scope, DataType, Expr);
      Expr.Declaration.DataType := DataType;
      Expr.AsConst.ExplicitDataType := DataType;
    end else begin
      // читаем значение константы
      parser_NextToken(Scope);
      Result := ParseConstExpression(Scope, Expr, ExprRValue);
      CheckEmptyExpression(Expr);
    end;

    CheckConstExpression(Expr);

    if Expr.Declaration.DataType.DataTypeID in [dtStaticArray, dtRecord, dtGuid] then
      AddConstant(Expr.AsConst);

    if Result = token_platform then
      Result := ParsePlatform(Scope);

    Result := CheckAndParseDeprecated(Scope, Result);

    parser_MatchToken(Result, token_semicolon);
    for i := 0 to c do begin
      // создаем алиас на константу
      Item := TIDAlias.CreateAlias(Scope, Names.Items[i], Expr.Declaration);
      InsertToScope(Scope, Item);
    end;
    c := 0;
    Result := parser_NextToken(Scope);
  until Result <> token_identifier;
end;

procedure TNPUnit.PutMessage(MessageType: TCompilerMessageType; const MessageText: string);
var
  SourcePosition: TTextPosition;
  Msg: TCompilerMessage;
begin
  SourcePosition.Row := -1;
  SourcePosition.Col := -1;
  Msg := TCompilerMessage.Create(Self, MessageType, MessageText, SourcePosition);
  Msg.UnitName := _ID.Name;
  FMessages.Add(Msg);
end;

procedure TNPUnit.PutMessage(MessageType: TCompilerMessageType; const MessageText: string; const SourcePosition: TTextPosition);
var
  Msg: TCompilerMessage;
begin
  Msg := TCompilerMessage.Create(Self, MessageType, MessageText, SourcePosition);
  Msg.UnitName := _ID.Name;
  FMessages.Add(Msg);
end;

procedure TNPUnit.PutMessage(Message: TCompilerMessage);
begin
  Message.UnitName := Self.Name;
  if Message.Row <=0 then
  begin
    Message.Row := parser_Position.Row;
    Message.Col := parser_Position.Col;
  end;
  FMessages.Add(Message);
end;

procedure TNPUnit.ParseVector(Scope: TScope; var EContext: TEContext);
//  function MaxType(Type1, Type2: TIDType): TIDType;
//  begin
//    if Type1 = Type2 then
//      Exit(Type1);
//
//    if (Type1 = SYSUnit._Variant) or
//       (Type2 = SYSUnit._Variant) then
//     Exit(SYSUnit._Variant);
//
//    if Assigned(Type2.GetImplicitOperatorTo(Type1)) then
//    begin
//      if Type1.DataSize >= Type2.DataSize then
//        Result := Type1
//      else
//        Result := Type2;
//    end else
//      Result := SYSUnit._Variant;
//  end;
//var
//  i, c, Capacity: Integer;
//  InnerEContext: TEContext;
//  AConst: TIDDynArrayConstant;
//  Expr: TIDExpression;
//  Token: TTokenID;
//  SItems, DItems: TIDExpressions;
//  AType: TIDDynArray;
//  ElDt: TIDType;
//  IsStatic: Boolean;
begin
  Assert(False);
//  i := 0;
//  c := 0;
//  Capacity := 8;
//  IsStatic := True;
//  SetLength(SItems, Capacity);
//  InitEContext(InnerEContext, EContext.SContext, ExprNested);
//  while True do begin
//    Token := ParseExpression(Scope, InnerEContext, FParser.NextToken);
//    Expr := InnerEContext.Result;
//    if Assigned(Expr) then begin
//      if Expr.Declaration.ItemType <> itConst then
//        IsStatic := False;
//      Inc(c);
//      SItems[i] := Expr;
//    end else
//    if i > 0 then
//      CheckEmptyExpression(Expr);
//
//    case Token of
//      token_coma: begin
//        if i = 0 then CheckEmptyExpression(Expr);
//        Inc(i);
//        if i >= Capacity then begin
//          Capacity := Capacity * 2;
//          SetLength(SItems, Capacity);
//        end;
//        InnerEContext.Reset;
//        Continue;
//      end;
//      token_closeblock: Break;
//      else
//        ERROR_UNCLOSED_OPEN_BLOCK;
//    end;
//  end;
//
//  if c > 0 then begin
//    // копирование элементов
//    SetLength(DItems, c);
//    Move(SItems[0], DItems[0], SizeOf(Pointer)*c);
//    // вывод типа элемента
//    Expr := SItems[0];
//    ElDt := Expr.DataType;
//    for i := 1 to c - 1 do begin
//      Expr := SItems[i];
//      ElDt := MaxType(ElDt, Expr.DataType);
//    end;
//  end else
//    ElDt := SYSUnit._Variant;
//
//  // создаем анонимный тип константного массива
//  AType := TIDDynArray.CreateAsAnonymous(Scope);
//  AType.ElementDataType := ElDt;
//  AType.OverloadImplicitTo(dtSet, TIDOpImplicitDynArrayToSet.CreateInternal(nil));
//  // добовляем его в пул
//  AddType(AType);
//  // создаем анонимный константный динамический массив
//  AConst := TIDDynArrayConstant.CreateAnonymous(Scope, AType, DItems);
//  AConst.ArrayStatic := IsStatic;
//  // если массив константный, добовляем его в пул констант
//  if IsStatic then
//    AddConstant(AConst);
//
//  Expr := TIDExpression.Create(AConst, FParser.Position);
//  // заталкиваем массив в стек
//  RPNPushExpression(EContext, Expr);
end;

function TNPUnit.ParseConstExpression(Scope: TScope; out Expr: TIDExpression; EPosition: TExpessionPosition): TTokenID;
begin
  Assert(False);
end;

function TNPUnit.ParseEmitExpression(Scope: TScope; out Expr: TIDExpression): TTokenID;
var
  EContext: TEContext;
  SContext: TSContext;
begin
  SContext.Initialize;
  SContext.WriteIL := False;
  SContext.ExpandMacro := False;
  InitEContext(EContext, addr(SContext), ExprRValue);
  Result := ParseExpression(Scope, EContext, parser_NextToken(Scope));
  Expr := EContext.Result;
  if not Assigned(Expr) then
    Exit;
  if Expr.IsAnonymous then
    Expr.TextPosition := parser_PrevPosition;
  if (Expr.ItemType <> itType) and (Expr.DataTypeID <> dtGeneric) and not (Expr.Declaration is TIDMacroArgument) then
    CheckConstExpression(Expr);
end;

procedure TNPUnit.CheckLeftOperand(const Status: TEContext.TRPNStatus);
begin
  if Status <> rpOperand then
    ERROR_EXPRESSION_EXPECTED;
end;

function TNPUnit.ParseExpression(Scope: TScope; var EContext: TEContext; SrartToken: TTokenID): TTokenID;
//var
//  ID: TIdentifier;
//  Status: TEContext.TRPNStatus;
//  Expr: TIDExpression;
//  RoundCount: Integer;
//  SContext: PSContext;
begin
  Assert(False);
//  Status := rprOk;
//  RoundCount := 0;
//  Result := SrartToken;
//  SContext := EContext.SContext;
//  while True do begin
//    case Result of
//      token_eof: Break;// ERROR_END_OF_FILE;
//      token_openround: begin
//        Inc(RoundCount);
//        EContext.RPNPushOpenRaund();
//        Status := rprOk;
//      end;
//      token_closeround: begin
//        Dec(RoundCount);
//        if RoundCount < 0 then
//        begin
//          if EContext.EPosition <> ExprLValue then
//            Break;
//
//          ERROR_UNNECESSARY_CLOSED_ROUND;
//        end;
//        EContext.RPNPushCloseRaund();
//        Status := rpOperand;
//      end;
//      token_iif: begin
//        ParseIIFStatement(Scope, EContext);
//        Status := rpOperand;
//      end;
//      token_openblock: begin
//        ParseVector(Scope, EContext);
//        Status := rpOperand;
//      end;
//      token_closeblock: begin
//        if EContext.EPosition = ExprNested then
//          Break
//        else
//          ERROR_UNNECESSARY_CLOSED_BLOCK;
//      end;
//      token_plus: begin
//        if Status = rpOperand then
//          Status := RPNPushOperator(EContext, opAdd)
//        else
//          Status := RPNPushOperator(EContext, opPositive);
//      end;
//      token_minus: begin
//        if Status = rpOperand then
//          Status := RPNPushOperator(EContext, opSubtract)
//        else
//          Status := RPNPushOperator(EContext, opNegative);
//      end;
//      token_equal: begin
//        CheckLeftOperand(Status);
//        Status := RPNPushOperator(EContext, opEqual);
//      end;
//      token_var: begin
//        Result := ParseInplaceVarDecl(Scope, Expr);
//        SContext.IL.AddVariable(Expr.AsVariable);
//        RPNPushExpression(EContext, Expr);
//        Status := rpOperand;
//        continue;
//      end;
////      token_lambda: begin
////        Result := ParseLambdaExpression(Scope, EContext);
////        Status := rpOperand;
////        continue;
////      end;
////      token_bindf: begin
////        Result := ParseBindFunctionExpression(Scope, EContext);
////        Status := rpOperand;
////        continue;
////      end;
//      token_notequal: begin
//        CheckLeftOperand(Status);
//        Status := RPNPushOperator(EContext, opNotEqual);
//      end;
//      token_less: begin
//        CheckLeftOperand(Status);
//        Status := RPNPushOperator(EContext, opLess);
//      end;
//      token_lessorequal: begin
//        CheckLeftOperand(Status);
//        Status := RPNPushOperator(EContext, opLessOrEqual);
//      end;
//      token_above: begin
//        if EContext.EPosition = ExprNestedGeneric then
//          Break;
//        CheckLeftOperand(Status);
//        Status := RPNPushOperator(EContext, opGreater);
//      end;
//      token_aboveorequal: begin
//        CheckLeftOperand(Status);
//        Status := RPNPushOperator(EContext, opGreaterOrEqual);
//      end;
//      token_asterisk: begin
//        CheckLeftOperand(Status);
//        Status := RPNPushOperator(EContext, opMultiply);
//      end;
//      token_in: begin
//        CheckLeftOperand(Status);
//        Status := RPNPushOperator(EContext, opIn);
//      end;
//      token_slash: begin
//        CheckLeftOperand(Status);
//        Status := RPNPushOperator(EContext, opDivide);
//      end;
//      token_div: begin
//        CheckLeftOperand(Status);
//        Status := RPNPushOperator(EContext, opIntDiv);
//      end;
//      token_mod: begin
//        CheckLeftOperand(Status);
//        Status := RPNPushOperator(EContext, opModDiv);
//      end;
//      token_period: begin
//        CheckLeftOperand(Status);
//        //Bool_AddExprNode(EContext, GetILLast(SContext), cGreaterOrEqual); // ???????????????? зачем?
//        Status := RPNPushOperator(EContext, opPeriod);
//      end;
//      token_address: begin
//        Status := RPNPushOperator(EContext, opAddr);
//      end;
//      token_plusplus: begin
//        RPNPushOperator(EContext, opPostInc);
//        Status := rpOperand;
//      end;
//      token_minusminus: begin
//        RPNPushOperator(EContext, opPostDec);
//        Status := rpOperand;
//      end;
//      token_caret: begin
//        RPNPushOperator(EContext, opDereference);
//        Status := rpOperand;
//      end;
//      token_and: begin
//        CheckLeftOperand(Status);
//        Status := RPNPushOperator(EContext, opAnd);
//      end;
//      token_or: begin
//        CheckLeftOperand(Status);
//        Status := RPNPushOperator(EContext, opOr);
//      end;
//      token_xor: begin
//        CheckLeftOperand(Status);
//        Status := RPNPushOperator(EContext, opXor);
//      end;
//      token_not: Status := RPNPushOperator(EContext, opNot);
//      token_shl: begin
//        CheckLeftOperand(Status);
//        Status := RPNPushOperator(EContext, opShiftLeft);
//      end;
//      token_shr: begin
//        CheckLeftOperand(Status);
//        Status := RPNPushOperator(EContext, opShiftRight);
//      end;
//      token_is: begin
//        CheckLeftOperand(Status);
//        Status := RPNPushOperator(EContext, opIs);
//      end;
//      token_as: begin
//        CheckLeftOperand(Status);
//        Status := RPNPushOperator(EContext, opAs);
//      end;
//      token_procedure, token_function: begin
//        if Status = rpOperand then
//          break;
//        Result := ParseAnonymousProc(Scope, EContext, SContext, Result);
//        Status := rpOperand;
//        continue;
//      end;
//      token_inherited: begin
//        //CheckLeftOperand(Status);
//        Result := ParseInheritedStatement(Scope, EContext);
//        Status := rpOperand;
//        continue;
//      end;
//      token_identifier: begin
//        // есил встретился подряд воторой идентификатор, то выходим
//        if Status = rpOperand then
//          Break;
//        if parser_IdentifireType = itIdentifier then
//        begin
//          Result := ParseMember(Scope, Expr, EContext);
//          // если результат = nil значит это был вызов функции и все
//          // необходимые параметры погружены в стек, поэтому идем дальше
//          if not Assigned(Expr) then
//          begin
//            Status := rpOperand;
//            continue;
//          end;
//
//          CheckPureExpression(SContext, Expr);
//
//          case Expr.ItemType of
//            {именованная константа}
//            itConst: status := rpOperand;
//            {переменная}
//            itVar: Status := rpOperand;
//            {тип}
//            itType: begin
//              case Result of
//                {явное преобразование типов}
//                token_openround: begin
//                  Result := ParseExplicitCast(Scope, SContext, Expr);
//                  Status := rpOperand;
//                end;
//                {доступ к члену типа}
//                token_dot: begin
//                  ERROR_FEATURE_NOT_SUPPORTED;
//                  Expr := nil;
//                end;
//                else begin
//                  Status := rpOperand;
//                end;
//              end;
//            end;
//          end;
//        end else begin
//          {анонимная константа}
//          parser_ReadCurrIdentifier(ID);
//          Expr := CreateAnonymousConstant(Scope, EContext, ID, parser_IdentifireType);
//          Result := parser_NextToken(Scope);
//          Status := rpOperand;
//        end;
//        RPNPushExpression(EContext, Expr);
//        Continue;
//      end;
//    else
//      Break;
//    end;
//    Result := parser_NextToken(Scope);
//  end;
//
//  if (EContext.EPosition <> ExprNested) and (Status <> rpOperand) and NeedRValue(EContext.RPNLastOp) then
//    ERROR_EXPRESSION_EXPECTED;
//
//  EContext.RPNFinish();
end;

function TNPUnit.ParseStaticArrayType(Scope: TScope; Decl: TIDArray): TTokenID;
  procedure CheckExpression(Expr: TIDExpression);
  begin
    CheckEmptyExpression(Expr);
    if not ((Expr.ItemType = itConst) or
           ((Expr.ItemType = itType) and (TIDType(Expr.Declaration).Ordinal))) then
      ERROR_ORDINAL_CONST_OR_TYPE_REQURED;
  end;
var
  Expr: TIDExpression;
  Bound: TIDOrdinal;
  DataType: TIDType;
begin
  // todo сделать парсинг многомерного массива вида array of array of ...
  while True do begin
    // нижняя граница/тип/размер
    parser_NextToken(Scope);
    Result := ParseConstExpression(Scope, Expr, ExprNested);
    CheckExpression(Expr);
    if Result = token_period then begin
     Bound := TIDRangeType.CreateAsAnonymous(Scope);
      ParseRangeType(Scope, Expr, TIDRangeType(Bound));
      AddType(Bound);
    end else begin
      if Expr.ItemType = itType then begin
        Bound := TIDOrdinal(Expr.Declaration);
      end else begin
        Bound := TIDRangeType.CreateAsAnonymous(Scope);
        if Expr.Declaration is TIDIntConstant then
        begin
          Bound.LowBound := 0;
          if Expr.AsIntConst.Value <> 0 then
            Bound.HighBound := Expr.AsIntConst.Value - 1
          else
            Bound.HighBound := 0;
        end else begin
          Bound.LowBound := Expr.AsRangeConst.Value.LBExpression.AsConst.AsInt64;
          Bound.HighBound := Expr.AsRangeConst.Value.HBExpression.AsConst.AsInt64;
        end;
        AddType(Bound);
      end;
    end;
    Decl.AddBound(Bound);
    if Result = token_coma then begin
      continue;
    end;
    Break;
  end;
  parser_MatchToken(Result, token_closeblock);
  Result := parser_NextToken(Scope);
  parser_MatchToken(Result, token_of);
  Result := ParseTypeSpec(Scope, DataType);
  Decl.ElementDataType := DataType;
end;

function TNPUnit.ParseAnonymousProc(Scope: TScope; var EContext: TEContext; SContext: PSContext; ProcType: TTokenID): TTokenID;
var
  Parameters: TScope;
  GenericsArgs: TIDTypeList;
  ResultType: TIDType;
  ResultParam: TIDVariable;
  ProcDecl: TIDProcedure;
  VarSpace: TVarSpace;
  Closure: TIDClosure;
  Expr: TIDExpression;
begin
  VarSpace.Initialize;
  Parameters := TProcScope.CreateInDecl(Scope, @VarSpace, nil);

  // создаем Result переменную (тип будет определен позже)
  if ProcType = token_function then
    ResultParam := AddResultParameter(Parameters)
  else
    ResultParam := nil;

  Result := parser_NextToken(Scope);

  // если generic
  if Result = token_less then
    Result := ParseGenericsHeader(Parameters, GenericsArgs);

  // парсим параметры
  if Result = token_openround then
  begin
    ParseParameters(Parameters);
    Result := parser_NextToken(Scope); // move to "token_colon"
  end;

  if ProcType = token_function then
  begin
    parser_MatchToken(Result, token_colon);
    // парсим тип возвращаемого значения
    Result := ParseTypeSpec(Parameters, ResultType);
    ResultParam.DataType := ResultType;
    ResultParam.TextPosition := parser_Position;
  end else
    ResultType := nil;

  parser_MatchToken(Result, token_begin);

  ProcDecl := TIDProcedure.CreateAsAnonymous(FImplScope);
  ProcDecl.VarSpace := VarSpace;
  ProcDecl.ParamsScope := Parameters;
  ProcDecl.EntryScope := Parameters;
  ProcDecl.ExplicitParams := ScopeToVarList(Parameters, IfThen(ProcType = token_procedure, 0, 1));
  ProcDecl.ResultType := ResultType;
  ProcDecl.CreateProcedureTypeIfNeed(Scope);
  {парсим тело анонимной процедуры}
  Result := ParseProcBody(ProcDecl, nil);

  Closure := CheckAndMakeClosure(SContext, ProcDecl);
  if Assigned(Closure) then
  begin
    {если это замыкание, меняем анонимную процедуру на метод замыкания}
    ProcDecl.Struct := Closure;
    ProcDecl.MakeSelfParam;
    ProcDecl.Name := 'Run';
    Expr := EmitCreateClosure(SContext, Closure);
  end else begin
    FImplScope.AddAnonymousProcedure(ProcDecl);
    Expr := TIDExpression.Create(ProcDecl, parser_PrevPosition);
  end;
  RPNPushExpression(EContext, Expr);
end;

function TNPUnit.ParseAsmProc(Scope: TScope): TTokenID;
{var
  ProcName: TIdentifier;
  Parameters: TScope;
  VarSpace: TSpace;
  ForwardDecl: TIDProcDeclaration;}
begin
{  with FParser do begin
    MatchToken(NextToken, token_procedure);
    ForwardDecl := TIDProcDeclaration(Scope.FindID(ID.Name));
    if Assigned(ForwardDecl) then begin
      if Scope = F
      ReadNextIdentifier(ProcName);
      ReadSemicolon;
      Result := NextToken;
    end;

    if Result = token_begin then begin
    MatchToken(NextToken, token_begin);
  end;}
  Result := token_unknown;
end;

function TNPUnit.ParseAsmSpecifier(out Platform: TIDPlatform): TTokenID;
  function FindPlatform(const ID: TIdentifier): TIDPlatform;
  begin
    Result := nil;
    //HWPlatforms.FindPlatform(ID.Name);
    if not Assigned(Result) then
      AbortWork(sUnknownPlatformFmt, [ID.Name], ID.TextPosition);
  end;
var
  ID: TIdentifier;
  PL: TIDPlatform;
begin
  Result := parser_NextToken(nil);
  if Result <> token_openround then
  begin
    ID.Name := 'IL';
    ID.TextPosition := parser_PrevPosition;
    Platform := FindPlatform(ID);
    Exit;
  end;
  Platform := nil;
  while True do begin
    parser_ReadNextIdentifier(nil, ID);
    PL := FindPlatform(ID);
    {if not Assigned(Platform) then
      Platform := PL
    else
      Platform.JoinPlatfrom(PL);}
    Result := parser_NextToken(nil);
    case Result of
      token_coma: continue;
      token_closeround: Exit(parser_NextToken(nil));
      else
        AbortWork(sComaOrCloseRoundExpected, parser_PrevPosition);
    end;
  end;
end;

procedure TNPUnit.ParseASMStatement(Scope: TScope; Platform: TIDPlatform; SContext: PSContext);
begin

end;

function TNPUnit.ParseBCStatements(Scope: TScope; Token: TTokenID; SContext: PSContext): TTokenID;
var
  Level, MaxLevel: Integer;
  LContext: PLContext;
  Instruction: TILInstruction;
  TryBlock: PTryContext;
begin
  if not Assigned(SContext.LContext) then
    AbortWork(sBreakOrContinueAreAllowedOnlyInALoops, parser_Position);
  Result := parser_NextToken(Scope);
  if Result = token_identifier then begin
    if parser_IdentifireType <> itInteger then
      AbortWork(sLoopLevelExprected, parser_Position);
    Level := StrToInt(FParser.OriginalToken);
    Result := parser_NextToken(Scope);
  end else begin
    Level := 0;
  end;
  MaxLevel := 0;
  LContext := SContext.LContext;
  while True do
  begin
    if Level = 0 then
    begin
      // continue
      if Token = token_continue then
        Instruction := TIL.IL_Jmp(FParser.Position.Row, cNone, LContext.BeginInstuction)
      else // break
        Instruction := TIL.IL_JmpNext(FParser.Position.Row, cNone, LContext.EndInstruction);
      {проверка на выход из try... секции}
      TryBlock := SContext.TryBlock;
      while Assigned(TryBlock) do begin
        if TryBlock.Section = SectionFinally then
          AbortWork(sBreakContinueExitAreNotAllowedInFinallyClause, parser_PrevPosition);
        // если добрались до внешней (к циклу) try... секци, выходим
        if TryBlock = LContext.TryContext then
          break;
        TryBlock.AddExit(etCallFinally, Instruction);
        TryBlock := TryBlock.Parent;
      end;
      ILWrite(SContext, Instruction);
      Break;
    end;
    LContext := LContext.Parent;
    if Assigned(LContext) then begin
      Dec(Level);
      Inc(MaxLevel);
    end else
      Break;
  end;
  if Level > 0 then
    AbortWork(sLoopLevelGreaterThenPossibleFmt, [MaxLevel], parser_PrevPosition);
end;

function TNPUnit.ParseTrySection(Scope: TScope; SContext: PSContext): TTokenID;
var
  TryBlock: TTryContext;
  TryBegin: TILTryBegin;
  TryEndInstruction: TILInstruction;
  procedure EmitFinallySectionProlog;
  var
    i: Integer;
    List: TTryContext.TExitList;
    Item: TTryContext.PExitListItem;
    Instruction: TILInstruction;
  begin
    List := TryBlock.ExitList;
    TryEndInstruction := TIL.IL_TryEnd(parser_Line);
    ILWrite(SContext, TryEndInstruction);
    for i := 0 to Length(List) - 1 do
    begin
      Item := @List[i];
      if Item.ExitType = etCallFinally then
        Instruction := TIL.IL_NearCall(TryEndInstruction)
      else
        Instruction := TIL.IL_Jmp(FParser.Position.Row, cNone, TryEndInstruction);
      SContext.IL.InsertBefore(Item.Instruction, Instruction);
    end;
    TryBegin.Destination := TryEndInstruction;
    TryBegin := nil;
  end;
  procedure EmitExceptSectionProlog;
  var
    TryEndInstruction: TILInstruction;
  begin
    TryEndInstruction := TIL.IL_TryEnd(parser_Line);
    TryBegin.Destination := TryEndInstruction;
    TryBegin := nil;
    ILWrite(SContext, TryEndInstruction);
  end;
var
  JumpOnEndSection: TILJmp;
begin
  // создаем новый TryBlock
  TryBegin := TIL.IL_TryBegin(nil, parser_Line);
  ILWrite(SContext, TryBegin);
  // запоминаем предыдущий TryBlock
  TryBlock.Parent := SContext.TryBlock;
  TryBlock.Section := SectionTry;
  // устанавливаем для текущего контекста новый TryBlock
  SContext.TryBlock := @TryBlock;
  parser_NextToken(Scope);
  Result := ParseStatements(Scope, SContext, True);
  JumpOnEndSection := TIL.IL_JmpNext(parser_Line, cNone, nil);
  ILWrite(SContext, JumpOnEndSection);
  case Result of
    {parse EXCEPT section}
    token_except: begin
      EmitExceptSectionProlog;
      FParser.NextToken;
      TryBlock.Section := SectionExcept;
      Result := ParseStatements(Scope, SContext, True);
      parser_MatchToken(Result, token_end);
      Result := parser_NextToken(Scope);
      // to do
    end;
    {parse FINALLY section}
    token_finally: begin
      EmitFinallySectionProlog;
      SContext.IL.InsertBefore(JumpOnEndSection, TIL.IL_NearCall(TryEndInstruction));
      parser_NextToken(Scope);
      TryBlock.Section := SectionFinally;
      Result := ParseStatements(Scope, SContext, True);
      parser_MatchToken(Result, token_end);
      Result := parser_NextToken(Scope);
      case Result of
        token_finally: AbortWork(sSectionFinallyAlreadyDefined, parser_Position);
        token_except: AbortWork(sExceptSectionMustBeDeclareBeforeFinally, parser_Position);
        else begin
          {завершение текущего TryBlock, устанавливаем для текущего контекста предыдущий TryBlock}
          SContext.TryBlock := TryBlock.Parent;
        end;
      end;
      ILWrite(SContext, TIL.IL_Ret(parser_Line));
    end;
    else
      AbortWork(sExceptOrFinallySectionWasMissed, parser_Position);
  end;
  JumpOnEndSection.Destination := SContext.ILLast;
end;

function TNPUnit.ParseStatements(Scope: TScope; SContext: PSContext; IsBlock: Boolean): TTokenID;
var
  EContext, REContext: TEContext;
  NewScope: TScope;
  Platform: TIDPlatform;
begin
  Result := parser_CurTokenID;
  while True do begin
    case Result of
      {BEGIN}
      token_begin: begin
        parser_NextToken(Scope);
        NewScope := TScope.Create(stLocal, Scope);
        Result := ParseStatements(NewScope, SContext, True);
        parser_MatchToken(Result, token_end);
        Result := parser_NextToken(Scope);
        if not IsBlock then
          Exit;
        continue;
      end;
      {END}
      token_end: begin
        exit;
      end;
      {UNTIL}
      token_until: Break;
      {EXIT}
      token_exit: Result := ParseExitStatement(Scope, SContext);
      {IF}
      token_if: Result := ParseIfThenStatement(Scope, SContext);
      {WHILE}
      token_while: Result := ParseWhileStatement(Scope, SContext);
      {REPEAT}
      token_repeat: Result := ParseRepeatStatement(Scope, SContext);
      {WITH}
      token_with: Result := ParseWithStatement(Scope, SContext);
      {USING}
      token_using: Result := ParseUsingStatement(Scope, SContext);
      {CASE}
      token_case: Result := ParseCaseStatement(Scope, SContext);
      {ASM}
      token_asm: begin
        ParseAsmSpecifier(Platform);
        ParseASMStatement(Scope, Platform, SContext);
        Result := parser_NextToken(Scope);
      end;
      {INHERITED}
      token_inherited: begin
        InitEContext(EContext, SContext, ExprLValue);
        Result := ParseInheritedStatement(Scope, EContext);
      end;
      {FOR}
      token_for: Result := ParseForStatement(Scope, SContext);
      {TRY}
      token_try: begin
        Result := ParseTrySection(Scope, SContext);
        if Result <> token_unknown then;
      end;
      {EXCEPT/FINALLY}
      token_except, token_finally: begin
        if not Assigned(SContext.TryBlock) then
          ERROR_TRY_KEYWORD_MISSED;
        Exit;
      end;
      {RAISE}
      token_raise: Result := ParseRaiseStatement(Scope, SContext);
      {BREAK, CONTINUE}
      token_break, token_continue: Result := ParseBCStatements(Scope, Result, SContext);
      token_semicolon:;
      token_unsafe: Result := ParseUnsafeStatement(Scope, SContext);

      {VAR}
      token_var: Result := ParseImmVarStatement(Scope, SContext);

      token_address: begin
        Result := parser_NextToken(Scope);
        Continue;
      end;

      {IDENTIFIER}
      token_identifier: begin
        InitEContext(EContext, SContext, ExprLValue);
        while True do
        begin
          Result := ParseExpression(Scope, EContext, parser_CurTokenID);
          if Result = token_assign then begin
            InitEContext(REContext, SContext, ExprRValue);
            Result := parser_NextToken(Scope);
            Result := ParseExpression(Scope, REContext, Result);
            if Assigned(REContext.LastBoolNode) then
              Bool_CompleteImmediateExpression(REContext, REContext.Result);

            RPNPushExpression(EContext, REContext.Result);
            RPNPushOperator(EContext, opAssignment);
            EContext.RPNFinish();
          end else
          if Result = token_coma then begin
            parser_NextToken(Scope);
            Continue;
          end;
          CheckAndCallFuncImplicit(EContext);
          CheckUnusedExprResult(EContext);
          Break;
        end;
      end;
      token_initialization,
      token_finalization: Break;
      token_eof: Exit;
    else
      if IsBlock then
        ERROR_EXPECTED_KEYWORD_OR_ID;
    end;

    if not Assigned(SContext.LContext) then
      FLoopPool.Clear;

    if IsBlock then
    begin
      parser_MatchToken(Result, token_semicolon);
      Result := parser_NextToken(Scope);
    end else
      Exit;
  end;
end;

function TNPUnit.ParseIfThenStatement(Scope: TScope; SContext: PSContext): TTokenID;
var
  Expression: TIDExpression;
  ToElseJump,
  ToEndJump: TILJmpNext;
  EContext: TEContext;
  ThenSContext: TSContext;
  ElseSContext: TSContext;
  NewScope: TScope;
  IFCondition: (condConstNone, condConstTrue, condConstFalse);
begin
  InitEContext(EContext, SContext, ExprRValue);
  CFBBegin(SContext, CFB_IF);
  Result := ParseExpression(Scope, EContext, parser_NextToken(Scope));
  Expression := EContext.Result;
  CheckEmptyExpression(Expression);
  CheckBooleanExpression(Expression);
  ToElseJump := nil;
  ThenSContext.Assign(SContext);
  ElseSContext.Assign(SContext);
  case Expression.ItemType of
    itVar: if not (Expression is TIDBoolResultExpression) then begin // Если if условие соотоит из одной boolean переменной
      ILWrite(SContext, TIL.IL_Test(Expression, Expression));
      ToElseJump := TIL.IL_JmpNext(Expression.Line, cZero, nil);
      ILWrite(SContext, ToElseJump);
      IFCondition := condConstNone;
    end else
      IFCondition := condConstNone;
    itConst: begin // Если if условие соотоит из константы
      if TIDBooleanConstant(Expression.Declaration).Value then
      begin
        ElseSContext.WriteIL := False;
        IFCondition := condConstTrue;
      end else begin
        ThenSContext.WriteIL := False;
        IFCondition := condConstFalse;
      end;
    end;
  else
    IFCondition := condConstNone;
  end;

  ReleaseExpression(SContext, Expression);

  {then section}
  parser_MatchToken(Result, token_then);
  Result := parser_NextToken(Scope);
  if Result <> token_semicolon then
  begin
    if IFCondition = condConstNone then
      CFBBegin(SContext, CFB_IF_THEN);

    {оптимизация, не создаем лишний scope, если внутри он создастся всеравно}
    if Result <> token_begin then
      NewScope := TScope.Create(stLocal, Scope)
    else
      NewScope := Scope;

    Result := ParseStatements(NewScope, @ThenSContext, False);

    if IFCondition = condConstNone then
      CFBEnd(SContext, CFB_IF_THEN);
  end else begin
    {выходим если if ... then пустой (типа: if x > y then;)}
    Bool_CompleteExpression(EContext.LastBoolNode, SContext.ILLast);
    Exit;
  end;
  { else section}
  if Result = token_else then begin
    if IFCondition = condConstNone then
      CFBBegin(SContext, CFB_IF_ELSE);
    ToEndJump := TIL.IL_JmpNext(parser_Line - 1, cNone, nil); // безусловный прыжок на END секцию
    // если есть then и else секции
    ThenSContext.WriteIL := ThenSContext.WriteIL and ElseSContext.WriteIL;
    ILWrite(@ThenSContext, ToEndJump);
    if Assigned(ToElseJump) then
      ToElseJump.Destination := SContext.ILLast
    else
      Bool_CompleteExpression(EContext.LastBoolNode, SContext.ILLast);
    parser_NextToken(Scope);

    {оптимизация, не создаем лишний scope, если внутри он создастся всеравно}
    if Result <> token_begin then
      NewScope := TScope.Create(stLocal, Scope)
    else
      NewScope := Scope;

    Result := ParseStatements(NewScope, @ElseSContext, False);
    ToEndJump.Destination := SContext.ILLast;
    if IFCondition = condConstNone then
      CFBEnd(SContext, CFB_IF_ELSE);
  end else
    if Assigned(ToElseJump) then
      ToElseJump.Destination := SContext.ILLast
    else
      Bool_CompleteExpression(EContext.LastBoolNode, SContext.ILLast);
  CFBEnd(SContext, CFB_IF);
end;

procedure TNPUnit.ParseIIFStatement(Scope: TScope; var EContext: TEContext);
  procedure ProcessSimpleMove(const EContext: TEContext; Dst, Src: TIDExpression);
  begin
    if (EContext.RPNExprCount = 1) and Assigned(Src.Instruction) and
       (TILInstruction(Src.Instruction) = EContext.SContext.IL.Last) and
       not (Src.Instruction is TILGetPtrMulti) then
    begin
      TILDestInstruction(Src.Instruction).Destination := Dst;
    end else
       ILWrite(EContext.SContext, TIL.IL_Move(Dst, Src));
  end;
var
  Expression: TIDExpression;
  ToElseJump,
  ToEndJump: TILJmpNext;
  InnerEContext, TEEContext: TEContext;
  SContext: PSContext;
  ThenSContext: TSContext;
  ElseSContext: TSContext;
  Result: TTokenID;
  ThenExpr, ElseExpr: TIDExpression;
  ResultExpr: TIDExpression;
begin
  SContext := EContext.SContext;
  parser_MatchToken(parser_NextToken(Scope), token_openround);
  InitEContext(InnerEContext, SContext, ExprRValue);
  Result := ParseExpression(Scope, InnerEContext, parser_NextToken(Scope));
  Expression := RPNPopExpression(InnerEContext);
  CheckEmptyExpression(Expression);
  CheckBooleanExpression(Expression);
  ToElseJump := nil;
  ThenSContext.Assign(SContext);
  ElseSContext.Assign(SContext);
  case Expression.ItemType of
    itVar: if not Expression.IsAnonymous then begin // Если if условие соотоит из одной boolean переменной
      ILWrite(SContext, TIL.IL_Test(Expression, Expression));
      ToElseJump := TIL.IL_JmpNext(Expression.Line, cZero, nil);
      ILWrite(SContext, ToElseJump);
    end;
    itConst: begin // Если if условие соотоит из константы
      if TIDBooleanConstant(Expression.Declaration).Value then
        ElseSContext.WriteIL := False
      else
        ThenSContext.WriteIL := False;
    end;
  end;

  // результат, временная переменная будет подствлена позже
  ResultExpr := TIDExpression.Create(nil);

  {then section}
  parser_MatchToken(Result, token_coma);
  InitEContext(TEEContext, @ThenSContext, ExprRValue);
  Result := ParseExpression(Scope, TEEContext, parser_NextToken(Scope));
  ThenExpr := RPNPopExpression(TEEContext);
  CheckEmptyExpression(ThenExpr);
  // выводим тип результата
  ResultExpr.Declaration := GetTMPVar(SContext, ThenExpr.DataType);
  // присваиваем
  ProcessSimpleMove(TEEContext, ResultExpr, ThenExpr);

  // безусловный прыжок на END секцию
  ToEndJump := TIL.IL_JmpNext(Expression.Line, cNone, nil);
  // если есть then и else секции
  ThenSContext.WriteIL := ThenSContext.WriteIL and ElseSContext.WriteIL;
  ILWrite(@ThenSContext, ToEndJump);

  { else section}
  parser_MatchToken(Result, token_coma);
  if Assigned(ToElseJump) then
    ToElseJump.Destination := SContext.ILLast
  else
    Bool_CompleteExpression(InnerEContext.LastBoolNode, SContext.ILLast);
  InitEContext(TEEContext, @ElseSContext, ExprNested);
  Result := ParseExpression(Scope, TEEContext, parser_NextToken(Scope));
  ElseExpr := RPNPopExpression(TEEContext);
  CheckEmptyExpression(ElseExpr);
  // проверяем тип "else" выражения
  if not Assigned(CheckImplicit(ElseExpr, ResultExpr.DataType)) then
    AbortWork(sTypesMustBeIdentical, parser_PrevPosition);
  // присваиваем
  ProcessSimpleMove(TEEContext, ResultExpr, ElseExpr);

  // проверяем на идентичность "then" и "else" секций
  if (ThenExpr.Declaration = ElseExpr.Declaration) or
     ((ThenExpr.ItemType = itConst) and (ElseExpr.ItemType = itConst) and
      (TIDConstant(ThenExpr.Declaration).CompareTo(TIDConstant(ElseExpr.Declaration)) = 0)) then
  begin
    Warning(msgThenAndElseSectionAreIdentical, [], parser_Position);
  end;
  parser_MatchToken(Result, token_closeround);
  ResultExpr.TextPosition := parser_Position;
  RPNPushExpression(EContext, ResultExpr);
  ToEndJump.Destination := SContext.ILLast;
end;

function TNPUnit.ParseForInStatement(Scope: TScope; SContext: PSContext; LoopVar: TIDExpression): TTokenID;
var
  EContext: TEContext;
  AExpr: TIDExpression;
  LoopArrayDT: TIDType;
  ForInIndex: TIDVariable;
  ForInIndexE: TIDExpression;
  ALength: TIDExpression;
  FirstInstruction,
  LastInstruction,
  GetPtrInstruction: TILInstruction;
  Expr: TIDExpression;
  ExitCond: TILCondition;
begin
  // парсим выражение-коллекцию
  InitEContext(EContext, SContext, ExprRValue);
  Result := ParseExpression(Scope, EContext, parser_NextToken(Scope));
  AExpr := EContext.Result;
  CheckArrayExpression(AExpr);
  LoopArrayDT := (AExpr.DataType as TIDArray).ElementDataType;

  ForInIndex := GetTMPVar(SContext, SYSUnit._Int32);
  ForInIndex.IncludeFlags([VarLoopIndex]);
  ForInIndexE := TIDExpression.Create(ForInIndex, parser_Position);

  if Assigned(LoopVar.DataType) then
  begin
    // если переменная цикла определена зарание
    if MatchImplicit(LoopArrayDT, LoopVar.DataType) = nil then
      ERROR_INCOMPATIBLE_TYPES(LoopVar, LoopArrayDT);
    Expr := TIDMultiExpression.Create(TIDExpressions.create(AExpr, ForInIndexE), AExpr.TextPosition);
    GetPtrInstruction := TIL.IL_Move(LoopVar, Expr);
    LoopVar.AsVariable.IncludeFlags([VarLoopIndex]);
  end else begin
    LoopVar.Declaration.DataType := LoopArrayDT;
    LoopVar.AsVariable.IncludeFlags([VarLoopIndex, VarInOut]); // !!!
    GetPtrInstruction := TIL.IL_GetPtr(LoopVar, AExpr, ForInIndexE);
  end;

  ExitCond := cGreater;
  if AExpr.IsDynArrayConst then
    ALength := IntConstExpression(AExpr.AsDynArrayConst.ArrayLength)
  else
  if AExpr.DataType.DataTypeID = dtStaticArray then
    ALength := IntConstExpression(TIDArray(AExpr.DataType).Dimensions[0].HighBound) // пока так, там будет видно
  else begin
    ALength := GetTMPVarExpr(SContext, SYSUnit._Int32);
    ILWrite(SContext, TIL.IL_Length(ALength, AExpr));
    ExitCond := cGreaterOrEqual;
  end;

  // инициализация индексной переменной
  ILWrite(SContext, TIL.IL_Move(ForInIndexE, SYSUnit._ZeroExpression));

  // первая инструкция тела цикла
  FirstInstruction := TIL.IL_Cmp(ForInIndexE, ALength);

  // последняя инструкция тела цикла
  LastInstruction := TIL.IL_Jmp(parser_Line, cNone, FirstInstruction);

  // первичная проверка условия цикла
  ILWrite(SContext, FirstInstruction);
  ILWrite(SContext, TIL.IL_JmpNext(parser_Line, ExitCond, LastInstruction));

  parser_MatchToken(Result, token_do);
  parser_NextToken(Scope);

  // получение цикловой переменной:
  ILWrite(SContext, GetPtrInstruction);

  // парсим тело
  Result := ParseStatements(Scope, SContext, False);

  // окончание тела
  ILWrite(SContext, TIL.IL_Add(ForInIndexE, ForInIndexE, SYSUnit._OneExpression));
  ILWrite(SContext, LastInstruction);
end;

function TNPUnit.ParseForStatement(Scope: TScope; SContext: PSContext): TTokenID;
var
  EContext: TEContext;
  ToEndJump: TILJmpNext;
  LContext: PLContext;
  NewSContext: TSContext;
  ID: TIdentifier;
  LoopVar: TIDDeclaration;
  LExpr, StartExpr, StopExpr: TIDExpression;
  JMPCondition: TILCondition;
  DeltaExpr: TIDExpression;
  Instruction: TILInstruction;
  NewScope: TScope;
  CmpInstr: TILInstruction;
begin
  LContext := PLContext(FLoopPool.Add);
  LContext.TryContext := SContext.TryBlock;
  LContext.Parent := SContext.LContext;
  NewSContext.Assign(SContext);
  NewSContext.LContext := LContext;
  Instruction := SContext.ILLast;
  CFBBegin(SContext, CFB_FOR_BODY);

  // цикловая переменная
  Result := parser_NextToken(Scope);
  if Result = token_var then begin
    parser_ReadNextIdentifier(Scope, ID);
    NewScope := TScope.Create(stLocal, Scope);
    LoopVar := TIDVariable.Create(NewScope, ID);
    NewScope.AddVariable(TIDVariable(LoopVar));
    SContext.IL.AddVariable(TIDVariable(LoopVar));
    Scope := NewScope;
  end else begin
    parser_ReadCurrIdentifier(ID);
    LoopVar := FindID(Scope, ID);
  end;

  InitEContext(EContext, SContext, ExprRValue);
  // заталкиваем в стек левое выражение
  LExpr := TIDExpression.Create(LoopVar, ID.TextPosition);
  RPNPushExpression(EContext, LExpr);

  Result := parser_NextToken(Scope);

  {если это цикл for ... in ...}
  if Result = token_in then
  begin
    Result := ParseForInStatement(Scope, SContext, LExpr);
    Exit;
  end;

  parser_MatchToken(Result, token_assign);

  if LoopVar.DataType = nil then
    LoopVar.DataType := SYSUnit._Int32
  else
  if (LoopVar.ItemType <> itVar) or not (LoopVar.DataTypeID in [dtInt32, dtUInt64]) then
    AbortWork(sForLoopIndexVarsMastBeSimpleIntVar, parser_Position);

  // начальное значение
  Result := ParseExpression(Scope, EContext, parser_NextToken(Scope));
  StartExpr := EContext.Result;
  CheckEmptyExpression(StartExpr);

  // пишем инструкцию присваениея начального значения
  RPNPushOperator(EContext, opAssignment);
  EContext.RPNFinish();

  // устанавливаем флаг цикловой переменной
  with TIDVariable(LoopVar) do Flags := Flags + [VarLoopIndex];

   // to/downto keyword
  case Result of
    token_to: JMPCondition := cGreater;
    token_downto: JMPCondition := cLess;
    else begin
      AbortWork(sKeywordToOrDowntoExpected, parser_PrevPosition);
      JMPCondition := cNone;
    end;
  end;
  // инструкция выхода за цикл по завершению условия
  ToEndJump := TIL.IL_JmpNext(parser_Line, JMPCondition, nil);

  // конечное значение
  InitEContext(EContext, SContext, ExprRValue);
  Result := ParseExpression(Scope, EContext, parser_NextToken(Scope));
  StopExpr := EContext.Result;
  CheckEmptyExpression(StopExpr);

  // если для вычисления конечного выражения использовалась временная переменная
  // помечаем ее как постоянно используемую в блоке FOR цикла
  if StopExpr.IsTMPVar then
    StopExpr.AsVariable.IncludeFlags([VarLoopIndex]);

  // пишем инструкцию сравнение
  CmpInstr := TIL.IL_Cmp(LExpr, StopExpr);
  ILWrite(SContext, CmpInstr);
  ILWrite(SContext, ToEndJump);

  // проверка на константы
  if (StartExpr.ItemType = itConst) and
     (StopExpr.ItemType = itConst) then
  begin
    NewSContext.WriteIL := ((JMPCondition = cGreater) and (StartExpr.AsIntConst.Value <= StopExpr.AsIntConst.Value)) or
                           ((JMPCondition = cLess) and (StartExpr.AsIntConst.Value >= StopExpr.AsIntConst.Value));
    if not NewSContext.WriteIL then begin
      ILDelete(SContext, Instruction.Next, SContext.ILLast);
      Warning(msgForOrWhileLoopExecutesZeroTimes, [], LoopVar.SourcePosition);
    end;
  end;

  // шаг приращения
  if Result = token_step then begin
    InitEContext(EContext, SContext, ExprRValue);
    Result := ParseExpression(Scope, EContext, parser_NextToken(Scope));
    DeltaExpr := EContext.Result;
    CheckEmptyExpression(DeltaExpr);
    if (DeltaExpr.ItemType = itConst) and (DeltaExpr.AsIntConst.Value = 0) then
      Warning(msgZeroDeltaInForLoop, [], parser_PrevPosition);
  end else
    DeltaExpr := SYSUnit._OneExpression;

  // последняя инструкция цикла
  Instruction := TIL.IL_Jmp(parser_Line, cNone, CmpInstr);
  LContext.EndInstruction := Instruction;
  ToEndJump.Destination := Instruction;

  // пересоздаем выражение, для корректного отображения в дебагере
  LExpr := TIDExpression.Create(LoopVar);

  // циклическое приращение (первая инструкция цикла)
  if JMPCondition = cGreater then
    Instruction := TIL.IL_Add(LExpr, LExpr, DeltaExpr)
  else
    Instruction := TIL.IL_Sub(LExpr, LExpr, DeltaExpr);
  LContext.BeginInstuction := Instruction;

  // тело цикла
  parser_MatchToken(Result, token_do);
  Result := parser_NextToken(Scope);
  if Result <> token_semicolon then
    Result := ParseStatements(Scope, @NewSContext, False);
  // если тело цикла пустое, цикл будет работать в холостую, возможно следует его вообще удалить(с хинтом)?

  // проставляем позиции инструкций
  LExpr.TextPosition := parser_Position;
  (LContext.EndInstruction as TILJmp).Line := parser_Line;

  // пишем последние 2 инструкции цикла
  ILWrite(@NewSContext, Instruction);
  ILWrite(@NewSContext, LContext.EndInstruction);

  // сбрасываем флаг цикловой переменной
  with TIDVariable(LoopVar) do Flags := Flags - [VarLoopIndex];
  CFBEnd(SContext, CFB_FOR_BODY);

  ReleaseExpression(SContext, StartExpr);
  ReleaseExpression(SContext, StopExpr);
  ReleaseExpression(SContext, DeltaExpr);
end;

function TNPUnit.ParseGenericsArgs(Scope: TScope; SContext: PSContext; out Args: TIDExpressions): TTokenID;
//var
//  EContext: TEContext;
//  Expr: TIDExpression;
//  ArgsCount: Integer;
begin
  Assert(False);
//  ArgsCount := 0;
//  while true do begin
//    InitEContext(EContext, SContext, ExprNestedGeneric);
//    Result := ParseExpression(Scope, EContext, parser_NextToken(Scope));
//    Expr := EContext.Result;
//    if Assigned(Expr) then begin
//      if Expr.DataType = SYSUnit._Boolean then
//      begin
//        if (Expr.ItemType = itVar) and Expr.IsAnonymous then
//          Bool_CompleteImmediateExpression(EContext, Expr);
//      end;
//    end else
//      ERROR_EXPRESSION_EXPECTED;
//
//    Inc(ArgsCount);
//    SetLength(Args, ArgsCount);
//    Args[ArgsCount - 1] := RPNPopExpression(EContext);
//
//    case Result of
//      token_coma: begin
//        continue;
//      end;
//      token_above: begin
//        Result := parser_NextToken(Scope);
//        Break;
//      end;
//      else
//        AbortWork(sIncompleteStatement, parser_PrevPosition);
//    end;
//  end;
end;

function TNPUnit.ParseGenericsHeader(Params: TScope; out Args: TIDTypeList): TTokenID;
var
  ID: TIdentifier;
  ParamsCount, ComaCount: Integer;
  TypeDecl: TIDGenericType;
begin
  ComaCount := 0;
  ParamsCount := 0;
  while True do begin
    Result := parser_NextToken(Params);
    case Result of
      token_identifier: begin
        parser_ReadCurrIdentifier(ID);
        {данный обобщенный-тип не добавляется в общий пул типов}
        TypeDecl := TIDGenericType.Create(Params, ID);
        InsertToScope(Params, TypeDecl);
        Inc(ParamsCount);
        SetLength(Args, ParamsCount);
        Args[ParamsCount - 1] := TypeDecl;
      end;
      token_coma: begin
        if ComaCount >= ParamsCount then
          ERROR_IDENTIFIER_EXPECTED();
        Inc(ComaCount);
      end;
      token_above: begin
        if ParamsCount = 0 then
          AbortWork(sNoOneTypeParamsWasFound, parser_PrevPosition);

        if ComaCount >= ParamsCount then
          ERROR_IDENTIFIER_EXPECTED();
        Break;
      end
    else
      ERROR_IDENTIFIER_EXPECTED();
    end;
  end;
  Result := parser_NextToken(Params);
end;

function TNPUnit.ParseWhileStatement(Scope: TScope; SContext: PSContext): TTokenID;
var
  Expression: TIDExpression;
  EContext: TEContext;
  ToEndJump: TILJmpNext;
  ToBeginJump: TILJmp;
  LContext: PLContext;
  NewSContext: TSContext;
  LastInstr: TILInstruction;
begin
  LContext := PLContext(FLoopPool.Add);
  LContext.TryContext := SContext.TryBlock;
  LContext.Parent := SContext.LContext;
  LastInstr := SContext.ILLast;
  CFBBegin(SContext, CFB_WHILE_BODY);

  // while выражение
  InitEContext(EContext, SContext, ExprRValue);
  Result := ParseExpression(Scope, EContext, FParser.NextToken);
  Expression := EContext.Result;
  CheckEmptyExpression(Expression);
  CheckBooleanExpression(Expression);
  ToEndJump := nil;
  NewSContext.Assign(SContext);
  NewSContext.LContext := LContext;
  case Expression.ItemType of
    itVar: if not (Expression is TIDBoolResultExpression) then
    begin // Если if условие соотоит из одной boolean переменной
      ILWrite(SContext, TIL.IL_Test(Expression, Expression));
      ToEndJump := TIL.IL_JmpNext(Expression.Line, cZero, nil);
      ILWrite(SContext, ToEndJump);
    end;
    itConst: begin
      if Expression.AsBoolConst.Value = False then
        NewSContext.WriteIL := False
      else
        // для while true do ... если это первая строчка в коде
        // добавляем инструкцию nope чтобы позже на нее можно было сослаться
        ILWrite(SContext, TIL.IL_Nope);
    end;
  end;

  // первая инструкция условия цикла (для continue)
  if Assigned(LastInstr) then
    LContext.BeginInstuction := LastInstr.Next
  else
    LContext.BeginInstuction := SContext.ILLast;

  // последняя инструкция цикла (безусловный джамп на начало цикла (необходим для break))
  ToBeginJump := TIL.IL_Jmp(0, cNone, LContext.BeginInstuction);
  LContext.EndInstruction := ToBeginJump;

  // тело цикла
  FParser.MatchToken(Result, token_do);
  Result := FParser.NextToken;
  if Result <> token_semicolon then
  begin
    Result := ParseStatements(Scope, addr(NewSContext), False);
    ToBeginJump.Line := parser_Line;
  end else begin
    CFBEnd(SContext, CFB_WHILE_BODY);
    Exit;
  end;

  ILWrite(addr(NewSContext), LContext.EndInstruction);
  if ToBeginJump.Destination = nil then
    ToBeginJump.Destination := LContext.BeginInstuction;

  if not Assigned(ToEndJump) then
    Bool_CompleteExpression(EContext.LastBoolNode, ToBeginJump)
  else
    ToEndJump.Destination := NewSContext.ILLast;

  CFBEnd(SContext, CFB_WHILE_BODY);
end;

function TNPUnit.ParseWithStatement(Scope: TScope; SContext: PSContext): TTokenID;
var
  ID: TIdentifier;
  Decl: TIDDeclaration;
  EContext: TEContext;
  Expression, Expr: TIDExpression;
  WNextScope: TWithScope;
  WPrevScope: TScope;
  TmpVar: TIDDeclaration;
begin
  WPrevScope := Scope;
  WNextScope := nil;
  while True do begin
    Result := parser_NextToken(Scope);
    TmpVar := nil;
    Expression := TIDExpression.Create(nil);
    InitEContext(EContext, SContext, ExprRValue);
    RPNPushExpression(EContext, Expression);
    parser_MatchToken(Result, token_identifier);
    Result := ParseExpression(Scope, EContext, Result);
    // Если выражение простое, удаляем временное выражение
    if EContext.Result.ExpressionType = etDeclaration then
    begin
      Expression := EContext.Result;
    end else begin
    // Если выражение сложное, создаем временную переменную и присваеваем ей вычисленное выше выражение
      //RPNPushExpression(EContext.ResultExpression);
      TmpVar := SContext.Proc.GetTMPVar(EContext.Result.DataType, True);
      Expression.Declaration := TmpVar;
      Process_operator_Assign(EContext);
    end;
    // Проверка что тип сложный
    if not Expression.DataType.InheritsFrom(TIDStructure) then
      AbortWork(sStructTypeRequired, parser_PrevPosition);

    Decl := Expression.Declaration;
    // проверка на повторное выражение/одинаковый тип
    while Assigned(WNextScope) and (WNextScope.ScopeType = stWithScope) do begin
      Expr := WNextScope.Expression;
      if Expr.Declaration = Decl then
        AbortWork('Duplicate expression', parser_PrevPosition);
      if Expr.DataType = Decl.DataType then
        AbortWork('Duplicate expressions types', parser_PrevPosition);
      WNextScope := TWithScope(WNextScope.OuterScope);
    end;
    // создаем специальный "WITH Scope"
    WNextScope := TWithScope.Create(Scope, Expression);
    Scope.AddScope(WNextScope);
    with WNextScope do begin
      OuterScope := WPrevScope;
      InnerScope := TIDStructure(Decl.DataType).Members;
    end;

    // алиас
    if Result = token_identifier then
    begin
      parser_ReadCurrIdentifier(ID);
      if Assigned(TmpVar) then
        TmpVar.ID := ID
      else begin
        TmpVar := TIDDeclarationClass(Decl.ClassType).Create(Decl.Scope, ID);
        TmpVar.DataType := Decl.DataType;
        TmpVar.Index := Decl.Index;
      end;
      InsertToScope(WNextScope, TmpVar);
      Result := parser_NextToken(Scope);
    end;

    case Result of
      token_coma: begin
        WPrevScope := WNextScope;
        Continue;
      end;
      token_do: begin
        parser_NextToken(Scope);
        Result := ParseStatements(WNextScope, SContext, False);
        Break;
      end;
      else begin
        parser_MatchToken(Result, token_do);
      end;
    end;
  end;
end;

procedure TNPUnit.PostCompileProcessUnit;
//var
//  T: TIDType;
begin
//  {провека неиспользуемых глобальных переменных}
//  CheckUnusedVariables(@FVarSpace);
//
//  T := FTypeSpace.First;
//  while Assigned(T) do
//  begin
//    if T is TIDStructure then
//    begin
//      TIDStructure(T).Fields.Reindex; // нужно будет это убрать от сюда !
//      TIDStructure(T).Methods.Reindex;// нужно будет это убрать от сюда !
//    end;
//
//    if (T.ImportLib = 0) and (T.DataTypeID in [dtRecord, dtClass]) then
//      CheckIncompletedProcs(TIDRecord(T).Members.ProcSpace);
//
//    if (T.DataTypeID = dtClass) and (TIDClass(T).InterfacesCount > 0) then
//      CheckIncompletedIntfProcs(TIDClass(T));
//
//    T := TIDType(T.NextItem);
//  end;
//
//  {провека предварительно определенных типов}
//  CheckIncompleteFwdTypes;
//  CheckIncompletedProcs(addr(FProcSpace));
//
//
//  CheckManagedInitFinalUnit;
//  CheckProc(FInitProc);
//  CheckProc(FFinalProc);
//
//  CheckAndDelGenericTypes(FTypeSpace);
end;

function TNPUnit.ParseEntryCall(Scope: TScope; SContext: PSContext; out Args: TIDExpressions): TTokenID;
var
  ArgumentsCount: Integer;
  Expr: TIDExpression;
  InnerEContext: TEContext;
begin
  ArgumentsCount := 0;
  InitEContext(InnerEContext, SContext, ExprNested);
  {цикл парсинга аргументов}
  while true do begin
    Result := parser_NextToken(Scope);
    if Result = token_closeround then
    begin
      Result := parser_NextToken(Scope);
      Break;
    end;
    Result := ParseExpression(Scope, InnerEContext, Result);
    Expr := InnerEContext.Result;

    if Assigned(Expr) then begin
      if Expr.DataType = SYSUnit._Boolean then
      begin
        if (Expr.ItemType = itVar) and Expr.IsAnonymous then
          Bool_CompleteImmediateExpression(InnerEContext, Expr);
      end;
    end;

    Inc(ArgumentsCount);
    if ArgumentsCount > Length(Args) then
      SetLength(Args, ArgumentsCount);

    Args[ArgumentsCount - 1] := Expr;

    case Result of
      token_coma: begin
        InnerEContext.Reset;
        continue;
      end;
      token_closeround: begin
        Result := parser_NextToken(Scope);
        Break;
      end;
    else
      ERROR_INCOMPLETE_STATEMENT;
    end;
  end;
end;

function TNPUnit.ParseEntryCall(Scope: TScope; CallExpr: TIDCallExpression; var EContext: TEContext): TTokenID;
var
  ArgumentsCount: Integer;
  Expr: TIDExpression;
  InnerEContext: TEContext;
  SContext: PSContext;
begin
  ArgumentsCount := 0;
  SContext := EContext.SContext;
  InitEContext(InnerEContext, SContext, ExprNested);
  {цикл парсинга аргументов}
  while true do begin
    Result := parser_NextToken(Scope);
    if Result = token_closeround then
    begin
      Result := parser_NextToken(Scope);
      Break;
    end;
    Result := ParseExpression(Scope, InnerEContext, Result);
    Expr := InnerEContext.Result;
    if Assigned(Expr) then begin
      if Expr.DataType = SYSUnit._Boolean then
      begin
        if (Expr.ItemType = itVar) and Expr.IsAnonymous then
          Bool_CompleteImmediateExpression(InnerEContext, Expr);
      end;
      RPNPushExpression(EContext, Expr);
    end else begin
      // Добавляем пустой Expression для значения по умолчанию
      RPNPushExpression(EContext, nil);
    end;
    Inc(ArgumentsCount);
    case Result of
      token_coma: begin
        InnerEContext.Reset;
        continue;
      end;
      token_closeround: begin
        Result := parser_NextToken(Scope);
        Break;
      end;
    else
      ERROR_INCOMPLETE_STATEMENT;
    end;
  end;

  if pfDestructor in CallExpr.AsProcedure.Flags then
    ERROR_DESTRUCTOR_CANNOT_BE_CALL_DIRECTLY;

  TIDCallExpression(CallExpr).ArgumentsCount := ArgumentsCount;
  RPNPushExpression(EContext, CallExpr);
  RPNPushOperator(EContext, opCall);
end;

function TNPUnit.ParseBuiltinCall(Scope: TScope; CallExpr: TIDExpression; var EContext: TEContext): TTokenID;
//var
//  FuncDecl: TIDBuiltInFunction;
//  ArgsCount: Integer;
//  Expr: TIDExpression;
//  InnerEContext: TEContext;
//  MacroID: TBuiltInFunctionID;
//  SContext: PSContext;
//  MacroParams: TVariableList;
//  MParam: TIDVariable;
//  i: Integer;
//  ParamsBeginPos: Integer;
//  ParamsBeginRow: Integer;
//  ParamsText: string;
//  Ctx: TSysFunctionContext;
begin
  Assert(False);
//  ArgsCount := 0;
//  Result := parser_CurTokenID;
//  SContext := EContext.SContext;
//  FuncDecl := TIDBuiltInFunction(CallExpr.Declaration);
//  if FuncDecl.FunctionID = bf_userdefined then
//  begin
//    Result := ParseUserDefinedMacroCall(Scope, CallExpr, EContext);
//    Exit;
//  end;
//
//  // парсинг аргументов
//  if Result = token_openround then
//  begin
//    ParamsBeginPos := FParser.SourcePosition;
//    ParamsBeginRow := FParser.LinePosition.Row;
//      InitEContext(InnerEContext, SContext, ExprNested);
//    while True do begin
//      Result := ParseExpression(Scope, InnerEContext, parser_NextToken(Scope));
//      Expr := InnerEContext.Result;
//      if Assigned(Expr) then begin
//        if Expr.DataType = SYSUnit._Boolean then
//        begin
//          if (Expr.ItemType = itVar) and Expr.IsAnonymous then
//            Bool_CompleteImmediateExpression(InnerEContext, Expr);
//        end;
//        Inc(ArgsCount);
//        RPNPushExpression(EContext, Expr);
//      end else begin
//        // Добавляем пустой Expression для значения по умолчанию
//        if FuncDecl.ParamsCount > 0 then
//          RPNPushExpression(EContext, nil);
//      end;
//      case Result of
//        token_coma: begin
//          InnerEContext.Reset;
//          continue;
//        end;
//        token_closeround: begin
//          ParamsText := Copy(FParser.Source, ParamsBeginPos, FParser.SourcePosition - ParamsBeginPos - 1);
//          Result := parser_NextToken(Scope);
//          Break;
//        end;
//      else
//        ERROR_INCOMPLETE_STATEMENT;
//      end;
//    end;
//  end else begin
//    ParamsBeginRow := 0;
//  end;
//
//  MacroParams := FuncDecl.ExplicitParams;
//  for i := 0 to FuncDecl.ParamsCount - 1 do
//  begin
//    MParam := MacroParams[i];
//    if Assigned(MParam.DefaultValue) and (ArgsCount < FuncDecl.ParamsCount) then
//    begin
//      Inc(ArgsCount);
//      RPNPushExpression(EContext, MParam.DefaultValue);
//    end;
//  end;
//
//  // проверка кол-ва аргуметнов
//  if ArgsCount > FuncDecl.ParamsCount then
//    ERROR_TOO_MANY_ACTUAL_PARAMS(CallExpr)
//  else if ArgsCount < FuncDecl.ParamsCount then
//    ERROR_NOT_ENOUGH_ACTUAL_PARAMS(CallExpr);
//
//  Ctx.UN := Self;
//  Ctx.Scope := Scope;
//  Ctx.ParamsStr := ParamsText;
//  Ctx.EContext := @EContext;
//  Ctx.SContext := SContext;
//
//  MacroID := FuncDecl.FunctionID;
//  case MacroID of
//    bf_sysrtfunction: Expr := TIDSysRuntimeFunction(FuncDecl).Process(EContext);
//    bf_sysctfunction: Expr := TIDSysCompileFunction(FuncDecl).Process(Ctx);
//    bf_assigned: Expr := ProcessBuiltin_Assigned(SContext, EContext);
//    bf_inc, bf_dec: Expr := ProcessBuiltin_IncDec(EContext, MacroID);
//    bf_length: Expr := ProcessBuiltin_Length(EContext);
//    bf_setlength: Expr := ProcessBuiltin_SetLength(EContext);
//    bf_copy: Expr := ProcessBuiltin_Copy(EContext);
//    bf_move: Expr := ProcessBuiltin_Move(EContext);
//    bf_memset: Expr := ProcessBuiltin_MemSet(EContext);
//    bf_sizeof: Expr := ProcessBuiltin_SizeOf(EContext);
//    bf_assert: Expr := ProcessBuiltin_Assert(EContext, ParamsText, ParamsBeginRow);
//    bf_typename: Expr := ProcessBuiltin_TypeName(EContext);
//    bf_current_unit: Expr := StrConstExpression(FUnitName.Name);
//    bf_current_function: Expr := StrConstExpression(SContext.Proc.Name);
//    bf_current_line: Expr := IntConstExpression(FParser.Position.Row);
//    bf_new: Expr := ProcessBuiltin_New(EContext);
//    bf_free: Expr := ProcessBuiltin_Free(EContext);
//    bf_getref: Expr := ProcessBuiltin_GetRef(EContext);
//    bf_typeinfo: Expr := ProcessBuiltin_TypeInfo(EContext);
//    bf_LoBound: Expr := ProcessBuiltin_LoHiBound(EContext, False);
//    bf_HiBound: Expr := ProcessBuiltin_LoHiBound(EContext, True);
//    bf_Ord: Expr := ProcessBuiltin_Ord(EContext);
//    bf_include: Expr := ProcessBuiltin_Include(EContext);
//    bf_exclude: Expr := ProcessBuiltin_Exclude(EContext);
//    bf_refcount: Expr := ProcessBuiltin_RefCount(EContext);
//    bf_getbit: Expr := ProcessBuiltin_GetBit(EContext);
//    bf_setbit: Expr := ProcessBuiltin_SetBit(EContext);
//  else
//    AbortWork('Unknown macro %d', [Integer(MacroID)], FParser.Position);
//    Expr := nil;
//  end;
//  if Assigned(Expr) then begin
//    Expr.TextPosition := CallExpr.TextPosition;
//    RPNPushExpression(EContext, Expr);
//  end;
end;

function TNPUnit.ParseUserDefinedMacroCall(Scope: TScope; CallExpr: TIDExpression; var EContext: TEContext): TTokenID;
//var
//  Macro: TIDUserDefinedMacro;
//  ArgsCount: Integer;
//  Expr: TIDExpression;
//  InnerEContext: TEContext;
//  SContext: PSContext;
//  MacroParams: TVariableList;
//  MParam: TIDVariable;
//  i, APos: Integer;
//  AText: string;
//  Args: TMacroArgs;
//  Arg: TIDMacroArgument;
//  NewScope: TScope;
//  PrevPos: TParserPosition;
//  VarSpace: TVarSpace;
//  PrevWriteIL, IsInvalidArg: Boolean;
begin
  Assert(False);
//  ArgsCount := 0;
//  Result := parser_CurTokenID;
//  SContext := EContext.SContext;
//  Macro := CallExpr.AsUserDefinedMacro;
//  IsInvalidArg := False;
//  VarSpace.Initialize;
//  NewScope := TScope.Create(stLocal, addr(VarSpace), Scope.ProcSpace, Scope, Scope.DeclUnit);
//
//  Args := TMacroArgs.Create;
//  try
//    // парсинг аргументов
//    if Result = token_openround then
//    begin
//      PrevWriteIL := SContext.WriteIL;
//      SContext.WriteIL := False;
//      InitEContext(InnerEContext, SContext, ExprNested);
//      while true do begin
//        Expr := nil;
//        APos := FParser.SourcePosition;
//        FParser.SaveState(PrevPos);
//        try
//          Result := ParseExpression(Scope, InnerEContext, parser_NextToken(Scope));
//          Expr := InnerEContext.Result;
//          IsInvalidArg := False;
//        except
//          on e: ECompilerAbort do begin
//            Result := parser_CurTokenID;
//            IsInvalidArg := True;
//          end;
//          on e: Exception do raise;
//        end;
//
//        if Assigned(Expr) then
//        begin
//          if Expr.DataType = SYSUnit._Boolean then
//          begin
//            if (Expr.ItemType = itVar) and Expr.IsAnonymous then
//              Bool_CompleteImmediateExpression(InnerEContext, Expr);
//          end;
//          //RPNPopExpression(InnerEContext); // убираем из стека
//        end;
//
//        if Assigned(Expr) or IsInvalidArg then
//        begin
//          AText := FParser.GetSubString(APos, FParser.SourcePosition - 1);
//          Arg := TIDMacroArgument.Create(NewScope, Identifier(''));
//          Arg.Text := Trim(AText);
//          Arg.ArgExpession := Expr;
//          Args.Add(Arg);
//          Inc(ArgsCount);
//        end;
//
//        case Result of
//          token_coma: begin
//            InnerEContext.Reset;
//            continue;
//          end;
//          token_closeround: begin
//            Result := parser_NextToken(Scope);
//            Break;
//          end;
//        else
//          ERROR_INCOMPLETE_STATEMENT;
//        end;
//      end;
//      SContext.WriteIL := PrevWriteIL;
//    end;
//
//    // создаем macro-аргументы на основе параметров макроса
//    MacroParams := Macro.ExplicitParams;
//    for i := 0 to Macro.ParamsCount - 1 do
//    begin
//      MParam := MacroParams[i];
//      if (ArgsCount < Macro.ParamsCount) then
//      begin
//        if Assigned(MParam.DefaultValue) then
//        begin
//          Arg := TIDMacroArgument.Create(NewScope, MParam.ID);  // надо подставлять просто декларацию значения по умолчанию
//          Arg.Text := '';
//          Arg.ArgExpession := MParam.DefaultValue;
//          Args.Add(Arg);
//        end else
//          ERROR_NOT_ENOUGH_ACTUAL_PARAMS(CallExpr);
//      end else
//        Args[i].ID := MParam.ID;
//    end;
//
//    // проверка кол-ва аргуметнов
//    if ArgsCount > Macro.ParamsCount then
//      ERROR_TOO_MANY_ACTUAL_PARAMS(CallExpr)
//    else if ArgsCount < Macro.ParamsCount then
//      ERROR_NOT_ENOUGH_ACTUAL_PARAMS(CallExpr);
//
//    // сохраняем состояние парсера
//    FParser.SaveState(PrevPos);
//
//    // перемещаем положение парсера на тело макроса
//    FParser.LoadState(Macro.BodyPosition);
//    FParser.NextToken;
//
//    // помещаем в новый scope аргументы вызова
//    for i := 0 to Args.Count - 1 do
//      NewScope.AddVariable(Args[i]);
//
//    // компилируем макрос
//    try
//      Result := ParseStatements(NewScope, SContext, False);
//    except
//      on e: ECompilerAbort do
//      begin
//        PutMessage(cmtError, 'Macro compilation ERROR:', CallExpr.TextPosition);
//        raise;
//      end;
//    end;
//
//    // восстанавливаем состояние парсера
//    FParser.LoadState(PrevPos);
//  finally
//    Args.Free;
//  end;
end;

procedure TNPUnit.ParseEnumType(Scope: TScope; Decl: TIDEnum);
var
  ID: TIdentifier;
  Token: TTokenID;
  Item: TIDIntConstant;
  Expr: TIDExpression;
  LB, HB, LCValue: Int64;
begin
  LCValue := 0;
  LB := MaxInt64;
  HB := MinInt64;
  Decl.Items := TScope.Create(stLocal, Scope);
  Token := parser_NextToken(Scope);
  while True do begin
    parser_MatchIdentifier(Token);
    parser_ReadCurrIdentifier(ID);
    Item := TIDIntConstant.Create(Decl.Items, ID);
    Item.DataType := Decl;
    InsertToScope(Decl.Items, Item);
    InsertToScope(Scope, Item);
    Token := parser_NextToken(Scope);
    if Token = token_equal then begin
      parser_NextToken(Scope);
      Token := ParseConstExpression(Scope, Expr, ExprNested);
      CheckEmptyExpression(Expr);
      CheckConstExpression(Expr);
      LCValue := TIDIntConstant(Expr.Declaration).Value;
    end;
    Item.Value := LCValue;
    LB := Min(LB, LCValue);
    HB := Max(HB, LCValue);
    case Token of
      token_coma: begin
        Token := parser_NextToken(Scope);
        Inc(LCValue);
        Continue;
      end;
      token_closeround: begin
        Decl.LowBound := LB;
        Decl.HighBound := HB;
        Break;
      end;
    else
      AbortWork(sComaOrCloseRoundExpected, parser_PrevPosition);
    end;
  end;
end;

function TNPUnit.ParseExitStatement(Scope: TScope; SContext: PSContext): TTokenID;
var
  EContext: TEContext;
  ResultDecl: TIDDeclaration;
  ResultExpr: TIDExpression;
  TryBlock: PTryContext;
  ExitType: TTryContext.TExitType;
begin
  Result := parser_NextToken(Scope);
  if Result = token_openround then
  begin
    if not Assigned(SContext.Proc.ResultType) then
      AbortWork(sReturnValueNotAllowedForProc, parser_Position);

    InitEContext(EContext, SContext, ExprNested);
    ResultDecl := SContext.Proc.EntryScope.FindID('RESULT');
    ResultExpr := TIDExpression.Create(ResultDecl, parser_Line);
    RPNPushExpression(EContext, ResultExpr);
    Result := ParseExpression(Scope, EContext, parser_NextToken(Scope));
    parser_MatchToken(Result, token_closeround);
    RPNPushOperator(EContext, opAssignment);
    EContext.RPNFinish();
    Result := parser_NextToken(Scope);
  end;
  {проверка на выход из try... секции}
  TryBlock := SContext.TryBlock;
  if not Assigned(TryBlock) then
    ILWrite(SContext, TIL.IL_Ret(parser_Line, cNone))
  else begin
    if TryBlock.Section = SectionFinally then
      AbortWork(sBreakContinueExitAreNotAllowedInFinallyClause, parser_PrevPosition);
    ExitType := etCallFinally;
    ILWrite(SContext, TIL.IL_Nope);
    while Assigned(TryBlock) do begin
      if not Assigned(TryBlock.Parent) then
        ExitType := etJumpToFinally;
      TryBlock.AddExit(ExitType, SContext.ILLast);
      TryBlock := TryBlock.Parent;
    end;
  end;
end;

function TNPUnit.ParseProcBody(Proc: TIDProcedure; Platform: TIDPlatform): TTokenID;
var
  Scope: TScope;
  SContext: TSContext;
begin
  Scope := Proc.EntryScope;
  Result := parser_CurTokenID;
  while true do begin
    case Result of
      token_var: begin
        parser_NextToken(Scope);
        Result := ParseVarSection(Scope, vLocal, nil, False);
      end;
      token_weak: begin
        parser_NextToken(Scope);
        Result := ParseVarSection(Scope, vLocal, nil, True);
      end;
      token_const: Result := ParseConstSection(Scope);
      token_type: Result := ParseNamedTypeDecl(Scope);
      token_procedure: Result := ParseProcedure(Scope, ptProc);
      token_function: Result := ParseProcedure(Scope, ptFunc);
      token_identifier: ERROR_KEYWORD_EXPECTED;
      token_asm: begin
        // skip the asm...end block
        Result := parser_SkipBlock(token_end);
        Result := parser_NextToken(Scope);
        Exit;
      end;
      token_begin: begin
        Proc.FirstBodyLine := parser_Line;
        Proc.IL := TIL.Create(Proc);
        SContext.Initialize;
        SContext.IL := TIL(Proc.IL);
        SContext.Proc := Proc;
        CheckInitVariables(@SContext, nil, @Proc.VarSpace);
        if not Assigned(Platform) then
        begin
          parser_NextToken(Scope);
          Result := ParseStatements(Scope, @SContext, True);
          parser_MatchToken(Result, token_end);
          Result := parser_NextToken(Scope);
        end else
          ParseASMStatement(Scope, Platform, @SContext);
        Proc.LastBodyLine := parser_Line;

        // геренация кода процедуры завершено
        Proc.Flags := Proc.Flags + [pfCompleted];
        FBENodesPool.Clear;
        Exit;
      end;
    else
      ERROR_BEGIN_KEYWORD_EXPECTED;
    end;
  end;
end;

function TNPUnit.ParseProcType(Scope: TScope; const ID: TIdentifier; ProcType: TProcType; out Decl: TIDProcType): TTokenID;
var
  Params: TScope;
  VarSpace: TVarSpace;
  ResultType: TIDType;
begin
  Decl := TIDProcType.Create(Scope, ID);
  Result := parser_NextToken(Scope);
  // если есть параметры
  if Result = token_openround then
  begin
    VarSpace.Initialize;
    Params := TScope.Create(stLocal, @VarSpace, nil, Scope, Self);
    ParseParameters(Params);
    Result := parser_NextToken(Scope);
    Decl.Params := ScopeToVarList(Params, 0);
  end;

  if ProcType = ptFunc then
  begin
    parser_MatchToken(Result, token_colon);
    Result := ParseTypeSpec(Scope, ResultType);
    Decl.ResultType := ResultType;
  end;

  if Result = token_of then
  begin
    parser_ReadToken(Scope, token_object);
    Result := parser_NextToken(Scope);
    Decl.IsStatic := False;
  end else
    Decl.IsStatic := True;

  if ID.Name <> '' then begin
    parser_MatchToken(Result, token_semicolon);
    InsertToScope(Scope, Decl);
  end;
end;

function GetProcDeclSring(Params: TScope; ResultType: TIDtype): string;
var
  sProcType,
  sProcParams,
  sProcResult,
  sProcParamName: string;
  Item: TIDDeclaration;
begin
  if Assigned(ResultType) then
  begin
    sProcType := 'function';
    sProcResult := ': ' + ResultType.DisplayName;
  end else
    sProcType := 'procedure';

  Item := Params.VarSpace.First;
  while Assigned(Item) do begin
    sProcParamName := Item.DisplayName + ': ' + Item.DataType.DisplayName;
    sProcParams := AddStringSegment(sProcParams, sProcParamName, '; ');
    Item := Item.NextItem;
  end;
  Result := sProcType + '(' + sProcParams + ')' + sProcResult;
end;

class procedure TNPUnit.MatchPropSetter(Prop: TIDProperty; Setter: TIDExpression; PropParams: TScope);
  procedure SetterDeclError(Proc: TIDProcedure);
  var
    V: TIDVariable;
  begin
    V := TIDVariable.CreateAsSystem(nil, 'Value');
    V.DataType := Prop.DataType;
    PropParams.AddVariable(V);
    ERROR_SETTER_MUST_BE_SUCH(Proc, GetProcDeclSring(PropParams, nil));
  end;
var
  Proc: TIDProcedure;
  i, pc: Integer;
  ProcParam, PropParam: TIDDeclaration;
  PropParamDataType: TIDType;
begin
  Proc := Setter.AsProcedure;
  if Assigned(Proc.ResultType) then
    SetterDeclError(Proc);

  if not Assigned(PropParams) then
  begin
    pc := 1;
    PropParam := nil;
  end else begin
    pc := PropParams.Count + 1;
    PropParam := PropParams.VarSpace.First;
  end;

  if Proc.ParamsCount <> pc then
    SetterDeclError(Proc);

  for i := 0 to pc - 1 do
  begin
    ProcParam := Proc.ExplicitParams[i];
    if Assigned(PropParam) then begin
      PropParamDataType := ProcParam.DataType;
      PropParam := PropParam.NextItem;
    end else
      PropParamDataType := Prop.DataType;

    if MatchImplicit(ProcParam.DataType, PropParamDataType) = nil then
      SetterDeclError(Proc);
  end;
end;

function TNPUnit.MatchRecordImplicit(SContext: PSContext; Source: TIDExpression; DstRecord: TIDRecord): TIDExpression;
var
  SrcRecord: TIDRecord;
  i: Integer;
  SrcFld, DstFld: TIDVariable;
begin
  SrcRecord :=  Source.DataType as TIDRecord;
  if SrcRecord.FieldsCount <> DstRecord.FieldsCount then
    Exit(nil);

  SrcFld := SrcRecord.Fields.First;
  DstFld := DstRecord.Fields.First;

  for i := 0 to SrcRecord.FieldsCount - 1 do
  begin
    if SrcFld.DataType.ActualDataType <> DstFld.DataType.ActualDataType then
      Exit(nil);
    SrcFld :=  TIDVariable(SrcFld.NextItem);
    DstFld :=  TIDVariable(DstFld.NextItem);
  end;
  Result := Source;
end;

class procedure TNPUnit.MatchPropGetter(Prop: TIDProperty; Getter: TIDProcedure; PropParams: TScope);
var
  i, pc: Integer;
  ProcParam, PropParam: TIDDeclaration;
begin
  pc := PropParams.Count;
  if Getter.ParamsCount <> pc then
    ERROR_GETTER_MUST_BE_SUCH(Getter, GetProcDeclSring(PropParams, Prop.DataType));

  PropParam := PropParams.VarSpace.First;
  for i := 0 to pc - 1 do
  begin
    ProcParam := Getter.ExplicitParams[i];
    if MatchImplicit(PropParam.DataType, ProcParam.DataType) = nil then
      ERROR_GETTER_MUST_BE_SUCH(Getter, GetProcDeclSring(PropParams, Prop.DataType));
    PropParam := PropParam.NextItem;
  end;
end;

function TNPUnit.ParseProperty(Struct: TIDStructure): TTokenID;
//var
//  ID: TIdentifier;
//  Prop: TIDProperty;
//  Scope: TScope;
//  PropDataType: TIDType;
//  Proc: TIDProcedure;
//  Expr: TIDExpression;
//  EContext: TEContext;
//  DataType: TIDType;
//  PropParams: TScope;
//  VarSpace: TVarSpace;
begin
  Assert(False);
//  Scope := Struct.Members;
//  parser_ReadNextIdentifier(Scope, ID);
//  Prop := TIDProperty.Create(Scope, ID);
//  Scope.AddProperty(Prop);
//
//  Result := parser_NextToken(Scope);
//  if Result = token_openblock then begin
//    VarSpace.Initialize;
//    PropParams := TProcScope.CreateInDecl(Scope, @VarSpace, nil);
//    Prop.Params := PropParams;
//    Result := ParseParameters(PropParams);
//    parser_MatchToken(Result, token_closeblock);
//    Result := parser_NextToken(Scope);
//  end else
//    PropParams := nil;
//
//  // парсим тип свойства
//  parser_MatchToken(Result, token_colon);
//  Result := ParseTypeSpec(Scope, PropDataType);
//  Prop.DataType := PropDataType;
//
//  // геттер
//  if Result = token_read then
//  begin
//    InitEContext(EContext, nil, ExprRValue);
//    Result := ParseExpression(Scope, EContext, parser_NextToken(Scope));
//    Expr := EContext.Result;
//    if Expr.ItemType = itProcedure then
//    begin
//      Proc := Expr.AsProcedure;
//      DataType := Proc.ResultType;
//      if not Assigned(DataType) then
//        AbortWork(sFieldConstOrFuncRequiredForGetter, Expr.TextPosition);
//
//      if Assigned(PropParams) then
//        MatchPropGetter(Prop, Proc, PropParams);
//
//    end else
//      DataType := Expr.DataType;
//
//    CheckImplicitTypes(DataType, PropDataType, Expr.TextPosition);
//    Prop.Getter := Expr.Declaration;
//  end;
//
//  // сеттер
//  if Result = token_write then
//  begin
//    InitEContext(EContext, nil, ExprRValue);
//    Result := ParseExpression(Scope, EContext, parser_NextToken(Scope));
//    Expr := EContext.Result;
//    case Expr.ItemType of
//      itConst: AbortWork(sFieldOrProcRequiredForSetter, Expr.TextPosition);
//      itProcedure: MatchPropSetter(Prop, Expr, PropParams);
//    end;
//    Prop.Setter := Expr.Declaration;
//  end;
//
//  parser_MatchToken(Result, token_semicolon);
//  Result := parser_NextToken(Scope);
//
//  // default - спецификатор
//  if Result = token_default then begin
//    if Prop.ParamsCount = 0 then
//      ERROR_DEFAULT_PROP_MUST_BE_ARRAY_PROP;
//    if not Assigned(Struct.DefaultProperty) then
//      Struct.DefaultProperty := Prop
//    else
//      ERROR_DEFAULT_PROP_ALREADY_EXIST(Struct.DefaultProperty);
//    parser_ReadSemicolon(Scope);
//    Result := parser_NextToken(Scope);
//  end;
end;

procedure TNPUnit.AddConstant(const Decl: TIDConstant);
var
  Item: TIDConstant;
begin
  Item := FConsts.First;
  while Assigned(Item) do
  begin
    if Item = Decl then
      Exit
    else
      Break;
    Item := TIDConstant(Item.NextItem);
  end;
  FConsts.Add(Decl);
end;

class function TNPUnit.AddResultParameter(Params: TScope): TIDVariable;
begin
  Result := TIDVariable.CreateAsAnonymous(Params);
  Result.Name := 'Result';
  Result.Flags := [VarParameter, VarOut, VarHiddenParam, VarResult];
  Params.AddVariable(Result);
end;

class procedure TNPUnit.AddSelfParameter(Params: TScope; Struct: TIDStructure; ClassMethod: Boolean);
var
  SelfParam: TIDVariable;
  DataType: TIDType;
begin
  if ClassMethod then
    DataType := Struct.ClassOfType
  else
    DataType := Struct;

  if Struct.DataTypeID = dtRecord then
    SelfParam := TIDVariable.Create(Params, Identifier('Self'), DataType, [VarParameter, VarSelf, VarConst, VarInOut, VarHiddenParam])
  else
    SelfParam := TIDVariable.Create(Params, Identifier('Self'), DataType, [VarParameter, VarSelf, VarConst, VarHiddenParam]);

  Params.AddVariable(SelfParam);
end;

function ScopeToVarList(Scope: TScope; SkipFirstCount: Integer): TVariableList;
var
  item: TIDDeclaration;
  i: Integer;
begin
  item := Scope.VarSpace.First;
  SetLength(Result, Scope.VarSpace.Count - SkipFirstCount);
  // пропускаем N первых элементов
  for i := 0 to SkipFirstCount - 1 do
    item := item.NextItem;

  i := 0;
  while Assigned(item) do begin
    Result[i] := TIDVariable(item);
    item := item.NextItem;
    Inc(i);
  end;
end;

function TNPUnit.ParseImportStatement(Scope: TScope; Decl: TIDDeclaration): TTokenID;
var
  LibExpr, NameExpr: TIDExpression;
begin
  // читаем имя библиотеки
  parser_NextToken(Scope);
  Result := ParseConstExpression(Scope, LibExpr, ExprRValue);

  if Assigned(LibExpr) then
  begin
    CheckStringExpression(LibExpr);

    Decl.ImportLib := TIDConstant(LibExpr.Declaration).Index;

    if Result = token_name then
    begin
      // читаем имя декларации
      parser_NextToken(Scope);
      Result := ParseConstExpression(Scope, NameExpr, ExprRValue);

      CheckEmptyExpression(NameExpr);
      CheckStringExpression(NameExpr);

      Decl.ImportName := TIDConstant(NameExpr.Declaration).Index;
    end else
      Decl.ImportName := FPackage.GetStringConstant(Decl.Name);
  end; // else todo:

  // delayed
  if Result = token_delayed then
    Result := parser_NextToken(Scope);

  parser_MatchSemicolon(Result);
end;

function TNPUnit.ProcSpec_Inline(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
begin
  if pfImport in Flags then
    ERROR_IMPORT_FUNCTION_CANNOT_BE_INLINE;
  if pfInline in Flags then
    ERROR_DUPLICATE_SPECIFICATION(PS_INLINE);
  Include(Flags, pfInline);
  parser_ReadSemicolon(Scope);
  Result := parser_NextToken(Scope);
end;

function TNPUnit.ProcSpec_Export(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
var
  ExportID: TIdentifier;
begin
  if pfExport in Flags then
    ERROR_DUPLICATE_SPECIFICATION(PS_EXPORT);
  Include(Flags, pfExport);
  {if Scope.ScopeClass <> scInterface then
    ERROR_EXPORT_ALLOWS_ONLY_IN_INTF_SECTION;}
  Result := parser_NextToken(Scope);
  if Result = token_identifier then begin
    parser_ReadCurrIdentifier(ExportID);
  Result := parser_NextToken(Scope);
  end else
    ExportID := Proc.ID;
  Proc.Export := FPackage.GetStringConstant(ExportID.Name);
  parser_MatchToken(Result, token_semicolon);
  Result := parser_NextToken(Scope);
end;

function TNPUnit.ProcSpec_Forward(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
begin
  if (pfForward in Flags) or (pfImport in Flags) then
    ERROR_DUPLICATE_SPECIFICATION(PS_FORWARD);
  Include(Flags, pfForward);
  parser_ReadSemicolon(Scope);
  Result := parser_NextToken(Scope);
end;

function TNPUnit.ProcSpec_Import(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
begin
  if pfImport in Flags then
    ERROR_DUPLICATE_SPECIFICATION(PS_IMPORT);
  Include(Flags, pfImport);
  ParseImportStatement(Scope, Proc);
  Result := FParser.NextToken;
end;

function TNPUnit.ProcSpec_Overload(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
begin
  if pfOveload in Flags then
    ERROR_DUPLICATE_SPECIFICATION(PS_OVELOAD);
  Include(Flags, pfOveload);
  Result := parser_ReadSemicolonAndToken(Scope);
end;

{function TNPUnit.ProcSpec_NoReturn(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
begin
  if pfNoReturn in Flags then
    ERROR_DUPLICATE_SPECIFICATION(PS_NORETURN);
  Include(Flags, pfNoReturn);
  parser_ReadSemicolon(Scope);
  Result := parser_NextToken(Scope);
end;}

{function TNPUnit.ProcSpec_Pure(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
begin
  if pfPure in Flags then
    ERROR_DUPLICATE_SPECIFICATION(PS_PURE);
  Proc.Flags := Proc.Flags + [pfPure];
  parser_ReadSemicolon(Scope);
  Result := parser_NextToken(Scope);
end;}

function TNPUnit.ProcSpec_Reintroduce(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
begin
  if pfReintroduce in Flags then
    ERROR_DUPLICATE_SPECIFICATION(PS_REINTRODUCE);
  Include(Flags, pfReintroduce);
  parser_ReadSemicolon(Scope);
  Result := parser_NextToken(Scope);
end;

function TNPUnit.ProcSpec_Virtual(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
begin
  if pfVirtual in Flags then
    ERROR_DUPLICATE_SPECIFICATION(PS_VIRTUAL);
  Include(Flags, pfVirtual);

  if not Assigned(Proc.Struct) then
    ERROR_VIRTUAL_ALLOWED_ONLY_IN_CLASSES;

  Proc.VirtualIndex := Proc.Struct.GetLastVirtualIndex + 1;

  parser_ReadSemicolon(Scope);
  Result := parser_NextToken(Scope);
end;

function TNPUnit.ProcSpec_Override(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
var
  PrevProc: TIDProcedure;
begin
  if pfOverride in Proc.Flags then
    ERROR_DUPLICATE_SPECIFICATION(PS_OVERRIDE);

  if not Assigned(Proc.Struct) then
    ERROR_STRUCT_TYPE_REQUIRED(parser_Position);

  PrevProc := Proc.Struct.FindVirtualProcInAncestor(Proc);
  if not Assigned(PrevProc) then
    ERROR_NO_METHOD_IN_BASE_CLASS(Proc);

  Proc.VirtualIndex := PrevProc.VirtualIndex;

  Include(Flags, pfOverride);
  Include(Flags, pfVirtual);
  parser_ReadSemicolon(Scope);
  Result := parser_NextToken(Scope);
end;

function TNPUnit.ProcSpec_Static(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
begin
  if pfStatic in Proc.Flags then
    ERROR_DUPLICATE_SPECIFICATION(PS_STATIC);
  Include(Flags, pfStatic);
  parser_ReadSemicolon(Scope);
  Result := parser_NextToken(Scope);
end;

function TNPUnit.ProcSpec_StdCall(Scope: TScope; var CallConvention: TCallConvention): TTokenID;
begin
  if CallConvention = ConvStdCall then
    ERROR_DUPLICATE_SPECIFICATION(PS_STDCALL);
  CallConvention := ConvStdCall;
  parser_ReadSemicolon(Scope);
  Result := parser_NextToken(Scope);
end;

function TNPUnit.ProcSpec_FastCall(Scope: TScope; var CallConvention: TCallConvention): TTokenID;
begin
  if CallConvention = ConvFastCall then
    ERROR_DUPLICATE_SPECIFICATION(PS_FASTCALL);
  CallConvention := ConvFastCall;
  parser_ReadSemicolon(Scope);
  Result := parser_NextToken(Scope);
end;

function TNPUnit.ProcSpec_CDecl(Scope: TScope; var CallConvention: TCallConvention): TTokenID;
begin
  if CallConvention = ConvCDecl then
    ERROR_DUPLICATE_SPECIFICATION(PS_CDECL);
  CallConvention := ConvCDecl;
  parser_ReadSemicolon(Scope);
  Result := parser_NextToken(Scope);
end;

function TNPUnit.ParseProcedure(Scope: TScope; ProcType: TProcType; Struct: TIDStructure): TTokenID;
type
  TFwdDeclState = (dsNew, dsDifferent, dsSame);
var
  ID: TIdentifier;
  Parameters: TProcScope;
  ResultType: TIDType;
  ResultParam: TIDVariable;
  VarSpace: TVarSpace;
  GenericsParams: TIDTypeList;
  Proc, ForwardDecl: TIDProcedure;
  FwdDeclState: TFwdDeclState;
  FirstSkipCnt: Integer;
  SRCProcPos: TParserPosition;
  CallConv: TCallConvention;
  ProcFlags: TProcFlags;
  ForwardScope: TScope;
begin
  ForwardScope := Scope;
  Result := ParseProcName(ForwardScope, ID, Struct, Parameters, GenericsParams);

  VarSpace.Initialize;
  Parameters.VarSpace := addr(VarSpace);

  // создаем Result переменную (тип будет определен позже)
  if ProcType < ptProc then
    ResultParam := AddResultParameter(Parameters)
  else
    ResultParam := nil;

  if Assigned(Struct) then
    AddSelfParameter(Parameters, Struct, (ProcType = ptClassProc) or (ProcType = ptClassFunc))
  else
    Parameters.OuterScope := Scope;

  FParser.SaveState(SRCProcPos);

  // парсим параметры
  if Result = token_openround then
  begin
    ParseParameters(Parameters);
    Result := parser_NextToken(Scope);
  end;

  // парсим тип возвращаемого значения
  if ProcType <= ptStaticFunc then begin
    parser_MatchToken(Result, token_colon);
    Result := ParseTypeSpec(Parameters, ResultType);
    ResultParam.DataType := ResultType;
    ResultParam.TextPosition := parser_Position;
  end else
    ResultType := nil;

  parser_MatchToken(Result, token_semicolon);

  // ищем ранее обьявленную декларацию с таким же именем
  if Assigned(Struct) then
  begin
    ForwardDecl := TIDProcedure(Struct.Members.FindID(ID.Name));
    if not Assigned(ForwardDecl) and (Scope.ScopeClass = scImplementation) then
      ERROR_METHOD_NOT_DECLARED_IN_CLASS(ID, Struct);
  end else
    ForwardDecl := TIDProcedure(ForwardScope.FindID(ID.Name));

  Proc := nil;
  FwdDeclState := dsDifferent;

  {если найдена ранее обьявленная декларация, проверяем соответствие}
  if Assigned(ForwardDecl) then begin
    // ошибка если перекрыли идентификатор другого типа:
    if ForwardDecl.ItemType <> itProcedure then
      ERROR_ID_REDECLARATED(ID);
    // ищем подходящую декларацию в списке перегруженных:
    while True do begin
      if ForwardDecl.SameDeclaration(Parameters) then begin
        // нашли подходящую декларацию
        FwdDeclState := dsSame;
        if Assigned(ForwardDecl.IL) or (Scope.ScopeClass = scInterface) then
          ERROR_ID_REDECLARATED(ID);
        Proc := ForwardDecl;
        Break;
      end;
      // не нашли подходящую декларацию, будем создавать новую,
      // проверку дерективы overload оставим на потом
      if not Assigned(ForwardDecl.NextOverload) then
        Break;
      ForwardDecl := ForwardDecl.NextOverload;
    end;
  end else
    FwdDeclState := dsNew;

  {создаем новую декларацию}
  if not Assigned(Proc) then
  begin
    Proc := TIDProcedure.Create(Scope, ID);
    // если это generic-процедура или это generic-метод
    if Assigned(GenericsParams) then
      Proc.CreateGenericDescriptor(GenericsParams, SRCProcPos)
    else if Assigned(Struct) and Assigned(Struct.GenericDescriptor) then
      Proc.CreateGenericDescriptor(Struct.GenericDescriptor.GenericParams, SRCProcPos);

    // Для Scope будут переопределены VarSpace и ProcSpace
    Proc.ParamsScope := Parameters;
    Proc.VarSpace := VarSpace;
    Proc.ResultType := ResultType;

    FirstSkipCnt := 0;
    if Assigned(Struct) then
      Inc(FirstSkipCnt);
    if Assigned(ResultType) then
      Inc(FirstSkipCnt);
    Proc.ExplicitParams := ScopeToVarList(Parameters, FirstSkipCnt);

    case ProcType of
      ptClassFunc,
      ptClassProc: ProcFlags := [pfClass];
      ptStaticFunc,
      ptStaticProc: ProcFlags := [pfStatic];
      ptConstructor: begin
        if not Assigned(Struct) then
          ERROR_CTOR_DTOR_MUST_BE_DECLARED_IN_STRUCT(parser_PrevPosition);
        ProcFlags := [pfConstructor];
      end;
      ptDestructor: begin
        if not Assigned(Struct) then
          ERROR_CTOR_DTOR_MUST_BE_DECLARED_IN_STRUCT(parser_PrevPosition);
        ProcFlags := [pfDestructor];
      end
    else
      ProcFlags := [];
    end;

    // добовляем новую декларацию в структуру или глобольный список или к списку перегруженных процедур
    if not Assigned(ForwardDecl) then
    begin
      if Assigned(Struct) then begin
        // метод
        Struct.Members.AddProcedure(Proc);
        Proc.Struct := Struct;
        if Struct.DataTypeID = dtRecord then
        case ProcType of
          ptConstructor: begin
            if Proc.ParamsCount = 0 then
            begin
              if Assigned(TIDRecord(Struct).StaticConstructor) then
                ERROR_RECORD_STATIC_CONSTRUCTOR_ALREADY_EXIST(Proc);
              TIDRecord(Struct).StaticConstructor := Proc;
            end;
          end;
          ptDestructor: begin
            CheckStaticRecordConstructorSign(Proc);
            if Assigned(TIDRecord(Struct).StaticDestructor) then
              ERROR_RECORD_STATIC_DESTRUCTOR_ALREADY_EXIST(Proc);
            TIDRecord(Struct).StaticDestructor := Proc;
          end;
        end;
      end else
        // глобальная процедура
        Scope.AddProcedure(Proc);
    end else begin
      // доавляем в список следующую перегруженную процедуру
      ForwardDecl.NextOverload := Proc;
      if Assigned(Struct) then
      begin
        Struct.AddMethod(Proc);
        Proc.Struct := Struct;
      end else
        Scope.ProcSpace.Add(Proc);
    end;
  end else begin
    if Assigned(Proc.GenericDescriptor) then
      Proc.GenericDescriptor.ImplSRCPosition := SRCProcPos;
    ProcFlags := [];
  end;
  CallConv := ConvNative;
  Result := parser_NextToken(Scope);
  while True do begin
    case Result of
      token_forward: Result := ProcSpec_Forward(Scope, Proc, ProcFlags);
      token_export: Result := ProcSpec_Export(Scope, Proc, ProcFlags);
      token_inline: Result := ProcSpec_Inline(Scope, Proc, ProcFlags);
      token_external: Result := ProcSpec_Import(Scope, Proc, ProcFlags);
      token_overload: Result := ProcSpec_Overload(Scope, Proc, ProcFlags);
      token_virtual: Result := ProcSpec_Virtual(Scope, Proc, ProcFlags);
      token_override: Result := ProcSpec_Override(Scope, Proc, ProcFlags);
      token_reintroduce: Result := ProcSpec_Reintroduce(Scope, Proc, ProcFlags);
      token_static: Result := ProcSpec_Static(Scope, Proc, ProcFlags);
      token_stdcall: Result := ProcSpec_StdCall(Scope, CallConv);
      token_fastcall: Result := ProcSpec_FastCall(Scope, CallConv);
      token_cdecl: Result := ProcSpec_CDecl(Scope, CallConv);
      token_varargs: begin
        parser_ReadSemicolon(Scope);
        Result := parser_NextToken(Scope);
      end;
      token_deprecated: begin
        Result := CheckAndParseDeprecated(Scope, token_deprecated);
        Result := parser_NextToken(Scope);
      end;
      token_platform: begin
        Result := ParsePlatform(Scope);
        Result := parser_NextToken(Scope);
      end;
    else
      if (Scope.ScopeClass = scInterface) or (pfImport in ProcFlags) then begin
        Proc.Flags := ProcFlags;
        Proc.CallConvention := CallConv;
        Break;
      end;

      // имена парметров реализации процедуры могут отличатся от ее определения
      // копируем накопленный VarSpace в процедуру

      Proc.VarSpace := VarSpace;

      Parameters.ProcSpace := Proc.ProcSpace;
      Proc.EntryScope := Parameters;

      if (FwdDeclState = dsDifferent) and not (pfOveload in ProcFlags) then
      begin
        if Assigned(Proc.IL) then
          ERROR_OVERLOADED_MUST_BE_MARKED(ID)
        else
          ERROR_DECL_DIFF_WITH_PREV_DECL(ID);
      end;
      Result := ParseProcBody(Proc, nil);
      if Result <> token_semicolon then
        ERROR_SEMICOLON_EXPECTED;

      Result := parser_NextToken(Scope);
      Break;
    end;
  end;

  if (ProcType = ptDestructor) and (Struct.DataTypeID = dtClass) then
    CheckDestructorSignature(Proc);
end;

function TNPUnit.FindID(Scope: TScope; const ID: TIdentifier; out Expression: TIDExpression): TIDDeclaration;
var
  i: Integer;
  IDName: string;
begin
  IDName := ID.Name;
  Result := Scope.FindIDRecurcive(IDName, Expression);
  if Assigned(Result) then
    Exit;
  with FIntfImportedUnits do
  begin
    for i := Count - 1 downto 0 do begin
      Result := TNPUnit(Objects[i]).IntfSection.FindID(IDName);
      if Assigned(Result) then
        Exit;
    end;
  end;
  ERROR_UNDECLARED_ID(ID);
end;


function TNPUnit.FindIDNoAbort(Scope: TScope; const ID: TIdentifier; out Expression: TIDExpression): TIDDeclaration;
var
  i: Integer;
  IDName: string;
begin
  IDName := ID.Name;
  Result := Scope.FindIDRecurcive(IDName, Expression);
  if Assigned(Result) then
    Exit;
  with FIntfImportedUnits do
  begin
    for i := Count - 1 downto 0 do begin
      Result := TNPUnit(Objects[i]).IntfSection.FindID(IDName);
      if Assigned(Result) then
        Exit;
    end;
  end;
end;

function TNPUnit.FindIDNoAbort(Scope: TScope; const ID: string): TIDDeclaration;
var
  i: Integer;
  Expr: TIDExpression;
begin
  Result := Scope.FindIDRecurcive(ID, Expr);
  if Assigned(Result) then
    Exit;
  with FIntfImportedUnits do
    for i := Count - 1 downto 0 do begin
      Result := TNPUnit(Objects[i]).IntfSection.FindID(ID);
      if Assigned(Result) then
        Exit;
    end;
end;

function TNPUnit.FindIDNoAbort(Scope: TScope; const ID: TIdentifier): TIDDeclaration;
var
  i: Integer;
  Expr: TIDExpression;
  IDName: string;
begin
  IDName := ID.Name;
  Result := Scope.FindIDRecurcive(IDName, Expr);
  if Assigned(Result) then
    Exit;
  for i := FIntfImportedUnits.Count - 1 downto 0 do
  begin
    var un := TNPUnit(FIntfImportedUnits.Objects[i]);
    Result := un.IntfSection.FindID(IDName);
    if Assigned(Result) then
      Exit;
  end;
end;

function TNPUnit.ParseProcName(var Scope: TScope; out Name: TIdentifier; var Struct: TIDStructure; out ProcScope: TProcScope; out GenericParams: TIDTypeList): TTokenID;
var
  Decl: TIDDeclaration;
  SearchName: string;
begin
  ProcScope := nil;
  while True do begin
    parser_ReadNextIdentifier(Scope, Name);
    Result := parser_NextToken(Scope);
    if Result = token_less then
    begin
      if Assigned(Struct) then
        ProcScope := TMethodScope.CreateInDecl(Scope, Struct.Members)
      else
        ProcScope := TProcScope.CreateInDecl(Scope, nil, nil);
      Result := ParseGenericsHeader(ProcScope, GenericParams);
      SearchName := Format('%s<%d>', [Name.Name, Length(GenericParams)]);
    end else
      SearchName := Name.Name;

    if Result = token_dot then
    begin
      Decl := Scope.FindID(SearchName);
      if not Assigned(Decl) then
        ERROR_UNDECLARED_ID(Name, GenericParams);

      if Decl is TIDStructure then
      begin
        Struct := TIDStructure(Decl);
        {т.к это имплементация обобщенного метода, то очищаем дупликатные обобщенные параметры}
        if Assigned(ProcScope) then begin
          ProcScope.OuterScope := Scope;
          ProcScope.Parent := Struct.Members;
          ProcScope.Clear;
        end;
      end
      else if Decl.ItemType = itNameSpace then
        Scope := TIDNameSpace(Decl).Members
      else
        ERROR_STRUCT_TYPE_REQUIRED(Name.TextPosition);
      continue;
    end;
    if not Assigned(ProcScope) then
    begin
      if Assigned(Struct) then
        ProcScope := TMethodScope.CreateInDecl(Scope, Struct.Members)
      else
        ProcScope := TProcScope.CreateInDecl(Scope, nil, nil);
    end;
    {$IFDEF DEBUG}ProcScope.Name := Name.Name + '_params'; {$ENDIF}
    Exit;
  end;
end;

function TNPUnit.FindID(Scope: TScope; const ID: TIdentifier): TIDDeclaration;
var
  tmp: TIDExpression;
begin
  Result := FindID(Scope, ID, tmp);
end;

procedure TNPUnit.AddType(const Decl: TIDType);
begin
  if not (Decl is TIDAliasType) and not Decl.IsPooled then
  begin
    FTypeSpace.Add(Decl);
    Decl.IsPooled := True;
  end;
end;

procedure TNPUnit.Bool_AddExprNode(var Context: TEContext; Instruction: TILInstruction; Condition: TILCondition);
var
  Node: PBoolExprNode;
begin
  Node := PBoolExprNode(FBENodesPool.Add);
  Node.Parent := nil;
  Node.NodeType := ntCmp;
  Node.Instruction := Instruction;
  Node.Condition := Condition;
  Node.LeftNode := nil;
  Node.RightNode := nil;
  Node.Orientation := NodeRoot;
  with Context do begin
    Node.PrevNode := LastBoolNode;
    LastBoolNode := Node;
  end;
end;

procedure TNPUnit.Bool_AddExprNode(var EContext: TEContext; NodeType: TBoolExprNode.TNodeType);
var
  LNode, RNode, Node: PBoolExprNode;
begin
  // выбираем из стека два последних нода
  // последний - для нас правый, предпоследний - для нас левый
  RNode := EContext.LastBoolNode;
  Assert(Assigned(RNode), 'AND/OR node need left and right nodes first');
  LNode := RNode.PrevNode;

  // создаем новый нод для AND/OR операции
  Node := PBoolExprNode(FBENodesPool.Add);
  Node.NodeType := NodeType;
  Node.Instruction := EContext.SContext.ILLast;
  Node.LeftChild := LNode;
  Node.RightChild := RNode;
  EContext.LastBoolNode := Node;

  // теперь последний стеку станет для нас предыдущим
  Node.PrevNode := LNode.PrevNode;

  // настраиваем левый и правый подноды
  LNode.Parent := Node;
  LNode.Orientation := NodeLeft;
  RNode.Parent := Node;
  RNode.Orientation := NodeRight;

  // находим самый левый (по исходному коду) нод
  while Assigned(LNode.RightChild) do
    LNode := LNode.RightChild;
  Node.LeftNode := LNode;
  LNode.RightNode := Node;

  // находим самый правый (по исходному коду) нод
  while Assigned(RNode.LeftChild) do
    RNode := RNode.LeftChild;
  Node.RightNode := RNode;
  RNode.LeftNode := Node;
end;

procedure TNPUnit.Bool_CompleteExpression(Node: PBoolExprNode; ElsePosition: TILInstruction);
  function PassTree(Node: PBoolExprNode; Condition: Boolean): TILInstruction;
  var
    Parent: PBoolExprNode;
  begin
    Parent := Node.Parent;
    if not Assigned(Parent) then
      if Condition then
        Exit(Node.Instruction)
      else
        Exit(ElsePosition);

    if (Parent.NodeType = ntOr) then
    begin
      if Condition = True then
        Result := PassTree(Parent, True)
      else begin
        if Node.Orientation = NodeRight then
          Result := PassTree(Parent, False)
        else
          Result := Node.Instruction;
      end;
    end else begin
      if Condition = False then
        Result := PassTree(Parent, False)
      else begin
        if Node.Orientation = NodeRight then
          Result := PassTree(Parent, True)
        else
          Result := Node.Instruction;
      end;
    end;
  end;
var
  Position: TILInstruction;
  c: TILCondition;
  TrueCondition: Boolean;
  RNode: PBoolExprNode;
  I: Integer;
begin
  if not Assigned(Node) or (not Assigned(Node.Instruction)) then Exit;
  // находим самый первый нод
  while Assigned(Node.LeftChild) do
    Node := Node.LeftChild;

  I := 0;
  while Assigned(Node) do
  begin
    Inc(I);
    // обрабатываем только ноды сравнений
    if Node.NodeType <> ntCmp then
    begin
      Node := Node.RightNode;
      Continue;
    end;
    RNode := Node.RightNode;
    // если следующий нод существует, значит
    // позицию перехода необходимо расчитывать дальше
    if Assigned(RNode) then begin
      TrueCondition := (RNode.NodeType = ntOr);
      Position := PassTree(RNode, TrueCondition);
      if TrueCondition then
        c := Node.Condition
      else
        c := InverseCondition(Node.Condition);
      // добавляем инструкцию перехода
      //FIL.InsertAfter(Node.Instruction, TIL.Instruction_JmpNext(c, Position));
    end else begin
      // если это последний нод, то переход всегда не "ELSE" секцию
      Position := ElsePosition;
      //TrueCondition := False;
      c := InverseCondition(Node.Condition);
      //FIL.InsertAfter(Node.Instruction, TIL.Instruction_JmpNext(c, ElsePosition));
    end;
    TILJmpNext(Node.Instruction).Condition := c;
    TILJmpNext(Node.Instruction).Destination := Position;
    // берем следующий (правый) нод
    Node := Node.RightNode;
  end;
  FBENodesPool.Position := FBENodesPool.Position - I;
end;

function TNPUnit.Bool_CompleteImmediateExpression(const EContext: TEContext; Destination: TIDExpression): Boolean;
var
  Node: PBoolExprNode;
  SContext: PSContext;
  ToElseJump: TILJmp;
begin
  Node := EContext.LastBoolNode;
  if not Assigned(Node) or (not Assigned(Node.Instruction)) then
    Exit(False);

  SContext := EContext.SContext;
  if Node.NodeType = ntCmp then
  begin
    SContext.IL.Replace(Node.Instruction, TIL.IL_SetBool(Node.Condition, Destination));
  end else begin
    ILWrite(EContext.SContext, TIL.IL_Move(Destination, SYSUnit._TrueExpression));
    ToElseJump := TIL.IL_JmpNext(Destination.Line, cNone, nil);
    ILWrite(EContext.SContext, ToElseJump);
    Bool_CompleteExpression(Node, ToElseJump);
    ILWrite(EContext.SContext, TIL.IL_Move(Destination, SYSUnit._FalseExpression));
    ToElseJump.Destination := SContext.ILLast;
  end;
  Result := True;
end;

function TNPUnit.ParseRaiseStatement(Scope: TScope; SContext: PSContext): TTokenID;
var
  EExcept: TIDExpression;
  EContext: TEContext;
begin
  InitEContext(EContext, SContext, ExprRValue);
  Result := ParseExpression(Scope, EContext, parser_NextToken(Scope));
  EExcept := EContext.Result;
  CheckEmptyExpression(EExcept);
  CheckClassExpression(EExcept);
  ILWrite(SContext, TIL.IL_EThrow(cNone, EExcept));
end;

procedure TNPUnit.ParseRangeType(Scope: TScope; Expr: TIDExpression; Decl: TIDRangeType);
var
  LB, HB: Int64;
  CRange: TIDRangeConstant;
  BoundExpr: TIDExpression;
  RDataTypeID: TDataTypeID;
  RDataType: TIDType;
begin
  CRange := TIDRangeConstant(Expr.Declaration);
  if (CRange.ItemType <> itConst) and (not (CRange is TIDRangeConstant)) then
    AbortWork(sConstRangeRequired, parser_Position);

  BoundExpr := CRange.Value.LBExpression;
  CheckConstExpression(BoundExpr);
  LB := TIDConstant(BoundExpr.Declaration).AsInt64;

  BoundExpr := CRange.Value.HBExpression;
  CheckConstExpression(BoundExpr);
  HB := TIDConstant(BoundExpr.Declaration).AsInt64;

  RDataTypeID := GetValueDataType(HB - LB);
  RDataType := SYSUnit.DataTypes[RDataTypeID];

  Decl.LowBound := LB;
  Decl.HighBound := HB;

  if Decl.Name = '' then
   Decl.ID := Identifier(Format('Range %d..%d', [LB, HB]), Expr.TextPosition.Row, Expr.TextPosition.Col);

  Decl.OverloadImplicitFrom(RDataType);
end;

function TNPUnit.ParseSetType(Scope: TScope; Decl: TIDSet): TTokenID;
var
  ID: TIdentifier;
  Base: TIDDeclaration;
  EContext: TEContext;
  Expression: TIDExpression;
begin
  parser_MatchToken(parser_NextToken(Scope), token_of);
  Result := parser_NextToken(Scope);
  case Result of
    token_identifier: begin
      InitEContext(EContext, nil, ExprRValue);
      Result := ParseExpression(Scope, EContext, Result);
      Expression := EContext.Result;
      if Expression.ItemType = itType then begin
        Base := Expression.Declaration;
        if not TIDType(Base).Ordinal then
          AbortWork(sOrdinalTypeRequired, parser_PrevPosition);
      end else begin
        Base := TIDRangeType.Create(Scope, ID);
        ParseRangeType(Scope, Expression, TIDRangeType(Base));
        AddType(TIDType(Base));
      end;
    end;
    token_openround: begin
      Base := TIDEnum.CreateAsAnonymous(Scope);
      ParseEnumType(Scope, TIDEnum(Base));
      Result := parser_NextToken(Scope);
      AddType(TIDType(Base));
    end;
    else begin
      ERROR_ORDINAL_TYPE_REQUIRED(parser_PrevPosition);
      Exit;
    end;
  end;
  Decl.AddBound(TIDOrdinal(Base));
  Decl.BaseType := TIDOrdinal(Base);
  Decl.BaseType.OverloadBinarOperator2(opIn, Decl, SYSUnit._Boolean);
end;

function TNPUnit.CompileSource(Section: TUnitSection; const Source: string): ICompilerMessages;
var
  ParserState: TParserPosition;
  ParserSource: string;
  Token: TTokenID;
  Scope: TScope;
begin
  Result := FMessages;
  FParser.SaveState(ParserState);
  ParserSource := FParser.Source;
  try
    try
      case Section of
        usInterface: Scope := FIntfScope;
        usImplementation: Scope := FImplScope;
      else
        Scope := nil;
      end;
      FParser.Source := Source;
      FParser.First;
      Token := parser_NextToken(Scope);
      while true do begin
        case Token of
          token_type: begin
            CheckIntfSectionMissing(Scope);
            Token := ParseNamedTypeDecl(Scope);
          end;
          token_uses: Token := ParseUsesSection(Scope);
          token_function: begin
            CheckIntfSectionMissing(Scope);
            Token := ParseProcedure(Scope, ptFunc);
          end;
          token_procedure: begin
            CheckIntfSectionMissing(Scope);
            Token := ParseProcedure(Scope, ptProc);
          end;
          //token_namespace: Token := ParseNameSpace(Scope);
          token_constructor: begin
            CheckIntfSectionMissing(Scope);
            Token := ParseProcedure(Scope, ptConstructor);
          end;
          token_destructor: begin
            CheckIntfSectionMissing(Scope);
            Token := ParseProcedure(Scope, ptDestructor);
          end;
          token_operator: begin
            CheckIntfSectionMissing(Scope);
            Token := ParseOperator(Scope, nil);
          end;
          token_const: begin
            CheckIntfSectionMissing(Scope);
            Token := ParseConstSection(Scope);
          end;
          token_class: begin
            CheckIntfSectionMissing(Scope);
            Token := parser_NextToken(Scope);
            case Token of
              token_function: Token := ParseProcedure(Scope, ptClassFunc);
              token_procedure: Token := ParseProcedure(Scope, ptClassProc);
              token_constructor: Token := ParseProcedure(Scope, ptClassConstructor);
              token_destructor: Token := ParseProcedure(Scope, ptClassDestructor);
            else
              ERROR_FEATURE_NOT_SUPPORTED;
            end;
          end;
          token_weak: begin
            CheckIntfSectionMissing(Scope);
            parser_NextToken(Scope);
            Token := ParseVarSection(Scope, vLocal, nil, True);
          end;
          token_var: begin
            CheckIntfSectionMissing(Scope);
            parser_NextToken(Scope);
            Token := ParseVarSection(Scope, vLocal, nil, False);
          end;
          token_interface: begin
            Scope := FIntfScope;
            Token := parser_NextToken(Scope);
          end;
          token_implementation: begin
            CheckIntfSectionMissing(Scope);
            Scope := FImplScope;
            Token := parser_NextToken(Scope);
          end;
          token_end: begin
            parser_MatchToken(parser_NextToken(Scope), token_dot);
            Token := parser_NextToken(Scope);
            if Token <> token_eof then
              HINT_TEXT_AFTER_END;
            Break;
          end;
          token_initialization: Token := ParseInitSection;
          token_finalization: Token := ParseFinalSection;
          token_eof: Break;
        else
          if Token >= token_cond_define then
          begin
            Token := ParseCondStatements(Scope, Token);
            continue;
          end;
          ERROR_KEYWORD_EXPECTED;
        end;
      end;
    except
      on e: ECompilerAbort do PutMessage(ECompilerAbort(e).CompilerMessage^);
      on e: Exception do PutMessage(cmtInteranlError, e.Message, parser_Position);
    end;
  finally
    FParser.Source := ParserSource;
    FParser.LoadState(ParserState);
  end;
end;

function TNPUnit.CompileTypeDecl(Section: TUnitSection; const Source: string; out Decl: TIDType): ICompilerMessages;
var
  ParserState: TParserPosition;
  ParserSource: string;
  Scope: TScope;
begin
  Result := FMessages;
  FParser.SaveState(ParserState);
  ParserSource := FParser.Source;
  try
    case Section of
      usInterface: Scope := FIntfScope;
      usImplementation: Scope := FImplScope;
    else
      Scope := nil;
    end;
    try
      ParseNamedTypeDecl(Scope);
      Decl := FTypeSpace.Last;
    except
      on e: ECompilerAbort do PutMessage(ECompilerAbort(e).CompilerMessage^);
      on e: Exception do PutMessage(cmtInteranlError, e.Message);
    end;
  finally
    FParser.Source := ParserSource;
    FParser.LoadState(ParserState);
  end;
end;

function TNPUnit.CompileProcDecl(Section: TUnitSection; const Source: string; out Proc: TIDProcedure): ICompilerMessages;
var
  ParserState: TParserPosition;
  ParserSource: string;
  Token: TTokenID;
  Scope: TScope;
begin
  Result := FMessages;
  FParser.SaveState(ParserState);
  ParserSource := FParser.Source;
  try
    try
      case Section of
        usInterface: Scope := FIntfScope;
        usImplementation: Scope := FImplScope;
      else
        Scope := nil;
      end;
      FParser.Source := Source;
      FParser.First;
      Token := parser_NextToken(Scope);
      case Token of
        token_function: begin
          CheckIntfSectionMissing(Scope);
          ParseProcedure(Scope, ptFunc);
          Proc := Scope.ProcSpace.Last;
        end;
        token_procedure: begin
          CheckIntfSectionMissing(Scope);
          ParseProcedure(Scope, ptProc);
          Proc := Scope.ProcSpace.Last;
        end;
      else
        ERROR_KEYWORD_EXPECTED;
      end;
    except
      on e: ECompilerAbort do PutMessage(ECompilerAbort(e).CompilerMessage^);
      on e: Exception do PutMessage(cmtInteranlError, e.Message);
    end;
  finally
    FParser.Source := ParserSource;
    FParser.LoadState(ParserState);
  end;
end;

function TNPUnit.ParseVarStaticArrayDefaultValue(Scope: TScope; ArrType: TIDArray; out DefaultValue: TIDExpression): TTokenID;
begin
  Assert(False);
end;

function TNPUnit.ParseVarDefaultValue(Scope: TScope; DataType: TIDType; out DefaultValue: TIDExpression): TTokenID;
//var
//  SContext: PSContext;
//  EContext: TEContext;
begin
  Assert(False);
//  case DataType.DataTypeID of
//    dtStaticArray: Result := ParseVarStaticArrayDefaultValue(Scope, DataType as TIDArray, DefaultValue);
//    dtRecord: Result := ParseVarRecordDefaultValue(Scope, DataType as TIDStructure, DefaultValue);
//  else
//    SContext := @fInitProcSConect;
//
//    Result := parser_NextToken(Scope);
//
//    if Scope.ScopeType = stLocal then
//      Result := ParseConstExpression(Scope, DefaultValue, Result, ExprRValue)
//    else begin
//      InitEContext(EContext, SContext, ExprRValue);
//      Result := ParseExpression(Scope, EContext, Result);
//      DefaultValue := EContext.Result;
//    end;
//    CheckEmptyExpression(DefaultValue);
//
//    if CheckImplicit(DefaultValue, DataType) = nil then
//      ERROR_INCOMPATIBLE_TYPES(DefaultValue, DataType);
//
//    DefaultValue := MatchImplicit3(SContext, DefaultValue, DataType);
//
//    if DefaultValue.IsAnonymous then
//      DefaultValue.Declaration.DataType := DataType; // подгоняем фактичиский тип константы под необходимый
//  end;
end;

function TNPUnit.ParseVarSection(Scope: TScope; Visibility: TVisibility; Struct: TIDStructure;
                                 IsWeak: Boolean = False; isRef: Boolean = False): TTokenID;
var
  i, c: Integer;
  DataType: TIDType;
  DefaultValue: TIDExpression;
  DeclAbsolute: TIDDeclaration;
  ID: TIdentifier;
  Field: TIDVariable;
  Names: TIdentifiersPool;
  VarFlags: TVariableFlags;
  Deprecated: TIDExpression;
begin
  c := 0;
  Names := TIdentifiersPool.Create(2);
  Result := parser_CurTokenID;
  while True do begin
    VarFlags := [];
    Result := CheckAndParseAttribute(Scope);
    parser_MatchIdentifier(Result);
    Names.Add;
    parser_ReadCurrIdentifier(Names.Items[c]);
    Result := parser_NextToken(Scope);
    if Result = token_Coma then begin
      Inc(c);
      Result := parser_NextToken(Scope);
      Continue;
    end;
    parser_MatchToken(Result, token_colon);
    // парсим тип
    Result := ParseTypeSpec(Scope, DataType);
    DeclAbsolute := nil;
    DefaultValue := nil;

    // platform declaration
    if Result = token_platform then
      Result := ParsePlatform(Scope);

    case Result of
      // значение по умолчанию
      token_equal: Result := ParseVarDefaultValue(Scope, DataType, DefaultValue);
      // absolute
      token_absolute: begin
        parser_ReadNextIdentifier(Scope, ID);
        DeclAbsolute := Scope.FindID(ID.Name);
        if not Assigned(DeclAbsolute) then
          AbortWork(sUndeclaredIdentifier, [ID.Name], ID.TextPosition);
        if DeclAbsolute.ItemType <> itVar then
          AbortWork(sVariableRequired, parser_Position);
        Result := parser_NextToken(Scope);
      end;
    end;

    // deprecated
    if Result = token_deprecated then
      Result := ParseDeprecated(Scope, Deprecated);

    parser_MatchToken(Result, token_semicolon);
    // check and parse procedural type call convention
    if DataType is TIDProcType then
      Result := CheckAndParseProcTypeCallConv(Scope, DataType)
    else
      Result := parser_NextToken(Scope);

    for i := 0 to c do begin
      if not Assigned(Struct) then
        Field := TIDVariable.Create(Scope, Names.Items[i])
      else
        Field := TIDField.Create(Struct, Names.Items[i]);
      // если это слабая ссылка - получаем соответствующий тип
      if IsWeak then
        DataType := GetWeakRefType(Scope, DataType);
      Field.DataType := DataType;
      Field.Visibility := Visibility;

      // if the init value for global var is not constant value
      if Assigned(DefaultValue) and DefaultValue.IsTMPVar then
      begin
        ILWrite(@fInitProcSConect, TIL.IL_Move(TIDExpression.Create(Field), DefaultValue));
      end else
      Field.DefaultValue := DefaultValue;

      Field.Absolute := TIDVariable(DeclAbsolute);
      Field.Flags := Field.Flags + VarFlags;
      if isRef then
        Field.Flags := Field.Flags + [VarNotNull];
      Scope.AddVariable(Field);
    end;

    if Result <> token_identifier then
      Exit;
    c := 0;
  end;
end;

function TNPUnit.ParseRecordType(Scope: TScope; Decl: TIDStructure): TTokenID;
var
  Visibility: TVisibility;
  //Expr: TIDExpression;
  //Ancestor: TIDRecord;
begin
  Visibility := vPublic;
  Result := parser_NextToken(Scope);
  while True do begin
    case Result of
      {token_openround: begin
        Result := ParseConstExpression(Scope, Expr, parser_NextToken(Scope), ExprNested);
        CheckRecordType(Expr);
        Ancestor := TIDRecord(Expr.Declaration);
        if Ancestor = Decl then
          AbortWork(sRecurciveTypeLinkIsNotAllowed, Expr.TextPosition);
        parser_MatchToken(Result, token_closeround);
        Decl.Ancestor := Ancestor;
        Result := parser_NextToken(Scope);
      end;}
      token_case: Result := ParseCaseRecord(Decl.Members, Decl);
      token_union: Result := ParseUnionSection(Decl.Members, Decl);
      token_class: Result := parser_NextToken(scope);
      token_procedure: Result := ParseProcedure(Decl.Members, ptProc, Decl);
      token_function: Result := ParseProcedure(Decl.Members, ptFunc, Decl);
      token_constructor: Result := ParseProcedure(Decl.Members, ptConstructor, Decl);
      token_destructor: Result := ParseProcedure(Decl.Members, ptDestructor, Decl);
      token_property: Result := ParseProperty(Decl);
      token_operator: Result := ParseOperator(Decl.Members, Decl);
      token_public: begin
        Visibility := vPublic;
        Result := parser_NextToken(Scope);
      end;
      token_private: begin
        Visibility := vPrivate;
        Result := parser_NextToken(Scope);
      end;
      token_strict: begin
        Result := parser_NextToken(Scope);
        case Result of
          token_private: Visibility := vStrictPrivate;
          token_protected: Visibility := vStrictProtected;
        else
          ERROR_EXPECTED_TOKEN(token_private);
        end;
        Result := parser_NextToken(Scope);
      end;
      token_var: begin
        Result := parser_NextToken(Scope);
        Result := ParseVarSection(Decl.Members, Visibility, Decl);
      end;
      token_const: Result := ParseConstSection(Decl.Members);
      token_type: Result := ParseNamedTypeDecl(Decl.Members);
      token_identifier, token_name: Result := ParseVarSection(Decl.Members, Visibility, Decl); // необходимо оптимизировать парсинг ключевых слов как идентификаторов
    else
      break;
    end;
  end;
  CheckIncompleteType(Decl.Members);
  Decl.StructFlags := Decl.StructFlags + [StructCompleted];
  CheckAndMakeInitFinalStruct(Decl);

  parser_MatchToken(Result, token_end);
  Result := parser_NextToken(Scope);
  if Result = token_external then
    Result := ParseImportStatement(Scope, Decl);

  if Result = token_platform then
    Result := parser_NextToken(Scope);

  Result := CheckAndParseDeprecated(Scope, Result);
end;

function TNPUnit.ParseRepeatStatement(Scope: TScope; SContext: PSContext): TTokenID;
var
  Expression: TIDExpression;
  EContext: TEContext;
  ToBeginJump: TILJmp;
  LContext: PLContext;
  NewSContext: TSContext;
  FirstLoopBodyCode: TILInstruction;
begin
  LContext := PLContext(FLoopPool.Add);
  LContext.TryContext := SContext.TryBlock;
  LContext.Parent := SContext.LContext;
  FirstLoopBodyCode := SContext.ILLast;
  LContext.BeginInstuction := TIL.IL_Nope;
  LContext.EndInstruction := TIL.IL_Nope;

  NewSContext.Assign(SContext);
  NewSContext.LContext := LContext;

  parser_NextToken(Scope);
  // тело цикла
  CFBBegin(SContext, CFB_WHILE_BODY);
  Result := ParseStatements(Scope, @NewSContext, True);
  parser_MatchToken(Result, token_until);

  ILWrite(@NewSContext, LContext.BeginInstuction);

  // выражение цикла
  InitEContext(EContext, @NewSContext, ExprRValue);
  Result := ParseExpression(Scope, EContext, parser_NextToken(Scope));
  Expression := EContext.Result;
  CheckEmptyExpression(Expression);
  CheckBooleanExpression(Expression);

  case Expression.ItemType of
    itVar: if not (Expression is TIDBoolResultExpression) then
    begin // Если if условие соотоит из одной boolean переменной
      ILWrite(@NewSContext, TIL.IL_Test(Expression, Expression));
      if Assigned(FirstLoopBodyCode) then
        FirstLoopBodyCode := FirstLoopBodyCode.Next;
      ToBeginJump := TIL.IL_Jmp(Expression.Line, cNone, FirstLoopBodyCode);
      ToBeginJump.Condition := cZero;
      ILWrite(@NewSContext, ToBeginJump);
    end;
    itConst: begin
      if Expression.AsBoolConst.Value = False then
      begin
        if Assigned(FirstLoopBodyCode) then
          FirstLoopBodyCode := FirstLoopBodyCode.Next;
        ToBeginJump := TIL.IL_Jmp(Expression.Line, cNone, FirstLoopBodyCode);
        ILWrite(@NewSContext, ToBeginJump);
      end;
    end;
  end;

  {если выражение ложно - переходим на начало цикла}
  if Assigned(EContext.LastBoolNode) then begin
    if not Assigned(FirstLoopBodyCode) then
    begin
      FirstLoopBodyCode := TIL.IL_Nope;
      ILWriteFirst(@NewSContext, FirstLoopBodyCode);
    end;
    Bool_CompleteExpression(EContext.LastBoolNode, FirstLoopBodyCode);
  end;

  SContext.IL.ReplaceJmpTarget(LContext.EndInstruction, SContext.ILLast);
  CFBEnd(SContext, CFB_WHILE_BODY);
end;

function TNPUnit.ParseTypeMember(Scope: TScope; Struct: TIDStructure): TTokenID;
begin
  Result := parser_NextToken(Scope);
  case Result of
    token_procedure: Result := ParseProcedure(Struct.Members, ptClassProc, Struct);
    token_function: Result := ParseProcedure(Struct.Members, ptClassFunc, Struct);
    token_property: Result := ParseProperty(Struct);
    token_operator: Result := ParseOperator(Struct.Members, Struct);
    token_var: ParseVarSection(Struct.Members, vLocal, Struct);
  else
    ERROR_PROC_OR_PROP_OR_VAR_REQUIRED;
  end;
end;

function TNPUnit.ParseClassOfType(Scope: TScope; const ID: TIdentifier; out Decl: TIDClassOf): TTokenID;
var
  RefType: TIDClass;
  Expr: TIDExpression;
begin
  parser_NextToken(Scope);
  Result := ParseConstExpression(Scope, Expr, ExprRValue);
  CheckClassType(Expr);
  RefType := TIDClass(Expr.Declaration);
  Decl := TIDClassOf.Create(Scope, ID);
  Decl.ReferenceType := RefType;
  InsertToScope(Scope, Decl);
end;

class procedure TNPUnit.CheckDestructorSignature(const DProc: TIDProcedure);
begin
  if LowerCase(DProc.Name) <> 'destroy' then
    AbortWork('Destructor must has a "Destroy()" signature', DProc.TextPosition);
  if DProc.ParamsCount > 0 then
    AbortWork('Destructor must be without parameters', DProc.TextPosition);
end;

class function TNPUnit.CheckSetImplicit(Source: TIDExpression; Destination: TIDSet): TIDDeclaration;
var
  i: Integer;
  CArray: TIDDynArrayConstant;
  SExpr: TIDExpression;
  ImplicitCast: TIDDeclaration;
  EnumDecl: TIDType;
begin
  EnumDecl := Destination.BaseType;
  CArray := Source.AsDynArrayConst;
  for i := 0 to CArray.ArrayLength - 1 do begin
    SExpr := CArray.Value[i];
    ImplicitCast := CheckImplicit(SExpr,  EnumDecl);
    if not Assigned(ImplicitCast) then
      ERROR_INCOMPATIBLE_TYPES(SExpr, EnumDecl);

  end;
  Result := Destination;
end;

class procedure TNPUnit.CheckSetType(Expression: TIDExpression);
begin
  if Expression.DataTypeID <> dtSet then
    AbortWork('SET type required', Expression.TextPosition);
end;

class procedure TNPUnit.CheckStaticRecordConstructorSign(const CProc: TIDProcedure);
begin
  if CProc.ParamsCount > 0 then
    AbortWork('Static record CONSTRUCTOR or DESTRUCTOR must be parameterless', CProc.TextPosition);
end;

function TNPUnit.ParseClassAncestorType(Scope, GenericScope: TScope; GDescriptor: PGenericDescriptor; ClassDecl: TIDClass): TTokenID;
var
  Expr: TIDExpression;
  Decl: TIDType;
  i: Integer;
begin
  i := 0;
  while True do begin
    parser_NextToken(Scope);
    Result := ParseConstExpression(Scope, Expr, ExprNested);
    if Assigned(Expr) then
    begin
      CheckClassOrIntfType(Expr);
      Decl := Expr.AsType;
      // проверка на зацикливание на себя
      if Decl = ClassDecl then
        AbortWork(sRecurciveTypeLinkIsNotAllowed, Expr.TextPosition);
    end else begin
      ERROR_CLASS_OR_INTF_TYPE_REQUIRED(parser_PrevPosition);
      Decl := nil;
    end;

    if (Decl.DataTypeID = dtClass) then
    begin
      if i = 0 then
        ClassDecl.Ancestor := TIDClass(Decl)
      else
        AbortWork('Multiple inheritance is not supported', Expr.TextPosition);
    end else begin
      if ClassDecl.FindInterface(TIDInterface(Decl)) then
        ERROR_INTF_ALREADY_IMPLEMENTED(Expr);
      ClassDecl.AddInterface(TIDInterface(Decl));
    end;

    inc(i);
    if Result = token_coma then
      continue;
    break;
  end;
  parser_MatchToken(Result, token_closeround);
  Result := parser_NextToken(Scope);
end;

function TNPUnit.ParseClassType(Scope, GenericScope: TScope; GDescriptor: PGenericDescriptor; const ID: TIdentifier; out Decl: TIDClass): TTokenID;
var
  Visibility: TVisibility;
  //Ancestor: TIDClass;
  FwdDecl: TIDDeclaration;
begin
  Visibility := vPublic;

  FwdDecl := Scope.FindID(ID.Name);
  if not Assigned(FwdDecl) then
  begin
    Decl := TIDClass.Create(Scope, ID);
    if Assigned(GenericScope) then
      Decl.Members.AddScope(GenericScope);
    Decl.GenericDescriptor := GDescriptor;
    if not Assigned(GDescriptor) then
      InsertToScope(Scope, Decl)
    else
      InsertToScope(Scope, GDescriptor.SearchName, Decl);
  end else begin
    if (FwdDecl.ItemType = itType) and (TIDType(FwdDecl).DataTypeID = dtClass) and TIDType(FwdDecl).NeedForward then
      Decl := FwdDecl as TIDClass
    else
      ERROR_ID_REDECLARATED(FwdDecl);
  end;

  Result := parser_CurTokenID;
  if Result = token_openround then
  begin
    Result := ParseClassAncestorType(Scope, GenericScope, GDescriptor, Decl);
  end else begin
    if Self <> SYSUnit then
      Decl.Ancestor := SYSUnit._TObject;
  end;

  // если найден символ ; - то это forward-декларация
  if Result = token_semicolon then
  begin
    if Decl.NeedForward then
      ERROR_ID_REDECLARATED(ID);
    Decl.NeedForward := True;
    Exit;
  end;

  while True do begin
    case Result of
      token_openblock: Result := ParseAttribute(Scope);
      token_class: Result := ParseTypeMember(Scope, Decl);
      token_procedure: Result := ParseProcedure(Decl.Members, ptProc, Decl);
      token_function: Result := ParseProcedure(Decl.Members, ptFunc, Decl);
      token_property: Result := ParseProperty(Decl);
      token_operator: Result := ParseOperator(Decl.Members, Decl);
      token_constructor: Result := ParseProcedure(Decl.Members, ptConstructor, Decl);
      token_destructor: Result := ParseProcedure(Decl.Members, ptDestructor, Decl);
      token_var: begin
        Result := parser_NextToken(Scope);
        Result := ParseVarSection(Decl.Members, Visibility, Decl);
      end;
      token_const: Result := ParseConstSection(Decl.Members);
      token_type: Result := ParseNamedTypeDecl(Decl.Members);
      token_public: begin
        Visibility := vPublic;
        Result := parser_NextToken(Scope);
      end;
      token_private: begin
        Visibility := vPrivate;
        Result := parser_NextToken(Scope);
      end;
      token_protected: begin
        Visibility := vProtected;
        Result := parser_NextToken(Scope);
      end;
      token_strict: begin
        Result := parser_NextToken(Scope);
        case Result of
          token_private: Visibility := vStrictPrivate;
          token_protected: Visibility := vStrictProtected;
        else
          ERROR_EXPECTED_TOKEN(token_private);
        end;
        Result := parser_NextToken(Scope);
      end;
      token_identifier: begin
        Result := ParseVarSection(Decl.Members, Visibility, Decl);
      end
      else break;
    end;
  end;
  CheckIncompleteType(Decl.Members);
  Decl.StructFlags := Decl.StructFlags + [StructCompleted];
  parser_MatchToken(Result, token_end);
  CheckAndMakeInitFinalStruct(Decl);
  Result := parser_NextToken(Scope);
  if Result = token_external then
    Result := ParseImportStatement(Scope, Decl);
end;

function TNPUnit.ParseIndexedPropertyArgs(Scope: TScope; out ArgumentsCount: Integer; var EContext: TEContext): TTokenID;
var
  Expr: TIDExpression;
  InnerEContext: TEContext;
  SContext: PSContext;
begin
  ArgumentsCount := 0;
  SContext := EContext.SContext;
  InitEContext(InnerEContext, SContext, ExprNested);
  {цикл парсинга аргументов}
  while true do begin
    Result := parser_NextToken(Scope);
    Result := ParseExpression(Scope, InnerEContext, Result);
    Expr := InnerEContext.Result;
    if Assigned(Expr) then begin
      if Expr.DataType = SYSUnit._Boolean then
      begin
        if (Expr.ItemType = itVar) and Expr.IsAnonymous then
          Bool_CompleteImmediateExpression(InnerEContext, Expr);
      end;
      RPNPushExpression(EContext, Expr);
    end else begin
      // Добавляем пустой Expression для значения по умолчанию
      RPNPushExpression(EContext, nil);
    end;
    Inc(ArgumentsCount);
    case Result of
      token_coma: begin
        InnerEContext.Reset;
        continue;
      end;
      token_closeblock: begin
        Result := parser_NextToken(Scope);
        Break;
      end;
      else
        AbortWork(sIncompleteStatement, parser_PrevPosition);
    end;
  end;
end;

function TNPUnit.ParseInheritedStatement(Scope: TScope; var EContext: TEContext): TTokenID;
var
  Proc, PrevProc: TIDProcedure;
  CallExpr: TIDCallExpression;
  CallArgs: TIDExpressions;
  i, ArgsCnt: Integer;
  ResultExpr: TIDExpression;
  Decl: TIDDeclaration;
  Ancestor: TIDStructure;
  ID: TIdentifier;
begin
  Proc := EContext.SContext.Proc;
  if not (pfOverride in Proc.Flags) then
    ERROR_INHERITED_ALLOWED_ONLY_IN_OVERRIDE_METHODS;

  Result := parser_NextToken(Scope);
  if Result = token_identifier then
  begin
    {если после inherited идет полное описание вызова метода}
    ArgsCnt := 0;
    PrevProc := nil;
    Ancestor := Proc.Struct.Ancestor;
    while True do begin
      parser_ReadCurrIdentifier(ID);
      Decl := FindID(Ancestor.Members, ID);
      if not Assigned(Decl) then
        ERROR_UNDECLARED_ID(ID);
      case Decl.ItemType of
        itProcedure: begin
          PrevProc := Decl as TIDProcedure;
          if PrevProc.Name <> Proc.Name then
            ERROR_NO_METHOD_IN_BASE_CLASS(PrevProc);
          Result := parser_NextToken(Scope);
          if Result = token_openround then
            Result := ParseEntryCall(Scope, EContext.SContext, CallArgs);
          ArgsCnt := Length(CallArgs);
          Break;
        end;
        itType: begin
          if TIDType(Decl).DataTypeID <> dtClass then
            ERROR_CLASS_TYPE_REQUIRED(parser_Position);
          Ancestor := Decl as TIDStructure;
          if (Ancestor = Proc.Struct) or not Proc.Struct.IsInheritsForm(Ancestor) then
            ERROR_TYPE_IS_NOT_AN_ANCESTOR_FOR_THIS_TYPE(Ancestor, Proc.Struct);
          Result := parser_NextToken(Scope);
          parser_MatchToken(Result, token_dot);
          Result := parser_NextToken(Scope);
          continue;
        end;
      else
        ERROR_PROC_OR_TYPE_REQUIRED(ID);
        Exit;
      end;
      Break;
    end;
  end else begin
    {если после inherited ничего больше нет, заполняем аргументы параметрами (если есть)}
    ArgsCnt := Length(Proc.ExplicitParams);
    SetLength(CallArgs, ArgsCnt);
    for i := 0 to ArgsCnt - 1 do
      CallArgs[i] := TIDExpression.Create(Proc.ExplicitParams[i]);
    PrevProc := Proc.Struct.FindVirtualProcInAncestor(Proc);
    if not Assigned(PrevProc) then
      ERROR_NO_METHOD_IN_BASE_CLASS(Proc);
  end;

  CallExpr := TIDCallExpression.Create(PrevProc, parser_Line);
  CallExpr.Instance := TIDExpression.Create(Proc.SelfParameter);
  CallExpr.ArgumentsCount := ArgsCnt;

  ResultExpr := Process_CALL_direct(EContext.SContext, CallExpr, CallArgs);
  if Assigned(ResultExpr) then
    RPNPushExpression(EContext, ResultExpr);
end;

function TNPUnit.ParseInitSection: TTokenID;
var
  SContext: TSContext;
begin
  if FInitProcExplicit then
    ERROR_INIT_SECTION_ALREADY_DEFINED;

  FInitProcExplicit := True;
  SContext.Initialize;
  SContext.IL := TIL(FInitProc.IL);
  SContext.Proc := FInitProc;
  parser_NextToken(FInitProc.Scope);
  FInitProc.FirstBodyLine := parser_Line;
  Result := ParseStatements(FInitProc.Scope, @SContext, True);
  FInitProc.LastBodyLine := parser_Line;
end;

function TNPUnit.ParseInplaceVarDecl(Scope: TScope; out Expression: TIDExpression): TTokenID;
var
  ID: TIdentifier;
  Variable: TIDVariable;
begin
  parser_ReadNextIdentifier(Scope,  ID);

  Variable := TIDVariable.Create(Scope, ID, _Void, []);
  Scope.AddVariable(Variable);
  Expression := TIDExpression.Create(Variable, parser_Position);

  Result := parser_NextToken(Scope);
end;

function TNPUnit.ParseFinalSection: TTokenID;
var
  SContext: TSContext;
begin
  if FFinalProcExplicit then
    ERROR_FINAL_SECTION_ALREADY_DEFINED;
  FFinalProcExplicit := True;
  SContext.Initialize;
  SContext.IL := TIL(FFinalProc.IL);
  SContext.Proc := FFinalProc;
  parser_NextToken(FFinalProc.Scope);
  FFinalProc.FirstBodyLine := parser_Line;
  Result := ParseStatements(FFinalProc.Scope, @SContext, True);
  FFinalProc.LastBodyLine := parser_Line;
end;

function TNPUnit.ParseIntfGUID(Scope: TScope; Decl: TIDInterface): TTokenID;
var
  UID: TIDExpression;
  GUID: TGUID;
begin
  Result := parser_NextToken(Scope);
  if parser_IdentifireType = itString then
  begin
    Result := ParseConstExpression(Scope, UID, ExprNested);
    CheckEmptyExpression(UID);
    CheckStringExpression(UID);

    try
      GUID := StringToGUID(UID.AsStrConst.Value);
    except
      AbortWork('Invalid GUID', parser_Position);
    end;

    Decl.GUID := GUID;

    parser_MatchToken(Result, token_closeblock);
    Result := parser_NextToken(Scope);
  end else
    Result := ParseAttribute(Scope);
end;

function TNPUnit.ParseClosureCapturedVarsDecl(Scope: TScope; out CapturedVars: TIDClosure.TCapturedVars): TTokenID;
begin
  // todo
  parser_MatchNextToken(Scope, token_closeblock);
  Result := parser_NextToken(Scope);
end;

function TNPUnit.ParseLambdaExpression(Scope: TScope; var EContext: TEContext): TTokenID;
var
  LPContext: TLambaParseContext;
  ResultParam: TIDVariable;
begin
  Result := parser_NextToken(Scope);
  // парсим параметры захывата
  if Result = token_openblock then
    Result := ParseClosureCapturedVarsDecl(Scope, LPContext.CapturedVars);

  LPContext.VarSpace.Initialize;
  LPContext.Parameters := TProcScope.CreateInDecl(Scope, @LPContext.VarSpace, nil);
  ResultParam := AddResultParameter(LPContext.Parameters);
  ResultParam.TextPosition := parser_Position;

  // парсим параметры анонимки
  if Result = token_openround then
  begin
    ParseParameters(LPContext.Parameters);
    Result := parser_NextToken(Scope);
  end;

  // парсим тип возвращаемого результата
  if Result = token_colon then
  begin
    ParseTypeSpec(Scope, LPContext.ResultType);
    ResultParam.DataType := LPContext.ResultType;
  end else begin
    LPContext.ResultType := nil;
    LPContext.Parameters.RemoveVariable(ResultParam);
  end;

  Result := ParseLambdaExpressionBody(Scope, EContext, LPContext);
end;

function TNPUnit.ParseLambdaExpressionBody(Scope: TScope; var EContext: TEContext; const LPContext: TLambaParseContext): TTokenID;
var
  Expr: TIDExpression;
  Proc: TIDProcedure;
  LSContext: TSContext;
begin
  Proc := TIDProcedure.CreateAsAnonymous(FImplScope);
  Proc.FirstBodyLine := parser_Line;
  Proc.IL := TIL.Create(Proc);
  Proc.VarSpace := LPContext.VarSpace;
  Proc.ParamsScope := LPContext.Parameters;
  Proc.EntryScope := LPContext.Parameters;
  Proc.ResultType := LPContext.ResultType;
  Proc.ExplicitParams := ScopeToVarList(LPContext.Parameters, IfThen(Assigned(LPContext.ResultType), 1, 0));
  Proc.CreateProcedureTypeIfNeed(Scope);
  FImplScope.AddAnonymousProcedure(Proc);

  LSContext.Initialize;
  LSContext.Proc := Proc;
  LSContext.IL := TIL(Proc.IL);
  CheckInitVariables(@LSContext, nil, @Proc.VarSpace);
  // парсим тело
  Result := ParseStatements(LPContext.Parameters, addr(LSContext), False);
  Proc.LastBodyLine := parser_Line;
  TIL(Proc.IL).Complete(FRCPathCount);
  CheckUnusedVariables(@Proc.VarSpace);
  // геренация кода процедуры завершено
  Proc.Flags := Proc.Flags + [pfCompleted];
  FBENodesPool.Clear;

  Expr := TIDExpression.Create(Proc, parser_PrevPosition);
  RPNPushExpression(EContext, Expr);
end;

function TNPUnit.ParseInterfaceType(Scope, GenericScope: TScope; GDescriptor: PGenericDescriptor; const ID: TIdentifier; out Decl: TIDInterface): TTokenID;
var
  FwdDecl: TIDDeclaration;
  Expr: TIDExpression;
  Ancestor: TIDInterface;
  SearchName: string;
begin
  if not Assigned(GDescriptor) then
    SearchName := ID.Name
  else
    SearchName := GDescriptor.SearchName;

  FwdDecl := Scope.FindID(SearchName);
  if not Assigned(FwdDecl) then
  begin
    Decl := TIDInterface.Create(Scope, ID);
    if Assigned(GenericScope) then
      Decl.Members.AddScope(GenericScope);
    Decl.GenericDescriptor := GDescriptor;
    if not Assigned(GDescriptor) then
      InsertToScope(Scope, Decl)
    else
      InsertToScope(Scope, GDescriptor.SearchName, Decl);
    //InsertToScope(Scope, Decl);
  end else
  begin
    if (FwdDecl.ItemType = itType) and (TIDType(FwdDecl).DataTypeID = dtInterface) and TIDType(FwdDecl).NeedForward then
      Decl := FwdDecl as TIDInterface
    else begin
      ERROR_ID_REDECLARATED(FwdDecl);
    end;
  end;

  Result := parser_NextToken(Scope);
  if Result = token_openround then
  begin
    parser_NextToken(Scope);
    Result := ParseConstExpression(Scope, Expr, ExprNested);
    CheckInterfaceType(Expr);
    Ancestor := TIDInterface(Expr.Declaration);
    if Ancestor = Decl then
      AbortWork(sRecurciveTypeLinkIsNotAllowed, Expr.TextPosition);
    parser_MatchToken(Result, token_closeround);
    Decl.Ancestor := Ancestor;
    Result := parser_NextToken(Scope);
  end;

  // если найден символ ; - то это forward-декларация
  if Result = token_semicolon then
  begin
    if Decl.NeedForward then
      ERROR_ID_REDECLARATED(ID);
    Decl.NeedForward := True;
    Exit;
  end;

  if Result = token_openblock then
    Result := ParseIntfGUID(Scope, Decl);

  while True do begin
    case Result of
      token_openblock: Result := ParseAttribute(Scope);
      token_procedure: Result := ParseProcedure(Decl.Members, ptProc, Decl);
      token_function: Result := ParseProcedure(Decl.Members, ptFunc, Decl);
      token_property: Result := ParseProperty(Decl);
    else
      break;
    end;
  end;
  CheckIncompleteType(Decl.Members);
  Decl.StructFlags := Decl.StructFlags + [StructCompleted];
  parser_MatchToken(Result, token_end);
  Result := parser_NextToken(Scope);
  if Result = token_external then
    Result := ParseImportStatement(Scope, Decl);
end;

function TNPUnit.ParseTypeOf(Scope: TScope; out Decl: TIDType): TTokenID;
var
  EContext: TEContext;
  Expr: TIDExpression;
begin
  InitEContext(EContext, nil, ExprRValue);
  Result := ParseExpression(Scope, EContext, parser_NextToken(Scope));
  Expr := EContext.Result;
  CheckEmptyExpression(Expr);
  if Expr.ExpressionType <> etDeclaration then
    AbortWork(sSingleExpressionRequired, FParser.Position);
  Decl := TIDType(Expr.Declaration);
  if Decl.ItemType <> itType then
    Decl := Decl.DataType;
end;

function TNPUnit.ParseTypeRecord(Scope, GenericScope: TScope; GDescriptor: PGenericDescriptor; const ID: TIdentifier; out Decl: TIDType): TTokenID;
{var
  TypeScope: TScope;}
begin
  {if Assigned(GDescriptor) then
    TypeScope := GDescriptor.Scope
  else
    TypeScope := Scope;}

  Decl := TIDRecord.Create(Scope, ID);
  Decl.GenericDescriptor := GDescriptor;
  if Assigned(GenericScope) then
    TIDRecord(Decl).Members.AddScope(GenericScope);

  if ID.Name <> '' then begin
    if Assigned(GDescriptor) then
      InsertToScope(Scope, GDescriptor.SearchName, Decl)
    else
      InsertToScope(Scope, Decl);
  end;

  Result := ParseRecordType(Scope, TIDRecord(Decl));
end;

function TNPUnit.ParseTypeArray(Scope, GenericScope: TScope; GDescriptor: PGenericDescriptor; const ID: TIdentifier; out Decl: TIDType): TTokenID;
var
  TypeScope: TScope;
  DataType: TIDType;
begin
  if Assigned(GDescriptor) then
    TypeScope := GDescriptor.Scope
  else
  if Assigned(GenericScope) then
    TypeScope := GenericScope
  else
    TypeScope := Scope;

  Result := parser_NextToken(Scope);
  if Result = token_openblock then
  begin
    {static array}
    Decl := TIDArray.Create(Scope, ID);
    Decl.GenericDescriptor := GDescriptor;
    if ID.Name <> '' then begin
      if Assigned(GDescriptor) then
        InsertToScope(Scope, GDescriptor.SearchName, Decl)
      else
        InsertToScope(Scope, Decl);
    end;
    Result := ParseStaticArrayType(TypeScope, TIDArray(Decl));
  end else begin
    {dynamic array}
    parser_MatchToken(Result, token_of);
    Decl := TIDDynArray.Create(Scope, ID);
    Decl.GenericDescriptor := GDescriptor;
    if ID.Name <> '' then begin
      if Assigned(GDescriptor) then
        InsertToScope(Scope, GDescriptor.SearchName, Decl)
      else
        InsertToScope(Scope, Decl);
    end;
    Result := ParseTypeSpec(TypeScope, DataType);
    TIDDynArray(Decl).ElementDataType := DataType;
  end;
  CheckAndMakeInitFinalArray(TIDArray(Decl));
end;

function TNPUnit.ParseTypeDecl(Scope, GenericScope: TScope; GDescriptor: PGenericDescriptor; const ID: TIdentifier; out Decl: TIDType): TTokenID;
var
  DataType: TIDType;
  IsPacked: Boolean;
  IsAnonimous: Boolean;
  TmpID: TIdentifier;
begin
  Result := parser_CurTokenID;

  // является ли тип анонимным
  IsAnonimous := (ID.Name = '');

  // является ли тип упакованным
  if Result <> token_packed then
    IsPacked := False
  else begin
    IsPacked := True;
    Result := parser_NextToken(Scope);
  end;

  case Result of
    /////////////////////////////////////////////////////////////////////////
    // type of
    /////////////////////////////////////////////////////////////////////////
    token_type: begin
      Result := parser_NextToken(Scope);
      var ResExpr: TIDExpression;
      Result := ParseConstExpression(Scope,  ResExpr, ExprRValue);
      if ResExpr.ItemType = itType then
      begin
        Decl := TIDAliasType.CreateAlias(Scope, ID, ResExpr.AsType);
        InsertToScope(Scope, Decl);
      end;
      Exit;
    end;
    /////////////////////////////////////////////////////////////////////////
    // pointer type
    /////////////////////////////////////////////////////////////////////////
    token_caret: begin
      parser_ReadNextIdentifier(Scope, TmpID);
      DataType := TIDType(FindIDNoAbort(Scope, TmpID));
      if Assigned(DataType) then
      begin
        if DataType.ItemType <> itType then
          AbortWork(sTypeIdExpectedButFoundFmt, [TmpID.Name], FParser.Position);
        if IsAnonimous then begin
          Decl := DataType.GetDefaultReference(Scope);
          {признак, что этот анонимный тип является ссылкой на структуру которая еще не закончена}
          if (TIDPointer(Decl).ReferenceType is TIDStructure) and (Scope.ScopeType = stStruct) and
             (TIDPointer(Decl).ReferenceType = TStructScope(Scope).Struct) then
            TIDPointer(Decl).NeedForward := True;
        end else begin
          Decl := TIDPointer.Create(Scope, ID);
          TIDPointer(Decl).ReferenceType := DataType;
          InsertToScope(Scope, Decl);
        end;
      end else begin
        Decl := TIDPointer.Create(Scope, ID);
        TIDPointer(Decl).ForwardID := TmpID;
        TIDPointer(Decl).NeedForward := True;
        InsertToScope(Scope, Decl);
      end;
      Result := parser_NextToken(Scope);
    end;
    /////////////////////////////////////////////////////////////////////////
    // helper type
    /////////////////////////////////////////////////////////////////////////
    token_helper: Result := ParseTypeHelper(Scope, GenericScope, GDescriptor, ID, Decl);
    /////////////////////////////////////////////////////////////////////////
    // array type
    /////////////////////////////////////////////////////////////////////////
    token_array: Result := ParseTypeArray(Scope, GenericScope, GDescriptor, ID, Decl);
    /////////////////////////////////////////////////////////////////////////
    // procedural type
    /////////////////////////////////////////////////////////////////////////
    token_function: Result := ParseProcType(Scope, ID, TProcType.ptFunc, TIDProcType(Decl));
    token_procedure: Result := ParseProcType(Scope, ID, TProcType.ptProc, TIDProcType(Decl));
    /////////////////////////////////////////////////////////////////////////
    // open array
    /////////////////////////////////////////////////////////////////////////
    (*token_openarray: begin
      if not IsAnonimous then
        AbortWork(sOpenArrayAllowedOnlyAsTypeOfParam, FParser.Position);
      parser_MatchToken(parser_NextToken(Scope), token_of);
      Decl := TIDOpenArray.CreateAsAnonymous(Scope);
      Result := ParseTypeSpec(Scope, DataType);
      TIDArray(Decl).ElementDataType := DataType;
    end;*)
    /////////////////////////////////////////////////////////////////////////
    // set
    /////////////////////////////////////////////////////////////////////////
    token_set: begin
      Decl := TIDSet.Create(Scope, ID);
      if not IsAnonimous then
        InsertToScope(Scope, Decl);
      Result := ParseSetType(Scope, TIDSet(Decl));
    end;
    /////////////////////////////////////////////////////////////////////////
    // enum
    /////////////////////////////////////////////////////////////////////////
    token_openround: begin
      Decl := TIDEnum.Create(Scope, ID);
      if not IsAnonimous then
        InsertToScope(Scope, Decl)
      else {если это анонимная декларация типа для параметра, то добавляем его в более высокий scope}
      if Assigned(Scope.Parent) and (Scope.ScopeClass = scProc) then
        Scope := Scope.Parent;
      ParseEnumType(Scope, TIDEnum(Decl));
      Result := parser_NextToken(Scope);
    end;
    /////////////////////////////////////////////////////////////////////////
    // record
    /////////////////////////////////////////////////////////////////////////
    token_record: Result := ParseTypeRecord(Scope, GenericScope, GDescriptor, ID, Decl);
    /////////////////////////////////////////////////////////////////////////
    // class
    /////////////////////////////////////////////////////////////////////////
    token_class: begin
      Result := parser_NextToken(Scope);
      if Result = token_of then
        Result := ParseClassOfType(Scope, ID, TIDClassOf(Decl))
      else begin
        if IsAnonimous then
          AbortWork(sClassTypeCannotBeAnonimous, FParser.PrevPosition);
        Result := ParseClassType(Scope, GenericScope, GDescriptor, ID, TIDClass(Decl));
      end;
    end;
    /////////////////////////////////////////////////////////////////////////
    // interface
    /////////////////////////////////////////////////////////////////////////
    token_interface: Result := ParseInterfaceType(Scope, GenericScope, GDescriptor, ID, TIDInterface(Decl));
    /////////////////////////////////////////////////////////////////////////
    // other
    /////////////////////////////////////////////////////////////////////////
    token_identifier: begin
      var ResExpr: TIDExpression;
      Result := ParseConstExpression(Scope, ResExpr, ExprRValue);
      {alias type}
      if ResExpr.ItemType = itType then begin
        Decl := TIDAliasType.CreateAlias(Scope, ID, ResExpr.AsType);
      end else
      {range type}
      begin
        Decl := TIDRangeType.Create(Scope, ID);
        ParseRangeType(Scope, ResExpr, TIDRangeType(Decl));
      end;
      if not IsAnonimous then
        InsertToScope(Scope, Decl);
    end;
    else begin
      Decl := nil;
      Exit;
    end;
  end;
  // добовляем тип в пул
  Decl.IsPacked := IsPacked;
  AddType(Decl);
end;

function TNPUnit.ParseGenericTypeSpec(Scope: TScope; const ID: TIdentifier; out DataType: TIDType): TTokenID;
//var
//  SContext: TSContext;
//  GenericArgs: TIDExpressions;
//  SearchName: string;
begin
  Assert(False);
//  SContext.Initialize;
//  Result := ParseGenericsArgs(Scope, @SContext, GenericArgs);
//  SearchName := format('%s<%d>', [ID.Name, Length(GenericArgs)]);
//  DataType := TIDType(FindIDNoAbort(Scope, SearchName));
//  if Assigned(DataType) then
//    DataType := SpecializeGenericType(DataType, ID, GenericArgs)
//  else
//    AbortWorkInternal('Invalid generic type params');
end;

function TNPUnit.ParseTypeHelper(Scope, GenericScope: TScope; GDescriptor: PGenericDescriptor;
                               const ID: TIdentifier; out Decl: TIDType): TTokenID;
var
  HelpedID: TIdentifier;
  HelpedDecl: TIDType;
begin
  parser_ReadToken(Scope, token_for);

  parser_ReadNextIdentifier(Scope, HelpedID);
  HelpedDecl := TIDType(FindID(Scope, HelpedID));
  if HelpedDecl.ItemType <> itType then
    ERROR_TYPE_REQUIRED(parser_PrevPosition);



  parser_ReadToken(Scope, token_end);
  Result := parser_NextToken(Scope);
end;

function TNPUnit.ParseTypeSpec(Scope: TScope; out DataType: TIDType): TTokenID;
var
  ID: TIdentifier;
  Decl: TIDDeclaration;
  Empty: TIdentifier;
  SearchScope: TScope;
begin
  Result := parser_NextToken(Scope);
  if Result = token_identifier then
  begin
    SearchScope := nil;
    while True do begin
      parser_ReadCurrIdentifier(ID);
      Result := parser_NextToken(Scope);

      {если это специализация обобщенного типа}
      if Result = token_less then
      begin
        Result := ParseGenericTypeSpec(Scope, ID, DataType);
        Exit;
      end;

      if SearchScope = nil then
        Decl := FindIDNoAbort(Scope, ID)
      else
        Decl := SearchScope.FindMembers(ID.Name);

      if not Assigned(Decl) then
      begin
        Decl := FindIDNoAbort(Scope, ID);
        ERROR_UNDECLARED_ID(ID);
      end;

      case Decl.ItemType of
        itType: begin
          DataType := TIDType(Decl).ActualDataType;
          if Result = token_dot then
          begin
            if not (DataType is TIDStructure) then
              ERROR_STRUCT_TYPE_REQUIRED(parser_PrevPosition);
            SearchScope := TIDStructure(DataType).Members;
            parser_NextToken(Scope);
            continue;
          end;
          Exit;
        end;
        itAlias: begin
          Decl := Decl.Original;
          if Decl.ItemType = itType then
          begin
            DataType := Decl as TIDType;
            if Result = token_dot then
            begin
              if not (DataType is TIDStructure) then
                ERROR_STRUCT_TYPE_REQUIRED(parser_PrevPosition);
              SearchScope := TIDStructure(DataType).Members;
              parser_NextToken(Scope);
              continue;
            end;
            Exit;
          end;
        end;
      else
        ERROR_INVALID_TYPE_DECLARATION;
      end;
      Exit;
    end;
    Exit;
  end;
  Result := ParseTypeDecl(Scope, nil, nil, Empty, DataType);
  if not Assigned(DataType) then
    ERROR_INVALID_TYPE_DECLARATION;
end;

function TNPUnit.ParseNamedTypeDecl(Scope: TScope): TTokenID;
var
  ID: TIdentifier;
  Decl: TIDType;
  GenericParams: TIDTypeList;
  ParserPos: TParserPosition;
  GDescriptor: PGenericDescriptor;
begin
  Result := parser_NextToken(Scope);
  parser_MatchIdentifier(Result);
  repeat
    parser_ReadCurrIdentifier(ID);

    Result := parser_NextToken(Scope);

    {если есть символ "<" - читаем обобщенные параметры}
    if Result = token_less then begin
      GDescriptor := TGenericDescriptor.Create(Scope);
      Result := ParseGenericsHeader(GDescriptor.Scope, GenericParams);
      GDescriptor.GenericParams := GenericParams;
      GDescriptor.SearchName := format('%s<%d>', [ID.Name, Length(GenericParams)]);
    end else
      GDescriptor := nil;

    parser_MatchToken(Result, token_equal);

    if Assigned(GDescriptor) then
    begin
      FParser.SaveState(ParserPos);
      GDescriptor.IntfSRCPosition := ParserPos;
    end;

    parser_NextToken(Scope);

    Result := ParseTypeDecl(Scope, nil, GDescriptor, ID, Decl);
    if not Assigned(Decl) then
      ERROR_INVALID_TYPE_DECLARATION;

    parser_MatchToken(Result, token_semicolon);

    // check and parse procedural type call convention
    if Decl is TIDProcType then
      Result := CheckAndParseProcTypeCallConv(Scope, Decl)
    else
      Result := parser_NextToken(Scope);

  until Result <> token_identifier;
end;

function TNPUnit.ParseOperator(Scope: TScope; Struct: TIDStructure): TTokenID;
type
  TFwdDeclState = (dsNew, dsDifferent, dsSame);
var
  ID: TIdentifier;
  Parameters: TProcScope;
  ResultType: TIDType;
  ResultParam: TIDVariable;
  VarSpace: TVarSpace;
  GenericsParams: TIDTypeList;
  Proc, ForwardDecl: TIDProcedure;
  FwdDeclState: TFwdDeclState;
  FirstSkipCnt: Integer;
  SRCProcPos: TParserPosition;
  OperatorID: TOperatorID;
  ParamCount: Integer;
  ProcFlags: TProcFlags;
begin
  Result := ParseProcName(Scope, ID, Struct, Parameters, GenericsParams);

  OperatorID := GetOperatorID(ID.Name);
  if OperatorID = opNone then
    AbortWork(sUnknownOperatorFmt, [ID.Name], ID.TextPosition);

  if not Assigned(Struct) then
    ERROR_OPERATOR_MUST_BE_DECLARED_IN_STRUCT(ID.TextPosition);

  VarSpace.Initialize;
  Parameters.VarSpace := @VarSpace;

  // создаем Result переменную
  ResultParam := AddResultParameter(Parameters);

  // если generic
  FParser.SaveState(SRCProcPos);

  // парсим параметры
  parser_MatchToken(Result, token_openround);
  ParseParameters(Parameters);
  Result := parser_NextToken(Scope);

  // проверка на кол-во необходимых параметров
  ParamCount := IfThen(OperatorID < OpIn, 1, 2);
  if ParamCount <> (Parameters.Count - 1) then
    AbortWork(sOperatorNeedNCountOfParameters, [ID.Name, ParamCount], ID.TextPosition);

  // парсим тип возвращаемого значения
  parser_MatchToken(Result, token_colon);
  Result := ParseTypeSpec(Parameters, ResultType);
  ResultParam.DataType := ResultType;
  ResultParam.TextPosition := parser_Position;
  parser_MatchToken(Result, token_semicolon);

  // ищем ранее обьявленную декларацию с таким же именем
  ForwardDecl := TIDProcedure(Struct.Members.FindID(ID.Name));

  Proc := nil;
  FwdDeclState := dsDifferent;

  {если найдена ранее обьявленная декларация, проверяем соответствие}
  if Assigned(ForwardDecl) then begin
    // ошибка если перекрыли идентификатор другого типа:
    if ForwardDecl.ItemType <> itProcedure then
      ERROR_ID_REDECLARATED(ID);
    // ищем подходящую декларацию в списке перегруженных:
    while True do begin
      if ForwardDecl.SameDeclaration(Parameters) then begin
        // нашли подходящую декларацию
        FwdDeclState := dsSame;
        if Assigned(ForwardDecl.IL) then
          ERROR_ID_REDECLARATED(ID);
        Proc := ForwardDecl;
        Break;
      end;
      // не нашли подходящую декларацию, создаем новую
      // проверку дерективы overload оставим на потом
      if not Assigned(ForwardDecl.NextOverload) then
      begin
        Break;
      end;
      ForwardDecl := ForwardDecl.NextOverload;
    end;
  end else
    FwdDeclState := dsNew;

  ProcFlags := [pfOperator];
  {создаем новую декларацию}
  if not Assigned(Proc) then
  begin
    Proc := TIDOperator.Create(Scope, ID, OperatorID);
    if Assigned(GenericsParams) then
      Proc.CreateGenericDescriptor(GenericsParams, SRCProcPos);

    // Для Scope будут переопределены VarSpace и ProcSpace
    Proc.ParamsScope := Parameters;
    Proc.VarSpace := Parameters.VarSpace^;
    Proc.ResultType := ResultType;

    FirstSkipCnt := 0;
    if Assigned(ResultType) then
      Inc(FirstSkipCnt);
    Proc.ExplicitParams := ScopeToVarList(Parameters, FirstSkipCnt);
    Proc.Struct := Struct;

    // добовляем новую декларацию в структуру или глобольный список или к списку перегруженных процедур
    if not Assigned(ForwardDecl) then
    begin
      Struct.Members.AddProcedure(Proc);
    end else begin
      ForwardDecl.NextOverload := Proc;
      Struct.Members.ProcSpace.Add(Proc);
    end;

    case OperatorID of
      opImplicit: begin
      if ResultType = Struct then
        Struct.OverloadImplicitFrom(Parameters.VarSpace.Last.DataType, Proc)
      else
        Struct.OverloadImplicitTo(ResultType, Proc);
      end;
      opExplicit: begin
        if ResultType = Struct then
          Struct.OverloadExplicitFrom(Parameters.VarSpace.Last.DataType, Proc)
        else
          Struct.OverloadExplicitTo(ResultType, Proc);
      end;
    else
    if OperatorID < opIn then
      Struct.OverloadUnarOperator(OperatorID, Proc)
    else
      Struct.OverloadBinarOperator(OperatorID, TIDOperator(Proc));
    end;

  end else begin
    if Assigned(Proc.GenericDescriptor) then
      Proc.GenericDescriptor.ImplSRCPosition := SRCProcPos;
  end;

  Result := parser_NextToken(Scope);
  while True do begin
    case Result of
      token_forward: Result := ProcSpec_Forward(Scope, Proc, ProcFlags);
      token_export: Result := ProcSpec_Export(Scope, Proc, ProcFlags);
      token_inline: Result := ProcSpec_Inline(Scope, Proc, ProcFlags);
      //token_noreturn: Result := ProcSpec_Import(Scope, Proc, ProcFlags);
      token_external: Result := ProcSpec_Import(Scope, Proc, ProcFlags);
      token_overload: Result := ProcSpec_Overload(Scope, Proc, ProcFlags);
      //token_pure: Result := ProcSpec_Pure(Scope, Proc, ProcFlags);
    else
      if (Scope.ScopeClass = scInterface) or (pfImport in ProcFlags) then
      begin
        Proc.Flags := ProcFlags;
        Break;
      end;

      // имена парметров реализации процедуры могут отличатся от ее определения
      // копируем накопленный VarSpace в процедуру
      Proc.VarSpace := Parameters.VarSpace^;
      // Для Scope будут переопределены VarSpace и ProcSpace
      Proc.EntryScope := Parameters;
      if (FwdDeclState = dsDifferent) then
      begin
        if not Assigned(Proc.IL) then
          ERROR_DECL_DIFF_WITH_PREV_DECL(ID);
      end;
      Result := ParseProcBody(Proc, nil);
      parser_MatchSemicolon(Result);
      Result := parser_NextToken(Scope);
      Break;
    end;
    //token_identifier: AbortWork(sKeywordExpected, parser_Position);
  {else
    if Scope.ScopeClass <> scInterface then
      ERROR_PROC_NEED_BODY;
    Break;}
  end;
end;

function TNPUnit.ParseUnionSection(Scope: TScope; Struct: TIDStructure): TTokenID;
var
  Decl: TIDDeclaration;
  UnionType: TIDType;
  ID: TIdentifier;
  Expr: TIDExpression;
  Names: TIdentifiersPool;
  Idx: Integer;
  FieldDataType: TIDType;
begin
  Result := parser_NextToken(Scope);

  // если определен тип union-a
  if Result = token_of then begin
    parser_ReadNextIdentifier(Scope, ID);
    Decl := FindID(Scope, ID);
    if not Assigned(Decl) then
      ERROR_EMPTY_EXPRESSION;
    if (Decl.ItemType <> itType) or (not TIDType(Decl).Ordinal) then
      ERROR_ORDINAL_TYPE_REQUIRED(parser_Position);
    UnionType := TIDType(Decl);
    Result := parser_NextToken(Scope);
  end else
    UnionType := nil;

  Idx := 0;
  Names := TIdentifiersPool.Create(2);
  while true do begin
    // если тип union-а был опеределен, - читаем константу этого типа
    if Assigned(UnionType) then
    begin
      Result := ParseConstExpression(Scope, Expr, ExprNested);
      // пока не проверяем уникальность
      parser_MatchToken(Result, token_colon);
      Result := parser_NextToken(Scope);
    end;

    // парсим поля в круглых скобках
    parser_MatchToken(Result, token_openround);
    parser_NextToken(Scope);
    while true do begin
      parser_ReadCurrIdentifier(Names.Items[Idx]);
      Result := parser_NextToken(Scope);
      if Result = token_Coma then begin
        Inc(idx);
        parser_NextToken(Scope);
        continue;
      end;
      parser_MatchToken(Result, token_colon);
      // парсим тип
      Result := ParseTypeSpec(Scope, FieldDataType);

      case Result of
        token_semicolon: begin
          parser_NextToken(Scope);
          Continue;
        end;
        token_closeround: Break;
      else
        ERROR_UNCLOSED_OPEN_BLOCK; // tmp
      end;
    end;
    Result := parser_NextToken(Scope);
    parser_MatchSemicolon(Result);
    Result := parser_NextToken(Scope);
    if Result = token_end then
      break;
  end;
  parser_ReadSemicolon(Scope);
  Result := parser_NextToken(Scope);
end;

function TNPUnit.ParseUnitName(Scope: TScope; out ID: TIdentifier): TTokenID;
var
  UName: string;
begin
  Result := parser_NextToken(Scope);
  while True do begin
    parser_MatchIdentifier(Result);
    UName := AddStringSegment(UName, FParser.OriginalToken, '.');
    Result := parser_NextToken(Scope);
    if Result <> token_dot then
      break;
    Result := parser_NextToken(Scope);
  end;
  FPackage.GetStringConstant(UName);
  ID := Identifier(UName, parser_PrevPosition);
end;

procedure TNPUnit.ParseUnitDecl(Scope: TScope);
var
  Decl: TIDUnit;
  Token: TTokenID;
begin
  parser_ReadToken(Scope, token_Unit);
  Token := ParseUnitName(Scope, FUnitName);
  parser_MatchSemicolon(Token);
  Decl := TIDUnit.Create(Scope, Self);
  InsertToScope(Scope, Decl);
end;

function TNPUnit.ParseUnsafeStatement(Scope: TScope; Scontext: PSContext): TTokenID;
begin
  parser_ReadToken(Scope, token_do);
  parser_NextToken(Scope);
  Result := ParseStatements(Scope, Scontext, False);
end;

function TNPUnit.ParseUsesSection(Scope: TScope): TTokenID;
var
  Token: TTokenID;
  ID: TIdentifier;
  UUnit: TNPUnit;
  Idx: Integer;
begin
  while true do begin
    Token := ParseUnitName(Scope, ID);

    if ID.Name = Self.FUnitName.Name then
      ERROR_UNIT_RECURSIVELY_USES_ITSELF(ID);

    UUnit := FPackage.UsesUnit(ID.Name, nil) as TNPUnit;
    if not Assigned(UUnit) then
      ERROR_UNIT_NOT_FOUND(ID);

    // если модуль используется первый раз - компилируем
    if not UUnit.Compiled then
    begin
      if UUnit.Compile = CompileFail then begin
        Messages.CopyFrom(UUnit.Messages);
        StopCompile(False);
      end;
    end;

    if not FIntfImportedUnits.Find(ID.Name, Idx) then
      FIntfImportedUnits.AddObject(ID.Name, UUnit)
    else begin
      if (UUnit <> SYSUnit) or FSystemExplicitUse then
        ERROR_ID_REDECLARATED(ID);
    end;

    if UUnit = SYSUnit then
      FSystemExplicitUse := True;

    case Token of
      token_coma: continue;
      token_semicolon: break;
    else
      ERROR_SEMICOLON_EXPECTED;
    end;
  end;
  Result := parser_NextToken(Scope);
end;

function TNPUnit.ParseUsingStatement(Scope: TScope; SContext: PSContext): TTokenID;
var
  Expr: TIDExpression;
  EContext: TEContext;
  UScope: TScope;
  Alias: TIDVariable;
  ID: TIdentifier;
  ToElseJump: TILJampedInstruction;
  ToElseJumps: array of TILJampedInstruction;
  i, Cnt: Integer;
begin
  Cnt := 0;
  UScope := TScope.Create(stLocal, Scope);
  CFBBegin(SContext, CFB_IF);
  while true do begin
    InitEContext(EContext, SContext, ExprRValue);
    Result := ParseExpression(UScope, EContext, parser_NextToken(UScope));
    Expr := RPNPopExpression(EContext);
    {проверка выражения на корректность}
    CheckEmptyExpression(Expr);

    if Expr.IsNullableVariable then
    begin
      {гегерируем условие проверки}
      ILWrite(SContext, TIL.IL_Cmp(Expr, SYSUnit._NullPtrExpression));
      ToElseJump := TIL.IL_JmpNext(Expr.Line, cZero, nil);
      SetLength(ToElseJumps, Cnt + 1);
      ToElseJumps[Cnt] := ToElseJump;
      Inc(Cnt);
      ILWrite(SContext, ToElseJump);

      {если это алиас}
      if Result = token_identifier then
      begin
        parser_ReadCurrIdentifier(ID);
        Alias := TIDVariable.Create(UScope, ID, Expr.DataType, [VarNotNull]);
        Result := parser_NextToken(UScope);
      end else begin
        {cоздаем алиас из выражения}
        if Expr.ItemType = itVar then
        begin
          ID.Name := Expr.Declaration.Name;
          ID.TextPosition := Expr.TextPosition;
          Alias := TIDVariable.Create(UScope, ID, Expr.DataType, [VarNotNull]);
          Alias.Absolute := Expr.AsVariable;
        end else
          continue;
      end;
      // добавляем в scope
      UScope.AddVariable(Alias);
    end;

    // если запятая - идем на следующий круг
    if Result = token_coma then
      continue;

    parser_MatchToken(Result, token_do);
    Break;
  end;
  {парсим тело}
  CFBBegin(SContext, CFB_IF_THEN);
  parser_NextToken(UScope);
  Result := ParseStatements(UScope, SContext, False);
  CFBEnd(SContext, CFB_IF_THEN);
  for i := 0 to Cnt - 1 do
    ToElseJumps[i].Destination := SContext.ILLast;
  CFBEnd(SContext, CFB_IF);
end;

function TNPUnit.Process_operator_Addr(var EContext: TEContext): TIDExpression;
var
  Expr: TIDExpression;
  TmpDecl: TIDVariable;
  DataType: TIDType;
  LEAInstruction: TILInstruction;
begin
  Expr := RPNPopExpression(EContext);
  if (Expr.ItemType <> itVar) and
     ((Expr.ItemType = itConst) and not (Expr.DataType.DataTypeID in [dtRecord, dtStaticArray, dtGuid])) and
     (Expr.ItemType <> itProcedure) then
    ERROR_VAR_OR_PROC_EXPRESSION_REQUIRED(Expr);

  DataType := Expr.DataType.DefaultReference;
  if DataType = nil then
  begin
    DataType := Expr.DataType.GetDefaultReference(FImplScope);
    AddType(DataType);
  end;

  if Expr.IsTMPVar and Expr.AsVariable.Reference then
  begin
    Result := GetTMPVarExpr(EContext.SContext, DataType, Expr.TextPosition);
    Result.AsVariable.Absolute := Expr.AsVariable;
  end else begin
    TmpDecl := GetTMPVar(EContext.SContext, DataType);
    TmpDecl.IncludeFlags([VarNotNull]);
    Result := TIDExpression.Create(TmpDecl, Expr.TextPosition);

    LEAInstruction := TIL.IL_LoadAddress(Result, Expr);
    Result.Instruction := LEAInstruction;

    ILWrite(EContext.SContext, LEAInstruction);
  end;
  ReleaseExpression(EContext.SContext, Expr);
end;

procedure TNPUnit.Process_operator_logical_AND_OR(var EContext: TEContext; Op: TOperatorID; Left, Right, Result: TIDExpression);
  procedure ProcessConstNode(ConstExpression, VarExpression: TIDExpression);
  var
    Node, LNode, RNode: PBoolExprNode;
    FirstInstruction: TILInstruction;
  begin
    // если константное выражение = TRUE
    if TIDBooleanConstant(ConstExpression.Declaration).Value then
    begin
      // и операция - OR, то результат операции известен = TRUE
      if Op <> opOr then Exit;
      RPNPushExpression(EContext, SYSUnit._TrueExpression);
    end else begin
      // если константное выражение = FALSE
      // и операция - AND, то результат операции известен = FALSE
      if Op <> opAnd then Exit;
      RPNPushExpression(EContext, SYSUnit._FalseExpression);
    end;
    // удаляем ненужный нод
    Node := EContext.LastBoolNode;
    if Node.NodeType = ntCmp then
    begin
      LNode := Node.LeftNode;
      RNode := Node;
    end else begin
      LNode := Node;
      while Assigned(LNode.LeftChild) do
        LNode := LNode.LeftChild;
      LNode := LNode.LeftNode;
      RNode := Node.RightNode;
    end;
    if Assigned(LNode) then
      FirstInstruction := LNode.Instruction.Next
    else
      FirstInstruction := EContext.LastInstruction.Next;
    ILDelete(EContext.SContext, FirstInstruction, RNode.Instruction);
    EContext.LastBoolNode := Node.PrevNode;
  end;
type
  TVarMode = (
    varNone,   // оба выражения не являются одиночными переменными
    varLeft,   // левое выражение является одиночной переменной
    varRight,  // правое выражение является одиночной переменной
    varBoth    // оба выражения являются одиночными переменными
  );
var
  Node, LNode, RNode: PBoolExprNode;
  Instruction, LeftLastCode: TILInstruction;
  SContext: PSContext;
begin
  SContext := EContext.SContext;
  // выясняем, являются ли одно з выраженией константой
  if Left.ItemType = itConst then begin
    ProcessConstNode(Left, Right);
    Exit;
  end;
  if Right.ItemType = itConst then begin
    ProcessConstNode(Right, Left);
    Exit;
  end;

  // выбираем из стека два последних нода
  // последний - для нас правый, предпоследний - для нас левый
  RNode := EContext.LastBoolNode;

  // если левое и правое выражения не были результатом сравнения
  // до генерируем сравнение на True
  if not Assigned(RNode) then
  begin
    // оптимизация для AND если Left и Right обычные переменные
    if (Op = opAnd) and (Left.IsNonAnonimousVariable and Right.IsNonAnonimousVariable) then
    begin
      Instruction := TIL.IL_Test(Left, Right);
      ILWrite(SContext, Instruction);
      ILWrite(SContext, TILJmpNext.Create(Instruction.Line));
      Bool_AddExprNode(EContext, Instruction.Next, cNonZero);
      Exit;
    end;
    Instruction := TIL.IL_Test(Left, Left);
    LeftLastCode := TILInstruction(Left.Instruction);
    if not Assigned(LeftLastCode) then
      LeftLastCode := SContext.ILLast;
    SContext.IL.InsertAfter(LeftLastCode, Instruction);
    SContext.IL.InsertAfter(Instruction, TILJmpNext.Create(Instruction.Line));
    Bool_AddExprNode(EContext, Instruction.Next, cNonZero);
    RNode := EContext.LastBoolNode;
  end;

  LNode := RNode.PrevNode;
  // if LNode is not assigned, that because one of left or right expressions is simple bool variable
  // and we need to generate TEST instruction for this variable:
  if not Assigned(LNode) then
  begin
    // if right expression is simple bool variable:
    if not (Right is TIDBoolResultExpression) then
    begin
    Instruction := TIL.IL_Test(Right, Right);
    ILWrite(SContext, Instruction);
    ILWrite(SContext, TILJmpNext.Create(Instruction.Line));
    Bool_AddExprNode(EContext, Instruction.Next, cNonZero);
    LNode := RNode;
    RNode := EContext.LastBoolNode;
    end else
    // if left expression is simple bool variable:
    if not (Left is TIDBoolResultExpression) then
    begin
      LeftLastCode := TILInstruction(Left.Instruction);
      if not Assigned(LeftLastCode) then
        LeftLastCode := RNode.Instruction.Prev.Prev;

      Instruction := TIL.IL_Test(Left, Left);
      SContext.IL.InsertAfter(LeftLastCode, Instruction);
      SContext.IL.InsertAfter(Instruction, TILJmpNext.Create(Instruction.Line));
      Bool_AddExprNode(EContext, Instruction.Next, cNonZero);
      LNode := EContext.LastBoolNode;
    end;
  end;

  // создаем новый нод для AND/OR операции
  Node := PBoolExprNode(FBENodesPool.Add);
  Node.Parent := nil;
  Node.NodeType := TBoolExprNode.TNodeType(Ord(Op) - Ord(opAnd) + 1);
  Node.Instruction := SContext.ILLast;
  Node.LeftChild := LNode;
  Node.RightChild := RNode;
  EContext.LastBoolNode := Node;

  // теперь последний по стеку нод станет для нас предыдущим
  Node.PrevNode := LNode.PrevNode;

  // настраиваем левый и правый подноды
  LNode.Parent := Node;
  LNode.Orientation := NodeLeft;
  RNode.Parent := Node;
  RNode.Orientation := NodeRight;

  // находим самый левый (по исходному коду) нод
  while Assigned(LNode.RightChild) do
    LNode := LNode.RightChild;
  Node.LeftNode := LNode;
  LNode.RightNode := Node;

  // находим самый правый (по исходному коду) нод
  while Assigned(RNode.LeftChild) do
    RNode := RNode.LeftChild;
  Node.RightNode := RNode;
  RNode.LeftNode := Node;
end;

function TNPUnit.ProcessBuiltin_Assigned(SContext: PSContext; var EContext: TEContext): TIDExpression;
var
  Expr: TIDExpression;
  Instruction: TILInstruction;
begin
  // читаем аргумент
  Expr := RPNPopExpression(EContext);
  CheckReferenceType(Expr);

  Result := GetBoolResultExpr(SContext);

  Instruction := TIL.IL_Cmp(Expr, SYSUnit._NullPtrExpression);
  ILWrite(SContext, Instruction);
  Instruction := TILJmpNext.Create(Expr.Line);
  ILWrite(EContext.SContext, Instruction);

  Bool_AddExprNode(EContext, Instruction, cNotEqual);
  ReleaseExpression(SContext, Expr);
end;

function TNPUnit.ProcessBuiltin_Copy(var EContext: TEContext): TIDExpression;
var
  Arr, From, Cnt, Len, NCnt: TIDExpression;
  TmpVar: TIDVariable;
  Code: TILInstruction;
begin
  // читаем третий аргумент
  Cnt := RPNPopExpression(EContext);
  // читаем второй аргумент
  From := RPNPopExpression(EContext);
  // читаем первый аргумент
  Arr := RPNPopExpression(EContext);

  CheckEmptyExpression(Arr);
  CheckEmptyExpression(From);
  CheckEmptyExpression(Cnt);

  CheckArrayExpression(Arr);

  TmpVar := GetTMPVar(EContext, Arr.DataType);
  TmpVar.IncludeFlags([VarTmpResOwner]);
  Result := TIDExpression.Create(TmpVar, parser_PrevPosition);

  // если Cnt = -1 (значение по умолчанию)
  if Cnt.IsConstant and (Cnt.AsIntConst.Value = -1) then
  begin
    if Arr.IsConstant then
    begin
      case Arr.DataTypeID of
        dtString, dtAnsiString: Cnt := IntConstExpression(Arr.AsStrConst.StrLength);
        dtDynArray: Cnt := IntConstExpression(Arr.AsDynArrayConst.ArrayLength);
      else
        ERROR_FEATURE_NOT_SUPPORTED;
      end;
    end else begin
      Cnt := GetTMPVarExpr(EContext, SYSUnit._Int32, parser_PrevPosition);
      Code := TIL.IL_Length(Cnt, Arr);
      ILWrite(EContext, Code);
    end;
  end;

  if not Arr.IsConstant then
  begin
    Len := GetTMPVarExpr(EContext, SYSUnit._Int32, parser_PrevPosition);
    ILWrite(EContext, TIL.IL_Length(Len, Arr))
  end else
    Len := IntConstExpression(Arr.AsStrConst.StrLength);

  // проверка From на значение < 0
  if not From.IsConstant then
  begin
    ILWrite(EContext, TIL.IL_Cmp(From, SYSUnit._ZeroExpression));
    ILWrite(EContext, TIL.IL_Move(cLess, From, SYSUnit._ZeroExpression));

    ILWrite(EContext, TIL.IL_Cmp(From, Len));
    ILWrite(EContext, TIL.IL_Move(cGreater, From, Len));
  end;
  // проверка Cnt на значение < 0
  if not Cnt.IsConstant then
  begin
    ILWrite(EContext, TIL.IL_Cmp(Cnt, SYSUnit._ZeroExpression));
    ILWrite(EContext, TIL.IL_Move(cLess, Cnt, SYSUnit._ZeroExpression));
  end;

  if (not From.IsConstant) or (From.AsIntConst.Value > 0) then
  begin
    NCnt := GetTMPVarExpr(EContext, SYSUnit._Int32, parser_PrevPosition);
    ILWrite(EContext, TIL.IL_Sub(NCnt, Len, From));
    ILWrite(EContext, TIL.IL_Cmp(NCnt, Cnt));
    ILWrite(EContext, TIL.IL_Move(cGreater, NCnt, Cnt)); {}
  end else
    NCnt := Cnt;

  Code := TIL.IL_DAlloc(Result, NCnt);
  ILWrite(EContext, Code);

  Code := TIL.IL_Copy(Result, Arr, From, NCnt);
  ILWrite(EContext, Code);

  {освобожадем временные переменные}
  ReleaseExpression(EContext, Arr);
  ReleaseExpression(EContext, From);
  ReleaseExpression(EContext, Cnt);
end;

function TNPUnit.ProcessBuiltin_MemSet(var EContext: TEContext): TIDExpression;
var
  Value, BytePattern: TIDExpression;
begin
  // читаем второй аргумент
  BytePattern := RPNPopExpression(EContext);
  CheckEmptyExpression(BytePattern);

  // читаем первый аргумент
  Value := RPNPopExpression(EContext);
  CheckEmptyExpression(Value);

  ILWrite(EContext, TIL.IL_MemSet(Value, BytePattern));
  ReleaseExpression(BytePattern);
  ReleaseExpression(Value);
  Result := nil;
end;

function TNPUnit.ProcessBuiltin_Move(var EContext: TEContext): TIDExpression;
var
  SrcArr, SrcIdx, DstArr, DstIdx, Cnt: TIDExpression;
  Code: TILInstruction;
begin
  Result := nil;
  // читаем пятый аргумент
  Cnt := RPNPopExpression(EContext);
  // читаем четвертый аргумент
  DstIdx := RPNPopExpression(EContext);
  // читаем третий аргумент
  DstArr := RPNPopExpression(EContext);
  // читаем второй аргумент
  SrcIdx := RPNPopExpression(EContext);
  // читаем первый аргумент
  SrcArr := RPNPopExpression(EContext);

  CheckArrayExpression(SrcArr);
  CheckArrayExpression(DstArr);

  Code := TIL.IL_MoveArray(SrcArr, SrcIdx, DstArr, DstIdx, Cnt);
  ILWrite(EContext, Code);

  CheckEmptyExpression(SrcArr);
  CheckEmptyExpression(SrcIdx);
  CheckEmptyExpression(DstArr);
  CheckEmptyExpression(DstIdx);
  CheckEmptyExpression(Cnt);
end;

function TNPUnit.ProcessBuiltin_IncDec(var EContext: TEContext; MacroID: TBuiltInFunctionID): TIDExpression;
var
  Increment, Value, NewIncrement: TIDExpression;
  Instruction: TILInstruction;
  DataType, RefType: TIDType;
begin
  // читаем второй аргумент (значение инкремента/декримента)
  Increment := RPNPopExpression(EContext);

  // читаем первый аргумент (переменная)
  Value := RPNPopExpression(EContext);

  CheckVarExpression(Value, TVarModifyPlace.vmpPassArgument);

  DataType := Value.DataType.ActualDataType;

  // адресная арифметика
  if DataType.DataTypeID = dtPointer then
  begin
    if Assigned(TIDPointer(DataType).ReferenceType) then
      RefType := TIDPointer(DataType).ReferenceType.ActualDataType
    else
      RefType := SYSUnit._UInt8;

    if RefType.DataSize > 1 then
    begin
      if Increment.IsConstant then
        Increment := IntConstExpression(Increment.AsIntConst.Value * RefType.DataSize)
      else begin
        NewIncrement := GetTMPVarExpr(EContext, SYSUnit._Int32);
        Instruction := TIL.IL_Mul(NewIncrement, Increment, IntConstExpression(RefType.DataSize));
        ILWrite(EContext, Instruction);
        Increment := NewIncrement;
      end;
    end;
  end;

  if DataType.Ordinal or (DataType.BinarOperator(opAdd, DataType) <> nil) then
  begin
    case MacroID of
      bf_inc: Instruction := TIL.IL_Add(Value, Value, Increment);
      bf_dec: Instruction := TIL.IL_Sub(Value, Value, Increment);
    else
      ERROR_INTERNAL;
      Exit(nil);
    end;
    ILWrite(EContext, Instruction);
  end else
    ERROR_ORDINAL_TYPE_REQUIRED(FParser.Position);

  ReleaseExpression(EContext, Value);
  ReleaseExpression(EContext, Increment);

  Result := nil;
end;

function TNPUnit.ProcessBuiltin_Length(var EContext: TEContext): TIDExpression;
var
  Expr: TIDExpression;
  DataType: TIDType;
  Decl: TIDDeclaration;
  ParamName: string;
begin
  // читаем аргумент
  Expr := RPNPopExpression(EContext);
  DataType := Expr.DataType;
  case DataType.DataTypeID of
    // статические массивы
    dtStaticArray, dtSet: Result := IntConstExpression(TIDArray(DataType).Dimensions[0].ElementsCount);
    // открытый массив
    dtOpenArray: begin
      Decl := Expr.Declaration;
      if Decl.ItemType = itConst then
        Result := IntConstExpression(TIDDynArrayConstant(Decl).ArrayLength)
      else begin
        ParamName := Decl.Name + '$Length';
        Decl := Expr.Declaration.Scope.FindID(ParamName);
        if not Assigned(Decl) then
          AbortWorkInternal('%s param is not forund', [ParamName], parser_Position);
        Result := TIDExpression.Create(Decl, Expr.TextPosition);
      end;
    end;
    // динамические массивы
    dtDynArray, dtString, dtAnsiString: begin
      if Expr.Declaration is TIDDynArrayConstant then
        Result := IntConstExpression(Expr.AsDynArrayConst.ArrayLength)
      else
      if Expr.IsConstant then
        Result := IntConstExpression(Expr.AsStrConst.StrLength)
      else begin
        Result := GetTMPVarExpr(EContext, SYSUnit._UInt32, Expr.TextPosition);
        ILWrite(EContext, TIL.IL_Length(Result, Expr));
      end;
    end;
    else begin
      AbortWork(sArrayOrStringTypeRequired, Expr.TextPosition);
      Result := nil;
    end;
  end;
  ReleaseExpression(EContext, Expr);
end;

function TNPUnit.ProcessBuiltin_SetLength(var EContext: TEContext): TIDExpression;
var
  ArrExpr, LenExpr: TIDExpression;
  DataType: TIDType;
  Instruction: TILInstruction;
  //NewArr: TIDExpression;
begin
  // читаем второй аргумент (NewLength)
  LenExpr := RPNPopExpression(EContext);
  // читаем первый аргумент (массив/строка)
  ArrExpr := RPNPopExpression(EContext);
  DataType := ArrExpr.DataType;
  if DataType.DataTypeID in [dtDynArray, dtString, dtAnsiString] then begin
    CheckVarExpression(ArrExpr, vmpPassArgument);
    CheckEmptyExpression(LenExpr);
    {Dest := TIDVariable(ArrExpr.Declaration);
    // если это первая инициализация массива
    // то используем только выделение памяти, без реаллокации
    if (Dest.ReadCount = 0) and (Dest.WriteCount = 0) then
      Instruction := TIL.IL_DAlloc(Result, LenExpr)
    else}

    //NewArr := GetTMPVarExpr(SContext, DataType);
    Instruction := TIL.IL_RAlloc(ArrExpr, LenExpr);

    ILWrite(EContext, Instruction);
    //CheckAndCallArrayInit(SContext.Proc, ArrExpr.AsVariable);
  end else
    AbortWork(sArrayOrStringTypeRequired, ArrExpr.TextPosition);

  ReleaseExpression(EContext, LenExpr);
  Result := nil;
end;

function TNPUnit.ProcessBuiltin_Assert(var EContext: TEContext; const ParamsTest: string; SourceRow: Integer): TIDExpression;
var
  AssertExpr, ETextExpr: TIDExpression;
  Instr: TILInstruction;
  Decl: TIDDeclaration;
  AssertText: string;
  LastInst: TILInstruction;
  Condition: TILCondition;
  Args: TIDExpressions;
begin
  // читаем второй аргумент (текст ошибки)
  ETextExpr := RPNPopExpression(EContext);
  // читаем первый аргумент (утверждение)
  AssertExpr := RPNPopExpression(EContext);
  // выражение длоджно быть булевым
  CheckBooleanExpression(AssertExpr);

  if ETextExpr.IsConstant and (ETextExpr.DataTypeID = dtString) and (ETextExpr.AsStrConst.Value = '') then
    AssertText := format('Assertion at line %d: condition "%s" is false', [SourceRow, ParamsTest])
  else
    AssertText := format('Assertion at line %d: %s', [SourceRow, ETextExpr.AsStrConst.Value]);

  Decl := TIDStringConstant.Create(nil, Identifier(''), SYSUnit._String, AssertText);
  Decl.Index := FPackage.GetStringConstant(AssertText);
  ETextExpr := TIDExpression.Create(Decl, AssertExpr.TextPosition);

  LastInst := EContext.SContext.ILLast;
  if AssertExpr.IsConstant then
  begin
    {если выражение - константа}
    if AssertExpr.AsBoolConst.Value = True then
      Exit(nil);
    Condition := cNone;
  end else
  if (AssertExpr.IsAnonymous) and Assigned(LastInst) and (LastInst.ILCode = icSetBool) then
  begin
    {если выражение - результат сравнения}
    Condition := InverseCondition(LastInst.Condition);
    // удаляем инструкцию SetBool
    EContext.SContext.IL.Delete(LastInst);
  end else begin
    {если выражение - результат переменная}
    Instr := TIL.IL_Test(AssertExpr, AssertExpr);
    ILWrite(EContext, Instr);
    Condition := cZero;
  end;
  Args := [ETextExpr];
  Instr := TIL.IL_ProcCall(TIDCallExpression.Create(SYSUnit._AssertProc), nil, nil, Args);
  Instr.Condition := Condition;
  ILWrite(EContext, Instr);

  ReleaseExpression(EContext, AssertExpr);
  ReleaseExpression(EContext, ETextExpr);

  Result := nil;
end;

function TNPUnit.ProcessBuiltin_SizeOf(var EContext: TEContext): TIDExpression;
var
  Expr: TIDExpression;
  Decl: TIDDeclaration;
  DataType: TIDType;
begin
  // читаем аргумент
  Expr := RPNPopExpression(EContext);
  Decl := Expr.Declaration;
  if Decl.ItemType <> itType then
    DataType := Decl.DataType.ActualDataType
  else
    DataType := TIDType(Decl).ActualDataType;
  // if the type size is defined (the target <> ANY) - generate the int constant
  if DataType.DataSize > 0 then
    Result := IntConstExpression(DataType.DataSize)
  else begin
  // else when data size is not defined yet
    Decl := TIDSizeofConstant.CreateAnonymous(FImplScope, SYSUnit._Int32, DataType);
    Result := TIDExpression.Create(Decl, Expr.TextPosition);
  end;
  ReleaseExpression(EContext, Expr);
end;

function TNPUnit.ProcessBuiltin_New(var EContext: TEContext): TIDExpression;
var
  Expr, PExpr: TIDExpression;
  Code: TILInstruction;
  DataType: TIDType;
begin
  // читаем аргумент
  Expr := RPNPopExpression(EContext);
  CheckPointerType(Expr);
  CheckVarExpression(Expr, TVarModifyPlace.vmpPassArgument);
  Code := TIL.IL_New(Expr);
  ILWrite(EContext, Code);
  DataType := (Expr.DataType as TIDPointer).ReferenceType;
  // проверка на наличие инициализатора
  if Assigned(DataType.InitProc) then
  begin
    PExpr := TIDCallExpression.Create(DataType.InitProc, Expr.TextPosition);
    if DataType.DataTypeID = dtRecord then
      Code := TIL.IL_ProcCall(PExpr, nil, Expr, [])
    else
      Code := TIL.IL_ProcCall(PExpr, nil, nil, [Expr]);
    ILWrite(EContext, Code);
  end;
  // проверка на наличие статического конструктора у структуры
  if (DataType.DataTypeID = dtRecord) and Assigned(TIDRecord(DataType).StaticConstructor) then
  begin
    PExpr := TIDExpression.Create(TIDRecord(DataType).StaticConstructor, Expr.TextPosition);
    Code := TIL.IL_ProcCall(PExpr, nil, Expr, []);
    ILWrite(EContext, Code);
  end;
  Result := nil;
end;

function TNPUnit.ProcessBuiltin_Free(var EContext: TEContext): TIDExpression;
var
  Expr, PExpr: TIDExpression;
  Code: TILInstruction;
  DataType: TIDType;
begin
  // читаем аргумент
  Expr := RPNPopExpression(EContext);
  CheckPointerType(Expr);

  // проверка на наличие статического конструктора у структуры
  DataType := (Expr.DataType as TIDPointer).ReferenceType;
  if (DataType.DataTypeID = dtRecord) and Assigned(TIDRecord(DataType).StaticDestructor) then
  begin
    PExpr := TIDExpression.Create(TIDRecord(DataType).StaticDestructor, Expr.TextPosition);
    Code := TIL.IL_ProcCall(PExpr, nil, Expr, []);
    ILWrite(EContext, Code);
  end;
  // проверка на наличие финализатора
  if Assigned(DataType.FinalProc) then
  begin
    PExpr := TIDCallExpression.Create(DataType.FinalProc, Expr.TextPosition);
    if DataType.DataTypeID = dtRecord then
      Code := TIL.IL_ProcCall(PExpr, nil, Expr, [])
    else
      Code := TIL.IL_ProcCall(PExpr, nil, nil, [Expr]);
    ILWrite(EContext, Code);
  end;

  Code := TIL.IL_FreeInstance(Expr);
  ILWrite(EContext, Code);

  Result := nil;
end;

function TNPUnit.ProcessBuiltin_GetBit(var EContext: TEContext): TIDExpression;
var
  Value, BitIndex: TIDExpression;
begin
  // читаем аргумент 2
  BitIndex := RPNPopExpression(EContext);
  CheckIntExpression(BitIndex);
  // читаем аргумент 1
  Value := RPNPopExpression(EContext);
  // результат функции
  Result := GetTMPVarExpr(EContext, SYSUnit._Boolean);

  // проверка константного индекса на вызод за диаппазон
  if BitIndex.IsConstant then
    CheckIntConstInRange(BitIndex, Value.DataType.DataSize*8 - 1, 0);

  ILWrite(EContext, TIL.IL_GetBit(Result, Value, BitIndex));

  ReleaseExpression(Value);
  ReleaseExpression(BitIndex);
end;

function TNPUnit.ProcessBuiltin_SetBit(var EContext: TEContext): TIDExpression;
var
  Value, BitIndex, BitValue: TIDExpression;
begin
  // читаем аргумент 3
  BitValue := RPNPopExpression(EContext);
  CheckBooleanExpression(BitValue);
  // читаем аргумент 2
  BitIndex := RPNPopExpression(EContext);
  CheckIntExpression(BitIndex);
  // читаем аргумент 1
  Value := RPNPopExpression(EContext);

  // проверка константного индекса на вызод за диаппазон
  if BitIndex.IsConstant then
    CheckIntConstInRange(BitIndex, Value.DataType.DataSize*8 - 1, 0);

  ILWrite(EContext, TIL.IL_SetBit(Value, BitIndex, BitValue));

  Result := nil;
  ReleaseExpression(Value);
  ReleaseExpression(BitIndex);
  ReleaseExpression(BitValue);
end;

function TNPUnit.ProcessBuiltin_GetRef(var EContext: TEContext): TIDExpression;
var
  Src, Dst: TIDExpression;
begin
  // читаем аргумент StrongPtr
  Dst := RPNPopExpression(EContext);
  // читаем аргумент WeakPtr
  Src := RPNPopExpression(EContext);
  ILWrite(EContext, TIL.IL_StrongRef(Dst, Src));
  ILWrite(EContext, TIL.IL_Cmp(Dst, SYSUnit._NullPtrExpression));
  Result := GetTMPVarExpr(EContext, SYSUnit._Boolean);
  ILWrite(EContext, TIL.IL_SetBool(cNotEqual, Result));
end;

function TNPUnit.ProcessBuiltin_Include(var EContext: TEContext): TIDExpression;
var
  ASet, ASubSet, SValue: TIDExpression;
  SetDT: TIDSet;
begin
  // читаем аргумент SubSet
  ASubSet := RPNPopExpression(EContext);
  // читаем аргумент Set
  ASet := RPNPopExpression(EContext);
  CheckSetType(ASet);
  SetDT := TIDSet(ASet.DataType.ActualDataType);
  case ASubSet.DataTypeID of
    dtSet: begin
      if ASubSet.DataType.ActualDataType <> SetDT then
        ERROR_INCOMPATIBLE_TYPES(ASubSet, SetDT.BaseType);
      ILWrite(EContext, TIL.IL_Or(ASet, ASet, ASubSet));
    end;
    dtEnum: begin
      if ASubSet.DataType.ActualDataType <> SetDT.BaseType.ActualDataType then
        ERROR_INCOMPATIBLE_TYPES(ASubSet, SetDT.BaseType);
      if ASubSet.IsConstant then
      begin
        SValue := IntConstExpression(1 shl ASubSet.AsIntConst.Value);
      end else
        SValue := nil;
      ILWrite(EContext, TIL.IL_Or(ASet, ASet, SValue));
    end;
    dtDynArray: begin
      SValue := ConstDynArrayToSet(ASubSet, SetDT);
      ILWrite(EContext, TIL.IL_Or(ASet, ASet, SValue));
    end;
  else
    ERROR_ORDINAL_OR_SET_REQUIRED(ASubSet);
  end;
  Result := nil;
end;

function TNPUnit.ProcessBuiltin_Exclude(var EContext: TEContext): TIDExpression;
var
  ASet, ASubSet, SValue: TIDExpression;
  SetDT: TIDSet;
begin
  // читаем аргумент SubSet
  ASubSet := RPNPopExpression(EContext);
  // читаем аргумент Set
  ASet := RPNPopExpression(EContext);
  CheckSetType(ASet);
  SetDT := TIDSet(ASet.DataType.ActualDataType);
  case ASubSet.DataTypeID of
    dtSet: begin
      if ASubSet.DataType.ActualDataType <> SetDT then
        ERROR_INCOMPATIBLE_TYPES(ASubSet, SetDT.BaseType);
      SValue := GetTMPVarExpr(EContext, SetDT);
      ILWrite(EContext, TIL.IL_Not(SValue, ASubSet));
      ILWrite(EContext, TIL.IL_And(ASet, ASet, SValue));
    end;
    dtEnum: begin
      if ASubSet.DataType.ActualDataType <> SetDT.BaseType.ActualDataType then
        ERROR_INCOMPATIBLE_TYPES(ASubSet, SetDT.BaseType);
      if ASubSet.IsConstant then
      begin
        SValue := IntConstExpression(not (1 shl ASubSet.AsIntConst.Value));
      end else
        SValue := nil;  // todo
      ILWrite(EContext, TIL.IL_And(ASet, ASet, SValue));
    end;
    dtDynArray: begin
      SValue := ConstDynArrayToSet(ASubSet, SetDT);
      SValue.AsIntConst.Value := not SValue.AsIntConst.Value;
      ILWrite(EContext, TIL.IL_And(ASet, ASet, SValue));
    end;
  else
    ERROR_ORDINAL_OR_SET_REQUIRED(ASubSet);
  end;
  Result := nil;
end;

function TNPUnit.ProcessBuiltin_TypeInfo(var EContext: TEContext): TIDExpression;
var
  Expr: TIDExpression;
begin
  // читаем аргумент
  Expr := RPNPopExpression(EContext);
  CheckEmptyExpression(Expr);
  Result := GetTMPVarExpr(EContext, SYSUnit._TObject);
  ILWrite(EContext, TIL.IL_TypeInfo(Result, Expr));
end;

function TNPUnit.ProcessBuiltin_Ord(var EContext: TEContext): TIDExpression;
var
  Expr: TIDExpression;
  CValue: Int64;
begin
  // читаем аргумент
  Expr := RPNPopExpression(EContext);
  CheckEmptyExpression(Expr);
  CheckOrdinalExpression(Expr);
  if Expr.IsConstant then begin
    CValue := Expr.AsConst.AsInt64;
    Result := TIDExpression.Create(TIDIntConstant.CreateAnonymous(nil, SYSUnit._Int32, CValue), Expr.TextPosition)
  end else
    Result := TIDCastExpression.Create(Expr.Declaration, SYSUnit._Int32, parser_PrevPosition);

  ReleaseExpression(EContext, Expr);
end;

function TNPUnit.ProcessBuiltin_RefCount(var EContext: TEContext): TIDExpression;
var
  Expr: TIDExpression;
begin
  // читаем аргумент
  Expr := RPNPopExpression(EContext);
  if not (Expr.DataTypeID in [dtString, dtAnsiString, dtDynArray, dtClass, dtInterface]) then
    ERROR_REFERENCE_TYPE_EXPECTED(Expr);

  Result := GetTMPVarExpr(EContext, SYSUnit._Int32);
  Result.TextPosition := parser_Position;

  ILWrite(EContext, TIL.IL_RefCount(Result, Expr));
end;

function TNPUnit.ProcessBuiltin_LoHiBound(var EContext: TEContext; HiBound: Boolean): TIDExpression;
var
  Expr: TIDExpression;
  Decl: TIDDeclaration;
  DataType: TIDType;
begin
  // читаем аргумент
  Expr := RPNPopExpression(EContext);
  CheckEmptyExpression(Expr);

  Decl := Expr.Declaration;
  case Decl.ItemType of
    itType: DataType := Decl as TIDType;
    itVar, itConst: DataType := Decl.DataType;
  else
    ERROR_ORDINAL_TYPE_REQUIRED(Expr.TextPosition);
    Exit(nil);
  end;

  DataType := DataType.ActualDataType;

  if DataType.Ordinal then
  begin
    case HiBound of
      False: Decl := TIDIntConstant.CreateAnonymous(nil, DataType, (DataType as TIDOrdinal).LowBound);
      True: Decl := TIDIntConstant.CreateAnonymous(nil, DataType, (DataType as TIDOrdinal).HighBound);
    end;
  end else
  if DataType.DataTypeID = dtStaticArray then
  begin
    DataType := (DataType as TIDArray).Dimensions[0];
    case HiBound of
      false: Decl := TIDIntConstant.CreateAnonymous(nil, SYSUnit._Int32, (DataType as TIDOrdinal).LowBound);
      True: Decl := TIDIntConstant.CreateAnonymous(nil, SYSUnit._Int32, (DataType as TIDOrdinal).HighBound);
    end
  end else
  if DataType.DataTypeID in [dtDynArray, dtString, dtAnsiString] then
  begin
    case HiBound of
      False: Exit(SYSUnit._ZeroExpression);
      True: begin
        Result := GetTMPVarExpr(EContext, SYSUnit._Int32);
        Result.TextPosition := parser_Position;
        ILWrite(EContext, TIL.IL_Length(Result, Expr));
        ILWrite(EContext, TIL.IL_Sub(Result, Result, SYSUnit._OneExpression));
        Exit;
      end;
    end;
  end else
    ERROR_ORDINAL_TYPE_REQUIRED(Expr.TextPosition);

  Result := TIDExpression.Create(Decl, parser_Position);
end;

function TNPUnit.ProcessBuiltin_TypeName(var EContext: TEContext): TIDExpression;
var
  Expr: TIDExpression;
  Decl: TIDDeclaration;
begin
  // читаем аргумент
  Expr := RPNPopExpression(EContext);
  Decl := Expr.Declaration;

  case Decl.ItemType of
    // статические массивы
    itVar, itConst: Result := StrConstExpression(Decl.DataType.ActualDataType.DisplayName);
    itType: Result := StrConstExpression(TIDType(Decl).ActualDataType.DisplayName);
    else begin
       AbortWork(sVariableOrTypeRequired, Expr.TextPosition);
       Result := nil;
    end;
  end;
  FPackage.GetStringConstant(TIDStringConstant(Result.Declaration));
end;

function ArgListIsConst(const List: TIDExpressions): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Length(List) - 1 do
    if List[i].ItemType <> itConst then
      Exit(False);
end;

function TNPUnit.Process_CALL(var EContext: TEContext): TIDExpression;
  function GenConstToVar(SContext: PSContext; Constant: TIDExpression; VarDatatType: TIDType): TIDExpression;
  var
    Instruction: TILInstruction;
  begin
    Result := GetTMPVarExpr(SContext, VarDatatType);
    Instruction := TIL.IL_Move(Result, Constant);
    ILWrite(SContext, Instruction);
  end;
var
  PIndex, AIndex, ArgsCount,
  ParamsCount: Integer;
  PExpr: TIDCallExpression;
  UserArguments,
  CallArguments: TIDExpressions;
  ProcParams: TVariableList;
  Decl: TIDDeclaration;
  ResVar: TIDVariable;
  ProcDecl: TIDProcedure;
  ProcResult: TIDType;
  Param: TIDVariable;
  AExpr, NewExpr: TIDExpression;
  CallCode: TILProcCall;
  SContext: PSContext;
begin
  SContext := EContext.SContext;
  // читаем декларацию функции
  PExpr := TIDCallExpression(RPNPopExpression(EContext));

  {вычитка явно указанных аргументов функции}
  ArgsCount := PExpr.ArgumentsCount;
  SetLength(UserArguments, ArgsCount);
  for AIndex := ArgsCount - 1 downto 0 do
    UserArguments[AIndex] := RPNPopExpression(EContext);

  ProcDecl := nil;
  Decl := PExpr.Declaration;


  {прямой вызов}
  if Decl.ItemType = itProcedure then
  begin
    ProcDecl := TIDProcedure(Decl);
    {if Assigned(ProcDecl.Struct) and (PExpr.Instance.Declaration is TIDField) then
    begin

    end;}
    if Assigned(ProcDecl.GenericDescriptor) and not Assigned(SContext.Proc.GenericDescriptor) then
    begin
      ProcDecl := SpecializeGenericProc(PExpr, UserArguments);
      ProcParams := ProcDecl.ExplicitParams;
      PExpr.Declaration := ProcDecl;
    end;

    {поиск подходящей декларации}
    if Assigned(ProcDecl.NextOverload) then begin
      ProcDecl := MatchOverloadProc(PExpr, UserArguments, ArgsCount);
      ProcParams := ProcDecl.ExplicitParams;
      PExpr.Declaration := ProcDecl;
    end else begin
      ProcParams := ProcDecl.ExplicitParams;
      MatchProc(PExpr, ProcParams, UserArguments);
    end;

    ProcResult := ProcDecl.ResultType;

  end else
  {вызов через переменную процедурного типа}
  if PExpr.DataTypeID = dtProcType then begin

    Decl := PExpr.DataType;

    ProcParams := TIDProcType(Decl).Params;
    ProcResult := TIDProcType(Decl).ResultType;
    MatchProc(PExpr, ProcParams, UserArguments);
  end else begin
    AbortWorkInternal(msgProcOrProcVarRequired, parser_Position);
    ProcResult := nil;
  end;

  ParamsCount := Length(ProcParams);
  {если все аргументы явно указаны}
  if ParamsCount = ArgsCount then
  begin
    for AIndex := 0 to ParamsCount - 1 do
    begin
      Param := ProcParams[AIndex];
      AExpr := UserArguments[AIndex];

      {если аргумент константый дин. массив то делаем доп. проверку}
      if AExpr.Declaration is TIDDynArrayConstant then
        AExpr := CheckConstDynArray(SContext, Param.DataType, AExpr);

      {проверка диаппазона для константных аргументов}
      if AExpr.IsConstant then
        CheckConstValueOverflow(AExpr, Param.DataType);

      {подбираем implicit оператор}
      AExpr := MatchImplicit3(SContext, AExpr, Param.DataType);
      CheckNotNullExpression(Param, AExpr);

      {если параметр - constref и аргумент - константа, то создаем временную переменную}
      if (VarConstRef in Param.Flags) and (AExpr.ItemType = itConst) then
        AExpr := GenConstToVar(SContext, AExpr, Param.DataType);

      {если параметр - метод, получаем ссылку на него}
      if (AExpr.ItemType = itProcedure) and (not TIDProcType(AExpr.DataType).IsStatic) then
      begin
        NewExpr := GetTMPVarExpr(SContext, AExpr.DataType);
        NewExpr.TextPosition := AExpr.TextPosition;
        ILWrite(SContext, TIL.IL_GetPtr(NewExpr, nil, AExpr));
        AExpr := NewExpr;
      end;

      UserArguments[AIndex] := AExpr;

      if Param.VarReference then
      begin
        CheckVarExpression(AExpr, vmpPassArgument);
        {проверка на строгость соответствия типов}
        if Param.DataType.ActualDataType <> AExpr.DataType.ActualDataType then
        begin
          if not ((Param.DataType.DataTypeID = dtPointer) and
             (AExpr.DataType.DataTypeID = dtPointer) and
             (TIDPointer(Param.DataType).ReferenceType = TIDPointer(AExpr.DataType).ReferenceType)) then
          ERROR_REF_PARAM_MUST_BE_IDENTICAL(AExpr);
        end;
      end;
    end;
    CallArguments := UserArguments;
  end else
  {если некоторые аргументы опущены}
  begin
    AIndex := 0;
    PIndex := 0;
    SetLength(CallArguments, ParamsCount);
    while (PIndex < ParamsCount) do
    begin
      Param := ProcParams[PIndex];
      if AIndex < ArgsCount then begin
        AExpr := UserArguments[AIndex];
        Inc(AIndex);
      end else
        AExpr := nil;
      {подстановка аргуметов по умолчанию}
      if Assigned(AExpr) then
        CallArguments[PIndex] := AExpr
      else begin
        AExpr := Param.DefaultValue;
        if not Assigned(AExpr) then
          ERROR_NOT_ENOUGH_ACTUAL_PARAMS(PExpr);
        CallArguments[PIndex] := AExpr;
      end;

      {если аргумент константый дин. массив то делаем доп. проверку}
      if AExpr.Declaration is TIDDynArrayConstant then
        AExpr := CheckConstDynArray(SContext, Param.DataType, AExpr);     // нужно ли????

      {проверка диаппазона для константных аргументов}
      if AExpr.IsConstant then
        CheckConstValueOverflow(AExpr, Param.DataType);

      {подбираем implicit оператор}
      AExpr := MatchImplicit3(SContext, AExpr, Param.DataType);

      {если параметр - constref и аргумент - константа, то создаем временную переменную}
      if (VarConstRef in Param.Flags) and (AExpr.ItemType = itConst) then
        AExpr := GenConstToVar(SContext, AExpr, Param.DataType);

      {если параметр - метод, получаем ссылку на него}
      if (AExpr.ItemType = itProcedure) and (not TIDProcType(AExpr.DataType).IsStatic) then
      begin
        NewExpr := GetTMPVarExpr(SContext, AExpr.DataType);
        ILWrite(SContext, TIL.IL_GetPtr(NewExpr, nil, AExpr));
        AExpr := NewExpr;
      end;

      CallArguments[PIndex] := AExpr;

      {если параметр открытый массив}
      if Param.DataTypeID = dtOpenArray then
      begin
        {добовляем скрытй аргумент}
        RPNPushExpression(EContext, AExpr);
        Inc(PIndex);
        CallArguments[PIndex] := ProcessBuiltin_Length(EContext);
      end;

      Inc(PIndex);
      {если параметр передается по ссылке, проверяем что аргумент можно менять}
      if Param.VarReference then
      begin
        CheckVarExpression(AExpr, vmpPassArgument);
        {проверка на строгость соответствия типов}
        if Param.DataType.ActualDataType <> AExpr.DataType.ActualDataType then
          ERROR_REF_PARAM_MUST_BE_IDENTICAL(AExpr);
      end;
    end;
  end;

  {конструирование обьекта}
  if (Assigned(ProcDecl) and (pfConstructor in ProcDecl.Flags)) then
  begin
    Result := process_CALL_constructor(SContext, PExpr, CallArguments);
    Exit;
  end;

  {результат функции}
  if Assigned(ProcResult) then begin
    ResVar := GetTMPVar(SContext, ProcResult);
    ResVar.IncludeFlags([VarTmpResOwner]);
    Result := TIDExpression.Create(ResVar, PExpr.TextPosition);
  end else
    Result := nil;

  {если вызов был коссвенным, инлайн невозможен}
  Decl := PExpr.Declaration;
  if Decl.ItemType = itVar then
  begin
    {если переменная - поле класа, получаем ссылку на поле}
    if (Decl.Scope.ScopeType = stStruct) and (TStructScope(Decl.Scope).Struct = SContext.Proc.Struct) then
    begin
      AExpr := GetTMPRefExpr(SContext, PExpr.DataType);
      AExpr.TextPosition := PExpr.TextPosition;
      ILWrite(SContext, TIL.IL_GetPtr(AExpr, nil, PExpr));
      PExpr := TIDCallExpression.Create(AExpr.Declaration);
      PExpr.TextPosition := AExpr.TextPosition;
      PExpr.ArgumentsCount := ArgsCount;
    end;
    if PExpr is TIDCastedCallExpression then
       CallCode := TIL.IL_ProcCallUnSafe(PExpr, Result, PExpr.Instance, CallArguments)
    else
       CallCode := TIL.IL_ProcCall(PExpr, Result, nil, CallArguments);
    ILWrite(SContext, CallCode);
    Exit;
  end;

  {проверка на constexpr}
  if (pfPure in ProcDecl.Flags) and ArgListIsConst(CallArguments) then
  begin
    Result.Declaration := ProcDecl.CECalc(CallArguments);
    Exit;
  end;

  {INLINE подстановка (пока только если инлайн процедура готова)}
  if (pfInline in ProcDecl.Flags) and
     (ProcDecl.IsCompleted) and
     (ProcDecl <> SContext.Proc) then
  begin
    SContext.IL.InlineProc(SContext.Proc, ProcDecl, GetILLast(SContext), PExpr.Instance, Result, CallArguments);
  end else begin
    if (pfVirtual in ProcDecl.Flags) then // нужно тщательнее проверить
      CallCode := TIL.IL_VirtCall(PExpr, Result, PExpr.Instance, CallArguments)
    else
      CallCode := TIL.IL_ProcCall(PExpr, Result, PExpr.Instance, CallArguments);
    ILWrite(SContext, CallCode);
  end;
end;

function TNPUnit.process_CALL_constructor(SContext: PSContext; CallExpression: TIDCallExpression;
                                        const CallArguments: TIDExpressions): TIDExpression;
var
  Proc: TIDProcedure;
  ResVar: TIDVariable;
  Code: TILInstruction;
begin
  Proc := CallExpression.AsProcedure;
  ResVar := GetTMPVar(SContext, Proc.Struct);
  ResVar.IncludeFlags([VarNotNull, VarTmpResOwner]);
  Result := TIDCastExpression.Create(ResVar, CallExpression.Instance.AsType, CallExpression.TextPosition);
  Code := TIL.IL_DNew(Result, CallExpression.Instance);
  ILWrite(SContext, Code);
  if not (pfInline in Proc.Flags) or (not Proc.IsCompleted) then
  begin
    Code := TIL.IL_ProcCall(CallExpression, nil, Result, CallArguments);
    ILWrite(SContext, Code);
  end else
    SContext.IL.InlineProc(SContext.Proc, Proc, Code, Result, nil, CallArguments);
end;

function TNPUnit.Process_CALL_direct(SContext: PSContext; PExpr: TIDCallExpression; CallArguments: TIDExpressions): TIDExpression;
  function GenConstToVar(Constant: TIDExpression; VarDatatType: TIDType): TIDExpression;
  var
    Instruction: TILInstruction;
  begin
    Result := TIDExpression.Create(GetTMPVar(SContext, VarDatatType));
    Instruction := TIL.IL_Move(Result, Constant);
    ILWrite(SContext, Instruction);
  end;

var
  AIndex, ArgsCount: Integer;
  ProcDecl: TIDProcedure;
  ProcParams: TVariableList;
  ResVar: TIDVariable;
  ProcResult: TIDType;
  Param: TIDVariable;
  AExpr: TIDExpression;
  CallInstruction: TILProcCall;
begin
  ArgsCount := Length(CallArguments);

  ProcDecl := PExpr.AsProcedure;
  ProcParams := ProcDecl.ExplicitParams;

  if Assigned(ProcDecl.GenericDescriptor) then
  begin
    ProcDecl := SpecializeGenericProc(PExpr, CallArguments);
    ProcParams := ProcDecl.ExplicitParams;
    PExpr.Declaration := ProcDecl;
  end;

  {если все аргументы явно указаны}
  for AIndex := 0 to ArgsCount - 1 do
  begin
    Param := ProcParams[AIndex];
    AExpr := CallArguments[AIndex];

    {если аргумент константый дин. массив то делаем доп. проверку}
    if AExpr.Declaration is TIDDynArrayConstant then
      AExpr := CheckConstDynArray(SContext, Param.DataType, AExpr);

    {проверка диаппазона для константных аргументов}
    if AExpr.IsConstant then
      CheckConstValueOverflow(AExpr, Param.DataType);

    {подбираем и если надо вызываем implicit оператор}
    AExpr := MatchImplicit3(SContext, AExpr, Param.DataType);

    {если параметр - constref и аргумент - константа, то создаем временную переменную}
    if (VarConstRef in Param.Flags) and (AExpr.ItemType = itConst) then
      AExpr := GenConstToVar(AExpr, Param.DataType);

    CallArguments[AIndex] := AExpr;

    if Param.Reference then
      CheckVarExpression(AExpr, vmpPassArgument);
  end;

  {результат функции}
  ProcResult := ProcDecl.ResultType;
  if Assigned(ProcResult) then begin
    ResVar := GetTMPVar(SContext, ProcResult);
    ResVar.IncludeFlags([VarTmpResOwner]);
    Result := TIDExpression.Create(ResVar, PExpr.TextPosition);
  end else
    Result := nil;

  {INLINE подстановка (пока только если инлайн процедура готова)}
  if (pfInline in ProcDecl.Flags) and
     (ProcDecl.IsCompleted) and
     (ProcDecl <> SContext.Proc) then
  begin
    SContext.IL.InlineProc(SContext.Proc, ProcDecl, nil, PExpr.Instance, Result, CallArguments);
  end else begin
    if Assigned(PExpr.Instance) and (ProcDecl.Struct <> PExpr.Instance.DataType) then
      CallInstruction := TIL.IL_InheritedCall(PExpr, Result, PExpr.Instance, CallArguments)
    else
      CallInstruction := TIL.IL_ProcCall(PExpr, Result, PExpr.Instance, CallArguments);
    ILWrite(SContext, CallInstruction);
  end;
  ProcDecl.IncRefCount(1);
end;

function TNPUnit.Process_operator_In(var EContext: TEContext; const Left, Right: TIDExpression): TIDExpression;
var
  CRange: TIDRangeConstant;
  //Node: PBoolExprNode;
  Instruction: TILInstruction;
  SContext: PSContext;
  LeftExpr: TIDExpression;
begin
  SContext := EContext.SContext;
  //Node := EContext.LastBoolNode;
  if Right.Declaration is TIDRangeConstant then
  begin
    CRange := TIDRangeConstant(Right.Declaration);
    if SContext.WriteIL then
    begin

      Instruction := TIL.IL_Cmp(Left, CRange.Value.LBExpression);
      ILWrite(SContext, Instruction);
      //SContext.IL.InsertAfter(Node.Instruction, Instruction);
      //Node.Instruction := TILJmpNext.Create;
      Instruction := TILJmpNext.Create(Instruction.Line);
      ILWrite(SContext, Instruction);
      Bool_AddExprNode(EContext, Instruction, cGreaterOrEqual);
      //SContext.IL.InsertAfter(Instruction, Node.Instruction);

      Instruction := TIL.IL_Cmp(Left, CRange.Value.HBExpression);
      ILWrite(SContext, Instruction);
      Instruction := TILJmpNext.Create(Instruction.Line);
      ILWrite(SContext, Instruction);
      Bool_AddExprNode(EContext, Instruction, cLessOrEqual);
      Bool_AddExprNode(EContext, ntAnd);
    end;
  end else begin
    if Left.IsConstant then
      LeftExpr := CreateAnonymousConstant(nil, EContext, Identifier(IntToStr(1 shl Left.AsIntConst.Value)), itInteger)
    else
      LeftExpr := Left;

    ILWrite(SContext, TIL.IL_Test(LeftExpr, Right));
    ILWrite(SContext, TILJmpNext.Create(LeftExpr.Line));
    Bool_AddExprNode(EContext, SContext.ILLast, cNonZero);
  end;
  Result := TIDExpression.Create(SContext.Proc.GetTMPVar(SYSUnit._Boolean));
end;

function TNPUnit.Process_operator_Is(var EContext: TEContext): TIDExpression;
var
  Src, Dst: TIDExpression;
begin
  Dst := RPNPopExpression(EContext);
  Src := RPNPopExpression(EContext);
  CheckClassOrIntfType(Src.DataType, Src.TextPosition);
  CheckClassOrIntfType(Dst);
  Result := GetTMPVarExpr(EContext, SYSUnit._Boolean, Dst.TextPosition);
  ILWrite(EContext, TIL.IL_QueryType(Result, Src, Dst));
end;

function TNPUnit.Process_operator_As(var EContext: TEContext): TIDExpression;
var
  Src, Dst: TIDExpression;
begin
  Dst := RPNPopExpression(EContext);
  Src := RPNPopExpression(EContext);
  CheckClassOrIntfType(Src.DataType, Src.TextPosition);
  CheckClassOrIntfType(Dst);
  Result := GetTMPVarExpr(EContext, Dst.AsType, Dst.TextPosition);
  ILWrite(EContext, TIL.IL_QueryType(Result, Src, Dst));
end;

function TNPUnit.ParseExplicitCast(Scope: TScope; SContext: PSContext; var DstExpression: TIDExpression): TTokenID;
//var
//  EContext: TEContext;
//  SrcExpr: TIDExpression;
//  OperatorDecl: TIDDeclaration;
//  CallExpr: TIDCallExpression;
//  TargetType: TIDType;
begin
  Assert(False);
//  InitEContext(EContext, SContext, ExprNested);
//  Result := ParseExpression(Scope, EContext, FParser.NextToken);
//  SrcExpr := RPNPopExpression(EContext);
//
//  if Assigned(EContext.LastBoolNode) then
//    Bool_CompleteExpression(EContext.LastBoolNode, SContext.ILLast);
//  if Result <> token_closeround then
//    ERROR_INCOMPLETE_STATEMENT;
//
//  TargetType := DstExpression.AsType;
//
//  OperatorDecl := MatchExplicit(SrcExpr, TargetType);
//  if not Assigned(OperatorDecl) then
//  begin
//    if (SrcExpr.DataType.DataTypeID = dtClass) and
//       (TargetType.DataTypeID = dtClass) and
//       (TIDClass(SrcExpr.DataType).IsInheritsForm(TIDClass(TargetType))) then
//      OperatorDecl := SYSUnit._Boolean
//    else begin
//      if (SrcExpr.DataTypeID = dtChar) and (TargetType.DataTypeID = dtAnsiChar) then
//      begin
//        if SrcExpr.IsConstant then
//        begin
//          if (Ord(SrcExpr.AsCharConst.Value) <= 255) then
//            OperatorDecl := TargetType
//          else
//            ERROR_CONST_VALUE_OVERFLOW(SrcExpr, TargetType);
//        end else
//          OperatorDecl := TargetType;
//      end else
//      if (SrcExpr.DataTypeID = dtPointer) and (DstExpression.AsType.DataTypeID = dtPointer) then
//         OperatorDecl := TargetType
//      else
//      if (DstExpression.AsType.DataTypeID = dtAnsiString) and
//         (SrcExpr.DataType is TIDOrdinal) and
//         not Assigned(SContext) then
//      begin
//        // todo: make new ANsiType with SrcExpr codepage
//        Exit(FParser.NextToken);
//      end else
//        ERROR_INVALID_TYPECAST(SrcExpr, DstExpression.AsType);
//    end;
//  end;
//
//  Result := parser_NextToken(Scope);
//
//  if OperatorDecl.ItemType = itType then
//  begin
//    if (SrcExpr.ItemType = itConst) and (SrcExpr.IsAnonymous) then
//    begin
//      TIDConstant(SrcExpr.Declaration).ExplicitDataType := OperatorDecl as TIDType;
//      DstExpression := TIDExpression.Create(SrcExpr.Declaration);
//    end else
//      DstExpression := TIDCastExpression.Create(SrcExpr.Declaration, TIDType(DstExpression.Declaration), DstExpression.TextPosition);
//    DstExpression.Instruction := SrcExpr.Instruction;
//  end else begin
//    // вызываем explicit-оператор
//    CallExpr := TIDCallExpression.Create(OperatorDecl, DstExpression.TextPosition);
//    CallExpr.ArgumentsCount := 1;
//    DstExpression := Process_CALL_direct(SContext, CallExpr, TIDExpressions.Create(SrcExpr));
//  end;
end;

procedure CheckUnique(Dest: TIDExpression; IL: TIL);
{var
  Instruction: TILInstruction;
  Expr: TIDExpression;}
begin
{  Instruction := TILInstruction(Dest.Instruction);
  if Assigned(Instruction) then
  begin
    if (Instruction.ClassType = TILGetPtr) then
      Expr := TILGetPtr(Instruction).Base
    else
    if Instruction.ClassType = TILGetPtrMulti then
      Expr := TILGetPtrMulti(Instruction).Arguments[0]
    else
      Exit;

    if Assigned(Expr) and (Expr.DataType is TIDArray) and (Expr.DataTypeID = dtString) then
      IL.InsertAfter(Instruction, TIL.IL_Unique(Expr));
  end;}
end;

procedure TNPUnit.ProcessMoveSet(SContext: PSContext; Dest, Source: TIDExpression);
var
  Base, Offset: TIDExpression;
  Instruction: TILInstruction;
begin
  // общий случай
  if Dest.ExpressionType <> etExpressionList then
  begin

  end else
  // общращаются к битовому набору как к массиву
  begin
    Base := TIDMultiExpression(Dest).Items[0];
    Offset := TIDMultiExpression(Dest).Items[1];

    Instruction := TIL.IL_SetBit(Base, Offset, Source);
    ILWrite(SContext, Instruction);
  end;
end;

function TNPUnit.Process_operator_assign_complex(Src: TIDExpression; Dst: TIDMultiExpression; var EContext: TEContext): Boolean;
var
  CallExpr: TIDCallExpression;
  i, ai, ac: Integer;
  Expr: TIDExpression;
begin
  ac := Length(Dst.Items);
  for i := 0 to ac - 1 do
  begin
    Expr := Dst.Items[i];
    // если необходимо вызывать setter
    if Expr.ItemType = itProperty then
    begin
      {if ((ac - 1) = i + 1) then
        ERROR_CANNOT_ASSIGN_TEMPORARRY_OBJECT(Expr); здесть небоходим детальный анализ!!!}

      CallExpr := TIDCallExpression.Create(Expr.AsProperty.Setter);
      CallExpr.ArgumentsCount := ac - i;
      CallExpr.Instance := Dst.Items[0]; // тут надо корректно расчитать self

      for ai := i + 1 to ac - 1 do
        RPNPushExpression(EContext, Dst.Items[ai]);

      RPNPushExpression(EContext, Src);
      RPNPushExpression(EContext, CallExpr);
      Process_CALL(EContext);   // возможно следует тут вызвать Process_operator_dall_direct ?
      Exit(True);
    end;
  end;

  {если приемник это перменная типа SET}
  if (Dst.Items[0].DataTypeID = dtSet) or
     ((Dst.DataTypeID = dtSet) and
      (Src.DataTypeID <> dtSet)) then
  begin
    ProcessMoveSet(EContext.SContext, Dst, Src);
    Exit(True); // !!!! необходимо правильно отчистить стек
  end;

  {обычная цепочка - обрабатываем в вызываемой процедуре}
  Result := False;
end;

function TNPUnit.Process_operator_Deref(var EContext: TEContext): TIDExpression;
var
  Src: TIDExpression;
  RefDt: TIDType;
begin
  Src := RPNPopExpression(EContext);
  CheckPointerType(Src);
  RefDt := TIDPointer(Src.DataType).ReferenceType;
  if not Assigned(RefDt) then
    AbortWork('Cannot dereference the untyped pointer', Src.TextPosition);

  Result := TIDDrefExpression.Create(Src);
end;

procedure TNPUnit.Process_operator_AssignMulti(var EContext: TEContext);
var
  Dest, Source, NewSrc: TIDExpression;
  Instruction: TILInstruction;
  i: Integer;
begin
  Source := RPNPopExpression(EContext);
  for i := 0 to EContext.RPNExprCount - 1 do
  begin
    {вычитываем очередной приемник (читаем слево-направо)}
    Dest := RPNReadExpression(EContext, i);
    {находим Implicit оператор}
    NewSrc := MatchImplicit3(EContext.SContext, Source, Dest.DataType);
    if not Assigned(NewSrc) then
       ERROR_INCOMPATIBLE_TYPES(Source, Dest);
    Source := NewSrc;

    {если приемник сложный}
    if Dest.ExpressionType = etExpressionList then
    begin
      if Process_operator_assign_complex(Source, TIDMultiExpression(Dest), EContext) then
        continue;
    end;

    // проверяем что приемник можно модифицировать
    CheckVarExpression(Dest, vmpAssignment);

    CheckUnique(Dest, EContext.SContext.IL);

    if EContext.RPNExprCount = 1 then
    begin
      if (Dest.DataType = SYSUnit._Boolean) then
        if Bool_CompleteImmediateExpression(EContext, Dest) then
          Continue;
      Instruction := TIL.IL_Move(Dest, Source);
      ILWrite(EContext.SContext, Instruction);
    end else begin
      Instruction := TIL.IL_Move(Dest, Source);
      ILWrite(EContext.SContext, Instruction);
    end;
  end;
end;

procedure TNPUnit.Process_operator_Assign(var EContext: TEContext);
  function IsSameRef(const Dst, Src: TIDExpression): Boolean;
  begin
    if Src.ItemType = itVar then
      Result := Dst.AsVariable.Reference = Src.AsVariable.Reference
    else
      Result := False;
  end;
var
  Dest, Source, NewSrc, NewDst: TIDExpression;
  SContext: PSContext;
  Instruction: TILDestInstruction;
begin
  if EContext.RPNExprCount = 2 then
  begin
    Source := RPNPopExpression(EContext);
    Dest := RPNPopExpression(EContext);

    SContext := EContext.SContext;
    ReleaseExpression(Source);
    ReleaseExpression(Dest);

    {находим Implicit оператор}
    NewSrc := MatchImplicitOrNil(SContext, Source, Dest.DataType);
    if not Assigned(NewSrc) then
    begin
      NewSrc := CheckAndCallFuncImplicit(SContext, Source);
    if not Assigned(NewSrc) then
       ERROR_INCOMPATIBLE_TYPES(Source, Dest);
    end;
    Source := NewSrc;

    if Dest.ClassType = TIDDrefExpression then
    begin
      ILWrite(SContext, TIL.IL_WriteDRef(Dest, Source));
      Exit;
    end;

    if (Dest.DataTypeID = dtRecord) and (Source.Declaration is TIDDynArrayConstant) then
    begin
      WriteRecordFromTuple(SContext, Source, Dest);
      Exit;
    end;

    {если приемник сложный}
    if Dest.ExpressionType = etExpressionList then
    begin
      if Process_operator_assign_complex(Source, TIDMultiExpression(Dest), EContext) then
        Exit;
    end;

    // проверяем что приемник можно модифицировать
    CheckVarExpression(Dest, vmpAssignment);

    if Dest.ItemType = itVar then
      CheckNotNullExpression(Dest.AsVariable, Source);

    CheckUnique(Dest, SContext.IL);

    if (Dest.DataType = SYSUnit._Boolean) then
      if Bool_CompleteImmediateExpression(EContext, Dest) then
        Exit;

    {если справа указан метод, генерируем код получение адреса метода}
    if (Source.ItemType = itProcedure) and (not TIDProcType(Source.DataType).IsStatic) then
    begin
      NewDst := GetTMPVarExpr(SContext, Source.DataType);
      ILWrite(EContext.SContext, TIL.IL_GetPtr(NewDst, nil, Source));
      Source := NewDst;
    end;
    if (Dest.DataTypeID <> dtWeakRef) and (Source.DataTypeID = dtWeakRef) then
      Instruction := TIL.IL_StrongRef(Dest, Source)
    else
    if (Dest.DataTypeID = dtWeakRef) and (Source.DataTypeID <> dtWeakRef) then
      Instruction := TIL.IL_WeakRef(Dest, Source)
    else
      Instruction := TIL.IL_Move(Dest, Source);
    ILWrite(SContext, Instruction);
  end else
  if EContext.RPNExprCount >  2 then
    Process_operator_AssignMulti(EContext)
  else
    ERROR_EMPTY_EXPRESSION;
end;

function TNPUnit.Process_operators(var EContext: TEContext; OpID: TOperatorID): TIDExpression;
var
  Left, Right: TIDExpression;
  SContext: PSContext;
  Op: TIDDeclaration;
  Instruction: TILInstruction;
  TmpVar: TIDVariable;
begin
  Result := nil;
  SContext := EContext.SContext;
  case OpID of
    opAssignment: Process_operator_Assign(EContext);
    opNegative: Result := Process_operator_neg(EContext);
    opNot: Result := Process_operator_not(EContext);
    opPositive: Result := RPNPopExpression(EContext);
    opDereference: Result := Process_operator_Deref(EContext);
    opCall: Result := Process_CALL(EContext);
    opPeriod: Result := Process_operator_Period(EContext);
    opAddr: Result := Process_operator_Addr(EContext);
    opIs: Result := Process_operator_Is(EContext);
    opAs: Result := Process_operator_As(EContext);
    opPostInc: begin
      // Читаем левый операнд
      Left := RPNPopExpression(EContext);
      Op := TIDType(MatchUnarOperator(OpID, Left.DataType));
      TmpVar := GetTMPVar(SContext, TIDType(Op));
      Result := TIDExpression.Create(TmpVar, Left.TextPosition);
      ILWrite(SContext, TIL.IL_Move(Result, Left));
      ILWrite(SContext, TIL.IL_Add(Left, Left, SYSUnit._OneExpression));
    end;
    opPostDec: begin
      // Читаем левый операнд
      Left := RPNPopExpression(EContext);
      Op := TIDType(MatchUnarOperator(OpID, Left.DataType));
      TmpVar := GetTMPVar(SContext, TIDType(Op));
      Result := TIDExpression.Create(TmpVar, Left.TextPosition);
      ILWrite(SContext, TIL.IL_Move(Result, Left));
      ILWrite(SContext, TIL.IL_Sub(Left, Left, SYSUnit._OneExpression));
    end;
    else begin
      // Читаем первый операнд
      Right := RPNPopExpression(EContext);
      // Читаем второй операнд
      Left := RPNPopExpression(EContext);

      // проверяем и если нужно разименовываем аргументы
      CheckAndMatchDerefExpression(SContext, Left);
      CheckAndMatchDerefExpression(SContext, Right);

      if OpID in [opShiftLeft, opShiftRight] then
        Op := MatchBinarOperator(SContext, OpID, Left, Left)
      else
        Op := MatchBinarOperator(SContext, OpID, Left, Right);

      if not Assigned(OP) and Left.IsDynArrayConst then
        Op := MatchBinarOperatorWithTuple(SContext, OpID, Left, Right);

      if not Assigned(OP) and Right.IsDynArrayConst then
        Op := MatchBinarOperatorWithTuple(SContext, OpID, Right, Left);

      if not Assigned(Op) then
        Op := MatchBinarOperatorWithImplicit(SContext, OpID, Left, Right);

      // если аргументы - константы, производим константные вычисления
      if (Left.IsConstant and Right.IsConstant) and
         (Left.Declaration.ClassType <> TIDSizeofConstant) and
         (Right.Declaration.ClassType <> TIDSizeofConstant) then
      begin
        Result := ProcessConstOperation(Left, Right, OpID);
        Exit;
      end else begin
        TmpVar := GetTMPVar(SContext, TIDType(Op));
        Result := TIDExpression.Create(TmpVar, Left.TextPosition);
      end;

      if Op.ItemType = itType then
      begin
        case OpID of
          opAdd: begin
            //ILWrite(SContext, TIL.IL_Add(Result, Left, Right));
            // если результат строка или дин. массив - помечаем переменную
            if Result.DataTypeID in [dtDynArray, dtString, dtAnsiString] then
              Result.AsVariable.IncludeFlags([VarTmpResOwner]);
          end;
          opSubtract: ;//ILWrite(SContext, TIL.IL_Sub(Result, Left, Right));
          opMultiply: ;//ILWrite(SContext, TIL.IL_Mul(Result, Left, Right));
          opDivide: ;//ILWrite(SContext, TIL.IL_Div(Result, Left, Right));
          opIntDiv: ;//ILWrite(SContext, TIL.IL_IntDiv(Result, Left, Right));
          opModDiv: ;//ILWrite(SContext, TIL.IL_ModDiv(Result, Left, Right));
          opEqual,
          opNotEqual,
          opLess,
          opLessOrEqual,
          opGreater,
          opGreaterOrEqual: begin
            Result := GetBoolResultExpr(Result);
            //ILWrite(SContext, TIL.IL_Cmp(Left, Right));
            {освобожадем временные переменные}
            ReleaseExpression(SContext, Left);
            ReleaseExpression(SContext, Right);
            //ILWrite(SContext, TIL.IL_JmpNext(Left.Line, cNone, nil));
            //Bool_AddExprNode(EContext, SContext.ILLast, TILCondition(Ord(OpID) - Ord(opEqual) + 1));
            Exit;
          end;
          opIn: Result := Process_operator_In(EContext, Left, Right);
          opAnd, opOr, opXor, opShiftLeft, opShiftRight: begin
            if (OpID in [opAnd, opOr]) and (TmpVar.DataType = SYSUnit._Boolean) then
            begin
              // логические операции
              //ReleaseExpression(Result);
              Result := GetBoolResultExpr(Result);
              Process_operator_logical_AND_OR(EContext, OpID, Left, Right, Result);
            end else begin
              // бинарные операции
              case OpID of
                opAnd: Instruction := TIL.IL_And(Result, Left, Right);
                opOr: Instruction := TIL.IL_Or(Result, Left, Right);
                opXor: Instruction := TIL.IL_Xor(Result, Left, Right);
                opShiftLeft: Instruction := TIL.IL_Shl(Result, Left, Right);
                opShiftRight: Instruction := TIL.IL_Shr(Result, Left, Right);
              else
                Instruction := nil;
              end;
              ILWrite(SContext, Instruction);
            end;
          end;
        else
          ERROR_UNSUPPORTED_OPERATOR(OpID);
        end;
      end else
      if Op.ItemType = itProcedure then begin
        // вызов перегруженного бинарного оператора
        Result := TIDCallExpression.Create(Op);
        Result.TextPosition := Left.TextPosition;
        TIDCallExpression(Result).ArgumentsCount := 2;
        TIDCallExpression(Result).Instance := GetOperatorInstance(OpID, Left, Right);
        Result := Process_CALL_direct(EContext.SContext, TIDCallExpression(Result), TIDExpressions.Create(Left, Right));
      end else
        ERROR_FEATURE_NOT_SUPPORTED;

      {освобожадем временные переменные}
      ReleaseExpression(SContext, Left);
      ReleaseExpression(SContext, Right);
    end;
  end;
end;

function TNPUnit.Process_operator_Period(var EContext: TEContext): TIDExpression;
var
  ValueType: TIDDeclaration;
  RangeType: TIDRangeType;
  Decl: TIDDeclaration;
  LB, HB: TIDExpression;
  RExpression: TIDRangeExpression;
begin
  HB := RPNPopExpression(EContext);
  LB := RPNPopExpression(EContext);

  CheckEmptyExpression(LB);
  CheckEmptyExpression(HB);

  CheckOrdinalExpression(LB);
  CheckOrdinalExpression(HB);

  ValueType := MatchImplicit(LB.DataType, HB.DataType);
  if not Assigned(ValueType) then
    AbortWork(sTypesMustBeIdentical, HB.TextPosition);

  RangeType := TIDRangeType.CreateAsSystem(nil, 'Range of ' + ValueType.Name);
  RangeType.ElementType := ValueType as TIDType;

  if LB.IsConstant and HB.IsConstant then
  begin
    if TIDConstant(LB.Declaration).CompareTo(TIDConstant(HB.Declaration)) > 0 then
      AbortWork(sLowerBoundExceedsHigherBound, HB.TextPosition);
  end;

  RExpression.LBExpression := LB;
  RExpression.HBExpression := HB;
  Decl := TIDRangeConstant.CreateAnonymous(nil, RangeType, RExpression);
  Result := TIDExpression.Create(Decl, HB.TextPosition);
end;

function TNPUnit.Process_operator_neg(var EContext: TEContext): TIDExpression;
var
  Right: TIDExpression;
  OperatorItem: TIDType;
begin
  // Читаем операнд
  Right := RPNPopExpression(EContext);

  ReleaseExpression(EContext.SContext, Right);

  OperatorItem := MatchUnarOperator(opNegative, Right.DataType);
  if not Assigned(OperatorItem) then
    ERROR_NO_OVERLOAD_OPERATOR_FOR_TYPES(opNegative, Right);

  if (Right.ItemType = itConst) and (Right.ClassType <> TIDSizeofConstant) then
    Result := ProcessConstOperation(Right, Right, opNegative)
  else begin
    Result := GetTMPVarExpr(EContext.SContext, OperatorItem, Right.TextPosition);
    ILWrite(EContext.SContext, TIL.IL_Neg(Result, Right));
  end;
end;

function TNPUnit.Process_operator_not(var EContext: TEContext): TIDExpression;
  procedure InverseNode(Node: PBoolExprNode); inline;
  begin
    case Node.NodeType of
      ntCmp: Node.Condition := InverseCondition(Node.Condition);
      ntAnd: Node.NodeType := ntOr;
      ntOr: Node.NodeType := ntAnd;
    end;

    if Assigned(Node.LeftChild) then
      InverseNode(Node.LeftChild);

    if Assigned(Node.RightChild) then
      InverseNode(Node.RightChild);
  end;
var
  Right: TIDExpression;
  OperatorItem: TIDType;
begin
  // Читаем операнд
  Right := RPNPopExpression(EContext);

  ReleaseExpression(EContext.SContext, Right);

  OperatorItem := MatchUnarOperator(opNot, Right.DataType);
  if not Assigned(OperatorItem) then
    ERROR_NO_OVERLOAD_OPERATOR_FOR_TYPES(opNot, Right);

  if (Right.ItemType = itConst) and (Right.ClassType <> TIDSizeofConstant) then
    Result := ProcessConstOperation(Right, Right, opNot)
  else begin
        // если это просто выражение (not Bool_Value)
    if (Right.DataTypeID = dtBoolean) and not (Right is TIDBoolResultExpression) then
        begin
          ILWrite(EContext.SContext, TIL.IL_Test(Right, Right));
          ILWrite(EContext.SContext, TIL.IL_JmpNext(Right.Line, cNone, nil));
          Bool_AddExprNode(EContext, EContext.SContext.ILLast, cNonZero);
        end;
        if not Assigned(EContext.LastBoolNode) then
        begin
          // т.к. это не логичиское отрицание
          // генерируем код бинарного отрицания
          Result := GetTMPVarExpr(EContext.SContext, OperatorItem, Right.TextPosition);
          ILWrite(EContext.SContext, TIL.IL_Not(Result, Right));
        end else begin
          // инвертируем условия сравнения
          InverseNode(EContext.LastBoolNode);
          Result := GetBoolResultExpr(EContext.SContext);
        end;
      end;
end;

function TNPUnit.GetMessagesText: string;
begin
  Result := FMessages.GetAsString;
end;

function TNPUnit.GetSource: string;
begin
  Result := FParser.Source;
end;

procedure TNPUnit.GetINFDeclarations(Items: TIDDeclarationList);
var
  Decl: TIDDeclaration;
begin
  {константы}
  Decl := FConsts.First;
  while Assigned(Decl) do
  begin
    if Decl.Scope = FIntfScope then
      Items.Add(Decl)
    else
      break;
    Decl := Decl.NextItem;
  end;
  {глобальные переменные}
  Decl := FVarSpace.First;
  while Assigned(Decl) do
  begin
    if Decl.Scope = FIntfScope then
      Items.Add(Decl)
    else
      break;
    Decl := Decl.NextItem;
  end;
  {типы}
  Decl := FTypeSpace.First;
  while Assigned(Decl) do
  begin
    if Decl.Scope = FIntfScope then
      Items.Add(Decl)
    else
      break;
    Decl := Decl.NextItem;
  end;
  {процедуры}
  Decl := FProcSpace.First;
  while Assigned(Decl) do
  begin
    if Decl.Scope = FIntfScope then
      Items.Add(Decl)
    else
      break;
    Decl := Decl.NextItem;
  end;
end;

procedure TNPUnit.GetSRCDeclarations(const SrcPos: TTextPosition; Items: TIDDeclarationList);
var
  Decl: TIDDeclaration;
  DPos: TTextPosition;
  i: Integer;
  UN: TNPUnit;
  CurType: TIDType;
  CurProc: TIDProcedure;
begin
  CurType := nil;
  CurProc := nil;
  {обьявления из подключенных модулей}
  for i := 0 to FIntfImportedUnits.Count - 1 do
  begin
    UN := FIntfImportedUnits.Objects[i] as TNPUnit;
    UN.GetINFDeclarations(Items);
  end;
  {константы}
  Decl := FConsts.First;
  while Assigned(Decl) do
  begin
    DPos := Decl.TextPosition;
    if (SrcPos.Row > DPos.Row) or
       ((SrcPos.Row = DPos.Row) and (SrcPos.Col >= DPos.Col)) then
    begin
      Items.Add(Decl);
    end else
      break;
    Decl := Decl.NextItem;
  end;
  {глобальные переменные}
  Decl := FVarSpace.First;
  while Assigned(Decl) do
  begin
    DPos := Decl.TextPosition;
    if (SrcPos.Row > DPos.Row) or
       ((SrcPos.Row = DPos.Row) and (SrcPos.Col >= DPos.Col)) then
    begin
      Items.Add(Decl);
    end else
      break;
    Decl := Decl.NextItem;
  end;
  {типы}
  Decl := FTypeSpace.First;
  while Assigned(Decl) do
  begin
    DPos := Decl.TextPosition;
    if (SrcPos.Row > DPos.Row) or
       ((SrcPos.Row = DPos.Row) and (SrcPos.Col >= DPos.Col)) then
    begin
      Items.Add(Decl);
    end else
      break;
    Decl := Decl.NextItem;
  end;
  {процедуры}
  Decl := FProcSpace.First;
  while Assigned(Decl) do
  begin
    DPos := Decl.TextPosition;
    if (SrcPos.Row > DPos.Row) or
       ((SrcPos.Row = DPos.Row) and (SrcPos.Col >= DPos.Col)) then
    begin
      Items.Add(Decl);
    end else
      break;
    Decl := Decl.NextItem;
  end;
  {текущий тип}
  if Assigned(CurType) then
  begin
  end;
  {текущая процедура}
  if Assigned(CurProc) then
  begin
  end;
end;

function TNPUnit.GetTMPVarExpr(SContext: PSContext; DataType: TIDType): TIDExpression;
begin
  Result := TIDExpression.Create(GetTMPVar(SContext, DataType));
end;

function TNPUnit.GetTMPVar(SContext: PSContext; DataType: TIDType; VarFlags: TVariableFlags): TIDVariable;
begin
  if Assigned(SContext) and SContext.WriteIL then
    Result := SContext.Proc.GetTMPVar(DataType, VarFlags)
  else begin
    Result := TIDVariable.CreateAsTemporary(nil, DataType);
    Result.IncludeFlags(VarFlags);
    FTMPVars.Push(Result);
  end;
end;

function TNPUnit.GetTMPVarExpr(SContext: PSContext; DataType: TIDType; const TextPos: TTextPosition): TIDExpression;
begin
  Result := TIDExpression.Create(GetTMPVar(SContext, DataType), TextPos);
end;

function TNPUnit.GetTMPVarExpr(var EContext: TEContext; DataType: TIDType; const TextPos: TTextPosition): TIDExpression;
begin
  Result := GetTMPVarExpr(EContext.SContext, DataType, TextPos);
end;

function TNPUnit.GetTMPVarExpr(SContext: PSContext; DataType: TIDType; VarFlags: TVariableFlags): TIDExpression;
begin
  Result := TIDExpression.Create(GetTMPVar(SContext, DataType, VarFlags));
end;

function TNPUnit.GetTMPRef(SContext: PSContext; DataType: TIDType): TIDVariable;
begin
  if Assigned(SContext) then
    Result := SContext.Proc.GetTMPRef(DataType)
  else
    Result := nil;
end;

function TNPUnit.GetTMPRefExpr(SContext: PSContext; DataType: TIDType): TIDExpression;
begin
  Result := TIDExpression.Create(GetTMPRef(SContext, DataType));
end;

function TNPUnit.GetTMPVar(SContext: PSContext; DataType: TIDType): TIDVariable;
begin
  if Assigned(SContext) and SContext.WriteIL then
    Result := SContext.Proc.GetTMPVar(DataType)
  else begin
    Result := TIDVariable.CreateAsTemporary(nil, DataType);
    FTMPVars.Push(Result);
  end;
end;

function TNPUnit.GetTMPVar(var EContext: TEContext; DataType: TIDType): TIDVariable;
begin
  Result := GetTMPVar(EContext.SContext, DataType);
end;

function TNPUnit.GetTMPVarExpr(const EContext: TEContext; DataType: TIDType): TIDExpression;
begin
  Result := GetTMPVarExpr(EContext.SContext, DataType);
end;

function TNPUnit.GetWeakRefType(Scope: TScope; SourceDataType: TIDType): TIDWeekRef;
begin
   if not (SourceDataType.DataTypeID in [dtClass, dtInterface]) then
     ERROR_WEAKREF_CANNOT_BE_DECLARED_FOR_TYPE(SourceDataType);

   Result := SourceDataType.WeakRefType;
   if not Assigned(Result) then
   begin
     Result := TIDWeekRef.CreateAnonymous(Scope, SourceDataType);
     Result.OverloadImplicitTo(SourceDataType);
     Result.OverloadImplicitFrom(SourceDataType);
     SourceDataType.WeakRefType := Result;
     AddType(Result);
   end;
end;

class function TNPUnit.GetBoolResultExpr(ExistExpr: TIDExpression): TIDBoolResultExpression;
begin
  Result := TIDBoolResultExpression.Create(ExistExpr.Declaration, ExistExpr.TextPosition);
end;

function TNPUnit.GetBoolResultExpr(SContext: PSContext): TIDBoolResultExpression;
var
  Decl: TIDVariable;
begin
  Decl := SContext.GetTMPVar(SYSUnit._Boolean);
  Result := TIDBoolResultExpression.Create(Decl, parser_Position);
end;

function TNPUnit.GetILText: string;
  function GetProcILText(const Proc: TIDProcedure): string;
  var
    VarsStr: string;
    NP: TIDProcedure;
  begin
    Result := '';
    Result := Result + '-------------------------' + #10 + GetProcName(Proc) + #10;
    VarsStr := Proc.GetDebugVariables;
    if VarsStr <> '' then begin
      Result := Result + 'var' + #10;
      Result := Result + VarsStr + #10;
    end;

    NP := Proc.ProcSpace.First;
    while Assigned(NP) do begin
      Result := Result + GetProcILText(NP);
      NP := TIDProcedure(NP.NextItem);
    end;

    Result := Result + 'begin' + #10;
    Result := Result + Proc.GetDebugIL + #10;
    Result := Result + 'end;' + #10;
  end;
var
  Proc: TIDProcedure;
  T: TIDType;
begin
  if FVarSpace.Count > 0 then begin
    Result := 'GLOBAL VARS'#10;
    Result := Result + GetVarDebugString(FVarSpace) + #10;
  end else
    Result := '';
  T := FTypeSpace.First;
  while Assigned(T) do
  begin
    if T.DataTypeID in [dtRecord, dtClass] then
    begin
      Proc := TIDStructure(T).Members.ProcSpace.First;
      if TIDStructure(T).ImportLib = 0 then
      while Assigned(Proc) do begin
        if not (pfImport in Proc.Flags) then
          Result := Result + GetProcILText(Proc);
        Proc := TIDProcedure(Proc.NextItem);
      end;
    end;
    Result := Result + 'type: ' + T.DisplayName + ' (rc: ' + IntTostr(T.RefCount) + ')'#10;
    T := TIDType(T.NextItem);
  end;

  Proc := FProcSpace.First;
  while Assigned(Proc) do begin
    if not (pfImport in Proc.Flags) then
      Result := Result + GetProcILText(Proc);
    Proc := TIDProcedure(Proc.NextItem);
  end;

  if Assigned(FInitProc) then
    Result := Result + GetProcILText(FInitProc);

  if Assigned(FFInalProc) then
    Result := Result + GetProcILText(FFInalProc);
end;

procedure TNPUnit.InitEContext(var EContext: TEContext; SContext: PSContext; EPosition: TExpessionPosition);
begin
  EContext.Initialize(Process_operators);
  EContext.SContext := SContext;
  if Assigned(SContext) then
    EContext.LastInstruction := SContext.ILLast;
  EContext.EPosition := EPosition;
end;

class function TNPUnit.IsConstEqual(const Left, Right: TIDExpression): Boolean;
var
  RExpr: TIDExpression;
begin
  RExpr := ProcessConstOperation(Left, Right, opEqual);
  Result := TIDBooleanConstant(RExpr.Declaration).Value;
end;

{parser methods}

procedure TNPUnit.parser_ReadTokenAsID(var Identifier: TIdentifier);
begin
  with FParser do begin
    if TokenCanBeID(TTokenID(CurrentTokenID)) then
    begin
      Identifier.Name := TokenLexem(TTokenID(CurrentTokenID));
      Identifier.TextPosition := Position;
    end else
      ERROR_IDENTIFIER_EXPECTED(TTokenID(CurrentTokenID));
  end;
end;

procedure TNPUnit.parser_ReadNextIdentifier(Scope: TScope; var Identifier: TIdentifier);
var
  Token: TTokenID;
begin
  Token := parser_NextToken(Scope);
  if Token = token_Identifier then
    FParser.GetIdentifier(Identifier)
  else
    parser_ReadTokenAsID(Identifier);
end;

procedure TNPUnit.parser_ReadSemicolon(Scope: TScope);
begin
  if parser_NextToken(Scope) <> token_semicolon then
    ERROR_SEMICOLON_EXPECTED;
end;

function TNPUnit.parser_ReadSemicolonAndToken(Scope: TScope): TTokenID;
begin
  Result := parser_NextToken(Scope);
  if Result = token_semicolon then
    Result := parser_NextToken(Scope);
end;

procedure TNPUnit.parser_ReadToken(Scope: TScope; const Token: TTokenID);
begin
  if parser_NextToken(Scope) <> Token then
    ERROR_EXPECTED_TOKEN(Token);
end;

procedure TNPUnit.parser_MatchToken(const ActualToken, ExpectedToken: TTokenID);
begin
  if ActualToken <> ExpectedToken then
    ERROR_EXPECTED_TOKEN(ExpectedToken, ActualToken);
end;

procedure TNPUnit.parser_MatchIdentifier(const ActualToken: TTokenID);
begin
  if ActualToken <> token_Identifier then
    if not TokenCanBeID(ActualToken) then
      ERROR_IDENTIFIER_EXPECTED(ActualToken);
end;

procedure TNPUnit.parser_MatchNextToken(Scope: TScope; ExpectedToken: TTokenID);
begin
  if parser_NextToken(Scope) <> ExpectedToken then
    ERROR_EXPECTED_TOKEN(ExpectedToken);
end;

procedure TNPUnit.parser_ReadCurrIdentifier(var Identifier: TIdentifier);
begin
  if parser_CurTokenID = token_identifier then
    FParser.GetIdentifier(Identifier)
  else
    FParser.GetTokenAsIdentifier(Identifier);
end;

function TNPUnit.parser_NextToken(Scope: TScope): TTokenID;
begin
  Result := TTokenID(FParser.NextToken);
  if Result >= token_cond_define then
    Result := ParseCondStatements(Scope, Result);
end;

function TNPUnit.RPNPushOperator(var EContext: TEContext; OpID: TOperatorID): TEContext.TRPNStatus;
begin
  Result := EContext.RPNPushOperator(OpID);
end;

procedure TNPUnit.RPNPushExpression(var EContext: TEContext; Operand: TIDExpression);
begin
  EContext.RPNPushExpression(Operand);
end;

function TNPUnit.RPNReadExpression(var EContext: TEContext; Index: Integer): TIDExpression;
begin
  {$IFDEF DEBUG}
  if (Index < 0) or (Index >= EContext.RPNExprCount) then
    AbortWork(sInvalidIndex, FParser.Position);
  {$ENDIF}
  Result := EContext.RPNReadExpression(Index);
end;

function TNPUnit.RPNPopExpression(var EContext: TEContext): TIDExpression;
begin
  Result := EContext.RPNPopExpression();
end;

procedure TNPUnit.SaveConstsToStream(Stream: TStream);
var
  i, ac, ec: Integer;
  ArrayDT: TIDArray;
  Decl: TIDConstant;
begin
  ac := FConsts.Count;
  Stream.WriteStretchUInt(ac);
  if ac = 0 then
    Exit;

  Decl := FConsts.First;
  while Assigned(Decl) do
  begin
    // сохраняем тип константы
    WriteDataTypeIndex(Stream, Decl.DataType);
    case Decl.DataTypeID of
      dtRecord: WriteConstToStream(Stream, Decl, Decl.DataType);
      dtStaticArray, dtDynArray, dtOpenArray: begin
        // кол-во элементов  (пока только одномерные массивы)
        ec := Length(TIDDynArrayConstant(Decl).Value);
        if Decl.DataTypeID = dtDynArray then
          Stream.WriteStretchUInt(ec);
        ArrayDT := TIDArray(Decl.DataType);
        // запись элементов
        for i := 0 to ec - 1 do
          WriteConstToStream(Stream, TIDConstant(TIDDynArrayConstant(Decl).Value[i].Declaration), ArrayDT.ElementDataType);
      end;
    else
      Decl.WriteToStream(Stream, Package);
    end;
    Decl := TIDConstant(Decl.NextItem);
  end;
end;

procedure TNPUnit.SaveMethodBodies(Stream: TStream);
var
  TDecl: TIDType;
  Cnt: Integer;
begin
  Cnt := 0;
  TDecl := FTypeSpace.First;
  while Assigned(TDecl) do
  begin
    TDecl := TDecl.ActualDataType;
    if (TDecl.DataTypeID in [dtRecord, dtClass]) and
       (TIDStructure(TDecl).MethodCount > 0) and
       (not Assigned(TDecl.GenericDescriptor)) and
       (TDecl.ImportLib = 0) then Inc(Cnt);
    TDecl := TDecl.NextItem as TIDType;
  end;

  // кол-во типов с методами
  Stream.WriteStretchUInt(Cnt);

  TDecl := FTypeSpace.First;
  while Assigned(TDecl) do
  begin
    TDecl := TDecl.ActualDataType;
    if (TDecl.DataTypeID in [dtRecord, dtClass]) and
       (TIDStructure(TDecl).MethodCount > 0) and
       (not Assigned(TDecl.GenericDescriptor)) and
       (TDecl.ImportLib = 0) then
    begin
      // пишем индекс типа
      Stream.WriteStretchUInt(TDecl.Index);
      // пишем тела методов
      TIDStructure(TDecl).SaveMethodBodiesToStream(Stream, FPackage);
    end;
    TDecl := TDecl.NextItem as TIDType;
  end;
end;

procedure WritePostForwardTypes(const Package: INPPackage; const Types: TTypeSpace; Stream: TStream);
var
  Decl: TIDType;
  cnt: Integer;
begin
  cnt := 0;
  Decl := Types.First;
  while Assigned(Decl) do
  begin
    if (Decl.DataTypeID = dtPointer) and TIDPointer(Decl).NeedForward then
      Inc(cnt);
    Decl := TIDType(Decl.NextItem);
  end;
  Stream.WriteStretchUInt(cnt);

  if cnt = 0 then
    Exit;

  Decl := Types.First;
  while Assigned(Decl) do
  begin
    if (Decl.DataTypeID = dtPointer) and TIDPointer(Decl).NeedForward then
    begin
      Stream.WriteStretchUInt(Decl.Index);
      Decl.SaveDeclToStream(Stream, Package);
    end;
    Decl := TIDType(Decl.NextItem);
  end;
end;

procedure TNPUnit.SaveTypesToStream(Stream: TStream);
var
  TDecl: TIDType;
begin
  if Self <> SYSUnit then
  begin
    Stream.WriteStretchUInt(FTypeSpace.Count);
    // сначала сохраняем id всех типов
    TDecl := FTypeSpace.First;
    while Assigned(TDecl) do begin
      Stream.WriteUInt8(UInt8(TDecl.DataTypeID));
      TDecl := TIDType(TDecl.NextItem);
    end;
    // сохраняем содержиоме типов
    TDecl := FTypeSpace.First;
    while Assigned(TDecl) do begin
      TDecl.SaveDeclToStream(Stream, FPackage);
      TDecl := TIDType(TDecl.NextItem);
    end;
  end else begin
    {из модуля system cохраняем только пользовательские типы}
    Stream.WriteStretchUInt(FTypeSpace.Count - Ord(TDataTypeID.dtPointer) - 1);
    // сначала сохраняем id всех типов
    TDecl := FTypeSpace.First;
    while Assigned(TDecl) do begin
      if TDecl.Index > Ord(TDataTypeID.dtPointer) then
        Stream.WriteUInt8(UInt8(TDecl.DataTypeID));
      TDecl := TIDType(TDecl.NextItem);
    end;
    // сохраняем содержиоме типов
    TDecl := FTypeSpace.First;
    while Assigned(TDecl) do begin
      if TDecl.Index > Ord(TDataTypeID.dtPointer) then
        TDecl.SaveDeclToStream(Stream, FPackage);
      TDecl := TIDType(TDecl.NextItem);
    end;
  end;
end;

procedure TNPUnit.CheckAndDelGenericProcs(var ProcSpace: TProcSpace);
var
  P: TIDProcedure;
begin
  P := ProcSpace.First;
  while Assigned(P) do
  begin
    if Assigned(P.GenericDescriptor) then
      ProcSpace.Delete(P);
    P := TIDProcedure(P.NextItem);
  end;
end;

procedure TNPUnit.SaveDeclToStream(Stream: TStream);
var
  VDecl: TIDVariable;
  Idx: Integer;
  Flags: Byte;
begin
  Flags := 0;
  {секция инициализации}
  if TIL(FInitProc.IL).Count > 0 then
    Flags := ILUNIT_HASINIT;

  {секция финализации}
  if TIL(FFinalProc.IL).Count > 0  then
    Flags := Flags + ILUNIT_HASFINAL;

  Stream.WriteStretchUInt(Flags);

  {RTTI имя}
  Idx := FPackage.GetStringConstant(Name);
  Stream.WriteStretchUInt(Idx);

  CheckAndDelGenericProcs(FProcSpace);

  {типы}
  SaveTypesToStream(Stream);

  {константные массивы/структуры}
  SaveConstsToStream(Stream);

  {глобальные переменные}
  Stream.WriteStretchUInt(FVarSpace.Count); // кол-во глобальных переменных
  VDecl := FVarSpace.First;
  while Assigned(VDecl) do begin
    TIDVariable(VDecl).SaveToStream(Stream, FPackage);
    VDecl := TIDVariable(VDecl.NextItem);
  end;

  {декларации процедуры}
  SaveProcDecls(Stream, Package, addr(FProcSpace));
end;

procedure TNPUnit.SaveBodyToStream(Stream: TStream);
var
  i: Integer;
  BP: TBreakPoint;
begin
  {$IFDEF DEBUG_WRITE_LEBELS}DBG_WRITE_UNIT_BODY_LABEL(Stream, Self.Name);{$ENDIF}
  {тела процедур}
  SaveProcBodies(Stream, Package, @FProcSpace);

  {тела методы объектов}
  SaveMethodBodies(Stream);

  {секция инициализации}
  if TIL(FInitProc.IL).Count > 0 then
    FInitProc.SaveBodyToStream(Stream, Package);

  {секция финализации}
  if TIL(FFinalProc.IL).Count > 0 then
    FFInalProc.SaveBodyToStream(Stream, Package);

  if FPackage.IncludeDebugInfo then
  begin
    Stream.WriteStretchUInt(FBreakPoints.Count);
    for i := 0 to FBreakPoints.Count - 1 do
    begin
      BP := FBreakPoints[i];
      Stream.WriteStretchUInt(BP.Line);
    end;
  end;
end;

function TNPUnit.ParseGenericProcRepeatedly(Scope: TScope; GenericProc, Proc: TIDProcedure; Struct: TIDStructure): TTokenID;
{type
  TFwdDeclState = (dsNew, dsDifferent, dsSame);}
var
  Parameters: TScope;
  ResultType: TIDType;
  RetVar: TIDVariable;
  VarSpace: TVarSpace;
  FirstSkipCnt: Integer;
  CurParserPos: TParserPosition;
  ProcFlags: TProcFlags;
  GD: PGenericDescriptor;
begin
  GD := GenericProc.GenericDescriptor;
  if not Assigned(GD) then
    GD := Struct.GenericDescriptor;

  if not Assigned(GD) then
    Assert(Assigned(GD));

  {перемещаем парсер на исходный код generic-процедуры}
  FParser.SaveState(CurParserPos);
  FParser.LoadState(GD.ImplSRCPosition);

  Result := parser_CurTokenID;

  VarSpace.Initialize;
  if Assigned(Struct) then
    Parameters := TMethodScope.CreateInDecl(Scope, Struct.Members, @VarSpace, nil)
  else
    Parameters := TProcScope.CreateInDecl(Scope, @VarSpace, nil);

  // создаем Result переменную (пока без имени) и добовляем ее в VarSpace чтобы зарезервировать индекс
  if Assigned(GenericProc.ResultType) then
    RetVar := AddResultParameter(Parameters)
  else
    RetVar := nil;

  if Assigned(Struct) then
    AddSelfParameter(Parameters, Struct, False);

  // парсим параметры
  if Result = token_openround then
  begin
    ParseParameters(Parameters);
    Result := parser_NextToken(Scope); // move to "token_colon"
  end;

  // парсим тип возвращаемого значения
  if Assigned(GenericProc.ResultType) then
  begin
    parser_MatchToken(Result, token_colon);
    Result := ParseTypeSpec(Parameters, ResultType);
    RetVar.DataType := ResultType;
    RetVar.TextPosition := parser_Position;
  end else
    ResultType := nil;

  parser_MatchToken(Result, token_semicolon);

  if not Assigned(Struct) then
  begin
    // Для Scope будут переопределены VarSpace и ProcSpace
    Proc.ParamsScope := Parameters;
    Proc.VarSpace := VarSpace;
    Proc.ResultType := ResultType;
    FirstSkipCnt := 0;
    if Assigned(ResultType) then
      Inc(FirstSkipCnt);
    Proc.ExplicitParams := ScopeToVarList(Parameters, FirstSkipCnt);
    FImplScope.AddProcedure(Proc);   // добовляем новую декларацию в структуру или глобольный список
  end;
  ProcFlags := [];

  Result := parser_NextToken(Scope);
  while True do begin
    case Result of
      token_forward: Result := ProcSpec_Forward(Scope, Proc, ProcFlags);
      token_export: Result := ProcSpec_Export(Scope, Proc, ProcFlags);
      token_inline: Result := ProcSpec_Inline(Scope, Proc, ProcFlags);
      //token_noreturn: Result := ProcSpec_NoReturn(Scope, Proc, ProcFlags);
      token_external: Result := ProcSpec_Import(Scope, Proc, ProcFlags);
      token_overload: Result := ProcSpec_Overload(Scope, Proc, ProcFlags);
      //token_pure: Result := ProcSpec_Pure(Scope, Proc, ProcFlags);
      token_procedure,
      token_function,
      token_const,
      token_type,
      token_var,
      token_begin:
      begin
        // т.к синтаксис проверен ранее, флаги процедуры нет необходимости проверять

        // имена парметров реализации процедуры могут отличатся от ее определения
        // копируем накопленный VarSpace в процедуру
        Proc.VarSpace := Parameters.VarSpace^;
        // Для Scope будут переопределены VarSpace и ProcSpace
        Proc.EntryScope := Parameters;
        Result := ParseProcBody(Proc, nil);
        parser_MatchSemicolon(Result);
        Result := parser_NextToken(Scope);
        Break;
      end;
      token_identifier: ERROR_KEYWORD_EXPECTED;
    else
      Break;
    end;
  end;
  FParser.LoadState(CurParserPos);
end;

function FindGenericInstance(const GenericInstances: TGenericInstanceList; const SpecializeArgs: TIDExpressions): TIDDeclaration;
var
  i, ac, ai: Integer;
  Item: ^TGenericInstance;
  SrcArg, DstArg: TIDExpression;
begin
  for i := 0 to Length(GenericInstances) - 1 do
  begin
    Item := @GenericInstances[i];
    ac := Length(SpecializeArgs);
    if ac <> Length(Item.Args) then
      AbortWorkInternal('Wrong length generics arguments');
    for ai := 0 to ac - 1 do
    begin
      DstArg := Item.Args[ai];
      SrcArg := SpecializeArgs[ai];
      // тут упрощенная проверка:
      case SrcArg.ItemType of
        itType: begin
          if SrcArg.AsType.ActualDataType <> DstArg.AsType.ActualDataType then
            continue;
        end;
        itConst, itVar: begin
          if SrcArg.DataType.ActualDataType <> DstArg.AsType.ActualDataType then
            continue;
        end;
        else
          AbortWork(sFeatureNotSupported, SrcArg.TextPosition);
      end;
      Exit(Item.Instance);
    end;
  end;
  Result := nil;
end;

function TNPUnit.SpecializeGenericProc(CallExpr: TIDCallExpression; const CallArgs: TIDExpressions): TIDProcedure;
var
  i, ai, ac, pc: Integer;
  SrcArg: TIDExpression;
  SpecializeArgs: TIDExpressions;
  Proc: TIDProcedure;
  GDescriptor: PGenericDescriptor;
  Scope: TScope;
  Param: TIDDeclaration;
  ProcName: string;
begin
  Proc := CallExpr.AsProcedure;
  GDescriptor := Proc.GenericDescriptor;

  {подготавливаем списко обобщенных аргументов}
  SpecializeArgs := CallExpr.GenericArgs;
  if Length(SpecializeArgs) = 0 then
  begin
    ac := Length(GDescriptor.GenericParams);
    pc := Length(Proc.ExplicitParams);
    if pc <> ac then
      AbortWork(sProcRequiresExplicitTypeArgumentFmt, [CallExpr.AsProcedure.ProcTypeName, CallExpr.DisplayName], CallExpr.TextPosition);
    ai := 0;
    SetLength(SpecializeArgs, ac);

    for i := 0 to pc - 1 do
    begin
      Param := Proc.ExplicitParams[i];
      if Param.DataTypeID = dtGeneric then
      begin
        if CallExpr.ArgumentsCount > i then
          SrcArg := CallArgs[i]
        else
          SrcArg := TIDVariable(Param).DefaultValue;
        SpecializeArgs[ai] := TIDExpression.Create(SrcArg.DataType, SrcArg.TextPosition);
        Inc(ai);
      end;
    end;
  end;

  Result := TIDProcedure(FindGenericInstance(GDescriptor.GenericInstances, SpecializeArgs));
  if Assigned(Result) then
    Exit;

  Scope := TScope.Create(Proc.Scope.ScopeType, Proc.EntryScope);
  {$IFDEF DEBUG}Scope.Name := Proc.DisplayName + '.generic_alias_scope';{$ENDIF}
  Proc.EntryScope.AddScope(Scope);

  {формируем уникальное название процедуры}
  for i := 0 to Length(SpecializeArgs) - 1 do
  begin
    SrcArg := SpecializeArgs[i];
    ProcName := AddStringSegment(ProcName, SrcArg.DisplayName, ', ');
    Param := TIDAlias.CreateAlias(Scope, GDescriptor.GenericParams[i].ID, SrcArg.Declaration);
    Scope.InsertID(Param);
  end;
  ProcName := Proc.Name + '<' + ProcName + '>';
  Result := TIDProcedure.Create(Proc.Scope, Identifier(ProcName, Proc.ID.TextPosition));

  {добовляем специализацию в пул }
  GDescriptor.AddGenericInstance(Result, SpecializeArgs);

  try
    ParseGenericProcRepeatedly(Scope, Proc, Result, Proc.Struct);
  except
    on e: ECompilerAbort do begin
      PutMessage(cmtError, Format(IfThen(Assigned(CallExpr.AsProcedure.ResultType), msgGenericFuncInstanceErrorFmt, msgGenericProcInstanceErrorFmt), [ProcName]), CallExpr.TextPosition);
      raise;
    end;
  end;
end;

function TNPUnit.SpecializeGenericType(GenericType: TIDType; const ID: TIdentifier; const SpecializeArgs: TIDExpressions): TIDType;
var
  i: Integer;
  GAScope: TScope;  // специальный скоуп для алиасов-параметров параметрического типа
  Param: TIDAlias;
  NewID: TIdentifier;
  TypeName: string;
  SrcArg: TIDExpression;
  ParserPos: TParserPosition;
  GDescriptor: PGenericDescriptor;
  GProc, SProc: TIDProcedure;
  GMethods, SMethods: PProcSpace;
begin
  GDescriptor := GenericType.GenericDescriptor;
  {поиск подходящей специализации в пуле}
  Result := TIDType(FindGenericInstance(GDescriptor.GenericInstances, SpecializeArgs));
  if Assigned(Result) then
    Exit;

  GAScope := TScope.Create(GenericType.Scope.ScopeType, GenericType.Scope);

  {формируем название типа}
  for i := 0 to Length(SpecializeArgs) - 1 do
  begin
    SrcArg := SpecializeArgs[i];
    TypeName := AddStringSegment(TypeName, SrcArg.DisplayName, ', ');
    Param := TIDAlias.CreateAlias(GAScope, GDescriptor.GenericParams[i].ID, SrcArg.Declaration);
    GAScope.InsertID(Param);
  end;

  NewID.Name := GenericType.Name + '<' + TypeName + '>';
  NewID.TextPosition := ID.TextPosition;
  {$IFDEF DEBUG}GAScope.Name := NewID.Name + '.generic_alias_scope';{$ENDIF}

  FParser.SaveState(ParserPos);
  FParser.LoadState(GDescriptor.ImplSRCPosition);
  parser_NextToken(GAScope);

  {парсим заново generic-тип}
  ParseTypeDecl(GenericType.Scope, GAScope, nil, NewID, Result);

  {если есть методы, пытаемся их перекомпилировать}
  if GenericType.InheritsFrom(TIDStructure) then
  begin
    GMethods := TIDStructure(GenericType).Methods;
    SMethods := TIDStructure(Result).Methods;
    GProc := GMethods.First;
    SProc := SMethods.First;
    while Assigned(GProc) do
    begin
      if Assigned(GProc.IL) and Assigned(GProc.GenericDescriptor) then
        ParseGenericProcRepeatedly(FImplScope, GProc, SProc, TIDStructure(Result))
      else
        SProc.GenericPrototype := GProc;
      GProc := TIDProcedure(GProc.NextItem);
      SProc := TIDProcedure(SProc.NextItem);
    end
  end;

  FParser.LoadState(ParserPos);

  {добовляем специализацию в пул}
  GDescriptor.AddGenericInstance(Result, SpecializeArgs);
end;

procedure TNPUnit.Warning(const Message: string; const Params: array of const; const TextPosition: TTextPosition);
begin
  PutMessage(cmtWarning, Format(Message, Params), TextPosition);
end;

procedure TNPUnit.Hint(const Message: string; const Params: array of const; const TextPosition: TTextPosition);
begin
  PutMessage(cmtHint, Format(Message, Params), TextPosition);
end;

procedure TNPUnit.Hint(const Message: string; const Params: array of const);
begin
  PutMessage(cmtHint, Format(Message, Params));
end;

procedure TNPUnit.HINT_RESULT_EXPR_IS_NOT_USED(const Expr: TIDExpression);
begin
  if Assigned(Expr.DataType) then
    PutMessage(cmtHint, 'Expression result (type: ' + Expr.DataType.DisplayName + ') is not used', Expr.TextPosition)
  else
    PutMessage(cmtHint, 'Expression result is not used', Expr.TextPosition);
end;

procedure TNPUnit.HINT_TEXT_AFTER_END;
begin
  PutMessage(cmtHint, 'Text after final END. - ignored by compiler', parser_PrevPosition);
end;


{ TPMContext }

procedure TPMContext.Add(const Expr: TIDExpression);
begin
  SetLength(FItems, FCnt + 1);
  FItems[FCnt] := Expr;
  Inc(FCnt);
end;

procedure TPMContext.Clear;
begin
  FItems := nil;
  FCnt := 0;
end;

function TPMContext.GetLast: TIDExpression;
begin
  if FCnt > 0 then
    Result := FItems[FCnt - 1]
  else
    Result := nil;
end;

procedure TPMContext.Init;
begin
  FCnt := 0;
  DataType := nil;
end;

type
  ttt = reference to procedure;

var
  g: int32;

procedure Test;
var
  i: int32;
  p1, p2: ttt;
begin
  g := SizeOf(p1);
  i := 12;
   P1 := procedure begin
         i := i + 1;
       end;

  P2 := procedure begin
         i := i * 1;
       end;

  p2();
  p1();
  if i <> 0 then;


  if i <> 0 then;

end;

{ TLoopCodeContext }

(*constructor TLoopCodeContext.Create(Proc: TIDProcedure; ArrParam: TIDVariable);
begin
  FillChar(Self, Sizeof(Self), #0);
  Self.Proc := Proc;
  Self.IL := TIL(Proc.IL);
  Self.Arr := ArrParam.DataType as TIDArray;
  Self.ArrParam := ArrParam;
  RET := TIL.IL_Ret(0);
end;       *)

procedure TNPUnit.CondCompPush(Condition: Boolean);
begin

end;

procedure TNPUnit.CondCompPop;
begin

end;

function TNPUnit.CheckAndParseDeprecated(Scope: TScope; CurrToken: TTokenID): TTokenID;
var
  MessageExpr: TIDExpression;
begin
  Result := CurrToken;
  if CurrToken = token_deprecated then
  begin
    Result := parser_NextToken(Scope);
    if Result = token_identifier then
    begin
      Result := ParseConstExpression(Scope, MessageExpr, TExpessionPosition.ExprRValue);
      CheckStringExpression(MessageExpr);
    end;
  end;
end;

function TNPUnit.CheckAndParseAttribute(Scope: TScope): TTokenID;
begin
  // todo: complete the attributes parsing
  Result := parser_CurTokenID;
  if Result = token_openblock then
    Result := ParseAttribute(Scope);
end;

function TNPUnit.ParseAttribute(Scope: TScope): TTokenID;
begin
  while (Result <> token_closeblock) and (Result <> token_eof) do
  begin
    Result := parser_NextToken(Scope);
  end;
  Result := parser_NextToken(Scope);
end;

function TNPUnit.ParseCaseRecord(Scope: TScope; Decl: TIDStructure): TTokenID;
var
  Expr: TIDExpression;
begin
  parser_NextToken(Scope);
  Result := ParseConstExpression(Scope, Expr, ExprNested);
  parser_MatchToken(Result,  token_of);
  Result := parser_NextToken(Scope);
  while Result <> token_eof do
  begin
    // parse case lable const expression: (example: ... 1: (a: integer; b: integer ...
    Result := ParseConstExpression(Scope, Expr, ExprNested);
    parser_MatchToken(Result, token_colon);
    parser_ReadToken(Scope, token_openround);
    Result := parser_NextToken(Scope);
    if Result = token_case then
    begin
      Result := ParseCaseRecord(Scope, Decl);
      parser_MatchToken(Result, token_closeround);
      parser_ReadSemicolon(Scope);
      Result := parser_NextToken(Scope);
    end else
    if Result <> token_closeround then
      Result := ParseVarInCaseRecord(Scope, vPublic, Decl);

    if Result = token_closeround then
    begin
      parser_ReadSemicolon(Scope);
      Result := parser_NextToken(Scope);
      case Result of
        token_minus, token_identifier: Continue;
        token_closeround, token_end: Exit;
      else
        ERROR_IDENTIFIER_EXPECTED(Result);
      end;
    end else
    if Result = token_case then
    begin
      Result := ParseCaseRecord(Scope, Decl);
      parser_MatchToken(Result, token_closeround);
      parser_ReadSemicolon(Scope);
      Result := parser_NextToken(Scope);
    end;
    if Result = token_closeround then
      Exit;
  end;
end;

function TNPUnit.ParseVarInCaseRecord(Scope: TScope; Visibility: TVisibility; Struct: TIDStructure): TTokenID;
var
  i, c: Integer;
  DataType: TIDType;
  DefaultValue: TIDExpression;
  ID: TIdentifier;
  Field: TIDVariable;
  Names: TIdentifiersPool;
  VarFlags: TVariableFlags;
begin
  c := 0;
  Names := TIdentifiersPool.Create(2);
  Result := parser_CurTokenID;
  while True do begin
    VarFlags := [];
    Result := CheckAndParseAttribute(Scope);
    parser_MatchIdentifier(Result);
    Names.Add;
    parser_ReadCurrIdentifier(Names.Items[c]);
    Result := parser_NextToken(Scope);
    if Result = token_Coma then begin
      Inc(c);
      Result := parser_NextToken(Scope);
      Continue;
    end;
    parser_MatchToken(Result, token_colon);
    // парсим тип
    Result := ParseTypeSpec(Scope, DataType);
    DefaultValue := nil;
    // спецификатор NOT NULL/NULLABLE
    case Result of
      token_exclamation: begin
        Include(VarFlags, VarNotNull);
        Result := parser_NextToken(Scope);
      end;
      token_question: begin
        Exclude(VarFlags, VarNotNull);
        Result := parser_NextToken(Scope);
      end;
    end;
    case Result of
      // значение по умолчанию
      token_equal: Result := ParseVarDefaultValue(Scope, DataType, DefaultValue);
    end;

    for i := 0 to c do begin
      if not Assigned(Struct) then
        Field := TIDVariable.Create(Scope, Names.Items[i])
      else
        Field := TIDField.Create(Struct, Names.Items[i]);
      Field.DataType := DataType;
      Field.Visibility := Visibility;
      Field.DefaultValue := DefaultValue;
      Field.Flags := Field.Flags + VarFlags;
      Scope.AddVariable(Field);
    end;

    if Result = token_semicolon then
    begin
      Result := parser_NextToken(Scope);
      if TokenCanBeID(Result) then
      begin
        c := 0;
        Continue;
      end;
    end;
    Break;
  end;
end;

function TNPUnit.CheckAndParseProcTypeCallConv(Scope: TScope; TypeDecl: TIDType): TTokenID;
begin
  Result := parser_NextToken(Scope);
  case Result of
    token_stdcall: begin
      TIDProcType(TypeDecl).CallConv := ConvStdCall;
      parser_ReadSemicolon(Scope);
      Result := parser_NextToken(Scope);
    end;
    token_fastcall: begin
      TIDProcType(TypeDecl).CallConv := ConvFastCall;
      parser_ReadSemicolon(Scope);
      Result := parser_NextToken(Scope);
    end;
    token_cdecl: begin
      TIDProcType(TypeDecl).CallConv := ConvCDecl;
      parser_ReadSemicolon(Scope);
      Result := parser_NextToken(Scope);
    end;
  end;
end;

function TNPUnit.ParseDeprecated(Scope: TScope; out &Deprecated: TIDExpression): TTokenID;
begin
  Result := parser_NextToken(Scope);
  if Result = token_identifier then
  begin
    Result := ParseConstExpression(Scope, &Deprecated, TExpessionPosition.ExprRValue);
    CheckStringExpression(&Deprecated);
  end else
    &Deprecated  := TIDExpression.Create(SYSUnit._DeprecatedDefaultStr, parser_Position);
end;


procedure TNPUnit.CheckAndCallFuncImplicit(const EContext: TEContext);
var
  Expr: TIDExpression;
  PExpr: TIDCallExpression;
begin
  Expr := EContext.Result;
  if not Assigned(Expr) then
    Exit;

  Expr := EContext.RPNPopExpression();
  Expr := CheckAndCallFuncImplicit(EContext.SContext, Expr);
  if Assigned(Expr) then
    EContext.RPNPushExpression(Expr);
end;

function TNPUnit.CheckAndCallFuncImplicit(SContext: PSContext; Source: TIDExpression): TIDExpression;
var
  Expr: TIDExpression;
  PExpr: TIDCallExpression;
begin
  if Source.DataTypeID <> dtProcType then
    Exit(nil);

  PExpr := TIDCallExpression.Create(Source.Declaration, Source.TextPosition);
  Result := Process_CALL_direct(SContext, PExpr, []);
end;

function TNPUnit.ParseVarRecordDefaultValue(Scope: TScope; Struct: TIDStructure;
                                            out DefaultValue: TIDExpression): TTokenID;
var
  i: Integer;
  ID: TIdentifier;
  Expr: TIDExpression;
  Decl: TIDDeclaration;
  Expressions: TIDRecordConstantFields;
begin
  i := 0;
  parser_MatchNextToken(Scope, token_openround);
  SetLength(Expressions, Struct.FieldsCount);
  while True do begin
    parser_ReadNextIdentifier(Scope, ID);
    parser_MatchNextToken(Scope, token_colon);
    Result := parser_NextToken(Scope);
    Result := ParseConstExpression(Scope, Expr, ExprRValue);
    Expressions[i].Field := Struct.FindField(ID.Name);
    Expressions[i].Value := Expr;
    Inc(i);
    if Result = token_semicolon then
      Continue;
    Break;
  end;
  // create anonymous record constant
  Decl := TIDRecordConstant.CreateAnonymous(Scope, Struct, Expressions);
  DefaultValue := TIDExpression.Create(Decl, parser_Position);

  parser_MatchToken(Result, token_closeround);
  Result := parser_NextToken(Scope);
end;


function TNPUnit.ParsePlatform(Scope: TScope): TTokenID;
begin
  Result := parser_NextToken(Scope);
end;

function TNPUnit.GetDefinesAsString: string;
begin
  Result := FDefines.Text;
end;

function TNPUnit.parser_SkipTo(Scope: TScope; StopToken: TTokenID): TTokenID;
begin
  while true do begin
    Result := parser_NextToken(Scope);
    if (Result = StopToken) or (Result = token_eof) then
      Exit;
  end;
end;

initialization
  Test();
  FormatSettings.DecimalSeparator := '.';

end.

