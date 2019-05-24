//====================================================================================================================//
//=========================================== THE OBJECT PASCAL COMPILER =============================================//
//====================================================================================================================//
unit OPCompiler;

interface

{$I compilers.inc}

uses SysUtils, Math, Classes, StrUtils, Types, IOUtils, Generics.Collections,
     OPCompiler.Parser,
     AST.Delphi.Classes,
     NPCompiler.DataTypes,
     iDStringParser,
     NPCompiler.Operators,
     NPCompiler.Errors,
     NPCompiler.Utils,
     NPCompiler.Intf,
     NPCompiler.Contexts,
     AST.Parser.Contexts,
     AST.Delphi.Contexts,
     AST.Classes,
     NPCompiler.Options,

     AST.Project;

type

  TNPUnit = class;
  TUnitSection = (usInterface, usImplementation);

  TIDPlatform = class
  end;

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

  TIDDeclarationList = TList<TIDDeclaration>;

  TNPUnit = class(TASTModule)
  type
    TVarModifyPlace = (vmpAssignment, vmpPassArgument);
    TIdentifiersPool = TPool<TIdentifier>;
    TCondIFValue = (condIFFalse, condIfTrue, condIFUnknown);
  strict private
    //////////////////////////////////////////////////////////////////////////////////////////
    procedure RPNPushExpression(var EContext: TEContext; Operand: TIDExpression); inline;
    function RPNReadExpression(var EContext: TEContext; Index: Integer): TIDExpression; inline;
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
    FDefines: TDefines;
    fCondStack: TSimpleStack<Boolean>;

    FConsts: TConstSpace;              // список нетривиальных констант (массивы, структуры)
    FCompiled: Boolean;

    FSystemExplicitUse: Boolean;
    FOptions: TCompilerOptions;
    function GetMessagesText: string;
    //========================================================================================================
    // процедуры генерации ошибок компиляции
  public
    class procedure ERROR_ARG_VAR_REQUIRED(Expr: TIDExpression); static;
    class procedure ERROR_CONST_EXPRESSION_REQUIRED(Expr: TIDExpression); static;
    class procedure ERROR_INCOMPATIBLE_TYPES(const Src, Dst: TIDExpression); overload; static;
    class procedure ERROR_INCOMPATIBLE_TYPES(const Src: TIDExpression; Dst: TIDType); overload; static;
    class procedure ERROR_INVALID_EXPLICIT_TYPECAST(const Src: TIDExpression; Dst: TIDType); static;
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

    //property BENodesPool: TBENodesPool read FBENodesPool;
    property InitProc: TIDProcedure read FInitProc;
    property FinalProc: TIDProcedure read FFinalProc;
    //========================================================================================================
    function ProcSpec_Inline(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
    function ProcSpec_Export(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
    function ProcSpec_Forward(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
    function ProcSpec_Import(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
    function ProcSpec_Overload(Scope: TScope; Proc: TIDProcedure; var Flags: TProcFlags): TTokenID;
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
    procedure CondCompPush(Condition: Boolean);
    procedure CondCompPop;
    procedure SetUnitName(const Name: string);
  public
    function GetModuleName: string; override;
    function GetFirstFunc: TASTDeclaration; override;
    function GetFirstVar: TASTDeclaration; override;
    function GetFirstType: TASTDeclaration; override;
    function GetFirstConst: TASTDeclaration; override;
    function CreateSysProc(const SysName: string): TIDProcedure;
    function CreateArraySysProc(const Arr: TIDArray; const Name: string; out ProcParam: TIDVariable): TIDProcedure;

    class procedure ERROR_VAR_IS_NOT_INITIALIZED(const Variable: TIDExpression);
    class function IsConstValueInRange(Value: TIDExpression; RangeExpr: TIDRangeConstant): Boolean; static;
    class function IsConstRangesIntersect(const Left, Right: TIDRangeConstant): Boolean; static;
    class function IsConstEqual(const Left, Right: TIDExpression): Boolean; static;
    { быстрая функция генерации вызова явно указанной процедуры (без перебора перегруженных процедур) }
    function Process_CALL_direct(SContext: PSContext; PExpr: TIDCallExpression; CallArguments: TIDExpressions): TIDExpression;
    //======================================================================================================================================
    function ProcessBuiltin_SetLength(var EContext: TEContext): TIDExpression;
    function ProcessBuiltin_Copy(var EContext: TEContext): TIDExpression;
    function ProcessBuiltin_Move(var EContext: TEContext): TIDExpression;
    function ProcessBuiltin_MemSet(var EContext: TEContext): TIDExpression;
    function ProcessBuiltin_Assert(var EContext: TEContext; const ParamsTest: string; SourceRow: Integer): TIDExpression;
    function ProcessBuiltin_Include(var EContext: TEContext): TIDExpression;
    function ProcessBuiltin_Exclude(var EContext: TEContext): TIDExpression;
    //======================================================================================================================================
    { Поиск и удаление неиспользуемых переменных/типов/процедур }
    procedure CheckUnusedVariables(VarSpace: PVarSpace);
    function CheckUnusedRecordType(Struct: TIDStructure): Boolean;
    function CheckAndDelUnusedTypes(var TypeSpace: TTypeSpace): Integer;
    function CheckAndDelUnusedProcs(var ProcSpace: TProcSpace): Integer;
    function CheckAndDelUnusedConsts(var ConstSpace: TConstSpace): Integer;
    procedure CheckAndDelGenericTypes(var TypeSpace: TTypeSpace);
    procedure CheckAndDelGenericProcs(var ProcSpace: TProcSpace);
    procedure CheckInitVariables(SContext: PSContext; ParentDecl: TIDMultiExpression; VarSpace: PVarSpace);

    procedure CheckIntfSectionMissing(Scope: TScope); inline;
    procedure CheckImplicitTypes(Src, Dst: TIDType; Position: TTextPosition); inline;
    procedure CheckLeftOperand(const Status: TRPNStatus); inline;
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
    class procedure InsertToScope(Scope: TScope; Item: TIDDeclaration); overload; static; inline;
    class procedure InsertToScope(Scope: TScope; const ID: string; Declaration: TIDDeclaration); overload; static; inline;
    procedure ParseUnitDecl(Scope: TScope);
    {функция парсит название модуля}
    function ParseUnitName(Scope: TScope; out ID: TIdentifier): TTokenID;
    function ParseUsesSection(Scope: TScope): TTokenID;
    // функция парсинга секции переменных/полей
    function ParseVarDefaultValue(Scope: TScope; DataType: TIDType; out DefaultValue: TIDExpression): TTokenID; virtual;
    function ParseVarStaticArrayDefaultValue(Scope: TScope; ArrType: TIDArray; out DefaultValue: TIDExpression): TTokenID; virtual;
    function ParseVarRecordDefaultValue(Scope: TScope; Struct: TIDStructure; out DefaultValue: TIDExpression): TTokenID;
    function ParseVarSection(Scope: TScope; Visibility: TVisibility; Struct: TIDStructure; IsWeak: Boolean = False; isRef: Boolean = False): TTokenID;
    function ParseVarInCaseRecord(Scope: TScope; Visibility: TVisibility; Struct: TIDStructure): TTokenID;
    //=======================================================================================================================
    ///  Парсинг типов
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
    function ParseTypeMember(Scope: TScope; Struct: TIDStructure): TTokenID;
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
    procedure ParseCondDefine(Scope: TScope; add_define: Boolean);
    function ParseCondInclude(Scope: TScope): TTokenID; virtual;
    //=======================================================================================================================
    class procedure AddSelfParameter(Params: TScope; Struct: TIDStructure; ClassMethod: Boolean); static; inline;
    class function AddResultParameter(Params: TScope): TIDVariable; static; inline;
    // statemets
    function ParseStatements(Scope: TScope; SContext: PSContext; IsBlock: Boolean): TTokenID; virtual;
    function ParsePlatform(Scope: TScope): TTokenID;
    function ParseAttribute(Scope: TScope): TTokenID;
    function ParseDeprecated(Scope: TScope; out &Deprecated: TIDExpression): TTokenID;
    function CheckAndParseDeprecated(Scope: TScope; CurrToken: TTokenID): TTokenID;
    function CheckAndParseAttribute(Scope: TScope): TTokenID;
    function CheckAndParseProcTypeCallConv(Scope: TScope; TypeDecl: TIDType): TTokenID;
    //=======================================================================================================================
    /// парсинг выражений
    function ParseExpression(Scope: TScope; var EContext: TEContext; SrartToken: TTokenID): TTokenID;
    function ParseClosureCapturedVarsDecl(Scope: TScope; out CapturedVars: TIDClosure.TCapturedVars): TTokenID;
    function ParseConstExpression(Scope: TScope; out Expr: TIDExpression; EPosition: TExpessionPosition): TTokenID; virtual;
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
    { функция парсинга вектор(кортеж) вид [a, b, ... z]}
    procedure ParseVector(Scope: TScope; var EContext: TEContext);
    { функция пост-оброботки откомпилированного модуля}
    procedure PostCompileProcessUnit;
    { функция пост-оброботки инициализации/финализации типа}
    function EmitCreateClosure(SContext: PSContext; Closure: TIDClosure): TIDExpression;
    {функция ищет заданный класс в секции interface, бросает ошибку если не нашла}
    function GetPublicClass(const Name: string): TIDClass;
    function GetPublicType(const Name: string): TIDType;
    // специализация (инстанцирование) обобщенной процедуры
    function SpecializeGenericProc(CallExpr: TIDCallExpression; const CallArgs: TIDExpressions): TIDProcedure;
    // специализация (инстанцирование) обобщенного типа
    function SpecializeGenericType(GenericType: TIDType; const ID: TIdentifier; const SpecializeArgs: TIDExpressions): TIDType;

    class function CreateRangeType(Scope: TScope; LoBound, HiBound: Integer): TIDRangeType; static; inline;

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
    property Options: TCompilerOptions read FOptions;
    property Source: string read GetSource;
    property TypeSpace: TTypeSpace read FTypeSpace;
    property VarSpace: TVarSpace read FVarSpace;
    property ProcSpace: TProcSpace read FProcSpace;
  end;

  function ScopeToVarList(Scope: TScope; SkipFirstCount: Integer): TVariableList;

implementation

{ TCompiler }

uses SystemUnit, NPCompiler.Messages, NPCompiler.ConstCalculator;

procedure StopCompile(CompileSuccess: Boolean);
begin
  raise ECompilerStop.Create(CompileSuccess);
end;

function GetILLast(const Context: PSContext): TILInstruction;
begin
//  if Assigned(Context) and Assigned(Context.IL) then
//    Result := Context.IL.Last
//  else
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

class procedure TNPUnit.ERROR_INVALID_EXPLICIT_TYPECAST(const Src: TIDExpression; Dst: TIDType);
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
  Result := nil;
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
//var
//  VItem: TIDVariable;
//  LCode: TILInstruction;
begin
//  VItem := Proc.VarSpace.First;
//  while Assigned(VItem) do
//  begin
//    LCode := TILInstruction(VItem.LastRWInstruction);
//    if Assigned(LCode) and (LCode.Position <= CurInstruction.Position) then
//    begin
//      if (VItem.DataType.ActualDataType = Src.DataType.ActualDataType) and
//         (VItem.Reference = Src.Reference) and
//         (VItem.IsTemporary) and
//         (VItem <> Src) then
//      begin
//        if not (VarLoopIndex in VItem.Flags) or (LCode.CFBlock <> CurInstruction.CFBlock) then
//          Exit(VItem);
//      end;
//    end;
//    VItem := TIDVariable(VItem.NextItem);
//  end;
  Result := nil;
end;

procedure ReplaceVar(const Proc: TIDProcedure; const DelVar, ReplVar: TIDVariable);
//var
//  Code: TILInstruction;
//  BreakEnum: Boolean;
//  VItem: TIDVariable;
//  RCPathCount: UInt32;
begin
//  RCPathCount := 1;
//  BreakEnum := False;
//  Proc.VarSpace.Delete(DelVar);
//  Code := TIL(Proc.IL).First;
//  while Assigned(Code) do begin
//    Code.EnumerateArgs(
//      procedure(const Arg: TIDExpression; var BreakEnum: Boolean)
//      begin
//        if Arg.Declaration = DelVar then
//          Arg.Declaration := ReplVar;
//      end,
//      BreakEnum);
//    Code.IncReferences(RCPathCount);
//    Code := Code.Next;
//  end;
//  VItem := Proc.VarSpace.First;
//  while Assigned(VItem) do begin
//    if VItem.Absolute = DelVar then
//      VItem.Absolute := ReplVar;
//    VItem := TIDVariable(VItem.NextItem);
//  end;
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
  Result := nil;
end;

class function TNPUnit.MatchArrayImplicitToRecord(Source: TIDExpression; Destination: TIDStructure): TIDExpression;
begin
  Assert(False);
  Result := nil;
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
//  function WriteImplicitCast(Implicit: TIDDeclaration; Src: TIDExpression): TIDExpression;
//  var
//    PCall: TIDCallExpression;
//  begin
//    if Implicit.ItemType = itType then
//    begin
//      Result := GetTMPVarExpr(SContext, TIDType(Implicit));
//      ILWrite(SContext, TIL.IL_Move(Result, Src));
//    end else
//    if Implicit.ItemType = itProcedure then
//    begin
//      Result := GetTMPVarExpr(SContext, Implicit.DataType);
//      PCall := TIDCallExpression.Create(Implicit);
//      PCall.TextPosition := Src.TextPosition;
//      PCall.ArgumentsCount := 2;
//      PCall.Instance := GetOperatorInstance(Op, Result, Src);
//      Result := Process_CALL_direct(SContext, PCall, TIDExpressions.Create(Result, Src));
//    end else begin
//      ERROR_FEATURE_NOT_SUPPORTED;
//      Result := nil;
//    end;
//  end;
//var
//  LeftDT, RightDT: TIDType;
//  Operators: TIDPairList;
//  LeftImplicit, RightImplicit, LeftBinarOp, RightBinarOp: TIDDeclaration;
//  LeftImplicitFactor, RightImplicitFactor: Integer;
begin
  Assert(False);
  Result := nil;
//  LeftDT := Left.DataType.ActualDataType;
//  RightDT := Right.DataType.ActualDataType;
//
//  Operators := LeftDT.BinarOperators[Op];
//  if Assigned(Operators) then
//    LeftImplicit := FindImplicitFormBinarOperators(Operators, RightDT, LeftImplicitFactor, LeftBinarOp)
//  else
//    LeftImplicit := nil;
//
//  Operators := RightDT.BinarOperators[Op];
//  if Assigned(Operators) then
//    RightImplicit := FindImplicitFormBinarOperators(Operators, LeftDT, RightImplicitFactor, RightBinarOp)
//  else
//    RightImplicit := nil;
//
//  if not Assigned(LeftImplicit) and not Assigned(RightImplicit) then
//    ERROR_NO_OVERLOAD_OPERATOR_FOR_TYPES(Op, Left, Right);
//
//  if LeftImplicitFactor >= RightImplicitFactor then
//  begin
//    Result := LeftBinarOp;
//    Right := WriteImplicitCast(LeftImplicit, Right);
//  end else begin
//    Result := RightBinarOp;
//    Left := WriteImplicitCast(RightImplicit, Left);
//  end;
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
  Source := Source.ActualDataType;
  Destination := Destination.ActualDataType;

  if Source = Destination then
    Exit(Destination);

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
  Result := nil;
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
  Result := nil;
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
//var
//  SrcDataType: TIDType;
//  DstDataType: TIDType;
//  ExplicitIntOp: TIDOperator;
begin
  Assert(False);
  Result := nil;
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
//  TIL(ProcDecl.IL).EnumerateArgs(
//    procedure (const Arg: TIDExpression; var BreakEnum: Boolean)
//    var
//      LVar: TIDVariable;
//    begin
//      {если это локальная переменная}
//      if Arg.IsLocalVar then
//      begin
//        LVar := Arg.AsVariable;
//        if LVar.Scope <> ProcDecl.EntryScope then
//        begin
//          if not Assigned(Closure) then
//          begin
//            Closure := TIDClosure.CreateClosure(SContext.Proc, ProcDecl);
//            Closure.Ancestor := SYSUnit._TObject;
//          end;
//          {замена декларации локальной переменной на поле замыкания}
//          Arg.Declaration := Closure.GetCapturedVar(LVar);
//        end;
//      end;
//    end);
//
//  if Assigned(Closure) then
//    AddType(Closure);
//
  Result := Closure;
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

{функция проверяет равна ли NIL-у managed переменная}
function CheckManagedNeedDecRef(const Instruction: TILInstruction): Boolean;
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
  if (VarConst in Flags) and (Expression.DataType <> SYSUnit._UntypedReference) then
  begin
    ERROR_CANNOT_MODIFY_CONSTANT(Expression);
  end;
  if VarLoopIndex in Flags then
    ERROR_CANNOT_MODIFY_FOR_LOOP_VARIABLE(Expression);
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
  Result := TCompilerResult.CompileFail;
end;

function TNPUnit.CompileIntfOnly: TCompilerResult;
begin
  Result := TCompilerResult.CompileFail;
end;

constructor TNPUnit.Create(const Project: IASTProject; const FileName: string; const Source: string = '');
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
  //FBENodesPool := TBENodesPool.Create(16);
  if Assigned(SYSUnit) then
  begin
    FTypeSpace.Initialize(SYSUnit.SystemTypesCount);
    // добовляем system в uses
    FIntfImportedUnits.AddObject('system', SYSUnit);
  end;
  FOptions := TCompilerOptions.Create(Package.Options);

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
  CItem := nil;
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
//  Result := TIDProcedure.CreateAsSystemMethod(Struct, '$init');
//  Result.Struct := Struct;
//  Result.IL := TIL.Create(Result);
//  Struct.InitProc := Result;
//  Struct.AddMethod(Result);
  Result := nil;
end;

class function TNPUnit.CreateStructCopyProc(const Struct: TIDStructure): TIDProcedure;
begin
//  Result := TIDProcedure.CreateAsSystemMethod(Struct, '$copy');
//  Result.Struct := Struct;
//  Result.IL := TIL.Create(Result);
//  Struct.CopyProc := Result;
//  Struct.AddMethod(Result);
  Result := nil;
end;

function TNPUnit.CreateStructFinalProc(const Struct: TIDStructure): TIDProcedure;
begin
//  Result := TIDProcedure.CreateAsSystemMethod(Struct, '$final');
//  Result.Struct := Struct;
//  Result.IL := TIL.Create(Result);
//  Struct.FinalProc := Result;
//  Struct.AddMethod(Result);
  Result := nil;
end;

function TNPUnit.CreateSysProc(const SysName: string): TIDProcedure;
begin
//  Result := TIDProcedure.CreateAsSystem(FImplScope, SysName);
//  Result.IL := TIL.Create(Result);
//  FProcSpace.Add(Result);
  Result := nil;
end;

function TNPUnit.CreateArraySysProc(const Arr: TIDArray; const Name: string; out ProcParam: TIDVariable): TIDProcedure;
begin
//  Result := TIDProcedure.CreateAsSystem(FImplScope, Name);
//  Result.IL := TIL.Create(Result);
//  ProcParam := TIDVariable.CreateAsSystem(Result.ParamsScope, 'Arr');
//  ProcParam.DataType := Arr;
//  {статические массивы передает по ссылке}
//  if Arr.DataTypeID = dtStaticArray then
//    ProcParam.Flags := [VarInOut, VarParameter]
//  else
//    ProcParam.Flags := [VarIn, VarConst, VarParameter];
//  Result.AddParam(ProcParam);
//  FProcSpace.Add(Result);
  Result := nil;
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
  Result := nil;
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

function TNPUnit.ProcessMemberExpression(SContext: PSContext; WasProperty: Boolean; var PMContext: TPMContext): TIDExpression;
//var
//  Decl: TIDDeclaration;
begin
  Assert(False);
  Result := nil;
//  if not WasProperty and (PMContext.Items[PMContext.Count - 2].DataTypeID <> dtSet) then
//  begin
//    Decl := PMContext.Last.Declaration;
//    if Decl.ItemType <> itProcedure then
//      Result := GetTMPRefExpr(SContext, PMContext.DataType)
//    else
//      Result := GetTMPVarExpr(SContext, PMContext.DataType);
//    Result.TextPosition := PMContext.ID.TextPosition;
//    ILWrite(SContext, TIL.IL_GetPtr(Result, PMContext.Items));
//    ReleaseExpressions(SContext, PMContext.Items);
//  end else begin
//    // если было свойтво(оно не может быть развернуто в GETPTR немедлено),
//    // то обработка откладывается на более поздний срок
//    Result := TIDMultiExpression.Create(PMContext.Items, PMContext.ID.TextPosition);
//    TIDMultiExpression(Result).EffectiveDataType := PMContext.DataType;
//  end;
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
  Result := token_unknown;
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
  Result := token_unknown;
end;

procedure TNPUnit.CheckLeftOperand(const Status: TRPNStatus);
begin
  if Status <> rpOperand then
    ERROR_EXPRESSION_EXPECTED;
end;

function TNPUnit.ParseExpression(Scope: TScope; var EContext: TEContext; SrartToken: TTokenID): TTokenID;
begin
  Assert(False);
  Result := token_unknown;
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
begin
  Result := token_unknown;
end;

function TNPUnit.ParseAsmSpecifier(out Platform: TIDPlatform): TTokenID;
begin
  Assert(False);
  Result := token_unknown;
end;

procedure TNPUnit.ParseASMStatement(Scope: TScope; Platform: TIDPlatform; SContext: PSContext);
begin

end;

function TNPUnit.ParseStatements(Scope: TScope; SContext: PSContext; IsBlock: Boolean): TTokenID;
begin
  Assert(False);
  Result := token_unknown;
end;

function TNPUnit.ParseGenericsArgs(Scope: TScope; SContext: PSContext; out Args: TIDExpressions): TTokenID;
begin
  Assert(False);
  Result := token_unknown;
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

procedure TNPUnit.PostCompileProcessUnit;
begin

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



function TNPUnit.ParseProcBody(Proc: TIDProcedure; Platform: TIDPlatform): TTokenID;
begin
  Assert(False);
  Result := token_unknown;
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
begin
  Assert(False);
  Result := token_unknown;
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
begin
  Assert(False);
  Result := token_unknown;
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

//function TNPUnit.CompileTypeDecl(Section: TUnitSection; const Source: string; out Decl: TIDType): ICompilerMessages;
//var
//  ParserState: TParserPosition;
//  ParserSource: string;
//  Scope: TScope;
//begin
//  Result := FMessages;
//  FParser.SaveState(ParserState);
//  ParserSource := FParser.Source;
//  try
//    case Section of
//      usInterface: Scope := FIntfScope;
//      usImplementation: Scope := FImplScope;
//    else
//      Scope := nil;
//    end;
//    try
//      ParseNamedTypeDecl(Scope);
//      Decl := FTypeSpace.Last;
//    except
//      on e: ECompilerAbort do PutMessage(ECompilerAbort(e).CompilerMessage^);
//      on e: Exception do PutMessage(cmtInteranlError, e.Message);
//    end;
//  finally
//    FParser.Source := ParserSource;
//    FParser.LoadState(ParserState);
//  end;
//end;

//function TNPUnit.CompileProcDecl(Section: TUnitSection; const Source: string; out Proc: TIDProcedure): ICompilerMessages;
//var
//  ParserState: TParserPosition;
//  ParserSource: string;
//  Token: TTokenID;
//  Scope: TScope;
//begin
//  Result := FMessages;
//  FParser.SaveState(ParserState);
//  ParserSource := FParser.Source;
//  try
//    try
//      case Section of
//        usInterface: Scope := FIntfScope;
//        usImplementation: Scope := FImplScope;
//      else
//        Scope := nil;
//      end;
//      FParser.Source := Source;
//      FParser.First;
//      Token := parser_NextToken(Scope);
//      case Token of
//        token_function: begin
//          CheckIntfSectionMissing(Scope);
//          ParseProcedure(Scope, ptFunc);
//          Proc := Scope.ProcSpace.Last;
//        end;
//        token_procedure: begin
//          CheckIntfSectionMissing(Scope);
//          ParseProcedure(Scope, ptProc);
//          Proc := Scope.ProcSpace.Last;
//        end;
//      else
//        ERROR_KEYWORD_EXPECTED;
//      end;
//    except
//      on e: ECompilerAbort do PutMessage(ECompilerAbort(e).CompilerMessage^);
//      on e: Exception do PutMessage(cmtInteranlError, e.Message);
//    end;
//  finally
//    FParser.Source := ParserSource;
//    FParser.LoadState(ParserState);
//  end;
//end;

function TNPUnit.ParseVarStaticArrayDefaultValue(Scope: TScope; ArrType: TIDArray; out DefaultValue: TIDExpression): TTokenID;
begin
  Assert(False);
  Result := token_unknown;
end;

function TNPUnit.ParseVarDefaultValue(Scope: TScope; DataType: TIDType; out DefaultValue: TIDExpression): TTokenID;
//var
//  SContext: PSContext;
//  EContext: TEContext;
begin
  Assert(False);
  Result := token_unknown;
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
  while True do begin
    VarFlags := [];
    Result := CheckAndParseAttribute(Scope);
    parser_MatchIdentifier(Result);
    Names.Add;
    parser_ReadCurrIdentifier(Names.Items[c]);
    Result := parser_NextToken(Scope);
    if Result = token_Coma then begin
      Inc(c);
      parser_NextToken(Scope);
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
//      if Assigned(DefaultValue) and DefaultValue.IsTMPVar then
//      begin
//        ILWrite(@fInitProcSConect, TIL.IL_Move(TIDExpression.Create(Field), DefaultValue));
//      end else
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
      token_case: Result := ParseCaseRecord(Decl.Members, Decl);
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
        parser_NextToken(Scope);
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

  parser_MatchToken(Result, token_end);
  Result := parser_NextToken(Scope);
  if Result = token_external then
    Result := ParseImportStatement(Scope, Decl);

  if Result = token_platform then
    Result := parser_NextToken(Scope);

  Result := CheckAndParseDeprecated(Scope, Result);
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
        parser_NextToken(Scope);
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
  Result := parser_NextToken(Scope);
  if Result = token_external then
    Result := ParseImportStatement(Scope, Decl);
end;

function TNPUnit.ParseIntfGUID(Scope: TScope; Decl: TIDInterface): TTokenID;
var
  UID: TIDExpression;
  GUID: TGUID;
begin
  parser_NextToken(Scope);
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
      parser_NextToken(Scope);
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
  Result := token_unknown;
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

function TNPUnit.ProcessBuiltin_Copy(var EContext: TEContext): TIDExpression;
//var
//  Arr, From, Cnt, Len, NCnt: TIDExpression;
//  TmpVar: TIDVariable;
//  Code: TILInstruction;
begin
  Assert(False);
//  // читаем третий аргумент
//  Cnt := RPNPopExpression(EContext);
//  // читаем второй аргумент
//  From := RPNPopExpression(EContext);
//  // читаем первый аргумент
//  Arr := RPNPopExpression(EContext);
//
//  CheckEmptyExpression(Arr);
//  CheckEmptyExpression(From);
//  CheckEmptyExpression(Cnt);
//
//  CheckArrayExpression(Arr);
//
//  TmpVar := GetTMPVar(EContext, Arr.DataType);
//  TmpVar.IncludeFlags([VarTmpResOwner]);
//  Result := TIDExpression.Create(TmpVar, parser_PrevPosition);
//
//  // если Cnt = -1 (значение по умолчанию)
//  if Cnt.IsConstant and (Cnt.AsIntConst.Value = -1) then
//  begin
//    if Arr.IsConstant then
//    begin
//      case Arr.DataTypeID of
//        dtString, dtAnsiString: Cnt := IntConstExpression(Arr.AsStrConst.StrLength);
//        dtDynArray: Cnt := IntConstExpression(Arr.AsDynArrayConst.ArrayLength);
//      else
//        ERROR_FEATURE_NOT_SUPPORTED;
//      end;
//    end else begin
//      Cnt := GetTMPVarExpr(EContext, SYSUnit._Int32, parser_PrevPosition);
//      Code := TIL.IL_Length(Cnt, Arr);
//      ILWrite(EContext, Code);
//    end;
//  end;
//
//  if not Arr.IsConstant then
//  begin
//    Len := GetTMPVarExpr(EContext, SYSUnit._Int32, parser_PrevPosition);
//    ILWrite(EContext, TIL.IL_Length(Len, Arr))
//  end else
//    Len := IntConstExpression(Arr.AsStrConst.StrLength);
//
//  // проверка From на значение < 0
//  if not From.IsConstant then
//  begin
//    ILWrite(EContext, TIL.IL_Cmp(From, SYSUnit._ZeroExpression));
//    ILWrite(EContext, TIL.IL_Move(cLess, From, SYSUnit._ZeroExpression));
//
//    ILWrite(EContext, TIL.IL_Cmp(From, Len));
//    ILWrite(EContext, TIL.IL_Move(cGreater, From, Len));
//  end;
//  // проверка Cnt на значение < 0
//  if not Cnt.IsConstant then
//  begin
//    ILWrite(EContext, TIL.IL_Cmp(Cnt, SYSUnit._ZeroExpression));
//    ILWrite(EContext, TIL.IL_Move(cLess, Cnt, SYSUnit._ZeroExpression));
//  end;
//
//  if (not From.IsConstant) or (From.AsIntConst.Value > 0) then
//  begin
//    NCnt := GetTMPVarExpr(EContext, SYSUnit._Int32, parser_PrevPosition);
//    ILWrite(EContext, TIL.IL_Sub(NCnt, Len, From));
//    ILWrite(EContext, TIL.IL_Cmp(NCnt, Cnt));
//    ILWrite(EContext, TIL.IL_Move(cGreater, NCnt, Cnt)); {}
//  end else
//    NCnt := Cnt;
//
//  Code := TIL.IL_DAlloc(Result, NCnt);
//  ILWrite(EContext, Code);
//
//  Code := TIL.IL_Copy(Result, Arr, From, NCnt);
//  ILWrite(EContext, Code);
//
//  {освобожадем временные переменные}
//  ReleaseExpression(EContext, Arr);
//  ReleaseExpression(EContext, From);
//  ReleaseExpression(EContext, Cnt);
  Result := nil;
end;

function TNPUnit.ProcessBuiltin_MemSet(var EContext: TEContext): TIDExpression;
//var
//  Value, BytePattern: TIDExpression;
begin
  Assert(False);
//  // читаем второй аргумент
//  BytePattern := RPNPopExpression(EContext);
//  CheckEmptyExpression(BytePattern);
//
//  // читаем первый аргумент
//  Value := RPNPopExpression(EContext);
//  CheckEmptyExpression(Value);
//
//  ILWrite(EContext, TIL.IL_MemSet(Value, BytePattern));
//  ReleaseExpression(BytePattern);
//  ReleaseExpression(Value);
  Result := nil;
end;

function TNPUnit.ProcessBuiltin_Move(var EContext: TEContext): TIDExpression;
//var
//  SrcArr, SrcIdx, DstArr, DstIdx, Cnt: TIDExpression;
//  Code: TILInstruction;
begin
  Assert(False);
//  Result := nil;
//  // читаем пятый аргумент
//  Cnt := RPNPopExpression(EContext);
//  // читаем четвертый аргумент
//  DstIdx := RPNPopExpression(EContext);
//  // читаем третий аргумент
//  DstArr := RPNPopExpression(EContext);
//  // читаем второй аргумент
//  SrcIdx := RPNPopExpression(EContext);
//  // читаем первый аргумент
//  SrcArr := RPNPopExpression(EContext);
//
//  CheckArrayExpression(SrcArr);
//  CheckArrayExpression(DstArr);
//
//  Code := TIL.IL_MoveArray(SrcArr, SrcIdx, DstArr, DstIdx, Cnt);
//  ILWrite(EContext, Code);
//
//  CheckEmptyExpression(SrcArr);
//  CheckEmptyExpression(SrcIdx);
//  CheckEmptyExpression(DstArr);
//  CheckEmptyExpression(DstIdx);
//  CheckEmptyExpression(Cnt);
  Result := nil;
end;

function TNPUnit.ProcessBuiltin_SetLength(var EContext: TEContext): TIDExpression;
//var
//  ArrExpr, LenExpr: TIDExpression;
//  DataType: TIDType;
//  Instruction: TILInstruction;
//  //NewArr: TIDExpression;
begin
  Assert(False);
//  // читаем второй аргумент (NewLength)
//  LenExpr := RPNPopExpression(EContext);
//  // читаем первый аргумент (массив/строка)
//  ArrExpr := RPNPopExpression(EContext);
//  DataType := ArrExpr.DataType;
//  if DataType.DataTypeID in [dtDynArray, dtString, dtAnsiString] then begin
//    CheckVarExpression(ArrExpr, vmpPassArgument);
//    CheckEmptyExpression(LenExpr);
//    {Dest := TIDVariable(ArrExpr.Declaration);
//    // если это первая инициализация массива
//    // то используем только выделение памяти, без реаллокации
//    if (Dest.ReadCount = 0) and (Dest.WriteCount = 0) then
//      Instruction := TIL.IL_DAlloc(Result, LenExpr)
//    else}
//
//    //NewArr := GetTMPVarExpr(SContext, DataType);
//    Instruction := TIL.IL_RAlloc(ArrExpr, LenExpr);
//
//    ILWrite(EContext, Instruction);
//    //CheckAndCallArrayInit(SContext.Proc, ArrExpr.AsVariable);
//  end else
//    AbortWork(sArrayOrStringTypeRequired, ArrExpr.TextPosition);
//
//  ReleaseExpression(EContext, LenExpr);
  Result := nil;
end;

function TNPUnit.ProcessBuiltin_Assert(var EContext: TEContext; const ParamsTest: string; SourceRow: Integer): TIDExpression;
//var
//  AssertExpr, ETextExpr: TIDExpression;
//  Instr: TILInstruction;
//  Decl: TIDDeclaration;
//  AssertText: string;
//  LastInst: TILInstruction;
//  Condition: TILCondition;
//  Args: TIDExpressions;
begin
  Assert(False);
//  // читаем второй аргумент (текст ошибки)
//  ETextExpr := RPNPopExpression(EContext);
//  // читаем первый аргумент (утверждение)
//  AssertExpr := RPNPopExpression(EContext);
//  // выражение длоджно быть булевым
//  CheckBooleanExpression(AssertExpr);
//
//  if ETextExpr.IsConstant and (ETextExpr.DataTypeID = dtString) and (ETextExpr.AsStrConst.Value = '') then
//    AssertText := format('Assertion at line %d: condition "%s" is false', [SourceRow, ParamsTest])
//  else
//    AssertText := format('Assertion at line %d: %s', [SourceRow, ETextExpr.AsStrConst.Value]);
//
//  Decl := TIDStringConstant.Create(nil, Identifier(''), SYSUnit._String, AssertText);
//  Decl.Index := FPackage.GetStringConstant(AssertText);
//  ETextExpr := TIDExpression.Create(Decl, AssertExpr.TextPosition);
//
//  LastInst := EContext.SContext.ILLast;
//  if AssertExpr.IsConstant then
//  begin
//    {если выражение - константа}
//    if AssertExpr.AsBoolConst.Value = True then
//      Exit(nil);
//    Condition := cNone;
//  end else
//  if (AssertExpr.IsAnonymous) and Assigned(LastInst) and (LastInst.ILCode = icSetBool) then
//  begin
//    {если выражение - результат сравнения}
//    Condition := InverseCondition(LastInst.Condition);
//    // удаляем инструкцию SetBool
//    EContext.SContext.IL.Delete(LastInst);
//  end else begin
//    {если выражение - результат переменная}
//    Instr := TIL.IL_Test(AssertExpr, AssertExpr);
//    ILWrite(EContext, Instr);
//    Condition := cZero;
//  end;
//  Args := [ETextExpr];
//  Instr := TIL.IL_ProcCall(TIDCallExpression.Create(SYSUnit._AssertProc), nil, nil, Args);
//  Instr.Condition := Condition;
//  ILWrite(EContext, Instr);
//
//  ReleaseExpression(EContext, AssertExpr);
//  ReleaseExpression(EContext, ETextExpr);
//
  Result := nil;
end;



function TNPUnit.ProcessBuiltin_Include(var EContext: TEContext): TIDExpression;
//var
//  ASet, ASubSet, SValue: TIDExpression;
//  SetDT: TIDSet;
begin
  Assert(False);
//  // читаем аргумент SubSet
//  ASubSet := RPNPopExpression(EContext);
//  // читаем аргумент Set
//  ASet := RPNPopExpression(EContext);
//  CheckSetType(ASet);
//  SetDT := TIDSet(ASet.DataType.ActualDataType);
//  case ASubSet.DataTypeID of
//    dtSet: begin
//      if ASubSet.DataType.ActualDataType <> SetDT then
//        ERROR_INCOMPATIBLE_TYPES(ASubSet, SetDT.BaseType);
//      ILWrite(EContext, TIL.IL_Or(ASet, ASet, ASubSet));
//    end;
//    dtEnum: begin
//      if ASubSet.DataType.ActualDataType <> SetDT.BaseType.ActualDataType then
//        ERROR_INCOMPATIBLE_TYPES(ASubSet, SetDT.BaseType);
//      if ASubSet.IsConstant then
//      begin
//        SValue := IntConstExpression(1 shl ASubSet.AsIntConst.Value);
//      end else
//        SValue := nil;
//      ILWrite(EContext, TIL.IL_Or(ASet, ASet, SValue));
//    end;
//    dtDynArray: begin
//      SValue := ConstDynArrayToSet(ASubSet, SetDT);
//      ILWrite(EContext, TIL.IL_Or(ASet, ASet, SValue));
//    end;
//  else
//    ERROR_ORDINAL_OR_SET_REQUIRED(ASubSet);
//  end;
  Result := nil;
end;

function TNPUnit.ProcessBuiltin_Exclude(var EContext: TEContext): TIDExpression;
//var
//  ASet, ASubSet, SValue: TIDExpression;
//  SetDT: TIDSet;
begin
  Assert(False);
//  // читаем аргумент SubSet
//  ASubSet := RPNPopExpression(EContext);
//  // читаем аргумент Set
//  ASet := RPNPopExpression(EContext);
//  CheckSetType(ASet);
//  SetDT := TIDSet(ASet.DataType.ActualDataType);
//  case ASubSet.DataTypeID of
//    dtSet: begin
//      if ASubSet.DataType.ActualDataType <> SetDT then
//        ERROR_INCOMPATIBLE_TYPES(ASubSet, SetDT.BaseType);
//      SValue := GetTMPVarExpr(EContext, SetDT);
//      ILWrite(EContext, TIL.IL_Not(SValue, ASubSet));
//      ILWrite(EContext, TIL.IL_And(ASet, ASet, SValue));
//    end;
//    dtEnum: begin
//      if ASubSet.DataType.ActualDataType <> SetDT.BaseType.ActualDataType then
//        ERROR_INCOMPATIBLE_TYPES(ASubSet, SetDT.BaseType);
//      if ASubSet.IsConstant then
//      begin
//        SValue := IntConstExpression(not (1 shl ASubSet.AsIntConst.Value));
//      end else
//        SValue := nil;  // todo
//      ILWrite(EContext, TIL.IL_And(ASet, ASet, SValue));
//    end;
//    dtDynArray: begin
//      SValue := ConstDynArrayToSet(ASubSet, SetDT);
//      SValue.AsIntConst.Value := not SValue.AsIntConst.Value;
//      ILWrite(EContext, TIL.IL_And(ASet, ASet, SValue));
//    end;
//  else
//    ERROR_ORDINAL_OR_SET_REQUIRED(ASubSet);
//  end;
  Result := nil;
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

function TNPUnit.Process_CALL_direct(SContext: PSContext; PExpr: TIDCallExpression; CallArguments: TIDExpressions): TIDExpression;
//  function GenConstToVar(Constant: TIDExpression; VarDatatType: TIDType): TIDExpression;
//  var
//    Instruction: TILInstruction;
//  begin
//    Result := TIDExpression.Create(GetTMPVar(SContext, VarDatatType));
//    Instruction := TIL.IL_Move(Result, Constant);
//    ILWrite(SContext, Instruction);
//  end;
//
//var
//  AIndex, ArgsCount: Integer;
//  ProcDecl: TIDProcedure;
//  ProcParams: TVariableList;
//  ResVar: TIDVariable;
//  ProcResult: TIDType;
//  Param: TIDVariable;
//  AExpr: TIDExpression;
//  CallInstruction: TILProcCall;
begin
  Assert(False);
  Result := nil;
//  ArgsCount := Length(CallArguments);
//
//  ProcDecl := PExpr.AsProcedure;
//  ProcParams := ProcDecl.ExplicitParams;
//
//  if Assigned(ProcDecl.GenericDescriptor) then
//  begin
//    ProcDecl := SpecializeGenericProc(PExpr, CallArguments);
//    ProcParams := ProcDecl.ExplicitParams;
//    PExpr.Declaration := ProcDecl;
//  end;
//
//  {если все аргументы явно указаны}
//  for AIndex := 0 to ArgsCount - 1 do
//  begin
//    Param := ProcParams[AIndex];
//    AExpr := CallArguments[AIndex];
//
//    {если аргумент константый дин. массив то делаем доп. проверку}
//    if AExpr.Declaration is TIDDynArrayConstant then
//      AExpr := CheckConstDynArray(SContext, Param.DataType, AExpr);
//
//    {проверка диаппазона для константных аргументов}
//    if AExpr.IsConstant then
//      CheckConstValueOverflow(AExpr, Param.DataType);
//
//    {подбираем и если надо вызываем implicit оператор}
//    AExpr := MatchImplicit3(SContext, AExpr, Param.DataType);
//
//    {если параметр - constref и аргумент - константа, то создаем временную переменную}
//    if (VarConstRef in Param.Flags) and (AExpr.ItemType = itConst) then
//      AExpr := GenConstToVar(AExpr, Param.DataType);
//
//    CallArguments[AIndex] := AExpr;
//
//    if Param.Reference then
//      CheckVarExpression(AExpr, vmpPassArgument);
//  end;
//
//  {результат функции}
//  ProcResult := ProcDecl.ResultType;
//  if Assigned(ProcResult) then begin
//    ResVar := GetTMPVar(SContext, ProcResult);
//    ResVar.IncludeFlags([VarTmpResOwner]);
//    Result := TIDExpression.Create(ResVar, PExpr.TextPosition);
//  end else
//    Result := nil;
//
//  {INLINE подстановка (пока только если инлайн процедура готова)}
//  if (pfInline in ProcDecl.Flags) and
//     (ProcDecl.IsCompleted) and
//     (ProcDecl <> SContext.Proc) then
//  begin
//    SContext.IL.InlineProc(SContext.Proc, ProcDecl, nil, PExpr.Instance, Result, CallArguments);
//  end else begin
//    if Assigned(PExpr.Instance) and (ProcDecl.Struct <> PExpr.Instance.DataType) then
//      CallInstruction := TIL.IL_InheritedCall(PExpr, Result, PExpr.Instance, CallArguments)
//    else
//      CallInstruction := TIL.IL_ProcCall(PExpr, Result, PExpr.Instance, CallArguments);
//    ILWrite(SContext, CallInstruction);
//  end;
//  ProcDecl.IncRefCount(1);
end;

function TNPUnit.GetMessagesText: string;
begin
  Result := FMessages.GetAsString;
end;

function TNPUnit.GetModuleName: string;
begin

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

function TNPUnit.GetWeakRefType(Scope: TScope; SourceDataType: TIDType): TIDWeekRef;
begin
   if not (SourceDataType.DataTypeID in [dtClass, dtInterface]) then
     ERROR_WEAKREF_CANNOT_BE_DECLARED_FOR_TYPE(SourceDataType);

   Result := SourceDataType.WeakRefType;
   if not Assigned(Result) then
   begin
     Result := TIDWeekRef.CreateAsAnonymous(Scope, SourceDataType);
     Result.OverloadImplicitTo(SourceDataType);
     Result.OverloadImplicitFrom(SourceDataType);
     SourceDataType.WeakRefType := Result;
     AddType(Result);
   end;
end;

{function TNPUnit.GetILText: string;
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
end; }

procedure TNPUnit.InitEContext(var EContext: TEContext; SContext: PSContext; EPosition: TExpessionPosition);
begin
  Assert(False);
//  EContext.Initialize(Process_operators);
//  EContext.SContext := SContext;
//  if Assigned(SContext) then
//    EContext.LastInstruction := SContext.ILLast;
//  EContext.EPosition := EPosition;
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

procedure TNPUnit.SaveConstsToStream(Stream: TStream);
//var
//  i, ac, ec: Integer;
//  ArrayDT: TIDArray;
//  Decl: TIDConstant;
begin
  Assert(False);
//  ac := FConsts.Count;
//  Stream.WriteStretchUInt(ac);
//  if ac = 0 then
//    Exit;
//
//  Decl := FConsts.First;
//  while Assigned(Decl) do
//  begin
//    // сохраняем тип константы
//    WriteDataTypeIndex(Stream, Decl.DataType);
//    case Decl.DataTypeID of
//      dtRecord: WriteConstToStream(Stream, Decl, Decl.DataType);
//      dtStaticArray, dtDynArray, dtOpenArray: begin
//        // кол-во элементов  (пока только одномерные массивы)
//        ec := Length(TIDDynArrayConstant(Decl).Value);
//        if Decl.DataTypeID = dtDynArray then
//          Stream.WriteStretchUInt(ec);
//        ArrayDT := TIDArray(Decl.DataType);
//        // запись элементов
//        for i := 0 to ec - 1 do
//          WriteConstToStream(Stream, TIDConstant(TIDDynArrayConstant(Decl).Value[i].Declaration), ArrayDT.ElementDataType);
//      end;
//    else
//      Decl.WriteToStream(Stream, Package);
//    end;
//    Decl := TIDConstant(Decl.NextItem);
//  end;
end;

procedure TNPUnit.SaveMethodBodies(Stream: TStream);
//var
//  TDecl: TIDType;
//  Cnt: Integer;
begin
  Assert(False);
//  Cnt := 0;
//  TDecl := FTypeSpace.First;
//  while Assigned(TDecl) do
//  begin
//    TDecl := TDecl.ActualDataType;
//    if (TDecl.DataTypeID in [dtRecord, dtClass]) and
//       (TIDStructure(TDecl).MethodCount > 0) and
//       (not Assigned(TDecl.GenericDescriptor)) and
//       (TDecl.ImportLib = 0) then Inc(Cnt);
//    TDecl := TDecl.NextItem as TIDType;
//  end;
//
//  // кол-во типов с методами
//  Stream.WriteStretchUInt(Cnt);
//
//  TDecl := FTypeSpace.First;
//  while Assigned(TDecl) do
//  begin
//    TDecl := TDecl.ActualDataType;
//    if (TDecl.DataTypeID in [dtRecord, dtClass]) and
//       (TIDStructure(TDecl).MethodCount > 0) and
//       (not Assigned(TDecl.GenericDescriptor)) and
//       (TDecl.ImportLib = 0) then
//    begin
//      // пишем индекс типа
//      Stream.WriteStretchUInt(TDecl.Index);
//      // пишем тела методов
//      TIDStructure(TDecl).SaveMethodBodiesToStream(Stream, FPackage);
//    end;
//    TDecl := TDecl.NextItem as TIDType;
//  end;
end;

procedure WritePostForwardTypes(const Package: INPPackage; const Types: TTypeSpace; Stream: TStream);
//var
//  Decl: TIDType;
//  cnt: Integer;
begin
  Assert(False);
//  cnt := 0;
//  Decl := Types.First;
//  while Assigned(Decl) do
//  begin
//    if (Decl.DataTypeID = dtPointer) and TIDPointer(Decl).NeedForward then
//      Inc(cnt);
//    Decl := TIDType(Decl.NextItem);
//  end;
//  Stream.WriteStretchUInt(cnt);
//
//  if cnt = 0 then
//    Exit;
//
//  Decl := Types.First;
//  while Assigned(Decl) do
//  begin
//    if (Decl.DataTypeID = dtPointer) and TIDPointer(Decl).NeedForward then
//    begin
//      Stream.WriteStretchUInt(Decl.Index);
//      Decl.SaveDeclToStream(Stream, Package);
//    end;
//    Decl := TIDType(Decl.NextItem);
//  end;
end;

procedure TNPUnit.SaveTypesToStream(Stream: TStream);
//var
//  TDecl: TIDType;
begin
  Assert(False);
//  if Self <> SYSUnit then
//  begin
//    Stream.WriteStretchUInt(FTypeSpace.Count);
//    // сначала сохраняем id всех типов
//    TDecl := FTypeSpace.First;
//    while Assigned(TDecl) do begin
//      Stream.WriteUInt8(UInt8(TDecl.DataTypeID));
//      TDecl := TIDType(TDecl.NextItem);
//    end;
//    // сохраняем содержиоме типов
//    TDecl := FTypeSpace.First;
//    while Assigned(TDecl) do begin
//      TDecl.SaveDeclToStream(Stream, FPackage);
//      TDecl := TIDType(TDecl.NextItem);
//    end;
//  end else begin
//    {из модуля system cохраняем только пользовательские типы}
//    Stream.WriteStretchUInt(FTypeSpace.Count - Ord(TDataTypeID.dtPointer) - 1);
//    // сначала сохраняем id всех типов
//    TDecl := FTypeSpace.First;
//    while Assigned(TDecl) do begin
//      if TDecl.Index > Ord(TDataTypeID.dtPointer) then
//        Stream.WriteUInt8(UInt8(TDecl.DataTypeID));
//      TDecl := TIDType(TDecl.NextItem);
//    end;
//    // сохраняем содержиоме типов
//    TDecl := FTypeSpace.First;
//    while Assigned(TDecl) do begin
//      if TDecl.Index > Ord(TDataTypeID.dtPointer) then
//        TDecl.SaveDeclToStream(Stream, FPackage);
//      TDecl := TIDType(TDecl.NextItem);
//    end;
//  end;
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
//var
//  VDecl: TIDVariable;
//  Idx: Integer;
//  Flags: Byte;
begin
  Assert(False);
//  Flags := 0;
//  {секция инициализации}
//  if TIL(FInitProc.IL).Count > 0 then
//    Flags := ILUNIT_HASINIT;
//
//  {секция финализации}
//  if TIL(FFinalProc.IL).Count > 0  then
//    Flags := Flags + ILUNIT_HASFINAL;
//
//  Stream.WriteStretchUInt(Flags);
//
//  {RTTI имя}
//  Idx := FPackage.GetStringConstant(Name);
//  Stream.WriteStretchUInt(Idx);
//
//  CheckAndDelGenericProcs(FProcSpace);
//
//  {типы}
//  SaveTypesToStream(Stream);
//
//  {константные массивы/структуры}
//  SaveConstsToStream(Stream);
//
//  {глобальные переменные}
//  Stream.WriteStretchUInt(FVarSpace.Count); // кол-во глобальных переменных
//  VDecl := FVarSpace.First;
//  while Assigned(VDecl) do begin
//    TIDVariable(VDecl).SaveToStream(Stream, FPackage);
//    VDecl := TIDVariable(VDecl.NextItem);
//  end;
//
//  {декларации процедуры}
//  SaveProcDecls(Stream, Package, addr(FProcSpace));
end;

procedure TNPUnit.SaveBodyToStream(Stream: TStream);
//var
//  i: Integer;
//  BP: TBreakPoint;
begin
  Assert(False);
//  {$IFDEF DEBUG_WRITE_LEBELS}DBG_WRITE_UNIT_BODY_LABEL(Stream, Self.Name);{$ENDIF}
//  {тела процедур}
//  SaveProcBodies(Stream, Package, @FProcSpace);
//
//  {тела методы объектов}
//  SaveMethodBodies(Stream);
//
//  {секция инициализации}
//  if TIL(FInitProc.IL).Count > 0 then
//    FInitProc.SaveBodyToStream(Stream, Package);
//
//  {секция финализации}
//  if TIL(FFinalProc.IL).Count > 0 then
//    FFInalProc.SaveBodyToStream(Stream, Package);
//
//  if FPackage.IncludeDebugInfo then
//  begin
//    Stream.WriteStretchUInt(FBreakPoints.Count);
//    for i := 0 to FBreakPoints.Count - 1 do
//    begin
//      BP := FBreakPoints[i];
//      Stream.WriteStretchUInt(BP.Line);
//    end;
//  end;
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
  Result := parser_CurTokenID;
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
  Field: TIDVariable;
  Names: TIdentifiersPool;
  VarFlags: TVariableFlags;
begin
  c := 0;
  Names := TIdentifiersPool.Create(2);
  while True do begin
    VarFlags := [];
    Result := CheckAndParseAttribute(Scope);
    parser_MatchIdentifier(Result);
    Names.Add;
    parser_ReadCurrIdentifier(Names.Items[c]);
    Result := parser_NextToken(Scope);
    if Result = token_Coma then begin
      Inc(c);
      parser_NextToken(Scope);
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
    parser_NextToken(Scope);
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

function TNPUnit.GetFirstConst: TASTDeclaration;
begin
  Result := nil;
end;

function TNPUnit.GetFirstFunc: TASTDeclaration;
begin
  Result := nil;
end;

function TNPUnit.GetFirstType: TASTDeclaration;
begin
  Result := nil;
end;

function TNPUnit.GetFirstVar: TASTDeclaration;
begin
  Result := nil;
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
