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

  TIDDeclarationList = TList<TIDDeclaration>;

  TCondIFValue = (condIFFalse, condIfTrue, condIFUnknown);

  TNPUnit = class(TASTModule)
  type
    TVarModifyPlace = (vmpAssignment, vmpPassArgument);
    TIdentifiersPool = TPool<TIdentifier>;
  private
    FID: Integer;                      // ID модуля в пакете
    FPackage: INPPackage;
    FParser: TDelphiLexer;

    FIntfScope: TScope;                // interface scope
    FImplScope: TScope;                // implementation scope
    FIntfImportedUnits: TUnitList;
    FImplImportedUnits: TUnitList;
    FMessages: ICompilerMessages;
    FVarSpace: TVarSpace;
    FProcSpace: TProcSpace;
    FTypeSpace: TTypeSpace;
    FDefines: TDefines;

    FConsts: TConstSpace;              // список нетривиальных констант (массивы, структуры)
    FCompiled: Boolean;

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
    class procedure ERROR_ARRAY_TYPE_REQUIRED(const ID: TIdentifier; const TextPosition: TTextPosition); static;
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
    class procedure ERROR_VAR_IS_NOT_INITIALIZED(const Variable: TIDExpression);
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
  protected
    FRCPathCount: UInt32;              // кол-во проходов increfcount/decrefcount для деклараций
    FInitProcExplicit: Boolean;        // определена ли явно секция init
    FFinalProcExplicit: Boolean;       // определена ли явно секция final
    FInitProc: TIDProcedure;
    FFinalProc: TIDProcedure;
    FUnitName: TIdentifier;
    FSystemExplicitUse: Boolean;
    fCondStack: TSimpleStack<Boolean>;

    property InitProc: TIDProcedure read FInitProc;
    property FinalProc: TIDProcedure read FFinalProc;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////
    function GetSource: string;
    procedure SetUnitName(const Name: string);
  public
    function GetModuleName: string; override;
    function GetFirstFunc: TASTDeclaration; override;
    function GetFirstVar: TASTDeclaration; override;
    function GetFirstType: TASTDeclaration; override;
    function GetFirstConst: TASTDeclaration; override;
    function CreateSysProc(const SysName: string): TIDProcedure;
    function CreateArraySysProc(const Arr: TIDArray; const Name: string; out ProcParam: TIDVariable): TIDProcedure;

    class function IsConstValueInRange(Value: TIDExpression; RangeExpr: TIDRangeConstant): Boolean; static;
    class function IsConstRangesIntersect(const Left, Right: TIDRangeConstant): Boolean; static;
    class function IsConstEqual(const Left, Right: TIDExpression): Boolean; static;
    //======================================================================================================================================
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
    function ParseConstSection(Scope: TScope): TTokenID;
    function ParseParameters(Scope: TScope; InMacro: Boolean = False): TTokenID;
    function ParseAsmSpecifier: TTokenID;
    function ParseAsmProc(Scope: TScope): TTokenID;
    //=======================================================================================================================
    /// условная компиляция
    function ParseCondStatements(Scope: TScope; Token: TTokenID): TTokenID; virtual;
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
    function ProcessMemberExpression(SContext: PSContext; WasProperty: Boolean; var PMContext: TPMContext): TIDExpression;
    //=======================================================================================================================
    function ParseGenericsHeader(Params: TScope; out Args: TIDTypeList): TTokenID;
    function ParseGenericsArgs(Scope: TScope; SContext: PSContext; out Args: TIDExpressions): TTokenID;
    function ParseProcedure(Scope: TScope; ProcType: TProcType; Struct: TIDStructure = nil): TTokenID; virtual;
    function GetWeakRefType(Scope: TScope; SourceDataType: TIDType): TIDWeekRef;
    function CheckAndMakeClosure(SContext: PSContext; const ProcDecl: TIDProcedure): TIDClosure;
    procedure SetProcGenericArgs(CallExpr: TIDCallExpression; Args: TIDExpressions);
    procedure ParseASMStatement(Scope: TScope; SContext: PSContext);
    { функция пост-оброботки откомпилированного модуля}
    procedure PostCompileProcessUnit;
    { функция пост-оброботки инициализации/финализации типа}
    {функция ищет заданный класс в секции interface, бросает ошибку если не нашла}
    function GetPublicClass(const Name: string): TIDClass;
    function GetPublicType(const Name: string): TIDType;

    class function CreateRangeType(Scope: TScope; LoBound, HiBound: Integer): TIDRangeType; static; inline;

    class function MatchOverloadProc(Item: TIDExpression; const CallArgs: TIDExpressions; CallArgsCount: Integer): TIDProcedure; static;
    class function MatchImplicit(Source, Destination: TIDType): TIDDeclaration; static; inline;
    class function MatchImplicitClassOf(Source: TIDExpression; Destination: TIDClassOf): TIDDeclaration; static;
    class function StrictMatchProcSingnatures(const SrcParams, DstParams: TVariableList; const SrcResultType, DstResultType: TIDType): Boolean;
    class function StrictMatchProc(const Src, Dst: TIDProcedure): Boolean;
    class function CreateStructInitProc(const Struct: TIDStructure): TIDProcedure;
    class function CreateStructCopyProc(const Struct: TIDStructure): TIDProcedure;
    function CreateStructFinalProc(const Struct: TIDStructure): TIDProcedure;

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

uses SystemUnit, NPCompiler.Messages, AST.Parser.Errors, NPCompiler.ConstCalculator;



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

class procedure TNPUnit.ERROR_ARRAY_TYPE_REQUIRED(const ID: TIdentifier; const TextPosition: TTextPosition);
begin
  AbortWork(sArrayTypeRequired, TextPosition);
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

function TNPUnit.MatchBinarOperatorWithImplicit(SContext: PSContext; Op: TOperatorID; var Left, Right: TIDexpression): TIDDeclaration;
begin
  Assert(False);
  Result := nil;
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
begin
  Assert(False);
  Result := nil;
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
begin
  Assert(False);
  Result := nil;
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

function TNPUnit.CheckAndMakeClosure(SContext: PSContext; const ProcDecl: TIDProcedure): TIDClosure;
var
  Closure: TIDClosure;
begin
  Closure := nil;
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
  Result := nil;
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
begin
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

    if DeclType is TIDArray then      
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



function TNPUnit.ProcessMemberExpression(SContext: PSContext; WasProperty: Boolean; var PMContext: TPMContext): TIDExpression;
begin
  Assert(False);
  Result := nil;
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
begin
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
begin
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
begin
{  if Result = token_cond_end then
    Result := FParser.NextToken;}
end;

function TNPUnit.ParseConstSection(Scope: TScope): TTokenID;
begin
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

function TNPUnit.ParseAsmProc(Scope: TScope): TTokenID;
begin
  Result := token_unknown;
end;

function TNPUnit.ParseAsmSpecifier: TTokenID;
begin
  Assert(False);
  Result := token_unknown;
end;

procedure TNPUnit.ParseASMStatement(Scope: TScope; SContext: PSContext);
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

function TNPUnit.ParseClosureCapturedVarsDecl(Scope: TScope; out CapturedVars: TIDClosure.TCapturedVars): TTokenID;
begin
  // todo
  parser_MatchNextToken(Scope, token_closeblock);
  Result := parser_NextToken(Scope);
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

procedure TNPUnit.SaveConstsToStream(Stream: TStream);
begin
  Assert(False);
end;

procedure TNPUnit.SaveMethodBodies(Stream: TStream);
begin
  Assert(False);
end;


procedure TNPUnit.SaveTypesToStream(Stream: TStream);
begin
  Assert(False);
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
begin
  Assert(False);
end;

procedure TNPUnit.SaveBodyToStream(Stream: TStream);
begin
  Assert(False);
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

function TNPUnit.CheckAndParseDeprecated(Scope: TScope; CurrToken: TTokenID): TTokenID;
begin
end;

function TNPUnit.CheckAndParseAttribute(Scope: TScope): TTokenID;
begin

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

function TNPUnit.CheckAndParseProcTypeCallConv(Scope: TScope; TypeDecl: TIDType): TTokenID;
begin

end;

function TNPUnit.ParseDeprecated(Scope: TScope; out &Deprecated: TIDExpression): TTokenID;
begin

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
  FormatSettings.DecimalSeparator := '.';

end.
