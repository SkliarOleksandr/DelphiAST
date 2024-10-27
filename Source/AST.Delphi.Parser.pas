unit AST.Delphi.Parser;

interface

uses
  AST.Pascal.Parser,
  AST.Lexer,
  AST.Classes,
  AST.Parser.Messages,
  AST.Delphi.Operators,
  AST.Delphi.Errors,
  AST.Lexer.Delphi,
  AST.Delphi.DataTypes,
  AST.Delphi.Classes,
  AST.Delphi.SysTypes,
  AST.Delphi.Declarations,
  AST.Parser.Utils,
  AST.Parser.Contexts,
  AST.Delphi.Contexts,
  AST.Parser.Options,
  AST.Parser.ProcessStatuses,
  AST.Intf,
  AST.Pascal.ConstCalculator,
  AST.Delphi.Options,
  AST.Delphi.Project,
  AST.Delphi.Intf,
  System.Generics.Collections;
// System.Generics.Defaults,
// System.Internal.GenericsHlpr
// system
// sysinit
// GETMEM.INC
// system.Classes
// System.IOUtils
// system.Rtti
// System.JSON
// system.Types
// system.TypInfo
// system.Threading
// system.UITypes
// System.Contnrs
// System.SyncObjs
// System.SysUtils
// System.Math
// System.Messaging
// System.Variants
// System.UITypes
// Winapi.Windows
// Winapi.ActiveX
// Winapi.CommCtrl
// Winapi.UrlMon
// Winapi.PropSys
// Winapi.MSXMLIntf
// Winapi.ShlObj
// Winapi.ImageHlp
// AnsiStrings
// Character
// Vcl.Forms
// Vcl.ImgList
// Vcl.ActnList
// Vcl.Controls
// Vcl.StdCtrls
// Vcl.ComCtrls

type

  {anonymous types cache}
  TDeclCache = class
  private
    fRanges: TList<TIDRangeType>;
    fSets: TList<TIDSet>;
    //Arrays: TList<TIDSet>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Decl: TIDRangeType); overload;
    procedure Add(Decl: TIDSet); overload;

    function FindRange(BaseType: TIDType; LoBound, HiBound: TIDExpression): TIDRangeType;
    function FindSet(BaseType: TIDType): TIDSet;
  end;

  TASTDelphiUnit = class(TPascalUnit, IASTDelphiUnit)
  type
    TVarModifyPlace = (vmpAssignment, vmpPassArgument);
  var
    fRCPathCount: UInt32;              // кол-во проходов increfcount/decrefcount для деклараций
    fInitProcExplicit: Boolean;        // определена ли явно секция init
    fFinalProcExplicit: Boolean;       // определена ли явно секция final
    fInitProc: TIDProcedure;
    fFinalProc: TIDProcedure;
    fSystemExplicitUse: Boolean;
    fCondStack: TSimpleStack<Boolean>;
    fPackage: IASTDelphiProject;
    fDefines: TDefines;
    fOptions: TDelphiOptions;
    fIncludeFilesStack: TSimpleStack<string>;
    fUnitSContext: TSContext;
    fCCalc: TExpressionCalculator;
    fErrors: TASTDelphiErrors;
    fSysDecls: PDelphiSystemDeclarations;
    fCache: TDeclCache;
    fForwardTypes: TList<TIDType>;
    property Sys: PDelphiSystemDeclarations read fSysDecls;
    procedure CheckLeftOperand(const Status: TRPNStatus);
    class procedure CheckAndCallFuncImplicit(const EContext: TEContext); overload; static;
    class function CheckAndCallFuncImplicit(const SContext: TSContext; Expr: TIDExpression; out WasCall: Boolean): TIDExpression; overload; static;
    class function CheckAndCallFuncImplicit(const EContext: TEContext; Expr: TIDExpression): TIDExpression; overload; static;

    function CreateAnonymousConstant(Scope: TScope; var EContext: TEContext;
      const ID: TIdentifier; IdentifierType: TIdentifierType): TIDExpression;
    procedure InitEContext(out EContext: TEContext; const SContext: TSContext; EPosition: TExpessionPosition); overload; inline;
    procedure AddType(const Decl: TIDType);
    class function CheckExplicit(const SContext: TSContext; const Source, Destination: TIDType; out ExplicitOp: TIDDeclaration): Boolean; overload; static;
    class function MatchExplicit(const SContext: TSContext; const Source: TIDExpression; Destination: TIDType; out Explicit: TIDDeclaration): TIDExpression; overload; static;

    //class function MatchArrayImplicitToRecord(const SContext: TSContext; Source: TIDExpression; Destination: TIDStructure): TIDExpression; static;

    function Lexer_MatchSemicolonAndNext(Scope: TScope; ActualToken: TTokenID): TTokenID;
    //========================================================================================================
    function ProcSpec_Inline(Scope: TScope; var Flags: TProcFlags): TTokenID;
    function ProcSpec_Export(Scope: TScope; var Flags: TProcFlags): TTokenID;
    function ProcSpec_Forward(Scope: TScope; var Flags: TProcFlags): TTokenID;
    function ProcSpec_External(Scope: TScope; out ImportLib, ImportName: TIDDeclaration; var Flags: TProcFlags): TTokenID;
    function ProcSpec_Overload(Scope: TScope; var Flags: TProcFlags): TTokenID;
    function ProcSpec_Virtual(Scope: TScope; Struct: TIDStructure; var Flags: TProcFlags): TTokenID;
    function ProcSpec_Dynamic(Scope: TScope; Struct: TIDStructure; var Flags: TProcFlags): TTokenID;
    function ProcSpec_Abstract(Scope: TScope; Struct: TIDStructure; var Flags: TProcFlags): TTokenID;
    function ProcSpec_Override(Scope: TScope; Struct: TIDStructure; var Flags: TProcFlags): TTokenID;
    function ProcSpec_Final(Scope: TScope; Struct: TIDStructure; var Flags: TProcFlags): TTokenID;
    function ProcSpec_Reintroduce(Scope: TScope; var Flags: TProcFlags): TTokenID;
    function ProcSpec_Static(Scope: TScope; var Flags: TProcFlags; var ProcType: TProcType): TTokenID;
    function ProcSpec_FastCall(Scope: TScope; var CallConvention: TCallConvention): TTokenID;
    function ProcSpec_StdCall(Scope: TScope; var CallConvention: TCallConvention): TTokenID;
    function ProcSpec_SafeCall(Scope: TScope; var CallConvention: TCallConvention): TTokenID;
    function ProcSpec_DispId(AScope: TScope; AStruct: TIDStructure; const AMethodID: TIdentifier): TTokenID;
    function ProcSpec_CDecl(Scope: TScope; var CallConvention: TCallConvention): TTokenID;
    function ProcSpec_Message(Scope: TScope; var Flags: TProcFlags): TTokenID;

    function InstantiateGenericType(AScope: TScope; AGenericType: TIDType;
                                    const AGenericArgs: TIDExpressions): TIDType;
    function InstantiateGenericProc(AScope: TScope; AGenericProc: TIDProcedure;
                                    const AGenericArgs: TIDExpressions): TIDProcedure;
    function GetWeakRefType(Scope: TScope; SourceDataType: TIDType): TIDWeekRef;
    function CreateAnonymousConstTuple(Scope: TScope; ElementDataType: TIDType): TIDExpression;
    function GetCurrentParsedFileName(OnlyFileName: Boolean): string;
    class function CreateRangeType(Scope: TScope; LoBound, HiBound: Integer): TIDRangeType; static;
    class procedure AddSelfParameter(AProcScope: TProcScope; Struct: TIDStructure; ClassMethod: Boolean); static; inline;
    function AddResultParameter(ParamsScope: TParamsScope; DataType: TIDType): TIDVariable;
    procedure CheckCorrectEndCondStatemet(var Token: TTokenID);
  public
    property Package: IASTDelphiProject read fPackage;
    property Options: TDelphiOptions read fOptions;
    property ERRORS: TASTDelphiErrors read fErrors;
    class function GetTMPVar(const SContext: TSContext; DataType: TIDType): TIDVariable; overload; static;
    class function GetTMPVar(const EContext: TEContext; DataType: TIDType): TIDVariable; overload; static;
    class function GetTMPRef(const SContext: TSContext; DataType: TIDType): TIDVariable; static;
    class function GetTMPVarExpr(const SContext: TSContext; DataType: TIDType; const TextPos: TTextPosition): TIDExpression; overload; inline; static;
    class function GetTMPVarExpr(const EContext: TEContext; DataType: TIDType; const TextPos: TTextPosition): TIDExpression; overload; inline; static;
    class function GetTMPRefExpr(const SContext: TSContext; DataType: TIDType): TIDExpression; overload; inline; static;
    class function GetTMPRefExpr(const SContext: TSContext; DataType: TIDType; const TextPos: TTextPosition): TIDExpression; overload; inline; static;
    class function GetStaticTMPVar(DataType: TIDType; VarFlags: TVariableFlags = []): TIDVariable; static;
    function GetBuiltins: IDelphiBuiltInTypes;
    property Builtins: IDelphiBuiltInTypes read GetBuiltins;
  protected
    function GetSource: string; override;
    function GetErrors: TASTDelphiErrors;
    function GetSystemDeclarations: PDelphiSystemDeclarations; virtual;
    procedure CheckLabelExpression(const Expr: TIDExpression); overload;
    procedure CheckLabelExpression(const Decl: TIDDeclaration); overload;

    function Process_operators(var EContext: TEContext; OpID: TOperatorID): TIDExpression;
    function Process_CALL(var EContext: TEContext): TIDExpression;
    function Process_CALL_direct(const SContext: TSContext; PExpr: TIDCallExpression; CallArguments: TIDExpressions): TIDExpression;
    function process_CALL_constructor(const SContext: TSContext; CallExpression: TIDCallExpression;
                                      const CallArguments: TIDExpressions): TIDExpression;
    procedure Process_operator_Assign(var EContext: TEContext);
    function Process_operator_neg(var EContext: TEContext): TIDExpression;
    function Process_operator_not(var EContext: TEContext): TIDExpression;
    function Process_operator_Addr(var EContext: TEContext): TIDExpression;
    function Process_operator_Deref(var EContext: TEContext): TIDExpression;
    function Process_operator_In(var EContext: TEContext; const Left, Right: TIDExpression): TIDExpression;
    function Process_operator_Is(var EContext: TEContext): TIDExpression;
    function Process_operator_As(var EContext: TEContext): TIDExpression;
    function Process_operator_Period(var EContext: TEContext): TIDExpression;
    function MatchImplicitOrNil(const SContext: TSContext; Source: TIDExpression; Dest: TIDType): TIDExpression;
    function MatchArrayImplicit(const SContext: TSContext; Source: TIDExpression; DstArray: TIDArray): TIDExpression;
    function MatchRecordImplicit(const SContext: TSContext; Source: TIDExpression; DstRecord: TIDRecord): TIDExpression;
    function MatchBinarOperator(const SContext: TSContext; Op: TOperatorID; var Left, Right: TIDExpression): TIDDeclaration;
    function MatchBinarOperatorWithImplicit(const SContext: TSContext; Op: TOperatorID; var Left, Right: TIDexpression): TIDDeclaration;
    function FindBinaryOperator(const SContext: TSContext; OpID: TOperatorID; var Left, Right: TIDExpression): TIDDeclaration;
    class function MatchUnarOperator(Op: TOperatorID; Right: TIDType): TIDType; overload; static; inline;
    class function MatchUnarOperator(const SContext: TSContext; Op: TOperatorID; Source: TIDExpression): TIDExpression; overload; static;
    function DoMatchBinarOperator(const SContext: TSContext; OpID: TOperatorID; var Left, Right: TIDExpression): TIDDeclaration;
    procedure MatchProc(const SContext: TSContext; CallExpr: TIDExpression; const ProcParams: TIDParamArray; var CallArgs: TIDExpressions);
    function FindImplicitFormBinarOperators(const Operators: TBinaryOperatorsArray;
                                            const Left, Right: TIDType;
                                            out LLeftImplicitCast: TIDDeclaration;
                                            out LRightrImplicitCast: TIDDeclaration;
                                            out BetterFactor: Integer): TIDDeclaration;
    function MatchBinarOperatorWithTuple(const SContext: TSContext; Op: TOperatorID; var CArray: TIDExpression;
      const SecondArg: TIDExpression): TIDDeclaration;
    procedure Progress(StatusClass: TASTProcessStatusClass); override;
  public
    function MatchImplicit3(const SContext: TSContext; Source: TIDExpression; Dest: TIDType;
                            AAbortIfError: Boolean = True): TIDExpression;
    class function MatchOperatorIn(const SContext: TSContext; const Left, Right: TIDExpression): TIDDeclaration; static;
    class function CheckConstDynArrayImplicit(const SContext: TSContext; Source: TIDExpression; Destination: TIDType): TIDType; static;
    class function MatchDynArrayImplicit(Source: TIDExpression; Destination: TIDType): TIDType; static;
    function MatchOverloadProc(const SContext: TSContext; Item: TIDExpression; const CallArgs: TIDExpressions; CallArgsCount: Integer): TIDProcedure;
    class function MatchImplicitClassOf(Source: TIDExpression; Destination: TIDClassOf): TIDDeclaration; static;
    class function MatchProcedureTypes(Src: TIDProcType; Dst: TIDProcType): TIDType; static;
    procedure MatchPropSetter(Prop: TIDProperty; Setter: TIDExpression; const PropParams: TIDParamArray);
    procedure MatchPropGetter(Prop: TIDProperty; Getter: TIDProcedure; const PropParams: TIDParamArray);
    class function MatchImplicit(Source, Destination: TIDType): TIDDeclaration; static; inline;
    procedure CheckVarParamConformity(Param: TIDVariable; Arg: TIDExpression);
    function IsConstValueInRange(Value: TIDExpression; RangeExpr: TIDRangeConstant): Boolean;
    function IsConstRangesIntersect(const Left, Right: TIDRangeConstant): Boolean;
    function IsConstEqual(const Left, Right: TIDExpression): Boolean;
    function IsStructsMethod(AStruct: TIDStructure; AProc: TIDProcedure): Boolean;
    procedure CheckIntfSectionMissing(Scope: TScope); inline;
    procedure CheckImplicitTypes(Src, Dst: TIDType; Position: TTextPosition); inline;
    procedure CheckEmptyExpression(Expression: TIDExpression); inline;
    procedure CheckArrayExpression(Expression: TIDExpression); inline;
    procedure CheckIncompletedProcs; virtual;
    procedure CheckIncompletedIntfProcs(AClassType: TIDClass);
    procedure StaticCheckBounds(ABound: TIDOrdinal; AValue: TIDConstant); overload;
    procedure CheckIncompleteFwdTypes;
    procedure CheckEndOfFile(Token: TTokenID);
    procedure CheckProcedureType(DeclType: TIDType); inline;
    procedure CheckStingType(DataType: TIDType); inline;
    class procedure CheckDestructorSignature(const DProc: TIDProcedure); static;
    class procedure CheckStaticRecordConstructorSign(const CProc: TIDProcedure); static;
    procedure CheckConstValueOverflow(Src: TIDExpression; DstDataType: TIDType);
    class procedure CheckStringExpression(Expression: TIDExpression); static; inline;
    procedure CheckConstExpression(Expression: TIDExpression); inline;
    procedure CheckIntExpression(Expression: TIDExpression); inline;
    procedure CheckOrdinalExpression(Expression: TIDExpression); inline;
    procedure CheckOrdinalType(DataType: TIDType); inline;
    class procedure CheckNumericExpression(Expression: TIDExpression); static; inline;
    procedure CheckBooleanExpression(Expression: TIDExpression); inline;
    procedure CheckVarExpression(Expression: TIDExpression; VarModifyPlace: TVarModifyPlace);
    class procedure CheckPointerType(Expression: TIDExpression); static; inline;
    class procedure CheckReferenceType(Expression: TIDExpression); static; inline;
    class procedure CheckRecordType(Expression: TIDExpression); static; inline;
    class procedure CheckStructType(Expression: TIDExpression); overload; static; inline;
    class procedure CheckStructType(Decl: TIDType); overload; static; inline;
    class procedure CheckExprHasMembers(Expression: TIDExpression); static;
    procedure CheckType(Expression: TIDExpression); inline;
    procedure CheckClassType(Expression: TIDExpression); inline;
    procedure CheckExceptionType(Decl: TIDDeclaration);
    procedure CheckClassExpression(Expression: TIDExpression); inline;
    class procedure CheckSetType(Expression: TIDExpression); static; inline;
    procedure CheckClassOrIntfType(Expression: TIDExpression); overload;
    procedure CheckClassOrClassOfOrIntfType(Expression: TIDExpression); overload;
    procedure CheckClassOrIntfType(DataType: TIDType; const TextPosition: TTextPosition); overload;
    class procedure CheckInterfaceType(Expression: TIDExpression); static; inline;
    procedure CheckIncompleteType(Fields: TScope);
    class procedure CheckAccessMember(SContext: PSContext; Decl: TIDDeclaration; const ID: TIdentifier);
    class procedure CheckIntConstInRange(const Expr: TIDExpression; HiBount, LowBound: Int64); static;
    procedure InsertToScope(Scope: TScope; Item: TIDDeclaration); overload; inline;
    procedure InsertToScope(Scope: TScope; const ID: string; Declaration: TIDDeclaration); overload; inline;
    //////////////////////////////////////////////////////////////////////////////////////////////////////
    ///  Lexer helper functions
    procedure Lexer_ReadToken(Scope: TScope; const Token: TTokenID); inline;
    procedure Lexer_ReadSemicolon(Scope: TScope); inline;
    procedure Lexer_MatchIdentifier(const ActualToken: TTokenID); inline;
    procedure Lexer_MatchToken(const ActualToken, ExpectedToken: TTokenID); inline;
    procedure Lexer_MatchCurToken(const ExpectedToken: TTokenID); inline;
    procedure Lexer_MatchSemicolon(const ActualToken: TTokenID); inline;
    procedure Lexer_ReadCurrIdentifier(var Identifier: TIdentifier); inline;
    procedure Lexer_ReadTokenAsID(var Identifier: TIdentifier);
    procedure Lexer_ReadNextIdentifier(Scope: TScope; out Identifier: TIdentifier); inline;
    procedure Lexer_ReadNextIFDEFLiteral(Scope: TScope; out Identifier: TIdentifier); inline;
    procedure Lexer_MatchParamNameIdentifier(ActualToken: TTokenID); inline;
    function Lexer_CurTokenID: TTokenID; inline;
    function Lexer_AmbiguousId: TTokenID; inline;
    function Lexer_ReadSemicolonAndToken(Scope: TScope): TTokenID; inline;
    function Lexer_NextToken(Scope: TScope): TTokenID; overload; inline;
    function Lexer_NextReseredToken(Scope: TScope): TTokenID; overload; inline;
    function Lexer_Position: TTextPosition; inline;
    function Lexer_PrevPosition: TTextPosition; inline;
    function Lexer_IdentifireType: TIdentifierType; inline;
    function Lexer_TokenLexem(const TokenID: TTokenID): string; inline;
    function Lexer_Line: Integer; inline;
    function Lexer_Original: string; inline;
    function Lexer_SkipBlock(StopToken: TTokenID): TTokenID;
    function Lexer_SkipTo(Scope: TScope; StopToken: TTokenID): TTokenID;
    function Lexer_NotEof: boolean; inline;
    function Lexer_IsCurrentIdentifier: boolean; inline;
    function Lexer_IsCurrentToken(TokenID: TTokenID): boolean; inline;
    function Lexer_TreatTokenAsToken(AToken: TTokenID): TTokenID; inline;

    function ReadNewOrExistingID(Scope: TScope; out ID: TIDentifier; out Decl: TIDDeclaration): TTokenID;

    procedure PutMessage(Message: TCompilerMessage); overload;
    procedure PutMessage(MessageType: TCompilerMessageType; const MessageText: string); overload;
    procedure PutMessage(MessageType: TCompilerMessageType; const MessageText: string; const SourcePosition: TTextPosition); overload;
    procedure Error(const Message: string; const Params: array of const; const TextPosition: TTextPosition);
    procedure Warning(const Message: string; const Params: array of const; const TextPosition: TTextPosition);
    procedure Hint(const Message: string; const Params: array of const; const TextPosition: TTextPosition); overload;
    procedure Hint(const Message: string; const Params: array of const); overload;

    function FindAll(Scope: TScope; const ID: TIdentifier): TIDDeclArray; overload;
    function FindID(Scope: TScope; const ID: TIdentifier): TIDDeclaration; overload; inline;
    function FindIDNoAbort(Scope: TScope; const ID: TIdentifier): TIDDeclaration; overload; inline;
    function FindIDNoAbort(Scope: TScope; const ID: string): TIDDeclaration; overload; inline;

    class function StrictMatchProcSingnatures(const SrcParams, DstParams: TIDParamArray; const SrcResultType, DstResultType: TIDType): Boolean;
    class function StrictMatchProc(const Src, Dst: TIDProcedure): Boolean;
  public
    class function CheckImplicit(const SContext: TSContext; Source: TIDExpression; Dest: TIDType;
                                 AResolveCalls: Boolean = False): TIDDeclaration;
    class function ConstDynArrayToSet(const SContext: TSContext; const CDynArray: TIDExpression; TargetSetType: TIDSet): TIDExpression; static;
    class function MatchSetImplicit(const SContext: TSContext; Source: TIDExpression; Destination: TIDSet): TIDExpression; static;
    function ParseUnitName(Scope: TScope; out ID: TIdentifier): TTokenID;
    procedure ParseUnitDecl(Scope: TScope);
    function ParseUsesSection(Scope: TScope): TTokenID;
    //=======================================================================================================================
    ///  Парсинг типов
    procedure ParseEnumType(Scope: TScope; Decl: TIDEnum);
    procedure ParseRangeType(Scope: TScope; Expr: TIDExpression; const ID: TIdentifier; out Decl: TIDRangeType);
    function ParseImportStatement(Scope: TScope; out ImportLib, ImportName: TIDDeclaration): TTokenID;
    function ParseStaticArrayType(Scope: TScope; Decl: TIDArray): TTokenID;
    function ParseSetType(Scope: TScope; Decl: TIDSet): TTokenID;
    function ParsePointerType(Scope: TScope; const ID: TIdentifier; out Decl: TIDPointer): TTokenID;

    function ParseProcType(Scope: TScope; const ID: TIdentifier;
                           GDescriptor: PGenericDescriptor; out Decl: TIDProcType): TTokenID;

    function ParseRecordType(Scope: TScope; Decl: TIDRecord): TTokenID;
    function ParseCaseRecord(Scope: TScope; Decl: TIDRecord): TTokenID;
    function ParseClassAncestorType(Scope: TScope; ClassDecl: TIDClass): TTokenID;
    function ParseClassType(Scope: TScope; GDescriptor: PGenericDescriptor; const ID: TIdentifier; out Decl: TIDClass): TTokenID;
    function ParseVisibilityModifiers(Scope: TScope; var AVisibility: TVisibility; AIsClass: Boolean): TTokenID;
    function ParseTypeMember(Scope: TScope; Struct: TIDStructure): TTokenID;
    function ParseClassOfType(Scope: TScope; const ID: TIdentifier; out Decl: TIDClassOf): TTokenID;
    function ParseInterfaceType(Scope, GenericScope: TScope; GDescriptor: PGenericDescriptor;
                                ADispInterface: Boolean; const ID: TIdentifier; out Decl: TIDInterface): TTokenID;
    function ParseIntfGUID(Scope: TScope; Decl: TIDInterface): TTokenID;
    //=======================================================================================================================
    function ParseTypeRecord(Scope, GenericScope: TScope; GDescriptor: PGenericDescriptor; const ID: TIdentifier; out Decl: TIDType): TTokenID;
    function ParseTypeArray(Scope, GenericScope: TScope; GDescriptor: PGenericDescriptor; const ID: TIdentifier; out Decl: TIDType): TTokenID;
    function ParseTypeHelper(Scope, GenericScope: TScope; GDescriptor: PGenericDescriptor; const ID: TIdentifier;
                             out Decl: TDlphHelper): TTokenID;
    // функция парсинга анонимного типа
    function ParseTypeDecl(Scope: TScope; GDescriptor: PGenericDescriptor; const ID: TIdentifier; out Decl: TIDType): TTokenID;
    function ParseTypeDeclOther(Scope: TScope; const ID: TIdentifier; out Decl: TIDType): TTokenID;
    // функция парсинга именованного типа
    function ParseNamedTypeDecl(Scope: TScope): TTokenID;
    function ParseTypeSection(Scope: TScope): TTokenID;
    function ParseTypeSectionInStruct(Scope: TScope): TTokenID;
    // функция парсинга указания типа (имени существующего или анонимного типа)
    function ParseTypeSpec(Scope: TScope; out DataType: TIDType): TTokenID;
    function ParseGenericTypeSpec(Scope, ASearchScope: TScope; const ID: TIdentifier; out DataType: TIDType): TTokenID; virtual;
    function ParseGenericsHeader(Scope: TScope; out Args: TIDTypeArray): TTokenID;
    function ParseGenericsConstraint(Scope: TScope; out AConstraint: TGenericConstraint;
                                                    out AConstraintType: TIDType): TTokenID;
    function ParseGenericsArgs(Scope: TScope;
                               const SContext: TSContext;
                               out Args: TIDExpressions;
                               out ACanInstantiate: Boolean): TTokenID;

    function ParseGenericTypeDecl(Scope: TScope;
                                  GDescriptor: PGenericDescriptor;
                                  const ID: TIdentifier;
                                  ATypeClass: TIDTypeClass): TIDType;

    function ParseStatements(Scope: TScope; const SContext: TSContext; IsBlock: Boolean): TTokenID; overload;
    function GetPtrReferenceType(Decl: TIDRefType): TIDType;
    procedure CheckForwardPtrDeclarations;

    function ParseExpression(Scope: TScope; const SContext: TSContext; var EContext: TEContext; out ASTE: TASTExpression): TTokenID; overload;
    function ParseConstExpression(Scope: TScope; out Expr: TIDExpression; EPosition: TExpessionPosition): TTokenID;

    function ParseMemberCall(Scope: TScope; var EContext: TEContext; const ASTE: TASTExpression): TTokenID;
    function ParseMember(Scope: TScope; var EContext: TEContext; const ASTE: TASTExpression): TTokenID;
    function ParseIdentifier(Scope, SearchScope: TScope; out Expression: TIDExpression;
                             var EContext: TEContext; const PrevExpr: TIDExpression; const ASTE: TASTExpression): TTokenID;
    function ParseArrayMember(Scope: TScope; var EContext: TEContext; ASTE: TASTExpression): TTokenID;
    //todo: call property accessors when needed
    //function ParsePropertyMember(var PMContext: TPMContext; Scope: TScope; Prop: TIDProperty; out Expression: TIDExpression;
    //                             var EContext: TEContext; const PrevExpr: TIDExpression): TTokenID;
    function ParseIndexedPropertyArgs(Scope: TScope; out ArgumentsCount: Integer; var EContext: TEContext): TTokenID;
    function InferTypeByVector(Scope: TScope; Vector: TIDExpressions): TIDType;
    procedure ParseVector(Scope: TScope; var EContext: TEContext);
    function ParseEntryCall(Scope: TScope; CallExpr: TIDCallExpression; const EContext: TEContext; const ASTE: TASTExpression): TTokenID;
    function ParseBuiltinCall(Scope: TScope; CallExpr: TIDExpression; var EContext: TEContext): TTokenID;
    function ParseExplicitCast(Scope: TScope; const SContext: TSContext; var DstExpression: TIDExpression): TTokenID;
    function ParseProcedure(Scope: TScope; ProcType: TProcType; Struct: TIDStructure = nil): TTokenID;
    function ParseGlobalProc(Scope: TScope; ProcType: TProcType; const ID: TIdentifier; ProcScope: TProcScope): TTokenID;
    function ParseNestedProc(Scope: TScope; ProcType: TProcType; const ID: TIdentifier; ProcScope: TProcScope): TTokenID;
    function ParseIntfMethodDelegation(Scope: TScope; AClass: TIDClass; const ID: TIdentifier): TTokenID;
    function ParseProcName(AScope: TScope;
                           AProcType: TProcType;
                           out AID: TIdentifier;
                           var AStruct: TIDStructure;
                           out AProcScope: TProcScope;
                           out AGenericParams: TIDTypeArray): TTokenID;
    function ParseProcBody(Proc: TASTDelphiProc): TTokenID;
    function ParseOperator(Scope: TScope; Struct: TIDStructure): TTokenID;
    function ParseGenericMember(const PMContext: TPMContext; const SContext: TSContext; StrictSearch: Boolean; out Decl: TIDDeclaration; out WithExpression: TIDExpression): TTokenID;
    function ParseIfThenStatement(Scope: TScope; const SContext: TSContext): TTokenID;
    function ParseWhileStatement(Scope: TScope; const SContext: TSContext): TTokenID;
    function ParseRepeatStatement(Scope: TScope; const SContext: TSContext): TTokenID;
    function ParseWithStatement(Scope: TScope; const SContext: TSContext): TTokenID;
    function ParseForStatement(Scope: TScope; const SContext: TSContext): TTokenID;
    function ParseForInStatement(Scope: TScope; const SContext: TSContext; LoopVar: TIDExpression): TTokenID;
    function ParseCaseStatement(Scope: TScope; const SContext: TSContext): TTokenID;
    function ParseInheritedStatement(Scope: TScope; const EContext: TEContext): TTokenID;
    function ParseImmVarStatement(Scope: TScope; const SContext: TSContext): TTokenID;
    function ParseTrySection(Scope: TScope; const SContext: TSContext): TTokenID;
    function ParseExceptOnSection(Scope: TScope; KW: TASTKWTryBlock; const SContext: TSContext): TTokenID;
    function ParseRaiseStatement(Scope: TScope; const SContext: TSContext): TTokenID;
    function ParseLabelSection(Scope: TScope): TTokenID;
    function ParseGoToStatement(Scope: TScope; const SContext: TSContext): TTokenID;
    function ParseASMStatement(Scope: TScope; const SContext: TSContext): TTokenID;
    function ParseProperty(Scope: TScope; Struct: TIDStructure): TTokenID;
    function ParsePropertyGetter(Scope: TScope; AProp: TIDProperty): TTokenID;
    function ParsePropertySetter(Scope: TScope; AProp: TIDProperty): TTokenID;
    function ParsePropertyStored(Scope: TScope; AProp: TIDProperty): TTokenID;
    function ParseVarDefaultValue(Scope: TScope; DataType: TIDType; out DefaultValue: TIDExpression): TTokenID;
    function ParseVarStaticArrayDefaultValue(Scope: TScope; ArrType: TIDArray; out DefaultValue: TIDExpression): TTokenID;
    function ParseVarRecordDefaultValue(Scope: TScope; Struct: TIDStructure; out DefaultValue: TIDExpression): TTokenID;
    function ParseRecordInitValue(Scope: TRecordInitScope; var FirstField: TIDExpression): TTokenID;
    function ParseConstSection(Scope: TScope; AInlineConst: Boolean = False): TTokenID;
    function ParseConstSectionInStruct(Scope: TScope): TTokenID;
    function ParseVarSection(Scope: TScope; Visibility: TVisibility; IsWeak: Boolean = False): TTokenID;
    function ParseFieldsSection(Scope: TScope; Visibility: TVisibility; Struct: TIDStructure; IsClass: Boolean): TTokenID;
    function ParseFieldsInCaseRecord(Scope: TScope; Visibility: TVisibility; ARecord: TIDRecord): TTokenID;
    function ParseParameters(EntryScope: TScope; ParamsScope: TParamsScope): TTokenID;
    function ParseParametersAndResult(EntryScope: TScope; ParamsScope: TParamsScope; out AResultType: TIDType): TTokenID;
    function ParseAnonymousProc(Scope: TScope; var EContext: TEContext; const SContext: TSContext; ProcType: TTokenID): TTokenID;
    function ParseInitSection: TTokenID;
    function ParseFinalSection: TTokenID;
    function ParseUnknownID(Scope: TScope; const PrevExpr: TIDExpression; ID: TIdentifier; out Decl: TIDDeclaration): TTokenID;
    function ParseDeprecated(Scope: TScope; out DeprecatedExpr: TIDExpression): TTokenID;
    function CheckAndMakeClosure(const SContext: TSContext; const ProcDecl: TIDProcedure): TIDClosure;
    function EmitCreateClosure(const SContext: TSContext; Closure: TIDClosure): TIDExpression;
    function CheckAndParseDeprecated(Scope: TScope; CurrToken: TTokenID): TTokenID;
    function ParseAttribute(Scope: TScope): TTokenID;
    function CheckAndParseAttribute(Scope: TScope): TTokenID;
    function CheckAndParseProcTypeCallConv(Scope: TScope; Token: TTokenID; TypeDecl: TIDProcType): TTokenID;
    function CheckAndParsePlatform(Scope: TScope): TTokenID;
    function CheckAndParseAbsolute(Scope: TScope; out ADecl: TIDDeclaration): TTokenID;
    function ParseAsmSpecifier: TTokenID;
    property InitProc: TIDProcedure read FInitProc;
    property FinalProc: TIDProcedure read FFinalProc;

    /// condition compilation
    function ParseCondStatements(Scope: TScope; Token: TTokenID): TTokenID;
    function ParseCondInclude(Scope: TScope): TTokenID;
    function ParseCondIfDef(Scope: TScope): Boolean;
    function ParseCondHint(Scope: TScope): TTokenID;
    function ParseCondWarn(Scope: TScope): TTokenID;
    function ParseCondError(Scope: TScope): TTokenID;
    function ParseCondMessage(Scope: TScope): TTokenID;
    function Defined(const Name: string): Boolean;
    function ParseCondIf(Scope: TScope; out ExpressionResult: TCondIFValue): TTokenID;
    function ParseCondOptSet(Scope: TScope): TTokenID;
    procedure ParseCondDefine(Scope: TScope; add_define: Boolean);
    function ParseCondOptions(Scope: TScope): TTokenID;

    procedure EnumDeclarations(const AEnumProc: TEnumASTDeclProc; AUnitScope: TUnitScopeKind); override;
    procedure PostCompileChecks;

    property Source: string read GetSource;
    function Compile(ACompileIntfOnly: Boolean; RunPostCompile: Boolean = True): TCompilerResult; override;
    function CompileIntfOnly: TCompilerResult; override;
    function CompileSource(Scope: TScope; const FileName: string; const Source: string): ICompilerMessages;
    constructor Create(const Project: IASTProject; const FileName: string; const Source: string = ''); override;
    destructor Destroy; override;
  end;

  function GetUnit(const SContext: PSContext): TASTDelphiUnit; overload;
  function GetUnit(const EContext: TEContext): TASTDelphiUnit; overload;
  function GetBoolResultExpr(const SContext: TSContext): TIDBoolResultExpression;
  function IntConstExpression(const SContext: TSContext; const Value: Int64): TIDExpression;
  function StrConstExpression(const SContext: TSContext; const Value: string): TIDExpression;

implementation

uses
   System.Math,
   System.Types,
   System.StrUtils,
   System.SysUtils,
   System.Classes,
   AST.Parser.Errors,
   AST.Delphi.System,
   AST.Delphi.SysOperators,
   AST.Delphi.Operators.Signatures,
   AST.Parser.Log;

type
  TSContextHelper = record helper for TSContext
  private
    function GetSysUnit: TSYSTEMUnit;
    function GetErrors: TASTDelphiErrors;
  public
    property SysUnit: TSYSTEMUnit read GetSysUnit;
    property ERRORS: TASTDelphiErrors read GetErrors;
  end;


function GetUnit(const SContext: PSContext): TASTDelphiUnit;
begin
  Result := SContext.Module as TASTDelphiUnit;
end;

function GetUnit(const EContext: TEContext): TASTDelphiUnit;
begin
  Result := EContext.SContext.Module as TASTDelphiUnit;
end;

function GetBoolResultExpr(const SContext: TSContext): TIDBoolResultExpression;
var
  Decl: TIDVariable;
begin
  Decl := TASTDelphiUnit.GetTMPVar(SContext, SContext.SysUnit._Boolean);
  Result := TIDBoolResultExpression.Create(Decl);
end;

function IntConstExpression(const SContext: TSContext; const Value: Int64): TIDExpression;
var
  DataType: TIDType;
begin
  DataType := SContext.SysUnit.DataTypes[GetValueDataType(Value)];
  Result := TIDExpression.Create(TIDIntConstant.CreateWithoutScope(DataType, Value));
end;

function StrConstExpression(const SContext: TSContext; const Value: string): TIDExpression;
var
  Decl: TIDStringConstant;
begin
  Decl := TIDStringConstant.CreateAsAnonymous(nil, SContext.SysUnit._UnicodeString, Value);
  Result := TIDExpression.Create(Decl);
end;

{ TASTDelphiUnit }

function TASTDelphiUnit.ParseSetType(Scope: TScope; Decl: TIDSet): TTokenID;
var
  LBaseType: TIDType;
begin
  Lexer_MatchToken(Lexer_NextToken(Scope), token_of);
  Result := ParseTypeSpec(Scope, {out} LBaseType);
  CheckOrdinalType(LBaseType);
  Decl.BaseType := TIDOrdinal(LBaseType);
  Decl.BaseType.OverloadBinarOperator2(opIn, Decl, Sys._Boolean);
end;

function TASTDelphiUnit.ParseStatements(Scope: TScope; const SContext: TSContext; IsBlock: Boolean): TTokenID;
var
  LEContext: TEContext;
  NewScope: TScope;
  AST: TASTItem;
begin
  //     ASTE.AddDeclItem(Expr.Declaration, Expr.TextPosition);
  Result := Lexer_CurTokenID;
  var LSemicolonRequired := False;
  while True do
  begin
    // in Delphi, a semicolon is not required at the end of the last (or single) statement in a block
    if LSemicolonRequired and not (Result in [token_end, token_finally, token_except]) then
      Result := Lexer_MatchSemicolonAndNext(Scope, Result);

    LSemicolonRequired := True;
    case Result of
      {BEGIN}
      token_begin: begin
        Lexer_NextToken(Scope);
        NewScope := TBlockScope.Create(stLocal, Scope);
        Result := ParseStatements(NewScope, SContext, {IsBlock:} True);
        Lexer_MatchToken(Result, token_end);
        Result := Lexer_NextToken(Scope);
        if not IsBlock then
          Exit;
        continue;
      end;
      {END}
      token_end: begin
        Exit;
      end;
      {UNTIL}
      token_until: Break;
      {IF}
      token_if: Result := ParseIfThenStatement(Scope, SContext);
      {WHILE}
      token_while: Result := ParseWhileStatement(Scope, SContext);
      {REPEAT}
      token_repeat: Result := ParseRepeatStatement(Scope, SContext);
      {WITH}
      token_with: Result := ParseWithStatement(Scope, SContext);
      {FOR}
      token_for: Result := ParseForStatement(Scope, SContext);
      {CASE}
      token_case: Result := ParseCaseStatement(Scope, SContext);
      {GOTO}
      token_goto: Result := ParseGoTOStatement(Scope, SContext);
      {ASM}
      token_asm: Result := ParseASMStatement(Scope, SContext);
      {INHERITED}
      token_inherited: begin
        InitEContext({out} LEContext, SContext, ExprLValue);
        Result := ParseInheritedStatement(Scope, LEContext);
      end;
      {TRY}
      token_try: Result := ParseTrySection(Scope, SContext);
      {EXCEPT/FINALLY}
      token_except, token_finally: begin
        if not SContext.IsTryBlock then
          ERRORS.TRY_KEYWORD_MISSED;
        Exit;
      end;
      {RAISE}
      token_raise: Result := ParseRaiseStatement(Scope, SContext);
      {;}
      token_semicolon:;
      {VAR}
      token_var: Result := ParseImmVarStatement(Scope, SContext);
      {CONST}
      token_const: begin
        Lexer_NextToken(Scope);
        Result := ParseConstSection(Scope, {AInlineConst:} True);
      end;
      {@}
      token_address: begin
        Result := Lexer_NextToken(Scope);
        LSemicolonRequired := False;
        Continue;
      end;
      {IDENTIFIER, OPEN ROUND}
      token_identifier, {token_id_or_keyword, }token_openround:
      begin
        InitEContext({out} LEContext, SContext, ExprLValue);
        begin
          var ASTEDst, ASTESrc: TASTExpression;
          Result := ParseExpression(Scope, SContext, LEContext, ASTEDst);
          if Result = token_assign then begin
            var REContext: TEContext;
            InitEContext(REContext, SContext, ExprRValue);
            Lexer_NextToken(Scope);
            Result := ParseExpression(Scope, SContext, REContext, ASTESrc);

            LEContext.RPNPushExpression(REContext.Result);
            LEContext.RPNPushOperator(opAssignment);
            LEContext.RPNFinish();

            var Op := SContext.Add(TASTOpAssign) as TASTOpAssign;
            Op.Dst := ASTEDst;
            Op.Src := ASTESrc;

          end else
          if Result = token_colon then
          begin
            var LExpr := LEContext.Result;
            CheckLabelExpression(LExpr);
            var KW := SContext.Add(TASTKWLabel) as TASTKWLabel;
            KW.LabelDecl := LExpr.Declaration;
            Result := Lexer_NextToken(Scope);
            LSemicolonRequired := False;
            Continue;
          end else
            SContext.AddItem(ASTEDst);
          //CheckAndCallFuncImplicit(LEContext);
          //CheckUnusedExprResult(LEContext);
        end;
      end;
      token_initialization,
      token_finalization: Break;
      token_eof: Exit;
    else
      if IsBlock then
        ERRORS.EXPECTED_KEYWORD_OR_ID;
    end;

    if not IsBlock then
      Exit;
  end;
end;

function TASTDelphiUnit.ParseStaticArrayType(Scope: TScope; Decl: TIDArray): TTokenID;

  procedure CheckExpression(Expr: TIDExpression);
  begin
    CheckEmptyExpression(Expr);
    if not ((Expr.ItemType = itConst) or
           ((Expr.ItemType = itType) and (TIDType(Expr.Declaration).IsOrdinal))) then
      ERRORS.ORDINAL_CONST_OR_TYPE_REQURED;
  end;

var
  Expr: TIDExpression;
  Bound: TIDOrdinal;
  DataType: TIDType;
begin
  // todo сделать парсинг многомерного массива вида array of array of ...
  while True do begin
    // нижняя граница/тип/размер
    Lexer_NextToken(Scope);
    Result := ParseConstExpression(Scope, Expr, ExprNested);
    CheckExpression(Expr);

    if Expr.ItemType = itType then
    begin
      CheckOrdinalType(Expr.AsType);
      Bound := Expr.AsType.ActualDataType as TIDOrdinal;
    end else
      Bound := Expr.AsRangeConst.DataType as TIDRangeType;

    Decl.AddBound(Bound);
    if Result = token_coma then begin
      continue;
    end;
    Break;
  end;
  Lexer_MatchToken(Result, token_closeblock);
  Result := Lexer_NextToken(Scope);
  Lexer_MatchToken(Result, token_of);
  Result := ParseTypeSpec(Scope, DataType);
  Decl.ElementDataType := DataType;
end;

function TASTDelphiUnit.ParseTrySection(Scope: TScope; const SContext: TSContext): TTokenID;
var
  KW: TASTKWTryBlock;
  NewContext: TSContext;
  ExceptItem: TASTKWTryExceptItem;
begin
  KW := SContext.Add(TASTKWTryBlock) as TASTKWTryBlock;
  NewContext := SContext.MakeChild(Scope, KW.Body);
  // запоминаем предыдущий TryBlock
  Lexer_NextToken(Scope);
  Result := ParseStatements(Scope, NewContext, {IsBlock:} True);
  // todo: move this into ParseStatements()
  if Result = token_semicolon then
    Result := Lexer_NextToken(Scope);
  case Result of
    {parse EXCEPT section}
    token_except: begin
      Result := Lexer_NextToken(Scope);
      if Lexer_AmbiguousId <> tokenD_on then
      begin
        ExceptItem := KW.AddExceptBlock(nil);
        NewContext := SContext.MakeChild(Scope, ExceptItem.Body);
        Result := ParseStatements(Scope, NewContext, {IsBlock:} True);
      end else begin
        while Lexer_AmbiguousId = tokenD_on do
          Result := ParseExceptOnSection(Scope, KW, SContext);
        // else section  
        if Result = token_else then
        begin
          Lexer_NextToken(Scope);
          Result := ParseStatements(Scope, NewContext, {IsBlock:} True);
        end;
      end;

      Lexer_MatchToken(Result, token_end);
      Result := Lexer_NextToken(Scope);
    end;
    {parse FINALLY section}
    token_finally: begin
      Lexer_NextToken(Scope);
      KW.FinallyBody := TASTBlock.Create(KW);
      NewContext := SContext.MakeChild(Scope, KW.FinallyBody);
      Result := ParseStatements(Scope, NewContext, {IsBlock:} True);
      Lexer_MatchToken(Result, token_end);
      Result := Lexer_NextToken(Scope);
    end;
  else
    AbortWork(sExceptOrFinallySectionWasMissed, Lexer_Position);
  end;
end;

function TASTDelphiUnit.ParseTypeArray(Scope, GenericScope: TScope; GDescriptor: PGenericDescriptor; const ID: TIdentifier;
  out Decl: TIDType): TTokenID;
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

  Result := Lexer_NextToken(Scope);
  if Result = token_openblock then
  begin
    {static array}
    Decl := ParseGenericTypeDecl(Scope, GDescriptor, ID, TIDStaticArray);
    Result := ParseStaticArrayType(TypeScope, TIDArray(Decl));
  end else begin
    {dynamic array}
    Lexer_MatchToken(Result, token_of);
    Decl := ParseGenericTypeDecl(Scope, GDescriptor, ID, TIDDynArray);
    Result := ParseTypeSpec(TypeScope, DataType);
    // case: array of const
    if (DataType = nil) and (Result = token_const) then
    begin
      DataType := TSYSTEMUnit(SysUnit).SystemDeclarations._TVarRec;
      if not Assigned(DataType) then
        AbortWorkInternal('System.TVarRec is not defined', Lexer_Position);
      Result := Lexer_NextToken(Scope);
    end;
    TIDDynArray(Decl).ElementDataType := DataType;
  end;
end;

function TASTDelphiUnit.ParseTypeDecl(Scope: TScope; GDescriptor: PGenericDescriptor; const ID: TIdentifier;
                                      out Decl: TIDType): TTokenID;
var
  IsPacked: Boolean;
  IsAnonimous: Boolean;
begin
  Result := Lexer_CurTokenID;

  // является ли тип анонимным
  IsAnonimous := (ID.Name = '');

  // является ли тип упакованным
  if Result <> token_packed then
    IsPacked := False
  else begin
    IsPacked := True;
    Result := Lexer_NextToken(Scope);
  end;

  var LTokenID := Lexer_TreatTokenAsToken(Result);
  case LTokenID of
    /////////////////////////////////////////////////////////////////////////
    // type of
    /////////////////////////////////////////////////////////////////////////
    token_type: begin
      Lexer_NextToken(Scope);
      var ResExpr: TIDExpression;
      Result := ParseConstExpression(Scope, ResExpr, ExprRValue);
      if ResExpr.ItemType = itType then
      begin
        Decl := TIDAliasType.CreateAlias(Scope, ID, ResExpr.AsType, {ANewType:} True);
        Scope.AddType(Decl);
      end;
      Exit;
    end;
    /////////////////////////////////////////////////////////////////////////
    // pointer type
    /////////////////////////////////////////////////////////////////////////
    token_caret: Result := ParsePointerType(Scope, ID, TIDPointer(Decl));
    /////////////////////////////////////////////////////////////////////////
    // array type
    /////////////////////////////////////////////////////////////////////////
    token_array: Result := ParseTypeArray(Scope, nil, GDescriptor, ID, Decl);
    /////////////////////////////////////////////////////////////////////////
    // procedural type
    /////////////////////////////////////////////////////////////////////////
    tokenD_reference: Result := ParseProcType(Scope, ID, GDescriptor, TIDProcType(Decl));
    token_procedure, token_function: Result := ParseProcType(Scope, ID, GDescriptor, TIDProcType(Decl));
    /////////////////////////////////////////////////////////////////////////
    // set
    /////////////////////////////////////////////////////////////////////////
    token_set: begin
      Decl := TIDSet.Create(Scope, ID);
      if not IsAnonimous then
        Scope.AddType(Decl);
      Result := ParseSetType(Scope, TIDSet(Decl));
    end;
    /////////////////////////////////////////////////////////////////////////
    // enum
    /////////////////////////////////////////////////////////////////////////
    token_openround: begin
      Decl := TIDEnum.Create(Scope, ID);
      if not IsAnonimous then
        Scope.AddType(Decl)
      else {если это анонимная декларация типа для параметра, то добавляем его в более высокий scope}
      if Assigned(Scope.Parent) and (Scope.ScopeClass = scProc) then
        Scope := Scope.Parent;
      ParseEnumType(Scope, TIDEnum(Decl));
      Result := Lexer_NextToken(Scope);
    end;
    /////////////////////////////////////////////////////////////////////////
    // record
    /////////////////////////////////////////////////////////////////////////
    token_record: Result := ParseTypeRecord(Scope, nil, GDescriptor, ID, Decl);
    /////////////////////////////////////////////////////////////////////////
    // class
    /////////////////////////////////////////////////////////////////////////
    token_class: begin
      Result := Lexer_NextToken(Scope);
      if Result = token_of then
        Result := ParseClassOfType(Scope, ID, TIDClassOf(Decl))
      else begin
        if IsAnonimous then
          AbortWork(sClassTypeCannotBeAnonimous, Lexer.PrevPosition);
        Result := ParseClassType(Scope, GDescriptor, ID, TIDClass(Decl));
      end;
    end;
    /////////////////////////////////////////////////////////////////////////
    // interface
    /////////////////////////////////////////////////////////////////////////
    token_interface,
    token_dispinterface:
      Result := ParseInterfaceType(Scope, nil, GDescriptor, (LTokenID = token_dispinterface), ID, TIDInterface(Decl));
    /////////////////////////////////////////////////////////////////////////
    // other
    /////////////////////////////////////////////////////////////////////////
  else
    Result := ParseTypeDeclOther(Scope, ID, Decl);
  end;
  if Assigned(Decl) then
  begin
    Decl.IsPacked := IsPacked;
    AddType(Decl);
  end;
  Result := CheckAndParseDeprecated(Scope, Result);

  Result := CheckAndParsePlatform(Scope);
end;

function TASTDelphiUnit.ParseTypeDeclOther(Scope: TScope; const ID: TIdentifier; out Decl: TIDType): TTokenID;
var
  IsAnonimous: Boolean;
  Expr: TIDExpression;
begin
  IsAnonimous := (ID.Name = '');
  Result := Lexer_CurTokenID;
  case Result of
    token_minus, token_plus: begin
      Result := ParseConstExpression(Scope, Expr, ExprRValue);
      ParseRangeType(Scope, Expr, ID, TIDRangeType(Decl));
      if not IsAnonimous then
        Scope.AddType(Decl);
    end;
    token_identifier: begin
      Result := ParseConstExpression(Scope, Expr, ExprType);
      {alias type}
      if Expr.ItemType = itType then begin
        Decl := TIDAliasType.CreateAlias(Scope, ID, Expr.AsType);
      end else
      {range type}
      begin
        ParseRangeType(Scope, Expr, ID, TIDRangeType(Decl));
      end;
      if not IsAnonimous then
        Scope.AddType(Decl);
    end;
  else
    Decl := nil;
  end;
end;

function TASTDelphiUnit.ParseTypeHelper(Scope, GenericScope: TScope; GDescriptor: PGenericDescriptor; const ID: TIdentifier;
  out Decl: TDlphHelper): TTokenID;
var
  TargetID: TIdentifier;
  TargetDecl: TIDType;
  Visibility: TVisibility;
begin
  Lexer_ReadToken(Scope, token_for);
  Lexer_ReadNextIdentifier(Scope, TargetID);
  TargetDecl := TIDType(FindID(Scope, TargetID));
  if TargetDecl.ItemType <> itType then
    ERRORS.TYPE_REQUIRED(Lexer_PrevPosition);

  Decl := TDlphHelper.Create(Scope, ID);
  Decl.Target := TargetDecl;

  if Assigned(TargetDecl.Helper) then
    Warning('Helper for %s was reassigned from %s to %s',
      [TargetDecl.Name, TargetDecl.Helper.Name, ID.Name], ID.TextPosition);

  TargetDecl.Helper := Decl;

  if ID.Name <> '' then
    Scope.AddType(Decl);

  Result := Lexer_NextToken(Scope);
  while True do begin
    Result := ParseVisibilityModifiers(Scope, {var} Visibility, {AIsClass:} False);
    case Result of
      token_class: begin
        Result := Lexer_NextReseredToken(scope);
        case Result of
          token_procedure: Result := ParseProcedure(Decl.Members, ptClassProc, Decl);
          token_function: Result := ParseProcedure(Decl.Members, ptClassFunc, Decl);
          tokenD_operator: Result := ParseOperator(Decl.Members, Decl);
          token_property: Result := ParseProperty(Decl.Members, Decl);
        else
          AbortWork('PROCEDURE, FUNCTION and PROPERIES are allowed in helpers only', Lexer_Position);
        end;
      end;
      token_procedure: Result := ParseProcedure(Decl.Members, ptProc, Decl);
      token_function: Result := ParseProcedure(Decl.Members, ptFunc, Decl);
      token_property: Result := ParseProperty(Decl.Members, Decl);
      token_const: Result := ParseConstSectionInStruct(Decl.Members);
      token_type: Result := ParseTypeSectionInStruct(Decl.Members);
      token_end: break;
    else
      AbortWork('PROCEDURE, FUNCTION and PROPERIES are allowed in helpers only', Lexer_Position);
    end;
  end;
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ParseTypeMember(Scope: TScope; Struct: TIDStructure): TTokenID;
begin
  Result := Lexer_NextToken(Scope);
  case Result of
    token_procedure: Result := ParseProcedure(Struct.Members, ptClassProc, Struct);
    token_function: Result := ParseProcedure(Struct.Members, ptClassFunc, Struct);
    token_property: Result := ParseProperty(Struct.Members, Struct);
    token_constructor: Result := ParseProcedure(Struct.Members, ptClassConstructor, Struct);
    token_destructor: Result := ParseProcedure(Struct.Members, ptClassDestructor, Struct);
    token_var: begin
      Lexer_NextToken(Scope);
      Result := ParseFieldsSection(Struct.Members, vLocal, Struct, {IsClass:} True);
    end;
    token_threadvar: begin
      Lexer_NextToken(Scope);
      Result := ParseFieldsSection(Struct.Members, vLocal, Struct, {IsClass:} True);
    end;
  else
    ERRORS.PROC_OR_PROP_OR_VAR_REQUIRED;
  end;
end;

function TASTDelphiUnit.ParseTypeRecord(Scope, GenericScope: TScope; GDescriptor: PGenericDescriptor;
                                        const ID: TIdentifier;
                                        out Decl: TIDType): TTokenID;
begin
  Result := Lexer_NextToken(Scope);

  if Lexer_AmbiguousId = tokenD_helper then
  begin
    Result := ParseTypeHelper(Scope, GenericScope, GDescriptor, ID, {out} TDlphHelper(Decl));
    Exit;
  end;

  // try to find forward declaration first (for system types)
  var FForwardDecl := FindIDNoAbort(Scope, ID);
  if Assigned(FForwardDecl) and (FForwardDecl is TIDRecord) and TIDType(FForwardDecl).NeedForward then
    Decl := TIDRecord(FForwardDecl)
  else begin
    Decl := TIDRecord.Create(Scope, ID);
    Decl.GenericDescriptor := GDescriptor;
    if Assigned(GenericScope) then
      TIDRecord(Decl).Members.AddScope(GenericScope);

    if ID.Name <> '' then
      Scope.AddType(Decl);
  end;

  Result := ParseRecordType(Scope, TIDRecord(Decl));
end;

function TASTDelphiUnit.ParseTypeSpec(Scope: TScope; out DataType: TIDType): TTokenID;
var
  Decl: TIDDeclaration;
  SearchScope: TScope;
  LCompoundIDName: string;
begin
  Result := Lexer_NextToken(Scope);
  if (Result = token_identifier) and (Lexer_IdentifireType = itIdentifier) then
  begin
    SearchScope := nil;
    while True do begin
      var ID: TIdentifier;
      Lexer_ReadCurrIdentifier(ID);
      Result := Lexer_NextToken(Scope);

      {if this is a generic type}
      if Result = token_less then
      begin
        // todo: rework ParseGenericTypeSpec (LSearchScope has been added as tmp solution)
        var LSearchScope := Scope;
        if Assigned(SearchScope) then
          LSearchScope := SearchScope;
        Result := ParseGenericTypeSpec(Scope, LSearchScope, ID, {out} DataType);
        Decl := DataType;
      end else
      begin
        LCompoundIDName := AddStringSegment(LCompoundIDName, ID.Name, '.');

        if SearchScope = nil then
          Decl := FindIDNoAbort(Scope, LCompoundIDName)
        else begin
          Decl := SearchScope.FindMembers(ID.Name);
          // in case we have no found an id, let's assume that it can be a unit name
          if not Assigned(Decl) then
          begin
            Decl := FindIDNoAbort(Scope, LCompoundIDName);
            if Assigned(Decl) then
              SearchScope := nil;
          end;
        end;

        if not Assigned(Decl) then
        begin
          if Result = token_dot then
          begin
            // probably it's compound unit name
            Lexer_NextToken(Scope);
            Continue;
          end else
            ERRORS.UNDECLARED_ID(ID);
        end;

        // workaround for a case when param or field can be named as type
        if Decl.ItemType = itVar then
        begin
          var OuterDecl := FindIDNoAbort(Scope.Parent, ID);
          if Assigned(OuterDecl) then
            Decl := OuterDecl;
        end;
      end;

      case Decl.ItemType of
        itType: begin
          DataType := TIDType(Decl);
          if Result = token_dot then
          begin
            if not (DataType is TIDStructure) then
              ERRORS.STRUCT_TYPE_REQUIRED(Lexer_PrevPosition);
            SearchScope := TIDStructure(DataType).Members;
            Lexer_NextToken(Scope);
            continue;
          end else
          if Result = token_openblock then
          begin
            CheckStingType(DataType);
            Lexer_NextToken(Scope);
            var Expr: TIDExpression := nil;
            Result := ParseConstExpression(Scope, Expr, ExprNested);
            Lexer_MatchToken(Result, token_closeblock);
            DataType := Sys._ShortString;
            Result := Lexer_NextToken(Scope);
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
                ERRORS.STRUCT_TYPE_REQUIRED(Lexer_PrevPosition);
              SearchScope := TIDStructure(DataType).Members;
              Lexer_NextToken(Scope);
              continue;
            end;
            Exit;
          end;
        end;
        itUnit: begin
          SearchScope := TIDUnit(Decl).Members;
          Lexer_NextToken(Scope);
          continue;
        end;
      else
        ERRORS.INVALID_TYPE_DECLARATION(ID);
      end;
      Exit;
    end;
    Exit;
  end else
  if Result = token_const then begin
    // case: array of const
    DataType := nil;
    Exit;
  end;
  // parse an anonymous type declaration
  Result := ParseTypeDecl(Scope, nil, TIdentifier.Make('', Lexer_Position), DataType);
  if not Assigned(DataType) then
    ERRORS.INVALID_TYPE_DECLARATION;
end;

procedure TASTDelphiUnit.ParseUnitDecl(Scope: TScope);
var
  Decl: TIDUnit;
  Token: TTokenID;
begin
  Lexer_ReadToken(Scope, token_Unit);
  Token := ParseUnitName(Scope, fUnitName);
  Lexer_MatchSemicolon(Token);
  Decl := TIDUnit.Create(Scope, Self);
  // add itself to the intf scope
  InsertToScope(Scope, Decl);
end;

function TASTDelphiUnit.ParseUnitName(Scope: TScope; out ID: TIdentifier): TTokenID;
var
  UName: string;
begin
  Result := Lexer_NextToken(Scope);
  while True do begin
    Lexer_MatchIdentifier(Result);
    UName := AddStringSegment(UName, Lexer.OriginalToken, '.');
    Result := Lexer_NextToken(Scope);
    if Result <> token_dot then
      break;
    Result := Lexer_NextToken(Scope);
  end;
  Package.GetStringConstant(UName);
  ID := Identifier(UName, Lexer_PrevPosition);
end;

procedure StopCompile(CompileSuccess: Boolean);
begin
  raise ECompilerStop.Create(CompileSuccess);
end;

function TASTDelphiUnit.ParseUsesSection(Scope: TScope): TTokenID;
var
  Token: TTokenID;
  ID: TIdentifier;
  LUnit: TPascalUnit;
  Idx: Integer;
begin
  while True do begin
    Token := ParseUnitName(Scope, {out} ID);

    // check recursive using
    if ID.Name = Self.FUnitName.Name then
      ERRORS.UNIT_RECURSIVELY_USES_ITSELF(ID);

    // find the unit file
    LUnit := Package.UsesUnit(ID.Name, nil) as TPascalUnit;
    if not Assigned(LUnit) then
      ERRORS.UNIT_NOT_FOUND(ID);

    // check unique using
    if IntfImportedUnits.Find(ID.Name, {var} Idx) or
       ImplImportedUnits.Find(ID.Name, {var} Idx) then
    begin
      //if (LUnit <> SYSUnit) or FSystemExplicitUse then
      ERRORS.ID_REDECLARATED(ID);
    end;

    // compile if not compiled yet
    if LUnit.Compiled = CompileNone then
    begin
      var AResult := LUnit.Compile({ACompileIntfOnly:} True);
      if (AResult <> CompileSuccess) and Package.StopCompileIfError then
        StopCompile({CompileSuccess:} False);
    end;

    // add uses unit scope as joint
    Scope.AddScope(LUnit.IntfScope);

    if Scope = IntfScope then
      IntfImportedUnits.AddObject(ID.Name, LUnit)
    else
    if Scope = ImplScope then
      ImplImportedUnits.AddObject(ID.Name, LUnit)
    else
      AbortWorkInternal('Wrong scope');

    case Token of
      token_coma: continue;
      token_semicolon: break;
    else
      ERRORS.SEMICOLON_EXPECTED;
    end;
  end;
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ParseEntryCall(Scope: TScope; CallExpr: TIDCallExpression; const EContext: TEContext;
                                       const ASTE: TASTExpression): TTokenID;
var
  ArgumentsCount: Integer;
  Expr: TIDExpression;
  InnerEContext: TEContext;
  ASTExpr: TASTExpression;
  //ASTCall: TASTOpCallProc;
begin
  //ASTCall := ASTE.AddOperation<TASTOpCallProc>;
  //ASTCall.Proc := CallExpr.Declaration;
  ArgumentsCount := 0;
  InitEContext(InnerEContext, EContext.SContext, ExprNested);
  {цикл парсинга аргументов}
  while true do begin
    Result := Lexer_NextToken(Scope);
    if Result = token_closeround then
    begin
      Result := Lexer_NextToken(Scope);
      Break;
    end;
    Result := ParseExpression(Scope, EContext.SContext, InnerEContext, ASTExpr);
    //ASTCall.AddArg(ASTExpr);
    Expr := InnerEContext.Result;

    CallExpr.Arguments := CallExpr.Arguments + [Expr];

    if Assigned(Expr) then begin
      {if Expr.DataType = Sys._Boolean then
      begin
        if (Expr.ItemType = itVar) and Expr.IsAnonymous then
          Bool_CompleteImmediateExpression(InnerEContext, Expr);
      end;}
      EContext.RPNPushExpression(Expr);
    end else begin
      // Добавляем пустой Expression для значения по умолчанию
      EContext.RPNPushExpression(nil);
    end;
    Inc(ArgumentsCount);
    case Result of
      token_coma: begin
        InnerEContext.Reset;
        continue;
      end;
      token_closeround: begin
        Result := Lexer_NextToken(Scope);
        Break;
      end;
    else
      ERRORS.INCOMPLETE_STATEMENT('call');
    end;
  end;

  TIDCallExpression(CallExpr).ArgumentsCount := ArgumentsCount;
  EContext.RPNPushExpression(CallExpr);
  EContext.RPNPushOperator(opCall);
end;

procedure TASTDelphiUnit.ParseEnumType(Scope: TScope; Decl: TIDEnum);
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
  Decl.Items := TEnumScope.Create(stLocal, Scope);
  Token := Lexer_NextToken(Scope);
  while True do begin
    Lexer_MatchIdentifier(Token);
    Lexer_ReadCurrIdentifier(ID);
    Item := TIDIntConstant.Create(Decl.Items, ID);
    Item.DataType := Decl;
    Decl.Items.AddConstant(Item);
    if not Options.SCOPEDENUMS then
      InsertToScope(Scope, Item);
    Token := Lexer_NextToken(Scope);
    if Token = token_equal then begin
      Lexer_NextToken(Scope);
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
        Token := Lexer_NextToken(Scope);
        Inc(LCValue);
        Continue;
      end;
      token_closeround: begin
        Decl.LowBound := LB;
        Decl.HighBound := HB;
        Break;
      end;
    else
      AbortWork(sComaOrCloseRoundExpected, Lexer_PrevPosition);
    end;
  end;
end;

function TASTDelphiUnit.ParseBuiltinCall(Scope: TScope; CallExpr: TIDExpression; var EContext: TEContext): TTokenID;
var
  LBuiltin: TIDBuiltInFunction;
  ArgsCount: Integer;
  Expr: TIDExpression;
  InnerEContext: TEContext;
  MacroID: TBuiltInFunctionID;
  SContext: TSContext;
  MacroParams: TIDParamArray;
  MParam: TIDVariable;
  i: Integer;
  ParamsBeginPos: Integer;
  ParamsBeginRow: Integer;
  ParamsText: string;
  Ctx: TSysFunctionContext;
  ASTExpr: TASTExpression;
begin
  ArgsCount := 0;
  Result := Lexer_CurTokenID;
  SContext := EContext.SContext;
  LBuiltin := TIDBuiltInFunction(CallExpr.Declaration);

  // парсинг аргументов
  if Result = token_openround then
  begin
    ParamsBeginPos := Lexer.SourcePosition;
    ParamsBeginRow := Lexer.LinePosition.Row;
    InitEContext(InnerEContext, EContext.SContext, ExprNested);
    while True do begin
      Lexer_NextToken(Scope);
      Result := ParseExpression(Scope, SContext, InnerEContext, ASTExpr);
      Expr := InnerEContext.Result;
      if Assigned(Expr) then begin
        {if Expr.DataType = Sys._Boolean then
        begin
          if (Expr.ItemType = itVar) and Expr.IsAnonymous then
            Bool_CompleteImmediateExpression(InnerEContext, Expr);
        end;}
        Inc(ArgsCount);
        EContext.RPNPushExpression(Expr);
      end else begin
        // Добавляем пустой Expression для значения по умолчанию
        if LBuiltin.ParamsCount > 0 then
          EContext.RPNPushExpression(nil);
      end;
      case Result of
        token_coma: begin
          InnerEContext.Reset;
          continue;
        end;
        token_closeround: begin
          ParamsText := Copy(Lexer.Source, ParamsBeginPos, Lexer.SourcePosition - ParamsBeginPos - 1);
          Result := Lexer_NextToken(Scope);
          Break;
        end;
      else
        ERRORS.INCOMPLETE_STATEMENT('call 2');
      end;
    end;
  end else begin
    ParamsBeginRow := 0;
  end;

  if LBuiltin.FunctionID = bf_sysrtfunction then
  begin
    {set default param values}
    MacroParams := LBuiltin.ExplicitParams;
    for i := 0 to LBuiltin.ParamsCount - 1 do
    begin
      MParam := MacroParams[i];
      if Assigned(MParam.DefaultValue) and (ArgsCount < LBuiltin.ParamsCount) then
      begin
        Inc(ArgsCount);
        EContext.RPNPushExpression(MParam.DefaultValue);
      end;
    end;

    {check args count}
    if LBuiltin.ParamsCount >= 0 then
    begin
      if ArgsCount > LBuiltin.ParamsCount then
        ERRORS.TOO_MANY_ACTUAL_PARAMS(CallExpr, LBuiltin.ParamsCount, ArgsCount)
      else if ArgsCount < LBuiltin.ParamsCount then
        ERRORS.NOT_ENOUGH_ACTUAL_PARAMS(CallExpr);
    end;
  end;

  Ctx.UN := Self;
  Ctx.Scope := Scope;
  Ctx.ParamsStr := ParamsText;
  Ctx.EContext := @EContext;
  Ctx.SContext := @SContext;
  Ctx.ArgsCount := ArgsCount;
  Ctx.ERRORS := ERRORS;

  MacroID := LBuiltin.FunctionID;
  case MacroID of
    bf_sysrtfunction: Expr := TIDSysRuntimeFunction(LBuiltin).Process(EContext);
    bf_sysctfunction: Expr := TIDSysCompileFunction(LBuiltin).Process(Ctx);
  else
    ERRORS.FEATURE_NOT_SUPPORTED;
    Expr := nil;
  end;
  if Assigned(Expr) then begin
    Expr.TextPosition := CallExpr.TextPosition;
    EContext.RPNPushExpression(Expr);
  end;
end;

function TASTDelphiUnit.ReadNewOrExistingID(Scope: TScope; out ID: TIDentifier; out Decl: TIDDeclaration): TTokenID;
var
  SearchScope: TScope;
  FullID: string;
  PrevFoundUnitDeclID: string;
  UnitDecl: TIDUnit;
begin
  Decl := nil;
  SearchScope := nil;
  while True do
  begin
    // read first ID
    Result := Lexer_NextToken(Scope);
    if (Result = token_identifier) {or (Result = token_id_or_keyword)} then
    begin
      Lexer_ReadCurrIdentifier(ID);

      if FullID = '' then
        FullID := ID.Name
      else
        FullID := FullID + '.' + ID.Name;

      // read next token, assuming "."
      Result := Lexer_NextToken(Scope);

      // search a declaration
      if SearchScope = nil then
        Decl := FindIDNoAbort(Scope, FullID)
      else
        Decl := SearchScope.FindMembers(FullID);

      if Result = token_dot then
      begin
        if Assigned(Decl) then
        begin
          case Decl.ItemType of
            itType: begin
              CheckStructType(TIDType(Decl));
              SearchScope := TIDStructure(Decl).Members;
              FullID := '';
            end;
            itUnit: begin
              SearchScope := TIDUnit(Decl).Members;
              PrevFoundUnitDeclID := FullID;
              FullID := '';
            end;
          end;
        end else
        if PrevFoundUnitDeclID <> '' then
        begin
          FullID := PrevFoundUnitDeclID + '.' + FullID;
          Decl := FindIDNoAbort(Scope, FullID);
          if Assigned(Decl) and (Decl.ItemType = itUnit) then
          begin
            SearchScope := TIDUnit(Decl).Members;
            PrevFoundUnitDeclID := FullID;
            FullID := '';
          end;
        end;

        Continue;
      end;
    end;

    if not Assigned(Decl) and (Pos('.', FullID) > 0) then
      ERRORS.UNDECLARED_ID(FullID, Lexer_Position);

    Exit;
  end;
end;

function TASTDelphiUnit.ParseExceptOnSection(Scope: TScope; KW: TASTKWTryBlock; const SContext: TSContext): TTokenID;
var
  ID, TypeID: TIdentifier;
  Decl, VarDecl, TypeDecl: TIDDeclaration;
  VarExpr: TASTExpression;
  NewScope: TScope;
  Item: TASTExpBlockItem;
  NewSContext: TSContext;
begin
  // try to parse...
  Result := ReadNewOrExistingID(Scope, {out} ID, {out} Decl);
  NewScope := TExceptScope.Create(stLocal, Scope);
  if Result = token_colon then
  begin
    VarDecl := TIDVariable.Create(NewScope, ID);
    VarExpr := TASTExpression.Create(nil);
    VarExpr.AddDeclItem(VarDecl, Lexer_Position);
    InsertToScope(NewScope, VarDecl);
    Result := ReadNewOrExistingID(Scope, {out} TypeID, {out} TypeDecl);
    CheckExceptionType(TypeDecl);
    VarDecl.DataType := TypeDecl as TIDType;
  end else
  begin
    VarExpr := nil;
    if not Assigned(Decl) then
      ERRORS.UNDECLARED_ID(ID);
    
    CheckExceptionType(Decl);
  end;

  Lexer_MatchToken(Result, token_do);

  Item := KW.AddExceptBlock(VarExpr);

  NewSContext := SContext.MakeChild(Scope, Item.Body);

  Lexer_NextToken(Scope);
  Result := ParseStatements(NewScope, NewSContext, {IsBlock:} False);

  if Result = token_semicolon then
    //Lexer_MatchSemicolon(Result);
    Result := Lexer_NextToken(Scope)
  else
  if Result <> token_else then  
    ERRORS.SEMICOLON_EXPECTED;
end;

function TASTDelphiUnit.ParseWhileStatement(Scope: TScope; const SContext: TSContext): TTokenID;
var
  Expression: TIDExpression;
  EContext: TEContext;
  BodySContext: TSContext;
  ASTExpr: TASTExpression;
  KW: TASTKWWhile;
begin
  KW := SContext.Add(TASTKWWhile) as TASTKWWhile;

  // loop expression
  InitEContext(EContext, SContext, ExprRValue);
  Lexer_NextToken(Scope);
  Result := ParseExpression(Scope, SContext, EContext, ASTExpr);
  KW.Expression := ASTExpr;
  Expression := EContext.Result;
  CheckEmptyExpression(Expression);

  if CheckImplicit(SContext, Expression, Sys._Boolean, {AResolveCalls:} True) = nil then
    CheckBooleanExpression(Expression);

  BodySContext := SContext.MakeChild(Scope, KW.Body);

  // loop body
  Lexer_MatchToken(Result, token_do);
  Result := Lexer_NextToken(Scope);
  if Result <> token_semicolon then
    Result := ParseStatements(Scope, BodySContext, {IsBlock:} False);
end;

function TASTDelphiUnit.ParseWithStatement(Scope: TScope; const SContext: TSContext): TTokenID;
var
  Decl: TIDDeclaration;
  EContext: TEContext;
  Expression, Expr: TIDExpression;
  WNextScope: TWithScope;
  WPrevScope: TScope;
  BodySContext: TSContext;
  ASTExpr: TASTExpression;
  KW: TASTKWWith;
begin
  WPrevScope := Scope;
  WNextScope := nil;
  KW := SContext.Add(TASTKWWith) as TASTKWWith;
  BodySContext := SContext.MakeChild(Scope, KW.Body);
  while True do begin
    Result := Lexer_NextToken(Scope);
    InitEContext(EContext, SContext, ExprRValue);
    Lexer_MatchToken(Result, token_identifier);
    Result := ParseExpression(Scope, SContext, EContext, ASTExpr);
    KW.AddExpression(ASTExpr);
    Expression := EContext.Result;

    CheckExprHasMembers(Expression);

    Decl := Expression.Declaration;
    // проверка на повторное выражение/одинаковый тип
    while Assigned(WNextScope) and (WNextScope.ScopeType = stWithScope) do begin
      Expr := WNextScope.Expression;
      if Expr.Declaration = Decl then
        AbortWork('Duplicate expression', Lexer_PrevPosition);
      if Expr.DataType = Decl.DataType then
        AbortWork('Duplicate expressions types', Lexer_PrevPosition);
      WNextScope := TWithScope(WNextScope.OuterScope);
    end;
    // создаем специальный "WITH Scope"
    WNextScope := TWithScope.Create(Scope, Expression);
    Scope.AddScope(WNextScope);
    WNextScope.OuterScope := WPrevScope;
    WNextScope.InnerScope := TIDStructure(Expression.DataType).Members;

    case Result of
      token_coma: begin
        WPrevScope := WNextScope;
        Continue;
      end;
      token_do: begin
        Lexer_NextToken(Scope);
        Result := ParseStatements(WNextScope, BodySContext, {IsBlock:} False);
        Break;
      end;
      else begin
        Lexer_MatchToken(Result, token_do);
      end;
    end;
  end;
end;

procedure TASTDelphiUnit.PostCompileChecks;
begin
  for var LIndex := 0 to fForwardTypes.Count - 1 do
  begin
    var LDecl := fForwardTypes[LIndex];
    // GetPtrReferenceType() aborts if reference type is not found
    if LDecl is TIDRefType then
      GetPtrReferenceType(LDecl as TIDRefType)
    else
      ERRORS.TYPE_NOT_COMPLETELY_DEFINED(LDecl);
  end;
end;

function GetCommonType(AType1, AType2: TIDType): TIDType;
begin
  // todo: implement
  Result := AType1;
end;

procedure InferImplicitGenericArgs(ACallExpr: TIDCallExpression);
begin
  var LProc := ACallExpr.Proc;
  var LDescriptor := LProc.GenericDescriptor;
  Assert(LProc.ParamsCount = ACallExpr.ArgumentsCount);
  // set generic array len according to proc params
  var LGenericArgs: TIDExpressions;
  SetLength(LGenericArgs, LDescriptor.ParamsCount);
  for var LIndex := 0 to LProc.ParamsCount - 1 do
  begin
    var LArg := ACallExpr.Arguments[LIndex];
    var LParam := LProc.ExplicitParams[LIndex];
    if Assigned(LArg) and LParam.IsGeneric then
    begin
      var LParamTypeName := LParam.DataType.Name;
      var LParamTypeIndex := LDescriptor.IndexOfType(LParamTypeName);
      Assert(LParamTypeIndex >= 0);

      var LCurArgType := LArg.DataType;
      if not Assigned(LGenericArgs[LParamTypeIndex]) then
        LGenericArgs[LParamTypeIndex] := TIDExpression.Create(LCurArgType)
      else begin
        var LPrevArgType := LGenericArgs[LParamTypeIndex].AsType;
        LGenericArgs[LParamTypeIndex].Declaration := GetCommonType(LCurArgType, LPrevArgType);
      end;
    end;
  end;
  ACallExpr.GenericArgs := LGenericArgs;
end;

function TASTDelphiUnit.Process_CALL(var EContext: TEContext): TIDExpression;
var
  PIndex, AIndex, ArgsCount,
  ParamsCount: Integer;
  PExpr: TIDCallExpression;
  UserArguments,
  CallArguments: TIDExpressions;
  ProcParams: TIDParamArray;
  Decl: TIDDeclaration;
  ResVar: TIDVariable;
  ProcDecl: TIDProcedure;
  ProcResult: TIDType;
  Param: TIDVariable;
  AExpr, NewExpr: TIDExpression;
  SContext: PSContext;
begin
  SContext := @EContext.SContext;
  // читаем декларацию функции
  PExpr := TIDCallExpression(EContext.RPNPopExpression());

  {вычитка явно указанных аргументов функции}
  ArgsCount := PExpr.ArgumentsCount;
  SetLength(UserArguments, ArgsCount);
  for AIndex := ArgsCount - 1 downto 0 do
    UserArguments[AIndex] := EContext.RPNPopExpression();

  ProcDecl := nil;
  Decl := PExpr.Declaration;

  {procedure call by name}
  if Decl.ItemType = itProcedure then
  begin
    ProcDecl := TIDProcedure(Decl);

    {match proper generic/overload declaration}
    if Assigned(ProcDecl.PrevOverload) then begin
      ProcDecl := MatchOverloadProc(EContext.SContext, PExpr, UserArguments, ArgsCount);
      ProcParams := ProcDecl.ExplicitParams;
      PExpr.Declaration := ProcDecl;
    end else begin
      ProcParams := ProcDecl.ExplicitParams;
      MatchProc(EContext.SContext, PExpr, ProcParams, UserArguments);
    end;

    if Assigned(ProcDecl.GenericDescriptor) then
    begin
      var LIsStructMethod := IsStructsMethod(SContext.Proc.Struct, ProcDecl);
      var LCurrentProcIsGeneric := Assigned(SContext.Proc.GenericDescriptor);
      var LIsGenericTypeStructMethod := IsGenericTypeThisStruct(SContext.Scope, ProcDecl.Struct);
      var LNeedsToInstantiate := GenericNeedsInstantiate(PExpr.GenericArgs);

      // instantiate "external" or "independent" generic methods
      if LNeedsToInstantiate and
         (not LIsStructMethod and
          not LCurrentProcIsGeneric and
          not LIsGenericTypeStructMethod) or PExpr.CanInstantiate then
      begin
        if not Assigned(PExpr.GenericArgs)  then
          InferImplicitGenericArgs(PExpr);

        ProcDecl := InstantiateGenericProc(ProcDecl.Scope, ProcDecl, PExpr.GenericArgs);
        ProcParams := ProcDecl.ExplicitParams;
        PExpr.Declaration := ProcDecl;
      end;
    end;


    ProcResult := ProcDecl.ResultType;

  end else
  {procedure call by proc type}
  if PExpr.DataTypeID = dtProcType then begin

    Decl := PExpr.DataType;

    ProcParams := TIDProcType(Decl).Params;
    ProcResult := TIDProcType(Decl).ResultType;
    MatchProc(EContext.SContext, PExpr, ProcParams, UserArguments);
  end else begin
    AbortWorkInternal(sProcOrProcVarRequired, Lexer_Position);
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
      //if AExpr.Declaration is TIDDynArrayConstant then
      //  AExpr := CheckConstDynArray(SContext, Param.DataType, AExpr);

      {проверка диаппазона для константных аргументов}
      if AExpr.IsConstant then
        CheckConstValueOverflow(AExpr, Param.DataType);

      {подбираем implicit оператор}
      AExpr := MatchImplicit3(SContext^, AExpr, Param.DataType);

      {если параметр - constref и аргумент - константа, то создаем временную переменную}
      //if (VarConstRef in Param.Flags) and (AExpr.ItemType = itConst) then
      //  AExpr := GenConstToVar(SContext, AExpr, Param.DataType);

      {если параметр - метод, получаем ссылку на него}
      if (AExpr.ItemType = itProcedure) and (not TIDProcType(AExpr.DataType).IsStatic) then
      begin
        NewExpr := GetTMPVarExpr(SContext^, AExpr.DataType, AExpr.TextPosition);
        AExpr := NewExpr;
      end;

      UserArguments[AIndex] := AExpr;
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
          ERRORS.NOT_ENOUGH_ACTUAL_PARAMS(PExpr);
        CallArguments[PIndex] := AExpr;
      end;

      {если аргумент константый дин. массив то делаем доп. проверку}
      //if AExpr.Declaration is TIDDynArrayConstant then
      //  AExpr := CheckConstDynArray(SContext, Param.DataType, AExpr);     // нужно ли????

      {проверка диаппазона для константных аргументов}
      if AExpr.IsConstant then
        CheckConstValueOverflow(AExpr, Param.DataType);

      {подбираем implicit оператор}
      AExpr := MatchImplicit3(SContext^, AExpr, Param.DataType);

      {если параметр - constref и аргумент - константа, то создаем временную переменную}
      //if (VarConstRef in Param.Flags) and (AExpr.ItemType = itConst) then
      //  AExpr := GenConstToVar(SContext, AExpr, Param.DataType);

      {если параметр - метод, получаем ссылку на него}
      if (AExpr.ItemType = itProcedure) and (not TIDProcType(AExpr.DataType).IsStatic) then
      begin
        NewExpr := GetTMPVarExpr(SContext^, AExpr.DataType, AExpr.TextPosition);
        AExpr := NewExpr;
      end;

      CallArguments[PIndex] := AExpr;

      Inc(PIndex);
      {если параметр передается по ссылке, проверяем что аргумент можно менять}
      if Param.VarReference then
      begin
        CheckVarExpression(AExpr, vmpPassArgument);
        {проверка на строгость соответствия типов}
        if Param.DataType.ActualDataType <> AExpr.DataType.ActualDataType then
          ERRORS.REF_PARAM_MUST_BE_IDENTICAL(AExpr);
      end;
    end;
  end;

  {конструирование обьекта}
  if (Assigned(ProcDecl) and (pfConstructor in ProcDecl.Flags)) then
  begin
    Result := process_CALL_constructor(EContext.SContext, PExpr, CallArguments);
    Exit;
  end;

  {результат функции}
  if Assigned(ProcResult) then begin
    ResVar := GetTMPVar(SContext^, ProcResult);
    ResVar.IncludeFlags([VarTmpResOwner]);
    Result := TIDExpression.Create(ResVar, PExpr.TextPosition);
  end else
    Result := nil;

  {если вызов был коссвенным, инлайн невозможен}
  Decl := PExpr.Declaration;
  if Decl.ItemType = itVar then
  begin
    {если переменная - поле класа, получаем ссылку на поле}
    //
    Exit;
  end;

  {INLINE подстановка (пока только если инлайн процедура готова)}
  if (pfInline in ProcDecl.Flags) and
     (ProcDecl.IsCompleted) and
     (ProcDecl <> SContext.Proc) then
  begin
    // todo:
  end else begin
    // todo:
  end;
end;

function TASTDelphiUnit.Process_CALL_direct(const SContext: TSContext; PExpr: TIDCallExpression; CallArguments: TIDExpressions): TIDExpression;
var
  AIndex, ArgsCount: Integer;
  ProcDecl: TIDProcedure;
  ProcParams: TIDParamArray;
  ResVar: TIDVariable;
  ProcResult: TIDType;
  Param: TIDVariable;
  AExpr: TIDExpression;
begin
  ArgsCount := Length(CallArguments);

  ProcDecl := PExpr.AsProcedure;
  ProcParams := ProcDecl.ExplicitParams;

  if Assigned(ProcDecl.GenericDescriptor) then
  begin
    // don't instantiate proc if this is a current struct's method
    if not IsStructsMethod(SContext.Proc.Struct, ProcDecl) then
    begin
      ProcDecl := InstantiateGenericProc(ProcDecl.Scope, ProcDecl, PExpr.GenericArgs);
      ProcParams := ProcDecl.ExplicitParams;
      PExpr.Declaration := ProcDecl;
    end;
  end;

   if Length(ProcParams) < ArgsCount then
     AbortWorkInternal('Ivalid proc call: %s', [PExpr.Text], PExpr.TextPosition);

  {если все аргументы явно указаны}
  for AIndex := 0 to ArgsCount - 1 do
  begin
    Param := ProcParams[AIndex];
    AExpr := CallArguments[AIndex];

    {если аргумент константый дин. массив то делаем доп. проверку}
//    if AExpr.Declaration is TIDDynArrayConstant then
//      AExpr := CheckConstDynArray(SContext, Param.DataType, AExpr);

    {проверка диаппазона для константных аргументов}
    if AExpr.IsConstant then
      CheckConstValueOverflow(AExpr, Param.DataType);

    {подбираем и если надо вызываем implicit оператор}
    AExpr := MatchImplicit3(SContext, AExpr, Param.DataType);

    {если параметр - constref и аргумент - константа, то создаем временную переменную}
//    if (VarConstRef in Param.Flags) and (AExpr.ItemType = itConst) then
//      AExpr := GenConstToVar(AExpr, Param.DataType);

    CallArguments[AIndex] := AExpr;

//    if Param.Reference then
//      CheckVarExpression(AExpr, vmpPassArgument);
  end;

  {результат функции}
  ProcResult := ProcDecl.ResultType;
  if Assigned(ProcResult) then begin
    ResVar := GetTMPVar(SContext, ProcResult);
    ResVar.IncludeFlags([VarTmpResOwner]);
    Result := TIDExpression.Create(ResVar, PExpr.TextPosition);
  end else
    Result := nil;
end;

function TASTDelphiUnit.process_CALL_constructor(const SContext: TSContext; CallExpression: TIDCallExpression;
                                                 const CallArguments: TIDExpressions): TIDExpression;
var
  LInstanceType: TIDType;
begin
  var LInstanceEpr := CallExpression.Instance;
  if Assigned(LInstanceEpr) and  Assigned(LInstanceEpr.Declaration) then
  begin
    case LInstanceEpr.ItemType of
      // #1 case: TMyClass.Create()
      itType: LInstanceType := LInstanceEpr.AsType;
      itVar: begin
        // #2 case: <class_of variable>.Create()
        if LInstanceEpr.DataType is TIDClassOf then
          LInstanceType := TIDClassOf(LInstanceEpr.DataType).ReferenceType
        else
          // #3 case: Self.Create()
          LInstanceType := CallExpression.Instance.DataType;
      end;
    else
      ERRORS.FEATURE_NOT_SUPPORTED;
      LInstanceType := nil;
    end;

    var LResultVar := GetTMPVar(SContext, LInstanceType);
    Result := TIDExpression.Create(LResultVar, CallExpression.TextPosition);
  end else
  begin
    AbortWorkInternal('expression is nil');
    Result := nil;
  end;
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

function TASTDelphiUnit.Defined(const Name: string): Boolean;
begin
  // поиск в локальном списке модуля
  Result := FDefines.IndexOf(Name) > -1;
  // поиск в пакете
  if not Result then
    Result := FPackage.Defines.IndexOf(Name) > -1;
end;

destructor TASTDelphiUnit.Destroy;
begin
  fForwardTypes.Free;
  fCache.Free;
  fDefines.Free;
  fOptions.Free;
  fErrors.Free;
  inherited;
end;

function TASTDelphiUnit.DoMatchBinarOperator(const SContext: TSContext; OpID: TOperatorID; var Left, Right: TIDExpression): TIDDeclaration;
begin
  if OpID in [opShiftLeft, opShiftRight] then
    Result := MatchBinarOperator(SContext, OpID, Left, Left)
  else
    Result := MatchBinarOperator(SContext, OpID, Left, Right);

  if not Assigned(Result) then
    Result := MatchBinarOperatorWithImplicit(SContext, OpID, Left, Right);
end;

function TASTDelphiUnit.EmitCreateClosure(const SContext: TSContext; Closure: TIDClosure): TIDExpression;
begin
  Result := nil;
end;

function TASTDelphiUnit.MatchBinarOperatorWithTuple(const SContext: TSContext; Op: TOperatorID; var CArray: TIDExpression; const SecondArg: TIDExpression): TIDDeclaration;
var
  DataType: TIDType;
begin
  Result := nil;
  DataType := SecondArg.DataType;
  if DataType.DataTypeID = dtSet then begin
    CArray := MatchSetImplicit(SContext, CArray, DataType as TIDSet);
    Result := Sys._Boolean; // tmp  надо проверять оператор
  end;
end;

class function TASTDelphiUnit.CheckConstDynArrayImplicit(const SContext: TSContext;
                                                         Source: TIDExpression;
                                                         Destination: TIDType): TIDType;
var
  i, ArrayLen: Integer;
  SConst: TIDDynArrayConstant;
  SExpr: TIDExpression;
  DstElementDataType: TIDType;
  ImplicitCast: TIDDeclaration;
  SrcArrayElement: TIDType;
begin
  SConst := TIDDynArrayConstant(Source.Declaration);
  ArrayLen := Length(SConst.Value);
  // exit if this is just empty array "[]"
  if ArrayLen = 0 then
    Exit(Destination);

  case Destination.DataTypeID of
    dtSet: begin
      DstElementDataType := TIDSet(Destination).BaseType;
    end;
    dtOpenArray: begin
      if TIDArray(Destination).IsOpenArrayOfConst then
        Exit(Destination);

      DstElementDataType := TIDDynArray(Destination).ElementDataType;
    end;
    dtDynArray: begin
      DstElementDataType := TIDDynArray(Destination).ElementDataType;
    end;
  else
    Exit(nil);
  end;

  // проверка каждого элемента
  for i := 0 to ArrayLen - 1 do begin
    SExpr := SConst.Value[i];
    ImplicitCast := CheckImplicit(SContext, SExpr, DstElementDataType, {AResolveCalls:} True);
    if not Assigned(ImplicitCast) then
      Exit(nil);
  end;
  Result := Destination;
end;

class function TASTDelphiUnit.MatchDynArrayImplicit(Source: TIDExpression; Destination: TIDType): TIDType;
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

function TASTDelphiUnit.FindBinaryOperator(const SContext: TSContext; OpID: TOperatorID; var Left, Right: TIDExpression): TIDDeclaration;
var
  WasCall: Boolean;
begin
  Result := DoMatchBinarOperator(SContext, OpID, Left, Right);

  if not Assigned(Result) then
  begin
    // if there is no operator found, let's assume that the left operand can be a function...
    Left := CheckAndCallFuncImplicit(SContext, Left, WasCall);
    if WasCall then
      Result := DoMatchBinarOperator(SContext, OpID, Left, Right);

    if not Assigned(Result) then
    begin
      // if there is no operator found, let's assume that the right operand can be a function...
      Right := CheckAndCallFuncImplicit(SContext, Right, WasCall);
      if WasCall then
        Result := FindBinaryOperator(SContext, OpID, Left, Right);
    end;

    if not Assigned(Result) then
      ERRORS.NO_OVERLOAD_OPERATOR_FOR_TYPES(OpID, Left, Right);
  end;
end;

function GetImplicitFactor(ASrcType, ADestType: TIDType): Integer;
begin
  if (ASrcType = ADestType) then
    Result := MaxInt8
  else
    Result := ImplicitFactor2(ASrcType.DataTypeID, ADestType.DataTypeID);
end;

function TASTDelphiUnit.FindImplicitFormBinarOperators(const Operators: TBinaryOperatorsArray;
                                                       const Left, Right: TIDType;
                                                       out LLeftImplicitCast: TIDDeclaration;
                                                       out LRightrImplicitCast: TIDDeclaration;
                                                       out BetterFactor: Integer): TIDDeclaration;
begin
  Result := nil;
  var LBetterFactor := 0;
  for var AIndex := 0 to Length(Operators) - 1 do
  begin
    var LItemPtr := PBinaryOperator(@Operators[AIndex]);
    LLeftImplicitCast := MatchImplicit({Source:} Left, {Destination:} LItemPtr.Left);
    LRightrImplicitCast := MatchImplicit({Source:} Right, {Destination:} LItemPtr.Right);
    if Assigned(LLeftImplicitCast) and Assigned(LRightrImplicitCast) then
    begin
      var LLeftImplicitFactor := GetImplicitFactor(Left, LItemPtr.Left);
      var LRightImplicitFactor := GetImplicitFactor(Right, LItemPtr.Right);
      var LMinCommonFactor := Min(LLeftImplicitFactor, LRightImplicitFactor);
      if LMinCommonFactor > LBetterFactor then
      begin
        BetterFactor := LBetterFactor;
        Result := LItemPtr.OpDecl;
      end;
    end;
  end;
end;

function TASTDelphiUnit.Process_operators(var EContext: TEContext; OpID: TOperatorID): TIDExpression;
var
  Left, Right: TIDExpression;
  Op: TIDDeclaration;
  TmpVar: TIDVariable;
begin
  Result := nil;
  case OpID of
    opAssignment: Process_operator_Assign(EContext);
    opNegative: Result := Process_operator_neg(EContext);
    opNot: Result := Process_operator_not(EContext);
    opPositive: Result := EContext.RPNPopExpression;
    opDereference: Result := Process_operator_Deref(EContext);
    opCall: Result := Process_CALL(EContext);
    opPeriod: Result := Process_operator_Period(EContext);
    opAddr: Result := Process_operator_Addr(EContext);
    opIs: Result := Process_operator_Is(EContext);
    opAs: Result := Process_operator_As(EContext);
  else begin
      // Читаем первый операнд
      Right := EContext.RPNPopExpression();
      // Читаем второй операнд
      Left := EContext.RPNPopExpression();

      Op := FindBinaryOperator(EContext.SContext, OpID, Left, Right);

      TmpVar := nil;

      // если аргументы - константы, производим константные вычисления
      if Left.IsConstant and Right.IsConstant then
      begin
        Result := fCCalc.ProcessConstOperation(EContext.Scope, Left, Right, OpID);
        Exit;
      end else begin
        if Op is TSysOpBinary then
        begin
          Result := TSysOpBinary(Op).Match(EContext.SContext, Left, Right);
          if Result = nil then
            ERRORS.NO_OVERLOAD_OPERATOR_FOR_TYPES(OpID, Left, Right);

        end else begin
          TmpVar := GetTMPVar(EContext, TIDType(Op));
          Result := TIDExpression.Create(TmpVar, Left.TextPosition);
        end;
      end;

      if Op.ItemType = itType then
      begin
        case OpID of
          opAdd: begin
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
            Result := AST.Delphi.Classes.GetBoolResultExpr(Result);
            //ILWrite(SContext, TIL.IL_Cmp(Left, Right));
            {освобожадем временные переменные}
            //ReleaseExpression(SContext, Left);
            //ReleaseExpression(SContext, Right);
            //ILWrite(SContext, TIL.IL_JmpNext(Left.Line, cNone, nil));
            //Bool_AddExprNode(EContext, SContext.ILLast, TILCondition(Ord(OpID) - Ord(opEqual) + 1));
            Exit;
          end;
          opIn: Result := Process_operator_In(EContext, Left, Right);
          opAnd, opOr, opXor, opShiftLeft, opShiftRight: begin
            if (OpID in [opAnd, opOr]) and (TmpVar.DataType = Sys._Boolean) then
            begin
              // логические операции
              Result := AST.Delphi.Classes.GetBoolResultExpr(Result);
              //Process_operator_logical_AND_OR(EContext, OpID, Left, Right, Result);
            end;
          end;
        else
          ERRORS.UNSUPPORTED_OPERATOR(OpID);
        end;
      end else
      if Op.ItemType = itProcedure then begin
        // вызов перегруженного бинарного оператора
        Result := TIDCallExpression.Create(Op);
        Result.TextPosition := Left.TextPosition;
        TIDCallExpression(Result).ArgumentsCount := 2;
        TIDCallExpression(Result).Instance := GetOperatorInstance(OpID, Left, Right);
        Result := Process_CALL_direct(EContext.SContext, TIDCallExpression(Result), TIDExpressions.Create(Left, Right));
      end;
    end;
  end;
end;

function TASTDelphiUnit.Process_operator_Addr(var EContext: TEContext): TIDExpression;
var
  Expr: TIDExpression;
  TmpDecl: TIDVariable;
  DataType: TIDType;
begin
  Expr := EContext.RPNPopExpression();
  DataType := Expr.DataType.DefaultReference;
  if DataType = nil then
  begin
    DataType := Expr.DataType.GetDefaultReference(ImplScope);
    AddType(DataType);
  end;

  if Expr.IsConstant or Expr.IsProcedure then
  begin
    // resourcestring -> PResStringRec support
    var LConst := TIDPointerConstant.CreateAsAnonymous(IntfScope, {DataType:} nil, Expr.Declaration);
    if Expr.DataTypeID in [dtString, dtAnsiString] then
      LConst.DataType := Sys._ResStringRecord
    else
      LConst.DataType := Sys._PointerType;

    Result := TIDExpression.Create(LConst, Expr.TextPosition);
  end else
  if Expr.IsTMPVar and Expr.AsVariable.Reference then
  begin
    Result := GetTMPVarExpr(EContext.SContext, DataType, Expr.TextPosition);
    Result.AsVariable.Absolute := Expr.AsVariable;
  end else begin
    TmpDecl := GetTMPVar(EContext.SContext, DataType);
    Result := TIDExpression.Create(TmpDecl, Expr.TextPosition);
  end;
end;

function TASTDelphiUnit.Process_operator_As(var EContext: TEContext): TIDExpression;
var
  Src, Dst: TIDExpression;
begin
  Dst := EContext.RPNPopExpression();
  Src := EContext.RPNPopExpression();
  CheckClassOrIntfType(Src.DataType, Src.TextPosition);
  CheckClassOrClassOfOrIntfType(Dst);
  Result := GetTMPVarExpr(EContext, Dst.AsType, Dst.TextPosition);
end;

procedure TASTDelphiUnit.Process_operator_Assign(var EContext: TEContext);

  function IsSameRef(const Dst, Src: TIDExpression): Boolean;
  begin
    if Src.ItemType = itVar then
      Result := Dst.AsVariable.Reference = Src.AsVariable.Reference
    else
      Result := False;
  end;

begin
  var Source := EContext.RPNPopExpression;
  var Dest := EContext.RPNPopExpression;

  {check Implicit}
  var NewSrc := MatchImplicit3(EContext.SContext, Source, Dest.DataType, {AAbortIfError:} False);
  if not Assigned(NewSrc) then
  begin
    // a case when the current function name is used as the result variable
    var LCurrentProc := EContext.SContext.Proc;
    if (Dest.Declaration = LCurrentProc) and Assigned(LCurrentProc.ResultType) then
    begin
      MatchImplicit3(EContext.SContext, Source, LCurrentProc.ResultType);
    end else
    begin
      // for debug
      MatchImplicit3(EContext.SContext, Source, Dest.DataType, {AAbortIfError:} False);
      ERRORS.INCOMPATIBLE_TYPES(Source, Dest);
    end;
  end else
    CheckVarExpression(Dest, vmpAssignment);
end;

function TASTDelphiUnit.Process_operator_Deref(var EContext: TEContext): TIDExpression;
var
  Src: TIDExpression;
  PtrType: TIDPointer;
  RefType: TIDType;
  AWasCall: Boolean;
begin
  Src := EContext.RPNPopExpression();
  Src := CheckAndCallFuncImplicit(EContext, Src);

  CheckPointerType(Src);

  PtrType := Src.DataType as TIDPointer;
  RefType := GetPtrReferenceType(PtrType);
  if not Assigned(RefType) then
    RefType := Sys._Untyped;

  Result := GetTMPVarExpr(EContext, RefType, Src.TextPosition);
end;

function TASTDelphiUnit.Process_operator_In(var EContext: TEContext; const Left, Right: TIDExpression): TIDExpression;
begin
  Result := TIDExpression.Create(GetTMPVar(EContext, Sys._Boolean));
end;

function TASTDelphiUnit.Process_operator_Is(var EContext: TEContext): TIDExpression;
var
  Src, Dst: TIDExpression;
begin
  Dst := EContext.RPNPopExpression();
  Src := EContext.RPNPopExpression();
  Src := CheckAndCallFuncImplicit(EContext, Src);

  CheckClassOrIntfType(Src.DataType, Src.TextPosition);
  CheckClassOrClassOfOrIntfType(Dst);
  Result := GetTMPVarExpr(EContext, Sys._Boolean, Dst.TextPosition);
end;

function TASTDelphiUnit.Process_operator_neg(var EContext: TEContext): TIDExpression;
var
  Right: TIDExpression;
  OperatorItem: TIDExpression;
begin
  // Читаем операнд
  Right := EContext.RPNPopExpression();

  OperatorItem := MatchUnarOperator(EContext.SContext, opNegative, Right);
  if not Assigned(OperatorItem) then
    ERRORS.NO_OVERLOAD_OPERATOR_FOR_TYPES(opNegative, Right);

  if Right.ItemType = itConst then
    Result := fCCalc.ProcessConstOperation(EContext.Scope, Right, Right, opNegative)
  else begin
    Result := GetTMPVarExpr(EContext, OperatorItem.DataType, Right.TextPosition);
  end;
end;

function TASTDelphiUnit.Process_operator_not(var EContext: TEContext): TIDExpression;
var
  Right, OperatorItem: TIDExpression;
  DataType: TIDType;
begin
  // Читаем операнд
  Right := EContext.RPNPopExpression();

  OperatorItem := MatchUnarOperator(EContext.SContext, opNot, Right);
  if not Assigned(OperatorItem) then
    ERRORS.NO_OVERLOAD_OPERATOR_FOR_TYPES(opNot, Right);

  if Right.ItemType = itConst then
    Result := fCCalc.ProcessConstOperation(EContext.Scope, Right, Right, opNot)
  else begin
    var WasCall := False;
    Right := CheckAndCallFuncImplicit(EContext.SContext, Right, WasCall);

    // если это просто выражение (not Bool_Value)
    if (Right.DataTypeID = dtBoolean) then
    begin
      var TmpVar := GetTMPVar(EContext, Sys._Boolean);
      Result := TIDBoolResultExpression.Create(TmpVar, Lexer_Position);
    end else
      Result:=  GetTMPVarExpr(EContext, Right.DataType, Lexer_Position);
  end;
end;

function TASTDelphiUnit.Process_operator_Period(var EContext: TEContext): TIDExpression;
var
  ValueType: TIDDeclaration;
  RangeType: TIDRangeType;
  Decl: TIDDeclaration;
  LB, HB: TIDExpression;
  SRValue: TSubRangeRecord;
begin
  HB := EContext.RPNPopExpression();
  LB := EContext.RPNPopExpression();

  CheckEmptyExpression(LB);
  CheckEmptyExpression(HB);
  CheckConstExpression(LB);
  CheckConstExpression(HB);
  CheckOrdinalExpression(LB);
  CheckOrdinalExpression(HB);

  ValueType := MatchImplicit(LB.DataType, HB.DataType);
  if not Assigned(ValueType) then
    AbortWork(sTypesMustBeIdentical, HB.TextPosition);

  // search in the cache first
  RangeType := fCache.FindRange(ValueType as TIDType, LB, HB);
  if not Assigned(RangeType) then
  begin
    RangeType := TIDRangeType.CreateAsAnonymous(IntfScope);
    RangeType.BaseType := ValueType as TIDOrdinal;
    RangeType.LoDecl := LB.AsConst;
    RangeType.HiDecl := HB.AsConst;
    fCache.Add(RangeType);
    AddType(RangeType);
  end;

  if LB.IsConstant and HB.IsConstant then
  begin
    if TIDConstant(LB.Declaration).CompareTo(TIDConstant(HB.Declaration)) > 0 then
      AbortWork(sLowerBoundExceedsHigherBound, HB.TextPosition);
  end;

  SRValue.LBExpression := LB;
  SRValue.HBExpression := HB;
  Decl := TIDRangeConstant.CreateAsAnonymous(IntfScope, RangeType, SRValue);
  Result := TIDExpression.Create(Decl, HB.TextPosition);
end;

procedure TASTDelphiUnit.CheckIncompletedProcs;
begin
//  inherited;

end;

procedure TASTDelphiUnit.CheckLabelExpression(const Expr: TIDExpression);
begin
  if Expr.ItemType <> itLabel then
    AbortWork('LABEL required', Expr.TextPosition);
end;

procedure TASTDelphiUnit.CheckLabelExpression(const Decl: TIDDeclaration);
begin
  if Decl.ItemType <> itLabel then
    AbortWork('LABEL required', Lexer_Position);
end;

function TASTDelphiUnit.Compile(ACompileIntfOnly: Boolean; RunPostCompile: Boolean): TCompilerResult;
var
  Token: TTokenID;
  Scope: TScope;
begin
  try
    Scope := nil;
    if UnitState = UnitNotCompiled then
    begin
      fTotalLinesParsed := 0;
      Progress(TASTStatusParseBegin);
      Result := inherited Compile(RunPostCompile);
      fSysDecls := TSYSTEMUnit(SysUnit).SystemDeclarations;
      fCCalc := TExpressionCalculator.Create(Self);
      Package.DoBeforeCompileUnit(Self);

      Messages.Clear;
      FRCPathCount := 1;

      Lexer.First;
      Scope := IntfScope;
      ParseUnitDecl(Scope);
    end else
    if UnitState = UnitIntfCompiled then
    begin
      Scope := ImplScope;
    end else
      AbortWorkInternal('Invalid unit state');

    Token := Lexer_NextToken(Scope);
    while true do begin
      // since this is a "root" of Delphi unit, all keywords should be treated as "reserved"
      case Lexer_AmbiguousId of
        token_type: begin
          CheckIntfSectionMissing(Scope);
          Token := ParseTypeSection(Scope);
        end;
        token_asm: begin
          CheckIntfSectionMissing(Scope);
          Token := ParseAsmSpecifier();
          case Token of
            token_function: Token := ParseProcedure(Scope, ptFunc);
            token_procedure: Token := ParseProcedure(Scope, ptProc);
            else
              ERRORS.FEATURE_NOT_SUPPORTED;
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
        token_constructor: begin
          CheckIntfSectionMissing(Scope);
          Token := ParseProcedure(Scope, ptConstructor);
        end;
        token_destructor: begin
          CheckIntfSectionMissing(Scope);
          Token := ParseProcedure(Scope, ptDestructor);
        end;
        token_const: begin
          CheckIntfSectionMissing(Scope);
          Lexer_NextToken(Scope);
          Token := ParseConstSection(Scope);
        end;
        token_resourcestring: begin
          CheckIntfSectionMissing(Scope);
          Lexer_NextToken(Scope);
          // todo: parse resourcestring
          Token := ParseConstSection(Scope);
        end;
        token_class: begin
          CheckIntfSectionMissing(Scope);
          Token := Lexer_NextReseredToken(Scope);
          case Token of
            token_function: Token := ParseProcedure(Scope, ptClassFunc);
            token_procedure: Token := ParseProcedure(Scope, ptClassProc);
            token_constructor: Token := ParseProcedure(Scope, ptClassConstructor);
            token_destructor: Token := ParseProcedure(Scope, ptClassDestructor);
            tokenD_operator: Token := ParseOperator(Scope, nil);
          else
            ERRORS.FEATURE_NOT_SUPPORTED(Lexer_TokenLexem(Token));
          end;
        end;
        token_var: begin
          CheckIntfSectionMissing(Scope);
          Lexer_NextToken(Scope);
          Token := ParseVarSection(Scope, vLocal, False);
        end;
        token_threadvar: begin
          CheckIntfSectionMissing(Scope);
          Lexer_NextToken(Scope);
          Token := ParseVarSection(Scope, vLocal, False);
        end;
        tokenD_exports: begin
          // todo:
          Token := Lexer_SkipTo(Scope, token_semicolon);
          Token := Lexer_NextToken(Scope);
        end;
        token_interface: begin
          Scope := IntfScope;
          Token := Lexer_NextToken(Scope);
        end;
        token_implementation: begin
          CheckIntfSectionMissing(Scope);
          fUnitState := UnitIntfCompiled;
          if ACompileIntfOnly then
          begin
            Package.DoFinishCompileUnit(Self, {AIntfOnly:} True);
            Progress(TASTStatusParseIntfSucess);
            Result := CompileSuccess;
            Exit;
          end;
          Scope := ImplScope;
          Token := Lexer_NextToken(Scope);
        end;
        token_end: begin
          Lexer_MatchToken(Lexer_NextToken(Scope), token_dot);
          Token := Lexer_NextToken(Scope);
          if Token <> token_eof then
            ERRORS.HINT_TEXT_AFTER_END;
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
        ERRORS.KEYWORD_EXPECTED;
      end;
    end;
    PostCompileChecks;
    fUnitState := UnitAllCompiled;
    Result := CompileSuccess;
    FCompiled := Result;
    Progress(TASTStatusParseSuccess);
    Package.DoFinishCompileUnit(Self, {AIntfOnly:} False);
  except
    on e: ECompilerStop do Exit(CompileFail);
    on e: ECompilerSkip do Exit(CompileSkip);
    on e: ECompilerAbort do begin
      PutMessage(ECompilerAbort(e).CompilerMessage^);
      Progress(TASTStatusParseFail);
      Result := CompileFail;
    end;
    on e: Exception do begin
      PutMessage(cmtInteranlError, e.Message, Lexer_Position);
      Progress(TASTStatusParseFail);
      Result := CompileFail;
    end;
  end;
end;

function TASTDelphiUnit.CompileIntfOnly: TCompilerResult;
begin
  Result := Compile({ACompileIntfOnly:} True);
end;

function TASTDelphiUnit.CompileSource(Scope: TScope; const FileName: string; const Source: string): ICompilerMessages;
var
  ParserState: TParserPosition;
  ParserSource: string;
  Token: TTokenID;
begin
  Result := Messages;
  Lexer.SaveState(ParserState);
  ParserSource := Lexer.Source;
  fIncludeFilesStack.Push(FileName);
  try
    try
      Lexer.Source := Source;
      Lexer.First;
      Token := Lexer_NextToken(Scope);
      while true do begin
        case Lexer_AmbiguousId of
          token_type: begin
            CheckIntfSectionMissing(Scope);
            Token := ParseTypeSection(Scope);
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
          token_constructor: begin
            CheckIntfSectionMissing(Scope);
            Token := ParseProcedure(Scope, ptConstructor);
          end;
          token_destructor: begin
            CheckIntfSectionMissing(Scope);
            Token := ParseProcedure(Scope, ptDestructor);
          end;
          tokenD_operator: begin
            CheckIntfSectionMissing(Scope);
            Token := ParseOperator(Scope, nil);
          end;
          token_const: begin
            CheckIntfSectionMissing(Scope);
            Lexer_NextToken(Scope);
            Token := ParseConstSection(Scope);
          end;
          token_class: begin
            CheckIntfSectionMissing(Scope);
            Token := Lexer_NextReseredToken(Scope);
            case Token of
              token_function: Token := ParseProcedure(Scope, ptClassFunc);
              token_procedure: Token := ParseProcedure(Scope, ptClassProc);
              token_constructor: Token := ParseProcedure(Scope, ptClassConstructor);
              token_destructor: Token := ParseProcedure(Scope, ptClassDestructor);
            else
              ERRORS.FEATURE_NOT_SUPPORTED;
            end;
          end;
          token_var: begin
            CheckIntfSectionMissing(Scope);
            Lexer_NextToken(Scope);
            Token := ParseVarSection(Scope, vLocal, False);
          end;
          token_interface: begin
            Scope := IntfScope;
            Token := Lexer_NextToken(Scope);
          end;
          token_implementation: begin
            CheckIntfSectionMissing(Scope);
            Scope := ImplScope;
            Token := Lexer_NextToken(Scope);
          end;
          token_end: begin
            Lexer_MatchToken(Lexer_NextToken(Scope), token_dot);
            Token := Lexer_NextToken(Scope);
            if Token <> token_eof then
              ERRORS.HINT_TEXT_AFTER_END;
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
          ERRORS.KEYWORD_EXPECTED;
        end;
      end;
    except
      on e: ECompilerAbort do PutMessage(ECompilerAbort(e).CompilerMessage^);
      on e: Exception do PutMessage(cmtInteranlError, e.Message, Lexer_Position);
    end;
  finally
    Lexer.Source := ParserSource;
    Lexer.LoadState(ParserState);
    fIncludeFilesStack.Pop;
  end;
end;

class function TASTDelphiUnit.ConstDynArrayToSet(const SContext: TSContext; const CDynArray: TIDExpression; TargetSetType: TIDSet): TIDExpression;
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
    ImplicitCast := CheckImplicit(SContext, SExpr, ItemType);
    if not Assigned(ImplicitCast) then
      SContext.ERRORS.INCOMPATIBLE_TYPES(SExpr, ItemType);

    ItemValue := SExpr.AsIntConst.Value;
    SetValue := SetValue or (1 shl ItemValue);
  end;
  Result := IntConstExpression(SContext, SetValue);
  Result.TextPosition := CDynArray.TextPosition;
end;

class function TASTDelphiUnit.MatchSetImplicit(const SContext: TSContext; Source: TIDExpression; Destination: TIDSet): TIDExpression;
var
  i: Integer;
  CArray: TIDDynArrayConstant;
  SExpr: TIDExpression;
  ImplicitCast: TIDDeclaration;
  EnumDecl: TIDType;
  ItemValue, SetValue: Int64;
begin
  SetValue := 0;
  EnumDecl := Destination.BaseType;
  CArray := Source.AsDynArrayConst;
  for i := 0 to CArray.ArrayLength - 1 do begin
    SExpr := CArray.Value[i];
    ImplicitCast := CheckImplicit(SContext, SExpr,  EnumDecl);
    if not Assigned(ImplicitCast) then
      SContext.ERRORS.INCOMPATIBLE_TYPES(SExpr, EnumDecl);

    ItemValue := SExpr.AsIntConst.Value;

    SetValue := SetValue or (1 shl ItemValue);
  end;
  var Scope := TASTDelphiUnit(SContext.Module).ImplScope;

  Result := TIDExpression.Create(TIDIntConstant.CreateAsAnonymous(Scope, SContext.SysUnit._Int32, SetValue), Source.TextPosition);
end;

function TASTDelphiUnit.GetBuiltins: IDelphiBuiltInTypes;
begin
  Result := fSysDecls;
end;

function TASTDelphiUnit.GetCurrentParsedFileName(OnlyFileName: Boolean): string;
begin
  if fIncludeFilesStack.Count > 0 then
    Result := fIncludeFilesStack.Top
  else
    Result := FileName;
  if OnlyFileName then
    Result := ExtractFileName(Result);
end;

function TASTDelphiUnit.GetErrors: TASTDelphiErrors;
begin
  Result := fErrors;
end;

procedure TASTDelphiUnit.EnumDeclarations(const AEnumProc: TEnumASTDeclProc; AUnitScope: TUnitScopeKind);
begin
  if AUnitScope in [scopeBoth, scopeInterface] then
    for var LIndex := 0 to IntfScope.Count - 1 do
      AEnumProc(Self, IntfScope.Items[LIndex]);

  if AUnitScope in [scopeBoth, scopeImplementation] then
    for var LIndex := 0 to ImplScope.Count - 1 do
      AEnumProc(Self, ImplScope.Items[LIndex]);
end;

class function TASTDelphiUnit.GetTMPVar(const EContext: TEContext; DataType: TIDType): TIDVariable;
begin
  Result := TIDProcedure(EContext.Proc).GetTMPVar(DataType);
end;

function TASTDelphiUnit.GetSource: string;
begin
  Result := Lexer.Source;
end;

class function TASTDelphiUnit.GetStaticTMPVar(DataType: TIDType; VarFlags: TVariableFlags): TIDVariable;
begin
  Result := TIDVariable.CreateAsTemporary(nil, DataType);
  Result.IncludeFlags(VarFlags);
end;

function TASTDelphiUnit.GetSystemDeclarations: PDelphiSystemDeclarations;
begin
  Result := fSysDecls;
end;

class function TASTDelphiUnit.GetTMPVar(const SContext: TSContext; DataType: TIDType): TIDVariable;
begin
  if Assigned(SContext.Proc) then
    Result := SContext.Proc.GetTMPVar(DataType)
  else
    Result := GetStaticTMPVar(DataType);
end;

class function TASTDelphiUnit.GetTMPVarExpr(const EContext: TEContext; DataType: TIDType; const TextPos: TTextPosition): TIDExpression;
begin
  Result := TIDExpression.Create(GetTMPVar(EContext, DataType), TextPos);
end;

class function TASTDelphiUnit.GetTMPVarExpr(const SContext: TSContext; DataType: TIDType; const TextPos: TTextPosition): TIDExpression;
begin
  Result := TIDExpression.Create(GetTMPVar(SContext, DataType), TextPos);
end;

class function TASTDelphiUnit.GetTMPRef(const SContext: TSContext; DataType: TIDType): TIDVariable;
begin
  Result := SContext.Proc.GetTMPRef(DataType);
end;

class function TASTDelphiUnit.GetTMPRefExpr(const SContext: TSContext; DataType: TIDType; const TextPos: TTextPosition): TIDExpression;
begin
  Result := TIDExpression.Create(GetTMPRef(SContext, DataType));
  Result.TextPosition := TextPos;
end;

class function TASTDelphiUnit.GetTMPRefExpr(const SContext: TSContext; DataType: TIDType): TIDExpression;
begin
  Result := TIDExpression.Create(GetTMPRef(SContext, DataType));
end;

procedure TASTDelphiUnit.InitEContext(out EContext: TEContext; const SContext: TSContext; EPosition: TExpessionPosition);
begin
  EContext.Initialize(SContext, Process_operators);
  EContext.EPosition := EPosition;
end;

function TASTDelphiUnit.IsConstEqual(const Left, Right: TIDExpression): Boolean;
var
  RExpr: TIDExpression;
begin
  RExpr := fCCalc.ProcessConstOperation(IntfScope, Left, Right, opEqual);
  Result := TIDBooleanConstant(RExpr.Declaration).Value;
end;

function TASTDelphiUnit.IsConstRangesIntersect(const Left, Right: TIDRangeConstant): Boolean;
var
  Expr,
  LeftLB, LeftHB,
  RightLB, RightHB: TIDExpression;
begin
  LeftLB := Left.Value.LBExpression;
  LeftHB := Left.Value.HBExpression;

  RightLB := Right.Value.LBExpression;
  RightHB := Right.Value.HBExpression;

  Expr := fCCalc.ProcessConstOperation(IntfScope, LeftLB, RightLB, opLess);
  // если Left.Low < Right.Low
  if TIDBooleanConstant(Expr.Declaration).Value then
  begin
    Expr := fCCalc.ProcessConstOperation(IntfScope, LeftHB, RightLB, opGreaterOrEqual);
    Result := TIDBooleanConstant(Expr.Declaration).Value;
  end else begin
    Expr := fCCalc.ProcessConstOperation(IntfScope, RightHB, LeftLB, opGreaterOrEqual);
    Result := TIDBooleanConstant(Expr.Declaration).Value;
  end;
end;

function TASTDelphiUnit.IsConstValueInRange(Value: TIDExpression; RangeExpr: TIDRangeConstant): Boolean;
var
  Expr: TIDExpression;
begin
  Expr := fCCalc.ProcessConstOperation(IntfScope, Value, RangeExpr.Value.LBExpression, opLess);
  if TIDBooleanConstant(Expr.Declaration).Value then
    Exit(False);

  Expr := fCCalc.ProcessConstOperation(IntfScope, Value, RangeExpr.Value.HBExpression, opLessOrEqual);
  Result := TIDBooleanConstant(Expr.Declaration).Value;
end;

function TASTDelphiUnit.IsStructsMethod(AStruct: TIDStructure; AProc: TIDProcedure): Boolean;
begin
  Result := Assigned(AStruct) and
            Assigned(AProc.Struct) and
            ((AStruct = AProc.Struct) or
             (AStruct.IsInheritsForm(AProc.Struct)));
end;

procedure TASTDelphiUnit.InsertToScope(Scope: TScope; Item: TIDDeclaration);
begin
  if not Scope.InsertID(Item) then
    ERRORS.ID_REDECLARATED(Item);
end;

procedure TASTDelphiUnit.InsertToScope(Scope: TScope; const ID: string; Declaration: TIDDeclaration);
begin
  if Assigned(Scope.InsertNode(ID, Declaration)) then
    ERRORS.ID_REDECLARATED(Declaration);
end;

{parser methods}

procedure TASTDelphiUnit.Lexer_ReadTokenAsID(var Identifier: TIdentifier);
begin
  with Lexer do begin
    if TokenCanBeID(TTokenID(CurrentTokenID)) then
    begin
      Identifier.Name := TokenLexem(TTokenID(CurrentTokenID));
      Identifier.TextPosition := Position;
    end else
      ERRORS.IDENTIFIER_EXPECTED(TTokenID(CurrentTokenID));
  end;
end;

procedure TASTDelphiUnit.Lexer_ReadNextIdentifier(Scope: TScope; out Identifier: TIdentifier);
var
  Token: TTokenID;
begin
  Token := Lexer_NextToken(Scope);
  if Token = token_Identifier then
    Lexer.GetIdentifier(Identifier)
  else
    Lexer_ReadTokenAsID(Identifier);
end;

procedure TASTDelphiUnit.CheckCorrectEndCondStatemet(var Token: TTokenID);
var
  Found: Boolean;
begin
  Found := False;
  while (Token < token_cond_define) and (Token <> token_eof) and (Token <> token_closefigure) do
  begin
    if not Found then
    begin
      Warning('Invalid chars in conditional statement: %s', [Lexer_TokenLexem(Token)], Lexer_Position);
      Found := True;
    end;
    Token := Lexer.NextToken;
  end;
end;

procedure TASTDelphiUnit.Lexer_ReadNextIFDEFLiteral(Scope: TScope; out Identifier: TIdentifier);
var
  Token: TTokenID;
begin
  Lexer_ReadNextIdentifier(Scope, Identifier);
  Token := Lexer.NextToken;
  CheckCorrectEndCondStatemet(Token);
  Lexer_MatchToken(Token, token_closefigure);
end;

procedure TASTDelphiUnit.Lexer_ReadSemicolon(Scope: TScope);
begin
  if Lexer_NextToken(Scope) <> token_semicolon then
    ERRORS.SEMICOLON_EXPECTED;
end;

function TASTDelphiUnit.Lexer_ReadSemicolonAndToken(Scope: TScope): TTokenID;
begin
  Result := Lexer_NextToken(Scope);
  if Result = token_semicolon then
    Result := Lexer_NextToken(Scope);
end;

procedure TASTDelphiUnit.Lexer_ReadToken(Scope: TScope; const Token: TTokenID);
var
  curToken: TTokenID;
begin
  curToken := Lexer_NextToken(Scope);
  if curToken <> Token then
    ERRORS.EXPECTED_TOKEN(Token, curToken);
end;

procedure TASTDelphiUnit.Lexer_MatchToken(const ActualToken, ExpectedToken: TTokenID);
begin
  if ActualToken <> ExpectedToken then
    ERRORS.EXPECTED_TOKEN(ExpectedToken, ActualToken);
end;

procedure TASTDelphiUnit.Lexer_MatchCurToken(const ExpectedToken: TTokenID);
begin
  if Lexer_CurTokenID <> ExpectedToken then
    ERRORS.EXPECTED_TOKEN(ExpectedToken, Lexer_CurTokenID);
end;

procedure TASTDelphiUnit.Lexer_MatchIdentifier(const ActualToken: TTokenID);
begin
  if ActualToken <> token_Identifier then
    if not Lexer.TokenCanBeID(ActualToken) then
      ERRORS.IDENTIFIER_EXPECTED(ActualToken);
end;

procedure TASTDelphiUnit.Lexer_MatchParamNameIdentifier(ActualToken: TTokenID);
begin
  if ActualToken <> token_Identifier then
    ERRORS.PARAM_NAME_ID_EXPECTED(ActualToken);
end;

procedure TASTDelphiUnit.Lexer_MatchSemicolon(const ActualToken: TTokenID);
begin
  if ActualToken <> token_semicolon then
    ERRORS.SEMICOLON_EXPECTED;
end;

procedure TASTDelphiUnit.Lexer_ReadCurrIdentifier(var Identifier: TIdentifier);
begin
  if Lexer_CurTokenID = token_identifier then
    Lexer.GetIdentifier(Identifier)
  else
    Lexer.GetTokenAsIdentifier(Identifier);
end;

function TASTDelphiUnit.Lexer_NextToken(Scope: TScope): TTokenID;
begin
  Result := TTokenID(Lexer.NextToken);
  if Result >= token_cond_define then
    Result := ParseCondStatements(Scope, Result);
end;

function TASTDelphiUnit.Lexer_NextReseredToken(Scope: TScope): TTokenID;
begin
  Lexer_NextToken(Scope);
  Result := TTokenID(Lexer.AmbiguousTokenID);
end;

function TASTDelphiUnit.Lexer_TreatTokenAsToken(AToken: TTokenID): TTokenID;
begin
  Result := TTokenID(Lexer.AmbiguousTokenId);
end;

function TASTDelphiUnit.Lexer_AmbiguousId: TTokenID;
begin
  Result := TTokenID(Lexer.AmbiguousTokenID);
end;

function TASTDelphiUnit.Lexer_CurTokenID: TTokenID;
begin
  Result := TTokenID(Lexer.CurrentTokenID);
end;

function TASTDelphiUnit.Lexer_Position: TTextPosition;
begin
  Result := Lexer.Position;
end;

function TASTDelphiUnit.Lexer_PrevPosition: TTextPosition;
begin
  Result := Lexer.PrevPosition;
end;

function TASTDelphiUnit.Lexer_IdentifireType: TIdentifierType;
begin
  Result := Lexer.IdentifireType;
end;

function TASTDelphiUnit.Lexer_IsCurrentIdentifier: boolean;
begin
  Result := Lexer.IdentifireType = itIdentifier;
end;

function TASTDelphiUnit.Lexer_IsCurrentToken(TokenID: TTokenID): boolean;
begin
  Result := Lexer.AmbiguousTokenId = Ord(TokenID);
end;

function TASTDelphiUnit.Lexer_NotEOF: boolean;
begin
  Result := Lexer_CurTokenID <> token_eof;
end;

function TASTDelphiUnit.Lexer_Original: string;
begin
  Result := Lexer.OriginalToken;
end;

function TASTDelphiUnit.Lexer_Line: Integer;
begin
  Result := Lexer.Position.Row;
end;

function TASTDelphiUnit.Lexer_TokenLexem(const TokenID: TTokenID): string;
begin
  Result := Lexer.TokenLexem(TokenID);
end;

function TASTDelphiUnit.Lexer_SkipBlock(StopToken: TTokenID): TTokenID;
var
  ECnt: Integer;
begin
  ECnt := 0;
  while True do begin
    Result := Lexer.NextToken;
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

function TASTDelphiUnit.Lexer_SkipTo(Scope: TScope; StopToken: TTokenID): TTokenID;
begin
  while true do begin
    Result := Lexer_NextToken(Scope);
    if (Result = StopToken) or (Result = token_eof) then
      Exit;
  end;
end;

procedure TASTDelphiUnit.PutMessage(MessageType: TCompilerMessageType; const MessageText: string);
var
  SourcePosition: TTextPosition;
  Msg: TCompilerMessage;
begin
  SourcePosition.Row := -1;
  SourcePosition.Col := -1;
  Msg := TCompilerMessage.Create(Self, MessageType, MessageText, SourcePosition);
  Msg.UnitName := _ID.Name;
  Messages.Add(Msg);
  fPackage.PutMessage(Msg);
end;

procedure TASTDelphiUnit.PutMessage(MessageType: TCompilerMessageType; const MessageText: string; const SourcePosition: TTextPosition);
var
  Msg: TCompilerMessage;
begin
  Msg := TCompilerMessage.Create(Self, MessageType, MessageText, SourcePosition);
  Msg.UnitName := GetCurrentParsedFileName(True);
  Messages.Add(Msg);
  fPackage.PutMessage(Msg);
end;

procedure TASTDelphiUnit.PutMessage(Message: TCompilerMessage);
begin
  Message.UnitName := Self.Name;
  if Message.Row <=0 then
  begin
    Message.Row := Lexer_Position.Row;
    Message.Col := Lexer_Position.Col;
  end;
  Messages.Add(Message);
  fPackage.PutMessage(Message);
end;

procedure TASTDelphiUnit.Error(const Message: string; const Params: array of const; const TextPosition: TTextPosition);
begin
  PutMessage(cmtError, Format(Message, Params), TextPosition);
end;

procedure TASTDelphiUnit.Warning(const Message: string; const Params: array of const; const TextPosition: TTextPosition);
begin
  PutMessage(cmtWarning, Format(Message, Params), TextPosition);
end;

procedure TASTDelphiUnit.Hint(const Message: string; const Params: array of const; const TextPosition: TTextPosition);
begin
  PutMessage(cmtHint, Format(Message, Params), TextPosition);
end;

procedure TASTDelphiUnit.Hint(const Message: string; const Params: array of const);
begin
  PutMessage(cmtHint, Format(Message, Params));
end;

function TASTDelphiUnit.FindAll(Scope: TScope; const ID: TIdentifier): TIDDeclArray;
begin
  Result := [];
  Scope.FindIDRecurcive(ID.Name, {var} Result);
  if not Assigned(Result) then
  begin
    // for debug:
    Scope.FindIDRecurcive(ID.Name, {var} Result);
    ERRORS.UNDECLARED_ID(ID);
  end;
end;

function TASTDelphiUnit.FindID(Scope: TScope; const ID: TIdentifier{; out Expression: TIDExpression}): TIDDeclaration;
var
  i: Integer;
  IDName: string;
begin
  IDName := ID.Name;
  Result := Scope.FindIDRecurcive(IDName);
  if not Assigned(Result) then
    ERRORS.UNDECLARED_ID(ID);
end;

function TASTDelphiUnit.FindIDNoAbort(Scope: TScope; const ID: string): TIDDeclaration;
begin
  Result := Scope.FindIDRecurcive(ID);
end;

function TASTDelphiUnit.FindIDNoAbort(Scope: TScope; const ID: TIdentifier): TIDDeclaration;
begin
  Result := Scope.FindIDRecurcive(ID.Name);
end;

procedure TASTDelphiUnit.AddType(const Decl: TIDType);
begin
  if not (Decl is TIDAliasType) and not Decl.IsPooled then
  begin
    Decl.IsPooled := True;
  end;
end;

class function TASTDelphiUnit.MatchImplicit(Source, Destination: TIDType): TIDDeclaration;
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

function TASTDelphiUnit.MatchImplicit3(const SContext: TSContext; Source: TIDExpression; Dest: TIDType;
                                       AAbortIfError: Boolean = True): TIDExpression;
var
  WasCall: Boolean;
begin
  Result := MatchImplicitOrNil(SContext, Source, Dest);
  if Assigned(Result) then
    Exit;
  WasCall := False;
  Source := CheckAndCallFuncImplicit(SContext, Source, {out} WasCall);
  if WasCall then
  begin
    Result := MatchImplicitOrNil(SContext, Source, Dest);
    if Assigned(Result) then
      Exit;
  end;

  if AAbortIfError then
  begin
    MatchImplicitOrNil(SContext, Source, Dest);
    ERRORS.INCOMPATIBLE_TYPES(Source, Dest);
  end;
end;

class function TASTDelphiUnit.MatchImplicitClassOf(Source: TIDExpression; Destination: TIDClassOf): TIDDeclaration;
var
  SrcDecl: TIDDeclaration;
  DstDecl: TIDClass;
begin
  SrcDecl := Source.Declaration;
  DstDecl := TIDClass(Destination.ReferenceType);
  // Src is a type
  if SrcDecl.ItemType = itType then begin
    if SrcDecl is TIDStructure then
    begin
      if (SrcDecl = DstDecl) or TIDStructure(SrcDecl).IsInheritsForm(DstDecl) then
        Exit(Destination);
    end else
    // Src is a generic parameter
    if (SrcDecl is TIDGenericParam) and
       Assigned(TIDGenericParam(SrcDecl).ConstraintType) and
       TIDStructure(TIDGenericParam(SrcDecl).ConstraintType).IsInheritsForm(DstDecl) then
      Exit(Destination)
    else
      AbortWork(sCLASSTypeRequired, Source.TextPosition);
  end else
  // Src is a variable
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

function TASTDelphiUnit.MatchArrayImplicit(const SContext: TSContext; Source: TIDExpression; DstArray: TIDArray): TIDExpression;

  function CreateAnonymousDynArrayType(ElementDataType: TIDType): TIDArray;
  begin
    Result := TIDDynArray.CreateAsAnonymous(ImplScope);
    Result.ElementDataType := ElementDataType;
    AddType(Result);
  end;

var
  i, ACnt: Integer;
  CArray: TIDDynArrayConstant;
  SExpr: TIDExpression;
  DstElementDT: TIDType;
  BoundType: TIDOrdinal;
  NeedCallImplicits: Boolean;
  ImplicitCast: TIDDeclaration;
  TmpArrayType: TIDArray;
  Expr: TIDExpression;
  EContext: TEContext;
  SrcArray: TIDArray;
begin
  Result := nil;
  SrcArray := Source.DataType as TIDArray;

  // skip any checks if the parameter is open array of const
  if DstArray.IsOpenArrayOfConst then
    Exit(Source);

  if Source.Declaration is TIDDynArrayConstant then
  begin
    CArray := Source.AsArrayConst;
    DstElementDT := DstArray.ElementDataType;

    // проверка каждого элемента
    NeedCallImplicits := False;
    ACnt := CArray.ArrayLength;
    for i := 0 to ACnt - 1 do begin
      SExpr := CArray.Value[i];
      ImplicitCast := CheckImplicit(SContext, SExpr, DstElementDT);
      if not Assigned(ImplicitCast) then
        ERRORS.INCOMPATIBLE_TYPES(SExpr, DstElementDT);

      if ImplicitCast.ItemType = itProcedure then
        NeedCallImplicits := True;
    end;

    {если тип вектора не совподает с типом приемника, подгоняем тип под нужный}
    if TIDArray(CArray.DataType).ElementDataType <> DstElementDT then
      CArray.DataType := CreateAnonymousDynArrayType(DstElementDT);

    // если типы массивов требуют вызовов пользовательских операторов implicit
    // то создаем временный статический массив, и для каждого элемента вызываем пользовательский implicit
    if NeedCallImplicits then
    begin
      if DstArray.DataTypeID = dtOpenArray then
      begin
        // для открытых массивов создаем анонимный тип - статический массив
        TmpArrayType := TIDArray.CreateAnonymousStatic1Dim(ImplScope, DstElementDT, ACnt, BoundType);
        AddType(BoundType);
        AddType(TmpArrayType);
        Result := GetTMPVarExpr(SContext, TmpArrayType, Lexer_Position);
      end else begin
        // для динамических массивов просто выделяем память
        Result := GetTMPVarExpr(SContext, DstArray, Lexer_Position);
        //ILWrite(SContext, TIL.IL_DAlloc(Result, IntConstExpression(ACnt)));
      end;

      for i := 0 to ACnt - 1 do begin
        SExpr := CArray.Value[i];
        InitEContext(EContext, SContext, ExprNested);
        Expr := TIDMultiExpression.CreateAnonymous(DstElementDT, [Result, IntConstExpression(SContext, i)]);
        EContext.RPNPushExpression(Expr);
        EContext.RPNPushExpression(SExpr);
        Process_operator_Assign(EContext);
      end;
    end else
      Result := Source;
  end else begin
    // приведение динамических массивов
    if (SrcArray.DataTypeID = DstArray.DataTypeID) and (SrcArray.DimensionsCount = DstArray.DimensionsCount) then
    begin
      for i := 0 to SrcArray.DimensionsCount - 1 do
      begin
        // todo: добавить глубокую проверку типов (range/enum/...)
        if SrcArray.Dimensions[i].ActualDataType <> DstArray.Dimensions[i].ActualDataType then
          Exit(nil);
      end;
      if SrcArray.ElementDataType.ActualDataType <> DstArray.ElementDataType.ActualDataType then
        Exit(nil);
      Result := Source;
    end;
  end;
end;

function TASTDelphiUnit.MatchRecordImplicit(const SContext: TSContext; Source: TIDExpression; DstRecord: TIDRecord): TIDExpression;
var
  SrcRecord: TIDRecord;
  SrcFld, DstFld: TIDDeclaration;
begin
  SrcRecord := Source.DataType as TIDRecord;
  if SrcRecord.FieldsCount <> DstRecord.FieldsCount then
    Exit(nil);

  for var LIndex := 0 to SrcRecord.Members.Count - 1 do
  begin
    SrcFld := SrcRecord.Members.Items[LIndex];
    DstFld := DstRecord.Members.Items[LIndex];
    if SrcFld.DataType.ActualDataType <> DstFld.DataType.ActualDataType then
      Exit(nil);
  end;
  Result := Source;
end;

class function TASTDelphiUnit.MatchUnarOperator(Op: TOperatorID; Right: TIDType): TIDType;
begin
  Right := Right.ActualDataType;
  Result := Right.UnarOperator(Op, Right);
  if Assigned(Result) then
    Exit;
  if (Right.DataTypeID = dtGeneric) then
    Exit(Right);
end;

class function TASTDelphiUnit.MatchUnarOperator(const SContext: TSContext; Op: TOperatorID; Source: TIDExpression): TIDExpression;
var
  OpDecl: TIDType;
  WasCall: Boolean;
begin
  OpDecl := MatchUnarOperator(Op, Source.DataType.ActualDataType);
  if Assigned(OpDecl) then
    Exit(Source);

  Source := CheckAndCallFuncImplicit(SContext, Source, WasCall);
  if WasCall then
  begin
    OpDecl := MatchUnarOperator(Op, Source.DataType.ActualDataType);
    if Assigned(OpDecl) then
      Exit(Source);
  end;

  Result := nil;
end;

function TASTDelphiUnit.MatchImplicitOrNil(const SContext: TSContext; Source: TIDExpression; Dest: TIDType): TIDExpression;
var
  SDataType: TIDType;
  Decl: TIDDeclaration;
  SrcDTID, DstDTID: TDataTypeID;
begin
  Result := nil;
  if not Assigned(Source) or not Assigned(Source.DataType) then
    AbortWorkInternal('Source data type is not assigned', Lexer_Position);

  SDataType := Source.DataType.ActualDataType;
  Dest := Dest.ActualDataType;

  if SDataType = Dest then
    Exit(Source);

  SrcDTID := SDataType.DataTypeID;
  DstDTID := Dest.DataTypeID;

  // ищем явно определенный implicit у источника
  Decl := SDataType.GetImplicitOperatorTo(Dest);
  if Decl is TSysTypeCast then
  begin
    Result := TSysTypeCast(Decl).Match(SContext, Source, Dest);
    if Assigned(Result) then
      Exit(Result);
    Decl := nil;
  end;

  if not Assigned(Decl) then
  begin
    // ищем явно определенный implicit у приемника
    Decl := Dest.GetImplicitOperatorFrom(SDataType);
    if not Assigned(Decl) then
    begin
      // если не нашли точных имплиситов, ищем подходящий (у источника)
      Decl := SDataType.FindImplicitOperatorTo(Dest);
      if not Assigned(Decl) then
      begin
        // если не нашли точных имплиситов, ищем подходящий (у приемника)
        Decl := Dest.FindImplicitOperatorFrom(SDataType);
        if not Assigned(Decl) then
        begin
          { если классы и интерфейс }
          if (SrcDTID = dtClass) and (DstDTID = dtInterface) then
          begin
            if TIDClass(Source.DataType).FindInterface(TIDInterface(Dest), {AFindInAncestors:} True) then
              Exit(Source)
            else begin
              // for debug
              TIDClass(Source.DataType).FindInterface(TIDInterface(Dest));
              ERRORS.CLASS_NOT_IMPLEMENT_INTF(Source, Dest);
            end;
          end;

          { есди приемник - class of }
          if DstDTID = dtClassOf then
            Decl := MatchImplicitClassOf(Source, TIDClassOf(Dest));

// todo: not needed?
//          {дин. массив как набор}
//          if (DstDTID = dtSet) and (SDataType is TIDDynArray) then
//          begin
//            Result := MatchSetImplicit(SContext, Source, TIDSet(Dest));
//            Exit;
//          end else
//          if (SrcDTID = dtProcType) and (DstDTID = dtProcType) then
//            Decl := MatchProcedureTypes(TIDProcType(SDataType), TIDProcType(Dest))
//          else
//          if SDataType is TIDArray then
//          begin
//            if Dest is TIDArray then
//              Result := MatchArrayImplicit(SContext, Source, TIDArray(Dest))
//            else
////            if DstDTID = dtRecord then
////              Result := MatchArrayImplicitToRecord(SContext, Source, TIDStructure(Dest))
////            else
//              Result := nil;
//            Exit;
//          end;
//          if (SrcDTID = dtRecord) and (DstDTID = dtRecord) then
//          begin
//            Result := MatchRecordImplicit(SContext, Source, TIDRecord(Dest));
//            Exit;
//          end;
        end else
          Exit(Source);
      end;
    end else
    if Decl is TSysTypeCast then
    begin
      Result := TSysTypeCast(Decl).Match(SContext, Source, Dest);
      if Assigned(Result) then
        Exit;
    end else
      Exit(Source);
  end;

  {generic case}
  // todo: constraint check is needed
  if (SrcDTID = dtGeneric) or (DstDTID = dtGeneric) then
    Exit(Source);

  if Source.ClassType = TIDDrefExpression then
  begin
    Result := GetTMPVarExpr(SContext, Source.DataType, Lexer_Position);
    Exit;
  end;

//  if Assigned(SDataType.SysImplicitToAny) then
//  begin
//    Decl := TSysOpImplicit(SDataType.SysImplicitToAny).Check(SContext, Source, Dest);
//    if Assigned(Decl) then
//      Exit(Source);
//    Decl := nil;
//  end;

//  if not Assigned(Decl) then
//  begin
//    if Assigned(Dest.SysImplicitFromAny) then
//    begin
//      Decl := TSysOpImplicit(Dest.SysImplicitFromAny).Check(SContext, Source, Dest);
//      if Assigned(Decl) then
//        Exit(Source);
//    end;
//    Exit(nil);
//  end;

  if Assigned(Decl) and (Decl.ItemType = itType) then
    Exit(Source);

  Result := nil;
end;

procedure TASTDelphiUnit.MatchProc(const SContext: TSContext; CallExpr: TIDExpression;
                                   const ProcParams: TIDParamArray;
                                   var CallArgs: TIDExpressions);
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
    ERRORS.TOO_MANY_ACTUAL_PARAMS(CallArgs[pc], pc, CallArgsCount);

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
        if (Arg.ItemType = itVar) and (Arg.DataType = Sys._Void) then
        begin
          if VarOut in Param.Flags then
            Arg.AsVariable.DataType := Param.DataType
          else
            ERRORS.INPLACEVAR_ALLOWED_ONLY_FOR_OUT_PARAMS(Arg.AsVariable);
        end;

        Implicit := CheckImplicit(SContext, Arg, Param.DataType, {AResolveCalls:} True);
        if Assigned(Implicit) then
        begin
          if Param.VarReference then
          begin
            CheckVarExpression(Arg, vmpPassArgument);
            {проверка на строгость соответствия типов}
            if Param.DataType.ActualDataType <> Arg.DataType.ActualDataType then
              CheckVarParamConformity(Param, Arg);
          end;
          continue;
        end;

        // for debug
        Implicit := CheckImplicit(SContext, Arg, Param.DataType, {AResolveCalls:} True);

        ERRORS.INCOMPATIBLE_TYPES(Arg, Param.DataType);
      end else
      if not Assigned(Param.DefaultValue) then
        ERRORS.NOT_ENOUGH_ACTUAL_PARAMS(CallExpr);
    end;
  end;
end;

class function TASTDelphiUnit.MatchProcedureTypes(Src, Dst: TIDProcType): TIDType;
begin
  if StrictMatchProcSingnatures(Src.Params, Dst.Params, Src.ResultType, Dst.ResultType) then
    Result := Dst
  else
    Result := nil;
end;

function GetProcDeclSring(const Params: TIDParamArray; ResultType: TIDtype): string;
var
  sProcParams,
  sProcParamName: string;
begin
  if Assigned(ResultType) then
    Result := 'function'
  else
    Result := 'procedure';

  for var LParam in Params do
  begin
    sProcParamName := LParam.DisplayName + ': ' + LParam.DataType.DisplayName;
    sProcParams := AddStringSegment(sProcParams, sProcParamName, '; ');
  end;

  if sProcParams <> '' then
    Result := Result + '(' + sProcParams + ')';

  if Assigned(ResultType) then
    Result := Result + ': ' + ResultType.DisplayName;
end;

procedure TASTDelphiUnit.MatchPropGetter(Prop: TIDProperty; Getter: TIDProcedure; const PropParams: TIDParamArray);
begin
  var LParamCount := Length(PropParams);
  if Getter.ParamsCount <> LParamCount then
    ERRORS.GETTER_MUST_BE_SUCH(Getter, GetProcDeclSring(PropParams, Prop.DataType));

  for var LParamIndex := 0 to LParamCount - 1 do
  begin
    var LGetterParam := Getter.ExplicitParams[LParamIndex];
    var LPropertyParam := PropParams[LParamIndex];
    if MatchImplicit(LPropertyParam.DataType, LGetterParam.DataType) = nil then
      ERRORS.GETTER_MUST_BE_SUCH(Getter, GetProcDeclSring(PropParams, Prop.DataType));
  end;
end;

procedure TASTDelphiUnit.MatchPropSetter(Prop: TIDProperty; Setter: TIDExpression; const PropParams: TIDParamArray);
var
  Proc: TIDProcedure;
  LSetterParam, LPropertryParam: TIDParam;
begin
  var LParamCount := Length(PropParams);
  // due to possible overloaded setters (getter can't be overloaded), serach for the proper setter
  Proc := Setter.AsProcedure;
  while Assigned(Proc) do
  begin
    if not Assigned(Proc.ResultType) and (Proc.ParamsCount = (LParamCount + 1)) then
    begin
      var IsMatch := True;
      // match indexed params first
      for var LParamIndex := 0 to LParamCount - 1 do
      begin
        LSetterParam := Proc.ExplicitParams[LParamIndex];
        LPropertryParam := PropParams[LParamIndex];
        if MatchImplicit(LPropertryParam.DataType, LSetterParam.DataType) = nil then
        begin
          IsMatch := False;
          Break;
        end;
      end;
      if IsMatch then
      begin      
        // match property setter value param 
        LSetterParam := Proc.ExplicitParams[LParamCount];
        if MatchImplicit(LSetterParam.DataType, Prop.DataType) <> nil then
          Exit;
      end;
    end;
    Proc := Proc.PrevOverload;
  end;
  if not Assigned(Proc) then
    ERRORS.SETTER_MUST_BE_SUCH(GetProcDeclSring(PropParams, nil), Setter.TextPosition);
end;

function TASTDelphiUnit.MatchBinarOperator(const SContext: TSContext; Op: TOperatorID; var Left, Right: TIDExpression): TIDDeclaration;
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

  Result := LeftDT.SysBinayOperator[Op];
  if Assigned(Result) then
    Exit;

  Result := RightDT.SysBinayOperator[Op];
  if Assigned(Result) then
    Exit;

  if (LeftDT.DataTypeID = dtGeneric) then
    Exit(LeftDT);

  if RightDT.DataTypeID = dtGeneric then
    Exit(RightDT);

  if (Op = opIn) and (Right.Declaration.ClassType = TIDRangeConstant) then
  begin
    Result := MatchOperatorIn(SContext, Left, Right);
    Exit;
  end;

  if (LeftDT.DataTypeID = dtClass) and
     (RightDT.DataTypeID = dtClass) then
  begin
    if TIDClass(LeftDT).IsInheritsForm(TIDClass(RightDT)) then
      Exit(Sys._Boolean);
    if TIDClass(RightDT).IsInheritsForm(TIDClass(LeftDT)) then
      Exit(Sys._Boolean);
  end;

  // если ненайдено напрямую - ищем через неявное привидение
  L2RImplicit := CheckImplicit(SContext, Left, RightDT);
  R2LImplicit := CheckImplicit(SContext, Right, LeftDT);
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

class function TASTDelphiUnit.MatchOperatorIn(const SContext: TSContext; const Left, Right: TIDExpression): TIDDeclaration;
var
  RangeConst: TIDRangeConstant;
  DataType: TIDType;
begin
  RangeConst := Right.Declaration as TIDRangeConstant;

  DataType := RangeConst.Value.LBExpression.DataType;
  Result := CheckImplicit(SContext, Left, DataType);
  if Assigned(Result) then
  begin
    DataType := RangeConst.Value.HBExpression.DataType;
    Result := CheckImplicit(SContext, Left, DataType);
  end;
  // пока без проверки на пользовательские операторы
end;

function TASTDelphiUnit.MatchOverloadProc(const SContext: TSContext; Item: TIDExpression; const CallArgs: TIDExpressions; CallArgsCount: Integer): TIDProcedure;

  procedure AbortWithAmbiguousOverload(AmbiguousCnt: Integer; AmbiguousRate: Integer);
  var
    Str, Args: string;
    ProcItem: PASTProcMatchItem;
  begin
    for var i := 0 to AmbiguousCnt - 1 do
    begin
      ProcItem := Addr(fProcMatches[i]);
      if ProcItem.TotalRate = AmbiguousRate then
        Str := Str + #13#10'  ' + ProcItem.Decl.Module.Name + '.' + ProcItem.Decl.DisplayName;
    end;
    for var i := 0 to CallArgsCount - 1 do
      Args := AddStringSegment(Args, CallArgs[i].DataType.DisplayName, ', ');

    Str := Str + #13#10'  Arguments: (' + Args + ')';

    // just warning, to not block parsing process
    // todo: AST parser doesn't have ability to resolve an ambiguous
    // in case argument array [..] of Char and params (1. PAnsiChar, 2. untyped ref)
    // Warning(sAmbiguousOverloadedCallFmt, [Str], Item.TextPosition);
  end;

const
  cRateFactor = 1000000;                // multiplication factor for total rate calculdation
var
  i,
  MatchedCount: Integer;                // Кол-во деклараций имеющих одинаковый максимальный коэффициент
  ParamDataType,                        // Тип формального параметра процедуры
  ArgDataType  : TIDType;               // Тип передаваемого аргумента
  Param: TIDVariable;
  ImplicitCast: TIDDeclaration;
  Declaration: TIDProcedure;
  SrcDataTypeID,
  DstDataTypeID: TDataTypeID;
  curProcMatchItem: PASTProcMatchItem;
  cutArgMatchItem: PASTArgMatchInfo;
  curLevel: TASTArgMatchLevel;
  curRate: TASTArgMatchRate;
begin
  Result := nil;
  MatchedCount := 0;
  Declaration := TIDProcedure(Item.Declaration);
  repeat
    if (Declaration.ParamsCount = 0) and (CallArgsCount = 0) then
      Exit(Declaration);

    // check and grow
    if MatchedCount >= Length(fProcMatches) then
      SetLength(fProcMatches, MatchedCount + 4);

    curProcMatchItem := Addr(fProcMatches[MatchedCount]);
    if CallArgsCount <= Declaration.ParamsCount then
    begin
      for i := 0 to Declaration.ParamsCount - 1 do begin
        Param := Declaration.ExplicitParams[i];
        // if cur arg presents
        if (i < CallArgsCount) and Assigned(CallArgs[i]) then
        begin
          ParamDataType := Param.DataType.ActualDataType;
          ArgDataType := CallArgs[i].DataType.ActualDataType;

          curRate := 0;
          curLevel := MatchNone;
          // сравнение типов формального параметра и аргумента (пока не учитываются модификаторы const, var... etc)
          if ParamDataType.DataTypeID = dtGeneric then
            curLevel := TASTArgMatchLevel.MatchGeneric
          else
          if ParamDataType = ArgDataType then
            curLevel := TASTArgMatchLevel.MatchStrict
          else begin
            // find implicit type cast
            ImplicitCast := CheckImplicit(SContext, CallArgs[i], ParamDataType, {AResolveCalls:} True);
            if Assigned(ImplicitCast) then
            begin
              SrcDataTypeID := ArgDataType.DataTypeID;
              DstDataTypeID := ParamDataType.DataTypeID;
              if SrcDataTypeID = DstDataTypeID then
              begin
                curLevel := TASTArgMatchLevel.MatchImplicit;
                curRate := 10;
              end else begin
                  var dataLoss: Boolean;
                  curRate := GetImplicitRate(SrcDataTypeID, DstDataTypeID, {out} dataLoss);  // todo
                  if dataLoss then
                    curLevel := TASTArgMatchLevel.MatchImplicitAndDataLoss
                  else
                    curLevel := TASTArgMatchLevel.MatchImplicit;
              end;
            end;

            // if any arg doesn't match - skip this declaration
            if curLevel = MatchNone then
              Break;

          end;
        end else begin
          // if the arg is missed and the param has a default value
          if VarHasDefault in param.Flags then
            curLevel := TASTArgMatchLevel.MatchStrict
          else begin
            // if not, skip this declaration
            curLevel := MatchNone;
            Break;
          end;
        end;
        // check and grow
        if i >= Length(curProcMatchItem.ArgsInfo) then
          SetLength(curProcMatchItem.ArgsInfo, i + 4);

        // store argument's match info into cache
        cutArgMatchItem := @curProcMatchItem.ArgsInfo[i];
        cutArgMatchItem.Level := curLevel;
        cutArgMatchItem.Rate := curRate;
      end;
    end else begin
      // skip this declaration due to params length doesn't match
      Declaration := Declaration.PrevOverload;
      Continue;
    end;

    if curLevel <> MatchNone then
    begin
      curProcMatchItem.Decl := Declaration;
      Inc(MatchedCount);
    end;
    // take next declaration
    Declaration := Declaration.PrevOverload;
  until Declaration = nil;

  if MatchedCount > 0 then
  begin
    // calculating total rates for each match
    for i := 0 to MatchedCount - 1 do
    begin
      var TotalRate: Integer := 0;
      curProcMatchItem := Addr(fProcMatches[i]);
      for var j := 0 to CallArgsCount - 1  do
      begin
        cutArgMatchItem := Addr(curProcMatchItem.ArgsInfo[j]);
        TotalRate := TotalRate + Ord(cutArgMatchItem.Level)*cRateFactor + cutArgMatchItem.Rate * 100000;
      end;
      curProcMatchItem.TotalRate := TotalRate;
    end;
    // finding the most matched or ambiguous
    var MaxRate := 0;
    var AmbiguousRate: Integer := 0;
    for i := 0 to MatchedCount - 1 do
    begin
      curProcMatchItem := Addr(fProcMatches[i]);
      if curProcMatchItem.TotalRate > MaxRate then
      begin
        MaxRate := curProcMatchItem.TotalRate;
        Result := curProcMatchItem.Decl;
        AmbiguousRate := 0;
      end else
      if curProcMatchItem.TotalRate = MaxRate then
      begin
        AmbiguousRate := curProcMatchItem.TotalRate;
      end;
    end;
    if AmbiguousRate > 0 then
      AbortWithAmbiguousOverload(MatchedCount, AmbiguousRate);
  end else
    ERRORS.NO_OVERLOAD(Item, CallArgs)
end;

function TASTDelphiUnit.MatchBinarOperatorWithImplicit(const SContext: TSContext; Op: TOperatorID; var Left,
                                                       Right: TIDexpression): TIDDeclaration;
var
  LeftDT, RightDT: TIDType;
  Operators: TBinaryOperatorsArray;
  LeftImplicit, RightImplicit, LeftBinarOp, RightBinarOp: TIDDeclaration;
  LeftImplicitFactor, RightImplicitFactor: Integer;
begin
  LeftImplicitFactor := 0;
  RightImplicitFactor := 0;
  LeftDT := Left.DataType.ActualDataType;
  RightDT := Right.DataType.ActualDataType;

  Operators := LeftDT.BinarOperators[Op];
  if Assigned(Operators) then
    LeftBinarOp := FindImplicitFormBinarOperators(Operators, LeftDT, RightDT,
                                                  {out} LeftImplicit, {out} RightImplicit,
                                                  {out} LeftImplicitFactor)
  else
    LeftBinarOp := nil;

  Operators := RightDT.BinarOperators[Op];
  if Assigned(Operators) then
    RightBinarOp := FindImplicitFormBinarOperators(Operators, LeftDT, RightDT,
                                                    {out} LeftImplicit, {out} RightImplicit,
                                                    {out} RightImplicitFactor)
  else
    RightBinarOp := nil;

  if not Assigned(LeftBinarOp) and not Assigned(RightBinarOp) then
    Exit(nil);

  if LeftImplicitFactor >= RightImplicitFactor then
  begin
    Result := LeftBinarOp;
    //Right := WriteImplicitCast(LeftImplicit, Right);
  end else begin
    Result := RightBinarOp;
    //Left := WriteImplicitCast(RightImplicit, Left);
  end;
end;

class function TASTDelphiUnit.CheckExplicit(const Scontext: TSContext; const Source, Destination: TIDType; out ExplicitOp: TIDDeclaration): Boolean;
var
  SrcDataType: TIDType;
  DstDataType: TIDType;
begin
  ExplicitOp := nil;
  SrcDataType := Source.ActualDataType;
  DstDataType := Destination.ActualDataType;
  if SrcDataType = DstDataType then
    Exit(True);

  if (SrcDataType.DataTypeID = dtClass) and (DstDataType.DataTypeID = dtClass) then
    Exit(True);

  ExplicitOp := SrcDataType.GetExplicitOperatorTo(DstDataType);
  if Assigned(ExplicitOp) then
  begin
    if ExplicitOp is TSysTypeCast then
    begin
      if TSysTypeCast(ExplicitOp).Check(SContext, SrcDataType, DstDataType) then
        Exit(True);
    end else
      Exit(True);
  end;

  ExplicitOp := DstDataType.GetExplicitOperatorFrom(SrcDataType);
  if Assigned(ExplicitOp) then
  begin
    if ExplicitOp is TSysTypeCast then
    begin
      if TSysTypeCast(ExplicitOp).Check(SContext, SrcDataType, DstDataType) then
        Exit(True);
    end else
      Exit(True);
  end;
  Result := False;
end;

class procedure TASTDelphiUnit.CheckExprHasMembers(Expression: TIDExpression);
begin
  if not (Expression.DataType is TIDStructure) and not Assigned(Expression.DataType.Helper) then
    AbortWork('Expression has no members', Expression.TextPosition);
end;

procedure TASTDelphiUnit.CheckForwardPtrDeclarations;
begin
  // search and assign reference type for forward pointer types
  // for searching use current scope only
  for var AIndex := fForwardTypes.Count - 1 downto 0 do
  begin
    var APtrDecl := fForwardTypes[AIndex];
    var ARefDecl := APtrDecl.Scope.FindID(APtrDecl.ForwardID.Name);
    if Assigned(ARefDecl) and (APtrDecl is TIDRefType) then
    begin
      if ARefDecl.ItemType <> itType then
        AbortWork(sTypeIdExpectedButFoundFmt, [APtrDecl.ForwardID.Name], APtrDecl.ForwardID.TextPosition);

      TIDRefType(APtrDecl).ReferenceType := TIDType(ARefDecl);
      fForwardTypes.Delete(AIndex);
    end;
  end;
end;

class function TASTDelphiUnit.MatchExplicit(const SContext: TSContext; const Source: TIDExpression; Destination: TIDType; out Explicit: TIDDeclaration): TIDExpression;
var
  SrcDataType: TIDType;
  DstDataType: TIDType;
begin
  SrcDataType := Source.DataType.ActualDataType;
  DstDataType := Destination.ActualDataType;

  if CheckExplicit(SContext, SrcDataType, DstDataType, Explicit) then
    Result := TIDCastExpression.Create(Source, DstDataType)
  else begin
    Result := nil;
    var WasCall := False;
    var NewSource := CheckAndCallFuncImplicit(Scontext, Source, WasCall);
    if WasCall then
    begin
      SrcDataType := NewSource.DataType.ActualDataType;
      Result := TIDCastExpression.Create(NewSource, DstDataType);
    end;
  end;
end;

class procedure TASTDelphiUnit.CheckAndCallFuncImplicit(const EContext: TEContext);
var
  Expr, Res: TIDExpression;
begin
  Expr := EContext.Result;
  if Assigned(Expr) and
    (Expr.DataTypeID = dtProcType) and
     Assigned(Expr.AsProcedure.ResultType) then
  begin
    // todo: generate a call expression
    Res := GetTMPVarExpr(EContext, Expr.AsProcedure.ResultType, Expr.TextPosition);
    EContext.RPNPushExpression(Res);
  end;
end;

function TASTDelphiUnit.AddResultParameter(ParamsScope: TParamsScope; DataType: TIDType): TIDVariable;
var
  Decl: TIDDeclaration;
begin
  Result := TIDVariable.CreateAsAnonymous(ParamsScope);

  Decl := ParamsScope.FindID('Result');
  if Assigned(Decl) then
    Result.Name := '$Result' // just a flag that Result was redeclared, should be cheched later
  else
    Result.Name := 'Result';

  Result.Flags := [VarParameter, VarOut, VarHiddenParam, VarResult];
  Result.DataType := DataType;
  Result.TextPosition := Lexer_Position;
  ParamsScope.InsertID(Result);
end;

class procedure TASTDelphiUnit.AddSelfParameter(AProcScope: TProcScope; Struct: TIDStructure; ClassMethod: Boolean);
var
  SelfParam: TIDVariable;
  DataType: TIDType;
begin
  if Struct is TDlphHelper then
    DataType := TDlphHelper(Struct).Target
  else
    DataType := Struct;

  if ClassMethod and (DataType is TIDClass)  then
    DataType := TIDClass(DataType).ClassOfType;

  SelfParam := TIDVariable.Create(AProcScope, Identifier('Self'), DataType, [VarParameter, VarSelf, VarHiddenParam]);
  AProcScope.AddVariable(SelfParam);
  AProcScope.ParamsScope.SelfParam := SelfParam;
end;

class function TASTDelphiUnit.CheckAndCallFuncImplicit(const SContext: TSContext; Expr: TIDExpression; out WasCall: Boolean): TIDExpression;
begin
  WasCall := False;
  if Expr.ItemType = itProcedure then
  begin
    var LProc := Expr.AsProcedure;
    var LResultType := LProc.ResultType;
    if Assigned(LResultType) or (pfConstructor in LProc.Flags) then
    begin
      WasCall := True;
      if (pfConstructor in LProc.Flags) then
        LResultType := (Expr as TIDCallExpression).Instance.AsType;

      Result := GetTMPVarExpr(SContext, LResultType, Expr.TextPosition);
    end else
      Result := Expr;
  end else
  begin
    if Expr.DataTypeID = dtProcType then
    begin
      var LProcType := TIDProcType(Expr.DataType);
      if Assigned(LProcType.ResultType) then
      begin
        WasCall := True;
        Result := GetTMPVarExpr(SContext, LProcType.ResultType, Expr.TextPosition);
      end else
        Result := Expr;
    end else
      Result := Expr;
  end;
end;

class function TASTDelphiUnit.CheckAndCallFuncImplicit(const EContext: TEContext; Expr: TIDExpression): TIDExpression;
begin
  if (Expr.DataTypeID = dtProcType) and
      Assigned(TIDProcType(Expr.DataType).ResultType) then
  begin
    // todo: generate func call
    Result := GetTMPVarExpr(EContext, TIDProcType(Expr.DataType).ResultType, Expr.TextPosition);
  end else
    Result := Expr;
end;

function TASTDelphiUnit.CheckAndMakeClosure(const SContext: TSContext; const ProcDecl: TIDProcedure): TIDClosure;
begin
  // todo: recognize using out context and create an anonymous class
  Result := nil;
end;

function TASTDelphiUnit.CheckAndParseAbsolute(Scope: TScope; out ADecl: TIDDeclaration): TTokenID;
begin
  if Lexer_IsCurrentToken(tokenD_absolute) then
  begin
    var LID: TIdentifier;
    Lexer_ReadNextIdentifier(Scope, LID);
    ADecl := Scope.FindID(LID.Name);
    if not Assigned(ADecl) then
      ERRORS.UNDECLARED_ID(LID);
    if ADecl.ItemType <> itVar then
      AbortWork(sVariableRequired, Lexer_Position);
    Result := Lexer_NextToken(Scope);
  end else
    Result := Lexer_CurTokenID;
end;

function TASTDelphiUnit.CheckAndParseAttribute(Scope: TScope): TTokenID;
begin
  // todo: complete the attributes parsing
  Result := Lexer_CurTokenID;
  if Result = token_openblock then
    Result := ParseAttribute(Scope);
end;

function TASTDelphiUnit.CheckAndParseDeprecated(Scope: TScope; CurrToken: TTokenID): TTokenID;
var
  MessageExpr: TIDExpression;
begin
  if Lexer_AmbiguousId = token_deprecated then
  begin
    Result := Lexer_NextToken(Scope);
    if Result = token_identifier then
    begin
      Result := ParseConstExpression(Scope, MessageExpr, TExpessionPosition.ExprRValue);
      CheckStringExpression(MessageExpr);
    end;
  end else
    Result := CurrToken;
end;

function TASTDelphiUnit.CheckAndParseProcTypeCallConv(Scope: TScope; Token: TTokenID; TypeDecl: TIDProcType): TTokenID;
begin
  if Token = token_semicolon then
    Result := Lexer_NextToken(Scope)
  else
    Result := Token;

  case Lexer_AmbiguousId of
    tokenD_stdcall: begin
      TypeDecl.CallConv := ConvStdCall;
      Result := Lexer_NextToken(Scope);
      if Result = token_semicolon then
        Result := Lexer_NextToken(Scope);
    end;
    tokenD_fastcall: begin
      TypeDecl.CallConv := ConvFastCall;
      Result := Lexer_NextToken(Scope);
      if Result = token_semicolon then
        Result := Lexer_NextToken(Scope);
    end;
    tokenD_cdecl: begin
      TypeDecl.CallConv := ConvCDecl;
      Result := Lexer_NextToken(Scope);
      if Result = token_semicolon then
        Result := Lexer_NextToken(Scope);
    end;
  end;
end;

class function TASTDelphiUnit.CheckImplicit(const SContext: TSContext; Source: TIDExpression; Dest: TIDType;
                                            AResolveCalls: Boolean): TIDDeclaration;
var
  SDataType: TIDType;
  SrcDTID, DstDTID: TDataTypeID;
begin
  SDataType := Source.DataType.ActualDataType;
  Dest := Dest.ActualDataType;

  if SDataType = Dest then
    Exit(Dest);

  // ищем явно определенный implicit у источника
  Result := SDataType.GetImplicitOperatorTo(Dest);
  if Result is TSysTypeCast then
    Result := TSysTypeCast(Result).Check(SContext, Source, Dest);

  if Assigned(Result) then
    Exit;

  // ищем явно определенный implicit у приемника
  Result := Dest.GetImplicitOperatorFrom(SDataType);
  if Result is TSysTypeCast then
    Result := TSysTypeCast(Result).Check(SContext, Source, Dest);
  if Assigned(Result) then
    Exit;

  // если не нашли точных имплиситов, ищем подходящий (у источника)
  Result := SDataType.FindImplicitOperatorTo(Dest);
  if Result is TSysTypeCast then
    Result := TSysTypeCast(Result).Check(SContext, Source, Dest);
  if Assigned(Result) then
    Exit;

  // если не нашли точных имплиситов, ищем подходящий (у приемника)
  Result := Dest.FindImplicitOperatorFrom(SDataType);
  if Result is TSysTypeCast then
    Result := TSysTypeCast(Result).Check(SContext, Source, Dest);
  if Assigned(Result) then
    Exit;

  SrcDTID := Source.DataTypeID;
  DstDTID := Dest.DataTypeID;

  // есди приемник - class of
  if DstDTID = dtClassOf then
  begin
    Result := MatchImplicitClassOf(Source, TIDClassOf(Dest));
    if Assigned(Result) then
      Exit;
  end;

  if (SrcDTID = dtPointer) and (DstDTID = dtPointer) then
  begin
    if (TIDPointer(SDataType).ReferenceType = nil) and
       (TIDPointer(Dest).ReferenceType = nil) then
      Exit(Source.DataType);

    // it needs to check !!!
    if not Assigned(TIDPointer(SDataType).ReferenceType) or not Assigned(TIDPointer(Dest).ReferenceType) then
      Exit(Source.DataType);

    if TIDPointer(SDataType).ReferenceType.ActualDataType = TIDPointer(Dest).ReferenceType.ActualDataType then
      Exit(Source.DataType);
  end;

  if (SrcDTID = dtProcType) and (DstDTID = dtProcType) then
    Result := MatchProcedureTypes(TIDProcType(SDataType), TIDProcType(Dest))
  else
  if (Source.Declaration.ItemType = itConst) and (SrcDTID = dtDynArray) then
    Result := CheckConstDynArrayImplicit(SContext, Source, Dest)
  else begin
    Result := MatchDynArrayImplicit(Source, Dest);
  end;
  if (DstDTID = dtGeneric) or (SrcDTID = dtGeneric) then
  begin
     // todo: constrains
     Exit(Source.Declaration);
  end;

  if AResolveCalls then
  begin
    if Source.ItemType = itProcedure then
    begin
      if Assigned(Source.AsProcedure.ResultType) then
      begin
        var AResultExpr := TIDExpression.Create(SContext.Proc.GetTMPVar(Source.AsProcedure.ResultType), Source.TextPosition);
        Exit(CheckImplicit(SContext, AResultExpr, Dest));
      end;
    end;

    if (Source.ItemType = itVar) and (Source.DataTypeID = dtProcType) then
    begin
      var AResultType := TIDProcType(Source.DataType).ResultType;
      if Assigned(AResultType) then
      begin
        var AResultExpr := TIDExpression.Create(SContext.Proc.GetTMPVar(AResultType), Source.TextPosition);
        Exit(CheckImplicit(SContext, AResultExpr, Dest));
      end;
    end;
  end;
end;

function TASTDelphiUnit.ParseAnonymousProc(Scope: TScope; var EContext: TEContext; const SContext: TSContext; ProcType: TTokenID): TTokenID;
var
  ProcScope: TProcScope;
  GenericsArgs: TIDTypeArray;
  ResultType: TIDType;
  ProcDecl: TASTDelphiProc;
  Closure: TIDClosure;
  Expr: TIDExpression;
begin
  ProcScope := TProcScope.CreateInDecl(Scope, {Proc:} nil);

  // создаем Result переменную (тип будет определен позже)

  Result := Lexer_NextToken(Scope);

  // если generic
  if Result = token_less then
    Result := ParseGenericsHeader(ProcScope, {out} GenericsArgs);

  // парсим параметры
  if Result = token_openround then
  begin
    ParseParameters(ProcScope, ProcScope.ParamsScope);
    Result := Lexer_NextToken(Scope); // move to "token_colon"
  end;

  if ProcType = token_function then
  begin
    Lexer_MatchToken(Result, token_colon);
    // парсим тип возвращаемого значения
    Result := ParseTypeSpec(ProcScope, {out} ResultType);
    AddResultParameter(ProcScope.ParamsScope, ResultType);
  end else
    ResultType := nil;

  ProcDecl := TASTDelphiProc.CreateAsAnonymous(ImplScope);
  ProcDecl.ParamsScope := ProcScope;
  ProcDecl.EntryScope := ProcScope;
  ProcDecl.ResultType := ResultType;
  ProcDecl.CreateProcedureTypeIfNeed(Scope);
  ProcScope.Proc := ProcDecl;
  ProcDecl.ExplicitParams := ProcScope.ExplicitParams;

  {парсим тело анонимной процедуры}
  Result := ParseProcBody(ProcDecl);

  Closure := CheckAndMakeClosure(SContext, ProcDecl);
  if Assigned(Closure) then
  begin
    {если это замыкание, меняем анонимную процедуру на метод замыкания}
    ProcDecl.Struct := Closure;
    ProcDecl.MakeSelfParam;
    ProcDecl.Name := 'Run';
    Expr := EmitCreateClosure(SContext, Closure);
  end else begin
    ImplScope.AddAnonymousProcedure(ProcDecl);
    Expr := TIDExpression.Create(ProcDecl, Lexer_PrevPosition);
  end;
  EContext.RPNPushExpression(Expr);
end;

function TASTDelphiUnit.ParseArrayMember(Scope: TScope; var EContext: TEContext; ASTE: TASTExpression): TTokenID;
var
  ArrExpr: TIDExpression;
  ArrDecl: TIDDeclaration;
  IdxCount: Integer; // кол-во индексов указанных при обращении к массиву
  Expr: TIDExpression;
  InnerEContext: TEContext;
  DimensionsCount: Integer;
  ArrType: TIDType;
  DataType: TIDType;
begin
  ArrExpr := EContext.RPNPopExpression();
  ArrDecl := ArrExpr.Declaration;
  // auto resolve function call when missed parentheses
  if (ArrDecl.ItemType = itProcedure) and Assigned(TIDProcedure(ArrDecl).ResultType) then
  begin
    ArrDecl := EContext.Proc.GetTMPVar(TIDProcedure(ArrDecl).ResultType);
    ArrExpr := TIDExpression.Create(ArrDecl, ArrExpr.TextPosition);
  end;
  ArrType := ArrExpr.DataType;
  DataType := nil;
  DimensionsCount := 0;
  // indexed property case
  if (ArrDecl.ItemType = itProperty) and (TIDProperty(ArrDecl).ParamsCount > 0) then
  begin
    DimensionsCount := TIDProperty(ArrDecl).ParamsCount;
    DataType := TIDProperty(ArrDecl).DataType;
  end else
  if ArrType.DataTypeID = dtPointer then
  begin
    var ARefType := GetPtrReferenceType(TIDPointer(ArrType));
    // auto-dereference for static array pointer (doesn't depend on POINTERMATH state)
    if ARefType.DataTypeID = dtStaticArray then
    begin
      DimensionsCount := TIDArray(ARefType).AllDimensionsCount;
      DataType := TIDArray(ARefType).ElementDataType;
    end else
    // POINTERMATH can be applied to the local scope or pointer type
    if Options.POINTERMATH or TIDPointer(ArrType).PointerMath then
    begin
      DimensionsCount := 1;
      DataType := ARefType;
    end else
      ERRORS.ARRAY_TYPE_REQUIRED(ArrExpr.Declaration.ID, Lexer_PrevPosition);
  end else
  if ArrType.DataTypeID in [dtPAnsiChar, dtPWideChar] then
  begin
    var ARefType := GetPtrReferenceType(TIDPointer(ArrType));
    DimensionsCount := 1;
    DataType := ARefType;
  end else
  if ArrType.DataTypeID = dtVariant then
  begin
    DimensionsCount := 1;
    DataType := Sys._Variant;
  end else
  if ArrType is TIDArray then
  begin
    DimensionsCount := TIDArray(ArrType).AllDimensionsCount;
    DataType := TIDArray(ArrType).ElementDataType;
  end else
  if (ArrType is TIDStructure) and (TIDStructure(ArrType).GetDefaultProperty <> nil) then
  begin
    ArrDecl := TIDStructure(ArrType).GetDefaultProperty;
    DimensionsCount := TIDProperty(ArrDecl).ParamsCount;
    DataType := TIDProperty(ArrDecl).DataType;
    Expr := TIDExpression.Create(ArrDecl);
  end else
  if (ArrDecl.ItemType = itType) and (TIDType(ArrDecl).DataTypeID in [dtString, dtAnsiString]) then
  begin
    // string with length restriction (ShortString)
    Lexer_NextToken(Scope);
    Result := ParseConstExpression(Scope, Expr, ExprNested);
    Lexer_MatchToken(Result, token_closeblock);
    Result := Lexer_NextToken(Scope);
    // todo: create new string type and add size restriction
    Expr := TIDExpression.Create(ArrDecl, ArrExpr.TextPosition);
    EContext.RPNPushExpression(Expr);
    Exit;
  end else begin
    ERRORS.ARRAY_TYPE_REQUIRED(ArrDecl.ID, ArrExpr.TextPosition);
    DimensionsCount := 0;
  end;

  var Op: TASTOpArrayAccess := ASTE.AddOperation<TASTOpArrayAccess>;

  IdxCount := 0;
  InitEContext(InnerEContext, EContext.SContext, ExprNested);
  var Indexes: TIDExpressions := [];
  while True do begin
    Lexer_NextToken(Scope);
    var ASTExpr: TASTExpression := nil;
    Result := ParseExpression(Scope, EContext.SContext, InnerEContext, ASTExpr);
    Op.AddIndex(ASTExpr);
    Expr := InnerEContext.Result;
    CheckEmptyExpression(Expr);

    Indexes := Indexes + [Expr];
    {if Assigned(InnerEContext.LastBoolNode) then
      Bool_CompleteImmediateExpression(InnerEContext, Expr);}

    // possible multi-dimentions array declarations
    // TA = array [0..N, 0..N] of <type>
    // TA = array [0..N] of array [0..N] of <type>
    // TA = array [0..N] of array of <type>

    if ArrType is TIDArray then
    begin
      if Expr.IsConstant then
        StaticCheckBounds(TIDArray(ArrType).Dimensions[IdxCount], Expr.AsConst);

      DataType := TIDArray(ArrType).ElementTypeByIndex[IdxCount];
    end else begin
       // динамическая проверка на границы массива
       {if UseCheckBound then
         EmitDynCheckBound(EContext.SContext, Decl, Expr);}
    end;

    //PMContext.Add(Expr);
    Inc(IdxCount);
    if Result = token_coma then
    begin
      InnerEContext.Reset;
      Continue;
    end;
    Lexer_MatchToken(Result, token_closeblock);
    Result := Lexer_NextToken(Scope);
    Break;
  end;
  if IdxCount > DimensionsCount then
    ERRORS.NEED_SPECIFY_NINDEXES(ArrDecl);


  var ATmpVar := GetTMPVar(EContext, DataType);
  var AExpr := TIDArrayExpression.Create(ATmpVar, ArrExpr.TextPosition);
  AExpr.DataType := DataType;
  AExpr.Indexes := Indexes;
  EContext.RPNPushExpression(AExpr);
end;

function TASTDelphiUnit.ParseAsmSpecifier: TTokenID;
begin
  Result := token_unknown;
end;

function TASTDelphiUnit.ParseASMStatement(Scope: TScope; const SContext: TSContext): TTokenID;
var
  KW: TASTKWAsm;
begin
  KW := SContext.Add(TASTKWAsm) as TASTKWAsm;
  Result := Lexer_CurTokenID;
  while (Result <> token_end) do
  begin
    // skip all to end
    if KW.Next <> nil then;
    Result := Lexer_NextToken(Scope);
  end;
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ParseAttribute(Scope: TScope): TTokenID;
begin
  Result := Lexer_CurTokenID;
  var LNestedBlocks := 1;
  while (LNestedBlocks > 0) and (Result <> token_eof) do
  begin
    Result := Lexer_NextToken(Scope);
    // todo: implement parsing attrubute class and arguments
    // tmp: skip all nested blocks for such cases [UserAttr([val1, val2])]
    case Result of
      token_openblock: Inc(LNestedBlocks);
      token_closeblock: Dec(LNestedBlocks);
    end;
  end;
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ParseProperty(Scope: TScope; Struct: TIDStructure): TTokenID;
var
  ID: TIdentifier;
  Prop: TIDProperty;
  PropDataType: TIDType;
  Proc: TIDProcedure;
  Expr: TIDExpression;
  EContext: TEContext;
  DataType: TIDType;
  IndexedPropParams: TIDParamArray;
  SContext: TSContext;
  ASTE: TASTExpression;
begin
  Lexer_ReadNextIdentifier(Scope, ID);
  Prop := TIDProperty.Create(Scope, ID);
  Scope.AddProperty(Prop);

  Result := Lexer_NextReseredToken(Scope);

  var LPropRedeclaration := True;

  if Result in [token_colon, token_openblock] then
  begin
    LPropRedeclaration := False;
    if Result = token_openblock then
    begin
      // indexed propery
      var LParamsScope := TParamsScope.Create(stLocal, Scope);
      Prop.Params := LParamsScope;
      Result := ParseParameters(Scope, LParamsScope);
      IndexedPropParams := LParamsScope.ExplicitParams;
      Lexer_MatchToken(Result, token_closeblock);
      Result := Lexer_NextToken(Scope);
    end;

    // property type
    Lexer_MatchToken(Result, token_colon);
    Result := ParseTypeSpec(Scope, PropDataType);
    Prop.DataType := PropDataType;

    SContext := fUnitSContext;

    if (Struct.DataTypeID = dtInterface) and (TIDInterface(Struct).IsDisp) then
    begin
      // readonly propery
      if Lexer_IsCurrentToken(tokenD_readonly) then
      begin
        Result := Lexer_NextToken(Scope);
      end else
      // writeonly propery
      if Lexer_IsCurrentToken(tokenD_writeonly) then
      begin
        Result := Lexer_NextToken(Scope);
      end;
      // dispid
      if Lexer_IsCurrentToken(tokenD_dispid) then
      begin
        Result := ProcSpec_DispId(Scope, Struct, ID);
      end;
      // indexed default propery (note: default is ambiguous keyword)
      if Lexer_IsCurrentToken(tokenD_default) then
      begin
        if Prop.ParamsCount = 0 then
          ERRORS.DEFAULT_PROP_MUST_BE_ARRAY_PROP;
        if not Assigned(Struct.DefaultProperty) then
        begin
          Struct.DefaultProperty := Prop;
          Prop.DefaultIndexedProperty := True;
        end else
          ERRORS.DEFAULT_PROP_ALREADY_EXIST(Prop);
        Lexer_ReadSemicolon(Scope);
        Result := Lexer_NextToken(Scope);
      end;
      Exit;
    end;

    // property index value (note: index is ambiguous keyword)
    // note: can only be after the property type
    if Lexer_IsCurrentToken(tokenD_index) then
    begin
      var LIndexValue: TIDExpression;
      Lexer_NextToken(Scope);
      Result := ParseConstExpression(Scope, {out} LIndexValue, ExprRValue);
      CheckEmptyExpression(LIndexValue);
      Prop.IndexValue := LIndexValue.AsConst;
      // create "index" param for the propery
      if not Assigned(Prop.Params) then
        Prop.Params := TParamsScope.Create(stLocal, Scope);
      var LIndexParam := TIDParam.CreateAsSystem(Prop.Scope, 'Index');
      LIndexParam.DataType := fSysDecls._Int32;
      Prop.Params.AddExplicitParam(LIndexParam);
    end;

    // getter
    if Lexer_AmbiguousId = tokenD_read then
      Result := ParsePropertyGetter(Scope, Prop);

    // setter
    if Lexer_AmbiguousId = tokenD_write then
      Result := ParsePropertySetter(Scope, Prop);
  end;

  if LPropRedeclaration then
  begin
    // if it's a re-declaration, find an existing property
    if Assigned(Struct.Ancestor) then
    begin
      var LExistingProp := Struct.Ancestor.FindProperty(ID.Name);
      if not Assigned(LExistingProp) or (LExistingProp.ItemType <> itProperty) then
        ERRORS.PROPERTY_DOES_NOT_EXIST_IN_BASE_CLASS(ID);

      Prop.Getter := LExistingProp.Getter;
      Prop.Setter := LExistingProp.Setter;
      Prop.DataType := LExistingProp.DataType;
    end else
      ERRORS.PROPERTY_DOES_NOT_EXIST_IN_BASE_CLASS(ID);

    // getter
    if Lexer_AmbiguousId = tokenD_read then
      Result := ParsePropertyGetter(Scope, Prop);

    // setter
    if Lexer_AmbiguousId = tokenD_write then
      Result := ParsePropertySetter(Scope, Prop);

    // property index value (note: index is ambiguous keyword)
    // note: can only be after the property name (and looks like a Delphi bug)
    if Lexer_IsCurrentToken(tokenD_index) then
    begin
      var LIndexValue: TIDExpression;
      Lexer_NextToken(Scope);
      Result := ParseConstExpression(Scope, {out} LIndexValue, ExprRValue);
      CheckEmptyExpression(LIndexValue);
    end;
  end;

  // stored propery (note: stored is ambiguous keyword)
  if Lexer_IsCurrentToken(tokenD_stored) then
    Result := ParsePropertyStored(Scope, Prop);

  // regular property default value (note: default is ambiguous keyword)
  if Lexer_IsCurrentToken(tokenD_default) then
  begin
    var LDefaultExpr: TIDExpression;
    Lexer_NextToken(Scope);
    Result := ParseConstExpression(Scope, {out} LDefaultExpr, ExprRValue);
    CheckEmptyExpression(LDefaultExpr);
  end else
  if Lexer_IsCurrentToken(tokenD_nodefault) then
    Result := Lexer_NextToken(Scope);

  Lexer_MatchToken(Result, token_semicolon);
  Result := Lexer_NextToken(Scope);

  // indexed default propery (note: default is ambiguous keyword)
  if Lexer_IsCurrentToken(tokenD_default) then
  begin
    if Prop.ParamsCount = 0 then
      ERRORS.DEFAULT_PROP_MUST_BE_ARRAY_PROP;
    if not Assigned(Struct.DefaultProperty) then
    begin
      Struct.DefaultProperty := Prop;
      Prop.DefaultIndexedProperty := True;
    end else
      ERRORS.DEFAULT_PROP_ALREADY_EXIST(Prop);
    Lexer_ReadSemicolon(Scope);
    Result := Lexer_NextToken(Scope);
  end;
end;

function TASTDelphiUnit.ParsePropertyGetter(Scope: TScope; AProp: TIDProperty): TTokenID;
var
  SContext: TSContext;
  EContext: TEContext;
  LExpr: TIDExpression;
  LDataType: TIDType;
  ASTE: TASTExpression;
begin
  SContext := fUnitSContext;
  InitEContext(EContext, SContext, ExprRValue);
  Lexer_NextToken(Scope);
  Result := ParseExpression(Scope, SContext, EContext, ASTE);
  LExpr := EContext.Result;
  if LExpr.ItemType = itProcedure then
  begin
    var LIndexedParams: TIDParamArray;
    if Assigned(AProp.Params) then
      LIndexedParams := AProp.Params.ExplicitParams;

    var LProc := LExpr.AsProcedure;
    LDataType := LProc.ResultType;
    if not Assigned(LDataType) then
      AbortWork(sFieldConstOrFuncRequiredForGetter, LExpr.TextPosition);

    MatchPropGetter(AProp, LProc, LIndexedParams);
  end else
    LDataType := LExpr.DataType;

  CheckImplicitTypes(LDataType, AProp.DataType, LExpr.TextPosition);
  AProp.Getter := LExpr.Declaration;
end;

function TASTDelphiUnit.ParsePropertySetter(Scope: TScope; AProp: TIDProperty): TTokenID;
var
  SContext: TSContext;
  EContext: TEContext;
  LExpr: TIDExpression;
  ASTE: TASTExpression;
begin
  SContext := fUnitSContext;
  InitEContext(EContext, SContext, ExprRValue);
  Lexer_NextToken(Scope);
  Result := ParseExpression(Scope, SContext, EContext, {out} ASTE);
  LExpr := EContext.Result;
  var LIndexedParams: TIDParamArray;
  if Assigned(AProp.Params) then
    LIndexedParams := AProp.Params.ExplicitParams;
  case LExpr.ItemType of
    itVar: {do nothing};  // todo: check scope of the field
    itProcedure: MatchPropSetter(AProp, LExpr, LIndexedParams);
  else
    AbortWork(sFieldOrProcRequiredForSetter, LExpr.TextPosition);
  end;
  AProp.Setter := LExpr.Declaration;
end;

function TASTDelphiUnit.ParsePropertyStored(Scope: TScope; AProp: TIDProperty): TTokenID;
var
  SContext: TSContext;
  EContext: TEContext;
  LExpr: TIDExpression;
  ASTE: TASTExpression;
begin
  SContext := fUnitSContext;
  InitEContext(EContext, SContext, ExprRValue);
  Lexer_NextToken(Scope);
  Result := ParseExpression(Scope, SContext, {var} EContext, {out} ASTE);
  LExpr := EContext.Result;
  CheckEmptyExpression(LExpr);
  if LExpr.IsProcedure then
  begin
    // check "stored" function signature
    if not Assigned(LExpr.AsProcedure.ResultType) or
       (LExpr.AsProcedure.ResultType.DataTypeID <> dtBoolean) then
      ERRORS.BOOLEAN_EXPRESSION_REQUIRED(LExpr);
  end else
    CheckBooleanExpression(LExpr);
end;

procedure TASTDelphiUnit.ParseCondDefine(Scope: TScope; add_define: Boolean);
var
  ID: TIdentifier;
  idx: Integer;
begin
  Lexer_ReadNextIFDEFLiteral(Scope, ID);
  idx := FDefines.IndexOf(ID.Name);
  if add_define then begin
    if idx = -1 then
      FDefines.Add(ID.Name);
  end else begin
    if idx > -1 then
      FDefines.Delete(idx);
  end;
end;

function TASTDelphiUnit.ParseCondError(Scope: TScope): TTokenID;
var
  ID: TIdentifier;
begin
  Lexer_ReadNextIdentifier(Scope, ID);
  AbortWork(ID.Name, ID.TextPosition);
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ParseCondHint(Scope: TScope): TTokenID;
var
  ID: TIdentifier;
begin
  Lexer_ReadNextIdentifier(Scope, ID);
  PutMessage(cmtHint, ID.Name, ID.TextPosition);
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ParseCondIf(Scope: TScope; out ExpressionResult: TCondIFValue): TTokenID;
var
  Expr: TIDExpression;
  CondScope: TConditionalScope;
begin
  CondScope := TConditionalScope.Create(TScopeType.stLocal, Scope);
  Lexer.NextToken;
  Result := ParseConstExpression(CondScope, Expr, ExprRValue);
  if Expr.DataTypeID <> dtGeneric then
  begin
    CheckBooleanExpression(Expr);
    ExpressionResult := TCondIFValue(Expr.AsBoolConst.Value);
  end else
    ExpressionResult := condIFUnknown;
end;

function TASTDelphiUnit.ParseCondInclude(Scope: TScope): TTokenID;
var
  LFileName: string;
begin
  while True do begin
    Result := TTokenID(Lexer.NextToken);
    if Result = token_closefigure then
      Break;

    LFileName := LFileName + Lexer.OriginalToken
  end;

  var LFullFileName := Project.FindUnitFile(LFileName, {AFileExt:} '');
  if LFullFileName <> '' then
  begin
    var LStrings := TStringList.Create;
    try
      // use TStringList since it proper handles any file encodings
      LStrings.LoadFromFile(LFullFileName);
      var LMessages := CompileSource(Scope, LFileName, LStrings.Text);
      if LMessages.HasErrors then
        AbortWork('The included file: %s has errors', [LFileName], Lexer_Position);
    finally
      LStrings.Free;
    end;
  end else
    AbortWork('File not found: %s', [LFileName], Lexer_Position);
end;

function TASTDelphiUnit.ParseCondIfDef(Scope: TScope): Boolean;
var
  ID: TIdentifier;
begin
  Lexer_ReadNextIFDEFLiteral(Scope, ID);
  Result := Defined(ID.Name);
end;

function TASTDelphiUnit.ParseCondMessage(Scope: TScope): TTokenID;
var
  ID: TIdentifier;
  MsgType: string;
begin
  Lexer_ReadNextIdentifier(Scope, ID);
  MsgType := UpperCase(ID.Name);
  Lexer_ReadNextIdentifier(Scope, ID);
  if MsgType = 'ERROR' then
    PutMessage(TCompilerMessageType.cmtWarning, 'message error: ' + ID.Name, ID.TextPosition);
  // todo:
  Result := Lexer.NextToken;
end;

function TASTDelphiUnit.ParseCondOptions(Scope: TScope): TTokenID;
  function SkipToEnd: TTokenID;
  begin
    repeat
      Result := Lexer.NextToken;
    until Result = token_closefigure;
    Result := Lexer.NextToken;
  end;
var
  OptID: TIdentifier;
  Opt: TOption;
  ErrorMsg: string;
begin
  while True do begin
    Lexer.ReadNextIdentifier(OptID);
    Opt := Options.FindOption(OptID.Name);
    if not Assigned(Opt) then begin
      Warning('Unknown compiler directive: %s', [OptID.Name], OptID.TextPosition);
      Exit(SkipToEnd());
    end;

    case Opt.ArgsCount of
      1: begin
        Result := Lexer.NextToken;
        var Value: string;
        if Result = token_identifier then
          Value := Lexer.OriginalToken
        else
          Value := Lexer_TokenLexem(Result);

        TValueOption(Opt).SetValue(Value, ErrorMsg);
      end;
      // todo:
    end;

    if ErrorMsg <> '' then
      AbortWork(OptID.Name + ' option arguments Error: ' + ErrorMsg, Lexer_Position);


    Result := Lexer.NextToken;
    if Result = token_coma then
      Continue;
    break;
  end;
  Lexer_MatchToken(Result, token_closefigure);
  Result := Lexer.NextToken;
end;

function TASTDelphiUnit.ParseCondOptSet(Scope: TScope): TTokenID;
var
  ID: TIdentifier;
  Expr: TIDExpression;
begin
  Lexer_ReadNextIdentifier(Scope, ID);
  if not Options.Exist(ID.Name) then
    ERRORS.UNKNOWN_OPTION(ID);

  Lexer_ReadToken(Scope, token_equal);

  Lexer.NextToken;
  Result := ParseConstExpression(Scope, Expr, ExprRValue);

  Lexer_MatchSemicolon(Result);

  CheckEmptyExpression(Expr);

  Options.OptSet(ID.Name, Expr.AsConst.AsVariant);
  Result := Lexer.NextToken;
end;

function TASTDelphiUnit.ParseCondStatements(Scope: TScope; Token: TTokenID): TTokenID;
  function SkipToElseOrEnd(SkipToEnd: Boolean): TTokenID;
  var
    ifcnt: Integer;
  begin
    ifcnt := 0;
    while True do begin
      Result := Lexer.NextToken;
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
        token_eof: ERRORS.END_OF_FILE;
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
      // {$define ...}
      token_cond_define: begin
        ParseCondDefine(Scope, True);
        Result := Lexer_NextToken(Scope);
      end;
      //////////////////////////////////////////
      // {$else ...}
      token_cond_else: begin
        // skip all comment tokens
        repeat
          Result := Lexer.NextToken;
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
        {skip optional condition}
        repeat
          Result := Lexer.NextToken;
        until (Result = token_eof) or (Result = token_closefigure);
        Result := Lexer.NextToken;
      end;
      //////////////////////////////////////////
      // {$include ...}
      token_cond_include: Result := ParseCondInclude(Scope);
      //////////////////////////////////////////
      // {$undefine ...}
      token_cond_undefine: begin
        ParseCondDefine(Scope, False);
        Result := Lexer.NextToken;
        Lexer_MatchToken(Result, token_closefigure);
        Result := Lexer_NextToken(Scope);
      end;
      //////////////////////////////////////////
      // {$ifdef ...}
      token_cond_ifdef: begin
        CondResult := ParseCondIfDef(Scope);
        fCondStack.Push(CondResult);
        if not CondResult then
          Result := SkipToElseOrEnd(False)
        else
          Result := Lexer.NextToken;
      end;
      //////////////////////////////////////////
      // {$ifndef ...}
      token_cond_ifndef: begin
        CondResult := ParseCondIfDef(Scope);
        fCondStack.Push(not CondResult);
        if CondResult then
          Result := SkipToElseOrEnd(False)
        else
          Result := Lexer.NextToken;
      end;
      //////////////////////////////////////////
      // {$if ...}
      token_cond_if: begin
        Result := ParseCondIf(Scope, ExprResult);
        fCondStack.Push(ExprResult = condIfTrue);
        case ExprResult of
          condIFFalse: Result := SkipToElseOrEnd(False);
          condIFUnknown: Result := SkipToElseOrEnd(True);
        end;
        CheckCorrectEndCondStatemet(Result);
      end;
      //////////////////////////////////////////
      // {$ifopt ...}
      token_cond_ifopt: begin
        // todo: complete the options check
        repeat
          Result := Lexer.NextToken;
        until (Result = token_eof) or (Result = token_closefigure);
        fCondStack.Push(False);
        Result := SkipToElseOrEnd(False);
      end;
      //////////////////////////////////////////
      // {$message ...}
      token_cond_message: Result := ParseCondMessage(Scope);
      //////////////////////////////////////////
      // {$<option> ...}
      token_cond_any: Result := ParseCondOptions(Scope);
    else
      ERRORS.FEATURE_NOT_SUPPORTED;
      Result := token_unknown;
    end;
    if Result = token_closefigure then
      Result := Lexer.NextToken;
    if Result < token_cond_define then
      Break;
  end;
end;

function TASTDelphiUnit.ParseCondWarn(Scope: TScope): TTokenID;
var
  ID: TIdentifier;
begin
  Lexer_ReadNextIdentifier(Scope, ID);
  PutMessage(cmtWarning, ID.Name, ID.TextPosition);
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ParseConstExpression(Scope: TScope; out Expr: TIDExpression; EPosition: TExpessionPosition): TTokenID;
var
  EContext: TEContext;
  ASTE: TASTExpression;
begin
  InitEContext(EContext, fUnitSContext, EPosition);
  Result := ParseExpression(Scope, fUnitSContext, EContext, ASTE);
  CheckEndOfFile(Result);
  Expr := EContext.Result;
  if not Assigned(Expr) then
    Exit;

  if (Expr.ItemType <> itType) and (Expr.DataTypeID <> dtGeneric) then
    CheckConstExpression(Expr);
end;

function TASTDelphiUnit.ParseConstSection(Scope: TScope; AInlineConst: Boolean): TTokenID;

  function CheckAndCorrectType(AExpr: TIDExpression): TIDType;
  begin
    // AST parser infers array of <type> by default for anonymous constants
    // Delphi expects set of <type> for named consts, so let's convert it
    if (AExpr.DataTypeID = dtDynArray) then
    begin
      var LArrayType := AExpr.DataType as TIDDynArray;
      if LArrayType.ElementDataType.IsOrdinal then
      begin
        Result := TIDSet.CreateAsAnonymous(LArrayType.Scope,
                                                 TIDOrdinal(LArrayType.ElementDataType));
      end else
      begin
        ERRORS.ORDINAL_TYPE_REQUIRED(AExpr.TextPosition);
        Result := nil;
      end;
    end else
      Result := AExpr.DataType;
  end;

var
  LExplicitType: TIDType;
  LConst: TIDConstant;
  LExpr: TIDExpression;
  LConstID: TIdentifier;
begin
  repeat
    Lexer_MatchIdentifier(Lexer_CurTokenID);
    Lexer_ReadCurrIdentifier(LConstID);
    Result := Lexer_NextToken(Scope);
    if Result = token_colon then
      Result := ParseTypeSpec(Scope, {out} LExplicitType)
    else
      LExplicitType := nil;

    // =
    Lexer_MatchToken(Result, token_equal);

    if Assigned(LExplicitType) then
    begin
      Result := ParseVarDefaultValue(Scope, LExplicitType, {out} LExpr);
      if LExpr.IsAnonymous then
      begin
        LExpr.Declaration.DataType := LExplicitType;
        LExpr.AsConst.ExplicitDataType := LExplicitType;
      end;
    end else begin
      // read constant value
      Lexer_NextToken(Scope);
      Result := ParseConstExpression(Scope, {out} LExpr, ExprRValue);
      CheckEmptyExpression(LExpr);
    end;
    CheckConstExpression(LExpr);

    // the const is an interface guid
    if LExpr.ItemType = itType then
    begin
      if LExpr.AsType.DataTypeID = dtInterface then
      begin
        var LIntfDecl := LExpr.AsType.Original as TIDInterface;
        LConst := LIntfDecl.GuidDecl;
        if not Assigned(LConst) then
          ERRORS.E2232_INTERFACE_HAS_NO_INTERFACE_IDENTIFICATION(LIntfDecl);
      end else
        ERRORS.CONST_EXPRESSION_REQUIRED(LExpr);
    end else
      LConst := LExpr.AsConst;

    if LConst.IsAnonymous then
      LConst.ID := LConstID
    else begin
      // create a const alias
      var LNewConst := LConst.MakeCopy as TIDConstant;
      LNewConst.ID := LConstID;
      LNewConst.DataType := LConst.DataType;
      LNewConst.AssignValue(LConst);
      LConst := LNewConst;
    end;

    if not Assigned(LExplicitType) then
      LConst.DataType := CheckAndCorrectType(LExpr);

    Result := CheckAndParsePlatform(Scope);

    Result := CheckAndParseDeprecated(Scope, Result);

    Lexer_MatchToken(Result, token_semicolon);

    Scope.AddConstant(LConst);
    // AddConstant() already called in ParseVector
    //AddConstant(LConst);

    Result := Lexer_NextToken(Scope);
  until (Result <> token_identifier) or AInlineConst;
end;

function TASTDelphiUnit.ParseConstSectionInStruct(Scope: TScope): TTokenID;
begin
  Lexer_NextToken(Scope);
  while True do begin
    Result := ParseConstSection(Scope, {AInlineConst:} True);
    if (Result <> token_identifier) or
       (Lexer_AmbiguousId in [tokenD_strict,
                              tokenD_private,
                              tokenD_protected,
                              tokenD_public,
                              tokenD_published]) then
     Break;
  end;
end;

function TASTDelphiUnit.ParseDeprecated(Scope: TScope; out DeprecatedExpr: TIDExpression): TTokenID;
begin
  Result := Lexer_NextToken(Scope);
  if Result = token_identifier then
  begin
    Result := ParseConstExpression(Scope, DeprecatedExpr, TExpessionPosition.ExprRValue);
    CheckStringExpression(DeprecatedExpr);
  end else
    DeprecatedExpr := TIDExpression.Create(Sys._DeprecatedDefaultStr, Lexer_Position);
end;

function TASTDelphiUnit.ParseCaseRecord(Scope: TScope; Decl: TIDRecord): TTokenID;
var
  Expr: TIDExpression;
  CaseTypeDecl: TIDDeclaration;
  ID: TIdentifier;
begin
  Lexer_ReadNextIdentifier(Scope, ID);
  CaseTypeDecl := FindIDNoAbort(Scope, ID);
  if not Assigned(CaseTypeDecl) then
  begin
    Result := Lexer_NextToken(Scope);
    Lexer_MatchToken(Result, token_colon);
    Lexer_NextToken(Scope);
  end;

  Result := ParseConstExpression(Scope, Expr, ExprNested);
  Lexer_MatchToken(Result, token_of);
  Result := Lexer_NextToken(Scope);
  while Result <> token_eof do
  begin
    // parse case lable const expression: (example: ... 1, 2: ...
    Result := ParseConstExpression(Scope, {out} Expr, ExprLValue);
    if Result = token_coma then
    begin
      Result := Lexer_NextToken(Scope);
      // just parse all lables for now
      // todo: labels should be validated
      Continue;
    end;
    // parse ":" & "("
    Lexer_MatchToken(Result, token_colon);
    Lexer_ReadToken(Scope, token_openround);
    Result := Lexer_NextToken(Scope);

    // parse fields first
    if (Result = token_identifier) {or (Result = token_id_or_keyword)} then
    begin
      Result := ParseFieldsInCaseRecord(Scope, vPublic, Decl);
    end;
    // parse nested case then
    if Result = token_case then
    begin
      Result := ParseCaseRecord(Scope, Decl);
      Lexer_MatchToken(Result, token_closeround);
    end;
    // parse close round at the end
    if Result = token_closeround then
    begin
      Result := Lexer_NextToken(Scope);
      if Result = token_semicolon then
        Result := Lexer_NextToken(Scope);

      case Result of
        token_minus, token_identifier{, token_id_or_keyword}:  Continue;
        token_closeround, token_end: Exit;
      else
        ERRORS.IDENTIFIER_EXPECTED(Result);
      end;
    end else
     ERRORS.EXPECTED_TOKEN(token_closeround);
  end;
end;

function TASTDelphiUnit.ParseCaseStatement(Scope: TScope; const SContext: TSContext): TTokenID;
type
  TMatchItem = record
    Expression: TIDExpression;
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
  CaseExpr,
  ItemExpr: TIDExpression;
  TotalMICount, ItemsCount: Integer;
  ElsePresent: Boolean;
  SEConst: Boolean;
  MISContext: TSContext;
  NeedWriteIL,
  NeedCMPCode: Boolean;
  Implicit: TIDDeclaration;
  ASTExpr: TASTExpression;
  KW: TASTKWCase;
  CaseItem: TASTExpBlockItem;
begin
  KW := SContext.Add(TASTKWCase) as TASTKWCase;

  // NeedCMPCode := False;
  // CASE выражение
  InitEContext(EContext, SContext, ExprRValue);
  Lexer_NextToken(Scope);
  Result := ParseExpression(Scope, SContext, EContext, ASTExpr);
  KW.Expression := ASTExpr;
  CaseExpr := EContext.RPNPopExpression();
  CheckEmptyExpression(CaseExpr);

  var WasCall := False;
  CaseExpr := CheckAndCallFuncImplicit(SContext, CaseExpr, WasCall);

  {if Assigned(EContext.LastBoolNode) then
    Bool_CompleteImmediateExpression(EContext, SExpression);}
  SEConst := CaseExpr.IsConstant;
  Lexer_MatchToken(Result, token_of);
  ItemsCount := 0;
  TotalMICount := 0;
  NeedWriteIL := True;
  ElsePresent := False;
  Result := Lexer_NextToken(Scope);

  while Result <> token_end do
  begin
    InitEContext(EContext, SContext, ExprRValue);
    if Result <> token_else then
    begin
      while True do begin
        //CFBBegin(SContext, CFB_CASE_ENTRY);
        Result := ParseExpression(Scope, SContext, EContext, ASTExpr);

        CaseItem := KW.AddItem(ASTExpr);
        MISContext := SContext.MakeChild(Scope, CaseItem.Body);

        ItemExpr := EContext.RPNPopExpression();
        CheckEmptyExpression(ItemExpr);
        // проверка на совпадение типа
        if ItemExpr.DataTypeID = dtRange then
          Implicit := CheckImplicit(SContext, CaseExpr, TIDRangeType(ItemExpr.DataType).BaseType)
        else
          Implicit := CheckImplicit(SContext, ItemExpr, CaseExpr.DataType);
        if not Assigned(Implicit) then
          if not Assigned(Implicit) then
            AbortWork(sMatchExprTypeMustBeIdenticalToCaseExprFmt, [ItemExpr.DataTypeName, CaseExpr.DataTypeName], ItemExpr.TextPosition);

        // проверяем на константу
        if ItemExpr.IsConstant and SEConst then
        begin
          // если данное выражение истенно, то для остальных генерировать IL код не нужно
          if ((ItemExpr.DataTypeID = dtRange) and IsConstValueInRange(CaseExpr, TIDRangeConstant(ItemExpr.Declaration)))
              or IsConstEqual(CaseExpr, ItemExpr) then
          begin
            NeedWriteIL := False;
          end else
            NeedWriteIL := True;
          NeedCMPCode := False;
        end else begin
          //MISContext.WriteIL := NeedWriteIL;
          NeedCMPCode := NeedWriteIL;
          if NeedCMPCode then
          begin
            SetLength(MatchItems, ItemsCount + 1);
            MatchItem := @MatchItems[ItemsCount];
            MatchItem.Expression := ItemExpr;

            // код проверки условия
            {if DExpression.DataTypeID <> dtRange then
            begin

            end else
              Process_operator_In(EContext, SExpression, DExpression);}
            Inc(ItemsCount);
          end;
        end;
        CheckUniqueMIExpression(ItemExpr);

        {if Assigned(EContext.LastBoolNode) and Assigned(EContext.LastBoolNode.PrevNode) then
          Bool_AddExprNode(EContext, ntOr);}

        // если была запятая, парсим следующее выражение
        if Result <> token_coma then
          break;

        Lexer_NextToken(Scope);
      end;
      // двоеточие
      Lexer_MatchToken(Result, token_colon);
      Lexer_NextToken(Scope);
      // корректируем переходы
      //Bool_CompleteExpression(EContext.LastBoolNode, JMPToEnd);
      // парсим код секции
      Result := ParseStatements(Scope, MISContext, {IsBlock:} False);

      // the last statement/block in the CASE statement doesn't require semicolon
      if not (Result in [token_end, token_else]) then
      begin
        Lexer_MatchToken(Result, token_semicolon);
        Result := Lexer_NextToken(Scope);
      end;

      Inc(TotalMICount);

    end else begin
      // ELSE секция
      MISContext := SContext.MakeChild(Scope, KW.ElseBody);
      Lexer_NextToken(Scope);
      Result := ParseStatements(Scope, MISContext, {IsBlock:} True);
      Lexer_MatchToken(Result, token_end);
      Result := Lexer_NextToken(Scope);
      ElsePresent := True;
      Inc(TotalMICount);
      Break;
    end;
  end;
  // проверяем есть ли хоть одна секция(включая ELSE) в кейсе
  if TotalMICount = 0 then
    AbortWork(sCaseStmtRequireAtLeastOneMatchExpr, Lexer_PrevPosition);

  // если небыло ELSE секции, парсим END;
  if not ElsePresent then begin
    Lexer_MatchToken(Result, token_end);
    Result := Lexer_NextToken(Scope);
  end;
end;

function TASTDelphiUnit.ParseClassAncestorType(Scope: TScope; ClassDecl: TIDClass): TTokenID;
var
  Expr: TIDExpression;
  Decl: TIDType;
begin
  var LAncestorsCount := 0;
  while True do begin
    Lexer_NextToken(Scope);
    Result := ParseConstExpression(Scope, Expr, ExprNested);
    if Assigned(Expr) then
    begin
      CheckClassOrIntfType(Expr);
      Decl := Expr.AsType;
      // проверка на зацикливание на себя
      if Decl = ClassDecl then
        AbortWork(sRecurciveTypeLinkIsNotAllowed, Expr.TextPosition);
    end else begin
      ERRORS.CLASS_OR_INTF_TYPE_REQUIRED(Lexer_PrevPosition);
      Decl := nil;
    end;

    if (Decl.DataTypeID = dtClass) then
    begin
      if LAncestorsCount = 0 then
      begin
        ClassDecl.AncestorDecl := Decl;
      end else
        AbortWork('Multiple inheritance is not supported', Expr.TextPosition);
    end else
    begin
      var LIntfDecl: TIDInterface;
      if Decl is TIDGenericInstantiation then
      begin
        LIntfDecl := TIDGenericInstantiation(Decl).Original as TIDInterface;
        ClassDecl.AddGenericInterface(TIDGenericInstantiation(Decl));
      end else
        LIntfDecl := TIDInterface(Decl);

      if ClassDecl.FindInterface(LIntfDecl) then
        ERRORS.INTF_ALREADY_IMPLEMENTED(Expr);
      ClassDecl.AddInterface(LIntfDecl);
    end;

    Inc(LAncestorsCount);
    if Result = token_coma then
      Continue;
    Break;
  end;
  Lexer_MatchToken(Result, token_closeround);
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ParseClassOfType(Scope: TScope; const ID: TIdentifier; out Decl: TIDClassOf): TTokenID;
var
  RefType: TIDClass;
  Expr: TIDExpression;
  LRefID: TIdentifier;
begin
  Lexer_ReadNextIdentifier(Scope, {out} LRefID);
  RefType := TIDClass(FindIDNoAbort(Scope, LRefID));
  // use the target type if it has been declared in the same unit
  if Assigned(RefType) and (RefType.Scope.DeclUnit = Self) then
  begin
    Result := ParseConstExpression(Scope, Expr, ExprRValue);
    CheckClassType(Expr);
    RefType := TIDClass(Expr.Declaration);
    Decl := TIDClassOf.Create(Scope, ID);
    Decl.ReferenceType := RefType;
    Scope.AddType(Decl);
  end else begin
    // if not, treat this as forward declaration
    Decl := TIDClassOf.Create(Scope, ID);
    Decl.ForwardID := LRefID;
    Decl.NeedForward := True;
    fForwardTypes.Add(Decl);
    if ID.Name <> '' then
      Scope.AddType(Decl);
    Result := Lexer_NextToken(Scope);
  end;
end;

function TASTDelphiUnit.ParseGenericTypeDecl(Scope: TScope;
                                             GDescriptor: PGenericDescriptor;
                                             const ID: TIdentifier;
                                             ATypeClass: TIDTypeClass): TIDType;
var
  LFwdDecl: TIDType;
begin
  Result := nil;

  if ID.Name <> '' then
  begin
    // search for forward declarations first
    LFwdDecl := TIDType(Scope.FindID(ID.Name));

    // TODO: accept forward declarations in the same scope only
    if Assigned(LFwdDecl) then
    begin
      // check existing declration is a forward declaration (none: can be an alias)
      if (LFwdDecl.Original is ATypeClass) then
      begin
        while True do
        begin
          var LFwdGDescriptor := TIDClass(LFwdDecl).GenericDescriptor;
          var LSameGenericParams :=
            (not Assigned(LFwdGDescriptor) and not Assigned(GDescriptor)) or
            (Assigned(LFwdGDescriptor) and Assigned(GDescriptor) and LFwdGDescriptor.IsEqual(GDescriptor));

          if LSameGenericParams then
          begin
            if LFwdDecl.NeedForward then
            begin
              // the forward declaration found
              LFwdDecl.NeedForward := False;
              fForwardTypes.Remove(LFwdDecl);
              Result := LFwdDecl;
              Break;
            end else
              ERRORS.ID_REDECLARATED(ID);
          end;
          if Assigned(LFwdDecl.NextGenericOverload) then
            LFwdDecl := TIDType(LFwdDecl.NextGenericOverload)
          else
            Break;
        end;
      end else
        ERRORS.ID_REDECLARATED(ID);
    end;
  end;

  if not Assigned(Result) then
  begin
    Result := ATypeClass.Create(Scope, ID);
    // for generic types, we need to move all generic params into static-members scope
    if Assigned(GDescriptor) and (Result is TIDStructure) then
      TIDStructure(Result).Members.AssingFrom(GDescriptor.Scope);

    Result.GenericDescriptor := GDescriptor;

    if ID.Name <> '' then
    begin
      // if this is a first declaration with this name, add to scope, otherwise - add to overloads
      if not Assigned(LFwdDecl) then
        Scope.AddType(Result)
      else
        LFwdDecl.AddGenecricOverload(Result);
    end;
  end;
end;

function TASTDelphiUnit.ParseVisibilityModifiers(Scope: TScope; var AVisibility: TVisibility; AIsClass: Boolean): TTokenID;
begin
  Result := Lexer_CurTokenID;
  while True do
    case Lexer_AmbiguousId of
      tokenD_published: begin
        if AIsClass then
        begin
          AVisibility := vPublic;
          Result := Lexer_NextToken(Scope);
        end else
          AbortWork('E2184 PUBLISHED section valid only in class types', Lexer_Position);
      end;
      tokenD_public: begin
        AVisibility := vPublic;
        Result := Lexer_NextToken(Scope);
      end;
      tokenD_private: begin
        AVisibility := vPrivate;
        Result := Lexer_NextToken(Scope);
      end;
      tokenD_protected: begin
        AVisibility := vProtected;
        Result := Lexer_NextToken(Scope);
      end;
      tokenD_strict: begin
        Result := Lexer_NextReseredToken(Scope);
        case Result of
          tokenD_private: AVisibility := vStrictPrivate;
          tokenD_protected: AVisibility := vStrictProtected;
        else
          ERRORS.EXPECTED_TOKEN(tokenD_private, Result);
        end;
        Result := Lexer_NextToken(Scope);
      end;
    else
      Break;
    end;
end;

function TASTDelphiUnit.ParseClassType(Scope: TScope; GDescriptor: PGenericDescriptor; const ID: TIdentifier;
  out Decl: TIDClass): TTokenID;
var
  Visibility: TVisibility;
  //Ancestor: TIDClass;
begin
  Visibility := vPublic;

  Result := Lexer_AmbiguousId;

  if Result = tokenD_helper then
  begin
    Result := ParseTypeHelper(Scope, nil, GDescriptor, ID, {out} TDlphHelper(Decl));
    Exit;
  end;


  Decl := TIDClass(ParseGenericTypeDecl(Scope, GDescriptor, ID, TIDClass));

  if (Self = SYSUnit) and (ID.Name = 'TObject') then
    Sys._TObject := Decl;


  // semicolon means this is forward declaration
  if Result = token_semicolon then
  begin
    if Decl.NeedForward then
      ERRORS.ID_REDECLARATED(ID);
    Decl.NeedForward := True;
    fForwardTypes.Add(Decl);
    Exit;
  end;

  if Lexer_IsCurrentToken(tokenD_abstract) then
  begin
    // class is abstract
    Decl.ClassDeclFlags := Decl.ClassDeclFlags + [cdfAbstract];
    Result := Lexer_NextToken(Scope);
  end else
  if Result = tokenD_sealed then
  begin
    // class is sealed
    Decl.ClassDeclFlags := Decl.ClassDeclFlags + [cdfSealed];
    Result := Lexer_NextToken(Scope);
  end;

  if Result = token_openround then
  begin
    Result := ParseClassAncestorType(Decl.Members, Decl);
  end else begin
    if Self <> SYSUnit then
      Decl.AncestorDecl := Sys._TObject;
  end;

  // semicolon means a short declaration (without end)
  if Result = token_semicolon then
  begin
    Decl.StructFlags := Decl.StructFlags + [StructCompleted];
    //Result := Lexer_NextToken(Scope);
    Exit;
  end;

  while True do begin
    Result := ParseVisibilityModifiers(Scope, {var} Visibility, {AIsClass:} True);
    case Result of
      token_openblock: Result := ParseAttribute(Scope);
      token_class: Result := ParseTypeMember(Scope, Decl);
      token_procedure: Result := ParseProcedure(Decl.Members, ptProc, Decl);
      token_function: Result := ParseProcedure(Decl.Members, ptFunc, Decl);
      token_property: Result := ParseProperty(Decl.Members, Decl);
      token_constructor: Result := ParseProcedure(Decl.Members, ptConstructor, Decl);
      token_destructor: begin
        Result := ParseProcedure(Decl.Members, ptDestructor, Decl);
      end;
      token_var: begin
        Lexer_NextToken(Scope);
        Result := ParseFieldsSection(Decl.Members, Visibility, Decl, False);
      end;
      token_const: Result := ParseConstSectionInStruct(Decl.Members);
      token_type: Result := ParseTypeSectionInStruct(Decl.Members);
      token_identifier: begin
        Result := ParseFieldsSection(Decl.Members, Visibility, Decl, False);
      end
      else break;
    end;
  end;
  CheckIncompleteType(Decl.Members);
  Decl.StructFlags := Decl.StructFlags + [StructCompleted];
  Lexer_MatchToken(Result, token_end);
  Result := Lexer_NextToken(Scope);
end;

procedure TASTDelphiUnit.CheckLeftOperand(const Status: TRPNStatus);
begin
  if Status <> rpOperand then
    ERRORS.EXPRESSION_EXPECTED;
end;

function TASTDelphiUnit.ParseExplicitCast(Scope: TScope; const SContext: TSContext; var DstExpression: TIDExpression): TTokenID;
var
  EContext: TEContext;
  SrcExpr: TIDExpression;
  ResExpr: TIDExpression;
  OperatorDecl: TIDDeclaration;
  CallExpr: TIDCallExpression;
  TargetType: TIDType;
  ASTE: TASTExpression;
begin
  InitEContext(EContext, SContext, ExprNested);
  Lexer_NextToken(Scope);
  Result := ParseExpression(Scope, SContext, EContext, ASTE);
  SrcExpr := EContext.RPNPopExpression();

  if Result <> token_closeround then
    ERRORS.INCOMPLETE_STATEMENT('explicit cast');

  TargetType := DstExpression.AsType;
  ResExpr := MatchExplicit(SContext, SrcExpr, TargetType, OperatorDecl);
  Result := Lexer_NextToken(Scope);

  if Assigned(ResExpr) then
  begin
    DstExpression := ResExpr;
    Exit;
  end;

  if Assigned(OperatorDecl) then
  begin
    if OperatorDecl.ItemType = itType then
    begin
      if (SrcExpr.ItemType = itConst) and (SrcExpr.IsAnonymous) then
      begin
        TIDConstant(SrcExpr.Declaration).ExplicitDataType := OperatorDecl as TIDType;
        DstExpression := TIDExpression.Create(SrcExpr.Declaration);
      end else
        DstExpression := TIDCastExpression.Create(SrcExpr.Declaration, TIDType(DstExpression.Declaration), DstExpression.TextPosition);
    end else
    if OperatorDecl.ItemType = itProcedure then
    begin
      // вызываем explicit-оператор
      CallExpr := TIDCallExpression.Create(OperatorDecl, DstExpression.TextPosition);
      CallExpr.ArgumentsCount := 1;
      DstExpression := Process_CALL_direct(SContext, CallExpr, TIDExpressions.Create(SrcExpr));
    end else
    if OperatorDecl.ItemType = itSysOperator then
    begin
      ResExpr := TSysTypeCast(OperatorDecl).Match(SContext, SrcExpr, TargetType);
      if not Assigned(ResExpr) then
      begin
        // for debug
        TSysTypeCast(OperatorDecl).Match(SContext, SrcExpr, TargetType);
        ERRORS.INVALID_EXPLICIT_TYPECAST(SrcExpr, TargetType);
      end;
    end;
  end else
  begin
    // for debug  
    MatchExplicit(SContext, SrcExpr, TargetType, OperatorDecl);
    ERRORS.INVALID_EXPLICIT_TYPECAST(SrcExpr, TargetType);
  end;
end;

function TASTDelphiUnit.ParseExpression(Scope: TScope; const SContext: TSContext; var EContext: TEContext;
                                        out ASTE: TASTExpression): TTokenID;
var
  ID: TIdentifier;
  Status: TRPNStatus;
  Expr: TIDExpression;
  RoundCount: Integer;
  RecordInitResultExpr: TIDExpression;
begin
  Expr := nil;
  Status := rprOk;
  RoundCount := 0;
  RecordInitResultExpr := nil;
  // treat any ambiguous token as identifier here
  Result := Lexer_CurTokenID;
  ASTE := TASTExpression.Create(nil);
  while True do begin
    case Result of
      token_eof: Break;
      token_openround: begin
        if Status = rpOperand then
        begin
          Result := ParseMemberCall(Scope, EContext, ASTE);
          Status := rpOperand;
          continue;
        end else begin
          Inc(RoundCount);
          EContext.RPNPushOpenRaund;
          ASTE.AddSubItem(TASTOpOpenRound);
          Status := rprOk;
        end;
      end;
      token_closeround: begin
        Dec(RoundCount);
        if RoundCount < 0 then
        begin
          if EContext.EPosition <> ExprLValue then
            Break;

          ERRORS.UNNECESSARY_CLOSED_ROUND;
        end;
        ASTE.AddSubItem(TASTOpCloseRound);
        EContext.RPNPushCloseRaund();
        Status := rpOperand;
      end;
      token_openblock: begin
        if Status = rpOperand then
        begin
          Result := ParseArrayMember(Scope, EContext, ASTE);
          continue;
        end else
          ParseVector(Scope, EContext);

        Status := rpOperand;
      end;
      token_closeblock: begin
        if EContext.EPosition = ExprNested then
          Break
        else
          ERRORS.UNNECESSARY_CLOSED_BLOCK;
      end;
      token_plus: begin
        if Status = rpOperand then
          Status := EContext.RPNPushOperator(opAdd)
        else
          Status := EContext.RPNPushOperator(opPositive);
        ASTE.AddSubItem(TASTOpPlus);
      end;
      token_minus: begin
        if Status = rpOperand then
          Status := EContext.RPNPushOperator(opSubtract)
        else
          Status := EContext.RPNPushOperator(opNegative);
        ASTE.AddSubItem(TASTOpMinus);
      end;
      token_equal: begin
        if EContext.EPosition = ExprType then
          break;

        CheckLeftOperand(Status);

        Status := EContext.RPNPushOperator(opEqual);
        ASTE.AddSubItem(TASTOpEqual);
      end;
      token_colon: begin
        // for now just skip
        if EContext.EPosition <> ExprNested then
          break;
        // todo: add parsiong args for Str() like: Val:X:Y
        Status := rpOperation;
      end;
      token_notequal: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opNotEqual);
        ASTE.AddSubItem(TASTOpNotEqual);
      end;
      token_less: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opLess);
        ASTE.AddSubItem(TASTOpLess);
      end;
      token_lessorequal: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opLessOrEqual);
        ASTE.AddSubItem(TASTOpLessEqual);
      end;
      token_above: begin
        if EContext.EPosition = ExprNestedGeneric then
          Break;
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opGreater);
        ASTE.AddSubItem(TASTOpGrater);
      end;
      token_aboveorequal: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opGreaterOrEqual);
        ASTE.AddSubItem(TASTOpGraterEqual);
      end;
      token_asterisk: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opMultiply);
        ASTE.AddSubItem(TASTOpMul);
      end;
      token_in: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opIn);
      end;
      token_slash: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opDivide);
        ASTE.AddSubItem(TASTOpDiv);
      end;
      token_div: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opIntDiv);
        ASTE.AddSubItem(TASTOpIntDiv);
      end;
      token_mod: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opModDiv);
        ASTE.AddSubItem(TASTOpMod);
      end;
      token_period: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opPeriod);
      end;
      token_address: begin
        Status := EContext.RPNPushOperator(opAddr);
      end;
      token_caret: begin
        // call the Process_operator_Deref directly
        Expr := Process_operator_Deref(EContext);
        EContext.RPNPushExpression(Expr);
        Status := rpOperand;
      end;
      token_and: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opAnd);
      end;
      token_or: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opOr);
      end;
      token_xor: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opXor);
      end;
      token_not: begin
        Status := EContext.RPNPushOperator(opNot);
      end;
      token_shl: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opShiftLeft);
        ASTE.AddSubItem(TASTOpShl);
      end;
      token_shr: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opShiftRight);
        ASTE.AddSubItem(TASTOpShr);
      end;
      token_is: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opIs);
        ASTE.AddSubItem(TASTOpCastCheck);
      end;
      token_as: begin
        CheckLeftOperand(Status);
        Status := EContext.RPNPushOperator(opAs);
        ASTE.AddSubItem(TASTOpDynCast);
      end;
      token_procedure, token_function: begin
        if Status = rpOperand then
          break;
        Result := ParseAnonymousProc(Scope, EContext, SContext, Result);
        Status := rpOperand;
        continue;
      end;
      token_inherited: begin
        //CheckLeftOperand(Status);
        Result := ParseInheritedStatement(Scope, EContext);
        Status := rpOperand;
        continue;
      end;
      token_dot: begin
        Result := ParseMember(Scope, EContext, ASTE);
        Status := rpOperand;
        continue;
      end;
      token_identifier{, token_id_or_keyword}: begin
        // есил встретился подряд воторой идентификатор, то выходим
        if Status = rpOperand then
          Break;
        if Lexer_IdentifireType = itIdentifier then
        begin
          Expr := nil;
          Result := ParseIdentifier(Scope, nil, Expr, EContext, nil, ASTE);
          // если результат = nil значит это был вызов функции и все
          // необходимые параметры погружены в стек, поэтому идем дальше
          if not Assigned(Expr) then
          begin
            Status := rpOperand;
            continue;
          end;

          // the record default value init
          if (Result = token_colon) and (Scope is TRecordInitScope) then
          begin
            if not Assigned(RecordInitResultExpr) then
            begin
              var Decl := TIDRecordConstant.CreateAsAnonymous(Scope, TRecordInitScope(Scope).Struct, nil);
              //Decl.DataType := TRecordInitScope(Scope).Struct;
              RecordInitResultExpr := TIDExpression.Create(Decl, Expr.TextPosition);
              EContext.RPNPushExpression(RecordInitResultExpr);
            end;
            Result := ParseRecordInitValue(TRecordInitScope(Scope), Expr);
            Continue;
          end;
        end else begin
          {анонимная константа}
          Lexer_ReadCurrIdentifier(ID);
          Expr := CreateAnonymousConstant(Scope, EContext, ID, Lexer_IdentifireType);
          Result := Lexer_NextToken(Scope);
          ASTE.AddDeclItem(Expr.Declaration, Expr.TextPosition);
        end;
        EContext.RPNPushExpression(Expr);
        Status := rpOperand;
        Continue;
      end;
    else
      Break;
    end;
    Result := Lexer_NextToken(Scope);
  end;

  if (EContext.EPosition <> ExprNested) and (Status <> rpOperand) and NeedRValue(EContext.RPNLastOp) then
    ERRORS.EXPRESSION_EXPECTED;

  if Assigned(Expr) and (Expr is TUnknownIDExpression) then
    ERRORS.UNDECLARED_ID(TUnknownIDExpression(Expr).ID);

  EContext.RPNFinish();
end;

constructor TASTDelphiUnit.Create(const Project: IASTProject; const FileName: string; const Source: string);
var
  Scope: TProcScope;
begin
  inherited Create(Project, FileName, Source);

  fDefines := TDefines.Create();
  fPackage := Project as IASTDelphiProject;
  fUnitSContext := TSContext.Create(Self, IntfScope);
  fErrors := TASTDelphiErrors.Create(Lexer);
  fCache := TDeclCache.Create;
  fForwardTypes := TList<TIDType>.Create;

  FOptions := TDelphiOptions.Create(Package.Options);

  fCondStack := TSimpleStack<Boolean>.Create(0);
  fCondStack.OnPopError := procedure begin ERRORS.INVALID_COND_DIRECTIVE end;

  FInitProc := TASTDelphiProc.CreateAsSystem(ImplScope, '$initialization');
  TASTDelphiProc(FInitProc).Body := TASTBlock.Create(FInitProc);

  FFinalProc := TASTDelphiProc.CreateAsSystem(ImplScope, '$finalization');
  TASTDelphiProc(FFinalProc).Body := TASTBlock.Create(FFinalProc);
end;

function TASTDelphiUnit.CreateAnonymousConstant(Scope: TScope; var EContext: TEContext; const ID: TIdentifier;
                                         IdentifierType: TIdentifierType): TIDExpression;
var
  i: Integer;
  IntValue: Int64;
  UInt64Value: UInt64;
  Int32Value: Int32;
  FltValue: Extended;
  DataType: TIDType;
  Value: string;
  CItem: TIDConstant;
  Chars: TStringDynArray;
begin
  CItem := nil;
  Value := ID.Name;
  case IdentifierType of
    itChar: CItem := TIDCharConstant.CreateAsAnonymous(Scope, Sys._WideChar, Value[1]);
    itString: begin
        // если чарсет метаданных равен ASCII, все строковые константы
        // удовлетворающе набору ASCII, создаются по умолчанию с типом AnsiString
        if (Package.RTTICharset = RTTICharsetASCII) and IsAnsiString(Value) then
          DataType := Sys._AnsiString
        else
          DataType := Sys._UnicodeString;
      CItem := TIDStringConstant.CreateAsAnonymous(Scope, DataType, Value);
    end;
    itInteger: begin
      if Value[1] = '#' then begin
        Value := Copy(Value, 2, Length(Value) - 1);
        if TryStrToInt(Value, Int32Value) then
          CItem := TIDCharConstant.CreateAsAnonymous(Scope, Sys._WideChar, Char(Int32Value))
        else
          AbortWorkInternal('int convert error', Lexer_Position);
      end else begin
        if not TryStrToInt64(Value, IntValue) then
        begin
          if (EContext.RPNLastOperator = TOperatorID.opNegative) then
          begin
            // хак для обработки MinInt64 !!!
            if TryStrToInt64('-' + Value, IntValue) then
              EContext.RPNEraiseTopOperator
            else
              AbortWork('Invalid decimal value: %s', [Value], Lexer_Position);
          end else
          if TryStrToUInt64(Value, UInt64Value) then
          begin
            IntValue := Int64(UInt64Value);
          end else
            AbortWork('Invalid decimal value: %s', [Value], Lexer_Position);
        end;
        DataType := Sys.DataTypes[GetValueDataType(IntValue)];
        CItem := TIDIntConstant.CreateAsAnonymous(Scope, DataType, IntValue);
      end;
    end;
    itFloat: begin
      // possible issue under WIN64 target
      // https://quality.embarcadero.com/browse/RSP-20333
      if not TryStrToFloat(Value, {out} FltValue) then
        FltValue := 0;
      DataType := Sys.DataTypes[GetValueDataType(FltValue)];
      CItem := TIDFloatConstant.CreateAsAnonymous(Scope, DataType, FltValue);
    end;
    itHextNumber: begin
      try
        IntValue := HexToInt64(Value);
      except
        ERRORS.INVALID_HEX_CONSTANT;
        IntValue := 0;
      end;
      DataType := Sys.DataTypes[GetValueDataType(IntValue)];
      CItem := TIDIntConstant.CreateAsAnonymous(Scope, DataType, IntValue);
    end;
    itBinNumber: begin
      try
        IntValue := BinStringToInt64(Value);
      except
        ERRORS.INVALID_BIN_CONSTANT;
        IntValue := 0;
      end;
      DataType := Sys.DataTypes[GetValueDataType(IntValue)];
      CItem := TIDIntConstant.CreateAsAnonymous(Scope, DataType, IntValue);
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
        if (Package.RTTICharset = RTTICharsetASCII) and IsAnsiString(Value) then
          DataType := Sys._AnsiString
        else
          DataType := Sys._UnicodeString;

        CItem := TIDStringConstant.CreateAsAnonymous(Scope, DataType, Value);
      end else
      // this is a char
        CItem := TIDCharConstant.CreateAsAnonymous(Scope, Sys._WideChar, Char(StrToInt(Chars[0])));
    end;
  else
    ERRORS.INTERNAL;
    CItem := nil;
  end;
  Result := TIDExpression.Create(CItem, ID.TextPosition);
end;

function TASTDelphiUnit.ParseForInStatement(Scope: TScope; const SContext: TSContext; LoopVar: TIDExpression): TTokenID;
var
  EContext: TEContext;
  LExpr: TIDExpression;
  LoopArrayDT: TIDType;
  ASTExpr: TASTExpression;
  KW: TASTKWForIn;
  BodySContext: TSContext;
  AWasCall: Boolean;
begin
  ASTExpr := TASTExpression.Create(nil);
  ASTExpr.AddDeclItem(LoopVar.Declaration, LoopVar.TextPosition);
  KW := SContext.Add(TASTKWForIn) as TASTKWForIn;
  KW.VarExpr := ASTExpr;
  // парсим выражение-коллекцию
  InitEContext(EContext, SContext, ExprRValue);
  Lexer_NextToken(Scope);
  Result := ParseExpression(Scope, SContext, EContext, ASTExpr);
  KW.ListExpr := ASTExpr;
  LExpr := EContext.Result;

  LExpr := CheckAndCallFuncImplicit(SContext, LExpr, {out} AWasCall);

  // expression is array:
  if (LExpr.DataType is TIDArray) then
  begin
    LoopArrayDT := (LExpr.DataType as TIDArray).ElementDataType;
    if Assigned(LoopVar.DataType) then
    begin
      // match loop var type to the array element type
      if MatchImplicit(LoopVar.DataType, LoopArrayDT) = nil then
        ERRORS.INCOMPATIBLE_TYPES(LoopVar, LoopArrayDT);
    end else begin
      LoopVar.Declaration.DataType := LoopArrayDT;
    end;
  end else
  // expression has enumerator:
  if LExpr.DataType is TIDStructure then
  begin
    var LStruct := TIDStructure(LExpr.DataType);
    var LCurrentProp: TIDProperty;
    if LStruct.GetEnumeratorSupported({out} LCurrentProp) then
    begin
      // to do: check enumerator
      if Assigned(LoopVar.DataType) then
      begin
        // match loop var type to the enumerator element type
        if MatchImplicit(LoopVar.DataType, LCurrentProp.DataType) = nil then
          ERRORS.INCOMPATIBLE_TYPES(LoopVar, LCurrentProp.DataType);
      end else begin
        LoopVar.Declaration.DataType := LCurrentProp.DataType;
      end;
    end else
      ERRORS.ARRAY_EXPRESSION_REQUIRED(LExpr);
  end else
    ERRORS.ARRAY_EXPRESSION_REQUIRED(LExpr);

  Lexer_MatchToken(Result, token_do);
  Lexer_NextToken(Scope);

  BodySContext := SContext.MakeChild(Scope, KW.Body);
  Result := ParseStatements(Scope, BodySContext, {IsBlock:} False);
end;

type
  TILCondition = (cNone, cEqual, cNotEqual, cGreater, cGreaterOrEqual, cLess, cLessOrEqual, cZero, cNonZero);

function TASTDelphiUnit.ParseForStatement(Scope: TScope; const SContext: TSContext): TTokenID;
var
  EContext: TEContext;
  BodySContext: TSContext;
  ID: TIdentifier;
  LoopVar: TIDDeclaration;
  LExpr, StartExpr, StopExpr: TIDExpression;
  NewScope: TScope;
  KW: TASTKWFor;
  JMPCondition: TILCondition;
  ASTExpr: TASTExpression;
  WriteIL: Boolean;
begin
  // цикловая переменная
  Result := Lexer_NextToken(Scope);
  if Result = token_var then begin
    Lexer_ReadNextIdentifier(Scope, ID);
    NewScope := TForScope.Create(stLocal, Scope);
    LoopVar := TIDVariable.Create(NewScope, ID);
    NewScope.AddVariable(TIDVariable(LoopVar));
    Scope := NewScope;
  end else begin
    Lexer_ReadCurrIdentifier(ID);
    LoopVar := FindID(Scope, ID);
  end;

  InitEContext(EContext, SContext, ExprRValue);
  // заталкиваем в стек левое выражение
  LExpr := TIDExpression.Create(LoopVar, ID.TextPosition);
  EContext.RPNPushExpression(LExpr);

  Result := Lexer_NextToken(Scope);

  {если это цикл for ... in ...}
  if Result = token_in then
  begin
    Result := ParseForInStatement(Scope, SContext, LExpr);
    Exit;
  end;

  KW := SContext.Add(TASTKWFor) as TASTKWFor;
  BodySContext := SContext.MakeChild(Scope, KW.Body);

  Lexer_MatchToken(Result, token_assign);

  if LoopVar.DataType = nil then
    LoopVar.DataType := Sys._Int32
  else
  if (LoopVar.ItemType <> itVar) or not (LoopVar.DataType.IsOrdinal) then
    AbortWork(sForLoopIndexVarsMastBeSimpleIntVar, Lexer_Position);

  // начальное значение
  Lexer_NextToken(Scope);
  Result := ParseExpression(Scope, SContext, EContext, ASTExpr);
  KW.ExprInit := ASTExpr;
  StartExpr := EContext.Result;
  CheckEmptyExpression(StartExpr);

  // пишем инструкцию присваениея начального значения
  EContext.RPNPushOperator(opAssignment);
  EContext.RPNFinish();

  // устанавливаем флаг цикловой переменной
  with TIDVariable(LoopVar) do Flags := Flags + [VarLoopIndex];

   // to/downto keyword
  case Result of
    token_to: JMPCondition := cGreater;
    token_downto: JMPCondition := cLess;
    else begin
      AbortWork(sKeywordToOrDowntoExpected, Lexer_PrevPosition);
      JMPCondition := cNone;
    end;
  end;

  // конечное значение
  InitEContext(EContext, SContext, ExprRValue);
  Lexer_NextToken(Scope);
  Result := ParseExpression(Scope, SContext, EContext, ASTExpr);
  KW.ExprTo := ASTExpr;
  StopExpr := EContext.Result;
  CheckEmptyExpression(StopExpr);

  // если для вычисления конечного выражения использовалась временная переменная
  // помечаем ее как постоянно используемую в блоке FOR цикла
  if StopExpr.IsTMPVar then
    StopExpr.AsVariable.IncludeFlags([VarLoopIndex]);

  // проверка на константы
  if (StartExpr.ItemType = itConst) and
     (StopExpr.ItemType = itConst) then
  begin
    WriteIL := ((JMPCondition = cGreater) and (StartExpr.AsIntConst.Value <= StopExpr.AsIntConst.Value)) or
               ((JMPCondition = cLess) and (StartExpr.AsIntConst.Value >= StopExpr.AsIntConst.Value));
    if not WriteIL then
      Warning(sForOrWhileLoopExecutesZeroTimes, [], StartExpr.TextPosition);
  end;

  // тело цикла
  Lexer_MatchToken(Result, token_do);
  Result := Lexer_NextToken(Scope);
  if Result <> token_semicolon then
    Result := ParseStatements(Scope, BodySContext, {IsBlock:} False);

  // сбрасываем флаг цикловой переменной
  with TIDVariable(LoopVar) do Flags := Flags - [VarLoopIndex];
end;

function TASTDelphiUnit.ParseGenericsArgs(Scope: TScope;
                                          const SContext: TSContext;
                                          out Args: TIDExpressions;
                                          out ACanInstantiate: Boolean): TTokenID;
var
  EContext: TEContext;
  Expr: TIDExpression;
  ArgsCount: Integer;
  ASTE: TASTExpression;
begin
  ArgsCount := 0;
  ACanInstantiate := True;
  while true do begin
    InitEContext(EContext, SContext, ExprNestedGeneric);
    Lexer_NextToken(Scope);
    Result := ParseExpression(Scope, SContext, EContext, ASTE);
    Expr := EContext.Result;
    if Assigned(Expr) then begin
      {if Expr.DataType = Sys._Boolean then
      begin
        if (Expr.ItemType = itVar) and Expr.IsAnonymous then
          Bool_CompleteImmediateExpression(EContext, Expr);
      end;}
    end else
      ERRORS.EXPRESSION_EXPECTED;

    Inc(ArgsCount);
    SetLength(Args, ArgsCount);
    var AArgExpression := EContext.RPNPopExpression();
    var AArgType := AArgExpression.AsType;
    if (AArgType is TIDGenericParam) or AArgType.IsGeneric then
      ACanInstantiate := False;

    Args[ArgsCount - 1] := AArgExpression;

    case Result of
      token_coma: begin
        continue;
      end;
      token_above: begin
        Result := Lexer_NextToken(Scope);
        Break;
      end;
    else
      ERRORS.INCOMPLETE_STATEMENT('generics args');
    end;
  end;

end;

function TASTDelphiUnit.ParseGenericsConstraint(Scope: TScope;
                                                out AConstraint: TGenericConstraint;
                                                out AConstraintType: TIDType): TTokenID;
begin
  AConstraint := gsNone;
  AConstraintType := nil;
  Result := Lexer_NextToken(Scope);
  while true do begin
    case Result of
      token_class: begin
        case AConstraint of
          gsNone: AConstraint := gsClass;
          gsConstructor: AConstraint := gsClassAndConstructor;
        else
          ERRORS.GENERIC_INVALID_CONSTRAINT(Result);
        end;
        AConstraintType := TSYSTEMUnit(SysUnit)._TObject;
      end;
      token_constructor: begin
        case AConstraint of
          gsNone: AConstraint := gsConstructor;
          gsClass: AConstraint := gsClassAndConstructor;
        else
          ERRORS.GENERIC_INVALID_CONSTRAINT(Result);
        end;
        AConstraintType := TSYSTEMUnit(SysUnit)._TObject;
      end;
      token_record: begin
        if AConstraint = gsNone then
        begin
          AConstraint := gsRecord;
          Exit;
        end else
          ERRORS.GENERIC_INVALID_CONSTRAINT(Result);
      end;
      token_identifier{, token_id_or_keyword}: begin
        if AConstraint = gsNone then
        begin
          var AID: TIdentifier;
          Lexer_ReadCurrIdentifier(AID);
          var ADeclaration := FindID(Scope, AID);
          if (ADeclaration.ItemType = itType) and
             (TIDType(ADeclaration).DataTypeID in [dtClass, dtInterface, dtGeneric]) then
          begin
            AConstraintType := TIDType(ADeclaration);
            AConstraint := gsType;
            Exit;
          end;
        end;
        ERRORS.GENERIC_INVALID_CONSTRAINT(Result);
      end;
      token_above: begin
        if AConstraint = gsNone then
          ERRORS.GENERIC_INVALID_CONSTRAINT(Result);
        Exit;
      end;
    else
      ERRORS.GENERIC_INVALID_CONSTRAINT(Result);
    end;
    Result := Lexer_NextToken(Scope);
  end;
end;

function TASTDelphiUnit.ParseGenericsHeader(Scope: TScope; out Args: TIDTypeArray): TTokenID;
var
  ID: TIdentifier;
  ParamsCount, ParamsInGroupCount, ComaCount: Integer;
  ParamDecl: TIDGenericParam;
begin
  ComaCount := 0;
  ParamsCount := 0;
  ParamsInGroupCount := 0;
  while True do begin
    Result := Lexer_NextToken(Scope);
    case Result of
      token_identifier: begin
        Lexer_ReadCurrIdentifier(ID);
        ParamDecl := TIDGenericParam.Create(Scope, ID);
        Scope.AddType(ParamDecl);
        Inc(ParamsCount);
        Inc(ParamsInGroupCount);
        SetLength(Args, ParamsCount);
        Args[ParamsCount - 1] := ParamDecl;
      end;
      token_colon: begin
        if ParamsInGroupCount = 0 then
          ERRORS.IDENTIFIER_EXPECTED();
        // parse generic constraint
        var AConstraint: TGenericConstraint;
        var AConstraintType: TIDType;
        Result := ParseGenericsConstraint(Scope, {out} AConstraint, {out} AConstraintType);
        // set constraint to all params in the group (like in <A, B, C: class>)
        for var AIndex := ParamsInGroupCount - 1 downto 0 do
        begin
          ParamDecl := TIDGenericParam(Args[ParamsInGroupCount - ParamsCount]);
          ParamDecl.Constraint := AConstraint;
          ParamDecl.ConstraintType := AConstraintType;
        end;
        if Result = token_above then
          Break;
        ParamsInGroupCount := 0;
      end;
      token_coma: begin
        if ComaCount >= ParamsCount then
          ERRORS.IDENTIFIER_EXPECTED();
        Inc(ComaCount);
      end;
      token_above: begin
        if ParamsCount = 0 then
          AbortWork(sNoOneTypeParamsWasFound, Lexer_PrevPosition);

        if ComaCount >= ParamsCount then
          ERRORS.IDENTIFIER_EXPECTED();
        Break;
      end;
    else
      ERRORS.IDENTIFIER_EXPECTED();
    end;
  end;
  Result := Lexer_NextToken(Scope);
end;

function IsTheStructIsOwner(AScope: TScope; AStruct: TIDType): Boolean;
begin
  while Assigned(AScope) do
  begin
    if (AScope.ScopeType = stStruct) and (TStructScope(AScope).Struct = AStruct) then
      Exit(True);
    AScope := AScope.Parent;
  end;
  Result := False;
end;

function TASTDelphiUnit.ParseGenericTypeSpec(Scope, ASearchScope: TScope; const ID: TIdentifier; out DataType: TIDType): TTokenID;

  function MakeGenericName(const AGenericArgs: TIDExpressions): string;
  begin
    var LParamsStr := '';
    for var LArg in AGenericArgs do
      LParamsStr := AddStringSegment(LParamsStr, LArg.DisplayName, ', ');
    Result := ID.Name + '<' + LParamsStr + '>';
  end;

var
  LGenericArgs: TIDExpressions;
  LCanInstantiate: Boolean;
begin
  DataType := nil;
  Result := ParseGenericsArgs(Scope, fUnitSContext, {out} LGenericArgs, {out} LCanInstantiate);
  // find all types with the same name
  var LDeclArray := FindAll(ASearchScope, ID);
  for var LIndex := 0 to Length(LDeclArray) - 1 do
  begin
    var LDecl := LDeclArray[LIndex];
    if LDecl.ItemType = itType then
    begin
      var LType := TIDType(LDecl);
      // each generic types can have overloads, find proper one
      while Assigned(LType) do
      begin
        // searching proper generic declaration
        if Assigned(LType.GenericDescriptor) and
          (LType.GenericDescriptor.ParamsCount = Length(LGenericArgs)) then
         Break;

        LType := TIDType(LType.NextGenericOverload);
      end;

      // if LType is assigned, we found proper declaration
      if Assigned(LType) then
      begin
        if LCanInstantiate and not IsGenericTypeThisStruct(Scope, LType) then
        begin
          DataType := InstantiateGenericType(Scope, LType, LGenericArgs);
        end else
        begin
          // check if this type is not the current parsed type
          if not IsTheStructIsOwner(Scope, LType) then
            DataType := TIDGenericInstantiation.CreateInstantiation(Scope, LType as TIDType, LGenericArgs)
          else
            DataType := LType;
        end;
        Break;
      end;
    end;
  end;
  if not Assigned(DataType) then
    ERRORS.UNDECLARED_ID(MakeGenericName(LGenericArgs), ID.TextPosition);
end;

function TASTDelphiUnit.ParseGoToStatement(Scope: TScope; const SContext: TSContext): TTokenID;
var
  ID: TIdentifier;
  LDecl: TIDDeclaration;
  KW: TASTKWGoTo;
begin
  Lexer_ReadNextIdentifier(Scope, ID);
  LDecl := FindID(Scope, ID);
  CheckLabelExpression(LDecl);
  KW := SContext.Add(TASTKWGoTo) as TASTKWGoTo;
  KW.LabelDecl := LDecl;
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ParseIfThenStatement(Scope: TScope; const SContext: TSContext): TTokenID;
var
  Expression: TIDExpression;
  EContext: TEContext;
  ThenSContext: TSContext;
  ElseSContext: TSContext;
  NewScope: TScope;
  KW: TASTKWIF;
  CondExpr: TASTExpression;
begin
  KW := SContext.Add(TASTKWIF) as TASTKWIF;
  InitEContext(EContext, SContext, ExprRValue);
  Lexer_NextToken(Scope);
  Result := ParseExpression(Scope, SContext, EContext, CondExpr);
  KW.Expression := CondExpr;
  CheckAndCallFuncImplicit(EContext);
  Expression := EContext.Result;
  CheckEmptyExpression(Expression);
  CheckBooleanExpression(Expression);

  {then section}
  Lexer_MatchToken(Result, token_then);
  Result := Lexer_NextToken(Scope);
  if Result <> token_semicolon then
  begin
    {OPT: do not create an empty scope in case begin..end exists}
    if Result <> token_begin then
      NewScope := TIfThenScope.Create(stLocal, Scope)
    else
      NewScope := Scope;

    ThenSContext := SContext.MakeChild(Scope, KW.ThenBody);

    Result := ParseStatements(NewScope, ThenSContext, {IsBlock:} False);
  end;
  { else section}
  if Result = token_else then
  begin
    Result := Lexer_NextToken(Scope);
    {OPT: do not create an empty scope in case begin..end exists}
    if Result <> token_begin then
      NewScope := TElseScope.Create(stLocal, Scope)
    else
      NewScope := Scope;

    KW.ElseBody := TASTKWIF.TASTKWIfElseBlock.Create(KW);
    ElseSContext := SContext.MakeChild(Scope, KW.ElseBody);
    Result := ParseStatements(NewScope, ElseSContext, {IsBlock:} False);
  end;
end;

function TASTDelphiUnit.ParseImmVarStatement(Scope: TScope; const SContext: TSContext): TTokenID;
var
  DataType: TIDType;
  Variable: TIDVariable;
  LVarID: TIdentifier;
  EContext: TEContext;
  KW: TASTKWInlineVarDecl;
begin
  Result := Lexer_NextToken(Scope);
  KW := SContext.Add(TASTKWInlineVarDecl) as TASTKWInlineVarDecl;

  var LVarCount := 0;
  while True do begin
    Lexer_MatchIdentifier(Result);
    Lexer_ReadCurrIdentifier(LVarID);

    if LVarCount > 0 then
      ERRORS.CANNOT_INIT_MULTIPLE_VARS(LVarID);

    Result := Lexer_NextToken(Scope);
    if Result = token_Coma then begin
      Inc(LVarCount);
      Result := Lexer_NextToken(Scope);
      Continue;
    end;

    // parse a type if declared
    if Result = token_colon then
      Result := ParseTypeSpec(Scope, DataType)
    else
      DataType := nil;

    Variable := TIDVariable.Create(Scope, LVarID);
    Variable.DataType := DataType;
    Variable.Visibility := vLocal;
    Variable.DefaultValue := nil;
    Variable.Absolute := nil;
    Scope.AddVariable(Variable);
    KW.AddDecl(Variable);

    // parse a default value if declared
    if Result = token_assign then
    begin
      InitEContext(EContext, SContext, ExprRValue);
      var ADefaultValueExpr := TIDExpression.Create(Variable, Variable.TextPosition);
      EContext.RPNPushExpression(ADefaultValueExpr);

      Lexer_NextToken(Scope);
      var ASTExpr: TASTExpression := nil;
      Result := ParseExpression(Scope, SContext, EContext, {out} ASTExpr);
      KW.Expression := ASTExpr;

      if not Assigned(DataType) then
      begin
        DataType := EContext.Result.DataType;
        if DataType = Sys._Untyped then
          ERRORS.COULD_NOT_INFER_VAR_TYPE_FROM_UNTYPED(EContext.Result);

        Variable.DataType := DataType;
      end;

      EContext.RPNPushOperator(opAssignment);
      EContext.RPNFinish;
    end;

    Break;
  end;
end;

function TASTDelphiUnit.ParseImportStatement(Scope: TScope; out ImportLib, ImportName: TIDDeclaration): TTokenID;
var
  LLibNameExpr, LProcNameExpr: TIDExpression;
begin
  // читаем имя библиотеки
  Lexer_NextToken(Scope);
  Result := ParseConstExpression(Scope, {out} LLibNameExpr, ExprRValue);
  if Assigned(LLibNameExpr) then
  begin
    CheckStringExpression(LLibNameExpr);

    ImportLib := LLibNameExpr.Declaration;

    if Lexer_AmbiguousId = tokenD_name then
    begin
      // читаем имя декларации
      Lexer_NextToken(Scope);
      Result := ParseConstExpression(Scope, LProcNameExpr, ExprRValue);

      CheckEmptyExpression(LProcNameExpr);
      CheckStringExpression(LProcNameExpr);

      ImportName := LProcNameExpr.Declaration;
    end else
      ImportName := nil;
  end; // else todo:

  // delayed
  if Lexer_AmbiguousId = tokenD_delayed then
    Result := Lexer_NextToken(Scope);

  Lexer_MatchSemicolon(Result);
end;

function TASTDelphiUnit.ParseLabelSection(Scope: TScope): TTokenID;
var
  ID: TIdentifier;
  Decl: TASTDelphiLabel;
begin
  while True do
  begin
    Lexer_ReadNextIdentifier(Scope, ID);
    Decl := TASTDelphiLabel.Create(Scope, ID);
    InsertToScope(Scope, Decl);
    Result := Lexer_NextToken(Scope);
    if Result = token_coma then
      continue;

    break;
  end;
  Lexer_MatchSemicolon(Result);
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ParseParameters(EntryScope: TScope; ParamsScope: TParamsScope): TTokenID;
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
  CurItem: PIDItem;
  NextItem: PIDItem;
begin
  CurItem := Addr(IDItem);
  CurItem.Param := nil;
  CurItem.NextItem := nil;
  // process param's specifiers (var, out, const) first
  while True do
  begin
    Result := Lexer_NextToken(EntryScope);
    if (Result = token_closeround) or
       (Result = token_closeblock) then
     Exit;

    VarFlags := [VarParameter];

    // todo: process attributes like [constref]
    if Result = token_openblock then
      Result := ParseAttribute(EntryScope);

    case Lexer_TreatTokenAsToken(Result) of
      token_const: begin
        Include(VarFlags, VarConst);
        Result := Lexer_NextToken(EntryScope);
      end;
      token_var: begin
        Include(VarFlags, VarInOut);
        Result := Lexer_NextToken(EntryScope);
      end;
      tokenD_out: begin
        Include(VarFlags, VarOut);
        Result := Lexer_NextToken(EntryScope);
      end;
    end;

    if Result = token_openblock then
      Result := ParseAttribute(EntryScope);

    Lexer_MatchIdentifier(Result);
    while True do begin
      // read param name
      Lexer_ReadCurrIdentifier(CurItem.ID);

      Result := Lexer_NextToken(EntryScope);
      if Result = token_coma then begin
        Result := Lexer_NextToken(EntryScope);
        Lexer_MatchParamNameIdentifier(Result);
        // one more param with the same datatype
        New(NextItem);
        CurItem.NextItem := NextItem;
        CurItem := NextItem;
        CurItem.Param := nil;
        CurItem.NextItem := nil;

        Continue;
      end;

      DefaultExpr := nil;
      if Result = token_colon then
      begin
        // parse param type
        Result := ParseTypeSpec(EntryScope, DataType);
        // open array case
        if DataType.IsAnonymous and (DataType.DataTypeID = dtDynArray) then
          DataType.DataTypeID := dtOpenArray;
        // parse default value
        if Result = token_equal then
        begin
          Lexer_NextToken(EntryScope);
          Result := ParseConstExpression(EntryScope, {out} DefaultExpr, ExprNested)
        end;
      end else begin

        // if type is not specified then it is untyped reference
        if (VarConst in VarFlags) or
           (VarInOut in VarFlags) or
           (VarOut in VarFlags) or
           (VarConstRef in VarFlags) then
        begin
          DataType := Sys._UntypedReference;
        end else
          ERRORS.PARAM_TYPE_REQUIRED;
      end;

      NextItem := addr(IDItem);
      while Assigned(NextItem) do begin
        if not Assigned(NextItem.Param) then
        begin
          Param := TIDParameter.Create(EntryScope, NextItem.ID, DataType, VarFlags);
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
      CurItem.NextItem := NextItem;
      CurItem := NextItem;
      CurItem.Param := nil;
      CurItem.NextItem := nil;
      continue;
    end;

    // insert params to proc scope
    NextItem := addr(IDItem);
    while Assigned(NextItem) do
    begin
      Param := NextItem.Param;
      ParamsScope.AddExplicitParam(Param);
      NextItem := NextItem.NextItem;
    end;
    // disposing items memory
    CurItem := Addr(IDItem);
    CurItem := CurItem.NextItem;
    while Assigned(CurItem) do
    begin
      NextItem := CurItem;
      CurItem := CurItem.NextItem;
      Dispose(NextItem);
    end;

    Break;
  end;
end;

function TASTDelphiUnit.ParseParametersAndResult(EntryScope: TScope;
                                                 ParamsScope: TParamsScope;
                                                 out AResultType: TIDType): TTokenID;
begin
  AResultType := nil;
  Result := ParseParameters(EntryScope, ParamsScope);
  if Result = token_closeround then
  begin
    Result := Lexer_NextToken(EntryScope);
    if Result = token_colon then
    begin
      Result := ParseTypeSpec(EntryScope, {out} AResultType);
      AddResultParameter(ParamsScope, AResultType);
    end;
  end;
end;

function TASTDelphiUnit.CheckAndParsePlatform(Scope: TScope): TTokenID;
begin
  //TODO: parse deprecated text
  if Lexer_IsCurrentToken(tokenD_platform) then
    Result := Lexer_NextToken(Scope)
  else
    Result := Lexer_CurTokenID;
end;

function TASTDelphiUnit.GetPtrReferenceType(Decl: TIDRefType): TIDType;
begin
  Result := Decl.ReferenceType;
  // if the type has been declared as forward, find the reference type
  // for searching use all parent scopes
  if not Assigned(Result) and Decl.NeedForward then
  begin
    var TypeDecl := FindID(Decl.Scope, Decl.ForwardID);
    if TypeDecl.ItemType <> itType then
      AbortWork(sTypeIdExpectedButFoundFmt, [Decl.ForwardID.Name], Decl.ForwardID.TextPosition);

    Decl.ReferenceType := TIDType(TypeDecl);
    Result := TIDType(TypeDecl);
  end;
end;

function TASTDelphiUnit.ParsePointerType(Scope: TScope; const ID: TIdentifier; out Decl: TIDPointer): TTokenID;
var
  TmpID: TIdentifier;
  DataType: TIDType;
begin
  Lexer_ReadNextIdentifier(Scope, TmpID);
  DataType := TIDType(FindIDNoAbort(Scope, TmpID));
  // use the target type if it has been declared in the same unit
  if Assigned(DataType) and (DataType.Scope.DeclUnit = Self) then
  begin
    if DataType.ItemType <> itType then
      AbortWork(sTypeIdExpectedButFoundFmt, [TmpID.Name], Lexer.Position);
    if ID.Name = '' then begin
      Decl := DataType.GetDefaultReference(Scope) as TIDPointer;
      {признак, что этот анонимный тип является ссылкой на структуру которая еще не закончена}
      if (Decl.ReferenceType is TIDStructure) and (Scope.ScopeType = stStruct) and
         (Decl.ReferenceType = TStructScope(Scope).Struct) then
        Decl.NeedForward := True;
    end else begin
      Decl := TIDPointer.Create(Scope, ID);
      Decl.ReferenceType := DataType;
      Scope.AddType(Decl);
    end;
  end else begin
    // if not, postpone it to first using
    Decl := TIDPointer.Create(Scope, ID);
    Decl.ForwardID := TmpID;
    Decl.NeedForward := True;
    fForwardTypes.Add(Decl);
    if ID.Name <> '' then
      Scope.AddType(Decl);
  end;
  Decl.PointerMath := Options.POINTERMATH;
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ParseProcBody(Proc: TASTDelphiProc): TTokenID;
var
  Scope: TScope;
  SContext: TSContext;
begin
  Scope := Proc.EntryScope;
  Result := Lexer_CurTokenID;
  while true do begin
    case Result of
      token_eof: Exit;
      token_var: begin
        Lexer_NextToken(Scope);
        Result := ParseVarSection(Scope, vLocal, False);
      end;
      token_label: Result := ParseLabelSection(Scope);
      token_const: begin
        Lexer_NextToken(Scope);
        Result := ParseConstSection(Scope);
      end;
      token_type: Result := ParseTypeSection(Scope);
      token_procedure: Result := ParseProcedure(Scope, ptProc);
      token_function: Result := ParseProcedure(Scope, ptFunc);
      token_identifier: ERRORS.KEYWORD_EXPECTED;
      token_asm: begin
        // skip the asm...end block
        Lexer_SkipBlock(token_end);
        Result := Lexer_NextToken(Scope);
        Exit;
      end;
      token_begin: begin
        Proc.FirstBodyLine := Lexer_Line;
        Proc.Body := TASTBlock.Create(Proc);
        SContext := TSContext.Create(Self, Scope, Proc, Proc.Body);
        //CheckInitVariables(@SContext, nil, @Proc.VarSpace);
        Lexer_NextToken(Scope);
        Result := ParseStatements(Scope, SContext, {IsBlock:} True);
        Lexer_MatchToken(Result, token_end);
        Result := Lexer_NextToken(Scope);
        Proc.LastBodyLine := Lexer_Line;
        // геренация кода процедуры завершено
        Proc.Flags := Proc.Flags + [pfCompleted];
        //BENodesPool.Clear;
        Exit;
      end;
    else
      ERRORS.BEGIN_KEYWORD_EXPECTED;
    end;
  end;
end;

procedure CopyExplicitParams(SrcScope, DstScope: TProcScope);
begin
  var Node := SrcScope.First;
  while Assigned(Node) do begin
    if TIDVariable(Node.Data).IsExplicit then
      DstScope.InsertNode(Node.Key, Node.Data);
    Node := SrcScope.Next(Node);
  end;
end;

function SearchInstanceMethodDecl(Struct: TIDStructure;
                                  const ID: TIdentifier;
                                  out ForwardDeclNode: TIDList.PAVLNode): TASTDelphiProc;
begin
  Result := nil;
  ForwardDeclNode := Struct.Members.Find(ID.Name);
  if not Assigned(ForwardDeclNode) then
  begin
    // second, search the decl including ancestors
    var LDecl := Struct.Members.FindMembers(ID.Name);
    if LDecl is TASTDelphiProc then
      Result := TASTDelphiProc(LDecl)
    else
    // third, if this is a helper, search the decl in the helper's target
    if (Struct is TDlphHelper) and
       (TDlphHelper(Struct).Target is TIDStructure) then
      ForwardDeclNode := TIDStructure(TDlphHelper(Struct).Target).Members.Find(ID.Name);
  end;
end;

function SearchClassMethodDecl(Struct: TIDStructure;
                               const ID: TIdentifier;
                               out ForwardDeclNode: TIDList.PAVLNode): TASTDelphiProc;
begin
  Result := nil;
  ForwardDeclNode := Struct.Members.Find(ID.Name);
  if not Assigned(ForwardDeclNode) then
  begin
    // second, search the decl including ancestors
    Result := Struct.Members.FindMembers(ID.Name) as TASTDelphiProc;
    // third, if this is a helper, search the decl in the helper's target
    if not Assigned(Result) and
       (Struct is TDlphHelper) and
       (TDlphHelper(Struct).Target is TIDStructure) then
    ForwardDeclNode := TIDStructure(TDlphHelper(Struct).Target).Members.Find(ID.Name);
  end;
end;

function TASTDelphiUnit.ParseIntfMethodDelegation(Scope: TScope; AClass: TIDClass; const ID: TIdentifier): TTokenID;
var
  LProcID: TIdentifier;
begin
  Lexer_ReadNextIdentifier(Scope, {var} LProcID);

  var LMethod := AClass.FindMethod(LProcID.Name);

  // todo: add intf method delegation entry, supporting "late binding"
  //if not Assigned(LMethod) then
  //  ERRORS.UNDECLARED_ID(LProcID);


  Lexer_ReadSemicolon(Scope);
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ParseProcedure(Scope: TScope; ProcType: TProcType; Struct: TIDStructure): TTokenID;
type
  TFwdDeclState = (dsNew, dsDifferent, dsSame);
var
  ID: TIdentifier;
  ProcScope: TProcScope;
  ResultType: TIDType;
  GenericsParams: TIDTypeArray;
  Proc, ForwardDecl: TASTDelphiProc;
  ForwardDeclNode: TIDList.PAVLNode;
  FwdDeclState: TFwdDeclState;
  CallConv: TCallConvention;
  ProcFlags: TProcFlags;
  ImportLib, ImportName: TIDDeclaration;
begin
  Result := ParseProcName(Scope, ProcType, {out} ID, {var} Struct,
                          {out} ProcScope, {out} GenericsParams);

  if (Result = token_equal) and Assigned(Struct) then
  begin
    Result := ParseIntfMethodDelegation(Scope, Struct as TIDClass, ID);
    Exit;
  end;

  if not Assigned(Struct) then
  begin
    if Scope.ScopeClass <> scProc then
      Result := ParseGlobalProc(Scope, ProcType, ID, ProcScope)
    else
      Result := ParseNestedProc(Scope, ProcType, ID, ProcScope);
    Exit;
  end;

  if ProcType in [ptFunc, ptProc, ptClassFunc, ptClassProc, ptConstructor, ptDestructor] then
    AddSelfParameter(ProcScope, Struct, (ProcType = ptClassProc) or (ProcType = ptClassFunc));

  // parse parameters
  if Result = token_openround then
  begin
    ParseParameters(ProcScope, ProcScope.ParamsScope);
    Result := Lexer_NextToken(Scope);
  end;

  // parse result type
  if ProcType <= ptStaticFunc then
  begin
    Lexer_MatchToken(Result, token_colon);
    Result := ParseTypeSpec(ProcScope, ResultType);
    AddResultParameter(ProcScope.ParamsScope, ResultType);
  end else
    ResultType := nil;

  if Result = token_semicolon then
    Result := Lexer_NextToken(Scope)
  {else
  if not (Result in [token_overload, token_stdcall, token_cdecl]) then
    ERRORS.SEMICOLON_EXPECTED};

  case ProcType of
    ptClassFunc,
    ptClassProc: ProcFlags := [pfClass];
    ptStaticFunc,
    ptStaticProc: ProcFlags := [pfStatic];
    ptConstructor: begin
      if (Struct.DataTypeID = dtRecord) and (ProcScope.ExplicitParamsCount = 0) then
        ERRORS.PARAMETERLESS_CTOR_NOT_ALLOWED_ON_RECORD(Lexer_PrevPosition);

      ProcFlags := [pfConstructor];
    end;
    ptDestructor: ProcFlags := [pfDestructor];
  else
    ProcFlags := [];
  end;

  CallConv := TCallConvention.ConvNative;

  // parse proc specifiers
  while True do begin
    case Lexer_TreatTokenAsToken(Result) of
      tokenD_forward: Result := ProcSpec_Forward(Scope, ProcFlags);
      tokenD_export: Result := ProcSpec_Export(Scope, ProcFlags);
      token_inline: Result := ProcSpec_Inline(Scope, ProcFlags);
      tokenD_external: Result := ProcSpec_External(Scope, ImportLib, ImportName, ProcFlags);
      tokenD_overload: Result := ProcSpec_Overload(Scope, ProcFlags);
      tokenD_virtual: Result := ProcSpec_Virtual(Scope, Struct, ProcFlags);
      tokenD_dynamic: Result := ProcSpec_Dynamic(Scope, Struct, ProcFlags);
      tokenD_abstract: Result := ProcSpec_Abstract(Scope, Struct, ProcFlags);
      tokenD_override: Result := ProcSpec_Override(Scope, Struct, ProcFlags);
      tokenD_final: Result := ProcSpec_Final(Scope, Struct, ProcFlags);
      tokenD_reintroduce: Result := ProcSpec_Reintroduce(Scope, ProcFlags);
      tokenD_static: Result := ProcSpec_Static(Scope, ProcFlags, ProcType);
      tokenD_stdcall: Result := ProcSpec_StdCall(Scope, CallConv);
      tokenD_safecall: Result := ProcSpec_SafeCall(Scope, CallConv);
      tokenD_fastcall: Result := ProcSpec_FastCall(Scope, CallConv);
      tokenD_cdecl: Result := ProcSpec_CDecl(Scope, CallConv);
      tokenD_dispid: Result := ProcSpec_DispId(Scope, Struct, ID);
      tokenD_message: Result := ProcSpec_Message(Scope, ProcFlags);
      tokenD_varargs: begin
        Lexer_ReadSemicolon(Scope);
        Result := Lexer_NextToken(Scope);
      end;
      token_deprecated: begin
        CheckAndParseDeprecated(Scope, token_deprecated);
        Result := Lexer_NextToken(Scope);
      end;
      tokenD_platform: begin
        CheckAndParsePlatform(Scope);
        Result := Lexer_NextToken(Scope);
      end;
    else
      break;
    end;
  end;

  ForwardDeclNode := nil;
  // search the procedure forward declaration
  // first, search the decl in the current members only
  if ProcType = ptClassConstructor then
    ForwardDecl := Struct.ClassConstructor as TASTDelphiProc
  else
  if ProcType = ptClassDestructor then
    ForwardDecl := Struct.ClassDestructor as TASTDelphiProc
  else
  if IsClassProc(ProcType) then
    ForwardDecl := SearchClassMethodDecl(Struct, ID, {out} ForwardDeclNode)
  else
    ForwardDecl := SearchInstanceMethodDecl(Struct, ID, {out} ForwardDeclNode);

  if not Assigned(ForwardDecl) and
     not Assigned(ForwardDeclNode) and
     (Scope.ScopeClass = scImplementation) then
   ERRORS.METHOD_NOT_DECLARED_IN_CLASS(ID, Struct);

  if Assigned(ForwardDeclNode) and not Assigned(ForwardDecl) then
    ForwardDecl := TASTDelphiProc(ForwardDeclNode.Data);

  Proc := nil;
  FwdDeclState := dsDifferent;

  {if found forward declaration, process overload}
  if Assigned(ForwardDecl) then
  begin
    // ошибка если перекрыли идентификатор другого типа:
    if ForwardDecl.ItemType <> itProcedure then
      ERRORS.ID_REDECLARATED(ID);

    // The case when proc impl doesn't have params at all insted of decl
    if (Scope.ScopeClass = scImplementation) and (ProcScope.ExplicitParamsCount = 0) and (ForwardDecl.PrevOverload = nil) then
    begin
      FwdDeclState := dsSame;
      Proc := ForwardDecl;
      ProcScope.CopyFrom(ForwardDecl.ParamsScope);
    end else
    begin
      // search overload
      var Decl := ForwardDecl;
      while True do begin
        if Decl.SameDeclaration(ProcScope.ExplicitParams, ResultType, {ACheckNames:} True) then
        begin
          if Decl.Scope = Scope then
            ERRORS.THE_SAME_METHOD_EXISTS(ID)
          else
          if (Decl.Scope.ScopeClass = scInterface) and
             (Scope.ScopeClass = scImplementation) then
          begin
            Proc := Decl;
            FwdDeclState := dsSame;
          end else
            FwdDeclState := dsNew;
          Break;
        end;
        if not Assigned(Decl.PrevOverload) then
          Break;
        Decl := TASTDelphiProc(Decl.PrevOverload);
      end;
    end;
  end else
    FwdDeclState := dsNew;

  {create a new declaration}
  if not Assigned(Proc) then
  begin
    Proc := TASTDelphiProc.Create(Scope, ID);
    // если это generic-процедура или это generic-метод
    if Assigned(GenericsParams) then
      Proc.CreateGenericDescriptor(GenericsParams)
    else
    if Assigned(Struct.GenericDescriptor) then
      Proc.CreateGenericDescriptor(Struct.GenericDescriptor.GenericParams);

    Proc.ParamsScope := ProcScope;
    Proc.EntryScope := ProcScope;
    Proc.ResultType := ResultType;
    ProcScope.Proc := Proc;

    if Scope.ScopeClass = scInterface then
      Proc.Flags := Proc.Flags + [pfForward];

    Proc.ExplicitParams := ProcScope.ExplicitParams;

    // добовляем новую декларацию в структуру или глобольный список или к списку перегруженных процедур
    if not Assigned(ForwardDecl) then
    begin
      Proc.Struct := Struct;
      case ProcType of
        ptConstructor: begin
          Scope.AddProcedure(Proc);
        end;
        ptClassConstructor: begin
          // class constructor doesn't need to be added to the scope
          if Assigned(Struct.ClassConstructor) then
            ERRORS.CLASS_CONSTRUCTOR_ALREADY_EXIST(Proc);
          Struct.ClassConstructor := Proc;
        end;
        ptClassDestructor: begin
          // class destructor doesn't need to be added to the scope
          if Assigned(Struct.ClassDestructor) then
            ERRORS.CLASS_DESTRUCTOR_ALREADY_EXIST(Proc);
          Struct.ClassDestructor := Proc;
        end;
      else
        Scope.AddProcedure(Proc);
      end;
    end else begin
      // доавляем в список следующую перегруженную процедуру
      Proc.PrevOverload := ForwardDecl;

      // override the declaration if the scope the same
      if (ForwardDecl.Scope = Scope) then
        ForwardDeclNode.Data := Proc
      else
        Scope.AddProcedure(Proc);

      //special case for constructors, we need to place overloaded contructor in the scope as well
      if ProcType = ptConstructor then
      begin
        SearchClassMethodDecl(Struct, ID, {out} ForwardDeclNode);
        if Assigned(ForwardDeclNode) then
          ForwardDeclNode.Data := Proc
        else
          Struct.Members.InsertID(Proc);
      end;
      Proc.Struct := Struct;
    end;
  end;

  CallConv := ConvNative;
  Proc.Flags := Proc.Flags + ProcFlags;
  Proc.CallConvention := CallConv;

  // check overload/override
  if Scope.ScopeClass = scInterface then
  begin
    if pfOverride in ProcFlags then
    begin
      Proc.InheritedProc := Struct.FindVirtualProcInAncestor(Proc);
      if not Assigned(Proc.InheritedProc) then
      begin
        // todo: for debug
        Proc.InheritedProc := Struct.FindVirtualProcInAncestor(Proc);
        ERRORS.NO_METHOD_IN_BASE_CLASS(Proc);
      end;
    end;

    if Assigned(ForwardDecl) and (Struct = ForwardDecl.Struct) and
       not ((pfOveload in ForwardDecl.Flags) or (pfOveload in ProcFlags)) and
       not Assigned(Proc.InheritedProc) then
      ERRORS.OVERLOADED_MUST_BE_MARKED(ID);
  end;

  if (Scope.ScopeClass <> scInterface) and not (pfImport in ProcFlags)
                                       and not (pfForward in ProcFlags) then
  begin
    // use initial EntryScope (declared parameters)
    // just set Implementation scope as outer scope for the Entry
    Proc.EntryScope.OuterScope := ProcScope.OuterScope;

    if (FwdDeclState = dsDifferent) and not (pfOveload in ProcFlags) then
    begin
      if Proc.IsCompleted then
        ERRORS.OVERLOADED_MUST_BE_MARKED(ID)
      else
        ERRORS.DECL_DIFF_WITH_PREV_DECL(ID, ForwardDecl.DisplayName, Proc.DisplayName);
    end;
    Result := ParseProcBody(Proc);
    if Result = token_eof then
      Exit;

    if Result <> token_semicolon then
      ERRORS.SEMICOLON_EXPECTED;

    Result := Lexer_NextToken(Scope);
  end;

  if (ProcType = ptDestructor) and (Struct.DataTypeID = dtClass) then
    CheckDestructorSignature(Proc);
end;

function TASTDelphiUnit.ParseGlobalProc(Scope: TScope; ProcType: TProcType;
                                        const ID: TIdentifier; ProcScope: TProcScope): TTokenID;
type
  TFwdDeclState = (dsNew, dsDifferent, dsSame);
var
  ResultType: TIDType;
  Proc, ForwardDecl: TASTDelphiProc;
  ForwardDeclNode: TIDList.PAVLNode;
  FwdDeclState: TFwdDeclState;
  CallConv: TCallConvention;
  ProcFlags: TProcFlags;
  ImportLib, ImportName: TIDDeclaration;
begin
  Result := Lexer_CurTokenID;

  // parse parameters
  if Result = token_openround then
  begin
    ParseParameters(ProcScope, ProcScope.ParamsScope);
    Result := Lexer_NextToken(Scope);
  end;

  // parse return type
  if ProcType = ptFunc then
  begin
    if Result <> token_semicolon then
    begin
      Lexer_MatchToken(Result, token_colon);
      Result := ParseTypeSpec(ProcScope, ResultType);
      AddResultParameter(ProcScope.ParamsScope, ResultType);
    end else
      ResultType := nil;
  end else
    ResultType := nil;

  if Result = token_semicolon then
    Result := Lexer_NextToken(Scope);

  ProcFlags := [];
  CallConv := ConvNative;

  // parse proc specifiers
  while True do begin
    case Lexer_TreatTokenAsToken(Result) of
      tokenD_forward: Result := ProcSpec_Forward(Scope, ProcFlags);
      tokenD_export: Result := ProcSpec_Export(Scope, ProcFlags);
      token_inline: Result := ProcSpec_Inline(Scope, ProcFlags);
      tokenD_external: Result := ProcSpec_External(Scope, ImportLib, ImportName, ProcFlags);
      tokenD_overload: Result := ProcSpec_Overload(Scope, ProcFlags);
      tokenD_stdcall: Result := ProcSpec_StdCall(Scope, CallConv);
      tokenD_safecall: Result := ProcSpec_SafeCall(Scope, CallConv);
      tokenD_fastcall: Result := ProcSpec_FastCall(Scope, CallConv);
      tokenD_cdecl: Result := ProcSpec_CDecl(Scope, CallConv);
      tokenD_varargs: begin
        Lexer_ReadSemicolon(Scope);
        Result := Lexer_NextToken(Scope);
      end;
      token_deprecated: begin
        CheckAndParseDeprecated(Scope, token_deprecated);
        Result := Lexer_NextToken(Scope);
      end;
      tokenD_platform:
      begin
        CheckAndParsePlatform(Scope);
        Result := Lexer_NextToken(Scope);
      end;
    else
      break;
    end;
  end;

  ForwardDecl := nil;

  {search the procedure forward declaration}

  // first, search the declaration in the current scope only
  ForwardDeclNode := Scope.Find(ID.Name);

  // second, search the declaration in the interface section scope
  if not Assigned(ForwardDeclNode) and (Scope = ImplScope) then
    ForwardDeclNode := IntfScope.Find(ID.Name);

  // third, search in the uses units
  if not Assigned(ForwardDeclNode) then
  begin
    if Scope = ImplScope then
    begin
      var LDecl := ImplScope.FindIDRecurcive(ID.Name);
      if LDecl is TASTDelphiProc then
        ForwardDecl := TASTDelphiProc(LDecl);
    end else
    begin
      var LDecl := IntfScope.FindIDRecurcive(ID.Name);
      if LDecl is TASTDelphiProc then
        ForwardDecl := TASTDelphiProc(LDecl);
    end;
  end;

  if Assigned(ForwardDeclNode) and not Assigned(ForwardDecl) then
    ForwardDecl := TASTDelphiProc(ForwardDeclNode.Data);

  Proc := nil;
  FwdDeclState := dsDifferent;

  {если найдена ранее обьявленная декларация, проверяем соответствие}
  if Assigned(ForwardDecl) then
  begin
    if (Scope.ScopeClass = scInterface) and (ForwardDecl.Scope.DeclUnit = Self) then
    begin
      if not (pfOveload in ForwardDecl.Flags) then
        ERRORS.OVERLOADED_MUST_BE_MARKED(ForwardDecl.ID)
      else
      if not (pfOveload in ProcFlags) then
        ERRORS.OVERLOADED_MUST_BE_MARKED(ID);
    end;

    // ошибка если перекрыли идентификатор другого типа:
    if ForwardDecl.ItemType <> itProcedure then
      ERRORS.ID_REDECLARATED(ID);

    // The case when proc impl doesn't have params at all insted of decl
    if (Scope.ScopeClass = scImplementation) and (ProcScope.ExplicitParamsCount = 0) and (ForwardDecl.PrevOverload = nil) then
    begin
      FwdDeclState := dsSame;
      Proc := ForwardDecl;
      ProcScope.CopyFrom(ForwardDecl.ParamsScope);
    end else
    begin
      // search overload
      var Decl := ForwardDecl;
      while True do begin
        if Decl.SameDeclaration(ProcScope.ExplicitParams, ResultType) then begin
          FwdDeclState := dsSame;

          if (Decl.Scope = Scope) and not (pfForward in Decl.Flags) then
            ERRORS.ID_REDECLARATED(ID);

          Proc := Decl;
          Break;
        end;
        if not Assigned(Decl.PrevOverload) then
          Break;
        Decl := TASTDelphiProc(Decl.PrevOverload);
      end;
    end;
  end else
    FwdDeclState := dsNew;

  {create a new declaration}
  if not Assigned(Proc) then
  begin
    Proc := TASTDelphiProc.Create(Scope, ID);
    Proc.ParamsScope := ProcScope;
    Proc.ResultType := ResultType;
    ProcScope.Proc := Proc;

    if Scope.ScopeClass = scInterface then
      Proc.Flags := Proc.Flags + [pfForward];

    Proc.ExplicitParams := ProcScope.ExplicitParams;

    // добовляем новую декларацию в структуру или глобольный список или к списку перегруженных процедур
    if not Assigned(ForwardDecl) then
    begin
      Scope.AddProcedure(Proc);
    end else begin
      // доавляем в список следующую перегруженную процедуру
      if pfOveload in ProcFlags then
        Proc.PrevOverload := ForwardDecl;
      // override the declaration if the scope the same
      if (ForwardDecl.Scope = Scope) then
        ForwardDeclNode.Data := Proc
      else
        Scope.InsertID(Proc);

      //Scope.ProcSpace.Add(Proc);
    end;
  end;

  Proc.Flags := Proc.Flags + ProcFlags;
  Proc.CallConvention := CallConv;

  if (Scope.ScopeClass <> scInterface) and not (pfImport in ProcFlags)
                                       and not (pfForward in ProcFlags) then
  begin
    Proc.EntryScope := ProcScope;

    if Assigned(ForwardDecl) and
       (ForwardDecl.Scope = Scope) and
       (FwdDeclState = dsDifferent) and
       not (pfOveload in ProcFlags) then
    begin
      if ForwardDecl.IsCompleted then
        ERRORS.OVERLOADED_MUST_BE_MARKED(ID)
      else
        ERRORS.DECL_DIFF_WITH_PREV_DECL(ID, ForwardDecl.DisplayName, Proc.DisplayName);
    end;
    // parse proc body
    Result := ParseProcBody(Proc);
    if Result = token_eof then
      Exit;

    if Result <> token_semicolon then
      ERRORS.SEMICOLON_EXPECTED;

    Result := Lexer_NextToken(Scope);
  end;
end;

function TASTDelphiUnit.ParseNestedProc(Scope: TScope; ProcType: TProcType; const ID: TIdentifier;
  ProcScope: TProcScope): TTokenID;
type
  TFwdDeclState = (dsNew, dsDifferent, dsSame);
var
  ResultType: TIDType;
  Proc, ForwardDecl: TASTDelphiProc;
  ForwardDeclNode: TIDList.PAVLNode;
  FwdDeclState: TFwdDeclState;
  CallConv: TCallConvention;
  ProcFlags: TProcFlags;
begin
  Result := Lexer_CurTokenID;

  // parse parameters
  if Result = token_openround then
  begin
    ParseParameters(ProcScope, ProcScope.ParamsScope);
    Result := Lexer_NextToken(Scope);
  end;

  // parse return type
  if ProcType = ptFunc then
  begin
    if Result <> token_semicolon then
    begin
      Lexer_MatchToken(Result, token_colon);
      Result := ParseTypeSpec(ProcScope, ResultType);
      AddResultParameter(ProcScope.ParamsScope, ResultType);
    end else
      ResultType := nil;
  end else
    ResultType := nil;

  if Result = token_semicolon then
    Result := Lexer_NextToken(Scope)
  {else
  if not (Result in [token_overload, token_stdcall, token_cdecl]) then
    ERRORS.SEMICOLON_EXPECTED};

  ProcFlags := [];

  // parse proc specifiers
  while True do begin
    case Lexer_TreatTokenAsToken(Result) of
      tokenD_forward: Result := ProcSpec_Forward(Scope, ProcFlags);
      token_inline: Result := ProcSpec_Inline(Scope, ProcFlags);
      tokenD_overload: Result := ProcSpec_Overload(Scope, ProcFlags);
      tokenD_stdcall: Result := ProcSpec_StdCall(Scope, CallConv);
      tokenD_safecall:  Result := ProcSpec_SafeCall(Scope, CallConv);
      tokenD_fastcall: Result := ProcSpec_FastCall(Scope, CallConv);
      tokenD_cdecl: Result := ProcSpec_CDecl(Scope, CallConv);
      tokenD_varargs: begin
        Lexer_ReadSemicolon(Scope);
        Result := Lexer_NextToken(Scope);
      end;
      token_deprecated: begin
        CheckAndParseDeprecated(Scope, token_deprecated);
        Result := Lexer_NextToken(Scope);
      end;
      tokenD_platform:
      begin
        CheckAndParsePlatform(Scope);
        Result := Lexer_NextToken(Scope);
      end;
    else
      break;
    end;
  end;

  ForwardDecl := nil;

  {search the procedure forward declaration}

  // first, search the declaration in the current scope only
  ForwardDeclNode := Scope.Find(ID.Name);

  // second, search the declaration in the interface section scope
  if not Assigned(ForwardDeclNode) and (Scope = ImplScope) then
    ForwardDeclNode := IntfScope.Find(ID.Name);

  // third, search in the uses units
  if not Assigned(ForwardDeclNode) then
  begin
    if Scope = ImplScope then
    begin
      var LDecl := ImplScope.FindIDRecurcive(ID.Name);
      if LDecl is TASTDelphiProc then
        ForwardDecl := TASTDelphiProc(LDecl);
    end else
    begin
      var LDecl := IntfScope.FindIDRecurcive(ID.Name);
      if LDecl is TASTDelphiProc then
        ForwardDecl := TASTDelphiProc(LDecl);
    end;
  end;

  if Assigned(ForwardDeclNode) and not Assigned(ForwardDecl) then
    ForwardDecl := TASTDelphiProc(ForwardDeclNode.Data);

  Proc := nil;
  FwdDeclState := dsDifferent;

  {если найдена ранее обьявленная декларация, проверяем соответствие}
  if Assigned(ForwardDecl) then
  begin
    if (Scope.ScopeClass = scInterface) and (ForwardDecl.Scope.DeclUnit = Self) then
    begin
      if not (pfOveload in ForwardDecl.Flags) then
        ERRORS.OVERLOADED_MUST_BE_MARKED(ForwardDecl.ID)
      else
      if not (pfOveload in ProcFlags) then
        ERRORS.OVERLOADED_MUST_BE_MARKED(ID);
    end;

    // ошибка если перекрыли идентификатор другого типа:
    if ForwardDecl.ItemType <> itProcedure then
      ERRORS.ID_REDECLARATED(ID);

    // The case when proc impl doesn't have params at all insted of decl
    if (ProcScope.ExplicitParamsCount = 0) and (ForwardDecl.PrevOverload = nil) and (ForwardDecl.Scope = Scope) then
    begin
      FwdDeclState := dsSame;
      Proc := ForwardDecl;
      ProcScope.CopyFrom(ForwardDecl.ParamsScope);
    end else
    if (pfOveload in ForwardDecl.Flags) then
    begin
      // search overload
      var Decl := ForwardDecl;
      while True do begin
        if Decl.SameDeclaration(ProcScope.ExplicitParams, ResultType) then begin
          FwdDeclState := dsSame;
          if ((Decl.Scope = Scope) and not (pfForward in Decl.Flags)) or
              ((pfCompleted in Decl.Flags) and (ForwardDecl.Scope.DeclUnit = Self)) then
            ERRORS.ID_REDECLARATED(ID);
          Proc := Decl;
          Break;
        end;
        if not Assigned(Decl.PrevOverload) then
          Break;
        Decl := TASTDelphiProc(Decl.PrevOverload);
      end;
    end else
      FwdDeclState := dsNew;
  end else
    FwdDeclState := dsNew;

  {create a new declaration}
  if not Assigned(Proc) then
  begin
    Proc := TASTDelphiProc.Create(Scope, ID);
    Proc.ParamsScope := ProcScope;
    Proc.ResultType := ResultType;
    ProcScope.Proc := Proc;

    if Scope.ScopeClass = scInterface then
      Proc.Flags := Proc.Flags + [pfForward];

    Proc.ExplicitParams := ProcScope.ExplicitParams;

    // добовляем новую декларацию в структуру или глобольный список или к списку перегруженных процедур
    if not Assigned(ForwardDecl) then
    begin
      Scope.AddProcedure(Proc);
    end else begin
      // доавляем в список следующую перегруженную процедуру
      if pfOveload in ProcFlags then
        Proc.PrevOverload := ForwardDecl;
      // override the declaration if the scope the same
      if (ForwardDecl.Scope = Scope) then
        ForwardDeclNode.Data := Proc
      else
        Scope.InsertID(Proc);

//      Scope.ProcSpace.Add(Proc);
    end;
  end;

  CallConv := ConvNative;
  Proc.Flags := Proc.Flags + ProcFlags;
  Proc.CallConvention := CallConv;

  if (Scope.ScopeClass <> scInterface) and not (pfImport in ProcFlags)
                                       and not (pfForward in ProcFlags) then
  begin
    Proc.EntryScope := ProcScope;

    if Assigned(ForwardDecl) and
       (ForwardDecl.Scope = Scope) and
       (FwdDeclState = dsDifferent) and
       not (pfOveload in ProcFlags) then
    begin
      if ForwardDecl.IsCompleted then
        ERRORS.OVERLOADED_MUST_BE_MARKED(ID)
      else
        ERRORS.DECL_DIFF_WITH_PREV_DECL(ID, ForwardDecl.DisplayName, Proc.DisplayName);
    end;
    // parse proc body
    Result := ParseProcBody(Proc);
    if Result = token_eof then
      Exit;

    if Result <> token_semicolon then
      ERRORS.SEMICOLON_EXPECTED;

    Result := Lexer_NextToken(Scope);
  end;
end;

function TASTDelphiUnit.ParseProcName(AScope: TScope;
                                      AProcType: TProcType;
                                      out AID: TIdentifier;
                                      var AStruct: TIDStructure;
                                      out AProcScope: TProcScope;
                                      out AGenericParams: TIDTypeArray): TTokenID;

  function GetStructScope(AStruct: TIDStructure; AProcType: TProcType): TScope;
  begin
    if AProcType = ptOperator then
      Result := AStruct.Operators
    else
      Result := AStruct.Members;
  end;

var
  Decl: TIDDeclaration;
  SearchScope: TScope;
begin
  AProcScope := nil;
  SearchScope := AScope;
  var LIntfMathedDelegation := False;
  while True do begin
    Lexer_ReadNextIdentifier(AScope, {out} AID);
    Result := Lexer_NextToken(AScope);
    if Result = token_less then
    begin
      if Assigned(AStruct) or AScope.InheritsFrom(TMethodScope) then
        AProcScope := TMethodScope.CreateInDecl(AScope, GetStructScope(AStruct, AProcType), nil)
      else
        AProcScope := TProcScope.CreateInDecl(AScope, {Proc:} nil);
      Result := ParseGenericsHeader(AProcScope, AGenericParams);
    end;

    if Result = token_dot then
    begin
      // there are two possible options here:
      // - this is a method implementation
      // - this is an interface method delegation
      Decl := nil;
      if AScope.ScopeClass <> scInterface then
      begin
        // TODO: support generic overloads here
        Decl := SearchScope.FindID(AID.Name);
        if not Assigned(Decl) then
        begin
          // for debug:
          Decl := SearchScope.FindID(AID.Name);
          ERRORS.UNDECLARED_ID(AID, AGenericParams);
        end;
      end else
      if Assigned(AStruct) and (AStruct.DataTypeID = dtClass) then
      begin
        var LIntf := TIDClass(AStruct).FindInterface(AID.Name, AGenericParams);
        if Assigned(LIntf) then
        begin
          LIntfMathedDelegation := True;
          SearchScope := LIntf.Members;
          Continue;
        end else
          ERRORS.UNDECLARED_ID(AID, AGenericParams);
      end else
        ERRORS.UNDECLARED_ID(AID, AGenericParams);

      if Decl is TIDStructure then
      begin
        AStruct := TIDStructure(Decl);
        SearchScope := AStruct.Members;
        {т.к это имплементация обобщенного метода, то очищаем дупликатные обобщенные параметры}
        if Assigned(AProcScope) then begin
          AProcScope.OuterScope := AScope;
          AProcScope.Parent := GetStructScope(AStruct, AProcType);
          AProcScope.Clear;
        end;
      end else
        ERRORS.STRUCT_TYPE_REQUIRED(AID.TextPosition);
      Continue;
    end;
    if not Assigned(AProcScope) and not LIntfMathedDelegation then
    begin
      if Assigned(AStruct) then
        AProcScope := TMethodScope.CreateInDecl(AScope, GetStructScope(AStruct, AProcType), nil)
      else
        AProcScope := TProcScope.CreateInDecl(AScope, {Proc:} nil);
    end;
    Exit;
  end;
end;

function TASTDelphiUnit.ParseProcType(Scope: TScope; const ID: TIdentifier;
                                      GDescriptor: PGenericDescriptor; out Decl: TIDProcType): TTokenID;
var
  ParentScope: TScope;
  ResultType: TIDType;
  ProcClass: TProcTypeClass;
  IsFunction: Boolean;
begin
  ProcClass := procStatic;
  Result := Lexer_AmbiguousId;
  if Result = tokenD_reference then
  begin
    ProcClass := procReference;
    Lexer_ReadToken(Scope, token_to);
    Result := Lexer_NextToken(Scope);
  end;
  case Result of
    token_procedure: IsFunction := False;
    token_function: IsFunction := True;
  else
    IsFunction := False;
    AbortWork('PROCEDURE or FUNCTION required', Lexer_Position);
  end;
  Result := Lexer_NextToken(Scope);

  Decl := TIDProcType(ParseGenericTypeDecl(Scope, GDescriptor, ID, TIDProcType));

  if Assigned(GDescriptor) then
    ParentScope := GDescriptor.Scope
  else
    ParentScope := Scope;

  // parsing params
  if Result = token_openround then
  begin
    var LParamsScope := TParamsScope.Create(stLocal, ParentScope);
    ParseParameters(ParentScope, LParamsScope);
    Result := Lexer_NextToken(Scope);
    Decl.Params := LParamsScope.ExplicitParams;
  end;
  // parsing result if this is function
  if IsFunction then
  begin
    Lexer_MatchToken(Result, token_colon);
    Result := ParseTypeSpec(ParentScope, ResultType);
    Decl.ResultType := ResultType;
  end;
  // parsing of object
  if Result = token_of then
  begin
    Lexer_ReadToken(Scope, token_object);
    Result := Lexer_NextToken(Scope);
    ProcClass := procMethod;
  end;

  Decl.ProcClass := ProcClass;

  // check and parse procedural type call convention
  Result := CheckAndParseProcTypeCallConv(Scope, Result, Decl);
end;

function TASTDelphiUnit.ParseOperator(Scope: TScope; Struct: TIDStructure): TTokenID;
type
  TFwdDeclState = (dsNew, dsDifferent, dsSame);
var
  ID: TIdentifier;
  ProcScope: TProcScope;
  ResultType: TIDType;
  GenericsParams: TIDTypeArray;
  Proc, ForwardDecl: TASTDelphiProc;
  FwdDeclState: TFwdDeclState;
  ProcFlags: TProcFlags;
  ImportLib, ImportName: TIDDeclaration;
  LOperatorDef: TOperatorClass;
begin
  Result := ParseProcName(Scope, ptOperator, {out} ID, {var} Struct,
                          {out} ProcScope, {out} GenericsParams);

  LOperatorDef := TOperatorSignatures.FindOperator(ID.Name);
  if not Assigned(LOperatorDef) then
    AbortWork(sUnknownOperatorFmt, [ID.Name], ID.TextPosition);

  if not Assigned(Struct) then
    ERRORS.OPERATOR_MUST_BE_DECLARED_IN_STRUCT(ID.TextPosition);

  // parse params and check signature
  Lexer_MatchToken(Result, token_openround);
  Result := ParseParametersAndResult(ProcScope, ProcScope.ParamsScope, {out} ResultType);
  Lexer_MatchToken(Result, token_semicolon);

  TOperatorSignatures.CheckSignature(Struct, ID, ProcScope.ParamsScope);

  // ищем ранее обьявленную декларацию с таким же именем
  ForwardDecl := TASTDelphiProc(Struct.Operators.FindID(ID.Name));

  Proc := nil;
  FwdDeclState := dsDifferent;

  {если найдена ранее обьявленная декларация, проверяем соответствие}
  if Assigned(ForwardDecl) then begin
    // ошибка если перекрыли идентификатор другого типа:
    if ForwardDecl.ItemType <> itProcedure then
      ERRORS.ID_REDECLARATED(ID);
    // ищем подходящую декларацию в списке перегруженных:
    while True do begin
      if ForwardDecl.SameDeclaration(ProcScope.ExplicitParams, ResultType) then begin
        // нашли подходящую декларацию
        FwdDeclState := dsSame;
        if ForwardDecl.IsCompleted then
        begin
          // for debug
          if ForwardDecl.SameDeclaration(ProcScope.ExplicitParams, ResultType) then
            ERRORS.ID_REDECLARATED(ID);
        end;
        Proc := ForwardDecl;
        Break;
      end;
      // не нашли подходящую декларацию, создаем новую
      // проверку дерективы overload оставим на потом
      if not Assigned(ForwardDecl.PrevOverload) then
      begin
        Break;
      end;
      ForwardDecl := ForwardDecl.PrevOverload as TASTDelphiProc;
    end;
  end else
    FwdDeclState := dsNew;

  ProcFlags := [pfOperator];
  {создаем новую декларацию}
  if not Assigned(Proc) then
  begin
    Proc := TIDOperator.Create(Struct.Operators, ID, LOperatorDef.OpID);
    if Assigned(GenericsParams) then
      Proc.CreateGenericDescriptor(GenericsParams);

    Proc.ParamsScope := ProcScope;
    Proc.ResultType := ResultType;
    Proc.ExplicitParams := ProcScope.ExplicitParams;
    Proc.Struct := Struct;

    // добовляем новую декларацию в структуру или глобольный список или к списку перегруженных процедур
    if not Assigned(ForwardDecl) then
    begin
      Struct.Operators.AddProcedure(Proc);
    end else begin
      ForwardDecl.PrevOverload := Proc;
      //Struct.Operators.ProcSpace.Add(Proc);
    end;

    case LOperatorDef.OpID of
      opImplicit: begin
      if ResultType = Struct then
        Struct.OverloadImplicitFrom(Proc.ExplicitParams[0].DataType, Proc)
      else
        Struct.OverloadImplicitTo(ResultType, Proc);
      end;
      opExplicit: begin
        if ResultType = Struct then
          Struct.OverloadExplicitFrom(Proc.ExplicitParams[0].DataType, Proc)
        else
          Struct.OverloadExplicitTo(ResultType, Proc);
      end;
    else
    if LOperatorDef.OpID < opIn then
      Struct.OverloadUnarOperator(LOperatorDef.OpID, Proc)
    else
      Struct.OverloadBinarOperator(LOperatorDef.OpID, TIDOperator(Proc));
    end;
  end;

  Result := Lexer_NextToken(Scope);
  while True do begin
    case Lexer_AmbiguousId of
      tokenD_forward: Result := ProcSpec_Forward(Scope, ProcFlags);
      tokenD_export: Result := ProcSpec_Export(Scope, ProcFlags);
      token_inline: Result := ProcSpec_Inline(Scope, ProcFlags);
      tokenD_external: Result := ProcSpec_External(Scope, ImportLib, ImportName, ProcFlags);
      tokenD_overload: Result := ProcSpec_Overload(Scope, ProcFlags);
    else
      if (Scope.ScopeClass = scInterface) or (pfImport in ProcFlags) then
      begin
        Proc.Flags := ProcFlags;
        Break;
      end;

      Proc.EntryScope := ProcScope;
      if (FwdDeclState = dsDifferent) then
      begin
        if not Proc.IsCompleted then
          ERRORS.DECL_DIFF_WITH_PREV_DECL(ID, ForwardDecl.DisplayName, Proc.DisplayName);
      end;
      Result := ParseProcBody(Proc);
      Lexer_MatchSemicolon(Result);
      Result := Lexer_NextToken(Scope);
      Break;
    end;
    //token_identifier: AbortWork(sKeywordExpected, Lexer_Position);
  {else
    if Scope.ScopeClass <> scInterface then
      ERRORS.PROC_NEED_BODY;
    Break;}
  end;
end;

function TASTDelphiUnit.ParseRaiseStatement(Scope: TScope; const SContext: TSContext): TTokenID;
var
  EExcept, EAtAddr: TIDExpression;
  EContext: TEContext;
  ASTExpr: TASTExpression;
  KW: TASTKWRaise;
begin
  Lexer_NextToken(Scope);
  InitEContext(EContext, SContext, ExprRValue);
  Result := ParseExpression(Scope, SContext, EContext, ASTExpr);
  EExcept := EContext.Result;
  if Assigned(EExcept) then
    CheckClassExpression(EExcept);

  if Lexer_AmbiguousId = token_at then
  begin
    Lexer_NextToken(Scope);
    InitEContext(EContext, SContext, ExprRValue);
    Result := ParseExpression(Scope, SContext, EContext, ASTExpr);
    EAtAddr := EContext.Result;
    if Assigned(EExcept) then
      CheckPointerType(EAtAddr);
  end;


  KW := SContext.Add(TASTKWRaise) as TASTKWRaise;
  KW.Expression := ASTExpr;
end;

procedure TASTDelphiUnit.ParseRangeType(Scope: TScope; Expr: TIDExpression; const ID: TIdentifier; out Decl: TIDRangeType);
var
  LB, HB: Int64;
  CRange: TIDRangeConstant;
  BoundExpr: TIDExpression;
  RDataTypeID: TDataTypeID;
  RDataType: TIDType;
begin
  CRange := TIDRangeConstant(Expr.Declaration);
  if (CRange.ItemType <> itConst) and (not (CRange is TIDRangeConstant)) then
    AbortWork(sConstRangeRequired, Lexer_Position);

  BoundExpr := CRange.Value.LBExpression;
  CheckConstExpression(BoundExpr);
  LB := TIDConstant(BoundExpr.Declaration).AsInt64;

  BoundExpr := CRange.Value.HBExpression;
  CheckConstExpression(BoundExpr);
  HB := TIDConstant(BoundExpr.Declaration).AsInt64;

  RDataTypeID := GetValueDataType(HB - LB);
  RDataType := Sys.DataTypes[RDataTypeID];

  Decl := TIDRangeType.Create(Scope, ID);
  Decl.LoDecl := CRange.Value.LBExpression.AsConst;
  Decl.HiDecl := CRange.Value.HBExpression.AsConst;
  Decl.LowBound := LB;
  Decl.HighBound := HB;
  Decl.OverloadImplicitFrom(RDataType);
end;

function TASTDelphiUnit.ParseRecordInitValue(Scope: TRecordInitScope; var FirstField: TIDExpression): TTokenID;
var
  FldValue: TIDExpression;
begin
  Lexer_NextToken(Scope);
  Result := ParseConstExpression(Scope, FldValue, ExprRValue);
  if Result = token_semicolon then
    Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ParseRecordType(Scope: TScope; Decl: TIDRecord): TTokenID;
var
  Visibility: TVisibility;
begin
  Visibility := vPublic;
  while True do
  begin
    Result := ParseVisibilityModifiers(Scope, {var} Visibility, {AIsClass:} False);
    case Result of
      token_openblock: Result := ParseAttribute(Scope);
      token_case: Result := ParseCaseRecord(Decl.Members, Decl);
      token_class: begin
        Result := Lexer_NextReseredToken(scope);
        case Result of
          token_procedure: Result := ParseProcedure(Decl.Members, ptClassProc, Decl);
          token_function: Result := ParseProcedure(Decl.Members, ptClassFunc, Decl);
          tokenD_operator: Result := ParseOperator(Decl.Operators, Decl);
          token_property: Result := ParseProperty(Decl.Members, Decl);
          token_constructor: Result := ParseProcedure(Decl.Members, ptClassConstructor, Decl);
          token_destructor: Result := ParseProcedure(Decl.Members, ptClassDestructor, Decl);
          token_var: begin
             Lexer_NextToken(Scope);
             Result := ParseFieldsSection(Decl.Members, Visibility, Decl, True);
          end;
        else
          AbortWork('Class members can be: PROCEDURE, FUNCTION, OPERATOR, CONSTRUCTOR, DESTRUCTOR, PROPERTY, VAR', Lexer_Position);
        end;
      end;
      token_procedure: Result := ParseProcedure(Decl.Members, ptProc, Decl);
      token_function: Result := ParseProcedure(Decl.Members, ptFunc, Decl);
      token_constructor: Result := ParseProcedure(Decl.Members, ptConstructor, Decl);
      token_destructor: Result := ParseProcedure(Decl.Members, ptDestructor, Decl);
      token_property: Result := ParseProperty(Decl.Members, Decl);
      token_var: begin
        Lexer_NextToken(Scope);
        Result := ParseFieldsSection(Decl.Members, Visibility, Decl, False);
      end;
      token_const: Result := ParseConstSectionInStruct(Decl.Members);
      token_type: Result := ParseTypeSectionInStruct(Decl.Members);
      token_identifier: Result := ParseFieldsSection(Decl.Members, Visibility, Decl, False);
    else
      break;
    end;
  end;
  CheckIncompleteType(Decl.Members);
  Decl.StructFlags := Decl.StructFlags + [StructCompleted];

  Lexer_MatchToken(Result, token_end);
  Result := Lexer_NextToken(Scope);

  // check for align keyword
  if Lexer_AmbiguousId = tokenD_aling then
  begin
    Lexer_NextToken(Scope);
    var AlignValueExpr: TIDExpression;
    Result := ParseConstExpression(Scope, {out} AlignValueExpr, ExprRValue);
  end;

  Result := CheckAndParsePlatform(Scope);
  Result := CheckAndParseDeprecated(Scope, Result);
end;

function TASTDelphiUnit.InstantiateGenericProc(AScope: TScope; AGenericProc: TIDProcedure;
                                               const AGenericArgs: TIDExpressions): TIDProcedure;
var
  GDescriptor: PGenericDescriptor;
  LContext: TGenericInstantiateContext;
  LGArgs: TIDTypeArray;
  LGArgsCount: Integer;
begin
  GDescriptor := AGenericProc.GenericDescriptor;

  if not Assigned(GDescriptor) then
    AbortWorkInternal('generic descriptor is not assigned', Lexer_Position);

  LGArgsCount := Length(AGenericArgs);
  SetLength(LGArgs, LGArgsCount);
  for var LIndex := 0 to LGArgsCount - 1 do
    LGArgs[LIndex] := AGenericArgs[LIndex].AsType;

  {find in the pool first}
  var LDecl: TIDDeclaration;
  if GDescriptor.TryGetInstance(LGArgs, {out} LDecl) then
  begin
    Result := LDecl as TIDProcedure;
    Exit;
  end;

  LContext.SrcDecl := AGenericProc;
  var AStrSufix := '';
  var AArgsCount := Length(AGenericArgs);
  LContext.Args := LGArgs;
  LContext.Params := GDescriptor.GenericParams;
  for var AIndex := 0 to AArgsCount - 1 do
  begin
    var AArgType := AGenericArgs[AIndex].AsType;
    AStrSufix := AddStringSegment(AStrSufix, AArgType.Name, ', ');
  end;
  LContext.DstID.Name := AGenericProc.Name + '<' + AStrSufix + '>';
  LContext.DstID.TextPosition := Lexer_Position;
  try
    WriteLog('# (%s: %d): %s', [Name, Lexer_Line, LContext.DstID.Name]);
    Result := AGenericProc.InstantiateGeneric(AScope, {ADstStruct:} nil, LContext) as TIDProcedure;
    {add new instance to the pool}
    GDescriptor.AddGenericInstance(Result, LGArgs);
  except
    Error('Generic Instantiation Error: %s', [LContext.DstID.Name], LContext.DstID.TextPosition);
    raise;
  end;
end;

function TASTDelphiUnit.InstantiateGenericType(AScope: TScope; AGenericType: TIDType;
                                               const AGenericArgs: TIDExpressions): TIDType;
var
  GDescriptor: PGenericDescriptor;
  LContext: TGenericInstantiateContext;
  LGArgs: TIDTypeArray;
  LGArgsCount: Integer;
begin
  GDescriptor := AGenericType.GenericDescriptor;

  LGArgsCount := Length(AGenericArgs);
  SetLength(LGArgs, LGArgsCount);
  for var LIndex := 0 to LGArgsCount - 1 do
    LGArgs[LIndex] := AGenericArgs[LIndex].AsType;

  {find in the pool first}
  var LDecl: TIDDeclaration;
  if GDescriptor.TryGetInstance(LGArgs, {out} LDecl) then
  begin
    Result := LDecl as TIDType;
    Exit;
  end;

  LContext.SrcDecl := AGenericType;
  var AStrSufix := '';
  var AArgsCount := Length(AGenericArgs);
  LContext.Args := LGArgs;
  LContext.Params := GDescriptor.GenericParams;
  for var AIndex := 0 to AArgsCount - 1 do
  begin
    var AArgType := AGenericArgs[AIndex].AsType;
    AStrSufix := AddStringSegment(AStrSufix, AArgType.Name, ', ');
  end;
  LContext.DstID.Name := AGenericType.Name + '<' + AStrSufix + '>';
  LContext.DstID.TextPosition := Lexer_Position;

  try
    WriteLog('# (%s: %d): %s', [Name, Lexer_Line, LContext.DstID.Name]);
    Result := AGenericType.InstantiateGeneric(AScope, {ADstStruct:} nil, LContext) as TIDType;
  except
    Error('Generic Instantiation Error: %s', [LContext.DstID.Name], LContext.DstID.TextPosition);
    raise;
  end;
end;

procedure TASTDelphiUnit.StaticCheckBounds(ABound: TIDOrdinal; AValue: TIDConstant);
begin
  if (AValue.AsInt64 < ABound.LowBound) or (AValue.AsUInt64 > ABound.HighBoundUInt64) then
   AbortWork(sConstExprOutOfRangeFmt, [AValue.AsInt64, ABound.LowBound, ABound.HighBoundUInt64], Lexer.PrevPosition);
end;

class function TASTDelphiUnit.StrictMatchProc(const Src, Dst: TIDProcedure): Boolean;
begin
  Result := StrictMatchProcSingnatures(Src.ExplicitParams, Dst.ExplicitParams, Src.ResultType, Dst.ResultType);
end;

class function TASTDelphiUnit.StrictMatchProcSingnatures(const SrcParams, DstParams: TIDParamArray;
                                                         const SrcResultType, DstResultType: TIDType): Boolean;
var
  SParamsCount,
  DParamsCount: Integer;
  SParamType, DParamType: TIDType;
begin
  // check matching for the result (if exists)
  if Assigned(SrcResultType) and Assigned(DstResultType) then
  begin
    if not SameTypes(SrcResultType.ActualDataType, DstResultType.ActualDataType) then
      Exit(False);
  end else
  if Assigned(SrcResultType) or Assigned(DstResultType) then
    Exit(False);

  SParamsCount := Length(SrcParams);
  DParamsCount := Length(DstParams);

  // check matching for each parameter
  if SParamsCount = DParamsCount then
  begin
    for var LIndex := 0 to SParamsCount - 1 do begin
      SParamType := SrcParams[LIndex].DataType.ActualDataType;
      DParamType := DstParams[LIndex].DataType.ActualDataType;
      if not SameTypes(SParamType, DParamType) then
        Exit(False);
    end;
    Result := True;
  end else
    Result := False;
end;

function TASTDelphiUnit.ParseRepeatStatement(Scope: TScope; const SContext: TSContext): TTokenID;
var
  Expression: TIDExpression;
  EContext: TEContext;
  BodySContext: TSContext;
  KW: TASTKWRepeat;
  ASTExpr: TASTExpression;
begin
  KW := SContext.Add(TASTKWRepeat) as TASTKWRepeat;

  BodySContext := SContext.MakeChild(Scope, KW.Body);

  Lexer_NextToken(Scope);
  // тело цикла
  Result := ParseStatements(Scope, BodySContext, {IsBlock:} True);
  Lexer_MatchToken(Result, token_until);

  // выражение цикла
  InitEContext(EContext, SContext, ExprRValue);
  Lexer_NextToken(Scope);
  Result := ParseExpression(Scope, SContext, EContext, ASTExpr);
  KW.Expression := ASTExpr;
  Expression := EContext.Result;
  CheckEmptyExpression(Expression);
  CheckBooleanExpression(Expression);
end;

function TASTDelphiUnit.Lexer_MatchSemicolonAndNext(Scope: TScope; ActualToken: TTokenID): TTokenID;
begin
  if ActualToken = token_semicolon then
    Result := Lexer_NextToken(Scope)
  else
  if ActualToken <> token_end then
  begin
    ERRORS.SEMICOLON_EXPECTED;
    Result := token_unknown;
  end else
    Result := ActualToken
end;

function TASTDelphiUnit.ProcSpec_Inline(Scope: TScope; var Flags: TProcFlags): TTokenID;
begin
  if pfImport in Flags then
    ERRORS.IMPORT_FUNCTION_CANNOT_BE_INLINE;
  if pfInline in Flags then
    ERRORS.DUPLICATE_SPECIFICATION(PS_INLINE);
  Include(Flags, pfInline);

  {semicolin can be oprional}
  Result := Lexer_NextToken(Scope);
  if Result = token_semicolon then
    Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ProcSpec_Message(Scope: TScope; var Flags: TProcFlags): TTokenID;
var
  LMessageExpr: TIDExpression;
begin
  Lexer_NextToken(Scope);
  ParseConstExpression(Scope, {out} LMessageExpr, ExprRValue);
  CheckEmptyExpression(LMessageExpr);
  CheckIntExpression(LMessageExpr);
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ProcSpec_Export(Scope: TScope; var Flags: TProcFlags): TTokenID;
var
  ExportID: TIdentifier;
begin
  if pfExport in Flags then
    ERRORS.DUPLICATE_SPECIFICATION(PS_EXPORT);
  Include(Flags, pfExport);
  {if Scope.ScopeClass <> scInterface then
    ERRORS.EXPORT_ALLOWS_ONLY_IN_INTF_SECTION;}
  Result := Lexer_NextToken(Scope);
  if Result = token_identifier then
  begin
    Lexer_ReadCurrIdentifier(ExportID);
    Result := Lexer_NextToken(Scope);
  end; //else
//    ExportID := Proc.ID;
//  Proc.Export := Package.GetStringConstant(ExportID.Name);
  Lexer_MatchToken(Result, token_semicolon);
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ProcSpec_Forward(Scope: TScope; var Flags: TProcFlags): TTokenID;
begin
  if (pfForward in Flags) or (pfImport in Flags) then
    ERRORS.DUPLICATE_SPECIFICATION(PS_FORWARD);
  Include(Flags, pfForward);
  Lexer_ReadSemicolon(Scope);
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ProcSpec_External(Scope: TScope; out ImportLib, ImportName: TIDDeclaration; var Flags: TProcFlags): TTokenID;
begin
  if pfImport in Flags then
    ERRORS.DUPLICATE_SPECIFICATION(PS_IMPORT);
  Include(Flags, pfImport);
  ParseImportStatement(Scope, ImportLib, ImportName);
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ProcSpec_Overload(Scope: TScope; var Flags: TProcFlags): TTokenID;
begin
  if pfOveload in Flags then
    ERRORS.DUPLICATE_SPECIFICATION(PS_OVELOAD);
  Include(Flags, pfOveload);
  Result := Lexer_ReadSemicolonAndToken(Scope);
end;

function TASTDelphiUnit.ProcSpec_Reintroduce(Scope: TScope; var Flags: TProcFlags): TTokenID;
begin
  if pfReintroduce in Flags then
    ERRORS.DUPLICATE_SPECIFICATION(PS_REINTRODUCE);
  Include(Flags, pfReintroduce);
  Lexer_ReadSemicolon(Scope);
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ProcSpec_Virtual(Scope: TScope; Struct: TIDStructure; var Flags: TProcFlags): TTokenID;
begin
  if pfVirtual in Flags then
    ERRORS.DUPLICATE_SPECIFICATION(PS_VIRTUAL);
  Include(Flags, pfVirtual);

  if not Assigned(Struct) then
    ERRORS.VIRTUAL_ALLOWED_ONLY_IN_CLASSES;

  //Proc.VirtualIndex := Proc.Struct.GetLastVirtualIndex + 1;

  Lexer_ReadSemicolon(Scope);
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ProcSpec_DispId(AScope: TScope; AStruct: TIDStructure; const AMethodID: TIdentifier): TTokenID;
var
  LDispIdExpr: TIDExpression;
begin
  Lexer_NextToken(AScope);

  if not Assigned(AStruct) or (AStruct.DataTypeID <> dtInterface) then
    ERRORS.E2185_CANNOT_SPECIFY_DISPID(AMethodID);

  Result := ParseConstExpression(AScope, {out} LDispIdExpr, ExprRValue);

  if Result = token_semicolon then
    Result := Lexer_NextToken(AScope);

  CheckIntExpression(LDispIdExpr);
end;

function TASTDelphiUnit.ProcSpec_Dynamic(Scope: TScope; Struct: TIDStructure; var Flags: TProcFlags): TTokenID;
begin
  if pfVirtual in Flags then
    ERRORS.DUPLICATE_SPECIFICATION(PS_VIRTUAL);
  Include(Flags, pfVirtual);

  if not Assigned(Struct) then
    ERRORS.VIRTUAL_ALLOWED_ONLY_IN_CLASSES;

  //Proc.VirtualIndex := Proc.Struct.GetLastVirtualIndex + 1;

  Lexer_ReadSemicolon(Scope);
  Result := Lexer_NextToken(Scope);
end;

procedure TASTDelphiUnit.Progress(StatusClass: TASTProcessStatusClass);
begin
  fTotalLinesParsed := Lexer_Line;
  inherited;
end;

function TASTDelphiUnit.ProcSpec_Abstract(Scope: TScope; Struct: TIDStructure; var Flags: TProcFlags): TTokenID;
begin
  if pfAbstract in Flags then
    ERRORS.DUPLICATE_SPECIFICATION(PS_ABSTRACT);
  Include(Flags, pfAbstract);

  if not Assigned(Struct) then
    ERRORS.VIRTUAL_ALLOWED_ONLY_IN_CLASSES;

  Lexer_ReadSemicolon(Scope);
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ProcSpec_Override(Scope: TScope; Struct: TIDStructure; var Flags: TProcFlags): TTokenID;
begin
  if pfOverride in Flags then
    ERRORS.DUPLICATE_SPECIFICATION(PS_OVERRIDE);

  if not Assigned(Struct) then
    ERRORS.STRUCT_TYPE_REQUIRED(Lexer_Position);

  Include(Flags, pfOverride);
  Include(Flags, pfVirtual);
  Lexer_ReadSemicolon(Scope);
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ProcSpec_Final(Scope: TScope; Struct: TIDStructure; var Flags: TProcFlags): TTokenID;
begin
  if pfFinal in Flags then
    ERRORS.DUPLICATE_SPECIFICATION(PS_FINAL);

  if not Assigned(Struct) then
    ERRORS.STRUCT_TYPE_REQUIRED(Lexer_Position);

  Include(Flags, pfFinal);
  Lexer_ReadSemicolon(Scope);
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ProcSpec_SafeCall(Scope: TScope; var CallConvention: TCallConvention): TTokenID;
begin
  if CallConvention = ConvSafeCall then
    ERRORS.DUPLICATE_SPECIFICATION(PS_SAFECALL);
  CallConvention := ConvSafeCall;

  Result := Lexer_NextToken(Scope);
  if Result = token_semicolon then
    Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ProcSpec_Static(Scope: TScope; var Flags: TProcFlags; var ProcType: TProcType): TTokenID;
begin
  if pfStatic in Flags then
    ERRORS.DUPLICATE_SPECIFICATION(PS_STATIC);
  Include(Flags, pfStatic);
  Lexer_ReadSemicolon(Scope);
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ProcSpec_StdCall(Scope: TScope; var CallConvention: TCallConvention): TTokenID;
begin
  if CallConvention = ConvStdCall then
    ERRORS.DUPLICATE_SPECIFICATION(PS_STDCALL);
  CallConvention := ConvStdCall;

  Result := Lexer_NextToken(Scope);
  if Result = token_semicolon then
    Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ProcSpec_FastCall(Scope: TScope; var CallConvention: TCallConvention): TTokenID;
begin
  if CallConvention = ConvFastCall then
    ERRORS.DUPLICATE_SPECIFICATION(PS_FASTCALL);
  CallConvention := ConvFastCall;
  Lexer_ReadSemicolon(Scope);
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ProcSpec_CDecl(Scope: TScope; var CallConvention: TCallConvention): TTokenID;
begin
  if CallConvention = ConvCDecl then
    ERRORS.DUPLICATE_SPECIFICATION(PS_CDECL);
  CallConvention := ConvCDecl;
  Lexer_ReadSemicolon(Scope);
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ParseGenericMember(const PMContext: TPMContext;
                                           const SContext: TSContext;
                                           StrictSearch: Boolean;
                                           out Decl: TIDDeclaration;
                                           out WithExpression: TIDExpression): TTokenID;
var
  Scope: TScope;
  LGenericArgs: TIDExpressions;
  ExprName: string;
  LCanInstantiate: Boolean;
  LGenericType: TIDDeclaration;
begin
  Scope := PMContext.ItemScope;

  Result := ParseGenericsArgs(Scope, SContext, {out} LGenericArgs, {out} LCanInstantiate);
  ExprName := format('%s<%d>', [PMContext.ID.Name, Length(LGenericArgs)]);

  if not StrictSearch then
    LGenericType := FindIDNoAbort(Scope, ExprName)
  else
    LGenericType := Scope.FindMembers(ExprName);

  if not Assigned(LGenericType) then
  begin
    // for debug
     if not StrictSearch then
      LGenericType := FindIDNoAbort(Scope, ExprName)
    else
      LGenericType := Scope.FindMembers(ExprName);
    Exit;
    ERRORS.UNDECLARED_ID(ExprName, Lexer_PrevPosition);
  end;

  if LCanInstantiate then
    Decl := InstantiateGenericType(Scope, LGenericType as TIDType, LGenericArgs)
  else begin
    // check if this type is not the current parsed type
    if not IsTheStructIsOwner(Scope, (LGenericType as TIDType)) then
      Decl := TIDGenericInstantiation.CreateInstantiation(Scope, LGenericType as TIDType, LGenericArgs)
    else
      Decl := LGenericType;
  end;
end;

function TASTDelphiUnit.ParseUnknownID(Scope: TScope; const PrevExpr: TIDExpression; ID: TIdentifier; out Decl: TIDDeclaration): TTokenID;
var
  FullID: TIdentifier;
begin
  if Assigned(PrevExpr) and (PrevExpr.ItemType = itUnit) then
    FullID := PrevExpr.Declaration.ID;

  Result := Lexer_CurTokenID;
  while True do begin
    FullID := TIdentifier.Combine(FullID, ID);
    Decl := FindIDNoAbort(Scope, FullID);
    if Assigned(Decl) then
      Exit;

    if Result = token_dot then
    begin
      Lexer_ReadNextIdentifier(Scope, ID);
      Result := Lexer_NextToken(Scope);
      continue;
    end;
    Break;
  end;

  ERRORS.UNDECLARED_ID(ID);
end;

function ProcCanBeGeneric(AProc: TIDProcedure): Boolean;
begin
  while Assigned(AProc) do
  begin
    if AProc.IsGeneric then
      Exit(True);

    AProc := AProc.PrevOverload;
  end;
  Result := False;
end;

function TASTDelphiUnit.ParseIdentifier(Scope, SearchScope: TScope;
                                        out Expression: TIDExpression;
                                        var EContext: TEContext;
                                        const PrevExpr: TIDExpression;
                                        const ASTE: TASTExpression): TTokenID;
var
  Decl: TIDDeclaration;
  Indexes: TIDExpressions;
  i: Integer;
  Expr, NExpr: TIDExpression;
  PMContext: TPMContext;
  StrictSearch: Boolean;
begin
  PMContext.Init;
  PMContext.ItemScope := Scope;

  Lexer_ReadCurrIdentifier(PMContext.ID);

  Expr := nil; // todo: with
  if not Assigned(SearchScope) then
    Decl := FindIDNoAbort(Scope, PMContext.ID)

  else begin
    Decl := SearchScope.FindMembers(PMContext.ID.Name);
  end;

  StrictSearch := Assigned(SearchScope);

  Result := Lexer_NextToken(Scope);
  if not Assigned(Decl) then begin
    if Result = token_less then
      Result := ParseGenericMember(PMContext, EContext.SContext, StrictSearch, Decl, Expr);

    if PMContext.ItemScope is TConditionalScope then
    begin
      Decl := TIDStringConstant.CreateAsAnonymous(PMContext.ItemScope, Sys._UnicodeString, PMContext.ID.Name);
    end;

    if not Assigned(Decl) then
      Result := ParseUnknownID(Scope, PrevExpr, PMContext.ID, Decl);
  end;

  // если Scope порожден конструкцией WITH
  // то добавляем в выражение первым элементом
  if Assigned(Expr) then begin
    if Expr.ExpressionType = etDeclaration then begin
      PMContext.Add(Expr);
    end else begin
      Indexes := TIDMultiExpression(Expr).Items;
      for i := 0 to Length(Indexes) - 1 do
        PMContext.Add(Indexes[i]);
    end;
  end;

  {проверяем на псевдоним}
  if Decl.ItemType = itAlias then
    Decl := TIDAlias(Decl).Original;

  {проверяем на доступ к члену типа}
  //if Assigned(EContext.SContext) then
  //  CheckAccessMember(SContext, Decl, PMContext.ID);

  case Decl.ItemType of
    {процедура/функция}
    itProcedure: begin
      var LGenericArgs: TIDExpressions;
      var LCallExpr := TIDCallExpression.Create(Decl, PMContext.ID.TextPosition);
      LCallExpr.Instance := PrevExpr;
      Expression := LCallExpr;
      // parse explicit generic arguments
      if (Result = token_less) and ProcCanBeGeneric(TIDProcedure(Decl)) then
      begin
        var LCanInstantiate: Boolean;
        Result := ParseGenericsArgs(Scope, EContext.SContext, {out} LGenericArgs, {out} LCanInstantiate);
        LCallExpr.GenericArgs := LGenericArgs;
        LCallExpr.CanInstantiate := LCanInstantiate;
      end;
      // если есть открытая скобка, - значит вызов
      if Result = token_openround then
      begin
        Result := ParseEntryCall(Scope, LCallExpr, EContext, ASTE);
        // если это метод, подставляем self из пула
        if PMContext.Count > 0 then
        begin
          if PMContext.Count > 1 then
          begin
            Expr := nil;
            Assert(false);
          end else
            Expr := PMContext.Last;

          if (Expr.Declaration is TIDField) and (PMContext.Count = 1) then
          begin
            NExpr := GetTMPRefExpr(EContext.SContext, Expr.DataType);
            NExpr.TextPosition := Expr.TextPosition;
            Expr := NExpr;
          end;
          LCallExpr.Instance := Expr;
        end else
        if (Decl.ItemType = itProcedure) and
            Assigned(TIDProcedure(Decl).Struct) and
            not Assigned(LCallExpr.Instance) and
           (TIDProcedure(Decl).Struct = EContext.SContext.Proc.Struct) then
        begin
          // если это собственный метод, добавляем self из списка параметров
          // todo: SelfParam has no declaration asssigned
          LCallExpr.Instance := TIDExpression.Create(EContext.SContext.Proc.SelfParam);
        end;

        // process call operator
        Expression := EContext.RPNPopOperator();
      end else
      // workaround for calling paramless constuctor
      if TIDProcedure(Decl).IsConstructor then
      begin
        // case when we call overload/inherited constructor as a method within a method
        if not Assigned(LCallExpr.Instance) then
          LCallExpr.Instance := EContext.SContext.Proc.SelfParamExpression;

        Expression := process_CALL_constructor(EContext.SContext, LCallExpr, []);
      end else
      begin
        // иначе создаем процедурный тип, если он отсутствовал
        TIDProcedure(Decl).CreateProcedureTypeIfNeed(Scope);
        PMContext.DataType := TIDProcedure(Decl).DataType;
        AddType(TIDProcedure(Decl).DataType);
      end;
    end;
    {built-in}
    itMacroFunction: begin
      var LBuiltinExpr := TIDExpression.Create(Decl, PMContext.ID.TextPosition);
      Result := ParseBuiltinCall(Scope, LBuiltinExpr, EContext);
    end;
    {variable}
    itVar: begin
      // если есть открытая скобка, - значит вызов
      if Result = token_openround then
      begin
        if Decl.DataTypeID <> dtProcType then
          ERRORS.PROC_OR_PROCVAR_REQUIRED(PMContext.ID);

        Expression := TIDCallExpression.Create(Decl, PMContext.ID.TextPosition);
        Result := ParseEntryCall(Scope, TIDCallExpression(Expression), EContext, ASTE);
        Expression := nil;
      end else
        Expression := TIDExpression.Create(Decl, PMContext.ID.TextPosition);
    end;
    {const/unit/label}
    itConst, itUnit, itLabel: begin
      Expression := TIDExpression.Create(Decl, PMContext.ID.TextPosition);
      PMContext.DataType := Decl.DataType;
    end;
    {property}
    itProperty: begin
      Expression := TIDExpression.Create(Decl, PMContext.ID.TextPosition);
      PMContext.DataType := Decl.DataType;
    end;
    {type}
    itType: begin
      {generic param}
      if Result = token_less then
        Result := ParseGenericTypeSpec(Scope, Scope, PMContext.ID, TIdType(Decl));

      Expression := TIDExpression.Create(Decl, PMContext.ID.TextPosition);

      {explicit typecase}
      if Result = token_openround then
         Result := ParseExplicitCast(Scope, EContext.SContext, Expression);

      PMContext.DataType := Decl.DataType;
    end;
  else
    ERRORS.FEATURE_NOT_SUPPORTED;
  end;

  if Assigned(Expression) then
    ASTE.AddDeclItem(Expression.Declaration, Expression.TextPosition);
end;

function TASTDelphiUnit.ParseMember(Scope: TScope; var EContext: TEContext; const ASTE: TASTExpression): TTokenID;
var
  Left, Right: TIDExpression;
  Decl: TIDDeclaration;
  SearchScope: TScope;
  DataType: TIDType;
begin
  Left := EContext.RPNPopExpression();
  Decl := Left.Declaration;
  // todo: system fail if decl is not assigned

  SearchScope := nil;

  case Decl.ItemType of

    itUnit: begin
      // if this is the full name of THIS unit, let's search in implementation first
      if Decl.ID.Name = Self._ID.Name then
        SearchScope := ImplScope
      else
        SearchScope := TIDNameSpace(Decl).Members
    end;

    itType: begin
      if Decl is TIDAliasType then
        Decl := TIDAliasType(Decl).Original;

      if Assigned(TIDType(Decl).Helper) then
        SearchScope := TIDType(Decl).Helper.Members
      else
      if Decl is TIDStructure then
        SearchScope := TIDStructure(Decl).Members
      else
      if Decl.ClassType = TIDEnum then
        SearchScope := TIDEnum(Decl).Items
      else
      if Decl.ClassType = TIDDynArray then
        SearchScope := TIDDynArray(Decl).GetHelperScope;

      if Decl.ClassType = TIDGenericParam then
      begin
        SearchScope := (TIDGenericParam(Decl).ConstraintType as TIDStructure).Members;
      end;
    end;

    itProcedure, itVar, itConst, itProperty: begin

      if Decl.ItemType = itProcedure then
        DataType := TIDProcedure(Decl).ResultType.ActualDataType
      else begin
        DataType := Left.DataType.ActualDataType;
        // auto-resolve of function call (when parentheses are missed)
        if (DataType.DataTypeID = dtProcType) and Assigned(TIDProcType(DataType).ResultType) then
          DataType := TIDProcType(DataType).ResultType;
      end;

      if DataType.ClassType = TIDAliasType then
        DataType := TIDAliasType(DataType).Original;

      while DataType.DataTypeID in [dtPointer, dtClassOf] do
        if Assigned(TIDPointer(DataType).ReferenceType) then
          DataType := GetPtrReferenceType(TIDPointer(DataType)).ActualDataType
        else
          ERRORS.IDENTIFIER_HAS_NO_MEMBERS(Decl);

      if (DataType.DataTypeID = dtGeneric) and Assigned(TIDGenericParam(DataType).ConstraintType) then
        DataType := TIDGenericParam(DataType).ConstraintType;

      if Assigned(DataType.Helper) then
        // todo: join helper scope and decl's scope together
        SearchScope := DataType.Helper.Members
      else
      if DataType is TIDStructure then
        SearchScope := TIDStructure(DataType).Members
      else
      if Decl.ClassType = TIDEnum then
        SearchScope := TIDEnum(Decl).Items
    end;
  else
    AbortWorkInternal('Unsuported decl type: %d', [Ord(Decl.ItemType)])
  end;

  if not Assigned(SearchScope) then
    ERRORS.IDENTIFIER_HAS_NO_MEMBERS(Decl);

  ASTE.AddOperation<TASTOpMemberAccess>;

  Lexer_NextToken(Scope);
  Result := ParseIdentifier(Scope, SearchScope, Right, EContext, Left, ASTE);
  if Assigned(Right) then
    EContext.RPNPushExpression(Right);
end;

function TASTDelphiUnit.ParseMemberCall(Scope: TScope; var EContext: TEContext; const ASTE: TASTExpression): TTokenID;
var
  Expr: TIDExpression;
  CallExpr: TIDCallExpression;
begin
  Expr := EContext.RPNPopExpression();

  if Expr.ItemType = itProcedure then
    CallExpr := TIDCallExpression.Create(Expr.Declaration, Expr.TextPosition)
  else
  if (Expr.ItemType = itVar) and (Expr.DataType is TIDProcType) then
  begin
    CallExpr := TIDCastedCallExpression.Create(Expr.Declaration, Expr.TextPosition);
    TIDCastedCallExpression(CallExpr).DataType := Expr.DataType;
  end else begin
    ERRORS.FEATURE_NOT_SUPPORTED;
    CallExpr := nil;
  end;
  Result := ParseEntryCall(Scope, CallExpr, EContext, ASTE);
    (*// если это метод, подставляем self из пула
    if PMContext.Count > 0 then
    begin
      if PMContext.Count > 1 then
        Expr := ProcessMemberExpression({SContext}nil, WasProperty, PMContext)
      else
        Expr := PMContext.Last;

      if (Expr.Declaration is TIDField) and (PMContext.Count = 1) then
      begin
        NExpr := GetTMPRefExpr(SContext, Expr.DataType);
        NExpr.TextPosition := Expr.TextPosition;
        Expr := NExpr;
      end;
      TIDCallExpression(Expression).Instance := Expr;
    end else
    if (Decl.ItemType = itProcedure) and Assigned(TIDProcedure(Decl).Struct) and
       (TIDProcedure(Decl).Struct = SContext.Proc.Struct) then
    begin
      // если это собственный метод, добавляем self из списка параметров
      TIDCallExpression(Expression).Instance := TIDExpression.Create(SContext.Proc.SelfParam);
    end;*)
end;

function TASTDelphiUnit.ParseNamedTypeDecl(Scope: TScope): TTokenID;
var
  ID: TIdentifier;
  Decl: TIDType;
  GenericParams: TIDTypeArray;
  GDescriptor: PGenericDescriptor;
begin
  CheckAndParseAttribute(Scope);

  Lexer_ReadCurrIdentifier(ID);

  Result := Lexer_NextToken(Scope);

  {if there is "<" - read generic params}
  if Result = token_less then begin
    GDescriptor := TGenericDescriptor.Create(Scope);
    Result := ParseGenericsHeader(GDescriptor.Scope, {out} GenericParams);
    GDescriptor.GenericParams := GenericParams;
    GDescriptor.SearchName := format('%s<%d>', [ID.Name, Length(GenericParams)]);
  end else
    GDescriptor := nil;

  Lexer_MatchToken(Result, token_equal);

  Lexer_NextToken(Scope);

  Result := ParseTypeDecl(Scope, GDescriptor, ID, Decl);
  if not Assigned(Decl) then
    ERRORS.INVALID_TYPE_DECLARATION(ID);

  if Result = token_semicolon then
    Result := Lexer_NextToken(Scope);

  CheckForwardPtrDeclarations;
end;

function TASTDelphiUnit.ParseTypeSection(Scope: TScope): TTokenID;
begin
  Lexer_NextToken(Scope);
  repeat
    Result :=  ParseNamedTypeDecl(Scope);
  until (Result <> token_identifier) and (Result <> token_openblock);
end;

function TASTDelphiUnit.ParseTypeSectionInStruct(Scope: TScope): TTokenID;
begin
  Lexer_NextToken(Scope);
  while True do begin
    Result := ParseNamedTypeDecl(Scope);

    if Result = token_openblock then
      Result := ParseAttribute(Scope);

    if (Result <> token_identifier) or
       (Lexer_AmbiguousId in [tokenD_strict,
                              tokenD_private,
                              tokenD_protected,
                              tokenD_public,
                              tokenD_published]) then
     Break;
  end;
end;

function TASTDelphiUnit.ParseIndexedPropertyArgs(Scope: TScope; out ArgumentsCount: Integer; var EContext: TEContext): TTokenID;
var
  Expr: TIDExpression;
  InnerEContext: TEContext;
  SContext: PSContext;
  ASTExpr: TASTExpression;
begin
  ArgumentsCount := 0;
  SContext := addr(EContext.SContext);
  InitEContext(InnerEContext, SContext^, ExprNested);
  {цикл парсинга аргументов}
  while true do begin
    Lexer_NextToken(Scope);
    Result := ParseExpression(Scope, SContext^, InnerEContext, ASTExpr);
    Expr := InnerEContext.Result;
    if Assigned(Expr) then begin
      if Expr.DataType = Sys._Boolean then
      begin
        {if (Expr.ItemType = itVar) and Expr.IsAnonymous then
          Bool_CompleteImmediateExpression(InnerEContext, Expr);}
      end;
      EContext.RPNPushExpression(Expr);
    end else begin
      // Добавляем пустой Expression для значения по умолчанию
      EContext.RPNPushExpression(nil);
    end;
    Inc(ArgumentsCount);
    case Result of
      token_coma: begin
        InnerEContext.Reset;
        continue;
      end;
      token_closeblock: begin
        Result := Lexer_NextToken(Scope);
        Break;
      end;
    else
      ERRORS.INCOMPLETE_STATEMENT('indexed args');
    end;
  end;
end;

function TASTDelphiUnit.ParseInheritedStatement(Scope: TScope; const EContext: TEContext): TTokenID;
var
  Proc, InheritedProc: TIDProcedure;
  CallExpr: TIDCallExpression;
  CallArgs: TIDExpressions;
  i, ArgsCnt: Integer;
  ResultExpr: TIDExpression;
  Decl: TIDDeclaration;
  Ancestor: TIDStructure;
  ID: TIdentifier;
  KW: TASTKWInheritedCall;
begin
  ArgsCnt := 0;
  Proc := EContext.SContext.Proc;
  KW := EContext.SContext.Add(TASTKWInheritedCall) as TASTKWInheritedCall;
  InheritedProc := nil;
  Result := Lexer_NextToken(Scope);
  if Result = token_identifier then
  begin
    {если после inherited идет полное описание вызова метода}
    Ancestor := Proc.Struct.Ancestor;
    while Assigned(Ancestor) do
    begin
      Lexer_ReadCurrIdentifier(ID);
      Decl := FindID(Ancestor.Members, ID);
      if not Assigned(Decl) then
        ERRORS.UNDECLARED_ID(ID);
      case Decl.ItemType of
        itProcedure: begin
          InheritedProc := Decl as TIDProcedure;
          Result := Lexer_NextToken(Scope);
         ArgsCnt := Length(CallArgs);
          if Result = token_openround then
          begin
            CallExpr := TIDCallExpression.Create(InheritedProc, Lexer_Line);
            CallExpr.Instance := Proc.SelfParamExpression;
            CallExpr.ArgumentsCount := ArgsCnt;
            Result := ParseEntryCall(Scope, CallExpr, EContext, nil {tmp!!!});
          end;
          Break;
        end;
        itProperty: begin
          // todo: need to specify class-owner to access the property
          var LExpr := TIDExpression.Create(Decl, Lexer_Position);
          EContext.RPNPushExpression(LExpr);
          Result := Lexer_NextToken(Scope);
        end
      else
        ERRORS.PROC_OR_TYPE_REQUIRED(ID);
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

    InheritedProc := Proc.Struct.FindVirtualProcInAncestor(Proc);

    if not Assigned(InheritedProc) and not (pfConstructor in Proc.Flags)  then
      ERRORS.NO_METHOD_IN_BASE_CLASS(Proc);
  end;

  // if inherited was found, generate call expression
  if Assigned(InheritedProc) then
  begin
    CallExpr := TIDCallExpression.Create(InheritedProc, Lexer_Line);
    CallExpr.Instance := Proc.SelfParamExpression;
    CallExpr.ArgumentsCount := ArgsCnt;

    ResultExpr := Process_CALL_direct(EContext.SContext, CallExpr, CallArgs);
    if Assigned(ResultExpr) then
      EContext.RPNPushExpression(ResultExpr);
  end;
end;

function TASTDelphiUnit.ParseInitSection: TTokenID;
var
  SContext: TSContext;
begin
  if FInitProcExplicit then
    ERRORS.INIT_SECTION_ALREADY_DEFINED;

  FInitProcExplicit := True;

  SContext := TSContext.Create(Self, ImplScope, InitProc as TASTDelphiProc, TASTDelphiProc(InitProc).Body);
  Lexer_NextToken(InitProc.Scope);
  InitProc.FirstBodyLine := Lexer_Line;
  Result := ParseStatements(InitProc.Scope, SContext, {IsBlock:} True);
  InitProc.LastBodyLine := Lexer_Line;
end;

function TASTDelphiUnit.ParseInterfaceType(Scope, GenericScope: TScope; GDescriptor: PGenericDescriptor;
  ADispInterface: Boolean; const ID: TIdentifier; out Decl: TIDInterface): TTokenID;
var
  Expr: TIDExpression;
  SearchName: string;
begin
  Decl := TIDInterface(ParseGenericTypeDecl(Scope, GDescriptor, ID, TIDInterface));
  Decl.IsDisp := ADispInterface;

  Result := Lexer_NextToken(Scope);

  // parse the ancestor
  if Result = token_openround then
  begin
    Lexer_NextToken(Scope);
    Result := ParseConstExpression(Scope, Expr, ExprNested);
    CheckInterfaceType(Expr);
    var Ancestor := Expr.AsType.ActualDataType;
    if Ancestor = Decl then
      AbortWork(sRecurciveTypeLinkIsNotAllowed, Expr.TextPosition);
    Lexer_MatchToken(Result, token_closeround);
    Decl.AncestorDecl := Expr.AsType;
    Result := Lexer_NextToken(Scope);
  end;

  // semicolon means this is forward declaration
  if Result = token_semicolon then
  begin
    if Decl.NeedForward then
      ERRORS.ID_REDECLARATED(ID);
    Decl.NeedForward := True;
    Exit;
  end;

  // parse a GUID constant
  if Result = token_openblock then
    Result := ParseIntfGUID(Scope, Decl);

  while True do begin
    case Result of
      token_openblock: Result := ParseAttribute(Scope);
      token_procedure: Result := ParseProcedure(Decl.Members, ptProc, Decl);
      token_function: Result := ParseProcedure(Decl.Members, ptFunc, Decl);
      token_property: Result := ParseProperty(Decl.Members, Decl);
    else
      break;
    end;
  end;
  CheckIncompleteType(Decl.Members);
  Decl.StructFlags := Decl.StructFlags + [StructCompleted];
  Lexer_MatchToken(Result, token_end);
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ParseIntfGUID(Scope: TScope; Decl: TIDInterface): TTokenID;
var
  LUIDStrExpr: TIDExpression;
  LGUIDConst: TIDGuidConstant;
  LGUID: TGUID;
begin
  Lexer_NextToken(Scope);
  if Lexer_IdentifireType = itString then
  begin
    Result := ParseConstExpression(Scope, {out} LUIDStrExpr, ExprNested);
    CheckEmptyExpression(LUIDStrExpr);
    CheckStringExpression(LUIDStrExpr);

    try
      LGUID := StringToGUID(LUIDStrExpr.AsStrConst.Value);
    except
      AbortWork('Invalid GUID', Lexer_Position);
    end;

    Lexer_MatchToken(Result, token_closeblock);
    
    LGUIDConst := TIDGuidConstant.CreateAsAnonymous(Scope, Sys._GuidType, LGUID);
    LGUIDConst.TextPosition := Lexer_Position;
    Decl.GUIDDecl := LGUIDConst;

    Result := Lexer_NextToken(Scope);
  end else
    Result := ParseAttribute(Scope);
end;

function TASTDelphiUnit.ParseFinalSection: TTokenID;
var
  SContext: TSContext;
begin
  if fFinalProcExplicit then
    ERRORS.FINAL_SECTION_ALREADY_DEFINED;
  FFinalProcExplicit := True;
  SContext := TSContext.Create(Self, ImplScope, FinalProc as TASTDelphiProc, TASTDelphiProc(FinalProc).Body);
  Lexer_NextToken(FinalProc.Scope);
  FinalProc.FirstBodyLine := Lexer_Line;
  Result := ParseStatements(FinalProc.Scope, SContext, {IsBlock:} True);
  FinalProc.LastBodyLine := Lexer_Line;
end;

function TASTDelphiUnit.ParseVarDefaultValue(Scope: TScope; DataType: TIDType; out DefaultValue: TIDExpression): TTokenID;
var
  SContext: TSContext;
  EContext: TEContext;
  ASTE: TASTExpression;
begin
  case DataType.DataTypeID of
    dtStaticArray: Result := ParseVarStaticArrayDefaultValue(Scope, DataType as TIDArray, DefaultValue);
    dtGuid, dtRecord: begin
      Result := Lexer_NextToken(Scope);
      if (DataType.Module = SYSUnit) and (DataType.Name = 'TGUID') and (Result <> token_openround) then
      begin
        Result := ParseConstExpression(Scope, DefaultValue, ExprRValue);
      end else
        Result := ParseVarRecordDefaultValue(Scope, DataType as TIDStructure, DefaultValue);
    end
  else
    SContext := fUnitSContext;

    Lexer_NextToken(Scope);

    if Scope.ScopeType = stLocal then
      Result := ParseConstExpression(Scope, DefaultValue, ExprRValue)
    else begin
      InitEContext(EContext, SContext, ExprRValue);
      Result := ParseExpression(Scope, SContext, EContext, ASTE);
      DefaultValue := EContext.Result;
    end;
    CheckEmptyExpression(DefaultValue);

    if CheckImplicit(SContext, DefaultValue, DataType) = nil then
    begin
      CheckImplicit(SContext, DefaultValue, DataType);
      ERRORS.INCOMPATIBLE_TYPES(DefaultValue, DataType);
    end;

    DefaultValue := MatchImplicit3(SContext, DefaultValue, DataType);

    //todo: do we still need it?
    // // replace actual anonymous constant type to the required
    // if DefaultValue.IsAnonymous and not DefaultValue.Declaration.IsSystem then
    //   DefaultValue.Declaration.DataType := DataType;
  end;
end;

function TASTDelphiUnit.ParseFieldsInCaseRecord(Scope: TScope; Visibility: TVisibility; ARecord: TIDRecord): TTokenID;
var
  i, c: Integer;
  DataType: TIDType;
  Names: TIdentifiersPool;
  VarFlags: TVariableFlags;
  LCaseFields: TIDFieldArray;
begin
  c := 0;
  Names := TIdentifiersPool.Create(2);
  while True do begin
    VarFlags := [];
    Result := CheckAndParseAttribute(Scope);
    Lexer_MatchIdentifier(Result);
    Names.Add;
    Lexer_ReadCurrIdentifier(Names.Items[c]);
    Result := Lexer_NextToken(Scope);
    if Result = token_Coma then begin
      Inc(c);
      Lexer_NextToken(Scope);
      Continue;
    end;
    Lexer_MatchToken(Result, token_colon);
    // парсим тип
    Result := ParseTypeSpec(Scope, DataType);

    for i := 0 to c do begin
      var LField := TIDField.Create(ARecord, Names.Items[i]);
      LField.DataType := DataType;
      LField.Visibility := Visibility;
      LField.DefaultValue := nil;
      LField.Flags := LField.Flags + VarFlags;
      Scope.AddVariable(LField);
      LCaseFields := LCaseFields + [LField];
    end;

    if Result = token_semicolon then
    begin
      Result := Lexer_NextToken(Scope);
      if Lexer.TokenCanBeID(Result) then
      begin
        c := 0;
        Continue;
      end;
    end;
    Break;
  end;
  // add a new case entry with all fields
  ARecord.AddCase(LCaseFields);
end;

function TASTDelphiUnit.ParseVarRecordDefaultValue(Scope: TScope; Struct: TIDStructure; out DefaultValue: TIDExpression): TTokenID;
var
  i: Integer;
  ID: TIdentifier;
  Expr: TIDExpression;
  Decl: TIDDeclaration;
  Field: TIDField;
  Expressions: TIDRecordConstantFields;
begin
  i := 0;
  Lexer_MatchCurToken(token_openround);
  SetLength(Expressions, Struct.FieldsCount);
  Lexer_NextToken(Scope);
  while True do begin
    Lexer_ReadCurrIdentifier(ID);
    Lexer_ReadToken(Scope, token_colon);
    Field := Struct.FindField(ID.Name);
    if not Assigned(Field) then
      ERRORS.UNDECLARED_ID(ID);

    if Field.DataType.DataTypeID = dtStaticArray then
      Result := ParseVarStaticArrayDefaultValue(Scope, TIDArray(Field.DataType), Expr)
    else begin
      Lexer_NextToken(Scope);
      Result := ParseConstExpression(Scope, Expr, ExprRValue);
    end;

    Expressions[i].Field := Field;
    Expressions[i].Value := Expr;
    Inc(i);
    if Result = token_semicolon then
    begin
      Result := Lexer_NextToken(Scope);
      if Result = token_identifier then
        Continue;
      end;
    Break;
  end;
  // create anonymous record constant
  Decl := TIDRecordConstant.CreateAsAnonymous(Scope, Struct, Expressions);
  DefaultValue := TIDExpression.Create(Decl, Lexer_Position);

  Lexer_MatchToken(Result, token_closeround);
  Result := Lexer_NextToken(Scope);
end;

function TASTDelphiUnit.ParseVarSection(Scope: TScope; Visibility: TVisibility; IsWeak: Boolean): TTokenID;
var
  i, c: Integer;
  DataType: TIDType;
  DefaultValue: TIDExpression;
  DeclAbsolute: TIDDeclaration;
  ID: TIdentifier;
  Variable: TIDVariable;
  Names: TIdentifiersPool;
  VarFlags: TVariableFlags;
  DeprecatedText: TIDExpression;
begin
  c := 0;
  Names := TIdentifiersPool.Create(2);
  while True do begin
    VarFlags := [];
    Result := CheckAndParseAttribute(Scope);
    Lexer_MatchIdentifier(Result);
    Names.Add;
    Lexer_ReadCurrIdentifier(Names.Items[c]);
    Result := Lexer_NextToken(Scope);
    if Result = token_Coma then begin
      Inc(c);
      Lexer_NextToken(Scope);
      Continue;
    end;
    Lexer_MatchToken(Result, token_colon);
    // парсим тип
    Result := ParseTypeSpec(Scope, {out} DataType);
    DeclAbsolute := nil;
    DefaultValue := nil;

    // platform declaration
    Result := CheckAndParsePlatform(Scope);

    // absolute
    Result := CheckAndParseAbsolute(Scope, {out} DeclAbsolute);

    if Result = token_equal then
      Result := ParseVarDefaultValue(Scope, DataType, {out} DefaultValue);

    // deprecated
    if Result = token_deprecated then
      Result := ParseDeprecated(Scope, DeprecatedText);

    for i := 0 to c do begin
      Variable := TIDVariable.Create(Scope, Names.Items[i]);
      // если это слабая ссылка - получаем соответствующий тип
      if IsWeak then
        DataType := GetWeakRefType(Scope, DataType);
      Variable.DataType := DataType;
      Variable.Visibility := Visibility;
      Variable.DefaultValue := DefaultValue;
      Variable.Absolute := TIDVariable(DeclAbsolute);
      Variable.Flags := Variable.Flags + VarFlags;
      Scope.AddVariable(Variable);
    end;

    if Result = token_semicolon then
      Result := Lexer_NextToken(Scope);

    if Result <> token_identifier then
      Exit;
    c := 0;
  end;
end;

function TASTDelphiUnit.ParseFieldsSection(Scope: TScope; Visibility: TVisibility; Struct: TIDStructure; IsClass: Boolean): TTokenID;
var
  i, c: Integer;
  DataType: TIDType;
  Field: TIDVariable;
  Names: TIdentifiersPool;
  VarFlags: TVariableFlags;
  //DeprecatedText: TIDExpression;
begin
  c := 0;
  Names := TIdentifiersPool.Create(2);
  while True do begin
    VarFlags := [];
    Result := CheckAndParseAttribute(Scope);
    Lexer_MatchIdentifier(Result);
    Names.Add;
    Lexer_ReadCurrIdentifier(Names.Items[c]);
    Result := Lexer_NextToken(Scope);
    if Result = token_Coma then begin
      Inc(c);
      Lexer_NextToken(Scope);
      Continue;
    end;

    Lexer_MatchToken(Result, token_colon);
    // парсим тип
    Result := ParseTypeSpec(Scope, DataType);

    // platform declaration
    Result := CheckAndParsePlatform(Scope);

    // deprecated
    Result := CheckAndParseDeprecated(Scope, Result);

    for i := 0 to c do begin
      Field := TIDField.Create(Struct, Names.Items[i]);
      Field.DataType := DataType;
      Field.Visibility := Visibility;
      Field.DefaultValue := nil;
      Field.Flags := Field.Flags + VarFlags;
      Scope.AddVariable(Field);
    end;

    if Result = token_semicolon then
      Result := Lexer_NextToken(Scope);

    Exit;
  end;
end;

function TASTDelphiUnit.ParseVarStaticArrayDefaultValue(Scope: TScope; ArrType: TIDArray; out DefaultValue: TIDExpression): TTokenID;
type
  TDimensionsArray = array of Integer;

  function DoParse(ArrType: TIDArray; const Dimensions: TDimensionsArray;
                   DimIndex: Integer; const CArray: TIDDynArrayConstant): TTokenID;
  var
    i, c: Integer;
    Expr: TIDExpression;
    NewScope: TScope;
    DimensionsCount: Integer;
  begin
    Result := Lexer_NextToken(Scope);
    // first element initializer
    if Result = token_identifier then
    begin
      Result := ParseConstExpression(Scope, Expr, ExprRValue);
      Exit; // todo:
    end;

    if ArrType.ElementDataType.DataTypeID = dtRecord then
    begin
      NewScope := TRecordInitScope.Create(stGlobal, Scope);
      TRecordInitScope(NewScope).Struct := TIDStructure(ArrType.ElementDataType);
      NewScope.AddScope(TIDStructure(ArrType.ElementDataType).Members);
    end else
      NewScope := Scope;

    DimensionsCount := Length(Dimensions);

    Lexer_MatchToken(Result, token_openround);
    c := Dimensions[DimIndex] - 1;
    for i := 0 to c do
    begin
      if DimensionsCount > (DimIndex + 1) then
        Result := DoParse(ArrType, Dimensions, DimIndex + 1, CArray)
      else begin
        Lexer_NextToken(Scope);
        Result := ParseConstExpression(NewScope, Expr, ExprNested);
        CheckEmptyExpression(Expr);
        if CheckImplicit(fUnitSContext, Expr, CArray.ElementType) = nil then
          ERRORS.INCOMPATIBLE_TYPES(Expr, CArray.ElementType);
        Expr.Declaration.DataType := CArray.ElementType;
        CArray.AddItem(Expr);
      end;
      if i < c  then
        Lexer_MatchToken(Result, token_coma);
    end;
    Lexer_MatchToken(Result, token_closeround);
    Result := Lexer_NextToken(Scope);
  end;

  function GetTotalDimensionsCount(ArrayType: TIDArray; out ElementType: TIDType): TDimensionsArray;
  begin
    SetLength(Result, ArrayType.DimensionsCount);
    for var i := 0 to ArrayType.DimensionsCount - 1 do
      Result[i] := ArrayType.Dimensions[i].ElementsCount;

    ElementType := ArrayType.ElementDataType;

    if ArrayType.ElementDataType.DataTypeID = dtStaticArray then
      Result := Result + GetTotalDimensionsCount(TIDArray(ArrayType.ElementDataType), ElementType);
  end;

var
  ElementType: TIDType;
  Dimensions: TDimensionsArray;
begin
  Dimensions := GetTotalDimensionsCount(ArrType, ElementType);
  DefaultValue := CreateAnonymousConstTuple(Scope, ElementType);
  Result := DoParse(ArrType, Dimensions, 0, DefaultValue.AsDynArrayConst);
end;

function TASTDelphiUnit.InferTypeByVector(Scope: TScope; Vector: TIDExpressions): TIDType;

  function MaxType(Type1, Type2: TIDType): TIDType;
  begin
    if Type1 = Type2 then
      Exit(Type1);

    if (Type1 = Sys._Variant) or
       (Type2 = Sys._Variant) then
     Exit(Sys._Variant);

    if (Type1.DataTypeID = dtRange) then
      Exit(Type1);

    if (Type2.DataTypeID = dtRange) then
      Exit(Type2);

    var AImplicit := Type2.GetImplicitOperatorTo(Type1);
    if Assigned(AImplicit) then
    begin
      if Type1.DataSize >= Type2.DataSize then
        Result := Type1
      else
        Result := Type2;
    end else
      Result := Sys._Variant;
  end;

begin
  if Length(Vector) = 0 then
    Exit(Sys._Void);

  var IsSet := False;
  var ItemType: TIDType := nil;

  for var AIndex := 0 to Length(Vector) - 1 do
  begin
    var Expr := Vector[AIndex];

    IsSet := IsSet or Expr.IsRangeConst or (Expr.DataTypeID = dtEnum);

    if AIndex > 0 then
      ItemType := MaxType(ItemType, Expr.DataType)
    else
      ItemType := Expr.DataType;
  end;

  if not Assigned(ItemType) then
    ERRORS.COULD_NOT_INFER_DYNAMIC_ARRAY_TYPE(Vector);

  if IsSet then
  begin
    // create or find in the cache anonymous set type
    var ASetBaseType: TIDOrdinal;
    if ItemType.DataTypeID = dtRange then
      ASetBaseType := (ItemType as TIDRangeType).BaseType
    else
      ASetBaseType := (ItemType as TIDEnum);
    // search in the cache first
    var SetType := fCache.FindSet(ASetBaseType);
    if not Assigned(SetType) then
    begin
      SetType := TIDSet.CreateAsAnonymous(Scope, ASetBaseType);
      // add to types pool
      AddType(SetType);
      fCache.Add(SetType);
    end;
    Result := SetType;
  end else begin
    // create anonymous array type
    var ArrType := TIDDynArray.CreateAsAnonymous(Scope);
    ArrType.ElementDataType := ItemType;
    // add to types pool
    AddType(ArrType);
    Result := ArrType;
  end;
end;

procedure TASTDelphiUnit.ParseVector(Scope: TScope; var EContext: TEContext);
var
  i, c, Capacity: Integer;
  InnerEContext: TEContext;
  Expr: TIDExpression;
  Token: TTokenID;
  SItems, DItems: TIDExpressions;
  IsStatic: Boolean;
  ASTExpr: TASTExpression;
begin
  i := 0;
  c := 0;
  Capacity := 8;
  IsStatic := True;
  SetLength(SItems, Capacity);
  var SContext := EContext.SContext;
  InitEContext(InnerEContext, SContext, ExprNested);
  while True do begin
    Lexer_NextToken(Scope);
    Token := ParseExpression(Scope, SContext, InnerEContext, {out} ASTExpr);
    Expr := InnerEContext.Result;
    if Assigned(Expr) then begin
      if Expr.Declaration.ItemType <> itConst then
        IsStatic := False;
      Inc(c);
      SItems[i] := Expr;
    end else
    if i > 0 then
      CheckEmptyExpression(Expr);

    case Token of
      token_coma: begin
        if i = 0 then CheckEmptyExpression(Expr);
        Inc(i);
        if i >= Capacity then begin
          Capacity := Capacity * 2;
          SetLength(SItems, Capacity);
        end;
        InnerEContext.Reset;
        Continue;
      end;
      token_closeblock: Break;
      else
        ERRORS.UNCLOSED_OPEN_BLOCK;
    end;
  end;

  var AConst: TIDConstant;
  if c > 0 then
  begin
    // prepare expressions array
    SetLength(DItems, c);
    Move(SItems[0], DItems[0], SizeOf(Pointer)*c);
    // infer array type
    var ArrayType := InferTypeByVector(Scope, DItems);
    if ArrayType.DataTypeID = dtSet then
    begin
      // create anonymous set constant
      AConst := TIDSetConstant.CreateAsAnonymous(Scope, ArrayType, DItems);
      AddConstant(AConst);
    end else begin
      // create anonymous array constant
      AConst := TIDDynArrayConstant.CreateAsAnonymous(Scope, ArrayType, DItems);
      TIDDynArrayConstant(AConst).ArrayStatic := IsStatic;
      if IsStatic then
        AddConstant(AConst);
    end;
  end else
    // treats empty brackets [] as empty array always
    AConst := Sys._EmptyArrayConstant;

  Expr := TIDExpression.Create(AConst, Lexer.Position);
  // заталкиваем массив в стек
  EContext.RPNPushExpression(Expr);
end;

function TASTDelphiUnit.GetWeakRefType(Scope: TScope; SourceDataType: TIDType): TIDWeekRef;
begin
   if not (SourceDataType.DataTypeID in [dtClass, dtInterface]) then
     ERRORS.WEAKREF_CANNOT_BE_DECLARED_FOR_TYPE(SourceDataType);

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

(*function TASTDelphiUnit.ParsePropertyMember(var PMContext: TPMContext;
                                            Scope: TScope;
                                            Prop: TIDProperty;
                                            out Expression: TIDExpression;
                                            var EContext: TEContext;
                                            const PrevExpr: TIDExpression): TTokenID;
var
  CallExpr: TIDCallExpression;
  SelfExpr: TIDExpression;
  Accessor: TIDDeclaration;
  ArgumentsCount: Integer;
begin
  Result := Lexer_CurTokenID;
  Expression := TIDExpression.Create(Prop, PMContext.ID.TextPosition);
  if EContext.EPosition = ExprLValue then begin
    Accessor := TIDProperty(Prop).Setter;
    if not Assigned(Accessor) then
      ERRORS.CANNOT_MODIFY_READONLY_PROPERTY(Expression);
    // если сеттер - процедура, отодвигаем генерацию вызова на момент оброботки опрератора присвоения
    if Accessor.ItemType = itProcedure then
    begin
      Exit;
    end;
  end else begin
    Accessor := TIDProperty(Prop).Getter;
    if not Assigned(Accessor) then
      ERRORS.CANNOT_ACCESS_TO_WRITEONLY_PROPERTY(Expression);
  end;
  Expression.Declaration := Accessor;
  case Accessor.ItemType of
    itVar: Expression.Declaration := Accessor;
    itProperty: {продолжаем выполнение};
    itProcedure: begin
      if Assigned(PrevExpr) then
        SelfExpr := PrevExpr
      else
        SelfExpr := TIDProcedure(EContext.Proc).SelfParamExpression;

      CallExpr := TIDCallExpression.Create(Accessor);
      CallExpr.TextPosition := Expression.TextPosition;
      CallExpr.Instance := SelfExpr;

      if Prop.ParamsCount > 0 then
        Result := ParseIndexedPropertyArgs(Scope, ArgumentsCount, EContext)
      else
        ArgumentsCount := 0;

      CallExpr.ArgumentsCount := ArgumentsCount;
      EContext.RPNPushExpression(CallExpr);

      Expression := Process_CALL(EContext);

      PMContext.Clear;
    end;
  else
    ERRORS.FEATURE_NOT_SUPPORTED;
  end;
end;*)

function TASTDelphiUnit.CreateAnonymousConstTuple(Scope: TScope; ElementDataType: TIDType): TIDExpression;
var
  Decl: TIDDynArrayConstant;
  AType: TIDDynArray;
begin
  AType := TIDDynArray.CreateAsAnonymous(Scope);
  AType.ElementDataType := ElementDataType;
  Decl := TIDDynArrayConstant.CreateAsAnonymous(Scope, AType, nil);
  Result := TIDExpression.Create(Decl);
end;

class function TASTDelphiUnit.CreateRangeType(Scope: TScope;  LoBound, HiBound: Integer): TIDRangeType;
begin
  Result := TIDRangeType.CreateAsAnonymous(Scope);
  Result.LowBound := LoBound;
  Result.HighBound := HiBound;
end;

procedure TASTDelphiUnit.CheckIntfSectionMissing(Scope: TScope);
begin
  if not Assigned(Scope) then
    ERRORS.INTF_SECTION_MISSING;
end;

procedure TASTDelphiUnit.CheckConstExpression(Expression: TIDExpression);
begin
  if not (Expression.Declaration.ItemType in [itConst, itType, itProcedure]) then
    ERRORS.CONST_EXPRESSION_REQUIRED(Expression);
end;

procedure TASTDelphiUnit.CheckConstValueOverflow(Src: TIDExpression; DstDataType: TIDType);
var
  Matched: Boolean;
  ValueI64: Int64;
  ValueU64: UInt64;
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
    // todo: it doesn't work with explicit casted values
    if not Matched then;
    //  ERRORS.CONST_VALUE_OVERFLOW(Src, DstDataType);
  end;
  // необходимо реализовать проверку остальных типов
end;

class procedure TASTDelphiUnit.CheckDestructorSignature(const DProc: TIDProcedure);
begin

end;

procedure TASTDelphiUnit.CheckEmptyExpression(Expression: TIDExpression);
begin
  if not Assigned(Expression) then
    ERRORS.EMPTY_EXPRESSION;
end;

procedure TASTDelphiUnit.CheckEndOfFile(Token: TTokenID);
begin
  if Token = token_eof then
    ERRORS.END_OF_FILE;
end;

procedure TASTDelphiUnit.CheckIntExpression(Expression: TIDExpression);
begin
  if not Assigned(Expression) or (Expression.DataTypeID >= dtNativeUInt) then
    ERRORS.INTEGER_TYPE_REQUIRED(Expression.TextPosition);
end;

procedure TASTDelphiUnit.CheckOrdinalExpression(Expression: TIDExpression);
begin
  if not Expression.DataType.IsOrdinal then
    ERRORS.ORDINAL_TYPE_REQUIRED(Expression.TextPosition);
end;

procedure TASTDelphiUnit.CheckOrdinalType(DataType: TIDType);
begin
  if not DataType.IsOrdinal then
    ERRORS.ORDINAL_TYPE_REQUIRED(Lexer_Position);
end;

class procedure TASTDelphiUnit.CheckNumericExpression(Expression: TIDExpression);
begin
  if not (Expression.DataTypeID in [dtInt8..dtChar]) then
    AbortWork(sNumericTypeRequired, Expression.TextPosition);
end;

class procedure TASTDelphiUnit.CheckPointerType(Expression: TIDExpression);
begin
  if not (Expression.DataTypeID in [dtPointer, dtPAnsiChar, dtPWideChar]) then
    AbortWork(sPointerTypeRequired, Expression.TextPosition);
end;

procedure TASTDelphiUnit.CheckProcedureType(DeclType: TIDType);
begin
  if (DeclType.DataTypeID <> dtProcType) then
    AbortWork('Procedure type required', Lexer.Position);
end;

class procedure TASTDelphiUnit.CheckIntConstInRange(const Expr: TIDExpression; HiBount, LowBound: Int64);
var
  V64: Int64;
begin
  V64 := Expr.AsIntConst.Value;
  if (V64 < LowBound) or (V64 > HiBount) then
    AbortWork('Eexpression must be in range [%d..%d]', [LowBound, HiBount], Expr.TextPosition);
end;

class procedure TASTDelphiUnit.CheckRecordType(Expression: TIDExpression);
var
  Decl: TIDDeclaration;
begin
  Decl := Expression.Declaration;
  if (Decl.ItemType <> itType) or
     (TIDType(Decl).DataTypeID <> dtRecord) then
    AbortWork(sRecordTypeRequired, Expression.TextPosition);
end;

class procedure TASTDelphiUnit.CheckReferenceType(Expression: TIDExpression);
begin
  if not Expression.DataType.IsReferenced then
    AbortWork(sReferenceTypeRequired, Expression.TextPosition);
end;

class procedure TASTDelphiUnit.CheckSetType(Expression: TIDExpression);
begin

end;

class procedure TASTDelphiUnit.CheckStaticRecordConstructorSign(const CProc: TIDProcedure);
begin

end;

procedure TASTDelphiUnit.CheckStingType(DataType: TIDType);
begin
  if DataType.DataTypeID <> dtString then
    AbortWork(sStringExpressionRequired, Lexer_Position);
end;

class procedure TASTDelphiUnit.CheckStringExpression(Expression: TIDExpression);
begin
  if MatchImplicit(Expression.DataType, Expression.Declaration.SysUnit._UnicodeString) = nil then
    AbortWork(sStringExpressionRequired, Expression.TextPosition);
end;

class procedure TASTDelphiUnit.CheckStructType(Decl: TIDType);
begin
  if not (Decl is TIDStructure) then
    AbortWork(sStructTypeRequired, Decl.TextPosition);
end;

class procedure TASTDelphiUnit.CheckStructType(Expression: TIDExpression);
var
  Decl: TIDDeclaration;
begin
  Decl := Expression.Declaration;
  if not (Decl is TIDStructure) then
    AbortWork(sStructTypeRequired, Expression.TextPosition);
end;

procedure TASTDelphiUnit.CheckType(Expression: TIDExpression);
begin
  if Expression.ItemType <> itType then
    ERRORS.TYPE_REQUIRED(Expression.TextPosition);
end;

class procedure TASTDelphiUnit.CheckInterfaceType(Expression: TIDExpression);
var
  Decl: TIDDeclaration;
begin
  Decl := Expression.Declaration;
  if (Decl.ItemType <> itType) or
     (TIDType(Decl).DataTypeID <> dtInterface) then
    AbortWork(sInterfaceTypeRequired, Expression.TextPosition);
end;

procedure TASTDelphiUnit.CheckClassOrIntfType(Expression: TIDExpression);
var
  Decl: TIDDeclaration;
begin
  Decl := Expression.Declaration;
  if (Decl.ItemType = itType) and
     ((TIDType(Decl).DataTypeID = dtClass) or
      (TIDType(Decl).DataTypeID = dtInterface)) then Exit;
  ERRORS.CLASS_OR_INTF_TYPE_REQUIRED(Expression.TextPosition);
end;

procedure TASTDelphiUnit.CheckClassOrClassOfOrIntfType(Expression: TIDExpression);
var
  Decl: TIDDeclaration;
begin
  Decl := Expression.Declaration;
  if (Decl.ItemType = itType) and
     ((TIDType(Decl).DataTypeID = dtClass) or
      (TIDType(Decl).DataTypeID = dtInterface)) then Exit;

  if (Decl.ItemType = itVar) and
     (TIDVariable(Decl).DataTypeID = dtClassOf) then Exit;

  ERRORS.CLASS_OR_INTF_TYPE_REQUIRED(Expression.TextPosition);
end;

procedure TASTDelphiUnit.CheckClassOrIntfType(DataType: TIDType; const TextPosition: TTextPosition);
begin
  if (DataType.DataTypeID = dtClass) or
     (DataType.DataTypeID = dtInterface) then Exit;
  ERRORS.CLASS_OR_INTF_TYPE_REQUIRED(TextPosition);
end;

procedure TASTDelphiUnit.CheckClassType(Expression: TIDExpression);
var
  Decl: TIDDeclaration;
begin
  Decl := Expression.Declaration;
  if (Decl.ItemType <> itType) or
     (TIDType(Decl).DataTypeID <> dtClass) then
    ERRORS.CLASS_TYPE_REQUIRED(Expression.TextPosition);
end;


procedure TASTDelphiUnit.CheckExceptionType(Decl: TIDDeclaration);
begin
  // todo: add System.Exception type inheritance
  if (Decl.ItemType <> itType) or (TIDType(Decl).DataTypeID <> dtClass) {or TIDClass(Decl).IsInheritsForm()} then
    ERRORS.CLASS_TYPE_REQUIRED(Lexer_Position);
end;

procedure TASTDelphiUnit.CheckClassExpression(Expression: TIDExpression);
begin
  if Expression.DataTypeID <> dtClass then
    ERRORS.CLASS_TYPE_REQUIRED(Expression.TextPosition);
end;

procedure TASTDelphiUnit.CheckImplicitTypes(Src, Dst: TIDType; Position: TTextPosition);
begin
  if not Assigned(Src) then
    ERRORS.EMPTY_EXPRESSION;
  if not Assigned(MatchImplicit(Src, Dst)) then
    AbortWork(sIncompatibleTypesFmt, [Src.DisplayName, Dst.DisplayName], Position);
end;

procedure TASTDelphiUnit.CheckIncompletedIntfProcs(AClassType: TIDClass);
begin
  for var LIntfIndex := 0 to AClassType.InterfacesCount - 1 do
  begin
    var LIntf := AClassType.Interfaces[LIntfIndex];
    for var LDeclIndex := 0 to LIntf.Members.ConstCount do
    begin
      var LDecl := LIntf.Members.Items[LDeclIndex];
      if LDecl.ItemType = itProcedure then
      begin
        var LIM := TIDProcedure(LDecl);
        var LCM := AClassType.FindMethod(LDecl.Name);
        if not Assigned(LCM) then
          ERRORS.INTF_METHOD_NOT_IMPLEMENTED(AClassType, LIM);

        if not StrictMatchProc(LIM, LCM) then
          ERRORS.INTF_METHOD_NOT_IMPLEMENTED(AClassType, LIM); // tmp !!!

        AClassType.MapInterfaceMethod(LIntf, LIM, LCM);
      end;
    end;
  end;
end;

procedure TASTDelphiUnit.CheckIncompleteFwdTypes;
begin
  EnumDeclarations(
    procedure (const AModule: TASTModule; const ADecl: TASTDeclaration)
    begin
      if ADecl is TIDPointer then
      begin
        // ReferenceType property will initialize the type
        var LDataType := TIDPointer(ADecl).ReferenceType;
        if Assigned(LDataType) then;
      end;
    end, {AUnitScope} scopeBoth);
end;

procedure TASTDelphiUnit.CheckIncompleteType(Fields: TScope);
begin
  EnumDeclarations(
    procedure (const AModule: TASTModule; const ADecl: TASTDeclaration)
    begin
      if ADecl is TIDVariable then
      begin
        var LFieldType := TIDVariable(ADecl).DataType;
        if (TIDVariable(ADecl).DataTypeID = dtRecord) and
           (not (StructCompleted in TIDStructure(LFieldType).StructFlags)) then
          AbortWork(sRecurciveTypeLinkIsNotAllowed, ADecl.ID.TextPosition);
      end;
    end, {AUnitScope} scopeBoth);
end;

procedure TASTDelphiUnit.CheckVarExpression(Expression: TIDExpression; VarModifyPlace: TVarModifyPlace);
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

  if (Decl.ItemType = itProperty) and Assigned(TIDProperty(Decl).Setter) then
    Exit;

  if Decl.ItemType <> itVar then
  case VarModifyPlace of
    vmpAssignment: ERRORS.VAR_EXPRESSION_REQUIRED(Expression);
    vmpPassArgument: ERRORS.ARG_VAR_REQUIRED(Expression);
  end;
  Flags := TIDVariable(Decl).Flags;
  if (VarConst in Flags) and (Expression.DataType <> Sys._UntypedReference) then
  begin
    ERRORS.CANNOT_MODIFY_CONSTANT(Expression);
  end;
  if VarLoopIndex in Flags then
    ERRORS.CANNOT_MODIFY_FOR_LOOP_VARIABLE(Expression);
end;

procedure TASTDelphiUnit.CheckVarParamConformity(Param: TIDVariable; Arg: TIDExpression);
begin
  if (Param.DataTypeID = dtOpenArray) and (Arg.DataType is TIDArray) then
    Exit;

  if (Param.DataType <> Sys._UntypedReference) and
     (Param.DataTypeID <> dtGeneric) and
     not ((Param.DataType.DataTypeID = dtPointer) and
          (Arg.DataType.DataTypeID = dtPointer)) then
  ERRORS.REF_PARAM_MUST_BE_IDENTICAL(Arg);
end;

class procedure TASTDelphiUnit.CheckAccessMember(SContext: PSContext; Decl: TIDDeclaration; const ID: TIdentifier);
begin
  case Decl.Visibility of
    vLocal, vPublic: Exit;
    vProtected: begin
      if Decl.Module = SContext.Proc.Module then
        Exit;
      if not SContext.Proc.Struct.IsInheritsForm(TStructScope(Decl.Scope).Struct) then
        SContext.ERRORS.CANNOT_ACCESS_PRIVATE_MEMBER(ID);
    end;
    vStrictProtected: begin
      if not SContext.Proc.Struct.IsInheritsForm(TStructScope(Decl.Scope).Struct) then
        SContext.ERRORS.CANNOT_ACCESS_PRIVATE_MEMBER(ID);
    end;
    vPrivate: begin
      if Decl.Module = SContext.Proc.Module then
        Exit;
      if TStructScope(Decl.Scope).Struct <> SContext.Proc.Struct then
        SContext.ERRORS.CANNOT_ACCESS_PRIVATE_MEMBER(ID);
    end;
    vStrictPrivate: begin
      if TStructScope(Decl.Scope).Struct <> SContext.Proc.Struct then
        SContext.ERRORS.CANNOT_ACCESS_PRIVATE_MEMBER(ID);
    end;
  end;
end;

procedure TASTDelphiUnit.CheckArrayExpression(Expression: TIDExpression);
begin
  if not (Expression.DataType is TIDArray) then
    ERRORS.ARRAY_EXPRESSION_REQUIRED(Expression);
end;

procedure TASTDelphiUnit.CheckBooleanExpression(Expression: TIDExpression);
begin
  if Expression.DataType <> Sys._Boolean then
    ERRORS.BOOLEAN_EXPRESSION_REQUIRED(Expression);
end;

{ TSContextHelper }

function TSContextHelper.GetErrors: TASTDelphiErrors;
begin
  Result := TASTDelphiUnit(Module).ERRORS;
end;

function TSContextHelper.GetSysUnit: TSYSTEMUnit;
begin
  Result := TASTDelphiUnit(Module).fSysUnit as TSYSTEMUnit;
end;

{ TDeclCache }

procedure TDeclCache.Add(Decl: TIDRangeType);
begin
  fRanges.Add(Decl);
end;

procedure TDeclCache.Add(Decl: TIDSet);
begin
  fSets.Add(Decl);
end;

constructor TDeclCache.Create;
begin
  fRanges := TList<TIDRangeType>.Create;
  fSets := TList<TIDSet>.Create;
end;

destructor TDeclCache.Destroy;
begin
  fRanges.Free;
  fSets.Free;
  inherited;
end;

function TDeclCache.FindRange(BaseType: TIDType; LoBound, HiBound: TIDExpression): TIDRangeType;
begin
  for var i := 0 to fRanges.Count - 1 do
  begin
    Result := fRanges[i];
    if (Result.BaseType = BaseType) and
       (Result.LoDecl.AsInt64 = LoBound.AsConst.AsInt64) and
       (Result.HiDecl.AsInt64 = HiBound.AsConst.AsInt64) then
     Exit;
  end;
  Result := nil;
end;

function TDeclCache.FindSet(BaseType: TIDType): TIDSet;
begin
  for var i := 0 to fSets.Count - 1 do
  begin
    Result := fSets[i];
    if Result.BaseType = BaseType then
     Exit;
  end;
  Result := nil;
end;

end.

