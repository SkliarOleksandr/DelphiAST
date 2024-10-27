unit AST.Delphi.Classes;

interface

{$I AST.Parser.Defines.inc}

uses System.SysUtils, System.Classes, System.StrUtils, System.Math, System.Generics.Collections,
     System.Variants,
     AST.Delphi.DataTypes,
     AST.Lexer,
     AST.Delphi.Operators,
     AST.Delphi.Declarations,
     AVL,
     AST.Parser.Utils,
     AST.Parser.Messages,
     AST.Parser.Options,
     AST.Pascal.Intf,
     AST.Classes,
     AST.Intf;
     // system
     // SysInit
     // System.Rtti
     // System.Types
     // System.TypInfo
     // GETMEM.INC
type

  TExpessionPosition = (ExprNested, ExprLValue, ExprRValue, ExprNestedGeneric, ExprType);

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
    PS_ABSTRACT,
    PS_OVERRIDE,
    PS_FINAL,
    PS_STATIC,
    PS_STDCALL,
    PS_FASTCALL,
    PS_CDECL,
    PS_SAFECALL,
    PS_REINTRODUCE
  );

  TIDList = class;
  TIDPairList = class;
  TIDDeclaration = class;
  TIDProcedure = class;
  TIDType = class;
  TIDWeekRef = class;
  TIDVariable = class;
  TIDField = class;
  TIDArray = class;
  TIDConstant = class;
  TIDGuidConstant = class;
  TIDStructure = class;
  TIDClass = class;
  TIDClassOf = class;
  TIDInterface = class;
  TIDProperty = class;
  TIDOperator = class;
  TIDExpression = class;
  TIDNameSpace = class;
  TIDGenericParam = class;
  TIDStringConstant = class;
  TIDProcType = class;
  TASTDelphiProc = class;
  TDlphHelper = class;
  TIDDynArrayConstant = class;

  TScope = class;
  TStructScope = class;
  TProcScope = class;
  TProcScopeClass = class of TProcScope;
  TParamsScope = class;

  TIDParam = TIDVariable;

  TIDItems = array of TObject;
  TIDExpressions = array of TIDExpression;
  TIDDeclArray = array of TIDDeclaration;

  TItemsStack = record
  strict private
    FItems: TIDItems;
    FCount: Integer;
    FCapasity: Integer;
  public
    constructor Create(DefaultPoolSize: Integer);
    procedure Push(const Item: TObject); inline;
    procedure SetCount(Value: Integer); inline;
    property Items: TIDItems read FItems;
    property Count: Integer read FCount write FCount;
  end;

  TScopeType = (
    stLocal,
    stGlobal,
    stStruct,
    stWithScope
  );

  TIDTypeClass = class of TIDType;

  TIDTypeArray = array of TIDType;
  TIDFieldArray = array of TIDField;

  TGenericInstance = record
    Args: TIDTypeArray;
    Instance: TIDDeclaration;
  end;
  PGenericInstance = ^TGenericInstance;

  TGenericInstanceList = array of TGenericInstance;

  PGenericDescriptor = ^TGenericDescriptor;
  TGenericDescriptor = record
  private
    FScope: TScope;
    FSearchName: string;
    FGenericInstances: TGenericInstanceList;
    FGenericParams: TIDTypeArray;
    function GetParamsCount: Integer; inline;
  public
    property Scope: TScope read FScope;
    property SearchName: string read FSearchName write FSearchName;
    property GenericInstances: TGenericInstanceList read FGenericInstances;
    property GenericParams: TIDTypeArray read FGenericParams write FGenericParams;
    property ParamsCount: Integer read GetParamsCount;
    procedure AddGenericInstance(Decl: TIDDeclaration; const Args: TIDTypeArray);
    class function Create(Scope: TScope): PGenericDescriptor; static;
    function SameParams(const AParams: TIDTypeArray): Boolean;
    function FindType(const ATypeName: string): TIDGenericParam; inline;
    function IndexOfType(const ATypeName: string): Integer; inline;
    function IsEqual(ADescriptor: PGenericDescriptor): Boolean;
    function TryGetInstance(const AArguments: TIDTypeArray; out ADecl: TIDDeclaration): Boolean;
    procedure Decl2Str(ABuilder: TStringBuilder);
  end;

  TIDGenericArg = record
    GParam: TIDGenericParam;
    GArg: TIDType;
  end;
  TIDGenericArguments = array of TIDGenericArg;

  TGenericInstantiateContext = record
    SrcDecl: TIDDeclaration;
    DstProc: TIDPRocedure;
    Params: TIDTypeArray;
    Args: TIDTypeArray;
    DstID: TIdentifier;
    function GetArgByParam(AParam: TIDGenericParam): TIDType;
    function Args2Str: string;
  end;

  TVisibility = (vLocal, vPublic, vProtected, vStrictProtected, vPrivate, vStrictPrivate);

  TIDDeclarationClass = class of TIDDeclaration;

  {base declaration class}
  TIDDeclaration = class(TASTDeclaration, IASTDelphiDeclaration)
  private
    FItemType: TIDItemType;
    FScope: TScope;                  // Обл. видимости где объявлена декларация
    FDataType: TIDType;              // Тип декларации (равен nil для процедур)
    FVisibility: TVisibility;        // Уровень видимости декларации
    FIndex: Integer;                 // Индекс элемента в TSpace
    FExportNameIndex: Integer;       // Показывает экспортируется ли декларация и с каким именем
    FNext: TIDDeclaration;           // Следующий добавленный в список элемент
    FRefCount: Integer;              // кол-во зависимостей
    FRCPath: UInt32;                 // номер прохода increfcount/decrefcount (необходим для алгоритма проставления зависимостей)
    FNoOverride: Boolean;            // запрещено ли переопределять
    FSystemDecl: Boolean;            // system declaration
    FImportLib: Integer;             // имя (индекс) билиотеки импорта
    FImportName: Integer;            // имя (индекс) функции импорта
    fDepricatedExpression: TIDExpression;
    function GetDataTypeID: TDataTypeID; inline;
    function GetUnitID: Integer;
    function GetIsAnonymous: Boolean; inline;
    function GetPackage: IASTPascalProject;
    procedure SetIndex(const Value: Integer);
    function GetDeclUnit: TASTModule; inline;
    function GetDeclUnitName: string; inline;
  protected
    function GetIsGeneric: Boolean; virtual;
    function GetOriginalDecl: TIDDeclaration; virtual;
    function GetIndex: Integer; virtual;
    function GetCValue: TIDConstant; virtual;
    function Get_Unit: IASTModule;
    procedure SetCValue(const Value: TIDConstant); virtual;
    procedure SetDataType(const Value: TIDType); virtual;
  public
    constructor CreateAsAnonymous(Scope: TScope); virtual;
    constructor CreateAsSystem(Scope: TScope; const Name: string); overload; virtual;
    constructor Create(Scope: TScope; const Identifier: TIdentifier); overload; virtual;
    destructor Destroy; override;
    ////////////////////////////////////////////////////////////////////////////
    property ItemType: TIDItemType read FItemType write FItemType;
    property DeclUnit: TASTModule read GetDeclUnit;
    property DeclUnitName: string read GetDeclUnitName;
    property Scope: TScope read FScope;
    property DataType: TIDType read FDataType write SetDataType;
    property Visibility: TVisibility read FVisibility write FVisibility;
    property Index: Integer read FIndex write SetIndex;
    property Export: Integer read FExportNameIndex write FExportNameIndex;
    property NextItem: TIDDeclaration read FNext write FNext;
    property RefCount: Integer read FRefCount;
    property DataTypeID: TDataTypeID read GetDataTypeID;
    property NoOverride: Boolean read FNoOverride write FNoOverride;
    property Original: TIDDeclaration read GetOriginalDecl;
    property ImportLib: Integer read FImportLib write FImportLib;
    property ImportName: Integer read FImportName write FImportName;
    property UnitID: Integer read GetUnitID;
    property IsAnonymous: Boolean read GetIsAnonymous;
    property IsGeneric: Boolean read GetIsGeneric;
    property IsSystem: Boolean read FSystemDecl;
    property SpaceIndex: Integer read GetIndex;
    procedure IncRefCount(RCPath: UInt32); virtual; // добавляет зависимость
    procedure DecRefCount(RCPath: UInt32); virtual; // удаляет зависимость
    {процедура делает DecReadCount для зависимостей}
    procedure RemoveILReferences(var RCPathCount: UInt32); virtual;
    property Package: IASTPascalProject read GetPackage;
    property CValue: TIDConstant read GetCValue write SetCValue;
    property DepricatedExpression: TIDExpression read fDepricatedExpression write fDepricatedExpression;

    function MakeCopy: TIDDeclaration; virtual;

    function InstantiateGeneric(ADstScope: TScope; ADstStruct: TIDStructure;
                                const AContext: TGenericInstantiateContext): TIDDeclaration; virtual;

    function DoesGenericUseParams(const AParams: TIDTypeArray): Boolean; virtual;
  end;

  {common ancestor for language entities that support generic parameterization}
  TIDDeclarationGeneric = class(TIDDeclaration)
  private
    fGenericDescriptor: PGenericDescriptor;
    fGenericOrigin: TIDDeclaration;
    fNextOverload: TIDDeclarationGeneric;
    procedure SetGenericDescriptor(const Value: PGenericDescriptor); virtual;
  protected
    procedure GenericInstances2Str(ABuilder: TStringBuilder; ANestedLevel: Integer);
    procedure GenericOverloads2Str(ABuilder: TStringBuilder; ANestedLevel: Integer);
  public
    property GenericDescriptor: PGenericDescriptor read FGenericDescriptor write SetGenericDescriptor;
    property GenericOrigin: TIDDeclaration read fGenericOrigin write fGenericOrigin;
    procedure AddGenecricOverload(ADecl: TIDDeclarationGeneric);
    property NextGenericOverload: TIDDeclarationGeneric read fNextOverload;
  end;


  TSpace<T: class{TIDDeclaration}> = record
  strict private
    FStartIndex: Integer;
    FCount: Integer;
    FFirst: T;
    FLast: T;
  public
    procedure Add(const Declaration: T); inline;
    procedure InsertFirst(const Declaration: T); inline;
    procedure Initialize; overload; inline;
    procedure Initialize(StartIndex: Integer); overload; inline;
    procedure Delete(const Declaration: T);
    {процедра делает реиндексацию всех элементов}
    procedure Reindex;
    property Count: Integer read FCount;
    property First: T read FFirst write FFirst;
    property Last: T read FLast write FLast;
  end;

  TVarSpace = TSpace<TIDVariable>;
  PVarSpace = ^TVarSpace;
  TProcSpace = TSpace<TIDProcedure>;
  PProcSpace = ^TProcSpace;
  TTypeSpace = TSpace<TIDType>;
  PTypeSpace = ^TTypeSpace;

  TTypeKind = (tkType, tkAlias, tkRefernce);

  {namespace}
  TIDNameSpace = class(TIDDeclaration)
  private
    fMembers: TScope;
  public
    constructor Create(Scope: TScope; const Identifier: TIdentifier); override;
    property Members: TScope read FMembers;
  end;

  {unit}
  TIDUnit = class(TIDNameSpace)
  private
    fModule: TASTModule;
  public
    constructor Create(Scope: TScope; AUnit: TASTModule);
    procedure Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer = 0; AAppendName: Boolean = True); override;
  end;

  TBinaryOperator = record
    Left: TIDType;
    Right: TIDType;
    OpDecl: TIDDeclaration;
  end;
  PBinaryOperator = ^TBinaryOperator;
  TBinaryOperatorsArray = array of TBinaryOperator;

  {base type class}
  TIDType = class(TIDDeclarationGeneric, IASTDelphiType)
  type
    TUnarOperators = array [opAssignment..opRound] of TIDDeclaration;
    TBinarOperators = array [opIn..High(TOperatorID)] of TBinaryOperatorsArray;
    TSysBinaryOperators = array [opIn..High(TOperatorID)] of TIDOperator;
    TExplistIDList = TAVLTree<TDataTypeId, TIDDeclaration>;
  private
    fElementary: Boolean;
    fIsPooled: Boolean;         // если это анонимный тип, то флаг показывает находится ли этот тип в пуле
    fDefaultReference: TIDType; // дефолтный анонимный указатель на этот тип
    fWeakType: TIDWeekRef;
    fDataTypeID: TDataTypeID;
    fTypeKind: TTypeKind;
    // lists of implicit operators
    fImplicitsTo: TIDPairList;      // self -> dest
    fImplicitsFrom: TIDPairList;    // src -> self
    fSysImplicitToAny: TIDOperator;    // self -> any
    fSysImplicitFromAny: TIDOperator;  // any -> self

    // lists of explicits operators (user level):
    fExplicitsTo: TIDPairList;
    fExplicitsFrom: TIDPairList;
    // lists of system explicits operators:
    fSysExplicitToAny: TIDOperator;
    fSysExplicitFromAny: TIDOperator;

    fUnarOperators: TUnarOperators;
    fBinarOperators: TBinarOperators;
    fBinarOperatorsFor: TBinarOperators;
    fSysBinaryOperators: TSysBinaryOperators;

    fGenericNextOverload: TIDType;
    /////////////////////////////////////////////////////////
    fPacked: Boolean;
    fNeedForward: Boolean;
    fForwardID: TIdentifier;
    fInitProc: TIDProcedure;
    fCopyProc: TIDProcedure;
    fFinalProc: TIDProcedure;
    fHelper: TDlphHelper;
    function GetOperators(const Op: TOperatorID): TBinaryOperatorsArray;
    function GetOperatorsFor(const Op: TOperatorID): TBinaryOperatorsArray;
    function GetIsReferenced: Boolean; inline;
    function GetIsInteger: Boolean; inline;
    function GetIsFloat: Boolean; inline;
  protected
    function GetDataSize: Integer; virtual;
    function GetOrdinal: Boolean; virtual;
    function GetDisplayName: string; overload; override;
    function GetIsManaged: Boolean; virtual;
    function GetActualDataType: TIDType; virtual;
    function GetParent: TIDType;
    function GetIsGeneric: Boolean; override;
  public
    constructor CreateAsAnonymous(Scope: TScope); override;
    constructor CreateAsBuiltin(AScope: TScope; const AName: string; ADataTypeID: TDataTypeID);
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    constructor Create(Scope: TScope; const ID: TIdentifier); override;
    destructor Destroy; override;
    ////////////////////////////////////////////////////////////////////////////
    function GetDefaultReference(Scope: TScope): TIDType;
    property DefaultReference: TIDType read fDefaultReference write fDefaultReference;
    property WeakRefType: TIDWeekRef read FWeakType write FWeakType;
    property DataTypeID: TDataTypeID read FDataTypeID write FDataTypeID;
    property Elementary: Boolean read FElementary write FElementary;
    property BinarOperators[const Op: TOperatorID]: TBinaryOperatorsArray read GetOperators;
    property BinarOperatorsFor[const Op: TOperatorID]: TBinaryOperatorsArray read GetOperatorsFor;
    property IsOrdinal: Boolean read GetOrdinal;
    property IsInteger: Boolean read GetIsInteger;
    property IsFloat: Boolean read GetIsFloat;
    property IsPacked: Boolean read FPacked write FPacked;
    property IsReferenced: Boolean read GetIsReferenced;
    property IsGeneric: Boolean read GetIsGeneric;
    // функция возвращает точный implicit оператор, если определен
    function GetImplicitOperatorTo(const Destination: TIDType): TIDDeclaration;
    function GetImplicitOperatorFrom(const Source: TIDType): TIDDeclaration;

    // функция подбирает максимально подходящий implicit оператор, если определен
    function FindImplicitOperatorTo(const Destination: TIDType): TIDDeclaration;
    function FindImplicitOperatorFrom(const Source: TIDType): TIDDeclaration;

    function GetExplicitOperatorTo(const Destination: TIDType): TIDDeclaration;
    function GetExplicitOperatorFrom(const Source: TIDType): TIDDeclaration;

    function UnarOperator(Op: TOperatorID; Right: TIDType): TIDType; overload; inline;
    function BinarOperator(Op: TOperatorID; Right: TIDType): TIDType; virtual;
    function BinarOperatorFor(const Op: TOperatorID; const Left: TIDType): TIDType;
    property ActualDataType: TIDType read GetActualDataType;
    property TypeKind: TTypeKind read FTypeKind write FTypeKind;
    property DataSize: Integer read GetDataSize;
    property Managed: Boolean read GetIsManaged;
    property IsPooled: Boolean read FIsPooled write FIsPooled;
    property NeedForward: Boolean read FNeedForward write FNeedForward;
    ///<summary> ForwardID points to the target type that is not already defined (in the same scope) </summary>
    property ForwardID: TIdentifier read FForwardID write FForwardID;
    property Parent: TIDType read GetParent;

    {переопределенным оператором может быть как функция так и тип, для простейших операций}
    procedure OverloadImplicitTo(const Destination: TIDDeclaration); overload;
    procedure OverloadImplicitTo(const Destination, Proc: TIDDeclaration); overload;
    procedure OverloadSysImplicitsTo(const ADestTypes: array of TDataTypeID); overload;

    procedure OverloadImplicitFrom(const Source: TIDDeclaration); overload;
    procedure OverloadImplicitFrom(const Source, Proc: TIDDeclaration); overload;

    procedure OverloadImplicitToAny(const Op: TIDOperator);
    procedure OverloadImplicitFromAny(const Op: TIDOperator);

    procedure OverloadExplicitTo(const Destination: TIDDeclaration); overload;
    procedure OverloadExplicitTo(const Destination, Proc: TIDDeclaration); overload;
    procedure OverloadExplicitFrom(const Source: TIDDeclaration); overload;
    procedure OverloadExplicitFrom(const Source, Proc: TIDDeclaration); overload;

    procedure OverloadExplicitToAny(const Op: TIDOperator);
    procedure OverloadExplicitFromAny(const Op: TIDOperator);

    procedure OverloadUnarOperator(Op: TOperatorID; Destination: TIDDeclaration); overload;
    procedure OverloadUnarOperator(Op: TOperatorID; Declaration: TIDOperator); overload; inline;
    procedure OverloadBinarOperator2(Op: TOperatorID; Right: TIDType; Result: TIDDeclaration); overload;
    procedure OverloadBinarOperatorFor(Op: TOperatorID; const Left: TIDType; const Result: TIDDeclaration);
    procedure OverloadBinarOperator(Op: TOperatorID; Declaration: TIDOperator); overload; inline;
    procedure OverloadCmpOperator(AOpID: TOperatorID; ARight: TIDType);
    procedure OverloadAllCmpOperators;

    procedure AddBinarySysOperator(Op: TOperatorID; Decl: TIDOperator);

    procedure CreateStandardOperators; virtual;

    property GenericNextOverload: TIDType read FGenericNextOverload write FGenericNextOverload;
    property InitProc: TIDProcedure read FInitProc write FInitProc;
    property CopyProc: TIDProcedure read FCopyProc write FCopyProc;
    property FinalProc: TIDProcedure read FFinalProc write FFinalProc;

    property SysExplicitToAny: TIDOperator read fSysExplicitToAny;
    property SysExplicitFromAny: TIDOperator read fSysExplicitFromAny;
    property SysImplicitToAny: TIDOperator read FSysImplicitToAny;
    property SysImplicitFromAny: TIDOperator read fSysImplicitFromAny;

    property SysBinayOperator: TSysBinaryOperators read fSysBinaryOperators;
    property Helper: TDlphHelper read fHelper write fHelper;
    function MakeCopy: TIDDeclaration; override;
    procedure Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer = 0; AAppendName: Boolean = True); override;
    function InstantiateGeneric(ADstScope: TScope; ADstStruct: TIDStructure;
                                const AContext: TGenericInstantiateContext): TIDDeclaration; override;

    function MatchImplicitTo(ADst: TIDType): Boolean; virtual;
    function MatchImplicitFrom(ASrc: TIDType): Boolean; virtual;

    function MatchExplicitTo(ADst: TIDType): Boolean; virtual;
    function MatchExplicitFrom(ASrc: TIDType): Boolean; virtual;
  end;

  {special system internal type to describe several possible types in one}
  TIDSysVariativeType = class(TIDType)
  private
    FTypes: TIDTypeArray;
  protected
    function GetDisplayName: string; override;
  public
    constructor CreateAsSystem(Scope: TScope; const Types: TIDTypeArray); reintroduce;
    procedure CreateStandardOperators; override;
    property Types: TIDTypeArray read FTypes;
  end;

  TGenericConstraint = (
    gsNone,                  // <T>
    gsClass,                 // <T: class>
    gsConstructor,           // <T: constructor>
    gsClassAndConstructor,   // <T: class constructor>
    gsRecord,                // <T: record>
    gsType                   // <T: IMyIntf, K: TMyClass>
  );

  {special type for describing the generic parameter}
  TIDGenericParam = class(TIDType)
  private
    fConstraint: TGenericConstraint;
    fConstraintType: TIDType;
  public
    constructor Create(Scope: TScope; const ID: TIdentifier); override;
    property Constraint: TGenericConstraint read fConstraint write fConstraint;
    property ConstraintType: TIDType read fConstraintType write fConstraintType;
    function InstantiateGeneric(ADstScope: TScope; ADstStruct: TIDStructure;
                                const AContext: TGenericInstantiateContext): TIDDeclaration; override;

    function SameConstraint(ADstParam: TIDGenericParam): Boolean;
    procedure Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer = 0; AAppendName: Boolean = True); override;
  end;

  {alias type}
  TIDAliasType = class(TIDType)
  private
    fOriginalType: TIDType; // actual type (can not be an alias)
    fLinkedType: TIDType;   // referenced type (may be an alias)
    FNewType: Boolean;   // determine a new type
  protected
    function GetActualDataType: TIDType; override;
    function GetOrdinal: Boolean; override;
    function GetIndex: Integer; override;
    function GetOriginalDecl: TIDDeclaration; override;
    function GetDisplayName: string; override;
  public
    constructor CreateAlias(Scope: TScope;
                            const ID: TIdentifier;
                            OriginalType: TIDType;
                            ANewType: Boolean = False);
    constructor CreateAliasAsSystem(Scope: TScope; const ID: string; SrcType: TIDType);
    procedure IncRefCount(RCPath: UInt32); override;
    procedure DecRefCount(RCPath: UInt32); override;
    property LinkedType: TIDType read FLinkedType;
    property Original: TIDType read fOriginalType;
    property NewType: Boolean read FNewType;
    function InstantiateGeneric(ADstScope: TScope; ADstStruct: TIDStructure;
                                const AContext: TGenericInstantiateContext): TIDDeclaration; override;
    procedure Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer = 0; AAppendName: Boolean = True); override;
  end;

  {special type for describing the generic type with generic arguments}
  TIDGenericInstantiation = class(TIDAliasType)
  private
    FGenericArguments: TIDExpressions;
  protected
    function GetIsGeneric: Boolean; override;
    function GetDisplayName: string; override;
  public
    constructor CreateInstantiation(AScope: TScope;
                                    AGenericType: TIDType;
                                    const AGenericArguments: TIDExpressions);

    function InstantiateGeneric(ADstScope: TScope; ADstStruct: TIDStructure;
                                const AContext: TGenericInstantiateContext): TIDDeclaration; override;

    function SameType(AType: TIDGenericInstantiation): Boolean;
  end;

  {base referenced type class}
  TIDRefType = class(TIDType)
  private
    fReferenceType: TIDType;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Scope: TScope; const ID: TIdentifier); override;
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    constructor CreateAsAnonymous(Scope: TScope; ReferenceType: TIDType); reintroduce; virtual;
    procedure CreateStandardOperators; override;
    property ReferenceType: TIDType read fReferenceType write FReferenceType;
    procedure IncRefCount(RCPath: UInt32); override;
    procedure DecRefCount(RCPath: UInt32); override;
  end;

  {pointer type}
  TIDPointer = class(TIDRefType)
  private
    FPointerMath: Boolean;
    FForwardDeclaration: Boolean;
  protected
    function GetIsGeneric: Boolean; override;
  public
    constructor Create(Scope: TScope; const ID: TIdentifier); override;
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    constructor CreateAsAnonymous(Scope: TScope; ReferenceType: TIDType); override;
    procedure CreateStandardOperators; override;
    property PointerMath: Boolean read FPointerMath write FPointerMath;
    property ForwardDeclaration: Boolean read FForwardDeclaration write FForwardDeclaration;
    procedure Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer = 0; AAppendName: Boolean = True); override;
    function InstantiateGeneric(ADstScope: TScope; ADstStruct: TIDStructure;
                                const AContext: TGenericInstantiateContext): TIDDeclaration; override;
  end;

  {untyped referenced type}
  TIDUntypedRef = class(TIDRefType)
  public
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    procedure Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer = 0; AAppendName: Boolean = True); override;
  end;

  {class of type}
  TIDClassOf = class(TIDRefType)
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Scope: TScope; const Identifier: TIdentifier); override;
    constructor CreateAsAnonymous(Scope: TScope; ReferenceType: TIDType); override;
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    procedure CreateStandardOperators; override;
    procedure Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer = 0; AAppendName: Boolean = True); override;
  end;

  TIDWeekRef = class(TIDPointer)
  protected
    function GetDisplayName: string; override;
  public
    constructor CreateAsAnonymous(Scope: TScope; ReferenceType: TIDType); override;
  end;

  {special nullptr type}
  TIDNullPointerType = class(TIDPointer)
  public
    procedure CreateStandardOperators; override;
  end;

  {base ordinal type class}
  TIDOrdinal = class(TIDType)
  private
    FSignedBound: Boolean;
    FLBound: Int64;
    FHBound: Int64;
    function GetElementsCount: UInt64; inline;
    function GetHighBoundUInt64: UInt64;
  protected
    function GetOrdinal: Boolean; override;
  public
    property LowBound: Int64 read FLBound write FLBound;
    property HighBound: Int64 read FHBound write FHBound;
    property HighBoundUInt64: UInt64 read GetHighBoundUInt64;
    // показывает знаковый ли диаппазон (Int64) или беззнаковый (UInt64)
    property SignedBound: Boolean read FSignedBound write FSignedBound;
    property ElementsCount: UInt64 read GetElementsCount;

    procedure Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer = 0; AAppendName: Boolean = True); override;

    function MatchExplicitFrom(ASrc: TIDType): Boolean; override;
  end;

  {alias (for generics parameters)}
  TIDAlias = class(TIDDeclaration)
  private
    FOriginalDecl: TIDDeclaration;
  protected
    function GetOriginalDecl: TIDDeclaration; override;
  public
    constructor CreateAlias(Scope: TScope; const ID: TIdentifier; Decl: TIDDeclaration);
    property Original: TIDDeclaration read FOriginalDecl;
  end;

  TWithAlias = class(TIDAlias)
  private
    fExpression: TIDExpression;
  public
    constructor CreateAlias(Scope: TScope; OriginalDecl: TIDDeclaration; Expression: TIDExpression);
    property Expression: TIDExpression read fExpression;
  end;

  {range type}
  TIDRangeType = class(TIDOrdinal)
  private
    fBaseType: TIDOrdinal;
    fLoDecl: TIDConstant;
    fHiDecl: TIDConstant;
    procedure SetHiDecl(const Value: TIDConstant);
    procedure SetLoDecl(const Value: TIDConstant);
  protected
    function GetDisplayName: string; override;
    procedure CreateStandardOperators; override;
  public
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    constructor CreateAsAnonymous(Scope: TScope); override;
    constructor Create(Scope: TScope; const Identifier: TIdentifier); override;
    property BaseType: TIDOrdinal read fBaseType write fBaseType;
    property LoDecl: TIDConstant read fLoDecl write SetLoDecl;
    property HiDecl: TIDConstant read fHiDecl write SetHiDecl;
  end;

  {enum type}
  TIDEnum = class(TIDOrdinal)
  private
    FItems: TScope;
  protected
    function GetDisplayName: string; override;
    function GetDataSize: Integer; override;
  public
    constructor Create(Scope: TScope; const ID: TIdentifier); override;
    constructor CreateAsAnonymous(Scope: TScope); override;
    procedure CreateStandardOperators; override;
    property Items: TScope read FItems write FItems;
    procedure Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer = 0; AAppendName: Boolean = True); override;
  end;

  TStructFlags = set of (StructCompleted);

  {base structure type}
  TIDStructure = class(TIDType)
  private
    fAncestor: TIDStructure;        // origianl type of fAncestorDecl
    fAncestorDecl: TIDType;         // can be TIDGenericInstantiation or TIDStructure successor
    fMembers: TStructScope;         // instance members
    fOperators: TStructScope;       // operators members
    fStrucFlags: TStructFlags;
    fDefaultProperty: TIDProperty;
    fClassOfType: TIDClassOf;
    fClassConstructor: TIDProcedure;
    FClassDestructor: TIDProcedure;
    function GetHasInitFiels: Boolean;
    function GetClassOfType: TIDClassOf;
    function GetFieldsCount: Integer; inline;
    function GetMethodCount: Integer; inline;
    procedure SetAncestorDecl(const Value: TIDType);
  protected
    function GetDataSize: Integer; override;
    function GetDisplayName: string; override;
    function GetIsManaged: Boolean; override;
    function GetExtraFlags: Byte; virtual;
    function GetIsGeneric: Boolean; override;
    function GetStructKeyword: string; virtual;
    procedure SetGenericDescriptor(const Value: PGenericDescriptor); override;
    procedure DoCreateStructure;
  public
    constructor Create(Scope: TScope; const Name: TIdentifier); override;
    constructor CreateAsAnonymous(Scope: TScope); override;
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    ////////////////////////////////////////////////////////////////////////////
    property Members: TStructScope read FMembers;
    property Operators: TStructScope read fOperators;
    property HasInitFiels: Boolean read GetHasInitFiels;
    property StructFlags: TStructFlags read FStrucFlags write FStrucFlags;
    property Ancestor: TIDStructure read FAncestor;
    property AncestorDecl: TIDType read fAncestorDecl write SetAncestorDecl;
    property MethodCount: Integer read GetMethodCount;
    property FieldsCount: Integer read GetFieldsCount;
    property DefaultProperty: TIDProperty read FDefaultProperty write FDefaultProperty;
    property ClassOfType: TIDClassOf read GetClassOfType;
    function IsInheritsForm(Ancestor: TIDStructure): Boolean;
    function FindVirtualProc(AProc: TIDProcedure): TIDProcedure;
    function FindVirtualProcInAncestor(Proc: TIDProcedure): TIDProcedure;

    function AddField(const Name: string; DataType: TIDType): TIDField;
    function FindField(const Name: string): TIDField;
    function FindMethod(const Name: string): TIDProcedure;
    function FindProperty(const Name: string): TIDProperty;
    function GetDefaultProperty: TIDProperty;

    procedure IncRefCount(RCPath: UInt32); override;
    procedure DecRefCount(RCPath: UInt32); override;

    property ClassConstructor: TIDProcedure read fClassConstructor write fClassConstructor;
    property ClassDestructor: TIDProcedure read FClassDestructor write FClassDestructor;

    function GetEnumeratorSupported(out ACurrentProp: TIDProperty): Boolean;

    procedure InstantiateGenericAncestors(ADstScope: TScope; ADstStruct: TIDStructure;
                                          const AContext: TGenericInstantiateContext); virtual;

    function InstantiateGeneric(ADstScope: TScope; ADstStruct: TIDStructure;
                                const AContext: TGenericInstantiateContext): TIDDeclaration; override;

    procedure AncestorsDecl2Str(ABuilder: TStringBuilder); virtual;
    procedure Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer = 0; AAppendName: Boolean = True); override;

    function DoesGenericUseParams(const AParams: TIDTypeArray): Boolean; override;
  end;

  TIDStructureClass = class of TIDStructure;

  {record type}
  TIDRecord = class(TIDStructure)
  private
    fStaticConstructor: TIDProcedure;
    fStaticDestructor: TIDProcedure;
    fCases: array of TIDFieldArray;
  protected
    function GetDataSize: Integer; override;
    function GetStructKeyword: string; override;
  public
    constructor Create(Scope: TScope; const Name: TIdentifier); override;
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    procedure CreateStandardOperators; override;
    property StaticConstructor: TIDProcedure read FStaticConstructor write FStaticConstructor;
    property StaticDestructor: TIDProcedure read FStaticDestructor write FStaticDestructor;
    procedure AddCase(const AFields: TIDFieldArray);
  end;

  TIDMethods = array of TIDProcedure;

  TClassDeclFlag = (
   cdfAbstract,
   cdfSealed
  );

  TClassDeclFlags = set of TClassDeclFlag;

  {class}
  TIDClass = class(TIDStructure)
  private
    FInterfaces: TList<TIDInterface>;
    FIntfGenericInstantiations: TArray<TIDGenericInstantiation>;
    FInterfacesMethods: TList<TIDMethods>;
    FClassDeclFlags: TClassDeclFlags;
    function GetInterfacesCount: Integer;
    function GetInterface(Index: Integer): TIDInterface;
    function GetIsAbstract: Boolean; inline;
    function GetIsSealed: Boolean; inline;
  protected
    function GetDataSize: Integer; override;
    function GetIsManaged: Boolean; override;
    function GetExtraFlags: Byte; override;
    function GetStructKeyword: string; override;
  public
    constructor Create(Scope: TScope; const Name: TIdentifier); override;
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    procedure CreateStandardOperators; override;
    destructor Destroy; override;
    //========================================================
    function FindInterface(const AIntf: TIDInterface; AFindInAncestors: Boolean = False): Boolean; overload;
    function FindInterface(const AIntfName: string;
                           const AGenericParams: TIDTypeArray = []): TIDInterface; overload;
    procedure AddInterface(const Intf: TIDInterface);
    procedure AddGenericInterface(AGenricIntf: TIDGenericInstantiation);
    procedure MapInterfaceMethod(const Intf: TIDInterface; IntfMethod, ImplMethod: TIDProcedure);
    property InterfacesCount: Integer read GetInterfacesCount;
    property Interfaces[Index: Integer]: TIDInterface read GetInterface;
    procedure IncRefCount(RCPath: UInt32); override;
    procedure DecRefCount(RCPath: UInt32); override;
    procedure AncestorsDecl2Str(ABuilder: TStringBuilder); override;
    procedure InstantiateGenericAncestors(ADstScope: TScope; ADstStruct: TIDStructure;
                                          const AContext: TGenericInstantiateContext); override;
    property ClassDeclFlags: TClassDeclFlags read FClassDeclFlags write FClassDeclFlags;
    property IsAbstract: Boolean read GetIsAbstract;
    property IsSealed: Boolean read GetIsSealed;
  end;

  {helper type}
  TDlphHelper = class(TIDStructure)
  private
    fTarget: TIDType;
    procedure SetTarget(const Value: TIDType);
  protected
    function GetStructKeyword: string; override;
  public
    property Target: TIDType read fTarget write SetTarget;
    procedure AncestorsDecl2Str(ABuilder: TStringBuilder); override;
  end;

  {closure}
  TIDClosure = class(TIDClass)
  public type
    TCapturedVarRec = record
      CapturedVar: TIDVariable;
      Field: TIDField;
      ByRef: Boolean;
    end;
    TCapturedVars = TAVLTree<TIDVariable, TCapturedVarRec>;
  private
    FDeclProc: TIDProcedure;
    FCapturedVars: TCapturedVars;
  protected
    function GetDisplayName: string; override;
  public
    constructor CreateClosure(const DeclProc, RunProc: TIDProcedure);
    destructor Destroy; override;
    {функция GetCapturedVar возвращает поле замыкания которое соответствует захватываемой переменной}
    function GetCapturedVar(const CapturedVar: TIDVariable): TIDField;
    function CaptureByReference: Boolean;
    property CapturedVars: TCapturedVars read FCapturedVars;

  end;

  {interface}
  TIDInterface = class(TIDStructure)
  private
    FIsDisp: Boolean;
    FGUIDDecl: TIDGuidConstant;
  protected
    function GetIsManaged: Boolean; override;
    function GetStructKeyword: string; override;
  public
    constructor Create(Scope: TScope; const Name: TIdentifier); override;
    property GUIDDecl: TIDGuidConstant read FGUIDDecl write FGUIDDecl;
    property IsDisp: Boolean read FIsDisp write FIsDisp;
    function MatchImplicitTo(ADst: TIDType): Boolean; override;
    function MatchImplicitFrom(ASrc: TIDType): Boolean; override;
  end;

  {base array type}
  TIDArray = class(TIDType)
  public
  type
    TDimensions = array of TIDOrdinal;
  private
    FElementDataType: TIDType;  // тип элемента массива
    FDimensionsCount: Integer;  // кол-во измерений
    FDimensions: TDimensions;   // измерения
    function GetDimension(Index: Integer): TIDOrdinal; virtual;
    function GetAllDimensionsCount: Integer;
    function GetElementTypeByIndex(Index: Integer): TIDType;
  protected
    function GetDisplayName: string; override;
    function GetDataSize: Integer; override;
    function GetIsGeneric: Boolean; override;
  public
    constructor Create(Scope: TScope; const Name: TIdentifier); override;
    constructor CreateAsAnonymous(Scope: TScope); overload; override;
    constructor CreateAnonymousStatic1Dim(Scope: TScope; ElementDataType: TIDType; Length: Integer; out BoundType: TIDOrdinal); overload;
    procedure CreateStandardOperators; override;
    ////////////////////////////////////////////////////////////////////////////
    property ElementDataType: TIDType read FElementDataType write FElementDataType;
    // this array dimensions only
    property DimensionsCount: Integer read FDimensionsCount;
    // this array and all nested arrays dimensions (since they can be addresed in the same way)
    property AllDimensionsCount: Integer read GetAllDimensionsCount;
    property Dimensions[Index: Integer]: TIDOrdinal read GetDimension;
    property ElementTypeByIndex[Index: Integer]: TIDType read GetElementTypeByIndex;

    procedure AddBound(Bound: TIDOrdinal);
    procedure IncRefCount(RCPath: UInt32); override;
    procedure DecRefCount(RCPath: UInt32); override;
    function IsOpenArrayOfConst: Boolean;
    function InstantiateGeneric(ADstScope: TScope; ADstStruct: TIDStructure;
                                const AContext: TGenericInstantiateContext): TIDDeclaration; override;

    function DoesGenericUseParams(const AParams: TIDTypeArray): Boolean; override;
  end;

  {set}
  TIDSet = class(TIDType)
  private
    fBaseType: TIDOrdinal;
    function GetBitsCount: UInt32; inline;
  protected
    function GetDisplayName: string; override;
    function GetDataSize: Integer; override;
  public
    constructor CreateAsAnonymous(Scope: TScope; BaseType: TIDOrdinal); reintroduce;
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    constructor Create(Scope: TScope; const ID: TIdentifier); override;
    procedure CreateStandardOperators; override;
    ////////////////////////////////////////////////////////////////////////////
    property BitsCount: UInt32 read GetBitsCount;
    property BaseType: TIDOrdinal read fBaseType write FBaseType;
    procedure IncRefCount(RCPath: UInt32); override;
    procedure Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer = 0; AAppendName: Boolean = True); override;

    function MatchImplicitFrom(ASrc: TIDType): Boolean; override;
    function MatchExplicitFrom(ASrc: TIDType): Boolean; override;
  end;

  {static array}
  TIDStaticArray = class(TIDArray)
  public
    constructor CreateAnonymous(Scope: TScope; BaseType: TIDType); reintroduce;
    constructor Create(Scope: TScope; const ID: TIdentifier); override;
    procedure CreateStandardOperators; override;
    procedure Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer = 0; AAppendName: Boolean = True); override;
  end;


  {dynamic array}
  TIDDynArray = class(TIDArray)
  private
    FMembersScope: TScope;
  protected
  public
    constructor Create(Scope: TScope; const Name: TIdentifier); override;
    constructor CreateAsAnonymous(Scope: TScope); override;
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    procedure CreateStandardOperators; override;
    function GetHelperScope: TScope;
    procedure Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer = 0; AAppendName: Boolean = True); override;
  end;

  {ancestor of all string types}
  TIDString = class(TIDDynArray)
  public
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    procedure Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer = 0; AAppendName: Boolean = True); override;
  end;

  {ancestor of all integer types}
  TIDInt = class(TIDOrdinal)

  end;

  {ancestor of all floating point types}
  TIDFloat = class(TIDType)
    procedure Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer = 0; AAppendName: Boolean = True); override;
  end;

  {variant}
  TIDVariant = class(TIDType)
    procedure Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer = 0; AAppendName: Boolean = True); override;
  end;

  {open array}
  TIDOpenArray = class(TIDDynArray)
  strict private
    {$HINTS OFF}
    constructor Create(Scope: TScope; const Name: TIdentifier); override; deprecated 'Only CreateAnonymous constructor allowed for this type';
    {$HINTS ON}
  protected
  public
    constructor CreateAsAnonymous(Scope: TScope); override;
    ////////////////////////////////////////////////////////////////////////////
  end;

  TIDParamArray = array of TIDParam;


  TProcType = (
    ptFunc,               // глобальная или локальная функция
    ptClassFunc,          // классовая функция
    ptStaticFunc,         // статичная функция
    ptProc,               // глобальная или локальная процедура
    ptClassProc,          // классовая процедура
    ptStaticProc,         // статичная процедура
    ptConstructor,        // конструктор
    ptDestructor,         // деструктор
    ptClassConstructor,   // классовый конструктор
    ptClassDestructor,    // классовый деструктор
    ptOperator
  );

  TProcTypeClass = (
    procStatic,           // simple static proc type, TProc = procedure(...)
    procMethod,           // method proc type, TProc = procedure(...) of object
    procReference         // referenced proc type, TProc = reference to procedure(...)
  );


  TCallConvention = (
    ConvNative,
    ConvRegister,
    ConvStdCall,
    ConvCDecl,
    ConvFastCall,
    ConvSafeCall
  );

  {procedural type}
  TIDProcType = class(TIDType)
  private
    fParams: TIDParamArray;
    fResultType: TIDType;
    fCallConv: TCallConvention;
    fProcClass: TProcTypeClass;
    function GetIsStatic: Boolean; inline;
  protected
    function GetDisplayName: string; override;
    function GetDataSize: Integer; override;
  public
    constructor Create(Scope: TScope; const Identifier: TIdentifier); override;
    constructor CreateAsAnonymous(Scope: TScope); override;
    ////////////////////////////////////////////////////////////////////////
    procedure AddParam(const ID: TIdentifier; DataType: TIDType);
    procedure CreateStandardOperators; override;
    property Params: TIDParamArray read fParams write fParams;
    property ResultType: TIDType read fResultType write fResultType;
    property IsStatic: Boolean read GetIsStatic;
    property CallConv: TCallConvention read FCallConv write FCallConv;
    property ProcClass: TProcTypeClass read fProcClass write fProcClass;
    procedure Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer = 0; AAppendName: Boolean = True); override;
    function InstantiateGeneric(ADstScope: TScope; ADstStruct: TIDStructure;
                                const AContext: TGenericInstantiateContext): TIDDeclaration; override;
  end;

  {base constant class}
  TIDConstant = class(TIDDeclaration)
  private
    FExplicitDataType: TIDType;
    procedure SetExplicitDataType(const Value: TIDType);
  protected
    function GetDisplayName: string; override;
    function GetCValue: TIDConstant; override;
    procedure SetCValue(const Value: TIDConstant); override;
  public
    constructor Create(Scope: TScope; const Identifier: TIdentifier); override;
    procedure AssignValue(Source: TIDConstant); virtual; abstract;
    property ExplicitDataType: TIDType read FExplicitDataType write SetExplicitDataType;
    function ValueDataType: TDataTypeID; virtual; abstract;
    function ValueByteSize: Integer; virtual; abstract;
    function FixedByteSize: Integer;
    function AsInt64: Int64; virtual; abstract;
    function AsUInt64: UInt64; inline;
    function AsString: string; virtual; abstract;
    function AsVariant: Variant; virtual; abstract;
    function CompareTo(Constant: TIDConstant): Integer; virtual; abstract;
    procedure IncRefCount(RCPath: UInt32); override;
    procedure DecRefCount(RCPath: UInt32); override;
    procedure Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer = 0; AAppendName: Boolean = True); override;
  end;

  TIDConstantClass = class of TIDConstant;

  {base generic constant class}
  TIDXXXConstant<T> = class(TIDConstant)
  private
    FValue: T;
  protected
    procedure SetAsVariant(const AValue: Variant); virtual; abstract;
  public
    constructor Create(Scope: TScope; const Identifier: TIdentifier; DataType: TIDType; Value: T); overload;
    constructor CreateAsSystem(AScope: TScope; const AName: string; ADataType: TIDType); reintroduce;
    constructor CreateAsAnonymous(Scope: TScope; DataType: TIDType; Value: T); reintroduce;
    constructor CreateWithoutScope(DataType: TIDType; Value: T);
    procedure AssignValue(Source: TIDConstant); override;
    property Value: T read FValue write FValue;
  end;

  {int constant}
  TIDIntConstant = class(TIDXXXConstant<Int64>)
  protected
    procedure SetAsVariant(const AValue: Variant); override;
  public
    function ValueDataType: TDataTypeID; override;
    function ValueByteSize: Integer; override;
    function AsInt64: Int64; override;
    function AsString: string; override;
    function AsVariant: Variant; override;
    function CompareTo(Constant: TIDConstant): Integer; override;
  end;

  {float constant}
  TIDFloatConstant = class(TIDXXXConstant<Extended>)
  protected
    procedure SetAsVariant(const AValue: Variant); override;
  public
    function ValueDataType: TDataTypeID; override;
    function ValueByteSize: Integer; override;
    function AsInt64: Int64; override;
    function AsString: string; override;
    function AsVariant: Variant; override;
    function CompareTo(Constant: TIDConstant): Integer; override;
  end;

  {string constant}
  TIDStringConstant = class(TIDXXXConstant<string>)
  protected
    procedure SetAsVariant(const AValue: Variant); override;
  public
    function ValueByteSize: Integer; override;
    function ValueDataType: TDataTypeID; override;
    function AsInt64: Int64; override;
    function AsString: string; override;
    function AsVariant: Variant; override;
    function StrLength: Integer;
    function CompareTo(Constant: TIDConstant): Integer; override;
  end;

  {char constant}
  TIDCharConstant = class(TIDXXXConstant<char>)
  protected
    procedure SetAsVariant(const AValue: Variant); override;
  public
    function ValueDataType: TDataTypeID; override;
    function ValueByteSize: Integer; override;
    function AsInt64: Int64; override;
    function AsString: string; override;
    function AsVariant: Variant; override;
    function CompareTo(Constant: TIDConstant): Integer; override;
  end;

  {boolean constant}
  TIDBooleanConstant = class(TIDXXXConstant<Boolean>)
  protected
    procedure SetAsVariant(const AValue: Variant); override;
  public
    function ValueDataType: TDataTypeID; override;
    function ValueByteSize: Integer; override;
    function AsInt64: Int64; override;
    function AsString: string; override;
    function AsVariant: Variant; override;
    function CompareTo(Constant: TIDConstant): Integer; override;
  end;

  {pointer constant}
  TIDPointerConstant = class(TIDXXXConstant<TIDDeclaration>)
  protected
    procedure SetAsVariant(const AValue: Variant); override;
  public
    function ValueDataType: TDataTypeID; override;
    function ValueByteSize: Integer; override;
    function AsString: string; override;
    function AsInt64: Int64; override;
    function AsVariant: Variant; override;
    function CompareTo(Constant: TIDConstant): Integer; override;
    procedure AssignValue(Source: TIDConstant); override;
  end;

  {array constant}
  TIDDynArrayConstant = class(TIDXXXConstant<TIDExpressions>)
  private
    FStatic: Boolean;
    function GetLength: Integer; inline;
    function GetElementType: TIDType; inline;
    function CheckAsSet: Boolean; inline;
  protected
    procedure SetAsVariant(const AValue: Variant); override;
  public
    function ValueDataType: TDataTypeID; override;
    function ValueByteSize: Integer; override;
    function AsInt64: Int64; override;
    function AsString: string; override;
    function AsVariant: Variant; override;
    function CompareTo(Constant: TIDConstant): Integer; override;
    property ArrayLength: Integer read GetLength;
    property ElementType: TIDType read GetElementType;
    {статический массив полностью состоит из констант и может размещатся в статической памяти}
    {не статический массив содержит переменные, поэтому может размещатся только в динамической (стек или куча) памяти}
    property ArrayStatic: Boolean read FStatic write FStatic;
    property CabBeSet: Boolean read CheckAsSet;
    procedure AddItem(const Item: TIDExpression);
  end;

  TConstSpace = TSpace<TIDConstant>;

  {subrange constant value record}
  TSubRangeRecord = record
    LBExpression: TIDExpression;
    HBExpression: TIDExpression;
  end;

  {range constant}
  TIDRangeConstant = class(TIDXXXConstant<TSubRangeRecord>)
  protected
    function GetDisplayName: string; override;
    procedure SetAsVariant(const AValue: Variant); override;
  public
    function ValueDataType: TDataTypeID; override;
    function ValueByteSize: Integer; override;
    function AsInt64: Int64; override;
    function AsString: string; override;
    function AsVariant: Variant; override;
    function CompareTo(Constant: TIDConstant): Integer; override;
  end;

  {guid constant}
  TIDGuidConstant = class(TIDXXXConstant<TGUID>)
  protected
    procedure SetAsVariant(const AValue: Variant); override;
  public
    function ValueDataType: TDataTypeID; override;
    function ValueByteSize: Integer; override;
    function AsString: string; override;
    function AsInt64: Int64; override;
    function AsVariant: Variant; override;
    function CompareTo(Constant: TIDConstant): Integer; override;
  end;

  TIDRecordConstantField = record
    Field: TIDField;
    Value: TIDExpression;
  end;
  TIDRecordConstantFields = array of TIDRecordConstantField;

  {record constant}
  TIDRecordConstant = class(TIDXXXConstant<TIDRecordConstantFields>)
  protected
    procedure SetAsVariant(const AValue: Variant); override;
  public
    function ValueDataType: TDataTypeID; override;
    function ValueByteSize: Integer; override;
    function AsString: string; override;
    function AsInt64: Int64; override;
    function AsVariant: Variant; override;
    function CompareTo(Constant: TIDConstant): Integer; override;
  end;

  {set constant}
  TIDSetConstant = class(TIDXXXConstant<TIDExpressions>)
  protected
    procedure SetAsVariant(const AValue: Variant); override;
  public
    function ValueDataType: TDataTypeID; override;
    function ValueByteSize: Integer; override;
    function AsString: string; override;
    function AsInt64: Int64; override;
    function AsVariant: Variant; override;
    function CompareTo(Constant: TIDConstant): Integer; override;
  end;

  TExpressonType = (
    etDeclaration,
    etExpressionList
  );

  {expression}
  TIDExpression = class(TPooledObject)
  private
    fDeclaration: TIDDeclaration;     // декларация
    fInstruction: TObject;            // иснтрукция результатом которой стало это выражение
                                      // необходима для дальнейшей оптимизации
    fTextPosition: TTextPosition;     // позиция в тексте где встретилось выражение
    function GetItemType: TIDItemType; inline;
    function GetIsAnonymous: Boolean; inline;
    function GetIsConstant: Boolean; inline;
    function GetIsProcedure: Boolean; inline;
    function GetDynArrayConst: Boolean;
    function GetDataTypeID: TDataTypeID; inline;
    function GetDataTypeName: string; inline;
    function GetDisplayName: string; virtual;
    function GetAsConst: TIDConstant; inline;
    function GetAsIntConst: TIDIntConstant; inline;
    function GetAsStrConst: TIDStringConstant; inline;
    function GetAsDynArrayConst: TIDDynArrayConstant; inline;
    function GetAsRangeConst: TIDRangeConstant; inline;
    function GetAsBoolConst: TIDBooleanConstant; inline;
    function GetAsProcedure: TIDProcedure; inline;
    function GetAsProperty: TIDProperty; inline;
    function GetAsType: TIDType; inline;
    function GetAsVariable: TIDVariable; inline;
    function GetAsArrayConst: TIDDynArrayConstant; inline;
    class function GetExpressionType: TExpressonType; virtual;
    function GetIsLocalVar: Boolean;
    function GetIsVariable: Boolean; inline;
    function GetLine: Integer; inline;
    function GetIsTMPVar: Boolean;
    function GetIsTMPRef: Boolean;
    function GetAsCharConst: TIDCharConstant;
    function GetAsClosure: TIDClosure;
    function GetIsAnonymousRef: Boolean;
    function GetIsAnonymousVar: Boolean;
    function GetIsNonAnonimousVariable: Boolean;
    function GetIsAnyLocalVar: Boolean;
    function GetCValue: TIDConstant; inline;
    procedure SetCValue(Value: TIDConstant); inline;
    function GetIsParam: Boolean;
    function GetText: string;
    function GetAsUnit: TIDUnit; inline;
    function GetDeclClass: TIDDeclarationClass;
    function GetIsRangeConst: Boolean;
  protected
    function GetDataType: TIDType; virtual;
  public
    constructor Create(Declaration: TIDDeclaration); overload;
    constructor Create(Declaration: TIDDeclaration; const TextLine: Integer); overload;
    constructor Create(Declaration: TIDDeclaration; const TextPosition: TTextPosition); overload;
    destructor Destroy; override;
    property ExpressionType: TExpressonType read GetExpressionType; // тип выражения
    property DataType: TIDType read GetDataType;
    property DataTypeName: string read GetDataTypeName;
    property DisplayName: string read GetDisplayName;
    property DataTypeID: TDataTypeID read GetDataTypeID;
    property Declaration: TIDDeclaration read FDeclaration write FDeclaration;
    property TextPosition: TTextPosition read FTextPosition write FTextPosition;
    property Line: Integer read GetLine;
    property Instruction: TObject read FInstruction write FInstruction;
    property ItemType: TIDItemType read GetItemType;
    property IsAnonymous: Boolean read GetIsAnonymous;
    property IsAnonymousVar: Boolean read GetIsAnonymousVar;
    property IsAnonymousRef: Boolean read GetIsAnonymousRef;
    property IsTMPVar: Boolean read GetIsTMPVar;
    property IsTMPRef: Boolean read GetIsTMPRef;
    property IsConstant: Boolean read GetIsConstant;
    property IsProcedure: Boolean read GetIsProcedure;
    property IsDynArrayConst: Boolean read GetDynArrayConst;
    property IsLocalVar: Boolean read GetIsLocalVar;
    property IsAnyLocalVar: Boolean read GetIsAnyLocalVar;
    property IsVariable: Boolean read GetIsVariable;
    property IsRangeConst: Boolean read GetIsRangeConst;
    property IsParam: Boolean read GetIsParam;
    property IsNonAnonimousVariable: Boolean read GetIsNonAnonimousVariable;
    //property IsNullableVariable: Boolean read GetIsNullableVariable;
    property AsType: TIDType read GetAsType;
    property AsConst: TIDConstant read GetAsConst;
    property AsIntConst: TIDIntConstant read GetAsIntConst;
    property AsStrConst: TIDStringConstant read GetAsStrConst;
    property AsCharConst: TIDCharConstant read GetAsCharConst;
    property AsDynArrayConst: TIDDynArrayConstant read GetAsDynArrayConst;
    property AsBoolConst: TIDBooleanConstant read GetAsBoolConst;
    property AsVariable: TIDVariable read GetAsVariable;
    property AsProperty: TIDProperty read GetAsProperty;
    property AsProcedure: TIDProcedure read GetAsProcedure;
    property AsArrayConst: TIDDynArrayConstant read GetAsArrayConst;
    property AsRangeConst: TIDRangeConstant read GetAsRangeConst;
    property AsClosure: TIDClosure read GetAsClosure;
    property AsUnit: TIDUnit read GetAsUnit;
    property CValue: TIDConstant read GetCValue write SetCValue;
    property DeclClass: TIDDeclarationClass read GetDeclClass;
    property Text: string read GetText;
  end;

  // Выражение - список (индексы массивов, поля структур)
  TIDMultiExpression = class(TIDExpression)
  private
    FItems: TIDExpressions;
    FDataType: TIDType;
    class function GetExpressionType: TExpressonType; override;
  protected
    function GetDataType: TIDType; override;
    function GetDisplayName: string; override;
  public
    constructor Create(const ExpressionList: TIDExpressions; const TextPosition: TTextPosition); reintroduce; overload;
    constructor Create(const Base, Member: TIDExpression; const TextPosition: TTextPosition); overload;
    constructor CreateAnonymous(Declaration: TIDDeclaration); overload;
    constructor CreateAnonymous(const ExpressionList: TIDExpressions); overload;
    constructor CreateAnonymous(DataType: TIDType; const Expressions: array of TIDExpression); overload;
    procedure AddItem(Expr: TIDExpression);
    property Items: TIDExpressions read FItems write FItems;
    property EffectiveDataType: TIDType read FDataType write FDataType;
  end;

  TIDCallExpression = class(TIDExpression)
  private
    FArgumentsCount: Integer;
    FArguments: TIDExpressions;
    FInstance: TIDExpression;
    FGenericArgs: TIDExpressions;
    FCanInstantiate: Boolean;
    function GetProc: TIDProcedure; inline;
  public
    property Proc: TIDProcedure read GetProc;
    property Arguments: TIDExpressions read FArguments write FArguments;
    property ArgumentsCount: Integer read FArgumentsCount write FArgumentsCount;
    property Instance: TIDExpression read FInstance write FInstance;
    property GenericArgs: TIDExpressions read FGenericArgs write FGenericArgs;
    // CanInstantiate means all generic arguments are real types (not outer generic params)
    property CanInstantiate: Boolean read FCanInstantiate write FCanInstantiate;
  end;

  TIDCastExpression = class(TIDExpression)
  private
    FDataType: TIDType;
  protected
    function GetDataType: TIDType; override;
    function GetDisplayName: string; override;
  public
    constructor Create(Declaration: TIDDeclaration; CastDataType: TIDType; const TextPosition: TTextPosition); reintroduce; overload;
    constructor Create(Source: TIDExpression; CastDataType: TIDType); reintroduce; overload;
  end;

  TIDCastedCallExpression = class(TIDCallExpression)
  private
    FDataType: TIDType;
  public
    function GetDataType: TIDType; override;
    property DataType: TIDType read GetDataType write FDataType;
  end;

  TIDDrefExpression = class(TIDExpression)
  private
    FSrc: TIDExpression;
  protected
    function GetDataType: TIDType; override;
  public
    constructor Create(Src: TIDExpression); reintroduce;
  end;

  TIDArrayExpression = class(TIDExpression)
  private
    fIndexes: TIDExpressions;
    fDataType: TIDType;        // casted data type (optional)
  protected
    function GetDataType: TIDType; override;
    function GetDisplayName: string; override;
  public
    property Indexes: TIDExpressions read fIndexes write fIndexes;
    property DataType: TIDType read GetDataType write FDataType;
  end;

  {result of logical boolean expression lile: a < b, x = y, ... }
  TIDBoolResultExpression = class(TIDExpression)
  end;

  TUnknownIDExpression = class(TIDExpression)
  private
    fID: TIdentifier;
  protected
    function GetDataType: TIDType; override;
    function GetDisplayName: string; override;
  public
    constructor Create(const ID: TIdentifier); reintroduce;
    property ID: TIdentifier read fID;
  end;

  TVariableFlags = set of
  (
    VarIn,           // входной параметр
    VarOut,          // выходной параметр (out)
    VarInOut,        // параметр переданный по ссылке (var)
    VarConst,        // константный параметр
    VarConstRef,     // константный параметр переданный по ссылке
    VarResult,       // параметр-результат функции
    VarSelf,         // параметр self метода
    VarHasDefault,   // параметр имеет значение по умолчаню
    VarParameter,    // параметр
    VarTemporatyVar, // временная переменная (не ссылка)
    VarTemporatyRef, // временная ссылка
    VarHiddenParam,  // неявно обьявленный параметр
    VarLoopIndex,    // переменная цикла for
    VarTmpResOwner,  // временная переменная - хранит указатель на временный обьект,
                     // которые должен быть присвоен в постоянную переменную
                     // к таким перменным не применяется incref/decref
    VarIsField       // поле структуры
  );

  {generic variable class}
  TIDVariable = class(TIDDeclaration)
  private
    FFlags: TVariableFlags;
    FAbsolute: TIDVariable;       // ссылка на поле absolute
    FDefaultValue: TIDExpression; // Значение по умолчанию
    FLastRInstruction: TObject;
    FLastWInstruction: TObject;
    FFirstWInstruction: TObject;
    FCValue: TIDConstant;          // значение переменной после расчета в compile-time
    procedure SetDefaultValue(const Value: TIDExpression);
    function GetIsField: Boolean; inline;
    function GetReference: Boolean; inline;
    function GetVarReference: Boolean; inline;
    function GetIsTemporary: Boolean; inline;
    function GetIsTemporaryOwner: Boolean;
    function GetLastRWInstruction: TObject;
    function GetIsAnyLocalVar: Boolean; inline;
    function GetIsExplicit: Boolean; inline;
  protected
    function GetDisplayName: string; override;
    function GetCValue: TIDConstant; override;
    function GetIsGeneric: Boolean; override;
    procedure SetCValue(const Value: TIDConstant); override;
  public
    constructor Create(Scope: TScope; const Identifier: TIdentifier); override;
    constructor Create(Scope: TScope; const Name: TIdentifier; DataType: TIDType; Flags: TVariableFlags); reintroduce; overload;
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    constructor CreateAsTemporary(Scope: TScope; DataType: TIDType);
    constructor CreateAsAnonymous(Scope: TScope); override;
    //////////////////////////////////////////////////////////////////////////////
    property Flags: TVariableFlags read FFlags write FFlags;
    property DefaultValue: TIDExpression read FDefaultValue write SetDefaultValue;
    property Reference: Boolean read GetReference;
    property VarReference: Boolean read GetVarReference;
    property Absolute: TIDVariable read FAbsolute write FAbsolute;
    property IsField: Boolean read GetIsField;
    property IsTemporary: Boolean read GetIsTemporary;
    property IsExplicit: Boolean read GetIsExplicit;
    property IsTemporaryOwner: Boolean read GetIsTemporaryOwner;
    property IsAnyLocalVar: Boolean read GetIsAnyLocalVar;
    // первая инструкция оперирующая с этой переменной
    property FirstWInstruction: TObject read FFirstWInstruction write FFirstWInstruction;
    // последняя инструкция оперирующая с этой переменной
    property LastRInstruction: TObject read FLastRInstruction write FLastRInstruction;
    property LastWInstruction: TObject read FLastWInstruction write FLastWInstruction;
    property LastRWInstruction: TObject read GetLastRWInstruction;
    procedure IncRefCount(RCPath: UInt32); override;
    procedure DecRefCount(RCPath: UInt32); override;
    procedure IncludeFlags(const Flags: TVariableFlags);
    procedure SaveAndIncludeFlags(const Flags: TVariableFlags; out PrevFlags: TVariableFlags);

    procedure Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer = 0; AAppendName: Boolean = True); override;
    function MakeCopy: TIDDeclaration; override;
    function InstantiateGeneric(ADstScope: TScope; ADstStruct: TIDStructure;
                                const AContext: TGenericInstantiateContext): TIDDeclaration; override;
  end;

  {proc parameter class}
  TIDParameter = class(TIDVariable)
  public
    procedure Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer = 0; AAppendName: Boolean = True); override;
  end;

  {structure field class}
  TIDField = class(TIDVariable)
  private
    fStruct: TIDStructure;
    fIsClass: Boolean;
    function GetFieldIndex: Integer;
  protected
    function GetIndex: Integer; override;
  public
    constructor Create(Struct: TIDStructure; const Identifier: TIdentifier); reintroduce;
    property Struct: TIDStructure read fStruct;
    property FieldIndex: Integer read GetFieldIndex;
    property IsClass: Boolean read fIsClass;
    procedure Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer = 0; AAppendName: Boolean = True); override;
  end;

  {свойство структуры}
  TIDProperty = class(TIDDeclaration)
  private
    FGetter: TIDDeclaration;
    FSetter: TIDDeclaration;
    FParams: TParamsScope;
    FIndexValue: TIDConstant;
    FDefaultIndexedProperty: Boolean;
    function GetParamsCount: Integer;
  public
    constructor Create(Scope: TScope; const Identifier: TIdentifier); override;
    property Getter: TIDDeclaration read FGetter write FGetter;
    property Setter: TIDDeclaration read FSetter write FSetter;
    property Params: TParamsScope read FParams write FParams;
    property ParamsCount: Integer read GetParamsCount;
    property IndexValue: TIDConstant read FIndexValue write FIndexValue;
    property DefaultIndexedProperty: Boolean read FDefaultIndexedProperty write FDefaultIndexedProperty;
    function MakeCopy: TIDDeclaration; override;
    function InstantiateGeneric(ADstScope: TScope; ADstStruct: TIDStructure;
                                const AContext: TGenericInstantiateContext): TIDDeclaration; override;
    procedure Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer = 0; AAppendName: Boolean = True); override;
  end;

  TProcFlag = (
    pfHasResult,
    pfVirtual,
    pfAbstract,
    pfInline,
    pfOveload,
    pfExport,
    pfImport,
    pfOperator,
    pfConstructor,
    pfDestructor,
    pfCompleted,
    pfReintroduce,
    pfOverride,
    pfFinal,
    pfClass,
    pfStatic,
    pfForward
  );

  TProcFlags = set of TProcFlag;


  TIDParamList = TList<TIDVariable>;

  {procedure/function}
  TIDProcedure = class(TIDDeclarationGeneric)
  strict private
    FExplicitParams: TIDParamArray; // actual arguments (excluding 'self', 'Result', etc...);
    FParamsScope: TProcScope;       // only parameters scope
    FEntryScope: TProcScope;        // proc body scope (params, local vars, types, procs etc...)
    FStruct: TIDStructure;          // self parameter
    FProcFlags: TProcFlags;         // флаги inline/pure
    FTempVars: TItemsStack;
    FNextOverload: TIDProcedure;
    FCallConv: TCallConvention;
    FResultType: TIDType;
    FResultParam: TIDParam;
    FVirtualIndex: Integer;
    FGenericPrototype: TIDProcedure;        // исходная generic-процедруа (если дання является специализированным инстансом)
    FFirstBodyLine: Integer;                // позиция в исходном коде начала тела процедуры
    FLastBodyLine: Integer;                 // позиция в исходном коде конца тела процедуры
    FFinalSection: TObject;                 // блок финализации процедуры (TIL)
    FInherited: TIDProcedure;               // inherited entry in case this is overridden method
  private
    procedure SetParameters(const Value: TIDParamArray); inline;
    procedure SetEntryScope(const Value: TProcScope);
    function GetIsCompleted: Boolean; inline;
    function GetProcKindName: string;
    function GetIsStatic: Boolean; inline;
    function GetDefaultParamsCount: Integer;
    function GetMethodIndex: Integer;
    function GetSelfParam: TIDVariable;
    function GetSelfParamExpression: TIDExpression;
    function GetResultParamExpression: TIDExpression;
    function GetIsClassMethod: Boolean; inline;
    function GetIsConstructor: Boolean; inline;
    function GetIsDestructor: Boolean;
  protected
    function GetDisplayName: string; override;
    function GetIndex: Integer; override;
    function GetParamsCount: Integer; virtual;
    function GetIsGeneric: Boolean; override;
  public
    constructor Create(Scope: TScope; const ID: TIdentifier); override;
    constructor CreateAsAnonymous(Scope: TScope); override;
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    constructor CreateAsSystemMethod(Struct: TIDStructure; const Name: string);
    destructor Destroy; override;
    //////////////////////////////////////////////////////////////////////////////////
    property ExplicitParams: TIDParamArray read FExplicitParams write SetParameters;

    function SameDeclaration(const AParams: TIDParamArray;
                             AResultType: TIDType;
                             ACheckNames: Boolean = False): Boolean; overload;
    // Temporary variable alloc
    function GetTMPVar(DataType: TIDType; Reference: Boolean = False): TIDVariable; overload;
    function GetTMPVar(DataType: TIDType; VarFlags: TVariableFlags): TIDVariable; overload;
    function GetTMPRef(DataType: TIDType): TIDVariable;
    property TempVars: TItemsStack read FTempVars;

    property PrevOverload: TIDProcedure read FNextOverload write FNextOverload;
    property EntryScope: TProcScope read FEntryScope write SetEntryScope;
    property ParamsScope: TProcScope read FParamsScope write FParamsScope;
    property ParamsCount: Integer read GetParamsCount;
    property ResultType: TIDType read FResultType write FResultType;
    property Flags: TProcFlags read FProcFlags write FProcFlags;
    property Struct: TIDStructure read FStruct write FStruct;
    property SelfParam: TIDVariable read GetSelfParam;
    property SelfParamExpression: TIDExpression read GetSelfParamExpression;
    property InheritedProc: TIDProcedure read FInherited write FInherited;

    property ResultParamExpression: TIDExpression read GetResultParamExpression;

    procedure MakeSelfParam;
    procedure SetResult(DataType: TIDType);
    procedure AddParam(const Param: TIDVariable); overload;
    function AddParam(const Name: string; DataType: TIDType): TIDParam; overload;
    function AddParam(const Name: string; DataType: TIDType; Flags: TVariableFlags; DefaultValue: TIDExpression = nil): TIDParam; overload;
    //======================================================================
    procedure IncRefCount(RCPath: UInt32); override;
    procedure DecRefCount(RCPath: UInt32); override;

    procedure IncTypesReadCountInSignature(RCPath: UInt32);
    procedure DecTypesReadCountInSignature(RCPath: UInt32);
    procedure RemoveILReferences(var RCPathCount: UInt32); override;

    procedure CreateProcedureTypeIfNeed(Scope: TScope);
    procedure CreateGenericDescriptor(const GenericParams: TIDTypeArray); inline;

    procedure Warning(const Message: string; const Params: array of const; const TextPosition: TTextPosition);
    procedure Hint(const Message: string; const Params: array of const; const TextPosition: TTextPosition);

    property IsCompleted: Boolean read GetIsCompleted;
    property IsStatic: Boolean read GetIsStatic;
    property IsClassMethod: Boolean read GetIsClassMethod;
    property ProcKindName: string read GetProcKindName;
    property DefaultParamsCount: Integer read GetDefaultParamsCount;
    property VirtualIndex: Integer read FVirtualIndex write FVirtualIndex;
    property MethodIndex: Integer read GetMethodIndex;
    property GenericPrototype: TIDProcedure read FGenericPrototype write FGenericPrototype;
    property CallConvention: TCallConvention read FCallConv write FCallConv;
    property IsGeneric: Boolean read GetIsGeneric;
    property IsConstructor: Boolean read GetIsConstructor;
    property IsDestructor: Boolean read GetIsDestructor;

    property FirstBodyLine: Integer read FFirstBodyLine write FFirstBodyLine;
    property LastBodyLine: Integer read FLastBodyLine write FLastBodyLine;
    property FinalSection: TObject read FFinalSection write FFinalSection;
    function GetDebugVariables: string;
    function GetAllOverloadSignatures(const LineSeparator: string = #13#10): string;

    function InstantiateGenericProc(ADstScope: TScope; ADstStruct: TIDStructure;
                                    ANextOverload: TIDProcedure;
                                    const AContext: TGenericInstantiateContext): TIDDeclaration;

    function InstantiateGeneric(ADstScope: TScope; ADstStruct: TIDStructure;
                                const AContext: TGenericInstantiateContext): TIDDeclaration; override;

    procedure Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer = 0; AAppendName: Boolean = True); override;
  end;

  TASTDelphiProc = class(TIDProcedure)
  private
    fBody: TASTBlock;
  public
    property Body: TASTBlock read fBody write fBody;
  end;

  {пользовательский перегруженный оператор}
  TIDOperator = class(TASTDelphiProc)
  private
    FOperator: TOperatorID;
    function GetRightOperand: TIDType;
  public
    constructor Create(Scope: TScope; const ID: TIdentifier; &Operator: TOperatorID); reintroduce;
    constructor CreateAsSystem(Scope: TScope); reintroduce;
    /////////////////////////////////////////////////////////////////////////////////////////
    property RightOperand: TIDType read GetRightOperand;
    property OperatorID: TOperatorID read FOperator;
  end;


  TBuiltInFunctionID = (
    bf_sysrtfunction,      // системная run-time встроенная функция
    bf_sysctfunction       // системная compile-time встроенная функция
  );

  TUnitList = TStringList;

  // case insensitive list
  TIDList = class(TAVLTree<string, TIDDeclaration>)
  public
    function InsertID(Item: TIDDeclaration): Boolean; inline; // true - ok, false - already exist
    function InsertIDAndReturnIfExist(Item: TIDDeclaration): TIDDeclaration; inline;
    function FindID(const Identifier: string): TIDDeclaration; virtual; //inline;
  end;

  TScopeClass = (scInterface, scImplementation, scProc);

  TScopes = array of TScope;

  TScope = class abstract (TIDList)
  private
    fUnit: TASTModule;          // parent unit
    fParent: TScope;            // parent scope
    fScopeType: TScopeType;     // scope type
    fAdditionalScopes: TScopes; // a list if joint scopes (with scopes, generic params scope)
    fChilds: TList<TScope>;     // child scopes (for memory management), don't search here
    fName: string;
    FVarCount: Integer;
    FProcCount: Integer;
    FTypeCount: Integer;
    FConstCount: Integer;
    procedure SetParent(const Value: TScope);
    function GetDeclUnitName: string; inline;
  protected
    function GetName: string; virtual;
    function GetNameEx: string; virtual;
    function GetScopeClass: TScopeClass; virtual;
    procedure AddChild(Scope: TScope);
    procedure RemoveChild(Scope: TScope);
  public
    constructor Create(ScopeType: TScopeType; DeclUnit: TASTModule); overload;
    constructor Create(ScopeType: TScopeType; Parent: TScope); overload;
    constructor Create(ScopeType: TScopeType; Parent: TScope; DeclUnit: TASTModule); overload;
    destructor Destroy; override;
    //////////////////////////////////////////////////////////////////////
    procedure AddType(ADeclaration: TIDType); virtual;
    procedure AddVariable(ADeclaration: TIDVariable); virtual;
    procedure AddProcedure(ADeclaration: TIDProcedure; AddOverload: Boolean = False); virtual;
    procedure AddAnonymousProcedure(ADeclaration: TIDProcedure); virtual;
    procedure AddProperty(ADeclaration: TIDProperty); virtual;
    procedure AddConstant(ADeclaration: TIDConstant); virtual;
    procedure AddScope(Scope: TScope);
    procedure RemoveVariable(ADeclaration: TIDVariable);
    function FindInAdditionalScopes(const ID: string): TIDDeclaration; overload;
    procedure FindInAdditionalScopes(const ID: string; var ADeclArray: TIDdeclarray); overload;
    function FindIDRecurcive(const ID: string): TIDDeclaration; overload; virtual;
    procedure FindIDRecurcive(const ID: string; var ADeclArray: TIDDeclArray); overload; virtual;
    function FindMembers(const ID: string): TIDDeclaration; overload; virtual;
    procedure FindMembers(const ID: string; var ADeclArray: TIDDeclArray); overload; virtual;
    function GetDeclArray(Recursively: Boolean = False): TIDDeclArray;
    function GetDeclNamesArray(Recursively: Boolean = False): TStrArray;
    function GetParentNames: string;
    property Parent: TScope read FParent write SetParent;
    property ScopeType: TScopeType read FScopeType;
    property ScopeClass: TScopeClass read GetScopeClass;
    property DeclUnit: TASTModule read FUnit;
    property DeclUnitName: string read GetDeclUnitName;
    property AdditionalScopes: TScopes read FAdditionalScopes;
    property Name: string read GetName write FName;

    property VarCount: Integer read FVarCount;
    property ProcCount: Integer read FProcCount;
    property TypeCount: Integer read FTypeCount;
    property ConstCount: Integer read FConstCount;
  end;

  TParamsScope = class(TScope)
  private
    FExplicitParams: TIDParamArray;
    FSelfParam: TIDParam;
    function GetItem(AIndex: Integer): TIDParam; inline;
  public
    procedure AddExplicitParam(AParam: TIDParam);
    procedure AddImplicitParam(AParam: TIDParam);
    property ExplicitParams: TIDParamArray read FExplicitParams;
    property Items[AIndex: Integer]: TIDParam read GetItem; default;
    property SelfParam: TIDParam read FSelfParam write FSelfParam;
  end;

  {procedure scope}
  TProcScope = class(TScope)
  private
    fProc: TIDProcedure;
    fOuterScope: TScope; // todo: implementation scope
    FParamsScope: TParamsScope;
    function GetExplicitParams: TIDParamArray;
    function GetExplicitParamsCount: Integer; inline;
  protected
    function GetScopeClass: TScopeClass; override;
    function GetName: string; override;
    function GetNameEx: string; override;
  public
    constructor CreateInDecl(Parent: TScope; Proc: TIDProcedure); reintroduce;
    constructor CreateInBody(Parent: TScope); reintroduce;
    procedure CopyFrom(SourceScope: TProcScope); reintroduce;
    property OuterScope: TScope read fOuterScope write fOuterScope;
    property Proc: TIDProcedure read fProc write fProc;
    property ParamsScope: TParamsScope read FParamsScope;
    property ExplicitParamsCount: Integer read GetExplicitParamsCount;
    property ExplicitParams: TIDParamArray read GetExplicitParams;
    function FindID(const Identifier: string): TIDDeclaration; override;
    function FindIDRecurcive(const ID: string): TIDDeclaration; override;
  end;

  TStructScope = class(TScope)
    fAncestorScope: TStructScope;
    fStruct: TIDStructure;
  protected
    function GetName: string; override;
    function GetNameEx: string; override;
  public
    constructor CreateAsStruct(Parent: TScope; Struct: TIDStructure; DeclUnit: TASTModule); reintroduce;
    function FindIDRecurcive(const ID: string): TIDDeclaration; override;
    procedure FindIDRecurcive(const ID: string; var ADeclArray: TIDDeclArray); override;
    function FindMembers(const ID: string): TIDDeclaration; override;
    property Struct: TIDStructure read fStruct;
    property AncestorScope: TStructScope read fAncestorScope;
  end;

  TRecordInitScope = class(TScope)
  private
    fStructType: TIDStructure;
  protected
    function GetName: string; override;
  public
    property Struct: TIDStructure read fStructType write fStructType;
  end;

  // begin...end scope
  TBlockScope = class(TScope)
  end;

  // if () then...; scope
  TIfThenScope = class(TScope)
  end;

  // else...; scope
  TElseScope = class(TScope)
  end;

  // for () do...; scope
  TForScope = class(TScope)
  end;

  // while () do...; scope
  TWhileScope = class(TScope)
  end;

  // repeat...until scope
  TRepeatScope = class(TScope)
  end;

  // try...end scope
  TTryScope = class(TScope)
  end;

  // finally..end scope
  TFinallyScope = class(TScope)
  end;

  // except..end scope
  TExceptScope = class(TScope)
  end;

  // with () do...; scope
  TWithScope = class(TProcScope)
  private
    fInnerScope: TScope;
    fExpression: TIDExpression;       // выражение, которое породило данный Scope (то что написано в WITH ... DO)
  protected
    function GetName: string; override;
  public
    constructor Create(Parent: TScope; Expression: TIDExpression); reintroduce;
    ///////////////////////////////////////
    function FindIDRecurcive(const ID: string): TIDDeclaration; override;
    property InnerScope: TScope read FInnerScope write FInnerScope;
    property Expression: TIDExpression read FExpression;
  end;

  // enumeration scope
  TEnumScope = class(TScope)

  end;

  TMethodScope = class(TProcScope)
  private
    function GetStruct: TIDStructure; inline;
  protected
    function GetName: string; override;
  public
    constructor CreateInDecl(OuterScope, Parent: TScope; AProc: TIDProcedure); reintroduce;
    function FindIDRecurcive(const ID: string): TIDDeclaration; override;
    property Struct: TIDStructure read GetStruct;
  end;

  TInterfaceScope = class(TScope)
  protected
    function GetName: string; override;
  public
    constructor Create(AUnit: TASTModule); reintroduce;
  end;

  TImplementationScope = class(TScope)
  private
    fIntfScope: TScope;
  protected
    function GetName: string; override;
    function GetScopeClass: TScopeClass; override;
  public
    constructor Create(InterfaceScope: TScope); reintroduce;
    function FindID(const Identifier: string): TIDDeclaration; override;
    //TODO: remove unneded code in TImplementationScope
    function FindIDRecurcive(const ID: string): TIDDeclaration; override;
    property IntfScope: TScope read fIntfScope;
  end;

  TConditionalScope = class(TScope)
  protected
    function GetName: string; override;
  end;

  TASTDelphiLabel = class(TIDDeclaration)
    constructor Create(Scope: TScope; const Identifier: TIdentifier); overload; override;
  end;

  TIDPairList = class (TAVLTree<TIDDeclaration, TObject>)
  private
    function GetItem(const Key: TIDDeclaration): TObject;
  public
    constructor Create; reintroduce;
    property Items[const Key: TIDDeclaration]: TObject read GetItem;
  end;

  function Identifier(const Name: string; TextPosition: TTextPosition): TIdentifier; overload; inline;
  function Identifier(const Name: string; SourceRow: Integer = 0; SourceCol: Integer = 0): TIdentifier; overload; inline;

  function DeclarationName(Decl: TIDDeclaration; IsList: Boolean = False): string;
  function ExpressionName(Expr: TIDExpression): string;
  function ExpressionsName(const Expressions: TIDExpressions): string;
  function GetExpressionsNames(const Expressions: TIDExpressions; const Separator: string = ', '): string;
  function GetProcName(Proc: TIDProcedure; WithParamsDataTypes: Boolean = False): string;
  function IDCompare(const Key1, Key2: TIDDeclaration): NativeInt;
  function IDVarCompare(const Key1, Key2: TIDVariable): NativeInt;
  function GetVarDebugString(AScope: TScope): string;
  procedure WriteDataTypeIndex(Stream: TStream; DataType: TIDType);

  procedure IncReadCount(const Expression: TIDExpression; const Instruction: TObject; RCPath: UInt32); inline;
  procedure DecReadCount(const Expression: TIDExpression; const Instruction: TObject; RCPath: UInt32); inline;

  function GetBoolResultExpr(ExistExpr: TIDExpression): TIDBoolResultExpression;
  function GetItemTypeName(ItemType: TIDItemType): string;

  function SameTypes(ASrcType, ADstType: TIDType): Boolean;
  function SameProcSignTypes(ASrcType, ADstType: TIDType): Boolean;
  function IsGenericTypeThisStruct(Scope: TScope; Struct: TIDType): Boolean;
  function GenericNeedsInstantiate(AGenericArgs: TIDExpressions): Boolean;

const
  CIsClassProc: array [TProcType] of Boolean =
  (
    {ptFunc} False,
    {ptClassFunc} True,
    {ptStaticFunc} True,
    {ptProc} False,
    {ptClassProc} True,
    {ptStaticProc} True,
    {ptConstructor} False,
    {ptDestructor} False,
    {ptClassConstructor} False,
    {ptClassDestructor} False,
    {ptOperator} False
  );

  function IsClassProc(AProcType: TProcType): Boolean; inline;

  function IsGenericArgMatch(ATypeArg: TIDType; const AGenericParam: TIDGenericParam): Boolean;
  function CallConventionToStr(AConvention: TCallConvention): string;

implementation

uses AST.Delphi.System,
     AST.Pascal.Parser,
     AST.Parser.Errors,
     AST.Delphi.Errors,
     AST.Delphi.Parser,
     AST.Delphi.SysFunctions,
     AST.Parser.Log;

procedure TypeNameToString(AType: TIDType; ABuilder: TStringBuilder);
begin
  if Assigned(AType) then
    ABuilder.Append(AType.DisplayName)
  else
    ABuilder.Append('<unassigned>');
end;

procedure Params2Str(ABuilder: TStringBuilder; const AParams: TIDParamArray);
begin
  if Length(AParams) > 0 then
  begin
    ABuilder.Append('(');
    for var LIndex := 0 to Length(AParams) - 1 do
    begin
      AParams[LIndex].Decl2Str(ABuilder);
      if LIndex < Length(AParams) - 1 then
        ABuilder.Append('; ');
    end;

    ABuilder.Append(')');
  end;
end;

procedure CheckVarTypeAssigned(AVariable: TIDVariable);
begin
  if not Assigned(AVariable.DataType) then
  begin
    if AVariable is TIDField then
    begin
      var LStructName := '<unassigned>';
      if Assigned(TIDField(AVariable).Struct) then
        LStructName := TIDField(AVariable).Struct.DisplayName;
      AbortWorkInternal('Field datatype is not assinged for: %s.%s',
                        [LStructName, AVariable.DisplayName], AVariable.TextPosition)
    end else
      AbortWorkInternal('Variable datatype is not assinged for: %s', [AVariable.DisplayName], AVariable.TextPosition);
  end;
end;

function IsGenericArgMatch(ATypeArg: TIDType; const AGenericParam: TIDGenericParam): Boolean;
begin
  case AGenericParam.Constraint of
    gsClass: Result := (ATypeArg.DataTypeID = dtClass);               // <T: class
    gsConstructor : Result := (ATypeArg.DataTypeID = dtClass);        // <T: constructor>
    gsClassAndConstructor: Result := (ATypeArg.DataTypeID = dtClass); // <T: class constructor>
    gsRecord: Result := (ATypeArg.DataTypeID = dtRecord {todo:});     // <T: record>
    gsType: Result := (ATypeArg is TIDStructure) and
                       TIDStructure(ATypeArg).IsInheritsForm(AGenericParam.ConstraintType as TIDStructure);
  else
    Result := True;
  end;
end;

function IsClassProc(AProcType: TProcType): Boolean; inline;
begin
  Result := CIsClassProc[AProcType];
end;

function GetItemTypeName(ItemType: TIDItemType): string;
begin
  case ItemType of
    itUnknown: Result := 'Unknown';
    itVar: Result := 'Var';
    itConst: Result := 'Const';
    itProcedure: Result := 'Procedure';
    itSysOperator: Result := 'SysOperator';
    itMacroFunction: Result := 'MacroFunction';
    itProperty: Result := 'Property';
    itAlias: Result := 'Alias';
    itType: Result := 'Type';
    itUnit: Result := 'Unit';
    itLabel: Result := 'Label';
  end;
end;


function GetBoolResultExpr(ExistExpr: TIDExpression): TIDBoolResultExpression;
begin
  Result := TIDBoolResultExpression.Create(ExistExpr.Declaration, ExistExpr.TextPosition);
end;

function IDCompare(const Key1, Key2: TIDDeclaration): NativeInt;
begin
  Result := NativeInt(Key1) - NativeInt(Key2);
end;

function DataTypeIDCompare(const Key1, Key2: TDataTypeID): NativeInt;
begin
  Result := Ord(Key1) - Ord(Key2);
end;

function IDVarCompare(const Key1, Key2: TIDVariable): NativeInt;
begin
  Result := NativeInt(Key1) - NativeInt(Key2);
end;

procedure IncReadCount(const Expression: TIDExpression; const Instruction: TObject; RCPath: UInt32); inline;
var
  i: Integer;
  Decl: TIDDeclaration;
  Items: TIDExpressions;
begin
  if Assigned(Expression) then
   if Expression.ExpressionType = etDeclaration then
   begin
     Decl := Expression.Declaration;
     Decl.IncRefCount(RCPath);
     if Expression.IsAnyLocalVar then
       TIDVariable(Decl).LastRInstruction := Instruction;

     if Expression is TIDCastedCallExpression then
       Expression.DataType.IncRefCount(RCPath);

   end else begin
     Items := TIDMultiExpression(Expression).Items;
     for i := 0 to Length(Items) - 1 do
     begin
       Decl := Items[i].Declaration;
       Decl.IncRefCount(RCPath);
       if Items[i].IsAnyLocalVar then
         TIDVariable(Decl).LastRInstruction := Instruction;
     end;
   end;
end;

procedure DecReadCount(const Expression: TIDExpression; const Instruction: TObject; RCPath: UInt32);
var
  i: Integer;
  Decl: TIDDeclaration;
  Items: TIDExpressions;
begin
  if Assigned(Expression) then
   if Expression.ExpressionType = etDeclaration then
   begin
     Decl := Expression.Declaration;
     Decl.DecRefCount(RCPath);
   end
   else begin
     Items := TIDMultiExpression(Expression).Items;
     for i := 0 to Length(Items) - 1 do
     begin
       Decl := Items[i].Declaration;
       Decl.DecRefCount(RCPath);
     end;
   end;
end;

function Identifier(const Name: string; SourceRow: Integer = 0; SourceCol: Integer = 0): TIdentifier;
begin
  Result.Name := Name;
  with Result do begin
    TextPosition.Row := SourceRow;
    TextPosition.Col := SourceRow;
  end;
end;

function Identifier(const Name: string; TextPosition: TTextPosition): TIdentifier;
begin
  Result.Name := Name;
  Result.TextPosition := TextPosition;
end;

function GetVarDebugString(AScope: TScope): string;
var
  LTypeName: string;
begin
  Result := '';
  for var LDeclIndex := 0 to AScope.Count - 1 do
  begin
    var LDecl := AScope.Items[LDeclIndex];
    if LDecl is TIDVariable then
    begin
      var LVarDecl := TIDVariable(LDecl);
      if Assigned(LVarDecl.DataType) then
        LTypeName := LVarDecl.DataType.DisplayName
      else
        LTypeName := '<untyped>';
      if LVarDecl.Reference then
        LTypeName := '^' + LTypeName;
      if Assigned(LVarDecl.Absolute) then
        LTypeName := LTypeName + ' absolute ' + LVarDecl.Absolute.DisplayName;
      if VarTmpResOwner in LVarDecl.Flags then
        LTypeName := LTypeName + '[tmpresown]';
      var LVarDesc := '  ' + format('%s: %s;', [DeclarationName(LVarDecl), LTypeName]);
      Result := AddStringSegment(Result, LVarDesc, #10);
    end;
  end;
end;

procedure WriteConstToStream(Stream: TStream; Decl: TIDConstant; DataType: TIDType);
{var
  i, ec: Integer;
  Field: TIDVariable;
  ArrayDT: TIDArray;}
begin
  Assert(False, 'Not supported');
(*  case DataType.DataTypeID of
    dtInt8: Stream.WriteInt8(Decl.AsInt64);
    dtUInt8, dtAnsiChar, dtBoolean: Stream.WriteUInt8(Decl.AsUInt64);
    dtInt16: Stream.WriteInt16(Decl.AsInt64);
    dtUInt16, dtChar: Stream.WriteUInt16(Decl.AsUInt64);
    dtInt32: Stream.WriteInt32(Decl.AsInt64);
    dtUInt32: Stream.WriteUInt32(Decl.AsUInt64);
    dtInt64: Stream.WriteInt64(Decl.AsInt64);
    dtUInt64: Stream.WriteUInt64(Decl.AsUInt64);
    dtFloat32: Stream.WriteFloat32(TIDFloatConstant(Decl).Value);
    dtFloat64: Stream.WriteFloat64(TIDFloatConstant(Decl).Value);
    dtString, dtAnsiString: Stream.WriteStretchUInt(Decl.Index);
    dtPointer: begin
      Assert(Decl.AsInt64 = 0); // временно так! только для nullptr
      Stream.WriteInt32(0);
    end;
    dtEnum, dtSet, dtRange: begin
      case DataType.DataSize of
        1: Stream.WriteInt8(Decl.AsInt64);
        2: Stream.WriteInt16(Decl.AsInt64);
        4: Stream.WriteInt32(Decl.AsInt64);
        8: Stream.WriteInt64(Decl.AsInt64);
      else
        raise Exception.CreateFmt('Type "%s" is not supported', [DataType.DisplayName]);
      end;
    end;
    dtStaticArray: begin
        // кол-во элементов  (пока только одномерные массивы)
        ec := Length(TIDDynArrayConstant(Decl).Value);
        ArrayDT := TIDArray(Decl.DataType);
        // запись элементов
        for i := 0 to ec - 1 do
          WriteConstToStream(Stream, TIDConstant(TIDDynArrayConstant(Decl).Value[i].Declaration), ArrayDT.ElementDataType);
      end;
    dtRecord: begin
      ec := Length(TIDDynArrayConstant(Decl).Value);
      // запись элементов
      Field := TIDStructure(DataType).FirstField;
      for i := 0 to ec - 1 do
      begin
        WriteConstToStream(Stream, TIDConstant(TIDDynArrayConstant(Decl).Value[i].Declaration), Field.DataType);
        Field := TIDVariable(Field.NextItem);
      end;
    end;
    dtVariant: begin
      Stream.WriteInt64(ord(Decl.ValueDataType)); // typeid
      Stream.WriteInt64(Decl.AsInt64);            // value
    end
  else
    // AbortWorkInternal('Type "%s" is not supported', [DataType.DisplayName]);
    // пока TVarRec не пишется
  end;*)
end;

{ TScope }

constructor TScope.Create(ScopeType: TScopeType; DeclUnit: TASTModule);
begin
  inherited Create(StrCICompare);
  FScopeType := ScopeType;
  FUnit := DeclUnit;
  Assert(Assigned(DeclUnit));
end;

constructor TScope.Create(ScopeType: TScopeType; Parent: TScope);
begin
  Assert(Parent <> Self);
  inherited Create(StrCICompare);
  FScopeType := ScopeType;
  FParent := Parent;
  FUnit := Parent.DeclUnit;
  Parent.AddChild(Self);
end;

constructor TScope.Create(ScopeType: TScopeType; Parent: TScope; DeclUnit: TASTModule);
begin
  Assert(Parent <> Self);
  inherited Create(StrCICompare);
  FScopeType := ScopeType;
  FParent := Parent;
  FUnit := DeclUnit;
  Assert(Assigned(DeclUnit));
  if Assigned(Parent) then
    Parent.AddChild(Self);
end;

destructor TScope.Destroy;
var
  i: Integer;
begin
  if Assigned(FChilds) then begin
    for i := 0 to FChilds.Count - 1 do
      FChilds[i].Free;
    FChilds.Free;
  end;
  inherited;
end;

procedure TScope.AddScope(Scope: TScope);
var
  c: Integer;
begin
  c := Length(FAdditionalScopes);
  SetLength(FAdditionalScopes, c + 1);
  FAdditionalScopes[c] := Scope;
end;

procedure TScope.AddType(ADeclaration: TIDType);
begin
  if not InsertID(ADeclaration) then
    AbortWork(sIdentifierRedeclaredFmt, [ADeclaration.DisplayName], ADeclaration.SourcePosition);

  Inc(FTypeCount);
end;

procedure TScope.AddAnonymousProcedure(ADeclaration: TIDProcedure);
begin
  // ?
end;

procedure TScope.AddChild(Scope: TScope);
begin
  if not Assigned(FChilds) then
    FChilds := TList<TScope>.Create;
  FChilds.Add(Scope);
end;

procedure TScope.AddConstant(ADeclaration: TIDConstant);
begin
  if not InsertID(ADeclaration) then
    AbortWork(sIdentifierRedeclaredFmt, [ADeclaration.Name], ADeclaration.SourcePosition);
  Inc(FConstCount);
end;

procedure TScope.RemoveChild(Scope: TScope);
begin
  if Assigned(FChilds) then
    FChilds.Remove(Scope);
end;

procedure TScope.RemoveVariable(ADeclaration: TIDVariable);
var
  Node: PAVLNode;
begin
  Assert(False, 'Not Supported!');
  Node := Find(ADeclaration.Name);
  if Assigned(Node) and (Node.Data = ADeclaration) then
  begin
    Delete(ADeclaration.Name);
//    FVarSpace.Delete(Declaration);
  end;
end;

procedure TScope.AddProcedure(ADeclaration: TIDProcedure; AddOverload: Boolean = False);
begin
  if not AddOverload and not InsertID(ADeclaration) then
    AbortWork(sIdentifierRedeclaredFmt, [ADeclaration.DisplayName], ADeclaration.SourcePosition);
  Inc(FProcCount);
end;

procedure TScope.AddProperty(ADeclaration: TIDProperty);
begin
  if not InsertID(ADeclaration) then
    AbortWork(sIdentifierRedeclaredFmt, [ADeclaration.Name], ADeclaration.SourcePosition);
end;

procedure TScope.AddVariable(ADeclaration: TIDVariable);
begin
  if not InsertID(ADeclaration) then
    AbortWork(sIdentifierRedeclaredFmt, [ADeclaration.Name], ADeclaration.SourcePosition);
  Inc(FVarCount);
end;

function TScope.FindIDRecurcive(const ID: string): TIDDeclaration;
begin
  // search within oneself
  Result := FindID(ID);
  if Assigned(Result) then
    Exit;

  // search in additional scopes
  Result := FindInAdditionalScopes(ID);
  if Assigned(Result) then
    Exit;

  // search in the parent
  if Assigned(FParent) then
    Result := FParent.FindIDRecurcive(ID);
end;

procedure TScope.FindIDRecurcive(const ID: string; var ADeclArray: TIDDeclArray);
begin
  // search within oneself
  var LDecl := FindID(ID);
  if Assigned(LDecl) then
    ADeclArray := ADeclArray + [LDecl];

  // search in additional scopes
  FindInAdditionalScopes(ID, {var} ADeclArray);

  // если есть родитель - ищем в нем
  if Assigned(FParent) then
    FParent.FindIDRecurcive(ID, {var} ADeclArray);
end;

procedure TScope.FindInAdditionalScopes(const ID: string; var ADeclArray: TIDdeclarray);
begin
  // search in the backward direction than defined in the code
  for var LIndex := Length(FAdditionalScopes) - 1 downto 0 do
    FAdditionalScopes[LIndex].FindMembers(ID, ADeclArray);
end;

function TScope.FindInAdditionalScopes(const ID: string): TIDDeclaration;
begin
  // search in the backward direction than defined in the code
  for var LIndex := Length(FAdditionalScopes) - 1 downto 0 do
  begin
    Result := FAdditionalScopes[LIndex].FindMembers(ID);
    if Assigned(Result) then
      Exit;
  end;
  Result := nil;
end;

function TScope.FindMembers(const ID: string): TIDDeclaration;
var
  sc: TScope;
begin
  sc := Self;
  repeat
    Result := sc.FindID(ID);
    if Assigned(Result) then
      Exit;
    sc := sc.FParent;
  until (sc = nil) or (sc.ScopeType <> stStruct);
end;

procedure TScope.FindMembers(const ID: string; var ADeclArray: TIDDeclArray);
var
  sc: TScope;
begin
  sc := Self;
  repeat
    var LDecl := sc.FindID(ID);
    if Assigned(LDecl) then
      ADeclArray := ADeclArray + [LDecl];
    sc := sc.FParent;
  until (sc = nil) or (sc.ScopeType <> stStruct);
end;

function TScope.GetScopeClass: TScopeClass;
begin
  Result := scInterface;
end;

procedure TScope.SetParent(const Value: TScope);
begin
  Assert(Parent <> Self);

  if Assigned(FParent) then
    FParent.RemoveChild(Self);

  FParent := Value;

  Value.AddChild(Self);
end;

function TScope.GetDeclArray(Recursively: Boolean): TIDDeclArray;
begin
  SetLength(Result, Count);
  for var LIndex := 0 to Count -1 do
    Result[LIndex] := Items[LIndex];

  if Recursively and Assigned(Parent) then
    Result := Parent.GetDeclArray(Recursively) + Result;
end;

function TScope.GetDeclNamesArray(Recursively: Boolean): TStrArray;
begin
  var Decls := GetDeclArray(Recursively);
  SetLength(Result, Length(Decls));
  for var i := 0 to Length(Decls) - 1 do
    Result[i] := Decls[i].Name;
end;

function TScope.GetDeclUnitName: string;
begin
  Result := fUnit.Name
end;

function TScope.GetName: string;
begin
  Result := FName;
end;

function TScope.GetNameEx: string;
begin
  Result := Format('%s[%d]', [Name, Count]);
end;

function TScope.GetParentNames: string;
begin
  if Assigned(Parent) then
    Result := Name + ' <-- ' + Parent.GetParentNames
  else
    Result := Name;
end;

{ TIDList }

function TIDList.FindID(const Identifier: string): TIDDeclaration;
var
  Node: PAVLNode;
begin
  Node := Find(Identifier);
  if Assigned(Node) then
    Result := TIDDeclaration(Node.Data)
  else
    Result := nil;
end;

function TIDList.InsertID(Item: TIDDeclaration): Boolean;
begin
  Result := InsertNode(Item.Name, Item) = nil;
end;

function TIDList.InsertIDAndReturnIfExist(Item: TIDDeclaration): TIDDeclaration;
var
  Node: PAVLNode;
begin
  Node := InsertNode(Item.Name, Item);
  if Assigned(Node) then
    Result := TIDDeclaration(Node.Data)
  else
    Result := nil;
end;

{ TIDDeclaration }

constructor TIDDeclaration.Create(Scope: TScope; const Identifier: TIdentifier);
begin
  Assert(Assigned(Scope));
  CreateFromPool;
  FScope := Scope;
  FID := Identifier;
  fModule := Scope.DeclUnit;
end;

constructor TIDDeclaration.CreateAsAnonymous(Scope: TScope);
begin
  Assert(Assigned(Scope));
  CreateFromPool;
  FScope := Scope;
  fModule := Scope.DeclUnit;
end;

constructor TIDDeclaration.CreateAsSystem(Scope: TScope; const Name: string);
begin
  Assert(Assigned(Scope));
  CreateFromPool;
  FScope := Scope;
  FID.Name := Name;
  fModule := Scope.DeclUnit;
  FSystemDecl := True;
end;

destructor TIDDeclaration.Destroy;
begin

  inherited;
end;

function TIDDeclaration.DoesGenericUseParams(const AParams: TIDTypeArray): Boolean;
begin
  Result := False;
end;

function TIDDeclaration.GetCValue: TIDConstant;
begin
  Result := nil;
end;

function TIDDeclaration.GetDataTypeID: TDataTypeID;
begin
  Result := FDataType.DataTypeID;
end;

function TIDDeclaration.GetDeclUnit: TASTModule;
begin
  Result := FScope.DeclUnit;
end;

function TIDDeclaration.GetDeclUnitName: string;
begin
  Result := FScope.DeclUnitName;
end;

function TIDDeclaration.GetIndex: Integer;
begin
  Result := FIndex;
end;

function TIDDeclaration.GetIsAnonymous: Boolean;
begin
  Result := (FID.Name = '');
end;

function TIDDeclaration.GetIsGeneric: Boolean;
begin
  Result := DataTypeID = dtGeneric;
end;

function TIDDeclaration.GetOriginalDecl: TIDDeclaration;
begin
  Result := self;
end;

function TIDDeclaration.GetPackage: IASTPascalProject;
var
  U: TASTDelphiUnit;
begin
  U := TASTDelphiUnit(Module);
  Assert(Assigned(U));
  Result := U.Package;
end;

function TIDDeclaration.GetUnitID: Integer;
begin
  if Assigned(FScope) then
    Result := TPascalUnit(FScope.DeclUnit).UnitID
  else begin
    AbortWorkInternal('Scope not assigned');
    Result := -1;
  end;
end;

function TIDDeclaration.Get_Unit: IASTModule;
begin
  Result := fModule;
end;

procedure TIDDeclaration.IncRefCount(RCPath: UInt32);
begin
  if FRCPath = RCPath then
    Exit;
  FRCPath := RCPath;
  Inc(FRefCount);
end;

function TIDDeclaration.InstantiateGeneric(ADstScope: TScope; ADstStruct: TIDStructure;
                                           const AContext: TGenericInstantiateContext): TIDDeclaration;
begin
  if IsGeneric then
    AbortWork('Declaration %s is not a generic', [DisplayName], TextPosition);

  Result := Self;
end;

function TIDDeclaration.MakeCopy: TIDDeclaration;
begin
  Result := TIDDeclarationClass(ClassType).Create(Scope, ID);
  Result.FDataType := FDataType;
  Result.FVisibility := FVisibility;
  Result.FIndex := FIndex;
  Result.FExportNameIndex := FExportNameIndex;
  Result.FRefCount := FRefCount;
  Result.FRCPath := FRCPath;
  Result.FNoOverride := FNoOverride;
  Result.FImportLib := FImportLib;
  Result.FImportName := FImportName;
  Result.fDepricatedExpression := fDepricatedExpression;
end;

procedure TIDDeclaration.DecRefCount(RCPath: UInt32);
begin
  if FRCPath = RCPath then
    Exit;
  FRCPath := RCPath;
  Dec(FRefCount);
end;

procedure TIDDeclaration.RemoveILReferences;
begin

end;

procedure TIDDeclaration.SetCValue(const Value: TIDConstant);
begin

end;

procedure TIDDeclaration.SetDataType(const Value: TIDType);
begin
  FDataType := Value;
end;

procedure TIDDeclaration.SetIndex(const Value: Integer);
begin
  FIndex := Value;
end;

{ TIDDeclarationGeneric }

procedure TIDDeclarationGeneric.AddGenecricOverload(ADecl: TIDDeclarationGeneric);
begin
  var LCurrent := Self;
  while Assigned(LCurrent.NextGenericOverload) do
     LCurrent := LCurrent.NextGenericOverload;

  LCurrent.fNextOverload := ADecl;
end;

procedure TIDDeclarationGeneric.GenericInstances2Str(ABuilder: TStringBuilder; ANestedLevel: Integer);
begin
  if Assigned(fGenericDescriptor) then
  begin
    for var LInstance in fGenericDescriptor.FGenericInstances do
    begin
      ABuilder.Append(sLineBreak);
      // ABuilder.Append(' ', ANestedLevel*2);
      // ABuilder.Append('// generic instase from pool:');
      // ABuilder.Append(sLineBreak);
      LInstance.Instance.Decl2Str(ABuilder, ANestedLevel);
    end;
  end;
end;

procedure TIDDeclarationGeneric.GenericOverloads2Str(ABuilder: TStringBuilder; ANestedLevel: Integer);
begin
  if Assigned(NextGenericOverload) then
  begin
    ABuilder.AppendLine;
    NextGenericOverload.Decl2Str(ABuilder, ANestedLevel);
  end;
end;

procedure TIDDeclarationGeneric.SetGenericDescriptor(const Value: PGenericDescriptor);
begin
  FGenericDescriptor := Value;
end;

{ TIDProcDeclaration }

procedure TIDProcedure.AddParam(const Param: TIDVariable);
begin
  var ACount := Length(FExplicitParams);
  SetLength(FExplicitParams, ACount + 1);
  FExplicitParams[ACount] := Param;
end;

function TIDProcedure.AddParam(const Name: string; DataType: TIDType): TIDParam;
begin
  Result := TIDParam.Create(ParamsScope, Identifier(Name));
  Result.DataType := DataType;
  AddParam(Result);
end;

function TIDProcedure.AddParam(const Name: string; DataType: TIDType; Flags: TVariableFlags; DefaultValue: TIDExpression = nil): TIDParam;
begin
  Result := TIDParam.Create(ParamsScope, Identifier(Name));
  Result.DataType := DataType;
  Result.Flags := Flags + [varParameter];
  Result.DefaultValue := DefaultValue;
  AddParam(Result);
end;

constructor TIDProcedure.Create(Scope: TScope; const ID: TIdentifier);
begin
  inherited Create(Scope, ID);
  FTempVars := TItemsStack.Create(4);
  FItemType := itProcedure;
end;

constructor TIDProcedure.CreateAsAnonymous(Scope: TScope);
begin
  inherited CreateAsAnonymous(Scope);
  FTempVars := TItemsStack.Create(4);
  FItemType := itProcedure;
end;

constructor TIDProcedure.CreateAsSystem(Scope: TScope; const Name: string);
begin
  inherited CreateAsSystem(Scope, Name);
  ItemType := itProcedure;
  FParamsScope := TProcScope.CreateInDecl(Scope, Self);
  FEntryScope := FParamsScope;
end;

constructor TIDProcedure.CreateAsSystemMethod(Struct: TIDStructure; const Name: string);
var
  SelfParam: TIDVariable;
begin
  inherited CreateAsSystem(Scope, Name);
  ItemType := itProcedure;
  FParamsScope := TMethodScope.CreateInDecl(Scope, Struct.Members, Self);
  if Struct.DataTypeID = dtRecord then
    SelfParam := TIDVariable.Create(FParamsScope, Identifier('Self'), Struct, [VarParameter, VarSelf, VarConst, VarInOut, VarHiddenParam])
  else
    SelfParam := TIDVariable.Create(FParamsScope, Identifier('Self'), Struct, [VarParameter, VarSelf, VarConst, VarHiddenParam]);

  FParamsScope.AddVariable(SelfParam);
  FEntryScope := FParamsScope;
end;

procedure TIDProcedure.CreateGenericDescriptor(const GenericParams: TIDTypeArray);
begin
  New(FGenericDescriptor);
  FGenericDescriptor.FScope := nil;
  FGenericDescriptor.FGenericParams := GenericParams;
end;

destructor TIDProcedure.Destroy;
begin
  FFinalSection.Free;
  if Assigned(FGenericDescriptor) then
    Dispose(FGenericDescriptor);
  inherited;
end;

function IsGenericParamsTheSame(AParam1, AParam2: TIDGenericParam): Boolean;
begin
  Result := SameText(AParam1.Name, AParam2.Name) and
            (AParam1.Constraint = AParam2.Constraint) and
            (AParam1.ConstraintType = AParam2.ConstraintType);
end;

function TIDProcedure.SameDeclaration(const AParams: TIDParamArray;
                                      AResultType: TIDType;
                                      ACheckNames: Boolean): Boolean;
var
  LCnt: Integer;
begin
  LCnt := Length(AParams);
  if LCnt <> Length(ExplicitParams) then
    Exit(False);

  for var LIndex := 0 to LCnt - 1 do
  begin
    var LParam1 := ExplicitParams[LIndex];
    var LParam2 := AParams[LIndex];
    if not SameProcSignTypes(LParam1.DataType, LParam2.DataType) or
       (ACheckNames and not SameText(LParam1.Name, LParam2.Name))
    then
      Exit(False);
  end;

  // treat a generic result as a parameter
  if (Assigned(ResultType) and not Assigned(AResultType) and ResultType.IsGeneric) or
     (not Assigned(ResultType) and Assigned(AResultType) and AResultType.IsGeneric) then
    Result := False
  else
    Result := True;
end;

procedure TIDProcedure.SetEntryScope(const Value: TProcScope);
begin
  FEntryScope := Value;
end;

procedure TIDProcedure.SetParameters(const Value: TIDParamArray);
begin
  if Assigned(FExplicitParams) then
    raise Exception.Create('Parameters alrady assigned!');
  FExplicitParams := Value;
end;

procedure TIDProcedure.SetResult(DataType: TIDType);
begin
  FResultParam := TIDParam.Create(EntryScope, Identifier('Result', ID.TextPosition));
  FResultParam.DataType := DataType;
  FResultParam.IncludeFlags([VarParameter, VarOut, VarHiddenParam, VarResult]);
  FResultType := DataType;
end;

procedure TIDProcedure.Warning(const Message: string; const Params: array of const; const TextPosition: TTextPosition);
begin
  TASTDelphiUnit(Module).Warning(Message, Params, TextPosition);
end;

function TIDProcedure.GetAllOverloadSignatures(const LineSeparator: string): string;
var
  Decl: TIDProcedure;
begin
  Result := DisplayName;
  Decl := PrevOverload;
  while Assigned(Decl) do begin
    Result := Result + LineSeparator + Decl.DisplayName;
    Decl := Decl.PrevOverload;
  end;
end;

function TIDProcedure.GetDebugVariables: string;
begin
  Result := GetVarDebugString(FEntryScope);
end;

function TIDProcedure.GetDefaultParamsCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Length(FExplicitParams) - 1 do
    if Assigned(FExplicitParams[i].DefaultValue) then
      Inc(Result);
end;

function GenericDescriptorAsText(Descriptor: PGenericDescriptor): string;
var
  i: Integer;
  ps: string;
begin
  ps := '';
  Result := '';
  for i := 0 to Length(Descriptor.FGenericParams) - 1 do
    ps := AddStringSegment(ps, Descriptor.FGenericParams[i].Name, ', ');
  Result := Result + '<' + ps + '>';
end;

function GenericArgsAsText(const AArguments: TIDExpressions): string; overload;
begin
  var LStr := '';
  for var LIndex := 0 to Length(AArguments) - 1 do
    LStr := AddStringSegment(LStr, AArguments[LIndex].DisplayName, ', ');
  Result := '<' + LStr + '>';
end;

function GenericArgsAsText(const AArguments: TIDTypeArray): string; overload;
begin
  var LStr := '';
  for var LIndex := 0 to Length(AArguments) - 1 do
    LStr := AddStringSegment(LStr, AArguments[LIndex].Name, ', ');
  Result := '<' + LStr + '>';
end;

function TIDProcedure.GetDisplayName: string;
var
  Param: TIDVariable;
  ParamsStr: string;
  i: Integer;
begin
  Result := Name;

  if Assigned(Struct) and not Struct.IsAnonymous then
    Result := Struct.DisplayName + '.' + Result;

  if Result = '' then
    Result := '$anonymous_proc_' + IntToStr(Index);

  if Assigned(FGenericDescriptor) then
    Result := Result + GenericDescriptorAsText(FGenericDescriptor);

  for i := 0 to Length(ExplicitParams) - 1 do
  begin
    Param := ExplicitParams[i];
    var LDataType := Param.DataType;
    if Assigned(LDataType) then
      ParamsStr := AddStringSegment(ParamsStr, LDataType.DisplayName, ', ')
    else
      ParamsStr := AddStringSegment(ParamsStr, '<unknown>', ', ');
  end;
  Result := Result + '(' + ParamsStr + ')';
  if Assigned(FResultType) then
    Result := Result + ': ' + FResultType.DisplayName;
end;

function TIDProcedure.GetIndex: Integer;
begin
  Result := MethodIndex;
end;

function TIDProcedure.GetIsCompleted: Boolean;
begin
  Result := pfCompleted in FProcFlags;
end;

function TIDProcedure.GetIsConstructor: Boolean;
begin
  Result := (pfConstructor in Flags);
end;

function TIDProcedure.GetIsGeneric: Boolean;
begin
  Result := Assigned(FGenericDescriptor);
end;

function TIDProcedure.GetIsDestructor: Boolean;
begin
  Result := (pfDestructor in Flags);
end;

function TIDProcedure.GetIsStatic: Boolean;
begin
  Result := (pfOperator in Flags) or (pfStatic in Flags);
end;

function TIDProcedure.GetIsClassMethod: Boolean;
begin
  Result := (pfClass in Flags);
end;

function TIDProcedure.GetParamsCount: Integer;
begin
  Result := Length(FExplicitParams);
end;

function TIDProcedure.GetProcKindName: string;
begin
  if pfOperator in Flags then
    Result := 'operator'
  else
  if Assigned(FResultType) then
    Result := 'function'
  else
    Result := 'procedure';

  if pfClass in Flags  then
    Result := 'class ' + Result;
end;

function TIDProcedure.GetResultParamExpression: TIDExpression;
begin
  if Assigned(ResultType) then
    Result := TIDExpression.Create(FResultParam)
  else
    Result := nil;
end;

procedure TIDProcedure.CreateProcedureTypeIfNeed(Scope: TScope);
begin
  if not Assigned(FDataType) then
  begin
    fDataType := TIDProcType.CreateAsAnonymous(Scope);
    TIDProcType(fDataType).fParams := ExplicitParams;
    TIDProcType(fDataType).fResultType := ResultType;
    if Assigned(Struct) then
      TIDProcType(fDataType).ProcClass := procMethod;
  end;
end;

function TIDProcedure.GetTMPRef(DataType: TIDType): TIDVariable;
begin
  Result := GetTMPVar(DataType, True);
  Result.FFlags := [VarInOut, VarTemporatyRef];
end;

function TIDProcedure.GetTMPVar(DataType: TIDType; VarFlags: TVariableFlags): TIDVariable;
begin
  Result := TIDVariable.CreateAsTemporary(EntryScope, DataType);
  Result.IncludeFlags(VarFlags);
  FTempVars.Push(Result);
end;

function TIDProcedure.GetTMPVar(DataType: TIDType; Reference: Boolean): TIDVariable;
begin
  if Reference then
    Result := GetTMPVar(DataType, [VarInOut])
  else
    Result := GetTMPVar(DataType, []);
end;

procedure TIDProcedure.Hint(const Message: string; const Params: array of const; const TextPosition: TTextPosition);
begin
  TASTDelphiUnit(Module).Hint(Message, Params, TextPosition);
end;

function TIDProcedure.GetMethodIndex: Integer;
var
  St: TIDStructure;
begin
  Result := FIndex;
  if not Assigned(Struct) then
    Exit;
  st := Struct.Ancestor;
  while Assigned(st) do begin
    Result := Result + St.MethodCount;
    st := St.Ancestor;
  end;
end;

procedure TIDProcedure.IncRefCount(RCPath: UInt32);
begin
  if FRCPath = RCPath then
    Exit;
  FRCPath := RCPath;
  Inc(FRefCount);
  IncTypesReadCountInSignature(RCPath);
end;

procedure TIDProcedure.Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer; AAppendName: Boolean);
begin
  if Assigned(PrevOverload) and (PrevOverload.Struct = Struct) then
  begin
    PrevOverload.Decl2Str(ABuilder, ANestedLevel, AAppendName);
    ABuilder.AppendLine;
  end;

  ABuilder.Append(' ', ANestedLevel*2);
  ABuilder.Append(ProcKindName);
  ABuilder.Append(' ');
  ABuilder.Append(Name);

  if Assigned(GenericDescriptor) then
    GenericDescriptor.Decl2Str(ABuilder);

  Params2Str(ABuilder, ExplicitParams);

  if Assigned(FResultType) then
  begin
    ABuilder.Append(': ');
    ABuilder.Append(FResultType.DisplayName);
  end;

  ABuilder.Append(';');

  if (pfVirtual in FProcFlags) and not (pfOverride in FProcFlags) then
    ABuilder.Append(' virtual;');

  if pfOveload in FProcFlags then
    ABuilder.Append(' overload;');

  if pfOverride in FProcFlags then
    ABuilder.Append(' override;');

  if pfAbstract in FProcFlags then
    ABuilder.Append(' abstract;');

  if pfStatic in FProcFlags then
    ABuilder.Append(' static;');

  if pfInline in FProcFlags then
    ABuilder.Append(' inline;');

  if CallConvention > ConvNative then
  begin
    ABuilder.Append(' ');
    ABuilder.Append(CallConventionToStr(CallConvention));
  end;

  GenericInstances2Str(ABuilder, ANestedLevel);

  GenericOverloads2Str(ABuilder, ANestedLevel);
end;

procedure TIDProcedure.DecRefCount(RCPath: UInt32);
begin
//  if FRCPath = RCPath then
//    Exit;
//  FRCPath := RCPath;
//  Dec(FRefCount);
//  DecTypesReadCountInSignature(RCPath);
end;

procedure TIDProcedure.DecTypesReadCountInSignature(RCPath: UInt32);
//var
//  Variable: TIDVariable;
begin
//  Variable := FVarSpace.First;
//  while Assigned(Variable) do
//  begin
//    if (VarParameter in Variable.Flags) and
//       (Variable.DataType <> FStruct) then
//      Variable.DataType.DecRefCount(RCPath);
//    Variable := TIDVariable(Variable.NextItem);
//  end;
//  if Assigned(ResultType) then
//    ResultType.DecRefCount(RCPath);
end;

procedure TIDProcedure.IncTypesReadCountInSignature(RCPath: UInt32);
//var
//  Variable: TIDVariable;
begin
//  Variable := FVarSpace.First;
//  while Assigned(Variable) do
//  begin
//    if (VarParameter in Variable.Flags) and
//       (Variable.DataType <> FStruct) then
//      Variable.DataType.IncRefCount(RCPath);
//    Variable := TIDVariable(Variable.NextItem);
//  end;
//  if Assigned(ResultType) then
//    ResultType.IncRefCount(RCPath);
end;

function TIDProcedure.InstantiateGenericProc(ADstScope: TScope; ADstStruct: TIDStructure;
                                             ANextOverload: TIDProcedure;
                                             const AContext: TGenericInstantiateContext): TIDDeclaration;

  function InstantiateParams(ANewScope: TScope;
                             const AProcParams: TIDParamArray;
                             const AContext: TGenericInstantiateContext): TIDParamArray;
  begin
    var AParamCount := Length(AProcParams);
    SetLength(Result, AParamCount);
    for var AIndex := 0 to AParamCount - 1 do
    begin
      var AParam := AProcParams[AIndex].MakeCopy as TIDParam;
      if AParam.IsGeneric then
        AParam.DataType := AParam.DataType.InstantiateGeneric(ANewScope, ADstStruct, AContext) as TIDType;

      Result[AIndex] := AParam;
    end;
  end;

begin
  LogBegin('inst [type: %s, src: %s]', [ClassName, Name]);

  var LNewProc := MakeCopy as TIDProcedure;

  LNewProc.FEntryScope := TProcScopeClass(EntryScope.ClassType).CreateInDecl(ADstScope, LNewProc);
  LNewProc.FParamsScope := LNewProc.FEntryScope;

  // todo: what about implicit params (self, open array len, etc)?
  LNewProc.ExplicitParams := InstantiateParams(ParamsScope, ExplicitParams, AContext);
  if Assigned(ResultType) then
    LNewProc.ResultType := ResultType.InstantiateGeneric(ADstScope, ADstStruct, AContext) as TIDType;

  LNewProc.FStruct := ADstStruct;
  LNewProc.FProcFlags := FProcFlags;
  LNewProc.FNextOverload := FNextOverload;
  LNewProc.FCallConv := FCallConv;
  LNewProc.FVirtualIndex := FVirtualIndex;
  LNewProc.FGenericDescriptor := nil;
  LNewProc.FGenericPrototype := nil;
  LNewProc.FFirstBodyLine := FFirstBodyLine;
  LNewProc.FLastBodyLine := FLastBodyLine;
  LNewProc.FFinalSection := FFinalSection;
  LNewProc.FInherited := FInherited;
  Result := LNewProc;

  if not Assigned(ADstStruct) then
    LNewProc.ID := AContext.DstID;

  // instantiate overloads
  if Assigned(PrevOverload) and (PrevOverload.Struct = Struct) then
  begin
    WriteLog('overload:', [ClassName, Result.Name]);
    LNewProc.PrevOverload := PrevOverload.InstantiateGenericProc(ADstScope, ADstStruct, LNewProc, AContext) as TIDProcedure;
  end;

  LogEnd('inst [type: %s, dst: %s]', [ClassName, Result.Name]);
end;

function TIDProcedure.InstantiateGeneric(ADstScope: TScope; ADstStruct: TIDStructure;
                                         const AContext: TGenericInstantiateContext): TIDDeclaration;
begin
  Result := InstantiateGenericProc(ADstScope, ADstStruct, nil, AContext);
end;

procedure TIDProcedure.MakeSelfParam;
begin
  Assert(Assigned(FStruct));
  var LSelfParam := TIDParam.Create(FParamsScope, Identifier('Self'), FStruct, [VarParameter, VarConst, VarHiddenParam]);
  FParamsScope.AddVariable(LSelfParam);
  FParamsScope.ParamsScope.SelfParam := LSelfParam;
end;

procedure TIDProcedure.RemoveILReferences(var RCPathCount: UInt32);
begin
//  TIL(FIL).RemoveReferences(RCPathCount);
end;

{ TIDTypeDeclaration }

function TIDType.BinarOperatorFor(const Op: TOperatorID; const Left: TIDType): TIDType;
var
  Ops: TBinaryOperatorsArray;
begin
  Ops := FBinarOperatorsFor[Op];
  for var AIndex := 0 to Length(Ops) - 1 do
  begin
    if Ops[AIndex].Left = Left then
      Exit(TIDType(Ops[AIndex].OpDecl));
  end;
  Result := nil;
end;

constructor TIDType.Create(Scope: TScope; const ID: TIdentifier);
begin
  inherited Create(Scope, ID);
  FItemType := itType;
  FImplicitsTo := TIDPairList.Create;
  FImplicitsFrom := TIDPairList.Create;
  FExplicitsTo := TIDPairList.Create;
  FDataType := SYSUnit._MetaType;
end;

constructor TIDType.CreateAsAnonymous(Scope: TScope);
begin
  inherited CreateAsAnonymous(Scope);
  FItemType := itType;
  FImplicitsTo := TIDPairList.Create;
  FImplicitsFrom := TIDPairList.Create;
  FExplicitsTo := TIDPairList.Create;
  FDataType := SYSUnit._MetaType;
end;

constructor TIDType.CreateAsBuiltin(AScope: TScope; const AName: string; ADataTypeID: TDataTypeID);
begin
  Create(AScope, TIdentifier.Make(AName));
  DataTypeID := ADataTypeID;
  AScope.AddType(Self);
end;

constructor TIDType.CreateAsSystem(Scope: TScope; const Name: string);
begin
  inherited CreateAsSystem(Scope, Name);
  FItemType := itType;
  FImplicitsTo := TIDPairList.Create;
  FImplicitsFrom := TIDPairList.Create;
  FExplicitsTo := TIDPairList.Create;
  FDataType := SYSUnit._MetaType;
end;

procedure TIDType.CreateStandardOperators;
begin

end;

procedure TIDType.Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer; AAppendName: Boolean);
begin
  if AAppendName then
  begin
    ABuilder.Append(' ', ANestedLevel*2);
    ABuilder.Append('type ');
    ABuilder.Append(Name);

    if Assigned(GenericDescriptor) then
      GenericDescriptor.Decl2Str(ABuilder);

    ABuilder.Append(' = ');

    if IsPacked then
      ABuilder.Append('packed ');
  end;
end;

destructor TIDType.Destroy;
var
  i: TOperatorID;
begin
  FImplicitsTo.Free;
  FImplicitsFrom.Free;

  FExplicitsTo.Free;
  FExplicitsFrom.Free;

  for i := opIn to High(TOperatorID)  do
  begin
    // todo: free
    //FBinarOperators[i].Free;
    //FBinarOperatorsFor[i].Free;
  end;
  if Assigned(FGenericDescriptor) then
    Dispose(FGenericDescriptor);
  inherited;
end;

function TIDType.GetExplicitOperatorFrom(const Source: TIDType): TIDDeclaration;
var
  Node: TIDPairList.PAVLNode;
begin
  if Assigned(FExplicitsFrom) then
  begin
    Node := FExplicitsFrom.Find(Source);
    if Assigned(Node) then
      Exit(TIDDeclaration(Node.Data));
  end;

  Result := SysExplicitFromAny;

  // run system implicit proc
  if not Assigned(Result) then
    if MatchExplicitFrom(Source) then
      Result := Self;
end;

function TIDType.GetExplicitOperatorTo(const Destination: TIDType): TIDDeclaration;
var
  Node: TIDPairList.PAVLNode;
begin
  Node := FExplicitsTo.Find(Destination);
  if Assigned(Node) then
    Exit(TIDDeclaration(Node.Data));

  Result := SysExplicitToAny;

  // run system implicit proc
  if not Assigned(Result) then
    if MatchExplicitTo(Destination) then
      Result := Destination;
end;

function TIDType.FindImplicitOperatorFrom(const Source: TIDType): TIDDeclaration;
var
  Node: TIDPairList.PAVLNode;
  Decl, Implicit: TIDDeclaration;
  TypeDecl: TIDType;
  SrcDTID: TDataTypeID;
  ImplicitFactor, MaxImplicitFactor: Integer;
begin
  Result := nil;
  Node := FImplicitsFrom.First;
  SrcDTID := Source.DataTypeID;
  MaxImplicitFactor := -1;
  while Assigned(Node) do begin
    Decl := TIDDeclaration(Node.Data);

    {if Decl.ItemType <> itProcedure then
      TypeDecl := TIDType(Decl)
    else
      TypeDecl := TIDProcedure(Decl).ExplicitParams[0].DataType;}

    if Decl.ItemType = itProcedure then
    begin
      TypeDecl := TIDProcedure(Decl).ExplicitParams[0].DataType;
      Implicit := Source.GetImplicitOperatorTo(TypeDecl);
      if not Assigned(Implicit) then
        Implicit := TypeDecl.GetImplicitOperatorFrom(Source);

      if Assigned(Implicit) then
      begin
        ImplicitFactor := ImplicitFactor2(SrcDTID, TypeDecl.DataTypeID);
        if ImplicitFactor > MaxImplicitFactor then
        begin
          MaxImplicitFactor := ImplicitFactor;
          if (Decl.ItemType = itType) and (Implicit.ItemType = itProcedure) then
            Result := Implicit
          else
            Result := Decl;
        end;
      end;
    end;
    Node := FImplicitsFrom.Next(Node);
  end;
end;

function TIDType.FindImplicitOperatorTo(const Destination: TIDType): TIDDeclaration;
var
  Node: TIDPairList.PAVLNode;
  Decl, Implicit: TIDDeclaration;
  TypeDecl: TIDType;
  DstDTID: TDataTypeID;
  ImplicitFactor, MaxImplicitFactor: Integer;
begin
  Result := nil;
  Node := FImplicitsTo.First;
  DstDTID := Destination.DataTypeID;
  MaxImplicitFactor := -1;
  while Assigned(Node) do begin
    Decl := TIDDeclaration(Node.Data);

{    if Decl.ItemType <> itProcedure then
      TypeDecl := TIDType(Decl)
    else
      TypeDecl := TIDProcedure(Decl).ExplicitParams[0].DataType;}

    if (Decl.ItemType = itProcedure) and (TIDProcedure(Decl).ParamsCount > 0) then
    begin
      TypeDecl := TIDProcedure(Decl).ExplicitParams[0].DataType;
      Implicit := TypeDecl.GetImplicitOperatorTo(Destination);
      if not Assigned(Implicit) then
        Implicit := Destination.GetImplicitOperatorFrom(TypeDecl);

      if Assigned(Implicit) then
      begin
        ImplicitFactor := ImplicitFactor2(TypeDecl.DataTypeID, DstDTID);
        if ImplicitFactor > MaxImplicitFactor then
        begin
          MaxImplicitFactor := ImplicitFactor;
          if (Decl.ItemType = itType) and (Implicit.ItemType = itProcedure) then
            Result := Implicit
          else
            Result := Decl;
        end;
      end;
    end;
    Node := FImplicitsTo.Next(Node);
  end;
end;

function TIDType.GetImplicitOperatorTo(const Destination: TIDType): TIDDeclaration;
var
  Node: TIDPairList.PAVLNode;
begin
  Node := FImplicitsTo.Find(Destination);
  if Assigned(Node) then
    Exit(TIDDeclaration(Node.Data));

  Result := SysImplicitToAny;

  // run system implicit proc
  if not Assigned(Result) then
    if MatchImplicitTo(Destination) then
      Result := Destination;
end;

function TIDType.GetImplicitOperatorFrom(const Source: TIDType): TIDDeclaration;
var
  Node: TIDPairList.PAVLNode;
begin
  Node := FImplicitsFrom.Find(Source);
  if Assigned(Node) then
    Exit(TIDDeclaration(Node.Data));

  Result := SysImplicitFromAny;

  // run system implicit proc
  if not Assigned(Result) then
    if MatchImplicitFrom(Source) then
      Result := Self;
end;

function TIDType.GetActualDataType: TIDType;
begin
  Result := Self;
end;

function TIDType.GetDataSize: Integer;
begin
  case DataTypeID of
    dtPAnsiChar,
    dtPWideChar,
    dtUntypedRef,
    dtPointer,
    dtWeakRef: Result := Package.PointerSize;
    dtNativeInt,
    dtNativeUInt: Result := Package.NativeIntSize;
  else
    Result := cDataTypeSizes[DataTypeID];
  end;
//  if Result <= 0 then
//    Assert(Result > 0);
end;

function TIDType.GetDefaultReference(Scope: TScope): TIDType;
begin
  if not Assigned(FDefaultReference) then
  begin
    FDefaultReference := TIDPointer.CreateAsAnonymous(Scope, Self);

  end;
  Result := FDefaultReference;
end;

function TIDType.GetDisplayName: string;
begin
//  if ID.Name <> '' then
//  begin
//    var LParent := Parent;
//    while Assigned(LParent) do
//    begin
//      Result := AddStringSegment(LParent.DisplayName, Result, '.');
//      LParent := LParent.Parent;
//    end;
//  end;

  Result := ID.Name;

  if Assigned(FGenericDescriptor) then
    Result := Result + GenericDescriptorAsText(FGenericDescriptor);
end;

function TIDType.GetIsReferenced: Boolean;
begin
  Result := IsDataTypeReferenced(FDataTypeID);
end;

function TIDType.GetIsGeneric: Boolean;
begin
  Result := (fDataTypeID = dtGeneric) or Assigned(fGenericDescriptor);
end;

function TIDType.GetIsInteger: Boolean;
begin
  Result := FDataTypeID in [dtInt8, dtInt16, dtInt32, dtInt64, dtUInt8, dtUInt16, dtUInt32, dtUInt64,
                            dtNativeInt, dtNativeUInt];
end;

function TIDType.GetIsFloat: Boolean;
begin
  Result := FDataTypeID in [dtFloat32, dtFloat64, dtFloat80, dtCurrency, dtComp];
end;

function TIDType.GetIsManaged: Boolean;
begin
  Result := cDataTypeManaged[DataTypeID];
end;

function TIDType.GetOperators(const Op: TOperatorID): TBinaryOperatorsArray;
begin
  Result := FBinarOperators[Op];
end;

function TIDType.GetOperatorsFor(const Op: TOperatorID): TBinaryOperatorsArray;
begin
  Result := FBinarOperatorsFor[Op];
end;

function TIDType.GetOrdinal: Boolean;
begin
  Result := False;
end;

function TIDType.GetParent: TIDType;
begin
  if Assigned(Scope) and (Scope.ScopeType = stStruct) then
    Result := TStructScope(Scope).Struct
  else
    Result := nil;
end;

function TIDType.InstantiateGeneric(ADstScope: TScope; ADstStruct: TIDStructure;
  const AContext: TGenericInstantiateContext): TIDDeclaration;
begin
  // first, try to find this type in the parent struct (if it's nested)
  if Assigned(Parent) and Assigned(ADstStruct) then
  begin
    Result := ADstStruct.Members.FindMembers(Name);
    if Assigned(Result) then
      Exit;
  end;
  if IsGeneric then
    Result := nil
  else
    Result := Self;
end;

function TIDType.MakeCopy: TIDDeclaration;
begin
  var LNewCopy := TIDType(inherited);
  LNewCopy.fDataTypeID := DataTypeID;
  LNewCopy.fPacked := fPacked;
  Result := LNewCopy;
end;

//function TIDType.MatchExplicitFrom(ASrc: TIDType): Boolean;
//begin
//  Result := False;
//end;
//
//function TIDType.MatchExplicitTo(ADst: TIDType): Boolean;
//begin
//  Result := False;
//end;
//
//function TIDType.MatchImplicitFrom(ASrc: TIDType): Boolean;
//begin
//  Result := False;
//end;
//
//function TIDType.MatchImplicitTo(ADst: TIDType): Boolean;
//begin
//  Result := False;
//end;

procedure ERROR_OPERATOR_ALREADY_OVERLOADED(Op: TOperatorID; Type1, Type2: TIDDeclaration; const Position: TTextPosition); overload;
begin
  AbortWorkInternal(sOperatorForTypesAlreadyOverloadedFmt,
                    [OperatorFullName(Op), Type1.DisplayName, Type2.DisplayName], Position);
end;

procedure ERROR_OPERATOR_ALREADY_OVERLOADED(Op: TOperatorID; Type1: TDataTypeID; Type2: TIDDeclaration; const Position: TTextPosition); overload;
begin
  AbortWorkInternal(sOperatorForTypesAlreadyOverloadedFmt,
                    [OperatorFullName(Op), GetDataTypeName(Type1), Type2.DisplayName], Position);
end;

procedure TIDType.OverloadUnarOperator(Op: TOperatorID; Destination: TIDDeclaration);
begin
  if Op > High(FUnarOperators) then
    AbortWorkInternal('Operator "%s" is not UNAR', [OperatorFullName(Op)]);

  if Assigned(FUnarOperators[Op]) then
    ERROR_OPERATOR_ALREADY_OVERLOADED(Op, Self, Destination, Destination.TextPosition);
  FUnarOperators[Op] := Destination;
end;

procedure TIDType.OverloadUnarOperator(Op: TOperatorID; Declaration: TIDOperator);
begin
  OverloadUnarOperator(Op, Declaration.ExplicitParams[0].DataType);
end;

procedure TIDType.OverloadBinarOperator2(Op: TOperatorID; Right: TIDType; Result: TIDDeclaration);
begin
  Assert(Assigned(Right));
  // todo: collect left, right, operator
  var LOperators := FBinarOperators[Op];
  for var AIndex := 0 to Length(LOperators) - 1 do
  begin
    var LItemPtr := PBinaryOperator(@LOperators[AIndex]);
    if (LItemPtr.Left = Self) and (LItemPtr.Right = Right) then
     // ERROR_OPERATOR_ALREADY_OVERLOADED(Op, Self, Right, Result.TextPosition);
  end;
  var LNewOperator: TBinaryOperator;
  LNewOperator.Left := Self;
  LNewOperator.Right := Right;
  LNewOperator.OpDecl := Result;
  FBinarOperators[Op] := LOperators + [LNewOperator];
end;

procedure TIDType.OverloadBinarOperatorFor(Op: TOperatorID; const Left: TIDType; const Result: TIDDeclaration);
begin
  Assert(Assigned(Left));
  // todo: collect left, right, operator
  var LOperators := FBinarOperators[Op];
  for var AIndex := 0 to Length(LOperators) - 1 do
  begin
    var LItemPtr := PBinaryOperator(@LOperators[AIndex]);
    if (LItemPtr.Left = Left) and (LItemPtr.Right = Self) then
      ERROR_OPERATOR_ALREADY_OVERLOADED(Op, Left, Self, Result.TextPosition);
  end;
  var LNewOperator: TBinaryOperator;
  LNewOperator.Left := Left;
  LNewOperator.Right := Self;
  LNewOperator.OpDecl := Result;
  FBinarOperators[Op] := LOperators + [LNewOperator];
end;

procedure TIDType.OverloadCmpOperator(AOpID: TOperatorID; ARight: TIDType);
begin
  OverloadBinarOperator2(AOpID, ARight, SYSUnit._Boolean);
end;

procedure TIDType.OverloadAllCmpOperators;
begin
  OverloadCmpOperator(opEqual, {ARight:} Self);
  OverloadCmpOperator(opEqual, {ARight:} Self);
  OverloadCmpOperator(opNotEqual, {ARight:} Self);
  OverloadCmpOperator(opLess, {ARight:} Self);
  OverloadCmpOperator(opLessOrEqual, {ARight:} Self);
  OverloadCmpOperator(opGreater, {ARight:} Self);
  OverloadCmpOperator(opGreaterOrEqual, {ARight:} Self);
end;

procedure TIDType.OverloadBinarOperator(Op: TOperatorID; Declaration: TIDOperator);
begin
  OverloadBinarOperator2(Op, Declaration.ExplicitParams[1].DataType, Declaration);
end;

procedure TIDType.OverloadExplicitTo(const Destination: TIDDeclaration);
begin
  if Assigned(FExplicitsTo.InsertNode(Destination, Destination)) then
    ERROR_OPERATOR_ALREADY_OVERLOADED(opExplicit, Self, Destination, TextPosition);
end;

procedure TIDType.OverloadExplicitTo(const Destination, Proc: TIDDeclaration);
begin
  if Assigned(FExplicitsTo.InsertNode(Destination, Proc)) then
    ERROR_OPERATOR_ALREADY_OVERLOADED(opExplicit, Self, Proc, TextPosition);
end;

procedure TIDType.OverloadExplicitFrom(const Source: TIDDeclaration);
begin
  if not Assigned(FExplicitsFrom) then
    FExplicitsFrom := TIDPairList.Create;

  if Assigned(FExplicitsFrom.InsertNode(Source, Source)) then
    ERROR_OPERATOR_ALREADY_OVERLOADED(opExplicit, Self, Source, TextPosition);
end;

procedure TIDType.OverloadExplicitFrom(const Source, Proc: TIDDeclaration);
begin
  if not Assigned(FExplicitsFrom) then
    FExplicitsFrom := TIDPairList.Create;

  if Assigned(FExplicitsFrom.InsertNode(Source, Proc)) then
    ERROR_OPERATOR_ALREADY_OVERLOADED(opExplicit, Source, Proc, TextPosition);
end;

procedure TIDType.OverloadExplicitFromAny(const Op: TIDOperator);
begin
//  if Assigned(fSysExplicitFromAny) then
//    ERROR_OPERATOR_ALREADY_OVERLOADED(opExplicit, Self, Op, TextPosition);

  fSysExplicitFromAny := Op;
end;

procedure TIDType.OverloadExplicitToAny(const Op: TIDOperator);
begin
//  if Assigned(fSysExplicitToAny) then
//    ERROR_OPERATOR_ALREADY_OVERLOADED(opExplicit, Self, Op, TextPosition);

  fSysExplicitToAny := Op;
end;

procedure TIDType.OverloadImplicitTo(const Destination: TIDDeclaration);
begin
  Assert(Assigned(Destination));
  if Assigned(FImplicitsTo.InsertNode(Destination, Destination)) then
    ERROR_OPERATOR_ALREADY_OVERLOADED(opImplicit, Self, Destination, TextPosition);
end;

procedure TIDType.OverloadImplicitTo(const Destination, Proc: TIDDeclaration);
var
  Node: TIDPairList.PAVLNode;
begin
  Assert(Assigned(Destination));
  Node := FImplicitsTo.InsertNode(Destination, Proc);
  if Assigned(Node) then
    ERROR_OPERATOR_ALREADY_OVERLOADED(opImplicit, Destination, Proc, Proc.TextPosition);
end;

procedure TIDType.OverloadSysImplicitsTo(const ADestTypes: array of TDataTypeID);
begin
  for var LDataTypeID in ADestTypes do
  begin
    var LDestType := SYSUnit.DataTypes[LDataTypeID];
    if Assigned(FImplicitsTo.InsertNode(LDestType, LDestType)) then
      ERROR_OPERATOR_ALREADY_OVERLOADED(opImplicit, Self, LDestType, TextPosition);
  end;
end;

procedure TIDType.OverloadImplicitFrom(const Source: TIDDeclaration);
begin
  Assert(Assigned(Source));
  if Assigned(FImplicitsFrom.InsertNode(Source, Source)) then
    ERROR_OPERATOR_ALREADY_OVERLOADED(opImplicit, Self, Source, TextPosition);
end;

procedure TIDType.OverloadImplicitFrom(const Source, Proc: TIDDeclaration);
begin
  Assert(Assigned(Source));
  if Assigned(FImplicitsFrom.InsertNode(Source, Proc)) then
    ERROR_OPERATOR_ALREADY_OVERLOADED(opImplicit, Source, Proc, Proc.TextPosition);
end;

procedure TIDType.OverloadImplicitToAny(const Op: TIDOperator);
begin
//  if Assigned(FSysImplicitToAny) then
//    ERROR_OPERATOR_ALREADY_OVERLOADED(opImplicit, Self, Op, TextPosition);

  FSysImplicitToAny := Op;
end;

procedure TIDType.OverloadImplicitFromAny(const Op: TIDOperator);
begin
  FSysImplicitFromAny := Op;
end;

function TIDType.MatchExplicitFrom(ASrc: TIDType): Boolean;
begin
  Result := False;
end;

function TIDType.MatchExplicitTo(ADst: TIDType): Boolean;
begin
  Result := False;
end;

function TIDType.MatchImplicitFrom(ASrc: TIDType): Boolean;
begin
  Result := False;
end;

function TIDType.MatchImplicitTo(ADst: TIDType): Boolean;
begin
  Result := False;
end;

function TIDType.UnarOperator(Op: TOperatorID; Right: TIDType): TIDType;
begin
  Result := TIDType(FUnarOperators[Op]);
end;

procedure TIDType.AddBinarySysOperator(Op: TOperatorID; Decl: TIDOperator);
begin
  if Assigned(fSysBinaryOperators[Op]) then
    ERROR_OPERATOR_ALREADY_OVERLOADED(Op, Self, Self, TTextPosition.Empty);

  fSysBinaryOperators[Op] := Decl;
end;

function TIDType.BinarOperator(Op: TOperatorID; Right: TIDType): TIDType;
begin
  var LOperators := FBinarOperators[Op];
  for var AIndex := 0 to Length(LOperators) - 1 do
  begin
    var LItemPtr := PBinaryOperator(@LOperators[AIndex]);
    if (LItemPtr.Left = Self) and (LItemPtr.Right = Right) then
      Exit(TIDtype(LItemPtr.OpDecl));
  end;
  Result := nil;
end;

{ TIDConstant }

function TIDConstant.AsUInt64: UInt64;
begin
  Result := UInt64(AsInt64);
end;

constructor TIDConstant.Create(Scope: TScope; const Identifier: TIdentifier);
begin
  inherited Create(Scope, Identifier);
  FItemType := itConst;
end;

function TIDConstant.FixedByteSize: Integer;
begin
  Result := FDataType.DataSize;
end;

function TIDConstant.GetCValue: TIDConstant;
begin
  Result := Self;
end;

function TIDConstant.GetDisplayName: string;
begin
  if ID.Name = '' then
    Result := AsString
  else
    Result := ID.Name;
end;

procedure TIDConstant.IncRefCount(RCPath: UInt32);
begin
  if FRCPath = RCPath then
    Exit;
  FRCPath := RCPath;
  Inc(FRefCount);
  FDataType.IncRefCount(RCPath);
end;

procedure TIDConstant.Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer; AAppendName: Boolean);
begin
  ABuilder.Append(' ', ANestedLevel*2);
  ABuilder.Append('const ');
  ABuilder.Append(DisplayName);
  if Assigned(FExplicitDataType) then
  begin
    ABuilder.Append(': ');
    ABuilder.Append(FExplicitDataType.DisplayName);
  end;
  ABuilder.Append(' = ');
  ABuilder.Append(AsString);
  ABuilder.Append(';');
end;

procedure TIDConstant.DecRefCount(RCPath: UInt32);
begin
  if FRCPath = RCPath then
    Exit;
  FRCPath := RCPath;
  Dec(FRefCount);
  FDataType.DecRefCount(RCPath);
end;

procedure TIDConstant.SetCValue(const Value: TIDConstant);
begin
  Self.AssignValue(Value);
end;

procedure TIDConstant.SetExplicitDataType(const Value: TIDType);
begin
  if Assigned(FExplicitDataType) then
    AbortWorkInternal('Constant explicit type already assigned');
  FExplicitDataType := Value;
end;

{ TIDXXXConstant<T> }

procedure TIDXXXConstant<T>.AssignValue(Source: TIDConstant);
begin
  FIndex := Source.Index;
  if Source.ClassType = ClassType then
  begin
    FValue := TIDXXXConstant<T>(Source).Value
  end else
    SetAsVariant(Source.AsVariant);
end;

constructor TIDXXXConstant<T>.Create(Scope: TScope; const Identifier: TIdentifier; DataType: TIDType; Value: T);
begin
  inherited Create(Scope, Identifier);
  FDataType := DataType;
  FValue := Value;
end;

constructor TIDXXXConstant<T>.CreateAsAnonymous(Scope: TScope; DataType: TIDType; Value: T);
begin
  inherited CreateAsAnonymous(Scope);
  FItemType := itConst;
  FDataType := DataType;
  FValue := Value;
end;

{ TIDVariableItem }

constructor TIDVariable.Create(Scope: TScope; const Name: TIdentifier; DataType: TIDType; Flags: TVariableFlags);
begin
  inherited Create(Scope, Name);
  FItemType := itVar;
  FDataType := DataType;
  FFlags := Flags;
end;

constructor TIDVariable.Create(Scope: TScope; const Identifier: TIdentifier);
begin
  inherited Create(Scope, Identifier);
  FItemType := itVar;
end;

constructor TIDVariable.CreateAsAnonymous(Scope: TScope);
begin
  CreateFromPool;
  FScope := Scope;
  FItemType := itVar;
end;

constructor TIDVariable.CreateAsSystem(Scope: TScope; const Name: string);
begin
  inherited CreateAsSystem(Scope, Name);
  FItemType := itVar;
end;

constructor TIDVariable.CreateAsTemporary(Scope: TScope; DataType: TIDType);
begin
  CreateFromPool;
  FScope := Scope;
  FItemType := itVar;
  FDataType := DataType;
  FFlags := [VarTemporatyVar];
end;

procedure TIDVariable.IncludeFlags(const Flags: TVariableFlags);
begin
  FFlags := FFlags + Flags;
end;

procedure TIDVariable.IncRefCount(RCPath: UInt32);
begin
  if FRCPath = RCPath then
    Exit;
  FRCPath := RCPath;
  Inc(FRefCount);
  if Assigned(FDataType) then
    FDataType.IncRefCount(RCPath);
end;

function TIDVariable.InstantiateGeneric(ADstScope: TScope; ADstStruct: TIDStructure;
                                        const AContext: TGenericInstantiateContext): TIDDeclaration;
begin
  LogBegin('inst [type: %s, src: %s]', [ClassName, DisplayName]);
  Result := MakeCopy;
  Result.DataType := DataType.InstantiateGeneric(ADstScope, ADstStruct, AContext) as TIDType;
  LogEnd('inst [type: %s, dst: %s]', [ClassName, Result.DisplayName]);
end;

function TIDVariable.MakeCopy: TIDDeclaration;
begin
  Result := inherited;
  Result.DataType := DataType;
  TIDVariable(Result).FFlags := FFlags;
  TIDVariable(Result).FAbsolute := FAbsolute;
  TIDVariable(Result).DefaultValue := DefaultValue;
end;

procedure TIDVariable.Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer; AAppendName: Boolean);
begin
  ABuilder.Append(' ', ANestedLevel*2);
  ABuilder.Append('var ');
  ABuilder.Append(DisplayName);
  ABuilder.Append(': ');
  TypeNameToString(DataType, ABuilder);

  if Assigned(DefaultValue) then
  begin
    ABuilder.Append(' := ');
    ABuilder.Append(DefaultValue.Text);
  end;

  ABuilder.Append(';');
end;

procedure TIDVariable.DecRefCount(RCPath: UInt32);
begin
  if FRCPath = RCPath then
    Exit;
  FRCPath := RCPath;
  Dec(FRefCount);
  if Assigned(FDataType) then
    FDataType.DecRefCount(RCPath);
end;

function TIDVariable.GetCValue: TIDConstant;
begin
  Result := FCValue;
end;

function TIDVariable.GetDisplayName: string;
begin
  if Name <> '' then
    Result := Name
  else
    Result := '$' + IntToStr(FIndex);
end;

function TIDVariable.GetIsAnyLocalVar: Boolean;
begin
  Result := (Scope.ScopeType = stLocal) and not Assigned(FAbsolute);
end;

function TIDVariable.GetIsField: Boolean;
begin
  Result := VarIsField in FFlags;
end;

function TIDVariable.GetIsGeneric: Boolean;
begin
  Result := DataType.IsGeneric;
end;

function TIDVariable.GetIsExplicit: Boolean;
begin
  Result := not (VarHiddenParam in FFlags);
end;

function TIDVariable.GetIsTemporary: Boolean;
begin
  Result := (VarTemporatyVar in FFlags) or (VarTemporatyRef in Flags);
end;

function TIDVariable.GetIsTemporaryOwner: Boolean;
begin
  Result := (VarTmpResOwner in Flags);
end;

function TIDVariable.GetLastRWInstruction: TObject;
begin
  Result := nil;
end;

function TIDVariable.GetReference: Boolean;
begin
  if VarResult in Flags then
    Result := FDataType.DataTypeID in [dtRecord, dtStaticArray, dtRange]
  else
    Result := (VarInOut in FFlags) or (VarOut in FFlags) or (VarConstRef in FFlags) or ((VarConst in FFlags) and (FDataType.DataTypeID = dtRecord));
end;

function TIDVariable.GetVarReference: Boolean;
begin
  if VarResult in Flags then
    Result := FDataType.DataTypeID in [dtRecord, dtStaticArray, dtRange]
  else
    Result := (VarInOut in FFlags) or (VarOut in FFlags);
end;

procedure TIDVariable.SaveAndIncludeFlags(const Flags: TVariableFlags; out PrevFlags: TVariableFlags);
begin
  PrevFlags := FFlags;
  FFlags := FFlags + Flags;
end;

procedure TIDVariable.SetCValue(const Value: TIDConstant);
begin
  FCValue := Value;
end;

procedure TIDVariable.SetDefaultValue(const Value: TIDExpression);
begin
  FDefaultValue := Value;
  if Assigned(Value) then
    Include(FFlags, VarHasDefault);
end;

constructor TIDXXXConstant<T>.CreateAsSystem(AScope: TScope; const AName: string; ADataType: TIDType);
begin
  inherited CreateAsSystem(AScope, AName);
  FDataType := ADataType;
  FItemType := itConst;
end;

constructor TIDXXXConstant<T>.CreateWithoutScope(DataType: TIDType; Value: T);
begin
  CreateFromPool;
  FScope := Scope;
  fModule := nil;
  FItemType := itConst;
  FDataType := DataType;
  FValue := Value;
end;

{ TIDExpression }

constructor TIDExpression.Create(Declaration: TIDDeclaration; const TextPosition: TTextPosition);
begin
  CreateFromPool;
  FDeclaration := Declaration;
  FTextPosition := TextPosition;
end;

constructor TIDExpression.Create(Declaration: TIDDeclaration; const TextLine: Integer);
begin
  CreateFromPool;
  FDeclaration := Declaration;
  FTextPosition.Row := TextLine;
end;

destructor TIDExpression.Destroy;
begin

  inherited;
end;

constructor TIDExpression.Create(Declaration: TIDDeclaration);
begin
  CreateFromPool;
  FDeclaration := Declaration;
end;

(*function DecStrToInt(Str: AnsiString; out Buf: PByte): cardinal;
var
  Neg: boolean;
  Raw: INT64;
begin
  Raw := 0;
  if not TryStrToInt64(Str, Raw) then
  begin
    Result := 0;
    Buf := nil;
  end
  else begin
    Result := 8;
    while PByte(@Raw)[Result - 1] = 0 do
      Dec(Result);
    GetMem(Buf, Result);
    Move(Raw, Buf[0], Result);
  end;
end;

function BinToInt(Str: AnsiString; out Buf: PByte): cardinal;
var
  I, J: Cardinal;
begin
  Result := 0;
//  Buf := nil;
  Result := Cardinal(Ceil(Length(Str) / 8));
  GetMem(Buf, Result);
  ZeroMemory(@Buf^, Result);
  J := 1;
  for I := Length(Str) downto 1 do
  begin
    if not (Str[I] in ['0', '1']) then raise Exception.Create('invalid string');
    PByte(Buf)[(J - 1) div 8] := PByte(Buf)[(J - 1) div 8] or
      ((1 shl ((J - 1) mod 8)) and (ifthen(Byte(Str[I]) - $30 = 1, $FF, 0)));
    inc(J);
  end;
end; *)

function TIDExpression.GetAsArrayConst: TIDDynArrayConstant;
begin
  Result := FDeclaration as TIDDynArrayConstant;
end;

function TIDExpression.GetAsBoolConst: TIDBooleanConstant;
begin
  Result := FDeclaration as TIDBooleanConstant;
end;

function TIDExpression.GetAsCharConst: TIDCharConstant;
begin
  Result := FDeclaration as TIDCharConstant;
end;

function TIDExpression.GetAsClosure: TIDClosure;
begin
  Result := FDeclaration as TIDClosure;
end;

function TIDExpression.GetAsConst: TIDConstant;
begin
  Result := FDeclaration as TIDConstant;
end;

function TIDExpression.GetAsDynArrayConst: TIDDynArrayConstant;
begin
  Result := FDeclaration as TIDDynArrayConstant;
end;

function TIDExpression.GetAsIntConst: TIDIntConstant;
begin
  Result := FDeclaration as TIDIntConstant;
end;

function TIDExpression.GetAsProcedure: TIDProcedure;
begin
  Result := FDeclaration as TIDProcedure;
end;

function TIDExpression.GetAsProperty: TIDProperty;
begin
  Result := FDeclaration as TIDProperty;
end;

function TIDExpression.GetAsRangeConst: TIDRangeConstant;
begin
  Result := FDeclaration as TIDRangeConstant;
end;

function TIDExpression.GetAsStrConst: TIDStringConstant;
begin
  Result := FDeclaration as TIDStringConstant;
end;

function TIDExpression.GetAsType: TIDType;
begin
  Result := FDeclaration as TIDType;
end;

function TIDExpression.GetAsUnit: TIDUnit;
begin
  Result := fDeclaration as TIDUnit;
end;

function TIDExpression.GetAsVariable: TIDVariable;
begin
  Result := FDeclaration as TIDVariable;
end;

function TIDExpression.GetCValue: TIDConstant;
begin
  Result := FDeclaration.CValue;
end;

function TIDExpression.GetDataType: TIDType;
begin
  Result := FDeclaration.DataType.ActualDataType;
  //Assert(Assigned(Result));
end;

function TIDExpression.GetDataTypeID: TDataTypeID;
begin
  Result := GetDataType.DataTypeID;
end;

function TIDExpression.GetDataTypeName: string;
var
  Dt: TIDType;
begin
  Dt := GetDataType();
  if Assigned(Dt) then
    Result := Dt.DisplayName
  else
    Result := '<NULL>';
end;

function TIDExpression.GetDeclClass: TIDDeclarationClass;
begin
  Result := TIDDeclarationClass(fDeclaration.ClassType);
end;

function TIDExpression.GetDisplayName: string;
begin
  Result := FDeclaration.DisplayName;
end;

class function TIDExpression.GetExpressionType: TExpressonType;
begin
  Result := etDeclaration;
end;

function TIDExpression.GetIsAnonymous: Boolean;
begin
  Result := FDeclaration.ID.Name = '';
end;

function TIDExpression.GetIsAnonymousRef: Boolean;
begin
  Result := (FDeclaration.ItemType = itVar) and (VarTemporatyRef in TIDVariable(FDeclaration).FFlags);
end;

function TIDExpression.GetIsAnonymousVar: Boolean;
begin
  Result := (FDeclaration.ItemType = itVar) and (VarTemporatyVar in TIDVariable(FDeclaration).FFlags);
end;

function TIDExpression.GetIsAnyLocalVar: Boolean;
begin
  Result := (FDeclaration.ItemType = itVar) and TIDVariable(FDeclaration).IsAnyLocalVar;
end;

function TIDExpression.GetIsConstant: Boolean;
begin
  Result := (FDeclaration.ItemType = itConst);
  if (FDeclaration.ClassType = TIDRangeConstant) then with TIDRangeConstant(FDeclaration).Value do
    Result := LBExpression.IsConstant and HBExpression.IsConstant;
end;

function TIDExpression.GetIsLocalVar: Boolean;
begin
  Result := (FDeclaration.ItemType = itVar) and (FDeclaration.Scope.ScopeType = stLocal) and (FDeclaration.ID.Name <> '') and not Assigned(TIDVariable(FDeclaration).Absolute);
end;

function TIDExpression.GetIsNonAnonimousVariable: Boolean;
begin
  Result := (FDeclaration.ItemType = itVar) and
            (FDeclaration.ID.Name <> '');
end;

function TIDExpression.GetIsParam: Boolean;
begin
  Result := FDeclaration is TIDParam;
end;

function TIDExpression.GetIsProcedure: Boolean;
begin
  Result := (FDeclaration.ItemType = itProcedure);
end;

function TIDExpression.GetIsRangeConst: Boolean;
begin
  Result := fDeclaration.ClassType = TIDRangeConstant;
end;

function TIDExpression.GetIsTMPRef: Boolean;
begin
  Result := (FDeclaration.ItemType = itVar) and (FDeclaration.Name = '') and (TIDVariable(FDeclaration).Reference);
end;

function TIDExpression.GetIsTMPVar: Boolean;
begin
  Result := (FDeclaration.ItemType = itVar) and (TIDVariable(FDeclaration).IsTemporary);
end;

function TIDExpression.GetIsVariable: Boolean;
begin
  Result := FDeclaration.ItemType = itVar;
end;

function TIDExpression.GetDynArrayConst: Boolean;
begin
  Result := Assigned(FDeclaration) and (FDeclaration.ClassType = TIDDynArrayConstant);
end;

function TIDExpression.GetItemType: TIDItemType;
begin
  Result := FDeclaration.ItemType;
end;

function TIDExpression.GetLine: Integer;
begin
  Result := FTextPosition.Row;
end;

function TIDExpression.GetText: string;
begin
  Result := DisplayName;
end;

procedure TIDExpression.SetCValue(Value: TIDConstant);
begin
  FDeclaration.CValue := Value;
end;


{ TIDOperator }

constructor TIDOperator.Create(Scope: TScope; const ID: TIdentifier; &Operator: TOperatorID);
begin
  inherited Create(Scope, ID);
  FOperator := &Operator;
end;

constructor TIDOperator.CreateAsSystem(Scope: TScope);
begin
  inherited CreateAsSystem(Scope, '');
  ItemType := itSysOperator;
end;

function TIDOperator.GetRightOperand: TIDType;
begin
  Result := ExplicitParams[0].DataType;
end;

{ TItemStack }

constructor TItemsStack.Create(DefaultPoolSize: Integer);
begin
  SetLength(FItems, DefaultPoolSize);
  FCapasity := DefaultPoolSize;
  FCount := 0;
end;

procedure TItemsStack.Push(const Item: TObject);
begin
  if FCount >= FCapasity then begin
    SetLength(FItems, FCapasity + 64);
    Inc(FCapasity, 64);
  end;
  FItems[FCount] := Item;
  Inc(FCount);
end;

procedure TItemsStack.SetCount(Value: Integer);
begin
  FCount := Value;
end;

{ TIDStructType }

function TIDStructure.AddField(const Name: string; DataType: TIDType): TIDField;
begin
  Result := TIDField.Create(Self, Identifier(Name));
  Result.DataType := DataType;
  FMembers.AddVariable(Result);
end;

procedure TIDStructure.AncestorsDecl2Str(ABuilder: TStringBuilder);
begin
  if Assigned(fAncestor) then
  begin
    ABuilder.Append('(');
    ABuilder.Append(fAncestor.Name);
    ABuilder.Append(')');
  end;
end;

constructor TIDStructure.Create(Scope: TScope; const Name: TIdentifier);
begin
  inherited Create(Scope, Name);
  DoCreateStructure;
end;

constructor TIDStructure.CreateAsAnonymous(Scope: TScope);
begin
  inherited CreateAsAnonymous(Scope);
  DoCreateStructure;
end;

constructor TIDStructure.CreateAsSystem(Scope: TScope; const Name: string);
begin
  inherited;
  DoCreateStructure;
end;

function TIDStructure.FindField(const Name: string): TIDField;
begin
  var Decl := FMembers.FindMembers(Name);
  if Decl is TIDField then
    Result := TIDField(Decl)
  else
    Result := nil;
end;

function TIDStructure.FindMethod(const Name: string): TIDProcedure;
begin
  var Decl := FMembers.FindMembers(Name);
  if Decl is TIDProcedure then
    Result := TIDProcedure(Decl)
  else
    Result := nil;
end;

function TIDStructure.FindProperty(const Name: string): TIDProperty;
begin
  var Decl := FMembers.FindMembers(Name);
  if Decl is TIDProperty then
    Result := TIDProperty(Decl)
  else
    Result := nil;
end;

function TIDStructure.FindVirtualProc(AProc: TIDProcedure): TIDProcedure;
begin
  var LDecl := fMembers.FindMembers(AProc.Name);
  while Assigned(LDecl) do
  begin
    if (LDecl.ItemType = itProcedure) then
    begin
      var LMethod := TIDProcedure(LDecl);
      if (pfVirtual in LMethod.Flags) and
         (LMethod.SameDeclaration(AProc.ExplicitParams, AProc.ResultType)) and
          LMethod.IsClassMethod = AProc.IsClassMethod then
      begin
        var LResultType1 := LMethod.ResultType;
        var LResultType2 := AProc.ResultType;
        if (
             Assigned(LResultType1) and
             Assigned(LResultType2) and
             SameTypes(LResultType1, LResultType2)
           ) or
           (
             not Assigned(LResultType1) and not Assigned(LResultType2)
           )
        then
          Exit(TIDProcedure(LDecl));
      end;

      LDecl := TIDProcedure(LDecl).PrevOverload;
    end else
      Exit(nil);
  end;

  if Assigned(FAncestor) then
    Result := FAncestor.FindVirtualProc(AProc)
  else
    Result := nil;
end;

function TIDStructure.FindVirtualProcInAncestor(Proc: TIDProcedure): TIDProcedure;
begin
  if Assigned(FAncestor) then
    Result := FAncestor.FindVirtualProc(Proc)
  else
    Result := nil;
end;

function TIDStructure.GetClassOfType: TIDClassOf;
begin
  if not Assigned(FClassOfType) then
    FClassOfType := TIDClassOf.CreateAsAnonymous(FMembers, Self);
  Result := FClassOfType;
end;

function TIDStructure.GetDataSize: Integer;
begin
  Result := 0;
  for var LIndex := 0 to fMembers.Count - 1 do
  begin
    var LDecl := fMembers.Items[LIndex];
    if LDecl.ItemType = itVar then
    begin
      var LSize := TIDVariable(LDecl).DataType.DataSize;
      if LSize = -1 then
        Exit(-1);
      Result := Result + LSize;
    end;
  end;
end;

function TIDStructure.GetDefaultProperty: TIDProperty;
begin
  Result := FDefaultProperty;
  if not Assigned(Result) and Assigned(Ancestor) then
    Result := Ancestor.DefaultProperty;
end;

function TIDStructure.GetDisplayName: string;
var
  DTIDName: string;
begin
  if ID.Name = '' then
  begin
    case DataTypeID of
      dtRecord: DTIDName := 'record ';
      dtClass: DTIDName := 'class ';
    else
      DTIDName := 'unknown '
    end;

    Result := inherited GetDisplayName + DTIDName + ' end';
  end else
    Result := inherited GetDisplayName;
end;

function TIDStructure.GetExtraFlags: Byte;
begin
  Result := 0;
end;

function TIDStructure.GetFieldsCount: Integer;
begin
  Result := fMembers.VarCount;
end;

function TIDStructure.GetHasInitFiels: Boolean;
begin
  for var LIndex := 0 to fMembers.Count - 1 do
  begin
    var LDecl := fMembers.Items[LIndex];
    if (LDecl.ItemType = itVar) and Assigned(TIDVariable(LDecl).DefaultValue) then
      Exit(True);
  end;
  Result := False;
end;

function TIDStructure.GetIsGeneric: Boolean;
begin
  Result := Assigned(fGenericDescriptor);
end;

function TIDStructure.GetIsManaged: Boolean;
begin
  for var LIndex := 0 to fMembers.Count - 1 do
  begin
    var LDecl := fMembers.Items[LIndex];
    if (LDecl.ItemType = itVar) and TIDVariable(LDecl).DataType.Managed then
      Exit(True);
  end;
  Result := False;
end;

function TIDStructure.GetMethodCount: Integer;
begin
  Result := fMembers.ProcCount;
end;

function TIDStructure.GetStructKeyword: string;
begin
  Result := Format('<unknown struct keyword: %s>', [ClassName]);
end;

procedure TIDStructure.IncRefCount(RCPath: UInt32);
//var
//  Fld: TIDVariable;
//  Proc: TIDProcedure;
begin
//  if FRCPath = RCPath then
//    Exit;
//  FRCPath := RCPath;
//
//  Inc(FRefCount);
//  // проставляем зависимости предку
//  if Assigned(FAncestor) then
//    FAncestor.IncRefCount(RCPath);
//  // проставляем зависимости по типам полей
//  Fld := FVarSpace.First;
//  while Assigned(Fld) do
//  begin
//    CheckVarTypeAssigned(Fld);
//    Fld.DataType.IncRefCount(RCPath);
//    Fld := TIDVariable(Fld.NextItem);
//  end;
//  // проставляем зависимости сигнатурам методов
//  Proc := FProcSpace.First;
//  while Assigned(Proc) do
//  begin
//    Proc.IncTypesReadCountInSignature(RCPath);
//    Proc := TIDProcedure(Proc.NextItem);
//  end;
end;

procedure TIDStructure.InstantiateGenericAncestors(ADstScope: TScope; ADstStruct: TIDStructure;
                                                   const AContext: TGenericInstantiateContext);
begin
  if Assigned(AncestorDecl) then
  begin
    // use actual original scope as for generic ancestors
    var LParentScope := AncestorDecl.ActualDataType.Scope;
    ADstStruct.AncestorDecl := AncestorDecl.InstantiateGeneric(LParentScope, nil, AContext) as TIDType;
  end;
end;

function TIDStructure.InstantiateGeneric(ADstScope: TScope; ADstStruct: TIDStructure;
                                         const AContext: TGenericInstantiateContext): TIDDeclaration;
begin
  // first, try to find this type in the parent struct (if it's nested)
  if Assigned(Parent) and Assigned(ADstStruct) then
  begin
    Result := ADstStruct.Members.FindMembers(Name);
    if Assigned(Result) then
      Exit;
  end;

  if IsGeneric or (Assigned(ADstStruct) and DoesGenericUseParams(AContext.Params)) then
  begin
    if not Assigned(GenericDescriptor) or
       not GenericDescriptor.TryGetInstance(AContext.Args, {out} Result) then
    begin
      LogBegin('inst [type: %s, src: %s]', [ClassName, DisplayName]);

      // create a new instance if generic type and add to the pool first
      var LNewStruct := TIDStructure(MakeCopy);

      if Assigned(GenericDescriptor)  then
      begin
        // override name according to the generic arguments
        LNewStruct.ID := AContext.DstID;
        // add this instance to the its pool
        GenericDescriptor.AddGenericInstance(LNewStruct, AContext.Args);
      end;

      LNewStruct.GenericOrigin := Self;
      LNewStruct.fStrucFlags := fStrucFlags;

      // instantiate ancestor(and implemented interfaces)
      InstantiateGenericAncestors(ADstScope, LNewStruct, AContext);

      // instantiate struct members
      for var LIndex := 0 to fMembers.Count - 1 do
      begin
        var LMember := fMembers.Items[LIndex];
        // ignore generic params (they are not needed in the new instance)
        if not (LMember is TIDGenericParam) then
        begin
          var LNewMember := LMember.InstantiateGeneric(LNewStruct.Members, LNewStruct, AContext);
          case LMember.ItemType of
            itVar: LNewStruct.Members.AddVariable(TIDVariable(LNewMember));
            itProcedure: LNewStruct.Members.AddProcedure(TIDProcedure(LNewMember));
            itProperty: LNewStruct.Members.AddProperty(TIDProperty(LNewMember));
            itConst: LNewStruct.Members.AddConstant(TIDConstant(LNewMember));
            itType: LNewStruct.Members.AddType(TIDType(LNewMember));
            // todo: AddConst (need implement SizeOf(T)), AddOperators,
          end;
        end;
      end;

      // set default indexed propery (if exists)
      if Assigned(DefaultProperty) then
        LNewStruct.DefaultProperty := LNewStruct.FindProperty(DefaultProperty.Name);

      Result := LNewStruct;

      LogEnd('inst [type: %s, dst: %s]', [ClassName, LNewStruct.DisplayName]);
    end;
  end else
    Result := Self;
end;

procedure TIDStructure.Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer; AAppendName: Boolean);
begin
  inherited;
  ABuilder.Append(GetStructKeyword);

  AncestorsDecl2Str(ABuilder);

  if not NeedForward then
  begin
    for var LIndex := 0  to fMembers.Count - 1 do
    begin
      var LDecl := fMembers.Items[LIndex];
      if not (LDecl is TIDGenericParam) then
      begin
        ABuilder.Append(sLineBreak);
        LDecl.Decl2Str(ABuilder, ANestedLevel + 1);
      end;
    end;

    ABuilder.Append(sLineBreak);
    ABuilder.Append(' ', ANestedLevel*2);
    ABuilder.Append('end');
  end else
    ABuilder.Append(';');

  GenericInstances2Str(ABuilder, ANestedLevel);

  GenericOverloads2Str(ABuilder, ANestedLevel);
end;

procedure TIDStructure.DecRefCount(RCPath: UInt32);
//var
//  Fld: TIDVariable;
//  Proc: TIDProcedure;
begin
//  if FRCPath = RCPath then
//    Exit;
//  FRCPath := RCPath;
//  Dec(FRefCount);
//
//  // проставляем зависимости предку
//  if Assigned(FAncestor) then
//    FAncestor.DecRefCount(RCPath);
//
//  // проставляем зависимости по типам полей
//  Fld := FVarSpace.First;
//  while Assigned(Fld) do
//  begin
//    Fld.DataType.DecRefCount(RCPath);
//    Fld := TIDVariable(Fld.NextItem);
//  end;
//  // проставляем зависимости сигнатурам методов
//  Proc := FProcSpace.First;
//  while Assigned(Proc) do
//  begin
//    Proc.DecTypesReadCountInSignature(RCPath);
//    Proc := TIDProcedure(Proc.NextItem);
//  end;
end;

procedure TIDStructure.DoCreateStructure;
begin
  fMembers := TStructScope.CreateAsStruct(Scope, Self, Scope.DeclUnit);
  {$IFDEF DEBUG}fMembers.Name := ID.Name + '.members';{$ENDIF}
  fOperators := TStructScope.CreateAsStruct(FMembers, Self, Scope.DeclUnit);
  {$IFDEF DEBUG}fOperators.Name := ID.Name + '.operators';{$ENDIF}
end;

function TIDStructure.DoesGenericUseParams(const AParams: TIDTypeArray): Boolean;

  function IsUsed(AType: TIDType): Boolean;
  begin
    for var LParamIndex := 0 to Length(AParams) - 1 do
      if (AParams[LParamIndex] = AType) or
          AType.DoesGenericUseParams(AParams) then
        Exit(True);
    Result := False;
  end;

begin
  if Assigned(Parent) then
  begin
    // iterate through all members, to find at least one that uses outer generic params
    for var LIndex := 0 to fMembers.Count - 1 do
    begin
      var LMember := fMembers.Items[LIndex];
      case LMember.ItemType of
        itVar: if IsUsed((LMember as TIDField).DataType) then Exit(True);
        itProcedure: if LMember.DoesGenericUseParams(AParams) then Exit(True);
        itType: if LMember.DoesGenericUseParams(AParams) then Exit(True);
        itProperty: if LMember.DoesGenericUseParams(AParams) then Exit(True);
      end;
    end;
  end;
  Result := False;
end;

function TIDStructure.GetEnumeratorSupported(out ACurrentProp: TIDProperty): Boolean;
begin
  var LGetEnumeratorFunc := FindMethod('GetEnumerator');
  if Assigned(LGetEnumeratorFunc) then
  begin
    var LEnumeratorType := LGetEnumeratorFunc.ResultType.Original as TIDStructure;
    if Assigned(LEnumeratorType) and (LEnumeratorType is TIDStructure) then
    begin
      var LMoveNext := LEnumeratorType.FindMethod('MoveNext');
      var LCurrent := LEnumeratorType.FindProperty('Current');

      Result := (LMoveNext.ResultType = SYSUnit._Boolean) and Assigned(LCurrent.Getter);
      if Result then
      begin
        // todo: return all needed declarations to the caller
        ACurrentProp := LCurrent;
        Exit;
      end;
    end;
  end;
  Result := False;
end;

function TIDStructure.IsInheritsForm(Ancestor: TIDStructure): Boolean;
var
  AC: TIDStructure;
begin
  Result := (Self = Ancestor);
  if not Result then
  begin
    AC := FAncestor;
    while Assigned(AC) do begin
      if AC = Ancestor then
        Exit(True);
      AC := AC.Ancestor;
    end;
    Result := False;
  end;
end;

function TIDProcedure.GetSelfParam: TIDVariable;
begin
  if not Assigned(FStruct) then
    AbortWorkInternal('Self param is not found');

  Result := FParamsScope.ParamsScope.SelfParam;
end;

function TIDProcedure.GetSelfParamExpression: TIDExpression;
begin
  Result := TIDExpression.Create(GetSelfParam);
end;

procedure TIDStructure.SetAncestorDecl(const Value: TIDType);
begin
  fAncestorDecl := Value;
  if Assigned(Value) then
  begin
    fAncestor := Value.Original as TIDStructure;
    fMembers.fAncestorScope := fAncestor.Members;
  end else
    fAncestor := nil;
end;

procedure TIDStructure.SetGenericDescriptor(const Value: PGenericDescriptor);
begin
  FGenericDescriptor := Value;
  if Assigned(Value) then
  begin
    var AGenericParam := Value.Scope.First;
    while Assigned(AGenericParam) do
    begin
      fMembers.InsertID(TIDdeclaration(AGenericParam.Data));
      AGenericParam := Value.Scope.Next(AGenericParam);
    end;
  end;
end;

{ TIDIntConstant }

function TIDIntConstant.AsInt64: Int64;
begin
  Result := Value;
end;

function TIDIntConstant.AsString: string;
begin
  Result := IntToStr(Value);
end;

function TIDIntConstant.AsVariant: Variant;
begin
  Result := FValue;
end;

function TIDIntConstant.ValueByteSize: Integer;
begin
  Result := GetValueByteSize(Value);
end;

function TIDIntConstant.ValueDataType: TDataTypeID;
begin
  Result := GetValueDataType(Value);
end;

function TIDIntConstant.CompareTo(Constant: TIDConstant): Integer;
begin
  if ClassType = Constant.ClassType then
  begin
    if Value > TIDIntConstant(Constant).Value then
      Exit(1);
    if Value < TIDIntConstant(Constant).Value then
      Exit(-1);
    Exit(0);
  end else
    Result := -1;
end;

procedure TIDIntConstant.SetAsVariant(const AValue: Variant);
begin
  FValue := AValue;
end;

{ TIDFloatConstant }

function TIDFloatConstant.AsInt64: Int64;
begin
  if FDataType.DataTypeID = dtFloat32 then
    Result := Float32ToInt32(Value)
  else
    Result := Float64ToInt64(Value);
end;

function TIDFloatConstant.AsString: string;
begin
  Result := FloatToStr(Value);
end;

function TIDFloatConstant.AsVariant: Variant;
begin
  Result := FValue;
end;

function TIDFloatConstant.ValueByteSize: Integer;
begin
  if (Value > MaxFloat32) or (Abs(Value) < MinFloat32) then
    Result := 8
  else
    Result := 4;
end;

function TIDFloatConstant.ValueDataType: TDataTypeID;
begin
  Result := GetValueDataType(Value);
end;

function TIDFloatConstant.CompareTo(Constant: TIDConstant): Integer;
begin
  if ClassType = Constant.ClassType then
  begin
    if Value = TIDFloatConstant(Constant).Value then
      Result := 0
    else
      Result := IfThen(Value > TIDFloatConstant(Constant).Value, 1, -1);
  end else
    Result := -1;
end;

procedure TIDFloatConstant.SetAsVariant(const AValue: Variant);
begin
  FValue := AValue;
end;

{ TIDStringConstant }

function TIDStringConstant.AsInt64: Int64;
begin
  raise Exception.Create('Cannot convert string constant into Int64');
end;

function TIDStringConstant.AsString: string;
begin
  Result := Value;
end;

function TIDStringConstant.AsVariant: Variant;
begin
  Result := FValue;
end;

function TIDStringConstant.ValueByteSize: Integer;
begin
  Result := ByteLength(Value);
end;

function TIDStringConstant.ValueDataType: TDataTypeID;
begin
  Result := dtString;
end;

function TIDStringConstant.CompareTo(Constant: TIDConstant): Integer;
begin
  if ClassType = Constant.ClassType then
    Result :=  AnsiCompareStr(Value, TIDStringConstant(Constant).Value)
  else
    Result := -1;
end;

procedure TIDStringConstant.SetAsVariant(const AValue: Variant);
begin
  FValue := AValue;
end;

function TIDStringConstant.StrLength: Integer;
begin
  Result := Length(Value);
end;

{ TIDCharConstant }

function TIDCharConstant.AsInt64: Int64;
begin
  Result := Int64(Value);
end;

function TIDCharConstant.AsString: string;
begin
  Result := Value;
end;

function TIDCharConstant.AsVariant: Variant;
begin
  Result := FValue;
end;

function TIDCharConstant.ValueByteSize: Integer;
begin
  Result := 2;//GetDataTypeSize(ValueDataType);
end;

function TIDCharConstant.ValueDataType: TDataTypeID;
begin
  Result := dtChar;// GetValueDataType(Value)
end;

function TIDCharConstant.CompareTo(Constant: TIDConstant): Integer;
begin
  if ClassType = Constant.ClassType then
    Result := Integer(Value) - Integer(TIDCharConstant(Constant).Value)
  else
    Result := -1;
end;

procedure TIDCharConstant.SetAsVariant(const AValue: Variant);
begin
  var AStr := string(AValue);
  Assert(Length(AStr) <= 1);
  if AStr <> '' then
    FValue := AStr[1];
end;

{ TIDBooleanConstant }

function TIDBooleanConstant.AsInt64: Int64;
begin
  Result := Int64(Value);
end;

function TIDBooleanConstant.AsString: string;
begin
  if Value then
    Result := 'TRUE'
  else
    Result := 'FALSE';
end;

function TIDBooleanConstant.AsVariant: Variant;
begin
  Result := FValue;
end;

function TIDBooleanConstant.ValueByteSize: Integer;
begin
  Result := 1;
end;

function TIDBooleanConstant.ValueDataType: TDataTypeID;
begin
  Result := dtBoolean;
end;

function TIDBooleanConstant.CompareTo(Constant: TIDConstant): Integer;
begin
  if ClassType = Constant.ClassType then
    Result := Integer(Value) - Integer(TIDBooleanConstant(Constant).Value)
  else
    Result := -1;
end;

procedure TIDBooleanConstant.SetAsVariant(const AValue: Variant);
begin
  FValue := AValue;
end;

{ TIDArray }

procedure TIDArray.AddBound(Bound: TIDOrdinal);
begin
  SetLength(FDimensions, FDimensionsCount + 1);
  FDimensions[FDimensionsCount] := Bound;
  Inc(FDimensionsCount);
end;

constructor TIDArray.Create(Scope: TScope; const Name: TIdentifier);
begin
  inherited Create(Scope, Name);
  FDataTypeID := dtStaticArray;
  if Assigned(SYSUnit) then
    CreateStandardOperators;
end;

constructor TIDArray.CreateAsAnonymous(Scope: TScope);
begin
  inherited CreateAsAnonymous(Scope);
  FDataTypeID := dtStaticArray;
  if Assigned(SYSUnit) then
    CreateStandardOperators;
end;

procedure TIDArray.CreateStandardOperators;
begin
  inherited;
  OverloadImplicitTo(Self);
  OverloadImplicitToAny(SYSUnit.Operators.ImplicitArrayToAny);
  OverloadImplicitFromAny(SYSUnit.Operators.ImplicitArrayFromAny);
  AddBinarySysOperator(opIn, SYSUnit.Operators.Ordinal_In_Set);
end;

constructor TIDArray.CreateAnonymousStatic1Dim(Scope: TScope; ElementDataType: TIDType; Length: Integer; out BoundType: TIDOrdinal);
begin
  inherited CreateAsAnonymous(Scope);
  FDataTypeID := dtStaticArray;
  OverloadImplicitTo(Self);
  BoundType := TIDRangeType.CreateAsAnonymous(Scope);
  BoundType.LowBound := 0;
  BoundType.HighBound := Length - 1;
  AddBound(BoundType);
  FElementDataType := ElementDataType;
end;

function TIDArray.GetDataSize: Integer;
var
  i: Integer;
begin
  if DataTypeID = dtStaticArray then
  begin
    Result := 1;
    for i := 0 to FDimensionsCount - 1 do
      Result := Result * FDimensions[i].ElementsCount;
    Result := ElementDataType.DataSize * Result;
  end else
    Result := Package.PointerSize;
end;

function TIDArray.GetDimension(Index: Integer): TIDOrdinal;
begin
  if Index < FDimensionsCount then
    Result := FDimensions[Index]
  else
  if FElementDataType is TIDArray then
    Result := TIDArray(FElementDataType).Dimensions[Index - FDimensionsCount]
  else begin
    AbortWorkInternal(sInvalidIndex);
    Result := nil;
  end;
end;

function TIDArray.GetDisplayName: string;
  function GetDimInfo: string;
  var
    i: Integer;
  begin
    Result := '';
    for i := 0 to FDimensionsCount - 1 do
      Result := AddStringSegment(Result, FDimensions[i].DisplayName, ', ');
  end;
begin
  Result := inherited GetDisplayName;
  if Result <> '' then
    Exit;

  case FDataTypeID of
    dtStaticArray: Result := 'static array [' + GetDimInfo + '] of ';
    dtDynArray: Result := 'array of ';
    dtOpenArray: Result := 'openarray of ';
  end;

  if Assigned(FElementDataType) then
    Result := Result + FElementDataType.DisplayName
  else
    Result := Result + '<null>';
end;

function TIDArray.GetElementTypeByIndex(Index: Integer): TIDType;
begin
  if Index < FDimensionsCount then
    Result := FElementDataType
  else
  if FElementDataType is TIDArray then
    Result := TIDArray(FElementDataType).ElementTypeByIndex[Index - FDimensionsCount]
  else begin
    AbortWorkInternal(sInvalidIndex);
    Result := nil; // avoid warning
  end;
end;

function TIDArray.GetIsGeneric: Boolean;
begin
  Result := Assigned(FElementDataType) and FElementDataType.IsGeneric;
end;

function TIDArray.GetAllDimensionsCount: Integer;
begin
  Result := FDimensionsCount;
  if FElementDataType is TIDArray then
    Result := Result + TIDArray(FElementDataType).AllDimensionsCount;
end;

procedure TIDArray.IncRefCount(RCPath: UInt32);
var
  i: Integer;
begin
  if FRCPath = RCPath then
    Exit;
  FRCPath := RCPath;
  Inc(FRefCount);
  for i := 0 to FDimensionsCount - 1 do
    FDimensions[i].IncRefCount(RCPath);
  ElementDataType.IncRefCount(RCPath);
end;

function TIDArray.InstantiateGeneric(ADstScope: TScope; ADstStruct: TIDStructure;
                                     const AContext: TGenericInstantiateContext): TIDDeclaration;
begin
  Result := inherited;
  if Assigned(Result) then
    Exit;

  if IsGeneric then
  begin
    if not Assigned(GenericDescriptor) or
       not GenericDescriptor.TryGetInstance(AContext.Args, {out} Result) then
    begin
      LogBegin('inst [type: %s, src: %s]', [ClassName, DisplayName]);
      var LNewArray := MakeCopy as TIDArray;
      if Assigned(GenericDescriptor) then
      begin
        // override name according to the generic arguments
        LNewArray.ID := AContext.DstID;
        // add this instance to the its pool
        GenericDescriptor.AddGenericInstance(LNewArray, AContext.Args);
      end;
      // instantiate array element type
      LNewArray.ElementDataType := ElementDataType.InstantiateGeneric(ADstScope, ADstStruct, AContext) as TIDType;

      Result := LNewArray;

      LogEnd('inst [type: %s, dst: %s]', [ClassName, Result.DisplayName]);
    end;
  end else
  begin
    Result := Self;
  end;
end;

function TIDArray.IsOpenArrayOfConst: Boolean;
begin
  Result := (DataTypeID = dtOpenArray) and (ElementDataType = SYSUnit.SystemDeclarations._TVarRec);
end;

procedure TIDArray.DecRefCount(RCPath: UInt32);
var
  i: Integer;
begin
  if FRCPath = RCPath then
    Exit;
  FRCPath := RCPath;
  Dec(FRefCount);
  for i := 0 to FDimensionsCount - 1 do
    FDimensions[i].DecRefCount(RCPath);
  ElementDataType.DecRefCount(RCPath);
end;

function TIDArray.DoesGenericUseParams(const AParams: TIDTypeArray): Boolean;
begin
  for var LParamIndex := 0 to Length(AParams) - 1 do
    if (AParams[LParamIndex] = ElementDataType) or
        ElementDataType.DoesGenericUseParams(AParams) then
      Exit(True);

  Result := False;
end;

procedure WriteDataTypeIndex(Stream: TStream; DataType: TIDType);
begin
  Stream.WriteStretchUInt(DataType.Index);
  if DataType.Index >= TSYSTEMUnit.SystemTypesCount then
    Stream.WriteStretchUInt(DataType.UnitID);
end;

{ TIDOrdinalType }

procedure TIDOrdinal.Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer; AAppendName: Boolean);
begin
  ABuilder.Append(' ', ANestedLevel*2);
  ABuilder.Append('type ');
  if Name <> '' then
    ABuilder.Append(Name)
  else begin
    ABuilder.Append(LowBound.ToString);
    ABuilder.Append('..');
    ABuilder.Append(HighBound.ToString);
  end;
end;

function TIDOrdinal.GetElementsCount: UInt64;
begin
  if FHBound > FLBound then
    Result := Abs(FHBound - FLBound) + 1
  else
    Result := Abs(FLBound - FHBound) + 1
end;

function TIDOrdinal.GetHighBoundUInt64: UInt64;
begin
  Result := UInt64(FHBound);
end;

function TIDOrdinal.GetOrdinal: Boolean;
begin
  Result := True;
end;

function TIDOrdinal.MatchExplicitFrom(ASrc: TIDType): Boolean;
begin
  Result := ASrc.IsOrdinal or (ASrc.DataSize <= DataSize);
end;

{ TIDRefType }

function TIDRefType.GetDisplayName: string;
begin
  Result := FID.Name;
  if Result = '' then
  begin
    if Assigned(ReferenceType) then
      Result := '^' + ReferenceType.DisplayName
    else
      Result := '^<untyped>';
  end;
end;

procedure TIDRefType.IncRefCount(RCPath: UInt32);
begin
  if FRCPath = RCPath then
    Exit;
  FRCPath := RCPath;
  Inc(FRefCount);
  if Assigned(FReferenceType) then
    FReferenceType.IncRefCount(RCPath);
end;

constructor TIDRefType.Create(Scope: TScope; const ID: TIdentifier);
begin
  inherited;
  TypeKind := tkRefernce;
end;

constructor TIDRefType.CreateAsAnonymous(Scope: TScope; ReferenceType: TIDType);
begin
  inherited CreateAsAnonymous(Scope);
  TypeKind := tkRefernce;
  FReferenceType := ReferenceType;
end;

constructor TIDRefType.CreateAsSystem(Scope: TScope; const Name: string);
begin
  inherited;
  TypeKind := tkRefernce;
end;

procedure TIDRefType.CreateStandardOperators;
begin
  inherited;
  OverloadImplicitTo(Self);
  OverloadBinarOperator2(opEqual, SYSUnit._NilPointer, SYSUnit._Boolean);
  OverloadBinarOperator2(opNotEqual, SYSUnit._NilPointer, SYSUnit._Boolean);
  OverloadBinarOperator2(opEqual, SYSUnit._Pointer, SYSUnit._Boolean);
  OverloadBinarOperator2(opNotEqual, SYSUnit._Pointer, SYSUnit._Boolean);
  OverloadBinarOperator2(opEqual, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opNotEqual, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opGreater, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opGreaterOrEqual, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opLess, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opLessOrEqual, Self, SYSUnit._Boolean);

  OverloadExplicitToAny(SYSUnit.Operators.ExplicitRefTypeToAny);
  OverloadExplicitFromAny(SYSUnit.Operators.ExplicitRefTypeFromAny);
end;

procedure TIDRefType.DecRefCount(RCPath: UInt32);
begin
  if FRCPath = RCPath then
    Exit;
  FRCPath := RCPath;
  Dec(FRefCount);
  if Assigned(FReferenceType) then
    FReferenceType.DecRefCount(RCPath);
end;

//
//function TIDRefType.MatchExplicitFrom(ASrc: TIDType): Boolean;
//begin
//  Result := ASrc.IsOrdinal or
//            (ASrc.DataTypeID in [dtPointer, dtClass, dtClassOf, dtInterface,
//                                 dtWideString, dtString, dtAnsiString, dtDynArray]) or
//            ((ASrc.DataTypeID in [dtRecord, dtStaticArray]) and (ASrc.DataSize = Package.PointerSize));
//end;
//
//function TIDRefType.MatchExplicitTo(ADst: TIDType): Boolean;
//begin
//  Result := ADst.IsOrdinal or (ADst.DataTypeID in [dtPointer,
//                                                   dtPAnsiChar,
//                                                   dtPWideChar,
//                                                   dtProcType,
//                                                   dtRecord,
//                                                   dtClass,
//                                                   dtInterface,
//                                                   dtDynArray,
//                                                   dtAnsiString,
//                                                   dtString,
//                                                   dtWideString]);
//end;

{ TIDPointerType }

constructor TIDPointer.Create(Scope: TScope; const ID: TIdentifier);
begin
  inherited Create(Scope, ID);
  fDataTypeID := dtPointer;
  TypeKind := tkRefernce;
  CreateStandardOperators;
end;

constructor TIDPointer.CreateAsAnonymous(Scope: TScope; ReferenceType: TIDType);
begin
  inherited CreateAsAnonymous(Scope, ReferenceType);
  fDataTypeID := dtPointer;
  CreateStandardOperators;
end;

constructor TIDPointer.CreateAsSystem(Scope: TScope; const Name: string);
begin
  inherited;
  fDataTypeID := dtPointer;
  // can't call CreateStandardOperators until all needed types will be initialized
  // CreateStandardOperators;
end;

procedure TIDPointer.CreateStandardOperators;
begin
  if not Assigned(SYSUnit) then
    Exit;
  inherited CreateStandardOperators;

  OverloadBinarOperator2(opSubtract, Self, SYSUnit._Int32);

  OverloadBinarOperator2(opAdd, SYSUnit._Int8, Self);
  OverloadBinarOperator2(opAdd, SYSUnit._Int16, Self);
  OverloadBinarOperator2(opAdd, SYSUnit._Int32, Self);
  OverloadBinarOperator2(opAdd, SYSUnit._Int64, Self);
  OverloadBinarOperator2(opAdd, SYSUnit._NativeInt, Self);

  OverloadBinarOperator2(opAdd, SYSUnit._UInt8, Self);
  OverloadBinarOperator2(opAdd, SYSUnit._UInt16, Self);
  OverloadBinarOperator2(opAdd, SYSUnit._UInt32, Self);
  OverloadBinarOperator2(opAdd, SYSUnit._UInt64, Self);
  OverloadBinarOperator2(opAdd, SYSUnit._NativeUInt, Self);

  OverloadBinarOperator2(opSubtract, SYSUnit._Int8, Self);
  OverloadBinarOperator2(opSubtract, SYSUnit._Int16, Self);
  OverloadBinarOperator2(opSubtract, SYSUnit._Int32, Self);
  OverloadBinarOperator2(opSubtract, SYSUnit._Int64, Self);
  OverloadBinarOperator2(opSubtract, SYSUnit._NativeInt, Self);

  OverloadBinarOperator2(opSubtract, SYSUnit._UInt8, Self);
  OverloadBinarOperator2(opSubtract, SYSUnit._UInt16, Self);
  OverloadBinarOperator2(opSubtract, SYSUnit._UInt32, Self);
  OverloadBinarOperator2(opSubtract, SYSUnit._UInt64, Self);
  OverloadBinarOperator2(opSubtract, SYSUnit._NativeUInt, Self);

  OverloadImplicitToAny(SYSUnit.Operators.ImplicitPointerToAny);
  OverloadImplicitFromAny(SYSUnit.Operators.ImplicitPointerFromAny);
end;

procedure TIDPointer.Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer; AAppendName: Boolean);
begin
  ABuilder.Append(' ', ANestedLevel*2);
  ABuilder.Append('type ');
  ABuilder.Append(Name);
  if Assigned(ReferenceType) then
  begin
    ABuilder.Append(' = ^');
    ABuilder.Append(ReferenceType.DisplayName);
  end;
end;

function TIDPointer.GetIsGeneric: Boolean;
begin
  Result := Assigned(ReferenceType) and ReferenceType.IsGeneric;
end;

function TIDPointer.InstantiateGeneric(ADstScope: TScope; ADstStruct: TIDStructure;
                                       const AContext: TGenericInstantiateContext): TIDDeclaration;
begin
  Result := inherited;
  if Assigned(Result) then
    Exit;

  if IsGeneric then
  begin
    if not Assigned(GenericDescriptor) or
       not GenericDescriptor.TryGetInstance(AContext.Args, {out} Result) then
    begin
      LogBegin('inst [type: %s, src: %s]', [ClassName, DisplayName]);
      var LNewPtr := MakeCopy as TIDPointer;

      if Assigned(GenericDescriptor) then
      begin
        // override name according to the generic arguments
        Result.ID := AContext.DstID;
        // add this instance to the its pool
        GenericDescriptor.AddGenericInstance(Result, AContext.Args);
        // add the new type to the original scope only in case it's not a nested one
        if not Assigned(ADstStruct) then
          Scope.AddType(LNewPtr);
      end;

      LNewPtr.ReferenceType := ReferenceType.InstantiateGeneric(ADstScope, ADstStruct, AContext) as TIDType;

      Result := LNewPtr;
      LogEnd('inst [type: %s, dst: %s]', [ClassName, Result.DisplayName]);
    end;
  end else
    Result := Self;
end;

//function TIDPointer.MatchImplicitFrom(ASrc: TIDType): Boolean;
//begin
//  Result := (
//    (
//      (Self.DataTypeID in [dtPAnsiChar]) and
//      (ASrc.DataTypeID = dtStaticArray) and
//      (TIDStaticArray(ASrc).ElementDataType = SYSUnit._AnsiChar)
//    ) or
//    (
//      (Self.DataTypeID in [dtPWideChar]) and
//      (ASrc.DataTypeID = dtStaticArray) and
//      (TIDStaticArray(ASrc).ElementDataType = SYSUnit._WideChar)
//    ) or
//      (ASrc.DataTypeID in [dtDynArray, dtClass, dtClassOf, dtInterface])
//  );
//end;
//
//function TIDPointer.MatchImplicitTo(ADst: TIDType): Boolean;
//begin
//  Result :=  ADst.DataTypeID in [dtPointer, dtPAnsiChar, dtPWideChar, dtClassOf];
//end;

{ TIDRecordType }

procedure TIDRecord.CreateStandardOperators;
begin
  inherited;
  OverloadImplicitTo(Self);
  OverloadBinarOperator2(opEqual, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opNotEqual, Self, SYSUnit._Boolean);
  fSysExplicitFromAny := SYSUnit.Operators.ExplicitRecordFromAny;
  OverloadExplicitToAny(SYSUnit.Operators.ExplicitRecordToAny);
end;

function TIDRecord.GetDataSize: Integer;
begin
  if Length(fCases) = 0 then
    Exit(inherited GetDataSize);

  Result := 0;
  for var LCaseIndex := 0 to Length(fCases) - 1 do
  begin
    var LCaseSize: Integer := 0;
    var LFieldArray := FCases[LCaseIndex];
    for var LFieldIndex := 0 to Length(LFieldArray) - 1 do
      LCaseSize := LCaseSize + LFieldArray[LFieldIndex].DataType.DataSize;

    if LCaseSize > Result then
      Result := LCaseSize;
  end;
end;

function TIDRecord.GetStructKeyword: string;
begin
  Result := 'record';
end;

procedure TIDRecord.AddCase(const AFields: TIDFieldArray);
begin
  var LArrayLen := Length(fCases);
  SetLength(fCases, LArrayLen + 1);
  fCases[LArrayLen] := AFields;
end;

constructor TIDRecord.Create(Scope: TScope; const Name: TIdentifier);
begin
  inherited Create(Scope, Name);
  FDataTypeID := dtRecord;
  CreateStandardOperators();
end;

constructor TIDRecord.CreateAsSystem(Scope: TScope; const Name: string);
begin
  inherited;
  CreateStandardOperators;
end;

{ TIDEnumType }

constructor TIDEnum.Create(Scope: TScope; const ID: TIdentifier);
begin
  inherited Create(Scope, ID);
  DataTypeID := dtEnum;
  CreateStandardOperators;
end;

constructor TIDEnum.CreateAsAnonymous(Scope: TScope);
begin
  inherited CreateAsAnonymous(Scope);
  DataTypeID := dtEnum;
  CreateStandardOperators;
end;

procedure TIDEnum.CreateStandardOperators;
begin
  inherited;
  OverloadImplicitTo(Self);
  OverloadBinarOperator2(opEqual, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opNotEqual, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opLess, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opLessOrEqual, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opGreater, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opGreaterOrEqual, Self, SYSUnit._Boolean);
  OverloadExplicitFromAny(SYSUnit.Operators.ExplicitEnumFromAny);
  OverloadExplicitToAny(SYSUnit.Operators.ExplicitEnumToAny);
end;

procedure TIDEnum.Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer; AAppendName: Boolean);
begin
  ABuilder.Append(' ', ANestedLevel*2);
  ABuilder.Append('type ');
  ABuilder.Append(Name);
  ABuilder.Append(' = (');
  for var AIndex := 0 to FItems.Count - 1 do
  begin
    var LItem := FItems.Items[AIndex];
    ABuilder.Append(LItem.Name);
    if AIndex < FItems.Count - 1 then
      ABuilder.Append(', ');
  end;
  ABuilder.Append(')');
end;

function TIDEnum.GetDataSize: Integer;
begin
  Result := GetRangeByteSize(HighBound, LowBound);
end;

function TIDEnum.GetDisplayName: string;
var
  Item: TIDDeclaration;
  Node: TScope.PAVLNode;
  s: string;
begin
  Result := Name;
  if Name = '' then
  begin
    s := '';
    Node := FItems.First;
    while Assigned(Node) do begin
      Item := Node.Data as TIDDeclaration; // надо сделать чтобы не по алфовиту а по мере добавления
      s := AddStringSegment(s, Item.DisplayName, ', ');
      Node := FItems.Next(Node);
    end;
    Result := 'Enum(' + s +')';
  end;
end;

{ TIDDynArray }

constructor TIDDynArray.Create(Scope: TScope; const Name: TIdentifier);
begin
  inherited;
  FDataTypeID := dtDynArray;
  if Assigned(SYSUnit) then
    AddBound(TIDOrdinal(SYSUnit._NativeUInt));
end;

constructor TIDDynArray.CreateAsAnonymous(Scope: TScope);
begin
  inherited CreateAsAnonymous(Scope);
  FDataTypeID := dtDynArray;
  if Assigned(SYSUnit) then
    AddBound(TIDOrdinal(SYSUnit._NativeUInt));
end;

constructor TIDDynArray.CreateAsSystem(Scope: TScope; const Name: string);
begin
  inherited;
  FDataTypeID := dtDynArray;
  if Assigned(SYSUnit) then
    AddBound(TIDOrdinal(SYSUnit._NativeUInt));
end;

procedure TIDDynArray.CreateStandardOperators;
begin
  inherited;
  AddBinarySysOperator(opEqual, SYSUnit.Operators.Equal_DynArray);
  OverloadExplicitToAny(SYSUnit.Operators.ExplicitDynArrayToAny);
end;

procedure TIDDynArray.Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer; AAppendName: Boolean);
begin
  if AAppendName then
  begin
    ABuilder.Append(' ', ANestedLevel*2);
    ABuilder.Append('type ');
    ABuilder.Append(DisplayName);
  end;

  ABuilder.Append(' = array of ');
  TypeNameToString(ElementDataType, ABuilder);

  if AAppendName then
  begin
    GenericInstances2Str(ABuilder, ANestedLevel);
    GenericOverloads2Str(ABuilder, ANestedLevel);
  end;
end;

function TIDDynArray.GetHelperScope: TScope;
begin
  if not Assigned(FMembersScope) then
  begin
    FMembersScope := TScope.Create(stStruct, FScope);
    var ACreateDecl := TCT_DynArrayCreate.CreateDecl(SYSUnit, Scope);
    ACreateDecl.ResultType := Self;
    FMembersScope.InsertID(ACreateDecl);
  end;
  Result := FMembersScope;
end;

{ TIDSetType }

constructor TIDSet.Create(Scope: TScope; const ID: TIdentifier);
begin
  inherited Create(Scope, ID);
  fDataTypeID := dtSet;
  if Assigned(SYSUnit) then
    CreateStandardOperators;
end;

constructor TIDSet.CreateAsAnonymous(Scope: TScope; BaseType: TIDOrdinal);
begin
  inherited CreateAsAnonymous(Scope);
  fDataTypeID := dtSet;
  fBaseType := BaseType;
  if Assigned(SYSUnit) then
    CreateStandardOperators;
end;

constructor TIDSet.CreateAsSystem(Scope: TScope; const Name: string);
begin
  inherited;
  fDataTypeID := dtSet;
  if Assigned(SYSUnit) then
    CreateStandardOperators;
end;

procedure TIDSet.CreateStandardOperators;
begin
  inherited;
  OverloadImplicitTo(Self);
  OverloadBinarOperator2(opEqual, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opNotEqual, Self, SYSUnit._Boolean);
  //OverloadImplicitToAny(SYSUnit.Operators.ImplicitSetToAny);
  AddBinarySysOperator(opIn, SYSUnit.Operators.Ordinal_In_Set);
  AddBinarySysOperator(opAdd, SYSUnit.Operators.Add_Set);
  AddBinarySysOperator(opSubtract, SYSUnit.Operators.Subtract_Set);
  AddBinarySysOperator(opMultiply, SYSUnit.Operators.Multiply_Set);
  AddBinarySysOperator(opEqual, SYSUnit.Operators.Equal_Set);
  AddBinarySysOperator(opNotEqual, SYSUnit.Operators.NotEqual_Set);

  OverloadImplicitFromAny(SYSUnit.Operators.ImplicitSetFromAny);
end;

procedure TIDSet.Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer; AAppendName: Boolean);
begin
  ABuilder.Append(' ', ANestedLevel*2);
  ABuilder.Append('type ');
  ABuilder.Append(Name);
  ABuilder.Append(' = set of ');
  TypeNameToString(BaseType, ABuilder);
end;

function TIDSet.GetBitsCount: UInt32;
begin
  Result := fBaseType.ElementsCount;
end;

function TIDSet.GetDataSize: Integer;
var
  bc: Integer;
begin
  bc := GetBitsCount;
  Result := bc div 8;
  if (bc mod 8) > 0 then
    Result := Result + 1;
end;

function TIDSet.GetDisplayName: string;
begin
  Result := Name;
  if (Name = '') then
  begin
    if Assigned(FBaseType) then
      Result := 'set of ' + FBaseType.DisplayName
    else
      Result := 'set';
  end;
end;

procedure TIDSet.IncRefCount(RCPath: UInt32);
begin
  if FRCPath = RCPath then
    Exit;
  FRCPath := RCPath;
  Inc(FRefCount);
  FBaseType.IncRefCount(RCPath);
end;

function TIDSet.MatchExplicitFrom(ASrc: TIDType): Boolean;
begin
  Result := (ASrc.DataSize <= DataSize) or
            (
              (ASrc.DataTypeID = dtSet) and
              (BaseType.DataTypeID <> dtEnum) and
              (TIDSet(ASrc).fBaseType.DataTypeID <> dtEnum)
            );
end;

function TIDSet.MatchImplicitFrom(ASrc: TIDType): Boolean;
begin
  Result := (ASrc.DataTypeID = dtSet) and
            (BaseType.DataTypeID = TIDSet(ASrc).BaseType.DataTypeID);
end;

{ TIDArrayConstant }

procedure TIDDynArrayConstant.AddItem(const Item: TIDExpression);
var
  c: Integer;
begin
  c := Length(FValue);
  SetLength(FValue, c + 1);
  FValue[c] := Item;
end;

function TIDDynArrayConstant.AsInt64: Int64;
begin
  Result := 0;
end;

function TIDDynArrayConstant.AsString: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(Value) - 1 do
    Result := AddStringSegment(Result, DeclarationName(Value[i].Declaration), ', ');
  Result := '[' + Result + ']';
end;

function TIDDynArrayConstant.AsVariant: Variant;
begin
  Result := NULL;
end;

function TIDDynArrayConstant.ValueByteSize: Integer;
begin
  Result := 0; /// !!!
end;

function TIDDynArrayConstant.ValueDataType: TDataTypeID;
begin
  Result := dtStaticArray;
end;

function TIDDynArrayConstant.CheckAsSet: Boolean;
begin
  // todo: add check for unique values
  Result := ElementType.IsOrdinal;
end;

function TIDDynArrayConstant.CompareTo(Constant: TIDConstant): Integer;
var
  i, c: Integer;
  Value2: TIDExpressions;
  CItem1, CItem2: TIDConstant;
begin
  if ClassType = Constant.ClassType then
  begin
    Value2 := TIDDynArrayConstant(Constant).Value;
    c := Length(Value);
    Result := c - Length(Value2);
    if Result = 0 then
    for i := 0 to c - 1 do begin
      CItem1 := TIDConstant(Value[i].Declaration);
      CItem2 := TIDConstant(Value2[i].Declaration);
      Result := CItem1.CompareTo(CItem2);
      if Result <> 0 then
        Exit;
    end;
  end else
    Result := -1;
end;

function TIDDynArrayConstant.GetElementType: TIDType;
begin
  Result := TIDArray(FDataType).ElementDataType;
end;

function TIDDynArrayConstant.GetLength: Integer;
begin
  Result := Length(Value);
end;

procedure TIDDynArrayConstant.SetAsVariant(const AValue: Variant);
begin
  // todo:
end;

function DeclarationName(Decl: TIDDeclaration; IsList: Boolean = False): string;
begin
  if Assigned(Decl) then
  begin
    if Decl is TIDConstant then begin
      if (Decl.ClassType = TIDStringConstant) or (Decl.ClassType = TIDCharConstant) then
        Result := '''' + TIDConstant(Decl).AsString + ''''
      else
        Result := TIDConstant(Decl).AsString;
    end else
    if Decl is TIDOperator then begin
      Result := TIDOperator(Decl).Struct.DisplayName + '.' + Decl.DisplayName;
    end else begin
      Result := Decl.DisplayName;
      {if (Decl.FItemType = itVar) and (TIDVariable(Decl).IsField) and not IsList then
        Result := 'Self.' + Result;}
      //if (Decl is TIDVariable) and TIDVariable(Decl).Reference then
      //  Result := '*' + Result;
      //Result := Format('%s[%d]', [Result, Decl.Index]);
    end;
  end else
    Result := 'nil';
end;

function ArrayExpression(ArrayInfo: TIDArray; const Expressions: TIDExpressions; var StartIndex: Integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to ArrayInfo.DimensionsCount do
  begin
    Result := AddStringSegment(Result, ExpressionName(Expressions[StartIndex]));
    Inc(StartIndex);
  end;
  Result := '[' + Result + ']';
end;

function GetExpressionsNames(const Expressions: TIDExpressions; const Separator: string = ', '): string;
begin
  Result := '';
  for var i := 0 to Length(Expressions) - 1 do
    Result := AddStringSegment(Result, Expressions[i].DisplayName, Separator);
end;

function ExpressionsName(const Expressions: TIDExpressions): string;
var
  i, c: Integer;
  E: TIDExpression;
  D: TIDDeclaration;
  s: string;
begin
  i := 0;
  Result :=  '';
  c := Length(Expressions);
  while i < c do begin
    E := Expressions[i];
    Inc(i);
    D := E.Declaration;
    s := DeclarationName(D, True);
    if (c > i) and E.DataType.InheritsFrom(TIDArray) then
      s := s + ArrayExpression(TIDArray(E.DataType), Expressions, i);
    if (D is TIDVariable) and TIDVariable(D).Reference then
      s := '*' + s;
    Result := AddStringSegment(Result, s, '.');
  end;
end;

function ExpressionName(Expr: TIDExpression): string;
var
  Inst: TIDExpression;
begin
  if Assigned(Expr) then
  begin
    if Expr.ExpressionType = etDeclaration then
    begin
      Result := DeclarationName(Expr.Declaration, False);
      if (Expr is TIDCallExpression) then
      begin
        Inst := TIDCallExpression(Expr).Instance;
        if Assigned(Inst) and (Inst.Declaration is TIDType) then
          Result := Inst.DisplayName + '.' + Result;
      end;
    end else
      Result := ExpressionsName(TIDMultiExpression(Expr).Items);
  end else
    Result := 'nil';
end;

{ TSpace }

procedure TSpace<T>.Add(const Declaration: T);
begin
  if FLast = Declaration then
    AbortWorkInternal('Duplicated declaration: ' + TIDDeclaration(Declaration).DisplayName,
      TIDDeclaration(Declaration).TextPosition);

  if Assigned(FLast) then
  begin
    Assert(not Assigned(TIDDeclaration(FLast).FNext));
    TIDDeclaration(FLast).FNext := TIDDeclaration(Declaration)
  end else
    FFirst := Declaration;
  FLast := Declaration;
  Inc(FCount);
end;

procedure TSpace<T>.Delete(const Declaration: T);
var
  Item, PrevItem: TIDDeclaration;
begin
  PrevItem := nil;
  Item := TIDDeclaration(FFirst);
  while Assigned(Item) do
  begin
    if Item = TIDDeclaration(Declaration) then
    begin
      if Assigned(PrevItem) then
        PrevItem.NextItem := Item.NextItem;

      if FFirst = Declaration then
        TIDDeclaration(FFirst) := TIDDeclaration(FFirst).NextItem;

      if FLast = Declaration then
        TIDDeclaration(FLast) := PrevItem;

      Dec(FCount);
      break;
    end;
    PrevItem := Item;
    Item := Item.NextItem;
  end;
end;

procedure TSpace<T>.Initialize(StartIndex: Integer);
begin
  FStartIndex := StartIndex;
  FCount := 0;
  FFirst := nil;
  FLast := nil;
end;

procedure TSpace<T>.InsertFirst(const Declaration: T);
begin
  TIDDeclaration(Declaration).NextItem := TIDDeclaration(First);
  TIDDeclaration(Declaration).FIndex := 0;
  First := Declaration;
  if not Assigned(FLast) then
    FLast := First;
  Inc(FCount);
end;

procedure TSpace<T>.Reindex;
var
  Index: Integer;
  Item: TIDDeclaration;
begin
  Index := FStartIndex;
  Item :=  TIDDeclaration(FFirst);
  while Assigned(Item) do
  begin
    Item.FIndex := Index;
    Inc(Index);
    Item := Item.NextItem;
  end;
end;

procedure TSpace<T>.Initialize;
begin
  FillChar(Self, SizeOf(Self), #0);
end;

{ TIDAliasType }

constructor TIDAliasType.CreateAlias(Scope: TScope;
                                     const ID: TIdentifier;
                                     OriginalType: TIDType;
                                     ANewType: Boolean);
begin
  inherited Create(Scope, ID);
  FItemType := itType;
  FTypeKind := tkAlias;
  FLinkedType := OriginalType;
  FOriginalType := OriginalType;
  FDataTypeID := FOriginalType.DataTypeID;
  FNewType := ANewType;
end;

constructor TIDAliasType.CreateAliasAsSystem(Scope: TScope; const ID: string; SrcType: TIDType);
begin
  inherited CreateAsSystem(Scope, ID);
  FItemType := itType;
  FTypeKind := tkAlias;
  FLinkedType := SrcType;
  FOriginalType := SrcType.ActualDataType;
  FDataTypeID := FOriginalType.DataTypeID;
end;

function TIDAliasType.GetActualDataType: TIDType;
begin
  Result := FOriginalType.ActualDataType;
end;

function TIDAliasType.GetDisplayName: string;
begin
  Result := Name;
  if Result = '' then
    Result := 'alias of ' + fOriginalType.DisplayName;
end;

function TIDAliasType.GetIndex: Integer;
begin
  Result := FOriginalType.SpaceIndex;
end;

function TIDAliasType.GetOrdinal: Boolean;
begin
  Result := FLinkedType.IsOrdinal;
end;

function TIDAliasType.GetOriginalDecl: TIDDeclaration;
begin
  Result := fOriginalType;
end;

procedure TIDAliasType.IncRefCount(RCPath: UInt32);
begin
  if FRCPath = RCPath then
    Exit;
  FRCPath := RCPath;
  Inc(FRefCount);
  FOriginalType.IncRefCount(RCPath);
end;

function TIDAliasType.InstantiateGeneric(ADstScope: TScope; ADstStruct: TIDStructure;
                                         const AContext: TGenericInstantiateContext): TIDDeclaration;
begin
  LogBegin('inst [type: %s, src: %s]', [ClassName, DisplayName]);
  Result := fOriginalType.InstantiateGeneric(ADstScope, nil, AContext);
  LogEnd('inst [type: %s, dst: %s]', [ClassName, Result.DisplayName]);
end;

procedure TIDAliasType.Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer; AAppendName: Boolean);
begin
  ABuilder.Append(' ', ANestedLevel*2);
  ABuilder.Append('type ');
  ABuilder.Append(Name);
  ABuilder.Append(' = ');
  if NewType then
    ABuilder.Append('type ');
  TypeNameToString(Original, ABuilder);
end;

procedure TIDAliasType.DecRefCount(RCPath: UInt32);
begin
  if FRCPath = RCPath then
    Exit;
  FRCPath := RCPath;
  Dec(FRefCount);
  FOriginalType.DecRefCount(RCPath);
end;

{ TIDListExpression }

procedure TIDMultiExpression.AddItem(Expr: TIDExpression);
var
  c: Integer;
begin
  c := Length(FItems);
  SetLength(FItems, c + 1);
  FItems[c] := Expr;
  FDeclaration := Expr.Declaration;
end;

constructor TIDMultiExpression.Create(const ExpressionList: TIDExpressions; const TextPosition: TTextPosition);
var
  c: Integer;
begin
  CreateFromPool;
  c := Length(ExpressionList);
  if c > 0 then
    FDeclaration := ExpressionList[c - 1].Declaration;
  FDataType := FDeclaration.DataType;
  FTextPosition := TextPosition;
  FItems := ExpressionList;
end;

constructor TIDMultiExpression.Create(const Base, Member: TIDExpression; const TextPosition: TTextPosition);
begin
  CreateFromPool;
  FDeclaration := Member.Declaration;
  FDataType := Member.DataType;
  FTextPosition := TextPosition;
  SetLength(FItems, 2);
  FItems[0] := Base;
  FItems[1] := Member;
end;


constructor TIDMultiExpression.CreateAnonymous(DataType: TIDType; const Expressions: array of TIDExpression);
var
  i, c: Integer;
begin
  CreateFromPool;
  c := Length(Expressions);
  SetLength(FItems, c);

  for i := 0 to c - 1 do
    FItems[i] := Expressions[i];

  if c > 0 then
    FDeclaration := Expressions[c - 1].Declaration;

  FDataType := DataType;
end;

constructor TIDMultiExpression.CreateAnonymous(Declaration: TIDDeclaration);
begin
  CreateFromPool;
  SetLength(FItems, 1);
  FItems[0] := TIDExpression.Create(Declaration);
  FDeclaration := Declaration;
  FDataType := Declaration.DataType;
end;

constructor TIDMultiExpression.CreateAnonymous(const ExpressionList: TIDExpressions);
var
  c: Integer;
begin
  CreateFromPool;
  c := Length(ExpressionList);
  if c > 0 then
    FDeclaration := ExpressionList[c - 1].Declaration;
  FItems := ExpressionList;
  FDataType := FDeclaration.DataType;
end;

function TIDMultiExpression.GetDataType: TIDType;
begin
  Result := FDataType;
end;

function TIDMultiExpression.GetDisplayName: string;
begin
  Result := ExpressionsName(FItems);
end;

class function TIDMultiExpression.GetExpressionType: TExpressonType;
begin
  Result := etExpressionList;
end;

{ TIDCastExpression }

constructor TIDCastExpression.Create(Declaration: TIDDeclaration; CastDataType: TIDType; const TextPosition: TTextPosition);
begin
  CreateFromPool;
  FDeclaration := Declaration;
  FTextPosition := TextPosition;
  FDataType := CastDataType;
end;

constructor TIDCastExpression.Create(Source: TIDExpression; CastDataType: TIDType);
begin
  CreateFromPool;
  FDeclaration := Source.Declaration;
  FTextPosition := Source.TextPosition;
  FDataType := CastDataType;
end;

function TIDCastExpression.GetDataType: TIDType;
begin
  Result := FDataType;
end;

function TIDCastExpression.GetDisplayName: string;
begin
  Result := inherited GetDisplayName + ' as ' + FDataType.DisplayName
end;

{ TInterfaceScope }

constructor TInterfaceScope.Create(AUnit: TASTModule);
begin
  inherited Create(StrCICompare);
  FScopeType := stGlobal;
  FUnit := AUnit;
  Assert(Assigned(AUnit));
end;

function TInterfaceScope.GetName: string;
begin
  Result := 'unit$' + fUnit.Name + '$interface';
end;

{ TImplementationScope }

constructor TImplementationScope.Create(InterfaceScope: TScope);
begin
  inherited Create(StrCICompare);
  FScopeType := InterfaceScope.ScopeType;
  FIntfScope := InterfaceScope;
  fParent := InterfaceScope;
  FUnit := InterfaceScope.DeclUnit;
  Assert(Assigned(FUnit));
end;

function TImplementationScope.FindID(const Identifier: string): TIDDeclaration;
begin
  Result := inherited FindID(Identifier);
  if not Assigned(Result) then
    Result := FIntfScope.FindID(Identifier);
end;

function TImplementationScope.FindIDRecurcive(const ID: string): TIDDeclaration;
begin
  Result := inherited FindIDRecurcive(ID);

  if not Assigned(Result) then
  begin
    var List := TPascalUnit(fUnit).ImplImportedUnits;
    for var LIndex := List.Count - 1 downto 0 do
    begin
      var LUnit := TPascalUnit(List.Objects[LIndex]);
      Result := LUnit.IntfScope.FindID(ID);
      if Assigned(Result) then
        Exit;
    end;
  end;

  if not Assigned(Result) then
  begin
    var List := TPascalUnit(fUnit).IntfImportedUnits;
    for var LIndex := List.Count - 1 downto 0 do
    begin
      var LUnit := TPascalUnit(List.Objects[LIndex]);
      Result := LUnit.IntfScope.FindID(ID);
      if Assigned(Result) then
        Exit;
    end;
  end;
end;

function TImplementationScope.GetName: string;
begin
  Result := 'unit$' + fUnit.Name + '$implementation';
end;

function TImplementationScope.GetScopeClass: TScopeClass;
begin
  Result := scImplementation;
end;

{ TIDConstantList }

function IDConstArrayCompare(const NodeKey1, NodeKey2: TIDExpressions): Integer;
var
  i, c: Integer;
  CItem1, CItem2: TIDConstant;
  //Items1, Items2: TIDExpressionList;
begin
  //Items1 := NodeKey1.Value;
  //Items2 := NodeKey2.Value;
  c := Length(NodeKey1);
  Result := c - Length(NodeKey2);
  if Result = 0 then begin

  end;

  if Result = 0 then
  for i := 0 to c - 1 do begin
    CItem1 := TIDConstant(NodeKey1[i].Declaration);
    CItem2 := TIDConstant(NodeKey2[i].Declaration);
    Result := CItem1.CompareTo(CItem2);
    if Result <> 0 then
      Exit;
  end;
end;

{ TWithScope }

constructor TWithScope.Create(Parent: TScope; Expression: TIDExpression);
begin
  inherited Create(stWithScope, Expression.Declaration.Scope.DeclUnit);
  FExpression := Expression;
  FParent := Parent;
  Parent.AddChild(Self);
end;

function TWithScope.FindIDRecurcive(const ID: string): TIDDeclaration;
begin
  Result := FInnerScope.FindMembers(ID);
  if Assigned(Result) then begin
    //Expression := FExpression; todo: with !!!!
    Exit;
  end;
  Result := fOuterScope.FindIDRecurcive(ID);
  if not Assigned(Result) then
    Result := inherited FindIDRecurcive(ID);
end;

function TWithScope.GetName: string;
begin
  Result := 'with$' + fName;
end;

{ TIDOpenArray }

constructor TIDOpenArray.Create(Scope: TScope; const Name: TIdentifier);
begin
  CreateAsAnonymous(Scope);
end;

constructor TIDOpenArray.CreateAsAnonymous(Scope: TScope);
begin
  inherited CreateAsAnonymous(Scope);
  FDataTypeID := dtOpenArray;
  FDimensionsCount := 1;
end;

{ TIDRangeType }

constructor TIDRangeType.Create(Scope: TScope; const Identifier: TIdentifier);
begin
  inherited;
  fDataTypeID := dtRange;
  if Assigned(SYSUnit) then
    CreateStandardOperators;
end;

constructor TIDRangeType.CreateAsAnonymous;
begin
  inherited;
  fDataTypeID := dtRange;
  if Assigned(SYSUnit) then
    CreateStandardOperators;
end;

constructor TIDRangeType.CreateAsSystem(Scope: TScope; const Name: string);
begin
  inherited;
  fDataTypeID := dtRange;
  if Assigned(SYSUnit) then
    CreateStandardOperators;
end;

procedure TIDRangeType.CreateStandardOperators;
begin
  inherited;
  OverloadBinarOperator2(opEqual, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opNotEqual, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opLess, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opLessOrEqual, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opGreater, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opGreaterOrEqual, Self, SYSUnit._Boolean);
  OverloadExplicitFromAny(SYSUnit.Operators.ExplicitRangeFromAny);
  OverloadImplicitFromAny(SYSUnit.Operators.ImplicitRangeFromAny);
  OverloadImplicitToAny(SYSUnit.Operators.ImplicitRangeToAny);
end;

function TIDRangeType.GetDisplayName: string;
begin

  Result := DeclarationName(fLoDecl) + '..' + DeclarationName(fHiDecl);
  if Name <> '' then
    Result := Name + '(' + Result + ')';
end;

procedure TIDRangeType.SetHiDecl(const Value: TIDConstant);
begin
  fHiDecl := Value;
  HighBound := Value.AsInt64;
end;

procedure TIDRangeType.SetLoDecl(const Value: TIDConstant);
begin
  fLoDecl := Value;
  LowBound := Value.AsInt64;
end;

{ TIDProcedureType }

procedure TIDProcType.AddParam(const ID: TIdentifier; DataType: TIDType);
var
  Param: TIDVariable;
begin
  Param := TIDVariable.Create(Scope, ID);
  Param.IncludeFlags([VarParameter]);
  FParams := FParams + [Param];
end;

constructor TIDProcType.Create(Scope: TScope; const Identifier: TIdentifier);
begin
  inherited Create(Scope, Identifier);
  CreateStandardOperators;
  FDataTypeID := dtProcType;
end;

constructor TIDProcType.CreateAsAnonymous(Scope: TScope);
begin
  inherited CreateAsAnonymous(Scope);
  CreateStandardOperators;
  FDataTypeID := dtProcType;
end;

procedure TIDProcType.CreateStandardOperators;
begin
  inherited;
  FSysExplicitFromAny := SYSUnit.Operators.ExplicitTProcFromAny;
  FSysImplicitFromAny := SYSUnit.Operators.ImplicitTProcFromAny;
end;

procedure TIDProcType.Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer; AAppendName: Boolean);
begin
  inherited;

  if fProcClass = procReference then
    ABuilder.Append('reference to ');

  if Assigned(fResultType) then
    ABuilder.Append('function')
  else
    ABuilder.Append('procedure');

  Params2Str(ABuilder, fParams);

  if Assigned(fResultType) then
  begin
    ABuilder.Append(': ');
    ABuilder.Append(fResultType.DisplayName);
  end;

  if fProcClass = procMethod then
    ABuilder.Append(' of object ');

  GenericInstances2Str(ABuilder, ANestedLevel);

  GenericOverloads2Str(ABuilder, ANestedLevel);
end;

function TIDProcType.GetDataSize: Integer;
begin
  case fProcClass of
    procStatic: Result := Package.PointerSize;
    procMethod: Result := Package.PointerSize*2;
    procReference: Result := Package.PointerSize;
  else
    Result := 0;
  end;
end;

function TIDProcType.GetDisplayName: string;
var
  i: Integer;
  Param: TIDVariable;
begin
  Result := inherited;
  if Result = '' then
  begin
    for i := 0 to Length(FParams) - 1 do
    begin
      Param := FParams[i];
      Result := AddStringSegment(Result, Param.Name + ': ' + Param.DataType.DisplayName, '; ')
    end;
    if Assigned(FResultType) then
      Result := 'function(' + Result + '): ' + FResultType.DisplayName
    else
      Result := 'procedure(' + Result + ')';

    case fProcClass of
      procMethod: Result := Result + ' of object';
      procReference: Result := 'reference to ' + Result;
    end;
  end;
end;

function TIDProcType.GetIsStatic: Boolean;
begin
  Result := fProcClass = procStatic;
end;

function TIDProcType.InstantiateGeneric(ADstScope: TScope; ADstStruct: TIDStructure;
  const AContext: TGenericInstantiateContext): TIDDeclaration;
begin
  Result := inherited;
  if Assigned(Result) then
    Exit;

  if IsGeneric then
  begin
    if not Assigned(GenericDescriptor) or
       not GenericDescriptor.TryGetInstance(AContext.Args, {out} Result) then
    begin
      LogBegin('inst [type: %s, src: %s]', [ClassName, DisplayName]);
      var LNewType := MakeCopy as TIDProcType;

      if Assigned(GenericDescriptor) then
      begin
        // override name according to the generic arguments
        LNewType.ID := AContext.DstID;
        // add this instance to the its pool
        GenericDescriptor.AddGenericInstance(LNewType, AContext.Args);
      end;

      var LParamCnt := Length(Params);
      SetLength(LNewType.fParams, LParamCnt);

      for var LParamIndex := 0 to LParamCnt - 1 do
      begin
        var LNewParam := Params[LParamIndex].InstantiateGeneric(ADstScope, ADstStruct, AContext) as TIDParam;
        LNewType.Params[LParamIndex] := LNewParam;
      end;

      if Assigned(ResultType) then
        LNewType.ResultType := ResultType.InstantiateGeneric(ADstScope, ADstStruct, AContext) as TIDType;

      Result := LNewType;
      LogEnd('inst [type: %s, dst: %s]', [ClassName, Result.DisplayName]);
    end;
  end else
    Result := Self;
end;

{ TIDRangeConstant }

function TIDRangeConstant.AsInt64: Int64;
begin
  Result := 0;
  AbortWorkInternal('Not supported', []);
end;

function TIDRangeConstant.AsString: string;
begin
  Result := DisplayName;
end;

function TIDRangeConstant.AsVariant: Variant;
begin
  Result := NULL;
  AbortWorkInternal('Not supported', []);
end;

function TIDRangeConstant.ValueByteSize: Integer;
begin
  Result := 0;
  AbortWorkInternal('Not supported', []);
end;

function TIDRangeConstant.ValueDataType: TDataTypeID;
begin
  Result := dtRange;
end;

function TIDRangeConstant.CompareTo(Constant: TIDConstant): Integer;
begin
  Result := 0;
  AbortWorkInternal('Not supported', []);
end;

function TIDRangeConstant.GetDisplayName: string;
begin
  Result := FValue.LBExpression.DisplayName + '..' + FValue.HBExpression.DisplayName;
end;

procedure TIDRangeConstant.SetAsVariant(const AValue: Variant);
begin
  // todo:
end;

{ TProcScope }

procedure TProcScope.CopyFrom(SourceScope: TProcScope);
begin
  inherited CopyFrom(SourceScope);
  FParamsScope := SourceScope.ParamsScope;
end;

constructor TProcScope.CreateInBody(Parent: TScope);
begin
  inherited Create(StrCICompare);
  FScopeType := stLocal;
  FParent := Parent;
  FUnit := Parent.DeclUnit;
  Parent.AddChild(Self);
end;

constructor TProcScope.CreateInDecl(Parent: TScope; Proc: TIDProcedure);
begin
  inherited Create(StrCICompare);
  FScopeType := stLocal;
  FParent := Parent;
  FUnit := Parent.DeclUnit;
  fProc := Proc;
  Parent.AddChild(Self);
  FParamsScope := TParamsScope.Create(stLocal, Parent);
end;

function TProcScope.GetExplicitParams: TIDParamArray;
begin
  if Assigned(FParamsScope) then
    Result := FParamsScope.ExplicitParams
  else
    Result := [];
end;

function TProcScope.GetExplicitParamsCount: Integer;
begin
  Result := FParamsScope.Count;
end;

function TProcScope.GetName: string;
begin
  if Assigned(Proc) then
    Result := 'proc$' + Proc.Name
  else
    Result := 'proc$<unassigned>';
end;

function TProcScope.GetNameEx: string;
begin
  Result := format('%s(outer: %s)[%d]', [GetName, fOuterScope.GetNameEx, Count]);
end;

function TProcScope.GetScopeClass: TScopeClass;
begin
  Result := scProc;
end;

function TProcScope.FindID(const Identifier: string): TIDDeclaration;
begin
  Result := inherited FindID(Identifier);
  if not Assigned(Result) and Assigned(FParamsScope) then
    Result := FParamsScope.FindID(Identifier);
end;

function TProcScope.FindIDRecurcive(const ID: string): TIDDeclaration;
begin
  Result := inherited FindIDRecurcive(ID);
  if not Assigned(Result) and Assigned(fOuterScope) then
    Result := fOuterScope.FindIDRecurcive(ID);
end;

{ TIDClassType }

procedure TIDClass.AddGenericInterface(AGenricIntf: TIDGenericInstantiation);
begin
  FIntfGenericInstantiations := FIntfGenericInstantiations + [AGenricIntf];
end;

procedure TIDClass.AddInterface(const Intf: TIDInterface);
var
  Methods: TIDMethods;
  c: Integer;
begin
  if not Assigned(FInterfaces) then
  begin
    FInterfaces := TList<TIDInterface>.Create;
    FInterfacesMethods := TList<TIDMethods>.Create;
  end;
  FInterfaces.Add(Intf);
  c := Intf.MethodCount;
  SetLength(Methods, c);
  if c > 0 then
    FillChar(Methods[0], PTR_SIZE*c, #0);
  FInterfacesMethods.Add(Methods);
end;

procedure TIDClass.AncestorsDecl2Str(ABuilder: TStringBuilder);
begin
  if IsAbstract then
    ABuilder.Append(' abstract')
  else
  if IsSealed then
    ABuilder.Append(' sealed');

  if Assigned(fAncestorDecl) or Assigned(FInterfaces) then
  begin
    ABuilder.Append('(');

    if Assigned(fAncestorDecl) then
    begin
      ABuilder.Append(fAncestorDecl.DisplayName);
      if Assigned(FInterfaces) then
        ABuilder.Append(', ');
    end;

    if Assigned(FInterfaces) then
      for var LItemIndex := 0 to FInterfaces.Count - 1 do
      begin
         ABuilder.Append(FInterfaces[LItemIndex].DisplayName);
         if LItemIndex < FInterfaces.Count - 1 then
           ABuilder.Append(', ');
      end;

    ABuilder.Append(')');
  end;
end;

constructor TIDClass.Create(Scope: TScope; const Name: TIdentifier);
begin
  inherited;
  FDataTypeID := dtClass;
  CreateStandardOperators;
end;

constructor TIDClass.CreateAsSystem(Scope: TScope; const Name: string);
begin
  inherited;
  FDataTypeID := dtClass;
  CreateStandardOperators;
end;

procedure TIDClass.CreateStandardOperators;
begin
  if Assigned(SYSUnit) then
  begin
    OverloadBinarOperator2(opEqual, Self, SYSUnit._Boolean);
    OverloadBinarOperator2(opNotEqual, Self, SYSUnit._Boolean);
    OverloadBinarOperator2(opEqual, SYSUnit._NilPointer, SYSUnit._Boolean);
    OverloadBinarOperator2(opNotEqual, SYSUnit._NilPointer, SYSUnit._Boolean);
    OverloadImplicitToAny(SYSUnit.Operators.ImplicitClassToClass);
    OverloadExplicitToAny(SYSUnit.Operators.ExplicitClassToAny);
    OverloadExplicitFromAny(SYSUnit.Operators.ExplicitClassFromAny);
  end;
end;

destructor TIDClass.Destroy;
begin
  FInterfaces.Free;
  FInterfacesMethods.Free;
  inherited;
end;

function TIDClass.FindInterface(const AIntfName: string;
                                const AGenericParams: TIDTypeArray): TIDInterface;
begin
  var LFullIntfName := AIntfName;
  if Length(AGenericParams) > 0 then
    LFullIntfName := LFullIntfName + GenericArgsAsText(AGenericParams);

  for var LIndex := 0 to FInterfaces.Count - 1 do
  begin
    var LIntf := FInterfaces[LIndex];
    if SameText(LIntf.Name, LFullIntfName) then
      Exit(LIntf)
    else
    if SameText(LIntf.Name, AIntfName) then
    begin
      if (
           Assigned(LIntf.GenericDescriptor) and
           Assigned(AGenericParams) and
           LIntf.GenericDescriptor.SameParams(AGenericParams)
          ) or (
           not Assigned(LIntf.GenericDescriptor) and
           not Assigned(AGenericParams)
          ) then
      begin
        Exit(LIntf);
      end;
    end;
  end;
  Result := nil;
end;

function TIDClass.FindInterface(const AIntf: TIDInterface; AFindInAncestors: Boolean): Boolean;
begin
  Result := Assigned(FInterfaces) and (FInterfaces.IndexOf(AIntf) >= 0);
  if not Result and AFindInAncestors and Assigned(Ancestor) then
    Result := (Ancestor as TIDClass).FindInterface(AIntf, AFindInAncestors);
end;

{ TIDInterfaceType }

constructor TIDInterface.Create(Scope: TScope; const Name: TIdentifier);
begin
  inherited;
  FDataTypeID := dtInterface;
  OverloadImplicitTo(Self);
  if Assigned(SYSUnit) then begin
    OverloadBinarOperator2(opEqual, Self, SYSUnit._Boolean);
    OverloadBinarOperator2(opNotEqual, Self, SYSUnit._Boolean);
    OverloadBinarOperator2(opEqual, SYSUnit._NilPointer, SYSUnit._Boolean);
    OverloadBinarOperator2(opNotEqual, SYSUnit._NilPointer, SYSUnit._Boolean);
    OverloadExplicitFromAny(SysUnit.Operators.ExplicitInterfaceFromAny);
  end;
end;

{ TIDProperty }

constructor TIDProperty.Create(Scope: TScope; const Identifier: TIdentifier);
begin
  inherited Create(Scope, Identifier);
  FItemType := itProperty;
end;

procedure TIDProperty.Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer; AAppendName: Boolean);
begin
  ABuilder.Append(' ', ANestedLevel*2);
  ABuilder.Append('property ');
  ABuilder.Append(Name);

  if ParamsCount > 0 then
  begin
    ABuilder.Append('[');
    for var LParamIndex := 0 to FParams.Count - 1 do
      FParams[LParamIndex].Decl2Str(ABuilder);
    ABuilder.Append(']');
  end;

  ABuilder.Append(': ');
  TypeNameToString(DataType, ABuilder);
  if Assigned(FGetter) then
  begin
    ABuilder.Append(' read ');
    ABuilder.Append(FGetter.Name);
  end;
  if Assigned(FSetter) then
  begin
    ABuilder.Append(' write ');
    ABuilder.Append(FSetter.Name);
  end;

  if DefaultIndexedProperty then
    ABuilder.Append('; default');

  ABuilder.Append(';');
end;

function TIDProperty.GetParamsCount: Integer;
begin
  if Assigned(FParams) then
    Result := FParams.Count
  else
    Result := 0;
end;

function TIDProperty.InstantiateGeneric(ADstScope: TScope; ADstStruct: TIDStructure;
                                        const AContext: TGenericInstantiateContext): TIDDeclaration;
begin
  LogBegin('inst [type: %s, src: %s]', [ClassName, Name]);

  var LNewProp := MakeCopy as TIDProperty;
  LNewProp.DataType := DataType.InstantiateGeneric(ADstScope, ADstStruct, AContext) as TIDType;

  // instantiate indexed params
  if Assigned(FParams) and (FParams.Count > 0) then
  begin
    var LNewParams := TParamsScope.Create(stLocal, ADstScope);
    for var LIndex := 0 to FParams.Count - 1 do
    begin
      var LNewParam := FParams.Items[LIndex].MakeCopy as TIDParam;
      if LNewParam.IsGeneric then
        LNewParam.DataType := LNewParam.DataType.InstantiateGeneric(ADstScope, ADstStruct, AContext) as TIDType;
      LNewParams.AddExplicitParam(LNewParam);
    end;
    LNewProp.FParams := LNewParams;
  end;

  // todo: check signatures
  if Assigned(Getter) then
    LNewProp.FGetter := ADstScope.FindMembers(Getter.Name);
  // todo: check signatures
  if Assigned(Setter) then
    LNewProp.FSetter := ADstScope.FindMembers(Setter.Name);

  Result := LNewProp;
  LogEnd('inst [type: %s, dst: %s]', [ClassName, Result.Name]);
end;

function TIDProperty.MakeCopy: TIDDeclaration;
begin
  Result := inherited;
  TIDProperty(Result).DefaultIndexedProperty := DefaultIndexedProperty;
end;

{ TMethodScope }

constructor TMethodScope.CreateInDecl(OuterScope, Parent: TScope; AProc: TIDProcedure);
begin
  inherited CreateInDecl(Parent, AProc);
  FProc := AProc;
  fOuterScope := OuterScope;
end;

function TMethodScope.FindIDRecurcive(const ID: string): TIDDeclaration;
begin
  // search in local scope
  Result := FindID(ID);
  if Assigned(Result) then
    Exit;

  // serach in local parent scopes
  var ALocalScope := FParent;
  while Assigned(ALocalScope) and (ALocalScope.ScopeType <> stStruct) do
  begin
    Result := ALocalScope.FindID(ID);
    if Assigned(Result) then
      Exit;
    ALocalScope := ALocalScope.Parent;
  end;

  // search in the impl scope (without uses)
  var AOuterDecl := fOuterScope.FindID(ID);

  // serach in local parent scopes
  var AStructScope := FParent as TStructScope;
  while Assigned(AStructScope) and (AStructScope.ScopeType = stStruct) do
  begin
    var AStructDecl := AStructScope.FindID(ID);
    if Assigned(AStructDecl) then
    begin
      if Assigned(AOuterDecl) and (AOuterDecl.ID.TextPosition.Col > AStructDecl.ID.TextPosition.Col) then
        Result := AOuterDecl
      else
        Result := AStructDecl;
      Exit;
    end;
    AStructScope := AStructScope.AncestorScope;
  end;

  // search in the all impl uses scopes
  Result := fOuterScope.FindIDRecurcive(ID);
  if Assigned(Result) then
    Exit;

  // search in the all intf uses scopes
  Result := fParent.FindIDRecurcive(ID);
end;

function TMethodScope.GetName: string;
begin
  if Assigned(fProc) then
  begin
    if Assigned(fProc.Struct) then
      Result := 'method$' + fProc.Struct.Name + ':' + fProc.Name
    else
      Result := 'method$<unassigned>:' + fProc.Name;
  end else
    Result := 'method$<unassigned>';
end;

function TMethodScope.GetStruct: TIDStructure;
begin
  Result := fProc.Struct;
end;

{ TIDField }

constructor TIDField.Create(Struct: TIDStructure; const Identifier: TIdentifier);
begin
  inherited Create(Struct.Members, Identifier);
  Include(fFlags, VarIsField);
  fStruct := Struct;
end;

procedure TIDField.Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer; AAppendName: Boolean);
begin
  ABuilder.Append(' ', ANestedLevel*2);
  ABuilder.Append(Name);
  ABuilder.Append(': ');
  TypeNameToString(DataType, ABuilder);
  ABuilder.Append(';');
end;

function TIDField.GetFieldIndex: Integer;
var
  St: TIDStructure;
begin
  Result := FIndex;
  if not Assigned(Struct) then
    Exit;
  st := Struct.Ancestor;
  while Assigned(st) do begin
    Result := Result + St.FieldsCount;
    st := St.Ancestor;
  end;
end;

function TIDField.GetIndex: Integer;
begin
  Result := FieldIndex;
end;

{ TIDGenericParam }

constructor TIDGenericParam.Create(Scope: TScope; const ID: TIdentifier);
begin
  inherited Create(Scope, ID);
  FDataTypeID := dtGeneric;
end;

procedure TIDGenericParam.Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer; AAppendName: Boolean);
begin
  ABuilder.Append(' ', ANestedLevel*2);
  ABuilder.Append('<');
  ABuilder.Append(DisplayName);
  ABuilder.Append('>');
end;

function TIDGenericParam.InstantiateGeneric(ADstScope: TScope; ADstStruct: TIDStructure;
                                            const AContext: TGenericInstantiateContext): TIDDeclaration;
begin
  Result := nil;
  for var AIndex := 0 to Length(AContext.Params) - 1 do
  begin
    if SameText(AContext.Params[AIndex].Name, Name) then
    begin
      Result := AContext.Args[AIndex];
      Exit;
    end;
  end;
  AbortWorkInternal('Unknonw arg type for %s', [AContext.DstID.Name], AContext.DstID.TextPosition);
end;

function TIDGenericParam.SameConstraint(ADstParam: TIDGenericParam): Boolean;
begin
  Result := (ADstParam.Constraint = gsNone) or
            (Constraint = ADstParam.Constraint) and
            (ConstraintType = ADstParam.ConstraintType);
end;

{ TIDPairList }

constructor TIDPairList.Create;
begin
  inherited Create(IDCompare);
end;

function TIDPairList.GetItem(const Key: TIDDeclaration): TObject;
var
  Node: PAVLNode;
begin
  Node := Find(Key);
  if Assigned(Node) then
    Result := Node.Data
  else
    Result := nil;
end;

{ TIDAlias }

constructor TIDAlias.CreateAlias(Scope: TScope; const ID: TIdentifier; Decl: TIDDeclaration);
begin
  inherited Create(Scope, ID);
  FItemType := itAlias;
  FOriginalDecl := Decl.Original;
end;

function TIDAlias.GetOriginalDecl: TIDDeclaration;
begin
  Result := FOriginalDecl;
end;

{ TGenericDescriptor }

procedure TGenericDescriptor.AddGenericInstance(Decl: TIDDeclaration; const Args: TIDTypeArray);
var
  c: Integer;
begin
  c := Length(FGenericInstances);
  SetLength(FGenericInstances, c + 1);
  FGenericInstances[c].Args := Args;
  FGenericInstances[c].Instance := Decl;
end;

function GetProcName(Proc: TIDProcedure; WithParamsDataTypes: Boolean): string;
var
  Param: TIDVariable;
  ParamsStr: string;
  i: Integer;
begin
  if Assigned(Proc.Struct) then
    Result := Proc.ProcKindName + ' ' + Proc.Struct.DisplayName + '.' + Proc.DisplayName
  else
    Result := Proc.ProcKindName + ' ' + Proc.DisplayName;

  if WithParamsDataTypes then
  begin
    for i := 0 to Length(Proc.ExplicitParams) - 1 do begin
      Param := Proc.ExplicitParams[i];
      ParamsStr := AddStringSegment(ParamsStr, Param.DataType.DisplayName, ', ');
    end;
    Result := Result + '(' + ParamsStr + ')';
  end;
end;

class function TGenericDescriptor.Create(Scope: TScope): PGenericDescriptor;
begin
  New(Result);
  Result.FScope := TScope.Create(stLocal, Scope);
  {$IFDEF DEBUG} Result.FScope.FName := 'gdescriptor_scope';{$ENDIF}
end;

procedure TGenericDescriptor.Decl2Str(ABuilder: TStringBuilder);
begin
  ABuilder.Append('<');
  for var LIndex := 0 to Length(FGenericParams) - 1 do
  begin
    ABuilder.Append(GenericParams[LIndex].Name);
    if LIndex < Length(FGenericParams) - 1 then
      ABuilder.Append(', ');
  end;
  ABuilder.Append('>');
end;

function TGenericDescriptor.FindType(const ATypeName: string): TIDGenericParam;
var
  Decl: TIDDeclaration;
begin
  Decl := FScope.FindID(ATypeName);
  if Decl is TIDGenericParam then
    Result := TIDGenericParam(Decl)
  else
    Result := nil;
end;

function TGenericDescriptor.GetParamsCount: Integer;
begin
  Result := Length(FGenericParams);
end;

function TGenericDescriptor.IndexOfType(const ATypeName: string): Integer;
begin
  for var LIndex := 0 to High(FGenericParams) do
    if SameText(ATypeName, FGenericParams[LIndex].Name) then
       Exit(LIndex);
  Result := -1;
end;

function TGenericDescriptor.IsEqual(ADescriptor: PGenericDescriptor): Boolean;
begin
  var AParamsCount := Length(FGenericParams);
  if AParamsCount = Length(ADescriptor.GenericParams) then
  begin
    for var AIndex := 0 to AParamsCount - 1 do
    begin
      var AParam1 := FGenericParams[AIndex] as TIDGenericParam;
      var AParam2 := ADescriptor.GenericParams[AIndex] as TIDGenericParam;
      if (AParam1.Constraint <> AParam2.Constraint) or
         (AParam1.ConstraintType <> AParam2.ConstraintType) then
        Exit(False);
    end;
    Result := True;
  end else
    Result := False;
end;

function TGenericDescriptor.SameParams(const AParams: TIDTypeArray): Boolean;
begin
  var LParamsCount := Length(AParams);
  Result := (LParamsCount = Length(FGenericParams));
//  if Result then
//  begin
//    for var LParamIndex := 0 to LParamsCount - 1 do
//    begin
//      var LThatParam := AParams[LParamIndex];
//      var LThisParam := FGenericParams[LParamIndex];
//    end;
//  end;
end;

function TGenericDescriptor.TryGetInstance(const AArguments: TIDTypeArray; out ADecl: TIDDeclaration): Boolean;
begin
  for var LItemIndex := 0 to Length(GenericInstances) - 1 do
  begin
    var LItem := PGenericInstance(@GenericInstances[LItemIndex]);
    var LArgsCount := Length(AArguments);
    if LArgsCount < Length(LItem.Args) then
      AbortWorkInternal('Wrong length generics arguments');
    for var LArgIndex := 0 to LArgsCount - 1 do
    begin
      // simple check
      var LDstType := LItem.Args[LArgIndex].ActualDataType;
      var LSrcType := AArguments[LArgIndex].ActualDataType;
      if LDstType <> LSrcType then
      begin
        if (LSrcType.DataTypeID = dtGeneric) and (LDstType.DataTypeID = dtGeneric) then
          Continue;
        LItem := nil;
        Break;
      end;
    end;
    if Assigned(LItem) then
    begin
      ADecl := LItem.Instance;
      Exit(True);
    end;
  end;
  ADecl := nil;
  Result := False;
end;

{ TIDNameSpace }

constructor TIDNameSpace.Create(Scope: TScope; const Identifier: TIdentifier);
begin
  inherited Create(Scope, Identifier);
  FMembers := TScope.Create(stGlobal, Scope);
  {$IFDEF DEBUG}FMembers.FName := 'namespace_' + Identifier.Name + '_scope';{$ENDIF}
end;

{ TIDUnit }

constructor TIDUnit.Create(Scope: TScope; AUnit: TASTModule);
begin
  CreateFromPool;
  FScope := Scope;
  FID := TPascalUnit(AUnit)._ID;
  fModule := AUnit;
  FMembers := TPascalUnit(AUnit).IntfScope;
  ItemType := itUnit;
end;

procedure TIDUnit.Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer; AAppendName: Boolean);
begin
  ABuilder.Append('unit ');
  ABuilder.Append(Name);
end;

{ TIDClassOf }

constructor TIDClassOf.Create(Scope: TScope; const Identifier: TIdentifier);
begin
  inherited;
  DataTypeID := dtClassOf;
  CreateStandardOperators;
end;

constructor TIDClassOf.CreateAsAnonymous(Scope: TScope; ReferenceType: TIDType);
begin
  inherited;
  DataTypeID := dtClassOf;
  CreateStandardOperators;
end;

constructor TIDClassOf.CreateAsSystem(Scope: TScope; const Name: string);
begin
  inherited;
  FDataTypeID := dtClassOf;
  CreateStandardOperators;
end;

procedure TIDClassOf.CreateStandardOperators;
begin
  if not Assigned(SYSUnit) then
    Exit;
  inherited CreateStandardOperators;
end;

procedure TIDClassOf.Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer; AAppendName: Boolean);
begin
  ABuilder.Append(' ', ANestedLevel*2);
  ABuilder.Append('type ');
  ABuilder.Append(Name);
  ABuilder.Append(' = class of ');
  TypeNameToString(ReferenceType, ABuilder);
end;

function TIDClassOf.GetDisplayName: string;
begin
  if Name <> '' then
    Result := Name
  else if Assigned(FReferenceType) then
    Result := 'class of ' + FReferenceType.Name
  else
    Result := '';
end;

{ TIDWeekRef }

constructor TIDWeekRef.CreateAsAnonymous(Scope: TScope; ReferenceType: TIDType);
begin
  inherited CreateAsAnonymous(Scope, ReferenceType);
  FDataTypeID := dtWeakRef;
end;

function TIDWeekRef.GetDisplayName: string;
begin
  if Assigned(FReferenceType) then
    Result := 'Weak of ' + FReferenceType.DisplayName
  else
    Result := 'Weak of NULL';
end;

{ TStructScope }

constructor TStructScope.CreateAsStruct(Parent: TScope; Struct: TIDStructure; DeclUnit: TASTModule);
begin
  inherited Create(StrCICompare);
  FScopeType := stStruct;
  FParent := Parent;
  FStruct := Struct;
  FUnit := DeclUnit;
  Assert(Assigned(DeclUnit));
  if Assigned(Parent) then
    Parent.AddChild(Self);

  if Assigned(Struct.Ancestor) then
    FAncestorScope := Struct.Ancestor.Members;
end;

function TStructScope.FindIDRecurcive(const ID: string): TIDDeclaration;
begin
  Result := inherited FindIDRecurcive(ID);
  if not Assigned(Result) and Assigned(FAncestorScope) then
    Result := FAncestorScope.FindIDRecurcive(ID);
end;

procedure TStructScope.FindIDRecurcive(const ID: string; var ADeclArray: TIDDeclArray);
begin
  inherited FindIDRecurcive(ID, ADeclArray);
  if Assigned(FAncestorScope) and (FAncestorScope <> Self) then
    FAncestorScope.FindIDRecurcive(ID, ADeclArray);
end;

function TStructScope.FindMembers(const ID: string): TIDDeclaration;
begin
  Result := FindID(ID);
  if Assigned(Result) then
    Exit;

  Result := FindInAdditionalScopes(ID);
  if Assigned(Result) then
    Exit;

  if Assigned(FAncestorScope) then
    Result := FAncestorScope.FindMembers(ID)
  else
    Result := nil;
end;

function TStructScope.GetName: string;
begin
  Result := 'struct$' + fStruct.Name;
end;

function TStructScope.GetNameEx: string;
begin
  var LAncestorScopeName := 'nil';
  if Assigned(fAncestorScope) then
    LAncestorScopeName := fAncestorScope.GetNameEx;
  Result := format('%s(ancestor: %s)[%d]', [GetName, LAncestorScopeName, Count]);
end;

function TIDClass.GetDataSize: Integer;
begin
  Result := Package.PointerSize;
end;

function TIDClass.GetExtraFlags: Byte;
begin
  Result := 0;
end;

function TIDClass.GetInterface(Index: Integer): TIDInterface;
begin
  Result := FInterfaces[Index]
end;

function TIDClass.GetInterfacesCount: Integer;
begin
  if Assigned(FInterfaces) then
    Result := FInterfaces.Count
  else
    Result := 0;
end;

function TIDClass.GetIsAbstract: Boolean;
begin
  Result := cdfAbstract in FClassDeclFlags;
end;

function TIDClass.GetIsManaged: Boolean;
begin
  Result := True;
end;

function TIDClass.GetIsSealed: Boolean;
begin
  Result := cdfSealed in FClassDeclFlags;
end;

function TIDClass.GetStructKeyword: string;
begin
  Result := 'class';
end;

procedure TIDClass.IncRefCount(RCPath: UInt32);
begin
  inherited;

end;

procedure TIDClass.InstantiateGenericAncestors(ADstScope: TScope; ADstStruct: TIDStructure;
  const AContext: TGenericInstantiateContext);
begin
  inherited;
  // add none-generic interfaces
  if Assigned(FInterfaces) then
  begin
    for var LIntfIndex := 0 to FInterfaces.Count - 1 do
    begin
      var LIntf := FInterfaces[LIntfIndex];
      if not LIntf.IsGeneric then
        TIDClass(ADstStruct).AddInterface(LIntf);
    end;
  end;

  // instantiate implemented interfaces
  for var LIntfIndex := 0 to Length(FIntfGenericInstantiations) - 1 do
  begin
    var LOldIntf := FIntfGenericInstantiations[LIntfIndex];
    var LNewIntf := LOldIntf.InstantiateGeneric(ADstScope, {ADestStruct:} nil, AContext) as TIDInterface;
    TIDClass(ADstStruct).AddInterface(LNewIntf);
  end;
end;

procedure TIDClass.DecRefCount(RCPath: UInt32);
begin
  inherited;

end;

procedure TIDClass.MapInterfaceMethod(const Intf: TIDInterface; IntfMethod, ImplMethod: TIDProcedure);
var
  Idx: Integer;
  Methods: TIDMethods;
begin
  Idx := FInterfaces.IndexOf(Intf);
  Assert(Idx >= 0);
  Methods := FInterfacesMethods[Idx];
  Methods[IntfMethod.Index] := ImplMethod;
end;

function TIDInterface.GetIsManaged: Boolean;
begin
  Result := True;
end;

function TIDInterface.GetStructKeyword: string;
begin
  Result := 'interface';
end;

function TIDInterface.MatchImplicitFrom(ASrc: TIDType): Boolean;
begin
  Result := (ASrc.DataTypeID = dtClass) and TIDClass(ASrc).FindInterface(Self, {AFindInAncestors:} True);
end;

function TIDInterface.MatchImplicitTo(ADst: TIDType): Boolean;
begin
  Result := (ADst.DataTypeID = dtInterface) and
            (IsInheritsForm(ADst as TIDStructure) or (GenericOrigin = ADst));
end;

{ TIDGuidConstant }

function TIDGuidConstant.AsInt64: Int64;
begin
  Result := 0;
  AbortWorkInternal('Not supported', []);
end;

function TIDGuidConstant.AsString: string;
begin
  Result := '[' + GUIDToString(FValue) + ']';
end;

function TIDGuidConstant.AsVariant: Variant;
begin
  AbortWorkInternal('Not supported', []);
end;

function TIDGuidConstant.ValueByteSize: Integer;
begin
  Result := SizeOf(FValue);
end;

function TIDGuidConstant.ValueDataType: TDataTypeID;
begin
  Result := dtGuid;
end;

function TIDGuidConstant.CompareTo(Constant: TIDConstant): Integer;
begin
  Result := -1;
end;

procedure TIDGuidConstant.SetAsVariant(const AValue: Variant);
begin
  // todo:
end;

{ TIDDrefExpression }

constructor TIDDrefExpression.Create(Src: TIDExpression);
begin
  CreateFromPool;
  FSrc := Src;
  FDeclaration := Src.Declaration;
  FTextPosition := Src.TextPosition;
end;

function TIDDrefExpression.GetDataType: TIDType;
begin
  Result := (FSrc.DataType as TIDPointer).ReferenceType;
  if not Assigned(Result) then
    Result := FSrc.Declaration.SYSUnit._UntypedReference;
end;

{ TIDClosure }

function TIDClosure.CaptureByReference: Boolean;
begin
  Result := False;
end;

constructor TIDClosure.CreateClosure(const DeclProc, RunProc: TIDProcedure);
begin
  inherited CreateAsAnonymous(DeclProc.Scope);
  FDataTypeID := dtClass;
  FDeclProc := DeclProc;
  FCapturedVars := TCapturedVars.Create(IDVarCompare);
//  Members.ProcSpace.Add(RunProc);
  OverloadImplicitTo(Self, SYSUnit.Operators.ImplicitClosureToTMethod);
end;

destructor TIDClosure.Destroy;
begin
  FCapturedVars.Free;
  inherited;
end;

function TIDClosure.GetCapturedVar(const CapturedVar: TIDVariable): TIDField;
var
  Node: TCapturedVars.PAVLNode;
  CVRec: TCapturedVarRec;
begin
  Node := FCapturedVars.Find(CapturedVar);
  if not Assigned(Node) then
  begin
    Result := AddField(CapturedVar.Name, CapturedVar.DataType);
    CVRec.CapturedVar := CapturedVar;
    CVRec.Field := Result;
    CVRec.ByRef := False;
    FCapturedVars.InsertNode(CapturedVar, CVRec);
  end else
    Result := Node.Data.Field;
end;

function TIDClosure.GetDisplayName: string;
begin
  Result := FDeclProc.Name + '$Closure' + IntToStr(Index);
end;

{ TIDString }

constructor TIDString.CreateAsSystem(Scope: TScope; const Name: string);
begin
  inherited CreateAsSystem(Scope, Name);
end;

procedure TIDString.Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer; AAppendName: Boolean);
begin
  ABuilder.Append(' ', ANestedLevel*2);
  ABuilder.Append('type ');
  ABuilder.Append(Name);
end;

{ TIDCastedCallExpression }

function TIDCastedCallExpression.GetDataType: TIDType;
begin
  Result := FDataType;
end;

{ TIDArrayExpression }

function TIDArrayExpression.GetDataType: TIDType;
var
  dataType: TIDType;
begin
  if Assigned(fDataType) then
    Exit(fDataType);

  if FDeclaration.DataTypeID = dtPointer then
    dataType := TIDPointer(FDeclaration.DataType).ReferenceType
  else
    dataType := FDeclaration.DataType;

  if dataType is TIDArray then
    Result := (dataType as TIDArray).ElementDataType
  else
    Result := dataType;
end;

function TIDArrayExpression.GetDisplayName: string;
var
  Str: string;
begin
  for var i := 0 to Length(fIndexes) - 1 do
    Str := AddStringSegment(Str, fIndexes[i].DisplayName);
  Result := inherited Declaration.DisplayName + '[' + Str + ']';
end;

{ TIDRecordConstant }

function TIDRecordConstant.AsInt64: Int64;
begin
  Result := 0;
  AbortWorkInternal('Not supported', []);
end;

function TIDRecordConstant.AsString: string;
begin
  Result := '';
end;

function TIDRecordConstant.AsVariant: Variant;
begin
  Result := null;
end;

function TIDRecordConstant.CompareTo(Constant: TIDConstant): Integer;
begin
  Result := 0;
end;

procedure TIDRecordConstant.SetAsVariant(const AValue: Variant);
begin
  // todo:
end;

function TIDRecordConstant.ValueByteSize: Integer;
begin
  Result := 0;
end;

function TIDRecordConstant.ValueDataType: TDataTypeID;
begin
  Result := dtRecord;
end;

{ TASTDelphiLabel }

constructor TASTDelphiLabel.Create(Scope: TScope; const Identifier: TIdentifier);
begin
  inherited;
  ItemType := itLabel;
end;

{ TIDStaticArray }

constructor TIDStaticArray.Create(Scope: TScope; const ID: TIdentifier);
begin
  inherited;
  DataTypeID := dtStaticArray;
end;

constructor TIDStaticArray.CreateAnonymous(Scope: TScope; BaseType: TIDType);
begin
  inherited;
  DataTypeID := dtStaticArray;
end;

procedure TIDStaticArray.CreateStandardOperators;
begin
  inherited;
  AddBinarySysOperator(opAdd, SYSUnit.Operators.StaticArray_Add);
  OverloadExplicitToAny(SYSUnit.Operators.ExplicitStaticArrayToAny);
end;

procedure TIDStaticArray.Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer; AAppendName: Boolean);
begin
  inherited;
  ABuilder.Append('array [');

  for var LIndex := 0 to DimensionsCount - 1 do
  begin
    // TODO: implement printing other ord type, like enums, etc
    ABuilder.Append(FDimensions[LIndex].FLBound.ToString);
    ABuilder.Append('..');
    ABuilder.Append(FDimensions[LIndex].FHBound.ToString);
  end;

  ABuilder.Append('] of ');

  if Assigned(ElementDataType) then
    ABuilder.Append(ElementDataType.DisplayName)
  else
    ABuilder.Append('<null>');

  GenericInstances2Str(ABuilder, ANestedLevel);

  GenericOverloads2Str(ABuilder, ANestedLevel);
end;

{ TUnknownIDExpression }

constructor TUnknownIDExpression.Create(const ID: TIdentifier);
begin
  fID := ID;
  fTextPosition := ID.TextPosition;
end;

function TUnknownIDExpression.GetDataType: TIDType;
begin
  Result := nil;
end;

function TUnknownIDExpression.GetDisplayName: string;
begin
  Result := Format('Unknown identifier at %d : %d', [FTextPosition.Row, FTextPosition.Col]);
end;

{ TIDHelper }

procedure TDlphHelper.AncestorsDecl2Str(ABuilder: TStringBuilder);
begin
  ABuilder.Append(fTarget.Name);
end;

function TDlphHelper.GetStructKeyword: string;
begin
  if fTarget.DataTypeID = dtClass then
    Result := 'class helper for '
  else
    Result := 'record helper for ';
end;

procedure TDlphHelper.SetTarget(const Value: TIDType);
begin
  fTarget := Value;
  if Value is TIDStructure then
  begin
    fMembers.FAncestorScope := TIDStructure(Value).Members;
    fAncestorDecl := Value as TIDStructure;
  end;
end;

{ TWithAlias }

constructor TWithAlias.CreateAlias(Scope: TScope; OriginalDecl: TIDDeclaration; Expression: TIDExpression);
begin
  inherited Create(Scope, OriginalDecl.ID);
  fItemType := itAlias;
  fOriginalDecl := OriginalDecl;
  fExpression := Expression;
end;

{ TIDNullPointerType }

procedure TIDNullPointerType.CreateStandardOperators;
begin
  inherited;
  OverloadImplicitToAny(SYSUnit.Operators.ImplicitNullPtrToAny);
end;

{ TIDSetConstant }

function TIDSetConstant.AsInt64: Int64;
begin
  Result := 0;
end;

function TIDSetConstant.AsString: string;
begin
  Result := '[' + GetExpressionsNames(Value) + ']';
end;

function TIDSetConstant.AsVariant: Variant;
begin
  Result := null;
end;

function TIDSetConstant.CompareTo(Constant: TIDConstant): Integer;
begin
  Result := 0;
end;

procedure TIDSetConstant.SetAsVariant(const AValue: Variant);
begin
  // todo:
end;

function TIDSetConstant.ValueByteSize: Integer;
begin
  Result := 0;
end;

function TIDSetConstant.ValueDataType: TDataTypeID;
begin
  Result := dtSet;
end;

{ TIDPointerConstant }

function TIDPointerConstant.AsInt64: Int64;
begin
  Result := 0;
end;

procedure TIDPointerConstant.AssignValue(Source: TIDConstant);
begin
  FValue := Source;
end;

function TIDPointerConstant.AsString: string;
begin
  if Assigned(FValue) then
  begin
    if FValue.ItemType = itConst then
      Result := TIDConstant(FValue).AsString
    else
    if FValue.ItemType = itProcedure then
      Result := TIDProcedure(FValue).Name
    else
      AbortWorkInternal('Unsupported Constant Declaration');
 end else
   Result := '<uknown>';
end;

function TIDPointerConstant.AsVariant: Variant;
begin
  Result := 0;
end;

function TIDPointerConstant.CompareTo(Constant: TIDConstant): Integer;
begin
  Result := 0;
end;

procedure TIDPointerConstant.SetAsVariant(const AValue: Variant);
begin
  // todo:
end;

function TIDPointerConstant.ValueByteSize: Integer;
begin
  Result := 0;  // todo
end;

function TIDPointerConstant.ValueDataType: TDataTypeID;
begin
  Result := dtPointer;
end;

{ TIDUntypedRef }

constructor TIDUntypedRef.CreateAsSystem(Scope: TScope; const Name: string);
begin
  inherited;
  fDataTypeID := dtUntypedRef;
end;

procedure TIDUntypedRef.Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer; AAppendName: Boolean);
begin
  ABuilder.Append(' ', ANestedLevel*2);
  ABuilder.Append('<');
  ABuilder.Append(DisplayName);
  ABuilder.Append('>');
end;

{ TIDSysVariativeType }

constructor TIDSysVariativeType.CreateAsSystem(Scope: TScope; const Types: TIDTypeArray);
begin
  inherited CreateAsSystem(Scope, '');
  FTypes := Types;
end;

procedure TIDSysVariativeType.CreateStandardOperators;
begin
  inherited;
  OverloadImplicitFromAny(SysUnit.Operators.ImplicitSysVarFromAny);
end;

function TIDSysVariativeType.GetDisplayName: string;
begin
  Result := '<';
  for var AType in FTypes do
    Result := AddStringSegment(Result, AType.DisplayName, ' or ');
  Result := Result + '>';
end;

{ TConditionalScope }

function TConditionalScope.GetName: string;
begin
  Result := 'conditional$scope';
end;

{ TRecordInitScope }

function TRecordInitScope.GetName: string;
begin
  Result := fStructType.Name + '$record_init';
end;

function SameTypes(ASrcType, ADstType: TIDType): Boolean;
begin
  Result := ASrcType.ActualDataType = ADstType.ActualDataType;
  if not Result then
  begin
    if ASrcType.IsGeneric and ADstType.IsGeneric then
    begin
      if (ASrcType.ClassType = TIDGenericParam) and
         (ADstType.ClassType = TIDGenericParam) then
        Result := TIDGenericParam(ASrcType).SameConstraint(TIDGenericParam(ADstType))
      else
      if (ASrcType.ClassType = TIDGenericInstantiation) and
         (ADstType.ClassType = TIDGenericInstantiation) then
        Result := TIDGenericInstantiation(ASrcType).SameType(TIDGenericInstantiation(ADstType))
      else
        Result := ((ASrcType.ClassType = TIDGenericInstantiation) and
                   (ADstType.ClassType = TIDGenericParam)) or
                   ((ASrcType.ClassType = TIDGenericParam) and
                    (ADstType.ClassType = TIDGenericInstantiation));
    end else
      Result := ADstType.IsGeneric;

    if not Result then
    begin
      if (ASrcType is TIDArray) and (ADstType is TIDArray) then
      begin
        if (ADstType.DataTypeID = dtOpenArray) or
           (ASrcType.DataTypeID = ADstType.DataTypeID) then
          Result := SameTypes(TIDArray(ASrcType).ElementDataType,
                              TIDArray(ADstType).ElementDataType);
      end;
    end;
  end;
end;

function SameProcSignTypes(ASrcType, ADstType: TIDType): Boolean;

  function GetActualType(AType: TIDType): TIDType;
  begin
    if (AType is TIDAliasType) and not TIDAliasType(AType).NewType then
      Result := AType.ActualDataType
    else
      Result := AType;
  end;

begin
  Result := GetActualType(ASrcType) = GetActualType(ADstType);
  if not Result then
  begin
    if (ASrcType.ClassType = TIDGenericParam) and
       (ADstType.ClassType = TIDGenericParam) then
      Result := (ASrcType.Name = ADstType.Name)
    else
    if (ASrcType is TIDArray) and (ADstType is TIDArray) then
    begin
      if (ADstType.DataTypeID = dtOpenArray) or
         (ASrcType.DataTypeID = ADstType.DataTypeID) then
        Result := SameProcSignTypes(TIDArray(ASrcType).ElementDataType,
                                    TIDArray(ADstType).ElementDataType);
    end;
  end;
end;

function IsGenericTypeThisStruct(Scope: TScope; Struct: TIDType): Boolean;
begin
  while Assigned(Scope) do
  begin
    if (Scope.ScopeType = stStruct) and
       (TStructScope(Scope).Struct = Struct) then
      Exit(True);
    Scope := Scope.Parent;
  end;
  Result := False;
end;

function GenericNeedsInstantiate(AGenericArgs: TIDExpressions): Boolean;
begin
  for var LArgIndex := 0 to High(AGenericArgs) do
  begin
    var LArgType := AGenericArgs[LArgIndex].AsType;
    if (LArgType is TIDGenericParam) or LArgType.IsGeneric then
     Exit(False);
  end;
  Result := True;
end;

{ TParamsScope }

procedure TParamsScope.AddExplicitParam(AParam: TIDParam);
begin
  if not InsertID(AParam) then
    TASTDelphiErrors.ID_REDECLARATED(AParam);

  // add the explicit param to the explicits array
  var LArrayLen := Length(FExplicitParams);
  SetLength(FExplicitParams, LArrayLen + 1);
  FExplicitParams[LArrayLen] := AParam;
end;

procedure TParamsScope.AddImplicitParam(AParam: TIDParam);
begin
  if not InsertID(AParam) then
    TASTDelphiErrors.ID_REDECLARATED(AParam);
end;

function TParamsScope.GetItem(AIndex: Integer): TIDParam;
begin
  Result := FExplicitParams[AIndex];
end;


{ TIDGenericInstantiation }


constructor TIDGenericInstantiation.CreateInstantiation(AScope: TScope;
                                                        AGenericType: TIDType;
                                                        const AGenericArguments: TIDExpressions);
begin
  inherited CreateAlias(AScope, AGenericType.ID, AGenericType);
  FGenericArguments := AGenericArguments;
end;

function TIDGenericInstantiation.GetDisplayName: string;
begin
  Result := Name + GenericArgsAsText(FGenericArguments);
end;

function TIDGenericInstantiation.GetIsGeneric: Boolean;
begin
  Result := True;
end;

function TIDGenericInstantiation.InstantiateGeneric(ADstScope: TScope; ADstStruct: TIDStructure;
                                                    const AContext: TGenericInstantiateContext): TIDDeclaration;
var
  LNewContext: TGenericInstantiateContext;
begin
  // Original - is the generic type that needs to be instantiated
  LNewContext.SrcDecl := Original;
  LNewContext.Params := Original.GenericDescriptor.GenericParams;
  var LGParamsCount := Length(LNewContext.Params);
  SetLength(LNewContext.Args, LGParamsCount);

  var LStrSufix := '';
  for var LIndex := 0 to LGParamsCount - 1 do
  begin
    var LGArg := FGenericArguments[LIndex].AsType;
    // if an argument is a generic param from the outer context, find the related argument from that context
    if LGArg is TIDGenericParam then
      LGArg := AContext.GetArgByParam(TIDGenericParam(LGArg))
    else
    // if an argument is a generic instantiation, instantiate it
    if LGArg is TIDGenericInstantiation then
      LGArg := LGArg.InstantiateGeneric(ADstScope, ADstStruct, AContext) as TIDType;

    LNewContext.Args[LIndex] := LGArg;
    LStrSufix := AddStringSegment(LStrSufix, LGArg.DisplayName, ', ');
  end;

  LNewContext.DstID.Name := Original.Name + '<' + LStrSufix + '>';
  LNewContext.DstID.TextPosition := TextPosition;

  if not Original.GenericDescriptor.TryGetInstance(LNewContext.Args, {out} Result) then
  begin
    LogBegin('inst [type: %s, src: %s]', [ClassName, Original.DisplayName]);
    Result := Original.InstantiateGeneric(ADstScope, ADstStruct, LNewContext);
    LogEnd('inst [class: %s, dst: %s]', [ClassName, LNewContext.DstID.Name]);
  end;
end;

function TIDGenericInstantiation.SameType(AType: TIDGenericInstantiation): Boolean;
begin
  Result := (Original = AType.Original) and
            (Length(FGenericArguments) = Length(AType.FGenericArguments)); // todo: improve
end;

{ TIDParameter }

procedure TIDParameter.Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer; AAppendName: Boolean);
begin
  if VarConst in FFlags then
    ABuilder.Append('const ')
  else
  if VarInOut in FFlags then
    ABuilder.Append('var ')
  else
  if VarOut in FFlags then
    ABuilder.Append('out ');

  ABuilder.Append(DisplayName);

  if not (DataType is TIDUntypedRef) then
  begin
    ABuilder.Append(': ');
    TypeNameToString(DataType, ABuilder);

    if Assigned(DefaultValue) then
    begin
      ABuilder.Append(' = ');
      ABuilder.Append(DefaultValue.Text);
    end;
  end;
end;

{ TIDFloat }

procedure TIDFloat.Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer; AAppendName: Boolean);
begin
  ABuilder.Append(' ', ANestedLevel*2);
  ABuilder.Append('type ');
  ABuilder.Append(Name);
end;

{ TIDVariant }

procedure TIDVariant.Decl2Str(ABuilder: TStringBuilder; ANestedLevel: Integer; AAppendName: Boolean);
begin
  ABuilder.Append(' ', ANestedLevel*2);
  ABuilder.Append('type ');
  ABuilder.Append(Name);
end;

{ TGenericInstantiateContext }

function TGenericInstantiateContext.Args2Str: string;
begin
  Result := '';
  for var LArg in Args do
    Result := AddStringSegment(Result, LArg.DisplayName, ', ');
  Result := '<' + Result + '>';
end;

function TGenericInstantiateContext.GetArgByParam(AParam: TIDGenericParam): TIDType;
begin
  for var LIndex := 0 to Length(Params) - 1 do
    if Params[LIndex] = AParam then
      Exit(Args[LIndex]);

  AbortWorkInternal('Unknown generic param: %s', [AParam.Name]);
  Result := nil;
end;

{ TIDCallExpression }

function TIDCallExpression.GetProc: TIDProcedure;
begin
  Result := fDeclaration as TIDProcedure;
end;

function CallConventionToStr(AConvention: TCallConvention): string;
begin
  case AConvention of
    ConvRegister: Result := 'register';
    ConvStdCall: Result := 'stdcall';
    ConvCDecl: Result := 'cdecl';
    ConvFastCall: Result := 'fastcall';
    ConvSafeCall: Result := 'safecall';
  else
    Result := '';
  end;
end;

initialization

finalization

end.
