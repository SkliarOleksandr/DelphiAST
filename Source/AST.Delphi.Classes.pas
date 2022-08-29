unit AST.Delphi.Classes;

interface

{$I compilers.inc}

uses System.SysUtils, System.Classes, System.StrUtils, System.Math, System.Generics.Collections,
     AST.Delphi.DataTypes,
     AST.Lexer,
     System.Variants,
     AST.Delphi.Operators,
     AVL,
     AST.Parser.Utils,
     AST.Parser.Messages,
     AST.Parser.Options,
     AST.Pascal.Intf,
     AST.Classes,
     AST.Intf;
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
    PS_STATIC,
    PS_STDCALL,
    PS_FASTCALL,
    PS_CDECL,
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
  TIDConstant = class;
  TIDStructure = class;
  TIDClass = class;
  TIDClassOf = class;
  TIDInterface = class;
  TIDProperty = class;
  TIDOperator = class;
  TIDExpression = class;
  TIDNameSpace = class;
  TIDGenericType = class;
  TIDStringConstant = class;
  TIDProcType = class;
  TASTDelphiProc = class;
  TDlphHelper = class;

  TScope = class;
  TStructScope = class;
  TProcScope = class;

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

  TIDItemType = (
    itUnknown,         // неизвестный тип
    itVar,             // переменная
    itConst,           // константа
    itProcedure,       // процедуры/функция
    itSysOperator,     // system operator
    itMacroFunction,   // функция времени компиляции
    itProperty,        // свойство
    itAlias,           // алиас
    itType,            // тип
    itUnit,            // модуль
    itLabel            // label
  );

  TScopeType = (
    stLocal,
    stGlobal,
    stStruct,
    stWithScope
  );

  TIDDynArrayConstant = class;

  TVisibility = (vLocal, vPublic, vProtected, vStrictProtected, vPrivate, vStrictPrivate);

  TIDDeclarationClass = class of TIDDeclaration;

  {base declaration class}
  TIDDeclaration = class(TASTDeclaration)
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
    FImportLib: Integer;             // имя (индекс) билиотеки импорта
    FImportName: Integer;            // имя (индекс) функции импорта
    fDepricatedExpression: TIDExpression;
    function GetDataTypeID: TDataTypeID; inline;
    function GetUnitID: Integer;
    function GetIsAnonymous: Boolean; inline;
    function GetPackage: IASTPascalProject;
    procedure SetIndex(const Value: Integer);
  protected
    function GetOriginalDecl: TIDDeclaration; virtual;
    function GetIndex: Integer; virtual;
    function GetCValue: TIDConstant; virtual;
    procedure SetCValue(const Value: TIDConstant); virtual;
    procedure SetDataType(const Value: TIDType); virtual;
  public
    constructor CreateAsAnonymous(Scope: TScope); virtual;
    constructor CreateAsSystem(Scope: TScope; const Name: string); overload; virtual;
    constructor Create(Scope: TScope; const Identifier: TIdentifier); overload; virtual;
    destructor Destroy; override;
    ////////////////////////////////////////////////////////////////////////////
    property ItemType: TIDItemType read FItemType write FItemType;
    property DisplayName: string read GetDisplayName;
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
    property SpaceIndex: Integer read GetIndex;
    procedure IncRefCount(RCPath: UInt32); virtual; // добавляет зависимость
    procedure DecRefCount(RCPath: UInt32); virtual; // удаляет зависимость
    {процедура делает DecReadCount для зависимостей}
    procedure RemoveILReferences(var RCPathCount: UInt32); virtual;
    property Package: IASTPascalProject read GetPackage;
    property CValue: TIDConstant read GetCValue write SetCValue;
    property DepricatedExpression: TIDExpression read fDepricatedExpression write fDepricatedExpression;
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

  TIDTypeClass = class of TIDType;

  TIDTypeList = array of TIDType;

  TGenericInstance = record
    Args: TIDExpressions;
    Instance: TIDDeclaration;
  end;

  TGenericInstanceList = array of TGenericInstance;

  PGenericDescriptor = ^TGenericDescriptor;
  TGenericDescriptor = record
  private
    FScope: TScope;
    FSearchName: string;
    FGenericInstances: TGenericInstanceList;
    FGenericParams: TIDTypeList;
    FIntfSRCPosition: TParserPosition;
    FImplSRCPosition: TParserPosition;
  public
    property Scope: TScope read FScope;
    property SearchName: string read FSearchName write FSearchName;
    property GenericInstances: TGenericInstanceList read FGenericInstances;
    property GenericParams: TIDTypeList read FGenericParams write FGenericParams;
    property IntfSRCPosition: TParserPosition read FIntfSRCPosition write FImplSRCPosition;
    property ImplSRCPosition: TParserPosition read FImplSRCPosition write FImplSRCPosition;
    procedure AddGenericInstance(Decl: TIDDeclaration; const Args: TIDExpressions);
    class function Create(Scope: TScope): PGenericDescriptor; static;
    function FindType(const Name: string): TIDGenericType; inline;
  end;

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
  end;

  {base type class}
  TIDType = class(TIDDeclaration)
  type
    TUnarOperators = array [opAssignment..opNot] of TIDDeclaration;
    TBinarOperators = array [opIn..High(TOperatorID)] of TIDPairList;
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
    fImplicitsIDTo: TIDPairList;    // self -> dest
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
    fGenericDescriptor: PGenericDescriptor; // содерижт всю информацию по generic-типу/процедуре
    fGenericNextOverload: TIDType;
    /////////////////////////////////////////////////////////
    fPacked: Boolean;
    fNeedForward: Boolean;
    fForwardID: TIdentifier;
    fInitProc: TIDProcedure;
    fCopyProc: TIDProcedure;
    fFinalProc: TIDProcedure;
    fHelper: TDlphHelper;
    function GetOperators(const Op: TOperatorID): TIDPairList;
    function GetOperatorsFor(const Op: TOperatorID): TIDPairList;
    function GetIsReferenced: Boolean; inline;
    function GetIsInteger: Boolean; inline;
    function GetIsGeneric: Boolean; inline;
  protected
    function GetDataSize: Integer; virtual;
    function GetOrdinal: Boolean; virtual;
    function GetDisplayName: string; override;
    function GetIsManaged: Boolean; virtual;
    function GetActualDataType: TIDType; virtual;
    function GetParent: TIDType;
    procedure SetGenericDescriptor(const Value: PGenericDescriptor); virtual;
  public
    constructor CreateAsAnonymous(Scope: TScope); override;
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    constructor Create(Scope: TScope; const ID: TIdentifier); override;
    destructor Destroy; override;
    ////////////////////////////////////////////////////////////////////////////
    function GetDefaultReference(Scope: TScope): TIDType;
    property DefaultReference: TIDType read fDefaultReference write fDefaultReference;
    property WeakRefType: TIDWeekRef read FWeakType write FWeakType;
    property DataTypeID: TDataTypeID read FDataTypeID write FDataTypeID;
    property Elementary: Boolean read FElementary write FElementary;
    property BinarOperators[const Op: TOperatorID]: TIDPairList read GetOperators;
    property BinarOperatorsFor[const Op: TOperatorID]: TIDPairList read GetOperatorsFor;
    property IsOrdinal: Boolean read GetOrdinal;
    property IsInteger: Boolean read GetIsInteger;
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
    function BinarOperator(Op: TOperatorID; Right: TIDType): TIDType; inline;
    function BinarOperatorFor(const Op: TOperatorID; const Left: TIDType): TIDType;
    property ActualDataType: TIDType read GetActualDataType;
    property TypeKind: TTypeKind read FTypeKind write FTypeKind;
    property DataSize: Integer read GetDataSize;
    property Managed: Boolean read GetIsManaged;
    property IsPooled: Boolean read FIsPooled write FIsPooled;
    property NeedForward: Boolean read FNeedForward write FNeedForward;
    property ForwardID: TIdentifier read FForwardID write FForwardID;
    property Parent: TIDType read GetParent;

    {переопределенным оператором может быть как функция так и тип, для простейших операций}
    procedure OverloadImplicitTo(const Destination: TIDDeclaration); overload;
    procedure OverloadImplicitTo(const DestinationID: TDataTypeID; const IntOp: TIDOperator); overload;
    procedure OverloadImplicitTo(const Destination, Proc: TIDDeclaration); overload;
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

    procedure AddBinarySysOperator(Op: TOperatorID; Decl: TIDOperator);

    procedure CreateStandardOperators; virtual;

    property GenericDescriptor: PGenericDescriptor read FGenericDescriptor write SetGenericDescriptor;
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
  end;

  TIDTypesArray = array of TIDType;

  {special system internal type to describe several possible types in one}
  TIDSysVariativeType = class(TIDType)
  private
    FTypes: TIDTypesArray;
  protected
    function GetDisplayName: string; override;
  public
    constructor CreateAsSystem(Scope: TScope; const Types: TIDTypesArray); reintroduce;
    procedure CreateStandardOperators; override;
    property Types: TIDTypesArray read FTypes;
  end;

  {special generic type}
  TIDGenericType = class(TIDType)
  private
    fDefaultValue: TIDExpression;
  public
    constructor Create(Scope: TScope; const ID: TIdentifier); override;
    property DefaultValue: TIDExpression read FDefaultValue write FDefaultValue;
  end;

  {alias type}
  TIDAliasType = class(TIDType)
  private
    fOriginalType: TIDType; // оригинальный тип (не алиас)
    fLinkedType: TIDType;   // тип на который ссылается алиас
  protected
    function GetActualDataType: TIDType; override;
    function GetOrdinal: Boolean; override;
    function GetIndex: Integer; override;
    function GetDisplayName: string; override;
  public
    constructor CreateAlias(Scope: TScope; const ID: TIdentifier; OriginalType: TIDType);
    constructor CreateAliasAsSystem(Scope: TScope; const ID: string; SrcType: TIDType);
    procedure IncRefCount(RCPath: UInt32); override;
    procedure DecRefCount(RCPath: UInt32); override;
    property LinkedType: TIDType read FLinkedType;
    property Original: TIDType read FOriginalType;
  end;

  {base referenced type class}
  TIDRefType = class(TIDType)
  private
    fReferenceType: TIDType;
  protected
    function GetDisplayName: string; override;
    procedure IncRefCount(RCPath: UInt32); override;
    procedure DecRefCount(RCPath: UInt32); override;
  public
    constructor Create(Scope: TScope; const ID: TIdentifier); override;
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    constructor CreateAsAnonymous(Scope: TScope; ReferenceType: TIDType); virtual;
    procedure CreateStandardOperators; override;
    property ReferenceType: TIDType read fReferenceType write FReferenceType;
  end;

  {pointer type}
  TIDPointer = class(TIDRefType)
  private
    FForwardDeclaration: Boolean;
  public
    constructor Create(Scope: TScope; const ID: TIdentifier); override;
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    constructor CreateAsAnonymous(Scope: TScope; ReferenceType: TIDType); override;
    procedure CreateStandardOperators; override;
    property ForwardDeclaration: Boolean read FForwardDeclaration write FForwardDeclaration;
  end;

  {untyped referenced type}
  TIDUntypedRef = class(TIDRefType)
  public
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
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
  protected
    function GetOrdinal: Boolean; override;
  public
    property LowBound: Int64 read FLBound write FLBound;
    property HighBound: Int64 read FHBound write FHBound;
    // показывает знаковый ли диаппазон (Int64) или беззнаковый (UInt64)
    property SignedBound: Boolean read FSignedBound write FSignedBound;
    property ElementsCount: UInt64 read GetElementsCount;
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
  end;

  TStructFlags = set of (StructCompleted);

  {base structure type}
  TIDStructure = class(TIDType)
  private
    fAncestor: TIDStructure;
    fStaticMembers: TStructScope;   // class (static) members
    fMembers: TStructScope;         // instance members
    fOperators: TStructScope;       // operators members
    fVarSpace: TVarSpace;
    fProcSpace: TProcSpace;
    fStaticVarSpace: TVarSpace;
    fStaticProcSpace: TProcSpace;
    fOperatorsSpace: TProcSpace;
    fStrucFlags: TStructFlags;
    fDefaultProperty: TIDProperty;
    fClassOfType: TIDClassOf;
    fClassConstructor: TIDProcedure;
    FClassDestructor: TIDProcedure;
    function GetHasInitFiels: Boolean;
    procedure SetAncestor(const Value: TIDStructure);
    function GetProcSpace: PProcSpace; inline;
    function GetVarSpace: PVarSpace; inline;
    function GetClassOfType: TIDClassOf;
  protected
    function GetDataSize: Integer; override;
    function GetDisplayName: string; override;
    function GetIsManaged: Boolean; override;
    function GetExtraFlags: Byte; virtual;
    procedure SetGenericDescriptor(const Value: PGenericDescriptor); override;
    procedure DoCreateStructure;
  public
    constructor Create(Scope: TScope; const Name: TIdentifier); override;
    constructor CreateAsAnonymous(Scope: TScope); override;
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    ////////////////////////////////////////////////////////////////////////////
    property Members: TStructScope read FMembers;
    property StaticMembers: TStructScope read FStaticMembers;
    property Operators: TStructScope read fOperators;
    property HasInitFiels: Boolean read GetHasInitFiels;
    property StructFlags: TStructFlags read FStrucFlags write FStrucFlags;
    property Ancestor: TIDStructure read FAncestor write SetAncestor;
    property Methods: PProcSpace read GetProcSpace;
    property Fields: PVarSpace read GetVarSpace;
    property MethodCount: Integer read FProcSpace.FCount;
    property FieldsCount: Integer read FVarSpace.FCount;
    property DefaultProperty: TIDProperty read FDefaultProperty write FDefaultProperty;
    property ClassOfType: TIDClassOf read GetClassOfType;
    property FirstField: TIDVariable read FVarSpace.FFirst;
    function IsInheritsForm(Ancestor: TIDStructure): Boolean;
    function GetLastVirtualIndex: Integer;
    function FindVirtualProc(Proc: TIDProcedure): TIDProcedure;
    function FindVirtualProcInAncestor(Proc: TIDProcedure): TIDProcedure;
    function AddField(const Name: string; DataType: TIDType): TIDField;
    function FindField(const Name: string): TIDField;
    function FindMethod(const Name: string): TIDProcedure;
    procedure IncRefCount(RCPath: UInt32); override;
    procedure DecRefCount(RCPath: UInt32); override;

    property ClassConstructor: TIDProcedure read fClassConstructor write fClassConstructor;
    property ClassDestructor: TIDProcedure read FClassDestructor write FClassDestructor;
  end;

  {record type}
  TIDRecord = class(TIDStructure)
  private
    fStaticConstructor: TIDProcedure;
    fStaticDestructor: TIDProcedure;
    fCases: array of TVarSpace;
  protected
    function GetDataSize: Integer; override;
  public
    constructor Create(Scope: TScope; const Name: TIdentifier); override;
    procedure CreateStandardOperators; override;
    property StaticConstructor: TIDProcedure read FStaticConstructor write FStaticConstructor;
    property StaticDestructor: TIDProcedure read FStaticDestructor write FStaticDestructor;
    function AddCase: PVarSpace;
  end;

  TIDMethods = array of TIDProcedure;

  {class}
  TIDClass = class(TIDStructure)
  private
    FInterfaces: TList<TIDInterface>;
    FInterfacesMethods: TList<TIDMethods>;
    function GetInterfacesCount: Integer;
    function GetInterface(Index: Integer): TIDInterface;
  protected
    function GetDataSize: Integer; override;
    function GetIsManaged: Boolean; override;
    function GetExtraFlags: Byte; override;
  public
    constructor Create(Scope: TScope; const Name: TIdentifier); override;
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    destructor Destroy; override;
    //========================================================
    function FindInterface(const Intf: TIDInterface): Boolean;
    procedure AddInterface(const Intf: TIDInterface);
    procedure MapInterfaceMethod(const Intf: TIDInterface; IntfMethod, ImplMethod: TIDProcedure);
    property InterfacesCount: Integer read GetInterfacesCount;
    property Interfaces[Index: Integer]: TIDInterface read GetInterface;
    procedure IncRefCount(RCPath: UInt32); override;
    procedure DecRefCount(RCPath: UInt32); override;
    procedure CreateStandardOperators; override;
  end;

  {helper type}
  TDlphHelper = class(TIDStructure)
  private
    fTarget: TIDType;
    procedure SetTarget(const Value: TIDType);
  public
    property Target: TIDType read fTarget write SetTarget;
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
    FGUID: TGUID;
  protected
    function GetIsManaged: Boolean; override;
  public
    constructor Create(Scope: TScope; const Name: TIdentifier); override;
    property GUID: TGUID read FGUID write FGUID;
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
  protected
    function GetDisplayName: string; override;
    function GetDataSize: Integer; override;
  public
    constructor Create(Scope: TScope; const Name: TIdentifier); override;
    constructor CreateAsAnonymous(Scope: TScope); overload; override;
    constructor CreateAnonymousStatic1Dim(Scope: TScope; ElementDataType: TIDType; Length: Integer; out BoundType: TIDOrdinal); overload;
    procedure CreateStandardOperators; override;
    ////////////////////////////////////////////////////////////////////////////
    property ElementDataType: TIDType read FElementDataType write FElementDataType;
    property DimensionsCount: Integer read FDimensionsCount;
    property Dimensions[Index: Integer]: TIDOrdinal read GetDimension;
    procedure AddBound(Bound: TIDOrdinal);
    procedure IncRefCount(RCPath: UInt32); override;
    procedure DecRefCount(RCPath: UInt32); override;
    function IsOpenArrayOfConst: Boolean;
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
    constructor Create(Scope: TScope; const ID: TIdentifier); override;
    procedure CreateStandardOperators; override;
    ////////////////////////////////////////////////////////////////////////////
    property BitsCount: UInt32 read GetBitsCount;
    property BaseType: TIDOrdinal read fBaseType write FBaseType;
    procedure IncRefCount(RCPath: UInt32); override;
  end;

  {static array}
  TIDStaticArray = class(TIDArray)
  public
    constructor CreateAnonymous(Scope: TScope; BaseType: TIDType); reintroduce;
    constructor Create(Scope: TScope; const ID: TIdentifier); override;
    procedure CreateStandardOperators; override;
  end;


  {dynamic array}
  TIDDynArray = class(TIDArray)
  private
    function GetDimension(Index: Integer): TIDOrdinal; override;
  protected
  public
    constructor Create(Scope: TScope; const Name: TIdentifier); override;
    constructor CreateAsAnonymous(Scope: TScope); override;
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    procedure CreateStandardOperators; override;
    ////////////////////////////////////////////////////////////////////////////
  end;

  {string}
  TIDString = class(TIDDynArray)
  public
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
  end;

  {variant}
  TIDVariant = class(TIDType)
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

  TVariableList = array of TIDVariable;


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


  TCallConvention = (ConvNative, ConvRegister, ConvStdCall, ConvCDecl, ConvFastCall);

  {procedural type}
  TIDProcType = class(TIDType)
  private
    fParams: TVariableList;
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
    property Params: TVariableList read fParams write fParams;
    property ResultType: TIDType read fResultType write fResultType;
    property IsStatic: Boolean read GetIsStatic;
    property CallConv: TCallConvention read FCallConv write FCallConv;
    property ProcClass: TProcTypeClass read fProcClass write fProcClass;
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
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    constructor CreateAsAnonymous(Scope: TScope; DataType: TIDType; Value: T);
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
    FInstance: TIDExpression;
    FGenericArgs: TIDExpressions;
  public
    property ArgumentsCount: Integer read FArgumentsCount write FArgumentsCount;
    property Instance: TIDExpression read FInstance write FInstance;
    property GenericArgs: TIDExpressions read FGenericArgs write FGenericArgs;
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

  {переменная}
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
  end;

  {поле структуры}
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
  end;

  {свойство структуры}
  TIDProperty = class(TIDDeclaration)
  private
    FGetter: TIDDeclaration;
    FSetter: TIDDeclaration;
    FParams: TScope;
    function GetParamsCount: Integer;
  public
    constructor Create(Scope: TScope; const Identifier: TIdentifier); override;
    property Getter: TIDDeclaration read FGetter write FGetter;
    property Setter: TIDDeclaration read FSetter write FSetter;
    property Params: TScope read FParams write FParams;
    property ParamsCount: Integer read GetParamsCount;
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
    pfClass,
    pfStatic,
    pfForward
  );

  TProcFlags = set of TProcFlag;


  TIDParamList = TList<TIDVariable>;

  {процедура/функция}
  TIDProcedure = class(TIDDeclaration)
  strict private
    FExplicitParams: TVariableList; // actual arguments (excluding 'self', 'Result', etc...);
    FCount: Integer;                // count of parameters
    FParamsScope: TProcScope;       // only parameters scope
    FEntryScope: TScope;            // proc body scope (params, local vars, types, procs etc...)
    FStruct: TIDStructure;          // self parameter
    FProcFlags: TProcFlags;         // флаги inline/pure
    FTempVars: TItemsStack;
    FNextOverload: TIDProcedure;
    FIL: TObject;
    FVarSpace: TVarSpace;
    FProcSpace: TProcSpace;
    FCallConv: TCallConvention;
    FResultType: TIDType;
    FVirtualIndex: Integer;
    FGenericDescriptor: PGenericDescriptor; // содерижт всю информацию по generic-типу/процедуре
    FGenericPrototype: TIDProcedure;        // исходная generic-процедруа (если дання является специализированным инстансом)
    FFirstBodyLine: Integer;                // позиция в исходном коде начала тела процедуры
    FLastBodyLine: Integer;                 // позиция в исходном коде конца тела процедуры
    FFinalSection: TObject;                 // блок финализации процедуры (TIL)
  private
    procedure SetParameters(const Value: TVariableList); inline;
    procedure SetEntryScope(const Value: TScope);
    function GetIsCompleted: Boolean; inline;
    function GetSelfDecl: TIDVariable;
    function GetProcTypeName: string;
    function GetIsStatic: Boolean; inline;
    function GetDefaultParamsCount: Integer;
    function GetMethodIndex: Integer;
    function GetSelfParam: TIDVariable;
    function GetSelfParamExpression: TIDExpression;
    function GetResultParamExpression: TIDExpression;
    function GetProcSpace: PProcSpace; inline;
    function GetIsGeneric: Boolean; inline;
  protected
    function GetDisplayName: string; override;
    function GetIndex: Integer; override;
  public
    constructor Create(Scope: TScope; const ID: TIdentifier); override;
    constructor CreateAsAnonymous(Scope: TScope); override;
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    constructor CreateAsSystemMethod(Struct: TIDStructure; const Name: string);
    destructor Destroy; override;
    //////////////////////////////////////////////////////////////////////////////////
    property ExplicitParams: TVariableList read FExplicitParams write SetParameters;
    function SameDeclaration(const ParamsScope: TScope): Boolean; overload;
    function SameDeclaration(const Params: TVariableList): Boolean; overload;

    // Temporary variable alloc
    function GetTMPVar(DataType: TIDType; Reference: Boolean = False): TIDVariable; overload;
    function GetTMPVar(DataType: TIDType; VarFlags: TVariableFlags): TIDVariable; overload;
    function GetTMPRef(DataType: TIDType): TIDVariable;
    property TempVars: TItemsStack read FTempVars;

    property PrevOverload: TIDProcedure read FNextOverload write FNextOverload;
    property EntryScope: TScope read FEntryScope write SetEntryScope;
    property ParamsScope: TProcScope read FParamsScope write FParamsScope;
    property ParamsCount: Integer read FCount;
    {$warnings off}
    property ResultType: TIDType read FResultType write FResultType;
    {$warnings on}
    property Flags: TProcFlags read FProcFlags write FProcFlags;
    property IL: TObject read FIL write FIL;
    property VarSpace: TVarSpace read FVarSpace write FVarSpace;
    property ProcSpace: PProcSpace read GetProcSpace;
    property Struct: TIDStructure read FStruct write FStruct;
    property SelfParam: TIDVariable read GetSelfParam;
    property SelfParamExpression: TIDExpression read GetSelfParamExpression;

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
    procedure CreateGenericDescriptor(const GenericParams: TIDTypeList; const SRCPosition: TParserPosition); inline;

    procedure Warning(const Message: string; const Params: array of const; const TextPosition: TTextPosition);
    procedure Hint(const Message: string; const Params: array of const; const TextPosition: TTextPosition);

    property IsCompleted: Boolean read GetIsCompleted;
    property IsStatic: Boolean read GetIsStatic;
    property SelfParameter: TIDVariable read GetSelfDecl;
    property ProcTypeName: string read GetProcTypeName;
    property DefaultParamsCount: Integer read GetDefaultParamsCount;
    property VirtualIndex: Integer read FVirtualIndex write FVirtualIndex;
    property MethodIndex: Integer read GetMethodIndex;
    property GenericDescriptor: PGenericDescriptor read FGenericDescriptor;
    property GenericPrototype: TIDProcedure read FGenericPrototype write FGenericPrototype;
    property CallConvention: TCallConvention read FCallConv write FCallConv;
    property IsGeneric: Boolean read GetIsGeneric;

    property FirstBodyLine: Integer read FFirstBodyLine write FFirstBodyLine;
    property LastBodyLine: Integer read FLastBodyLine write FLastBodyLine;
    property FinalSection: TObject read FFinalSection write FFinalSection;
    function GetDebugVariables: string;
    function GetDebugIL: string;
    {функция выполняет IL код в compile-time и возвращает константное выражение}
    function CECalc(const Args: TIDExpressions): TIDConstant;
    function GetAllOverloadSignatures(const LineSeparator: string = #13#10): string;
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
  TIDList = class(TCaseInsensitiveObjectList)
  public
    function InsertID(Item: TIDDeclaration): Boolean; inline; // true - ok, false - already exist
    function InsertIDAndReturnIfExist(Item: TIDDeclaration): TIDDeclaration; inline;
    function FindID(const Identifier: string): TIDDeclaration; virtual; //inline;
  end;

  TScopeClass = (scInterface, scImplementation, scProc);

  TScopes = array of TScope;

  TScope = class(TIDList)
  private
    fUnit: TASTModule;          // модуль (индекс модуля в пакете)
    fParent: TScope;            // Parent scope
    fScopeType: TScopeType;     // Тип scope
    fVarSpace: PVarSpace;       // ссылка на список переменных
    fProcSpace: PProcSpace;     // ссылка на список процедур
    fAdditionalScopes: TScopes; // список дополнительных (присоедененных) областей
    fChilds: TList<TScope>;     // вложенные области (необходимо для корректного удаления)
    {$IFDEF DEBUG}
    fName: string;
   {$ENDIF}
    procedure SetParent(const Value: TScope);
  protected
    function GetName: string; virtual;
    function GetScopeClass: TScopeClass; virtual;
    procedure AddChild(Scope: TScope);
    procedure RemoveChild(Scope: TScope);
  public
    constructor Create(ScopeType: TScopeType; DeclUnit: TASTModule); overload;
    constructor Create(ScopeType: TScopeType; Parent: TScope); overload;
    constructor Create(ScopeType: TScopeType; VarSpace: PVarSpace; ProcSpace: PProcSpace; Parent: TScope; DeclUnit: TASTModule); overload;
    destructor Destroy; override;
    //////////////////////////////////////////////////////////////////////
    procedure AddVariable(Declaration: TIDVariable);
    procedure AddProcedure(Declaration: TIDProcedure);
    procedure AddAnonymousProcedure(Declaration: TIDProcedure);
    procedure AddProperty(Declaration: TIDProperty);
    procedure AddScope(Scope: TScope);
    procedure RemoveVariable(Declaration: TIDVariable);
    function FindInAdditionalScopes(const ID: string): TIDDeclaration;
    function FindIDRecurcive(const ID: string): TIDDeclaration; virtual;
    function FindMembers(const ID: string): TIDDeclaration; virtual;
    function GetDeclArray(Recursively: Boolean = False): TIDDeclArray;
    function GetDeclNamesArray(Recursively: Boolean = False): TStrArray;
    property Parent: TScope read FParent write SetParent;
    property ScopeType: TScopeType read FScopeType;
    property VarSpace: PVarSpace read FVarSpace write FVarSpace;
    property ProcSpace: PProcSpace read FProcSpace write FProcSpace;
    property ScopeClass: TScopeClass read GetScopeClass;
    property DeclUnit: TASTModule read FUnit;
    property AdditionalScopes: TScopes read FAdditionalScopes;
    {$IFDEF DEBUG}property Name: string read GetName write FName;{$ENDIF}
  end;

  TProcScope = class(TScope)
  private
    fOuterScope: TScope; // внешняя по отношению к методу, область видимости (секция implementation реализации метода)
  protected
    function GetScopeClass: TScopeClass; override;
  public
    constructor CreateInDecl(Parent: TScope; VarSpace: PVarSpace; ProcSpace: PProcSpace); reintroduce;
    constructor CreateInBody(Parent: TScope); reintroduce;
    property OuterScope: TScope read FOuterScope write FOuterScope;
    function FindIDRecurcive(const ID: string): TIDDeclaration; override;
  end;

  TStructScope = class(TScope)
    fAncestorScope: TScope;
    fStruct: TIDStructure;
  protected
    function GetName: string; override;
  public
    constructor CreateAsStruct(Parent: TScope; Struct: TIDStructure; VarSpace: PVarSpace; ProcSpace: PProcSpace; DeclUnit: TASTModule); reintroduce;
    function FindIDRecurcive(const ID: string): TIDDeclaration; override;
    function FindMembers(const ID: string): TIDDeclaration; override;
    property Struct: TIDStructure read FStruct;
  end;

  TRecordInitScope = class(TScope)
  private
    fStructType: TIDStructure;
  public
    property Struct: TIDStructure read fStructType write fStructType;
  end;

  TWithScope = class(TProcScope)
  private
    fInnerScope: TScope;
    fExpression: TIDExpression;       // выражение, которое породило данный Scope (то что написано в WITH ... DO)
  public
    constructor Create(Parent: TScope; Expression: TIDExpression); reintroduce;
    ///////////////////////////////////////
    function FindIDRecurcive(const ID: string): TIDDeclaration; override;
    property InnerScope: TScope read FInnerScope write FInnerScope;
    property Expression: TIDExpression read FExpression;
  end;

  TMethodScope = class(TProcScope)
  private
    // FExpression: TIDExpression;
  public
    constructor CreateInDecl(OuterScope, Parent: TScope; VarSpace: PVarSpace; ProcSpace: PProcSpace); reintroduce; overload;
    constructor CreateInDecl(OuterScope, Parent: TScope); overload;
    function FindIDRecurcive(const ID: string): TIDDeclaration; override;
  end;

  TInterfaceScope = class(TScope)
  protected
    function GetName: string; override;
  public
    constructor Create(AUnit: TASTModule;
                       VarSpace: PVarSpace;
                       ProcSpace: PProcSpace); reintroduce;
    function FindIDRecurcive(const ID: string): TIDDeclaration; override;
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
    function FindIDRecurcive(const ID: string): TIDDeclaration; override;
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
  function GetVarDebugString(const VarSpace: TVarSpace): string;
  procedure WriteDataTypeIndex(Stream: TStream; DataType: TIDType);

  procedure IncReadCount(const Expression: TIDExpression; const Instruction: TObject; RCPath: UInt32); inline;
  procedure DecReadCount(const Expression: TIDExpression; const Instruction: TObject; RCPath: UInt32); inline;

  procedure WriteConstToStream(Stream: TStream; Decl: TIDConstant; DataType: TIDType);

  function GetBoolResultExpr(ExistExpr: TIDExpression): TIDBoolResultExpression;
  function GetItemTypeName(ItemType: TIDItemType): string;

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

  function GetConstantClassByDataType(DataTypeID: TDataTypeID): TIDConstantClass;

implementation

uses AST.Delphi.System,
     AST.Pascal.Parser,
     AST.Parser.Errors,
     AST.Delphi.Errors,
     AST.Delphi.Parser,
     AST.Delphi.SysOperators;

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

function GetVarDebugString(const VarSpace: TVarSpace): string;
var
  Item: TIDVariable;
  TypeName, VarDesc: string;
begin
  Result := '';
  Item := VarSpace.First;
  while Assigned(Item) do begin
    if Assigned(Item.DataType) then
      TypeName := Item.DataType.DisplayName
    else
      TypeName := '<untyped>';
    if Item.Reference then
      TypeName := '^' + TypeName;
    if Assigned(Item.Absolute) then
      TypeName := TypeName + ' absolute ' + Item.Absolute.DisplayName;
    if VarTmpResOwner in Item.Flags then
      TypeName := TypeName + '[tmpresown]';
    //VarDesc := '  ' + format('%s: %s; // RC=%d WC=%d', [DeclarationName(Item), TypeName, Item.ReadCount, Item.WriteCount]);
    VarDesc := '  ' + format('%s: %s;', [DeclarationName(Item), TypeName]);
    Result := AddStringSegment(Result, VarDesc, #10);


    Item := TIDVariable(Item.NextItem);
  end;
end;

procedure WriteConstToStream(Stream: TStream; Decl: TIDConstant; DataType: TIDType);
var
  i, ec: Integer;
  Field: TIDVariable;
  ArrayDT: TIDArray;
begin
  case DataType.DataTypeID of
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
  end;
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
  inherited Create(StrCICompare);
  FScopeType := ScopeType;
  FParent := Parent;
  FUnit := Parent.DeclUnit;
  FVarSpace := Parent.VarSpace;
  FProcSpace := Parent.ProcSpace;
  Parent.AddChild(Self);
end;

constructor TScope.Create(ScopeType: TScopeType; VarSpace: PVarSpace; ProcSpace: PProcSpace; Parent: TScope; DeclUnit: TASTModule);
begin
  inherited Create(StrCICompare);
  FScopeType := ScopeType;
  FVarSpace := VarSpace;
  FProcSpace := ProcSpace;
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

procedure TScope.AddAnonymousProcedure(Declaration: TIDProcedure);
begin
  FProcSpace.Add(Declaration);
end;

procedure TScope.AddChild(Scope: TScope);
begin
  if not Assigned(FChilds) then
    FChilds := TList<TScope>.Create;
  FChilds.Add(Scope);
end;

procedure TScope.RemoveChild(Scope: TScope);
begin
  if Assigned(FChilds) then
    FChilds.Remove(Scope);
end;

procedure TScope.RemoveVariable(Declaration: TIDVariable);
var
  Node: PAVLNode;
begin
  Node := Find(Declaration.Name);
  if Assigned(Node) and (Node.Data = Declaration) then
  begin
    Delete(Declaration.Name);
    FVarSpace.Delete(Declaration);
  end;
end;

procedure TScope.AddProcedure(Declaration: TIDProcedure);
begin
  if not InsertID(Declaration) then
    AbortWork(sIdentifierRedeclaredFmt, [Declaration.DisplayName], Declaration.SourcePosition);
  FProcSpace.Add(Declaration);
end;

procedure TScope.AddProperty(Declaration: TIDProperty);
begin
  if not InsertID(Declaration) then
    AbortWork(sIdentifierRedeclaredFmt, [Declaration.Name], Declaration.SourcePosition);
end;

procedure TScope.AddVariable(Declaration: TIDVariable);
begin
  if not InsertID(Declaration) then
    AbortWork(sIdentifierRedeclaredFmt, [Declaration.Name], Declaration.SourcePosition);
  FVarSpace.Add(Declaration);
end;

function TScope.FindIDRecurcive(const ID: string): TIDDeclaration;
begin
  // ищим только в себе
  Result := FindID(ID);
  if Assigned(Result) then
    Exit;

  // search in additional scopes
  Result := FindInAdditionalScopes(ID);
  if Assigned(Result) then
    Exit;

  // если есть родитель - ищем в нем
  if Assigned(FParent) then
    Result := FParent.FindIDRecurcive(ID);
end;

function TScope.FindInAdditionalScopes(const ID: string): TIDDeclaration;
var
  i: Integer;
begin
  for i := 0 to Length(FAdditionalScopes) - 1 do
  begin
    Result := FAdditionalScopes[i].FindMembers(ID);
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

function TScope.GetScopeClass: TScopeClass;
begin
  Result := scInterface;
end;

procedure TScope.SetParent(const Value: TScope);
begin
  if Value = Self then
    Assert(False);

  if Assigned(FParent) then
    FParent.RemoveChild(Self);

  FParent := Value;

  Value.AddChild(Self);
end;

function TScope.GetDeclArray(Recursively: Boolean): TIDDeclArray;
var
  i: Integer;
  Node: PAVLNode;
begin
  SetLength(Result, Count);
  i := 0;
  Node := First;
  while Assigned(Node) do
  begin
    Result[i] := TIDDeclaration(Node.Data);
    Inc(i);
    Node := Next(Node);
  end;
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

function TScope.GetName: string;
begin
  Result := FName;
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
  CreateFromPool;
  FScope := Scope;
  FID := Identifier;
  fModule := Scope.DeclUnit;
end;

constructor TIDDeclaration.CreateAsAnonymous(Scope: TScope);
begin
  CreateFromPool;
  FScope := Scope;
  fModule := Scope.DeclUnit;
end;

constructor TIDDeclaration.CreateAsSystem(Scope: TScope; const Name: string);
begin
  CreateFromPool;
  FScope := Scope;
  FID.Name := Name;
  fModule := Scope.DeclUnit;
end;

destructor TIDDeclaration.Destroy;
begin

  inherited;
end;

function TIDDeclaration.GetCValue: TIDConstant;
begin
  Result := nil;
end;

function TIDDeclaration.GetDataTypeID: TDataTypeID;
begin
  Result := FDataType.DataTypeID;
end;

function TIDDeclaration.GetIndex: Integer;
begin
  Result := FIndex;
end;

function TIDDeclaration.GetIsAnonymous: Boolean;
begin
  Result := (FID.Name = '');
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

procedure TIDDeclaration.IncRefCount(RCPath: UInt32);
begin
  if FRCPath = RCPath then
    Exit;
  FRCPath := RCPath;
  Inc(FRefCount);
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

{ TIDProcDeclaration }

procedure TIDProcedure.AddParam(const Param: TIDVariable);
begin
  SetLength(FExplicitParams, FCount + 1);
  FExplicitParams[FCount] := Param;
  FVarSpace.Add(Param);
  Inc(FCount);
end;

function TIDProcedure.AddParam(const Name: string; DataType: TIDType): TIDParam;
begin
  Result := TIDVariable.Create(ParamsScope, Identifier(Name));
  Result.DataType := DataType;
  AddParam(Result);
end;

function TIDProcedure.AddParam(const Name: string; DataType: TIDType; Flags: TVariableFlags; DefaultValue: TIDExpression = nil): TIDParam;
begin
  Result := TIDVariable.Create(ParamsScope, Identifier(Name));
  Result.DataType := DataType;
  Result.Flags := Flags + [varParameter];
  Result.DefaultValue := DefaultValue;
  AddParam(Result);
end;

function TIDProcedure.CECalc(const Args: TIDExpressions): TIDConstant;
//var
//  i: Integer;
//  Param: TIDVariable;
begin
//  // загрузка аргументов в параметры
//  Param := FVarSpace.First;
//  if Assigned(FResultType) then
//    Param := TIDVariable(Param.NextItem);
//  for i := 0 to Length(Args) - 1 do
//  begin
//    Param.CValue := Args[i].CValue;
//    Param := TIDVariable(Param.NextItem);
//  end;
//  // запуск выполнения IL кода
//  if IsCompleted then
//    TIL(FIL).CECalc;
//  // получение результата
//  if Assigned(FResultType) then
//    Result := FVarSpace.First.CValue
//  else
    Result := nil;
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
  FParamsScope := TMethodScope.CreateInDecl(Scope, Scope, addr(FVarSpace), addr(FProcSpace));
  FEntryScope := FParamsScope;
end;

constructor TIDProcedure.CreateAsSystemMethod(Struct: TIDStructure; const Name: string);
var
  SelfParam: TIDVariable;
begin
  inherited CreateAsSystem(Scope, Name);
  ItemType := itProcedure;
  FParamsScope := TMethodScope.CreateInDecl(Scope, Struct.Members, addr(FVarSpace), addr(FProcSpace));
  if Struct.DataTypeID = dtRecord then
    SelfParam := TIDVariable.Create(FParamsScope, Identifier('Self'), Struct, [VarParameter, VarSelf, VarConst, VarInOut, VarHiddenParam])
  else
    SelfParam := TIDVariable.Create(FParamsScope, Identifier('Self'), Struct, [VarParameter, VarSelf, VarConst, VarHiddenParam]);

  FParamsScope.AddVariable(SelfParam);
  FEntryScope := FParamsScope;
end;

procedure TIDProcedure.CreateGenericDescriptor(const GenericParams: TIDTypeList; const SRCPosition: TParserPosition);
begin
  New(FGenericDescriptor);
  FGenericDescriptor.FScope := nil;
  FGenericDescriptor.FGenericParams := GenericParams;
  FGenericDescriptor.FIntfSRCPosition := SRCPosition;
  FGenericDescriptor.FImplSRCPosition := SRCPosition;
end;

destructor TIDProcedure.Destroy;
begin
  FIL.Free;
  FFinalSection.Free;
  if Assigned(FGenericDescriptor) then
    Dispose(FGenericDescriptor);
  inherited;
end;

function TIDProcedure.SameDeclaration(const ParamsScope: TScope): Boolean;
var
  item1, item2: TScope.PAVLNode;
  Decl1, Decl2: TIDParam;
  AType1, AType2: TIDType;
begin
  if FParamsScope.Count <> ParamsScope.Count then
    Exit(False);
  item1 := FParamsScope.First;
  item2 := ParamsScope.First;
  while Assigned(item1) do begin
    Decl1 := TIDVariable(Item1.Data);
    Decl2 := TIDVariable(Item2.Data);

    if Decl1.IsExplicit <> Decl2.IsExplicit then
      Exit(False);

    AType1 := Decl1.DataType.ActualDataType;
    AType2 := Decl2.DataType.ActualDataType;
    if AType1 <> AType2 then
    begin
      if (AType1.DataTypeID = dtOpenArray) and (AType2.DataTypeID = dtOpenArray) then
      begin
        if TIDArray(AType1).ElementDataType <> TIDArray(AType2).ElementDataType then
          Exit(False);
      end else
      if (AType1.IsGeneric and AType2.IsGeneric) and (AType1.Name = AType2.Name) then
      begin
      end else
        Exit(False);
    end;
    item1 := FParamsScope.Next(item1);
    item2 := ParamsScope.Next(item2);
  end;
  Result := True;
end;

function TIDProcedure.SameDeclaration(const Params: TVariableList): Boolean;
var
  i, c: Integer;
  Param1, Param2: TIDVariable;
begin
  c := Length(Params);
  if c <> Length(ExplicitParams) then
    Exit(False);

  for i := 0 to c - 1 do begin
    Param1 := ExplicitParams[i];
    Param2 := Params[i];
    if not AnsiContainsText(Param1.Name, Param2.Name) or
       (Param1.DataType.ActualDataType <> Param2.DataType.ActualDataType) then
      Exit(False);
  end;
  Result := True;
end;

procedure TIDProcedure.SetEntryScope(const Value: TScope);
begin
  FEntryScope := Value;
  Value.FVarSpace := @FVarSpace;
  Value.FProcSpace := ProcSpace;
end;

procedure TIDProcedure.SetParameters(const Value: TVariableList);
begin
  if Assigned(FExplicitParams) then
    raise Exception.Create('Parameters alrady assigned!');
  FExplicitParams := Value;
  FCount := Length(Value);
end;

procedure TIDProcedure.SetResult(DataType: TIDType);
var
  Param: TIDVariable;
begin
  Param := TIDVariable.Create(EntryScope, Identifier('Result', ID.TextPosition));
  Param.DataType := DataType;
  Param.IncludeFlags([VarParameter, VarOut, VarHiddenParam, VarResult]);
  FResultType := DataType;
  FVarSpace.Add(Param);
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

function TIDProcedure.GetDebugIL: string;
begin
//  if Assigned(FIL) then
//    Result := TIL(FIL).GetAsText(True, True)
//  else
//    Result := ''
end;

function TIDProcedure.GetDebugVariables: string;
begin
  Result := GetVarDebugString(FVarSpace);
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

function TIDProcedure.GetDisplayName: string;
var
  Param: TIDVariable;
  ParamsStr: string;
  i: Integer;
begin
  Result := Name;

  if Assigned(Struct) and not Struct.IsAnonymous then
    Result := Struct.Name + '.' + Result;

  if Result = '' then
    Result := '$anonymous_proc_' + IntToStr(Index);

  if Assigned(FGenericDescriptor) then
    Result := Result + GenericDescriptorAsText(FGenericDescriptor);

  for i := 0 to Length(ExplicitParams) - 1 do begin
    Param := ExplicitParams[i];
    ParamsStr := AddStringSegment(ParamsStr, Param.DataType.DisplayName, ', ');
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

function TIDProcedure.GetIsGeneric: Boolean;
begin
  Result := Assigned(FGenericDescriptor);
end;

function TIDProcedure.GetIsStatic: Boolean;
begin
  Result := (pfOperator in Flags) or (pfStatic in Flags);
end;

function TIDProcedure.GetProcSpace: PProcSpace;
begin
  Result := Addr(FProcSpace);
end;

function TIDProcedure.GetProcTypeName: string;
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
    Result := TIDExpression.Create(FVarSpace.First)
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
  FVarSpace.Add(Result);
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

procedure TIDProcedure.DecRefCount(RCPath: UInt32);
begin
  if FRCPath = RCPath then
    Exit;
  FRCPath := RCPath;
  Dec(FRefCount);
  DecTypesReadCountInSignature(RCPath);
end;

procedure TIDProcedure.DecTypesReadCountInSignature(RCPath: UInt32);
var
  Variable: TIDVariable;
begin
  Variable := FVarSpace.First;
  while Assigned(Variable) do
  begin
    if (VarParameter in Variable.Flags) and
       (Variable.DataType <> FStruct) then
      Variable.DataType.DecRefCount(RCPath);
    Variable := TIDVariable(Variable.NextItem);
  end;
  if Assigned(ResultType) then
    ResultType.DecRefCount(RCPath);
end;

procedure TIDProcedure.IncTypesReadCountInSignature(RCPath: UInt32);
var
  Variable: TIDVariable;
begin
  Variable := FVarSpace.First;
  while Assigned(Variable) do
  begin
    if (VarParameter in Variable.Flags) and
       (Variable.DataType <> FStruct) then
      Variable.DataType.IncRefCount(RCPath);
    Variable := TIDVariable(Variable.NextItem);
  end;
  if Assigned(ResultType) then
    ResultType.IncRefCount(RCPath);
end;

procedure TIDProcedure.MakeSelfParam;
var
  SelfParam: TIDVariable;
begin
  Assert(Assigned(FStruct));
  SelfParam := TIDVariable.Create(FParamsScope, Identifier('Self'), FStruct, [VarParameter, VarConst, VarHiddenParam]);
  if not FParamsScope.InsertID(SelfParam) then
    AbortWorkInternal('self already assigned');
  FVarSpace.InsertFirst(SelfParam);
end;

procedure TIDProcedure.RemoveILReferences(var RCPathCount: UInt32);
begin
//  TIL(FIL).RemoveReferences(RCPathCount);
end;

{ TIDTypeDeclaration }

function TIDType.BinarOperatorFor(const Op: TOperatorID; const Left: TIDType): TIDType;
var
  List: TIDPairList;
  Node: TIDPairList.PAVLNode;
begin
  List := FBinarOperatorsFor[Op];
  if Assigned(List) then begin
    Node := List.Find(Left);
    if Assigned(Node) then
      Exit(TIDType(Node.Data));

    // sys operator
    Node := List.Find(nil);
    if Assigned(Node) then
      Exit(TIDType(Node.Data));
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

destructor TIDType.Destroy;
var
  i: TOperatorID;
begin
  FImplicitsTo.Free;
  FImplicitsIDTo.Free;
  FImplicitsFrom.Free;

  FExplicitsTo.Free;
  FExplicitsFrom.Free;

  for i := opIn to High(TOperatorID)  do
  begin
    FBinarOperators[i].Free;
    FBinarOperatorsFor[i].Free;
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
end;

function TIDType.GetExplicitOperatorTo(const Destination: TIDType): TIDDeclaration;
var
  Node: TIDPairList.PAVLNode;
begin
  Node := FExplicitsTo.Find(Destination);
  if Assigned(Node) then
    Exit(TIDDeclaration(Node.Data));

  Result := SysExplicitToAny;
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

  if Assigned(FImplicitsIDTo) then
  begin
    Node := FImplicitsIDTo.Find(TIDDeclaration(Destination.DataTypeID));
    if Assigned(Node) then
      Exit(TIDDeclaration(Node.Data));
  end;

  Result := SysImplicitToAny;
end;

function TIDType.GetImplicitOperatorFrom(const Source: TIDType): TIDDeclaration;
var
  Node: TIDPairList.PAVLNode;
begin
  Node := FImplicitsFrom.Find(Source);
  if Assigned(Node) then
    Exit(TIDDeclaration(Node.Data));

  Result := SysImplicitFromAny;
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
var
  Pt: TIDType;
begin
  Result := FID.Name;
  if Assigned(FGenericDescriptor) then
    Result := Result + GenericDescriptorAsText(FGenericDescriptor);

  if Result = '' then
    Exit;

  Pt := GetParent;
  while Assigned(Pt) do
  begin
    Result := Pt.DisplayName + '.' + Result;
    Pt := Pt.Parent;
  end;
end;

function TIDType.GetIsReferenced: Boolean;
begin
  Result := IsDataTypeReferenced(FDataTypeID);
end;

function TIDType.GetIsGeneric: Boolean;
begin
  Result := fDataTypeID = dtGeneric;
end;

function TIDType.GetIsInteger: Boolean;
begin
  Result := FDataTypeID in [dtInt8, dtInt16, dtInt32, dtInt64, dtUInt8, dtUInt16, dtUInt32, dtUInt64,
                            dtNativeInt, dtNativeUInt];
end;

function TIDType.GetIsManaged: Boolean;
begin
  Result := cDataTypeManaged[DataTypeID];
end;

function TIDType.GetOperators(const Op: TOperatorID): TIDPairList;
begin
  Result := FBinarOperators[Op];
end;

function TIDType.GetOperatorsFor(const Op: TOperatorID): TIDPairList;
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
var
  List: TIDPairList;
  ExistKey: TIDPairList.PAVLNode;
begin
  Assert(Assigned(Right));

  List := FBinarOperators[Op];
  if not Assigned(List) then begin
    List := TIDPairList.Create;
    FBinarOperators[Op] := List;
  end;
  ExistKey := List.InsertNode(Right, Result);
  if Assigned(ExistKey) and (TIDDeclaration(ExistKey.Data) <> SYSUnit._Boolean) then
    ERROR_OPERATOR_ALREADY_OVERLOADED(Op, Self, Right, Result.TextPosition);
end;

procedure TIDType.OverloadBinarOperatorFor(Op: TOperatorID; const Left: TIDType; const Result: TIDDeclaration);
var
  List: TIDPairList;
begin
  List := FBinarOperatorsFor[Op];
  if not Assigned(List) then begin
    List := TIDPairList.Create;
    FBinarOperatorsFor[Op] := List;
  end;
  if Assigned(List.InsertNode(Left, Result)) then
    ERROR_OPERATOR_ALREADY_OVERLOADED(Op, Self, Left, Result.TextPosition);
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
    ERROR_OPERATOR_ALREADY_OVERLOADED(opImplicit, Destination, Proc, TextPosition);
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
    ERROR_OPERATOR_ALREADY_OVERLOADED(opImplicit, Source, Proc, TextPosition);
end;

procedure TIDType.OverloadImplicitTo(const DestinationID: TDataTypeID; const IntOp: TIDOperator);
var
  Node: TIDPairList.PAVLNode;
begin
  if not Assigned(FImplicitsIDTo) then
    FImplicitsIDTo := TIDPairList.Create;

  Node := FImplicitsIDTo.InsertNode(TIDDeclaration(DestinationID), IntOp);
  if Assigned(Node) then
    AbortWorkInternal(sOperatorForTypesAlreadyOverloadedFmt, [OperatorFullName(opImplicit), DisplayName, GetDataTypeName(DestinationID)]);
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

procedure TIDType.SetGenericDescriptor(const Value: PGenericDescriptor);
begin
  FGenericDescriptor := Value;
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
var
  List: TIDPairList;
  Node: TIDPairList.PAVLNode;
begin
  List := FBinarOperators[Op];
  if Assigned(List) then begin
    Node := List.Find(Right);
    if Assigned(Node) then
      Exit(TIDType(Node.Data));

    // sys operator
    Node := List.Find(nil);
    if Assigned(Node) then
      Exit(TIDType(Node.Data));
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

constructor TIDXXXConstant<T>.CreateAsSystem(Scope: TScope; const Name: string);
begin
  inherited;
  fItemType := itConst;
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
  Result := FDeclaration.DataType;
  Assert(Assigned(Result));
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
  OverloadImplicitTo(Self);
  OverloadExplicitTo(Self);
end;

function TIDStructure.FindField(const Name: string): TIDField;
begin
  Result := FMembers.FindMembers(Name) as TIDField;
end;

function TIDStructure.FindMethod(const Name: string): TIDProcedure;
begin
  Result := TIDProcedure(FMembers.FindMembers(Name));
end;

function TIDStructure.FindVirtualProc(Proc: TIDProcedure): TIDProcedure;
var
  NextProc: TIDProcedure;
begin
  NextProc := FProcSpace.First;
  while Assigned(NextProc) do begin
    if pfVirtual in NextProc.Flags then
    begin
      if AnsiCompareText(NextProc.Name, Proc.Name) = 0 then
      begin
        if NextProc.SameDeclaration(Proc.ExplicitParams) and
          (NextProc.ResultType = Proc.ResultType) then
        Exit(NextProc);
      end;
    end;
    NextProc := TIDProcedure(NextProc.NextItem);
  end;

  if Assigned(FAncestor) then
    Result := FAncestor.FindVirtualProc(Proc)
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
var
  Field: TIDVariable;
  FSize: Integer;
begin
  Result := 0;
  Field := FVarSpace.First;
  while Assigned(Field) do
  begin
    FSize := Field.DataType.DataSize;
    if FSize = -1 then
      Exit(-1);
    Result := Result + FSize;
    Field := TIDVariable(Field.NextItem);
  end;
end;

function TIDStructure.GetDisplayName: string;
var
  s, DTIDName: string;
  Member: TIDDeclaration;
begin
  if ID.Name = '' then
  begin
    s := '';
    Member := FMembers.VarSpace.First;
    while Assigned(Member) do
    begin
      s := AddStringSegment(s, Member.DisplayName + ': ' + Member.DataType.DisplayName, '; ');
      Member := Member.NextItem;
    end;

    case DataTypeID of
      dtRecord: DTIDName := 'record ';
      dtClass: DTIDName := 'class ';
    else
      DTIDName := 'unknown '
    end;

    Result := inherited GetDisplayName + DTIDName + s + ' end';
  end else
    Result := inherited GetDisplayName;
end;

function TIDStructure.GetExtraFlags: Byte;
begin
  Result := 0;
end;

function TIDStructure.GetHasInitFiels: Boolean;
var
  Field: TIDDeclaration;
begin
  Field := FVarSpace.First;
  while Assigned(Field) do begin
    if TIDVariable(Field).DefaultValue <> nil then
      Exit(True);
    Field := Field.NextItem;
  end;
  Result := False;
end;

function TIDStructure.GetIsManaged: Boolean;
var
  Item: TIDVariable;
begin
  Item := FVarSpace.First;
  while Assigned(Item) do
  begin
    if Item.DataType.Managed then
      Exit(True);
    Item := TIDVariable(Item.NextItem);
  end;
  Result := False;
end;

function TIDStructure.GetLastVirtualIndex: Integer;
var
  Proc: TIDProcedure;
begin
  Result := -1;
  Proc := FProcSpace.First;
  while Assigned(Proc) do begin
    if pfVirtual in Proc.Flags then
      Result := Proc.VirtualIndex;
    Proc := TIDProcedure(Proc.NextItem);
  end;
  if (Result = -1) and Assigned(FAncestor) then
    Result := FAncestor.GetLastVirtualIndex;
end;

function TIDStructure.GetProcSpace: PProcSpace;
begin
  Result := @FProcSpace;
end;

function TIDStructure.GetVarSpace: PVarSpace;
begin
  Result := addr(FVarSpace);
end;

procedure TIDStructure.IncRefCount(RCPath: UInt32);
var
  Fld: TIDVariable;
  Proc: TIDProcedure;
begin
  if FRCPath = RCPath then
    Exit;
  FRCPath := RCPath;

  Inc(FRefCount);
  // проставляем зависимости предку
  if Assigned(FAncestor) then
    FAncestor.IncRefCount(RCPath);
  // проставляем зависимости по типам полей
  Fld := FVarSpace.First;
  while Assigned(Fld) do
  begin
    Fld.DataType.IncRefCount(RCPath);
    Fld := TIDVariable(Fld.NextItem);
  end;
  // проставляем зависимости сигнатурам методов
  Proc := FProcSpace.First;
  while Assigned(Proc) do
  begin
    Proc.IncTypesReadCountInSignature(RCPath);
    Proc := TIDProcedure(Proc.NextItem);
  end;
end;

procedure TIDStructure.DecRefCount(RCPath: UInt32);
var
  Fld: TIDVariable;
  Proc: TIDProcedure;
begin
  if FRCPath = RCPath then
    Exit;
  FRCPath := RCPath;
  Dec(FRefCount);

  // проставляем зависимости предку
  if Assigned(FAncestor) then
    FAncestor.DecRefCount(RCPath);

  // проставляем зависимости по типам полей
  Fld := FVarSpace.First;
  while Assigned(Fld) do
  begin
    Fld.DataType.DecRefCount(RCPath);
    Fld := TIDVariable(Fld.NextItem);
  end;
  // проставляем зависимости сигнатурам методов
  Proc := FProcSpace.First;
  while Assigned(Proc) do
  begin
    Proc.DecTypesReadCountInSignature(RCPath);
    Proc := TIDProcedure(Proc.NextItem);
  end;
end;

procedure TIDStructure.DoCreateStructure;
begin
  fStaticMembers := TStructScope.CreateAsStruct(Scope, Self, @fStaticVarSpace, @fStaticProcSpace, Scope.DeclUnit);
  {$IFDEF DEBUG}fStaticMembers.Name := ID.Name + '.staticmembers';{$ENDIF}
  fMembers := TStructScope.CreateAsStruct(Scope, Self, @fVarSpace, @fProcSpace, Scope.DeclUnit);
  fMembers.AddScope(fStaticMembers);
  {$IFDEF DEBUG}fMembers.Name := ID.Name + '.members';{$ENDIF}
  fOperators := TStructScope.CreateAsStruct(Scope, Self, nil, @fOperatorsSpace, Scope.DeclUnit);
  fOperators.AddScope(fStaticMembers);
  {$IFDEF DEBUG}fOperators.Name := ID.Name + '.operators';{$ENDIF}
end;

function TIDStructure.IsInheritsForm(Ancestor: TIDStructure): Boolean;
var
  AC: TIDStructure;
begin
  AC := FAncestor;
  while Assigned(AC) do begin
    if AC = Ancestor then
      Exit(True);
    AC := AC.Ancestor;
  end;
  Result := False;
end;

function TIDProcedure.GetSelfDecl: TIDVariable;
begin
  Result := TIDVariable(VarSpace.First);
  if Assigned(FResultType) then
    Result := TIDVariable(Result.NextItem);
end;

function TIDProcedure.GetSelfParam: TIDVariable;
begin
  if not Assigned(FStruct) then
    AbortWorkInternal('Self param is not found');
  Result := FVarSpace.First;
  if Assigned(ResultType) then
    Result := Result.NextItem as TIDVariable;
end;

function TIDProcedure.GetSelfParamExpression: TIDExpression;
begin
  Result := TIDExpression.Create(GetSelfParam);
end;

procedure TIDStructure.SetAncestor(const Value: TIDStructure);
begin
  FAncestor := Value;
  FVarSpace.Initialize;
  if Assigned(Value) then
  begin
    if not Assigned(FGenericDescriptor) then
    begin
      fMembers.fAncestorScope := Value.Members;
      fStaticMembers.fAncestorScope := Value.StaticMembers;
    end else
      FGenericDescriptor.Scope.Parent := Value.Members;
    FAncestor.IncRefCount(1);
  end;
end;

procedure TIDStructure.SetGenericDescriptor(const Value: PGenericDescriptor);
{var
  ParentScope: TScope;}
begin
  FGenericDescriptor := Value;
  if Assigned(Value) then begin
    //ParentScope := FMembers.Parent;
    FMembers.Parent := Value.Scope;     // надо править скоупы.
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
  Result := FDimensions[Index];
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

procedure WriteDataTypeIndex(Stream: TStream; DataType: TIDType);
begin
  Stream.WriteStretchUInt(DataType.Index);
  if DataType.Index >= TSYSTEMUnit.SystemTypesCount then
    Stream.WriteStretchUInt(DataType.UnitID);
end;

{ TIDOrdinalType }

function TIDOrdinal.GetElementsCount: UInt64;
begin
  Result := Abs(FHBound - FLBound) + 1;
end;

function TIDOrdinal.GetOrdinal: Boolean;
begin
  Result := True;
end;

{ TIDRefType }


function TIDRefType.GetDisplayName: string;
var
  Pt: TIDType;
begin
  if FID.Name <> '' then
  begin
    Result := FID.Name;
    Pt := GetParent;
    while Assigned(Pt) do
    begin
      Result := Pt.DisplayName + '.' + Result;
      Pt := Pt.Parent;
    end;
  end else
    Result := '^' + ReferenceType.DisplayName;
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


  OverloadExplicitTo(SYSUnit._NativeInt);
  OverloadExplicitTo(SYSUnit._NativeUInt);
  OverloadExplicitFrom(SYSUnit._NativeUInt);
  OverloadExplicitFrom(SYSUnit._NativeInt);
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
  OverloadExplicitToAny(SYSUnit.Operators.ExplicitPointerToAny);

  OverloadImplicitFromAny(SYSUnit.Operators.ImplicitPointerFromAny);
  OverloadExplicitFromAny(SYSUnit.Operators.ExplicitPointerFromAny);
end;

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
  for var i := 0 to Length(fCases) - 1 do
  begin
    var CaseSize: Integer := 0;
    var Field := TIDField(FCases[i].First);
    while Assigned(Field) do begin
      CaseSize := CaseSize + Field.DataType.DataSize;
      Field := TIDField(Field.NextItem);
    end;
    if CaseSize > Result then
      Result := CaseSize;
  end;
end;

function TIDRecord.AddCase: PVarSpace;
var
  Len: Integer;
begin
  Len := Length(fCases);
  SetLength(fCases, Len + 1);
  Result := addr(fCases[Len]);
end;

constructor TIDRecord.Create(Scope: TScope; const Name: TIdentifier);
begin
  inherited Create(Scope, Name);
  FDataTypeID := dtRecord;
  CreateStandardOperators();
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
end;

function TIDDynArray.GetDimension(Index: Integer): TIDOrdinal;
begin
  Result := TIDOrdinal(SYSUnit._UInt32);
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

procedure TIDSet.CreateStandardOperators;
begin
  inherited;
  OverloadImplicitTo(Self);
  OverloadBinarOperator2(opEqual, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opNotEqual, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opAdd, Self, Self);
  OverloadImplicitToAny(SYSUnit.Operators.ImplicitArrayToAny);
  AddBinarySysOperator(opIn, SYSUnit.Operators.Ordinal_In_Set);
  AddBinarySysOperator(opMultiply, SYSUnit.Operators.Multiply_Set);
  OverloadImplicitFromAny(SYSUnit.Operators.ImplicitSetFromAny);
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
  if Name = '' then
    Result := 'set of ' + FBaseType.DisplayName;
end;

procedure TIDSet.IncRefCount(RCPath: UInt32);
begin
  if FRCPath = RCPath then
    Exit;
  FRCPath := RCPath;
  Inc(FRefCount);
  FBaseType.IncRefCount(RCPath);
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
  AbortWorkInternal('Not supported', []);
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
  Idx: Integer;
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

constructor TIDAliasType.CreateAlias(Scope: TScope; const ID: TIdentifier; OriginalType: TIDType);
begin
  inherited Create(Scope, ID);
  FItemType := itType;
  FTypeKind := tkAlias;
  FLinkedType := OriginalType;
  FOriginalType := OriginalType.ActualDataType;
  FDataTypeID := FOriginalType.DataTypeID;
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
  Result := FOriginalType;
end;

function TIDAliasType.GetDisplayName: string;
begin
  Result := inherited GetDisplayName;
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

procedure TIDAliasType.IncRefCount(RCPath: UInt32);
begin
  if FRCPath = RCPath then
    Exit;
  FRCPath := RCPath;
  Inc(FRefCount);
  FOriginalType.IncRefCount(RCPath);
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

constructor TInterfaceScope.Create(AUnit: TASTModule;
                       VarSpace: PVarSpace;
                       ProcSpace: PProcSpace);
begin
  inherited Create(StrCICompare);
  FScopeType := stGlobal;
  FVarSpace := VarSpace;
  FProcSpace := ProcSpace;
  FUnit := AUnit;
  Assert(Assigned(AUnit));
end;

function TInterfaceScope.FindIDRecurcive(const ID: string): TIDDeclaration;
begin
  Result := inherited FindIDRecurcive(ID);

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

function TInterfaceScope.GetName: string;
begin
  Result := fUnit.Name + '$intf_scope';
end;

{ TImplementationScope }

constructor TImplementationScope.Create(InterfaceScope: TScope);
begin
  inherited Create(StrCICompare);
  FScopeType := InterfaceScope.ScopeType;
  FVarSpace := InterfaceScope.VarSpace;
  FProcSpace := InterfaceScope.ProcSpace;
  FIntfScope := InterfaceScope;
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
  Result := fUnit.Name + '$impl_scope';
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
  Result := FOuterScope.FindIDRecurcive(ID);
  if not Assigned(Result) then
    Result := inherited FindIDRecurcive(ID);
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
  fSysExplicitFromAny := SYSUnit.Operators.ExplicitTProcFromAny;
  FSysImplicitFromAny := SYSUnit.Operators.ExplicitTProcFromAny;
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
  Result := Name;
  if Result = '' then
  begin
    for i := 0 to Length(FParams) - 1 do
    begin
      Param := FParams[i];
      Result := AddStringSegment(Result, Param.Name + ': ' + Param.DataType.DisplayName, ';')
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

constructor TProcScope.CreateInBody(Parent: TScope);
begin
  inherited Create(StrCICompare);
  FScopeType := stLocal;
  FParent := Parent;
  FVarSpace := Parent.VarSpace;
  FProcSpace := Parent.ProcSpace;
  FUnit := Parent.DeclUnit;
  Parent.AddChild(Self);
end;

constructor TProcScope.CreateInDecl(Parent: TScope; VarSpace: PVarSpace; ProcSpace: PProcSpace);
begin
  inherited Create(StrCICompare);
  FScopeType := stLocal;
  FParent := Parent;
  FUnit := Parent.DeclUnit;
  FVarSpace := VarSpace;
  FProcSpace := ProcSpace;
  Parent.AddChild(Self);
end;

function TProcScope.GetScopeClass: TScopeClass;
begin
  Result := scProc;
end;

function TProcScope.FindIDRecurcive(const ID: string): TIDDeclaration;
begin
  Result := inherited FindIDRecurcive(ID);
  if not Assigned(Result) and Assigned(FOuterScope) then
    Result := FOuterScope.FindIDRecurcive(ID);
end;

{ TIDClassType }

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
  OverloadImplicitTo(Self);
  if Assigned(SYSUnit) then begin
    OverloadBinarOperator2(opEqual, Self, SYSUnit._Boolean);
    OverloadBinarOperator2(opNotEqual, Self, SYSUnit._Boolean);
    OverloadBinarOperator2(opEqual, SYSUnit._NilPointer, SYSUnit._Boolean);
    OverloadBinarOperator2(opNotEqual, SYSUnit._NilPointer, SYSUnit._Boolean);
    OverloadExplicitTo(SYSUnit._NativeInt);
    OverloadExplicitTo(SYSUnit._NativeUInt);
    OverloadExplicitFrom(SYSUnit._NativeUInt);
    OverloadExplicitFrom(SYSUnit._NativeInt);
    OverloadImplicitTo(dtClass, SYSUnit.Operators.ImplicitClassToClass);
  end;
end;

destructor TIDClass.Destroy;
begin
  FInterfaces.Free;
  FInterfacesMethods.Free;
  inherited;
end;

function TIDClass.FindInterface(const Intf: TIDInterface): Boolean;
begin
  Result := Assigned(FInterfaces) and (FInterfaces.IndexOf(Intf) >= 0);
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
  end;
end;

{ TIDProperty }

constructor TIDProperty.Create(Scope: TScope; const Identifier: TIdentifier);
begin
  inherited Create(Scope, Identifier);
  FItemType := itProperty;
end;

function TIDProperty.GetParamsCount: Integer;
begin
  if Assigned(FParams) then
    Result := FParams.Count
  else
    Result := 0;
end;

{ TMethodScope }

constructor TMethodScope.CreateInDecl(OuterScope, Parent: TScope; VarSpace: PVarSpace; ProcSpace: PProcSpace);
begin
  inherited CreateInDecl(Parent, VarSpace, ProcSpace);
  FOuterScope := OuterScope;
end;

constructor TMethodScope.CreateInDecl(OuterScope, Parent: TScope);
begin
  inherited CreateInDecl(Parent, nil, nil);
  FOuterScope := OuterScope;
end;

function TMethodScope.FindIDRecurcive(const ID: string): TIDDeclaration;
begin
  Result := inherited FindIDRecurcive(ID);
  if Assigned(Result) then
    Exit;
  Result := FOuterScope.FindIDRecurcive(ID);
end;

{ TIDField }

constructor TIDField.Create(Struct: TIDStructure; const Identifier: TIdentifier);
begin
  inherited Create(Struct.Members, Identifier);
  Include(fFlags, VarIsField);
  fStruct := Struct;
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

{ TIDGenericType }

constructor TIDGenericType.Create(Scope: TScope; const ID: TIdentifier);
begin
  inherited Create(Scope, ID);
  FDataTypeID := dtGeneric;
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

procedure TGenericDescriptor.AddGenericInstance(Decl: TIDDeclaration; const Args: TIDExpressions);
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
    Result := Proc.ProcTypeName + ' ' + Proc.Struct.DisplayName + '.' + Proc.DisplayName
  else
    Result := Proc.ProcTypeName + ' ' + Proc.DisplayName;

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

function TGenericDescriptor.FindType(const Name: string): TIDGenericType;
var
  Decl: TIDDeclaration;
begin
  Decl := FScope.FindID(Name);
  if Decl is TIDGenericType then
    Result := TIDGenericType(Decl)
  else
    Result := nil;
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
  OverloadExplicitToAny(SYSUnit.Operators.ExplicitClassOfToAny);
  OverloadExplicitFromAny(SYSUnit.Operators.ExplicitClassOfFromAny);
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

constructor TStructScope.CreateAsStruct(Parent: TScope; Struct: TIDStructure; VarSpace: PVarSpace; ProcSpace: PProcSpace; DeclUnit: TASTModule);
begin
  inherited Create(StrCICompare);
  FScopeType := stStruct;
  FVarSpace := VarSpace;
  FProcSpace := ProcSpace;
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
  Result := format('%s$struct_scope(parent: %s)', [fStruct.Name, Parent.Name]);
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

function TIDClass.GetIsManaged: Boolean;
begin
  Result := True;
end;

procedure TIDClass.IncRefCount(RCPath: UInt32);
begin
  inherited;

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

{ TIDGuidConstant }

function TIDGuidConstant.AsInt64: Int64;
begin
  Result := 0;
  AbortWorkInternal('Not supported', []);
end;

function TIDGuidConstant.AsString: string;
begin
  Result := GUIDToString(FValue);
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
  Members.ProcSpace.Add(RunProc);
  OverloadImplicitTo(dtProcType, SYSUnit.Operators.ImplicitClosureToTMethod);
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

procedure TDlphHelper.SetTarget(const Value: TIDType);
begin
  fTarget := Value;
  if Value is TIDStructure then
  begin
    fMembers.FAncestorScope := TIDStructure(Value).Members;
    fAncestor := TIDStructure(Value);
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

procedure TIDPointerConstant.AssignValue(Source: TIDConstant);
begin
  FValue := Source;
end;

function TIDPointerConstant.AsString: string;
begin
  if Assigned(FValue) and (FValue.ItemType = itConst) then
    Result := TIDConstant(FValue).AsString
  else
    Result := '<uknown>';
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

{ TIDSysVariativeType }

constructor TIDSysVariativeType.CreateAsSystem(Scope: TScope; const Types: TIDTypesArray);
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


function GetConstantClassByDataType(DataTypeID: TDataTypeID): TIDConstantClass;
begin
  case DataTypeID of
    dtInt8, dtInt16, dtInt32, dtInt64,
    dtUInt8, dtUInt16, dtUInt32, dtUInt64,
    dtNativeInt, dtNativeUInt, dtEnum, dtRange: Result := TIDIntConstant;

    dtFloat32, dtFloat64, dtFloat80, dtCurrency: Result := TIDFloatConstant;

    dtBoolean: Result := TIDBooleanConstant;

    dtAnsiChar,
    dtChar: Result := TIDCharConstant;

    dtShortString,
    dtAnsiString,
    dtString,
    dtWideString: Result := TIDStringConstant;

    dtPAnsiChar,
    dtPWideChar,
    dtPointer: Result := TIDPointerConstant;

    dtSet: Result := TIDSetConstant;

    dtStaticArray,
    dtDynArray: Result := TIDDynArrayConstant;
    dtRecord: Result := TIDRecordConstant;
    dtGuid: Result := TIDGuidConstant;

  else
    Result := nil;
    AbortWorkInternal('Unknown constant type');
  end;
end;

{ TConditionalScope }

function TConditionalScope.GetName: string;
begin
  Result := format('conditional$scope(parent: %s)', [Parent.Name]);
end;

initialization

finalization

end.
