unit NPCompiler.Classes;

interface

{$I compilers.inc}

uses System.SysUtils, System.Classes, System.StrUtils, System.Math, System.Generics.Collections,
     NPCompiler.DataTypes,
     NPCompiler.Messages,
     iDStringParser,
     System.Variants,
     NPCompiler.Operators,
     AVL,
     NPCompiler.Utils,
     IL.Types,
     NPCompiler.Errors,
     NPCompiler.Intf,
     NPCompiler.Options,
     AST.Classes,
     AST.Project;
type

  TExpessionPosition = (ExprNested, ExprLValue, ExprRValue, ExprNestedGeneric);

  ECompilerAbort = class(EAbort)
  private
    FCompilerMessage: TCompilerMessage;
    function GetCompilerMessage: PCompilerMessage;
  public
    constructor Create(const MessageText: string); overload;
    constructor Create(const MessageText: string; const SourcePosition: TTextPosition); overload;
    constructor CreateAsInteranl(const MessageText: string; const SourcePosition: TTextPosition);
    property CompilerMessage: PCompilerMessage read GetCompilerMessage;
  end;

  ECompilerInternalError = class(ECompilerAbort);

  ECompilerStop = class(ECompilerAbort)
  private
    FCompileSuccess: Boolean;
  public
    constructor Create(CompileSuccess: Boolean); reintroduce;
    property CompileSuccess: Boolean read FCompileSuccess;
  end;

  ECompilerSkip = class(ECompilerAbort)
    constructor Create();
  end;

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
  TIDInterface = class;
  TIDProperty = class;
  TIDOperator = class;
  TIDExpression = class;
  TIDNameSpace = class;
  TIDGenericType = class;
//  TIDUserDefinedMacro = class;
  TIDMacroArgument = class;
  TIDStringConstant = class;
  TIDProcType = class;

  TScope = class;
  TStructScope = class;


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
    itMacroFunction,   // функция времени компиляции
    itProperty,        // свойство
    itAlias,           // алиас
    itType,            // тип
    itNameSpace,       // неймспейс
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

  TCompilerResult = (
    CompileSuccess,
    CompileFail,
    CompileSkip
  );

  INPPackage = interface(IASTProject)
    ['{32BDB0E9-59FD-4116-9B7F-6B2DEAB26F59}']
    function GetStringConstant(const Value: string): Integer; overload;
    function GetStringConstant(const Value: TIDStringConstant): Integer; overload;
    function GetIncludeDebugInfo: Boolean;
    function GetUnitsCount: Integer;
    function GetUnit(Index: Integer): TObject; overload;
    function GetSearchPathes: TStrings;
    function GetOptions: TPackageOptions;
    function GetTarget: string;
    function GetDefines: TDefines;
    function FindUnitFile(const UnitName: string): string;
    function GetUnit(const UnitName: string): TObject; overload;
    function UsesUnit(const UnitName: string; AfterUnit: TASTModule): TASTModule;
    procedure SetIncludeDebugInfo(const Value: Boolean);
    procedure SetRTTICharset(const Value: TRTTICharset);
    procedure SetTarget(const Value: string);
    procedure SaveToStream(Stream: TStream);
    procedure AddUnit(aUnit, AfterUnit: TASTModule); overload;
    procedure AddUnit(const Source: string); overload;
    procedure AddUnitSearchPath(const Path: string; IncludeSubDirectories: Boolean = True);
    procedure Clear;
    procedure InitUnits;
    function GetMessages: ICompilerMessages;
    function GetRTTICharset: TRTTICharset;
    function RefCount: Integer;
    function Compile: TCompilerResult;
    function CompileInterfacesOnly: TCompilerResult;
    function GetPointerSize: Integer;
    function GetNativeIntSize: Integer;
    property Messages: ICompilerMessages read GetMessages;
    property RTTICharset: TRTTICharset read GetRTTICharset write SetRTTICharset;
    property IncludeDebugInfo: Boolean read GetIncludeDebugInfo write SetIncludeDebugInfo;
    property UnitsCount: Integer read GetUnitsCount;
    property Units[Index: Integer]: TObject read GetUnit;
    property SearchPathes: TStrings read GetSearchPathes;
    property Options: TPackageOptions read GetOptions;
    property Target: string read GetTarget write SetTarget;
    property Defines: TDefines read GetDefines;
    property PointerSize: Integer read GetPointerSize;
    property NativeIntSize: Integer read GetNativeIntSize;
  end;

  TVisibility = (vLocal, vPublic, vProtected, vStrictProtected, vPrivate, vStrictPrivate);

  TIDDeclarationClass = class of TIDDeclaration;

  {декларация - базовый класс}
  TIDDeclaration = class(TASTDeclaration)
  private
    FItemType: TIDItemType;
    //FID: TIdentifier;                // Идентификатор
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
    function GetDeclUnit: TObject; inline;
    function GetUnitID: Integer;
    function GetIsAnonymous: Boolean; inline;
    procedure SetIndex(const Value: Integer);
    function GetPackage: INPPackage;
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
    //property ID: TIdentifier read FID write FID;
    //property Name: string read FID.Name write FID.Name;
    //property TextPosition: TTextPosition read FID.TextPosition write FID.TextPosition;
    property DisplayName: string read GetDisplayName;
    property Scope: TScope read FScope;
    property DataType: TIDType read FDataType write SetDataType;
    //property SourcePosition: TTextPosition read FID.TextPosition;
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
    property DeclUnit: TObject read GetDeclUnit;
    property UnitID: Integer read GetUnitID;
    property IsAnonymous: Boolean read GetIsAnonymous;
    property SpaceIndex: Integer read GetIndex;
    procedure IncRefCount(RCPath: UInt32); virtual; // добавляет зависимость
    procedure DecRefCount(RCPath: UInt32); virtual; // удаляет зависимость
    {процедура делает DecReadCount для зависимостей}
    procedure RemoveILReferences(var RCPathCount: UInt32); virtual;
    property Package: INPPackage read GetPackage;
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

  TUnarOperatorInfo = record

  end;


  TIDILInstruction = class(TIDDeclaration)
  private
    FILCode: TILCode;
  public
    constructor Create(Scope: TScope; const Name: string; ILCode: TILCode); reintroduce;
    ////////////////////////////////////////////////////////////////////////////
    property ILCode: TILCode read FILCode;
  end;

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

  {пространство имен}
  TIDNameSpace = class(TIDDeclaration)
  private
    FMembers: TScope;
  public
    constructor Create(Scope: TScope; const Identifier: TIdentifier); override;
    property Members: TScope read FMembers;
  end;

  {модуль}
  TIDUnit = class(TIDNameSpace)
  private
    FUnit: TObject;
  public
    constructor Create(Scope: TScope; AUnit: TObject);
  end;

  {тип - базовый класс}
  TIDType = class(TIDDeclaration)
  type
    TUnarOperators = array [opAssignment..opNot] of TIDDeclaration;
    TBinarOperators = array [opIn..High(TOperatorID)] of TIDPairList;
  private
    FElementary: Boolean;
    FIsPooled: Boolean;         // если это анонимный тип, то флаг показывает находится ли этот тип в пуле
    FDefaultReference: TIDType; // дефолтный анонимный указатель на этот тип
    FWeakType: TIDWeekRef;
    FDataTypeID: TDataTypeID;
    FTypeKind: TTypeKind;
    // lists of implicit operators
    FImplicitsTo: TIDPairList;      // self -> dest
    FImplicitsIDTo: TIDPairList;    // self -> dest
    FImplicitsFrom: TIDPairList;    // src -> self
    FSysImplicitToAny: TIDOperator;    // self -> any
    FSysImplicitFromAny: TIDOperator;  // any -> self
    // lists of explicits operators
    FExplicitsTo: TIDPairList;
    FExplicitsFrom: TIDPairList;
    fSysExplicitToAny: TIDOperator;     // явное вриведенеие типа к любому
    fSysExplicitFromAny: TIDOperator;   // явное вриведенеие типа к любому

    FUnarOperators: TUnarOperators;
    FBinarOperators: TBinarOperators;
    FBinarOperatorsFor: TBinarOperators;
    FGenericDescriptor: PGenericDescriptor; // содерижт всю информацию по generic-типу/процедуре
    FGenericNextOverload: TIDType;
    /////////////////////////////////////////////////////////
    FPacked: Boolean;
    FNeedForward: Boolean;
    FForwardID: TIdentifier;
    FInitProc: TIDProcedure;
    FCopyProc: TIDProcedure;
    FFinalProc: TIDProcedure;
    function GetOperators(const Op: TOperatorID): TIDPairList;
    function GetOperatorsFor(const Op: TOperatorID): TIDPairList;
    function GetIsReferenced: Boolean;
  protected
    function GetDataSize: Integer; virtual;
    function GetOrdinal: Boolean; virtual;
    function GetDisplayName: string; override;
    function GetIsManaged: Boolean; virtual;
    function GetManagedFlags: TManagedDataTypeFlags; virtual;
    function GetActualDataType: TIDType; virtual;
    function GetParent: TIDType;
    procedure SetGenericDescriptor(const Value: PGenericDescriptor); virtual;
  public
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    constructor Create(Scope: TScope; const ID: TIdentifier); override;
    constructor CreateAsAnonymous(Scope: TScope); override;
    destructor Destroy; override;
    ////////////////////////////////////////////////////////////////////////////
    function GetDefaultReference(Scope: TScope): TIDType;
    property DefaultReference: TIDType read FDefaultReference;
    property WeakRefType: TIDWeekRef read FWeakType write FWeakType;
    property DataTypeID: TDataTypeID read FDataTypeID write FDataTypeID;
    property Elementary: Boolean read FElementary write FElementary;
    property BinarOperators[const Op: TOperatorID]: TIDPairList read GetOperators;
    property BinarOperatorsFor[const Op: TOperatorID]: TIDPairList read GetOperatorsFor;
    property Ordinal: Boolean read GetOrdinal;
    property IsPacked: Boolean read FPacked write FPacked;
    property IsReferenced: Boolean read GetIsReferenced;
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
    property ManagedFlags: TManagedDataTypeFlags read GetManagedFlags;
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

    property GenericDescriptor: PGenericDescriptor read FGenericDescriptor write SetGenericDescriptor;
    property GenericNextOverload: TIDType read FGenericNextOverload write FGenericNextOverload;
    property InitProc: TIDProcedure read FInitProc write FInitProc;
    property CopyProc: TIDProcedure read FCopyProc write FCopyProc;
    property FinalProc: TIDProcedure read FFinalProc write FFinalProc;

    property SysExplicitToAny: TIDOperator read fSysExplicitToAny;
    property SysExplicitFromAny: TIDOperator read fSysExplicitFromAny;
    property SysImplicitToAny: TIDOperator read FSysImplicitToAny;
    property SysImplicitFromAny: TIDOperator read fSysImplicitFromAny;

    procedure SaveDeclToStream(Stream: TStream; const Package: INPPackage); virtual;
  end;

  {тип - специальный, для обобщенных типов}
  TIDGenericType = class(TIDType)
  private
    FDefaultValue: TIDExpression;
  public
    constructor Create(Scope: TScope; const ID: TIdentifier); override;
    property DefaultValue: TIDExpression read FDefaultValue write FDefaultValue;
  end;

  {тип - ????}
  TIDAliasType = class(TIDType)
  private
    FOriginalType: TIDType; // оригинальный тип (не алиас)
    FLinkedType: TIDType;   // тип на который ссылается алиас
  protected
    function GetActualDataType: TIDType; override;
    function GetOrdinal: Boolean; override;
    function GetIndex: Integer; override;
  public
    constructor CreateAlias(Scope: TScope; const ID: TIdentifier; OriginalType: TIDType);
    constructor CreateAliasAsSystem(Scope: TScope; const ID: string; SrcType: TIDType);
    procedure IncRefCount(RCPath: UInt32); override;
    procedure DecRefCount(RCPath: UInt32); override;
    property LinkedType: TIDType read FLinkedType;
    property Original: TIDType read FOriginalType;
  end;

  {тип - указатель}
  TIDPointer = class(TIDType)
  private
    FReferenceType: TIDType;
    function GetReferenceType: TIDType;
  protected
    function GetDisplayName: string; override;
  public
    constructor CreateAnonymous(Scope: TScope; ReferenceType: TIDType); reintroduce; virtual;
    constructor Create(Scope: TScope; const ID: TIdentifier); override;
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    procedure IncRefCount(RCPath: UInt32); override;
    procedure DecRefCount(RCPath: UInt32); override;
    procedure SaveDeclToStream(Stream: TStream; const Package: INPPackage); override;
    property ReferenceType: TIDType read GetReferenceType write FReferenceType;
  end;

  TIDClassOf = class(TIDPointer)
  protected
    function GetDisplayName: string; override;
  public
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    constructor Create(Scope: TScope; const Identifier: TIdentifier); override;
  end;

  TIDWeekRef = class(TIDPointer)
  protected
    function GetDisplayName: string; override;
  public
    constructor CreateAnonymous(Scope: TScope; ReferenceType: TIDType); override;
  end;

  {тип - специальный, только для константы nullptr}
  TIDNullPointerType = class(TIDPointer)
  end;

  {тип - ordinal}
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
    procedure SaveDeclToStream(Stream: TStream; const Package: INPPackage); override;
  end;

  {алиас}
  TIDAlias = class(TIDDeclaration)
  private
    FOriginalDecl: TIDDeclaration;   // оригинальная декларация (не псевдоним)
  protected
    function GetOriginalDecl: TIDDeclaration; override;
  public
    constructor CreateAlias(Scope: TScope; const ID: TIdentifier; Decl: TIDDeclaration);
    property Original: TIDDeclaration read FOriginalDecl;
  end;

  {тип - диаппазон}
  TIDRangeType = class(TIDOrdinal)
  private
    FRangeType: TIDType;
  protected
    function GetDisplayName: string; override;
  public
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    constructor CreateAsAnonymous(Scope: TScope); override;
    constructor Create(Scope: TScope; const Identifier: TIdentifier); override;
    procedure Init;
    property ElementType: TIDType read FRangeType write FRangeType;
  end;

  {тип - перечисление}
  TIDEnum = class(TIDOrdinal)
  private
    FItems: TScope;
  protected
    function GetDisplayName: string; override;
    function GetDataSize: Integer; override;
  public
    constructor Create(Scope: TScope; const ID: TIdentifier); override;
    constructor CreateAsAnonymous(Scope: TScope); override;
    property Items: TScope read FItems write FItems;
  end;

  TStructFlags = set of (StructCompleted);

  {тип - структура (базовый класс)}
  TIDStructure = class(TIDType)
  private
    FAncestor: TIDStructure;
    FMembers: TStructScope;  // члены структуры
    FVarSpace: TVarSpace;
    FProcSpace: TProcSpace;
    FStrucFlags: TStructFlags;
    FDefaultProperty: TIDProperty;
    FClassOfType: TIDClassOf;
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
  public
    constructor Create(Scope: TScope; const Name: TIdentifier); override;
    constructor CreateAsAnonymous(Scope: TScope); override;
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    ////////////////////////////////////////////////////////////////////////////
    property Members: TStructScope read FMembers;
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
    procedure AddMethod(const Decl: TIDProcedure); inline;
    function AddField(const Name: string; DataType: TIDType): TIDField;
    function FindField(const Name: string): TIDField;
    function FindMethod(const Name: string): TIDProcedure;
    procedure IncRefCount(RCPath: UInt32); override;
    procedure DecRefCount(RCPath: UInt32); override;
    procedure SaveMethodBodiesToStream(Stream: TStream; const Package: INPPackage);
    procedure SaveDeclToStream(Stream: TStream; const Package: INPPackage); override;
  end;

  {тип - структура}
  TIDRecord = class(TIDStructure)
  private
    FStaticConstructor: TIDProcedure;
    FStaticDestructor: TIDProcedure;
  protected
  public
    constructor Create(Scope: TScope; const Name: TIdentifier); override;
    property StaticConstructor: TIDProcedure read FStaticConstructor write FStaticConstructor;
    property StaticDestructor: TIDProcedure read FStaticDestructor write FStaticDestructor;
  end;

  TIDMethods = array of TIDProcedure;

  {тип - класс}
  TIDClass = class(TIDStructure)
  private
    FInterfaces: TList<TIDInterface>;
    FInterfacesMethods: TList<TIDMethods>;
    function GetInterfacesCount: Integer;
    function GetInterface(Index: Integer): TIDInterface;
  protected
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
    procedure SaveDeclToStream(Stream: TStream; const Package: INPPackage); override;
    property InterfacesCount: Integer read GetInterfacesCount;
    property Interfaces[Index: Integer]: TIDInterface read GetInterface;
    procedure IncRefCount(RCPath: UInt32); override;
    procedure DecRefCount(RCPath: UInt32); override;
  end;

  {тип - замыкание}
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
    function GetManagedFlags: TManagedDataTypeFlags; override;
  public
    constructor CreateClosure(const DeclProc, RunProc: TIDProcedure);
    destructor Destroy; override;
    {функция GetCapturedVar возвращает поле замыкания которое соответствует захватываемой переменной}
    function GetCapturedVar(const CapturedVar: TIDVariable): TIDField;
    function CaptureByReference: Boolean;
    property CapturedVars: TCapturedVars read FCapturedVars;

  end;

  {тип - интрефейс}
  TIDInterface = class(TIDStructure)
  private
    FGUID: TGUID;
  protected
    function GetIsManaged: Boolean; override;
  public
    constructor Create(Scope: TScope; const Name: TIdentifier); override;
    procedure SaveDeclToStream(Stream: TStream; const Package: INPPackage); override;
    property GUID: TGUID read FGUID write FGUID;
  end;

  {тип - массив (базовый класс)}
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
    function GetManagedFlags: TManagedDataTypeFlags; override;
    function GetDataSize: Integer; override;
  public
    constructor Create(Scope: TScope; const Name: TIdentifier); override;
    constructor CreateAsAnonymous(Scope: TScope); overload; override;
    constructor CreateAnonymousStatic1Dim(Scope: TScope; ElementDataType: TIDType; Length: Integer; out BoundType: TIDOrdinal); overload;
    ////////////////////////////////////////////////////////////////////////////
    property ElementDataType: TIDType read FElementDataType write FElementDataType;
    property DimensionsCount: Integer read FDimensionsCount;
    property Dimensions[Index: Integer]: TIDOrdinal read GetDimension;
    procedure AddBound(Bound: TIDOrdinal);
    procedure SaveDeclToStream(Stream: TStream; const Package: INPPackage); override;
    procedure IncRefCount(RCPath: UInt32); override;
    procedure DecRefCount(RCPath: UInt32); override;
  end;

  {тип - битовый набор}
  TIDSet = class(TIDArray)
  private
    FBaseType: TIDType;
    function GetBitsCount: UInt32; inline;
  protected
    function GetDisplayName: string; override;
    function GetDataSize: Integer; override;
  public
    constructor CreateAnonymous(Scope: TScope; BaseType: TIDType); reintroduce;
    constructor Create(Scope: TScope; const ID: TIdentifier); override;
    ////////////////////////////////////////////////////////////////////////////
    property BitsCount: UInt32 read GetBitsCount;
    property BaseType: TIDType read FBaseType write FBaseType;
    procedure IncRefCount(RCPath: UInt32); override;
    procedure SaveDeclToStream(Stream: TStream; const Package: INPPackage); override;
  end;

  {тип - динамический массив}
  TIDDynArray = class(TIDArray)
  private
    function GetDimension(Index: Integer): TIDOrdinal; override;
  protected
    function GetManagedFlags: TManagedDataTypeFlags; override;
  public
    constructor Create(Scope: TScope; const Name: TIdentifier); override;
    constructor CreateAsAnonymous(Scope: TScope); override;
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    ////////////////////////////////////////////////////////////////////////////
  end;

  {тип - строка}
  TIDString = class(TIDDynArray)
  public
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
  end;

  {тип - вариант}
  TIDVariant = class(TIDType)
  end;

  {тип - открытый массив}
  TIDOpenArray = class(TIDDynArray)
  strict private
    {$HINTS OFF}
    constructor Create(Scope: TScope; const Name: TIdentifier); override; deprecated 'Only CreateAnonymous constructor allowed for this type';
    {$HINTS ON}
  protected
    function GetManagedFlags: TManagedDataTypeFlags; override;
  public
    constructor CreateAsAnonymous(Scope: TScope); override;
    ////////////////////////////////////////////////////////////////////////////
    procedure SaveDeclToStream(Stream: TStream; const Package: INPPackage); override;
    //property Flags: Integer read FFlags write FFlags;
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
    ptClassDestructor     // классовый деструктор
  );

  TCallConvention = (ConvNative, ConvRegister, ConvStdCall, ConvCDecl, ConvFastCall);

  {процедурный тип}
  TIDProcType = class(TIDType)
  private
    FParams: TVariableList;
    FIsStatic: Boolean;
    FResultType: TIDType;
    FCallConv: TCallConvention;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Scope: TScope; const Identifier: TIdentifier); override;
    constructor CreateAsAnonymous(Scope: TScope); override;
    ////////////////////////////////////////////////////////////////////////
    procedure SaveDeclToStream(Stream: TStream; const Package: INPPackage); override;
    procedure AddParam(const ID: TIdentifier; DataType: TIDType);
    property Params: TVariableList read FParams write FParams;
    property ResultType: TIDType read FResultType write FResultType;
    property IsStatic: Boolean read FIsStatic write FIsStatic;
    property CallConv: TCallConvention read FCallConv write FCallConv;
  end;

  TIDRangeExpression  = record
    LBExpression: TIDExpression;
    HBExpression: TIDExpression;
  end;

  {константа (базовый класс)}
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
    // WriteToStream пишет значение константы в Stream фиксированного размера (согласно типу)
    procedure WriteToStream(Stream: TStream; const Package: INPPackage); virtual; abstract;
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

  {константа - базовый класс}
  TIDXXXConstant<T> = class(TIDConstant)
  private
    FValue: T;
  public
    constructor Create(Scope: TScope; const Identifier: TIdentifier; DataType: TIDType; Value: T); overload;
    constructor CreateAsSystem(Scope: TScope; const Name: string); override;
    constructor CreateAnonymous(Scope: TScope; DataType: TIDType; Value: T);
    procedure AssignValue(Source: TIDConstant); override;
    procedure WriteToStream(Stream: TStream; const Package: INPPackage); override;
    property Value: T read FValue write FValue;
  end;

  {константа целочисленная}
  TIDIntConstant = class(TIDXXXConstant<Int64>)
  public
    function ValueDataType: TDataTypeID; override;
    function ValueByteSize: Integer; override;
    function AsInt64: Int64; override;
    function AsString: string; override;
    function AsVariant: Variant; override;
    function CompareTo(Constant: TIDConstant): Integer; override;
  end;

  {константа специальная = размер обьекта}
  TIDSizeofConstant = class(TIDXXXConstant<TIDType>)
    function ValueByteSize: Integer; override;
    function ValueDataType: TDataTypeID; override;
    function AsInt64: Int64; override;
    function AsString: string; override;
    function AsVariant: Variant; override;
    function CompareTo(Constant: TIDConstant): Integer; override;
    procedure IncRefCount(RCPath: UInt32); override;
    procedure DecRefCount(RCPath: UInt32); override;
  end;

  {константа с плавоющей запятой}
  TIDFloatConstant = class(TIDXXXConstant<Double>)
  public
    procedure WriteToStream(Stream: TStream; const Package: INPPackage); override;
    function ValueDataType: TDataTypeID; override;
    function ValueByteSize: Integer; override;
    function AsInt64: Int64; override;
    function AsString: string; override;
    function AsVariant: Variant; override;
    function CompareTo(Constant: TIDConstant): Integer; override;
  end;

  {константа строковая}
  TIDStringConstant = class(TIDXXXConstant<string>)
  public
    procedure WriteToStream(Stream: TStream; const Package: INPPackage); override;
    function ValueByteSize: Integer; override;
    function ValueDataType: TDataTypeID; override;
    function AsInt64: Int64; override;
    function AsString: string; override;
    function AsVariant: Variant; override;
    function StrLength: Integer;
    function CompareTo(Constant: TIDConstant): Integer; override;
  end;

  {константа символьная}
  TIDCharConstant = class(TIDXXXConstant<char>)
  public
    procedure WriteToStream(Stream: TStream; const Package: INPPackage); override;
    function ValueDataType: TDataTypeID; override;
    function ValueByteSize: Integer; override;
    function AsInt64: Int64; override;
    function AsString: string; override;
    function AsVariant: Variant; override;
    function CompareTo(Constant: TIDConstant): Integer; override;
  end;

  {константа булева}
  TIDBooleanConstant = class(TIDXXXConstant<Boolean>)
    function ValueDataType: TDataTypeID; override;
    function ValueByteSize: Integer; override;
    function AsInt64: Int64; override;
    function AsString: string; override;
    function AsVariant: Variant; override;
    function CompareTo(Constant: TIDConstant): Integer; override;
  end;

  {константа массив}
  TIDDynArrayConstant = class(TIDXXXConstant<TIDExpressions>)
  private
    FStatic: Boolean;
    function GetLength: Integer;
  public
    procedure WriteToStream(Stream: TStream; const Package: INPPackage); override;
    function ValueDataType: TDataTypeID; override;
    function ValueByteSize: Integer; override;
    function AsInt64: Int64; override;
    function AsString: string; override;
    function AsVariant: Variant; override;
    function CompareTo(Constant: TIDConstant): Integer; override;
    property ArrayLength: Integer read GetLength;
    {статический массив полностью состоит из констант и может размещатся в статической памяти}
    {не статический массив содержит переменные, поэтому может размещатся только в динамической (стек или куча) памяти}
    property ArrayStatic: Boolean read FStatic write FStatic;
    procedure AddItem(const Item: TIDExpression);
  end;

  TConstSpace = TSpace<TIDConstant>;

  {константа диаппазон}
  TIDRangeConstant = class(TIDXXXConstant<TIDRangeExpression>)
  protected
    function GetDisplayName: string; override;
  public
    function ValueDataType: TDataTypeID; override;
    function ValueByteSize: Integer; override;
    function AsInt64: Int64; override;
    function AsString: string; override;
    function AsVariant: Variant; override;
    function CompareTo(Constant: TIDConstant): Integer; override;
  end;

  {константа GUID}
  TIDGuidConstant = class(TIDXXXConstant<TGUID>)
  public
    function ValueDataType: TDataTypeID; override;
    function ValueByteSize: Integer; override;
    function AsString: string; override;
    function AsInt64: Int64; override;
    function AsVariant: Variant; override;
    function CompareTo(Constant: TIDConstant): Integer; override;
    procedure WriteToStream(Stream: TStream; const Package: INPPackage); override;
  end;

  TIDRecordConstantField = record
    Field: TIDField;
    Value: TIDExpression;
  end;
  TIDRecordConstantFields = array of TIDRecordConstantField;

  TIDRecordConstant = class(TIDXXXConstant<TIDRecordConstantFields>)
  end;

  TExpressonType = (
    etDeclaration,
    etExpressionList
  );

  {выражение}
  TIDExpression = class(TPooledObject)
  private
    FDeclaration: TIDDeclaration;     // декларация
    FInstruction: TObject;            // иснтрукция результатом которой стало это выражение
                                      // необходима для дальнейшей оптимизации
    FTextPosition: TTextPosition;     // позиция в тексте где встретилось выражение
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
    //function GetAsUserDefinedMacro: TIDUserDefinedMacro; inline;
    class function GetExpressionType: TExpressonType; virtual;
    function GetIsLocalVar: Boolean;
    function GetIsVariable: Boolean; inline;
    function GetLine: Integer; inline;
    function GetAsMacroArgument: TIDMacroArgument;
    function GetIsTMPVar: Boolean;
    function GetIsTMPRef: Boolean;
    function GetAsCharConst: TIDCharConstant;
    function GetAsClosure: TIDClosure;
    function GetIsNullableVariable: Boolean;
    function GetIsAnonymousRef: Boolean;
    function GetIsAnonymousVar: Boolean;
    function GetIsNonAnonimousVariable: Boolean;
    function GetIsAnyLocalVar: Boolean;
    function GetCValue: TIDConstant; inline;
    procedure SetCValue(Value: TIDConstant); inline;        
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
    property IsNonAnonimousVariable: Boolean read GetIsNonAnonimousVariable;
    property IsNullableVariable: Boolean read GetIsNullableVariable;
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
    //property AsUserDefinedMacro: TIDUserDefinedMacro read GetAsUserDefinedMacro;
    property AsArrayConst: TIDDynArrayConstant read GetAsArrayConst;
    property AsRangeConst: TIDRangeConstant read GetAsRangeConst;
    property AsMacroArgument: TIDMacroArgument read GetAsMacroArgument;
    property AsClosure: TIDClosure read GetAsClosure;
    property CValue: TIDConstant read GetCValue write SetCValue;
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
  public
    constructor Create(Declaration: TIDDeclaration; CastDataType: TIDType; const TextPosition: TTextPosition); reintroduce;
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

  {result of logical boolean expression lile: a < b, x = y, ... }
  TIDBoolResultExpression = class(TIDExpression)
  end;


  TVariableFlags = set of
  (
    VarIn,           // входной параметр
    VarOut,          // выходной параметр (out)
    VarInOut,        // параметр переданный по ссылке (var)
    VarConst,        // константный параметр
    VarConstRef,     // константный параметр переданный по ссылке
    VarNotNull,      // переменная является ссылкой not null
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
    FAbsoluteOffset: Integer;     // смещение в байтах
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
    procedure SaveToStream(Stream: TStream; const Package: INPPackage);
    procedure IncludeFlags(const Flags: TVariableFlags);
    procedure SaveAndIncludeFlags(const Flags: TVariableFlags; out PrevFlags: TVariableFlags);
  end;

  {поле структуры}
  TIDField = class(TIDVariable)
  private
    FStruct: TIDStructure;
    function GetFieldIndex: Integer;
  protected
    function GetIndex: Integer; override;
  public
    constructor Create(Struct: TIDStructure; const Identifier: TIdentifier); reintroduce;
    property Struct: TIDStructure read FStruct;
    property FieldIndex: Integer read GetFieldIndex;
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
    pfInline,
    pfPure,
    pfNoReturn,
    pfNoExcept,
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
    FParamsScope: TScope;           // only parameters scope
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

    property NextOverload: TIDProcedure read FNextOverload write FNextOverload;
    property EntryScope: TScope read FEntryScope write SetEntryScope;
    property ParamsScope: TScope read FParamsScope write FParamsScope;
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
    procedure SaveDeclToStream(Stream: TStream; const Package: INPPackage);
    procedure SaveBodyToStream(Stream: TStream; const Package: INPPackage);
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

    property FirstBodyLine: Integer read FFirstBodyLine write FFirstBodyLine;
    property LastBodyLine: Integer read FLastBodyLine write FLastBodyLine;
    property FinalSection: TObject read FFinalSection write FFinalSection;
    function GetDebugVariables: string;
    function GetDebugIL: string;
    {функция выполняет IL код в compile-time и возвращает константное выражение}
    function CECalc(const Args: TIDExpressions): TIDConstant;
  end;

  {пользовательский перегруженный оператор}
  TIDOperator = class(TIDProcedure)
  private
    FOperator: TOperatorID;
    function GetRightOperand: TIDType;
  public
    constructor Create(Scope: TScope; const ID: TIdentifier; &Operator: TOperatorID); reintroduce;
    constructor CreateInternal(Op: TOperatorID);
    /////////////////////////////////////////////////////////////////////////////////////////
    property RightOperand: TIDType read GetRightOperand;
    property OperatorID: TOperatorID read FOperator;
  end;


  TBuiltInFunctionID = (
//    bf_userdefined,        // пользовательская макро-функция
    bf_sysrtfunction,      // системная run-time встроенная функция
    bf_sysctfunction,      // системная compile-time встроенная функция
    bf_assigned,
    bf_inc,
    bf_dec,
    bf_memset,
    bf_length,
    bf_setlength,
    bf_copy,
    bf_move,
    bf_sizeof,
    bf_assert,
    bf_typename,
    bf_current_unit,
    bf_current_function,
    bf_current_line,
    bf_new,
    bf_free,
    bf_getref,
    bf_typeinfo,
    bf_LoBound,
    bf_HiBound,
    bf_Ord,
    bf_include,
    bf_exclude,
    bf_refcount,
    bf_getbit,
    bf_setbit
  );

  TIDMacroArgument = class(TIDVariable)
  private
    FText: string;
    FExpr: TIDExpression;
  public
    property Text: string read FText write FText;
    property ArgExpession: TIDExpression read FExpr write FExpr;
  end;

  TMacroArgs = TList<TIDMacroArgument>;



  {TIDUserDefinedMacro = class(TIDBuiltInFunction)
  private
    //FBody: string;
    FBodyPosition: TParserPosition;
  public
    constructor Create(Scope: TScope; const ID: TIdentifier); reintroduce;
    //property Body: string read FBody;
    //procedure Append(const Text: string);
    property BodyPosition: TParserPosition read FBodyPosition write FBodyPosition;
  end;}

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
    FUnit: TObject;             // модуль (индекс модуля в пакете)
    FParent: TScope;            // Parent scope
    FScopeType: TScopeType;     // Тип scope
    FVarSpace: PVarSpace;       // ссылка на список переменных
    FProcSpace: PProcSpace;     // ссылка на список процедур
    FAdditionalScopes: TScopes; // список дополнительных (присоедененных) областей
    FChilds: TList<TScope>;     // вложенные области (необходимо для корректного удаления)
    {$IFDEF DEBUG}
    FName: string;
   {$ENDIF}
    procedure SetParent(const Value: TScope);
  protected
    function GetScopeClass: TScopeClass; virtual;
    procedure AddChild(Scope: TScope);
    procedure RemoveChild(Scope: TScope);
  public
    constructor Create(ScopeType: TScopeType; DeclUnit: TObject); overload;
    constructor Create(ScopeType: TScopeType; Parent: TScope); overload;
    constructor Create(ScopeType: TScopeType; VarSpace: PVarSpace; ProcSpace: PProcSpace; Parent: TScope; DeclUnit: TObject); overload;
    destructor Destroy; override;
    //////////////////////////////////////////////////////////////////////
    procedure AddVariable(Declaration: TIDVariable);
    procedure AddProcedure(Declaration: TIDProcedure);
    procedure AddAnonymousProcedure(Declaration: TIDProcedure);
    procedure AddProperty(Declaration: TIDProperty);
    procedure AddScope(Scope: TScope);
    procedure RemoveVariable(Declaration: TIDVariable);
    function FindInChilds(const ID: string): TIDDeclaration;
    function FindIDRecurcive(const ID: string): TIDDeclaration; overload; virtual;
    function FindIDRecurcive(const ID: string; out Expression: TIDExpression): TIDDeclaration; overload; virtual;
    function FindMembers(const ID: string): TIDDeclaration; virtual;
    function GetDeclArray(Recursively: Boolean = False): TIDDeclArray;
    function GetDeclNamesArray(Recursively: Boolean = False): TStrArray;
    property Parent: TScope read FParent write SetParent;
    property ScopeType: TScopeType read FScopeType;
    property VarSpace: PVarSpace read FVarSpace write FVarSpace;
    property ProcSpace: PProcSpace read FProcSpace write FProcSpace;
    property ScopeClass: TScopeClass read GetScopeClass;
    property DeclUnit: TObject read FUnit;
    property AdditionalScopes: TScopes read FAdditionalScopes;
    {$IFDEF DEBUG}property Name: string read FName write FName;{$ENDIF}
  end;

  TProcScope = class(TScope)
  private
    FOuterScope: TScope; // внешняя по отношению к методу, область видимости (секция implementation реализации метода)
  protected
    function GetScopeClass: TScopeClass; override;
  public
    constructor CreateInDecl(Parent: TScope; VarSpace: PVarSpace; ProcSpace: PProcSpace); reintroduce;
    constructor CreateInBody(Parent: TScope); reintroduce;
    property OuterScope: TScope read FOuterScope write FOuterScope;
    function FindIDRecurcive(const ID: string; out Expression: TIDExpression): TIDDeclaration; override;
  end;

  TStructScope = class(TScope)
    FAncestorScope: TScope;
    FStruct: TIDStructure;
  public
    constructor CreateAsStruct(Parent: TScope; Struct: TIDStructure; VarSpace: PVarSpace; ProcSpace: PProcSpace; DeclUnit: TObject); reintroduce;
    function FindIDRecurcive(const ID: string; out Expression: TIDExpression): TIDDeclaration; override;
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
    FInnerScope: TScope;
    FExpression: TIDExpression;       // выражение, которое породило данный Scope (то что написано в WITH ... DO)
  public
    constructor Create(Parent: TScope; Expression: TIDExpression); reintroduce;
    ///////////////////////////////////////
    function FindIDRecurcive(const ID: string; out Expression: TIDExpression): TIDDeclaration; override;
    property InnerScope: TScope read FInnerScope write FInnerScope;
    property Expression: TIDExpression read FExpression;
  end;

  TMethodScope = class(TProcScope)
  private
    // FExpression: TIDExpression;
  public
    constructor CreateInDecl(OuterScope, Parent: TScope; VarSpace: PVarSpace; ProcSpace: PProcSpace); reintroduce; overload;
    constructor CreateInDecl(OuterScope, Parent: TScope); overload;
    function FindIDRecurcive(const ID: string; out Expression: TIDExpression): TIDDeclaration; override;
  end;

  TImplementationScope = class(TScope)
  private
    FIntfScope: TScope;
  protected
    function GetScopeClass: TScopeClass; override;
  public
    constructor Create(InterfaceScope, Parent: TScope); overload;
    function FindID(const Identifier: string): TIDDeclaration; override;
  end;

  TConditionalScope = class(TScope)
  end;

  TIDPairList = class (TAVLTree<TIDDeclaration, TObject>)
  private
    function GetItem(const Key: TIDDeclaration): TObject;
  public
    constructor Create; reintroduce;
    property Items[const Key: TIDDeclaration]: TObject read GetItem;
  end;

  procedure AbortWork(Error: TCompilerError); overload;
  procedure AbortWork(Error: TCompilerError; const SourcePosition: TTextPosition); overload;
  procedure AbortWork(Error: TCompilerError; const Params: array of const; const SourcePosition: TTextPosition); overload;


  procedure AbortWork(const Message: string; const SourcePosition: TTextPosition); overload;
  procedure AbortWork(const MessageFmt: string; const Params: array of const; const SourcePosition: TTextPosition); overload;

  procedure AbortWorkInternal(const Message: string); overload;
  procedure AbortWorkInternal(const Message: string; const SourcePosition: TTextPosition); overload;
  procedure AbortWorkInternal(const Message: string; const Params: array of const); overload;
  procedure AbortWorkInternal(const Message: string; const Params: array of const; const SourcePosition: TTextPosition); overload;

  procedure SaveProcDecls(Stream: TStream; const Package: INPPackage; ProcSpace: PProcSpace);
  procedure SaveProcBodies(Stream: TStream; const Package: INPPackage; ProcSpace: PProcSpace);
  function Identifier(const Name: string; TextPosition: TTextPosition): TIdentifier; overload; inline;
  function Identifier(const Name: string; SourceRow: Integer = 0; SourceCol: Integer = 0): TIdentifier; overload; inline;

  function DeclarationName(Decl: TIDDeclaration; IsList: Boolean = False): string;
  function ExpressionName(Expr: TIDExpression): string;
  function ExpressionsName(const Expressions: TIDExpressions): string;
  function GetProcName(Proc: TIDProcedure; WithParamsDataTypes: Boolean = False): string;
  function IntConstExpression(const Value: Int64): TIDExpression; inline;
  function StrConstExpression(const Value: string): TIDExpression; inline;
  function IDCompare(const Key1, Key2: TIDDeclaration): NativeInt;
  function IDVarCompare(const Key1, Key2: TIDVariable): NativeInt;
  function GetVarDebugString(const VarSpace: TVarSpace): string;
  procedure WriteDataTypeIndex(Stream: TStream; DataType: TIDType);

  procedure IncReadCount(const Expression: TIDExpression; const Instruction: TObject; RCPath: UInt32); inline;
  procedure DecReadCount(const Expression: TIDExpression; const Instruction: TObject; RCPath: UInt32); inline;

  procedure WriteConstToStream(Stream: TStream; Decl: TIDConstant; DataType: TIDType);

implementation

uses IL.Instructions, SystemUnit, OPCompiler, AST.Delphi.Parser;

function IDCompare(const Key1, Key2: TIDDeclaration): NativeInt;
begin
  Result := NativeInt(Key1) - NativeInt(Key2);
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

function IntConstExpression(const Value: Int64): TIDExpression;
var
  DataType: TIDType;
begin
  DataType := SYSUnit.DataTypes[GetValueDataType(Value)];
  Result := TIDExpression.Create(TIDIntConstant.CreateAnonymous(nil, DataType, Value));
end;

function StrConstExpression(const Value: string): TIDExpression; inline;
var
  Decl: TIDStringConstant;
begin
  Decl := TIDStringConstant.CreateAnonymous(nil, SYSUnit._String, Value);
  Result := TIDExpression.Create(Decl);
end;

procedure AbortWork(const Message: string; const SourcePosition: TTextPosition);
begin
  raise ECompilerAbort.Create(Message, SourcePosition);
end;

procedure AbortWorkInternal(const Message: string);
var
  POS: TTextPosition;
begin
  POS.Row := 0;
  POS.Col := 0;
  raise ECompilerAbort.CreateAsInteranl(Message, POS);
end;

procedure AbortWorkInternal(const Message: string; const SourcePosition: TTextPosition);
begin
  raise ECompilerAbort.CreateAsInteranl(Message, SourcePosition);
end;

procedure AbortWorkInternal(const Message: string; const Params: array of const);
var
  POS: TTextPosition;
begin
  POS.Row := 0;
  POS.Col := 0;
  raise ECompilerAbort.CreateAsInteranl(Format(Message, Params), POS);
end;

procedure AbortWorkInternal(const Message: string; const Params: array of const; const SourcePosition: TTextPosition);
begin
  raise ECompilerAbort.CreateAsInteranl(Format(Message, Params), SourcePosition);
end;

procedure AbortWork(Error: TCompilerError);
var
  ErrorStr: string;
begin
  ErrorStr := GetErrorText(Error);
  raise ECompilerAbort.Create(ErrorStr);
end;

procedure AbortWork(Error: TCompilerError; const SourcePosition: TTextPosition); overload;
var
  ErrorStr: string;
begin
  ErrorStr := GetErrorText(Error);
  raise ECompilerAbort.Create(ErrorStr, SourcePosition);
end;

procedure AbortWork(Error: TCompilerError; const Params: array of const; const SourcePosition: TTextPosition); overload;
var
  ErrorStr: string;
begin
  ErrorStr := GetErrorText(Error);
  ErrorStr := Format(ErrorStr, Params);
  raise ECompilerAbort.Create(ErrorStr, SourcePosition);
end;

procedure AbortWork(const MessageFmt: string; const  Params: array of const; const SourcePosition: TTextPosition);
begin
  AbortWork(Format(MessageFmt, Params), SourcePosition);
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

constructor TScope.Create(ScopeType: TScopeType; DeclUnit: TObject);
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

constructor TScope.Create(ScopeType: TScopeType; VarSpace: PVarSpace; ProcSpace: PProcSpace; Parent: TScope; DeclUnit: TObject);
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

function TScope.FindIDRecurcive(const ID: string): TIDDeclaration;
var
  sc: TScope;
begin
  sc := Self;
  repeat
    Result := sc.FindID(ID);
    if Assigned(Result) then Exit;
    Result := sc.FindInChilds(ID);
    if Assigned(Result) then Exit;
    sc := sc.FParent;
  until sc = nil;
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
    AbortWork(sIdentifierRedeclared, [Declaration.Name], Declaration.SourcePosition);
  FProcSpace.Add(Declaration);
end;

procedure TScope.AddProperty(Declaration: TIDProperty);
begin
  if not InsertID(Declaration) then
    AbortWork(sIdentifierRedeclared, [Declaration.Name], Declaration.SourcePosition);
end;

procedure TScope.AddVariable(Declaration: TIDVariable);
begin
  if not InsertID(Declaration) then
    AbortWork(sIdentifierRedeclared, [Declaration.Name], Declaration.SourcePosition);
  FVarSpace.Add(Declaration);
end;

function TScope.FindIDRecurcive(const ID: string; out Expression: TIDExpression): TIDDeclaration;
begin
  Expression := nil;

  // ищим только в себе
  Result := FindID(ID);
  if Assigned(Result) then
    Exit;

  // ищим в своих вложенных
  Result := FindInChilds(ID);
  if Assigned(Result) then
    Exit;

  // если есть родитель - ищем в нем
  if Assigned(FParent) then
    Result := FParent.FindIDRecurcive(ID, Expression);
end;

function TScope.FindInChilds(const ID: string): TIDDeclaration;
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
end;

constructor TIDDeclaration.CreateAsAnonymous(Scope: TScope);
begin
  CreateFromPool;
  FScope := Scope;
end;

constructor TIDDeclaration.CreateAsSystem(Scope: TScope; const Name: string);
begin
  CreateFromPool;
  FScope := Scope;
  FID.Name := Name;
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

function TIDDeclaration.GetDeclUnit: TObject;
begin
  if Assigned(Scope) then
    Result := Scope.DeclUnit
  else
    Result := nil;
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

function TIDDeclaration.GetPackage: INPPackage;
var
  U: TNPUnit;
begin
  U := TNPUnit(DeclUnit);
  Assert(Assigned(U));
  Result := U.Package;
end;

function TIDDeclaration.GetUnitID: Integer;
begin
  if Assigned(FScope) then
    Result := TNPUnit(FScope.DeclUnit).UnitID
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
var
  i: Integer;
  Param: TIDVariable;
begin
  // загрузка аргументов в параметры
  Param := FVarSpace.First;
  if Assigned(FResultType) then
    Param := TIDVariable(Param.NextItem);
  for i := 0 to Length(Args) - 1 do
  begin
    Param.CValue := Args[i].CValue;
    Param := TIDVariable(Param.NextItem);
  end;
  // запуск выполнения IL кода
  if IsCompleted then
    TIL(FIL).CECalc;
  // получение результата
  if Assigned(FResultType) then
    Result := FVarSpace.First.CValue
  else
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
  CreateFromPool;
  FScope := Scope;
  FID.Name := Name;
  ItemType := itProcedure;
  FParamsScope := TMethodScope.CreateInDecl(Scope, Scope, addr(FVarSpace), addr(FProcSpace));
  FEntryScope := FParamsScope;
end;

constructor TIDProcedure.CreateAsSystemMethod(Struct: TIDStructure; const Name: string);
var
  SelfParam: TIDVariable;
begin
  CreateFromPool;
  FScope := Scope;
  FID.Name := Name;
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
  Decl1, Decl2: TIDDeclaration;
begin
  if FParamsScope.Count <> ParamsScope.Count then
    Exit(False);
  item1 := FParamsScope.First;
  item2 := ParamsScope.First;
  while Assigned(item1) do begin
    Decl1 := TIDVariable(Item1.Data);
    Decl2 := TIDVariable(Item2.Data);
    if Decl1.DataType.ActualDataType <> Decl2.DataType.ActualDataType then
      Exit(False);
    item1 := FParamsScope.Next(item1);
    item2 := ParamsScope.Next(item2);
  end;
  Result := True;
end;

procedure TIDProcedure.SaveDeclToStream(Stream: TStream; const Package: INPPackage);
var
  i, c: Integer;
  ProcFlags: UInt8;
  ResultVar: TIDDeclaration;
  ProcName: string;
begin
  c := ParamsCount;
  {флаг процедура/функция}
  if Assigned(ResultType) then
  begin
    ProcFlags := ILPROC_HASRESULT;
    Inc(c);
  end else
    ProcFlags := 0;

  if pfConstructor in Flags then
    ProcFlags := ProcFlags or ILPROC_CONSTRUCTOR;

  if (pfVirtual in Flags) or (Assigned(FStruct) and (FStruct.DataTypeID = dtInterface)) then
    ProcFlags := ProcFlags or ILPROC_VIRTUAL;

  {флаг наличия локальных процедур}
  if Assigned(FProcSpace.First) then
    ProcFlags := ProcFlags or ILPROC_HASLOCALPROCS;

  {флаги экспорт/импорт}
  if (ImportLib > 0) or (Assigned(FStruct) and (FStruct.ImportLib <> 0)) then
    ProcFlags := ProcFlags or ILPROC_IMPORT;

  if Export > 0 then
    ProcFlags := ProcFlags or ILPROC_EXPORT;

  if Assigned(FStruct) and (not (pfOperator in Flags)) then
    ProcFlags := ProcFlags or ILPROC_HASSELFPTR;

  {write flags}
  Stream.WriteUInt8(UInt8(ProcFlags));

  {write calling convention}
  Stream.WriteUInt8(Ord(FCallConv));

  {если процедура экспортная пишем индекс имени}
  if Export > 0 then
    Stream.WriteStretchUInt(Export)
  else
  if Package.IncludeDebugInfo then
  begin
    if Name <> '' then
      ProcName := Name
    else
      ProcName := DisplayName;
    Stream.WriteStretchUInt(Package.GetStringConstant(ProcName));
  end;

  {если процедура импортируемая пишем индекс имени билиотеки и имени функции}
  if ImportLib > 0 then begin
    Stream.WriteStretchUInt(ImportLib);
    Stream.WriteStretchUInt(ImportName);
  end else
  if Assigned(FStruct) and (FStruct.ImportLib <> 0) then
  begin
    Stream.WriteStretchUInt(FStruct.ImportLib);
    Stream.WriteStretchUInt(Package.GetStringConstant(Name));
  end;

  {если метод виртуальный - сохраняем индекс в VMT}
  if (pfVirtual in Flags) or (Assigned(FStruct) and (FStruct.DataTypeID = dtInterface)) then
    Stream.WriteStretchUInt(FVirtualIndex);

  {кол-во параметров}
  Stream.WriteStretchUInt(c);

  FVarSpace.Reindex;

  // RTTI выходного параметра Result
  if Assigned(ResultType) then
  begin
    ResultVar := FVarSpace.First;
    TIDVariable(ResultVar).SaveToStream(Stream, Package);
  end;

  // RTTI входных параметров
  for i := 0 to ParamsCount - 1 do
    FExplicitParams[i].SaveToStream(Stream, Package);
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

procedure TIDProcedure.SaveBodyToStream(Stream: TStream; const Package: INPPackage);
var
  c: Integer;
  Variable: TIDVariable;
begin
  {Локальные переменные}
  c := FVarSpace.Count - ParamsCount;
  if Assigned(FResultType) then
    Dec(c);
  if Assigned(FStruct) and (not (pfOperator in Flags)) then
    Dec(c);

  Stream.WriteStretchUInt(c);
  Variable := TIDVariable(FVarSpace.First);
  while Assigned(Variable) do begin
    if not (VarParameter in Variable.FFlags) then
      Variable.SaveToStream(Stream, Package);
    Variable := TIDVariable(Variable.NextItem);
  end;

  {локальные процедуры}
  if Assigned(ProcSpace.First) then
  begin
    SaveProcDecls(Stream, Package, @FProcSpace);
    SaveProcBodies(Stream, Package, @FProcSpace);
  end;

  {IL код}
  if Assigned(FIL) then
    TIL(FIL).SaveToStream(Stream, Package)
  else
    Stream.WriteUInt8(0);  // IL instructions count
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
  TNPUnit(DeclUnit).Warning(Message, Params, TextPosition);
end;

function TIDProcedure.GetDebugIL: string;
begin
  if Assigned(FIL) then
    Result := TIL(FIL).GetAsText(True, True)
  else
    Result := ''
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
  if Name = '' then
    Result := '$anonymous_proc_' + IntToStr(Index)
  else
    Result := Name;

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
    FDataType := TIDProcType.CreateAsAnonymous(Scope);
    TIDProcType(FDataType).FParams := ExplicitParams;
    TIDProcType(FDataType).FResultType := ResultType;
    TIDProcType(FDataType).IsStatic := not Assigned(Struct);
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
  TNPUnit(DeclUnit).Hint(Message, Params, TextPosition);
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
  TIL(FIL).RemoveReferences(RCPathCount);
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
  FDataType := _MetaType;
end;

constructor TIDType.CreateAsAnonymous(Scope: TScope);
begin
  CreateFromPool;
  FScope := Scope;
  FItemType := itType;
  FImplicitsTo := TIDPairList.Create;
  FImplicitsFrom := TIDPairList.Create;
  FExplicitsTo := TIDPairList.Create;
  FDataType := _MetaType;
end;

constructor TIDType.CreateAsSystem(Scope: TScope; const Name: string);
begin
  inherited CreateAsSystem(Scope, Name);
  FItemType := itType;
  FImplicitsTo := TIDPairList.Create;
  FImplicitsFrom := TIDPairList.Create;
  FExplicitsTo := TIDPairList.Create;
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
  Result := nil;
end;

function TIDType.GetExplicitOperatorTo(const Destination: TIDType): TIDDeclaration;
var
  Node: TIDPairList.PAVLNode;
begin
  Node := FExplicitsTo.Find(Destination);
  if Assigned(Node) then
    Exit(TIDDeclaration(Node.Data));
  Result := nil;
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

  Result := nil;
end;

function TIDType.GetImplicitOperatorFrom(const Source: TIDType): TIDDeclaration;
var
  Node: TIDPairList.PAVLNode;
begin
  Node := FImplicitsFrom.Find(Source);
  if Assigned(Node) then
    Exit(TIDDeclaration(Node.Data));

  Result := nil;
end;

function TIDType.GetActualDataType: TIDType;
begin
  Result := Self;
end;

function TIDType.GetDataSize: Integer;
const
  cDataTypeSizes: array [TDataTypeID] of Integer = (
    {dtInt8}        1,
    {dtInt16}       2,
    {dtInt32}       4,
    {dtInt64}       8,
    {dtUint8}       1,
    {dtUint16}      2,
    {dtUint32}      4,
    {dtUint64}      8,
    {dtNativeInt}  -1,
    {dtNativeUInt} -1,
    {dtFloat32}     4,
    {dtFloat64}     8,
    {dtBoolean}     1,
    {dtAnsiChar}    1,
    {dtChar}        2,
    {dtAnsiString} -1,
    {dtString}     -1,
    {dtVariant}    16,
    {dtGeneric}     0,
    {dtPointer}    -1,
    {dtWeakRef}    -1,
    {dtRange}      -1,
    {dtEnum}        0,
    {dtSet}         0,
    {dtArray}       0,
    {dtDynArray}   -1,
    {dtOpenArray}  -1,
    {dtProcType}    0,
    {dtRecord}      0,
    {dtClass}       0,
    {dtClassOf}    -1,
    {dtInterface}   0,
    {dtGuid}        Sizeof(TGUID)
  );
begin
  case DataTypeID of
    dtPointer: Result := Package.PointerSize;
    dtNativeInt, dtNativeUInt: Result := Package.NativeIntSize;
  else
    Result := cDataTypeSizes[DataTypeID];
  end;
end;

function TIDType.GetDefaultReference(Scope: TScope): TIDType;
begin
  if not Assigned(FDefaultReference) then
  begin
    FDefaultReference := TIDPointer.CreateAnonymous(Scope, Self);

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

function TIDType.GetManagedFlags: TManagedDataTypeFlags;
begin
  Result := cDataTypeManagedFlags[DataTypeID];
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

procedure ERROR_OPERATOR_ALREADY_OVERLOADED(Op: TOperatorID; Type1, Type2: TIDDeclaration; const Position: TTextPosition);
begin
  AbortWorkInternal(msgOperatorForTypesAlreadyOverloadedFmt,
                    [OperatorFullName(Op), Type1.DisplayName, Type2.DisplayName], Position);
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
  if Assigned(fSysExplicitFromAny) then
    ERROR_OPERATOR_ALREADY_OVERLOADED(opExplicit, Self, Op, TextPosition);

  fSysExplicitFromAny := Op;
end;

procedure TIDType.OverloadExplicitToAny(const Op: TIDOperator);
begin
  if Assigned(fSysExplicitToAny) then
    ERROR_OPERATOR_ALREADY_OVERLOADED(opExplicit, Self, Op, TextPosition);

  fSysExplicitToAny := Op;
end;

procedure TIDType.OverloadImplicitTo(const Destination: TIDDeclaration);
begin
  if Assigned(FImplicitsTo.InsertNode(Destination, Destination)) then
    ERROR_OPERATOR_ALREADY_OVERLOADED(opImplicit, Self, Destination, TextPosition);
end;

procedure TIDType.OverloadImplicitTo(const Destination, Proc: TIDDeclaration);
var
  Node: TIDPairList.PAVLNode;
begin
  Node := FImplicitsTo.InsertNode(Destination, Proc);
  if Assigned(Node) then
    ERROR_OPERATOR_ALREADY_OVERLOADED(opImplicit, Destination, Proc, TextPosition);
end;

procedure TIDType.OverloadImplicitFrom(const Source: TIDDeclaration);
begin
  if Assigned(FImplicitsFrom.InsertNode(Source, Source)) then
    ERROR_OPERATOR_ALREADY_OVERLOADED(opImplicit, Self, Source, TextPosition);
end;

procedure TIDType.OverloadImplicitFrom(const Source, Proc: TIDDeclaration);
begin
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
    AbortWorkInternal(msgOperatorForTypesAlreadyOverloadedFmt, [OperatorFullName(opImplicit), DisplayName, GetDataTypeName(DestinationID)]);
end;

procedure TIDType.OverloadImplicitToAny(const Op: TIDOperator);
begin
  if Assigned(FSysImplicitToAny) then
    ERROR_OPERATOR_ALREADY_OVERLOADED(opImplicit, Self, Op, TextPosition);

  FSysImplicitToAny := Op;
end;

procedure TIDType.OverloadImplicitFromAny(const Op: TIDOperator);
begin
  if Assigned(FSysImplicitFromAny) then
    ERROR_OPERATOR_ALREADY_OVERLOADED(opImplicit, Self, Op, TextPosition);

  FSysImplicitFromAny := Op;
end;

procedure TIDType.SaveDeclToStream(Stream: TStream; const Package: INPPackage);
var
  DT: UInt8;
begin
  DT := UInt8(FDataTypeID);
  Stream.WriteUInt8(DT);
  if Package.IncludeDebugInfo then
    Stream.WriteStretchUInt(Package.GetStringConstant(DisplayName));
end;

procedure TIDType.SetGenericDescriptor(const Value: PGenericDescriptor);
begin
  FGenericDescriptor := Value;
end;

function TIDType.UnarOperator(Op: TOperatorID; Right: TIDType): TIDType;
begin
  Result := TIDType(FUnarOperators[Op]);
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
  FValue := TIDXXXConstant<T>(Source).Value;
  FDataType := TIDXXXConstant<T>(Source).DataType;
end;

constructor TIDXXXConstant<T>.Create(Scope: TScope; const Identifier: TIdentifier; DataType: TIDType; Value: T);
begin
  inherited Create(Scope, Identifier);
  FDataType := DataType;
  FValue := Value;
end;

constructor TIDXXXConstant<T>.CreateAnonymous(Scope: TScope; DataType: TIDType; Value: T);
begin
  CreateFromPool;
  FScope := Scope;
  FItemType := itConst;
  FDataType := DataType;
  FValue := Value;
end;

procedure TIDXXXConstant<T>.WriteToStream(Stream: TStream; const Package: INPPackage);
var
  Size: Integer;
begin
  Size := FixedByteSize;
  if Size = -1 then
    Size := PTR_SIZE;

  Stream.Write(FValue, Size)
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
  if Assigned(FLastRInstruction) then
  begin
    if Assigned(FLastWInstruction) then
    begin
      if TILInstruction(FLastRInstruction).Position < TILInstruction(FLastWInstruction).Position then
        Exit(FLastWInstruction);
    end;
    Result := FLastRInstruction;
  end else
    Result := FLastWInstruction;
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

procedure TIDVariable.SaveToStream(Stream: TStream; const Package: INPPackage);
var
  ILFlags: UInt8;
  VarDataType: TIDType;
begin
  ILFlags := 0;

  if IsTemporary then
    ILFlags := ILFlags or ILVAR_TEMP;

  if Reference then
    ILFlags := ILFlags or ILVAR_REFERENCE;

  if Assigned(FAbsolute) then
    ILFlags := ILFlags or ILVAR_HASINDEX;

  // если это константный параметр
  if (VarConst in Flags) or (VarConstRef in Flags) then
    ILFlags := ILFlags or ILVAR_CONSTPARAM;

  // если это глобальная переменная со значением по умолчанию
  if Assigned(DefaultValue) and (Scope.ScopeType = stGlobal) then
    ILFlags := ILFlags or ILVAR_HASVALUE;

  VarDataType := DataType.ActualDataType;

  // если тип обьявлен в другом модуле или не элементарный, пишим индекс модуля
  if Assigned(Scope) and (DeclUnit <> VarDataType.DeclUnit) and not (VarDataType.Elementary) then
    ILFlags := ILFlags + ILVAR_UNITTYPE;

  Stream.WriteUInt8(UInt8(ILFlags));

  if (DeclUnit <> VarDataType.DeclUnit) and not (VarDataType.Elementary) then
    Stream.WriteStretchUInt(TNPUnit(VarDataType.Scope.DeclUnit).UnitID);

  Stream.WriteStretchUInt(VarDataType.Index);

  if Assigned(FAbsolute) then begin
    Stream.WriteStretchUInt(FAbsolute.Index);
    Stream.WriteStretchUInt(FAbsoluteOffset);
  end;

  if Package.IncludeDebugInfo then begin
    {$IFDEF DEBUG} if FID.Name = '' then FID.Name := '$' + IntToStr(Index); {$ENDIF}
    Stream.WriteStretchUInt(Package.GetStringConstant(Name));
  end;

  if Check(ILFlags, ILVAR_HASVALUE) then
    WriteConstToStream(Stream, DefaultValue.AsConst, DataType);
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
  Result := TIDDynArrayConstant(FDeclaration);
end;

function TIDExpression.GetAsBoolConst: TIDBooleanConstant;
begin
  Result := TIDBooleanConstant(FDeclaration);
end;

function TIDExpression.GetAsCharConst: TIDCharConstant;
begin
  Result := FDeclaration as TIDCharConstant;
end;

function TIDExpression.GetAsClosure: TIDClosure;
begin
  Result := TIDClosure(FDeclaration);
end;

function TIDExpression.GetAsConst: TIDConstant;
begin
  Result := TIDConstant(FDeclaration);
end;

function TIDExpression.GetAsDynArrayConst: TIDDynArrayConstant;
begin
  Result := TIDDynArrayConstant(FDeclaration);
end;

function TIDExpression.GetAsIntConst: TIDIntConstant;
begin
  Result := TIDIntConstant(FDeclaration);
end;

function TIDExpression.GetAsMacroArgument: TIDMacroArgument;
begin
  Result := FDeclaration as TIDMacroArgument;
end;

function TIDExpression.GetAsProcedure: TIDProcedure;
begin
  Result := TIDProcedure(FDeclaration);
end;

function TIDExpression.GetAsProperty: TIDProperty;
begin
  Result := TIDProperty(FDeclaration);
end;

function TIDExpression.GetAsRangeConst: TIDRangeConstant;
begin
  Result := TIDRangeConstant(FDeclaration);
end;

function TIDExpression.GetAsStrConst: TIDStringConstant;
begin
  Result := TIDStringConstant(FDeclaration);
end;

function TIDExpression.GetAsType: TIDType;
begin
  Result := TIDType(FDeclaration);
end;

//function TIDExpression.GetAsUserDefinedMacro: TIDUserDefinedMacro;
//begin
//  Result := FDeclaration as TIDUserDefinedMacro;
//end;

function TIDExpression.GetAsVariable: TIDVariable;
begin
  Result := TIDVariable(FDeclaration);
end;

function TIDExpression.GetCValue: TIDConstant;
begin
  Result := FDeclaration.CValue;
end;

function TIDExpression.GetDataType: TIDType;
begin
  Result := FDeclaration.DataType
end;

function TIDExpression.GetDataTypeID: TDataTypeID;
begin
  Result := GetDataType.DataTypeID;
end;

function TIDExpression.GetDataTypeName: string;
begin
  Result := GetDataType.DisplayName
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

function TIDExpression.GetIsNullableVariable: Boolean;
begin
  Result := (FDeclaration.ItemType = itVar) and
            (FDeclaration.DataType.IsReferenced) and
            (not (VarNotNull in TIDVariable(FDeclaration).Flags));
end;

function TIDExpression.GetIsProcedure: Boolean;
begin
  Result := (FDeclaration.ItemType = itProcedure);
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

procedure TIDExpression.SetCValue(Value: TIDConstant);
begin
  FDeclaration.CValue := Value;
end;

{ EComplilerAbort }

constructor ECompilerAbort.Create(const MessageText: string);
begin
  inherited Create(MessageText);
  FCompilerMessage := TCompilerMessage.Create(nil, cmtError, MessageText, TTextPosition.Create(0, 0));
end;

constructor ECompilerAbort.Create(const MessageText: string; const SourcePosition: TTextPosition);
begin
  inherited Create(MessageText);
  FCompilerMessage := TCompilerMessage.Create(nil, cmtError, MessageText, SourcePosition);
end;

constructor ECompilerAbort.CreateAsInteranl(const MessageText: string; const SourcePosition: TTextPosition);
begin
  inherited Create(MessageText);
  FCompilerMessage := TCompilerMessage.Create(nil, cmtInteranlError, MessageText, SourcePosition);
end;

function ECompilerAbort.GetCompilerMessage: PCompilerMessage;
begin
  Result := addr(FCompilerMessage);
end;

{ TIDOperator }

constructor TIDOperator.Create(Scope: TScope; const ID: TIdentifier; &Operator: TOperatorID);
begin
  inherited Create(Scope, ID);
  FOperator := &Operator;
end;

constructor TIDOperator.CreateInternal(Op: TOperatorID);
begin
  CreateFromPool;
  FOperator := Op;
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

procedure TIDStructure.AddMethod(const Decl: TIDProcedure);
begin
  FMembers.ProcSpace.Add(Decl);
  Decl.FScope := FMembers;
end;

constructor TIDStructure.Create(Scope: TScope; const Name: TIdentifier);
begin
  inherited Create(Scope, Name);
  FMembers := TStructScope.CreateAsStruct(Scope, Self, @FVarSpace, @FProcSpace, Scope.DeclUnit);
  {$IFDEF DEBUG}FMembers.Name := Name.Name + '.members';{$ENDIF}
end;

constructor TIDStructure.CreateAsAnonymous(Scope: TScope);
begin
  inherited CreateAsAnonymous(Scope);
  FMembers := TStructScope.CreateAsStruct(Scope, Self, @FVarSpace, @FProcSpace, Scope.DeclUnit);
  {$IFDEF DEBUG}FMembers.Name := 'members';{$ENDIF}
end;

constructor TIDStructure.CreateAsSystem(Scope: TScope; const Name: string);
begin
  inherited;
  FMembers := TStructScope.CreateAsStruct(Scope, Self, @FVarSpace, @FProcSpace, Scope.DeclUnit);
  {$IFDEF DEBUG}FMembers.Name := FID.Name + '.members';{$ENDIF}
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
    FClassOfType := TIDClassOf.CreateAnonymous(FMembers, Self);
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

{function TIDStructure.GetOperator(const ID: TIdentifier): TIDProcedure;
var
  op: TOperator;
  Decl: TIDDeclaration;
begin
  op := GetOperatorID(ID.Name);
  if op = opNone then
    AbortWork(sUnknownOperatorFmt, [ID.Name], ID.TextPosition)
  else
  if op < opIn then
    Decl := FUnarOperators[op]
  else
    Decl := nil;//FBinarOperators[op]
end;}

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

procedure TIDStructure.SaveMethodBodiesToStream(Stream: TStream; const Package: INPPackage);
var
  Proc: TIDProcedure;
begin
  // кол-во методов
  Stream.WriteStretchUInt(FProcSpace.Count);
  // тела методов
  Proc := TIDProcedure(FProcSpace.First);
  while Assigned(Proc) do begin
    Proc.SaveBodyToStream(Stream, Package);
    Proc := TIDProcedure(Proc.NextItem);
  end;
end;

procedure TIDStructure.SaveDeclToStream(Stream: TStream; const Package: INPPackage);
var
  Decl: TIDVariable;
  Proc: TIDProcedure;
  Flags: UInt8;
begin
  inherited SaveDeclToStream(Stream, Package);
  // флаги
  Flags := 0;

  if Assigned(FAncestor) then
    Flags := ILTYPE_HAS_ANCESTOR;

  if FImportName > 0 then
    Flags := Flags or ILTYPE_IMPORT;

  if FPacked then
    Flags := Flags or ILTYPE_PACKED;

  Flags := Flags or GetExtraFlags;

  Stream.WriteUInt8(Flags);

  // сохраняем информацию о предке (если есть)
  if Assigned(FAncestor) then
    WriteDataTypeIndex(Stream, FAncestor);

  // сохраняем информацию об иморте (если есть)
  if FImportName > 0 then begin
    Stream.WriteStretchUInt(FImportLib);
    Stream.WriteStretchUInt(FImportName);
  end;

  // сохраняем поля
  Stream.WriteStretchUInt(FVarSpace.Count);
  Decl := TIDVariable(FVarSpace.First);
  while Assigned(Decl) do begin
    Decl.SaveToStream(Stream, Package);
    Decl := TIDVariable(Decl.NextItem);
  end;

  // сохраняем определения методов
  Stream.WriteStretchUInt(FProcSpace.Count);
  Proc := FProcSpace.First;
  while Assigned(Proc) do begin
    Proc.SaveDeclToStream(Stream, Package);
    Proc := TIDProcedure(Proc.NextItem);
  end;
end;

procedure TIDStructure.SetAncestor(const Value: TIDStructure);
begin
  FAncestor := Value;
  FVarSpace.Initialize;
  if Assigned(Value) then
  begin
    if not Assigned(FGenericDescriptor) then
      FMembers.FAncestorScope := Value.Members
    else
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
    Result := Value - TIDIntConstant(Constant).Value
  else
    Result := -1;
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

procedure TIDFloatConstant.WriteToStream(Stream: TStream; const Package: INPPackage);
var
  f32: Single;
begin
  case DataType.DataTypeID of
    dtFloat32: begin
      f32 := Value;
      Stream.WriteFloat32(F32);
    end;
  else
    Stream.WriteFloat64(Value);
  end;
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

function TIDStringConstant.StrLength: Integer;
begin
  Result := Length(Value);
end;

procedure TIDStringConstant.WriteToStream(Stream: TStream; const Package: INPPackage);
var
  Idx: Integer;
begin
  Idx := Package.GetStringConstant(Self);
  Stream.WriteStretchUInt(Idx);
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

procedure TIDCharConstant.WriteToStream(Stream: TStream; const Package: INPPackage);
var
  ach: Byte;
begin
  if DataType.DataTypeID = dtAnsiChar then begin
    ach := ord(Value);
    Stream.WriteUInt8(ach);
  end else
    Stream.WriteChar(Value);
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
  OverloadImplicitTo(Self);
  if Assigned(SYSUnit) then begin
    OverloadBinarOperator2(opEqual, Self, SYSUnit._Boolean);
    OverloadBinarOperator2(opNotEqual, Self, SYSUnit._Boolean);
  end;
end;

constructor TIDArray.CreateAsAnonymous(Scope: TScope);
begin
  inherited CreateAsAnonymous(Scope);
  FDataTypeID := dtStaticArray;
  OverloadImplicitTo(Self);
  OverloadBinarOperator2(opEqual, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opNotEqual, Self, SYSUnit._Boolean);
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
      Result := AddStringSegment(Result, IntToStr(FDimensions[i].GetElementsCount), ', ');
  end;
begin
  if FID.Name <> '' then
    Exit(FID.Name);

  case FDataTypeID of
    dtStaticArray: Result := 'static array [' + GetDimInfo + '] of ' + FElementDataType.DisplayName;
    dtDynArray: Result := 'array of ' + FElementDataType.DisplayName;
    dtOpenArray: Result := 'openarray of ' + FElementDataType.DisplayName;
  end;
end;

function TIDArray.GetManagedFlags: TManagedDataTypeFlags;
begin
  Result := cDataTypeManagedFlags[DataTypeID];
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

procedure TIDArray.SaveDeclToStream(Stream: TStream; const Package: INPPackage);
var
  i: Integer;
  DAFlags: Byte;
begin
  inherited SaveDeclToStream(Stream, Package);
  case DataTypeID of
    dtStaticArray: begin
      Stream.WriteStretchUInt(FDimensionsCount);
      // сохраняем индексы типов размерностей
      for i := 0 to FDimensionsCount - 1 do
        WriteDataTypeIndex(Stream, FDimensions[i].ActualDataType);
    end;
    dtSet: begin
      WriteDataTypeIndex(Stream, FDimensions[0].ActualDataType);
      Exit;
    end;
    dtDynArray, dtOpenArray: begin
      DAFlags := 0;
      if Assigned(InitProc) then
        DAFlags := DAFlags or ILARRAY_HAS_INIT;
      if Assigned(FinalProc) then
        DAFlags := DAFlags or ILARRAY_HAS_FINAL;
      Stream.WriteUInt8(DAFlags);
      if Assigned(InitProc) then
        Stream.WriteStretchUInt(InitProc.Index);
      if Assigned(FinalProc) then
      begin
        Stream.WriteStretchUInt(FinalProc.UnitID);
        Stream.WriteStretchUInt(FinalProc.Index);
      end;
    end;
  end;
  // тип элемента
  WriteDataTypeIndex(Stream, FElementDataType.ActualDataType);
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

procedure TIDOrdinal.SaveDeclToStream(Stream: TStream; const Package: INPPackage);
begin
  inherited SaveDeclToStream(Stream, Package);
  Stream.WriteBoolean(FSignedBound);
  Stream.WriteInt64(FLBound);
  Stream.WriteInt64(FHBound);
end;

{ TIDPointerType }

constructor TIDPointer.Create(Scope: TScope; const ID: TIdentifier);
begin
  inherited Create(Scope, ID);
  DataTypeID := dtPointer;
  TypeKind := tkRefernce;
  OverloadImplicitTo(Self);
  if Assigned(SYSUnit) then begin
    OverloadBinarOperator2(opEqual, SYSUnit._NilPointer, SYSUnit._Boolean);
    OverloadBinarOperator2(opNotEqual, SYSUnit._NilPointer, SYSUnit._Boolean);
    OverloadBinarOperator2(opEqual, Self, SYSUnit._Boolean);
    OverloadBinarOperator2(opNotEqual, Self, SYSUnit._Boolean);
    OverloadExplicitTo(SYSUnit._NativeInt);
    OverloadExplicitTo(SYSUnit._NativeUInt);
    OverloadImplicitTo(SYSUnit._Pointer);

    OverloadBinarOperator2(opAdd, Self, Self);
    OverloadBinarOperator2(opSubtract, Self, Self);

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
  end;
end;

constructor TIDPointer.CreateAnonymous(Scope: TScope; ReferenceType: TIDType);
begin
  inherited CreateAsAnonymous(Scope);
  FDataTypeID := dtPointer;
  FReferenceType := ReferenceType;
  OverloadImplicitTo(Self);
  if Assigned(SYSUnit) then begin
    OverloadBinarOperator2(opEqual, SYSUnit._NilPointer, SYSUnit._Boolean);
    OverloadBinarOperator2(opNotEqual, SYSUnit._NilPointer, SYSUnit._Boolean);
    OverloadExplicitTo(SYSUnit._NativeInt);
    OverloadExplicitTo(SYSUnit._NativeUInt);
    OverloadImplicitTo(SYSUnit._Pointer);

    OverloadBinarOperator2(opAdd, Self, Self);
    OverloadBinarOperator2(opSubtract, Self, Self);

    OverloadBinarOperator2(opAdd, SYSUnit._Int32, Self);
    OverloadBinarOperator2(opSubtract, SYSUnit._Int32, Self);
  end;
end;

constructor TIDPointer.CreateAsSystem(Scope: TScope; const Name: string);
begin
  inherited;
  FDataTypeID := dtPointer;
end;

function TIDPointer.GetDisplayName: string;
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

function TIDPointer.GetReferenceType: TIDType;
var
  Decl: TIDDeclaration;
begin
  if not Assigned(FReferenceType) and NeedForward then
  begin
    Decl := Scope.FindID(ForwardID.Name);
    if Assigned(Decl) and (Decl.ItemType = itType) then
      FReferenceType := TIDType(Decl)
    else
      TNPUnit.ERROR_UNDECLARED_ID(ForwardID);
  end;
  Result := FReferenceType;
end;

procedure TIDPointer.IncRefCount(RCPath: UInt32);
begin
  if FRCPath = RCPath then
    Exit;
  FRCPath := RCPath;
  Inc(FRefCount);
  if Assigned(FReferenceType) then
    FReferenceType.IncRefCount(RCPath);
end;

procedure TIDPointer.DecRefCount(RCPath: UInt32);
begin
  if FRCPath = RCPath then
    Exit;
  FRCPath := RCPath;
  Dec(FRefCount);
  if Assigned(FReferenceType) then
    FReferenceType.DecRefCount(RCPath);
end;

procedure TIDPointer.SaveDeclToStream(Stream: TStream; const Package: INPPackage);
begin
  inherited;
  WriteDataTypeIndex(Stream, ReferenceType);
end;

{ TIDEnumType }

constructor TIDEnum.Create(Scope: TScope; const ID: TIdentifier);
begin
  inherited Create(Scope, ID);
  DataTypeID := dtEnum;
  OverloadImplicitTo(Self);
  OverloadBinarOperator2(opEqual, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opNotEqual, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opLess, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opLessOrEqual, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opGreater, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opGreaterOrEqual, Self, SYSUnit._Boolean);
  OverloadExplicitFromAny(SYSUnit._ExplicitEnumFromAny);
end;

{ TIDRecordType }

constructor TIDRecord.Create(Scope: TScope; const Name: TIdentifier);
begin
  inherited Create(Scope, Name);
  FDataTypeID := dtRecord;
  OverloadImplicitTo(Self);
  OverloadBinarOperator2(opEqual, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opNotEqual, Self, SYSUnit._Boolean);
end;

constructor TIDEnum.CreateAsAnonymous(Scope: TScope);
begin
  inherited CreateAsAnonymous(Scope);
  DataTypeID := dtEnum;
  OverloadImplicitTo(Self);
  OverloadBinarOperator2(opEqual, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opNotEqual, Self, SYSUnit._Boolean);
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

function TIDDynArray.GetDimension(Index: Integer): TIDOrdinal;
begin
  Result := TIDOrdinal(SYSUnit._UInt32);
end;

function TIDDynArray.GetManagedFlags: TManagedDataTypeFlags;
begin
  Result := [mtNeedClear, dtNeedFinal, dtNeedAlwaysFinal, mtNeedIncRef];
end;

{ TIDSetType }

constructor TIDSet.Create(Scope: TScope; const ID: TIdentifier);
begin
  inherited Create(Scope, ID);
  FElementDataType := SYSUnit._Boolean;
  FDataTypeID := dtSet;
end;

constructor TIDSet.CreateAnonymous(Scope: TScope; BaseType: TIDType);
begin
  inherited CreateAsAnonymous(Scope);
  FElementDataType := SYSUnit._Boolean;
  FDataTypeID := dtSet;
  FBaseType := BaseType;
end;

function TIDSet.GetBitsCount: UInt32;
begin
  Result := FDimensions[0].ElementsCount;
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

procedure TIDSet.SaveDeclToStream(Stream: TStream; const Package: INPPackage);
begin
  inherited SaveDeclToStream(Stream, Package);
  //WriteDataTypeIndex(Stream, FBaseType);
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

function TIDDynArrayConstant.GetLength: Integer;
begin
  Result := Length(Value);
end;

procedure TIDDynArrayConstant.WriteToStream(Stream: TStream; const Package: INPPackage);
var
  i: Integer;
  Expr: TIDExpression;
begin
  for i := 0 to Length(Value) - 1 do
  begin
    Expr := Value[i];
    Expr.AsConst.WriteToStream(Stream, Package);
  end;
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
  Assert(FLast <> Declaration);
  if Assigned(FLast) then
    TIDDeclaration(FLast).FNext := TIDDeclaration(Declaration)
  else
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

{ TIDILInstruction }

constructor TIDILInstruction.Create(Scope: TScope; const Name: string;  ILCode: TILCode);
begin
  inherited Create(Scope, Identifier(Name));
  FILCode := ILCode;
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

function TIDAliasType.GetIndex: Integer;
begin
  Result := FOriginalType.SpaceIndex;
end;

function TIDAliasType.GetOrdinal: Boolean;
begin
  Result := FLinkedType.Ordinal;
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

function TIDCastExpression.GetDataType: TIDType;
begin
  Result := FDataType;
end;

{ TImplementationScope }

constructor TImplementationScope.Create(InterfaceScope, Parent: TScope);
begin
  inherited Create(StrCICompare);
  FScopeType := InterfaceScope.ScopeType;
  FVarSpace := InterfaceScope.VarSpace;
  FProcSpace := InterfaceScope.ProcSpace;
  FIntfScope := InterfaceScope;
  FParent := Parent;
  FUnit := InterfaceScope.DeclUnit;
  Assert(Assigned(FUnit));
end;

function TImplementationScope.FindID(const Identifier: string): TIDDeclaration;
begin
  Result := inherited FindID(Identifier);
  if not Assigned(Result) then
    Result := FIntfScope.FindID(Identifier);
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

function TWithScope.FindIDRecurcive(const ID: string; out Expression: TIDExpression): TIDDeclaration;
begin
  Result := FInnerScope.FindMembers(ID);
  if Assigned(Result) then begin
    Expression := FExpression;
    Exit;
  end;
  Expression := nil;
  Result := FOuterScope.FindIDRecurcive(ID, Expression);
  if not Assigned(Result) then
    Result := inherited FindIDRecurcive(ID, Expression);
end;



{ TIDUserDefinedMacro }

//constructor TIDUserDefinedMacro.Create(Scope: TScope; const ID: TIdentifier);
//begin
//  CreateFromPool;
//  FID := ID;
//  FScope := Scope;
//  FItemType := itMacroFunction;
//  FFunctionID := bf_userdefined;
//end;

{procedure TIDUserDefinedMacro.Append(const Text: string);
begin
  FBody := FBody + Text;
end;}

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

function TIDOpenArray.GetManagedFlags: TManagedDataTypeFlags;
begin
  Result := [];
end;

procedure TIDOpenArray.SaveDeclToStream(Stream: TStream; const Package: INPPackage);
begin
  inherited SaveDeclToStream(Stream, Package);
  //Stream.WriteStretchUInt(FFlags);
end;

{ TIDRangeType }

constructor TIDRangeType.Create(Scope: TScope; const Identifier: TIdentifier);
begin
  inherited;
  Init;
end;

constructor TIDRangeType.CreateAsAnonymous;
begin
  inherited;
  Init;
end;

constructor TIDRangeType.CreateAsSystem(Scope: TScope; const Name: string);
begin
  inherited;
  Init;
end;

function TIDRangeType.GetDisplayName: string;
begin
  Result := Name;
  if (Result = '') and Assigned(FRangeType) then
    Result := FRangeType.DisplayName
  else
    Result := IntToStr(LowBound) + '..' + IntToStr(HighBound);
end;

procedure TIDRangeType.Init;
begin
  OverloadImplicitTo(Self);
  OverloadBinarOperator2(opEqual, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opNotEqual, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opLess, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opLessOrEqual, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opGreater, Self, SYSUnit._Boolean);
  OverloadBinarOperator2(opGreaterOrEqual, Self, SYSUnit._Boolean);
  FDataTypeID := dtRange;
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
  fSysExplicitFromAny := SYSUnit._ExplicitTProcFromAny;
  FSysImplicitFromAny := SYSUnit._ExplicitTProcFromAny;

  FDataTypeID := dtProcType;
end;

constructor TIDProcType.CreateAsAnonymous(Scope: TScope);
begin
  inherited CreateAsAnonymous(Scope);
  fSysExplicitFromAny := SYSUnit._ExplicitTProcFromAny;
  FSysImplicitFromAny := SYSUnit._ExplicitTProcFromAny;

  FDataTypeID := dtProcType;
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

    if not FIsStatic then
      Result := Result + ' of object';
  end;
end;

procedure TIDProcType.SaveDeclToStream(Stream: TStream; const Package: INPPackage);
var
  i, pc: Integer;
  Flags: set of (ProcHasResult, ProcStatic);
  ResultVar: TIDVariable;
begin
  inherited;
  if Assigned(ResultType) then
    Flags := [ProcHasResult]
  else
    Flags := [];

  if FIsStatic then
    Flags := Flags + [ProcStatic];

  {флаги}
  Stream.WriteStretchUInt(Byte(Flags));

  pc := Length(FParams);

  {RTTI выходного параметра Result}
  if Assigned(ResultType) then
  begin
    //WriteDataTypeIndex(Stream, ResultType);
    inc(pc);
  end;

  {кол-во параметров}
  Stream.WriteStretchUInt(pc);

  if Assigned(ResultType) then
  begin
    ResultVar := TIDVariable.CreateAsSystem(Scope, 'Result');
    ResultVar.DataType := ResultType;
    ResultVar.Flags := [VarResult];
    ResultVar.SaveToStream(Stream, Package);
  end;

  {параметры процедуры}
  for i := 0 to Length(FParams) - 1 do
    FParams[i].SaveToStream(Stream, Package);
end;

{ TIDRangeConstant }

function TIDRangeConstant.AsInt64: Int64;
begin
  Result := 0;
  AbortWorkInternal('Not supported', []);
end;

function TIDRangeConstant.AsString: string;
begin
  Result := '';
  AbortWorkInternal('Not supported', []);
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
  Result := FValue.LBExpression.DisplayName + '..' + FValue.HBExpression.DataTypeName;
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

function TProcScope.FindIDRecurcive(const ID: string; out Expression: TIDExpression): TIDDeclaration;
begin
  Result := inherited FindIDRecurcive(ID, Expression);
  if not Assigned(Result) and Assigned(FOuterScope) then
    Result := FOuterScope.FindIDRecurcive(ID, Expression);
end;

procedure SaveProcDecls(Stream: TStream; const Package: INPPackage; ProcSpace: PProcSpace);
var
  Decl: TIDDeclaration;
begin
  {Процедуры}
  Stream.WriteStretchUInt(ProcSpace.Count); // кол-во процедур
  ProcSpace.Reindex;

  {Декларации}
  Decl := ProcSpace.First;
  while Assigned(Decl) do begin
    TIDProcedure(Decl).SaveDeclToStream(Stream, Package);
    Decl := TIDDeclaration(Decl.NextItem);
  end;
end;

procedure SaveProcBodies(Stream: TStream; const Package: INPPackage; ProcSpace: PProcSpace);
var
  Decl: TIDProcedure;
begin
  {тела}
  Decl := ProcSpace.First;
  while Assigned(Decl) do begin
    if Decl.ImportLib = 0 then // тело не пишется если функция импортная
      Decl.SaveBodyToStream(Stream, Package);
    Decl := TIDProcedure(Decl.NextItem);
  end;
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
  OverloadImplicitTo(Self);
  if Assigned(SYSUnit) then begin
    OverloadBinarOperator2(opEqual, Self, SYSUnit._Boolean);
    OverloadBinarOperator2(opNotEqual, Self, SYSUnit._Boolean);
    OverloadBinarOperator2(opEqual, SYSUnit._NilPointer, SYSUnit._Boolean);
    OverloadBinarOperator2(opNotEqual, SYSUnit._NilPointer, SYSUnit._Boolean);
  end;
end;

constructor TIDClass.CreateAsSystem(Scope: TScope; const Name: string);
begin
  inherited;
  FDataTypeID := dtClass;
  if Assigned(SYSUnit) then begin
    OverloadBinarOperator2(opEqual, Self, SYSUnit._Boolean);
    OverloadBinarOperator2(opNotEqual, Self, SYSUnit._Boolean);
    OverloadBinarOperator2(opEqual, SYSUnit._NilPointer, SYSUnit._Boolean);
    OverloadBinarOperator2(opNotEqual, SYSUnit._NilPointer, SYSUnit._Boolean);
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

function TMethodScope.FindIDRecurcive(const ID: string; out Expression: TIDExpression): TIDDeclaration;
begin
  Result := inherited FindIDRecurcive(ID, Expression);
  if Assigned(Result) then
  begin
    // Expression := FExpression; // ???????
    Exit;
  end;
  Result := FOuterScope.FindIDRecurcive(ID, Expression);
end;

{ TIDField }

constructor TIDField.Create(Struct: TIDStructure; const Identifier: TIdentifier);
begin
  inherited Create(Struct.Members, Identifier);
  Include(FFlags, VarIsField);
  FStruct := Struct;
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
  CreateFromPool;
  FID := ID;
  FScope := Scope;
  FItemType := itAlias;
  FOriginalDecl := Decl.Original;
end;

function TIDAlias.GetOriginalDecl: TIDDeclaration;
begin
  Result := FOriginalDecl;
end;

{ TIDSizeofConstant }

function TIDSizeofConstant.AsInt64: Int64;
begin
  Result := FValue.Index;
end;

function TIDSizeofConstant.AsString: string;
begin
  Result := 'sizeof(' + FValue.DisplayName + ')';
end;

function TIDSizeofConstant.AsVariant: Variant;
begin
  Result := NULL;
  AbortWorkInternal('Not supported', []);
end;

function TIDSizeofConstant.ValueByteSize: Integer;
begin
  Result := GetValueByteSize(FValue.Index);
end;

function TIDSizeofConstant.ValueDataType: TDataTypeID;
begin
  Result := GetValueDataType(FValue.Index);
end;

function TIDSizeofConstant.CompareTo(Constant: TIDConstant): Integer;
begin
  if ClassType = Constant.ClassType then
    Result := NativeInt(Value) - NativeInt(TIDIntConstant(Constant).Value)
  else
    Result := -1;
end;

procedure TIDSizeofConstant.IncRefCount(RCPath: UInt32);
begin
  if FRCPath = RCPath then
    Exit;
  FRCPath := RCPath;
  Inc(FRefCount);
  Value.IncRefCount(RCPath);
end;

procedure TIDSizeofConstant.DecRefCount(RCPath: UInt32);
begin
  if FRCPath = RCPath then
    Exit;
  FRCPath := RCPath;
  Dec(FRefCount);
  Value.DecRefCount(RCPath);
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
  ItemType := itNameSpace;
end;

{ TIDUnit }

constructor TIDUnit.Create(Scope: TScope; AUnit: TObject);
begin
  CreateFromPool;
  FScope := Scope;
  FID := TNPUnit(AUnit)._ID;
  FUnit := AUnit;
  FMembers := TNPUnit(AUnit).IntfSection;
  ItemType := itUnit;
end;

{ EComplilerStop }

constructor ECompilerStop.Create(CompileSuccess: Boolean);
begin
  FCompileSuccess := CompileSuccess;
end;

{ TIDClassOf }

constructor TIDClassOf.Create(Scope: TScope; const Identifier: TIdentifier);
begin
  inherited;
  DataTypeID := dtClassOf;
end;

constructor TIDClassOf.CreateAsSystem(Scope: TScope; const Name: string);
begin
  inherited;
  FDataTypeID := dtClassOf;
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

constructor TIDWeekRef.CreateAnonymous(Scope: TScope; ReferenceType: TIDType);
begin
  inherited CreateAnonymous(Scope, ReferenceType);
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

constructor TStructScope.CreateAsStruct(Parent: TScope; Struct: TIDStructure; VarSpace: PVarSpace; ProcSpace: PProcSpace; DeclUnit: TObject);
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

function TStructScope.FindIDRecurcive(const ID: string; out Expression: TIDExpression): TIDDeclaration;
begin
  Result := inherited FindIDRecurcive(ID, Expression);
  if not Assigned(Result) and Assigned(FAncestorScope) then
    Result := FAncestorScope.FindIDRecurcive(ID, Expression);
end;

function TStructScope.FindMembers(const ID: string): TIDDeclaration;
begin
  Result := FindID(ID);
  if Assigned(Result) then
    Exit;

  if Assigned(FAncestorScope) then
    Result := FAncestorScope.FindMembers(ID)
  else
    Result := nil;
end;

{ ECompilerSkip }

constructor ECompilerSkip.Create;
begin

end;

function TIDClass.GetExtraFlags: Byte;
begin
  Result := 0;
  if InterfacesCount > 0 then
    Result := ILTYPE_IMPL_INTERFACES;
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

procedure TIDClass.SaveDeclToStream(Stream: TStream; const Package: INPPackage);
var
  i, j, c: Integer;
  Intf: TIDInterface;
  Proc: TIDProcedure;
  Methods: TIDMethods;
begin
  inherited SaveDeclToStream(Stream, Package);
  c := InterfacesCount;
  if c > 0 then
    Stream.WriteStretchUInt(c);
  for i := 0 to c - 1 do
  begin
    Intf := FInterfaces[i];
    Stream.WriteStretchUInt(Intf.UnitID);
    Stream.WriteStretchUInt(Intf.Index);
    Stream.WriteStretchUInt(Intf.MethodCount);
    // сохраняем индексы методов имплементаторов (согласно списку методов интерфейса)
    Methods := FInterfacesMethods[i];
    for j := 0 to Intf.MethodCount - 1 do
    begin
      Proc := Methods[j];
      Stream.WriteStretchUInt(Proc.MethodIndex);
    end;
  end;
end;

function TIDInterface.GetIsManaged: Boolean;
begin
  Result := True;
end;

procedure TIDInterface.SaveDeclToStream(Stream: TStream; const Package: INPPackage);
begin
  inherited;
  Stream.WriteGuid(FGUID);
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

procedure TIDGuidConstant.WriteToStream(Stream: TStream; const Package: INPPackage);
begin
  Stream.WriteGuid(FValue);
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
  AddMethod(RunProc);
  OverloadImplicitTo(dtProcType, TIDOpImplicitClosureToTMethod.CreateAsIntOp);
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

function TIDClosure.GetManagedFlags: TManagedDataTypeFlags;
begin
  Result := [mtNeedIncRef, dtNeedFinal, dtNeedAlwaysFinal];
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

initialization

finalization

end.
