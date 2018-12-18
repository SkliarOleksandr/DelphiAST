unit IL.Instructions;

interface

{$i compilers.inc}

uses SysUtils, Classes, TypInfo, StrUtils, Math, AVL, IL.Types, NPCompiler.Utils, NPCompiler.DataTypes,
     NPCompiler.Classes; // system

type

  TILInstruction = class;

  // контекст встраивания процедуры
  TPIContext = record
    InlineProc: TIDProcedure;    // встраиваемая процедура/функция
    Instruction: TILInstruction; // текущая встраиваемая инструкция
    MethodSelf: TIDExpression;   // self-указатель для полей/методов встраиваемой процедуры
    EqualList: TIDPairList;      // список соответсвий локальных переменных
  end;

  TCFBlockType = (
    CFB_PROC,             // основной блок процедуры

    CFB_IF,               // блок ветвления IF
    CFB_IF_THEN,          // блок секции THEN ветвления IF
    CFB_IF_ELSE,          // блок секции ELSE ветвления IF

    CFB_FOR_BODY,         // блок тела цикла FOR
    CFB_FOR_ELSE,         // блок секции ELSE циклы FOR

    CFB_CASE,             // блок ветвления CASE
    CFB_CASE_ENTRY,       // блок очередной секции ветвления CASE
    CFB_CASE_ELSE,        // блок секции DEFAULT ветвления CASE

    CFB_WHILE_BODY,       // блок тела цикла WHILE или REPEAT
    CFB_EXCEPT,           // блок секции EXCEPT
    CFB_FINALLY           // блок секции FINALLY ??? нужен ли ???
  );


  TCFBVars = TAVLTree<TIDVariable, Boolean>;

  {Control Flow Block}
  TCFBlock = class(TPooledObject)
  private
    FParent: TCFBlock;
    FType: TCFBlockType;
    FPrev: TCFBlock;
    FLastChild: TCFBlock;
    FVars: TCFBVars;
  protected
    function FindVarInitDOWN(const Variable: TIDVariable): Boolean; virtual;
    function FindVarInitUP(const Variable: TIDVariable; Child: TCFBlock): Boolean;
  public
    constructor Create(Parent: TCFBlock);
    destructor Destroy; override;
    procedure AddVariable(const Variable: TIDVariable);
    property BlockType: TCFBlockType read FType;
    property Parent: TCFBlock read FParent;
    property LastChild: TCFBlock read FLastChild write FLastChild;
    property Prev: TCFBlock read FPrev write FPrev;
    function IsVarInitialized(const Variable: TIDVariable): Boolean; virtual;
  end;

  TCFBIF = class(TCFBlock)
  private
    FTrue: TCFBlock;
    FElse: TCFBlock;
  protected
    function FindVarInitDOWN(const Variable: TIDVariable): Boolean; override;
  public
    function IsVarInitialized(const Variable: TIDVariable): Boolean; override;
  end;

  TCFBCASE = class(TCFBlock)
  private
    //FType: TIDDeclaration;     // тип выражения CASE <Expr> OF ...
    //FFirstItem: TCFBlock;
    FLastItem: TCFBlock;
    //FElseItem: TCFBlock;
  protected
    function FindVarInitDOWN(const Variable: TIDVariable): Boolean; override;
  public
    function IsVarInitialized(const Variable: TIDVariable): Boolean; override;
  end;

  TCFBFOR = class(TCFBlock)
  private
    //FElse: TCFBlock;
  end;


(*====================================================================

  // Алгоритм проверки что некая переменная инициализированна:
  // - каждое выражение имеет ссылку на инструкцию, которая его порадила
  // - каждая инструкция иммет ссылку на TCFBlock в котором она находится
  // 1. Если интрукция чтения переменной находится в том же CF блоке что
  // и инструкция записи, только ниже по списку, значит что переменная
  // инициализированна!
  // 2.

  |-------------------------------------|
  | CFB_COMMON                          |
  |                   |--------------|  |
  |   |-----------|<--| CFB_IF_TRUE  |  |
  |   |           |   |--------------|  |
  |<--|  CFB_IF   |                     |
  |   |           |   |--------------|  |
  |   |-----------|<--| CFB_IF_ELSE  |  |
  |                   |--------------|  |
  |                                     |
  |                                     |
  |                   |--------------|  |
  |   |-----------|<--| CFB_FOR_BODY |  |
  |   |           |   |--------------|  |
  |<--|  CFB_FOR  |                     |
  |   |           |   |--------------|  |
  |   |-----------|<--| CFB_FOR_ELSE |  |
  |                   |--------------|  |
  |                                     |
  |                                     |
  |   |-----------|                     |
  |   |           |   |--------------|  |
  |<--|  CFB_LOOP |<--| CFB_LP_BODY  |  |
  |   |           |   |--------------|  |
  |   |-----------|                     |
  |                                     |
  |                                     |
  |                   |--------------|  |
  |   |-----------|<--| CFB_WH_BODY  |  |
  |   |           |   |--------------|  |
  |<--| CFB_WHILE |                     |
  |   |           |   |--------------|  |
  |   |-----------|<--| CFB_WH_ELSE? |  |
  |                   |--------------|  |
  |                                     |
  |                                     |
  |                   |--------------|  |
  |   |-----------|<--| CFB_CASE_1   |  |
  |   |           |   |--------------|  |
  |   |           |                     |
  |   |           |   |--------------|  |
  |<--| CFB_CASE  |<--| CFB_CASE_N   |  |
  |   |           |   |--------------|  |
  |   |           |                     |
  |   |           |   |--------------|  |
  |   |-----------|<--| CFB_CASE_ELSE|  |
  |                   |--------------|  |
  |                                     |
  |                                     |
  |                   |--------------|  |
  |   |-----------|<--| CFB_EXCEPT   |  |
  |   |           |   |--------------|  |
  |<--| CFB_TRY   |                     |
  |   |           |   |--------------|  |
  |   |-----------|<--| CFB_FINALLY  |  |
  |                   |--------------|  |
  |                                     |
  |-------------------------------------|



====================================================================*)

  TILArgsEnumProc = reference to procedure (const Arg: TIDExpression; var BreakEnum: Boolean);


  TIL = class;

  {контекст вычисления в режиме ConstExpr}
  TILCECalcContext = record
    Next: TILInstruction;       // следующая инструкция
    Cond: Boolean;              // условие инструкций сравнения
    Return: Boolean;            // признак выхода
  end;

  TILInstruction = class(TPooledObject)
  private
    FCondition: TILCondition; // условие исполнения
    FPrev: TILInstruction;
    FNext: TILInstruction;
    FPosition: Integer;
    FCFBlock: TCFBlock;       // указывает какому CF блоку принадлежит инструкция
  protected
    function ArgumentsCount: Integer; virtual;
    function GetArgument(Index: Integer): TIDExpression; virtual;
    function GetLine: Integer; virtual;
    function CondAsText: string;
    procedure CheckArgInit(Proc: TIDProcedure; Expr: TIDExpression);
  public
    constructor Create;
    function Text: string; virtual;
    function ILCode: TILCode; virtual; abstract;
    function ILCodeString(InUpperCase: Boolean = True): string;
    property Condition: TILCondition read FCondition write FCondition;
    property Position: Integer read FPosition write FPosition;
    property Prev: TILInstruction read FPrev;
    property Next: TILInstruction read FNext;
    property Line: Integer read GetLine; // строка исходного кода для которой была создана эта инструкция
    property CFBlock: TCFBlock read FCFBlock;
    procedure SwapArguments(const Context: TPIContext); virtual;
    procedure Write(Proc: TIDProcedure; Stream: TStream); virtual;
    procedure Read(Stream: TStream); virtual;
    procedure RemoveBack(ToInstruction: TILInstruction);
    {процедура увеличивает счетчики чтения/записи для аргументов инструкции}
    procedure IncReferences(var RCPath: UInt32); virtual;
    {процедура уменьшает счетчики чтения/записи для аргументов инструкции}
    procedure DecReferences(var RCPath: UInt32); virtual;
    procedure ProcessVarInit(Proc: TIDProcedure); virtual;
    procedure EnumerateArgs(const EnumProc: TILArgsEnumProc; var BreakEnum: Boolean); virtual;
    {constexpr calculate}
    procedure CECalc(var Ctx: TILCECalcContext); virtual; abstract;
  end;

  TILInstructionClass = class of TILInstruction;

  TILNoArgInstruction = class(TILInstruction)
  private
    FLine: Integer;
  protected
    function GetLine: Integer; override;
  public
    constructor Create(Line: Integer);
    property Line: Integer read GetLine write FLine;
  end;

  TILNope = class(TILNoArgInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure CECalc(var Ctx: TILCECalcContext); override;
  end;

  TILDestInstruction = class(TILInstruction)
  private
    FDestination: TIDExpression;
    procedure SetDestination(const Value: TIDExpression);
  protected
    function ArgumentsCount: Integer; override;
    function GetArgument(Index: Integer): TIDExpression; override;
    function GetLine: Integer; override;
  public
    procedure Init(Condition: TILCondition; Destination: TIDExpression); overload; inline;
    procedure SwapArguments(const Context: TPIContext); override;
    procedure Write(Proc: TIDProcedure; Stream: TStream); override;
    procedure IncReferences(var RCPath: UInt32); override;
    procedure DecReferences(var RCPath: UInt32); override;
    procedure ProcessVarInit(Proc: TIDProcedure); override;
    procedure EnumerateArgs(const EnumProc: TILArgsEnumProc; var BreakEnum: Boolean); override;
    property Destination: TIDExpression read FDestination write SetDestination;
    function Text: string; override;
  end;

  TIL1ArgInstriction = class(TILInstruction)
  private
    FArg: TIDExpression;
  protected
    function ArgumentsCount: Integer; override;
    function GetArgument(Index: Integer): TIDExpression; override;
    function GetLine: Integer; override;
  public
    procedure Init(Condition: TILCondition; Arg: TIDExpression); overload; inline;
    procedure SwapArguments(const Context: TPIContext); override;
    procedure Write(Proc: TIDProcedure; Stream: TStream); override;
    procedure IncReferences(var RCPath: UInt32); override;
    procedure DecReferences(var RCPath: UInt32); override;
    procedure ProcessVarInit(Proc: TIDProcedure); override;
    procedure EnumerateArgs(const EnumProc: TILArgsEnumProc; var BreakEnum: Boolean); override;
    property Arg: TIDExpression read FArg write FArg;
    function Text: string; override;
  end;

  TILDstSrcInstruction = class(TILDestInstruction)
  private
    FSource: TIDExpression;
  protected
    function ArgumentsCount: Integer; override;
    function GetArgument(Index: Integer): TIDExpression; override;
    function GetLine: Integer; override;
  public
    procedure Init(Condition: TILCondition; Destination, Source: TIDExpression); overload; inline;
    procedure SwapArguments(const Context: TPIContext); override;
    procedure Write(Proc: TIDProcedure; Stream: TStream); override;
    procedure IncReferences(var RCPath: UInt32); override;
    procedure DecReferences(var RCPath: UInt32); override;
    procedure EnumerateArgs(const EnumProc: TILArgsEnumProc; var BreakEnum: Boolean); override;
    function Text: string; override;
    procedure ProcessVarInit(Proc: TIDProcedure); override;
    property Source: TIDExpression read FSource write FSource;
  end;

  TILReadDRef = class(TILDstSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILWriteDRef = class(TILDstSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILCompareInstruction = class(TILInstruction)
  private
    FLeft: TIDExpression;
    FRight: TIDExpression;
  protected
    function ArgumentsCount: Integer; override;
    function GetArgument(Index: Integer): TIDExpression; override;
    function GetLine: Integer; override;
  public
    procedure Init(Condition: TILCondition; Left, Right: TIDExpression); overload; inline;
    procedure SwapArguments(const Context: TPIContext); override;
    procedure Write(Proc: TIDProcedure; Stream: TStream); override;
    procedure IncReferences(var RCPath: UInt32); override;
    procedure DecReferences(var RCPath: UInt32); override;
    function Text: string; override;
    procedure ProcessVarInit(Proc: TIDProcedure); override;
    procedure EnumerateArgs(const EnumProc: TILArgsEnumProc; var BreakEnum: Boolean); override;
    property Left: TIDExpression read FLeft write FLeft;
    property Right: TIDExpression read FRight write FRight;
  end;

  TILCmpJmp = class(TILCompareInstruction)
  private
    FDestination: TILInstruction;
  public
    procedure Init(Condition: TILCondition; Left, Right: TIDExpression; Destination: TILInstruction); overload;
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure Write(Proc: TIDProcedure; Stream: TStream); override;
  end;

  TILDstSrcSrcInstruction = class(TILDestInstruction)
  private
    FLeft: TIDExpression;
    FRight: TIDExpression;
  protected
    function ArgumentsCount: Integer; override;
    function GetArgument(Index: Integer): TIDExpression; override;
  public
    procedure Init(Condition: TILCondition; Dest, Left, Right: TIDExpression); overload; inline;
    procedure SwapArguments(const Context: TPIContext); override;
    procedure Write(Proc: TIDProcedure; Stream: TStream); override;
    procedure IncReferences(var RCPath: UInt32); override;
    procedure DecReferences(var RCPath: UInt32); override;
    procedure ProcessVarInit(Proc: TIDProcedure); override;
    procedure EnumerateArgs(const EnumProc: TILArgsEnumProc; var BreakEnum: Boolean); override;
    function Text: string; override;
    property Left: TIDExpression read FLeft write FLeft;
    property Right: TIDExpression read FRight write FRight;
  end;

  TILRet = class(TILNoArgInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure CECalc(var Ctx: TILCECalcContext); override;
  end;

  TILNeg = class(TILDstSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure CECalc(var Ctx: TILCECalcContext); override;
  end;

  TILNot = class(TILDstSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure CECalc(var Ctx: TILCECalcContext); override;
  end;

  TILAnd = class(TILDstSrcSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure CECalc(var Ctx: TILCECalcContext); override;
  end;

  TILOr = class(TILDstSrcSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure CECalc(var Ctx: TILCECalcContext); override;
  end;

  TILXor = class(TILDstSrcSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure CECalc(var Ctx: TILCECalcContext); override;
  end;

  TILShl = class(TILDstSrcSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure CECalc(var Ctx: TILCECalcContext); override;
  end;

  TILShr = class(TILDstSrcSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure CECalc(var Ctx: TILCECalcContext); override;
  end;

  TILMove = class(TILDstSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure Write(Proc: TIDProcedure; Stream: TStream); override;
    procedure CECalc(var Ctx: TILCECalcContext); override;
  end;

  TILSetBool = class(TILDestInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILGetBit = class(TILDstSrcSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILSetBit = class(TILDstSrcSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILAdd = class(TILDstSrcSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure Write(Proc: TIDProcedure; Stream: TStream); override;
    procedure CECalc(var Ctx: TILCECalcContext); override;
  end;

  TILSub = class(TILDstSrcSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure Write(Proc: TIDProcedure; Stream: TStream); override;
    procedure CECalc(var Ctx: TILCECalcContext); override;
  end;

  TILMul = class(TILDstSrcSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure CECalc(var Ctx: TILCECalcContext); override;
  end;

  TILDiv = class(TILDstSrcSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure CECalc(var Ctx: TILCECalcContext); override;
  end;

  TILCast = class(TILDstSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILIntDiv = class(TILDstSrcSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure CECalc(var Ctx: TILCECalcContext); override;
  end;

  TILMod = class(TILDstSrcSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure CECalc(var Ctx: TILCECalcContext); override;
  end;

  TILCmp = class(TILCompareInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure CECalc(var Ctx: TILCECalcContext); override;
  end;

  TILTest = class(TILCompareInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILConvert = class(TILDstSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILCheckBound = class(TILCompareInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILUniqueInstruction = class(TILDestInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILLoadAddr = class(TILDstSrcSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure Write(Proc: TIDProcedure; Stream: TStream); override;
  end;

  TIL1ConstArgInstruction = class(TILNoArgInstruction)
  protected
    FArgument: Integer;
    function ArgumentsCount: Integer; override;
  public
    function Text: string; override;
    property Argument: Integer read FArgument write FArgument;
    procedure Write(Proc: TIDProcedure; Stream: TStream); override;
  end;

  {инструкции перехода/прыжка (аргумет - инструкция на которую необходимо совершить переход)}
  TILJampedInstruction = class(TILNoArgInstruction)
  private
    FDestination: TILInstruction;
  protected
    function GetDestinationPosition: Integer; virtual;
  public
    procedure Write(Proc: TIDProcedure; Stream: TStream); override;
    procedure CECalc(var Ctx: TILCECalcContext); override;
    property Destination: TILInstruction read FDestination write FDestination;
    property DestinationPosition: Integer read GetDestinationPosition;
    function Text: string; override;
  end;

  {Прыжок на инструкциию с адресом Destination}
  TILJmp = class(TILJampedInstruction)
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  {Прыжок на инструкциию идущую после Destination}
  TILJmpNext = class(TILJmp)
  protected
    function GetDestinationPosition: Integer; override;
    procedure CECalc(var Ctx: TILCECalcContext); override;
  end;

  TILDestMultiArgsInstruction = class(TILDestInstruction)
  private
    FArgs: TIDExpressions;
    function GetArgsNames: string;
  public
    procedure Init(Condition: TILCondition; Dest: TIDExpression; const Args: TIDExpressions); overload; inline;
    procedure IncReferences(var RCPath: UInt32); override;
    procedure DecReferences(var RCPath: UInt32); override;
    procedure SwapArguments(const Context: TPIContext); override;
    property Arguments: TIDExpressions read FArgs;
  end;

  TILProcCall = class(TILDestMultiArgsInstruction)
  private
    FProc: TIDExpression;
    FInstance: TIDExpression;
  protected
    function ArgumentsCount: Integer; override;
    function GetArgument(Index: Integer): TIDExpression; override;
    function GetLine: Integer; override;
  public
    procedure Init(Condition: TILCondition; Proc, Result, Instance: TIDExpression; const Args: TIDExpressions); overload; inline;
    procedure Write(Proc: TIDProcedure; Stream: TStream); override;
    procedure IncReferences(var RCPath: UInt32); override;
    procedure DecReferences(var RCPath: UInt32); override;
    procedure ProcessVarInit(Proc: TIDProcedure); override;
    procedure SwapArguments(const Context: TPIContext); override;
    procedure EnumerateArgs(const EnumProc: TILArgsEnumProc; var BreakEnum: Boolean); override;
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure CECalc(var Ctx: TILCECalcContext); override;
    property Proc: TIDExpression read FProc;
    property Instance: TIDExpression read FInstance;
  end;

  TILProcCallUnSafe = class(TILProcCall)
  public
    function ILCode: TILCode; override;
    procedure Write(Proc: TIDProcedure; Stream: TStream); override;
  end;

  TILInheritedCall = class(TILProcCall)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure Write(Proc: TIDProcedure; Stream: TStream); override;
  end;

  TILVirtCall = class(TILProcCall)
  public
    function ILCode: TILCode; override;
  end;

  TILGetPtr = class(TILDestInstruction)
  private
    FBase: TIDExpression;                // Base может быть nil (для GetSelfPtr)
    FMember: TIDExpression;
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure SwapArguments(const Context: TPIContext); override;
    procedure EnumerateArgs(const EnumProc: TILArgsEnumProc; var BreakEnum: Boolean); override;
    property Base: TIDExpression read FBase;
    property Member: TIDExpression read FMember;
    procedure Write(Proc: TIDProcedure; Stream: TStream); override;
  end;

  TILGetPtrMulti = class(TILDestMultiArgsInstruction)
  protected
    function ArgumentsCount: Integer; override;
    function GetArgument(Index: Integer): TIDExpression; override;
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure Write(Proc: TIDProcedure; Stream: TStream); override;
    procedure EnumerateArgs(const EnumProc: TILArgsEnumProc; var BreakEnum: Boolean); override;
  end;

  TILDNewObj = class(TILDestInstruction)
  private
    FInstance: TIDExpression;
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure Write(Proc: TIDProcedure; Stream: TStream); override;
    procedure EnumerateArgs(const EnumProc: TILArgsEnumProc; var BreakEnum: Boolean); override;
    procedure IncReferences(var RCPath: UInt32); override;
    procedure DecReferences(var RCPath: UInt32); override;
  end;

  TILLDMethod = class(TILGetPtr)
  public
    function ILCode: TILCode; override;
  end;

  TILTypeInfo = class(TILDstSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILQueryType = class(TILDstSrcSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILSNewObj = class(TILProcCall)
  public
    function ILCode: TILCode; override;
  end;

  TILMemGet = class(TILDestInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILMemFree = class(TILDestInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILArrayDAlloc = class(TILDstSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILArrayRAlloc = class(TILDstSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILArraySAlloc = class(TILDstSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILArrayLength = class(TILDstSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILArrayCopy = class(TILDstSrcInstruction)
  private
    FFrom: TIDExpression;
    FCount: TIDExpression;
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure Write(Proc: TIDProcedure; Stream: TStream); override;
    procedure IncReferences(var RCPath: UInt32); override;
    procedure DecReferences(var RCPath: UInt32); override;
  end;

  TILMemMove = class(TILInstruction)
  private
    FSrcArr: TIDExpression;
    FSrcIdx: TIDExpression;
    FDstArr: TIDExpression;
    FDstIdx: TIDExpression;
    FCnt: TIDExpression;
  protected
    function GetLine: Integer; override;
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure Write(Proc: TIDProcedure; Stream: TStream); override;
  end;

  TILMemSet = class(TILDstSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILNearCall = class(TILJampedInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILTryBegin = class(TILJampedInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILTryEnd = class(TIL1ConstArgInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  {инструкция выброса исключения}
  TILEThrow = class(TIL1ArgInstriction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILTryCallHandler = class(TILInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILIncRef = class(TILDestInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILDecRef = class(TILDestInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
    procedure Write(Proc: TIDProcedure; Stream: TStream); override;
    procedure IncReferences(var RCPath: UInt32); override;
    procedure DecReferences(var RCPath: UInt32); override;
  end;

  TILWeakRef = class(TILDstSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILStrongRef = class(TILDstSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILInit = class(TILDestInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  {TILFinal = class(TILDestInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;}

  TILRefCount = class(TILDstSrcInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILNow = class(TILDestInstruction)
  public
    function ILCode: TILCode; override;
    function Text: string; override;
  end;

  TILFMacro = class(TILDestMultiArgsInstruction)
  private
    FMacroID: TILMacroID;
  public
    procedure SwapArguments(const Context: TPIContext); override;
    procedure Write(Proc: TIDProcedure; Stream: TStream); override;
    function ILCode: TILCode; override;
    function Text: string; override;
    property MacroID: TILMacroID read FMacroID;
  end;

  TIL = class
  private
    FProc: TIDProcedure;
    FCount: Integer;   // Позиция следующей инструкции
    FFirst: TILInstruction;
    FLast: TILInstruction;
    FCFBRoot: TCFBlock;
    FCFBCurrent: TCFBlock;
  protected
    procedure OptimizeIL;
  public
    constructor Create(Proc: TIDProcedure);
    destructor Destroy; override;
    ////////////////////////////////////////////////////////////////////////////////
    property First: TILInstruction read FFirst;
    property Last: TILInstruction read FLast;
    //==============================================================================
    class function IL_Nope: TILNope; inline;
    class function IL_Move(Dst, Src: TIDExpression): TILMove; overload; static; inline;
    class function IL_Move(Cond: TILCondition; Dst, Src: TIDExpression): TILMove; overload; static; inline;
    class function IL_SetBool(Cond: TILCondition; Dst: TIDExpression): TILSetBool; static; inline;
    class function IL_GetBit(Dst, Value, BitIndex: TIDExpression): TILGetBit; static; inline;
    class function IL_SetBit(Dst, BitIndex, BitValue: TIDExpression): TILSetBit; static; inline;
    class function IL_Add(Dst, Left, Right: TIDExpression): TILAdd; static; inline;
    class function IL_Sub(Dst, Left, Right: TIDExpression): TILSub; overload; static; inline;
    class function IL_Sub(Cond: TILCondition; Dst, Left, Right: TIDExpression): TILSub; overload; static; inline;
    class function IL_Mul(Dst, Left, Right: TIDExpression): TILMul; static; inline;
    class function IL_Div(Dst, Left, Right: TIDExpression): TILDiv; static; inline;
    class function IL_IntDiv(Dst, Left, Right: TIDExpression): TILIntDiv; static; inline;
    class function IL_ModDiv(Dst, Left, Right: TIDExpression): TILMod; static; inline;
    class function IL_Neg(Dst, Src: TIDExpression): TILNeg; static; inline;
    class function IL_Not(Dst, Src: TIDExpression): TILNot; static; inline;
    class function IL_And(Dst, Left, Right: TIDExpression): TILAnd; static; inline;
    class function IL_Or(Dst, Left, Right: TIDExpression): TILOr; static; inline;
    class function IL_Xor(Dst, Left, Right: TIDExpression): TILXor; static; inline;
    class function IL_Shl(Dst, Left, Right: TIDExpression): TILShl; static; inline;
    class function IL_Shr(Dst, Left, Right: TIDExpression): TILShr; static; inline;
    class function IL_Cmp(Left, Right: TIDExpression): TILCmp; static; inline;
    class function IL_Test(Left, Right: TIDExpression): TILTest; static; inline;
    class function IL_Convert(Dst, Src: TIDExpression): TILConvert; static; inline;
    class function IL_CheckBound(Src, Index: TIDExpression): TILCheckBound; static; inline;
    class function IL_Jmp(Line: Integer; Condition: TILCondition; Destination: TILInstruction): TILJmp; static; inline;
    class function IL_JmpNext(Line: Integer; Condition: TILCondition; Destination: TILInstruction): TILJmpNext; static; inline;
    class function IL_CmpJmp(Cond: TILCondition; Left, Right: TIDExpression; Destination: TILInstruction): TILCmpJmp; static; inline;
    class function IL_Cast(Dst, Src: TIDExpression): TILCast; static; inline;
    class function IL_Ret(Line: Integer; Condition: TILCondition = cNone): TILRet; static; inline;
    class function IL_Unique(Dst: TIDExpression): TILUniqueInstruction; static; inline;
    class function IL_ProcCall(Proc, Dst, Instance: TIDExpression; const Args: TIDExpressions): TILProcCall; static; inline;
    class function IL_ProcCallUnSafe(Proc, Dst, Instance: TIDExpression; const Args: TIDExpressions): TILProcCallUnSafe; static; inline;
    class function IL_VirtCall(Proc, Dst, Instance: TIDExpression; const Args: TIDExpressions): TILVirtCall; static; inline;
    class function IL_InheritedCall(Proc, Dst, Instance: TIDExpression; const Args: TIDExpressions): TILInheritedCall; static; inline;
    class function IL_GetPtr(Dst: TIDExpression; const Args: TIDExpressions): TILGetPtrMulti; overload; static; inline;
    class function IL_GetPtr(Dst: TIDExpression; const Base, Member: TIDExpression): TILGetPtr; overload; static; inline;
    class function IL_ReadDRef(Dst, Src: TIDExpression): TILReadDRef; overload; static; inline;
    class function IL_WriteDRef(Dst, Src: TIDExpression): TILWriteDRef; overload; static; inline;
    class function IL_LoadAddress(const Dst, Src: TIDExpression; Offset: TIDExpression = nil): TILLoadAddr; static; inline;
    class function IL_DAlloc(const Dst, Src: TIDExpression): TILArrayDAlloc; static; inline;
    class function IL_RAlloc(const Dst, Src: TIDExpression): TILArrayRAlloc; static; inline;
    class function IL_SAlloc(const Dst, Src: TIDExpression): TILArraySAlloc; static; inline;
    class function IL_Length(const Dst, Src: TIDExpression): TILArrayLength; static; inline;
    class function IL_Copy(const Dst, Src, From, Count: TIDExpression): TILArrayCopy; static; inline;
    class function IL_MoveArray(const SrcArr, SrcIdx, DstArr, DstIdx, Count: TIDExpression): TILMemMove; static; inline;
    class function IL_MemSet(const Dst, SetByte: TIDExpression): TILMemSet; static; inline;
    class function IL_DNew(Dst, Instance: TIDExpression): TILDNewObj; static; inline;
    class function IL_New(const Dst: TIDExpression): TILMemGet; static; inline;
    class function IL_FreeInstance(const Dst: TIDExpression): TILMemFree; static; inline;
    class function IL_TypeInfo(const Dst, Src: TIDExpression): TILTypeInfo; static; inline;
    class function IL_QueryType(const Dst, Src, DstType: TIDExpression): TILQueryType; static; inline;
    class function IL_NearCall(Dest: TILInstruction): TILNearCall; static; inline;
    class function IL_TryBegin(Dest: TILInstruction; Line: Integer): TILTryBegin; static; inline;
    class function IL_TryEnd(Line: Integer): TILTryEnd; static; inline;
    class function IL_TryCallHandler: TILTryCallHandler; static; inline;
    class function IL_EThrow(Condition: TILCondition; ExceptExpr: TIDExpression): TILEThrow; static; inline;
    class function IL_Init(Dst: TIDExpression): TILInit; static; inline;
    class function IL_IncRef(Dst: TIDExpression): TILIncRef; static; inline;
    class function IL_DecRef(Dst: TIDExpression): TILDecRef; static; inline;
    class function IL_WeakRef(const Dst, Src: TIDExpression): TILWeakRef; static; inline;
    class function IL_StrongRef(const Dst, Src: TIDExpression): TILStrongRef; static; inline;
    class function IL_LDMethod(const Dst, Base, Member: TIDExpression): TILLDMethod; static; inline;
    class function IL_RefCount(const Dst, Src: TIDExpression): TILRefCount; static; inline;
    class function IL_Now(const Dst: TIDExpression): TILNow; static; inline;
    class function IL_Macro(const Dst: TIDExpression; MacroID: TILMacroID; const Args: TIDExpressions): TILFMacro; overload; static; inline;
    //==============================================================================
    procedure InlineProc(DestinationProc, InlineProc: TIDProcedure; NextInstruction: TILInstruction;
                         Struct, FuncResult: TIDExpression; const Arguments: TIDExpressions);
    function GetAsText(ShowCFB, ShowLineNum: Boolean): string;
    {процедура завершения записи}
    procedure Complete(var RCPath: UInt32);
    procedure RemoveReferences(var RCPath: UInt32);
    procedure CheckVarsInitialized;
    procedure Write(const Instruction: TILInstruction); inline;
    procedure InsertAfter(Instruction, NewInstruction: TILInstruction);
    procedure InsertFirst(Instruction: TILInstruction);
    procedure InsertBefore(Instruction, NewInstruction: TILInstruction);
    procedure Replace(const Prev, New: TILInstruction); overload; inline;
    {процедура заменяет цель перехода для инструкций перехода}
    procedure ReplaceJmpTarget(const OldTarget, NewTarget: TILInstruction);
    procedure SaveToStream(Stream: TStream; const Package: INPPackage);
    procedure ChecRefToInstruction(Instruction: TILInstruction);
    procedure Delete(FromInstruction, ToInstruction: TILInstruction); overload;
    procedure Delete(Instruction: TILInstruction); overload;
    procedure CFBBegin(const CFBlockType: TCFBlockType);
    procedure CFBEnd(const CFBlockType: TCFBlockType);
    procedure AddVariable(const Variable: TIDVariable);
    procedure EnumerateArgs(const EnumProc: TILArgsEnumProc);
    property Count: Integer read FCount;
    property Proc: TIDProcedure read FProc;
    procedure CopyFrom(const IL: TIL);
    function GetWriteCount(Expr: TIDExpression): Integer;
    procedure CECalc;
  end;


implementation

uses SystemUnit, OPCompiler, NPCompiler.Operators, NPCompiler.ConstCalculator;

{функция определят "нулевая" ли константа}
function ZeroConstant(Decl: TIDConstant): Boolean;
begin
  Result := ((Decl.ClassType = TIDIntConstant) and (TIDIntConstant(Decl).Value = 0)) or
            ((Decl.ClassType = TIDCharConstant) and (TIDCharConstant(Decl).Value = #0)) or
            ((Decl.ClassType = TIDFloatConstant) and (TIDFloatConstant(Decl).Value = 0)) or
            ((Decl.ClassType = TIDStringConstant) and (TIDStringConstant(Decl).Value = '')) or
            ((Decl.ClassType = TIDDynArrayConstant) and (Length(TIDDynArrayConstant(Decl).Value) = 0));
end;

procedure WriteConstArgument(Stream: TStream; Value: Integer; Next: Boolean = False);
var
  Prefix: UInt8;
  ByteSize: Integer;
  Edt: TDataTypeID;
begin
  Edt := GetValueDataType(Value);
  ByteSize := GetValueByteSize(Value);
  Prefix := UInt8(ARG_IMMCONST) + UInt8(SYSUnit.DataTypes[Edt].Index);
  if Next then Prefix := Prefix + UInt8(ARG_NEXT);
  Stream.WriteUInt8(Prefix);
  Stream.Write(Value, ByteSize);
end;

procedure WriteArgument(Stream: TStream; Declaration: TIDDeclaration; UnitID: Integer = -1; Next: Boolean = False);
var
  Data: Int64;
  Prefix: UInt8;
  ByteSize: Integer;
  EDataType: TDataTypeID;
begin
  if Declaration.ItemType = itConst then
  begin
    {аргумент - табличная константа}
    if (Declaration.ClassType = TIDStringConstant) then
    begin
      Data := Declaration.Index;
      if Data = 0 then
        Data := Declaration.Package.GetStringConstant(TIDStringConstant(Declaration).Value);
      Prefix := (UInt8(TILARG_CLASS.ARG_SCONST) shl 2);
    end else
    if Declaration.ClassType = TIDDynArrayConstant then begin
      if ZeroConstant(TIDConstant(Declaration)) then
      begin
        Data := 0;
        Prefix := UInt8(ARG_IMMCONST) or UInt8(dtInt8);
      end else begin
        Data := Declaration.Index;
        Prefix := (UInt8(TILARG_CLASS.ARG_TCONST) shl 2);
      end;
    end else
    if Declaration.ClassType = TIDSizeofConstant then
    begin
      Data := TIDSizeofConstant(Declaration).AsInt64;
      Prefix := (UInt8(TILARG_CLASS.ARG_SIZEOF) shl 2);
    end else
    if (Declaration.ItemType = itConst) and
       (Declaration.DataType.DataTypeID in [dtRecord, dtStaticArray, dtGuid]) then
    begin
      Data := Declaration.Index;
      Prefix := (UInt8(TILARG_CLASS.ARG_TCONST) shl 2);
    end else
    {аргумент - непосредственная константа}
    begin
      {для явно указанного типа пишем константу как есть}
      if Assigned(TIDConstant(Declaration).ExplicitDataType) then
      begin
        ByteSize := TIDConstant(Declaration).ExplicitDataType.DataSize;
        EDataType := TIDConstant(Declaration).ExplicitDataType.DataTypeID;
      end else begin
        {если тип явно не указан, выбираем подходящий}
        ByteSize := TIDConstant(Declaration).ValueByteSize;
        EDataType := TIDConstant(Declaration).ValueDataType;
      end;

      Data := TIDConstant(Declaration).AsInt64;

      Prefix := UInt8(ARG_IMMCONST) or UInt8(EDataType);
      if Next then Prefix := Prefix or UInt8(ARG_NEXT);
      Stream.WriteUInt8(Prefix);
      Stream.Write(Data, ByteSize);
      Exit;
    end;
  end else
  begin
    {аргумент - переменная, процедура, тип}
    case Declaration.ItemType of
      itVar: Prefix := UInt8(TILARG_CLASS.ARG_VAR) shl 2;
      itProcedure: if Assigned(TIDProcedure(Declaration).Struct) then
         Prefix := UInt8(TILARG_CLASS.ARG_METHOD) shl 2
       else
         Prefix := UInt8(TILARG_CLASS.ARG_PROC) shl 2;
      itType: Prefix := UInt8(TILARG_CLASS.ARG_TYPE) shl 2;
    else
      Prefix := 0;
    end;
    case Declaration.Scope.ScopeType of
      stLocal: Prefix := Prefix or UInt8(ARG_SCOPE_LOCAL);
      stGlobal: Prefix := Prefix or UInt8(ARG_SCOPE_GLOBAL);
      stStruct: Prefix := Prefix or UInt8(ARG_SCOPE_STRUCT);
    end;
    Data := Declaration.SpaceIndex;
  end;

  if Next then
    Prefix := Prefix + ARG_NEXT;

  if (UnitID <> -1) and (Declaration.Scope.ScopeType <> stStruct) then
     Prefix := Prefix or UInt8(ARG_SCOPE_UNIT);

  Stream.WriteUInt8(Prefix);

  if (UnitID <> -1) and (Declaration.Scope.ScopeType <> stStruct) then
    Stream.WriteStretchUInt(UnitID);

  Stream.WriteStretchUInt(Data);
end;

function GetUnitID(Proc: TIDProcedure; Decl: TIDDeclaration): Integer;
var
  DeclUnit: TNPUnit;
begin
  Result := -1;
  // если Scope не определен, значит это временный обьект из текущего модуля
  if Assigned(Decl.Scope) then
  begin
    DeclUnit := TNPUnit(Decl.DeclUnit);
    if Assigned(DeclUnit) and (DeclUnit <> Proc.DeclUnit) then
      Result := DeclUnit.UnitID;
  end;
end;

procedure WriteILArgument(Proc: TIDProcedure; Stream: TStream; Declaration: TIDDeclaration); overload;
var
  UnitID: Integer;
begin
  UnitID := GetUnitID(Proc, Declaration);
  WriteArgument(Stream, Declaration, UnitID, False);
end;

{записует список аргументов как цепочку (без кол-ва)}
procedure WriteILArgumentsChain(Proc: TIDProcedure; Stream: TStream; const Args: TIDExpressions); overload;
var
  i, c: Integer;
  BDT: TDataTypeID;
  Arg: TIDExpression;
  UnitID: Integer;
begin
  c := Length(Args) - 1;
  // вычисляем бызовый тип
  BDT := Args[0].DataType.DataTypeID;
  if BDT = dtPointer then
    BDT := TIDPointer(Args[0].DataType).ReferenceType.DataTypeID;
  for i := 0 to c do
  begin
    Arg := Args[i];
    if (Arg.ItemType = itVar) and (BDT = dtRecord) and (i > 0) then
      WriteConstArgument(Stream, Arg.Declaration.SpaceIndex, i < c)
    else begin
      UnitID := GetUnitID(Proc, Arg.Declaration);
      WriteArgument(Stream, Arg.Declaration, UnitID, i < c);
    end;
    // вычисляем новый бызовый тип
    BDT := Arg.DataTypeID;
    if BDT = dtPointer then
      if Assigned(TIDPointer(Arg.DataType).ReferenceType) then
        BDT := TIDPointer(Arg.DataType).ReferenceType.DataTypeID;
  end;
end;

procedure WriteILArgumentsChain(Proc: TIDProcedure; Stream: TStream; const Base, Member: TIDExpression); overload;
var
  UnitID: Integer;
begin
  UnitID := GetUnitID(Proc, Base.Declaration);
  WriteArgument(Stream, Base.Declaration, UnitID, True);
  if (Member.ItemType = itVar) and (Base.DataTypeID in [dtRecord, dtClass]) then
  begin
    WriteConstArgument(Stream, Member.Declaration.SpaceIndex, False)
  end else
    WriteArgument(Stream, Member.Declaration, UnitID, False);
end;

procedure WriteILArgument(Proc: TIDProcedure; Stream: TStream; Expression: TIDExpression; Next: Boolean = False); overload;
var
  UnitID: Integer;
begin
  if Expression.ExpressionType = etDeclaration then
  begin
    UnitID := GetUnitID(Proc, Expression.Declaration);
    WriteArgument(Stream, Expression.Declaration, UnitID, Next)
  end else
    WriteILArgumentsChain(Proc, Stream, TIDMultiExpression(Expression).Items);
end;

procedure WriteILArguments(Proc: TIDProcedure; Stream: TStream; const Args: TIDExpressions; WriteElCountFirst: Boolean); overload; inline;
var
  i, c: Integer;
begin
  c := Length(Args);
  if WriteElCountFirst then Stream.WriteStretchUInt(c);
  for i := 0 to c - 1 do
    WriteILArgument(Proc, Stream, Args[i]);
end;

function ConditionToStr(Condition: TILCondition): string;
begin
  case Condition of
    cEqual: Result := '[=] ';
    cNotEqual: Result := '[<>] ';
    cGreater: Result := '[>] ';
    cGreaterOrEqual: Result := '[>=] ';
    cLess: Result := '[<] ';
    cLessOrEqual: Result := '[<=] ';
    cZero: Result := '[=0] ';
    cNonZero: Result := '[<>0] ';
  else
    Result := '';
  end;
end;

{ TIL }

constructor TIL.Create(Proc: TIDProcedure);
var
  V: TIDVariable;
begin
  FProc := Proc;
  FCFBRoot := TCFBlock.Create(nil);
  FCFBRoot.FType := CFB_PROC;
  FCFBCurrent := FCFBRoot;

  {всем входным параметрам проставляем инициализацию}
  V := Proc.VarSpace.First;
  while Assigned(V) do begin
    if VarParameter in V.Flags then
      FCFBRoot.AddVariable(V);
    V := TIDVariable(V.NextItem);
  end;
end;

destructor TIL.Destroy;
{var
  Block, DBlock: TCFBlock;}
begin
  {Block := FCFBCurrent;
  while Assigned(Block) do
  begin
    DBlock := Block;
    Block := Block.FirstChild;
    DBlock.Free;
  end;}
  inherited;
end;

procedure TIL.EnumerateArgs(const EnumProc: TILArgsEnumProc);
var
  Code: TILInstruction;
  BreakEnum: Boolean;
begin
  Code := FFirst;
  BreakEnum := False;
  while Assigned(Code) do
  begin
    Code.EnumerateArgs(EnumProc, BreakEnum);
    if BreakEnum then
      Exit;
    Code := Code.Next;
  end;
end;

procedure TIL.Delete(FromInstruction, ToInstruction: TILInstruction);
var
  Instruction: TILInstruction;
begin
  while True do begin
    Instruction := FromInstruction;
    FromInstruction := FromInstruction.Next;
    Delete(Instruction);
    if Instruction = ToInstruction then
      Exit;
  end;
end;

procedure TIL.Delete(Instruction: TILInstruction);
var
  Prev, Next: TILInstruction;
begin
  ChecRefToInstruction(Instruction);
  Prev := Instruction.Prev;
  Next := Instruction.Next;
  if Assigned(Prev) then
    Prev.FNext := Next;
  if Assigned(Next) then
    Next.FPrev := Prev;
  if Instruction = FFirst then
    FFirst := Next;
  if Instruction = FLast then
    FLast := Prev;
  Dec(FCount);
end;

function TIL.GetAsText(ShowCFB, ShowLineNum: Boolean): string;
var
  c: Integer;
  f: string;
  Instruction: TILInstruction;
begin
  c := Length(IntToStr(FCount));
  f := DupeString('0', c);

  Result := '';
  Instruction := FFirst;
  while Assigned(Instruction) do begin
    Result := AddStringSegment(Result, FormatFloat(f, Instruction.Position) + ': ' + Instruction.Text, #10);
    if ShowCFB then
      if Assigned(Instruction.FCFBlock) then
        Result := Result + '    // ' + GetEnumName(TypeInfo(TCFBlockType), Ord(Instruction.FCFBlock.FType))
      else
        Result := Result + '    // NULL';

    if ShowLineNum then
      Result := Result + '  //Line: ' + IntToStr(Instruction.Line);

    Instruction := Instruction.Next;
  end;
end;

function TIL.GetWriteCount(Expr: TIDExpression): Integer;
var
  Code: TILInstruction;
begin
  Result := 0;
  Code := FLast;
  while Assigned(Code) do
  begin
    if Code is TILDestInstruction then
    begin
      if TILDestInstruction(Code).Destination = Expr then
        Inc(Result);
    end;
    Code := Code.Prev;
  end;
end;

procedure TIL.InlineProc(DestinationProc, InlineProc: TIDProcedure; NextInstruction: TILInstruction;
                         Struct, FuncResult: TIDExpression; const Arguments: TIDExpressions);
type
  TInlineCodeInfo = record
    Src: TILInstruction;
    Dst: TILInstruction;
  end;
var
  ILPos, i: Integer;
  item: TIDVariable;
  SourceIL: TIL;
  SInstruction, DInstruction: TILInstruction;
  InlineCodes: array of TInlineCodeInfo;
  //Expr: TIDExpression;
  Context: TPIContext;
begin
  Context.InlineProc := InlineProc;
  Context.EqualList := TIDPairList.Create;
  { Создаем спосок соответствий локальных/временных переменных и параметров встраиваемой функции
    и аргументов функции назначения}

  // порядок следования параметров в функции/методе/процедуре: [RESULT][SELF][PARAM1...]
  item := TIDVariable(InlineProc.VarSpace.First);

  // результат
  if Assigned(FuncResult) then begin
    Context.EqualList.InsertNode(item, FuncResult);
    item := TIDVariable(item.NextItem);
  end;

  // self-параметр
  if Assigned(Struct) then begin
    Context.MethodSelf := Struct;
    Context.EqualList.InsertNode(item, Struct);
    item := TIDVariable(item.NextItem);
  end;

  // параметры
  for i := 0 to Length(Arguments) - 1 do begin
    (*if (not item.Reference {and (item.WriteCount > 0)}) and ((i > 0) or  not assigned(InlineProc.Struct)) then begin
      // делаем копию при передаче аргумента по значению
      Expr := TIDExpression.Create(DestinationProc.GetTMPVar(item.DataType));
      Write(IL_Move(Expr, Arguments[i]));
      Context.EqualList.InsertNode(item, Expr);
    end else*)
      Context.EqualList.InsertNode(item, Arguments[i]);
    item := TIDVariable(item.NextItem);
  end;

  // локальные переменные
  while Assigned(item) do begin
    if item.ItemType = itVar then
      Context.EqualList.InsertNode(item, TIDExpression.Create(DestinationProc.GetTMPVar(item.DataType, item.Reference)));
    item := TIDVariable(item.NextItem);
  end;

  // временные переменные
  (*with InlineFunc.TempVars do
  for i := 0 to Count - 1 do begin
    item := TIDVariable(InlineFunc.TempVars.items[i]);
    i2 := DestinationFunc.GetTMPVar(item.DataType, item.Reference);
    Context.EqualList.InsertNode(item, TIDExpression.Create(i2));
  end;*)

  ILPos := 0;

  SourceIL := TIL(InlineProc.IL);
  SetLength(InlineCodes, SourceIL.Count);
  SInstruction := SourceIL.First;
  while Assigned(SInstruction) do
  begin
    SInstruction.Position := ILPos;
    if SInstruction.ILCode <> icRet then
    begin
      Context.Instruction := SInstruction;
      DInstruction := TILInstructionClass(SInstruction.ClassType).Create;
      DInstruction.SwapArguments(Context);
      if DInstruction.ILCode = icGetSelfPtr then
        TILGetPtr(DInstruction).FBase := Struct;
    end else begin
       // если встретился RET (не последний),- заменяем его на JMP на конец процедуры
      if Assigned(SInstruction.Next) then
        DInstruction := TILJmpNext.Create(SInstruction.Line)
      else
        DInstruction := TIL.IL_Nope;
    end;
    DInstruction.Condition := SInstruction.Condition;

    with InlineCodes[ILPos] do
    begin
      Src := SInstruction;
      Dst := DInstruction;
    end;
    Inc(ILPos);
    InsertAfter(NextInstruction, DInstruction);
    NextInstruction := DInstruction;
    SInstruction := SInstruction.Next;
  end;
  Context.EqualList.Free;
  // корректировка переходов
  for i := 0 to SourceIL.Count - 1 do begin
    SInstruction := InlineCodes[i].Src;
    if SInstruction.ILCode = icJmp then
    begin
      ILPos := TILJampedInstruction(SInstruction).Destination.Position;
      DInstruction := InlineCodes[ILPos].Dst;
      TILJampedInstruction(InlineCodes[i].Dst).Destination := DInstruction;
    end else
    // если это не последний RET - корректируем смещение
    if (SInstruction.ILCode = icRet) and (i < SourceIL.Count - 1) then
      TILJampedInstruction(InlineCodes[i].Dst).Destination := InlineCodes[SourceIL.Count - 1].Dst;
  end;
end;

procedure TIL.Write(const Instruction: TILInstruction);
begin
  if Assigned(FLast) then begin
    Instruction.FPrev := FLast;
    FLast.FNext := Instruction;
  end else
    FFirst := Instruction;
  FLast := Instruction;
  Instruction.FCFBlock := FCFBCurrent;
  {$IFDEF DEBUG}
  Instruction.Position := FCount;
  {$ENDIF}
  Inc(FCount);
end;

procedure TIL.InsertAfter(Instruction, NewInstruction: TILInstruction);
var
  Next: TILInstruction;
begin
  {если Instruction = nil вставляем в начало}
  if not Assigned(Instruction) then
  begin
    NewInstruction.FNext := FFirst;
    NewInstruction.FCFBlock := FCFBCurrent;
    if Assigned(FFirst) then
      FFirst.FPrev := NewInstruction
    else
      FLast := NewInstruction;
    FFirst := NewInstruction;
    Inc(FCount);
    Exit;
  end;

  Next := Instruction.Next;
  Instruction.FNext := NewInstruction;
  if Assigned(Next) then
    Next.FPrev := NewInstruction;
  NewInstruction.FNext := Next;
  NewInstruction.FPrev := Instruction;
  NewInstruction.FCFBlock := Instruction.FCFBlock;
  if Instruction = FLast then
    FLast := NewInstruction;
  Inc(FCount);
end;

procedure TIL.InsertBefore(Instruction, NewInstruction: TILInstruction);
var
  Prev: TILInstruction;
begin
  {если Instruction = nil вставляем в начало}
  if not Assigned(Instruction) then
  begin
    NewInstruction.FNext := FFirst;
    if Assigned(FFirst) then
      FFirst.FPrev := NewInstruction
    else
      FLast := NewInstruction;
    FFirst := NewInstruction;
    NewInstruction.FCFBlock := FCFBCurrent;
    Inc(FCount);
    Exit;
  end;
  Prev := Instruction.Prev;
  Instruction.FPrev := NewInstruction;
  if Assigned(Prev) then
    Prev.FNext := NewInstruction;
  NewInstruction.FPrev := Prev;
  NewInstruction.FNext := Instruction;
  NewInstruction.FCFBlock := Instruction.FCFBlock;
  if Instruction = FFirst then
    FFirst := NewInstruction;
  Inc(FCount);
end;

procedure TIL.InsertFirst(Instruction: TILInstruction);
begin
  if Assigned(FFirst) then
    FFirst.FPrev := Instruction
  else
    FLast := Instruction;

  Instruction.FPrev := nil;
  Instruction.FNext := FFirst;
  Instruction.FCFBlock := FCFBCurrent;
  FFirst := Instruction;
  {$IFDEF DEBUG}
  Instruction.Position := FCount;
  {$ENDIF}
  Inc(FCount);
end;

procedure TIL.SaveToStream(Stream: TStream; const Package: INPPackage);
var
  Instruction: TILInstruction;
begin
  {завершающая инструкция Return отсутствует (оптимизация), ее добавляет транслятор если нужно}
  Stream.WriteStretchUInt(FCount); // кол-во инструкций
  Instruction := FFirst;
  while Assigned(Instruction) do
  begin
    Instruction.Write(FProc, Stream);
    if Package.IncludeDebugInfo then
      Stream.WriteStretchUInt(Instruction.Line);
    // {$IFDEF DEBUG}Stream.WriteUInt32($10203040);{$ENDIF}
    Instruction := Instruction.Next;
  end;
end;

class function TIL.IL_Add(Dst, Left, Right: TIDExpression): TILAdd;
begin
  Result := TILAdd.Create;
  Result.Init(cNone, Dst, Left, Right);
end;

class function TIL.IL_And(Dst, Left, Right: TIDExpression): TILAnd;
begin
  Result := TILAnd.Create;
  Result.Init(cNone, Dst, Left, Right);
end;

class function TIL.IL_LDMethod(const Dst, Base, Member: TIDExpression): TILLDMethod;
begin
  Result := TILLDMethod.Create;
  Result.Init(cNone, Dst);
  Result.FBase := Base;
  Result.FMember := Member;
end;

class function TIL.IL_Length(const Dst, Src: TIDExpression): TILArrayLength;
begin
  Result := TILArrayLength.Create;
  Result.Init(cNone, Dst, Src);
end;

class function TIL.IL_Convert(Dst, Src: TIDExpression): TILConvert;
begin
  Result := TILConvert.Create;
  Result.Init(cNone, Dst, Src);
end;

class function TIL.IL_Copy(const Dst, Src, From, Count: TIDExpression): TILArrayCopy;
begin
  Result := TILArrayCopy.Create;
  Result.Init(cNone, Dst, Src);
  Result.FFrom := From;
  Result.FCount := Count;
end;

class function TIL.IL_MoveArray(const SrcArr, SrcIdx, DstArr, DstIdx, Count: TIDExpression): TILMemMove;
begin
  Result := TILMemMove.Create;
  Result.FSrcArr := SrcArr;
  Result.FSrcIDx := SrcIdx;
  Result.FDstArr := DstArr;
  Result.FDstIdx := DstIdx;
  Result.FCnt := Count;
end;

class function TIL.IL_RAlloc(const Dst, Src: TIDExpression): TILArrayRAlloc;
begin
  Result := TILArrayRAlloc.Create;
  Result.Init(cNone, Dst, Src);
end;

class function TIL.IL_Or(Dst, Left, Right: TIDExpression): TILOr;
begin
  Result := TILOr.Create;
  Result.Init(cNone, Dst, Left, Right);
end;

class function TIL.IL_Xor(Dst, Left, Right: TIDExpression): TILXor;
begin
  Result := TILXor.Create;
  Result.Init(cNone, Dst, Left, Right);
end;

procedure TIL.OptimizeIL;
var
  Cur, Next, New: TILInstruction;
begin
  Cur := FFirst;
  while Assigned(Cur) do begin
    Next := Cur.Next;
    if not Assigned(Next) then
      Exit;

    if (Cur.ILCode = icCmp) and (Next.ILCode = icJmp) and (Cur.Condition = cNone) then
    begin
      New := TIL.IL_CmpJmp(Next.Condition, TILCompareInstruction(Cur).Left, TILCompareInstruction(Cur).Right, TILJmp(Next).Destination);
      Delete(Next);
      Replace(Cur, New);
      Cur := New.Next
    end else
      Cur := Next;
  end;
end;

procedure TIL.AddVariable(const Variable: TIDVariable);
begin
  FCFBCurrent.AddVariable(Variable);
end;

procedure TIL.CECalc;
var
  Ctx: TILCECalcContext;
  Instr: TILInstruction;
  str: string;
begin
  Ctx.Next := nil;
  Ctx.Cond := False;
  Ctx.Return := False;
  Instr := FFirst;
  while Assigned(Instr) do
  begin
    str := Instr.Text;
    Instr.CECalc(Ctx);
    if Ctx.Return then
      Break;
    if Assigned(Ctx.Next) then
    begin
      Instr := Ctx.Next;
      Ctx.Next := nil;
    end else
      Instr := Instr.Next;
  end;
end;

procedure TIL.CFBBegin(const CFBlockType: TCFBlockType);
var
  Block, Parent: TCFBlock;
begin
  Parent := FCFBCurrent;
  case CFBlockType of
    CFB_IF: begin
      Block := TCFBIF.Create(Parent);
      Block.Prev := Parent.LastChild;
      Parent.LastChild := Block;
    end;
    CFB_IF_THEN: begin
      Assert(Parent.BlockType = CFB_IF);
      Assert(TCFBIF(Parent).FTrue = nil);
      Block := TCFBlock.Create(Parent);
      TCFBIF(Parent).FTrue := Block;
    end;
    CFB_IF_ELSE: begin
      Assert(Parent.BlockType = CFB_IF);
      Assert(TCFBIF(Parent).FElse = nil);
      Block := TCFBlock.Create(Parent);
      TCFBIF(Parent).FElse := Block;
    end;
    CFB_CASE: begin
      Block := TCFBCASE.Create(Parent);
      Block.Prev := Parent.LastChild;
      Parent.LastChild := Block;
    end;
    CFB_CASE_ENTRY: begin
      Assert(Parent.BlockType = CFB_CASE);
      Block := TCFBlock.Create(Parent);
      TCFBCASE(Parent).FLastItem := Block;
    end;
  else
    Block := TCFBlock.Create(Parent);
    Block.Prev := Parent.LastChild;
    Parent.LastChild := Block;
  end;

  Block.FType := CFBlockType;
  FCFBCurrent := Block;
end;

procedure TIL.CFBEnd(const CFBlockType: TCFBlockType);
begin
  Assert(Assigned(FCFBCurrent));
  Assert(FCFBCurrent.BlockType = CFBlockType);
  FCFBCurrent := FCFBCurrent.Parent;
end;

procedure TIL.CheckVarsInitialized;
var
  Instruction: TILInstruction;
begin
  Instruction := FFirst;
  while Assigned(Instruction) do
  begin
    Instruction.ProcessVarInit(FProc);
    Instruction := Instruction.Next;
  end;
end;

procedure TIL.ChecRefToInstruction(Instruction: TILInstruction);
var
  Curr: TILInstruction;
begin
  Curr := FFirst;
  while Assigned(Curr) do begin
    if Curr is TILJampedInstruction then
      if TILJampedInstruction(Curr).Destination = Instruction then
        TILJampedInstruction(Curr).Destination := Instruction.Next;
    Curr := Curr.Next;
  end;
end;

procedure TIL.Complete(var RCPath: UInt32);
var
  i, JmpPos: Integer;
  Instr, RetCode: TILInstruction;
begin
  // OptimizeIL; !!! пока не поддерживается

  i := 0;
  Instr := FFirst;

  while Assigned(Instr) do
  begin
    if Instr.ILCode <> icNope then
    begin
      Instr.Position := i;
      // проставляем зависимости
      Instr.IncReferences(RCPath);
      Inc(i);
    end else
      Delete(Instr); // удаляем инструкции-пустышки {}

    Instr := Instr.Next;
  end;

  Instr := FFirst;
  while Assigned(Instr) do
  begin
    // Замена переходов за пределы процедуры на RET
    if (Instr.ILCode = icJmp) then
    begin
      JmpPos := TILJampedInstruction(Instr).DestinationPosition;
      if (JmpPos >= i) or (JmpPos < 0) then
      begin
        RetCode := IL_Ret(Instr.Line, Instr.Condition);
        Replace(Instr, RetCode);
      end;
    end;
    Instr := Instr.Next;
  end;{}
end;

procedure TIL.CopyFrom(const IL: TIL);
var
  Inst: TILInstruction;
begin
  Inst := IL.First;
  while Assigned(Inst) do
  begin
    Write(Inst);
    Inst := Inst.Next;
  end;
end;

procedure TIL.RemoveReferences(var RCPath: UInt32);
var
  Instr: TILInstruction;
begin
  Instr := FFirst;
  while Assigned(Instr) do
  begin
    Instr.DecReferences(RCPath);
    Instr := Instr.Next;
  end;
end;

procedure TIL.Replace(const Prev, New: TILInstruction);
var
  Inst: TILInstruction;
begin
  Inst := Prev.Prev;
  if Assigned(Inst) then
    Inst.FNext := New
  else
    FFirst := New;

  New.FPrev := Inst;

  Inst := Prev.Next;
  if Assigned(Inst) then
    Inst.FPrev := New
  else
    FLast := New;

  New.FNext := Inst;
  New.Position := Prev.Position;
  New.FCFBlock := Prev.FCFBlock;

  ReplaceJmpTarget(Prev, New);
end;

procedure TIL.ReplaceJmpTarget(const OldTarget, NewTarget: TILInstruction);
var
  Inst: TILInstruction;
begin
  Inst := FFirst;
  while Assigned(Inst) do
  begin
    if (Inst.ILCode = icJmp) and (TILJmp(Inst).Destination = OldTarget) then
      TILJmp(Inst).Destination := NewTarget;
    Inst := Inst.Next;
  end;
end;

class function TIL.IL_Init(Dst: TIDExpression): TILInit;
begin
  Result := TILInit.Create;
  Result.Init(cNone, Dst);
end;

{class function TIL.IL_Final(Dst: TIDExpression): TILFinal;
begin
  Result := TILFinal.Create;
  Result.Init(cNone, Dst);
end;}

class function TIL.IL_Ret(Line: Integer; Condition: TILCondition = cNone): TILRet;
begin
  Result := TILRet.Create(Line);
  Result.Condition := Condition;
end;

class function TIL.IL_ProcCall(Proc, Dst, Instance: TIDExpression; const Args: TIDExpressions): TILProcCall;
begin
  Result := TILProcCall.Create;
  Result.Init(cNone, Proc, Dst, Instance, Args);
end;

class function TIL.IL_ProcCallUnSafe(Proc, Dst, Instance: TIDExpression; const Args: TIDExpressions): TILProcCallUnSafe;
begin
  Result := TILProcCallUnSafe.Create;
  Result.Init(cNone, Proc, Dst, Instance, Args);
end;

class function TIL.IL_QueryType(const Dst, Src, DstType: TIDExpression): TILQueryType;
begin
  Result := TILQueryType.Create;
  Result.Init(cNone, Dst, Src, DstType);
end;

class function TIL.IL_VirtCall(Proc, Dst, Instance: TIDExpression; const Args: TIDExpressions): TILVirtCall;
begin
  Result := TILVirtCall.Create;
  Result.Init(cNone, Proc, Dst, Instance, Args);
end;

class function TIL.IL_IncRef(Dst: TIDExpression): TILIncRef;
begin
  Result := TILIncRef.Create;
  Result.Init(cNone, Dst);
end;

class function TIL.IL_InheritedCall(Proc, Dst, Instance: TIDExpression; const Args: TIDExpressions): TILInheritedCall;
begin
  Result := TILInheritedCall.Create;
  Result.Init(cNone, Proc, Dst, Instance, Args);
end;

class function TIL.IL_Cast(Dst, Src: TIDExpression): TILCast;
begin
  Result := TILCast.Create;
  Result.Init(cNone, Dst, Src);
end;

class function TIL.IL_CheckBound(Src, Index: TIDExpression): TILCheckBound;
begin
  Result := TILCheckBound.Create;
  Result.Init(cNone, Src, Index);
end;

class function TIL.IL_Cmp(Left, Right: TIDExpression): TILCmp;
begin
  Result := TILCmp.Create;
  Result.Init(cNone, Left, Right);
end;

class function TIL.IL_CmpJmp(Cond: TILCondition; Left, Right: TIDExpression; Destination: TILInstruction): TILCmpJmp;
begin
  Result := TILCmpJmp.Create;
  Result.Init(Cond, Left, Right, Destination);
end;

class function TIL.IL_Div(Dst, Left, Right: TIDExpression): TILDiv;
begin
  Result := TILDiv.Create;
  Result.Init(cNone, Dst, Left, Right);
end;

class function TIL.IL_TryBegin(Dest: TILInstruction; Line: Integer): TILTryBegin;
begin
  Result := TILTryBegin.Create(0);
  Result.Destination := Dest;
  Result.Line := Line;
end;

class function TIL.IL_TryEnd(Line: Integer): TILTryEnd;
begin
  Result := TILTryEnd.Create(0);
  Result.Line := Line;
end;

class function TIL.IL_TypeInfo(const Dst, Src: TIDExpression): TILTypeInfo;
begin
  Result := TILTypeInfo.Create;
  Result.Init(cNone, Dst, Src);
end;

class function TIL.IL_TryCallHandler: TILTryCallHandler;
begin
  Result := TILTryCallHandler.Create;
end;

class function TIL.IL_GetPtr(Dst: TIDExpression; const Args: TIDExpressions): TILGetPtrMulti;
begin
  Result := TILGetPtrMulti.Create;
  Result.Init(cNone, Dst, Args);
end;

class function TIL.IL_GetPtr(Dst: TIDExpression; const Base, Member: TIDExpression): TILGetPtr;
begin
  Result := TILGetPtr.Create;
  Result.Init(cNone, Dst);
  Result.FBase := Base;
  Result.FMember := Member;
end;

class function TIL.IL_IntDiv(Dst, Left, Right: TIDExpression): TILIntDiv;
begin
  Result := TILIntDiv.Create;
  Result.Init(cNone, Dst, Left, Right);
end;

class function TIL.IL_Macro(const Dst: TIDExpression; MacroID: TILMacroID; const Args: TIDExpressions): TILFMacro;
begin
  Result := TILFMacro.Create;
  Result.Init(cNone, Dst, Args);
  Result.FMacroID := MacroID;
end;

class function TIL.IL_MemSet(const Dst, SetByte: TIDExpression): TILMemSet;
begin
  Result := TILMemSet.Create;
  Result.Init(cNone, Dst, SetByte);
end;

class function TIL.IL_ModDiv(Dst, Left, Right: TIDExpression): TILMod;
begin
  Result := TILMod.Create;
  Result.Init(cNone, Dst, Left, Right);
end;

class function TIL.IL_Jmp(Line: Integer; Condition: TILCondition; Destination: TILInstruction): TILJmp;
begin
  Result := TILJmp.Create(Line);
  Result.Condition := Condition;
  Result.Destination := Destination;
end;

class function TIL.IL_JmpNext(Line: Integer; Condition: TILCondition; Destination: TILInstruction): TILJmpNext;
begin
  Result := TILJmpNext.Create(Line);
  Result.Condition := Condition;
  Result.Destination := Destination;
end;

class function TIL.IL_Move(Dst, Src: TIDExpression): TILMove;
begin
  Result := TILMove.Create;
  Result.Init(cNone, Dst, Src);
end;

class function TIL.IL_Move(Cond: TILCondition; Dst, Src: TIDExpression): TILMove;
begin
  Result := TILMove.Create;
  Result.Init(Cond, Dst, Src);
end;

class function TIL.IL_LoadAddress(const Dst, Src: TIDExpression; Offset: TIDExpression = nil): TILLoadAddr;
begin
  Result := TILLoadAddr.Create;
  Result.Init(cNone, Dst, Src, Offset);
end;

class function TIL.IL_Mul(Dst, Left, Right: TIDExpression): TILMul;
begin
  Result := TILMul.Create;
  Result.Init(cNone, Dst, Left, Right);
end;

class function TIL.IL_NearCall(Dest: TILInstruction): TILNearCall;
begin
  Result := TILNearCall.Create(0);
  Result.Destination := Dest;
end;

class function TIL.IL_Neg(Dst, Src: TIDExpression): TILNeg;
begin
  Result := TILNeg.Create;
  Result.Init(cNone, Dst, Src);
end;

class function TIL.IL_New(const Dst: TIDExpression): TILMemGet;
begin
  Result := TILMemGet.Create;
  Result.Init(cNone, Dst);
end;

class function TIL.IL_DNew(Dst, Instance: TIDExpression): TILDNewObj;
begin
  Result := TILDNewObj.Create;
  Result.Init(cNone, Dst);
  Result.FInstance := Instance;
end;

class function TIL.IL_ReadDRef(Dst, Src: TIDExpression): TILReadDRef;
begin
  Result := TILReadDRef.Create;
  Result.Init(cNone, Dst, Src);
end;

class function TIL.IL_RefCount(const Dst, Src: TIDExpression): TILRefCount;
begin
  Result := TILRefCount.Create;
  Result.Init(cNone, Dst, Src);
end;

class function TIL.IL_WriteDRef(Dst, Src: TIDExpression): TILWriteDRef;
begin
  Result := TILWriteDRef.Create;
  Result.Init(cNone, Dst, Src);
end;

class function TIL.IL_FreeInstance(const Dst: TIDExpression): TILMemFree;
begin
  Result := TILMemFree.Create;
  Result.Init(cNone, Dst);
end;

class function TIL.IL_StrongRef(const Dst, Src: TIDExpression): TILStrongRef;
begin
  Result := TILStrongRef.Create;
  Result.Init(cNone, Dst, Src);
end;

class function TIL.IL_WeakRef(const Dst, Src: TIDExpression): TILWeakRef;
begin
  Result := TILWeakRef.Create;
  Result.Init(cNone, Dst, Src);
end;

class function TIL.IL_Nope: TILNope;
begin
  Result := TILNope.Create(0);
end;

class function TIL.IL_Not(Dst, Src: TIDExpression): TILNot;
begin
  Result := TILNot.Create;
  Result.Init(cNone, Dst, Src);
end;

class function TIL.IL_Now(const Dst: TIDExpression): TILNow;
begin
  Result := TILNow.Create;
  Result.Init(cNone, Dst);
end;

class function TIL.IL_SetBool(Cond: TILCondition; Dst: TIDExpression): TILSetBool;
begin
  Result := TILSetBool.Create;
  Result.Init(Cond, Dst);
end;

class function TIL.IL_GetBit(Dst, Value, BitIndex: TIDExpression): TILGetBit;
begin
  Result := TILGetBit.Create;
  Result.Init(cNone, Dst, Value, BitIndex);
end;

class function TIL.IL_SetBit(Dst, BitIndex, BitValue: TIDExpression): TILSetBit;
begin
  Result := TILSetBit.Create;
  Result.Init(cNone, Dst, BitIndex, BitValue);
end;

class function TIL.IL_Shl(Dst, Left, Right: TIDExpression): TILShl;
begin
  Result := TILShl.Create;
  Result.Init(cNone, Dst, Left, Right);
end;

class function TIL.IL_Shr(Dst, Left, Right: TIDExpression): TILShr;
begin
  Result := TILShr.Create;
  Result.Init(cNone, Dst, Left, Right);
end;

class function TIL.IL_DAlloc(const Dst, Src: TIDExpression): TILArrayDAlloc;
begin
  Result := TILArrayDAlloc.Create;
  Result.Init(cNone, Dst, Src);
end;

class function TIL.IL_DecRef(Dst: TIDExpression): TILDecRef;
begin
  Result := TILDecRef.Create;
  Result.Init(cNone, Dst);
end;

class function TIL.IL_SAlloc(const Dst, Src: TIDExpression): TILArraySAlloc;
begin
  Result := TILArraySAlloc.Create;
  Result.Init(cNone, Dst, Src);
end;

class function TIL.IL_Sub(Dst, Left, Right: TIDExpression): TILSub;
begin
  Result := TILSub.Create;
  Result.Init(cNone, Dst, Left, Right);
end;

class function TIL.IL_Sub(Cond: TILCondition; Dst, Left, Right: TIDExpression): TILSub;
begin
  Result := TILSub.Create;
  Result.Init(Cond, Dst, Left, Right);
end;

class function TIL.IL_Test(Left, Right: TIDExpression): TILTest;
begin
  Result := TILTest.Create;
  Result.Init(cNone, Left, Right);
end;

class function TIL.IL_EThrow(Condition: TILCondition; ExceptExpr: TIDExpression): TILEThrow;
begin
  Result := TILEThrow.Create;
  Result.Init(Condition, ExceptExpr);
end;

class function TIL.IL_Unique(Dst: TIDExpression): TILUniqueInstruction;
begin
  Result := TILUniqueInstruction.Create;
  Result.Init(cNone, Dst);
end;

{ TILInstruction }

function TILInstruction.ArgumentsCount: Integer;
begin
  Result := 0;
end;

procedure _ERROR_VAR_IS_NOT_INITIALIZED(Proc: TIDProcedure; const Arg: TIDExpression);
begin
  Proc.Warning('Variable "%s" is not initialized', [Arg.DisplayName], Arg.TextPosition);
end;

procedure TILInstruction.CheckArgInit(Proc: TIDProcedure; Expr: TIDExpression);
begin
  if Assigned(Expr) and Expr.IsLocalVar then
  begin
    if Expr.DataTypeID in [dtStaticArray, dtRecord] then
      Exit;
    if not FCFBlock.IsVarInitialized(Expr.AsVariable) then
      _ERROR_VAR_IS_NOT_INITIALIZED(Proc, Expr);
  end;
end;

constructor TILInstruction.Create;
begin
  CreateFromPool;
end;

procedure TILInstruction.EnumerateArgs(const EnumProc: TILArgsEnumProc; var BreakEnum: Boolean);
begin

end;

function TILInstruction.GetArgument(Index: Integer): TIDExpression;
begin
  Result := nil;
end;

function TILInstruction.GetLine: Integer;
begin
  Result := -1;
end;

function TILInstruction.ILCodeString(InUpperCase: Boolean): string;
begin
  Result := GetILCodeName(ILCode);
  if InUpperCase then
    Result := UpperCase(Result);
end;

procedure TILInstruction.IncReferences(var RCPath: UInt32);
begin

end;

procedure TILInstruction.ProcessVarInit(Proc: TIDProcedure);
begin

end;

function TILInstruction.CondAsText: string;
begin
  case FCondition of
    cEqual: Result := '[=] ';
    cNotEqual: Result := '[<>] ';
    cGreater: Result := '[>] ';
    cGreaterOrEqual: Result := '[>=] ';
    cLess: Result := '[<] ';
    cLessOrEqual: Result := '[<=] ';
    cZero: Result := '[=0] ';
    cNonZero: Result := '[<>0] ';
    else
      Result := '';
  end;
end;


function TILInstruction.Text: string;
begin
  Result := CondAsText;
end;

procedure TILInstruction.DecReferences(var RCPath: UInt32);
begin

end;

procedure TILInstruction.Read(Stream: TStream);
begin
  FCondition := TILCondition(Stream.ReadUInt8);
end;

procedure TILInstruction.RemoveBack(ToInstruction: TILInstruction);
begin

end;

{ TILDstSrcInstruction }

function TILDstSrcInstruction.ArgumentsCount: Integer;
begin
  Result := 2;
end;

procedure TILDstSrcInstruction.EnumerateArgs(const EnumProc: TILArgsEnumProc; var BreakEnum: Boolean);
begin
  EnumProc(FDestination, BreakEnum);
  if BreakEnum then
    Exit;
  EnumProc(FSource, BreakEnum);
end;

function TILDstSrcInstruction.GetArgument(Index: Integer): TIDExpression;
begin
  case Index of
    0: Result := FDestination;
    1: Result := FSource;
    else Result := nil;
  end;
end;

function TILDstSrcInstruction.GetLine: Integer;
begin
  Result := inherited GetLine;
  if Result = 0 then
    Result := FSource.Line;
end;

procedure TILDstSrcInstruction.Init(Condition: TILCondition; Destination, Source: TIDExpression);
begin
  Init(Condition, Destination);
  FSource := Source;
end;

procedure TILDstSrcInstruction.IncReferences(var RCPath: UInt32);
begin
  IncReadCount(FDestination, Self, RCPath); Inc(RCPath);
  IncReadCount(FSource, Self, RCPath); Inc(RCPath);
end;

procedure TILDstSrcInstruction.DecReferences(var RCPath: UInt32);
begin
  DecReadCount(FDestination, Self, RCPath); Inc(RCPath);
  DecReadCount(FSource, Self, RCPath); Inc(RCPath);
end;

procedure TILDstSrcInstruction.ProcessVarInit(Proc: TIDProcedure);
begin
  inherited;
  CheckArgInit(Proc, FSource);
end;

function TILDstSrcInstruction.Text: string;
begin
  Result := Format('%s, %s', [inherited Text,  ExpressionName(FSource)]);
end;

procedure TILInstruction.SwapArguments(const Context: TPIContext);
begin
  FCFBlock := Context.Instruction.FCFBlock;
end;

procedure WriteILCode(Stream: TStream; ILCode: TILCode; Condition: TILCondition); inline;
var
  CodeByte: UInt8;
begin
  CodeByte := UInt8(ILCode);
  if Condition <> cNone then
    CodeByte := CodeByte or 128;
  Stream.WriteUInt8(CodeByte);
  if Condition <> cNone then
    Stream.WriteUInt8(UInt8(Condition));
end;

procedure TILInstruction.Write(Proc: TIDProcedure; Stream: TStream);
begin
  WriteILCode(Stream, ILCode, FCondition);
end;

{ TILMove }

procedure TILMove.CECalc(var Ctx: TILCECalcContext);
begin
  FDestination.CValue := FSource.CValue;
end;

function TILMove.ILCode: TILCode;
var
  Src: TIDDeclaration;
begin
  Src := Source.Declaration;
  if (Source.ExpressionType = etDeclaration) and (Src.ItemType = itConst) and ZeroConstant(TIDConstant(Src)) then
    Result := icMoveZero
  else
    Result := icMove;
end;

function TILMove.Text: string;
begin
  if ILCode = icMove then
    Result := 'MOVE ' + inherited Text
  else
    Result := 'MOVEZERO ' + CondAsText + ExpressionName(FDestination);
end;

procedure TILMove.Write(Proc: TIDProcedure; Stream: TStream);
var
  MoveCode: TILCode;
begin
  MoveCode := ILCode;
  WriteILCode(Stream, MoveCode, FCondition);
  WriteILArgument(Proc, Stream, Destination);
  if MoveCode <> icMoveZero then
    WriteILArgument(Proc, Stream, FSource);
end;

{ TILDstSrcSrcInstruction }

function TILDstSrcSrcInstruction.ArgumentsCount: Integer;
begin
  Result := 3;
end;

procedure TILDstSrcSrcInstruction.ProcessVarInit(Proc: TIDProcedure);
begin
  inherited;
  CheckArgInit(Proc, Left);
  CheckArgInit(Proc, Right);
end;

procedure TILDstSrcSrcInstruction.EnumerateArgs(const EnumProc: TILArgsEnumProc; var BreakEnum: Boolean);
begin
  EnumProc(FDestination, BreakEnum);
  if BreakEnum then
    Exit;
  EnumProc(FLeft, BreakEnum);
  if BreakEnum then
    Exit;
  if Assigned(FRight) then
    EnumProc(FRight, BreakEnum);
end;

function TILDstSrcSrcInstruction.GetArgument(Index: Integer): TIDExpression;
begin
  case Index of
    0: Result := FDestination;
    1: Result := FLeft;
    2: Result := FRight;
    else Result := nil;
  end;
end;

procedure TILDstSrcSrcInstruction.Init(Condition: TILCondition; Dest, Left, Right: TIDExpression);
begin
  Init(Condition, Dest);
  FLeft := Left;
  FRight := Right;
end;

procedure TILDstSrcSrcInstruction.IncReferences(var RCPath: UInt32);
begin
  IncReadCount(FDestination, Self, RCPath); Inc(RCPath);
  IncReadCount(FLeft, Self, RCPath); Inc(RCPath);
  IncReadCount(FRight, Self, RCPath); Inc(RCPath);
end;

procedure TILDstSrcSrcInstruction.DecReferences(var RCPath: UInt32);
begin
  DecReadCount(FDestination, Self, RCPath); Inc(RCPath);
  DecReadCount(FLeft, Self, RCPath); Inc(RCPath);
  DecReadCount(FRight, Self, RCPath); Inc(RCPath);
end;

function TILDstSrcSrcInstruction.Text: string;
begin
  if Assigned(FRight) then
    Result := Format('%s, %s, %s', [inherited Text, ExpressionName(FLeft), ExpressionName(FRight)])
  else
    Result := Format('%s, %s', [inherited Text, ExpressionName(FLeft)])
end;

{ TILAdd }

procedure TILAdd.CECalc(var Ctx: TILCECalcContext);
begin
  FDestination.CValue := ProcessConstOperation(FLeft.CValue, FRight.CValue, opAdd);
end;

function TILAdd.ILCode: TILCode;
var
  Dest: TIDDeclaration;
begin
  Dest := Destination.Declaration;
  if ((Left.ExpressionType = etDeclaration) and (Left.ItemType = itConst) and (Dest = Right.Declaration) and (Left.Declaration = SYSUnit._OneConstant)) or
     ((Right.ExpressionType = etDeclaration) and (Right.ItemType = itConst) and (Dest = Left.Declaration) and (Right.Declaration = SYSUnit._OneConstant)) then
    Result := icInc
  else
  if ((Destination.ExpressionType = etDeclaration) and (Dest = Left.Declaration)) or // нужно дописать полную проверку для цепочек
     ((Destination.ExpressionType = etDeclaration) and (Dest = Right.Declaration)) then
    Result := icAdd2
  else
    Result := icAdd;
end;

function TILAdd.Text: string;
begin
  if ILCode = icInc then begin
    Result := 'INC ' + ConditionToStr(Condition) + ExpressionName(FDestination);
  end else
    Result := 'ADD ' + inherited Text;
end;

procedure TILAdd.Write(Proc: TIDProcedure; Stream: TStream);
var
  iCode: TILCode;
begin
  iCode := ILCode;
  WriteILCode(Stream, iCode, FCondition);
  WriteILArgument(Proc, Stream, Destination);
  if (iCode = icInc) then Exit;
  if (iCode = icAdd) or ((iCode = icAdd2) and (Right.Declaration = Destination.Declaration)) then
    WriteILArgument(Proc, Stream, Left)
  else
    WriteILArgument(Proc, Stream, Right);
  if iCode = icAdd then
    WriteILArgument(Proc, Stream, Right);
end;

{ TILIntDiv }

procedure TILIntDiv.CECalc(var Ctx: TILCECalcContext);
begin
  FDestination.CValue := ProcessConstOperation(FLeft.CValue, FRight.CValue, opIntDiv);
end;

function TILIntDiv.ILCode: TILCode;
begin
  if Destination.Declaration <> Left.Declaration then
    Result := icIntDiv
  else
    Result := icIntDiv2;
end;

function TILIntDiv.Text: string;
begin
  Result := 'DIV ' + inherited Text;
end;

{ TILDiv }

procedure TILDiv.CECalc(var Ctx: TILCECalcContext);
begin
  FDestination.CValue := ProcessConstOperation(FLeft.CValue, FRight.CValue, opDivide);
end;

function TILDiv.ILCode: TILCode;
begin
  if Destination.Declaration <> Left.Declaration then
    Result := icDiv
  else
    Result := icDiv2;
end;

function TILDiv.Text: string;
begin
  Result := 'DIV ' + inherited Text;
end;

{ TILMod }

procedure TILMod.CECalc(var Ctx: TILCECalcContext);
begin
  FDestination.CValue := ProcessConstOperation(FLeft.CValue, FRight.CValue, opModDiv);
end;

function TILMod.ILCode: TILCode;
begin
  if Destination.Declaration <> Left.Declaration then
    Result := icModDiv
  else
    Result := icModDiv2;
end;

function TILMod.Text: string;
begin
  Result := 'MOD ' + inherited Text;
end;

{ TILMul }

procedure TILMul.CECalc(var Ctx: TILCECalcContext);
begin
  FDestination.CValue := ProcessConstOperation(FLeft.CValue, FRight.CValue, opMultiply);
end;

function TILMul.ILCode: TILCode;
begin
  if Destination.Declaration <> Left.Declaration then
    Result := icMul
  else
    Result := icMul2;
end;

function TILMul.Text: string;
begin
  Result := 'MUL ' + inherited Text;
end;

{ TILSub }

procedure TILSub.CECalc(var Ctx: TILCECalcContext);
begin
  FDestination.CValue := ProcessConstOperation(FLeft.CValue, FRight.CValue, opSubtract);
end;

function TILSub.ILCode: TILCode;
var
  Decl: TIDDeclaration;
begin
  Decl := Destination.Declaration;
  if  (Right.ExpressionType = etDeclaration) and (Right.ItemType = itConst) and  (Decl = Left.Declaration) and (Right.Declaration = SYSUnit._OneConstant) then
    Result := icDec
  else
  if Decl = Left.Declaration then
    Result := icSub2
  else
    Result := icSub;
end;

function TILSub.Text: string;
begin
  if ILCode = icDec then
    Result := 'DEC ' + ConditionToStr(Condition) + ExpressionName(FDestination)
  else
    Result := 'SUB ' + inherited Text;
end;

procedure TILSub.Write(Proc: TIDProcedure; Stream: TStream);
var
  iCode: TILCode;
begin
  iCode := ILCode;
  WriteILCode(Stream, iCode, FCondition);
  WriteILArgument(Proc, Stream, Destination);
  if iCode = icDec then Exit;
  if iCode = icSub then
    WriteILArgument(Proc, Stream, Left);
  WriteILArgument(Proc, Stream, Right);
end;

{ TILProcCall }

function TILProcCall.ArgumentsCount: Integer;
begin
  Result := 1 + 1 + Length(FArgs);
end;

procedure TILProcCall.EnumerateArgs(const EnumProc: TILArgsEnumProc; var BreakEnum: Boolean);
var
  i: Integer;
begin
  if Assigned(FDestination) then EnumProc(FDestination, BreakEnum);
  if BreakEnum then
    Exit;

  EnumProc(FProc, BreakEnum);
  if BreakEnum then
    Exit;

  if Assigned(FInstance) then EnumProc(FInstance, BreakEnum);
  if BreakEnum then
    Exit;

  for i := 0 to Length(FArgs) - 1 do
  begin
    EnumProc(FArgs[i], BreakEnum);
    if BreakEnum then
      Exit;
  end;
end;

function TILProcCall.GetArgument(Index: Integer): TIDExpression;
begin
  case Index of
    0: Result := FDestination;
    1: Result := FProc;
    else
      Result := FArgs[Index];
  end;
end;

function TILProcCall.GetLine: Integer;
begin
  Result := FProc.TextPosition.Row;
end;

function TILProcCall.ILCode: TILCode;
begin
  Result := icProcCall;
end;

procedure TILProcCall.Init(Condition: TILCondition; Proc, Result, Instance: TIDExpression; const Args: TIDExpressions);
begin
  Init(Condition, Result, Args);
  FProc := Proc;
  FInstance := Instance;
end;

procedure TILProcCall.IncReferences(var RCPath: UInt32);
begin
  inherited IncReferences(RCPath);
  IncReadCount(FProc, Self, RCPath); Inc(RCPath);
  if Assigned(FInstance) then
  begin
    IncReadCount(FInstance, Self, RCPath);
    Inc(RCPath);
  end;
end;

procedure TILProcCall.CECalc(var Ctx: TILCECalcContext);
begin
  if FProc.ItemType = itProcedure then
    FDestination.CValue := FProc.AsProcedure.CECalc(FArgs);
end;

procedure TILProcCall.DecReferences(var RCPath: UInt32);
begin
  inherited DecReferences(RCPath);
  DecReadCount(FProc, Self, RCPath); Inc(RCPath);
  if Assigned(FInstance) then
  begin
    DecReadCount(FInstance, Self, RCPath);
    Inc(RCPath);
  end;
end;

procedure TILProcCall.ProcessVarInit(Proc: TIDProcedure);
var
  Param: TIDVariable;
  Arg: TIDExpression;
  i: Integer;
begin
  inherited;
  if FProc.ItemType = itProcedure then
    Param := FProc.AsProcedure.VarSpace.First
  else begin
     if Length((FProc.DataType as TIDProcType).Params) > 0 then
       Param := (FProc.DataType as TIDProcType).Params[0]
     else
       Param := nil;
  end;
  i := 0;
  {проставляем модификацию возвращаемым аргументам}
  while Assigned(Param) do
  begin
    if (VarResult in Param.Flags) or
       (VarSelf in Param.Flags) then
    begin
      Param := TIDVariable(Param.NextItem);
      continue;
    end;
    if VarOut in Param.Flags then
    begin
      Arg := FArgs[i];
      if Arg.IsLocalVar then
        FCFBlock.AddVariable(Arg.AsVariable);
    end;
    Inc(i);
    Param := TIDVariable(Param.NextItem);
  end;
end;

procedure TILProcCall.SwapArguments(const Context: TPIContext);
begin
  inherited;
  FProc := TILProcCall(Context.Instruction).FProc;
end;

function TILProcCall.Text: string;
var
  Proc: TIDProcedure;
  DeclName: string;
begin
  if Assigned(FDestination) then
    Result := Trim(inherited Text)
  else
    Result := '';
  Result := AddStringSegment(Result, GetArgsNames, ', ');
  Proc := FProc.AsProcedure;
  if Assigned(FInstance) then
    DeclName := FInstance.DisplayName + '.' + Proc.DisplayName
  else
    DeclName := DeclarationName(Proc);

  Result := ILCodeString + ' ' + CondAsText + AddStringSegment(DeclName, Result, ', ');
end;

procedure TILProcCall.Write(Proc: TIDProcedure; Stream: TStream);
var
  Cnt, UnitID: Integer;
  ProcDecl: TIDProcedure;
begin
  WriteILCode(Stream, ILCode, Condition);
  // сохраняем информацию о типе если это статический метод
  if FProc.ItemType = itProcedure then
  begin
    ProcDecl := FProc.AsProcedure;
    if ProcDecl.IsStatic then begin
      UnitID := GetUnitID(Proc, ProcDecl.Struct);
      WriteArgument(Stream, ProcDecl.Struct, UnitID, True);
    end else
    if Assigned(FInstance) then
      WriteILArgument(Proc, Stream, FInstance, True);
  end;
  WriteILArgument(Proc, Stream, FProc);

  // пишем кол-во аргуметов
  Cnt := Length(FArgs);
  if Assigned(FDestination) then
    Inc(Cnt);
  Stream.WriteStretchUInt(Cnt);

  // пишем аргуметы
  if Assigned(FDestination) then
    WriteILArgument(Proc, Stream, FDestination);
  WriteILArguments(Proc, Stream, FArgs, False);
end;

{ TILProcCallUnSafe }

function TILProcCallUnSafe.ILCode: TILCode;
begin
  Result := icUSafeCall;
end;

procedure TILProcCallUnSafe.Write(Proc: TIDProcedure; Stream: TStream);
var
  Cnt, UnitID: Integer;
  ProcDecl: TIDProcedure;
begin
  // write il code header
  WriteILCode(Stream, ILCode, Condition);

  // write ptr-variable arg
  WriteILArgument(Proc, Stream, FProc);
  // write the casted-type arg
  WriteILArgument(Proc, Stream, TIDCastedCallExpression(FProc).DataType);

  // write count of args
  Cnt := Length(FArgs);
  if Assigned(FDestination) then
    Inc(Cnt);
  Stream.WriteStretchUInt(Cnt);

  // write the args...
  if Assigned(FDestination) then
    WriteILArgument(Proc, Stream, FDestination);
  WriteILArguments(Proc, Stream, FArgs, False);
end;

{ TILNeg }

procedure TILNeg.CECalc(var Ctx: TILCECalcContext);
begin
  FDestination.CValue := ProcessConstOperation(FSource.CValue, FSource.CValue, opNegative);
end;

function TILNeg.ILCode: TILCode;
begin
  Result := icNeg;
end;

function TILNeg.Text: string;
begin
  Result := 'NEG ' + inherited Text;
end;

{ TILCmp }

procedure TILCmp.CECalc(var Ctx: TILCECalcContext);
var
  CRes: TIDConstant;
  CmpOp: TOperatorID;
begin
  case FNext.Condition of
    cEqual: CmpOp := opEqual;
    cNotEqual: CmpOp := opNotEqual;
    cGreater: CmpOp := opGreater;
    cGreaterOrEqual: CmpOp := opGreaterOrEqual;
    cLess: CmpOp := opLess;
    cLessOrEqual: CmpOp := opLessOrEqual;
  else
    AbortWorkInternal('Unknown condition');
    CmpOp := opNone;
  end;
  CRes := ProcessConstOperation(FLeft.CValue, FRight.CValue, CmpOp);
  Ctx.Cond := (CRes as TIDBooleanConstant).Value;
end;

function TILCmp.ILCode: TILCode;
begin
  Result := icCmp;
end;

function TILCmp.Text: string;
begin
  Result := 'CMP ' + inherited Text;
end;

{ TILUniqueInstruction }

function TILUniqueInstruction.ILCode: TILCode;
begin
  Result := icUnique;
end;

function TILUniqueInstruction.Text: string;
begin
  Result := 'UNIQUE ' + inherited Text;;
end;

{ TILJmp }

function TILJmp.ILCode: TILCode;
begin
  Result := icJmp
end;

function TILJmp.Text: string;
begin
  Result := Format('JMP %s', [inherited Text]);
end;

{ TILJmpNext }

procedure TILJmpNext.CECalc(var Ctx: TILCECalcContext);
begin
  if (FCondition <> cNone) and not Ctx.Cond then
    Exit;

  if Assigned(FDestination.Next) then
    Ctx.Next := FDestination.Next
  else
    Ctx.Return := True;
end;

function TILJmpNext.GetDestinationPosition: Integer;
begin
  if Assigned(FDestination) then
    Result := FDestination.Position + 1
  else
    Result := -1;
end;

{ TILCast }

function TILCast.ILCode: TILCode;
begin
  Result := icCovert;
end;

function TILCast.Text: string;
begin
  Result := 'CAST ' + inherited Text;
end;

{ TILDestInstruction }

function TILDestInstruction.ArgumentsCount: Integer;
begin
  Result := 1;
end;

procedure TILDestInstruction.EnumerateArgs(const EnumProc: TILArgsEnumProc; var BreakEnum: Boolean);
begin
  EnumProc(FDestination, BreakEnum);
end;

function TILDestInstruction.GetArgument(Index: Integer): TIDExpression;
begin
  if Index = 0 then
    Result := FDestination
  else
    Result := nil;
end;

function TILDestInstruction.GetLine: Integer;
begin
  if Assigned(FDestination) then
    Result := FDestination.TextPosition.Row
  else
    Result := -1;
end;

procedure TILDestInstruction.Init(Condition: TILCondition; Destination: TIDExpression);
begin
  FCondition := Condition;
  FDestination := Destination;
  if Assigned(Destination) then
    Destination.Instruction := Self;
end;

procedure TILDestInstruction.IncReferences(var RCPath: UInt32);
begin
  IncReadCount(FDestination, Self, RCPath);
  Inc(RCPath);
end;

procedure TILDestInstruction.DecReferences(var RCPath: UInt32);
begin
  DecReadCount(FDestination, Self, RCPath);
  Inc(RCPath);
end;

procedure TILDestInstruction.ProcessVarInit(Proc: TIDProcedure);
begin
  if Assigned(FDestination) and FDestination.IsLocalVar then
    FCFBlock.AddVariable(FDestination.AsVariable);
end;

function TILDestInstruction.Text: string;
var
  Name: string;
begin
  Name := ExpressionName(FDestination);
  Result := Format('%s%s',[inherited Text, Name]);
end;

procedure TILDestInstruction.Write(Proc: TIDProcedure; Stream: TStream);
begin
  inherited Write(Proc, Stream);
  WriteILArgument(Proc, Stream, FDestination);
end;

function InlineInstructionArgument(Src: TIDExpression; const Context: TPIContext): TIDExpression;
  procedure CheckNode(Node: TIDPairList.PAVLNode; Src: TIDExpression); inline;
  begin
    if not Assigned(Node) then
      AbortWorkInternal('Equal object for [%s: %s] is not found', [Src.DisplayName, Src.DataType.DisplayName]);
  end;
var
  EList: TIDExpressions;
  SrcItems: TIDExpressions;
  Node: TIDPairList.PAVLNode;
begin
  if (Src.ItemType = itVar) and (Src.AsVariable.Scope.ScopeType <> stGlobal) then
  begin
    if Src.ExpressionType = etDeclaration then begin
      if Src.AsVariable.IsField then
      begin
        {SetLength(EList, 2);                          // !!!!!!!!! неизвестно правильно ли изменен код
        EList[0] := Context.MethodSelf;
        EList[1] := Src;}
        Result := Src;// TIDMultyExpression.Create(EList, Src.TextPosition);
      end else begin
        Node := Context.EqualList.Find(Src.Declaration);
        CheckNode(Node, Src);
        Result := TIDExpression(Node.Data);
      end;
    end else begin
      SrcItems := TIDMultiExpression(Src).Items;
      SetLength(EList, Length(SrcItems)); // пока только список из 2-х элементов!!!
      EList[1] := TIDMultiExpression(Src).Items[1];
      Node := Context.EqualList.Find(SrcItems[0].Declaration);
      CheckNode(Node, Src);
      EList[0] := TIDExpression(Node.Data);
      Result := TIDMultiExpression.Create(EList, Src.TextPosition);
    end;
  end else
    Result := Src;
end;

procedure TILDestInstruction.SetDestination(const Value: TIDExpression);
begin
  FDestination := Value;
  if Assigned(FDestination) and FDestination.IsLocalVar then
    FCFBlock.AddVariable(FDestination.AsVariable);
end;

procedure TILDestInstruction.SwapArguments(const Context: TPIContext);
var
  SrcInstrDst: TIDExpression;
begin
  inherited SwapArguments(Context);
  SrcInstrDst := TILDestInstruction(Context.Instruction).Destination;
  if Assigned(SrcInstrDst) then begin
    Destination := InlineInstructionArgument(SrcInstrDst, Context);
    Destination.Instruction := Self;
  end;
end;

{ TILSetBool }

function TILSetBool.ILCode: TILCode;
begin
  Result := icSetBool;
end;

function TILSetBool.Text: string;
begin
  Result := 'SETBOOL ' + inherited Text;
end;

{ TILGetBit }

function TILGetBit.ILCode: TILCode;
begin
  Result := icGetBit;
end;

function TILGetBit.Text: string;
begin
  Result := 'GETBIT ' + inherited Text;
end;

{ TILSetBit }

function TILSetBit.ILCode: TILCode;
begin
  Result := icSetBit;
end;

function TILSetBit.Text: string;
begin
  Result := 'SETBIT ' + inherited Text;
end;

{ TILNot }

procedure TILNot.CECalc(var Ctx: TILCECalcContext);
begin
  FDestination.CValue := ProcessConstOperation(FSource.CValue, FSource.CValue, opNot);
end;

function TILNot.ILCode: TILCode;
begin
  Result := icNot;
end;

function TILNot.Text: string;
begin
  Result := 'NOT ' + inherited Text;
end;

{ TILAnd }

procedure TILAnd.CECalc(var Ctx: TILCECalcContext);
begin
  FDestination.CValue := ProcessConstOperation(FLeft.CValue, FRight.CValue, opAnd);
end;

function TILAnd.ILCode: TILCode;
begin
  if Destination.Declaration <> Left.Declaration then
    Result := icAnd
  else
    Result := icAnd2;
end;

function TILAnd.Text: string;
begin
  Result := 'AND ' + inherited Text;
end;

{ TILOr }

procedure TILOr.CECalc(var Ctx: TILCECalcContext);
begin
  FDestination.CValue := ProcessConstOperation(FLeft.CValue, FRight.CValue, opOr);
end;

function TILOr.ILCode: TILCode;
begin
  if Destination.Declaration <> Left.Declaration then
    Result := icOr
  else
    Result := icOr2;
end;

function TILOr.Text: string;
begin
  Result := 'OR ' + inherited Text;
end;

{ TILXor }

procedure TILXor.CECalc(var Ctx: TILCECalcContext);
begin
  FDestination.CValue := ProcessConstOperation(FLeft.CValue, FRight.CValue, opXor);
end;

function TILXor.ILCode: TILCode;
begin
  if Destination.Declaration <> Left.Declaration then
    Result := icXor
  else
    Result := icXor2;
end;

function TILXor.Text: string;
begin
  Result := 'XOR ' + inherited Text;
end;

{ TILShr }

procedure TILShr.CECalc(var Ctx: TILCECalcContext);
begin
  FDestination.CValue := ProcessConstOperation(FLeft.CValue, FRight.CValue, opShiftRight);
end;

function TILShr.ILCode: TILCode;
begin
  if Destination.Declaration <> Left.Declaration then
    Result := icShr
  else
    Result := icShr2;
end;

function TILShr.Text: string;
begin
  Result := 'SHR ' + inherited Text;
end;

{ TILShl }

procedure TILShl.CECalc(var Ctx: TILCECalcContext);
begin
  FDestination.CValue := ProcessConstOperation(FLeft.CValue, FRight.CValue, opShiftLeft);
end;

function TILShl.ILCode: TILCode;
begin
  if Destination.Declaration <> Left.Declaration then
    Result := icShl
  else
    Result := icShl2;
end;

function TILShl.Text: string;
begin
  Result := 'SHL ' + inherited Text;
end;


{ TILTest }

function TILTest.ILCode: TILCode;
begin
  Result := icTest;
end;

function TILTest.Text: string;
begin
  Result := 'TEST ' + inherited Text;
end;

{ TILCheckBound }

function TILCheckBound.ILCode: TILCode;
begin
  Result := icCheckBound;
end;

function TILCheckBound.Text: string;
begin
  Result := 'CHKB ' + inherited Text;
end;

{ TILCompareInstruction }

function TILCompareInstruction.ArgumentsCount: Integer;
begin
  Result := 2;
end;

procedure TILCompareInstruction.EnumerateArgs(const EnumProc: TILArgsEnumProc; var BreakEnum: Boolean);
begin
  EnumProc(FLeft, BreakEnum);
  if BreakEnum then
    Exit;
  EnumProc(FRight, BreakEnum);
end;

function TILCompareInstruction.GetArgument(Index: Integer): TIDExpression;
begin
  case Index of
    0: Result := FLeft;
    1: Result := FRight;
    else Result := nil;
  end;
end;

function TILCompareInstruction.GetLine: Integer;
begin
  if Assigned(FLeft) then
    Result := FLeft.TextPosition.Row
  else
    Result := -1;
end;

procedure TILCompareInstruction.Init(Condition: TILCondition; Left, Right: TIDExpression);
begin
  FCondition := Condition;
  FLeft := Left;
  FRight := Right;
end;

procedure TILCompareInstruction.IncReferences(var RCPath: UInt32);
begin
  IncReadCount(FLeft, Self, RCPath); Inc(RCPath);
  IncReadCount(FRight, Self, RCPath); Inc(RCPath);
end;

procedure TILCompareInstruction.DecReferences(var RCPath: UInt32);
begin
  DecReadCount(FLeft, Self, RCPath); Inc(RCPath);
  DecReadCount(FRight, Self, RCPath); Inc(RCPath);
end;

procedure TILCompareInstruction.ProcessVarInit(Proc: TIDProcedure);
begin
  CheckArgInit(Proc, Left);
  CheckArgInit(Proc, Right);
end;

function TILCompareInstruction.Text: string;
begin
  Result := Format('%s%s, %s', [inherited Text,  ExpressionName(FLeft), ExpressionName(FRight)]);
end;

{ TILDstSrcInstruction }

procedure TILDstSrcInstruction.SwapArguments(const Context: TPIContext);
var 
  Src: TIDExpression;
begin
  inherited SwapArguments(Context);
  Src := TILDstSrcInstruction(Context.Instruction).Source;
  Source := InlineInstructionArgument(Src, Context);
end;

procedure TILDstSrcInstruction.Write(Proc: TIDProcedure; Stream: TStream);
begin
  inherited Write(Proc, Stream);
  WriteILArgument(Proc, Stream, FSource);
end;

procedure TILDstSrcSrcInstruction.SwapArguments(const Context: TPIContext);
begin
  inherited SwapArguments(Context);
  with TILDstSrcSrcInstruction(Context.Instruction) do begin
    Self.Left := InlineInstructionArgument(Left, Context);
    Self.Right := InlineInstructionArgument(Right, Context);
  end;
end;

procedure TILDstSrcSrcInstruction.Write(Proc: TIDProcedure; Stream: TStream);
begin
  inherited Write(Proc, Stream);
  if Destination.Declaration <> Left.Declaration then
    WriteILArgument(Proc, Stream, FLeft);
  if Assigned(FRight) then
    WriteILArgument(Proc, Stream, FRight);
end;

procedure TILCompareInstruction.SwapArguments(const Context: TPIContext);
begin
  inherited SwapArguments(Context);
  with TILCompareInstruction(Context.Instruction) do begin
    Self.Left := InlineInstructionArgument(Left, Context);
    Self.Right := InlineInstructionArgument(Right, Context);
  end;
end;

procedure TILCompareInstruction.Write(Proc: TIDProcedure; Stream: TStream);
begin
  inherited;
  WriteILArgument(Proc, Stream, FLeft);
  WriteILArgument(Proc, Stream, FRight);
end;

{ TILGetPtr }

procedure TILGetPtr.EnumerateArgs(const EnumProc: TILArgsEnumProc; var BreakEnum: Boolean);
begin
  EnumProc(FDestination, BreakEnum);
  if BreakEnum then
    Exit;

  if Assigned(FBase) then EnumProc(FBase, BreakEnum);
  if BreakEnum then
    Exit;

  EnumProc(FMember, BreakEnum);
end;

function TILGetPtr.ILCode: TILCode;
begin
  if FMember.IsProcedure then
  begin
    if Assigned(FBase) then
      Result := icLoadMethod
    else
      Result := icLoadSelfMethod;
  end else begin
    if Assigned(FBase) then
      Result := icGetPtr
    else
      Result := icGetSelfPtr;
  end;
end;

procedure TILGetPtr.SwapArguments(const Context: TPIContext);
var
  Code: TILGetPtr;
begin
  inherited SwapArguments(Context);
  Code := TILGetPtr(Context.Instruction);
  if Assigned(Code.Base) then
    Self.FBase := InlineInstructionArgument(Code.Base, Context)
  else
    Self.FBase := nil;
  Self.FMember := InlineInstructionArgument(Code.Member, Context);
end;

function TILGetPtr.Text: string;
var
  BaseStr, S1, S2: string;
begin
  if Assigned(FBase) then
  begin
    BaseStr := ExpressionName(FBase);
    if FBase.DataType is TIDArray then
    begin
      S1 := '[';
      S2 := ']';
    end else begin
      S1 := '.';
      S2 := '';
    end;
  end;
  Result := ILCodeString + ' ' + inherited Text + ', ' + BaseStr + S1 + ExpressionName(FMember) + S2;
end;

procedure TILGetPtr.Write(Proc: TIDProcedure; Stream: TStream);
begin
  inherited Write(Proc, Stream);
  if Assigned(FBase) then
    WriteILArgumentsChain(Proc, Stream, FBase, FMember)
  else
    WriteILArgument(Proc, Stream, FMember);
end;

{ TILGetPtrMulti }

function TILGetPtrMulti.ArgumentsCount: Integer;
begin
  Result := 1 + Length(FArgs);
end;

procedure TILGetPtrMulti.EnumerateArgs(const EnumProc: TILArgsEnumProc; var BreakEnum: Boolean);
begin
  EnumProc(FDestination, BreakEnum);
  if BreakEnum then
    Exit;

  EnumProc(FArgs[0], BreakEnum);
  if BreakEnum then
    Exit;
end;

function TILGetPtrMulti.GetArgument(Index: Integer): TIDExpression;
begin
  if Index = 0 then
    Result := FDestination
  else
    Result := FArgs[Index];
end;

function TILGetPtrMulti.ILCode: TILCode;
begin
  if FArgs[0].Declaration.ClassType <> TIDField then
    Result := icGetPtr
  else
    Result := icGetSelfPtr;
end;

function TILGetPtrMulti.Text: string;
begin
  Result := ILCodeString + ' ' + ExpressionName(FDestination) + ', ' + ExpressionsName(FArgs);
end;

procedure TILGetPtrMulti.Write(Proc: TIDProcedure; Stream: TStream);
begin
  inherited Write(Proc, Stream);
  WriteILArgumentsChain(Proc, Stream, FArgs);
end;

{ TILRet }

procedure TILRet.CECalc(var Ctx: TILCECalcContext);
begin
  if (FCondition <> cNone) and not Ctx.Cond then
    Exit;
  Ctx.Return := True;
end;

function TILRet.ILCode: TILCode;
begin
  Result := icRet;
end;

function TILRet.Text: string;
begin
  Result := 'RET ' + inherited Text;
end;

{ TILDestMultiArgsInstruction }

function TILDestMultiArgsInstruction.GetArgsNames: string;
var
  i: Integer;
  Expr: TIDExpression;
begin
  Result := '';
  for i := 0 to Length(FArgs) - 1 do
  begin
    Expr := FArgs[i];
    if Assigned(Expr) then
      Result := AddStringSegment(Result, DeclarationName(Expr.Declaration), ', ')
    else
      Result := AddStringSegment(Result, 'nil', ', ');
  end;
end;

procedure TILDestMultiArgsInstruction.Init(Condition: TILCondition; Dest: TIDExpression; const Args: TIDExpressions);
begin
  Init(Condition, Dest);
  FArgs := Args;
end;

procedure TILDestMultiArgsInstruction.IncReferences(var RCPath: UInt32);
var
  i: Integer;
begin
  IncReadCount(FDestination, Self, RCPath); Inc(RCPath);
  for i := 0 to Length(FArgs) - 1 do
  begin
    IncReadCount(FArgs[i], Self, RCPath);
    Inc(RCPath);
  end;
end;

procedure TILDestMultiArgsInstruction.DecReferences(var RCPath: UInt32);
var
  i: Integer;
begin
  DecReadCount(FDestination, Self, RCPath); Inc(RCPath);
  for i := 0 to Length(FArgs) - 1 do
  begin
    DecReadCount(FArgs[i], Self, RCPath);
    Inc(RCPath);
  end;
end;

procedure TILDestMultiArgsInstruction.SwapArguments(const Context: TPIContext);
var
  i: Integer;
  SrcInstruction: TILDestMultiArgsInstruction;
begin
  inherited SwapArguments(Context);
  SrcInstruction := TILDestMultiArgsInstruction(Context.Instruction);
  SetLength(FArgs, Length(SrcInstruction.FArgs));
  for i := 0 to High(SrcInstruction.FArgs) do
    FArgs[i] := InlineInstructionArgument(SrcInstruction.Arguments[i], Context);
end;

{ TILLoadAddress }

function TILLoadAddr.ILCode: TILCode;
begin
  if Assigned(Right) then
    Result := icLea
  else
    Result := icLea2
end;

function TILLoadAddr.Text: string;
begin
  Result := 'LEA ' + Trim(inherited Text);
end;

procedure TILLoadAddr.Write(Proc: TIDProcedure; Stream: TStream);
begin
  WriteILCode(Stream, ILCode, FCondition);
  WriteILArgument(Proc, Stream, FDestination);
  WriteILArgument(Proc, Stream, FLeft);
  if Assigned(FRight) then
    WriteILArgument(Proc, Stream, FRight);
end;

{ TILArrayDAlloc }

function TILArrayDAlloc.ILCode: TILCode;
begin
  Result := icArrayDAlloc
end;

function TILArrayDAlloc.Text: string;
begin
  Result := 'ARR_DALLOC ' + inherited Text;
end;

{ TILArrayRAlloc }

function TILArrayRAlloc.ILCode: TILCode;
begin
  Result := icArrayRAlloc
end;

function TILArrayRAlloc.Text: string;
begin
  Result := 'ARR_REALLOC ' + inherited Text;
end;

{ TILArraySAlloc }

function TILArraySAlloc.ILCode: TILCode;
begin
  Result := icArraySAlloc
end;

function TILArraySAlloc.Text: string;
begin
  Result := 'ARR_SALLOC ' + inherited Text;
end;

{ TILArrayLength }

function TILArrayLength.ILCode: TILCode;
begin
  Result := icArrayLength;
end;

function TILArrayLength.Text: string;
begin
  Result := 'LENGTH ' + inherited Text;
end;

{ TILTryBegin }

function TILTryBegin.ILCode: TILCode;
begin
  Result := icTryBegin;
end;

function TILTryBegin.Text: string;
begin
  Result := 'TRYBEGIN ' + inherited Text;
end;

{ TILTryEnd }

function TILTryEnd.ILCode: TILCode;
begin
  Result := icTryEnd;
end;

function TILTryEnd.Text: string;
begin
  Result := 'TRYEND ' + inherited Text;
end;

{ TIL1ConstArgInstruction }

function TIL1ConstArgInstruction.ArgumentsCount: Integer;
begin
  Result := 1;
end;

function TIL1ConstArgInstruction.Text: string;
begin
  Result := Format('%s%d', [inherited Text, Argument]);
end;

procedure TIL1ConstArgInstruction.Write(Proc: TIDProcedure; Stream: TStream);
begin
  inherited Write(Proc, Stream);
  WriteConstArgument(Stream, FArgument);
end;

{ TILEThrow }

function TILEThrow.ILCode: TILCode;
begin
  Result := icEThrow;
end;

function TILEThrow.Text: string;
begin
  Result := 'ETHROW ' + inherited Text;
end;

{ TILTryCallHandler }

function TILTryCallHandler.ILCode: TILCode;
begin
  Result := icTryCallHandler;
end;

function TILTryCallHandler.Text: string;
begin
  Result := 'TRYCALLHANDLER ' + inherited Text;
end;

{ TILNearCall }

function TILNearCall.ILCode: TILCode;
begin
  Result := icNearCall;
end;

function TILNearCall.Text: string;
begin
  Result := 'NCALL ' + inherited Text;
end;

{ TILDNewObj }

procedure TILDNewObj.EnumerateArgs(const EnumProc: TILArgsEnumProc; var BreakEnum: Boolean);
begin
  EnumProc(FDestination, BreakEnum);
  if BreakEnum then
    Exit;

  EnumProc(FInstance, BreakEnum);
end;

function TILDNewObj.ILCode: TILCode;
begin
  Result := icDNewObj;
end;

procedure TILDNewObj.IncReferences(var RCPath: UInt32);
begin
  IncReadCount(FDestination, Self, RCPath); Inc(RCPath);
  IncReadCount(FInstance, Self, RCPath); Inc(RCPath);
end;

procedure TILDNewObj.DecReferences(var RCPath: UInt32);
begin
  DecReadCount(FDestination, Self, RCPath); Inc(RCPath);
  DecReadCount(FInstance, Self, RCPath); Inc(RCPath);
end;

function TILDNewObj.Text: string;
begin
  Result := 'DNEWOBJ ' + FDestination.DisplayName + ', ' + FInstance.DisplayName;
end;

procedure TILDNewObj.Write(Proc: TIDProcedure; Stream: TStream);
begin
  WriteILCode(Stream, ILCode, FCondition);
  WriteILArgument(Proc, Stream, FInstance);
  WriteILArgument(Proc, Stream, FDestination);
end;

{ TILSNewObj }

function TILSNewObj.ILCode: TILCode;
begin
  Result := icSNewObj;
end;

{ TILNewInstruction }

function TILMemGet.ILCode: TILCode;
begin
  Result := icMemGet;
end;

function TILMemGet.Text: string;
begin
  Result := 'MEMGET ' + inherited Text;
end;

{ TILFreeInstanceInstruction }

function TILMemFree.ILCode: TILCode;
begin
  Result := icMemFree;
end;

function TILMemFree.Text: string;
begin
  Result := 'MEMFREE ' + inherited Text;
end;

{ TILJampedInstruction }

procedure TILJampedInstruction.CECalc(var Ctx: TILCECalcContext);
begin
  if (FCondition <> cNone) and not Ctx.Cond then
    Exit;
  Ctx.Next := FDestination;
end;

function TILJampedInstruction.GetDestinationPosition: Integer;
begin
  if Assigned(FDestination) then
    Result := FDestination.Position
  else
    Result := -1;
end;

function TILJampedInstruction.Text: string;
begin
  Result := inherited Text + IntToStr(DestinationPosition);
end;

procedure TILJampedInstruction.Write(Proc: TIDProcedure; Stream: TStream);
begin
  inherited Write(Proc, Stream);
  WriteConstArgument(Stream, DestinationPosition);
end;

{ TILNopeInstruction }

procedure TILNope.CECalc(var Ctx: TILCECalcContext);
begin
  // nope
end;

function TILNope.ILCode: TILCode;
begin
  Result := icNope;
end;

function TILNope.Text: string;
begin
  Result := 'NOPE';
end;

{ TILVirtCallInstruction }

function TILVirtCall.ILCode: TILCode;
begin
  Result := icVirtCall;
end;

{ TILMethodCallInstruction }

function TILInheritedCall.ILCode: TILCode;
begin
  Result := icInhtCall;
end;

function TILInheritedCall.Text: string;
var
  Proc: TIDProcedure;
  DeclName: string;
begin
  if Assigned(FDestination) then
    Result := FDestination.DisplayName
  else
    Result := '';
  Result := AddStringSegment(Result, GetArgsNames, ', ');
  Proc := FProc.AsProcedure;
  DeclName := FInstance.Declaration.DisplayName + '.' + Proc.Struct.DisplayName + '.' + Proc.DisplayName;
  Result := ILCodeString + ' ' + AddStringSegment(DeclName, Result, ', ');
end;

procedure TILInheritedCall.Write(Proc: TIDProcedure; Stream: TStream);
var
  ProcDecl: TIDProcedure;
  Cnt: Integer;
begin
  WriteILCode(Stream, ILCode, Condition);
  ProcDecl := FProc.AsProcedure;
  WriteILArgument(Proc, Stream, ProcDecl.Struct);
  Stream.WriteStretchUInt(ProcDecl.Index);
  {сохраняем инстанс}
  WriteILArgument(Proc, Stream, FInstance);

  {пишем кол-во аргуметов}
  Cnt := Length(FArgs);
  if Assigned(FDestination) then
    Inc(Cnt);
  Stream.WriteStretchUInt(Cnt);

  {пишем аргуметы}
  if Assigned(FDestination) then
    WriteILArgument(Proc, Stream, FDestination);
  WriteILArguments(Proc, Stream, FArgs, False);
end;

{ TILIncRef }

function TILIncRef.ILCode: TILCode;
begin
  Result := icIncRef;
end;

function TILIncRef.Text: string;
begin
  Result := 'INCREF ' + inherited Text;
end;

{ TILDecRef }

function TILDecRef.ILCode: TILCode;
begin
  if not Assigned(Destination.DataType.FinalProc) then
    Result := icDecRef
  else
    Result := icDecRefFinal;
end;

procedure TILDecRef.IncReferences(var RCPath: UInt32);
begin
  IncReadCount(FDestination, Self, RCPath); Inc(RCPath);
  if ILCode = icDecRefFinal then
  begin
    FDestination.DataType.FinalProc.IncRefCount(RCPath);
    Inc(RCPath);
  end;
end;

procedure TILDecRef.DecReferences(var RCPath: UInt32);
begin
  DecReadCount(FDestination, Self, RCPath); Inc(RCPath);
  if ILCode = icDecRefFinal then
  begin
    FDestination.DataType.FinalProc.DecRefCount(RCPath);
    Inc(RCPath);
  end;
end;

function TILDecRef.Text: string;
begin
  Result := 'DECREF ' + inherited Text;
  if ILCode = icDecRefFinal then
    Result := Result + ', ' + DeclarationName(FDestination.DataType.FinalProc);
end;

procedure TILDecRef.Write(Proc: TIDProcedure; Stream: TStream);
var
  FProc: TIDProcedure;
begin
  inherited;
  if ILCode = icDecRefFinal then
  begin
    FProc := FDestination.DataType.FinalProc;
    WriteArgument(Stream, FProc, FProc.UnitID);
  end;
end;

{ TILRefInit }

function TILInit.ILCode: TILCode;
begin
  Result := icInit;
end;

function TILInit.Text: string;
begin
  Result := 'INIT ' + inherited Text;
end;

{ TILGetStrong }

function TILStrongRef.ILCode: TILCode;
begin
  Result := icStrongRef;
end;

function TILStrongRef.Text: string;
begin
  Result := 'STRONGREF ' + inherited Text;
end;

{ TILGetWeakRef }

function TILWeakRef.ILCode: TILCode;
begin
  Result := icWeakRef;
end;

function TILWeakRef.Text: string;
begin
  Result := 'WEAKREF ' + inherited Text;
end;

{ TIL1ArgsInstriction }

function TIL1ArgInstriction.ArgumentsCount: Integer;
begin
  Result := 1;
end;

procedure TIL1ArgInstriction.ProcessVarInit(Proc: TIDProcedure);
begin
  CheckArgInit(Proc, Arg);
end;

procedure TIL1ArgInstriction.EnumerateArgs(const EnumProc: TILArgsEnumProc; var BreakEnum: Boolean);
begin
  EnumProc(FArg, BreakEnum);
end;

function TIL1ArgInstriction.GetArgument(Index: Integer): TIDExpression;
begin
  if Index = 0 then
    Result := FArg
  else
    Result := nil;
end;

function TIL1ArgInstriction.GetLine: Integer;
begin
  if Assigned(FArg) then
    Result := FArg.TextPosition.Row
  else
    Result := -1;
end;

procedure TIL1ArgInstriction.Init(Condition: TILCondition; Arg: TIDExpression);
begin
  FCondition := Condition;
  FArg := Arg;
end;

procedure TIL1ArgInstriction.IncReferences(var RCPath: UInt32);
begin
  IncReadCount(FArg, Self, RCPath);
  Inc(RCPath);
end;

procedure TIL1ArgInstriction.DecReferences(var RCPath: UInt32);
begin
  DecReadCount(FArg, Self, RCPath);
  Inc(RCPath);
end;

procedure TIL1ArgInstriction.SwapArguments(const Context: TPIContext);
var
  SrcInstrArg: TIDExpression;
begin
  inherited SwapArguments(Context);
  SrcInstrArg := TIL1ArgInstriction(Context.Instruction).Arg;
  Arg := InlineInstructionArgument(SrcInstrArg, Context);
end;

function TIL1ArgInstriction.Text: string;
var
  Name: string;
begin
  Name := ExpressionName(FArg);
  Result := Format('%s%s',[inherited Text, Name]);
end;

procedure TIL1ArgInstriction.Write(Proc: TIDProcedure; Stream: TStream);
begin
  inherited Write(Proc, Stream);
  WriteILArgument(Proc, Stream, FArg);
end;

{ TILTypeInfo }

function TILTypeInfo.ILCode: TILCode;
begin
  Result := icTypeInfo;
end;

function TILTypeInfo.Text: string;
begin
  Result := 'TYPEINFO ' + inherited Text;
end;

{ TILCmpJmp }

function TILCmpJmp.ILCode: TILCode;
begin
  Result := icCmpJmp;
end;

procedure TILCmpJmp.Init(Condition: TILCondition; Left, Right: TIDExpression; Destination: TILInstruction);
begin
  inherited Init(Condition, Left, Right);
  FDestination := Destination;
end;

function TILCmpJmp.Text: string;
var
  Pos: Integer;
begin
  if Assigned(FDestination) then
    Pos := FDestination.Position + 1
  else
    Pos := -1;

  Result := 'CMPJ ' + inherited Text + ', ' +  IntToStr(Pos);
end;

procedure TILCmpJmp.Write(Proc: TIDProcedure; Stream: TStream);
begin
  inherited Write(Proc, Stream);
  WriteConstArgument(Stream, FDestination.Position + 1);
end;

{ TILFinal

function TILFinal.ILCode: TILCode;
begin
  Result := icFinal;
end;

function TILFinal.Text: string;
begin
  Result := 'FINAL ' + inherited;
end;       }

{ TCFBlock }

function vars_cmp(const Left, Right: TIDVariable): NativeInt;
begin
  Result := NativeInt(Left) - NativeInt(Right);
end;

constructor TCFBlock.Create(Parent: TCFBlock);
begin
  CreateFromPool;
  FVars := TCFBVars.Create(vars_cmp);
  {if Assigned(Parent) then
    FVars.CopyFrom(Parent.FVars);}
  FParent := Parent;
end;

destructor TCFBlock.Destroy;
begin
  FVars.Free;
  inherited;
end;

function TCFBlock.FindVarInitDOWN(const Variable: TIDVariable): Boolean;
var
  Child: TCFBlock;
begin
  Result := (FVars.Find(Variable) <> nil);
  if Result then
    Exit;

  Child := FLastChild;
  while Assigned(Child) do
  begin
    Result := Child.FindVarInitDOWN(Variable);
    if Result then
      Exit;
    Child := Child.Prev;
  end;
end;

function TCFBlock.FindVarInitUP(const Variable: TIDVariable; Child: TCFBlock): Boolean;
var
  PrevBlock: TCFBlock;
begin
  Result := (FVars.Find(Variable) <> nil);
  if Result then
    Exit;

  PrevBlock := Child.Prev;
  while Assigned(PrevBlock) do
  begin
    Result := PrevBlock.FindVarInitDOWN(Variable);
    if Result then
      Exit;
    PrevBlock := PrevBlock.Prev;
  end;

  if Assigned(FParent) then
  begin
    Result := FParent.FindVarInitUP(Variable, Self);
    if Result then
      Exit;
  end;
end;

procedure TCFBlock.AddVariable(const Variable: TIDVariable);
begin
  FVars.InsertNode(Variable, True);
end;

function TCFBlock.IsVarInitialized(const Variable: TIDVariable): Boolean;
var
  PrevBlock: TCFBlock;
begin
  Result := (FVars.Find(Variable) <> nil);
  if Result then
    Exit;

  PrevBlock := FLastChild;
  while Assigned(PrevBlock) do
  begin
    Result := PrevBlock.FindVarInitDOWN(Variable);
    if Result then
      Exit;
    PrevBlock := PrevBlock.Prev;
  end;

  if Assigned(FParent) then
  begin
    Result := FParent.FindVarInitUP(Variable, Self);
    if Result then
      Exit;
  end;
end;

{ TILNoArgInstruction }

constructor TILNoArgInstruction.Create(Line: Integer);
begin
  CreateFromPool;
  FLine := Line;
end;

function TILNoArgInstruction.GetLine: Integer;
begin
  Result := FLine;
end;

{ TILArrayCopy }

function TILArrayCopy.ILCode: TILCode;
begin
  Result := icArrayCopy;
end;

procedure TILArrayCopy.IncReferences(var RCPath: UInt32);
begin
  inherited;
  IncReadCount(FFrom, Self, RCPath); Inc(RCPath);
  IncReadCount(FCount, Self, RCPath); Inc(RCPath);
end;

procedure TILArrayCopy.DecReferences(var RCPath: UInt32);
begin
  inherited;
  DecReadCount(FFrom, Self, RCPath); Inc(RCPath);
  DecReadCount(FCount, Self, RCPath); Inc(RCPath);
end;

function TILArrayCopy.Text: string;
begin
  Result := 'COPY ' + inherited + ', ' + ExpressionName(FFrom) + ', ' + ExpressionName(FCount);
end;

procedure TILArrayCopy.Write(Proc: TIDProcedure; Stream: TStream);
begin
  inherited;
  WriteILArgument(Proc, Stream, FFrom);
  WriteILArgument(Proc, Stream, FCount);
end;

{ TCFBIF }

function TCFBIF.FindVarInitDOWN(const Variable: TIDVariable): Boolean;
begin
  Result := Assigned(FTrue) and FTrue.FindVarInitDOWN(Variable) and
            Assigned(FElse) and FElse.FindVarInitDOWN(Variable);
end;

function TCFBIF.IsVarInitialized(const Variable: TIDVariable): Boolean;
begin
  Result := (FVars.Find(Variable) <> nil);
  if Result then
    Exit;

  Result := FindVarInitUP(Variable, Self);
end;

{ TCFBCASE }

function TCFBCASE.FindVarInitDOWN(const Variable: TIDVariable): Boolean;
begin
  Result := True;
end;

function TCFBCASE.IsVarInitialized(const Variable: TIDVariable): Boolean;
begin
  Result := FindVarInitUP(Variable, Self);
end;

{ TILDref }

function TILReadDRef.ILCode: TILCode;
begin
  Result := icReadDRef;
end;

function TILReadDRef.Text: string;
begin
  Result := 'RDREF ' + inherited;
end;

{ TILSetRef }

function TILWriteDRef.ILCode: TILCode;
begin
  Result := icWriteDRef;
end;

function TILWriteDRef.Text: string;
begin
  Result := 'WDREF ' + inherited;
end;

{ TILQueryIntf }

function TILQueryType.ILCode: TILCode;
begin
  Result := icQueryType;
end;

function TILQueryType.Text: string;
begin
  Result := 'QTYPE ' + inherited;
end;

{ TILMemMove }

function TILMemMove.GetLine: Integer;
begin
  Result := FSrcArr.Line;
end;

function TILMemMove.ILCode: TILCode;
begin
  Result := icMemMove;
end;

function TILMemMove.Text: string;
begin
  Result := 'MEMMOVE ' + CondAsText + ' ' + ExpressionName(FSrcArr) + ', ' +
                                            ExpressionName(FSrcIdx) + ', ' +
                                            ExpressionName(FDstArr) + ', ' +
                                            ExpressionName(FDstIdx) + ', ' +
                                            ExpressionName(FCnt);
end;

procedure TILMemMove.Write(Proc: TIDProcedure; Stream: TStream);
begin
  inherited;
  WriteILArgument(Proc, Stream, FSrcArr);
  WriteILArgument(Proc, Stream, FSrcIdx);
  WriteILArgument(Proc, Stream, FDstArr);
  WriteILArgument(Proc, Stream, FDstIdx);
  WriteILArgument(Proc, Stream, FCnt);
end;

{ TILLDMethod }

function TILLDMethod.ILCode: TILCode;
begin
  if Assigned(FBase) then
    Result := icLoadMethod
  else
    Result := icLoadSelfMethod;
end;

{ TILConvert }

function TILConvert.ILCode: TILCode;
begin
  Result := icCovert;
end;

function TILConvert.Text: string;
begin
  Result := 'CONVERT ' + inherited;
end;

{ TILMemSet }

function TILMemSet.ILCode: TILCode;
begin
  Result := icMemSet;
end;

function TILMemSet.Text: string;
begin
  Result := 'MEMSET ' + inherited;
end;

{ TILRefCount }

function TILRefCount.ILCode: TILCode;
begin
  Result := icRefCount;
end;

function TILRefCount.Text: string;
begin
  Result := 'REFCNT ' + inherited;
end;

{ TILNow }

function TILNow.ILCode: TILCode;
begin
  Result := icNow;

end;

function TILNow.Text: string;
begin
  Result := 'NOW ' + inherited;
end;

{ TILMacro }

function TILFMacro.ILCode: TILCode;
begin
  Result := icFMacro;
end;

procedure TILFMacro.SwapArguments(const Context: TPIContext);
begin
  inherited;
  FMacroID := TILFMacro(Context.Instruction).MacroID;
end;

function TILFMacro.Text: string;
begin
  Result := 'MACRO ' + GetILMacroName(FMacroID) + ', ' + inherited Text{ + ', ' + GetArgsNames};
end;

procedure TILFMacro.Write(Proc: TIDProcedure; Stream: TStream);
begin
  inherited;
  WriteConstArgument(Stream, FMacroID, False);
  WriteILArguments(Proc, Stream, FArgs, False);
end;

end.










