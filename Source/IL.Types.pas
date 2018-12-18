unit IL.Types;

{$I compilers.inc}

interface

type

  {коды инструкций IL-кода}
  TILCode = (
    icNope,           // нет операции
    icRet,            // возврат из процедуры
    icSetBool,        // X := iif(condition, true, false)
    icNot,            // X := not Y
    icNeg,            // X := neg Y
    icJmp,            // переход внутри процедуры

    icGetBit,         // получение бита в битовом наборе
    icSetBit,         // установка бита в битовом наборе

    icMove,           // присвоение X := Y
    icMoveZero,       // зануление X := 0
    icExChange,
    icByteSwap,
    icCmp,            // cравнение X - Y
    icCmpZero,        // сравнение с нулем X - 0

    icCmpJmp,         // cравнение X - Y и переход
    icCmpZeroJmp,     // сравнение с нулем X - 0 и переход

    icTest,           // сравнение X and Y
    icCovert,         // конвертирование типов
    icCheckBound,     // проверка границ стат./дин. массивов  [тип массива/ссылка на массив или строку, переменная-индекс]
    icCheckRange,     // проверка границ значения [тип, переменная]

    icAdd,            // сложение Z := X + Y
    icSub,            // вычитание Z := X - Y
    icMul,            // умножение Z := X * Y
    icDiv,            // деление Z := X / Y
    icIntDiv,         // целочисленное деление Z := X div Y
    icModDiv,         // остаток от целочисленного деления Z := X mod Y
    icAnd,            // Z := X and Y
    icOr,             // Z := X or Y
    icXor,            // Z := X xor Y
    icShl,            // Z := X shl Y
    icShr,            // Z := X shr Y
    icRol,            // Z := X rol Y
    icRor,            // Z := X ror Y

    icAdd2,           // X := X + Y
    icSub2,           // X := X - Y
    icMul2,           // X := X * Y
    icDiv2,           // X := X / Y
    icIntDiv2,        // X := X div Y
    icModDiv2,        // X := X mod Y
    icAnd2,           // X := X and Y
    icOr2,            // X := X or Y
    icXor2,           // X := X xor Y
    icShl2,           // X := X shl Y
    icShr2,           // X := X shr Y
    icRol2,           // X := X rol Y
    icRor2,           // X := X ror Y

    icInc,            // X := X + 1
    icDec,            // X := X - 1

    icLea,            // взятие адреса X := @Y + offset
    icLea2,           // взятие адреса X := @Y

    icReadDRef,       // чтение из переменной с разименованием X := Y^
    icWriteDRef,      // запись в переменную с разименованием  X^ := Y

    icUnique,         // Инструкция создающая уникальную ссылку дин.массива/строки чтобы обеспечить его/ее локальную копию (copy-on-write)
    icGetPtr,         // Инструкция получения ссылки элемент (X := A.B.C или X := A[1][2])
    icGetSelfPtr,     // Инструкция получения ссылки на поле/цепочку структуры в методе которой есть эта инструкция

    icLoadMethod,     // Инструкция получения ссылки на метод
    icLoadSelfMethod, // Инструкция получения ссылки на собственный метод

    icInhtCall,       // инструкция вызова inherited метода
    icProcCall,       // инструкция вызова процедур/функций
    icVirtCall,       // инструкция вызова виртуального метода
    icNearCall,       // инструкция ближнего вызова (внутри процедуры)
    icUSafeCall,      // инструкция небезопасного вызова по указателю

    icDNewObj,        // создание инстанса класса в динамической памяти без вызова конструктора (или его нет или он был заинлайнен)
    icSNewObj,        // создание инстанса класса в статической памяти

    icMemGet,         // выделение динамической памяти (по явному размеру или по ссылке на тип)
    icMemFree,        // освобождение динамической памяти

    icArrayDAlloc,    // аллокация памяти в динамической памяти (также установка размера дин. массива)
    icArrayRAlloc,    // реаллокация памяти в динамической памяти (с копированием предыдущего содержимого)
    icArraySAlloc,    // аллокация памяти на стеке
    icArrayLength,    // Инструкция Length
    icArrayCopy,      // Инструкция Copy
    icMemMove,        // Инструкция Move
    icMemSet,         // Инструкция MemSet(Dst, SetByte)

    icTypeInfo,       // инструкция TypeInfo
    icQueryType,      // оператор IS или AS (в зависимости от типа результата, если булев - то IS иначе AS), QueryInterface

    icInit,           // инициализация managed - переменной (присвоение nil/null без вызова DecRef)
    //icFinal,          // финализация managed - переменной
    icIncRef,         // инструкция захвата управляемых типов данных, увеличивает счетчик ссылок на 1
    icDecRef,         // инструкция освобождения управляемых типов данных, уменьшает счетчик ссылок на 1
    icDecRefFinal,    // инструкция декремента счетчика ссылки с вызовом финализатора
    icRefCount,       // инструкция получения значения счетчика ссылок
    icNow,            // инструкция получает системное время

    icWeakRef,        // инструкция получения слабой ссылки из сильной
    icStrongRef,      // инструкция получения сильной ссылки из слабой

    icTryBegin,       // Начало секции try...
    icTryEnd,         // конец секции try...
    icTryNext,        // конец текущей секции TRY... и начало следующей
    icTryCallHandler, // инструкция вызова оброботчика
    icEThrow,         // инструкция выброса исключения
    icFMacro,         // макро функция [имя макро-функции, аргументы...]
    icPlatform        // инструкция-контейнер платформо-зависимых инструкций
  ); //  < 70 команд


  // формат кода IL-инструкции - один байт
  // максимум допустимо 128 команд
  // в страшем бите кода IL-инструкции хранится признак  наличия дополнительного байта условия выполнения инструкции


// бинарынй формат динамических строк/массивов
// [offset: -8][offset: -4][offset: 0...n]
// [  refcount][     count][   data bytes]

const
  ILCODE_HAS_EXTENSION = 128; // включенный старший бит в байте ILCode показывает есть ли дополнительный байт EXTENSION

  IL_MACRONAME_GETCALLSTACK = 'getcallstack';
  IL_MACRONAME_GETCURUNIT = 'getcurrentunit';
  IL_MACRONAME_GETUNITSLIST = 'getunitslist';

  IL_MACROID_UNKNOWN = $FFFFFFFF;
  IL_MACROID_GETCALLSTACK = 1;
  IL_MACROID_GETCURUNIT = 2;
  IL_MACROID_GETUNITSLIST = 3;

type
  TILMacroID = UInt32;

  TInstructionPrefix = (ipLock, ipOverflowCheck);

  TILCondition = (cNone, cEqual, cNotEqual, cGreater, cGreaterOrEqual, cLess, cLessOrEqual, cZero, cNonZero);

  { EXTENSION BYTE MAP                                      }
  {=========================================================}
  {| BIT7 | BIT6 | BIT5 | BIT4 | BIT3 | BIT2 | BIT1 | BIT0 |}
  {=========================================================}
  {|      |      |      |      | COND | COND | COND | COND |}
  {=========================================================}
  { COND - условие исполнения инструкции TILCondition       }

type

  (* ФОРМАТ ПРЕФИКСА АРГУМЕНТА IL-ИНСТРУКЦИИ *)

  {=========================================================}
  {| BIT7 | BIT6 | BIT5 | BIT4 | BIT3 | BIT2 | BIT1 | BIT0 |}
  {=========================================================}
  {| IMMC | NEXT | RSRV | ACLS | ACLS | ACLS | ASCP | ASCP |}
  {=========================================================}

  { IMMC - непосредственная константа, биты 0..5 - TDataTypeID }
  { NEXT - признак наличия следующего аргумента в цепочке (при доступе к элементам массивов и стуктур)}
  { RSRV - резерв
  { ACLS - класс аргумента (переменная, процедура, тип, табличная константа, модуль)}
  { ASCP - область видимости аругмента (локальная/глобальная/структура)}


  {класс аругмента (биты 2..4)}
  TILARG_CLASS = (
    ARG_VAR    = 0,  // аргумент - переменная (локальная/глобальная/поле/парамерт - зависит от TILArgumentScope)
    ARG_PROC   = 1,  // аргумент - процедура (глобальная/вложенная - зависит от TILArgumentScope)
    ARG_METHOD = 2,  // аргумент - метод
    ARG_TYPE   = 3,  // аргумент - тип
    ARG_SCONST = 4,  // аргумент - табличная строковая(UTF8) константа (общий список для пакета)
    ARG_TCONST = 5,  // аргумент - табличная константа(спиок для каждого модуля)
    ARG_UNIT   = 6,  // аргумент - модуль
    ARG_SIZEOF = 7,  // аргумент - специальная константа равная размеру типа, который следует следующим байтом
    ARG_CONST        // аргумент - непосредственная константа
  );

  {область видимости аругмента (биты 0..1)}
  TILARG_SCOPE = (
    ARG_SCOPE_LOCAL  = 0,  // аргумент - локальный элемент для данной процедуры (локальная переменная/параметр/тип)
    ARG_SCOPE_GLOBAL = 1,  // аргумент - глобальный элемент для данной процедуры (глобальная переменная/процедура/тип)
    ARG_SCOPE_STRUCT = 2,  // аргумент - член структуры (поле/метод/тип)
    ARG_SCOPE_UNIT   = 3   // аргумент - чден другого модуля (следом идет индекс модуля))
  );

  TILProcFlag = (ProcNormal, ProcExported, ProcImported);

const
  { вспомогательные флаги для префикса аргумента }
  ARG_IMMCONST = 128; // аргумент - непосредственная константа
  ARG_NEXT = 64;      // аргумент - не последний элемент цепочки

  { префикс переменной/поля/параметра }
  ILVAR_HASINDEX   = 1;  // переопределен индекс переменной, absolute
  ILVAR_REFERENCE  = 2;  // ссылка
  ILVAR_HASVALUE   = 4;  // переменная имеет значение (глобальная)
  ILVAR_UNITTYPE   = 8;  // тип указан с индексом модуля
  ILVAR_CONSTPARAM = 16; // константный параметр (необходим для автомат. упр. памятью)
  ILVAR_TEMP = 32;       // временная переменная (необходим для трансляции регистровой машины в стековую)

  { Флаги декларации процедуры }
  ILPROC_HASRESULT = 1;          // есть ли результат
  ILPROC_HASLOCALPROCS = 2;      // есть ли вложенные процедуры
  ILPROC_HASSELFPTR = 4;         // есть ли доп. параметр self
  ILPROC_CONSTRUCTOR = 8;        // является конструктором
  ILPROC_VIRTUAL = 16;           // является ли виртульаным
  ILPROC_EMPTY = 32;             // пустая, нет тела
  ILPROC_IMPORT = 64;            // импортируется
  ILPROC_EXPORT = 128;           // экспортируется

  { Флаги декларации типа }
  ILTYPE_HAS_ANCESTOR = 1;       // для типа определен предкок
  ILTYPE_IMPORT = 2;             // типа импортируется
  ILTYPE_PACKED = 4;             // для типа определен размер выравнивания
  ILTYPE_IMPL_INTERFACES = 8;    // для типа определен список реализованных интерфейсов

  ILARRAY_HAS_INIT = 1;         // для типа определен инициализатор
  ILARRAY_HAS_COPY = 2;         // для типа определена процедура копирования
  ILARRAY_HAS_FINAL = 4;        // для типа определен финализатор



  { Флаги декларации модуля }
  ILUNIT_HASINIT = 1;            // модуль содержит секцию инициализации
  ILUNIT_HASFINAL = 2;           // модуль содержит секцию финализации

  function InverseCondition(Condition: TILCondition): TILCondition; inline;
  function GetILCodeName(Code: TILCode): string;
  function GetILConditionSymbol(Condition: TILCondition): string;
  function GetILMacroID(const MacroName: string): TILMacroID;
  function GetILMacroName(const MacroID: TILMacroID): string;

implementation

uses SysUtils;

const
  ILCODENAMES: array [TILCode] of string = (
    {icNope}              'nop',
    {icRet}               'ret',
    {icSetBool}           'setb',
    {icNot}               'not',
    {icNeg}               'neg',
    {icJmp}               'jmp',

    {icGetBit}            'getbit',
    {icSetBit}            'setbit',

    {icMove}              'mov',
    {icMoveZero}          'movz',
    {icExChange}          'exch',
    {icByteSwap}          'bswp',
    {icCmp}               'cmp',
    {icCmpZero}           'cmpz',
    {icCmpJmp}            'cmpj',
    {icCmpZeroJmp}        'cmpzj',
    {icTest}              'test',
    {icCovert}            'cnvt',
    {icCheckBound}        'chkb',
    {icCheckRange}        'chkr',

    {icAdd}               'add',
    {icSub}               'sub',
    {icMul}               'mul',
    {icDiv}               'div',
    {icIntDiv}            'idiv',
    {icModDiv}            'mdiv',
    {icAnd}               'and',
    {icOr}                'or',
    {icXor}               'xor',
    {icShl}               'shl',
    {icShr}               'shr',
    {icRol}               'rol',
    {icRor}               'ror',

    {icAdd2}              'add',
    {icSub2}              'sub',
    {icMul2}              'mul',
    {icDiv2}              'div',
    {icIntDiv2}           'idiv',
    {icModDiv2}           'mdiv',
    {icAnd2}              'and',
    {icOr2}               'or',
    {icXor2}              'xor',
    {icShl2}              'shl',
    {icShr2}              'shr',
    {icRol2}              'rol',
    {icRor2}              'ror',

    {icInc}               'inc',
    {icDec}               'dec',
    {icLea}               'lea',
    {icLea2}              'lea',

    {icReadDRef}          'rdref',
    {icWriteDRef}         'wdref',

    {icUnique}            'unique',
    {icGetPtr}            'getptr',
    {icGetSelfPtr}        'getsptr',
    {icLoadMethod}        'ldmethod',
    {icLoadSelfMethod}    'ldsmethod',

    {icInheritedCall}     'icall',
    {icProcCall}          'pcall',
    {icVirtCall}          'vcall',
    {icNearCall}          'ncall',
    {icUSafeCall}         'uscall',

    {icDNewObj}           'dnewobj',
    {icSNewObj}           'snewobj',

    {icMemGet}            'memget',
    {icMemFree}           'memfree',

    {icArrayDAlloc}       'arraydalloc',
    {icArrayRAlloc}       'arrayralloc',
    {icArraySAlloc}       'arraysalloc',
    {icArrayLength}       'arraylength',
    {icArrayCopy}         'arraycopy',
    {icMemMove}           'memmove',
    {icMemSet}            'memset',

    {icTypeInfo}          'typeinfo',
    {icQueryType}         'qtype',

    {icInit}              'init',
    //{icFinal}             'final',

    {icIncRef}            'incref',
    {icDecRef}            'decref',
    {icDecRefFinal}       'decreffinal',
    {icRefCount}          'refcnt',
    {icNow}               'now',

    {icWeakRef}           'weakref',
    {icStrongRef}         'strongref',

    {icTryBegin}          'trybegin',
    {icTryEnd}            'tryend',
    {icTryNext}           'trynext',
    {icTryCallHandler}    'trycallhandler',
    {icEThrow}            'ethrow',
    {icFMacro}            'fmacro',
    {icPlatform}          'platform'
  );


function GetILCodeName(Code: TILCode): string;
begin
  Result := ILCODENAMES[Code];
end;

function GetILConditionSymbol(Condition: TILCondition): string;
begin
  case Condition of
    cEqual: Result := '=';
    cNotEqual: Result := '<>';
    cGreater: Result := '>';
    cGreaterOrEqual: Result := '>=';
    cLess: Result := '<';
    cLessOrEqual: Result := '<=';
    cZero: Result := '=0';
    cNonZero: Result := '<>0';
  else
    Result := '';
  end;
end;

function InverseCondition(Condition: TILCondition): TILCondition;
begin
  case Condition of
    cNone: Result := cNone;
    cEqual: Result := cNotEqual;
    cNotEqual: Result := cEqual;
    cGreater: Result := cLessOrEqual;
    cGreaterOrEqual: Result := cLess;
    cLess: Result := cGreaterOrEqual;
    cLessOrEqual: Result := cGreater;
    cZero: Result := cNonZero;
    cNonZero: Result := cZero;
    else
      Result := cNone;
  end;
end;

function GetILMacroID(const MacroName: string): TILMacroID;
var
  Name: string;
begin
  Name := lowercase(MacroName);
  if Name = IL_MACRONAME_GETCALLSTACK then
    Result := IL_MACROID_GETCALLSTACK
  else
  if Name = IL_MACRONAME_GETCURUNIT then
    Result := IL_MACROID_GETCURUNIT
  else
  if Name = IL_MACRONAME_GETUNITSLIST then
    Result := IL_MACROID_GETUNITSLIST
  else
    Result := IL_MACROID_UNKNOWN;
end;

function GetILMacroName(const MacroID: TILMacroID): string;
begin
  case MacroID of
    IL_MACROID_GETCALLSTACK: Result := IL_MACRONAME_GETCALLSTACK;
    IL_MACROID_GETCURUNIT: Result := IL_MACRONAME_GETCURUNIT;
    IL_MACROID_GETUNITSLIST: Result := IL_MACRONAME_GETUNITSLIST;
  else
    Result := '';
  end;
end;

initialization
  {$WARNINGS OFF}
  Assert(Ord(High(TILCode)) <= 127);
  {$WARNINGS ON}

end.
