unit IL.Types;

{$I compilers.inc}

interface

type

  TILCondition = (cNone, cEqual, cNotEqual, cGreater, cGreaterOrEqual, cLess, cLessOrEqual, cZero, cNonZero);


type

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

  TILARG_SCOPE = (
    ARG_SCOPE_LOCAL  = 0,  // аргумент - локальный элемент для данной процедуры (локальная переменная/параметр/тип)
    ARG_SCOPE_GLOBAL = 1,  // аргумент - глобальный элемент для данной процедуры (глобальная переменная/процедура/тип)
    ARG_SCOPE_STRUCT = 2,  // аргумент - член структуры (поле/метод/тип)
    ARG_SCOPE_UNIT   = 3   // аргумент - чден другого модуля (следом идет индекс модуля))
  );

  TILProcFlag = (ProcNormal, ProcExported, ProcImported);

const
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
  function GetILConditionSymbol(Condition: TILCondition): string;

implementation

uses SysUtils;

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

initialization

end.
