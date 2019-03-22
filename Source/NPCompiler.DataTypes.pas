unit NPCompiler.DataTypes;

interface

uses SysUtils, classes;

type

{

  Elementary
       |
       |--Ordinal
       |     |
       |     |- Integer
       |     |- Boolean
       |     |- Enumerated
       |     |- Character
       |     |- Subrange
       |
       |-- Sets
       |-- Real
       |-- String
       |-- Variant

   Structured
       |
       |-- Record
       |-- Array

   Referenced
       |
       |--Reference
       |--Class
       |--Interface

}

  TCastResolution = (crDenied, crImplicit, crExplicit);

type

  TDataTypeID =
  (
    dtInt8,
    dtInt16,
    dtInt32,
    dtInt64,
    dtUInt8,
    dtUInt16,
    dtUInt32,
    dtUInt64,
    dtNativeInt,
    dtNativeUInt,
    dtFloat32,
    dtFloat64,
    dtBoolean,      // булевый тип
    dtAnsiChar,     // ansi-символ
    dtChar,         // utf16-символ
    dtAnsiString,   // ansi-строка
    dtString,       // utf16-строка
    dtVariant,      // тип вариант
    dtGuid,         // GUID
    dtPointer,      // указатель (любой)
    dtWeakRef,      // слабая ссылка
    dtGeneric,      // обобщенный тип
    dtRange,        // диаппазоный тип (тип с ограниченным диаппазоном)
    dtEnum,         // перечисление
    dtSet,
    dtStaticArray,  // статический массив
    dtDynArray,     // динамический массив
    dtOpenArray,
    dtProcType,     // процедурная ссылка
    dtRecord,       // структура
    dtClass,        // класс
    dtClassOf,      // метакласс
    dtInterface     // интерфейс
  );

const

  // неизвестный тип
  dtUnknown = 128;

  cDataTypeManaged: array [TDataTypeID] of Boolean =
  (
    {dtInt8}        False,
    {dtInt16}       False,
    {dtInt32}       False,
    {dtInt64}       False,
    {dtUint8}       False,
    {dtUint16}      False,
    {dtUint32}      False,
    {dtUint64}      False,
    {dtNativeInt}   False,
    {dtNativeUInt}  False,
    {dtFloat32}     False,
    {dtFloat64}     False,
    {dtBoolean}     False,
    {dtAnsiChar}    False,
    {dtChar}        False,
    {dtAnsiString}  True,
    {dtString}      True,
    {dtVariant}     True,
    {dtGuid}        False,
    {dtPointer}     False,
    {dtWeakRef}     True,
    {dtGeneric}     False,
    {dtRange}       False,
    {dtEnum}        False,
    {dtSet}         False,
    {dtArray}       False,
    {dtDynArray}    True,
    {dtOpenArray}   False,
    {dtProcType}    False,
    {dtRecord}      False,
    {dtClass}       True,
    {dtClassOf}     False,
    {dtInterface}   True
  );


type
  TManagedDataTypeFlag = (
    mtNeedClear,             // необходимо занулить переменную
    mtNeedIncRef,            // тип использует ARC
    dtNeedFinal,             // нужно финализировать (но только для не анонимных переменных) ???
    dtNeedAlwaysFinal        // нужно финализировать (для всех переменных) ??? todo: надо обьединить
  );

  TManagedDataTypeFlags = set of TManagedDataTypeFlag;

const
  cDataTypeManagedFlags: array [TDataTypeID] of TManagedDataTypeFlags =
  (
    {dtInt8}        [],
    {dtInt16}       [],
    {dtInt32}       [],
    {dtInt64}       [],
    {dtUint8}       [],
    {dtUint16}      [],
    {dtUint32}      [],
    {dtUint64}      [],
    {dtNativeInt}   [],
    {dtNativeUInt}  [],
    {dtFloat32}     [],
    {dtFloat64}     [],
    {dtBoolean}     [],
    {dtAnsiChar}    [],
    {dtChar}        [],
    {dtAnsiString}  [mtNeedClear, dtNeedFinal, mtNeedIncRef, dtNeedAlwaysFinal],
    {dtString}      [mtNeedClear, dtNeedFinal, mtNeedIncRef, dtNeedAlwaysFinal],
    {dtVariant}     [mtNeedClear, dtNeedFinal, dtNeedAlwaysFinal],
    {dtGuid}        [],
    {dtPointer}     [],
    {dtWeakRef}     [mtNeedClear, dtNeedFinal, mtNeedIncRef, dtNeedAlwaysFinal],
    {dtGeneric}     [],
    {dtRange}       [],
    {dtEnum}        [],
    {dtSet}         [],
    {dtArray}       [],
    {dtDynArray}    [mtNeedClear, dtNeedFinal, mtNeedIncRef, dtNeedAlwaysFinal],
    {dtOpenArray}   [],
    {dtProcType}    [],
    {dtRecord}      [],
    {dtClass}       [mtNeedClear, dtNeedFinal, mtNeedIncRef],
    {dtClassOf}     [],
    {dtInterface}   [mtNeedClear, dtNeedFinal, mtNeedIncRef]
  );

  // Таблица совместимости типов, Строка описывает тип [SOURCE], колонка [DESTINATION]
  _ImplicitFactors: array [dtInt8..dtVariant, dtInt8..dtVariant] of integer = (
{-----------|----|-----|-----|-----|-----|------|------|------|------|-------|-------|-------|-------|--------|----|----------|------|-------|}
{           |Int8|Int16|Int32|Int64|UInt8|UInt16|UInt32|UInt64| NInt | NUInt |Float32|Float64|Boolean|AnsiChar|Char|AnsiString|String|Variant|}
{-----------|----|-----|-----|-----|-----|------|------|------|------|-------|-------|-------|-------|--------|----|----------|------|-------|}
{Int8      }( 100,   99,   98,   97,  100,    98,    97,    96,    95,     95,     94,     93,      0,       0,   0,         0,     0,    100),
{-----------|----|-----|-----|-----|-----|------|------|------|------|-------|-------|-------|-------|--------|----|----------|------|-------|}
{Int16     }(  94,  100,   99,   98,   50,   100,    50,    50,    50,     50,     90,     85,      0,       0,   0,         0,     0,     70),
{-----------|----|-----|-----|-----|-----|------|------|------|------|-------|-------|-------|-------|--------|----|----------|------|-------|}
{Int32     }(  96,   99,  100,   99,   95,    97,   100,    98,    97,     97,     96,     95,      0,       0,   0,         0,     0,    100),
{-----------|----|-----|-----|-----|-----|------|------|------|------|-------|-------|-------|-------|--------|----|----------|------|-------|}
{Int64     }(  94,   95,   97,  100,   50,    50,    50,   100,    50,     50,     85,     90,      0,       0,   0,         0,     0,     70),
{-----------|----|-----|-----|-----|-----|------|------|------|------|-------|-------|-------|-------|--------|----|----------|------|-------|}
{UInt8     }( 100,   97,   97,   95,  100,    99,    98,    96,    94,     94,     90,     85,      0,       0,   0,         0,     0,     70),
{-----------|----|-----|-----|-----|-----|------|------|------|------|-------|-------|-------|-------|--------|----|----------|------|-------|}
{UInt16    }(  89,  100,   97,   96,   90,   100,    99,    98,    95,     95,     90,     85,      0,       0,   0,         0,     0,     70),
{-----------|----|-----|-----|-----|-----|------|------|------|------|-------|-------|-------|-------|--------|----|----------|------|-------|}
{UInt32    }(  89,   91,  100,   97,   90,    92,   100,    99,    98,     98,     90,     85,      0,       0,   0,         0,     0,     70),
{-----------|----|-----|-----|-----|-----|------|------|------|------|-------|-------|-------|-------|--------|----|----------|------|-------|}
{UInt64    }(  89,   91,   94,  100,   90,    92,    95,   100,    99,     99,     85,     90,      0,       0,   0,         0,     0,     70),
{-----------|----|-----|-----|-----|-----|------|------|------|------|-------|-------|-------|-------|--------|----|----------|------|-------|}
{NativeInt }(  96,   99,  100,   99,   95,    97,   100,    98,   100,    100,     96,     95,      0,       0,   0,         0,     0,    100),
{-----------|----|-----|-----|-----|-----|------|------|------|------|-------|-------|-------|-------|--------|----|----------|------|-------|}
{NativeUInt}(  89,   91,  100,   97,   90,    92,   100,    99,   100,    100,     90,     85,      0,       0,   0,         0,     0,     70),
{-----------|----|-----|-----|-----|-----|------|------|------|------|-------|-------|-------|-------|--------|----|----------|------|-------|}
{Float32   }(   0,    0,    0,    0,    0,     0,     0,     0,     0,      0,    100,     99,      0,       0,   0,         0,     0,     70),
{-----------|----|-----|-----|-----|-----|------|------|------|------|-------|-------|-------|-------|--------|----|----------|------|-------|}
{Float64   }(   0,    0,    0,    0,    0,     0,     0,     0,     0,      0,     99,    100,      0,       0,   0,         0,     0,     70),
{-----------|----|-----|-----|-----|-----|------|------|------|------|-------|-------|-------|-------|--------|----|----------|------|-------|}
{Boolean   }(   0,    0,    0,    0,    0,     0,     0,     0,     0,      0,      0,      0,    100,       0,   0,         0,     0,     70),
{-----------|----|-----|-----|-----|-----|------|------|------|------|-------|-------|-------|-------|--------|----|----------|------|-------|}
{AnsiChar  }(   0,    0,    0,    0,    0,     0,     0,     0,     0,      0,      0,      0,      0,     100,   0,         0,     0,     70),
{-----------|----|-----|-----|-----|-----|------|------|------|------|-------|-------|-------|-------|--------|----|----------|------|-------|}
{Char      }(   0,    0,    0,    0,    0,     0,     0,     0,     0,      0,      0,      0,      0,       0, 100,         0,     0,     70),
{-----------|----|-----|-----|-----|-----|------|------|------|------|-------|-------|-------|-------|--------|----|----------|------|-------|}
{AnsiString}(   0,    0,    0,    0,    0,     0,     0,     0,     0,      0,      0,      0,      0,       0,   0,       100,     0,     70),
{-----------|----|-----|-----|-----|-----|------|------|------|------|-------|-------|-------|-------|--------|----|----------|------|-------|}
{String    }(   0,    0,    0,    0,    0,     0,     0,     0,     0,      0,      0,      0,      0,       0,   0,         0,   100,     70),
{-----------|----|-----|-----|-----|-----|------|------|------|------|-------|-------|-------|-------|--------|----|----------|------|-------|}
{Variant   }(  70,   70,   70,   70,   70,    70,    70,    70,    70,     70,     70,     70,     70,      70,  70,        70,    70,    100)
{-----------|----|-----|-----|-----|-----|------|------|------|------|-------|-------|-------|-------|--------|----|----------|------|-------|});


  // Таблица совместимости типов по потере данных, Строка описывает тип [SOURCE], колонка [DESTINATION] (0 - означает что потери данных нет)
  _DataLossFactors: array [dtInt8..dtVariant, dtInt8..dtVariant] of integer = (
{-----------|----|-----|-----|-----|-----|------|------|------|-----|------|-------|-------|-------|--------|----|----------|------|-------|}
{           |Int8|Int16|Int32|Int64|UInt8|UInt16|UInt32|UInt64| NInt| NUInt|Float32|Float64|Boolean|AnsiChar|Char|AnsiString|String|Variant|}
{-----------|----|-----|-----|-----|-----|------|------|------|-----|------|-------|-------|-------|--------|----|----------|------|-------|}
{Int8      }(   0,    0,    0,    0,    1,     1,     1,     1,    0,     1,      0,      0,      0,       0,   0,         0,     0,      0),
{-----------|----|-----|-----|-----|-----|------|------|------|-----|------|-------|-------|-------|--------|----|----------|------|-------|}
{Int16     }(   1,    0,    0,    0,    1,     1,     1,     1,    0,     1,      0,      0,      0,       0,   0,         0,     0,      0),
{-----------|----|-----|-----|-----|-----|------|------|------|-----|------|-------|-------|-------|--------|----|----------|------|-------|}
{Int32     }(   1,    1,    0,    0,    1,     1,     1,     1,    0,     1,      0,      0,      0,       0,   0,         0,     0,      0),
{-----------|----|-----|-----|-----|-----|------|------|------|-----|------|-------|-------|-------|--------|----|----------|------|-------|}
{Int64     }(   1,    1,    1,    0,    1,     1,     1,     1,    1,     1,      1,      0,      0,       0,   0,         0,     0,      0),
{-----------|----|-----|-----|-----|-----|------|------|------|-----|------|-------|-------|-------|--------|----|----------|------|-------|}
{UInt8     }(   1,    0,    0,    0,    0,     0,     0,     0,    0,     0,      0,      0,      0,       0,   0,         0,     0,      0),
{-----------|----|-----|-----|-----|-----|------|------|------|-----|------|-------|-------|-------|--------|----|----------|------|-------|}
{UInt16    }(   1,    1,    0,    0,    1,     0,     0,     0,    0,     0,      0,      0,      0,       0,   0,         0,     0,      0),
{-----------|----|-----|-----|-----|-----|------|------|------|-----|------|-------|-------|-------|--------|----|----------|------|-------|}
{UInt32    }(   1,    1,    1,    0,    1,     1,     0,     0,    1,     0,      0,      0,      0,       0,   0,         0,     0,      0),
{-----------|----|-----|-----|-----|-----|------|------|------|-----|------|-------|-------|-------|--------|----|----------|------|-------|}
{UInt64    }(   1,    1,    1,    1,    1,     1,     1,     0,    1,     1,      1,      0,      0,       0,   0,         0,     0,      0),
{-----------|----|-----|-----|-----|-----|------|------|------|-----|------|-------|-------|-------|--------|----|----------|------|-------|}
{NativeInt }(   1,    1,    0,    0,    1,     1,     1,     1,    0,     1,      0,      0,      0,       0,   0,         0,     0,      0),
{-----------|----|-----|-----|-----|-----|------|------|------|-----|------|-------|-------|-------|--------|----|----------|------|-------|}
{NativeUInt}(   1,    1,    1,    0,    1,     1,     0,     0,    1,     0,      0,      0,      0,       0,   0,         0,     0,      0),
{-----------|----|-----|-----|-----|-----|------|------|------|-----|------|-------|-------|-------|--------|----|----------|------|-------|}
{Float32   }(   0,    0,    0,    0,    0,     0,     0,     0,    0,     0,      0,      0,      0,       0,   0,         0,     0,      0),
{-----------|----|-----|-----|-----|-----|------|------|------|-----|------|-------|-------|-------|--------|----|----------|------|-------|}
{Float64   }(   0,    0,    0,    0,    0,     0,     0,     0,    0,     0,      1,      0,      0,       0,   0,         0,     0,      0),
{-----------|----|-----|-----|-----|-----|------|------|------|-----|------|-------|-------|-------|--------|----|----------|------|-------|}
{Boolean   }(   0,    0,    0,    0,    0,     0,     0,     0,    0,     0,      0,      0,      0,       0,   0,         0,     0,      0),
{-----------|----|-----|-----|-----|-----|------|------|------|-----|------|-------|-------|-------|--------|----|----------|------|-------|}
{AnsiChar  }(   0,    0,    0,    0,    0,     0,     0,     0,    0,     0,      0,      0,      0,       0,   0,         0,     0,      0),
{-----------|----|-----|-----|-----|-----|------|------|------|-----|------|-------|-------|-------|--------|----|----------|------|-------|}
{Char      }(   0,    0,    0,    0,    0,     0,     0,     0,    0,     0,      0,      0,      0,       1,   0,         1,     0,      0),
{-----------|----|-----|-----|-----|-----|------|------|------|-----|------|-------|-------|-------|--------|----|----------|------|-------|}
{AnsiString}(   0,    0,    0,    0,    0,     0,     0,     0,    0,     0,      0,      0,      0,       0,   0,         0,     0,      0),
{-----------|----|-----|-----|-----|-----|------|------|------|-----|------|-------|-------|-------|--------|----|----------|------|-------|}
{String    }(   0,    0,    0,    0,    0,     0,     0,     0,    0,     0,      0,      0,      0,       0,   0,         1,     0,      0),
{-----------|----|-----|-----|-----|-----|------|------|------|-----|------|-------|-------|-------|--------|----|----------|------|-------|}
{Variant   }(   1,    1,    1,    1,    1,     1,     1,     1,    1,     1,      1,      1,      1,       1,   1,         1,     1,      0)
{-----------|----|-----|-----|-----|-----|------|------|------|-----|------|-------|-------|-------|--------|----|----------|------|-------|});

function ImplicitFactor(const Source, Destination: TDataTypeID): Integer; inline;
function DataLossFactor(const Source, Destination: TDataTypeID): Integer; inline;
function GetDataTypeName(DataType: TDataTypeID): string; inline;
function GetDataTypeSize(DataType: TDataTypeID): Integer; inline;

function IsDataTypeReferenced(DataType: TDataTypeID): Boolean; inline;

function ImplicitFactor2(const Source, Destination: TDataTypeID): Integer; inline;

const
  cDataTypeNames: array [TDataTypeID] of string =
  (
    {dtInt8}        'Int8',
    {dtInt16}       'Int16',
    {dtInt32}       'Int32',
    {dtInt64}       'Int64',
    {dtUInt8}       'UInt8',
    {dtUInt16}      'UInt16',
    {dtUInt32}      'UInt32',
    {dtUInt64}      'UInt64',
    {dtNativeInt}   'NativeInt',
    {dtNativeUInt}  'NativeUInt',
    {dtFloat32}     'Float32',
    {dtFloat64}     'Float64',
    {dtBoolean}     'Boolean',
    {dtAnsiChar}    'AnsiChar',
    {dtChar}        'Char',
    {dtAnsiString}  'AnsiString',
    {dtString}      'String',
    {dtVariant}     'Variant',
    {dtGuid}        'Guid',
    {dtPointer}     'Pointer',
    {dtWeakRef}     'WeakRef',
    {dtGeneric}     'Generic',
    {dtRange}       'Range',
    {dtEnum}        'Enum',
    {dtSet}         'Set',
    {dtArray}       'Array',
    {dtDynArray}    'DynArray',
    {dtOpenArray}   'OpenArray',
    {dtProcType}    'ProcType',
    {dtRecord}      'Record',
    {dtClass}       'Class',
    {dtClassOf}     'ClassOf',
    {dtInterface}   'Interface'
  );

  cDataTypeISReferenced: array [TDataTypeID] of Boolean =
  (
    {dtInt8}        False,
    {dtInt16}       False,
    {dtInt32}       False,
    {dtInt64}       False,
    {dtUint8}       False,
    {dtUint16}      False,
    {dtUint32}      False,
    {dtUint64}      False,
    {dtNativeInt}   False,
    {dtNativeUInt}  False,
    {dtFloat32}     False,
    {dtFloat64}     False,
    {dtBoolean}     False,
    {dtAnsiChar}    False,
    {dtChar}        False,
    {dtAnsiString}  True,
    {dtString}      True,
    {dtVariant}     True,
    {dtGuid}        False,
    {dtPointer}     True,
    {dtWeakRef}     True,
    {dtGeneric}     False,
    {dtRange}       False,
    {dtEnum}        False,
    {dtSet}         False,
    {dtArray}       False,
    {dtDynArray}    True,
    {dtOpenArray}   True,
    {dtProcType}    False,
    {dtRecord}      False,
    {dtClass}       True,
    {dtClassOf}     True,
    {dtInterface}   True
  );

  cDataTypeSizes: array [TDataTypeID] of Integer = (
    {dtInt8}        1,
    {dtInt16}       2,
    {dtInt32}       4,
    {dtInt64}       8,
    {dtUInt8}       1,
    {dtUInt16}      2,
    {dtUInt32}      4,
    {dtUInt64}      8,
    {dtNativeInt}   SizeOf(Pointer),
    {dtNativeUInt}  SizeOf(Pointer),
    {dtFloat32}     4,
    {dtFloat64}     8,
    {dtBoolean}     1,
    {dtAnsiChar}    1,
    {dtChar}        2, // UTF16
    {dtAnsiString}  SizeOf(Pointer), // refernce type
    {dtString}      SizeOf(Pointer), // refernce type
    {dtVariant}     16,
    {dtGuid}        SizeOf(TGUID),
    {dtPointer}     SizeOf(Pointer),
    {dtWeakRef}     SizeOf(Pointer),
    {dtGeneric}     0,
    {dtRange}       0,
    {dtEnum}        0,
    {dtSet}         0,
    {dtArray}       0,
    {dtDynArray}    SizeOf(Pointer),
    {dtOpenArray}   0,
    {dtProcType}    0,
    {dtRecord}      0,
    {dtClass}       SizeOf(Pointer),
    {dtClassOf}     SizeOf(Pointer),
    {dtInterface}   SizeOf(Pointer)
  );

implementation

function IsDataTypeReferenced(DataType: TDataTypeID): Boolean;
begin
  Result := cDataTypeISReferenced[DataType];
end;

function ImplicitFactor(const Source, Destination: TDataTypeID): Integer;
begin
  if (Source <= dtVariant) and (Destination <= dtVariant) then
    Result := _ImplicitFactors[Source, Destination]
  else
    Result := 100;
end;

function DataLossFactor(const Source, Destination: TDataTypeID): Integer;
begin
  if (Source <= dtVariant) and (Destination <= dtVariant) then
    Result := _DataLossFactors[Source, Destination]
  else
    Result := 0;
end;

function ImplicitFactor2(const Source, Destination: TDataTypeID): Integer; inline;
var
  DLF, ILF: Integer;
begin
  if (Source <= dtPointer) and (Destination <= dtPointer) then
  begin
    DLF := 1 - _DataLossFactors[Source, Destination];
    ILF := _ImplicitFactors[Source, Destination];
    Result := (DLF shl 16) + ILF;
  end else
    Result := 0;
end;

function GetDataTypeName(DataType: TDataTypeID): string;
begin
  Result := cDataTypeNames[DataType];
end;

function GetDataTypeSize(DataType: TDataTypeID): Integer; inline;
begin
  Result := cDataTypeSizes[DataType];
end;


initialization

finalization
end.
