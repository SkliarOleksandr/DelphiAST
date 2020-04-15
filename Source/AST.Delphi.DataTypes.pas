unit AST.Delphi.DataTypes;

interface

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
    dtShortString,  // ansi-строка
    dtAnsiString,   // ansi-строка
    dtString,       // unicode string
    dtWideString,   // WideString
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

    TASTArgMatchRate = Integer;

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
    {dtShortString} True,
    {dtAnsiString}  True,
    {dtString}      True,
    {dtWideString}  True,
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

function GetDataTypeName(DataType: TDataTypeID): string; inline;
function GetDataTypeSize(DataType: TDataTypeID): Integer; inline;

function IsDataTypeReferenced(DataType: TDataTypeID): Boolean; inline;

function ImplicitFactor2(const Source, Destination: TDataTypeID): Integer; inline;

function GetImplicitRate(const Src, Dst: TDataTypeID; out DataLoss: Boolean): TASTArgMatchRate;

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
    {dtShortString} 'ShortString',
    {dtAnsiString}  'AnsiString',
    {dtString}      'String',
    {dtWideString}  'WideString',
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
    {dtShortString} True,
    {dtAnsiString}  True,
    {dtString}      True,
    {dtWideString}  True,
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
    {dtProcType}    True,
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
    {dtShortString} SizeOf(Pointer), // refernce type
    {dtAnsiString}  SizeOf(Pointer), // refernce type
    {dtString}      SizeOf(Pointer), // refernce type
    {dtWideString}  SizeOf(Pointer), // refernce type
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

uses SysUtils;

var
  implicitRates: array [dtInt8..dtVariant, dtInt8..dtVariant] of Integer;

function IsDataTypeReferenced(DataType: TDataTypeID): Boolean;
begin
  Result := cDataTypeISReferenced[DataType];
end;

function ImplicitFactor2(const Source, Destination: TDataTypeID): Integer; inline;
var
  DLF, ILF: Integer;
  IsDataLoss: Boolean;
begin
  if (Source <= dtPointer) and (Destination <= dtPointer) then
  begin
    // todo
    ILF := GetImplicitRate(Source, Destination, IsDataLoss);
    DLF := ord(not IsDataLoss);
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

function GetImplicitRate(const Src, Dst: TDataTypeID; out DataLoss: Boolean): TASTArgMatchRate;
begin
  if (Src <= dtVariant) and (Dst <= dtVariant) then
  begin
    var RValue := implicitRates[Src, Dst];
    DataLoss := RValue < 0;
    Exit(Abs(RValue));
  end;
  Result := 0;
end;

procedure Rate(Src: TDataTypeID; Dsts: array of TDataTypeID);
begin
  for var i := 0 to Length(Dsts) - 1 do
  begin
    if implicitRates[Src, Dsts[i]] <> 0 then
      raise Exception.Create('Rate is already set');

    var RateValue: Integer := Length(Dsts) - i;
    implicitRates[Src, Dsts[i]] := RateValue;
  end;
end;

procedure RateWDL(Src: TDataTypeID; Dsts: array of TDataTypeID);
begin
  for var i := 0 to Length(Dsts) - 1 do
  begin
    if implicitRates[Src, Dsts[i]] <> 0 then
      raise Exception.Create('Rate is already set');

    var RateValue: Integer := - (Length(Dsts) - i);
    implicitRates[Src, Dsts[i]] := RateValue;
  end;
end;

procedure InitImplicitRates;
begin
  FillChar(implicitRates, Sizeof(implicitRates), #0);
  // Int8 ///////////////////////////////////////////
  Rate(dtInt8, [dtInt8, dtInt16, dtInt32, dtInt64, dtFloat32, dtFloat64, dtVariant]);
  RateWDL(dtInt8, [dtUInt16, dtUInt32, dtUInt64, dtUInt8]);
  // Int16 //////////////////////////////////////////
  Rate(dtInt16, [dtInt16, dtInt32, dtInt64, dtFloat32, dtFloat64, dtVariant]);
  RateWDL(dtInt16, [dtUInt32, dtUInt64, dtUInt16, dtInt8, dtUInt8]);
  // Int32 //////////////////////////////////////////
  Rate(dtInt32, [dtInt32, dtInt64, dtFloat64, dtVariant]);
  RateWDL(dtInt32, [dtUInt32, dtFloat32, dtUInt64, dtInt16, dtUInt16, dtInt8, dtUInt8]);
  // Int64 //////////////////////////////////////////
  Rate(dtInt64, [dtInt64, dtVariant]);
  RateWDL(dtInt64, [dtUInt64, dtFloat64, dtInt32, dtFloat32, dtUInt32, dtInt16, dtUInt16, dtInt8, dtUInt8]);
  // UInt8 ///////////////////////////////////////////
  Rate(dtUInt8, [dtUInt8, dtUInt16, dtInt16, dtInt32, dtUInt32, dtInt64, dtUInt64, dtFloat32, dtFloat64, dtVariant]);
  RateWDL(dtUInt8, [dtInt8]);
  // UInt16 //////////////////////////////////////////
  Rate(dtUInt16, [dtUInt16, dtUInt32, dtInt32, dtInt64, dtUInt64, dtFloat32, dtFloat64, dtVariant]);
  RateWDL(dtUInt16, [dtInt16, dtUInt8, dtInt8]);
  // UInt32 //////////////////////////////////////////
  Rate(dtUInt32, [dtUInt32, dtInt64, dtUInt64, dtFloat64, dtVariant]);
  RateWDL(dtUInt32, [dtInt32, dtFloat32, dtUInt16, dtInt16, dtUInt8, dtInt8]);
  // UInt64 //////////////////////////////////////////
  Rate(dtUInt64, [dtUInt64, dtVariant]);
  RateWDL(dtUInt64, [dtInt64, dtFloat64, dtInt32, dtUInt32, dtFloat32, dtInt16, dtUInt16, dtInt8, dtUInt8]);
  // Float32 /////////////////////////////////////////
  Rate(dtFloat32, [dtFloat32, dtFloat64, dtVariant]);
  // Float64 /////////////////////////////////////////
  Rate(dtFloat64, [dtFloat64, dtVariant]);
  RateWDL(dtFloat64, [dtFloat32]);
  // Boolean /////////////////////////////////////////
  Rate(dtBoolean, [dtBoolean, dtVariant]);
  // AnsiChar /////////////////////////////////////////
  Rate(dtAnsiChar, [dtAnsiChar, dtVariant]);
  // Char /////////////////////////////////////////
  Rate(dtChar, [dtChar, dtVariant]);
  // ShortString /////////////////////////////////////////
  Rate(dtShortString, [dtShortString, dtAnsiString, dtVariant, dtString, dtWideString]);
  // AnsiString /////////////////////////////////////////
  Rate(dtAnsiString, [dtAnsiString, dtVariant, dtString, dtWideString]);
  // String /////////////////////////////////////////
  Rate(dtString, [dtString, dtWideString, dtVariant]);
  RateWDL(dtString, [dtAnsiString]);
  // WideString /////////////////////////////////////////
  Rate(dtWideString, [dtWideString, dtString, dtVariant]);
  // Variant /////////////////////////////////////////
  Rate(dtVariant, [dtVariant]); // todo
end;


initialization
  InitImplicitRates();

finalization

end.
