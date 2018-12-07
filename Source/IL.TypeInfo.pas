unit IL.TypeInfo;

{$i compilers.inc}

interface

uses SysUtils, NPCompiler.DataTypes, TypInfo;

type

  TOffset = NativeUInt;
  TProcType = (ptProcedure, ptFunction);

  TVMObjHeader = packed object
  public
    TypeInfo: Pointer;
    RefCount: NativeInt;
    WeekInfo: Pointer;
    SyncInfo: Pointer;
  end;
  PVMObjHeader = ^TVMObjHeader;

  TRTTIClassID = (
    rttiUnit,
    rttiType,
    rttiProc,
    rttiVar
  );

  //=====================================================
  // COMMON RTTI CLASSES ANCESTOR
  //=====================================================

  TRTTI = packed object(TVMObjHeader)
    FClassID: TRTTIClassID;
    Name: TOffset;            // имя
    ImportLib: TOffset;       // название библиотеки импорта
    ImportName: TOffset;      // название типа в библиотке импорта
  end;

  //=====================================================
  // COMMON TYPE ANCESTOR
  //=====================================================

  TRTTYType = packed object(TRTTI)
  public
    DataTypeID: TDataTypeID;  // ID типа
    TypePacked: Boolean;      // тип упакованный
    DataSize: Integer;        // размер в байтах элемента данного типа
    UnitID: Integer;          // ID модуля
    Index: Integer;           // индекс
  end;
  PRTTIType = ^TRTTYType;

  //=====================================================
  // POINTER TYPE
  //=====================================================

  TRTTIPointer = packed object(TRTTYType)
    RefTypeInfoOffset: TOffset;
  end;
  PRTTIPointer = ^TRTTIPointer;

  //=====================================================
  // ORDINAL TYPE
  //=====================================================

  TRTTIOrdinal = packed object(TRTTYType)
    LoBound: Int64;  // нижняя граница ordinal типа
    HiBound: Int64;  // верхняя граница ordinal типа
    Signed: Boolean;  // знаковый ли тип
  end;
  PRTTIOrdinal = ^TRTTIOrdinal;

  //=====================================================
  // FLOAT TYPE
  //=====================================================

  TRTTIFloat = packed object(TRTTYType)
  end;

  //=====================================================
  // ARRAY TYPE
  //=====================================================

  TRTTIDimensions = array [0..65535] of TOffset;
  PRTTIDimensions = ^TRTTIDimensions;

  TRTTIArray = packed object(TRTTYType)
    ElementTypeInfoOffset: TOffset; // ссылка на тип элемента массива
    DimensionsCount: UInt32;        // кол-во измерений массива
    Dimensions: TOffset;            // информация по измерениям (ссылки на TypeInfo)
  end;
  PRTTIArray = ^TRTTIArray;

  //=====================================================
  // DYNAMIC ARRAY TYPE
  //=====================================================
  TRTTIDynArray = packed object(TRTTIArray)
    Flags: Integer;
    InitProc: TOffset;    // tmp
    FinalUIdx: Integer;   // tmp
    FinalProc: TOffset;   // tmp
  end;
  PRTTIDynArray = ^TRTTIDynArray;

  TRTTISet = TRTTIArray;
  PRTTISet = ^TRTTISet;

  //=====================================================
  // STRUCT FIELD INFO
  //=====================================================

  TCopyOnWriteState = (cowNotNeeded, cowNeeded, cowDone);

  TRTTIField = packed object(TRTTI)
    DataType: TOffset;    // тип поля
    Offset: Integer;      // смещение в стековом фрейме/области глобальных переменных/структуре
    IsReference: Boolean; // это ссылочный параметр
    IsConstant: Boolean;  // это константа
  end;

  PRTTIField = ^TRTTIField;

  TRTTIFields = array of TRTTIField;
  //=====================================================
  // PROCEDURE PARAMETER INFO
  //=====================================================

  TRTTIParameter = TRTTIField;
  PRTTIParameter = PRTTIField;

  TRTTIParams = array [0..65535] of TRTTIParameter;
  PRTTIParams = ^TRTTIParams;

  //=====================================================
  // LOCAL VAR INFO
  //=====================================================
  TRTTILocalVar = packed record
    Name: TOffset;          // имя
    DataType: TOffset;      // тип переменной
    Offset: TOffset;        // смещение на стеке
    IsParam: Boolean;       // это параметр
    IsReference: Boolean;   // это ссылка
  end;
  PRTTILocalVar = ^TRTTILocalVar;

  TRTTILocalVars = array of TRTTILocalVar;

  _TRTTILocalVarsSArray = array [0..65535] of TRTTILocalVar;
  PRTTILocalVarsSArray = ^_TRTTILocalVarsSArray;
  //=====================================================
  // PROCEDURAL TYPE
  //=====================================================

  TCallConvention = (ConvNative, ConvRegister, ConvStdCall, ConvCDecl, ConvFastCall);

  TRTTIProcType = packed object(TRTTYType)
    ProcStatic: Boolean; // является ли обявление типа указателем на статическую процедуру или на метод
    ResultType: TOffset;
    CallConvention: TCallConvention;
    ParamsCount: Integer;
    Params: TOffset;
  end;
  PRTTIProcType = ^TRTTIProcType;

  TILMethod = record
    Proc: Pointer;
    Self: Pointer;
  end;
  PILMethod = ^TILMethod;
  //=====================================================
  // PROCEDURE INFO
  //=====================================================
  TRTTIProcedure = packed object(TRTTI)
    ADDR: TOffset;
    CallConvention: TCallConvention;
    StructInfo: TOffset;          // self
    ExternalMethod: TObject;
    ResultType: TOffset;
    ParamsCount: Integer;
    Params: TOffset;
    StackSize: Integer;
    Flags: Integer;
    VirtualIndex: Int32;
    LocalVars: TOffset;    // TRTTILocalVars
  end;
  PRTTIProcedure = ^TRTTIProcedure;

  TRTTIProcedures = array [0..65535] of TRTTIProcedure;
  PRTTIProcedures = ^TRTTIProcedures;

  TRTTIProcs = array of NativeUInt;
  //=====================================================
  // COMMON STRUCT TYPE
  //=====================================================

  TRTTIStruct = packed object(TRTTYType)
    Ancestor: TOffset;                    // предок
    FieldsCount: Integer;                 // кол-во полей
    Fields: TOffset;                      // поля (array of TRTTIField)
    MethodsCount: Integer;                // кол-во методов
    Methods: TOffset;                     // методы
    TotalMethodsCount: Integer;           // кол-во методов включая всех предков
    TotalFieldsCount: Integer;            // кол-во полей включая всех предков
  end;
  PRTTIStruct = ^TRTTIStruct;

  //=====================================================
  // RECORD TYPE
  //=====================================================

  TRecordFlag = (RecordHasManaged, RecordPacked);
  TRecordFlags = set of TRecordFlag;

  TRTTIRecord = packed object(TRTTIStruct)
    Flags: TRecordFlags;
  end;
  PRTTIRecord = ^TRTTIRecord;

  TVMT = array [0..65535] of NativeUInt;
  PVMT = ^TVMT;

  //=====================================================
  // INTERFACE TYPE
  //=====================================================

  TRTTIInterface = packed object(TRTTIStruct)
    InterfaceID: Integer;
    GUID: TGUID;
  end;
  PRTTIInterface = ^TRTTIInterface;

  TIMTS = array [0..65535] of TOffset;
  PIMTS = ^TIMTS;

  TIMT = array [0..65535] of TOffset;
  PIMT = ^TIMT;
  //=====================================================
  // CLASS TYPE
  //=====================================================

  TRTTIClass = packed object(TRTTIStruct)
    VMTCount: Integer;  // кол-во строк в таблице VMT
    VMT: TOffset;       // указатель на таблицу VMT, таблица содержит все вируальные методы начиная с TObject
    IMTS: TOffset;      // ссылка на таблицу таблиц IMT
  end;
  PRTTIClass = ^TRTTIClass;

  //=====================================================
  // VARIANT TYPE
  //=====================================================

  TRTTIVariant = packed object(TRTTYType)
  end;
  PRTTIVariant = ^TRTTIVariant;

  //=====================================================
  // UNIT INFO
  //=====================================================
  TRTTIUnit = packed object(TRTTI)
    Types: TOffset;             // список типов (RTTI)
    VarsCount: Int32;           // кол-во глобальных переменных
    Vars: TOffset;              // список RTTI глобальных переменных
    Procs: TOffset;             // список RTTI процедур
    ExportProcs: TOffset;       // список экспортируемых процедур модуля (RTTI)
    InitProc: TOffset;          // процедура инициализации (Address)
    FinalProc: TOffset;         // процедура финализации (Address)
  end;
  PRTTIUnit = ^TRTTIUnit;

  TRTTIUnitsSArray = array [0..65535] of TRTTIUnit; // нужен для обхода копирования дин. массивов
  PRTTIUnitsSArray = ^TRTTIUnitsSArray;
  //=====================================================

const
  IL_VMT_FINALIZE = 1;   // индекс финализатора обьекта (код генерируется компилятором)
  IL_VMT_DESTROY = 0;    // индекс деструктора обьекта (содержимое определяется пользователем)


type
  TSystemTypes = packed record
  const
    Count = Ord(TDataTypeID.dtPointer) + 1;
  var
    _Int8: TRTTIOrdinal;
    _Int16: TRTTIOrdinal;
    _Int32: TRTTIOrdinal;
    _Int64: TRTTIOrdinal;
    _UInt8: TRTTIOrdinal;
    _UInt16: TRTTIOrdinal;
    _UInt32: TRTTIOrdinal;
    _UInt64: TRTTIOrdinal;
    _NativeInt: TRTTIOrdinal;
    _NativeUInt: TRTTIOrdinal;
    _Float32: TRTTYType;
    _Float64: TRTTYType;
    _Boolean: TRTTIOrdinal;
    _AnsiChar: TRTTIOrdinal;
    _Char: TRTTIOrdinal;
    _AnsiString: TRTTIArray;
    _String: TRTTIArray;
    _Variant: TRTTYType;
    _GUID: TRTTIStruct;
    _Pointer: TRTTIPointer;
  end;
  PSystemTypes = ^TSystemTypes;

procedure InitSystemTypes(var Types: TSystemTypes);

function GetRTTIStructSize(const DataTypeID: TDataTypeID): Integer;
function ClassInheritsFrom(const ChildClass, AncestorClass: PRTTIClass): Boolean;
function IntfSupportedFrom(const IntfID: Integer; const ClassType: PRTTIClass): Boolean;

implementation

uses NPCompiler.Utils;

function GetRTTIStructSize(const DataTypeID: TDataTypeID): Integer;
begin
  case DataTypeID of
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
    dtBoolean, dtAnsiChar, dtChar: Result := SizeOf(TRTTIOrdinal);
    dtEnum: Result := SizeOf(TRTTIOrdinal);
    dtRange: Result := SizeOf(TRTTIOrdinal);
    dtSet: Result := SizeOf(TRTTIOrdinal);
    dtStaticArray: Result := SizeOf(TRTTIArray);
    dtDynArray, dtOpenArray: Result := SizeOf(TRTTIDynArray);
    dtPointer: Result := SizeOf(TRTTIPointer);
    dtWeakRef: Result := SizeOf(TRTTIPointer);
    dtRecord: Result := SizeOf(TRTTIRecord);
    dtClass: Result := SizeOf(TRTTIClass);
    dtClassOf: Result := SizeOf(TRTTIPointer);
    dtProcType: Result := SizeOf(TRTTIProcType);
    dtInterface: Result := SizeOf(TRTTIInterface);
    dtFloat32, dtFloat64: Result := SizeOf(TRTTIFloat);
    dtAnsiString, dtString: Result := SizeOf(TRTTIArray);
    dtGuid: Result := SizeOf(TRTTIStruct);
    dtVariant: Result := SizeOf(TRTTYType);
  else
    raise Exception.CreateFmt('Unknown data type ID: %d', [Ord(DataTypeID)]);
  end;
end;

function ClassInheritsFrom(const ChildClass, AncestorClass: PRTTIClass): Boolean;
var
  Anc: PRTTIClass;
begin
  Assert(Assigned(ChildClass));
  Assert(Assigned(AncestorClass));
  if ChildClass = AncestorClass then
    Exit(True);
  Anc := PRTTIClass(ChildClass.Ancestor);
  while Assigned(Anc) do
  begin
    if Anc = AncestorClass then
      Exit(True);
    Anc := PRTTIClass(Anc.Ancestor);
  end;
  Result := False;
end;

function IntfSupportedFrom(const IntfID: Integer; const ClassType: PRTTIClass): Boolean;
begin
  Result := False;
end;

procedure InitOrdinal(var TypeInfo: TRTTIOrdinal; DataType: TDataTypeID; LowBound: Int64; HighBound: UInt64); inline;
begin
  TypeInfo.TypeInfo := nil;
  TypeInfo.RefCount := -1;
  TypeInfo.WeekInfo := nil;
  TypeInfo.SyncInfo := nil;
  TypeInfo.FClassID := rttiType;
  TypeInfo.DataTypeID := DataType;
  TypeInfo.Index := Integer(DataType);
  TypeInfo.Name := 0; // в таблице литералов названий встроенных типов нет
  TypeInfo.DataSize := cDataTypeSizes[DataType];
  TypeInfo.LoBound := LowBound;
  TypeInfo.HiBound := HighBound;
  TypeInfo.Signed := DataType in [dtInt8, dtInt16, dtInt32, dtInt64, dtNativeInt];
end;

procedure InitType(var TypeInfo: TRTTYType; DataType: TDataTypeID); inline;
begin
  TypeInfo.TypeInfo := nil;
  TypeInfo.RefCount := -1;
  TypeInfo.WeekInfo := nil;
  TypeInfo.SyncInfo := nil;
  TypeInfo.FClassID := rttiType;
  TypeInfo.DataTypeID := DataType;
  TypeInfo.DataSize := cDataTypeSizes[DataType];
  TypeInfo.Name := 0; // в таблице литералов названий встроенных типов нет
end;

procedure InitSystemTypes(var Types: TSystemTypes);
var
  off: UInt32;
begin
  off := SizeOf(UInt32); // RTTI signature
  InitOrdinal(Types._Int8, dtInt8, MinInt8, MaxInt8);
  InitOrdinal(Types._Int16, dtInt16, MinInt16, MaxInt16);
  InitOrdinal(Types._Int32, dtInt32, MinInt32, MaxInt32);
  InitOrdinal(Types._Int64, dtInt64, MinInt64, MaxInt64);
  InitOrdinal(Types._UInt8, dtUInt8, 0, MaxUInt8);
  InitOrdinal(Types._UInt16, dtUInt16, 0, MaxUInt16);
  InitOrdinal(Types._UInt32, dtUInt32, 0, MaxUInt32);
  InitOrdinal(Types._UInt64, dtUInt64, 0, MaxUInt64);
  InitOrdinal(Types._NativeInt, dtNativeInt, MinNativeInt, MaxNativeInt);
  InitOrdinal(Types._NativeUInt, dtNativeUInt, MinNativeUInt, MaxNativeUInt);
  InitType(Types._Float32, dtFloat32);
  InitType(Types._Float64, dtFloat64);
  InitOrdinal(Types._Boolean, dtBoolean, 0, 1);
  InitOrdinal(Types._AnsiChar, dtAnsiChar, 0, MaxUInt8);
  InitOrdinal(Types._Char, dtChar, 0, MaxUInt16);
  InitType(Types._AnsiString, dtAnsiString);
  Types._AnsiString.DimensionsCount := 1;
  Types._AnsiString.ElementTypeInfoOffset := off + GetOffset(@Types, @Types._AnsiChar);
  InitType(Types._String, dtString);
  Types._String.DimensionsCount := 1;
  Types._String.ElementTypeInfoOffset := off + GetOffset(@Types, @Types._Char);
  InitType(Types._Variant, dtVariant);
  InitType(Types._GUID, dtGuid);
  InitType(Types._Pointer, dtPointer);
  Types._Pointer.RefTypeInfoOffset := 0;
end;

end.
