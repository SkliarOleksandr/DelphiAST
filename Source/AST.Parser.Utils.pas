unit AST.Parser.Utils;

{$I AST.Parser.Defines.inc}

interface

uses SysUtils, StrUtils, DateUtils, Classes, Math, AST.Delphi.DataTypes, Types, AVL, TypInfo, AnsiStrings,
     {$IFNDEF FPC}IOUtils{$ELSE}FileUtil{$ENDIF};

const
  MinInt8 = -128;
  MaxInt8 = 127;
  MinInt16 = -32768;
  MaxInt16 = 32767;
  MinInt32 = -2147483648;
  MaxInt32 = 2147483647;
  MinInt64 = -9223372036854775808; {F333333333333334}
  MaxInt64 = $7FFFFFFFFFFFFFFF;

  MaxUInt8 = 255;
  MaxUInt16 = 65535;
  MaxUInt32 = 4294967295;
  MaxUInt64: UInt64 = UInt64($FFFFFFFFFFFFFFFF);

  MinNativeInt = {$IFDEF PTR_SIZE4} MinInt32 {$ELSE} MinInt64 {$ENDIF};
  MaxNativeInt = {$IFDEF PTR_SIZE4} MaxInt32 {$ELSE} MaxInt64 {$ENDIF};
  MinNativeUInt = 0;
  MaxNativeUInt: NativeUInt = {$IFDEF PTR_SIZE4} MaxUInt32 {$ELSE} UInt64($FFFFFFFFFFFFFFFF) {$ENDIF};


  MaxFloat32 = MaxSingle;
  MinFloat32 = MinSingle;
  MaxFloat64 = MaxDouble;
  MinFloat64 = MinDouble;
  MaxFloat80 = MaxExtended80;
  MinFloat80 = MinExtended80;

  PTR_SIZE = Sizeof(Pointer);

type

  TIntArray = TIntegerDynArray;
  TStrArray = array of string;

  {$IFDEF NEXTGEN}
  AnsiString = UTF8String;
  AnsiChar = System.UTF8Char;
  {$ENDIF}

  {$IFDEF FPC}TArray<T> = array of T;{$ENDIF}

  TPool<T> = record
  const
    cDeltaSize = 64;
  type
    TPoolItems = TArray<T>;
    PItem = ^T;
  strict private
    FItems: TPoolItems;
    FPosition: Integer;
    FSize: Integer;
  public
    constructor Create(DefaultPoolSize: Integer);
    procedure Clear; inline;
    property Items: TPoolItems read FItems;
    property Position: Integer read FPosition write FPosition;
    function Add: PItem; overload; inline;
    procedure Add(const Item: T); overload; inline;
  end;

  TObjectsPool<T> = record
  private
    FItems: array of T;
    FCount: Integer;
    function GetCount: Integer; inline;
    function GetItem(Index: Integer): T;
  public
    property Count: Integer read GetCount;
    property Items[Index: Integer]: T read GetItem;
    function Get: T; inline;
    procedure Add(const Item: T); inline;
  end;

  TSimpleStack<T> = record
  private
    fItems: TArray<T>;
    fCount: Integer;
    fOnPopError: TProc;
    function GetTop: T; inline;
    procedure SetTop(const Value: T); inline;
  public
    constructor Create(Capacity: Integer);
    procedure Push(const Value: T);
    procedure Pop; inline;
    property Count: Integer read fCount;
    property Top: T read GetTop write SetTop;
    property OnPopError: TProc read fOnPopError write fOnPopError;
  end;

  TPooledObject = class(TNoRefCountObject)
  private
    FPrevObect: TPooledObject;
    class var FLastObject: TPooledObject;
    class var FCount: Integer;
    class procedure Initialize;
  protected
    procedure CreateFromPool; inline;
  public
    class procedure ClearPool;
  end;

  TRTTICharset = (
    RTTICharsetASCII,
    RTTICharsetUTF8,
    RTTICharsetUTF16,
    RTTICharsetUTF32
  );

  TStreamHelper = class helper for TStream
  private
    procedure _WriteBuffer(const Buffer; Count: Integer); inline;
    procedure _ReadBuffer(var Buffer; Count: Integer); inline;
  public
    function ReadBoolean: Boolean; inline;
    function ReadInt8: ShortInt; inline;
    function ReadUInt8: Byte; inline;
    function ReadInt16: SmallInt; inline;
    function ReadUInt16: Word; inline;
    function ReadInt32: Longint; inline;
    function ReadUInt32: Cardinal; inline;
    function ReadInt64: Int64; inline;
    function ReadUInt64: UInt64; inline;
    function ReadString: string; inline;
    function ReadAnsiString: AnsiString; inline;
    function ReadUTF8String: UnicodeString; inline;
    function ReadRawByteString: RawByteString; inline;
    function AsUTF8String: UnicodeString; inline;
    function AsUTF16String: UnicodeString; inline;
    function AsAnsiString: AnsiString; inline;
    function ReadFloat64: Double; inline;
    function ReadFloat32: Single; inline;
    function ReadDate: TDate; inline;
    function ReadTime: TTime; inline;
    function ReadDateTime: TDateTime; inline;
    function ReadGuid: TGUID; inline;
    function ReadStretchUInt: UInt64;
    procedure WriteGuid(const Value: TGUID); inline;
    procedure WriteBoolean(const Value: Boolean); inline;
    procedure WriteAnsiChar(const Value: AnsiChar); inline;
    procedure WriteChar(const Value: Char); inline;
    procedure WriteString(const Value: string); inline;
    procedure WriteUTF8String(const Value: string); inline;
    procedure WriteAnsiString(const Value: AnsiString); inline;
    procedure WriteInt8(const Value: ShortInt); inline;
    procedure WriteUInt8(const Value: Byte); inline;
    procedure WriteInt16(const Value: SmallInt); inline;
    procedure WriteUInt16(const Value: Word); inline;
    procedure WriteInt32(const Value: Longint); inline;
    procedure WriteUInt32(const Value: Cardinal); inline;
    procedure WriteInt64(const Value: Int64); inline;
    procedure WriteUInt64(const Value: UInt64); inline;
    procedure WriteNativeInt(const Value: NativeInt); inline;
    procedure WriteNativeUInt(const Value: NativeUInt); inline;
    procedure WriteFloat64(const Value: Double); inline;
    procedure WriteFloat32(const Value: Single); inline;
    procedure WriteDate(const Value: TDate); inline;
    procedure WriteTime(const Value: TTime); inline;
    procedure WriteDateTime(const Value: TDateTime); inline;
    procedure WriteStretchUInt(const Value: UInt64);
    procedure WriteVarUInt32(const Value: UInt32);
  end;

  TCaseInsensitiveObjectList = TAVLTree<string, TObject>;

  TCaseSensitiveIDList = TAVLTree<string, Integer>;

  TEnum<T> = record
    class function Name(const Value: T): string; static; stdcall;
  end;

function StringSegCount(const Str: string; const Separator: string = ','): Integer;
function StringSegment(const Str: string; Idx: Integer; const Separator: string = ','): string;
function AddStringSegment(const S1, S2: string; const Separator: string = ','): string; overload;
function AddStringSegment(const S1, S2: AnsiString; const Separator: AnsiString = ','): AnsiString; overload;

function Int32ToFloat32(Value: Int32): Single; inline;
function Int64ToFloat64(Value: Int64): Double; inline;

function Float32ToInt32(const Value: Single): Int32; inline;
function Float64ToInt64(const Value: Double): Int64; inline;

function GetValueByteSize(const Value: Int64): Integer;
function GetRangeByteSize(const HiBound: UInt64; const LoBound: Int64): Integer;

function GetValueDataType(const Value: Int64): TDataTypeID; overload; inline;
function GetValueDataType(const Value: Extended): TDataTypeID; overload; inline;
function GetValueDataType(const Value: Char): TDataTypeID; overload; inline;

function GetOffset(Base, Member: Pointer): NativeUint; inline;

function OpenArrayToDynamic(const Value: array of string): TStringDynArray;

function StrCICompare(const Key1, Key2: string): NativeInt; // case insencitive
function StrCSCompare(const Key1, Key2: string): NativeInt; // case sencitive
function AStrCSCompare(const Key1, Key2: AnsiString): NativeInt; // case sencitive


function HexToInt64(const Hex: string): Int64;
function HexToInt32(const Hex: string): Int32;

function TryHexToInt32(const Hex: string; out Value: Int32): Boolean;
function TryHexToUInt32(const Hex: string; out Value: UInt32): Boolean;
function TryHexToInt64(const Hex: string; out Value: Int64): Boolean;

function BinStringToInt64(const BinString: string): Int64;
function IsAnsiString(const Str: string): Boolean;

function Check(const Value: Cardinal; Flag: Cardinal): Boolean; overload; inline;

function GetDirectoryFiles(const SearchPath: string; SearchMask: string = ''; SearchSubDirs: Boolean = True): TStrArray;

function GetApplicationPath: string;
function TryStrToGUID(const Str: string; out GUID: TGUID): Boolean;

function OpenExe(const Path, Params, StdOutputFile, StdErrorFile: string): Boolean;

implementation

uses  Winapi.Windows;

function OpenExe(const Path, Params, StdOutputFile, StdErrorFile: string): Boolean;
var
  CmdString, RootPath: string;
  StartupInfo : TStartupInfo;
  ProcessInfo : TProcessInformation;
  StdOutput, StdError: TFileStream;
begin
  StdOutput := nil;
  StdError := nil;

  GetStartupInfo(StartupInfo);
  StartupInfo.wShowWindow := SW_HIDE; // не показывать окно
  StartupInfo.dwFlags := StartupInfo.dwFlags or STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;

  try
    if StdOutputFile <> '' then
    begin
      StdOutput := TFileStream.Create(StdOutputFile, fmCreate);
      SetHandleInformation(StdOutput.Handle, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT);
      StartupInfo.hStdOutput := StdOutput.Handle;
    end;

    if StdErrorFile <> '' then
    begin
      StdError := TFileStream.Create(StdErrorFile, fmCreate);
      SetHandleInformation(StdError.Handle, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT);
      StartupInfo.hStdError := StdError.Handle;
    end;

    RootPath := ExtractFilePath(Path);
    CmdString := '"' + Path + '" "' + Params + '"';
    if CreateProcess(nil, PChar(CmdString), nil, nil, True, NORMAL_PRIORITY_CLASS, nil, PChar(RootPath), StartupInfo, ProcessInfo) then
      WaitforSingleObject(ProcessInfo.hProcess, 1000)
    else
      RaiseLastOSError;

    CloseHandle(ProcessInfo.hProcess);

    if Assigned(StdError) then
    begin
      Result := (StdError.Size = 0);
    end else
      Result := True;

  finally
    StdOutput.Free;
    StdError.Free;
  end;

  if Result and (StdErrorFile <> '') then
    SysUtils.DeleteFile(StdErrorFile);
end;

{function TryStrToGUID(const S: string; out GUID: TGUID): Boolean;
  function HexChar(c: Char): Byte;
  begin
    case c of
      '0'..'9':  Result := Byte(c) - Byte('0');
      'a'..'f':  Result := (Byte(c) - Byte('a')) + 10;
      'A'..'F':  Result := (Byte(c) - Byte('A')) + 10;
    else
      Result := 0;
    end;
  end;

  function HexByte(p: PChar): Byte;
  begin
    Result := Byte((HexChar(p[0]) shl 4) + HexChar(p[1]));
  end;
var
  i: Integer;
  src: PChar;
  dest: PByte;
begin
  if ((Length(S) <> 38) or (S[Low(string)] <> '{')) then
    Exit(False);
  dest := @Result;
  src := PChar(s);
  Inc(src);
  for i := 0 to 3 do
    dest[i] := HexByte(src+(3-i)*2);
  Inc(src, 8);
  Inc(dest, 4);
  if src[0] <> '-' then Exit(False);
  Inc(src);
  for i := 0 to 1 do
  begin
    dest^ := HexByte(src+2);
    Inc(dest);
    dest^ := HexByte(src);
    Inc(dest);
    Inc(src, 4);
    if src[0] <> '-' then Exit(False);
    inc(src);
  end;
  dest^ := HexByte(src);
  Inc(dest);
  Inc(src, 2);
  dest^ := HexByte(src);
  Inc(dest);
  Inc(src, 2);
  if src[0] <> '-' then Exit(False);
  Inc(src);
  for i := 0 to 5 do
  begin
    dest^ := HexByte(src);
    Inc(dest);
    Inc(src, 2);
  end;
end; }

function GetApplicationPath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function Check(const Value: Cardinal; Flag: Cardinal): Boolean; overload; inline;
begin
  Result := (Value and Flag) = Flag;
end;

function BinStringToInt64(const BinString: string): Int64;
var
  i, c: Integer;
  Digit: Char;
begin
  Result := 0;
  c := Length(BinString);
  if c = 0 then
    raise Exception.Create('Invalid binary string');
  for i := 0 to c - 1 do
  begin
    Digit := BinString[i + 1];
    case Digit of
      '0':;
      '1': Result := Result + 1;
    else
      raise Exception.CreateFmt('Invalid binary string: %s', [BinString]);
    end;
    if i < c - 1 then
      Result := Result shl 1;
  end;
end;

function IsAnsiString(const Str: string): Boolean;
var
  i: Integer;
begin
  for i := Low(Str) to High(Str) do
    if Str[i] > High(AnsiChar) then
      Exit(False);
  Result := True;
end;

function HexToInt32(const Hex: string): Int32;
  function Multiplier(const Position: Integer): Int32; inline;
  var
    I: Integer;
  begin
    Result := 1;
    for I := 1 to Position do
      Result := 16 * Result;
  end;
var
  A, B, I: Integer;
begin
  B := 0;
  Result := 0;
  for I := Length(Hex) downto 1 do begin
    A := Ord(Hex[I]);
    case A of
      48: ; // '0' -- Do nothing.
      49..57: Result := Result + (A - 48) * Multiplier(B); // '1'..'9'
      65..70: Result := Result + (A - 55) * Multiplier(B); // 'A'..'F'
      97..102: Result := Result + (A - 87) * Multiplier(B); // 'a'..'f'
    else
      raise EConvertError.CreateFmt('%S is not a hexadecimal number.', [Hex]);
    end;
    Inc(B);
  end;
end;

function TryHexToInt32(const Hex: string; out Value: Int32): Boolean;
  function Multiplier(const Position: Integer): Int32; inline;
  var
    I: Integer;
  begin
    Result := 1;
    for I := 1 to Position do
      Result := 16 * Result;
  end;
var
  A, B, I: Integer;
begin
  B := 0;
  Value := 0;
  for I := High(Hex) downto Low(Hex) do begin
    A := Ord(Hex[I]);
    case A of
      48: ; // '0' - Do nothing.
      49..57: Value := Value + (A - 48) * Multiplier(B); // '1'..'9'
      65..70: Value := Value + (A - 55) * Multiplier(B); // 'A'..'F'
      97..102: Value := Value + (A - 87) * Multiplier(B); // 'a'..'f'
    else
      Exit(False);
    end;
    Inc(B);
  end;
  Result := True;
end;

function TryHexToUInt32(const Hex: string; out Value: UInt32): Boolean;
  function Multiplier(const Position: UInt32): UInt32; inline;
  var
    I: Integer;
  begin
    Result := 1;
    for I := 1 to Position do
      Result := 16 * Result;
  end;
var
  A, B, I: UInt32;
begin
  B := 0;
  Value := 0;
  for I := High(Hex) downto Low(Hex) do begin
    A := Ord(Hex[I]);
    case A of
      48: ; // '0' - Do nothing.
      49..57: Value := Value + (A - 48) * Multiplier(B); // '1'..'9'
      65..70: Value := Value + (A - 55) * Multiplier(B); // 'A'..'F'
      97..102: Value := Value + (A - 87) * Multiplier(B); // 'a'..'f'
    else
      Exit(False);
    end;
    Inc(B);
  end;
  Result := True;
end;

function TryHexToInt64(const Hex: string; out Value: Int64): Boolean;
  function Multiplier(const Position: Integer): Int64; inline;
  var
    I: Integer;
  begin
    Result := 1;
    for I := 1 to Position do
      Result := 16 * Result;
  end;
var
  A, B, I: Integer;
begin
  B := 0;
  Value := 0;
  for I := High(Hex) downto Low(Hex) do begin
    A := Ord(Hex[I]);
    case A of
      48: ;   // '0' -- Do nothing.
      49..57: Value := Value + (A - 48) * Multiplier(B); // '1'..'9'
      65..70: Value := Value + (A - 55) * Multiplier(B); // 'A'..'F'
      97..102: Value := Value + (A - 87) * Multiplier(B); // 'a'..'f'
    else
      Exit(False);
    end;
    Inc(B);
  end;
  Result := True;
end;

function HexToInt64(const Hex: string): Int64;
  function Multiplier(const Position: Integer): UInt64; inline;
  var
    I: Integer;
  begin
    Result := 1;
    for I := 1 to Position do
      Result := 16 * Result;
  end;
var
  A, B, I: Cardinal;
  R: UInt64;
begin
  B := 0;
  R := 0;
  for I := High(Hex) downto Low(Hex) do begin
    A := Ord(Hex[I]);
    case A of
      48: ;   // '0' -- Do nothing.
      49..57: R := R + (A - 48) * Multiplier(B); // '1'..'9'
      65..70: R := R + (A - 55) * Multiplier(B); // 'A'..'F'
      97..102: R := R + (A - 87) * Multiplier(B); // 'a'..'f'
    else
      raise EConvertError.CreateFmt('%S is not a hexadecimal number.', [Hex]);
    end;
    Inc(B);
  end;
  Result := Int64(R);
end;

procedure ExchangeHiLo(var Value: WORD); overload; inline;
var
  Bytes: array [0..1] of Byte absolute Value;
  Tmp: Byte;
begin
  Tmp := Bytes[0];
  Bytes[0] := Bytes[1];
  Bytes[1] := Tmp;
end;

procedure ExchangeHiLo(var Value: DWORD); overload; inline;
var
  Bytes: array [0..1] of Word absolute Value;
  Tmp: Word;
begin
  Tmp := Bytes[0];
  Bytes[0] := Bytes[1];
  Bytes[1] := Tmp;
end;

{процедура в 2.5 раза медленнее StringToGUID !!!}
function TryStrToGuid(const Str: string; out GUID: TGUID): Boolean;
{type
  TGUIDHelper = packed record
    D1: Cardinal;
    D2: Word;
    D3: Word;
    D4: Word;
    D5: Word;
    D6: Word;
    D7: Word;
  end;
  PInt32 = ^Int32;
  PUInt32 = ^UInt32;
var
  HexData: string;
  D: TGUIDHelper;    }
begin
(*  Result := (Length(Str) = 38) and
            (Str[Low(Str)] = '{') and
            (Str[High(Str)] = '}') and
            (Str[Low(Str) + 9] = '-') and
            (Str[Low(Str) + 14] = '-') and
            (Str[Low(Str) + 19] = '-') and
            (Str[Low(Str) + 24] = '-');

  if not Result then Exit;

  HexData := Copy(Str, Low(Str) + 1, 8);
  Result := TryHexToUInt32(HexData, D.D1);
  if not Result then
    Exit;

  HexData := Copy(Str, Low(Str) + 10, 4);
  Result := TryHexToUInt32(HexData, PUInt32(@D.D2)^);
  if not Result then
    Exit;

  HexData := Copy(Str, Low(Str) + 15, 4);
  Result := TryHexToUInt32(HexData, PUInt32(@D.D3)^);
  if not Result then
    Exit;

  HexData := Copy(Str, Low(Str) + 20, 4);
  Result := TryHexToUInt32(HexData, PUInt32(@D.D4)^);
  ExchangeHiLo(D.D4);
  if not Result then
    Exit;

  HexData := Copy(Str, Low(Str) + 25, 4);
  Result := TryHexToUInt32(HexData, PUInt32(@D.D5)^);
  ExchangeHiLo(D.D5);
  if not Result then
    Exit;

  HexData := Copy(Str, Low(Str) + 29, 4);
  Result := TryHexToUInt32(HexData, PUInt32(@D.D6)^);
  ExchangeHiLo(D.D6);
  if not Result then
    Exit;

  HexData := Copy(Str, Low(Str) + 33, 4);
  Result := TryHexToUInt32(HexData, PUInt32(@D.D7)^);
  ExchangeHiLo(D.D7);
  if not Result then
    Exit;

  Move(D, Guid, SizeOf(D)); *)
  try
    GUID := StringToGUID(Str);
    Result := True;
  except
    Result := False;
  end;
end;

function StrCICompare(const Key1, Key2: string): NativeInt;
begin
  Result := CompareText(Key1, Key2);
end;

function StrCSCompare(const Key1, Key2: string): NativeInt;
begin
  Result := AnsiCompareStr(Key1, Key2);
end;

function AStrCSCompare(const Key1, Key2: AnsiString): NativeInt; // case sencitive
begin
  Result := AnsiStrings.AnsiCompareStr(Key1, Key2);
end;

function OpenArrayToDynamic(const Value: array of string): TStringDynArray;
var
  i, c: Integer;
begin
  c := Length(Value);
  SetLength(Result, c);
  for i := 0 to c - 1 do
    Result[i] := Value[i];
end;

function GetOffset(Base, Member: Pointer): NativeUint;
begin
  Result := NativeUint(Member) - NativeUint(Base);
end;

function Int32ToFloat32(Value: Int32): Single;
begin
  PInteger(@Result)^ := Value;
end;

function Int64ToFloat64(Value: Int64): Double;
begin
  PInt64(@Result)^ := Value;
end;

function Float32ToInt32(const Value: Single): Int32;
begin
  PSingle(@Result)^ := Value;
end;

function Float64ToInt64(const Value: Double): Int64;
begin
  PDouble(@Result)^ := Value;
end;

function GetValueByteSize(const Value: Int64): Integer;
begin
  if (Value > MaxUInt32) or (Value < MinInt32) then
    Result := 8
  else
  if (Value > MaxUInt16) or (Value < MinInt16) then
    Result := 4
  else
  if (Value > MaxUInt8) or (Value < MinInt8) then
    Result := 2
  else
    Result := 1;
end;

function GetRangeByteSize(const HiBound: UInt64; const LoBound: Int64): Integer;
begin
  if (HiBound > MaxUInt32) or (LoBound < MinInt32) then
    Result := 8
  else
  if (HiBound > MaxUInt16) or (LoBound < MinInt16) then
    Result := 4
  else
  if (HiBound > MaxUInt8) or (LoBound < MinInt8) then
    Result := 2
  else
    Result := 1;
end;

function GetValueDataType(const Value: Int64): TDataTypeID;
begin
  if Value > MaxUInt32 then
    Result := dtInt64
  else
  if Value > MaxInt32 then
    Result := dtUInt32
  else
    Result := dtInt32;
end;

function GetValueDataType(const Value: Extended): TDataTypeID;
begin
  if Value = 0 then
    Result := dtFloat64
  else
  if (Value > MaxFloat64) or (Value < MinFloat64) then
    Result := dtFloat80
  else
    Result := dtFloat64
end;

function GetValueDataType(const Value: Char): TDataTypeID;
begin
  if Ord(Value) > 255 then
    Result := dtChar
  else
    Result := dtAnsiChar;
end;

function StringSegCount(const Str: string; const Separator: string): Integer;
var
  ppos, seplen: integer;
begin
  Result := 0;
  seplen := Length(separator);
  ppos := 1 - seplen;
  repeat
    Inc(ppos, seplen);
    Inc(Result);
    ppos := PosEx(separator, Str, ppos);
  until ppos = 0;
end;

function StringSegment(const Str: string; Idx: Integer; const Separator: string = ','): string;
var
  predpos,currpos:integer;
  seplen:integer;
begin
  seplen:=length(separator);
  currpos:=1-seplen;
  predpos := 0;
  result:='';
  while idx>0 do
  begin
    predpos:=currpos+seplen;
    currpos:= PosEx(separator,Str,predpos);
    dec(idx);
    if currpos=0 then
    begin
      if idx=0 then
        currpos:=length(Str)+1;
      Break;
    end;
  end;
  if idx=0 then begin
    result:=copy(Str,predpos,currpos-predpos);
  end;
end;

function AddStringSegment(const S1, S2: string; const Separator: string): string;
var
  sl: integer;
begin
  sl := Length(separator);
  if (Copy(S1, Length(S1) - sl + 1, sl) <> Separator) and
     (copy(S2, 1, sl) <> Separator) and (S1 <> '') and (S2 <> '') then
    Result := S1 + Separator + S2
  else
    Result := S1 + S2;
end;

function AddStringSegment(const S1, S2: AnsiString; const Separator: AnsiString = ','): AnsiString;
var
  sl: integer;
begin
  sl := Length(separator);
  if (Copy(S1, Length(S1) - sl + 1, sl) <> Separator) and
     (copy(S2, 1, sl) <> Separator) and (S1 <> '') and (S2 <> '') then
    Result := S1 + Separator + S2
  else
    Result := S1 + S2;
end;

{ TPool<T> }

function TPool<T>.Add: PItem;
begin
  Inc(FPosition);
  if FPosition >= FSize then begin
    Inc(FSize, cDeltaSize);
    SetLength(FItems, FSize);
  end;
  Result := @FItems[FPosition];
end;

procedure TPool<T>.Add(const Item: T);
begin
  Inc(FPosition);
  if FPosition >= FSize then begin
    Inc(FSize, cDeltaSize);
    SetLength(FItems, FSize);
  end;
  FItems[FPosition] := Item;
end;

procedure TPool<T>.Clear;
begin
  FPosition := -1;
end;

constructor TPool<T>.Create(DefaultPoolSize: Integer);
begin
  SetLength(FItems, DefaultPoolSize);
  FSize := DefaultPoolSize;
  FPosition := -1;
end;

{function TPool<T>.GetCurrentItem: PItem;
begin
  if (FPosition > -1) and (FPosition < FSize) then
    Result := @FItems[FPosition]
  else
    Result := nil;
end;}

{ TStreamHelper }

procedure TStreamHelper._ReadBuffer(var Buffer; Count: Integer);
begin
  if Read(Buffer, Count) <> Count then
    raise EReadError.Create('Stream read error');
end;

procedure TStreamHelper._WriteBuffer(const Buffer; Count: Integer);
begin
  if Write(Buffer, Count) <> Count then
    raise EWriteError.Create('Stream write error');
end;

function TStreamHelper.ReadAnsiString: AnsiString;
var
  ByteLen: Cardinal;
begin
  ByteLen := ReadStretchUInt;
  if ByteLen = 0 then
    Exit('');
  SetLength(Result, ByteLen);
  ReadBuffer(Result[1], ByteLen);
end;

function TStreamHelper.ReadBoolean: Boolean;
begin
  Result := False;
  _ReadBuffer(Byte(Result), SizeOf(Byte));
end;

function TStreamHelper.ReadDate: TDate;
begin
  _ReadBuffer(Result, SizeOf(Result));
end;

function TStreamHelper.ReadDateTime: TDateTime;
begin
  _ReadBuffer(Result, SizeOf(Result));
end;

function TStreamHelper.ReadFloat64: Double;
begin
  _ReadBuffer(Result, SizeOf(Result));
end;

function TStreamHelper.ReadGuid: TGUID;
begin
  _ReadBuffer(Result, SizeOf(Result));
end;

function TStreamHelper.ReadInt64: Int64;
begin
  _ReadBuffer(Result, SizeOf(Result));
end;

function TStreamHelper.ReadInt16: SmallInt;
begin
  _ReadBuffer(Result, SizeOf(Result));
end;

function TStreamHelper.ReadInt8: ShortInt;
begin
  _ReadBuffer(Result, SizeOf(Result));
end;

function TStreamHelper.ReadRawByteString: RawByteString;
var
  ByteLen: Cardinal;
begin
  ByteLen := ReadStretchUInt;
  if ByteLen = 0 then
    Exit('');
  SetLength(Result, ByteLen);
  _ReadBuffer(Result[Low(Result)], ByteLen);
end;

function TStreamHelper.ReadInt32: Longint;
begin
  _ReadBuffer(Result, SizeOf(Result));
end;

function TStreamHelper.ReadFloat32: Single;
begin
  _ReadBuffer(Result, SizeOf(Result));
end;

function TStreamHelper.ReadStretchUInt: UInt64;
var
  B: Byte;
begin
  // 0-Byte ////////////////////////////////////////////////////
  _ReadBuffer(B, 1);
  Result := (B and 127);
  if (B and 128) = 0 then
    Exit;
  // 1-Byte ////////////////////////////////////////////////////
  _ReadBuffer(B, 1);
  Result := Result + (B and 127) shl 7;
  if (B and 128) = 0 then
    Exit;
  // 2-Byte ////////////////////////////////////////////////////
  _ReadBuffer(B, 1);
  Result := Result + (B and 127) shl 14;
  if (B and 128) = 0 then
    Exit;
  // 3-Byte ////////////////////////////////////////////////////
  _ReadBuffer(B, 1);
  Result := Result + (B and 127) shl 21;
  if (B and 128) = 0 then
    Exit;
  // 4-Byte ////////////////////////////////////////////////////
  _ReadBuffer(B, 1);
  Result := Result + UInt64(B and 127) shl 28;
  if (B and 128) = 0 then
    Exit;
  // 5-Byte ////////////////////////////////////////////////////
  _ReadBuffer(B, 1);
  Result := Result + UInt64(B and 127) shl 35;
  if (B and 128) = 0 then
    Exit;
  // 6-Byte ////////////////////////////////////////////////////
  _ReadBuffer(B, 1);
  Result := Result + UInt64(B and 127) shl 42;
  if (B and 128) = 0 then
    Exit;
  // 7-Byte ////////////////////////////////////////////////////
  _ReadBuffer(B, 1);
  Result := Result + UInt64(B) shl 49; // в последнем байте учитываются все 8 бит
end;

function TStreamHelper.ReadString: string;
var
  ByteLen: Cardinal;
begin
  ByteLen := ReadStretchUInt;
  if ByteLen = 0 then
    Exit('');
  SetLength(Result, ByteLen div SizeOf(Char));
  _ReadBuffer(Result[1], ByteLen);
end;

function TStreamHelper.ReadTime: TTime;
begin
  _ReadBuffer(Result, SizeOf(Result));
end;

function TStreamHelper.ReadUInt16: Word;
begin
  _ReadBuffer(Result, SizeOf(Result));
end;

function TStreamHelper.ReadUInt32: Cardinal;
begin
  _ReadBuffer(Result, SizeOf(Result));
end;

function TStreamHelper.ReadUInt64: UInt64;
begin
  _ReadBuffer(Result, SizeOf(Result));
end;

function TStreamHelper.ReadUInt8: Byte;
begin
  _ReadBuffer(Result, SizeOf(Result));
end;

function TStreamHelper.ReadUTF8String: Unicodestring;
var
  ByteLen: Cardinal;
  utf8str: RawByteString;
begin
  ByteLen := ReadStretchUInt;
  if ByteLen = 0 then
    Exit('');
  SetLength(utf8str, ByteLen);
  _ReadBuffer(utf8str[1], ByteLen);
  Result := {$IFNDEF FPC}UTF8ToString{$ELSE}UTF8Decode{$ENDIF}(utf8str);
end;

function TStreamHelper.AsAnsiString: AnsiString;
begin
  SetLength(Result, Size);
  if Size > 0 then
    _ReadBuffer(Result[1], Size);
end;

function TStreamHelper.AsUTF16String: UnicodeString;
begin
  SetLength(Result, Size div 2);
  if Size > 0 then
    _ReadBuffer(Result[1], Size);
end;

function TStreamHelper.AsUTF8String: UnicodeString;
var
  utf8str: RawByteString;
begin
  SetLength(utf8str, Size);
  if Size > 0 then
    _ReadBuffer(utf8str[1], Size);
  Result := {$IFNDEF FPC}UTF8ToString{$ELSE}UTF8Decode{$ENDIF}(utf8str);
end;

procedure TStreamHelper.WriteAnsiChar(const Value: AnsiChar);
begin
  _WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.WriteAnsiString(const Value: AnsiString);
var
  ByteLen: Cardinal;
begin
  ByteLen := Length(Value);
  WriteStretchUInt(ByteLen);
  if ByteLen > 0 then
    _WriteBuffer(Value[1], ByteLen);
end;

procedure TStreamHelper.WriteBoolean(const Value: Boolean);
begin
  _WriteBuffer(Byte(Value), SizeOf(Byte));
end;

procedure TStreamHelper.WriteChar(const Value: Char);
begin
  _WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.WriteDate(const Value: TDate);
begin
  _WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.WriteDateTime(const Value: TDateTime);
begin
  _WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.WriteFloat64(const Value: Double);
begin
  _WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.WriteGuid(const Value: TGUID);
begin
  _WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.WriteInt16(const Value: SmallInt);
begin
  _WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.WriteInt32(const Value: Longint);
begin
  _WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.WriteInt64(const Value: Int64);
begin
  _WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.WriteInt8(const Value: ShortInt);
begin
  _WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.WriteFloat32(const Value: Single);
begin
  _WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.WriteStretchUInt(const Value: UInt64);
const
   C1: UInt64 = 1;
   C127: UInt64 = 127;
var
  Bytes: array [0..7] of Byte;
  ByteSize: Integer;
begin
  ByteSize := 1;
  if Value > (C1 shl 56) then  // max value 72_057_594_037_927_985
    raise Exception.CreateFmt('Value %d is to big for StretchUInt64', [Value]);
  // 0-Byte ////////////////////////////////////////////
  while true do begin
    Bytes[0] := (Value and 127);
    if Value < (1 shl 7) then begin
      //ByteSize := 1;
      break;
    end;
    Bytes[0] := Bytes[0] or 128;
    // 1-Byte ////////////////////////////////////////////////////
    Bytes[1] := (Value and (127 shl 7)) shr 7;
    if Value < (1 shl 14) then begin
      ByteSize := 2;
      break;
    end;
    Bytes[1] := Bytes[1] or 128;
    /// 2-Byte ///////////////////////////////////////////////////
    Bytes[2] := (Value and (127 shl 14)) shr 14;
    if Value < (1 shl 21) then begin
      ByteSize := 3;
      break;
    end;
    Bytes[2] := Bytes[2] or 128;
    // 3-Byte ////////////////////////////////////////////////////
    Bytes[3] := (Value and (127 shl 21)) shr 21;
    if Value < (1 shl 28) then begin
      ByteSize := 4;
      break;
    end;
    Bytes[3] := Bytes[3] or 128;
    // 4-Byte ////////////////////////////////////////////////////
    Bytes[4] := (Value and (C127 shl 28)) shr 28;
    if Value < (C1 shl 35) then begin
      ByteSize := 5;
      break;
    end;
    Bytes[4] := Bytes[4] or 128;
    // 5-Byte ////////////////////////////////////////////////////
    Bytes[5] := (Value and (C127 shl 35)) shr 35;
    if Value < (C1 shl 42) then begin
      ByteSize := 6;
      break;
    end;
    Bytes[5] := Bytes[5] or 128;
    // 6-Byte ////////////////////////////////////////////////////
    Bytes[6] := (Value and (C127 shl 42)) shr 42;
    if Value < (C1 shl 49) then begin
      ByteSize := 7;
      break;
    end;
    Bytes[6] := Bytes[6] or 128;
    // 7-Byte ////////////////////////////////////////////////////
    Bytes[7] := (Value and (UInt64(255) shl 49)) shr 49;  // в последнем байте учитываются все 8 бит
    ByteSize := 8;
    break;
  end;
  _WriteBuffer(Bytes[0], ByteSize);
end;

procedure TStreamHelper.WriteString(const Value: String);
var
  ByteLen: Cardinal;
begin
  ByteLen := ByteLength(Value);
  WriteStretchUInt(ByteLen);
  if ByteLen > 0 then
    _WriteBuffer(Value[1], ByteLen);
end;

procedure TStreamHelper.WriteUTF8String(const Value: String);
var
  ByteLen: Cardinal;
  utf8str: RawByteString;
begin
  utf8str := UTF8Encode(Value);
  ByteLen := Length(utf8str);
  WriteStretchUInt(ByteLen);
  if ByteLen > 0 then
    _WriteBuffer(utf8str[1], ByteLen);
end;

procedure TStreamHelper.WriteVarUInt32(const Value: UInt32);
var
  Bytes: array [0..3] of Byte;
  ByteSize: Integer;
begin
  ByteSize := 1;
  if Value > (1 shl 28) then  // max value
    raise Exception.CreateFmt('Value %d is to big for VarUInt32', [Value]);

  while true do begin
    // 0-Byte ////////////////////////////////////////////////////
    Bytes[0] := (Value and 127);
    if Value < (1 shl 7) then
      break;
    Bytes[0] := Bytes[0] or 128;
    // 1-Byte ////////////////////////////////////////////////////
    Bytes[1] := (Value and (127 shl 7)) shr 7;
    if Value < (1 shl 14) then begin
      ByteSize := 2;
      break;
    end;
    Bytes[1] := Bytes[1] or 128;
    /// 2-Byte ///////////////////////////////////////////////////
    Bytes[2] := (Value and (127 shl 14)) shr 14;
    if Value < (1 shl 21) then begin
      ByteSize := 3;
      break;
    end;
    Bytes[2] := Bytes[2] or 128;
    // 3-Byte ////////////////////////////////////////////////////
    Bytes[3] := (Value and (127 shl 21)) shr 21;
    ByteSize := 4;
    break;
  end;
  _WriteBuffer(Bytes[0], ByteSize);
end;

procedure TStreamHelper.WriteTime(const Value: TTime);
begin
  _WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.WriteUInt16(const Value: Word);
begin
  _WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.WriteUInt32(const Value: Cardinal);
begin
  _WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.WriteUInt64(const Value: UInt64);
begin
  _WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.WriteUInt8(const Value: Byte);
begin
  _WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.WriteNativeInt(const Value: NativeInt);
begin
  _WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.WriteNativeUInt(const Value: NativeUInt);
begin
  _WriteBuffer(Value, SizeOf(Value));
end;

{ TPooledObject }

procedure TPooledObject.CreateFromPool;
begin
  FPrevObect := FLastObject;
  FLastObject := Self;
  Inc(FCount);
end;

class procedure TPooledObject.ClearPool;
var
  Obj, Tmp: TPooledObject;
begin
  Obj := FLastObject;
  while Assigned(Obj) do begin
    Tmp := Obj;
    Obj := Obj.FPrevObect;
    try
      dec(FCount);
      Tmp.Free;
    except
    end;
  end;
  FLastObject := nil;
end;

class procedure TPooledObject.Initialize;
begin
  FLastObject := nil;
end;

function GetDirectoryFiles(const SearchPath: string; SearchMask: string = ''; SearchSubDirs: Boolean = True): TStrArray;
{$IFDEF FPC}
var
  Files: TStringList;
  FilesCount, i: Integer;
{$ENDIF}
begin
{$IFDEF FPC}
  Files := FindAllFiles(SearchPath, SearchMask, SearchSubDirs);
  try
    Files.Sort;
    FilesCount := Files.Count;
    SetLength(Result, FilesCount);
    for i := 0 to FilesCount - 1 do
      Result[i] := Files[i];
  finally
    Files.Free;
  end;
{$ELSE}
  Result := TStrArray(TDirectory.GetFiles(SearchPath, SearchMask, TSearchOption.soAllDirectories));
 {$ENDIF}
end;

{ TEnum<T> }

class function TEnum<T>.Name(const Value: T): string;
var
  TI: PtypeInfo;
begin
  TI := TypeInfo(T);
  Result := TypInfo.GetEnumName(TI, PInteger(@Value)^);
end;

{ TObjectsPool }

procedure TObjectsPool<T>.Add(const Item: T);
var
  c: Integer;
begin
  c := Length(FItems);
  if c = 0 then
    FCount := 0;

  if FCount >= c then
  begin
    SetLength(FItems, c + 1);
    FItems[c] := Item;
  end else
    FItems[FCount] := Item;
  Inc(FCount);
end;

function TObjectsPool<T>.Get: T;
var
  c: Integer;
begin
  c := Length(FItems);
  if c = 0 then
    FCount := 0;

  if FCount > 0 then
  begin
    Result := FItems[FCount - 1];
    Dec(FCount);
  end else
    Result := Default(T);
end;

function TObjectsPool<T>.GetCount: Integer;
begin
  if Length(FItems) = 0 then
    FCount := 0;
  Result := FCount;
end;

function TObjectsPool<T>.GetItem(Index: Integer): T;
begin
  Result := FItems[Index];
end;


procedure tesss;
var
  dt1, dt2: TDateTime;
  i, c1, c2: Integer;
  s: string;
  g: tguid;

begin
  s := '{92A2BD02-9DEA-45AC-B064-9FE38AD8AC74}';
  dt1 := Now;
  for i := 0 to 10000000 do
    g := StringToGUID(S);
  c1 := MilliSecondsBetween(Now, dt1);

  dt2 := Now;
  for i := 0 to 10000000 do
    TryStrToGUID(S, g);
  c2 := MilliSecondsBetween(Now, dt2);

  if c1 <> c2 then
end;


{ TSimpleStack<T> }

constructor TSimpleStack<T>.Create(Capacity: Integer);
begin
  SetLength(fItems, Capacity);
  fCount := 0;
end;

procedure TSimpleStack<T>.Push(const Value: T);
var
  Len: Integer;
begin
  Len := Length(fItems);
  if Len <= fCount then
    SetLength(fItems, (Len + 1) * 2);

  fItems[fCount] := Value;
  Inc(fCount);
end;

procedure TSimpleStack<T>.Pop;
begin
  if fCount <= 0 then
  begin
    fOnPopError();
    Exit;
  end;
  Dec(fCount);
end;

function TSimpleStack<T>.GetTop: T;
begin
  Result := fItems[fCount - 1];
end;

procedure TSimpleStack<T>.SetTop(const Value: T);
begin
  fItems[fCount - 1] := Value;
end;

initialization
  TPooledObject.Initialize;

finalization
  TPooledObject.ClearPool;


end.
