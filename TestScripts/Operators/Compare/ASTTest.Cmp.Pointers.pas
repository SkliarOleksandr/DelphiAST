unit ASTTest.Cmp.Pointers;

interface

implementation

type
  TRecord = record
    A, B: Integer;
  end;

  TMyObj = class

  end;

  IMyIntf = interface

  end;

  TMyProc = procedure;

  PByte = ^Byte;
  PWord = ^Word;
  PInteger = ^Integer;
  PRecord = ^TRecord;
  {$POINTERMATH ON}
  PBytePM = ^Byte;
  PWordPM = ^PWord;
  PIntegerPM = ^Integer;
  PRecordPM = ^TRecord;
  {$POINTERMATH OFF}
  PMyIntf = ^IMyIntf;
  PMyObj = ^TMyObj;
  PMyProc = ^TMyProc;

procedure CmpUntypedPointer(
  APtr: Pointer;
  APInt: PInteger;
  APRec: PRecord;
  APIntf: PMyIntf;
  APObj: PMyObj;
  APProc: PMyProc);
begin
  if APtr = APtr then;
  if APInt = APtr then;
  if APRec = APtr then;
  if APIntf = APtr then;
  if APObj = APtr then;
  if APProc = APtr then;
end;

procedure CmpPointers(
  APtr: Pointer;
  APByte1, APByte2: PByte;
  APWord1, APWord2: PWord;
  APInt1, APInt2: PInteger;
  APRec1, APRec2: PRecord);
begin
  if APByte1 = APtr then;
  if APByte1 <> APtr then;

  if APWord1 = APtr then;
  if APWord1 <> APtr then;

  if APInt1 = APtr then;
  if APInt1 <> APtr then;

  if APInt1 = APInt2 then;
  if APInt1 <> APInt2 then;
  
  if APRec1 = APRec2 then;
  if APRec1 <> APRec2 then;  

  // if APInt1 > APtr then; // E2015 Operator not applicable to this operand type
  // if APInt1 > APInt2 then; // E2015 Operator not applicable to this operand type
  // if APRec1 > APRec1 then; // E2015 Operator not applicable to this operand type

  // if LPByte1 = LPWord1 then; // E2010 Incompatible types: 'Byte' and 'Word'
  // if LPByte1 = LPInt1 then;  // E2010 Incompatible types: 'Byte' and 'Integer'
end;

procedure CmpPointersWithPoiterMatch(
  APInt: PInteger;
  APIntPM: PIntegerPM;
  APByte: PByte;
  APBytePM: PBytePM;
  APRec: PRecord;
  APRecPM: PRecordPM);
begin
  if APIntPM = APIntPM then;
  if APIntPM <> APIntPM then;
  if APIntPM > APIntPM then;
  if APIntPM < APIntPM then;
  if APIntPM >= APIntPM then;
  if APIntPM <= APIntPM then;

  // if APIntPM = APInt then; // E2008 Incompatible types
  // if APByte = APBytePM then; // E2008 Incompatible types

  // if APIntPM = APBytePM then; // E2010 Incompatible types: 'Integer' and 'Byte'
  // if APIntPM <> APBytePM then; // E2010 Incompatible types: 'Integer' and 'Byte'
  if APIntPM > APBytePM then;
  if APIntPM < APBytePM then;
  if APIntPM >= APBytePM then;
  if APIntPM <= APBytePM then;


  if APRec = APRec then;
  if APRec <> APRec then;

  if APRecPM = APRecPM then;
  if APRecPM <> APRecPM then;
  if APRecPM < APRecPM then;
  if APRecPM >= APRecPM then;
  if APRecPM <= APRecPM then;
end;

end.