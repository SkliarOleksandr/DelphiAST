unit ASTTest.Cmp.PChars;

interface

implementation

type
  PInteger = ^Integer;
  {$POINTERMATH ON}
  PByte = ^Byte;
  {$POINTERMATH OFF}

procedure CmpPAnsiChar(
  APAChar1: PAnsiChar;
  APAChar2: PAnsiChar);
begin
  if APAChar1 = APAChar2 then;
  if APAChar1 <> APAChar2 then;
end;

procedure CmpPWideChar(
  APWChar1: PWideChar;
  APWChar2: PWideChar);
begin
  if APWChar1 = APWChar2 then;
  if APWChar1 <> APWChar2 then;
end;

procedure CmpAStrVsPAChar(
  AAStr: AnsiString;
  APAChar: PAnsiChar);
begin
  if AAStr = APAChar then;
  if AAStr <> APAChar then;
end;

procedure CmpUStrVsPUChar(
  AUStr: string;
  APUChar: PWideChar);
begin
  if AUStr = APUChar then;
  if AUStr <> APUChar then;
end;

procedure CmpPCharsVsUntypedPtr(
  APAChar: PAnsiChar;
  APWChar: PWideChar;
  APtr: Pointer);
begin
  if APAChar = APtr then;
  if APAChar <> APtr then;
  if APWChar = APtr then;
  if APWChar <> APtr then;
end;

procedure CmpPCharsVsTypedPtrs(
  APAChr: PAnsiChar;
  APWChr: PWideChar;
  APUChr: PChar;
  APInt: PInteger;
  APByte: PByte);
begin
  if APAChr > APByte then;
  if APWChr > APByte then;
  if APUChr > APByte then;

  if APAChr > APByte then;
  if APWChr > APByte then;
  if APUChr > APByte then;

  if APByte > APAChr then;
  if APByte > APWChr then;
  if APByte > APUChr then;

  if APAChr > APByte then;
  if APWChr > APByte then;
  if APUChr > APByte then;
end;

end.