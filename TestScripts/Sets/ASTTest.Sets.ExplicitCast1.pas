unit ASTTest.Sets.ExplicitCast1;

interface

{$HINTS OFF}

type
  TEnum8 = (Bit0, Bit1, Bit2, Bit3, Bit4, Bit5, Bit6, Bit7);
  TByteSet = set of TEnum8;
  TWordSet = set of 0..15;
  TIntSet = set of 0..31;
  TInt64Set = set of 0..63;

implementation

procedure MainByteSet;
var 
  LByteSet: TByteSet;
 
  LUByte: Byte;
  LSByte: ShortInt;
  LAnsiChar: AnsiChar;

  LWord: Word;
  LInteger: Integer;
begin
  LByteSet := [];

  LUByte := Byte(LByteSet);
  LSByte := ShortInt(LByteSet);
  LAnsiChar := AnsiChar(LByteSet);

  // LWord := Word(LByteSet);        // E2089 Invalid typecast
  // LInteger := Integer(LByteSet);  // E2089 Invalid typecast

  LByteSet := TByteSet(LUByte);
  LByteSet := TByteSet(LSByte);
  LByteSet := TByteSet(LAnsiChar);
  // LByteSet := TByteSet(LWord);    // E2089 Invalid typecast
  // LByteSet := TByteSet(LInteger); // E2089 Invalid typecast
  // LByteSet := TByteSet(1);        // E2089 Invalid typecast
end;

procedure MainWordSet;
var
  LWordSet: TWordSet;

  LUByte: Byte;
  LSByte: ShortInt;
  LAnsiChar: AnsiChar;

  LWord: Word;
  LInteger: Integer;
begin
  LWordSet := [];

  // LUByte := Byte(LWordSet);         // E2089 Invalid typecast
  // LSByte := ShortInt(LWordSet);     // E2089 Invalid typecast
  // LAnsiChar := AnsiChar(LWordSet);  // E2089 Invalid typecast

  LWord := Word(LWordSet);             // E2089 Invalid typecast
  // LInteger := Integer(LWordSet);    // E2089 Invalid typecast

  //  LWordSet := TWordSet(LUByte);    // E2089 Invalid typecast
  //  LWordSet := TWordSet(LSByte);    // E2089 Invalid typecast
  //  LWordSet := TWordSet(LAnsiChar); // E2089 Invalid typecast
  LWordSet := TWordSet(LWord);
  // LWordSet := TWordSet(LInteger);   // E2089 Invalid typecast
  // LByteSet := TByteSet(1);          // E2089 Invalid typecast
end;

end.