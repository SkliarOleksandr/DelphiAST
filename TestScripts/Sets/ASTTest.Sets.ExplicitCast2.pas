unit ASTTest.Sets.ExplicitCast2;

interface

{$HINTS OFF}

type
  TSet8 = set of 0..7;
  TSet16 = set of 0..15;
  TSet32 = set of 0..31;  

implementation

procedure Main8;
var 
  LSet8: TSet8;
  LByte: Byte;
begin
  LByte := 42;
  LSet8 := TSet8(LByte and $F0);
end;

procedure Main16;
var
  LSet16: TSet16;
  LByte: Byte;
begin
  LByte := 42;
  LSet16 := TSet16(LByte and $FF0);
end;

procedure Main32;
var
  LSet32: TSet32;
  LByte: Byte;
begin
  LByte := 42;
  LSet32 := TSet32(LByte and $FFFF0);
end;

end.