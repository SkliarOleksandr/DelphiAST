unit ASTTest.Sets.ExplicitCast2;

interface

{$HINTS OFF}

type
  TEnum = (Item1, Item2, Item3);
  TEnumSet = set of TEnum;

implementation

procedure Main;
var 
  LSet: TEnumSet;
  LByte: Byte;
begin
  LByte := 42;
  LSet := TEnumSet(LByte and $F0);
end;

end.