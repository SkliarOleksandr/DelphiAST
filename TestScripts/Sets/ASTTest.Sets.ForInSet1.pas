unit ASTTest.Sets.ForInSet1;

interface

{$HINTS OFF}

type
  TEnum = (Item1, Item2, Item3);
  TEnumSet = set of TEnum;
  
var
  EnumSet: TEnumSet;

implementation

procedure Main;
var
  LEnum: TEnum;
begin
  for LEnum in EnumSet do;
end;

end.