unit ASTTest.Ranges.EnumRange1;

interface

{$HINTS OFF}

type
  TEnum = (it1, it2, it3);
  TRange = it2..it3;

var
  R: TRange;

implementation

procedure Main;
begin
  // R := it1; // E1012 Constant expression violates subrange bounds
  R := it2;
  if R = it2 then;
end;

end.