unit ASTTest.Variants.Enum1;

interface

{$HINTS OFF}

type
  TEnum = (it1, it2, it3);

var
  V: Variant;
  E: TEnum;

implementation

procedure Main;
begin
  V := it1;
  E := V;
end;

end.