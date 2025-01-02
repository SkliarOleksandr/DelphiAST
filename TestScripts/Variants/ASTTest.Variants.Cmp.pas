unit ASTTest.Variants.Cmp;

interface

{$HINTS OFF}

implementation

procedure Main;
var
  V: Variant;
begin
  if V then;
  if V > 0 then;
  if V > 0.0 then;
  if V = 'A' then;
  if V = 'ABC' then;
  if V = True then;      
end;

end.