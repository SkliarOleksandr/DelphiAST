unit ASTTest.Variants.Variant1;

interface

implementation

procedure Main;
var
  V: Variant;
  I: Integer;
  D: Double;
  S: string;
  B: Boolean;
begin
  V := 0; 

  I := V;
  D := V;
  S := V;
  B := V;

  V := I;
  V := D;
  V := S;
  V := B;
end;

end.