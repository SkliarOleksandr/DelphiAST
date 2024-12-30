unit ASTTest.Variants.Variant1;

interface

implementation

procedure Main;
var
  V: Variant;
  I: Integer;
  D: Double;
  S: string;    
begin
  V := 0; 

  I := V;
  D := V;
  S := V;

  V := I;
  V := D;
  V := S;   
end;

end.