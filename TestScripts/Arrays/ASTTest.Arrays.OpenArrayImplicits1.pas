unit ASTTest.Arrays.OpenArrayImplicits1;

interface

procedure DoSmth(Values: array of Integer);

var
  SIntDArray: array of Integer;
  SIntSArray: array [0..15] of Integer;

implementation

procedure Main;
begin
  DoSmth(SIntDArray);
  DoSmth(SIntSArray); 
end;

procedure DoSmth(Values: array of Integer);
begin

end;

end.