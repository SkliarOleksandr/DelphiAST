unit ASTTest.Loops.ForIn1;

interface

{$HINTS OFF}

implementation

var Arr: array of Integer;

procedure Main;
begin
  var LTotal := 0;
  var LValue1: Integer;
 
  for LValue1 in Arr do
    LTotal := LTotal + LValue1;      
  
  for var LValue2 in Arr do
    LTotal := LTotal + LValue2;
    
  for var LValue3: Integer in Arr do
    LTotal := LTotal + LValue3;      
end;

end.