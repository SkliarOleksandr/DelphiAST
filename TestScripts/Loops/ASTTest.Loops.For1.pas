unit ASTTest.Loops.For1;

interface

{$HINTS OFF}

implementation

var Arr: array of Integer;

procedure Main;
begin
  var LTotal := 0;
  var LIndex1: Integer;
 
  for LIndex1 := 0 to Length(Arr) - 1 do
    LTotal := LTotal + Arr[LIndex1];      
  
  for var LIndex2 := 0 to Length(Arr) - 1 do
    LTotal := LTotal + Arr[LIndex2];  

  for var LIndex2: Integer := 0 to Length(Arr) - 1 do
    LTotal := LTotal + Arr[LIndex2];        
end;

end.