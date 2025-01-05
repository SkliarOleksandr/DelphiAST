unit ASTTest.Sets.ForInSet2;

interface

{$HINTS OFF}

type 
  TIntSet = set of 1..10;
  
var
  IntSet: TIntSet;

implementation

procedure Main;
begin
  for var LItem in IntSet do;
end;

end.