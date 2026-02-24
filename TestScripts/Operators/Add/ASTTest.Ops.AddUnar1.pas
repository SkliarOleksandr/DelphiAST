unit ASTTest.Ops.AddUnar1;

interface

implementation

var
  A1, A2: Integer;

procedure Main;
begin
  A1 := +++ 1;
  A2 := +++ A1 +++ A1;
end;

end.