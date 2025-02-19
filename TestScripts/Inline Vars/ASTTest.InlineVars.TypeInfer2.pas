unit ASTTest.InlineVars.TypeInfer2;

interface

implementation

procedure Test(var AInt: Integer);
begin
end;

procedure Main;
begin
  var LInt := 1;
  Test({var} LInt);
end;

end.