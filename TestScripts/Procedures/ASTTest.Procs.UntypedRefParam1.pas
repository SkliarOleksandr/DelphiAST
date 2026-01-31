unit ASTTest.Procs.UntypedRefParam1;

interface

implementation

procedure ParamVar(var X);
begin
end;

procedure ParamOut(out X);
begin
end;

procedure ParamConst(const X);
begin
end;

procedure Main;
var
  LValue: Integer;
begin
  LValue := 0;
  ParamVar(LValue);
  ParamOut(LValue);
  ParamConst(LValue);
end;

end.