unit ASTTest.Procs.UntypedRefParam3;

interface

implementation

procedure ParamConst(const X);
begin
end;

procedure Main(const AValue: Integer);
begin
  ParamConst(AValue);
end;

end.