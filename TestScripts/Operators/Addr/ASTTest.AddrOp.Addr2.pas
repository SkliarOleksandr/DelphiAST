unit ASTTest.AddrOp.Addr2;

interface

type
  TFunc = function: string;

var
  GFunc: TFunc;

implementation

procedure Test1(var APtr: Pointer);
begin
end;

procedure Test2(var APtr: TFunc);
begin
end;

procedure Main;
begin
  Test1(@GFunc);
  Test2(GFunc);
end;

end.