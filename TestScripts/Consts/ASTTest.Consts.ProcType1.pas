unit ASTTest.Consts.ProcType1;

interface

procedure Test;

type
  TProc = procedure;

const
  CProc: TProc = Test;

implementation

procedure Test;
begin
  // do smth 
end;

procedure Main;
begin
  CProc();
end;


end.