unit ASTTest.Consts.ProcTypeAsConst;

interface

procedure Test;

type
  TProc = procedure;

const
  CProc: TProc = Test;

implementation

procedure Test;
begin
end;

procedure Main;
begin
  //CProc();
end;


end.