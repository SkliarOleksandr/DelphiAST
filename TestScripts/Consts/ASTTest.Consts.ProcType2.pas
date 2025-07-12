unit ASTTest.Consts.ProcType2;

interface

const
  CProc: procedure(s: string) = nil;

implementation

procedure Main;
begin
  CProc('str');
end;

end.