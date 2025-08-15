unit ASTTest.ProcTypes.ProcType2;

interface

{$HINTS OFF}

type
  PStaticProc = ^TStaticProc;
  TStaticProc = procedure(A: Integer);

var
  StaticProcPtr: PStaticProc;

implementation

procedure Main;
begin
  StaticProcPtr^(1);
end;

end.