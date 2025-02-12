unit ASTTest.Procs.DiffSigns1;

interface

{$HINTS OFF}

procedure TestProc(a, b: Integer);

implementation

procedure TestProc;
begin
  if a > b then;
end;

end.