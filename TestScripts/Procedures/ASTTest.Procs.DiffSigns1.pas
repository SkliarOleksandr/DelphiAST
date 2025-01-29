unit ASTTest.Procs.DiffSigns1;

interface

procedure TestProc(a, b: Integer); overload;

implementation

procedure TestProc;
begin
  if a > b then;
end;

end.