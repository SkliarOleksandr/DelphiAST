unit ASTTest.ProcTypes.ProcType4;

interface

type
  TProc = procedure (A, B: Integer);

implementation

procedure DoWork(A, B: Integer);
begin
end;

procedure Invoke(AProc: TProc);
begin
  AProc(1, 2);
end;

procedure Main;
begin
  Invoke(DoWork);
end;

end.