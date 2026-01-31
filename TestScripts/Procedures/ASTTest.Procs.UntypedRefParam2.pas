unit ASTTest.Procs.UntypedRefParam2;

interface

type
  TRecord = record
    procedure ParamVar(var X);
    procedure ParamOut(out X);
    procedure ParamConst(const X);
    procedure Main<T>(A: T);
  end;

implementation

{ TRecord }

procedure TRecord.Main<T>(A: T);
begin
  ParamVar(A);
  ParamOut(A);
  ParamConst(A);
end;

procedure TRecord.ParamConst(const X);
begin
end;

procedure TRecord.ParamOut(out X);
begin
end;

procedure TRecord.ParamVar(var X);
begin
end;

end.