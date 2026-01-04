unit ASTTest.Generics.OverloadProc3;

interface

type
  TProc<T1> = reference to procedure (P1: T1);
  TProc<T1, T2> = reference to procedure (P1: T1; P2: T2);

  TRecord = record
    class procedure Run(AProc: TProc<Integer>); overload; static;
    class procedure Run(AProc: TProc<Integer, String>); overload; static;
  end;

implementation


{ TRecord }

class procedure TRecord.Run(AProc: TProc<Integer, String>);
begin

end;

class procedure TRecord.Run(AProc: TProc<Integer>);
begin

end;

end.