unit ASTTest.Generics.OverloadProc3;

interface

{$HINTS OFF}

type
  TProc<T1> = reference to procedure (P1: T1);
  TProc<T1, T2> = reference to procedure (P1: T1; P2: T2);

  TRecord = record
    class procedure Run(AProc: TProc<Integer>); overload; static;
    class procedure Run(AProc: TProc<Integer, String>); overload; static;
  end;

implementation

{ TRecord }

class procedure TRecord.Run(AProc: TProc<Integer>);
begin
  AProc(1);
end;

class procedure TRecord.Run(AProc: TProc<Integer, String>);
begin
  AProc(1, '2');
end;

end.