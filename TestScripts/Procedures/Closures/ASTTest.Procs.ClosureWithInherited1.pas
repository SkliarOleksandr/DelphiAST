unit ASTTest.Procs.ClosureWithInherited1;

interface

type
  TProc = reference to procedure;

  TBase = class
    procedure DoSmth; virtual; abstract;
  end;

  TChild = class(TBase)
    procedure DoSmth; override;
    procedure RunAsync(AProc: TProc);
  end;

implementation

{ TChild }

procedure TChild.DoSmth;
begin
  RunAsync(procedure
           begin
             inherited DoSmth();
           end);
end;

procedure TChild.RunAsync(AProc: TProc);
begin
  AProc();
end;

end.