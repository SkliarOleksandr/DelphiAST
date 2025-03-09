unit ASTTest.Generics.MethodWithConstr;

interface

{$HINTS OFF}

type
  TMyClass = class
    class function Cast<TSrc, TDst: class>(ASrc: TSrc): TDst;
  end;
  
implementation

class function TMyClass.Cast<TSrc, TDst>(ASrc: TSrc): TDst;
begin
  Result := ASrc as TDst;
end;

end.