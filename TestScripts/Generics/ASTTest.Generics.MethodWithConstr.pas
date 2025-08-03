unit ASTTest.Generics.MethodWithConstr;

interface

{$HINTS OFF}

type
  TMyClass = class
    class function Cast<TSrc, TDst: class>(ASrc: TSrc): TDst;
    class function Cast2<TSrc: class; TDst: class>(ASrc: TSrc): TDst;
  end;
  
implementation

class function TMyClass.Cast2<TSrc, TDst>(ASrc: TSrc): TDst;
begin
  Result := ASrc as TDst;
end;

class function TMyClass.Cast<TSrc, TDst>(ASrc: TSrc): TDst;
begin
  Result := ASrc as TDst;
end;

end.