unit ASTTest.Generics.Methods;

interface

{$HINTS OFF}

type
  TStruct = record
    class procedure GProc<T>(Value: T); static;
  end;

implementation

procedure Main;
begin
  TStruct.GProc<Integer>(1);
end;

class procedure TStruct.GProc<T>(Value: T);
begin

end;

end.