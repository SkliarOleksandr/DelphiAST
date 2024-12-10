unit ASTTest.Generics.GenResult;

interface

{$HINTS OFF}

type
  TRec<T> = class
    class function Get: T;
  end;

implementation

procedure Main;
begin
  TRec<string>.Get();
end;


{ TRec<T> }

class function TRec<T>.Get: T;
begin
  Result := Default(T);
end;

end.