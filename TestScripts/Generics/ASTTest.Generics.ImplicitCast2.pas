unit ASTTest.Generics.ImplicitCast2;

interface

{$HINTS OFF}

type
  TGeneric<T> = class
    A, B: T;
    function Clone: TGeneric<T>;
  end;

  TAlias = TGeneric<Integer>;

implementation

procedure Test(A: TAlias);
var
  B: TAlias;
begin
  B := A.Clone();
end;

{ TGeneric<T> }

function TGeneric<T>.Clone: TGeneric<T>;
begin
  Result := nil;
end;

end.