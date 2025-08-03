unit ASTTest.Classes.OverloadMethodNoParams2;

interface

{$HINTS OFF}

type
  TBase = class
    function Add: Integer; overload;
  end;

  TChild = class(TBase)
    function Add<T>: T; overload;
  end;

implementation

procedure Main(AChild: TChild);
var
  LResult: string;
begin
  // LResult := AChild.Add;  // E2531 Method 'Add' requires explicit type argument(s)
  LResult := AChild.Add<string>;
end;

{ TBase }

function TBase.Add: Integer;
begin
  Result := 0;
end;

{ TChild }

function TChild.Add<T>: T;
begin
  Result := Default(T);
end;

end.