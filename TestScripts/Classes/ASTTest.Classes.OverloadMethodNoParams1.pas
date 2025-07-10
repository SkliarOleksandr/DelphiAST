unit ASTTest.Classes.OverloadMethodNoParams1;

interface

{$HINTS OFF}

type
  TBase = class
    function Add: Integer;
  end;

  TChild = class(TBase)
    function Add: string; overload;
  end;


implementation

procedure Main(AChild: TChild);
var
  LResult: string;
begin
  LResult := AChild.Add;
end;

{ TBase }

function TBase.Add: Integer;
begin
  Result := 0;
end;

{ TChild }

function TChild.Add: string;
begin

end;

end.
