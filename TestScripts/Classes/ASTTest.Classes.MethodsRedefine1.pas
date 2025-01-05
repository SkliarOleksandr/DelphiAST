unit ASTTest.Classes.MethodsRedefine1;

interface

{$HINTS OFF}

type
  TBase = class
    function ToInt: Integer;
  end;

  TChild = class(TBase)
    function ToInt: Integer; virtual; 
  end;

implementation

procedure Main(AChild: TChild);
begin
  var LResult: Integer := AChild.ToInt;
end;

{ TBase }

function TBase.ToInt: Integer;
begin
  Result := 0;
end;

{ TChild }

function TChild.ToInt: Integer;
begin
  Result := 0;
end;

end.