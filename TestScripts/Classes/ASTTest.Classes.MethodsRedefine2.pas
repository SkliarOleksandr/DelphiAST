unit ASTTest.Classes.MethodsRedefine2;

interface

{$HINTS OFF}

type
  TBase = class
    function ToInt: Integer;
  end;

  TChild = class(TBase)
    function ToInt(A: Boolean): Integer;
  end;

implementation

procedure Main(AChild: TChild);
begin
  //var LResult: Integer := AChild.ToInt; // E2035 Not enough actual parameters
  var LResult: Integer := AChild.ToInt(True);
end;

{ TBase }

function TBase.ToInt: Integer;
begin
  Result := 0;
end;

{ TChild }

function TChild.ToInt(A: Boolean): Integer;
begin
  Result := 0;
end;

end.