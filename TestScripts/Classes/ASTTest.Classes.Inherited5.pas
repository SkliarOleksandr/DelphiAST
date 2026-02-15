unit ASTTest.Classes.Inherited5;

interface

type
  TBase = class
    function DoSmth: Integer;
  end;

  TChild = class(TBase)
    function DoSmth: Integer; virtual;
  end;

implementation

{ TChild }

function TChild.DoSmth: Integer;
begin
  // in Delphi it works!
  Result := inherited;
end;

{ TBase }

function TBase.DoSmth: Integer;
begin
  Result := 5;
end;

end.
