unit ASTTest.NamesOverloading2;

interface

{$HINTS OFF}

type
  TBase = class
    function ToStr: string;
  end; 
  
  TChild = class(TBase)
    procedure Test1;
    procedure Test2;
  end;

implementation

{ TBase }

function TBase.ToStr: string;
begin
  Result := '';
end;

{ TChild }

procedure TChild.Test1;

  function ToStr: string;
  begin
    Result := '';
  end;

  function Get: string;
  begin
    Result := ToStr; // take local ToStr()
  end;

begin
  if Get <> '' then;
end;

function ToStr: string;
begin
  Result := '';
end;

procedure TChild.Test2;

  function Get: string;
  begin
    Result := ToStr; // take TBase ToStr()
  end;

  function ToStr: string;
  begin
    Result := '';
  end;

begin
  if Get <> '' then;
end;

end.