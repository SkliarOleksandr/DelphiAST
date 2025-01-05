unit ASTTest.Procs.Overloads.ImplicitCall1;

interface

{$HINTS OFF}

function Get: Integer; overload;
function Get(V: Integer): string; overload;

implementation

procedure Main;
var 
  LResult: Integer;
begin
  LResult := Get;
end;

function Get: Integer;
begin
  Result := 0;
end;

function Get(V: Integer): string;
begin
  Result := '';
end;

end.