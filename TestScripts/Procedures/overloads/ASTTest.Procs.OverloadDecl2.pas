unit ASTTest.Procs.OverloadDecl2;

interface

function GetValue(AValue: string): string; overload;

implementation

function GetValue(AValue: string): string;
begin
  Result := AValue;
end;

end.