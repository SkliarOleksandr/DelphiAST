unit ASTTest.Procs.OverloadDecl1;

interface

function GetValue(AValue: Integer): Integer; overload;

implementation

function GetValue(AValue: Integer): Integer;
begin
  Result := AValue;
end;

end.