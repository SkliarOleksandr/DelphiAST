unit ASTTest.Procs.OverloadDecl2;

interface

function GetValue(AValue: Double): Double; overload;

implementation

function GetValue(AValue: Double): Double;
begin
  Result := AValue;
end;

end.