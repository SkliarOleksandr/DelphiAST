unit ASTTest.Ops.IfThen1;

interface

implementation

function Abs(AValue: Integer): Integer;
begin
  Result := if AValue > 0 then AValue else 5;
end;

end.