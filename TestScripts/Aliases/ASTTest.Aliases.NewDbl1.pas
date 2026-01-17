unit ASTTest.Aliases.NewDbl1;

interface

type
  TMyDouble = type Double; 

implementation

function GetMyDouble: TMyDouble;
begin
  Result := 1;
end;

procedure Mian;
var
  LDbl: Double;
begin
  LDbl := GetMyDouble; 
end;

end.