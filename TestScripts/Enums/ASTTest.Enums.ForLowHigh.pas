unit ASTTest.Enums.ForLowHigh;

interface

type
  TEnum = (item1, item2, item3);

var
  GArray: array [TEnum] of Integer;

implementation

procedure Main;
begin
  for var LIndex := Low(TEnum) to High(TEnum) do
    if GArray[LIndex] > 0 then;
end;

end.