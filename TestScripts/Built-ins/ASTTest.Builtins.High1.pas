unit ASTTest.Builtins.High1;

interface

type
  TEnum = (item1, item2, item3, item4);
  
var
  GEnum: TEnum;
  GArray1: array[TEnum] of Integer;  
  GArray2: array[item2..item3] of Integer;

implementation

procedure Main;
begin
  if GEnum > High(TEnum) then;
  if GEnum > High(GArray1) then;
  if GEnum > High(GArray2) then;    
  
  if GEnum > Low(TEnum) then;
  if GEnum > Low(GArray1) then;
  if GEnum > Low(GArray2) then;      
end;

end.