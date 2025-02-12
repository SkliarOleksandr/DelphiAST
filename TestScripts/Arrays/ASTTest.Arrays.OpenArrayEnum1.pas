unit ASTTest.Arrays.OpenArrayEnum1;

interface

type
  TEnum = (item1, item2, item3);
  TEnumArray = array of TEnum;  

procedure Add1(Items: array of TEnum);
procedure Add2(Items: TEnumArray);

implementation

procedure Main;
begin
  Add1([item1, item2]);
  Add2([item1, item2]);  
end;

procedure Add1(Items: array of TEnum);
begin
end;

procedure Add2(Items: TEnumArray);
begin
end;

end.