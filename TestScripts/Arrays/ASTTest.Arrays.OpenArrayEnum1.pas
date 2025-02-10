unit ASTTest.Arrays.OpenArrayEnum1;

interface

type
  TEnum = (item1, item2, item3);

procedure Add(Items: array of TEnum);

implementation

procedure Main;
begin
  Add([item1, item2]);
end;

procedure Add(Items: array of TEnum);
begin
end;

end.