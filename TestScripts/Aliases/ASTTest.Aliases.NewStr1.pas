unit ASTTest.Aliases.NewStr1;

interface

type  
  MyString = type String;  

implementation

function GetMyStr: MyString;
begin
  Result := '';
end;

end.