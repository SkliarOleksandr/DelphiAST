unit ASTTest.Strings.ArrayOfChar2;

interface

implementation

var 
  UCharSArr: array [1..15] of Char;

function GetStr: string;
begin
  Result := string('\') + UCharSArr;
end;  

end.