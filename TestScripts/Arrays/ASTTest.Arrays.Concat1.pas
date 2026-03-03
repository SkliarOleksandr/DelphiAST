unit ASTTest.Arrays.Concat1;

interface

type
  TIntArray = array of Integer;
  TUIntArray = array of Cardinal;

implementation

function GetArray1: TIntArray;
begin
  // case 1 - const array element type (Integer) is precisely the same  as for the named array (Integer) 
  Result := [1, 2, 3] + Result;
end;

function GetArray2: TUIntArray;
begin
  // case 2 - const array element type (Integer) is a bit different from the named array one  (Cardinal) 
  Result := [1, 2, 3] + Result;
end;

end.