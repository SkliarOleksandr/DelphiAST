unit ASTTest.Arrays.MultiDimention1;

interface

type
  TArr1 = array [0..7, 0..7] of Byte;
  TArr2 = array [0..7] of array [0..7] of Byte;

var
  Arr1: TArr1;
  Arr2: TArr2;

implementation

procedure DoSmth(AArr: array of Byte);
begin
end;

procedure Main;
begin
  Arr1[0, 0] := 1;
  Arr1[0][0] := 1; 
    
  if Length(Arr1[0]) > 0 then
    DoSmth(Arr1[0]);    
  
  Arr2[0, 0] := 1;
  Arr2[0][0] := 1;
    
  if Length(Arr2[0]) > 0 then
    DoSmth(Arr2[0]);    
end;

end.