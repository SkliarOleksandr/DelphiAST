unit ASTTest.Variants.DynArray1;

interface

{$HINTS OFF}

type
  TIntArray = array of Integer;
  TDblArray = array of Double;
  TStrArray = array of string;
  TBlnArray = array of string;

implementation

procedure Main;
var
  LValue: Variant;
  LIntArray: TIntArray;
  LDblArray: TDblArray;
  LStrArray: TStrArray;
  LBlnArray: TBlnArray;
  Dim2Array: array of array of Integer;
  Dim3Array: array of array of array of Integer;
begin
  LValue := LIntArray;
  LValue := LDblArray;
  LValue := LStrArray;
  LValue := LBlnArray;
  LValue := Dim2Array;
  LValue := Dim3Array;
end;

end.