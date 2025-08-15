unit ASTTest.ProcTypes.ProcType3;

interface

{$HINTS OFF}

type
  TFunc = function: Integer;
   
var
  ArrOfFunc: array of TFunc;
  ArrOfInt: array of Integer;

implementation

function Func1: Integer; begin Result := 1; end; 

function Func2: Integer; begin Result := 2; end; 

procedure Main;
begin
  //ArrOfFunc := [Func1, Func2]; // E2010 Incompatible types: 'TFunc' and 'Integer'
  ArrOfFunc := [@Func1, @Func2];
  ArrOfInt := [Func1, Func2];

  ArrOfFunc := ArrOfFunc + [@Func1, @Func2];
  ArrOfInt := ArrOfInt + [Func1, Func2];
end;

end.