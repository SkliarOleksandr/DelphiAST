unit ASTTest.Procs.ImplicitCall1;

interface

implementation

type
  TIntFunc = function: Integer;

var
  IntFunc: TIntFunc;

function GetInt: Integer;
begin
  Result := 0;
end;

procedure DoIt(AInt: Integer); overload; begin end;
procedure DoIt(AFunc: TIntFunc); overload; begin end;

procedure Main;
begin
  DoIt(GetInt);    // calls 1st
  DoIt(IntFunc);   // calls 2nd
end;


end.