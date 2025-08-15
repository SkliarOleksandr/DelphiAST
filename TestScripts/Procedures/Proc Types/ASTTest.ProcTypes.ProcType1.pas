unit ASTTest.ProcTypes.ProcType1;

interface

{$HINTS OFF}

type 
  TStaticProc = procedure(A: Integer);  
  TStaticFunc = function: Integer;

  TObjectProc = procedure(A: Integer) of object;  
  TObjectFunc = function: Integer of object;

  TRefProc = reference to procedure(A: Integer);  
  TRefFunc = reference to function: Integer;

var
  StaticProc: TStaticProc;  
  StaticFunc: TStaticFunc;

  ObjectProc: TObjectProc;  
  ObjectFunc: TObjectFunc;

  RefProc: TRefProc;  
  RefFunc: TRefFunc;

implementation

procedure Main;
var
  LInt: Integer;
begin
  StaticProc(1);  
  LInt := StaticFunc();

  ObjectProc(1);  
  LInt := ObjectFunc();

  RefProc(1);  
  LInt := RefFunc();
end;

end.
