unit ASTTest.Proc.ParamName1;

interface

type
  HANDLE = type Integer;

procedure Test(Handle: HANDLE);

implementation

procedure Test(Handle: HANDLE);
begin
  if Handle <> 0 then;  
end;

end.