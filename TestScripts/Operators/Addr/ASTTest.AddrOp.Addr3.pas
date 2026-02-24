unit ASTTest.AddrOp.Addr3;

interface

type
  TProc = procedure;

var
  Proc: TProc;
  Ptr1, Ptr2: Pointer;

implementation

procedure Main;
begin
  Ptr1 := @Proc;
  if Ptr1 <> nil then;

  Ptr2 := @@Proc;
  if Ptr2 <> nil then;
end;


initialization
  Main;
end.