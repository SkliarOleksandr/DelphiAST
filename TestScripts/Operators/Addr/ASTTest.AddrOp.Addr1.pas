unit ASTTest.AddrOp.Addr1;

interface

var
  ProcPtr: procedure = nil;
  FuncPtr: function: Integer = nil;

implementation

procedure Main;
begin
  if @ProcPtr <> nil then;
  if @FuncPtr <> nil then;
end;

end.