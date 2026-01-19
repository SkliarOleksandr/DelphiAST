unit ASTTest.Procs.VarOutParams1;

interface

{$HINTS OFF}

implementation

procedure Proc1(var I: Integer);
begin

end;

procedure Proc2(out I: Integer);
begin

end;

procedure Main;
var
  LInt: Integer;
  LByte: Byte;
begin
  // Proc1(LByte); // [dcc32 Error] ASTTest.Procs.VarOutParams1.pas(16): E2033 Types of actual and formal var parameters must be identical: Integer and Byte
  Proc1(LInt);

  // Proc2(LByte); // [dcc32 Error] ASTTest.Procs.VarOutParams1.pas(16): E2033 Types of actual and formal var parameters must be identical: Integer and Byte
  Proc2(LInt);
end;

end.