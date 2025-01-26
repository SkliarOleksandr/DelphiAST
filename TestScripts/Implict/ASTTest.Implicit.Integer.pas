unit ASTTest.Implicit.Integer;

interface

{$HINTS OFF}

implementation

{$I sassert.inc}

procedure Main;
var
  LByte: Byte;
begin
  LByte := 1;
  var LRes := (LByte and $f0);
  StaticAssert(SizeOf(LRes) = 1);
end;

initialization
  Main();

end.