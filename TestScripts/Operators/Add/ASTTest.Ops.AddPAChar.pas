unit ASTTest.Ops.AddPAChar;

interface

{$HINTS OFF}

var
  PCh: PAnsiChar;
  Str: AnsiString;

implementation

procedure Main;
begin
  // Str := PCh + PCh; // E2015 Operator not applicable to this operand type
  Str := PCh + Str;
  Str := Str +  PCh; 
end;

end.