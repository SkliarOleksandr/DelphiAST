unit ASTTest.Ops.AddPUChar;

interface

{$HINTS OFF}

var
  PCh: PChar;
  Str: string;

implementation

procedure Main;
begin
  //Str := PCh + PCh; // E2015 Operator not applicable to this operand type
  Str := PCh + Str;
  Str := Str +  PCh; 
end;

end.