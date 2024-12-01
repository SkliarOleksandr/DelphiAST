unit ASTTest.Sub.Integer;

interface

implementation

procedure SubIntVsFlt;
var
  LInt32: Integer;
  LFlt32: Single;
begin
  LInt32 := 0;
  LFlt32 := 0;

  //LFlt32 := LFlt32 - LInt32;
  LFlt32 := LInt32 - LFlt32;
end;

end.