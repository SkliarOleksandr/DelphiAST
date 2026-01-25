unit ASTTest.Aliases.NewIntWithCast1;

interface

type
  TMyInteger = type Integer;

var
  _Int: Integer;

implementation

procedure DoSmth(var AValue: TMyInteger);
begin
end;

procedure Main;
begin
  //DoSmth(_Int); // Expected: E2033 Types of actual and formal var parameters must be identical: TMyInteger and Integer
  DoSmth(TMyInteger(_Int));
end;

end.