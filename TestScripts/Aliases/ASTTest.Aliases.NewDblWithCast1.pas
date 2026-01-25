unit ASTTest.Aliases.NewDblWithCast1;

interface

type
  TMyDouble = type Double;

var
  _Dbl: Double;

implementation

procedure DoSmth(var AValue: TMyDouble);
begin
end;

procedure Main;
begin
  // DoSmth(_Dbl); // Expected: E2033 Types of actual and formal var parameters must be identical: TMyDouble and Double
  DoSmth(TMyDouble(_Dbl));
end;

end.