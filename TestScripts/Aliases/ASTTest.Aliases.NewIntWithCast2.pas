unit ASTTest.Aliases.NewIntWithCast2;

interface

type
  TMyInteger1 = type Integer;
  TMyInteger2 = type TMyInteger1;

var
  _Int: Integer;

implementation

procedure DoSmth(var AValue: TMyInteger1); overload;
begin
end;

procedure DoSmth(var AValue: TMyInteger2); overload;
begin
end;

procedure Main;
begin
  DoSmth(TMyInteger1(_Int));
  DoSmth(TMyInteger2(_Int));
end;

end.