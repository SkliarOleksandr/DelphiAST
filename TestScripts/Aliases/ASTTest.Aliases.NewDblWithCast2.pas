unit ASTTest.Aliases.NewDblWithCast2;

interface

type
  TMyDouble1 = type Double;
  TMyDouble2 = type TMyDouble1;  

var
  _Dbl: Double;

implementation

procedure DoSmth(var AValue: TMyDouble1); overload;
begin
end;

procedure DoSmth(var AValue: TMyDouble2); overload;
begin
end;

procedure Main;
begin
  DoSmth(TMyDouble1(_Dbl));
  DoSmth(TMyDouble2(_Dbl));  
end;

end.