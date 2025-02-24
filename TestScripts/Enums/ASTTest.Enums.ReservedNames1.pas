unit ASTTest.Enums.ReservedNames1;

interface

type
  {$SCOPEDENUMS ON} 
  TEnum = (
    Integer,
    &string
  );
  TSet = set of TEnum;

var
  GSet: TSet;

implementation

procedure Main;
begin
  GSet := [TEnum.Integer, TEnum.string];
end;

end.