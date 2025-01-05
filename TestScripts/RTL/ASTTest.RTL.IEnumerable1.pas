unit ASTTest.RTL.IEnumerable1;

interface

{$HINTS OFF}

implementation

procedure Test1(AValues:  IEnumerable<string>);
var
  Enumerator: IEnumerator<string>;
begin
  Enumerator := AValues.GetEnumerator;
end;

procedure Test2(AValues:  IEnumerable);
var
  Enumerator: IEnumerator;
begin
  Enumerator := AValues.GetEnumerator;
end;


end.