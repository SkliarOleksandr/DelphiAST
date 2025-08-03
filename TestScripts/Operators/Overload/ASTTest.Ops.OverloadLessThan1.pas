unit ASTTest.Ops.OverloadLessThan1;

interface

type

  TMyType<T> = record
    class operator LessThan(ALeft: TMyType<T>; ARight: TClass): Boolean; static;
  end;

implementation

procedure Main;
var
  LVar: TMyType<string>;
begin
  if LVar < TObject then;
end;

{ TMyType }

class operator TMyType<T>.LessThan(ALeft: TMyType<T>; ARight: TClass): Boolean;
begin
  Result := False;
end;

end.