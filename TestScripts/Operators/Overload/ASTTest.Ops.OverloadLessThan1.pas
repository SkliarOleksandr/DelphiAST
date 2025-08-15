unit ASTTest.Ops.OverloadLessThan1;

interface

type

  TMyType<T> = record
    class operator LessThan(ALeft: TMyType<T>; ARight: TClass): Boolean; overload; static;
    class operator LessThan(ALeft: TClass; ARight: TMyType<T>): Boolean; overload; static;
  end;

  TGeneric<T> = class
  end;

implementation

procedure Main;
var
  LVar: TMyType<string>;
begin
  if LVar < TObject then;
  if TObject < LVar then;

  if LVar < TGeneric<string> then;
  if TGeneric<string> < LVar then;
end;

{ TMyType }

class operator TMyType<T>.LessThan(ALeft: TMyType<T>; ARight: TClass): Boolean;
begin
  Result := False;
end;

class operator TMyType<T>.LessThan(ALeft: TClass; ARight: TMyType<T>): Boolean;
begin
  Result := False;
end;

end.