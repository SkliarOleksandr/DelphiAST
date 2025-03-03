unit ASTTest.Genrics.RecursiveUsing1;

interface

{$HINTS OFF}

type
  TValue = record
    class var FEmpty: TValue;
    class function CreateFrom<T>(AValue: T): TValue; static;
  end;

implementation

procedure Main;
var
  LInt: TValue;
begin
  LInt := TValue.CreateFrom<Integer>(1);
end;

{ TValue }

class function TValue.CreateFrom<T>(AValue: T): TValue;
begin
  Result := FEmpty;
end;

end.