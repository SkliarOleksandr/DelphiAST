unit ASTTest.Generics.GenericMethod5;

interface

type
  TGeneric = record
    class procedure GetAsValue<T>(out AValue: T); static;
    class function GetValue<T>: T; static;
  end;

implementation

procedure Main;
var
  LInt: Integer;
  LStr: string;
begin
  TGeneric.GetAsValue({out} LInt);
  TGeneric.GetAsValue({out} LStr);

  LInt := TGeneric.GetValue<Integer>;
  LStr := TGeneric.GetValue<string>;
end;

{ TGeneric }

class procedure TGeneric.GetAsValue<T>(out AValue: T);
begin

end;

class function TGeneric.GetValue<T>: T;
begin
  GetAsValue(Result);
end;

end.