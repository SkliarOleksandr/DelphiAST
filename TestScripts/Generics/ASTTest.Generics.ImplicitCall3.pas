unit ASTTest.Generics.ImplicitCall3;

interface

type
  TRec = record
    class function GetValue<T>: T; static;
  end;

  TGetStrValueFunc = function: string;
  TGetIntValueFunc = function: Integer;

var
  _GetStr: TGetStrValueFunc;
  _GetInt: TGetIntValueFunc;

implementation

procedure Main;
begin
  _GetStr := TRec.GetValue<string>;
  _GetInt := TRec.GetValue<Integer>;
end;

class function TRec.GetValue<T>: T;
begin
  Result := Default(T);
end;

end.