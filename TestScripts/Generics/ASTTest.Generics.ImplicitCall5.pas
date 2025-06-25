unit ASTTest.Generics.ImplicitCall5;

interface

type
  TRec = record
    class function GetInt: Integer; overload; static;
    class function GetInt<T>: Integer; overload; static;
  end;


implementation

{ TRec }

procedure Main;
begin
  if TRec.GetInt < 5 then;
  if TRec.GetInt<Integer> < 5 then;
end;

class function TRec.GetInt: Integer;
begin
  Result := 0;
end;

class function TRec.GetInt<T>: Integer;
begin
  Result := 0;
end;

end.