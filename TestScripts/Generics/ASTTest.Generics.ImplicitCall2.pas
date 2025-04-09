unit ASTTest.Generics.ImplicitCall2;

interface

type
  TRec = record
    class function GetValue<T>: T; static; 
  end;

implementation

procedure EnsureStr(var AStr: string);
begin
end;

procedure Main;
begin
  var LValue := TRec.GetValue<string>;
  EnsureStr(LValue);  
end;

class function TRec.GetValue<T>: T;
begin
  Result := Default(T);
end;

end.