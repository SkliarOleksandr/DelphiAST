unit ASTTest.Generics.InplaceInstantiation1;

interface

type
  TRec = record
    class function Get<T>: T; static;
    class procedure SetValue<T>(Value: T); static;
  end;

  TRec<T> = record
    class var X: T;
  end;

  TFunc<T> = function: T;

var
  Ptr: Pointer;

implementation

procedure Main;
begin
  TRec<Integer>.X := 5;
  var X := TRec.Get<Integer>;
  TRec.SetValue<Integer>(1);
  var Y := TFunc<Integer>(Ptr);
end;

{ TRec }

class function TRec.Get<T>: T;
begin
  Result := Default(T);
end;

class procedure TRec.SetValue<T>(Value: T);
begin

end;

end.