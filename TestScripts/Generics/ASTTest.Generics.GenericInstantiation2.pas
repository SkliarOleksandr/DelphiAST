unit ASTTest.Generics.GenericInstantiation2;

interface

type
  TMyClass = class
    class function DoSmth<T>(Arr: Integer): Integer;
  end;

implementation

{ TMyClass }

class function TMyClass.DoSmth<T>(Arr: Integer): Integer;
begin
  Result := TMyClass.DoSmth<T>(Arr);
end;

procedure Main;
begin
  TMyClass.DoSmth<Integer>(42);
end;

end.