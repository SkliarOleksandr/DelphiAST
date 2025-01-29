unit ASTTest.Generics.Constr1;

interface

{$HINTS OFF}

type
  TObjectList<T: class> = class
    procedure Add(const AObj: T);
  end;

  TMyClass = class
    Data: Integer;
  end;

implementation

{ TObjectList<T> }

procedure TObjectList<T>.Add(const AObj: T);
begin
  if TMyClass(AObj).Data > 0 then;
end;

end.