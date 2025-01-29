unit ASTTest.Generics.Constr2;

interface

{$HINTS OFF}

type

  TBase = class
    Data: Integer;
  end;

  TObjectList<T: TBase> = class
    procedure Add(const AObj: T);
  end;
  
  TMyClass = class
    Data: Integer;
  end;

implementation

{ TObjectList<T> }

procedure TObjectList<T>.Add(const AObj: T);
begin
  if TBase(AObj).Data > 0 then;
  if TMyClass(AObj).Data > 0 then;
end;

end.