unit ASTTest.Generics.Constr3;

interface

type
  TBase = class
    Field1: Integer;
    Field2: Integer;
  end;

  TGeneric<T: TBase> = class
    procedure DoSmth(AObj: T);
  end;

implementation

{ TGeneric<T> }

procedure TGeneric<T>.DoSmth(AObj: T);
begin
  if AObj.Field1 <> AObj.Field2 then
end;

end.