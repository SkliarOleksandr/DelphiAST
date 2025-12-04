unit ASTTest.Generics.GenericInstantiation0_6;

interface

{$HINTS OFF}

type
  TWrapper<T> = class
    Value: T;
  end;

  TBase<K> = class
  public
    function GetValue: TWrapper<K>;
  end;

  TGeneric<X> = class(TBase<X>)
  end;

var
  Generic: TGeneric<Integer>;
  
implementation

procedure Main;
begin
  if Generic.GetValue.Value > 0 then;
end;

{ TBase<K> }

function TBase<K>.GetValue: TWrapper<K>;
begin
  Result := nil;
end;

end.