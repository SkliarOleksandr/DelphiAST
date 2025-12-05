unit ASTTest.Generics.InstantiationAncestors1;

interface

{$HINTS OFF}

type

  TBase<T> = class
    Value: T;
  end;
  
  TGeneric<K> = class(TBase<K>)
  end;      

var
  MyObj: TGeneric<string>;

implementation

procedure Main;
begin
  MyObj.Value := 'Str';
end;

end.