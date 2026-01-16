unit ASTTest.Generics.InstantiationAncestors1;

interface

{$HINTS OFF}

type

  TBase<T> = class
    Value: T;
  end;

  TChild<K> = class(TBase<K>)
  end;

var
  MyObj: TChild<string>;

implementation

procedure Main;
begin
  MyObj.Value := 'Str';
end;

end.