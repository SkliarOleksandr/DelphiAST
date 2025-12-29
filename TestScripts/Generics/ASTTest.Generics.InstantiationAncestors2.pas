unit ASTTest.Generics.InstantiationAncestors2;

interface

{$HINTS OFF}

type

  TItem<T> = record
    Value: T;
  end;

  TBase<TItem> = class abstract
    Item: TItem;
  end;

  TChild<K> = class(TBase<TItem<K>>)

  end;

var
  MyObj: TChild<string>;

implementation

procedure Main;
begin
  MyObj.Item.Value := 'Str';
end;

end.