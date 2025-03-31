unit ASTTest.Generics.GenericInstantiation1;

interface

type
  IComparer<T> = interface
  end;

  TList<K, V> = class
    constructor Create(AComparer: IComparer<K>);  
  end;
 
  TListBuilder<T> = class
    function Make(AComparer: IComparer<T>): TList<T, Integer>;
  end; 

implementation

{ TList<K> }

constructor TList<K, V>.Create(AComparer: IComparer<K>);
begin
end;

{ TListBuilder<T> }

function TListBuilder<T>.Make(AComparer: IComparer<T>): TList<T, Integer>;
begin
  Result := TList<T, Integer>.Create(AComparer);
end;

end.