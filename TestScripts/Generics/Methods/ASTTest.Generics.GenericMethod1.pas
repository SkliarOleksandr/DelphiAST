unit ASTTest.Generics.GenericMethod1;

interface

type
  TArray<E> = array of E;

  TGeneric1 = record
    class procedure DoSmth1<T>(Arr: TArray<T>); static;
  end;

  TGeneric2 = record
    class procedure DoSmth2<K>(Arr: TArray<K>); static;
  end;

implementation

{ TGeneric1 }

class procedure TGeneric1.DoSmth1<T>(Arr: TArray<T>);
begin

end;

{ TGeneric2 }

class procedure TGeneric2.DoSmth2<K>(Arr: TArray<K>);
begin
  // 1. TGeneric1.DoSmth1 must be instantiated here with "K"  
  // 1. TGeneric1.DoSmth1 must be instantiated here with "string"  
  TGeneric1.DoSmth1<K>(Arr);
end;

procedure Main;
begin
  TGeneric2.DoSmth2<string>(['aaa', 'bbb']);
end;

end.