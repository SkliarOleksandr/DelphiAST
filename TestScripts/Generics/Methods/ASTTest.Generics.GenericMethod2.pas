unit ASTTest.Generics.GenericMethod2;

interface

type
  TArray<E> = array of E;

  TGeneric = record
    class procedure DoSmth1<T>(Arr: TArray<T>); static;
    class procedure DoSmth2<K>(Arr: TArray<K>); static;
  end;

implementation

{ TGeneric }

class procedure TGeneric.DoSmth1<T>(Arr: TArray<T>);
begin

end;

class procedure TGeneric.DoSmth2<K>(Arr: TArray<K>);
begin
  // 1. TGeneric1.DoSmth1 must be instantiated here with "K"
  // 1. TGeneric1.DoSmth1 must be instantiated here with "string"
  TGeneric.DoSmth1<K>(Arr);
end;

procedure Main;
begin
  TGeneric.DoSmth2<string>(['aaa', 'bbb']);
end;

end.