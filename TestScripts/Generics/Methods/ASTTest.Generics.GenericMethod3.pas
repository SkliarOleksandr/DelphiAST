unit ASTTest.Generics.GenericMethod3;

interface

type
  TArray<E> = array of E;

  TGeneric = record
    class procedure DoSmth<T>(Arr: TArray<T>); static;
  end;

implementation

{ TGeneric }

class procedure TGeneric.DoSmth<T>(Arr: TArray<T>);
begin
  // TGeneric.DoSmth must be instantiated here with "string" only!
  TGeneric.DoSmth<T>(Arr); 
end;

procedure Main;
begin
  TGeneric.DoSmth<string>(['aaa', 'bbb']);
end;

end.