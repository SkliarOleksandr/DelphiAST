unit ASTTest.Generics.GenericInstantiation0;

interface

type
  TWraper<T> = record
    Value: T;   
  end;
  
  TMyObj<K> = class
    ValueWraper1: TWraper<K>; // this is generic instantiation promise 
    ValueWraper2: TWraper<K>; // this is generic instantiation promise
  end;

var
  Obj: TMyObj<Integer>; 

implementation

procedure Main;
begin
  Obj.ValueWraper1.Value := 1;
  Obj.ValueWraper2.Value := 2; 
end;

end.