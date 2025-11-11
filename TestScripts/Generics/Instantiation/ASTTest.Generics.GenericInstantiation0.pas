unit ASTTest.Generics.GenericInstantiation0;

interface

type
  TWraper<T> = record
    Value: T;   
  end;
  
  TMyObj<K> = class
    ValueWraper1: TWraper<K>; // here, TWrapper<T> will be partially instantiated with <K>
    ValueWraper2: TWraper<K>; // here, TWrapper<K> will be re-used from the pool
  end;

var
  Obj: TMyObj<Integer>; // here, TMyObj<K>, and TWrapper<K> will be instantiated with <Integer>

implementation

//procedure Main;
//begin
//  Obj.ValueWraper1.Value := 1;
//  Obj.ValueWraper2.Value := 2; 
//end;

end.