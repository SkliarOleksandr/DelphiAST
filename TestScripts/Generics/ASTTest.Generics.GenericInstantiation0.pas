unit ASTTest.Generics.GenericInstantiation0;

interface

type
  TWraper<T> = record
    Value: T;   
  end;
  
  TMyObj<K> = class
    ValueWraper: TWraper<K>; // this is generic instantiation promise 
  end;

var
  Obj: TMyObj<Integer>; 

implementation

procedure Main;
begin
  Obj.ValueWraper.Value := 1; 
end;

end.