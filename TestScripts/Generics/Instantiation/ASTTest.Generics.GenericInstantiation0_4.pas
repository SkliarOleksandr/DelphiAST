unit ASTTest.Generics.GenericInstantiation0_4;

interface

type

  TMyObj<K1, K2> = class
  type
    TWraper<T> = record
      Value: T;
    end;  
  var
    ValueWraper1: TWraper<K1>;
    ValueWraper2: TWraper<K2>;
  end;

var
  MyObj: TMyObj<Integer, String>;

implementation

end.