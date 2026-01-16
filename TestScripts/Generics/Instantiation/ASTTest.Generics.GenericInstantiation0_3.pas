unit ASTTest.Generics.GenericInstantiation0_3;

interface

type

  TWraper<T> = record
    Value: T;
  end;

  TMyObj<K1, K2> = class
    ValueWraper1: TWraper<K1>;
    ValueWraper2: TWraper<K2>;
  end;

var
  MyObj: TMyObj<Integer, String>;

implementation

end.