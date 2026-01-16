unit ASTTest.Generics.GenericInstantiation0_1;

interface

type

  TWraper<T> = record
    Value: T;
  end;

  TMyObj<K1, K2> = class
  type
    TRec1 = record
      Value: TWraper<K1>;
    end;
    TRec2 = record
      Value: TWraper<K2>;
    end;
  var
    ValueWraper1: TRec1;
    ValueWraper2: TRec2;
  end;

var
  MyObj: TMyObj<Integer, String>;

implementation

end.