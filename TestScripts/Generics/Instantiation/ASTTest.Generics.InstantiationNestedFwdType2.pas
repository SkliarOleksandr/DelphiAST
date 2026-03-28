unit ASTTest.Generics.InstantiationNestedFwdType2;

interface

type
  TGeneric<T> = class
  type
    TValue = class; // forward class type!    
    TNode = record
      Value: TValue;
    end;
    TValue = class
      FValue: T;
    end;
  var
    FRoot: TNode;
  end;

var
  MyObj: TGeneric<Integer>;

implementation

end.