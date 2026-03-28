unit ASTTest.Generics.InstantiationNestedPtrType1;

interface

type

  TGeneric<T> = class
  type
    TNode = record
      Value: T;
    end;
    PNode = ^TNode; // normal (not forward) pointer type
  var
    FRoot: TNode;
    FRootPtr: PNode;
  end;

var
  MyObj: TGeneric<Integer>;

implementation

procedure Main;
begin
  MyObj.FRootPtr := @MyObj.FRoot;
end;

end.