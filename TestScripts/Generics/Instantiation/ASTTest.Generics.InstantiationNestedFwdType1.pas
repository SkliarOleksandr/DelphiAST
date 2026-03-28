unit ASTTest.Generics.InstantiationNestedFwdType1;

interface

type

  TGeneric<T> = class
  type
    PNode = ^TNode; // forward pointer type!
    TNode = record    
      Value: T;
      Next: PNode;
    end;
  var
    Root: TNode;
    NextPtr: PNode;
  end;

var
  MyObj: TGeneric<Integer>;

implementation

procedure Main;
begin
  //MyObj.Root.Next := MyObj;
end;

end.