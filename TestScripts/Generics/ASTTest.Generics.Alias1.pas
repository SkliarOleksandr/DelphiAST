unit ASTTest.Generics.Alias1;

interface

type
  TList<T> = class
  type
    TItem = T;
  var
    FItem: TItem;
  end;

var
  List: TList<Integer>;

implementation

end.