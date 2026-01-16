unit ASTTest.Generics.InstantiationPointers1;

interface

type
  
  TRec<T> = record
  type
    PItem = ^T;
  var
    FItem: T;  
  end;

var
  Rec: TRec<Integer>;
  PRec: TRec<Integer>.PItem;

implementation

procedure Main;
begin
  PRec := @Rec.FItem;
end;

end.