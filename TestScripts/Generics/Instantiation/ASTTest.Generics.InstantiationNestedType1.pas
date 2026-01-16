unit ASTTest.Generics.InstantiationNestedType1;

interface

type

  TWrapper<T> = record
    Value: T;
  end;

  TMyObj<K> = class
  type
    TRec = record
      Wrapped: TWrapper<K>;
    end;
  var
    Rec: TRec;
  end;

var
  MyObj: TMyObj<Integer>;

implementation

procedure Main;
begin
  MyObj.Rec.Wrapped.Value := 5;
end;

end.