unit ASTTest.Generics.IntfImpl1;

interface

type

  IIntf<T> = interface

  end;

  TGeneric<T> = class(TInterfacedObject, IIntf<T>)

  end;

  TMyObject = class(TGeneric<Integer>)

  end;

var
  Intf: IIntf<Integer>;

implementation

procedure Main;
begin
  Intf := TMyObject.Create;
end;

end.