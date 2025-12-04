unit ASTTest.Generics.Intf.Delegation;

interface

type
  IIntf<T> = interface
    function GetData: T;
  end;

  TMyObj<K> = class(TInterfacedObject, IIntf<K>)
    function GetMyData: K;
    function IIntf<K>.GetData = GetMyData;
  end;

implementation

{ TMyObj<T> }

function TMyObj<K>.GetMyData: K;
begin
  Result := Default(K);
end;

end.