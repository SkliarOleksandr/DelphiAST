unit ASTTest.Generics.Intf.Delegation;

interface

type
  IIntf<T> = interface
    function GetData: T;
  end;

  TMyObj<T> = class(TInterfacedObject, IIntf<T>)
    function GetMyData: T;   
    function IIntf<T>.GetData = GetMyData;
  end;

implementation

{ TMyObj<T> }

function TMyObj<T>.GetMyData: T;
begin
  Result := Default(T);
end;

end.