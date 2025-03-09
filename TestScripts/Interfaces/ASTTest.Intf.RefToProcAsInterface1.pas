unit ASTTest.Intf.RefToProcAsInterface1;

interface

type

  TIntFunc = reference to function: Integer;

  TFunctor = class(TInterfacedObject, TIntFunc)
  public
    function Invoke: Integer;
  end;

implementation

{ TFunctor }

function TFunctor.Invoke: Integer;
begin
  Result := 0;
end;

end.