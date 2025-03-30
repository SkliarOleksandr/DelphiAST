unit ASTTest.Classes.Inheritance.Generics1;

interface

type
  TBase<K, V> = class
    function GetData(AKey: K): V; virtual; abstract;
  end;
  
  TChild1<K> = class(TBase<K, Integer>)
    function GetData(AKey: K): Integer; override;
  end;

  TChild2 = class(TBase<string, Integer>)
    function GetData(AKey: string): Integer; override;  
  end;

implementation

{ TChild1<K> }

function TChild1<K>.GetData(AKey: K): Integer;
begin
  Result := 0;
end;

{ TChild2 }

function TChild2.GetData(AKey: string): Integer;
begin
  Result := 0;
end;

end.