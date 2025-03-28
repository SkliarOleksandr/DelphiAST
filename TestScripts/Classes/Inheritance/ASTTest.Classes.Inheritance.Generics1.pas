unit ASTTest.Classes.Inheritance.Generics1;

interface

type
  TBase<T, K> = class
    function GetData: K; virtual; abstract;
  end;
  
  TChild<T> = class(TBase<T, Integer>)
    function GetData: Integer; override;
  end;


implementation

{ TChild<T> }

function TChild<T>.GetData: Integer;
begin
  Result := 0;
end;

end.