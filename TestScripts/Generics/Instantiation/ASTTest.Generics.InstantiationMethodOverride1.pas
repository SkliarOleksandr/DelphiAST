unit ASTTest.Generics.InstantiationMethodOverride1;

interface

type
  TBase<T> = class
    function GetSelf: TBase<T>; virtual; abstract;
  end;

  TChild<K> = class(TBase<K>)
    // NOTE: TBase<K> here is strictly the same as TBase<T>
    // Expected: compile with no errors
    // Actual: E2170 Cannot override a non-virtual method  
    function GetSelf: TBase<K>; override;
  end;

var
  MyBase: TBase<string>;
  MyChild: TChild<string>;

implementation

{ TChild<K> }

function TChild<K>.GetSelf: TBase<K>;
begin
  Result := Self;
end;

procedure Main;
begin
  //MyChild := TChild<string>.Create;
  Assert(MyChild.GetSelf = MyChild); 
end;


end.