unit ASTTest.Generics.GenericInstantiation0_5;

interface

type
  TList<T> = class
    Parent: TList<T>;   
    procedure Assign(Source: TList<T>);
  end;

var
  List: TList<Integer>;

implementation

{ TList<T> }

procedure TList<T>.Assign(Source: TList<T>);
begin

end;

end.