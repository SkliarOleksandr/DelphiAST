unit ASTTest.Generics.Class1;

interface

{$HINTS OFF}

type
  TList<T> = class
    F: T;
  public
    procedure Add(Value: T); 
  end;

var
  List: TList<Integer>;

implementation

procedure TList<T>.Add(Value: T);
begin
  var X := procedure (A: T)
           begin

           end;
end;

end.
