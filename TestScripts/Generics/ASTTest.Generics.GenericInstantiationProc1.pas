unit ASTTest.Generics.GenericInstantiationProc1;

interface

type
  
  THelper = record
    class procedure DoSmth<T>(Value: T); static;
  end;

  TList<T> = class
    procedure Add(AItem: T); 
  end;

var
  List: TList<Integer>;

implementation

{ THelper }

class procedure THelper.DoSmth<T>(Value: T);
begin

end;

{ TList<T> }

procedure TList<T>.Add(AItem: T);
begin
  THelper.DoSmth<T>(AItem);
end;

end.