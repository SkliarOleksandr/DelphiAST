unit ASTTest.Generics.Inheritance1;

interface

type
  TGeneric<T> = class
  end;
  
  TMyObject = class(TGeneric<String>)
  end;

var
  MyObj: TMyObject;

implementation

procedure Main; 
begin
  MyObj := TMyObject.Create;
end;

end.