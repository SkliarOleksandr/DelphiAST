unit ASTTest.Generics.Nested1;

interface

{$HINTS OFF}

type
  TNested<T> = class
    Value: T;  
  end; 

  TGeneric<T> = class
  type
    //TNested = TNested<T>;
    TNested = class(TNested<T>)
    end;
  public
    Nested: TNested;      
  end;

var 
  R: TGeneric<Integer>;

implementation

procedure Main; 
begin
  R.Nested.Value := 5;
end;

end.