unit ASTTest.Generics.Nested3;

interface

{$HINTS OFF}

type
  TNested<T1, T2> = class
    V1: T1;  
    V2: T2;
  end; 

  TGeneric<T> = class
  type
    TNested1 = class(TNested<T, Integer>)
    end;
    TNested2 = class(TNested<String, Integer>)
    end;
  private  
    F1: TNested1;
    F2: TNested2;
    F3: TNested<T, Integer>;
    F4: TNested<T, T>;
    F5: TNested<string, string>;
  end;

var 
  R: TGeneric<Integer>;

implementation

end.