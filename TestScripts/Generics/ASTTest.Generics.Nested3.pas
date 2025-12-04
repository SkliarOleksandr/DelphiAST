unit ASTTest.Generics.Nested3;

interface

{$HINTS OFF}

type
  TNested<T1, T2> = class
    V1: T1;  
    V2: T2;
  end; 

  TGeneric<K> = class
  type
    TNested1 = class(TNested<K, Integer>)
    end;
    TNested2 = class(TNested<String, Integer>)
    end;
  private  
    F1: TNested1;
    F2: TNested2;
    F3: TNested<K, Integer>;
    F4: TNested<K, K>;
    F5: TNested<string, string>;
  end;

var 
  R: TGeneric<String>;

implementation

procedure Main;
begin
  R.F1.V1 := 'str';   
  R.F1.V2 := 5;
  
  R.F2.V1 := 'str';
  R.F2.V2 := 5;
  
  R.F3.V1 := 'str';
  R.F3.V2 := 5;  

  R.F4.V1 := 'str';
  R.F4.V2 := '5';

  R.F5.V1 := 'str';
  R.F5.V2 := '5';
end;

end.