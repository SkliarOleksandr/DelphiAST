unit ASTTest.Generics.Overload1;

interface

type
  TList = class
    constructor Create;
  end;
  
  TList<T> = class(TList)
  end;
   
  TList<T1, T2> = class(TList)
  end;   

  TList<T1, T2, T3> = class(TList)
  end;  

var
  L0: TList;
  L1: TList<Integer>;
  L2: TList<Integer, Integer>;
  L3: TList<Integer, Integer, Integer>;      

implementation

constructor TList.Create;
begin
  L0 := TList.Create;
  L1 := TList<Integer>.Create;
  L2 := TList<Integer, Integer>.Create;
  L3 := TList<Integer, Integer, Integer>.Create;      
end;

procedure Main; 
begin


end;

end.