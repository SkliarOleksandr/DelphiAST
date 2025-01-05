unit ASTTest.Classes.Construct1;

interface

{$HINTS OFF}

type
  TBaseClass = class
  public 
    constructor CreateA; overload;
  end;
  
  TMyClass = class(TBaseClass)
  public   
    constructor CreateB; overload;
  end;

implementation

constructor TBaseClass.CreateA;
begin
end;

constructor TMyClass.CreateB;
begin
  CreateA();
end;

end.