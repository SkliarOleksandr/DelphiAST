unit ASTTest.Classes.Construct2;

interface

{$HINTS OFF}

type
  TBaseClass = class
  public 
    constructor Create(A: Integer); overload;
  end;
  
  TMyClass = class(TBaseClass)
  public   
    constructor Create(A, B: Integer); overload;
  end;

implementation

constructor TBaseClass.Create(A: Integer);
begin
end;

constructor TMyClass.Create(A, B: Integer);
begin
  Create(A);
end;

end.