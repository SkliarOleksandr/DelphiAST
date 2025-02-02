unit ASTTest.Classes.ConstructUsingClassOf1;

interface

type
  TCls = class of TObj;
  
  TObj = class
  public
    constructor Create(AInt: Integer);
  end;

implementation

function GetClass: TCls;
begin
  Result := TObj; 
end;

procedure Main;
begin
  GetClass.Create(1);  
end;

end.