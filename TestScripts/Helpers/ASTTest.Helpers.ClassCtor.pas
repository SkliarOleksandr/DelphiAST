unit ASTTest.Helpers.ClassCtor;

interface

type
  TMyObj = class
  
  end;
  
  TMyObjHelper = class helper for TMyObj
  private
    class var FData: Integer;
  public
    class constructor ClassCreate;
    class destructor ClassDestroy;
  end;

implementation

{ TMyObjHelper }

class constructor TMyObjHelper.ClassCreate;
begin
  FData := 1;
end;

class destructor TMyObjHelper.ClassDestroy;
begin
  FData := 0;
end;

end.