unit ASTTest.Helpers.ClassVar;

interface

type
  TMyObj = class
  
  end;
  
  TMyObjHelper = class helper for TMyObj
  public
    class var IntVar: Integer;
    class threadvar ThrVar: Integer;
  end;

implementation

procedure Main;
begin
  TMyObj.IntVar := 1;
  TMyObj.ThrVar := 1;
end;

end.