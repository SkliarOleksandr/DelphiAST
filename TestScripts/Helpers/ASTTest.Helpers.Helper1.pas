unit ASTTest.Helpers.Helper1;

interface

{$HINTS OFF}

type
  TMyObj1 = class
  end;
  
  TMyObj2 = class(TMyObj1)
  end;

  THelper1 = class helper for TMyObj1
    procedure Proc1;
  end;
  
  THelper2 = class helper for TMyObj2
    procedure Proc2;
  end;  

implementation

var
  Obj: TMyObj2;
    
procedure Main;
begin
  Obj.Proc1;
  Obj.Proc2;   
end;

{ THelper1 }

procedure THelper1.Proc1;
begin

end;

{ THelper2 }

procedure THelper2.Proc2;
begin

end;

end.