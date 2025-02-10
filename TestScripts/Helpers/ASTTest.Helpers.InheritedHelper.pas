unit ASTTest.Helpers.InheritedHelper;

interface

{$HINTS OFF}

type
  TMyObj1 = class    
  end;

  THelper1 = class helper for TMyObj1
    procedure Proc1;
  end;

  THelper2 = class helper(THelper1) for TMyObj1
    procedure Proc2;
  end;

implementation

var
  Obj: TMyObj1;

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