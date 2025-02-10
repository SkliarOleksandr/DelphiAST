unit ASTTest.Helpers.Helper2;

interface

{$HINTS OFF}

type
  TMyObj1 = class    
    procedure Proc1;
  end;

  TMyObj2 = class(TMyObj1)
  type
    THelper = class helper for TMyObj1
      procedure Proc0;
    end;
  public
    procedure Proc2;
  end;

implementation

var
  Obj: TMyObj2;
    
procedure Main;
begin
  Obj.Proc0;
  Obj.Proc1;
  Obj.Proc2;   
end;

{ TMyObj1 }

procedure TMyObj1.Proc1;
begin
  Proc0;
end;

{ TMyObj2.THelper }

procedure TMyObj2.THelper.Proc0;
begin

end;

{ TMyObj2 }

procedure TMyObj2.Proc2;
begin
  Proc0;
end;

end.