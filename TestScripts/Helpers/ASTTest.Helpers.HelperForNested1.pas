unit ASTTest.Helpers.HelperForNested1;

interface

type
  TMyObj = class
  type
    TNested = class
    end;
  end;

implementation

var
  N: TMyObj.TNested;

type
  THelper = class helper for TMyObj.TNested
    procedure Foo;
  end;

procedure Main;
begin
  N.Foo();
end;

{ THelper }

procedure THelper.Foo;
begin

end;

end.