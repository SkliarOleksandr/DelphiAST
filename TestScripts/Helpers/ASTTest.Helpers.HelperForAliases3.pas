unit ASTTest.Helpers.HelperForAliases3;

interface

{$HINTS OFF}

type
  TMyInteger = type Integer;

  TIntegerHelper = record helper for Integer
    procedure Proc1;
  end;

  TMyIntegerHelper = record helper for TMyInteger
    procedure Proc2;
  end;

var
  Int1: Integer;
  Int2: TMyInteger;

implementation

procedure Main;
begin
  Int1.Proc1;
  // Int1.Proc2; // E2003 Undeclared identifier: 'Proc2'

  // Int2.Proc1; // E2003 Undeclared identifier: 'Proc1'
  Integer(Int2).Proc1;
  Int2.Proc2;
end;

{ TIntegerHelper }

procedure TIntegerHelper.Proc1;
begin

end;

{ TMyIntegerHelper }

procedure TMyIntegerHelper.Proc2;
begin

end;

end.