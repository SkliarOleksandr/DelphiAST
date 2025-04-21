unit ASTTest.Ops.OverloadInHelper1;

interface

type

  TOrigin = record
  end;

  THelper1 = record helper for TOrigin
    class operator Implicit(V: Integer): TOrigin;
  end;

  // it must be a new type, to make worked both helpers
  // otherwise second helper replaces the first one in case of simple alias 
  TAlias1 = type TOrigin;  

  THelper2 = record helper for TAlias1
    class operator Implicit(V: string): TAlias1;
  end;

implementation

procedure Main;
var
  LOrigin: TOrigin;
  LAlias1: TAlias1;
begin
  LOrigin := 5;
  // LOrigin := '5'; // E2010 Incompatible types: 'TOrigin' and 'Char'

  // LAlias1 := 5; // E2010 Incompatible types: 'TAlias1' and 'Integer'
  LAlias1 := '5';
end;

{ THelper1 }

class operator THelper1.Implicit(V: Integer): TOrigin;
begin

end;

{ THelper2 }

class operator THelper2.Implicit(V: string): TAlias1;
begin

end;

end.

