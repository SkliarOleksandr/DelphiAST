unit ASTTest.Ops.OverloadBinOps;

interface

type
  TInt128 = record
    class operator Add(ALeft: TInt128; ARight: Byte): TInt128;
    class operator Add(ALeft: Byte; ARight: TInt128): TInt128;    
  end;

implementation

var
  I32: Integer; 
  I128: TInt128;

procedure Main;
begin
  I128 := I128 + I32;
  I128 := I32 + I128;
end;

{ TInt128 }

class operator TInt128.Add(ALeft: TInt128; ARight: Byte): TInt128;
begin

end;

class operator TInt128.Add(ALeft: Byte; ARight: TInt128): TInt128;
begin

end;

end.