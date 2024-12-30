unit ASTTest.Variants.TypeCast1;

interface

implementation

type
  // Variant Size, 32bit = 16, 64Bit = 24
  TCustomVarData = record
    VType: Integer;
    case Integer of
      0: (VInt1, VInt2, VInt3: Integer);
      1: (VBytes: array [1..12] of Byte);        
  end;

procedure Main;
var
  V: Variant;
  VarData: TCustomVarData;
begin
  if TCustomVarData(V).VType > 0 then;

  V := Variant(VarData);
end;

end.