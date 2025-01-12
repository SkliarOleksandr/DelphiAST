unit ASTTest.BinAnd1;

interface

implementation

var
  B1, B2: Byte;
  W1, W2: Word;
  SInt1, SInt2: Integer;
  UInt1, UInt2: Cardinal;

procedure MainAnd;
begin
  B1 := B1 and B2;
  W1 := W1 and W2;
  SInt1 := SInt1 and SInt2;
  UInt1 := UInt1 and UInt2; 
end;

end.