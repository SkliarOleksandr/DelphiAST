unit ASTTest.Add.Pointers;

interface

{$HINTS OFF}

implementation

procedure AddPChars;
var
  LPAChar: PAnsiChar;
  LPWChar: PWideChar;
  
  LUInt32: Cardinal;
begin
  LPAChar := nil;
  LPWChar := nil;
  LUInt32 := 1;
    
  LPAChar := LPAChar + 1;
  LPWChar := LPWChar + 1;

  LPAChar := LPAChar - 1;
  LPWChar := LPWChar - 1;

  // LPAChar := 1 - LPAChar; // E2015 Operator not applicable to this operand type
  // LPWChar := 1 - LPWChar; // E2015 Operator not applicable to this operand type

  LPAChar := LPAChar + LUInt32;
  LPWChar := LPWChar + LUInt32;

  LPAChar := LPAChar - LUInt32;
  LPWChar := LPWChar - LUInt32;

  // LPAChar := LUInt32 - LPAChar; // E2015 Operator not applicable to this operand type
  // LPWChar := LUInt32 - LPWChar; // E2015 Operator not applicable to this operand type
end;

end.