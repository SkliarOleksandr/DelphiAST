unit ASTTest.Cases.Chars;

interface

implementation

function IsNonDelimiter(C: Char): Boolean;
begin
  case C of
      Char($00)..Char($08), Char($0A)..Char($1F), '0'..'9', ':', 
      'A'..'Z', 'a'..'z', Char($7F)..Char($FF): Result := True;
  else
    Result := False;
  end;
end;

end.