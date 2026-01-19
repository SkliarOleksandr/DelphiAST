unit ASTTest.Strings.ArrayOfChar1;

interface

var
  UStr: string;
  UArr: array of char;

  AStr: AnsiString;
  AArr: array of AnsiChar;

implementation

procedure Main;
begin
  // UStr := UArr; // E2010 Incompatible types: 'string' and 'array of Char'
  // AStr := AArr; // E2010 Incompatible types: 'AnsiString' and 'array of AnsiChar'
end;

end.