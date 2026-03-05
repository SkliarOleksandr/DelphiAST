unit ASTTest.Consts.FlexConst1;

interface

implementation

procedure Main(AChar: AnsiChar);
begin
  // problem: Delphi compiler treats a char/str literal as WideChar/UnicodeString
  // so, to avoid any implicit casting we need a way to "ajust" 
  // the constant type to waht we need at the moment 
  
  // in these cases, we need to treat ' ' literal as AnsiChar, not a WideChar
  if AChar <> ' ' then;
  
  if ' ' <> AChar then; 
end;

end.