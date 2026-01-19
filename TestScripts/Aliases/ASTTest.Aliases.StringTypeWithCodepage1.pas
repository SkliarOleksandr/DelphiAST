unit ASTTest.Aliases.StringTypeWithCodepage1;

interface

type

  TMyStrType1 =  type AnsiString($ffff);

  // these two must not be compiled
  // TMyStrType2 = type string(65001); // E2029 ';' expected but '(' found
  // TMyStrType3 = AnsiString(65001); // E2029 ';' expected but '(' found

var
  _Str: AnsiString;
  _MyStr: TMyStrType1;

implementation

procedure Main;
begin
  _Str := _MyStr;
  _MyStr := _Str;
end;

end.