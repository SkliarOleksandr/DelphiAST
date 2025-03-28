unit ASTTest.Aliases.StringTypeWithCodepage1;

interface

type

  TMyStrType1 = type AnsiString(65001);
  // TMyStrType2 = type string(65001); // E2029 ';' expected but '(' found
  // TMyStrType3 = AnsiString(65001); // E2029 ';' expected but '(' found

var
  LStr: TMyStrType1;

implementation

end.