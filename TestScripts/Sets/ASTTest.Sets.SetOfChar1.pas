unit ASTTest.Sets.SetOfChar1;

interface

{$HINTS OFF}

type
  TCharSet = set of AnsiChar;

var
  CharSet: TCharSet;

implementation

const
  CCharA = 'A';
  CCharB = 'B';

procedure DoSmth(ASet: TCharSet);
begin
  CharSet := ASet;
end;

procedure Main;
begin
  DoSmth([CCharA, CCharB]);
  CharSet := ['A', CCharA, CCharB];
end;

end.