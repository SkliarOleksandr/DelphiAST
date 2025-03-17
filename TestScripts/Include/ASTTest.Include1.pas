unit ASTTest.Include1;

interface

const
{$I decl1.inc}

var
{$I decl2.inc}

procedure Test({$I decl3.inc});

implementation

procedure Test({$I decl3.inc});
begin
end;

end.