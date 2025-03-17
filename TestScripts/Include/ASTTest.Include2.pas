unit ASTTest.Include2;

interface

const
  {$I decl4.inc};

//const {$I decl4.inc};

implementation

procedure Main({$I decl4.inc});
begin
//  const {$I decl4.inc};
end;

end.