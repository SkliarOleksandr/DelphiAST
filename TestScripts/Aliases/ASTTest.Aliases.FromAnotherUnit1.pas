unit ASTTest.Aliases.FromAnotherUnit1;

interface

uses
  ASTTest.Aliases.Decls;

type 

  TNewType = type ASTTest.Aliases.Decls.TStruct.TNested;

var
  NewVar: TNewType;
  
implementation

end.