unit ASTTest.Aliases.FromAnotherUnit2;

interface

uses
  ASTTest.Aliases.Decls,
  ASTTest.Aliases.Decls.Types;

type 

  TNewType = type ASTTest.Aliases.Decls.Types.TStruct.TNested;

var
  NewVar: TNewType;

implementation

end.