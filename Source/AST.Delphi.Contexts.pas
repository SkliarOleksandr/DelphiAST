unit AST.Delphi.Contexts;

interface

uses AST.Delphi.Classes, AST.Parser.Contexts;

type
  TSContext = TASTSContext<TASTDelphiProc>;
  TEContext = TASTEcontext<TASTDelphiProc>;

  PSContext = ^TSContext;
  PEContext = ^TEContext;


implementation

end.
