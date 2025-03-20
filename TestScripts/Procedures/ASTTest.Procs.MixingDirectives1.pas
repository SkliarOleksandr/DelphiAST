unit ASTTest.Procs.MixingDirectives1;

interface

function Func1: Integer; cdecl; varargs;

type
  TFunc1 = function: Integer; cdecl varargs;

implementation

function Func1: Integer; external 'LibName';

end.