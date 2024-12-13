unit ASTTest.Procs.External;

interface

// static linking:

{$IFDEF AST_PARSING}
function s_extermal1(const pattern: string): Pointer;
  cdecl; external;

function s_extermal2(const pattern: string): Pointer;
  cdecl; external name 's_extermal2';
{$ENDIF}

// dynamic linking:

function d_extermal1(const pattern: string): Pointer;
  cdecl; external 'mylib';

function d_extermal2(const pattern: string): Pointer;
  cdecl; external 'mylib' name 'myproc';

implementation

end.