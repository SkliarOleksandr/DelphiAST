unit ASTTest.Cond.UnknownVar1;

interface

{$IF not Declared(System.UnknownVar)}

type
  TMyInt = Integer; 

{$ENDIF} 


var
  MyInt: TMyInt;

implementation

end.