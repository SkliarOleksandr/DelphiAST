unit ASTTest.Cond.Declared1;

interface

{$IF not Declared(System.UnknownWar)}

type
  TMyInt = Integer; 

{$ENDIF} 


var
  MyInt: TMyInt;

implementation

end.