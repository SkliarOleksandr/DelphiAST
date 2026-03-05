unit ASTTest.Cond.UnknownVar2;

interface

//todo:
//{$DEFINE XXX}
//const YYY = 1;

{$IF DEFINED(XXX) and (YYY < 30301)}
  Must Not Compile
{$ENDIF}

implementation

end.