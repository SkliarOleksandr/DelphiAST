unit ASTTest.Genrics.ProcType1;

interface

type
   TFunc<TResult> = reference to function: TResult;
 
implementation

procedure Main(AFunc: TFunc<Boolean>);
begin
  if AFunc() then;
end;

end.