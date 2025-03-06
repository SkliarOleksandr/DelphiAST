unit ASTTest.Builtins.Declared1;

interface

implementation

{$IF not Declared(ReturnAddress)}
function ReturnAddress: Pointer;
begin
  Result := nil;
end; 
{$ENDIF}

end.