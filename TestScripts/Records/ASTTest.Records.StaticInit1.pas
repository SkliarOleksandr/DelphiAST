unit ASTTest.Records.StaticInit1;

interface

type
  TRecord = record
    Int: Integer;   
    Ptr: Pointer;
  end;
  
var
  GInt: Integer;
  GRec: TRecord = (Int: 1; Ptr: @GInt);

implementation

end.