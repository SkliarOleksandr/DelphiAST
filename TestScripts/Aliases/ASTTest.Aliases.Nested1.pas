unit ASTTest.Aliases.Nested1;

interface

type 
  TStruct = record
  type
    TNested = record
    end;
  end;

  TNewType = type TStruct.TNested;

var
  NewVar: TNewType;

implementation

end.