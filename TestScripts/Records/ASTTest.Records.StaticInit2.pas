unit ASTTest.Records.StaticInit2;

interface

type
  TInternal = record
    s1: string;   
    s2: string;
  end;

  TRecord = record
    Int: Integer;
    Ptr: Pointer;
    Rec: TInternal;
  end;
  
var
  GInt: Integer;
  GRec: TRecord = (Int: 1; Ptr: @GInt; Rec: (s1: 'str1'; s2: 'str2'));

implementation

initialization
  GInt := (1 + 2);


end.