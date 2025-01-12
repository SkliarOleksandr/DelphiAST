unit ASTTest.With1;

interface

{$HINST OFF}

type
  TRecord = record
    A, B: Integer;
  end;
  
  TAlias = TRecord;

implementation

procedure Main(R: TAlias);
begin
  with R do begin
    A := 1;
    B := 2;
  end;
end;

end.