unit ASTTest.Records.CaseRecord1;

interface

type
  {$HINTS OFF}
  
  TCaseRecord = record
    case Boolean of
      False: (A: array[0..7] of Byte);
      True: (B: Double);
  end;

var
  R: TCaseRecord;

implementation

procedure Main;
begin
  R.A[0] := 1;
  R.B := 1;
end;

end.