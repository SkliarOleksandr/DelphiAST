unit ASTTest.Records.CaseRecord2;

interface

type
  {$HINTS OFF}
  
  TCaseRecord = record
    case Value: Boolean of
      False: (A: array[0..7] of Byte);
      True: (B: Double);
  end;

var
  R: TCaseRecord;

implementation

procedure Main;
begin
  R.Value := True;
  R.A[0] := 1;
  R.B := 1;
end;

end.