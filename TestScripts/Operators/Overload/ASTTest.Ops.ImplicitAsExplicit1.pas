unit ASTTest.Ops.ImplicitAsExplicit1;

interface

type
  TRecord = record 
    class operator Implicit(Value: string): TRecord;
  end;

implementation

procedure Main(ARec: TRecord; AStr: string);
begin
  ARec := AStr;           // use implict as expected
  ARec := TRecord(AStr);  // use implict when there is no explict
end;

{ TRecord }

class operator TRecord.Implicit(Value: string): TRecord;
begin

end;

end.