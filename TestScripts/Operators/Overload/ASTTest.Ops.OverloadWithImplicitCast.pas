unit ASTTest.Ops.OverloadWithImplicitCast;

interface

type
  TRecord = record
    class operator Multiply(const ALeft: TRecord; const ARight: Single): TRecord;
    class operator Divide(const ALeft: TRecord; const ARight: Single): TRecord;
  end;

implementation

class operator TRecord.Multiply(const ALeft: TRecord; const ARight: Single): TRecord;
begin 
  Result := Default(TRecord);
end;  

class operator TRecord.Divide(const ALeft: TRecord; const ARight: Single): TRecord;
begin
  // there is no overloading for Double datatype, there is Single only,
  // but Delhi finds a way to use implecit casting
  Result := ALeft * (1 / ARight);
end;

end.