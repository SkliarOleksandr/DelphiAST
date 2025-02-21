unit ASTTest.Operators.Neg1;

interface

{$HINTS OFF}

implementation

type
  TRange = -1..1;

procedure Main(AInt: Integer;
               ARange: TRange;
               AFloat: Double);

begin
  AInt := - AInt;
  ARange := - ARange;
  AFloat := - AFloat;
end;

end.