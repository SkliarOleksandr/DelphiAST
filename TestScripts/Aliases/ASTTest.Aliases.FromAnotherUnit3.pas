unit ASTTest.Aliases.FromAnotherUnit3;

interface

type
  PInteger = type System.PInteger;
  PInt64 = type System.PInteger;

var
  Int32Ptr: PInteger;
  Int64Ptr: PInt64;

implementation

end.