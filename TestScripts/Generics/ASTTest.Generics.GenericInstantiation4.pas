unit ASTTest.Generics.GenericInstantiation4;

interface

type
  TList<T> = class
  type
    TType1 = T;
  var
    FValue1: TList<Integer>.TType1; // instantiating an incomplete class
    property Value1: Integer read FValue1;
  type
    TType2 = T;
  var
    FValue2: TList<Integer>.TType2;  // continue instantiating an incomplete class
    property Value2: Integer read FValue2;
  type
    TType3 = T;
  end;

var
  GVar: TList<Integer>.TType3; // finish instantiating a complete class

implementation

end.