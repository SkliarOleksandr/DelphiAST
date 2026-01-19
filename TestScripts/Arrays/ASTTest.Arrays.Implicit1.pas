unit ASTTest.Arrays.Implicit1;

interface

type
  TIntDynArray1 = array of Integer;
  TIntDynArray2 = array of Integer;

  TIntSArray1 = array [0..15] of Integer;
  TIntSArray2 = array [0..15] of Integer;

  TCharDynArray1 = array of Char;
  TCharDynArray2 = array of Char;

var
  SIntArray1: TIntSArray1;
  SIntArray2: TIntSArray2;

  DIntArray1: TIntDynArray1;
  DIntArray2: TIntDynArray2;

  DChrArray1: TCharDynArray1;
  DChrArray2: TCharDynArray2;

  Str: string;

implementation

procedure Main;
begin
  // implicit cast is not supported, Expected: E2010 Incompatible types

  //  SIntArray1 := SIntArray2;
  // DIntArray1 := DIntArray2;
  // DChrArray1 := DChrArray2;
  // Str := DChrArray1;
  // DChrArray1 := Str;
end;

end.