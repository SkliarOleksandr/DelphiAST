unit ASTTest.Ordian.Enums.Init;

interface

type
  TEnumA = (itA0, itA1, itA2);

  TEnumB = (
    itB0 = 1,
    itB1 = - itB0,
    itB2 = itB0 + 1,
    itB3 = itB0 - 1,
    itB4 = itB0 * 2,
    itB5 = itB0 div 2,
    itB6 = itB0 mod 2,
    itB7 = itB0 shl 2,
    itB8 = itB0 shr 2,
    itB9 = not itB0,
    itB10 = itB0 and 1,
    itB11 = itB0 or 1,
    itB12 = itB0 xor 1,
    //itB17 = itA0, // E2010 Incompatible types: 'Integer' and 'TEnumA'
    itB100
  );

//const
//  CConts1 = itB0 + 1; // E2008 Incompatible types

implementation

procedure Main;
begin
// if eItem1 = (itB0 + 1) then; // E2008 Incompatible types
end;

end.