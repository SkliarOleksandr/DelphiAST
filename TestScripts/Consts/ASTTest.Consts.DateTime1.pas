unit ASTTest.Consts.DateTime1;

interface

{$HINTS OFF}

type
  TMyDateTime = type Double;

const
  MSecsPerDay = 86400000;

  MinDateTime: TMyDateTime = -657434.0; { 01/01/0100 12:00:00.000 AM }
  MaxDateTime: TMyDateTime =  2958465 + (MSecsPerDay - 1) / MSecsPerDay; { 12/31/9999 11:59:59.999 PM }

implementation

end.