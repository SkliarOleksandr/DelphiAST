unit ASTTest.RTL.TDictionaty1;

interface

{$HINTS OFF}

uses
  System.Generics.Collections;

type
  TMyRecord = record
    A, B: Integer;
  end;
 
  TMyDict = TDictionary<string, TMyRecord>;
  TMyPair = TPair<string, TMyRecord>;

var
  MyDict: TMyDict;

implementation

procedure Main1;
var
  LMyPair: TMyPair;
begin 
  var LResult := 0;
  for LMyPair in MyDict do
    LResult := LResult + LMyPair.Value.A + LMyPair.Value.B;
end;

procedure Main2;
begin
  var LResult := 0;
  for var LMyPair in MyDict do
    LResult := LResult + LMyPair.Value.A + LMyPair.Value.B;
end;

end.