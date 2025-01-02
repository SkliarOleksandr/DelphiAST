unit ASTTest.Helpers.HelperForAliases1;

interface

{$HINTS OFF}

type
  TInteger = Integer;

  TMyInteger = TInteger;

  TMyIntegerHelper = record helper for TMyInteger
    function ToString: string;
  end;

var
  Int1: Integer;
  Int2: TInteger;
  Int3: TMyInteger;

implementation

procedure Main;
begin
  var LStr1 := Int1.ToString;
  var LStr2 := Int2.ToString;
  var LStr3 := Int3.ToString;
end;

{ TMyIntegerHelper }

function TMyIntegerHelper.ToString: string;
begin
  Result := '';
end;

end.