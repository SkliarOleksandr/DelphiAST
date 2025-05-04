unit ASTTest.Helpers.HelperForAliases1;

interface

{$HINTS OFF}

type
  TInteger = Integer;

  TMyInteger = TInteger;

  // this helper actually extends the original type instead of the actual alias
  TMyIntegerHelper = record helper for TMyInteger
    function ToStr: string;
  end;

var
  Int1: Integer;
  Int2: TInteger;
  Int3: TMyInteger;

implementation

procedure Main;
begin
  var LStr1 := Int1.ToStr;
  var LStr2 := Int2.ToStr;
  var LStr3 := Int3.ToStr;
end;

{ TMyIntegerHelper }

function TMyIntegerHelper.ToStr: string;
begin
  Result := '';
end;

end.
