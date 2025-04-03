unit ASTTest.Vars.TypeInfer1;

interface

{$HINTS OFF}

type
  TMyObj = class
    function GetBool: Boolean;
  end;

implementation

procedure Main(AObj: TMyObj);
type
  TBoolFunc = function: Boolean of object;
begin
  var LVar1: TBoolFunc := AObj.GetBool;
  LVar1();

  var LVar2 := AObj.GetBool;
  LVar2 := not LVar2;
end;

{ TMyObj }

function TMyObj.GetBool: Boolean;
begin
  Result := True;
end;

end.