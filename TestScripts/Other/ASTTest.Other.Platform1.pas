unit ASTTest.Other.Platform1 platform;

interface

{$HINTS OFF}

const
  CPlatform1 = 1 platform;

type
  TPlatformRecord = record
    F1: Integer platform;
    F2: string platform;
    class var F3: Integer platform;
    function GetMethod: Integer; platform;
    class operator Implicit(AValue: Integer): TPlatformRecord; platform;
  end platform;

var
  GPlatform1, GPlatform2: Integer platform;
  GPlatform3: Boolean platform = False platform deprecated;

procedure PlatformProc; platform

implementation

procedure PlatformProc; platform
begin

end;

{ TPlatformRecord }

function TPlatformRecord.GetMethod: Integer; platform;

  procedure Nested; platform
  begin
  end;

var
  Local1: Integer platform;
begin
  Result := 0;
end;

class operator TPlatformRecord.Implicit(AValue: Integer): TPlatformRecord; platform;
begin
  inherited;

end;

end.