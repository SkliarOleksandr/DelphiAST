unit ASTTest.Other.PlatformAndDeprecatedMix platform deprecated;

interface

{$HINTS OFF}

const
  CPlatform1 = 1 deprecated platform;
  CPlatform2 = 2 platform deprecated;

type
  TPlatformRecord = record
    F1: Integer platform deprecated;
    F2: string deprecated platform;
    class var F3: Integer platform deprecated;
    function GetMethod: Integer; deprecated; platform;
    class operator Implicit(AValue: Integer): TPlatformRecord; platform; deprecated;
  end platform deprecated;

var
  GPlatform1, GPlatform2: Integer platform deprecated;
  GPlatform3, GPlatform4: Integer deprecated platform;
  GPlatform5: Boolean platform = False deprecated;

procedure PlatformProc1; platform deprecated
procedure PlatformProc2; deprecated platform

implementation

procedure PlatformProc1;
begin
end;

procedure PlatformProc2;
begin
end;

{ TPlatformRecord }

function TPlatformRecord.GetMethod: Integer; platform;

  procedure Nested; platform deprecated
  begin
  end;

var
  Local1: Integer platform deprecated;
  Local2: Integer deprecated platform;
begin
  Result := 0;
end;

class operator TPlatformRecord.Implicit(AValue: Integer): TPlatformRecord; platform;
begin
end;

end.