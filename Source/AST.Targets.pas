unit AST.Targets;

interface

uses SysUtils;

type
  TNPLAbstractTarget = class abstract
  public
    class function TargetName: string; virtual; abstract;
    class function PointerSize: Integer; virtual; abstract;
    class function NativeIntSize: Integer; virtual; abstract;
  end;

  TNPLTarget = class of TNPLAbstractTarget;

  TANY_Target = class(TNPLAbstractTarget)
  public
    class function TargetName: string; override;
    class function PointerSize: Integer; override;
    class function NativeIntSize: Integer; override;
  end;

  TWINX86_Target = class(TNPLAbstractTarget)
  public
    class function TargetName: string; override;
    class function PointerSize: Integer; override;
    class function NativeIntSize: Integer; override;
  end;

  TWINX64_Target = class(TNPLAbstractTarget)
  public
    class function TargetName: string; override;
    class function PointerSize: Integer; override;
    class function NativeIntSize: Integer; override;
  end;

  function FindTarget(const Name: string): TNPLTarget;

implementation

var RegisteredTargets: array of TNPLTarget;

function FindTarget(const Name: string): TNPLTarget;
var
  Target: TNPLTarget;
  UpperName: string;
begin
  UpperName := UpperCase(Name);
  for Target in RegisteredTargets do
    if Target.TargetName = UpperName then
      Exit(Target);
  Result := nil;
end;

procedure RegisterTarget(const TargetClass: TNPLTarget);
begin
  RegisteredTargets := RegisteredTargets + [TargetClass];
end;


{ TANY_Target }

class function TANY_Target.TargetName: string;
begin
  Result := 'ANY';
end;

class function TANY_Target.PointerSize: Integer;
begin
  Result := -1;
end;

class function TANY_Target.NativeIntSize: Integer;
begin
  Result := -1;
end;

{ TWINX86_Target }

class function TWINX86_Target.TargetName: string;
begin
  Result := 'WIN-X86';
end;

class function TWINX86_Target.PointerSize: Integer;
begin
  Result := 4;
end;

class function TWINX86_Target.NativeIntSize: Integer;
begin
  Result := 4;
end;

{ TWINX64_Target }

class function TWINX64_Target.TargetName: string;
begin
  Result := 'WIN-X64';
end;

class function TWINX64_Target.PointerSize: Integer;
begin
  Result := 8;
end;

class function TWINX64_Target.NativeIntSize: Integer;
begin
  Result := 8;
end;

initialization
  RegisterTarget(TANY_Target);
  RegisterTarget(TWINX86_Target);
  RegisterTarget(TWINX64_Target);

end.
