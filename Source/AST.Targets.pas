unit AST.Targets;

interface

uses SysUtils;

type
  TASTTarget = class abstract
  public
    class function TargetName: string; virtual; abstract;
    class function PointerSize: Integer; virtual; abstract;
    class function NativeIntSize: Integer; virtual; abstract;
    class function VariantSize: Integer; virtual; abstract;
    class function MinNativeInt: Int64; virtual; abstract;
    class function MaxNativeInt: Int64; virtual; abstract;
    class function MinNativeUInt: UInt64; virtual; abstract;
    class function MaxNativeUInt: UInt64; virtual; abstract;
  end;

  TASTTargetClass = class of TASTTarget;

  TANY_Target = class(TASTTarget)
  public
    class function TargetName: string; override;
    class function PointerSize: Integer; override;
    class function NativeIntSize: Integer; override;
    class function VariantSize: Integer; override;
    class function MinNativeInt: Int64; override;
    class function MaxNativeInt: Int64; override;
    class function MinNativeUInt: UInt64; override;
    class function MaxNativeUInt: UInt64; override;
  end;

  TWINX86_Target = class(TASTTarget)
  public
    class function TargetName: string; override;
    class function PointerSize: Integer; override;
    class function NativeIntSize: Integer; override;
    class function VariantSize: Integer; override;
    class function MinNativeInt: Int64; override;
    class function MaxNativeInt: Int64; override;
    class function MinNativeUInt: UInt64; override;
    class function MaxNativeUInt: UInt64; override;
  end;

  TWINX64_Target = class(TASTTarget)
  public
    class function TargetName: string; override;
    class function PointerSize: Integer; override;
    class function NativeIntSize: Integer; override;
    class function VariantSize: Integer; override;
    class function MinNativeInt: Int64; override;
    class function MaxNativeInt: Int64; override;
    class function MinNativeUInt: UInt64; override;
    class function MaxNativeUInt: UInt64; override;
  end;

  function FindTarget(const Name: string): TASTTargetClass;

implementation

uses
  AST.Parser.Utils;

var RegisteredTargets: array of TASTTargetClass;

function FindTarget(const Name: string): TASTTargetClass;
var
  Target: TASTTargetClass;
  UpperName: string;
begin
  UpperName := UpperCase(Name);
  for Target in RegisteredTargets do
    if Target.TargetName = UpperName then
      Exit(Target);
  Result := nil;
end;

procedure RegisterTarget(const TargetClass: TASTTargetClass);
begin
  RegisteredTargets := RegisteredTargets + [TargetClass];
end;


{ TANY_Target }

class function TANY_Target.TargetName: string;
begin
  Result := 'ANY';
end;

class function TANY_Target.VariantSize: Integer;
begin
  Result := -1;
end;

class function TANY_Target.PointerSize: Integer;
begin
  Result := -1;
end;

class function TANY_Target.MaxNativeInt: Int64;
begin
  Result := 0;
end;

class function TANY_Target.MaxNativeUInt: UInt64;
begin
  Result := 0;
end;

class function TANY_Target.MinNativeInt: Int64;
begin
  Result := 0;
end;

class function TANY_Target.MinNativeUInt: UInt64;
begin
  Result := 0;
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

class function TWINX86_Target.VariantSize: Integer;
begin
  Result := 16;
end;

class function TWINX86_Target.PointerSize: Integer;
begin
  Result := 4;
end;

class function TWINX86_Target.MaxNativeInt: Int64;
begin
  Result := MaxInt32;
end;

class function TWINX86_Target.MaxNativeUInt: UInt64;
begin
  Result := MaxUInt32;
end;

class function TWINX86_Target.MinNativeInt: Int64;
begin
  Result := MinInt32;
end;

class function TWINX86_Target.MinNativeUInt: UInt64;
begin
  Result := 0;
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

class function TWINX64_Target.VariantSize: Integer;
begin
  Result := 24;
end;

class function TWINX64_Target.PointerSize: Integer;
begin
  Result := 8;
end;

class function TWINX64_Target.MaxNativeInt: Int64;
begin
  Result := MaxInt64;
end;

class function TWINX64_Target.MaxNativeUInt: UInt64;
begin
  Result := MaxUInt64
end;

class function TWINX64_Target.MinNativeInt: Int64;
begin
  Result := MinInt64;
end;

class function TWINX64_Target.MinNativeUInt: UInt64;
begin
  Result := 0;
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
