unit AST.Parser.Options;

interface

uses SysUtils, Variants, Generics.Collections, System.Classes;

type
  TOptDataType = (odtInteger, odtBoolean, odtFloat, odtString, odtEnum, optSpecial);

  // base class for option
  TOption = class(TList<Variant>)
  private
    OptionType: TOptDataType;
    BoosterPtr: Pointer;  // указатель на значение-кеш
  public
    class function ArgsCount: Integer; virtual;
  end;
  TOptionClass = class of TOption;

  TValueOption = class(TOption)
  public
    class function ArgsCount: Integer; override;
    procedure SetValue(const Value: string; out Error: string); virtual;
  end;

  TStrOption = class(TValueOption)
  private
    fValue: string;
  public
    procedure SetValue(const Value: string; out Error: string); override;
    property Value: string read fValue;
  end;

//  TIntOption = class(TValueOption)
//  private
//    fValue: Integer;
//  end;

  TBoolOption = class(TValueOption)
  private
    fValue: Boolean;
  public
    procedure SetValue(const Value: string; out Error: string); override;
    property Value: Boolean read fValue;
  end;

  TOptions = class
  private type
    TOptionList = TDictionary<string, TOption>;
  private var
    FParent: TOptions;
    FOptions: TOptionList;
  public
    constructor Create(Parent: TOptions); virtual;
    destructor Destroy; override;

    function AddBoolOption(const OptName: string): TBoolOption; overload;
    function AddOption(const OptShortName, OptName: string; OptionClass: TOptionClass): TOption; overload;
    function FindOption(const Name: string): TOption;

    function Exist(const OptName: string): Boolean;
    function OptPush(const OptName: string): Boolean;
    function OptPop(const OptName: string): Boolean;
    function OptSet(const OptName: string; const Value: Variant): Boolean; virtual;
  end;

  TDefines = TStringList;

  // глобальные опции сборки
  TPackageOptions = class(TOptions)
  public
    constructor Create(Parent: TOptions); override;
  end;


implementation

type
  TOptionAlias = class(TOption)
  private
    fOriginalOption: TOption;
  end;


{ TOptions }

function TOptions.AddBoolOption(const OptName: string): TBoolOption;
begin
  Result := TBoolOption.Create();
  FOptions.Add(OptName, Result);
end;

function TOptions.AddOption(const OptShortName, OptName: string; OptionClass: TOptionClass): TOption;
begin
  Result := nil;
end;

constructor TOptions.Create(Parent: TOptions);
begin
  FParent := Parent;
  FOptions := TOptionList.Create(4);
end;

destructor TOptions.Destroy;
var
  V: TOption;
begin
  for V in FOptions.Values do
    V.Free;
  FOptions.Free;
  inherited;
end;

function TOptions.Exist(const OptName: string): Boolean;
begin
  Result := FOptions.ContainsKey(UpperCase(OptName));
  if not Result and Assigned(FParent) then
    Result := FParent.Exist(OptName);
end;

function TOptions.FindOption(const Name: string): TOption;
begin
  if not FOptions.TryGetValue(UpperCase(Name), Result) and Assigned(FParent) then
    Result := FParent.FindOption(Name);
end;

function TOptions.OptSet(const OptName: string; const Value: Variant): Boolean;
var
  Values: TOption;
  OptNameUC: string;
begin
  OptNameUC := UpperCase(OptName);
  if not FOptions.ContainsKey(OptNameUC) then
  begin
    if Assigned(FParent) then
      Exit(FParent.OptSet(OptNameUC, Value))
    else
      Exit(False);
  end;

  Values := FOptions.Items[OptNameUC];
  Values.Items[Values.Count - 1] := Value;

  if Assigned(Values.BoosterPtr) then
  case Values.OptionType of
    odtInteger: PInt64(Values.BoosterPtr)^ := Value;
    odtBoolean: PBoolean(Values.BoosterPtr)^ := Value;
    odtFloat: PDouble(Values.BoosterPtr)^ := Value;
    odtString: PString(Values.BoosterPtr)^ := Value;
  end;

  Result := True;
end;

function TOptions.OptPop(const OptName: string): Boolean;
var
  Values: TOption;
begin
  if not FOptions.ContainsKey(OptName) then
    Exit(False);

  Values := FOptions.Items[OptName];
  if Values.Count > 1 then
    Values.Delete(Values.Count - 1);
  Result := True;
end;

function TOptions.OptPush(const OptName: string): Boolean;
var
  Values: TOption;
begin
  if not FOptions.ContainsKey(OptName) then
    Exit(False);

  Values := FOptions.Items[OptName];
  if Values.Count > 0 then
    Values.Add(Values.Last);
  Result := True;
end;

{ TPackageOptions }

constructor TPackageOptions.Create(Parent: TOptions);
begin
  inherited;
end;

{ TValueOption }

class function TValueOption.ArgsCount: Integer;
begin
  Result := 1;
end;

procedure TValueOption.SetValue(const Value: string; out Error: string);
begin

end;

{ TOption }

class function TOption.ArgsCount: Integer;
begin
  Result := 0;
end;

{ TStrOption }

procedure TStrOption.SetValue(const Value: string; out Error: string);
begin
  fValue := Value;
end;

{ TBoolOption }

procedure TBoolOption.SetValue(const Value: string; out Error: string);
begin
  var UCVal := UpperCase(Value);
  if (UCVal <> '+') and
     (UCVal <> '-') and
     (UCVal <> 'ON') and
     (UCVal <> 'OFF') then
  begin
    Error := 'The switch value can be only +, -, ON, OFF';
    Exit;
  end;
  fValue := (UCVal = '+') or (UCVal = 'ON');
end;

end.
