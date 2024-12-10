unit AST.Parser.Options;

interface

uses SysUtils, Variants, Generics.Collections, System.Classes;

type
  TOptDataType = (odtInteger, odtBoolean, odtFloat, odtString, odtEnum, optSpecial);

  // base class for option
  TOption = class(TList<Variant>)
  private
    FName: string;
    OptionType: TOptDataType;
    BoosterPtr: Pointer;  // указатель на значение-кеш
  public
    class function ArgsCount: Integer; virtual;
    procedure SetValueFromStr(const AValue: string; out AError: string); virtual; abstract;
    property Name: string read FName;
  end;
  TOptionClass = class of TOption;

  TValueOption<T> = class(TOption)
  protected
    FValue: T;
  public
    constructor Create(const AName: string; const ADefaultValue: T);
    class function ArgsCount: Integer; override;
    property Value: T read FValue;
  end;

  TStrOption = class(TValueOption<string>)
  public
    procedure SetValueFromStr(const AValue: string; out AError: string); override;
  end;

  TSwitchOption = class(TValueOption<Boolean>)
  private
    FShortName: string;
  public
    constructor Create(const AName, AShortName: string; const ADefaultValue: Boolean); overload;
    procedure SetValueFromStr(const AValue: string; out AError: string); override;
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

    function AddSwitchOption(const AName: string; const AShortName: string = ''; ADefault: Boolean = False): TSwitchOption;
    function AddStrOption(const AName: string): TStrOption;

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

function TOptions.AddStrOption(const AName: string): TStrOption;
begin
  Result := TStrOption.Create(AName, {ADefaultValue:} '');
  FOptions.Add(AName, Result);
end;

function TOptions.AddSwitchOption(const AName, AShortName: string; ADefault: Boolean): TSwitchOption;
begin
  Result := TSwitchOption.Create(AName, AShortName, ADefault);
  FOptions.Add(AName, Result);
  if AShortName <> '' then
    FOptions.Add(AShortName, Result);
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

class function TValueOption<T>.ArgsCount: Integer;
begin
  Result := 1;
end;

{ TOption }

class function TOption.ArgsCount: Integer;
begin
  Result := 0;
end;

{ TSwitchOption }

constructor TSwitchOption.Create(const AName, AShortName: string; const ADefaultValue: Boolean);
begin
  inherited Create(AName, ADefaultValue);
  FShortName := AShortName;
end;

procedure TSwitchOption.SetValueFromStr(const AValue: string; out AError: string);
begin
  var LValue := UpperCase(AValue);

  if LValue = 'ON' then
    FValue := True
  else
  if LValue = 'OFF' then
    FValue := False
  else
  if FShortName <> ''  then
  begin
    if LValue = '+' then
      FValue := True
    else
    if LValue = '-' then
      FValue := False
    else
      AError := 'The switch value can be only +, -, ON, OFF';
  end else
    AError := 'The switch value can be only +, -, ON, OFF';
end;

constructor TValueOption<T>.Create(const AName: string; const ADefaultValue: T);
begin
  FName := AName;
  FValue := ADefaultValue;
end;

{ TStrOption }

procedure TStrOption.SetValueFromStr(const AValue: string; out AError: string);
begin
  FValue := AValue;
end;

end.
