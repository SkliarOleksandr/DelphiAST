unit AST.Parser.Options;

interface

uses SysUtils, Variants, Generics.Collections, System.Classes;

type
  TOptions = class
  private type
    TOptDataType = (odtInteger, odtBoolean, odtFloat, odtString, odtEnum);
    TOptValues = class(TList<Variant>)
    private
      OptionType: TOptDataType;
      BoosterPtr: Pointer;  // указатель на значение-кеш
    end;
    TOptionList = TDictionary<string, TOptValues>;
  private var
    FParent: TOptions;
    FOptions: TOptionList;
  protected
    procedure AddOption(const OptName: string; OptDataType: TOptDataType; BoosterPtr: Pointer; const DefaultValue: Variant);
  public
    constructor Create(Parent: TOptions); virtual;
    destructor Destroy; override;
    function Exist(const OptName: string): Boolean;
    function OptPush(const OptName: string): Boolean;
    function OptPop(const OptName: string): Boolean;
    function OptSet(const OptName: string; const Value: Variant): Boolean; virtual;
  end;

  TDefines = TStringList;

  {TDefines = class
  private
    FItems: TStrings;
    FParent: TDefines;
  public
    constructor Create(Parent: TDefines);
    destructor Destroy; override;
  end;}

  TCompilerOptions = class(TOptions)
  private
    FARC: Boolean;
    FSAFECODE: Boolean;
    FOPT_REUSE_TMP_VARS: Boolean;
    FOPT_ELEMINATE_UNUSED_LOCAL_VARS: Boolean;
    FOPT_REDUCE_TMP_VARS: Boolean;
    FOPT_REDUCE_INCREF_DECREF: Boolean;
    FOPT_SHOW_UNUSED_HINTS: Boolean;
  public
    constructor Create(Parent: TOptions); override;
    property ARC: Boolean read FARC;
    property SAFECODE: Boolean read FSAFECODE;

    // оптимизации
    property OPT_ELEMINATE_UNUSED_LOCAL_VARS: Boolean read FOPT_ELEMINATE_UNUSED_LOCAL_VARS write FOPT_ELEMINATE_UNUSED_LOCAL_VARS;
    property OPT_REUSE_TMP_VARS: Boolean read FOPT_REUSE_TMP_VARS write FOPT_REUSE_TMP_VARS;
    property OPT_REDUCE_TMP_VARS: Boolean read FOPT_REDUCE_TMP_VARS write FOPT_REDUCE_TMP_VARS;
    property OPT_REDUCE_INCREF_DECREF: Boolean read FOPT_REDUCE_INCREF_DECREF write FOPT_REDUCE_INCREF_DECREF;
    property OPT_SHOW_UNUSED_HINTS: Boolean read FOPT_SHOW_UNUSED_HINTS write FOPT_SHOW_UNUSED_HINTS;
  end;

  // глобальные опции сборки
  TPackageOptions = class(TOptions)
  private
    FVARIANT_EXPLICIT_CONVERT: Boolean;
  public
    constructor Create(Parent: TOptions); override;
    ///////////////////////////////////
    property VARIANT_EXPLICIT_CONVERT: Boolean read FVARIANT_EXPLICIT_CONVERT;
  end;


implementation

{ TCompilerOptions }

procedure TOptions.AddOption(const OptName: string; OptDataType: TOptDataType; BoosterPtr: Pointer; const DefaultValue: Variant);
var
  Values: TOptValues;
begin
  Values := TOptValues.Create;
  Values.BoosterPtr := BoosterPtr;
  Values.OptionType := OptDataType;
  Values.Add(DefaultValue);
  FOptions.Add(OptName, Values);

  if Assigned(BoosterPtr) then
  case Values.OptionType of
    odtInteger: PInt64(BoosterPtr)^ := DefaultValue;
    odtBoolean: PBoolean(BoosterPtr)^ := DefaultValue;
    odtFloat: PDouble(BoosterPtr)^ := DefaultValue;
    odtString: PString(BoosterPtr)^ := DefaultValue;
  end;
end;

constructor TOptions.Create(Parent: TOptions);
begin
  FParent := Parent;
  FOptions := TOptionList.Create(4);
end;

destructor TOptions.Destroy;
var
  V: TOptValues;
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

function TOptions.OptSet(const OptName: string; const Value: Variant): Boolean;
var
  Values: TOptValues;
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
  Values: TOptValues;
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
  Values: TOptValues;
begin
  if not FOptions.ContainsKey(OptName) then
    Exit(False);

  Values := FOptions.Items[OptName];
  if Values.Count > 0 then
    Values.Add(Values.Last);
  Result := True;
end;

{ TPascalCompilerOptions }

constructor TCompilerOptions.Create(Parent: TOptions);
begin
  inherited;
  AddOption('ARC', odtBoolean, @FARC, True);
  AddOption('SAFECODE', odtBoolean, @FSAFECODE, False);

  FOPT_REUSE_TMP_VARS := True;
  FOPT_REDUCE_TMP_VARS := True;
  FOPT_REDUCE_INCREF_DECREF := True;
  FOPT_ELEMINATE_UNUSED_LOCAL_VARS := TRUE;
end;

{ TPackageOptions }

constructor TPackageOptions.Create(Parent: TOptions);
begin
  inherited;
  AddOption('VARIANTEXPLICITCONVERT', odtBoolean, @FVARIANT_EXPLICIT_CONVERT, True);
end;

{ TDefines }

{constructor TDefines.Create(Parent: TDefines);
begin
  FItems := TStringList.Create;
  FParent := Parent;
end;

destructor TDefines.Destroy;
begin
  FItems.Free;
  inherited;
end;}

end.
