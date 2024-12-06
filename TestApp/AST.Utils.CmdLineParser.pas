unit AST.Utils.CmdLineParser;

interface

uses
  System.SysUtils, System.Types, System.Generics.Collections, System.Generics.Defaults;

type
  ICMDLineParam = interface
    function GetName: string;
    function GetValue: string;
    function ToString: string;
    property Name: string read GetName;
    property Value: string read GetValue;
  end;

  TCMDLineParam = class(TInterfacedObject, ICMDLineParam)
  private
    FName: string;
    FValue: string;
  protected
    constructor Create(const AName: string);
    function TryParse(const ACMDLine: string; var APos: Integer): Boolean; virtual;
    function GetName: string;
    function GetValue: string;
    function ReadOptValue(const ACMDLine: string; var APos: Integer): string;
  public
    function ToString: string; override;
  end;

  TCMDLineParamClass = class of TCMDLineParam;


  ICMDLineOptions = interface
    ['{B7104FFA-A614-430A-9DC6-8FCFDE00C0DA}']
    function GetOption(const AName: string): ICMDLineParam;
    property Options[const AName: string]: ICMDLineParam read GetOption; default;
  end;

  TCMDLineParams = class(TInterfacedObject, ICMDLineOptions)
  private
    FParams: TList<ICMDLineParam>;
    function GetOption(const AName: string): ICMDLineParam;
  public
    constructor Create;
    destructor Destroy; override;
    function TryGetParam(const AName: string; out AParam: ICMDLineParam): Boolean;
    function ToString: string; override;
    property Options[const AName: string]: ICMDLineParam read GetOption;
  end;

  TCMDLineParser = class {static}
  type
    TOptData = record
      Name: string;
      OptClass: TCMDLineParamClass;
    end;
    POptData = ^TOptData;
  private
    class var FOptions: TArray<TOptData>;
    class var FOptionsPrepared: Boolean;
  protected
    class function ReadOption(const ACMDLine: string; var APos: Integer): ICMDLineParam;
    class procedure PrepareOptions;
  public
    class constructor ClassCreate;
    class destructor ClassDestroy;
    class procedure AddOpt(const AOptName: string; AOptClass: TCMDLineParamClass = nil);
    class function Parse(const ACMDLine: string): ICMDLineOptions;
  end;

  TDcc32CMDLineParser = class(TCMDLineParser)
  public
    class constructor ClassCreate;
  end;

implementation

const
  S_CMD_SEPARATOR = ' '; // space

type
  TDcc32AliasParam = class(TCMDLineParam)
  public
    function TryParse(const ACMDLine: string; var APos: Integer): Boolean; override;
  end;

  TDcc32CompDirOpt = class(TCMDLineParam)
  private
    FDirective: string;
    FEnabled: Boolean;
  public
    function TryParse(const ACMDLine: string; var APos: Integer): Boolean; override;
  end;

  TDcc32SimpleOpt = class(TCMDLineParam)
  public
    function TryParse(const ACMDLine: string; var APos: Integer): Boolean; override;
  end;

  TDcc32StrOpt = class(TCMDLineParam)
  end;

  TInlineOptState = (optOn, optOff, optAuto);

  TDcc32InlineOpt = class(TCMDLineParam)
  private
    FState: TInlineOptState;
  public
    function TryParse(const ACMDLine: string; var APos: Integer): Boolean; override;
  end;

{ TCMDLineParser }

class constructor TCMDLineParser.ClassCreate;
begin

end;

class destructor TCMDLineParser.ClassDestroy;
begin

end;

class procedure TCMDLineParser.PrepareOptions;
begin
  // sort by length(opt) descending
  TArray.Sort<TOptData>({var} FOptions,
    TComparer<TOptData>.Construct(
      function(const ALeft, ARight: TOptData): Integer
      begin
        Result := Length(ARight.Name) - Length(ALeft.Name);
      end)
  );
  FOptionsPrepared := True;
end;

class function TCMDLineParser.Parse(const ACMDLine: string): ICMDLineOptions;
begin
  if not FOptionsPrepared then
    PrepareOptions;

  var LPos := 1;
  var LCMDLen := Length(ACMDLine);
  Var LParams := TCmdLineParams.Create;
  while LPos < LCMDLen do
  begin
    var LOpt := ReadOption(ACMDLine, {var} LPos);
    if Assigned(LOpt) then
    begin
      LParams.FParams.Add(LOpt);
      // skip option separator
      while (ACMDLine[LPos] = S_CMD_SEPARATOR) and (LPos < LCMDLen) do
        Inc(LPos);
    end else
      raise Exception.CreateFmt('Unknown command-line option (%d): %s',
        [LPos, Copy(ACMDLine, LPos, LCMDLen)]);
  end;
  Result := LParams;
end;

class function TCMDLineParser.ReadOption(const ACMDLine: string; var APos: Integer): ICMDLineParam;
begin
  for var LIndex := 0 to Length(FOptions) - 1 do
  begin
    var LOptData := POptData(@FOptions[LIndex]);
    // parse only first option
    if Pos(LOptData.Name, ACMDLine, APos) = APos then
    begin
      Inc(APos, Length(LOptData.Name));

      var LOption := LOptData.OptClass.Create(LOptData.Name);

      if LOption.TryParse(ACMDLine, APos) then
        Exit(LOption);
    end;
  end;
  Result := nil;
end;

class procedure TCMDLineParser.AddOpt(const AOptName: string; AOptClass: TCMDLineParamClass);
begin
  for var LOptData in FOptions do
    if LOptData.Name = AOptName then
      raise Exception.CreateFmt('Command line option already defined: %s', [AOptName]);

  var LNewOptData: TOptData;
  LNewOptData.Name := AOptName;
  LNewOptData.OptClass := AOptClass;
  FOptions := FOptions + [LNewOptData];
end;

{ TDcc32CMDLineParser }

class constructor TDcc32CMDLineParser.ClassCreate;
begin
  inherited;
  AddOpt('-A', TDcc32StrOpt);     //<unit>=<alias> = Set unit alias
  AddOpt('-B', TDcc32SimpleOpt);     // = Build all units
  AddOpt('-CC', TDcc32SimpleOpt);    // = Console target
  AddOpt('-CG', TDcc32SimpleOpt);    // = GUI target
  AddOpt('-D', TDcc32StrOpt);     //<syms> = Define conditionals
  AddOpt('-E', TDcc32StrOpt);     //<path> = EXE/DLL output directory
  AddOpt('-F', TDcc32StrOpt);     //<offset> = Find error
  AddOpt('-GD', TDcc32SimpleOpt);    // = Detailed map file
  AddOpt('-GP', TDcc32SimpleOpt);    // = Map file with publics
  AddOpt('-GS', TDcc32SimpleOpt);    // = Map file with segments
  AddOpt('-H', TDcc32SimpleOpt);     // = Output hint messages
  AddOpt('-I', TDcc32StrOpt);     //<paths> = Include directories
  AddOpt('-J', TDcc32SimpleOpt);     // = Generate .obj file
  AddOpt('-JPHNE', TDcc32SimpleOpt); // = Generate C++ .obj file, .hpp file, in namespace, export all
  AddOpt('-JL', TDcc32SimpleOpt);    // = Generate package .lib, .bpi, and all .hpp files for C++
  AddOpt('-K', TDcc32StrOpt);     //<addr> = Set image base addr
  AddOpt('-LE', TDcc32StrOpt);    //<path> = package .bpl output directory
  AddOpt('-LN', TDcc32StrOpt);    //<path> = package .dcp output directory
  AddOpt('-LU', TDcc32StrOpt);    //<package> = Use package
  AddOpt('-M', TDcc32SimpleOpt);     // = Make modified units
  AddOpt('-NU', TDcc32StrOpt);    //<path> = unit .dcu output directory
  AddOpt('-NH', TDcc32StrOpt);    //<path> = unit .hpp output directory
  AddOpt('-NO', TDcc32StrOpt);    //<path> = unit .obj output directory
  AddOpt('-NB', TDcc32StrOpt);    //<path> = unit .bpi output directory
  AddOpt('-NX', TDcc32StrOpt);    //<path> = unit .xml output directory
  AddOpt('-NS', TDcc32StrOpt);    //<namespaces> = Namespace search path
  AddOpt('-O', TDcc32StrOpt);     //<paths> = Object directories
  AddOpt('-P', TDcc32SimpleOpt);     // = look for 8.3 file names also
  AddOpt('-Q', TDcc32SimpleOpt);     // = Quiet compile
  AddOpt('-R', TDcc32StrOpt);     //<paths> = Resource directories
  AddOpt('-TX', TDcc32StrOpt);    //<ext> = Output name extension
  AddOpt('-U', TDcc32StrOpt);     //<paths> = Unit directories
  AddOpt('-V', TDcc32SimpleOpt);     // = Debug information in EXE
  AddOpt('-VR', TDcc32SimpleOpt);    // = Generate remote debug (RSM)
  AddOpt('-VT', TDcc32SimpleOpt);    // = Debug information in TDS
  AddOpt('-VN', TDcc32SimpleOpt);    // = TDS symbols in namespace
  AddOpt('-W', TDcc32StrOpt);     // [+|-|^][warn_id]'); // = Output warning messages
  AddOpt('-Z', TDcc32SimpleOpt);     // = Output 'never build' DCPs
  AddOpt('-$', TDcc32CompDirOpt);     //<dir> = Compiler directive
  AddOpt('--help', TDcc32SimpleOpt);               // = Show this help screen
  AddOpt('--version', TDcc32SimpleOpt);            // = Show name and version
  AddOpt('--codepage:', TDcc32StrOpt);          //<cp> = specify source file encoding
  AddOpt('--default-namespace:', TDcc32StrOpt); //<namespace> = set namespace
  AddOpt('--depends', TDcc32SimpleOpt);            // = output unit dependency information
  AddOpt('--doc', TDcc32SimpleOpt);                // = output XML documentation
  AddOpt('--drc', TDcc32SimpleOpt);                // = output resource string .drc file
  AddOpt('--no-config', TDcc32SimpleOpt);          // = do not load default dcc32.cfg file
  AddOpt('--description:', TDcc32StrOpt);       //<string> = set executable description
  AddOpt('--inline:', TDcc32InlineOpt);           //{on|off|auto} = function inlining control
  AddOpt('--legacy-ifend', TDcc32SimpleOpt);       // = allow legacy $IFEND directive
  AddOpt('--zero-based-strings', TDcc32StrOpt); //[+|-] = strings are indexed starting at 0
  AddOpt('--peflags:', TDcc32StrOpt);           //<flags> = set extra PE Header flags field
  AddOpt('--peoptflags:', TDcc32StrOpt);        //<flags> = set extra PE Header optional flags field
  AddOpt('--peosversion:', TDcc32StrOpt);       //<major>.<minor> = set OS Version fields in PE Header (default: 5.0)
  AddOpt('--pesubsysversion:', TDcc32StrOpt);   //<major>.<minor> = set Subsystem Version fields in PE Header (default: 5.0)
  AddOpt('--peuserversion:', TDcc32StrOpt);     //<major>.<minor> = set User Version fields in PE Header (default: 0.0)
  AddOpt('--lib-version:', TDcc32StrOpt);       //<version> = Output package name version
  AddOpt('--lib-suffix:', TDcc32StrOpt);        //<suffix> = Output package name suffix
  AddOpt('--large-address-aware', TDcc32SimpleOpt); // = large address aware
end;

{ TCmdLineParams }

constructor TCmdLineParams.Create;
begin
  inherited;
  FParams := TList<ICMDLineParam>.Create;
end;

destructor TCmdLineParams.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TCMDLineParams.GetOption(const AName: string): ICMDLineParam;
begin
  if not TryGetParam(AName, {out} Result) then
    raise Exception.CreateFmt('Unknown cmd-line option: %s', [AName]);
end;

function TCMDLineParams.ToString: string;
begin
  Result := '';
  for var LOpt in FParams do
    Result := Result.Join(' ', [Result, LOpt.ToString]);

  Result := Trim(Result);
end;

function TCmdLineParams.TryGetParam(const AName: string; out AParam: ICMDLineParam): Boolean;
begin
  for var LIndex := 0 to FParams.Count - 1 do
    if SameText(FParams[LIndex].Name, AName) then
    begin
      AParam := FParams[LIndex];
      Exit(True);
    end;
  Result := False;
  AParam := nil;
end;

{ TCMDLineParam }

constructor TCMDLineParam.Create(const AName: string);
begin
  FName := AName;
end;

function TCMDLineParam.GetName: string;
begin
  Result := FName;
end;

function TCMDLineParam.GetValue: string;
begin
  Result := FValue;
end;

function TCMDLineParam.ReadOptValue(const ACMDLine: string; var APos: Integer): string;
begin
  var LPos := APos;
  var LQuouteCnt := 0;
  var LStrLen := Length(ACMDLine);
  while LPos < LStrLen do
  begin
    var LChar := ACMDLine[LPos];
    // read whole string until separator, with quotes handling
    if (LChar = S_CMD_SEPARATOR) and (LQuouteCnt mod 2 = 0) then
    begin
      Result := Copy(ACMDLine, APos, LPos - APos);
      APos := LPos;
      Exit;
    end else
    if (LChar = '"') then
      Inc(LQuouteCnt);

    Inc(LPos);
  end;
  Result := Copy(ACMDLine, APos, LStrLen);
  APos := LStrLen;
end;

function TCMDLineParam.ToString: string;
begin
  Result := FName + FValue;
end;

function TCMDLineParam.TryParse(const ACMDLine: string; var APos: Integer): Boolean;
begin
  FValue := ReadOptValue(ACMDLine, APos);
  Result := True;
end;

{ TDcc32AliasParam }

function TDcc32AliasParam.TryParse(const ACMDLine: string; var APos: Integer): Boolean;
begin
  Result := False;
end;

{ TDcc32CompDirOpt }

function TDcc32CompDirOpt.TryParse(const ACMDLine: string; var APos: Integer): Boolean;
begin
  inherited;
  var LValueLen := Length(FValue);
  Result := (LValueLen >= 2) and CharInSet(FValue[LValueLen], ['-', '+']);
  if Result then
  begin
    FDirective := Copy(FValue, 1, LValueLen - 1);
    case FValue[LValueLen] of
      '-': FEnabled := False;
      '+': FEnabled := True;
    else
      Result := False;
    end;
  end;
end;

{ TDcc32SimpleOpt }

function TDcc32SimpleOpt.TryParse(const ACMDLine: string; var APos: Integer): Boolean;
begin
  // simple option doesn't have data
  Result := True;
end;

{ TDcc32InlineOpt }

function TDcc32InlineOpt.TryParse(const ACMDLine: string; var APos: Integer): Boolean;
begin
  Result := inherited;
  if FValue = 'on' then
    FState := optOn
  else
  if FValue = 'off' then
    FState := optOff
  else
  if FValue = 'auto' then
    FState := optAuto
  else
    Exit(False);
end;

end.
