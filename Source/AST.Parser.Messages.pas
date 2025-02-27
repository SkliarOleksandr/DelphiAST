unit AST.Parser.Messages;

interface

uses SysUtils, Generics.Collections,
  AST.Lexer, AST.Intf;

type

  { TCompilerMessage }

  TCompilerMessage = class(TInterfacedObject, IASTParserMessage)
  strict private
    FModule: IASTModule;
    FModuleName: string; // can differs from FModule.Nam, bacuse of .inc files
    FMessageType: TCompilerMessageType;
    FMessageText: string;
    FSourcePosition: TTextPosition;
  private
    function GetModuleSource: string;
    function GetModuleName: string;
    function GetModule: IASTModule;
    procedure SetModule(const Value: IASTModule);
    procedure SetModuleName(const Value: string);
    function GetMessageType: TCompilerMessageType;
    function GetMessageTypeName: string;
    function GetMessageText: string;
    function GetCol: Integer;
    function GetRow: Integer;
    function GetModuleFullPath: string;
  public
    property Module: IASTModule read GetModule write SetModule;
    property ModuleName: string read GetModuleName write SetModuleName;
    property ModuleFullPath: string read GetModuleFullPath;
    property ModuleSource: string read GetModuleSource;
    property MessageType: TCompilerMessageType read GetMessageType;
    property MessageTypeName: string read GetMessageTypeName;
    property MessageText: string read GetMessageText;
    property Row: Integer read GetRow;
    property Col: Integer read GetCol;
    function AsString(AUnitFullPath: Boolean): string;
    constructor Create(const AModule: IASTModule; AMessageType: TCompilerMessageType;
                       const AMessageText: string; const APosition: TTextPosition);
  end;
  //PCompilerMessage = ^TCompilerMessage;

  ICompilerMessages = interface
    ['{0F47607D-E9F5-41F2-BB05-B539743EC65A}']
    procedure Add(const Message: IASTParserMessage);
    procedure Clear;
    procedure CopyFrom(const Messages: ICompilerMessages);
    function GetAsString: string;
    function GetCount: Integer;
    function GetItem(Index: Integer): IASTParserMessage;
    function GetErrorCount: Integer;
    function GetHintCount: Integer;
    function GetWarningCount: Integer;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IASTParserMessage read GetItem; default;
    property Text: string read GetAsString;
    property ErrorCount: Integer read GetErrorCount;
    property WarningCount: Integer read GetWarningCount;
    property HintCount: Integer read GetHintCount;
  end;

  TCompilerMessages = class(TInterfacedObject, ICompilerMessages)
  private
    FMessages: TList<IASTParserMessage>;
    function GetCount: Integer;
    function GetErrorCount: Integer;
    function GetHintCount: Integer;
    function GetWarningCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Message: IASTParserMessage);
    procedure CopyFrom(const Messages: ICompilerMessages);
    procedure Clear;
    property Count: Integer read GetCount;
    function GetAsString: string;
    function GetItem(Index: Integer): IASTParserMessage;
    property ErrorCount: Integer read GetErrorCount;
    property WarningCount: Integer read GetWarningCount;
    property HintCount: Integer read GetHintCount;
  end;

implementation

uses
  System.StrUtils,
  AST.Pascal.Parser,
  AST.Delphi.Errors,
  AST.Parser.Utils;

{ TCompilerMessage }

constructor TCompilerMessage.Create(const AModule: IASTModule; AMessageType: TCompilerMessageType;
  const AMessageText: string; const APosition: TTextPosition);
begin
  FModule := AModule;
  FMessageType := AMessageType;
  FMessageText := AMessageText;
  FSourcePosition := APosition;
end;

function TCompilerMessage.AsString(AUnitFullPath: Boolean): string;
begin
  Result := Format('%s [%s(%d, %d)]: %s', [
              GetMessageTypeName,
              IfThen(AUnitFullPath, ModuleFullPath, ModuleName),
              Row,
              Col,
              MessageText]);
end;

function TCompilerMessage.GetCol: Integer;
begin
  Result := FSourcePosition.Col;
end;

function TCompilerMessage.GetMessageText: string;
begin
  Result := FMessageText;
end;

function TCompilerMessage.GetMessageType: TCompilerMessageType;
begin
  Result := FMessageType;
end;

function TCompilerMessage.GetMessageTypeName: string;
begin
  case FMessageType of
    cmtHint: Result := sHintWord;
    cmtWarning: Result := sWarningWord;
    cmtError: Result := sErrorWord;
    cmtInteranlError: Result := sInternalErrorWord;
  end;
end;

function TCompilerMessage.GetModule: IASTModule;
begin
  Result := FModule;
end;

function TCompilerMessage.GetModuleFullPath: string;
begin
  Result := FModule.FileName;
end;

function TCompilerMessage.GetRow: Integer;
begin
  Result := FSourcePosition.Row;
end;

function TCompilerMessage.GetModuleName: string;
begin
  if FModuleName <> '' then
    Result := FModuleName
  else
    Result := FModule.Name;
end;

function TCompilerMessage.GetModuleSource: string;
begin
  if Assigned(FModule) then
    Result := (FModule as TPascalUnit).Lexer.Source
  else
    Result := '';
end;

procedure TCompilerMessage.SetModule(const Value: IASTModule);
begin
  FModule := Value;
end;

procedure TCompilerMessage.SetModuleName(const Value: string);
begin
  FModuleName := Value;
end;

{ TCompilerMessages }

procedure TCompilerMessages.Add(const Message: IASTParserMessage);
begin
  FMessages.Add(Message);
end;

procedure TCompilerMessages.Clear;
begin
  FMessages.Clear;
end;

procedure TCompilerMessages.CopyFrom(const Messages: ICompilerMessages);
var
  i: Integer;
begin
  for i := 0 to Messages.Count - 1 do
    FMessages.Add(Messages[i]);
end;

constructor TCompilerMessages.Create;
begin
  FMessages := TList<IASTParserMessage>.Create;
end;

destructor TCompilerMessages.Destroy;
begin
  FMessages.Free;
end;

function TCompilerMessages.GetAsString: string;
var
  LSrcPosStr: string;
begin
  for var LMessage in FMessages do
  begin
    if LMessage.Row <> -1 then
    begin
      if LMessage.Col <> -1 then
        LSrcPosStr := format('(%d,%d)', [LMessage.Row, LMessage.Col])
      else
        LSrcPosStr := format('(%d)', [LMessage.Row]);
    end;
    Result := AddStringSegment(Result, Format('[%s] %s%s: %s',
      [LMessage.MessageTypeName, LMessage.ModuleName, LSrcPosStr, LMessage.MessageText]), #13#10);
  end;
end;

function TCompilerMessages.GetCount: Integer;
begin
  Result := FMessages.Count;
end;

function TCompilerMessages.GetErrorCount: Integer;
begin
  Result := 0;
  for var LIndex := 0 to FMessages.Count - 1 do
    if FMessages[LIndex].MessageType in [cmtError, cmtInteranlError] then
      Inc(Result);
end;

function TCompilerMessages.GetHintCount: Integer;
begin
  Result := 0;
  for var LIndex := 0 to FMessages.Count - 1 do
    if FMessages[LIndex].MessageType = cmtHint then
      Inc(Result);
end;

function TCompilerMessages.GetItem(Index: Integer): IASTParserMessage;
begin
  Result := FMessages[Index];
end;

function TCompilerMessages.GetWarningCount: Integer;
begin
  Result := 0;
  for var LIndex := 0 to FMessages.Count - 1 do
    if FMessages[LIndex].MessageType = cmtWarning then
      Inc(Result);
end;

end.
