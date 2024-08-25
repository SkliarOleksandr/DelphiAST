unit AST.Parser.Messages;

interface

{$I AST.Parser.Defines.inc}

uses SysUtils, Generics.Collections, AST.Lexer;

type


  IUnit = interface
    ['{80A26C85-754B-4D35-BFA4-5FFFBA78322B}']
  end;

  TCompilerMessageType = (cmtHint, cmtWarning, cmtError, cmtInteranlError);

  { TCompilerMessage }

  TCompilerMessage = record
  strict private
    FUnit: TObject;
    FUnitName: string;
    FMessageType: TCompilerMessageType;
    FMessageText: string;
    FSourcePosition: TTextPosition;
    function GetMessageTypeName: string;
    function GetAsString: string;
  public
    //property DeclUnit: TObject read FUnit write FUnit;
    property UnitName: string read FUnitName write FUnitName;
    property MessageType: TCompilerMessageType read FMessageType;
    property MessageTypeName: string read GetMessageTypeName;
    property MessageText: string read FMessageText;
    property Row: Integer read FSourcePosition.Row write FSourcePosition.Row;
    property Col: Integer read FSourcePosition.Col write FSourcePosition.Col;
    property AsString: string read GetAsString;
    constructor Create(DeclUnit: TObject; MessageType: TCompilerMessageType; const MessageText: string; const SourcePosition: TTextPosition);
    {$IFDEF FPC}
    class operator Equal(const Left, Right: TCompilerMessage): boolean;
    {$ENDIF}
  end;
  PCompilerMessage = ^TCompilerMessage;

  ICompilerMessages = interface
    ['{0F47607D-E9F5-41F2-BB05-B539743EC65A}']
    procedure Add(const Message: TCompilerMessage);
    procedure Clear;
    procedure CopyFrom(const Messages: ICompilerMessages);
    function GetHasErrors: Boolean;
    function GetAsString: string;
    function GetCount: Integer;
    function GetItem(Index: Integer): TCompilerMessage;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TCompilerMessage read GetItem; default;
    property Text: string read GetAsString;
    property HasErrors: Boolean read GetHasErrors;
  end;

  TCompilerMessages = class(TInterfacedObject, ICompilerMessages)
  private
    FMessages: TList<TCompilerMessage>;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Message: TCompilerMessage);
    procedure CopyFrom(const Messages: ICompilerMessages);
    procedure Clear;
    property Count: Integer read GetCount;
    function GetHasErrors: Boolean;
    function GetAsString: string;
    function GetItem(Index: Integer): TCompilerMessage;
  end;

implementation

uses AST.Pascal.Parser,
     AST.Delphi.Errors,
     AST.Parser.Utils;

{ TCompilerMessage }

constructor TCompilerMessage.Create(DeclUnit: TObject; MessageType: TCompilerMessageType; const MessageText: string;
                                    const SourcePosition: TTextPosition);
begin
  FUnit := DeclUnit;
  FMessageType := MessageType;
  FMessageText := MessageText;
  FSourcePosition := SourcePosition;
end;

{$IFDEF FPC}
class operator TCompilerMessage.Equal(const Left, Right: TCompilerMessage): boolean;
begin

end;
{$ENDIF}

function TCompilerMessage.GetAsString: string;
begin
  Result := Format('%s [%s(%d, %d)]: %s', [GetMessageTypeName, Self.UnitName, Row, Col, MessageText]);
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


{ TCompilerMessages }

procedure TCompilerMessages.Add(const Message: TCompilerMessage);
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
  FMessages := TList<TCompilerMessage>.Create;
end;

destructor TCompilerMessages.Destroy;
begin
  FMessages.Free;
end;

function TCompilerMessages.GetAsString: string;
var
  i: Integer;
  SrcCoords: string;
begin
  for i := 0 to FMessages.Count - 1 do
  with FMessages[i] do begin
    if Row <> -1 then
    begin
      if Col <> -1 then
        SrcCoords := format('(%d,%d)', [Row, Col])
      else
        SrcCoords := format('(%d)', [Row]);
    end;
    Result := AddStringSegment(Result, Format('[%s] %s%s: %s', [MessageTypeName, UnitName, SrcCoords, MessageText]), #13#10);
  end;
end;

function TCompilerMessages.GetCount: Integer;
begin
  Result := FMessages.Count;
end;

function TCompilerMessages.GetHasErrors: Boolean;
var
  i: Integer;
begin
  for i := 0 to FMessages.Count - 1 do
    if FMessages[i].MessageType in [cmtError, cmtInteranlError] then
      Exit(True);
  Result := False;
end;

function TCompilerMessages.GetItem(Index: Integer): TCompilerMessage;
begin
  Result := FMessages[Index];
end;

end.
