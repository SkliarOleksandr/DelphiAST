unit AST.Parser.Log;

interface

uses
  System.SysUtils;

type
  TASTLogWriteProc = reference to procedure (const AMessage: string; ANestedLevel: Integer);

  TASTParserLog = {singleton} class
  private
    FOnWriteProc: TASTLogWriteProc;
    FNestedLevel: Integer;
    class var FInstance: TASTParserLog;
  public
    class property Instance: TASTParserLog read FInstance;
    property OnWriteProc: TASTLogWriteProc read FOnWriteProc write FOnWriteProc;
    procedure Write(const AMessage: string); overload;
    procedure Write(const AMessage: string; const AParams: array of const); overload;
    procedure WriteBegin(const AMessage: string; const AParams: array of const);
    procedure WriteEnd(const AMessage: string; const AParams: array of const);
    procedure ResetNestedLevel;
  end;

procedure WriteLog(const AMessage: string); overload;
procedure WriteLog(const AMessage: string; const AParams: array of const); overload;

procedure LogBegin(const AMessage: string); overload;
procedure LogBegin(const AMessage: string; const AParams: array of const); overload;
procedure LogEnd(const AMessage: string); overload;
procedure LogEnd(const AMessage: string; const AParams: array of const); overload;

implementation

{ TASTParserLog }

procedure TASTParserLog.Write(const AMessage: string; const AParams: array of const);
begin
  if Assigned(FOnWriteProc) then
    FOnWriteProc(Format(AMessage, AParams), FNestedLevel);
end;

procedure TASTParserLog.ResetNestedLevel;
begin
  FNestedLevel := 0;
end;

procedure TASTParserLog.Write(const AMessage: string);
begin
  if Assigned(FOnWriteProc) then
    FOnWriteProc(AMessage, FNestedLevel);
end;

procedure TASTParserLog.WriteBegin(const AMessage: string; const AParams: array of const);
begin
  if Assigned(FOnWriteProc) then
    FOnWriteProc(Format('< ' + AMessage, AParams), FNestedLevel);
  Inc(FNestedLevel);
end;

procedure TASTParserLog.WriteEnd(const AMessage: string; const AParams: array of const);
begin
  Dec(FNestedLevel);
  if Assigned(FOnWriteProc) then
    FOnWriteProc(Format('> ' + AMessage, AParams), FNestedLevel);
end;

procedure WriteLog(const AMessage: string; const AParams: array of const);
begin
  TASTParserLog.Instance.Write(AMessage, AParams);
end;

procedure WriteLog(const AMessage: string);
begin
  TASTParserLog.Instance.Write(AMessage);
end;

procedure LogBegin(const AMessage: string);
begin
  TASTParserLog.Instance.WriteBegin(AMessage, []);
end;

procedure LogBegin(const AMessage: string; const AParams: array of const);
begin
  TASTParserLog.Instance.WriteBegin(AMessage, AParams);
end;

procedure LogEnd(const AMessage: string);
begin
  TASTParserLog.Instance.WriteEnd(AMessage, []);
end;

procedure LogEnd(const AMessage: string; const AParams: array of const);
begin
  TASTParserLog.Instance.WriteEnd(AMessage, AParams);
end;


initialization
  TASTParserLog.FInstance := TASTParserLog.Create;

finalization
  FreeAndNil(TASTParserLog.FInstance);

end.
