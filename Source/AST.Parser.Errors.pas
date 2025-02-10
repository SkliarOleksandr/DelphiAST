unit AST.Parser.Errors;

interface

uses SysUtils, AST.Intf, AST.Lexer;

type

  ECompilerAbort = class(EAbort)
  private
    FCompilerMessage: IASTParserMessage;
  public
    constructor Create(const MessageText: string); overload;
    constructor Create(const MessageText: string; const SourcePosition: TTextPosition); overload;
    constructor CreateAsInteranl(const MessageText: string; const SourcePosition: TTextPosition);
    property CompilerMessage: IASTParserMessage read FCompilerMessage;
  end;

  ECompilerInternalError = class(ECompilerAbort);

  ECompilerStop = class(ECompilerAbort)
  private
    FCompileSuccess: Boolean;
  public
    constructor Create(CompileSuccess: Boolean); reintroduce;
    property CompileSuccess: Boolean read FCompileSuccess;
  end;

  ECompilerSkip = class(ECompilerAbort)
    constructor Create();
  end;

  procedure AbortWork(const Message: string; const SourcePosition: TTextPosition); overload;
  procedure AbortWork(const MessageFmt: string; const Params: array of const; const SourcePosition: TTextPosition); overload;

  procedure AbortWorkInternal(const Message: string); overload;
  procedure AbortWorkInternal(const Message: string; const SourcePosition: TTextPosition); overload;
  procedure AbortWorkInternal(const Message: string; const Params: array of const); overload;
  procedure AbortWorkInternal(const Message: string; const Params: array of const; const SourcePosition: TTextPosition); overload;

var
  BreakpointOnError: Boolean;

implementation

{ EComplilerAbort }

uses
  Winapi.Windows,
  AST.Parser.Messages,
  AST.Parser.Log;

constructor ECompilerAbort.Create(const MessageText: string);
begin
  inherited Create(MessageText);
  FCompilerMessage := TCompilerMessage.Create(nil, cmtError, MessageText, TTextPosition.Create(0, 0));
end;

constructor ECompilerAbort.Create(const MessageText: string; const SourcePosition: TTextPosition);
begin
  inherited Create(MessageText);
  FCompilerMessage := TCompilerMessage.Create(nil, cmtError, MessageText, SourcePosition);
end;

constructor ECompilerAbort.CreateAsInteranl(const MessageText: string; const SourcePosition: TTextPosition);
begin
  inherited Create(MessageText);
  FCompilerMessage := TCompilerMessage.Create(nil, cmtInteranlError, MessageText, SourcePosition);
end;

{ EComplilerStop }

constructor ECompilerStop.Create(CompileSuccess: Boolean);
begin
  FCompileSuccess := CompileSuccess;
end;

{ ECompilerSkip }

constructor ECompilerSkip.Create;
begin

end;

procedure DebugBreak;
asm
  int 3;
end;

procedure AbortWork(const Message: string; const SourcePosition: TTextPosition);
begin
  // stop if run under IDE only
  if (BreakpointOnError) and (System.DebugHook > 0) then
    DebugBreak;

  WriteLog('ERROR: ' + Message);
  raise ECompilerAbort.Create(Message, SourcePosition);
end;

procedure AbortWork(const MessageFmt: string; const Params: array of const; const SourcePosition: TTextPosition);
begin
  AbortWork(Format(MessageFmt, Params), SourcePosition);
end;

procedure AbortWorkInternal(const Message: string; const Params: array of const; const SourcePosition: TTextPosition);
begin
  if BreakpointOnError then
    DebugBreak;
  WriteLog('INTERNAL ERROR: ' + Message, Params);
  raise ECompilerInternalError.CreateAsInteranl(Format(Message, Params), SourcePosition);
end;

procedure AbortWorkInternal(const Message: string);
begin
  AbortWorkInternal(Message, [], TTextPosition.Empty);
end;

procedure AbortWorkInternal(const Message: string; const SourcePosition: TTextPosition);
begin
  AbortWorkInternal(Message, [], SourcePosition);
end;

procedure AbortWorkInternal(const Message: string; const Params: array of const);
begin
  AbortWorkInternal(Message, Params, TTextPosition.Empty);
end;


end.
