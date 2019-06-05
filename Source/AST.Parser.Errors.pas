unit AST.Parser.Errors;

interface

uses SysUtils, NPCompiler.Errors, NPCompiler.Intf, AST.Lexer;

type

  ECompilerAbort = class(EAbort)
  private
    FCompilerMessage: TCompilerMessage;
    function GetCompilerMessage: PCompilerMessage;
  public
    constructor Create(const MessageText: string); overload;
    constructor Create(const MessageText: string; const SourcePosition: TTextPosition); overload;
    constructor CreateAsInteranl(const MessageText: string; const SourcePosition: TTextPosition);
    property CompilerMessage: PCompilerMessage read GetCompilerMessage;
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

  procedure AbortWork(Error: TCompilerError); overload;
  procedure AbortWork(Error: TCompilerError; const SourcePosition: TTextPosition); overload;
  procedure AbortWork(Error: TCompilerError; const Params: array of const; const SourcePosition: TTextPosition); overload;


  procedure AbortWork(const Message: string; const SourcePosition: TTextPosition); overload;
  procedure AbortWork(const MessageFmt: string; const Params: array of const; const SourcePosition: TTextPosition); overload;

  procedure AbortWorkInternal(const Message: string); overload;
  procedure AbortWorkInternal(const Message: string; const SourcePosition: TTextPosition); overload;
  procedure AbortWorkInternal(const Message: string; const Params: array of const); overload;
  procedure AbortWorkInternal(const Message: string; const Params: array of const; const SourcePosition: TTextPosition); overload;

implementation

{ EComplilerAbort }

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

function ECompilerAbort.GetCompilerMessage: PCompilerMessage;
begin
  Result := addr(FCompilerMessage);
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

procedure AbortWork(const Message: string; const SourcePosition: TTextPosition);
begin
  raise ECompilerAbort.Create(Message, SourcePosition);
end;

procedure AbortWorkInternal(const Message: string);
var
  POS: TTextPosition;
begin
  POS.Row := 0;
  POS.Col := 0;
  raise ECompilerAbort.CreateAsInteranl(Message, POS);
end;

procedure AbortWorkInternal(const Message: string; const SourcePosition: TTextPosition);
begin
  raise ECompilerAbort.CreateAsInteranl(Message, SourcePosition);
end;

procedure AbortWorkInternal(const Message: string; const Params: array of const);
var
  POS: TTextPosition;
begin
  POS.Row := 0;
  POS.Col := 0;
  raise ECompilerAbort.CreateAsInteranl(Format(Message, Params), POS);
end;

procedure AbortWorkInternal(const Message: string; const Params: array of const; const SourcePosition: TTextPosition);
begin
  raise ECompilerAbort.CreateAsInteranl(Format(Message, Params), SourcePosition);
end;

procedure AbortWork(Error: TCompilerError);
var
  ErrorStr: string;
begin
  ErrorStr := GetErrorText(Error);
  raise ECompilerAbort.Create(ErrorStr);
end;

procedure AbortWork(Error: TCompilerError; const SourcePosition: TTextPosition); overload;
var
  ErrorStr: string;
begin
  ErrorStr := GetErrorText(Error);
  raise ECompilerAbort.Create(ErrorStr, SourcePosition);
end;

procedure AbortWork(Error: TCompilerError; const Params: array of const; const SourcePosition: TTextPosition); overload;
var
  ErrorStr: string;
begin
  ErrorStr := GetErrorText(Error);
  ErrorStr := Format(ErrorStr, Params);
  raise ECompilerAbort.Create(ErrorStr, SourcePosition);
end;

procedure AbortWork(const MessageFmt: string; const  Params: array of const; const SourcePosition: TTextPosition);
begin
  AbortWork(Format(MessageFmt, Params), SourcePosition);
end;


end.
