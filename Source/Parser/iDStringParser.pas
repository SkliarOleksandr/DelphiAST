unit iDStringParser;

interface

{$i compilers.inc}

uses Classes, SysUtils;

const
  MaxTokenChar = #127;

type


  TTokenType = (
    ttNone,
    ttToken,
    ttOmited,
    ttDigit,
    ttCharCode,
    ttNewLine,
    ttHexPrefix,
    ttUnicodeChars,
    ttBinPrefix,
    ttQuote,
    ttQuoteMulti,
    ttOneLineRem,
    ttStartRem,
    ttEndRem
  );

  TIdentifierType = (
    itNone,
    itIdentifier, // named identifier like: 'a', 'value', 'width', etc.
    itChar,       // char symbol like: 'a', 'b', '1', etc.
    itCharCodes,  // string like: #$D, #1234, #10#13, etc.
    itString,     // string like: 'abc', '111222333', etc.
    itInteger,    // integer number like: 12345, -4534534534, etc.
    itHextNumber, // hexadecimal number like: $FF, -$AABB, $12, etc.
    itBinNumber,  // binary number like: %1010101, -%1010101, etc.
    itFloat       // floating point number like: 1.5, -4234.53, etc
  );


  TParserErrorState = (psNoError, psCommentError, psStringError, psCharError, psSyntaxError);

  TTokenFlag = (tfBinDigit, tfHexDigit, tfDigit, tfFloat, tfSeparator, tfEndBlockRem);
  TTokenFlags = set of TTokenFlag;

  TCharToken = record
  type
    TTokens = array of TCharToken;
  var
    TokenChar: Char;
    TokenID: Integer;
    TokenType: TTokenType;
    Flags: TTokenFlags;
    ChildTokens: TTokens;
  end;
  PCharToken = ^TCharToken;

  TParseTokens = array [#0..#127] of TCharToken;
  PParseTokens = ^TParseTokens;

  TTextPosition = packed record
    Row: Integer;
    Col: Integer;
    constructor Create(Row: Integer); overload;
    constructor Create(Row, Col: Integer); overload;
  end;

  TIdentifier = record
    Name: string;
    TextPosition: TTextPosition;
  end;

  TParserPosition = record
    SourcePosition: Integer;
    Row, Col: Integer;
    LastEnterPos: Integer;
    TokenID: Integer;
    OriginalToken: string;
  end;

  {$IFDEF NEXTGEN}
  AnsiChar = System.UTF8Char;
  {$ENDIF}


  TGenericLexer = class
  strict private
    FTokens: TParseTokens;
    FSource: string;                   // Текст
    FLength: integer;                  // Предвычесленная длинна текста
    FSrcPos: integer;                  // Текущая позиция парсера в тексте
    FCurrentToken: string;
    FCurrentTokenID: Integer;
    FRow: Integer;                     // Номер строки текущей позиции (начинается с единицы)
    FLastEnterPos: Integer;            // Позиция последнего переноса строки (необходим для определения позиции формата (row,col))
    FPrevPostion: TTextPosition;       // Позиция предыдущего токена формата (row,col)
    FUpCase: array [#0..#127] of AnsiChar; // символы в UpCase
    FIdentifireType: TIdentifierType;  // Тип идентификатора (литерал/строка/число/символ)
    FIdentifireID: Integer;            // TokenID если идентификатор
    FEofID: Integer;                   // TokenID если конец файла
    FTokenCaptions: TStrings;          // Название токоенов
    FSeparators: string;
    FErrorState: TParserErrorState;
    FOmitted: string;
    FTokenIDGenerator: Integer;
    fCutToken: PCharToken;
  private
    function GetPosition: TTextPosition; inline;
    function GetLinePosition: TTextPosition; inline;
    procedure SetSource(const Value: string);
    procedure SetSeparators(const Value: string);
    procedure SetOmitted(const Value: string);
    function GetTokenName: string;
  protected
    procedure RegisterToken(const Token: string; aTokenID: Integer; aTokenType: TTokenType; const TokenCaption: string = '');
    //procedure RegisterRemToken(const BeginToken, EndToken: string);

    procedure RegisterRemToken(const BeginToken, EndToken: string; BeginTokenID, EndTokenID: Integer);
    procedure ParseMultiLineRem(TokenID: Integer; var SPos: Integer);
    procedure ParseOneLineRem(var SPos: Integer);
    procedure ParseDidgit(SPos: Integer);
    procedure ParseCharCode(SPos: Integer); overload;
    procedure ParseCharCode(SPos: Integer; out Chars: string); overload;
    procedure ParseQuote(Ch: Char; SPos: Integer);
    function ParseQuoteMulti(Ch: Char; SPos: Integer): Integer;
    procedure ParseUnicodeChars(SPos: Integer);
    procedure ParseBinPrefix(SPos: Integer);
    procedure ParseHexPrefix(SPos: Integer);

    property TokenCaptions: TStrings read FTokenCaptions;
    property EofID: Integer read FEofID write FEofID;
    property IdentifireID: Integer read FIdentifireID write FIdentifireID;
    property SeparatorChars: string read FSeparators write SetSeparators;
    property OmittedChars: string read FOmitted write SetOmitted;
    property Tokens: TParseTokens read FTokens;
    property CurToken: PCharToken read fCutToken;
    property CurrentToken: string read FCurrentToken;            // Для не идентификаторов равен пустой строке
    function NextTokenID: Integer;
    function GetNextToken: PCharToken;
  public
    constructor Create(const Source: string); virtual;
    destructor Destroy; override;
    ///////////////////////////////////////////////////
    procedure First;
    property CurrentTokenID: Integer read FCurrentTokenID;
    property ErrorState: TParserErrorState read FErrorState;
    property Source: string read FSource write SetSource;
    property SourcePosition: Integer read FSrcPos;
    property TokenName: string read GetTokenName;                 // Строковое представление токена
    property Position: TTextPosition read GetPosition;            // Позиция текущего токена формата (row,col)
    property LinePosition: TTextPosition read GetLinePosition;
    property IdentifireType: TIdentifierType read FIdentifireType;
    function TokenLexem(TokenID: Integer): string;
    function GetSubString(StartPos, EndPos: Integer): string;
    procedure GetIdentifier(var ID: TIdentifier);
    procedure GetTokenAsIdentifier(var ID: TIdentifier);
    property PrevPosition: TTextPosition read FPrevPostion;
    procedure SaveState(out State: TParserPosition);
    procedure LoadState(const State: TParserPosition);

  end;

implementation

{ TStringParser }

function TGenericLexer.GetNextToken: PCharToken;
var
  Ch: Char;
begin
  Ch := Char(FUpCase[FSource[FSrcPos]]);
  Result := addr(Tokens[Ch]);
end;

constructor TGenericLexer.Create(const Source: string);
var
  c: Char;
begin
  FTokenIDGenerator := 1000;
  SetSource(Source);
  FTokenCaptions := TStringList.Create;
  FTokens[MaxTokenChar].TokenType := ttUnicodeChars;
  FTokens['0'].Flags := [tfBinDigit];
  FTokens['1'].Flags := [tfBinDigit];
  for c := '0' to '9' do begin
    with FTokens[c] do begin
      TokenType := ttDigit;
      Flags := Flags + [tfDigit, tfHexDigit];
    end;
  end;
  for c := 'A' to 'F' do begin
    FTokens[c].Flags := [tfHexDigit];
  end;
  // UpCase таблица
  for c := Low(FUpCase) to High(FUpCase) do
    FUpCase[c] := AnsiChar(UpCase(c));
end;

destructor TGenericLexer.Destroy;
begin
  FTokenCaptions.Free;
  inherited;
end;

procedure TGenericLexer.First;
begin
  FSrcPos := 1;
  FRow := 1;
  FCurrentToken := '';
  FCurrentTokenID := -1;
  FLastEnterPos := 0;
end;

function TGenericLexer.NextTokenID: Integer;
var
  SPos, SPos2, ReadedChars, i, RemID: Integer;
  Ch: Char;
  Token, ptmp, ptmp2: PCharToken;
begin
  FPrevPostion := Position;
  FIdentifireType := itNone;
  Result := FEofID;
  SPos := FSrcPos;
  ReadedChars := 1;
  RemID := -1;
  while SPos <= FLength do begin
    Ch := FSource[SPos];
    if Ch < MaxTokenChar then begin
      // первые символы токенов в списке Tokens есть и в LowerCase и в UpperCase
      // поэтому приводить к UpperCase не нужно
      Token := Addr(Tokens[Ch]);
      if Assigned(Token.ChildTokens) then
      begin
        ptmp2 := Token;
        SPos2 := SPos + 1;
        while SPos2 <= FLength do begin
          // следующие символы токенов в списке Tokens только в UpperCase
          // поэтому приводим к UpperCase
          Ch := FSource[SPos2];
          if Ch > MaxTokenChar then begin
            Token := ptmp2;
            Break;
          end;
          Ch := Char(FUpCase[Ch]);
          ptmp := nil;
          for i := 0 to Length(ptmp2.ChildTokens) - 1 do
          begin
            ptmp := Addr(ptmp2.ChildTokens[i]);
            if Ch = ptmp.TokenChar then begin
              ptmp2 := ptmp;
              Inc(SPos2);
              Break; // next step in multi-char block rem
            end;
          end;
          if ptmp = ptmp2 then Continue else Break;
        end;
        if ptmp2 <> Token then begin
          if (ptmp2.TokenType <> ttNone) and ((SPos2 > FLength) or (tfSeparator in Tokens[Ch].Flags) or (tfSeparator in ptmp2.Flags)) then begin
            Token := ptmp2;
          end else
            ReadedChars := SPos2 - SPos;
          SPos := SPos2 - 1;
        end;
      end;
    end else
      Token := Addr(Tokens[MaxTokenChar]); // Unicode chars...

    fCutToken := Token;
    case Token.TokenType of
      ttNone: begin // read some identifier:
        Inc(SPos);
        while SPos <= FLength do
        begin
          Ch := FSource[SPos];
          if (Ord(Ch) < Length(Tokens)) and (tfSeparator in Tokens[Ch].Flags) then
            Break;
          Inc(ReadedChars);
          Inc(SPos);
        end;
        // read identifier:
        FCurrentToken := Copy(FSource, SPos - ReadedChars, ReadedChars);
        FIdentifireType := itIdentifier;
        Result := FIdentifireID;
        FCurrentTokenID := Result;
        FSrcPos := SPos;
        Exit;
      end;
      ttToken: begin
        FSrcPos := SPos + 1;
        Result := Token.TokenID;
        FCurrentTokenID := Result;
        {$IFDEF DEBUG} FCurrentToken := TokenLexem(Result); {$ENDIF}
        Exit;
      end;
      ttOmited: begin
        Inc(SPos);
        Continue;
      end;
      ttUnicodeChars: begin
        ParseUnicodeChars(SPos);
        Result := FIdentifireID;
        Exit;
      end;
      ttDigit: begin
        ParseDidgit(SPos);
        Result := FIdentifireID;
        Exit;
      end;
      ttCharCode: begin
        ParseCharCode(SPos);
        Result := FIdentifireID;
        Exit;
      end;
      ttNewLine: begin
        // process NEWLINE:
        Inc(FRow);
        FLastEnterPos := SPos;
      end;
      ttHexPrefix: begin
        ParseHexPrefix(SPos);
        Result := FIdentifireID;
        Exit;
      end;
      ttBinPrefix: begin
        ParseBinPrefix(SPos);
        Result := FIdentifireID;
        Exit;
      end;
      ttQuote: begin
        ParseQuote(Ch, SPos);
        Result := FIdentifireID;
        Exit;
      end;
      ttQuoteMulti: begin
        Result := ParseQuoteMulti(Ch, SPos);
        Exit;
      end;
      ttOneLineRem: begin
        ParseOneLineRem(SPos);
        ReadedChars := 1;
      end;
      ttStartRem: begin
        ParseMultiLineRem(Token.TokenID, SPos);
        ReadedChars := 1;
      end;
      ttEndRem: begin
        if RemID <> -1 then
        begin
          Inc(SPos);
          Result := FIdentifireID;// !!!
          FSrcPos := SPos;
        end else begin
          Result := Token.TokenID;
          FSrcPos := SPos + 1;
        end;
        FCurrentTokenID := Result;
        {$IFDEF DEBUG} FCurrentToken := TokenLexem(Result); {$ENDIF}
        Exit;
      end;
    end;
    Inc(SPos);
  end;
end;

procedure TGenericLexer.ParseBinPrefix(SPos: Integer);
var
  ReadedChars: Integer;
begin
  Inc(SPos);
  ReadedChars := 0;
  while (SPos <= FLength) and (tfBinDigit in Tokens[UpCase(FSource[SPos])].Flags) do begin
    Inc(SPos);
    Inc(ReadedChars);
  end;
  // read identifier:
  FCurrentToken := Copy(FSource, SPos - ReadedChars, ReadedChars);
  FIdentifireType := itBinNumber;
  FCurrentTokenID := FIdentifireID;
  FSrcPos := SPos;
end;

procedure TGenericLexer.ParseHexPrefix(SPos: Integer);
var
  ReadedChars: Integer;
begin
  Inc(SPos);
  ReadedChars := 0;
  while (SPos <= FLength) and (tfHexDigit in Tokens[UpCase(FSource[SPos])].Flags) do begin
    inc(SPos);
    Inc(ReadedChars);
  end;
  // read identifier:
  FCurrentToken := Copy(FSource, SPos - ReadedChars, ReadedChars);
  FIdentifireType := itHextNumber;
  FCurrentTokenID := FIdentifireID;
  FSrcPos := SPos;
end;

procedure TGenericLexer.ParseDidgit(SPos: Integer);
type
  TNumberSymbols = set of (nsExponent, nsPoint, nsSign);
var
  SPos2, ReadedChars: integer;
  Ch: Char;
  NumberSymbols: TNumberSymbols;
begin
  NumberSymbols := [];
  ReadedChars := 0;
  Inc(SPos);
  Inc(ReadedChars);
  while SPos <= FLength do begin
    Ch := Char(FUpCase[FSource[SPos]]);
    if Ch = 'E' then begin
      if nsExponent in NumberSymbols then Break;
      Include(NumberSymbols, nsExponent);
    end else
    if (Ch = '.') then begin
      SPos2 := SPos + 1;
      if (nsPoint in NumberSymbols) or (SPos2 > FLength) or (not (tfDigit in Tokens[FSource[SPos2]].Flags)) then break;
      Include(NumberSymbols, nsPoint);
    end else
    if ((Ch = '+') or (Ch = '-')) and (nsExponent in NumberSymbols) then begin
      if nsSign in NumberSymbols then Break;
      Include(NumberSymbols, nsSign);
    end else
    if (Tokens[Ch].TokenType <> ttDigit) then Break;
    Inc(SPos);
    Inc(ReadedChars);
  end;
  // read identifier:
  FCurrentToken := Copy(FSource, SPos - ReadedChars, ReadedChars);
  if nsPoint in NumberSymbols then
    FIdentifireType := itFloat
  else
    FIdentifireType := itInteger;

  FCurrentTokenID := FIdentifireID;
  FSrcPos := SPos;
end;

procedure TGenericLexer.ParseMultiLineRem(TokenID: Integer; var SPos: Integer);
var
  SPos2, i, RemID: integer;
  Token, ptmp: PCharToken;
  Ch: Char;
begin
  RemID := TokenID;
  while SPos < FLength do begin
    Inc(SPos);
    Ch := FSource[SPos];
    if Ch > MaxTokenChar then
      continue;
    Token := Addr(Tokens[Ch]);
    // process NEWLINE:
    if Token.TokenType = ttNewLine then begin
      Inc(FRow);
      FLastEnterPos := SPos;
    end else
    if (tfEndBlockRem in Token.Flags) then begin
      if Token.TokenType = ttEndRem then begin
         if Token.TokenID = RemID then Break // end of "one-char" block rem
      end else begin
        SPos2 := SPos + 1;
        while SPos2 < FLength do begin
          Ch := FSource[SPos2];
          for i := 0 to Length(Token.ChildTokens) - 1 do
          begin
            ptmp := Addr(Token.ChildTokens[i]);
            with ptmp^ do
            if Ch = TokenChar then begin
              if tfEndBlockRem in Flags then begin
                if (TokenID = RemID) and (TokenType = ttEndRem) then begin
                  SPos := SPos2;
                  Break; // end of "multi-char" block rem
                end;
              end else begin
                Inc(SPos2);
                Break; // next step in multi-char block rem
              end;
            end else Continue;
            Break;
          end;
          Break;
        end;
        if SPos = SPos2 then Break; // "multi-char" block rem was successfully found
      end;
    end;
  end;
end;

procedure TGenericLexer.ParseOneLineRem(var SPos: Integer);
var
  Ch: Char;
begin
  repeat
     Inc(SPos);
     if SPos > FLength then
       Break;

     Ch := FSource[SPos];
     if (Ord(Ch) < Length(Tokens)) and (Tokens[Ch].TokenType = ttNewLine) then
       Break;
  until False;
  // process NEWLINE:
  Inc(FRow);
  FLastEnterPos := SPos;
end;

procedure TGenericLexer.ParseQuote(Ch: Char; SPos: Integer);
var
  QuoteChar: Char;
  SPos2, ReadedChars: Integer;
begin
  QuoteChar := Ch;
  ReadedChars := 0;
  Inc(SPos);
  while SPos <= FLength do begin
    Ch := FSource[SPos];
    if (Ch = QuoteChar) then
    begin
      SPos2 := SPos + 1;
      if (SPos2 <= FLength) and (FSource[SPos2] = QuoteChar) then
      begin
        Inc(SPos); // skip second quote char
        Inc(ReadedChars);
      end else begin
        Break;
      end;
    end;
    Inc(SPos);
    Inc(ReadedChars);
  end;
  // read identifier:
  FCurrentToken := Copy(FSource, SPos - ReadedChars, ReadedChars);
  FCurrentToken := StringReplace(FCurrentToken, '''''', '''', [rfReplaceAll]);

  if ReadedChars = 1 then
    FIdentifireType := itChar
  else
    FIdentifireType := itString;

  // end read;
  FCurrentTokenID := FIdentifireID;
  FSrcPos := SPos + 1;

//  Inc(SPos);
end;

function TGenericLexer.ParseQuoteMulti(Ch: Char; SPos: Integer): Integer;
var
  QuoteChar: Char;
  SignIdx, SPos2, ReadedChars, SignReadedChars, SignLen: Integer;
  Sign: string;
begin
  QuoteChar := Ch;
  SignReadedChars := 0;
  // читаем сигнатуру
  Inc(SPos);
  while SPos <= FLength do begin
    Ch := FSource[SPos];
    if (Ch = QuoteChar) then
      break;
    Inc(SPos);
    Inc(SignReadedChars);
  end;

  Sign := UpperCase(Copy(FSource, SPos - SignReadedChars, SignReadedChars)) + QuoteChar;
  SignLen := Length(Sign);

  ReadedChars := 0;
  // читаем многострочную строку
  Inc(SPos);
  while SPos <= FLength do begin
    Ch := FSource[SPos];
    if (Ch = QuoteChar) then
    begin
      SPos2 := SPos + 1;
      SignReadedChars := 0;
      while (SPos2 <= FLength) do begin
        Ch := FSource[SPos2];
        SignIdx := SPos2 - SPos;
        if (SignIdx <= SignLen) and (UpCase(Ch) = Sign[SignIdx]) then
        begin
          Inc(SPos2);
          Inc(SignReadedChars);
          continue;
        end else
          break;
      end;
      if SignReadedChars = Length(Sign) then
        break;
    end;
    Inc(SPos);
    Inc(ReadedChars);
  end;

  if SPos > FLength then
    Exit(FEofID);

  // read identifier:
  FCurrentToken := Copy(FSource, SPos - ReadedChars, ReadedChars);
  if ReadedChars = 1 then
    FIdentifireType := itChar
  else
    FIdentifireType := itString;

  // end read;
  FCurrentTokenID := FIdentifireID;
  FSrcPos := SPos + SignLen + 1;

  Result := FIdentifireID;
end;

procedure TGenericLexer.ParseUnicodeChars(SPos: Integer);
var
  ReadedChars: Integer;
  Ch: Char;
begin
  ReadedChars := 1;
  Inc(SPos);
  while SPos <= FLength do begin
    Ch := FSource[SPos];
    if (Ch < MaxTokenChar) and (tfSeparator in Tokens[Ch].Flags) then Break;
    Inc(ReadedChars);
    Inc(SPos);
  end;
  // read identifier:
  FCurrentToken := Copy(FSource, SPos - ReadedChars, ReadedChars);
  FIdentifireType := itIdentifier;
  FSrcPos := SPos;
  FCurrentTokenID := FIdentifireID;
  // end read;
end;

procedure TGenericLexer.RegisterRemToken(const BeginToken, EndToken: string; BeginTokenID, EndTokenID: Integer);
begin
  RegisterToken(BeginToken, EndTokenID, ttStartRem);
  RegisterToken(EndToken, EndTokenID, ttEndRem);
end;

{procedure TStringParser.RegisterRemToken(const BeginToken, EndToken: string);
begin
  RegisterToken(BeginToken, FTokenIDGenerator, ttStartRem);
  RegisterToken(EndToken, FTokenIDGenerator, ttEndRem);
  Inc(FTokenIDGenerator);
end;}

procedure TGenericLexer.RegisterToken(const Token: string; aTokenID: Integer; aTokenType: TTokenType; const TokenCaption: string);
var
  i, j, c, c2: Integer;
  pToken, pt: PCharToken;
  ch: Char;
  s: string;
begin
  c := Length(Token);
  {$IFDEF DEBUG}
  if c = 0 then raise Exception.Create('Token must be assigned');
  {$ENDIF}
  pToken := @FTokens[UpCase(Token[1])];
  if aTokenType = ttEndRem then
    Include(pToken.Flags, tfEndBlockRem);
  for i := 2 to c do begin
    ch := UpCase(Token[i]);
    pt := nil;
    with pToken^ do begin
      c2 := Length(ChildTokens);
      for j := 0 to c2 - 1 do
        if ChildTokens[j].TokenChar = ch then begin
          pt := @ChildTokens[j];
          Break;
        end;
      if not Assigned(pt) then begin
        SetLength(ChildTokens, c2 + 1);
        pt := @ChildTokens[c2];
        pt.TokenChar := ch;
        pt.Flags := FTokens[ch].Flags;
        if aTokenType = ttEndRem then
          Include(pt.Flags, tfEndBlockRem);
      end;
    end;
    pToken := pt;
  end;
  pToken.TokenType := aTokenType;
  pToken.TokenID := aTokenID;
  if TokenCaption <> '' then
    s := TokenCaption
  else
    s := Token;
  FTokenCaptions.AddObject(s, TObject(aTokenID));
  // Добавляем LowerCase
  ch := Char(FUpCase[Token[1]]);
  FTokens[AnsiLowerCase(ch)[1]] := FTokens[ch];
end;

procedure TGenericLexer.SaveState(out State: TParserPosition);
begin
  State.SourcePosition := FSrcPos;
  State.Row := FRow;
  State.Col := FSrcPos - FLastEnterPos;
  State.LastEnterPos := FLastEnterPos;
  State.TokenID  := FCurrentTokenID;
  State.OriginalToken := FCurrentToken;
end;

procedure TGenericLexer.LoadState(const State: TParserPosition);
begin
  FSrcPos := State.SourcePosition;
  FRow := State.Row;
  FLastEnterPos := State.LastEnterPos;
  FCurrentTokenID := State.TokenID;
  FCurrentToken := State.OriginalToken;
end;

procedure TGenericLexer.SetOmitted(const Value: string);
var
  i: Integer;
  c: Char;
begin
  for i := 1 to Length(Value) do begin
    c := Value[i];
    {$IFDEF DEBUG}
    if c > MaxTokenChar then
      raise Exception.CreateFmt('Token  = "%s" is out of range', [c]);
    {$ENDIF}
    with FTokens[UpCase(c)] do begin
      TokenChar := Value[i];
      TokenType := ttOmited;
    end;
  end;
  FOmitted := Value;
end;

procedure TGenericLexer.SetSeparators(const Value: string);
var
  i: Integer;
  c: Char;
begin
  for i := 1 to Length(Value) do begin
    c := Value[i];
    {$IFDEF DEBUG}
    if c > MaxTokenChar then
      raise Exception.CreateFmt('Token  = "%s" is out of range', [c]);
    {$ENDIF}
    Include(FTokens[c].Flags, tfSeparator);
  end;
  FSeparators := Value;
end;

procedure TGenericLexer.SetSource(const Value: string);
begin
  FLength := Length(Value);
  FSource := Value;
  First;
end;

function TGenericLexer.TokenLexem(TokenID: Integer): string;
var
  i: Integer;
begin
  i := FTokenCaptions.IndexOfObject(TObject(TokenID));
  if i <> -1 then
    Result := FTokenCaptions[i]
  else
    Result := '';
end;

function TGenericLexer.GetLinePosition: TTextPosition;
begin
  Result.Row := FRow;
  Result.Col := -1;
end;

procedure TGenericLexer.GetIdentifier(var ID: TIdentifier);
begin
  ID.Name := FCurrentToken;
  ID.TextPosition.Row := FRow;
  ID.TextPosition.Col := FSrcPos - FLastEnterPos;
end;

function TGenericLexer.GetPosition: TTextPosition;
begin
  Result.Row := FRow;
  Result.Col := FSrcPos - FLastEnterPos;
end;

function TGenericLexer.GetSubString(StartPos, EndPos: Integer): string;
begin
  Result := Copy(FSource, StartPos, EndPos - StartPos);
end;

procedure TGenericLexer.GetTokenAsIdentifier(var ID: TIdentifier);
begin
  ID.Name := TokenLexem(FCurrentTokenID);
  ID.TextPosition.Row := FRow;
  ID.TextPosition.Col := FSrcPos - FLastEnterPos;
end;

function TGenericLexer.GetTokenName: string;
begin
  if FCurrentTokenID = FIdentifireID then
    Result := FCurrentToken
  else
    Result := TokenLexem(FCurrentTokenID);
end;

procedure TGenericLexer.ParseCharCode(SPos: Integer);
type
  TNumberSymbols = set of (nsExponent, nsPoint, nsSign);
var
  ReadedChars: integer;
  Ch: Char;
  NumberSymbols: TNumberSymbols;
  CToken: PCharToken;
begin
  NumberSymbols := [];
  ReadedChars := 0;
  Inc(SPos);
  Inc(ReadedChars);
  while SPos <= FLength do
  begin
    Ch := Char(FUpCase[FSource[SPos]]);
    CToken := addr(Tokens[Ch]);
    if (CToken.TokenType <> ttCharCode) and
       (CToken.TokenType <> ttDigit) then Break;
    Inc(SPos);
    Inc(ReadedChars);
  end;
  // read identifier:
  FCurrentToken := Copy(FSource, SPos - ReadedChars, ReadedChars);
  FIdentifireType := itCharCodes;
  FCurrentTokenID := FIdentifireID;
  FSrcPos := SPos;
end;

procedure TGenericLexer.ParseCharCode(SPos: Integer; out Chars: string);
type
  TNumberSymbols = set of (nsExponent, nsPoint, nsSign);
var
  ReadedChars: integer;
  Ch: Char;
  NumberSymbols: TNumberSymbols;
  CToken: PCharToken;
begin
  Chars := '';
  NumberSymbols := [];
  ReadedChars := 0;
  Inc(SPos);
  while SPos <= FLength do
  begin
    Ch := Char(FUpCase[FSource[SPos]]);
    CToken := addr(Tokens[Ch]);
    if (CToken.TokenType <> ttCharCode) and
       (CToken.TokenType <> ttDigit) then Break;
    Inc(SPos);
    Inc(ReadedChars);
  end;
  // read identifier:
  Chars := Copy(FSource, SPos - ReadedChars, ReadedChars);
  FSrcPos := SPos;
end;

{ TTextPosition }

constructor TTextPosition.Create(Row: Integer);
begin
  Self.Row := Row;
  Self.Col := 0;
end;

constructor TTextPosition.Create(Row, Col: Integer);
begin
  Self.Row := Row;
  Self.Col := Col;
end;

end.
