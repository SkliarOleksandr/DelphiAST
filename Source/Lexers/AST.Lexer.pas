unit AST.Lexer;

interface

{$I AST.Parser.Defines.inc}

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
    ttSingleQuote,
    ttDoubleQuote,
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

  // Token Class
  TTokenClass = (
    StrongKeyword,                  // Strong keyword
    AmbiguousPriorityKeyword,       // Can be keyword or id, returns as keyword
    AmbiguousPriorityIdentifier,    // Can be keyword or id, returns as id
    Ambiguous                       // Can be keyword or id, returns as ambiguous
  );

type

  TCharToken = record
  type
    TTokens = array of TCharToken;
  var
    TokenChar: Char;
    TokenID: Integer;
    TokenType: TTokenType;
    Flags: TTokenFlags;
    ChildTokens: TTokens;
    TokenClass: TTokenClass;
  end;
  PCharToken = ^TCharToken;

  TParseTokens = array [#0..#127] of TCharToken;
  PParseTokens = ^TParseTokens;

  TTextPosition = packed record
    Row: Integer;
    Col: Integer;
    constructor Create(Row: Integer); overload;
    constructor Create(Row, Col: Integer); overload;
    class function Empty: TTextPosition; static;
  end;

  TIdentifier = record
    Name: string;
    TextPosition: TTextPosition;
    class function Make(const Name: string): TIdentifier; overload; static; inline;
    class function Make(const Name: string; const ATextPosition: TTextPosition): TIdentifier; overload; static; inline;
    class function Combine(const Left, Right: TIdentifier): TIdentifier; static;
    class function Empty: TIdentifier; static;
  end;

  TParserPosition = record
    Source: string;
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
    fTokens: TParseTokens;             // Registered tokens
    fSource: string;                   // Source string
    fLength: Integer;                  // Length of source string
    fSrcPos: Integer;                  // Current position in the source string

    fCutToken: PCharToken;             // The current token structure
    fAmbiguousTokenId: Integer;        // The current ambiguous token Id

    fRow: Integer;                     // Номер строки текущей позиции (начинается с единицы)
    fLastEnterPos: Integer;            // Позиция последнего переноса строки (необходим для определения позиции формата (row,col))
    fPrevPostion: TTextPosition;       // Позиция предыдущего токена формата (row,col)
    fUpCase: array [#0..#127] of AnsiChar; // символы в UpCase

    fIdentifireId: Integer;            // Id for an identifier
    fEofId: Integer;                   // Id for the and of file
    fAmbiguousId: Integer;             // Id for ambiguous tokens

    fTokenCaptions: TStrings;          // Название токоенов
    fSeparators: string;
    fErrorState: TParserErrorState;
    fOmitted: string;
    fTokenIDGenerator: Integer;

  private
    function GetPosition: TTextPosition; inline;
    function GetLinePosition: TTextPosition; inline;
    procedure SetSource(const Value: string);
    procedure SetSeparators(const Value: string);
    procedure SetOmitted(const Value: string);
    function GetTokenName: string;
    function MoveNextInternal: Integer;
  protected
    fCurrentToken: string;
    fCurrentTokenId: Integer;          // The current token Id
    fIdentifireType: TIdentifierType;  // Тип идентификатора (литерал/строка/число/символ)
    property AmbiguousId: Integer read fAmbiguousId write fAmbiguousId;
    procedure RegisterToken(const Token: string; aTokenID: Integer; aTokenType: TTokenType; const TokenCaption: string = ''); overload;
    procedure RegisterToken(const Token: string; aTokenID: Integer; aTokenType: TTokenType;
                            Priority: TTokenClass; const TokenCaption: string = ''); overload;
    //procedure RegisterRemToken(const BeginToken, EndToken: string);

    procedure RegisterRemToken(const BeginToken, EndToken: string; BeginTokenID, EndTokenID: Integer);
    procedure ParseMultiLineRem(TokenID: Integer; var SPos: Integer);
    procedure ParseOneLineRem(var SPos: Integer);
    procedure ParseDidgit(SPos: Integer);
    procedure ParseCharCode(SPos: Integer); overload;
    procedure ParseQuote(Ch: Char; SPos: Integer);
    function ParseDoubleQuote(Ch: Char; SPos: Integer): Integer;
    procedure ParseUnicodeChars(SPos: Integer);
    procedure ParseBinPrefix(SPos: Integer);
    procedure ParseHexPrefix(SPos: Integer);
    procedure SetIdentifireType(const Value: TIdentifierType);
    property TokenCaptions: TStrings read fTokenCaptions;
    property EofID: Integer read fEofId write fEofId;
    property IdentifireID: Integer read fIdentifireId write fIdentifireId;
    property SeparatorChars: string read fSeparators write SetSeparators;
    property OmittedChars: string read fOmitted write SetOmitted;
    property Tokens: TParseTokens read fTokens;
    property CurToken: PCharToken read fCutToken;
    property CurrentToken: string read fCurrentToken;            // Для не идентификаторов равен пустой строке
    function GetNextToken: PCharToken;
    function GetNextChar: Char; inline;
    function GetNextTokenId: Integer;
    function GetNextCharIsSeparator: Boolean; inline;
  public
    constructor Create(const Source: string); virtual;
    destructor Destroy; override;
    ///////////////////////////////////////////////////
    procedure First;
    property CurrentTokenId: Integer read fCurrentTokenID;
    property AmbiguousTokenId: Integer read fAmbiguousTokenID;
    property ErrorState: TParserErrorState read fErrorState;
    property Source: string read fSource write SetSource;
    property SourcePosition: Integer read fSrcPos;
    property TokenName: string read GetTokenName;                 // Строковое представление токена
    property Position: TTextPosition read GetPosition;            // Позиция текущего токена формата (row,col)
    property PrevPosition: TTextPosition read fPrevPostion;
    property LinePosition: TTextPosition read GetLinePosition;
    property IdentifireType: TIdentifierType read fIdentifireType;
    function TokenLexem(TokenID: Integer): string;
    function GetSubString(StartPos, EndPos: Integer): string;
    procedure GetIdentifier(var ID: TIdentifier);
    procedure GetTokenAsIdentifier(var ID: TIdentifier);
    procedure SaveState(out State: TParserPosition);
    procedure LoadState(const State: TParserPosition);
  end;

implementation

{ TStringParser }

function TGenericLexer.GetNextChar: Char;
begin
  Result := fSource[fSrcPos];
end;

function TGenericLexer.GetNextCharIsSeparator: Boolean;
var
  NextChar: Char;
  Token: PCharToken;
begin
  Result := False;
  NextChar := GetNextChar;
  if NextChar <= MaxTokenChar then
  begin
    Token := addr(fTokens[GetNextChar]);
    Result := tfSeparator in Token.Flags;
  end;
end;

function TGenericLexer.GetNextToken: PCharToken;
var
  Ch: Char;
begin
  Ch := Char(fUpCase[fSource[fSrcPos]]);
  Result := addr(Tokens[Ch]);
end;

constructor TGenericLexer.Create(const Source: string);
var
  c: Char;
begin
  fTokenIDGenerator := 1000;
  SetSource(Source);
  fTokenCaptions := TStringList.Create;
  fTokens[MaxTokenChar].TokenType := ttUnicodeChars;
  fTokens['0'].Flags := [tfBinDigit];
  fTokens['1'].Flags := [tfBinDigit];
  for c := '0' to '9' do begin
    with fTokens[c] do begin
      TokenType := ttDigit;
      Flags := Flags + [tfDigit, tfHexDigit];
    end;
  end;
  for c := 'A' to 'F' do begin
    fTokens[c].Flags := [tfHexDigit];
  end;
  // UpCase таблица
  for c := Low(fUpCase) to High(fUpCase) do
    fUpCase[c] := AnsiChar(UpCase(c));
end;

destructor TGenericLexer.Destroy;
begin
  fTokenCaptions.Free;
  inherited;
end;

procedure TGenericLexer.First;
begin
  fSrcPos := 1;
  fRow := 1;
  fCurrentToken := '';
  fCurrentTokenID := -1;
  fLastEnterPos := 0;
end;

function TGenericLexer.MoveNextInternal: Integer;
var
  SPos, SPos2, SIdStart, ReadedChars, i, RemID: Integer;
  Ch: Char;
  Token, ptmp, ptmp2: PCharToken;
begin
  fPrevPostion := Position;
  fIdentifireType := itNone;
  Result := fEofId;
  SPos := fSrcPos;
  ReadedChars := 1;
  RemID := -1;
  SIdStart := SPos;
  while SPos <= fLength do begin
    Ch := fSource[SPos];
    if Ch < MaxTokenChar then begin
      // первые символы токенов в списке Tokens есть и в LowerCase и в UpperCase
      // поэтому приводить к UpperCase не нужно
      Token := Addr(Tokens[Ch]);
      if Assigned(Token.ChildTokens) then
      begin
        ptmp2 := Token;
        SPos2 := SPos + 1;
        while SPos2 <= fLength do begin
          // следующие символы токенов в списке Tokens только в UpperCase
          // поэтому приводим к UpperCase
          Ch := fSource[SPos2];
          if Ch > MaxTokenChar then begin
            Token := ptmp2;
            Break;
          end;
          Ch := Char(fUpCase[Ch]);
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
          if (ptmp2.TokenType <> ttNone) and ((SPos2 > fLength) or (tfSeparator in Tokens[Ch].Flags) or (tfSeparator in ptmp2.Flags)) then begin
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
        while SPos <= fLength do
        begin
          Ch := fSource[SPos];
          if (Ord(Ch) < Length(Tokens)) and (tfSeparator in Tokens[Ch].Flags) then
            Break;
          Inc(ReadedChars);
          Inc(SPos);
        end;
        // read identifier:
        fCurrentToken := Copy(fSource, SPos - ReadedChars, ReadedChars);
        fIdentifireType := itIdentifier;
        Result := fIdentifireId;
        fCurrentTokenID := Result;
        fSrcPos := SPos;
        Exit;
      end;
      ttToken: begin
        fSrcPos := SPos + 1;
        Result := Token.TokenID;
        fCurrentTokenID := Result;
        fCurrentToken := Copy(fSource, SIdStart, fSrcPos - SIdStart);
        Exit;
      end;
      ttOmited: begin
        Inc(SPos);
        SIdStart := SPos;
        Continue;
      end;
      ttUnicodeChars: begin
        ParseUnicodeChars(SPos);
        Result := fIdentifireId;
        Exit;
      end;
      ttDigit: begin
        ParseDidgit(SPos);
        Result := fIdentifireId;
        Exit;
      end;
      ttCharCode: begin
        ParseCharCode(SPos);
        Result := fIdentifireId;
        Exit;
      end;
      ttNewLine: begin
        // process NEWLINE:
        Inc(FRow);
        fLastEnterPos := SPos;
        SIdStart := SPos + 1;
      end;
      ttHexPrefix: begin
        ParseHexPrefix(SPos);
        Result := fIdentifireId;
        Exit;
      end;
      ttBinPrefix: begin
        ParseBinPrefix(SPos);
        Result := fIdentifireId;
        Exit;
      end;
      ttSingleQuote, ttDoubleQuote: begin
        ParseQuote(Ch, SPos);
        Result := fIdentifireId;
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
          Result := fIdentifireId;// !!!
          fSrcPos := SPos;
        end else begin
          Result := Token.TokenID;
          fSrcPos := SPos + 1;
        end;
        fCurrentTokenID := Result;
        {$IFDEF DEBUG} fCurrentToken := TokenLexem(Result); {$ENDIF}
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
  while (SPos <= fLength) and (tfBinDigit in Tokens[UpCase(fSource[SPos])].Flags) do begin
    Inc(SPos);
    Inc(ReadedChars);
  end;
  // read identifier:
  fCurrentToken := Copy(fSource, SPos - ReadedChars, ReadedChars);
  fIdentifireType := itBinNumber;
  fCurrentTokenID := fIdentifireId;
  fSrcPos := SPos;
end;

procedure TGenericLexer.ParseHexPrefix(SPos: Integer);
var
  ReadedChars: Integer;
begin
  Inc(SPos);
  ReadedChars := 0;
  while (SPos <= fLength) and (tfHexDigit in Tokens[UpCase(fSource[SPos])].Flags) do begin
    inc(SPos);
    Inc(ReadedChars);
  end;
  // read identifier:
  fCurrentToken := Copy(fSource, SPos - ReadedChars, ReadedChars);
  fIdentifireType := itHextNumber;
  fCurrentTokenID := fIdentifireId;
  fSrcPos := SPos;
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
  while SPos <= fLength do begin
    Ch := Char(fUpCase[fSource[SPos]]);
    if Ch = 'E' then begin
      if nsExponent in NumberSymbols then Break;
      Include(NumberSymbols, nsExponent);
    end else
    if (Ch = '.') then begin
      SPos2 := SPos + 1;
      if (nsPoint in NumberSymbols) or (SPos2 > fLength) or (not (tfDigit in Tokens[fSource[SPos2]].Flags)) then break;
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
  fCurrentToken := Copy(fSource, SPos - ReadedChars, ReadedChars);
  if (nsPoint in NumberSymbols) or (nsExponent in NumberSymbols) then
    fIdentifireType := itFloat
  else
    fIdentifireType := itInteger;

  fCurrentTokenID := fIdentifireId;
  fSrcPos := SPos;
end;

procedure TGenericLexer.ParseMultiLineRem(TokenID: Integer; var SPos: Integer);
var
  SPos2, i, RemID: integer;
  Token, ptmp: PCharToken;
  Ch: Char;
begin
  RemID := TokenID;
  while SPos < fLength do begin
    Inc(SPos);
    Ch := fSource[SPos];
    if Ch > MaxTokenChar then
      continue;
    Token := Addr(Tokens[Ch]);
    // process NEWLINE:
    if Token.TokenType = ttNewLine then begin
      Inc(FRow);
      fLastEnterPos := SPos;
    end else
    if (tfEndBlockRem in Token.Flags) then begin
      if Token.TokenType = ttEndRem then begin
         if Token.TokenID = RemID then Break // end of "one-char" block rem
      end else begin
        SPos2 := SPos + 1;
        while SPos2 < fLength do begin
          Ch := fSource[SPos2];
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
     if SPos > fLength then
       Break;

     Ch := fSource[SPos];
     if (Ord(Ch) < Length(Tokens)) and (Tokens[Ch].TokenType = ttNewLine) then
       Break;
  until False;
  // process NEWLINE:
  Inc(FRow);
  fLastEnterPos := SPos;
end;

procedure TGenericLexer.ParseQuote(Ch: Char; SPos: Integer);
var
  QuoteChar: Char;
  SPos2, ReadedChars: Integer;
begin
  QuoteChar := Ch;
  ReadedChars := 0;
  Inc(SPos);
  while SPos <= fLength do begin
    Ch := fSource[SPos];
    if (Ch = QuoteChar) then
    begin
      SPos2 := SPos + 1;
      if (SPos2 <= fLength) and (fSource[SPos2] = QuoteChar) then
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
  fCurrentToken := Copy(fSource, SPos - ReadedChars, ReadedChars);

  // replace double <''> with single <'>
  if QuoteChar = '''' then
    fCurrentToken := StringReplace(fCurrentToken, '''''', '''', [rfReplaceAll]);

  if Length(fCurrentToken) = 1 then
    fIdentifireType := itChar
  else
    fIdentifireType := itString;

  // end read;
  fCurrentTokenID := fIdentifireId;
  fSrcPos := SPos + 1;

//  Inc(SPos);
end;

function TGenericLexer.ParseDoubleQuote(Ch: Char; SPos: Integer): Integer;
var
  QuoteChar: Char;
  SignIdx, SPos2, ReadedChars, SignReadedChars, SignLen: Integer;
  Sign: string;
begin
  QuoteChar := Ch;
  SignReadedChars := 0;
  // читаем сигнатуру
  Inc(SPos);
  while SPos <= fLength do begin
    Ch := fSource[SPos];
    if (Ch = QuoteChar) then
      break;
    Inc(SPos);
    Inc(SignReadedChars);
  end;

  Sign := UpperCase(Copy(fSource, SPos - SignReadedChars, SignReadedChars)) + QuoteChar;
  SignLen := Length(Sign);

  ReadedChars := 0;
  // читаем многострочную строку
  Inc(SPos);
  while SPos <= fLength do begin
    Ch := fSource[SPos];
    if (Ch = QuoteChar) then
    begin
      SPos2 := SPos + 1;
      SignReadedChars := 0;
      while (SPos2 <= fLength) do begin
        Ch := fSource[SPos2];
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

  if SPos > fLength then
    Exit(fEofId);

  // read identifier:
  fCurrentToken := Copy(fSource, SPos - ReadedChars, ReadedChars);
  if ReadedChars = 1 then
    fIdentifireType := itChar
  else
    fIdentifireType := itString;

  // end read;
  fCurrentTokenID := fIdentifireId;
  fSrcPos := SPos + SignLen + 1;

  Result := fIdentifireId;
end;

procedure TGenericLexer.ParseUnicodeChars(SPos: Integer);
var
  ReadedChars: Integer;
  Ch: Char;
begin
  ReadedChars := 1;
  Inc(SPos);
  while SPos <= fLength do begin
    Ch := fSource[SPos];
    if (Ch < MaxTokenChar) and (tfSeparator in Tokens[Ch].Flags) then Break;
    Inc(ReadedChars);
    Inc(SPos);
  end;
  // read identifier:
  fCurrentToken := Copy(fSource, SPos - ReadedChars, ReadedChars);
  fIdentifireType := itIdentifier;
  fSrcPos := SPos;
  fCurrentTokenID := fIdentifireId;
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
begin
  RegisterToken(Token, aTokenID, aTokenType, TTokenClass.StrongKeyword, TokenCaption);
end;

procedure TGenericLexer.RegisterToken(const Token: string; aTokenID: Integer; aTokenType: TTokenType;
                                      Priority: TTokenClass; const TokenCaption: string);
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
  pToken := @fTokens[UpCase(Token[1])];
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
        pt.Flags := fTokens[ch].Flags;
        if aTokenType = ttEndRem then
          Include(pt.Flags, tfEndBlockRem);
      end;
    end;
    pToken := pt;
  end;
  pToken.TokenType := aTokenType;
  pToken.TokenID := aTokenID;
  pToken.TokenClass := Priority;
  if TokenCaption <> '' then
    s := TokenCaption
  else
    s := Token;
  fTokenCaptions.AddObject(s, TObject(aTokenID));
  // Добавляем LowerCase
  ch := Char(fUpCase[Token[1]]);
  fTokens[AnsiLowerCase(ch)[1]] := fTokens[ch];
end;

procedure TGenericLexer.SaveState(out State: TParserPosition);
begin
  State.Source := fSource;
  State.SourcePosition := fSrcPos;
  State.Row := fRow;
  State.Col := fSrcPos - fLastEnterPos;
  State.LastEnterPos := fLastEnterPos;
  State.TokenID  := fCurrentTokenID;
  State.OriginalToken := fCurrentToken;
end;

procedure TGenericLexer.LoadState(const State: TParserPosition);
begin
  fSource := State.Source;
  fSrcPos := State.SourcePosition;
  fRow := State.Row;
  fLength := Length(fSource);
  fLastEnterPos := State.LastEnterPos;
  fCurrentTokenID := State.TokenID;
  fCurrentToken := State.OriginalToken;
end;

function TGenericLexer.GetNextTokenId: Integer;
begin
  Result := MoveNextInternal();
  fAmbiguousTokenId := Result;
  case CurToken.TokenClass of
    TTokenClass.AmbiguousPriorityIdentifier: begin
      fCurrentTokenId := fIdentifireId;
      fIdentifireType := itIdentifier;
      Exit(fIdentifireId);
    end;
    TTokenClass.AmbiguousPriorityKeyword: begin
      fIdentifireType := itIdentifier;
    end;
    TTokenClass.Ambiguous: begin
      fCurrentTokenId := fAmbiguousId;
      fIdentifireType := itIdentifier;
      Exit(fAmbiguousId);
    end;
  end;
end;

procedure TGenericLexer.SetIdentifireType(const Value: TIdentifierType);
begin
  fIdentifireType := Value;
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
    with fTokens[UpCase(c)] do begin
      TokenChar := Value[i];
      TokenType := ttOmited;
    end;
  end;
  fOmitted := Value;
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
    Include(fTokens[c].Flags, tfSeparator);
  end;
  fSeparators := Value;
end;

procedure TGenericLexer.SetSource(const Value: string);
begin
  fLength := Length(Value);
  fSource := Value;
  First;
end;

function TGenericLexer.TokenLexem(TokenID: Integer): string;
var
  i: Integer;
begin
  if TokenID = fAmbiguousId then
    TokenID := fAmbiguousTokenId;
  i := fTokenCaptions.IndexOfObject(TObject(TokenID));
  if i <> -1 then
    Result := fTokenCaptions[i]
  else
    Result := '';
end;

function TGenericLexer.GetLinePosition: TTextPosition;
begin
  Result.Row := fRow;
  Result.Col := -1;
end;

procedure TGenericLexer.GetIdentifier(var ID: TIdentifier);
begin
  ID.Name := fCurrentToken;
  ID.TextPosition.Row := fRow;
  ID.TextPosition.Col := fSrcPos - fLastEnterPos;
end;

function TGenericLexer.GetPosition: TTextPosition;
begin
  Result.Row := fRow;
  Result.Col := fSrcPos - fLastEnterPos;
end;

function TGenericLexer.GetSubString(StartPos, EndPos: Integer): string;
begin
  Result := Copy(fSource, StartPos, EndPos - StartPos);
end;

procedure TGenericLexer.GetTokenAsIdentifier(var ID: TIdentifier);
begin
  ID.Name := TokenLexem(FCurrentTokenID);
  ID.TextPosition.Row := fRow;
  ID.TextPosition.Col := fSrcPos - fLastEnterPos;
end;

function TGenericLexer.GetTokenName: string;
begin
  if fCurrentTokenID = fIdentifireId then
    Result := fCurrentToken
  else
    Result := TokenLexem(FCurrentTokenID);
end;

procedure TGenericLexer.ParseCharCode(SPos: Integer);
var
  ReadedChars: integer;
  Ch: Char;
  CToken: PCharToken;
begin
  ReadedChars := 0;
  Inc(SPos);
  Inc(ReadedChars);
  while SPos <= fLength do
  begin
    Ch := Char(fUpCase[fSource[SPos]]);
    CToken := addr(Tokens[Ch]);
    if (CToken.TokenType <> ttCharCode) and
       (CToken.TokenType <> ttHexPrefix) and
       not (tfHexDigit in CToken.Flags) and
       not (tfDigit in CToken.Flags)
       then Break;
    Inc(SPos);
    Inc(ReadedChars);
  end;
  // read identifier:
  fCurrentToken := Copy(fSource, SPos - ReadedChars, ReadedChars);
  fIdentifireType := itCharCodes;
  fCurrentTokenID := fIdentifireId;
  fSrcPos := SPos;
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

class function TTextPosition.Empty: TTextPosition;
begin
  Result.Row := 0;
  Result.Col := 0;
end;

{ TIdentifier }

class function TIdentifier.Combine(const Left, Right: TIdentifier): TIdentifier;
begin
  if Left.Name <> '' then
    Result.Name := Left.Name + '.' + Right.Name
  else
    Result.Name := Right.Name;
  Result.TextPosition := Right.TextPosition;
end;

class function TIdentifier.Empty: TIdentifier;
begin
  Result.Name := '';
  Result.TextPosition := TTextPosition.Empty;
end;

class function TIdentifier.Make(const Name: string; const ATextPosition: TTextPosition): TIdentifier;
begin
  Result.Name := Name;
  Result.TextPosition := ATextPosition;
end;

class function TIdentifier.Make(const Name: string): TIdentifier;
begin
  Result.Name := Name;
  Result.TextPosition := TTextPosition.Empty;
end;

end.
