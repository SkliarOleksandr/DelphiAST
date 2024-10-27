unit AST.Lexer.Delphi;

interface

{$I AST.Parser.Defines.inc}

uses AST.Lexer, SysUtils, StrUtils, Types, Classes, AST.Parser.Errors;

type

  TTokenID = (
    token_unknown {=0},             // unknown token
    token_eof,                      // end of file
    token_identifier,               // some id

    token_numbersign,               // #
    token_semicolon,                // ;
    token_colon,                    // :

    token_assign,                   // :=
    token_equal,                    // =
    token_above,                    // >
    token_aboveorequal {=10},       // >=
    token_less,                     // <
    token_lessorequal,              // <=
    token_notequal,                 // <>
    token_period,                   // ..

    token_exclamation,              // !
    token_question,                 // ?

    token_plus,                     // +
    token_minus,                    // -
    token_asterisk,                 // *
    token_slash  {=20},             // /
    token_caret,                    // ^
    token_address,                  // @
    token_ampersand,                // &


    token_coma,                     // ,
    token_dot,                      // .
    token_openround,                // (
    token_closeround,               // )
    token_openblock,                // [
    token_closeblock,               // ]
    token_openfigure  {=30},        // {
    token_closefigure,              // }
    token_quote,                    // '

    token_openround_asteriks,       // (*
    token_closeround_asteriks,      // *)

    token_at,                       // keyword: at
    tokenD_absolute,                // directive: absolute
    tokenD_abstract,                // directive: abstract
    tokenD_aling,                   // directive: align
    token_asm,                      // keyword: asm
    tokenD_stdcall  {=40},          // directive: stdcall
    tokenD_fastcall,                // directive: fastcall
    tokenD_cdecl,                   // directive: cdecl
    tokenD_safecall,                // directive: safecall
    token_unit,                     // keyword: unit
    token_uses,                     // keyword: uses

    token_program,                  // keyword: program
    token_library,                  // keyword: library

    tokenD_export,                  // directive: export
    tokenD_exports,                 // directive: exports
    tokenD_external,                // directive: extern
    tokenD_name  {=50},             // directive: name
    token_interface,                // keyword: interface
    token_dispinterface,            // keyword: dispinterface
    tokenD_dispid,                  // directive: dispid
    token_implementation,           // keyword: implementation
    token_implement,                // keyword: implementation
    token_initialization,           // keyword: initialization
    token_finalization,             // keyword: finalization
    token_begin,                    // keyword: begin
    token_end,                      // keyword: end
    token_var,                      // keyword: var
    tokenD_out,                     // directive: out
    token_const,                    // keyword: const
    token_procedure,                // keyword: procedure
    token_function,                 // keyword: function
    tokenD_overload,                // directive: overload
    tokenD_override,                // directive: override
    tokenD_message,                 // directive: message
    token_type,                     // keyword: type
    token_class,                    // keyword: class
    token_record,                   // keyword: record
    token_packed,                   // keyword: packed
    tokenD_package,                 // directive: package
    token_set,                      // keyword: set
    tokenD_sealed,                  // directive: sealed
    token_array,                    // keyword: array
    token_if,                       // keyword: if
    token_in,                       // keyword: in
    tokenD_index,                   // directive: index
    token_inline,                   // keyword: inline
    token_is,                       // keyword: is
    token_then,                     // keyword: if
    token_else,                     // keyword: else
    tokenD_forward,                 // directive: forward
    tokenD_helper,                  // directive: helper
    tokenD_stored,                  // directive: stored

    token_not,                      // keyword: not
    token_and,                      // keyword: and
    token_or,                       // keyword: or
    token_xor,                      // keyword: xor

    token_div,                      // keyword: div
    token_mod,                      // keyword: mod
    token_shl,                      // keyword: shl
    token_shr,                      // keyword: shr

    token_as,                       // keyword: as
    token_for,                      // keyword: for
    token_to,                       // keyword: to
    token_downto,                   // keyword: downto
    token_do,                       // keyword: do
    token_deprecated,               // keyword: depricated
    token_while,                    // keyword: while
    tokenD_reference,               // directive: reference
    token_repeat,                   // keyword: repeat
    tokenD_reintroduce,             // directive: reintroduce
    token_until,                    // keyword: until
    token_with,                     // keyword: until
    token_case,                     // keyword: case
    token_of,                       // keyword: of
    tokenD_on,                      // directive: on
    token_object,                   // keyword: object
    tokenD_operator,                // directive: operator
    token_try,                      // keyword: try
    tokenD_final,                   // directive: final
    token_finally,                  // keyword: finally
    token_except,                   // keyword: except
    token_raise,                    // keyword: raise
    tokenD_strict,                  // directive: strict
    token_property,                 // keyword: property
    tokenD_private,                 // directive: private
    tokenD_protected,               // directive: protected
    tokenD_public,                  // directive: public
    tokenD_published,               // directive: published
    tokenD_platform,                // keyword: platform
    tokenD_read,                    // keyword: read
    tokenD_readonly,                // keyword: readonly
    tokenD_write,                   // keyword: write
    tokenD_writeonly,               // keyword: writeonly
    token_inherited,                // keyword: inherited
    tokenD_virtual,                 // directive: virtual
    tokenD_dynamic,                 // directive: dynamic
    tokenD_delayed,                 // directive: delayed
    tokenD_static,                  // directive: static
    token_constructor,              // keyword: constructor
    token_destructor,               // keyword: destructor
    tokenD_default,                 // directive: default
    tokenD_nodefault,               // directive: nodefault
    tokenD_varargs,                 // directive: varargs
    token_threadvar,                // keyword: threadvar
    token_label,                    // keyword: label
    token_goto,                     // keyword: goto
    token_resourcestring,           // keyword: resourcestring

    token_cond_define,              // {$DEFINE...
    token_cond_else,                // {$ELSE...
    token_cond_else_if,             // {$ELSEIF (condition)}
    token_cond_end,                 // {$END... / {$IFEND... / {$ENDIF...
    token_cond_include,             // {$INCLUDE
    token_cond_undefine,            // {$UNDEFINE
    token_cond_ifdef,               // {$IFDEF
    token_cond_ifndef,              // {$IFNDEF
    token_cond_if,                  // {$IF...
    token_cond_ifopt,               // {$IFOPT...
    token_cond_message,             // {$MESSAGE...
    token_cond_any
  );


  TDelphiLexer = class(TGenericLexer)
  private
    fOriginalToken: string;
  protected
    procedure ParseChainedString;
    procedure ParseCharCodeSymbol;
  public
    constructor Create(const Source: string); override;
    function NextToken: TTokenID;
    function TokenLexem(TokenID: TTokenID): string;
    procedure RegisterToken(const Token: string; TokenID: TTokenID; const TokenCaption:
                            string; TokenType: TTokenType = ttToken); overload;
    procedure RegisterToken(const Token: string; TokenID: TTokenID;
                            Priority: TTokenClass = tcStrongKeyword); overload;
    procedure ReadCurrIdentifier(var Identifier: TIdentifier); inline;
    procedure ReadNextIdentifier(var Identifier: TIdentifier); inline;
    procedure MatchToken(ActualToken, ExpectedToken: TTokenID); inline;
    procedure MatchNextToken(ExpectedToken: TTokenID); inline;
    function TokenCanBeID(TokenID: TTokenID): Boolean; inline;
    property OriginalToken: string read fOriginalToken;
  end;


implementation

uses AST.Delphi.Errors, AST.Parser.Utils;

{ TDelphiParser }

function TDelphiLexer.TokenCanBeID(TokenID: TTokenID): Boolean;
begin
  Result := (TokenID = token_identifier) or (CurToken.TokenClass = tcWeakKeyword);
end;

procedure TDelphiLexer.ReadCurrIdentifier(var Identifier: TIdentifier);
begin
  Identifier.Name := OriginalToken;
  Identifier.TextPosition := Position;
end;

procedure TDelphiLexer.ReadNextIdentifier(var Identifier: TIdentifier);
begin
  if NextToken = token_Identifier then begin
    Identifier.Name := OriginalToken;
    Identifier.TextPosition := Position;
  end else
    AbortWork(sIdExpectedButFoundFmt, [TokenLexem(TTokenID(CurrentTokenID))], PrevPosition);
end;

procedure TDelphiLexer.MatchToken(ActualToken, ExpectedToken: TTokenID);
begin
  if ActualToken <> ExpectedToken then
    AbortWork(sExpected, [UpperCase(TokenLexem(ExpectedToken))], PrevPosition);
end;

procedure TDelphiLexer.MatchNextToken(ExpectedToken: TTokenID);
begin
  if TTokenID(GetNextTokenId()) <> ExpectedToken then
    AbortWork(sExpected, [UpperCase(TokenLexem(ExpectedToken))], PrevPosition);
end;

function CharsToStr(const Str: string): string;
var
  Chars: TStringDynArray;
begin
  Result := '';

  Chars := SplitString(Str, '#');
  if Chars[0] = '' then
    Delete(Chars, 0, 1);
  // this is a string
  if Length(Chars) > 0 then
  begin
    SetLength(Result, Length(Chars));
    for var i := 0 to Length(Chars) - 1 do
      Result[Low(string) + i] := Char(StrToInt(Chars[i]));
  end;
end;

function TDelphiLexer.NextToken: TTokenID;
begin
  Result := TTokenID(GetNextTokenId());
  case Result of
    token_ampersand: begin
      if not GetNextCharIsSeparator then
      begin
        Result := TTokenID(GetNextTokenId());
        fOriginalToken := TokenLexem(Result);
        fCurrentToken := fOriginalToken;
        fCurrentTokenID := ord(token_identifier);
        fIdentifireType := itIdentifier;
        Result := token_identifier;
      end;
    end;
  else
    fOriginalToken := CurrentToken;
  end;
  if CurToken.TokenID = ord(token_quote) then
    ParseChainedString()
  else
  if CurToken.TokenID = ord(token_numbersign) then
  begin
    ParseCharCodeSymbol();
  end;
end;

procedure TDelphiLexer.ParseChainedString;
var
  Token: PCharToken;
begin
  while True do
  begin
    Token := GetNextToken();
    if Token.TokenID = ord(token_numbersign) then
    begin
      GetNextTokenId();
      fOriginalToken := fOriginalToken + CharsToStr(CurrentToken);
    end else
    if Token.TokenID = ord(token_quote) then
    begin
      GetNextTokenId();
      fOriginalToken := fOriginalToken + CurrentToken;
    end else
      break;
  end;
end;

procedure TDelphiLexer.ParseCharCodeSymbol;
var
  HexStr: string;
begin
  if Length(fOriginalToken) > 1 then
  begin
    fOriginalToken := CharsToStr(fOriginalToken);
    ParseChainedString();
  end else begin
    if GetNextChar() = '$' then
    begin
      GetNextTokenId();
      SetIdentifireType(TIdentifierType.itCharCodes);
      HexStr := IntToStr(HexToInt32(CurrentToken));
      fCurrentToken := fOriginalToken + HexStr;
    end;
  end;
end;

procedure TDelphiLexer.RegisterToken(const Token: string; TokenID: TTokenID; const TokenCaption: string; TokenType: TTokenType);
begin
  inherited RegisterToken(Token, Integer(TokenID), TokenType, tcStrongKeyword, TokenCaption);
end;

procedure TDelphiLexer.RegisterToken(const Token: string; TokenID: TTokenID; Priority: TTokenClass);
begin
  inherited RegisterToken(Token, Integer(TokenID), ttToken, Priority, Token);
end;

function TDelphiLexer.TokenLexem(TokenID: TTokenID): string;
begin
  Result := inherited TokenLexem(Integer(TokenID));
end;

constructor TDelphiLexer.Create(const Source: string);
begin
  inherited Create(Source);
  IdentifireID := ord(token_identifier);
  EofID := ord(token_eof);
//  AmbiguousId := ord(token_id_or_keyword);
  TokenCaptions.AddObject('end of file', TObject(token_eof));
  TokenCaptions.AddObject('identifier', TObject(token_identifier));
  SeparatorChars := '#$ '''#9#10#13'%^&*@()+-{}[]\/,.;:<>=~!?';
  RegisterToken('#', token_NumberSign, '', ttCharCode);

  RegisterToken('$', token_Unknown, '', ttHexPrefix);
  RegisterToken('%', token_Unknown, '', ttBinPrefix);
  RegisterToken(' ', token_Unknown, '', ttOmited);
  RegisterToken(#9, token_Unknown, '', ttOmited);
  RegisterToken(#10, token_Unknown, '', ttNewLine);
  RegisterToken(#13#10, token_Unknown, '', ttNewLine);
  RegisterToken(#13, token_Unknown, '', ttOmited);
  RegisterToken('''', token_quote, 'single quote', ttSingleQuote);
  RegisterToken('"', token_unknown, 'double quote', ttDoubleQuote);
  RegisterToken('//', token_unknown, '', ttOneLineRem);
  RegisterToken(';', token_semicolon, 'semicolon');
  RegisterToken(',', token_coma, 'coma');
  RegisterToken(':', token_colon, 'colon');
  RegisterToken('=', token_equal, 'equal');
  RegisterToken('>', token_above);
  RegisterToken('>=', token_aboveorequal);
  RegisterToken('<', token_less);
  RegisterToken('<=', token_lessorequal);
  RegisterToken('<>', token_notequal);
  RegisterToken('.', token_dot, 'dot', ttToken);
  RegisterToken('..', token_period, 'period');
  RegisterToken('(', token_openround, 'open round');
  RegisterToken(')', token_closeround, 'close round');
  RegisterToken('[', token_openblock);
  RegisterToken(']', token_closeblock);
  RegisterToken('{', token_openfigure);
  RegisterToken('}', token_closefigure);
  RegisterToken('+', token_plus, 'plus');
  RegisterToken('-', token_minus, 'minus');
  RegisterToken('?', token_question, 'question');
  RegisterToken('*', token_asterisk, 'asterisk');
  RegisterToken('/', token_slash);
  RegisterToken('^', token_caret);
  RegisterToken('@', token_address);
  RegisterToken('&', token_ampersand);
  RegisterToken(':=', token_assign);
  RegisterToken('at', token_at, tcWeakKeyword);
  RegisterToken('absolute', tokenD_absolute, tcWeakKeyword);
  RegisterToken('abstract', tokenD_abstract, tcWeakKeyword);
  RegisterToken('align', tokenD_aling, tcWeakKeyword);
  RegisterToken('as', token_as);
  RegisterToken('asm', token_asm);
  RegisterToken('and', token_and);
  RegisterToken('array', token_array);
  RegisterToken('begin', token_begin);
  RegisterToken('case', token_case);
  RegisterToken('cdecl', tokenD_cdecl, tcWeakKeyword);
  RegisterToken('const', token_const);
  RegisterToken('constructor', token_constructor);
  RegisterToken('class', token_class);
  RegisterToken('do', token_do);
  RegisterToken('downto', token_downto);
  RegisterToken('div', token_div);
  RegisterToken('destructor', token_destructor);
  RegisterToken('deprecated', token_deprecated);
  RegisterToken('default', tokenD_default, tcWeakKeyword);
  RegisterToken('nodefault', tokenD_nodefault, tcWeakKeyword);
  RegisterToken('dynamic', tokenD_dynamic, tcWeakKeyword);
  RegisterToken('delayed', tokenD_delayed, tcWeakKeyword);
  RegisterToken('end', token_end);
  RegisterToken('else', token_else);
  RegisterToken('except', token_except);
  RegisterToken('export', tokenD_export, tcWeakKeyword);
  RegisterToken('exports', tokenD_exports, tcWeakKeyword);
  RegisterToken('external', tokenD_external, tcWeakKeyword);
  RegisterToken('function', token_Function);
  RegisterToken('for', token_for);
  RegisterToken('forward', tokenD_forward, tcWeakKeyword);
  RegisterToken('final', tokenD_final, tcWeakKeyword);
  RegisterToken('finally', token_finally);
  RegisterToken('finalization', token_finalization);
  RegisterToken('fastcall', tokenD_fastcall, tcWeakKeyword);
  RegisterToken('goto', token_goto);
  RegisterToken('if', token_if);
  RegisterToken('is', token_is);
  RegisterToken('in', token_in);
  RegisterToken('index', tokenD_index, tcWeakKeyword);
  RegisterToken('interface', token_Interface);
  RegisterToken('dispinterface', token_dispinterface);
  RegisterToken('dispid', tokenD_dispid, tcWeakKeyword);
  RegisterToken('inherited', token_inherited);
  RegisterToken('inline', token_inline);
  RegisterToken('initialization', token_initialization);
  RegisterToken('implementation', token_Implementation);
  RegisterToken('implement', token_implement);
  RegisterToken('library', token_library);
  RegisterToken('label', token_label);
  RegisterToken('mod', token_mod);
  RegisterToken('message', tokenD_message, tcWeakKeyword);
  RegisterToken('not', token_not);
  RegisterToken('name', tokenD_name, tcWeakKeyword);
  RegisterToken('object', token_object);
  RegisterToken('of', token_of);
  RegisterToken('on', tokenD_on, tcWeakKeyword);
  RegisterToken('or', token_or);
  RegisterToken('out', tokenD_out, tcWeakKeyword);
  RegisterToken('override', tokenD_override, tcWeakKeyword);
  RegisterToken('overload', tokenD_overload, tcWeakKeyword);
  RegisterToken('operator', tokenD_operator, tcWeakKeyword);
  RegisterToken('package', tokenD_package, tcWeakKeyword);
  RegisterToken('procedure', token_procedure);
  RegisterToken('program', token_program);
  RegisterToken('property', token_property);
  RegisterToken('protected', tokenD_protected, tcWeakKeyword);
  RegisterToken('program', token_program);
  RegisterToken('private', tokenD_private, tcWeakKeyword);
  RegisterToken('public', tokenD_public, tcWeakKeyword);
  RegisterToken('published', tokenD_published, tcWeakKeyword);
  RegisterToken('packed', token_packed);
  RegisterToken('platform', tokenD_platform, tcWeakKeyword);
  RegisterToken('raise', token_raise);
  RegisterToken('read', tokenD_read, tcWeakKeyword);
  RegisterToken('readonly', tokenD_readonly, tcWeakKeyword);
  RegisterToken('record', token_record);
  RegisterToken('reference', tokenD_reference, tcWeakKeyword);
  RegisterToken('repeat', token_repeat);
  RegisterToken('resourcestring', token_resourcestring);
  RegisterToken('reintroduce', tokenD_reintroduce, tcWeakKeyword);
  RegisterToken('set', token_set);
  RegisterToken('sealed', tokenD_sealed, tcWeakKeyword);
  RegisterToken('shl', token_shl);
  RegisterToken('shr', token_shr);
  RegisterToken('static', tokenD_static, tcWeakKeyword);
  RegisterToken('strict', tokenD_strict, tcWeakKeyword);
  RegisterToken('stdcall', tokenD_stdcall, tcWeakKeyword);
  RegisterToken('safecall', tokenD_safecall, tcWeakKeyword);
  RegisterToken('stored', tokenD_stored, tcWeakKeyword);
  RegisterToken('then', token_then);
  RegisterToken('to', token_to);
  RegisterToken('try', token_try);
  RegisterToken('threadvar', token_threadvar);
  RegisterToken('type', token_type);
  RegisterToken('helper', tokenD_helper, tcWeakKeyword);
  RegisterToken('until', token_until);
  RegisterToken('unit', token_unit);
  RegisterToken('uses', token_uses);
  RegisterToken('var', token_var);
  RegisterToken('varargs', tokenD_varargs, tcWeakKeyword);
  RegisterToken('virtual', tokenD_virtual, tcWeakKeyword);
  RegisterToken('with', token_with);
  RegisterToken('while', token_while);
  RegisterToken('write', tokenD_write, tcWeakKeyword);
  RegisterToken('writeonly', tokenD_writeonly, tcWeakKeyword);
  RegisterToken('xor', token_xor);
  RegisterRemToken('{', '}', ord(token_openfigure), Ord(token_closefigure));
  RegisterRemToken('(*', '*)', ord(token_openround_asteriks), Ord(token_closeround_asteriks));

  RegisterToken('{$DEFINE', token_cond_define);
  RegisterToken('{$UNDEFINE', token_cond_undefine);
  RegisterToken('{$IFDEF', token_cond_ifdef);
  RegisterToken('{$IFNDEF', token_cond_ifndef);
  RegisterToken('{$IF', token_cond_if);
  RegisterToken('{$ELSE', token_cond_else);
  RegisterToken('{$ELSEIF', token_cond_else_if);
  RegisterToken('{$ENDIF', token_cond_end);
  RegisterToken('{$IFEND', token_cond_end);
  RegisterToken('{$IFOPT', token_cond_ifopt);
  RegisterToken('{$MESSAGE', token_cond_message);
  RegisterToken('{$INCLUDE', token_cond_include);
  RegisterToken('{$I', token_cond_include);
  RegisterToken('{$', token_cond_any);
end;

end.

