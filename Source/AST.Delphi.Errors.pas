unit AST.Delphi.Errors;

interface

uses SysUtils,
     AST.Lexer,
     AST.Lexer.Delphi,
     AST.Delphi.Operators,
     AST.Delphi.Classes,
     AST.Delphi.Parser;

type
  TASTDelphiErrors = class helper for TASTDelphiUnit
    class procedure ERROR_ARG_VAR_REQUIRED(Expr: TIDExpression); static;
    class procedure ERROR_CONST_EXPRESSION_REQUIRED(Expr: TIDExpression); static;
    class procedure ERROR_INCOMPATIBLE_TYPES(const Src, Dst: TIDExpression); overload; static;
    class procedure ERROR_INCOMPATIBLE_TYPES(const Src: TIDExpression; Dst: TIDType); overload; static;
    class procedure ERROR_INVALID_EXPLICIT_TYPECAST(const Src: TIDExpression; Dst: TIDType); static;
    class procedure ERROR_VAR_EXPRESSION_REQUIRED(Expr: TIDExpression); static;
    class procedure ERROR_VAR_OR_PROC_EXPRESSION_REQUIRED(Expr: TIDExpression); static;
    class procedure ERROR_CANNOT_MODIFY_FOR_LOOP_VARIABLE(Expr: TIDExpression); static;
    class procedure ERROR_CANNOT_MODIFY_CONSTANT(Expr: TIDExpression); static;
    class procedure ERROR_BOOLEAN_EXPRESSION_REQUIRED(Expr: TIDExpression); static;
    class procedure ERROR_ARRAY_EXPRESSION_REQUIRED(Expr: TIDExpression); static;
    class procedure ERROR_ID_REDECLARATED(Decl: TIDDeclaration); overload; static;
    class procedure ERROR_ID_REDECLARATED(const ID: TIdentifier); overload; static;
    class procedure ERROR_UNDECLARED_ID(const ID: TIdentifier); overload; static;
    class procedure ERROR_UNDECLARED_ID(const ID: TIdentifier; const GenericParams: TIDTypeList); overload; static;
    class procedure ERROR_UNDECLARED_ID(const Name: string; const TextPosition: TTextPosition); overload; static;
    class procedure ERROR_NOT_ENOUGH_ACTUAL_PARAMS(CallExpr: TIDExpression); static;
    class procedure ERROR_TOO_MANY_ACTUAL_PARAMS(CallExpr: TIDExpression); static;
    class procedure ERROR_OVERLOADED_MUST_BE_MARKED(const ID: TIdentifier); static;
    class procedure ERROR_DECL_DIFF_WITH_PREV_DECL(const ID: TIdentifier); static;
    class procedure ERROR_STRUCT_TYPE_REQUIRED(const TextPosition: TTextPosition); static;
    class procedure ERROR_ARRAY_TYPE_REQUIRED(const ID: TIdentifier; const TextPosition: TTextPosition); static;
    class procedure ERROR_INTEGER_TYPE_REQUIRED(const Pos: TTextPosition); static;
    class procedure ERROR_ORDINAL_TYPE_REQUIRED(const Pos: TTextPosition); static;
    class procedure ERROR_OBJECT_CANNOT_BE_USED_ON_PURE_PROC(const Expr: TIDExpression); static;
    class procedure ERROR_PROC_OR_PROCVAR_REQUIRED(const ID: TIdentifier); static;
    class procedure ERROR_PROC_REQUIRED(const Position: TTextPosition); static;
    class procedure ERROR_PROC_OR_TYPE_REQUIRED(const ID: TIdentifier); static;
    class procedure ERROR_CANNOT_ACCESS_TO_WRITEONLY_PROPERTY(const Expr: TIDExpression); static;
    class procedure ERROR_CANNOT_MODIFY_READONLY_PROPERTY(const Expr: TIDExpression); static;
    class procedure ERROR_OVERLOAD(CallExpr: TIDExpression); static;
    class procedure ERROR_AMBIGUOUS_OVERLOAD_CALL(CallExpr: TIDExpression); static;
    class procedure ERROR_INCOMPLETE_PROC(Decl: TIDDeclaration); static;
    class procedure ERROR_NO_OVERLOAD_OPERATOR_FOR_TYPES(Op: TOperatorID; Left, Right: TIDExpression); overload; static;
    class procedure ERROR_NO_OVERLOAD_OPERATOR_FOR_TYPES(Op: TOperatorID; Right: TIDExpression); overload;
    class procedure ERROR_UNIT_NOT_FOUND(const ID: TIdentifier); static;
    class procedure ERROR_UNIT_RECURSIVELY_USES_ITSELF(const ID: TIdentifier); static;
    class procedure ERROR_SETTER_MUST_BE_SUCH(const Setter: TIDProcedure; const DeclString: string); static;
    class procedure ERROR_GETTER_MUST_BE_SUCH(const Getter: TIDProcedure; const DeclString: string); static;
    class procedure ERROR_DIVISION_BY_ZERO(Expr: TIDExpression); static;
    class procedure ERROR_CONST_VALUE_OVERFLOW(Expr: TIDExpression; DstDataType: TIDType); static;
    class procedure ERROR_CLASS_TYPE_REQUIRED(const TextPosition: TTextPosition); static;
    class procedure ERROR_TYPE_REQUIRED(const TextPosition: TTextPosition); static;
    class procedure ERROR_CLASS_OR_INTF_TYPE_REQUIRED(const TextPosition: TTextPosition); static;
    class procedure ERROR_CLASS_NOT_IMPLEMENT_INTF(const Src: TIDExpression; Dest: TIDType); static;
    class procedure ERROR_METHOD_NOT_DECLARED_IN_CLASS(const ID: TIdentifier; Struct: TIDStructure); static;
    class procedure ERROR_INPLACEVAR_ALLOWED_ONLY_FOR_OUT_PARAMS(Variable: TIDVariable);
    class procedure ERROR_CANNOT_ACCESS_PRIVATE_MEMBER(const ID: TIdentifier); static;
    class procedure ERROR_REF_PARAM_MUST_BE_IDENTICAL(Expr: TIDExpression);
    class procedure ERROR_UNKNOWN_OPTION(const ID: TIdentifier);
    class procedure ERROR_CANNOT_ASSIGN_TEMPORARRY_OBJECT(Expr: TIDExpression);
    class procedure ERROR_INTF_TYPE_REQUIRED(Expr: TIDExpression);
    class procedure ERROR_INTF_ALREADY_IMPLEMENTED(Expr: TIDExpression);
    class procedure ERROR_INTF_METHOD_NOT_IMPLEMENTED(ClassType: TIDClass; Proc: TIDProcedure);
    class procedure ERROR_RECORD_STATIC_CONSTRUCTOR_ALREADY_EXIST(const Proc: TIDProcedure);
    class procedure ERROR_RECORD_STATIC_DESTRUCTOR_ALREADY_EXIST(const Proc: TIDProcedure);
    class procedure ERROR_CTOR_DTOR_MUST_BE_DECLARED_IN_STRUCT(const Position: TTextPosition);
    class procedure ERROR_OPERATOR_MUST_BE_DECLARED_IN_STRUCT(const Position: TTextPosition);
    class procedure ERROR_CANNOT_ASSIGN_NULL_TO_NOTNULL(const Src: TIDExpression);
    class procedure ERROR_ORDINAL_OR_SET_REQUIRED(const Src: TIDExpression);
    class procedure ERROR_REFERENCE_TYPE_EXPECTED(const Expr: TIDExpression);
    class procedure ERROR_STRING_CONST_IS_NOT_ANSI(const Src: TIDExpression);
    class procedure ERROR_VAR_IS_NOT_INITIALIZED(const Variable: TIDExpression);
    procedure ERROR_NO_METHOD_IN_BASE_CLASS(Proc: TIDProcedure);
    procedure ERROR_DEFAULT_PROP_MUST_BE_ARRAY_PROP;
    procedure ERROR_DEFAULT_PROP_ALREADY_EXIST(Prop: TIDProperty);
    procedure ERROR_IMPORT_FUNCTION_CANNOT_BE_INLINE;
    procedure ERROR_INVALID_TYPE_DECLARATION;
    procedure ERROR_EXPECTED_TOKEN(Token: TTokenID; ActulToken: TTokenID = token_unknown);
    procedure ERROR_EXPECTED_KEYWORD_OR_ID;
    procedure ERROR_IDENTIFIER_EXPECTED(ActualToken: TTokenID); overload;
    procedure ERROR_IDENTIFIER_EXPECTED; overload;
    procedure ERROR_PARAM_NAME_ID_EXPECTED(ActualToken: TTokenID);
    procedure ERROR_SEMICOLON_EXPECTED;
    procedure ERROR_EMPTY_EXPRESSION;
    procedure ERROR_END_OF_FILE;
    procedure ERROR_EXPRESSION_EXPECTED;
    procedure ERROR_FEATURE_NOT_SUPPORTED;
    procedure ERROR_INTF_SECTION_MISSING;
    procedure ERROR_KEYWORD_EXPECTED;
    procedure ERROR_BEGIN_KEYWORD_EXPECTED;
    procedure ERROR_DUPLICATE_SPECIFICATION(Spec: TProcSpecification);
    procedure ERROR_UNSUPPORTED_OPERATOR(Op: TOperatorID);
    procedure ERROR_TRY_KEYWORD_MISSED;
    procedure ERROR_EXPORT_ALLOWS_ONLY_IN_INTF_SECTION;
    procedure ERROR_NEED_SPECIFY_NINDEXES(const Decl: TIDDeclaration);
    procedure ERROR_IDENTIFIER_HAS_NO_MEMBERS(const Decl: TIDDeclaration);
    procedure ERROR_INVALID_COND_DIRECTIVE;
    procedure ERROR_INCOMPLETE_STATEMENT;
    procedure ERROR_PROC_OR_PROP_OR_VAR_REQUIRED;
    procedure HINT_TEXT_AFTER_END;
    procedure ERROR_PARAM_TYPE_REQUIRED;
    procedure ERROR_INHERITED_ALLOWED_ONLY_IN_OVERRIDE_METHODS;
    procedure ERROR_UNCLOSED_OPEN_BLOCK;
    procedure ERROR_UNNECESSARY_CLOSED_BLOCK;
    procedure ERROR_UNNECESSARY_CLOSED_ROUND;
    procedure ERROR_INIT_SECTION_ALREADY_DEFINED;
    procedure ERROR_FINAL_SECTION_ALREADY_DEFINED;
    procedure ERROR_ORDINAL_CONST_OR_TYPE_REQURED;
    procedure ERROR_DESTRUCTOR_CANNOT_BE_CALL_DIRECTLY;
    procedure ERROR_VIRTUAL_ALLOWED_ONLY_IN_CLASSES;
    procedure ERROR_WEAKREF_CANNOT_BE_DECLARED_FOR_TYPE(TypeDecl: TIDType);
    procedure ERROR_TYPE_IS_NOT_AN_ANCESTOR_FOR_THIS_TYPE(TypeDecl, ChildType: TIDType);
    procedure ERROR_INVALID_HEX_CONSTANT;
    procedure ERROR_INVALID_BIN_CONSTANT;
    procedure ERROR_INTERNAL(const Message: string = 'GENERAL ERROR');
    procedure HINT_RESULT_EXPR_IS_NOT_USED(const Expr: TIDExpression);
    //procedure HINT_TYPE_DELETE_UNUSED(Decl: TIDDeclaration);
    //procedure HINT_PROC_DELETE_UNUSED(Decl: TIDDeclaration);
  end;

implementation

uses NPCompiler.Utils,
     AST.Parser.Errors,
     NPCompiler.Messages,
     NPCompiler.Intf,
     NPCompiler.Errors;

procedure TASTDelphiErrors.ERROR_INCOMPLETE_STATEMENT;
begin
  AbortWork(sIncompleteStatement, Lexer.Position);
end;

procedure TASTDelphiErrors.ERROR_INHERITED_ALLOWED_ONLY_IN_OVERRIDE_METHODS;
begin
  AbortWork('INHERITED calls allowed only in OVERRIDE methods', Lexer.PrevPosition);
end;

procedure TASTDelphiErrors.ERROR_INIT_SECTION_ALREADY_DEFINED;
begin
  AbortWork('INITIALIZATION section is already defined', Lexer.Position);
end;

class procedure TASTDelphiErrors.ERROR_INPLACEVAR_ALLOWED_ONLY_FOR_OUT_PARAMS(Variable: TIDVariable);
begin
  AbortWork('Inpalce VAR declaration allowed only for OUT parameters', Variable.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_INCOMPATIBLE_TYPES(const Src, Dst: TIDExpression);
var
  SrcName, DstName: string;
begin
  if Src.ItemType <> itType then SrcName := Src.DataTypeName else SrcName := Src.DisplayName;
  if Dst.ItemType <> itType then DstName := Dst.DataTypeName else DstName := Dst.DisplayName;
  AbortWork(sIncompatibleTypesFmt, [SrcName, DstName], Src.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_INCOMPATIBLE_TYPES(const Src: TIDExpression; Dst: TIDType);
begin
  AbortWork(sIncompatibleTypesFmt, [Src.DataTypeName, Dst.DisplayName], Src.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_CONST_EXPRESSION_REQUIRED(Expr: TIDExpression);
begin
  AbortWork(sExpressionMustBeConstant, Expr.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_CONST_VALUE_OVERFLOW(Expr: TIDExpression; DstDataType: TIDType);
begin
  AbortWork(errConstValueOverflow, [Expr.DisplayName, DstDataType.DisplayName], Expr.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_CTOR_DTOR_MUST_BE_DECLARED_IN_STRUCT(const Position: TTextPosition);
begin
  AbortWork('CONSTRUCTOR or DESTRUCTOR must be declared only in a CLASS or RECORD type', Position);
end;

class procedure TASTDelphiErrors.ERROR_OPERATOR_MUST_BE_DECLARED_IN_STRUCT(const Position: TTextPosition);
begin
  AbortWork('OPERATOR must be declared only in a CLASS or RECORD type', Position);
end;

class procedure TASTDelphiErrors.ERROR_VAR_EXPRESSION_REQUIRED(Expr: TIDExpression);
begin
  AbortWork(sVariableRequired, Expr.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_VAR_IS_NOT_INITIALIZED(const Variable: TIDExpression);
begin
  // пока еще не доделано
  // AbortWork('Variable "%s" is not initialized', [Variable.DisplayName], Variable.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_VAR_OR_PROC_EXPRESSION_REQUIRED(Expr: TIDExpression);
begin
  AbortWork(sVarOrProcRequired, Expr.TextPosition);
end;

procedure TASTDelphiErrors.ERROR_VIRTUAL_ALLOWED_ONLY_IN_CLASSES;
begin
  AbortWork('VIRTUAL allowed only for class methods', Lexer_PrevPosition);
end;

procedure TASTDelphiErrors.ERROR_WEAKREF_CANNOT_BE_DECLARED_FOR_TYPE(TypeDecl: TIDType);
begin
  AbortWork('The WEAK reference can not be declared for type: %s', [TypeDecl.DisplayName], Lexer_PrevPosition);
end;

class procedure TASTDelphiErrors.ERROR_ARG_VAR_REQUIRED(Expr: TIDExpression);
begin
  AbortWork(sConstCannotBePassedAsVarParam, Expr.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_CANNOT_MODIFY_CONSTANT(Expr: TIDExpression);
begin
  AbortWork(sCannotModifyObjectPassedAsConstParam, Expr.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_CANNOT_MODIFY_FOR_LOOP_VARIABLE(Expr: TIDExpression);
begin
  AbortWork(sCannotModifyForLoopIndexVarFmt, [Expr.DisplayName], Expr.TextPosition);
end;

procedure TASTDelphiErrors.ERROR_BEGIN_KEYWORD_EXPECTED;
begin
  AbortWork('BEGIN expected', Lexer_PrevPosition);
end;

class procedure TASTDelphiErrors.ERROR_BOOLEAN_EXPRESSION_REQUIRED(Expr: TIDExpression);
begin
  AbortWork(sExpressionMustBeBoolean, Expr.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_NOT_ENOUGH_ACTUAL_PARAMS(CallExpr: TIDExpression);
begin
  AbortWork(sNotEnoughActualParametersFmt, [CallExpr.DisplayName], CallExpr.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_OVERLOADED_MUST_BE_MARKED(const ID: TIdentifier);
begin
  AbortWork(sOverloadedMustBeMarked, [ID.Name], ID.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_DECL_DIFF_WITH_PREV_DECL(const ID: TIdentifier);
begin
  AbortWork(sDeclDifWithPrevDecl, [ID.Name], ID.TextPosition);
end;

procedure TASTDelphiErrors.ERROR_DEFAULT_PROP_ALREADY_EXIST(Prop: TIDProperty);
begin
  AbortWork(errorDefaultPropertyAlreadyExistsFmt, [Prop.Name], Lexer.Position);
end;

procedure TASTDelphiErrors.ERROR_DEFAULT_PROP_MUST_BE_ARRAY_PROP;
begin
  AbortWork(errorDefaultPropertyMustBeAnArrayProperty, Lexer.Position);
end;

procedure TASTDelphiErrors.ERROR_DESTRUCTOR_CANNOT_BE_CALL_DIRECTLY;
begin
  AbortWork('DESTRUCTOR cannot be call directly', Lexer.PrevPosition);
end;

class procedure TASTDelphiErrors.ERROR_DIVISION_BY_ZERO(Expr: TIDExpression);
begin
  AbortWork(sDevisionByZero, Expr.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_TOO_MANY_ACTUAL_PARAMS(CallExpr: TIDExpression);
begin
  AbortWork(sTooManyActualParameters, CallExpr.TextPosition);
end;

procedure TASTDelphiErrors.ERROR_SEMICOLON_EXPECTED;
begin
  AbortWork(sSemicolonExpected, Lexer.PrevPosition);
end;

procedure TASTDelphiErrors.ERROR_EXPECTED_KEYWORD_OR_ID;
begin
  AbortWork(sExpectedButFoundFmt, ['Identifier or keyword', Lexer.TokenName], Lexer_Position);
end;

procedure TASTDelphiErrors.ERROR_EXPECTED_TOKEN(Token: TTokenID; ActulToken: TTokenID = token_unknown);
begin
  if ActulToken = token_unknown then
    AbortWork(sExpected, ['Token "' + UpperCase(Lexer.TokenLexem(Token)) + '"'], Lexer.PrevPosition)
  else
    AbortWork(sExpectedButFoundFmt, ['Token "' + UpperCase(Lexer.TokenLexem(Token)) + '"',
                                                 UpperCase(Lexer.TokenLexem(ActulToken))], Lexer.PrevPosition);
end;

procedure TASTDelphiErrors.ERROR_IDENTIFIER_EXPECTED(ActualToken: TTokenID);
begin
  AbortWork(sIdExpectedButFoundFmt, [Lexer.TokenLexem(ActualToken)], Lexer.PrevPosition);
end;

procedure TASTDelphiErrors.ERROR_PARAM_NAME_ID_EXPECTED(ActualToken: TTokenID);
begin
  AbortWork(sIdExpectedButFoundFmt, [Lexer.TokenLexem(ActualToken)], Lexer.PrevPosition);
end;

procedure TASTDelphiErrors.ERROR_PARAM_TYPE_REQUIRED;
begin
  AbortWork(errParameterTypeRequred, Lexer.PrevPosition);
end;

procedure TASTDelphiErrors.ERROR_IDENTIFIER_EXPECTED;
begin
  AbortWork(sIdentifierExpected, Lexer.PrevPosition);
end;

procedure TASTDelphiErrors.ERROR_EMPTY_EXPRESSION;
begin
  AbortWork(sEmptyExpression, Lexer.PrevPosition);
end;

procedure TASTDelphiErrors.ERROR_EXPRESSION_EXPECTED;
begin
  AbortWork(sExpressionExpected, Lexer.PrevPosition);
end;

procedure TASTDelphiErrors.ERROR_END_OF_FILE;
begin
  AbortWork(sUnexpectedEndOfFile, Lexer.PrevPosition);
end;

procedure TASTDelphiErrors.ERROR_FEATURE_NOT_SUPPORTED;
begin
  AbortWork(sFeatureNotSupported, Lexer.Position);
end;

procedure TASTDelphiErrors.ERROR_FINAL_SECTION_ALREADY_DEFINED;
begin
  AbortWork('FINALIZATION section is already defined', Lexer.Position);
end;

procedure TASTDelphiErrors.ERROR_INTERNAL(const Message: string);
begin
  raise ECompilerInternalError.Create('Internal error: ' + Message, Lexer_Position);
end;

class procedure TASTDelphiErrors.ERROR_INTF_ALREADY_IMPLEMENTED(Expr: TIDExpression);
begin
  AbortWork('INTERFACE "%s" is already implemented', [Expr.DisplayName], Expr.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_INTF_METHOD_NOT_IMPLEMENTED(ClassType: TIDClass; Proc: TIDProcedure);
begin
  AbortWork('Interface method "%s" is not implemented in class "%s"', [Proc.DisplayName, ClassType.DisplayName], ClassType.TextPosition);
end;

procedure TASTDelphiErrors.ERROR_INTF_SECTION_MISSING;
begin
  AbortWork(sIntfSectionMissing, Lexer.Position);
end;

class procedure TASTDelphiErrors.ERROR_INTF_TYPE_REQUIRED(Expr: TIDExpression);
begin
  AbortWork('INTERFACE type required', Expr.TextPosition);
end;

procedure TASTDelphiErrors.ERROR_INVALID_BIN_CONSTANT;
begin
  AbortWork('Invalid BIN constant', Lexer_Position);
end;

procedure TASTDelphiErrors.ERROR_INVALID_COND_DIRECTIVE;
begin
  AbortWork(sInvalidConditionalStatement, Lexer_Position);
end;

procedure TASTDelphiErrors.ERROR_INVALID_HEX_CONSTANT;
begin
  AbortWork('Invalid HEX constant', Lexer_Position);
end;

class procedure TASTDelphiErrors.ERROR_INVALID_EXPLICIT_TYPECAST(const Src: TIDExpression; Dst: TIDType);
begin
  AbortWork(sInvalidTypecast, [Src.DataTypeName, Dst.DisplayName], Src.TextPosition);
end;

procedure TASTDelphiErrors.ERROR_INVALID_TYPE_DECLARATION;
begin
  AbortWork(errInvalidTypeDeclaration, Lexer.PrevPosition);
end;

class procedure TASTDelphiErrors.ERROR_ID_REDECLARATED(Decl: TIDDeclaration);
begin
  AbortWork(sIdentifierRedeclared, [Decl.DisplayName], Decl.SourcePosition);
end;

class procedure TASTDelphiErrors.ERROR_ID_REDECLARATED(const ID: TIdentifier);
begin
  AbortWork(sIdentifierRedeclared, [ID.Name], ID.TextPosition);
end;

procedure TASTDelphiErrors.ERROR_IMPORT_FUNCTION_CANNOT_BE_INLINE;
begin
  AbortWork(sImportFuncCannotBeInline, Lexer_Position);
end;

procedure TASTDelphiErrors.ERROR_UNCLOSED_OPEN_BLOCK;
begin
  AbortWork(sUnclosedOpenBlock, Lexer.PrevPosition);
end;

class procedure TASTDelphiErrors.ERROR_UNDECLARED_ID(const ID: TIdentifier);
begin
  AbortWork(sUndeclaredIdentifier, [ID.Name], ID.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_UNDECLARED_ID(const ID: TIdentifier; const GenericParams: TIDTypeList);
var
  i: integer;
  s: string;
begin
  if Assigned(GenericParams) then
  begin
    for i := 0 to Length(GenericParams) - 1 do
      s := AddStringSegment(s, GenericParams[i].Name, ', ');
    s := '<' + s + '>';
  end;
  AbortWork(sUndeclaredIdentifier, [ID.Name + s], ID.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_UNDECLARED_ID(const Name: string; const TextPosition: TTextPosition);
begin
  AbortWork(sUndeclaredIdentifier, [Name], TextPosition);
end;

procedure TASTDelphiErrors.ERROR_UNNECESSARY_CLOSED_BLOCK;
begin
  AbortWork(sUnnecessaryClosedBlock, Lexer_Position);
end;

procedure TASTDelphiErrors.ERROR_UNNECESSARY_CLOSED_ROUND;
begin
  AbortWork(sUnnecessaryClosedBracket, Lexer_Position);
end;

class procedure TASTDelphiErrors.ERROR_UNIT_NOT_FOUND(const ID: TIdentifier);
begin
  AbortWork(errUnitNotFoundFmt, [ID.Name], ID.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_UNIT_RECURSIVELY_USES_ITSELF(const ID: TIdentifier);
begin
  AbortWork(errUnitRecursivelyUsesItselfFmt, [ID.Name], ID.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_UNKNOWN_OPTION(const ID: TIdentifier);
begin
  AbortWork('Unknown option: %s', [ID.Name], ID.TextPosition);
end;

procedure TASTDelphiErrors.ERROR_KEYWORD_EXPECTED;
begin
  AbortWork(sKeywordExpected, [Lexer.OriginalToken], Lexer.Position);
end;

class procedure TASTDelphiErrors.ERROR_METHOD_NOT_DECLARED_IN_CLASS(const ID: TIdentifier; Struct: TIDStructure);
begin
  AbortWork('Method "%s" is not declared in "%s"', [ID.Name, Struct.Name], ID.TextPosition);
end;

procedure TASTDelphiErrors.ERROR_DUPLICATE_SPECIFICATION(Spec: TProcSpecification);
var
  SpecName: string;
begin
  case Spec of
    PS_FORWARD: SpecName := 'FORWARD';
    PS_INLINE: SpecName := 'INLINE';
    PS_PURE: SpecName := 'PURE';
    PS_NORETURN: SpecName := 'NORETURN';
    PS_NOEXCEPT: SpecName := 'NOEXCEPT';
    PS_OVELOAD: SpecName := 'OVERLOAD';
    PS_EXPORT: SpecName := 'EXPORT';
    PS_IMPORT: SpecName := 'EXTERNAL';
    PS_VIRTUAL: SpecName := 'VIRTUAL';
    PS_OVERRIDE: SpecName := 'OVERRIDE';
    PS_STATIC: SpecName := 'STATIC';
    PS_STDCALL: SpecName := 'STDCALL';
    PS_CDECL: SpecName := 'CDECL';
    PS_FASTCALL: SpecName := 'REGISTER';
    PS_REINTRODUCE: SpecName := 'REINTRODUCE';
  else
    SpecName := '<UNKNOWN>';
  end;
  AbortWork(sDuplicateSpecificationFmt, [SpecName], Lexer.Position);
end;

procedure TASTDelphiErrors.ERROR_UNSUPPORTED_OPERATOR(Op: TOperatorID);
begin
  AbortWork('Unsupported operator "%s"', [OperatorFullName(Op)], Lexer.Position);
end;

procedure TASTDelphiErrors.ERROR_TRY_KEYWORD_MISSED;
begin
  AbortWork(sTryKeywordMissed, Lexer.Position);
end;

procedure TASTDelphiErrors.ERROR_TYPE_IS_NOT_AN_ANCESTOR_FOR_THIS_TYPE(TypeDecl, ChildType: TIDType);
begin
  AbortWork('The type "%s" is not an ancestor of type "%s"', [TypeDecl.DisplayName, ChildType.DisplayName], Lexer.Position);
end;

class procedure TASTDelphiErrors.ERROR_TYPE_REQUIRED(const TextPosition: TTextPosition);
begin
  AbortWork('TYPE specification required', TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_SETTER_MUST_BE_SUCH(const Setter: TIDProcedure; const DeclString: string);
begin
  AbortWork('Setter must have declaration: %s', [DeclString], Setter.ID.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_GETTER_MUST_BE_SUCH(const Getter: TIDProcedure; const DeclString: string);
begin
  AbortWork('Getter must have declaration: %s', [DeclString], Getter.ID.TextPosition);
end;

procedure TASTDelphiErrors.ERROR_EXPORT_ALLOWS_ONLY_IN_INTF_SECTION;
begin
  AbortWork(sExportAllowsOnlyInIntfSection, Lexer.Position);
end;

class procedure TASTDelphiErrors.ERROR_STRING_CONST_IS_NOT_ANSI(const Src: TIDExpression);
begin
  AbortWork('string constant is not an ANSI string', Src.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_STRUCT_TYPE_REQUIRED(const TextPosition: TTextPosition);
begin
  AbortWork(sStructTypeRequired, TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_ARRAY_EXPRESSION_REQUIRED(Expr: TIDExpression);
begin
  AbortWork('ARRAY expression required', Expr.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_ARRAY_TYPE_REQUIRED(const ID: TIdentifier; const TextPosition: TTextPosition);
begin
  AbortWork(sArrayTypeRequired, TextPosition);
end;

procedure TASTDelphiErrors.ERROR_ORDINAL_CONST_OR_TYPE_REQURED;
begin
  AbortWork(sOrdinalConstOrTypeRequred, Lexer_PrevPosition);
end;

class procedure TASTDelphiErrors.ERROR_ORDINAL_OR_SET_REQUIRED(const Src: TIDExpression);
begin
  AbortWork('ORDINAL or SET required', Src.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_INTEGER_TYPE_REQUIRED(const Pos: TTextPosition);
begin
  AbortWork('INTEGER type required', Pos);
end;

class procedure TASTDelphiErrors.ERROR_ORDINAL_TYPE_REQUIRED(const Pos: TTextPosition);
begin
  AbortWork(sOrdinalTypeRequired, Pos);
end;

class procedure TASTDelphiErrors.ERROR_OBJECT_CANNOT_BE_USED_ON_PURE_PROC(const Expr: TIDExpression);
begin
  AbortWork(sObjectCannotBeUsedOnPureProc, [Expr.DisplayName], Expr.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_PROC_OR_PROCVAR_REQUIRED(const ID: TIdentifier);
begin
  AbortWork(sProcOrProcVarRequired, ID.TextPosition);
end;

procedure TASTDelphiErrors.ERROR_PROC_OR_PROP_OR_VAR_REQUIRED;
begin
  AbortWork('PROCEDURE or FUNCTION or PROPERTY or VAR required', [], Lexer.PrevPosition);
end;

class procedure TASTDelphiErrors.ERROR_PROC_OR_TYPE_REQUIRED(const ID: TIdentifier);
begin
  AbortWork('PROCEDURE or TYPE required', ID.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_PROC_REQUIRED(const Position: TTextPosition);
begin
  AbortWork('PROCEDURE or FUNCTION required', Position);
end;

class procedure TASTDelphiErrors.ERROR_RECORD_STATIC_CONSTRUCTOR_ALREADY_EXIST(const Proc: TIDProcedure);
begin
  AbortWork('Record CONSTRUCTOR already exist', Proc.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_RECORD_STATIC_DESTRUCTOR_ALREADY_EXIST(const Proc: TIDProcedure);
begin
  AbortWork('Record DESTRUCTOR already exist', Proc.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_REFERENCE_TYPE_EXPECTED(const Expr: TIDExpression);
begin
  AbortWork('REFERENCE type required', Expr.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_REF_PARAM_MUST_BE_IDENTICAL(Expr: TIDExpression);
begin
  AbortWork('Types of actual and formal VAR parameters must be identical', Expr.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_CANNOT_MODIFY_READONLY_PROPERTY(const Expr: TIDExpression);
begin
  AbortWork(sCannotModifyReadOnlyProperty, Expr.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_CLASS_NOT_IMPLEMENT_INTF(const Src: TIDExpression; Dest: TIDType);
begin
  AbortWork('CLASS "%s" not implement the "%s" interface', [Src.DataTypeName, Dest.DisplayName], Src.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_CLASS_OR_INTF_TYPE_REQUIRED(const TextPosition: TTextPosition);
begin
  AbortWork('CLASS or INTERFACE type required', TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_CLASS_TYPE_REQUIRED(const TextPosition: TTextPosition);
begin
  AbortWork(errCLASSTypeRequired, TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_CANNOT_ACCESS_PRIVATE_MEMBER(const ID: TIdentifier);
begin
  AbortWork('Cannot access private member: %s', [ID.Name], ID.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_CANNOT_ACCESS_TO_WRITEONLY_PROPERTY(const Expr: TIDExpression);
begin
  AbortWork(sCannotAccessToWriteOnlyProperty, Expr.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_CANNOT_ASSIGN_NULL_TO_NOTNULL(const Src: TIDExpression);
begin
  AbortWork('Cannot assign NULLPTR to NOT NULL variable', [], Src.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_CANNOT_ASSIGN_TEMPORARRY_OBJECT(Expr: TIDExpression);
begin
  AbortWork('Cannot modify a temporary object', Expr.TextPosition);
end;

procedure TASTDelphiErrors.ERROR_NEED_SPECIFY_NINDEXES(const Decl: TIDDeclaration);
var
  ADataType: TIDArray;
begin
  ADataType := TIDArray(Decl.DataType);
  AbortWork(sNeedSpecifyNIndexesFmt, [Decl.DisplayName, ADataType.DisplayName, ADataType.DimensionsCount], Lexer_PrevPosition);
end;

procedure TASTDelphiErrors.ERROR_IDENTIFIER_HAS_NO_MEMBERS(const Decl: TIDDeclaration);
begin
  AbortWork(sIdentifierHasNoMembersFmt, [Decl.DisplayName], Lexer.PrevPosition);
end;

class procedure TASTDelphiErrors.ERROR_OVERLOAD(CallExpr: TIDExpression);
begin
  AbortWork(sErrorOverload, CallExpr.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_AMBIGUOUS_OVERLOAD_CALL(CallExpr: TIDExpression);
begin
  AbortWork(sAmbiguousOverloadedCallFmt, [CallExpr.DisplayName], CallExpr.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_INCOMPLETE_PROC(Decl: TIDDeclaration);
var
  ProcName: string;
begin
  ProcName := Decl.DisplayName;
  if Assigned(TIDProcedure(Decl).Struct) then
    ProcName := TIDProcedure(Decl).Struct.Name + '.' + ProcName;
  AbortWork(sIncompleteProcFmt, [ProcName], Decl.SourcePosition);
end;

class procedure TASTDelphiErrors.ERROR_NO_OVERLOAD_OPERATOR_FOR_TYPES(Op: TOperatorID; Left, Right: TIDExpression);
begin
  AbortWork(sNoOverloadOperatorForTypesFmt, [OperatorFullName(Op), Left.DataTypeName, Right.DataTypeName], Left.TextPosition);
end;

class procedure TASTDelphiErrors.ERROR_NO_OVERLOAD_OPERATOR_FOR_TYPES(Op: TOperatorID; Right: TIDExpression);
begin
  AbortWork(sNoOverloadOperatorForTypeFmt, [OperatorFullName(Op), Right.DataTypeName], Right.TextPosition);
end;

procedure TASTDelphiErrors.ERROR_NO_METHOD_IN_BASE_CLASS(Proc: TIDProcedure);
begin
  AbortWork('Method %s is not found in base classes', [Proc.DisplayName], Lexer.Position);
end;

procedure TASTDelphiErrors.HINT_TEXT_AFTER_END;
begin
  PutMessage(cmtHint, 'Text after final END. - ignored by compiler', Lexer_PrevPosition);
end;

procedure TASTDelphiErrors.HINT_RESULT_EXPR_IS_NOT_USED(const Expr: TIDExpression);
begin
  if Assigned(Expr.DataType) then
    PutMessage(cmtHint, 'Expression result (type: ' + Expr.DataType.DisplayName + ') is not used', Expr.TextPosition)
  else
    PutMessage(cmtHint, 'Expression result is not used', Expr.TextPosition);
end;


end.
