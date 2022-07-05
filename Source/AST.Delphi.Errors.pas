unit AST.Delphi.Errors;

interface

uses SysUtils,
     AST.Lexer,
     AST.Lexer.Delphi,
     AST.Delphi.Operators,
     AST.Delphi.Classes;

resourcestring
 // internal errors
  sUnitAlreadyExistFmt =  'Unit ''%s'' already exist';

  sIntfSectionMissing = 'INTERFACE section are missing';
  sKeywordExpected = 'KEYWORD expected but %s found';
  sInternalErrorWord = 'Internal error';
  sErrorWord = 'Error';
  sWarningWord = 'Warning';
  sHintWord = 'Hint';
  sExpected = '%s expected';
  sExpectedButFoundFmt = '%s expected but "%s" found';
  sIdentifierExpected = 'Identifier expected';
  sIdExpectedButFoundFmt = 'Identifier expected but "%s" found';
  sParamNameExpectedButFoundFmt = 'Param name expected but "%s" found';
  sTypeIdExpectedButFoundFmt = 'Type identifier expected but "%s" found';
  sIdentifierRedeclaredFmt = 'Identifier redeclared: "%s"';
  sUndeclaredIdentifier = 'Undeclared identifier: "%s"';
  sDeclDifWithPrevDecl = 'Declaration of "%s" differs from previous declaration';
  sBeginEndCountAreDiffers = 'Count of BEGIN/END clauses does not equals';
  sDevisionByZero = 'Devision by zero';
  sVariableRequired = 'Variable required';
  sVariableOrTypeRequired = 'VARIABLE or TYPE required';
  sCannotModifyObjectPassedAsConstParam = 'Cannot modify object passed as CONST parameter';
  sConstCannotBePassedAsVarParam = 'Constant cannot be passed as VAR parameter';
  sIncompleteProcFmt = 'Incomplete forward declaration ''%s''';
  sVariableIsDeclaredButNeverUsedInFmt =  'Variable "%s" is declared but never used';
    // overload:
  sOverloadedMustBeMarked = 'Overloaded entry "%s" must be marked with the "overload" directive';
  sErrorOverload = '%s: There is no overload version with such parameters';
  sAmbiguousOverloadedCallFmt = 'Ambiguous overloaded call, declarations: %s';
  sInvalidIndex = 'Index is out of bounds';
  sNotAllowedHere = 'Not allowed here: "%s"';
  sUnexpectedEndOfFile = 'Unexpected end of file';
  sUnknownLanguageExpression = 'Unknown language expression: "%s"';
  sStatementExpectedButExpFoundFmt = 'Statement expected, but expression "%s" found';
  sExpressionExpectedButStmntFoundFmt = 'Expression expected, but statement "%s" found';
  sExpressionExpected = 'Expression expected';

  sUnnecessaryClosedBracket = 'Unnecessary closed bracket';
  sUnnecessaryClosedBlock = 'Unnecessary closed block';
  sUnnecessaryEndClause = 'Unnecessary end clause';
  sUnclosedOpenBracket = 'Unclosed open bracket';
  sUnclosedOpenBlock = 'Unclosed open block';
  sMissingOperatorOrSemicolon = 'Missing operator or semicolon';
  sSemicolonExpected = 'SEMICOLON expected';
  sDublicateOperationFmt = 'Dublicate operation "%s"';
  sDuplicateSpecificationFmt = 'Duplicate "%s" specification';
  sReturnValueNotAllowedForProc = 'Return value is not allowed for procedure';
  sParameterTypeRequred = 'Parameter type required';


    // Assignment
  sAssignmentIsImpossible = 'The assignment is impossible, "%s" is not a variable';
  sRightExpressionHasNoResult = 'Right expression has no result';
    // parameters
  sNotEnoughActualParametersFmt = 'Not enough actual parameters for "%s"';
  sTooManyActualParameters = 'Too many actual parameters';
  sCannotPassConstAsVarParamFmt = 'Can not pass const as var parameter "%s"';
    // Types
  sNoOverloadOperatorForTypesFmt = 'No overload operator "%s" for types "%s" and "%s"';
  sNoOverloadOperatorForTypeFmt = 'No overload operator "%s" for type "%s"';
  sUnknownOperatorFmt = 'Unknown operator "%s"';
  sOperatorNeedNCountOfParameters = 'Operator "%s" need %d explicit parameters';
  sIncompatibleTypesFmt = 'Incompatible types: Src: "%s" and Dst: "%s"';
  sInvalidTypecastFmt = 'Can not explicitly convert type "%s" into "%s"';
  sEmptyExpression = 'Empty expression';
  sExpressionMustBeBoolean = 'Type of expression must be BOOLEAN';
  sExpressionMustBeConstant = 'Expression must be a CONSTANT';
  sDeclarationHasNoDataTypeFmt = 'Declaration "%s" has no data type';
  sInvalidTypeDeclarationFmt = '%s Invalid type declaration';
  sTypeKeywordRequred = 'The TYPE keyword required';
  sConstValueOverflowFmt = 'CONST value "%s" exceeds values range for "%s" data type';

  // uses
  sUnitNotFoundFmt = 'Unit not found: %s';
  sUnitRecursivelyUsesItselfFmt = 'Program or unit ''%s'' recursively uses itself';

  // loops
  sContinueAllowedOnlyInLoop = 'continue allowed only in loop';
  sBreakOrContinueAreAllowedOnlyInALoops = 'BREAK and CONTINUE are allowed only in a loops';
  sLoopLevelExprected = 'Loop level exprected';
  sLoopLevelGreaterThenPossibleFmt = 'The loop level is greater than possible, max level is %d';
  sForLoopIndexVarsMastBeSimpleIntVar = 'For loop index variable must be local integer variable';
  sKeywordToOrDowntoExpected = 'Кeyword TO or DOWNTO are expected';
  sForOrWhileLoopExecutesZeroTimes = 'FOR or WHILE-loop executes zero times - deleted';
  sZeroDeltaInForLoop = 'Zero delta in FOR-loop, infinity loop';
  sCannotModifyForLoopIndexVarFmt = 'Cannot modify FOR-loop index variable %s';

    // records
  sIdentifierHasNoMembersFmt = 'Identifier "%s" has no members';
  sRecordTypeRequired = 'Record type is required';
  sStructTypeRequired = 'Structured type is required';
  sRecurciveTypeLinkIsNotAllowed = 'Recurcive link is not allowed';
  sFieldConstOrFuncRequiredForGetter = 'The field, constant or function are required for a getter';
  sFieldOrProcRequiredForSetter = 'The field or procedure are required for a setter';
  sCannotModifyReadOnlyProperty = 'Cannot modify the read-only property';
  sCannotAccessToWriteOnlyProperty = 'Cannot access to write-only property';
  sSetterMustBeMethodWithSignFmt = 'Setter must be a method with signature: procedure(const Value: %s);';
  sDefaultPropertyMustBeAnArrayProperty =  'Default property must be an array property';
  sDefaultPropertyAlreadyExistsFmt = 'Default property already exist: "%s"';


    // classes
  sClassTypeCannotBeAnonimous = 'CLASS type cannot be anonimous';
  sClassTypeRequired = 'CLASS type required';


    // interfaces
  sInterfacedTypeCannotBeAnonimous = 'Interfaced type cannot be anonimous';
  sInterfaceTypeRequired = 'Interface type required';


    // arrays
  sOrdinalTypeRequired = 'ORDINAL type required';
  sOrdinalConstOrTypeRequred = 'ORDINAL constant or type required';
  sArrayTypeRequired = 'Array type required';
  sArrayOrStringTypeRequired = 'ARRAY or STRING type required';
  sNeedSpecifyNIndexesFmt = 'For access to array ''%s: %s'' need specify %d indexes';
  sConstExprOutOfRangeFmt = 'Const expression ''%d'' out of range [%d..%d]';
  sOpenArrayAllowedOnlyAsTypeOfParam = 'Open array allowed only as type of parameter';

    // New/Free
  sPointerTypeRequired = 'POINTER type required';
  sReferenceTypeRequired = 'REFERENCE type required';


    // Ranges
  sLowerBoundExceedsHigherBound = 'Lower bound exceeds higher bound';
  sNumericTypeRequired = 'Numeric type required';
  sConstRangeRequired = 'Const range required';


    // Enums
  sComaOrCloseRoundExpected = ''','' or '')'' expected';

    // Expressions
  sSingleExpressionRequired = 'Single expression required';
  sStringExpressionRequired = 'STRING expression required';

    // Import/Export
  sImportFuncCannotBeInline = 'Import function can not be INLINE';
  sExportAllowsOnlyInIntfSection = 'Export allows only in INTERFACE section';

    // Interanl errors
  sOperatorForTypesAlreadyOverloadedFmt = 'Operator ''%s'' for types ''%s'' and ''%s'' already overloaded';
  sFeatureNotSupported = 'Feature is not supported';

    // Platform/ASM
  sInstructionAlreadyDeclaredFmt = 'Instruction "%s" already declared';
  sRegisterAlreadyDeclaredFmt = 'Register "%s" already declared';
  sUnknownInstructionFmt = 'Unknown instruction "%s"';
  sUnknownPlatformFmt = 'Unknown platform "%s"';
  sProcedureOrFunctionKeywordAreExpected = 'PROCEDURE or FUNCTION keyword are expected';
    //sProcOrFuncRequired = 'Procedure or function required';
  sDuplicateOpenBlock = 'Duplicate open block';
  sASMSyntaxError = 'Assembler syntax error';
  sDestArgCannotBeConst = 'Destination argument cannot be a constant';
  sEmptyArgument = 'Empty argument';
  sInvalidArgument = 'Invalid argument';
  sLabelRedeclaretedFmt = 'Label "%s" redeclareted';
  sImmediateOffsetIsOutOfRangeFmt = 'Immediate offset is out of range (%d..%d)';

    // IIF
  sTypesMustBeIdentical = 'Types must be identical';
  sThenAndElseSectionAreIdentical = 'THEN and ELSE sections are identical';

    // Try/Except/Finally
  sTryKeywordMissed = 'TRY keyword missed';
  sExceptOrFinallySectionWasMissed = 'EXCEPT or FINALLY section was missed';
  sSectionFinallyAlreadyDefined = 'Section FINALLY already defined';
  sSectionExceptAlreadyDefined = 'Section EXCEPT already defined';
  sExceptSectionMustBeDeclareBeforeFinally = 'EXCEPT section must be declared before FINALLY section';
  sBreakContinueExitAreNotAllowedInFinallyClause = 'BREAK, CONTINUE or EXIT are not allowed in FINALLY clause';

    // case
  sCaseStmtRequireAtLeastOneMatchExpr = 'CASE statement require at least one match expression';
  sMatchExprTypeMustBeIdenticalToCaseExprFmt = 'Match expression type [%s] must be identical to CASE expression type [%s]';
  sDuplicateMatchExpression = 'Duplicate match expression';

    // addr
  sVarOrProcRequired = 'Variable or procedure required';

  sProcOrProcVarRequired = 'Procedure or variable of procedure type is required';

    // generics
  sNoOneTypeParamsWasFound = 'No one type parameters was found';
  sGenericProcInstanceErrorFmt = 'Generic procedure %s specialization ERROR:';
  sGenericFuncInstanceErrorFmt = 'Generic function %s specialization ERROR:';
  sProcHasNoGenericParams = 'The %s "%s" has no generic parameters';
  sProcRequiresExplicitTypeArgumentFmt = 'The %s "%s" requires explicit type argument(s)';
  sTooManyActualTypeParameters = 'Too many actual type arguments';

  // conditional statements
  sInvalidConditionalStatement = 'Invalid conditional statement';


type
  TASTDelphiErrors = class
  private
    fLexer: TDelphiLexer;
    property Lexer: TDelphiLexer read fLexer;
  public
    constructor Create(Lexer: TDelphiLexer);
    class procedure ARG_VAR_REQUIRED(Expr: TIDExpression); static;
    class procedure CONST_EXPRESSION_REQUIRED(Expr: TIDExpression); static;
    class procedure INCOMPATIBLE_TYPES(const Src, Dst: TIDExpression); overload; static;
    class procedure INCOMPATIBLE_TYPES(const Src: TIDExpression; Dst: TIDType); overload; static;
    class procedure INVALID_EXPLICIT_TYPECAST(const Src: TIDExpression; Dst: TIDType); static;
    class procedure VAR_EXPRESSION_REQUIRED(Expr: TIDExpression); static;
    class procedure VAR_OR_PROC_EXPRESSION_REQUIRED(Expr: TIDExpression); static;
    class procedure CANNOT_MODIFY_FOR_LOOP_VARIABLE(Expr: TIDExpression); static;
    class procedure CANNOT_MODIFY_CONSTANT(Expr: TIDExpression); static;
    class procedure BOOLEAN_EXPRESSION_REQUIRED(Expr: TIDExpression); static;
    class procedure ARRAY_EXPRESSION_REQUIRED(Expr: TIDExpression); static;
    class procedure ID_REDECLARATED(Decl: TIDDeclaration); overload; static;
    class procedure ID_REDECLARATED(const ID: TIdentifier); overload; static;
    class procedure UNDECLARED_ID(const ID: TIdentifier); overload; static;
    class procedure UNDECLARED_ID(const ID: TIdentifier; const GenericParams: TIDTypeList); overload; static;
    class procedure UNDECLARED_ID(const Name: string; const TextPosition: TTextPosition); overload; static;
    class procedure NOT_ENOUGH_ACTUAL_PARAMS(CallExpr: TIDExpression); static;
    class procedure TOO_MANY_ACTUAL_PARAMS(CallExpr: TIDExpression); static;
    class procedure OVERLOADED_MUST_BE_MARKED(const ID: TIdentifier); static;
    class procedure DECL_DIFF_WITH_PREV_DECL(const ID: TIdentifier); static;

    class procedure TYPE_REQUIRED(const TextPosition: TTextPosition); static;
    class procedure STRUCT_TYPE_REQUIRED(const TextPosition: TTextPosition); static;
    class procedure ARRAY_TYPE_REQUIRED(const ID: TIdentifier; const TextPosition: TTextPosition); static;
    class procedure INTEGER_TYPE_REQUIRED(const Pos: TTextPosition); static;
    class procedure ORDINAL_TYPE_REQUIRED(const Pos: TTextPosition); static;
    class procedure PROC_OR_PROCVAR_REQUIRED(const ID: TIdentifier); static;
    class procedure PROC_REQUIRED(const Position: TTextPosition); static;
    class procedure PROC_OR_TYPE_REQUIRED(const ID: TIdentifier); static;
    class procedure CLASS_TYPE_REQUIRED(const TextPosition: TTextPosition); static;
    class procedure CLASS_OR_INTF_TYPE_REQUIRED(const TextPosition: TTextPosition); static;
    class procedure INTF_TYPE_REQUIRED(Expr: TIDExpression);
    class procedure REFERENCE_TYPE_EXPECTED(const Expr: TIDExpression);

    class procedure CANNOT_ACCESS_TO_WRITEONLY_PROPERTY(const Expr: TIDExpression); static;
    class procedure CANNOT_MODIFY_READONLY_PROPERTY(const Expr: TIDExpression); static;
    class procedure NO_OVERLOAD(CallExpr: TIDExpression; const CallArgs: TIDExpressions); static;
    class procedure AMBIGUOUS_OVERLOAD_CALL(CallExpr: TIDExpression); overload; static;
    class procedure INCOMPLETE_PROC(Decl: TIDDeclaration); static;
    class procedure NO_OVERLOAD_OPERATOR_FOR_TYPES(Op: TOperatorID; Left, Right: TIDExpression); overload; static;
    class procedure NO_OVERLOAD_OPERATOR_FOR_TYPES(Op: TOperatorID; Right: TIDExpression); overload;
    class procedure UNIT_NOT_FOUND(const ID: TIdentifier); static;
    class procedure UNIT_RECURSIVELY_USES_ITSELF(const ID: TIdentifier); static;
    class procedure SETTER_MUST_BE_SUCH(const DeclString: string; const TextPosition: TTextPosition); static;
    class procedure GETTER_MUST_BE_SUCH(const Getter: TIDProcedure; const DeclString: string); static;
    class procedure DIVISION_BY_ZERO(Expr: TIDExpression); static;
    class procedure CONST_VALUE_OVERFLOW(Expr: TIDExpression; DstDataType: TIDType); static;
    class procedure CLASS_NOT_IMPLEMENT_INTF(const Src: TIDExpression; Dest: TIDType); static;
    class procedure METHOD_NOT_DECLARED_IN_CLASS(const ID: TIdentifier; Struct: TIDStructure); static;
    class procedure INPLACEVAR_ALLOWED_ONLY_FOR_OUT_PARAMS(Variable: TIDVariable);
    class procedure CANNOT_ACCESS_PRIVATE_MEMBER(const ID: TIdentifier); static;
    class procedure REF_PARAM_MUST_BE_IDENTICAL(Expr: TIDExpression);
    class procedure UNKNOWN_OPTION(const ID: TIdentifier);
    class procedure CANNOT_ASSIGN_TEMPORARRY_OBJECT(Expr: TIDExpression);

    class procedure CLASS_CONSTRUCTOR_ALREADY_EXIST(const Proc: TIDProcedure);
    class procedure CLASS_DESTRUCTOR_ALREADY_EXIST(const Proc: TIDProcedure);


    class procedure INTF_ALREADY_IMPLEMENTED(Expr: TIDExpression);
    class procedure INTF_METHOD_NOT_IMPLEMENTED(ClassType: TIDClass; Proc: TIDProcedure);
    class procedure RECORD_STATIC_CONSTRUCTOR_ALREADY_EXIST(const Proc: TIDProcedure);
    class procedure RECORD_STATIC_DESTRUCTOR_ALREADY_EXIST(const Proc: TIDProcedure);
    class procedure CTOR_DTOR_MUST_BE_DECLARED_IN_STRUCT(const Position: TTextPosition);
    class procedure PARAMETERLESS_CTOR_NOT_ALLOWED_ON_RECORD(const Position: TTextPosition); static;
    class procedure OPERATOR_MUST_BE_DECLARED_IN_STRUCT(const Position: TTextPosition);
    class procedure CANNOT_ASSIGN_NULL_TO_NOTNULL(const Src: TIDExpression);
    class procedure ORDINAL_OR_SET_REQUIRED(const Src: TIDExpression);
    class procedure STRING_CONST_IS_NOT_ANSI(const Src: TIDExpression);
    class procedure VAR_IS_NOT_INITIALIZED(const Variable: TIDExpression);
    procedure NO_METHOD_IN_BASE_CLASS(Proc: TIDProcedure);
    procedure DEFAULT_PROP_MUST_BE_ARRAY_PROP;
    procedure DEFAULT_PROP_ALREADY_EXIST(Prop: TIDProperty);
    procedure IMPORT_FUNCTION_CANNOT_BE_INLINE;
    procedure INVALID_TYPE_DECLARATION(const ID: TIdentifier); overload;
    procedure INVALID_TYPE_DECLARATION; overload;
    procedure EXPECTED_TOKEN(Token: TTokenID; ActulToken: TTokenID = token_unknown);
    procedure EXPECTED_KEYWORD_OR_ID;
    procedure IDENTIFIER_EXPECTED(ActualToken: TTokenID); overload;
    procedure IDENTIFIER_EXPECTED; overload;
    procedure PARAM_NAME_ID_EXPECTED(ActualToken: TTokenID);
    procedure SEMICOLON_EXPECTED;
    procedure EMPTY_EXPRESSION;
    procedure END_OF_FILE;
    procedure EXPRESSION_EXPECTED;
    procedure FEATURE_NOT_SUPPORTED(const UnsupportedKeyword: string = '');
    procedure INTF_SECTION_MISSING;
    procedure KEYWORD_EXPECTED;
    procedure BEGIN_KEYWORD_EXPECTED;
    procedure DUPLICATE_SPECIFICATION(Spec: TProcSpecification);
    procedure UNSUPPORTED_OPERATOR(Op: TOperatorID);
    procedure TRY_KEYWORD_MISSED;
    procedure EXPORT_ALLOWS_ONLY_IN_INTF_SECTION;
    procedure NEED_SPECIFY_NINDEXES(const Decl: TIDDeclaration);
    procedure IDENTIFIER_HAS_NO_MEMBERS(const Decl: TIDDeclaration);
    procedure INVALID_COND_DIRECTIVE;
    procedure INCOMPLETE_STATEMENT(const Statement: string = '');
    procedure PROC_OR_PROP_OR_VAR_REQUIRED;
    procedure HINT_TEXT_AFTER_END;
    procedure PARAM_TYPE_REQUIRED;
    procedure INHERITED_ALLOWED_ONLY_IN_OVERRIDE_METHODS;
    procedure UNCLOSED_OPEN_BLOCK;
    procedure UNNECESSARY_CLOSED_BLOCK;
    procedure UNNECESSARY_CLOSED_ROUND;
    procedure INIT_SECTION_ALREADY_DEFINED;
    procedure FINAL_SECTION_ALREADY_DEFINED;
    procedure ORDINAL_CONST_OR_TYPE_REQURED;
    procedure DESTRUCTOR_CANNOT_BE_CALL_DIRECTLY;
    procedure VIRTUAL_ALLOWED_ONLY_IN_CLASSES;
    procedure WEAKREF_CANNOT_BE_DECLARED_FOR_TYPE(TypeDecl: TIDType);
    procedure TYPE_IS_NOT_AN_ANCESTOR_FOR_THIS_TYPE(TypeDecl, ChildType: TIDType);
    procedure INVALID_HEX_CONSTANT;
    procedure INVALID_BIN_CONSTANT;
    procedure INTERNAL(const Message: string = 'GENERAL ERROR');
    //procedure HINT_RESULT_EXPR_IS_NOT_USED(const Expr: TIDExpression);
    //procedure HINT_TYPE_DELETE_UNUSED(Decl: TIDDeclaration);
    //procedure HINT_PROC_DELETE_UNUSED(Decl: TIDDeclaration);
  end;

implementation

uses AST.Parser.Utils,
     AST.Parser.Errors,
     AST.Parser.Messages;

resourcestring
  sIncompleteStatement = 'Incomplete statement';

constructor TASTDelphiErrors.Create(Lexer: TDelphiLexer);
begin
  fLexer := Lexer;
end;

procedure TASTDelphiErrors.INCOMPLETE_STATEMENT(const Statement: string);
begin
  if Statement <> '' then
    AbortWork(sIncompleteStatement + ': ' + Statement, Lexer.Position)
  else
    AbortWork(sIncompleteStatement, Lexer.Position);
end;

procedure TASTDelphiErrors.INHERITED_ALLOWED_ONLY_IN_OVERRIDE_METHODS;
begin
  AbortWork('INHERITED calls allowed only in OVERRIDE methods', Lexer.PrevPosition);
end;

procedure TASTDelphiErrors.INIT_SECTION_ALREADY_DEFINED;
begin
  AbortWork('INITIALIZATION section is already defined', Lexer.Position);
end;

class procedure TASTDelphiErrors.INPLACEVAR_ALLOWED_ONLY_FOR_OUT_PARAMS(Variable: TIDVariable);
begin
  AbortWork('Inpalce VAR declaration allowed only for OUT parameters', Variable.TextPosition);
end;

class procedure TASTDelphiErrors.INCOMPATIBLE_TYPES(const Src, Dst: TIDExpression);
var
  SrcName, DstName: string;
begin
  if Src.ItemType <> itType then SrcName := Src.DataTypeName else SrcName := Src.DisplayName;
  if Dst.ItemType <> itType then DstName := Dst.DataTypeName else DstName := Dst.DisplayName;
  AbortWork(sIncompatibleTypesFmt, [SrcName, DstName], Src.TextPosition);
end;

class procedure TASTDelphiErrors.INCOMPATIBLE_TYPES(const Src: TIDExpression; Dst: TIDType);
begin
  AbortWork(sIncompatibleTypesFmt, [Src.DataTypeName, Dst.DisplayName], Src.TextPosition);
end;

class procedure TASTDelphiErrors.CONST_EXPRESSION_REQUIRED(Expr: TIDExpression);
begin
  AbortWork(sExpressionMustBeConstant, Expr.TextPosition);
end;

class procedure TASTDelphiErrors.CONST_VALUE_OVERFLOW(Expr: TIDExpression; DstDataType: TIDType);
begin
  AbortWork(sConstValueOverflowFmt, [Expr.DisplayName, DstDataType.DisplayName], Expr.TextPosition);
end;

class procedure TASTDelphiErrors.CTOR_DTOR_MUST_BE_DECLARED_IN_STRUCT(const Position: TTextPosition);
begin
  AbortWork('CONSTRUCTOR or DESTRUCTOR must be declared only in a CLASS or RECORD type', Position);
end;

class procedure TASTDelphiErrors.OPERATOR_MUST_BE_DECLARED_IN_STRUCT(const Position: TTextPosition);
begin
  AbortWork('OPERATOR must be declared only in a CLASS or RECORD type', Position);
end;

class procedure TASTDelphiErrors.PARAMETERLESS_CTOR_NOT_ALLOWED_ON_RECORD(const Position: TTextPosition);
begin
  AbortWork('Parameterless constructors not allowed on record types', Position);
end;

class procedure TASTDelphiErrors.VAR_EXPRESSION_REQUIRED(Expr: TIDExpression);
begin
  AbortWork(sVariableRequired, Expr.TextPosition);
end;

class procedure TASTDelphiErrors.VAR_IS_NOT_INITIALIZED(const Variable: TIDExpression);
begin
  // пока еще не доделано
  // AbortWork('Variable "%s" is not initialized', [Variable.DisplayName], Variable.TextPosition);
end;

class procedure TASTDelphiErrors.VAR_OR_PROC_EXPRESSION_REQUIRED(Expr: TIDExpression);
begin
  AbortWork(sVarOrProcRequired, Expr.TextPosition);
end;

procedure TASTDelphiErrors.VIRTUAL_ALLOWED_ONLY_IN_CLASSES;
begin
  AbortWork('VIRTUAL allowed only for class methods', Lexer.PrevPosition);
end;

procedure TASTDelphiErrors.WEAKREF_CANNOT_BE_DECLARED_FOR_TYPE(TypeDecl: TIDType);
begin
  AbortWork('The WEAK reference can not be declared for type: %s', [TypeDecl.DisplayName], Lexer.PrevPosition);
end;

class procedure TASTDelphiErrors.ARG_VAR_REQUIRED(Expr: TIDExpression);
begin
  AbortWork(sConstCannotBePassedAsVarParam, Expr.TextPosition);
end;

class procedure TASTDelphiErrors.CANNOT_MODIFY_CONSTANT(Expr: TIDExpression);
begin
  AbortWork(sCannotModifyObjectPassedAsConstParam, Expr.TextPosition);
end;

class procedure TASTDelphiErrors.CANNOT_MODIFY_FOR_LOOP_VARIABLE(Expr: TIDExpression);
begin
  AbortWork(sCannotModifyForLoopIndexVarFmt, [Expr.DisplayName], Expr.TextPosition);
end;

procedure TASTDelphiErrors.BEGIN_KEYWORD_EXPECTED;
begin
  AbortWork('BEGIN expected', Lexer.PrevPosition);
end;

class procedure TASTDelphiErrors.BOOLEAN_EXPRESSION_REQUIRED(Expr: TIDExpression);
begin
  AbortWork(sExpressionMustBeBoolean, Expr.TextPosition);
end;

class procedure TASTDelphiErrors.NOT_ENOUGH_ACTUAL_PARAMS(CallExpr: TIDExpression);
begin
  AbortWork(sNotEnoughActualParametersFmt, [CallExpr.DisplayName], CallExpr.TextPosition);
end;

class procedure TASTDelphiErrors.OVERLOADED_MUST_BE_MARKED(const ID: TIdentifier);
begin
  AbortWork(sOverloadedMustBeMarked, [ID.Name], ID.TextPosition);
end;

class procedure TASTDelphiErrors.DECL_DIFF_WITH_PREV_DECL(const ID: TIdentifier);
begin
  AbortWork(sDeclDifWithPrevDecl, [ID.Name], ID.TextPosition);
end;

procedure TASTDelphiErrors.DEFAULT_PROP_ALREADY_EXIST(Prop: TIDProperty);
begin
  AbortWork(sDefaultPropertyAlreadyExistsFmt, [Prop.Name], Lexer.Position);
end;

procedure TASTDelphiErrors.DEFAULT_PROP_MUST_BE_ARRAY_PROP;
begin
  AbortWork(sDefaultPropertyMustBeAnArrayProperty, Lexer.Position);
end;

procedure TASTDelphiErrors.DESTRUCTOR_CANNOT_BE_CALL_DIRECTLY;
begin
  AbortWork('DESTRUCTOR cannot be call directly', Lexer.PrevPosition);
end;

class procedure TASTDelphiErrors.DIVISION_BY_ZERO(Expr: TIDExpression);
begin
  AbortWork(sDevisionByZero, Expr.TextPosition);
end;

class procedure TASTDelphiErrors.TOO_MANY_ACTUAL_PARAMS(CallExpr: TIDExpression);
begin
  AbortWork(sTooManyActualParameters, CallExpr.TextPosition);
end;

procedure TASTDelphiErrors.SEMICOLON_EXPECTED;
begin
  AbortWork(sSemicolonExpected, Lexer.PrevPosition);
end;

procedure TASTDelphiErrors.EXPECTED_KEYWORD_OR_ID;
begin
  AbortWork(sExpectedButFoundFmt, ['Identifier or keyword', Lexer.TokenName], Lexer.Position);
end;

procedure TASTDelphiErrors.EXPECTED_TOKEN(Token: TTokenID; ActulToken: TTokenID = token_unknown);
begin
  if ActulToken = token_unknown then
    AbortWork(sExpected, ['Token "' + UpperCase(Lexer.TokenLexem(Token)) + '"'], Lexer.PrevPosition)
  else
    AbortWork(sExpectedButFoundFmt, ['Token "' + UpperCase(Lexer.TokenLexem(Token)) + '"',
                                                 UpperCase(Lexer.TokenLexem(ActulToken))], Lexer.PrevPosition);
end;

procedure TASTDelphiErrors.IDENTIFIER_EXPECTED(ActualToken: TTokenID);
begin
  AbortWork(sIdExpectedButFoundFmt, [Lexer.TokenLexem(ActualToken)], Lexer.PrevPosition);
end;

procedure TASTDelphiErrors.PARAM_NAME_ID_EXPECTED(ActualToken: TTokenID);
begin
  AbortWork(sIdExpectedButFoundFmt, [Lexer.TokenLexem(ActualToken)], Lexer.PrevPosition);
end;

procedure TASTDelphiErrors.PARAM_TYPE_REQUIRED;
begin
  AbortWork(sParameterTypeRequred, Lexer.PrevPosition);
end;

procedure TASTDelphiErrors.IDENTIFIER_EXPECTED;
begin
  AbortWork(sIdentifierExpected, Lexer.PrevPosition);
end;

procedure TASTDelphiErrors.EMPTY_EXPRESSION;
begin
  AbortWork(sEmptyExpression, Lexer.PrevPosition);
end;

procedure TASTDelphiErrors.EXPRESSION_EXPECTED;
begin
  AbortWork(sExpressionExpected, Lexer.PrevPosition);
end;

procedure TASTDelphiErrors.END_OF_FILE;
begin
  AbortWork(sUnexpectedEndOfFile, Lexer.PrevPosition);
end;

procedure TASTDelphiErrors.FEATURE_NOT_SUPPORTED(const UnsupportedKeyword: string);
begin
  if UnsupportedKeyword <> '' then
    AbortWork(sFeatureNotSupported + ': ' + UnsupportedKeyword, Lexer.Position)
  else
    AbortWork(sFeatureNotSupported + UnsupportedKeyword, Lexer.Position);
end;

procedure TASTDelphiErrors.FINAL_SECTION_ALREADY_DEFINED;
begin
  AbortWork('FINALIZATION section is already defined', Lexer.Position);
end;

procedure TASTDelphiErrors.INTERNAL(const Message: string);
begin
  AbortWorkInternal('Internal error: ' + Message, Lexer.Position);
end;

class procedure TASTDelphiErrors.INTF_ALREADY_IMPLEMENTED(Expr: TIDExpression);
begin
  AbortWork('INTERFACE "%s" is already implemented', [Expr.DisplayName], Expr.TextPosition);
end;

class procedure TASTDelphiErrors.INTF_METHOD_NOT_IMPLEMENTED(ClassType: TIDClass; Proc: TIDProcedure);
begin
  AbortWork('Interface method "%s" is not implemented in class "%s"', [Proc.DisplayName, ClassType.DisplayName], ClassType.TextPosition);
end;

procedure TASTDelphiErrors.INTF_SECTION_MISSING;
begin
  AbortWork(sIntfSectionMissing, Lexer.Position);
end;

class procedure TASTDelphiErrors.INTF_TYPE_REQUIRED(Expr: TIDExpression);
begin
  AbortWork('INTERFACE type required', Expr.TextPosition);
end;

procedure TASTDelphiErrors.INVALID_BIN_CONSTANT;
begin
  AbortWork('Invalid BIN constant', Lexer.Position);
end;

procedure TASTDelphiErrors.INVALID_COND_DIRECTIVE;
begin
  AbortWork(sInvalidConditionalStatement, Lexer.Position);
end;

procedure TASTDelphiErrors.INVALID_HEX_CONSTANT;
begin
  AbortWork('Invalid HEX constant', Lexer.Position);
end;

class procedure TASTDelphiErrors.INVALID_EXPLICIT_TYPECAST(const Src: TIDExpression; Dst: TIDType);
begin
  AbortWork(sInvalidTypecastFmt, [Src.DataTypeName, Dst.DisplayName], Src.TextPosition);
end;

procedure TASTDelphiErrors.INVALID_TYPE_DECLARATION(const ID: TIdentifier);
begin
  AbortWork(sInvalidTypeDeclarationFmt, [ID.Name], ID.TextPosition);
end;

procedure TASTDelphiErrors.INVALID_TYPE_DECLARATION;
begin
  AbortWork('Invalid type declaration', Lexer.Position);
end;

class procedure TASTDelphiErrors.ID_REDECLARATED(Decl: TIDDeclaration);
begin
  AbortWork(sIdentifierRedeclaredFmt, [Decl.DisplayName], Decl.SourcePosition);
end;

class procedure TASTDelphiErrors.ID_REDECLARATED(const ID: TIdentifier);
begin
  AbortWork(sIdentifierRedeclaredFmt, [ID.Name], ID.TextPosition);
end;

procedure TASTDelphiErrors.IMPORT_FUNCTION_CANNOT_BE_INLINE;
begin
  AbortWork(sImportFuncCannotBeInline, Lexer.Position);
end;

procedure TASTDelphiErrors.UNCLOSED_OPEN_BLOCK;
begin
  AbortWork(sUnclosedOpenBlock, Lexer.PrevPosition);
end;

class procedure TASTDelphiErrors.UNDECLARED_ID(const ID: TIdentifier);
begin
  AbortWork(sUndeclaredIdentifier, [ID.Name], ID.TextPosition);
end;

class procedure TASTDelphiErrors.UNDECLARED_ID(const ID: TIdentifier; const GenericParams: TIDTypeList);
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

class procedure TASTDelphiErrors.UNDECLARED_ID(const Name: string; const TextPosition: TTextPosition);
begin
  AbortWork(sUndeclaredIdentifier, [Name], TextPosition);
end;

procedure TASTDelphiErrors.UNNECESSARY_CLOSED_BLOCK;
begin
  AbortWork(sUnnecessaryClosedBlock, Lexer.Position);
end;

procedure TASTDelphiErrors.UNNECESSARY_CLOSED_ROUND;
begin
  AbortWork(sUnnecessaryClosedBracket, Lexer.Position);
end;

class procedure TASTDelphiErrors.UNIT_NOT_FOUND(const ID: TIdentifier);
begin
  AbortWork(sUnitNotFoundFmt, [ID.Name], ID.TextPosition);
end;

class procedure TASTDelphiErrors.UNIT_RECURSIVELY_USES_ITSELF(const ID: TIdentifier);
begin
  AbortWork(sUnitRecursivelyUsesItselfFmt, [ID.Name], ID.TextPosition);
end;

class procedure TASTDelphiErrors.UNKNOWN_OPTION(const ID: TIdentifier);
begin
  AbortWork('Unknown option: %s', [ID.Name], ID.TextPosition);
end;

procedure TASTDelphiErrors.KEYWORD_EXPECTED;
begin
  AbortWork(sKeywordExpected, [Lexer.OriginalToken], Lexer.Position);
end;

class procedure TASTDelphiErrors.METHOD_NOT_DECLARED_IN_CLASS(const ID: TIdentifier; Struct: TIDStructure);
begin
  AbortWork('Method "%s" is not declared in "%s"', [ID.Name, Struct.Name], ID.TextPosition);
end;

procedure TASTDelphiErrors.DUPLICATE_SPECIFICATION(Spec: TProcSpecification);
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

procedure TASTDelphiErrors.UNSUPPORTED_OPERATOR(Op: TOperatorID);
begin
  AbortWork('Unsupported operator "%s"', [OperatorFullName(Op)], Lexer.Position);
end;

procedure TASTDelphiErrors.TRY_KEYWORD_MISSED;
begin
  AbortWork(sTryKeywordMissed, Lexer.Position);
end;

procedure TASTDelphiErrors.TYPE_IS_NOT_AN_ANCESTOR_FOR_THIS_TYPE(TypeDecl, ChildType: TIDType);
begin
  AbortWork('The type "%s" is not an ancestor of type "%s"', [TypeDecl.DisplayName, ChildType.DisplayName], Lexer.Position);
end;

class procedure TASTDelphiErrors.TYPE_REQUIRED(const TextPosition: TTextPosition);
begin
  AbortWork('TYPE specification required', TextPosition);
end;

class procedure TASTDelphiErrors.SETTER_MUST_BE_SUCH(const DeclString: string; const TextPosition: TTextPosition);
begin
  AbortWork('Setter must have declaration: %s', [DeclString], TextPosition);
end;

class procedure TASTDelphiErrors.GETTER_MUST_BE_SUCH(const Getter: TIDProcedure; const DeclString: string);
begin
  AbortWork('Getter must have declaration: %s', [DeclString], Getter.ID.TextPosition);
end;

procedure TASTDelphiErrors.EXPORT_ALLOWS_ONLY_IN_INTF_SECTION;
begin
  AbortWork(sExportAllowsOnlyInIntfSection, Lexer.Position);
end;

class procedure TASTDelphiErrors.STRING_CONST_IS_NOT_ANSI(const Src: TIDExpression);
begin
  AbortWork('string constant is not an ANSI string', Src.TextPosition);
end;

class procedure TASTDelphiErrors.STRUCT_TYPE_REQUIRED(const TextPosition: TTextPosition);
begin
  AbortWork(sStructTypeRequired, TextPosition);
end;

class procedure TASTDelphiErrors.ARRAY_EXPRESSION_REQUIRED(Expr: TIDExpression);
begin
  AbortWork('ARRAY expression required', Expr.TextPosition);
end;

class procedure TASTDelphiErrors.ARRAY_TYPE_REQUIRED(const ID: TIdentifier; const TextPosition: TTextPosition);
begin
  AbortWork(sArrayTypeRequired, TextPosition);
end;

procedure TASTDelphiErrors.ORDINAL_CONST_OR_TYPE_REQURED;
begin
  AbortWork(sOrdinalConstOrTypeRequred, Lexer.PrevPosition);
end;

class procedure TASTDelphiErrors.ORDINAL_OR_SET_REQUIRED(const Src: TIDExpression);
begin
  AbortWork('ORDINAL or SET required', Src.TextPosition);
end;

class procedure TASTDelphiErrors.INTEGER_TYPE_REQUIRED(const Pos: TTextPosition);
begin
  AbortWork('INTEGER type required', Pos);
end;

class procedure TASTDelphiErrors.ORDINAL_TYPE_REQUIRED(const Pos: TTextPosition);
begin
  AbortWork(sOrdinalTypeRequired, Pos);
end;

class procedure TASTDelphiErrors.PROC_OR_PROCVAR_REQUIRED(const ID: TIdentifier);
begin
  AbortWork(sProcOrProcVarRequired, ID.TextPosition);
end;

procedure TASTDelphiErrors.PROC_OR_PROP_OR_VAR_REQUIRED;
begin
  AbortWork('PROCEDURE or FUNCTION or PROPERTY or VAR required', [], Lexer.PrevPosition);
end;

class procedure TASTDelphiErrors.PROC_OR_TYPE_REQUIRED(const ID: TIdentifier);
begin
  AbortWork('PROCEDURE or TYPE required', ID.TextPosition);
end;

class procedure TASTDelphiErrors.PROC_REQUIRED(const Position: TTextPosition);
begin
  AbortWork('PROCEDURE or FUNCTION required', Position);
end;

class procedure TASTDelphiErrors.RECORD_STATIC_CONSTRUCTOR_ALREADY_EXIST(const Proc: TIDProcedure);
begin
  AbortWork('Record CONSTRUCTOR already exist', Proc.TextPosition);
end;

class procedure TASTDelphiErrors.RECORD_STATIC_DESTRUCTOR_ALREADY_EXIST(const Proc: TIDProcedure);
begin
  AbortWork('Record DESTRUCTOR already exist', Proc.TextPosition);
end;

class procedure TASTDelphiErrors.REFERENCE_TYPE_EXPECTED(const Expr: TIDExpression);
begin
  AbortWork('REFERENCE type required', Expr.TextPosition);
end;

class procedure TASTDelphiErrors.REF_PARAM_MUST_BE_IDENTICAL(Expr: TIDExpression);
begin
  AbortWork('Types of actual and formal VAR parameters must be identical: ' + Expr.DisplayName, Expr.TextPosition);
end;

class procedure TASTDelphiErrors.CANNOT_MODIFY_READONLY_PROPERTY(const Expr: TIDExpression);
begin
  AbortWork(sCannotModifyReadOnlyProperty, Expr.TextPosition);
end;

class procedure TASTDelphiErrors.CLASS_CONSTRUCTOR_ALREADY_EXIST(const Proc: TIDProcedure);
begin
  AbortWork('CLASS CONSTRUCTOR already exists', Proc.TextPosition);
end;

class procedure TASTDelphiErrors.CLASS_DESTRUCTOR_ALREADY_EXIST(const Proc: TIDProcedure);
begin
  AbortWork('CLASS DESTRUCTOR already exists', Proc.TextPosition);
end;

class procedure TASTDelphiErrors.CLASS_NOT_IMPLEMENT_INTF(const Src: TIDExpression; Dest: TIDType);
begin
  AbortWork('CLASS "%s" not implement the "%s" interface', [Src.DataTypeName, Dest.DisplayName], Src.TextPosition);
end;

class procedure TASTDelphiErrors.CLASS_OR_INTF_TYPE_REQUIRED(const TextPosition: TTextPosition);
begin
  AbortWork('CLASS or INTERFACE type required', TextPosition);
end;

class procedure TASTDelphiErrors.CLASS_TYPE_REQUIRED(const TextPosition: TTextPosition);
begin
  AbortWork(sCLASSTypeRequired, TextPosition);
end;

class procedure TASTDelphiErrors.CANNOT_ACCESS_PRIVATE_MEMBER(const ID: TIdentifier);
begin
  AbortWork('Cannot access private member: %s', [ID.Name], ID.TextPosition);
end;

class procedure TASTDelphiErrors.CANNOT_ACCESS_TO_WRITEONLY_PROPERTY(const Expr: TIDExpression);
begin
  AbortWork(sCannotAccessToWriteOnlyProperty, Expr.TextPosition);
end;

class procedure TASTDelphiErrors.CANNOT_ASSIGN_NULL_TO_NOTNULL(const Src: TIDExpression);
begin
  AbortWork('Cannot assign NULLPTR to NOT NULL variable', [], Src.TextPosition);
end;

class procedure TASTDelphiErrors.CANNOT_ASSIGN_TEMPORARRY_OBJECT(Expr: TIDExpression);
begin
  AbortWork('Cannot modify a temporary object', Expr.TextPosition);
end;

procedure TASTDelphiErrors.NEED_SPECIFY_NINDEXES(const Decl: TIDDeclaration);
var
  ADataType: TIDArray;
begin
  ADataType := TIDArray(Decl.DataType);
  AbortWork(sNeedSpecifyNIndexesFmt, [Decl.DisplayName, ADataType.DisplayName, ADataType.DimensionsCount], Lexer.PrevPosition);
end;

procedure TASTDelphiErrors.IDENTIFIER_HAS_NO_MEMBERS(const Decl: TIDDeclaration);
begin
  AbortWork(sIdentifierHasNoMembersFmt, [Decl.DisplayName], Lexer.PrevPosition);
end;

class procedure TASTDelphiErrors.NO_OVERLOAD(CallExpr: TIDExpression; const CallArgs: TIDExpressions);
var
  ArgStr: string;
begin
  for var Item in CallArgs do
    ArgStr := AddStringSegment(ArgStr, Item.DataTypeName, ',');

  AbortWork(sErrorOverload + #13#10 + 'Call argumensts: ' + ArgStr +
                             #13#10 + CallExpr.AsProcedure.GetAllOverloadSignatures,
    [CallExpr.Declaration.Name], CallExpr.TextPosition);
end;

class procedure TASTDelphiErrors.AMBIGUOUS_OVERLOAD_CALL(CallExpr: TIDExpression);
begin
  AbortWork(sAmbiguousOverloadedCallFmt, [CallExpr.DisplayName], CallExpr.TextPosition);
end;

class procedure TASTDelphiErrors.INCOMPLETE_PROC(Decl: TIDDeclaration);
var
  ProcName: string;
begin
  ProcName := Decl.DisplayName;
  if Assigned(TIDProcedure(Decl).Struct) then
    ProcName := TIDProcedure(Decl).Struct.Name + '.' + ProcName;
  AbortWork(sIncompleteProcFmt, [ProcName], Decl.SourcePosition);
end;

class procedure TASTDelphiErrors.NO_OVERLOAD_OPERATOR_FOR_TYPES(Op: TOperatorID; Left, Right: TIDExpression);
begin
  AbortWork(sNoOverloadOperatorForTypesFmt, [OperatorFullName(Op), Left.DataTypeName, Right.DataTypeName], Left.TextPosition);
end;

class procedure TASTDelphiErrors.NO_OVERLOAD_OPERATOR_FOR_TYPES(Op: TOperatorID; Right: TIDExpression);
begin
  AbortWork(sNoOverloadOperatorForTypeFmt, [OperatorFullName(Op), Right.DataTypeName], Right.TextPosition);
end;

procedure TASTDelphiErrors.NO_METHOD_IN_BASE_CLASS(Proc: TIDProcedure);
begin
  AbortWork('Method %s is not found in base classes', [Proc.DisplayName], Lexer.Position);
end;

procedure TASTDelphiErrors.HINT_TEXT_AFTER_END;
begin
//  PutMessage(cmtHint, 'Text after final END. - ignored by compiler', Lexer.PrevPosition);
end;

//procedure TASTDelphiErrors.HINT_RESULT_EXPR_IS_NOT_USED(const Expr: TIDExpression);
//begin
//  if Assigned(Expr.DataType) then
//    PutMessage(cmtHint, 'Expression result (type: ' + Expr.DataType.DisplayName + ') is not used', Expr.TextPosition)
//  else
//    PutMessage(cmtHint, 'Expression result is not used', Expr.TextPosition);
//end;


end.
