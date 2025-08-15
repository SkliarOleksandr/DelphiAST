unit AST.Delphi.Errors;

interface

uses SysUtils,
     AST.Lexer,
     AST.Lexer.Delphi,
     AST.Intf,
     AST.Delphi.Operators,
     AST.Delphi.Declarations,
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
  sDeclDifWithPrevDecl = 'Declaration of "%s" differs from previous declaration:' + sLineBreak + '%s'  + sLineBreak + '%s';
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
  sProcedureCannotHaveResult = 'Procedure cannot have a result type';

    // Assignment
  sAssignmentIsImpossible = 'The assignment is impossible, "%s" is not a variable';
  sRightExpressionHasNoResult = 'Right expression has no result';
    // parameters
  sNotEnoughActualParametersFmt = 'Not enough actual parameters for "%s"';
  sTooManyActualParametersFmt = 'Too many actual parameters (expected: %d, actual: %d)';
  sCannotPassConstAsVarParamFmt = 'Can not pass const as var parameter "%s"';
    // Types
  sUnknownOperatorFmt = 'Unknown operator "%s"';
  sOperatorNeedNCountOfParameters = 'Operator "%s" need %d explicit parameters';
  sIncompatibleTypesFmt = 'Incompatible types: Src: "%s" and Dst: "%s"';
  sInvalidTypecastFmt = 'Can not explicitly convert type "%s" into "%s"';
  sEmptyExpression = 'Empty expression';
  sExpressionMustBeBoolean = 'Type of expression must be BOOLEAN';
  sDeclarationHasNoDataTypeFmt = 'Declaration "%s" has no data type';
  sInvalidTypeDeclarationFmt = '%s Invalid type declaration';
  sTypeKeywordRequred = 'The TYPE keyword required';
  sConstValueOverflowFmt = 'CONST value "%s" exceeds values range for "%s" data type';

  // uses
  sUnitNotFoundFmt = 'Unit not found: %s';
  sUnitRecursivelyUsesItselfFmt = 'Program or unit ''%s'' recursively uses itself';

  // loops
  sBreakOrContinueAreAllowedOnlyInALoops = 'BREAK and CONTINUE are allowed only in a loops';
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
  sNoGenericMethodWithSuchParamsFmt = 'There is no type parameterized methods of %s that can be used with these number of type parameters';

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
    class procedure INCOMPATIBLE_TYPES(const Src, Dst: TIDExpression); overload; static;
    class procedure INCOMPATIBLE_TYPES(const Src: TIDExpression; Dst: TIDType); overload; static;
    class procedure INVALID_EXPLICIT_TYPECAST(const Src: TIDExpression; Dst: TIDType); static;
    class procedure VAR_EXPRESSION_REQUIRED(Expr: TIDExpression); static;
    class procedure VAR_OR_PROC_EXPRESSION_REQUIRED(Expr: TIDExpression); static;
    class procedure CANNOT_MODIFY_FOR_LOOP_VARIABLE(Expr: TIDExpression); static;
    class procedure CANNOT_MODIFY_CONSTANT(Expr: TIDExpression); static;
    class procedure BOOLEAN_EXPRESSION_REQUIRED(Expr: TIDExpression); static;
    class procedure ARRAY_EXPRESSION_REQUIRED(Expr: TIDExpression); static;
    class procedure THE_SAME_METHOD_EXISTS(const ID: TIdentifier); overload; static;
    class procedure UNDECLARED_ID(const ID: TIdentifier); overload; static;
    class procedure UNDECLARED_ID(const ID: TIdentifier; const GenericParams: TIDTypeArray); overload; static;
    class procedure UNDECLARED_ID(const Name: string; const TextPosition: TTextPosition); overload; static;
    class procedure OVERLOADED_MUST_BE_MARKED(const ID: TIdentifier); static;

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
    class procedure PROPERTY_DOES_NOT_EXIST_IN_BASE_CLASS(const AID: TIdentifier); static;

    class procedure AMBIGUOUS_OVERLOAD_CALL(CallExpr: TIDExpression); overload; static;
    class procedure INCOMPLETE_PROC(Decl: TIDDeclaration); static;
    class procedure TYPE_NOT_COMPLETELY_DEFINED(Decl: TIDDeclaration); static;
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
    class procedure UNKNOWN_OPTION(const ID: TIdentifier);
    class procedure CANNOT_ASSIGN_TEMPORARRY_OBJECT(Expr: TIDExpression);

    class procedure CLASS_CONSTRUCTOR_ALREADY_EXIST(const Proc: TIDProcedure);
    class procedure CLASS_DESTRUCTOR_ALREADY_EXIST(const Proc: TIDProcedure);

    class procedure COULD_NOT_INFER_DYNAMIC_ARRAY_TYPE(Exprs: TIDExpressions);
    class procedure COULD_NOT_INFER_VAR_TYPE_FROM_UNTYPED(Expr: TIDExpression);

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

    procedure GENERIC_INVALID_CONSTRAINT(ActualToken: TTokenID);

    class procedure E2003_UNDECLARED_IDENTIFIER(const AModule: IASTModule; const AID: TIdentifier);
    class procedure E2004_IDENTIFIER_REDECLARED(const AModule: IASTModule; const AID: TIdentifier);
    class procedure E2005_ID_IS_NOT_A_TYPE_IDENTIFIER(const AModule: IASTModule; const AID: TIdentifier);
    class procedure E2008_INCOMPATIBLE_TYPES(const AModule: IASTModule; const ATextPosition: TTextPosition);
    class procedure E2010_INCOMPATIBLE_TYPES(const AModule: IASTModule; ALeft, ARight: TIDType; const ATextPosition: TTextPosition);
    class procedure E2015_OPERATOR_NOT_APPLICABLE_TO_THIS_OPERAND_TYPE(const AModule: IASTModule; const ATextPosition: TTextPosition);
    class procedure E2016_ARRAY_TYPE_REQUIRED(const AModule: IASTModule; const ATextPosition: TTextPosition);
    class procedure E2018_RECORD_OBJECT_OR_CLASS_TYPE_REQUIRED(const AModule: IASTModule; const ATextPosition: TTextPosition);
    class procedure E2021_CLASS_TYPE_REQUIRED(const AModule: IASTModule; const APosition: TTextPosition);
    class procedure E2022_CLASS_HELPER_TYPE_REQUIRED(const AModule: IASTModule; const APosition: TTextPosition);
    class procedure E2026_CONSTANT_EXPRESSION_EXPECTED(const AModule: IASTModule; const APosition: TTextPosition);
    class procedure E2029_ID_EXPECTED_BUT_FOUND(const AModule: IASTModule; const AActual: string; const APosition: TTextPosition);
    class procedure E2029_TOKEN_EXPECTED_BUT_ID_FOUND(const AModule: IASTModule; AExpectedToken: TTokenID; const AID: TIdentifier);
    class procedure E2029_EXPECTED_BUT_FOUND(const AModule: IASTModule; const AExpected, AActual: string; const APosition: TTextPosition);
    class procedure E2033_TYPES_OF_ACTUAL_AND_FORMAL_VAR_PARAMETER_MUST_BE_IDENTICAL(const AModule: IASTModule; const APosition: TTextPosition);
    class procedure E2034_TOO_MANY_ACTUAL_PARAMETERS(const AModule: IASTModule; const ATextPosition: TTextPosition);
    class procedure E2035_NOT_ENOUGH_ACTUAL_PARAMETERS(const AModule: IASTModule; const ATextPosition: TTextPosition);
    class procedure E2037_DECLARATION_OF_DIFFERS_FROM_PREVIOUS_DECLARATION(const AModule: IASTModule; const AID: TIdentifier);
    class procedure E2050_STATEMENTS_NOT_ALLOWED_IN_INTERFACE_PART(const AModule: IASTModule; const APosition: TTextPosition);
    class procedure E2064_LEFT_SIDE_CANNOT_BE_ASSIGNED_TO(const AModule: IASTModule; const APosition: TTextPosition);
    class procedure E2066_MISSING_OPERATOR_OR_SEMICOLON(const AModule: IASTModule; const APosition: TTextPosition);
    class procedure E2075_THIS_FORM_OF_METHOD_CALL_ONLY_ALLOWED_IN_METHODS_OF_DERIVED_TYPES(const AModule: IASTModule; const APosition: TTextPosition);
    class procedure E2086_TYPE_IS_NOT_YET_COMPLETELY_DEFINED(const AModule: IASTModule; const AID: TIdentifier);
    class procedure E2089_INVALID_TYPECAST(const AModule: IASTModule; const ATextPosition: TTextPosition);
    class procedure E2137_METHOD_NOT_FOUND_IN_BASE_CLASS(const AModule: IASTModule; const AID: TIdentifier);
    class procedure E2168_FIELD_OR_METHOD_IDENTIFIER_EXPECTED(const AModule: IASTModule; const APosition: TTextPosition);
    class procedure E2169_FIELD_DEFINITION_NOT_ALLOWED_AFTER_METHODS_OR_PROPERTIES(const AModule: IASTModule; const APosition: TTextPosition);
    class procedure E2170_CANNOT_OVERRIDE_A_NON_VIRTUAL_METHOD(const AModule: IASTModule; const ATextPosition: TTextPosition);
    class procedure E2185_CANNOT_SPECIFY_DISPID(const AModule: IASTModule; const AMethodID: TIdentifier);
    class procedure E2196_CANNOT_INIT_MULTIPLE_VARS(const ATextPosition: TTextPosition); static;
    class procedure E2197_CONSTANT_OBJECT_CANNOT_BE_PASSED_AS_VAR_PARAMETER(const AModule: IASTModule; const APosition: TTextPosition);
    class procedure E2205_INTERFACE_TYPE_REQUIRED(const AModule: IASTModule; const APosition: TTextPosition); static;
    class procedure E2250_NO_OVERLOADED_PROC_FOR_THESE_ARGUMENTS(const AModule: IASTModule; AExpression: TIDExpression); overload;
    class procedure E2250_NO_OVERLOADED_PROC_FOR_THESE_ARGUMENTS(const AModule: IASTModule; ACallExpr: TIDCallExpression); overload;
    class procedure E2251_AMBIGUOUS_OVERLOADED_CALL(const AModule: IASTModule; AExpression: TIDExpression);
    class procedure E2258_IMPLEMENTS_CLAUSE_ONLY_ALLOWED_WITHIN_CLASS_TYPES(const AModule: IASTModule; const APosition: TTextPosition);
    class procedure E2265_INTERFACE_NOT_MENTIONED_IN_INTERFACE_LIST(const AModule: IASTModule; const AID: TIdentifier);
    class procedure E2232_INTERFACE_HAS_NO_INTERFACE_IDENTIFICATION(const AModule: IASTModule; ADecl: TIDDeclaration);
    class procedure E2430_FOR_IN_STATEMENT_CANNOT_OPERATE_ON_COLLECTION_TYPE(const AModule: IASTModule; const ATypeName: string; const ATextPosition: TTextPosition);
    class procedure E2436_TYPE_NOT_ALLOWED_IN_ANONYMOUS_OR_LOCAL_RECORD_TYPE(const AModule: IASTModule; const ATextPosition: TTextPosition);
    class procedure E2529_TYPE_PARAMETERS_NOT_ALLOWED_ON_OPERATOR(const AModule: IASTModule; const ATextPosition: TTextPosition);
    class procedure E2530_TYPE_PARAMETERS_NOT_ALLOWED_ON_GLOBAL_PROCEDURE_OR_FUNCTION(const AModule: IASTModule; const ATextPosition: TTextPosition);
    class procedure E2531_METHOD_REQUIRES_EXPLICIT_TYPE_ARGUMENTS(const AModule: IASTModule; const AName: string; const APosition: TTextPosition);
    class procedure E2535_INTERFACE_METHODS_MUST_NOT_HAVE_PARAMETERIZED_METHODS(const AModule: IASTModule; const ATextPosition: TTextPosition);
    class procedure E2599_FIELD_DEFINITION_NOT_ALLOWED_IN_HELPER_TYPE(const AModule: IASTModule; const APosition: TTextPosition);

    procedure PROCEDURE_CANNOT_HAVE_RESULT;
    procedure BREAK_OR_CONTINUE_ALLOWED_ONLY_IN_LOOPS;
    procedure DEFAULT_PROP_MUST_BE_ARRAY_PROP;
    procedure DEFAULT_PROP_ALREADY_EXIST(Prop: TIDProperty);
    procedure IMPORT_FUNCTION_CANNOT_BE_INLINE;
    class procedure INVALID_TYPE_DECLARATION(const AModule: IASTModule; const ID: TIdentifier); overload;
    class procedure INVALID_TYPE_DECLARATION(const AModule: IASTModule; const APosition: TTextPosition); overload;
    procedure EXPECTED_TOKEN(Token: TTokenID; ActulToken: TTokenID = token_unknown);
    procedure EXPECTED_KEYWORD_OR_ID;
    procedure IDENTIFIER_EXPECTED(ActualToken: TTokenID); overload;
    procedure IDENTIFIER_EXPECTED; overload;
    procedure PARAM_NAME_ID_EXPECTED(ActualToken: TTokenID);
    procedure SEMICOLON_EXPECTED;
    procedure EMPTY_EXPRESSION;
    procedure END_OF_FILE;
    procedure EXPRESSION_EXPECTED;
    procedure FEATURE_NOT_SUPPORTED(const AModule: IASTModule; const UnsupportedKeyword: string = '');
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

  procedure STATIC_ASSERT_ERROR(const AModule: IASTModule; const ATextPosition: TTextPosition; const AMessage: string);

  procedure INTERNAL_ERROR(const AModule: IASTModule; const AMessage: string; const APosition: TTextPosition); overload;
  procedure INTERNAL_ERROR(const AModule: IASTModule; const AMessage: string; AParams: array of const; const
                           APosition: TTextPosition); overload;

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

class procedure TASTDelphiErrors.CONST_VALUE_OVERFLOW(Expr: TIDExpression; DstDataType: TIDType);
begin
  AbortWork(sConstValueOverflowFmt, [Expr.DisplayName, DstDataType.DisplayName], Expr.TextPosition);
end;

class procedure TASTDelphiErrors.COULD_NOT_INFER_DYNAMIC_ARRAY_TYPE(Exprs: TIDExpressions);
begin
  AbortWork('Couldn''t infer dynamic array element type from different element types', Exprs[0].TextPosition);
end;

class procedure TASTDelphiErrors.COULD_NOT_INFER_VAR_TYPE_FROM_UNTYPED(Expr: TIDExpression);
begin
  AbortWork('Couldn''t infer variable/constant type from untyped', Expr.TextPosition);
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

procedure TASTDelphiErrors.BREAK_OR_CONTINUE_ALLOWED_ONLY_IN_LOOPS;
begin
  AbortWork(sBreakOrContinueAreAllowedOnlyInALoops, Lexer.Position);
end;

class procedure TASTDelphiErrors.OVERLOADED_MUST_BE_MARKED(const ID: TIdentifier);
begin
  AbortWork(sOverloadedMustBeMarked, [ID.Name], ID.TextPosition);
end;

procedure TASTDelphiErrors.DEFAULT_PROP_ALREADY_EXIST(Prop: TIDProperty);
begin
  AbortWork(sDefaultPropertyAlreadyExistsFmt, [Prop.Name], Prop.TextPosition);
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

class procedure TASTDelphiErrors.THE_SAME_METHOD_EXISTS(const ID: TIdentifier);
begin
  AbortWork('Method ''%s'' with identical parameters already exists', [ID.Name], ID.TextPosition);
end;

procedure TASTDelphiErrors.SEMICOLON_EXPECTED();
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
                                                 Lexer.OriginalToken], Lexer.PrevPosition);
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

class procedure TASTDelphiErrors.E2003_UNDECLARED_IDENTIFIER(const AModule: IASTModule; const AID: TIdentifier);
begin
  AModule.PutError('E2003 Undeclared identifier: ''%s''', [AID.Name], AID.TextPosition);
end;

class procedure TASTDelphiErrors.E2004_IDENTIFIER_REDECLARED(const AModule: IASTModule; const AID: TIdentifier);
begin
  AModule.PutError('E2004 Identifier redeclared: ''%s''', [AID.Name], AID.TextPosition);
end;

class procedure TASTDelphiErrors.E2005_ID_IS_NOT_A_TYPE_IDENTIFIER(const AModule: IASTModule; const AID: TIdentifier);
begin
  AModule.PutError('E2005 ''%s'' is not a type identifier', [AID.Name], AID.TextPosition);
end;

class procedure TASTDelphiErrors.E2008_INCOMPATIBLE_TYPES(const AModule: IASTModule; const ATextPosition: TTextPosition);
begin
  AModule.PutError('E2008 Incompatible types', ATextPosition);
end;

class procedure TASTDelphiErrors.E2010_INCOMPATIBLE_TYPES(const AModule: IASTModule; ALeft, ARight: TIDType;
  const ATextPosition: TTextPosition);
begin
  AModule.PutError('E2010 Incompatible types: ''%s'' and ''%s''', [ALeft.DisplayName, ARight.DisplayName], ATextPosition);
end;

class procedure TASTDelphiErrors.E2015_OPERATOR_NOT_APPLICABLE_TO_THIS_OPERAND_TYPE(const AModule: IASTModule; const ATextPosition: TTextPosition);
begin
  AModule.PutError('E2015 Operator not applicable to this operand type', ATextPosition);
end;

class procedure TASTDelphiErrors.E2016_ARRAY_TYPE_REQUIRED(const AModule: IASTModule; const ATextPosition: TTextPosition);
begin
  AModule.PutError('E2016 Array type required', ATextPosition);
end;

class procedure TASTDelphiErrors.E2018_RECORD_OBJECT_OR_CLASS_TYPE_REQUIRED(const AModule: IASTModule; const ATextPosition: TTextPosition);
begin
  AModule.PutError('E2018 Record, object or class type required', ATextPosition);
end;

class procedure TASTDelphiErrors.E2021_CLASS_TYPE_REQUIRED(const AModule: IASTModule; const APosition: TTextPosition);
begin
  AModule.PutError('E2022 Class type required', APosition);
end;

class procedure TASTDelphiErrors.E2022_CLASS_HELPER_TYPE_REQUIRED(const AModule: IASTModule; const APosition: TTextPosition);
begin
  AModule.PutError('E2022 Class helper type required', APosition);
end;

class procedure TASTDelphiErrors.E2026_CONSTANT_EXPRESSION_EXPECTED(const AModule: IASTModule; const APosition: TTextPosition);
begin
  AModule.PutError('E2026 Constant expression expected', APosition);
end;

class procedure TASTDelphiErrors.E2029_EXPECTED_BUT_FOUND(const AModule: IASTModule; const AExpected, AActual: string;
  const APosition: TTextPosition);
begin
  AModule.PutError('E2029 ''%s'' expected but ''%s'' found', [AExpected, AActual], APosition, {ACritical:} True);
end;

class procedure TASTDelphiErrors.E2029_ID_EXPECTED_BUT_FOUND(const AModule: IASTModule; const AActual: string;
  const APosition: TTextPosition);
begin
  AModule.PutError('E2029 Identifier expected but ''%s'' found', [AActual], APosition, {ACritical:} True);
end;

class procedure TASTDelphiErrors.E2029_TOKEN_EXPECTED_BUT_ID_FOUND(const AModule: IASTModule; AExpectedToken: TTokenID; const AID: TIdentifier);
begin
  E2029_EXPECTED_BUT_FOUND(AModule, AModule.Lexer_TokenText(Ord(AExpectedToken)), AID.Name, AID.TextPosition);
end;

class procedure TASTDelphiErrors.E2033_TYPES_OF_ACTUAL_AND_FORMAL_VAR_PARAMETER_MUST_BE_IDENTICAL(const AModule: IASTModule;
  const APosition: TTextPosition);
begin
  AModule.PutError('E2033 Types of actual and formal var parameters must be identical', APosition);
end;

class procedure TASTDelphiErrors.E2034_TOO_MANY_ACTUAL_PARAMETERS(const AModule: IASTModule;
  const ATextPosition: TTextPosition);
begin
  AModule.PutError('E2034 Too many actual parameters', ATextPosition);
end;

class procedure TASTDelphiErrors.E2035_NOT_ENOUGH_ACTUAL_PARAMETERS(const AModule: IASTModule; const ATextPosition: TTextPosition);
begin
  AModule.PutError('E2035 Not enough actual parameters', ATextPosition);
end;

class procedure TASTDelphiErrors.E2037_DECLARATION_OF_DIFFERS_FROM_PREVIOUS_DECLARATION(const AModule: IASTModule; const AID: TIdentifier);
begin
  AModule.PutError('E2037 Declaration of ''%s'' differs from previous declaration', [AID.Name], AID.TextPosition);
end;

class procedure TASTDelphiErrors.E2050_STATEMENTS_NOT_ALLOWED_IN_INTERFACE_PART(const AModule: IASTModule;
  const APosition: TTextPosition);
begin
  AModule.PutError('E2050 Statements not allowed in interface part', APosition);
end;

class procedure TASTDelphiErrors.E2064_LEFT_SIDE_CANNOT_BE_ASSIGNED_TO(const AModule: IASTModule;
  const APosition: TTextPosition);
begin
  AModule.PutError('E2064 Left side cannot be assigned to', APosition);
end;

class procedure TASTDelphiErrors.E2066_MISSING_OPERATOR_OR_SEMICOLON(const AModule: IASTModule; const APosition: TTextPosition);
begin
  AModule.PutError('E2066 Missing operator or semicolon', APosition);
end;

class procedure TASTDelphiErrors.E2075_THIS_FORM_OF_METHOD_CALL_ONLY_ALLOWED_IN_METHODS_OF_DERIVED_TYPES(
  const AModule: IASTModule; const APosition: TTextPosition);
begin
  AModule.PutError('E2075 This form of method call only allowed in methods of derived types', APosition);
end;

class procedure TASTDelphiErrors.E2086_TYPE_IS_NOT_YET_COMPLETELY_DEFINED(const AModule: IASTModule; const AID: TIdentifier);
begin
  AModule.PutError('E2086 Type ''%s'' is not yet completely defined', [AID.Name], AID.TextPosition);
end;

class procedure TASTDelphiErrors.E2089_INVALID_TYPECAST(const AModule: IASTModule;const ATextPosition: TTextPosition);
begin
  AModule.PutError('E2089 Invalid typecast', ATextPosition);
end;

class procedure TASTDelphiErrors.E2137_METHOD_NOT_FOUND_IN_BASE_CLASS(const AModule: IASTModule; const AID: TIdentifier);
begin
  AModule.PutError('E2137 Method ''%s'' not found in base class', [AID.Name], AID.TextPosition);
end;

class procedure TASTDelphiErrors.E2168_FIELD_OR_METHOD_IDENTIFIER_EXPECTED(const AModule: IASTModule;
  const APosition: TTextPosition);
begin
  AModule.PutError('E2168 Field or method identifier expected', APosition);
end;

class procedure TASTDelphiErrors.E2169_FIELD_DEFINITION_NOT_ALLOWED_AFTER_METHODS_OR_PROPERTIES(const AModule: IASTModule;
  const APosition: TTextPosition);
begin
  AModule.PutError('E2169 Field definition not allowed after methods or properties', APosition);
end;

class procedure TASTDelphiErrors.E2170_CANNOT_OVERRIDE_A_NON_VIRTUAL_METHOD(const AModule: IASTModule;
  const ATextPosition: TTextPosition);
begin
  AModule.PutError('E2170 Cannot override a non-virtual method', ATextPosition);
end;

class procedure TASTDelphiErrors.E2185_CANNOT_SPECIFY_DISPID(const AModule: IASTModule; const AMethodID: TIdentifier);
begin
  AModule.PutError('E2185 Overriding automated virtual method ''%s'' cannot specify a dispid',
    [AMethodID.Name], AMethodID.TextPosition);
end;

class procedure TASTDelphiErrors.E2196_CANNOT_INIT_MULTIPLE_VARS(const ATextPosition: TTextPosition);
begin
  AbortWork('E2196 Cannot initialize multiple variables', ATextPosition);
end;

class procedure TASTDelphiErrors.E2197_CONSTANT_OBJECT_CANNOT_BE_PASSED_AS_VAR_PARAMETER(const AModule: IASTModule;
  const APosition: TTextPosition);
begin
  AModule.PutError('E2197 Constant object cannot be passed as var parameter', APosition);
end;

class procedure TASTDelphiErrors.E2205_INTERFACE_TYPE_REQUIRED(const AModule: IASTModule; const APosition: TTextPosition);
begin
  AModule.PutError('E2205 Interface type required', APosition);
end;

class procedure TASTDelphiErrors.E2232_INTERFACE_HAS_NO_INTERFACE_IDENTIFICATION(const AModule: IASTModule; ADecl: TIDDeclaration);
begin
  AModule.PutError('E2232 Interface ''%s'' has no interface identification', [ADecl.Name], ADecl.TextPosition);
end;

class procedure TASTDelphiErrors.E2250_NO_OVERLOADED_PROC_FOR_THESE_ARGUMENTS(
  const AModule: IASTModule; AExpression: TIDExpression);
begin
  AModule.PutError('E2250 There is no overloaded version of ''%s'' that can be called with these arguments',
    [AExpression.Declaration.Name], AExpression.TextPosition);
end;

class procedure TASTDelphiErrors.E2250_NO_OVERLOADED_PROC_FOR_THESE_ARGUMENTS(const AModule: IASTModule;
  ACallExpr: TIDCallExpression);
var
  LArgsStr: string;
begin
  for var LArg in ACallExpr.Arguments do
    LArgsStr := AddStringSegment(LArgsStr, LArg.DataTypeName, ', ');

  AModule.PutError('E2250 There is no overloaded version of ''%s'' that can be called with these arguments' +
                    sLineBreak + 'Call argumensts: (' + LArgsStr + ')' +
                    sLineBreak + ACallExpr.AsProcedure.GetAllOverloadSignatures,

    [ACallExpr.Declaration.Name], ACallExpr.TextPosition);
end;

class procedure TASTDelphiErrors.E2251_AMBIGUOUS_OVERLOADED_CALL(const AModule: IASTModule; AExpression: TIDExpression);
begin
  AModule.PutError('E2251 Ambiguous overloaded call to ''%s''', [AExpression.Declaration.Name], AExpression.TextPosition);
end;

class procedure TASTDelphiErrors.E2258_IMPLEMENTS_CLAUSE_ONLY_ALLOWED_WITHIN_CLASS_TYPES(const AModule: IASTModule;
  const APosition: TTextPosition);
begin
  AModule.PutError('E2258 Implements clause only allowed within class types', APosition);
end;

class procedure TASTDelphiErrors.E2265_INTERFACE_NOT_MENTIONED_IN_INTERFACE_LIST(const AModule: IASTModule;
  const AID: TIdentifier);
begin
  AModule.PutError('E2265 Interface ''%s'' not mentioned in interface list', [AID.Name], AID.TextPosition);
end;

class procedure TASTDelphiErrors.E2430_FOR_IN_STATEMENT_CANNOT_OPERATE_ON_COLLECTION_TYPE(const AModule: IASTModule;
  const ATypeName: string; const ATextPosition: TTextPosition);
begin
  AModule.PutError('E2430 for-in statement cannot operate on collection type ''%s''',
    [ATypeName], ATextPosition);
end;

class procedure TASTDelphiErrors.E2436_TYPE_NOT_ALLOWED_IN_ANONYMOUS_OR_LOCAL_RECORD_TYPE(
  const AModule: IASTModule; const ATextPosition: TTextPosition);
begin
  AModule.PutError('E2436 Type declarations not allowed in anonymous record or local record type', ATextPosition);
end;

class procedure TASTDelphiErrors.E2529_TYPE_PARAMETERS_NOT_ALLOWED_ON_OPERATOR(const AModule: IASTModule;
  const ATextPosition: TTextPosition);
begin
  AModule.PutError('E2529 Type parameters not allowed on operator', ATextPosition);
end;

class procedure TASTDelphiErrors.E2530_TYPE_PARAMETERS_NOT_ALLOWED_ON_GLOBAL_PROCEDURE_OR_FUNCTION(
  const AModule: IASTModule; const ATextPosition: TTextPosition);
begin
  AModule.PutError('E2530 Type parameters not allowed on global procedure or function', ATextPosition);
end;

class procedure TASTDelphiErrors.E2531_METHOD_REQUIRES_EXPLICIT_TYPE_ARGUMENTS(const AModule: IASTModule;
  const AName: string; const APosition: TTextPosition);
begin
  AModule.PutError('E2531 Method ''%s'' requires explicit type argument(s)', [AName], APosition);
end;

class procedure TASTDelphiErrors.E2535_INTERFACE_METHODS_MUST_NOT_HAVE_PARAMETERIZED_METHODS(
  const AModule: IASTModule; const ATextPosition: TTextPosition);
begin
  AModule.PutError('E2535 Interface methods must not have parameterized methods', ATextPosition);
end;

class procedure TASTDelphiErrors.E2599_FIELD_DEFINITION_NOT_ALLOWED_IN_HELPER_TYPE(const AModule: IASTModule;
  const APosition: TTextPosition);
begin
  AModule.PutError('E2599 Field definition not allowed in helper type', APosition);
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

procedure TASTDelphiErrors.FEATURE_NOT_SUPPORTED(const AModule: IASTModule; const UnsupportedKeyword: string);
begin
  if UnsupportedKeyword <> '' then
    AModule.PutError(sFeatureNotSupported + ': ' + UnsupportedKeyword, Lexer.Position)
  else
    AModule.PutError(sFeatureNotSupported, Lexer.Position);
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

class procedure TASTDelphiErrors.INVALID_TYPE_DECLARATION(const AModule: IASTModule; const ID: TIdentifier);
begin
  AModule.PutError(sInvalidTypeDeclarationFmt, [ID.Name], ID.TextPosition);
end;

class procedure TASTDelphiErrors.INVALID_TYPE_DECLARATION(const AModule: IASTModule; const APosition: TTextPosition);
begin
  AModule.PutError('Invalid type declaration', APosition);
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

class procedure TASTDelphiErrors.UNDECLARED_ID(const ID: TIdentifier; const GenericParams: TIDTypeArray);
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
    PS_FASTCALL: SpecName := 'FASTCALL';
    PS_REGISTER: SpecName := 'REGISTER';
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

class procedure TASTDelphiErrors.TYPE_NOT_COMPLETELY_DEFINED(Decl: TIDDeclaration);
begin
  AbortWork('Type ''%s'' is not yet completely defined', [Decl.DisplayName], Decl.TextPosition);
end;

class procedure TASTDelphiErrors.TYPE_REQUIRED(const TextPosition: TTextPosition);
begin
  AbortWork('TYPE specification required', TextPosition);
end;

class procedure TASTDelphiErrors.SETTER_MUST_BE_SUCH(const DeclString: string; const TextPosition: TTextPosition);
begin
  AbortWork('Setter must have declaration: %s', [DeclString], TextPosition);
end;

procedure TASTDelphiErrors.GENERIC_INVALID_CONSTRAINT(ActualToken: TTokenID);
begin
  AbortWork('Invalid generic constraint: %s', [Lexer.TokenLexem(ActualToken)], Lexer.Position);
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

procedure TASTDelphiErrors.PROCEDURE_CANNOT_HAVE_RESULT;
begin
  AbortWork(sProcedureCannotHaveResult, Lexer.Position);
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

class procedure TASTDelphiErrors.PROPERTY_DOES_NOT_EXIST_IN_BASE_CLASS(const AID: TIdentifier);
begin
  AbortWork('Property ''%s'' does not exist in base class', [AID.Name], AID.TextPosition);
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

procedure STATIC_ASSERT_ERROR(const AModule: IASTModule; const ATextPosition: TTextPosition; const AMessage: string);
begin
  AModule.PutError(AMessage, ATextPosition);
end;

procedure INTERNAL_ERROR(const AModule: IASTModule; const AMessage: string; const APosition: TTextPosition);
begin
  AModule.PutError(AMessage, APosition, {ACritical:} True);
end;

procedure INTERNAL_ERROR(const AModule: IASTModule; const AMessage: string; AParams: array of const; const
                         APosition: TTextPosition);
begin
  AModule.PutError(AMessage, AParams, APosition, {ACritical:} True);
end;


end.
