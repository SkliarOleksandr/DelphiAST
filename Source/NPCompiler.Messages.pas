unit NPCompiler.Messages;

interface

resourcestring

  // internal errors
  msgUnitAlreadyExistFmt =  'Unit ''%s'' already exist';

  msgIntfSectionMissing = 'INTERFACE section are missing';
  msgKeywordExpected = 'KEYWORD expected but %s found';
  msgInternalErrorWord = 'Internal error';
  msgErrorWord = 'Error';
  msgWarningWord = 'Warning';
  msgHintWord = 'Hint';
  msgExpected = '%s expected';
  msgExpectedButFoundFmt = '%s expected but "%s" found';
  msgIdentifierExpected = 'Identifier expected';
  msgIdExpectedButFoundFmt = 'Identifier expected but "%s" found';
  msgParamNameExpectedButFoundFmt = 'Param name expected but "%s" found';
  msgTypeIdExpectedButFoundFmt = 'Type identifier expected but "%s" found';
  msgIdentifierRedeclaredFmt = 'Identifier redeclared: "%s"';
  msgUndeclaredIdentifier = 'Undeclared identifier: "%s"';
  msgDeclDifWithPrevDecl = 'Declaration of "%s" differs from previous declaration';
  msgBeginEndCountAreDiffers = 'Count of BEGIN/END clauses does not equals';
  msgDevisionByZero = 'Devision by zero';
  msgVariableRequired = 'Variable required';
  msgVariableOrTypeRequired = 'VARIABLE or TYPE required';
  msgCannotModifyObjectPassedAsConstParam = 'Cannot modify object passed as CONST parameter';
  msgConstCannotBePassedAsVarParam = 'Constant cannot be passed as VAR parameter';
  msgIncompleteProcFmt = 'Incomplete forward declaration ''%s''';
  msgVariableIsDeclaredButNeverUsedInFmt =  'Variable "%s" is declared but never used';
    // overload:
  msgOverloadedMustBeMarked = 'Overloaded entry "%s" must be marked with the "overload" directive';
  msgErrorOverload = 'There isn''t function with such parameters';
  msgAmbiguousOverloadedCallFmt = 'Ambiguous overloaded call to "%s"';
  msgInvalidIndex = 'Index is out of bounds';
  msgNotAllowedHere = 'Not allowed here: "%s"';
  msgUnexpectedEndOfFile = 'Unexpected end of file';
  msgUnknownLanguageExpression = 'Unknown language expression: "%s"';
  msgStatementExpectedButExpFoundFmt = 'Statement expected, but expression "%s" found';
  msgExpressionExpectedButStmntFoundFmt = 'Expression expected, but statement "%s" found';
  msgExpressionExpected = 'Expression expected';
  msgIncompleteStatement = 'Incomplete statement';
  msgUnnecessaryClosedBracket = 'Unnecessary closed bracket';
  msgUnnecessaryClosedBlock = 'Unnecessary closed block';
  msgUnnecessaryEndClause = 'Unnecessary end clause';
  msgUnclosedOpenBracket = 'Unclosed open bracket';
  msgUnclosedOpenBlock = 'Unclosed open block';
  msgMissingOperatorOrSemicolon = 'Missing operator or semicolon';
  msgSemicolonExpected = 'SEMICOLON expected';
  msgDublicateOperationFmt = 'Dublicate operation "%s"';
  msgDuplicateSpecificationFmt = 'Duplicate "%s" specification';
  msgReturnValueNotAllowedForProc = 'Return value is not allowed for procedure';

    // pure
  msgObjectCannotBeUsedOnPureProc = 'Object "%s" can not be used in PURE function';


    // Assignment
  msgAssignmentIsImpossible = 'The assignment is impossible, "%s" is not a variable';
  msgRightExpressionHasNoResult = 'Right expression has no result';
    // parameters
  msgNotEnoughActualParametersFmt = 'Not enough actual parameters for "%s"';
  msgTooManyActualParameters = 'Too many actual parameters';
  msgCannotPassConstAsVarParamFmt = 'Can not pass const as var parameter "%s"';
    // Types
  msgNoOverloadOperatorForTypesFmt = 'No overload operator "%s" for types "%s" and "%s"';
  msgNoOverloadOperatorForTypeFmt = 'No overload operator "%s" for type "%s"';
  msgUnknownOperatorFmt = 'Unknown operator "%s"';
  msgOperatorNeedNCountOfParameters = 'Operator "%s" need %d explicit parameters';
  msgIncompatibleTypesFmt = 'Incompatible types: "%s" and "%s"';
  msgInvalidTypecastFmt = 'Can not explicitly convert type "%s" into "%s"';
  msgEmptyExpression = 'Empty expression';
  msgExpressionMustBeBoolean = 'Type of expression must be BOOLEAN';
  msgExpressionMustBeConstant = 'Expression must be a CONSTANT';
  msgDeclarationHasNoDataTypeFmt = 'Declaration "%s" has no data type';
  msgInvalidTypeDeclaration = 'Invalid type declaration';
  msgTypeKeywordRequred = 'The TYPE keyword required';
  msgConstValueOverflowFmt = 'CONST value "%s" exceeds values range for "%s" data type';

  // uses
  msgUnitNotFoundFmt = 'Unit not found: %s';
  msgUnitRecursivelyUsesItselfFmt = 'Program or unit ''%s'' recursively uses itself';

  // loops
  msgContinueAllowedOnlyInLoop = 'continue allowed only in loop';
  msgBreakOrContinueAreAllowedOnlyInALoops = 'BREAK and CONTINUE are allowed only in a loops';
  msgLoopLevelExprected = 'Loop level exprected';
  msgLoopLevelGreaterThenPossibleFmt = 'The loop level is greater than possible, max level is %d';
  msgForLoopIndexVarsMastBeSimpleIntVar = 'For loop index variable must be local integer variable';
  msgKeywordToOrDowntoExpected = 'Кeyword TO or DOWNTO are expected';
  msgForOrWhileLoopExecutesZeroTimes = 'FOR or WHILE-loop executes zero times - deleted';
  msgZeroDeltaInForLoop = 'Zero delta in FOR-loop, infinity loop';
  msgCannotModifyForLoopIndexVarFmt = 'Cannot modify FOR-loop index variable %s';

    // records
  msgIdentifierHasNoMembersFmt = 'Identifier "%s" has no members';
  msgRecordTypeRequired = 'Record type is required';
  msgStructTypeRequired = 'Structured type is required';
  msgRecurciveTypeLinkIsNotAllowed = 'Recurcive link is not allowed';
  msgFieldConstOrFuncRequiredForGetter = 'The field, constant or function are required for a getter';
  msgFieldOrProcRequiredForSetter = 'The field or procedure are required for a setter';
  msgCannotModifyReadOnlyProperty = 'Cannot modify the read-only property';
  msgCannotAccessToWriteOnlyProperty = 'Cannot access to write-only property';
  msgSetterMustBeMethodWithSignFmt = 'Setter must be a method with signature: procedure(const Value: %s);';
  msgDefaultPropertyMustBeAnArrayProperty =  'Default property must be an array property';
  msgDefaultPropertyAlreadyExistsFmt = 'Default property already exist: "%s"';


    // classes
  msgClassTypeCannotBeAnonimous = 'CLASS type cannot be anonimous';
  msgClassTypeRequired = 'CLASS type required';


    // interfaces
  msgInterfacedTypeCannotBeAnonimous = 'Interfaced type cannot be anonimous';
  msgInterfaceTypeRequired = 'Interface type required';


    // arrays
  msgOrdinalTypeRequired = 'ORDINAL type required';
  msgOrdinalConstOrTypeRequred = 'ORDINAL constant or type required';
  msgArrayTypeRequired = 'Array type required';
  msgArrayOrStringTypeRequired = 'ARRAY or STRING type required';
  msgNeedSpecifyNIndexesFmt = 'For access to array ''%s: %s'' need specify %d indexes';
  msgConstExprOutOfRangeFmt = 'Const expression ''%d'' out of range [%d..%d]';
  msgOpenArrayAllowedOnlyAsTypeOfParam = 'Open array allowed only as type of parameter';

    // New/Free
  msgPointerTypeRequired = 'POINTER type required';
  msgReferenceTypeRequired = 'REFERENCE type required';


    // Ranges
  msgLowerBoundExceedsHigherBound = 'Lower bound exceeds higher bound';
  msgNumericTypeRequired = 'Numeric type required';
  msgConstRangeRequired = 'Const range required';


    // Enums
  msgComaOrCloseRoundExpected = ''','' or '')'' expected';

    // Expressions
  msgSingleExpressionRequired = 'Single expression required';
  msgStringExpressionRequired = 'STRING expression required';

    // Import/Export
  msgImportFuncCannotBeInline = 'Import function can not be INLINE';
  msgExportAllowsOnlyInIntfSection = 'Export allows only in INTERFACE section';

    // Interanl errors
  msgOperatorForTypesAlreadyOverloadedFmt = 'Operator ''%s'' for types ''%s'' and ''%s'' already overloaded';
  msgFeatureNotSupported = 'Feature is not supported';

    // Platform/ASM
  msgInstructionAlreadyDeclaredFmt = 'Instruction "%s" already declared';
  msgRegisterAlreadyDeclaredFmt = 'Register "%s" already declared';
  msgUnknownInstructionFmt = 'Unknown instruction "%s"';
  msgUnknownPlatformFmt = 'Unknown platform "%s"';
  msgProcedureOrFunctionKeywordAreExpected = 'PROCEDURE or FUNCTION keyword are expected';
    //sProcOrFuncRequired = 'Procedure or function required';
  msgDuplicateOpenBlock = 'Duplicate open block';
  msgASMSyntaxError = 'Assembler syntax error';
  msgDestArgCannotBeConst = 'Destination argument cannot be a constant';
  msgEmptyArgument = 'Empty argument';
  msgInvalidArgument = 'Invalid argument';
  msgLabelRedeclaretedFmt = 'Label "%s" redeclareted';
  msgImmediateOffsetIsOutOfRangeFmt = 'Immediate offset is out of range (%d..%d)';

    // IIF
  msgTypesMustBeIdentical = 'Types must be identical';
  msgThenAndElseSectionAreIdentical = 'THEN and ELSE sections are identical';

    // Try/Except/Finally
  msgTryKeywordMissed = 'TRY keyword missed';
  msgExceptOrFinallySectionWasMissed = 'EXCEPT or FINALLY section was missed';
  msgSectionFinallyAlreadyDefined = 'Section FINALLY already defined';
  msgSectionExceptAlreadyDefined = 'Section EXCEPT already defined';
  msgExceptSectionMustBeDeclareBeforeFinally = 'EXCEPT section must be declared before FINALLY section';
  msgBreakContinueExitAreNotAllowedInFinallyClause = 'BREAK, CONTINUE or EXIT are not allowed in FINALLY clause';

    // case
  msgCaseStmtRequireAtLeastOneMatchExpr = 'CASE statement require at least one match expression';
  msgMatchExprTypeMustBeIdenticalToCaseExprFmt = 'Match expression type [%s] must be identical to CASE expression type [%s]';
  msgDuplicateMatchExpression = 'Duplicate match expression';

    // addr
  msgVarOrProcRequired = 'Variable or procedure required';

  msgProcOrProcVarRequired = 'Procedure or variable of procedure type is required';

    // generics
  msgNoOneTypeParamsWasFound = 'No one type parameters was found';
  msgGenericProcInstanceErrorFmt = 'Generic procedure %s specialization ERROR:';
  msgGenericFuncInstanceErrorFmt = 'Generic function %s specialization ERROR:';
  msgProcHasNoGenericParams = 'The %s "%s" has no generic parameters';
  msgProcRequiresExplicitTypeArgumentFmt = 'The %s "%s" requires explicit type argument(s)';
  msgTooManyActualTypeParameters = 'Too many actual type arguments';



    // conditional statements
  msgInvalidConditionalStatement = 'Invalid conditional statement';

  const
  // Errors
  cKeywordExpected = 0001;
  cIdentifierExpected = 0002;
  // Warnings
  // Hints


implementation

end.

