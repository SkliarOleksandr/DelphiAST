unit NPCompiler.Errors;

interface

type
  TCompilerError = (
    cUnitAlreadyExistFmt,
    sIntfSectionMissing,
    sKeywordExpected,
    sInternalErrorWord,
    sErrorWord,
    sWarningWord,
    sHintWord,
    sExpected,
    sExpectedButFoundFmt,
    sIdentifierExpected,
    sIdExpectedButFoundFmt,
    sParamNameExpectedButFoundFmt,
    sTypeIdExpectedButFoundFmt,
    sIdentifierRedeclared,
    sUndeclaredIdentifier,
    sDeclDifWithPrevDecl,
    sBeginEndCountAreDiffers,
    sDevisionByZero,
    sVariableRequired,
    sVariableOrTypeRequired,
    sCannotModifyObjectPassedAsConstParam,
    sConstCannotBePassedAsVarParam,
    sIncompleteProcFmt,
    sVariableIsDeclaredButNeverUsedInFmt,
    // overload:
    sOverloadedMustBeMarked,
    sErrorOverload,
    sAmbiguousOverloadedCallFmt,
    sInvalidIndex,
    sNotAllowedHere,
    sUnexpectedEndOfFile,
    sUnknownLanguageExpression,
    sStatementExpectedButExpFoundFmt,
    sExpressionExpectedButStmntFoundFmt,
    sExpressionExpected,
    sIncompleteStatement,
    sUnnecessaryClosedBracket,
    sUnnecessaryClosedBlock,
    sUnnecessaryEndClause,
    sUnclosedOpenBracket,
    sUnclosedOpenBlock,
    sMissingOperatorOrSemicolon,
    sSemicolonExpected,
    sDublicateOperationFmt,
    sDuplicateSpecificationFmt,
    sReturnValueNotAllowedForProc,

    // pure
    sObjectCannotBeUsedOnPureProc,


    // Assignment
    sAssignmentIsImpossible,
    sRightExpressionHasNoResult,
    // parameters
    sNotEnoughActualParametersFmt,
    sTooManyActualParameters,
    sCannotPassConstAsVarParamFmt,
    errParameterTypeRequred,
    // Types
    sNoOverloadOperatorForTypesFmt,
    sNoOverloadOperatorForTypeFmt,
    sUnknownOperatorFmt,
    sOperatorNeedNCountOfParameters,
    sIncompatibleTypesFmt,
    sInvalidTypecast,
    sEmptyExpression,
    sExpressionMustBeBoolean,
    sExpressionMustBeConstant,
    sDeclarationHasNoDataTypeFmt,
    errInvalidTypeDeclaration,
    sTypeKeywordRequred,
    errConstValueOverflow,

    //units
    errUnitNotFoundFmt,
    errUnitRecursivelyUsesItselfFmt,


    // loops
    sContinueAllowedOnlyInLoop,
    sBreakOrContinueAreAllowedOnlyInALoops,
    sLoopLevelExprected,
    sLoopLevelGreaterThenPossibleFmt,
    sForLoopIndexVarsMastBeSimpleIntVar,
    sKeywordToOrDowntoExpected,
    sForOrWhileLoopExecutesZeroTimes,
    sZeroDeltaInForLoop,
    sCannotModifyForLoopIndexVarFmt,

    // records
    sIdentifierHasNoMembersFmt,
    sRecordTypeRequired,
    sStructTypeRequired,
    sRecurciveTypeLinkIsNotAllowed,
    sFieldConstOrFuncRequiredForGetter,
    sFieldOrProcRequiredForSetter,
    sCannotModifyReadOnlyProperty,
    sCannotAccessToWriteOnlyProperty,
    sSetterMustBeMethodWithSignFmt,
    errorDefaultPropertyMustBeAnArrayProperty,
    errorDefaultPropertyAlreadyExistsFmt,


    // classes
    sClassTypeCannotBeAnonimous,
    errCLASSTypeRequired,


    // interfaces
    sInterfacedTypeCannotBeAnonimous,
    sInterfaceTypeRequired,


    // arrays
    sOrdinalTypeRequired,
    sOrdinalConstOrTypeRequred,
    sArrayTypeRequired,
    sArrayOrStringTypeRequired,
    sNeedSpecifyNIndexesFmt,
    sConstExprOutOfRangeFmt,
    sOpenArrayAllowedOnlyAsTypeOfParam,

    // New/Free
    sPointerTypeRequired,
    sReferenceTypeRequired,


    // Ranges
    sLowerBoundExceedsHigherBound,
    sNumericTypeRequired,
    sConstRangeRequired,

    // Enums
    sComaOrCloseRoundExpected,

    // Expressions
    sSingleExpressionRequired,
    errStringExpressionRequired,

    // Import/Export
    sImportFuncCannotBeInline,
    sExportAllowsOnlyInIntfSection,

    // Interanl errors
    sOperatorAlreadyOverloadedFmt,
    sFeatureNotSupported,

    // Platform/ASM
    sInstructionAlreadyDeclaredFmt,
    sRegisterAlreadyDeclaredFmt,
    sUnknownInstructionFmt,
    sUnknownPlatformFmt,
    sProcedureOrFunctionKeywordAreExpected,

    sDuplicateOpenBlock,
    sASMSyntaxError,
    sDestArgCannotBeConst,
    sEmptyArgument,
    sInvalidArgument,
    sLabelRedeclaretedFmt,
    sImmediateOffsetIsOutOfRangeFmt,

    // IIF
    sTypesMustBeIdentical,
    sThenAndElseSectionAreIdentical,

    // Try/Except/Finally
    sTryKeywordMissed,
    sExceptOrFinallySectionWasMissed,
    sSectionFinallyAlreadyDefined,
    sSectionExceptAlreadyDefined,
    sExceptSectionMustBeDeclareBeforeFinally,
    sBreakContinueExitAreNotAllowedInFinallyClause,

    // case
    sCaseStmtRequireAtLeastOneMatchExpr,
    sMatchExprTypeMustBeIdenticalToCaseExprFmt,
    sDuplicateMatchExpression,

    // addr
    sVarOrProcRequired,

    sProcOrProcVarRequired,

    // generics
    sNoOneTypeParamsWasFound,
    sGenericProcInstanceErrorFmt,
    sGenericFuncInstanceErrorFmt,
    sProcHasNoGenericParams,
    sProcRequiresExplicitTypeArgumentFmt,
    sTooManyActualTypeParameters,

    // conditional statements
    sInvalidConditionalStatement
  );

  function GetErrorText(const Error: TCompilerError): string;

implementation

uses SysUtils, TypInfo, NPCompiler.Messages;

var
  ERRORS: array [TCompilerError] of string;

function GetErrorText(const Error: TCompilerError): string;
begin
  Result := ERRORS[Error];
  if Result = '' then
    Result := 'ERROR DESCRIPTION NOT FOUND FOR: ' + TypInfo.GetEnumName(TypeInfo(TCompilerError), ord(Error));
end;

procedure ERROR_TEXT(Error: TCompilerError; const ErrorText: string);
var
  Str: string;
begin
  Str := ERRORS[Error];
  if Str <> '' then
    raise Exception.Create('ERROR is already described');
  ERRORS[Error] := ErrorText;
end;

initialization

    ERROR_TEXT(sExpected, msgExpected);
    ERROR_TEXT(cUnitAlreadyExistFmt, msgUnitAlreadyExistFmt);
    ERROR_TEXT(sIntfSectionMissing, msgIntfSectionMissing);
    ERROR_TEXT(sKeywordExpected, msgKeywordExpected);
    ERROR_TEXT(sExpectedButFoundFmt, msgExpectedButFoundFmt);
    ERROR_TEXT(sIdentifierExpected, msgIdentifierExpected);
    ERROR_TEXT(sIdExpectedButFoundFmt, msgIdExpectedButFoundFmt);
    ERROR_TEXT(sParamNameExpectedButFoundFmt, msgParamNameExpectedButFoundFmt);
    ERROR_TEXT(sTypeIdExpectedButFoundFmt, msgTypeIdExpectedButFoundFmt);
    ERROR_TEXT(sIdentifierRedeclared, msgIdentifierRedeclaredFmt);
    ERROR_TEXT(sUndeclaredIdentifier, msgUndeclaredIdentifier);
    ERROR_TEXT(sDeclDifWithPrevDecl, msgDeclDifWithPrevDecl);
    ERROR_TEXT(sBeginEndCountAreDiffers, msgBeginEndCountAreDiffers);
    ERROR_TEXT(sDevisionByZero, msgDevisionByZero);
    ERROR_TEXT(sVariableRequired, msgVariableRequired);
    ERROR_TEXT(sVariableOrTypeRequired, msgVariableOrTypeRequired);
    ERROR_TEXT(sCannotModifyObjectPassedAsConstParam, msgCannotModifyObjectPassedAsConstParam);
    ERROR_TEXT(sConstCannotBePassedAsVarParam, msgConstCannotBePassedAsVarParam);
    ERROR_TEXT(sIncompleteProcFmt, msgIncompleteProcFmt);
    ERROR_TEXT(sVariableIsDeclaredButNeverUsedInFmt, msgVariableIsDeclaredButNeverUsedInFmt);
    // overload:
    ERROR_TEXT(sOverloadedMustBeMarked, msgOverloadedMustBeMarked);
    ERROR_TEXT(sErrorOverload, msgErrorOverload);
    ERROR_TEXT(sAmbiguousOverloadedCallFmt, msgAmbiguousOverloadedCallFmt);
    ERROR_TEXT(sInvalidIndex, msgInvalidIndex);
    ERROR_TEXT(sNotAllowedHere, msgNotAllowedHere);
    ERROR_TEXT(sUnexpectedEndOfFile, msgUnexpectedEndOfFile);
    ERROR_TEXT(sUnknownLanguageExpression, msgUnknownLanguageExpression);
    ERROR_TEXT(sStatementExpectedButExpFoundFmt, msgStatementExpectedButExpFoundFmt);
    ERROR_TEXT(sExpressionExpectedButStmntFoundFmt, msgExpressionExpectedButStmntFoundFmt);
    ERROR_TEXT(sExpressionExpected, msgExpressionExpected);
    ERROR_TEXT(sIncompleteStatement, msgIncompleteStatement);
    ERROR_TEXT(sUnnecessaryClosedBracket, msgUnnecessaryClosedBracket);
    ERROR_TEXT(sUnnecessaryClosedBlock, msgUnnecessaryClosedBlock);
    ERROR_TEXT(sUnnecessaryEndClause, msgUnnecessaryEndClause);
    ERROR_TEXT(sUnclosedOpenBracket, msgUnclosedOpenBracket);
    ERROR_TEXT(sUnclosedOpenBlock, msgUnclosedOpenBlock);
    ERROR_TEXT(sMissingOperatorOrSemicolon, msgMissingOperatorOrSemicolon);
    ERROR_TEXT(sSemicolonExpected, msgSemicolonExpected);
    ERROR_TEXT(sDublicateOperationFmt, msgDublicateOperationFmt);
    ERROR_TEXT(sDuplicateSpecificationFmt, msgDuplicateSpecificationFmt);
    ERROR_TEXT(sReturnValueNotAllowedForProc, msgReturnValueNotAllowedForProc);

    // pure
    ERROR_TEXT(sObjectCannotBeUsedOnPureProc, msgObjectCannotBeUsedOnPureProc);

    // Assignment
    ERROR_TEXT(sAssignmentIsImpossible, msgAssignmentIsImpossible);
    ERROR_TEXT(sRightExpressionHasNoResult, msgRightExpressionHasNoResult);
    // parameters
    ERROR_TEXT(sNotEnoughActualParametersFmt, msgNotEnoughActualParametersFmt);
    ERROR_TEXT(sTooManyActualParameters, msgTooManyActualParameters);
    ERROR_TEXT(sCannotPassConstAsVarParamFmt, msgCannotPassConstAsVarParamFmt);
    ERROR_TEXT(errParameterTypeRequred, 'Parameter type required');

    // Types
    ERROR_TEXT(sNoOverloadOperatorForTypesFmt, msgNoOverloadOperatorForTypesFmt);
    ERROR_TEXT(sNoOverloadOperatorForTypeFmt, msgNoOverloadOperatorForTypeFmt);
    ERROR_TEXT(sUnknownOperatorFmt, msgUnknownOperatorFmt);
    ERROR_TEXT(sOperatorNeedNCountOfParameters, msgOperatorNeedNCountOfParameters);
    ERROR_TEXT(sIncompatibleTypesFmt, msgIncompatibleTypesFmt);
    ERROR_TEXT(sInvalidTypecast, msgInvalidTypecastFmt);
    ERROR_TEXT(sEmptyExpression, msgEmptyExpression);
    ERROR_TEXT(sExpressionMustBeBoolean, msgExpressionMustBeBoolean);
    ERROR_TEXT(sExpressionMustBeConstant, msgExpressionMustBeConstant);
    ERROR_TEXT(sDeclarationHasNoDataTypeFmt, msgDeclarationHasNoDataTypeFmt);
    ERROR_TEXT(errInvalidTypeDeclaration, msgInvalidTypeDeclaration);
    ERROR_TEXT(sTypeKeywordRequred, msgTypeKeywordRequred);
    ERROR_TEXT(errConstValueOverflow, msgConstValueOverflowFmt);

    // units
    ERROR_TEXT(errUnitNotFoundFmt, msgUnitNotFoundFmt);
    ERROR_TEXT(errUnitRecursivelyUsesItselfFmt, msgUnitRecursivelyUsesItselfFmt);

    // loops
    ERROR_TEXT(sContinueAllowedOnlyInLoop, msgContinueAllowedOnlyInLoop);
    ERROR_TEXT(sBreakOrContinueAreAllowedOnlyInALoops, msgBreakOrContinueAreAllowedOnlyInALoops);
    ERROR_TEXT(sLoopLevelExprected, msgLoopLevelExprected);
    ERROR_TEXT(sLoopLevelGreaterThenPossibleFmt, msgLoopLevelGreaterThenPossibleFmt);
    ERROR_TEXT(sForLoopIndexVarsMastBeSimpleIntVar, msgForLoopIndexVarsMastBeSimpleIntVar);
    ERROR_TEXT(sKeywordToOrDowntoExpected, msgKeywordToOrDowntoExpected);
    ERROR_TEXT(sForOrWhileLoopExecutesZeroTimes, msgForOrWhileLoopExecutesZeroTimes);
    ERROR_TEXT(sZeroDeltaInForLoop, msgZeroDeltaInForLoop);
    ERROR_TEXT(sCannotModifyForLoopIndexVarFmt, msgCannotModifyForLoopIndexVarFmt);

    // records
    ERROR_TEXT(sIdentifierHasNoMembersFmt, msgIdentifierHasNoMembersFmt);
    ERROR_TEXT(sRecordTypeRequired, msgRecordTypeRequired);
    ERROR_TEXT(sStructTypeRequired, msgStructTypeRequired);
    ERROR_TEXT(sRecurciveTypeLinkIsNotAllowed, msgRecurciveTypeLinkIsNotAllowed);
    ERROR_TEXT(sFieldConstOrFuncRequiredForGetter, msgFieldConstOrFuncRequiredForGetter);
    ERROR_TEXT(sFieldOrProcRequiredForSetter, msgFieldOrProcRequiredForSetter);
    ERROR_TEXT(sCannotModifyReadOnlyProperty, msgCannotModifyReadOnlyProperty);
    ERROR_TEXT(sCannotAccessToWriteOnlyProperty, msgCannotAccessToWriteOnlyProperty);
    ERROR_TEXT(sSetterMustBeMethodWithSignFmt, msgSetterMustBeMethodWithSignFmt);
    ERROR_TEXT(errorDefaultPropertyMustBeAnArrayProperty, msgDefaultPropertyMustBeAnArrayProperty);
    ERROR_TEXT(errorDefaultPropertyAlreadyExistsFmt, msgDefaultPropertyAlreadyExistsFmt);

    // classes
    ERROR_TEXT(sClassTypeCannotBeAnonimous, msgClassTypeCannotBeAnonimous);
    ERROR_TEXT(errCLASSTypeRequired, msgClassTypeRequired);


    // interfaces
    ERROR_TEXT(sInterfacedTypeCannotBeAnonimous, msgInterfacedTypeCannotBeAnonimous);
    ERROR_TEXT(sInterfaceTypeRequired, msgInterfaceTypeRequired);


    // arrays
    ERROR_TEXT(sOrdinalTypeRequired, msgOrdinalTypeRequired);
    ERROR_TEXT(sOrdinalConstOrTypeRequred, msgOrdinalConstOrTypeRequred);
    ERROR_TEXT(sArrayTypeRequired, msgArrayTypeRequired);
    ERROR_TEXT(sArrayOrStringTypeRequired, msgArrayOrStringTypeRequired);
    ERROR_TEXT(sNeedSpecifyNIndexesFmt, msgNeedSpecifyNIndexesFmt);
    ERROR_TEXT(sConstExprOutOfRangeFmt, msgConstExprOutOfRangeFmt);
    ERROR_TEXT(sOpenArrayAllowedOnlyAsTypeOfParam, msgOpenArrayAllowedOnlyAsTypeOfParam);

    // New/Free
    ERROR_TEXT(sPointerTypeRequired, msgPointerTypeRequired);
    ERROR_TEXT(sReferenceTypeRequired, msgReferenceTypeRequired);


    // Ranges
    ERROR_TEXT(sLowerBoundExceedsHigherBound, msgLowerBoundExceedsHigherBound);
    ERROR_TEXT(sNumericTypeRequired, msgNumericTypeRequired);
    ERROR_TEXT(sConstRangeRequired, msgConstRangeRequired);


    // Enums
    ERROR_TEXT(sComaOrCloseRoundExpected, msgComaOrCloseRoundExpected);

    // Expressions
    ERROR_TEXT(sSingleExpressionRequired, msgSingleExpressionRequired);
    ERROR_TEXT(errStringExpressionRequired, msgStringExpressionRequired);


    // Import/Export
    ERROR_TEXT(sImportFuncCannotBeInline, msgImportFuncCannotBeInline);
    ERROR_TEXT(sExportAllowsOnlyInIntfSection, msgExportAllowsOnlyInIntfSection);

    // Interanl errors
    ERROR_TEXT(sOperatorAlreadyOverloadedFmt, msgOperatorForTypesAlreadyOverloadedFmt);
    ERROR_TEXT(sFeatureNotSupported, msgFeatureNotSupported);

    // Platform/ASM
    ERROR_TEXT(sInstructionAlreadyDeclaredFmt, msgInstructionAlreadyDeclaredFmt);
    ERROR_TEXT(sRegisterAlreadyDeclaredFmt, msgRegisterAlreadyDeclaredFmt);
    ERROR_TEXT(sUnknownInstructionFmt, msgUnknownInstructionFmt);
    ERROR_TEXT(sUnknownPlatformFmt, msgUnknownPlatformFmt);
    ERROR_TEXT(sProcedureOrFunctionKeywordAreExpected, msgProcedureOrFunctionKeywordAreExpected);

    ERROR_TEXT(sDuplicateOpenBlock, msgDuplicateOpenBlock);
    ERROR_TEXT(sASMSyntaxError, msgASMSyntaxError);
    ERROR_TEXT(sDestArgCannotBeConst, msgDestArgCannotBeConst);
    ERROR_TEXT(sEmptyArgument, msgEmptyArgument);
    ERROR_TEXT(sInvalidArgument, msgInvalidArgument);
    ERROR_TEXT(sLabelRedeclaretedFmt, msgLabelRedeclaretedFmt);
    ERROR_TEXT(sImmediateOffsetIsOutOfRangeFmt, msgImmediateOffsetIsOutOfRangeFmt);

    // IIF
    ERROR_TEXT(sTypesMustBeIdentical, msgTypesMustBeIdentical);
    ERROR_TEXT(sThenAndElseSectionAreIdentical, msgThenAndElseSectionAreIdentical);

    // Try/Except/Finally
    ERROR_TEXT(sTryKeywordMissed, msgTryKeywordMissed);
    ERROR_TEXT(sExceptOrFinallySectionWasMissed, msgExceptOrFinallySectionWasMissed);
    ERROR_TEXT(sSectionFinallyAlreadyDefined, msgSectionFinallyAlreadyDefined);
    ERROR_TEXT(sSectionExceptAlreadyDefined, msgSectionExceptAlreadyDefined);
    ERROR_TEXT(sExceptSectionMustBeDeclareBeforeFinally, msgExceptSectionMustBeDeclareBeforeFinally);
    ERROR_TEXT(sBreakContinueExitAreNotAllowedInFinallyClause, msgBreakContinueExitAreNotAllowedInFinallyClause);

    // case
    ERROR_TEXT(sCaseStmtRequireAtLeastOneMatchExpr, msgCaseStmtRequireAtLeastOneMatchExpr);
    ERROR_TEXT(sMatchExprTypeMustBeIdenticalToCaseExprFmt, msgMatchExprTypeMustBeIdenticalToCaseExprFmt);
    ERROR_TEXT(sDuplicateMatchExpression, msgDuplicateMatchExpression);

    // addr
    ERROR_TEXT(sVarOrProcRequired, msgVarOrProcRequired);

    ERROR_TEXT(sProcOrProcVarRequired, msgProcOrProcVarRequired);

    // generics
    ERROR_TEXT(sNoOneTypeParamsWasFound, msgNoOneTypeParamsWasFound);
    ERROR_TEXT(sGenericProcInstanceErrorFmt, msgGenericProcInstanceErrorFmt);
    ERROR_TEXT(sGenericFuncInstanceErrorFmt, msgGenericFuncInstanceErrorFmt);
    ERROR_TEXT(sProcHasNoGenericParams, msgProcHasNoGenericParams);
    ERROR_TEXT(sProcRequiresExplicitTypeArgumentFmt, msgProcRequiresExplicitTypeArgumentFmt);
    ERROR_TEXT(sTooManyActualTypeParameters, msgTooManyActualTypeParameters);

    // conditional statements
    ERROR_TEXT(sInvalidConditionalStatement, msgInvalidConditionalStatement);
end.

