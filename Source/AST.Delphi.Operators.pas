unit AST.Delphi.Operators;

interface

{$I AST.Parser.Defines.inc}

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

type

  TOperatorID = (
    // not oveloaded: //////////////////
    opNone           = 0,
    opOpenRound      = 1,
    opCloseRound     = 2,
    opSubExpression  = 3,
    opCall           = 4,
    opDereference    = 5,
    opPeriod         = 6,
    opAddr           = 7,
    opIs             = 8,
    opAs             = 9,
    // oveloaded: //////////////////////
    opAssignment     = 10,
    // unar:
    opInitialize     = 11,
    opFinalize       = 12,
    opImplicit       = 13,
    opExplicit       = 14,
    opNegative       = 15,
    opPositive       = 16,
    opNot            = 17,
    opInc            = 18,
    opDec            = 19,
    opTrunc          = 20,
    opRound          = 21,
    // binar:
    opIn             = 22,
    opEqual          = 23,
    opNotEqual       = 24,
    opGreater        = 25,
    opGreaterOrEqual = 26,
    opLess           = 27,
    opLessOrEqual    = 28,
    opAdd            = 29,
    opSubtract       = 30,
    opMultiply       = 31,
    opDivide         = 32,
    opIntDiv         = 33,
    opModDiv         = 34,
    opShiftLeft      = 35,
    opShiftRight     = 36,
    opAnd            = 37,
    opOr             = 38,
    opXor            = 39
//    opBitwiseAnd,
//    opBitwiseOr,
//    opBitwiseXor
  );


  TOperatorType =
  (
    opSpecial,
    opBinary,
    opUnarPrefix,
    opUnarSufix

  );

{ signatures of operators:
|--------------------|------------|------------------------------------------------|-------------------|
|  Operator          | Category	  | Declaration Signature	                         | Symbol Mapping    |
|--------------------|------------|------------------------------------------------|-------------------|
| Implicit           | Conversion | Implicit(a : type) : resultType;               | implicit typecast |
| Explicit           | Conversion | Explicit(a: type) : resultType;                | explicit typecast |
| Negative           | Unary      | Negative(a: type) : resultType;                | -                 |
| Positive           | Unary      | Positive(a: type): resultType;                 | +                 |
| LogicalNot         | Unary      | LogicalNot(a: type): resultType;               | not               |
| In                 | Set        | In(a: type; b: type) : Boolean;                | in                |
| Eq                 | Comparison | Equal(a: type; b: type) : Boolean;             | =                 |
| NotEqual           | Comparison | NotEqual(a: type; b: type): Boolean;           | <>                |
| GreaterThan        | Comparison | GreaterThan(a: type; b: type) Boolean;         | >                 |
| GreaterThanOrEqual | Comparison | GreaterThanOrEqual(a: type; b: type): Boolean; | >=                |
| LessThan           | Comparison | LessThan(a: type; b: type): Boolean;           | <                 |
| LessThanOrEqual    | Comparison | LessThanOrEqual(a: type; b: type): Boolean;    | <=                |
| Add                | Binary     | Add(a: type; b: type): resultType;             | +                 |
| Subtract           | Binary     | Subtract(a: type; b: type) : resultType;       | -                 |
| Multiply           | Binary     | Multiply(a: type; b: type) : resultType;       | *                 |
| Divide             | Binary     | Divide(a: type; b: type) : resultType;         | /                 |
| IntDivide          | Binary     | IntDivide(a: type; b: type): resultType;       | div               |
| Modulus            | Binary     | Modulus(a: type; b: type): resultType;         | mod               |
| LeftShift          | Binary     | LeftShift(a: type; b: type): resultType;       | shl               |
| RightShift         | Binary     | RightShift(a: type; b: type): resultType;      | shr               |
| LogicalAnd         | Binary     | LogicalAnd(a: type; b: type): resultType;      | and               |
| LogicalOr          | Binary     | LogicalOr(a: type; b: type): resultType;       | or                |
| LogicalXor         | Binary     | LogicalXor(a: type; b: type): resultType;      | xor               |
| BitwiseAnd         | Binary     | BitwiseAnd(a: type; b: type): resultType;      | and               |
| BitwiseOr          | Binary     | BitwiseOr(a: type; b: type): resultType;       | or                |
| BitwiseXor         | Binary     | BitwiseXor(a: type; b: type): resultType;      | xor               |
|--------------------|------------|------------------------------------------------|-------------------|}


function OperatorFullName(&Operator: TOperatorID): string;
function OperatorShortName(&Operator: TOperatorID): string;
function GetOperatorID(const Name: string): TOperatorID; inline;


// todo:
// 1. ( ).
// 2. not.
// 3. *, /, div, mod, and, shl, shr, as.
// 4. +, –, or, xor.
// 5. =, <>, <, >, <=, >=, in, is.


const
  // Приоритеты операций (-1 - низший приоритет, 10 - высшый приоритет)
  cOperatorPriorities: array [TOperatorID] of Integer = (

  {opNone}                -1,
  {opOpenRound}           10,
  {opCloseRound}          10,
  {opSubExpression}       -1,
  {opCall}                 9,
  {opDereference}          8,
  {opPeriod}               2,
  {opAddr}                 8,
  {opIS}                   8,
  {opAS}                   8,

  {opAssign}               0,
  {opInitialize}           0,
  {opFinalize}             0,
  {opImplicit}            -1,
  {opExplicit}            -1,
  {opNegative}             8,
  {opPositive}             2,
  {opLogicalNot}           8,
  {opInc} 0,
  {opDec} 0,
  {opTrunc} 0,
  {opRound} 0,

  {opIn}                   1,
  {opEqual}                4,
  {opNotEqual}             4,
  {opGreaterThan}          4,
  {opGreaterThanOrEqual}   4,
  {opLessThan}             4,
  {opLessThanOrEqual}      4,
  {opAdd}                  6,
  {opSubtract}             6,
  {opMultiply}             7,
  {opDivide}               7,
  {opIntDiv}               7,
  {opModDiv}               7,
  {opLeftShift}            7,
  {opRightShift}           7,
  {opLogicalAnd}           7,
  {opLogicalOr}            5,
  {opLogicalXor}           5
//  {opBitwiceAnd}           7,
//  {opBitwiceOr}            5,
//  {opBitwiceXor}           5
  );

const
  // Приоритеты операций (-1 - низший приоритет, 10 - высшый приоритет)
  cOperatorTypes: array [TOperatorID] of TOperatorType = (

  {opNone}                opSpecial,
  {opOpenRound}           opSpecial,
  {opCloseRound}          opSpecial,
  {opSubExpression}       opSpecial,
  {opCall}                opSpecial,
  {opDereference}         opUnarSufix,
  {opPeriod}              opBinary,
  {opAddr}                opUnarPrefix,
  {opIS}                  opBinary,
  {opAS}                  opBinary,

  {opAssign}              opBinary,
  {opInitialize}          opUnarPrefix,
  {opFinalize}            opUnarPrefix,
  {opImplicit}            opSpecial,
  {opExplicit}            opSpecial,
  {opNegative}            opUnarPrefix,
  {opPositive}            opUnarPrefix,
  {opLogicalNot}          opUnarPrefix,
  {opInc}                 opUnarPrefix,
  {opDec}                 opUnarPrefix,
  {opTrunc}               opUnarPrefix,
  {opRound}               opUnarPrefix,

  {opIn}                   opBinary,
  {opEqual}                opBinary,
  {opNotEqual}             opBinary,
  {opGreaterThan}          opBinary,
  {opGreaterThanOrEqual}   opBinary,
  {opLessThan}             opBinary,
  {opLessThanOrEqual}      opBinary,
  {opAdd}                  opBinary,
  {opSubtract}             opBinary,
  {opMultiply}             opBinary,
  {opDivide}               opBinary,
  {opIntDiv}               opBinary,
  {opModDiv}               opBinary,
  {opLeftShift}            opBinary,
  {opRightShift}           opBinary,
  {opLogicalAnd}           opBinary,
  {opLogicalOr}            opBinary,
  {opLogicalXor}           opBinary
//  {opBitwiceAnd}           opBinary,
//  {opBitwiceOr}            opBinary,
//  {opBitwiceXor}           opBinary,

  );

function NeedRValue(Op: TOperatorID): Boolean; inline;

var
  _operators: TStringList = nil;

implementation


const
  cOpFullNames: array [TOperatorID] of string = (

  {opNone}            '',
  {opOpenRound}       '(',
  {opCloseRound}      ')',
  {opSubExpression}   '',
  {opCall}            'call',
  {opDereference}     'Dereference',
  {opPeriod}          'period',
  {opAddr}            'Addr',
  {opIS}              'IS',
  {opAS}              'AS',

  {opAssign}          'Assign',
  {opInitialize}      'Initialize',
  {opFinalize}        'Finalize',
  {opImplicit}        'Implicit',
  {opExplicit}        'Explicit',
  {opNegative}        'Negative',
  {opPositive}        'Positive',
  {opLogicalNot}      'Not',
  {opInc}             '',
  {opDec}             '',
  {opTrunc}           '',
  {opRound}           '',

  {opIn}              'In',
  {opEqual}           'Equal',
  {opNotEqual}        'NotEqual',
  {opGreater}         'GreaterThan',
  {opGreaterOrEqual}  'GreaterThanOrEqual',
  {opLess}            'LessThan',
  {opLessOrEqual}     'LessThanOrEqual',
  {opAdd}             'Add',
  {opSubtract}        'Subtract',
  {opMultiply}        'Multiply',
  {opDivide}          'Divide',
  {opIntDiv}          'IntDivide',
  {opModDiv}          'Modulus',
  {opLeftShift}       'LeftShift',
  {opRightShift}      'RightShift',
  {opLogicalAnd}      'And',
  {opLogicalOr}       'Or',
  {opLogicalXor}      'Xor'
// {opBitwiceAnd}      'And',
//  {opBitwiceOr}       'Or',
//  {opBitwiceXor}      'Xor',


  );


  cOpShortNames: array [TOperatorID] of string = (

  {opNone}            '',
  {opOpenRound}       '(',
  {opCloseRound}      ')',
  {opSubExpression}   '',
  {opCall}            'call',
  {opDereference}     '^',
  {opPeriod}          '..',
  {opAddr}            '@',
  {opIs}              'is',
  {opAs}              'as',

  {opAssign}          ':=',
  {opInitialize}      '',
  {opFinalize}        '',
  {opImplicit}        'Implicit',
  {opExplicit}        'Explicit',
  {opNegative}        'Negative',
  {opPositive}        'Positive',
  {opLogicalNot}      'not',
  {opInc}             '',
  {opDec}             '',
  {opTrunc}           '',
  {opRound}           '',

  {opIn}              'in',
  {opEqual}           '=',
  {opNotEqual}        '<>',
  {opGreater}         '>',
  {opGreaterOrEqual}  '>=',
  {opLess}            '<',
  {opLessOrEqual}     '<=',
  {opAdd}             '+',
  {opSubtract}        '-',
  {opMultiply}        '*',
  {opDivide}          '/',
  {opIntDiv}          'div',
  {opModDiv}          'mod',
  {opLeftShift}       'shl',
  {opRightShift}      'shr',
  {opLogicalAnd}      'and',
  {opLogicalOr}       'or',
  {opLogicalXor}      'xor'
// {opBitwiceAnd}      'and',
//  {opBitwiceOr}       'or',
//  {opBitwiceXor}      'xor',
  );


function NeedRValue(Op: TOperatorID): Boolean; inline;
begin
  Result := (Op >= opPeriod);
end;


function OperatorFullName(&Operator: TOperatorID): string;
begin
  Result := cOpFullNames[&Operator];
end;

function OperatorShortName(&Operator: TOperatorID): string;
begin
  Result := cOpShortNames[&Operator];
end;

function GetOperatorID(const Name: string): TOperatorID; inline;
var
  idx: Integer;
begin
  idx := _operators.IndexOf(Name);
  if idx >= 0 then
    Result := TOperatorID(_operators.Objects[idx])
  else
    Result := opNone;
end;

initialization
  _operators := TStringList.Create;
  _operators.AddObject('Assignment', TObject(opAssignment));
  _operators.AddObject('Implicit', TObject(opImplicit));
  _operators.AddObject('Explicit', TObject(opExplicit));
  _operators.AddObject('Negative', TObject(opNegative));
  _operators.AddObject('Positive', TObject(opPositive));
  _operators.AddObject('BitwiceNot', TObject(opNot));
  _operators.AddObject('In', TObject(opIn));
  _operators.AddObject('Equal', TObject(opEqual));
  _operators.AddObject('NotEqual', TObject(opNotEqual));
  _operators.AddObject('GreaterThan', TObject(opGreater));
  _operators.AddObject('GreaterThanOrEqual', TObject(opGreaterOrEqual));
  _operators.AddObject('LessThan', TObject(opLess));
  _operators.AddObject('LessThanOrEqual', TObject(opLessOrEqual));
  _operators.AddObject('Add', TObject(opAdd));
  _operators.AddObject('Subtract', TObject(opSubtract));
  _operators.AddObject('Multiply', TObject(opMultiply));
  _operators.AddObject('Divide', TObject(opDivide));
  _operators.AddObject('IntDiv', TObject(opIntDiv));
  _operators.AddObject('ModDiv', TObject(opModDiv));
  _operators.AddObject('ShiftLeft', TObject(opShiftLeft));
  _operators.AddObject('ShiftRight', TObject(opShiftRight));
  _operators.AddObject('BitwiceAnd', TObject(opAnd));
  _operators.AddObject('BitwiceOr', TObject(opOr));
  _operators.AddObject('BitwiceXor', TObject(opXor));
  _operators.Sort;


finalization
  _operators.Free;

end.

