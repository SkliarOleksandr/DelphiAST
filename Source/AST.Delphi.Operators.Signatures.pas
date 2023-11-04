unit AST.Delphi.Operators.Signatures;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  AST.Classes,
  AST.Lexer,
  AST.Delphi.Operators,
  AST.Delphi.Classes;

type

  TOperator = class
    class function OpID: TOperatorID; virtual; abstract;
    class function OpPriority: Integer; virtual; abstract;
    class function CheckParams(AStruct: TIDStructure; AParams: TParamsScope): Boolean; virtual;
    class function GetStrSignatureFmt: string; virtual; abstract;
  end;
  TOperatorClass = class of TOperator;

  TOperatorSignatures = {static} class
  private
    class var FOperators: TDictionary<{name:} string, {operator:} TOperatorClass>;
  public
    class constructor ClassCreate;
    class destructor ClassDestroy;
    class function FindOperator(const AName: string): TOperatorClass; static; inline;

    class procedure CheckSignature(const AStruct: TIDStructure;
                                   const AOperatorID: TIdentifier;
                                   const AParams: TParamsScope);
  end;


implementation

uses
  AST.Parser.Errors;

type
    TOp_Initialize = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
      class function CheckParams(AStruct: TIDStructure; AParams: TParamsScope): Boolean; override;
      class function GetStrSignatureFmt: string; override;
    end;

    TOp_Finalize = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
      class function CheckParams(AStruct: TIDStructure; AParams: TParamsScope): Boolean; override;
      class function GetStrSignatureFmt: string; override;
    end;

    TOp_Implicit = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_Explicit = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_Negative = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_Positive = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_Inc = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_Dec = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_LogicalNot = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_Trunc = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_Round = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_In = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_Equal = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_NotEqual = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_GreaterThan = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_GreaterThanOrEqual = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_LessThan = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_LessThanOrEqual = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_Assign = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_Add = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_Subtract = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_Multiply = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_Divide = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_IntDivide = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_Modulus = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_LeftShift = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_RightShift = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_LogicalAnd = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_LogicalOr = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_LogicalXor = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_BitwiseAnd = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_BitwiseOr = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;

    TOp_BitwiseXor = class(TOperator)
    public
      class function OpID: TOperatorID; override;
      class function OpPriority: Integer; override;
    end;


{ TOperatorSignatures }

class procedure TOperatorSignatures.CheckSignature(const AStruct: TIDStructure;
                                                   const AOperatorID: TIdentifier;
                                                   const AParams: TParamsScope);
var
  LOperator: TOperatorClass;
begin
  if FOperators.TryGetValue(UpperCase(AOperatorID.Name), {out} LOperator) then
  begin
    var LTest := LOperator.CheckParams(AStruct, AParams);
    if not LTest then
      AbortWork('operator must have signature: %s' + LOperator.GetStrSignatureFmt,
                [AOperatorID.Name, AStruct.Name], AOperatorID.TextPosition);
  end else
    AbortWork('Unknown operator %s', [AOperatorID.Name], AOperatorID.TextPosition)
end;

class constructor TOperatorSignatures.ClassCreate;
begin
  FOperators := TDictionary<{name:} string, {operator:} TOperatorClass>.Create;
  FOperators.Add('INITIALIZE', TOp_Initialize);
  FOperators.Add('FINALIZE', TOp_Finalize);
  FOperators.Add('IMPLICIT', TOp_Implicit);
  FOperators.Add('EXPLICIT', TOp_Explicit);
  FOperators.Add('NEGATIVE', TOp_Negative);
  FOperators.Add('POSITIVE', TOp_Positive);
  FOperators.Add('INC', TOp_Inc);
  FOperators.Add('DEC', TOp_Dec);
  FOperators.Add('LOGICALNOT', TOp_LogicalNot);
  FOperators.Add('TRUNC', TOp_Trunc);
  FOperators.Add('ROUND', TOp_Round);
  FOperators.Add('IN', TOp_In);
  FOperators.Add('EQUAL', TOp_Equal);
  FOperators.Add('NOTEQUAL', TOp_NotEqual);
  FOperators.Add('GREATERTHAN', TOp_GreaterThan);
  FOperators.Add('GREATERTHANOREQUAL', TOp_GreaterThanOrEqual);
  FOperators.Add('LESSTHAN', TOp_LessThan);
  FOperators.Add('LESSTHANOREQUAL', TOp_LessThanOrEqual);
  FOperators.Add('ASSIGN', TOp_Assign);
  FOperators.Add('ADD', TOp_Add);
  FOperators.Add('SUBTRACT', TOp_Subtract);
  FOperators.Add('MULTIPLY', TOp_Multiply);
  FOperators.Add('DIVIDE', TOp_Divide);
  FOperators.Add('INTDIVIDE', TOp_IntDivide);
  FOperators.Add('MODULUS', TOp_Modulus);
  FOperators.Add('LEFTSHIFT', TOp_LeftShift);
  FOperators.Add('RIGHTSHIFT', TOp_RightShift);
  FOperators.Add('LOGICALAND', TOp_LogicalAnd);
  FOperators.Add('LOGICALOR', TOp_LogicalOr);
  FOperators.Add('LOGICALXOR', TOp_LogicalXor);
  FOperators.Add('BITWISEAND', TOp_BitwiseAnd);
  FOperators.Add('BITWISEOR', TOp_BitwiseOr);
  FOperators.Add('BITWISEXOR', TOp_BitwiseXor);
end;

class destructor TOperatorSignatures.ClassDestroy;
begin
  FOperators.Free;
end;

class function TOperatorSignatures.FindOperator(const AName: string): TOperatorClass;
begin
  FOperators.TryGetValue(UpperCase(AName), {out} Result);
end;

{ TOp_Initialize }


class function TOp_Initialize.OpID: TOperatorID;
begin
  Result := opInitialize;
end;

class function TOp_Initialize.OpPriority: Integer;
begin
  Result := -1;
end;

class function TOp_Initialize.CheckParams(AStruct: TIDStructure; AParams: TParamsScope): Boolean;
begin
  Result := (AParams.Count = 1) and (AParams[0].DataType = AStruct) and (VarOut in AParams[0].Flags);
end;

class function TOp_Initialize.GetStrSignatureFmt: string;
begin
  Result := '(out ADest: %s)';
end;


{ TOp_Finalize }

class function TOp_Finalize.CheckParams(AStruct: TIDStructure; AParams: TParamsScope): Boolean;
begin
  Result := (AParams.Count = 1) and (AParams[0].DataType = AStruct) and (VarInOut in AParams[0].Flags);
end;

class function TOp_Finalize.GetStrSignatureFmt: string;
begin
  Result := '(var ADest: %s)';
end;

class function TOp_Finalize.OpID: TOperatorID;
begin
  Result := opFinalize;
end;

class function TOp_Finalize.OpPriority: Integer;
begin
  Result := -1;
end;

{ TOperator }

class function TOperator.CheckParams(AStruct: TIDStructure; AParams: TParamsScope): Boolean;
begin
  Result := True;
end;

{ TOp_Implicit }

class function TOp_Implicit.OpID: TOperatorID;
begin
  Result := opImplicit;
end;

class function TOp_Implicit.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_Explicit }

class function TOp_Explicit.OpID: TOperatorID;
begin
  Result := opExplicit;
end;

class function TOp_Explicit.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_Negative }

class function TOp_Negative.OpID: TOperatorID;
begin
  Result := opNegative;
end;

class function TOp_Negative.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_Positive }

class function TOp_Positive.OpID: TOperatorID;
begin
  Result := opPositive;
end;

class function TOp_Positive.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_Inc }

class function TOp_Inc.OpID: TOperatorID;
begin
  Result := OpInc;
end;

class function TOp_Inc.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_Dec }

class function TOp_Dec.OpID: TOperatorID;
begin
  Result := opDec;
end;

class function TOp_Dec.OpPriority: Integer;
begin
  Result := 0;
end;

{ TOp_LogicalNot }

class function TOp_LogicalNot.OpID: TOperatorID;
begin
  Result := opNot;
end;

class function TOp_LogicalNot.OpPriority: Integer;
begin
  Result := 0;
end;

{ TOp_Trunc }

class function TOp_Trunc.OpID: TOperatorID;
begin
  Result := opTrunc;
end;

class function TOp_Trunc.OpPriority: Integer;
begin
  Result := 0;
end;

{ TOp_Round }

class function TOp_Round.OpID: TOperatorID;
begin
  Result := opRound;
end;

class function TOp_Round.OpPriority: Integer;
begin
  Result := 0;
end;

{ TOp_In }

class function TOp_In.OpID: TOperatorID;
begin
  Result := opIn;
end;

class function TOp_In.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_Equal }

class function TOp_Equal.OpID: TOperatorID;
begin
  Result := opEqual;
end;

class function TOp_Equal.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_NotEqual }

class function TOp_NotEqual.OpID: TOperatorID;
begin
  Result := opNotEqual;
end;

class function TOp_NotEqual.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_GreaterThan }

class function TOp_GreaterThan.OpID: TOperatorID;
begin
  Result := opGreater;
end;

class function TOp_GreaterThan.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_GreaterThanOrEqual }

class function TOp_GreaterThanOrEqual.OpID: TOperatorID;
begin
  Result := opGreaterOrEqual;
end;

class function TOp_GreaterThanOrEqual.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_LessThan }

class function TOp_LessThan.OpID: TOperatorID;
begin
  Result := opLess;
end;

class function TOp_LessThan.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_LessThanOrEqual }

class function TOp_LessThanOrEqual.OpID: TOperatorID;
begin
  Result := opLessOrEqual;
end;

class function TOp_LessThanOrEqual.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_Assign }

class function TOp_Assign.OpID: TOperatorID;
begin
  Result := opAssignment;
end;

class function TOp_Assign.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_Add }

class function TOp_Add.OpID: TOperatorID;
begin
  Result := opAdd;
end;

class function TOp_Add.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_Subtract }

class function TOp_Subtract.OpID: TOperatorID;
begin
  Result  := opSubtract;
end;

class function TOp_Subtract.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_Multiply }

class function TOp_Multiply.OpID: TOperatorID;
begin
  Result := opMultiply;
end;

class function TOp_Multiply.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_Divide }

class function TOp_Divide.OpID: TOperatorID;
begin
  Result := opDivide;
end;

class function TOp_Divide.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_IntDivide }

class function TOp_IntDivide.OpID: TOperatorID;
begin
  Result := opIntDiv;
end;

class function TOp_IntDivide.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_Modulus }

class function TOp_Modulus.OpID: TOperatorID;
begin
  Result := opModDiv;
end;

class function TOp_Modulus.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_LeftShift }

class function TOp_LeftShift.OpID: TOperatorID;
begin
  Result := opShiftLeft;
end;

class function TOp_LeftShift.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_RightShift }

class function TOp_RightShift.OpID: TOperatorID;
begin
  Result := opShiftRight;
end;

class function TOp_RightShift.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_LogicalAnd }

class function TOp_LogicalAnd.OpID: TOperatorID;
begin
  Result := opAnd;
end;

class function TOp_LogicalAnd.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_LogicalOr }

class function TOp_LogicalOr.OpID: TOperatorID;
begin
  Result := opOr;
end;

class function TOp_LogicalOr.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_LogicalXor }

class function TOp_LogicalXor.OpID: TOperatorID;
begin
  Result := opXor;
end;

class function TOp_LogicalXor.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_BitwiseAnd }

class function TOp_BitwiseAnd.OpID: TOperatorID;
begin
  Result := opAnd;
end;

class function TOp_BitwiseAnd.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_BitwiseOr }

class function TOp_BitwiseOr.OpID: TOperatorID;
begin
  Result := opOr;
end;

class function TOp_BitwiseOr.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{ TOp_BitwiseXor }

class function TOp_BitwiseXor.OpID: TOperatorID;
begin
  Result := opXor;
end;

class function TOp_BitwiseXor.OpPriority: Integer;
begin
  Result := cOperatorPriorities[OpID];
end;

{test code}
(*
unit TestUnit.XXX;

interface

type
  TRecord = record
    class operator Initialize (out Dest: TRecord);
    class operator Finalize (var Dest: TRecord);
    class operator Implicit(a : Integer) : TRecord;
    class operator Explicit(a: Integer) : TRecord;
    class operator Negative(a: TRecord) : TRecord;
    class operator Positive(a: TRecord): TRecord;
    class operator Inc(a: TRecord) : TRecord;
    class operator Dec(a: TRecord): TRecord;
    class operator LogicalNot(a: TRecord): TRecord;
    class operator Trunc(a: TRecord): TRecord;
    class operator Round(a: TRecord): TRecord;
    class operator In(a: TRecord; b: TRecord) : Boolean;
    class operator Equal(a: TRecord; b: TRecord) : Boolean;
    class operator NotEqual(a: TRecord; b: TRecord): Boolean;
    class operator GreaterThan(a: TRecord; b: TRecord): Boolean;
    class operator GreaterThanOrEqual(a: TRecord; b: TRecord): Boolean;
    class operator LessThan(a: TRecord; b: TRecord): Boolean;
    class operator LessThanOrEqual(a: TRecord; b: TRecord): Boolean;
    class operator Assign(var Dest: TRecord; const [ref] Src: TRecord);
    class operator Add(a: TRecord; b: TRecord): TRecord;
    class operator Subtract(a: TRecord; b: TRecord) : TRecord;
    class operator Multiply(a: TRecord; b: TRecord) : TRecord;
    class operator Divide(a: TRecord; b: TRecord) : TRecord;
    class operator IntDivide(a: TRecord; b: TRecord): TRecord;
    class operator Modulus(a: TRecord; b: TRecord): TRecord;
    class operator LeftShift(a: TRecord; b: TRecord): TRecord;
    class operator RightShift(a: TRecord; b: TRecord): TRecord;
    class operator LogicalAnd(a: TRecord; b: TRecord): TRecord;
    class operator LogicalOr(a: TRecord; b: TRecord): TRecord;
    class operator LogicalXor(a: TRecord; b: TRecord): TRecord;
    class operator BitwiseAnd(a: TRecord; b: TRecord): TRecord;
    class operator BitwiseOr(a: TRecord; b: TRecord): TRecord;
    class operator BitwiseXor(a: TRecord; b: TRecord): TRecord;
  end;

implementation

{ TRecord }

class operator TRecord.Add(a, b: TRecord): TRecord;
begin

end;

class operator TRecord.Assign(var Dest: TRecord; const [ref] Src: TRecord);
begin

end;

class operator TRecord.BitwiseAnd(a, b: TRecord): TRecord;
begin

end;

class operator TRecord.BitwiseOr(a, b: TRecord): TRecord;
begin

end;

class operator TRecord.BitwiseXor(a, b: TRecord): TRecord;
begin

end;

class operator TRecord.Dec(a: TRecord): TRecord;
begin

end;

class operator TRecord.Divide(a, b: TRecord): TRecord;
begin

end;

class operator TRecord.Equal(a, b: TRecord): Boolean;
begin

end;

class operator TRecord.Explicit(a: Integer): TRecord;
begin

end;

class operator TRecord.GreaterThan(a, b: TRecord): Boolean;
begin

end;

class operator TRecord.GreaterThanOrEqual(a, b: TRecord): Boolean;
begin

end;

class operator TRecord.Implicit(a: Integer): TRecord;
begin

end;

class operator TRecord.In(a, b: TRecord): Boolean;
begin

end;

class operator TRecord.Inc(a: TRecord): TRecord;
begin

end;

class operator TRecord.IntDivide(a, b: TRecord): TRecord;
begin

end;

class operator TRecord.LeftShift(a, b: TRecord): TRecord;
begin

end;

class operator TRecord.LessThan(a, b: TRecord): Boolean;
begin

end;

class operator TRecord.LessThanOrEqual(a, b: TRecord): Boolean;
begin

end;

class operator TRecord.LogicalAnd(a, b: TRecord): TRecord;
begin

end;

class operator TRecord.LogicalNot(a: TRecord): TRecord;
begin

end;

class operator TRecord.LogicalOr(a, b: TRecord): TRecord;
begin

end;

class operator TRecord.LogicalXor(a, b: TRecord): TRecord;
begin

end;

class operator TRecord.Modulus(a, b: TRecord): TRecord;
begin

end;

class operator TRecord.Multiply(a, b: TRecord): TRecord;
begin

end;

class operator TRecord.Negative(a: TRecord): TRecord;
begin

end;

class operator TRecord.NotEqual(a, b: TRecord): Boolean;
begin

end;

class operator TRecord.Positive(a: TRecord): TRecord;
begin

end;

class operator TRecord.RightShift(a, b: TRecord): TRecord;
begin

end;

class operator TRecord.Round(a: TRecord): TRecord;
begin

end;

class operator TRecord.Subtract(a, b: TRecord): TRecord;
begin

end;

class operator TRecord.Trunc(a: TRecord): TRecord;
begin

end;

*)

end.
