unit AST.Pascal.ConstCalculator;

interface

uses System.SysUtils,
     System.Math,
     AST.Classes,
     AST.Delphi.Classes,
     AST.Parser.Errors,
     AST.Delphi.Operators,
     AST.Delphi.Errors,
     AST.Delphi.Intf;

type
  TExpressionCalculator = record
  private
    fErrors: TASTDelphiErrors;
    fSysDecls: PDelphiSystemDeclarations;
    function CalcSets(const Left, Right: TIDConstant; Operation: TOperatorID): TIDConstant;
    function CalcPointer(LeftConst, RightConst: TIDConstant; Operation: TOperatorID): TIDConstant;
    property Sys: PDelphiSystemDeclarations read fSysDecls;
  public
    constructor Create(const Module: IASTDelphiUnit);
    function ProcessConstOperation(Left, Right: TIDExpression; Operation: TOperatorID): TIDExpression; overload;
    function ProcessConstOperation(const Left, Right: TIDConstant; Operation: TOperatorID): TIDConstant; overload;
  end;


implementation

uses
   AST.Delphi.System,
   AST.Pascal.Parser,
   AST.Delphi.DataTypes,
   AST.Parser.Utils,
   AST.Delphi.Parser;

function AddSets(Left, Right: TIDSetConstant): TIDConstant;
var
  LList, RList: TIDExpressions;
begin
  LList := Left.Value;
  RList := Right.Value;
  // todo: remove duplicates
  Result := TIDSetConstant.CreateAsAnonymous(Left.Scope, Left.DataType, LList + RList);
  Result.TextPosition := Left.TextPosition;
end;

function TExpressionCalculator.CalcSets(const Left, Right: TIDConstant; Operation: TOperatorID): TIDConstant;
begin
  case Operation of
    opEqual: Result := Sys._False; // todo:
    opNotEqual: Result := Sys._False;  // todo:
    opAdd: Result := AddSets(Left as TIDSetConstant, Right as TIDSetConstant);
    opSubtract: Result := Left; // todo:
    opMultiply: Result := Left; // todo:
  else
    Result := nil;
  end;
end;

function TExpressionCalculator.ProcessConstOperation(const Left, Right: TIDConstant; Operation: TOperatorID): TIDConstant;
  //////////////////////////////////////////////////////////////
  function CalcInteger(LValue, RValue: Int64; Operation: TOperatorID): TIDConstant;
  var
    iValue: Int64;
    fValue: Double;
    bValue: Boolean;
    DT: TIDType;
  begin
    case Operation of
      opAdd: iValue := LValue + RValue;
      opSubtract: iValue := LValue - RValue;
      opMultiply: iValue := LValue * RValue;
      opNegative: iValue := -RValue;
      opIntDiv, opDivide: begin
        if RValue = 0 then
          fErrors.DIVISION_BY_ZERO(Sys._EmptyStrExpression);
        if Operation = opIntDiv then
          iValue := LValue div RValue
        else begin
         fValue := LValue / RValue;
         Exit(TIDFloatConstant.CreateAsAnonymous(Left.Scope, Sys._Float64, fValue));
        end;
      end;
      opEqual,
      opNotEqual,
      opGreater,
      opGreaterOrEqual,
      opLess,
      opLessOrEqual: begin
        case Operation of
          opEqual: bValue := (LValue = RValue);
          opNotEqual: bValue := (LValue <> RValue);
          opGreater: bValue := (LValue > RValue);
          opGreaterOrEqual: bValue := (LValue >= RValue);
          opLess: bValue := (LValue < RValue);
          opLessOrEqual: bValue := (LValue <= RValue);
          else bValue := False;
        end;
        Exit(TIDBooleanConstant.CreateAsAnonymous(Left.Scope, Sys._Boolean, bValue));
      end;
      opAnd: iValue := LValue and RValue;
      opOr: iValue := LValue or RValue;
      opXor: iValue := LValue xor RValue;
      opNot: iValue := not RValue;
      opShiftLeft: iValue := LValue shl RValue;
      opShiftRight: iValue := LValue shr RValue;
      else Exit(nil);
    end;
    DT := Sys.DataTypes[GetValueDataType(iValue)];
    Result := TIDIntConstant.CreateAsAnonymous(Left.Scope, DT, iValue);
  end;
  //////////////////////////////////////////////////////////////
  function CalcFloat(LValue, RValue: Double; Operation: TOperatorID): TIDConstant;
  var
    fValue: Double;
    bValue: Boolean;
    ValueDT: TIDType;
  begin
    ValueDT := Sys._Float64;
    case Operation of
      opAdd: fValue := LValue + RValue;
      opSubtract: fValue := LValue - RValue;
      opMultiply: fValue := LValue * RValue;
      opDivide: begin
        if RValue = 0 then
          fErrors.DIVISION_BY_ZERO(Sys._EmptyStrExpression);
        fValue := LValue / RValue;
      end;
      opNegative: begin
        fValue := -RValue;
        ValueDT := Right.DataType;
      end;
      opEqual,
      opNotEqual,
      opGreater,
      opGreaterOrEqual,
      opLess,
      opLessOrEqual: begin
        case Operation of
          opEqual: bValue := LValue = RValue;
          opNotEqual: bValue := LValue <> RValue;
          opGreater: bValue := LValue > RValue;
          opGreaterOrEqual: bValue := LValue >= RValue;
          opLess: bValue := LValue < RValue;
          opLessOrEqual: bValue := LValue <= RValue;
        else
          bValue := False;
        end;
        Exit(TIDBooleanConstant.CreateAsAnonymous(Left.Scope, Sys._Boolean, bValue));
      end;
    else
      Exit(nil);
    end;
    Result := TIDFloatConstant.CreateAsAnonymous(Left.Scope, ValueDT, fValue);
    Result.ExplicitDataType := ValueDT;
  end;
  //////////////////////////////////////////////////////////////
  function CalcBoolean(LValue, RValue: Boolean; Operation: TOperatorID): TIDConstant;
  var
    Value: Boolean;
  begin
    case Operation of
      opAnd: Value := LValue and RValue;
      opOr: Value := LValue or RValue;
      opXor: Value := LValue xor RValue;
      opNot: Value := not RValue;
      else Exit(nil);
    end;
    Result := TIDBooleanConstant.Create(Left.Scope, Identifier(BoolToStr(Value, True)), Sys._Boolean, Value);
  end;
  //////////////////////////////////////////////////////////////
  function CalcString(const LValue, RValue: string): TIDConstant;
  var
    sValue: string;
    bValue: Boolean;
  begin
    case Operation of
      opAdd: begin
        sValue := LValue + RValue;
        Result := TIDStringConstant.CreateAsAnonymous(Left.Scope, Sys._UnicodeString, sValue);
      end;
      opEqual,
      opNotEqual: begin
        case Operation of
          opEqual: bValue := LValue = RValue;
          opNotEqual: bValue := LValue <> RValue;
          else bValue := False;
        end;
        Exit(TIDBooleanConstant.CreateAsAnonymous(Left.Scope, Sys._Boolean, bValue));
      end;
      else Exit(nil);
    end;
  end;
  ///////////////////////////////////////////////////////////////
  function CalcIn(const Left: TIDConstant; const Right: TIDRangeConstant): TIDConstant;
  var
    LB, HB: TIDConstant;
    bValue: Boolean;
  begin
    LB := TIDConstant(Right.Value.LBExpression.Declaration);
    HB := TIDConstant(Right.Value.HBExpression.Declaration);
    bValue := (Left.CompareTo(LB) >= 0) and (Left.CompareTo(HB) <= 0);
    Result := TIDBooleanConstant.CreateAsAnonymous(Left.Scope, Sys._Boolean, bValue);
  end;
var
  LeftType, RightType: TClass;
  Constant: TIDConstant;
begin
  LeftType := Left.ClassType;
  RightType := Right.ClassType;

  Constant := nil;
  if RightType = TIDRangeConstant then
  begin
    Constant := CalcIn(Left, TIDRangeConstant(Right));
  end else
  if LeftType = TIDIntConstant then
  begin
    if RightType = TIDIntConstant then
      Constant := CalcInteger(TIDIntConstant(Left).Value, TIDIntConstant(Right).Value, Operation)
    else
      Constant := CalcFloat(TIDIntConstant(Left).Value, TIDFloatConstant(Right).Value, Operation)
  end else
  if LeftType = TIDFloatConstant then
  begin
    if RightType = TIDIntConstant then
      Constant := CalcFloat(TIDFloatConstant(Left).Value, TIDIntConstant(Right).Value, Operation)
    else
      Constant := CalcFloat(TIDFloatConstant(Left).Value, TIDFloatConstant(Right).Value, Operation)
  end else
  if LeftType = TIDStringConstant then begin
    if RightType = TIDStringConstant then
      Constant := CalcString(TIDStringConstant(Left).Value, TIDStringConstant(Right).Value)
    else
      Constant := CalcString(TIDStringConstant(Left).Value, TIDCharConstant(Right).Value)
  end else
  if LeftType = TIDCharConstant then begin
    if RightType = TIDCharConstant then
      Constant := CalcString(TIDCharConstant(Left).Value, TIDCharConstant(Right).Value)
    else
      Constant := CalcString(TIDCharConstant(Left).Value, TIDStringConstant(Right).Value)
  end else
  if LeftType = TIDBooleanConstant then
    Constant := CalcBoolean(TIDBooleanConstant(Left).Value, TIDBooleanConstant(Right).Value, Operation)
  else
    AbortWorkInternal('Invalid parameters', Left.SourcePosition);

  if not Assigned(Constant) then
    AbortWork('Operation %s not supported for constants', [OperatorFullName(Operation)], Left.SourcePosition);

  Result := Constant;
end;

function TExpressionCalculator.CalcPointer(LeftConst, RightConst: TIDConstant; Operation: TOperatorID): TIDConstant;
begin
  // todo:
  if LeftConst is TIDPointerConstant then
    Exit(LeftConst)
  else
    Exit(RightConst);
end;

function TExpressionCalculator.ProcessConstOperation(Left, Right: TIDExpression; Operation: TOperatorID): TIDExpression;
  //////////////////////////////////////////////////////////////
  function CalcInteger(LValue, RValue: Int64; Operation: TOperatorID): TIDConstant;
  var
    iValue: Int64;
    fValue: Double;
    bValue: Boolean;
    DT: TIDType;
  begin
    case Operation of
      opAdd: iValue := LValue + RValue;
      opSubtract: iValue := LValue - RValue;
      opMultiply: iValue := LValue * RValue;
      opNegative: iValue := -RValue;
      opIntDiv, opDivide: begin
        {if RValue = 0 then
          TASTDelphiUnit.ERROR_DIVISION_BY_ZERO(Right);}
        if Operation = opIntDiv then
          iValue := LValue div RValue
        else begin
         fValue := LValue / RValue;
         Exit(TIDFloatConstant.CreateWithoutScope(Sys._Float64, fValue));
        end;
      end;
      opEqual,
      opNotEqual,
      opGreater,
      opGreaterOrEqual,
      opLess,
      opLessOrEqual: begin
        case Operation of
          opEqual: bValue := (LValue = RValue);
          opNotEqual: bValue := (LValue <> RValue);
          opGreater: bValue := (LValue > RValue);
          opGreaterOrEqual: bValue := (LValue >= RValue);
          opLess: bValue := (LValue < RValue);
          opLessOrEqual: bValue := (LValue <= RValue);
          else bValue := False;
        end;
        Exit(TIDBooleanConstant.CreateWithoutScope(Sys._Boolean, bValue));
      end;
      opAnd: iValue := LValue and RValue;
      opOr: iValue := LValue or RValue;
      opXor: iValue := LValue xor RValue;
      opNot: iValue := not RValue;
      opShiftLeft: iValue := LValue shl RValue;
      opShiftRight: iValue := LValue shr RValue;
      else Exit(nil);
    end;
    DT := Sys.DataTypes[GetValueDataType(iValue)];
    Result := TIDIntConstant.CreateWithoutScope(DT, iValue);
  end;
  //////////////////////////////////////////////////////////////
  function CalcFloat(LValue, RValue: Extended; Operation: TOperatorID): TIDConstant;
  var
    fValue: Extended;
    bValue: Boolean;
    ValueDT: TIDType;
  begin
    ValueDT := Sys._Float64;
    case Operation of
      opAdd: fValue := LValue + RValue;
      opSubtract: fValue := LValue - RValue;
      opMultiply: fValue := LValue * RValue;
      opDivide: begin
        if RValue = 0 then
        begin
          if LValue = 0 then
            fValue := System.Math.NaN
          else
          if LValue < 0 then
            fValue := System.Math.NegInfinity
          else
            fValue := System.Math.Infinity;
        end else
          fValue := LValue / RValue;
      end;
      opNegative: begin
        fValue := -RValue;
        ValueDT := Right.DataType;
      end;
      opEqual,
      opNotEqual,
      opGreater,
      opGreaterOrEqual,
      opLess,
      opLessOrEqual: begin
        case Operation of
          opEqual: bValue := LValue = RValue;
          opNotEqual: bValue := LValue <> RValue;
          opGreater: bValue := LValue > RValue;
          opGreaterOrEqual: bValue := LValue >= RValue;
          opLess: bValue := LValue < RValue;
          opLessOrEqual: bValue := LValue <= RValue;
        else
          bValue := False;
        end;
        Exit(TIDBooleanConstant.CreateWithoutScope(Sys._Boolean, bValue));
      end;
    else
      Exit(nil);
    end;
    Result := TIDFloatConstant.CreateWithoutScope(ValueDT, fValue);
  end;
  //////////////////////////////////////////////////////////////
  function CalcBoolean(LValue, RValue: Boolean; Operation: TOperatorID): TIDConstant;
  var
    Value: Boolean;
  begin
    case Operation of
      opAnd: Value := LValue and RValue;
      opOr: Value := LValue or RValue;
      opXor: Value := LValue xor RValue;
      opNot: Value := not RValue;
      else Exit(nil);
    end;
    Result := TIDBooleanConstant.CreateWithoutScope(Sys._Boolean, Value);
  end;
  //////////////////////////////////////////////////////////////
  function CalcString(const LValue, RValue: string): TIDConstant;
  var
    sValue: string;
    bValue: Boolean;
  begin
    case Operation of
      opAdd: begin
        sValue := LValue + RValue;
        Result := TIDStringConstant.CreateWithoutScope(Sys._UnicodeString, sValue);
      end;
      opEqual,
      opNotEqual: begin
        case Operation of
          opEqual: bValue := LValue = RValue;
          opNotEqual: bValue := LValue <> RValue;
          else bValue := False;
        end;
        Exit(TIDBooleanConstant.CreateWithoutScope(Sys._Boolean, bValue));
      end;
      else Exit(nil);
    end;
  end;
  //////////////////////////////////////////////////////////////
  function CalcChar(const LValue, RValue: Char): TIDConstant;
  var
    sValue: string;
    bValue: Boolean;
  begin
    case Operation of
      opAdd: begin
        sValue := LValue + RValue;
        Result := TIDStringConstant.CreateWithoutScope(Sys._UnicodeString, sValue);
      end;
      opEqual,
      opNotEqual,
      opGreater,
      opGreaterOrEqual,
      opLess,
      opLessOrEqual: begin
        case Operation of
          opEqual: bValue := (LValue = RValue);
          opNotEqual: bValue := (LValue <> RValue);
          opGreater: bValue := (LValue > RValue);
          opGreaterOrEqual: bValue := (LValue >= RValue);
          opLess: bValue := (LValue < RValue);
          opLessOrEqual: bValue := (LValue <= RValue);
          else bValue := False;
        end;
        Exit(TIDBooleanConstant.CreateWithoutScope(Sys._Boolean, bValue));
      end;
    else
      Exit(nil);
    end;
  end;
  ///////////////////////////////////////////////////////////////
  function CalcIn(const Left: TIDConstant; const Right: TIDRangeConstant): TIDConstant;
  var
    LB, HB: TIDConstant;
    bValue: Boolean;
  begin
    LB := TIDConstant(Right.Value.LBExpression.Declaration);
    HB := TIDConstant(Right.Value.HBExpression.Declaration);
    bValue := (Left.CompareTo(LB) >= 0) and (Left.CompareTo(HB) <= 0);
    Result := TIDBooleanConstant.CreateWithoutScope(Sys._Boolean, bValue);
  end;

  function CalcDynArrays(const Left, Right: TIDConstant; Operation: TOperatorID): TIDConstant;
  begin
    // todo:
    case Operation of
      opEqual: Result := TIDBooleanConstant.CreateAsAnonymous(Left.Scope, Sys._Boolean, False);
      opMultiply: Result := Left; // todo:
    else
      Result := nil;
    end;
  end;

var
  L, R: TIDConstant;
  LeftType, RightType: TClass;
  Constant: TIDConstant;
begin
  L := Left.Declaration as TIDConstant;
  R := Right.Declaration as TIDConstant;
  LeftType := L.ClassType;
  RightType := R.ClassType;

  Constant := nil;
  if RightType = TIDRangeConstant then
  begin
    Constant := CalcIn(L, TIDRangeConstant(R));
  end else
  if LeftType = TIDIntConstant then
  begin
    if RightType = TIDIntConstant then
      Constant := CalcInteger(TIDIntConstant(L).Value, TIDIntConstant(R).Value, Operation)
    else
      Constant := CalcFloat(TIDIntConstant(L).Value, TIDFloatConstant(R).Value, Operation)
  end else
  if LeftType = TIDFloatConstant then
  begin
    if RightType = TIDIntConstant then
      Constant := CalcFloat(TIDFloatConstant(L).Value, TIDIntConstant(R).Value, Operation)
    else
      Constant := CalcFloat(TIDFloatConstant(L).Value, TIDFloatConstant(R).Value, Operation)
  end else
  if LeftType = TIDStringConstant then begin
    if RightType = TIDStringConstant then
      Constant := CalcString(TIDStringConstant(L).Value, TIDStringConstant(R).Value)
    else
      Constant := CalcString(TIDStringConstant(L).Value, TIDCharConstant(R).Value)
  end else
  if LeftType = TIDCharConstant then begin
    if RightType = TIDCharConstant then
      Constant := CalcChar(TIDCharConstant(L).Value, TIDCharConstant(R).Value)
    else
      Constant := CalcString(TIDCharConstant(L).Value, TIDStringConstant(R).Value)
  end else
  if LeftType = TIDBooleanConstant then
    Constant := CalcBoolean(TIDBooleanConstant(L).Value, TIDBooleanConstant(R).Value, Operation)
  else
  if (LeftType = TIDSetConstant) and (RightType = TIDSetConstant) then
  begin
    Constant := CalcSets(L, R, Operation);
  end else
  if (LeftType = TIDDynArrayConstant) and (RightType = TIDDynArrayConstant) then
  begin
    Constant := CalcDynArrays(L, R, Operation);
  end else
  if (LeftType = TIDPointerConstant) or (RightType = TIDPointerConstant) then
  begin
    Constant := CalcPointer(L, R, Operation)
  end else
    AbortWorkInternal('Const Calc: invalid arguments', L.SourcePosition);

  if not Assigned(Constant) then
    AbortWork('Operation %s not supported for constants', [OperatorFullName(Operation)], L.SourcePosition);
  Result := TIDExpression.Create(Constant, Right.TextPosition);

end;

{ TExpressionCalculator }

constructor TExpressionCalculator.Create(const Module: IASTDelphiUnit);
begin
  fSysDecls := Module.SystemDeclarations;
  fErrors := Module.Errors;
end;

end.
