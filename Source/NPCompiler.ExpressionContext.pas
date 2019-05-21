unit NPCompiler.ExpressionContext;

interface

uses System.SysUtils, AST.Delphi.Classes, NPCompiler.Operators, NPCompiler.Contexts,
     IL.Types;

type
  TILInstruction = TObject;

//  PBoolExprNode = ^TBoolExprNode;
//  TBoolExprNode = record
//  type
//    TNodeOrientation = (NodeRoot, NodeLeft, NodeRight);
//    TNodeType = (
//      ntCmp,   // любое сравнение
//      ntAnd,   // логическое AND
//      ntOr     // логическое OR
//    );
//  var
//    NodeType: TNodeType;             // Тип нода
//    Parent: PBoolExprNode;           // Parent в дереве
//    Orientation: TNodeOrientation;   // Ориентация нода относительно Parent-а
//    Instruction: TILInstruction;     // это последняя инструкция выражения; для типа ntNode это JMP инструкция
//                                     // для типов ntAnd, ntOr это IL последняя инструкциия правого выражения
//    LeftChild: PBoolExprNode;        // Левый child
//    RightChild: PBoolExprNode;       // Правый child
//    Condition: TILCondition;         // Условия сравнения (только для типа ntNode)
//    LeftNode: PBoolExprNode;         // Левый нод (по исходному коду)
//    RightNode: PBoolExprNode;        // Правый нод (по исходному коду)
//    PrevNode: PBoolExprNode;         // Предыдущий нод (в стеке)
//  end;

  {expression context - use RPN (Reverse Polish Notation) stack}
  TEContext = record
  type
    TRPNItems = array of TOperatorID;
    TRPNStatus = (rprOk, rpOperand, rpOperation);
    TRPNError = (reDublicateOperation, reUnnecessaryClosedBracket, reUnclosedOpenBracket);
    TRPNPocessProc = function (var EContext: TEContext; OpID: TOperatorID): TIDExpression of object;
  private
    fRPNOArray: TRPNItems;              // Operations array
    fRPNEArray: TIDExpressions;         // Operands array
    fRPNOArrayLen: Integer;             // пердвычисленный размер входного списка
    fRPNOpCount: Integer;               // указывает на следющий свободный элемент входного списка
    fRPNEArrayLen: Integer;             // пердвычисленный размер выходного списка
    fRPNExprCount: Integer;             // указывает на следющий свободный элемент выходного списка
    fRPNLastOp: TOperatorID;
    fRPNPrevPriority: Integer;
    fProcessProc: TRPNPocessProc;
    fPosition: TExpessionPosition;      // позиция выражения (Nested, LValue, RValue...);
    procedure RPNCheckInputSize;
    function GetExpression: TIDExpression;
  public
    SContext: PSContext;             // statement контекст
    //LastBoolNode: PBoolExprNode;     // содерижт Root узел boolean выражений
    LastInstruction: TILInstruction; // последняя инструкция на момент начала выражения
    procedure Initialize(const ProcessProc: TRPNPocessProc);
    procedure Reset;                 // clear RPN stack and reinit
    procedure RPNPushExpression(Expr: TIDExpression);
    procedure RPNError(Status: TRPNError);
    procedure RPNPushOpenRaund;
    procedure RPNPushCloseRaund;
    procedure RPNEraiseTopOperator;
    procedure RPNFinish;
    property RPNExprCount: Integer read fRPNExprCount;
    property RPNLastOp: TOperatorID read fRPNLastOp;
    property Result: TIDExpression read GetExpression;
    property EPosition: TExpessionPosition read fPosition write fPosition;
    function RPNPopOperator: TIDExpression;
    function RPNPushOperator(OpID: TOperatorID): TRPNStatus;
    function RPNReadExpression(Index: Integer): TIDExpression; inline;
    function RPNLastOperator: TOperatorID;
    function RPNPopExpression: TIDExpression;
  end;
  PEContext = ^TEContext;


implementation

uses
  OPCompiler, NPCompiler.Errors;

{ TRPN }

procedure TEContext.RPNCheckInputSize;
begin
  if fRPNOpCount >= fRPNOArrayLen then begin
    Inc(fRPNOArrayLen, 8);
    SetLength(fRPNOArray, fRPNOArrayLen);
  end;
end;

procedure TEContext.RPNPushExpression(Expr: TIDExpression);
begin
  fRPNEArray[fRPNExprCount] := Expr;
  Inc(fRPNExprCount);
  if fRPNExprCount >= fRPNEArrayLen then begin
    Inc(fRPNEArrayLen, 8);
    SetLength(fRPNEArray, fRPNEArrayLen);
  end;
  fRPNLastOp := opNone;
end;

procedure TEContext.RPNError(Status: TRPNError);
begin
  case Status of
    reUnclosedOpenBracket: AbortWork(sUnclosedOpenBracket);
    reDublicateOperation: AbortWork(sDublicateOperationFmt);
  end;
end;

procedure TEContext.RPNPushOpenRaund;
begin
  fRPNOArray[fRPNOpCount] := opOpenRound;
  Inc(fRPNOpCount);
  RPNCheckInputSize;
  fRPNLastOp := opOpenRound;
end;

procedure TEContext.RPNPushCloseRaund;
var
  op: TOperatorID;
begin
  fRPNLastOp := opCloseRound;
  while fRPNOpCount > 0 do begin
    Dec(fRPNOpCount);
    op := fRPNOArray[fRPNOpCount];
    if op <> opOpenRound then begin
      fRPNEArray[fRPNExprCount] := fProcessProc(Self, Op);
      Inc(fRPNExprCount);
    end else
      Exit;
  end;
  RPNError(reUnnecessaryClosedBracket);
end;

function TEContext.RPNPopOperator: TIDExpression;
var
  Op: TOperatorID;
begin
  if fRPNOpCount > 0 then
  begin
    Dec(fRPNOpCount);
    Op := fRPNOArray[fRPNOpCount];
    Result := fProcessProc(Self, op);
  end else
    Result := nil;
end;

procedure TEContext.RPNFinish;
var
  op: TOperatorID;
  Expr: TIDExpression;
begin
  while fRPNOpCount > 0 do
  begin
    Dec(fRPNOpCount);
    op := fRPNOArray[fRPNOpCount];
    if op <> opOpenRound then
    begin
      Expr := fProcessProc(Self, op);
      fRPNEArray[fRPNExprCount] := Expr;
      if Assigned(Expr) then
        Inc(fRPNExprCount);

      if fRPNOpCount > 0 then
        fRPNLastOp := fRPNOArray[fRPNOpCount - 1]
      else
        fRPNLastOp := opNone;

      fRPNPrevPriority := cOperatorPriorities[fRPNLastOp];
    end else
      RPNError(reUnclosedOpenBracket);
  end;
end;

function TEContext.RPNPushOperator(OpID: TOperatorID): TRPNStatus;
var
  Priority: Integer;
  Op: TOperatorID;
begin
  if OpID = fRPNLastOp then
    RPNError(reDublicateOperation);

  fRPNLastOp := OpID;
  Priority := cOperatorPriorities[OpID];

  if cOperatorTypes[OpID] <> opUnarPrefix then
  begin
    if (Priority <= fRPNPrevPriority) then
    begin
    while fRPNOpCount > 0 do begin
      Op := fRPNOArray[fRPNOpCount - 1];
      if (cOperatorPriorities[Op] >= Priority) and (Op <> opOpenRound) then
      begin
        Dec(fRPNOpCount);
        fRPNEArray[fRPNExprCount] := fProcessProc(Self, Op);
        Inc(fRPNExprCount);
      end else
        Break;
    end;
  end;
  end;

  fRPNPrevPriority := Priority;
  fRPNOArray[fRPNOpCount] := OpID;
  Inc(fRPNOpCount);
  RPNCheckInputSize;
  Result := rpOperation;
end;

function TEContext.RPNReadExpression(Index: Integer): TIDExpression;
begin
  Result := fRPNEArray[Index];
end;

function TEContext.RPNLastOperator: TOperatorID;
begin
  if fRPNOpCount > 0 then
    Result := fRPNOArray[fRPNOpCount - 1]
  else
    Result := TOperatorID.opNone;
end;

function TEContext.RPNPopExpression: TIDExpression;
begin
  Dec(fRPNExprCount);
  if fRPNExprCount >= 0 then begin
    Result := fRPNEArray[fRPNExprCount];
    if Assigned(Result) then
      Exit;
  end;
  AbortWorkInternal('Empty Expression');
  Result := nil; // for prevent compiler warning
end;

procedure TEContext.Initialize(const ProcessProc: TRPNPocessProc);
begin
  SetLength(fRPNOArray, 4);
  fRPNOArrayLen := 4;
  SetLength(fRPNEArray, 8);
  fRPNEArrayLen := 8;
  fRPNOpCount := 0;
  fRPNExprCount := 0;
  fRPNLastOp := opNone;
  fRPNPrevPriority := 0;
  //LastBoolNode := nil;
  //LastInstruction := nil;
  fProcessProc := ProcessProc;
end;

procedure TEContext.RPNEraiseTopOperator;
begin
  Dec(fRPNOpCount);
end;

procedure TEContext.Reset;
begin
  fRPNLastOp := opNone;
  fRPNPrevPriority := 0;
  fRPNOpCount := 0;
  fRPNExprCount := 0;
  if Assigned(SContext) then
    LastInstruction := SContext.ILLast;
end;

function TEContext.GetExpression: TIDExpression;
begin
  if fRPNExprCount > 0 then
    Result := fRPNEArray[fRPNExprCount - 1]
  else
    Result := nil;
end;

end.
