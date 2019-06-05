unit NPCompiler.Contexts;

interface

uses System.SysUtils, AST.Delphi.Classes, AST.Lexer;

type
  TILInstruction = TObject;
  TIL = TObject;

  {try context - контекст оброботки исключений}
  PTryContext = ^TTryContext;
  TTryContext = record
  type
    TExitType = (etCallFinally, etJumpToFinally);
    TExitListItem = record
      ExitType: TExitType;
      Instruction: TILInstruction;
    end;
    PExitListItem = ^TExitListItem;
    TExitList = array of TExitListItem;
    TTrySection = (SectionTry, SectionFinally, SectionExcept);
  var
    Parent: PTryContext;   // внешняя try... секция
    ExitList: TExitList;   // список инструкций [EXIT, BREAK, CONTINUE] из try... секции, которые надо перенапрвить на секцию FINALLY (если она имеется)
    Section: TTrySection;
    procedure AddExit(ExitType: TExitType; const Instruction: TILInstruction);
  end;

  {loop context - контекст цикла}
  PLContext = ^TLContext;
  TLContext = record
    Parent: PLContext;               // цикл верхнего уровня
    TryContext: PTryContext;         // try контекст, который был на момент начала цикла
    BeginInstuction: TILInstruction; // инструкция начала итерации цикла (код проверки условия или тело)
    EndInstruction: TILInstruction;  // последняя инструкции тела цикла
  end;

  {statements context - контекст кода процедуры}
  PSContext = ^TSContext;
  TSContext = record
    IL: TIL;
    WriteIL: Boolean;                // показывает нужно ли генерить IL код
    ExpandMacro: Boolean;            // показывает нужно ли раскрывать макрос или нужно взять его как строку
    LContext: PLContext;             // контекст цикла  (если есть)
    TryBlock: PTryContext;           // указывает на текущую try... секцию (если есть)
    Proc: TIDProcedure;
    procedure Initialize;
    procedure Assign(const Source: PSContext);
    procedure ILWrite(Instruction: TILInstruction); inline;
    function ILLast: TILInstruction; inline;
    function ILFirst: TILInstruction; inline;
    function GetTMPVar(DataType: TIDType): TIDVariable; overload; inline;
    function GetTMPVar(DataType: TIDType; VarFlags: TVariableFlags): TIDVariable; overload; inline;
    function GetTMPVarExpr(DataType: TIDType; const TextPos: TTextPosition): TIDExpression; inline;
  end;

implementation

{ TTryContext }

procedure TTryContext.AddExit(ExitType: TExitType; const Instruction: TILInstruction);
var
  l: Integer;
  PItem: PExitListItem;
begin
  l := Length(ExitList);
  SetLength(ExitList, l + 1);
  PItem := @ExitList[l];
  PItem.ExitType := ExitType;
  PItem.Instruction := Instruction;
end;

{ TSContext }

procedure TSContext.Assign(const Source: PSContext);
begin
  IL := Source.IL;
  Proc := Source.Proc;
  WriteIL := Source.WriteIL;
  ExpandMacro := Source.ExpandMacro;
  LContext := Source.LContext;
  TryBlock := Source.TryBlock;
end;

function TSContext.GetTMPVar(DataType: TIDType): TIDVariable;
begin
  Result := Proc.GetTMPVar(DataType);
end;

function TSContext.GetTMPVar(DataType: TIDType; VarFlags: TVariableFlags): TIDVariable;
begin
  Result := Proc.GetTMPVar(DataType, VarFlags);
end;

function TSContext.GetTMPVarExpr(DataType: TIDType; const TextPos: TTextPosition): TIDExpression;
begin
  Result := TIDExpression.Create(Proc.GetTMPVar(DataType), TextPos);
end;

function TSContext.ILFirst: TILInstruction;
begin
  Result := nil;
end;

function TSContext.ILLast: TILInstruction;
begin
  Result := nil;
end;

procedure TSContext.ILWrite(Instruction: TILInstruction);
begin
//  if WriteIL then
//    IL.Write(Instruction);
end;

procedure TSContext.Initialize;
begin
  IL := nil;
  LContext := nil;
  WriteIL := True;
  ExpandMacro := True;
  TryBlock := nil;
  Proc := nil;
end;

end.
