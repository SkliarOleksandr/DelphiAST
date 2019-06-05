unit NPCompiler.Contexts;

interface

uses System.SysUtils, AST.Delphi.Classes, AST.Lexer;

type
  TILInstruction = TObject;
  TIL = TObject;

  {try context - �������� ��������� ����������}
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
    Parent: PTryContext;   // ������� try... ������
    ExitList: TExitList;   // ������ ���������� [EXIT, BREAK, CONTINUE] �� try... ������, ������� ���� ������������ �� ������ FINALLY (���� ��� �������)
    Section: TTrySection;
    procedure AddExit(ExitType: TExitType; const Instruction: TILInstruction);
  end;

  {loop context - �������� �����}
  PLContext = ^TLContext;
  TLContext = record
    Parent: PLContext;               // ���� �������� ������
    TryContext: PTryContext;         // try ��������, ������� ��� �� ������ ������ �����
    BeginInstuction: TILInstruction; // ���������� ������ �������� ����� (��� �������� ������� ��� ����)
    EndInstruction: TILInstruction;  // ��������� ���������� ���� �����
  end;

  {statements context - �������� ���� ���������}
  PSContext = ^TSContext;
  TSContext = record
    IL: TIL;
    WriteIL: Boolean;                // ���������� ����� �� �������� IL ���
    ExpandMacro: Boolean;            // ���������� ����� �� ���������� ������ ��� ����� ����� ��� ��� ������
    LContext: PLContext;             // �������� �����  (���� ����)
    TryBlock: PTryContext;           // ��������� �� ������� try... ������ (���� ����)
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
