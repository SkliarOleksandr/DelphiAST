unit AST.Classes;

interface

uses AST.Lexer, AST.Intf, AST.Parser.Utils, AST.Parser.ProcessStatuses, System.SysUtils;

type
  TASTItemTypeID = Integer;
  TASTTokenId = Integer;

  TASTItem = class;
  TASTProject = class;
  TASTModule = class;
  TASTDeclaration = class;

  TASTExpression = class;

  TASTExpressionArray = array of TASTExpression;

  TASTUnitClass = class of TASTModule;

  TCompilerResult = (
    CompileNone,
    CompileInProgress,
    CompileSuccess,
    CompileFail,
    CompileSkip
  );

  TASTItem = class(TPooledObject)
  private
    fParent: TASTItem;
    fNext: TASTItem;
    //function GetItemTypeID: TASTItemTypeID; virtual; abstract;
  protected
    function GetDisplayName: string; virtual;
  public
    constructor Create(Parent: TASTItem); virtual;
//    property TypeID: TASTItemTypeID read GetItemTypeID;
    property Next: TASTItem read fNext write fNext;
    property DisplayName: string read GetDisplayName;
    property Parent: TASTItem read fParent;
  end;

  TASTItemClass = class of TASTItem;


  TASTProjectSettings = class(TInterfacedObject, IASTProjectSettings)
  end;

  TASTProject = class(TInterfacedObject, IASTProject)
  private
    fOnProgress: TASTProgressEvent;
    fOnConsoleProc: TASTRrojectConsoleWriteEvent;
    procedure SetOnProgress(const Value: TASTProgressEvent);
    procedure SetOnConsoleWrite(const Value: TASTRrojectConsoleWriteEvent);
    function GetOnProgress: TASTProgressEvent;
    function GetOnConsoleWrite: TASTRrojectConsoleWriteEvent;
  protected
    function GetUnitClass: TASTUnitClass; virtual; abstract;
    function GetPointerSize: Integer; virtual; abstract;
    function GetNativeIntSize: Integer; virtual; abstract;
    function GetTotalLinesParsed: Integer; virtual;
  public
    constructor Create(const Name: string); virtual; abstract;
    property OnProgress: TASTProgressEvent read GetOnProgress write SetOnProgress;
    procedure CosoleWrite(const Module: IASTModule; Line: Integer; const Message: string);
  end;

  TASTParentItem = class(TASTItem)
  private
    fFirstChild: TASTItem;
    fLastChild: TASTItem;
  protected
    function GetDisplayName: string; override;
  public
    procedure AddChild(Item: TASTItem);
    property FirstChild: TASTItem read fFirstChild;
    property LastChild: TASTItem read fLastChild;
  end;

  TASTBlock = class(TASTParentItem)
  private
    function GetIsLoopBody: Boolean;
    function GetIsTryBlock: Boolean;
  public
    property IsLoopBody: Boolean read GetIsLoopBody;
    property IsTryBlock: Boolean read GetIsTryBlock;
  end;

  TEnumASTDeclProc = reference to procedure (const Module: TASTModule; const Decl: TASTDeclaration);

  TASTModule = class(TInterfacedObject, IASTModule)
  private
    fProject: IASTProject;
    fFileName: string;
  protected
    fTotalLinesParsed: Integer;
    function GetModuleName: string; virtual;
    function GetSource: string; virtual; abstract;
    procedure SetFileName(const Value: string);
    procedure Progress(StatusClass: TASTProcessStatusClass); virtual;
  public
    property Name: string read GetModuleName;
    property FileName: string read fFileName write SetFileName;
    property Project: IASTProject read fProject;
    function GetFirstFunc: TASTDeclaration; virtual; abstract;
    function GetFirstVar: TASTDeclaration; virtual; abstract;
    function GetFirstType: TASTDeclaration; virtual; abstract;
    function GetFirstConst: TASTDeclaration; virtual; abstract;
    function GetTotalLinesParsed: Integer;
    constructor Create(const Project: IASTProject; const FileName: string; const Source: string = ''); virtual;
    constructor CreateFromFile(const Project: IASTProject; const FileName: string); virtual;
    procedure EnumIntfDeclarations(const Proc: TEnumASTDeclProc); virtual; abstract;
    procedure EnumAllDeclarations(const Proc: TEnumASTDeclProc); virtual; abstract;
    property TotalLinesParsed: Integer read GetTotalLinesParsed;
  end;

  TASTDeclaration = class(TASTItem)
  protected
    fID: TIdentifier;
    fModule: TASTModule;
    function GetDisplayName: string; override;
  public
    property ID: TIdentifier read fID write fID;
    property Name: string read fID.Name write FID.Name;
    property TextPosition: TTextPosition read FID.TextPosition write FID.TextPosition;
    property SourcePosition: TTextPosition read FID.TextPosition;
    property Module: TASTModule read fModule;
  end;

  TASTDeclarations = array of TASTDeclaration;

  TASTOperation = class(TASTItem)
  end;

  TASTOperationClass = class of TASTOperation;

  TASTOpOpenRound = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpCloseRound = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpPlus = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpMinus = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpEqual = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpNotEqual = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpGrater = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpGraterEqual = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpLess = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpLessEqual = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpMul = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpDiv = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpIntDiv = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpMod = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpBinAnd = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpBinOr = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpBinXor = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpBinNot = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpLogicalAnd = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpLogicalOr = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpLogicalNot = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpShr = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpShl = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpRawCast = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpDynCast = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpCastCheck = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTOpCallProc = class(TASTOperation)
  private
    fProc: TASTDeclaration;
    fArgs: TASTExpressionArray;
  protected
    function GetDisplayName: string; override;
  public
    property Proc: TASTDeclaration read fProc write fProc;
    procedure AddArg(const Expr: TASTExpression);
  end;

  TASTOpArrayAccess = class(TASTOperation)
  private
    fIndexes: TASTExpressionArray;
  protected
    function GetDisplayName: string; override;
  public
    property Indexes: TASTExpressionArray read fIndexes write fIndexes;
    procedure AddIndex(Expr: TASTExpression);
  end;

  TASTOpMemberAccess = class(TASTOperation)
  protected
    function GetDisplayName: string; override;
  end;

  TASTEIDecl = class(TASTOperation)
  private
    fDecl: TASTDeclaration;
    fSPos: TTextPosition;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Decl: TASTDeclaration; const SrcPos: TTextPosition); reintroduce;
  end;

  TASTExpression = class(TASTParentItem)
  protected
    function GetDisplayName: string; override;
  public
    procedure AddSubItem(ItemClass: TASTOperationClass);
    procedure AddDeclItem(Decl: TASTDeclaration; const SrcPos: TTextPosition);
    function AddOperation<TASTClass: TASTOperation>: TASTClass;
  end;

  TASTKeyword = class(TASTItem)

  end;

  TASTOpAssign = class(TASTItem)
  private
    fDst: TASTExpression;
    fSrc: TASTExpression;
  protected
    function GetDisplayName: string; override;
  public
    property Dst: TASTExpression read fDst write fDst;
    property Src: TASTExpression read fSrc write fSrc;
  end;

  TASTKWGoTo = class(TASTKeyword)
  private
    fLabel: TASTDeclaration;
  protected
    function GetDisplayName: string; override;
  public
    property LabelDecl: TASTDeclaration read fLabel write fLabel;
  end;

  TASTKWLabel = class(TASTKeyword)
  private
    fLabel: TASTDeclaration;
  protected
    function GetDisplayName: string; override;
  public
    property LabelDecl: TASTDeclaration read fLabel write fLabel;
  end;

  TASTCall = class(TASTExpression)
  end;

  TASTVariable = class(TASTDeclaration)

  end;

  TASTKWExit = class(TASTKeyword)
  private
    fExpression: TASTExpression;
  protected
    function GetDisplayName: string; override;
  public
    property Expression: TASTExpression read fExpression write fExpression;
  end;



  TASTKWIF = class(TASTKeyword)
  type
    TASTKWIfThenBlock = class(TASTBlock) end;
    TASTKWIfElseBlock = class(TASTBlock) end;
  private
    fExpression: TASTExpression;
    fThenBody: TASTBlock;
    fElseBody: TASTBlock;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Parent: TASTItem = nil); override;
    property Expression: TASTExpression read fExpression write fExpression;
    property ThenBody: TASTBlock read fThenBody write fThenBody;
    property ElseBody: TASTBlock read fElseBody write fElseBody;
  end;

  TASTKWLoop = class(TASTKeyword)
  private
    fBody: TASTBlock;
  public
    constructor Create(Parent: TASTItem = nil); override;
    property Body: TASTBlock read fBody;
  end;

  TASTKWWhile = class(TASTKWLoop)
  private
    fExpression: TASTExpression;
  protected
    function GetDisplayName: string; override;
  public
    property Expression: TASTExpression read fExpression write fExpression;
  end;

  TASTKWRepeat = class(TASTKWLoop)
  private
    fExpression: TASTExpression;
  protected
    function GetDisplayName: string; override;
  public
    property Expression: TASTExpression read fExpression write fExpression;
  end;

  TDirection = (dForward, dBackward);

  TASTKWFor = class(TASTKWLoop)
  private
    fExprInit: TASTExpression;
    fExprTo: TASTExpression;
    fDirection: TDirection;
  protected
    function GetDisplayName: string; override;
  public
    property ExprInit: TASTExpression read fExprInit write fExprInit;
    property ExprTo: TASTExpression read fExprTo write fExprTo;
    property Direction: TDirection read fDirection write fDirection;
  end;

  TASTKWForIn = class(TASTKWLoop)
  private
    fVar: TASTExpression;
    fList: TASTExpression;
  protected
    function GetDisplayName: string; override;
  public
    property VarExpr: TASTExpression read fVar write fVar;
    property ListExpr: TASTExpression read fList write fList;
  end;

  TASTKWBreak = class(TASTKeyword)
  protected
    function GetDisplayName: string; override;
  end;

  TASTKWContinue = class(TASTKeyword)
  protected
    function GetDisplayName: string; override;
  end;

  TASTKWRaise = class(TASTKeyword)
  private
    fExpr: TASTExpression;
  protected
    function GetDisplayName: string; override;
  public
    property Expression: TASTExpression read fExpr write fExpr;
  end;

  TASTKWInherited = class(TASTKeyword)
  private
    fExpr: TASTExpression;
  protected
    function GetDisplayName: string; override;
  public
    property Expression: TASTExpression read fExpr write fExpr;
  end;

  TASTKWWith = class(TASTKeyword)
  private
    fExpressions: TASTExpressionArray;
    fBody: TASTBlock;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Parent: TASTItem); override;
    property Expressions: TASTExpressionArray read fExpressions;
    property Body: TASTBlock read fBody;
    procedure AddExpression(const Expr: TASTExpression);
  end;

  TASTExpBlockItem = class(TASTItem)
  private
    fExpression: TASTExpression;
    fBody: TASTBlock;
  public
    constructor Create(Parent: TASTItem); override;
    property Expression: TASTExpression read fExpression;
    property Body: TASTBlock read fBody;
  end;

  TASTKWTryExceptItem = class(TASTExpBlockItem)
  protected
    function GetDisplayName: string; override;
  end;

  TASTKWInheritedCall = class(TASTKeyword)
  private
    fProc: TASTExpression;
  protected
    function GetDisplayName: string; override;
  end;


  TASTKWCase = class(TASTKeyword)
  private
    fExpression: TASTExpression;
    fFirstItem: TASTExpBlockItem;
    fLastItem: TASTExpBlockItem;
    fElseBody: TASTBlock;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Parent: TASTItem); override;
    function AddItem(Expression: TASTExpression): TASTExpBlockItem;
    property Expression: TASTExpression read fExpression write fExpression;
    property FirstItem: TASTExpBlockItem read fFirstItem;
    property ElseBody: TASTBlock read fElseBody;
  end;

  TASTKWTryBlock = class(TASTKeyword)
  private
    fBody: TASTBlock;
    fFinallyBody: TASTBlock;
    fFirstExceptBlock: TASTKWTryExceptItem;
    fLastExceptBlock: TASTKWTryExceptItem;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Parent: TASTItem); override;
    property Body: TASTBlock read fBody;
    property FinallyBody: TASTBlock read fFinallyBody write fFinallyBody;
    property FirstExceptBlock: TASTKWTryExceptItem read fFirstExceptBlock;
    property LastExceptBlock: TASTKWTryExceptItem read fLastExceptBlock;
    function AddExceptBlock(Expression: TASTExpression): TASTKWTryExceptItem;
  end;

  TASTKWDeclSection = class(TASTKeyword)
  private
    fDecls: TASTDeclarations;
  public
    procedure AddDecl(const Decl: TASTDeclaration);
    property Decls: TASTDeclarations read fDecls;
  end;

  TASTKWInlineVarDecl = class(TASTKWDeclSection)
  private
    fExpression: TASTExpression;
  protected
    function GetDisplayName: string; override;
  public
    property Expression: TASTExpression read fExpression write fExpression;
  end;

  TASTKWInlineConstDecl = class(TASTKeyword)
  protected
    function GetDisplayName: string; override;
  end;

  TASTKWAsm = class(TASTKeyword)
  protected
    function GetDisplayName: string; override;
  end;

  TASTFunc = class(TASTDeclaration)
  private
    fBody: TASTBlock;
  protected
    property Body: TASTBlock read fBody;
  end;

  TASTType = class(TASTDeclaration)

  end;


implementation

uses System.StrUtils;

procedure TASTParentItem.AddChild(Item: TASTItem);
begin
  if Assigned(fLastChild) then
    fLastChild.Next := Item
  else
    fFirstChild := Item;

  fLastChild := Item;
end;

{ TASTExpression }

procedure TASTExpression.AddDeclItem(Decl: TASTDeclaration; const SrcPos: TTextPosition);
var
  Item: TASTEIDecl;
begin
  Item := TASTEIDecl.Create(Decl, SrcPos);
  AddChild(Item);
end;

function TASTExpression.AddOperation<TASTClass>: TASTClass;
begin
  Result := TASTClass.Create(Self);
  AddChild(Result);
end;

procedure TASTExpression.AddSubItem(ItemClass: TASTOperationClass);
var
  Item: TASTOperation;
begin
  Item := ItemClass.Create(Self);
  AddChild(Item);
end;

{ TASTUnit }

constructor TASTModule.Create(const Project: IASTProject; const FileName: string; const Source: string);
begin
  fProject := Project;
  fFileName := FileName;
end;

function TASTExpression.GetDisplayName: string;
var
  Item: TASTItem;
begin
  Result := '';
  Item := FirstChild;
  while Assigned(Item) do
  begin
    Result := AddStringSegment(Result, Item.DisplayName, ' ');
    Item := Item.Next;
  end;
end;

{ TASTItem }

constructor TASTItem.Create(Parent: TASTItem);
begin
  CreateFromPool;
  fParent := Parent;
end;

function TASTItem.GetDisplayName: string;
begin
  Result := '';
end;

{ TASTKWExit }

function TASTKWExit.GetDisplayName: string;
var
  ExprStr: string;
begin
  if Assigned(fExpression) then
    ExprStr := fExpression.DisplayName;
  Result := 'return ' + ExprStr;
end;

function TASTParentItem.GetDisplayName: string;
begin
end;

{ TASTEIOpenRound }

function TASTOpOpenRound.GetDisplayName: string;
begin
  Result := '(';
end;

{ TASTEICloseRound }

function TASTOpCloseRound.GetDisplayName: string;
begin
  Result := ')';
end;

{ TASTEIPlus }

function TASTOpPlus.GetDisplayName: string;
begin
  Result := '+';
end;

{ TASTEIMinus }

function TASTOpMinus.GetDisplayName: string;
begin
  Result := '-';
end;

{ TASTEIMul }

function TASTOpMul.GetDisplayName: string;
begin
  Result := '*';
end;

{ TASTEIDiv }

function TASTOpDiv.GetDisplayName: string;
begin
  Result := '/';
end;

{ TASTEIIntDiv }

function TASTOpIntDiv.GetDisplayName: string;
begin
  Result := 'div';
end;

{ TASTEIMod }

function TASTOpMod.GetDisplayName: string;
begin
  Result := 'mod';
end;

{ TASTEIEqual }

function TASTOpEqual.GetDisplayName: string;
begin
  Result := '=';
end;

{ TASTEINotEqual }

function TASTOpNotEqual.GetDisplayName: string;
begin
  Result := '<>';
end;

{ TASTEIGrater }

function TASTOpGrater.GetDisplayName: string;
begin
  Result := '>';
end;

{ TASTEIGraterEqual }

function TASTOpGraterEqual.GetDisplayName: string;
begin
  Result := '>=';
end;

{ TASTEILess }

function TASTOpLess.GetDisplayName: string;
begin
  Result := '<';
end;

{ TASTEILessEqual }

function TASTOpLessEqual.GetDisplayName: string;
begin
  Result := '<=';
end;

{ TASTEIVariable }

constructor TASTEIDecl.Create(Decl: TASTDeclaration; const SrcPos: TTextPosition);
begin
  CreateFromPool;
  fDecl := Decl;
  fSPos := SrcPos;
end;

function TASTEIDecl.GetDisplayName: string;
begin
  Result := fDecl.DisplayName;
end;

{ TASTDeclaration }

function TASTDeclaration.GetDisplayName: string;
begin
  Result := fID.Name;
end;

{ TASTKWAssign }

function TASTOpAssign.GetDisplayName: string;
begin
  Result := fDst.DisplayName + ' := ' + fSrc.DisplayName;
end;

{ TASTKWIF }

constructor TASTKWIF.Create(Parent: TASTItem);
begin
  inherited;
  fThenBody := TASTKWIfThenBlock.Create(Self);
end;

function TASTKWIF.GetDisplayName: string;
begin
  Result := 'IF ' + fExpression.DisplayName;
end;

{ TASTKWhile }

function TASTKWWhile.GetDisplayName: string;
begin
  Result := 'while ' + fExpression.DisplayName;
end;

{ TASTRepeat }

function TASTKWRepeat.GetDisplayName: string;
begin
  Result := 'repeat ' + fExpression.DisplayName;
end;

{ TASTKWWith }

procedure TASTKWWith.AddExpression(const Expr: TASTExpression);
begin
  fExpressions := fExpressions + [Expr];
end;

constructor TASTKWWith.Create(Parent: TASTItem);
begin
  inherited;
  fBody := TASTBlock.Create(Self);
end;

function TASTKWWith.GetDisplayName: string;
begin
  Result := 'with ';
end;

{ TASTKWFor }

function TASTKWFor.GetDisplayName: string;
begin
  Result := 'for ' + fExprInit.DisplayName + ' ' + ifthen(fDirection = dForward, 'to', 'downto') + ' ' + fExprTo.DisplayName;
end;


{ TASTKWLoop }

constructor TASTKWLoop.Create(Parent: TASTItem = nil);
begin
  inherited;
  fBody := TASTBlock.Create(Self);
end;

{ TASTKWSwitch }

function TASTKWCase.AddItem(Expression: TASTExpression): TASTExpBlockItem;
begin
  Result := TASTExpBlockItem.Create(Self);
  Result.fExpression := Expression;

  if Assigned(fLastItem) then
    fLastItem.Next := Result
  else
    fFirstItem := Result;

  fLastItem := Result;
end;

constructor TASTKWCase.Create(Parent: TASTItem);
begin
  inherited;
  fElseBody := TASTBlock.Create(Self);
end;

function TASTKWCase.GetDisplayName: string;
begin
  Result := 'case ' + fExpression.DisplayName;
end;

{ TASTKWSwitchItem }

constructor TASTExpBlockItem.Create(Parent: TASTItem);
begin
  inherited;
  fBody := TASTBlock.Create(Self);
end;

{ TASTBody }

function TASTBlock.GetIsLoopBody: Boolean;
var
  Item: TASTItem;
begin
  Result := fParent is TASTKWLoop;
  if not Result then
  begin
    Item := fParent;
    while Assigned(Item) do
    begin
       if (Item is TASTBlock) and TASTBlock(Item).IsLoopBody then
         Exit(True);
       Item := Item.Parent;
    end;
  end;
end;

function TASTBlock.GetIsTryBlock: Boolean;
begin
  Result := fParent.ClassType = TASTKWTryBlock;
end;

{ TASTKWBreak }

function TASTKWBreak.GetDisplayName: string;
begin
  Result := 'break';
end;

{ TASTKContinue }

function TASTKWContinue.GetDisplayName: string;
begin
  Result := 'continue';
end;

{ TASTKWTryBlock }

function TASTKWTryBlock.AddExceptBlock(Expression: TASTExpression): TASTKWTryExceptItem;
begin
  Result := TASTKWTryExceptItem.Create(Self);
  Result.fExpression := Expression;

  if Assigned(fLastExceptBlock) then
    fLastExceptBlock.Next := Result
  else
    fFirstExceptBlock := Result;

  fLastExceptBlock := Result;
end;

constructor TASTKWTryBlock.Create(Parent: TASTItem);
begin
  inherited;
  fBody := TASTBlock.Create(Self);
end;

function TASTKWTryBlock.GetDisplayName: string;
begin
  Result := 'try';
end;

{ TASTKWInherited }

function TASTKWInherited.GetDisplayName: string;
begin
  Result := 'inherited call';
  if Assigned(Expression) then
    Result := Result + ' ' + Expression.DisplayName;
end;

{ TASTKWRaise }

function TASTKWRaise.GetDisplayName: string;
begin
  Result := 'raise';
  if Assigned(Expression) then
    Result := Result + ' ' + Expression.DisplayName;
end;

{ TASTKWImmVarDecl }

function TASTKWInlineVarDecl.GetDisplayName: string;
begin
  Result := 'var';
  if Assigned(fExpression) then
    Result := Result + ' = ' + fExpression.DisplayName;
end;

{ TASTKWImmConstDecl }

function TASTKWInlineConstDecl.GetDisplayName: string;
begin
  Result := 'const';
end;

{ TASTKWDeclSection }

procedure TASTKWDeclSection.AddDecl(const Decl: TASTDeclaration);
begin
  fDecls := fDecls + [Decl];
end;

{ TASTKWGoTo }

function TASTKWGoTo.GetDisplayName: string;
begin
  Result := 'goto ' + fLabel.DisplayName;
end;

{ TASTKWLabel }

function TASTKWLabel.GetDisplayName: string;
begin
  Result := 'label ' + fLabel.DisplayName + ':';
end;

{ TASTKWTryExceptItem }

function TASTKWTryExceptItem.GetDisplayName: string;
begin
  if Assigned(fExpression) then
    Result := fExpression.DisplayName + ': '
  else
    Result := '';
end;

{ TASTKWAsm }

function TASTKWAsm.GetDisplayName: string;
begin
  Result := 'asm';
end;

{ TASTKWInheritedCall }

function TASTKWInheritedCall.GetDisplayName: string;
begin
  Result := 'inherited ' + fProc.DisplayName;
end;

{ TASTEICallProc }

procedure TASTOpCallProc.AddArg(const Expr: TASTExpression);
begin
  fArgs := fArgs + [Expr];
end;

function TASTOpCallProc.GetDisplayName: string;
var
  SArgs: string;
begin
  for var Arg in fArgs do
    SArgs := AddStringSegment(SArgs, Arg.DisplayName, ', ');
  Result := 'call ' + fProc.DisplayName + '(' + SArgs + ')';
end;

{ TASTKWForIn }

function TASTKWForIn.GetDisplayName: string;
begin
  Result := 'for ' + fVar.DisplayName + ' in ' + fList.DisplayName;
end;

{ TASTOpArrayAccess }

procedure TASTOpArrayAccess.AddIndex(Expr: TASTExpression);
begin
  fIndexes := fIndexes + [Expr];
end;

function TASTOpArrayAccess.GetDisplayName: string;
var
  SIndexes: string;
begin
  for var Expr in fIndexes do
    SIndexes := AddStringSegment(SIndexes, Expr.DisplayName, ', ');
  Result := '[' + SIndexes + ']';
end;

{ TASTOpMemberAccess }

function TASTOpMemberAccess.GetDisplayName: string;
begin
  Result := '.';
end;

{ TASTOpShr }

function TASTOpShr.GetDisplayName: string;
begin
  Result := 'shr';
end;

{ TASTOpShl }

function TASTOpShl.GetDisplayName: string;
begin
  Result := 'shl';
end;

{ TASTOpRawCast }

function TASTOpRawCast.GetDisplayName: string;
begin
  Result := 'rawcast';
end;

{ TASTOpDynCast }

function TASTOpDynCast.GetDisplayName: string;
begin
  Result := 'dyncast';
end;

{ TASTOpCastCheck }

function TASTOpCastCheck.GetDisplayName: string;
begin
  Result := 'castcheck';
end;

{ TASTOpBinAnd }

function TASTOpBinAnd.GetDisplayName: string;
begin
  Result := 'band';
end;

{ TASTOpBinOr }

function TASTOpBinOr.GetDisplayName: string;
begin
  Result := 'bor';
end;

{ TASTOpBinXor }

function TASTOpBinXor.GetDisplayName: string;
begin
  Result := 'bxor';
end;

{ TASTOpBinNot }

function TASTOpBinNot.GetDisplayName: string;
begin
  Result := 'bnot';
end;

{ TASTOpLogicalAnd }

function TASTOpLogicalAnd.GetDisplayName: string;
begin
  Result := 'land';
end;

{ TASTOpLogicalOr }

function TASTOpLogicalOr.GetDisplayName: string;
begin
  Result := 'lor';
end;

{ TASTOpLogicalNot }

function TASTOpLogicalNot.GetDisplayName: string;
begin
  Result := 'lnot';
end;

constructor TASTModule.CreateFromFile(const Project: IASTProject; const FileName: string);
begin
  fFileName := FileName;
end;

function TASTModule.GetModuleName: string;
begin
  Result := ExtractFileName(fFileName);
end;

function TASTModule.GetTotalLinesParsed: Integer;
begin
  Result := fTotalLinesParsed;
end;

procedure TASTModule.Progress(StatusClass: TASTProcessStatusClass);
var
  Event: TASTProgressEvent;
begin
  Event := fProject.OnProgress;
  if Assigned(Event) then
    Event(Self, StatusClass);
end;

procedure TASTModule.SetFileName(const Value: string);
begin
  fFileName := Value;
end;

{ TASTProject }

procedure TASTProject.CosoleWrite(const Module: IASTModule; Line: Integer; const Message: string);
begin
  if Assigned(fOnConsoleProc) then
    fOnConsoleProc(Module, Line, Message);
end;

function TASTProject.GetOnConsoleWrite: TASTRrojectConsoleWriteEvent;
begin
  Result := fOnConsoleProc;
end;

function TASTProject.GetOnProgress: TASTProgressEvent;
begin
  Result := fOnProgress;
end;

function TASTProject.GetTotalLinesParsed: Integer;
begin
  Result := 0;
end;

procedure TASTProject.SetOnConsoleWrite(const Value: TASTRrojectConsoleWriteEvent);
begin
  fOnConsoleProc := Value;
end;

procedure TASTProject.SetOnProgress(const Value: TASTProgressEvent);
begin
  fOnProgress := Value;
end;

end.
