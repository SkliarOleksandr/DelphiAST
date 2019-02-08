unit AST.Classes;

interface

uses iDStringParser, AST.Project, NPCompiler.Utils;

type
  TASTItemTypeID = Integer;
  TASTTokenId = Integer;

  TASTItem = class;
  TASTProject = class;
  TASTModule = class;
  TASTDeclaration = class;

  TASTUnitClass = class of TASTModule;

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

  TASTProject = class(TInterfacedObject, IASTProject)
  protected
    function GetUnitClass: TASTUnitClass; virtual; abstract;
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

  TASTBody = class(TASTParentItem)
  private
    function GetIsLoopBody: Boolean;
  public
    property IsLoopBody: Boolean read GetIsLoopBody;
  end;


  TASTModule = class
  private

  protected
    function GetModuleName: string; virtual; abstract;
  public
    property Name: string read GetModuleName;

    function GetFirstFunc: TASTDeclaration; virtual; abstract;
    function GetFirstVar: TASTDeclaration; virtual; abstract;
    function GetFirstType: TASTDeclaration; virtual; abstract;
    function GetFirstConst: TASTDeclaration; virtual; abstract;

    constructor Create(const Project: IASTProject; const Source: string = ''); virtual;
  end;

  TASTDeclaration = class(TASTItem)
  protected
    fID: TIdentifier;
    function GetDisplayName: string; override;
  public
    property ID: TIdentifier read fID write fID;
    property Name: string read fID.Name write FID.Name;
    property TextPosition: TTextPosition read FID.TextPosition write FID.TextPosition;
    property SourcePosition: TTextPosition read FID.TextPosition;
  end;

  TASTDeclarations = array of TASTDeclaration;

  TASTExpressionItem = class(TASTItem)
  end;

  TASTExpressionItemClass = class of TASTExpressionItem;

  TASTEIOpenRound = class(TASTExpressionItem)
  protected
    function GetDisplayName: string; override;
  end;

  TASTEICloseRound = class(TASTExpressionItem)
  protected
    function GetDisplayName: string; override;
  end;

  TASTEIPlus = class(TASTExpressionItem)
  protected
    function GetDisplayName: string; override;
  end;

  TASTEIMinus = class(TASTExpressionItem)
  protected
    function GetDisplayName: string; override;
  end;

  TASTEIEqual = class(TASTExpressionItem)
  protected
    function GetDisplayName: string; override;
  end;

  TASTEINotEqual = class(TASTExpressionItem)
  protected
    function GetDisplayName: string; override;
  end;

  TASTEIGrater = class(TASTExpressionItem)
  protected
    function GetDisplayName: string; override;
  end;

  TASTEIGraterEqual = class(TASTExpressionItem)
  protected
    function GetDisplayName: string; override;
  end;

  TASTEILess = class(TASTExpressionItem)
  protected
    function GetDisplayName: string; override;
  end;

  TASTEILessEqual = class(TASTExpressionItem)
  protected
    function GetDisplayName: string; override;
  end;

  TASTEIMul = class(TASTExpressionItem)
  protected
    function GetDisplayName: string; override;
  end;

  TASTEIDiv = class(TASTExpressionItem)
  protected
    function GetDisplayName: string; override;
  end;

  TASTEIIntDiv = class(TASTExpressionItem)
  protected
    function GetDisplayName: string; override;
  end;

  TASTEIMod = class(TASTExpressionItem)
  protected
    function GetDisplayName: string; override;
  end;

  TASTEIDecl = class(TASTExpressionItem)
  private
    fDecl: TASTDeclaration;
    fSPos: TTextPosition;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Decl: TASTDeclaration; const SrcPos: TTextPosition);
  end;

  TASTExpression = class(TASTParentItem)
  protected
    function GetDisplayName: string; override;
  public
    procedure AddSubItem(ItemClass: TASTExpressionItemClass);
    procedure AddDeclItem(Decl: TASTDeclaration; const SrcPos: TTextPosition);
  end;

  TASTKeyword = class(TASTItem)
  end;

  TASTKWAssign = class(TASTKeyword)
  private
    fDst: TASTExpression;
    fSrc: TASTExpression;
  protected
    function GetDisplayName: string; override;
  public
    property Dst: TASTExpression read fDst write fDst;
    property Src: TASTExpression read fSrc write fSrc;
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
  private
    fExpression: TASTExpression;
    fThenBody: TASTBody;
    fElseBody: TASTBody;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Parent: TASTItem = nil); override;
    property Expression: TASTExpression read fExpression write fExpression;
    property ThenBody: TASTBody read fThenBody write fThenBody;
    property ElseBody: TASTBody read fElseBody write fElseBody;
  end;


  TASTKWLoop = class(TASTKeyword)
  private
    fBody: TASTBody;
  public
    constructor Create(Parent: TASTItem = nil); override;
    property Body: TASTBody read fBody;
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

  TASTExpressionArray = array of TASTExpression;

  TASTKWWith = class(TASTKeyword)
  private
    fExpressions: TASTExpressionArray;
    fBody: TASTBody;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Parent: TASTItem); override;
    property Expressions: TASTExpressionArray read fExpressions;
    property Body: TASTBody read fBody;
    procedure AddExpression(const Expr: TASTExpression);
  end;

  TASTKWCaseItem = class(TASTItem)
  private
    fExpression: TASTExpression;
    fBody: TASTBody;
  public
    constructor Create(Parent: TASTItem); override;
    property Expression: TASTExpression read fExpression;
    property Body: TASTBody read fBody;
  end;

  TASTKWCase = class(TASTKeyword)
  private
    fExpression: TASTExpression;
    fFirstItem: TASTKWCaseItem;
    fLastItem: TASTKWCaseItem;
    fElseBody: TASTBody;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Parent: TASTItem); override;
    function AddItem(Expression: TASTExpression): TASTKWCaseItem;
    property Expression: TASTExpression read fExpression write fExpression;
    property FirstItem: TASTKWCaseItem read fFirstItem;
    property ElseBody: TASTBody read fElseBody;
  end;

  TASTKWTryFinally = class(TASTKeyword)
  private
    fBody: TASTBody;
    fFinallyBody: TASTBody;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Parent: TASTItem); override;
  end;

  TASTKWTryExcept = class(TASTKeyword)
  private
    fBody: TASTBody;
    //fExceptBody: TASTBody;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Parent: TASTItem); override;
  end;

  TASTKWDeclSection = class(TASTKeyword)
  private
    fDecls: TASTDeclarations;
  public
    procedure AddDecl(const Decl: TASTDeclaration);
    property Decls: TASTDeclarations read fDecls;
  end;

  TASTKWInlineVarDecl = class(TASTKWDeclSection)
  protected
    function GetDisplayName: string; override;
  end;

  TASTKWInlineConstDecl = class(TASTKeyword)
  protected
    function GetDisplayName: string; override;
  end;


  TASTFunc = class(TASTDeclaration)
  private
    fBody: TASTBody;
  protected
    property Body: TASTBody read fBody;
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

procedure TASTExpression.AddSubItem(ItemClass: TASTExpressionItemClass);
var
  Item: TASTExpressionItem;
begin
  Item := ItemClass.Create(Self);
  AddChild(Item);
end;

{ TASTUnit }

constructor TASTModule.Create(const Project: IASTProject; const Source: string);
begin

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

function TASTEIOpenRound.GetDisplayName: string;
begin
  Result := '(';
end;

{ TASTEICloseRound }

function TASTEICloseRound.GetDisplayName: string;
begin
  Result := ')';
end;

{ TASTEIPlus }

function TASTEIPlus.GetDisplayName: string;
begin
  Result := '+';
end;

{ TASTEIMinus }

function TASTEIMinus.GetDisplayName: string;
begin
  Result := '-';
end;

{ TASTEIMul }

function TASTEIMul.GetDisplayName: string;
begin
  Result := '*';
end;

{ TASTEIDiv }

function TASTEIDiv.GetDisplayName: string;
begin
  Result := '/';
end;

{ TASTEIIntDiv }

function TASTEIIntDiv.GetDisplayName: string;
begin
  Result := 'div';
end;

{ TASTEIMod }

function TASTEIMod.GetDisplayName: string;
begin
  Result := 'mod';
end;

{ TASTEIEqual }

function TASTEIEqual.GetDisplayName: string;
begin
  Result := '=';
end;

{ TASTEINotEqual }

function TASTEINotEqual.GetDisplayName: string;
begin
  Result := '<>';
end;

{ TASTEIGrater }

function TASTEIGrater.GetDisplayName: string;
begin
  Result := '>';
end;

{ TASTEIGraterEqual }

function TASTEIGraterEqual.GetDisplayName: string;
begin
  Result := '>=';
end;

{ TASTEILess }

function TASTEILess.GetDisplayName: string;
begin
  Result := '<';
end;

{ TASTEILessEqual }

function TASTEILessEqual.GetDisplayName: string;
begin
  Result := '<=';
end;

{ TASTEIVariable }

constructor TASTEIDecl.Create(Decl: TASTDeclaration; const SrcPos: TTextPosition);
begin
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

function TASTKWAssign.GetDisplayName: string;
begin
  Result := fDst.DisplayName + ' := ' + fSrc.DisplayName;
end;

{ TASTKWIF }

constructor TASTKWIF.Create(Parent: TASTItem);
begin
  fThenBody := TASTBody.Create(Self);
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
  fBody := TASTBody.Create(Self);
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
  fBody := TASTBody.Create(Self);
end;

{ TASTKWSwitch }

function TASTKWCase.AddItem(Expression: TASTExpression): TASTKWCaseItem;
begin
  Result := TASTKWCaseItem.Create(Self);
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
  fElseBody := TASTBody.Create(Self);
end;

function TASTKWCase.GetDisplayName: string;
begin
  Result := 'case ' + fExpression.DisplayName;
end;

{ TASTKWSwitchItem }

constructor TASTKWCaseItem.Create(Parent: TASTItem);
begin
  inherited;
  fBody := TASTBody.Create(Self);
end;

{ TASTBody }

function TASTBody.GetIsLoopBody: Boolean;
begin
  Result := fParent is TASTKWLoop;
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

{ TASTTryFinally }

constructor TASTKWTryFinally.Create(Parent: TASTItem);
begin
  inherited;
  fBody := TASTBody.Create(Self);
  fFinallyBody := TASTBody.Create(Self);
end;

function TASTKWTryFinally.GetDisplayName: string;
begin
  Result := 'try_finnaly';
end;

{ TASTTryExcept }

constructor TASTKWTryExcept.Create(Parent: TASTItem);
begin
  inherited;
  fBody := TASTBody.Create(Self);
end;

function TASTKWTryExcept.GetDisplayName: string;
begin
  Result := 'try_except';
end;

{ TASTKWInherited }

function TASTKWInherited.GetDisplayName: string;
begin

end;

{ TASTKWRaise }

function TASTKWRaise.GetDisplayName: string;
begin

end;

{ TASTKWImmVarDecl }

function TASTKWInlineVarDecl.GetDisplayName: string;
begin
  Result := 'var';
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

end.
