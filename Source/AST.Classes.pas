unit AST.Classes;

interface

uses iDStringParser, AST.Project;

type
  TASTItemTypeID = Integer;
  TASTTokenId = Integer;

  TASTItem = class;
  TASTProject = class;
  TASTModule = class;

  TASTUnitClass = class of TASTModule;

  TASTItem = class
  private
    fNext: TASTItem;
    function GetItemTypeID: TASTItemTypeID; virtual; abstract;
  protected
    function GetDisplayName: string; virtual;
  public
    property TypeID: TASTItemTypeID read GetItemTypeID;
    property Next: TASTItem read fNext write fNext;
    property DisplayName: string read GetDisplayName;
  end;

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

  TASTModule = class
  private

  public
    constructor Create(const Project: IASTProject; const Source: string = ''); virtual;
  end;

  TASTDeclaration = class(TASTItem)
  private
    fID: TIdentifier;
  public
    property ID: TIdentifier read fID write fID;
    property Name: string read fID.Name;
  end;

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


  TASTExprItemes = array of TASTExpressionItemClass;

  TASTExpression = class(TASTParentItem)
  protected
    function GetDisplayName: string; override;
  public
    procedure AddSubItem(ItemClass: TASTExpressionItemClass);
  end;

  TASTKeyword = class(TASTItem)

  end;

  TASTKWAssign = class(TASTKeyword)
  private
    fDest: TASTExpression;
    fSource: TASTExpression;
  end;

  TASTCall = class(TASTExpression)
    //fFunc: TASt
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

  TASTBody = class(TASTParentItem)

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

uses NPCompiler.Utils;

procedure TASTParentItem.AddChild(Item: TASTItem);
begin
  if Assigned(fLastChild) then
    fLastChild.Next := Item
  else begin
    fFirstChild := Item;
    fLastChild := Item;
  end;
end;

{ TASTExpression }

procedure TASTExpression.AddSubItem(ItemClass: TASTExpressionItemClass);
var
  Item: TASTExpressionItem;
begin
  Item := ItemClass.Create();
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

end.
