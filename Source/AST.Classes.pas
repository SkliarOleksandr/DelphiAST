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
  public
    property TypeID: TASTItemTypeID read GetItemTypeID;
    property Next: TASTItem read fNext write fNext;
  end;

  TASTProject = class(TInterfacedObject, IASTProject)
  protected
    function GetUnitClass: TASTUnitClass; virtual; abstract;
  end;

  TASTParentItem = class(TASTItem)
  private
    fFirstChild: TASTItem;
    fLastChild: TASTItem;
  public
    procedure AddChild(Item: TASTItem);
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

  TASTEIOpenRound = class(TASTExpressionItem) end;
  TASTEICloseRound = class(TASTExpressionItem) end;
  TASTEIPlus = class(TASTExpressionItem) end;
  TASTEIMinus = class(TASTExpressionItem) end;
  TASTEIMul = class(TASTExpressionItem) end;
  TASTEIDiv = class(TASTExpressionItem) end;
  TASTEIIntDiv = class(TASTExpressionItem) end;
  TASTEIMod = class(TASTExpressionItem) end;


  TASTExprItemes = array of TASTExpressionItemClass;

  TASTExpression = class(TASTParentItem)
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

end.
