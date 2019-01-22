unit AST.Classes;

interface

uses AST.Project;

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
  end;

  TASTModule = class
  private

  public
    constructor Create(const Project: IASTProject; const Source: string = ''); virtual;
  end;

  TASTNamedItem = class(TASTItem)
  private
    fName: string;
  public
    property Name: string read fName write fName;
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

  TASTVariable = class(TASTNamedItem)

  end;

  TASTKWExit = class(TASTKeyword)
  private
    fExpression: TASTExpression;
  public
    property Expression: TASTExpression read fExpression write fExpression;
  end;

  TASTFunc = class(TASTNamedItem)

  end;

  TASTType = class(TASTNamedItem)

  end;


implementation

uses iDStringParser;

{ TASTExpression }

procedure TASTExpression.AddSubItem(ItemClass: TASTExpressionItemClass);
var
  Item: TASTExpressionItem;
begin
  Item := ItemClass.Create();
  if Assigned(fLastChild) then
    fLastChild.Next := Item
  else begin
    fFirstChild := Item;
    fLastChild := Item;
  end;

end;

{ TASTUnit }

constructor TASTModule.Create(const Project: IASTProject; const Source: string);
begin

end;

end.
