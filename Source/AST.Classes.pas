unit AST.Classes;

interface

type
  TASTItemTypeID = Integer;
  TASTTokenId = Integer;

  TASTItem = class
  private
    function GetItemTypeID: TASTItemTypeID; virtual; abstract;
  public
    property TypeID: TASTItemTypeID read GetItemTypeID;
  end;

  TASTNamedItem = class(TASTItem)
  private
    fName: string;
  public
    property Name: string read fName write fName;
  end;


  TASTExpressionItem = class
  end;

  TASTExpressionItemClass = class of TASTExpressionItem;


  TASTExprOpenRound = class(TASTExpressionItem) end;
  TASTExprCloseRound = class(TASTExpressionItem) end;

  TASTExprItemes = array of TASTExpressionItemClass;

  TASTExpression = class(TASTItem)
  private
    fItems: TASTExprItemes;
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
begin
  fItems := fItems + [ItemClass];
end;

end.
