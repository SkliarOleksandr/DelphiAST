unit ASTTest.Props.ArrayPropOverload2;

interface

{$HINTS OFF}

type
  TBaseList = class
  private
    function GetItem(AIndex: Integer): Integer; overload;
  public
    property Items[AIndex: Integer]: Integer read GetItem; default;
  end;

  TList = class(TBaseList)
  private
    function GetItem(AIndex: string): Integer; overload;
  public
    property Items[AIndex: string]: Integer read GetItem; default;
  end;   

implementation

var
  List: TList;

procedure Main();
begin
  var V1 := List[0];
  var V2 := List['index'];
end;

{TBaseList}

function TBaseList.GetItem(AIndex: Integer): Integer;
begin
  Result := 0;
end;

{ TList }

function TList.GetItem(AIndex: string): Integer;
begin
  Result := 0;
end;

end.