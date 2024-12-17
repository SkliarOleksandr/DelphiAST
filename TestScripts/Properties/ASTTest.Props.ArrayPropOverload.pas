unit ASTTest.Props.ArrayPropOverload;

interface

{$HINTS OFF}

type
  TList = record
  private
    function GetItem(AIndex: Integer): Integer; overload;
    function GetItem(AIndex: string): Integer; overload;   
  public  
    property Items[AIndex: Integer]: Integer read GetItem; default;    
   // property Items[AIndex: string]: Integer read GetItem; default;
  end; 

implementation

procedure Main;
var
  LList: TList;
begin
  var V1 := LList[0];
  //var V2 := LList['index'];
end;

{ TList }

function TList.GetItem(AIndex: string): Integer;
begin
  Result := 0;
end;

function TList.GetItem(AIndex: Integer): Integer;
begin
  Result := 0;
end;

end.