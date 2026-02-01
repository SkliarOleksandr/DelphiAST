unit ASTTest.Procs.VarOutParams2;

interface

type

  TArrayOf = array of Integer;

  TList = class
  private
    FItems: TArrayOf;
    function GetList: TArrayOf;
  public
    property List: TArrayOf read GetList;
    procedure Work;
  end;

implementation

{ TList }

procedure DoWork(var AValue: array of Integer);
begin
end;

function TList.GetList: TArrayOf;
begin
  Result := FItems;
end;

procedure TList.Work;
begin
  DoWork(List);
end;

end.