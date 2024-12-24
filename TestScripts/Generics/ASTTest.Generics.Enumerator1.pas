unit ASTTest.Generics.Enumerator1;

interface

{$HINTS OFF}

type
  TArray<T> = array of T;

  TEnumerator<T> = class abstract
  protected
    function DoGetCurrent: T; virtual; abstract;
    function DoMoveNext: Boolean; virtual; abstract;
  public
    property Current: T read DoGetCurrent;
    function MoveNext: Boolean; inline;
  end;

  TEnumerable<T> = class abstract
  protected
    //function ToArrayImpl(Count: NativeInt): TArray<T>; // used by descendants
    function DoGetEnumerator: TEnumerator<T>; virtual; abstract;
  public
    //destructor Destroy; override;
    function GetEnumerator: TEnumerator<T>; inline;
    //function ToArray: TArray<T>; virtual;
  end;

  TList<T> = class
  type
    TCollection = class(TEnumerable<T>)
    end;
  private
    FCollection: TCollection;
    function GetCollection: TCollection;
  public
    property Values: TCollection read GetCollection;
  end;

  TRec = record
    V: Integer;
  end;
   
implementation

var
  List: TList<TRec>;

procedure Main;
begin
  for var LItem in List.Values do
    if  LItem.V > 0 then;
end;

{ TList<T> }

function TList<T>.GetCollection: TCollection;
begin
  Result := FCollection;
end;

{ TEnumerator<T> }

function TEnumerator<T>.MoveNext: Boolean;
begin
  Result := True;
end;

{ TEnumerable<T> }

function TEnumerable<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := DoGetEnumerator;
end;

end.