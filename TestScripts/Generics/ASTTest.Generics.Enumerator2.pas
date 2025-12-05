unit ASTTest.Generics.Enumerator2;

interface

{$HINTS OFF}

type
  TArray<T> = array of T;
  
  TPair<TKey,TValue> = record
    Key: TKey;
    Value: TValue;
  end;

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
    function DoGetEnumerator: TEnumerator<T>; virtual; abstract;
  public
    function GetEnumerator: TEnumerator<T>; inline;
  end;
  
  TDictionary<K,V> = class(TEnumerable<TPair<K,V>>)
  private type
    TValueCollection = class(TEnumerable<V>)
    end;

    TKeyCollection = class(TEnumerable<K>)
    end;

    TPairEnumerator = class(TEnumerator<TPair<K,V>>)
    end;

  var
    FKeyCollection: TKeyCollection;
    FValueCollection: TValueCollection;
    FPairEnumerator: TPairEnumerator;
    function GetKeys: TKeyCollection;
    function GetValues: TValueCollection;
  protected
    function DoGetEnumerator: TEnumerator<TPair<K,V>>; override;
  public
    property Keys: TKeyCollection read GetKeys;
    property Values: TValueCollection read GetValues;
  end;      

  TTestRec = record
    IntVal: Integer;
  end;

var
  Dict: TDictionary<string, TTestRec>;

implementation

procedure Main;
begin
  var LInt := 0;
  for var LValue in Dict.Values do
    LInt := LInt + LValue.IntVal;

  var LStr := '';
  for var LKey in Dict.Keys do
    LStr := LStr + LKey;

  for var LPair in Dict do
  begin
    LStr := LStr + LPair.Key;
    LInt := LInt + LPair.Value.IntVal;
  end;
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

{ TDictionary<K, V> }

function TDictionary<K, V>.DoGetEnumerator: TEnumerator<TPair<K, V>>;
begin
  Result := FPairEnumerator;
end;

function TDictionary<K, V>.GetKeys: TKeyCollection;
begin
  Result := FKeyCollection;
end;

function TDictionary<K, V>.GetValues: TValueCollection;
begin
  Result := FValueCollection;
end;

end.