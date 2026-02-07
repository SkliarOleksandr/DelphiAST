unit ASTTest.Generics.ProcType2;

interface

{$HINTS OFF}

type
  TArray<T> = array of T;

  TRecord = record
  type
    TFunc<TRes> = function: TArray<TRes>;
  public
    function GetObject<K>(const AFunc: TFunc<K>): K;
  end;  

implementation

{ TRecord }

function TRecord.GetObject<K>(const AFunc: TFunc<K>): K;
var
  LArray: TArray<K>;
begin
  LArray := AFunc();
  Result := LArray[0];
end;

end.