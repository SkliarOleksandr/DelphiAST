unit ASTTest.Generics.Array2;

interface

type

  TArray<T> = array of T;

  TArrayHelper = class
  public
    class function Concat<K>(const Arrays: array of TArray<K>): TArray<K>;
    class function Count<T>(const Arrays: array of T): Integer;
  end;
 
implementation

procedure Main;
var 
  Arr: TArray<TArray<string>>;
  Res: TArray<string>; 
begin
  Res := TArrayHelper.Concat<string>(Arr);
  
  // todo: fix assert
  // if TArrayHelper.Count(Res) > 0 then;
  // if TArrayHelper.Count(Arr) > 0 then;
end;

{ TArrayHelper }

class function TArrayHelper.Concat<K>(const Arrays: array of TArray<K>): TArray<K>;
begin
  Result := nil;
  for var LArr in Arrays do
    Result := Result + LArr;
end;

class function TArrayHelper.Count<T>(const Arrays: array of T): Integer;
begin
  Result := 0;
end;

end.