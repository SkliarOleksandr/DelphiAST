unit ASTTest.Generics.Arrays;

interface

type

  TArray<T> = array of T;

  TArrayHelper = class
  public
    class function Concat<T>(const Arrays: array of TArray<T>): TArray<T>;
    class function Count<T>(const Arrays: array of T): Integer;          
  end;
 
implementation

procedure Main;
var 
  Arr: TArray<TArray<string>>;
  Res: TArray<string>; 
begin
  Res := TArrayHelper.Concat<string>(Arr);   
end;

{ TArrayHelper }

class function TArrayHelper.Concat<T>(const Arrays: array of TArray<T>): TArray<T>;
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