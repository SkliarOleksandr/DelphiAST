unit ASTTest.Generics.Arrays;

interface

type

  TArray<T> = array of T;

  TArrayHelper = class
  public
    class function Concat<T>(const Arrays: array of TArray<T>): TArray<T>;   
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

end.