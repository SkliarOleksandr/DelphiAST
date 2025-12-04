unit ASTTest.Generics.InstantiationInMethods1;

interface

{$HINTS OFF}

type
  TArray<T> = array of T;

  THelper = class
  public
    class function Transform<K>(const AArray: TArray<K>): TArray<K>;
  end;

implementation

{ THelper }

class function THelper.Transform<K>(const AArray: TArray<K>): TArray<K>;
begin
  Result := AArray;
end;

procedure Main;
var
  LArray: TArray<string>;
begin
  LArray := THelper.Transform<string>(LArray);  
end;

end.