unit ASTTest.Generics.GenericMethod4;

interface

type
  TValue = record
    function AsType<T>: T;
  end;

  TArray<T> = array of T;

  TBytes = TArray<Byte>;

implementation

{ TValue }

function TValue.AsType<T>: T;
begin
  Result := Default(T);
end;

function GetBytes(AValue: TValue): TBytes;
begin
  Result := AValue.AsType<TBytes>;
end;

end.