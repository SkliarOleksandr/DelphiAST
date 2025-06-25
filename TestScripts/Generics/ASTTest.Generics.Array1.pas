unit ASTTest.Generics.Array1;

interface

type

  TArray<T> = array of T;

var
  A1: TArray<string>;
  A2: TArray<TArray<string>>;

implementation

end.