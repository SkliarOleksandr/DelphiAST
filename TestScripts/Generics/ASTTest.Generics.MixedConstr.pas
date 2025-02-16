unit ASTTest.Generics.MixedConstr;

interface

type

  TMyClass<T: class, constructor> = class
  private
    FValue: T;
  end;

implementation

end.