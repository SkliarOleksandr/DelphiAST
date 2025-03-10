unit ASTTest.Generics.MixedConstr;

interface

type

  TMyClass1<T: class, constructor> = class
  private
    FValue: T;
  end;
  
  TBase = class
  end;
  
  TMyClass2<T: TBase, constructor> = class
  private
    FValue: T;
  end;  
  
implementation

end.