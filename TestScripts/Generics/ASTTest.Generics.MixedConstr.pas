unit ASTTest.Generics.MixedConstr;

interface

{$HINTS OFF}

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