unit ASTTest.Classes.Inherited3;

interface

type
  TBase = class
    constructor Create;
  end;

  TChild1 = class(TBase)
    constructor Create(A: Integer);
  end;

  TChild2 = class(TChild1)
    constructor Create(A, B: Integer);
  end;

  TChild3 = class(TChild1)
    constructor Create;
  end;

  TBase2 = class
  end;

  TChild4 = class(TBase2)
    constructor Create;
  end;


implementation

{ TBase }

constructor TBase.Create;
begin

end;

{ TChild1 }

constructor TChild1.Create(A: Integer);
begin
  //inherited;      // E2008 Incompatible types
  inherited Create;  // OK
end;

{ TChild2 }

constructor TChild2.Create(A, B: Integer);
begin
  // inherited;         // E2008 Incompatible types
  // inherited Create;  // E2035 Not enough actual parameters
  inherited Create(A);  // OK
end;

{ TChild3 }

constructor TChild3.Create;
begin
  // inherited;         // E2008 Incompatible types
  // inherited Create;  // E2035 Not enough actual parameters
  inherited Create(1);  // OK
end;

{ TChild4 }

constructor TChild4.Create;
begin
  inherited Create; // OK
end;

end.