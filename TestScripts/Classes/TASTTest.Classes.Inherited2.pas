unit TASTTest.Classes.Inherited2;

interface

{$HINTS OFF}

type
  TBase = class
    function Foo: Integer; overload; virtual;
    function Foo(S: string): Integer; overload; virtual;
  end;

  TChild = class(TBase)
    function Foo: Integer; override;
    function Foo(S: string): Integer; override;      
  end;

implementation

function TBase.Foo: Integer;
begin
  Result := 0;
end;

function TBase.Foo(S: string): Integer;
begin
  Result := 0;
end;  

function TChild.Foo: Integer;
begin
  Result := inherited Foo();
  Result := inherited Foo;
  Result := inherited;  
end;

function TChild.Foo(S: string): Integer;
begin
  Result := inherited Foo(S);
  Result := inherited;
end; 

end.