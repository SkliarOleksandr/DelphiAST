unit TASTTest.Classes.Inherited1;

interface

{$HINTS OFF}

type
  TBase = class
    function Foo: Integer; virtual;
    function Bar(S: string): Integer; virtual;     
  end;

  TChild = class(TBase)
    function Foo: Integer; override;
    function Bar(S: string): Integer; override; 
  end;

implementation

function TBase.Foo: Integer;
begin
  Result := 0;
end;

function TBase.Bar(S: string): Integer;
begin
  Result := 0;
end;

function TChild.Foo: Integer;
begin
  Result := inherited Foo();
  Result := inherited Foo;
  Result := inherited;
end;

function TChild.Bar(S: string): Integer;
begin
  Result := inherited Bar(S);
  Result := inherited;
end;

end.