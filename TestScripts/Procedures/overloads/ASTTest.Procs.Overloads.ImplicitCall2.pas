unit ASTTest.Procs.Overloads.ImplicitCall2;

interface

{$HINTS OFF}

type

  TBase = class
    function ToInt: Integer; overload; virtual;
  end;
  
  TChild = class(TBase)
    function ToInt(B: Boolean): Integer; overload;
    function ToInt: Integer; overload; override;    
  end;  

implementation

var
  Child: TChild;

procedure Main;
begin
  if Child.ToInt <> 0 then;
end;

{ TChild }

function TChild.ToInt(B: Boolean): Integer;
begin
  Result := 0;
end;

function TChild.ToInt: Integer;
begin
  Result := 0;
end;

{ TBase }

function TBase.ToInt: Integer;
begin
  Result := 0;
end;

end.