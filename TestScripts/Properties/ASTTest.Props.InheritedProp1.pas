unit ASTTest.Props.InheritedProp1;

interface

{$HINTS OFF}

type

  TBase = class
    FValue: Integer;
    property Value: Integer read FValue write FValue;
  end;

  TChild = class(TBase)
    function Value: Integer;
    procedure Clear;
  end;

implementation

{ TChild }

procedure TChild.Clear;
begin
  inherited Value := 0;
end;

function TChild.Value: Integer;
begin
  Result := inherited Value;
end;

end.