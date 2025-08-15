unit ASTTest.Ops.Ampersand1;

interface

type
  &type = record
    &in: Integer;
    &out: string;
    procedure &begin(&var: string);
  end;

var
  &var: &type;

implementation

procedure &procedure;
begin
  &var.&in := 1;
  &var.&begin('str');
end;

{ type }

procedure &type.&begin(&var: string);
begin
  &out := &var;
end;

end.