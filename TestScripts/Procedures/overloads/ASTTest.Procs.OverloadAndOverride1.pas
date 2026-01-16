unit ASTTest.Procs.OverloadAndOverride1;

interface

{$HINTS OFF}

type
  TBase = class
    function GetValue(AIndex: Integer): string; overload; virtual;
    function GetValue(AName: string): string; overload;  
  end;

  TChild = class(TBase)
    function GetValue(AIndex: Integer): string; override;
    procedure Main;
  end;


implementation

{ TBase }

function TBase.GetValue(AName: string): string;
begin

end;

function TBase.GetValue(AIndex: Integer): string;
begin

end;

{ TChild }

function TChild.GetValue(AIndex: Integer): string;
begin

end;

procedure TChild.Main;
begin
  if GetValue(1) <> GetValue('name') then;
end;

end.