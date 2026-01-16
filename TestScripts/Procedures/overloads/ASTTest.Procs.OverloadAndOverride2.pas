unit ASTTest.Procs.OverloadAndOverride2;

interface

{$HINTS OFF}

type
  TBase = class
    function GetValue(AName: string): string; overload;  
    function GetValue(AIndex: Integer): string; overload; virtual;
  end;

  TChild = class(TBase)
    function GetValue(AIndex: Integer): string; override;
  end;

  TChild2 = class(TChild)
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

{ TChild2 }

function TChild2.GetValue(AIndex: Integer): string;
begin

end;

procedure TChild2.Main;
begin
  if GetValue(1) <> GetValue('name') then;
end;

end.