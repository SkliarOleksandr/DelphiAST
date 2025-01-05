unit ASTTest.Classes.VirtualsOverlod1;

interface

{$HINTS OFF}

type
  TBase = class
    procedure Test(A: Integer); overload; virtual;
    procedure Test(A, B: Integer); overload; virtual;
  end;

  TChild = class(TBase)
    procedure Test(A: Integer); override;
  end;

implementation

procedure Main(AChild: TChild);
begin
  AChild.Test(1, 2);
end;

{ TBase }

procedure TBase.Test(A, B: Integer);
begin

end;

procedure TBase.Test(A: Integer);
begin

end;

{ TChild }

procedure TChild.Test(A: Integer);
begin
  inherited;

end;

end.