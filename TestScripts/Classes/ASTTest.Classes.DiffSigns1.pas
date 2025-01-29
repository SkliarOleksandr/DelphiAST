unit ASTTest.Classes.DiffSigns1;

interface

type
  TBase = class
    constructor Create(str: string); virtual;
  end;

  TMyObj = class(TBase)
    procedure TestProc(a, b: Integer);
    constructor Create(str: string); override;
  end;

implementation

procedure TMyObj.TestProc;
begin
  if a > b then;
end;

constructor TMyObj.Create;
begin
  if str <> '' then;
end;

{ TBase }

constructor TBase.Create;
begin

end;

end.