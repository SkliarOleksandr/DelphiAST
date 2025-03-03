unit ASTTest.Procs.CallConv.Register1;

interface

{$HINTS OFF}

type

  IMyIntf = interface
    procedure Method1; register;
  end;

  TMyObj = class
    procedure Method1; register;
    class procedure Method2; register;
  end;

  TRecord = record
    class operator Implicit(A: Integer): TRecord; // register; TODO:
  end;

  TProcType = procedure; register;

implementation

procedure GlobalProc1; register;

  procedure NestedProc1; register;
  begin

  end;

begin

end;


{ TMyObj }

procedure TMyObj.Method1;
begin

end;

class procedure TMyObj.Method2;
begin

end;

{ TRecord }

class operator TRecord.Implicit(A: Integer): TRecord;
begin

end;

end.