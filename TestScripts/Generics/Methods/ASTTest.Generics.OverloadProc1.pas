unit ASTTest.Generics.OverloadProc1;

interface

{$HINTS OFF}

type
  TRec = record
    class function GetValue(AKey: string): Integer; overload; static;
    class function GetValue<TValue>(AKey: string): TValue; overload; static;
  end;

implementation

procedure Main;
var
  LInt: Integer;
  LStr: string;
begin
  LInt := TRec.GetValue('key');
  LStr := TRec.GetValue<string>('key');
end;

{ TRec }

class function TRec.GetValue(AKey: string): Integer;
begin
  Result := 0;
end;

class function TRec.GetValue<TValue>(AKey: string): TValue;
begin
  Result := Default(TValue);
end;

end.