unit ASTTest.Procs.Overloads.Generics3;

interface

{$HINTS OFF}

type
  TRecord = record
    class procedure DoSmth(var AValue: Int64); overload; static;
    class procedure DoSmth<T>(var AValue: T); overload; static;
  end;

implementation

procedure Main;
var
  LInt64: Integer;
begin
  //TRecord.DoSmth(42); // E2250 There is no overloaded version of 'DoSmth' that can be called with these arguments
  TRecord.DoSmth(LInt64);
end;

{ TRecord }

class procedure TRecord.DoSmth(var AValue: Int64);
begin
  if AValue <> 0 then;
end;

class procedure TRecord.DoSmth<T>(var AValue: T);
begin
  if AValue <> Default(T) then;
end;

end.