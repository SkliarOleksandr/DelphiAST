unit ASTTest.Procs.Overloads.RawByteString1;

interface

{$HINTS OFF}

implementation

procedure DoString(AStr: string); overload;
begin
  if AStr <> '' then;
end;

procedure DoString(AStr: AnsiString); overload;
begin
  if AStr <> '' then;
end;

procedure Main;
var
  LStr: RawByteString;
begin
  DoString(LStr);
end;

end.