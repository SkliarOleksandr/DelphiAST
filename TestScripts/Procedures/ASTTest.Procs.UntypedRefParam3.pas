unit ASTTest.Procs.UntypedRefParam3;

interface

implementation

procedure Proc(const X);
begin
end;

procedure Test1(const AValue: Integer);
begin
  Proc(AValue);
end;

procedure Test2;
 // Delphi treats such constant as "read-only" variable
const
  CValue: Integer = 5;
begin
  Proc(CValue);
end;

procedure Test3;
 // the same for string types
const
  StrConst = 'string';
begin
  Proc(StrConst);
end;

procedure Test4;
 // the same for array types
const
  StrConst = [1, 2, 3];
begin
  Proc(StrConst);
end;

end.