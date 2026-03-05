unit ASTTest.Procs.Overloads.ImplicitCall3;

interface

implementation

function GetData: pointer; overload;
begin
  Result := nil;
end;

function GetData(APtr: Pointer): Integer; overload;
begin
  Result := 0;
end;

procedure DoSmth(AStr: pointer);
begin
end;

var
  Ptr: pointer;

procedure Main;
begin
  Ptr := GetData;  // case 1
  DoSmth(GetData); // case 2
end;

end.