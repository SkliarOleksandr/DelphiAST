unit ASTTest.Procs.Overloads.Generics1;

interface

type 
  TMyObj1 = class
    function GetData: Pointer; overload;
    function GetData<T>: T; overload;
  end;

  TMyObj2 = class
    function GetData<T>: T; overload;
    function GetData: Pointer; overload;
  end;

implementation

{ TMyObj1 }

function TMyObj1.GetData: Pointer;
begin
  Result := nil;
end;

function TMyObj1.GetData<T>: T;
begin
  Result := Default(T);
end;

{ TMyObj2 }

function TMyObj2.GetData: Pointer;
begin
  Result := nil;
end;

function TMyObj2.GetData<T>: T;
begin
  Result := Default(T);
end;

end.