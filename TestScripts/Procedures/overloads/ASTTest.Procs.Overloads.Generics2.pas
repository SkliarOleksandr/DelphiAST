unit ASTTest.Procs.Overloads.Generics2;

interface

type 
  TMyObj = class
    function GetData: Pointer; overload;  
  end;

  TMyObj<T> = class(TMyObj)
    FValue: T;
    function GetData: T; overload;
  end;

  TMyObj<T1, T2> = class(TMyObj)
    FValue: T1;
    function GetData: T1; overload;
  end;  

implementation

{ TMyObj }

function TMyObj.GetData: Pointer;
begin
  Result := nil;
end;

{ TMyObj<T> }

function TMyObj<T>.GetData: T;
begin
  Result := FValue;
end;

{ TMyObj<T1, T2> }

function TMyObj<T1, T2>.GetData: T1;
begin
  Result := FValue;
end;

end.