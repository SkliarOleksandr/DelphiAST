unit ASTTest.Generics.ImplicitCall;

interface

{$HINTS OFF}

type
  TRecord = record
    function GetData: Pointer; overload;
    function GetData<T>: T; overload;
  end;

implementation

procedure Main(ARecord: TRecord);
begin
  var LPtr: Pointer := ARecord.GetData;
  var LStr: string :=  ARecord.GetData<string>;
end;

{ TRecord }

function TRecord.GetData: Pointer;
begin
  Result := nil;
end;

function TRecord.GetData<T>: T;
begin
  Result := Default(T);
end;

end.