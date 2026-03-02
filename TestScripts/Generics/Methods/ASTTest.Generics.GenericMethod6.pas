unit ASTTest.Generics.GenericMethod6;

interface

type
  TList<T> = class
  end;

  TValue = record
    function GetAsValue<V>(out AValue: V; ADefParam: Integer = 0): Boolean; overload;
    function GetAsValue(out AValue: Pointer; ATypeInfo: Pointer): Boolean; overload;
  end;

  TGeneric<K> = class
    procedure DoSmth(AValue: TValue);
  end;

implementation

{ TGeneric<K> }

procedure TGeneric<K>.DoSmth(AValue: TValue);
var
  LList: TList<K>;
begin
  if AValue.GetAsValue({out} LList) then;
end;

function TValue.GetAsValue(out AValue: Pointer; ATypeInfo: Pointer): Boolean;
begin
  Result := True;
end;

function TValue.GetAsValue<V>(out AValue: V; ADefParam: Integer = 0): Boolean;
begin
  Result := True;
end;

end.