unit ASTTest.Generics.SmartRef1;

interface

type
  SmartRef<T: class> = reference to function: T;

  TSmartRef = class {static}
  type
    TSmartRef<T: class> = class(TInterfacedObject, SmartRef<T>)
    private
      FValue: T;
    protected
      function Invoke: T;
    public
      constructor Create(const AValue: T);
      destructor Destroy; override;
    end;
  public
    class function Make<T: class>(const AObject: T): SmartRef<T>; static; inline;
    class function Cast<TSrc, TDst: class>(const ASource: SmartRef<TSrc>): SmartRef<TDst>; static; inline;
  end;

implementation

{ TSmartRef }

class function TSmartRef.Cast<TSrc, TDst>(const ASource: SmartRef<TSrc>): SmartRef<TDst>;
type
  PInterface = ^IInterface;
begin
  if ASource() is TDst then
     PInterface(@Result)^ := PInterface(@ASource)^
  else
    Result := nil;
end;

class function TSmartRef.Make<T>(const AObject: T): SmartRef<T>;
begin
  Result := TSmartRef<T>.Create(AObject);
end;

{ TSmartRef.TSmartRef<T> }

constructor TSmartRef.TSmartRef<T>.Create(const AValue: T);
begin
  FValue := AValue;
end;

destructor TSmartRef.TSmartRef<T>.Destroy;
begin
  FValue.Free;
  inherited;
end;

function TSmartRef.TSmartRef<T>.Invoke: T;
begin
  Result := FValue;
end;

end.