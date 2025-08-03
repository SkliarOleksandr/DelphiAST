unit ASTTest.Generics.RecursiveUsing2;

interface

type
  TArray<T> = array of T;


  TBase = class
  end; 

  TMyType = class(TBase)
  type
    TFunc<T: TBase> = function (AType: TMyType): TArray<T>;
    function DoCall<T: TBase>(const AGetListFunc: TFunc<T>): T;
    procedure Test;
  end;

implementation

{ TMyType }

function TMyType.DoCall<T>(const AGetListFunc: TFunc<T>): T;
begin
  Result := AGetListFunc(Self)[0];
end;

procedure TMyType.Test;
begin
  DoCall<TMyType>(nil);
end;

end.