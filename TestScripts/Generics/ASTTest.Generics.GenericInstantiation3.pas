unit ASTTest.Generics.GenericInstantiation3;

interface

{$HINTS OFF}

type
  IInterface<T> = interface
  end;

  TRecord1 = record
    class function GetIntf<K1>(APtr: Pointer): IInterface<K1>; static;
  end;

  TRecord2<K2> = record
    class function GetIntf<K3>(APtr: Pointer): IInterface<K2>; static;
  end;

implementation

{ TRecord1 }

class function TRecord1.GetIntf<K1>(APtr: Pointer): IInterface<K1>;
begin
  Result := IInterface<K1>(APtr);
end;

{ TRecord2<K> }

class function TRecord2<K2>.GetIntf<K3>(APtr: Pointer): IInterface<K2>;
begin
  Result := IInterface<K2>(APtr);
end;

end.