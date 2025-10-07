unit ASTTest.Generics.GenericInstantiation3;

interface

{$HINTS OFF}

type
  IInterface<T> = interface
  end;

  TRecord1 = record
    class function GetIntf<T>(APtr: Pointer): IInterface<T>; static;
  end;

  TRecord2<K> = record
    class function GetIntf<T>(APtr: Pointer): IInterface<K>; static;
  end;

implementation

{ TRecord1 }

class function TRecord1.GetIntf<T>(APtr: Pointer): IInterface<T>;
begin
  Result := IInterface<T>(APtr);
end;

{ TRecord2<K> }

class function TRecord2<K>.GetIntf<T>(APtr: Pointer): IInterface<K>;
begin
  Result := IInterface<K>(APtr);
end;

end.