unit ASTTest.Generics.GenericInstantiation3;

interface

{$HINTS OFF}

type
  IInterface<T> = interface
  end;

  TRecord = record
    class function GetIntf<T>(APtr: Pointer): IInterface<T>; static;
  end;

implementation

{ TRecord }

class function TRecord.GetIntf<T>(APtr: Pointer): IInterface<T>;
begin
  Result := IInterface<T>(APtr);
end;

end.