unit ASTTest.Generics.GenericInstantiationProc2;

interface

type

  TGeneric<T> = class

    function Get1: T;
    function Get2<K>: K;

    property Value1: T read Get1;

    // it's not possible in Delphi (including Delphi 13) to specify generic proc future instantiation
    // it means the only generic types can have future instantiation

    // property Value1: T read Get2<T>; // E2008 Incompatible types
  end;

implementation

{ TGeneric<T> }

function TGeneric<T>.Get1: T;
begin
  Result := Default(T);
end;

function TGeneric<T>.Get2<K>: K;
begin
  Result := Default(K);
end;

end.