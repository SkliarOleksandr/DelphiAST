unit ASTTest.Generics.Array3;

interface

type

  TArrayHelper = class
    class procedure DoWork<T>();
    class procedure Sort<K>(AArray: array of K); overload;
  end;

implementation

{ TArrayHelper }

class procedure TArrayHelper.DoWork<T>;
var
  Arr1: array [0..0] of T;
  Arr2: array of array of T;
begin
  // Delphi can infer instantiation arg implicitly here
  Sort(Arr1);
  Sort(Arr2);
end;

class procedure TArrayHelper.Sort<K>(AArray: array of K);
begin

end;

end.