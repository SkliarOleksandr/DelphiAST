unit ASTTest.Generics.Array4;

interface

type

  TArray<T> = array of array of T;

  TArrayHelper = class
    class procedure DoWork<T>();
    class procedure Sort<K>(AArray: TArray<K>); overload;
  end;

implementation

{ TArrayHelper }

class procedure TArrayHelper.DoWork<T>;
var
  Arr: TArray<T>;
begin
  // Delphi can't infer instantiation arg implicitly here
  // Sort(Arr);  // expected: E2010 Incompatible types: 'TArray<K>' and 'TArray<T>'
  
  // The only explicit argument specification works here 
  Sort<T>(Arr); 
end;

class procedure TArrayHelper.Sort<K>(AArray: TArray<K>);
begin

end;


end.