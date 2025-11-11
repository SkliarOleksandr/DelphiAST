unit ASTTest.Generics.GenericInstantiation0_2;

interface

type
  TWrapper<T> = record
    Value: T;
  end;

  TMyObject = class                                 
    class function Sum<K>(
      ALeft: TWrapper<K>; // here, TWrapper<T> will be partially instantiated with <K>
      ARight: TWrapper<K> // here (and below) TWrapper<K> will be re-used from the pool
      ): TWrapper<K>;        
  end;

implementation

{ TMyObject }

class function TMyObject.Sum<K>(
  ALeft: TWrapper<K>; // here, TWrapper<T> will be partially instantiated with <K> again!
                      // since at this point we can't link this method implementation with the definition
  ARight: TWrapper<K> // here (and below) TWrapper<K> will be re-used from the pool
  ): TWrapper<K>;
begin
  // NOTE1:
  // Since the TWrapper<K> created in lines 12 and 22 are not strictly equal (due to the different <K> parameter),
  // the parser uses "relax-generic-type-comparison", i.e. comparison by name, to find the correct method definition.

  // NOTE2:
  // at this point, the parser will find appropriate method definition, and use only it,
  // it means, all instances of TWrapper<T>, created starting line 22 (and until the end of method signature) will be ignored


  // Result := Default(TWrapper<K>);
end;

var 
  LLeft, LRight, LResult: TWrapper<Integer>; 

procedure Main;
begin
  LResult := TMyObject.Sum<Integer>(LLeft, LRight)
end;

end.