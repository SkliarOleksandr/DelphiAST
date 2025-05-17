unit ASTTest.Helpers.GenericTypeInHelper1;

interface

{$HINTS OFF}

type
  TOriginal = class
  type
    TNested = record

    end;
  end;


  THelper = class helper for TOriginal
  type
    TNested<T> = record

    end;
  end;

implementation

var
  Nested: TOriginal.TNested<Integer>;

end.