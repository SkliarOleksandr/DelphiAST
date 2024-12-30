unit ASTTest.Generics.Nested2;

interface

{$HINTS OFF}

type
  TNested<T> = class
    Value: T;  
  end; 

  TBase<T> = class
  public
    function GetNested: TNested<T>; virtual; abstract;
  end;

  TGeneric<T> = class(TBase<T>)
  type
    TNested = class(TNested<T>)
    end;
  private
    FNested: TNested;
  public
    function GetNested: TNested; reintroduce;
  end;

var
  Generic: TGeneric<Integer>;

implementation

procedure Main;
begin
  Generic.FNested := Generic.GetNested;
end;


{ TGeneric<T> }

function TGeneric<T>.GetNested: TNested;
begin
  Result := nil;
end;

end.