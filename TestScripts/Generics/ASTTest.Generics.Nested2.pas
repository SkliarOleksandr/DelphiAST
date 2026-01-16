unit ASTTest.Generics.Nested2;

interface

{$HINTS OFF}

type
  TNested<T> = class
    Value: T;  
  end; 

  TBase<K> = class
  public
    function GetNested: TNested<K>; virtual; abstract;
  end;

  TGeneric<X> = class(TBase<X>)
  type
    TMyNested = class(TNested<X>)
    end;
  private
    FNested: TMyNested;
  public
    function GetNested: TMyNested; reintroduce;
  end;

var
  Generic: TGeneric<Integer>;

implementation

procedure Main;
begin
  Generic.FNested := Generic.GetNested; 
  Generic.FNested := Generic.GetNested();
end;

{ TGeneric<X> }

function TGeneric<X>.GetNested: TMyNested;
begin
  Result := nil;
end;

end.