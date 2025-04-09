unit ASTTest.Generics.ImplicitCall4;

interface

type
  TRec = record
    class function GetValue<T>: T; static;
  end;
  
  TGetIntFunc = function: Integer;

var
  IntFunc: TGetIntFunc; 

implementation

procedure DoSmth(AValue: Integer); overload; begin end;
procedure DoSmth(AValue: string); overload; begin end;
procedure DoSmth(AValue: TGetIntFunc); overload; begin end;

procedure Main;
begin
  DoSmth(TRec.GetValue<Integer>);  // takes 1st
  DoSmth(TRec.GetValue<string>);   // takes 2nd
  DoSmth(IntFunc);                 // takes 3rd
end;

{ TRec }

class function TRec.GetValue<T>: T;
begin
  Result := Default(T);
end;

end.