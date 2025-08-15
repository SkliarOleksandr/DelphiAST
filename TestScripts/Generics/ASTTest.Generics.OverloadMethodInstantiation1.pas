unit ASTTest.Generics.OverloadMethodInstantiation1;

interface

type

  TRec = record
    class procedure DoSmth(value: Integer); overload; static;
    class procedure DoSmth<T>(V1, V2: T); overload; static;
  end;

implementation

procedure Main;
begin
  TRec.DoSmth(1);
  TRec.DoSmth('str1', 'str2');
end;


{ TRec }

class procedure TRec.DoSmth(value: Integer);
begin

end;

class procedure TRec.DoSmth<T>(V1, V2: T);
begin

end;

end.