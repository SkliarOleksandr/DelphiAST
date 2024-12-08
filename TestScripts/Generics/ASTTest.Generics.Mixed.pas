unit ASTTest.Generics.Mixed;

interface

type
  TRec<T> = record
    procedure Foo<V>(Value: V);
  end;

var
  GRec: TRec<Integer>;

implementation

//procedure Main;
//begin
//  GRec.Foo<string>('str');
//end;

{ TRec<T> }

procedure TRec<T>.Foo<V>(Value: V);
begin

end;

end.