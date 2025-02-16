unit ASTTest.Props.PropWithIndex2;

interface

type
  TMyClass = class
  type
    TEnum = (item1, item2);
  private
    function GetValue(AIndex: TEnum): string;
  public
    property Value1: string index item1 read GetValue;
    property Value2: string index item2 read GetValue;
  end;

implementation

{ TMyClass }

function TMyClass.GetValue(AIndex: TEnum): string;
begin

end;

end.