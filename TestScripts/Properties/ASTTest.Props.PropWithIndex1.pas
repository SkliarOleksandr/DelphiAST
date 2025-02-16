unit ASTTest.Props.PropWithIndex1;

interface

type
  TMyClass = class
  private
    function GetValue(AIndex: Integer): string;
  public
    property Value1: string index 0 read GetValue;
    property Value2: string index 2 read GetValue;
  end;

implementation

{ TMyClass }

function TMyClass.GetValue(AIndex: Integer): string;
begin

end;

end.