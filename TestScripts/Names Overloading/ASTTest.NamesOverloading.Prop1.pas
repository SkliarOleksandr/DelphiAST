unit ASTTest.NamesOverloading.Prop1;

interface

type
  TStruct = record
  private
    FInt: Integer;
  public
    property Integer: Integer read FInt;
    property Integer2: Integer read FInt;
  end;

implementation

end.