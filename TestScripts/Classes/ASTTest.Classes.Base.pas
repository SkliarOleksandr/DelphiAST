unit ASTTest.Classes.Base;

interface

uses
  Data.db; 

type
  TBase = class
  protected
    function GetFieldClass(FieldType: TFieldType): TFieldClass; virtual; abstract; 
  end;

implementation

end.