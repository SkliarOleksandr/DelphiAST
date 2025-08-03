unit ASTTest.Classes.Inherited4;

interface

uses 
  ASTTest.Classes.Base, Data.DB;

type

  TSuccesor = class abstract (TBase)
  protected
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
  end; 

implementation

{ TSuccesor }

function TSuccesor.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  Result := nil;
end;

end.