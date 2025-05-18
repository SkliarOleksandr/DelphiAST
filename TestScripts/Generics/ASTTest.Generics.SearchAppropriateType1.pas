unit ASTTest.Generics.SearchAppropriateType1;

interface

uses
  ASTTest.Generics.TListDecl1; // TList declaration

type
  TMyClass = class
    FList: TList;  
    constructor Create;
  end;
    
implementation

uses
  ASTTest.Generics.TListDecl2; // TList<T> declaration

constructor TMyClass.Create
begin
  FList := TList.Create; // E2010 Incompatible types: 'TList' and 'TList<T>'
end;

end.