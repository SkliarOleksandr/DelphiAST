unit ASTTest.Helpers.Main;

interface

type
  TSourceStruct = class
  end;
  
  TSourceStruct2 = class(TSourceStruct)
  end;

implementation

uses 
  ASTTest.Helpers.Declaration;
  
var
  Obj: TSourceStruct2;
  
procedure Main;
begin
  Obj.HelperProc; 
end;
  
  
end.