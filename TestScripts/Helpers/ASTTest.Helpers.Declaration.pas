unit ASTTest.Helpers.Declaration;

interface

uses 
  ASTTest.Helpers.Main;

type
 
  TX = class
  type
    THelper = class helper for TSourceStruct
      procedure HelperProc;
    end;
  end;


implementation


procedure TX.THelper.HelperProc;
begin
 
end;

end.