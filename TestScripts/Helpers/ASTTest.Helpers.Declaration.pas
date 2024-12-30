unit ASTTest.Helpers.Declaration;

interface

uses 
  ASTTest.Helpers.Main;

type

  THelper = class helper for TSourceStruct
    procedure HelperProc;
  end;

implementation

procedure THelper.HelperProc;
begin
 
end;

end.