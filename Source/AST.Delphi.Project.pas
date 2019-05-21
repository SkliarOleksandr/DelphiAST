unit AST.Delphi.Project;

interface

uses AST.Classes, NPCompiler.Package, AST.Delphi.Parser, AST.Delphi.Classes;

type
  TASTDelphiProject = class(TNPPackage)
  protected
    function GetUnitClass: TASTUnitClass; override;
  end;

implementation

{ TASTDelphiProject }

function TASTDelphiProject.GetUnitClass: TASTUnitClass;
begin
  Result := TASTDelphiUnit;
end;

end.
