unit AST.Delphi.Project;

interface

uses AST.Intf,
     AST.Classes,
     AST.Pascal.Intf,
     AST.Pascal.Project,
     AST.Parser.Utils;

type
  IASTDelphiProject = interface(IASTPascalProject)
    ['{07D970E0-C4D9-4627-AD95-A561286C049E}']
  end;


  TASTDelphiProject = class(TPascalProject, IASTDelphiProject)
  protected
    function GetUnitClass: TASTUnitClass; override;
    function GetSystemUnitClass: TASTUnitClass; override;
  public
    constructor Create(const Name: string); override;
  end;

implementation

uses AST.Delphi.Parser,
     AST.Delphi.System;

{ TASTDelphiProject }

constructor TASTDelphiProject.Create(const Name: string);
begin
  inherited;

end;

function TASTDelphiProject.GetSystemUnitClass: TASTUnitClass;
begin
  Result := TSYSTEMUnit;
end;

function TASTDelphiProject.GetUnitClass: TASTUnitClass;
begin
  Result := TASTDelphiUnit;
end;

end.
