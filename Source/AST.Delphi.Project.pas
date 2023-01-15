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
    function GetSysInitUnit: TASTModule;
    property SysInitUnit: TASTModule read GetSysInitUnit;
  end;


  TASTDelphiProject = class(TPascalProject, IASTDelphiProject)
  private
    fSysInitUnit: TASTModule;
  protected
    function GetUnitClass: TASTUnitClass; override;
    function GetSystemUnitClass: TASTUnitClass; override;
    function GetSysInitUnit: TASTModule;
    procedure DoBeforeCompileUnit(AUnit: TASTModule); override;
    procedure DoFinishCompileUnit(AUnit: TASTModule; AIntfOnly: Boolean); override;
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

procedure TASTDelphiProject.DoBeforeCompileUnit(AUnit: TASTModule);
begin
  inherited;
  // add SysInit unit implicitly
  if Assigned(fSysInitUnit) then
    (AUnit as TASTDelphiUnit).IntfImportedUnits.AddObject('SysInit', fSysInitUnit);
end;

procedure TASTDelphiProject.DoFinishCompileUnit(AUnit: TASTModule; AIntfOnly: Boolean);
begin
  inherited;
  if AUnit.Name = 'SysInit' then
    fSysInitUnit := AUnit;
end;

function TASTDelphiProject.GetSysInitUnit: TASTModule;
begin
  Result := fSysInitUnit;
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
