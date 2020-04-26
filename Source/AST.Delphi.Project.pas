unit AST.Delphi.Project;

interface

uses AST.Classes,
     AST.Pascal.Project,
     AST.Delphi.Parser,
     AST.Parser.Utils,
     AST.Delphi.Classes;

type
  TASTDelphiProject = class(TPascalProject)
  protected
    function GetUnitClass: TASTUnitClass; override;
  public
    constructor Create(const Name: string; RTTICharset: TRTTICharset = RTTICharsetASCII); override;
  end;

implementation

{ TASTDelphiProject }

constructor TASTDelphiProject.Create(const Name: string; RTTICharset: TRTTICharset);
begin
  inherited;

end;

function TASTDelphiProject.GetUnitClass: TASTUnitClass;
begin
  Result := TASTDelphiUnit;
end;

end.
