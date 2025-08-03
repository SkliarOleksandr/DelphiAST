unit ASTTest.Procs.OverloadDecl3;

interface

{$HINTS OFF}

uses
  ASTTest.Procs.OverloadDecl1,
  ASTTest.Procs.OverloadDecl2;

function GetValue(AValue: Boolean): Boolean; overload;

implementation

function GetValue(AValue: Boolean): Boolean;
begin
  Result := AValue;
end;

procedure Main;
var
  LInt: Integer;
  LDbl: Double;
  LBln: Boolean;
begin
  // This problem illustrates the need for a complete redesign of scope searhing... 
  LInt := GetValue(1); //  E2010 Incompatible types: 'Integer' and 'Double'
  LDbl := GetValue(1.1);
  LBln := GetValue(True);
end;

end.