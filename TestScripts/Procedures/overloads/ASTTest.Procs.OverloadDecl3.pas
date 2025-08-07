unit ASTTest.Procs.OverloadDecl3;

interface

{$HINTS OFF}

uses
  ASTTest.Procs.OverloadDecl1, // declared: function GetValue(AValue: Integer): Integer; overload;
  ASTTest.Procs.OverloadDecl2; // declared: function GetValue(AValue: string): string; overload;

function GetValue(AValue: Boolean): Boolean; overload;

implementation

function GetValue(AValue: Boolean): Boolean;
begin
  Result := AValue;
end;

procedure Main;
var
  LInt: Integer;
  LStr: string;
  LBln: Boolean;
begin
  // This problem illustrates the need for a complete redesign of scope searhing... 
  LInt := GetValue(1); //  E2250 There is no overloaded version of 'GetValue' that can be called with these arguments
  LStr := GetValue('1');
  LBln := GetValue(True);
end;

end.