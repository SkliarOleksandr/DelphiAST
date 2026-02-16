unit ASTTest.RTL.TValue1;

interface

implementation

uses 
  System.SysUtils, System.Rtti;
  
function GetBytes(AValue: TValue): TBytes;
begin
  Result := AValue.AsType<TBytes>;
end;  

end.