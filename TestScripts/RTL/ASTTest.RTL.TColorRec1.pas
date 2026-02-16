unit ASTTest.RTL.TColorRec1;

interface

implementation

uses
  System.UITypes, System.UIConsts;
   
function AlphaColorToColor(const Color: TAlphaColor): TColor;
begin
  TColorRec(Result).R := TAlphaColorRec(Color).R;
  TColorRec(Result).G := TAlphaColorRec(Color).G;
  TColorRec(Result).B := TAlphaColorRec(Color).B;
  TColorRec(Result).A := 0;
end;   

end.