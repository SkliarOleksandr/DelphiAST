unit ASTTest.Helpers.TypeCast1;

interface

type
  
  TStrHelper = record helper for string
    function IsStr: Boolean;
  end;

implementation

procedure Main(APtr: Pointer);
begin
  if string(APtr).IsStr then
end;

{ TStrHelper }

function TStrHelper.IsStr: Boolean;
begin
  Result := True;
end;

end.