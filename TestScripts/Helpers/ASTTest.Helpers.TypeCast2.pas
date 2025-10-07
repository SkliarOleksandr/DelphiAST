unit ASTTest.Helpers.TypeCast2;

interface

{$HINTS OFF}

type
  TCaption = type string;

  TCaption2 = type string;


  TMYObj = class
    FText: TCaption;
    property Text: TCaption read FText;
  end;


  TStrHelper = record helper for TCaption2
    function IsStr: Boolean;
  end;

implementation

procedure Main(AObj: TMyObj);
begin
  var LStrs := TCaption2(AObj.Text).IsStr;
end;

{ TStrHelper }

function TStrHelper.IsStr: Boolean;
begin
  Result := True;
end;

end.