unit ASTTest.Props.ArrayProp1;

interface

type
  TStrings = class
  private
    function Get(Index: Integer): string;
    procedure Put(Index: Integer; const Value: string);
  public
     property Strings[Index: Integer]: string read Get write Put; default;
     procedure Exchange(Index1, Index2: Integer);
  end;

implementation

var
  Str: TStrings;

procedure TStrings.Exchange(Index1, Index2: Integer);
var
  TempString: string;
begin
  TempString := Strings[Index1];
  Strings[Index1] := Strings[Index2];
  Strings[Index2] := TempString;
end;

{ TStrings }

function TStrings.Get(Index: Integer): string;
begin

end;

procedure TStrings.Put(Index: Integer; const Value: string);
begin

end;

end.