unit ASTTest.Props.OverloadGetSet;

interface

{$HINTS OFF}

type
  TRec = record
  private
    procedure SetValue(const Value: string); overload;
    procedure SetValue(AIndex: Integer; const Value: string); overload;
    //function GetValue(const Value: string): string; overload;
    function GetValue: string; overload;
    function GetValue(AIndex: Integer): string; overload;
  public
    property Value: string read GetValue write SetValue;
  end;

implementation

{ TRec }

function TRec.GetValue: string;
begin

end;

function TRec.GetValue(AIndex: Integer): string;
begin

end;

procedure TRec.SetValue(AIndex: Integer; const Value: string);
begin

end;

procedure TRec.SetValue(const Value: string);
begin

end;

end.