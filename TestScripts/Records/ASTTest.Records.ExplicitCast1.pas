unit ASTTest.Records.ExplicitCast1;

interface

type
  TAlphaColor = type Cardinal;

  TAlphaColorRec = record
    constructor Create(const Color: TAlphaColor);
    case Cardinal of
      0:
        (Color: TAlphaColor);
      2:
        (HiWord, LoWord: Word);
  end;

implementation

constructor TAlphaColorRec.Create(const Color: TAlphaColor);
begin
  Self := TAlphaColorRec(Color);
end;


end.