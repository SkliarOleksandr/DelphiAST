unit ASTTest.Other.Deprecated1 deprecated 'do not use';

interface

{$HINTS OFF}

const
  DeprecatedConst1 = 1234 deprecated;
  DeprecatedConst2 = 1234 deprecated 'do not use';

type
  TDeprecatedRecord1 = record
  end deprecated;

  TDeprecatedRecord2 = record
  end deprecated 'do not use';

  TDeprecatedClass1 = class
  end deprecated;

  TDeprecatedClass2 = class
  end deprecated 'do not use';

  TRecord = record
  private
    FField1: Integer deprecated;
    FField2: Integer deprecated 'do not use';
  public
    class operator Implicit(const AValue: string): TRecord; deprecated;
    class operator Implicit(const AValue: Integer): TRecord; deprecated 'Do not use';
    // property Field1: Integer read FField1 deprecated; // is not supported by Delphi!
  end;

  TDeprecatedEnum1 = (item1, item2) deprecated;
  TDeprecatedEnum2 = (item3, item4) deprecated 'do not use';

  TDeprecatedRange1 = 1..5 deprecated;
  TDeprecatedRange2 = 1..5 deprecated 'do not use';

  TDeprecatedAlias1 = Integer deprecated;
  TDeprecatedAlias2 = Integer deprecated 'do not use';

  // TDeprecatedArray = array [0..2] of Integer deprecated; // is not supported by Delphi!
  // TDeprecatedArray = array of Integer deprecated;        // is not supported by Delphi!
  // TNewDeprecatedType = type Integer deprecated;          // is not supported by Delphi!
  // TProcType = procedure(A: Integer) deprecated;        // is not supported by Delphi!

var
  DeprecatedVar1: Boolean deprecated;
  DeprecatedVar2: Boolean deprecated 'do not use';

procedure DeprecatedProc1; deprecated
procedure DeprecatedProc2; deprecated 'do not use'

implementation

{ TRecord }

class operator TRecord.Implicit(const AValue: Integer): TRecord;
begin
  inherited;

end;

class operator TRecord.Implicit(const AValue: string): TRecord;
begin

end;

procedure DeprecatedProc1;
begin

end;

procedure DeprecatedProc2;
begin

end;

end.