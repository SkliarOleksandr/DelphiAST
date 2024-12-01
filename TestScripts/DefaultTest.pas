unit DefaultTest;

interface

type
  TStruct<T> = record
  type
    TRecord = record
      A, B: T;
    end;
  private
    Value: T;
  end;

var
  GVar1: TStruct<string>;
  GVar2: TStruct<Integer>.TRecord;

implementation

procedure Main;
begin
  GVar1.Value := 'string';
  GVar2.A := 1;
  GVar2.B := 2;
end;
