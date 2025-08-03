unit ASTTest.Intf.Implements1;

interface

type
  IMyInterface = interface
    procedure Test;
  end;

  IMyInterface2 = interface
    procedure Test;
  end;

  TMyImplementator = class(TInterfacedObject, IMyInterface)
    procedure Test;
  end;

  TMyClass = class(TObject, IMyInterface)
  private
    FImpl: TMyImplementator;
  public
    property _Impl: TMyImplementator read FImpl implements IMyInterface;
    // E2265 Interface 'IMyInterface2' not mentioned in interface list
    // property _Impl2: TMyImplementator read FImpl implements IMyInterface2;
  end;

implementation

{ TMyImplementator }

procedure TMyImplementator.Test;
begin
  // do nothing
end;

end.