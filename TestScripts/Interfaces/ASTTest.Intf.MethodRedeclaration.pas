unit ASTTest.Intf.MethodRedeclaration;

interface

{$HINTS OFF}

type
  IIntf1 = interface
    function Get: Integer;
  end;

  IIntf2 = interface(IIntf1)
    function Get: string;
  end;

  IIntf3 = interface(IIntf2)
    function Get: Boolean;
  end;

var
  GInt: Integer;
  GStr: string;
  GBln: Boolean;

implementation

procedure Main(AIntf1: IIntf1; AIntf2: IIntf2; AIntf3: IIntf3);
begin
  GInt := AIntf1.Get;
  GStr := AIntf2.Get;
  GBln := AIntf3.Get;
end;

end.